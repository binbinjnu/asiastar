%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     kv专属sql语句, 数据结构 {id, data, gmt_modified, gmt_create}
%%%     其中data为 term_to_binary 的结构, 取出来之后可以 binary_to_term 反序列化
%%% @end
%%% Created : 26. 11月 2019 15:50
%%%-------------------------------------------------------------------
-module(db_mysql_kv_command).
-author("Administrator").

-include("hrl_logs.hrl").
-include("hrl_db.hrl").
-include("hrl_common.hrl").

%% API
-export([
    ensure_kv_table/2,

    kv_insert/3,
    kv_insert_from_ets/2,
    kv_insert_from_ets/3,

    kv_fold/3
]).


-export([
    insert_kvs/3,
    insert_kvs/4,

    select_kv_data/2,
    select_kv_many_data/2,
    select_kv_seq_data/3,
    select_kv_first_data/2,
    select_kv_next_data/3,
    select_kv_max_id/1,

    delete_kv/2,
    delete_kv_many/2
]).

-define(MAX_INSERT_NUM, 300).   %% 一次最大插入数
-define(MAX_READ_NUM, 300).     %% MySQL 一次读取最大条数
-define(MAX_RETRY_NUM, 3).      %% MySQL 最大重试次数

%% 创建kv类型的表
ensure_kv_table(Table, Opts) ->
    KeyStr =
        case proplists:get_value(keyformat, Opts, int) of
            uint -> "BIGINT UNSIGNED";
            int -> "BIGINT";
            {uint, N} -> lists:concat(["INT(", N, ") UNSIGNED"]);
            {int, N} -> lists:concat(["INT(", N, ")"]);
            varbinary -> "VARBINARY(255)";
            {varbinary, N} -> lists:concat(["VARBINARY(", N, ")"])
        end,
    IndexStr =
        case proplists:get_bool(index_modified, Opts) of
            ?true -> ",INDEX `idx_gmt_modified` (`gmt_modified`)";
            ?false -> ""
        end,
    Sql = lists:concat(["CREATE TABLE IF NOT EXISTS ", Table, " (",
        "`id` ", KeyStr, " PRIMARY KEY,",
        "`data` MEDIUMBLOB not null,",
        "`gmt_modified` TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,",
        "`gmt_create` TIMESTAMP DEFAULT CURRENT_TIMESTAMP",
        IndexStr,
        ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin"]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql).



%% 直接向数据库中插入, 当数据量过大时自动分段, 全部成功返ok
kv_insert(Table, Objects, KeyPos) ->
    kv_insert_retry(Table, Objects, KeyPos, 1, 0).

%% 带重试次数的insert
kv_insert_retry(_Table, [], _KeyPos, _MaxRetry, _N) ->
    ok;
kv_insert_retry(Table, Objects, KeyPos, MaxRetry, N) ->
    F = fun(Data, Success) ->
            Success andalso insert_kvs(Table, Data, KeyPos) =:= ok
        end,
    case util_list:batch(F, ?true, Objects, ?MAX_INSERT_NUM) of
        ?true ->
            ok;
        _Error when N < MaxRetry ->
            kv_insert_retry(Table, Objects, KeyPos, MaxRetry, N + 1);
        Error ->
            Error
    end.


%% 从ETS读取数据向数据库中插入, 传入Key的列表,
%% 当数据量过大时自动分段, 全部成功返ok
kv_insert_from_ets(Table, Keys) when is_list(Keys) ->
    kv_insert_from_ets(Table, Keys, 0).

kv_insert_from_ets(Table, Keys, Compress) ->
    KeyPos = ets:info(Table, keypos),
    kv_insert_from_ets_retry(Table, Keys, KeyPos, Compress, ?MAX_RETRY_NUM, 0).

kv_insert_from_ets_retry(_Table, [], _KeyPos, _Compress, _MaxRetry, _N) ->
    ok;
kv_insert_from_ets_retry(Table, Keys, KeyPos, Compress, MaxRetry, N) ->
    F = fun(IDs, Success) ->
            RecordL = find_in_ets(Table, Compress, IDs),
            Success andalso db_mysql_command:insert(Table, [id, data], RecordL) =:= ok
        end,
    case util_list:batch(F, ?true, Keys, ?MAX_INSERT_NUM) of
        ?true ->
            ok;
        _Error when N < MaxRetry ->
            kv_insert_from_ets_retry(Table, Keys, KeyPos, Compress, MaxRetry, N + 1);
        Error ->
            Error
    end.

find_in_ets(Table, IDs, Compress) ->
    find_in_ets(Table, IDs, Compress, []).

find_in_ets(_Table, [], _Compress, RecordL) ->
    RecordL;
find_in_ets(Table, [ID|T], Compress, RecordL) ->
    case ets:lookup(Table, ID) of
        [Data] ->
            %% 获取数据后直接进行转换
            DataBin = erlang:term_to_binary(Data, [{compressed, Compress}]),
            RecordL1 = [#kv_id_data{id = ID, data = DataBin} | RecordL],
            find_in_ets(Table, T, Compress, RecordL1);
        _E ->
            ?WARNING("Error data got in ets ~p for id ~w, got: ~p", [Table, ID, _E]),
            find_in_ets(Table, T, Compress, RecordL)
    end.


%% fold
kv_fold(Func, Acc, Table) ->
    KeyPos = ets:info(Table, keypos),
    case select_kv_first_data(Table, ?MAX_READ_NUM) of
        {ok, [_ | _] = DateList} ->
            Acc1 = lists:foldl(Func, Acc, DateList),
            Last = lists:last(DateList),
            LastKey = erlang:element(KeyPos, Last),
            kv_fold_1(Func, Acc1, Table, KeyPos, LastKey, ?MAX_READ_NUM);
        _ ->
            Acc
    end.

kv_fold_1(Func, Acc, Table, KeyPos, LastKey, MaxRead) ->
    case select_kv_next_data(Table, LastKey, MaxRead) of
        {ok, [_ | _] = DateList} ->
            Acc1 = lists:foldl(Func, Acc, DateList),
            Last = lists:last(DateList),
            LastKey1 = erlang:element(KeyPos, Last),
            kv_fold_1(Func, Acc1, Table, KeyPos, LastKey1, MaxRead);
        _ ->
            Acc
    end.

%%% ------------------------------- insert -------------------------------
%% kv类型的数据插入, 对应表结构 id, data, (gmt_modified, gmt_create)
insert_kvs(Table, Objects, KeyPos) ->
    insert_kvs(Table, Objects, KeyPos, 0).

insert_kvs(_Table, [], _KeyPos, _Compress) ->
    ok;
insert_kvs(Table, Objects, KeyPos, Compress) ->
    F = fun(E, AccIn) ->
            %% 外部传入正常数据, 在此进行term_to_binary转换
            ID = erlang:element(KeyPos, E),
            Data = erlang:term_to_binary(E, [{compressed, Compress}]),
            [#kv_id_data{id = ID, data = Data} | AccIn]
        end,
    Records = lists:foldr(F, [], Objects),
    db_mysql_command:insert(Table, [id, data], Records).

%%% ------------------------------- select -------------------------------
%% 特殊结构的操作
%% select key value结构的数据 data字段
select_kv_data(Table, ValueOfKey) ->
    case db_mysql_command:select_by_key(Table, id, ValueOfKey, [data]) of
        {ok, {Table, Data}} ->
            {ok, erlang:binary_to_term(Data)};
        Res ->
            Res
    end.

select_kv_many_data(Table, ValueOfKeyL) ->
    case db_mysql_command:select_by_key(Table, id, ValueOfKeyL, [data]) of
        {ok, TupleList} ->
            DataList = [erlang:binary_to_term(E) || {_, E} <- TupleList],
            {ok, DataList};
        Res ->
            Res
    end.

select_kv_seq_data(Table, From, Len) ->
    case db_mysql_command:select_seq(Table, [data], From, Len) of
        {ok, TupleList} ->
            DataList = [erlang:binary_to_term(E) || {_, E} <- TupleList],
            {ok, DataList};
        Res ->
            Res
    end.

select_kv_first_data(Table, Len) ->
    case db_mysql_command:select_first(Table, id, [data], Len) of
        {ok, TupleList} ->
            DataList = [erlang:binary_to_term(E) || {_, E} <- TupleList],
            {ok, DataList};
        Res ->
            Res
    end.

select_kv_next_data(Table, ValueOfKey, Len) ->
    case db_mysql_command:select_next(Table, id, ValueOfKey, [data], Len) of
        {ok, TupleList} ->
            DataList = [erlang:binary_to_term(E) || {_, E} <- TupleList],
            {ok, DataList};
        Res ->
            Res
    end.

select_kv_max_id(Table) ->
    db_mysql_command:select_max(Table, id).

%%% ------------------------------- delete -------------------------------
%%
delete_kv(Table, KeyOfValue) ->
    db_mysql_command:delete_one(Table, id, KeyOfValue).

%%
delete_kv_many(Table, KeyOfValueL) ->
    db_mysql_command:delete_many(Table, id, KeyOfValueL).
