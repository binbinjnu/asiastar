%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 11月 2019 15:50
%%%-------------------------------------------------------------------
-module(xf_sql_command).
-author("Administrator").

-include("hrl_logs.hrl").
-include("hrl_db.hrl").
-include("hrl_common.hrl").

%% API
-export([
    disk_tables/0,
    disk_tables/1,

    create_kv_table/2,
    truncate/1,

    kv_insert/3,

    kv_delete/2,
    kv_delete_many/2,

    kv_insert_from_ets/2,
    kv_insert_from_ets/3
]).

-define(MAX_INSERT_NUM, 300).   %% 一次最大插入数
-define(MAX_READ_NUM, 300).     %% MySQL 一次读取最大条数
-define(MAX_RETRY_NUM, 3).      %% MySQL 最大重试次数

%% 对应数据库中的所有表
disk_tables() ->
    [_Host, _Port, _User, _Passwd, DB, _ConnNum] = config:get(mysql),
    disk_tables(DB).

disk_tables(DBName) ->
    Sql = "SELECT table_name FROM information_schema.tables WHERE table_schema = ?;",
    {ok, _, L} = mysql_poolboy:query(?POOL_NAME, Sql, [DBName]),
    [erlang:binary_to_atom(E, utf8) || [E] <- L].


%% 创建kv类型的表
create_kv_table(Table, Opts) ->
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


%% 清空表
truncate(Table) ->
    Sql = lists:concat(["TRUNCATE ", Table]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql).


%% 直接向数据库中插入, 当数据量过大时自动分段, 全部成功返ok
kv_insert(Table, Objects, KeyPos) ->
    kv_insert_retry(Table, Objects, KeyPos, 1, 0).

%% 带重试次数的insert
kv_insert_retry(_Table, [], _KeyPos, _MaxRetry, _N) ->
    ok;
kv_insert_retry(Table, Objects, KeyPos, MaxRetry, N) ->
    F = fun(Data, Success) ->
            Success andalso db_mysql_command:insert_kvs(Table, Data, KeyPos) =:= ok
        end,
    case batch(F, ?true, Objects, ?MAX_INSERT_NUM) of
        ?true ->
            ok;
        _Error when N < MaxRetry ->
            kv_insert_retry(Table, Objects, KeyPos, MaxRetry, N + 1);
        Error ->
            Error
    end.


%%
kv_delete(Table, KeyOfValue) ->
    db_mysql_command:delete_one(Table, id, KeyOfValue).

%%
kv_delete_many(Table, KeyOfValueL) ->
    db_mysql_command:delete_many(Table, id, KeyOfValueL).


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
            Objects = find_in_est(Table, IDs),
            Success andalso db_mysql_command:insert_kvs(Table, Objects, KeyPos, Compress) =:= ok
        end,
    case batch(F, ?true, Keys, ?MAX_INSERT_NUM) of
        ?true ->
            ok;
        _Error when N < MaxRetry ->
            kv_insert_from_ets_retry(Table, Keys, KeyPos, Compress, MaxRetry, N + 1);
        Error ->
            Error
    end.

find_in_est(Table, IDs) ->
    find_in_est(Table, IDs, []).

find_in_est(_Table, [], AccData) ->
    AccData;
find_in_est(Table, [ID|T], AccData) ->
    case ets:lookup(Table, ID) of
        [Data] ->
            Data1 = [Data|AccData],
            find_in_est(Table, T, Data1);
        _E ->
            ?WARNING("Error data got in ets ~p for id ~w, got: ~p", [Table, ID, _E]),
            find_in_est(Table, T, AccData)
    end.




%%% --------------------------------------
%%%         Local Function
%%% --------------------------------------
%% 批量, 每个分段最大为Max, 对每个分段执行Func
batch(Func, Acc0, Data, Max) when is_list(Data)->
    batch(Func, Acc0, Data, Max, [], 0).

batch(Func, Acc0, Remain, Max, AccData, N) when N >= Max ->
    Acc1 = Func(AccData, Acc0),
    batch(Func, Acc1, Remain, Max, [], 0);
batch(Func, Acc0, []=_Remain, _Max, AccData, _) ->
    Func(AccData, Acc0);
batch(Func, Acc0, [H|T], Max, AccData, N) ->
    batch(Func, Acc0, T, Max, [H|AccData], N+1).