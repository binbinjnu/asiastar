%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     数据库语句拼接\执行模块
%%% @end
%%% Created : 19. 11月 2019 10:11
%%%-------------------------------------------------------------------
-module(db_mysql_command).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_db.hrl").

%% test
-export([
    disk_tables/0,
    disk_tables/1,

    insert/3,

    select_by_key/4,
    select_many/4,
    select_seq/4,
    select_first/4,
    select_next/5,
    select_all/2,
    select_table_size/1,
    select_max/2,

    delete_one/3,
    delete_many/3,
    truncate/1
]).

-export([
    make_fields_str/1,
    make_param_mark_str/1,
    make_dup_param_mark_str/2,
    make_fields_and_param_mark/1
]).

%%% ------------------------------- table -------------------------------
%% 对应数据库中的所有表
disk_tables() ->
    [_Host, _Port, _User, _Passwd, DB, _ConnNum] = config:get(mysql),
    disk_tables(DB).

disk_tables(DBName) ->
    Sql = "SELECT table_name FROM information_schema.tables WHERE table_schema = ?;",
    {ok, _, L} = mysql_poolboy:query(?POOL_NAME, Sql, [DBName]),
    [erlang:binary_to_atom(E, utf8) || [E] <- L].

%%% ------------------------------- insert -------------------------------
%% 插入记录, 如果存在, 则update
insert(Table, Fields, Record) when is_tuple(Record) ->
    insert(Table, Fields, [Record]);
insert(Table, Fields, Records) when is_list(Records) ->
    FieldsStr = make_fields_str(Fields),
    FieldsLen = erlang:length(Fields),
    RecordsLen = erlang:length(Records),
    ParamMarkStr = make_dup_param_mark_str(FieldsLen, RecordsLen),
    TableStr = erlang:atom_to_list(Table),
    F = fun(E, AccIn) ->
            [_ | T] = erlang:tuple_to_list(E),
            T ++ AccIn
        end,
    ValuesL = lists:foldr(F, [], Records),
    UpdateValueStr = make_update_values(Fields),
    Sql = "INSERT INTO " ++ TableStr ++
        " (" ++ FieldsStr ++ ") VALUES " ++ ParamMarkStr ++
        " ON DUPLICATE KEY UPDATE " ++ UpdateValueStr ++ ";",
    ?INFO("sql:~s, ValuesL:~w", [Sql, ValuesL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL).

%%% ------------------------------- select -------------------------------
%% 根据key获取table中的fields对应的值
%% 如果record中的某些字段需要进行字段转换, 调用方各自处理
select_by_key(Table, Key, ValueOfKey, Fields) ->
    FieldsStr = make_fields_str(Fields),
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    Sql = "SELECT " ++ FieldsStr ++
        " FROM `" ++ TableStr ++
        "` WHERE `" ++ KeyStr ++ "` = ?;",
    ?INFO("Sql:~s", [Sql]),
    case mysql_poolboy:query(?POOL_NAME, Sql, [ValueOfKey]) of
        {ok, _, [List]} ->
            Record = erlang:list_to_tuple([Table | List]),
            {ok, Record};
        {ok, _, []} ->
            ?NULL_VAL;
        _Err ->
            ?WARNING("select by key ~w fail, Err:~w", [Table, _Err]),
            ?false
    end.

%% 获取一定对应keys的数据
select_many(Table, Key, ValueOfKeyL, Fields) ->
    FieldsStr = make_fields_str(Fields),
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    ParamMarkStr = db_mysql_command:make_param_mark_str(erlang:length(ValueOfKeyL)),
    Sql = "SELECT " ++ FieldsStr ++
        " FROM `" ++ TableStr ++
        "` WHERE `" ++ KeyStr ++ "` in " ++ ParamMarkStr ++ ";",
    ?INFO("Sql:~s", [Sql]),
    case mysql_poolboy:query(?POOL_NAME, Sql, ValueOfKeyL) of
        {ok, _, Lists} ->
            RecordL = [erlang:list_to_tuple([Table | E]) || E <- Lists],
            {ok, RecordL};
        _Err -> %% 此处需要返回false, 上层调用需要报错或直接匹配异常
            ?WARNING("select many ~w fail, Err:~w", [Table, _Err]),
            ?false
    end.

%% 从From开始获取Len的数据
%% From对应行不计入
select_seq(Table, Fields, From, Len) ->
    FieldsStr = make_fields_str(Fields),
    TableStr = erlang:atom_to_list(Table),
    FromStr = erlang:integer_to_list(From),
    LenStr = erlang:integer_to_list(Len),
    Sql = "SELECT " ++ FieldsStr ++
        " FROM `" ++ TableStr ++
        "` LIMIT " ++ FromStr ++ "," ++ LenStr ++ ";",
    ?INFO("Sql:~s", [Sql]),
    case mysql_poolboy:query(?POOL_NAME, Sql) of
        {ok, _, Lists} ->
            RecordL = [erlang:list_to_tuple([Table | E]) || E <- Lists],
            {ok, RecordL};
        _Err -> %% 此处需要返回false, 上层调用需要报错或直接匹配异常
            ?WARNING("select seq ~w fail, From:~w, Len:~w, Err:~w",
                [Table, From, Len, _Err]),
            ?false
    end.

%% 获取前Len个
select_first(Table, Key, Fields, Len) ->
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    FieldsStr = make_fields_str(Fields),
    LenStr = erlang:integer_to_list(Len),
    Sql = "SELECT " ++ FieldsStr ++
        " FROM `" ++ TableStr ++
        "` ORDER BY `" ++ KeyStr ++
        "` LIMIT " ++ LenStr ++ ";",
    ?INFO("Sql:~s", [Sql]),
    case mysql_poolboy:query(?POOL_NAME, Sql) of
        {ok, _, Lists} ->
            RecordL = [erlang:list_to_tuple([Table | E]) || E <- Lists],
            {ok, RecordL};
        _Err -> %% 此处需要返回false, 上层调用需要报错或直接匹配异常
            ?WARNING("select first ~w fail, Key:~w, Len:~w, Err:~w",
                [Table, Key, Len, _Err]),
            ?false
    end.

%% 获取对应值后Len个 (> key)
select_next(Table, Key, ValueOfKey, Fields, Len) ->
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    FieldsStr = make_fields_str(Fields),
    LenStr = erlang:integer_to_list(Len),
    Sql = "SELECT " ++ FieldsStr ++
        " FROM `" ++ TableStr ++
        "` WHERE `" ++ KeyStr ++ "` > ? ORDER BY `" ++ KeyStr ++
        "` LIMIT " ++ LenStr ++ ";",
    ?INFO("Sql:~s", [Sql]),
    case mysql_poolboy:query(?POOL_NAME, Sql, [ValueOfKey]) of
        {ok, _, Lists} ->
            RecordL = [erlang:list_to_tuple([Table | E]) || E <- Lists],
            {ok, RecordL};
        _Err -> %% 此处需要返回false, 上层调用需要报错或直接匹配异常
            ?WARNING("select next ~w fail, Key:~w, ValueOfKey:~w, Len:~w, Err:~w",
                [Table, Key, ValueOfKey, Len, _Err]),
            ?false
    end.

%% 获取所有
%% 如果record中的某些字段需要进行字段转换, 调用方各自处理
select_all(Table, Fields) ->
    FieldsStr = make_fields_str(Fields),
    TableStr = erlang:atom_to_list(Table),
    Sql = "SELECT " ++ FieldsStr ++ " FROM `" ++ TableStr ++ "`;",
    ?INFO("Sql:~s", [Sql]),
    mysql_poolboy:query(?POOL_NAME, Sql),
    case mysql_poolboy:query(?POOL_NAME, Sql) of
        {ok, _, Lists} ->
            RecordL = [erlang:list_to_tuple([Table | E]) || E <- Lists],
            {ok, RecordL};
        _Err -> %% 此处需要返回false, 上层调用需要报错或直接匹配异常
            ?WARNING("select all fail, Err:~w", [_Err]),
            ?false
    end.

%% 获取table的size
select_table_size(Table) ->
    TableStr = erlang:atom_to_list(Table),
    Sql = "SELECT count(*) FROM `" ++ TableStr ++ "`;",
    case mysql_poolboy:query(?POOL_NAME, Sql) of
        {ok, _, [[Size]]} ->
            {ok, Size};
        _Err -> %% 此处需要返回false, 上层调用需要报错或直接匹配异常
            ?WARNING("select ~w size fail, Err:~w", [Table, _Err]),
            ?false
    end.

%% 获取最大值
select_max(Table, Key) ->
    KeyStr = erlang:atom_to_list(Key),
    TableStr = erlang:atom_to_list(Table),
    Sql = "SELECT max(`" ++ KeyStr ++ "`) FROM `" ++ TableStr ++ "`;",
    case mysql_poolboy:query(?POOL_NAME, Sql) of
        {ok, _, [[Max]]} ->
            {ok, Max};
        _Err -> %% 此处需要返回false, 上层调用需要报错或直接匹配异常
            ?WARNING("select ~w max ~w fail, Err:~w", [Table, Key, _Err]),
            ?false
    end.

%%% ------------------------------- delete -------------------------------
%% 根据主键删除1行
delete_one(Table, Key, ValueOfKey) ->
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    Sql = "DELETE FROM `" ++ TableStr ++ "` WHERE `" ++ KeyStr ++ "` = ?;",
    ?INFO("sql:~s, ValueOfKey:~w", [Sql, ValueOfKey]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, [ValueOfKey]).

%% 根据主键列表删除多行
delete_many(_Table, _Key, []) ->
    ok;
delete_many(Table, Key, ValueOfKeyL) when is_list(ValueOfKeyL) ->
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    ParamMarkStr = make_param_mark_str(erlang:length(ValueOfKeyL)),
    Sql = "DELETE FROM `" ++ TableStr ++
        "` WHERE `" ++ KeyStr ++ "` in " ++ ParamMarkStr ++ ";",
    ?INFO("sql:~s, ValueOfKeyL:~w", [Sql, ValueOfKeyL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValueOfKeyL).

%% 清空表
truncate(Table) ->
    Sql = lists:concat(["TRUNCATE ", Table]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql).

%%% --------------------------------------------------------
%%%                     Local Function
%%% --------------------------------------------------------
%% 将tuple或list转成去掉括号的string
%% [a, b, c] => "a, b, c"
%% {a, b, c} => "a, b, c"
make_fields_str(Fields) ->
    [_ | T] = util_code:term_to_string(Fields),
    [_ | T1] = lists:reverse(T),
    lists:reverse(T1).

%% 生成对应数量的?
%% 3 => "(?, ?, ?)"
make_param_mark_str(Len) ->
    [_ | T] = lists:concat(lists:duplicate(Len, ",?")),
    "(" ++ T ++ ")".

%% 生成Len1个?, Len2个(?, ?, ..)
%% 3, 2 => "(?,?,?),(?,?,?)"
make_dup_param_mark_str(Len1, Len2) ->
    Str = make_param_mark_str(Len1),
    [_ | T] = lists:concat(lists:duplicate(Len2, "," ++ Str)),
    T.

%% 将list中的元素和?拼接
%% [a, b, c] => "a=?, b=?, c=?"
make_fields_and_param_mark(Fields) ->
    Str = lists:concat([erlang:atom_to_list(E) ++ "=?," || E <- Fields]),
    [_ | T] = lists:reverse(Str),
    lists:reverse(T).


%% [a, b, c] => "a=VALUES(a),b=VALUES(b),c=VALUES(c)"
make_update_values(Fields) ->
    make_update_values(Fields, []).

make_update_values([], [_ | Str]) ->
    Str;
make_update_values([Field | T], Str) ->
    FieldStr = erlang:atom_to_list(Field),
    Str1 = Str ++ "," ++ FieldStr ++ "=VALUES(" ++ FieldStr ++ ")",
    make_update_values(T, Str1).