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
    insert_one/3,

    update_by_key/5,

    replace_one/3,
    replace_many/3,

    select_by_key/4,
    select_all/2,

    make_fields_str/1,
    make_param_mark_str/1,
    make_dup_param_mark_str/2,
    make_fields_and_param_mark/1
]).

%%% ------------------------------- create table -------------------------------


%%% ------------------------------- insert -------------------------------

%% 插入一条记录
insert_one(Table, Fields, Record) ->
    FieldsStr = make_fields_str(Fields),
    FieldsLen = erlang:length(Fields),
    ParamMarkStr = make_param_mark_str(FieldsLen),
    TableStr = erlang:atom_to_list(Table),
    Sql = "INSERT INTO " ++ TableStr ++ " (" ++ FieldsStr ++ ") VALUES " ++ ParamMarkStr ++ ";",
    [_ | ValuesL] = erlang:tuple_to_list(Record),
    ?INFO("insert sql:~s, ValuesL:~w", [Sql, ValuesL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL).


%%% ------------------------------- replace -------------------------------

%% replace原理是先删除再insert, 所以需要所以字段齐全
replace_one(Table, Fields, Record) ->
    FieldsStr = make_fields_str(Fields),
    FieldsLen = erlang:length(Fields),
    ParamMarkStr = make_param_mark_str(FieldsLen),
    TableStr = erlang:atom_to_list(Table),
    Sql = "REPLACE INTO " ++ TableStr ++ " (" ++ FieldsStr ++ ") VALUES " ++ ParamMarkStr ++ ";",
    [_ | ValuesL] = erlang:tuple_to_list(Record),
    ?INFO("replace sql:~s, ValuesL:~w", [Sql, ValuesL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL).

%% Records数量不能过多
replace_many(Table, Fields, Records) ->
    FieldsStr = make_fields_str(Fields),
    FieldsLen = erlang:length(Fields),
    RecordsLen = erlang:length(Records),
    ParamMarkStr = make_dup_param_mark_str(FieldsLen, RecordsLen),
    TableStr = erlang:atom_to_list(Table),
    Sql = "REPLACE INTO " ++ TableStr ++ " (" ++ FieldsStr ++ ") VALUES " ++ ParamMarkStr ++ ";",
    ValuesL =
        [begin
             [_ | T] = erlang:tuple_to_list(E),
             T
         end || E <- Records],
    ?INFO("replace sql:~s, ValuesL:~w", [Sql, ValuesL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL).

%%% ------------------------------- update -------------------------------

%% update一定需要where!!!! 不然会阳光普照!!!
%% 更新一条记录
update_by_key(Table, Key, KeyOfValue, Fields, Record) ->
    FieldsAndParamStr = make_fields_and_param_mark(Fields),
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    Sql = "UPDATE  " ++ TableStr ++ " SET " ++ FieldsAndParamStr ++ " WHERE `" ++ KeyStr ++ "` = ?;",
    [_ | ValuesL] = erlang:tuple_to_list(Record),
    ?INFO("update sql:~s, ValuesL:~w", [Sql, ValuesL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL ++ [KeyOfValue]).


%%% ------------------------------- select -------------------------------

%% 根据key获取table中的fields对应的值
%% 如果record中的某些字段需要进行字段转换, 调用方各自处理
select_by_key(Table, Key, ValueOfKey, Fields) ->
    FieldsStr = make_fields_str(Fields),
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    Sql = "SELECT " ++ FieldsStr ++ " FROM `" ++ TableStr ++ "` WHERE `" ++ KeyStr ++ "` = ?;",
    ?INFO("Sql:~s", [Sql]),
    case mysql_poolboy:query(?POOL_NAME, Sql, [ValueOfKey]) of
        {ok, _, [List]} ->
            Record = erlang:list_to_tuple([Table | List]),
            {ok, Record};
        {ok, _, []} ->
            ?NULL_VAL;
        _Err ->
            ?WARNING("select ~w fail, Err:~w", [Table, _Err]),
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
        _Err ->
            ?WARNING("select index fail, Err:~w", [_Err]),
            {ok, []}
    end.


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

