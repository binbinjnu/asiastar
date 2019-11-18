%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     常用的数据库语句接口
%%%     mysql_poolboy:query 和 mysql_poolboy:execute 返回格式
%%%     ok
%%%     | {ok, column_names(), rows()}              单语句结果
%%%     | {ok, [{column_names(), rows()}, ...]}     多语句结果
%%%     | {error, server_reason()}.
%%%
%%%     column_names(): 字段名行 [字段名1, 字段名2 ...]
%%%     rows(): n行m列 [ [行1字段1, 行1字段2 ...], [行2字段1, 行2字段2 ...] ... ]
%%% @end
%%% Created : 06. 11月 2019 16:54
%%%-------------------------------------------------------------------
-module(db_mysql_api).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_db.hrl").


%% 基础
-export([
    prepare_list/0,
    is_database_exist/1,
    change_database/1,
    create_database/1
]).

%% 建表
-export([
    create_table_index/0
]).

%% insert
-export([
    insert_user_master/1,
    insert_user_money/1
]).

%% update
-export([
    update_user_money/1
]).

%% select
-export([
    select_user_master/1,
    select_user_money/1,
    select_user_master_max_user_id/0,
    select_index/0
]).

%% test
-export([
    insert_one/3,
    update_one/5,
    make_fields_str/1,
    make_param_mark_str/1,
    make_dup_param_mark_str/2,
    make_fields_and_param_mark/1
]).

prepare_list() ->
    [

    ].

%%% ------------------------------- base -------------------------------
%% 判断库是否存在
is_database_exist(DB) ->
    Sql = "SELECT count(SCHEMA_NAME) FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME=?",
    case mysql_poolboy:query(?POOL_NAME, Sql, [DB]) of
        {ok, _, [[1]]} ->
            ?true;
        {ok, _, [[0]]} ->
            ?false;
        Err ->
            {error, Err}
    end.

%% 切换数据库
change_database(DB) ->
    Sql = "use " ++ DB,
    ok = mysql_poolboy:query(?POOL_NAME, Sql).

%% 创建数据库
create_database(DB) ->
    Sql =
        "CREATE DATABASE IF NOT EXISTS `" ++
        DB ++
        "` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci */ /*!80016 DEFAULT ENCRYPTION='N' */;",
    ok = mysql_poolboy:query(?POOL_NAME, Sql).


%%% ------------------------------- create table -------------------------------
%% 创建index表
create_table_index() ->
    %% 判断数据库中表是否存在, 不存在则创建
    Sql =
        "CREATE TABLE IF NOT EXISTS `index`(" ++
           "`key` varchar(255) CHARACTER SET utf8 COLLATE utf8_bin DEFAULT NULL COMMENT 'index key'," ++
             "`value` int(10) unsigned DEFAULT NULL COMMENT 'index value'" ++
             ") ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;",
%%    ?INFO("Sql:~s", [Sql]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql).

%% 创建user_master表

%% 创建user_money表


%%% ------------------------------- insert -------------------------------
%% 插入玩家信息
insert_user_master(UserMaster) ->
    Fields = record_info(fields, user_master),
    insert_one(user_master, Fields, UserMaster).

%% 插入玩家金钱信息
insert_user_money(UserMoney) ->
    Fields = record_info(fields, user_money),
    insert_one(user_money, Fields, UserMoney).

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
    ?INFO("insert sql:~s, ValuesL:~w", [Sql, ValuesL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL).

%% Records数量不能过多
replace_many(Table, Fields, Records) ->
    FieldsStr = make_fields_str(Fields),
    FieldsLen = erlang:length(Fields),
    RecordsLen = erlang:length(Records),
    ParamMarkStr = make_dup_param_mark_str(FieldsLen, RecordsLen),
    TableStr = erlang:atom_to_list(Table),
    Sql = "REPLACE INTO " ++ TableStr ++ " (" ++ FieldsStr ++ ") VALUES " ++ ParamMarkStr ++ ";",

    ok.




%%% ------------------------------- update -------------------------------
%% 更新玩家金钱
update_user_money(#user_money{user_id = UserID} = UserMoney) ->
    Fields = record_info(fields, user_money),
    update_one(user_money, user_id, UserID, Fields, UserMoney).

%% update一定需要where!!!! 不然会阳光普照!!!
%% 更新一条记录
update_one(Table, Key, KeyOfValue, Fields, Record) ->
    FieldsAndParamStr = make_fields_and_param_mark(Fields),
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    Sql = "UPDATE  " ++ TableStr ++ " SET " ++ FieldsAndParamStr ++ " WHERE `" ++ KeyStr ++ "` = ?;",
    [_ | ValuesL] = erlang:tuple_to_list(Record),
    ?INFO("update sql:~s, ValuesL:~w", [Sql, ValuesL]),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL ++ [KeyOfValue]).


%%% ------------------------------- select -------------------------------
%% 判断账号是否存在
select_user_master(Account) ->
    Fields = record_info(fields, user_master),
    case select_by_key(user_master, account, Fields, Account) of
        {ok, _, [List]} ->
             UserMaster = erlang:list_to_tuple([user_master | List]),
            {ok, UserMaster};
        {ok, _, []} ->
            ?NULL_VAL;
        _Err ->
            ?WARNING("select user master fail, Err:~w", [_Err]),
            ?false
    end.

%% 获取最大user_id
select_user_master_max_user_id() ->
    case mysql_poolboy:query(?POOL_NAME, "SELECT max(user_id) FROM user_master;") of
        {ok, _, [[MaxUserID]]} ->
            {ok, MaxUserID};
        _Err ->
            ?WARNING("select user master max user id fail, Err:~w", [_Err]),
            ?false
    end.

%% 获取所有index的数据
select_index() ->
    case mysql_poolboy:query(?POOL_NAME, "SELECT * FROM `index`") of
        {ok, _, List} ->
            KVList = [{util_code:bitstring_to_term(BinKey), Value} || [BinKey, Value] <- List],
            KVList;
        _Err ->
            ?WARNING("select index fail, Err:~w", [_Err]),
            []
    end.

%% 获取玩家金钱
select_user_money(UserID) ->
    Fields = record_info(fields, user_money),
    case select_by_key(user_money, user_id, Fields, UserID) of
        {ok, _, [List]} ->
            UserMoney = erlang:list_to_tuple([user_money | List]),
            {ok, UserMoney};
        {ok, _, []} ->
            ?NULL_VAL;
        _Err ->
            ?WARNING("select user moeny fail, Err:~w", [_Err]),
            ?false
    end.

%% 根据key获取table中的fields对应的值
select_by_key(Table, Key, Fields, ValueOfKey) ->
    FieldsStr = make_fields_str(Fields),
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    Sql = "SELECT " ++ FieldsStr ++ " FROM `" ++ TableStr ++ "` WHERE `" ++ KeyStr ++ "` = ?;",
    ?INFO("Sql:~s", [Sql]),
    mysql_poolboy:query(?POOL_NAME, Sql, [ValueOfKey]).

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
