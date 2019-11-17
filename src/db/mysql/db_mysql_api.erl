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
    insert_user_master/1
]).

%% select
-export([
    select_user_master/1,
    select_user_master_max_user_id/0,
    select_index/0
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
        "CREATE TABLE IF NOT EXISTS `index`(
           `key` varchar(255) CHARACTER SET utf8 COLLATE utf8_unicode_ci DEFAULT NULL COMMENT 'index的key',
             `value` int(10) unsigned DEFAULT NULL COMMENT 'index值'
             ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;",
    ok = mysql_poolboy:query(?POOL_NAME, Sql).


%%% ------------------------------- insert -------------------------------
%% 插入玩家信息
insert_user_master(UserMaster) ->
    Fields = record_info(fields, user_master),
    Len = record_info(size, user_master) - 1,
    insert(Fields, Len, UserMaster).

insert(Fields, Len, Record) ->
    FieldsStr = make_fields_str(Fields),
    ParamMarkStr = make_param_mark_str(Len),
    Sql = "INSERT INTO user_master (" ++ FieldsStr ++ ") VALUES (" ++ ParamMarkStr ++ ");",
    [_ | ValuesL] = erlang:tuple_to_list(Record),
    ok = mysql_poolboy:query(?POOL_NAME, Sql, ValuesL).


%%% ------------------------------- update -------------------------------


%%% ------------------------------- select -------------------------------
%% 判断账号是否存在
select_user_master(Account) ->
    Fields = record_info(fields, user_master),
    case select_by_key(user_master, account_name, Fields, Account) of
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
        {ok, _, [List]} ->
            UserMaster = erlang:list_to_tuple([user_master | List]),
            {ok, UserMaster};
        _ ->
            ?false
    end.

%% 获取所有index的数据
select_index() ->
    case mysql_poolboy:query(?POOL_NAME, "SELECT * FROM `index`") of
        {ok, _, List} ->
            KVList = [{util:bitstring_to_term(BinKey), Value} || [BinKey, Value] <- List],
            KVList;
        _Err ->
            ?WARNING("select index fail, Err:~w", [_Err]),
            []
    end.

%% 根据key获取table中的fields对应的值
select_by_key(Table, Key, Fields, ValueOfKey) ->
    TableStr = erlang:atom_to_list(Table),
    KeyStr = erlang:atom_to_list(Key),
    Sql = "SELECT " ++ Fields ++ " FROM `" ++ TableStr ++ "` WHERE `" ++ KeyStr ++ "` = ?",
    mysql_poolboy:query(?POOL_NAME, Sql, [ValueOfKey]).

%%% --------------------------------------------------------
%%%                     Local Function
%%% --------------------------------------------------------
make_fields_str(Fields) ->
    [_ | T] = util:term_to_string(Fields),
    [_ | T1] = lists:reverse(T),
    lists:reverse(T1).

make_param_mark_str(Len) ->
    [_ | T] = lists:concat(lists:duplicate(Len, ",?")),
    T.
