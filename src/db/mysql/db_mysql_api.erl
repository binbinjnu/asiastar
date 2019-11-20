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
        "CREATE TABLE IF NOT EXISTS `index_t`(" ++
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
    Fields = record_info(fields, user_master_t),
    db_mysql_command:insert_one(user_master_t, Fields, UserMaster).

%% 插入玩家金钱信息
insert_user_money(UserMoney) ->
    Fields = record_info(fields, user_money_t),
    db_mysql_command:insert_one(user_money_t, Fields, UserMoney).


%%% ------------------------------- replace -------------------------------


%%% ------------------------------- update -------------------------------
%% 更新玩家金钱
update_user_money(#user_money_t{user_id = UserID} = UserMoney) ->
    Fields = record_info(fields, user_money_t),
    db_mysql_command:update_by_key(user_money_t, user_id, UserID, Fields, UserMoney).

%%% ------------------------------- select -------------------------------
%% 判断账号是否存在
select_user_master(Account) ->
    Fields = record_info(fields, user_master_t),
    db_mysql_command:select_by_key(user_master_t, account, Account, Fields).

%% 获取最大user_id
select_user_master_max_user_id() ->
    case mysql_poolboy:query(?POOL_NAME, "SELECT max(user_id) FROM user_master_t;") of
        {ok, _, [[MaxUserID]]} ->
            {ok, MaxUserID};
        _Err ->
            ?WARNING("select user master max user id fail, Err:~w", [_Err]),
            ?false
    end.

%% 获取所有index的数据
select_index() ->
    Fields = record_info(fields, index_t),
    {ok, RecordL} = db_mysql_command:select_all(index_t, Fields),
    [{util_code:bitstring_to_term(BK), V} || #index_t{index_k = BK, index_v = V} <- RecordL].

%% 获取玩家金钱
select_user_money(UserID) ->
    Fields = record_info(fields, user_money_t),
    db_mysql_command:select_by_key(user_money_t, user_id, UserID, Fields).
