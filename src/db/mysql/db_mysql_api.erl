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


-export([
    prepare_list/0,
    is_database_exist/1,
    change_database/1,
    create_database/1,
    select_user_master/1
]).

prepare_list() ->
    [

    ].

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
    Sql = "CREATE DATABASE IF NOT EXISTS `" ++ DB ++ "` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci */ /*!80016 DEFAULT ENCRYPTION='N' */;",
    ok = mysql_poolboy:query(?POOL_NAME, Sql).

%% 判断账号是否存在
select_user_master(Account) ->
    case mysql_poolboy:query(?POOL_NAME, "SELECT * FROM user_master WHERE account_name = ?", [Account]) of
        {ok, _, [List]} ->
             UserMaster = erlang:list_to_tuple([user_master | List]),
            {ok, UserMaster};
        _ ->
            ?false
    end.