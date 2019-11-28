%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     由于各种原因可能导致连接断掉,
%%%     断掉后虽然会重连, 但重连次数超过监控树重启策略后, application会挂掉
%%%     需要有个timer定时检查application
%%% @end
%%% Created : 07. 11月 2019 10:05
%%%-------------------------------------------------------------------
-module(db_mysql).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_db.hrl").

%% API
-export([
    start_mysql/0,
    check_mysql/0,
    stop_mysql/0
]).

start_mysql() ->
    ?NOTICE("Start mysql!"),
    %% 判断是否有DB, 没有的话建立数据库
    pre_start(),
    do_start(),
    check_start(),
    ?NOTICE("Start mysql finish!"),
    ok.

%% 启动1个连接, 判断是否有库, init 或 updata
pre_start() ->
    [Host, Port, User, Passwd, DB, _ConnNum] = config:get(mysql),
    PoolOpts  = [{size, 1}, {max_overflow, 1}],
    MySqlOpts = [{host, Host}, {port, Port}, {user, User}, {password, Passwd}],
    {ok, _} = mysql_poolboy:add_pool(?POOL_NAME, PoolOpts, MySqlOpts),
    case db_mysql_api:is_database_exist(DB) of
        ?false ->        %% 数据库不存在, 建库建表
            ?NOTICE("Database ~s do not exist, init db!", [DB]),
            init_db(DB),
            stop_mysql();
        ?true ->    %% 数据库存在
            ?NOTICE("Database ~s exist, update db!", [DB]),
            update_db(DB),
            stop_mysql();
        Err ->
            ?WARNING("pre_start fail, Err:~w", [Err]),
            erlang:error(check_db_exist_fail)
    end.

init_db(DB) ->
    db_mysql_api:create_database(DB),
    db_mysql_api:change_database(DB),
    ok.

update_db(DB) ->
    db_mysql_api:change_database(DB),
    ok.

%% 正常启动数据库连接
do_start() ->
    {PoolOpts, MySqlOpts} = make_opts(),
    {ok, _} = mysql_poolboy:add_pool(?POOL_NAME, PoolOpts, MySqlOpts),
    ok.

%% 启动检测, 1分钟检测一次
check_start() ->
    CronTime = {every, {1, min}, {between, {0, 1, am}, {11, 59, pm}}},
    CronMFA = {?MODULE, check_mysql, []},
    erlcron:cron({{daily, CronTime}, CronMFA}),
    ok.

%% 检查mysql的application是否还在
check_mysql() ->
    %% todo 需要判断服务器状态
    application:ensure_started(mysql_poolboy),
    {PoolOpts, MySqlOpts} = make_opts(),
    mysql_poolboy:add_pool(?POOL_NAME, PoolOpts, MySqlOpts),
    ok.

%% 关闭mysql连接池
stop_mysql() ->
    ok = supervisor:terminate_child(mysql_poolboy_sup, ?POOL_NAME),
    ok = supervisor:delete_child(mysql_poolboy_sup, ?POOL_NAME),
    %% todo 需要加入服务器状态
    ok.

make_opts() ->
    [Host, Port, User, Passwd, DB, ConnNum] = config:get(mysql),
    PoolOpts  = [{size, ConnNum}, {max_overflow, 2 * ConnNum}],
    MySqlOpts = [{host, Host}, {port, Port}, {user, User}, {password, Passwd}, {database, DB}],
    {PoolOpts, MySqlOpts}.