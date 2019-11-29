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

-export([
    open/2,
    open_kv/2
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

make_opts() ->
    [Host, Port, User, Passwd, DB, ConnNum] = config:get(mysql),
    PoolOpts  = [{size, ConnNum}, {max_overflow, 2 * ConnNum}],
    MySqlOpts = [{host, Host}, {port, Port}, {user, User}, {password, Passwd}, {database, DB}],
    {PoolOpts, MySqlOpts}.


%% 关闭mysql连接池
stop_mysql() ->
    ok = supervisor:terminate_child(mysql_poolboy_sup, ?POOL_NAME),
    ok = supervisor:delete_child(mysql_poolboy_sup, ?POOL_NAME),
    %% todo 需要加入服务器状态
    ok.



%% 打开一个数据库表, 如果不存在则新建
%% Opts为Proplists, 可用的参数有:
%% [
%%  %% ets 初始化的一些参数, ets_svr中用到
%%  %% 比如:
%%      set | ordered_set | bag | duplicate_bag
%%      public | private | protected
%%      write_concurrency
%%      read_concurrency
%%      {keypos, V}
%%
%%  %% 数据存储等需要用到的参数, db_mysql_svr和db_mysql_kv_api中用到
%%  %% 例子: 对应hrl_db中的记录test_t
%%      {rec_name, test_t}          对应的记录名, 一般跟表名一样. kv的跟表名不同, 用 kv_id_data
%%      {key, test_id}              主键字段名. kv的用id
%%      {fields, [t_id, t_str, t_tuple, t_list]}
%%                                  对应fields, 一般是RecordName对应的 record_info(fields, rec_name)
%%                                  注意, record_info(fields, rec_name) 中rec_name必须是显式的atom, 不能用参数
%%                                  如果是kv_id_data的, 用 [id, data]
%%      {transfer, [#test_t.t_tuple, #test_t.t_list]}
%%                                  需要转换的element key, term_to_bitstring和bitstring_to_term
%%                                  会将对应的字段进行转换
%%      {compress, Levels}          压缩等级, 一般用于 kv_id_data
%%
%%  %% 创建数据表用, 一般用于open_kv中创建 {id, data, gmt_modified, gmt_create} 结构的表
%%      {key_format, Format}        int | uint | {uint, N} | {int, N} | varbinary | {varbinary, N}
%%      index_modified              表示是否对修改时间进行索引, 没有改参数表示 false
%% ]
open(Table, Opts) ->
    new_ets(Table, Opts),
    Opts1 = [{recname, Table} | Opts],
    {ok, _} = db_mysql_svr:start(Table, Opts1),
    ok.

open_kv(Table, Opts) ->
    new_ets(Table, Opts),
    Opts1 =
        [
            {recname, kv_id_data},
            {key, id},
            {fields, [id, data]} | Opts
        ],
    {ok, _} = db_mysql_svr:start(Table, Opts1),
    %% kv的表在此自动创建
    db_mysql_kv_api:ensure_kv_table(Table, Opts1),
    ok.

new_ets(Table, Opts) ->
    case proplists:get_bool(protected, Opts) orelse proplists:get_bool(private, Opts) of
        ?true ->
            ets_svr:new(Table, Opts);
        _ ->
            ets_svr:hold_new(Table, Opts)
    end.