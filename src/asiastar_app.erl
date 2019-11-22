%%%-------------------------------------------------------------------
%% @doc asiastar public API
%% @end
%%%-------------------------------------------------------------------

-module(asiastar_app).


-behaviour(application).

-include("hrl_logs.hrl").
-include("hrl_common.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = pre_start(),
    ok = config:start(),

    {ok, Sup} = ?MAIN_SUP:start_link(),
    %% 游戏启动db, 有些factory需要依赖db
    start_db(),

    %% 开启主监控树后执行
    start_factory(),

    %% 开启net, 需要在start_factory后, 游戏需要依赖factory中的服务
    start_net(),

    %% handle文件夹中的各种进程,监控树加载
    start_handle(),

    after_start(),

    {ok, Sup}.

%% 开启各个服务进程或者服务监控树
start_factory() ->
    %% ets服务进程
    {ok, _} = ?MAIN_SUP:start_child(ets_gsvr),
    %% 定时器进程
    {ok, _} = ?MAIN_SUP:start_child(timer_gsvr),
    %% gc进程
    {ok, _} = ?MAIN_SUP:start_child(background_gc_gsvr),
    %% 群组监控树
    {ok, _} = ?MAIN_SUP:start_child(group_sup, [], supervisor),
    %% 进程检测监控树
    {ok, _} = ?MAIN_SUP:start_child(proc_checker_sup, [], supervisor),
    %% index进程
    {ok, _} = ?MAIN_SUP:start_child(index_gsvr),

    ok.

%% 启动网络服务
start_net() ->
    Ref = config:get_app(),
    Port = config:get(port),
    Opts = #{handler => net_handler, socket_type => tcp},
    {ok, _} = net_api:start_listener(Ref, Port, Opts),  %% 开放网络

    {ok, _} = ?MAIN_SUP:start_child(net_debug_gsvr),      %% 网络包输出管理进程启动
    {ok, _} = proc_checker_sup:start_child(net_checker),  %% 网络进程监控

    ok.

%% 启动数据库服务
start_db() ->
    ok = db_mysql:start_mysql(),
    ok.

%% handle文件夹中的各种进程,监控树加载
start_handle() ->
    %% 登录进程
    {ok, _} = ?MAIN_SUP:start_child(login_gsvr),
    %% 玩家进程监控树
    {ok, _} = ?MAIN_SUP:start_child(player_sup, [], supervisor),
    {ok, _} = proc_checker_sup:start_child(player_checker), %% 玩家进程监控进程

    %% game mgr 监控树和 game 监控树
    {ok, _} = ?MAIN_SUP:start_child(game_mgr_sup, [], supervisor),  %% game mgr监控树
    {ok, _} = game_mgr_sup:start_child(game_sup, [], supervisor),   %% game监控树
    {ok, _} = game_mgr_sup:start_child(game_mgr_main_gsvr), %% 主管理进程
    ok.


%% 在一些服务start后, 启动一些游戏逻辑相关的
after_start() ->
    ets_gsvr:hold_new(ets_uid_acc, [public, named_table, {keypos, 1}]),         %% uid对应账号
    ets_gsvr:hold_new(ets_nickname_uid, [public, named_table, {keypos, 1}]),    %% 角色名对应uid

    group_api:new(world_player),    %% 全局用, 直接new
    group_api:new(world_sender),    %% 全局用, 直接new

    ok.



stop(_State) ->
    ok.

%% internal functions
pre_start() ->
    xref_check(),
    ?NOTICE("after xref check!", []),
    ok.


-ifdef(DEBUG).
xref_check() ->
    ok.

-else.
xref_check() ->
    case xref:d("../ebin") of
        [_, {undefined, []}|_] ->
            ?INFO("Xref check ok", []),
            ok;
        [_, Undef|_] ->
            ?WARNING("Xref check fail: ~p", [Undef]),
            io:format("Xref check fail: ~p~n~n", [Undef]),
            erlang:error(xref_check_fail)
    end.


-endif.