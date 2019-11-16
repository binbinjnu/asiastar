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
    config:start(),

    {ok, Sup} = ?MAIN_SUP:start_link(),

    %% 开启主监控树后执行
    start_factory(),

    %% net和db比较独立, 不放start_factory中
    start_net(),
    start_db(),

    after_start(),

    {ok, Sup}.

%% 开启各个服务进程或者服务监控树
start_factory() ->
    %% ets服务进程
    ?MAIN_SUP:start_child(ets_gsvr),
    %% 定时器进程
    ?MAIN_SUP:start_child(timer_gsvr),
    %% gc进程
    ?MAIN_SUP:start_child(background_gc_gsvr),
    %% 群组监控树
    ?MAIN_SUP:start_child(group_sup, [], supervisor),
    %% 进程检测监控树
    ?MAIN_SUP:start_child(proc_checker_sup, [], supervisor),

    ok.

%% 启动网络服务
start_net() ->
    Ref = config:get_app(),
    Port = config:get(port),
    Opts = #{handler => net_handler, socket_type => tcp},
    {ok, _} = net_api:start_listener(Ref, Port, Opts),  %% 开放网络

    ?MAIN_SUP:start_child(net_debug_gsvr),    %% 网络包输出管理进程启动
    proc_checker_sup:start_child(net_checker),  %% 网络进程监控

    ok.

%% 启动数据库服务
start_db() ->
    db_mysql:start_mysql(),
    ok.

%% 在一些服务start后, 启动一些游戏逻辑相关的
after_start() ->
    ets_gsvr:hold_new(ets_uid_acc, [public, named_table, {keypos, 1}]),         %% uid对应账号
    ets_gsvr:hold_new(ets_nickname_uid, [public, named_table, {keypos, 1}]),    %% 角色名对应uid

    group_api:new(world_player),    %% 全局用, 直接new
    group_api:new(world_sender),    %% 全局用, 直接new

    proc_checker_sup:start_child(player_checker), %% 玩家进程监控
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