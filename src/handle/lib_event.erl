%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 17:01
%%%-------------------------------------------------------------------
-module(lib_event).
-author("Administrator").

-include("record.hrl").
-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_event.hrl").

-export([
    trigger_call/3,
    trigger_event/3,
    trigger_event/2,
    trigger_after/4,
    cancel_trigger/1,
%%    off_event/3,
    tevent/3,
    tevent/4,
    tgen_cast/3,
    world_event/2,
    do_world_event/3,
    join_world/1
]).

-define(WORLD_SLOTS, 32).

%% 事件触发call
%% Call玩家进程, 必须非常小心, 使用此接口必须进行code review
trigger_call(UUID, EvtID, Content) when is_integer(UUID) ->
    ?true = (player =/= erlang:get(process_type)), % 不允许从player进程call
    gen_server:call(player_api:pid(UUID), {route_evt, EvtID, Content});
trigger_call(Pid, EvtID, Content) when is_pid(Pid) ->
    ?true = (player =/= erlang:get(process_type)), % 不允许从player进程call
    gen_server:call(Pid, {route_evt, EvtID, Content});
trigger_call({Name, Node} = Pid, EvtID, Content)
    when is_atom(Name) andalso is_atom(Node) ->
    ?true = (player =/= erlang:get(process_type)), % 不允许从player进程call
    gen_server:call(Pid, {route_evt, EvtID, Content}).


%%
trigger_event(EvtID, Content) ->
    case erlang:get(process_type) of
        player ->
            gen_server:cast(self(), {route_evt, EvtID, Content});
        _ ->
            ?NOTICE("trigger event need UUID ~p", [xg_util:get_call_stack()])
    end.


%% 事件触发cast
trigger_event(UUID, EvtID, Content) when is_integer(UUID) ->
    gen_server:cast(player_api:pid(UUID), {route_evt, EvtID, Content});
trigger_event(Pid, EvtID, Content) when is_pid(Pid) ->
    gen_server:cast(Pid, {route_evt, EvtID, Content});
trigger_event({Name, Node} = Pid, EvtID, Content)
    when is_atom(Name) andalso is_atom(Node) ->
    gen_server:cast(Pid, {route_evt, EvtID, Content});
trigger_event(Other, EvtID, Content) ->
    ?WARNING("Cannot event to ~p: ~p: ~p, stack: ~p", [Other, EvtID, Content, xg_util:get_call_stack()]),
    ok.


%% 延迟事件触发
trigger_after(Time, UUID, EvtID, Content) when is_integer(UUID) ->
    trigger_after(Time, player_api:pid(UUID), EvtID, Content);
trigger_after(0, Target, EvtID, Content) ->
    trigger_event(Target, EvtID, Content);
trigger_after(Time, {Name, Node}, EvtID, Content) when Node =:= node() ->
    erlang:send_after(Time, Name, {route_evt, EvtID, Content});
trigger_after(Time, Pid, EvtID, Content) ->
    case is_pid(Pid) andalso node(Pid) =:= node() of
        ?true -> % 本节点
            erlang:send_after(Time, Pid, {route_evt, EvtID, Content});
        _ -> % 跨节点
            spawn(fun() -> timer:sleep(Time),
                trigger_event(Pid, EvtID, Content) end)
    end.


%% 传入trigger_after的返回值
cancel_trigger(Ref) when is_reference(Ref) ->
    erlang:cancel_timer(Ref);
cancel_trigger(Pid) when is_pid(Pid) ->
    erlang:exit(Pid, normal);
cancel_trigger(_) ->
    ok.


%%%% 可离线事件
%%off_event(0, _EvtID, _Content) ->
%%    ok;
%%off_event(UUID, EvtID, Content) ->
%%    case player_api:pid(UUID) of
%%        ?undefined ->
%%            xg_offevent_svr:event(UUID, EvtID, Content);
%%        Pid when is_pid(Pid) ->
%%            trigger_event(Pid, EvtID, Content);
%%        _E ->
%%            ?WARNING("~p not in this node, EvtID=~p", [UUID, EvtID]),
%%            ok
%%    end.


%% 事务事件
tevent(EvtID, Content, #player{tevents = TEvents} = Player) ->
    Player#player{tevents = [{EvtID, Content} | TEvents]}.

tevent(UUID, EvtID, Content, #player{tevents = TEvents} = Player) ->
    Player#player{tevents = [{UUID, EvtID, Content} | TEvents]}.

tgen_cast(Dest, Request, Player) ->
    tevent(?EVT_PLAYER_TCAST, {Dest, Request}, Player).


%% 世界广播
world_event(EvtID, Content) ->
    do_world_event(world_player, EvtID, Content).

do_world_event(Group, EvtID, Content) ->
    Slots = lists:seq(1, ?WORLD_SLOTS),
    group_api:mcast({route_evt, EvtID, Content}, Group, Slots),
    ok.

join_world(UUID) ->
    Slot = UUID rem ?WORLD_SLOTS + 1,
    group_api:join(self(), world_player, Slot).
