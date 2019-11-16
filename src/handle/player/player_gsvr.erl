%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 15:50
%%%-------------------------------------------------------------------
-module(player_gsvr).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("record.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([flush/1]).

-define(FLUSH_LIMIT, 5).
-define(MAX_MSG_LENGTH, 200). % 最大消息队列长度
-define(MAX_HEAP_SIZE, 10 * 1024 * 1024). % 最大堆内存大小(word)

%% ====================================================================
%% External functions
%% ====================================================================

start(UID) ->
    supervisor:start_child(player_sup, [UID]).

start_link(UID) ->
    gen_server:start_link(?MODULE, [UID], []).


init([UID]) ->
    erlang:process_flag(trap_exit, ?true),
    erlang:process_flag(max_heap_size, ?MAX_HEAP_SIZE),
    player_checker:reg(),
    gen_server:cast(self(), init),
    {ok, [UID]}.


handle_call(Request, From, Player) ->
    try
        do_call(Request, From, Player)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p,Reason:~p", [Err, Reason]),
            {reply, error, Player}
    end.

handle_cast(init, [UID]) ->
    try
        ?true = erlang:register(player_api:pid_name(UID), self()),
        %% todo load data
        Player = #player{},
        erlang:put(process_type, player),
        erlang:put(player_uid, UID),
%%        Player1 = xg_player:after_load(Player),
        {noreply, Player}
    catch
        Err:Reason ->
            ?ERROR("ERR:~p,Reason:~p", [Err, Reason]),
            {stop, Reason, []}
    end;
handle_cast(Request, Player) ->
    try
        do_cast(Request, Player)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p,Reason:~p", [Err, Reason]),
            {noreply, Player}
    end.

handle_info(Request, Player) ->
    try
        do_info(Request, Player)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p,Reason:~p", [Err, Reason]),
            {noreply, Player}
    end.

code_change(_OldVsn, Player, _Extra) ->
    {ok, Player}.

terminate(_Reason, []) ->
    ok;
terminate(_Reason, #player{spid = SPid} = _Player) ->
%%    xg_player:logout(Player),
    net_api:stop(SPid),
    ok.

%%do_call({gm_query, Action}, _From, Player) ->
%%    Result = xg_man:do_query(Action, Player),
%%    {reply, Result, Player};

do_call({route_evt, MsgID, Data}, _From, Player) ->
    case net_route:route_evt(MsgID, Data, Player) of
        {reply, Reply, #player{} = Player1} ->
            Player2 = after_route(Player1),
            {reply, Reply, Player2};
        _E ->
            ?WARNING("Invalid return, EvtID: ~p, return ~1000p", [MsgID, _E]),
            {reply, error, Player}
    end;

do_call(_Msg, _From, Player) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, Player}.

do_cast(kick, Player) ->
    {stop, normal, Player};

do_cast({route_msg, MsgID, Data}, Player) ->
    case net_route:route_msg(MsgID, Data, Player) of
        {noreply, #player{} = Player1} ->
            Player2 = after_route(Player1),
            {noreply, Player2};
        _E ->
            ?WARNING("Invalid return, MsgID: ~p, return ~1000p", [MsgID, _E]),
            {noreply, Player}
    end;

do_cast({route_evt, MsgID, Data}, Player) ->
    case net_route:route_evt(MsgID, Data, Player) of
        {noreply, #player{} = Player1} ->
            Player2 = after_route(Player1),
            {noreply, Player2};
        _E ->
            ?WARNING("Invalid return, EvtID: ~p, return ~1000p", [MsgID, _E]),
            {noreply, Player}
    end;

do_cast(_Msg, Player) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, Player}.

do_info({'EXIT', Pid, _Reason}, Player) ->
    Player1 = #player{} = player_logic:on_process_down(Pid, Player),
    {noreply, Player1};

do_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, Player) ->
    Player1 = #player{} = player_logic:on_process_down(Pid, Player),
    {noreply, Player1};

do_info(doloop, Player) ->
    check_busy(Player),
    Player1 = player_logic:loop(Player),
    Player2 = after_route(Player1),
    {noreply, Player2};

do_info({route_evt, MsgID, Data}, Player) ->
    case net_route:route_evt(MsgID, Data, Player) of
        {noreply, #player{} = Player1} ->
            Player2 = after_route(Player1),
            {noreply, Player2};
        _E ->
            ?WARNING("Invalid return, EvtID: ~p, return ~1000p", [MsgID, _E]),
            {noreply, Player}
    end;

do_info(_Msg, Player) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, Player}.


check_busy(#player{spid = SPid} = Player) ->
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len >= ?MAX_MSG_LENGTH ->
            ?WARNING("Message queue overflow ~p, kicked", [Len]),
            net_api:stop(SPid),
            player_session:delete(Player),
            discard_msg();
        _ ->
            ok
    end.

discard_msg() ->
    receive
        {'$gen_cast', {route_msg, _MsgID, _Data}} ->
            discard_msg();
        doloop ->
            discard_msg()
    after
        0 -> ok
    end.


%% route之后, tevents和tsends处理
after_route(#player{tevents = TEvents} = Player) ->
    Player1 = Player#player{tevents = []},
    #player{tsends = TSends} = Player2 = event(lists:reverse(TEvents), Player1),
    send(lists:reverse(TSends), Player2),
    Player2#player{tsends = []}.

send([], _Player) ->
    ok;
send(Sends, Player) ->
    lib_send:send(Player, Sends).

event([{EvtID, Cont} | T], #player{} = Player) ->
    {noreply, Player1} = handle_cast({route_evt, EvtID, Cont}, Player),
    event(T, Player1);
event([{UID, EvtID, Cont} | T], #player{uid = UID} = Player) ->
    {noreply, Player1} = handle_cast({route_evt, EvtID, Cont}, Player),
    event(T, Player1);
event([{UUID, EvtID, Cont} | T], #player{} = Player) ->
    event:trigger_event(UUID, EvtID, Cont),
    event(T, Player);
event([], Player) ->
    Player.

flush(Player) ->
    flush(Player, ?FLUSH_LIMIT).

flush(Player, N) when N > 0 ->
    case after_route(Player) of
        #player{tsends = [], tevents = []} = Player1 ->
            Player1;
        Player1 ->
            flush(Player1, N - 1)
    end;
flush(Player, _) ->
    Player.
