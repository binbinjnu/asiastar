%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 10月 2019 9:24
%%%-------------------------------------------------------------------
-module(net_api).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_net.hrl").

%% API
-export([
    start_listener/3,
    stop_listener/1
]).


-export([
    send/2,
    pack_send/2,
    send_after/3,
    pack_send_after/3,
    send_flush/2,
    event/2,
    stop/1
]).


%% Opts => #{handler => net_handler, socket_type => tcp | websocket}
start_listener(Ref, Port, Opts) ->
    case ranch:start_listener(Ref, ?NUM_ACCEPTORS, ranch_tcp, [{port, Port}], net_protocol, Opts) of
        {ok, Pid} ->
            ranch:set_max_connections(Ref, infinity),
            ?NOTICE("~p listen on tcp ~p", [Ref, Port]),
            {ok, Pid};
        Err ->
            Err
    end.

%%-spec stop_listener(Ref) -> ok | {error, not_found} when
%%    Ref :: atom().
stop_listener(Ref) ->
    ranch:stop_listener(Ref).


-spec send(Pid, Data) -> ok when
    Pid :: pid() | atom() | {atom(), atom()} | undefined,
    Data :: iolist().
send(?undefined, _) ->
    ok;
send(Pid, Data) ->
    erlang:send(Pid, {send, Data}, [noconnect]),
    ok.


-spec pack_send(Pid, Data) -> ok when
    Pid :: pid() | atom() | {atom(), atom()} | undefined,
    Data :: term().
pack_send(?undefined, _) ->
    ok;
pack_send(Pid, Data) ->
    erlang:send(Pid, {pack_send, Data}, [noconnect]),
    ok.


-spec send_flush(Pid, Data) -> ok when
    Pid :: pid() | atom() | {atom(), atom()} | undefined,
    Data :: iolist().
send_flush(?undefined, _) ->
    ok;
send_flush(Pid, Data) ->
    erlang:send(Pid, {send_flush, Data}, [noconnect]),
    ok.


-spec send_after(Time, Pid, Data) -> ok when
    Time :: integer(),
    Pid :: pid() | atom() | {atom(), atom()} | undefined,
    Data :: iolist().
send_after(_Time, ?undefined, _) ->
    ok;
send_after(0, Pid, Data) ->
    erlang:send(Pid, {send, Data}, [noconnect]),
    ok;
send_after(Time, {Name, Node}=Target, Data) ->
    case Node =:= node() of
        ?true ->
            erlang:send_after(Time, Name, {send, Data});
        _ ->
            erlang:send(Target, {later, Time, {send, Data}}, [noconnect])
    end,
    ok;
send_after(Time, Pid, Data) ->
    case node(Pid) =:= node() of
        ?true ->
            erlang:send_after(Time, Pid, {send, Data});
        _ ->
            erlang:send(Pid, {later, Time, {send, Data}}, [noconnect])
    end,
    ok.


-spec pack_send_after(Time, Pid, Data) -> ok when
    Time :: integer(),
    Pid :: pid() | atom() | {atom(), atom()} | undefined,
    Data :: term().
pack_send_after(_Time, ?undefined, _) ->
    ok;
pack_send_after(0, Pid, Data) ->
    erlang:send(Pid, {pack_send, Data}),
    ok;
pack_send_after(Time, {Name, Node}=Target, Data) ->
    case Node =:= node() of
        ?true ->
            erlang:send_after(Time, Name, {pack_send, Data});
        _ ->
            erlang:send(Target, {later, Time, {pack_send, Data}}, [noconnect])
    end,
    ok;
pack_send_after(Time, Pid, Data) ->
    case node(Pid) =:= node() of
        ?true ->
            erlang:send_after(Time, Pid, {pack_send, Data});
        _ ->
            erlang:send(Pid, {later, Time, {pack_send, Data}}, [noconnect])
    end,
    ok.


event(?undefined, _) ->
    ok;
event(Pid, Data) ->
    erlang:send(Pid, {event, Data}, [noconnect]),
    ok.


-spec stop(Pid) -> ok when
    Pid :: pid().
stop(?undefined) ->
    ok;
stop(Pid) ->
    erlang:send(Pid, stop, [noconnect]),
    ok.
