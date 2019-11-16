%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:36
%%%-------------------------------------------------------------------
-module(lib_send).
-author("Administrator").

-include("record.hrl").
-include("hrl_common.hrl").
-include("hrl_logs.hrl").

%% API
-export([
    mark/2,
    set_spid/1,

    send/1,
    send/2
]).

mark(UID, SPid) ->
    Mark = player_api:spid_name(UID),
    catch unregister(Mark),
    ?true = register(Mark, SPid).

set_spid(Pid) when is_pid(Pid)->
    erlang:put(spid, Pid).

%% 直接发消息
send(Msg) ->
    case erlang:get(spid) of
        SPid when is_pid(SPid) ->
            net_api:pack_send(SPid, Msg);
        _ ->
            ?WARNING("Cannot send. Call from:~n", util:get_call_stack())
    end.

send(_, []) ->
    ok;
send(#player{spid=SPid}, Msg) ->
    net_api:pack_send(SPid, Msg);
send(#handler_state{}, Msg) ->
    net_api:pack_send(self(), Msg);
send({_Name, _Node} = SPid, Msg) ->
    net_api:pack_send(SPid, Msg);
send(SPid, Msg) when is_pid(SPid) ->
    net_api:pack_send(SPid, Msg);
send(UID, Msg) when is_integer(UID)->
    net_api:pack_send(player_api:spid(UID), Msg);
send(?undefined, _) ->
    ok.