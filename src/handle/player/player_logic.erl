%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:24
%%%-------------------------------------------------------------------
-module(player_logic).
-author("Administrator").

-include("record.hrl").
-include("hrl_common.hrl").

%% API
-export([
    login_by_session/1,
    loop/1,
    on_process_down/2
]).

login_by_session(Player) ->
    Player.

loop(Player) ->
    Player.

on_process_down(SPid, #player{spid = SPid} = Player) ->
    Player.
%%    case pl:is_online(Player) of
%%        ?true ->
%%            offline(Player);
%%        _ ->
%%            Player
%%    end;
%%on_process_down(_Pid, Player) ->
%%    ?TRAC(pid_down, _Pid),
%%    Player.
%%
%%log_connection(Type, Player) ->
%%    Data = [{ip, get_ip_binary(Player)}],
%%    xg_logs:log(connection, Type, Data, Player).
