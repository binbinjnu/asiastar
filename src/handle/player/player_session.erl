%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:36
%%%-------------------------------------------------------------------
-module(player_session).
-author("Administrator").

-include("record.hrl").
-include("hrl_common.hrl").

-export([
    gen_token/1,
%%    check_from_gate/2,
    login/3,
    delete/1
]).


gen_token(Player) ->
    A = util:rand(1, 16#FFFFFFFF),
    B = util:rand(1, 16#FFFFFFFF),
    C = util:rand(1, 16#FFFFFFFF),
    D = util:rand(1, 16#FFFFFFFF),
    Token = <<A:32, B:32, C:32, D:32>>,
    erlang:put(session_token, Token),
%%    Player1 = lib_send:tsend(Player, #s2c_acc_session{token=Token}),
    Player.


%%check_from_gate(Token, UUID) ->
%%    From = self(), % gate
%%    Name = player_api:pid_name(UUID),
%%    event:trigger_event({Name, Node}, ?EVT_PLAYER_PUT_PEERNAME, lib_net:get_peername()),
%%    event:trigger_event({Name, Node}, ?EVT_PLAYER_LOGIN_BY_SESSION, {Token, From}),
%%    receive
%%        {token_check_ok, Pid, Account} ->
%%            {ok, Pid, Account};
%%        token_check_fail ->
%%            ?false
%%    after
%%        3000 ->
%%            ?false
%%    end.


login(Token, From, #player{} = Player) ->
    case erlang:get(session_token) of
        Token ->
            Player1 = Player#player{spid = From},
            Player2 = player_logic:login_by_session(Player1),
            From ! {token_check_ok, self()},
            Player2;
        _ ->
            From ! token_check_fail,
            Player
    end.

delete(_Player) ->
    erlang:erase(session_token).
