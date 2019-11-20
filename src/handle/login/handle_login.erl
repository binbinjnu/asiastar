%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:12
%%%-------------------------------------------------------------------
-module(handle_login).
-author("Administrator").

-include("hrl_logs.hrl").
-include("login_pb.hrl").

%% API
-export([handle_msg/2,
    handle_evt/3]).

handle_msg(#c2s_login{} = Req, State) ->
    State1 = login_logic:login(Req, State),
    {noreply, State1};

handle_msg(#c2s_re_login{iUserID = UserID, sToken = Token} = _Req, State) ->
    State1 = login_logic:re_login(UserID, Token, State),
    {noreply, State1};

handle_msg(_Req, State) ->
    ?WARNING("unhandle Req ~p", [_Req]),
    {noreply, State}.


handle_evt(_ID, _Msg, State) ->
    ?WARNING("unhandle evt ID:~p, Msg:~w", [_ID, _Msg]),
    {noreply, State}.
