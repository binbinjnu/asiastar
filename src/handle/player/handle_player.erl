%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:12
%%%-------------------------------------------------------------------
-module(handle_player).
-author("Administrator").

-include("hrl_logs.hrl").

%% API
-export([handle_msg/3,
    handle_evt/3]).


handle_msg(_ID, _Msg, State) ->
    ?WARNING("unhandle msg ~p, ~p", [_ID, _Msg]),
    {noreply, State}.


handle_evt(_ID, _, State) ->
    ?WARNING("unhandle evt ~p", [_ID]),
    {noreply, State}.
