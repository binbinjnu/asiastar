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
    ok = main:pre_start(),
    {ok, Sup} = ?MAIN_SUP:start_link(),
    ok = main:start(),
    ok = main:after_start(),
    {ok, Sup}.

stop(_State) ->
    ok = main:pre_stop(),
    ok = main:stop(),
    ok.
