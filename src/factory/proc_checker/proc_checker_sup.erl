%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 19:11
%%%-------------------------------------------------------------------
-module(proc_checker_sup).
-behaviour(supervisor).
-author("Administrator").

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_child(Mod) ->
    Child = #{id => Mod,
        start => {Mod, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [Mod]},
    supervisor:start_child(?MODULE, Child).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one,
        intensity => 10,
        period => 10},
    {ok, {SupFlags, []}}.

