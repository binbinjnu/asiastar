%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 19:11
%%%-------------------------------------------------------------------
-module(group_sup).
-behaviour(supervisor).
-author("Administrator").

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_child(Args) ->
    Child = #{id => {group, Args},
        start => {group_gsvr, start_link, Args},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [group_gsvr]},
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
    ChildSpec = #{id => group_manager_gsvr,
                start => {group_manager_gsvr, start_link, []},
                restart => permanent,
                shutdown => 30000,
                type => worker,
                modules => [group_manager_gsvr]},
    {ok, {SupFlags, [ChildSpec]}}.

