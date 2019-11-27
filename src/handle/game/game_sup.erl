%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 15:49
%%%-------------------------------------------------------------------
-module(game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).
-export([all_games/0]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    erlang:process_flag(priority, high),
    {ok, { {simple_one_for_one, 0, 1},
        [{game_svr,
            {game_svr, start_link, []},
            temporary,
            10000,
            worker,
            [game_svr]}
        ]}}.

all_games() ->
    Children = supervisor:which_children(?MODULE),
    [Pid || {_, Pid, _, _} <- Children].
