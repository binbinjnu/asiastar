%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 10月 2019 19:49
%%%-------------------------------------------------------------------
-module(haishen).
-author("Administrator").

%% API

-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.


handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p, Reason:~p",[Err, Reason]),
            {reply, error, State}
    end.

handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p, Reason:~p", [Err, Reason]),
            {noreply, State}
    end.

handle_info(Request, State) ->
    try
        do_info(Request, State)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p, Reason:~p, Req:~p",[Err, Reason, Request]),
            {noreply, State}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


do_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.

do_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.

do_info(_Msg, State) ->
    ?WARNING("unhandle info ~p", [_Msg]),
    {noreply, State}.


