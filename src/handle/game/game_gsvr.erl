%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     子游戏进程
%%% @end
%%% Created : 31. 10月 2019 18:43
%%%-------------------------------------------------------------------
-module(game_gsvr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([start/3]).
-export([start_link/3, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(game_table, {
}).

%% 开启进程接口
start(GameID, RoomType, TableID) ->
    supervisor:start_child(game_sup, [GameID, RoomType, TableID]).

%% 监控进程调用的开启接口
start_link(GameID, RoomType, TableID) ->
    gen_server:start_link(?MODULE, [GameID, RoomType, TableID], []).

init([_GameID, _RoomType, _TableID]) ->
    {ok, []}.

handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        Err:Reason->
            ?ERROR("ERR:~p,Reason:~p",[Err,Reason]),
            {reply, error, State}
    end.


handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch
        Err:Reason->
            ?ERROR("ERR:~p,Reason:~p",[Err,Reason]),
            {noreply, State}
    end.

handle_info(Request, State) ->
    try
        do_info(Request, State)
    catch
        Err:Reason->
            ?ERROR("ERR:~p,Reason:~p",[Err,Reason]),
            {noreply, State}
    end.

do_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.

do_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.

do_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.