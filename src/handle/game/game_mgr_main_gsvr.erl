%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     子游戏总管理进程, 管理每个子游戏管理进程
%%%     一个服开一个
%%% @end
%%% Created : 31. 10月 2019 18:43
%%%-------------------------------------------------------------------
-module(game_mgr_main_gsvr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_game_mgr/2]).

-record(game_mgr, {
    game_id = 0,
    pid = ?undefined,
    switch = on         %% 开关(on/off) 默认: on(开启)
}).

-record(state, {
    l_game = []       %% 子游戏列表 [#game_mgr{} | _]
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #state{}}.

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

do_info({'DOWN', _Ref, process, Pid, Reason}, #state{l_game = LGame} = State) ->
    case lists:keytake(Pid, #game_mgr.pid, LGame) of
        {value, #game_mgr{game_id = GameID, switch = Switch}, LGame1} ->
            ?WARNING("~w down!  Reason:~w, switch:~w", [GameID, Reason, Switch]),
            {noreply, State#state{l_game = LGame1}};
        _->
            {noreply, State}
    end;

do_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%% --------------------------------------------------------
%%%                     Local Function
%%% --------------------------------------------------------
start_game_mgr(GameID, #state{l_game = LGame} = State) ->
    case lists:keyfind(GameID, #game_mgr.game_id, LGame) of
        #game_mgr{pid = Pid} when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                ?true ->
                    ?WARNING("start game manager fail! Process alive! GameID:~w", [GameID]),
                    State;
                _ ->
                    do_start_game_mgr(GameID, State)
            end;
        _ ->
            do_start_game_mgr(GameID, State)
    end.

do_start_game_mgr(GameID, #state{l_game = LGame} = State) ->
    case game_mgr_sup:start_child(game_mgr_gsvr, [GameID]) of
        {ok, Pid} ->
            erlang:monitor(process, Pid),
            GameInfo = #game_mgr{game_id = GameID, pid = Pid},
            LGame1 = lists:keystore(GameID, #game_mgr.game_id, LGame, GameInfo),
            State#state{l_game = LGame1};
        _R ->
            ?WARNING("start game manager fail! GameID:~w, Err:~w", [GameID, _R]),
            State
    end.