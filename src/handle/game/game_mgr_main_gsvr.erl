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
-include("hrl_db.hrl").
-include("hrl_logs.hrl").

-export([
    sub_process_init/2,
    get_all_game/0
]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([local_start_game_mgr/2]).

-record(game_mgr, {
    game_id = 0,
    game_name = <<>>,
    pid = ?undefined,
    switch = ?SWITCH_ON         %% 开关,  默认: ?SWITCH_ON(开启)
}).

-record(state, {
    l_game = []       %% 子游戏列表 [#game_mgr{} | _]
}).

%%% ---------------------- API ----------------------
%% 获取当前所有游戏信息
get_all_game() ->
    ok.

%% 子进程开启
sub_process_init(GameID, Pid) ->
    gen_server:cast(?MODULE, {sub_process_init, GameID, Pid}).

%%% ---------------------- API ----------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    %% 初始化的时候开启各个子进程
    self() ! init,
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


do_cast({sub_process_init, GameID, Pid}, #state{l_game = LGame} = State) ->
    %% monitor 子进程
    erlang:monitor(process, Pid),
    case lists:keytake(GameID, #game_mgr.game_id, LGame) of
        {value, #game_mgr{game_id = GameID} = GameMgr, LGame1} ->
            erlang:monitor(process, Pid),
            ?NOTICE("sub process init! GameID:~w", [GameID]),
            {noreply, State#state{l_game = [GameMgr#game_mgr{pid = Pid} | LGame1]}};
        _->
            {noreply, State}
    end;

do_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.

do_info(init, _State) ->
    try
        State = local_init(),
        {noreply, State}
    catch
        Err:Reason ->
            %% 开启不成功, 需要关闭程序
            ?ERROR("~w init fail, Err:~w, Reason:~w", [?MODULE, Err, Reason]),
            timer:sleep(200),
            erlang:halt(110)
    end;

%% 将对应子游戏的pid置位undefined
do_info({'DOWN', _Ref, process, Pid, Reason}, #state{l_game = LGame} = State) ->
    case lists:keytake(Pid, #game_mgr.pid, LGame) of
        {value, #game_mgr{game_id = GameID, switch = Switch} = GameMgr, LGame1} ->
            ?WARNING("~w down!  Reason:~w, switch:~w", [GameID, Reason, Switch]),
            {noreply, State#state{l_game = [GameMgr#game_mgr{pid = ?undefined} | LGame1]}};
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
local_init() ->
    State = #state{},
    {ok, LGameT} = db_mysql_api:select_game(),
    ?INFO("LGameT:~w", [LGameT]),
    LGame =
        [begin
             {ok, _Pid} = local_start_game_mgr(GameID, State),
             #game_mgr{game_id = GameID, game_name = GameName, switch = Switch}
         end || #game_t{game_id = GameID, game_name = GameName, switch = Switch} <- LGameT],
    ?INFO("LGame:~w", [LGame]),
    State#state{l_game = LGame}.

%% 开启game_mgr_gsvr
local_start_game_mgr(GameID, #state{l_game = LGame} = _State) ->
    case lists:keyfind(GameID, #game_mgr.game_id, LGame) of
        #game_mgr{pid = Pid} when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                ?true ->    %% 进程已存在
                    ?WARNING("start game manager fail! Process alive! GameID:~w", [GameID]),
                    {ok, Pid};
                _ ->
                    do_start_game_mgr(GameID)
            end;
        _ ->
            do_start_game_mgr(GameID)
    end.

do_start_game_mgr(GameID) ->
    case game_mgr_gsvr:start(GameID) of
        {ok, Pid} ->
            {ok, Pid};
        _R ->
            ?WARNING("start game manager fail! GameID:~w, Err:~w", [GameID, _R]),
            {?false, start_child_err}
    end.