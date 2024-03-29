%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     子游戏管理进程, 管理每个子游戏
%%%     有几类子游戏就开几个
%%% @end
%%% Created : 31. 10月 2019 18:43
%%%-------------------------------------------------------------------
-module(game_mgr_svr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([
    update_game_room/0,
    room_switch/2,
    enter_game/3
]).

-export([start/1, start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% 进程字典
%% {table, RoomType, TableID} :: #game_table{}  桌台信息
%% {table_id_counter, RoomType} :: TableID  桌台号自增id

%% 桌台信息
-record(game_table, {
    table_id = 0,
    p_num = 0,      %% 桌台玩家数
    pid = ?undefined
}).

%% 存放数据库中对应的信息
-record(game_room, {
    room_id = 0,            %% 房间id, 数据库中的key (game_id * 100 + room_type)
    room_type = 0,          %% 房间类型
    room_name = <<>>,       %% 房间名字
    enter_min_limit = 0,    %% 进入最低金币限制
    bet_max_limit = 0,      %% 最大下注限制
    p_num_limit = 0,        %% 每桌最大人数限制
    switch = ?SWITCH_ON     %% 开关,  默认: ?SWITCH_ON(开启)
}).

-record(state, {
    game_id = 0,
    l_room = []
}).

%%% ---------------------- API ----------------------
%% todo 更新房间信息
update_game_room() ->
    ok.

%% todo 开关房间
room_switch(_RoomID, _Switch) ->
    ok.


%% 进入子游戏
enter_game(GameID, RoomType, GameCoin) ->
    RegName = util_code:reg_name([?MODULE, "_", GameID]),
    gen_server:call(RegName, {enter_game, RoomType, GameCoin}).

%%% ---------------------- API ----------------------


start(GameID) ->
    game_mgr_sup:start_child(game_mgr_svr, [GameID]).

start_link(GameID) ->
    RegName = util_code:reg_name([?MODULE, "_", GameID]),
    gen_server:start_link({local, RegName}, ?MODULE, [GameID], []).


init([GameID]) ->
    %% 到数据库中找有对应GameID的数据
    ?INFO("game mgr gsvr init! GameID:~w", [GameID]),
    %% 通知父进程自己开启成功
    game_mgr_main_svr:sub_process_init(GameID, self()),
    self() ! init,
    {ok, #state{game_id = GameID}}.


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


do_call({enter_game, RoomType, GameCoin}, _From, State) ->
    Res = local_enter_game(RoomType, GameCoin, State),
    {reply, Res, State};

do_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.


do_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.


do_info(init, State) ->
    %% init不成功的话, 不会halt
    %% 暂时不需要起各种table进程, 动态开启
    %% 加载房间信息
    {noreply, State};

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
%% 进入游戏
%% Res :: {ok, Pid} | {false, Error}
local_enter_game(RoomType, GameCoin, #state{game_id = GameID, l_room = LRoom} = _State) ->
    case lists:keyfind(RoomType, #game_room.room_type, LRoom) of
        #game_room{} = GameRoom ->
            case check_enter(GameCoin, GameRoom) of
                ?true ->
                    _Res = select_table(GameID, GameRoom);
                Res ->
                    Res
            end;
        _ ->
            {?false, no_room}
    end.

%% 判断进入条件
check_enter(_GameCoin , #game_room{switch = off}) ->    %% 开关关闭
    {?false, switch_off};
check_enter(GameCoin, #game_room{enter_min_limit = EnterMinLimit})
    when GameCoin < EnterMinLimit ->    %% 携带金币不足
    {?false, enter_min_limit};
check_enter(_, _) ->
    ?true.

%% 选择房间, 如果没有合适的, 创建新的
select_table(GameID, GameRoom) ->
    #game_room{
        room_type = RoomType,
        p_num_limit = PNumLimit
    } = GameRoom,
    MaxTableID = util:erlang_get({table_id_counter, RoomType}, 0),
    case do_select_table(PNumLimit, RoomType, 1, MaxTableID) of
        {ok, TableID, Pid} ->
            case erlang:is_pid(Pid) andalso erlang:is_process_alive(Pid) of
                ?true ->
                    {ok, Pid};
                _ ->    %% 如果进程有问题, 重启一个新的
                    _Res = create_table(GameID, RoomType, TableID)
            end;
        _ ->    %% 找不到合适的房间, 新起一个
            TableID = MaxTableID + 1,
            case create_table(GameID, RoomType, TableID) of
                {ok, Pid} ->
                    erlang:put({table_id_counter, RoomType}, TableID),
                    {ok, Pid};
                Res ->  %% 启动进程有问题
                    Res
            end
    end.

%% 选择合适的房间
do_select_table(PNumLimit, RoomType, TableID, MaxTableID) when TableID =< MaxTableID ->
    case erlang:get({table, RoomType, TableID}) of
        #game_table{p_num = PNum, pid = Pid} when PNum < PNumLimit ->
            {ok, TableID, Pid};
        _ ->
            _Res = do_select_table(PNumLimit, RoomType, TableID + 1, MaxTableID)
    end;
do_select_table(_PNumLimit, _RoomType, _TableID, _MaxTableID) ->
    {?false, no_table}.


%% 开启桌台
create_table(GameID, RoomType, TableID) ->
    case game_svr:start(GameID, RoomType, TableID) of
        {ok, Pid} ->
            GameTable = #game_table{table_id = TableID, pid = Pid},
            erlang:put({table, RoomType, TableID}, GameTable),
            {ok, Pid};
        _Res ->
            ?WARNING("create table fail! Res:~w", [_Res]),
            {?false, create_table_err}
    end.



