%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:24
%%%-------------------------------------------------------------------
-module(player_logic).
-author("Administrator").

-include("hrl_player.hrl").
-include("hrl_db.hrl").
-include("hrl_logs.hrl").
-include("hrl_common.hrl").
-include("login_pb.hrl").

%% API
-export([
    connect/4,
    loop/1,
    on_process_down/2
]).

%%% 客户端建立连接, player进程接到消息进程的connect消息
connect(_ConnType, SPid, UserMaster, #player{state = PState} = Player) ->
    Player1 =
        case PState of
            ?PLAYER_STATE_INIT ->   %% 需要获取其他数据初始化Player
                CPlayer1 = init_player(UserMaster, Player),
                do_connect(SPid, CPlayer1);
            _ ->
                %% TODO 如果玩家是在游戏中, 需要做重入游戏的逻辑
                do_connect(SPid, Player)
        end,
    Player1.

%%
init_player(UserMaster, #player{user_id = UserID} = Player) ->
    #user_master{
        account = Account,
        password = Password,
        nick_name = NickName,
        phone = Phone,
        icon = Icon,
        channel = Channel
    } = UserMaster,
    Player1 =
        Player#player{
            account = Account,
            password = Password,
            nick_name = NickName,
            phone = Phone,
            icon = Icon,
            channel = Channel
        },
    %% 从数据库中获取金币
    case db_mysql_api:select_user_money(UserID) of
        {ok, #user_money{game_coin = GCoin, bank_coin = BCoin}} ->
            Player1#player{game_coin = GCoin, bank_coin = BCoin};
        ?NULL_VAL ->    %% 初始化
            db_mysql_api:insert_user_money(#user_money{user_id = UserID}),
            Player1;
        _ ->    %% 有错误, 关闭进程
            ?WARNING("Init player err! Stop!"),
            self() ! stop,
            Player1
    end.

do_connect(SPid, Player) ->
    lib_send:set_spid(SPid),
    Player#player{
        state = ?PLAYER_STATE_NORMAL,
        spid = SPid
    }.


loop(Player) ->
    Player.

on_process_down(SPid, #player{spid = SPid} = Player) ->
    Player.
%%    case pl:is_online(Player) of
%%        ?true ->
%%            offline(Player);
%%        _ ->
%%            Player
%%    end;
%%on_process_down(_Pid, Player) ->
%%    ?TRAC(pid_down, _Pid),
%%    Player.
%%
%%log_connection(Type, Player) ->
%%    Data = [{ip, get_ip_binary(Player)}],
%%    xg_logs:log(connection, Type, Data, Player).
