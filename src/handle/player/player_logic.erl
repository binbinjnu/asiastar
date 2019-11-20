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
    gen_token/0,
    connect/4,
    re_login/3,
    loop/1,
    on_disconnection/2
]).

gen_token() ->
    A = util_math:rand(1, 16#FFFFFFFF),
    B = util_math:rand(1, 16#FFFFFFFF),
    C = util_math:rand(1, 16#FFFFFFFF),
    D = util_math:rand(1, 16#FFFFFFFF),
    <<A:32, B:32, C:32, D:32>>.

%%% 客户端建立连接, player进程接到消息进程的connect消息
connect(_ConnType, SPid, UserMaster, Player) ->
    #player{state = PState} = Player,
    Player1 =
        case PState of
            ?PLAYER_STATE_INIT ->   %% 需要获取其他数据初始化Player
                ?INFO("~w init player!!!", [Player#player.user_id]),
                CPlayer1 = init_player(UserMaster, Player),
                do_connect(SPid, CPlayer1);
            _ ->
                %% 如果PState为PLAYER_STATE_NORMAL为顶号
                %% 需要对顶号和断线重连进行不同处理
                %% TODO 如果玩家是在游戏中, 需要做重入游戏的逻辑
                case PState of
                    ?PLAYER_STATE_NORMAL ->
                        ?INFO("~w kick! OldSPid:~w, NewSPid:~w",
                            [Player#player.user_id, Player#player.spid, SPid]);
                    _->
                        ?INFO("~w reconn!", [Player#player.user_id])
                end,

                do_connect(SPid, Player)
        end,
    Player1.

%%
init_player(UserMaster, #player{user_id = UserID} = Player) ->
    #user_master_t{
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
        {ok, #user_money_t{game_coin = GCoin, bank_coin = BCoin}} ->
            Player1#player{game_coin = GCoin, bank_coin = BCoin};
        ?NULL_VAL ->    %% 初始化
            db_mysql_api:insert_user_money(#user_money_t{user_id = UserID}),
            Player1;
        _ ->    %% 有错误, 关闭进程
            ?WARNING("Init player err! Stop!"),
            self() ! stop,
            Player1
    end.

do_connect(SPid, #player{spid = OldSPid} = Player) ->
    net_api:stop(OldSPid),
    lib_send:set_spid(SPid),
    Player#player{
        state = ?PLAYER_STATE_NORMAL,
        spid = SPid
    }.


%%% token 重连
re_login(SPid, Token, #player{state = ?PLAYER_STATE_DISCONN, token = Token} = Player) ->
    Player1 = do_connect(SPid, Player),
    Player1;
re_login(SPid, _Token, Player) ->
    ?WARNING("~w re login fail, PState:~w, Token:~w, PToken:~w",
        [Player#player.user_id, Player#player.state, _Token, Player#player.token]),
    net_api:stop(SPid),
    Player.


%%% 玩家循环
loop(Player) ->
    Player.


%% 消息进程挂掉了
on_disconnection(SPid, #player{state = ?PLAYER_STATE_NORMAL, spid = SPid} = Player) ->
    %% todo 处理断线相关信息
    %% 1. 断线日志
    %% 2. 设置重连时间和token有效期
    %% 3. 发送重连时间过期后真实下线处理消息
    %% ... ...
    Player#player{state = ?PLAYER_STATE_DISCONN, spid = ?undefined};
on_disconnection(DownPid, #player{state = State, spid = SPid} = Player) ->
    ?WARNING("on process down not match! DownPid:~w, PlayerState:~w, SPid:~w",
        [DownPid, State, SPid]),
    Player.


%%%
