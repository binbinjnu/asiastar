%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     玩家的一些宏定义
%%% @end
%%% Created : 17. 11月 2019 20:14
%%%-------------------------------------------------------------------
-author("Administrator").

%% 玩家状态
-define(PLAYER_STATE_INIT, 0).      %% 进程创建完成, 但没有连接上来
-define(PLAYER_STATE_NORMAL, 1).    %% 跟消息进程建立连接, 并初始化完数据了
-define(PLAYER_STATE_DISCONN, 2).   %% 已加载过数据的连接断开


%% 玩家数据
-record(player, {
    version         = 0,                % 数据版本, 必须为第一位
    %% 玩家数据库中有的数据
    user_id         = 0,                % 角色唯一ID

    game_coin       = 0,                % 身上金币
    bank_coin       = 0,                % 银行金币

    account         = <<"">>,           % 账号
    password        = <<"">>,           % 密码
    nick_name       = <<"">>,           % 昵称
    phone           = <<"">>,           % 手机号码
    icon            = <<"">>,           % 头像
    channel         = <<"">>,           % 渠道

    %% 玩家临时数据
    token           = undefined,        % 玩家token, 跟着进程一起存在, 方便客户端在进程存活期间直接重连
    state           = 0,                % 玩家状态, 对应宏定义 PLAYER_STATE_XXX
    game_state      = undefined,        % 游戏状态, 不在游戏中:undefined, 游戏中:{game_id, type_id, table_id, PID}
    loop_counter    = 0,                % 进程循环计数    单位: s
    spid            = undefined,



    tsends          = [],
    tevents         = [],
    logs            = []
}).