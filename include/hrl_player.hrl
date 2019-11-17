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
    uid             = 0,                 % 角色唯一ID
    state           = 0,                 %
    online_loop     = 0,                 % 玩家累计在线秒数计数
    spid            = undefined,
    name            = <<"">>,

    tsends          = [],
    tevents         = [],
    logs            = []
}).