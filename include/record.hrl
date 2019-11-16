%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 15:58
%%%-------------------------------------------------------------------
-author("Administrator").

%% net_handler 的state record
-record(handler_state, {
    state     = undefined   :: term(),              %% 状态
    pid       = undefined   :: pid() | undefined,   %%
    socket    = undefined   :: port() | undefined,  %%
    peername  = undefined,
    acc       = undefined,
    init_time = 0,                                  %% 初始化时间
    debug_pid = 0
}).

%% 玩家数据
-record(player, {
    version         = 0,                % 数据版本, 必须为第一位
    uid             = 0,                 % 角色唯一ID
    state           = 0,                 % 使用xg_player:set_state设置
    online_loop     = 0,                 % 玩家累计在线秒数计数
    spid            = undefined,
    name            = <<"">>,

    tsends          = [],
    tevents         = [],
    logs            = []
}).