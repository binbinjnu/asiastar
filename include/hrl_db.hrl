%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     数据库相关头文件, 数据库字段名不要用mysql预留字段!!!!! 如果不好取名, 可加前缀或后缀
%%%     尽量避免 key, id 等无意义命名
%%% @end
%%% Created : 15. 11月 2019 20:23
%%%-------------------------------------------------------------------
-author("Administrator").

-define(POOL_NAME, pool).       %%
-define(NULL_VAL, null_val).        %% 数据库返回空值

%% kv data
-record(kv_data, {
    id,     %% id
    data    %% 二进制数据 mediumblob, 16M
}).

%%% 普通表名接后缀 _t
%% index表
-record(index_t, {
    index_k = <<>>,     % 主键
    index_v = 0         % 值
}).

%% 玩家主信息表
-record(user_master_t, {
    user_id = 0,
    account = <<>>,
    password = <<>>,
    nick_name = <<>>,
    phone = <<>>,
    icon = <<>>,
    channel = <<>>
}).

%% 玩家金钱表
-record(user_money_t, {
    user_id = 0,
    game_coin = 0,  %% 单位: 分
    bank_coin = 0   %% 单位: 分
}).

%% 子游戏
-record(game_t, {
    game_id = 0,        %% 游戏id
    switch = 0,         %% 开关
    game_name = <<>>    %% 游戏名
}).

%% 子游戏房间
-record(game_room_t, {
    room_id = 0,
    game_id = 0,
    room_name = <<>>,
    room_type = 0,
    switch = 0,
    max_player_num = 0,     %% 同桌最大玩家数
    min_enter_coin = 0,     %% 进入房间金钱下限（大于多少金钱才能进入）单位: 分
    max_enter_coin = 0,     %% 进入房间金钱上限（小于多少金钱才能进入）单位: 分
    base_coin = 0,          %% 底注 单位: 分
    ratio_revenue = 0.0,    %% 抽水比例
    create_time             %% 创建时间 {{Y, M, D}, {H, M, S}}
}).

