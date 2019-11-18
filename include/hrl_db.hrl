%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 11月 2019 20:23
%%%-------------------------------------------------------------------
-author("Administrator").

-define(POOL_NAME, config:get_app()).   %% asiastar
-define(NULL_VAL, null_val).        %% 数据库返回空值

%% 玩家主信息表
-record(user_master, {
    user_id = 0,
    account = <<>>,
    password = <<>>,
    nick_name = <<>>,
    phone = <<>>,
    icon = <<>>,
    channel = <<>>
}).

%% 玩家金钱表
-record(user_money, {
    user_id = 0,
    game_coin = 0,  %% 单位: 分
    bank_coin = 0   %% 单位: 分
}).