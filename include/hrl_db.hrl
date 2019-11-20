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

-define(POOL_NAME, config:get_app()).   %% asiastar
-define(NULL_VAL, null_val).        %% 数据库返回空值

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