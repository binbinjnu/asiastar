%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 11月 2019 20:23
%%%-------------------------------------------------------------------
-author("Administrator").

-define(POOL_NAME, config:get_app()).
-define(NULL_VAL, null_val).        %% 数据库返回空值

-record(user_master, {
    user_id = 0,
    account_name = <<>>,
    account_passwd = <<>>,
    account_nickname = <<>>,
    account_mobile = <<>>,
    account_sourceid = 0,
    account_status = 0,
    channel_id = <<>>
}).