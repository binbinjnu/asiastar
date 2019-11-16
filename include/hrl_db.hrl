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

-record(user_master, {
    user_id,
    account_name,
    account_passwd,
    account_nickname,
    account_mobile,
    account_sourceid,
    account_status,
    channel_id
}).