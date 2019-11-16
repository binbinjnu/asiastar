%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 11月 2019 18:43
%%%-------------------------------------------------------------------
-module(net_checker).
-author("Administrator").

%% API
-export([reg/0]).
-export([start_link/0]).

reg() ->
    Pid = self(),
    proc_checker_gsvr:reg(?MODULE, Pid).

start_link() ->
    Args = #{
        slot_num => 60,
        interval => 1000,
        kill_msg_q => 200,      %% 最大消息队列长度
        check_msg_q => 0,
        check_timeout => 10000  %% 网络进程检查超时, ms
    },
    proc_checker_gsvr:start_link(?MODULE, Args).

