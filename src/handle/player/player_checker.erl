%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 15:51
%%%-------------------------------------------------------------------
-module(player_checker).

-export([reg/0]).
-export([start_link/0]).

-define(SLOTS_NUM, 60).

reg() ->
    Pid = self(),
    proc_checker_svr:reg(?MODULE, Pid).

start_link() ->
    Args = #{
        slot_num => 60,
        interval => 900,
        kill_msg_q => 2000,
        check_msg_q => 9999 % 设置成很长, 比kill_msg_q长, 不会做call检查
    },
    proc_checker_svr:start_link(?MODULE, Args).

