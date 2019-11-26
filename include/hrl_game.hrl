%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     玩家的一些宏定义
%%% @end
%%% Created : 17. 11月 2019 20:14
%%%-------------------------------------------------------------------
-author("Administrator").

%% 桌台状态

%% 游戏状态

%% 游戏桌台信息
%% 回调函数
%% on_enter
%% on_exit
%% on_
-record(game_table, {
    game_id = 0,
    room_type = 0,
    table_id = 0,
    loop_counter = 0,       %% 进程循环计数, 单位: s

    handler = undefined,    %% 回调模块
    call_back = #{}         %% 回调函数

}).