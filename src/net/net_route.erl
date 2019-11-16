%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     网络路由
%%% @end
%%% Created : 30. 10月 2019 20:24
%%%-------------------------------------------------------------------
-module(net_route).
-author("Administrator").

%% API
-export([
    route_msg/3,
    route_evt/3,
    msg_mod/1
]).

%% 客户端消息的路由
route_msg(MsgID, Data, State) ->
    Handler = msg_mod(MsgID),
    Handler:handle_msg(Data, State).

%% 服务端事件路由
route_evt(MsgID, Data, State) ->
    Handler = msg_mod(MsgID),
    Handler:handle_evt(MsgID, Data, State).

%% 消息路由
msg_mod(MsgID) ->
    case MsgID div 1000 of
        100 -> handle_login;
        101 -> handle_player;
        _ ->  handle_err
    end.