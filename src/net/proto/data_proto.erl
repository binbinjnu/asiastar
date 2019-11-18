%%%-------------------------------------------------------------------
%%% proto文件导出, 根据协议号和协议名分别获取对应信息
%%%-------------------------------------------------------------------
-module(data_proto).
-include("hrl_logs.hrl").

%% API
-export([get/1]).
-export([get_c2s/1]).
-export([get_s2c/1]).


%% login.proto get
get(c2s_heartbeat) -> {10000, login_pb};
get(s2c_heartbeat) -> {10000, login_pb};
get(c2s_login) -> {10001, login_pb};
get(s2c_login) -> {10001, login_pb};

%% player.proto get
get(c2s_player_info) -> {10100, player_pb};
get(s2c_player_info) -> {10100, player_pb};

get(_ID) ->
    ?WARNING("Cannot get ~p, ~p", [_ID, util:get_call_from()]),
    undefined.


%% login.proto get_c2s
get_c2s(10000) -> {c2s_heartbeat, login_pb};
get_c2s(10001) -> {c2s_login, login_pb};

%% player.proto get_c2s
get_c2s(10100) -> {c2s_player_info, player_pb};

get_c2s(_ID) ->
    ?WARNING("Cannot get_c2s ~p, ~p", [_ID, util:get_call_from()]),
    undefined.


%% login.proto get_s2c
get_s2c(10000) -> {s2c_heartbeat, login_pb};
get_s2c(10001) -> {s2c_login, login_pb};

%% player.proto get_s2c
get_s2c(10100) -> {s2c_player_info, player_pb};

get_s2c(_ID) ->
    ?WARNING("Cannot get_s2c ~p, ~p", [_ID, util:get_call_from()]),
    undefined.
