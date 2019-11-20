%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:12
%%%-------------------------------------------------------------------
-module(handle_player).
-author("Administrator").

-include("hrl_logs.hrl").
-include("hrl_player.hrl").
-include("player_pb.hrl").

%% API
-export([handle_msg/2,
    handle_evt/3]).

%% 请求玩家信息
handle_msg(#c2s_player_info{}, Player) ->
    Resp =
        #s2c_player_info{
            sToken = Player#player.token,
            iUserID = Player#player.user_id,
            sNickName = Player#player.nick_name,
            iGameCoin = Player#player.game_coin,
            iBankCoin = Player#player.bank_coin,
            sPhone = Player#player.phone,
            sIcon = Player#player.icon
        },
    lib_send:send(Resp),
    {noreply, Player};

handle_msg(_Req, Player) ->
    ?WARNING("unhandle Req ~p", [_Req]),
    {noreply, Player}.


handle_evt(_ID, _, Player) ->
    ?WARNING("unhandle evt ~p", [_ID]),
    {noreply, Player}.
