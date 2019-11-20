%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 11月 2019 16:36
%%%-------------------------------------------------------------------
-module(player_token).
-author("Administrator").

-include("hrl_player.hrl").
-include("hrl_common.hrl").

-export([
    gen_token/1
]).

gen_token(Player) ->
    A = util_math:rand(1, 16#FFFFFFFF),
    B = util_math:rand(1, 16#FFFFFFFF),
    C = util_math:rand(1, 16#FFFFFFFF),
    D = util_math:rand(1, 16#FFFFFFFF),
    Token = <<A:32, B:32, C:32, D:32>>,
    Player#player{token = Token}.


