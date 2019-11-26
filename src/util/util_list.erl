%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     list相关的
%%% @end
%%% Created : 25. 11月 2019 13:35
%%%-------------------------------------------------------------------
-module(util_list).
-author("Administrator").

%% API
-export([
    shuffle/1,
    shuffle2/2
]).


%% 列表乱序(洗牌)
shuffle(L) when is_list(L) ->
    [X || {_,X} <- lists:sort( [{rand:uniform(), N} || N <- L])].

%% 洗到不那么乱的序（更高效率）
%% N 隔几个洗一下
shuffle2(L, N) ->
    do_shuffle2(L, N, N, 0.5, []).

do_shuffle2([], _, _, _, RetList) ->
    [X || {_, X} <- lists:keysort(1, RetList)];
do_shuffle2([H | Rem], Interval, Index, LastRand, RetList) ->
    if
        Index rem Interval =:= 0 ->
            Rand = rand:uniform(),
            do_shuffle2(Rem, Interval, Index + 1, Rand, [{Rand, H} | RetList]);
        true ->
            Rand = LastRand / Interval,
            do_shuffle2(Rem, Interval, Index + 1, LastRand, [{Rand, H} | RetList])
    end.