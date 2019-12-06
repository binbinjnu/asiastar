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
    shuffle2/2,

    batch/4,

    get_value/2,
    get_value/3,
    
    index_of/2,
    
    set_list_nth/3

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

%% 批量, 每个分段最大为Max, 对每个分段执行Func
batch(Func, Acc0, Data, Max) when is_list(Data)->
    batch(Func, Acc0, Data, Max, [], 0).

batch(Func, Acc0, Remain, Max, AccData, N) when N >= Max ->
    Acc1 = Func(AccData, Acc0),
    batch(Func, Acc1, Remain, Max, [], 0);
batch(Func, Acc0, []=_Remain, _Max, AccData, _) ->
    Func(AccData, Acc0);
batch(Func, Acc0, [H|T], Max, AccData, N) ->
    batch(Func, Acc0, T, Max, [H|AccData], N+1).

%% 兼容List和maps的参数提取, 约等于proplist:get_value
get_value(Key, Args) ->
    get_value(Key, Args, undefined).


get_value(Key, Maps, Default) when is_map(Maps) ->
    maps:get(Key, Maps, Default);

get_value(Key, List, Default) when is_list(List) ->
    case lists:keyfind(Key, 1, List) of
        {_, Val} -> Val;
        false -> Default
    end.


%% 查找Item在列表List中的位置，如果没有则返回not_found
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | T], Index) -> index_of(Item, T, Index + 1).


%% 替掉列表中的第N项为Item
set_list_nth(N, Item, [H|T]) when N > 1 ->
    [H|set_list_nth(N-1, Item, T)];
set_list_nth(_, Item, [_|T]) ->
    [Item|T].

