%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     数学计算的一些方法
%%% @end
%%% Created : 30. 10月 2019 9:39
%%%-------------------------------------------------------------------
-module(util_math).
-author("Administrator").

%% API
-export([
    rand/2,
    ceil/1,
    floor/1
]).

%% 在两个整数间随机
rand(Same, Same) ->
    Same;
rand(Min, Max) ->
    M = Min - 1,
    rand:uniform(Max - M) + M.

%% 向上取整 大于N的最小整数
ceil(N) ->
    case trunc(N) of
        M when M == N ->
            M;
        M when N > 0 ->
            M + 1;
        M -> M
    end.

%% 向下取整 小于X的最大整数
floor(X) ->
    case trunc(X) of
        T when X == T ->
            T;
        T when X > 0 ->
            T;
        T -> T - 1
    end.
