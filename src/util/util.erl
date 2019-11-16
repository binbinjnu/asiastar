%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 10月 2019 9:39
%%%-------------------------------------------------------------------
-module(util).
-author("Administrator").

%% API
-export([
    rand/2,
    term_to_string/1,
    term_to_bitstring/1,
    get_call_from/0,
    erlang_get/2,

    process_name/0,
    process_name/1,

    filter_opts/2,
    deny_opts/2,

    ceil/1,
    floor/1
]).

rand(Same, Same) ->
    Same;
rand(Min, Max) ->
    M = Min - 1,
    rand:uniform(Max - M) + M.

get_call_from() ->
    lists:sublist(get_call_stack(), 3, 1).

get_call_stack() ->
    try
        throw(get_call_stack)
    catch
        get_call_stack ->
            Trace1 =
                case erlang:get_stacktrace() of
                    [_|Trace] -> Trace;
                    Trace -> Trace
                end,
            empty_stacktrace(),
            [stack_format(S) || S <- Trace1]
    end.

empty_stacktrace() ->
    try
        erlang:raise(throw, clear, [])
    catch
        _ ->
            ok
    end.

stack_format({M, F, A, Info}) when is_list(A) ->
    A1 = lists:sublist(term_to_string(A), 40),
    case lists:keyfind(line, 1, Info) of
        {_, Line} ->
            {M, F, Line, A1};
        _ ->
            {M, F, A1}
    end;
stack_format({M, F, _A, Info}) ->
    case lists:keyfind(line, 1, Info) of
        {_, Line} ->
            {M, F, Line};
        _ ->
            {M, F}
    end.


term_to_string(Term) ->
    lists:flatten(io_lib:format("~9999999p", [Term])).


term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~999999p", [Term])).

%% erlang:get带默认值
erlang_get(Key, Default) ->
    case erlang:get(Key) of
        undefined -> Default;
        Res -> Res
    end.

%% 获取process_name
process_name() ->
    process_name(self()).

process_name(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        _ -> Pid
    end.

%% 过滤opts
filter_opts(Opts, Allows) ->
    F = fun ({O, _}) -> lists:member(O, Allows);
        (O) -> lists:member(O, Allows)
        end,
    [Op || Op <- Opts, F(Op)].

%% 确保没有不允许的参数
deny_opts(Opts, Denies) ->
    F = fun ({O, _}) -> lists:member(O, Denies);
        (O) -> lists:member(O, Denies)
        end,
    case [Op || Op <- Opts, F(Op)] of
        [] -> ok;
        List -> erlang:error({invalid_opts, List})
    end.

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