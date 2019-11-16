%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 10月 2019 18:49
%%%-------------------------------------------------------------------
-module(logs_api).
-author("Administrator").

%% API

-export([
    info_msg/4,
    notice_msg/4,
    warning_msg/4,
    error_msg/5,

    debug_msg/6,
    info_msg/6,
    notice_msg/6,
    warning_msg/6,
    error_msg/7,

    force_notice_msg/4,
    force_warning_msg/4,
    force_error_msg/5,
    force_error_msg_nostack/4
]).

-define(NO_FUNC, '?').
-define(NO_ARITY, '?').

-define(LOG_NOTICE_LIMIT, 10).
-define(LOG_WARNING_LIMIT, 10).
-define(LOG_ERROR_LIMIT, 10).

-compile([{parse_transform, lager_transform}]).

debug_msg(Module, Line, Func, Arity, Format, Args) ->
    lager:debug("{~p} ~p:~p/~p[~w] "++Format++"~n",
        [util:process_name(), Module, Func, Arity, Line] ++ Args).

info_msg(Module, Line, Format, Args) ->
    info_msg(Module, Line, ?NO_FUNC, ?NO_ARITY, Format, Args).

info_msg(Module, Line, Func, Arity, Format, Args) ->
    lager:info("{~p} ~p:~p/~p[~w] "++Format++"~n",
        [util:process_name(), Module, Func, Arity, Line]++Args).

notice_msg(Module, Line, Format, Args) ->
    notice_msg(Module, Line, ?NO_FUNC, ?NO_ARITY, Format, Args).

notice_msg(Module, Line, Func, Arity, Format, Args) ->
    case log_count_up(Module, Line) of
        Count when Count < ?LOG_NOTICE_LIMIT ->
            lager:notice("{~p} ~p:~p/~p[~w] "++Format++"~n",
                [util:process_name(), Module, Func, Arity, Line]++Args);
        ?LOG_NOTICE_LIMIT ->
            lager:notice("{~p} ~p:~p: (Silenced!) "++Format++"~n",
                [util:process_name(), Module, Line]++Args);
        Count when Count < 1000 andalso Count rem 100 =:= 0 ->
            lager:notice("{~p} ~p:~p: (~p Times!) "++Format++"~n",
                [util:process_name(), Module, Line, Count]++Args);
        Count when Count rem 1000 =:= 0 ->
            lager:notice("{~p} ~p:~p: (~p Times!) "++Format++"~n",
                [util:process_name(), Module, Line, Count]++Args);
        _ ->
            skip
    end.


force_notice_msg(Module, Line, Format, Args) ->
    lager:notice("{~p} ~p:~p: "++Format++"~n",
        [util:process_name(), Module, Line]++Args).


warning_msg(Module, Line, Format, Args) ->
    warning_msg(Module, Line, ?NO_FUNC, ?NO_ARITY, Format, Args).

warning_msg(Module, Line, Func, Arity, Format, Args) ->
    case log_count_up(Module, Line) of
        Count when Count < ?LOG_WARNING_LIMIT ->
            lager:warning("{~p} ~p:~p/~p[~w] "++Format++"~n",
                [util:process_name(), Module, Func, Arity, Line]++Args);
        ?LOG_WARNING_LIMIT ->
            lager:warning("{~p} ~p:~p: (Silenced!) "++Format++"~n",
                [util:process_name(), Module, Line]++Args);
        Count when Count < 1000 andalso Count rem 100 =:= 0 ->
            lager:warning("{~p} ~p:~p: (~p Times!) "++Format++"~n",
                [util:process_name(), Module, Line, Count]++Args);
        Count when Count rem 1000 =:= 0 ->
            lager:warning("{~p} ~p:~p: (~p Times!) "++Format++"~n",
                [util:process_name(), Module, Line, Count]++Args);
        _ ->
            skip
    end.

force_warning_msg(Module, Line, Format, Args) ->
    lager:warning("{~p} ~p:~p: "++Format++"~n",
        [util:process_name(), Module, Line]++Args).


error_msg(Module, Line, Format, Args, Stack) ->
    error_msg(Module, Line, ?NO_FUNC, ?NO_ARITY, Format, Args, Stack).

error_msg(Module, Line, Func, Arity, Format, Args, _Stack) ->
    Stack = erlang:get_stacktrace(),
    {StackM, StackF, StackL} = get_mod_line(Module, Line, Stack),
    case log_count_up(StackM, StackL) of
        Count when Count < ?LOG_ERROR_LIMIT ->
            {StackM, StackF, StackL} = get_mod_line(Module, Line, Stack),
            lager:error("{~p} ~p:~p/~p[~w] "++Format++"~n~p~n",
                [util:process_name(), Module, Func, Arity, Line] ++ Args ++ [[stack_format(S) || S <- Stack]]);
        ?LOG_ERROR_LIMIT ->
            {StackM, StackF, StackL} = get_mod_line(Module, Line, Stack),
            lager:error("{~p} ~p:~p: (Silenced!) {~p:~p:~p} "++Format++"~n",
                [util:process_name(), Module, Line, StackM, StackF, StackL] ++ Args);
        Count when Count < 1000 andalso Count rem 100 =:= 0 ->
            {StackM, StackF, StackL} = get_mod_line(Module, Line, Stack),
            lager:error("{~p} ~p:~p: (~p Times!) {~p:~p:~p} "++Format++"~n",
                [util:process_name(), Module, Line, Count, StackM, StackF, StackL]++Args);
        Count when Count rem 1000 =:= 0 ->
            {StackM, StackF, StackL} = get_mod_line(Module, Line, Stack),
            lager:error("{~p} ~p:~p: (~p Times!) {~p:~p:~p} "++Format++"~n",
                [util:process_name(), Module, Line, Count, StackM, StackF, StackL]++Args);
        _ ->
            skip
    end.


force_error_msg(Module, Line, Format, Args, _Stack) ->
    Stack = erlang:get_stacktrace(),
    lager:error("{~p} ~p:~p: "++Format++"~n~p~n",
        [util:process_name(), Module, Line] ++ Args ++ [[stack_format(S) || S <- Stack]]).


force_error_msg_nostack(Module, Line, Format, Args) ->
    lager:error("{~p} ~p:~p: "++Format++"~n", [util:process_name(), Module, Line] ++ Args).


log_count_up(Module, Line) ->
    Mark = {?MODULE, Module, Line},
    case erlang:get(Mark) of
        undefined ->
            erlang:put(Mark, 1),
            1;
        Count ->
            erlang:put(Mark, Count + 1),
            Count + 1
    end.

get_mod_line(_Module, _Line, [{M, F, _A, Info}|_]) ->
    case lists:keyfind(line, 1, Info) of
        {_, L} ->
            {M, F, L};
        _ ->
            {M, F, F}
    end;
get_mod_line(Module, Line, _) ->
    {Module, unknown, Line}.

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

