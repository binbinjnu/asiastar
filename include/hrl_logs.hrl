%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 10月 2019 18:56
%%%-------------------------------------------------------------------
-author("Administrator").

-ifndef(X2_LOG_HRL).
-define(X2_LOG_HRL, true).

-define(INFO(Format, Args), logs_api:info_msg(?MODULE,?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Format, Args)).
-define(INFO(Format), ?INFO(Format, [])).
-define(NOTICE(Format, Args), logs_api:notice_msg(?MODULE,?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Format, Args)).
-define(NOTICE(Format), ?NOTICE(Format, [])).
-define(WARNING(Format, Args), logs_api:warning_msg(?MODULE,?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Format, Args)).
-define(WARNING(Format), ?WARNING(Format, [])).
-define(ERROR(Format, Args, Stack), logs_api:error_msg(?MODULE,?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Format, Args, Stack)).
-define(ERROR(Format, Args), ?ERROR(Format, Args, [])).
-define(ERROR(Format), ?ERROR(Format, [])).

-define(FORCE_NOTICE(Format, Args), logs_api:force_notice_msg(?MODULE,?LINE, Format, Args)).
-define(FORCE_WARNING(Format, Args), logs_api:force_warning_msg(?MODULE,?LINE, Format, Args)).
-define(FORCE_ERROR(Format, Args, Stack), logs_api:force_error_msg(?MODULE,?LINE, Format, Args, Stack)).
-define(FORCE_ERROR(Format, Args), ?FORCE_ERROR(Format, Args, [])).

-define(PRINT(X),
    io:format("~p|~w:~w ====> ~p ~n", [logs_api:pname(), ?MODULE,?LINE,X])).

-define(DEBUG(Format, Args), logs_api:debug_msg(?MODULE, ?LINE, ?FUNCTION_NAME, ?FUNCTION_ARITY, Format, Args)).
-define(TRACE(X),
    ?DEBUG("~p", [X]),
    io:format("~p|~w:~w/~w[~w] ==> ~p ~n", [logs_api:pname(),?MODULE,?FUNCTION_NAME,?FUNCTION_ARITY,?LINE,X])).
-define(TTRACE(X),
    io:format("~w:~w:~w.~w ~p|~w[~w] ==> ~p ~n",
        tuple_to_list(erlang:time()) ++ [erlang:system_time(milli_seconds) rem 1000, logs_api:pname(), ?MODULE, ?LINE,X])).
-define(TRACE_IF(B, X), case B of true -> ?TRACE(X); _ -> ok end).

-endif.
