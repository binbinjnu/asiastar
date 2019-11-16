%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     心跳包检测
%%% @end
%%% Created : 05. 11月 2019 19:57
%%%-------------------------------------------------------------------
-module(heartbeat).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-define(HEARTBEAT_INTERVAL, 15). % 心跳时间 s
-define(ALLOW_HEART_FAST, 1). % 最大允许心跳过快次数

%% API
-export([beat/1]).

beat(Time) ->
    case erlang:put(heartbeat, Time) of
        ?undefined ->
            ok;
        LastTime ->
            check_fast(Time, LastTime)
    end.

check_fast(Now, Last) ->
    Interval = Now - Last,
    MinInterval = ?HEARTBEAT_INTERVAL * 1000 - 500, % 允许0.5秒误差
    case Interval < MinInterval of
        ?true -> % 过快
            case erlang:get(heartbeat_fast) of
                ?undefined ->
                    ?WARNING("heart fast", []),
                    erlang:put(heartbeat_fast, 1);
                Times when Times >= ?ALLOW_HEART_FAST ->
                    ?WARNING("heart fast, Times:~w", [Times]),
                    too_fast();
                Times ->
                    erlang:put(heartbeat_fast, Times + 1)
            end;
        _ -> % 正常
            case erlang:get(heartbeat_fast) of
                ?undefined ->
                    ok;
                Times when Times > 1 ->
                    erlang:put(heartbeat_fast, Times - 1);
                _ -> % =< 1
                    erlang:erase(heartbeat_fast)
            end
    end.

too_fast() ->
%%    lib_send:err_code(self(), ?ERR_HEART_TOO_FAST),
    net_api:stop(self()).
