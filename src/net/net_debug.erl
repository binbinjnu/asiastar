%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%      网络包输出
%%% @end
%%% Created : 30. 10月 2019 10:35
%%%-------------------------------------------------------------------
-module(net_debug).
-author("Administrator").

-include_lib("kernel/include/file.hrl").
-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_proto.hrl").

%% API
-export([]).

-export([
    init/1,
    start_debug/1,
    debug_msg/2,
    stop_debug/0
]).

-define(NET_PROTO_DEBUG_PATH, "./logs/proto/"). % 网络协议输出路径

init(InitTime) ->
    case should_init() of
        ?true ->
            start_debug(InitTime);
        _ ->
            pass
    end.


should_init() ->
    UID = erlang:get(user_id),
    case config:get(net_proto_debug) of
        0 ->
            ?false;
        1 ->
            net_debug_gsvr:should_log(UID);
        Filter when Filter > 0 andalso UID rem Filter =:= 1 ->
            net_debug_gsvr:should_log(UID);
        _ ->
            ?false
    end.


start_debug(InitTime) ->
    case is_debug() of
        ?false ->
            UID = erlang:get(user_id),
            Filename = lists:concat(["pt_", UID, ".txt"]),
            FullFn = filename:join(?NET_PROTO_DEBUG_PATH, Filename),
            filelib:ensure_dir(FullFn),
            case file:read_file_info(FullFn) of
                {ok, #file_info{ctime = {CDate, _CTime}}} ->
                    case CDate =/= erlang:date() of
                        ?true ->
                            FullFnBak = FullFn ++ ".bak",
                            file:rename(FullFn, FullFnBak);
                        _ ->
                            pass
                    end;
                _Err ->
                    pass
            end,
            {ok, Fd} = file:open(FullFn, [append, raw]),
            file:write(Fd, "\n\n\n\n"),
            file:write(Fd, io_lib:format("========= Connect Time: ~p ========~n", [InitTime])),
            erlang:put(net_debug_fd, Fd);
        _ ->
            ok
    end.


stop_debug() ->
    erlang:erase(net_debug_fd).


is_debug() ->
    erlang:get(net_debug_fd) =/= ?undefined.


debug_msg(Type, IOList) ->
    case is_debug() of
        ?true ->
            catch do_debug_msg(Type, erlang:iolist_to_binary(IOList), []);
        _ ->
            pass
    end.

do_debug_msg(Type, <<Len:16, MsgBin:Len/binary, Res/binary>>, Acc) ->
    <<MsgID:16, Bin/binary>> = MsgBin,
    Acc1 =
        case is_ignore_msg(MsgID) of
            ?false ->
                Fun = ?IF(Type =:= send, get_s2c, get_c2s),
                case data_proto:Fun(MsgID) of
                    {MsgName, MsgPb} ->
                        Msg = MsgPb:decode(MsgName, Bin),
                        Rec = {erlang:time(), util_time:long_unixtime(), Type, MsgID, Msg},
                        [Rec | Acc];
                    _ ->
                        Acc
                end;
            ?true ->
                Acc
        end,
    do_debug_msg(Type, Res, Acc1);
do_debug_msg(_, <<>>, []) ->
    ok;
do_debug_msg(_, <<>>, Recs) ->
    write_msgs(Recs),
    ok;
do_debug_msg(_, _E, _) ->
    ?INFO("rec_msg_fail, Size:~w", [byte_size(_E)]),
    ok.

write_msgs(Recs) ->
    case erlang:get(net_debug_fd) of
        ?undefined ->
            pass;
        Fd ->
            file:write(Fd, reverse_msgs(Recs, []))
    end.

reverse_msgs([H | T], Acc) ->
    Rec = [util:term_to_bitstring(H), "\n"],
    Acc1 = [Rec | Acc],
    reverse_msgs(T, Acc1);
reverse_msgs([], Acc) ->
    Acc.


is_ignore_msg(0) -> ?true;
is_ignore_msg(?c2s_hello) -> ?true;
is_ignore_msg(_) -> ?false.
