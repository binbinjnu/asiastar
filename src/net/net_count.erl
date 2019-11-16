%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%      网络包数量长度统计
%%% @end
%%% Created : 30. 10月 2019 10:37
%%%-------------------------------------------------------------------
-module(net_count).
-author("Administrator").

-include("hrl_common.hrl").

%% API
-export([]).

%%-export([
%%    boot/0,
%%    init/0,
%%    count_msg/2,
%%    dump/0
%%]).
%%
%%-define(TAB, ?MODULE).
%%-define(DUMP_INTERVAL, 60 * 60 * 1000).
%%
%%boot() ->
%%    fg_ets:hold_new(?TAB, [named_table, {write_concurrency, true}]),
%%    timer:apply_interval(?DUMP_INTERVAL, ?MODULE, dump, []).
%%
%%
%%init() ->
%%    case should_init() of
%%        ?true ->
%%            ?TRAC(should_net_count),
%%            erlang:put(?MODULE, ?true);
%%        _ ->
%%            ?TRAC(should_not_net_count),
%%            pass
%%    end.
%%
%%
%%should_init() ->
%%    UUID = erlang:get(player_uuid),
%%    Type = xg_node:node_type(),
%%    case xg_config:get(net_proto_count_filter) of
%%        0 ->
%%            ?false;
%%        1 ->
%%            net_count_mgr:should_log(UUID, Type);
%%        Filter when Filter > 0 andalso UUID rem Filter =:= 1 ->
%%            net_count_mgr:should_log(UUID, Type);
%%        _ ->
%%            ?false
%%    end.
%%
%%
%%count_msg(Type, Iolist) ->
%%    case erlang:get(?MODULE) of
%%        ?true ->
%%            catch do_count_msg(Type, erlang:iolist_to_binary(Iolist));
%%        _ ->
%%            pass
%%    end.
%%
%%do_count_msg(Type, <<Len:16, MsgBin:Len/binary, Res/binary>>) ->
%%    <<MsgID:16, _Bin/binary>> = MsgBin,
%%    case is_ignore_msg(MsgID) of
%%        ?false ->
%%            Key = ?IF(Type=:=send, data_msg_s2c:get(MsgID), data_msg_c2s:get(MsgID)),
%%            ets:update_counter(?TAB, Key, [{2, 1}, {3, Len}], {Key, 0, 0}),
%%            do_count_msg(Type, Res);
%%        ?true ->
%%            do_count_msg(Type, Res)
%%    end;
%%do_count_msg(_, <<>>) ->
%%    ok;
%%do_count_msg(_, _E) ->
%%    ok.
%%
%%
%%is_ignore_msg(0)     -> ?true;
%%is_ignore_msg(?s2c_mwar_debug_bullet)     -> ?true;
%%is_ignore_msg(?s2c_mwar_debug_pos)        -> ?true;
%%is_ignore_msg(?s2c_mwar_debug_line)       -> ?true;
%%is_ignore_msg(?s2c_mwar_debug_word)       -> ?true;
%%is_ignore_msg(_)                          -> ?false.
%%
%%
%%dump() ->
%%    F = fun({Msg, Count, Len}, _) ->
%%        ets:delete(?TAB, Msg),
%%        xg_influx:write(proto, #{count=>Count, size=>Len}, #{pt_name=>Msg}),
%%        timer:sleep(10)
%%        end,
%%    ets:foldl(F, 0, ?TAB).

