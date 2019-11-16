%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     协议encode decode
%%% @end
%%% Created : 30. 10月 2019 10:51
%%%-------------------------------------------------------------------
-module(net_pack).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

%% API
-export([
    pack/1,
    pack_bin/1,
    unpacks/1]).

pack_bin(Msg) ->
    Bin = pack(Msg),
    iolist_to_binary(Bin).

pack(Msgs) ->
    try
        do_pack(Msgs)
    catch
        Err:Reason ->
            ?WARNING("Invalid Msgs: ~p, Err:~p, Reason:~p", [Msgs, Err, Reason]),
            []
    end.

do_pack(Msgs) when is_list(Msgs) ->
    [do_pack(Msg) || Msg <- Msgs];
do_pack(Msg) when is_binary(Msg)->
    Msg;
do_pack({}) ->
    [];
do_pack(Msg) when is_tuple(Msg)->
    MsgName = erlang:element(1, Msg),
    case data_proto:get(MsgName) of
        {MsgID, MsgPb} ->
            try
                Content = MsgPb:encode_msg(Msg),
                Len = erlang:iolist_size(Content) + 2,
                [<<Len:16, MsgID:16>>, Content]
            catch _:_ ->
                ?ERROR("Invalid msg ~p",[Msg]),
                []
            end;
        _ ->
            []
    end;
do_pack(?undefined) ->
    [];
do_pack(_E) ->
    ?WARNING("Invalid Msg:~p", [_E]),
    erlang:error(invalid_msg).

unpacks(Bin) ->
    unpacks(Bin, []).

unpacks(<<>>, Acc) ->
    lists:reverse(Acc);
unpacks(<<2:16, MsgID:16, Res/binary>>, Acc) ->
    try decode(MsgID, <<>>) of
        error ->
            unpacks(Res, Acc);
        Cont ->
            unpacks(Res, [{MsgID, Cont}|Acc])
    catch
        _:_ ->
            ?WARNING("Decode error ~w: ~w", [MsgID, <<>>]),
            unpacks(Res, Acc)
    end;
unpacks(<<Len:16, Bin:Len/binary, Res/binary>>, Acc) ->
    <<MsgID:16, Bin1/binary>> = Bin,
    try decode(MsgID, Bin1) of
        error ->
            unpacks(Res, Acc);
        Cont ->
            % ?TRAC(Cont),
            unpacks(Res, [{MsgID, Cont}|Acc])
    catch
        _:_ ->
            ?WARNING("Decode error ~w: ~w", [MsgID, Bin1]),
            unpacks(Res, Acc)
    end;
unpacks(_E, Acc) ->
    ?WARNING("msg err ~p", [_E]),
    lists:reverse(Acc).


decode(MsgID, Bin) -> % 新协议
    case data_proto:get_c2s(MsgID) of
        {MsgName, MsgPb} ->
            MsgPb:decode_msg(Bin, MsgName);
        _ ->
            error
    end.

%% 原生的pb性能约高 50%

% 15> G = [{oid,20},{gid,30101},{num,10}].
% 16> G2 = {s2cgoodsinfo_goodsinfo, 20, 30101,10}.

% 17> bench:rp(pbson, encode, [G]).
% Single Process:    1261065 call per sec,    2097152 times in  1663 ms
%      1 Process:    1125685 call per sec,    2097152 times in  1863 ms
%     10 Process:    3855058 call per sec,    5242880 times in  1360 ms
%    100 Process:    3900952 call per sec,    6553600 times in  1680 ms
%   1000 Process:    3778597 call per sec,    4096000 times in  1084 ms
% ok
% 18> bench:rp(message_pb, encode, [G2]).
% Single Process:    1710564 call per sec,    2097152 times in  1226 ms
%      1 Process:    1630755 call per sec,    2097152 times in  1286 ms
%     10 Process:    5714310 call per sec,   10485760 times in  1835 ms
%    100 Process:    5877668 call per sec,    6553600 times in  1115 ms
%   1000 Process:    5595628 call per sec,    8192000 times in  1464 ms
% ok

% E = [{oid,7},
%  {eid,17001},
%  {pos,6},
%  {elevel,1},
%  {grade,0},
%  {sate,0},
%  {strong,0},
%  {gems,[]}]

% E2 = {goodseqbrief, 7, 17001, 6, 1, 0 , 0,0,[]}.

% 12> bench:rp(pbson, encode, [E]).
% Single Process:     643693 call per sec,    1048576 times in  1629 ms
%      1 Process:     606463 call per sec,    1048576 times in  1729 ms
%     10 Process:    2107266 call per sec,    2621440 times in  1244 ms
%    100 Process:    2137508 call per sec,    3276800 times in  1533 ms
%   1000 Process:    2045954 call per sec,    2048000 times in  1001 ms
% ok

% 14> bench:rp(message_pb, encode, [E2]).
% Single Process:     920611 call per sec,    1048576 times in  1139 ms
%      1 Process:     830884 call per sec,    1048576 times in  1262 ms
%     10 Process:    2872810 call per sec,    5242880 times in  1825 ms
%    100 Process:    2978909 call per sec,    3276800 times in  1100 ms
%   1000 Process:    2913229 call per sec,    4096000 times in  1406 ms
% ok

