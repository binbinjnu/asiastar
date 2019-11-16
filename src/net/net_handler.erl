%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 10月 2019 10:33
%%%-------------------------------------------------------------------
-module(net_handler).
-author("Administrator").

-include("record.hrl").
-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_net.hrl").

%% API

-export([
    init/1,
    handle/2,
    pack/1,
    handle_evt/2,
    prepare_sends/2,
    terminate/2
]).

-export([
    get_tcp_socket/1
]).

-define(RECV_KEY, <<"7918cae8">>).
-define(SEND_KEY, <<"7918cae8">>).
-define(ZIP_MIN_LEN, 100).
-define(MAX_HEAP_SIZE, 5 * 1024 * 1024). % 最大堆内存大小(word)

-define(REC_MSG(Type, Bin), net_debug:debug_msg(Type, Bin)).

%%-define(REC_MSG(Type, Bin),
%%    net_debug:debug_msg(Type, Bin),
%%    net_count:count_msg(Type, Bin)).


get_tcp_socket(SPid) ->
    Ref = erlang:make_ref(),
    net_api:event(SPid, {get_tcp_socket, self(), Ref}),
    receive
        {Ref, Socket} ->
            Socket
    after
        1000 ->
            ?undefined
    end.

init(Socket) ->
    erlang:process_flag(max_heap_size, ?MAX_HEAP_SIZE),
    erlang:put(sender_pid, self()),
%%    lib_send:set_sender(self()),
    erlang:put(process_type, sender),
    PeerName = socket2peername(Socket),
    ?INFO("PeerName:~w", [PeerName]),
    #handler_state{
        socket = Socket,
        peername = PeerName,
        init_time = util_time:localtime()
    }.


socket2peername(?undefined) ->
    {{0, 0, 0, 0}, 0};
socket2peername(Socket) ->
    case inet:peername(Socket) of
        {ok, PN} ->
            PN;
        _Err ->
            ?WARNING("Get peername error: ~p", [_Err]),
            {{0, 0, 0, 0}, 0}
    end.


handle(Bin, #handler_state{debug_pid = Pid} = HState) ->
    ?REC_MSG(recv, Bin),
    case is_pid(Pid) of
        ?true -> Pid ! {recv, Bin};
        _ -> ok
    end,
    Packets = net_pack:unpacks(Bin),
    HState1 = handle_1(Packets, HState),
    HState1.

handle_1([], HState) ->
    HState;
handle_1([{MsgID, Content} | T], HState) ->
    HState2 = handle_2(MsgID, Content, HState),
    handle_1(T, HState2).

handle_2(MsgID, Content, #handler_state{pid = ?undefined} = HState) ->     %% 本地的
    handle_local(MsgID, Content, HState);
handle_2(MsgID, Content, #handler_state{} = HState) when MsgID div 1000 =:= 10 ->  %% 本地的Login
    handle_local(MsgID, Content, HState);
handle_2(MsgID, Content, #handler_state{pid = Pid} = HState) ->            %% 对应Pid的
    gen_server:cast(Pid, {route_msg, MsgID, Content}),
    HState.


handle_local(MsgID, Content, HState) ->
    try net_route:route_msg(MsgID, Content, HState) of
        {noreply, #handler_state{} = HState1} ->
            HState1;
        {stop, #handler_state{} = HState1} ->
            self() ! stop,
            HState1;
        _E ->
            ?WARNING("Invalid state. MsgID: ~p, state ~p", [MsgID, _E]),
            HState
    catch
        Err:Reason ->
            ?ERROR("~p:~p", [Err, Reason]),
            HState
    end.


pack(Msg) ->
    net_pack:pack(Msg).


handle_evt(Req, HState) ->
    try do_handle_evt(Req, HState) of
        #handler_state{} = HState1 ->
            HState1;
        _E ->
            ?WARNING("Invalid return ~p", [_E]),
            HState
    catch Err:Reason ->
        ?ERROR("~p:~p", [Err, Reason]),
        HState
    end.


%%do_handle_evt(queue_max, HState) ->
%%    xg_login:queue_max(HState);
%%
%%do_handle_evt(queue_ok, HState) ->
%%    xg_login:allow_select_role(HState);
%%
%%do_handle_evt({debug_pid, Pid}, HState) ->
%%    HState#handler_state{debug_pid = Pid};

%%do_handle_evt({test_rtt, From, Info}, HState) ->
%%    event:trigger_event(From, ?EVT_PLAYER_MAIN_PING_ACK, Info),
%%    HState;

do_handle_evt(login_again, HState) ->
    erlang:send_after(1000, self(), stop),
%%    ?SEND_ERR(?ERR_ACCOUNT_LOGIN_AGAIN),
    HState#handler_state{pid = ?undefined};

do_handle_evt(net_debug_start, #handler_state{init_time = InitTime} = HState) ->
    net_debug:start_debug(InitTime),
    HState;

do_handle_evt({get_tcp_socket, From, Ref}, #handler_state{socket = Socket} = HState) ->
    From ! {Ref, Socket},
    HState;

do_handle_evt(net_debug_stop, HState) ->
    net_debug:stop_debug(),
    HState;

do_handle_evt({test_client_msg, Msg}, HState) ->
    handle_1(Msg, HState);

do_handle_evt(_E, HState) ->
    ?WARNING("unhandle event ~p", [_E]),
    HState.


prepare_sends(Sends, HState) ->
    try do_prepare_sends(Sends, HState)
    catch
        _:_ ->
            ?ERROR("Cannot prepare send ~p", [Sends]),
            {[], HState}
    end.

do_prepare_sends([], #handler_state{} = HState) ->
    {[], HState};
do_prepare_sends(Sends, #handler_state{debug_pid = Pid} = HState) ->
    ?REC_MSG(send, Sends),
    case is_pid(Pid) of
        ?true -> Pid ! {send, Sends};
        _ -> ok
    end,
    case erlang:iolist_size(Sends) of
        0 ->
            {[], HState};
        Len when Len > ?ZIP_MIN_LEN ->
            ?IF(Len >= 50000, ?WARNING("Huge net pack ~p", [Len]), ok),
            % ?IF(Len >= 32768, erlang:put(huge_pack, do_rec_send(Sends)), ok),
            {ok, Bin1} = lz4:compress(Sends),
            Len2 = byte_size(Bin1),
            {[<<Len2:16, 1:8>>, Bin1], HState};
        Len ->
            {[<<Len:16, 0:8>>, Sends], HState}
    end.


%% 通过monitor实现, 不主动发消息
% terminate(Reason, #handler_state{pid=Pid}) when is_pid(Pid) ->
%     event:trigger_event(Pid, ?EVT_PLAYER_OFFLINE, Reason);
terminate(_Reason, _HState) ->
    ok.
