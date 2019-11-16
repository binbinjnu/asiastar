%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 10月 2019 20:53
%%%-------------------------------------------------------------------
-module(net_protocol).
-author("Administrator").

-behaviour(gen_server).

-include_lib("kernel/src/inet_int.hrl").
-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_net.hrl").

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
    init/4,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).


%% 统计信息
-record(stat, {
    rec_time = 0, %% 时间戳 milli_seconds
    tcp_send = 0,
    tcp_recv = 0,
    tcp_send_count = 0,
    tcp_recv_count = 0
}).

-record(net_state, {
    socket :: port(),
    socket_type :: atom(),
    transport :: atom(),
    handler :: atom(),
    recv_res = <<>> :: binary(),
    sends = [] :: list(),
    last_packet_time = 0,
    % timeref :: reference() | undefined,
    water_lv = 0 :: integer(),
    stat = #stat{},
    handler_state
}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Ref, Socket, Transport, Opts) ->
    SpawnOpts = [{min_heap_size, 1024}, {min_bin_vheap_size, 128 * 1024}],
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts], infinity, SpawnOpts).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, undefined}.

init(Ref, Socket, Transport, Opts) ->
    Handler = maps:get(handler, Opts, net_handler),
    SocketType = maps:get(socket_type, Opts, tcp),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, true} | ?TCP_OPTIONS]),
    erlang:put(net_tcp_socket, Socket),
    RandInterval = util:rand(0, ?NET_STAT_INTERVAL * 1000),
    Stat = #stat{rec_time = util_time:long_unixtime() + RandInterval},
    State =
        #net_state{
            socket = Socket,
            socket_type = SocketType,
            transport = Transport,
            handler = Handler,  % 目前为lib_net
            recv_res = <<>>,
            stat = Stat,
            sends = [],
            handler_state = []
        },
    State1 = handler_up(State),
%%    ?fg_folsom(tcp_connect()),
%%    fg_net_checker:reg(),
    gen_server:enter_loop(?MODULE, [], State1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
handle_call(proc_check, _From, #net_state{last_packet_time = Time} = State) ->
    Now = util_time:long_unixtime(),
    case Now - Time of
        PassTime when PassTime >= ?NET_HEART_TIMEOUT * 1000 ->
            ?WARNING("net_heart_timeout ~p", [PassTime]),
            {stop, normal, ok, State};
        _ ->
            {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% 处理websocket消息
handle_info({tcp, Socket, Data}, #net_state{socket = Socket, socket_type = websocket} = State) ->
    ?NOTICE("Data:~p", [Data]),
    DataList = string:tokens(binary_to_list(Data), "\r\n"),
    MatchLine = hd(lists:filter(fun(S) -> lists:prefix("Sec-WebSocket-Key:", S) end, DataList)),
    List = string:tokens(MatchLine, ": "),
    ClientRandomString = lists:last(List),
    Key = list_to_binary(ClientRandomString),
    Challenge = base64:encode(crypto:hash(sha, <<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
    Handshake =
        ["HTTP/1.1 101 Switching Protocols\r\n",
         "connection: Upgrade\r\n",
         "upgrade: websocket\r\n",
         "sec-websocket-accept: ", Challenge, "\r\n",
         "\r\n",<<>>],
    erlang:port_command(Socket, Handshake),
    {noreply, State#net_state{socket_type = tcp}};

handle_info({tcp, Socket, Data}, #net_state{socket = Socket, socket_type = tcp} = State) ->
    ?NOTICE("Data:~w", [Data]),
    State1 = water_down(State),
    case net_data(Data, State1) of
        {ok, State2} ->
            State3 = folsom_stat(State2),
            {noreply, State3};
        {error, Reason} ->
            ?WARNING("Error: ~p", [Reason]),
            {stop, normal, State1}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    % ?TRACE(tcp_connect_closed),
    {stop, normal, State};

handle_info({tcp_error, _, Reason}, State) ->
    ?WARNING("Tcp_error: ~p", [Reason]),
    {stop, normal, State};

handle_info({pack_send, Data}, State) ->
    State1 = do_pack_send(Data, State),
    {noreply, State1};

handle_info({later, Time, Data}, #net_state{} = State) ->
    erlang:send_after(Time, self(), Data),
    {noreply, State};

handle_info({send, Data}, #net_state{} = State) ->
    State1 = buffer_send(Data, State),
    {noreply, State1};

handle_info({send_flush, Data}, State) ->
    State1 = buffer_send(Data, State),
    State2 = flush_send(State1),
    {noreply, State2};

handle_info({event, Event}, #net_state{handler = Mod, handler_state = HState} = State) ->
    HState1 = Mod:handle_evt(Event, HState),
    {noreply, State#net_state{handler_state = HState1}};

handle_info({inet_reply, _, ok}, State) ->
    {noreply, State};

handle_info({inet_reply, Socket, _Err}, #net_state{socket = Socket} = State) ->
    ?WARNING("inet_reply tcp error ~p", [_Err]),
    {stop, normal, State};

handle_info(_Info, State) ->
    ?WARNING("not match info: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #net_state{handler = Mod, handler_state = HState}) ->
    Mod:terminate(Reason, HState),
%%    ?fg_folsom(tcp_close()),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%% init handle
handler_up(#net_state{socket = Socket, handler = Mod} = NetState) ->
    HState = Mod:init(Socket),
    NetState#net_state{handler_state = HState}.

%%% do handle
handle(Message, #net_state{handler = Mod, handler_state = HState} = NetState) ->
    HState1 = Mod:handle(Message, HState),
    NetState#net_state{handler_state = HState1}.


%% 网络包水位
water_down(#net_state{water_lv = WaterLv, last_packet_time = Time} = NetState) ->
    Now = util_time:long_unixtime(),
    Down = erlang:max(0, (Now - Time) * ?NET_PACKET_PER_SEC div 1000),
    WaterLv1 = erlang:max(WaterLv - Down, 0),
    NetState#net_state{water_lv = WaterLv1, last_packet_time = Now}.


%% 网络包提取/监控
net_data(Bin, #net_state{recv_res = RecvRes, water_lv = WaterLv} = NetState) ->
    NetState1 = NetState#net_state{recv_res = <<>>},
    net_data(<<RecvRes/binary, Bin/binary>>, NetState1, WaterLv, ?NET_MAX_WATER_LV).

net_data(_Bin, _NetState, WaterLv, MaxWater) when WaterLv >= MaxWater ->
    {error, packet_flood};
net_data(<<Len:?PACKAGE_HEAD_LEN, Package:Len/binary, Res/binary>>, NetState, WaterLv, MaxWater) ->
    NetState1 = handle(Package, NetState),
    NetState2 = stat_count_up(tcp_recv, Len + 2, NetState1),
    WaterBySize = Len div 100, % 每100byte涨1, 防止大包攻击
    net_data(Res, NetState2, WaterLv + 1 + WaterBySize, MaxWater);
net_data(Bin, NetState, WaterLv, _MaxWater) ->
    {ok, NetState#net_state{recv_res = Bin, water_lv = WaterLv}}.


%% 收发数据大小和包数统计
stat_count_up(tcp_send, Val,
    #net_state{stat = #stat{tcp_send = Old, tcp_send_count = OldCount} = Stat} = NetState) ->
    NetState#net_state{stat = Stat#stat{tcp_send = Old + Val, tcp_send_count = OldCount + 1}};
stat_count_up(tcp_recv, Val,
    #net_state{stat = #stat{tcp_recv = Old, tcp_recv_count = OldCount} = Stat} = NetState) ->
    NetState#net_state{stat = Stat#stat{tcp_recv = Old + Val, tcp_recv_count = OldCount + 1}}.

folsom_stat(State) ->
    State.

%%folsom_stat(#net_state{last_packet_time=LastPackTime,
%%    stat=#stat{rec_time=RecTime,
%%        tcp_send=_TcpSend,
%%        tcp_recv=_TcpRecv,
%%        tcp_send_count=_TcpSendCount,
%%        tcp_recv_count=_TcpRecvCount}=Stat}=NetState) ->
%%    case LastPackTime >= RecTime of
%%        ?true ->
%%            ?fg_folsom(net_send_byte(_TcpSend + _UdpSend)),
%%            ?fg_folsom(net_recv_byte(_TcpRecv + _UdpRecv)),
%%            ?fg_folsom(net_send_packet(_TcpSendCount + _UdpSendCount)),
%%            ?fg_folsom(net_recv_packet(_TcpRecvCount + _UdpRecvCount)),
%%            Intv = fg_config:get(net_folsom_interval) * 1000,
%%            RecTime1 =
%%                case RecTime + Intv > LastPackTime of
%%                    ?true -> RecTime + Intv;
%%                    ?false -> LastPackTime + fg_util:rand(0, Intv)
%%                end,
%%            NetState#net_state{stat=Stat#stat{rec_time=RecTime1,
%%                tcp_send=0,
%%                tcp_recv=0,
%%                tcp_send_count=0,
%%                tcp_recv_count=0}};
%%        ?false ->
%%            NetState
%%    end.

%% pack send
do_pack_send(Data, State) ->
    do_pack_send_1(Data, State, 0, 0).

do_pack_send_1(Data, #net_state{handler = Mod} = State, N, Len) ->
    % ?TRACE(Data),
    Data1 = Mod:pack(Data),
    buffer_send_1(Data1, State, N, Len).


buffer_send(Data, State) ->
    buffer_send_1(Data, State, 0, 0).

buffer_send_1(Data, #net_state{sends = Sends} = State, N, Len) ->
    try erlang:iolist_size(Data) of   %% size in bytes
        Len1 when Len + Len1 >= ?PACK_FLUSH_SIZE ->
            flush_send(State#net_state{sends = [Data | Sends]});
        _ when N >= ?MAX_PACK_ACC ->
            flush_send(State#net_state{sends = [Data | Sends]});
        Len1 ->
            State1 = State#net_state{sends = [Data | Sends]},
            receive
                {pack_send, Data2} ->
                    do_pack_send_1(Data2, State1, N + 1, Len + Len1);
                {send, Data2} ->
                    buffer_send_1(Data2, State1, N + 1, Len + Len1)
            after 1 ->
                flush_send(State1)
            end
    catch
        _:_ ->
            ?WARNING("Cannot send Data:~p", [Data]),
            flush_send(State) % 注意是State
    end.


flush_send(#net_state{sends = []} = State) ->
    State;
flush_send(#net_state{socket = Socket, sends = Sends} = State) ->
    Sends1 = lists:reverse(Sends),
    {Bin1, State1} = prepare_sends(Sends1, State),
    Size1 = erlang:iolist_size(Bin1),
    case Size1 > 0 of
        ?true ->
            try
                erlang:port_command(Socket, Bin1)
            catch
                error:_Reason -> % 端口被关闭也会badarg的
                    ?WARNING("Send error:~p", [_Reason]),
                    erlang:exit(normal)
            end;
        _ ->
            ok
    end,
    State2 = stat_count_up(tcp_send, Size1, State1),
    State2#net_state{sends = []}.


prepare_sends(Data, #net_state{handler = Handler, handler_state = HState} = NetState) ->
    case erlang:function_exported(Handler, prepare_sends, 2) of
        ?true ->
            {Data1, HState1} = Handler:prepare_sends(Data, HState),
            {Data1, NetState#net_state{handler_state = HState1}};
        _ ->
            {Data, NetState}
    end.
