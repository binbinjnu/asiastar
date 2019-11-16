%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     进程信息监控, 监控进程消息队列, 将消息队列过长的进程kill掉
%%% @end
%%% Created : 31. 10月 2019 18:43
%%%-------------------------------------------------------------------
-module(proc_checker_gsvr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([reg/2]).
-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    loop,
    slot_num,
    slots,
    kill_msg_q,
    check_msg_q,
    check_timeout
}).


-define(DEFAULT_SLOTS_NUM, 60).        %% 默认槽位数量
-define(DEFAULT_INTERVAL, 1000).      %% 默认时间间隔
-define(DEFAULT_KILL_MSG_Q, 1000).      %% 默认超过
-define(DEFAULT_CHECK_MSG_Q, 0).         %%
-define(DEFAULT_CHECK_TIMEOUT, 5000).      %% 默认检测超时

reg(Server, Pid) ->
    gen_server:cast(Server, {reg, Pid}).


start_link(ServerName, Args) ->
    gen_server:start_link({local, ServerName}, ?MODULE, Args, []).


init(Args) ->
    SlotNum = maps:get(slot_num, Args, ?DEFAULT_SLOTS_NUM),
    Interval = maps:get(interval, Args, ?DEFAULT_INTERVAL),
    KillMsgQ = maps:get(kill_msg_q, Args, ?DEFAULT_KILL_MSG_Q),
    CheckMsgQ = maps:get(check_msg_q, Args, ?DEFAULT_CHECK_MSG_Q),
    CheckTimeout = maps:get(check_timeout, Args, ?DEFAULT_CHECK_TIMEOUT),
    timer_gsvr:reg(self(), Interval),
    Slots = erlang:list_to_tuple(lists:duplicate(SlotNum, [])),
    {ok, #state{loop = 0,
        slots = Slots,
        slot_num = SlotNum,
        kill_msg_q = KillMsgQ,
        check_msg_q = CheckMsgQ,
        check_timeout = CheckTimeout}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({reg, Pid}, State) ->
    State1 = do_reg(Pid, State),
    {noreply, State1};

handle_cast(Msg, State) ->
    ?WARNING("handle cast is not match : ~p", [Msg]),
    {noreply, State}.

handle_info(doloop, #state{loop = Loop} = State) ->
    State1 = State#state{loop = Loop + 1},
    State2 = loop_check(State1),
    {noreply, State2};

handle_info(Info, State) ->
    ?WARNING("~p not handle.", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.


do_reg(Pid, #state{slots = Slots, slot_num = SlotNum} = State) ->
    N = erlang:phash2(Pid, SlotNum),
    Slot = erlang:element(N + 1, Slots),
    Slot1 =
        case lists:member(Pid, Slot) of
            ?true -> Slot;
            ?false -> [Pid | Slot]
        end,
    Slots1 = erlang:setelement(N + 1, Slots, Slot1),
    State#state{slots = Slots1}.

loop_check(#state{loop = Loop,
    slots = Slots,
    slot_num = SlotNum} = State) ->
    N = Loop rem SlotNum,
    Slot = erlang:element(N + 1, Slots),
    Slot1 = loop_check_1(Slot, State, [], []),
    Slots1 = erlang:setelement(N + 1, Slots, Slot1),
    State#state{slots = Slots1}.

loop_check_1([Pid | T],
    #state{kill_msg_q = KillMsgQ, check_msg_q = CheckMsgQ} = State,
    Killed,
    Acc) ->
    case erlang:process_info(Pid, message_queue_len) of
        ?undefined -> % 进程已死
            loop_check_1(T, State, Killed, Acc);
        {message_queue_len, Len} when Len >= KillMsgQ ->
            erlang:exit(Pid, kill),
            loop_check_1(T, State, [{Pid, Len} | Killed], Acc);
        {message_queue_len, Len} when Len >= CheckMsgQ ->
            check(Pid, State),
            loop_check_1(T, State, Killed, [Pid | Acc]);
        {message_queue_len, _Len} ->
            loop_check_1(T, State, Killed, [Pid | Acc])
    end;
loop_check_1([], _State, [], Acc) ->
    Acc;
loop_check_1([], _State, Killed, Acc) ->
    spawn(fun() ->
        ?WARNING("Killed msg_q overflow processes: ~p", [Killed]) end),
    Acc.


check(Pid, #state{check_timeout = Timeout}) ->
    spawn(fun() ->
        try gen_server:call(Pid, proc_check, Timeout)
        catch
            _:_ ->
                ?WARNING("Proc ~p check timeout, killed", [util:process_name(Pid)]),
                erlang:exit(Pid, kill)
        end
          end).