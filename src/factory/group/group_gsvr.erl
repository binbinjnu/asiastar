%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     TODO delay时, 可做逻辑上的优化, 有join或leave的时候直接先把旧的广播出去
%%% @end
%%% Created : 04. 11月 2019 19:20
%%%-------------------------------------------------------------------
-module(group_gsvr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([
    start_link/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    type = ?undefined,      %% group类型(名字)
    groups = [],
    tmp_groups = [],
    loop_ref = ?undefined,
    count = 0,
    waits = [],
    delay = 0,              %% 延迟发送时间, ms
    cache_cmds = [],
    clear_time = 0,
    to_be_clear = ?false,
    strict = ?false
}).

-define(BCAST_DELAY, 0).        %% 广播延迟
-define(CLEAR_EMPTY_GROUP_INTERVAL, 10).    %% 清除空的group间隔

-spec start_link(GroupType, Opts) -> {ok, pid()} | {error, term()} when
    Opts :: [term()],
    GroupType :: term().
start_link(GroupType, Opts) ->
    case proplists:get_bool(noname, Opts) of
        ?true ->
            gen_server:start_link(?MODULE, [GroupType, Opts], []);
        _ ->
            Name = group_api:group_name(GroupType),
            gen_server:start_link({local, Name}, ?MODULE, [GroupType, Opts], [])
    end.


init([GroupType, Opts]) ->
    Delay = proplists:get_value(delay, Opts, ?BCAST_DELAY),
    Strict = proplists:get_value(strict, Opts, ?false),
    {ok, #state{type = GroupType, delay = Delay, strict = Strict}}.


handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p,Reason:~p", [Err, Reason]),
            {reply, error, State}
    end.


handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p,Reason:~p", [Err, Reason]),
            {noreply, State}
    end.


handle_info(Request, State) ->
    try
        do_info(Request, State)
    catch
        Err:Reason ->
            ?ERROR("ERR:~p,Reason:~p", [Err, Reason]),
            {noreply, State}
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


do_call(groups, _From, #state{groups = Groups} = State) ->
    {reply, Groups, State};

do_call(count, _From, #state{count = Count} = State) ->
    {reply, Count, State};

do_call({count, GroupID}, _From, State) ->
    Count = length(members(GroupID)),
    {reply, Count, State};

do_call({members, GroupID}, _From, State) ->
    Members = members(GroupID),
    {reply, Members, State};

do_call(wait_loop, _From, #state{loop_ref = ?undefined} = State) ->
    {reply, ok, State};

do_call(wait_loop, From, #state{waits = Waits} = State) ->
    State1 = State#state{waits = [From | Waits]},
    {noreply, State1};

do_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.


do_cast({bcast_to_all, GroupIDs, Msg}, #state{delay = 0} = State) ->
    case erlang:iolist_to_binary(Msg) of
        <<>> ->
            ok;
        Msg1 ->
            [[net_api:send(M, Msg1) || M <- members(GroupID)] || GroupID <- GroupIDs]
    end,
    {noreply, State};

%% 对网络广播做优化, 缓存广播信息
do_cast({bcast_to_all, GroupIDs, Msg}, State) ->
    State1 =
        case erlang:iolist_to_binary(Msg) of
            <<>> ->
                State;
            Msg1 ->
                cache_bcast(GroupIDs, Msg1, State)
        end,
    State2 = set_timer(State1),
    {noreply, State2};

%% 分批广播
do_cast({batch_bcast_to_all, GroupMsgs}, #state{delay = 0} = State) ->
    F = fun({GroupIDs, Msg}) ->
            case erlang:iolist_to_binary(Msg) of
                <<>> ->
                    ok;
                Msg1 ->
                    [[net_api:send(M, Msg1) || M <- members(GroupID)] || GroupID <- GroupIDs]
            end
        end,
    lists:foreach(F, GroupMsgs),
    {noreply, State};

do_cast({batch_bcast_to_all, GroupMsgs}, State) -> % 对网络广播做优化
    F = fun({GroupIDs, Msg}, FState) when GroupIDs =/= [] ->
            case erlang:iolist_to_binary(Msg) of
                <<>> -> FState;
                Msg1 -> cache_bcast(GroupIDs, Msg1, FState)
            end;
        (_, FState) ->
            FState
        end,
    State1 = lists:foldl(F, State, GroupMsgs),
    State2 = set_timer(State1),
    {noreply, State2};

do_cast({to_all, {Mod, Func}, GroupID, Msg}, State) ->
    Members = members(GroupID),
    [Mod:Func(M, Msg) || M <- Members],
    {noreply, State};

do_cast({to_mall, {Mod, Func}, GroupIDs, Msg}, State) ->
    [[Mod:Func(M, Msg) || M <- members(GroupID)] || GroupID <- GroupIDs],
    {noreply, State};

do_cast({to_other, {Mod, Func}, GroupID, Msg, MyPID}, State) ->
    Members = members(GroupID),
    [Mod:Func(M, Msg) || M <- Members, M =/= MyPID],
    {noreply, State};

do_cast(close, State) ->
    {stop, normal, State};

do_cast(Cmd, State) ->
    cache_cmd(Cmd, State).


do_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    State1 = do_leave_all(Pid, State),
    {noreply, State1};

do_info(doloop, #state{groups = Groups, tmp_groups = TmpGroups, waits = Waits} = State) ->
    [send_cache(G, State) || G <- TmpGroups ++ Groups],
    [gen_server:reply(W, ok) || W <- Waits],
    State1 = run_cached_cmd(State),
    State2 = clear_empty_groups(State1),
    {noreply, State2#state{loop_ref = ?undefined, waits = [], cache_cmds = [], tmp_groups = []}};

do_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.


set_timer(#state{loop_ref = ?undefined, delay = Delay} = State) ->
    Ref = erlang:send_after(Delay, self(), doloop),
    State#state{loop_ref = Ref};
set_timer(State) ->
    State.


cache_cmd(Cmd, #state{cache_cmds = Cmds, strict = ?true} = State) ->
    State1 = State#state{cache_cmds = [Cmd | Cmds]},
    State2 = cache_bcast_cmd(Cmd, State1),
    {noreply, State2};
cache_cmd(Cmd, #state{} = State) ->
    State1 = run_cmd(Cmd, State),
    {noreply, State1}.


run_cached_cmd(#state{cache_cmds = Cmds} = State) ->
    Cmds1 = lists:reverse(Cmds),
    State1 = lists:foldl(fun run_cmd/2, State, Cmds1),
    State1.


run_cmd({join, GroupID, Pid}, #state{} = State) when is_pid(Pid) ->
    State1 = do_join(GroupID, Pid, State),
    State1;

run_cmd({join_many, GroupIDs, Pid}, #state{} = State) when is_pid(Pid) ->
    F = fun(GroupID, S) -> do_join(GroupID, Pid, S) end,
    State1 = lists:foldl(F, State, GroupIDs),
    State1;

run_cmd({leave, GroupID, Pid}, #state{} = State) when is_pid(Pid) ->
    State1 = do_leave(GroupID, Pid, State),
    State1;

run_cmd({leave_all, Pid}, #state{} = State) when is_pid(Pid) ->
    State1 = do_leave_all(Pid, State),
    State1;

run_cmd({leave_many, GroupIDs, Pid}, #state{} = State) when is_pid(Pid) ->
    F = fun(GroupID, S) -> do_leave(GroupID, Pid, S) end,
    State1 = lists:foldl(F, State, GroupIDs),
    State1;

run_cmd({dismiss, GroupID}, #state{groups = GIDs} = State) ->
    send_cache(GroupID, State), % 解散前先把残留消息发出
    Members = members(GroupID),
    [del_member_groups(M, GroupID) || M <- Members],
    set_members(GroupID, []),
    GIDs1 = lists:delete(GroupID, GIDs),
    State#state{groups = GIDs1};

run_cmd(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    State.


get_cmd_groups_ids({join, GroupID, Pid}) ->
    {{join, Pid}, [GroupID]};

get_cmd_groups_ids({join_many, GroupIDs, Pid}) ->
    {{join, Pid}, GroupIDs};

get_cmd_groups_ids({leave, GroupID, Pid}) ->
    {{leave, Pid}, [GroupID]};

get_cmd_groups_ids({leave_all, Pid}) ->
    case erlang:get({groups, Pid}) of
        ?undefined ->
            {leave, []};
        {_Ref, Groups} ->
            {{leave, Pid}, Groups}
    end;
get_cmd_groups_ids({leave_many, GroupIDs, Pid}) ->
    {{leave, Pid}, GroupIDs};

get_cmd_groups_ids({dismiss, _GroupID}) ->
    {dismiss, []}.


do_leave_all(Pid, #state{count = Count} = State) ->
    case erlang:erase({groups, Pid}) of
        ?undefined ->
            State;
        {Ref, Groups} ->
            erlang:demonitor(Ref),
            [del_member(G, Pid) || G <- Groups],
            State1 = State#state{count = Count - 1},
            State2 = check_empty_groups(State1),
            State2
    end.

do_join(GroupID, Pid, #state{groups = GIDs, count = Count} = State) ->
    Count1 =
        case erlang:get({groups, Pid}) of
            ?undefined -> % 第一次加入此类群组
                Ref = erlang:monitor(process, Pid),
                set_member_groups(Pid, Ref, [GroupID]),
                Count + 1;
            {Ref, Groups} ->
                Groups1 = store(GroupID, Groups),
                set_member_groups(Pid, Ref, Groups1),
                Count
        end,
    GIDs1 =
        case add_member(GroupID, Pid) of
            ?undefined -> % 新群组
                store(GroupID, GIDs);
            _ ->
                GIDs
        end,
    State#state{groups = GIDs1, count = Count1}.


do_leave(GroupID, Pid, #state{groups = GIDs} = State) ->
    del_member_groups(Pid, GroupID),
    GIDs1 =
        case del_member(GroupID, Pid) of
            empty ->
                lists:delete(GroupID, GIDs);
            _ ->
                GIDs
        end,
    State#state{groups = GIDs1}.


cache_bcast_cmd(Cmd, #state{strict = ?true} = State) ->
    {Cmd1, GroupIDs} = get_cmd_groups_ids(Cmd),
    F = fun(GID, #state{tmp_groups = TmpGroups} = S) ->
            case erlang:get({bcast_cache, GID}) of
                ?undefined ->
                    erlang:put({bcast_cache, GID}, [Cmd1]),
                    S#state{tmp_groups = [GID | TmpGroups]};
                Msgs ->
                    erlang:put({bcast_cache, GID}, [Cmd1 | Msgs]),
                    S
            end
        end,
    lists:foldl(F, State, GroupIDs);
cache_bcast_cmd(_Cmd, State) ->
    State.


cache_bcast([GroupID | T], Msg, #state{tmp_groups = TmpGroups} = State) ->
    case members(GroupID) =/= [] orelse lists:member(GroupID, TmpGroups) of
        ?true ->
            case erlang:get({bcast_cache, GroupID}) of
                ?undefined ->
                    erlang:put({bcast_cache, GroupID}, [Msg]);
                Msgs ->
                    erlang:put({bcast_cache, GroupID}, [Msg | Msgs])
            end;
        _ ->
            pass
    end,
    cache_bcast(T, Msg, State);
cache_bcast([], _Msg, State) ->
    State.


send_cache(GroupID, #state{strict = ?true}) ->
    case erlang:erase({bcast_cache, GroupID}) of
        ?undefined ->
            ok;
        Msgs ->
            Members = members(GroupID),
            send_cache_strict(Msgs, Members, [])
    end;
send_cache(GroupID, _) ->
    case erlang:erase({bcast_cache, GroupID}) of
        ?undefined ->
            ok;
        Msgs ->
            Msgs1 = lists:reverse(Msgs),
            Members = members(GroupID),
            bcast_msgs(fun net_api:send/2, Msgs1, Members)
    end.

%% 包含其他一些cast的信息需要先处理
send_cache_strict([{join, Pid} | T], Members, MsgAcc) ->
    Members1 = [Pid | lists:delete(Pid, Members)],
    bcast_msgs(fun net_api:send/2, MsgAcc, Members1),
    send_cache_strict(T, Members1, []);

send_cache_strict([{leave, Pid} | T], Members, MsgAcc) ->
    Members1 = lists:delete(Pid, Members),
    bcast_msgs(fun net_api:send/2, MsgAcc, Members1),
    send_cache_strict(T, Members1, []);

send_cache_strict([Msg | T], Members, MsgAcc) ->
    send_cache_strict(T, Members, [Msg | MsgAcc]);

send_cache_strict([], Members, MsgAcc) ->
    bcast_msgs(fun net_api:send/2, MsgAcc, Members).


bcast_msgs(Func, Msgs, Members) ->
    case iolist_to_binary(Msgs) of
        <<>> ->
            ok;
        Msgs1 ->
            [Func(M, Msgs1) || M <- Members],
            ok
    end.


set_member_groups(Member, Ref, Groups) ->
    erlang:put({groups, Member}, {Ref, Groups}).


%% 成员所属群组去除某群
del_member_groups(Member, GroupID) ->
    case erlang:get({groups, Member}) of
        ?undefined -> % 此人已离线
            ok;
        {Ref, Groups} ->
            set_member_groups(Member, Ref, lists:delete(GroupID, Groups))
    end.

%% 获取群组中的成员
members(GroupID) ->
    case erlang:get({members, GroupID}) of
        ?undefined -> [];
        Val -> Val
    end.

set_members(GroupID, []) ->
    erlang:erase({members, GroupID}),
    erlang:erase({bcast_cache, GroupID}), % 清除空群组的发送缓存!
    empty;
set_members(GroupID, Members) ->
    erlang:put({members, GroupID}, Members).

% 如群组未创建, 返undefined
add_member(GroupID, Member) ->
    Members = members(GroupID),
    Members1 = store(Member, Members),
    set_members(GroupID, Members1).

% 如群组被清空, 返empty
del_member(GroupID, Member) ->
    Members = members(GroupID),
    Members1 = lists:delete(Member, Members),
    set_members(GroupID, Members1).

store(Elem, List) ->
    case lists:member(Elem, List) of
        ?true -> List;
        ?false -> [Elem | List]
    end.


check_empty_groups(#state{clear_time = CTime} = State) ->
    Now = util_time:unixtime(),
    case Now - CTime > ?CLEAR_EMPTY_GROUP_INTERVAL of
        ?true ->
            State#state{to_be_clear = ?true};
        _ ->
            State
    end.


clear_empty_groups(#state{groups = Groups, to_be_clear = ?true} = State) ->
    Now = util_time:unixtime(),
    Groups1 = clear_empty_groups_1(Groups, []),
    State#state{groups = Groups1, clear_time = Now, to_be_clear = ?false};
clear_empty_groups(State) ->
    State.


clear_empty_groups_1([GID | Tail], Acc) ->
    Acc1 =
        case members(GID) of
            [] -> Acc;
            _ -> [GID | Acc]
        end,
    clear_empty_groups_1(Tail, Acc1);
clear_empty_groups_1([], Acc) ->
    Acc.
