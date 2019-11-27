%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     网络协议输出数量控制进程
%%% @end
%%% Created : 30. 10月 2019 20:53
%%%-------------------------------------------------------------------
-module(net_debug_svr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([start/0]).

-export([
    should_log/1,
    set_max_logs/1,
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TIMEOUT, 100).

-record(state, {pid_uids = #{}, uid_pids = #{}, max_logs = 0}).

start() ->
    ?MAIN_SUP:start_child(?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


should_log(UID) ->
    gen_server:call(?MODULE, {should_log, UID, self()}, ?TIMEOUT).

set_max_logs(N) ->
    gen_server:call(?MODULE, {set_max_logs, N}).

init([]) ->
    State = #state{max_logs = config:get(net_proto_debug_max, 0)},
    {ok, State}.

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

do_call({should_log, UID, Pid}, _From,
    #state{pid_uids = PidUIDs, uid_pids = UIDPids, max_logs = Max} = State) ->
    case maps:size(PidUIDs) < Max of
        ?true ->
            erlang:monitor(process, Pid),
            PidUIDs1 = PidUIDs#{Pid => UID},
            UIDPids1 = UIDPids#{UID => Pid},
            State1 = State#state{pid_uids = PidUIDs1, uid_pids = UIDPids1},
            {reply, ?true, State1};
        _ ->
            {reply, ?false, State}
    end;

do_call({set_max_logs, N}, _From, State) ->
    {reply, ok, State#state{max_logs = N}};

do_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.


do_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.


do_info({'DOWN', _MonitorRef, _Type, Pid, _Info},
    #state{pid_uids = PidUIDs, uid_pids = UIDPids} = State) ->
    case maps:take(Pid, PidUIDs) of
        {UID, PidUIDs1} ->
            UIDPids1 = maps:remove(UID, UIDPids),
            State1 = State#state{pid_uids = PidUIDs1, uid_pids = UIDPids1},
            {noreply, State1};
        _ ->
            {noreply, State}
    end;

do_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.
