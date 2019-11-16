%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     广播组进程的管理进程, 主要用于新建和分配
%%% @end
%%% Created : 04. 11月 2019 19:03
%%%-------------------------------------------------------------------
-module(group_manager_gsvr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_logs.hrl").
-include("hrl_common.hrl").

%% API
-export([new/2, assign/3, group_pid/1]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {groups = #{}}).
-define(ETS_TAB, group).

%% 群组的进程pid
group_pid(GroupType) ->
    case ets:lookup(?ETS_TAB, GroupType) of
        [{_, GPid}] ->
            GPid;
        _ ->
            erlang:error({undefined_group_type, GroupType})
    end.

%% 新建广播组
new(GroupType, Opts) ->
    % ?INFO("Create new group ~p", [GroupType]),
    gen_server:call(?MODULE, {new, GroupType, Opts}).


%% 分配广播组
assign(GroupType, Opts, Pid) ->
    case proplists:get_bool(noname, Opts) of
        ?false -> gen_server:call(?MODULE, {assign, GroupType, Pid});
        ?true -> ?true
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    ets_gsvr:new(?ETS_TAB, [named_table, protected, {keypos, 1}, {read_concurrency, true}]),
    {ok, #state{}}.



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


%% 分配group
do_call({assign, GroupType, Pid}, _From, State) ->
    Ret = register_group(GroupType, Pid),
    {reply, Ret, State};

do_call({new, GroupType, Opts}, _From, #state{groups = Groups} = State) ->
    case proplists:get_bool(noname, Opts) of
        ?true -> % 无名广播组不注册, 不monitor, 不可重复
            {ok, Pid} = group_sup:start_child([GroupType, Opts]),
            Groups1 = maps:put(Pid, {noname, GroupType}, Groups),
            {reply, {ok, Pid}, State#state{groups = Groups1}};
        ?false ->
            case ets:lookup(?ETS_TAB, GroupType) of
                [] ->
                    {ok, Pid} = group_sup:start_child([GroupType, Opts]),
                    Groups1 = maps:put(Pid, {name, GroupType}, Groups),
                    register_group(GroupType, Pid),
                    {reply, {ok, Pid}, State#state{groups = Groups1}};
                [{_, Pid}] -> % 有名广播组不重复
                    {reply, {ok, Pid}, State}
            end
    end;

do_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.


do_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.


do_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, #state{groups = Groups} = State) ->
    case maps:get(Pid, Groups, ?undefined) of
        {name, GroupType} ->
            ets:delete(?ETS_TAB, GroupType);
        _ ->
            ok
    end,
    Groups1 = maps:remove(Pid, Groups),
    {noreply, State#state{groups = Groups1}};

%% 处理ets:give_away产生的消息
do_info({'ETS-TRANSFER', _Tab, _Pid, _Name}, State) ->
    {noreply, State};

do_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.


register_group(GroupType, Pid) ->
    erlang:monitor(process, Pid),
    ets:insert(?ETS_TAB, {GroupType, Pid}).
