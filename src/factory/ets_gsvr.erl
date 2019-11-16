%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     ets管理进程, 通过该进程启动ets, 确保ets不会因为启动进程崩溃而出现错误
%%% @end
%%% Created : 04. 11月 2019 18:45
%%%-------------------------------------------------------------------
-module(ets_gsvr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

-export([
    new/2,
    holder/0,
    hold_new/2
]).

-export([start/0, start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tabs = [], holder}).

%% 创建ets, 并托管给调用进程
new(Name, Args) ->
    Allows =
        [public,
            protected,
            private,
            set,
            ordered_set,
            bag,
            duplicate_bag,
            % named_table, % 这个要强制
            keypos,
            write_concurrency,
            read_concurrency,
            compressed],
    Args1 = util:filter_opts(Args, Allows),
    gen_server:call(?MODULE, {new, Name, [named_table | Args1]}).

%% 创建并托管ets表, 因为宿主是公共进程, 所以必须使用public, named_table表才有意义.
%% 因为是named_table, 返tid没有意义, 故成功返回ok, 表名已存在返error.
hold_new(Name, Args) ->
    Denies = [protected, private],
    util:deny_opts(Args, Denies),
    Allows =
        [set,
            ordered_set,
            bag,
            duplicate_bag,
            keypos,
            write_concurrency,
            read_concurrency,
            compressed],
    Args1 = util:filter_opts(Args, Allows),
    Args2 = [public, named_table | Args1],
    gen_server:call(?MODULE, {hold_new, Name, Args2}).


start() ->
    ?MAIN_SUP:start_child(?MODULE).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


holder() ->
    receive
        _ ->
            ?MODULE:holder()
    after
        100 ->
            ?MODULE:holder()
    end.


init([]) ->
    process_flag(trap_exit, true),
    Holder = proc_lib:spawn(?MODULE, holder, []),
    {ok, #state{tabs = [], holder = Holder}}.


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


do_call({hold_new, Name, Args}, _From, #state{holder = Holder} = State) ->
    Tab = ets:new(Name, [{heir, self(), Name} | Args]),
    ets:give_away(Tab, Holder, Name),
    {reply, ok, State};

do_call({new, Name, Args}, {FromPid, _Ref}, #state{tabs = Tabs} = State) ->
    Tab =
        case lists:keyfind(Name, 1, Tabs) of
            {_, T} ->
                T;
            _ ->
                ets:new(Name, [{heir, self(), Name} | Args])
        end,
    ets:give_away(Tab, FromPid, Name),
    Tabs1 = lists:keydelete(Name, 1, Tabs),
    {reply, Tab, State#state{tabs = Tabs1}};

do_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.


do_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.


%% give_away的目标进程如果挂了, 会返回给heir对应的进程
do_info({'ETS-TRANSFER', Tab, _Pid, Name}, #state{tabs = Tabs} = State) ->
    Tabs1 = [{Name, Tab} | Tabs],
    {noreply, State#state{tabs = Tabs1}};

do_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.


