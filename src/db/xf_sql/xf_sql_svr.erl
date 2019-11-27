%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     数据管理进程, 定时插入/删除
%%%     如果是周期性的, 对应周期进行flush
%%%     否则将数据放入到inserts和deletes中, 等doloop的时候统一处理
%%% @end
%%% Created : 26. 11月 2019 9:33
%%%-------------------------------------------------------------------
-module(xf_sql_svr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

%% API
-export([
    server_name/1,
    start/2,
    start_link/2
]).

-export([
    wait_ready/1,
    flush/1,
    all_tables/0
]).

-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    table          = ?undefined,    %% 表名
    loop           = 0,             %%
    save_func      = ?undefined,    %% 存储函数
    inserts        = ?undefined,    %% 需要插入的数据  gb_sets
    deletes        = ?undefined,    %% 需要删除的数据  gb_sets
    period         = ?undefined,    %% flush周期, 可以照着周期划分对应的槽位, 每次loop检测对应的rem值的槽位
    period_insert  = ?undefined,    %% gb_trees, 里面放有对应period个的gb_sets

    %% 这个对于存储来说没用处
    hot_marker     = ?undefined,    %%
    cool_down      = ?undefined,    %% 缓存冷却时间(冷却后进行insert\delete)
    cool_down_check = 0             %% 不同的表分散开来check, 在1-cool_down建随机一个check点
}).

-define(INSERT_INTERVAL, 300).  %% 存储间隔 单位 s
-define(CACHE_COOL_DOWN, 3600). %% 缓存冷却时间 单位 s
-define(COOL_DOWN_CHECK_LOOP, 400). %% 每N个循环检查一次缓存冷却

%% 等待开启成功
wait_ready(Table) ->
    Name = server_name(Table),
    gen_server:call(Name, ready, infinity).


%% 强制写盘, 成功返true
flush(Table) ->
    Name = server_name(Table),
    gen_server:call(Name, flush, infinity).


%% 获取所有表名
all_tables() ->
    Children = supervisor:which_children(xf_sql_sup),
    Tables = [Table || {{?MODULE, [Table, _Opts]}, _Pid, _Type, _Module} <- Children],
    Tables.


%% 进程名字
server_name(Table) ->
    util_code:reg_name([?MODULE, "_", Table]).


start(Table, Opts) ->
    Name = server_name(Table),
    case erlang:whereis(Name) of
        ?undefined ->
            xf_sql_sup:start_child(?MODULE, [Table, Opts]);
        Pid ->
            ?WARNING("Table ~p already opened", [Table]),
            {ok, Pid}
    end.


start_link(Table, Opts) ->
    Name = server_name(Table),
    SOpts = [{fullsweep_after, 10}],    %% 分代gc, 最多经过多少代就可以强制进行充分垃圾回收
    gen_server:start_link({local, Name}, ?MODULE, [Table, Opts], [{spawn_opt,SOpts}]).


init([Table, Opts]) ->
    erlang:process_flag(trap_exit, ?true),
    {save_func, {M, F, A}} = lists:keyfind(save_func, 1, Opts),
    timer_svr:reg(self(), ?INSERT_INTERVAL),
    State =
        #state{
            table = Table,
            save_func = {M, F, A},
            inserts = gb_sets:new(),
            deletes = gb_sets:new()
        },
    State1 = init_cool_down(Opts, State),
    State2 = init_period(Opts, State1),
    {ok, State2}.


handle_call(ready, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call(Req, From, State) ->
    try
        do_call(Req, From, State)
    catch
        Err:Reason ->
            ?ERROR("~p:~p", [Err, Reason]),
            {reply, error, State}
    end.

handle_cast(Msg, State) ->
    Result =
        try
            do_cast(Msg, State)
        catch
            Err:Reason->
                ?ERROR("~w,~w",[Err,Reason]),
                {noreply, State}
        end,
    Result.

handle_info(Info, State) ->
    Result =
        try
            do_info(Info,State)
        catch
            Err:Reason->
                ?ERROR("~w,~w",[Err,Reason]),
                {noreply, State}
        end,
    Result.

terminate(_Reason, State) ->
    case do_flush(State) of
        #state{inserts=Inserts} ->
            case gb_sets:is_empty(Inserts) of
                ?true ->
                    ok;
                _ ->
                    ?WARNING("~p items not flushed on terminate !", [gb_sets:size(Inserts)])
            end
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
do_call(flush, _From, State) ->
    State1 = flush_all_period(State),
    State2 = #state{inserts=Inserts} = do_flush(State1),
    Reply = gb_sets:is_empty(Inserts),
    {reply, Reply, State2};

do_call(_Request, _From, State) ->
    ?WARNING("unhandle call ~p.",[_Request]),
    Reply = unexpect_call,
    {reply, Reply, State}.


do_cast({hot, Keys}, #state{} = State) ->
    mark_hot(Keys, State),
    {noreply, State};

%% 非周期性处理的数据, 直接放到inserts中, 并将deletes中的删除, 等到loop的时候进行flush
do_cast({insert, Data}, #state{period = ?undefined, inserts = Buffer, deletes = Delete} = State)
        when is_list(Data)->
    mark_hot(Data, State),
    Buffer1 = sets_add(Data, Buffer),
    Delete1 = sets_del(Data, Delete),
    {noreply, State#state{inserts = Buffer1, deletes = Delete1}};

%% 周期性处理的, 放到period_insert
do_cast({insert, Data}, #state{} = State) when is_list(Data) ->
    mark_hot(Data, State),
    State1 = period_insert(Data, State),
    {noreply, State1};

%% 将所有数据插入
do_cast(insert_all, #state{table = Table, period = Period} = State)->
    KeysList = all_keys(Table),
    KeysSet = gb_sets:from_list(KeysList),
    NewPI = new_period_insert(Period),  %% 将period_insert重新初始化
    State1 = State#state{inserts = KeysSet, period_insert = NewPI},
    {noreply, State1};

%% 非周期性处理的数据, 直接放到deletes中, 并将inserts中的删除, 等到loop的时候进行flush
do_cast({delete, Key}, #state{period = ?undefined, inserts = Buffer, deletes = Delete} = State) ->
    Buffer1 = sets_del([Key], Buffer),
    Delete1 = sets_add([Key], Delete),
    {noreply, State#state{inserts = Buffer1, deletes = Delete1}};
do_cast({delete_many, Keys}, #state{period = ?undefined, inserts = Buffer, deletes = Delete} = State) ->
    Buffer1 = sets_del(Keys, Buffer),
    Delete1 = sets_add(Keys, Delete),
    {noreply, State#state{inserts = Buffer1, deletes = Delete1}};

%% 周期性处理的数据, 直接放到deletes中, 并将period_delete中的对应数据删除, 等到loop的时候进行flush
do_cast({delete, Key}, #state{deletes = Delete} = State) ->
    Delete1 = sets_add([Key], Delete),
    State1 = period_delete(Key, State),
    {noreply, State1#state{deletes=Delete1}};
do_cast({delete_many, Keys}, #state{deletes = Delete} = State) ->
    Delete1 = sets_add(Keys, Delete),
    State1 = lists:foldl(fun period_delete/2, State, Keys),
    {noreply, State1#state{deletes = Delete1}};

%% 删除表中所有数据
do_cast(delete_all_objects, #state{period=Period, table = _Table}=State) ->
    clear_hot_marker(State),
    %% todo fg_sql_inf:truncate(Table),     %% 通过truncate来操作(truncate会锁表!!!!! 操作频繁可能引发问题)
    NewPI = new_period_insert(Period),      %% 将period_insert重新初始化
    State1 = State#state{inserts = gb_sets:new(), period_insert = NewPI},
    {noreply, State1};

do_cast(Msg, #state{}=State) ->
    ?WARNING("Msg:~w is not match.",[Msg]),
    {noreply, State}.


do_info(doloop, #state{loop = Loop} = State)->
    State1 = State#state{loop = Loop + 1},
    State2 = loop_period(State1),
    Now = util_time:unixtime(), %% 时间需要取do_loop_flush之前的
    State3 = do_loop_flush(State2),
    check_cool(Now, State3),
    {noreply, State3};

%% ets控制器转交
do_info({'ETS-TRANSFER',_,_,_}, State)->
    {noreply, State};

do_info(Info, State)->
    ?WARNING("Msg:~w is not match.",[Info]),
    {noreply, State}.

%%% -------------------------------------------------------------
%%%                     Local Function
%%% -------------------------------------------------------------

%% 初始化数据冷却相关信息
init_cool_down(Opts, State) ->
    case proplists:get_value(cool_down, Opts) of
        ?undefined ->
            State#state{hot_marker = ?undefined, cool_down = infinity};
        infinity ->
            State#state{hot_marker = ?undefined, cool_down = infinity};
        ?true ->
            init_cool_down_1(?CACHE_COOL_DOWN, State);
        CoolDown when is_integer(CoolDown) ->
            init_cool_down_1(CoolDown, State);
        Error ->
            erlang:error({invalid_cool_down, Error})
    end.

init_cool_down_1(CoolDown, #state{table = Table} = State) ->
    CheckPoint = util_math:rand(0, ?COOL_DOWN_CHECK_LOOP - 1),
    HotMarker = xf_sql_hot:start(Table),
    HotMarker = ?undefined,
    State#state{
        hot_marker = HotMarker,
        cool_down = CoolDown,
        cool_down_check = CheckPoint
    }.

%% 初始化数据操作周期相关信息
init_period(Opts, State) ->
    case proplists:get_value(period, Opts) of
        ?undefined ->
            State#state{period = ?undefined};
        Val when Val > 0 ->
            NewPI = new_period_insert(Val),
            State#state{period = Val, period_insert = NewPI}
    end.


%% 安装周期新建Period个gb_sets, 将所有gb_sets房到gb_trees中
%% 最后返回gb_trees
new_period_insert(?undefined) ->
    ?undefined;
new_period_insert(Period) ->
    F = fun(Slot, Tree) -> gb_trees:insert(Slot, gb_sets:new(), Tree) end,
    NewPI = lists:foldl(F, gb_trees:empty(), lists:seq(0, Period - 1)),
    NewPI.


%%
flush_all_period(#state{period = ?undefined} = State) ->
    State;
flush_all_period(#state{inserts = Inserts, period = Period, period_insert = PI} = State) ->
    IDSet = gb_sets:union(gb_trees:values(PI)),
    NewPI = new_period_insert(Period),
    State#state{inserts = gb_sets:union(IDSet, Inserts), period_insert = NewPI}.


%% 数据落地
%% 将inserts和deletes中的数据落地
do_flush(State) ->
    #state{
        save_func = {M, F, A},
        inserts   = Inserts,
        table     = _Table,
        deletes   = Deletes
    } = State,
    InsertList = gb_sets:to_list(Inserts),
    case M:F(A, InsertList) of
        ?true ->
            case gb_sets:to_list(Deletes) of
                [] -> ok;
                _Keys ->
                    %% todo fg_sql_inf:kv_delete_many(Table, Keys)
                    ok
            end,
            State#state{inserts = gb_sets:new(), deletes = gb_sets:new()};
        _Err ->
            ?WARNING("~p items in ~p flush failed: ~p",
                [length(InsertList), State#state.table, _Err]),
            mark_hot(InsertList, State), % 出错数据要保持热度, 正常数据不需要
            State#state{inserts = Inserts}
    end.

mark_hot(_, #state{hot_marker = ?undefined}) ->
    ok;
mark_hot(_, #state{cool_down = infinity}) ->
    ok;
mark_hot(Keys, #state{hot_marker = HotMarker}) ->
    xf_sql_hot:raw_mark_hots(HotMarker, Keys).

clear_hot_marker(#state{hot_marker = ?undefined}) ->
    ok;
clear_hot_marker(#state{cool_down = infinity}) ->
    ok;
clear_hot_marker(#state{hot_marker = _HotMarker}) ->
    %% todo fg_sql_hot:clear_all(HotMarker).
    ok.


%% 将安周期数据插入到不同槽位中
period_insert([H | T], #state{period = Period, period_insert = PI} = State) ->
    Slot = get_period_slot(H, Period),
    IDs = gb_trees:get(Slot, PI) ,
    PI1 = gb_trees:update(Slot, gb_sets:add(H, IDs), PI),
    period_insert(T, State#state{period_insert = PI1});
period_insert([], State) ->
    State.


%% 删除
period_delete(Key, #state{period = Period, period_insert = PI} = State) ->
    Slot = get_period_slot(Key, Period),
    IDs = gb_trees:get(Slot, PI),
    PI1 = gb_trees:update(Slot, gb_sets:delete_any(Key, IDs), PI),
    State#state{period_insert = PI1}.


get_period_slot(Key, Period) when is_integer(Key) ->
    erlang:abs(Key rem Period);
get_period_slot(Key, Period) ->
    erlang:phash2(Key, Period).


%% period_insert中的数据循环提取
loop_period(#state{period = ?undefined} = State) ->
    State;
loop_period(#state{loop = Loop, period = Period, inserts = Inserts, period_insert = PI} = State) ->
    %% 每个loop找对应槽位的数据, 将period_insert中对应的数据放入到inserts中, 等待flush处理
    Slot = Loop rem Period,
    IDSet = gb_trees:get(Slot, PI),
    PI1 = gb_trees:update(Slot, gb_sets:new(), PI),
    State#state{inserts = gb_sets:union(IDSet, Inserts), period_insert = PI1}.


%% 如果消息阻塞的话, 将所有doloop的消息receive出来, 最后处理do_flush
do_loop_flush(State) ->
    receive
        doloop ->
            do_loop_flush(State)
    after
        0 ->
            do_flush(State)
    end.


check_cool(_, #state{hot_marker = ?undefined}) ->
    ok;
check_cool(_, #state{cool_down = infinity}) ->
    ok;
check_cool(Now, State) ->
    #state{
        table = Table,
        loop = Loop,
        hot_marker = HotMarker,
        cool_down = CoolDown,
        cool_down_check = CheckPoint
    } = State,
    case Loop rem ?COOL_DOWN_CHECK_LOOP of
        CheckPoint ->   %% 定时删除冷数据
            CoolTime = Now - CoolDown,
            xf_sql_hot:del_cools(Table, HotMarker, CoolTime);
        _ ->
            ok
    end.


%% Table对应ets表中的所有key
all_keys(Table) ->
    ets:safe_fixtable(Table, true),
    Keys = all_keys(Table, ets:first(Table), []),
    ets:safe_fixtable(Table, false),
    Keys.

all_keys(_Table, '$end_of_table', Acc) ->
    Acc;
all_keys(Table, Key, Acc) ->
    all_keys(Table, ets:next(Table, Key), [Key | Acc]).

%% ----------------------- 一些gb_sets的操作 -----------------------
sets_add(IDs, Sets) ->
    lists:foldl(fun gb_sets:add_element/2, Sets, IDs).

sets_del(IDs, Sets) ->
    lists:foldl(fun gb_sets:del_element/2, Sets, IDs).
