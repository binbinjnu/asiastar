%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     自增id todo 定时同步数据库
%%% @end
%%% Created : 16. 11月 2019 13:04
%%%-------------------------------------------------------------------
-module(index_svr).
-author("Administrator").
-behaviour(gen_server).

-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_db.hrl").

%% API
-export([
    start_link/0
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([
    get_user_id/0
]).

-export([
    get_id/1,
    get_id/2,
    get_ids/2
]).

-define(ETS_TAB, index).

%% 获取玩家id
get_user_id() ->
    get_id(user_id).

get_id(Key) ->
    ets:update_counter(?ETS_TAB, Key, 1).

get_id(Key, Init) ->
    ets:update_counter(?ETS_TAB, Key, 1, {Key, Init}).

%% 需要使用连续的N个ID, 返回这段ID中的第一个
get_ids(Key, N) ->
    ets:update_counter(?ETS_TAB, Key, N) - N + 1.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% 初始化不成功的话, 外面调用启动进程的会报错, 整个服开不起来
init(_) ->
    ets_svr:new(?ETS_TAB, [named_table, public, {keypos, 1}, {write_concurrency, true}]),
%%    db_mysql_api:create_table_index(),  %% 如果表不存在, 则新建表
    KVList = db_mysql_api:select_index(),
    ets:insert(?ETS_TAB, KVList),
    erlang:process_flag(trap_exit, ?true),
    init_index(),
    {ok, []}.

handle_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.

handle_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.

handle_info(stop, State) ->
    {stop, normal, State};

handle_info(doloop, State) ->
    {noreply, State};

handle_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


init_index() ->
    ok = init_user_id(),
    ok = init_index(mail_id, 1000),
    ok.

%% 初始化user_id
init_user_id() ->
    case db_mysql_api:select_user_master_max_user_id() of
        {ok, MaxExistUserID} ->
            MaxExistUserID1 = ?IF(erlang:is_integer(MaxExistUserID), MaxExistUserID, 0),
            LimitUserID = max(MaxExistUserID1, 65536),
            case ets:lookup(?ETS_TAB, user_id) of
                [{_, Value}] when Value >= LimitUserID ->
                    ok;
                _ ->
                    ?NOTICE("Index ~p is inited with number ~w", [user_id, LimitUserID]),
                    ets:insert(?ETS_TAB, {user_id, LimitUserID})
            end,
            ok;
        _ ->
            ?WARNING("can not get user master max user id!"),
            ?false
    end.

init_index(Key, Init) when is_integer(Init)->
    case ets:lookup(?ETS_TAB, Key)  of
        [] ->
            ?INFO("Index ~p is inited with number ~w", [Key, Init]),
            ets:insert(?ETS_TAB, {Key, Init}),
            Init;
        [{_, Val, _}] ->
            ?INFO("Index ~p is inited with number ~w", [Key, Val]),
            Val
    end,
    ok.


