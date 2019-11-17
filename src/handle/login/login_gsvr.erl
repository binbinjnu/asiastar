%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     登录注册进程
%%% @end
%%% Created : 16. 11月 2019 19:29
%%%-------------------------------------------------------------------
-module(login_gsvr).
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
    get_user_master/1
]).

%% 获取玩家信息
get_user_master(BaseInfo) ->
    gen_server:call(?MODULE, {get_user_master, BaseInfo}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, []}.

handle_call({get_user_master, #user_master{account_name = Account} = BaseInfo}, _From, State) ->
    %% 确保创建角色不会重复
    %% TODO 加上各种ets缓存(暂时不用缓存)
    Res =
        case db_mysql_api:select_user_master(Account) of
            {ok, UserMaster} ->
                {ok, UserMaster};
            ?NULL_VAL ->    %% 玩家不存在
                UserID = index_gsvr:get_user_id(),
                UserMaster =
                    BaseInfo#user_master{
                        user_id = UserID,
                        account_nickname = Account
                    },
                try
                    db_mysql_api:insert_user_master(UserMaster),
                    %% TODO  注册日志
                    UserMaster
                catch
                    Err:Reason ->
                        ?ERROR("insert user master fail, Err:~w, Reason:~w", [Err, Reason]),
                        ?false
                end
        end,
    {reply, Res, State};

handle_call(_Msg, _From, State) ->
    ?WARNING("unhandle call ~w", [_Msg]),
    {reply, error, State}.

handle_cast(_Msg, State) ->
    ?WARNING("unhandle cast ~p", [_Msg]),
    {noreply, State}.

handle_info(_Msg, State) ->
    ?WARNING("unhandle info ~w", [_Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

