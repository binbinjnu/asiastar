%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     登录注册进程
%%% @end
%%% Created : 16. 11月 2019 19:29
%%%-------------------------------------------------------------------
-module(login_svr).
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

%% 确保创建角色不会重复
handle_call({get_user_master, #user_master_t{} = BaseInfo}, _From, State) ->
    #user_master_t{
        account = Account,
        password = BasePassword
    } = BaseInfo,
    Md5Password = util_code:password(erlang:binary_to_list(BasePassword)),
    %% TODO 加上各种ets缓存(暂时不用缓存)
    Res =
        case db_mysql_api:select_user_master(Account) of
            {ok, #user_master_t{password = Password} = UserMaster} ->
                ?INFO("Password:~w, Md5Password:~w", [Password, Md5Password]),
                %% 判断登录密码
                case Md5Password =:= Password of
                    ?true ->
                        {ok, old, UserMaster};
                    _ ->
                        {?false, err_password}
                end;
            ?NULL_VAL ->    %% 玩家不存在
                UserID = index_svr:get_user_id(),
                UserMaster =
                    BaseInfo#user_master_t{
                        user_id = UserID,
                        nick_name = Account,
                        password = Md5Password
                    },
                try
                    db_mysql_api:insert_user_master(UserMaster),
                    {ok, new, UserMaster}
                catch
                    Err:Reason ->
                        ?ERROR("insert user master fail, Err:~w, Reason:~w", [Err, Reason]),
                        {?false, err_db}
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

