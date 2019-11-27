%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     登录逻辑流程
%%% @end
%%% Created : 04. 11月 2019 16:15
%%%-------------------------------------------------------------------
-module(login_logic).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").
-include("hrl_db.hrl").
-include("hrl_net.hrl").
-include("login_pb.hrl").


%% API
-export([
    login/2,
    re_login/3
]).

%% 登录 (在玩家进程中取返回s2c_login)
login(#c2s_login{sAccount = Account} = Req, State) ->
    #c2s_login{
%%        iSiteID = SiteID,
%%        iTerminalType = TerminalType,
%%        iLoginType = LoginType,
        sAccount = Account,
        sPassword = Password,
%%        sMachine = Machine,
        sChannel = Channel
    } = Req,
    BaseInfo =
        #user_master_t{
            account = Account,
            password = Password,
            channel = Channel
        },
    case login_svr:get_user_master(BaseInfo) of
        {ok, OldOrNew, UserMaster} ->
            Pid = enter(UserMaster),
            Resp = #s2c_login{iCode = 0},
            lib_send:send(Resp),
            case OldOrNew of
                new ->  %% todo 新建角色, 写注册日志
                    ok;
                _ ->
                    skip
            end,
            %% todo 登录日志
            ?INFO("Player Pid:~w", [Pid]),
            State#handler_state{state = normal, pid = Pid};
        {?false, err_password} ->   %% 密码错误
            Resp = #s2c_login{iCode = 1},
            lib_send:send(Resp),
            State;
        _ ->
            net_api:stop(self()),
            State#handler_state{state = error}
    end.


%% 进入游戏
enter(UserMaster) ->
    #user_master_t{user_id = UserID} = UserMaster,
    %% 判断进程是否存在
    case player_api:ensure_player(UserID) of
        {ok, ConnType, Pid} ->
            erlang:link(Pid),
            gen_server:cast(Pid, {connect, ConnType, self(), UserMaster}),
            Pid;
        _ ->
            net_api:stop(self()),
            ?false
    end.


%%% token 重连
re_login(UserID, Token, State) ->
    case player_api:pid(UserID) of
        Pid when is_pid(Pid) ->
            %% 直接当成功, 如果在玩家进程中判断到有错误, 在玩家进程中断掉连接
            erlang:link(Pid),
            gen_server:cast(Pid, {re_login, self(), Token}),
            Resp = #s2c_re_login{iCode = 0},
            lib_send:send(Resp),
            State#handler_state{state = normal, pid = Pid};
        _ ->
            %% 关闭连接
            Resp = #s2c_re_login{iCode = 1},
            lib_send:send(Resp),
            net_api:stop(self()),
            State#handler_state{state = error}
    end.