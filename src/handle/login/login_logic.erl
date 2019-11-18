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
-export([login/2]).

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
        #user_master{
            account = Account,
            password = Password,
            channel = Channel
        },
    case login_gsvr:get_user_master(BaseInfo) of
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
            State#handler_state{pid = Pid};
        {?false, err_password} ->   %% 密码错误
            Resp = #s2c_login{iCode = 1},
            lib_send:send(Resp),
            State;
        _ ->
            self() ! stop,
            State
    end.


%% 进入游戏
enter(UserMaster) ->
    #user_master{user_id = UserID} = UserMaster,
    %% 判断进程是否存在
    case player_api:ensure_player(UserID) of
        {ok, ConnType, Pid} ->
            erlang:link(Pid),
            gen_server:cast(Pid, {connect, ConnType, self(), UserMaster}),
            Pid;
        _ ->
            self() ! stop,
            ?false
    end.

