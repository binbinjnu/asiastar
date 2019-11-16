%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     广播组api
%%% @end
%%% Created : 04. 11月 2019 18:38
%%%-------------------------------------------------------------------
-module(group_api).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

%% API
-export([group_name/1]).

-export([
    new/1,
    new/2,
    close/1,
    join/3,
    leave/3,
    leave_all/2,
    dismiss/2,
    bcast/3,
    mbcast/3,
    batch_mbcast/2,
    send/3,
    cast/3,
    mcast/3,
    bcast_to_other/4,
    send_to_other/4,
    cast_to_other/4,

    count/1,
    count/2,
    members/2,
    groups/1,
    group_svr/1
]).

group_name(GroupType) ->
    list_to_atom(lists:flatten(io_lib:format("group_~p", [GroupType]))).


%% 新建广播组
-spec new(GroupType) -> {ok, pid()} when
    GroupType :: pid() | term().
new(GroupType) ->
    new(GroupType, []).

%% 新建广播组
-spec new(GroupType, Opts) -> {ok, pid()} when
    Opts :: [term()],
    GroupType :: pid() | term().
new(GroupType, Opts) ->
    group_manager_gsvr:new(GroupType, Opts).


-spec close(GroupType) -> ok when
    GroupType :: pid() | term().
close(GPid) when is_pid(GPid)->
    gen_server:cast(GPid, close);
close(GroupType) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, close).


-spec dismiss(GroupType, GroupID) -> ok when
    GroupType :: pid() | term(),
    GroupID :: term().
dismiss(GPid, GroupID) when is_pid(GPid) ->
    gen_server:cast(GPid, {dismiss, GroupID});
dismiss(GroupType, GroupID) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {dismiss, GroupID}).


-spec join(Pid, GroupType, GroupID) -> ok when
    Pid :: pid(),
    GroupType :: pid() | term(),
    GroupID :: term().
join(_Pid, _GPid, ?undefined) ->
    ?WARNING("GroupID cannot be 'undefined'", []),
    ok;
join(Pid, GPid, GroupID) when is_pid(Pid) andalso is_pid(GPid)->
    gen_server:cast(GPid, {join, GroupID, Pid});
join(Pid, GroupType, GroupID) when is_pid(Pid)->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {join, GroupID, Pid}).


-spec leave(Pid, GroupType, GroupID) -> ok when
    Pid :: pid(),
    GroupType :: pid() | term(),
    GroupID :: term().
leave(Pid, GPid, GroupID) when is_pid(Pid) andalso is_pid(GPid)->
    gen_server:cast(GPid, {leave, GroupID, Pid});
leave(Pid, GroupType, GroupID) when is_pid(Pid)->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {leave, GroupID, Pid}).


-spec leave_all(Pid, GroupType) -> ok when
    Pid :: pid(),
    GroupType :: pid() | term().
leave_all(Pid, GPid) when is_pid(Pid) andalso is_pid(GPid)->
    gen_server:cast(GPid, {leave_all, Pid});
leave_all(Pid, GroupType) when is_pid(Pid)->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {leave_all, Pid}).


%% 使用 net_api:send/2 广播
-spec bcast(Msg, GroupType, GroupID) -> ok when
    Msg :: iolist(),
    GroupType :: pid() | term(),
    GroupID :: term().
bcast(Msg, GPid, GroupID) when is_pid(GPid)->
    gen_server:cast(GPid, {bcast_to_all, [GroupID], Msg});
bcast(Msg, GroupType, GroupID) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {bcast_to_all, [GroupID], Msg}).


%% 对多个广播组使用 fg_net:send/2 广播
-spec mbcast(Msg, GroupType, GroupIDs) -> ok when
    Msg :: iolist(),
    GroupType :: pid() | term(),
    GroupIDs :: [term()].
mbcast(Msg, GPid, GroupIDs) when is_pid(GPid) andalso is_list(GroupIDs) ->
    gen_server:cast(GPid, {bcast_to_all, GroupIDs, Msg});
mbcast(Msg, GroupType, GroupIDs) when is_list(GroupIDs) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {bcast_to_all, GroupIDs, Msg}).


%% 使用 fg_net:send/2 广播 {GroupID, Msg} 的列表
-spec batch_mbcast(GroupMsgs, GroupType) -> ok when
    GroupType :: pid() | term(),
    GroupMsgs :: [{term(), iolist()}].
batch_mbcast(GroupMsgs, GPid) when is_pid(GPid) andalso is_list(GroupMsgs) ->
    gen_server:cast(GPid, {batch_bcast_to_all, GroupMsgs});
batch_mbcast(GroupMsgs, GroupType) when is_list(GroupMsgs) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {batch_bcast_to_all, GroupMsgs}).


-spec bcast_to_other(Msg, GroupType, GroupID, MyPid) -> ok when
    Msg :: iolist(),
    GroupType :: pid() | term(),
    GroupID :: term(),
    MyPid :: pid().
bcast_to_other(Msg, GPid, GroupID, MyPid) when is_pid(MyPid) andalso is_pid(GPid)->
    gen_server:cast(GPid, {to_other, {fg_net, send}, GroupID, Msg, MyPid});
bcast_to_other(Msg, GroupType, GroupID, MyPid) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {to_other, {fg_net, send}, GroupID, Msg, MyPid}).

%% 使用 Pid ! Msg 广播
-spec send(Msg, GroupType, GroupID) -> ok when
    Msg :: term(),
    GroupType :: pid() | term(),
    GroupID :: term().
send(Msg, GPid, GroupID) when is_pid(GPid)->
    gen_server:cast(GPid, {to_all, {erlang, send}, GroupID, Msg});
send(Msg, GroupType, GroupID) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {to_all, {erlang, send}, GroupID, Msg}).

-spec send_to_other(Msg, GroupType, GroupID, MyPid) -> ok when
    Msg :: iolist(),
    GroupType :: pid() | term(),
    GroupID :: term(),
    MyPid :: pid().
send_to_other(Msg, GPid, GroupID, MyPid) when is_pid(GPid)->
    gen_server:cast(GPid, {to_other, {erlang, send}, GroupID, Msg, MyPid});
send_to_other(Msg, GroupType, GroupID, MyPid) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {to_other, {erlang, send}, GroupID, Msg, MyPid}).

%% 使用 gen_server:cast/2 广播
-spec cast(Msg, GroupType, GroupID) -> ok when
    Msg :: term(),
    GroupType :: pid() | term(),
    GroupID :: term().
cast(Msg, GPid, GroupID) when is_pid(GPid)->
    gen_server:cast(GPid, {to_all, {gen_server, cast}, GroupID, Msg});
cast(Msg, GroupType, GroupID) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {to_all, {gen_server, cast}, GroupID, Msg}).

-spec mcast(Msg, GroupType, GroupIDs) -> ok when
    Msg :: term(),
    GroupType :: pid() | term(),
    GroupIDs :: term().
mcast(Msg, GPid, GroupIDs) when is_pid(GPid)->
    gen_server:cast(GPid, {to_mall, {gen_server, cast}, GroupIDs, Msg});
mcast(Msg, GroupType, GroupIDs) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {to_mall, {gen_server, cast}, GroupIDs, Msg}).

-spec cast_to_other(Msg, GroupType, GroupID, MyPid) -> ok when
    Msg :: iolist(),
    GroupType :: pid() | term(),
    GroupID :: term(),
    MyPid :: pid().
cast_to_other(Msg, GPid, GroupID, MyPid) when is_pid(GPid)->
    gen_server:cast(GPid, {to_other, {gen_server, cast}, GroupID, Msg, MyPid});
cast_to_other(Msg, GroupType, GroupID, MyPid) ->
    GPid = group_svr(GroupType),
    gen_server:cast(GPid, {to_other, {gen_server, cast}, GroupID, Msg, MyPid}).


count(GPid) when is_pid(GPid) ->
    gen_server:call(GPid, count);
count(GroupType) ->
    GPid = group_svr(GroupType),
    gen_server:call(GPid, count).

count(GPid, GroupID) when is_pid(GPid) ->
    gen_server:call(GPid, {count, GroupID});
count(GroupType, GroupID) ->
    GPid = group_svr(GroupType),
    gen_server:call(GPid, {count, GroupID}).

members(GPid, GroupID) when is_pid(GPid) ->
    gen_server:call(GPid, {members, GroupID});
members(GroupType, GroupID) ->
    GPid = group_svr(GroupType),
    gen_server:call(GPid, {members, GroupID}).

groups(GPid) when is_pid(GPid) ->
    gen_server:call(GPid, groups);
groups(GroupType) ->
    GPid = group_svr(GroupType),
    gen_server:call(GPid, groups).


group_svr(GroupType) ->
    group_manager_gsvr:group_pid(GroupType).