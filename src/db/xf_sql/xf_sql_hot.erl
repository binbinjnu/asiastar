%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     创建xf_sql_svr进程的私有ets, 暂时没啥意义, 可以看数据热度
%%%     需要用到fun2ms
%%% @end
%%% Created : 26. 11月 2019 15:19
%%%-------------------------------------------------------------------
-module(xf_sql_hot).
-author("Administrator").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([
    start/1,
    mark_hots/2,
    raw_mark_hots/2,
    del_cools/3,
    clear_all/1
]).

start(Table) ->
    HotMarker = util_code:reg_name([?MODULE, "_", Table]),
    ets_svr:new(HotMarker, [private, {keypos, 1}]).

%% 标记为热数据
mark_hots(Table, Keys) when is_list(Keys)->
    Server = xf_sql_svr:server_name(Table),
    gen_server:cast(Server, {hot, Keys}).

%% 将keys设置成热数据
raw_mark_hots(HotMarker, Keys) ->
    Now = util_time:unixtime(),
    ets:insert(HotMarker, [{Key, Now} || Key <- Keys]).

%% 清理访问时间早于CoolTime的记录
del_cools(Table, HotMarker, CoolTime) ->
    MS = ets:fun2ms(fun({ID, Time}) when Time < CoolTime -> ID end),
    CoolIDs = ets:select(HotMarker, MS),
    [ets:delete(Table, ID) || ID <- CoolIDs],

    MS2 = ets:fun2ms(fun({_, Time}) when Time < CoolTime -> true end),
    ets:select_delete(HotMarker, MS2).

%% 清除所有标记
clear_all(HotMarker) ->
    ets:delete_all_objects(HotMarker).