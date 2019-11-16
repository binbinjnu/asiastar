%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     配置信息
%%% @end
%%% Created : 30. 10月 2019 21:00
%%%-------------------------------------------------------------------
-module(config).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").

%% API
-export([
    start/0,
    get/1,
    get/2,
    set/2
]).

-export([
    get_app/0,
    is_dev/0
]).

-define(CONFIG_MODULE, config_values).

start() ->
    {ok, App} = application:get_application(),
    config_maker:init(?CONFIG_MODULE, App).

config_tag() ->
    ?CONFIG_MODULE.

get(Par, Default) ->
    try (config_tag()):Par()
    catch _:_ -> Default
    end.

get(Par) ->
    try (config_tag()):Par()
    catch
        _:_ ->
            ?ERROR("missing env: ~p", [Par]),
            timer:sleep(100),
            erlang:error({miss_env, Par})
    end.

get_app() ->
    ?MODULE:get(app).

%% 慎用! 一般仅限于控制台应急使用
set(Key, Val) ->
    App = get_app(),
    config_maker:set(?CONFIG_MODULE, App, Key, Val).


is_dev() ->
    config:get(dev_mode, ?false).
