%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. 10月 2019 9:27
%%%-------------------------------------------------------------------
-module(config_maker).
-author("Administrator").

-include("hrl_common.hrl").
-include("hrl_logs.hrl").
%% API

-export([
    init/2,
    set/4
]).


init(Tag, App) ->
    try Tag:config_info(inited) of
        ?true ->
            ok
    catch
        error:undef ->
            Envs = application:get_all_env(App),
            gen(Tag, App, [{app, App} | Envs]),
            ok
    end.


gen(Tag, App, Envs) ->
    Binary = make(Tag, App, Envs),
    {module, Tag} = code:load_binary(Tag, atom_to_list(Tag), Binary).


make(Tag, App, Envs) ->
    TextH1 = io_lib:format("-module(~w).~n", [Tag]),
    TextH2 = io_lib:format("-compile([export_all]).~n", []),
    TextCauses = [make_cause(E) || E <- Envs],

    TextInfo1 = make_info_kv(inited, ?true),
    TextInfo2 = make_info_kv(all_env, Envs),
    TextInfo3 = make_info_kv(app_name, App),
    TextInfoEnd = make_info_end(),

    Text = lists:flatten([TextH1, TextH2, TextCauses,
        TextInfo1, TextInfo2, TextInfo3, TextInfoEnd]),
    {_Module, Binary} = dynamic_compile:from_string(Text),
    Binary.

make_cause({Key, Val}) ->
    io_lib:format("~w()->~w.~n", [Key, Val]).


make_info_kv(Key, Val) ->
    io_lib:format("config_info(~w)->~w;~n", [Key, Val]).

make_info_end() ->
    io_lib:format("config_info(X)->erlang:error({no_config_info,X}).~n", []).


set(Tag, App, Key, Val) ->
    try Tag:config_info(app_name) of
        App ->
            application:set_env(App, Key, Val),
            Envs = application:get_all_env(App),
            gen(Tag, App, Envs);
        App1 ->
            erlang:error({app_name_not_match, App1})
    catch
        error:undef ->
            erlang:error({app_config_not_inited, App})
    end.

