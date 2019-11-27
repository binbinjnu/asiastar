%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 11月 2019 16:04
%%%-------------------------------------------------------------------
-module(xf_sql).
-author("Administrator").

-include("hrl_logs.hrl").
-include("hrl_db.hrl").

%% API
-export([
]).



%% 打开一个数据库表, 如果不存在则新建
%% Opts为Proplists, 可用的参数有:
%% 默认使用ets作为缓存表, 可用ets:new/2中的所有参数
%% {data_compress, V} 使用数据压缩, 默认为不压缩, 即{data_compress, 0}
%% {keyformat, Format} MySQL的主键格式, 默认为int
%%      可用的Format格式有
%%          int, 表示 BIGINT
%%          uint 表示 BIGINT UNSIGNED
%%          {uint, N} 表示 INT(N) UNSIGNED 其中N是长度
%%          {int, N} 表示 INT(N) 其中N是长度
%%          varbinary 表示 VARBINARY(255)
%%          {varbinary, N} 表示 VARBINARY(N)
%% {index_modified, Bool} 表示是否对修改时间进行索引, 默认为false,
%%          使用转换服的表需要设置为true
%%open(Table, Opts) ->
%%    case proplists:get_value(mnesia, Opts) of
%%        ?undefined ->
%%            case proplists:get_bool(protected, Opts)
%%                orelse proplists:get_bool(private, Opts) of
%%                ?true ->
%%                    fg_ets:new(Table, Opts);
%%                _ ->
%%                    ok = fg_ets:hold_new(Table, Opts)
%%            end;
%%        MnesiaOpts ->
%%            {atomic, ok} = mnesia:create_table(Table, MnesiaOpts)
%%    end,
%%    StoreArgs = #{
%%        table         => Table,
%%        data_compress => proplists:get_value(data_compress, Opts, 0)
%%    },
%%    SqlOpts = [{save_func, {fg_sql_inf, kv_insert_from_ets, StoreArgs}}|Opts],
%%    {ok, _} = fg_sql_svr:start(Table, SqlOpts),
%%
%%    ok = fg_sql_inf:ensure_kv_table(Table, Opts).