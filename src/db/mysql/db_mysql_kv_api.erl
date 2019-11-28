%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 11月 2019 16:04
%%%-------------------------------------------------------------------
-module(db_mysql_kv_api).
-author("Administrator").

-include("hrl_logs.hrl").
-include("hrl_db.hrl").
-include("hrl_common.hrl").

%% API
-export([
    open/2
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
open(Table, Opts) ->
    case proplists:get_bool(protected, Opts) orelse proplists:get_bool(private, Opts) of
        ?true ->
            ets_svr:new(Table, Opts);
        _ ->
            ets_svr:hold_new(Table, Opts)
    end,
    Compress = proplists:get_value(data_compress, Opts, 0),
    Opts1 = [{save_func, {db_mysql_kv_command, kv_insert_from_ets, [Compress]}} | Opts],
    {ok, _} = db_mysql_svr:start(Table, Opts1),

    ok = db_mysql_kv_command:ensure_kv_table(Table, Opts).