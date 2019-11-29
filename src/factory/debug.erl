%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     线上环境不能瞎比用
%%%     调试用的相关接口, 如: fprof, cprof等
%%%     可利用erlcron来进行调试
%%% @end
%%% Created : 29. 11月 2019 15:43
%%%-------------------------------------------------------------------
-module(debug).
-author("Administrator").

%% API
%% cprof
%% 测试每个函数被调用了多少次
%% 轻量, 在运行系统上使用会给系统带来5%~10%的额外负载
-export([
    cprof/0,
    cprof/1,
    cprof_txt/0,
    cprof_txt/1
]).

%% eprof
%% 分析一个Erlang程序中计算时间主要消耗在哪里
%% fprof的前任，不同在于：这适合规模小一些的性能前评估.
-export([
    eprof/0,
    eprof/1,
    eprof/2,
    eprof_all/1
]).

%% fprof
%% 显示函数调用和被调用的埋单，并将结果输出到一个文件中，
%% 比较适合于在实验环境或模拟环境中对进行大规模的性能前一段
%% 它会带来非常显著的系统负载.
-export([
    fprof/1,
    fprof/2,
    fprof_all/0,
    fprof_all/1,
    fprof_children/1,
    fprof_children/2,
    fprof_analyse/1,
    fprof_analyse/2
]).


%% cprof
cprof() ->
    cprof(2000).

cprof(Time) ->
    cprof:start(),
    timer:sleep(Time),
    cprof:pause(),
    Ret = cprof:analyse(),
    cprof:stop(),
    Ret.

cprof_txt() ->
    cprof_txt(2000).

cprof_txt(Time) ->
    Ret = cprof(Time),
    file:write_file("cprof.txt", io_lib:format("~p", [Ret])).


%% eprof
eprof() ->
    eprof(processes() -- [whereis(eprof)], 2000).

eprof(Pid) ->
    eprof(Pid, 2000).

eprof(Pid, Time) ->
    Pid1 = recon_lib:term_to_pid(Pid),
    do_eprof([Pid1], Time).

eprof_all(TimeoutSec) when is_integer(TimeoutSec) ->
    do_eprof(processes() -- [whereis(eprof)], TimeoutSec).

do_eprof(Pids, TimeoutSec) ->
    eprof:start(),
    Filename = "eprof_" ++ ymd_his() ++ ".log",
    eprof:log(Filename),
    eprof:start_profiling(Pids),
    timer:sleep(TimeoutSec),
    eprof:stop_profiling(),
    eprof:analyze(total),
    eprof:stop().


%% fprof
fprof(Pid) ->
    fprof(Pid, 2000).

fprof(Pid, Time) ->
    Pid1 = recon_lib:term_to_pid(Pid),
    do_fprof(Pid1, Time).

fprof_all() ->
    fprof_all(2000).

fprof_all(Time) when is_integer(Time) ->
    All = lists:delete(whereis(fprof_server), erlang:processes()),
    do_fprof(All, Time).

fprof_children(Sup) ->
    fprof_children(Sup, 2000).

fprof_children(Sup, Time) ->
    Pids = children(Sup),
    do_fprof(Pids, Time).

do_fprof(Pid, Time) ->
    file:delete("fprof.trace"),
    fprof:trace([start,{procs,Pid}]),
    timer:sleep(Time),
    fprof:trace(stop).

fprof_analyse(Pid) when is_pid(Pid) ->
    fprof_analyse(Pid, 2 * 1000);

fprof_analyse(Time) when is_integer(Time) ->
    All = lists:delete(whereis(fprof_server), erlang:processes()),
    fprof_analyse(All, Time).

fprof_analyse(Pid, Time) ->
    file:delete("fprof.trace"),
    fprof:trace([start,{procs,Pid}]),
    timer:sleep(Time),
    fprof:trace(stop),
    fprof:profile(),
    Filename = "fprof_" ++ ymd_his() ++ ".log",
    fprof:analyse({dest, Filename}).



%%% --------------------------------------------------------
%%%                     Local Function
%%% --------------------------------------------------------
ymd_his() ->
    {Y,M,D} = date(),
    {HH,MM,SS} = time(),
    DD = 10000 * Y + 100 * M + D,
    TT = 10000 * HH + 100 * MM + SS,
    integer_to_list(DD) ++ "_" ++ integer_to_list(TT).

children(Sup) ->
    SupPid = recon_lib:term_to_pid(Sup),
    Children = supervisor:which_children(SupPid),
    [Pid || {_, Pid, _, _} <- Children].