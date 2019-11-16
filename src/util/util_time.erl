%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 10月 2019 9:39
%%%-------------------------------------------------------------------
-module(util_time).
-author("Administrator").

%% API
-export([
    unixtime/0,
    long_unixtime/0,
    localtime/0,
    date/0,
    time/0,
    wee_hours_time/0,
    iso_week_number/0
]).

-define(DIFF_SECONDS_0000_1970_UTC_8, 62167248000).

%% 单位：秒
unixtime() ->
    erlang:system_time(seconds).

%% 单位：毫秒
long_unixtime() ->
    erlang:system_time(milli_seconds).

%% 本地时间
localtime() ->
    timestamp_to_localtime(unixtime()).

%% 时间戳转本地时间
timestamp_to_localtime(T) ->
    MS = T div 1000000,
    S = T rem 1000000,
    calendar:now_to_local_time({MS, S, 0}).

%% 日期
date() ->
    {Date, _} = localtime(),
    Date.

%% 时间
time() ->
    {_, Time} = localtime(),
    Time.

%% 获取当天凌晨时间
wee_hours_time() ->
    localtime_to_timestamp({?MODULE:date(), {0,0,0}}).

%% 获取指定日期的unix时间戳(只限中国时区)
%% DateTime格式: {{2013,10,9},{17,10,0}}
localtime_to_timestamp(DateTime)->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?DIFF_SECONDS_0000_1970_UTC_8.

%% 第几周
iso_week_number() ->
    calendar:iso_week_number().