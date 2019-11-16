%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 10月 2019 19:06
%%%-------------------------------------------------------------------
-author("Administrator").

-define(MAIN_SUP, asiastar_sup).    %% 主监控树模块名

-define(true, true).
-define(false, false).
-define(undefined, undefined).
-define(infinity, infinity).

-ifndef(IF).
-define(IF(C,T,F), case C of true -> T; _ -> F end).
-endif.
-ifndef(TRY).
-define(TRY(B,T), (try (B) catch _:_->(T) end)).
-endif.

%% 时间相关的几个宏
-define(SEC_DAY, 86400).        % 天
-define(SEC_HOUR, 3600).        % 小时
-define(SEC_MINUTE, 60).        % 分
-define(CLI_FRAME_TIME, 33).    % 前端每帧毫秒数