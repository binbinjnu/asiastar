%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 10月 2019 10:22
%%%-------------------------------------------------------------------
-author("Administrator").

-define(NUM_ACCEPTORS, 10).     %% acceptor 池大小
-define(PACKAGE_HEAD_LEN, 16).  %% 包头长度
-define(TCP_OPTIONS, [
    {nodelay, true},                %% (默认false)数据包直接发送到套接字，不过它多么小。
    {delay_send, false},            %% (默认true)数据不是立即发送，而是存到发送队列里，等 socket 可写的时候再发送
    {send_timeout, 15000},          %% 设置一个时间去等待操作系统发送数据，如果底层在这个时间段后还没发出数据，那么就会返回 {error,timeout}
    {send_timeout_close, true},     %%
    {keepalive, false},             %% (默认false) 当没有转移数据时，确保所连接的套接字发送保持活跃（keepalive）的消息。
    {exit_on_close, true}]).        %% 设置为false时, socket 被关闭之后还能将缓冲区中的数据发送出去

-define(MAX_PACK_ACC, 10).          %% 最大包数量, 超出直接send
-define(PACK_FLUSH_SIZE, 1024).     %% 溢出bytes size, 溢出后直接send
-define(NET_STAT_INTERVAL, 10).     %% 网络统计间隔 seconds
-define(NET_PACKET_PER_SEC, 20).    %% 每秒中网络包数量
-define(NET_MAX_WATER_LV, 300).     %% 水位警戒等级
-define(NET_HEART_TIMEOUT, 120).    %% 心跳超时 seconds
