asiastar
=====

An OTP application

Build
-----

    $ rebar3 compile

```text
使用 start 开启进程, 会有2个进程, 一个是守护进程, 一个是正常的erl进程
使用 foreground & 可后台开启进程, 只有一个erl进程, 但是关掉shell会导致进程关闭
使用 attach 后用 Ctrl+c 退出会导致整个进程退出 
可使用 remote_console , Ctrl+c 不会导致整个进程退出
```