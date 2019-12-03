%% proto协议中的协议名对应协议号宏定义

%% login.proto 协议号宏定义
-define(c2s_heartbeat, 10000).
-define(s2c_heartbeat, 10000).
-define(c2s_login, 10001).
-define(s2c_login, 10001).
-define(c2s_re_login, 10002).
-define(s2c_re_login, 10002).

%% player.proto 协议号宏定义
-define(c2s_player_info, 10100).
-define(s2c_player_info, 10100).
