%% proto协议中的协议名对应协议号宏定义

%% hello.proto 协议号宏定义
-define(c2s_hello, 59801).
-define(s2c_hello, 59801).

%% login.proto 协议号宏定义
-define(c2s_heartbeat, 10000).
-define(s2c_heartbeat, 10000).
-define(c2s_login, 10001).
-define(s2c_login, 10001).

%% player.proto 协议号宏定义
-define(c2s_player_info, 10100).
-define(s2c_player_info, 10100).

%% proto.proto 协议号宏定义

%% test.proto 协议号宏定义
-define(c2s_test1, 59901).
-define(c2s_test2, 59902).
-define(s2c_test1, 59901).
-define(s2c_test2, 59902).
