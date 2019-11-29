%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 11月 2019 14:54
%%%-------------------------------------------------------------------
-module(db_mysql_test).
-author("Administrator").

-include("hrl_logs.hrl").
-include("hrl_db.hrl").

%% API
-compile(export_all).
-compile(nowarn_export_all).

test_disk_tables() ->
    Res = db_mysql_command:disk_tables(),
    ?INFO("~w", [Res]).

test_insert() ->
    Fields = record_info(fields, user_money_t),
    Record = #user_money_t{user_id = 1, bank_coin = 100, game_coin = 200},
    Res = db_mysql_command:insert(user_money_t, Fields, Record),
    ?INFO("~w", [Res]),
    Record1 = #user_money_t{user_id = 2, bank_coin = 100, game_coin = 100},
    Res1 = db_mysql_command:insert(user_money_t, Fields, Record1),
    ?INFO("~w", [Res1]),
    Record2 = #user_money_t{user_id = 2, bank_coin = 200, game_coin = 200},
    Res2 = db_mysql_command:insert(user_money_t, Fields, Record2),
    ?INFO("~w", [Res2]),
    Record3 = Record2#user_money_t{bank_coin = 2000},
    Record4 = #user_money_t{user_id = 3, bank_coin = 200, game_coin = 200},
    Res3 = db_mysql_command:insert(user_money_t, Fields, [Record3, Record4]),
    ?INFO("~w", [Res3]),

    ok.

test_select_by_key() ->
    Fields = record_info(fields, user_money_t),
    %% 测试正常情况
    Res1 = db_mysql_command:select_by_key(user_money_t, user_id, 1, Fields),
    ?INFO("~w", [Res1]),
    %% 测试null_val
    Res2 = db_mysql_command:select_by_key(user_money_t, user_id, 100, Fields),
    ?INFO("~w", [Res2]),
    %% 测试错误情况
    Res3 = db_mysql_command:select_by_key(user_money_t, id, 100, Fields),
    ?INFO("~w", [Res3]),
    ok.

test_select_many() ->
    Fields = record_info(fields, user_money_t),
    %% 测试正常情况
    Res1 = db_mysql_command:select_many(user_money_t, user_id, [1,2,3], Fields),
    ?INFO("~w", [Res1]),
    Res2 = db_mysql_command:select_many(user_money_t, user_id, [4,5], Fields),
    ?INFO("~w", [Res2]),
    Res3 = db_mysql_command:select_many(user_money, user_id, [3,4,5], Fields),
    ?INFO("~w", [Res3]),
    ok.

test_select_seq() ->
    Fields = record_info(fields, user_money_t),
    %% 测试正常情况
    Res1 = db_mysql_command:select_seq(user_money_t, Fields, 1, 4),
    ?INFO("~w", [Res1]),
    Res2 = db_mysql_command:select_seq(user_money_t, Fields, 2, 5),
    ?INFO("~w", [Res2]),
    Res3 = db_mysql_command:select_seq(user_money_t, Fields, 5, 1),
    ?INFO("~w", [Res3]),
    ok.

test_select_first() ->
    Fields = record_info(fields, user_money_t),
    Res1 = db_mysql_command:select_first(user_money_t, user_id, Fields, 2),
    ?INFO("~w", [Res1]),
    Res2 = db_mysql_command:select_first(user_money_t, user_id, Fields, 10),
    ?INFO("~w", [Res2]),
    ok.

test_select_next() ->
    Fields = record_info(fields, user_money_t),
    %% 测试正常情况
    Res1 = db_mysql_command:select_next(user_money_t, user_id, 1, Fields, 4),
    ?INFO("~w", [Res1]),
    Res2 = db_mysql_command:select_next(user_money_t, user_id, 1000000, Fields, 1),
    ?INFO("~w", [Res2]),
    ok.


test_select_table_size() ->
    Res1 = db_mysql_command:select_table_size(user_money_t),
    ?INFO("~w", [Res1]),
    ok.

test_select_max() ->
    Res1 = db_mysql_command:select_max(user_money_t, user_id),
    ?INFO("~w", [Res1]),
    Res2 = db_mysql_command:select_max(user_money_t, user),
    ?INFO("~w", [Res2]),
    Res3 = db_mysql_command:select_max(user_money_t, game_coin),
    ?INFO("~w", [Res3]),
    ok.

