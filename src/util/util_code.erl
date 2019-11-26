%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%     编码, 字符转换的一些方法
%%% @end
%%% Created : 30. 10月 2019 9:39
%%%-------------------------------------------------------------------
-module(util_code).
-author("Administrator").

%% API
-export([
    term_to_string/1,
    string_to_term/1,
    term_to_bitstring/1,
    bitstring_to_term/1,
    md5/1,
    bool2int/1,
    int2bool/1
]).

-export([
    password/1,
    reg_name/1
]).

-define(PASSWORD_KEY, "YXNpYXN0YXJrZXk=").  % asiastarkey的base64编码
%% 密码
password(Str) ->
    erlang:list_to_binary(md5(Str ++ ?PASSWORD_KEY)).

%% 生成可用于进程register的atom
%% [test, "_", 1] => test_1
reg_name(L) ->
    erlang:list_to_atom(lists:concat(L)).

term_to_string(Term) ->
    lists:flatten(io_lib:format("~9999999p", [Term])).

string_to_term(String) ->
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> undefined
            end;
        _Error ->
            undefined
    end.

bitstring_to_term(BitString) ->
    String = erlang:binary_to_list(BitString),
    string_to_term(String).

term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~999999p", [Term])).


%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b",[N]) || N <- binary_to_list(erlang:md5(S))]).

%% bool型转int
bool2int(B) ->
    case B of
        true -> 1;
        false -> 0
    end.

%% int转bool
int2bool(I) ->
    I > 0.