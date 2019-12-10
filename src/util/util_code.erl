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
    md5/2,
    bool2int/1,
    int2bool/1,

    to_binary/1
]).

-export([
    password/1,
    reg_name/1
]).

-compile(export_all).

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

md5(Data, Key) ->
    List = to_sort_list(Data),
    ParamForSign = to_form(List ++ [{key, Key}]),
    Sign = md5(ParamForSign),
    string:to_upper(Sign).

to_sort_list(Map) when is_map(Map)-> lists:sort(maps:to_list(Map));
to_sort_list(List) -> lists:sort(List).

to_form(L) ->
    L1 = [<<(util_code:to_binary(K))/binary, "=", (util_code:to_binary(V))/binary>> || {K, V} <- L],
    join(L1).

join([H|T]) ->
    TBin = << <<"&", E/binary>> || E <- T >>,
    <<H/binary, TBin/binary>>;
join([]) ->
    <<>>.

to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_list(X) ->
    erlang:list_to_binary(X);
to_binary(X) when is_integer(X) ->
    erlang:integer_to_binary(X);
to_binary(X) when is_float(X) ->
    iolist_to_binary(io_lib:format("~w", [X]));
to_binary(X) when is_atom(X) ->
    erlang:atom_to_binary(X, latin1);
to_binary(_) ->
    <<"invalid_to_binary">>.

%%[{amount, "100.00"}, {amount_true, "100.00"}, {appid, "1082039"}, {callbacks, "CODE_SUCCESS"}, {error_url, "123456"}, {out_trade_no, "68250020191209173509"}, {out_uid, "123457"}, {pay_type, "alipay"}, {success_url, "123456"}]