-module('v.net.urllib').
-export([error_msg/2, should_escape/2, query_unescape/1, path_unescape/1, unescape/2, query_escape/1, path_escape/1, escape/2, 'URL.debug'/1, user/1, user_password/2, 'Userinfo.empty'/1, 'Userinfo.str'/1, split_by_scheme/1, get_scheme/1, split/3, parse/1, parse_request_uri/1, parse_url/2, parse_authority/1, parse_host/1, 'URL.set_path'/2, 'URL.escaped_path'/1, valid_encoded_path/1, valid_optional_port/1, 'URL.str'/1, parse_query/1, parse_query_silent/1, parse_query_values/2, 'Values.encode'/1, resolve_path/2, 'URL.is_abs'/1, 'URL.parse'/2, 'URL.resolve_reference'/2, 'URL.query'/1, 'URL.request_uri'/1, 'URL.hostname'/1, 'URL.port'/1, split_host_port/1, valid_userinfo/1, string_contains_ctl_u8/1, ishex/1, unhex/1, new_values/0, 'Values.get'/2, 'Values.get_all'/2, 'Values.set'/3, 'Values.add'/3, 'Values.del'/2, 'Values.values'/1, 'Values.to_map'/1, 'EncodingMode__static__from'/1]).

error_msg(Message, Val) ->
    Msg = <<"net.urllib.", (Message)/binary>>,
    case Val /= <<"">> of
        true -> ok;
        false -> ok
    end,
    Msg.

should_escape(C, Mode) ->
    case 'u8.is_alnum'(C) of
        true -> false;
        false -> begin
            case Mode == encode_host orelse Mode == encode_zone of
                true -> case lists:member(C, [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo]) of
                    true -> false;
                    false -> ok
                end;
                false -> ok
            end,
            case C of
                todo; todo; todo; todo -> false;
                todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> case Mode of
                    encode_path -> C == todo;
                    encode_path_segment -> C == todo orelse C == todo orelse C == todo orelse C == todo;
                    encode_user_password -> C == todo orelse C == todo orelse C == todo orelse C == todo;
                    encode_query_component -> true;
                    encode_fragment -> false;
                    _ -> ok
                end;
                _ -> ok
            end,
            case Mode == encode_fragment of
                true -> case C of
                    todo; todo; todo; todo -> false;
                    _ -> ok
                end;
                false -> ok
            end,
            true
        end
        end.

query_unescape(S) ->
    unescape(S, encode_query_component).

path_unescape(S) ->
    unescape(S, encode_path_segment).

unescape(S_, Mode) ->
    S = S_,
    N = 0,
    Has_plus = false,
    % TODO: unhandled stmt type
    ok    case N == 0 andalso not Has_plus of
        true -> S;
        false -> 
            case length(S) < 2 * N of
                true -> error(error_msg(<<"unescape: invalid escape sequence">>, <<"">>));
                false -> begin
                    T = new_builder(length(S) - 2 * N),
                    % TODO: unhandled stmt type
                    ok                    'Builder.str'(T)
                end
                        end
                end.

query_escape(S) ->
    escape(S, encode_query_component).

path_escape(S) ->
    escape(S, encode_path_segment).

escape(S, Mode) ->
    Space_count = 0,
    Hex_count = 0,
    C = todo,
    C1 = lists:foldl(fun(I, CAcc) ->
        COut = lists:nth(I + 1, S),
        case should_escape(C1, Mode) of
            true -> case C1 == todo andalso Mode == encode_query_component of
                true -> todo;
                false -> todo
            end;
            false -> ok
        end,
        COut
    end, C, lists:seq(0, length(S) - 1)),
    case Space_count == 0 andalso Hex_count == 0 of
        true -> S;
        false -> begin
            Required = length(S) + 2 * Hex_count,
            T = [],
            case Hex_count == 0 of
                true -> '[]u8.bytestr'(T);
                false -> begin
                    Upperhex = <<"0123456789ABCDEF">>,
                    J = 0,
                    C1 = lists:foldl(fun(I, C1Acc) ->
                        C1Out = lists:nth(I + 1, S),
                        case C1 == todo andalso Mode == encode_query_component of
                            true -> begin
                                todo
                            end;
                            false -> case should_escape(C1, Mode) of
                                true -> begin
                                    J1 = 3,
                                end;
                                false -> begin
                                    todo
                                end
                            end
                        end,
                        C1Out
                    end, C1, lists:seq(0, length(S) - 1)),
                    '[]u8.bytestr'(T)
                end
                        end
        end
        end.

'URL.debug'(Url) ->
    <<"URL{\\n  scheme: ", (maps:get(scheme, Url))/binary, "\\n  opaque: ", (maps:get(opaque, Url))/binary, "\\n  user: ", (maps:get(user, Url))/binary, "\\n  host: ", (maps:get(host, Url))/binary, "\\n  path: ", (maps:get(path, Url))/binary, "\\n  raw_path: ", (maps:get(raw_path, Url))/binary, "\\n  force_query: ", (atom_to_binary(maps:get(force_query, Url)))/binary, "\\n  raw_query: ", (maps:get(raw_query, Url))/binary, "\\n  fragment: ", (maps:get(fragment, Url))/binary, "\\n}">>.

user(Username) ->
    #{username => Username, password => <<"">>, password_set => false, {vbeam, type} => 'Userinfo'}.

user_password(Username, Password) ->
    #{username => Username, password => Password, password_set => true, {vbeam, type} => 'Userinfo'}.

'Userinfo.empty'(U) ->
    maps:get(username, U) == <<"">> andalso maps:get(password, U) == <<"">>.

'Userinfo.str'(U) ->
    case 'Userinfo.empty'(U) of
        true -> <<"">>;
        false -> begin
            S = escape(maps:get(username, U), encode_user_password),
            case maps:get(password_set, U) of
                true -> ok;
                false -> ok
            end,
            S
        end
        end.

split_by_scheme(Rawurl) ->
    C = lists:foldl(fun(I, CAcc) ->
        COut = lists:nth(I + 1, Rawurl),
        case 'u8.is_letter'(C) of
            true -> ok;
            false -> case 'u8.is_digit'(C) orelse lists:member(C, [todo, todo, todo]) of
                true -> case I == 0 of
                    true -> [<<"">>, Rawurl];
                    false -> ok
                end;
                false -> case C == todo of
                    true -> begin
                        case I == 0 of
                            true -> error(error_msg(<<"split_by_scheme: missing protocol scheme">>, <<"">>));
                            false -> ok
                        end,
                        [lists:nth(todo + 1, Rawurl), lists:nth(todo + 1, Rawurl)]
                    end;
                    false -> [<<"">>, Rawurl]
                end
            end
        end,
        COut
    end, C, lists:seq(0, length(Rawurl) - 1)),
    [<<"">>, Rawurl].

get_scheme(Rawurl) ->
    Split = split_by_scheme(Rawurl),
    lists:nth(1, Split).

split(S, Sep, Cutc) ->
    I = 'string.index_u8'(S, Sep),
    case I < 0 of
        true -> S;
        false -> 
            case Cutc of
                true -> lists:nth(todo + 1, S);
                false -> lists:nth(todo + 1, S)
                        end
                end.

parse(Rawurl) ->
    U = element(1, split(Rawurl, todo, true)),
    Frag = element(2, split(Rawurl, todo, true)),
    Url = parse_url(U, false),
    case Frag == <<"">> of
        true -> Url;
        false -> begin
            F = unescape(Frag, encode_fragment),
            Url
        end
        end.

parse_request_uri(Rawurl) ->
    parse_url(Rawurl, true).

parse_url(Rawurl, Via_request) ->
    case string_contains_ctl_u8(Rawurl) of
        true -> error(error_msg(<<"parse_url: invalid control character in URL">>, Rawurl));
        false -> 
            case Rawurl == <<"">> andalso Via_request of
                true -> error(error_msg(<<"parse_url: empty URL">>, Rawurl));
                false -> begin
                    Url = #{user => todo, {vbeam, type} => 'URL'},
                    case Rawurl == <<"*">> of
                        true -> Url;
                        false -> begin
                            P = split_by_scheme(Rawurl),
                            Rest = lists:nth(2, P),
                            case 'string.ends_with'(Rest, <<"?">>) andalso not 'string.contains'(lists:nth(todo + 1, Rest), <<"?">>) of
                                true -> begin
                                    Rest1 = lists:nth(todo + 1, Rest),
                                end;
                                false -> begin
                                    R = element(1, split(Rest1, todo, true)),
                                    Raw_query = element(2, split(Rest1, todo, true)),
                                    Rest2 = R,
                                end
                            end,
                            case not 'string.starts_with'(Rest2, <<"/">>) of
                                true -> begin
                                    case maps:get(scheme, Url) /= <<"">> of
                                        true -> begin
                                            Url
                                        end;
                                        false -> ok
                                    end,
                                    case Via_request of
                                        true -> error(error_msg(<<"parse_url: invalid URI for request">>, <<"">>));
                                        false -> ok
                                    end,
                                    case todo of
                                        true -> begin
                                            Slash = 'string.index'(Rest2, <<"/">>),
                                            case Colon >= 0 andalso (Slash < 0 orelse Colon < Slash) of
                                                true -> error(error_msg(<<"parse_url: first path segment in URL cannot contain colon">>, <<"">>));
                                                false -> ok
                                            end
                                        end;
                                        false -> ok
                                    end
                                end;
                                false -> ok
                            end,
                            case ((maps:get(scheme, Url) /= <<"">> orelse not Via_request) andalso not 'string.starts_with'(Rest2, <<"///">>)) andalso 'string.starts_with'(Rest2, <<"//">>) andalso length(Rest2) > 2 of
                                true -> begin
                                    Authority = element(1, split(lists:nth(todo + 1, Rest2), todo, false)),
                                    R1 = element(2, split(lists:nth(todo + 1, Rest2), todo, false)),
                                    Rest3 = R1,
                                    A = parse_authority(Authority),
                                end;
                                false -> ok
                            end,
                            'URL.set_path'(Url, Rest3),
                            Url
                        end
                                        end
                end
                        end
                end.

parse_authority(Authority) ->
    I = 'string.last_index_u8'(Authority, todo),
    case I < 0 of
        true -> #{host => parse_host(Authority), user => user(<<"">>), {vbeam, type} => 'ParseAuthorityRes'};
        false -> begin
            Raw_user = lists:nth(todo + 1, Authority),
            Raw_host = lists:nth(todo + 1, Authority),
            case not valid_userinfo(Raw_user) of
                true -> error(error_msg(<<"parse_authority: invalid userinfo">>, <<"">>));
                false -> begin
                    Host = parse_host(Raw_host),
                    Name = element(1, split(Raw_user, todo, true)),
                    Pwd = element(2, split(Raw_user, todo, true)),
                    Auth = case Pwd /= <<"">> of
                        true -> user_password(unescape(Name, encode_user_password), unescape(Pwd, encode_user_password));
                        false -> user(unescape(Name, encode_user_password))
                    end,
                    #{user => Auth, host => Host, {vbeam, type} => 'ParseAuthorityRes'}
                end
                        end
        end
        end.

parse_host(Host) ->
    case length(Host) > 0 andalso lists:nth(1, Host) == todo of
        true -> begin
            I = 'string.last_index_u8'(Host, todo),
            case I == -1 of
                true -> error(error_msg(<<"parse_host: missing ']' in host">>, <<"">>));
                false -> ok
            end,
            Colon_port = lists:nth(todo + 1, Host),
            case not valid_optional_port(Colon_port) of
                true -> error(error_msg(<<"parse_host: invalid port ", (Colon_port)/binary, " after host ">>, <<"">>));
                false -> ok
            end,
            case todo of
                true -> begin
                    Host1 = unescape(lists:nth(todo + 1, Host), encode_host),
                    Host2 = unescape(lists:nth(todo + 1, Host), encode_zone),
                    Host3 = unescape(lists:nth(todo + 1, Host), encode_host),
                    <<(<<(Host1)/binary, (Host2)/binary>>)/binary, (Host3)/binary>>
                end;
                false -> ok
            end
        end;
        false -> begin
            I1 = 'string.last_index_u8'(Host, todo),
            case I1 /= -1 of
                true -> begin
                    Colon_port1 = lists:nth(todo + 1, Host),
                    case not valid_optional_port(Colon_port1) of
                        true -> error(error_msg(<<"parse_host: invalid port ", (Colon_port1)/binary, " after host ">>, <<"">>));
                        false -> ok
                    end
                end;
                false -> ok
            end
        end
    end,
    H = unescape(Host, encode_host),
    H.

'URL.set_path'(U, P) ->
    true.

'URL.escaped_path'(U) ->
    case maps:get(raw_path, U) /= <<"">> andalso valid_encoded_path(maps:get(raw_path, U)) of
        true -> maps:get(raw_path, U);
        false -> 
            case maps:get(path, U) == <<"*">> of
                true -> <<"*">>;
                false -> escape(maps:get(path, U), encode_path)
                        end
                end.

valid_encoded_path(S) ->
    X = lists:foldl(fun(I, XAcc) ->
        XOut = lists:nth(I + 1, S),
        case X of
            todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> ok;
            todo; todo -> ok;
            todo -> ok;
            _ -> case should_escape(lists:nth(I + 1, S), encode_path) of
                true -> false;
                false -> ok
            end
        end,
        XOut
    end, X, lists:seq(0, length(S) - 1)),
    true.

valid_optional_port(Port) ->
    case Port == <<"">> of
        true -> true;
        false -> 
            case lists:nth(1, Port) /= todo of
                true -> false;
                false -> begin
                    lists:foreach(fun(B) ->
                        case B < todo orelse B > todo of
                            true -> false;
                            false -> ok
                        end,
                        ok
                    end, lists:nth(todo + 1, Port)),
                    true
                end
                        end
                end.

'URL.str'(U) ->
    Buf = new_builder(200),
    case maps:get(scheme, U) /= <<"">> of
        true -> begin
            'Builder.write_string'(Buf, maps:get(scheme, U)),
            'Builder.write_string'(Buf, <<":">>)
        end;
        false -> ok
    end,
    case maps:get(opaque, U) /= <<"">> of
        true -> 'Builder.write_string'(Buf, maps:get(opaque, U));
        false -> begin
            User = maps:get(user, U),
            case maps:get(scheme, U) /= <<"">> orelse maps:get(host, U) /= <<"">> orelse not 'Userinfo.empty'(User) of
                true -> begin
                    case maps:get(host, U) /= <<"">> orelse maps:get(path, U) /= <<"">> orelse not 'Userinfo.empty'(User) of
                        true -> 'Builder.write_string'(Buf, <<"//">>);
                        false -> ok
                    end,
                    case not 'Userinfo.empty'(User) of
                        true -> begin
                            'Builder.write_string'(Buf, 'Userinfo.str'(User)),
                            'Builder.write_string'(Buf, <<"@">>)
                        end;
                        false -> ok
                    end,
                    case maps:get(host, U) /= <<"">> of
                        true -> 'Builder.write_string'(Buf, escape(maps:get(host, U), encode_host));
                        false -> ok
                    end
                end;
                false -> ok
            end,
            Path = 'URL.escaped_path'(U),
            case Path /= <<"">> andalso lists:nth(1, Path) /= todo andalso maps:get(host, U) /= <<"">> of
                true -> 'Builder.write_string'(Buf, <<"/">>);
                false -> ok
            end,
            case length(Buf) == 0 of
                true -> begin
                    I = 'string.index_u8'(Path, todo),
                    case I > -1 of
                        true -> case I > -1 andalso 'string.index_u8'(lists:nth(todo + 1, Path), todo) == -1 of
                            true -> 'Builder.write_string'(Buf, <<"./">>);
                            false -> ok
                        end;
                        false -> ok
                    end
                end;
                false -> ok
            end,
            'Builder.write_string'(Buf, Path)
        end
    end,
    case maps:get(force_query, U) orelse maps:get(raw_query, U) /= <<"">> of
        true -> begin
            'Builder.write_string'(Buf, <<"?">>),
            'Builder.write_string'(Buf, maps:get(raw_query, U))
        end;
        false -> ok
    end,
    case maps:get(fragment, U) /= <<"">> of
        true -> begin
            'Builder.write_string'(Buf, <<"#">>),
            'Builder.write_string'(Buf, escape(maps:get(fragment, U), encode_fragment))
        end;
        false -> ok
    end,
    'Builder.str'(Buf).

parse_query(Query) ->
    M = new_values(),
    parse_query_values(M, Query),
    M.

parse_query_silent(Query) ->
    M = new_values(),
    parse_query_values(M, Query),
    M.

parse_query_values(M, Query) ->
    Had_error = false,
    Q = Query,
    % TODO: unhandled stmt type
    ok    case Had_error of
        true -> error(error_msg(<<"parse_query_values: failed parsing query string">>, <<"">>));
        false -> true
        end.

'Values.encode'(V) ->
    case length(V) == 0 of
        true -> <<"">>;
        false -> begin
            Buf = new_builder(200),
            lists:foreach(fun(Qvalue) ->
                Key_kscaped = query_escape(maps:get(key, Qvalue)),
                case length(Buf) > 0 of
                    true -> 'Builder.write_string'(Buf, <<"&">>);
                    false -> ok
                end,
                'Builder.write_string'(Buf, Key_kscaped),
                case maps:get(value, Qvalue) == <<"">> of
                    true -> ok;
                    false -> ok
                end,
                'Builder.write_string'(Buf, <<"=">>),
                'Builder.write_string'(Buf, query_escape(maps:get(value, Qvalue))),
                ok
            end, maps:get(data, V)),
            'Builder.str'(Buf)
        end
        end.

resolve_path(Base, Ref) ->
    Full = <<"">>,
    case Ref == <<"">> of
        true -> ok;
        false -> case lists:nth(1, Ref) /= todo of
            true -> begin
                I = 'string.last_index_u8'(Base, todo),
                Full1 = <<(lists:nth(todo + 1, Base))/binary, (Ref)/binary>>,
            end;
            false -> ok
        end
    end,
    case Full1 == <<"">> of
        true -> <<"">>;
        false -> begin
            Dst = [],
            Src = 'string.split'(Full1, <<"/">>),
            lists:foreach(fun(Elem) ->
                case Elem of
                    <<".">> -> ok;
                    <<"..">> -> case length(Dst) > 0 of
                        true -> ok;
                        false -> ok
                    end;
                    _ -> Dst bsl Elem
                end,
                ok
            end, Src),
            Last = lists:nth(length(Src) - 1 + 1, Src),
            case Last == <<".">> orelse Last == <<"..">> of
                true -> Dst bsl <<"">>;
                false -> ok
            end,
            <<(<<"/">>)/binary, ('string.trim_left'('[]string.join'(Dst, <<"/">>), <<"/">>))/binary>>
        end
        end.

'URL.is_abs'(U) ->
    maps:get(scheme, U) /= <<"">>.

'URL.parse'(U, Ref) ->
    Refurl = parse(Ref),
    'URL.resolve_reference'(U, Refurl).

'URL.resolve_reference'(U, Ref) ->
    Url = *Ref,
    case maps:get(scheme, Ref) == <<"">> of
        true -> ok;
        false -> ok
    end,
    Ref_user = maps:get(user, Ref),
    case maps:get(scheme, Ref) /= <<"">> orelse maps:get(host, Ref) /= <<"">> orelse not 'Userinfo.empty'(Ref_user) of
        true -> Url;
        false -> 
            case maps:get(opaque, Ref) /= <<"">> of
                true -> Url;
                false -> begin
                    case maps:get(path, Ref) == <<"">> andalso maps:get(raw_query, Ref) == <<"">> of
                        true -> begin
                            case maps:get(fragment, Ref) == <<"">> of
                                true -> ok;
                                false -> ok
                            end
                        end;
                        false -> ok
                    end,
                    'URL.set_path'(Url, resolve_path('URL.escaped_path'(U), 'URL.escaped_path'(Ref))),
                    Url
                end
                        end
                end.

'URL.query'(U) ->
    V = parse_query_silent(maps:get(raw_query, U)),
    V.

'URL.request_uri'(U) ->
    Result = maps:get(opaque, U),
    case Result == <<"">> of
        true -> begin
            Result1 = 'URL.escaped_path'(U),
            case Result1 == <<"">> of
                true -> ok;
                false -> ok
            end
        end;
        false -> case 'string.starts_with'(Result1, <<"//">>) of
            true -> ok;
            false -> ok
        end
    end,
    case maps:get(force_query, U) orelse maps:get(raw_query, U) /= <<"">> of
        true -> ok;
        false -> ok
    end,
    Result1.

'URL.hostname'(U) ->
    Host = element(1, split_host_port(maps:get(host, U))),
    Host.

'URL.port'(U) ->
    Port = element(2, split_host_port(maps:get(host, U))),
    Port.

split_host_port(Hostport) ->
    Host = Hostport,
    Port = <<"">>,
    Colon = 'string.last_index_u8'(Host, todo),
    case Colon /= -1 of
        true -> case valid_optional_port(lists:nth(todo + 1, Host)) of
            true -> begin
                Port1 = lists:nth(todo + 1, Host),
                Host1 = lists:nth(todo + 1, Host),
            end;
            false -> ok
        end;
        false -> ok
    end,
    case length(Host1) > 1 andalso lists:nth(1, Host1) == todo andalso 'string.ends_with'(Host1, <<"]">>) of
        true -> ok;
        false -> ok
    end,
    Host1.

valid_userinfo(S) ->
    lists:foreach(fun(R) ->
        case 'u8.is_alnum'(R) of
            true -> ok;
            false -> ok
        end,
        case R of
            todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo; todo -> ok;
            _ -> false
        end,
        ok
    end, S),
    true.

string_contains_ctl_u8(S) ->
    B = lists:foldl(fun(I, BAcc) ->
        BOut = lists:nth(I + 1, S),
        case B < todo orelse B == 16#7f of
            true -> true;
            false -> ok
        end,
        BOut
    end, B, lists:seq(0, length(S) - 1)),
    false.

ishex(C) ->
    case todo =< C andalso C =< todo of
        true -> true;
        false -> case todo =< C andalso C =< todo of
            true -> true;
            false -> case todo =< C andalso C =< todo of
                true -> true;
                false -> ok
            end
        end
    end,
    false.

unhex(C) ->
    case todo =< C andalso C =< todo of
        true -> C - todo;
        false -> case todo =< C andalso C =< todo of
            true -> C - todo + 10;
            false -> case todo =< C andalso C =< todo of
                true -> C - todo + 10;
                false -> ok
            end
        end
    end,
    0.

new_values() ->
    #{data => [], {vbeam, type} => 'Values'}.

'Values.get'(V, Key) ->
    case length(maps:get(data, V)) == 0 of
        true -> todo;
        false -> begin
            lists:foreach(fun(Qvalue) ->
                case maps:get(key, Qvalue) == Key of
                    true -> maps:get(value, Qvalue);
                    false -> ok
                end,
                ok
            end, maps:get(data, V)),
            todo
        end
        end.

'Values.get_all'(V, Key) ->
    case length(maps:get(data, V)) == 0 of
        true -> [];
        false -> begin
            Values = [],
            lists:foreach(fun(Qvalue) ->
                case maps:get(key, Qvalue) == Key of
                    true -> Values bsl maps:get(value, Qvalue);
                    false -> ok
                end,
                ok
            end, maps:get(data, V)),
            Values
        end
        end.

'Values.set'(V, Key, Value) ->
    Found = false,
    lists:foreach(fun(Qvalue) ->
        case maps:get(key, Qvalue) == Key of
            true -> begin
                Found1 = true,
            end;
            false -> ok
        end,
        ok
    end, maps:get(data, V)),
    case not Found1 of
        true -> 'Values.add'(V, Key, Value);
        false -> ok
    end.

'Values.add'(V, Key, Value) ->
    maps:get(data, V) bsl #{key => Key, value => Value, {vbeam, type} => 'QueryValue'},

'Values.del'(V, Key) ->
    lists:foreach(fun(Qvalue) ->
        case maps:get(key, Qvalue) == Key of
            true -> 'QueryValue.delete'(maps:get(data, V), Idx);
            false -> ok
        end,
        ok
    end, maps:get(data, V)),

'Values.values'(V) ->
    Values = [],
    lists:foreach(fun(Qvalue) ->
        case maps:get(value, Qvalue) /= <<"">> of
            true -> Values bsl maps:get(value, Qvalue);
            false -> ok
        end,
        ok
    end, maps:get(data, V)),
    Values.

'Values.to_map'(V) ->
    Result = #{},
    lists:foreach(fun(Qvalue) ->
        case lists:member(maps:get(key, Qvalue), Result) of
            true -> maps:get(maps:get(key, Qvalue), Result) bsl maps:get(value, Qvalue);
            false -> ok
        end,
        ok
    end, maps:get(data, V)),
    Result.

'EncodingMode__static__from'(Input) ->
    error(<<"invalid value">>).
