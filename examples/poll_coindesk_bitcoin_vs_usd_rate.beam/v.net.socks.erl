-module('v.net.socks').
-export([socks5_dial/4, socks5_ssl_dial/4, new_socks5_dialer/4, 'SOCKS5Dialer.dial'/2, handshake/4, reply/1, parse_ipv4/1]).
% TODO: const socks_version5 = u8(5);
% TODO: const addr_type_ipv4 = u8(1);
% TODO: const addr_type_fqdn = u8(3);
% TODO: const addr_type_ipv6 = u8(4);
% TODO: const no_auth = u8(0);
% TODO: const auth_user_password = u8(2);

socks5_dial(Proxy_url, Host, Username, Password) ->
    Con = dial_tcp(Proxy_url),
    Socks_conn_as_interface = handshake(Con, Host, Username, Password),
    Socks_conn = todo,
    &Socks_conn.

socks5_ssl_dial(Proxy_url, Host, Username, Password) ->
    Ssl_conn = new_ssl_conn(#{verify => <<"">>, cert => <<"">>, cert_key => <<"">>, validate => false, in_memory_verification => false, {vbeam, type} => 'SSLConnectConfig'}),
    Con = socks5_dial(Proxy_url, Host, Username, Password),
    'SSLConn.connect'(Ssl_conn, Con, 'string.all_before_last'(Host, <<":">>)),
    Ssl_conn.

new_socks5_dialer(Base, Proxy_address, Username, Password) ->
    &#{dialer => Base, proxy_address => Proxy_address, username => Username, password => Password, {vbeam, type} => 'SOCKS5Dialer'}.

'SOCKS5Dialer.dial'(Sd, Address) ->
    Conn = 'Dialer.dial'(maps:get(dialer, Sd), maps:get(proxy_address, Sd)),
    handshake(Conn, Address, maps:get(username, Sd), maps:get(password, Sd)).

handshake(Con, Host, Username, Password) ->
    V = [todo, 1],
    case length(Username) > 0 of
        true -> V << todo;
        false -> V << todo
    end,
    'Connection.write'(Con, V),
    Bf = [],
    'Connection.read'(Con, Bf),
    case lists:nth(1, Bf) != todo of
        true -> begin
            'Connection.close'(Con),
            error(<<"unexpected protocol version ", (integer_to_binary(lists:nth(1, Bf)))/binary>>)
        end;
        false -> ok
    end,
    case length(Username) == 0 of
        true -> case lists:nth(2, Bf) != 0 of
            true -> begin
                'Connection.close'(Con),
                error(reply(lists:nth(2, Bf)))
            end;
            false -> ok
        end;
        false -> ok
    end,
    case length(Username) > 0 of
        true -> begin
            '[]u8.clear'(V),
            V << todo,
            V << todo,
            V << 'string.bytes'(Username),
            V << todo,
            V << 'string.bytes'(Password),
            'Connection.write'(Con, V),
            Resp = [],
            'Connection.read'(Con, Resp),
            case lists:nth(1, Resp) != 1 of
                true -> begin
                    'Connection.close'(Con),
                    error(<<"server does not support user/password version 1">>)
                end;
                false -> case lists:nth(2, Resp) != 0 of
                    true -> begin
                        'Connection.close'(Con),
                        error(<<"user/password login failed">>)
                    end;
                    false -> ok
                end
            end
        end;
        false -> ok
    end,
    '[]u8.clear'(V),
    V1 = [todo, 1, 0],
    Port = 'string.u64'('string.all_after_last'(Host, <<":">>)),
    case Port == 0 of
        true -> ok;
        false -> ok
    end,
    Address = 'string.all_before_last'(Host, <<":">>),
    case 'string.contains_only'(Address, <<".1234567890">>) of
        true -> begin
            V1 << todo,
            V1 << parse_ipv4(Address)
        end;
        false -> case 'string.contains_only'(Address, <<":1234567890abcdf">>) of
            true -> ok;
            false -> case length(Address) > 255 of
                true -> error(<<(Address)/binary, " is too long">>);
                false -> begin
                    V1 << todo,
                    V1 << todo,
                    V1 << 'string.bytes'(Address)
                end
            end
        end
    end,
    V1 << todo,
    V1 << todo,
    'Connection.write'(Con, V1),
    Bff = [],
    'Connection.read'(Con, Bff),
    case lists:nth(2, Bff) != 0 of
        true -> begin
            'Connection.close'(Con),
            error(reply(lists:nth(2, Bff)))
        end;
        false -> ok
    end,
    Con.

reply(Code) ->
    case Code of
        0 -> <<"succeeded">>;
        1 -> <<"general SOCKS server failure">>;
        2 -> <<"connection not allowed by ruleset">>;
        3 -> <<"network unreachable">>;
        4 -> <<"host unreachable">>;
        5 -> <<"connection refused">>;
        6 -> <<"TTL expired">>;
        7 -> <<"command not supported">>;
        8 -> <<"address type not supported">>;
        _ -> <<"unknown code: ", (integer_to_binary(Code))/binary>>
    end.

parse_ipv4(Addr) ->
    Ip = [],
    lists:foreach(fun(Part) ->
        Ip << 'string.u8'(Part),
        ok
    end, 'string.split'(Addr, <<".">>)),
    Ip.
