-module('v.net.mbedtls').
-export([new_sslcerts/0, new_sslcerts_in_memory/3, new_sslcerts_from_file/3, 'SSLCerts.cleanup'/1, new_ssl_listener/2, 'SSLListener.shutdown'/1, 'SSLListener.accept'/1, new_ssl_conn/1, 'SSLConn.close'/1, 'SSLConn.shutdown'/1, 'SSLConn.init'/1, 'SSLConn.connect'/3, 'SSLConn.dial'/3, 'SSLConn.addr'/1, 'SSLConn.peer_addr'/1, 'SSLConn.socket_read_into_ptr'/3, 'SSLConn.read'/2, 'SSLConn.write_ptr'/3, 'SSLConn.write'/2, 'SSLConn.write_string'/2]).
% TODO: const mbedtls_client_read_timeout_ms = $d('mbedtls_client_read_timeout_ms', 10000);
% TODO: const mbedtls_server_read_timeout_ms = $d('mbedtls_server_read_timeout_ms', 41000);

new_sslcerts() ->
    &#{{vbeam, type} => 'SSLCerts'}.

new_sslcerts_in_memory(Verify, Cert, Cert_key) ->
    Certs = new_sslcerts(),
    case Verify != <<"">> of
        true -> ok;
        false -> ok
    end,
    case Cert != <<"">> of
        true -> ok;
        false -> ok
    end,
    case Cert_key != <<"">> of
        true -> ok;
        false -> ok
    end,
    Certs.

new_sslcerts_from_file(Verify, Cert, Cert_key) ->
    error(<<"net.mbedtls new_sslcerts_from_file not yet implemented for BEAM backend">>).

'SSLCerts.cleanup'(C) ->

new_ssl_listener(Saddr, Config) ->
    error(<<"net.mbedtls new_ssl_listener not yet implemented for BEAM backend">>).

'SSLListener.shutdown'(L) ->
    case todo of
        true -> 'SSLCerts.cleanup'(maps:get(certs, L));
        false -> ok
    end,
    ok.

'SSLListener.accept'(L) ->
    error(<<"net.mbedtls SSLListener.accept not yet implemented for BEAM backend">>).

new_ssl_conn(Config) ->
    Conn = &#{config => Config, {vbeam, type} => 'SSLConn'},
    'SSLConn.init'(Conn),
    Conn.

'SSLConn.close'(S) ->
    'SSLConn.shutdown'(S),
    ok.

'SSLConn.shutdown'(S) ->
    case !maps:get(opened, S) of
        true -> error(<<"net.mbedtls SSLConn.shutdown, connection was not open">>);
        false -> ok
    end,
    case todo of
        true -> 'SSLCerts.cleanup'(maps:get(certs, S));
        false -> ok
    end,
    case maps:get(owns_socket, S) of
        true -> begin
            shutdown(maps:get(handle, S)),
            close(maps:get(handle, S))
        end;
        false -> ok
    end,
    ok.

'SSLConn.init'(S) ->
    case maps:get(verify, maps:get(config, S)) != <<"">> || maps:get(cert, maps:get(config, S)) != <<"">> || maps:get(cert_key, maps:get(config, S)) != <<"">> of
        true -> ok;
        false -> ok
    end,
    case maps:get(in_memory_verification, maps:get(config, S)) of
        true -> begin
            case maps:get(verify, maps:get(config, S)) != <<"">> of
                true -> ok;
                false -> ok
            end,
            case maps:get(cert, maps:get(config, S)) != <<"">> of
                true -> ok;
                false -> ok
            end,
            case maps:get(cert_key, maps:get(config, S)) != <<"">> of
                true -> ok;
                false -> ok
            end
        end;
        false -> ok
    end,
    ok.

'SSLConn.connect'(S, Tcp_conn, Hostname) ->
    case maps:get(opened, S) of
        true -> error(<<"net.mbedtls SSLConn.connect, ssl connection was already open">>);
        false -> ok
    end,
    error(<<"net.mbedtls SSLConn.connect not yet implemented for BEAM backend">>).

'SSLConn.dial'(S, Hostname, Port) ->
    case maps:get(opened, S) of
        true -> error(<<"net.mbedtls SSLConn.dial, the ssl connection was already open">>);
        false -> ok
    end,
    error(<<"net.mbedtls SSLConn.dial not yet implemented for BEAM backend">>).

'SSLConn.addr'(S) ->
    #{{vbeam, type} => 'Addr'}.

'SSLConn.peer_addr'(S) ->
    #{{vbeam, type} => 'Addr'}.

'SSLConn.socket_read_into_ptr'(S, Buf_ptr, Len) ->
    todo.

'SSLConn.read'(S, Buffer) ->
    'SSLConn.socket_read_into_ptr'(S, todo, length(Buffer)).

'SSLConn.write_ptr'(S, Bytes, Len) ->
    error(<<"net.mbedtls SSLConn.write_ptr not yet implemented for BEAM backend">>).

'SSLConn.write'(S, Bytes) ->
    'SSLConn.write_ptr'(S, todo, length(Bytes)).

'SSLConn.write_string'(S, Str) ->
    'SSLConn.write_ptr'(S, maps:get(str, Str), length(Str)).
