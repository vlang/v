-module('v.net').
-export(['Ip6.str'/1, 'Ip.str'/1, 'Addr.family'/1, 'Addr.port'/1, 'Addr.len'/1, 'Addr.str'/1, 'Socket.address'/1, 'UdpSocket.remote'/1, 'TcpConn.close'/1, 'TcpConn.read'/2, 'TcpConn.read_ptr'/3, 'TcpConn.write'/2, 'TcpConn.write_string'/2, 'TcpConn.peer_addr'/1, 'TcpConn.peer_ip'/1, 'TcpConn.addr'/1, 'TcpConn.str'/1, 'TcpConn.set_read_timeout'/2, 'TcpConn.set_write_timeout'/2, 'UdpConn.close'/1, 'UdpConn.read'/2, 'UdpConn.write'/2, 'UdpConn.write_to_ptr'/4, 'UdpConn.write_to'/3, 'UdpConn.write_to_string'/3, 'UdpConn.write_string'/2, 'UdpConn.str'/1, 'TcpListener.accept'/1, 'TcpListener.set_accept_timeout'/2, 'TcpListener.close'/1, 'TcpListener.addr'/1, 'TCPDialer.dial'/2, default_tcp_dialer/0, dial_tcp/1, listen_tcp/2, listen_udp/1, dial_udp/1, new_ip/2, new_ip6/2, resolve_addrs/3, resolve_addrs_fuzzy/2, error_code/0, set_blocking/2, shutdown/1, close/1, validate_port/1, split_address/1, 'Select__static__from'/1, 'SocketType__static__from'/1, 'AddrFamily__static__from'/1, 'SocketOption__static__from'/1, 'ShutdownDirection__static__from'/1]).

'Ip6.str'(A) ->
    <<"[::]:", (integer_to_binary(maps:get(port, A)))/binary>>.

'Ip.str'(A) ->
    <<(integer_to_binary(lists:nth(1, maps:get(addr, A))))/binary, ".", (integer_to_binary(lists:nth(2, maps:get(addr, A))))/binary, ".", (integer_to_binary(lists:nth(3, maps:get(addr, A))))/binary, ".", (integer_to_binary(lists:nth(4, maps:get(addr, A))))/binary, ":", (integer_to_binary(maps:get(port, A)))/binary>>.

'Addr.family'(A) ->
    todo.

'Addr.port'(A) ->
    case todo of
        ip -> ok;
        ip6 -> ok;
        unix -> error(<<"unix addr has no port">>);
        unspec -> error(<<"cannot find port for unspec addr family">>)
    end.

'Addr.len'(A) ->
    case 'Addr.family'(A) of
        ip -> todo + 2;
        ip6 -> todo + 2;
        unix -> todo + 2;
        _ -> 0
    end.

'Addr.str'(A) ->
    case todo of
        ip -> ok;
        ip6 -> ok;
        unix -> <<"<unix>">>;
        unspec -> <<"<.unspec>">>
    end.

'Socket.address'(S) ->
    #{{vbeam, type} => 'Addr'}.

'UdpSocket.remote'(S) ->
    case maps:get(has_r, S) of
        true -> maps:get(r, S);
        false -> todo
        end.

'TcpConn.close'(C) ->
    ok.

'TcpConn.read'(C, Buf) ->
    error(<<"TcpConn.read not implemented for BEAM backend">>).

'TcpConn.read_ptr'(C, Buf_ptr, Len) ->
    error(<<"TcpConn.read_ptr not implemented for BEAM backend">>).

'TcpConn.write'(C, Bytes) ->
    error(<<"TcpConn.write not implemented for BEAM backend">>).

'TcpConn.write_string'(C, S) ->
    error(<<"TcpConn.write_string not implemented for BEAM backend">>).

'TcpConn.peer_addr'(C) ->
    #{{vbeam, type} => 'Addr'}.

'TcpConn.peer_ip'(C) ->
    Addr = 'TcpConn.peer_addr'(C),
    'Addr.str'(Addr).

'TcpConn.addr'(C) ->
    #{{vbeam, type} => 'Addr'}.

'TcpConn.str'(C) ->
    <<"TcpConn{}">>.

'TcpConn.set_read_timeout'(C, T) ->

'TcpConn.set_write_timeout'(C, T) ->

'UdpConn.close'(C) ->
    ok.

'UdpConn.read'(C, Buf) ->
    error(<<"UdpConn.read not implemented for BEAM backend">>).

'UdpConn.write'(C, Buf) ->
    error(<<"UdpConn.write not implemented for BEAM backend">>).

'UdpConn.write_to_ptr'(C, Addr, B, Len) ->
    error(<<"UdpConn.write_to_ptr not implemented for BEAM backend">>).

'UdpConn.write_to'(C, Addr, Buf) ->
    error(<<"UdpConn.write_to not implemented for BEAM backend">>).

'UdpConn.write_to_string'(C, Addr, S) ->
    error(<<"UdpConn.write_to_string not implemented for BEAM backend">>).

'UdpConn.write_string'(C, S) ->
    error(<<"UdpConn.write_string not implemented for BEAM backend">>).

'UdpConn.str'(C) ->
    <<"UdpConn">>.

'TcpListener.accept'(L) ->
    error(<<"TcpListener.accept not implemented for BEAM backend">>).

'TcpListener.set_accept_timeout'(L, T) ->

'TcpListener.close'(C) ->
    ok.

'TcpListener.addr'(C) ->
    #{{vbeam, type} => 'Addr'}.

'TCPDialer.dial'(T, Address) ->
    dial_tcp(Address).

default_tcp_dialer() ->
    #{{vbeam, type} => 'TCPDialer'}.

dial_tcp(Address) ->
    error(<<"dial_tcp not implemented for BEAM backend">>).

listen_tcp(Family, Saddr) ->
    error(<<"listen_tcp not implemented for BEAM backend">>).

listen_udp(Laddr) ->
    error(<<"listen_udp not implemented for BEAM backend">>).

dial_udp(Raddr) ->
    error(<<"dial_udp not implemented for BEAM backend">>).

new_ip(Port, Addr) ->
    #{f => todo, addr => #{Ip => #{port => Port, {vbeam, type} => 'Ip'}, {vbeam, type} => 'AddrData'}, {vbeam, type} => 'Addr'}.

new_ip6(Port, Addr) ->
    #{f => todo, addr => #{Ip6 => #{port => Port, {vbeam, type} => 'Ip6'}, {vbeam, type} => 'AddrData'}, {vbeam, type} => 'Addr'}.

resolve_addrs(Addr, Family, Typ) ->
    error(<<"resolve_addrs not implemented for BEAM backend">>).

resolve_addrs_fuzzy(Addr, Typ) ->
    error(<<"resolve_addrs_fuzzy not implemented for BEAM backend">>).

error_code() ->
    0.

set_blocking(Handle, State) ->
    ok.

shutdown(Handle) ->
    0.

close(Handle) ->
    ok.

validate_port(Port) ->
    case Port >= 0 andalso Port =< 16#FFFF of
        true -> todo;
        false -> error_with_code(<<"net: port out of range">>, 5)
    end.

split_address(Addr) ->
    case todo of
        true -> begin
            Address = 'string.all_before_last'('string.all_after'(Addr, <<"[">>), <<"]">>),
            Port = binary_to_integer('string.all_after_last'(Addr, <<"]:">>)),
            P = validate_port(Port),
            Address
        end;
        false -> case todo of
            true -> case 'string.count'(Addr, <<":">>) == 2 andalso 'string.all_before_last'(Addr, <<"::">>) == <<"">> of
                true -> Addr;
                false -> begin
                    Address1 = 'string.all_before_last'(Addr, <<":">>),
                    Port1 = binary_to_integer('string.all_after_last'(Addr, <<":">>)),
                    P1 = validate_port(Port1),
                    Address1
                end
            end;
            false -> case todo of
                true -> begin
                    Address2 = 'string.all_before_last'(Addr, <<":">>),
                    P2 = validate_port(binary_to_integer('string.all_after_last'(Addr, <<":">>))),
                    Address2
                end;
                false -> Addr
            end
        end
    end.

'Select__static__from'(Input) ->
    error(<<"invalid value">>).

'SocketType__static__from'(Input) ->
    error(<<"invalid value">>).

'AddrFamily__static__from'(Input) ->
    error(<<"invalid value">>).

'SocketOption__static__from'(Input) ->
    error(<<"invalid value">>).

'ShutdownDirection__static__from'(Input) ->
    error(<<"invalid value">>).
