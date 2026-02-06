-module('v.main').
-export([main/0]).

main() ->
    Conn = dial_tcp(<<"google.com:80">>),
    % TODO: unhandled stmt type
    ok    vbeam_io:println(<<" peer: ", ('TcpConn.peer_addr'(Conn))/binary>>),
    vbeam_io:println(<<"local: ", ('TcpConn.addr'(Conn))/binary>>),
    'TcpConn.write_string'(Conn, <<"HEAD /index.html HTTP/1.0\\r\\n\\r\\n">>),
    Result = read_all(#{reader => Conn, {vbeam, type} => 'ReadAllConfig'}),
    vbeam_io:println('[]u8.bytestr'(Result)),
    ok.
