-module('v.main').
-export([main/0]).

main() ->
    Conn = dial_tcp(<<"google.com:80">>),
    Peer_addr = 'TcpConn.peer_addr'(Conn),
    vbeam_io:println(Peer_addr),
    ok.
