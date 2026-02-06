-module('v.main').
-export([main/0]).

main() ->
    Conn = dial_tcp(<<"google.com:80">>),
    'TcpConn.write_string'(Conn, <<"GET /index.html HTTP/1.0\\r\\n\\r\\n">>),
    R = new_buffered_reader(#{reader => Conn, {vbeam, type} => 'BufferedReaderConfig'}),
    % TODO: unhandled stmt type
    ok