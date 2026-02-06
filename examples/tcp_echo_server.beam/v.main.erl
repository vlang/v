-module('v.main').
-export([main/0, handle_client/1]).

main() ->
    Server = listen_tcp(ip6, <<":12345">>),
    Laddr = 'TcpListener.addr'(Server),
    eprintln(<<"Listen on ", (Laddr)/binary, " ...">>),
    % TODO: unhandled stmt type
    ok
handle_client(Socket) ->
    % TODO: unhandled stmt type
    ok    Client_addr = 'TcpConn.peer_addr'(Socket),
    eprintln(<<"> new client: ", (Client_addr)/binary>>),
    Reader = new_buffered_reader(#{reader => Socket, {vbeam, type} => 'BufferedReaderConfig'}),
    % TODO: unhandled stmt type
    ok    'TcpConn.write_string'(Socket, <<"server: hello\\n">>),
    % TODO: unhandled stmt type
    ok