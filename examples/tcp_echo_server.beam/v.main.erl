-module('v.main').
-export([main/0, handle_client/1]).

main() ->
    Server = listen_tcp(ip6, <<":12345">>),
    Laddr = 'TcpListener.addr'(Server),
    io:format(standard_error, "~s~n", [<<"Listen on ", (Laddr)/binary, " ...">>]),
    % TODO: unhandled stmt type
        ok.

handle_client(Socket) ->
    % TODO: unhandled stmt type
    Client_addr = 'TcpConn.peer_addr'(Socket),
    io:format(standard_error, "~s~n", [<<"> new client: ", (Client_addr)/binary>>]),
    Reader = new_buffered_reader(#{reader => Socket, {vbeam, type} => 'BufferedReaderConfig'}),
    % TODO: unhandled stmt type
    'TcpConn.write_string'(Socket, <<"server: hello\\n">>),
    % TODO: unhandled stmt type
        ok.
