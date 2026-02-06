-module('v.main').
-export([main/0]).

main() ->
    Listener = listen_tcp(ip, <<"localhost:9001">>),
    % TODO: unhandled stmt type
    ok    Addr = 'TcpListener.addr'(Listener),
    eprintln(<<"Listening on ", (Addr)/binary>>),
    eprintln(<<"Type `stop` to stop the server">>),
    Notifier = new(),
    % TODO: unhandled stmt type
    ok    'FdNotifier.add'(Notifier, maps:get(fd, stdin()), read),
    'FdNotifier.add'(Notifier, maps:get(handle, maps:get(sock, Listener)), read),
    % TODO: unhandled stmt type
    ok