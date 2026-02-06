-module('v.main').
-export([main/0]).

main() ->
    Listener = listen_tcp(ip, <<"localhost:9001">>),
    % TODO: unhandled stmt type
    Addr = 'TcpListener.addr'(Listener),
    io:format(standard_error, "~s~n", [<<"Listening on ", (Addr)/binary>>]),
    io:format(standard_error, "~s~n", [<<"Type `stop` to stop the server">>]),
    Notifier = new(),
    % TODO: unhandled stmt type
    'FdNotifier.add'(Notifier, maps:get(fd, stdin()), read),
    'FdNotifier.add'(Notifier, maps:get(handle, maps:get(sock, Listener)), read),
    % TODO: unhandled stmt type
        ok.
