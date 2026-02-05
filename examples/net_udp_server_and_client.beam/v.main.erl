-module('v.main').
-export([main/0]).

main() ->
    io:format("~s~n", [<<"Usage: net_udp_server_and_client [-l] [-p 5000]">>]),
    io:format("~s~n", [<<"     -l      - act as a server and listen">>]),
    io:format("~s~n", [<<"     -p XXXX - custom port number">>]),
    io:format("~s~n", [<<"------------------------------------------">>]),
    Is_server = <<"-l">> in arguments(),
    Port = 'string.int'(option(arguments(), <<"-p">>, <<"40001">>)),
    Buf = [],
    case Is_server of
        true -> begin
            vbeam_io:println(<<"UDP echo server, listening for udp packets on port: ", (integer_to_binary(Port))/binary>>),
            C = listen_udp(<<"0.0.0.0:", (integer_to_binary(Port))/binary>>),
            % TODO: for {
        end;
        false -> begin
            vbeam_io:println(<<"UDP client, sending packets to port: ", (integer_to_binary(Port))/binary, ".\\nType `exit` to exit.">>),
            C1 = dial_udp(<<"127.0.0.1:", (integer_to_binary(Port))/binary>>),
            % TODO: for {
        end
    end.
