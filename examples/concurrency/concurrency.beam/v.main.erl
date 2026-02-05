-module('v.main').
-export([expensive_computing/2, main/0]).

expensive_computing(Id, Duration) ->
    vbeam_io:println(<<"Executing expensive computing task (", (integer_to_binary(Id))/binary, ")...">>),
    sleep(Duration * todo),
    vbeam_io:println(<<"Finish task ", (integer_to_binary(Id))/binary, " on ", (integer_to_binary(Duration))/binary, " ms">>),
    ok.

main() ->
    Threads = [],
    Threads << todo,
    Threads << todo,
    Threads << todo,
    '[]thread.wait'(Threads),
    io:format("~s~n", [<<"All jobs finished!">>]),
    ok.
