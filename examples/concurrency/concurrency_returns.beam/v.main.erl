-module('v.main').
-export([expensive_computing/1, main/0]).

expensive_computing(I) ->
    I * I.

main() ->
    Threads = [],
    lists:foreach(fun(I) ->
        Threads << todo,
        ok
    end, lists:seq(1, 10 - 1)),
    R = '[]thread int.wait'(Threads),
    vbeam_io:println(<<"All jobs finished: ", (R)/binary>>),
    ok.
