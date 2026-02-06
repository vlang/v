-module('v.main').
-export([main/0]).

main() ->
    lists:foreach(fun(_) ->
        vbeam_io:println(<<(integer_to_binary(intn(255)))/binary, ".", (integer_to_binary(intn(255)))/binary, ".", (integer_to_binary(intn(255)))/binary, ".", (integer_to_binary(intn(255)))/binary>>),
        ok.
        ok
    end, lists:seq(0, 10 - 1)),
        ok.
