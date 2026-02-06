-module('v.main').
-export([main/0]).

main() ->
    lists:foreach(fun(I) ->
        vbeam_io:println(<<"Hello from V.js (", (integer_to_binary(I))/binary, ")">>),
        ok.
        ok
    end, lists:seq(0, 3 - 1)),
        ok.
