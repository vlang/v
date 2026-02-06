-module('v.main').
-export([filter/2, uppercase/1, main/0]).

filter(S, F) ->
    f(S).

uppercase(S) ->
    'string.to_upper'(S).

main() ->
    My_filter = todo,
    vbeam_io:println(filter(<<"Hello world">>, My_filter)),
    vbeam_io:println(filter(<<"Hello world">>, Main.uppercase)),
    vbeam_io:println(filter(<<"Hello world">>, todo)),
    ok.
