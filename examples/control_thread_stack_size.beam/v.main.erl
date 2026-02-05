-module('v.main').
-export([main/0, my_print1/0, my_print2/0]).

main() ->
    Th = todo,
    'thread.wait'(Th),
    Th2 = todo,
    'thread.wait'(Th2),
    ok.

my_print1() ->
    io:format("~s~n", [<<"hello word">>]),
    ok.

my_print2() ->
    io:format("~s~n", [<<"ahoj svet">>]),
    ok.
