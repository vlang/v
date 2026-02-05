-module('v.main').
-export([main/0]).

main() ->
    io:format("~s~n", [<<"Hello, World!">>]),
    ok.
