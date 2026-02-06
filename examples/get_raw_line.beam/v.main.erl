-module('v.main').
-export([main/0]).

main() ->
    io:format("~s~n", [<<"Press Ctrl+D(Linux) or Ctrl+Z(Windows) at line begin to exit">>]),
    I = 0,
    % TODO: unhandled stmt type
    ok