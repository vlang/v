-module('v.main').
-export([horizontal_add_sse3/3, main/0]).

horizontal_add_sse3(A, B, Result) ->
    % TODO: unhandled stmt type
        ok.

main() ->
    A = [todo, 2.0, 3.0, 4.0],
    B = [todo, 6.0, 7.0, 8.0],
    Result = [],
    horizontal_add_sse3(lists:nth(1, A), lists:nth(1, B), lists:nth(1, Result)),
    vbeam_io:println(Result),
    % TODO: unhandled stmt type
        ok.
