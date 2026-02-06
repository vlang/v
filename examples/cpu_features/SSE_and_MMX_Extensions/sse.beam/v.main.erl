-module('v.main').
-export([add_vectors_sse/3, main/0]).

add_vectors_sse(A, B, Result) ->
    % TODO: unhandled stmt type
    ok
main() ->
    A = [todo, 2.0, 3.0, 4.0],
    B = [todo, 3.0, 2.0, 1.0],
    Result = [],
    add_vectors_sse(lists:nth(1, A), lists:nth(1, B), lists:nth(1, Result)),
    vbeam_io:println(Result),
    % TODO: unhandled stmt type
    ok