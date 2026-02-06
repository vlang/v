-module('v.main').
-export([round_floats_sse4_1/2, main/0]).

round_floats_sse4_1(A, Result) ->
    % TODO: unhandled stmt type
    ok
main() ->
    A = [todo, 2.5, 3.8, 4.4],
    Result = [],
    round_floats_sse4_1(lists:nth(1, A), lists:nth(1, Result)),
    vbeam_io:println(Result),
    % TODO: unhandled stmt type
    ok