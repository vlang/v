-module('v.main').
-export([psignw_example/3, main/0]).

psignw_example(A, B, Result) ->
    % TODO: unhandled stmt type
    ok
main() ->
    A0 = [todo, -2, 3, -4, 5, -6, 7, -8],
    B0 = [todo, -1, 1, -1, 1, -1, 1, -1],
    Result0 = [],
    psignw_example(lists:nth(1, A0), lists:nth(1, B0), lists:nth(1, Result0)),
    todo,
    % TODO: unhandled stmt type
    ok