-module('v.main').
-export([multiply_vectors_sse2/3, main/0]).

multiply_vectors_sse2(A, B, Result) ->
    % TODO: {[unhandled stmt str type: v.ast.AsmStmt ];}

main() ->
    A = [todo, 2.5],
    B = [todo, 4.5],
    Result = [],
    multiply_vectors_sse2(&lists:nth(1, A), &lists:nth(1, B), &lists:nth(1, Result)),
    vbeam_io:println(Result),
    % TODO: assert result == [f64(5.25), 11.25]
