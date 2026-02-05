-module('v.main').
-export([add_vectors_mmx/3, main/0]).

add_vectors_mmx(A, B, Result) ->
    % TODO: {[unhandled stmt str type: v.ast.AsmStmt ];}

main() ->
    A = [todo, 2, 3, 4, 5, 6, 7, 8],
    B = [todo, 7, 6, 5, 4, 3, 2, 1],
    Result = [],
    add_vectors_mmx(&lists:nth(1, A), &lists:nth(1, B), &lists:nth(1, Result)),
    vbeam_io:println(Result),
    % TODO: assert result == [u8(9), 9, 9, 9, 9, 9, 9, 9]
