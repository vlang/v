-module('v.main').
-export([main/0, is_valid/4, find_empty/1, solve_sudoku/1, print_grid/2]).

main() ->
    Grid = [[0, 3, 0, 0, 7, 0, 0, 0, 0], [0, 0, 0, 1, 3, 5, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0, 5, 0], [1, 0, 0, 0, 6, 0, 0, 0, 3], [4, 0, 0, 8, 0, 3, 0, 0, 1], [7, 0, 0, 0, 2, 0, 0, 0, 6], [0, 0, 0, 0, 0, 0, 2, 1, 0], [0, 0, 0, 4, 1, 2, 0, 0, 5], [0, 0, 0, 0, 0, 0, 0, 7, 4]],
    print_grid(<<"Sudoku Puzzle:">>, Grid),
    io:format("~s~n", [<<"Solving...">>]),
    case solve_sudoku(Grid) of
        true -> print_grid(<<"Solution:">>, Grid);
        false -> begin
            io:format("~s~n", [<<"No solution exists.">>]),
            exit(1)
        end
    end.

is_valid(Grid, Row, Col, Num) ->
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    Start_row = Row - Row % 3,
    Start_col = Col - Col % 3,
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    true.

find_empty(Grid) ->
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    todo.

solve_sudoku(Grid) ->
    Row = element(1, find_empty(Grid)),
    Col = element(2, find_empty(Grid)),
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
    false.

print_grid(Label, Grid) ->
    vbeam_io:println(Label),
    % TODO: [unhandled stmt str type: v.ast.ForCStmt ]
