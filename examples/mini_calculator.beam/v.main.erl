-module('v.main').
-export([expr_to_rev_pol/1, eval_rev_pol/1, is_num_string/1, main/0]).
% TODO: const numeric_char = [`0`, `1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `.`, `e`, `E`];

expr_to_rev_pol(Expr) ->
    case Expr == <<"">> of
        true -> error(<<"err: empty expression">>);
        false -> ok
    end,
    Stack = [],
    Rev_pol = [],
    Pos = 0,
    % TODO: for pos < expr.len {
    % TODO: for stack.len > 0 {
    Rev_pol.

eval_rev_pol(Rev_pol) ->
    Stack = [],
    lists:foreach(fun(Item) ->
        case is_num_string(Item) of
            true -> Stack << 'string.f64'(Item);
            false -> case length(Stack) >= 2 of
                true -> begin
                    Oprand_r = '[]f64.last'(Stack),
                    '[]f64.delete'(Stack, length(Stack) - 1),
                    Oprand_l = '[]f64.last'(Stack),
                    '[]f64.delete'(Stack, length(Stack) - 1),
                    case Item of
                        <<"+">> -> Stack << Oprand_l + Oprand_r;
                        <<"-">> -> Stack << Oprand_l - Oprand_r;
                        <<"*">> -> Stack << Oprand_l * Oprand_r;
                        <<"/">> -> begin
                            case Oprand_r == 0 of
                                true -> error(<<"err: divide by zero">>);
                                false -> ok
                            end,
                            Stack << Oprand_l / Oprand_r
                        end;
                        _ -> ok
                    end
                end;
                false -> error(<<"err: invalid expression">>)
            end
        end,
        ok
    end, Rev_pol),
    lists:nth(1, Stack).

is_num_string(Str) ->
    lists:foreach(fun(C) ->
        case C !in [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo] of
            true -> false;
            false -> ok
        end,
        ok
    end, Str),
    true.

main() ->
    io:format("~s~n", [<<"Please enter the expression you want to calculate, e.g. 1e2+(3-2.5)*6/1.5 .">>]),
    io:format("~s~n", [<<"Enter 'exit' or 'EXIT' to quit.">>]),
    Expr_count = 0,
    % TODO: for {
