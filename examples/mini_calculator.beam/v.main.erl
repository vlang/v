-module('v.main').
-export([expr_to_rev_pol/1, eval_rev_pol/1, is_num_string/1, main/0]).

expr_to_rev_pol(Expr) ->
    case Expr == <<"">> of
        true -> error(<<"err: empty expression">>);
        false -> begin
            Stack = [],
            Rev_pol = [],
            Pos = 0,
            % TODO: unhandled stmt type
            % TODO: unhandled stmt type
            Rev_pol
        end
        end.

eval_rev_pol(Rev_pol) ->
    Stack = [],
    lists:foreach(fun(Item) ->
        case is_num_string(Item) of
            true -> Stack bsl binary_to_float(Item);
            false -> case length(Stack) >= 2 of
                true -> begin
                    Oprand_r = lists:last(Stack),
                    lists:delete(length(Stack) - 1, Stack),
                    Oprand_l = lists:last(Stack),
                    lists:delete(length(Stack) - 1, Stack),
                    case Item of
                        <<"+">> -> Stack bsl Oprand_l + Oprand_r;
                        <<"-">> -> Stack bsl Oprand_l - Oprand_r;
                        <<"*">> -> Stack bsl Oprand_l * Oprand_r;
                        <<"/">> -> begin
                            case Oprand_r == 0 of
                                true -> error(<<"err: divide by zero">>);
                                false -> ok
                            end,
                            Stack bsl Oprand_l / Oprand_r
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
        case (not lists:member(C, [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo])) of
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
    % TODO: unhandled stmt type
        ok.
