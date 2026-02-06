-module('v.main').
-export(['Parser.expr'/1, 'Parser.term'/1, 'Parser.factor'/1, 'Parser.number'/1, main/0]).

'Parser.expr'(P) ->
    Result = 'Parser.term'(P),
    % TODO: unhandled stmt type
    Result.

'Parser.term'(P) ->
    Result = 'Parser.factor'(P),
    % TODO: unhandled stmt type
    Result.

'Parser.factor'(P) ->
    'Parser.skip_whitespace'(P),
    C = 'Parser.peek_u8'(P),
    case 'u8.is_digit'(C) of
        true -> 'Parser.number'(P);
        false -> case C == todo of
            true -> begin
                'Parser.next'(P),
                Result = 'Parser.expr'(P),
                'Parser.skip_whitespace'(P),
                case 'Parser.next'(P) /= todo of
                    true -> error(<<"Expected closing parenthesis">>);
                    false -> ok
                end,
                Result
            end;
            false -> ok
        end
    end,
    error(<<"Expected number or opening parenthesis">>).

'Parser.number'(P) ->
    Start = maps:get(pos, P),
    % TODO: unhandled stmt type
    atof64(lists:nth(todo + 1, maps:get(input, P)), #{{vbeam, type} => 'AtoF64Param'}).

main() ->
    io:format("~s~n", [<<"Enter expressions to calculate, e.g. `2 * (5-1)` or `exit` to quit.">>]),
    % TODO: unhandled stmt type
        ok.
