-module('v.main').
-export([main/0]).

main() ->
    N = 'string.int'(lists:nth(2, arguments())),
    Sz = case N < 15 of
        true -> 50;
        false -> begin
            Ln = log(todo),
            todo + 1
        end
    end,
    Sieve = [],
    % TODO: unhandled stmt type
    ok    C = 0,
    % TODO: unhandled stmt type
    ok