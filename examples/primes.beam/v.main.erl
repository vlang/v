-module('v.main').
-export([main/0]).

main() ->
    N = binary_to_integer(lists:nth(2, init:get_plain_arguments())),
    Sz = case N < 15 of
        true -> 50;
        false -> begin
            Ln = log(todo),
            todo + 1
        end
    end,
    Sieve = [],
    % TODO: unhandled stmt type
    C = 0,
    % TODO: unhandled stmt type
        ok.
