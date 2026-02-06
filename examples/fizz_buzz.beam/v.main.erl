-module('v.main').
-export([main/0]).

main() ->
    lists:foreach(fun(N) ->
        vbeam_io:println(if
            N rem 15 == 0 -> <<"FizzBuzz">>;
            N rem 5 == 0 -> <<"Buzz">>;
            N rem 3 == 0 -> <<"Fizz">>;
            true -> integer_to_binary(N)
        end),
        ok.
        ok
    end, lists:seq(1, 101 - 1)),
