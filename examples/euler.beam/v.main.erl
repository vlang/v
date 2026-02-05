-module('v.main').
-export([gauss_sum/1, main/0]).

gauss_sum(N) ->
    todo.

main() ->
    Gs = gauss_sum(1000),
    vbeam_io:println(<<"O(1) arithmetic progression sum: ", (integer_to_binary(gs(3) + gs(5) - gs(15)))/binary>>),
    Sum = 0,
    lists:foreach(fun(N) ->
        case N % 3 == 0 || N % 5 == 0 of
            true -> ok;
            false -> ok
        end,
        ok
    end, lists:seq(1, 1000 - 1)),
    vbeam_io:println(<<"O(n) brute force calculated sum: ", (integer_to_binary(Sum))/binary>>),
    ok.
