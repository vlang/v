-module('v.main').
-export([main/0, move/3, hanoi/4]).

main() ->
    hanoi(7, <<"A">>, <<"B">>, <<"C">>),
    ok.

move(N, A, B) ->
    vbeam_io:println(<<"Disc ", (integer_to_binary(N))/binary, " from ", (A)/binary, " to ", (B)/binary, "...">>),
    ok.

hanoi(N, A, B, C) ->
    case N == 1 of
        true -> move(1, A, C);
        false -> begin
            hanoi(N - 1, A, C, B),
            move(N, A, C),
            hanoi(N - 1, B, A, C)
        end
    end.
