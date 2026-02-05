-module('v.main').
-export([evala/2, times/2, times_trans/2, a_times_transp/2, main/0]).

evala(I, J) ->
    (I + J) * (I + J + 1) / 2 + I + 1.

times(V, U) ->
    A = lists:foldl(fun(I, AAcc) ->
        AOut = todo,
        A1 = lists:foldl(fun(J, AAcc) ->
            AOut = lists:nth(J + 1, U) / todo,
            AOut
        end, A, lists:seq(0, length(U) - 1)),
        AOut
    end, A, lists:seq(0, length(V) - 1)),

times_trans(V, U) ->
    A = lists:foldl(fun(I, AAcc) ->
        AOut = todo,
        A1 = lists:foldl(fun(J, AAcc) ->
            AOut = lists:nth(J + 1, U) / todo,
            AOut
        end, A, lists:seq(0, length(U) - 1)),
        AOut
    end, A, lists:seq(0, length(V) - 1)),

a_times_transp(V, U) ->
    X = [],
    times(X, U),
    times_trans(V, X),
    ok.

main() ->
    N = 0,
    case length(arguments()) == 2 of
        true -> ok;
        false -> ok
    end,
    U = [],
    V = [],
    lists:foreach(fun(_) ->
        a_times_transp(V, U),
        a_times_transp(U, V),
        ok
    end, lists:seq(0, 10 - 1)),
    Vbv = todo,
    Vv = todo,
    {Vbv1, Vv1} = lists:foldl(fun(I, {VbvAcc, VvAcc}) ->
        VbvOut = lists:nth(I + 1, U) * lists:nth(I + 1, V),
        VvOut = lists:nth(I + 1, V) * lists:nth(I + 1, V),
        {VbvOut, VvOut}
    end, {Vbv, Vv}, lists:seq(0, N - 1)),
    Ans = sqrt(Vbv1 / Vv1),
    vbeam_io:println(float_to_binary(Ans)),
    ok.
