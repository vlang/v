-module('v.main').
-export(['Mars.dust_storm'/1, 'Lander.deorbit'/1, 'Lander.open_parachutes'/2, wait/0, 'Lander.land'/2, main/0]).

'Mars.dust_storm'(M) ->
    int() >= 0.

'Lander.deorbit'(L) ->
    io:format("~s~n", [<<"leaving orbit">>]),
    ok.

'Lander.open_parachutes'(L, N) ->
    vbeam_io:println(<<"opening ", (integer_to_binary(N))/binary, " parachutes">>),
    ok.

wait() ->
    io:format("~s~n", [<<"waiting...">>]),
    timer:sleep(1 * todo),
    ok.

'Lander.land'(L, W) ->
    case W is todo of
        true -> ok;
        false -> ok
    end,
    'Lander.deorbit'(L),
    case W of
        todo -> ok;
        todo -> 'Lander.open_parachutes'(L, 3);
        todo -> 'Lander.open_parachutes'(L, 1)
    end,
    io:format("~s~n", [<<"landed">>]),
    ok.

main() ->
    L = #{{vbeam, type} => 'Lander'},
    'Lander.land'(L, #{{vbeam, type} => 'Venus'}),
    'Lander.land'(L, #{{vbeam, type} => 'Mars'}),
    ok.
