-module('v.main').
-export([advance/2, offsetmomentum/1, energy/1, arr_momentum/0, arr_position/0, main/0]).

advance(Sys, Dt) ->
    {Vx, Vy, Vz} = lists:foldl(fun(I, {VxAcc, VyAcc, VzAcc}) ->
        VxOut = maps:get(x, lists:nth(I + 1, maps:get(v, Sys))),
        VyOut = maps:get(y, lists:nth(I + 1, maps:get(v, Sys))),
        VzOut = maps:get(z, lists:nth(I + 1, maps:get(v, Sys))),
        % TODO: unhandled stmt type
        ok        {VxOut, VyOut, VzOut}
    end, {Vx, Vy, Vz}, lists:seq(0, 5 - 1 - 1)),
    lists:foreach(fun(I) ->
        ok
    end, lists:seq(0, 5 - 1)),

offsetmomentum(Sys) ->
    Px = todo,
    Py = todo,
    Pz = todo,
    {Px1, Py1, Pz1} = lists:foldl(fun(I, {PxAcc, PyAcc, PzAcc}) ->
        PxOut = maps:get(x, lists:nth(I + 1, maps:get(v, Sys))) * maps:get(m, lists:nth(I + 1, maps:get(v, Sys))),
        PyOut = maps:get(y, lists:nth(I + 1, maps:get(v, Sys))) * maps:get(m, lists:nth(I + 1, maps:get(v, Sys))),
        PzOut = maps:get(z, lists:nth(I + 1, maps:get(v, Sys))) * maps:get(m, lists:nth(I + 1, maps:get(v, Sys))),
        {PxOut, PyOut, PzOut}
    end, {Px, Py, Pz}, lists:seq(0, 5 - 1)),

energy(Sys) ->
    E = todo,
    E1 = lists:foldl(fun(I, EAcc) ->
        EOut = todo * maps:get(m, lists:nth(I + 1, maps:get(v, Sys))) * (maps:get(x, lists:nth(I + 1, maps:get(v, Sys))) * maps:get(x, lists:nth(I + 1, maps:get(v, Sys))) + maps:get(y, lists:nth(I + 1, maps:get(v, Sys))) * maps:get(y, lists:nth(I + 1, maps:get(v, Sys))) + maps:get(z, lists:nth(I + 1, maps:get(v, Sys))) * maps:get(z, lists:nth(I + 1, maps:get(v, Sys)))),
        % TODO: unhandled stmt type
        ok        EOut
    end, E, lists:seq(0, 5 - 1)),
    E1.

arr_momentum() ->
    [#{x => 0.0, y => 0.0, z => 0.0, m => 39.47841760435743197, {vbeam, type} => 'Momentum'}, #{x => 0.606326392995832, y => 2.81198684491626, z => -0.02521836165988763, m => 0.03769367487038949, {vbeam, type} => 'Momentum'}, #{x => -1.0107743461787924, y => 1.8256623712304119, z => 0.008415761376584154, m => 0.011286326131968767, {vbeam, type} => 'Momentum'}, #{x => 1.0827910064415354, y => 0.8687130181696082, z => -0.010832637401363636, m => 0.0017237240570597112, {vbeam, type} => 'Momentum'}, #{x => 0.979090732243898, y => 0.5946989986476762, z => -0.034755955504078104, m => 0.0020336868699246304, {vbeam, type} => 'Momentum'}].

arr_position() ->
    [#{x => 0.0, y => 0.0, z => 0.0, {vbeam, type} => 'Position'}, #{x => 4.84143144246472090e+00, y => -1.16032004402742839e+00, z => -1.03622044471123109e-01, {vbeam, type} => 'Position'}, #{x => 8.34336671824457987e+00, y => 4.12479856412430479e+00, z => -4.03523417114321381e-01, {vbeam, type} => 'Position'}, #{x => 1.28943695621391310e+01, y => -1.51111514016986312e+01, z => -2.23307578892655734e-01, {vbeam, type} => 'Position'}, #{x => 1.53796971148509165e+01, y => -2.59193146099879641e+01, z => 1.79258772950371181e-01, {vbeam, type} => 'Position'}].

main() ->
    Sys = #{v => arr_momentum(), s => arr_position(), {vbeam, type} => 'System'},
    offsetmomentum(Sys),
    vbeam_io:println(float_to_binary(energy(Sys))),
    lists:foreach(fun(_) ->
        advance(Sys, 0.01),
        ok
    end, lists:seq(0, 50000000 - 1)),
    vbeam_io:println(float_to_binary(energy(Sys))),
    ok.
