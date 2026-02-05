-module('v.math.vec').
-export([vec4/4, 'Vec4.zero'/1, 'Vec4.one'/1, 'Vec4.copy'/1, 'Vec4.from'/2, 'Vec4.from_vec2'/2, 'Vec4.as_vec2'/1, 'Vec4.from_vec3'/2, 'Vec4.as_vec3'/1, 'Vec4.+'/2, 'Vec4.add'/2, 'Vec4.add_vec2'/2, 'Vec4.add_vec3'/2, 'Vec4.add_scalar'/2, 'Vec4.plus'/2, 'Vec4.plus_scalar'/2, 'Vec4.-'/2, 'Vec4.sub'/2, 'Vec4.sub_scalar'/2, 'Vec4.subtract'/2, 'Vec4.subtract_scalar'/2, 'Vec4.*'/2, 'Vec4.mul'/2, 'Vec4.mul_scalar'/2, 'Vec4.multiply'/2, 'Vec4.multiply_scalar'/2, 'Vec4./'/2, 'Vec4.div'/2, 'Vec4.div_scalar'/2, 'Vec4.divide'/2, 'Vec4.divide_scalar'/2, 'Vec4.magnitude'/1, 'Vec4.dot'/2, 'Vec4.cross_xyz'/2, 'Vec4.unit'/1, 'Vec4.perpendicular'/2, 'Vec4.project'/2, 'Vec4.eq'/2, 'Vec4.eq_epsilon'/2, 'Vec4.eq_approx'/3, 'Vec4.is_approx_zero'/2, 'Vec4.eq_scalar'/2, 'Vec4.distance'/2, 'Vec4.manhattan_distance'/2, 'Vec4.abs'/1, 'Vec4.clean'/2, 'Vec4.clean_tolerance'/2, 'Vec4.inv'/1, 'Vec4.normalize'/1, 'Vec4.sum'/1]).

vec4(X, Y, Z, W) ->
    #{x => X, y => Y, z => Z, w => W, {vbeam, type} => 'Vec4'}.

'Vec4.zero'(V) ->

'Vec4.one'(V) ->

'Vec4.copy'(V) ->
    #{x => maps:get(x, V), y => maps:get(y, V), z => maps:get(z, V), w => maps:get(w, V), {vbeam, type} => 'Vec4'}.

'Vec4.from'(V, U) ->

'Vec4.from_vec2'(V, U) ->

'Vec4.as_vec2'(V) ->
    #{ => maps:get(x, V),  => maps:get(y, V), {vbeam, type} => 'Vec2<U>'}.

'Vec4.from_vec3'(V, U) ->

'Vec4.as_vec3'(V) ->
    #{ => maps:get(x, V),  => maps:get(y, V),  => maps:get(z, V), {vbeam, type} => 'Vec3<U>'}.

'Vec4.+'(V, U) ->
    #{x => maps:get(x, V) + maps:get(x, U), y => maps:get(y, V) + maps:get(y, U), z => maps:get(z, V) + maps:get(z, U), w => maps:get(w, V) + maps:get(w, U), {vbeam, type} => 'Vec4'}.

'Vec4.add'(V, U) ->
    V + U.

'Vec4.add_vec2'(V, U) ->
    #{ => maps:get(x, V) + maps:get(x, U),  => maps:get(y, V) + maps:get(y, U),  => 0,  => 0, {vbeam, type} => 'Vec4'}.

'Vec4.add_vec3'(V, U) ->
    #{ => maps:get(x, V) + maps:get(x, U),  => maps:get(y, V) + maps:get(y, U),  => maps:get(z, V) + maps:get(z, U),  => 0, {vbeam, type} => 'Vec4'}.

'Vec4.add_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) + todo,  => maps:get(y, V) + todo,  => maps:get(z, V) + todo,  => maps:get(w, V) + todo, {vbeam, type} => 'Vec4'}.

'Vec4.plus'(V, U) ->

'Vec4.plus_scalar'(V, Scalar) ->

'Vec4.-'(V, U) ->
    #{x => maps:get(x, V) - maps:get(x, U), y => maps:get(y, V) - maps:get(y, U), z => maps:get(z, V) - maps:get(z, U), w => maps:get(w, V) - maps:get(w, U), {vbeam, type} => 'Vec4'}.

'Vec4.sub'(V, U) ->
    V - U.

'Vec4.sub_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) - todo,  => maps:get(y, V) - todo,  => maps:get(z, V) - todo,  => maps:get(w, V) - todo, {vbeam, type} => 'Vec4'}.

'Vec4.subtract'(V, U) ->

'Vec4.subtract_scalar'(V, Scalar) ->

'Vec4.*'(V, U) ->
    #{x => maps:get(x, V) * maps:get(x, U), y => maps:get(y, V) * maps:get(y, U), z => maps:get(z, V) * maps:get(z, U), w => maps:get(w, V) * maps:get(w, U), {vbeam, type} => 'Vec4'}.

'Vec4.mul'(V, U) ->
    V * U.

'Vec4.mul_scalar'(V, Scalar) ->
    #{x => maps:get(x, V) * todo, y => maps:get(y, V) * todo, z => maps:get(z, V) * todo, w => maps:get(w, V) * todo, {vbeam, type} => 'Vec4'}.

'Vec4.multiply'(V, U) ->

'Vec4.multiply_scalar'(V, Scalar) ->

'Vec4./'(V, U) ->
    #{x => maps:get(x, V) / maps:get(x, U), y => maps:get(y, V) / maps:get(y, U), z => maps:get(z, V) / maps:get(z, U), w => maps:get(w, V) / maps:get(w, U), {vbeam, type} => 'Vec4'}.

'Vec4.div'(V, U) ->
    V / U.

'Vec4.div_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) / todo,  => maps:get(y, V) / todo,  => maps:get(z, V) / todo,  => maps:get(w, V) / todo, {vbeam, type} => 'Vec4'}.

'Vec4.divide'(V, U) ->

'Vec4.divide_scalar'(V, Scalar) ->

'Vec4.magnitude'(V) ->
    case maps:get(x, V) == 0 && maps:get(y, V) == 0 && maps:get(z, V) == 0 && maps:get(w, V) == 0 of
        true -> todo;
        false -> ok
    end,
    todo.

'Vec4.dot'(V, U) ->
    todo.

'Vec4.cross_xyz'(V, U) ->
    #{x => (maps:get(y, V) * maps:get(z, U)) - (maps:get(z, V) * maps:get(y, U)), y => (maps:get(z, V) * maps:get(x, U)) - (maps:get(x, V) * maps:get(z, U)), z => (maps:get(x, V) * maps:get(y, U)) - (maps:get(y, V) * maps:get(x, U)), w => 0, {vbeam, type} => 'Vec4'}.

'Vec4.unit'(V) ->
    M = 'Vec4.magnitude'(V),
    #{x => maps:get(x, V) / M, y => maps:get(y, V) / M, z => maps:get(z, V) / M, w => maps:get(w, V) / M, {vbeam, type} => 'Vec4'}.

'Vec4.perpendicular'(V, U) ->
    V - 'Vec4.project'(V, U).

'Vec4.project'(V, U) ->
    Scale = todo,
    'Vec4.mul_scalar'(U, Scale).

'Vec4.eq'(V, U) ->
    maps:get(x, V) == maps:get(x, U) && maps:get(y, V) == maps:get(y, U) && maps:get(z, V) == maps:get(z, U) && maps:get(w, V) == maps:get(w, U).

'Vec4.eq_epsilon'(V, U) ->
    'Vec4.eq_approx'(V, U, todo).

'Vec4.eq_approx'(V, U, Tolerance) ->
    Diff_x = abs(maps:get(x, V) - maps:get(x, U)),
    Diff_y = abs(maps:get(y, V) - maps:get(y, U)),
    Diff_z = abs(maps:get(z, V) - maps:get(z, U)),
    Diff_w = abs(maps:get(w, V) - maps:get(w, U)),
    case Diff_x <= Tolerance && Diff_y <= Tolerance && Diff_z <= Tolerance && Diff_w <= Tolerance of
        true -> true;
        false -> ok
    end,
    Max_x = max(abs(maps:get(x, V)), abs(maps:get(x, U))),
    Max_y = max(abs(maps:get(y, V)), abs(maps:get(y, U))),
    Max_z = max(abs(maps:get(z, V)), abs(maps:get(z, U))),
    Max_w = max(abs(maps:get(w, V)), abs(maps:get(w, U))),
    case Diff_x < Max_x * Tolerance && Diff_y < Max_y * Tolerance && Diff_z < Max_z * Tolerance && Diff_w < Max_w * Tolerance of
        true -> true;
        false -> ok
    end,
    false.

'Vec4.is_approx_zero'(V, Tolerance) ->
    case abs(maps:get(x, V)) <= Tolerance && abs(maps:get(y, V)) <= Tolerance && abs(maps:get(z, V)) <= Tolerance && abs(maps:get(w, V)) <= Tolerance of
        true -> true;
        false -> ok
    end,
    false.

'Vec4.eq_scalar'(V, Scalar) ->
    maps:get(x, V) == Scalar && maps:get(y, V) == todo && maps:get(z, V) == todo && maps:get(w, V) == todo.

'Vec4.distance'(V, U) ->
    sqrt((maps:get(x, V) - maps:get(x, U)) * (maps:get(x, V) - maps:get(x, U)) + (maps:get(y, V) - maps:get(y, U)) * (maps:get(y, V) - maps:get(y, U)) + (maps:get(z, V) - maps:get(z, U)) * (maps:get(z, V) - maps:get(z, U)) + (maps:get(w, V) - maps:get(w, U)) * (maps:get(w, V) - maps:get(w, U))).

'Vec4.manhattan_distance'(V, U) ->
    abs(maps:get(x, V) - maps:get(x, U)) + abs(maps:get(y, V) - maps:get(y, U)) + abs(maps:get(z, V) - maps:get(z, U)) + abs(maps:get(w, V) - maps:get(w, U)).

'Vec4.abs'(V) ->
    case maps:get(x, V) < 0 of
        true -> ok;
        false -> ok
    end,
    case maps:get(y, V) < 0 of
        true -> ok;
        false -> ok
    end,
    case maps:get(z, V) < 0 of
        true -> ok;
        false -> ok
    end,
    case maps:get(w, V) < 0 of
        true -> ok;
        false -> ok
    end.

'Vec4.clean'(V, Tolerance) ->
    R = 'unknown.copy'(V),
    case abs(maps:get(x, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(y, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(z, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(w, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    R.

'Vec4.clean_tolerance'(V, Tolerance) ->
    case abs(maps:get(x, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(y, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(z, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(w, V)) < Tolerance of
        true -> ok;
        false -> ok
    end.

'Vec4.inv'(V) ->
    #{x => case maps:get(x, V) != 0 of
        true -> todo / maps:get(x, V);
        false -> 0
    end, y => case maps:get(y, V) != 0 of
        true -> todo / maps:get(y, V);
        false -> 0
    end, z => case maps:get(z, V) != 0 of
        true -> todo / maps:get(z, V);
        false -> 0
    end, w => case maps:get(w, V) != 0 of
        true -> todo / maps:get(w, V);
        false -> 0
    end, {vbeam, type} => 'Vec4'}.

'Vec4.normalize'(V) ->
    M = 'Vec4.magnitude'(V),
    case M == 0 of
        true -> vec4(0, 0, 0, 0);
        false -> ok
    end,
    #{x => maps:get(x, V) * (1 / M), y => maps:get(y, V) * (1 / M), z => maps:get(z, V) * (1 / M), w => maps:get(w, V) * (1 / M), {vbeam, type} => 'Vec4'}.

'Vec4.sum'(V) ->
    maps:get(x, V) + maps:get(y, V) + maps:get(z, V) + maps:get(w, V).
