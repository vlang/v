-module('v.math.vec').
-export([vec2/2, 'Vec2.zero'/1, 'Vec2.one'/1, 'Vec2.copy'/1, 'Vec2.from'/2, 'Vec2.from_vec3'/2, 'Vec2.as_vec3'/1, 'Vec2.from_vec4'/2, 'Vec2.as_vec4'/1, 'Vec2.+'/2, 'Vec2.add'/2, 'Vec2.add_scalar'/2, 'Vec2.plus'/2, 'Vec2.plus_scalar'/2, 'Vec2.-'/2, 'Vec2.sub'/2, 'Vec2.sub_scalar'/2, 'Vec2.subtract'/2, 'Vec2.subtract_scalar'/2, 'Vec2.*'/2, 'Vec2.mul'/2, 'Vec2.mul_scalar'/2, 'Vec2.multiply'/2, 'Vec2.multiply_scalar'/2, 'Vec2./'/2, 'Vec2.div'/2, 'Vec2.div_scalar'/2, 'Vec2.divide'/2, 'Vec2.divide_scalar'/2, 'Vec2.magnitude'/1, 'Vec2.magnitude_x'/1, 'Vec2.magnitude_y'/1, 'Vec2.dot'/2, 'Vec2.cross'/2, 'Vec2.unit'/1, 'Vec2.perp_cw'/1, 'Vec2.perp_ccw'/1, 'Vec2.perpendicular'/2, 'Vec2.project'/2, 'Vec2.rotate_around_cw'/3, 'Vec2.rotate_around_ccw'/3, 'Vec2.eq'/2, 'Vec2.eq_epsilon'/2, 'Vec2.eq_approx'/3, 'Vec2.is_approx_zero'/2, 'Vec2.eq_scalar'/2, 'Vec2.distance'/2, 'Vec2.manhattan_distance'/2, 'Vec2.angle_between'/2, 'Vec2.angle_towards'/2, 'Vec2.angle'/1, 'Vec2.abs'/1, 'Vec2.clean'/2, 'Vec2.clean_tolerance'/2, 'Vec2.inv'/1, 'Vec2.normalize'/1, 'Vec2.sum'/1, vec3/3, 'Vec3.zero'/1, 'Vec3.one'/1, 'Vec3.copy'/1, 'Vec3.from'/2, 'Vec3.from_vec2'/2, 'Vec3.as_vec2'/1, 'Vec3.from_vec4'/2, 'Vec3.as_vec4'/1, 'Vec3.+'/2, 'Vec3.add'/2, 'Vec3.add_vec2'/2, 'Vec3.add_scalar'/2, 'Vec3.plus'/2, 'Vec3.plus_vec2'/2, 'Vec3.plus_scalar'/2, 'Vec3.-'/2, 'Vec3.sub'/2, 'Vec3.sub_scalar'/2, 'Vec3.subtract'/2, 'Vec3.subtract_scalar'/2, 'Vec3.*'/2, 'Vec3.mul'/2, 'Vec3.mul_scalar'/2, 'Vec3.multiply'/2, 'Vec3.multiply_scalar'/2, 'Vec3./'/2, 'Vec3.div'/2, 'Vec3.div_scalar'/2, 'Vec3.divide'/2, 'Vec3.divide_scalar'/2, 'Vec3.magnitude'/1, 'Vec3.dot'/2, 'Vec3.cross'/2, 'Vec3.unit'/1, 'Vec3.perpendicular'/2, 'Vec3.project'/2, 'Vec3.eq'/2, 'Vec3.eq_epsilon'/2, 'Vec3.eq_approx'/3, 'Vec3.is_approx_zero'/2, 'Vec3.eq_scalar'/2, 'Vec3.distance'/2, 'Vec3.manhattan_distance'/2, 'Vec3.angle_between'/2, 'Vec3.abs'/1, 'Vec3.clean'/2, 'Vec3.clean_tolerance'/2, 'Vec3.inv'/1, 'Vec3.normalize'/1, 'Vec3.sum'/1, vec4/4, 'Vec4.zero'/1, 'Vec4.one'/1, 'Vec4.copy'/1, 'Vec4.from'/2, 'Vec4.from_vec2'/2, 'Vec4.as_vec2'/1, 'Vec4.from_vec3'/2, 'Vec4.as_vec3'/1, 'Vec4.+'/2, 'Vec4.add'/2, 'Vec4.add_vec2'/2, 'Vec4.add_vec3'/2, 'Vec4.add_scalar'/2, 'Vec4.plus'/2, 'Vec4.plus_scalar'/2, 'Vec4.-'/2, 'Vec4.sub'/2, 'Vec4.sub_scalar'/2, 'Vec4.subtract'/2, 'Vec4.subtract_scalar'/2, 'Vec4.*'/2, 'Vec4.mul'/2, 'Vec4.mul_scalar'/2, 'Vec4.multiply'/2, 'Vec4.multiply_scalar'/2, 'Vec4./'/2, 'Vec4.div'/2, 'Vec4.div_scalar'/2, 'Vec4.divide'/2, 'Vec4.divide_scalar'/2, 'Vec4.magnitude'/1, 'Vec4.dot'/2, 'Vec4.cross_xyz'/2, 'Vec4.unit'/1, 'Vec4.perpendicular'/2, 'Vec4.project'/2, 'Vec4.eq'/2, 'Vec4.eq_epsilon'/2, 'Vec4.eq_approx'/3, 'Vec4.is_approx_zero'/2, 'Vec4.eq_scalar'/2, 'Vec4.distance'/2, 'Vec4.manhattan_distance'/2, 'Vec4.abs'/1, 'Vec4.clean'/2, 'Vec4.clean_tolerance'/2, 'Vec4.inv'/1, 'Vec4.normalize'/1, 'Vec4.sum'/1]).

vec2(X, Y) ->
    #{x => X, y => Y, {vbeam, type} => 'Vec2'}.

'Vec2.zero'(V) ->

'Vec2.one'(V) ->

'Vec2.copy'(V) ->
    #{x => maps:get(x, V), y => maps:get(y, V), {vbeam, type} => 'Vec2'}.

'Vec2.from'(V, U) ->

'Vec2.from_vec3'(V, U) ->

'Vec2.as_vec3'(V) ->
    #{x => maps:get(x, V), y => maps:get(y, V), z => 0, {vbeam, type} => 'Vec3'}.

'Vec2.from_vec4'(V, U) ->

'Vec2.as_vec4'(V) ->
    #{x => maps:get(x, V), y => maps:get(y, V), z => 0, w => 0, {vbeam, type} => 'Vec4'}.

'Vec2.+'(V, U) ->
    #{x => maps:get(x, V) + maps:get(x, U), y => maps:get(y, V) + maps:get(y, U), {vbeam, type} => 'Vec2'}.

'Vec2.add'(V, U) ->
    V + U.

'Vec2.add_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) + todo,  => maps:get(y, V) + todo, {vbeam, type} => 'Vec2'}.

'Vec2.plus'(V, U) ->

'Vec2.plus_scalar'(V, Scalar) ->

'Vec2.-'(V, U) ->
    #{x => maps:get(x, V) - maps:get(x, U), y => maps:get(y, V) - maps:get(y, U), {vbeam, type} => 'Vec2'}.

'Vec2.sub'(V, U) ->
    V - U.

'Vec2.sub_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) - todo,  => maps:get(y, V) - todo, {vbeam, type} => 'Vec2'}.

'Vec2.subtract'(V, U) ->

'Vec2.subtract_scalar'(V, Scalar) ->

'Vec2.*'(V, U) ->
    #{x => maps:get(x, V) * maps:get(x, U), y => maps:get(y, V) * maps:get(y, U), {vbeam, type} => 'Vec2'}.

'Vec2.mul'(V, U) ->
    V * U.

'Vec2.mul_scalar'(V, Scalar) ->
    #{x => maps:get(x, V) * todo, y => maps:get(y, V) * todo, {vbeam, type} => 'Vec2'}.

'Vec2.multiply'(V, U) ->

'Vec2.multiply_scalar'(V, Scalar) ->

'Vec2./'(V, U) ->
    #{x => maps:get(x, V) / maps:get(x, U), y => maps:get(y, V) / maps:get(y, U), {vbeam, type} => 'Vec2'}.

'Vec2.div'(V, U) ->
    V / U.

'Vec2.div_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) / todo,  => maps:get(y, V) / todo, {vbeam, type} => 'Vec2'}.

'Vec2.divide'(V, U) ->

'Vec2.divide_scalar'(V, Scalar) ->

'Vec2.magnitude'(V) ->
    case maps:get(x, V) == 0 andalso maps:get(y, V) == 0 of
        true -> todo;
        false -> 
        end.

'Vec2.magnitude_x'(V) ->
    todo.

'Vec2.magnitude_y'(V) ->
    todo.

'Vec2.dot'(V, U) ->
    (maps:get(x, V) * maps:get(x, U)) + (maps:get(y, V) * maps:get(y, U)).

'Vec2.cross'(V, U) ->
    (maps:get(x, V) * maps:get(y, U)) - (maps:get(y, V) * maps:get(x, U)).

'Vec2.unit'(V) ->
    M = 'Vec2.magnitude'(V),
    #{x => maps:get(x, V) / M, y => maps:get(y, V) / M, {vbeam, type} => 'Vec2'}.

'Vec2.perp_cw'(V) ->
    #{x => maps:get(y, V), y => -maps:get(x, V), {vbeam, type} => 'Vec2'}.

'Vec2.perp_ccw'(V) ->
    #{x => -maps:get(y, V), y => maps:get(x, V), {vbeam, type} => 'Vec2'}.

'Vec2.perpendicular'(V, U) ->
    V - 'Vec2.project'(V, U).

'Vec2.project'(V, U) ->
    Scale = todo,
    'Vec2.mul_scalar'(U, Scale).

'Vec2.rotate_around_cw'(V, O, Radians) ->
    'Vec2.rotate_around_ccw'(V, O, -Radians).

'Vec2.rotate_around_ccw'(V, O, Radians) ->
    S = sin(Radians),
    C = cos(Radians),
    Dx = maps:get(x, V) - maps:get(x, O),
    Dy = maps:get(y, V) - maps:get(y, O),
    #{x => todo, y => todo, {vbeam, type} => 'Vec2'}.

'Vec2.eq'(V, U) ->
    maps:get(x, V) == maps:get(x, U) andalso maps:get(y, V) == maps:get(y, U).

'Vec2.eq_epsilon'(V, U) ->
    'Vec2.eq_approx'(V, U, todo).

'Vec2.eq_approx'(V, U, Tolerance) ->
    Diff_x = abs(maps:get(x, V) - maps:get(x, U)),
    Diff_y = abs(maps:get(y, V) - maps:get(y, U)),
    case Diff_x =< Tolerance andalso Diff_y =< Tolerance of
        true -> true;
        false -> begin
            Max_x = max(abs(maps:get(x, V)), abs(maps:get(x, U))),
            Max_y = max(abs(maps:get(y, V)), abs(maps:get(y, U))),
            case Diff_x < Max_x * Tolerance andalso Diff_y < Max_y * Tolerance of
                true -> true;
                false -> false
                        end
        end
        end.

'Vec2.is_approx_zero'(V, Tolerance) ->
    case abs(maps:get(x, V)) =< Tolerance andalso abs(maps:get(y, V)) =< Tolerance of
        true -> true;
        false -> false
        end.

'Vec2.eq_scalar'(V, Scalar) ->
    maps:get(x, V) == todo andalso maps:get(y, V) == Scalar.

'Vec2.distance'(V, U) ->

'Vec2.manhattan_distance'(V, U) ->
    abs(maps:get(x, V) - maps:get(x, U)) + abs(maps:get(y, V) - maps:get(y, U)).

'Vec2.angle_between'(V, U) ->

'Vec2.angle_towards'(P1, P2) ->

'Vec2.angle'(V) ->

'Vec2.abs'(V) ->

'Vec2.clean'(V, Tolerance) ->
    R = 'unknown.copy'(V),
    case abs(maps:get(x, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(y, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    R.

'Vec2.clean_tolerance'(V, Tolerance) ->
    case abs(maps:get(x, V)) < Tolerance of
        true -> ok;
        false -> ok
    end,
    case abs(maps:get(y, V)) < Tolerance of
        true -> ok;
        false -> ok
    end.

'Vec2.inv'(V) ->
    #{x => case maps:get(x, V) /= 0 of
        true -> todo / maps:get(x, V);
        false -> 0
    end, y => case maps:get(y, V) /= 0 of
        true -> todo / maps:get(y, V);
        false -> 0
    end, {vbeam, type} => 'Vec2'}.

'Vec2.normalize'(V) ->
    M = 'Vec2.magnitude'(V),
    case M == 0 of
        true -> vec2(0, 0);
        false -> #{x => maps:get(x, V) * (1 / M), y => maps:get(y, V) * (1 / M), {vbeam, type} => 'Vec2'}
        end.

'Vec2.sum'(V) ->
    maps:get(x, V) + maps:get(y, V).

vec3(X, Y, Z) ->
    #{x => X, y => Y, z => Z, {vbeam, type} => 'Vec3'}.

'Vec3.zero'(V) ->

'Vec3.one'(V) ->

'Vec3.copy'(V) ->
    #{x => maps:get(x, V), y => maps:get(y, V), z => maps:get(z, V), {vbeam, type} => 'Vec3'}.

'Vec3.from'(V, U) ->

'Vec3.from_vec2'(V, U) ->

'Vec3.as_vec2'(V) ->
    #{x => maps:get(x, V), y => maps:get(y, V), {vbeam, type} => 'Vec2'}.

'Vec3.from_vec4'(V, U) ->

'Vec3.as_vec4'(V) ->
    #{x => maps:get(x, V), y => maps:get(y, V), z => maps:get(z, V), w => 0, {vbeam, type} => 'Vec4'}.

'Vec3.+'(V, U) ->
    #{x => maps:get(x, V) + maps:get(x, U), y => maps:get(y, V) + maps:get(y, U), z => maps:get(z, V) + maps:get(z, U), {vbeam, type} => 'Vec3'}.

'Vec3.add'(V, U) ->
    V + U.

'Vec3.add_vec2'(V, U) ->
    #{ => maps:get(x, V) + todo,  => maps:get(y, V) + todo,  => maps:get(z, V), {vbeam, type} => 'Vec3'}.

'Vec3.add_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) + todo,  => maps:get(y, V) + todo,  => maps:get(z, V) + todo, {vbeam, type} => 'Vec3'}.

'Vec3.plus'(V, U) ->

'Vec3.plus_vec2'(V, U) ->

'Vec3.plus_scalar'(V, Scalar) ->

'Vec3.-'(V, U) ->
    #{x => maps:get(x, V) - maps:get(x, U), y => maps:get(y, V) - maps:get(y, U), z => maps:get(z, V) - maps:get(z, U), {vbeam, type} => 'Vec3'}.

'Vec3.sub'(V, U) ->
    V - U.

'Vec3.sub_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) - todo,  => maps:get(y, V) - todo,  => maps:get(z, V) - todo, {vbeam, type} => 'Vec3'}.

'Vec3.subtract'(V, U) ->

'Vec3.subtract_scalar'(V, Scalar) ->

'Vec3.*'(V, U) ->
    #{x => maps:get(x, V) * maps:get(x, U), y => maps:get(y, V) * maps:get(y, U), z => maps:get(z, V) * maps:get(z, U), {vbeam, type} => 'Vec3'}.

'Vec3.mul'(V, U) ->
    V * U.

'Vec3.mul_scalar'(V, Scalar) ->
    #{x => maps:get(x, V) * todo, y => maps:get(y, V) * todo, z => maps:get(z, V) * todo, {vbeam, type} => 'Vec3'}.

'Vec3.multiply'(V, U) ->

'Vec3.multiply_scalar'(V, Scalar) ->

'Vec3./'(V, U) ->
    #{x => maps:get(x, V) / maps:get(x, U), y => maps:get(y, V) / maps:get(y, U), z => maps:get(z, V) / maps:get(z, U), {vbeam, type} => 'Vec3'}.

'Vec3.div'(V, U) ->
    V / U.

'Vec3.div_scalar'(V, Scalar) ->
    #{ => maps:get(x, V) / todo,  => maps:get(y, V) / todo,  => maps:get(z, V) / todo, {vbeam, type} => 'Vec3'}.

'Vec3.divide'(V, U) ->

'Vec3.divide_scalar'(V, Scalar) ->

'Vec3.magnitude'(V) ->
    case maps:get(x, V) == 0 andalso maps:get(y, V) == 0 andalso maps:get(z, V) == 0 of
        true -> todo;
        false -> todo
        end.

'Vec3.dot'(V, U) ->
    todo.

'Vec3.cross'(V, U) ->
    #{x => (maps:get(y, V) * maps:get(z, U)) - (maps:get(z, V) * maps:get(y, U)), y => (maps:get(z, V) * maps:get(x, U)) - (maps:get(x, V) * maps:get(z, U)), z => (maps:get(x, V) * maps:get(y, U)) - (maps:get(y, V) * maps:get(x, U)), {vbeam, type} => 'Vec3'}.

'Vec3.unit'(V) ->
    M = 'Vec3.magnitude'(V),
    Invm = 1 / M,
    #{x => maps:get(x, V) * Invm, y => maps:get(y, V) * Invm, z => maps:get(z, V) * Invm, {vbeam, type} => 'Vec3'}.

'Vec3.perpendicular'(V, U) ->
    V - 'Vec3.project'(V, U).

'Vec3.project'(V, U) ->
    Scale = todo,
    'Vec3.mul_scalar'(U, Scale).

'Vec3.eq'(V, U) ->
    maps:get(x, V) == maps:get(x, U) andalso maps:get(y, V) == maps:get(y, U) andalso maps:get(z, V) == maps:get(z, U).

'Vec3.eq_epsilon'(V, U) ->
    'Vec3.eq_approx'(V, U, todo).

'Vec3.eq_approx'(V, U, Tolerance) ->
    Diff_x = abs(maps:get(x, V) - maps:get(x, U)),
    Diff_y = abs(maps:get(y, V) - maps:get(y, U)),
    Diff_z = abs(maps:get(z, V) - maps:get(z, U)),
    case Diff_x =< Tolerance andalso Diff_y =< Tolerance andalso Diff_z =< Tolerance of
        true -> true;
        false -> begin
            Max_x = max(abs(maps:get(x, V)), abs(maps:get(x, U))),
            Max_y = max(abs(maps:get(y, V)), abs(maps:get(y, U))),
            Max_z = max(abs(maps:get(z, V)), abs(maps:get(z, U))),
            case Diff_x < Max_x * Tolerance andalso Diff_y < Max_y * Tolerance andalso Diff_z < Max_z * Tolerance of
                true -> true;
                false -> false
                        end
        end
        end.

'Vec3.is_approx_zero'(V, Tolerance) ->
    case abs(maps:get(x, V)) =< Tolerance andalso abs(maps:get(y, V)) =< Tolerance andalso abs(maps:get(z, V)) =< Tolerance of
        true -> true;
        false -> false
        end.

'Vec3.eq_scalar'(V, Scalar) ->
    maps:get(x, V) == todo andalso maps:get(y, V) == todo andalso maps:get(z, V) == todo.

'Vec3.distance'(V, U) ->
    sqrt((maps:get(x, V) - maps:get(x, U)) * (maps:get(x, V) - maps:get(x, U)) + (maps:get(y, V) - maps:get(y, U)) * (maps:get(y, V) - maps:get(y, U)) + (maps:get(z, V) - maps:get(z, U)) * (maps:get(z, V) - maps:get(z, U))).

'Vec3.manhattan_distance'(V, U) ->
    abs(maps:get(x, V) - maps:get(x, U)) + abs(maps:get(y, V) - maps:get(y, U)) + abs(maps:get(z, V) - maps:get(z, U)).

'Vec3.angle_between'(V, U) ->

'Vec3.abs'(V) ->
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
    end.

'Vec3.clean'(V, Tolerance) ->
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
    R.

'Vec3.clean_tolerance'(V, Tolerance) ->
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
    end.

'Vec3.inv'(V) ->
    #{x => case maps:get(x, V) /= 0 of
        true -> todo / maps:get(x, V);
        false -> 0
    end, y => case maps:get(y, V) /= 0 of
        true -> todo / maps:get(y, V);
        false -> 0
    end, z => case maps:get(z, V) /= 0 of
        true -> todo / maps:get(z, V);
        false -> 0
    end, {vbeam, type} => 'Vec3'}.

'Vec3.normalize'(V) ->
    M = 'Vec3.magnitude'(V),
    case M == 0 of
        true -> vec3(0, 0, 0);
        false -> #{x => maps:get(x, V) / M, y => maps:get(y, V) / M, z => maps:get(z, V) / M, {vbeam, type} => 'Vec3'}
        end.

'Vec3.sum'(V) ->
    maps:get(x, V) + maps:get(y, V) + maps:get(z, V).

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
    case maps:get(x, V) == 0 andalso maps:get(y, V) == 0 andalso maps:get(z, V) == 0 andalso maps:get(w, V) == 0 of
        true -> todo;
        false -> todo
        end.

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
    maps:get(x, V) == maps:get(x, U) andalso maps:get(y, V) == maps:get(y, U) andalso maps:get(z, V) == maps:get(z, U) andalso maps:get(w, V) == maps:get(w, U).

'Vec4.eq_epsilon'(V, U) ->
    'Vec4.eq_approx'(V, U, todo).

'Vec4.eq_approx'(V, U, Tolerance) ->
    Diff_x = abs(maps:get(x, V) - maps:get(x, U)),
    Diff_y = abs(maps:get(y, V) - maps:get(y, U)),
    Diff_z = abs(maps:get(z, V) - maps:get(z, U)),
    Diff_w = abs(maps:get(w, V) - maps:get(w, U)),
    case Diff_x =< Tolerance andalso Diff_y =< Tolerance andalso Diff_z =< Tolerance andalso Diff_w =< Tolerance of
        true -> true;
        false -> begin
            Max_x = max(abs(maps:get(x, V)), abs(maps:get(x, U))),
            Max_y = max(abs(maps:get(y, V)), abs(maps:get(y, U))),
            Max_z = max(abs(maps:get(z, V)), abs(maps:get(z, U))),
            Max_w = max(abs(maps:get(w, V)), abs(maps:get(w, U))),
            case Diff_x < Max_x * Tolerance andalso Diff_y < Max_y * Tolerance andalso Diff_z < Max_z * Tolerance andalso Diff_w < Max_w * Tolerance of
                true -> true;
                false -> false
                        end
        end
        end.

'Vec4.is_approx_zero'(V, Tolerance) ->
    case abs(maps:get(x, V)) =< Tolerance andalso abs(maps:get(y, V)) =< Tolerance andalso abs(maps:get(z, V)) =< Tolerance andalso abs(maps:get(w, V)) =< Tolerance of
        true -> true;
        false -> false
        end.

'Vec4.eq_scalar'(V, Scalar) ->
    maps:get(x, V) == Scalar andalso maps:get(y, V) == todo andalso maps:get(z, V) == todo andalso maps:get(w, V) == todo.

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
    #{x => case maps:get(x, V) /= 0 of
        true -> todo / maps:get(x, V);
        false -> 0
    end, y => case maps:get(y, V) /= 0 of
        true -> todo / maps:get(y, V);
        false -> 0
    end, z => case maps:get(z, V) /= 0 of
        true -> todo / maps:get(z, V);
        false -> 0
    end, w => case maps:get(w, V) /= 0 of
        true -> todo / maps:get(w, V);
        false -> 0
    end, {vbeam, type} => 'Vec4'}.

'Vec4.normalize'(V) ->
    M = 'Vec4.magnitude'(V),
    case M == 0 of
        true -> vec4(0, 0, 0, 0);
        false -> #{x => maps:get(x, V) * (1 / M), y => maps:get(y, V) * (1 / M), z => maps:get(z, V) * (1 / M), w => maps:get(w, V) * (1 / M), {vbeam, type} => 'Vec4'}
        end.

'Vec4.sum'(V) ->
    maps:get(x, V) + maps:get(y, V) + maps:get(z, V) + maps:get(w, V).
