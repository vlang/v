-module('v.main').
-export(['Vec.norm'/1, new_image/2, 'Image.save_as_ppm'/2, 'Sphere.intersect'/2, clamp/1, to_int/1, intersect/3, rand_f64/0, new_tabs/0, radiance/3, ray_trace/4, main/0, 'Refl_t__static__from'/1]).

'Vec.norm'(V) ->
    Tmp_norm = todo / sqrt(maps:get(x, V) * maps:get(x, V) + maps:get(y, V) * maps:get(y, V) + maps:get(z, V) * maps:get(z, V)),
    #{x => maps:get(x, V) * Tmp_norm, y => maps:get(y, V) * Tmp_norm, z => maps:get(z, V) * Tmp_norm, {vbeam, type} => 'Vec'}.

new_image(W, H) ->
    Vecsize = todo,
    #{width => W, height => H, data => todo, {vbeam, type} => 'Image'}.

'Image.save_as_ppm'(Image, File_name) ->
    Npixels = maps:get(width, Image) * maps:get(height, Image),
    F_out = create(File_name),
    'File.writeln'(F_out, <<"P3">>),
    'File.writeln'(F_out, <<(integer_to_binary(maps:get(width, Image)))/binary, " ", (integer_to_binary(maps:get(height, Image)))/binary>>),
    'File.writeln'(F_out, <<"255">>),
    {C_r, C_g, C_b} = lists:foldl(fun(I, {C_rAcc, C_gAcc, C_bAcc}) ->
        C_rOut = to_int(maps:get(x, todo)),
        C_gOut = to_int(maps:get(y, todo)),
        C_bOut = to_int(maps:get(z, todo)),
        'File.write_string'(F_out, <<(integer_to_binary(C_r))/binary, " ", (integer_to_binary(C_g))/binary, " ", (integer_to_binary(C_b))/binary, " ">>),
        {C_rOut, C_gOut, C_bOut}
    end, {C_r, C_g, C_b}, lists:seq(0, Npixels - 1)),
    'File.close'(F_out),
    ok.

'Sphere.intersect'(Sp, R) ->
    Op = maps:get(p, Sp) - maps:get(o, R),
    B = 'Vec.dot'(Op, maps:get(d, R)),
    Det = B * B - 'Vec.dot'(Op, Op) + maps:get(rad, Sp) * maps:get(rad, Sp),
    case Det < 0 of
        true -> 0;
        false -> begin
            Det1 = sqrt(Det),
            T = B - Det1,
            case T > todo of
                true -> T;
                false -> begin
                    T1 = B + Det1,
                    case T1 > todo of
                        true -> T1;
                        false -> 0
                                        end
                end
                        end
        end
        end.

clamp(X) ->
    case X < 0 of
        true -> 0;
        false -> 
            case X > 1 of
                true -> 1;
                false -> X
                        end
                end.

to_int(X) ->
    P = pow(clamp(X), 0.45454545454545453),
    todo.

intersect(R, Spheres, Nspheres) ->
    D = 0.0,
    T = 1e+10,
    Id = 0,
    % TODO: unhandled stmt type
    ok    T < todo.

rand_f64() ->
    X = u32() band 16#3FFFFFFF,
    todo / todo.

new_tabs() ->
    C = #{{vbeam, type} => 'Cache'},
    Inv_len = todo / todo,
    X = lists:foldl(fun(I, XAcc) ->
        XOut = todo * todo * todo * Inv_len,
        XOut
    end, X, lists:seq(0, 65536 - 1)),
    C.

radiance(R, Depthi, Scene_id) ->
    case Depthi > 1024 of
        true -> #{{vbeam, type} => 'Vec'};
        false -> begin
            Depth = Depthi,
            T = 0.0,
            Id = 0,
            Res = false,
            V_1 = 1.0,
            Scene = lists:nth(Scene_id + 1, [[#{rad => 1e+5, p => #{x => 1e+5 + 1, y => 40.8, z => 81.6, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{x => .75, y => .25, z => .25, {vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 1e+5, p => #{x => -1e+5 + 99, y => 40.8, z => 81.6, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{x => .25, y => .25, z => .75, {vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 1e+5, p => #{x => 50, y => 40.8, z => 1e+5, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{x => .75, y => .75, z => .75, {vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 1e+5, p => #{x => 50, y => 40.8, z => -1e+5 + 170, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{{vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 1e+5, p => #{x => 50, y => 1e+5, z => 81.6, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{x => .75, y => .75, z => .75, {vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 1e+5, p => #{x => 50, y => -99918.4, z => 81.6, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{x => .75, y => .75, z => .75, {vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 16.5, p => #{x => 27, y => 16.5, z => 47, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, .999), refl => spec, {vbeam, type} => 'Sphere'}, #{rad => 16.5, p => #{x => 73, y => 16.5, z => 78, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, .999), refl => refr, {vbeam, type} => 'Sphere'}, #{rad => 600, p => #{x => 50, y => 681.33, z => 81.6, {vbeam, type} => 'Vec'}, e => #{x => 12, y => 12, z => 12, {vbeam, type} => 'Vec'}, c => #{{vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}], [#{rad => 1600, p => 'Vec.mul_scalar'(#{x => 1.0, y => 0.0, z => 2.0, {vbeam, type} => 'Vec'}, 3000), e => 'Vec.mul_scalar'(#{x => 1.0, y => .9, z => .8, {vbeam, type} => 'Vec'}, 18.72 * 2), c => #{{vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 1560, p => 'Vec.mul_scalar'(#{x => 1, y => 0, z => 2, {vbeam, type} => 'Vec'}, 3500), e => 'Vec.mul_scalar'(#{x => 1.0, y => .5, z => .05, {vbeam, type} => 'Vec'}, 74.88 * 2), c => #{{vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 10000, p => #{x => 50, y => 40.8, z => -860, {vbeam, type} => 'Vec'} + #{x => 0, y => 0, z => -200, {vbeam, type} => 'Vec'}, e => 'Vec.mul_scalar'(#{x => 0.00063842, y => 0.02001478, z => 0.28923243, {vbeam, type} => 'Vec'}, 6e-2 * 8), c => 'Vec.mul_scalar'(#{x => .7, y => .7, z => 1, {vbeam, type} => 'Vec'}, .25), refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 100000, p => #{x => 50, y => -100000, z => 0, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{x => .3, y => .3, z => .3, {vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 110000, p => #{x => 50, y => -110048.5, z => 0, {vbeam, type} => 'Vec'}, e => 'Vec.mul_scalar'(#{x => .9, y => .5, z => .05, {vbeam, type} => 'Vec'}, 4), c => #{{vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 4e+4, p => #{x => 50, y => -4e+4 - 30, z => -3000, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => #{x => .2, y => .2, z => .2, {vbeam, type} => 'Vec'}, refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 26.5, p => #{x => 22, y => 26.5, z => 42, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, .596), refl => spec, {vbeam, type} => 'Sphere'}, #{rad => 13, p => #{x => 75, y => 13, z => 82, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => 'Vec.mul_scalar'(#{x => .96, y => .96, z => .96, {vbeam, type} => 'Vec'}, .96), refl => refr, {vbeam, type} => 'Sphere'}, #{rad => 22, p => #{x => 87, y => 22, z => 24, {vbeam, type} => 'Vec'}, e => #{{vbeam, type} => 'Vec'}, c => 'Vec.mul_scalar'(#{x => .6, y => .6, z => .6, {vbeam, type} => 'Vec'}, .696), refl => refr, {vbeam, type} => 'Sphere'}], [#{rad => 150, p => #{x => 125, y => 28, z => 62, {vbeam, type} => 'Vec'}, e => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, 0e-3), c => 'Vec.mul_scalar'(#{x => 1, y => .9, z => .8, {vbeam, type} => 'Vec'}, .93), refl => refr, {vbeam, type} => 'Sphere'}, #{rad => 28, p => #{x => 55, y => -28, z => 62, {vbeam, type} => 'Vec'}, e => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, 1e+1), c => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, 0), refl => diff, {vbeam, type} => 'Sphere'}, #{rad => 300, p => #{x => 50, y => 28, z => 62, {vbeam, type} => 'Vec'}, e => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, 0e-3), c => 'Vec.mul_scalar'(#{x => 1, y => 1, z => 1, {vbeam, type} => 'Vec'}, .93), refl => spec, {vbeam, type} => 'Sphere'}]]),
            Res1 = element(1, intersect(R, maps:get(data, Scene), length(Scene))),
            T1 = element(2, intersect(R, maps:get(data, Scene), length(Scene))),
            Id1 = element(3, intersect(R, maps:get(data, Scene), length(Scene))),
            case not Res1 of
                true -> #{{vbeam, type} => 'Vec'};
                false -> begin
                    Obj = lists:nth(Id1 + 1, Scene),
                    X = maps:get(o, R) + 'Vec.mul_scalar'(maps:get(d, R), T1),
                    N = 'Vec.norm'(todo),
                    Nl = case 'Vec.dot'(N, maps:get(d, R)) < todo of
                        true -> N;
                        false -> 'Vec.mul_scalar'(N, -1)
                    end,
                    F = maps:get(c, Obj),
                    P = maps:get(z, F),
                    case maps:get(x, F) > maps:get(y, F) andalso maps:get(x, F) > maps:get(z, F) of
                        true -> ok;
                        false -> case maps:get(y, F) > maps:get(z, F) of
                            true -> ok;
                            false -> ok
                        end
                    end,
                    todo,
                    case Depth > 5 of
                        true -> case rand_f64() < P of
                            true -> ok;
                            false -> maps:get(e, Obj)
                        end;
                        false -> ok
                    end,
                    case maps:get(refl, Obj) == diff of
                        true -> begin
                            R1 = u32() band 65535,
                            R2 = rand_f64(),
                            R2s = sqrt(R2),
                            W = Nl,
                            U = case abs(maps:get(x, W)) > todo of
                                true -> #{x => 0, y => 1, z => 0, {vbeam, type} => 'Vec'};
                                false -> #{x => 1, y => 0, z => 0, {vbeam, type} => 'Vec'}
                            end,
                            U1 = 'Vec.norm'(todo),
                            V = 'Vec.cross'(W, U1),
                            D = 'Vec.norm'(todo),
                            maps:get(e, Obj) + F * radiance(#{o => X, d => D, {vbeam, type} => 'Ray'}, Depth, Scene_id)
                        end;
                        false -> case maps:get(refl, Obj) == spec of
                            true -> maps:get(e, Obj) + F * radiance(#{o => X, d => maps:get(d, R) - 'Vec.mul_scalar'(N, todo * 'Vec.dot'(N, maps:get(d, R))), {vbeam, type} => 'Ray'}, Depth, Scene_id);
                            false -> ok
                        end
                    end,
                    Refl_ray = #{o => X, d => maps:get(d, R) - 'Vec.mul_scalar'(N, todo * 'Vec.dot'(N, maps:get(d, R))), {vbeam, type} => 'Ray'},
                    Into = 'Vec.dot'(N, Nl) > 0,
                    Nc = todo,
                    Nt = todo,
                    Nnt = case Into of
                        true -> Nc / Nt;
                        false -> Nt / Nc
                    end,
                    Ddn = 'Vec.dot'(maps:get(d, R), Nl),
                    Cos2t = V_1 - Nnt * Nnt * (V_1 - Ddn * Ddn),
                    case Cos2t < todo of
                        true -> maps:get(e, Obj) + F * radiance(Refl_ray, Depth, Scene_id);
                        false -> begin
                            Dirc = case Into of
                                true -> todo;
                                false -> todo
                            end,
                            Tdir = 'Vec.norm'(todo),
                            A = Nt - Nc,
                            B = Nt + Nc,
                            R0 = A * A / (B * B),
                            C = case Into of
                                true -> V_1 + Ddn;
                                false -> V_1 - 'Vec.dot'(Tdir, N)
                            end,
                            Re = R0 + (V_1 - R0) * C * C * C * C * C,
                            Tr = V_1 - Re,
                            Pp = todo + todo * Re,
                            Rp = Re / Pp,
                            Tp = Tr / (V_1 - Pp),
                            Tmp = #{{vbeam, type} => 'Vec'},
                            case Depth > 2 of
                                true -> ok;
                                false -> ok
                            end,
                            maps:get(e, Obj) + (F * Tmp)
                        end
                                        end
                end
                        end
        end
        end.

ray_trace(W, H, Samps, Scene_id) ->
    Image = new_image(W, H),
    W1 = todo,
    H1 = todo,
    Samps1 = todo,
    Cam = #{o => #{x => 50, y => 52, z => 295.6, {vbeam, type} => 'Vec'}, d => 'Vec.norm'(#{x => 0, y => -0.042612, z => -1, {vbeam, type} => 'Vec'}), {vbeam, type} => 'Ray'},
    Cx = #{x => todo * todo / todo, y => 0, z => 0, {vbeam, type} => 'Vec'},
    Cy = 'Vec.mul_scalar'('Vec.norm'(todo), 0.5135),
    R = #{{vbeam, type} => 'Vec'},
    V_1 = todo,
    V_2 = todo,
    % TODO: unhandled stmt type
    ok    Image.

main() ->
    case length('v.os':'arguments'()) > 6 of
        true -> begin
            eprintln(<<"Usage:\\n     path_tracing [samples] [image.ppm] [scene_n] [width] [height]">>),
            exit(1)
        end;
        false -> ok
    end,
    Width = 320,
    Height = 200,
    Samples = 4,
    Scene_id = 0,
    File_name = <<"image.ppm">>,
    case length('v.os':'arguments'()) >= 2 of
        true -> ok;
        false -> ok
    end,
    case length('v.os':'arguments'()) >= 3 of
        true -> ok;
        false -> ok
    end,
    case length('v.os':'arguments'()) >= 4 of
        true -> ok;
        false -> ok
    end,
    case length('v.os':'arguments'()) >= 5 of
        true -> ok;
        false -> ok
    end,
    case length('v.os':'arguments'()) == 6 of
        true -> ok;
        false -> ok
    end,
    seed([todo, 0]),
    eprintln(<<"Path tracing samples: ", (integer_to_binary(Samples))/binary, ", file_name: ", (File_name)/binary, ", scene_id: ", (integer_to_binary(Scene_id))/binary, ", width: ", (integer_to_binary(Width))/binary, ", height: ", (integer_to_binary(Height))/binary>>),
    eprintln(<<"">>),
    T1 = ticks(),
    Image = ray_trace(Width, Height, Samples, Scene_id),
    T2 = ticks(),
    eprintln(<<"Rendering finished. Took: ", (integer_to_binary((T2 - T1)))/binary, "ms">>),
    'Image.save_as_ppm'(Image, File_name),
    T3 = ticks(),
    eprintln(<<"Image saved as [", (File_name)/binary, "]. Took: ", (integer_to_binary((T3 - T2)))/binary, "ms">>),
    ok.

'Refl_t__static__from'(Input) ->
    error(<<"invalid value">>).
