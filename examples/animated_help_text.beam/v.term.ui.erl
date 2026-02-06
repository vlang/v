-module('v.term.ui').
-export([init_color_table/0, clamp/3, approximate_rgb/3, lookup_rgb/3, rgb2ansi/3, 'Context.init'/1, 'Context.frame'/1, 'Context.cleanup'/1, 'Context.fail'/2, 'Context.event'/2, 'Color.hex'/1, init/1, 'Context.run'/1, 'Context.flush'/1, flush_stdout/0, 'Context.write'/2, 'Context.bold'/1, 'Context.set_cursor_position'/3, 'Context.show_cursor'/1, 'Context.hide_cursor'/1, 'Context.set_color'/2, 'Context.set_bg_color'/2, 'Context.reset_color'/1, 'Context.reset_bg_color'/1, 'Context.reset'/1, 'Context.clear'/1, 'Context.set_window_title'/2, 'Context.draw_point'/3, 'Context.draw_text'/4, 'Context.draw_line'/5, 'Context.draw_dashed_line'/5, 'Context.draw_rect'/5, 'Context.draw_empty_dashed_rect'/5, 'Context.draw_empty_rect'/5, 'Context.horizontal_separator'/2, 'KeyCode__static__from'/1, 'Direction__static__from'/1, 'MouseButton__static__from'/1, 'EventType__static__from'/1, 'Modifiers.is_empty'/1, 'Modifiers.has'/2, 'Modifiers.all'/2, 'Modifiers.set'/2, 'Modifiers.set_all'/1, 'Modifiers.clear'/2, 'Modifiers.clear_all'/1, 'Modifiers.toggle'/2, 'Modifiers__static__zero'/0, 'Modifiers__static__from'/1]).

init_color_table() ->
    Color_table_ = [],
    {R, G, B} = lists:foldl(fun(I, {RAcc, GAcc, BAcc}) ->
        ROut = lists:nth((I div 36) rem 6 + 1, [16#00, 16#5f, 16#87, 16#af, 16#d7, 16#ff]),
        GOut = lists:nth((I div 6) rem 6 + 1, [16#00, 16#5f, 16#87, 16#af, 16#d7, 16#ff]),
        BOut = lists:nth(I rem 6 + 1, [16#00, 16#5f, 16#87, 16#af, 16#d7, 16#ff]),
        {ROut, GOut, BOut}
    end, {R, G, B}, lists:seq(0, 216 - 1)),
    R1 = lists:foldl(fun(I, RAcc) ->
        ROut = 8 + (I * 10),
        ROut
    end, R, lists:seq(0, 24 - 1)),
    Color_table_.

clamp(X, Y, Z) ->
    case X < Y of
        true -> Y;
        false -> 
            case X > Z of
                true -> Z;
                false -> X
                        end
                end.

approximate_rgb(R, G, B) ->
    Grey = R > 0 andalso R < 255 andalso R == G andalso R == B,
    case Grey of
        true -> 232 + todo;
        false -> begin
            K = todo,
            R2 = clamp(R div K, 0, 5),
            G2 = clamp(G div K, 0, 5),
            B2 = clamp(B div K, 0, 5),
            16 + (R2 * 36) + (G2 * 6) + B2
        end
        end.

lookup_rgb(R, G, B) ->
    Color = (todo bsl 16) + (todo bsl 8) + todo,
    lists:foreach(fun(I) ->
        case lists:nth(I + 1, init_color_table()) == Color of
            true -> I;
            false -> ok
        end,
        ok
    end, lists:seq(16, 256 - 1)),
    -1.

rgb2ansi(R, G, B) ->
    C = lookup_rgb(R, G, B),
    case C == -1 of
        true -> approximate_rgb(R, G, B);
        false -> C
        end.

'Context.init'(Ctx) ->
    F = maps:get(init_fn, maps:get(cfg, Ctx)),
    f(maps:get(user_data, maps:get(cfg, Ctx))),
    ok.

'Context.frame'(Ctx) ->
    F = maps:get(frame_fn, maps:get(cfg, Ctx)),
    f(maps:get(user_data, maps:get(cfg, Ctx))),
    ok.

'Context.cleanup'(Ctx) ->
    F = maps:get(cleanup_fn, maps:get(cfg, Ctx)),
    f(maps:get(user_data, maps:get(cfg, Ctx))),
    ok.

'Context.fail'(Ctx, Error) ->
    F = maps:get(fail_fn, maps:get(cfg, Ctx)),
    f(Error),
    ok.

'Context.event'(Ctx, Event) ->
    F = maps:get(event_fn, maps:get(cfg, Ctx)),
    f(Event, maps:get(user_data, maps:get(cfg, Ctx))),
    ok.

'Color.hex'(C) ->
    <<"#", ('u8.hex'(maps:get(r, C)))/binary, ('u8.hex'(maps:get(g, C)))/binary, ('u8.hex'(maps:get(b, C)))/binary>>.

init(Cfg) ->
    Ctx = #{cfg => Cfg, {vbeam, type} => 'Context'},
    Ctx_ptr = Ctx,
    Ctx.

'Context.run'(Ctx) ->
    case maps:get(use_x11, maps:get(cfg, Ctx)) of
        true -> error(<<"x11 not supported on BEAM">>);
        false -> begin
            'Context.fail'(Ctx, <<"error: full terminal UI not supported on BEAM backend">>),
            error(<<"terminal UI not supported on BEAM">>)
        end
        end.

'Context.flush'(Ctx) ->
    case length(maps:get(print_buf, Ctx)) > 0 of
        true -> begin
            S = todo,
            print(S),
            flush_stdout(),
            '[]u8.clear'(maps:get(print_buf, Ctx))
        end;
        false -> ok
    end.

flush_stdout() ->
    ok.

'Context.write'(Ctx, S) ->
    case S == <<"">> of
        true -> ok;
        false -> todo
        end.

'Context.bold'(Ctx) ->
    'Context.write'(Ctx, <<"[1m">>),
    ok.

'Context.set_cursor_position'(Ctx, X, Y) ->
    'Context.write'(Ctx, <<"[", (integer_to_binary(Y))/binary, ";", (integer_to_binary(X))/binary, "H">>),
    ok.

'Context.show_cursor'(Ctx) ->
    'Context.write'(Ctx, <<"[?25h">>),
    ok.

'Context.hide_cursor'(Ctx) ->
    'Context.write'(Ctx, <<"[?25l">>),
    ok.

'Context.set_color'(Ctx, C) ->
    case maps:get(enable_rgb, Ctx) of
        true -> 'Context.write'(Ctx, <<"[38;2;", (integer_to_binary(todo))/binary, ";", (integer_to_binary(todo))/binary, ";", (integer_to_binary(todo))/binary, "m">>);
        false -> 'Context.write'(Ctx, <<"[38;5;", (integer_to_binary(rgb2ansi(maps:get(r, C), maps:get(g, C), maps:get(b, C))))/binary, "m">>)
    end.

'Context.set_bg_color'(Ctx, C) ->
    case maps:get(enable_rgb, Ctx) of
        true -> 'Context.write'(Ctx, <<"[48;2;", (integer_to_binary(todo))/binary, ";", (integer_to_binary(todo))/binary, ";", (integer_to_binary(todo))/binary, "m">>);
        false -> 'Context.write'(Ctx, <<"[48;5;", (integer_to_binary(rgb2ansi(maps:get(r, C), maps:get(g, C), maps:get(b, C))))/binary, "m">>)
    end.

'Context.reset_color'(Ctx) ->
    'Context.write'(Ctx, <<"[39m">>),
    ok.

'Context.reset_bg_color'(Ctx) ->
    'Context.write'(Ctx, <<"[49m">>),
    ok.

'Context.reset'(Ctx) ->
    'Context.write'(Ctx, <<"[0m">>),
    ok.

'Context.clear'(Ctx) ->
    'Context.write'(Ctx, <<"[2J[3J">>),
    ok.

'Context.set_window_title'(Ctx, S) ->
    print(<<"]0;", (S)/binary, "">>),
    flush_stdout(),
    ok.

'Context.draw_point'(Ctx, X, Y) ->
    'Context.set_cursor_position'(Ctx, X, Y),
    'Context.write'(Ctx, <<" ">>),
    ok.

'Context.draw_text'(Ctx, X, Y, S) ->
    'Context.set_cursor_position'(Ctx, X, Y),
    'Context.write'(Ctx, S),
    ok.

'Context.draw_line'(Ctx, X, Y, X2, Y2) ->
    Min_x = case X < X2 of
        true -> X;
        false -> X2
    end,
    Min_y = case Y < Y2 of
        true -> Y;
        false -> Y2
    end,
    Max_x = case X > X2 of
        true -> X;
        false -> X2
    end,
    _ = case Y > Y2 of
        true -> Y;
        false -> Y2
    end,
    case Y == Y2 of
        true -> ok;
        false -> begin
            X0 = X,
            X1 = X2,
            Y0 = Y,
            Y1 = Y2,
            Sx = case X0 < X1 of
                true -> 1;
                false -> -1
            end,
            Sy = case Y0 < Y1 of
                true -> 1;
                false -> -1
            end,
            Dx = case X0 < X1 of
                true -> X1 - X0;
                false -> X0 - X1
            end,
            Dy = case Y0 < Y1 of
                true -> Y0 - Y1;
                false -> Y1 - Y0
            end,
            Err = Dx + Dy,
            % TODO: unhandled stmt type
            ok        end
        end.

'Context.draw_dashed_line'(Ctx, X, Y, X2, Y2) ->
    X0 = X,
    X1 = X2,
    Y0 = Y,
    Y1 = Y2,
    Sx = case X0 < X1 of
        true -> 1;
        false -> -1
    end,
    Sy = case Y0 < Y1 of
        true -> 1;
        false -> -1
    end,
    Dx = case X0 < X1 of
        true -> X1 - X0;
        false -> X0 - X1
    end,
    Dy = case Y0 < Y1 of
        true -> Y0 - Y1;
        false -> Y1 - Y0
    end,
    Err = Dx + Dy,
    I = 0,
    % TODO: unhandled stmt type
    ok
'Context.draw_rect'(Ctx, X, Y, X2, Y2) ->
    case Y == Y2 orelse X == X2 of
        true -> ok;
        false -> begin
            Min_y = element(1, case Y < Y2 of
                true -> todo;
                false -> todo
            end),
            Max_y = element(2, case Y < Y2 of
                true -> todo;
                false -> todo
            end),
            lists:foreach(fun(Y_pos) ->
                'Context.draw_line'(Ctx, X, Y_pos, X2, Y_pos),
                ok
                ok
            end, lists:seq(Min_y, Max_y + 1 - 1)),
        end
        end.

'Context.draw_empty_dashed_rect'(Ctx, X, Y, X2, Y2) ->
    case Y == Y2 orelse X == X2 of
        true -> ok;
        false -> begin
            Min_x = element(1, case X < X2 of
                true -> todo;
                false -> todo
            end),
            Max_x = element(2, case X < X2 of
                true -> todo;
                false -> todo
            end),
            Min_y = element(1, case Y < Y2 of
                true -> todo;
                false -> todo
            end),
            Max_y = element(2, case Y < Y2 of
                true -> todo;
                false -> todo
            end),
            'Context.draw_dashed_line'(Ctx, Min_x, Min_y, Max_x, Min_y),
            'Context.draw_dashed_line'(Ctx, Min_x, Min_y, Min_x, Max_y),
            case (Max_y - Min_y) band 1 == 0 of
                true -> 'Context.draw_dashed_line'(Ctx, Min_x, Max_y, Max_x, Max_y);
                false -> 'Context.draw_dashed_line'(Ctx, Min_x + 1, Max_y, Max_x, Max_y)
            end,
            case (Max_x - Min_x) band 1 == 0 of
                true -> 'Context.draw_dashed_line'(Ctx, Max_x, Min_y, Max_x, Max_y);
                false -> 'Context.draw_dashed_line'(Ctx, Max_x, Min_y + 1, Max_x, Max_y)
            end
        end
        end.

'Context.draw_empty_rect'(Ctx, X, Y, X2, Y2) ->
    case Y == Y2 orelse X == X2 of
        true -> ok;
        false -> begin
            'Context.draw_line'(Ctx, X, Y, X2, Y),
            'Context.draw_line'(Ctx, X, Y2, X2, Y2),
            'Context.draw_line'(Ctx, X, Y, X, Y2),
            'Context.draw_line'(Ctx, X2, Y, X2, Y2),
            ok
        end
        end.

'Context.horizontal_separator'(Ctx, Y) ->
    'Context.set_cursor_position'(Ctx, 0, Y),
    'Context.write'(Ctx, repeat(todo, maps:get(window_width, Ctx))),
    ok.

'KeyCode__static__from'(Input) ->
    error(<<"invalid value">>).

'Direction__static__from'(Input) ->
    error(<<"invalid value">>).

'MouseButton__static__from'(Input) ->
    error(<<"invalid value">>).

'EventType__static__from'(Input) ->
    error(<<"invalid value">>).

'Modifiers.is_empty'(E) ->
    todo == 0.

'Modifiers.has'(E, Flag_) ->
    (todo band (todo)) /= 0.

'Modifiers.all'(E, Flag_) ->
    (todo band (todo)) == todo.

'Modifiers.set'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'Modifiers.set_all'(E) ->
    % TODO: unhandled stmt type
    ok
'Modifiers.clear'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'Modifiers.clear_all'(E) ->
    % TODO: unhandled stmt type
    ok
'Modifiers.toggle'(E, Flag_) ->
    % TODO: unhandled stmt type
    ok
'Modifiers__static__zero'() ->
    todo.

'Modifiers__static__from'(Input) ->
    error(<<"invalid value">>).
