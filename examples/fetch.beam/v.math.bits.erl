-module('v.math.bits').
-export([leading_zeros_8/1, leading_zeros_8_default/1, leading_zeros_16/1, leading_zeros_16_default/1, leading_zeros_32/1, leading_zeros_32_default/1, leading_zeros_64/1, leading_zeros_64_default/1, trailing_zeros_8/1, trailing_zeros_8_default/1, trailing_zeros_16/1, trailing_zeros_16_default/1, trailing_zeros_32/1, trailing_zeros_32_default/1, trailing_zeros_64/1, trailing_zeros_64_default/1, ones_count_8/1, ones_count_8_default/1, ones_count_16/1, ones_count_16_default/1, ones_count_32/1, ones_count_32_default/1, ones_count_64/1, ones_count_64_default/1, rotate_left_8/2, rotate_left_16/2, rotate_left_32/2, rotate_left_64/2, reverse_8/1, reverse_16/1, reverse_32/1, reverse_64/1, reverse_bytes_16/1, reverse_bytes_32/1, reverse_bytes_64/1, len_8/1, len_16/1, len_32/1, len_64/1, add_32/3, add_64/3, sub_32/3, sub_64/3, mul_32/2, mul_32_default/2, mul_64/2, mul_64_default/2, mul_add_32/3, mul_add_32_default/3, mul_add_64/3, mul_add_64_default/3, div_32/3, div_32_default/3, div_64/3, div_64_default/3, rem_32/3, rem_64/3, normalize/1, f32_bits/1, f32_from_bits/1, f64_bits/1, f64_from_bits/1]).

leading_zeros_8(X) ->
    leading_zeros_8_default(X).

leading_zeros_8_default(X) ->
    8 - len_8(X).

leading_zeros_16(X) ->
    leading_zeros_16_default(X).

leading_zeros_16_default(X) ->
    16 - len_16(X).

leading_zeros_32(X) ->
    leading_zeros_32_default(X).

leading_zeros_32_default(X) ->
    32 - len_32(X).

leading_zeros_64(X) ->
    leading_zeros_64_default(X).

leading_zeros_64_default(X) ->
    64 - len_64(X).

trailing_zeros_8(X) ->
    trailing_zeros_8_default(X).

trailing_zeros_8_default(X) ->
    todo.

trailing_zeros_16(X) ->
    trailing_zeros_16_default(X).

trailing_zeros_16_default(X) ->
    case X == 0 of
        true -> 16;
        false -> todo
        end.

trailing_zeros_32(X) ->
    trailing_zeros_32_default(X).

trailing_zeros_32_default(X) ->
    case X == 0 of
        true -> 32;
        false -> todo
        end.

trailing_zeros_64(X) ->
    trailing_zeros_64_default(X).

trailing_zeros_64_default(X) ->
    case X == 0 of
        true -> 64;
        false -> todo
        end.

ones_count_8(X) ->
    ones_count_8_default(X).

ones_count_8_default(X) ->
    todo.

ones_count_16(X) ->
    ones_count_16_default(X).

ones_count_16_default(X) ->
    todo.

ones_count_32(X) ->
    ones_count_32_default(X).

ones_count_32_default(X) ->
    todo.

ones_count_64(X) ->
    ones_count_64_default(X).

ones_count_64_default(X) ->
    Y = ((X bsr todo) band (todo band todo)) + (X band (todo band todo)),
    Y1 = ((Y bsr todo) band (todo band todo)) + (Y band (todo band todo)),
    Y2 = ((Y1 bsr 4) + Y1) band (todo band todo),
    Y3 = Y2 bsr 8,
    Y4 = Y3 bsr 16,
    Y5 = Y4 bsr 32,
    todo band 127.

rotate_left_8(X, K) ->
    S = todo band (todo - todo),
    (X bsl S) bor (X bsr (todo - S)).

rotate_left_16(X, K) ->
    S = todo band (todo - todo),
    (X bsl S) bor (X bsr (todo - S)).

rotate_left_32(X, K) ->
    S = todo band (todo - todo),
    (X bsl S) bor (X bsr (todo - S)).

rotate_left_64(X, K) ->
    S = todo band (todo - todo),
    (X bsl S) bor (X bsr (todo - S)).

reverse_8(X) ->
    lists:nth(X + 1, [todo, 16#80, 16#40, 16#c0, 16#20, 16#a0, 16#60, 16#e0, 16#10, 16#90, 16#50, 16#d0, 16#30, 16#b0, 16#70, 16#f0, 16#08, 16#88, 16#48, 16#c8, 16#28, 16#a8, 16#68, 16#e8, 16#18, 16#98, 16#58, 16#d8, 16#38, 16#b8, 16#78, 16#f8, 16#04, 16#84, 16#44, 16#c4, 16#24, 16#a4, 16#64, 16#e4, 16#14, 16#94, 16#54, 16#d4, 16#34, 16#b4, 16#74, 16#f4, 16#0c, 16#8c, 16#4c, 16#cc, 16#2c, 16#ac, 16#6c, 16#ec, 16#1c, 16#9c, 16#5c, 16#dc, 16#3c, 16#bc, 16#7c, 16#fc, 16#02, 16#82, 16#42, 16#c2, 16#22, 16#a2, 16#62, 16#e2, 16#12, 16#92, 16#52, 16#d2, 16#32, 16#b2, 16#72, 16#f2, 16#0a, 16#8a, 16#4a, 16#ca, 16#2a, 16#aa, 16#6a, 16#ea, 16#1a, 16#9a, 16#5a, 16#da, 16#3a, 16#ba, 16#7a, 16#fa, 16#06, 16#86, 16#46, 16#c6, 16#26, 16#a6, 16#66, 16#e6, 16#16, 16#96, 16#56, 16#d6, 16#36, 16#b6, 16#76, 16#f6, 16#0e, 16#8e, 16#4e, 16#ce, 16#2e, 16#ae, 16#6e, 16#ee, 16#1e, 16#9e, 16#5e, 16#de, 16#3e, 16#be, 16#7e, 16#fe, 16#01, 16#81, 16#41, 16#c1, 16#21, 16#a1, 16#61, 16#e1, 16#11, 16#91, 16#51, 16#d1, 16#31, 16#b1, 16#71, 16#f1, 16#09, 16#89, 16#49, 16#c9, 16#29, 16#a9, 16#69, 16#e9, 16#19, 16#99, 16#59, 16#d9, 16#39, 16#b9, 16#79, 16#f9, 16#05, 16#85, 16#45, 16#c5, 16#25, 16#a5, 16#65, 16#e5, 16#15, 16#95, 16#55, 16#d5, 16#35, 16#b5, 16#75, 16#f5, 16#0d, 16#8d, 16#4d, 16#cd, 16#2d, 16#ad, 16#6d, 16#ed, 16#1d, 16#9d, 16#5d, 16#dd, 16#3d, 16#bd, 16#7d, 16#fd, 16#03, 16#83, 16#43, 16#c3, 16#23, 16#a3, 16#63, 16#e3, 16#13, 16#93, 16#53, 16#d3, 16#33, 16#b3, 16#73, 16#f3, 16#0b, 16#8b, 16#4b, 16#cb, 16#2b, 16#ab, 16#6b, 16#eb, 16#1b, 16#9b, 16#5b, 16#db, 16#3b, 16#bb, 16#7b, 16#fb, 16#07, 16#87, 16#47, 16#c7, 16#27, 16#a7, 16#67, 16#e7, 16#17, 16#97, 16#57, 16#d7, 16#37, 16#b7, 16#77, 16#f7, 16#0f, 16#8f, 16#4f, 16#cf, 16#2f, 16#af, 16#6f, 16#ef, 16#1f, 16#9f, 16#5f, 16#df, 16#3f, 16#bf, 16#7f, 16#ff]).

reverse_16(X) ->
    todo bor (todo bsl 8).

reverse_32(X) ->
    Y = (((X bsr todo) band (todo band todo)) bor ((X band (todo band todo)) bsl 1)),
    Y1 = (((Y bsr todo) band (todo band todo)) bor ((Y band (todo band todo)) bsl todo)),
    Y2 = (((Y1 bsr todo) band (todo band todo)) bor ((Y1 band (todo band todo)) bsl todo)),
    reverse_bytes_32(todo).

reverse_64(X) ->
    Y = (((X bsr todo) band (todo band todo)) bor ((X band (todo band todo)) bsl 1)),
    Y1 = (((Y bsr todo) band (todo band todo)) bor ((Y band (todo band todo)) bsl 2)),
    Y2 = (((Y1 bsr todo) band (todo band todo)) bor ((Y1 band (todo band todo)) bsl 4)),
    reverse_bytes_64(Y2).

reverse_bytes_16(X) ->
    (X bsr 8) bor (X bsl 8).

reverse_bytes_32(X) ->
    Y = (((X bsr todo) band (todo band todo)) bor ((X band (todo band todo)) bsl todo)),
    todo.

reverse_bytes_64(X) ->
    Y = (((X bsr todo) band (todo band todo)) bor ((X band (todo band todo)) bsl todo)),
    Y1 = (((Y bsr todo) band (todo band todo)) bor ((Y band (todo band todo)) bsl todo)),
    (Y1 bsr 32) bor (Y1 bsl 32).

len_8(X) ->
    todo.

len_16(X) ->
    Y = X,
    N = 0,
    case Y >= 256 of
        true -> begin
            Y1 = 8,
            N1 = 8,
        end;
        false -> ok
    end,
    N1 + todo.

len_32(X) ->
    Y = X,
    N = 0,
    case Y >= 65536 of
        true -> begin
            Y1 = 16,
            N1 = 16,
        end;
        false -> ok
    end,
    case Y1 >= 256 of
        true -> begin
            Y2 = 8,
            N2 = 8,
        end;
        false -> ok
    end,
    N2 + todo.

len_64(X) ->
    Y = X,
    N = 0,
    case Y >= todo bsl todo of
        true -> begin
            Y1 = 32,
            N1 = 32,
        end;
        false -> ok
    end,
    case Y1 >= todo bsl todo of
        true -> begin
            Y2 = 16,
            N2 = 16,
        end;
        false -> ok
    end,
    case Y2 >= todo bsl todo of
        true -> begin
            Y3 = 8,
            N3 = 8,
        end;
        false -> ok
    end,
    N3 + todo.

add_32(X, Y, Carry) ->
    Sum64 = todo + todo + todo,
    Sum = todo,
    Carry_out = todo,
    Sum.

add_64(X, Y, Carry) ->
    Sum = X + Y + Carry,
    Carry_out = ((X band Y) bor ((X bor Y) band bnot Sum)) bsr 63,
    Sum.

sub_32(X, Y, Borrow) ->
    Diff = X - Y - Borrow,
    Borrow_out = ((bnot X band Y) bor (bnot (X bxor Y) band Diff)) bsr 31,
    Diff.

sub_64(X, Y, Borrow) ->
    Diff = X - Y - Borrow,
    Borrow_out = ((bnot X band Y) bor (bnot (X bxor Y) band Diff)) bsr 63,
    Diff.

mul_32(X, Y) ->
    mul_32_default(X, Y).

mul_32_default(X, Y) ->
    Tmp = todo * todo,
    Hi = todo,
    Lo = todo,
    Hi.

mul_64(X, Y) ->
    mul_64_default(X, Y).

mul_64_default(X, Y) ->
    X0 = X band todo - 1,
    X1 = X bsr 32,
    Y0 = Y band todo - 1,
    Y1 = Y bsr 32,
    W0 = X0 * Y0,
    T = X1 * Y0 + (W0 bsr 32),
    W1 = T band todo - 1,
    W2 = T bsr 32,
    W11 = X0 * Y1,
    Hi = X1 * Y1 + W2 + (W11 bsr 32),
    Lo = X * Y,
    Hi.

mul_add_32(X, Y, Z) ->
    mul_add_32_default(X, Y, Z).

mul_add_32_default(X, Y, Z) ->
    Tmp = todo * todo + todo,
    Hi = todo,
    Lo = todo,
    Hi.

mul_add_64(X, Y, Z) ->
    mul_add_64_default(X, Y, Z).

mul_add_64_default(X, Y, Z) ->
    H = element(1, mul_64(X, Y)),
    L = element(2, mul_64(X, Y)),
    Lo = L + Z,
    Hi = H + todo,
    Hi.

div_32(Hi, Lo, Y) ->
    div_32_default(Hi, Lo, Y).

div_32_default(Hi, Lo, Y) ->
    case Y /= 0 andalso Y =< Hi of
        true -> erlang:error({panic, <<"Overflow Error">>});
        false -> ok
    end,
    Z = (todo bsl 32) bor todo,
    Quo = todo,
    Rem = todo,
    Quo.

div_64(Hi, Lo, Y1) ->
    div_64_default(Hi, Lo, Y1).

div_64_default(Hi, Lo, Y1) ->
    Y = Y1,
    case Y == 0 of
        true -> erlang:error({panic, <<"Overflow Error">>});
        false -> ok
    end,
    case Y =< Hi of
        true -> erlang:error({panic, <<"Overflow Error">>});
        false -> ok
    end,
    S = todo,
    Y1 = S,
    Yn1 = Y1 bsr 32,
    Yn0 = Y1 band todo - 1,
    Ss1 = (Hi bsl S),
    Xxx = 64 - S,
    Ss2 = Lo bsr Xxx,
    case Xxx == 64 of
        true -> ok;
        false -> ok
    end,
    Un32 = Ss1 bor Ss2,
    Un10 = Lo bsl S,
    Un1 = Un10 bsr 32,
    Un0 = Un10 band todo - 1,
    Q1 = Un32 div Yn1,
    Rhat = Un32 - (Q1 * Yn1),
    % TODO: unhandled stmt type
    Un21 = (Un32 * todo) + (Un1 - (Q1 * Y1)),
    Q0 = Un21 div Yn1,
    Rhat1 = Un21 - Q0 * Yn1,
    % TODO: unhandled stmt type
    Qq = ((Q1 * todo) + Q0),
    Rr = ((Un21 * todo) + Un0 - (Q0 * Y1)) bsr S,
    Qq.

rem_32(Hi, Lo, Y) ->
    todo.

rem_64(Hi, Lo, Y) ->
    Rem = element(2, div_64(Hi rem Y, Lo, Y)),
    Rem.

normalize(X) ->
    Smallest_normal = 2.2250738585072014e-308,
    case (case X > todo of
        true -> X;
        false -> -X
    end) < Smallest_normal of
        true -> X * (todo bsl todo);
        false -> X
        end.

f32_bits(F) ->
    P = *todo,
    P.

f32_from_bits(B) ->
    P = *todo,
    P.

f64_bits(F) ->
    P = *todo,
    P.

f64_from_bits(B) ->
    P = *todo,
    P.
