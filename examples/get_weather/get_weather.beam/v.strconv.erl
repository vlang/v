-module('v.strconv').
-export([byte_to_lower/1, common_parse_uint/5, common_parse_uint2/3, parse_uint/3, common_parse_int/5, parse_int/3, atoi_common_check/1, atoi_common/3, atoi/1, atoi8/1, atoi16/1, atoi32/1, atoi64/1, safe_add_64bits/2, safe_mul10_64bits/1, atou_common_check/1, atou_common/2, atou8/1, atou16/1, atou/1, atou32/1, atou64/1, format_str/2, atof64/2, atof_quick/1, format_int/2, format_uint/2, ftoa_64/1, ftoa_long_64/1, ftoa_32/1, ftoa_long_32/1, f64_to_str/2, f64_to_str_pad/2, f32_to_str/2, f32_to_str_pad/2, f64_to_str_l/1, f64_to_str_l_with_dot/1, f32_to_str_l/1, f32_to_str_l_with_dot/1, fxx_to_str_l_parse/1, fxx_to_str_l_parse_with_dot/1, f64_to_str_lnd1/2, remove_tail_zeros/1, v_printf/2, v_sprintf/2, assert1/2, bool_to_int/1, bool_to_u32/1, bool_to_u64/1, get_string_special/3, mul_shift_32/3, mul_pow5_invdiv_pow2/3, mul_pow5_div_pow2/3, pow5_factor_32/1, multiple_of_power_of_five_32/2, multiple_of_power_of_two_32/2, log10_pow2/1, log10_pow5/1, pow5_bits/1, shift_right_128/2, mul_shift_64/3, pow5_factor_64/1, multiple_of_power_of_five_64/2, multiple_of_power_of_two_64/2, dec_digits/1, 'Align_text__static__from'/1]).

byte_to_lower(C) ->
    C bor 32.

common_parse_uint(S, _base, _bit_size, Error_on_non_digit, Error_on_high_digit) ->
    Result = element(1, common_parse_uint2(S, _base, _bit_size)),
    Err = element(2, common_parse_uint2(S, _base, _bit_size)),
    case Err /= 0 andalso (Error_on_non_digit orelse Error_on_high_digit) of
        true -> case Err of
            -1 -> error(<<"common_parse_uint: wrong base ", (integer_to_binary(_base))/binary, " for ", (S)/binary>>);
            -2 -> error(<<"common_parse_uint: wrong bit size ", (integer_to_binary(_bit_size))/binary, " for ", (S)/binary>>);
            -3 -> error(<<"common_parse_uint: integer overflow ", (S)/binary>>);
            _ -> error(<<"common_parse_uint: syntax error ", (S)/binary>>)
        end;
        false -> ok
    end,
    Result.

common_parse_uint2(S, _base, _bit_size) ->
    case S == <<"">> of
        true -> todo;
        false -> begin
            Bit_size = _bit_size,
            Base = _base,
            Start_index = 0,
            case Base == 0 of
                true -> begin
                    Base1 = 10,
                    case lists:nth(1, S) == todo of
                        true -> begin
                            Ch = case length(S) > 1 of
                                true -> lists:nth(2, S) bor 32;
                                false -> todo
                            end,
                            case length(S) >= 3 of
                                true -> begin
                                    case Ch == todo of
                                        true -> begin
                                            Base2 = 2,
                                            Start_index1 = 2,
                                        end;
                                        false -> case Ch == todo of
                                            true -> begin
                                                Base3 = 8,
                                                Start_index2 = 2,
                                            end;
                                            false -> case Ch == todo of
                                                true -> begin
                                                    Base4 = 16,
                                                    Start_index3 = 2,
                                                end;
                                                false -> ok
                                            end
                                        end
                                    end,
                                    case lists:nth(Start_index3 + 1, S) == todo of
                                        true -> todo;
                                        false -> ok
                                    end
                                end;
                                false -> case length(S) >= 2 andalso (lists:nth(2, S) >= todo andalso lists:nth(2, S) =< todo) of
                                    true -> begin
                                        Base5 = 10,
                                        todo
                                    end;
                                    false -> begin
                                        Base6 = 8,
                                        todo
                                    end
                                end
                            end
                        end;
                        false -> ok
                    end
                end;
                false -> ok
            end,
            case Bit_size == 0 of
                true -> ok;
                false -> case Bit_size < 0 orelse Bit_size > 64 of
                    true -> todo;
                    false -> ok
                end
            end,
            Cutoff = todo div todo + todo,
            Max_val = case Bit_size == 64 of
                true -> todo;
                false -> (todo bsl todo) - todo
            end,
            Basem1 = Base6 - 1,
            N = todo,
            {C, Sub_count, N1, N1} = lists:foldl(fun(I, {CAcc, Sub_countAcc, NAcc, N1Acc}) ->
                COut = lists:nth(I + 1, S),
                case C == todo of
                    true -> begin
                        case I == Start_index3 orelse I >= (length(S) - 1) of
                            true -> todo;
                            false -> ok
                        end,
                        case lists:nth(I - 1 + 1, S) == todo orelse lists:nth(I + 1 + 1, S) == todo of
                            true -> todo;
                            false -> ok
                        end,
                        % TODO: unhandled stmt type
                    end;
                    false -> ok
                end,
                Sub_countOut = 0,
                COut = 48,
                case C >= 17 of
                    true -> begin
                        todo,
                        C1 = 7,
                        case C1 >= 42 of
                            true -> begin
                                todo,
                                C2 = 32,
                            end;
                            false -> ok
                        end
                    end;
                    false -> ok
                end,
                case C2 > Basem1 orelse (Sub_count == 0 andalso C2 > 9) of
                    true -> N1;
                    false -> ok
                end,
                case N1 >= Cutoff of
                    true -> Max_val;
                    false -> ok
                end,
                NOut = todo,
                N1Out = NAcc + todo,
                case N1 < N1 orelse N1 > Max_val of
                    true -> Max_val;
                    false -> ok
                end,
                NOut = N1Acc,
                {COut, Sub_countOut, NOut, N1Out}
            end, {C, Sub_count, N, N1}, lists:seq(Start_index3, length(S) - 1)),
            N1
        end
        end.

parse_uint(S, _base, _bit_size) ->
    common_parse_uint(S, _base, _bit_size, true, true).

common_parse_int(_s, Base, _bit_size, Error_on_non_digit, Error_on_high_digit) ->
    case _s == <<"">> of
        true -> todo;
        false -> begin
            Bit_size = _bit_size,
            case Bit_size == 0 of
                true -> ok;
                false -> ok
            end,
            S = _s,
            Neg = false,
            case lists:nth(1, S) == todo of
                true -> ok;
                false -> case lists:nth(1, S) == todo of
                    true -> begin
                        Neg1 = true,
                        % TODO: unhandled stmt type
                    end;
                    false -> ok
                end
            end,
            Un = common_parse_uint(S, Base, Bit_size, Error_on_non_digit, Error_on_high_digit),
            case Un == 0 of
                true -> todo;
                false -> begin
                    Cutoff = todo bsl todo,
                    case not Neg1 andalso Un >= Cutoff of
                        true -> todo;
                        false -> 
                            case Neg1 andalso Un > Cutoff of
                                true -> -todo;
                                false -> case Neg1 of
                                    true -> -todo;
                                    false -> todo
                                end
                                                        end
                                                                end
                end
                        end
        end
        end.

parse_int(_s, Base, _bit_size) ->
    common_parse_int(_s, Base, _bit_size, true, true).

atoi_common_check(S) ->
    case S == <<"">> of
        true -> error(<<"strconv.atoi: parsing \"\": empty string">>);
        false -> begin
            Start_idx = 0,
            Sign = todo,
            case lists:nth(1, S) == todo orelse lists:nth(1, S) == todo of
                true -> begin
                    todo,
                    case lists:nth(1, S) == todo of
                        true -> ok;
                        false -> ok
                    end
                end;
                false -> ok
            end,
            case length(S) - Start_idx < 1 of
                true -> error(<<"strconv.atoi: parsing \"", (S)/binary, "\": no number after sign">>);
                false -> 
                    case lists:nth(Start_idx + 1, S) == todo orelse lists:nth(length(S) - 1 + 1, S) == todo of
                        true -> error(<<"strconv.atoi: parsing \"", (S)/binary, "\": values cannot start or end with underscores">>);
                        false -> Sign
                                        end
                                        end
        end
        end.

atoi_common(S, Type_min, Type_max) ->
    Sign = element(1, atoi_common_check(S)),
    Start_idx = element(2, atoi_common_check(S)),
    X = todo,
    Underscored = false,
    C = lists:foldl(fun(I, CAcc) ->
        COut = lists:nth(I + 1, S) - todo,
        case C == 47 of
            true -> begin
                case Underscored == true of
                    true -> error(<<"strconv.atoi: parsing \"", (S)/binary, "\": consecutives underscores are not allowed">>);
                    false -> ok
                end,
                Underscored1 = true,
                % TODO: unhandled stmt type
            end;
            false -> begin
                case C > 9 of
                    true -> error(<<"strconv.atoi: parsing \"", (S)/binary, "\": invalid radix 10 character">>);
                    false -> ok
                end,
                Underscored2 = false,
                X1 = (X * 10) + (C * Sign),
                case Sign == 1 andalso X1 > Type_max of
                    true -> error(<<"strconv.atoi: parsing \"", (S)/binary, "\": integer overflow">>);
                    false -> case X1 < Type_min of
                        true -> error(<<"strconv.atoi: parsing \"", (S)/binary, "\": integer underflow">>);
                        false -> ok
                    end
                end
            end
        end,
        COut
    end, C, lists:seq(Start_idx, length(S) - 1)),
    X1.

atoi(S) ->
    todo.

atoi8(S) ->
    todo.

atoi16(S) ->
    todo.

atoi32(S) ->
    todo.

atoi64(S) ->
    Sign = element(1, atoi_common_check(S)),
    Start_idx = element(2, atoi_common_check(S)),
    X = todo,
    Underscored = false,
    C = lists:foldl(fun(I, CAcc) ->
        COut = lists:nth(I + 1, S) - todo,
        case C == 47 of
            true -> begin
                case Underscored == true of
                    true -> error(<<"strconv.atoi64: parsing \"", (S)/binary, "\": consecutives underscores are not allowed">>);
                    false -> ok
                end,
                Underscored1 = true,
                % TODO: unhandled stmt type
            end;
            false -> begin
                case C > 9 of
                    true -> error(<<"strconv.atoi64: parsing \"", (S)/binary, "\": invalid radix 10 character">>);
                    false -> ok
                end,
                Underscored2 = false,
                X1 = safe_mul10_64bits(X),
                X2 = safe_add_64bits(X1, todo),
            end
        end,
        COut
    end, C, lists:seq(Start_idx, length(S) - 1)),
    X2.

safe_add_64bits(A, B) ->
    case A > 0 andalso B > (todo - A) of
        true -> error(<<"integer overflow">>);
        false -> case A < 0 andalso B < (todo - A) of
            true -> error(<<"integer underflow">>);
            false -> ok
        end
    end,
    A + B.

safe_mul10_64bits(A) ->
    case A > 0 andalso A > (todo div 10) of
        true -> error(<<"integer overflow">>);
        false -> 
            case A < 0 andalso A < (todo div 10) of
                true -> error(<<"integer underflow">>);
                false -> A * 10
                        end
                end.

atou_common_check(S) ->
    case S == <<"">> of
        true -> error(<<"strconv.atou: parsing \"\": empty string">>);
        false -> begin
            Start_idx = 0,
            case lists:nth(1, S) == todo of
                true -> error(<<"strconv.atou: parsing \"{s}\" : negative value">>);
                false -> begin
                    case lists:nth(1, S) == todo of
                        true -> todo;
                        false -> ok
                    end,
                    case length(S) - Start_idx < 1 of
                        true -> error(<<"strconv.atou: parsing \"", (S)/binary, "\": no number after sign">>);
                        false -> 
                            case lists:nth(Start_idx + 1, S) == todo orelse lists:nth(length(S) - 1 + 1, S) == todo of
                                true -> error(<<"strconv.atou: parsing \"", (S)/binary, "\": values cannot start or end with underscores">>);
                                false -> Start_idx
                                                        end
                                                                end
                end
                        end
        end
        end.

atou_common(S, Type_max) ->
    Start_idx = atou_common_check(S),
    X = todo,
    Underscored = false,
    C = lists:foldl(fun(I, CAcc) ->
        COut = lists:nth(I + 1, S) - todo,
        case C == 47 of
            true -> begin
                case Underscored == true of
                    true -> error(<<"strconv.atou: parsing \"", (S)/binary, "\": consecutives underscores are not allowed">>);
                    false -> ok
                end,
                Underscored1 = true,
                % TODO: unhandled stmt type
            end;
            false -> begin
                case C > 9 of
                    true -> error(<<"strconv.atou: parsing \"", (S)/binary, "\": invalid radix 10 character">>);
                    false -> ok
                end,
                Underscored2 = false,
                Oldx = X,
                X1 = (X * 10) + todo,
                case X1 > Type_max orelse Oldx > X1 of
                    true -> error(<<"strconv.atou: parsing \"", (S)/binary, "\": integer overflow">>);
                    false -> ok
                end
            end
        end,
        COut
    end, C, lists:seq(Start_idx, length(S) - 1)),
    X1.

atou8(S) ->
    todo.

atou16(S) ->
    todo.

atou(S) ->
    todo.

atou32(S) ->
    todo.

atou64(S) ->
    todo.

format_str(S, P) ->
    case maps:get(len0, P) =< 0 of
        true -> 'string.clone'(S);
        false -> begin
            Dif = maps:get(len0, P) - utf8_str_visible_length(S),
            case Dif =< 0 of
                true -> 'string.clone'(S);
                false -> begin
                    Res = new_builder(length(S) + Dif),
                    % TODO: unhandled stmt type
                    case maps:get(align, P) == right of
                        true -> ok;
                        false -> ok
                    end,
                    'Builder.write_string'(Res, S),
                    case maps:get(align, P) == left of
                        true -> ok;
                        false -> ok
                    end,
                    'Builder.str'(Res)
                end
                        end
        end
        end.

atof64(S, Param) ->
    case length(S) == 0 of
        true -> error(<<"expected a number found an empty string">>);
        false -> 0.0
        end.

atof_quick(S) ->
    0.0.

format_int(N, Radix) ->
    <<"">>.

format_uint(N, Radix) ->
    <<"">>.

ftoa_64(F) ->
    <<"">>.

ftoa_long_64(F) ->
    <<"">>.

ftoa_32(F) ->
    <<"">>.

ftoa_long_32(F) ->
    <<"">>.

f64_to_str(F, N_digit) ->
    <<"">>.

f64_to_str_pad(F, N_digit) ->
    <<"">>.

f32_to_str(F, N_digit) ->
    <<"">>.

f32_to_str_pad(F, N_digit) ->
    <<"">>.

f64_to_str_l(F) ->
    <<"">>.

f64_to_str_l_with_dot(F) ->
    <<"">>.

f32_to_str_l(F) ->
    <<"">>.

f32_to_str_l_with_dot(F) ->
    <<"">>.

fxx_to_str_l_parse(S) ->
    S.

fxx_to_str_l_parse_with_dot(S) ->
    S.

f64_to_str_lnd1(F, Dec_digit) ->
    <<"">>.

remove_tail_zeros(S) ->
    S.

v_printf(Str, Pt) ->
    ok.

v_sprintf(Str, Pt) ->
    <<"">>.

assert1(T, Msg) ->
    case not T of
        true -> erlang:error({panic, Msg});
        false -> ok
    end.

bool_to_int(B) ->
    case B of
        true -> 1;
        false -> 0
        end.

bool_to_u32(B) ->
    case B of
        true -> todo;
        false -> todo
        end.

bool_to_u64(B) ->
    case B of
        true -> todo;
        false -> todo
        end.

get_string_special(Neg, ExpZero, MantZero) ->
    case not MantZero of
        true -> <<"nan">>;
        false -> begin
            case not ExpZero of
                true -> case Neg of
                    true -> <<"-inf">>;
                    false -> <<"+inf">>
                end;
                false -> ok
            end,
            case Neg of
                true -> <<"-0e+00">>;
                false -> <<"0e+00">>
                        end
        end
        end.

mul_shift_32(M, Mul, Ishift) ->
    Hi = element(1, mul_64(todo, Mul)),
    Lo = element(2, mul_64(todo, Mul)),
    Shifted_sum = (Lo bsr todo) + (Hi bsl todo),
    assert1(Shifted_sum =< 2147483647, <<"shiftedSum <= math.max_u32">>),
    todo.

mul_pow5_invdiv_pow2(M, Q, J) ->
    assert1(Q < length([todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo]), <<"q < pow5_inv_split_32.len">>),
    mul_shift_32(M, lists:nth(Q + 1, [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo]), J).

mul_pow5_div_pow2(M, I, J) ->
    assert1(I < length([todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo]), <<"i < pow5_split_32.len">>),
    mul_shift_32(M, lists:nth(I + 1, [todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo, todo]), J).

pow5_factor_32(I_v) ->
    V = I_v,
    % TODO: unhandled stmt type
    V.

multiple_of_power_of_five_32(V, P) ->
    pow5_factor_32(V) >= P.

multiple_of_power_of_two_32(V, P) ->
    todo >= P.

log10_pow2(E) ->
    assert1(E >= 0, <<"e >= 0">>),
    assert1(E =< 1650, <<"e <= 1650">>),
    (todo * 78913) bsr 18.

log10_pow5(E) ->
    assert1(E >= 0, <<"e >= 0">>),
    assert1(E =< 2620, <<"e <= 2620">>),
    (todo * 732923) bsr 20.

pow5_bits(E) ->
    assert1(E >= 0, <<"e >= 0">>),
    assert1(E =< 3528, <<"e <= 3528">>),
    todo.

shift_right_128(V, Shift) ->
    assert1(Shift < 64, <<"shift < 64">>),
    (maps:get(hi, V) bsl todo) bor (maps:get(lo, V) bsr todo).

mul_shift_64(M, Mul, Shift) ->
    Hihi = element(1, mul_64(M, maps:get(hi, Mul))),
    Hilo = element(2, mul_64(M, maps:get(hi, Mul))),
    Lohi = element(1, mul_64(M, maps:get(lo, Mul))),
    Sum = #{lo => Lohi + Hilo, hi => Hihi, {vbeam, type} => 'Uint128'},
    case maps:get(lo, Sum) < Lohi of
        true -> todo;
        false -> ok
    end,
    shift_right_128(Sum, Shift - 64).

pow5_factor_64(V_i) ->
    V = V_i,
    % TODO: unhandled stmt type
    todo.

multiple_of_power_of_five_64(V, P) ->
    pow5_factor_64(V) >= P.

multiple_of_power_of_two_64(V, P) ->
    todo >= P.

dec_digits(N) ->
    case N =< 9999999999 of
        true -> case N =< 99999 of
            true -> case N =< 99 of
                true -> case N =< 9 of
                    true -> 1;
                    false -> 2
                end;
                false -> case N =< 999 of
                    true -> 3;
                    false -> case N =< 9999 of
                        true -> 4;
                        false -> 5
                    end
                end
            end;
            false -> case N =< 9999999 of
                true -> case N =< 999999 of
                    true -> 6;
                    false -> 7
                end;
                false -> case N =< 99999999 of
                    true -> 8;
                    false -> begin
                        case N =< 999999999 of
                            true -> 9;
                            false -> ok
                        end,
                        10
                    end
                end
            end
        end;
        false -> case N =< 999999999999999 of
            true -> case N =< 999999999999 of
                true -> case N =< 99999999999 of
                    true -> 11;
                    false -> 12
                end;
                false -> case N =< 9999999999999 of
                    true -> 13;
                    false -> case N =< 99999999999999 of
                        true -> 14;
                        false -> 15
                    end
                end
            end;
            false -> case N =< 99999999999999999 of
                true -> case N =< 9999999999999999 of
                    true -> 16;
                    false -> 17
                end;
                false -> case N =< 999999999999999999 of
                    true -> 18;
                    false -> begin
                        case N =< 9999999999999999999 of
                            true -> 19;
                            false -> ok
                        end,
                        20
                    end
                end
            end
        end
    end.

'Align_text__static__from'(Input) ->
    error(<<"invalid value">>).
