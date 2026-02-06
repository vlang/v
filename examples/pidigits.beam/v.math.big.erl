-module('v.math.big').
-export([compare_digit_array/2, add_digit_array/3, subtract_digit_array/3, multiply_digit_array/3, simple_multiply_digit_array/3, multiply_array_by_digit/3, divide_digit_array/4, divide_array_by_digit/4, divide_array_by_array/4, shift_digits_left/3, shift_digits_right/3, bitwise_or_digit_array/3, bitwise_and_digit_array/3, bitwise_xor_digit_array/3, bitwise_not_digit_array/2, bit_set/2, knuth_divide_array_by_array/4, greater_than/4, 'Integer.montgomery'/1, 'Integer.mont_odd'/3, 'Integer.mont_even'/3, 'Integer.exp_binary'/3, get_window_size/1, 'Integer.mont_mul'/3, 'Integer.to_mont'/2, 'Integer.from_mont'/2, 'Integer.free'/1, 'Integer.clone'/1, int_signum/1, integer_from_int/1, integer_from_u32/1, integer_from_i64/1, integer_from_u64/1, integer_from_bytes/2, integer_from_string/1, integer_from_radix/2, validate_string/2, integer_from_regular_string/2, regular_string_to_radix/2, 'Integer.abs'/1, 'Integer.neg'/1, 'Integer.+'/2, 'Integer.-'/2, 'Integer.add'/2, 'Integer.subtract'/2, 'Integer.*'/2, 'Integer.div_mod_internal'/2, 'Integer.div_mod'/2, 'Integer.div_mod_checked'/2, 'Integer./'/2, 'Integer.%'/2, 'Integer.div_checked'/2, 'Integer.mod_checked'/2, 'Integer.mod_euclid'/2, 'Integer.mod_euclid_checked'/2, 'Integer.mask_bits'/2, 'Integer.pow'/2, 'Integer.mod_pow'/3, 'Integer.big_mod_pow'/3, 'Integer.inc'/1, 'Integer.dec'/1, 'Integer.=='/2, 'Integer.abs_cmp'/2, 'Integer.<'/2, 'Integer.get_bit'/2, 'Integer.set_bit'/3, 'Integer.bitwise_or'/2, 'Integer.bitwise_and'/2, 'Integer.bitwise_not'/1, 'Integer.bitwise_com'/1, 'Integer.bitwise_xor'/2, 'Integer.left_shift'/2, 'Integer.right_shift'/2, 'Integer.bin_str'/1, 'Integer.hex'/1, 'Integer.radix_str'/2, 'Integer.general_radix_str'/2, general_str/3, 'Integer.str'/1, 'Integer.int'/1, 'Integer.bytes'/1, 'Integer.factorial'/1, 'Integer.isqrt'/1, 'Integer.isqrt_checked'/1, bi_min/2, bi_max/2, 'Integer.gcd'/2, 'Integer.gcd_binary'/2, 'Integer.gcd_euclid'/2, 'Integer.mod_inverse'/2, 'Integer.mod_inv'/2, 'Integer.rsh_to_set_bit'/1, 'Integer.is_odd'/1, 'Integer.is_power_of_2'/1, 'Integer.bit_len'/1, 'Integer.from_json_number'/2, 'Integer.to_json'/1, imax/2, imin/2, umax/2, umin/2, iabs/1, shrink_tail_zeros/1, 'Integer.shrink_tail_zeros'/1, debug_u64_str/1, debug_u32_str/1, found_multiplication_base_case/3, karatsuba_multiply_digit_array/3, toom3_multiply_digit_array/3, pow2/1, left_shift_digits_in_place/2, right_shift_digits_in_place/2, add_in_place/2, subtract_in_place/2]).

compare_digit_array(Operand_a, Operand_b) ->
    A_len = length(Operand_a),
    B_len = length(Operand_b),
    case A_len /= B_len of
        true -> case A_len < B_len of
            true -> -1;
            false -> 1
        end;
        false -> begin
            % TODO: unhandled stmt type
            ok            0
        end
        end.

add_digit_array(Operand_a, Operand_b, Sum) ->
    case length(Operand_a) == 0 of
        true -> ok;
        false -> 
            case length(Operand_b) == 0 of
                true -> ok;
                false -> begin
                    A = element(1, case length(Operand_a) >= length(Operand_b) of
                        true -> todo;
                        false -> todo
                    end),
                    B = element(2, case length(Operand_a) >= length(Operand_b) of
                        true -> todo;
                        false -> todo
                    end),
                    Carry = todo,
                    {Partial, Carry1} = lists:foldl(fun(Index, {PartialAcc, CarryAcc}) ->
                        PartialOut = CarryAcc + lists:nth(Index + 1, A) + lists:nth(Index + 1, B),
                        CarryOut = PartialAcc >> 60,
                        {PartialOut, CarryOut}
                    end, {Partial, Carry}, lists:seq(0, length(B) - 1)),
                    {Partial1, Carry2} = lists:foldl(fun(Index, {PartialAcc, CarryAcc}) ->
                        PartialOut = CarryAcc + lists:nth(Index + 1, A),
                        CarryOut = PartialAcc >> 60,
                        {PartialOut, CarryOut}
                    end, {Partial, Carry1}, lists:seq(length(B), length(A) - 1)),
                    shrink_tail_zeros(Sum),
                    ok
                end
                        end
                end.

subtract_digit_array(Operand_a, Operand_b, Storage) ->
    case length(Operand_a) == 0 of
        true -> ok;
        false -> 
            case length(Operand_b) == 0 of
                true -> ok;
                false -> begin
                    Borrow = todo,
                    {A, B, Diff, Borrow1} = lists:foldl(fun(Index, {AAcc, BAcc, DiffAcc, BorrowAcc}) ->
                        AOut = lists:nth(Index + 1, Operand_a),
                        BOut = lists:nth(Index + 1, Operand_b) + BorrowAcc,
                        DiffOut = AAcc - BAcc,
                        BorrowOut = (DiffAcc >> 60) & 1,
                        {AOut, BOut, DiffOut, BorrowOut}
                    end, {A, B, Diff, Borrow}, lists:seq(0, length(Operand_b) - 1)),
                    {Diff1, Borrow2} = lists:foldl(fun(Index, {DiffAcc, BorrowAcc}) ->
                        DiffOut = lists:nth(Index + 1, Operand_a) - BorrowAcc,
                        BorrowOut = (DiffAcc >> 60) & 1,
                        {DiffOut, BorrowOut}
                    end, {Diff, Borrow1}, lists:seq(length(Operand_b), length(Operand_a) - 1)),
                    shrink_tail_zeros(Storage),
                    ok
                end
                        end
                end.

multiply_digit_array(Operand_a, Operand_b, Storage) ->
    Max_len = case length(Operand_a) >= length(Operand_b) of
        true -> length(Operand_a);
        false -> length(Operand_b)
    end,
    case Max_len >= 360 of
        true -> toom3_multiply_digit_array(Operand_a, Operand_b, Storage);
        false -> case Max_len >= 70 of
            true -> karatsuba_multiply_digit_array(Operand_a, Operand_b, Storage);
            false -> simple_multiply_digit_array(Operand_a, Operand_b, Storage)
        end
    end.

simple_multiply_digit_array(Operand_a, Operand_b, Storage) ->
    {Hi, Lo} = lists:foldl(fun(B_index, {HiAcc, LoAcc}) ->
        HiOut = todo,
        LoOut = todo,
        {Hi1, Lo1} = lists:foldl(fun(A_index, {HiAcc, LoAcc}) ->
            HiOut = mul_add_64(lists:nth(A_index + 1, Operand_a), lists:nth(B_index + 1, Operand_b), lists:nth(A_index + B_index + 1, Storage) + Hi1),
            LoOut = mul_add_64(lists:nth(A_index + 1, Operand_a), lists:nth(B_index + 1, Operand_b), lists:nth(A_index + B_index + 1, Storage) + Hi1),
            HiOut = (HiAcc << 4) | (LoAcc >> 60),
            {HiOut, LoOut}
        end, {Hi, Lo}, lists:seq(0, length(Operand_a) - 1)),
        case Hi1 /= 0 of
            true -> ok;
            false -> ok
        end,
        {HiOut, LoOut}
    end, {Hi, Lo}, lists:seq(0, length(Operand_b) - 1)),
    shrink_tail_zeros(Storage),
    ok.

multiply_array_by_digit(Operand_a, Value, Storage) ->
    case Value == 0 of
        true -> ok;
        false -> 
            case Value == 1 of
                true -> ok;
                false -> begin
                    Hi = todo,
                    Lo = todo,
                    {Hi1, Lo1} = lists:foldl(fun(Index, {HiAcc, LoAcc}) ->
                        HiOut = mul_add_64(lists:nth(Index + 1, Operand_a), Value, Hi1),
                        LoOut = mul_add_64(lists:nth(Index + 1, Operand_a), Value, Hi1),
                        HiOut = HiAcc << 4 + (LoAcc >> 60),
                        {HiOut, LoOut}
                    end, {Hi, Lo}, lists:seq(0, length(Operand_a) - 1)),
                    case Hi1 > 0 of
                        true -> ok;
                        false -> ok
                    end,
                    shrink_tail_zeros(Storage),
                    ok
                end
                        end
                end.

divide_digit_array(Operand_a, Operand_b, Quotient, Remainder) ->
    Cmp_result = compare_digit_array(Operand_a, Operand_b),
    case Cmp_result == 0 of
        true -> ok;
        false -> 
            case Cmp_result < 0 of
                true -> ok;
                false -> case length(Operand_b) == 1 of
                    true -> divide_array_by_digit(Operand_a, lists:nth(1, Operand_b), Quotient, Remainder);
                    false -> divide_array_by_array(Operand_a, Operand_b, Quotient, Remainder)
                end
                        end
                end.

divide_array_by_digit(Operand_a, Divisor, Quotient, Remainder) ->
    case length(Operand_a) == 1 of
        true -> ok;
        false -> begin
            Rem = todo,
            Quo = todo,
            % TODO: unhandled stmt type
            ok            shrink_tail_zeros(Quotient),
            shrink_tail_zeros(Remainder),
            ok
        end
        end.

divide_array_by_array(Operand_a, Operand_b, Quotient, Remainder) ->
    knuth_divide_array_by_array(Operand_a, Operand_b, Quotient, Remainder),
    ok.

shift_digits_left(Original, Amount, Storage) ->
    Leftover = todo,
    Offset = 60 - Amount,
    {Value, Leftover1} = lists:foldl(fun(Index, {ValueAcc, LeftoverAcc}) ->
        ValueOut = (LeftoverAcc | (lists:nth(Index + 1, Original) << Amount)) & (todo bsl 60) - todo,
        LeftoverOut = (lists:nth(Index + 1, Original) & (todo << Offset)) >> Offset,
        {ValueOut, LeftoverOut}
    end, {Value, Leftover}, lists:seq(0, length(Original) - 1)),
    case Leftover1 /= 0 of
        true -> Storage bsl Leftover1;
        false -> ok
    end.

shift_digits_right(Original, Amount, Storage) ->
    Moveover = todo,
    Mask = (todo bsl Amount) - 1,
    Offset = 60 - Amount,
    % TODO: unhandled stmt type
    ok    shrink_tail_zeros(Storage),
    ok.

bitwise_or_digit_array(Operand_a, Operand_b, Storage) ->
    Lower = element(1, case length(Operand_a) < length(Operand_b) of
        true -> todo;
        false -> todo
    end),
    Upper = element(2, case length(Operand_a) < length(Operand_b) of
        true -> todo;
        false -> todo
    end),
    Bigger = element(3, case length(Operand_a) < length(Operand_b) of
        true -> todo;
        false -> todo
    end),
    lists:foreach(fun(Index) ->
        ok
    end, lists:seq(0, Lower - 1)),
    lists:foreach(fun(Index) ->
        ok
    end, lists:seq(Lower, Upper - 1)),
    shrink_tail_zeros(Storage),
    ok.

bitwise_and_digit_array(Operand_a, Operand_b, Storage) ->
    Lower = imin(length(Operand_a), length(Operand_b)),
    lists:foreach(fun(Index) ->
        ok
    end, lists:seq(0, Lower - 1)),
    shrink_tail_zeros(Storage),
    ok.

bitwise_xor_digit_array(Operand_a, Operand_b, Storage) ->
    Lower = element(1, case length(Operand_a) < length(Operand_b) of
        true -> todo;
        false -> todo
    end),
    Upper = element(2, case length(Operand_a) < length(Operand_b) of
        true -> todo;
        false -> todo
    end),
    Bigger = element(3, case length(Operand_a) < length(Operand_b) of
        true -> todo;
        false -> todo
    end),
    lists:foreach(fun(Index) ->
        ok
    end, lists:seq(0, Lower - 1)),
    lists:foreach(fun(Index) ->
        ok
    end, lists:seq(Lower, Upper - 1)),
    shrink_tail_zeros(Storage),
    ok.

bitwise_not_digit_array(Original, Storage) ->
    lists:foreach(fun(Index) ->
        ok
    end, lists:seq(0, length(Original) - 1)),
    shrink_tail_zeros(Storage),
    ok.

bit_set(A, N) ->
    Byte_offset = N div 60,
    Mask = todo bsl todo,
    % TODO: unhandled stmt type
    ok
knuth_divide_array_by_array(Operand_a, Operand_b, Quotient, Remainder) ->
    M = length(Operand_a) - length(Operand_b),
    N = length(Operand_b),
    U = [],
    V = [],
    Leading_zeros = leading_zeros_64('[]u64.last'(Operand_b)) - 4,
    case Leading_zeros > 0 of
        true -> begin
            Carry = todo,
            Amount = 60 - Leading_zeros,
            {Temp, Carry1} = lists:foldl(fun(I, {TempAcc, CarryAcc}) ->
                TempOut = (lists:nth(I + 1, Operand_a) << Leading_zeros) | CarryAcc,
                CarryOut = lists:nth(I + 1, Operand_a) >> Amount,
                {TempOut, CarryOut}
            end, {Temp, Carry}, lists:seq(0, length(Operand_a) - 1)),
            Carry2 = 0,
            {Temp1, Carry3} = lists:foldl(fun(I, {TempAcc, CarryAcc}) ->
                TempOut = (lists:nth(I + 1, Operand_b) << Leading_zeros) | CarryAcc,
                CarryOut = lists:nth(I + 1, Operand_b) >> Amount,
                {TempOut, CarryOut}
            end, {Temp, Carry2}, lists:seq(0, length(Operand_b) - 1)),
        end;
        false -> begin
            lists:foreach(fun(I) ->
                ok
            end, lists:seq(0, length(Operand_a) - 1)),
            lists:foreach(fun(I) ->
                ok
            end, lists:seq(0, length(Operand_b) - 1)),
        end
    end,
    case length(Remainder) >= (N + 1) of
        true -> '[]u64.trim'(Remainder, N + 1);
        false -> ok
    end,
    V_n_1 = lists:nth(N - 1 + 1, V),
    V_n_2 = lists:nth(N - 2 + 1, V),
    % TODO: unhandled stmt type
    ok    '[]u64.delete_last'(Remainder),
    case Leading_zeros > 0 of
        true -> begin
            Carry4 = todo,
            Max_leading_digit = (todo bsl Leading_zeros) - 1,
            % TODO: unhandled stmt type
            ok        end;
        false -> ok
    end,
    shrink_tail_zeros(Quotient),
    shrink_tail_zeros(Remainder),
    ok.

greater_than(X1, X2, Y1, Y2) ->
    X1 > Y1 orelse (X1 == Y1 andalso X2 > Y2).

'Integer.montgomery'(M) ->
    % TODO: unhandled stmt type
    ok    N = 'Integer.abs'(M),
    B = todo,
    #{n => N, ni => ('Integer.left_shift'('Integer.mod_inv'('Integer.left_shift'(#{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}, B), N), B) - #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}) / N, rr => 'Integer.left_shift'(#{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}, B * 2) rem N, {vbeam, type} => 'MontgomeryContext'}.

'Integer.mont_odd'(A, X, M) ->
    % TODO: unhandled stmt type
    ok    Window = get_window_size(todo),
    Table = [],
    Ctx = 'Integer.montgomery'(M),
    Aa = case maps:get(signum, A) < 0 orelse 'Integer.abs_cmp'(A, M) >= 0 of
        true -> A rem M;
        false -> A
    end,
    % TODO: unhandled stmt type
    ok    R = case '[]u64.last'(maps:get(digits, M)) band (todo bsl 59) /= 0 of
        true -> begin
            Rdigits = [],
            % TODO: unhandled stmt type
            ok            #{digits => Rdigits, signum => 1, {vbeam, type} => 'Integer'}
        end;
        false -> 'Integer.to_mont'(#{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}, Ctx)
    end,
    Start = true,
    Wstart = 'Integer.bit_len'(X) - 1,
    Wvalue = 0,
    Wend = 0,
    % TODO: unhandled stmt type
    ok    'Integer.from_mont'(R, Ctx).

'Integer.mont_even'(A, X, M) ->
    % TODO: unhandled stmt type
    ok    M1 = element(1, 'Integer.rsh_to_set_bit'(M)),
    J = element(2, 'Integer.rsh_to_set_bit'(M)),
    M2 = 'Integer.left_shift'(#{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}, J),
    % TODO: unhandled stmt type
    ok    X1 = 'Integer.mont_odd'(A, X, M1),
    X2 = 'Integer.exp_binary'(A, X, M2),
    M2n = todo - 1,
    M1i = 'Integer.mod_inv'(M1, M2),
    % TODO: unhandled stmt type
    ok    T1 = 'Integer.mask_bits'(X1, M2n),
    T2 = 'Integer.mask_bits'(X2, M2n),
    T = 'Integer.mask_bits'((case 'Integer.abs_cmp'(T2, T1) >= 0 of
        true -> 'Integer.mask_bits'((T2 - T1), M2n);
        false -> 'Integer.mask_bits'('Integer.bitwise_not'('Integer.abs'((T1 - T2))), M2n) + #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}
    end * M1i), M2n),
    X1 + M1 * T.

'Integer.exp_binary'(A, X, M) ->
    % TODO: unhandled stmt type
    ok    N = todo - 1,
    Window = get_window_size(todo),
    Table = [],
    D = 'Integer.mask_bits'((lists:nth(1, Table) * lists:nth(1, Table)), N),
    % TODO: unhandled stmt type
    ok    R = #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'},
    Start = true,
    Wstart = 'Integer.bit_len'(X) - 1,
    Wend = 0,
    Wvalue = 1,
    % TODO: unhandled stmt type
    ok    'Integer.mask_bits'(R, N).

get_window_size(N) ->
    case N > 768 of
        true -> 6;
        false -> case N > 256 of
            true -> 5;
            false -> case N > 32 of
                true -> 4;
                false -> 3
            end
        end
    end.

'Integer.mont_mul'(A, B, Ctx) ->
    case (length(maps:get(digits, A)) + length(maps:get(digits, B))) > 2 * length(maps:get(digits, maps:get(n, Ctx))) of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            T = A * B,
            'Integer.from_mont'(T, Ctx)
        end
        end.

'Integer.to_mont'(A, Ctx) ->
    'Integer.mont_mul'(A, maps:get(rr, Ctx), Ctx).

'Integer.from_mont'(A, Ctx) ->
    Log2n = todo,
    R = 'Integer.right_shift'((A + ('Integer.mask_bits'(('Integer.mask_bits'(A, Log2n) * maps:get(ni, Ctx)), Log2n) * maps:get(n, Ctx))), Log2n),
    case 'Integer.abs_cmp'(R, maps:get(n, Ctx)) >= 0 of
        true -> R - maps:get(n, Ctx);
        false -> R
    end.

'Integer.free'(X) ->
    case maps:get(is_const, X) of
        true -> ok;
        false -> todo
        end.

'Integer.clone'(X) ->
    #{digits => '[]u64.clone'(maps:get(digits, X)), signum => maps:get(signum, X), is_const => false, {vbeam, type} => 'Integer'}.

int_signum(Value) ->
    case Value == 0 of
        true -> 0;
        false -> case Value < 0 of
            true -> -1;
            false -> 1
        end
        end.

integer_from_int(Value) ->
    case Value == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> case Value == todo of
            true -> #{digits => [todo], signum => -1, {vbeam, type} => 'Integer'};
            false -> #{digits => [todo], signum => int_signum(Value), {vbeam, type} => 'Integer'}
        end
        end.

integer_from_u32(Value) ->
    case Value == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> #{digits => [todo], signum => 1, {vbeam, type} => 'Integer'}
        end.

integer_from_i64(Value) ->
    case Value == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            Signum_value = case Value < 0 of
                true -> -1;
                false -> 1
            end,
            Abs_value = case Value == todo of
                true -> todo;
                false -> todo
            end,
            Lower = todo,
            Upper = todo,
            case Upper == 0 of
                true -> #{digits => [Lower], signum => Signum_value, {vbeam, type} => 'Integer'};
                false -> #{digits => [Lower, Upper], signum => Signum_value, {vbeam, type} => 'Integer'}
            end
        end
        end.

integer_from_u64(Value) ->
    case Value == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            Lower = todo,
            Upper = todo,
            case Upper == 0 of
                true -> #{digits => [Lower], signum => 1, {vbeam, type} => 'Integer'};
                false -> #{digits => [Lower, Upper], signum => 1, {vbeam, type} => 'Integer'}
            end
        end
        end.

integer_from_bytes(Oinput, Config) ->
    case length(Oinput) == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            First_non_zero_index = -1,
            lists:foreach(fun(I) ->
                case lists:nth(I + 1, Oinput) /= 0 of
                    true -> begin
                        First_non_zero_index1 = I,
                        % TODO: unhandled stmt type
                        ok                    end;
                    false -> ok
                end,
                ok
            end, lists:seq(0, length(Oinput) - 1)),
            case First_non_zero_index1 == -1 of
                true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
                false -> begin
                    Input = lists:nth(todo + 1, Oinput),
                    Carry_bits = 0,
                    Carry_value = todo,
                    Digits = [],
                    % TODO: unhandled stmt type
                    ok                    case Carry_bits > 0 of
                        true -> begin
                            Remaining_shift = 60 - Carry_bits,
                            Digits bsl (Carry_value bsr Remaining_shift)
                        end;
                        false -> ok
                    end,
                    shrink_tail_zeros(Digits),
                    case length(Digits) == 0 of
                        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
                        false -> #{digits => Digits, signum => maps:get(signum, Config), {vbeam, type} => 'Integer'}
                                        end
                end
                        end
        end
        end.

integer_from_string(Characters) ->
    integer_from_radix(Characters, 10).

integer_from_radix(All_characters, Radix) ->
    case Radix < 2 orelse Radix > 36 of
        true -> error(<<"math.big: Radix must be between 2 and 36 (inclusive)">>);
        false -> begin
            Characters = 'string.to_lower'(All_characters),
            validate_string(Characters, Radix),
            integer_from_regular_string(Characters, Radix)
        end
        end.

validate_string(Characters, Radix) ->
    Sign_present = length(Characters) > 0 andalso (lists:nth(1, Characters) == todo orelse lists:nth(1, Characters) == todo),
    Start_index = case Sign_present of
        true -> 1;
        false -> 0
    end,
    % TODO: unhandled stmt type
    ok    ok.

integer_from_regular_string(Characters, Radix) ->
    Sign_present = length(Characters) > 0 andalso (lists:nth(1, Characters) == todo orelse lists:nth(1, Characters) == todo),
    Signum = case Sign_present of
        true -> case lists:nth(1, Characters) == todo of
            true -> -1;
            false -> 1
        end;
        false -> 1
    end,
    Start_index = case Sign_present of
        true -> 1;
        false -> 0
    end,
    Result = #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'},
    Radix_int = integer_from_u32(Radix),
    Pow = maps:get(todo, #{2 => 59, 3 => 37, 4 => 29, 5 => 25, 6 => 23, 7 => 21, 8 => 19, 9 => 18, 10 => 18, 11 => 17, 12 => 16, 13 => 16, 14 => 15, 15 => 15, 16 => 14, 17 => 14, 18 => 14, 19 => 14, 20 => 13, 21 => 13, 22 => 13, 23 => 13, 24 => 13, 25 => 12, 26 => 12, 27 => 12, 28 => 12, 29 => 12, 30 => 12, 31 => 12, 32 => 11, 33 => 11, 34 => 11, 35 => 11, 36 => 11}),
    Radix_pow = 'Integer.pow'(Radix_int, todo),
    % TODO: unhandled stmt type
    ok    #{digits => '[]u64.clone'(maps:get(digits, Result)), signum => maps:get(signum, Result) * Signum, {vbeam, type} => 'Integer'}.

regular_string_to_radix(Characters, Radix) ->
    Result = todo,
    lists:foreach(fun(C) ->
        Result1 = Result * Radix + todo,
        ok
    end, Characters),
    Result1.

'Integer.abs'(A) ->
    case maps:get(signum, A) == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> #{digits => '[]u64.clone'(maps:get(digits, A)), signum => 1, {vbeam, type} => 'Integer'}
    end.

'Integer.neg'(A) ->
    case maps:get(signum, A) == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> #{digits => '[]u64.clone'(maps:get(digits, A)), signum => -maps:get(signum, A), {vbeam, type} => 'Integer'}
    end.

'Integer.+'(Augend, Addend) ->
    case maps:get(signum, Augend) == 0 of
        true -> 'Integer.clone'(Addend);
        false -> 
            case maps:get(signum, Addend) == 0 of
                true -> 'Integer.clone'(Augend);
                false -> 
                    case maps:get(signum, Augend) == maps:get(signum, Addend) of
                        true -> 'Integer.add'(Augend, Addend);
                        false -> case 'Integer.abs_cmp'(Augend, Addend) < 0 of
                            true -> 'Integer.neg'('Integer.subtract'(Augend, Addend));
                            false -> 'Integer.subtract'(Augend, Addend)
                        end
                                        end
                                        end
                end.

'Integer.-'(Minuend, Subtrahend) ->
    case maps:get(signum, Minuend) == 0 of
        true -> 'Integer.neg'(Subtrahend);
        false -> 
            case maps:get(signum, Subtrahend) == 0 of
                true -> 'Integer.clone'(Minuend);
                false -> 
                    case maps:get(signum, Minuend) == maps:get(signum, Subtrahend) of
                        true -> 'Integer.subtract'(Minuend, Subtrahend);
                        false -> 'Integer.add'(Minuend, Subtrahend)
                                        end
                                        end
                end.

'Integer.add'(Integer, Addend) ->
    A = maps:get(digits, Integer),
    B = maps:get(digits, Addend),
    Storage = [],
    add_digit_array(A, B, Storage),
    #{signum => maps:get(signum, Integer), digits => Storage, {vbeam, type} => 'Integer'}.

'Integer.subtract'(Integer, Subtrahend) ->
    Cmp = 'Integer.abs_cmp'(Integer, Subtrahend),
    case Cmp == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            A = element(1, case Cmp > 0 of
                true -> todo;
                false -> todo
            end),
            B = element(2, case Cmp > 0 of
                true -> todo;
                false -> todo
            end),
            Storage = [],
            subtract_digit_array(maps:get(digits, A), maps:get(digits, B), Storage),
            #{signum => Cmp * maps:get(signum, A), digits => Storage, {vbeam, type} => 'Integer'}
        end
        end.

'Integer.*'(Multiplicand, Multiplier) ->
    case maps:get(signum, Multiplicand) == 0 orelse maps:get(signum, Multiplier) == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> 
            case Multiplicand == #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'} of
                true -> 'Integer.clone'(Multiplier);
                false -> 
                    case Multiplier == #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'} of
                        true -> 'Integer.clone'(Multiplicand);
                        false -> begin
                            Storage = [],
                            multiply_digit_array(maps:get(digits, Multiplicand), maps:get(digits, Multiplier), Storage),
                            #{signum => maps:get(signum, Multiplicand) * maps:get(signum, Multiplier), digits => Storage, {vbeam, type} => 'Integer'}
                        end
                                        end
                                        end
                end.

'Integer.div_mod_internal'(Dividend, Divisor) ->
    Q = [],
    R = [],
    Q_signum = 0,
    R_signum = 0,
    divide_digit_array(maps:get(digits, Dividend), maps:get(digits, Divisor), Q, R),
    case maps:get(signum, Dividend) > 0 andalso maps:get(signum, Divisor) > 0 of
        true -> begin
            Q_signum1 = 1,
            R_signum1 = 1,
        end;
        false -> case maps:get(signum, Dividend) > 0 andalso maps:get(signum, Divisor) < 0 of
            true -> begin
                Q_signum2 = -1,
                R_signum2 = 1,
            end;
            false -> case maps:get(signum, Dividend) < 0 andalso maps:get(signum, Divisor) > 0 of
                true -> begin
                    Q_signum3 = -1,
                    R_signum3 = -1,
                end;
                false -> begin
                    Q_signum4 = 1,
                    R_signum4 = -1,
                end
            end
        end
    end,
    Quotient = #{signum => case length(Q) == 0 of
        true -> 0;
        false -> Q_signum4
    end, digits => Q, {vbeam, type} => 'Integer'},
    Remainder = #{signum => case length(R) == 0 of
        true -> 0;
        false -> R_signum4
    end, digits => R, {vbeam, type} => 'Integer'},
    Quotient.

'Integer.div_mod'(Dividend, Divisor) ->
    case todo of
        true -> panic(<<"math.big: Cannot divide by zero">>);
        false -> ok
    end,
    'Integer.div_mod_internal'(Dividend, Divisor).

'Integer.div_mod_checked'(Dividend, Divisor) ->
    case todo of
        true -> error(<<"math.big: Cannot divide by zero">>);
        false -> 'Integer.div_mod_internal'(Dividend, Divisor)
        end.

'Integer./'(Dividend, Divisor) ->
    Q = element(1, 'Integer.div_mod'(Dividend, Divisor)),
    Q.

'Integer.%'(Dividend, Divisor) ->
    R = element(2, 'Integer.div_mod'(Dividend, Divisor)),
    R.

'Integer.div_checked'(Dividend, Divisor) ->
    Q = element(1, 'Integer.div_mod_checked'(Dividend, Divisor)),
    Q.

'Integer.mod_checked'(Dividend, Divisor) ->
    R = element(2, 'Integer.div_mod_checked'(Dividend, Divisor)),
    R.

'Integer.mod_euclid'(Dividend, Divisor) ->
    R = Dividend rem Divisor,
    case R < #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'} of
        true -> R + 'Integer.abs'(Divisor);
        false -> R
    end.

'Integer.mod_euclid_checked'(Dividend, Divisor) ->
    R = 'Integer.mod_checked'(Dividend, Divisor),
    case R < #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'} of
        true -> R + 'Integer.abs'(Divisor);
        false -> R
    end.

'Integer.mask_bits'(A, N) ->
    % TODO: unhandled stmt type
    ok    case length(maps:get(digits, A)) == 0 orelse N == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            W = N div 60,
            B = N rem 60,
            case W >= length(maps:get(digits, A)) of
                true -> A;
                false -> #{digits => case B == 0 of
                    true -> begin
                        Storage = [],
                        % TODO: unhandled stmt type
                        ok                        Storage
                    end;
                    false -> begin
                        Storage1 = [],
                        % TODO: unhandled stmt type
                        ok                        Storage1
                    end
                end, signum => 1, {vbeam, type} => 'Integer'}
                        end
        end
        end.

'Integer.pow'(Base, Exponent) ->
    case Exponent == 0 of
        true -> #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'};
        false -> 
            case Exponent == 1 of
                true -> 'Integer.clone'(Base);
                false -> begin
                    N = Exponent,
                    X = Base,
                    Y = #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'},
                    % TODO: unhandled stmt type
                    ok                    X * Y
                end
                        end
                end.

'Integer.mod_pow'(Base, Exponent, Modulus) ->
    case Exponent == 0 of
        true -> #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'};
        false -> 
            case Exponent == 1 of
                true -> Base rem Modulus;
                false -> begin
                    N = Exponent,
                    X = Base rem Modulus,
                    Y = #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'},
                    % TODO: unhandled stmt type
                    ok                    X * Y rem Modulus
                end
                        end
                end.

'Integer.big_mod_pow'(Base, Exponent, Modulus) ->
    case maps:get(signum, Exponent) < 0 of
        true -> error(<<"math.big: Exponent needs to be non-negative.">>);
        false -> 
            case 'Integer.bit_len'(Modulus) =< 1 of
                true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
                false -> 
                    case maps:get(signum, Exponent) == 0 orelse 'Integer.bit_len'(Base) == 1 of
                        true -> #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'};
                        false -> 
                            case maps:get(signum, Base) == 0 of
                                true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
                                false -> 
                                    case 'Integer.bit_len'(Exponent) == 1 of
                                        true -> Base rem Modulus;
                                        false -> case length(maps:get(digits, Exponent)) > 1 of
                                            true -> case 'Integer.is_odd'(Modulus) of
                                                true -> 'Integer.mont_odd'(Base, Exponent, Modulus);
                                                false -> case 'Integer.is_power_of_2'(Modulus) of
                                                    true -> 'Integer.exp_binary'(Base, Exponent, Modulus);
                                                    false -> 'Integer.mont_even'(Base, Exponent, Modulus)
                                                end
                                            end;
                                            false -> 'Integer.mod_pow'(Base, lists:nth(1, maps:get(digits, Exponent)), Modulus)
                                        end
                                                                        end
                                                                                        end
                                                                end
                                        end
                end.

'Integer.inc'(A) ->
    A = A + #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'},

'Integer.dec'(A) ->
    A = A - #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'},

'Integer.=='(A, B) ->
    maps:get(signum, A) == maps:get(signum, B) andalso length(maps:get(digits, A)) == length(maps:get(digits, B)) andalso maps:get(digits, A) == maps:get(digits, B).

'Integer.abs_cmp'(A, B) ->
    compare_digit_array(maps:get(digits, A), maps:get(digits, B)).

'Integer.<'(A, B) ->
    case maps:get(signum, A) < maps:get(signum, B) of
        true -> true;
        false -> 
            case maps:get(signum, A) > maps:get(signum, B) of
                true -> false;
                false -> begin
                    Signum = maps:get(signum, A),
                    case Signum == 0 of
                        true -> false;
                        false -> begin
                            Cmp = 'Integer.abs_cmp'(A, B),
                            case Signum < 0 of
                                true -> Cmp > 0;
                                false -> Cmp < 0
                            end
                        end
                                        end
                end
                        end
                end.

'Integer.get_bit'(A, I) ->
    Target_index = I div 60,
    Offset = I rem 60,
    case Target_index >= length(maps:get(digits, A)) of
        true -> false;
        false -> (lists:nth(Target_index + 1, maps:get(digits, A)) bsr Offset) band 1 /= 0
        end.

'Integer.set_bit'(A, I, Value) ->
    Target_index = I div 60,
    Offset = I rem 60,
    case Target_index >= length(maps:get(digits, A)) of
        true -> ok;
        false -> begin
            Copy = '[]u64.clone'(maps:get(digits, A)),
            case Value of
                true -> ok;
                false -> ok
            end,
            A = #{signum => maps:get(signum, A), digits => Copy, {vbeam, type} => 'Integer'},
        end
        end.

'Integer.bitwise_or'(A, B) ->
    Result = [],
    bitwise_or_digit_array(maps:get(digits, A), maps:get(digits, B), Result),
    #{digits => Result, signum => case length(Result) == 0 of
        true -> 0;
        false -> 1
    end, {vbeam, type} => 'Integer'}.

'Integer.bitwise_and'(A, B) ->
    Result = [],
    bitwise_and_digit_array(maps:get(digits, A), maps:get(digits, B), Result),
    #{digits => Result, signum => case length(Result) == 0 of
        true -> 0;
        false -> 1
    end, {vbeam, type} => 'Integer'}.

'Integer.bitwise_not'(A) ->
    Result = [],
    bitwise_not_digit_array(maps:get(digits, A), Result),
    #{digits => Result, signum => case length(Result) == 0 of
        true -> 0;
        false -> 1
    end, {vbeam, type} => 'Integer'}.

'Integer.bitwise_com'(A) ->
    case maps:get(signum, A) == -1 of
        true -> 'Integer.abs'(A) - #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'};
        false -> 'Integer.neg'((A + #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}))
    end.

'Integer.bitwise_xor'(A, B) ->
    Result = [],
    bitwise_xor_digit_array(maps:get(digits, A), maps:get(digits, B), Result),
    #{digits => Result, signum => case length(Result) == 0 of
        true -> 0;
        false -> 1
    end, {vbeam, type} => 'Integer'}.

'Integer.left_shift'(A, Amount) ->
    case maps:get(signum, A) == 0 of
        true -> A;
        false -> 
            case Amount == 0 of
                true -> A;
                false -> begin
                    Normalised_amount = Amount rem 60,
                    Digit_offset = todo,
                    New_array = [],
                    lists:foreach(fun(Index) ->
                        ok
                    end, lists:seq(0, length(maps:get(digits, A)) - 1)),
                    case Normalised_amount > 0 of
                        true -> shift_digits_left(New_array, Normalised_amount, New_array);
                        false -> ok
                    end,
                    #{digits => New_array, signum => maps:get(signum, A), {vbeam, type} => 'Integer'}
                end
                        end
                end.

'Integer.right_shift'(A, Amount) ->
    case maps:get(signum, A) == 0 of
        true -> A;
        false -> 
            case Amount == 0 of
                true -> A;
                false -> begin
                    Normalised_amount = Amount rem 60,
                    Digit_offset = todo,
                    case Digit_offset >= length(maps:get(digits, A)) of
                        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
                        false -> begin
                            New_array = [],
                            lists:foreach(fun(Index) ->
                                ok
                            end, lists:seq(0, length(New_array) - 1)),
                            case Normalised_amount > 0 of
                                true -> shift_digits_right(New_array, Normalised_amount, New_array);
                                false -> ok
                            end,
                            #{digits => New_array, signum => case length(New_array) > 0 of
                                true -> maps:get(signum, A);
                                false -> 0
                            end, {vbeam, type} => 'Integer'}
                        end
                                        end
                end
                        end
                end.

'Integer.bin_str'(Integer) ->
    'Integer.radix_str'(Integer, 2).

'Integer.hex'(Integer) ->
    'Integer.radix_str'(Integer, 16).

'Integer.radix_str'(Integer, Radix) ->
    case maps:get(signum, Integer) == 0 orelse Radix == 0 of
        true -> <<"0">>;
        false -> 'Integer.general_radix_str'(Integer, todo)
        end.

'Integer.general_radix_str'(Integer, Radix) ->
    % TODO: unhandled stmt type
    ok    Divisor = 'Integer.pow'(integer_from_int(Radix), todo),
    Current = 'Integer.abs'(Integer),
    Digit = #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'},
    Sb = new_builder(length(maps:get(digits, Integer)) * maps:get(Radix, #{2 => 59, 3 => 37, 4 => 29, 5 => 25, 6 => 23, 7 => 21, 8 => 19, 9 => 18, 10 => 18, 11 => 17, 12 => 16, 13 => 16, 14 => 15, 15 => 15, 16 => 14, 17 => 14, 18 => 14, 19 => 14, 20 => 13, 21 => 13, 22 => 13, 23 => 13, 24 => 13, 25 => 12, 26 => 12, 27 => 12, 28 => 12, 29 => 12, 30 => 12, 31 => 12, 32 => 11, 33 => 11, 34 => 11, 35 => 11, 36 => 11})),
    St = [],
    % TODO: unhandled stmt type
    ok    case maps:get(signum, Integer) == -1 of
        true -> 'Builder.write_string'(Sb, <<"-">>);
        false -> ok
    end,
    % TODO: unhandled stmt type
    ok    'Builder.str'(Sb).

general_str(Quotient, Remainder, Radix) ->
    case maps:get(signum, Quotient) == 0 andalso maps:get(signum, Remainder) == 0 of
        true -> <<"0">>;
        false -> begin
            Divisor = integer_from_int(Radix),
            Current = 'Integer.abs'(Remainder),
            Digit = #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'},
            Sb = new_builder(maps:get(Radix, #{2 => 59, 3 => 37, 4 => 29, 5 => 25, 6 => 23, 7 => 21, 8 => 19, 9 => 18, 10 => 18, 11 => 17, 12 => 16, 13 => 16, 14 => 15, 15 => 15, 16 => 14, 17 => 14, 18 => 14, 19 => 14, 20 => 13, 21 => 13, 22 => 13, 23 => 13, 24 => 13, 25 => 12, 26 => 12, 27 => 12, 28 => 12, 29 => 12, 30 => 12, 31 => 12, 32 => 11, 33 => 11, 34 => 11, 35 => 11, 36 => 11})),
            St = [],
            % TODO: unhandled stmt type
            ok            case maps:get(signum, Quotient) > 0 of
                true -> 'Builder.write_string'(Sb, repeat(48, maps:get(Radix, #{2 => 59, 3 => 37, 4 => 29, 5 => 25, 6 => 23, 7 => 21, 8 => 19, 9 => 18, 10 => 18, 11 => 17, 12 => 16, 13 => 16, 14 => 15, 15 => 15, 16 => 14, 17 => 14, 18 => 14, 19 => 14, 20 => 13, 21 => 13, 22 => 13, 23 => 13, 24 => 13, 25 => 12, 26 => 12, 27 => 12, 28 => 12, 29 => 12, 30 => 12, 31 => 12, 32 => 11, 33 => 11, 34 => 11, 35 => 11, 36 => 11}) - length(St)));
                false -> ok
            end,
            % TODO: unhandled stmt type
            ok            'Builder.str'(Sb)
        end
        end.

'Integer.str'(Integer) ->
    'Integer.radix_str'(Integer, todo).

'Integer.int'(A) ->
    case maps:get(signum, A) == 0 of
        true -> 0;
        false -> 
            case lists:nth(1, maps:get(digits, A)) >= 2147483648 andalso maps:get(signum, A) == -1 of
                true -> -2147483648;
                false -> begin
                    Value = todo,
                    Value * maps:get(signum, A)
                end
                        end
                end.

'Integer.bytes'(A) ->
    case maps:get(signum, A) == 0 of
        true -> [];
        false -> begin
            Bit_len = 'Integer.bit_len'(A),
            Bytes = [],
            Current_byte = todo,
            Bits_in_byte = 0,
            Digit = '[]u64.last'(maps:get(digits, A)),
            Bit = todo,
            Bits_in_byte1 = 8 - Bit_len rem 8,
            case Bits_in_byte1 == 8 of
                true -> ok;
                false -> ok
            end,
            Msb_bits = Bit_len rem 60,
            case Msb_bits == 0 of
                true -> ok;
                false -> ok
            end,
            % TODO: unhandled stmt type
            ok            % TODO: unhandled stmt type
            ok            Bytes
        end
        end.

'Integer.factorial'(A) ->
    case maps:get(signum, A) == 0 of
        true -> #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            Product = #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'},
            Current = A,
            % TODO: unhandled stmt type
            ok            Product
        end
        end.

'Integer.isqrt'(A) ->
    'Integer.isqrt_checked'(A).

'Integer.isqrt_checked'(A) ->
    case maps:get(signum, A) < 0 of
        true -> error(<<"math.big: Cannot calculate square root of negative integer">>);
        false -> 
            case maps:get(signum, A) == 0 of
                true -> A;
                false -> 
                    case length(maps:get(digits, A)) == 1 andalso '[]u64.last'(maps:get(digits, A)) == 1 of
                        true -> A;
                        false -> begin
                            Shift = 'Integer.bit_len'(A),
                            case Shift band 1 == 1 of
                                true -> ok;
                                false -> ok
                            end,
                            Result = #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'},
                            % TODO: unhandled stmt type
                            ok                            Result
                        end
                                        end
                                        end
                end.

bi_min(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

bi_max(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

'Integer.gcd'(A, B) ->
    case length(maps:get(digits, B)) < 8 of
        true -> 'Integer.gcd_euclid'(A, B);
        false -> 'Integer.gcd_binary'(A, B)
        end.

'Integer.gcd_binary'(A, B) ->
    case maps:get(signum, A) == 0 of
        true -> 'Integer.abs'(B);
        false -> 
            case maps:get(signum, B) == 0 of
                true -> 'Integer.abs'(A);
                false -> 
                    case 'Integer.abs_cmp'(A, #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}) == 0 orelse 'Integer.abs_cmp'(B, #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'}) == 0 of
                        true -> #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'};
                        false -> begin
                            Aa = element(1, 'Integer.rsh_to_set_bit'('Integer.abs'(A))),
                            Az = element(2, 'Integer.rsh_to_set_bit'('Integer.abs'(A))),
                            Bb = element(1, 'Integer.rsh_to_set_bit'('Integer.abs'(B))),
                            Bz = element(2, 'Integer.rsh_to_set_bit'('Integer.abs'(B))),
                            Shift = umin(Az, Bz),
                            % TODO: unhandled stmt type
                            ok                            'Integer.left_shift'(Bb, Shift)
                        end
                                        end
                                        end
                end.

'Integer.gcd_euclid'(A, B) ->
    case maps:get(signum, A) == 0 of
        true -> 'Integer.abs'(B);
        false -> 
            case maps:get(signum, B) == 0 of
                true -> 'Integer.abs'(A);
                false -> 
                    case maps:get(signum, A) < 0 of
                        true -> 'Integer.gcd_euclid'('Integer.neg'(A), B);
                        false -> 
                            case maps:get(signum, B) < 0 of
                                true -> 'Integer.gcd_euclid'(A, 'Integer.neg'(B));
                                false -> begin
                                    X = A,
                                    Y = B,
                                    R = X rem Y,
                                    % TODO: unhandled stmt type
                                    ok                                    Y
                                end
                                                        end
                                                                end
                                        end
                end.

'Integer.mod_inverse'(A, N) ->
    case 'Integer.bit_len'(N) =< 1 of
        true -> error(<<"math.big: Modulus `n` must be greater than 1">>);
        false -> case 'Integer.gcd'(A, N) /= #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'} of
            true -> error(<<"math.big: No multiplicative inverse">>);
            false -> 'Integer.mod_inv'(A, N)
        end
    end.

'Integer.mod_inv'(A, M) ->
    N = #{digits => '[]u64.clone'(maps:get(digits, M)), signum => 1, {vbeam, type} => 'Integer'},
    B = A,
    X = #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'},
    Y = #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'},
    case maps:get(signum, B) < 0 orelse 'Integer.abs_cmp'(B, N) >= 0 of
        true -> ok;
        false -> ok
    end,
    Sign = -1,
    % TODO: unhandled stmt type
    ok    case Sign < 0 of
        true -> ok;
        false -> ok
    end,
    % TODO: unhandled stmt type
    ok    case maps:get(signum, Y) > 0 andalso 'Integer.abs_cmp'(Y, M) < 0 of
        true -> Y;
        false -> Y rem M
    end.

'Integer.rsh_to_set_bit'(X) ->
    case length(maps:get(digits, X)) == 0 of
        true -> #{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'};
        false -> begin
            N = todo,
            % TODO: unhandled stmt type
            ok            N1 = (N * 60) + todo,
            'Integer.right_shift'(X, N1)
        end
        end.

'Integer.is_odd'(X) ->
    length(maps:get(digits, X)) /= 0 andalso lists:nth(1, maps:get(digits, X)) band 1 == 1.

'Integer.is_power_of_2'(X) ->
    case maps:get(signum, X) =< 0 of
        true -> false;
        false -> begin
            % TODO: unhandled stmt type
            ok            N = '[]u64.last'(maps:get(digits, X)),
            N band (N - todo) == 0
        end
        end.

'Integer.bit_len'(X) ->
    case maps:get(signum, X) == 0 of
        true -> 0;
        false -> 
            case length(maps:get(digits, X)) == 0 of
                true -> 0;
                false -> length(maps:get(digits, X)) * 60 - (leading_zeros_64('[]u64.last'(maps:get(digits, X))) - 4)
                        end
                end.

'Integer.from_json_number'(Result, Raw_number) ->
    Index = 0,
    Is_negative = false,
    case lists:nth(1, Raw_number) == todo of
        true -> begin
            Is_negative1 = true,
            todo
        end;
        false -> ok
    end,
    Ten = integer_from_int(10),
    % TODO: unhandled stmt type
    ok    case Is_negative1 of
        true -> ok;
        false -> ok
    end,
    ok.

'Integer.to_json'(Result) ->
    'Integer.str'(Result).

imax(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

imin(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

umax(A, B) ->
    case A > B of
        true -> A;
        false -> B
    end.

umin(A, B) ->
    case A < B of
        true -> A;
        false -> B
    end.

iabs(V) ->
    case V > 0 of
        true -> V;
        false -> -V
    end.

shrink_tail_zeros(A) ->
    Alen = length(A),
    % TODO: unhandled stmt type
    ok    % TODO: unhandled stmt type
    ok
'Integer.shrink_tail_zeros'(I) ->
    Alen = length(maps:get(digits, I)),
    % TODO: unhandled stmt type
    ok    % TODO: unhandled stmt type
    ok
debug_u64_str(A) ->
    Sb = new_builder(30),
    'Builder.write_string'(Sb, <<"[">>),
    First = true,
    First1 = lists:foldl(fun(I, FirstAcc) ->
        case not First1 of
            true -> 'Builder.write_string'(Sb, <<", ">>);
            false -> ok
        end,
        'Builder.write_string'(Sb, <<"0x", ('u64.hex'(lists:nth(I + 1, A)))/binary>>),
        FirstOut = false,
        FirstOut
    end, First, lists:seq(0, length(A) - 1)),
    'Builder.write_string'(Sb, <<"]">>),
    'Builder.str'(Sb).

debug_u32_str(A) ->
    B = [],
    Curr_u32 = todo,
    Bits_collected = 0,
    lists:foreach(fun(W) ->
        {Bit, Curr_u321} = lists:foldl(fun(I, {BitAcc, Curr_u32Acc}) ->
            BitOut = (W >> I) & 1,
            Curr_u32Out = todo << Bits_collected,
            todo,
            case Bits_collected == 32 of
                true -> begin
                    B bsl Curr_u321,
                    Curr_u322 = 0,
                    Bits_collected1 = 0,
                end;
                false -> ok
            end,
            {BitOut, Curr_u32Out}
        end, {Bit, Curr_u32}, lists:seq(0, 60 - 1)),
        ok
    end, A),
    case Bits_collected1 > 0 of
        true -> B bsl Curr_u322;
        false -> ok
    end,
    Blen = length(B),
    % TODO: unhandled stmt type
    ok    % TODO: unhandled stmt type
    ok    Sb = new_builder(30),
    'Builder.write_string'(Sb, <<"[">>),
    First = true,
    First1 = lists:foldl(fun(I, FirstAcc) ->
        case not First1 of
            true -> 'Builder.write_string'(Sb, <<", ">>);
            false -> ok
        end,
        'Builder.write_string'(Sb, <<"0x", ('u32.hex'(lists:nth(I + 1, B)))/binary>>),
        FirstOut = false,
        FirstOut
    end, First, lists:seq(0, length(B) - 1)),
    'Builder.write_string'(Sb, <<"]">>),
    'Builder.str'(Sb).

found_multiplication_base_case(Operand_a, Operand_b, Storage) ->
    case length(Operand_a) == 0 orelse length(Operand_b) == 0 of
        true -> true;
        false -> 
            case length(Operand_a) < length(Operand_b) of
                true -> true;
                false -> 
                    case length(Operand_b) == 1 of
                        true -> true;
                        false -> false
                                        end
                                        end
                end.

karatsuba_multiply_digit_array(Operand_a, Operand_b, Storage) ->
    case found_multiplication_base_case(Operand_a, Operand_b, Storage) of
        true -> ok;
        false -> begin
            Half = imax(length(Operand_a), length(Operand_b)) div 2,
            A_l = todo,
            A_h = todo,
            B_l = [],
            B_h = [],
            case Half =< length(Operand_b) of
                true -> begin
                    B_l1 = todo,
                    B_h1 = todo,
                end;
                false -> ok
            end,
            shrink_tail_zeros(A_l),
            shrink_tail_zeros(A_h),
            shrink_tail_zeros(B_l1),
            shrink_tail_zeros(B_h1),
            multiply_digit_array(A_h, B_h1, Storage),
            P_3 = [],
            multiply_digit_array(A_l, B_l1, P_3),
            Tmp_1 = [],
            Tmp_2 = [],
            add_digit_array(A_h, A_l, Tmp_1),
            add_digit_array(B_h1, B_l1, Tmp_2),
            P_2 = [],
            multiply_digit_array(Tmp_1, Tmp_2, P_2),
            subtract_in_place(P_2, Storage),
            subtract_in_place(P_2, P_3),
            left_shift_digits_in_place(Storage, 2 * Half),
            left_shift_digits_in_place(P_2, Half),
            add_in_place(Storage, P_2),
            add_in_place(Storage, P_3),
            shrink_tail_zeros(Storage),
            ok
        end
        end.

toom3_multiply_digit_array(Operand_a, Operand_b, Storage) ->
    case found_multiplication_base_case(Operand_a, Operand_b, Storage) of
        true -> ok;
        false -> begin
            K = (length(Operand_a) + 2) div 3,
            K2 = 2 * K,
            A0 = #{digits => todo, signum => case '[]u64.all'(lists:nth(todo + 1, Operand_a), It == 0) of
                true -> 0;
                false -> 1
            end, {vbeam, type} => 'Integer'},
            'Integer.shrink_tail_zeros'(A0),
            A1 = #{digits => todo, signum => case '[]u64.all'(lists:nth(todo + 1, Operand_a), It == 0) of
                true -> 0;
                false -> 1
            end, {vbeam, type} => 'Integer'},
            'Integer.shrink_tail_zeros'(A1),
            A2 = #{digits => todo, signum => 1, {vbeam, type} => 'Integer'},
            B0 = 'Integer.clone'(#{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'}),
            B1 = 'Integer.clone'(#{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'}),
            B2 = 'Integer.clone'(#{digits => [], signum => 0, is_const => true, {vbeam, type} => 'Integer'}),
            case length(Operand_b) < K of
                true -> ok;
                false -> case length(Operand_b) < K2 of
                    true -> begin
                        case not '[]u64.all'(lists:nth(todo + 1, Operand_b), It == 0) of
                            true -> ok;
                            false -> ok
                        end,
                        'Integer.shrink_tail_zeros'(B0),
                        B11 = #{digits => '[]u64.clone'(lists:nth(todo + 1, Operand_b)), signum => 1, {vbeam, type} => 'Integer'},
                    end;
                    false -> begin
                        case not '[]u64.all'(lists:nth(todo + 1, Operand_b), It == 0) of
                            true -> ok;
                            false -> ok
                        end,
                        'Integer.shrink_tail_zeros'(B0),
                        case not '[]u64.all'(lists:nth(todo + 1, Operand_b), It == 0) of
                            true -> ok;
                            false -> ok
                        end,
                        'Integer.shrink_tail_zeros'(B11),
                        B21 = #{digits => '[]u64.clone'(lists:nth(todo + 1, Operand_b)), signum => 1, {vbeam, type} => 'Integer'},
                    end
                end
            end,
            P0 = A0 * B0,
            Ptemp = A2 + A0,
            Qtemp = B21 + B0,
            Vm1 = (Ptemp - A1) * (Qtemp - B11),
            Ptemp1 = A1,
            Qtemp1 = B11,
            P1 = Ptemp1 * Qtemp1,
            P2 = ('Integer.left_shift'((Ptemp1 + A2), 1) - A0) * ('Integer.left_shift'((Qtemp1 + B21), 1) - B0),
            Pinf = A2 * B21,
            T2 = element(1, 'Integer.div_mod_internal'((P2 - Vm1), #{digits => [todo], signum => 1, is_const => true, {vbeam, type} => 'Integer'})),
            Tm1 = 'Integer.right_shift'((P1 - Vm1), 1),
            T1 = P1 - P0,
            T21 = 'Integer.right_shift'((T2 - T1), 1),
            T11 = (T1 - Tm1 - Pinf),
            T22 = T21 - 'Integer.left_shift'(Pinf, 1),
            Tm11 = Tm1 - T22,
            S = todo * 60,
            Result = 'Integer.left_shift'(('Integer.left_shift'(('Integer.left_shift'(('Integer.left_shift'(Pinf, S) + T22), S) + T11), S) + Tm11), S) + P0,
            Storage = '[]u64.clone'(maps:get(digits, Result)),
        end
        end.

pow2(K) ->
    Ret = [],
    bit_set(Ret, K),
    #{signum => 1, digits => Ret, {vbeam, type} => 'Integer'}.

left_shift_digits_in_place(A, Amount) ->
    Old_len = length(A),
    Elem_size = maps:get(element_size, A),
    % TODO: unhandled stmt type
    ok
right_shift_digits_in_place(A, Amount) ->
    '[]u64.drop'(A, Amount),
    ok.

add_in_place(A, B) ->
    Len_a = length(A),
    Len_b = length(B),
    Max = imax(Len_a, Len_b),
    Min = imin(Len_a, Len_b),
    Carry = todo,
    {Partial, Carry1} = lists:foldl(fun(Index, {PartialAcc, CarryAcc}) ->
        PartialOut = CarryAcc + lists:nth(Index + 1, A) + lists:nth(Index + 1, B),
        CarryOut = todo,
        {PartialOut, CarryOut}
    end, {Partial, Carry}, lists:seq(0, Min - 1)),
    case Len_a >= Len_b of
        true -> ok;
        false -> ok
    end,
    case Carry1 > 0 of
        true -> A bsl Carry1;
        false -> ok
    end.

subtract_in_place(A, B) ->
    Len_a = length(A),
    Len_b = length(B),
    Max = imax(Len_a, Len_b),
    Min = imin(Len_a, Len_b),
    Borrow = false,
    {A_digit, B_digit, Borrow1} = lists:foldl(fun(Index, {A_digitAcc, B_digitAcc, BorrowAcc}) ->
        A_digitOut = lists:nth(Index + 1, A),
        B_digitOut = lists:nth(Index + 1, B) + case Borrow1 of
            true -> todo;
            false -> todo
        end,
        BorrowOut = A_digitAcc < B_digitAcc,
        case Borrow1 of
            true -> ok;
            false -> ok
        end,
        {A_digitOut, B_digitOut, BorrowOut}
    end, {A_digit, B_digit, Borrow}, lists:seq(0, Min - 1)),
    case Len_a >= Len_b of
        true -> ok;
        false -> '[]u64.clear'(A)
    end.
