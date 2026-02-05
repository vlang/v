-module('v.math.big').
-export([shrink_tail_zeros/1, 'Integer.shrink_tail_zeros'/1, debug_u64_str/1, debug_u32_str/1, found_multiplication_base_case/3, karatsuba_multiply_digit_array/3, toom3_multiply_digit_array/3, pow2/1, left_shift_digits_in_place/2, right_shift_digits_in_place/2, add_in_place/2, subtract_in_place/2]).

shrink_tail_zeros(A) ->
    Alen = length(A),
    % TODO: for alen > 0 && a[alen - 1] == 0 {
    % TODO: {a.len = alen;}

'Integer.shrink_tail_zeros'(I) ->
    Alen = length(maps:get(digits, I)),
    % TODO: for alen > 0 && i.digits[alen - 1] == 0 {
    % TODO: {i.digits.len = alen;}

debug_u64_str(A) ->
    Sb = new_builder(30),
    'Builder.write_string'(Sb, <<"[">>),
    First = true,
    First1 = lists:foldl(fun(I, FirstAcc) ->
        case !First1 of
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
                    B << Curr_u321,
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
        true -> B << Curr_u322;
        false -> ok
    end,
    Blen = length(B),
    % TODO: for blen > 0 && b[blen - 1] == 0 {
    % TODO: {b.len = blen;}
    Sb = new_builder(30),
    'Builder.write_string'(Sb, <<"[">>),
    First = true,
    First1 = lists:foldl(fun(I, FirstAcc) ->
        case !First1 of
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
    case length(Operand_a) == 0 || length(Operand_b) == 0 of
        true -> begin
            '[]u64.clear'(Storage),
            true
        end;
        false -> ok
    end,
    case length(Operand_a) < length(Operand_b) of
        true -> begin
            multiply_digit_array(Operand_b, Operand_a, Storage),
            true
        end;
        false -> ok
    end,
    case length(Operand_b) == 1 of
        true -> begin
            multiply_array_by_digit(Operand_a, lists:nth(1, Operand_b), Storage),
            true
        end;
        false -> ok
    end,
    false.

karatsuba_multiply_digit_array(Operand_a, Operand_b, Storage) ->
    case found_multiplication_base_case(Operand_a, Operand_b, Storage) of
        true -> ok;
        false -> ok
    end,
    Half = imax(length(Operand_a), length(Operand_b)) / 2,
    A_l = todo,
    A_h = todo,
    B_l = [],
    B_h = [],
    case Half <= length(Operand_b) of
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
    ok.

toom3_multiply_digit_array(Operand_a, Operand_b, Storage) ->
    case found_multiplication_base_case(Operand_a, Operand_b, Storage) of
        true -> ok;
        false -> ok
    end,
    K = (length(Operand_a) + 2) / 3,
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
                case !'[]u64.all'(lists:nth(todo + 1, Operand_b), It == 0) of
                    true -> ok;
                    false -> ok
                end,
                'Integer.shrink_tail_zeros'(B0),
                B11 = #{digits => '[]u64.clone'(lists:nth(todo + 1, Operand_b)), signum => 1, {vbeam, type} => 'Integer'},
            end;
            false -> begin
                case !'[]u64.all'(lists:nth(todo + 1, Operand_b), It == 0) of
                    true -> ok;
                    false -> ok
                end,
                'Integer.shrink_tail_zeros'(B0),
                case !'[]u64.all'(lists:nth(todo + 1, Operand_b), It == 0) of
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

pow2(K) ->
    Ret = [],
    bit_set(Ret, K),
    #{signum => 1, digits => Ret, {vbeam, type} => 'Integer'}.

left_shift_digits_in_place(A, Amount) ->
    Old_len = length(A),
    Elem_size = maps:get(element_size, A),
    % TODO: {a.grow_len(amount);sptr := &u8(a.data);dptr := &u8(a.data) + u64(amount) * u64(elem_size);math.big.vmemmove(dptr, sptr, u64(old_len) * u64(elem_size));math.big.vmemset(sptr, 0, u64(amount) * u64(elem_size));}

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
        true -> A << Carry1;
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
