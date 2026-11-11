module ssa

fn (mut b Builder) negate_wide_integer_limbs(block BlockID, low ValueID, high ValueID) (ValueID, ValueID) {
	zero := b.m.get_or_add_const(b.u64_type, '0')
	negated_low := b.block_instr2(.sub, block, b.u64_type, zero, low)
	borrow := b.block_instr2(.ne, block, b.i1_type, low, zero)
	borrow64 := b.block_instr1(.zext, block, b.u64_type, borrow)
	negated_high := b.block_instr2(.sub, block, b.u64_type, zero, high)
	carried_high := b.block_instr2(.sub, block, b.u64_type, negated_high, borrow64)
	return negated_low, carried_high
}

fn (mut b Builder) build_wide_integer_division(lhs ValueID, rhs ValueID, signed bool, remainder bool) ValueID {
	lhs_low, lhs_high := b.wide_integer_limbs(b.cur_block, lhs)
	rhs_low, rhs_high := b.wide_integer_limbs(b.cur_block, rhs)
	mut dividend_low := lhs_low
	mut dividend_high := lhs_high
	mut divisor_low := rhs_low
	mut divisor_high := rhs_high
	mut negate_result := b.m.get_or_add_const(b.i1_type, '0')
	if signed {
		zero := b.m.get_or_add_const(b.u64_type, '0')
		lhs_negative := b.block_instr2(.lt, b.cur_block, b.i1_type, lhs_high, zero)
		rhs_negative := b.block_instr2(.lt, b.cur_block, b.i1_type, rhs_high, zero)
		lhs_neg_low, lhs_neg_high := b.negate_wide_integer_limbs(b.cur_block, lhs_low, lhs_high)
		rhs_neg_low, rhs_neg_high := b.negate_wide_integer_limbs(b.cur_block, rhs_low, rhs_high)
		dividend_low = b.block_instr3(.select, b.cur_block, b.u64_type, lhs_negative, lhs_neg_low, lhs_low)
		dividend_high = b.block_instr3(.select, b.cur_block, b.u64_type, lhs_negative, lhs_neg_high, lhs_high)
		divisor_low = b.block_instr3(.select, b.cur_block, b.u64_type, rhs_negative, rhs_neg_low, rhs_low)
		divisor_high = b.block_instr3(.select, b.cur_block, b.u64_type, rhs_negative, rhs_neg_high, rhs_high)
		// Division truncates toward zero; the remainder has the dividend's sign.
		// Unsigned magnitudes also represent abs(min_i128), so min/-1 wraps.
		negate_result = if remainder {
			lhs_negative
		} else {
			b.block_instr2(.xor, b.cur_block, b.i1_type, lhs_negative, rhs_negative)
		}
	}
	func_id := b.ensure_wide_integer_divmod()
	wide_type := b.wide_integer_type()
	callee := b.m.add_value(.func_ref, wide_type, '__ssa_u128_divmod', func_id)
	return_remainder := b.m.get_or_add_const(b.i1_type, if remainder { '1' } else { '0' })
	result := b.m.add_instr(.call, b.cur_block, wide_type,
		[callee, dividend_low, dividend_high, divisor_low, divisor_high, return_remainder])
	if !signed {
		return result
	}
	low, high := b.wide_integer_limbs(b.cur_block, result)
	negated_low, negated_high := b.negate_wide_integer_limbs(b.cur_block, low, high)
	signed_low := b.block_instr3(.select, b.cur_block, b.u64_type, negate_result, negated_low, low)
	signed_high := b.block_instr3(.select, b.cur_block, b.u64_type, negate_result, negated_high, high)
	return b.make_wide_integer(b.cur_block, signed_low, signed_high)
}

// Share one unsigned long-division implementation between signed/unsigned /
// and %. It consumes the dividend from most to least significant bit, retaining
// the quotient and remainder in two 64-bit limbs. Each remainder is at most the
// consumed input prefix, so shifting it never needs a 129th bit in these 128 passes.
fn (mut b Builder) ensure_wide_integer_divmod() int {
	if func_id := b.fn_ids['__ssa_u128_divmod'] {
		return func_id
	}
	wide_type := b.wide_integer_type()
	func_id := b.register_synthetic_function('__ssa_u128_divmod', wide_type,
		[b.u64_type, b.u64_type, b.u64_type, b.u64_type, b.i1_type])
	entry := b.m.add_block(func_id, 'wide_divide_entry')
	zero_divisor := b.m.add_block(func_id, 'wide_divide_zero')
	division_error := b.m.add_block(func_id, 'wide_divide_error')
	modulo_error := b.m.add_block(func_id, 'wide_modulo_error')
	loop := b.m.add_block(func_id, 'wide_divide_loop')
	done := b.m.add_block(func_id, 'wide_divide_done')
	dividend_low := b.func_add_argument(func_id, b.u64_type, 'dividend_low')
	dividend_high := b.func_add_argument(func_id, b.u64_type, 'dividend_high')
	divisor_low := b.func_add_argument(func_id, b.u64_type, 'divisor_low')
	divisor_high := b.func_add_argument(func_id, b.u64_type, 'divisor_high')
	return_remainder := b.func_add_argument(func_id, b.i1_type, 'return_remainder')
	ptr_u64 := b.m.type_store.get_ptr(b.u64_type)
	input_low_slot := b.block_instr0(.alloca, entry, ptr_u64)
	input_high_slot := b.block_instr0(.alloca, entry, ptr_u64)
	quotient_low_slot := b.block_instr0(.alloca, entry, ptr_u64)
	quotient_high_slot := b.block_instr0(.alloca, entry, ptr_u64)
	remainder_low_slot := b.block_instr0(.alloca, entry, ptr_u64)
	remainder_high_slot := b.block_instr0(.alloca, entry, ptr_u64)
	count_slot := b.block_instr0(.alloca, entry, ptr_u64)
	zero := b.m.get_or_add_const(b.u64_type, '0')
	one := b.m.get_or_add_const(b.u64_type, '1')
	top_bit := b.m.get_or_add_const(b.u64_type, '63')
	count := b.m.get_or_add_const(b.u64_type, '128')
	b.block_instr2(.store, entry, b.void_type, dividend_low, input_low_slot)
	b.block_instr2(.store, entry, b.void_type, dividend_high, input_high_slot)
	for slot in [quotient_low_slot, quotient_high_slot, remainder_low_slot, remainder_high_slot] {
		b.block_instr2(.store, entry, b.void_type, zero, slot)
	}
	b.block_instr2(.store, entry, b.void_type, count, count_slot)
	divisor := b.block_instr2(.or_, entry, b.u64_type, divisor_low, divisor_high)
	is_zero := b.block_instr2(.eq, entry, b.i1_type, divisor, zero)
	b.block_instr3(.br, entry, b.void_type, is_zero, ValueID(zero_divisor), ValueID(loop))
	b.block_instr3(.br, zero_divisor, b.void_type, return_remainder, ValueID(modulo_error), ValueID(division_error))
	panic_ref := b.m.add_value(.func_ref, b.void_type, 'panic', b.fn_ids['panic'])
	division_message := b.m.add_value(.string_literal, b.str_type, 'division by zero', 0)
	modulo_message := b.m.add_value(.string_literal, b.str_type, 'modulo by zero', 0)
	b.block_instr2(.call, division_error, b.void_type, panic_ref, division_message)
	b.block_instr0(.unreachable, division_error, b.void_type)
	b.block_instr2(.call, modulo_error, b.void_type, panic_ref, modulo_message)
	b.block_instr0(.unreachable, modulo_error, b.void_type)
	input_low := b.block_instr1(.load, loop, b.u64_type, input_low_slot)
	input_high := b.block_instr1(.load, loop, b.u64_type, input_high_slot)
	quotient_low := b.block_instr1(.load, loop, b.u64_type, quotient_low_slot)
	quotient_high := b.block_instr1(.load, loop, b.u64_type, quotient_high_slot)
	remainder_low := b.block_instr1(.load, loop, b.u64_type, remainder_low_slot)
	remainder_high := b.block_instr1(.load, loop, b.u64_type, remainder_high_slot)
	remaining := b.block_instr1(.load, loop, b.u64_type, count_slot)
	bit := b.block_instr2(.lshr, loop, b.u64_type, input_high, top_bit)
	shifted_input_low := b.block_instr2(.shl, loop, b.u64_type, input_low, one)
	shifted_input_high := b.block_instr2(.shl, loop, b.u64_type, input_high, one)
	input_carry := b.block_instr2(.lshr, loop, b.u64_type, input_low, top_bit)
	next_input_high := b.block_instr2(.or_, loop, b.u64_type, shifted_input_high, input_carry)
	shifted_remainder_low := b.block_instr2(.shl, loop, b.u64_type, remainder_low, one)
	shifted_remainder_high := b.block_instr2(.shl, loop, b.u64_type, remainder_high, one)
	remainder_carry := b.block_instr2(.lshr, loop, b.u64_type, remainder_low, top_bit)
	next_remainder_low := b.block_instr2(.or_, loop, b.u64_type, shifted_remainder_low, bit)
	next_remainder_high := b.block_instr2(.or_, loop, b.u64_type, shifted_remainder_high, remainder_carry)
	high_greater := b.block_instr2(.ugt, loop, b.i1_type, next_remainder_high, divisor_high)
	high_equal := b.block_instr2(.eq, loop, b.i1_type, next_remainder_high, divisor_high)
	low_at_least := b.block_instr2(.uge, loop, b.i1_type, next_remainder_low, divisor_low)
	tied_at_least := b.block_instr2(.and_, loop, b.i1_type, high_equal, low_at_least)
	subtract := b.block_instr2(.or_, loop, b.i1_type, high_greater, tied_at_least)
	difference_low := b.block_instr2(.sub, loop, b.u64_type, next_remainder_low, divisor_low)
	borrow := b.block_instr2(.ult, loop, b.i1_type, next_remainder_low, divisor_low)
	borrow64 := b.block_instr1(.zext, loop, b.u64_type, borrow)
	difference_high := b.block_instr2(.sub, loop, b.u64_type, next_remainder_high, divisor_high)
	carried_difference_high := b.block_instr2(.sub, loop, b.u64_type, difference_high, borrow64)
	final_remainder_low := b.block_instr3(.select, loop, b.u64_type, subtract, difference_low, next_remainder_low)
	final_remainder_high := b.block_instr3(.select, loop, b.u64_type, subtract, carried_difference_high, next_remainder_high)
	shifted_quotient_low := b.block_instr2(.shl, loop, b.u64_type, quotient_low, one)
	shifted_quotient_high := b.block_instr2(.shl, loop, b.u64_type, quotient_high, one)
	quotient_carry := b.block_instr2(.lshr, loop, b.u64_type, quotient_low, top_bit)
	next_quotient_high := b.block_instr2(.or_, loop, b.u64_type, shifted_quotient_high, quotient_carry)
	quotient_bit := b.block_instr1(.zext, loop, b.u64_type, subtract)
	next_quotient_low := b.block_instr2(.or_, loop, b.u64_type, shifted_quotient_low, quotient_bit)
	b.block_instr2(.store, loop, b.void_type, shifted_input_low, input_low_slot)
	b.block_instr2(.store, loop, b.void_type, next_input_high, input_high_slot)
	b.block_instr2(.store, loop, b.void_type, next_quotient_low, quotient_low_slot)
	b.block_instr2(.store, loop, b.void_type, next_quotient_high, quotient_high_slot)
	b.block_instr2(.store, loop, b.void_type, final_remainder_low, remainder_low_slot)
	b.block_instr2(.store, loop, b.void_type, final_remainder_high, remainder_high_slot)
	next_count := b.block_instr2(.sub, loop, b.u64_type, remaining, one)
	b.block_instr2(.store, loop, b.void_type, next_count, count_slot)
	more := b.block_instr2(.ne, loop, b.i1_type, next_count, zero)
	b.block_instr3(.br, loop, b.void_type, more, ValueID(loop), ValueID(done))
	result_low := b.block_instr3(.select, done, b.u64_type, return_remainder, final_remainder_low, next_quotient_low)
	result_high := b.block_instr3(.select, done, b.u64_type, return_remainder, final_remainder_high, next_quotient_high)
	result := b.make_wide_integer(done, result_low, result_high)
	b.block_instr1(.ret, done, b.void_type, result)
	return func_id
}
