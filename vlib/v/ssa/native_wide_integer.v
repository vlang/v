module ssa

import strconv
import v.flat

// 128-bit integer lowering for the native backends.
//
// An `i128` or `u128` value is a struct of two u64 limbs, see wide_integer_type.
// Bitwise operators, addition, subtraction, negation and comparisons stay inline
// on the limbs. Multiplication, division, remainder, variable shifts and
// floating-point conversions call helper functions that are synthesized on first
// use, so a program that does not need them carries none of their code. The
// helpers exchange limbs through u64 arguments and out pointers, which every
// 64-bit native backend already passes like the arguments of a V function.
//
// Every shift inside the helpers uses a count from 0 through 63. x64 answers 0
// for a larger count while ARM64 masks it, so a larger count would make the two
// backends disagree.

const wide_helper_mul = 'v3_wide_mul'
const wide_helper_udivmod = 'v3_wide_udivmod'
const wide_helper_sdivmod = 'v3_wide_sdivmod'
const wide_helper_shl = 'v3_wide_shl'
const wide_helper_lshr = 'v3_wide_lshr'
const wide_helper_ashr = 'v3_wide_ashr'
const wide_helper_to_f64 = 'v3_wide_to_f64'
const wide_helper_from_f64 = 'v3_wide_from_f64'

const wide_shift_ops = [flat.Op.left_shift, .right_shift, .right_shift_unsigned]

// wide_integer_literal_limbs returns the two's complement limbs of an integer
// literal with an optional sign, base prefix and `_` separators. The digits are
// accumulated in four 32-bit limbs, so no 128-bit arithmetic is needed here.
fn wide_integer_literal_limbs(text string) (u64, u64) {
	mut digits := text.replace('_', '')
	negative := digits.starts_with('-')
	if negative || digits.starts_with('+') {
		digits = digits[1..]
	}
	mut base := u64(10)
	if digits.len > 2 && digits[0] == `0` {
		prefix := digits[1]
		if prefix == `x` || prefix == `X` {
			base = 16
		} else if prefix == `o` || prefix == `O` {
			base = 8
		} else if prefix == `b` || prefix == `B` {
			base = 2
		}
		if base != 10 {
			digits = digits[2..]
		}
	}
	if digits.len == 0 {
		panic('ssa: invalid 128-bit integer literal `${text}`')
	}
	mut limbs := [u64(0), 0, 0, 0]!
	for ch in digits {
		digit := if ch >= `0` && ch <= `9` {
			u64(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			u64(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			u64(ch - `A`) + 10
		} else {
			base
		}
		if digit >= base {
			panic('ssa: invalid 128-bit integer literal `${text}`')
		}
		mut carry := digit
		for i in 0 .. 4 {
			product := limbs[i] * base + carry
			limbs[i] = product & 0xffffffff
			carry = product >> 32
		}
		if carry != 0 {
			panic('ssa: unsupported 128-bit integer literal `${text}` wider than 128 bits')
		}
	}
	mut low := limbs[0] | (limbs[1] << 32)
	mut high := limbs[2] | (limbs[3] << 32)
	if negative {
		// Two's complement negation: invert both limbs, then add one to the low
		// limb and carry into the high limb when the low limb wraps to zero.
		high = ~high
		if low == 0 {
			high++
		}
		low = u64(0) - low
	}
	return low, high
}

// wide_literal_fits_scalar reports whether a literal's magnitude fits in 64 bits.
fn wide_literal_fits_scalar(text string) bool {
	strconv.parse_uint(text.trim_left('-'), 0, 64) or { return false }
	return true
}

// wide_source_type_is_signed reports whether a source type, after pointers and
// aliases, is `i128`.
fn (b &Builder) wide_source_type_is_signed(type_id u32) bool {
	mut id := ssa_pointer_base_source_type(type_id)
	for _ in 0 .. 16 {
		if id == ssa_i128_source_type {
			return true
		}
		if id == ssa_u128_source_type {
			return false
		}
		id = ssa_pointer_base_source_type(b.type_alias_target(id) or { return false })
	}
	return false
}

fn (mut b Builder) wide_u64(value string) ValueID {
	return b.m.get_or_add_const(b.u64_type, value)
}

fn (mut b Builder) wide_signed_flag(signed bool) ValueID {
	return b.wide_u64(if signed { '1' } else { '0' })
}

fn (mut b Builder) wide_op(block BlockID, op OpCode, x ValueID, y ValueID) ValueID {
	return b.block_instr2(op, block, b.u64_type, x, y)
}

fn (mut b Builder) wide_cond(block BlockID, op OpCode, x ValueID, y ValueID) ValueID {
	return b.block_instr2(op, block, b.i1_type, x, y)
}

// wide_flag widens an i1 condition to a u64 that is 0 or 1.
fn (mut b Builder) wide_flag(block BlockID, cond ValueID) ValueID {
	return b.block_instr1(.zext, block, b.u64_type, cond)
}

// wide_mask turns an i1 condition into a u64 with every bit set or clear.
fn (mut b Builder) wide_mask(block BlockID, cond ValueID) ValueID {
	return b.wide_op(block, .sub, b.wide_u64('0'), b.wide_flag(block, cond))
}

fn (mut b Builder) wide_add_limbs(block BlockID, x_low ValueID, x_high ValueID, y_low ValueID, y_high ValueID) (ValueID, ValueID) {
	low := b.wide_op(block, .add, x_low, y_low)
	carry := b.wide_flag(block, b.wide_cond(block, .ult, low, x_low))
	high := b.wide_op(block, .add, b.wide_op(block, .add, x_high, y_high), carry)
	return low, high
}

fn (mut b Builder) wide_sub_limbs(block BlockID, x_low ValueID, x_high ValueID, y_low ValueID, y_high ValueID) (ValueID, ValueID) {
	low := b.wide_op(block, .sub, x_low, y_low)
	borrow := b.wide_flag(block, b.wide_cond(block, .ult, x_low, y_low))
	high := b.wide_op(block, .sub, b.wide_op(block, .sub, x_high, y_high), borrow)
	return low, high
}

// wide_negate_if negates a value when `mask` has every bit set and keeps it when
// `mask` is zero: (x ^ mask) - mask.
fn (mut b Builder) wide_negate_if(block BlockID, low ValueID, high ValueID, mask ValueID) (ValueID, ValueID) {
	flipped_low := b.wide_op(block, .xor, low, mask)
	flipped_high := b.wide_op(block, .xor, high, mask)
	return b.wide_sub_limbs(block, flipped_low, flipped_high, mask, mask)
}

// wide_uge_limbs compares two unsigned 128-bit values: x >= y.
fn (mut b Builder) wide_uge_limbs(block BlockID, x_low ValueID, x_high ValueID, y_low ValueID, y_high ValueID) ValueID {
	high_above := b.wide_cond(block, .ugt, x_high, y_high)
	high_equal := b.wide_cond(block, .eq, x_high, y_high)
	low_at_least := b.wide_cond(block, .uge, x_low, y_low)
	tied := b.block_instr2(.and_, block, b.i1_type, high_equal, low_at_least)
	return b.block_instr2(.or_, block, b.i1_type, high_above, tied)
}

// wide_clz64 counts the leading zero bits of a u64 by binary search, without
// branches: each step shifts by 0 or by its width, so every count stays in range.
fn (mut b Builder) wide_clz64(block BlockID, value ValueID) ValueID {
	mut x := value
	mut count := b.wide_u64('0')
	for bits in [32, 16, 8, 4, 2, 1] {
		top := b.wide_op(block, .lshr, x, b.wide_u64('${64 - bits}'))
		empty := b.wide_flag(block, b.wide_cond(block, .eq, top, b.wide_u64('0')))
		step := b.wide_op(block, .mul, empty, b.wide_u64('${bits}'))
		count = b.wide_op(block, .add, count, step)
		x = b.wide_op(block, .shl, x, step)
	}
	// Zero passes every test above and stops one short of 64.
	zero := b.wide_flag(block, b.wide_cond(block, .eq, value, b.wide_u64('0')))
	return b.wide_op(block, .add, count, zero)
}

// wide_clz128 counts the leading zero bits of a 128-bit value.
fn (mut b Builder) wide_clz128(block BlockID, low ValueID, high ValueID) ValueID {
	high_zeros := b.wide_clz64(block, high)
	low_zeros := b.wide_clz64(block, low)
	high_empty := b.wide_mask(block, b.wide_cond(block, .eq, high, b.wide_u64('0')))
	return b.wide_op(block, .add, high_zeros, b.wide_op(block, .and_, low_zeros, high_empty))
}

fn (mut b Builder) wide_store_limbs(block BlockID, out ValueID, low ValueID, high ValueID) {
	wide_type := b.wide_integer_type()
	low_ptr := b.block_struct_field_ptr(block, out, wide_type, 0)
	high_ptr := b.block_struct_field_ptr(block, out, wide_type, 1)
	b.block_instr2(.store, block, b.void_type, low, low_ptr)
	b.block_instr2(.store, block, b.void_type, high, high_ptr)
}

fn (mut b Builder) wide_load_limbs(block BlockID, slot ValueID) (ValueID, ValueID) {
	wide_type := b.wide_integer_type()
	low_ptr := b.block_struct_field_ptr(block, slot, wide_type, 0)
	high_ptr := b.block_struct_field_ptr(block, slot, wide_type, 1)
	low := b.block_instr1(.load, block, b.u64_type, low_ptr)
	high := b.block_instr1(.load, block, b.u64_type, high_ptr)
	return low, high
}

fn (mut b Builder) wide_block_slot(block BlockID, typ TypeID) ValueID {
	return b.block_instr0(.alloca, block, b.m.type_store.get_ptr(typ))
}

fn (mut b Builder) wide_block_call(block BlockID, name string, ret TypeID, args []ValueID) ValueID {
	func_id := b.wide_helper(name)
	mut operands := []ValueID{cap: args.len + 1}
	operands << b.m.add_value(.func_ref, ret, name, func_id)
	operands << args
	return b.m.add_instr(.call, block, ret, operands)
}

// wide_helper returns the function id of a 128-bit helper, generating its body
// on first use. The id is recorded before the body is built, because the signed
// division body calls the unsigned one.
fn (mut b Builder) wide_helper(name string) int {
	if id := b.wide_helper_ids[name] {
		return id
	}
	u := b.u64_type
	wide_ptr := b.m.type_store.get_ptr(b.wide_integer_type())
	mut ret := b.void_type
	mut params := []TypeID{}
	if name == wide_helper_mul {
		params = [u, u, u, u, wide_ptr]
	} else if name in [wide_helper_udivmod, wide_helper_sdivmod] {
		params = [u, u, u, u, wide_ptr, wide_ptr]
	} else if name in [wide_helper_shl, wide_helper_lshr, wide_helper_ashr] {
		params = [u, u, u, wide_ptr]
	} else if name == wide_helper_to_f64 {
		params = [u, u, u]
		ret = b.f64_type
	} else if name == wide_helper_from_f64 {
		params = [b.f64_type, u, wide_ptr]
	} else {
		panic('ssa: unknown 128-bit integer helper `${name}`')
	}
	id := b.register_synthetic_function(name, ret, params)
	b.wide_helper_ids[name] = id
	if name == wide_helper_mul {
		b.generate_wide_mul_body(id)
	} else if name == wide_helper_udivmod {
		b.generate_wide_udivmod_body(id)
	} else if name == wide_helper_sdivmod {
		b.generate_wide_sdivmod_body(id)
	} else if name == wide_helper_to_f64 {
		b.generate_wide_to_f64_body(id)
	} else if name == wide_helper_from_f64 {
		b.generate_wide_from_f64_body(id)
	} else {
		b.generate_wide_shift_body(id, name)
	}
	return id
}

// generate_wide_mul_body keeps the low 128 bits of a product. The low limbs are
// multiplied in 32-bit halves so their full 128-bit product is available; the
// cross products with the high limbs only reach the high limb. Two's complement
// makes the same product correct for signed operands.
fn (mut b Builder) generate_wide_mul_body(func_id int) {
	u := b.u64_type
	entry := b.m.add_block(func_id, 'wide_mul_entry')
	x_low := b.func_add_argument(func_id, u, 'x_low')
	x_high := b.func_add_argument(func_id, u, 'x_high')
	y_low := b.func_add_argument(func_id, u, 'y_low')
	y_high := b.func_add_argument(func_id, u, 'y_high')
	out := b.func_add_argument(func_id, b.m.type_store.get_ptr(b.wide_integer_type()), 'out')
	half_mask := b.wide_u64('4294967295')
	half := b.wide_u64('32')
	x0 := b.wide_op(entry, .and_, x_low, half_mask)
	x1 := b.wide_op(entry, .lshr, x_low, half)
	y0 := b.wide_op(entry, .and_, y_low, half_mask)
	y1 := b.wide_op(entry, .lshr, y_low, half)
	p0 := b.wide_op(entry, .mul, x0, y0)
	p1 := b.wide_op(entry, .add, b.wide_op(entry, .mul, x0, y1), b.wide_op(entry, .lshr, p0, half))
	p2 := b.wide_op(entry, .add, b.wide_op(entry, .mul, x1, y0), b.wide_op(entry, .and_, p1,
		half_mask))
	low := b.wide_op(entry, .or_, b.wide_op(entry, .shl, p2, half), b.wide_op(entry, .and_,
		p0, half_mask))
	mut high := b.wide_op(entry, .mul, x1, y1)
	high = b.wide_op(entry, .add, high, b.wide_op(entry, .lshr, p1, half))
	high = b.wide_op(entry, .add, high, b.wide_op(entry, .lshr, p2, half))
	high = b.wide_op(entry, .add, high, b.wide_op(entry, .mul, x_low, y_high))
	high = b.wide_op(entry, .add, high, b.wide_op(entry, .mul, x_high, y_low))
	b.wide_store_limbs(entry, out, low, high)
	b.block_instr0(.ret, entry, b.void_type)
}

// generate_wide_shift_body shifts by a u64 count. A count of 128 or more shifts
// every bit out, leaving zero or, for a signed right shift, the sign.
fn (mut b Builder) generate_wide_shift_body(func_id int, name string) {
	u := b.u64_type
	entry := b.m.add_block(func_id, 'wide_shift_entry')
	low := b.func_add_argument(func_id, u, 'low')
	high := b.func_add_argument(func_id, u, 'high')
	count := b.func_add_argument(func_id, u, 'count')
	out := b.func_add_argument(func_id, b.m.type_store.get_ptr(b.wide_integer_type()), 'out')
	zero := b.wide_u64('0')
	limb_bits := b.wide_u64('64')
	fill := if name == wide_helper_ashr {
		b.wide_op(entry, .ashr, high, b.wide_u64('63'))
	} else {
		zero
	}
	all_out := b.m.add_block(func_id, 'wide_shift_all_out')
	in_range := b.m.add_block(func_id, 'wide_shift_in_range')
	b.block_instr3(.br, entry, b.void_type, b.wide_cond(entry, .uge, count, b.wide_u64('128')),
		ValueID(all_out), ValueID(in_range))
	b.wide_store_limbs(all_out, out, fill, fill)
	b.block_instr0(.ret, all_out, b.void_type)

	whole := b.m.add_block(func_id, 'wide_shift_whole_limb')
	below_limb := b.m.add_block(func_id, 'wide_shift_below_limb')
	b.block_instr3(.br, in_range, b.void_type, b.wide_cond(in_range, .uge, count, limb_bits),
		ValueID(whole), ValueID(below_limb))
	// 64 through 127: one limb moves into the other and shifts by the rest.
	rest := b.wide_op(whole, .sub, count, limb_bits)
	if name == wide_helper_shl {
		b.wide_store_limbs(whole, out, zero, b.wide_op(whole, .shl, low, rest))
	} else if name == wide_helper_lshr {
		b.wide_store_limbs(whole, out, b.wide_op(whole, .lshr, high, rest), zero)
	} else {
		b.wide_store_limbs(whole, out, b.wide_op(whole, .ashr, high, rest), fill)
	}
	b.block_instr0(.ret, whole, b.void_type)

	unchanged := b.m.add_block(func_id, 'wide_shift_unchanged')
	partial := b.m.add_block(func_id, 'wide_shift_partial')
	b.block_instr3(.br, below_limb, b.void_type, b.wide_cond(below_limb, .eq, count, zero),
		ValueID(unchanged), ValueID(partial))
	b.wide_store_limbs(unchanged, out, low, high)
	b.block_instr0(.ret, unchanged, b.void_type)
	// 1 through 63: bits cross between the limbs.
	back := b.wide_op(partial, .sub, limb_bits, count)
	if name == wide_helper_shl {
		next_low := b.wide_op(partial, .shl, low, count)
		next_high := b.wide_op(partial, .or_, b.wide_op(partial, .shl, high, count),
			b.wide_op(partial, .lshr, low, back))
		b.wide_store_limbs(partial, out, next_low, next_high)
	} else {
		next_low := b.wide_op(partial, .or_, b.wide_op(partial, .lshr, low, count),
			b.wide_op(partial, .shl, high, back))
		right := if name == wide_helper_ashr { OpCode.ashr } else { OpCode.lshr }
		b.wide_store_limbs(partial, out, next_low, b.wide_op(partial, right, high, count))
	}
	b.block_instr0(.ret, partial, b.void_type)
}

// generate_wide_udivmod_body divides two u128 values and writes the quotient and
// the remainder. The divisor is never zero; callers check that first. It follows
// compiler-rt's __udivmodti4 (thirdparty/builtins/compiler_builtins.c): operands
// that fit in 64 bits take one hardware division, a dividend below the divisor is
// its own remainder, and everything else runs a shift-and-subtract loop over the
// quotient bits, starting from the divisor aligned with the dividend's top bit.
// A divisor below 2^32 additionally takes a fast path of four 64-bit divisions,
// one per 32-bit digit of the dividend, because the remainder of each step and
// the next digit fit in 64 bits together.
fn (mut b Builder) generate_wide_udivmod_body(func_id int) {
	u := b.u64_type
	wide_ptr := b.m.type_store.get_ptr(b.wide_integer_type())
	entry := b.m.add_block(func_id, 'wide_udivmod_entry')
	n_low := b.func_add_argument(func_id, u, 'n_low')
	n_high := b.func_add_argument(func_id, u, 'n_high')
	d_low := b.func_add_argument(func_id, u, 'd_low')
	d_high := b.func_add_argument(func_id, u, 'd_high')
	quotient_out := b.func_add_argument(func_id, wide_ptr, 'quotient')
	remainder_out := b.func_add_argument(func_id, wide_ptr, 'remainder')
	zero := b.wide_u64('0')
	one := b.wide_u64('1')
	half := b.wide_u64('32')
	half_mask := b.wide_u64('4294967295')
	// The loop state lives in stack slots, declared up front in the entry block.
	quotient_low_slot := b.wide_block_slot(entry, u)
	quotient_high_slot := b.wide_block_slot(entry, u)
	remainder_low_slot := b.wide_block_slot(entry, u)
	remainder_high_slot := b.wide_block_slot(entry, u)
	divisor_low_slot := b.wide_block_slot(entry, u)
	divisor_high_slot := b.wide_block_slot(entry, u)
	steps_slot := b.wide_block_slot(entry, u)
	aligned_slot := b.wide_block_slot(entry, b.wide_integer_type())

	small := b.m.add_block(func_id, 'wide_udivmod_small')
	wide := b.m.add_block(func_id, 'wide_udivmod_wide')
	high_bits := b.wide_op(entry, .or_, n_high, d_high)
	b.block_instr3(.br, entry, b.void_type, b.wide_cond(entry, .eq, high_bits, zero),
		ValueID(small), ValueID(wide))
	b.wide_store_limbs(small, quotient_out, b.wide_op(small, .udiv, n_low, d_low), zero)
	b.wide_store_limbs(small, remainder_out, b.wide_op(small, .urem, n_low, d_low), zero)
	b.block_instr0(.ret, small, b.void_type)

	below := b.m.add_block(func_id, 'wide_udivmod_below')
	at_least := b.m.add_block(func_id, 'wide_udivmod_at_least')
	b.block_instr3(.br, wide, b.void_type, b.wide_uge_limbs(wide, n_low, n_high, d_low,
		d_high), ValueID(at_least), ValueID(below))
	b.wide_store_limbs(below, quotient_out, zero, zero)
	b.wide_store_limbs(below, remainder_out, n_low, n_high)
	b.block_instr0(.ret, below, b.void_type)

	short := b.m.add_block(func_id, 'wide_udivmod_short')
	general := b.m.add_block(func_id, 'wide_udivmod_general')
	short_high := b.wide_cond(at_least, .eq, d_high, zero)
	short_low := b.wide_cond(at_least, .eq, b.wide_op(at_least, .lshr, d_low, half), zero)
	b.block_instr3(.br, at_least, b.void_type, b.block_instr2(.and_, at_least, b.i1_type,
		short_high, short_low), ValueID(short), ValueID(general))
	digits := [
		b.wide_op(short, .lshr, n_high, half),
		b.wide_op(short, .and_, n_high, half_mask),
		b.wide_op(short, .lshr, n_low, half),
		b.wide_op(short, .and_, n_low, half_mask),
	]
	mut quotient_digits := []ValueID{cap: 4}
	mut remainder := zero
	for index, digit in digits {
		step := if index == 0 {
			digit
		} else {
			b.wide_op(short, .or_, b.wide_op(short, .shl, remainder, half), digit)
		}
		quotient_digits << b.wide_op(short, .udiv, step, d_low)
		remainder = b.wide_op(short, .urem, step, d_low)
	}
	b.wide_store_limbs(short, quotient_out, b.wide_op(short, .or_, b.wide_op(short, .shl,
		quotient_digits[2], half), quotient_digits[3]), b.wide_op(short, .or_, b.wide_op(short,
		.shl, quotient_digits[0], half), quotient_digits[1]))
	b.wide_store_limbs(short, remainder_out, remainder, zero)
	b.block_instr0(.ret, short, b.void_type)

	// The dividend is at least the divisor here, so the alignment is 0..127 and
	// the aligned divisor keeps every bit.
	alignment := b.wide_op(general, .sub, b.wide_clz128(general, d_low, d_high), b.wide_clz128(general,
		n_low, n_high))
	b.wide_block_call(general, wide_helper_shl, b.void_type, [d_low, d_high, alignment, aligned_slot])
	aligned_low, aligned_high := b.wide_load_limbs(general, aligned_slot)
	b.block_instr2(.store, general, b.void_type, zero, quotient_low_slot)
	b.block_instr2(.store, general, b.void_type, zero, quotient_high_slot)
	b.block_instr2(.store, general, b.void_type, n_low, remainder_low_slot)
	b.block_instr2(.store, general, b.void_type, n_high, remainder_high_slot)
	b.block_instr2(.store, general, b.void_type, aligned_low, divisor_low_slot)
	b.block_instr2(.store, general, b.void_type, aligned_high, divisor_high_slot)
	b.block_instr2(.store, general, b.void_type, b.wide_op(general, .add, alignment, one),
		steps_slot)
	step_block := b.m.add_block(func_id, 'wide_udivmod_step')
	done := b.m.add_block(func_id, 'wide_udivmod_done')
	b.block_instr1(.jmp, general, b.void_type, ValueID(step_block))

	quotient_low := b.block_instr1(.load, step_block, u, quotient_low_slot)
	quotient_high := b.block_instr1(.load, step_block, u, quotient_high_slot)
	remainder_low := b.block_instr1(.load, step_block, u, remainder_low_slot)
	remainder_high := b.block_instr1(.load, step_block, u, remainder_high_slot)
	divisor_low := b.block_instr1(.load, step_block, u, divisor_low_slot)
	divisor_high := b.block_instr1(.load, step_block, u, divisor_high_slot)
	steps := b.block_instr1(.load, step_block, u, steps_slot)
	top := b.wide_u64('63')
	// Make room for the next quotient bit.
	shifted_quotient_high := b.wide_op(step_block, .or_, b.wide_op(step_block, .shl, quotient_high,
		one), b.wide_op(step_block, .lshr, quotient_low, top))
	shifted_quotient_low := b.wide_op(step_block, .shl, quotient_low, one)
	// Subtract the divisor when it fits, without a branch: `take` is all ones then.
	take := b.wide_mask(step_block, b.wide_uge_limbs(step_block, remainder_low, remainder_high,
		divisor_low, divisor_high))
	next_remainder_low, next_remainder_high := b.wide_sub_limbs(step_block, remainder_low,
		remainder_high, b.wide_op(step_block, .and_, divisor_low, take), b.wide_op(step_block,
			.and_, divisor_high, take))
	next_quotient_low := b.wide_op(step_block, .or_, shifted_quotient_low, b.wide_op(step_block,
		.and_, take, one))
	next_divisor_low := b.wide_op(step_block, .or_, b.wide_op(step_block, .lshr, divisor_low,
		one), b.wide_op(step_block, .shl, divisor_high, top))
	next_divisor_high := b.wide_op(step_block, .lshr, divisor_high, one)
	next_steps := b.wide_op(step_block, .sub, steps, one)
	b.block_instr2(.store, step_block, b.void_type, next_quotient_low, quotient_low_slot)
	b.block_instr2(.store, step_block, b.void_type, shifted_quotient_high, quotient_high_slot)
	b.block_instr2(.store, step_block, b.void_type, next_remainder_low, remainder_low_slot)
	b.block_instr2(.store, step_block, b.void_type, next_remainder_high, remainder_high_slot)
	b.block_instr2(.store, step_block, b.void_type, next_divisor_low, divisor_low_slot)
	b.block_instr2(.store, step_block, b.void_type, next_divisor_high, divisor_high_slot)
	b.block_instr2(.store, step_block, b.void_type, next_steps, steps_slot)
	b.block_instr3(.br, step_block, b.void_type, b.wide_cond(step_block, .eq, next_steps, zero),
		ValueID(done), ValueID(step_block))

	b.wide_store_limbs(done, quotient_out, b.block_instr1(.load, done, u, quotient_low_slot),
		b.block_instr1(.load, done, u, quotient_high_slot))
	b.wide_store_limbs(done, remainder_out, b.block_instr1(.load, done, u, remainder_low_slot),
		b.block_instr1(.load, done, u, remainder_high_slot))
	b.block_instr0(.ret, done, b.void_type)
}

// generate_wide_sdivmod_body divides the magnitudes of two i128 values. The
// quotient is negative when the signs differ and the remainder takes the sign of
// the dividend, as in C. The minimum value divided by -1 wraps back to the
// minimum value and leaves a remainder of 0, matching the C backend.
fn (mut b Builder) generate_wide_sdivmod_body(func_id int) {
	u := b.u64_type
	wide_type := b.wide_integer_type()
	wide_ptr := b.m.type_store.get_ptr(wide_type)
	entry := b.m.add_block(func_id, 'wide_sdivmod_entry')
	n_low := b.func_add_argument(func_id, u, 'n_low')
	n_high := b.func_add_argument(func_id, u, 'n_high')
	d_low := b.func_add_argument(func_id, u, 'd_low')
	d_high := b.func_add_argument(func_id, u, 'd_high')
	quotient_out := b.func_add_argument(func_id, wide_ptr, 'quotient')
	remainder_out := b.func_add_argument(func_id, wide_ptr, 'remainder')
	top := b.wide_u64('63')
	n_sign := b.wide_op(entry, .ashr, n_high, top)
	d_sign := b.wide_op(entry, .ashr, d_high, top)
	n_abs_low, n_abs_high := b.wide_negate_if(entry, n_low, n_high, n_sign)
	d_abs_low, d_abs_high := b.wide_negate_if(entry, d_low, d_high, d_sign)
	quotient_slot := b.wide_block_slot(entry, wide_type)
	remainder_slot := b.wide_block_slot(entry, wide_type)
	b.wide_block_call(entry, wide_helper_udivmod, b.void_type, [n_abs_low, n_abs_high, d_abs_low,
		d_abs_high, quotient_slot, remainder_slot])
	quotient_low, quotient_high := b.wide_load_limbs(entry, quotient_slot)
	remainder_low, remainder_high := b.wide_load_limbs(entry, remainder_slot)
	quotient_sign := b.wide_op(entry, .xor, n_sign, d_sign)
	signed_quotient_low, signed_quotient_high := b.wide_negate_if(entry, quotient_low,
		quotient_high, quotient_sign)
	signed_remainder_low, signed_remainder_high := b.wide_negate_if(entry, remainder_low,
		remainder_high, n_sign)
	b.wide_store_limbs(entry, quotient_out, signed_quotient_low, signed_quotient_high)
	b.wide_store_limbs(entry, remainder_out, signed_remainder_low, signed_remainder_high)
	b.block_instr0(.ret, entry, b.void_type)
}

// generate_wide_to_f64_body converts a 128-bit value to the nearest f64, rounding
// the whole magnitude once, as the C backend does. A magnitude wider than 64 bits
// keeps its top 64 significant bits and folds every dropped bit into the lowest
// kept one. That bit sits below the f64 rounding position, so the u64 conversion
// that follows rounds exactly as a direct 128-bit conversion would; the scale by
// a power of two after it is exact.
fn (mut b Builder) generate_wide_to_f64_body(func_id int) {
	u := b.u64_type
	f := b.f64_type
	entry := b.m.add_block(func_id, 'wide_to_f64_entry')
	low := b.func_add_argument(func_id, u, 'low')
	high := b.func_add_argument(func_id, u, 'high')
	signed := b.func_add_argument(func_id, u, 'signed')
	zero := b.wide_u64('0')
	one := b.wide_u64('1')
	top := b.wide_u64('63')
	// Every bit is set only for a negative value of a signed type.
	sign := b.wide_op(entry, .and_, b.wide_op(entry, .ashr, high, top), b.wide_op(entry, .sub,
		zero, signed))
	magnitude_low, magnitude_high := b.wide_negate_if(entry, low, high, sign)
	result_slot := b.wide_block_slot(entry, f)
	narrow := b.m.add_block(func_id, 'wide_to_f64_narrow')
	wide := b.m.add_block(func_id, 'wide_to_f64_wide')
	finish := b.m.add_block(func_id, 'wide_to_f64_finish')
	b.block_instr3(.br, entry, b.void_type, b.wide_cond(entry, .eq, magnitude_high, zero),
		ValueID(narrow), ValueID(wide))
	b.block_instr2(.store, narrow, b.void_type, b.block_instr1(.uitofp, narrow, f, magnitude_low),
		result_slot)
	b.block_instr1(.jmp, narrow, b.void_type, ValueID(finish))

	leading := b.wide_clz64(wide, magnitude_high)
	// The low limb moves right by 64 - leading, written as two shifts so that
	// leading == 0 stays in range.
	carried := b.wide_op(wide, .lshr, b.wide_op(wide, .lshr, magnitude_low, one), b.wide_op(wide,
		.sub, top, leading))
	kept := b.wide_op(wide, .or_, b.wide_op(wide, .shl, magnitude_high, leading), carried)
	dropped := b.wide_op(wide, .shl, magnitude_low, leading)
	sticky := b.wide_flag(wide, b.wide_cond(wide, .ne, dropped, zero))
	rounded := b.block_instr1(.uitofp, wide, f, b.wide_op(wide, .or_, kept, sticky))
	// 2^(64 - leading), from a power of two that converts exactly.
	half_scale := b.block_instr1(.uitofp, wide, f, b.wide_op(wide, .shl, one, b.wide_op(wide,
		.sub, top, leading)))
	scale := b.block_instr2(.fmul, wide, f, half_scale, b.m.get_or_add_const(f, '2.0'))
	b.block_instr2(.store, wide, b.void_type, b.block_instr2(.fmul, wide, f, rounded, scale),
		result_slot)
	b.block_instr1(.jmp, wide, b.void_type, ValueID(finish))

	value := b.block_instr1(.load, finish, f, result_slot)
	negate := b.m.add_block(func_id, 'wide_to_f64_negate')
	positive := b.m.add_block(func_id, 'wide_to_f64_positive')
	b.block_instr3(.br, finish, b.void_type, b.wide_cond(finish, .ne, sign, zero), ValueID(negate),
		ValueID(positive))
	negative_zero := b.m.get_or_add_const(f, '-0.0')
	b.block_instr1(.ret, negate, b.void_type, b.block_instr2(.fsub, negate, f, negative_zero,
		value))
	b.block_instr1(.ret, positive, b.void_type, value)
}

// generate_wide_from_f64_body truncates an f64 toward zero. A value below 1 in
// magnitude, NaN, and a negative value converted to u128 give 0, which is what
// the C backend's portable helper answers; the high limb is the value divided by
// 2^64, and the rest below it converts exactly into the low limb.
fn (mut b Builder) generate_wide_from_f64_body(func_id int) {
	u := b.u64_type
	f := b.f64_type
	entry := b.m.add_block(func_id, 'wide_from_f64_entry')
	value := b.func_add_argument(func_id, f, 'value')
	signed := b.func_add_argument(func_id, u, 'signed')
	out := b.func_add_argument(func_id, b.m.type_store.get_ptr(b.wide_integer_type()), 'out')
	zero := b.wide_u64('0')
	magnitude_slot := b.wide_block_slot(entry, f)
	sign_slot := b.wide_block_slot(entry, u)
	negative := b.m.add_block(func_id, 'wide_from_f64_negative')
	negative_signed := b.m.add_block(func_id, 'wide_from_f64_negative_signed')
	positive := b.m.add_block(func_id, 'wide_from_f64_positive')
	convert := b.m.add_block(func_id, 'wide_from_f64_convert')
	compute := b.m.add_block(func_id, 'wide_from_f64_compute')
	empty := b.m.add_block(func_id, 'wide_from_f64_zero')
	b.block_instr3(.br, entry, b.void_type, b.block_instr2(.lt, entry, b.i1_type, value,
		b.m.get_or_add_const(f, '0.0')), ValueID(negative), ValueID(positive))
	b.block_instr3(.br, negative, b.void_type, b.wide_cond(negative, .ne, signed, zero),
		ValueID(negative_signed), ValueID(empty))
	b.block_instr2(.store, negative_signed, b.void_type, b.block_instr2(.fsub, negative_signed,
		f, b.m.get_or_add_const(f, '-0.0'), value), magnitude_slot)
	b.block_instr2(.store, negative_signed, b.void_type, b.wide_u64('18446744073709551615'),
		sign_slot)
	b.block_instr1(.jmp, negative_signed, b.void_type, ValueID(convert))
	b.block_instr2(.store, positive, b.void_type, value, magnitude_slot)
	b.block_instr2(.store, positive, b.void_type, zero, sign_slot)
	b.block_instr1(.jmp, positive, b.void_type, ValueID(convert))

	magnitude := b.block_instr1(.load, convert, f, magnitude_slot)
	sign := b.block_instr1(.load, convert, u, sign_slot)
	b.block_instr3(.br, convert, b.void_type, b.block_instr2(.ge, convert, b.i1_type, magnitude,
		b.m.get_or_add_const(f, '1.0')), ValueID(compute), ValueID(empty))
	limb_weight := b.m.get_or_add_const(f, '18446744073709551616.0')
	high := b.block_instr1(.fptoui, compute, u, b.block_instr2(.fdiv, compute, f, magnitude,
		limb_weight))
	high_value := b.block_instr2(.fmul, compute, f, b.block_instr1(.uitofp, compute, f, high),
		limb_weight)
	low := b.block_instr1(.fptoui, compute, u, b.block_instr2(.fsub, compute, f, magnitude,
		high_value))
	signed_low, signed_high := b.wide_negate_if(compute, low, high, sign)
	b.wide_store_limbs(compute, out, signed_low, signed_high)
	b.block_instr0(.ret, compute, b.void_type)
	b.wide_store_limbs(empty, out, zero, zero)
	b.block_instr0(.ret, empty, b.void_type)
}

fn (mut b Builder) wide_slot() ValueID {
	return b.emit0(.alloca, b.m.type_store.get_ptr(b.wide_integer_type()))
}

// wide_helper_result calls a helper whose last argument is an out pointer and
// returns the wide value it wrote.
fn (mut b Builder) wide_helper_result(name string, args []ValueID) ValueID {
	out := b.wide_slot()
	mut operands := args.clone()
	operands << out
	b.wide_block_call(b.cur_block, name, b.void_type, operands)
	return b.emit1(.load, b.wide_integer_type(), out)
}

// wide_integer_arith applies an arithmetic or bitwise infix operator to two
// wide values. `signed` selects the division family.
fn (mut b Builder) wide_integer_arith(op flat.Op, signed bool, lhs ValueID, rhs ValueID) ValueID {
	lhs_low, lhs_high := b.wide_integer_limbs(b.cur_block, lhs)
	rhs_low, rhs_high := b.wide_integer_limbs(b.cur_block, rhs)
	match op {
		.pipe, .amp, .xor {
			code := match op {
				.pipe { OpCode.or_ }
				.amp { OpCode.and_ }
				else { OpCode.xor }
			}
			low := b.wide_op(b.cur_block, code, lhs_low, rhs_low)
			high := b.wide_op(b.cur_block, code, lhs_high, rhs_high)
			return b.make_wide_integer(b.cur_block, low, high)
		}
		.plus {
			low, high := b.wide_add_limbs(b.cur_block, lhs_low, lhs_high, rhs_low, rhs_high)
			return b.make_wide_integer(b.cur_block, low, high)
		}
		.minus {
			low, high := b.wide_sub_limbs(b.cur_block, lhs_low, lhs_high, rhs_low, rhs_high)
			return b.make_wide_integer(b.cur_block, low, high)
		}
		.mul {
			return b.wide_helper_result(wide_helper_mul, [lhs_low, lhs_high, rhs_low, rhs_high])
		}
		.div, .mod {
			return b.wide_integer_divide(op, signed, lhs_low, lhs_high, rhs_low, rhs_high)
		}
		else {
			panic('ssa: unsupported 128-bit integer operation: ${op}')
		}
	}
}

// wide_integer_divide panics on a zero divisor like the other integer widths,
// then calls the division helper of the operand's signedness.
fn (mut b Builder) wide_integer_divide(op flat.Op, signed bool, n_low ValueID, n_high ValueID, d_low ValueID, d_high ValueID) ValueID {
	divisor_bits := b.emit2(.or_, b.u64_type, d_low, d_high)
	is_zero := b.emit2(.eq, b.i1_type, divisor_bits, b.wide_u64('0'))
	failure := b.m.add_block(b.cur_func, 'wide_divide_by_zero')
	success := b.m.add_block(b.cur_func, 'wide_divide')
	b.emit3(.br, b.void_type, is_zero, ValueID(failure), ValueID(success))
	message := if op == .div { 'division by zero' } else { 'modulo by zero' }
	text := b.m.add_value(.string_literal, b.str_type, message, 0)
	panic_ref := b.m.add_value(.func_ref, b.void_type, 'panic', b.fn_ids['panic'])
	b.block_instr2(.call, failure, b.void_type, panic_ref, text)
	b.block_instr0(.unreachable, failure, b.void_type)
	b.cur_block = success
	quotient := b.wide_slot()
	remainder := b.wide_slot()
	name := if signed { wide_helper_sdivmod } else { wide_helper_udivmod }
	b.wide_block_call(b.cur_block, name, b.void_type, [n_low, n_high, d_low, d_high, quotient,
		remainder])
	return b.emit1(.load, b.wide_integer_type(), if op == .div { quotient } else { remainder })
}

// wide_integer_shift shifts a wide value by a count of any integer width. The
// count is read at its own width, so a negative count or a wide count with any
// upper bit set is at least 128 and shifts every bit out, as in the C backend.
fn (mut b Builder) wide_integer_shift(op flat.Op, signed bool, value ValueID, count ValueID) ValueID {
	low, high := b.wide_integer_limbs(b.cur_block, value)
	count_type := b.value_type(count)
	mut amount := count
	if count_type == b.wide_integer_type() {
		count_low, count_high := b.wide_integer_limbs(b.cur_block, count)
		saturate := b.wide_mask(b.cur_block, b.emit2(.ne, b.i1_type, count_high, b.wide_u64('0')))
		amount = b.emit2(.or_, b.u64_type, count_low, saturate)
	} else if b.is_int_type(count_type) {
		amount = b.coerce_numeric_value(count, b.u64_type)
	} else {
		panic('ssa: unsupported 128-bit integer operation: shift by a non-integer count')
	}
	name := match op {
		.left_shift { wide_helper_shl }
		.right_shift {
			if signed { wide_helper_ashr } else { wide_helper_lshr }
		}
		else { wide_helper_lshr }
	}
	return b.wide_helper_result(name, [low, high, amount])
}

fn (mut b Builder) wide_integer_negate(value ValueID) ValueID {
	low, high := b.wide_integer_limbs(b.cur_block, value)
	zero := b.wide_u64('0')
	negated_low, negated_high := b.wide_sub_limbs(b.cur_block, zero, zero, low, high)
	return b.make_wide_integer(b.cur_block, negated_low, negated_high)
}

fn (mut b Builder) wide_integer_complement(value ValueID) ValueID {
	low, high := b.wide_integer_limbs(b.cur_block, value)
	ones := b.wide_u64('18446744073709551615')
	return b.make_wide_integer(b.cur_block, b.wide_op(b.cur_block, .xor, low, ones), b.wide_op(b.cur_block,
		.xor, high, ones))
}

// wide_integer_from_value converts an integer, float or pointer into a wide
// integer. Integers widen by their own signedness; `signed` is the target's and
// only decides how a negative float converts.
fn (mut b Builder) wide_integer_from_value(value ValueID, signed bool) ValueID {
	typ := b.value_type(value)
	if typ == b.wide_integer_type() {
		return value
	}
	if b.is_int_type(typ) {
		return b.wide_integer_from_scalar(value)
	}
	if b.is_float_type(typ) {
		float := if typ == b.f64_type { value } else { b.coerce_numeric_value(value, b.f64_type) }
		return b.wide_helper_result(wide_helper_from_f64, [float, b.wide_signed_flag(signed)])
	}
	if b.is_pointer_type(typ) {
		address := b.emit1(.bitcast, b.u64_type, value)
		return b.make_wide_integer(b.cur_block, address, b.wide_u64('0'))
	}
	panic('ssa: unsupported 128-bit integer conversion from a non-integer value')
}

// wide_integer_to_value converts a wide integer to a narrower integer, a float or
// a pointer. Integers and pointers keep the low bits, as in the C backend.
fn (mut b Builder) wide_integer_to_value(value ValueID, signed bool, target TypeID) ValueID {
	if target == b.wide_integer_type() {
		return value
	}
	low, high := b.wide_integer_limbs(b.cur_block, value)
	if b.is_int_type(target) {
		return b.coerce_numeric_value(low, target)
	}
	if b.is_pointer_type(target) {
		return b.emit1(.bitcast, target, low)
	}
	if b.is_float_type(target) {
		float := b.wide_block_call(b.cur_block, wide_helper_to_f64, b.f64_type, [low, high,
			b.wide_signed_flag(signed)])
		return if target == b.f64_type { float } else { b.coerce_numeric_value(float, target) }
	}
	panic('ssa: unsupported 128-bit integer conversion to a non-integer value')
}

// wide_cast_source builds a cast operand, reading the value behind a mutable
// pointer the way build_cast_expr does.
fn (mut b Builder) wide_cast_source(child_id flat.NodeId) ValueID {
	value := b.build_expr(child_id)
	value_type := b.value_type(value)
	if !b.is_pointer_type(value_type) {
		return value
	}
	checked := b.checked_expr_type_name(child_id)
	if checked == 0 || checked == flat.source_type_unknown {
		return value
	}
	checked_type := b.resolve_type(checked)
	if value_type != checked_type && b.deref_type(value) == checked_type {
		return b.emit1(.load, checked_type, value)
	}
	return value
}

// wide_integer_compare compares two wide values. Equality compares every bit;
// ordering compares the high limbs first, signed or unsigned, then the unsigned
// low limbs on a tie.
fn (mut b Builder) wide_integer_compare(op flat.Op, signed bool, lhs ValueID, rhs ValueID) ValueID {
	lhs_low, lhs_high := b.wide_integer_limbs(b.cur_block, lhs)
	rhs_low, rhs_high := b.wide_integer_limbs(b.cur_block, rhs)
	high_equal := b.block_instr2(.eq, b.cur_block, b.i1_type, lhs_high, rhs_high)
	if op in [.eq, .ne] {
		low_equal := b.block_instr2(.eq, b.cur_block, b.i1_type, lhs_low, rhs_low)
		equal := b.block_instr2(.and_, b.cur_block, b.i1_type, high_equal, low_equal)
		if op == .eq {
			return equal
		}
		return b.block_instr2(.xor, b.cur_block, b.i1_type, equal, b.m.get_or_add_const(b.i1_type,
			'1'))
	}
	less := op in [.lt, .le]
	high_op := if less {
		if signed { OpCode.lt } else { OpCode.ult }
	} else {
		if signed { OpCode.gt } else { OpCode.ugt }
	}
	low_op := match op {
		.lt { OpCode.ult }
		.le { OpCode.ule }
		.gt { OpCode.ugt }
		else { OpCode.uge }
	}
	high_order := b.block_instr2(high_op, b.cur_block, b.i1_type, lhs_high, rhs_high)
	low_order := b.block_instr2(low_op, b.cur_block, b.i1_type, lhs_low, rhs_low)
	tied_order := b.block_instr2(.and_, b.cur_block, b.i1_type, high_equal, low_order)
	return b.block_instr2(.or_, b.cur_block, b.i1_type, high_order, tied_order)
}

// build_wide_compound_assign_value lowers `x op= y` on a wide target. A narrower
// right operand widens by its own signedness; a shift reads it as a count.
fn (mut b Builder) build_wide_compound_assign_value(op flat.Op, current ValueID, rhs ValueID, lhs_type_source_id u32) ValueID {
	if b.value_type(current) != b.wide_integer_type() {
		panic('ssa: unsupported 128-bit integer operation: compound ${op} into a narrower target')
	}
	base := match op {
		.plus_assign { flat.Op.plus }
		.minus_assign { flat.Op.minus }
		.mul_assign { flat.Op.mul }
		.div_assign { flat.Op.div }
		.mod_assign { flat.Op.mod }
		.amp_assign { flat.Op.amp }
		.pipe_assign { flat.Op.pipe }
		.xor_assign { flat.Op.xor }
		.left_shift_assign { flat.Op.left_shift }
		.right_shift_assign { flat.Op.right_shift }
		.right_shift_unsigned_assign { flat.Op.right_shift_unsigned }
		else { panic('ssa: unsupported 128-bit integer operation: compound ${op}') }
	}
	signed := b.wide_source_type_is_signed(lhs_type_source_id)
	if base in wide_shift_ops {
		return b.wide_integer_shift(base, signed, current, rhs)
	}
	rhs_type := b.value_type(rhs)
	if rhs_type != b.wide_integer_type() && !b.is_int_type(rhs_type) {
		panic('ssa: unsupported 128-bit integer operation: compound ${op} with a non-integer value')
	}
	return b.wide_integer_arith(base, signed, current, b.wide_integer_from_value(rhs, signed))
}

// build_wide_postfix lowers `x++` and `x--` on a wide target and returns the old
// value.
fn (mut b Builder) build_wide_postfix(op flat.Op, addr ValueID) ValueID {
	wide_type := b.wide_integer_type()
	current := b.emit1(.load, wide_type, addr)
	one := b.make_wide_integer(b.cur_block, b.wide_u64('1'), b.wide_u64('0'))
	step := if op == .inc { flat.Op.plus } else { flat.Op.minus }
	b.emit2(.store, b.void_type, b.wide_integer_arith(step, false, current, one), addr)
	return current
}

// build_wide_range_for_in lowers `for i in start .. end` when a bound is wide.
// The loop variable is wide, and the bounds compare by the signedness of the
// first wide bound.
fn (mut b Builder) build_wide_range_for_in(node flat.Node, key_id flat.NodeId, start_id flat.NodeId, end_id flat.NodeId, start_val ValueID, end_val ValueID, body_start int, label string) {
	signed := if b.is_wide_integer_expr(start_id) {
		b.is_signed_wide_integer_expr(start_id)
	} else {
		b.is_signed_wide_integer_expr(end_id)
	}
	wide_type := b.wide_integer_type()
	start := b.wide_integer_from_value(start_val, signed)
	end := b.wide_integer_from_value(end_val, signed)
	index_slot := b.wide_slot()
	b.emit2(.store, b.void_type, start, index_slot)
	key_name := b.ident_name(key_id)
	old_key := b.save_var_binding(key_name)
	key_type := if signed { ssa_i128_source_type } else { ssa_u128_source_type }
	key_slot := b.bind_loop_var(key_name, wide_type, key_type)

	cond_block := b.m.add_block(b.cur_func, 'for_in_wide_range_cond')
	body_block := b.m.add_block(b.cur_func, 'for_in_wide_range_body')
	post_block := b.m.add_block(b.cur_func, 'for_in_wide_range_post')
	exit_block := b.m.add_block(b.cur_func, 'for_in_wide_range_exit')
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.cur_block = cond_block
	index := b.emit1(.load, wide_type, index_slot)
	in_range := b.wide_integer_compare(.lt, signed, index, end)
	b.emit3(.br, b.void_type, in_range, ValueID(body_block), ValueID(exit_block))

	b.cur_block = body_block
	if key_slot > 0 {
		b.emit2(.store, b.void_type, index, key_slot)
	}
	b.push_loop_targets(label, node, body_start, exit_block, post_block)
	b.build_for_in_body(node, body_start)
	b.loop_targets.delete_last()
	if !b.current_block_terminated() {
		b.emit1(.jmp, b.void_type, ValueID(post_block))
	}

	b.cur_block = post_block
	current := b.emit1(.load, wide_type, index_slot)
	one := b.make_wide_integer(b.cur_block, b.wide_u64('1'), b.wide_u64('0'))
	b.emit2(.store, b.void_type, b.wide_integer_arith(.plus, signed, current, one), index_slot)
	b.emit1(.jmp, b.void_type, ValueID(cond_block))

	b.restore_var_binding(key_name, old_key)
	b.cur_block = exit_block
}
