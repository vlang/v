module c

import v.flat
import v.types

// Preamble and expression lowering for the `u128` / `i128` primitives.
//
// The C type names `u128` and `i128` come from `prim_c_type`, which already
// falls through by size. What they mean is decided by the helper header
// (`int128_helpers.h`, embedded below): the compiler's own `__int128` where it
// exists, a two-u64 struct everywhere else.
//
// Every 128-bit operation is emitted as a call to a `__v_u128_*` / `__v_i128_*`
// helper, never as a C operator. That keeps one code path for both
// representations, which is why the portable path is exercised by the same
// tests as the native one.

// int128_portable reports whether this build must use the struct
// representation. The header decides for itself from `__SIZEOF_INT128__`, which
// the compilers that have the native type set; `-d v3_no_native_int128` forces
// the fallback so the MSVC code path can be built and run on a host with no
// MSVC.
fn (g &FlatGen) int128_portable() bool {
	return 'v3_no_native_int128' in g.compile_defines
}

// emit_int128_preamble writes the 128-bit typedefs and helper functions into the
// generated C. The text is one file so the same block lands in every build, and
// it is read through a local because `$embed_file` splices the file's contents
// at the point of use, which only a variable can hold intact.
fn (mut g FlatGen) emit_int128_preamble() {
	if g.int128_portable() {
		g.writeln('#define V_INT128_PORTABLE 1')
	}
	helpers := $embed_file('int128_helpers.h').to_string()
	g.writeln(helpers)
	// The decimal helpers come after the block above, because they call it.
	str_helpers := $embed_file('int128_string.h').to_string()
	g.writeln(str_helpers)
}

// int128_decimal_helper returns the C helper that renders a 128-bit value as
// decimal text, or none for every other type. The assert and panic printers use
// it because `long long` cannot carry a 128-bit value on the struct path.
fn int128_decimal_helper(t types.Type) ?string {
	signed := int128_signedness(t) or { return none }
	return if signed { '__v_i128_str' } else { '__v_u128_str' }
}

// int128_signedness returns none unless t is one of the 128-bit integer
// primitives. `true` means signed (`i128`), `false` means `u128`.
fn int128_signedness(t types.Type) ?bool {
	clean := cgen_unalias_type(t)
	if clean is types.Primitive {
		if clean.props.has(.integer) && clean.size == 128 {
			return !clean.props.has(.unsigned)
		}
	}
	return none
}

// int128_helper names a helper in the family that matches the operand's
// signedness, for example `__v_i128_mul`.
fn int128_helper(signed bool, name string) string {
	return if signed { '__v_i128_${name}' } else { '__v_u128_${name}' }
}

// int128_infix_helper maps an infix operator to the helper that implements it.
// Shifts are handled separately, because their right operand is a count rather
// than a value of the same type.
fn int128_infix_helper(op flat.Op, signed bool) ?string {
	return match op {
		.plus { int128_helper(signed, 'add') }
		.minus { int128_helper(signed, 'sub') }
		.mul { int128_helper(signed, 'mul') }
		.div { int128_helper(signed, 'div') }
		.mod { int128_helper(signed, 'rem') }
		.amp { int128_helper(signed, 'and') }
		.pipe { int128_helper(signed, 'or') }
		.xor { int128_helper(signed, 'xor') }
		.eq { int128_helper(signed, 'eq') }
		.ne { int128_helper(signed, 'ne') }
		.lt { int128_helper(signed, 'lt') }
		.gt { int128_helper(signed, 'gt') }
		.le { int128_helper(signed, 'le') }
		.ge { int128_helper(signed, 'ge') }
		else { none }
	}
}

// int128_arith_ops are the infix operators whose operand and result are the same
// 128-bit type.
const int128_arith_ops = [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor]

// int128_comparison_ops are the infix operators with a 128-bit operand and a
// `bool` result.
const int128_comparison_ops = [.eq, .ne, .lt, .gt, .le, .ge]

// int128_shift_ops are the shifts, whose right operand is a count.
const int128_shift_ops = [.left_shift, .right_shift, .right_shift_unsigned]

// gen_int128_infix emits the helper call for an infix node with a 128-bit
// operand. It returns false for everything it does not own, so the caller keeps
// its existing path.
fn (mut g FlatGen) gen_int128_infix(id flat.NodeId, node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type types.Type, rhs_type types.Type) bool {
	op := node.op
	lhs_signed := int128_signedness(lhs_type)
	rhs_signed := int128_signedness(rhs_type)
	if op in int128_shift_ops {
		lhs_kind := lhs_signed or { return false }
		helper := match op {
			.left_shift { int128_helper(lhs_kind, 'shl') }
			.right_shift { int128_helper(lhs_kind, 'shr') }
			// `>>>` is logical whatever the operand is signed as, so it always
			// reads the bit pattern through the unsigned helper.
			else { int128_helper(false, 'shr') }
		}
		g.write('${helper}(')
		g.gen_expr(lhs_id)
		g.write(', ')
		g.gen_int128_shift_count(rhs_id, rhs_type)
		g.write(')')
		return true
	}
	mut signed := false
	if op in int128_comparison_ops {
		// A comparison is bool, so one 128-bit operand is enough; the other side
		// is widened below.
		if lhs_signed == none && rhs_signed == none {
			return false
		}
		signed = lhs_signed or { rhs_signed or { return false } }
	} else {
		// An arithmetic or bitwise expression is 128-bit when its own type is,
		// which is also what decides the helper family. A narrower operand is
		// widened into that family rather than left to C's implicit conversion,
		// because the struct representation has no such conversion.
		signed = int128_signedness(g.usable_expr_type(id)) or { return false }
	}
	helper := int128_infix_helper(op, signed) or { return false }
	if op in [.div, .mod] && g.has_builtins {
		// The other widths panic on a zero divisor, so 128-bit division does too
		// instead of trapping or dividing by zero in the helper.
		c_type := g.value_c_type(g.usable_expr_type(id))
		lhs_tmp := g.tmp_name()
		rhs_tmp := g.tmp_name()
		message := if op == .div { 'division by zero' } else { 'modulo by zero' }
		g.write('({ ${c_type} ${lhs_tmp} = ')
		g.gen_int128_operand(lhs_id, lhs_type, signed)
		g.write('; ${c_type} ${rhs_tmp} = ')
		g.gen_int128_operand(rhs_id, rhs_type, signed)
		g.write('; if (__v_u128_is_zero(${rhs_tmp})) v_panic(_S("${message}")); ')
		g.write('${helper}(${lhs_tmp}, ${rhs_tmp}); })')
		return true
	}
	g.write('${helper}(')
	g.gen_int128_operand(lhs_id, lhs_type, signed)
	g.write(', ')
	g.gen_int128_operand(rhs_id, rhs_type, signed)
	g.write(')')
	return true
}

// gen_int128_operand writes one operand of a 128-bit helper call, widening a
// narrower integer into the helper's type. Widening by explicit i64/u64 casts
// keeps the sign of signed values and the full range of unsigned ones.
fn (mut g FlatGen) gen_int128_operand(id flat.NodeId, typ types.Type, signed bool) {
	if int128_signedness(typ) != none {
		g.gen_expr(id)
		return
	}
	wrap := if signed { '__v_i128_from_i64' } else { '__v_u128_from_u64' }
	cast := if signed { 'i64' } else { 'u64' }
	g.write('${wrap}((${cast})(')
	g.gen_expr(id)
	g.write('))')
}

// gen_int128_shift_count writes a shift count as a u64. The helpers answer 0 for
// counts at or above 128, which is the same rule `gen_guarded_shift` applies at
// the other widths; a negative count becomes a large u64 and lands on that same
// rule.
fn (mut g FlatGen) gen_int128_shift_count(id flat.NodeId, typ types.Type) {
	if kind := int128_signedness(typ) {
		g.write(if kind { '__v_i128_to_u64' } else { '__v_u128_to_u64' })
		g.write('(')
		g.gen_expr(id)
		g.write(')')
		return
	}
	g.write('(u64)(')
	g.gen_expr(id)
	g.write(')')
}

// gen_int128_prefix emits the helper call for unary minus and bitwise not on a
// 128-bit operand.
fn (mut g FlatGen) gen_int128_prefix(node flat.Node, child_id flat.NodeId) bool {
	if node.op !in [.minus, .bit_not] {
		return false
	}
	child_type := g.usable_expr_type(child_id)
	signed := int128_signedness(child_type) or { return false }
	name := match node.op {
		.minus { 'neg' }
		else { 'not' }
	}
	g.write('${int128_helper(signed, name)}(')
	g.gen_expr(child_id)
	g.write(')')
	return true
}

// gen_int128_cast emits a cast where either side is a 128-bit integer. The
// helpers are used on both sides so the struct representation needs no C cast
// that does not exist for it. Anything it does not own returns false and keeps
// the existing cast path.
// A literal that needs more than 64 bits cannot travel to the C compiler as
// digits: C keeps the low 64 bits of an oversized constant and stays quiet about
// it, so `u128(31732946804115296442105984367)` used to become 4047906774079501679
// without a word. Splitting the digits here and emitting the halves avoids the C
// constant altogether, and __v_u128_make exists on both representations, so the
// native and portable paths agree.
//
// A struct rather than two return values, because `.0` and `.1` on a multi-return
// value compile to C that assigns the whole multi_return struct to a u64.
struct Int128LiteralParts {
	high u64
	low  u64
}

// Returns the halves of a literal, or none when it fits in 64 bits and can stay
// on the ordinary path.
fn int128_literal_parts(text string) ?Int128LiteralParts {
	cleaned := text.replace('_', '')
	if cleaned.len == 0 {
		return none
	}
	mut base := u32(10)
	mut digits := cleaned
	if cleaned.len >= 2 && cleaned[0] == `0` {
		prefix := cleaned[1]
		if prefix == `x` || prefix == `X` {
			base = 16
			digits = cleaned[2..]
		} else if prefix == `o` || prefix == `O` {
			base = 8
			digits = cleaned[2..]
		} else if prefix == `b` || prefix == `B` {
			base = 2
			digits = cleaned[2..]
		}
	}
	if digits.len == 0 {
		return none
	}
	// Four 32-bit limbs, least significant first. Multiplying a limb by a base of
	// at most 16 and adding a carry stays inside 64 bits, so this needs no
	// 128-bit arithmetic of its own and keeps bootstrapping simple.
	mut limbs := [u32(0), 0, 0, 0]
	for ch in digits {
		digit := if ch >= `0` && ch <= `9` {
			u64(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			u64(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			u64(ch - `A`) + 10
		} else {
			return none
		}
		if digit >= u64(base) {
			return none
		}
		mut carry := digit
		for i in 0 .. 4 {
			product := u64(limbs[i]) * u64(base) + carry
			limbs[i] = u32(product)
			carry = product >> 32
		}
		if carry != 0 {
			// More than 128 bits: not a value this type can hold at all.
			return none
		}
	}
	high := u64(limbs[2]) | (u64(limbs[3]) << 32)
	low := u64(limbs[0]) | (u64(limbs[1]) << 32)
	if high == 0 {
		return none
	}
	return Int128LiteralParts{
		high: high
		low:  low
	}
}

fn (mut g FlatGen) gen_int128_cast(node flat.Node, target_type types.Type, source_id flat.NodeId) bool {
	target := int128_signedness(target_type)
	source_type := g.usable_expr_type(source_id)
	source := int128_signedness(source_type)
	if target == none && source == none {
		return false
	}
	if target != none && source_id >= 0 && int(source_id) < g.a.nodes.len {
		// The digits of a wide literal are lowered here, so the C compiler never
		// sees an oversized constant and cannot quietly keep the low 64 bits.
		literal := g.a.nodes[int(source_id)]
		if literal.kind == .int_literal {
			if parts := int128_literal_parts(literal.value) {
				g.write('__v_u128_make(${parts.high}ULL, ${parts.low}ULL)')
				return true
			}
		}
		if literal.kind == .prefix && literal.op == .minus && literal.children_count > 0 {
			magnitude := g.a.child_node(literal, 0)
			if magnitude.kind == .int_literal {
				if parts := int128_literal_parts(magnitude.value) {
					g.write('__v_i128_neg(__v_u128_make(${parts.high}ULL, ${parts.low}ULL))')
					return true
				}
			}
		}
	}
	if target != none {
		to_signed := target?
		// Every helper call wraps the source in a C cast, so two parens close it
		// (the cast and the call) while a plain reinterpretation closes one.
		mut prefix := ''
		mut open_count := 1
		if source != none {
			if to_signed && !source? {
				prefix = '__v_i128_from_u128('
			} else if !to_signed && source? {
				prefix = '__v_u128_from_i128('
			} else {
				prefix = '('
			}
		} else if source_type is types.Primitive && source_type.props.has(.float) {
			// V's f32/f64 lower to the C `float`/`double` spellings, so the cast
			// has to use the same name cgen would emit.
			float_ct := g.value_c_type(source_type)
			prefix = if to_signed {
				'__v_i128_from_f64((${float_ct})('
			} else {
				'__v_u128_from_f64((${float_ct})('
			}
			open_count = 2
		} else if g.int128_source_is_plain_literal(source_id) {
			// A literal carries no sign of its own, so it widens as unsigned. Using
			// the signed helper sign-extended it, which turned `u128(0x8000000000000000)`
			// into 2^128 - 2^63 instead of 2^63. A minus prefix keeps the signed
			// path, because `u128(-1)` is meant to wrap to the maximum.
			name := if to_signed { '__v_i128_from_u64' } else { '__v_u128_from_u64' }
			prefix = '${name}((u64)('
			open_count = 2
		} else if source_signedness_known(source_type) {
			from_signed := int128_source_is_signed(source_type)
			name := if to_signed {
				if from_signed { '__v_i128_from_i64' } else { '__v_i128_from_u64' }
			} else {
				if from_signed { '__v_u128_from_i64' } else { '__v_u128_from_u64' }
			}
			cast := if from_signed { 'i64' } else { 'u64' }
			prefix = '${name}((${cast})('
			open_count = 2
		} else {
			// Not an integer or a float source: leave it to the existing path.
			return false
		}
		g.write(prefix)
		g.gen_expr(source_id)
		g.write(')'.repeat(open_count))
		return true
	}
	// A 128-bit source narrowing into a narrower primitive or a float.
	if target_type is types.Primitive && target_type.props.has(.float) {
		g.write(if source? { '__v_i128_to_f64' } else { '__v_u128_to_f64' })
		g.write('(')
		g.gen_expr(source_id)
		g.write(')')
		return true
	}
	if narrow := g.int128_narrow_c_type(target_type) {
		truncate := if source? { '__v_i128_to_i64' } else { '__v_u128_to_u64' }
		// The cast has to name the target's own C type. `(int)` kept every value
		// that fits in 32 bits whole, so `u8(u128(300))` was 300 instead of 44.
		g.write('(${narrow})${truncate}(')
		g.gen_expr(source_id)
		g.write(')')
		return true
	}
	return false
}

// int128_narrow_c_type returns the C type a 128-bit value narrows into, or none
// when the target is not a narrower integer. `isize`, `usize`, `rune` and `char`
// are types of their own in V rather than primitives, so they need naming here
// too, or the cast falls through to a C cast the struct representation rejects.
fn (mut g FlatGen) int128_narrow_c_type(t types.Type) ?string {
	clean := cgen_unalias_type(t)
	match clean {
		types.Primitive {
			if clean.props.has(.integer) && clean.size > 0 && clean.size < 128 {
				return g.value_c_type(t)
			}
		}
		types.ISize, types.USize, types.Rune, types.Char {
			return g.value_c_type(t)
		}
		else {}
	}
	return none
}

// int128_assign_base_op maps a compound assignment operator to the infix
// operator it applies.
fn int128_assign_base_op(op flat.Op) ?flat.Op {
	return match op {
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
		else { none }
	}
}

// gen_int128_compound_assign rewrites `x += y` on a 128-bit value into an
// assignment from the helper call, because the C struct representation has no
// compound operators. The lvalue is evaluated once by taking its address, the
// way the shift compound path already does it for the other widths.
fn (mut g FlatGen) gen_int128_compound_assign(op flat.Op, lhs_id flat.NodeId, rhs_id flat.NodeId, lhs_type types.Type, rhs_type types.Type) bool {
	signed := int128_signedness(lhs_type) or { return false }
	base_op := int128_assign_base_op(op) or { return false }
	helper := if base_op in int128_shift_ops {
		match base_op {
			.left_shift { int128_helper(signed, 'shl') }
			.right_shift { int128_helper(signed, 'shr') }
			else { int128_helper(false, 'shr') }
		}
	} else {
		int128_infix_helper(base_op, signed) or { return false }
	}
	lhs := g.a.nodes[int(lhs_id)]
	mut lhs_text := ''
	if lhs.kind == .ident {
		lhs_text = g.expr_to_string(lhs_id)
		if g.assign_lhs_needs_deref(lhs_id, lhs_type, rhs_type, op) {
			lhs_text = '*${lhs_text}'
		}
	} else {
		addr_tmp := g.tmp_name()
		lhs_ct := g.value_c_type(lhs_type)
		g.write('{ ${lhs_ct}* ${addr_tmp} = &(')
		g.gen_expr(lhs_id)
		g.write('); ')
		lhs_text = '*${addr_tmp}'
	}
	if base_op in [.div, .mod] && g.has_builtins {
		// `/= 0` and `%= 0` have to panic like every other width. The divisor goes
		// through a temporary because a divisor expression may have side effects and
		// must run once, and it is checked before the helper sees it.
		message := if base_op == .div { 'division by zero' } else { 'modulo by zero' }
		divisor := g.tmp_name()
		value_ct := g.value_c_type(lhs_type)
		g.write('{ ${value_ct} ${divisor} = ')
		g.gen_int128_operand(rhs_id, rhs_type, signed)
		g.write('; if (__v_u128_is_zero(${divisor})) v_panic(_S("${message}")); ${lhs_text} = ${helper}(${lhs_text}, ${divisor}); ')
		if lhs.kind == .ident {
			g.write('} ')
		} else {
			g.write('} } ')
		}
		return true
	}
	g.write('${lhs_text} = ${helper}(${lhs_text}, ')
	if base_op in int128_shift_ops {
		g.gen_int128_shift_count(rhs_id, rhs_type)
	} else {
		g.gen_int128_operand(rhs_id, rhs_type, signed)
	}
	if lhs.kind == .ident {
		g.write('); ')
	} else {
		g.write('); } ')
	}
	return true
}

// gen_int128_inc_dec lowers `x++` and `x--` on a 128-bit value. C has no operator
// for the portable struct representation, and a plain `++` only compiles where the
// compiler has __int128.
fn (mut g FlatGen) gen_int128_inc_dec(op flat.Op, target_id flat.NodeId, typ types.Type) bool {
	signed := int128_signedness(typ) or { return false }
	name := if op == .inc { 'add' } else { 'sub' }
	helper := int128_helper(signed, name)
	one := if signed { '__v_i128_from_i64((i64)(1))' } else { '__v_u128_from_u64((u64)(1))' }
	target := g.a.nodes[int(target_id)]
	if target.kind == .ident && !g.current_param_is_mut(target.value) {
		// A plain assignment keeps this usable in the post slot of a C for loop,
		// where a block is not allowed. Reading an ident twice costs nothing.
		g.gen_expr(target_id)
		g.write(' = ${helper}(')
		g.gen_expr(target_id)
		g.write(', ${one})')
		return true
	}
	if target.kind == .ident && g.current_param_is_mut(target.value) {
		g.write('(*')
		if g.current_param_is_mut_pointer(target.value) {
			g.gen_mut_pointer_slot_expr(target_id)
		} else {
			g.gen_expr(target_id)
		}
		g.write(') = ${helper}((*')
		if g.current_param_is_mut_pointer(target.value) {
			g.gen_mut_pointer_slot_expr(target_id)
		} else {
			g.gen_expr(target_id)
		}
		g.write('), ${one})')
		return true
	}
	// The address of the target is taken once, so a target that costs something to
	// evaluate (an index, say) is evaluated a single time.
	g.write('{ ${g.value_c_type(typ)}* _p = &(')
	g.gen_expr(target_id)
	g.write('); *_p = ${helper}(*_p, ${one}); }')
	return true
}

// int128_source_is_plain_literal reports whether a cast source is an integer
// literal with no minus sign written in front of it.
fn (g &FlatGen) int128_source_is_plain_literal(id flat.NodeId) bool {
	if id < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	return node.kind == .int_literal && !node.value.starts_with('-')
}

// source_signedness_known reports whether the cast source is an integer whose
// signedness can be read, which is what decides between the from_i64 and
// from_u64 widening helpers.
fn source_signedness_known(t types.Type) bool {
	clean := cgen_unalias_type(t)
	if clean is types.Primitive {
		return clean.props.has(.integer)
	}
	return clean is types.ISize || clean is types.USize || clean is types.Rune || clean is types.Char
}

// int128_source_is_signed reports whether an integer source casts with a sign
// extension. `isize` is signed, `usize` and the unsigned primitives are not.
fn int128_source_is_signed(t types.Type) bool {
	clean := cgen_unalias_type(t)
	if clean is types.Primitive {
		return !clean.props.has(.unsigned)
	}
	if clean is types.ISize || clean is types.Rune {
		return true
	}
	return false
}
