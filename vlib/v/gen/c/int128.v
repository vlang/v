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
fn (mut g FlatGen) gen_int128_cast(node flat.Node, target_type types.Type, source_id flat.NodeId) bool {
	target := int128_signedness(target_type)
	source_type := g.usable_expr_type(source_id)
	source := int128_signedness(source_type)
	if target == none && source == none {
		return false
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
	if target_type is types.Primitive {
		if target_type.props.has(.float) {
			g.write(if source? { '__v_i128_to_f64' } else { '__v_u128_to_f64' })
			g.write('(')
			g.gen_expr(source_id)
			g.write(')')
			return true
		}
		if target_type.props.has(.integer) {
			truncate := if source? { '__v_i128_to_i64' } else { '__v_u128_to_u64' }
			g.write(if target_type.size == 64 {
				'${truncate}('
			} else {
				'(int)${truncate}('
			})
			g.gen_expr(source_id)
			g.write(')')
			return true
		}
	}
	return false
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
