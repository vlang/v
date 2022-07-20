// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.ast

// TODO: promote(), check_types(), symmetric_check() and check() overlap - should be rearranged
pub fn (mut c Checker) check_types(got ast.Type, expected ast.Type) bool {
	if got == expected {
		return true
	}
	got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()
	if c.pref.translated {
		if expected.is_int() && got.is_int() {
			return true
		}
		if expected == ast.byteptr_type {
			return true
		}
		if expected == ast.voidptr_type {
			return true
		}
		if (expected == ast.bool_type && (got.is_any_kind_of_pointer() || got.is_int()))
			|| ((expected.is_any_kind_of_pointer() || expected.is_int()) && got == ast.bool_type) {
			return true
		}

		if expected.is_any_kind_of_pointer() { //&& !got.is_any_kind_of_pointer() {
			// Allow `int` as `&i8` etc in C code.
			deref := expected.deref()
			// deref := expected.set_nr_muls(0)
			got_sym := c.table.sym(got)
			if deref.is_number() && (got_sym.is_number() || got_sym.kind == .enum_) {
				return true
			}
		}

		// allow rune -> any int and vice versa
		if (expected == ast.rune_type && got.is_int())
			|| (got == ast.rune_type && expected.is_int()) {
			return true
		}
		got_sym := c.table.sym(got)
		expected_sym := c.table.sym(expected)

		// Allow `[N]anyptr` as `[N]anyptr`
		if got_sym.kind == .array && expected_sym.kind == .array {
			if (got_sym.info as ast.Array).elem_type.is_any_kind_of_pointer()
				&& (expected_sym.info as ast.Array).elem_type.is_any_kind_of_pointer() {
				return true
			}
		} else if got_sym.kind == .array_fixed && expected_sym.kind == .array_fixed {
			if (got_sym.info as ast.ArrayFixed).elem_type.is_any_kind_of_pointer()
				&& (expected_sym.info as ast.ArrayFixed).elem_type.is_any_kind_of_pointer() {
				return true
			}
			if c.check_types((got_sym.info as ast.ArrayFixed).elem_type, (expected_sym.info as ast.ArrayFixed).elem_type) {
				return true
			}
		}

		if got_sym.kind == .enum_ {
			// Allow ints as enums
			if expected_sym.is_number() {
				return true
			}
		} else if got_sym.kind == .array_fixed {
			// Allow fixed arrays as `&i8` etc
			if expected_sym.is_number() || expected.is_any_kind_of_pointer() {
				return true
			}
		} else if expected_sym.kind == .array_fixed {
			if got_sym.is_number() && got.is_any_kind_of_pointer() {
				return true
			} else if got_sym.kind == .array {
				info := expected_sym.info as ast.ArrayFixed
				info2 := got_sym.info as ast.Array
				if c.check_types(info.elem_type, info2.elem_type) {
					return true
				}
			}
		} else if got_sym.kind == .array {
			if expected_sym.is_number() || expected.is_any_kind_of_pointer() {
				return true
			}
		} else if expected_sym.kind == .array {
			if got_sym.is_number() && got.is_any_kind_of_pointer() {
				return true
			}
		}
		if expected_sym.kind == .enum_ && got_sym.is_number() {
			// Allow enums as numbers
			return true
		}
		if got_is_ptr && exp_is_ptr {
			// deref_sym := c.table.sym(expected.deref()) // set_nr_muls(0))
			if expected_sym.is_number() && got_sym.is_number() {
				// Allow `&&u8` used as `&&int` etc
				return true
			}
		}
	}
	if got_is_ptr && exp_is_ptr {
		if got.nr_muls() != expected.nr_muls() {
			return false
		}
	}
	exp_idx := expected.idx()
	got_idx := got.idx()
	if exp_idx == got_idx {
		return true
	}
	if exp_idx == ast.voidptr_type_idx || exp_idx == ast.byteptr_type_idx
		|| (expected.is_ptr() && expected.deref().idx() == ast.u8_type_idx) {
		if got.is_ptr() || got.is_pointer() {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if expected.is_real_pointer() {
		if got == ast.int_literal_type {
			return true
		}
	}
	if got_idx == ast.voidptr_type_idx || got_idx == ast.byteptr_type_idx
		|| (got_idx == ast.u8_type_idx && got.is_ptr()) {
		if expected.is_ptr() || expected.is_pointer() {
			return true
		}
	}
	if expected == ast.charptr_type && got == ast.char_type.ref() {
		return true
	}
	if expected.has_flag(.optional) || expected.has_flag(.result) {
		sym := c.table.sym(got)
		if ((sym.idx == ast.error_type_idx || got in [ast.none_type, ast.error_type])
			&& expected.has_flag(.optional))
			|| ((sym.idx == ast.error_type_idx || got == ast.error_type)
			&& expected.has_flag(.result)) {
			// IErorr
			return true
		} else if !c.check_basic(got, expected.clear_flag(.optional).clear_flag(.result)) {
			return false
		}
	}
	if !c.check_basic(got, expected) { // TODO: this should go away...
		return false
	}
	if got.is_number() && expected.is_number() {
		if got == ast.rune_type && expected == ast.u8_type {
			return true
		} else if expected == ast.rune_type && got == ast.u8_type {
			return true
		}
		if c.promote_num(expected, got) != expected {
			// println('could not promote ${c.table.sym(got).name} to ${c.table.sym(expected).name}')
			return false
		}
	}
	if expected.has_flag(.generic) {
		return false
	}
	return true
}

pub fn (mut c Checker) check_expected_call_arg(got ast.Type, expected_ ast.Type, language ast.Language, arg ast.CallArg) ? {
	if got == 0 {
		return error('unexpected 0 type')
	}
	mut expected := expected_
	// variadic
	if expected.has_flag(.variadic) {
		exp_type_sym := c.table.sym(expected_)
		exp_info := exp_type_sym.info as ast.Array
		expected = exp_info.elem_type
	}
	if language == .c {
		// allow number types to be used interchangeably
		if got.is_number() && expected.is_number() {
			return
		}
		// allow bool & int to be used interchangeably for C functions
		if (got.idx() == ast.bool_type_idx
			&& expected.idx() in [ast.int_type_idx, ast.int_literal_type_idx])
			|| (expected.idx() == ast.bool_type_idx
			&& got.idx() in [ast.int_type_idx, ast.int_literal_type_idx]) {
			return
		}
		exp_sym := c.table.sym(expected)
		// unknown C types are set to int, allow int to be used for types like `&C.FILE`
		// eg. `C.fflush(C.stderr)` - error: cannot use `int` as `&C.FILE` in argument 1 to `C.fflush`
		if expected.is_ptr() && exp_sym.language == .c && exp_sym.kind in [.placeholder, .struct_]
			&& got == ast.int_type_idx {
			return
		}
	}
	idx_got := got.idx()
	idx_expected := expected.idx()
	if idx_got in [ast.byteptr_type_idx, ast.charptr_type_idx]
		|| idx_expected in [ast.byteptr_type_idx, ast.charptr_type_idx] {
		igot := int(got)
		iexpected := int(expected)
		// TODO: remove; transitional compatibility for byteptr === &byte
		if (igot == ast.byteptr_type_idx && iexpected == 65545)
			|| (iexpected == ast.byteptr_type_idx && igot == 65545) {
			return
		}
		// TODO: remove; transitional compatibility for charptr === &char
		if (igot == ast.charptr_type_idx && iexpected == 65551)
			|| (iexpected == ast.charptr_type_idx && igot == 65551) {
			return
		}
		muls_got := got.nr_muls()
		muls_expected := expected.nr_muls()
		if idx_got == ast.byteptr_type_idx && idx_expected == ast.u8_type_idx
			&& muls_got + 1 == muls_expected {
			return
		}
		if idx_expected == ast.byteptr_type_idx && idx_got == ast.u8_type_idx
			&& muls_expected + 1 == muls_got {
			return
		}
		if idx_got == ast.charptr_type_idx && idx_expected == ast.char_type_idx
			&& muls_got + 1 == muls_expected {
			return
		}
		if idx_expected == ast.charptr_type_idx && idx_got == ast.char_type_idx
			&& muls_expected + 1 == muls_got {
			return
		}
	}

	if c.check_types(got, expected) {
		if language != .v || expected.is_ptr() == got.is_ptr() || arg.is_mut
			|| arg.expr.is_auto_deref_var() || got.has_flag(.shared_f)
			|| c.table.sym(expected_).kind !in [.array, .map] {
			return
		}
	} else {
		got_typ_sym := c.table.sym(got)
		expected_typ_sym := c.table.sym(expected_)

		// Check on Generics types, there are some case where we have the following case
		// `&Type<int> == &Type<>`. This is a common case we are implementing a function
		// with generic parameters like `compare(bst Bst<T> node) {}`
		if got_typ_sym.symbol_name_except_generic() == expected_typ_sym.symbol_name_except_generic() {
			// Check if we are making a comparison between two different types of
			// the same type like `Type<int> and &Type<>`
			if (got.is_ptr() != expected.is_ptr()) || !c.check_same_module(got, expected) {
				got_typ_str, expected_typ_str := c.get_string_names_of(got, expected)
				return error('cannot use `$got_typ_str` as `$expected_typ_str`')
			}
			return
		}
		if got == ast.void_type {
			return error('`$arg.expr` (no value) used as value')
		}
		got_typ_str, expected_typ_str := c.get_string_names_of(got, expected)
		return error('cannot use `$got_typ_str` as `$expected_typ_str`')
	}

	if got != ast.void_type {
		got_typ_str, expected_typ_str := c.get_string_names_of(got, expected)
		return error('cannot use `$got_typ_str` as `$expected_typ_str`')
	}
}

fn (c Checker) get_string_names_of(got ast.Type, expected ast.Type) (string, string) {
	got_typ_str := c.table.type_to_str(got.clear_flag(.variadic))
	expected_typ_str := c.table.type_to_str(expected.clear_flag(.variadic))
	return got_typ_str, expected_typ_str
}

// helper method to check if the type is of the same module.
// FIXME(vincenzopalazzo) This is a work around to the issue
// explained in the https://github.com/vlang/v/pull/13718#issuecomment-1074517800
fn (c Checker) check_same_module(got ast.Type, expected ast.Type) bool {
	clean_got_typ := c.table.clean_generics_type_str(got.clear_flag(.variadic)).all_before('<')
	clean_expected_typ := c.table.clean_generics_type_str(expected.clear_flag(.variadic)).all_before('<')
	if clean_got_typ == clean_expected_typ {
		return true
		// The following if confition should catch the bugs descripted in the issue
	} else if clean_expected_typ.all_after('.') == clean_got_typ.all_after('.') {
		return true
	}
	return false
}

pub fn (mut c Checker) check_basic(got ast.Type, expected ast.Type) bool {
	unalias_got, unalias_expected := c.table.unalias_num_type(got), c.table.unalias_num_type(expected)
	if unalias_got.idx() == unalias_expected.idx() {
		// this is returning true even if one type is a ptr
		// and the other is not, is this correct behaviour?
		return true
	}
	if (unalias_expected.is_pointer() || unalias_expected.is_number())
		&& (unalias_got.is_pointer() || unalias_got.is_number()) {
		return true
	}
	// allow pointers to be initialized with 0. TODO: use none instead
	if expected.is_ptr() && unalias_got == ast.int_literal_type {
		return true
	}
	// TODO: use sym so it can be absorbed into below [.voidptr, .any] logic
	if expected.idx() == ast.array_type_idx || got.idx() == ast.array_type_idx {
		return true
	}
	got_sym, exp_sym := c.table.sym(got), c.table.sym(expected)
	// multi return
	if exp_sym.kind == .multi_return && got_sym.kind == .multi_return {
		exp_types := exp_sym.mr_info().types
		got_types := got_sym.mr_info().types.map(ast.mktyp(it))
		if exp_types.len != got_types.len {
			return false
		}
		for i in 0 .. exp_types.len {
			if !c.check_types(got_types[i], exp_types[i]) {
				return false
			}
		}
		return true
	}
	// array/map as argument
	if got_sym.kind in [.array, .map, .array_fixed] && exp_sym.kind == got_sym.kind {
		if c.table.type_to_str(got) == c.table.type_to_str(expected).trim('&') {
			return true
		}
	}
	if !unalias_got.is_ptr() && got_sym.kind == .array_fixed
		&& (unalias_expected.is_pointer() || unalias_expected.is_ptr()) {
		// fixed array needs to be a struct, not a pointer
		return false
	}
	if exp_sym.kind in [.voidptr, .any] || got_sym.kind in [.voidptr, .any] {
		return true
	}
	// sum type
	if c.table.sumtype_has_variant(expected, ast.mktyp(got), false) {
		return true
	}
	// type alias
	if (got_sym.kind == .alias && got_sym.parent_idx == expected.idx())
		|| (exp_sym.kind == .alias && exp_sym.parent_idx == got.idx()) {
		return true
	}
	// fn type
	if got_sym.kind == .function && exp_sym.kind == .function {
		return c.check_matching_function_symbols(got_sym, exp_sym)
	}
	// allow `return 0` in a function with `?int` return type
	expected_nonflagged := expected.clear_flags()
	if got == ast.int_literal_type && expected_nonflagged.is_int() {
		return true
	}
	// allow `return 0` in a function with `?f32` return type
	if got == ast.float_literal_type && expected_nonflagged.is_float() {
		return true
	}
	return false
}

pub fn (mut c Checker) check_matching_function_symbols(got_type_sym &ast.TypeSymbol, exp_type_sym &ast.TypeSymbol) bool {
	if c.pref.translated {
		// TODO too open
		return true
	}
	got_info := got_type_sym.info as ast.FnType
	exp_info := exp_type_sym.info as ast.FnType
	got_fn := got_info.func
	exp_fn := exp_info.func
	// we are using check() to compare return type & args as they might include
	// functions themselves. TODO: optimize, only use check() when needed
	if got_fn.params.len != exp_fn.params.len {
		return false
	}
	if got_fn.return_type.has_flag(.optional) != exp_fn.return_type.has_flag(.optional) {
		return false
	}
	if !c.check_basic(got_fn.return_type, exp_fn.return_type) {
		return false
	}
	for i, got_arg in got_fn.params {
		exp_arg := exp_fn.params[i]
		exp_arg_is_ptr := exp_arg.typ.is_ptr() || exp_arg.typ.is_pointer()
		got_arg_is_ptr := got_arg.typ.is_ptr() || got_arg.typ.is_pointer()
		if exp_arg_is_ptr != got_arg_is_ptr {
			exp_arg_pointedness := if exp_arg_is_ptr { 'a pointer' } else { 'NOT a pointer' }
			got_arg_pointedness := if got_arg_is_ptr { 'a pointer' } else { 'NOT a pointer' }
			c.add_error_detail('`$exp_fn.name`\'s expected fn argument: `$exp_arg.name` is $exp_arg_pointedness, but the passed fn argument: `$got_arg.name` is $got_arg_pointedness')
			return false
		} else if exp_arg_is_ptr && got_arg_is_ptr {
			continue
		}
		if got_arg.typ != exp_arg.typ {
			return false
		}
	}
	return true
}

fn (mut c Checker) check_shift(mut node ast.InfixExpr, left_type ast.Type, right_type ast.Type) ast.Type {
	if !left_type.is_int() {
		left_sym := c.table.sym(left_type)
		// maybe it's an int alias? TODO move this to is_int()?
		if left_sym.kind == .alias && (left_sym.info as ast.Alias).parent_type.is_int() {
			return left_type
		}
		if c.pref.translated && left_type == ast.bool_type {
			// allow `bool << 2` in translated C code
			return ast.int_type
		}
		c.error('invalid operation: shift on type `$left_sym.name`', node.left.pos())
		return ast.void_type
	}
	if !right_type.is_int() && !c.pref.translated {
		left_sym := c.table.sym(left_type)
		right_sym := c.table.sym(right_type)
		c.error('cannot shift non-integer type `$right_sym.name` into type `$left_sym.name`',
			node.right.pos())
		return ast.void_type
	}
	// At this point, it is guaranteed that we have a `number1 << number2`, or `number1 >> number2`, or `number1 >>> number2`:
	if !node.ct_left_value_evaled {
		if lval := c.eval_comptime_const_expr(node.left, 0) {
			node.ct_left_value_evaled = true
			node.ct_left_value = lval
		}
	}
	if !node.ct_right_value_evaled {
		if rval := c.eval_comptime_const_expr(node.right, 0) {
			node.ct_right_value_evaled = true
			node.ct_right_value = rval
		}
	}
	// if node.ct_left_value_evaled && node.ct_right_value_evaled {
	//	c.note('>>> node.ct_left_value: $node.ct_left_value | node.ct_right_value: $node.ct_right_value', node.pos)
	// }
	match node.op {
		.left_shift, .right_shift, .unsigned_right_shift {
			// The following code tries to disallow C UBs and IDs at the V level.
			// From the C++ standart (see https://pvs-studio.com/en/docs/warnings/v610/):
			// 1. The type of the result is that of the promoted left operand.
			// The behavior is undefined (UB), if the right operand is negative,
			// or greater than or equal to the length in bits of the promoted left operand.
			// 2. The value of E1 << E2 is E1 left-shifted E2 bit positions;
			// vacated bits are zero-filled. If E1 has an unsigned type,
			// the value of the result is E1 * 2^E2, reduced modulo one more
			// than the maximum value representable in the result type.
			// Otherwise, if E1 has a signed type and non-negative value,
			// and E1*2^E2 is representable in the result type, then that is
			// the resulting value; otherwise, the behavior is undefined (UB).
			// 3. The value of E1 >> E2 is E1 right-shifted E2 bit positions.
			// If E1 has an unsigned type, or if E1 has a signed type and a
			// non-negative value, the value of the result is the integral
			// part of the quotient of E1/2^E2. If E1 has a signed type and
			// a negative value, the resulting value is implementation-defined (ID).
			left_sym_final := c.table.final_sym(left_type)
			left_type_final := ast.Type(left_sym_final.idx)
			if node.op == .left_shift && left_type_final.is_signed() && !(c.inside_unsafe
				&& c.is_generated) {
				c.note('shifting a value from a signed type `$left_sym_final.name` can change the sign',
					node.left.pos())
			}
			if node.ct_right_value_evaled {
				if node.ct_right_value !is ast.EmptyExpr {
					ival := node.ct_right_value.i64() or { -999 }
					if ival < 0 {
						c.error('invalid negative shift count', node.right.pos())
						return left_type
					}
					moffset := match left_type_final {
						ast.char_type { 7 }
						ast.i8_type { 7 }
						ast.i16_type { 15 }
						ast.int_type { 31 }
						ast.i64_type { 63 }
						//
						ast.u8_type { 7 }
						// ast.u8_type { 7 }
						ast.u16_type { 15 }
						ast.u32_type { 31 }
						ast.u64_type { 63 }
						else { 64 }
					}
					if ival > moffset && !c.pref.translated && !c.file.is_translated {
						c.error('shift count for type `$left_sym_final.name` too large (maximum: $moffset bits)',
							node.right.pos())
						return left_type
					}
					if node.ct_left_value_evaled {
						if lval := node.ct_left_value.i64() {
							if lval < 0 {
								c.error('invalid bitshift of a negative number', node.left.pos())
								return left_type
							}
						}
					}
				} else {
					// c.note('can not evaluate "$node.right" at comptime, err: $err', node.pos)
					return left_type
				}
			}
		}
		else {
			c.error('unknown shift operator: $node.op', node.pos)
			return left_type
		}
	}
	return left_type
}

pub fn (mut c Checker) promote(left_type ast.Type, right_type ast.Type) ast.Type {
	if left_type.is_any_kind_of_pointer() {
		if right_type.is_int() || c.pref.translated {
			return left_type
		} else {
			return ast.void_type
		}
	} else if right_type.is_any_kind_of_pointer() {
		if left_type.is_int() || c.pref.translated {
			return right_type
		} else {
			return ast.void_type
		}
	}
	if left_type == right_type {
		return left_type // strings, self defined operators
	}
	if right_type.is_number() && left_type.is_number() {
		return c.promote_num(left_type, right_type)
	} else if left_type.has_flag(.optional) != right_type.has_flag(.optional) {
		// incompatible
		return ast.void_type
	} else {
		return left_type // default to left if not automatic promotion possible
	}
}

fn (c &Checker) promote_num(left_type ast.Type, right_type ast.Type) ast.Type {
	// sort the operands to save time
	mut type_hi := left_type
	mut type_lo := right_type
	if type_hi.idx() < type_lo.idx() {
		type_hi, type_lo = type_lo, type_hi
	}
	idx_hi := type_hi.idx()
	idx_lo := type_lo.idx()
	// the following comparisons rely on the order of the indices in table/types.v
	if idx_hi == ast.int_literal_type_idx {
		return type_lo
	} else if idx_hi == ast.float_literal_type_idx {
		if idx_lo in ast.float_type_idxs {
			return type_lo
		} else {
			return ast.void_type
		}
	} else if type_hi.is_float() {
		if idx_hi == ast.f32_type_idx {
			if idx_lo in [ast.i64_type_idx, ast.u64_type_idx] {
				return ast.void_type
			} else {
				return type_hi
			}
		} else { // f64, float_literal
			return type_hi
		}
	} else if idx_lo >= ast.u8_type_idx { // both operands are unsigned
		return type_hi
	} else if idx_lo >= ast.i8_type_idx
		&& (idx_hi <= ast.isize_type_idx || idx_hi == ast.rune_type_idx) { // both signed
		return if idx_lo == ast.i64_type_idx { type_lo } else { type_hi }
	} else if idx_hi - idx_lo < (ast.u8_type_idx - ast.i8_type_idx) {
		return type_lo // conversion unsigned -> signed if signed type is larger
	} else if c.pref.translated {
		return type_hi
	} else {
		return ast.void_type // conversion signed -> unsigned not allowed
	}
}

pub fn (mut c Checker) check_expected(got ast.Type, expected ast.Type) ? {
	if !c.check_types(got, expected) {
		return error(c.expected_msg(got, expected))
	}
}

fn (c &Checker) expected_msg(got ast.Type, expected ast.Type) string {
	exps := c.table.type_to_str(expected)
	gots := c.table.type_to_str(got)
	return 'expected `$exps`, not `$gots`'
}

pub fn (mut c Checker) symmetric_check(left ast.Type, right ast.Type) bool {
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if right.is_ptr() || right.is_pointer() {
		if left == ast.int_literal_type {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	if left.is_ptr() || left.is_pointer() {
		if right == ast.int_literal_type {
			return true
		}
	}
	return c.check_basic(left, right)
}

pub fn (mut c Checker) infer_fn_generic_types(func ast.Fn, mut node ast.CallExpr) {
	mut inferred_types := []ast.Type{}
	for gi, gt_name in func.generic_names {
		// skip known types
		if gi < node.concrete_types.len {
			inferred_types << node.concrete_types[gi]
			continue
		}
		mut typ := ast.void_type
		for i, param in func.params {
			mut to_set := ast.void_type
			// resolve generic struct receiver
			if node.is_method && param.typ.has_flag(.generic) {
				sym := c.table.final_sym(node.receiver_type)
				match sym.info {
					ast.Struct, ast.Interface, ast.SumType {
						if !isnil(c.table.cur_fn) && c.table.cur_fn.generic_names.len > 0 { // in generic fn
							if gt_name in c.table.cur_fn.generic_names
								&& c.table.cur_fn.generic_names.len == c.table.cur_concrete_types.len {
								idx := c.table.cur_fn.generic_names.index(gt_name)
								typ = c.table.cur_concrete_types[idx]
							}
						} else { // in non-generic fn
							receiver_generic_names := sym.info.generic_types.map(c.table.sym(it).name)
							if gt_name in receiver_generic_names
								&& sym.info.generic_types.len == sym.info.concrete_types.len {
								idx := receiver_generic_names.index(gt_name)
								typ = sym.info.concrete_types[idx]
							}
						}
					}
					else {}
				}
			}
			arg_i := if i != 0 && node.is_method { i - 1 } else { i }
			if node.args.len <= arg_i {
				break
			}
			mut arg := node.args[arg_i]
			arg.typ = c.unwrap_generic(arg.typ)
			param_type_sym := c.table.sym(param.typ)

			if param.typ.has_flag(.generic) && param_type_sym.name == gt_name {
				to_set = ast.mktyp(arg.typ)
				sym := c.table.sym(arg.typ)
				if sym.info is ast.FnType {
					mut func_ := sym.info.func
					func_.name = ''
					idx := c.table.find_or_register_fn_type(c.mod, func_, true, false)
					to_set = ast.new_type(idx).derive(arg.typ)
				}
				if arg.expr.is_auto_deref_var() {
					to_set = to_set.deref()
				}
				// resolve &T &&T ...
				if param.typ.nr_muls() > 0 && to_set.nr_muls() > 0 {
					to_set = to_set.set_nr_muls(0)
				}
				// If the parent fn param is a generic too
				if to_set.has_flag(.generic) {
					to_set = c.unwrap_generic(to_set)
				}
			} else if param.typ.has_flag(.generic) {
				arg_sym := c.table.sym(arg.typ)
				if param.typ.has_flag(.variadic) {
					to_set = ast.mktyp(arg.typ)
				} else if arg_sym.kind == .array && param_type_sym.kind == .array {
					mut arg_elem_info := arg_sym.info as ast.Array
					mut param_elem_info := param_type_sym.info as ast.Array
					mut arg_elem_sym := c.table.sym(arg_elem_info.elem_type)
					mut param_elem_sym := c.table.sym(param_elem_info.elem_type)
					for {
						if arg_elem_sym.kind == .array && param_elem_sym.kind == .array
							&& !isnil(c.table.cur_fn)
							&& param_elem_sym.name !in c.table.cur_fn.generic_names {
							arg_elem_info = arg_elem_sym.info as ast.Array
							arg_elem_sym = c.table.sym(arg_elem_info.elem_type)
							param_elem_info = param_elem_sym.info as ast.Array
							param_elem_sym = c.table.sym(param_elem_info.elem_type)
						} else {
							if param_elem_sym.name == gt_name {
								typ = arg_elem_info.elem_type
							}
							break
						}
					}
				} else if arg_sym.kind == .array_fixed && param_type_sym.kind == .array_fixed {
					mut arg_elem_info := arg_sym.info as ast.ArrayFixed
					mut param_elem_info := param_type_sym.info as ast.ArrayFixed
					mut arg_elem_sym := c.table.sym(arg_elem_info.elem_type)
					mut param_elem_sym := c.table.sym(param_elem_info.elem_type)
					for {
						if arg_elem_sym.kind == .array_fixed && param_elem_sym.kind == .array_fixed
							&& !isnil(c.table.cur_fn)
							&& param_elem_sym.name !in c.table.cur_fn.generic_names {
							arg_elem_info = arg_elem_sym.info as ast.ArrayFixed
							arg_elem_sym = c.table.sym(arg_elem_info.elem_type)
							param_elem_info = param_elem_sym.info as ast.ArrayFixed
							param_elem_sym = c.table.sym(param_elem_info.elem_type)
						} else {
							if param_elem_sym.name == gt_name {
								typ = arg_elem_info.elem_type
							}
							break
						}
					}
				} else if arg_sym.kind == .map && param_type_sym.kind == .map {
					arg_map_info := arg_sym.info as ast.Map
					param_map_info := param_type_sym.info as ast.Map
					if param_map_info.key_type.has_flag(.generic)
						&& c.table.sym(param_map_info.key_type).name == gt_name {
						typ = arg_map_info.key_type
					}
					if param_map_info.value_type.has_flag(.generic)
						&& c.table.sym(param_map_info.value_type).name == gt_name {
						typ = arg_map_info.value_type
					}
				} else if arg_sym.kind == .function && param_type_sym.kind == .function {
					arg_type_func := (arg_sym.info as ast.FnType).func
					param_type_func := (param_type_sym.info as ast.FnType).func
					if param_type_func.params.len == arg_type_func.params.len {
						for n, fn_param in param_type_func.params {
							if fn_param.typ.has_flag(.generic)
								&& c.table.sym(fn_param.typ).name == gt_name {
								typ = arg_type_func.params[n].typ
							}
						}
						if param_type_func.return_type.has_flag(.generic)
							&& c.table.sym(param_type_func.return_type).name == gt_name {
							typ = arg_type_func.return_type
						}
					}
				} else if arg_sym.kind in [.struct_, .interface_, .sum_type] {
					mut generic_types := []ast.Type{}
					mut concrete_types := []ast.Type{}
					match arg_sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							generic_types = arg_sym.info.generic_types
							concrete_types = arg_sym.info.concrete_types
						}
						else {}
					}
					generic_names := generic_types.map(c.table.sym(it).name)
					if gt_name in generic_names && generic_types.len == concrete_types.len {
						idx := generic_names.index(gt_name)
						typ = concrete_types[idx]
					}
				}
			}

			if to_set != ast.void_type {
				if typ != ast.void_type {
					// try to promote
					// only numbers so we don't promote pointers
					if typ.is_number() && to_set.is_number() {
						promoted := c.promote_num(typ, to_set)
						if promoted != ast.void_type {
							to_set = promoted
						}
					}
					if !c.check_types(typ, to_set) {
						c.error('inferred generic type `$gt_name` is ambiguous: got `${c.table.sym(to_set).name}`, expected `${c.table.sym(typ).name}`',
							arg.pos)
					}
				}
				typ = to_set
			}
		}
		if typ == ast.void_type {
			c.error('could not infer generic type `$gt_name` in call to `$func.name`',
				node.pos)
			return
		}
		if c.pref.is_verbose {
			s := c.table.type_to_str(typ)
			println('inferred `$func.name<$s>`')
		}
		inferred_types << typ
		node.concrete_types << typ
	}

	if c.table.register_fn_concrete_types(func.fkey(), inferred_types) {
		c.need_recheck_generic_fns = true
	}
}
