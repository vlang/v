// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.ast
import v.token

// TODO: promote(), check_types(), symmetric_check() and check() overlap - should be rearranged
fn (mut c Checker) check_types(got ast.Type, expected ast.Type) bool {
	if got == expected {
		return true
	}
	exp_idx := expected.idx()
	got_idx := got.idx()
	if exp_idx == got_idx {
		return true
	}
	got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()

	// allow int literals where any kind of real integers are expected:
	if (got == ast.int_literal_type && expected.is_pure_int())
		|| (expected == ast.int_literal_type && got.is_pure_int()) {
		return true
	}

	got_is_any_kind_of_pointer := got.is_any_kind_of_pointer()
	exp_is_any_kind_of_pointer := expected.is_any_kind_of_pointer()

	if c.pref.translated {
		got_is_int := got.is_int()
		exp_is_int := expected.is_int()

		if exp_is_int && got_is_int {
			return true
		}
		if expected == ast.byteptr_type {
			return true
		}
		if expected == ast.voidptr_type || expected == ast.nil_type {
			return true
		}
		if (expected == ast.bool_type && (got_is_int || got_is_any_kind_of_pointer))
			|| ((exp_is_int || exp_is_any_kind_of_pointer) && got == ast.bool_type) {
			return true
		}

		if exp_is_any_kind_of_pointer {
			// Allow `int` as `&i8` etc in C code.
			deref := expected.deref()
			// deref := expected.set_nr_muls(0)
			got_sym := c.table.sym(got)
			if deref.is_number() && (got_sym.is_number() || got_sym.kind == .enum) {
				return true
			}
		}

		// allow rune -> any int and vice versa
		if (expected == ast.rune_type && got_is_int) || (got == ast.rune_type && exp_is_int) {
			return true
		}
		got_sym := c.table.sym(got)
		expected_sym := c.table.sym(expected)

		// Allow `[N]anyptr` as `[N]anyptr`
		if got_sym.info is ast.Array && expected_sym.info is ast.Array {
			if got_sym.info.elem_type.is_any_kind_of_pointer()
				&& expected_sym.info.elem_type.is_any_kind_of_pointer() {
				return true
			}
		} else if got_sym.info is ast.ArrayFixed && expected_sym.info is ast.ArrayFixed {
			if got_sym.info.elem_type.is_any_kind_of_pointer()
				&& expected_sym.info.elem_type.is_any_kind_of_pointer() {
				return true
			}
			if c.check_types(got_sym.info.elem_type, expected_sym.info.elem_type) {
				return true
			}
		}

		if got_sym.kind == .enum {
			// Allow ints as enums
			if expected_sym.is_number() {
				return true
			}
		} else if got_sym.kind == .array_fixed {
			// Allow fixed arrays as `&i8` etc
			if expected_sym.is_number() || exp_is_any_kind_of_pointer {
				return true
			}
		} else if expected_sym.kind == .array_fixed {
			if got_sym.is_number() && got_is_any_kind_of_pointer {
				return true
			} else if got_sym.kind == .array {
				info := expected_sym.info as ast.ArrayFixed
				info2 := got_sym.info as ast.Array
				if c.check_types(info.elem_type, info2.elem_type) {
					return true
				}
			}
		} else if got_sym.kind == .array {
			if expected_sym.is_number() || exp_is_any_kind_of_pointer {
				return true
			}
		} else if expected_sym.kind == .array {
			if got_sym.is_number() && got_is_any_kind_of_pointer {
				return true
			}
		}
		if expected_sym.kind == .enum && got_sym.is_number() {
			// Allow enums as numbers
			return true
		}
		if got_is_ptr && exp_is_ptr && expected_sym.is_number() && got_sym.is_number() {
			// Allow `&&u8` used as `&&int` etc
			return true
		}
	}
	if got_is_ptr && exp_is_ptr && got.nr_muls() != expected.nr_muls() {
		return false
	}
	if got_is_any_kind_of_pointer && (exp_idx == ast.voidptr_type_idx
		|| exp_idx == ast.nil_type_idx || exp_idx == ast.byteptr_type_idx
		|| (exp_is_ptr && expected.deref().idx() == ast.u8_type_idx)) {
		return true
	}

	if (exp_idx == ast.nil_type_idx && got_idx == ast.string_type_idx)
		|| (got_idx == ast.nil_type_idx && exp_idx == ast.string_type_idx) {
		if expected.is_ptr() || got.is_ptr() {
			return true
		}
		got_sym := c.table.sym(got)
		exp_sym := c.table.sym(expected)
		if got_sym.language != .c || exp_sym.language != .c {
			return false
		}
	}

	// allow direct int-literal assignment for pointers for now
	// maybe in the future options should be used for that
	if exp_is_any_kind_of_pointer && got == ast.int_literal_type {
		return true
	}
	if exp_is_any_kind_of_pointer && (got_idx == ast.voidptr_type_idx
		|| got_idx == ast.nil_type_idx || got_idx == ast.byteptr_type_idx
		|| (got_idx == ast.u8_type_idx && got_is_ptr)) {
		return true
	}
	if expected == ast.charptr_type && got == ast.char_type.ref() {
		return true
	}
	if expected.has_option_or_result() {
		sym := c.table.sym(got)
		// Allow error() for Option and Result types
		// `none` for Option only
		if ((sym.idx == ast.error_type_idx || got in [ast.none_type, ast.error_type])
			&& expected.has_flag(.option))
			|| ((sym.idx == ast.error_type_idx || got == ast.error_type)
			&& expected.has_flag(.result)) {
			// IError
			return true
		} else if !c.check_basic(got, expected.clear_option_and_result()) {
			return false
		}
	}
	if !c.check_basic(got, expected) { // TODO: this should go away...
		return false
	}
	if got.is_number() && expected.is_number() {
		if (got == ast.rune_type && expected == ast.u8_type)
			|| (expected == ast.rune_type && got == ast.u8_type) {
			return true
		}
		if c.promote_num(expected, got) != expected {
			// println('could not promote ${c.table.sym(got).name} to ${c.table.sym(expected).name}')
			return false
		}
	}
	if expected.has_flag(.generic) && !got.has_flag(.generic) {
		return false
	}
	return true
}

fn (c &Checker) check_multiple_ptr_match(got ast.Type, expected ast.Type, param ast.Param, arg ast.CallArg) bool {
	param_nr_muls := if param.is_mut && !expected.is_ptr() { 1 } else { expected.nr_muls() }
	if got.is_ptr() && got.nr_muls() > 1 && got.nr_muls() != param_nr_muls {
		if arg.expr is ast.PrefixExpr && arg.expr.op == .amp {
			return false
		}
		if arg.expr is ast.UnsafeExpr {
			expr := arg.expr.expr
			if expr is ast.PrefixExpr && expr.op == .amp {
				return false
			}
		}
	}
	return true
}

fn (mut c Checker) check_expected_call_arg(got_ ast.Type, expected_ ast.Type, language ast.Language, arg ast.CallArg) ! {
	if got_ == 0 {
		return error('unexpected 0 type')
	}
	if got_ == expected_ {
		return
	}
	mut expected := c.table.unaliased_type(expected_)
	is_aliased := expected != expected_
	is_exp_sumtype := c.table.type_kind(expected_) == .sum_type
	got := c.table.unaliased_type(got_)
	// variadic
	if expected_.has_flag(.variadic) {
		exp_type_sym := c.table.sym(expected_)
		exp_info := exp_type_sym.info as ast.Array
		expected = exp_info.elem_type
	}
	if expected == got {
		return
	}
	got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()
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
		if exp_is_ptr && exp_sym.language == .c && exp_sym.kind in [.placeholder, .struct]
			&& got == ast.int_type_idx {
			return
		}
	} else {
		// passing &expr where no-pointer is expected
		if expected != ast.voidptr_type && !exp_is_ptr && got_is_ptr && arg.expr.is_reference() {
			got_typ_str, expected_typ_str := c.get_string_names_of(got_, expected_)
			return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
		}
		if expected.has_flag(.option) {
			is_ptr := got_is_ptr
				|| (arg.expr is ast.Ident && (arg.expr as ast.Ident).is_mut())
				|| arg.expr is ast.None
			if (exp_is_ptr && !is_ptr) || (!exp_is_ptr && got_is_ptr) {
				got_typ_str, expected_typ_str := c.get_string_names_of(got_, expected_)
				return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
			}
		}

		// `fn foo(mut p &Expr); mut expr := Expr{}; foo(mut expr)`
		if arg.is_mut && expected.nr_muls() > 1 && got.nr_muls() < expected.nr_muls() {
			got_typ_str, expected_typ_str := c.get_string_names_of(got_, expected_)
			return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
		}

		exp_sym_idx := expected.idx()
		got_sym_idx := got.idx()

		if exp_is_ptr && got_is_ptr && exp_sym_idx != got_sym_idx
			&& exp_sym_idx in [ast.u8_type_idx, ast.byteptr_type_idx]
			&& got_sym_idx !in [ast.u8_type_idx, ast.byteptr_type_idx] {
			got_typ_str, expected_typ_str := c.get_string_names_of(got_, expected_)
			return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
		}

		is_expected_optional := if is_aliased {
			expected_.has_flag(.option)
		} else {
			expected.has_flag(.option)
		}
		if !is_expected_optional && got.has_flag(.option)
			&& (!(arg.expr is ast.Ident || arg.expr is ast.ComptimeSelector)
			|| (arg.expr is ast.Ident && c.comptime.get_ct_type_var(arg.expr) != .field_var)) {
			got_typ_str, expected_typ_str := c.get_string_names_of(got_, expected_)
			return error('cannot use `${got_typ_str}` as `${expected_typ_str}`, it must be unwrapped first')
		}
	}
	// check int signed/unsigned mismatch
	if got == ast.int_literal_type_idx && expected in ast.unsigned_integer_type_idxs
		&& arg.expr is ast.IntegerLiteral && arg.expr.val.i64() < 0 {
		expected_typ_str := c.table.type_to_str(expected.clear_flag(.variadic))
		return error('cannot use literal signed integer as `${expected_typ_str}`')
	}

	idx_got := got.idx()
	idx_expected := expected.idx()
	if idx_got in [ast.byteptr_type_idx, ast.charptr_type_idx]
		|| idx_expected in [ast.byteptr_type_idx, ast.charptr_type_idx] {
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
	exp_type := if !is_aliased || expected_.has_flag(.variadic) {
		expected
	} else {
		expected_
	}
	if c.check_types(if is_exp_sumtype { got_ } else { got }, exp_type) {
		if language == .v && idx_got == ast.voidptr_type_idx {
			if expected.is_int_valptr() || expected.is_int() || exp_is_ptr {
				return
			}
			exp_sym := c.table.final_sym(expected)
			if exp_sym.language == .v
				&& exp_sym.kind !in [.voidptr, .charptr, .byteptr, .function, .placeholder, .array_fixed, .sum_type, .struct] {
				got_typ_str, expected_typ_str := c.get_string_names_of(got_, exp_type)
				return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
			}
		}
		if language != .v || exp_is_ptr == got_is_ptr || arg.is_mut || arg.expr.is_auto_deref_var()
			|| got.has_flag(.shared_f) || c.table.sym(expected_).kind !in [.array, .map] {
			return
		}
	} else {
		got_typ_sym := c.table.sym(c.unwrap_generic(got))
		expected_typ_sym := c.table.sym(c.unwrap_generic(expected))
		if expected_typ_sym.kind == .interface && c.type_implements(got, expected, token.Pos{}) {
			return
		}

		// Check on Generics types, there are some case where we have the following case
		// `&Type[int] == &Type[]`. This is a common case we are implementing a function
		// with generic parameters like `compare(bst Bst[T] node) {}`
		if got_typ_sym.symbol_name_except_generic() == expected_typ_sym.symbol_name_except_generic() {
			// Check if we are making a comparison between two different types of
			// the same type like `Type[int] and &Type[]`
			if got_is_ptr != exp_is_ptr || !c.check_same_module(got, expected)
				|| (!got_is_ptr && !exp_is_ptr && got_typ_sym.name != expected_typ_sym.name) {
				got_typ_str, expected_typ_str := c.get_string_names_of(got_, exp_type)
				return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
			}
			return
		}
		if got == ast.void_type {
			return error('`${arg.expr}` (no value) used as value')
		}
		if expected == ast.voidptr_type && got_typ_sym.kind == .array_fixed {
			return
		}
		got_typ_str, expected_typ_str := c.get_string_names_of(got_, exp_type)
		return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
	}

	if got != ast.void_type {
		got_typ_str, expected_typ_str := c.get_string_names_of(got_, exp_type)
		return error('cannot use `${got_typ_str}` as `${expected_typ_str}`')
	}
}

fn (c &Checker) get_string_names_of(got ast.Type, expected ast.Type) (string, string) {
	got_typ_str := c.table.type_to_str(got.clear_flag(.variadic))
	expected_typ_str := c.table.type_to_str(expected.clear_flag(.variadic))
	return got_typ_str, expected_typ_str
}

// helper method to check if the type is of the same module.
// FIXME(vincenzopalazzo) This is a work around to the issue
// explained in the https://github.com/vlang/v/pull/13718#issuecomment-1074517800
fn (c &Checker) check_same_module(got ast.Type, expected ast.Type) bool {
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

fn (mut c Checker) check_basic(got ast.Type, expected ast.Type) bool {
	if got == expected {
		return true
	}
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
	if (expected.idx() == ast.array_type_idx && c.table.final_sym(got).kind == .array)
		|| (got.idx() == ast.array_type_idx && c.table.final_sym(expected).kind == .array) {
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
		&& unalias_expected.is_any_kind_of_pointer() {
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
	// struct
	if exp_sym.kind == .struct && got_sym.kind == .struct {
		if c.table.type_to_str(expected) == c.table.type_to_str(got) {
			return true
		}
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
	// decode and check array to aliased array type
	if got_sym.kind == .array && exp_sym.info is ast.Array {
		exp_elem_sym := c.table.sym(exp_sym.info.elem_type)
		if exp_elem_sym.info is ast.Alias {
			parent_elem_sym := c.table.sym(exp_elem_sym.info.parent_type)
			if parent_elem_sym.info is ast.Array {
				array_info := parent_elem_sym.array_info()
				elem_type := c.table.find_or_register_array_with_dims(array_info.elem_type,
					array_info.nr_dims + exp_sym.info.nr_dims)
				if c.table.type_to_str(got) == c.table.type_to_str(ast.idx_to_type(elem_type)) {
					return true
				}
			}
		}
	}
	return false
}

fn (mut c Checker) check_matching_function_symbols(got_type_sym &ast.TypeSymbol, exp_type_sym &ast.TypeSymbol) bool {
	if c.pref.translated {
		// TODO: too open
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
	if got_fn.return_type.has_flag(.option) != exp_fn.return_type.has_flag(.option) {
		return false
	}
	if got_fn.return_type.has_flag(.result) != exp_fn.return_type.has_flag(.result) {
		return false
	}
	if !c.check_basic(got_fn.return_type, exp_fn.return_type) {
		return false
	}
	// The check for sumtype in c.check_basic() in the previous step is only for its variant to be subsumed
	// So we need to do a second, more rigorous check of the return value being sumtype.
	if c.table.final_sym(exp_fn.return_type).kind == .sum_type
		&& got_fn.return_type.idx() != exp_fn.return_type.idx() {
		return false
	}
	for i, got_arg in got_fn.params {
		exp_arg := exp_fn.params[i]
		exp_arg_typ := c.unwrap_generic(exp_arg.typ)
		got_arg_typ := c.unwrap_generic(got_arg.typ)
		exp_arg_is_ptr := exp_arg_typ.is_any_kind_of_pointer()
		got_arg_is_ptr := got_arg_typ.is_any_kind_of_pointer()
		if exp_arg.is_mut && !got_arg.is_mut {
			return false
		}
		if exp_arg_is_ptr != got_arg_is_ptr {
			exp_arg_pointedness := if exp_arg_is_ptr { 'a pointer' } else { 'NOT a pointer' }
			got_arg_pointedness := if got_arg_is_ptr { 'a pointer' } else { 'NOT a pointer' }
			if exp_fn.name == '' {
				c.add_error_detail('expected argument ${i + 1} to be ${exp_arg_pointedness}, but the passed argument ${
					i + 1} is ${got_arg_pointedness}')
			} else {
				c.add_error_detail('`${exp_fn.name}`\'s expected argument `${exp_arg.name}` to be ${exp_arg_pointedness}, but the passed argument `${got_arg.name}` is ${got_arg_pointedness}')
			}
			return false
		} else if exp_arg_is_ptr && got_arg_is_ptr {
			if exp_arg_typ.is_pointer() || got_arg_typ.is_pointer() {
				continue
			}
		}
		if c.table.unaliased_type(got_arg_typ).idx() != c.table.unaliased_type(exp_arg_typ).idx() {
			return false
		}
	}
	return true
}

fn (mut c Checker) check_shift(mut node ast.InfixExpr, left_type_ ast.Type, right_type_ ast.Type) ast.Type {
	left_type := c.unwrap_generic(left_type_)
	right_type := c.unwrap_generic(right_type_)
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
		c.error('invalid operation: shift on type `${left_sym.name}`', node.left.pos())
		return ast.void_type
	}
	if !right_type.is_int() && !c.pref.translated {
		left_sym := c.table.sym(left_type)
		right_sym := c.table.sym(right_type)
		c.error('cannot shift non-integer type `${right_sym.name}` into type `${left_sym.name}`',
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
			// From the C++ standard (see https://pvs-studio.com/en/docs/warnings/v610/):
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
			left_type_final := ast.idx_to_type(left_sym_final.idx)
			if !(c.is_generated || c.inside_unsafe || c.file.is_translated || c.pref.translated) {
				if node.op == .left_shift && left_type_final.is_signed() {
					c.note('shifting a value from a signed type `${left_sym_final.name}` can change the sign',
						node.left.pos())
				}
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
						c.error('shift count for type `${left_sym_final.name}` too large (maximum: ${moffset} bits)',
							node.right.pos())
						return left_type
					}
					if !c.inside_unsafe {
						if node.ct_left_value_evaled {
							if lval := node.ct_left_value.i64() {
								if lval < 0 {
									c.error('invalid bitshift of a negative number', node.left.pos())
									return left_type
								}
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
			c.error('unknown shift operator: ${node.op}', node.pos)
			return left_type
		}
	}
	return left_type
}

fn (mut c Checker) promote_keeping_aliases(left_type ast.Type, right_type ast.Type, left_kind ast.Kind,
	right_kind ast.Kind) ast.Type {
	if left_type == right_type && left_kind == .alias && right_kind == .alias {
		return left_type
	}
	return c.promote(left_type, right_type)
}

fn (mut c Checker) promote(left_type ast.Type, right_type ast.Type) ast.Type {
	if left_type == right_type {
		return left_type // strings, self defined operators
	}
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
	if right_type.is_number() && left_type.is_number() {
		return c.promote_num(left_type, right_type)
	} else if left_type.has_flag(.option) != right_type.has_flag(.option) {
		// incompatible
		return ast.void_type
	} else if (left_type == ast.void_type && right_type != ast.void_type)
		|| (right_type == ast.void_type && left_type != ast.void_type) {
		// incompatible as well
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
	} else if idx_hi == ast.u8_type_idx && idx_lo > ast.i8_type_idx {
		return type_lo // conversion unsigned u8 -> signed if signed type is larger
	} else if idx_hi == ast.u16_type_idx && idx_lo > ast.i16_type_idx {
		return type_lo // conversion unsigned u16 -> signed if signed type is larger
	} else if idx_hi == ast.u32_type_idx && idx_lo > ast.int_type_idx {
		return type_lo // conversion unsigned u32 -> signed if signed type is larger
	} else if idx_hi == ast.u64_type_idx && idx_lo >= ast.i64_type_idx {
		return type_lo // conversion unsigned u64 -> signed if signed type is larger
	} else if idx_hi == ast.usize_type_idx && idx_lo >= ast.isize_type_idx {
		return type_lo // conversion unsigned usize -> signed if signed type is larger
	} else if c.pref.translated {
		return type_hi
	} else {
		return ast.void_type // conversion signed -> unsigned not allowed
	}
}

fn (mut c Checker) check_expected(got ast.Type, expected ast.Type) ! {
	if !c.check_types(got, expected) {
		return error(c.expected_msg(got, expected))
	}
}

fn (c &Checker) expected_msg(got ast.Type, expected ast.Type) string {
	exps := c.table.type_to_str(expected)
	gots := c.table.type_to_str(got)
	return 'expected `${exps}`, not `${gots}`'
}

fn (mut c Checker) symmetric_check(left ast.Type, right ast.Type) bool {
	// allow direct int-literal assignment for pointers for now
	// maybe in the future options should be used for that
	if right.is_any_kind_of_pointer() {
		if left == ast.int_literal_type {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	if left.is_any_kind_of_pointer() {
		if right == ast.int_literal_type {
			return true
		}
	}
	return c.check_basic(left, right)
}

fn (mut c Checker) infer_struct_generic_types(typ ast.Type, node ast.StructInit) []ast.Type {
	mut concrete_types := []ast.Type{}
	sym := c.table.sym(typ)
	if sym.info is ast.Struct {
		generic_names := sym.info.generic_types.map(c.table.sym(it).name)
		gname: for gt_name in generic_names {
			for ft in sym.info.fields {
				field_sym := c.table.sym(ft.typ)
				if field_sym.name == gt_name {
					for t in node.init_fields {
						if ft.name == t.name && t.typ != 0 {
							concrete_types << ast.mktyp(t.typ)
							continue gname
						}
					}
				}
				if field_sym.info is ast.Array {
					for t in node.init_fields {
						if ft.name == t.name {
							init_sym := c.table.sym(t.typ)
							if init_sym.info is ast.Array {
								mut init_elem_typ, mut field_elem_typ := init_sym.info.elem_type, field_sym.info.elem_type
								mut init_elem_sym, mut field_elem_sym := c.table.sym(init_elem_typ), c.table.sym(field_elem_typ)
								for {
									if mut init_elem_sym.info is ast.Array
										&& mut field_elem_sym.info is ast.Array {
										init_elem_typ, field_elem_typ = init_elem_sym.info.elem_type, field_elem_sym.info.elem_type
										init_elem_sym, field_elem_sym = c.table.sym(init_elem_typ), c.table.sym(field_elem_typ)
									} else {
										if field_elem_sym.name == gt_name {
											mut elem_typ := init_elem_typ
											if field_elem_typ.nr_muls() > 0
												&& elem_typ.nr_muls() > 0 {
												elem_typ = elem_typ.set_nr_muls(0)
											}
											concrete_types << ast.mktyp(elem_typ)
											continue gname
										}
										break
									}
								}
							}
						}
					}
				} else if field_sym.info is ast.ArrayFixed {
					for t in node.init_fields {
						if ft.name == t.name {
							init_sym := c.table.sym(t.typ)
							if init_sym.info is ast.ArrayFixed {
								mut init_elem_typ, mut field_elem_typ := init_sym.info.elem_type, field_sym.info.elem_type
								mut init_elem_sym, mut field_elem_sym := c.table.sym(init_elem_typ), c.table.sym(field_elem_typ)
								for {
									if mut init_elem_sym.info is ast.ArrayFixed
										&& mut field_elem_sym.info is ast.ArrayFixed {
										init_elem_typ, field_elem_typ = init_elem_sym.info.elem_type, field_elem_sym.info.elem_type
										init_elem_sym, field_elem_sym = c.table.sym(init_elem_typ), c.table.sym(field_elem_typ)
									} else {
										if field_elem_sym.name == gt_name {
											mut elem_typ := init_elem_typ
											if field_elem_typ.nr_muls() > 0
												&& elem_typ.nr_muls() > 0 {
												elem_typ = elem_typ.set_nr_muls(0)
											}
											concrete_types << ast.mktyp(elem_typ)
											continue gname
										}
										break
									}
								}
							}
						}
					}
				} else if field_sym.info is ast.Map {
					for t in node.init_fields {
						if ft.name == t.name {
							init_sym := c.table.sym(t.typ)
							if init_sym.info is ast.Map {
								if field_sym.info.key_type.has_flag(.generic)
									&& c.table.sym(field_sym.info.key_type).name == gt_name {
									mut key_typ := init_sym.info.key_type
									if field_sym.info.key_type.nr_muls() > 0
										&& key_typ.nr_muls() > 0 {
										key_typ = key_typ.set_nr_muls(0)
									}
									concrete_types << ast.mktyp(key_typ)
									continue gname
								}
								if field_sym.info.value_type.has_flag(.generic)
									&& c.table.sym(field_sym.info.value_type).name == gt_name {
									mut val_typ := init_sym.info.value_type
									if field_sym.info.value_type.nr_muls() > 0
										&& val_typ.nr_muls() > 0 {
										val_typ = val_typ.set_nr_muls(0)
									}
									concrete_types << ast.mktyp(val_typ)
									continue gname
								}
							}
						}
					}
				} else if field_sym.info is ast.FnType {
					for t in node.init_fields {
						if ft.name == t.name {
							init_sym := c.table.sym(t.typ)
							if init_sym.info is ast.FnType {
								if field_sym.info.func.params.len == init_sym.info.func.params.len {
									for n, fn_param in field_sym.info.func.params {
										if fn_param.typ.has_flag(.generic)
											&& c.table.sym(fn_param.typ).name == gt_name {
											mut arg_typ := init_sym.info.func.params[n].typ
											if fn_param.typ.nr_muls() > 0 && arg_typ.nr_muls() > 0 {
												arg_typ = arg_typ.set_nr_muls(0)
											}
											concrete_types << ast.mktyp(arg_typ)
											continue gname
										}
									}
									if field_sym.info.func.return_type.has_flag(.generic)
										&& c.table.sym(field_sym.info.func.return_type).name == gt_name {
										mut ret_typ := init_sym.info.func.return_type
										if field_sym.info.func.return_type.nr_muls() > 0
											&& ret_typ.nr_muls() > 0 {
											ret_typ = ret_typ.set_nr_muls(0)
										}
										concrete_types << ast.mktyp(ret_typ)
										continue gname
									}
								}
							}
						}
					}
				}
			}
			c.error('could not infer generic type `${gt_name}` in generic struct `${sym.name}[${generic_names.join(', ')}]`',
				node.pos)
			return concrete_types
		}
	}
	return concrete_types
}

fn (g Checker) get_generic_array_element_type(array ast.Array) ast.Type {
	mut cparam_elem_info := array as ast.Array
	mut cparam_elem_sym := g.table.sym(cparam_elem_info.elem_type)
	mut typ := ast.void_type
	for {
		if cparam_elem_sym.kind == .array {
			cparam_elem_info = cparam_elem_sym.info as ast.Array
			cparam_elem_sym = g.table.sym(cparam_elem_info.elem_type)
		} else {
			return cparam_elem_info.elem_type.set_nr_muls(0)
		}
	}
	return typ
}

fn (mut c Checker) infer_fn_generic_types(func &ast.Fn, mut node ast.CallExpr) {
	mut inferred_types := []ast.Type{}
	mut arg_inferred := []int{}
	for gi, gt_name in func.generic_names {
		// skip known types
		if gi < node.concrete_types.len {
			inferred_types << node.concrete_types[gi]
			continue
		}
		mut typ := ast.void_type
		for i, param in func.params {
			// resolve generic struct receiver
			if node.is_method && i == 0 && param.typ.has_flag(.generic) {
				sym := c.table.final_sym(node.left_type)
				if node.left_type.has_flag(.generic) {
					match sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							receiver_generic_names := sym.info.generic_types.map(c.table.sym(it).name)
							if gt_name in receiver_generic_names {
								idx := receiver_generic_names.index(gt_name)
								typ = sym.info.generic_types[idx]
							}
						}
						else {}
					}
				} else {
					match sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							if c.table.cur_fn != unsafe { nil }
								&& c.table.cur_fn.generic_names.len > 0 { // in generic fn
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
			}
			arg_i := if i != 0 && node.is_method { i - 1 } else { i }
			if node.args.len <= arg_i || typ != ast.void_type {
				break
			}
			arg := node.args[arg_i]
			param_sym := c.table.sym(param.typ)

			if param.typ.has_flag(.generic) && param_sym.name == gt_name {
				typ = ast.mktyp(arg.typ)
				sym := c.table.final_sym(arg.typ)
				if sym.info is ast.FnType {
					mut func_ := sym.info.func
					func_.name = ''
					idx := c.table.find_or_register_fn_type(func_, true, false)
					typ = ast.new_type(idx).derive(arg.typ)
				} else if c.comptime.comptime_for_field_var != '' && sym.kind in [.struct, .any]
					&& arg.expr is ast.ComptimeSelector {
					comptime_typ := c.type_resolver.get_comptime_selector_type(arg.expr,
						ast.void_type)
					if comptime_typ != ast.void_type {
						typ = comptime_typ
						if func.return_type.has_flag(.generic)
							&& gt_name == c.table.type_to_str(func.return_type) {
							node.comptime_ret_val = true
						}
					}
				}
				if arg.expr.is_auto_deref_var() {
					typ = typ.deref()
				}
				// resolve &T &&T ...
				if param.typ.nr_muls() > 0 && typ.nr_muls() > 0 {
					typ = typ.set_nr_muls(0)
				}
			} else if param.typ.has_flag(.generic) {
				arg_typ := if c.table.sym(arg.typ).kind == .any {
					c.unwrap_generic(arg.typ)
				} else {
					arg.typ
				}
				arg_sym := c.table.final_sym(arg_typ)
				if param.typ.has_flag(.variadic) {
					typ = ast.mktyp(arg_typ)
				} else if arg_sym.info is ast.Array && param_sym.info is ast.Array {
					mut arg_elem_typ, mut param_elem_typ := arg_sym.info.elem_type, param_sym.info.elem_type
					mut arg_elem_sym, mut param_elem_sym := c.table.sym(arg_elem_typ), c.table.sym(param_elem_typ)
					for {
						if mut arg_elem_sym.info is ast.Array
							&& mut param_elem_sym.info is ast.Array
							&& c.table.cur_fn != unsafe { nil }
							&& param_elem_sym.name !in c.table.cur_fn.generic_names {
							arg_elem_typ, param_elem_typ = arg_elem_sym.info.elem_type, param_elem_sym.info.elem_type
							arg_elem_sym, param_elem_sym = c.table.sym(arg_elem_typ), c.table.sym(param_elem_typ)
						} else {
							if param_elem_sym.name == gt_name {
								typ = arg_elem_typ
								if param_elem_typ.nr_muls() > 0 && typ.nr_muls() > 0 {
									typ = typ.set_nr_muls(0)
								}
							}
							break
						}
					}
				} else if arg_sym.info is ast.ArrayFixed && param_sym.info is ast.ArrayFixed {
					mut arg_elem_typ, mut param_elem_typ := arg_sym.info.elem_type, param_sym.info.elem_type
					mut arg_elem_sym, mut param_elem_sym := c.table.sym(arg_elem_typ), c.table.sym(param_elem_typ)
					for {
						if mut arg_elem_sym.info is ast.ArrayFixed
							&& mut param_elem_sym.info is ast.ArrayFixed
							&& c.table.cur_fn != unsafe { nil }
							&& param_elem_sym.name !in c.table.cur_fn.generic_names {
							arg_elem_typ, param_elem_typ = arg_elem_sym.info.elem_type, param_elem_sym.info.elem_type
							arg_elem_sym, param_elem_sym = c.table.sym(arg_elem_typ), c.table.sym(param_elem_typ)
						} else {
							if param_elem_sym.name == gt_name {
								typ = arg_elem_typ
								if param_elem_typ.nr_muls() > 0 && typ.nr_muls() > 0 {
									typ = typ.set_nr_muls(0)
								}
							}
							break
						}
					}
				} else if arg_sym.info is ast.Map && param_sym.info is ast.Map {
					if param_sym.info.key_type.has_flag(.generic)
						&& c.table.sym(param_sym.info.key_type).name == gt_name {
						typ = arg_sym.info.key_type
						if param_sym.info.key_type.nr_muls() > 0 && typ.nr_muls() > 0 {
							typ = typ.set_nr_muls(0)
						}
					}
					if param_sym.info.value_type.has_flag(.generic)
						&& c.table.sym(param_sym.info.value_type).name == gt_name {
						typ = arg_sym.info.value_type
						if param_sym.info.value_type.nr_muls() > 0 && typ.nr_muls() > 0 {
							typ = typ.set_nr_muls(0)
						}
					}
				} else if arg_sym.info is ast.FnType && param_sym.info is ast.FnType {
					if param_sym.info.func.params.len == arg_sym.info.func.params.len {
						for n, fn_param in param_sym.info.func.params {
							if fn_param.typ.has_flag(.generic)
								&& c.table.sym(fn_param.typ).name == gt_name {
								typ = arg_sym.info.func.params[n].typ
								if fn_param.typ.nr_muls() > 0 && typ.nr_muls() > 0 {
									typ = typ.set_nr_muls(0)
								}
							}
						}
						if param_sym.info.func.return_type.has_flag(.generic)
							&& c.table.sym(param_sym.info.func.return_type).name == gt_name {
							typ = arg_sym.info.func.return_type
							if param_sym.info.func.return_type.nr_muls() > 0 && typ.nr_muls() > 0 {
								typ = typ.set_nr_muls(0)
							}
							// resolve lambda with generic return type
							if arg.expr is ast.LambdaExpr && typ.has_flag(.generic) {
								typ = c.type_resolver.unwrap_generic_expr(arg.expr.expr,
									typ)
								if typ.has_flag(.generic) {
									lambda_ret_gt_name := c.table.type_to_str(typ)
									idx := func.generic_names.index(lambda_ret_gt_name)
									if idx < node.concrete_types.len {
										typ = node.concrete_types[idx]
									} else {
										typ = ast.void_type
									}
								}
							}
						}
					}
				} else if arg_sym.kind in [.struct, .interface, .sum_type] {
					mut generic_types := []ast.Type{}
					mut concrete_types := []ast.Type{}
					match arg_sym.info {
						ast.Struct, ast.Interface, ast.SumType {
							if param_sym.generic_types.len > 0 {
								generic_types = param_sym.generic_types.clone()
							} else {
								generic_types = arg_sym.info.generic_types.clone()
							}
							concrete_types = arg_sym.info.concrete_types.clone()
						}
						else {}
					}
					generic_names := generic_types.map(c.table.sym(it).name)
					if gt_name in generic_names && generic_types.len == concrete_types.len {
						idx := generic_names.index(gt_name)
						typ = concrete_types[idx]
					}
				} else if arg_sym.kind == .any && c.table.cur_fn.generic_names.len > 0
					&& c.table.cur_fn.params.len > 0 && func.generic_names.len > 0
					&& arg.expr is ast.Ident && arg_i !in arg_inferred {
					var_name := arg.expr.name
					for k, cur_param in c.table.cur_fn.params {
						if !cur_param.typ.has_flag(.generic) || k < gi || cur_param.name != var_name {
							continue
						}
						typ = cur_param.typ
						mut cparam_type_sym := c.table.sym(c.unwrap_generic(typ))
						if cparam_type_sym.kind == .array {
							typ = c.type_resolver.get_generic_array_element_type(cparam_type_sym.info as ast.Array)
						} else if cparam_type_sym.kind == .array_fixed {
							typ = c.type_resolver.get_generic_array_fixed_element_type(cparam_type_sym.info as ast.ArrayFixed)
						}
						typ = c.unwrap_generic(typ)
						break
					}
				}
			}
			if typ != ast.void_type {
				arg_inferred << arg_i
			}
		}
		if typ == ast.void_type {
			c.error('could not infer generic type `${gt_name}` in call to `${func.name}`',
				node.pos)
			return
		}
		if c.pref.is_verbose {
			s := c.table.type_to_str(typ)
			println('inferred `${func.name}[${s}]`')
		}
		inferred_types << c.unwrap_generic(typ)
		node.concrete_types << typ
	}

	if c.table.register_fn_concrete_types(func.fkey(), inferred_types) {
		c.need_recheck_generic_fns = true
	}
}

// is_contains_any_kind_of_pointer check that the type and submember types(arrays, fixed arrays, maps, struct fields, and so on)
// contain pointer types.
fn (mut c Checker) is_contains_any_kind_of_pointer(typ ast.Type, mut checked_types []ast.Type) bool {
	if typ.is_any_kind_of_pointer() {
		return true
	}
	if typ in checked_types {
		return false
	}
	checked_types << typ
	sym := c.table.sym(typ)
	match sym.info {
		ast.Array, ast.ArrayFixed {
			return c.is_contains_any_kind_of_pointer(sym.info.elem_type, mut checked_types)
		}
		ast.Map {
			return c.is_contains_any_kind_of_pointer(sym.info.value_type, mut checked_types)
		}
		ast.Alias {
			return c.is_contains_any_kind_of_pointer(sym.info.parent_type, mut checked_types)
		}
		ast.Struct {
			if sym.kind == .struct && sym.language == .v {
				fields := c.table.struct_fields(sym)
				for field in fields {
					ret := c.is_contains_any_kind_of_pointer(field.typ, mut checked_types)
					if ret {
						return true
					}
				}
			}
		}
		else {}
	}
	return false
}
