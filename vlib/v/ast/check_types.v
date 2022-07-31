// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

pub fn (t &Table) check_types(got Type, expected Type, is_translated bool) bool {
	if got == expected {
		return true
	}
	got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()
	if is_translated {
		if expected.is_int() && got.is_int() {
			return true
		}
		if expected == byteptr_type {
			return true
		}
		if expected == voidptr_type {
			return true
		}
		if (expected == bool_type && (got.is_any_kind_of_pointer() || got.is_int()))
			|| ((expected.is_any_kind_of_pointer() || expected.is_int()) && got == bool_type) {
			return true
		}

		if expected.is_any_kind_of_pointer() { //&& !got.is_any_kind_of_pointer() {
			// Allow `int` as `&i8` etc in C code.
			deref := expected.deref()
			// deref := expected.set_nr_muls(0)
			got_sym := t.sym(got)
			if deref.is_number() && (got_sym.is_number() || got_sym.kind == .enum_) {
				return true
			}
		}

		// allow rune -> any int and vice versa
		if (expected == rune_type && got.is_int()) || (got == rune_type && expected.is_int()) {
			return true
		}
		got_sym := t.sym(got)
		expected_sym := t.sym(expected)

		// Allow `[N]anyptr` as `[N]anyptr`
		if got_sym.kind == .array && expected_sym.kind == .array {
			if (got_sym.info as Array).elem_type.is_any_kind_of_pointer()
				&& (expected_sym.info as Array).elem_type.is_any_kind_of_pointer() {
				return true
			}
		} else if got_sym.kind == .array_fixed && expected_sym.kind == .array_fixed {
			if (got_sym.info as ArrayFixed).elem_type.is_any_kind_of_pointer()
				&& (expected_sym.info as ArrayFixed).elem_type.is_any_kind_of_pointer() {
				return true
			}
			if t.check_types((got_sym.info as ArrayFixed).elem_type, (expected_sym.info as ArrayFixed).elem_type,
				is_translated)
			{
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
				info := expected_sym.info as ArrayFixed
				info2 := got_sym.info as Array
				if t.check_types(info.elem_type, info2.elem_type, is_translated) {
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
	if exp_idx == voidptr_type_idx || exp_idx == byteptr_type_idx
		|| (expected.is_ptr() && expected.deref().idx() == u8_type_idx) {
		if got.is_ptr() || got.is_pointer() {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if expected.is_real_pointer() {
		if got == int_literal_type {
			return true
		}
	}
	if got_idx == voidptr_type_idx || got_idx == byteptr_type_idx
		|| (got_idx == u8_type_idx && got.is_ptr()) {
		if expected.is_ptr() || expected.is_pointer() {
			return true
		}
	}
	if expected == charptr_type && got == char_type.ref() {
		return true
	}
	if expected.has_flag(.optional) || expected.has_flag(.result) {
		sym := t.sym(got)
		if ((sym.idx == error_type_idx || got in [none_type, error_type])
			&& expected.has_flag(.optional))
			|| ((sym.idx == error_type_idx || got == error_type) && expected.has_flag(.result)) {
			// IErorr
			return true
		} else if !t.check_basic(got, expected.clear_flag(.optional).clear_flag(.result),
			is_translated) {
			return false
		}
	}
	if !t.check_basic(got, expected, is_translated) { // TODO: this should go away...
		return false
	}
	if got.is_number() && expected.is_number() {
		if got == rune_type && expected == u8_type {
			return true
		} else if expected == rune_type && got == u8_type {
			return true
		}
		if t.promote_num(expected, got, is_translated) != expected {
			// println('could not promote ${c.table.sym(got).name} to ${c.table.sym(expected).name}')
			return false
		}
	}
	if expected.has_flag(.generic) {
		return false
	}
	return true
}

fn (t &Table) check_basic(got Type, expected Type, is_translated bool) bool {
	unalias_got, unalias_expected := t.unalias_num_type(got), t.unalias_num_type(expected)
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
	if expected.is_ptr() && unalias_got == int_literal_type {
		return true
	}
	// TODO: use sym so it can be absorbed into below [.voidptr, .any] logic
	if expected.idx() == array_type_idx || got.idx() == array_type_idx {
		return true
	}
	got_sym, exp_sym := t.sym(got), t.sym(expected)
	// multi return
	if exp_sym.kind == .multi_return && got_sym.kind == .multi_return {
		exp_types := exp_sym.mr_info().types
		got_types := got_sym.mr_info().types.map(mktyp(it))
		if exp_types.len != got_types.len {
			return false
		}
		for i in 0 .. exp_types.len {
			if !t.check_types(got_types[i], exp_types[i], is_translated) {
				return false
			}
		}
		return true
	}
	// array/map as argument
	if got_sym.kind in [.array, .map, .array_fixed] && exp_sym.kind == got_sym.kind {
		if t.type_to_str(got) == t.type_to_str(expected).trim('&') {
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
	if t.sumtype_has_variant(expected, mktyp(got), false) {
		return true
	}
	// type alias
	if (got_sym.kind == .alias && got_sym.parent_idx == expected.idx())
		|| (exp_sym.kind == .alias && exp_sym.parent_idx == got.idx()) {
		return true
	}
	// fn type
	if got_sym.kind == .function && exp_sym.kind == .function {
		return t.check_matching_function_symbols(got_sym, exp_sym, is_translated)
	}
	// allow `return 0` in a function with `?int` return type
	expected_nonflagged := expected.clear_flags()
	if got == int_literal_type && expected_nonflagged.is_int() {
		return true
	}
	// allow `return 0` in a function with `?f32` return type
	if got == float_literal_type && expected_nonflagged.is_float() {
		return true
	}
	return false
}

fn (t &Table) check_matching_function_symbols(got_type_sym &TypeSymbol, exp_type_sym &TypeSymbol, is_translated bool) bool {
	if is_translated {
		// TODO too open
		return true
	}
	got_info := got_type_sym.info as FnType
	exp_info := exp_type_sym.info as FnType
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
	if !t.check_basic(got_fn.return_type, exp_fn.return_type, is_translated) {
		return false
	}
	for i, got_arg in got_fn.params {
		exp_arg := exp_fn.params[i]
		exp_arg_is_ptr := exp_arg.typ.is_ptr() || exp_arg.typ.is_pointer()
		got_arg_is_ptr := got_arg.typ.is_ptr() || got_arg.typ.is_pointer()
		if exp_arg_is_ptr != got_arg_is_ptr {
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

pub fn (t &Table) promote(left_type Type, right_type Type, is_translated bool) Type {
	if left_type.is_any_kind_of_pointer() {
		if right_type.is_int() || is_translated {
			return left_type
		} else {
			return void_type
		}
	} else if right_type.is_any_kind_of_pointer() {
		if left_type.is_int() || is_translated {
			return right_type
		} else {
			return void_type
		}
	}
	if left_type == right_type {
		return left_type // strings, self defined operators
	}
	if right_type.is_number() && left_type.is_number() {
		return t.promote_num(left_type, right_type, is_translated)
	} else if left_type.has_flag(.optional) != right_type.has_flag(.optional) {
		// incompatible
		return void_type
	} else {
		return left_type // default to left if not automatic promotion possible
	}
}

pub fn (t &Table) promote_num(left_type Type, right_type Type, is_translated bool) Type {
	// sort the operands to save time
	mut type_hi := left_type
	mut type_lo := right_type
	if type_hi.idx() < type_lo.idx() {
		type_hi, type_lo = type_lo, type_hi
	}
	idx_hi := type_hi.idx()
	idx_lo := type_lo.idx()
	// the following comparisons rely on the order of the indices in table/types.v
	if idx_hi == int_literal_type_idx {
		return type_lo
	} else if idx_hi == float_literal_type_idx {
		if idx_lo in float_type_idxs {
			return type_lo
		} else {
			return void_type
		}
	} else if type_hi.is_float() {
		if idx_hi == f32_type_idx {
			if idx_lo in [i64_type_idx, u64_type_idx] {
				return void_type
			} else {
				return type_hi
			}
		} else { // f64, float_literal
			return type_hi
		}
	} else if idx_lo >= u8_type_idx { // both operands are unsigned
		return type_hi
	} else if idx_lo >= i8_type_idx && (idx_hi <= isize_type_idx || idx_hi == rune_type_idx) { // both signed
		return if idx_lo == i64_type_idx { type_lo } else { type_hi }
	} else if idx_hi - idx_lo < (u8_type_idx - i8_type_idx) {
		return type_lo // conversion unsigned -> signed if signed type is larger
	} else if is_translated {
		return type_hi
	} else {
		return void_type // conversion signed -> unsigned not allowed
	}
}

pub fn (t &Table) check_expected_call_arg(got Type, expected_ Type, language Language, arg CallArg, is_translated bool) ? {
	if got == 0 {
		return error('unexpected 0 type')
	}
	mut expected := expected_
	// variadic
	if expected.has_flag(.variadic) {
		exp_type_sym := t.sym(expected_)
		exp_info := exp_type_sym.info as Array
		expected = exp_info.elem_type
	}
	if language == .c {
		// allow number types to be used interchangeably
		if got.is_number() && expected.is_number() {
			return
		}
		// allow bool & int to be used interchangeably for C functions
		if (got.idx() == bool_type_idx && expected.idx() in [int_type_idx, int_literal_type_idx])
			|| (expected.idx() == bool_type_idx
			&& got.idx() in [int_type_idx, int_literal_type_idx]) {
			return
		}
		exp_sym := t.sym(expected)
		// unknown C types are set to int, allow int to be used for types like `&C.FILE`
		// eg. `C.fflush(C.stderr)` - error: cannot use `int` as `&C.FILE` in argument 1 to `C.fflush`
		if expected.is_ptr() && exp_sym.language == .c && exp_sym.kind in [.placeholder, .struct_]
			&& got == int_type_idx {
			return
		}
	}
	idx_got := got.idx()
	idx_expected := expected.idx()
	if idx_got in [byteptr_type_idx, charptr_type_idx]
		|| idx_expected in [byteptr_type_idx, charptr_type_idx] {
		igot := int(got)
		iexpected := int(expected)
		// TODO: remove; transitional compatibility for byteptr === &byte
		if (igot == byteptr_type_idx && iexpected == 65545)
			|| (iexpected == byteptr_type_idx && igot == 65545) {
			return
		}
		// TODO: remove; transitional compatibility for charptr === &char
		if (igot == charptr_type_idx && iexpected == 65551)
			|| (iexpected == charptr_type_idx && igot == 65551) {
			return
		}
		muls_got := got.nr_muls()
		muls_expected := expected.nr_muls()
		if idx_got == byteptr_type_idx && idx_expected == u8_type_idx
			&& muls_got + 1 == muls_expected {
			return
		}
		if idx_expected == byteptr_type_idx && idx_got == u8_type_idx
			&& muls_expected + 1 == muls_got {
			return
		}
		if idx_got == charptr_type_idx && idx_expected == char_type_idx
			&& muls_got + 1 == muls_expected {
			return
		}
		if idx_expected == charptr_type_idx && idx_got == char_type_idx
			&& muls_expected + 1 == muls_got {
			return
		}
	}

	if t.check_types(got, expected, is_translated) {
		if language != .v || expected.is_ptr() == got.is_ptr() || arg.is_mut
			|| arg.expr.is_auto_deref_var() || got.has_flag(.shared_f)
			|| t.sym(expected_).kind !in [.array, .map] {
			return
		}
	} else {
		got_typ_sym := t.sym(got)
		expected_typ_sym := t.sym(expected_)

		// Check on Generics types, there are some case where we have the following case
		// `&Type<int> == &Type<>`. This is a common case we are implementing a function
		// with generic parameters like `compare(bst Bst<T> node) {}`
		if got_typ_sym.symbol_name_except_generic() == expected_typ_sym.symbol_name_except_generic() {
			// Check if we are making a comparison between two different types of
			// the same type like `Type<int> and &Type<>`
			if (got.is_ptr() != expected.is_ptr()) || !t.check_same_module(got, expected) {
				got_typ_str, expected_typ_str := t.get_string_names_of(got, expected)
				return error('cannot use `$got_typ_str` as `$expected_typ_str`')
			}
			return
		}
		if got == void_type {
			return error('`$arg.expr` (no value) used as value')
		}
		got_typ_str, expected_typ_str := t.get_string_names_of(got, expected)
		return error('cannot use `$got_typ_str` as `$expected_typ_str`')
	}

	if got != void_type {
		got_typ_str, expected_typ_str := t.get_string_names_of(got, expected)
		return error('cannot use `$got_typ_str` as `$expected_typ_str`')
	}
}

fn (t &Table) get_string_names_of(got Type, expected Type) (string, string) {
	got_typ_str := t.type_to_str(got.clear_flag(.variadic))
	expected_typ_str := t.type_to_str(expected.clear_flag(.variadic))
	return got_typ_str, expected_typ_str
}

// helper method to check if the type is of the same module.
// FIXME(vincenzopalazzo) This is a work around to the issue
// explained in the https://github.com/vlang/v/pull/13718#issuecomment-1074517800
fn (t &Table) check_same_module(got Type, expected Type) bool {
	clean_got_typ := t.clean_generics_type_str(got.clear_flag(.variadic)).all_before('<')
	clean_expected_typ := t.clean_generics_type_str(expected.clear_flag(.variadic)).all_before('<')
	if clean_got_typ == clean_expected_typ {
		return true
		// The following if confition should catch the bugs descripted in the issue
	} else if clean_expected_typ.all_after('.') == clean_got_typ.all_after('.') {
		return true
	}
	return false
}

pub fn (t &Table) symmetric_check(left Type, right Type) bool {
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if right.is_ptr() || right.is_pointer() {
		if left == int_literal_type {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	if left.is_ptr() || left.is_pointer() {
		if right == int_literal_type {
			return true
		}
	}
	return t.check_basic(left, right, false)
}
