// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.table
import v.token
import v.ast

pub fn (mut c Checker) check_expected_call_arg(got table.Type, expected_ table.Type) ? {
	mut expected := expected_
	// variadic
	if expected.has_flag(.variadic) {
		exp_type_sym := c.table.get_type_symbol(expected_)
		exp_info := exp_type_sym.info as table.Array
		expected = exp_info.elem_type
	}
	if c.check_types(got, expected) {
		return
	}
	return error('cannot use `${c.table.type_to_str(got.clear_flag(.variadic))}` as `${c.table.type_to_str(expected.clear_flag(.variadic))}`')
}

pub fn (mut c Checker) check_basic(got table.Type, expected table.Type) bool {
	if got == expected {
		return true
	}
	t := c.table
	got_idx := t.unalias_num_type(got).idx()
	exp_idx := t.unalias_num_type(expected).idx()
	// got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()
	// exp_is_optional := expected.has_flag(.optional)
	// got_is_optional := got.has_flag(.optional)
	// if (exp_is_optional && !got_is_optional) || (!exp_is_optional && got_is_optional) {
	// return false
	//}
	// println('check: $got_type_sym.name, $exp_type_sym.name')
	// # NOTE: use idxs here, and symbols below for perf
	if got_idx == exp_idx {
		// this is returning true even if one type is a ptr
		// and the other is not, is this correct behaviour?
		return true
	}
	if got_idx == table.none_type_idx && expected.has_flag(.optional) {
		return false
	}
	// allow pointers to be initialized with 0. TODO: use none instead
	if exp_is_ptr && got_idx == table.int_type_idx {
		return true
	}
	if exp_idx == table.voidptr_type_idx || got_idx == table.voidptr_type_idx {
		return true
	}
	if exp_idx == table.any_type_idx || got_idx == table.any_type_idx {
		return true
	}
	// TODO i64 as int etc
	if (exp_idx in table.pointer_type_idxs ||
		exp_idx in table.number_type_idxs) &&
		(got_idx in table.pointer_type_idxs || got_idx in table.number_type_idxs) {
		return true
	}
	// if exp_idx in pointer_type_idxs && got_idx in pointer_type_idxs {
	// return true
	// }
	// see hack in checker IndexExpr line #691
	if (got_idx == table.byte_type_idx &&
		exp_idx == table.byteptr_type_idx) ||
		(exp_idx == table.byte_type_idx && got_idx == table.byteptr_type_idx) {
		return true
	}
	if (got_idx == table.char_type_idx &&
		exp_idx == table.charptr_type_idx) ||
		(exp_idx == table.char_type_idx && got_idx == table.charptr_type_idx) {
		return true
	}
	// TODO: this should no longer be needed
	// if expected == table.t_type && got == table.t_type {
	// return true
	// }
	// # NOTE: use symbols from this point on for perf
	got_type_sym := t.get_type_symbol(got)
	exp_type_sym := t.get_type_symbol(expected)
	//
	if exp_type_sym.kind == .function && got_type_sym.kind == .int {
		// TODO temporary
		// fn == 0
		return true
	}
	// allow enum value to be used as int
	if (got_type_sym.is_int() && exp_type_sym.kind == .enum_) ||
		(exp_type_sym.is_int() && got_type_sym.kind == .enum_) {
		return true
	}
	// array fn
	if got_type_sym.kind == .array && exp_type_sym.kind == .array {
		if c.table.type_to_str(got) == c.table.type_to_str(expected) {
			return true
		}
	}
	if got_type_sym.kind == .array_fixed && exp_type_sym.kind == .byteptr {
		info := got_type_sym.info as table.ArrayFixed
		if info.elem_type.idx() == table.byte_type_idx {
			return true
		}
	}
	// TODO
	// if exp_type_sym.name == 'array' || got_type_sym.name == 'array' {
	if got_idx == table.array_type_idx || exp_idx == table.array_type_idx {
		return true
	}
	// TODO
	// accept [] when an expected type is an array
	if got_type_sym.kind == .array &&
		exp_type_sym.kind == .array && got_type_sym.name == 'array_void' {
		return true
	}
	// type alias
	if (got_type_sym.kind == .alias &&
		got_type_sym.parent_idx == exp_idx) ||
		(exp_type_sym.kind == .alias && exp_type_sym.parent_idx == got_idx) {
		return true
	}
	// sum type
	if c.table.sumtype_has_variant(expected, got) {
		return true
	}
	// fn type
	if got_type_sym.kind == .function && exp_type_sym.kind == .function {
		return c.check_matching_function_symbols(got_type_sym, exp_type_sym)
	}
	return false
}

pub fn (mut c Checker) check_matching_function_symbols(got_type_sym &table.TypeSymbol, exp_type_sym &table.TypeSymbol) bool {
	got_info := got_type_sym.info as table.FnType
	exp_info := exp_type_sym.info as table.FnType
	got_fn := got_info.func
	exp_fn := exp_info.func
	// we are using check() to compare return type & args as they might include
	// functions themselves. TODO: optimize, only use check() when needed
	if got_fn.params.len != exp_fn.params.len {
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
			c.add_error_detail("`$exp_fn.name`\'s expected fn argument: `$exp_arg.name` is $exp_arg_pointedness, but the passed fn argument: `$got_arg.name` is $got_arg_pointedness")
			return false
		}
		if !c.check_basic(got_arg.typ, exp_arg.typ) {
			return false
		}
	}
	return true
}

[inline]
fn (mut c Checker) check_shift(left_type table.Type, right_type table.Type, left_pos token.Position, right_pos token.Position) table.Type {
	if !left_type.is_int() {
		// maybe it's an int alias? TODO move this to is_int() ?
		sym := c.table.get_type_symbol(left_type)
		if sym.kind == .alias && (sym.info as table.Alias).parent_type.is_int() {
			return left_type
		}
		if c.pref.translated && left_type == table.bool_type {
			// allow `bool << 2` in translated C code
			return table.int_type
		}
		c.error('invalid operation: shift of type `$sym.name`', left_pos)
		return table.void_type
	} else if !right_type.is_int() {
		c.error('cannot shift non-integer type ${c.table.get_type_symbol(right_type).name} into type ${c.table.get_type_symbol(left_type).name}',
			right_pos)
		return table.void_type
	}
	return left_type
}

pub fn (c &Checker) promote(left_type table.Type, right_type table.Type) table.Type {
	if left_type.is_ptr() || left_type.is_pointer() {
		if right_type.is_int() {
			return left_type
		} else {
			return table.void_type
		}
	} else if right_type.is_ptr() || right_type.is_pointer() {
		if left_type.is_int() {
			return right_type
		} else {
			return table.void_type
		}
	}
	if left_type == right_type {
		return left_type // strings, self defined operators
	}
	if right_type.is_number() && left_type.is_number() {
		return c.promote_num(left_type, right_type)
	} else if left_type.has_flag(.optional) != right_type.has_flag(.optional) {
		// incompatible
		return table.void_type
	} else {
		return left_type // default to left if not automatic promotion possible
	}
}

fn (c &Checker) promote_num(left_type table.Type, right_type table.Type) table.Type {
	// sort the operands to save time
	mut type_hi := left_type
	mut type_lo := right_type
	if type_hi.idx() < type_lo.idx() {
		type_hi, type_lo = type_lo, type_hi
	}
	idx_hi := type_hi.idx()
	idx_lo := type_lo.idx()
	// the following comparisons rely on the order of the indices in atypes.v
	if idx_hi == table.any_int_type_idx {
		return type_lo
	} else if idx_hi == table.any_flt_type_idx {
		if idx_lo in table.float_type_idxs {
			return type_lo
		} else {
			return table.void_type
		}
	} else if type_hi.is_float() {
		if idx_hi == table.f32_type_idx {
			if idx_lo in [table.i64_type_idx, table.u64_type_idx] {
				return table.void_type
			} else {
				return type_hi
			}
		} else { // f64, any_flt
			return type_hi
		}
	} else if idx_lo >= table.byte_type_idx { // both operands are unsigned
		return type_hi
	} else if idx_lo >= table.i8_type_idx &&
		(idx_hi <= table.i64_type_idx || idx_hi == table.rune_type_idx) { // both signed
		return if idx_lo == table.i64_type_idx {
			type_lo
		} else {
			type_hi
		}
	} else if idx_hi - idx_lo < (table.byte_type_idx - table.i8_type_idx) {
		return type_lo // conversion unsigned -> signed if signed type is larger
	} else {
		return table.void_type // conversion signed -> unsigned not allowed
	}
}

// TODO: promote(), check_types(), symmetric_check() and check() overlap - should be rearranged
pub fn (mut c Checker) check_types(got table.Type, expected table.Type) bool {
	if got == expected {
		return true
	}
	exp_idx := expected.idx()
	got_idx := got.idx()
	if exp_idx == got_idx {
		return true
	}
	if exp_idx == table.voidptr_type_idx || exp_idx == table.byteptr_type_idx {
		if got.is_ptr() || got.is_pointer() {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if expected.is_ptr() || expected.is_pointer() {
		if got == table.any_int_type {
			return true
		}
	}
	if got_idx == table.voidptr_type_idx || got_idx == table.byteptr_type_idx {
		if expected.is_ptr() || expected.is_pointer() {
			return true
		}
	}
	if !c.check_basic(got, expected) { // TODO: this should go away...
		return false
	}
	if got.is_number() && expected.is_number() {
		if got == table.rune_type && expected == table.byte_type {
			return true
		} else if expected == table.rune_type && got == table.byte_type {
			return true
		}
		if c.promote_num(expected, got) != expected {
			// println('could not promote ${c.table.get_type_symbol(got).name} to ${c.table.get_type_symbol(expected).name}')
			return false
		}
	}
	return true
}

pub fn (mut c Checker) check_expected(got table.Type, expected table.Type) ? {
	if c.check_types(got, expected) {
		return
	}
	return error(c.expected_msg(got, expected))
}

[inline]
fn (c &Checker) expected_msg(got table.Type, expected table.Type) string {
	exps := c.table.type_to_str(expected)
	gots := c.table.type_to_str(got)
	return 'expected `$exps`, not `$gots`'
}

pub fn (mut c Checker) symmetric_check(left table.Type, right table.Type) bool {
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if right.is_ptr() || right.is_pointer() {
		if left == table.any_int_type {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	if left.is_ptr() || left.is_pointer() {
		if right == table.any_int_type {
			return true
		}
	}
	return c.check_basic(left, right)
}

pub fn (c &Checker) get_default_fmt(ftyp table.Type, typ table.Type) byte {
	if ftyp.has_flag(.optional) {
		return `s`
	} else if typ.is_float() {
		return `g`
	} else if typ.is_signed() || typ.is_any_int() {
		return `d`
	} else if typ.is_unsigned() {
		return `u`
	} else if typ.is_pointer() {
		return `p`
	} else {
		mut sym := c.table.get_type_symbol(c.unwrap_generic(ftyp))
		if sym.kind == .alias {
			// string aliases should be printable
			info := sym.info as table.Alias
			sym = c.table.get_type_symbol(info.parent_type)
			if info.parent_type == table.string_type {
				return `s`
			}
		}
		if sym.kind == .function {
			return `s`
		}
		if ftyp in [table.string_type, table.bool_type] ||
			sym.kind in
			[.enum_, .array, .array_fixed, .struct_, .map, .multi_return, .sum_type, .none_] || ftyp.has_flag(.optional) ||
			sym.has_method('str') {
			return `s`
		} else {
			return `_`
		}
	}
}

pub fn (mut c Checker) string_inter_lit(mut node ast.StringInterLiteral) table.Type {
	for i, expr in node.exprs {
		ftyp := c.expr(expr)
		node.expr_types << ftyp
		typ := c.table.unalias_num_type(ftyp)
		mut fmt := node.fmts[i]
		// analyze and validate format specifier
		if fmt !in
			[`E`, `F`, `G`, `e`, `f`, `g`, `d`, `u`, `x`, `X`, `o`, `c`, `s`, `p`, `_`] {
			c.error('unknown format specifier `${fmt:c}`', node.fmt_poss[i])
		}
		if fmt == `_` { // set default representation for type if none has been given
			fmt = c.get_default_fmt(ftyp, typ)
			if fmt == `_` {
				if typ != table.void_type {
					c.error('no known default format for type `${c.table.get_type_name(ftyp)}`',
						node.fmt_poss[i])
				}
			} else {
				node.fmts[i] = fmt
				node.need_fmts[i] = false
			}
		} else { // check if given format specifier is valid for type
			if node.precisions[i] != 987698 && !typ.is_float() {
				c.error('precision specification only valid for float types', node.fmt_poss[i])
			}
			if node.pluss[i] && !typ.is_number() {
				c.error('plus prefix only allowed for numbers', node.fmt_poss[i])
			}
			if (typ.is_unsigned() && fmt !in [`u`, `x`, `X`, `o`, `c`]) ||
				(typ.is_signed() && fmt !in [`d`, `x`, `X`, `o`, `c`]) ||
				(typ.is_any_int() && fmt !in [`d`, `c`, `x`, `X`, `o`, `u`, `x`, `X`, `o`]) ||
				(typ.is_float() && fmt !in [`E`, `F`, `G`, `e`, `f`, `g`]) ||
				(typ.is_pointer() && fmt !in [`p`, `x`, `X`]) ||
				(typ.is_string() && fmt != `s`) ||
				(typ.idx() in [table.i64_type_idx, table.f64_type_idx] && fmt == `c`) {
				c.error('illegal format specifier `${fmt:c}` for type `${c.table.get_type_name(ftyp)}`',
					node.fmt_poss[i])
			}
			node.need_fmts[i] = fmt != c.get_default_fmt(ftyp, typ)
		}
		// check recursive str
		if c.cur_fn.is_method && c.cur_fn.name == 'str' && c.cur_fn.receiver.name == expr.str() {
			c.error('cannot call `str()` method recursively', expr.position())
		}
	}
	return table.string_type
}

pub fn (mut c Checker) infer_fn_types(f table.Fn, mut call_expr ast.CallExpr) {
	gt_name := 'T'
	mut typ := table.void_type
	for i, param in f.params {
		arg := call_expr.args[i]
		if param.typ.has_flag(.generic) {
			typ = arg.typ
			break
		}
		arg_sym := c.table.get_type_symbol(arg.typ)
		param_type_sym := c.table.get_type_symbol(param.typ)
		if arg_sym.kind == .array && param_type_sym.kind == .array {
			param_info := param_type_sym.info as table.Array
			if param_info.elem_type.has_flag(.generic) {
				arg_info := arg_sym.info as table.Array
				typ = arg_info.elem_type
				break
			}
		}
	}
	if typ == table.void_type {
		c.error('could not infer generic type `$gt_name` in call to `$f.name`', call_expr.pos)
	} else {
		if c.pref.is_verbose {
			s := c.table.type_to_str(typ)
			println('inferred `$f.name<$s>`')
		}
		c.table.register_fn_gen_type(f.name, typ)
		call_expr.generic_type = typ
	}
}
