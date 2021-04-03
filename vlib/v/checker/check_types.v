// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module checker

import v.ast
import v.token

pub fn (mut c Checker) check_expected_call_arg(got ast.Type, expected_ ast.Type, language ast.Language) ? {
	mut expected := expected_
	// variadic
	if expected.has_flag(.variadic) {
		exp_type_sym := c.table.get_type_symbol(expected_)
		exp_info := exp_type_sym.info as ast.Array
		expected = exp_info.elem_type
	}
	if language == .c {
		// allow number types to be used interchangeably
		if got.is_number() && expected.is_number() {
			return
		}
		// mode_t - currently using u32 as mode_t for C fns
		// if got.idx() in [ast.int_type_idx, ast.u32_type_idx] && expected.idx() in [ast.int_type_idx, ast.u32_type_idx] {
		// 	return
		// }
		// allow number to be used as size_t
		if got.is_number() && expected.idx() == ast.size_t_type_idx {
			return
		}
		// allow bool & int to be used interchangeably for C functions
		if (got.idx() == ast.bool_type_idx
			&& expected.idx() in [ast.int_type_idx, ast.int_literal_type_idx])
			|| (expected.idx() == ast.bool_type_idx
			&& got.idx() in [ast.int_type_idx, ast.int_literal_type_idx]) {
			return
		}
		if got.idx() == ast.string_type_idx
			&& expected in [ast.byteptr_type_idx, ast.charptr_type_idx] {
			return
		}
		exp_sym := c.table.get_type_symbol(expected)
		// unknown C types are set to int, allow int to be used for types like `&C.FILE`
		// eg. `C.fflush(C.stderr)` - error: cannot use `int` as `&C.FILE` in argument 1 to `C.fflush`
		if expected.is_ptr() && exp_sym.language == .c && exp_sym.kind == .placeholder
			&& got == ast.int_type_idx {
			return
		}
		// return
	}
	if c.check_types(got, expected) {
		return
	}
	return error('cannot use `${c.table.type_to_str(got.clear_flag(.variadic))}` as `${c.table.type_to_str(expected.clear_flag(.variadic))}`')
}

pub fn (mut c Checker) check_basic(got ast.Type, expected ast.Type) bool {
	got_, exp_ := c.table.unalias_num_type(got), c.table.unalias_num_type(expected)
	if got_.idx() == exp_.idx() {
		// this is returning true even if one type is a ptr
		// and the other is not, is this correct behaviour?
		return true
	}
	if (exp_.is_pointer() || exp_.is_number()) && (got_.is_pointer() || got_.is_number()) {
		return true
	}
	// allow pointers to be initialized with 0. TODO: use none instead
	if expected.is_ptr() && got_ == ast.int_literal_type {
		return true
	}
	// TODO: use sym so it can be absorbed into below [.voidptr, .any] logic
	if expected.idx() == ast.array_type_idx || got.idx() == ast.array_type_idx {
		return true
	}
	got_sym, exp_sym := c.table.get_type_symbol(got), c.table.get_type_symbol(expected)
	// array/map as argument
	if got_sym.kind in [.array, .map, .array_fixed] && exp_sym.kind == got_sym.kind {
		if c.table.type_to_str(got) == c.table.type_to_str(expected).trim('&') {
			return true
		}
	}
	if !got_.is_ptr() && got_sym.kind == .array_fixed && (exp_.is_pointer() || exp_.is_ptr()) {
		// fixed array needs to be a struct, not a pointer
		return false
	}
	if exp_sym.kind in [.voidptr, .any] || got_sym.kind in [.voidptr, .any] {
		return true
	}
	// sum type
	if c.table.sumtype_has_variant(expected, c.table.mktyp(got)) {
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
	// allow using Error as a string for now (avoid a breaking change)
	if got == ast.error_type_idx && expected == ast.string_type_idx {
		return true
	}
	return false
}

pub fn (mut c Checker) check_matching_function_symbols(got_type_sym &ast.TypeSymbol, exp_type_sym &ast.TypeSymbol) bool {
	got_info := got_type_sym.info as ast.FnType
	exp_info := exp_type_sym.info as ast.FnType
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
fn (mut c Checker) check_shift(left_type ast.Type, right_type ast.Type, left_pos token.Position, right_pos token.Position) ast.Type {
	if !left_type.is_int() {
		// maybe it's an int alias? TODO move this to is_int() ?
		sym := c.table.get_type_symbol(left_type)
		if sym.kind == .alias && (sym.info as ast.Alias).parent_type.is_int() {
			return left_type
		}
		if c.pref.translated && left_type == ast.bool_type {
			// allow `bool << 2` in translated C code
			return ast.int_type
		}
		c.error('invalid operation: shift on type `$sym.name`', left_pos)
		return ast.void_type
	} else if !right_type.is_int() {
		c.error('cannot shift non-integer type `${c.table.get_type_symbol(right_type).name}` into type `${c.table.get_type_symbol(left_type).name}`',
			right_pos)
		return ast.void_type
	}
	return left_type
}

pub fn (c &Checker) promote(left_type ast.Type, right_type ast.Type) ast.Type {
	if left_type.is_ptr() || left_type.is_pointer() {
		if right_type.is_int() {
			return left_type
		} else {
			return ast.void_type
		}
	} else if right_type.is_ptr() || right_type.is_pointer() {
		if left_type.is_int() {
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
	} else if idx_lo >= ast.byte_type_idx { // both operands are unsigned
		return type_hi
	} else if idx_lo >= ast.i8_type_idx
		&& (idx_hi <= ast.i64_type_idx || idx_hi == ast.rune_type_idx) { // both signed
		return if idx_lo == ast.i64_type_idx { type_lo } else { type_hi }
	} else if idx_hi - idx_lo < (ast.byte_type_idx - ast.i8_type_idx) {
		return type_lo // conversion unsigned -> signed if signed type is larger
	} else {
		return ast.void_type // conversion signed -> unsigned not allowed
	}
}

// TODO: promote(), check_types(), symmetric_check() and check() overlap - should be rearranged
pub fn (mut c Checker) check_types(got ast.Type, expected ast.Type) bool {
	if got == expected {
		return true
	}
	got_is_ptr := got.is_ptr()
	exp_is_ptr := expected.is_ptr()
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
	if exp_idx == ast.voidptr_type_idx || exp_idx == ast.byteptr_type_idx {
		if got.is_ptr() || got.is_pointer() {
			return true
		}
	}
	// allow direct int-literal assignment for pointers for now
	// maybe in the future optionals should be used for that
	if expected.is_ptr() || expected.is_pointer() {
		if got == ast.int_literal_type {
			return true
		}
	}
	if got_idx == ast.voidptr_type_idx || got_idx == ast.byteptr_type_idx {
		if expected.is_ptr() || expected.is_pointer() {
			return true
		}
	}
	if expected == ast.charptr_type && got == ast.char_type.to_ptr() {
		return true
	}
	if !c.check_basic(got, expected) { // TODO: this should go away...
		return false
	}
	if got.is_number() && expected.is_number() {
		if got == ast.rune_type && expected == ast.byte_type {
			return true
		} else if expected == ast.rune_type && got == ast.byte_type {
			return true
		}
		if c.promote_num(expected, got) != expected {
			// println('could not promote ${c.table.get_type_symbol(got).name} to ${c.table.get_type_symbol(expected).name}')
			return false
		}
	}
	if expected.has_flag(.generic) {
		return false
	}
	return true
}

pub fn (mut c Checker) check_expected(got ast.Type, expected ast.Type) ? {
	if c.check_types(got, expected) {
		return
	}
	return error(c.expected_msg(got, expected))
}

[inline]
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

pub fn (c &Checker) get_default_fmt(ftyp ast.Type, typ ast.Type) byte {
	if ftyp.has_flag(.optional) {
		return `s`
	} else if typ.is_float() {
		return `g`
	} else if typ.is_signed() || typ.is_int_literal() {
		return `d`
	} else if typ.is_unsigned() {
		return `u`
	} else if typ.is_pointer() {
		return `p`
	} else {
		mut sym := c.table.get_type_symbol(c.unwrap_generic(ftyp))
		if sym.kind == .alias {
			// string aliases should be printable
			info := sym.info as ast.Alias
			sym = c.table.get_type_symbol(info.parent_type)
			if info.parent_type == ast.string_type {
				return `s`
			}
		}
		if sym.kind == .function {
			return `s`
		}
		if ftyp in [ast.string_type, ast.bool_type]
			|| sym.kind in [.enum_, .array, .array_fixed, .struct_, .map, .multi_return, .sum_type, .interface_, .none_]
			|| ftyp.has_flag(.optional) || sym.has_method('str') {
			return `s`
		} else {
			return `_`
		}
	}
}

pub fn (mut c Checker) fail_if_unreadable(expr ast.Expr, typ ast.Type, what string) {
	mut pos := token.Position{}
	match expr {
		ast.Ident {
			if typ.has_flag(.shared_f) {
				if expr.name !in c.rlocked_names && expr.name !in c.locked_names {
					action := if what == 'argument' { 'passed' } else { 'used' }
					c.error('$expr.name is `shared` and must be `rlock`ed or `lock`ed to be $action as non-mut $what',
						expr.pos)
				}
			}
			return
		}
		ast.SelectorExpr {
			pos = expr.pos
			c.fail_if_unreadable(expr.expr, expr.expr_type, what)
		}
		ast.IndexExpr {
			pos = expr.left.position().extend(expr.pos)
			c.fail_if_unreadable(expr.left, expr.left_type, what)
		}
		else {}
	}
	if typ.has_flag(.shared_f) {
		c.error('you have to create a handle and `rlock` it to use a `shared` element as non-mut $what',
			pos)
	}
}

pub fn (mut c Checker) string_inter_lit(mut node ast.StringInterLiteral) ast.Type {
	inside_println_arg_save := c.inside_println_arg
	c.inside_println_arg = true
	for i, expr in node.exprs {
		ftyp := c.expr(expr)
		c.fail_if_unreadable(expr, ftyp, 'interpolation object')
		node.expr_types << ftyp
		typ := c.table.unalias_num_type(ftyp)
		mut fmt := node.fmts[i]
		// analyze and validate format specifier
		if fmt !in [`E`, `F`, `G`, `e`, `f`, `g`, `d`, `u`, `x`, `X`, `o`, `c`, `s`, `p`, `_`] {
			c.error('unknown format specifier `${fmt:c}`', node.fmt_poss[i])
		}
		if fmt == `_` { // set default representation for type if none has been given
			fmt = c.get_default_fmt(ftyp, typ)
			if fmt == `_` {
				if typ != ast.void_type {
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
			if (typ.is_unsigned() && fmt !in [`u`, `x`, `X`, `o`, `c`])
				|| (typ.is_signed() && fmt !in [`d`, `x`, `X`, `o`, `c`])
				|| (typ.is_int_literal() && fmt !in [`d`, `c`, `x`, `X`, `o`, `u`, `x`, `X`, `o`])
				|| (typ.is_float() && fmt !in [`E`, `F`, `G`, `e`, `f`, `g`])
				|| (typ.is_pointer() && fmt !in [`p`, `x`, `X`])
				|| (typ.is_string() && fmt != `s`)
				|| (typ.idx() in [ast.i64_type_idx, ast.f64_type_idx] && fmt == `c`) {
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
	c.inside_println_arg = inside_println_arg_save
	return ast.string_type
}

pub fn (mut c Checker) infer_fn_types(f ast.Fn, mut call_expr ast.CallExpr) {
	mut inferred_types := []ast.Type{}
	for gi, gt_name in f.generic_names {
		// skip known types
		if gi < call_expr.generic_types.len {
			inferred_types << call_expr.generic_types[gi]
			continue
		}
		mut typ := ast.void_type
		for i, param in f.params {
			mut to_set := ast.void_type
			arg_i := if i != 0 && call_expr.is_method { i - 1 } else { i }
			if call_expr.args.len <= arg_i {
				break
			}
			arg := call_expr.args[arg_i]
			param_type_sym := c.table.get_type_symbol(param.typ)

			if param.typ.has_flag(.generic) && param_type_sym.name == gt_name {
				to_set = c.table.mktyp(arg.typ)
				if arg.expr.is_auto_deref_var() {
					to_set = to_set.deref()
				}
				// If the parent fn param is a generic too
				if to_set.has_flag(.generic) {
					to_set = c.unwrap_generic(to_set)
				}
			} else {
				arg_sym := c.table.get_type_symbol(arg.typ)
				if arg_sym.kind == .array && param_type_sym.kind == .array {
					mut arg_elem_info := arg_sym.info as ast.Array
					mut param_elem_info := param_type_sym.info as ast.Array
					mut arg_elem_sym := c.table.get_type_symbol(arg_elem_info.elem_type)
					mut param_elem_sym := c.table.get_type_symbol(param_elem_info.elem_type)
					for {
						if arg_elem_sym.kind == .array && param_elem_sym.kind == .array
							&& c.cur_fn.generic_params.filter(it.name == param_elem_sym.name).len == 0 {
							arg_elem_info = arg_elem_sym.info as ast.Array
							arg_elem_sym = c.table.get_type_symbol(arg_elem_info.elem_type)
							param_elem_info = param_elem_sym.info as ast.Array
							param_elem_sym = c.table.get_type_symbol(param_elem_info.elem_type)
						} else {
							to_set = arg_elem_info.elem_type
							break
						}
					}
				} else if param.typ.has_flag(.variadic) {
					to_set = c.table.mktyp(arg.typ)
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
						c.error('inferred generic type `$gt_name` is ambigous got `${c.table.get_type_symbol(to_set).name}`, expected `${c.table.get_type_symbol(typ).name}`',
							arg.pos)
					}
				}
				typ = to_set
			}
		}
		if typ == ast.void_type {
			c.error('could not infer generic type `$gt_name` in call to `$f.name`', call_expr.pos)
			return
		}
		if c.pref.is_verbose {
			s := c.table.type_to_str(typ)
			println('inferred `$f.name<$s>`')
		}
		inferred_types << typ
		call_expr.generic_types << typ
	}
	c.table.register_fn_gen_type(f.name, inferred_types)
}
