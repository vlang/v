// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token

fn (mut c Checker) array_init(mut node ast.ArrayInit) ast.Type {
	mut elem_type := ast.void_type
	unwrap_elem_type := c.unwrap_generic(node.elem_type)
	// `x := []string{}` (the type was set in the parser)
	if node.typ != ast.void_type {
		if node.elem_type != 0 {
			elem_sym := c.table.sym(node.elem_type)

			if node.typ.has_flag(.option) && (node.has_cap || node.has_len) {
				c.error('Option array `${elem_sym.name}` cannot have initializers', node.pos)
			}
			match elem_sym.info {
				ast.Struct {
					if elem_sym.info.generic_types.len > 0 && elem_sym.info.concrete_types.len == 0
						&& !node.elem_type.has_flag(.generic) {
						if c.table.cur_concrete_types.len == 0 {
							c.error('generic struct `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[int]',
								node.elem_type_pos)
						} else {
							c.error('generic struct `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[T]',
								node.elem_type_pos)
						}
					}
				}
				ast.Interface {
					if elem_sym.info.generic_types.len > 0 && elem_sym.info.concrete_types.len == 0
						&& !node.elem_type.has_flag(.generic) {
						if c.table.cur_concrete_types.len == 0 {
							c.error('generic interface `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[int]',
								node.elem_type_pos)
						} else {
							c.error('generic interface `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[T]',
								node.elem_type_pos)
						}
					}
				}
				ast.SumType {
					if elem_sym.info.generic_types.len > 0 && elem_sym.info.concrete_types.len == 0
						&& !node.elem_type.has_flag(.generic) {
						if c.table.cur_concrete_types.len == 0 {
							c.error('generic sumtype `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[int]',
								node.elem_type_pos)
						} else {
							c.error('generic sumtype `${elem_sym.name}` must specify type parameter, e.g. ${elem_sym.name}[T]',
								node.elem_type_pos)
						}
					}
				}
				ast.Alias {
					if elem_sym.name == 'byte' {
						c.warn('byte is deprecated, use u8 instead', node.elem_type_pos)
					}
				}
				else {}
			}
		}
		if node.exprs.len == 0 {
			if node.has_cap {
				c.check_array_init_para_type('cap', mut node.cap_expr, node.pos)
			}
			if node.has_len {
				c.check_array_init_para_type('len', mut node.len_expr, node.pos)
			}
		}
		if node.has_init {
			c.check_array_init_default_expr(mut node)
		}
		if node.has_len {
			len_typ := c.check_expr_option_or_result_call(node.len_expr, c.expr(mut node.len_expr))
			if len_typ.has_flag(.option) {
				c.error('cannot use unwrapped Option as length', node.len_expr.pos())
			}
			// check &int{}, interface, sum_type initialized
			if !node.has_init {
				c.check_elements_initialized(unwrap_elem_type) or {
					c.warn('${err.msg()}, therefore `len:` cannot be used (unless inside `unsafe`, or if you also use `init:`)',
						node.pos)
				}
			}
		}
		if node.has_cap {
			cap_typ := c.check_expr_option_or_result_call(node.cap_expr, c.expr(mut node.cap_expr))
			if cap_typ.has_flag(.option) {
				c.error('cannot use unwrapped Option as capacity', node.cap_expr.pos())
			}
		}
		c.ensure_type_exists(node.elem_type, node.elem_type_pos)
		if node.typ.has_flag(.generic) && c.table.cur_fn != unsafe { nil }
			&& c.table.cur_fn.generic_names.len == 0 {
			c.error('generic struct cannot be used in non-generic function', node.pos)
		}

		// `&Struct{} check
		if node.has_len {
			c.check_elements_ref_fields_initialized(unwrap_elem_type, node.pos)
		}
		return node.typ
	}

	if node.is_fixed {
		c.ensure_type_exists(node.elem_type, node.elem_type_pos)
		if !c.is_builtin_mod {
			c.check_elements_initialized(unwrap_elem_type) or {
				c.warn('fixed ${err.msg()} (unless inside `unsafe`)', node.pos)
			}
		}
		c.check_elements_ref_fields_initialized(unwrap_elem_type, node.pos)
	}
	// `a = []`
	if node.exprs.len == 0 {
		// `a := fn_returning_opt_array() or { [] }`
		if c.expected_type == ast.void_type && c.expected_or_type != ast.void_type {
			c.expected_type = c.expected_or_type
		}
		mut type_sym := c.table.sym(c.expected_type)
		if type_sym.kind != .array || type_sym.array_info().elem_type == ast.void_type {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				node.pos)
			return ast.void_type
		}
		array_info := type_sym.array_info()
		node.elem_type = array_info.elem_type
		// clear option flag in case of: `fn opt_arr() ?[]int { return [] }`
		return if c.expected_type.has_flag(.shared_f) {
			c.expected_type.clear_flag(.shared_f).deref()
		} else {
			c.expected_type
		}.clear_option_and_result()
	}
	// `[1,2,3]`
	if node.exprs.len > 0 && node.elem_type == ast.void_type {
		mut expected_value_type := ast.void_type
		mut expecting_interface_array := false
		mut expecting_sumtype_array := false
		mut is_first_elem_ptr := false
		if c.expected_type != 0 {
			expected_value_type = c.table.value_type(c.expected_type)
			expected_value_sym := c.table.sym(expected_value_type)
			if expected_value_sym.kind == .interface_ {
				// array of interfaces? (`[dog, cat]`) Save the interface type (`Animal`)
				expecting_interface_array = true
			} else if expected_value_sym.kind == .sum_type {
				expecting_sumtype_array = true
			}
		}
		for i, mut expr in node.exprs {
			typ := c.check_expr_option_or_result_call(expr, c.expr(mut expr))
			if typ == ast.void_type {
				c.error('invalid void array element type', expr.pos())
			}
			node.expr_types << typ
			// the first element's type
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
					c.type_implements(typ, elem_type, expr.pos())
				}
				if !typ.is_any_kind_of_pointer() && !c.inside_unsafe {
					typ_sym := c.table.sym(typ)
					if typ_sym.kind != .interface_ {
						c.mark_as_referenced(mut &expr, true)
					}
				}
				continue
			} else if expecting_sumtype_array {
				if i == 0 {
					if c.table.is_sumtype_or_in_variant(expected_value_type, ast.mktyp(typ)) {
						elem_type = expected_value_type
					} else {
						if expr.is_auto_deref_var() {
							elem_type = ast.mktyp(typ.deref())
						} else {
							elem_type = ast.mktyp(typ)
						}
					}
					c.expected_type = elem_type
				}
				continue
			}
			// the first element's type
			if i == 0 {
				if expr.is_auto_deref_var() {
					elem_type = ast.mktyp(typ.deref())
				} else {
					elem_type = ast.mktyp(typ)
				}
				if typ.is_ptr() && c.in_for_count == 0 {
					is_first_elem_ptr = true
				}
				c.expected_type = elem_type
				continue
			} else {
				if !typ.is_any_kind_of_pointer() && !typ.is_int() && is_first_elem_ptr {
					c.error('cannot have non-pointer of type `${c.table.type_to_str(typ)}` in a pointer array of type `${c.table.type_to_str(elem_type)}`',
						expr.pos())
				}
			}
			if expr !is ast.TypeNode {
				if c.table.type_kind(elem_type) == .interface_ {
					if c.type_implements(typ, elem_type, expr.pos()) {
						continue
					}
				}
				c.check_expected(typ, elem_type) or {
					c.error('invalid array element: ${err.msg()}', expr.pos())
				}
				if !elem_type.has_flag(.option)
					&& (typ.has_flag(.option) || typ.idx() == ast.none_type_idx) {
					typ_str, elem_type_str := c.get_string_names_of(typ, elem_type)
					if typ.idx() == ast.none_type_idx {
						c.error('cannot use `${typ_str}` as `${elem_type_str}`', expr.pos())
					} else {
						c.error('cannot use `${typ_str}` as `${elem_type_str}`, it must be unwrapped first',
							expr.pos())
					}
				} else if elem_type.has_flag(.option) && !typ.has_flag(.option)
					&& typ.idx() != ast.none_type_idx && !expr.is_pure_literal() {
					typ_str, elem_type_str := c.get_string_names_of(typ, elem_type)
					c.error('cannot use `${typ_str}` as `${elem_type_str}`', expr.pos())
				}
			}
		}
		if node.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, node.exprs.len, ast.empty_expr,
				false)
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		} else {
			idx := c.table.find_or_register_array(elem_type)
			if elem_type.has_flag(.generic) {
				node.typ = ast.new_type(idx).set_flag(.generic)
			} else {
				node.typ = ast.new_type(idx)
			}
		}
		node.elem_type = elem_type
	} else if node.is_fixed && node.exprs.len == 1 && node.elem_type != ast.void_type {
		// `[50]u8`
		sym := c.table.sym(node.typ)
		if sym.info !is ast.ArrayFixed
			|| c.array_fixed_has_unresolved_size(sym.info as ast.ArrayFixed) {
			mut size_expr := node.exprs[0]
			node.typ = c.eval_array_fixed_sizes(mut size_expr, 0, node.elem_type)
			node.elem_type = (c.table.sym(node.typ).info as ast.ArrayFixed).elem_type
		}
		if node.has_init {
			c.check_array_init_default_expr(mut node)
		}
	}
	return node.typ
}

fn (mut c Checker) check_array_init_default_expr(mut node ast.ArrayInit) {
	mut init_expr := node.init_expr
	init_typ := c.check_expr_option_or_result_call(init_expr, c.expr(mut init_expr))
	node.init_type = init_typ
	if !node.elem_type.has_flag(.option) && init_typ.has_flag(.option) {
		c.error('cannot use unwrapped Option as initializer', init_expr.pos())
	}
	c.check_expected(init_typ, node.elem_type) or { c.error(err.msg(), init_expr.pos()) }
}

fn (mut c Checker) check_array_init_para_type(para string, mut expr ast.Expr, pos token.Pos) {
	sym := c.table.sym(c.unwrap_generic(c.expr(mut expr)))
	if sym.kind !in [.int, .int_literal] {
		c.error('array ${para} needs to be an int', pos)
	}
	if expr is ast.IntegerLiteral {
		lit := expr as ast.IntegerLiteral
		if lit.val.int() < 0 {
			c.error('array ${para} can not be negative', lit.pos)
		}
	}
}

// When the fixed array has multiple dimensions, it needs to be evaluated recursively.
// `[const]int`, `[const][3]int`, `[3][const]int`, `[const + 1][3][const]int`...
fn (mut c Checker) eval_array_fixed_sizes(mut size_expr ast.Expr, size int, elem_type ast.Type) ast.Type {
	elem_sym := c.table.sym(elem_type)
	elem_info := elem_sym.info

	new_elem_typ := if elem_sym.kind == .array_fixed {
		mut info := elem_info as ast.ArrayFixed
		mut elem_size_expr := unsafe { info.size_expr }
		c.eval_array_fixed_sizes(mut elem_size_expr, info.size, info.elem_type)
	} else {
		elem_type
	}

	mut fixed_size := i64(size)
	if fixed_size <= 0 {
		c.expr(mut size_expr)
		match mut size_expr {
			ast.IntegerLiteral {
				fixed_size = size_expr.val.int()
			}
			ast.CastExpr {
				if !size_expr.typ.is_pure_int() {
					c.error('only integer types are allowed', size_expr.pos)
				}
				match mut size_expr.expr {
					ast.IntegerLiteral {
						fixed_size = size_expr.expr.val.int()
					}
					ast.EnumVal {
						if val := c.table.find_enum_field_val(size_expr.expr.enum_name,
							size_expr.expr.val)
						{
							fixed_size = val
						}
					}
					else {}
				}
			}
			ast.EnumVal {
				c.error('${size_expr.enum_name}.${size_expr.val} has to be casted to integer to be used as size',
					size_expr.pos)
			}
			ast.Ident {
				if mut size_expr.obj is ast.ConstField {
					if mut size_expr.obj.expr is ast.EnumVal {
						c.error('${size_expr.obj.expr.enum_name}.${size_expr.obj.expr.val} has to be casted to integer to be used as size',
							size_expr.pos)
					}
					if mut size_expr.obj.expr is ast.CastExpr {
						if !size_expr.obj.expr.typ.is_pure_int() {
							c.error('only integer types are allowed', size_expr.pos)
						}
						if size_expr.obj.expr.expr is ast.IntegerLiteral {
							if comptime_value := c.eval_comptime_const_expr(size_expr.obj.expr.expr,
								0)
							{
								fixed_size = comptime_value.i64() or { fixed_size }
							}
						}
						if size_expr.obj.expr.expr is ast.InfixExpr {
							if comptime_value := c.eval_comptime_const_expr(size_expr.obj.expr.expr,
								0)
							{
								fixed_size = comptime_value.i64() or { fixed_size }
							}
						}
					}
					if comptime_value := c.eval_comptime_const_expr(size_expr.obj.expr,
						0)
					{
						fixed_size = comptime_value.i64() or { fixed_size }
					}
				} else {
					c.error('non-constant array bound `${size_expr.name}`', size_expr.pos)
				}
			}
			ast.InfixExpr {
				if comptime_value := c.eval_comptime_const_expr(size_expr, 0) {
					fixed_size = comptime_value.i64() or { fixed_size }
				}
			}
			else {
				c.error('fixed array size cannot use non-constant value', size_expr.pos())
			}
		}
		if fixed_size <= 0 {
			c.error('fixed size cannot be zero or negative (fixed_size: ${fixed_size})',
				size_expr.pos())
		}
	}

	idx := c.table.find_or_register_array_fixed(new_elem_typ, int(fixed_size), size_expr,
		false)
	return if elem_type.has_flag(.generic) {
		ast.new_type(idx).set_flag(.generic)
	} else {
		ast.new_type(idx)
	}
}

fn (mut c Checker) array_fixed_has_unresolved_size(info &ast.ArrayFixed) bool {
	if info.size <= 0 {
		return true
	}
	mut elem_type := info.elem_type
	mut elem_sym := c.table.sym(elem_type)
	for {
		if mut elem_sym.info is ast.ArrayFixed {
			if elem_sym.info.size <= 0 {
				return true
			}
			elem_type = elem_sym.info.elem_type
			elem_sym = c.table.sym(elem_type)
		} else {
			break
		}
	}
	return false
}

fn (mut c Checker) map_init(mut node ast.MapInit) ast.Type {
	// `map = {}`
	if node.keys.len == 0 && node.vals.len == 0 && node.typ == 0 {
		sym := c.table.sym(c.expected_type)
		if sym.kind == .map {
			info := sym.map_info()
			node.typ = c.expected_type.clear_option_and_result()
			node.key_type = info.key_type
			node.value_type = info.value_type
			return node.typ
		} else {
			if sym.kind == .struct_ {
				c.error('`{}` can not be used for initialising empty structs any more. Use `${c.table.type_to_str(c.expected_type)}{}` instead.',
					node.pos)
			} else {
				c.error('invalid empty map initialisation syntax, use e.g. map[string]int{} instead',
					node.pos)
			}
			return ast.void_type
		}
	}
	// `x := map[string]string` - set in parser
	if node.typ != 0 {
		info := c.table.sym(node.typ).map_info()
		if info.value_type != 0 {
			if info.value_type.has_flag(.result) {
				c.error('cannot use Result type as map value type', node.pos)
			}
			val_sym := c.table.sym(info.value_type)
			if val_sym.kind == .struct_ {
				val_info := val_sym.info as ast.Struct
				if val_info.generic_types.len > 0 && val_info.concrete_types.len == 0
					&& !info.value_type.has_flag(.generic) {
					if c.table.cur_concrete_types.len == 0 {
						c.error('generic struct `${val_sym.name}` must specify type parameter, e.g. ${val_sym.name}[int]',
							node.pos)
					} else {
						c.error('generic struct `${val_sym.name}` must specify type parameter, e.g. ${val_sym.name}[T]',
							node.pos)
					}
				}
			}
		}
		c.ensure_type_exists(info.key_type, node.pos)
		c.ensure_type_exists(info.value_type, node.pos)
		node.key_type = info.key_type
		node.value_type = info.value_type
		return node.typ
	}

	if node.keys.len > 0 && node.vals.len > 0 {
		mut key0_type := ast.void_type
		mut val0_type := ast.void_type
		use_expected_type := c.expected_type != ast.void_type && !c.inside_const
			&& c.table.sym(c.expected_type).kind == .map && !(c.inside_fn_arg
			&& c.expected_type.has_flag(.generic))
		if use_expected_type {
			sym := c.table.sym(c.expected_type)
			info := sym.map_info()
			key0_type = c.unwrap_generic(info.key_type)
			val0_type = c.unwrap_generic(info.value_type)
		} else {
			// `{'age': 20}`
			mut key_ := node.keys[0]
			key0_type = ast.mktyp(c.expr(mut key_))
			if node.keys[0].is_auto_deref_var() {
				key0_type = key0_type.deref()
			}
			mut val_ := node.vals[0]
			val0_type = ast.mktyp(c.expr(mut val_))
			if node.vals[0].is_auto_deref_var() {
				val0_type = val0_type.deref()
			}
			node.val_types << val0_type
		}
		key0_type = c.unwrap_generic(key0_type)
		val0_type = c.unwrap_generic(val0_type)
		map_type := ast.new_type(c.table.find_or_register_map(key0_type, val0_type))
		node.typ = map_type
		node.key_type = key0_type
		node.value_type = val0_type
		map_value_sym := c.table.sym(node.value_type)
		expecting_interface_map := map_value_sym.kind == .interface_
		//
		mut same_key_type := true

		if node.keys.len == 1 && val0_type == ast.none_type {
			c.error('map value cannot be only `none`', node.vals[0].pos())
		}

		for i, mut key in node.keys {
			if i == 0 && !use_expected_type {
				continue
			}
			mut val := node.vals[i]
			c.expected_type = key0_type
			key_type := c.expr(mut key)
			c.expected_type = val0_type
			val_type := c.expr(mut val)
			node.val_types << val_type
			val_type_sym := c.table.sym(val_type)
			if !c.check_types(key_type, key0_type) || (i == 0 && key_type.is_number()
				&& key0_type.is_number() && key0_type != ast.mktyp(key_type)) {
				msg := c.expected_msg(key_type, key0_type)
				c.error('invalid map key: ${msg}', key.pos())
				same_key_type = false
			}
			if expecting_interface_map {
				if val_type == node.value_type {
					continue
				}
				if val_type_sym.kind == .struct_
					&& c.type_implements(val_type, node.value_type, val.pos()) {
					node.vals[i] = ast.CastExpr{
						expr: val
						typname: c.table.get_type_name(node.value_type)
						typ: node.value_type
						expr_type: val_type
						pos: val.pos()
					}
					continue
				} else {
					msg := c.expected_msg(val_type, node.value_type)
					c.error('invalid map value: ${msg}', val.pos())
				}
			}
			if val_type == ast.none_type && val0_type.has_flag(.option) {
				continue
			}
			if !c.check_types(val_type, val0_type)
				|| val0_type.has_flag(.option) != val_type.has_flag(.option)
				|| (i == 0 && val_type.is_number() && val0_type.is_number()
				&& val0_type != ast.mktyp(val_type)) {
				msg := c.expected_msg(val_type, val0_type)
				c.error('invalid map value: ${msg}', val.pos())
			}
		}
		if same_key_type {
			for i in 1 .. node.keys.len {
				c.check_dup_keys(node, i)
			}
		}
		return map_type
	}
	return node.typ
}

// check the element, and its children for ref uninitialized fields
fn (mut c Checker) check_elements_ref_fields_initialized(typ ast.Type, pos &token.Pos) {
	if typ == 0 || c.inside_const {
		return
	}
	sym := c.table.sym(typ)
	mut checked_types := []ast.Type{}
	c.do_check_elements_ref_fields_initialized(sym, mut checked_types, pos)
}

// Recursively check the element, and its children for ref uninitialized fields
fn (mut c Checker) do_check_elements_ref_fields_initialized(sym &ast.TypeSymbol, mut checked_types []ast.Type, pos &token.Pos) {
	if sym.info is ast.Struct {
		linked_name := sym.name
		// For now, let's call this method and give a notice instead of an error.
		// After some time, we remove the check_ref_fields_initialized_note() method and
		// simply call check_ref_fields_initialized()
		c.check_ref_fields_initialized_note(sym, mut checked_types, linked_name, pos)
		return
	}
	match sym.info {
		ast.Array {
			elem_type := sym.info.elem_type
			if elem_type in checked_types {
				return
			}
			checked_types << elem_type
			elem_sym := c.table.sym(elem_type)
			c.do_check_elements_ref_fields_initialized(elem_sym, mut checked_types, pos)
		}
		ast.ArrayFixed {
			elem_type := sym.info.elem_type
			if elem_type in checked_types {
				return
			}
			checked_types << elem_type
			elem_sym := c.table.sym(elem_type)
			c.do_check_elements_ref_fields_initialized(elem_sym, mut checked_types, pos)
		}
		ast.Map {
			key_type := sym.info.key_type
			if key_type in checked_types {
				return
			}
			checked_types << key_type
			key_sym := c.table.sym(key_type)
			c.do_check_elements_ref_fields_initialized(key_sym, mut checked_types, pos)
			value_type := sym.info.value_type
			if value_type in checked_types {
				return
			}
			checked_types << value_type
			value_sym := c.table.sym(value_type)
			c.do_check_elements_ref_fields_initialized(value_sym, mut checked_types, pos)
		}
		ast.Alias {
			parent_type := sym.info.parent_type
			if parent_type in checked_types {
				return
			}
			checked_types << parent_type
			parent_sym := c.table.sym(parent_type)
			c.do_check_elements_ref_fields_initialized(parent_sym, mut checked_types,
				pos)
		}
		else {}
	}
}

const err_ref_uninitialized = error('arrays of references need to be initialized right away')
const err_interface_uninitialized = error('arrays of interfaces need to be initialized right away')
const err_sumtype_uninitialized = error('arrays of sumtypes need to be initialized right away')

// check the element, and its children for `ref/interface/sumtype` initialized
fn (mut c Checker) check_elements_initialized(typ ast.Type) ! {
	if typ == 0 || c.inside_unsafe {
		return
	}
	if typ.is_any_kind_of_pointer() {
		return checker.err_ref_uninitialized
	}
	sym := c.table.sym(typ)
	if sym.kind == .interface_ {
		return checker.err_interface_uninitialized
	} else if sym.kind == .sum_type {
		return checker.err_sumtype_uninitialized
	}

	match sym.info {
		ast.Array {
			elem_type := sym.info.elem_type
			return c.check_elements_initialized(elem_type)
		}
		ast.ArrayFixed {
			elem_type := sym.info.elem_type
			if !c.is_builtin_mod {
				return c.check_elements_initialized(elem_type)
			}
		}
		ast.Map {
			value_type := sym.info.value_type
			if !c.is_builtin_mod {
				return c.check_elements_initialized(value_type)
			}
		}
		ast.Alias {
			parent_type := sym.info.parent_type
			return c.check_elements_initialized(parent_type)
		}
		else {}
	}
}
