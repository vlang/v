// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token

pub fn (mut c Checker) array_init(mut node ast.ArrayInit) ast.Type {
	mut elem_type := ast.void_type
	// `x := []string{}` (the type was set in the parser)
	// TODO type is not set for fixed arrays
	if node.typ != ast.void_type {
		if node.elem_type != 0 {
			elem_sym := c.table.sym(node.elem_type)
			if elem_sym.kind == .struct_ {
				elem_info := elem_sym.info as ast.Struct
				if elem_info.generic_types.len > 0 && elem_info.concrete_types.len == 0
					&& !node.elem_type.has_flag(.generic) {
					if c.table.cur_concrete_types.len == 0 {
						c.error('generic struct must specify type parameter, e.g. Foo<int>',
							node.elem_type_pos)
					} else {
						c.error('generic struct must specify type parameter, e.g. Foo<T>',
							node.elem_type_pos)
					}
				}
			} else if elem_sym.kind == .interface_ {
				elem_info := elem_sym.info as ast.Interface
				if elem_info.generic_types.len > 0 && elem_info.concrete_types.len == 0
					&& !node.elem_type.has_flag(.generic) {
					if c.table.cur_concrete_types.len == 0 {
						c.error('generic interface must specify type parameter, e.g. Foo<int>',
							node.elem_type_pos)
					} else {
						c.error('generic interface must specify type parameter, e.g. Foo<T>',
							node.elem_type_pos)
					}
				}
			} else if elem_sym.kind == .sum_type {
				elem_info := elem_sym.info as ast.SumType
				if elem_info.generic_types.len > 0 && elem_info.concrete_types.len == 0
					&& !node.elem_type.has_flag(.generic) {
					if c.table.cur_concrete_types.len == 0 {
						c.error('generic sumtype must specify type parameter, e.g. Foo<int>',
							node.elem_type_pos)
					} else {
						c.error('generic sumtype must specify type parameter, e.g. Foo<T>',
							node.elem_type_pos)
					}
				}
			}
		}
		if node.exprs.len == 0 {
			if node.has_cap {
				c.check_array_init_para_type('cap', node.cap_expr, node.pos)
			}
			if node.has_len {
				c.check_array_init_para_type('len', node.len_expr, node.pos)
			}
		}
		if node.has_default {
			default_expr := node.default_expr
			default_typ := c.check_expr_opt_call(default_expr, c.expr(default_expr))
			node.default_type = default_typ
			c.check_expected(default_typ, node.elem_type) or {
				c.error(err.msg(), default_expr.pos())
			}
		}
		if node.has_len {
			if node.has_len && !node.has_default {
				elem_type_sym := c.table.sym(node.elem_type)
				if elem_type_sym.kind == .interface_ {
					c.error('cannot instantiate an array of interfaces without also giving a default `init:` value',
						node.len_expr.pos())
				}
			}
			c.ensure_sumtype_array_has_default_value(node)
		}
		c.ensure_type_exists(node.elem_type, node.elem_type_pos) or {}
		if node.typ.has_flag(.generic) && c.table.cur_fn != unsafe { nil }
			&& c.table.cur_fn.generic_names.len == 0 {
			c.error('generic struct cannot be used in non-generic function', node.pos)
		}

		// &int{} check
		if node.elem_type.is_any_kind_of_pointer() && !c.inside_unsafe && node.has_len {
			c.warn('arrays of references need to be initialized right away, therefore `len:` cannot be used (unless inside `unsafe`)',
				node.pos)
		}
		return node.typ
	}
	if node.is_fixed {
		c.ensure_sumtype_array_has_default_value(node)
		c.ensure_type_exists(node.elem_type, node.elem_type_pos) or {}
		if node.elem_type.is_any_kind_of_pointer() && !c.inside_unsafe && !c.is_builtin_mod {
			c.warn('fixed arrays of references need to be initialized right away (unless inside `unsafe`)',
				node.pos)
		}
	}
	// a = []
	if node.exprs.len == 0 {
		// a := fn_returing_opt_array() or { [] }
		if c.expected_type == ast.void_type && c.expected_or_type != ast.void_type {
			c.expected_type = c.expected_or_type
		}
		mut type_sym := c.table.sym(c.expected_type)
		if type_sym.kind != .array || type_sym.array_info().elem_type == ast.void_type {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				node.pos)
			return ast.void_type
		}
		// TODO: seperate errors once bug is fixed with `x := if expr { ... } else { ... }`
		// if c.expected_type == ast.void_type {
		// c.error('array_init: use `[]Type{}` instead of `[]`', node.pos)
		// return ast.void_type
		// }
		array_info := type_sym.array_info()
		node.elem_type = array_info.elem_type
		// clear optional flag incase of: `fn opt_arr() ?[]int { return [] }`
		return if c.expected_type.has_flag(.shared_f) {
			c.expected_type.clear_flag(.shared_f).deref()
		} else {
			c.expected_type
		}.clear_flag(.optional).clear_flag(.result)
	}
	// [1,2,3]
	if node.exprs.len > 0 && node.elem_type == ast.void_type {
		mut expected_value_type := ast.void_type
		mut expecting_interface_array := false
		mut expecting_sumtype_array := false
		if c.expected_type != 0 {
			expected_value_type = c.table.value_type(c.expected_type)
			expected_value_sym := c.table.sym(expected_value_type)
			if expected_value_sym.kind == .interface_ {
				// Array of interfaces? (`[dog, cat]`) Save the interface type (`Animal`)
				expecting_interface_array = true
			} else if expected_value_sym.kind == .sum_type {
				expecting_sumtype_array = true
			}
		}
		// expecting_interface_array := c.expected_type != 0 &&
		// c.table.sym(c.table.value_type(c.expected_type)).kind ==			.interface_
		//
		// if expecting_interface_array {
		// println('ex $c.expected_type')
		// }
		for i, mut expr in node.exprs {
			typ := c.check_expr_opt_call(expr, c.expr(expr))
			if typ == ast.void_type {
				c.error('invalid void array element type', expr.pos())
			}
			node.expr_types << typ
			// The first element's type
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
					c.type_implements(typ, elem_type, expr.pos())
				}
				if !typ.is_ptr() && !typ.is_pointer() && !c.inside_unsafe {
					typ_sym := c.table.sym(typ)
					if typ_sym.kind != .interface_ {
						c.mark_as_referenced(mut &expr, true)
					}
				}
				continue
			} else if expecting_sumtype_array {
				if i == 0 {
					if c.table.is_sumtype_or_in_variant(expected_value_type, typ) {
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
			// The first element's type
			if i == 0 {
				if expr.is_auto_deref_var() {
					elem_type = ast.mktyp(typ.deref())
				} else {
					elem_type = ast.mktyp(typ)
				}
				c.expected_type = elem_type
				continue
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
			}
		}
		if node.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, node.exprs.len, ast.empty_expr)
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
		// [50]u8
		mut fixed_size := i64(0)
		init_expr := node.exprs[0]
		c.expr(init_expr)
		match init_expr {
			ast.IntegerLiteral {
				fixed_size = init_expr.val.int()
			}
			ast.Ident {
				if init_expr.obj is ast.ConstField {
					if comptime_value := c.eval_comptime_const_expr(init_expr.obj.expr,
						0)
					{
						fixed_size = comptime_value.i64() or { fixed_size }
					}
				} else {
					c.error('non-constant array bound `${init_expr.name}`', init_expr.pos)
				}
			}
			ast.InfixExpr {
				if comptime_value := c.eval_comptime_const_expr(init_expr, 0) {
					fixed_size = comptime_value.i64() or { fixed_size }
				}
			}
			else {
				c.error('fixed array size cannot use non-constant value', init_expr.pos())
			}
		}
		if fixed_size <= 0 {
			c.error('fixed size cannot be zero or negative (fixed_size: ${fixed_size})',
				init_expr.pos())
		}
		idx := c.table.find_or_register_array_fixed(node.elem_type, int(fixed_size), init_expr)
		if node.elem_type.has_flag(.generic) {
			node.typ = ast.new_type(idx).set_flag(.generic)
		} else {
			node.typ = ast.new_type(idx)
		}
		if node.has_default {
			c.expr(node.default_expr)
		}
	}
	return node.typ
}

fn (mut c Checker) check_array_init_para_type(para string, expr ast.Expr, pos token.Pos) {
	sym := c.table.sym(c.unwrap_generic(c.expr(expr)))
	if sym.kind !in [.int, .int_literal] {
		c.error('array ${para} needs to be an int', pos)
	}
}

pub fn (mut c Checker) ensure_sumtype_array_has_default_value(node ast.ArrayInit) {
	sym := c.table.sym(node.elem_type)
	if sym.kind == .sum_type && !node.has_default {
		c.error('cannot initialize sum type array without default value', node.pos)
	}
}

pub fn (mut c Checker) map_init(mut node ast.MapInit) ast.Type {
	// `map = {}`
	if node.keys.len == 0 && node.vals.len == 0 && node.typ == 0 {
		sym := c.table.sym(c.expected_type)
		if sym.kind == .map {
			info := sym.map_info()
			node.typ = c.expected_type.clear_flag(.optional).clear_flag(.result)
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
			val_sym := c.table.sym(info.value_type)
			if val_sym.kind == .struct_ {
				val_info := val_sym.info as ast.Struct
				if val_info.generic_types.len > 0 && val_info.concrete_types.len == 0
					&& !info.value_type.has_flag(.generic) {
					if c.table.cur_concrete_types.len == 0 {
						c.error('generic struct `${val_sym.name}` must specify type parameter, e.g. Foo<int>',
							node.pos)
					} else {
						c.error('generic struct `${val_sym.name}` must specify type parameter, e.g. Foo<T>',
							node.pos)
					}
				}
			}
		}
		c.ensure_type_exists(info.key_type, node.pos) or {}
		c.ensure_type_exists(info.value_type, node.pos) or {}
		node.key_type = info.key_type
		node.value_type = info.value_type
		return node.typ
	}

	if node.keys.len > 0 && node.vals.len > 0 {
		mut key0_type := ast.void_type
		mut val0_type := ast.void_type
		use_expected_type := c.expected_type != ast.void_type && !c.inside_const
			&& c.table.sym(c.expected_type).kind == .map
		if use_expected_type {
			sym := c.table.sym(c.expected_type)
			info := sym.map_info()
			key0_type = c.unwrap_generic(info.key_type)
			val0_type = c.unwrap_generic(info.value_type)
		} else {
			// `{'age': 20}`
			key0_type = ast.mktyp(c.expr(node.keys[0]))
			if node.keys[0].is_auto_deref_var() {
				key0_type = key0_type.deref()
			}
			val0_type = ast.mktyp(c.expr(node.vals[0]))
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
		for i, key in node.keys {
			if i == 0 && !use_expected_type {
				continue
			}
			val := node.vals[i]
			c.expected_type = key0_type
			key_type := c.expr(key)
			c.expected_type = val0_type
			val_type := c.expr(val)
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
			if !c.check_types(val_type, val0_type) || (i == 0 && val_type.is_number()
				&& val0_type.is_number() && val0_type != ast.mktyp(val_type)) {
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
