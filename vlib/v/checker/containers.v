// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.table

pub fn (mut c Checker) array_init(mut array_init ast.ArrayInit) table.Type {
	// println('checker: array init $array_init.pos.line_nr $c.file.path')
	mut elem_type := table.void_type
	// []string - was set in parser
	if array_init.typ != table.void_type {
		if array_init.exprs.len == 0 {
			if array_init.has_cap {
				c.check_array_init_para_type('cap', array_init.cap_expr, array_init.pos)
			}
			if array_init.has_len {
				c.check_array_init_para_type('len', array_init.len_expr, array_init.pos)
			}
		}
		if array_init.has_default {
			default_typ := c.expr(array_init.default_expr)
			c.check_expected(default_typ, array_init.elem_type) or {
				c.error(err.msg, array_init.default_expr.position())
			}
		}
		if array_init.has_len {
			c.ensure_sumtype_array_has_default_value(array_init)
		}
		c.ensure_type_exists(array_init.elem_type, array_init.elem_type_pos) or {}
		return array_init.typ
	}
	if array_init.is_fixed {
		c.ensure_sumtype_array_has_default_value(array_init)
		c.ensure_type_exists(array_init.elem_type, array_init.elem_type_pos) or {}
	}
	// a = []
	if array_init.exprs.len == 0 {
		// a := fn_returing_opt_array() or { [] }
		if c.expected_type == table.void_type && c.expected_or_type != table.void_type {
			c.expected_type = c.expected_or_type
		}
		mut type_sym := c.table.get_type_symbol(c.expected_type)
		if type_sym.kind != .array {
			c.error('array_init: no type specified (maybe: `[]Type{}` instead of `[]`)',
				array_init.pos)
			return table.void_type
		}
		// TODO: seperate errors once bug is fixed with `x := if expr { ... } else { ... }`
		// if c.expected_type == table.void_type {
		// c.error('array_init: use `[]Type{}` instead of `[]`', array_init.pos)
		// return table.void_type
		// }
		array_info := type_sym.array_info()
		array_init.elem_type = array_info.elem_type
		// clear optional flag incase of: `fn opt_arr ?[]int { return [] }`
		return c.expected_type.clear_flag(.optional)
	}
	// [1,2,3]
	if array_init.exprs.len > 0 && array_init.elem_type == table.void_type {
		mut expected_value_type := table.void_type
		mut expecting_interface_array := false
		if c.expected_type != 0 {
			expected_value_type = c.table.value_type(c.expected_type)
			if c.table.get_type_symbol(expected_value_type).kind == .interface_ {
				// Array of interfaces? (`[dog, cat]`) Save the interface type (`Animal`)
				expecting_interface_array = true
			}
		}
		// expecting_interface_array := c.expected_type != 0 &&
		// c.table.get_type_symbol(c.table.value_type(c.expected_type)).kind ==			.interface_
		//
		// if expecting_interface_array {
		// println('ex $c.expected_type')
		// }
		for i, expr in array_init.exprs {
			typ := c.check_expr_opt_call(expr, c.expr(expr))
			array_init.expr_types << typ
			// The first element's type
			if expecting_interface_array {
				if i == 0 {
					elem_type = expected_value_type
					c.expected_type = elem_type
					c.type_implements(typ, elem_type, expr.position())
				}
				continue
			}
			// The first element's type
			if i == 0 {
				if expr.is_auto_deref_var() {
					elem_type = c.table.mktyp(typ.deref())
				} else {
					elem_type = c.table.mktyp(typ)
				}
				c.expected_type = elem_type
				continue
			}
			c.check_expected(typ, elem_type) or {
				c.error('invalid array element: $err.msg', expr.position())
			}
		}
		if array_init.is_fixed {
			idx := c.table.find_or_register_array_fixed(elem_type, array_init.exprs.len)
			array_init.typ = table.new_type(idx)
		} else {
			idx := c.table.find_or_register_array(elem_type)
			array_init.typ = table.new_type(idx)
		}
		array_init.elem_type = elem_type
	} else if array_init.is_fixed && array_init.exprs.len == 1
		&& array_init.elem_type != table.void_type {
		// [50]byte
		mut fixed_size := 0
		init_expr := array_init.exprs[0]
		c.expr(init_expr)
		match init_expr {
			ast.IntegerLiteral {
				fixed_size = init_expr.val.int()
			}
			ast.Ident {
				if init_expr.obj is ast.ConstField {
					if cint := eval_int_expr(init_expr.obj.expr, 0) {
						fixed_size = cint
					}
				} else {
					c.error('non-constant array bound `$init_expr.name`', init_expr.pos)
				}
			}
			ast.InfixExpr {
				if cint := eval_int_expr(init_expr, 0) {
					fixed_size = cint
				}
			}
			else {
				c.error('expecting `int` for fixed size', array_init.pos)
			}
		}
		if fixed_size <= 0 {
			c.error('fixed size cannot be zero or negative', init_expr.position())
		}
		idx := c.table.find_or_register_array_fixed(array_init.elem_type, fixed_size)
		array_type := table.new_type(idx)
		array_init.typ = array_type
		if array_init.has_default {
			c.expr(array_init.default_expr)
		}
	}
	return array_init.typ
}

pub fn (mut c Checker) map_init(mut node ast.MapInit) table.Type {
	// `map = {}`
	if node.keys.len == 0 && node.vals.len == 0 && node.typ == 0 {
		sym := c.table.get_type_symbol(c.expected_type)
		if sym.kind == .map {
			info := sym.map_info()
			node.typ = c.expected_type
			node.key_type = info.key_type
			node.value_type = info.value_type
			return node.typ
		} else {
			c.error('invalid empty map initilization syntax, use e.g. map[string]int{} instead',
				node.pos)
		}
	}
	// `x := map[string]string` - set in parser
	if node.typ != 0 {
		info := c.table.get_type_symbol(node.typ).map_info()
		c.ensure_type_exists(info.key_type, node.pos) or {}
		c.ensure_type_exists(info.value_type, node.pos) or {}
		node.key_type = info.key_type
		node.value_type = info.value_type
		return node.typ
	}
	if node.keys.len > 0 && node.vals.len > 0 {
		// `{'age': 20}`
		mut key0_type := c.table.mktyp(c.expr(node.keys[0]))
		if node.keys[0].is_auto_deref_var() {
			key0_type = key0_type.deref()
		}
		mut val0_type := c.table.mktyp(c.expr(node.vals[0]))
		if node.vals[0].is_auto_deref_var() {
			val0_type = val0_type.deref()
		}
		mut same_key_type := true
		for i, key in node.keys {
			if i == 0 {
				continue
			}
			val := node.vals[i]
			key_type := c.expr(key)
			c.expected_type = val0_type
			val_type := c.expr(val)
			if !c.check_types(key_type, key0_type) {
				msg := c.expected_msg(key_type, key0_type)
				c.error('invalid map key: $msg', key.position())
				same_key_type = false
			}
			if !c.check_types(val_type, val0_type) {
				msg := c.expected_msg(val_type, val0_type)
				c.error('invalid map value: $msg', val.position())
			}
		}
		if same_key_type {
			for i in 1 .. node.keys.len {
				c.check_dup_keys(node, i)
			}
		}
		map_type := table.new_type(c.table.find_or_register_map(key0_type, val0_type))
		node.typ = map_type
		node.key_type = key0_type
		node.value_type = val0_type
		return map_type
	}
	return node.typ
}
