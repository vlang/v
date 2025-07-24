// Copyright (c) 2019-2024 Felipe Pena. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast

@[inline]
fn (mut c Checker) markused_comptime_call(check bool, key string) {
	if check {
		c.table.used_features.comptime_calls[key] = true
	}
}

fn (mut c Checker) markused_assertstmt_auto_str(mut node ast.AssertStmt) {
	if !c.table.used_features.auto_str && !c.is_builtin_mod && mut node.expr is ast.InfixExpr {
		if !c.table.sym(c.unwrap_generic(node.expr.left_type)).has_method('str') {
			c.table.used_features.auto_str = true
			return
		}
		if !c.table.sym(c.unwrap_generic(node.expr.right_type)).has_method('str') {
			c.table.used_features.auto_str = true
		}
	}
}

fn (mut c Checker) markused_dumpexpr(mut node ast.DumpExpr) {
	if c.is_builtin_mod {
		return
	}
	unwrapped_type := c.unwrap_generic(node.expr_type)
	if node.expr_type.has_flag(.generic) {
		c.table.used_features.comptime_syms[unwrapped_type] = true
	}
	if !c.table.sym(unwrapped_type).has_method('str') {
		c.table.used_features.auto_str = true
		if node.expr_type.is_ptr() {
			c.table.used_features.auto_str_ptr = true
		}
	} else {
		c.table.used_features.print_types[node.expr_type.idx()] = true
	}
	c.table.used_features.print_types[ast.int_type_idx] = true
}

@[inline]
fn (mut c Checker) markused_used_maps(check bool) {
	if check {
		c.table.used_features.used_maps++
	}
}

fn (mut c Checker) markused_castexpr(mut node ast.CastExpr, to_type ast.Type, mut final_to_sym ast.TypeSymbol) {
	if c.is_builtin_mod {
		return
	}
	if c.table.used_features.used_maps == 0 && mut final_to_sym.info is ast.SumType {
		if final_to_sym.info.variants.any(c.table.final_sym(it).kind == .map) {
			c.table.used_features.used_maps++
		}
	}
	if c.mod !in ['strings', 'math.bits'] && to_type.is_ptr() {
		c.table.used_features.cast_ptr = true
	}
}

fn (mut c Checker) markused_external_type(check bool) {
	if check {
		c.table.used_features.external_types = true
	}
}

fn (mut c Checker) markused_comptimecall(mut node ast.ComptimeCall) {
	c.markused_comptime_call(true, '${int(c.unwrap_generic(c.comptime.comptime_for_method.receiver_type))}.${c.comptime.comptime_for_method.name}')
	if c.inside_anon_fn {
		// $method passed to anon fn, mark all methods as used
		sym := c.table.sym(c.unwrap_generic(node.left_type))
		for m in sym.get_methods() {
			c.table.used_features.comptime_calls['${int(c.unwrap_generic(m.receiver_type))}.${m.name}'] = true
			if node.args.len > 0 && m.params.len > 0 {
				last_param := m.params.last().typ
				if (last_param.is_int() || last_param.is_bool())
					&& c.table.final_sym(node.args.last().typ).kind == .array {
					c.table.used_features.comptime_calls['${ast.string_type_idx}.${c.table.type_to_str(m.params.last().typ)}'] = true
				}
			}
		}
	} else {
		m := c.comptime.comptime_for_method
		if node.args.len > 0 && m.params.len > 0 {
			last_param := m.params.last().typ
			if (last_param.is_int() || last_param.is_bool())
				&& c.table.final_sym(node.args.last().typ).kind == .array {
				c.table.used_features.comptime_calls['${ast.string_type_idx}.${c.table.type_to_str(m.params.last().typ)}'] = true
			}
		}
	}
}

fn (mut c Checker) markused_comptimefor(mut node ast.ComptimeFor, unwrapped_expr_type ast.Type) {
	c.table.used_features.dump = true
	if c.table.used_features.used_maps == 0 {
		final_sym := c.table.final_sym(unwrapped_expr_type)
		if final_sym.info is ast.Map {
			c.table.used_features.used_maps++
		} else if final_sym.info is ast.SumType {
			if final_sym.info.variants.any(c.table.final_sym(it).kind == .map) {
				c.table.used_features.used_maps++
			}
		}
	}
}

fn (mut c Checker) markused_call_expr(left_type ast.Type, mut node ast.CallExpr) {
	if !c.is_builtin_mod && c.mod == 'main' && !c.table.used_features.external_types {
		if node.is_method {
			if c.table.sym(node.left_type).is_builtin() {
				c.table.used_features.external_types = true
			}
		} else if node.name.contains('.') {
			c.table.used_features.external_types = true
		}
	}
	if left_type != 0 && left_type.is_ptr() && !c.table.used_features.auto_str_ptr
		&& node.name == 'str' {
		c.table.used_features.auto_str_ptr = true
	}
}

fn (mut c Checker) markused_fn_call(mut node ast.CallExpr) {
	if !c.is_builtin_mod && c.mod != 'math.bits' && node.args[0].expr !is ast.StringLiteral {
		if (node.args[0].expr is ast.CallExpr && node.args[0].expr.is_method
			&& node.args[0].expr.name == 'str')
			|| !c.table.sym(c.unwrap_generic(node.args[0].typ)).has_method('str') {
			c.table.used_features.auto_str = true
		} else {
			if node.args[0].typ.has_option_or_result() {
				c.table.used_features.print_options = true
			}
			c.table.used_features.print_types[node.args[0].typ.idx()] = true
			if !c.table.used_features.auto_str_ptr && node.args[0].expr is ast.Ident {
				var_obj := node.args[0].expr.obj
				if var_obj is ast.Var {
					if var_obj.orig_type != 0
						&& c.table.final_sym(var_obj.orig_type).kind == .interface {
						c.table.used_features.auto_str_ptr = true
						return
					}
				}
			}
		}
		if node.args[0].typ.is_ptr() {
			c.table.used_features.auto_str_ptr = true
		}
	}
}

fn (mut c Checker) markused_method_call(mut node ast.CallExpr, mut left_expr ast.Expr, left_type ast.Type) {
	if !left_type.has_flag(.generic) && mut left_expr is ast.Ident {
		if left_expr.obj is ast.Var && left_expr.obj.ct_type_var == .smartcast {
			c.table.used_features.comptime_calls['${int(left_type)}.${node.name}'] = true
		}
	} else if left_type.has_flag(.generic) {
		unwrapped_left := c.unwrap_generic(left_type)
		c.table.used_features.comptime_calls['${int(unwrapped_left)}.${node.name}'] = true
		if !unwrapped_left.is_ptr() && left_expr is ast.Ident && left_expr.is_mut() {
			c.table.used_features.comptime_calls['${int(unwrapped_left.ref())}.${node.name}'] = true
		}
	}
}

fn (mut c Checker) markused_string_inter_lit(mut node ast.StringInterLiteral, ftyp ast.Type) {
	if c.is_builtin_mod {
		return
	}
	if !c.table.sym(ftyp).has_method('str') {
		c.table.used_features.auto_str = true
	} else {
		c.table.used_features.print_types[ftyp.idx()] = true
	}
	if ftyp.is_ptr() {
		c.table.used_features.auto_str_ptr = true
	}
	if ftyp.has_option_or_result() {
		c.table.used_features.print_options = true
	}
}

fn (mut c Checker) markused_infixexpr(check bool) {
	if check {
		c.table.used_features.index = true
		c.table.used_features.arr_init = true
	}
}

fn (mut c Checker) markused_array_method(check bool, method_name string) {
	if !check {
		return
	}
	match method_name {
		'' { // array init
			c.table.used_features.arr_init = true
		}
		'first' {
			c.table.used_features.arr_first = true
		}
		'last' {
			c.table.used_features.arr_last = true
		}
		'pop' {
			c.table.used_features.arr_pop = true
		}
		'delete' {
			c.table.used_features.arr_delete = true
		}
		'map' {
			c.table.used_features.arr_map = true
		}
		else {}
	}
}
