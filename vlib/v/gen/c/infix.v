// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.token
import v.util

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	g.expected_fixed_arr = true
	defer {
		g.expected_fixed_arr = false
	}
	if node.auto_locked != '' {
		g.writeln('sync__RwMutex_lock(&${node.auto_locked}->mtx);')
	}
	match node.op {
		.arrow {
			g.infix_expr_arrow_op(node)
		}
		.eq, .ne {
			g.infix_expr_eq_op(node)
		}
		.gt, .ge, .lt, .le {
			g.infix_expr_cmp_op(node)
		}
		.key_in, .not_in {
			g.infix_expr_in_op(node)
		}
		.key_is, .not_is {
			g.infix_expr_is_op(node)
		}
		.plus, .minus, .mul, .div, .mod {
			g.infix_expr_arithmetic_op(node)
		}
		.left_shift {
			// `a << b` can mean many things in V ...
			// TODO: disambiguate everything in the checker; cgen should not decide all this.
			// Instead it should be as simple, as the branch for .right_shift is.
			// `array << val` should have its own separate operation internally.
			g.infix_expr_left_shift_op(node)
		}
		.right_shift {
			g.write('(')
			g.gen_plain_infix_expr(node)
			g.write(')')
		}
		.and, .logical_or {
			g.infix_expr_and_or_op(node)
		}
		else {
			// `x & y == 0` => `(x & y) == 0` in C
			need_par := node.op in [.amp, .pipe, .xor]
			if need_par {
				g.write('(')
			}
			g.gen_plain_infix_expr(node)
			if need_par {
				g.write(')')
			}
		}
	}
	if node.auto_locked != '' {
		g.writeln(';')
		g.write('sync__RwMutex_unlock(&${node.auto_locked}->mtx)')
	}
}

// infix_expr_arrow_op generates C code for pushing into channels (chan <- val)
fn (mut g Gen) infix_expr_arrow_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	styp := left.sym.cname
	elem_type := (left.sym.info as ast.Chan).elem_type
	gen_or := node.or_block.kind != .absent
	tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
	if gen_or {
		elem_styp := g.styp(elem_type)
		g.register_chan_push_option_fn(elem_styp, styp)
		g.write('${option_name}_void ${tmp_opt} = __Option_${styp}_pushval(')
	} else {
		g.write('__${styp}_pushval(')
	}
	g.expr(node.left)
	g.write(', ')
	if g.table.sym(elem_type).kind in [.sum_type, .interface] {
		g.expr_with_cast(node.right, node.right_type, elem_type)
	} else {
		g.expr(node.right)
	}
	g.write(')')
	if gen_or {
		g.or_block(tmp_opt, node.or_block, ast.void_type)
	}
}

// infix_expr_eq_op generates code for `==` and `!=`
fn (mut g Gen) infix_expr_eq_op(node ast.InfixExpr) {
	left_type := g.type_resolver.get_type_or_default(node.left, node.left_type)
	right_type := g.type_resolver.get_type_or_default(node.right, node.right_type)
	left := g.unwrap(left_type)
	right := g.unwrap(right_type)
	mut has_defined_eq_operator := false
	mut eq_operator_expects_ptr := false
	if m := g.table.find_method(left.sym, '==') {
		has_defined_eq_operator = true
		eq_operator_expects_ptr = m.receiver_type.is_ptr()
	}
	// TODO: investigate why the following is needed for vlib/v/tests/string_alias_test.v and vlib/v/tests/anon_fn_with_alias_args_test.v
	has_alias_eq_op_overload := left.sym.info is ast.Alias && left.sym.has_method('==')
	if g.pref.translated && !g.is_builtin_mod {
		g.gen_plain_infix_expr(node)
		return
	}
	left_is_option := left_type.has_flag(.option)
	right_is_option := right_type.has_flag(.option)
	is_none_check := left_is_option && node.right is ast.None
	if is_none_check {
		g.gen_is_none_check(node)
	} else if (left.typ.is_ptr() && right.typ.is_int())
		|| (right.typ.is_ptr() && left.typ.is_int())
		|| (left.typ.is_ptr() && right.typ == ast.nil_type) {
		g.gen_plain_infix_expr(node)
	} else if (left.typ.idx() == ast.string_type_idx || (!has_defined_eq_operator
		&& left.unaliased.idx() == ast.string_type_idx)) && node.right is ast.StringLiteral
		&& (node.right.val == '' || (node.left is ast.SelectorExpr
		|| (node.left is ast.Ident && node.left.or_expr.kind == .absent))) {
		if node.right.val == '' {
			// `str == ''` -> `str.len == 0` optimization
			g.write('(')
			g.expr(node.left)
			g.write(')')
			arrow := if left.typ.is_ptr() { '->' } else { '.' }
			g.write('${arrow}len ${node.op} 0')
		} else if node.left is ast.Ident {
			// vmemcmp(left, "str", sizeof("str")) optimization
			slit := cescape_nonascii(util.smart_quote(node.right.val, node.right.is_raw))
			var := g.expr_string(node.left)
			arrow := if left.typ.is_ptr() { '->' } else { '.' }
			if node.op == .eq {
				g.write('_SLIT_EQ(${var}${arrow}str, ${var}${arrow}len, "${slit}")')
			} else {
				g.write('_SLIT_NE(${var}${arrow}str, ${var}${arrow}len, "${slit}")')
			}
		} else {
			// fast_string_eq optimization for string selector comparison to literals
			if node.op == .ne {
				g.write('!fast_string_eq(')
			} else {
				g.write('fast_string_eq(')
			}
			g.expr(node.left)
			g.write(', ')
			g.expr(node.right)
			g.write(')')
		}
	} else if has_defined_eq_operator {
		if node.op == .ne {
			g.write('!')
		}
		if has_alias_eq_op_overload {
			g.write(g.styp(left.typ.set_nr_muls(0)))
		} else {
			g.write(g.styp(left.unaliased.set_nr_muls(0)))
		}
		g.write2('__eq(', '*'.repeat(left.typ.nr_muls()))
		if eq_operator_expects_ptr {
			g.write('&')
		}
		if node.left is ast.ArrayInit && g.table.sym(node.left_type).kind == .array_fixed {
			g.fixed_array_init_with_cast(node.left, node.left_type)
		} else {
			g.expr(node.left)
		}
		g.write2(', ', '*'.repeat(right.typ.nr_muls()))
		if eq_operator_expects_ptr {
			g.write('&')
		}
		if node.right is ast.ArrayInit && g.table.sym(node.right_type).kind == .array_fixed {
			g.fixed_array_init_with_cast(node.right, node.right_type)
		} else {
			g.expr(node.right)
		}
		g.write(')')
	} else if left.unaliased.idx() == right.unaliased.idx()
		&& left.sym.kind in [.array, .array_fixed, .alias, .map, .struct, .sum_type, .interface] {
		if g.pref.translated && !g.is_builtin_mod {
			g.gen_plain_infix_expr(node)
			return
		}
		kind := if left.sym.kind == .alias && right.sym.kind != .alias {
			left.unaliased_sym.kind
		} else {
			left.sym.kind
		}
		match kind {
			.alias {
				// optimize simple eq/ne operation on numbers
				if left.unaliased_sym.is_int() {
					if left.typ.is_ptr() && node.left.is_auto_deref_var() && !right.typ.is_pointer() {
						g.write('*'.repeat(left.typ.nr_muls()))
					}
					g.expr(node.left)
					g.write(' ${node.op} ')
					if right.typ.is_ptr() {
						g.write('*'.repeat(right.typ.nr_muls()))
					}
					g.expr(node.right)
					g.no_eq_method_types[left.typ] = true
				} else {
					ptr_typ := g.equality_fn(left.typ)
					if node.op == .ne {
						g.write('!')
					}
					g.write('${ptr_typ}_alias_eq(')
					if left.typ.is_ptr() {
						g.write('*'.repeat(left.typ.nr_muls()))
					}
					if node.left is ast.StructInit && left.unaliased_sym.is_primitive_fixed_array() {
						s := g.styp(left.unaliased)
						g.write('(${s})')
					}
					g.expr(node.left)
					g.write(', ')
					if node.right is ast.StructInit
						&& right.unaliased_sym.is_primitive_fixed_array() {
						s := g.styp(right.unaliased)
						g.write('(${s})')
					}
					if right.typ.is_ptr() {
						g.write('*'.repeat(right.typ.nr_muls()))
					}
					g.expr(node.right)
					g.write(')')
				}
			}
			.array {
				ptr_typ := g.equality_fn(left.unaliased.clear_flag(.shared_f))
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_arr_eq(')
				if left.typ.is_ptr() && !left.typ.has_flag(.shared_f) {
					if node.left !is ast.ArrayInit {
						g.write('*'.repeat(left.typ.nr_muls()))
					}
				}
				g.expr(node.left)
				if left.typ.has_flag(.shared_f) {
					g.write('->val')
				}
				g.write(', ')
				if right.typ.is_ptr() && !right.typ.has_flag(.shared_f) {
					if node.right !is ast.ArrayInit {
						g.write('*'.repeat(right.typ.nr_muls()))
					}
				}
				g.expr(node.right)
				if right.typ.has_flag(.shared_f) {
					g.write('->val')
				}
				g.write(')')
			}
			.array_fixed {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_arr_eq(')
				if left.typ.is_ptr() {
					g.write('*')
				}
				if node.left is ast.ArrayInit {
					if !node.left.has_index {
						s := g.styp(left.unaliased)
						g.write('(${s})')
					}
				} else if node.left is ast.StructInit
					&& left.unaliased_sym.is_primitive_fixed_array() {
					s := g.styp(left.unaliased)
					g.write('(${s})')
				}
				g.expr(node.left)
				g.write(', ')
				if node.right is ast.ArrayInit {
					if !node.right.has_index {
						s := g.styp(right.unaliased)
						g.write('(${s})')
					}
				} else if node.right is ast.StructInit
					&& right.unaliased_sym.is_primitive_fixed_array() {
					s := g.styp(right.unaliased)
					g.write('(${s})')
				}
				g.expr(node.right)
				g.write(')')
			}
			.map {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_map_eq(')
				if left.typ.is_ptr() {
					g.write('*'.repeat(left.typ.nr_muls()))
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*'.repeat(right.typ.nr_muls()))
				}
				g.expr(node.right)
				g.write(')')
			}
			.struct {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_struct_eq(')
				if left.typ.is_ptr() {
					g.write('*'.repeat(left.typ.nr_muls()))
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*'.repeat(right.typ.nr_muls()))
				}
				g.expr(node.right)
				g.write(')')
			}
			.sum_type {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_sumtype_eq(')
				if left.typ.is_ptr() {
					g.write('*'.repeat(left.typ.nr_muls()))
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*'.repeat(right.typ.nr_muls()))
				}
				g.expr(node.right)
				g.write(')')
			}
			.interface {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_interface_eq(')
				if left.typ.is_ptr() {
					g.write('*'.repeat(left.typ.nr_muls()))
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*'.repeat(right.typ.nr_muls()))
				}
				g.expr(node.right)
				g.write(')')
			}
			else {
				g.gen_plain_infix_expr(node)
			}
		}
	} else if left.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& right.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op:            node.op
			unsigned_type: left.unaliased
			unsigned_expr: node.left
			signed_type:   right.unaliased
			signed_expr:   node.right
		)
	} else if right.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& left.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op:            node.op
			reverse:       true
			unsigned_type: right.unaliased
			unsigned_expr: node.right
			signed_type:   left.unaliased
			signed_expr:   node.left
		)
	} else if left_is_option && right_is_option {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		if node.op == .eq {
			g.write('!')
		}
		g.write('memcmp(')
		g.expr(node.left)
		g.write('.data, ')
		g.expr(node.right)
		g.write('.data, sizeof(${g.base_type(left_type)}))')
		g.inside_opt_or_res = old_inside_opt_or_res
	} else {
		g.gen_plain_infix_expr(node)
	}
}

// infix_expr_cmp_op generates code for `<`, `<=`, `>`, `>=`
// It handles operator overloading when necessary
fn (mut g Gen) infix_expr_cmp_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)

	mut has_operator_overloading := false
	mut operator_expects_ptr := false
	if m := g.table.find_method(left.sym, '<') {
		has_operator_overloading = true
		operator_expects_ptr = m.receiver_type.is_ptr()
	}

	if g.pref.translated && !g.is_builtin_mod {
		g.gen_plain_infix_expr(node)
		return
	}
	if left.sym.kind == .struct && (left.sym.info as ast.Struct).generic_types.len > 0 {
		if node.op in [.le, .ge] {
			g.write('!')
		}
		concrete_types := (left.sym.info as ast.Struct).concrete_types
		mut method_name := left.sym.cname + '__lt'
		method_name = g.generic_fn_name(concrete_types, method_name)
		g.write(method_name)
		if node.op in [.lt, .ge] {
			g.write2('(', '*'.repeat(left.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			g.expr(node.left)
			g.write2(', ', '*'.repeat(right.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			g.expr(node.right)
			g.write(')')
		} else {
			g.write2('(', '*'.repeat(right.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			g.expr(node.right)
			g.write2(', ', '*'.repeat(left.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			g.expr(node.left)
			g.write(')')
		}
	} else if left.unaliased_sym.kind == right.unaliased_sym.kind && has_operator_overloading {
		if node.op in [.le, .ge] {
			g.write('!')
		}
		g.write2(g.styp(left.typ.set_nr_muls(0)), '__lt')
		if node.op in [.lt, .ge] {
			g.write2('(', '*'.repeat(left.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			if node.left is ast.ArrayInit && g.table.sym(node.left_type).kind == .array_fixed {
				g.fixed_array_init_with_cast(node.left, node.left_type)
			} else {
				g.expr(node.left)
			}
			g.write2(', ', '*'.repeat(right.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			if node.right is ast.ArrayInit && g.table.sym(node.right_type).kind == .array_fixed {
				g.fixed_array_init_with_cast(node.right, node.right_type)
			} else {
				g.expr(node.right)
			}
			g.write(')')
		} else {
			g.write2('(', '*'.repeat(right.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			g.expr(node.right)
			g.write2(', ', '*'.repeat(left.typ.nr_muls()))
			if operator_expects_ptr {
				g.write('&')
			}
			g.expr(node.left)
			g.write(')')
		}
	} else if left.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& right.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op:            node.op
			unsigned_type: left.unaliased
			unsigned_expr: node.left
			signed_type:   right.unaliased
			signed_expr:   node.right
		)
	} else if right.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& left.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op:            node.op
			reverse:       true
			unsigned_type: right.unaliased
			unsigned_expr: node.right
			signed_type:   left.unaliased
			signed_expr:   node.left
		)
	} else {
		g.gen_plain_infix_expr(node)
	}
}

fn (mut g Gen) infix_expr_in_sumtype_interface_array(infix_exprs []ast.InfixExpr) {
	for i in 0 .. infix_exprs.len {
		g.infix_expr_is_op(infix_exprs[i])
		if i != infix_exprs.len - 1 {
			g.write(' || ')
		}
	}
}

// infix_expr_in_op generates code for `in` and `!in`
fn (mut g Gen) infix_expr_in_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	if node.op == .not_in {
		g.write('!')
	}
	if right.unaliased_sym.kind == .array {
		if left.sym.kind in [.sum_type, .interface] {
			if node.right is ast.ArrayInit {
				if node.right.exprs.len > 0
					&& g.table.sym(node.right.expr_types[0]).kind !in [.sum_type, .interface] {
					mut infix_exprs := []ast.InfixExpr{}
					for i in 0 .. node.right.exprs.len {
						infix_exprs << ast.InfixExpr{
							op:         .key_is
							left:       node.left
							left_type:  node.left_type
							right:      node.right.exprs[i]
							right_type: node.right.expr_types[i]
						}
					}
					g.write('(')
					g.infix_expr_in_sumtype_interface_array(infix_exprs)
					g.write(')')
					return
				}
			}
		}
		if node.right is ast.ArrayInit {
			elem_type := node.right.elem_type
			elem_sym := g.table.sym(elem_type)
			// TODO: replace ast.Ident check with proper side effect analysis
			if node.right.exprs.len > 0 && (node.left is ast.Ident
				|| node.left is ast.IndexExpr || node.left is ast.SelectorExpr) {
				// `a in [1,2,3]` optimization => `a == 1 || a == 2 || a == 3`
				// avoids an allocation
				g.write('(')
				if elem_sym.kind == .sum_type && left.sym.kind != .sum_type {
					if node.left_type in elem_sym.sumtype_info().variants {
						new_node_left := ast.CastExpr{
							arg:       ast.empty_expr
							typ:       elem_type
							expr:      node.left
							expr_type: node.left_type
						}
						g.infix_expr_in_optimization(new_node_left, node.left_type, node.right)
					}
				} else {
					g.infix_expr_in_optimization(node.left, node.left_type, node.right)
				}
				g.write(')')
				return
			}
		}
		if right.sym.info is ast.Array {
			elem_type := right.sym.info.elem_type
			elem_type_ := g.unwrap(elem_type)
			if elem_type_.sym.kind == .sum_type {
				if ast.mktyp(node.left_type) in elem_type_.sym.sumtype_info().variants {
					new_node_left := ast.CastExpr{
						arg:       ast.empty_expr
						typ:       elem_type
						expr:      node.left
						expr_type: ast.mktyp(node.left_type)
					}
					g.write('(')
					g.gen_array_contains(node.right_type, node.right, elem_type, new_node_left)
					g.write(')')
					return
				}
			} else if elem_type_.sym.kind == .interface {
				new_node_left := ast.CastExpr{
					arg:       ast.empty_expr
					typ:       elem_type
					expr:      node.left
					expr_type: ast.mktyp(node.left_type)
				}
				g.write('(')
				g.gen_array_contains(node.right_type, node.right, elem_type, new_node_left)
				g.write(')')
				return
			}
		}
		g.write('(')
		g.gen_array_contains(node.right_type, node.right, node.left_type, node.left)
		g.write(')')
	} else if right.unaliased_sym.kind == .map {
		g.write('_IN_MAP(')
		if !left.typ.is_ptr() {
			styp := g.styp(node.left_type)
			g.write('ADDR(${styp}, ')
			g.expr(node.left)
			g.write(')')
		} else {
			g.expr(node.left)
		}
		g.write(', ')
		if !right.typ.is_ptr() || right.typ.has_flag(.shared_f) {
			g.write('ADDR(map, ')
			g.expr(node.right)
			if right.typ.has_flag(.shared_f) {
				g.write('->val')
			}
			g.write(')')
		} else {
			g.expr(node.right)
		}
		g.write(')')
	} else if right.unaliased_sym.kind == .array_fixed {
		if left.sym.kind in [.sum_type, .interface] {
			if node.right is ast.ArrayInit {
				if node.right.exprs.len > 0 {
					mut infix_exprs := []ast.InfixExpr{}
					for i in 0 .. node.right.exprs.len {
						infix_exprs << ast.InfixExpr{
							op:         .key_is
							left:       node.left
							left_type:  node.left_type
							right:      node.right.exprs[i]
							right_type: node.right.expr_types[i]
						}
					}
					g.write('(')
					g.infix_expr_in_sumtype_interface_array(infix_exprs)
					g.write(')')
					return
				}
			}
		}
		if node.right is ast.ArrayInit {
			if node.right.exprs.len > 0 {
				// `a in [1,2,3]!` optimization => `a == 1 || a == 2 || a == 3`
				// avoids an allocation
				g.write('(')
				g.infix_expr_in_optimization(node.left, node.left_type, node.right)
				g.write(')')
				return
			}
		}
		if right.sym.info is ast.ArrayFixed {
			elem_type := right.sym.info.elem_type
			elem_type_ := g.unwrap(elem_type)
			if elem_type_.sym.kind == .sum_type {
				if ast.mktyp(node.left_type) in elem_type_.sym.sumtype_info().variants {
					new_node_left := ast.CastExpr{
						arg:       ast.empty_expr
						typ:       elem_type
						expr:      node.left
						expr_type: ast.mktyp(node.left_type)
					}
					g.write('(')
					g.gen_array_contains(node.right_type, node.right, elem_type, new_node_left)
					g.write(')')
					return
				}
			}
		}
		g.write('(')
		g.gen_array_contains(node.right_type, node.right, node.left_type, node.left)
		g.write(')')
	} else if right.unaliased_sym.kind == .string && node.right !is ast.RangeExpr {
		g.write2('(', 'string_contains(')
		g.expr(node.right)
		g.write(', ')
		g.expr(node.left)
		g.write('))')
	} else if node.right is ast.RangeExpr {
		// call() in min..max
		if node.left is ast.CallExpr {
			line := g.go_before_last_stmt().trim_space()
			g.empty_line = true
			tmp_var := g.new_tmp_var()
			g.write('${g.styp(node.left.return_type)} ${tmp_var} = ')
			g.expr(node.left)
			g.writeln(';')
			g.write(line)
			g.write('(')
			g.write('${tmp_var} >= ')
			g.expr(node.right.low)
			g.write(' && ')
			g.write('${tmp_var} < ')
			g.expr(node.right.high)
			g.write(')')
		} else {
			g.write('(')
			g.expr(node.left)
			g.write(' >= ')
			g.expr(node.right.low)
			g.write(' && ')
			g.expr(node.left)
			g.write(' < ')
			g.expr(node.right.high)
			g.write(')')
		}
	}
}

// infix_expr_in_optimization optimizes `<var> in <array>` expressions,
// and transform them in a series of equality comparison
// i.e. `a in [1,2,3]` => `a == 1 || a == 2 || a == 3`
fn (mut g Gen) infix_expr_in_optimization(left ast.Expr, left_type ast.Type, right ast.ArrayInit) {
	tmp_var := if left is ast.CallExpr { g.new_tmp_var() } else { '' }
	mut elem_sym := g.table.sym(right.elem_type)
	left_parent_idx := g.table.sym(left_type).parent_idx
	for i, array_expr in right.exprs {
		match elem_sym.kind {
			.string, .alias, .sum_type, .map, .interface, .array, .struct {
				if elem_sym.kind == .string {
					is_auto_deref_var := left.is_auto_deref_var()
					if left is ast.Ident && left.or_expr.kind == .absent
						&& array_expr is ast.StringLiteral {
						var := g.expr_string(left)
						slit := cescape_nonascii(util.smart_quote(array_expr.val, array_expr.is_raw))
						if is_auto_deref_var || (left.info is ast.IdentVar
							&& g.table.sym(left.obj.typ).kind in [.interface, .sum_type]) {
							g.write('_SLIT_EQ(${var}->str, ${var}->len, "${slit}")')
						} else {
							g.write('_SLIT_EQ(${var}.str, ${var}.len, "${slit}")')
						}
						if i < right.exprs.len - 1 {
							g.write(' || ')
						}
						continue
					} else if array_expr is ast.StringLiteral {
						g.write('fast_string_eq(')
					} else {
						g.write('string__eq(')
					}
					if is_auto_deref_var || (left is ast.Ident && left.info is ast.IdentVar
						&& g.table.sym(left.obj.typ).kind in [.interface, .sum_type]) {
						g.write('*')
					}
				} else {
					ptr_typ := g.equality_fn(right.elem_type)
					if elem_sym.kind == .alias {
						// optimization for alias to number
						if elem_sym.is_int() {
							g.expr(left)
							g.write(' == ')
							if left_parent_idx != 0 && !((array_expr is ast.SelectorExpr
								&& array_expr.typ == left_type)
								|| (array_expr is ast.Ident && array_expr.obj.typ == left_type)) {
								g.write('(${g.styp(left_parent_idx)})')
							}
							g.expr(array_expr)
							if i < right.exprs.len - 1 {
								g.write(' || ')
							}
							continue
						} else {
							g.write('${ptr_typ}_alias_eq(')
						}
					} else if elem_sym.kind == .sum_type {
						g.write('${ptr_typ}_sumtype_eq(')
					} else if elem_sym.kind == .map {
						g.write('${ptr_typ}_map_eq(')
					} else if elem_sym.kind == .interface {
						g.write('${ptr_typ}_interface_eq(')
					} else if elem_sym.kind == .array {
						g.write('${ptr_typ}_arr_eq(')
					} else if elem_sym.kind == .struct {
						g.write('${ptr_typ}_struct_eq(')
					}
				}
				if left is ast.CallExpr {
					if i == 0 {
						line := g.go_before_last_stmt().trim_space()
						g.empty_line = true
						g.write('${g.styp(left.return_type)} ${tmp_var} = ')
						g.expr(left)
						g.writeln(';')
						g.write2(line, tmp_var)
					} else {
						g.write(tmp_var)
					}
				} else {
					g.expr(left)
				}
				g.write(', ')
				g.expr(array_expr)
				g.write(')')
			}
			else { // works in function kind
				if left is ast.CallExpr {
					if i == 0 {
						line := g.go_before_last_stmt().trim_space()
						g.empty_line = true
						g.write('${g.styp(left.return_type)} ${tmp_var} = ')
						g.expr(left)
						g.writeln(';')
						g.write2(line, tmp_var)
					} else {
						g.write(tmp_var)
					}
				} else {
					g.expr(left)
				}
				g.write(' == ')
				if elem_sym.kind == .array_fixed {
					g.write('(${g.styp(right.elem_type)})')
				}
				g.expr(array_expr)
			}
		}
		if i < right.exprs.len - 1 {
			g.write(' || ')
		}
	}
}

// infix_expr_is_op generates code for `is` and `!is`
fn (mut g Gen) infix_expr_is_op(node ast.InfixExpr) {
	mut left_sym := g.table.sym(g.unwrap_generic(g.type_resolver.get_type_or_default(node.left,
		node.left_type)))
	is_aggregate := node.left is ast.Ident && g.comptime.get_ct_type_var(node.left) == .aggregate
	right_sym := g.table.sym(node.right_type)
	if left_sym.kind == .interface && right_sym.kind == .interface {
		g.gen_interface_is_op(node)
		return
	}

	cmp_op := if node.op == .key_is { '==' } else { '!=' }
	g.write('(')
	if node.left_type.nr_muls() > 1 {
		g.write('*'.repeat(node.left_type.nr_muls() - 1))
	}
	if is_aggregate {
		g.write('${node.left}')
	} else {
		g.expr(node.left)
	}
	g.write(')')
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	if left_sym.kind == .interface {
		g.write('_typ ${cmp_op} ')
		// `_Animal_Dog_index`
		sub_type := match node.right {
			ast.TypeNode {
				g.unwrap_generic(node.right.typ)
			}
			ast.None {
				ast.idx_to_type(g.table.type_idxs['None__'])
			}
			else {
				ast.no_type
			}
		}
		sub_sym := g.table.sym(sub_type)
		g.write('_${left_sym.cname}_${sub_sym.cname}_index')
		return
	} else if left_sym.kind == .sum_type || is_aggregate {
		g.write('_typ ${cmp_op} ')
	}
	if node.right is ast.None {
		g.write('${ast.none_type.idx()}')
	} else if node.right is ast.Ident && node.right.name == g.comptime.comptime_for_variant_var {
		variant_idx := g.type_resolver.get_ct_type_or_default('${g.comptime.comptime_for_variant_var}.typ',
			ast.void_type)
		g.write('${int(variant_idx)}')
	} else {
		g.expr(node.right)
	}
}

fn (mut g Gen) gen_interface_is_op(node ast.InfixExpr) {
	mut left_sym := g.table.sym(node.left_type)
	right_sym := g.table.sym(node.right_type)

	mut info := left_sym.info as ast.Interface
	lock info.conversions {
		common_variants := info.conversions[node.right_type] or {
			left_variants := g.table.iface_types[left_sym.name]
			right_variants := g.table.iface_types[right_sym.name]
			c := left_variants.filter(it in right_variants)
			info.conversions[node.right_type] = c
			c
		}
		left_sym.info = info
		if common_variants.len == 0 {
			g.write('false')
			return
		}
	}
	g.write('I_${left_sym.cname}_is_I_${right_sym.cname}(')
	if node.left_type.is_ptr() {
		g.write('*')
	}
	g.expr(node.left)
	g.write(')')
}

// infix_expr_arithmetic_op generates code for `+`, `-`, `*`, `/`, and `%`
// It handles operator overloading when necessary
fn (mut g Gen) infix_expr_arithmetic_op(node ast.InfixExpr) {
	left := g.unwrap(g.type_resolver.get_type_or_default(node.left, node.left_type))
	right := g.unwrap(g.type_resolver.get_type_or_default(node.right, node.right_type))
	if left.sym.info is ast.Struct && left.sym.info.generic_types.len > 0 {
		mut method_name := left.sym.cname + '_' + util.replace_op(node.op.str())
		method_name = g.generic_fn_name(left.sym.info.concrete_types, method_name)
		g.write2(method_name, '(')
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write(')')
	} else {
		mut method := ast.Fn{}
		mut method_name := ''
		if left.sym.has_method(node.op.str()) {
			method = left.sym.find_method(node.op.str()) or { ast.Fn{} }
			method_name = left.sym.cname + '_' + util.replace_op(node.op.str())
		} else if left.unaliased_sym.has_method_with_generic_parent(node.op.str()) {
			method = left.unaliased_sym.find_method_with_generic_parent(node.op.str()) or {
				ast.Fn{}
			}
			method_name = left.unaliased_sym.cname + '_' + util.replace_op(node.op.str())
			if left.unaliased_sym.info is ast.Struct
				&& left.unaliased_sym.info.generic_types.len > 0 {
				method_name = g.generic_fn_name(left.unaliased_sym.info.concrete_types,
					method_name)
			}
		} else {
			g.gen_plain_infix_expr(node)
			return
		}

		mut right_var := ''
		if node.right is ast.Ident && node.right.or_expr.kind != .absent {
			cur_line := g.go_before_last_stmt().trim_space()
			right_var = g.new_tmp_var()
			g.write('${g.styp(right.typ)} ${right_var} = ')
			g.op_arg(node.right, method.params[1].typ, right.typ)
			g.writeln(';')
			g.write(cur_line)
		}
		g.write2(method_name, '(')
		g.op_arg(node.left, method.params[0].typ, left.typ)
		if right_var != '' {
			g.write(', ${right_var}')
		} else {
			g.write(', ')
			g.op_arg(node.right, method.params[1].typ, right.typ)
		}
		g.write(')')

		if left.typ != 0 && !left.typ.has_option_or_result()
			&& g.table.final_sym(left.typ).kind == .array_fixed {
			// it's non-option fixed array, requires accessing .ret_arr member to get the array
			g.write('.ret_arr')
		}
	}
}

// infix_expr_left_shift_op generates code for the `<<` operator
// This can either be a value pushed into an array or a bit shift
fn (mut g Gen) infix_expr_left_shift_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	if left.unaliased_sym.kind == .array {
		// arr << val
		tmp_var := g.new_tmp_var()
		array_info := left.unaliased_sym.info as ast.Array
		noscan := g.check_noscan(array_info.elem_type)
		elem_is_option := array_info.elem_type.has_flag(.option)
		if (right.unaliased_sym.kind == .array
			|| (right.unaliased_sym.kind == .struct && right.unaliased_sym.name == 'array'))
			&& left.sym.nr_dims() == right.sym.nr_dims() && array_info.elem_type != right.typ
			&& !elem_is_option && !(right.sym.kind == .alias
			&& g.table.sumtype_has_variant(array_info.elem_type, node.right_type, false)) {
			// push an array => PUSH_MANY, but not if pushing an array to 2d array (`[][]int << []int`)
			g.write('_PUSH_MANY${noscan}(')
			mut expected_push_many_atype := left.typ
			is_shared := expected_push_many_atype.has_flag(.shared_f)
			if !expected_push_many_atype.is_ptr() {
				// fn f(mut a []int) { a << [1,2,3] } -> type of `a` is `array_int*` -> no need for &
				g.write('&')
			} else {
				expected_push_many_atype = expected_push_many_atype.deref()
			}
			if is_shared {
				g.write('&')
			}
			if is_shared {
				expected_push_many_atype = expected_push_many_atype.clear_flag(.shared_f)
			}
			g.expr(node.left)
			if node.left_type.has_flag(.shared_f) {
				g.write('->val')
			}
			if left.typ.is_ptr() && right.typ.is_ptr() {
				g.write(', *(')
			} else {
				g.write(', (')
			}
			g.expr_with_cast(node.right, right.typ, left.unaliased.clear_flag(.shared_f))
			styp := g.styp(expected_push_many_atype)
			g.write('), ${tmp_var}, ${styp})')
		} else {
			// push a single element
			elem_type_str := g.styp(array_info.elem_type)
			elem_sym := g.table.final_sym(array_info.elem_type)
			elem_is_array_var := !elem_is_option && elem_sym.kind in [.array, .array_fixed]
				&& node.right is ast.Ident
			g.write('array_push${noscan}((array*)')
			mut needs_addr := false
			if !left.typ.is_ptr()
				|| (node.left_type.has_flag(.shared_f) && !node.left_type.deref().is_ptr()) {
				if node.left is ast.CallExpr {
					g.write('ADDR(${g.styp(node.left_type)}, ')
					needs_addr = true
				} else {
					g.write('&')
				}
			}
			g.expr(node.left)
			if node.left_type.has_flag(.shared_f) {
				g.write('->val')
			}
			if needs_addr {
				g.write(')')
			}
			if elem_sym.kind == .function {
				g.write(', _MOV((voidptr[]){ ')
			} else if elem_is_array_var {
				addr := if elem_sym.kind == .array_fixed { '' } else { '&' }
				g.write(', ${addr}')
			} else {
				g.write(', _MOV((${elem_type_str}[]){ ')
			}
			if array_info.elem_type.has_flag(.option) {
				g.expr_with_opt(node.right, right.typ, array_info.elem_type)
			} else {
				// if g.autofree
				needs_clone := !g.is_builtin_mod
					&& array_info.elem_type.idx() == ast.string_type_idx
					&& array_info.elem_type.nr_muls() == 0
					&& node.right !in [ast.StringLiteral, ast.StringInterLiteral, ast.CallExpr, ast.IndexExpr, ast.InfixExpr]
				if needs_clone {
					g.write('string_clone(')
				}
				if node.right is ast.CastExpr && node.right.expr is ast.ArrayInit {
					g.expr(node.right.expr)
				} else if elem_sym.info is ast.ArrayFixed
					&& node.right in [ast.CallExpr, ast.DumpExpr] {
					tmpvar := g.expr_with_var(node.right, array_info.elem_type, false)
					g.fixed_array_var_init(tmpvar, false, elem_sym.info.elem_type, elem_sym.info.size)
				} else {
					g.expr_with_cast(node.right, right.typ, array_info.elem_type)
				}
				if needs_clone {
					g.write(')')
				}
			}
			if elem_is_array_var {
				g.write(')')
			} else {
				g.write(' }))')
			}
		}
	} else {
		g.write('(')
		g.gen_plain_infix_expr(node)
		g.write(')')
	}
}

fn (mut g Gen) need_tmp_var_in_array_call(node ast.Expr) bool {
	match node {
		ast.CallExpr {
			if node.left_type != 0 && g.table.sym(node.left_type).kind == .array
				&& node.name in ['all', 'any', 'filter', 'map', 'count'] {
				return true
			}
		}
		ast.IndexExpr {
			return g.need_tmp_var_in_array_call(node.left)
		}
		ast.InfixExpr {
			return g.need_tmp_var_in_array_call(node.left)
				|| g.need_tmp_var_in_array_call(node.right)
		}
		ast.ParExpr {
			return g.need_tmp_var_in_array_call(node.expr)
		}
		ast.PostfixExpr {
			return g.need_tmp_var_in_array_call(node.expr)
		}
		ast.PrefixExpr {
			return g.need_tmp_var_in_array_call(node.right)
		}
		ast.RangeExpr {
			return g.need_tmp_var_in_array_call(node.low) || g.need_tmp_var_in_array_call(node.high)
		}
		ast.SelectorExpr {
			return g.need_tmp_var_in_array_call(node.expr)
		}
		else {}
	}
	return false
}

// infix_expr_and_or_op generates code for `&&` and `||`
fn (mut g Gen) infix_expr_and_or_op(node ast.InfixExpr) {
	if g.need_tmp_var_in_array_call(node.right) {
		// `if a == 0 || arr.any(it.is_letter()) {...}`
		tmp := g.new_tmp_var()
		cur_line := g.go_before_last_stmt().trim_space()
		g.empty_line = true
		if g.infix_left_var_name.len > 0 {
			g.write('bool ${tmp} = ((${g.infix_left_var_name}) ${node.op.str()} ')
		} else {
			g.write('bool ${tmp} = (')
		}
		g.expr(node.left)
		g.writeln(');')
		g.set_current_pos_as_last_stmt_pos()
		g.write('${cur_line} ${tmp} ${node.op.str()} ')
		g.infix_left_var_name = if node.op == .and { tmp } else { '!${tmp}' }
		g.expr(node.right)
		g.infix_left_var_name = ''
	} else if g.need_tmp_var_in_expr(node.right) && g.inside_ternary == 0 {
		prev_inside_ternary := g.inside_ternary
		g.inside_ternary = 0
		tmp := g.new_tmp_var()
		cur_line := g.go_before_last_stmt().trim_space()
		g.empty_line = true
		g.write('bool ${tmp} = (')
		g.expr(node.left)
		g.writeln(');')
		g.set_current_pos_as_last_stmt_pos()
		g.write('${cur_line} ${tmp} ${node.op.str()} ')
		g.infix_left_var_name = if node.op == .and { tmp } else { '!${tmp}' }
		g.expr(node.right)
		g.infix_left_var_name = ''
		g.inside_ternary = prev_inside_ternary
	} else {
		g.gen_plain_infix_expr(node)
	}
}

fn (mut g Gen) gen_is_none_check(node ast.InfixExpr) {
	if node.left in [ast.Ident, ast.SelectorExpr, ast.IndexExpr, ast.CallExpr] {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		g.write('(')
		g.expr(node.left)
		g.write(')')
		g.inside_opt_or_res = old_inside_opt_or_res
		dot_or_ptr := if !node.left_type.has_flag(.option_mut_param_t) { '.' } else { '->' }
		g.write('${dot_or_ptr}state')
	} else {
		stmt_str := g.go_before_last_stmt().trim_space()
		g.empty_line = true
		left_var := g.expr_with_opt(node.left, node.left_type, node.left_type)
		g.writeln(';')
		g.write2(stmt_str, ' ')
		dot_or_ptr := if !node.left_type.has_flag(.option_mut_param_t) { '.' } else { '->' }
		g.write('${left_var}${dot_or_ptr}state')
	}
	g.write(' ${node.op.str()} 2') // none state
}

// gen_plain_infix_expr generates basic code for infix expressions,
// without any overloading of any kind
// i.e. v`a + 1` => c`a + 1`
// It handles auto dereferencing of variables, as well as automatic casting
// (see Gen.expr_with_cast for more details)
fn (mut g Gen) gen_plain_infix_expr(node ast.InfixExpr) {
	needs_cast := node.left_type.is_number() && node.right_type.is_number()
		&& node.op in [.plus, .minus, .mul, .div, .mod] && !(g.pref.translated
		|| g.file.is_translated)
	if needs_cast {
		typ_str := if node.left_ct_expr {
			g.styp(g.type_resolver.get_type_or_default(node.left, node.left_type))
		} else if node.left !in [ast.Ident, ast.CastExpr] && node.right_ct_expr {
			g.styp(g.type_resolver.get_type_or_default(node.right, node.promoted_type))
		} else {
			g.styp(node.promoted_type)
		}
		g.write('(${typ_str})(')
	}
	if node.left_type.is_ptr() && node.left.is_auto_deref_var() && !node.right_type.is_pointer() {
		g.write('*')
	} else if !g.inside_interface_deref && node.left is ast.Ident
		&& g.table.is_interface_var(node.left.obj) {
		inside_interface_deref_old := g.inside_interface_deref
		g.inside_interface_deref = true
		defer {
			g.inside_interface_deref = inside_interface_deref_old
		}
	}
	is_ctemp_fixed_ret := node.op in [.eq, .ne] && node.left is ast.CTempVar
		&& node.left.is_fixed_ret
	if is_ctemp_fixed_ret {
		if node.op == .eq {
			g.write('!')
		}
		g.write('memcmp(')
	}
	g.expr(node.left)
	if !is_ctemp_fixed_ret {
		g.write(' ${node.op.str()} ')
	} else {
		g.write(', ')
	}

	if is_ctemp_fixed_ret {
		g.write('(${g.styp(node.right_type)})')
	}
	if node.right_type.is_ptr() && node.right.is_auto_deref_var() && !node.left_type.is_pointer() {
		g.write('*')
		g.expr(node.right)
	} else {
		g.expr_with_cast(node.right, node.right_type, node.left_type)
	}
	if is_ctemp_fixed_ret {
		g.write(', sizeof(${g.styp(node.right_type)}))')
	}
	if needs_cast {
		g.write(')')
	}
}

fn (mut g Gen) op_arg(expr ast.Expr, expected ast.Type, got ast.Type) {
	mut needs_closing := false
	mut nr_muls := got.nr_muls()
	if expected.is_ptr() {
		if nr_muls > 0 {
			nr_muls--
		} else {
			if expr.is_lvalue() {
				g.write('&')
			} else {
				styp := g.styp(got.set_nr_muls(0))
				g.write('ADDR(${styp}, ')
				needs_closing = true
			}
		}
	}
	g.write('*'.repeat(nr_muls))
	g.expr(expr)
	if needs_closing {
		g.write(')')
	}
}

struct GenSafeIntegerCfg {
	op            token.Kind
	reverse       bool
	unsigned_type ast.Type
	unsigned_expr ast.Expr
	signed_type   ast.Type
	signed_expr   ast.Expr
}

// gen_safe_integer_infix_expr generates code for comparison of
// unsigned and signed integers
fn (mut g Gen) gen_safe_integer_infix_expr(cfg GenSafeIntegerCfg) {
	bitsize := if cfg.unsigned_type.idx() == ast.u32_type_idx
		&& cfg.signed_type.idx() != ast.i64_type_idx {
		32
	} else {
		64
	}
	op_idx := int(cfg.op) - int(token.Kind.eq)
	op_str := if cfg.reverse { cmp_rev[op_idx] } else { cmp_str[op_idx] }
	g.write('_us${bitsize}_${op_str}(')
	g.expr(cfg.unsigned_expr)
	g.write(',')
	g.expr(cfg.signed_expr)
	g.write(')')
}
