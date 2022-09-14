// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.token
import v.util

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	if node.auto_locked != '' {
		g.writeln('sync__RwMutex_lock(&$node.auto_locked->mtx);')
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
			g.infix_expr_left_shift_op(node)
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
		g.write('sync__RwMutex_unlock(&$node.auto_locked->mtx)')
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
		elem_styp := g.typ(elem_type)
		g.register_chan_push_optional_fn(elem_styp, styp)
		g.write('${option_name}_void $tmp_opt = __Option_${styp}_pushval(')
	} else {
		g.write('__${styp}_pushval(')
	}
	g.expr(node.left)
	g.write(', ')
	if g.table.sym(elem_type).kind in [.sum_type, .interface_] {
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
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	has_defined_eq_operator := g.table.has_method(left.sym, '==')
	has_alias_eq_op_overload := left.sym.info is ast.Alias && left.sym.has_method('==')
	if g.pref.translated && !g.is_builtin_mod {
		g.gen_plain_infix_expr(node)
		return
	}
	if (left.typ.is_ptr() && right.typ.is_int()) || (right.typ.is_ptr() && left.typ.is_int()) {
		g.gen_plain_infix_expr(node)
	} else if (left.typ.idx() == ast.string_type_idx || (!has_defined_eq_operator
		&& left.unaliased.idx() == ast.string_type_idx)) && node.right is ast.StringLiteral
		&& (node.right as ast.StringLiteral).val == '' {
		// `str == ''` -> `str.len == 0` optimization
		g.write('(')
		g.expr(node.left)
		g.write(')')
		arrow := if left.typ.is_ptr() { '->' } else { '.' }
		g.write('${arrow}len $node.op 0')
	} else if has_defined_eq_operator {
		if node.op == .ne {
			g.write('!')
		}
		if has_alias_eq_op_overload {
			g.write(g.typ(left.typ.set_nr_muls(0)))
		} else {
			g.write(g.typ(left.unaliased.set_nr_muls(0)))
		}
		g.write('__eq(')
		g.write('*'.repeat(left.typ.nr_muls()))
		g.expr(node.left)
		g.write(', ')
		g.write('*'.repeat(right.typ.nr_muls()))
		g.expr(node.right)
		g.write(')')
	} else if left.typ.idx() == right.typ.idx()
		&& left.sym.kind in [.array, .array_fixed, .alias, .map, .struct_, .sum_type, .interface_] {
		if g.pref.translated && !g.is_builtin_mod {
			g.gen_plain_infix_expr(node)
			return
		}
		match left.sym.kind {
			.alias {
				ptr_typ := g.equality_fn(left.typ)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_alias_eq(')
				if left.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.right)
				g.write(')')
			}
			.array {
				ptr_typ := g.equality_fn(left.unaliased.clear_flag(.shared_f))
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_arr_eq(')
				if left.typ.is_ptr() && !left.typ.has_flag(.shared_f) {
					g.write('*')
				}
				g.expr(node.left)
				if left.typ.has_flag(.shared_f) {
					g.write('->val')
				}
				g.write(', ')
				if right.typ.is_ptr() && !right.typ.has_flag(.shared_f) {
					g.write('*')
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
					if !node.left.has_it {
						s := g.typ(left.unaliased)
						g.write('($s)')
					}
				}
				g.expr(node.left)
				g.write(', ')
				if node.right is ast.ArrayInit {
					if !node.right.has_it {
						s := g.typ(right.unaliased)
						g.write('($s)')
					}
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
					g.write('*')
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.right)
				g.write(')')
			}
			.struct_ {
				// if g.pref.translated {
				// g.gen_plain_infix_expr(node)
				//} else {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_struct_eq(')
				if left.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.right)
				g.write(')')
				//}
			}
			.sum_type {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_sumtype_eq(')
				if left.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.right)
				g.write(')')
			}
			.interface_ {
				ptr_typ := g.equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_interface_eq(')
				if left.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.left)
				g.write(', ')
				if right.typ.is_ptr() {
					g.write('*')
				}
				g.expr(node.right)
				g.write(')')
			}
			else {}
		}
	} else if left.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& right.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op: node.op
			unsigned_type: left.unaliased
			unsigned_expr: node.left
			signed_type: right.unaliased
			signed_expr: node.right
		)
	} else if right.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& left.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op: node.op
			reverse: true
			unsigned_type: right.unaliased
			unsigned_expr: node.right
			signed_type: left.unaliased
			signed_expr: node.left
		)
	} else {
		g.gen_plain_infix_expr(node)
	}
}

// infix_expr_cmp_op generates code for `<`, `<=`, `>`, `>=`
// It handles operator overloading when necessary
fn (mut g Gen) infix_expr_cmp_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	has_operator_overloading := g.table.has_method(left.sym, '<')
	if g.pref.translated && !g.is_builtin_mod {
		g.gen_plain_infix_expr(node)
		return
	}
	if left.sym.kind == .struct_ && (left.sym.info as ast.Struct).generic_types.len > 0 {
		if node.op in [.le, .ge] {
			g.write('!')
		}
		concrete_types := (left.sym.info as ast.Struct).concrete_types
		mut method_name := left.sym.cname + '__lt'
		method_name = g.generic_fn_name(concrete_types, method_name)
		g.write(method_name)
		if node.op in [.lt, .ge] {
			g.write('(')
			g.write('*'.repeat(left.typ.nr_muls()))
			g.expr(node.left)
			g.write(', ')
			g.write('*'.repeat(right.typ.nr_muls()))
			g.expr(node.right)
			g.write(')')
		} else {
			g.write('(')
			g.write('*'.repeat(right.typ.nr_muls()))
			g.expr(node.right)
			g.write(', ')
			g.write('*'.repeat(left.typ.nr_muls()))
			g.expr(node.left)
			g.write(')')
		}
	} else if left.sym.kind == right.sym.kind && has_operator_overloading {
		if node.op in [.le, .ge] {
			g.write('!')
		}
		g.write(g.typ(left.typ.set_nr_muls(0)))
		g.write('__lt')
		if node.op in [.lt, .ge] {
			g.write('(')
			g.write('*'.repeat(left.typ.nr_muls()))
			g.expr(node.left)
			g.write(', ')
			g.write('*'.repeat(right.typ.nr_muls()))
			g.expr(node.right)
			g.write(')')
		} else {
			g.write('(')
			g.write('*'.repeat(right.typ.nr_muls()))
			g.expr(node.right)
			g.write(', ')
			g.write('*'.repeat(left.typ.nr_muls()))
			g.expr(node.left)
			g.write(')')
		}
	} else if left.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& right.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op: node.op
			unsigned_type: left.unaliased
			unsigned_expr: node.left
			signed_type: right.unaliased
			signed_expr: node.right
		)
	} else if right.unaliased.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& left.unaliased.is_signed() {
		g.gen_safe_integer_infix_expr(
			op: node.op
			reverse: true
			unsigned_type: right.unaliased
			unsigned_expr: node.right
			signed_type: left.unaliased
			signed_expr: node.left
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
		if left.sym.kind in [.sum_type, .interface_] {
			if node.right is ast.ArrayInit {
				if node.right.exprs.len > 0
					&& g.table.sym(node.right.expr_types[0]).kind !in [.sum_type, .interface_] {
					mut infix_exprs := []ast.InfixExpr{}
					for i in 0 .. node.right.exprs.len {
						infix_exprs << ast.InfixExpr{
							op: .key_is
							left: node.left
							left_type: node.left_type
							right: node.right.exprs[i]
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
			if node.right.exprs.len > 0 {
				// `a in [1,2,3]` optimization => `a == 1 || a == 2 || a == 3`
				// avoids an allocation
				g.write('(')
				if elem_sym.kind == .sum_type && left.sym.kind != .sum_type {
					if node.left_type in elem_sym.sumtype_info().variants {
						new_node_left := ast.CastExpr{
							arg: ast.empty_expr
							typ: elem_type
							expr: node.left
							expr_type: node.left_type
						}
						g.infix_expr_in_optimization(new_node_left, node.right)
					}
				} else {
					g.infix_expr_in_optimization(node.left, node.right)
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
						arg: ast.empty_expr
						typ: elem_type
						expr: node.left
						expr_type: ast.mktyp(node.left_type)
					}
					g.gen_array_contains(node.right_type, node.right, new_node_left)
					return
				}
			}
		}
		g.gen_array_contains(node.right_type, node.right, node.left)
	} else if right.unaliased_sym.kind == .map {
		g.write('_IN_MAP(')
		if !left.typ.is_ptr() {
			styp := g.typ(node.left_type)
			g.write('ADDR($styp, ')
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
		if left.sym.kind in [.sum_type, .interface_] {
			if node.right is ast.ArrayInit {
				if node.right.exprs.len > 0 {
					mut infix_exprs := []ast.InfixExpr{}
					for i in 0 .. node.right.exprs.len {
						infix_exprs << ast.InfixExpr{
							op: .key_is
							left: node.left
							left_type: node.left_type
							right: node.right.exprs[i]
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
				g.infix_expr_in_optimization(node.left, node.right)
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
						arg: ast.empty_expr
						typ: elem_type
						expr: node.left
						expr_type: ast.mktyp(node.left_type)
					}
					g.gen_array_contains(node.right_type, node.right, new_node_left)
					return
				}
			}
		}
		g.gen_array_contains(node.right_type, node.right, node.left)
	} else if right.unaliased_sym.kind == .string {
		g.write('string_contains(')
		g.expr(node.right)
		g.write(', ')
		g.expr(node.left)
		g.write(')')
	}
}

// infix_expr_in_optimization optimizes `<var> in <array>` expressions,
// and transform them in a serie of equality comparison
// i.e. `a in [1,2,3]` => `a == 1 || a == 2 || a == 3`
fn (mut g Gen) infix_expr_in_optimization(left ast.Expr, right ast.ArrayInit) {
	mut elem_sym := g.table.sym(right.elem_type)
	for i, array_expr in right.exprs {
		match elem_sym.kind {
			.string, .alias, .sum_type, .map, .interface_, .array, .struct_ {
				if elem_sym.kind == .string {
					g.write('string__eq(')
				} else {
					ptr_typ := g.equality_fn(right.elem_type)
					if elem_sym.kind == .alias {
						g.write('${ptr_typ}_alias_eq(')
					} else if elem_sym.kind == .sum_type {
						g.write('${ptr_typ}_sumtype_eq(')
					} else if elem_sym.kind == .map {
						g.write('${ptr_typ}_map_eq(')
					} else if elem_sym.kind == .interface_ {
						g.write('${ptr_typ}_interface_eq(')
					} else if elem_sym.kind == .array {
						g.write('${ptr_typ}_arr_eq(')
					} else if elem_sym.kind == .struct_ {
						g.write('${ptr_typ}_struct_eq(')
					}
				}
				g.expr(left)
				g.write(', ')
				g.expr(array_expr)
				g.write(')')
			}
			else { // works in function kind
				g.expr(left)
				g.write(' == ')
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
	mut left_sym := g.table.sym(node.left_type)
	is_aggregate := left_sym.kind == .aggregate
	if is_aggregate {
		parent_left_type := (left_sym.info as ast.Aggregate).sum_type
		left_sym = g.table.sym(parent_left_type)
	}
	right_sym := g.table.sym(node.right_type)
	if left_sym.kind == .interface_ && right_sym.kind == .interface_ {
		g.gen_interface_is_op(node)
		return
	}

	cmp_op := if node.op == .key_is { '==' } else { '!=' }
	g.write('(')
	if is_aggregate {
		g.write('$node.left')
	} else {
		g.expr(node.left)
	}
	g.write(')')
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	if left_sym.kind == .interface_ {
		g.write('_typ $cmp_op ')
		// `_Animal_Dog_index`
		sub_type := match node.right {
			ast.TypeNode {
				g.unwrap_generic(node.right.typ)
			}
			ast.None {
				g.table.type_idxs['None__']
			}
			else {
				ast.Type(0)
			}
		}
		sub_sym := g.table.sym(sub_type)
		g.write('_${left_sym.cname}_${sub_sym.cname}_index')
		return
	} else if left_sym.kind == .sum_type {
		g.write('_typ $cmp_op ')
	}
	if node.right is ast.None {
		g.write('$ast.none_type.idx() /* none */')
	} else {
		g.expr(node.right)
	}
}

fn (mut g Gen) gen_interface_is_op(node ast.InfixExpr) {
	mut left_sym := g.table.sym(node.left_type)
	right_sym := g.table.sym(node.right_type)

	mut info := left_sym.info as ast.Interface

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
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	if left.sym.kind == .struct_ && (left.sym.info as ast.Struct).generic_types.len > 0 {
		concrete_types := (left.sym.info as ast.Struct).concrete_types
		mut method_name := left.sym.cname + '_' + util.replace_op(node.op.str())
		method_name = g.generic_fn_name(concrete_types, method_name)
		g.write(method_name)
		g.write('(')
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
		} else if left.unaliased_sym.has_method(node.op.str()) {
			method = left.unaliased_sym.find_method(node.op.str()) or { ast.Fn{} }
			method_name = left.unaliased_sym.cname + '_' + util.replace_op(node.op.str())
		} else {
			g.gen_plain_infix_expr(node)
			return
		}
		g.write(method_name)
		g.write('(')
		g.op_arg(node.left, method.params[0].typ, left.typ)
		g.write(', ')
		g.op_arg(node.right, method.params[1].typ, right.typ)
		g.write(')')
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
		if right.unaliased_sym.kind == .array && array_info.elem_type != right.typ
			&& !(right.sym.kind == .alias
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
			g.write(', (')
			g.expr_with_cast(node.right, node.right_type, left.unaliased.clear_flag(.shared_f))
			styp := g.typ(expected_push_many_atype)
			g.write('), $tmp_var, $styp)')
		} else {
			// push a single element
			elem_type_str := g.typ(array_info.elem_type)
			elem_sym := g.table.sym(array_info.elem_type)
			elem_is_array_var := elem_sym.kind in [.array, .array_fixed] && node.right is ast.Ident
			g.write('array_push${noscan}((array*)')
			if !left.typ.is_ptr()
				|| (node.left_type.has_flag(.shared_f) && !node.left_type.deref().is_ptr()) {
				g.write('&')
			}
			g.expr(node.left)
			if node.left_type.has_flag(.shared_f) {
				g.write('->val')
			}
			if elem_sym.kind == .function {
				g.write(', _MOV((voidptr[]){ ')
			} else if elem_is_array_var {
				addr := if elem_sym.kind == .array_fixed { '' } else { '&' }
				g.write(', $addr')
			} else {
				g.write(', _MOV(($elem_type_str[]){ ')
			}
			// if g.autofree
			needs_clone := !g.is_builtin_mod && array_info.elem_type.idx() == ast.string_type_idx
				&& array_info.elem_type.nr_muls() == 0
			if needs_clone {
				g.write('string_clone(')
			}
			g.expr_with_cast(node.right, node.right_type, array_info.elem_type)
			if needs_clone {
				g.write(')')
			}
			if elem_is_array_var {
				g.write(')')
			} else {
				g.write(' }))')
			}
		}
	} else {
		g.gen_plain_infix_expr(node)
	}
}

fn (mut g Gen) need_tmp_var_in_array_call(node ast.Expr) bool {
	match node {
		ast.CallExpr {
			if node.left_type != 0 && g.table.sym(node.left_type).kind == .array
				&& node.name in ['all', 'any', 'filter', 'map'] {
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
	if node.right is ast.IfExpr {
		// `b := a && if true { a = false ...} else {...}`
		prev_inside_ternary := g.inside_ternary
		g.inside_ternary = 0
		if g.need_tmp_var_in_if(node.right) {
			tmp := g.new_tmp_var()
			cur_line := g.go_before_stmt(0).trim_space()
			g.empty_line = true
			g.write('bool $tmp = (')
			g.expr(node.left)
			g.writeln(');')
			g.set_current_pos_as_last_stmt_pos()
			g.write('$cur_line $tmp $node.op.str() ')
			g.infix_left_var_name = if node.op == .and { tmp } else { '!$tmp' }
			g.expr(node.right)
			g.infix_left_var_name = ''
			g.inside_ternary = prev_inside_ternary
			return
		}
		g.inside_ternary = prev_inside_ternary
	} else if g.need_tmp_var_in_array_call(node.right) {
		// `if a == 0 || arr.any(it.is_letter()) {...}`
		tmp := g.new_tmp_var()
		cur_line := g.go_before_stmt(0).trim_space()
		g.empty_line = true
		if g.infix_left_var_name.len > 0 {
			g.write('bool $tmp = (($g.infix_left_var_name) $node.op.str() ')
		} else {
			g.write('bool $tmp = (')
		}
		g.expr(node.left)
		g.writeln(');')
		g.set_current_pos_as_last_stmt_pos()
		g.write('$cur_line $tmp $node.op.str() ')
		g.infix_left_var_name = if node.op == .and { tmp } else { '!$tmp' }
		g.expr(node.right)
		g.infix_left_var_name = ''
		return
	}
	g.gen_plain_infix_expr(node)
}

// gen_plain_infix_expr generates basic code for infix expressions,
// without any overloading of any kind
// i.e. v`a + 1` => c`a + 1`
// It handles auto dereferencing of variables, as well as automatic casting
// (see Gen.expr_with_cast for more details)
fn (mut g Gen) gen_plain_infix_expr(node ast.InfixExpr) {
	if node.left_type.is_ptr() && node.left.is_auto_deref_var() {
		g.write('*')
	}
	g.expr(node.left)
	g.write(' $node.op.str() ')
	g.expr_with_cast(node.right, node.right_type, node.left_type)
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
				styp := g.typ(got.set_nr_muls(0))
				g.write('ADDR($styp, ')
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
