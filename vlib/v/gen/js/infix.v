module js

import v.util
import v.ast

fn (mut g JsGen) gen_plain_infix_expr(node ast.InfixExpr) {
	it := node
	l_sym := g.table.final_sym(it.left_type)
	r_sym := g.table.final_sym(it.right_type)
	greater_typ := g.greater_typ(it.left_type, it.right_type)
	cast_ty := if greater_typ == it.left_type { l_sym.cname } else { r_sym.cname }
	g.write('new ${g.js_name(cast_ty)}( ')
	g.cast_stack << greater_typ
	if !g.pref.output_es5 && ((l_sym.kind == .i64 || l_sym.kind == .u64)
		|| (r_sym.kind == .i64 || r_sym.kind == .u64)) {
		g.write('BigInt(')
		g.expr(node.left)
		g.gen_deref_ptr(node.left_type)
		g.write('.valueOf())')
		g.write(' ${node.op.str()} ')
		g.write('BigInt(')
		g.expr(node.right)
		g.gen_deref_ptr(node.left_type)
		g.write('.valueOf())')
	} else {
		g.expr(node.left)
		g.gen_deref_ptr(node.left_type)
		g.write('.valueOf()')
		g.write(' ${node.op.str()} ')
		g.expr(node.right)
		g.gen_deref_ptr(node.left_type)
		g.write('.valueOf()')
	}

	g.cast_stack.delete_last()
	g.write(')')
}

fn (mut g JsGen) infix_expr_arithmetic_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	method := g.table.find_method(left.sym, node.op.str()) or {
		g.gen_plain_infix_expr(node)
		return
	}
	left_styp := g.typ(left.typ.set_nr_muls(0))
	g.write(left_styp)
	g.write('_')
	g.write(util.replace_op(node.op.str()))
	g.write('(')
	g.op_arg(node.left, method.params[0].typ, left.typ)
	g.write(', ')
	g.op_arg(node.right, method.params[1].typ, right.typ)
	g.write(')')
}

fn (mut g JsGen) op_arg(expr ast.Expr, expected ast.Type, got ast.Type) {
	mut needs_closing := 0
	mut nr_muls := got.nr_muls()
	if expected.is_ptr() {
		if nr_muls > 0 {
			nr_muls--
		} else {
			g.write('new \$ref(')
			needs_closing++
		}
	}
	g.expr(expr)
	g.write('.val'.repeat(nr_muls))
	for i := 0; i < needs_closing; i++ {
		g.write(')')
	}
}

fn (mut g JsGen) infix_expr_eq_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	has_operator_overloading := g.table.has_method(left.sym, '==')
	g.write('new bool(')
	if (left.typ.is_ptr() && right.typ.is_int()) || (right.typ.is_ptr() && left.typ.is_int()) {
		g.gen_plain_infix_expr(node)
	} else if has_operator_overloading {
		if node.op == .ne {
			g.write('!')
		}
		g.write(g.typ(left.unaliased.set_nr_muls(0)))
		g.write('__eq(')
		g.expr(node.left)
		g.gen_deref_ptr(node.left_type)
		g.write(',')
		g.expr(node.right)
		g.gen_deref_ptr(node.right_type)
		g.write(')')
		if node.op == .ne {
			g.write('.valueOf()')
		}
	} else if left.typ.idx() == right.typ.idx()
		&& left.sym.kind in [.array, .array_fixed, .alias, .map, .struct_, .sum_type] {
		match left.sym.kind {
			.alias {
				ptr_typ := g.gen_alias_equality_fn(left.typ)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_alias_eq(')
				g.expr(node.left)
				g.gen_deref_ptr(node.left_type)
				g.write(', ')
				g.expr(node.right)
				g.gen_deref_ptr(node.right_type)
				g.write(')')
				if node.op == .ne {
					g.write('.valueOf()')
				}
			}
			.array {
				ptr_typ := g.gen_array_equality_fn(left.unaliased.clear_flag(.shared_f))
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_arr_eq(')
				g.expr(node.left)
				g.gen_deref_ptr(node.left_type)
				g.write(', ')
				g.expr(node.right)
				g.gen_deref_ptr(node.right_type)
				g.write(')')
				if node.op == .ne {
					g.write('.valueOf()')
				}
			}
			.array_fixed {
				ptr_typ := g.gen_fixed_array_equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_arr_eq(')
				g.expr(node.left)
				g.gen_deref_ptr(node.left_type)
				g.write(', ')
				g.expr(node.right)
				g.gen_deref_ptr(node.right_type)
				g.write(')')
				if node.op == .ne {
					g.write('.valueOf()')
				}
			}
			.map {
				ptr_typ := g.gen_map_equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_map_eq(')
				g.expr(node.left)
				g.gen_deref_ptr(node.left_type)
				g.write(', ')
				g.expr(node.right)
				g.gen_deref_ptr(node.right_type)
				g.write(')')
				if node.op == .ne {
					g.write('.valueOf()')
				}
			}
			.struct_ {
				ptr_typ := g.gen_struct_equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_struct_eq(')
				g.expr(node.left)
				g.gen_deref_ptr(node.left_type)
				g.write(', ')
				g.expr(node.right)
				g.gen_deref_ptr(node.right_type)
				g.write(')')
				if node.op == .ne {
					g.write('.valueOf()')
				}
			}
			.sum_type {
				ptr_typ := g.gen_sumtype_equality_fn(left.unaliased)
				if node.op == .ne {
					g.write('!')
				}
				g.write('${ptr_typ}_sumtype_eq(')

				g.expr(node.left)
				g.gen_deref_ptr(node.left_type)
				g.write(', ')
				g.expr(node.right)
				g.gen_deref_ptr(node.right_type)
				g.write(')')
				if node.op == .ne {
					g.write('.valueOf()')
				}
			}
			else {}
		}
	} else {
		g.expr(node.left)
		g.gen_deref_ptr(node.left_type)
		g.write('.valueOf() ${node.op.str()} ')
		g.expr(node.right)
		g.gen_deref_ptr(node.right_type)
		g.write('.valueOf()')
	}
	g.write(')')
}

fn (mut g JsGen) infix_expr_cmp_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	has_operator_overloading := g.table.has_method(left.sym, '<')
	g.write('new bool(')
	if left.sym.kind == right.sym.kind && has_operator_overloading {
		if node.op in [.le, .ge] {
			g.write('!')
		}
		g.write(g.typ(left.typ.set_nr_muls(0)))
		g.write('__lt')
		if node.op in [.lt, .ge] {
			g.write('(')

			g.expr(node.left)
			g.gen_deref_ptr(left.typ)
			g.write(', ')
			g.expr(node.right)
			g.gen_deref_ptr(right.typ)
			g.write(')')
		} else {
			g.write('(')
			g.expr(node.right)
			g.gen_deref_ptr(left.typ)
			g.write(', ')
			g.expr(node.left)
			g.gen_deref_ptr(right.typ)
			g.write(')')
		}
	} else {
		g.expr(node.left)
		g.gen_deref_ptr(node.left_type)
		g.write('.valueOf() ${node.op.str()} ')
		g.expr(node.right)
		g.gen_deref_ptr(node.right_type)
		g.write('.valueOf()')
	}

	g.write(')')
}

fn (mut g JsGen) infix_expr_left_shift_op(node ast.InfixExpr) {
	left := g.unwrap(node.left_type)
	right := g.unwrap(node.right_type)
	if left.unaliased_sym.kind == .array {
		// arr << val
		array_info := left.unaliased_sym.info as ast.Array
		g.write('array_push(')
		//&& array_info.elem_type != g.unwrap_generic(node.right_type)
		if right.unaliased_sym.kind == .array && array_info.elem_type != right.typ {
			g.expr(node.left)
			g.gen_deref_ptr(left.typ)
			g.write(',')
			g.expr(node.right)
			g.gen_deref_ptr(right.typ)
			g.write('.arr.arr')
			g.write(',true)')
		} else {
			g.expr(node.left)
			g.gen_deref_ptr(left.typ)
			g.write(',')
			g.expr(node.right)
			g.write(',false)')
		}
	} else {
		g.gen_plain_infix_expr(node)
	}
}

fn (mut g JsGen) infix_in_not_in_op(node ast.InfixExpr) {
	l_sym := g.unwrap(node.left_type)
	r_sym := g.unwrap(node.right_type)
	if node.op == .not_in {
		g.write('!')
	}
	if r_sym.unaliased_sym.kind in [.array, .array_fixed] {
		fn_name := g.gen_array_contains_method(node.right_type)
		g.write('(${fn_name}(')
		g.expr(node.right)
		g.gen_deref_ptr(node.right_type)
		g.write(',')
		g.expr(node.left)
		g.write(')')
		if node.op == .not_in {
			g.write('.valueOf()')
		}
		g.write(')')
		return
	} else if r_sym.unaliased_sym.kind == .map {
		g.expr(node.right)
		g.gen_deref_ptr(node.right_type)
		g.write('.has(')
		g.expr(node.left)
		/*
		if l_sym.sym.kind == .string {
			g.write('.str')
		} else {
			g.write('.valueOf()')
		}*/
		g.write('.\$toJS()')
		g.write(')')
	} else {
		g.write('.str.includes(')
		g.expr(node.right)
		g.gen_deref_ptr(node.right_type)
		g.expr(node.left)
		if l_sym.sym.kind == .string {
			g.write('.str')
		} else {
			g.write('.valueOf()')
		}
		g.write(')')
	}

	/*
	if r_sym.kind == .map {
		g.write('.map.has(')
	} else if r_sym.kind == .string {
		g.write('.str.includes(')
	} else {
		g.write('.\$includes(')
	}
	g.expr(node.left)
	if l_sym.kind == .string {
		g.write('.str')
	}
	g.write(')')*/
}

fn (mut g JsGen) infix_is_not_is_op(node ast.InfixExpr) {
	g.expr(node.left)
	rsym := g.table.sym(g.unwrap(node.right_type).typ)

	g.gen_deref_ptr(node.left_type)
	g.write(' instanceof ')
	g.write(g.js_name(rsym.name))
}

fn (mut g JsGen) infix_expr(node ast.InfixExpr) {
	match node.op {
		.plus, .minus, .mul, .div, .mod {
			g.infix_expr_arithmetic_op(node)
		}
		.eq, .ne {
			g.infix_expr_eq_op(node)
		}
		.gt, .ge, .lt, .le {
			g.infix_expr_cmp_op(node)
		}
		.logical_or, .and {
			g.gen_plain_infix_expr(node)
		}
		.left_shift {
			g.infix_expr_left_shift_op(node)
		}
		.not_in, .key_in {
			g.infix_in_not_in_op(node)
		}
		.key_is, .not_is {
			g.infix_is_not_is_op(node)
		}
		else {
			g.gen_plain_infix_expr(node)
		}
	}
}
