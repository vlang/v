// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.token
import v.util

fn (mut g Gen) infix_expr(node ast.InfixExpr) {
	// TODO lot of clean required here
	if node.auto_locked != '' {
		g.writeln('sync__RwMutex_lock(&$node.auto_locked->mtx);')
	}
	if node.op in [.key_is, .not_is] {
		g.is_expr(node)
		return
	}
	left_type := g.unwrap_generic(node.left_type)
	// println('>>$node')
	left_sym := g.table.get_type_symbol(left_type)
	left_final_sym := g.table.get_final_type_symbol(left_type)
	// TODO cleanup: linked to left/right_final_sym, unaliasing done twice
	unaliased_left := if left_sym.kind == .alias {
		(left_sym.info as ast.Alias).parent_type
	} else {
		left_type
	}
	op_is_eq_or_ne := node.op in [.eq, .ne]
	right_sym := g.table.get_type_symbol(node.right_type)
	right_final_sym := g.table.get_final_type_symbol(node.right_type)
	if node.op in [.key_in, .not_in] {
		// TODO cleanup: handle the same as is / !is
		g.infix_in_or_not_in(node, left_final_sym, right_final_sym)
		return
	}
	unaliased_right := if right_sym.info is ast.Alias {
		right_sym.info.parent_type
	} else {
		node.right_type
	}
	if unaliased_left == ast.string_type_idx && op_is_eq_or_ne && node.right is ast.StringLiteral
		&& (node.right as ast.StringLiteral).val == '' {
		// `str == ''` -> `str.len == 0` optimization
		g.write('(')
		g.expr(node.left)
		g.write(')')
		arrow := if left_type.is_ptr() { '->' } else { '.' }
		g.write('${arrow}len $node.op 0')
	} else if op_is_eq_or_ne && left_sym.kind == right_sym.kind
		&& left_sym.kind in [.array, .array_fixed, .alias, .map, .struct_, .sum_type] {
		g.infix_gen_equality(node, left_type, left_sym, right_sym)
	} else if op_is_eq_or_ne && left_sym.kind == .alias && unaliased_right == ast.string_type {
		// TODO cleanup: almost copy of above
		g.infix_gen_equality(node, unaliased_left, left_final_sym, right_sym)
	} else if node.op == .left_shift && left_final_sym.kind == .array {
		// arr << val
		tmp := g.new_tmp_var()
		info := left_final_sym.info as ast.Array
		noscan := g.check_noscan(info.elem_type)
		if right_final_sym.kind == .array && info.elem_type != g.unwrap_generic(node.right_type) {
			// push an array => PUSH_MANY, but not if pushing an array to 2d array (`[][]int << []int`)
			g.write('_PUSH_MANY${noscan}(')
			mut expected_push_many_atype := left_type
			if !expected_push_many_atype.is_ptr() {
				// fn f(mut a []int) { a << [1,2,3] } -> type of `a` is `array_int*` -> no need for &
				g.write('&')
			} else {
				expected_push_many_atype = expected_push_many_atype.deref()
			}
			g.expr(node.left)
			g.write(', (')
			g.expr_with_cast(node.right, node.right_type, left_type)
			styp := g.typ(expected_push_many_atype)
			g.write('), $tmp, $styp)')
		} else {
			// push a single element
			elem_type_str := g.typ(info.elem_type)
			elem_sym := g.table.get_type_symbol(info.elem_type)
			g.write('array_push${noscan}((array*)')
			if !left_type.is_ptr() {
				g.write('&')
			}
			g.expr(node.left)
			if elem_sym.kind == .function {
				g.write(', _MOV((voidptr[]){ ')
			} else {
				g.write(', _MOV(($elem_type_str[]){ ')
			}
			// if g.autofree
			needs_clone := info.elem_type == ast.string_type && !g.is_builtin_mod
			if needs_clone {
				g.write('string_clone(')
			}
			g.expr_with_cast(node.right, node.right_type, info.elem_type)
			if needs_clone {
				g.write(')')
			}
			g.write(' }))')
		}
	} else if node.op == .arrow {
		// chan <- val
		gen_or := node.or_block.kind != .absent
		styp := left_sym.cname
		mut left_inf := left_sym.info as ast.Chan
		elem_type := left_inf.elem_type
		tmp_opt := if gen_or { g.new_tmp_var() } else { '' }
		if gen_or {
			elem_styp := g.typ(elem_type)
			g.register_chan_push_optional_call(elem_styp, styp)
			g.write('Option_void $tmp_opt = __Option_${styp}_pushval(')
		} else {
			g.write('__${styp}_pushval(')
		}
		g.expr(node.left)
		g.write(', ')
		g.expr(node.right)
		g.write(')')
		if gen_or {
			g.or_block(tmp_opt, node.or_block, ast.void_type)
		}
	} else if unaliased_left.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& unaliased_right.is_signed() && node.op in [.eq, .ne, .gt, .lt, .ge, .le] {
		bitsize := if unaliased_left.idx() == ast.u32_type_idx
			&& unaliased_right.idx() != ast.i64_type_idx {
			32
		} else {
			64
		}
		g.write('_us${bitsize}_${c.cmp_str[int(node.op) - int(token.Kind.eq)]}(')
		g.expr(node.left)
		g.write(',')
		g.expr(node.right)
		g.write(')')
	} else if unaliased_right.idx() in [ast.u32_type_idx, ast.u64_type_idx]
		&& unaliased_left.is_signed() && node.op in [.eq, .ne, .gt, .lt, .ge, .le] {
		bitsize := if unaliased_right.idx() == ast.u32_type_idx
			&& unaliased_left.idx() != ast.i64_type_idx {
			32
		} else {
			64
		}
		g.write('_us${bitsize}_${c.cmp_rev[int(node.op) - int(token.Kind.eq)]}(')
		g.expr(node.right)
		g.write(',')
		g.expr(node.left)
		g.write(')')
	} else {
		// this likely covers more than V struct, but no idea what...
		is_v_struct := ((left_sym.name[0].is_capital() || left_sym.name.contains('.'))
			&& left_sym.kind !in [.enum_, .function, .interface_, .sum_type]
			&& left_sym.language != .c) || left_sym.kind == .string
			|| unaliased_left == ast.ustring_type
		is_alias := left_sym.kind == .alias
		is_c_alias := is_alias && (left_sym.info as ast.Alias).language == .c
		// Check if aliased type is a struct
		is_struct_alias := is_alias
			&& g.typ((left_sym.info as ast.Alias).parent_type).split('__').last()[0].is_capital()
		// Do not generate operator overloading with these `right_sym.kind`.
		not_exception := right_sym.kind !in [.voidptr, .int_literal, .int]
		if node.op in [.plus, .minus, .mul, .div, .mod, .lt, .eq]
			&& ((is_v_struct && !is_alias && not_exception) || is_c_alias
			|| is_struct_alias) {
			// Overloaded operators
			the_left_type := if !is_struct_alias
				|| g.table.get_type_symbol((left_sym.info as ast.Alias).parent_type).kind in [.array, .array_fixed, .map] {
				left_type
			} else {
				(left_sym.info as ast.Alias).parent_type
			}
			left_nr_muls := '*'.repeat(the_left_type.nr_muls())
			g.write(g.typ(the_left_type.set_nr_muls(0)))
			g.write('_')
			g.write(util.replace_op(node.op.str()))
			g.write('($left_nr_muls')
			g.expr(node.left)
			right_nr_muls := '*'.repeat(node.right_type.nr_muls())
			g.write(', $right_nr_muls')
			g.expr(node.right)
			g.write(')')
		} else if node.op in [.ne, .gt, .ge, .le] && ((is_v_struct && !is_alias && not_exception)
			|| is_c_alias || is_struct_alias) {
			the_left_type := if !is_struct_alias {
				left_type
			} else {
				(left_sym.info as ast.Alias).parent_type
			}
			nr_muls := '*'.repeat(the_left_type.nr_muls())
			typ := g.typ(the_left_type.set_nr_muls(0))
			if node.op == .gt {
				g.write('$typ')
			} else {
				g.write('!$typ')
			}
			g.write('_')
			if node.op == .ne {
				g.write('_eq')
			} else if node.op in [.ge, .le, .gt] {
				g.write('_lt')
			}
			if node.op in [.le, .gt] {
				g.write('($nr_muls')
				g.expr(node.right)
				g.write(', $nr_muls')
				g.expr(node.left)
				g.write(')')
			} else {
				g.write('($nr_muls')
				g.expr(node.left)
				g.write(', $nr_muls')
				g.expr(node.right)
				g.write(')')
			}
		} else {
			need_par := node.op in [.amp, .pipe, .xor] // `x & y == 0` => `(x & y) == 0` in C
			if need_par {
				g.write('(')
			}
			if node.left_type.is_ptr() && node.left.is_auto_deref_var() {
				g.write('*')
			}
			g.expr(node.left)
			g.write(' $node.op.str() ')
			g.expr_with_cast(node.right, node.right_type, node.left_type)
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

fn (mut g Gen) infix_gen_equality(node ast.InfixExpr, left_type ast.Type, left_sym ast.TypeSymbol, right_sym ast.TypeSymbol) {
	if left_sym.kind != right_sym.kind {
		return
	}
	match left_sym.kind {
		.array {
			ptr_typ := g.gen_array_equality_fn(left_type.clear_flag(.shared_f))
			if node.op == .ne {
				g.write('!')
			}
			g.write('${ptr_typ}_arr_eq(')
			if node.left_type.is_ptr() && !node.left_type.has_flag(.shared_f) {
				g.write('*')
			}
			g.expr(node.left)
			if node.left_type.has_flag(.shared_f) {
				if node.left_type.is_ptr() {
					g.write('->val')
				} else {
					g.write('.val')
				}
			}
			g.write(', ')
			if node.right_type.is_ptr() && !node.right_type.has_flag(.shared_f) {
				g.write('*')
			}
			g.expr(node.right)
			if node.right_type.has_flag(.shared_f) {
				if node.right_type.is_ptr() {
					g.write('->val')
				} else {
					g.write('.val')
				}
			}
			g.write(')')
		}
		.array_fixed {
			ptr_typ := g.gen_fixed_array_equality_fn(left_type)
			if node.op == .ne {
				g.write('!')
			}
			g.write('${ptr_typ}_arr_eq(')
			if node.left_type.is_ptr() {
				g.write('*')
			}
			if node.left is ast.ArrayInit {
				s := g.typ(left_type)
				g.write('($s)')
			}
			g.expr(node.left)
			g.write(', ')
			if node.right is ast.ArrayInit {
				s := g.typ(left_type)
				g.write('($s)')
			}
			g.expr(node.right)
			g.write(')')
		}
		.alias {
			ptr_typ := g.gen_alias_equality_fn(left_type)
			if node.op == .eq {
				g.write('${ptr_typ}_alias_eq(')
			} else if node.op == .ne {
				g.write('!${ptr_typ}_alias_eq(')
			}
			if node.left_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.left)
			g.write(', ')
			if node.right_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.right)
			g.write(')')
		}
		.map {
			ptr_typ := g.gen_map_equality_fn(left_type)
			if node.op == .eq {
				g.write('${ptr_typ}_map_eq(')
			} else if node.op == .ne {
				g.write('!${ptr_typ}_map_eq(')
			}
			if node.left_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.left)
			g.write(', ')
			if node.right_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.right)
			g.write(')')
		}
		.string, .struct_ {
			// Auto generate both `==` and `!=`
			ptr_typ := g.gen_struct_equality_fn(left_type)
			if node.op == .eq {
				g.write('${ptr_typ}_struct_eq(')
			} else if node.op == .ne {
				g.write('!${ptr_typ}_struct_eq(')
			}
			if node.left_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.left)
			g.write(', ')
			if node.right_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.right)
			g.write(')')
		}
		.sum_type {
			ptr_typ := g.gen_sumtype_equality_fn(left_type)
			if node.op == .eq {
				g.write('${ptr_typ}_sumtype_eq(')
			} else if node.op == .ne {
				g.write('!${ptr_typ}_sumtype_eq(')
			}
			if node.left_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.left)
			g.write(', ')
			if node.right_type.is_ptr() {
				g.write('*')
			}
			g.expr(node.right)
			g.write(')')
		}
		else {}
	}
}

fn (mut g Gen) infix_in_or_not_in(node ast.InfixExpr, left_sym ast.TypeSymbol, right_sym ast.TypeSymbol) {
	if node.op == .not_in {
		g.write('!')
	}
	if right_sym.kind == .array {
		if mut node.right is ast.ArrayInit {
			if node.right.exprs.len > 0 {
				// `a in [1,2,3]` optimization => `a == 1 || a == 2 || a == 3`
				// avoids an allocation
				// g.write('/*in opt*/')
				g.write('(')
				g.in_optimization(node.left, node.right)
				g.write(')')
				return
			}
		}
		fn_name := g.gen_array_contains_method(node.right_type)
		g.write('(${fn_name}(')
		if node.right_type.is_ptr() {
			g.write('*')
		}
		g.expr(node.right)
		g.write(', ')
		g.expr(node.left)
		g.write('))')
		return
	} else if right_sym.kind == .map {
		g.write('_IN_MAP(')
		if !node.left_type.is_ptr() {
			styp := g.typ(node.left_type)
			g.write('ADDR($styp, ')
			g.expr(node.left)
			g.write(')')
		} else {
			g.expr(node.left)
		}
		g.write(', ')
		if !node.right_type.is_ptr() {
			g.write('ADDR(map, ')
			g.expr(node.right)
			g.write(')')
		} else {
			g.expr(node.right)
		}
		g.write(')')
	} else if right_sym.kind == .string {
		g.write('string_contains(')
		g.expr(node.right)
		g.write(', ')
		g.expr(node.left)
		g.write(')')
	}
}

// `a in [1,2,3]` => `a == 1 || a == 2 || a == 3`
fn (mut g Gen) in_optimization(left ast.Expr, right ast.ArrayInit) {
	is_str := right.elem_type == ast.string_type
	elem_sym := g.table.get_type_symbol(right.elem_type)
	is_array := elem_sym.kind == .array
	for i, array_expr in right.exprs {
		if is_str {
			g.write('string__eq(')
		} else if is_array {
			ptr_typ := g.gen_array_equality_fn(right.elem_type)
			g.write('${ptr_typ}_arr_eq(')
		}
		g.expr(left)
		if is_str || is_array {
			g.write(', ')
		} else {
			g.write(' == ')
		}
		g.expr(array_expr)
		if is_str || is_array {
			g.write(')')
		}
		if i < right.exprs.len - 1 {
			g.write(' || ')
		}
	}
}

fn (mut g Gen) is_expr(node ast.InfixExpr) {
	eq := if node.op == .key_is { '==' } else { '!=' }
	g.write('(')
	g.expr(node.left)
	g.write(')')
	if node.left_type.is_ptr() {
		g.write('->')
	} else {
		g.write('.')
	}
	sym := g.table.get_type_symbol(node.left_type)
	if sym.kind == .interface_ {
		g.write('_typ $eq ')
		// `_Animal_Dog_index`
		sub_type := match mut node.right {
			ast.TypeNode { node.right.typ }
			ast.None { g.table.type_idxs['None__'] }
			else { ast.Type(0) }
		}
		sub_sym := g.table.get_type_symbol(sub_type)
		g.write('_${c_name(sym.name)}_${c_name(sub_sym.name)}_index')
		return
	} else if sym.kind == .sum_type {
		g.write('_typ $eq ')
	}
	g.expr(node.right)
}
