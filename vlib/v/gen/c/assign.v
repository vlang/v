// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util
import v.token

fn (mut g Gen) expr_with_opt_or_block(expr ast.Expr, expr_typ ast.Type, var_expr ast.Expr, ret_typ ast.Type,
	in_heap bool) {
	gen_or := expr is ast.Ident && expr.or_expr.kind != .absent
	if gen_or {
		old_inside_opt_or_res := g.inside_opt_or_res
		g.inside_opt_or_res = true
		g.expr_with_cast(expr, expr_typ, ret_typ)
		if in_heap {
			g.write('))')
		}
		g.writeln(';')
		expr_var := if expr is ast.Ident && expr.kind == .constant {
			g.get_const_name(expr)
		} else if expr is ast.Ident && expr.is_auto_heap() {
			'(*${expr.name})'
		} else {
			'${expr}'
		}
		dot_or_ptr := if !expr_typ.has_flag(.option_mut_param_t) { '.' } else { '-> ' }
		g.writeln('if (${c_name(expr_var)}${dot_or_ptr}state != 0) { // assign')
		if expr is ast.Ident && expr.or_expr.kind == .propagate_option {
			g.writeln('\tpanic_option_not_set(_S("none"));')
		} else {
			g.inside_or_block = true
			defer {
				g.inside_or_block = false
			}
			stmts := (expr as ast.Ident).or_expr.stmts
			// handles stmt block which returns something
			// e.g. { return none }
			if stmts.len > 0 && stmts.last() is ast.ExprStmt && stmts.last().typ != ast.void_type {
				g.gen_or_block_stmts(c_name(var_expr.str()), '', stmts, ret_typ, false)
			} else {
				// handles stmt block which doesn't returns value
				// e.g. { return }
				g.stmts(stmts)
				if stmts.len > 0 && stmts.last() is ast.ExprStmt {
					g.writeln(';')
				}
			}
		}
		g.writeln('}')
		g.inside_opt_or_res = old_inside_opt_or_res
	} else {
		g.expr_with_opt(expr, expr_typ, ret_typ)
	}
}

// expr_opt_with_alias handles conversion from different option alias type name
fn (mut g Gen) expr_opt_with_alias(expr ast.Expr, expr_typ ast.Type, ret_typ ast.Type) string {
	styp := g.base_type(ret_typ)

	line := g.go_before_last_stmt().trim_space()
	g.empty_line = true

	ret_var := g.new_tmp_var()
	ret_styp := g.styp(ret_typ).replace('*', '_ptr')
	g.writeln('${ret_styp} ${ret_var} = {.state=2, .err=_const_none__, .data={E_STRUCT}};')

	if expr !is ast.None {
		is_option_expr := expr_typ.has_flag(.option)
		if is_option_expr {
			g.write('_option_clone((${option_name}*)')
		} else {
			g.write('_option_ok(&(${styp}[]){ ')
		}
		has_addr := is_option_expr && expr !in [ast.Ident, ast.SelectorExpr]
		if has_addr {
			expr_styp := g.styp(expr_typ).replace('*', '_ptr')
			g.write('ADDR(${expr_styp}, ')
		} else if is_option_expr {
			g.write('&')
		}
		g.expr(expr)
		if has_addr {
			g.write(')')
		}
		if !is_option_expr {
			g.write(' }')
		}
		g.writeln(', (${option_name}*)&${ret_var}, sizeof(${styp}));')
	}
	g.write(line)
	if g.inside_return {
		g.write(' ')
	}
	g.write(ret_var)
	return ret_var
}

// expr_opt_with_cast is used in cast expr when converting compatible option types
// e.g. ?int(?u8(0))
fn (mut g Gen) expr_opt_with_cast(expr ast.Expr, expr_typ ast.Type, ret_typ ast.Type) string {
	if !expr_typ.has_flag(.option) || !ret_typ.has_flag(.option) {
		panic('cgen: expected expr_type and ret_typ to be options')
	}

	if expr_typ.idx() == ret_typ.idx() && g.table.sym(expr_typ).kind != .alias {
		return g.expr_with_opt(expr, expr_typ, ret_typ)
	} else {
		if expr is ast.CallExpr && expr.return_type.has_flag(.option) {
			return g.expr_opt_with_alias(expr, expr_typ, ret_typ)
		} else {
			past := g.past_tmp_var_new()
			defer {
				g.past_tmp_var_done(past)
			}
			styp := g.base_type(ret_typ)
			decl_styp := g.styp(ret_typ).replace('*', '_ptr')
			g.writeln('${decl_styp} ${past.tmp_var};')
			is_none := expr is ast.CastExpr && expr.expr is ast.None
			if is_none {
				g.write('_option_none(&(${styp}[]) {')
			} else {
				g.write('_option_ok(&(${styp}[]) {')
			}
			if expr is ast.CastExpr && expr_typ.has_flag(.option) {
				ret_sym := g.table.sym(ret_typ)
				if ret_sym.kind == .sum_type {
					exp_sym := g.table.sym(expr_typ)
					fname := g.get_sumtype_casting_fn(expr_typ, ret_typ)
					g.call_cfn_for_casting_expr(fname, expr, ret_typ, expr_typ, ret_sym.cname,
						expr_typ.is_ptr(), exp_sym.kind == .function, g.styp(expr_typ))
				} else {
					g.write('*((${g.base_type(expr_typ)}*)')
					g.expr(expr)
					g.write('.data)')
				}
			} else {
				old_inside_opt_or_res := g.inside_opt_or_res
				g.inside_opt_or_res = false
				g.expr_with_cast(expr, expr_typ, ret_typ)
				g.inside_opt_or_res = old_inside_opt_or_res
			}
			g.writeln(' }, (${option_name}*)(&${past.tmp_var}), sizeof(${styp}));')
			return past.tmp_var
		}
	}
}

// expr_with_opt is used in assigning an expression to an `option` variable
// e.g. x = y (option lhs and rhs), mut x = ?int(123), y = none
fn (mut g Gen) expr_with_opt(expr ast.Expr, expr_typ ast.Type, ret_typ ast.Type) string {
	old_inside_opt_or_res := g.inside_opt_or_res
	g.inside_opt_or_res = true
	defer {
		g.inside_opt_or_res = old_inside_opt_or_res
	}
	if expr_typ.has_flag(.option) && ret_typ.has_flag(.option) && !g.is_arraymap_set
		&& expr in [ast.SelectorExpr, ast.DumpExpr, ast.Ident, ast.ComptimeSelector, ast.AsCast, ast.CallExpr, ast.MatchExpr, ast.IfExpr, ast.IndexExpr, ast.UnsafeExpr, ast.CastExpr] {
		if expr in [ast.Ident, ast.CastExpr] {
			if expr_typ.idx() != ret_typ.idx() {
				return g.expr_opt_with_cast(expr, expr_typ, ret_typ)
			}
		}
		g.expr(expr)
		if expr is ast.ComptimeSelector {
			return g.gen_comptime_selector(expr)
		} else {
			return expr.str()
		}
	} else {
		tmp_out_var := g.new_tmp_var()
		g.expr_with_tmp_var(expr, expr_typ, ret_typ, tmp_out_var)
		return tmp_out_var
	}
	return ''
}

fn (mut g Gen) assign_stmt(node_ ast.AssignStmt) {
	mut node := unsafe { node_ }
	if node.is_static {
		is_defer_var := node.left[0] is ast.Ident && node.left[0].name in g.defer_vars
		if is_defer_var && node.op == .decl_assign {
			return
		}
		if !is_defer_var {
			g.write('static ')
		}
	}
	if node.is_volatile && node.left[0] is ast.Ident && node.left[0].name !in g.defer_vars {
		g.write('volatile ')
	}
	mut return_type := ast.void_type
	is_decl := node.op == .decl_assign
	g.assign_op = node.op
	g.inside_assign = true
	g.assign_ct_type = 0
	g.arraymap_set_pos = 0
	g.is_arraymap_set = false
	g.is_assign_lhs = false
	g.is_shared = false
	defer {
		g.assign_op = .unknown
		g.inside_assign = false
		g.assign_ct_type = 0
		g.arraymap_set_pos = 0
		g.is_arraymap_set = false
		g.is_assign_lhs = false
		g.is_shared = false
	}
	op := if is_decl { token.Kind.assign } else { node.op }
	right_expr := node.right[0]
	match right_expr {
		ast.CallExpr { return_type = right_expr.return_type }
		ast.LockExpr { return_type = right_expr.typ }
		ast.MatchExpr { return_type = right_expr.return_type }
		ast.IfExpr { return_type = right_expr.typ }
		else {}
	}
	// Free the old value assigned to this string var (only if it's `str = [new value]`
	// or `x.str = [new value]` )
	mut af := g.is_autofree && !g.is_builtin_mod && node.op == .assign && node.left_types.len == 1
		&& node.left[0] in [ast.Ident, ast.SelectorExpr]
	mut sref_name := ''
	mut type_to_free := ''
	if af {
		first_left_type := node.left_types[0]
		first_left_sym := g.table.sym(node.left_types[0])
		if first_left_type == ast.string_type || first_left_sym.kind == .array {
			type_to_free = if first_left_type == ast.string_type { 'string' } else { 'array' }
			mut ok := true
			left0 := node.left[0]
			if left0 is ast.Ident {
				if left0.name == '_' {
					ok = false
				}
			}
			if ok {
				sref_name = '_sref${node.pos.pos}'
				g.write('${type_to_free} ${sref_name} = (') // TODO: we are copying the entire string here, optimize
				// we can't just do `.str` since we need the extra data from the string struct
				// doing `&string` is also not an option since the stack memory with the data will be overwritten
				g.expr(left0) // node.left[0])
				if first_left_type.has_flag(.shared_f) {
					g.write('->val')
				}
				g.writeln('); // free ${type_to_free} on re-assignment2')
				defer {
					if af {
						g.writeln('${type_to_free}_free(&${sref_name});')
					}
				}
			} else {
				af = false
			}
		} else {
			af = false
		}
	}
	// TODO: g.gen_assign_vars_autofree(node)
	// json_test failed w/o this check
	if return_type != ast.void_type && return_type != 0 {
		sym := g.table.sym(return_type)
		if sym.kind == .multi_return {
			g.gen_multi_return_assign(node, return_type, sym)
			return
		}
	}
	// TODO: non idents on left (exprs)
	if node.has_cross_var {
		g.gen_cross_var_assign(node)
	}
	// `a := 1` | `a,b := 1,2`
	if node.right.len < node.left.len {
		g.checker_bug('node.right.len < node.left.len', node.pos)
	}
	if node.right_types.len < node.left.len {
		g.checker_bug('node.right_types.len < node.left.len', node.pos)
	}
	if node.left_types.len < node.left.len {
		g.checker_bug('node.left_types.len < node.left.len', node.pos)
	}

	last_curr_var_name := g.curr_var_name.clone()
	defer {
		g.curr_var_name = last_curr_var_name
	}
	g.curr_var_name = []

	for i, mut left in node.left {
		mut is_auto_heap := false
		mut var_type := node.left_types[i]
		mut val_type := node.right_types[i]
		val := node.right[i]
		mut is_call := false
		mut gen_or := false
		mut blank_assign := false
		mut is_va_list := false // C varargs
		mut ident := ast.Ident{
			scope: unsafe { nil }
		}
		mut cur_indexexpr := -1
		left_sym := g.table.sym(g.unwrap_generic(var_type))
		is_va_list = left_sym.language == .c && left_sym.name == 'C.va_list'
		if mut left is ast.Ident {
			ident = left
			g.curr_var_name << ident.name
			// id_info := ident.var_info()
			// var_type = id_info.typ
			blank_assign = left.kind == .blank_ident
			// TODO: temporary, remove this
			left_info := left.info
			if left_info is ast.IdentVar {
				share := left_info.share
				if share == .shared_t {
					var_type = var_type.set_flag(.shared_f)
				}
				if share == .atomic_t {
					var_type = var_type.set_flag(.atomic_f)
				}
			}
			if mut left.obj is ast.Var {
				if is_decl {
					if val is ast.Ident && val.ct_expr {
						ctyp := g.unwrap_generic(g.type_resolver.get_type(val))
						if ctyp != ast.void_type {
							var_type = ctyp
							val_type = var_type
							gen_or = val.or_expr.kind != .absent
							if gen_or {
								var_type = val_type.clear_flag(.option)
							}
							left.obj.typ = var_type
							g.assign_ct_type = var_type
						}
					} else if val is ast.ComptimeSelector {
						if val.typ_key != '' {
							if is_decl {
								var_type = g.type_resolver.get_ct_type_or_default(val.typ_key,
									var_type)
								val_type = var_type
								left.obj.typ = var_type
							} else {
								val_type = g.type_resolver.get_ct_type_or_default(val.typ_key,
									var_type)
							}
							g.assign_ct_type = var_type
						}
					} else if val is ast.ComptimeCall {
						key_str := '${val.method_name}.return_type'
						var_type = g.type_resolver.get_ct_type_or_default(key_str, var_type)
						left.obj.typ = var_type
						g.assign_ct_type = var_type
					} else if val is ast.Ident && val.info is ast.IdentVar {
						val_info := (val as ast.Ident).info as ast.IdentVar
						gen_or = val.or_expr.kind != .absent
						if val_info.is_option && gen_or {
							var_type = val_type.clear_flag(.option)
							left.obj.typ = var_type
						}
					} else if val is ast.DumpExpr {
						if val.expr is ast.ComptimeSelector {
							if val.expr.typ_key != '' {
								var_type = g.type_resolver.get_ct_type_or_default(val.expr.typ_key,
									var_type)
								val_type = var_type
								left.obj.typ = var_type
							}
							g.assign_ct_type = var_type
						}
					} else if val is ast.IndexExpr && (val.left is ast.Ident && val.left.ct_expr) {
						ctyp := g.unwrap_generic(g.type_resolver.get_type(val))
						if ctyp != ast.void_type {
							var_type = ctyp
							val_type = var_type
							left.obj.typ = var_type
							g.assign_ct_type = var_type
						}
					} else if left.obj.ct_type_var == .generic_var && val is ast.CallExpr {
						if val.return_type_generic != 0
							&& val.return_type_generic.has_flag(.generic) {
							fn_ret_type := g.resolve_return_type(val)
							if fn_ret_type != ast.void_type {
								var_type = fn_ret_type
								val_type = var_type
								left.obj.typ = var_type
							}
						} else if val.is_static_method && val.left_type.has_flag(.generic) {
							fn_ret_type := g.resolve_return_type(val)
							var_type = fn_ret_type
							val_type = var_type
							left.obj.typ = var_type
							g.assign_ct_type = var_type
						} else if val.left_type != 0 && g.table.type_kind(val.left_type) == .array
							&& val.name == 'map' && val.args.len > 0
							&& val.args[0].expr is ast.AsCast
							&& val.args[0].expr.typ.has_flag(.generic) {
							var_type = g.table.find_or_register_array(g.unwrap_generic((val.args[0].expr as ast.AsCast).typ))
							val_type = var_type
							left.obj.typ = var_type
							g.assign_ct_type = var_type
						}
					} else if val is ast.InfixExpr && val.op in [.plus, .minus, .mul, .div, .mod]
						&& val.left_ct_expr {
						ctyp := g.type_resolver.promote_type(g.unwrap_generic(g.type_resolver.get_type(val.left)),
							g.unwrap_generic(g.type_resolver.get_type_or_default(val.right,
							val.right_type)))
						if ctyp != ast.void_type {
							ct_type_var := g.comptime.get_ct_type_var(val.left)
							if ct_type_var in [.key_var, .value_var] {
								g.type_resolver.update_ct_type(left.name, g.unwrap_generic(ctyp))
							}
							var_type = ctyp
							val_type = var_type
							left.obj.typ = var_type
							g.assign_ct_type = var_type
						}
					} else if val is ast.PostfixExpr && val.op == .question
						&& (val.expr is ast.Ident && val.expr.ct_expr) {
						ctyp := g.unwrap_generic(g.type_resolver.get_type(val))
						if ctyp != ast.void_type {
							var_type = ctyp
							val_type = var_type
							left.obj.typ = var_type
							g.assign_ct_type = var_type

							ct_type_var := g.comptime.get_ct_type_var(val.expr)
							if ct_type_var == .field_var {
								g.type_resolver.update_ct_type(left.name, ctyp)
							}
						}
					}
				}
				is_auto_heap = left.obj.is_auto_heap
			}
		} else if mut left is ast.ComptimeSelector {
			if left.typ_key != '' {
				var_type = g.type_resolver.get_ct_type_or_default(left.typ_key, var_type)
			}
			g.assign_ct_type = var_type
			if val is ast.ComptimeSelector {
				if val.typ_key != '' {
					val_type = g.type_resolver.get_ct_type_or_default(val.typ_key, var_type)
				}
			} else if val is ast.CallExpr {
				g.assign_ct_type = g.comptime.comptime_for_field_type
			}
		} else if mut left is ast.IndexExpr && val is ast.ComptimeSelector {
			if val.typ_key != '' {
				val_type = g.type_resolver.get_ct_type_or_default(val.typ_key, var_type)
			}
			g.assign_ct_type = val_type
		}
		mut styp := g.styp(var_type)
		mut is_fixed_array_init := false
		mut has_val := false
		match val {
			ast.ArrayInit {
				is_fixed_array_init = val.is_fixed
				has_val = val.has_val
			}
			ast.CallExpr {
				is_call = true
				if val.comptime_ret_val {
					return_type = g.comptime.comptime_for_field_type
					styp = g.styp(return_type)
				} else {
					return_type = val.return_type
				}
			}
			// TODO: no buffer fiddling
			ast.AnonFn {
				if !var_type.has_option_or_result() {
					if blank_assign {
						g.write('{')
					}
					// if it's a decl assign (`:=`) or a blank assignment `_ =`/`_ :=` then generate `void (*ident) (args) =`
					if (is_decl || blank_assign) && left is ast.Ident {
						sig := g.fn_var_signature(val.decl.return_type, val.decl.params.map(it.typ),
							ident.name)
						g.write(sig + ' = ')
					} else {
						g.is_assign_lhs = true
						g.assign_op = node.op
						g.expr(left)
						g.is_assign_lhs = false
						g.is_arraymap_set = false
						if mut left is ast.IndexExpr {
							sym := g.table.final_sym(left.left_type)
							if sym.kind in [.map, .array] {
								g.expr(val)
								g.writeln('});')
								continue
							}
						}
						g.write(' = ')
					}
					g.expr(val)
					g.writeln(';')
					if blank_assign {
						g.write('}')
					}
					continue
				}
			}
			else {}
		}
		unwrapped_val_type := g.unwrap_generic(val_type)
		right_sym := g.table.sym(unwrapped_val_type)
		unaliased_right_sym := g.table.final_sym(unwrapped_val_type)
		is_fixed_array_var := !g.pref.translated && unaliased_right_sym.kind == .array_fixed
			&& val !is ast.ArrayInit
			&& (val in [ast.Ident, ast.IndexExpr, ast.CallExpr, ast.SelectorExpr, ast.DumpExpr, ast.InfixExpr]
			|| (val is ast.CastExpr && val.expr !is ast.ArrayInit)
			|| (val is ast.PrefixExpr && val.op == .arrow)
			|| (val is ast.UnsafeExpr && val.expr in [ast.SelectorExpr, ast.Ident, ast.CallExpr]))
		g.is_assign_lhs = true
		g.assign_op = node.op

		g.left_is_opt = var_type.has_option_or_result()
		g.right_is_opt = val_type.has_option_or_result()
		defer {
			g.left_is_opt = false
			g.right_is_opt = false
		}

		if blank_assign {
			if val is ast.IndexExpr {
				g.assign_op = .decl_assign
			}
			g.is_assign_lhs = false
			if is_call {
				old_is_void_expr_stmt := g.is_void_expr_stmt
				g.is_void_expr_stmt = true
				g.expr(val)
				g.is_void_expr_stmt = old_is_void_expr_stmt
			} else if g.inside_for_c_stmt {
				g.expr(val)
			} else if var_type.has_flag(.option) {
				g.expr_with_opt(val, val_type, var_type)
			} else {
				if left_sym.kind == .function {
					g.write('{void* _ = ')
				} else {
					g.write('{${styp} _ = ')
				}
				if val in [ast.MatchExpr, ast.IfExpr] && unaliased_right_sym.info is ast.ArrayFixed {
					tmp_var := g.expr_with_var(val, var_type, false)
					g.fixed_array_var_init(tmp_var, false, unaliased_right_sym.info.elem_type,
						unaliased_right_sym.info.size)
				} else {
					g.expr(val)
				}
				g.writeln(';}')
			}
		} else if node.op == .assign && !g.pref.translated && (is_fixed_array_init
			|| (unaliased_right_sym.kind == .array_fixed && val in [ast.Ident, ast.CastExpr])) {
			// Fixed arrays
			if is_fixed_array_init && var_type.has_flag(.option) {
				g.expr(left)
				g.write(' = ')
				g.expr_with_opt(val, val_type, var_type)
			} else if unaliased_right_sym.kind == .array_fixed && val is ast.CastExpr {
				if var_type.has_flag(.option) {
					g.expr(left)
					g.writeln('.state = 0;')
					g.write('memcpy(')
					g.expr(left)
					g.write('.data, ')
					g.expr(val)
					g.writeln(', sizeof(${g.styp(var_type.clear_flag(.option))}));')
				} else {
					g.write('memcpy(')
					g.expr(left)
					g.write(', ')
					g.expr(val)
					g.writeln(', sizeof(${g.styp(var_type)}));')
				}
			} else {
				mut v_var := ''
				arr_typ := styp.trim('*')
				if is_fixed_array_init {
					right := val as ast.ArrayInit
					v_var = g.new_tmp_var()
					g.write('${arr_typ} ${v_var} = ')
					g.expr(right)
					g.writeln(';')
				} else {
					right := val as ast.Ident
					v_var = right.name
				}
				pos := g.out.len
				g.expr(left)

				if g.is_arraymap_set && g.arraymap_set_pos >= 0 {
					if g.arraymap_set_pos > 0 {
						g.go_back_to(g.arraymap_set_pos)
					}
					g.write(', &${v_var})')
					g.is_arraymap_set = false
					g.arraymap_set_pos = 0
				} else {
					g.go_back_to(pos)
					is_var_mut := !is_decl && left.is_auto_deref_var()
					addr_left := if is_var_mut { '' } else { '&' }
					g.writeln('')
					g.write('memcpy(${addr_left}')
					g.expr(left)
					addr_val := if is_fixed_array_var { '' } else { '&' }
					g.writeln(', ${addr_val}${v_var}, sizeof(${arr_typ}));')
				}
				g.is_assign_lhs = false
			}
		} else {
			is_inside_ternary := g.inside_ternary != 0
			cur_line := if is_inside_ternary && is_decl {
				g.register_ternary_name(ident.name)
				g.empty_line = false
				g.go_before_ternary()
			} else {
				''
			}
			mut str_add := false
			mut op_overloaded := false
			mut op_expected_left := ast.no_type
			mut op_expected_right := ast.no_type
			is_shared_re_assign := !is_decl && node.left_types[i].has_flag(.shared_f)
				&& left is ast.Ident && left_sym.kind in [.array, .map, .struct]
			if node.op == .plus_assign && unaliased_right_sym.kind == .string {
				if mut left is ast.IndexExpr {
					if g.table.sym(left.left_type).kind == .array_fixed {
						// strs[0] += str2 => `strs[0] = string__plus(strs[0], str2)`
						g.expr(left)
						g.write(' = string__plus(')
					} else {
						// a[0] += str => `array_set(&a, 0, &(string[]) {string__plus(...))})`
						g.expr(left)
						g.write('string__plus(')
					}
				} else {
					// allow literal values to auto deref var (e.g.`for mut v in values { v += 1.0 }`)
					if left.is_auto_deref_var() {
						g.write('*')
					}
					// str += str2 => `str = string__plus(str, str2)`
					g.expr(left)
					g.write(' = string__plus(')
				}
				g.is_assign_lhs = false
				str_add = true
			}
			// Assignment Operator Overloading
			if ((left_sym.kind == .struct && right_sym.kind == .struct)
				|| (left_sym.kind == .alias && right_sym.kind == .alias))
				&& node.op in [.plus_assign, .minus_assign, .div_assign, .mult_assign, .mod_assign] {
				extracted_op := match node.op {
					.plus_assign { '+' }
					.minus_assign { '-' }
					.div_assign { '/' }
					.mod_assign { '%' }
					.mult_assign { '*' }
					else { 'unknown op' }
				}
				pos := g.out.len
				g.expr(left)
				if left_sym.info is ast.Struct && left_sym.info.generic_types.len > 0 {
					concrete_types := left_sym.info.concrete_types
					mut method_name := left_sym.cname + '_' + util.replace_op(extracted_op)
					method_name = g.generic_fn_name(concrete_types, method_name)
					g.write(' = ${method_name}(')
					g.expr(left)
					g.write(', ')
					g.expr(val)
					g.writeln(');')
					return
				} else if left_sym.kind == .alias
					&& g.table.final_sym(g.unwrap_generic(var_type)).is_number()
					&& !left_sym.has_method(extracted_op) {
					g.write(' = ')
					g.expr(left)
					g.write(' ${extracted_op} ')
					g.expr(val)
					if !g.inside_for_c_stmt {
						g.write(';')
					}
					return
				} else if left_sym.kind == .alias && g.table.final_sym(var_type).kind == .struct {
					struct_info := g.table.final_sym(var_type)
					if struct_info.info is ast.Struct && struct_info.info.generic_types.len > 0 {
						mut method_name := struct_info.cname + '_' + util.replace_op(extracted_op)
						method_name = g.generic_fn_name(struct_info.info.concrete_types,
							method_name)
						g.write(' = ${method_name}(')
						g.expr(left)
						g.write(', ')
						g.expr(val)
						g.writeln(');')
						return
					}
				} else {
					if g.table.final_sym(g.unwrap_generic(var_type)).kind == .array_fixed {
						g.go_back_to(pos)
						g.empty_line = true
						g.write('memcpy(')
						g.expr(left)
						g.write(', ${styp}_${util.replace_op(extracted_op)}(')
					} else {
						g.write(' = ${styp}_${util.replace_op(extracted_op)}(')
					}
					method := g.table.find_method(left_sym, extracted_op) or {
						// the checker will most likely have found this, already...
						g.error('assignment operator `${extracted_op}=` used but no `${extracted_op}` method defined',
							node.pos)
						ast.Fn{}
					}
					op_expected_left = method.params[0].typ
					op_expected_right = method.params[1].typ
					op_overloaded = true
				}
			}
			final_left_sym := g.table.final_sym(g.unwrap_generic(var_type))
			final_right_sym := g.table.final_sym(unwrapped_val_type)
			mut aligned := 0
			if final_left_sym.info is ast.Struct {
				if attr := final_left_sym.info.attrs.find_first('aligned') {
					aligned = if attr.arg == '' { 0 } else { attr.arg.int() }
				}
			}

			if final_left_sym.kind == .bool && final_right_sym.kind == .bool
				&& node.op in [.boolean_or_assign, .boolean_and_assign] {
				extracted_op := match node.op {
					.boolean_or_assign {
						'||'
					}
					.boolean_and_assign {
						'&&'
					}
					else {
						'unknown op'
					}
				}
				g.expr(left)
				g.write(' = ')
				g.expr(left)
				g.write(' ${extracted_op} ')
				g.expr(val)
				g.writeln(';')
				return
			}
			if right_sym.info is ast.FnType && is_decl {
				if is_inside_ternary {
					g.out.write_string(util.tabs(g.indent - g.inside_ternary))
				}
				fn_name := c_fn_name(g.get_ternary_name(ident.name))

				if val_type.has_flag(.option) {
					ret_styp := g.styp(g.unwrap_generic(val_type))
					g.write('${ret_styp} ${fn_name}')
				} else {
					ret_styp := g.styp(right_sym.info.func.return_type)
					mut call_conv := ''
					mut msvc_call_conv := ''
					for attr in right_sym.info.func.attrs {
						match attr.name {
							'callconv' {
								if g.is_cc_msvc {
									msvc_call_conv = '__${attr.arg} '
								} else {
									call_conv = '${attr.arg}'
								}
							}
							else {}
						}
					}
					call_conv_attribute_suffix := if call_conv.len != 0 {
						'__attribute__((${call_conv}))'
					} else {
						''
					}
					g.write('${ret_styp} (${msvc_call_conv}*${fn_name}) (')
					def_pos := g.definitions.len
					g.fn_decl_params(right_sym.info.func.params, unsafe { nil }, false,
						false)
					g.definitions.go_back(g.definitions.len - def_pos)
					g.write(')${call_conv_attribute_suffix}')
				}
			} else {
				if is_decl {
					if is_inside_ternary {
						g.out.write_string(util.tabs(g.indent - g.inside_ternary))
					}
					mut is_used_var_styp := false
					if ident.name !in g.defer_vars {
						val_sym := g.table.sym(val_type)
						if val_sym.info is ast.Struct && val_sym.info.generic_types.len > 0 {
							if val is ast.StructInit {
								var_styp := g.styp(val.typ)
								if var_type.has_flag(.shared_f) {
									g.write('__shared__${var_styp}* ')
								} else {
									g.write('${var_styp} ')
								}
								is_used_var_styp = true
							} else if val is ast.PrefixExpr {
								if val.op == .amp && val.right is ast.StructInit {
									var_styp := g.styp(val.right.typ.ref())
									if var_type.has_flag(.shared_f) {
										g.write('__shared__')
									}
									g.write('${var_styp} ')
									is_used_var_styp = true
								}
							}
						}
						if !is_used_var_styp {
							if !val_type.has_flag(.option) && left_sym.is_array_fixed() {
								if left_sym.info is ast.Alias {
									parent_sym := g.table.final_sym(left_sym.info.parent_type)
									styp = g.styp(left_sym.info.parent_type)
									if !parent_sym.is_array_fixed_ret() {
										g.write('${styp} ')
									} else {
										g.write('${styp[3..]} ')
									}
								} else {
									if !left_sym.is_array_fixed_ret() {
										g.write('${styp} ')
									} else {
										g.write('${styp[3..]} ')
									}
								}
							} else {
								g.write('${styp} ')
							}
						}
						if is_auto_heap && !(val_type.is_ptr() && val_type.has_flag(.option)) {
							g.write('*')
						}
					}
				}
				if left in [ast.Ident, ast.SelectorExpr] {
					g.prevent_sum_type_unwrapping_once = true
				}
				if !is_fixed_array_var || is_decl || is_shared_re_assign {
					if op_overloaded {
						g.op_arg(left, op_expected_left, var_type)
					} else {
						if !is_decl && !is_shared_re_assign && left.is_auto_deref_var()
							&& !var_type.has_flag(.option) {
							g.write('*')
						}
						if node_.op == .assign && var_type.has_flag(.option_mut_param_t) {
							g.write('memcpy(&')
							g.expr(left)
							g.write('->data, *(${g.styp(val_type)}**)&')
						} else if var_type.has_flag(.option_mut_param_t) {
							g.expr(left)
							g.write(' = ')
						} else {
							g.expr(left)
						}
						if !is_decl && var_type.has_flag(.shared_f) {
							g.write('->val') // don't reset the mutex, just change the value
						}
					}
				}
			}
			if is_inside_ternary && is_decl {
				g.write(';\n${cur_line}')
				g.out.write_string(util.tabs(g.indent))
				g.expr(left)
			}
			g.is_assign_lhs = false
			if left is ast.IndexExpr && g.cur_indexexpr.len > 0 {
				cur_indexexpr = g.cur_indexexpr.index(left.pos().pos)
			}
			if is_fixed_array_var || is_va_list {
				if is_decl {
					g.writeln(';')
					if is_va_list {
						continue
					}
				}
			} else if !var_type.has_flag(.option_mut_param_t) && cur_indexexpr == -1 && !str_add
				&& !op_overloaded {
				g.write(' ${op} ')
			} else if str_add || op_overloaded {
				g.write(', ')
			}
			mut cloned := false
			if g.is_autofree {
				if right_sym.kind in [.array, .string] && !unwrapped_val_type.has_flag(.shared_f) {
					if g.gen_clone_assignment(var_type, val, unwrapped_val_type, false) {
						cloned = true
					}
				} else if right_sym.info is ast.Interface && var_type != ast.error_type {
					g.register_free_method(var_type)
				}
			}
			if !cloned {
				if g.comptime.comptime_for_field_var == ''
					&& ((var_type.has_flag(.option) && !val_type.has_flag(.option))
					|| (var_type.has_flag(.result) && !val_type.has_flag(.result))) {
					old_inside_opt_or_res := g.inside_opt_or_res
					defer {
						g.inside_opt_or_res = old_inside_opt_or_res
					}
					g.inside_opt_or_res = true
					if is_auto_heap && var_type.has_flag(.option) {
						g.write('&')
					}
					tmp_var := g.new_tmp_var()
					g.expr_with_tmp_var(val, val_type, var_type, tmp_var)
				} else if is_fixed_array_var {
					// TODO: Instead of the translated check, check if it's a pointer already
					// and don't generate memcpy &
					typ_str := g.styp(val_type).trim('*')
					final_typ_str := if is_fixed_array_var { '' } else { '(${typ_str}*)' }
					final_ref_str := if is_fixed_array_var {
						''
					} else if val_type.is_ptr() {
						'(byte*)'
					} else {
						'(byte*)&'
					}
					if val_type.has_flag(.option) {
						g.expr(left)
						g.write(' = ')
						g.expr(val)
					} else {
						if op_overloaded {
							g.expr(left)
							g.write(', ')
							g.expr(val)
							g.write(').ret_arr, sizeof(${typ_str})')
						} else {
							g.write('memcpy(${final_typ_str}')
							g.expr(left)
							g.write(', ${final_ref_str}')
							g.expr(val)
							g.write(', sizeof(${typ_str}))')
						}
					}
				} else if is_decl {
					g.is_shared = var_type.has_flag(.shared_f)
					if is_fixed_array_init && !has_val {
						if val is ast.ArrayInit {
							g.array_init(val, c_name(ident.name))
						} else {
							g.write('{0}')
						}
					} else {
						is_option_unwrapped := val is ast.Ident && val.or_expr.kind != .absent
						is_option_auto_heap := is_auto_heap && is_option_unwrapped
						if is_auto_heap {
							if aligned != 0 {
								g.write('HEAP_align(${styp}, (')
							} else {
								g.write('HEAP(${styp}, (')
							}
						}
						if val.is_auto_deref_var() && !is_option_unwrapped {
							g.write('*')
						}
						if (var_type.has_flag(.option) && val !in [ast.Ident, ast.SelectorExpr])
							|| gen_or {
							g.expr_with_opt_or_block(val, val_type, left, var_type, is_option_auto_heap)
						} else if val is ast.ArrayInit {
							cvar_name := c_name(ident.name)
							if val.is_fixed && ident.name in g.defer_vars {
								g.go_before_last_stmt()
								g.empty_line = true
								g.write('memcpy(${cvar_name}, ')
								g.write('(${styp})')
								g.array_init(val, cvar_name)
								g.write(', sizeof(${styp}))')
							} else {
								g.array_init(val, cvar_name)
							}
						} else if val_type.has_flag(.shared_f) {
							g.expr_with_cast(val, val_type, var_type)
						} else if val in [ast.MatchExpr, ast.IfExpr]
							&& unaliased_right_sym.info is ast.ArrayFixed {
							tmp_var := g.expr_with_var(val, var_type, false)
							g.fixed_array_var_init(tmp_var, false, unaliased_right_sym.info.elem_type,
								unaliased_right_sym.info.size)
						} else {
							g.expr(val)
						}
						if is_auto_heap && !is_option_auto_heap {
							if aligned != 0 {
								g.write('), ${aligned})')
							} else {
								g.write('))')
							}
						}
					}
				} else {
					// var = &auto_heap_var
					old_is_auto_heap := g.is_option_auto_heap
					defer {
						g.is_option_auto_heap = old_is_auto_heap
					}
					if val is ast.Ident && val.is_mut() && var_type.is_ptr() {
						if var_type.nr_muls() < val_type.nr_muls() {
							g.write('*'.repeat(var_type.nr_muls()))
						}
					}
					g.is_option_auto_heap = val_type.has_flag(.option) && val is ast.PrefixExpr
						&& val.right is ast.Ident && (val.right as ast.Ident).is_auto_heap()
					if var_type.has_flag(.option) || gen_or {
						g.expr_with_opt_or_block(val, val_type, left, var_type, false)
					} else if node.has_cross_var {
						g.gen_cross_tmp_variable(node.left, val)
					} else {
						if op_overloaded {
							g.op_arg(val, op_expected_right, val_type)
						} else {
							exp_type := if var_type.is_ptr()
								&& (left.is_auto_deref_var() || var_type.has_flag(.shared_f)) {
								var_type.deref()
							} else {
								var_type
							}.clear_flag(.shared_f) // don't reset the mutex, just change the value
							g.expr_with_cast(val, val_type, exp_type)
						}
					}
				}
			}
			if str_add || op_overloaded {
				g.write(')')
			}
			if node_.op == .assign && var_type.has_flag(.option_mut_param_t) {
				g.write('.data, sizeof(${g.base_type(val_type)}))')
			}
			if cur_indexexpr != -1 {
				g.cur_indexexpr.delete(cur_indexexpr)
				g.write(' })')
				g.is_arraymap_set = g.cur_indexexpr.len > 0
			}
			g.is_shared = false
		}
		g.right_is_opt = false
		if g.inside_ternary == 0 && (node.left.len > 1 || !node.is_simple) {
			g.writeln(';')
		}
	}
}

fn (mut g Gen) gen_multi_return_assign(node &ast.AssignStmt, return_type ast.Type, return_sym ast.TypeSymbol) {
	// multi return
	// TODO: Handle in if_expr
	mr_var_name := 'mr_${node.pos.pos}'
	mut is_option := return_type.has_flag(.option)
	mut mr_styp := g.styp(return_type.clear_flag(.result))
	if node.right[0] is ast.CallExpr && node.right[0].or_block.kind != .absent {
		is_option = false
		mr_styp = g.styp(return_type.clear_option_and_result())
	}
	g.write('${mr_styp} ${mr_var_name} = ')
	g.expr(node.right[0])
	g.writeln(';')
	mr_types := (return_sym.info as ast.MultiReturn).types
	for i, lx in node.left {
		mut cur_indexexpr := -1
		mut is_auto_heap := false
		mut ident := ast.Ident{
			scope: unsafe { nil }
		}
		if lx is ast.Ident {
			ident = lx
			if lx.kind == .blank_ident {
				continue
			}
			if lx.obj is ast.Var {
				is_auto_heap = lx.obj.is_auto_heap
			}
		}
		if lx is ast.IndexExpr && g.cur_indexexpr.len > 0 {
			cur_indexexpr = g.cur_indexexpr.index(lx.pos.pos)
		}
		styp := if ident.name in g.defer_vars { '' } else { g.styp(node.left_types[i]) }
		if node.op == .decl_assign {
			g.write('${styp} ')
		}
		if lx.is_auto_deref_var() {
			g.write('*')
		}
		noscan := if is_auto_heap { g.check_noscan(return_type) } else { '' }
		mut aligned := 0
		sym := g.table.final_sym(node.left_types[i])
		if sym.info is ast.Struct {
			if attr := sym.info.attrs.find_first('aligned') {
				aligned = if attr.arg == '' { 0 } else { attr.arg.int() }
			}
		}
		if node.left_types[i].has_flag(.option) {
			base_typ := g.base_type(node.left_types[i])
			tmp_var := if is_auto_heap {
				if aligned != 0 {
					'HEAP_align(${styp}, ${mr_var_name}.arg${i}, ${aligned})'
				} else {
					'HEAP${noscan}(${styp}, ${mr_var_name}.arg${i})'
				}
			} else if is_option {
				'(*((${g.base_type(return_type)}*)${mr_var_name}.data)).arg${i}'
			} else {
				'${mr_var_name}.arg${i}'
			}
			if mr_types[i].has_flag(.option) {
				old_left_is_opt := g.left_is_opt
				g.left_is_opt = true
				g.expr(lx)
				g.writeln(' = ${tmp_var};')
				g.left_is_opt = old_left_is_opt
			} else {
				g.write('_option_ok(&(${base_typ}[]) { ${tmp_var} }, (${option_name}*)(&')
				tmp_left_is_opt := g.left_is_opt
				g.left_is_opt = true
				g.expr(lx)
				g.left_is_opt = tmp_left_is_opt
				g.writeln('), sizeof(${base_typ}));')
			}
		} else {
			g.expr(lx)
			if sym.kind == .array_fixed {
				g.writeln2(';', 'memcpy(&${g.expr_string(lx)}, &${mr_var_name}.arg${i}, sizeof(${styp}));')
			} else {
				if cur_indexexpr != -1 {
					if is_auto_heap {
						if aligned != 0 {
							g.writeln('HEAP_align(${styp}, ${mr_var_name}.arg${i}, ${aligned}) });')
						} else {
							g.writeln('HEAP${noscan}(${styp}, ${mr_var_name}.arg${i}) });')
						}
					} else if is_option {
						g.writeln('(*((${g.base_type(return_type)}*)${mr_var_name}.data)).arg${i} });')
					} else {
						g.writeln('${mr_var_name}.arg${i} });')
					}
					g.cur_indexexpr.delete(cur_indexexpr)
				} else {
					if is_auto_heap {
						if aligned != 0 {
							g.writeln(' = HEAP_align(${styp}, ${mr_var_name}.arg${i}, ${aligned});')
						} else {
							g.writeln(' = HEAP${noscan}(${styp}, ${mr_var_name}.arg${i});')
						}
					} else if is_option {
						g.writeln(' = (*((${g.base_type(return_type)}*)${mr_var_name}.data)).arg${i};')
					} else {
						g.writeln(' = ${mr_var_name}.arg${i};')
					}
				}
			}
		}
	}
	if g.is_arraymap_set {
		g.is_arraymap_set = false
	}
}

fn (mut g Gen) gen_cross_var_assign(node &ast.AssignStmt) {
	for i, left in node.left {
		left_is_auto_deref_var := left.is_auto_deref_var()
		match left {
			ast.Ident {
				left_typ := node.left_types[i]
				left_sym := g.table.sym(left_typ)
				mut anon_ctx := ''
				if g.anon_fn {
					if obj := left.scope.find_var(left.name) {
						if obj.is_inherited {
							anon_ctx = '${closure_ctx}->'
						}
					}
				}
				if left_sym.info is ast.FnType {
					g.write_fn_ptr_decl(&left_sym.info, '_var_${left.pos.pos}')
					g.writeln(' = ${anon_ctx}${c_name(left.name)};')
				} else if left_is_auto_deref_var {
					styp := g.styp(left_typ).trim('*')
					if left_sym.kind == .array {
						g.writeln('${styp} _var_${left.pos.pos} = array_clone(${anon_ctx}${c_name(left.name)});')
					} else {
						g.writeln('${styp} _var_${left.pos.pos} = *${anon_ctx}${c_name(left.name)};')
					}
				} else {
					styp := g.styp(left_typ)
					if left_sym.kind == .array {
						g.writeln('${styp} _var_${left.pos.pos} = array_clone(&${anon_ctx}${c_name(left.name)});')
					} else {
						g.writeln('${styp} _var_${left.pos.pos} = ${anon_ctx}${c_name(left.name)};')
					}
				}
			}
			ast.IndexExpr {
				sym := g.table.sym(g.table.unaliased_type(left.left_type))
				if sym.kind == .array {
					info := sym.info as ast.Array
					elem_typ := g.table.sym(info.elem_type)
					needs_clone := info.elem_type == ast.string_type && g.is_autofree

					if elem_typ.kind == .function {
						left_typ := node.left_types[i]
						left_sym := g.table.sym(left_typ)
						g.write_fn_ptr_decl(left_sym.info as ast.FnType, '_var_${left.pos.pos}')
						g.write(' = *(voidptr*)array_get(')
					} else {
						styp := g.styp(info.elem_type)
						string_clone := if needs_clone { 'string_clone(' } else { '' }

						g.write('${styp} _var_${left.pos.pos} = ${string_clone}*(${styp}*)array_get(')
					}

					if left.left_type.is_ptr() {
						g.write('*')
					}
					g.expr(left.left)
					g.write(', ')
					g.expr(left.index)
					if needs_clone {
						g.write(')')
					}
					g.writeln(');')
				} else if sym.kind == .array_fixed {
					info := sym.info as ast.ArrayFixed
					elem_typ := g.table.sym(info.elem_type)
					if elem_typ.kind == .function {
						left_typ := node.left_types[i]
						left_sym := g.table.sym(left_typ)
						g.write_fn_ptr_decl(left_sym.info as ast.FnType, '_var_${left.pos.pos}')
						g.write(' = *(voidptr*)')
					} else {
						styp := g.styp(info.elem_type)
						g.write('${styp} _var_${left.pos.pos} = ')
					}
					if left.left_type.is_ptr() {
						g.write('*')
					}
					needs_clone := info.elem_type == ast.string_type && g.is_autofree
					if needs_clone {
						g.write('string_clone(')
					}
					g.expr(left)
					if needs_clone {
						g.write(')')
					}
					g.writeln(';')
				} else if sym.kind == .map {
					info := sym.info as ast.Map
					skeytyp := g.styp(info.key_type)
					styp := g.styp(info.value_type)
					zero := g.type_default(info.value_type)
					val_typ := g.table.sym(info.value_type)
					if val_typ.kind == .function {
						left_type := node.left_types[i]
						left_sym := g.table.sym(left_type)
						g.write_fn_ptr_decl(left_sym.info as ast.FnType, '_var_${left.pos.pos}')
						g.write(' = *(voidptr*)map_get(')
					} else {
						g.write('${styp} _var_${left.pos.pos} = *(${styp}*)map_get(')
					}
					if !left.left_type.is_ptr() {
						g.write('ADDR(map, ')
						g.expr(left.left)
						g.write(')')
					} else {
						g.expr(left.left)
					}
					g.write(', &(${skeytyp}[]){')
					g.expr(left.index)
					g.write('}')
					if val_typ.kind == .function {
						g.writeln(', &(voidptr[]){ ${zero} });')
					} else {
						g.writeln(', &(${styp}[]){ ${zero} });')
					}
				}
			}
			ast.SelectorExpr {
				styp := g.styp(left.typ)
				g.write('${styp} _var_${left.pos.pos} = ')
				g.expr(left.expr)
				sel := g.dot_or_ptr(left.expr_type)
				g.writeln('${sel}${left.field_name};')
			}
			else {}
		}
	}
}

fn (mut g Gen) gen_cross_tmp_variable(left []ast.Expr, val ast.Expr) {
	val_ := val
	match val {
		ast.Ident {
			mut has_var := false
			for lx in left {
				if lx is ast.Ident {
					if val.name == lx.name {
						g.write2('_var_', lx.pos.pos.str())
						has_var = true
						break
					}
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		ast.IndexExpr {
			mut has_var := false
			for lx in left {
				if val_.str() == lx.str() {
					g.write2('_var_', lx.pos().pos.str())
					has_var = true
					break
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		ast.InfixExpr {
			sym := g.table.sym(val.left_type)
			svalop := val.op.str()
			if _ := g.table.find_method(sym, svalop) {
				left_styp := g.styp(val.left_type.set_nr_muls(0))
				g.write2(left_styp, '_')
				g.write2(util.replace_op(svalop), '(')
				g.gen_cross_tmp_variable(left, val.left)
				g.write(', ')
				g.gen_cross_tmp_variable(left, val.right)
				g.write(')')
			} else {
				g.gen_cross_tmp_variable(left, val.left)
				g.write(svalop)
				g.gen_cross_tmp_variable(left, val.right)
			}
		}
		ast.ParExpr {
			g.write('(')
			g.gen_cross_tmp_variable(left, val.expr)
			g.write(')')
		}
		ast.CallExpr {
			if val.is_method {
				unwrapped_rec_type, typ_sym := g.unwrap_receiver_type(val)
				left_type := g.unwrap_generic(val.left_type)
				left_sym := g.table.sym(left_type)
				final_left_sym := g.table.final_sym(left_type)
				rec_typ_name := g.resolve_receiver_name(val, unwrapped_rec_type, final_left_sym,
					left_sym, typ_sym)
				fn_name := util.no_dots('${rec_typ_name}_${val.name}')
				g.write('${fn_name}(&')
				g.gen_cross_tmp_variable(left, val.left)
				for i, arg in val.args {
					g.gen_cross_tmp_variable(left, arg.expr)
					if i != val.args.len - 1 {
						g.write(', ')
					}
				}
				g.write(')')
			} else {
				mut fn_name := val.name.replace('.', '__')
				if val.concrete_types.len > 0 {
					fn_name = g.generic_fn_name(val.concrete_types, fn_name)
				}
				g.write('${fn_name}(')
				for i, arg in val.args {
					g.gen_cross_tmp_variable(left, arg.expr)
					if i != val.args.len - 1 {
						g.write(', ')
					}
				}
				g.write(')')
			}
		}
		ast.PrefixExpr {
			g.write(val.op.str())
			g.gen_cross_tmp_variable(left, val.right)
		}
		ast.PostfixExpr {
			g.gen_cross_tmp_variable(left, val.expr)
			g.write(val.op.str())
		}
		ast.SelectorExpr {
			mut has_var := false
			for lx in left {
				if val_.str() == lx.str() {
					g.write2('_var_', lx.pos().pos.str())
					has_var = true
					break
				}
			}
			if !has_var {
				g.expr(val_)
			}
		}
		else {
			g.expr(val_)
		}
	}
}
