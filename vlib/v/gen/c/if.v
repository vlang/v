// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) need_tmp_var_in_if(node ast.IfExpr) bool {
	if node.is_expr && (g.inside_ternary == 0 || g.is_assign_lhs) {
		if g.is_autofree || node.typ.has_option_or_result() || node.is_comptime || g.is_assign_lhs {
			return true
		}
		for branch in node.branches {
			if branch.stmts.len > 1 {
				return true
			}
			if g.need_tmp_var_in_expr(branch.cond) {
				return true
			}
			if branch.stmts.len == 1 {
				if branch.stmts[0] is ast.ExprStmt {
					stmt := branch.stmts[0] as ast.ExprStmt
					if stmt.expr is ast.ArrayInit && stmt.expr.is_fixed {
						return true
					}
					if g.need_tmp_var_in_expr(stmt.expr) {
						return true
					}
				} else if branch.stmts[0] is ast.Return {
					return true
				} else if branch.stmts[0] is ast.BranchStmt {
					return true
				}
			}
		}
	}
	return false
}

fn (mut g Gen) need_tmp_var_in_expr(expr ast.Expr) bool {
	if is_noreturn_callexpr(expr) {
		return true
	}
	match expr {
		ast.ArrayInit {
			if g.need_tmp_var_in_expr(expr.len_expr) {
				return true
			}
			if g.need_tmp_var_in_expr(expr.cap_expr) {
				return true
			}
			if g.need_tmp_var_in_expr(expr.init_expr) {
				return true
			}
			for elem_expr in expr.exprs {
				if g.need_tmp_var_in_expr(elem_expr) {
					return true
				}
			}
		}
		ast.CallExpr {
			if expr.is_method {
				left_sym := g.table.sym(expr.receiver_type)
				if left_sym.kind in [.array, .array_fixed, .map] {
					return true
				}
			}
			if expr.or_block.kind != .absent {
				return true
			}
			if g.need_tmp_var_in_expr(expr.left) {
				return true
			}
			for arg in expr.args {
				if g.need_tmp_var_in_expr(arg.expr) {
					return true
				}
			}
			return expr.expected_arg_types.any(it.has_flag(.option))
		}
		ast.CastExpr {
			return g.need_tmp_var_in_expr(expr.expr)
		}
		ast.ConcatExpr {
			for val in expr.vals {
				if val is ast.CallExpr {
					if val.return_type.has_option_or_result() {
						return true
					}
				}
			}
		}
		ast.Ident {
			return expr.or_expr.kind != .absent
		}
		ast.IfExpr {
			if g.need_tmp_var_in_if(expr) {
				return true
			}
		}
		ast.IfGuardExpr {
			return true
		}
		ast.IndexExpr {
			if expr.or_expr.kind != .absent {
				return true
			}
			if g.need_tmp_var_in_expr(expr.index) {
				return true
			}
		}
		ast.InfixExpr {
			if g.need_tmp_var_in_expr(expr.left) {
				return true
			}
			if g.need_tmp_var_in_expr(expr.right) {
				return true
			}
		}
		ast.MapInit {
			for key in expr.keys {
				if g.need_tmp_var_in_expr(key) {
					return true
				}
			}
			for val in expr.vals {
				if g.need_tmp_var_in_expr(val) {
					return true
				}
			}
		}
		ast.MatchExpr {
			return true
		}
		ast.ParExpr {
			return g.need_tmp_var_in_expr(expr.expr)
		}
		ast.PrefixExpr {
			return g.need_tmp_var_in_expr(expr.right)
		}
		ast.SelectorExpr {
			if g.need_tmp_var_in_expr(expr.expr) {
				return true
			}
			return expr.or_block.kind != .absent
		}
		ast.StringInterLiteral {
			for e in expr.exprs {
				if g.need_tmp_var_in_expr(e) {
					return true
				}
			}
		}
		ast.StructInit {
			if g.need_tmp_var_in_expr(expr.update_expr) {
				return true
			}
			for init_field in expr.init_fields {
				if g.need_tmp_var_in_expr(init_field.expr) {
					return true
				}
			}
			sym := g.table.sym(expr.typ)
			return sym.info is ast.Struct && sym.info.has_option
		}
		else {}
	}
	return false
}

fn (mut g Gen) needs_conds_order(node ast.IfExpr) bool {
	if node.branches.len > 1 {
		for branch in node.branches {
			if g.need_tmp_var_in_expr(branch.cond) {
				return true
			}
		}
	}
	return false
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	// For simple if expressions we can use C's `?:`
	// `if x > 0 { 1 } else { 2 }` => `(x > 0)? (1) : (2)`
	// For if expressions with multiple statements or another if expression inside, it's much
	// easier to use a temp var, than do C tricks with commas, introduce special vars etc
	// (as it used to be done).
	// Always use this in -autofree, since ?: can have tmp expressions that have to be freed.
	needs_tmp_var := g.inside_if_option || g.need_tmp_var_in_if(node)
	needs_conds_order := g.needs_conds_order(node)
	tmp := if g.inside_if_option || (node.typ != ast.void_type && needs_tmp_var) {
		g.new_tmp_var()
	} else {
		''
	}
	mut cur_line := ''
	mut raw_state := false
	tmp_if_option_type := g.last_if_option_type
	if needs_tmp_var {
		mut styp := g.styp(node.typ)
		if g.inside_if_option || node.typ.has_flag(.option) {
			raw_state = g.inside_if_option
			if node.typ != ast.void_type {
				g.last_if_option_type = node.typ
				defer {
					g.last_if_option_type = tmp_if_option_type
				}
			}
			defer {
				g.inside_if_option = raw_state
			}
			g.inside_if_option = true
			styp = styp.replace('*', '_ptr')
		} else if node.typ.has_flag(.result) {
			raw_state = g.inside_if_result
			defer {
				g.inside_if_result = raw_state
			}
			g.inside_if_result = true
			styp = styp.replace('*', '_ptr')
		} else {
			g.last_if_option_type = node.typ
			defer {
				g.last_if_option_type = tmp_if_option_type
			}
		}
		cur_line = g.go_before_last_stmt()
		g.empty_line = true
		if tmp != '' {
			if node.typ == ast.void_type && g.last_if_option_type != 0 {
				// nested if on return stmt
				g.write2(g.styp(g.last_if_option_type), ' ')
			} else {
				g.write('${styp} ')
			}
			g.writeln('${tmp}; /* if prepend */')
		}
		if g.infix_left_var_name.len > 0 {
			g.writeln('if (${g.infix_left_var_name}) {')
			g.indent++
		}
	} else if node.is_expr || g.inside_ternary != 0 {
		g.inside_ternary++
		g.write('(')
		for i, branch in node.branches {
			if i > 0 {
				g.write(' : ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				g.expr(branch.cond)
				g.write(' ? ')
			}
			prev_expected_cast_type := g.expected_cast_type
			if node.is_expr
				&& (g.table.sym(node.typ).kind == .sum_type || node.typ.has_flag(.shared_f)) {
				g.expected_cast_type = node.typ
			}
			g.stmts(branch.stmts)
			g.expected_cast_type = prev_expected_cast_type
		}
		if node.branches.len == 1 && !node.is_expr {
			g.write(': 0')
		}
		g.write(')')
		g.decrement_inside_ternary()
		return
	}
	mut is_guard := false
	mut guard_idx := 0
	mut guard_vars := []string{}
	for i, branch in node.branches {
		cond := branch.cond
		if cond is ast.IfGuardExpr {
			if !is_guard {
				is_guard = true
				guard_vars = []string{len: node.branches.len}
			}
			guard_idx = i // saves the last if guard index
			if cond.expr !in [ast.IndexExpr, ast.PrefixExpr] {
				var_name := g.new_tmp_var()
				guard_vars[i] = var_name
				cond_expr_type := if cond.expr is ast.CallExpr && cond.expr.is_static_method
					&& cond.expr.left_type.has_flag(.generic) {
					g.resolve_return_type(cond.expr)
				} else {
					cond.expr_type
				}
				g.writeln('${g.styp(g.unwrap_generic(cond_expr_type))} ${var_name};')
			} else if cond.expr is ast.IndexExpr {
				value_type := g.table.value_type(g.unwrap_generic(cond.expr.left_type))
				if value_type.has_flag(.option) {
					var_name := g.new_tmp_var()
					guard_vars[i] = var_name
					g.writeln('${g.styp(value_type)} ${var_name};')
				} else {
					guard_vars[i] = ''
				}
			} else {
				guard_vars[i] = ''
			}
		}
	}
	mut branch_cond_var_names := []string{}
	for i, branch in node.branches {
		if i > 0 {
			g.write('} else ')
		}
		// if last branch is `else {`
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('{')
			// define `err` for the last branch after a `if val := opt {...}' guard
			if is_guard && guard_idx == i - 1 {
				cvar_name := guard_vars[guard_idx]
				g.writeln('\tIError err = ${cvar_name}.err;')
			}
		} else if branch.cond is ast.IfGuardExpr {
			mut var_name := guard_vars[i]
			mut short_opt := false
			g.left_is_opt = true
			if var_name == '' {
				short_opt = true // we don't need a further tmp, so use the one we'll get later
				var_name = g.new_tmp_var()
				guard_vars[i] = var_name // for `else`
				g.tmp_count--
				if branch.cond.expr_type.has_flag(.option) {
					g.writeln('if (${var_name}.state == 0) {')
				} else if branch.cond.expr_type.has_flag(.result) {
					g.writeln('if (!${var_name}.is_error) {')
				}
			} else {
				g.write('if (${var_name} = ')
				g.expr(branch.cond.expr)
				if branch.cond.expr_type.has_flag(.option) {
					dot_or_ptr := if !branch.cond.expr_type.has_flag(.option_mut_param_t) {
						'.'
					} else {
						'-> '
					}
					g.writeln(', ${var_name}${dot_or_ptr}state == 0) {')
				} else if branch.cond.expr_type.has_flag(.result) {
					g.writeln(', !${var_name}.is_error) {')
				}
			}
			if short_opt || branch.cond.vars.len > 1 || branch.cond.vars[0].name != '_' {
				base_type := g.base_type(branch.cond.expr_type)
				if short_opt {
					cond_var_name := if branch.cond.vars[0].name == '_' {
						'_dummy_${g.tmp_count + 1}'
					} else {
						branch.cond.vars[0].name
					}
					if g.table.sym(branch.cond.expr_type).kind == .array_fixed {
						g.writeln('\t${base_type} ${cond_var_name} = {0};')
						g.write('\tmemcpy((${base_type}*)${cond_var_name}, &')
						g.expr(branch.cond.expr)
						g.writeln(', sizeof(${base_type}));')
					} else {
						g.write('\t${base_type} ${cond_var_name} = ')
						g.expr(branch.cond.expr)
						g.writeln(';')
					}
				} else {
					mut is_auto_heap := false
					if branch.stmts.len > 0 {
						scope := g.file.scope.innermost(ast.Node(branch.stmts.last()).pos().pos)
						if v := scope.find_var(branch.cond.vars[0].name) {
							is_auto_heap = v.is_auto_heap
						}
					}
					if branch.cond.vars.len == 1 {
						left_var_name := c_name(branch.cond.vars[0].name)
						if is_auto_heap {
							g.writeln('\t${base_type}* ${left_var_name} = HEAP(${base_type}, *(${base_type}*)${var_name}.data);')
						} else if base_type.starts_with('Array_fixed') {
							g.writeln('\t${base_type} ${left_var_name} = {0};')
							g.writeln('memcpy(${left_var_name}, (${base_type}*)${var_name}.data, sizeof(${base_type}));')
						} else {
							dot_or_ptr := if !branch.cond.expr_type.has_flag(.option_mut_param_t) {
								'.'
							} else {
								'-> '
							}
							g.writeln('\t${base_type} ${left_var_name} = *(${base_type}*)${var_name}${dot_or_ptr}data;')
						}
					} else if branch.cond.vars.len > 1 {
						sym := g.table.sym(branch.cond.expr_type)
						if sym.info is ast.MultiReturn {
							if sym.info.types.len == branch.cond.vars.len {
								for vi, var in branch.cond.vars {
									if var.name == '_' {
										continue
									}
									var_typ := g.styp(sym.info.types[vi])
									left_var_name := c_name(var.name)
									if is_auto_heap {
										g.writeln('\t${var_typ}* ${left_var_name} = (HEAP(${base_type}, *(${base_type}*)${var_name}.data).arg${vi});')
									} else {
										g.writeln('\t${var_typ} ${left_var_name} = (*(${base_type}*)${var_name}.data).arg${vi};')
									}
								}
							}
						}
					}
				}
			}
		} else {
			if i == 0 && node.branches.len > 1 && !needs_tmp_var && needs_conds_order {
				cond_var_name := g.new_tmp_var()
				line := g.go_before_last_stmt().trim_space()
				g.empty_line = true
				g.write('bool ${cond_var_name} = ')
				g.expr(branch.cond)
				g.writeln(';')
				branch_cond_var_names << cond_var_name
				g.set_current_pos_as_last_stmt_pos()
				g.writeln2(line, 'if (${cond_var_name}) {')
			} else if i > 0 && branch_cond_var_names.len > 0 && !needs_tmp_var && needs_conds_order {
				cond_var_name := g.new_tmp_var()
				line := g.go_before_last_stmt()
				g.empty_line = true
				g.writeln('bool ${cond_var_name};')
				branch_cond := branch_cond_var_names.join(' || ')
				g.writeln('if (!(${branch_cond})) {')
				g.set_current_pos_as_last_stmt_pos()
				g.indent++
				g.write('${cond_var_name} = ')
				prev_is_autofree := g.is_autofree
				g.is_autofree = false
				g.expr(branch.cond)
				g.is_autofree = prev_is_autofree
				g.writeln(';')
				g.indent--
				g.writeln('}')
				branch_cond_var_names << cond_var_name
				g.set_current_pos_as_last_stmt_pos()
				g.write(line)
				g.writeln('if (${cond_var_name}) {')
			} else {
				mut no_needs_par := false
				if branch.cond is ast.InfixExpr {
					if branch.cond.op == .key_in && branch.cond.left !is ast.InfixExpr
						&& branch.cond.right is ast.ArrayInit {
						no_needs_par = true
					}
				}
				inside_interface_deref_old := g.inside_interface_deref
				if !g.inside_interface_deref && branch.cond is ast.Ident
					&& g.table.is_interface_var(branch.cond.obj) {
					g.inside_interface_deref = true
				}
				if no_needs_par {
					g.write('if ')
				} else {
					g.write('if (')
				}
				g.expr(branch.cond)
				if no_needs_par {
					g.writeln(' {')
				} else {
					g.writeln(') {')
				}
				g.inside_interface_deref = inside_interface_deref_old
			}
		}
		if needs_tmp_var {
			prev_expected_cast_type := g.expected_cast_type
			if node.is_expr
				&& (g.table.sym(node.typ).kind == .sum_type || node.typ.has_flag(.shared_f)) {
				g.expected_cast_type = node.typ
			}
			g.stmts_with_tmp_var(branch.stmts, tmp)
			g.expected_cast_type = prev_expected_cast_type
		} else {
			// restore if_expr stmt header pos
			stmt_pos := g.nth_stmt_pos(0)
			g.stmts(branch.stmts)
			g.stmt_path_pos << stmt_pos
		}
	}
	if node.branches.len > 0 {
		g.writeln('}')
		g.set_current_pos_as_last_stmt_pos()
	}
	if needs_tmp_var {
		if g.infix_left_var_name.len > 0 {
			g.indent--
			g.writeln('}')
			g.set_current_pos_as_last_stmt_pos()
		}
		g.empty_line = false
		g.write('${cur_line}${tmp}')
	}
}
