// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) need_tmp_var_in_if(node ast.IfExpr) bool {
	if node.is_expr && g.inside_ternary == 0 {
		if g.is_autofree || node.typ.has_flag(.optional) {
			return true
		}
		for branch in node.branches {
			if branch.cond is ast.IfGuardExpr || branch.stmts.len > 1 {
				return true
			}
			if branch.stmts.len == 1 {
				if branch.stmts[0] is ast.ExprStmt {
					stmt := branch.stmts[0] as ast.ExprStmt
					if is_noreturn_callexpr(stmt.expr) {
						return true
					}
					if stmt.expr is ast.CallExpr {
						if stmt.expr.is_method {
							left_sym := g.table.sym(stmt.expr.receiver_type)
							if left_sym.kind in [.array, .array_fixed, .map] {
								return true
							}
						} else if stmt.expr.or_block.kind != .absent {
							return true
						}
					}
				}
			}
		}
	}
	return false
}

fn (mut g Gen) if_expr(node ast.IfExpr) {
	if node.is_comptime {
		g.comptime_if(node)
		return
	}
	// For simpe if expressions we can use C's `?:`
	// `if x > 0 { 1 } else { 2 }` => `(x > 0) ? (1) : (2)`
	// For if expressions with multiple statements or another if expression inside, it's much
	// easier to use a temp var, than do C tricks with commas, introduce special vars etc
	// (as it used to be done).
	// Always use this in -autofree, since ?: can have tmp expressions that have to be freed.
	needs_tmp_var := g.need_tmp_var_in_if(node)
	tmp := if needs_tmp_var { g.new_tmp_var() } else { '' }
	mut cur_line := ''
	if needs_tmp_var {
		if node.typ.has_flag(.optional) {
			g.inside_if_optional = true
		}
		styp := g.typ(node.typ)
		cur_line = g.go_before_stmt(0)
		g.empty_line = true
		g.writeln('$styp $tmp; /* if prepend */')
		if g.infix_left_var_name.len > 0 {
			g.writeln('if ($g.infix_left_var_name) {')
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
			g.stmts(branch.stmts)
		}
		if node.branches.len == 1 {
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
				guard_idx = i
				guard_vars = []string{len: node.branches.len}
			}
			if cond.expr !is ast.IndexExpr && cond.expr !is ast.PrefixExpr {
				var_name := g.new_tmp_var()
				guard_vars[i] = var_name
				g.writeln('${g.typ(cond.expr_type)} $var_name;')
			} else {
				guard_vars[i] = ''
			}
		}
	}
	for i, branch in node.branches {
		if i > 0 {
			g.write('} else ')
		}
		// if last branch is `else {`
		if i == node.branches.len - 1 && node.has_else {
			g.writeln('{')
			// define `err` only for simple `if val := opt {...} else {`
			if is_guard && guard_idx == i - 1 {
				cvar_name := guard_vars[guard_idx]
				g.writeln('\tIError err = ${cvar_name}.err;')
			}
		} else {
			match branch.cond {
				ast.IfGuardExpr {
					mut var_name := guard_vars[i]
					mut short_opt := false
					if var_name == '' {
						short_opt = true // we don't need a further tmp, so use the one we'll get later
						var_name = g.new_tmp_var()
						guard_vars[i] = var_name // for `else`
						g.tmp_count--
						g.writeln('if (${var_name}.state == 0) {')
					} else {
						g.write('if ($var_name = ')
						g.expr(branch.cond.expr)
						g.writeln(', ${var_name}.state == 0) {')
					}
					if short_opt || branch.cond.vars[0].name != '_' {
						base_type := g.base_type(branch.cond.expr_type)
						if short_opt {
							cond_var_name := if branch.cond.vars[0].name == '_' {
								'_dummy_${g.tmp_count + 1}'
							} else {
								branch.cond.vars[0].name
							}
							g.write('\t$base_type $cond_var_name = ')
							g.expr(branch.cond.expr)
							g.writeln(';')
						} else {
							mut is_auto_heap := false
							if branch.stmts.len > 0 {
								scope := g.file.scope.innermost(ast.Node(branch.stmts[branch.stmts.len - 1]).pos().pos)
								if v := scope.find_var(branch.cond.vars[0].name) {
									is_auto_heap = v.is_auto_heap
								}
							}
							if branch.cond.vars.len == 1 {
								left_var_name := c_name(branch.cond.vars[0].name)
								if is_auto_heap {
									g.writeln('\t$base_type* $left_var_name = HEAP($base_type, *($base_type*)${var_name}.data);')
								} else {
									g.writeln('\t$base_type $left_var_name = *($base_type*)${var_name}.data;')
								}
							} else if branch.cond.vars.len > 1 {
								for vi, var in branch.cond.vars {
									left_var_name := c_name(var.name)
									sym := g.table.sym(branch.cond.expr_type)
									if sym.kind == .multi_return {
										mr_info := sym.info as ast.MultiReturn
										if mr_info.types.len == branch.cond.vars.len {
											var_typ := g.typ(mr_info.types[vi])
											if is_auto_heap {
												g.writeln('\t$var_typ* $left_var_name = (HEAP($base_type, *($base_type*)${var_name}.data).arg$vi);')
											} else {
												g.writeln('\t$var_typ $left_var_name = (*($base_type*)${var_name}.data).arg$vi;')
											}
										}
									}
								}
							}
						}
					}
				}
				else {
					mut no_needs_par := false
					if branch.cond is ast.InfixExpr {
						if branch.cond.op == .key_in && branch.cond.left !is ast.InfixExpr
							&& branch.cond.right is ast.ArrayInit {
							no_needs_par = true
						}
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
				}
			}
		}
		if needs_tmp_var {
			g.stmts_with_tmp_var(branch.stmts, tmp)
		} else {
			// restore if_expr stmt header pos
			stmt_pos := g.nth_stmt_pos(0)
			g.stmts(branch.stmts)
			g.stmt_path_pos << stmt_pos
		}
	}
	if node.branches.len > 0 {
		g.writeln('}')
	}
	g.set_current_pos_as_last_stmt_pos()
	if needs_tmp_var {
		if g.infix_left_var_name.len > 0 {
			g.indent--
			g.writeln('}')
		}
		g.empty_line = false
		g.write('$cur_line $tmp')
	}
	if node.typ.has_flag(.optional) {
		g.inside_if_optional = false
	}
}
