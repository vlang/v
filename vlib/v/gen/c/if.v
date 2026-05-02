// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast

fn (mut g Gen) if_guard_var_needs_gc_pin(scope &ast.Scope, name string) bool {
	if g.pref.gc_mode !in [.boehm_full, .boehm_incr, .boehm_full_opt, .boehm_incr_opt] {
		return false
	}
	if name == '_' {
		return false
	}
	if v := scope.find_var(name) {
		return v.is_auto_heap || v.typ.is_any_kind_of_pointer() || g.contains_ptr(v.typ)
	}
	return false
}

fn (g &Gen) if_guard_else_uses_err(node ast.IfExpr, branch_idx int) bool {
	if !node.has_else || branch_idx != node.branches.len - 2 {
		return false
	}
	else_branch := node.branches[branch_idx + 1]
	if err_var := else_branch.scope.find_var('err') {
		return err_var.is_used
	}
	return false
}

fn (mut g Gen) write_if_guard_gc_pin(scope &ast.Scope, name string, cvar_name string) {
	if g.if_guard_var_needs_gc_pin(scope, name) {
		g.writeln('\tGC_reachable_here(&${cvar_name});')
	}
}

fn (mut g Gen) if_guard_error_cleanup(cvar_name string, expr_type ast.Type) {
	if expr_type.has_flag(.option) {
		g.writeln('\tif (${cvar_name}.state == 2 && ${cvar_name}.err._object != _const_none__._object) { builtin___v_free(${cvar_name}.err._object); }')
	} else if expr_type.has_flag(.result) {
		g.writeln('\tif (${cvar_name}.is_error && ${cvar_name}.err._object != _const_none__._object) { builtin___v_free(${cvar_name}.err._object); }')
	}
}

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
			elem_type := g.unwrap_generic(expr.elem_type)
			elem_kind := if elem_type != 0 {
				g.table.final_sym(elem_type).kind
			} else {
				ast.Kind.placeholder
			}
			if expr.has_index || (expr.has_len && (g.struct_has_array_or_map_field(elem_type)
				|| (elem_kind in [.array, .map] && !expr.has_init))) {
				return true
			}
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
					return expr.name !in ['contains', 'exists', 'len', 'cap', 'first', 'last',
						'index', 'last_index', 'get']
				}
			}
			if expr.or_block.kind != .absent {
				return true
			}
			if g.need_tmp_var_in_expr(expr.left) {
				return true
			}
			for arg in expr.args {
				if arg.expr is ast.ArrayDecompose {
					return true
				}
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
			if g.need_tmp_var_in_expr(expr.left) {
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
			// struct pointer equality comparisons may hoist temp vars
			// (via gen_struct_pointer_eq_op) which breaks short-circuit
			// evaluation when used on the right side of `&&` after an
			// `is` check. Detect this so that infix_expr_and_or_op uses
			// its safe short-circuit pattern instead.
			if expr.op in [.eq, .ne] {
				if (expr.left_type.is_ptr() && !expr.left.is_lvalue())
					|| (expr.right_type.is_ptr() && !expr.right.is_lvalue()) {
					left_sym := g.table.sym(expr.left_type)
					right_sym := g.table.sym(expr.right_type)
					if left_sym.kind == .struct || right_sym.kind == .struct {
						return true
					}
				}
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
		ast.SqlExpr {
			return true
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
	use_outer_tmp := g.outer_tmp_var != ''
	saved_outer_tmp_var := g.outer_tmp_var
	if use_outer_tmp {
		g.outer_tmp_var = ''
	}
	resolved_node_typ := g.infer_if_expr_type(node)

	// For simple if expressions we can use C's `?:`
	// `if x > 0 { 1 } else { 2 }` => `(x > 0)? (1) : (2)`
	// For if expressions with multiple statements or another if expression inside, it's much
	// easier to use a temp var, than do C tricks with commas, introduce special vars etc
	// (as it used to be done).
	// Always use this in -autofree, since ?: can have tmp expressions that have to be freed.
	needs_tmp_var := g.inside_if_option || g.need_tmp_var_in_if(node) || use_outer_tmp
	needs_conds_order := g.needs_conds_order(node)
	tmp := if use_outer_tmp {
		// Use the tmp var from outer context (e.g. from stmts_with_tmp_var)
		saved_outer_tmp_var
	} else if g.inside_if_option || (resolved_node_typ != ast.void_type && needs_tmp_var) {
		g.new_tmp_var()
	} else {
		''
	}
	mut cur_line := ''
	mut raw_state := false
	tmp_if_option_type := g.last_if_option_type
	mut exit_label := ''
	if needs_tmp_var {
		exit_label = g.new_tmp_var()
		node_typ := if g.inside_or_block {
			resolved_node_typ.clear_option_and_result()
		} else {
			resolved_node_typ
		}
		// For generic functions, if the if-expression's type was set to a concrete type
		// by the checker but we're generating a different generic instance, we need to
		// use the correct concrete type from cur_concrete_types
		resolved_typ := if g.cur_fn != unsafe { nil } && g.cur_fn.generic_names.len > 0
			&& g.cur_concrete_types.len > 0 {
			// Try to unwrap generic, and if that doesn't work, check if we should use
			// the function's return type
			unwrapped := g.unwrap_generic(node_typ)
			if g.inside_return_expr && !g.inside_struct_init && unwrapped == node_typ
				&& g.cur_fn.return_type.has_flag(.generic) {
				// The node type didn't unwrap, but the function return type is generic
				// Get the unwrapped function return type for this instance
				mut fn_ret_typ := g.unwrap_generic(g.cur_fn.return_type)
				if g.inside_or_block {
					fn_ret_typ = fn_ret_typ.clear_option_and_result()
				}
				// Check if the function return type directly matches one of the concrete types
				// If it does, the if expression type should also match that concrete type
				fn_ret_is_direct_generic := g.cur_concrete_types.any(it == fn_ret_typ)
				if fn_ret_is_direct_generic && node_typ != fn_ret_typ {
					// The function returns T directly, and node_typ doesn't match the current T
					// This means node_typ is stale from another instance
					fn_ret_typ
				} else {
					// Either the function return type is wrapped (like !T or []T),
					// or node_typ is correct for this instance
					unwrapped
				}
			} else {
				unwrapped
			}
		} else {
			g.unwrap_generic(node_typ)
		}
		resolved_sym := g.table.final_sym(resolved_typ)
		mut styp := g.styp(resolved_typ)
		if (g.inside_if_option || node_typ.has_flag(.option)) && !g.inside_or_block {
			raw_state = g.inside_if_option
			if resolved_node_typ != ast.void_type {
				g.last_if_option_type = resolved_node_typ
				defer(fn) {
					g.last_if_option_type = tmp_if_option_type
				}
			}
			defer(fn) {
				g.inside_if_option = raw_state
			}
			g.inside_if_option = true
			styp = styp.replace('*', '_ptr')
		} else if node_typ.has_flag(.result) && !g.inside_or_block {
			raw_state = g.inside_if_result
			defer(fn) {
				g.inside_if_result = raw_state
			}
			g.inside_if_result = true
			styp = styp.replace('*', '_ptr')
		} else {
			g.last_if_option_type = node_typ
			defer(fn) {
				g.last_if_option_type = tmp_if_option_type
			}
		}
		cur_line = g.go_before_last_stmt()
		g.empty_line = true
		if tmp != '' && !use_outer_tmp {
			// Only declare the tmp var if it's not from outer context
			mut declared_tmp := false
			if resolved_node_typ == ast.void_type && g.last_if_option_type != 0 {
				// nested if on return stmt
				g.write2(g.styp(g.unwrap_generic(g.last_if_option_type)), ' ')
			} else if resolved_sym.kind == .function && resolved_sym.info is ast.FnType {
				param_types := resolved_sym.info.func.params.map(it.typ)
				g.writeln('${g.fn_var_signature(resolved_typ, resolved_sym.info.func.return_type,
					param_types, tmp)}; /* if prepend */')
				declared_tmp = true
			} else {
				g.write('${styp} ')
			}
			if !declared_tmp {
				g.writeln('${tmp}; /* if prepend */')
			}
			g.set_current_pos_as_last_stmt_pos()
		}
		if g.infix_left_var_name.len > 0 {
			g.writeln('if (${g.infix_left_var_name}) {')
			g.set_current_pos_as_last_stmt_pos()
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
			if node.is_expr && (g.table.sym(resolved_node_typ).kind == .sum_type
				|| resolved_node_typ.has_flag(.shared_f)) {
				g.expected_cast_type = resolved_node_typ
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
	mut guard_expr_types := []ast.Type{len: node.branches.len}
	mut guard_else_uses_err := []bool{len: node.branches.len}
	mut guard_owns_error := []bool{len: node.branches.len}
	for i, branch in node.branches {
		cond := branch.cond
		if cond is ast.IfGuardExpr {
			if !is_guard {
				is_guard = true
				guard_vars = []string{len: node.branches.len}
			}
			guard_idx = i // saves the last if guard index
			guard_else_uses_err[i] = g.if_guard_else_uses_err(node, i)
			guard_owns_error[i] = cond.expr is ast.IndexExpr
			if cond.expr !in [ast.IndexExpr, ast.PrefixExpr] {
				var_name := g.new_tmp_var()
				guard_vars[i] = var_name
				cond_expr_type := if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0
					&& cond.expr is ast.CallExpr {
					resolved := g.resolve_return_type(cond.expr)
					if resolved != ast.void_type && !resolved.has_flag(.generic) {
						if cond.expr_type.has_flag(.option) && !resolved.has_flag(.option) {
							resolved.set_flag(.option)
						} else if cond.expr_type.has_flag(.result) && !resolved.has_flag(.result) {
							resolved.set_flag(.result)
						} else {
							resolved
						}
					} else {
						cond.expr_type
					}
				} else {
					cond.expr_type
				}
				g.writeln('${g.styp(g.unwrap_generic(cond_expr_type))} ${var_name} = {0};')
			} else if cond.expr is ast.IndexExpr {
				value_type := g.table.value_type(g.unwrap_generic(cond.expr.left_type))
				if value_type.has_flag(.option) {
					var_name := g.new_tmp_var()
					guard_vars[i] = var_name
					g.writeln('${g.styp(value_type)} ${var_name} = {0};')
				} else {
					guard_vars[i] = ''
				}
			} else {
				guard_vars[i] = ''
			}
		}
	}
	mut branch_cond_var_names := []string{}
	mut tmp_var_scope_count := 0
	for i, branch in node.branches {
		is_else := i == node.branches.len - 1 && node.has_else
		if i > 0 {
			if needs_tmp_var {
				g.writeln('};')
				// Open a new scope so that any variables generated by the next
				// branch's condition evaluation (e.g. from `.any()` or `.all()`)
				// are inside a block that the previous branch's `goto` skips over
				// entirely, preventing gcc's -Wjump-misses-init with -cstrict.
				if !is_else {
					g.writeln('{')
					tmp_var_scope_count++
				}
				g.set_current_pos_as_last_stmt_pos()
			} else {
				g.write('} else ')
			}
		}
		// if last branch is `else {`
		if is_else {
			g.writeln('{')
			// define `err` for the last branch after a `if val := opt {...}' guard
			if is_guard && guard_idx == i - 1 {
				cvar_name := guard_vars[guard_idx]
				if guard_else_uses_err[guard_idx] {
					if err_var := branch.scope.find_var('err') {
						if err_var.is_used {
							g.writeln('\tIError err = ${cvar_name}.err;')
						}
					}
				} else if guard_owns_error[guard_idx] && guard_expr_types[guard_idx] != 0 {
					g.if_guard_error_cleanup(cvar_name, guard_expr_types[guard_idx])
				}
			}
		} else if branch.cond is ast.IfGuardExpr {
			mut var_name := guard_vars[i]
			mut short_opt := false
			g.left_is_opt = true
			// Resolve the expression type in generic context
			mut guard_expr_type := branch.cond.expr_type
			if g.cur_fn != unsafe { nil } && g.cur_concrete_types.len > 0 {
				if branch.cond.expr is ast.CallExpr {
					resolved := g.resolve_return_type(branch.cond.expr)
					if resolved != ast.void_type && !resolved.has_flag(.generic) {
						guard_expr_type = if branch.cond.expr_type.has_flag(.option)
							&& !resolved.has_flag(.option) {
							resolved.set_flag(.option)
						} else if branch.cond.expr_type.has_flag(.result)
							&& !resolved.has_flag(.result) {
							resolved.set_flag(.result)
						} else {
							resolved
						}
					}
				} else {
					resolved := g.unwrap_generic(g.recheck_concrete_type(guard_expr_type))
					if resolved != ast.void_type {
						guard_expr_type = resolved
					}
				}
			}
			guard_expr_types[i] = guard_expr_type
			if var_name == '' {
				short_opt = true // we don't need a further tmp, so use the one we'll get later
				var_name = g.new_tmp_var()
				guard_vars[i] = var_name // for `else`
				g.tmp_count--
				if guard_expr_type.has_flag(.option) {
					g.writeln('if (${var_name}.state == 0) {')
				} else if guard_expr_type.has_flag(.result) {
					g.writeln('if (!${var_name}.is_error) {')
				}
			} else {
				g.write('if (${var_name} = ')
				g.expr(branch.cond.expr)
				if guard_expr_type.has_flag(.option) {
					dot_or_ptr := if !guard_expr_type.has_flag(.option_mut_param_t) {
						'.'
					} else {
						'-> '
					}
					g.writeln(', ${var_name}${dot_or_ptr}state == 0) {')
				} else if guard_expr_type.has_flag(.result) {
					g.writeln(', !${var_name}.is_error) {')
				}
			}
			if short_opt || branch.cond.vars.len > 1 || branch.cond.vars[0].name != '_' {
				base_type := g.base_type(guard_expr_type)
				if short_opt {
					cond_var_name := if branch.cond.vars[0].name == '_' {
						'_dummy_${g.tmp_count + 1}'
					} else {
						branch.cond.vars[0].name
					}
					mut short_opt_is_auto_heap := false
					if branch.stmts.len > 0 {
						scope := g.file.scope.innermost(ast.Node(branch.stmts.last()).pos().pos)
						if v := scope.find_var(branch.cond.vars[0].name) {
							short_opt_is_auto_heap = v.is_auto_heap
						}
					}
					if g.table.sym(branch.cond.expr_type).kind == .array_fixed {
						g.writeln('\t${base_type} ${cond_var_name} = {0};')
						g.write('\tmemcpy((${base_type}*)${cond_var_name}, &')
						g.expr(branch.cond.expr)
						g.writeln(', sizeof(${base_type}));')
						g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
							cond_var_name)
					} else if short_opt_is_auto_heap {
						g.write('\t${base_type}* ${cond_var_name} = HEAP(${base_type}, ')
						g.expr(branch.cond.expr)
						g.writeln(');')
						g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
							cond_var_name)
					} else {
						g.write('\t${base_type} ${cond_var_name} = ')
						g.expr(branch.cond.expr)
						g.writeln(';')
						g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
							cond_var_name)
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
						dot_or_ptr := if !branch.cond.expr_type.has_flag(.option_mut_param_t) {
							'.'
						} else {
							'-> '
						}
						guard_typ :=
							g.unwrap_generic(branch.cond.expr_type.clear_option_and_result())
						guard_is_heap_obj := g.table.final_sym(guard_typ).is_heap()
							&& !guard_typ.is_ptr()
						if guard_is_heap_obj {
							g.writeln('\t${base_type}* ${left_var_name} = (${base_type}*)${var_name}${dot_or_ptr}data;')
							g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
								left_var_name)
						} else if is_auto_heap {
							// Non-heap structs still need a dedicated heap copy when the guard value escapes.
							// `@[heap]` structs already live behind the option data pointer and must not be copied.
							g.writeln('\t${base_type}* ${left_var_name} = HEAP(${base_type}, *(${base_type}*)${var_name}${dot_or_ptr}data);')
							g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
								left_var_name)
						} else if base_type.starts_with('Array_fixed') {
							g.writeln('\t${base_type} ${left_var_name} = {0};')
							g.writeln('memcpy(${left_var_name}, (${base_type}*)${var_name}.data, sizeof(${base_type}));')
							g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
								left_var_name)
						} else {
							expr_sym := g.table.sym(branch.cond.expr_type)
							if expr_sym.info is ast.FnType {
								g.write_fntype_decl(left_var_name, expr_sym.info,
									guard_expr_type.nr_muls())
								if guard_expr_type.nr_muls() == 0 {
									g.writeln(' = *(${base_type}*)${var_name}${dot_or_ptr}data;')
								} else {
									g.writeln(' = (${base_type}*)${var_name}${dot_or_ptr}data;')
								}
								g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
									left_var_name)
							} else {
								g.write('\t${base_type} ${left_var_name}')
								g.writeln(' = *(${base_type}*)${var_name}${dot_or_ptr}data;')
								g.write_if_guard_gc_pin(branch.scope, branch.cond.vars[0].name,
									left_var_name)
							}
						}
					} else if branch.cond.vars.len > 1 {
						sym := g.table.sym(guard_expr_type)
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
										g.write_if_guard_gc_pin(branch.scope, var.name,
											left_var_name)
									} else {
										g.writeln('\t${var_typ} ${left_var_name} = (*(${base_type}*)${var_name}.data).arg${vi};')
										g.write_if_guard_gc_pin(branch.scope, var.name,
											left_var_name)
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
			if node.is_expr && (g.table.sym(resolved_node_typ).kind == .sum_type
				|| resolved_node_typ.has_flag(.shared_f)) {
				g.expected_cast_type = resolved_node_typ
			}
			g.stmts_with_tmp_var(branch.stmts, tmp)
			g.write_defer_stmts(branch.scope, false, node.pos)
			g.expected_cast_type = prev_expected_cast_type
			if !is_else
				&& (branch.stmts.len > 0 && branch.stmts.last() !in [ast.Return, ast.BranchStmt]) {
				g.writeln('\tgoto ${exit_label};')
			}
		} else {
			// restore if_expr stmt header pos
			stmt_pos := g.nth_stmt_pos(0)
			g.stmts(branch.stmts)
			g.write_defer_stmts(branch.scope, false, node.pos)
			g.stmt_path_pos << stmt_pos
		}
	}
	if node.branches.len > 0 {
		g.writeln('}')
		if !needs_tmp_var {
			g.set_current_pos_as_last_stmt_pos()
		}
	}
	for i, var_name in guard_vars {
		if var_name == '' || guard_expr_types[i] == 0 || guard_else_uses_err[i]
			|| !guard_owns_error[i] {
			continue
		}
		if node.has_else && i == node.branches.len - 2 {
			continue
		}
		g.if_guard_error_cleanup(var_name, guard_expr_types[i])
	}
	if needs_tmp_var {
		// Close the extra scopes opened between branches to isolate
		// condition-evaluation variables from earlier branches' gotos.
		for _ in 0 .. tmp_var_scope_count {
			g.writeln('}')
		}
		if g.infix_left_var_name.len > 0 {
			g.indent--
			g.writeln('}')
		}
		g.empty_line = false
		g.writeln('\t${exit_label}: {};')
		g.set_current_pos_as_last_stmt_pos()
		g.write('${cur_line}${tmp}')
	}
}
