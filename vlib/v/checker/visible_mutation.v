module checker

import v.ast

enum RootMutationVisibility {
	none
	direct
	private_path
	public_path
}

@[inline]
fn is_visible_root_mutation(vis RootMutationVisibility) bool {
	return vis in [.direct, .public_path]
}

fn (mut c Checker) visible_param_mutation_cache_key(func ast.Fn, param_idx int) string {
	return '${func.fkey()}|${param_idx}'
}

fn (mut c Checker) fn_has_visible_mutation_for_param(func ast.Fn, param_idx int) bool {
	if param_idx < 0 || param_idx >= func.params.len || !func.params[param_idx].is_mut {
		return false
	}
	cache_key := c.visible_param_mutation_cache_key(func, param_idx)
	if cache_key in c.visible_param_mutation_cache {
		return c.visible_param_mutation_cache[cache_key]
	}
	if cache_key in c.visible_param_mutation_in_progress {
		return true
	}
	if func.source_fn == unsafe { nil } || func.no_body || func.language != .v {
		c.visible_param_mutation_cache[cache_key] = true
		return true
	}
	fn_decl := unsafe { &ast.FnDecl(func.source_fn) }
	if fn_decl == unsafe { nil } || param_idx >= fn_decl.params.len {
		c.visible_param_mutation_cache[cache_key] = true
		return true
	}
	c.visible_param_mutation_in_progress[cache_key] = true
	res := c.fn_decl_has_visible_mutation_for_param(fn_decl, param_idx)
	c.visible_param_mutation_in_progress.delete(cache_key)
	c.visible_param_mutation_cache[cache_key] = res
	return res
}

fn (mut c Checker) fn_decl_has_visible_mutation_for_param(fn_decl &ast.FnDecl, param_idx int) bool {
	if param_idx < 0 || param_idx >= fn_decl.params.len || !fn_decl.params[param_idx].is_mut {
		return false
	}
	root_name := fn_decl.params[param_idx].name
	root_type := fn_decl.params[param_idx].typ
	for stmt in fn_decl.stmts {
		if c.stmt_has_visible_mutation(stmt, root_name, root_type) {
			return true
		}
	}
	return false
}

fn (mut c Checker) stmt_has_visible_mutation(stmt ast.Stmt, root_name string, root_type ast.Type) bool {
	match stmt {
		ast.FnDecl {
			return false
		}
		ast.ExprStmt {
			return c.expr_has_visible_mutation(stmt.expr, root_name, root_type)
		}
		ast.AssignStmt {
			for left_expr in stmt.left {
				if is_visible_root_mutation(c.expr_mutation_visibility(left_expr, root_name,
					root_type))
				{
					return true
				}
			}
			if c.assign_stmt_aliases_visible_state(stmt, root_name, root_type) {
				return true
			}
		}
		else {}
	}
	return c.node_children_have_visible_mutation(ast.Node(stmt), root_name, root_type)
}

fn (mut c Checker) expr_has_visible_mutation(expr ast.Expr, root_name string, root_type ast.Type) bool {
	match expr {
		ast.AnonFn, ast.LambdaExpr {
			return false
		}
		ast.CallExpr {
			if c.call_has_visible_root_mutation(expr, root_name, root_type) {
				return true
			}
		}
		ast.PrefixExpr {
			if expr.op == .amp
				&& is_visible_root_mutation(c.expr_mutation_visibility(expr.right, root_name, root_type)) {
				return true
			}
		}
		ast.PostfixExpr {
			if is_visible_root_mutation(c.expr_mutation_visibility(expr.expr, root_name,
				root_type))
			{
				return true
			}
		}
		ast.InfixExpr {
			if expr.op == .left_shift
				&& is_visible_root_mutation(c.expr_mutation_visibility(expr.left, root_name, root_type)) {
				return true
			}
		}
		else {}
	}
	return c.node_children_have_visible_mutation(ast.Node(expr), root_name, root_type)
}

fn (mut c Checker) node_children_have_visible_mutation(node ast.Node, root_name string, root_type ast.Type) bool {
	for child in node.children() {
		match child {
			ast.Expr {
				if c.expr_has_visible_mutation(child, root_name, root_type) {
					return true
				}
			}
			ast.Stmt {
				if child is ast.FnDecl {
					continue
				}
				if c.stmt_has_visible_mutation(child, root_name, root_type) {
					return true
				}
			}
			ast.CallArg {
				if c.expr_has_visible_mutation(child.expr, root_name, root_type) {
					return true
				}
			}
			ast.IfBranch {
				if c.expr_has_visible_mutation(child.cond, root_name, root_type) {
					return true
				}
				for stmt in child.stmts {
					if c.stmt_has_visible_mutation(stmt, root_name, root_type) {
						return true
					}
				}
			}
			ast.MatchBranch {
				for branch_expr in child.exprs {
					if c.expr_has_visible_mutation(branch_expr, root_name, root_type) {
						return true
					}
				}
				for stmt in child.stmts {
					if c.stmt_has_visible_mutation(stmt, root_name, root_type) {
						return true
					}
				}
			}
			ast.SelectBranch {
				if c.stmt_has_visible_mutation(child.stmt, root_name, root_type) {
					return true
				}
				for stmt in child.stmts {
					if c.stmt_has_visible_mutation(stmt, root_name, root_type) {
						return true
					}
				}
			}
			else {}
		}
	}
	return false
}

fn (mut c Checker) expr_mutation_visibility(expr ast.Expr, root_name string, root_type ast.Type) RootMutationVisibility {
	mut reduced := expr
	reduced = reduced.remove_par()
	current := reduced
	if current is ast.Ident {
		return if current.name == root_name {
			.direct
		} else {
			.none
		}
	}
	if current is ast.SelectorExpr {
		mut parent_expr := current.expr
		parent_expr = parent_expr.remove_par()
		parent := parent_expr
		if parent is ast.Ident && parent.name == root_name {
			root_sym := c.table.final_sym(c.unwrap_generic(root_type))
			field := c.table.find_field_with_embeds(root_sym, current.field_name) or {
				return .public_path
			}
			return if field.is_pub { .public_path } else { .private_path }
		}
		return c.expr_mutation_visibility(parent, root_name, root_type)
	}
	if current is ast.IndexExpr {
		return c.expr_mutation_visibility(current.left, root_name, root_type)
	}
	if current is ast.PrefixExpr {
		return c.expr_mutation_visibility(current.right, root_name, root_type)
	}
	if current is ast.PostfixExpr {
		return c.expr_mutation_visibility(current.expr, root_name, root_type)
	}
	if current is ast.CastExpr {
		return c.expr_mutation_visibility(current.expr, root_name, root_type)
	}
	if current is ast.CallExpr {
		if current.is_method {
			return c.expr_mutation_visibility(current.left, root_name, root_type)
		}
	}
	return .none
}

fn (mut c Checker) call_has_visible_root_mutation(node ast.CallExpr, root_name string, root_type ast.Type) bool {
	mut called_fn := ast.Fn{}
	mut has_called_fn := false
	if func := c.find_called_fn(node) {
		called_fn = func
		has_called_fn = true
	}
	if node.is_method {
		left_vis := c.expr_mutation_visibility(node.left, root_name, root_type)
		if has_called_fn {
			if called_fn.params.len > 0 && called_fn.params[0].is_mut {
				match left_vis {
					.direct {
						if c.fn_has_visible_mutation_for_param(called_fn, 0) {
							return true
						}
					}
					.public_path {
						return true
					}
					else {}
				}
			}
		} else if is_visible_root_mutation(left_vis) {
			return true
		}
	}
	if !has_called_fn {
		for arg in node.args {
			if arg.is_mut
				&& is_visible_root_mutation(c.expr_mutation_visibility(arg.expr, root_name, root_type)) {
				return true
			}
		}
		return false
	}
	for i, arg in node.args {
		if !arg.is_mut {
			continue
		}
		param_idx := c.call_arg_param_index(called_fn, i)
		arg_vis := c.expr_mutation_visibility(arg.expr, root_name, root_type)
		if param_idx < 0 || param_idx >= called_fn.params.len {
			if is_visible_root_mutation(arg_vis) {
				return true
			}
			continue
		}
		if !called_fn.params[param_idx].is_mut {
			continue
		}
		match arg_vis {
			.direct {
				if c.fn_has_visible_mutation_for_param(called_fn, param_idx) {
					return true
				}
			}
			.public_path {
				return true
			}
			else {}
		}
	}
	return false
}

fn (mut c Checker) find_called_fn(node ast.CallExpr) ?ast.Fn {
	if node.is_method {
		mut candidate_types := []ast.Type{}
		for typ in [node.receiver_type, node.left_type, c.unwrap_generic(node.receiver_type),
			c.unwrap_generic(node.left_type)] {
			if typ != 0 && typ !in candidate_types {
				candidate_types << typ
			}
		}
		for typ in candidate_types {
			sym := c.table.sym(c.unwrap_generic(typ))
			if method := c.table.find_method(sym, node.name) {
				return method
			}
			if method := c.table.find_method_with_embeds(sym, node.name) {
				return method
			}
		}
		return none
	}
	return c.table.find_fn(node.name)
}

fn (c &Checker) call_arg_param_index(func ast.Fn, arg_idx int) int {
	offset := if func.is_method { 1 } else { 0 }
	if func.is_variadic && func.params.len > 0 && arg_idx + offset >= func.params.len - 1 {
		return func.params.len - 1
	}
	param_idx := arg_idx + offset
	if param_idx >= func.params.len {
		return -1
	}
	return param_idx
}

fn (mut c Checker) assign_stmt_aliases_visible_state(node ast.AssignStmt, root_name string, root_type ast.Type) bool {
	mut pair_count := node.left.len
	if node.right.len < pair_count {
		pair_count = node.right.len
	}
	for i in 0 .. pair_count {
		right_vis := c.expr_mutation_visibility(node.right[i], root_name, root_type)
		if !is_visible_root_mutation(right_vis) {
			continue
		}
		right_type := if i < node.right_types.len {
			node.right_types[i]
		} else {
			ast.no_type
		}
		if !c.type_may_share_mutable_storage(right_type) {
			continue
		}
		mut left_expr := node.left[i]
		left_expr = left_expr.remove_par()
		if left_expr is ast.Ident && left_expr.is_mut() {
			return true
		}
	}
	return false
}

fn (mut c Checker) type_may_share_mutable_storage(typ ast.Type) bool {
	if typ == 0 || typ == ast.no_type {
		return false
	}
	unwrapped := c.unwrap_generic(typ)
	if unwrapped.is_any_kind_of_pointer() || unwrapped.has_flag(.shared_f) {
		return true
	}
	return c.table.final_sym(unwrapped).kind in [.array, .map, .chan, .interface, .thread, .function]
}
