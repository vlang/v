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
	if param_idx < 0 || param_idx >= func.params.len
		|| (!func.params[param_idx].is_mut && !func.params[param_idx].typ.is_any_kind_of_pointer()) {
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
	if param_idx < 0 || param_idx >= fn_decl.params.len
		|| (!fn_decl.params[param_idx].is_mut
		&& !fn_decl.params[param_idx].typ.is_any_kind_of_pointer()) {
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

fn (mut c Checker) fn_pointer_param_may_escape_or_mutate(func ast.Fn, param_idx int, param_type ast.Type) bool {
	mut seen := map[string]bool{}
	return c.fn_param_may_replace_or_escape(func, param_idx, param_type, mut seen)
}

fn (mut c Checker) fn_param_may_replace_or_escape(func ast.Fn, param_idx int, param_type ast.Type, mut seen map[string]bool) bool {
	if param_idx < 0 || param_idx >= func.params.len
		|| (!func.params[param_idx].is_mut && !param_type.is_any_kind_of_pointer()
		&& !c.table.unaliased_type(param_type).is_any_kind_of_pointer()) {
		return true
	}
	if func.no_body || func.language != .v {
		return true
	}
	if func.source_fn == unsafe { nil } {
		// The final pending-call pass rechecks same-build helpers after their bodies are available.
		return false
	}
	key := '${func.fkey()}|${param_idx}|${param_type}'
	if key in seen {
		return false
	}
	seen[key] = true
	defer {
		seen.delete(key)
	}
	fn_decl := unsafe { &ast.FnDecl(func.source_fn) }
	if fn_decl == unsafe { nil } || param_idx >= fn_decl.params.len {
		return true
	}
	mut aliases := [fn_decl.params[param_idx].name]
	for stmt in fn_decl.stmts {
		if c.node_captures_or_stores_pointer_param(stmt, param_type, func.params[param_idx].is_mut, mut
			aliases, mut seen)
		{
			return true
		}
	}
	return false
}

fn (mut c Checker) expr_references_pointer_param(expr ast.Expr, typ ast.Type, aliases []string) bool {
	return aliases.any(is_visible_root_mutation(c.expr_mutation_visibility(expr, it, typ)))
}

fn (mut c Checker) ident_is_local_pointer_alias(ident ast.Ident) bool {
	if ident.obj is ast.Var {
		return !ident.obj.is_arg && !ident.obj.is_static && !ident.obj.is_inherited
			&& (ident.obj.typ.is_any_kind_of_pointer()
			|| c.table.unaliased_type(ident.obj.typ).is_any_kind_of_pointer())
	}
	if ident.scope != unsafe { nil } {
		if variable := ident.scope.find_var(ident.name) {
			return !variable.is_arg && !variable.is_static && !variable.is_inherited
				&& (variable.typ.is_any_kind_of_pointer()
				|| c.table.unaliased_type(variable.typ).is_any_kind_of_pointer())
		}
	}
	return false
}

fn (mut c Checker) pointer_param_field_target(expr ast.Expr, typ ast.Type, aliases []string) bool {
	reduced := expr.remove_par()
	return match reduced {
		ast.SelectorExpr {
			c.expr_references_pointer_param(reduced.expr, typ, aliases)
				|| c.pointer_param_field_target(reduced.expr, typ, aliases)
		}
		ast.IndexExpr {
			c.pointer_param_field_target(reduced.left, typ, aliases)
		}
		ast.CastExpr {
			c.pointer_param_field_target(reduced.expr, typ, aliases)
		}
		ast.AsCast {
			c.pointer_param_field_target(reduced.expr, typ, aliases)
		}
		ast.UnsafeExpr {
			c.pointer_param_field_target(reduced.expr, typ, aliases)
		}
		else {
			false
		}
	}
}

fn (mut c Checker) node_captures_or_stores_pointer_param(node ast.Node, typ ast.Type, root_is_mut bool, mut aliases []string, mut seen map[string]bool) bool {
	match node {
		ast.Expr {
			if node is ast.AnonFn {
				return node.inherited_vars.any(it.name in aliases)
			}
			if node is ast.CallExpr && c.call_escapes_pointer_param(node, typ, aliases, mut seen) {
				return true
			}
		}
		ast.Stmt {
			if node is ast.FnDecl {
				return false
			}
			if node is ast.Return
				&& node.exprs.any(c.return_expr_contains_pointer_param(it, aliases)) {
				return true
			}
			if node is ast.AssignStmt {
				for i, right in node.right {
					left := if i < node.left.len {
						node.left[i].remove_par()
					} else {
						ast.empty_expr
					}
					if c.expr_references_pointer_param(left, typ, aliases) {
						if left is ast.Ident {
							if root_is_mut && left.name == aliases[0] {
								return true
							}
							continue
						}
						if !c.pointer_param_field_target(left, typ, aliases) {
							return true
						}
					}
					if !c.expr_references_pointer_param(right, typ, aliases) {
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
					if left is ast.Ident && c.ident_is_local_pointer_alias(left) {
						if left.name !in aliases {
							aliases << left.name
						}
					} else {
						return true
					}
				}
			}
		}
		else {}
	}

	for child in node.children() {
		if c.node_captures_or_stores_pointer_param(child, typ, root_is_mut, mut aliases, mut seen) {
			return true
		}
	}
	return false
}

fn (mut c Checker) call_escapes_pointer_param(node ast.CallExpr, typ ast.Type, aliases []string, mut seen map[string]bool) bool {
	called_fn := c.find_called_fn(node) or {
		if node.is_method && c.expr_references_pointer_param(node.left, typ, aliases) {
			return true
		}
		return node.args.any(c.expr_references_pointer_param(it.expr, typ, aliases))
	}
	if node.is_method && called_fn.params.len > 0
		&& (called_fn.params[0].is_mut || called_fn.params[0].typ.is_any_kind_of_pointer())
		&& c.expr_references_pointer_param(node.left, typ, aliases)
		&& c.fn_param_may_replace_or_escape(called_fn, 0, node.left_type, mut seen) {
		return true
	}
	for i, arg in node.args {
		if !c.expr_references_pointer_param(arg.expr, typ, aliases) {
			continue
		}
		param_idx := c.call_arg_param_index(called_fn, i)
		if param_idx < 0 || param_idx >= called_fn.params.len {
			return true
		}
		resolved_type := if arg.typ != ast.no_type {
			arg.typ
		} else {
			called_fn.params[param_idx].typ
		}
		if (called_fn.params[param_idx].is_mut || resolved_type.is_any_kind_of_pointer()
			|| c.table.unaliased_type(resolved_type).is_any_kind_of_pointer())
			&& c.fn_param_may_replace_or_escape(called_fn, param_idx, resolved_type, mut seen) {
			return true
		}
	}
	return false
}

fn (mut c Checker) return_expr_contains_pointer_param(expr ast.Expr, aliases []string) bool {
	reduced := expr.remove_par()
	return match reduced {
		ast.Ident {
			reduced.name in aliases
		}
		ast.CastExpr {
			c.return_expr_contains_pointer_param(reduced.expr, aliases)
				|| (reduced.has_arg && c.return_expr_contains_pointer_param(reduced.arg, aliases))
		}
		ast.AsCast {
			c.return_expr_contains_pointer_param(reduced.expr, aliases)
		}
		ast.UnsafeExpr {
			c.return_expr_contains_pointer_param(reduced.expr, aliases)
		}
		ast.IfExpr {
			reduced.branches.any(c.stmts_return_pointer_param(it.stmts, aliases))
		}
		ast.MatchExpr {
			reduced.branches.any(c.stmts_return_pointer_param(it.stmts, aliases))
		}
		ast.ArrayInit {
			reduced.exprs.any(c.return_expr_contains_pointer_param(it, aliases))
				|| (reduced.has_update_expr
				&& c.return_expr_contains_pointer_param(reduced.update_expr, aliases))
		}
		ast.MapInit {
			reduced.keys.any(c.return_expr_contains_pointer_param(it, aliases))
				|| reduced.vals.any(c.return_expr_contains_pointer_param(it, aliases))
				|| (reduced.has_update_expr
				&& c.return_expr_contains_pointer_param(reduced.update_expr, aliases))
		}
		ast.StructInit {
			reduced.init_fields.any(c.return_expr_contains_pointer_param(it.expr, aliases))
				|| (reduced.has_update_expr
				&& c.return_expr_contains_pointer_param(reduced.update_expr, aliases))
		}
		ast.SelectorExpr {
			c.type_may_share_mutable_storage(reduced.typ)
				&& c.return_expr_contains_pointer_param(reduced.expr, aliases)
		}
		ast.IndexExpr {
			c.type_may_share_mutable_storage(reduced.typ)
				&& c.return_expr_contains_pointer_param(reduced.left, aliases)
		}
		else {
			false
		}
	}
}

fn (mut c Checker) stmts_return_pointer_param(stmts []ast.Stmt, aliases []string) bool {
	if stmts.len == 0 {
		return false
	}
	last_stmt := stmts.last()
	return match last_stmt {
		ast.ExprStmt {
			c.return_expr_contains_pointer_param(last_stmt.expr, aliases)
		}
		ast.Return {
			last_stmt.exprs.any(c.return_expr_contains_pointer_param(it, aliases))
		}
		else {
			false
		}
	}
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
			if is_visible_root_mutation(c.expr_mutation_visibility(expr.expr, root_name, root_type)) {
				return true
			}
		}
		ast.InfixExpr {
			if expr.op == .left_shift {
				if is_visible_root_mutation(c.expr_mutation_visibility(expr.left, root_name, root_type))
					|| is_visible_root_mutation(c.expr_mutation_visibility(expr.right, root_name, root_type)) {
					return true
				}
			}
			if expr.op == .arrow
				&& is_visible_root_mutation(c.expr_mutation_visibility(expr.right, root_name, root_type)) {
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
	if current is ast.AsCast {
		return c.expr_mutation_visibility(current.expr, root_name, root_type)
	}
	if current is ast.UnsafeExpr {
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
			if called_fn.params.len > 0
				&& (called_fn.params[0].is_mut || called_fn.params[0].typ.is_any_kind_of_pointer()) {
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
			if is_visible_root_mutation(c.expr_mutation_visibility(arg.expr, root_name, root_type)) {
				return true
			}
		}
		return false
	}
	for i, arg in node.args {
		param_idx := c.call_arg_param_index(called_fn, i)
		arg_vis := c.expr_mutation_visibility(arg.expr, root_name, root_type)
		if param_idx < 0 || param_idx >= called_fn.params.len {
			if is_visible_root_mutation(arg_vis) {
				return true
			}
			continue
		}
		if !called_fn.params[param_idx].is_mut
			&& !called_fn.params[param_idx].typ.is_any_kind_of_pointer() {
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
