// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast

// error_type_name returns a proper type name reference for error messages
// ? => Option type
// ! => Result type
// others => type `name`
fn (mut c Checker) error_type_name(exp_type ast.Type) string {
	return if exp_type == ast.void_type.set_flag(.result) {
		'Result type'
	} else if exp_type == ast.void_type.set_flag(.option) {
		'Option type'
	} else {
		'type `${c.table.type_to_str(exp_type)}`'
	}
}

@[inline]
fn (mut c Checker) error_unaliased_type_name(exp_type ast.Type) string {
	return c.error_type_name(c.table.unaliased_type(exp_type))
}

// TODO: non deferred
fn (mut c Checker) return_stmt(mut node ast.Return) {
	if c.table.cur_fn == unsafe { nil } {
		return
	}
	prev_inside_return := c.inside_return
	c.inside_return = true
	defer {
		c.inside_return = prev_inside_return
	}

	// check `defer_stmts` in return, to ensure the same behavior with `cgen`
	old_inside_defer := c.inside_defer
	c.inside_defer = true
	for i := c.table.cur_fn.defer_stmts.len - 1; i >= 0; i-- {
		c.stmts(mut c.table.cur_fn.defer_stmts[i].stmts)
	}
	c.inside_defer = old_inside_defer

	c.expected_type = c.table.cur_fn.return_type
	mut expected_type := c.unwrap_generic(c.expected_type)
	if expected_type != 0 && c.table.sym(expected_type).kind == .alias {
		unaliased_type := c.table.unaliased_type(expected_type)
		if unaliased_type.has_option_or_result() {
			expected_type = unaliased_type
		}
	}
	expected_type_sym := c.table.sym(expected_type)
	if expected_type_sym.info is ast.ArrayFixed {
		c.table.find_or_register_array_fixed(expected_type_sym.info.elem_type,
			expected_type_sym.info.size, expected_type_sym.info.size_expr, true)
	}
	if node.exprs.len > 0 && c.table.cur_fn.return_type == ast.void_type {
		c.error('unexpected argument, current function does not return anything',
			node.exprs[0].pos())
		return
	} else if node.exprs.len > 1 && c.table.cur_fn.return_type == ast.void_type.set_flag(.option) {
		c.error('can only return `none` from an Option-only return function', node.exprs[0].pos())
		return
	} else if node.exprs.len > 1 && c.table.cur_fn.return_type == ast.void_type.set_flag(.result) {
		c.error('functions with Result-only return types can only return an error',
			node.exprs[0].pos())
		return
	} else if node.exprs.len == 0 && !(c.expected_type == ast.void_type
		|| expected_type_sym.kind == .void) {
		stype := c.table.type_to_str(expected_type)
		arg := if expected_type_sym.kind == .multi_return { 'arguments' } else { 'argument' }
		c.error('expected `${stype}` ${arg}', node.pos)
		return
	}
	if node.exprs.len == 0 {
		return
	}
	exp_is_option := expected_type.has_flag(.option)
	exp_is_result := expected_type.has_flag(.result)
	mut expected_types := [expected_type]
	if expected_type_sym.info is ast.MultiReturn {
		expected_types = expected_type_sym.info.types.clone()
		if c.table.cur_concrete_types.len > 0 {
			expected_types = expected_types.map(c.unwrap_generic(it))
			c.table.used_features.comptime_syms[c.table.find_or_register_multi_return(expected_types)] = true
		}
	}
	mut got_types := []ast.Type{}
	mut expr_idxs := []int{}
	for i, mut expr in node.exprs {
		mut typ := c.expr(mut expr)
		if typ == 0 {
			return
		}
		// Handle `return unsafe { none }`
		if mut expr is ast.UnsafeExpr && expr.expr is ast.None {
			c.error('cannot return `none` in unsafe block', expr.expr.pos)
		}
		if typ == ast.void_type {
			c.error('`${expr}` used as value', node.pos)
			return
		}
		// Unpack multi return types
		sym := c.table.sym(typ)
		if sym.kind == .multi_return {
			if i > 0 || i != node.exprs.len - 1 {
				c.error('cannot use multi-return with other return types', expr.pos())
			}
			for t in sym.mr_info().types {
				got_types << t
				expr_idxs << i
			}
		} else {
			if mut expr is ast.Ident && expr.obj is ast.Var {
				if expr.obj.smartcasts.len > 0 {
					typ = c.unwrap_generic(c.visible_var_type_for_read(expr.obj))
				}
				if expr.obj.ct_type_var != .no_comptime {
					typ = c.type_resolver.get_type_or_default(expr, typ)
				}
				if expr.obj.expr is ast.IfGuardExpr {
					if var := expr.scope.find_var(expr.name) {
						typ = var.typ
					}
				}
			} else if mut expr is ast.SelectorExpr {
				if expr.expr_type != 0 {
					scope_field := expr.scope.find_struct_field(smartcast_selector_expr_str(expr),
						expr.expr_type, expr.field_name)
					if scope_field != unsafe { nil } && scope_field.smartcasts.len > 0 {
						typ = c.unwrap_generic(c.exposed_smartcast_type(scope_field.orig_type,
							scope_field.smartcasts.last(), scope_field.is_mut))
					}
				}
			}
			got_types << typ
			expr_idxs << i
		}
	}
	node.types = got_types
	$if debug_manualfree ? {
		cfn := c.table.cur_fn
		if cfn.is_manualfree {
			pnames := cfn.params.map(it.name)
			for expr in node.exprs {
				if expr is ast.Ident {
					if expr.name in pnames {
						c.note('returning a parameter in a fn marked with `@[manualfree]` can cause double freeing in the caller',
							node.pos)
					}
				}
			}
		}
	}
	// allow `none` & `error` return types for function that returns option
	option_type_idx := c.table.type_idxs['_option']
	result_type_idx := c.table.type_idxs['_result']
	got_types_0_idx := got_types[0].idx()
	if exp_is_option && got_types_0_idx == ast.error_type_idx {
		c.error('Option and Result types have been split, use `!Foo` to return errors', node.pos)
	} else if exp_is_result && got_types_0_idx == ast.none_type_idx {
		c.error('Option and Result types have been split, use `?` to return none', node.pos)
	}
	expected_fn_return_type_has_option := c.table.cur_fn.return_type.has_flag(.option)
	expected_fn_return_type_has_result := c.table.cur_fn.return_type.has_flag(.result)
	if exp_is_option && expected_fn_return_type_has_result {
		if got_types_0_idx == ast.none_type_idx {
			c.error('cannot use `none` as ${c.error_type_name(c.table.unaliased_type(c.table.cur_fn.return_type))} in return argument',
				node.pos)
			return
		}
		return
	}
	if exp_is_result && expected_fn_return_type_has_option {
		c.error('expecting to return a ?Type, but you are returning ${c.error_type_name(expected_type)} instead',
			node.pos)
		return
	}
	if (exp_is_option
		&& got_types_0_idx in [ast.none_type_idx, ast.error_type_idx, option_type_idx])
		|| (exp_is_result && got_types_0_idx in [ast.error_type_idx, result_type_idx]) {
		return
	}
	if expected_types.len > 0 && expected_types.len != got_types.len {
		// `fn foo() !(int, string) { return Err{} }`
		if (exp_is_option || exp_is_result) && node.exprs.len == 1 {
			mut expr_ := node.exprs[0]
			got_type := c.expr(mut expr_)
			got_type_sym := c.table.sym(got_type)
			if got_type_sym.kind == .struct && c.type_implements(got_type, ast.error_type, node.pos) {
				node.exprs[0] = ast.CastExpr{
					expr:      node.exprs[0]
					typname:   'IError'
					typ:       ast.error_type
					expr_type: got_type
					pos:       node.pos
				}
				node.types[0] = ast.error_type
				return
			}
		}
		arg := if expected_types.len == 1 { 'argument' } else { 'arguments' }
		midx := int_max(0, int_min(expected_types.len, expr_idxs.len - 1))
		mismatch_pos := node.exprs[expr_idxs[midx]].pos()
		c.error('expected ${expected_types.len} ${arg}, but got ${got_types.len}', mismatch_pos)
		return
	}
	for i, exp_type in expected_types {
		exprv := node.exprs[expr_idxs[i]]
		if exprv is ast.Ident && exprv.or_expr.kind == .propagate_option {
			if exp_type.has_flag(.option) {
				c.warn('unwrapping option is redundant as the function returns option', node.pos)
			} else {
				c.error('should not unwrap option var on return, it could be none', node.pos)
			}
		}
		got_type := c.unwrap_generic(got_types[i])
		if got_type.has_flag(.option) && (!exp_type.has_flag(.option)
			|| !c.check_types(got_type.clear_flag(.option), exp_type.clear_flag(.option))) {
			c.error('cannot use `${c.table.type_to_str(got_type)}` as ${c.error_type_name(exp_type)} in return argument',
				exprv.pos())
		}
		if got_type.has_flag(.result) && (!exp_type.has_flag(.result)
			|| c.table.type_to_str(got_type) != c.table.type_to_str(exp_type)) {
			c.error('cannot use `${c.table.type_to_str(got_type)}` as ${c.error_type_name(exp_type)} in return argument',
				exprv.pos())
		}
		if exprv is ast.ComptimeCall && exprv.kind == .tmpl
			&& c.table.final_sym(exp_type).kind != .string {
			c.error('cannot use `string` as type `${c.table.type_to_str(exp_type)}` in return argument',
				exprv.pos)
		}
		if exprv !is ast.ComptimeCall {
			got_type_sym := c.table.sym(got_type)
			exp_type_sym := c.table.sym(exp_type)
			if c.check_types(got_type, exp_type) {
				if exp_type.is_unsigned() && got_type.is_int_literal() {
					if exprv is ast.IntegerLiteral {
						var := (node.exprs[expr_idxs[i]] as ast.IntegerLiteral).val
						if var[0] == `-` {
							c.note('cannot use a negative value as value of ${c.error_type_name(exp_type)} in return argument',
								exprv.pos)
						}
					}
				}
			} else {
				if c.pref.skip_unused && got_types[i].has_flag(.generic) {
					c.table.used_features.comptime_syms[got_type] = true
				}
				if exp_type_sym.kind == .interface
					|| (exp_type_sym.kind == .generic_inst && exp_type_sym.info is ast.GenericInst
					&& c.table.type_symbols[exp_type_sym.info.parent_idx].kind == .interface) {
					if c.type_implements(got_type, exp_type, node.pos) {
						if !got_type.is_any_kind_of_pointer() && got_type_sym.kind != .interface
							&& !c.inside_unsafe {
							c.mark_as_referenced(mut &node.exprs[expr_idxs[i]], true)
						}
					}
					continue
				}
				exp_final_sym := c.table.final_sym(exp_type)
				got_final_sym := c.table.final_sym(got_type)
				if exp_final_sym.kind == .array_fixed && got_final_sym.kind == .array_fixed {
					got_arr_sym := c.table.sym(c.cast_to_fixed_array_ret(got_type, got_final_sym))
					if (exp_final_sym.info as ast.ArrayFixed).is_compatible(got_arr_sym.info as ast.ArrayFixed) {
						continue
					}
				}
				// `fn foo() !int { return Err{} }`
				if expected_fn_return_type_has_result && got_type_sym.kind == .struct
					&& c.type_implements(got_type, ast.error_type, node.pos) {
					node.exprs[expr_idxs[i]] = ast.CastExpr{
						expr:      exprv
						typname:   'IError'
						typ:       ast.error_type
						expr_type: got_type
						pos:       node.pos
					}
					node.types[expr_idxs[i]] = ast.error_type
					continue
				}
				got_type_name := if got_type_sym.kind == .function {
					'${c.table.type_to_str(got_type)}'
				} else {
					got_type_sym.name
				}
				// ignore generic lambda return in this phase
				if c.inside_lambda && exp_type.has_flag(.generic) {
					continue
				}
				if exp_final_sym.info is ast.ArrayFixed
					&& c.array_fixed_has_unresolved_size(exp_final_sym.info) {
					continue
				}
				// In generic functions, scope variable types can be stale from a
				// different instantiation pass. Skip the error if the original
				// return type is generic — the cgen resolves types per-instantiation.
				if c.table.cur_fn != unsafe { nil } && c.table.cur_fn.return_type.has_flag(.generic)
					&& c.table.cur_concrete_types.len > 0 {
					continue
				}

				c.error('cannot use `${got_type_name}` as ${c.error_type_name(exp_type)} in return argument',
					exprv.pos())
			}
		}
		if exprv is ast.Ident {
			if exprv.obj is ast.Var && exprv.obj.smartcasts.len > 0 {
				orig_sym := c.table.final_sym(exprv.obj.orig_type)
				if orig_sym.kind == .interface {
					continue
				}
			}
		}
		if got_type.is_any_kind_of_pointer() && !exp_type.is_any_kind_of_pointer()
			&& !c.table.unaliased_type(exp_type).is_any_kind_of_pointer() {
			if exprv.is_auto_deref_var() {
				continue
			}
			c.add_error_detail('use `return *pointer` instead of `return pointer`, and just `return value` instead of `return &value`')
			c.error('fn `${c.table.cur_fn.name}` expects you to return a non reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_type)}` instead',
				exprv.pos())
		}
		if exp_type.is_any_kind_of_pointer() && !got_type.is_any_kind_of_pointer()
			&& !c.table.unaliased_type(got_type).is_any_kind_of_pointer()
			&& got_type != ast.int_literal_type && !c.pref.translated && !c.file.is_translated {
			if exprv.is_auto_deref_var() {
				continue
			}
			if c.table.final_sym(exp_type).kind == .interface
				&& c.table.final_sym(got_type).kind == .interface
				&& exp_type.deref().idx() == got_type.idx() {
				continue
			}
			c.error('fn `${c.table.cur_fn.name}` expects you to return a reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_type)}` instead',
				exprv.pos())
		}
		if exp_type.is_ptr() && got_type.is_ptr() {
			r_expr := node.exprs[expr_idxs[i]]
			if r_expr is ast.Ident {
				mut ident_expr := r_expr
				c.fail_if_stack_struct_action_outside_unsafe(mut ident_expr, 'returned')
			} else if r_expr is ast.PrefixExpr && r_expr.op == .amp {
				// &var
				if r_expr.right is ast.Ident {
					mut ident_expr := r_expr.right
					c.fail_if_stack_struct_action_outside_unsafe(mut ident_expr, 'returned')
				}
			}
		}
	}
	if exp_is_option && node.exprs.len > 0 {
		expr0 := node.exprs[0]
		if expr0 is ast.CallExpr {
			if expr0.or_block.kind == .propagate_option && node.exprs.len == 1 {
				v_name := if expr0.is_static_method {
					expr0.name.all_before('__static__') + '.' + expr0.name.all_after('__static__')
				} else {
					expr0.name
				}
				c.error('`?` is not needed, use `return ${v_name}()`', expr0.pos)
			}
		}
	}
}

fn (mut c Checker) find_unreachable_statements_after_noreturn_calls(stmts []ast.Stmt) {
	mut prev_stmt_was_noreturn_call := false
	for stmt in stmts {
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.CallExpr {
				if prev_stmt_was_noreturn_call {
					c.warn('unreachable code after a @[noreturn] call', stmt.pos)
					return
				}
				prev_stmt_was_noreturn_call = stmt.expr.is_noreturn
			}
		} else {
			prev_stmt_was_noreturn_call = false
		}
	}
}

// Note: has_top_return/1 should be called on *already checked* stmts,
// which do have their stmt.expr.is_noreturn set properly:
fn (mut c Checker) has_top_return(stmts []ast.Stmt) bool {
	for stmt in stmts {
		match stmt {
			ast.Return {
				return true
			}
			ast.Block {
				if c.has_top_return(stmt.stmts) {
					return true
				}
			}
			ast.ExprStmt {
				if c.expr_never_falls_through(stmt.expr) {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn (mut c Checker) expr_never_falls_through(expr ast.Expr) bool {
	match expr {
		ast.CallExpr {
			// do not ignore panic() calls on non checked stmts
			return expr.is_noreturn
				|| (expr.is_method == false && expr.name == 'panic')
				|| c.call_expr_propagates_always_error(expr)
		}
		ast.ComptimeCall {
			return expr.kind == .compile_error
		}
		ast.LockExpr {
			return c.has_top_return(expr.stmts)
		}
		ast.IfExpr {
			return c.has_top_return_in_if_expr(expr)
		}
		ast.MatchExpr {
			return c.has_top_return_in_match_expr(expr)
		}
		else {}
	}

	return false
}

fn (mut c Checker) call_expr_propagates_always_error(node ast.CallExpr) bool {
	if node.should_be_skipped || node.or_block.kind != .propagate_result {
		return false
	}
	mut func := ast.Fn{}
	if node.is_method {
		if node.left_type == 0 {
			return false
		}
		left_sym := c.table.sym(c.unwrap_generic(node.left_type))
		if left_sym.kind == .placeholder {
			return false
		}
		func = c.table.find_method(left_sym, node.name) or { return false }
	} else {
		if node.name == '' {
			return false
		}
		func = c.table.find_fn(node.name) or { return false }
	}
	return c.fn_always_errors(func)
}

fn (mut c Checker) fn_always_errors(func ast.Fn) bool {
	if func.name == '' || func.source_fn == unsafe { nil } || func.no_body || func.language != .v
		|| !func.return_type.has_flag(.result) {
		return false
	}
	fkey := func.fkey()
	if fkey in c.always_error_fn_cache {
		return c.always_error_fn_cache[fkey]
	}
	if fkey in c.always_error_fn_in_progress {
		return false
	}
	c.always_error_fn_in_progress[fkey] = true
	defer {
		c.always_error_fn_in_progress.delete(fkey)
	}
	fn_decl := unsafe { &ast.FnDecl(func.source_fn) }
	result := c.stmts_always_error(fn_decl.stmts)
	c.always_error_fn_cache[fkey] = result
	return result
}

fn (mut c Checker) stmts_always_error(stmts []ast.Stmt) bool {
	for stmt in stmts {
		if c.stmt_always_errors(stmt) {
			return true
		}
	}
	return false
}

fn (mut c Checker) stmt_always_errors(stmt ast.Stmt) bool {
	match stmt {
		ast.Return {
			return c.return_stmt_always_errors(stmt)
		}
		ast.Block {
			return c.stmts_always_error(stmt.stmts)
		}
		ast.ExprStmt {
			return c.expr_always_errors(stmt.expr)
		}
		ast.ForStmt {
			return stmt.is_inf && stmt.stmts.len == 0
		}
		else {}
	}

	return false
}

fn (mut c Checker) expr_always_errors(expr ast.Expr) bool {
	match expr {
		ast.CallExpr {
			return expr.is_noreturn || c.call_expr_propagates_always_error(expr)
		}
		ast.ComptimeCall {
			return expr.kind == .compile_error
		}
		ast.IfExpr {
			return c.if_expr_always_errors(expr)
		}
		ast.MatchExpr {
			return c.match_expr_always_errors(expr)
		}
		ast.LockExpr {
			return c.stmts_always_error(expr.stmts)
		}
		else {}
	}

	return false
}

fn (mut c Checker) return_stmt_always_errors(node ast.Return) bool {
	if node.types.len == 1 && c.table.unaliased_type(node.types[0]).idx() == ast.error_type_idx {
		return true
	}
	return false
}

fn (mut c Checker) has_top_return_in_if_expr(if_expr ast.IfExpr) bool {
	if if_expr.branches.len < 2 || !if_expr.has_else {
		return false
	}
	for branch in if_expr.branches {
		if !c.has_top_return(branch.stmts) {
			return false
		}
	}
	return true
}

fn (mut c Checker) if_expr_always_errors(if_expr ast.IfExpr) bool {
	if if_expr.branches.len < 2 || !if_expr.has_else {
		return false
	}
	for branch in if_expr.branches {
		if !c.stmts_always_error(branch.stmts) {
			return false
		}
	}
	return true
}

fn (mut c Checker) has_top_return_in_match_expr(match_expr ast.MatchExpr) bool {
	if match_expr.branches.len == 0 {
		return false
	}
	if match_expr.is_comptime {
		mut has_else := false
		for branch in match_expr.branches {
			if branch.is_else {
				has_else = true
				break
			}
			for expr in branch.exprs {
				if expr is ast.Ident && expr.name == '\$else' {
					has_else = true
					break
				}
			}
			if has_else {
				break
			}
		}
		if has_else {
			for branch in match_expr.branches {
				if !c.has_top_return(branch.stmts) {
					return false
				}
			}
			return true
		}
		return false
	}
	for branch in match_expr.branches {
		if !c.has_top_return(branch.stmts) {
			return false
		}
	}
	return true
}

fn (mut c Checker) match_expr_always_errors(match_expr ast.MatchExpr) bool {
	if match_expr.branches.len == 0 {
		return false
	}
	if match_expr.is_comptime {
		mut has_else := false
		for branch in match_expr.branches {
			if branch.is_else {
				has_else = true
				break
			}
			for expr in branch.exprs {
				if expr is ast.Ident && expr.name == '\$else' {
					has_else = true
					break
				}
			}
			if has_else {
				break
			}
		}
		if !has_else {
			return false
		}
	}
	for branch in match_expr.branches {
		if !c.stmts_always_error(branch.stmts) {
			return false
		}
	}
	return true
}

fn (mut c Checker) check_noreturn_fn_decl(mut node ast.FnDecl) {
	if !node.is_noreturn {
		return
	}
	if node.no_body {
		return
	}
	if node.return_type != ast.void_type {
		c.error('[noreturn] functions cannot have return types', node.pos)
	}
	if uses_return_stmt(node.stmts) {
		c.error('[noreturn] functions cannot use return statements', node.pos)
	}
	mut pos := node.pos
	mut is_valid_end_of_noreturn_fn := false
	if node.stmts.len != 0 {
		last_stmt := node.stmts.last()
		match last_stmt {
			ast.ExprStmt {
				if last_stmt.expr is ast.CallExpr {
					if last_stmt.expr.should_be_skipped {
						c.error('@[noreturn] functions cannot end with a skippable `@[if ..]` call',
							last_stmt.pos)
						return
					}
					if last_stmt.expr.is_noreturn {
						is_valid_end_of_noreturn_fn = true
					}
				}
			}
			ast.ForStmt {
				if last_stmt.is_inf && last_stmt.stmts.len == 0 {
					is_valid_end_of_noreturn_fn = true
				}
			}
			else {}
		}

		if !is_valid_end_of_noreturn_fn {
			pos = last_stmt.pos
		}
	}
	if !is_valid_end_of_noreturn_fn {
		c.error('@[noreturn] functions should end with a call to another @[noreturn] function, or with an infinite `for {}` loop',
			pos)
	}
}

fn uses_return_stmt(stmts []ast.Stmt) bool {
	if stmts.len == 0 {
		return false
	}
	for stmt in stmts {
		match stmt {
			ast.Return {
				return true
			}
			ast.Block {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ExprStmt {
				match stmt.expr {
					ast.CallExpr {
						if uses_return_stmt(stmt.expr.or_block.stmts) {
							return true
						}
					}
					ast.MatchExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					ast.SelectExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					ast.IfExpr {
						for b in stmt.expr.branches {
							if uses_return_stmt(b.stmts) {
								return true
							}
						}
					}
					else {}
				}
			}
			ast.ForStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ForCStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			ast.ForInStmt {
				if uses_return_stmt(stmt.stmts) {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn is_noreturn_callexpr(expr ast.Expr) bool {
	if expr is ast.CallExpr {
		return expr.is_noreturn
	}
	return false
}
