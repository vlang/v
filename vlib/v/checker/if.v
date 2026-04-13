// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token
import v.util
import strings

fn (c &Checker) simple_expr_name_for_dead_branch_check(expr ast.Expr) string {
	mut current_expr := expr
	current_expr = current_expr.remove_par()
	if current_expr !in [ast.Ident, ast.SelectorExpr] {
		return ''
	}
	return current_expr.str()
}

fn (mut c Checker) is_always_true_self_comparison(cond ast.Expr) bool {
	mut cond_expr := cond
	cond_expr = cond_expr.remove_par()
	if mut cond_expr is ast.InfixExpr {
		if cond_expr.op != .eq {
			return false
		}
		left_name := c.simple_expr_name_for_dead_branch_check(cond_expr.left)
		right_name := c.simple_expr_name_for_dead_branch_check(cond_expr.right)
		if left_name == '' || left_name != right_name {
			return false
		}
		mut left_type := if cond_expr.left_type == ast.no_type {
			c.get_expr_type(cond_expr.left)
		} else {
			cond_expr.left_type
		}
		mut right_type := if cond_expr.right_type == ast.no_type {
			c.get_expr_type(cond_expr.right)
		} else {
			cond_expr.right_type
		}
		left_type = c.table.unaliased_type(left_type)
		right_type = c.table.unaliased_type(right_type)
		if left_type.is_float() || right_type.is_float() {
			// `NaN == NaN` is always false.
			return false
		}
		return true
	}
	return false
}

fn (c &Checker) is_interface_smartcast_sumtype_variant(expr ast.Expr, expected_type ast.Type, got_type ast.Type) bool {
	sum_type := expected_type.clear_option_and_result()
	if c.table.type_kind(sum_type) != .sum_type || !got_type.is_ptr() {
		return false
	}
	if expr is ast.Ident && expr.obj is ast.Var {
		return c.table.is_interface_var(expr.obj)
			&& c.table.is_sumtype_or_in_variant(sum_type, got_type.deref())
	}
	return false
}

// gen_branch_context_string generate current branches context string.
// context include generic types, `$for`.
fn (mut c Checker) gen_branch_context_string() string {
	mut arr := []string{}

	// gen `T=int,X=string`
	if !isnil(c.table.cur_fn) && c.table.cur_fn.generic_names.len == c.table.cur_concrete_types.len {
		for i in 0 .. c.table.cur_fn.generic_names.len {
			arr << c.table.cur_fn.generic_names[i] + '=' +
				util.strip_main_name(c.table.type_to_str(c.table.cur_concrete_types[i]))
		}
	}

	// gen comptime `$for`
	if c.comptime.inside_comptime_for {
		// variants
		if c.comptime.comptime_for_variant_var.len > 0 {
			variant := c.table.type_to_str(c.type_resolver.get_ct_type_or_default('${c.comptime.comptime_for_variant_var}.typ',
				ast.no_type))
			arr << c.comptime.comptime_for_variant_var + '.typ=' + variant
		}
		// fields
		if c.comptime.comptime_for_field_var.len > 0 {
			arr << c.comptime.comptime_for_field_var + '.name=' +
				c.comptime.comptime_for_field_value.name
		}
		// values
		if c.comptime.comptime_for_enum_var.len > 0 {
			enum_var := c.table.type_to_str(c.type_resolver.get_ct_type_or_default('${c.comptime.comptime_for_enum_var}.typ',
				ast.void_type))
			arr << c.comptime.comptime_for_enum_var + '.typ=' + enum_var
		}
		// attributes
		if c.comptime.comptime_for_attr_var.len > 0 {
			arr << c.comptime.comptime_for_attr_var + '.name=' +
				c.comptime.comptime_for_attr_value.name
		}
		// methods
		if c.comptime.comptime_for_method_var.len > 0 {
			arr << c.comptime.comptime_for_method_var + '.name=' +
				c.comptime.comptime_for_method.name
		}
		// args
		if c.comptime.comptime_for_method_param_var.len > 0 {
			arg_var := c.table.type_to_str(c.type_resolver.get_ct_type_or_default('${c.comptime.comptime_for_method_param_var}.typ',
				ast.void_type))
			arr << c.comptime.comptime_for_method_param_var + '.typ=' + arg_var
		}
	}
	return arr.join(',')
}

fn (mut c Checker) if_expr(mut node ast.IfExpr) ast.Type {
	if_kind := if node.is_comptime { '\$if' } else { 'if' }
	mut node_is_expr := false
	if node.branches.len > 0 && node.has_else {
		stmts := node.branches[0].stmts
		if stmts.len > 0 && stmts.last() is ast.ExprStmt && stmts.last().typ != ast.void_type {
			node_is_expr = true
		} else if node.is_expr {
			node_is_expr = true
		}
	}
	if c.expected_type == ast.void_type && node_is_expr {
		c.expected_type = c.expected_or_type
	}
	expr_required := c.expected_type != ast.void_type
		|| (node.is_comptime && node.is_expr && node.has_else && c.fn_level > 0)
	former_expected_type := c.expected_type
	if node_is_expr {
		c.expected_expr_type = c.expected_type
		defer(fn) {
			c.expected_expr_type = ast.void_type
		}
	}
	node.typ = ast.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	mut comptime_if_result := false
	mut comptime_if_multi_pass_branch := false
	mut comptime_if_found_branch := false
	mut comptime_if_has_multi_pass_branch := false

	last_in_comptime_if := c.comptime.inside_comptime_if
	defer {
		c.comptime.inside_comptime_if = last_in_comptime_if
	}

	comptime_branch_context_str := if node.is_comptime { c.gen_branch_context_string() } else { '' }

	for i, mut branch in node.branches {
		if node.is_comptime {
			c.push_new_comptime_info()
		}
		orig_branch_cond := branch.cond
		mut comptime_remove_curr_branch_stmts := false
		if branch.cond is ast.ParExpr && !c.pref.translated && !c.file.is_translated {
			c.warn('unnecessary `()` in `${if_kind}` condition, use `${if_kind} expr {` instead of `${if_kind} (expr) {`.',
				branch.pos)
		}
		if !node.has_else || i < node.branches.len - 1 {
			// if branch
			if node.is_comptime {
				// `idx_str` is composed of two parts:
				// The first part represents the current context of the branch statement, `comptime_branch_context_str`, formatted like `T=int,X=string,method.name=json`
				// The second part is the branch's id.
				// This format must match what is in `cgen`.
				if branch.id == 0 {
					// this is a new branch, alloc a new id for it
					c.cur_ct_id++
					branch.id = c.cur_ct_id
				}
				mut idx_str := comptime_branch_context_str + '|id=${branch.id}|'
				if c.comptime.inside_comptime_for && c.comptime.comptime_for_field_var != '' {
					idx_str += '|field_type=${c.comptime.comptime_for_field_type}|'
				}
				c.comptime.inside_comptime_if = true
				mut sb := strings.new_builder(256)
				comptime_if_result, comptime_if_multi_pass_branch =
					c.comptime_if_cond(mut branch.cond, mut sb)
				if comptime_if_multi_pass_branch {
					comptime_if_has_multi_pass_branch = true
				}

				if comptime_if_found_branch {
					comptime_if_result = false
				}

				if !comptime_if_has_multi_pass_branch
					&& (comptime_if_found_branch || !comptime_if_result) {
					// when all prev branchs are single pass branchs,
					// 1. already has a true branch or
					// 2. `comptime_if_result is` false
					// remove current branchs' stmts
					comptime_remove_curr_branch_stmts = true
				}
				if old_val := c.table.comptime_is_true[idx_str] {
					if old_val.val != comptime_if_result {
						c.error('checker error : branch eval wrong', branch.cond.pos())
					}
				}

				// set `comptime_is_true` which can be used by `cgen`
				c.table.comptime_is_true[idx_str] = ast.ComptTimeCondResult{
					val:   comptime_if_result
					c_str: sb.str()
				}
			} else {
				// check condition type is boolean
				c.expected_type = ast.bool_type
				cond_typ := c.table.unaliased_type(c.unwrap_generic(c.expr(mut branch.cond)))
				if (cond_typ.idx() != ast.bool_type_idx || cond_typ.has_flag(.option)
					|| cond_typ.has_flag(.result)) && !c.pref.translated && !c.file.is_translated {
					//&& cond_typ.idx() != ast.void_type_idx { TODO bring back after the void split
					c.error('non-bool type `${c.table.type_to_str(cond_typ)}` used as if condition',
						branch.cond.pos())
				}
				if !c.pref.translated && !c.file.is_translated && !c.inside_unsafe {
					mut check_expr := branch.cond
					t_expr := c.checker_transformer.expr(mut check_expr)
					if t_expr is ast.BoolLiteral {
						if t_expr.val {
							c.note('condition is always true', branch.cond.pos())
						} else {
							c.note('condition is always false', branch.cond.pos())
						}
					}
				}
				if !c.pref.translated && !c.file.is_translated && !c.inside_unsafe
					&& c.is_always_true_self_comparison(orig_branch_cond) {
					c.warn('self-comparison in `if` condition is always true; following branches may be unreachable',
						branch.cond.pos())
				}
			}
		} else {
			// else branch
			if node.is_comptime {
				c.comptime.inside_comptime_if = true
				comptime_if_result = !comptime_if_found_branch
				// if all other branchs has at least one multi pass branch, we should keep this else branch
				comptime_if_multi_pass_branch = comptime_if_has_multi_pass_branch
				if !comptime_if_has_multi_pass_branch && comptime_if_found_branch {
					// when all prev branchs are single pass branchs, already has a true branch
					// remove current branchs' stmts
					comptime_remove_curr_branch_stmts = true
				}
				// hack: as a `else` has no `cond`, so we use `branch.pos` here
				if branch.id == 0 {
					// this is a new branch, alloc a new id for it
					c.cur_ct_id++
					branch.id = c.cur_ct_id
				}
				mut idx_str := comptime_branch_context_str + '|id=${branch.id}|'
				if c.comptime.inside_comptime_for && c.comptime.comptime_for_field_var != '' {
					idx_str += '|field_type=${c.comptime.comptime_for_field_type}|'
				}
				c.table.comptime_is_true[idx_str] = ast.ComptTimeCondResult{
					val:   comptime_if_result
					c_str: ''
				}
			}
		}
		if mut branch.cond is ast.IfGuardExpr {
			if branch.cond.expr_type.clear_option_and_result() == ast.void_type
				&& !(branch.cond.vars.len == 1 && branch.cond.vars[0].name == '_') {
				c.error('if guard expects non-propagate option or result', branch.pos)
				continue
			}
			sym := c.table.sym(branch.cond.expr_type)
			if sym.kind == .multi_return {
				mr_info := sym.info as ast.MultiReturn
				if branch.cond.vars.len != mr_info.types.len {
					c.error('if guard expects ${mr_info.types.len} variables, but got ${branch.cond.vars.len}',
						branch.pos)
					continue
				} else {
					for vi, var in branch.cond.vars {
						branch.scope.update_var_type(var.name, mr_info.types[vi])
					}
				}
			} else {
				if branch.cond.vars.len != 1 {
					c.error('if guard expects a single variable, but got ${branch.cond.vars.len}',
						branch.pos)
					continue
				}
				for _, var in branch.cond.vars {
					if var.name == '_' {
						continue
					}
					unwrapped_guard_typ := branch.cond.expr_type.clear_option_and_result()
					if w := branch.scope.find_var(var.name) {
						$if trace_vweb_guard ? {
							if var.name == 'params' {
								expr_type_str := if branch.cond.expr_type == 0 {
									'<none>'
								} else {
									c.table.type_to_str(branch.cond.expr_type)
								}
								clear_type := branch.cond.expr_type.clear_option_and_result()
								clear_type_str := if clear_type == 0 {
									'<none>'
								} else {
									c.table.type_to_str(clear_type)
								}
								old_type_str := if w.typ == 0 {
									'<none>'
								} else {
									c.table.type_to_str(w.typ)
								}
								eprintln('if_guard scope name=${var.name} expr_type=${expr_type_str} clear=${clear_type_str} old=${old_type_str} file=${c.file.path}')
							}
						}
						if var.name !in branch.scope.objects {
							branch.scope.objects[var.name] = w
						}
						branch.scope.update_var_type(var.name, unwrapped_guard_typ)
						resolved_guard_typ := c.unwrap_generic(unwrapped_guard_typ)
						if !resolved_guard_typ.is_ptr() && c.table.sym(resolved_guard_typ).is_heap() {
							if mut guard_var := branch.scope.find_var(var.name) {
								guard_var.is_auto_heap = true
							}
						}
					}
				}
			}
		}
		if node.is_comptime { // Skip checking if needed
			cur_skip_flags := c.skip_flags
			// if current cond result is false, or we already found a branch, we should skip current branch
			c.skip_flags = !comptime_if_result || comptime_if_found_branch
			if c.fn_level == 0 && c.pref.output_cross_c {
				// do not skip any of the branches for top level `$if OS {`
				// statements, in `-cross` mode
				comptime_remove_curr_branch_stmts = false
				c.skip_flags = false
				// hack: because `else` branch has no `cond`, so create an Ident, set the `pos`, for `hash_stmt()` work.
				if branch.cond is ast.NodeError {
					c.ct_cond_stack << ast.Ident{
						name: '__else_branch__'
						pos:  branch.pos
					}
				} else {
					c.ct_cond_stack << branch.cond
				}
			}
			if !c.skip_flags {
				if node_is_expr {
					c.stmts_ending_with_expression(mut branch.stmts, c.expected_or_type)
				} else {
					c.stmts(mut branch.stmts)
					c.check_non_expr_branch_last_stmt(branch.stmts)
				}
			} else if c.pref.output_cross_c {
				mut is_freestanding_block := false
				if mut branch.cond is ast.Ident {
					if branch.cond.name == 'freestanding' {
						is_freestanding_block = true
					}
				}
				if is_freestanding_block {
					branch.stmts = []
					node.branches[i].stmts = []
				}
				if node_is_expr {
					c.stmts_ending_with_expression(mut branch.stmts, c.expected_or_type)
				} else {
					c.stmts(mut branch.stmts)
					c.check_non_expr_branch_last_stmt(branch.stmts)
				}
			}
			c.skip_flags = cur_skip_flags
			if c.fn_level == 0 && c.pref.output_cross_c && c.ct_cond_stack.len > 0 {
				c.ct_cond_stack.delete_last()
			}
			if comptime_if_result {
				comptime_if_found_branch = true
			}
		} else {
			// smartcast sumtypes and interfaces when using `is`
			c.smartcast_if_conds(mut branch.cond, mut branch.scope, node)
			if node_is_expr {
				c.stmts_ending_with_expression(mut branch.stmts, c.expected_or_type)
			} else {
				c.stmts(mut branch.stmts)
				c.check_non_expr_branch_last_stmt(branch.stmts)
			}
			c.smartcast_mut_pos = token.Pos{}
			c.smartcast_cond_pos = token.Pos{}
		}
		if expr_required {
			if branch.stmts.len > 0 {
				mut stmt := branch.stmts.last()
				if mut stmt is ast.ExprStmt {
					if mut stmt.expr is ast.ConcatExpr {
						for mut val in stmt.expr.vals {
							c.check_expr_option_or_result_call(val, c.expr(mut val))
						}
					}
					c.expected_type = former_expected_type
					if c.table.type_kind(c.expected_type) == .sum_type
						&& c.table.is_sumtype_or_in_variant(c.expected_type, node.typ) {
						node.is_expr = true
						node.typ = c.expected_type
					}
					if c.expected_type.has_option_or_result() {
						if node.typ == ast.void_type {
							node.is_expr = true
							node.typ = c.expected_type
						}
					}
					if c.expected_type.has_flag(.generic) {
						if node.typ == ast.void_type {
							node.is_expr = true
							node.typ = c.unwrap_generic(c.expected_type)
						}
						unsafe {
							goto end_if
						}
					}
					if c.expected_expr_type != ast.void_type {
						c.expected_type = c.expected_expr_type
					}
					stmt.typ = c.expr(mut stmt.expr)
					if c.table.type_kind(c.expected_type) == .multi_return
						&& c.table.type_kind(stmt.typ) == .multi_return {
						if node.typ == ast.void_type {
							node.is_expr = true
							node.typ = c.expected_type
						}
					}
					if stmt.typ == ast.void_type && !is_noreturn_callexpr(stmt.expr)
						&& !c.skip_flags {
						// cannot return void type and use it as expr in any circumstances
						// (e.g. argument expression, variable declaration / assignment)
						c.error('the final expression in `if` or `match`, must have a value of a non-void type',
							stmt.pos)
						unsafe {
							goto end_if
						}
					}
					if !c.check_types(stmt.typ, node.typ)
						&& !c.check_types(c.unwrap_generic(stmt.typ), c.unwrap_generic(node.typ)) {
						if node.typ == ast.void_type {
							// first branch of if expression
							node.is_expr = true
							if stmt.expr.is_auto_deref_var() {
								node.typ = stmt.typ.deref()
							} else {
								node.typ = stmt.typ
							}
							c.expected_expr_type = node.typ
							unsafe {
								goto end_if
							}
						} else if node.typ in [ast.float_literal_type, ast.int_literal_type] {
							if node.typ == ast.int_literal_type {
								if stmt.typ.is_int() || stmt.typ.is_float() {
									node.typ = stmt.typ
									unsafe {
										goto end_if
									}
								}
							} else { // node.typ == float_literal
								if stmt.typ.is_float() {
									node.typ = stmt.typ
									unsafe {
										goto end_if
									}
								}
							}
						}
						if stmt.typ in [ast.float_literal_type, ast.int_literal_type] {
							if stmt.typ == ast.int_literal_type {
								if node.typ.is_int() || node.typ.is_float() {
									unsafe {
										goto end_if
									}
								}
							} else { // expr_type == float_literal
								if node.typ.is_float() {
									unsafe {
										goto end_if
									}
								}
							}
						}
						if c.is_interface_smartcast_sumtype_variant(stmt.expr,
							former_expected_type, stmt.typ)
						{
							node.is_expr = true
							node.typ = former_expected_type.clear_option_and_result()
							unsafe {
								goto end_if
							}
						}
						if node.is_expr && c.table.sym(former_expected_type).kind == .sum_type {
							node.typ = former_expected_type
							unsafe {
								goto end_if
							}
						}
						if is_noreturn_callexpr(stmt.expr) {
							unsafe {
								goto end_if
							}
						}
						if (node.typ.has_option_or_result())
							&& c.table.sym(stmt.typ).kind == .struct
							&& c.type_implements(stmt.typ, ast.error_type, node.pos) {
							stmt.expr = ast.CastExpr{
								expr:      stmt.expr
								typname:   'IError'
								typ:       ast.error_type
								expr_type: stmt.typ
								pos:       node.pos
							}
							stmt.typ = ast.error_type
							unsafe {
								goto end_if
							}
						}
						if (node.typ == ast.none_type && stmt.typ != ast.none_type)
							|| (stmt.typ == ast.none_type && node.typ != ast.none_type) {
							node.typ = if stmt.typ != ast.none_type {
								stmt.typ.set_flag(.option)
							} else {
								node.typ.set_flag(.option)
							}
							unsafe {
								goto end_if
							}
						}
						c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(stmt.typ)}`',
							node.pos)
					} else {
						if !node.typ.has_option_or_result() && !node.typ.has_flag(.shared_f)
							&& stmt.typ != ast.voidptr_type
							&& stmt.typ.nr_muls() != node.typ.nr_muls()
							&& !c.is_interface_smartcast_sumtype_variant(stmt.expr, node.typ, stmt.typ) {
							c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(stmt.typ)}`',
								node.pos)
						}
						if node.is_expr == false && c.type_resolver.is_generic_param_var(stmt.expr) {
							// generic variable no yet type bounded
							node.is_expr = true
						}
						if c.inside_assign && node.is_expr && !node.typ.has_flag(.shared_f)
							&& stmt.typ != ast.voidptr_type
							&& !c.is_interface_smartcast_sumtype_variant(stmt.expr, node.typ, stmt.typ) {
							if stmt.typ.is_ptr() != node.typ.is_ptr() {
								c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(stmt.typ)}`',
									node.pos)
							} else if stmt.typ != ast.none_type {
								if !node.typ.has_flag(.option) && stmt.typ.has_flag(.option) {
									c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(stmt.typ)}`',
										node.pos)
								} else if !node.typ.has_flag(.result) && stmt.typ.has_flag(.result) {
									c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(stmt.typ)}`',
										node.pos)
								}
							}
						}
					}
				} else if !node.is_comptime && stmt !in [ast.Return, ast.BranchStmt] {
					pos := if node_is_expr { stmt.pos } else { branch.pos }
					c.error('`${if_kind}` expression requires an expression as the last statement of every branch',
						pos)
				}
			} else if !node.is_comptime {
				c.error('`${if_kind}` expression requires an expression as the last statement of every branch',
					branch.pos)
			}
		}
		// Also check for returns inside a comp.if's statements, even if its contents aren't parsed
		if has_return := c.has_return(branch.stmts) {
			if has_return {
				nbranches_with_return++
			} else {
				nbranches_without_return++
			}
		}
		end_if:
		if comptime_remove_curr_branch_stmts && !c.pref.output_cross_c {
			// remove the branch statements since they may contain OS-specific code.
			branch.stmts = []
		}
		if node.is_comptime {
			c.pop_comptime_info()
		}
	}
	if nbranches_with_return > 0 {
		if nbranches_with_return == node.branches.len {
			// if/else... where all branches returned
			c.returns = true
		}
		if !node.has_else {
			// `if cond { return ... }` means that when cond is false, execution continues
			c.returns = false
		}
		if nbranches_without_return > 0 {
			// some of the branches did not return
			c.returns = false
		}
	}
	if !node.is_comptime && node.branches.len > 0 && has_top_return(node.branches[0].stmts)
		&& node.branches[0].scope != unsafe { nil }
		&& node.branches[0].scope.parent != unsafe { nil } {
		mut continuation_scope := node.branches[0].scope.parent
		c.smartcast_none_guard_fallthrough(node.branches[0].cond, mut continuation_scope)
	}
	if node.typ == ast.none_type {
		c.error('invalid if expression, must supply at least one value other than `none`', node.pos)
	}
	// if only untyped literals were given default to int/f64
	node.typ = ast.mktyp(node.typ)
	if expr_required && !node.has_else {
		d := if node.is_comptime { '$' } else { '' }
		c.error('`${if_kind}` expression needs `${d}else` clause', node.pos)
	}
	return node.typ
}

fn (mut c Checker) smartcast_if_conds(mut node ast.Expr, mut scope ast.Scope, control_expr ast.Expr) {
	if mut node is ast.InfixExpr {
		if node.op == .and {
			c.smartcast_if_conds(mut node.left, mut scope, control_expr)
			c.smartcast_if_conds(mut node.right, mut scope, control_expr)
		} else if node.left in [ast.Ident, ast.SelectorExpr] && node.op == .ne
			&& node.right is ast.None {
			if (node.left is ast.Ident && node.left.is_mut)
				|| (node.left is ast.SelectorExpr && node.left.is_mut) {
				c.fail_if_immutable(mut node.left)
			}
			if c.comptime.inside_comptime_for && c.comptime.comptime_for_field_var != ''
				&& node.left is ast.Ident {
				if mut node.left is ast.Ident {
					if mut node.left.obj is ast.Var {
						if node.left.obj.ct_type_var == .field_var {
							scope.register(ast.Var{
								name:              node.left.name
								typ:               node.left_type
								pos:               node.left.pos
								is_used:           true
								is_mut:            node.left.is_mut
								is_inherited:      node.left.obj.is_inherited
								is_unwrapped:      true
								orig_type:         node.left_type
								ct_type_var:       .field_var
								ct_type_unwrapped: true
							})
						}
					}
				}
			} else {
				if node.left is ast.Ident && c.comptime.get_ct_type_var(node.left) == .smartcast {
					node.left_type = c.type_resolver.get_type(node.left)
					c.smartcast(mut node.left, node.left_type, node.left_type.clear_flag(.option), mut
						scope, true, true, false)
				} else {
					c.smartcast(mut node.left, node.left_type, node.left_type.clear_flag(.option), mut
						scope, false, true, false)
				}
			}
		} else if node.op == .key_is {
			if node.left is ast.Ident && node.left.ct_expr {
				node.left_type = c.type_resolver.get_type(node.left)
			} else {
				node.left_type = c.expr(mut node.left)
			}
			mut is_comptime := false
			right_expr := node.right
			right_type := match right_expr {
				ast.TypeNode {
					right_expr.typ
				}
				ast.None {
					ast.none_type_idx
				}
				ast.Ident {
					if right_expr.name == c.comptime.comptime_for_variant_var {
						is_comptime = true
						c.type_resolver.get_ct_type_or_default('${c.comptime.comptime_for_variant_var}.typ',
							ast.no_type)
					} else {
						c.error('invalid type `${right_expr}`', right_expr.pos)
						ast.no_type
					}
				}
				else {
					c.error('invalid type `${right_expr}`', right_expr.pos())
					ast.no_type
				}
			}
			if right_type != ast.no_type {
				$if trace_ci_fixes ? {
					source_path := if node.pos.file_idx >= 0
						&& node.pos.file_idx < c.table.filelist.len {
						c.table.filelist[node.pos.file_idx]
					} else {
						c.file.path
					}
					if source_path.contains('decode_sumtype.v')
						&& node.pos.line_nr + 1 in [12, 111, 138, 215] {
						right_kind := match right_expr {
							ast.Ident { 'ident:${right_expr.name}' }
							ast.TypeNode { 'typenode:${c.table.type_to_str(right_expr.typ)}' }
							ast.None { 'none' }
							else { typeof(right_expr).name }
						}
						eprintln('if is left=${node.left} left_type=${c.table.type_to_str(node.left_type)} right_kind=${right_kind} right_type=${c.table.type_to_str(right_type)} variant_var=${c.comptime.comptime_for_variant_var} file=${c.file.path} source=${source_path} line=${
							node.pos.line_nr + 1}')
					}
				}
				right_sym := c.table.sym(right_type)
				mut expr_type := c.unwrap_generic(node.left_type)
				left_sym := c.table.sym(expr_type)
				left_final_sym := c.table.final_sym(expr_type)
				if left_sym.kind == .aggregate {
					expr_type = (left_sym.info as ast.Aggregate).sum_type
				}
				if left_sym.kind == .interface {
					if right_sym.kind != .interface {
						c.type_implements(right_type, expr_type, node.pos)
					}
				} else if !c.check_types(right_type, expr_type) && left_final_sym.kind != .sum_type {
					expect_str := c.table.type_to_str(right_type)
					expr_str := c.table.type_to_str(expr_type)
					c.error('cannot use type `${expect_str}` as type `${expr_str}`', node.pos)
				}
				if node.left in [ast.Ident, ast.SelectorExpr]
					&& node.right in [ast.ComptimeType, ast.TypeNode, ast.Ident] {
					is_variable := if mut node.left is ast.Ident {
						node.left.kind == .variable
					} else {
						true
					}
					if is_variable {
						if (node.left is ast.Ident && node.left.is_mut)
							|| (node.left is ast.SelectorExpr && node.left.is_mut) {
							c.fail_if_immutable(mut node.left)
						}
						// TODO: Add check for sum types in a way that it doesn't break a lot of compiler code
						if mut node.left is ast.Ident
							&& (left_sym.kind == .interface && right_sym.kind != .interface) {
							v := scope.find_var(node.left.name) or { &ast.Var{} }
							if v.is_mut && !node.left.is_mut {
								c.error('smart casting a mutable interface value requires `if mut ${node.left.name} is ...`',
									node.left.pos)
							}
						}
						is_option_unwrap := node.left_type.has_flag(.option)
							&& !right_type.has_flag(.option)
						skip_smartcast := c.comptime.inside_comptime_for
							&& c.comptime.comptime_for_field_var != '' && node.left is ast.Ident
							&& (node.left as ast.Ident).name == c.comptime.comptime_for_field_var
						if !skip_smartcast
							&& (left_final_sym.kind in [.interface, .sum_type] || is_option_unwrap) {
							c.smartcast(mut node.left, node.left_type, right_type, mut scope,
								is_comptime, is_option_unwrap, false)
						}
					}
				}
			}
		}
	} else if mut node is ast.Likely {
		c.smartcast_if_conds(mut node.expr, mut scope, control_expr)
	} else if control_expr is ast.IfExpr && mut node is ast.NodeError { // IfExpr else branch
		if control_expr.branches.len != 2 {
			return
		}
		first_cond := control_expr.branches[0].cond
		// `if x !is Type { ... } else { /* smartcast x to Type here */ }`
		if first_cond is ast.InfixExpr && first_cond.op == .not_is
			&& first_cond.left in [ast.Ident, ast.SelectorExpr] {
			right_type := first_cond.right_type
			left_type := first_cond.left_type
			left_sym := c.table.final_sym(left_type)
			is_option_unwrap := left_type.has_flag(.option) && !right_type.has_flag(.option)
			if left_sym.kind in [.interface, .sum_type] || is_option_unwrap {
				mut left_expr := first_cond.left
				c.smartcast(mut left_expr, left_type, right_type, mut scope, false,
					is_option_unwrap, false)
			}
		}
		c.smartcast_none_guard_unwrap(control_expr.branches[0].cond, mut scope)
	}
}

fn (mut c Checker) smartcast_none_guard_unwrap(cond ast.Expr, mut scope ast.Scope) {
	mut cond_expr := cond
	cond_expr = cond_expr.remove_par()
	// Handles unwrapping on `if var == none { return }` fallthroughs and on
	// `if var == none { ... } else { /* unwrapped var */ }` branches.
	if mut cond_expr is ast.InfixExpr {
		if cond_expr.left !in [ast.Ident, ast.SelectorExpr] || cond_expr.op != .eq
			|| cond_expr.right !is ast.None {
			return
		}
		to_type := cond_expr.left_type.clear_flag(.option)
		if c.comptime.get_ct_type_var(cond_expr.left) == .smartcast {
			cond_expr.left_type = c.type_resolver.get_type(cond_expr.left)
			c.smartcast(mut cond_expr.left, cond_expr.left_type, to_type, mut scope, true, true,
				false)
		} else {
			c.smartcast(mut cond_expr.left, cond_expr.left_type, to_type, mut scope, false, true,
				false)
		}
	}
}

fn (mut c Checker) smartcast_none_guard_fallthrough(cond ast.Expr, mut scope ast.Scope) {
	mut cond_expr := cond
	cond_expr = cond_expr.remove_par()
	if mut cond_expr is ast.InfixExpr {
		if mut cond_expr.left is ast.Ident && cond_expr.op == .eq && cond_expr.right is ast.None {
			to_type := cond_expr.left_type.clear_flag(.option)
			if mut cond_expr.left.obj is ast.Var && cond_expr.left.name in scope.objects {
				if scope_var := scope.find_var(cond_expr.left.name) {
					if scope_var.pos.pos == cond_expr.left.obj.pos.pos {
						cond_expr.left.obj.smartcasts = [to_type]
						cond_expr.left.obj.is_unwrapped = true
						scope.update_smartcasts(cond_expr.left.name, to_type, true)
						return
					}
				}
			}
			if c.comptime.get_ct_type_var(cond_expr.left) == .smartcast {
				cond_expr.left_type = c.type_resolver.get_type(cond_expr.left)
				c.smartcast(mut cond_expr.left, cond_expr.left_type, to_type, mut scope, true,
					true, false)
			} else {
				c.smartcast(mut cond_expr.left, cond_expr.left_type, to_type, mut scope, false,
					true, false)
			}
		}
	}
}

fn (mut c Checker) check_non_expr_branch_last_stmt(stmts []ast.Stmt) {
	if stmts.len == 0 {
		return
	}
	last_stmt := stmts.last()
	if last_stmt is ast.ExprStmt && (last_stmt.expr is ast.InfixExpr
		&& last_stmt.expr.op !in [.left_shift, .right_shift, .unsigned_right_shift, .arrow]) {
		c.error('expression evaluated but not used', last_stmt.pos)
	}
}
