// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.token

fn (mut c Checker) check_compatible_types(left_type ast.Type, right ast.TypeNode) ComptimeBranchSkipState {
	right_type := c.unwrap_generic(right.typ)
	sym := c.table.sym(right_type)

	if right_type.has_flag(.option) != left_type.has_flag(.option) {
		return .skip
	}

	if sym.kind == .interface {
		checked_type := c.unwrap_generic(left_type)
		return if c.table.does_type_implement_interface(checked_type, right_type) {
			.eval
		} else {
			.skip
		}
	} else {
		return if left_type == right_type { .eval } else { .skip }
	}
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
	former_expected_type := c.expected_type
	if node_is_expr {
		c.expected_expr_type = c.expected_type
		defer {
			c.expected_expr_type = ast.void_type
		}
	}
	node.typ = ast.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	mut skip_state := ComptimeBranchSkipState.unknown
	mut found_branch := false // Whether a matching branch was found- skip the rest
	mut is_comptime_type_is_expr := false // if `$if T is string`
	last_in_comptime_if := c.comptime.inside_comptime_if
	defer {
		c.comptime.inside_comptime_if = last_in_comptime_if
	}
	for i in 0 .. node.branches.len {
		mut branch := node.branches[i]
		if branch.cond is ast.ParExpr && !c.pref.translated && !c.file.is_translated {
			c.warn('unnecessary `()` in `${if_kind}` condition, use `${if_kind} expr {` instead of `${if_kind} (expr) {`.',
				branch.pos)
		}
		if !node.has_else || i < node.branches.len - 1 {
			if node.is_comptime {
				skip_state = c.comptime_if_cond(mut branch.cond, branch.pos)
				node.branches[i].pkg_exist = if skip_state == .eval { true } else { false }
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
				for _, var in branch.cond.vars {
					if var.name == '_' {
						continue
					}
					if w := branch.scope.find_var(var.name) {
						if var.name !in branch.scope.objects {
							branch.scope.objects[var.name] = w
						}
						branch.scope.update_var_type(var.name, branch.cond.expr_type.clear_option_and_result())
					}
				}
			}
		}
		if node.is_comptime { // Skip checking if needed
			// smartcast field type on comptime if
			mut comptime_field_name := ''
			if branch.cond is ast.SelectorExpr && skip_state != .unknown {
				is_comptime_type_is_expr = true
			} else if mut branch.cond is ast.PrefixExpr {
				if branch.cond.right is ast.SelectorExpr && skip_state != .unknown {
					is_comptime_type_is_expr = true
				}
			} else if mut branch.cond is ast.InfixExpr {
				if branch.cond.op in [.not_is, .key_is] {
					left := branch.cond.left
					right := branch.cond.right
					if right !in [ast.TypeNode, ast.ComptimeType] {
						c.error('invalid `\$if` condition: expected a type', branch.cond.right.pos())
						return 0
					}
					if right is ast.ComptimeType {
						mut checked_type := ast.void_type
						if left is ast.TypeNode {
							is_comptime_type_is_expr = true
							checked_type = c.unwrap_generic(left.typ)
							skip_state = if c.type_resolver.is_comptime_type(checked_type,
								right as ast.ComptimeType)
							{
								.eval
							} else {
								.skip
							}
						} else if left is ast.Ident && left.info is ast.IdentVar {
							is_comptime_type_is_expr = true
							if var := left.scope.find_var(left.name) {
								checked_type = c.unwrap_generic(var.typ)
								if var.smartcasts.len > 0 {
									checked_type = c.unwrap_generic(var.smartcasts.last())
								}
							}
							skip_state = if c.type_resolver.is_comptime_type(checked_type,
								right as ast.ComptimeType)
							{
								.eval
							} else {
								.skip
							}
						} else if left is ast.SelectorExpr {
							comptime_field_name = left.expr.str()
							is_comptime_type_is_expr = true
						}
					} else {
						got_type := c.unwrap_generic((right as ast.TypeNode).typ)
						sym := c.table.sym(got_type)
						if sym.kind == .placeholder || got_type.has_flag(.generic) {
							c.error('unknown type `${sym.name}`', branch.cond.right.pos())
						}
						if left is ast.SelectorExpr {
							comptime_field_name = left.expr.str()
							is_comptime_type_is_expr = true
							if comptime_field_name == c.comptime.comptime_for_field_var {
								left_type := c.unwrap_generic(c.comptime.comptime_for_field_type)
								if left.field_name == 'typ' {
									skip_state = c.check_compatible_types(left_type, right as ast.TypeNode)
								} else if left.field_name == 'unaliased_typ' {
									skip_state = c.check_compatible_types(c.table.unaliased_type(left_type),
										right as ast.TypeNode)
								}
							} else if c.comptime.check_comptime_is_field_selector_bool(left) {
								skip_state = if c.type_resolver.get_comptime_selector_bool_field(left.field_name) {
									.eval
								} else {
									.skip
								}
							} else if comptime_field_name == c.comptime.comptime_for_method_var
								&& left.field_name == 'return_type' {
								skip_state = c.check_compatible_types(c.unwrap_generic(c.comptime.comptime_for_method_ret_type),
									right as ast.TypeNode)
							} else if comptime_field_name in [
								c.comptime.comptime_for_variant_var,
								c.comptime.comptime_for_enum_var,
							] {
								if left.field_name == 'typ' {
									skip_state = c.check_compatible_types(c.type_resolver.get_ct_type_or_default('${comptime_field_name}.typ',
										ast.void_type), right as ast.TypeNode)
								}
							}
						} else if left is ast.TypeNode {
							is_comptime_type_is_expr = true
							left_type := c.unwrap_generic(left.typ)
							skip_state = c.check_compatible_types(left_type, right as ast.TypeNode)
						} else if left is ast.Ident {
							mut checked_type := ast.void_type
							is_comptime_type_is_expr = true
							if var := left.scope.find_var(left.name) {
								checked_type = c.unwrap_generic(var.typ)
								if var.smartcasts.len > 0 {
									checked_type = c.unwrap_generic(var.smartcasts.last())
								}
							}
							if sym.info is ast.FnType
								&& c.comptime.comptime_for_method_var == left.name {
								skip_state = if c.table.fn_signature(sym.info.func,
									skip_receiver: true
									type_only:     true
								) == c.table.fn_signature(c.comptime.comptime_for_method,
									skip_receiver: true
									type_only:     true
								) {
									.eval
								} else {
									.skip
								}
							} else {
								skip_state = c.check_compatible_types(checked_type, right as ast.TypeNode)
							}
						}
					}
					if branch.cond.op == .not_is && skip_state != .unknown {
						skip_state = if skip_state == .eval { .skip } else { .eval }
					}
				} else if branch.cond.op in [.eq, .ne, .gt, .lt, .ge, .le] {
					left := branch.cond.left
					right := branch.cond.right
					if left is ast.SelectorExpr && right is ast.IntegerLiteral {
						comptime_field_name = left.expr.str()
						is_comptime_type_is_expr = true
						if comptime_field_name == c.comptime.comptime_for_field_var {
							if left.field_name == 'indirections' {
								skip_state = match branch.cond.op {
									.gt {
										if c.comptime.comptime_for_field_type.nr_muls() > right.val.i64() {
											ComptimeBranchSkipState.eval
										} else {
											ComptimeBranchSkipState.skip
										}
									}
									.lt {
										if c.comptime.comptime_for_field_type.nr_muls() < right.val.i64() {
											ComptimeBranchSkipState.eval
										} else {
											ComptimeBranchSkipState.skip
										}
									}
									.ge {
										if c.comptime.comptime_for_field_type.nr_muls() >= right.val.i64() {
											ComptimeBranchSkipState.eval
										} else {
											ComptimeBranchSkipState.skip
										}
									}
									.le {
										if c.comptime.comptime_for_field_type.nr_muls() <= right.val.i64() {
											ComptimeBranchSkipState.eval
										} else {
											ComptimeBranchSkipState.skip
										}
									}
									.ne {
										if c.comptime.comptime_for_field_type.nr_muls() != right.val.i64() {
											ComptimeBranchSkipState.eval
										} else {
											ComptimeBranchSkipState.skip
										}
									}
									.eq {
										if c.comptime.comptime_for_field_type.nr_muls() == right.val.i64() {
											ComptimeBranchSkipState.eval
										} else {
											ComptimeBranchSkipState.skip
										}
									}
									else {
										ComptimeBranchSkipState.skip
									}
								}
							}
						} else if comptime_field_name == c.comptime.comptime_for_method_var {
							if left.field_name == 'return_type' {
								skip_state = if c.unwrap_generic(c.comptime.comptime_for_method_ret_type).idx() == right.val.i64() {
									ComptimeBranchSkipState.eval
								} else {
									ComptimeBranchSkipState.skip
								}
							}
						} else if left.expr is ast.TypeOf {
							skip_state = if left.expr.typ.nr_muls() == right.val.i64() {
								ComptimeBranchSkipState.eval
							} else {
								ComptimeBranchSkipState.skip
							}
						}
					} else if branch.cond.op in [.eq, .ne] && left is ast.SelectorExpr
						&& right is ast.StringLiteral {
						match left.expr.str() {
							c.comptime.comptime_for_field_var {
								if left.field_name == 'name' {
									is_comptime_type_is_expr = true
									match branch.cond.op {
										.eq {
											skip_state = if c.comptime.comptime_for_field_value.name == right.val.str() {
												ComptimeBranchSkipState.eval
											} else {
												ComptimeBranchSkipState.skip
											}
										}
										.ne {
											skip_state = if c.comptime.comptime_for_field_value.name == right.val.str() {
												ComptimeBranchSkipState.skip
											} else {
												ComptimeBranchSkipState.eval
											}
										}
										else {}
									}
								}
							}
							c.comptime.comptime_for_method_var {
								if left.field_name == 'name' {
									is_comptime_type_is_expr = true
									match branch.cond.op {
										.eq {
											skip_state = if c.comptime.comptime_for_method.name == right.val.str() {
												ComptimeBranchSkipState.eval
											} else {
												ComptimeBranchSkipState.skip
											}
										}
										.ne {
											skip_state = if c.comptime.comptime_for_method.name == right.val.str() {
												ComptimeBranchSkipState.skip
											} else {
												ComptimeBranchSkipState.eval
											}
										}
										else {}
									}
								}
							}
							else {}
						}
					} else if left is ast.SizeOf && right is ast.IntegerLiteral {
						// TODO: support struct.fieldname
						typ := c.unwrap_generic(left.typ)
						if typ == 0 {
							c.error('invalid `\$if` condition: expected a type', branch.cond.left.pos())
						} else {
							s, _ := c.table.type_size(c.unwrap_generic(typ))
							skip_state = match branch.cond.op {
								.gt {
									if s > right.val.i64() {
										ComptimeBranchSkipState.eval
									} else {
										ComptimeBranchSkipState.skip
									}
								}
								.lt {
									if s < right.val.i64() {
										ComptimeBranchSkipState.eval
									} else {
										ComptimeBranchSkipState.skip
									}
								}
								.ge {
									if s >= right.val.i64() {
										ComptimeBranchSkipState.eval
									} else {
										ComptimeBranchSkipState.skip
									}
								}
								.le {
									if s <= right.val.i64() {
										ComptimeBranchSkipState.eval
									} else {
										ComptimeBranchSkipState.skip
									}
								}
								.ne {
									if s != right.val.i64() {
										ComptimeBranchSkipState.eval
									} else {
										ComptimeBranchSkipState.skip
									}
								}
								.eq {
									if s == right.val.i64() {
										ComptimeBranchSkipState.eval
									} else {
										ComptimeBranchSkipState.skip
									}
								}
								else {
									ComptimeBranchSkipState.skip
								}
							}
						}
					}
				}
			}
			cur_skip_flags := c.skip_flags
			if found_branch {
				c.skip_flags = true
			} else if skip_state == .skip {
				c.skip_flags = true
				skip_state = .unknown // Reset the value of `skip_state` for the next branch
			} else if skip_state == .eval {
				found_branch = true // If a branch wasn't skipped, the rest must be
				c.skip_flags = skip_state == .skip
			}
			if c.fn_level == 0 && c.pref.output_cross_c {
				// do not skip any of the branches for top level `$if OS {`
				// statements, in `-cross` mode
				found_branch = false
				c.skip_flags = false
				c.ct_cond_stack << branch.cond
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
			} else if !is_comptime_type_is_expr {
				node.branches[i].stmts = []
			}
			if comptime_field_name.len > 0 {
				if comptime_field_name == c.comptime.comptime_for_method_var {
					c.type_resolver.update_ct_type(comptime_field_name, c.comptime.comptime_for_method_ret_type)
				} else if comptime_field_name == c.comptime.comptime_for_field_var {
					c.type_resolver.update_ct_type(comptime_field_name, c.comptime.comptime_for_field_type)
				}
			}
			c.skip_flags = cur_skip_flags
			if c.fn_level == 0 && c.pref.output_cross_c && c.ct_cond_stack.len > 0 {
				c.ct_cond_stack.delete_last()
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
						continue
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
						continue
					}
					if !c.check_types(stmt.typ, node.typ) {
						if node.typ == ast.void_type {
							// first branch of if expression
							node.is_expr = true
							if stmt.expr.is_auto_deref_var() {
								node.typ = stmt.typ.deref()
							} else {
								node.typ = stmt.typ
							}
							c.expected_expr_type = node.typ
							continue
						} else if node.typ in [ast.float_literal_type, ast.int_literal_type] {
							if node.typ == ast.int_literal_type {
								if stmt.typ.is_int() || stmt.typ.is_float() {
									node.typ = stmt.typ
									continue
								}
							} else { // node.typ == float_literal
								if stmt.typ.is_float() {
									node.typ = stmt.typ
									continue
								}
							}
						}
						if stmt.typ in [ast.float_literal_type, ast.int_literal_type] {
							if stmt.typ == ast.int_literal_type {
								if node.typ.is_int() || node.typ.is_float() {
									continue
								}
							} else { // expr_type == float_literal
								if node.typ.is_float() {
									continue
								}
							}
						}
						if node.is_expr && c.table.sym(former_expected_type).kind == .sum_type {
							node.typ = former_expected_type
							continue
						}
						if is_noreturn_callexpr(stmt.expr) {
							continue
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
							continue
						}
						if (node.typ == ast.none_type && stmt.typ != ast.none_type)
							|| (stmt.typ == ast.none_type && node.typ != ast.none_type) {
							node.typ = if stmt.typ != ast.none_type {
								stmt.typ.set_flag(.option)
							} else {
								node.typ.set_flag(.option)
							}
							continue
						}
						c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(stmt.typ)}`',
							node.pos)
					} else {
						if node.is_expr == false && c.type_resolver.is_generic_param_var(stmt.expr) {
							// generic variable no yet type bounded
							node.is_expr = true
						}
						if c.inside_assign && node.is_expr && !node.typ.has_flag(.shared_f)
							&& stmt.typ != ast.voidptr_type {
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
	if node.typ == ast.none_type {
		c.error('invalid if expression, must supply at least one value other than `none`',
			node.pos)
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
			if node.left is ast.Ident && c.comptime.get_ct_type_var(node.left) == .smartcast {
				node.left_type = c.type_resolver.get_type(node.left)
				c.smartcast(mut node.left, node.left_type, node.left_type.clear_flag(.option), mut
					scope, true, true)
			} else {
				c.smartcast(mut node.left, node.left_type, node.left_type.clear_flag(.option), mut
					scope, false, true)
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
				right_sym := c.table.sym(right_type)
				mut expr_type := c.unwrap_generic(node.left_type)
				left_sym := c.table.sym(expr_type)
				if left_sym.kind == .aggregate {
					expr_type = (left_sym.info as ast.Aggregate).sum_type
				}
				if left_sym.kind == .interface {
					if right_sym.kind != .interface {
						c.type_implements(right_type, expr_type, node.pos)
					}
				} else if !c.check_types(right_type, expr_type) && left_sym.kind != .sum_type {
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
						if left_sym.kind in [.interface, .sum_type] {
							c.smartcast(mut node.left, node.left_type, right_type, mut
								scope, is_comptime, false)
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
		mut first_cond := control_expr.branches[0].cond
		// handles unwrapping on if var == none { /**/ } else { /*unwrapped var*/ }
		if mut first_cond is ast.InfixExpr {
			if first_cond.left in [ast.Ident, ast.SelectorExpr] && first_cond.op == .eq
				&& first_cond.right is ast.None {
				if c.comptime.get_ct_type_var(first_cond.left) == .smartcast {
					first_cond.left_type = c.type_resolver.get_type(first_cond.left)
					c.smartcast(mut first_cond.left, first_cond.left_type, first_cond.left_type.clear_flag(.option), mut
						scope, true, true)
				} else {
					c.smartcast(mut first_cond.left, first_cond.left_type, first_cond.left_type.clear_flag(.option), mut
						scope, false, true)
				}
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
