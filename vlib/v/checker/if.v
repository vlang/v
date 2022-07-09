// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.pref
import v.token

pub fn (mut c Checker) if_expr(mut node ast.IfExpr) ast.Type {
	if_kind := if node.is_comptime { '\$if' } else { 'if' }
	mut node_is_expr := false
	if node.branches.len > 0 && node.has_else {
		stmts := node.branches[0].stmts
		if stmts.len > 0 && stmts.last() is ast.ExprStmt
			&& (stmts.last() as ast.ExprStmt).typ != ast.void_type {
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
	for i in 0 .. node.branches.len {
		mut branch := node.branches[i]
		if branch.cond is ast.ParExpr && !c.pref.translated && !c.file.is_translated {
			c.error('unnecessary `()` in `$if_kind` condition, use `$if_kind expr {` instead of `$if_kind (expr) {`.',
				branch.pos)
		}
		if !node.has_else || i < node.branches.len - 1 {
			if node.is_comptime {
				skip_state = c.comptime_if_branch(branch.cond, branch.pos)
				node.branches[i].pkg_exist = if skip_state == .eval { true } else { false }
			} else {
				// check condition type is boolean
				c.expected_type = ast.bool_type
				cond_typ := c.unwrap_generic(c.expr(branch.cond))
				if (cond_typ.idx() != ast.bool_type_idx || cond_typ.has_flag(.optional))
					&& !c.pref.translated && !c.file.is_translated {
					c.error('non-bool type `${c.table.type_to_str(cond_typ)}` used as if condition',
						branch.cond.pos())
				}
			}
		}
		if node.is_comptime { // Skip checking if needed
			// smartcast field type on comptime if
			mut comptime_field_name := ''
			if mut branch.cond is ast.InfixExpr {
				if branch.cond.op == .key_is {
					if branch.cond.right !is ast.TypeNode && branch.cond.right !is ast.ComptimeType {
						c.error('invalid `\$if` condition: expected a type', branch.cond.right.pos())
						return 0
					}
					left := branch.cond.left
					if branch.cond.right is ast.ComptimeType && left is ast.TypeNode {
						is_comptime_type_is_expr = true
						checked_type := c.unwrap_generic(left.typ)
						skip_state = if c.table.is_comptime_type(checked_type, branch.cond.right as ast.ComptimeType) {
							.eval
						} else {
							.skip
						}
					} else {
						got_type := c.unwrap_generic((branch.cond.right as ast.TypeNode).typ)
						sym := c.table.sym(got_type)
						if sym.kind == .placeholder || got_type.has_flag(.generic) {
							c.error('unknown type `$sym.name`', branch.cond.right.pos())
						}

						if left is ast.SelectorExpr {
							comptime_field_name = left.expr.str()
							c.comptime_fields_type[comptime_field_name] = got_type
							is_comptime_type_is_expr = true
						} else if branch.cond.right is ast.TypeNode && left is ast.TypeNode
							&& sym.kind == .interface_ {
							is_comptime_type_is_expr = true
							// is interface
							checked_type := c.unwrap_generic(left.typ)
							skip_state = if c.table.does_type_implement_interface(checked_type,
								got_type)
							{
								.eval
							} else {
								.skip
							}
						} else if left is ast.TypeNode {
							is_comptime_type_is_expr = true
							left_type := c.unwrap_generic(left.typ)
							skip_state = if left_type == got_type { .eval } else { .skip }
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
			} else if !is_comptime_type_is_expr && skip_state == .eval {
				found_branch = true // If a branch wasn't skipped, the rest must be
			}
			if c.fn_level == 0 && c.pref.output_cross_c {
				// do not skip any of the branches for top level `$if OS {`
				// statements, in `-os cross` mode
				found_branch = false
				c.skip_flags = false
				c.ct_cond_stack << branch.cond
			}
			if !c.skip_flags {
				if node_is_expr {
					c.stmts_ending_with_expression(branch.stmts)
				} else {
					c.stmts(branch.stmts)
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
					c.stmts_ending_with_expression(branch.stmts)
				} else {
					c.stmts(branch.stmts)
				}
			} else if !is_comptime_type_is_expr {
				node.branches[i].stmts = []
			}
			if comptime_field_name.len > 0 {
				c.comptime_fields_type[comptime_field_name] = c.comptime_fields_default_type
			}
			c.skip_flags = cur_skip_flags
			if c.fn_level == 0 && c.pref.output_cross_c && c.ct_cond_stack.len > 0 {
				c.ct_cond_stack.delete_last()
			}
		} else {
			// smartcast sumtypes and interfaces when using `is`
			c.smartcast_if_conds(branch.cond, mut branch.scope)
			if node_is_expr {
				c.stmts_ending_with_expression(branch.stmts)
			} else {
				c.stmts(branch.stmts)
			}
			c.smartcast_mut_pos = token.Pos{}
			c.smartcast_cond_pos = token.Pos{}
		}
		if expr_required {
			if branch.stmts.len > 0 && branch.stmts.last() is ast.ExprStmt {
				mut last_expr := branch.stmts.last() as ast.ExprStmt
				c.expected_type = former_expected_type
				if c.expected_type.has_flag(.optional) {
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
				last_expr.typ = c.expr(last_expr.expr)
				if c.table.type_kind(c.expected_type) == .multi_return
					&& c.table.type_kind(last_expr.typ) == .multi_return {
					if node.typ == ast.void_type {
						node.is_expr = true
						node.typ = c.expected_type
					}
				}
				if !c.check_types(last_expr.typ, node.typ) {
					if node.typ == ast.void_type {
						// first branch of if expression
						node.is_expr = true
						node.typ = last_expr.typ
						continue
					} else if node.typ in [ast.float_literal_type, ast.int_literal_type] {
						if node.typ == ast.int_literal_type {
							if last_expr.typ.is_int() || last_expr.typ.is_float() {
								node.typ = last_expr.typ
								continue
							}
						} else { // node.typ == float_literal
							if last_expr.typ.is_float() {
								node.typ = last_expr.typ
								continue
							}
						}
					}
					if last_expr.typ in [ast.float_literal_type, ast.int_literal_type] {
						if last_expr.typ == ast.int_literal_type {
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
					if is_noreturn_callexpr(last_expr.expr) {
						continue
					}
					c.error('mismatched types `${c.table.type_to_str(node.typ)}` and `${c.table.type_to_str(last_expr.typ)}`',
						node.pos)
				}
			} else {
				c.error('`$if_kind` expression requires an expression as the last statement of every branch',
					branch.pos)
			}
			for st in branch.stmts {
				// must not contain C statements
				st.check_c_expr() or { c.error('`if` expression branch has $err.msg()', st.pos) }
			}
		}
		if mut branch.cond is ast.IfGuardExpr {
			sym := c.table.sym(branch.cond.expr_type)
			if sym.kind == .multi_return {
				mr_info := sym.info as ast.MultiReturn
				if branch.cond.vars.len != mr_info.types.len {
					c.error('if guard expects $mr_info.types.len variables, but got $branch.cond.vars.len',
						branch.pos)
				} else {
					for vi, var in branch.cond.vars {
						branch.scope.update_var_type(var.name, mr_info.types[vi])
					}
				}
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
	// if only untyped literals were given default to int/f64
	node.typ = ast.mktyp(node.typ)
	if expr_required && !node.has_else {
		d := if node.is_comptime { '$' } else { '' }
		c.error('`$if_kind` expression needs `${d}else` clause', node.pos)
	}
	return node.typ
}

fn (mut c Checker) smartcast_if_conds(node ast.Expr, mut scope ast.Scope) {
	if node is ast.InfixExpr {
		if node.op == .and {
			c.smartcast_if_conds(node.left, mut scope)
			c.smartcast_if_conds(node.right, mut scope)
		} else if node.op == .key_is {
			right_expr := node.right
			right_type := match right_expr {
				ast.TypeNode {
					right_expr.typ
				}
				ast.None {
					ast.none_type_idx
				}
				else {
					c.error('invalid type `$right_expr`', right_expr.pos())
					ast.Type(0)
				}
			}
			if right_type != ast.Type(0) {
				left_sym := c.table.sym(node.left_type)
				right_sym := c.table.sym(right_type)
				mut expr_type := c.expr(node.left)
				if left_sym.kind == .aggregate {
					expr_type = (left_sym.info as ast.Aggregate).sum_type
				}
				if left_sym.kind == .interface_ {
					if right_sym.kind != .interface_ {
						c.type_implements(right_type, expr_type, node.pos)
					} else {
						return
					}
				} else if !c.check_types(right_type, expr_type) && left_sym.kind != .sum_type {
					expect_str := c.table.type_to_str(right_type)
					expr_str := c.table.type_to_str(expr_type)
					c.error('cannot use type `$expect_str` as type `$expr_str`', node.pos)
				}
				if node.left in [ast.Ident, ast.SelectorExpr] && node.right is ast.TypeNode {
					is_variable := if node.left is ast.Ident {
						node.left.kind == .variable
					} else {
						true
					}
					if is_variable {
						if (node.left is ast.Ident && (node.left as ast.Ident).is_mut)
							|| (node.left is ast.SelectorExpr
							&& (node.left as ast.SelectorExpr).is_mut) {
							c.fail_if_immutable(node.left)
						}
						if left_sym.kind in [.interface_, .sum_type] {
							c.smartcast(node.left, node.left_type, right_type, mut scope)
						}
					}
				}
			}
		}
	} else if node is ast.Likely {
		c.smartcast_if_conds(node.expr, mut scope)
	}
}
