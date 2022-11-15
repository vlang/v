module checker

import v.ast
import v.pref
import v.util
import v.token
import strings

pub fn (mut c Checker) match_expr(mut node ast.MatchExpr) ast.Type {
	node.is_expr = c.expected_type != ast.void_type
	node.expected_type = c.expected_type
	if mut node.cond is ast.ParExpr && !c.pref.translated && !c.file.is_translated {
		c.error('unnecessary `()` in `match` condition, use `match expr {` instead of `match (expr) {`.',
			node.cond.pos)
	}
	if node.is_expr {
		c.expected_expr_type = c.expected_type
		defer {
			c.expected_expr_type = ast.void_type
		}
	}
	cond_type := c.expr(node.cond)
	// we setting this here rather than at the end of the method
	// since it is used in c.match_exprs() it saves checking twice
	node.cond_type = ast.mktyp(cond_type)
	if (node.cond is ast.Ident && (node.cond as ast.Ident).is_mut)
		|| (node.cond is ast.SelectorExpr && (node.cond as ast.SelectorExpr).is_mut) {
		c.fail_if_immutable(node.cond)
	}
	c.ensure_type_exists(node.cond_type, node.pos) or { return ast.void_type }
	c.check_expr_opt_call(node.cond, cond_type)
	cond_type_sym := c.table.sym(cond_type)
	node.is_sum_type = cond_type_sym.kind in [.interface_, .sum_type]
	c.match_exprs(mut node, cond_type_sym)
	c.expected_type = cond_type
	mut first_iteration := true
	mut ret_type := ast.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	for branch in node.branches {
		if node.is_expr {
			c.stmts_ending_with_expression(branch.stmts)
		} else {
			c.stmts(branch.stmts)
		}
		c.smartcast_mut_pos = token.Pos{}
		c.smartcast_cond_pos = token.Pos{}
		if node.is_expr {
			if branch.stmts.len == 0 && ret_type != ast.void_type {
				c.error('`match` expression requires an expression as the last statement of every branch',
					branch.branch_pos)
			}
		}
		// If the last statement is an expression, return its type
		if branch.stmts.len > 0 {
			mut stmt := branch.stmts.last()
			if mut stmt is ast.ExprStmt {
				if node.is_expr {
					c.expected_type = node.expected_type
				}
				expr_type := c.expr(stmt.expr)
				stmt.typ = expr_type
				if first_iteration {
					if node.is_expr && (node.expected_type.has_flag(.optional)
						|| node.expected_type.has_flag(.result)
						|| c.table.type_kind(node.expected_type) in [.sum_type, .multi_return]) {
						c.check_match_branch_last_stmt(stmt, node.expected_type, expr_type)
						ret_type = node.expected_type
					} else {
						ret_type = expr_type
					}
				} else if node.is_expr && ret_type.idx() != expr_type.idx() {
					c.check_match_branch_last_stmt(stmt, ret_type, expr_type)
				}
			} else {
				if node.is_expr && ret_type != ast.void_type {
					c.error('`match` expression requires an expression as the last statement of every branch',
						stmt.pos)
				}
			}
		}
		first_iteration = false
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
			// an exhaustive match, and all branches returned
			c.returns = true
		}
		if nbranches_without_return > 0 {
			// some of the branches did not return
			c.returns = false
		}
	}
	node.return_type = ret_type
	cond_var := c.get_base_name(&node.cond)
	if cond_var != '' {
		mut cond_is_auto_heap := false
		for branch in node.branches {
			if v := branch.scope.find_var(cond_var) {
				if v.is_auto_heap {
					cond_is_auto_heap = true
					break
				}
			}
		}
		if cond_is_auto_heap {
			for branch in node.branches {
				mut v := branch.scope.find_var(cond_var) or { continue }
				v.is_auto_heap = true
			}
		}
	}
	return ret_type
}

fn (mut c Checker) check_match_branch_last_stmt(last_stmt ast.ExprStmt, ret_type ast.Type, expr_type ast.Type) {
	if !c.check_types(ret_type, expr_type) && !c.check_types(expr_type, ret_type) {
		ret_sym := c.table.sym(ret_type)
		is_noreturn := is_noreturn_callexpr(last_stmt.expr)
		if !(ret_sym.kind == .sum_type && (ret_type.has_flag(.generic)
			|| c.table.is_sumtype_or_in_variant(ret_type, expr_type))) && !is_noreturn {
			expr_sym := c.table.sym(expr_type)
			if expr_sym.kind == .multi_return && ret_sym.kind == .multi_return {
				ret_types := ret_sym.mr_info().types
				expr_types := expr_sym.mr_info().types.map(ast.mktyp(it))
				if expr_types == ret_types {
					return
				}
			}
			c.error('return type mismatch, it should be `${ret_sym.name}`', last_stmt.pos)
		}
	}
}

fn (mut c Checker) match_exprs(mut node ast.MatchExpr, cond_type_sym ast.TypeSymbol) {
	// branch_exprs is a histogram of how many times
	// an expr was used in the match
	mut branch_exprs := map[string]int{}
	for branch_i, _ in node.branches {
		mut branch := node.branches[branch_i]
		mut expr_types := []ast.TypeNode{}
		for k, expr in branch.exprs {
			mut key := ''
			if expr is ast.RangeExpr {
				mut low := i64(0)
				mut high := i64(0)
				c.expected_type = node.expected_type
				low_expr := expr.low
				high_expr := expr.high
				final_cond_sym := c.table.final_sym(node.cond_type)
				if low_expr is ast.IntegerLiteral {
					if high_expr is ast.IntegerLiteral
						&& (final_cond_sym.is_int() || final_cond_sym.info is ast.Enum) {
						low = low_expr.val.i64()
						high = high_expr.val.i64()
						if low > high {
							c.error('start value is higher than end value', branch.pos)
						}
					} else {
						c.error('mismatched range types - ${expr.low} is an integer, but ${expr.high} is not',
							low_expr.pos)
					}
				} else if low_expr is ast.CharLiteral {
					if high_expr is ast.CharLiteral && final_cond_sym.kind in [.u8, .char, .rune] {
						low = low_expr.val[0]
						high = high_expr.val[0]
						if low > high {
							c.error('start value is higher than end value', branch.pos)
						}
					} else {
						typ := c.table.type_to_str(c.expr(node.cond))
						c.error('mismatched range types - trying to match `${node.cond}`, which has type `${typ}`, against a range of `rune`',
							low_expr.pos)
					}
				} else {
					typ := c.table.type_to_str(c.expr(expr.low))
					c.error('cannot use type `${typ}` in match range', branch.pos)
				}
				high_low_cutoff := 1000
				if high - low > high_low_cutoff {
					c.warn('more than ${high_low_cutoff} possibilities (${low} ... ${high}) in match range',
						branch.pos)
				}
				for i in low .. high + 1 {
					key = i.str()
					val := if key in branch_exprs { branch_exprs[key] } else { 0 }
					if val == 1 {
						c.error('match case `${key}` is handled more than once', branch.pos)
					}
					branch_exprs[key] = val + 1
				}
				continue
			}
			match expr {
				ast.TypeNode {
					key = c.table.type_to_str(expr.typ)
					expr_types << expr
				}
				ast.EnumVal {
					key = expr.val
				}
				else {
					key = expr.str()
				}
			}
			val := if key in branch_exprs { branch_exprs[key] } else { 0 }
			if val == 1 {
				c.error('match case `${key}` is handled more than once', branch.pos)
			}
			c.expected_type = node.cond_type
			expr_type := c.expr(expr)
			if expr_type.idx() == 0 {
				// parser failed, stop checking
				return
			}
			expr_type_sym := c.table.sym(expr_type)
			if cond_type_sym.kind == .interface_ {
				// TODO
				// This generates a memory issue with TCC
				// Needs to be checked later when TCC errors are fixed
				// Current solution is to move expr.pos() to its own statement
				// c.type_implements(expr_type, c.expected_type, expr.pos())
				expr_pos := expr.pos()
				if c.type_implements(expr_type, c.expected_type, expr_pos) {
					if !expr_type.is_ptr() && !expr_type.is_pointer() && !c.inside_unsafe {
						if expr_type_sym.kind != .interface_ {
							c.mark_as_referenced(mut &branch.exprs[k], true)
						}
					}
				}
			} else if cond_type_sym.info is ast.SumType {
				if expr_type !in cond_type_sym.info.variants {
					expr_str := c.table.type_to_str(expr_type)
					expect_str := c.table.type_to_str(node.cond_type)
					sumtype_variant_names := cond_type_sym.info.variants.map(c.table.type_to_str_using_aliases(it,
						{}))
					suggestion := util.new_suggestion(expr_str, sumtype_variant_names)
					c.error(suggestion.say('`${expect_str}` has no variant `${expr_str}`'),
						expr.pos())
				}
			} else if cond_type_sym.info is ast.Alias && expr_type_sym.info is ast.Struct {
				expr_str := c.table.type_to_str(expr_type)
				expect_str := c.table.type_to_str(node.cond_type)
				c.error('cannot match alias type `${expect_str}` with `${expr_str}`',
					expr.pos())
			} else if !c.check_types(expr_type, node.cond_type) {
				expr_str := c.table.type_to_str(expr_type)
				expect_str := c.table.type_to_str(node.cond_type)
				c.error('cannot match `${expect_str}` with `${expr_str}`', expr.pos())
			}
			branch_exprs[key] = val + 1
		}
		// when match is type matching, then register smart cast for every branch
		if expr_types.len > 0 {
			if cond_type_sym.kind in [.sum_type, .interface_] {
				mut expr_type := ast.Type(0)
				if expr_types.len > 1 {
					mut agg_name := strings.new_builder(20)
					mut agg_cname := strings.new_builder(20)
					agg_name.write_string('(')
					for i, expr in expr_types {
						if i > 0 {
							agg_name.write_string(' | ')
							agg_cname.write_string('___')
						}
						type_str := c.table.type_to_str(expr.typ)
						name := if c.is_builtin_mod { type_str } else { '${c.mod}.${type_str}' }
						agg_name.write_string(name)
						agg_cname.write_string(util.no_dots(name))
					}
					agg_name.write_string(')')
					name := agg_name.str()
					existing_idx := c.table.type_idxs[name]
					if existing_idx > 0 {
						expr_type = existing_idx
					} else {
						expr_type = c.table.register_sym(ast.TypeSymbol{
							name: name
							cname: agg_cname.str()
							kind: .aggregate
							mod: c.mod
							info: ast.Aggregate{
								sum_type: node.cond_type
								types: expr_types.map(it.typ)
							}
						})
					}
				} else {
					expr_type = expr_types[0].typ
				}

				c.smartcast(node.cond, node.cond_type, expr_type, mut branch.scope)
			}
		}
	}
	// check that expressions are exhaustive
	// this is achieved either by putting an else
	// or, when the match is on a sum type or an enum
	// by listing all variants or values
	mut is_exhaustive := true
	mut unhandled := []string{}
	if node.cond_type == ast.bool_type {
		variants := ['true', 'false']
		for v in variants {
			if v !in branch_exprs {
				is_exhaustive = false
				unhandled << '`${v}`'
			}
		}
	} else {
		match cond_type_sym.info {
			ast.SumType {
				for v in cond_type_sym.info.variants {
					v_str := c.table.type_to_str(v)
					if v_str !in branch_exprs {
						is_exhaustive = false
						unhandled << '`${v_str}`'
					}
				}
			}
			//
			ast.Enum {
				for v in cond_type_sym.info.vals {
					if v !in branch_exprs {
						is_exhaustive = false
						unhandled << '`.${v}`'
					}
				}
			}
			else {
				is_exhaustive = false
			}
		}
	}
	mut else_branch := node.branches.last()
	mut has_else := else_branch.is_else
	if !has_else {
		for i, branch in node.branches {
			if branch.is_else && i != node.branches.len - 1 {
				c.error('`else` must be the last branch of `match`', branch.pos)
				else_branch = branch
				has_else = true
			}
		}
	}
	if is_exhaustive {
		if has_else && !c.pref.translated && !c.file.is_translated {
			c.error('match expression is exhaustive, `else` is unnecessary', else_branch.pos)
		}
		return
	}
	if has_else {
		return
	}
	mut err_details := 'match must be exhaustive'
	if unhandled.len > 0 {
		err_details += ' (add match branches for: '
		if unhandled.len < c.match_exhaustive_cutoff_limit {
			err_details += unhandled.join(', ')
		} else {
			remaining := unhandled.len - c.match_exhaustive_cutoff_limit
			err_details += unhandled[0..c.match_exhaustive_cutoff_limit].join(', ')
			if remaining > 0 {
				err_details += ', and ${remaining} others ...'
			}
		}
		err_details += ' or `else {}` at the end)'
	} else {
		err_details += ' (add `else {}` at the end)'
	}
	c.error(err_details, node.pos)
}
