// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import strings
import v.ast
import v.table
import v.token
import v.util
import v.pref

pub fn (mut c Checker) if_expr(mut node ast.IfExpr) table.Type {
	if_kind := if node.is_comptime { '\$if' } else { 'if' }
	expr_required := c.expected_type != table.void_type
	former_expected_type := c.expected_type
	node.typ = table.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	mut should_skip := false // Whether the current branch should be skipped
	mut found_branch := false // Whether a matching branch was found- skip the rest
	mut is_comptime_type_is_expr := false // if `$if T is string`
	for i in 0 .. node.branches.len {
		mut branch := node.branches[i]
		if branch.cond is ast.ParExpr {
			c.error('unnecessary `()` in `$if_kind` condition, use `$if_kind expr {` instead of `$if_kind (expr) {`.',
				branch.pos)
		}
		if !node.has_else || i < node.branches.len - 1 {
			if node.is_comptime {
				should_skip = c.comp_if_branch(branch.cond, branch.pos)
			} else {
				// check condition type is boolean
				c.expected_type = table.bool_type
				cond_typ := c.expr(branch.cond)
				if cond_typ.idx() != table.bool_type_idx && !c.pref.translated {
					typ_sym := c.table.get_type_symbol(cond_typ)
					c.error('non-bool type `$typ_sym.name` used as if condition', branch.cond.position())
				}
			}
		}
		if node.is_comptime { // Skip checking if needed
			// smartcast field type on comptime if
			mut comptime_field_name := ''
			if branch.cond is ast.InfixExpr {
				if branch.cond.op == .key_is {
					if branch.cond.right !is ast.Type {
						c.error('invalid `\$if` condition: expected a type', branch.cond.right.position())
						return 0
					}
					got_type := c.unwrap_generic((branch.cond.right as ast.Type).typ)
					sym := c.table.get_type_symbol(got_type)
					if sym.kind == .placeholder || got_type.has_flag(.generic) {
						c.error('unknown type `$sym.name`', branch.cond.right.position())
					}
					left := branch.cond.left
					if left is ast.SelectorExpr {
						comptime_field_name = left.expr.str()
						c.comptime_fields_type[comptime_field_name] = got_type
						is_comptime_type_is_expr = true
					} else if left is ast.Type {
						is_comptime_type_is_expr = true
						left_type := c.unwrap_generic(left.typ)
						if left_type != got_type {
							should_skip = true
						}
					}
				}
			}
			cur_skip_flags := c.skip_flags
			if found_branch {
				c.skip_flags = true
			} else if should_skip {
				c.skip_flags = true
				should_skip = false // Reset the value of `should_skip` for the next branch
			} else if !is_comptime_type_is_expr {
				found_branch = true // If a branch wasn't skipped, the rest must be
			}
			if !c.skip_flags || c.pref.output_cross_c {
				c.stmts(branch.stmts)
			} else if !is_comptime_type_is_expr {
				node.branches[i].stmts = []
			}
			if comptime_field_name.len > 0 {
				c.comptime_fields_type.delete(comptime_field_name)
			}
			c.skip_flags = cur_skip_flags
		} else {
			// smartcast sumtypes and interfaces when using `is`
			pos := branch.cond.position()
			if branch.cond is ast.InfixExpr {
				if branch.cond.op == .key_is {
					right_expr := branch.cond.right as ast.Type
					left_sym := c.table.get_type_symbol(branch.cond.left_type)
					expr_type := c.expr(branch.cond.left)
					if left_sym.kind == .interface_ {
						c.type_implements(right_expr.typ, expr_type, pos)
					} else if !c.check_types(right_expr.typ, expr_type) {
						expect_str := c.table.type_to_str(right_expr.typ)
						expr_str := c.table.type_to_str(expr_type)
						c.error('cannot use type `$expect_str` as type `$expr_str`', pos)
					}
					if (branch.cond.left is ast.Ident || branch.cond.left is ast.SelectorExpr)
						&& branch.cond.right is ast.Type {
						is_variable := if mut branch.cond.left is ast.Ident {
							branch.cond.left.kind == .variable
						} else {
							true
						}
						if is_variable {
							if left_sym.kind in [.interface_, .sum_type] {
								if branch.cond.left is ast.Ident && left_sym.kind == .interface_ {
									// TODO: rewrite interface smartcast
									left := branch.cond.left as ast.Ident
									mut is_mut := false
									mut sum_type_casts := []table.Type{}
									if v := branch.scope.find_var(left.name) {
										is_mut = v.is_mut
										sum_type_casts << v.sum_type_casts
									}
									branch.scope.register(ast.Var{
										name: left.name
										typ: right_expr.typ.to_ptr()
										sum_type_casts: sum_type_casts
										pos: left.pos
										is_used: true
										is_mut: is_mut
									})
									// TODO: needs to be removed
									node.branches[i].smartcast = true
								} else {
									c.smartcast_sumtype(branch.cond.left, branch.cond.left_type,
										right_expr.typ, mut branch.scope)
								}
							}
						}
					}
				}
			}
			c.stmts(branch.stmts)
		}
		if expr_required {
			if branch.stmts.len > 0 && branch.stmts[branch.stmts.len - 1] is ast.ExprStmt {
				mut last_expr := branch.stmts[branch.stmts.len - 1] as ast.ExprStmt
				c.expected_type = former_expected_type
				if c.expected_type.has_flag(.optional) {
					if node.typ == table.void_type {
						node.is_expr = true
						node.typ = c.expected_type
					}
					continue
				}
				if c.expected_type.has_flag(.generic) {
					if node.typ == table.void_type {
						node.is_expr = true
						node.typ = c.unwrap_generic(c.expected_type)
					}
					continue
				}
				last_expr.typ = c.expr(last_expr.expr)
				if !c.check_types(last_expr.typ, node.typ) {
					if node.typ == table.void_type {
						// first branch of if expression
						node.is_expr = true
						node.typ = last_expr.typ
						continue
					} else if node.typ in [table.float_literal_type, table.int_literal_type] {
						if node.typ == table.int_literal_type {
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
					if last_expr.typ in [table.float_literal_type, table.int_literal_type] {
						if last_expr.typ == table.int_literal_type {
							if node.typ.is_int() || node.typ.is_float() {
								continue
							}
						} else { // expr_type == float_literal
							if node.typ.is_float() {
								continue
							}
						}
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
				st.check_c_expr() or { c.error('`if` expression branch has $err.msg', st.pos) }
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
	if node.typ == table.int_literal_type {
		node.typ = table.int_type
	} else if node.typ == table.float_literal_type {
		node.typ = table.f64_type
	}
	if expr_required && !node.has_else {
		d := if node.is_comptime { '$' } else { '' }
		c.error('`$if_kind` expression needs `${d}else` clause', node.pos)
	}
	return node.typ
}

// comp_if_branch checks the condition of a compile-time `if` branch. It returns a `bool` that
// saying whether that branch's contents should be skipped (targets a different os for example)
fn (mut c Checker) comp_if_branch(cond ast.Expr, pos token.Position) bool {
	// TODO: better error messages here
	match cond {
		ast.BoolLiteral {
			return !cond.val
		}
		ast.ParExpr {
			return c.comp_if_branch(cond.expr, pos)
		}
		ast.PrefixExpr {
			if cond.op != .not {
				c.error('invalid `\$if` condition', cond.pos)
			}
			return !c.comp_if_branch(cond.right, cond.pos)
		}
		ast.PostfixExpr {
			if cond.op != .question {
				c.error('invalid \$if postfix operator', cond.pos)
			} else if cond.expr is ast.Ident {
				return cond.expr.name !in c.pref.compile_defines_all
			} else {
				c.error('invalid `\$if` condition', cond.pos)
			}
		}
		ast.InfixExpr {
			match cond.op {
				.and {
					l := c.comp_if_branch(cond.left, cond.pos)
					r := c.comp_if_branch(cond.right, cond.pos)
					return l || r // skip (return true) if at least one should be skipped
				}
				.logical_or {
					l := c.comp_if_branch(cond.left, cond.pos)
					r := c.comp_if_branch(cond.right, cond.pos)
					return l && r // skip (return true) only if both should be skipped
				}
				.key_is, .not_is {
					if cond.left is ast.SelectorExpr || cond.left is ast.Type {
						// $if method.@type is string
						c.expr(cond.left)
						return false
					} else {
						c.error('invalid `\$if` condition: expected a type or selector expression',
							cond.left.position())
					}
				}
				.eq, .ne {
					if cond.left is ast.SelectorExpr && cond.right is ast.IntegerLiteral {
						// $if method.args.len == 1
					} else if cond.left is ast.Ident {
						// $if version == 2
						left_type := c.expr(cond.left)
						right_type := c.expr(cond.right)
						expr := c.find_definition(cond.left) or {
							c.error(err.msg, cond.left.pos)
							return false
						}
						if !c.check_types(right_type, left_type) {
							left_name := c.table.type_to_str(left_type)
							right_name := c.table.type_to_str(right_type)
							c.error('mismatched types `$left_name` and `$right_name`',
								cond.pos)
						}
						// :)
						// until `v.eval` is stable, I can't think of a better way to do this
						different := expr.str() != cond.right.str()
						return if cond.op == .eq { different } else { !different }
					} else {
						c.error('invalid `\$if` condition: ${cond.left.type_name()}1',
							cond.pos)
					}
				}
				else {
					c.error('invalid `\$if` condition', cond.pos)
				}
			}
		}
		ast.Ident {
			if cond.name in valid_comp_if_os {
				return cond.name != c.pref.os.str().to_lower() // TODO hack
			} else if cond.name in valid_comp_if_compilers {
				return pref.cc_from_string(cond.name) != c.pref.ccompiler_type
			} else if cond.name in valid_comp_if_platforms {
				return false // TODO
			} else if cond.name in valid_comp_if_other {
				// TODO: This should probably be moved
				match cond.name {
					'js' { return c.pref.backend != .js }
					'debug' { return !c.pref.is_debug }
					'prod' { return !c.pref.is_prod }
					'test' { return !c.pref.is_test }
					'glibc' { return false } // TODO
					'prealloc' { return !c.pref.prealloc }
					'no_bounds_checking' { return cond.name !in c.pref.compile_defines_all }
					else { return false }
				}
			} else if cond.name !in c.pref.compile_defines_all {
				// `$if some_var {}`
				typ := c.expr(cond)
				if cond.obj !is ast.Var && cond.obj !is ast.ConstField
					&& cond.obj !is ast.GlobalField {
					c.error('unknown var: `$cond.name`', pos)
					return false
				}
				expr := c.find_obj_definition(cond.obj) or {
					c.error(err.msg, cond.pos)
					return false
				}
				if !c.check_types(typ, table.bool_type) {
					type_name := c.table.type_to_str(typ)
					c.error('non-bool type `$type_name` used as \$if condition', cond.pos)
				}
				// :)
				// until `v.eval` is stable, I can't think of a better way to do this
				return !(expr as ast.BoolLiteral).val
			}
		}
		else {
			c.error('invalid `\$if` condition', pos)
		}
	}
	return false
}

pub fn (mut c Checker) match_expr(mut node ast.MatchExpr) table.Type {
	node.is_expr = c.expected_type != table.void_type
	node.expected_type = c.expected_type
	cond_type := c.expr(node.cond)
	// we setting this here rather than at the end of the method
	// since it is used in c.match_exprs() it saves checking twice
	node.cond_type = c.table.mktyp(cond_type)
	c.ensure_type_exists(node.cond_type, node.pos) or { return table.void_type }
	cond_type_sym := c.table.get_type_symbol(cond_type)
	if cond_type_sym.kind !in [.interface_, .sum_type] {
		node.is_sum_type = false
	}
	c.match_exprs(mut node, cond_type_sym)
	c.expected_type = cond_type
	mut ret_type := table.void_type
	mut nbranches_with_return := 0
	mut nbranches_without_return := 0
	for branch in node.branches {
		c.stmts(branch.stmts)
		if node.is_expr && branch.stmts.len > 0 {
			// ignore last statement - workaround
			// currently the last statement in a match branch does not have an
			// expected value set, so e.g. IfExpr.is_expr is not set.
			// probably any mismatch will be caught by not producing a value instead
			for st in branch.stmts[0..branch.stmts.len - 1] {
				// must not contain C statements
				st.check_c_expr() or { c.error('`match` expression branch has $err.msg', st.pos) }
			}
		}
		// If the last statement is an expression, return its type
		if branch.stmts.len > 0 {
			mut stmt := branch.stmts[branch.stmts.len - 1]
			match mut stmt {
				ast.ExprStmt {
					expr_type := c.expr(stmt.expr)
					if ret_type == table.void_type {
						ret_type = expr_type
						stmt.typ = ret_type
					} else if node.is_expr && ret_type != expr_type {
						if !c.check_types(ret_type, expr_type) {
							ret_sym := c.table.get_type_symbol(ret_type)
							c.error('return type mismatch, it should be `$ret_sym.name`',
								stmt.expr.position())
						}
					}
				}
				else {
					// TODO: ask alex about this
					// typ := c.expr(stmt.expr)
					// type_sym := c.table.get_type_symbol(typ)
					// p.warn('match expr ret $type_sym.name')
					// node.typ = typ
					// return typ
				}
			}
		}
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
	// if ret_type != table.void_type {
	// node.is_expr = c.expected_type != table.void_type
	// node.expected_type = c.expected_type
	// }
	node.return_type = ret_type
	return ret_type
}

fn (mut c Checker) match_exprs(mut node ast.MatchExpr, cond_type_sym table.TypeSymbol) {
	// branch_exprs is a histogram of how many times
	// an expr was used in the match
	mut branch_exprs := map[string]int{}
	for branch_i, _ in node.branches {
		mut branch := node.branches[branch_i]
		mut expr_types := []ast.Type{}
		for expr in branch.exprs {
			mut key := ''
			if expr is ast.RangeExpr {
				mut low := i64(0)
				mut high := i64(0)
				c.expected_type = node.expected_type
				low_expr := expr.low
				high_expr := expr.high
				if low_expr is ast.IntegerLiteral {
					if high_expr is ast.IntegerLiteral {
						low = low_expr.val.i64()
						high = high_expr.val.i64()
					} else {
						c.error('mismatched range types', low_expr.pos)
					}
				} else if low_expr is ast.CharLiteral {
					if high_expr is ast.CharLiteral {
						low = low_expr.val[0]
						high = high_expr.val[0]
					} else {
						c.error('mismatched range types', low_expr.pos)
					}
				} else {
					typ := c.table.type_to_str(c.expr(expr.low))
					c.error('cannot use type `$typ` in match range', branch.pos)
				}
				high_low_cutoff := 1000
				if high - low > high_low_cutoff {
					c.warn('more than $high_low_cutoff possibilities ($low ... $high) in match range',
						branch.pos)
				}
				for i in low .. high + 1 {
					key = i.str()
					val := if key in branch_exprs { branch_exprs[key] } else { 0 }
					if val == 1 {
						c.error('match case `$key` is handled more than once', branch.pos)
					}
					branch_exprs[key] = val + 1
				}
				continue
			}
			match expr {
				ast.Type {
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
				c.error('match case `$key` is handled more than once', branch.pos)
			}
			c.expected_type = node.cond_type
			expr_type := c.expr(expr)
			if expr_type.idx() == 0 {
				// parser failed, stop checking
				return
			}
			if cond_type_sym.kind == .interface_ {
				// TODO
				// This generates a memory issue with TCC
				// Needs to be checked later when TCC errors are fixed
				// Current solution is to move expr.position() to its own statement
				// c.type_implements(expr_type, c.expected_type, expr.position())
				expr_pos := expr.position()
				c.type_implements(expr_type, c.expected_type, expr_pos)
			} else if mut cond_type_sym.info is table.SumType {
				if expr_type !in cond_type_sym.info.variants {
					expr_str := c.table.type_to_str(expr_type)
					expect_str := c.table.type_to_str(node.cond_type)
					c.error('`$expect_str` has no variant `$expr_str`', expr.position())
				}
			} else if !c.check_types(expr_type, node.cond_type) {
				expr_str := c.table.type_to_str(expr_type)
				expect_str := c.table.type_to_str(node.cond_type)
				c.error('cannot match `$expr_str` with `$expect_str` condition', expr.position())
			}
			branch_exprs[key] = val + 1
		}
		// when match is sum type matching, then register smart cast for every branch
		if expr_types.len > 0 {
			if cond_type_sym.kind == .sum_type {
				mut expr_type := table.Type(0)
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
						name := if c.is_builtin_mod { type_str } else { '${c.mod}.$type_str' }
						agg_name.write_string(name)
						agg_cname.write_string(util.no_dots(name))
					}
					agg_name.write_string(')')
					name := agg_name.str()
					existing_idx := c.table.type_idxs[name]
					if existing_idx > 0 {
						expr_type = existing_idx
					} else {
						expr_type = c.table.register_type_symbol(table.TypeSymbol{
							name: name
							cname: agg_cname.str()
							kind: .aggregate
							mod: c.mod
							info: table.Aggregate{
								types: expr_types.map(it.typ)
							}
						})
					}
				} else {
					expr_type = expr_types[0].typ
				}
				c.smartcast_sumtype(node.cond, node.cond_type, expr_type, mut branch.scope)
			}
		}
	}
	// check that expressions are exhaustive
	// this is achieved either by putting an else
	// or, when the match is on a sum type or an enum
	// by listing all variants or values
	mut is_exhaustive := true
	mut unhandled := []string{}
	if node.cond_type == table.bool_type {
		variants := ['true', 'false']
		for v in variants {
			if v !in branch_exprs {
				is_exhaustive = false
				unhandled << '`$v`'
			}
		}
	} else {
		match mut cond_type_sym.info {
			table.SumType {
				for v in cond_type_sym.info.variants {
					v_str := c.table.type_to_str(v)
					if v_str !in branch_exprs {
						is_exhaustive = false
						unhandled << '`$v_str`'
					}
				}
			}
			//
			table.Enum {
				for v in cond_type_sym.info.vals {
					if v !in branch_exprs {
						is_exhaustive = false
						unhandled << '`.$v`'
					}
				}
			}
			else {
				is_exhaustive = false
			}
		}
	}
	mut else_branch := node.branches[node.branches.len - 1]
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
		if has_else {
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
			err_details += ', and $remaining others ...'
		}
		err_details += ' or `else {}` at the end)'
	} else {
		err_details += ' (add `else {}` at the end)'
	}
	c.error(err_details, node.pos)
}
