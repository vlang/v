// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import v.ast
import v.pref

// TODO: non deferred
pub fn (mut c Checker) return_stmt(mut node ast.Return) {
	if isnil(c.table.cur_fn) {
		return
	}
	c.expected_type = c.table.cur_fn.return_type
	mut expected_type := c.unwrap_generic(c.expected_type)
	expected_type_sym := c.table.sym(expected_type)
	if node.exprs.len > 0 && c.table.cur_fn.return_type == ast.void_type {
		c.error('unexpected argument, current function does not return anything', node.exprs[0].pos())
		return
	} else if node.exprs.len == 0 && !(c.expected_type == ast.void_type
		|| expected_type_sym.kind == .void) {
		stype := c.table.type_to_str(expected_type)
		arg := if expected_type_sym.kind == .multi_return { 'arguments' } else { 'argument' }
		c.error('expected `$stype` $arg', node.pos)
		return
	}
	if node.exprs.len == 0 {
		return
	}
	exp_is_optional := expected_type.has_flag(.optional)
	exp_is_result := expected_type.has_flag(.result)
	mut expected_types := [expected_type]
	if expected_type_sym.info is ast.MultiReturn {
		expected_types = expected_type_sym.info.types
		if c.table.cur_concrete_types.len > 0 {
			expected_types = expected_types.map(c.unwrap_generic(it))
		}
	}
	mut got_types := []ast.Type{}
	mut expr_idxs := []int{}
	for i, expr in node.exprs {
		mut typ := c.expr(expr)
		if typ == ast.void_type {
			c.error('`$expr` used as value', node.pos)
		}
		// Unpack multi return types
		sym := c.table.sym(typ)
		if sym.kind == .multi_return {
			for t in sym.mr_info().types {
				got_types << t
				expr_idxs << i
			}
		} else {
			if expr is ast.Ident {
				if expr.obj is ast.Var {
					if expr.obj.smartcasts.len > 0 {
						typ = c.unwrap_generic(expr.obj.smartcasts.last())
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
						c.note('returning a parameter in a fn marked with `[manualfree]` can cause double freeing in the caller',
							node.pos)
					}
				}
			}
		}
	}
	// allow `none` & `error` return types for function that returns optional
	option_type_idx := c.table.type_idxs['_option']
	result_type_idx := c.table.type_idxs['_result']
	got_types_0_idx := got_types[0].idx()
	if (exp_is_optional
		&& got_types_0_idx in [ast.none_type_idx, ast.error_type_idx, option_type_idx])
		|| (exp_is_result && got_types_0_idx in [ast.error_type_idx, result_type_idx]) {
		if got_types_0_idx == ast.none_type_idx && expected_type == ast.ovoid_type {
			c.error('returning `none` in functions, that have a `?` result type is not allowed anymore, either `return error(message)` or just `return` instead',
				node.pos)
		}
		return
	}
	if expected_types.len > 0 && expected_types.len != got_types.len {
		arg := if expected_types.len == 1 { 'argument' } else { 'arguments' }
		midx := imax(0, imin(expected_types.len, expr_idxs.len - 1))
		mismatch_pos := node.exprs[expr_idxs[midx]].pos()
		c.error('expected $expected_types.len $arg, but got $got_types.len', mismatch_pos)
		return
	}
	for i, exp_type in expected_types {
		got_typ := c.unwrap_generic(got_types[i])
		if got_typ.has_flag(.optional) && (!exp_type.has_flag(.optional)
			|| c.table.type_to_str(got_typ) != c.table.type_to_str(exp_type)) {
			pos := node.exprs[expr_idxs[i]].pos()
			c.error('cannot use `${c.table.type_to_str(got_typ)}` as type `${c.table.type_to_str(exp_type)}` in return argument',
				pos)
		}
		if !c.check_types(got_typ, exp_type) {
			got_typ_sym := c.table.sym(got_typ)
			mut exp_typ_sym := c.table.sym(exp_type)
			if exp_typ_sym.kind == .interface_ {
				if c.type_implements(got_typ, exp_type, node.pos) {
					if !got_typ.is_ptr() && !got_typ.is_pointer() && got_typ_sym.kind != .interface_
						&& !c.inside_unsafe {
						c.mark_as_referenced(mut &node.exprs[expr_idxs[i]], true)
					}
				}
				continue
			}
			pos := node.exprs[expr_idxs[i]].pos()
			c.error('cannot use `$got_typ_sym.name` as type `${c.table.type_to_str(exp_type)}` in return argument',
				pos)
		}
		if (got_typ.is_ptr() || got_typ.is_pointer())
			&& (!exp_type.is_ptr() && !exp_type.is_pointer()) {
			pos := node.exprs[expr_idxs[i]].pos()
			if node.exprs[expr_idxs[i]].is_auto_deref_var() {
				continue
			}
			c.error('fn `$c.table.cur_fn.name` expects you to return a non reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_typ)}` instead',
				pos)
		}
		if (exp_type.is_ptr() || exp_type.is_pointer())
			&& (!got_typ.is_ptr() && !got_typ.is_pointer()) && got_typ != ast.int_literal_type
			&& !c.pref.translated && !c.file.is_translated {
			pos := node.exprs[expr_idxs[i]].pos()
			if node.exprs[expr_idxs[i]].is_auto_deref_var() {
				continue
			}
			c.error('fn `$c.table.cur_fn.name` expects you to return a reference type `${c.table.type_to_str(exp_type)}`, but you are returning `${c.table.type_to_str(got_typ)}` instead',
				pos)
		}
		if exp_type.is_ptr() && got_typ.is_ptr() {
			mut r_expr := &node.exprs[expr_idxs[i]]
			if mut r_expr is ast.Ident {
				if mut r_expr.obj is ast.Var {
					mut obj := unsafe { &r_expr.obj }
					if c.fn_scope != voidptr(0) {
						obj = c.fn_scope.find_var(r_expr.obj.name) or { obj }
					}
					if obj.is_stack_obj && !c.inside_unsafe {
						type_sym := c.table.sym(obj.typ.set_nr_muls(0))
						if !type_sym.is_heap() && !c.pref.translated && !c.file.is_translated {
							suggestion := if type_sym.kind == .struct_ {
								'declaring `$type_sym.name` as `[heap]`'
							} else {
								'wrapping the `$type_sym.name` object in a `struct` declared as `[heap]`'
							}
							c.error('`$r_expr.name` cannot be returned outside `unsafe` blocks as it might refer to an object stored on stack. Consider ${suggestion}.',
								r_expr.pos)
						}
					}
				}
			}
		}
	}
	if exp_is_optional && node.exprs.len > 0 {
		expr0 := node.exprs[0]
		if expr0 is ast.CallExpr {
			if expr0.or_block.kind == .propagate_option && node.exprs.len == 1 {
				c.error('`?` is not needed, use `return ${expr0.name}()`', expr0.pos)
			}
		}
	}
}

pub fn (mut c Checker) find_unreachable_statements_after_noreturn_calls(stmts []ast.Stmt) {
	mut prev_stmt_was_noreturn_call := false
	for stmt in stmts {
		if stmt is ast.ExprStmt {
			if stmt.expr is ast.CallExpr {
				if prev_stmt_was_noreturn_call {
					c.error('unreachable code after a [noreturn] call', stmt.pos)
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
fn has_top_return(stmts []ast.Stmt) bool {
	for stmt in stmts {
		match stmt {
			ast.Return {
				return true
			}
			ast.Block {
				if has_top_return(stmt.stmts) {
					return true
				}
			}
			ast.ExprStmt {
				if stmt.expr is ast.CallExpr {
					if stmt.expr.is_noreturn {
						return true
					}
				}
			}
			else {}
		}
	}
	return false
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
	if node.stmts.len != 0 {
		mut is_valid_end_of_noreturn_fn := false
		last_stmt := node.stmts.last()
		match last_stmt {
			ast.ExprStmt {
				if last_stmt.expr is ast.CallExpr {
					if last_stmt.expr.should_be_skipped {
						c.error('[noreturn] functions cannot end with a skippable `[if ..]` call',
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
			c.error('[noreturn] functions should end with a call to another [noreturn] function, or with an infinite `for {}` loop',
				last_stmt.pos)
			return
		}
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

fn imin(a int, b int) int {
	return if a < b { a } else { b }
}

fn imax(a int, b int) int {
	return if a < b { b } else { a }
}
