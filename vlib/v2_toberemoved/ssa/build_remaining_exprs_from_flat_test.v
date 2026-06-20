// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Parity pin for the final build_expr_from_flat sweep after s197. The fixture
// intentionally mixes value-producing arms (postfix, array init, string
// interpolation, call-or-cast, as-cast, tuple, or, fn literal) with legacy
// zero-fallback arms (range, map, match, assoc, if-guard, comptime, generic,
// lambda, lifetime, lock, select, sql).
module ssa

import v2.ast
import v2.token
import v2.types

fn rem_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn rem_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn rem_ret_fn(name string, expr ast.Expr) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name:  name
		typ:   ast.FnType{
			return_type: rem_ident('int')
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [expr]
			}),
		]
	})
}

fn rem_expr_fn(name string, expr ast.Expr) ast.Stmt {
	return ast.Stmt(ast.FnDecl{
		name:  name
		typ:   ast.FnType{}
		stmts: [
			ast.Stmt(ast.ExprStmt{
				expr: expr
			}),
		]
	})
}

fn make_remaining_expr_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	stmts << rem_ret_fn('keyword_true', ast.Expr(ast.Keyword{
		tok: .key_true
	}))
	stmts << rem_ret_fn('sizeof_int', ast.Expr(ast.KeywordOperator{
		op:    .key_sizeof
		exprs: [rem_ident('int')]
	}))
	stmts << ast.Stmt(ast.FnDecl{
		name:  'postfix_inc'
		typ:   ast.FnType{
			return_type: rem_ident('int')
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [rem_ident('x')]
				rhs: [rem_num('1')]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.PostfixExpr{
					op:   .inc
					expr: rem_ident('x')
				})
			}),
			ast.Stmt(ast.ReturnStmt{
				exprs: [rem_ident('x')]
			}),
		]
	})
	stmts << rem_expr_fn('array_literal', ast.Expr(ast.ArrayInitExpr{
		exprs: [rem_num('1'), rem_num('2')]
	}))
	stmts << rem_expr_fn('string_inter', ast.Expr(ast.StringInterLiteral{
		kind:   .v
		values: ["'hi'"]
	}))
	stmts << rem_ret_fn('or_inner', ast.Expr(ast.OrExpr{
		expr: rem_num('9')
	}))
	stmts << rem_expr_fn('anon_fn', ast.Expr(ast.FnLiteral{
		typ: ast.FnType{}
	}))
	stmts << rem_ret_fn('call_or_cast', ast.Expr(ast.CallOrCastExpr{
		lhs:  rem_ident('int')
		expr: rem_num('7')
	}))
	stmts << rem_ret_fn('as_cast', ast.Expr(ast.AsCastExpr{
		expr: rem_num('8')
		typ:  rem_ident('int')
	}))
	stmts << rem_ret_fn('tuple_first', ast.Expr(ast.Tuple{
		exprs: [rem_num('3'), rem_num('4')]
	}))
	stmts << rem_ret_fn('map_zero', ast.Expr(ast.MapInitExpr{
		keys: [rem_num('1')]
		vals: [rem_num('2')]
	}))
	stmts << rem_ret_fn('range_zero', ast.Expr(ast.RangeExpr{
		op:    .dotdot
		start: rem_num('0')
		end:   rem_num('3')
	}))
	stmts << rem_ret_fn('match_zero', ast.Expr(ast.MatchExpr{
		expr: rem_num('1')
	}))
	stmts << rem_ret_fn('assoc_zero', ast.Expr(ast.AssocExpr{
		typ:  rem_ident('Point')
		expr: rem_ident('p')
	}))
	stmts << rem_ret_fn('if_guard_zero', ast.Expr(ast.IfGuardExpr{
		stmt: ast.AssignStmt{
			op:  .decl_assign
			lhs: [rem_ident('x')]
			rhs: [rem_num('1')]
		}
	}))
	stmts << rem_ret_fn('comptime_zero', ast.Expr(ast.ComptimeExpr{
		expr: rem_num('1')
	}))
	stmts << rem_ret_fn('generic_index_zero', ast.Expr(ast.GenericArgOrIndexExpr{
		lhs:  rem_ident('foo')
		expr: rem_ident('int')
	}))
	stmts << rem_ret_fn('generic_args_zero', ast.Expr(ast.GenericArgs{
		lhs:  rem_ident('foo')
		args: [rem_ident('int')]
	}))
	stmts << rem_ret_fn('lambda_zero', ast.Expr(ast.LambdaExpr{
		args: [ast.Ident{
			name: 'x'
		}]
		expr: rem_num('1')
	}))
	stmts << rem_ret_fn('lifetime_zero', ast.Expr(ast.LifetimeExpr{
		name: 'a'
	}))
	stmts << rem_ret_fn('lock_zero', ast.Expr(ast.LockExpr{
		lock_exprs: [rem_ident('x')]
	}))
	stmts << rem_ret_fn('select_zero', ast.Expr(ast.SelectExpr{
		stmt: ast.empty_stmt
		next: ast.empty_expr
	}))
	stmts << rem_ret_fn('sql_zero', ast.Expr(ast.SqlExpr{
		expr:       rem_ident('db')
		table_name: 'User'
	}))
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: stmts
		},
	]
}

fn build_remaining_via_legacy(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_remaining_via_flat(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_remaining_exprs_from_flat_match_legacy() {
	_ = token.Token.dotdot
	files := make_remaining_expr_fixture()
	env := types.Environment.new()
	mod_legacy := build_remaining_via_legacy(files, env, 'remaining_legacy')
	mod_flat := build_remaining_via_flat(files, env, 'remaining_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
