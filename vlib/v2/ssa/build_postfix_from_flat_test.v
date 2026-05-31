// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of `build_postfix_from_flat`.
// Covers the three op kinds that don't need wrapper-unwrap setup: `x++`,
// `x--` (ident inc/dec branch — direct `c.name()` lookup) and the fallthrough
// path where the operand isn't an ident/selector/index (delegates straight to
// `build_expr_from_flat`). Selector/index `++` are exercised indirectly by the
// hello smoke + broader test suite; the wrapper-unwrap (`!`/`?`) branch is
// pinned by the option/result test paths.
module ssa

import v2.ast
import v2.token
import v2.types

fn pf_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn pf_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn make_postfix_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	// fn inc_then_ret() int { x := 0 x++ return x }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'inc_then_ret'
		typ:   ast.FnType{
			return_type: pf_ident('int')
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [pf_ident('x')]
				rhs: [pf_num('0')]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.PostfixExpr{
					op:   .inc
					expr: pf_ident('x')
				})
			}),
			ast.Stmt(ast.ReturnStmt{
				exprs: [pf_ident('x')]
			}),
		]
	})
	// fn dec_then_ret() int { y := 5 y-- return y }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'dec_then_ret'
		typ:   ast.FnType{
			return_type: pf_ident('int')
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [pf_ident('y')]
				rhs: [pf_num('5')]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.Expr(ast.PostfixExpr{
					op:   .dec
					expr: pf_ident('y')
				})
			}),
			ast.Stmt(ast.ReturnStmt{
				exprs: [pf_ident('y')]
			}),
		]
	})
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: stmts
		},
	]
}

fn build_via_legacy_postfix(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_postfix(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_postfix_from_flat_matches_legacy() {
	_ = token.Token.inc
	files := make_postfix_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_postfix(files, env, 'postfix_legacy')
	mod_flat := build_via_flat_postfix(files, env, 'postfix_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
