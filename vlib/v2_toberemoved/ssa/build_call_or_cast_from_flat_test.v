// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of `build_call_or_cast_from_flat`.
// The previous helper rehydrated CallOrCastExpr into `ast.Expr` via two
// `decode_expr` calls and dispatched to legacy `build_call_or_cast`; the
// cursor-native rewrite uses `ast_type_to_ssa_from_flat` for the lhs type-expr,
// plus `build_addr_from_flat` and `build_expr_from_flat` for the operand
// edge. This pin asserts module-count parity for a focused fixture exercising
// the common numeric cast path `i64(value)`.
module ssa

import v2.ast
import v2.token
import v2.types

fn coc_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn coc_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn make_call_or_cast_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	stmts << ast.Stmt(ast.FnDecl{
		name:  'widen'
		typ:   ast.FnType{
			return_type: coc_ident('i64')
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.CallOrCastExpr{
						lhs:  coc_ident('i64')
						expr: coc_num('7')
					}),
				]
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

fn build_via_legacy_call_or_cast(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_call_or_cast(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_call_or_cast_from_flat_matches_legacy() {
	_ = token.Token.amp
	files := make_call_or_cast_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_call_or_cast(files, env, 'coc_legacy')
	mod_flat := build_via_flat_call_or_cast(files, env, 'coc_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
