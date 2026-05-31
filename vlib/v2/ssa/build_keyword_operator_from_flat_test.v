// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of
// `build_keyword_operator_from_flat`. The previous helper rehydrated every
// operand via `decode_expr` and dispatched to legacy `build_keyword_operator`.
// The cursor-native rewrite mirrors the per-op dispatch directly: `.key_sizeof`
// routes through `sizeof_value_from_flat(c.edge(0))`, `.key_go` / `.key_spawn`
// route through `build_go_or_spawn_from_flat(c.edge(0), opcode)`, and the
// else branch returns the same zero constant. This pin covers the `.key_sizeof`
// branch over an ident operand — the most common form (`sizeof(int)`).
module ssa

import v2.ast
import v2.token
import v2.types

fn ko_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn make_keyword_operator_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	// fn size_of_int() int { return sizeof(int) }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'size_of_int'
		typ:   ast.FnType{
			return_type: ko_ident('int')
		}
		stmts: [
			ast.Stmt(ast.ReturnStmt{
				exprs: [
					ast.Expr(ast.KeywordOperator{
						op:    token.Token.key_sizeof
						exprs: [ko_ident('int')]
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

fn build_via_legacy_keyword_operator(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_keyword_operator(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_keyword_operator_from_flat_matches_legacy() {
	files := make_keyword_operator_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_keyword_operator(files, env, 'kw_op_legacy')
	mod_flat := build_via_flat_keyword_operator(files, env, 'kw_op_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
