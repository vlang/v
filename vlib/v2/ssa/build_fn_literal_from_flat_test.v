// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for the cursor-native rewrite of `build_fn_literal_from_flat`.
// The previous helper rehydrated the FnLiteral payload via three `decode_*`
// rounds — `decode_expr` on the FnType edge, `decode_expr` on every captured
// var edge, `decode_stmt` on every body-stmt edge — and dispatched to legacy
// `build_fn_literal`. The cursor-native rewrite walks the `.typ_fn` child for
// params + return_type, drops the captured-var rehydration entirely (legacy
// `build_fn_literal` never reads `captured_vars`), and routes body-stmt edges
// through `build_stmt_from_flat`. Pin asserts module-count parity for a
// fixture that constructs an anonymous fn with one int param and an int return.
module ssa

import v2.ast
import v2.types

fn fl_ident(name string) ast.Expr {
	return ast.Expr(ast.Ident{
		name: name
	})
}

fn fl_num(value string) ast.Expr {
	return ast.Expr(ast.BasicLiteral{
		kind:  .number
		value: value
	})
}

fn make_fn_literal_fixture() []ast.File {
	mut stmts := [
		ast.Stmt(ast.ModuleStmt{
			name: 'main'
		}),
	]
	// fn use_anon() { f := fn (n int) int { return n } }
	stmts << ast.Stmt(ast.FnDecl{
		name:  'use_anon'
		typ:   ast.FnType{
			return_type: ast.empty_expr
		}
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [fl_ident('f')]
				rhs: [
					ast.Expr(ast.FnLiteral{
						typ:   ast.FnType{
							params:      [
								ast.Parameter{
									name: 'n'
									typ:  fl_ident('int')
								},
							]
							return_type: fl_ident('int')
						}
						stmts: [
							ast.Stmt(ast.ReturnStmt{
								exprs: [fl_ident('n')]
							}),
						]
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

fn build_via_legacy_fn_literal(files []ast.File, env &types.Environment, name string) &Module {
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures(files[0])
	b.build_fn_bodies(files[0])
	return mod
}

fn build_via_flat_fn_literal(files []ast.File, env &types.Environment, name string) &Module {
	flat := ast.flatten_files(files)
	mut mod := Module.new(name)
	mut b := Builder.new_with_env(mod, env)
	b.register_fn_signatures_from_flat(flat.file_cursor(0))
	b.build_fn_bodies_from_flat(flat.file_cursor(0))
	return mod
}

fn test_build_fn_literal_from_flat_matches_legacy() {
	files := make_fn_literal_fixture()
	env := types.Environment.new()
	mod_legacy := build_via_legacy_fn_literal(files, env, 'fn_literal_legacy')
	mod_flat := build_via_flat_fn_literal(files, env, 'fn_literal_flat')

	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	assert mod_legacy.values.len == mod_flat.values.len
}
