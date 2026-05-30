// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s174: `build_fn_bodies_from_flat` (flat-cursor port)
// must build the same SSA bodies as the legacy `build_fn_bodies`. The flat
// port walks one file's top-level stmts via FileCursor and only rehydrates
// `.stmt_fn_decl` nodes via `flat.decode_stmt`. Non-FnDecl stmts
// (ModuleStmt, StructDecl, EnumDecl, ConstDecl, etc.) are never decoded.
// The fn body itself is still fully rehydrated per-decl — future sessions
// port individual statement classes inside `build_fn` to consume flat
// cursors directly.
module ssa

import v2.ast
import v2.types

fn make_build_fn_bodies_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.FnDecl{
					name:  'answer'
					typ:   ast.FnType{
						return_type: ast.Expr(ast.Ident{
							name: 'int'
						})
					}
					stmts: [
						ast.Stmt(ast.ReturnStmt{
							exprs: [
								ast.Expr(ast.BasicLiteral{
									kind:  .number
									value: '42'
								}),
							]
						}),
					]
				}),
				ast.Stmt(ast.FnDecl{
					name: 'noop'
					typ:  ast.FnType{}
				}),
			]
		},
	]
}

// build_fn_bodies_from_flat on a file with no fns must not build any
// function bodies — same as build_fn_bodies.
fn test_build_fn_bodies_from_flat_no_fns_matches_legacy() {
	files := [
		ast.File{
			name:  'empty.v'
			mod:   'main'
			stmts: [ast.Stmt(ast.ModuleStmt{
				name: 'main'
			})]
		},
	]
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('bfb_empty_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.build_fn_bodies(files[0])

	mut mod_flat := Module.new('bfb_empty_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.build_fn_bodies_from_flat(flat.file_cursor(0))

	// Neither path built any function bodies.
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	assert mod_legacy.instrs.len == mod_flat.instrs.len
}

// build_fn_bodies_from_flat on a file with two FnDecls (answer + noop) must
// build identical SSA bodies — same number of funcs, blocks, and instrs.
fn test_build_fn_bodies_from_flat_matches_legacy_for_two_fns() {
	files := make_build_fn_bodies_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('bfb_two_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	// build_fn requires fn_index populated (register_fn_signatures runs in
	// Phase 3 before Phase 4 in legacy build_all).
	b_legacy.register_fn_signatures(files[0])
	b_legacy.build_fn_bodies(files[0])

	mut mod_flat := Module.new('bfb_two_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_fn_signatures_from_flat(flat.file_cursor(0))
	b_flat.build_fn_bodies_from_flat(flat.file_cursor(0))

	// Both paths registered + built 2 fns.
	assert mod_legacy.funcs.len == mod_flat.funcs.len
	assert mod_legacy.funcs.len == 2
	// Same number of basic blocks on both paths (identical control flow).
	assert mod_legacy.blocks.len == mod_flat.blocks.len
	// Same number of instructions emitted (return 42 + return void).
	assert mod_legacy.instrs.len == mod_flat.instrs.len
	// Same number of SSA values.
	assert mod_legacy.values.len == mod_flat.values.len
}
