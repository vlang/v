// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s171: `register_consts_and_globals_from_flat`
// (flat-cursor port) must register the same globals + const_values as the
// legacy `register_consts_and_globals`. The flat port walks one file's
// top-level stmts via FileCursor and only rehydrates `.stmt_const_decl` /
// `.stmt_global_decl` nodes via `flat.decode_stmt`. ModuleStmt, FnDecl,
// StructDecl, EnumDecl, TypeDecl, ImportStmt etc. are never decoded.
module ssa

import v2.ast
import v2.types

fn make_consts_and_globals_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'answer'
							value: ast.Expr(ast.BasicLiteral{
								kind:  .number
								value: '42'
							})
						},
					]
				}),
				ast.Stmt(ast.GlobalDecl{
					fields: [
						ast.FieldDecl{
							name:  'g_counter'
							typ:   ast.Expr(ast.Ident{
								name: 'int'
							})
							value: ast.Expr(ast.BasicLiteral{
								kind:  .number
								value: '7'
							})
						},
					]
				}),
			]
		},
	]
}

// register_consts_and_globals_from_flat on a file with no const/global decls
// must produce zero new entries — same as register_consts_and_globals.
fn test_register_consts_and_globals_from_flat_empty_file_matches_legacy() {
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

	mut mod_legacy := Module.new('cg_empty_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_consts_and_globals(files[0])

	mut mod_flat := Module.new('cg_empty_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_consts_and_globals_from_flat(flat.file_cursor(0))

	// No const/global decls -> no new globals on either side.
	assert mod_legacy.globals.len == mod_flat.globals.len
	assert b_legacy.const_values.len == b_flat.const_values.len
}

// register_consts_and_globals_from_flat on a file with one ConstDecl + one
// GlobalDecl must register identical entries via both code paths.
fn test_register_consts_and_globals_from_flat_matches_legacy_for_consts_and_globals() {
	files := make_consts_and_globals_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('cg_mix_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_consts_and_globals(files[0])

	mut mod_flat := Module.new('cg_mix_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_consts_and_globals_from_flat(flat.file_cursor(0))

	// Both paths registered one const + one global => 2 globals each.
	assert mod_legacy.globals.len == mod_flat.globals.len
	assert mod_legacy.globals.len == 2
	for i in 0 .. mod_legacy.globals.len {
		assert mod_legacy.globals[i].name == mod_flat.globals[i].name
	}
	// const_values map must match for the answer constant.
	assert b_legacy.const_values.len == b_flat.const_values.len
	assert 'answer' in b_legacy.const_values
	assert 'answer' in b_flat.const_values
	assert b_legacy.const_values['answer'] == b_flat.const_values['answer']
	assert b_legacy.const_values['answer'] == 42
}
