// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s173: `resolve_forward_const_refs_from_flat`
// (flat-cursor port) must re-evaluate the same forward-referenced constants
// as the legacy `resolve_forward_const_refs`. The flat port walks each
// file's top-level stmts via FileCursor and only rehydrates
// `.stmt_const_decl` nodes via `flat.decode_stmt`. ModuleStmt, FnDecl,
// StructDecl, EnumDecl, TypeDecl, GlobalDecl, ImportStmt etc. are never
// decoded.
module ssa

import v2.ast
import v2.types

// Two consts where `A = B` references a later-declared `B = 5`. After
// `register_consts_and_globals`, A is 0 (forward ref). The resolver pass
// must re-evaluate A to 5 on both legacy and flat paths.
fn make_forward_const_fixture() []ast.File {
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
							name:  'a'
							value: ast.Expr(ast.Ident{
								name: 'b'
							})
						},
					]
				}),
				ast.Stmt(ast.ConstDecl{
					fields: [
						ast.FieldInit{
							name:  'b'
							value: ast.Expr(ast.BasicLiteral{
								kind:  .number
								value: '5'
							})
						},
					]
				}),
			]
		},
	]
}

// resolve_forward_const_refs_from_flat on a file with no forward references
// must be a no-op — same as resolve_forward_const_refs.
fn test_resolve_forward_const_refs_from_flat_noop_matches_legacy() {
	files := [
		ast.File{
			name:  'simple.v'
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
			]
		},
	]
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('rfcr_noop_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_consts_and_globals(files[0])
	b_legacy.resolve_forward_const_refs(files)

	mut mod_flat := Module.new('rfcr_noop_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_consts_and_globals_from_flat(flat.file_cursor(0))
	b_flat.resolve_forward_const_refs_from_flat(flat)

	assert b_legacy.const_values.len == b_flat.const_values.len
	assert 'answer' in b_legacy.const_values
	assert 'answer' in b_flat.const_values
	assert b_legacy.const_values['answer'] == b_flat.const_values['answer']
	assert b_legacy.const_values['answer'] == 42
}

// resolve_forward_const_refs_from_flat must resolve a forward reference
// (const A = B where B is declared after A) identically to the legacy
// walker.
fn test_resolve_forward_const_refs_from_flat_matches_legacy_for_forward_ref() {
	files := make_forward_const_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('rfcr_fwd_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_consts_and_globals(files[0])
	// Before the resolver: A is 0 (forward ref), B is 5.
	assert b_legacy.const_values['b'] == 5
	b_legacy.resolve_forward_const_refs(files)

	mut mod_flat := Module.new('rfcr_fwd_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_consts_and_globals_from_flat(flat.file_cursor(0))
	assert b_flat.const_values['b'] == 5
	b_flat.resolve_forward_const_refs_from_flat(flat)

	// After the resolver: both paths agree A == B == 5.
	assert 'a' in b_legacy.const_values
	assert 'a' in b_flat.const_values
	assert b_legacy.const_values['a'] == b_flat.const_values['a']
	assert b_legacy.const_values['a'] == 5
	assert b_legacy.const_values['b'] == b_flat.const_values['b']
	assert b_legacy.const_values.len == b_flat.const_values.len
}
