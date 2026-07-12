// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s170: `register_types_pass2_from_flat` (flat-cursor
// port) must fill in the same struct field types as the legacy
// `register_types_pass2`. The flat port walks one file's top-level stmts
// via FileCursor and only rehydrates `.stmt_struct_decl` nodes via
// `flat.decode_stmt`. Non-StructDecl stmts are never decoded.
module ssa

import v2.ast
import v2.types

fn make_pass2_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.StructDecl{
					name:   'Point'
					fields: [
						ast.FieldDecl{
							name: 'x'
							typ:  ast.Expr(ast.Ident{
								name: 'int'
							})
						},
						ast.FieldDecl{
							name: 'y'
							typ:  ast.Expr(ast.Ident{
								name: 'int'
							})
						},
					]
				}),
				ast.Stmt(ast.EnumDecl{
					name: 'Color'
				}),
				ast.Stmt(ast.StructDecl{
					name: 'Empty'
				}),
			]
		},
	]
}

// register_types_pass2_from_flat on a file with no StructDecls must not
// change the type_store — same as register_types_pass2.
fn test_register_types_pass2_from_flat_no_structs_matches_legacy() {
	files := [
		ast.File{
			name:  'empty.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.EnumDecl{
					name: 'Color'
				}),
			]
		},
	]
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('pass2_no_structs_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_types_pass2(files[0])

	mut mod_flat := Module.new('pass2_no_structs_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_types_pass2_from_flat(flat.file_cursor(0))

	// No struct decls -> neither path touches struct_types or type_store.
	assert mod_legacy.type_store.types.len == mod_flat.type_store.types.len
	assert b_legacy.struct_types.len == b_flat.struct_types.len
}

// register_types_pass2_from_flat must fill in fields for previously-
// forward-declared structs identically to the legacy walker.
fn test_register_types_pass2_from_flat_matches_legacy_for_struct_fields() {
	files := make_pass2_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('pass2_fields_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	// Pass1 forward-declares struct names so pass2 can fill fields.
	b_legacy.register_types_pass1(files[0])
	b_legacy.register_types_pass2(files[0])

	mut mod_flat := Module.new('pass2_fields_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_types_pass1_from_flat(flat.file_cursor(0))
	b_flat.register_types_pass2_from_flat(flat.file_cursor(0))

	// Both paths registered Point + Empty in pass1.
	assert 'Point' in b_legacy.struct_types
	assert 'Point' in b_flat.struct_types
	assert 'Empty' in b_legacy.struct_types
	assert 'Empty' in b_flat.struct_types
	// type_ids match because identical registration order on isolated Modules.
	assert b_legacy.struct_types['Point'] == b_flat.struct_types['Point']
	// Pass2 filled in Point's fields on both paths.
	point_tid_legacy := b_legacy.struct_types['Point']
	point_tid_flat := b_flat.struct_types['Point']
	point_legacy := mod_legacy.type_store.types[point_tid_legacy]
	point_flat := mod_flat.type_store.types[point_tid_flat]
	assert point_legacy.fields.len == point_flat.fields.len
	assert point_legacy.field_names.len == point_flat.field_names.len
	assert point_legacy.field_names == point_flat.field_names
	assert point_legacy.fields == point_flat.fields
	// Empty has zero fields on both paths.
	empty_tid_legacy := b_legacy.struct_types['Empty']
	empty_tid_flat := b_flat.struct_types['Empty']
	assert mod_legacy.type_store.types[empty_tid_legacy].fields.len == 0
	assert mod_flat.type_store.types[empty_tid_flat].fields.len == 0
}
