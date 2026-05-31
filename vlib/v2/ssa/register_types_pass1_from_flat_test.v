// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s169: `register_types_pass1_from_flat` (flat-cursor
// port) must register the same struct/enum/sumtype names as the legacy
// `register_types_pass1`. The flat port walks one file's top-level stmts via
// FileCursor and only rehydrates StructDecl/EnumDecl/TypeDecl nodes via
// `flat.decode_stmt`. ModuleStmt/FnDecl/ConstDecl etc. are never decoded.
module ssa

import v2.ast
import v2.types

fn make_pass1_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.ModuleStmt{
					name: 'main'
				}),
				ast.Stmt(ast.StructDecl{
					name: 'Point'
				}),
				ast.Stmt(ast.EnumDecl{
					name: 'Color'
				}),
				ast.Stmt(ast.StructDecl{
					name: 'Line'
				}),
			]
		},
	]
}

// register_types_pass1_from_flat on an empty file (only ModuleStmt) must
// produce zero new entries — same as register_types_pass1.
fn test_register_types_pass1_from_flat_empty_file_matches_legacy() {
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

	mut mod_legacy := Module.new('pass1_empty_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_types_pass1(files[0])

	mut mod_flat := Module.new('pass1_empty_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_types_pass1_from_flat(flat.file_cursor(0))

	assert b_legacy.struct_types.len == b_flat.struct_types.len
	assert b_legacy.enum_values.len == b_flat.enum_values.len
	assert mod_legacy.type_store.types.len == mod_flat.type_store.types.len
}

// register_types_pass1_from_flat on a file with two structs + one enum must
// register both names in struct_types and the enum's fields in enum_values,
// matching the legacy walker bit-for-bit.
fn test_register_types_pass1_from_flat_matches_legacy_for_struct_enum_mix() {
	files := make_pass1_fixture()
	flat := ast.flatten_files(files)

	env := types.Environment.new()

	mut mod_legacy := Module.new('pass1_mix_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_types_pass1(files[0])

	mut mod_flat := Module.new('pass1_mix_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_types_pass1_from_flat(flat.file_cursor(0))

	// Both must have registered Point and Line as struct names.
	assert b_legacy.struct_types.len == b_flat.struct_types.len
	assert 'Point' in b_legacy.struct_types
	assert 'Line' in b_legacy.struct_types
	assert 'Point' in b_flat.struct_types
	assert 'Line' in b_flat.struct_types
	// struct type ids should also match because both paths register them in
	// the same order against an isolated Module/TypeStore.
	assert b_legacy.struct_types['Point'] == b_flat.struct_types['Point']
	assert b_legacy.struct_types['Line'] == b_flat.struct_types['Line']
	// Type store should have grown by the same count.
	assert mod_legacy.type_store.types.len == mod_flat.type_store.types.len
}
