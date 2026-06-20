// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
module ssa

import v2.ast
import v2.types

fn make_forward_alias_fixture() []ast.File {
	return [
		ast.File{
			name:  'main.v'
			mod:   'main'
			stmts: [
				ast.Stmt(ast.TypeDecl{
					name:      'Alias'
					base_type: ast.Expr(ast.Ident{
						name: 'Later'
					})
				}),
				ast.Stmt(ast.StructDecl{
					name: 'Later'
				}),
				ast.Stmt(ast.StructDecl{
					name:   'Holder'
					fields: [
						ast.FieldDecl{
							name: 'value'
							typ:  ast.Expr(ast.Ident{
								name: 'Alias'
							})
						},
					]
				}),
			]
		},
	]
}

fn test_register_type_aliases_resolves_forward_struct_alias_after_pass1() {
	files := make_forward_alias_fixture()
	flat := ast.flatten_files(files)
	env := types.Environment.new()

	mut mod_legacy := Module.new('alias_forward_legacy')
	mut b_legacy := Builder.new_with_env(mod_legacy, env)
	b_legacy.register_types_pass1(files[0])
	b_legacy.register_type_aliases(files[0])
	b_legacy.register_types_pass2(files[0])

	mut mod_flat := Module.new('alias_forward_flat')
	mut b_flat := Builder.new_with_env(mod_flat, env)
	b_flat.register_types_pass1_from_flat(flat.file_cursor(0))
	b_flat.register_type_aliases_from_flat(flat.file_cursor(0))
	b_flat.register_types_pass2_from_flat(flat.file_cursor(0))

	later_legacy := b_legacy.struct_types['Later']
	later_flat := b_flat.struct_types['Later']
	assert b_legacy.type_aliases['Alias'] == later_legacy
	assert b_flat.type_aliases['Alias'] == later_flat

	holder_legacy := mod_legacy.type_store.types[b_legacy.struct_types['Holder']]
	holder_flat := mod_flat.type_store.types[b_flat.struct_types['Holder']]
	assert holder_legacy.fields.len == 1
	assert holder_flat.fields.len == 1
	assert holder_legacy.fields[0] == later_legacy
	assert holder_flat.fields[0] == later_flat
}
