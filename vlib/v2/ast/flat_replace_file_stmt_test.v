// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for `FlatBuilder.replace_file_stmt`. The companion primitive to
// `prepend_to_fn_body`: when a caller swaps a stmt for a re-emitted variant
// (e.g. the new FnDecl returned by `prepend_to_fn_body`), `replace_file_stmt`
// rebuilds the enclosing file root so the file's stmts list references the
// new id. Together they support the prepend-style post_pass mutations
// (`inject_main_runtime_const_init_calls`, s155).

fn make_minimal_file_with_three_consts() File {
	return File{
		name:  'inline.v'
		mod:   'foo'
		stmts: [
			Stmt(ModuleStmt{
				name: 'foo'
			}),
			Stmt(ConstDecl{
				fields: [
					FieldInit{
						name:  'A'
						value: Expr(BasicLiteral{
							kind:  .number
							value: '1'
						})
					},
				]
			}),
			Stmt(ConstDecl{
				fields: [
					FieldInit{
						name:  'B'
						value: Expr(BasicLiteral{
							kind:  .number
							value: '2'
						})
					},
				]
			}),
			Stmt(ConstDecl{
				fields: [
					FieldInit{
						name:  'C'
						value: Expr(BasicLiteral{
							kind:  .number
							value: '3'
						})
					},
				]
			}),
		]
	}
}

fn make_replacement_const_stmt() Stmt {
	return Stmt(ConstDecl{
		fields: [
			FieldInit{
				name:  'Z'
				value: Expr(BasicLiteral{
					kind:  .number
					value: '99'
				})
			},
		]
	})
}

fn make_const_stmt_named(name string, value string) Stmt {
	return Stmt(ConstDecl{
		fields: [
			FieldInit{
				name:  name
				value: Expr(BasicLiteral{
					kind:  .number
					value: value
				})
			},
		]
	})
}

// Reference: build the file with stmt index 2 (the `B` const) already
// replaced by the `Z` const at construction time.
fn build_reference_with_replacement() FlatAst {
	mut b := new_flat_builder()
	b.append_file(File{
		name:  'inline.v'
		mod:   'foo'
		stmts: [
			Stmt(ModuleStmt{
				name: 'foo'
			}),
			make_const_stmt_named('A', '1'),
			make_replacement_const_stmt(),
			make_const_stmt_named('C', '3'),
		]
	})
	return b.flat
}

// Subject: build the bare three-const file, emit the replacement stmt
// separately, then splice it in via `replace_file_stmt(0, 2, z_id)`.
fn build_subject_with_replacement() FlatAst {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_three_consts())
	z_id := b.emit_stmt(make_replacement_const_stmt())
	b.replace_file_stmt(0, 2, z_id)
	return b.flat
}

fn test_replace_file_stmt_signature_matches_reference() {
	ref_sig := build_reference_with_replacement().signature()
	sub_sig := build_subject_with_replacement().signature()
	assert ref_sig == sub_sig
}

fn test_replace_file_stmt_invalid_file_idx_returns_invalid() {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_three_consts())
	z_id := b.emit_stmt(make_replacement_const_stmt())
	returned_id := b.replace_file_stmt(-1, 0, z_id)
	assert returned_id == invalid_flat_node_id
}

fn test_replace_file_stmt_invalid_stmt_idx_returns_invalid() {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_three_consts())
	z_id := b.emit_stmt(make_replacement_const_stmt())
	// File has 4 stmts (module + 3 consts); idx 99 is out of range.
	returned_id := b.replace_file_stmt(0, 99, z_id)
	assert returned_id == invalid_flat_node_id
}

fn test_replace_file_stmt_invalid_new_id_returns_invalid() {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_three_consts())
	returned_id := b.replace_file_stmt(0, 0, FlatNodeId(-1))
	assert returned_id == invalid_flat_node_id
}

fn test_replace_file_stmt_updates_files_table() {
	mut b := new_flat_builder()
	original_file_id := b.append_file(make_minimal_file_with_three_consts())
	z_id := b.emit_stmt(make_replacement_const_stmt())
	new_file_id := b.replace_file_stmt(0, 1, z_id)
	// The flat.files[0].file_id must point at the new file root, not the old.
	assert new_file_id != original_file_id
	assert b.flat.files[0].file_id == new_file_id
}
