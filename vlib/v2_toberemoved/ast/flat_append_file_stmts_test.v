// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for `FlatBuilder.append_file_stmts`. The helper is the foundational
// primitive behind the transformer's upcoming `post_pass_to_flat` port —
// it appends pre-emitted stmt nodes to a registered file's stmt list and
// re-emits the file root, mirroring the post-pass mutation pattern used by
// `inject_embed_file_helper`, `inject_test_main`, and `generated_fns_parts`.

fn make_minimal_file_with_stmts(extra_stmts []Stmt) File {
	mut stmts := []Stmt{cap: 1 + extra_stmts.len}
	stmts << Stmt(ModuleStmt{
		name: 'foo'
	})
	for s in extra_stmts {
		stmts << s
	}
	return File{
		name:  'inline_minimal.v'
		mod:   'foo'
		stmts: stmts
	}
}

fn make_extra_stmt_a() Stmt {
	return Stmt(ConstDecl{
		fields: [
			FieldInit{
				name:  'A'
				value: Expr(BasicLiteral{
					kind:  .number
					value: '1'
				})
			},
		]
	})
}

fn make_extra_stmt_b() Stmt {
	return Stmt(ConstDecl{
		fields: [
			FieldInit{
				name:  'B'
				value: Expr(BasicLiteral{
					kind:  .number
					value: '2'
				})
			},
		]
	})
}

// Reference: build a file directly with both extras at construction time.
fn build_reference_with_extras() FlatAst {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_stmts([make_extra_stmt_a(),
		make_extra_stmt_b()]))
	return b.flat
}

// Subject: build the bare file, emit extras separately into the same builder,
// then splice them in via append_file_stmts.
fn build_subject_with_appended_extras() FlatAst {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_stmts([]))
	a_id := b.emit_stmt(make_extra_stmt_a())
	b_id := b.emit_stmt(make_extra_stmt_b())
	b.append_file_stmts(0, [a_id, b_id])
	return b.flat
}

fn test_append_file_stmts_signature_matches_reference() {
	ref_sig := build_reference_with_extras().signature()
	sub_sig := build_subject_with_appended_extras().signature()
	assert ref_sig == sub_sig
}

fn test_append_file_stmts_zero_extras_returns_existing_root() {
	mut b := new_flat_builder()
	original_id := b.append_file(make_minimal_file_with_stmts([]))
	returned_id := b.append_file_stmts(0, [])
	assert returned_id == original_id
}

fn test_append_file_stmts_invalid_idx_returns_invalid() {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_stmts([]))
	returned_id := b.append_file_stmts(-1, [FlatNodeId(0)])
	assert returned_id == invalid_flat_node_id
}
