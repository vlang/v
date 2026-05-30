// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for `FlatBuilder.prepend_file_stmts`. Sibling of `append_file_stmts`
// (s150) — same shape, opposite order. Needed by `inject_live_reload`'s
// file-level prepend where c_decls + global_decls go BEFORE the file's
// existing stmts.

fn make_minimal_file_with_stmts_prepend(extra_stmts []Stmt) File {
	mut stmts := []Stmt{cap: 1 + extra_stmts.len}
	for s in extra_stmts {
		stmts << s
	}
	stmts << Stmt(ModuleStmt{
		name: 'foo'
	})
	stmts << Stmt(ConstDecl{
		fields: [
			FieldInit{
				name:  'TAIL'
				value: Expr(BasicLiteral{
					kind:  .number
					value: '0'
				})
			},
		]
	})
	return File{
		name:  'inline_prepend.v'
		mod:   'foo'
		stmts: stmts
	}
}

fn make_prepend_const_a() Stmt {
	return Stmt(ConstDecl{
		fields: [
			FieldInit{
				name:  'PA'
				value: Expr(BasicLiteral{
					kind:  .number
					value: '11'
				})
			},
		]
	})
}

fn make_prepend_const_b() Stmt {
	return Stmt(ConstDecl{
		fields: [
			FieldInit{
				name:  'PB'
				value: Expr(BasicLiteral{
					kind:  .number
					value: '22'
				})
			},
		]
	})
}

// Reference: construct the file with both prepended consts BEFORE the module
// stmt at append time.
//
// Why this layout: prepend_file_stmts puts new stmts at the head of the
// stmts list — even before the ModuleStmt — so the reference must mirror
// that ordering at construction time to produce a bit-equal signature.
//
// Why subtree-of-stmts-list instead of full file signature: the file root's
// `extra` slot stores `intern(mod)` as a raw index. When the reference path
// emits the file's stmts in order, the prepended consts intern their strings
// BEFORE 'foo' the module name → 'foo' lands at a later intern slot. The
// subject path interns 'foo' first (via the bare file's ModuleStmt) → 'foo'
// at slot 0. Comparing the stmts-list subtree sidesteps this leak: it covers
// the actual prepended stmts ordering + content (what the primitive controls)
// without including the file root's leaky `extra` field. The s155 multi-file
// test uses the same subtree_signature workaround for the same reason.
fn build_reference_stmts_list_signature() string {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_stmts_prepend([make_prepend_const_a(),
		make_prepend_const_b()]))
	stmts_list_id := b.flat.child_at(b.flat.files[0].file_id, 2)
	return b.flat.subtree_signature(stmts_list_id)
}

// Subject: build the bare file, emit prepended stmts separately, then splice
// them via prepend_file_stmts.
fn build_subject_stmts_list_signature() string {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_stmts_prepend([]))
	a_id := b.emit_stmt(make_prepend_const_a())
	c_id := b.emit_stmt(make_prepend_const_b())
	b.prepend_file_stmts(0, [a_id, c_id])
	stmts_list_id := b.flat.child_at(b.flat.files[0].file_id, 2)
	return b.flat.subtree_signature(stmts_list_id)
}

fn test_prepend_file_stmts_signature_matches_reference() {
	ref_sig := build_reference_stmts_list_signature()
	sub_sig := build_subject_stmts_list_signature()
	assert ref_sig == sub_sig
}

fn test_prepend_file_stmts_zero_prepended_returns_existing_root() {
	mut b := new_flat_builder()
	original_id := b.append_file(make_minimal_file_with_stmts_prepend([]))
	returned_id := b.prepend_file_stmts(0, [])
	assert returned_id == original_id
}

fn test_prepend_file_stmts_invalid_idx_returns_invalid() {
	mut b := new_flat_builder()
	b.append_file(make_minimal_file_with_stmts_prepend([]))
	returned_id := b.prepend_file_stmts(-1, [FlatNodeId(0)])
	assert returned_id == invalid_flat_node_id
}

fn test_prepend_file_stmts_updates_files_table() {
	mut b := new_flat_builder()
	original_file_id := b.append_file(make_minimal_file_with_stmts_prepend([]))
	x_id := b.emit_stmt(make_prepend_const_a())
	new_file_id := b.prepend_file_stmts(0, [x_id])
	assert new_file_id != original_file_id
	assert b.flat.files[0].file_id == new_file_id
}
