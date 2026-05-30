// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for `FlatBuilder.prepend_to_fn_body`. The helper is the seed primitive
// for the remaining prepend-style post_pass mutations
// (`inject_main_runtime_const_init_calls`, `inject_live_reload`): it re-emits
// a stmt_fn_decl with extras prepended to its body, returning the new
// FlatNodeId so callers can rewire the surrounding file/stmt list.
//
// We compare via `subtree_signature(fn_id)` rather than the whole-file
// `signature()` because the file root's `extra=intern(mod)` slot would leak
// intern order between the reference (which builds extras before file mod
// gets re-interned) and subject (which interns extras AFTER file mod) paths.

fn make_fn_decl_with_body(name string, body []Stmt) Stmt {
	return Stmt(FnDecl{
		name:  name
		stmts: body
	})
}

fn make_const_stmt(name string, value string) Stmt {
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

// Reference: emit a FnDecl whose body is already `[extras + old]`.
fn build_reference_fn_with_prepended() (FlatAst, FlatNodeId) {
	mut b := new_flat_builder()
	fn_id := b.emit_stmt(make_fn_decl_with_body('foo', [
		make_const_stmt('A', '1'),
		make_const_stmt('B', '2'),
		make_const_stmt('C', '3'),
	]))
	return b.flat, fn_id
}

// Subject: emit a FnDecl whose body is `[old]`, emit extras separately,
// then prepend via the primitive under test.
fn build_subject_fn_with_prepended() (FlatAst, FlatNodeId) {
	mut b := new_flat_builder()
	base_fn_id := b.emit_stmt(make_fn_decl_with_body('foo', [
		make_const_stmt('C', '3'),
	]))
	a_id := b.emit_stmt(make_const_stmt('A', '1'))
	b_id := b.emit_stmt(make_const_stmt('B', '2'))
	new_fn_id := b.prepend_to_fn_body(base_fn_id, [a_id, b_id])
	return b.flat, new_fn_id
}

fn test_prepend_to_fn_body_signature_matches_reference() {
	ref_flat, ref_fn_id := build_reference_fn_with_prepended()
	sub_flat, sub_fn_id := build_subject_fn_with_prepended()
	ref_sig := ref_flat.subtree_signature(ref_fn_id)
	sub_sig := sub_flat.subtree_signature(sub_fn_id)
	assert ref_sig == sub_sig
}

fn test_prepend_to_fn_body_zero_extras_returns_existing_id() {
	mut b := new_flat_builder()
	original_id := b.emit_stmt(make_fn_decl_with_body('foo', [
		make_const_stmt('C', '3'),
	]))
	returned_id := b.prepend_to_fn_body(original_id, [])
	assert returned_id == original_id
}

fn test_prepend_to_fn_body_invalid_id_returns_invalid() {
	mut b := new_flat_builder()
	a_id := b.emit_stmt(make_const_stmt('A', '1'))
	returned_id := b.prepend_to_fn_body(FlatNodeId(-1), [a_id])
	assert returned_id == invalid_flat_node_id
}

fn test_prepend_to_fn_body_non_fn_decl_returns_invalid() {
	// Pass a non-FnDecl node id (a ConstDecl) — primitive must refuse.
	mut b := new_flat_builder()
	const_id := b.emit_stmt(make_const_stmt('C', '3'))
	extra_id := b.emit_stmt(make_const_stmt('A', '1'))
	returned_id := b.prepend_to_fn_body(const_id, [extra_id])
	assert returned_id == invalid_flat_node_id
}

fn test_prepend_to_fn_body_empty_original_body() {
	// FnDecl with no original body stmts; prepending should still produce a
	// new FnDecl whose body is exactly the extras.
	ref_flat, ref_fn_id := fn () (FlatAst, FlatNodeId) {
		mut b := new_flat_builder()
		fn_id := b.emit_stmt(make_fn_decl_with_body('foo', [make_const_stmt('A', '1')]))
		return b.flat, fn_id
	}()

	mut sb := new_flat_builder()
	base_fn_id := sb.emit_stmt(make_fn_decl_with_body('foo', []))
	a_id := sb.emit_stmt(make_const_stmt('A', '1'))
	new_fn_id := sb.prepend_to_fn_body(base_fn_id, [a_id])

	assert ref_flat.subtree_signature(ref_fn_id) == sb.flat.subtree_signature(new_fn_id)
}
