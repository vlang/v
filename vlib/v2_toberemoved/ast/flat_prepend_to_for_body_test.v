// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for `FlatBuilder.prepend_to_for_body`. The ForStmt analog of s154's
// `prepend_to_fn_body`. Body-stmt encoding differs: ForStmt encodes body
// stmts as INLINE edges starting at index 3 (no list child), so this
// primitive rebuilds the for's edge list directly rather than rebuilding a
// stmts-list child node.

fn make_for_body_assert_stmt(value string) Stmt {
	return Stmt(AssertStmt{
		expr: Expr(BasicLiteral{
			kind:  .number
			value: value
		})
	})
}

// Build a stmt_for via the legacy add_stmt path so the test exercises the
// same encoding that real callsites produce.
fn build_for_stmt(body_stmts []Stmt) (FlatBuilder, FlatNodeId) {
	mut b := new_flat_builder()
	for_stmt := Stmt(ForStmt{
		init:  Stmt(EmptyStmt{})
		cond:  Expr(BasicLiteral{
			kind:  .number
			value: '1'
		})
		stmts: body_stmts
	})
	for_id := b.emit_stmt(for_stmt)
	return b, for_id
}

fn ref_sig_for_body(body_stmts []Stmt) string {
	mut b, for_id := build_for_stmt(body_stmts)
	return b.flat.subtree_signature(for_id)
}

fn sub_sig_for_prepend(initial_body []Stmt, extra_values []string) string {
	mut b, for_id := build_for_stmt(initial_body)
	mut extra_ids := []FlatNodeId{}
	for v in extra_values {
		extra_ids << b.emit_stmt(make_for_body_assert_stmt(v))
	}
	new_for_id := b.prepend_to_for_body(for_id, extra_ids)
	return b.flat.subtree_signature(new_for_id)
}

fn test_prepend_to_for_body_signature_matches_reference() {
	ref_sig := ref_sig_for_body([make_for_body_assert_stmt('99'),
		make_for_body_assert_stmt('1')])
	sub_sig := sub_sig_for_prepend([make_for_body_assert_stmt('1')], ['99'])
	assert ref_sig == sub_sig
}

fn test_prepend_to_for_body_zero_extras_returns_existing_id() {
	mut b, for_id := build_for_stmt([make_for_body_assert_stmt('1')])
	returned_id := b.prepend_to_for_body(for_id, [])
	assert returned_id == for_id
}

fn test_prepend_to_for_body_invalid_id_returns_invalid() {
	mut b := new_flat_builder()
	extra_id := b.emit_stmt(make_for_body_assert_stmt('99'))
	returned_id := b.prepend_to_for_body(FlatNodeId(-1), [extra_id])
	assert returned_id == invalid_flat_node_id
}

fn test_prepend_to_for_body_non_for_kind_returns_invalid() {
	mut b := new_flat_builder()
	// stmt_assert is not stmt_for — kind check must reject it.
	assert_id := b.emit_stmt(make_for_body_assert_stmt('1'))
	extra_id := b.emit_stmt(make_for_body_assert_stmt('99'))
	returned_id := b.prepend_to_for_body(assert_id, [extra_id])
	assert returned_id == invalid_flat_node_id
}

fn test_prepend_to_for_body_empty_original_body() {
	// ForStmt with no body stmts at all — extras become the entire new body.
	ref_sig := ref_sig_for_body([make_for_body_assert_stmt('99')])
	sub_sig := sub_sig_for_prepend([]Stmt{}, ['99'])
	assert ref_sig == sub_sig
}

fn test_prepend_to_for_body_preserves_existing_body_order() {
	// Two existing body stmts + one prepended; order must be [extra, b1, b2].
	ref_sig := ref_sig_for_body([make_for_body_assert_stmt('99'),
		make_for_body_assert_stmt('1'), make_for_body_assert_stmt('2')])
	sub_sig := sub_sig_for_prepend([make_for_body_assert_stmt('1'),
		make_for_body_assert_stmt('2')], ['99'])
	assert ref_sig == sub_sig
}
