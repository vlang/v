// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

// Tests for `FlatBuilder.replace_fn_body_stmts`. The companion primitive to
// `prepend_to_fn_body`: replaces the FnDecl's body entirely (vs prepending
// to it). Needed by `inject_live_reload` which builds a fully-rewritten
// FnDecl body in one shot (preamble + rewritten for-stmts + rewritten calls).

fn make_assert_stmt_value(value string) Stmt {
	return Stmt(AssertStmt{
		expr: Expr(BasicLiteral{
			kind:  .number
			value: value
		})
	})
}

fn build_fn_decl(body_stmts []Stmt) (FlatBuilder, FlatNodeId) {
	mut b := new_flat_builder()
	fn_stmt := Stmt(FnDecl{
		name:  'f'
		stmts: body_stmts
	})
	fn_id := b.emit_stmt(fn_stmt)
	return b, fn_id
}

fn ref_sig_for_fn_body(body_stmts []Stmt) string {
	mut b, fn_id := build_fn_decl(body_stmts)
	return b.flat.subtree_signature(fn_id)
}

fn sub_sig_for_replace(initial_body []Stmt, new_body_values []string) string {
	mut b, fn_id := build_fn_decl(initial_body)
	mut new_ids := []FlatNodeId{}
	for v in new_body_values {
		new_ids << b.emit_stmt(make_assert_stmt_value(v))
	}
	new_fn_id := b.replace_fn_body_stmts(fn_id, new_ids)
	return b.flat.subtree_signature(new_fn_id)
}

fn test_replace_fn_body_stmts_signature_matches_reference() {
	ref_sig := ref_sig_for_fn_body([make_assert_stmt_value('7'),
		make_assert_stmt_value('8'), make_assert_stmt_value('9')])
	sub_sig := sub_sig_for_replace([make_assert_stmt_value('1'),
		make_assert_stmt_value('2')], ['7', '8', '9'])
	assert ref_sig == sub_sig
}

fn test_replace_fn_body_stmts_empty_new_body() {
	// Replacing with an empty body — the fn becomes bodyless.
	ref_sig := ref_sig_for_fn_body([]Stmt{})
	sub_sig := sub_sig_for_replace([make_assert_stmt_value('1')], []string{})
	assert ref_sig == sub_sig
}

fn test_replace_fn_body_stmts_invalid_id_returns_invalid() {
	mut b := new_flat_builder()
	x_id := b.emit_stmt(make_assert_stmt_value('1'))
	returned_id := b.replace_fn_body_stmts(FlatNodeId(-1), [x_id])
	assert returned_id == invalid_flat_node_id
}

fn test_replace_fn_body_stmts_non_fn_kind_returns_invalid() {
	mut b := new_flat_builder()
	// stmt_assert, not stmt_fn_decl — kind check must reject it.
	assert_id := b.emit_stmt(make_assert_stmt_value('1'))
	x_id := b.emit_stmt(make_assert_stmt_value('2'))
	returned_id := b.replace_fn_body_stmts(assert_id, [x_id])
	assert returned_id == invalid_flat_node_id
}

fn test_replace_fn_body_stmts_returns_new_id() {
	mut b, fn_id := build_fn_decl([make_assert_stmt_value('1')])
	x_id := b.emit_stmt(make_assert_stmt_value('2'))
	new_fn_id := b.replace_fn_body_stmts(fn_id, [x_id])
	// New id must differ from the original — the old fn is unreachable garbage.
	assert new_fn_id != fn_id
	assert new_fn_id != invalid_flat_node_id
}
