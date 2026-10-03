module types

import os
import v.flat
import v.parser
import v.pref

fn snapshot_restore_children(mut a flat.FlatAst, kind flat.NodeKind, children []flat.NodeId) flat.NodeId {
	start := a.begin_children()
	for child in children {
		a.add_child(child)
	}
	return a.add_node(flat.Node{
		kind:           kind
		children_start: start
		children_count: children.len
	})
}

fn snapshot_restore_condition(mut a flat.FlatAst, name string, pattern string) flat.NodeId {
	subject := a.add_val(.ident, name)
	condition := snapshot_restore_children(mut a, .is_expr, [subject])
	a.nodes[int(condition)].value = pattern
	return condition
}

fn snapshot_restore_assertion(mut a flat.FlatAst, name string, pattern string) flat.NodeId {
	condition := snapshot_restore_condition(mut a, name, pattern)
	return snapshot_restore_children(mut a, .assert_stmt, [condition])
}

fn snapshot_restore_seed(mut tc TypeChecker) string {
	tc.cur_scope = new_scope(tc.file_scope)
	tc.sum_types['Value'] = ['int', 'string']
	tc.cur_scope.insert('value', Type(SumType{ name: 'Value' }))
	tc.cur_scope.insert('other', Type(SumType{ name: 'Value' }))
	tc.cur_scope.insert('branch_value', Type(SumType{ name: 'Value' }))
	tc.cur_scope.insert('kept', builtin_string_type)
	tc.smartcasts['kept'] = builtin_string_type
	owner := tc.cur_scope.insert_with_owner('outer', builtin_int_type)
	tc.fn_context.mut_local_owners['outer'] = owner
	tc.fn_context.return_type = builtin_void_type
	return owner.storage_key()
}

fn snapshot_restore_assert_state(tc &TypeChecker, outer_key string) {
	assert tc.smartcasts.len == 1, tc.smartcasts.str()
	assert tc.smartcasts['kept'] or { panic('lost outer smartcast') } == builtin_string_type
	assert tc.fn_context.mut_local_owners.len == 1
	owner := tc.fn_context.mut_local_owners['outer'] or { panic('lost mutable outer binding') }
	assert owner.storage_key() == outer_key
	assert owner.belongs_to_scope(tc.cur_scope)
}

fn test_logical_condition_snapshot_restores_outer_narrowing() {
	for fast in [false, true] {
		for op in [flat.Op.logical_and, .logical_or] {
			mut a := flat.FlatAst.new()
			lhs := snapshot_restore_condition(mut a, 'value', 'int')
			rhs := snapshot_restore_condition(mut a, 'other', 'string')
			condition := snapshot_restore_children(mut a, .infix, [lhs, rhs])
			a.nodes[int(condition)].op = op
			mut tc := TypeChecker.new(&a)
			outer_key := snapshot_restore_seed(mut tc)
			tc.valid_resolution_fast = fast
			for _ in 0 .. 8 {
				assert tc.check_condition(condition).len == 0
				snapshot_restore_assert_state(&tc, outer_key)
				assert tc.resolve_type(condition) == builtin_bool_type
				// The restored map must remain independently writable after the call.
				tc.smartcasts['after'] = builtin_int_type
				tc.smartcasts.delete('after')
			}
			assert tc.errors.len == 0, tc.errors.str()
		}
	}
}

fn test_nested_if_match_and_sequence_snapshots_keep_outer_state() {
	for fast in [false, true] {
		mut a := flat.FlatAst.new()
		pattern_int := a.add_val(.ident, 'int')
		pattern_string := a.add_val(.ident, 'string')
		branch_assert := snapshot_restore_assertion(mut a, 'branch_value', 'int')
		multi_branch := snapshot_restore_children(mut a, .match_branch, [pattern_int, pattern_string,
			branch_assert])
		a.nodes[int(multi_branch)].value = '2'
		match_subject := a.add_val(.ident, 'other')
		match_id := snapshot_restore_children(mut a, .match_stmt, [match_subject, multi_branch])
		then_tail := a.add_val(.int_literal, '1')
		then_body := snapshot_restore_children(mut a, .block, [match_id, then_tail])
		else_assert := snapshot_restore_assertion(mut a, 'other', 'string')
		else_tail := a.add_val(.int_literal, '2')
		else_body := snapshot_restore_children(mut a, .block, [else_assert, else_tail])
		condition := snapshot_restore_condition(mut a, 'value', 'int')
		if_id := snapshot_restore_children(mut a, .if_expr, [condition, then_body, else_body])
		// A return parent makes this an if value, exercising the final tail-typing restore.
		snapshot_restore_children(mut a, .return_stmt, [if_id])
		sequence_assert := snapshot_restore_assertion(mut a, 'other', 'int')
		return_id := a.add(.return_stmt)
		sequence := snapshot_restore_children(mut a, .block, [sequence_assert, return_id])
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		outer_key := snapshot_restore_seed(mut tc)
		tc.valid_resolution_fast = fast
		tc.mark_statement_context(match_id)
		for _ in 0 .. 8 {
			tc.check_if_expr(if_id, *a.node(if_id))
			snapshot_restore_assert_state(&tc, outer_key)
			// Check the multipattern match without the outer if's int narrowing too.
			tc.check_match_stmt(match_id, *a.node(match_id))
			snapshot_restore_assert_state(&tc, outer_key)
			tc.check_statement_sequence(*a.node(sequence), 0, false)
			snapshot_restore_assert_state(&tc, outer_key)
			tc.smartcasts['after'] = builtin_int_type
			tc.smartcasts.delete('after')
		}
		assert tc.errors.len == 0, tc.errors.str()
	}
}

fn test_if_guard_snapshot_keeps_mutable_owner_identity_outside_siblings() {
	for fast in [false, true] {
		mut a := flat.FlatAst.new()
		mut_lhs := a.add_val(.ident, 'binding')
		mut_rhs := a.add_val(.ident, 'optional')
		mut_guard := snapshot_restore_children(mut a, .decl_assign, [mut_lhs, mut_rhs])
		a.nodes[int(mut_guard)].op = .assign
		a.nodes[int(mut_guard)].is_mut = true
		plain_lhs := a.add_val(.ident, 'binding')
		plain_rhs := a.add_val(.ident, 'optional')
		plain_guard := snapshot_restore_children(mut a, .decl_assign, [plain_lhs, plain_rhs])
		a.nodes[int(plain_guard)].op = .assign
		plain_body := a.add(.block)
		plain_else := a.add(.block)
		else_if := snapshot_restore_children(mut a, .if_expr, [plain_guard, plain_body, plain_else])
		mut_body := a.add(.block)
		if_id := snapshot_restore_children(mut a, .if_expr, [mut_guard, mut_body, else_if])
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		outer_key := snapshot_restore_seed(mut tc)
		tc.cur_scope.insert('optional', Type(OptionType{ base_type: builtin_int_type }))
		tc.valid_resolution_fast = fast
		tc.mark_statement_context(if_id)
		for _ in 0 .. 8 {
			tc.check_if_expr(if_id, *a.node(if_id))
			snapshot_restore_assert_state(&tc, outer_key)
			assert !tc.cur_scope.contains('binding')
			assert 'binding' !in tc.fn_context.mut_local_owners
			// Assigning through the restored owner table cannot affect its next snapshot.
			tc.fn_context.mut_local_owners['temporary'] = tc.cur_scope.insert_with_owner('temporary',
				builtin_int_type)
			tc.fn_context.mut_local_owners.delete('temporary')
		}
		assert tc.errors.len == 0, tc.errors.str()
	}
}

fn test_contextual_match_snapshot_survives_compatible_and_rejected_tails() {
	mut a := flat.FlatAst.new()
	mut branches := []flat.NodeId{}
	for pattern in ['int', 'string'] {
		pattern_id := a.add_val(.ident, pattern)
		tail := a.add_val(.int_literal, '1')
		branch := snapshot_restore_children(mut a, .match_branch, [pattern_id, tail])
		a.nodes[int(branch)].value = '1'
		branches << branch
	}
	subject := a.add_val(.ident, 'value')
	match_id := snapshot_restore_children(mut a, .match_stmt, [subject, branches[0], branches[1]])
	snapshot_restore_children(mut a, .return_stmt, [match_id])
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	outer_key := snapshot_restore_seed(mut tc)
	for _ in 0 .. 8 {
		assert tc.branches_compatible_with(match_id, builtin_int_type)
		snapshot_restore_assert_state(&tc, outer_key)
		assert !tc.branches_compatible_with(match_id, builtin_bool_type)
		snapshot_restore_assert_state(&tc, outer_key)
		tc.check_match_stmt(match_id, *a.node(match_id))
		snapshot_restore_assert_state(&tc, outer_key)
		tc.smartcasts['after'] = builtin_int_type
		tc.smartcasts.delete('after')
	}
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_branch_snapshot_restoration_preserves_sibling_diagnostics() {
	path := os.join_path(os.vtmp_dir(), 'v3_snapshot_siblings_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	for invalid in [false, true] {
		mutation := if invalid { 'binding++' } else { '_ = binding' }
		os.write_file(path, 'module main
type Value = int | string
fn inspect(value Value, optional ?int) int {
 defer { assert true }
 if mut binding := optional {
  binding++
  if value is int { if binding > 0 { return value } }
 } else if binding := optional {
  ${mutation}
 }
 match value {
  int, string { _ = value }
 }
 return if value is int { value } else { 0 }
}
fn main() { _ = inspect(Value(1), 1) }
')!
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.diagnostic_files[path] = true
		tc.collect(a)
		tc.check_semantics_opt(false)
		if invalid {
			assert tc.errors.any(it.msg.contains('immutable') && it.msg.contains('binding')), tc.errors.str()
		} else {
			assert tc.errors.len == 0, tc.errors.str()
		}
	}
}
