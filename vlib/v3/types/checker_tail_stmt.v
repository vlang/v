module types

import os
import v3.flat
import v3.gen.c.naming
import v3.token
import v3.util

fn (mut tc TypeChecker) check_stmt_node(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	if tc.has_goto_nodes {
		tc.update_pointer_alias_goto_state(id)
	}
	tc.mark_statement_context(id)
	tc.check_node(id)
	if !tc.valid_resolution_fast {
		tc.check_unused_expression_statement(id)
	}
	$if ownership ? {
		tc.ownership_after_stmt_node(id)
	}
}

fn (mut tc TypeChecker) mark_statement_context(id flat.NodeId) {
	mut current := id
	for tc.valid_node_id(current) {
		idx := int(current)
		if tc.parallel_check_sparse {
			if tc.in_check_range(idx) && idx < tc.statement_nodes.len {
				tc.statement_nodes[idx] = true
			} else {
				tc.sparse_statement_nodes[idx] = true
			}
		} else {
			if idx >= tc.statement_nodes.len {
				tc.extend_node_caches(tc.a.nodes.len)
			}
			if idx < tc.statement_nodes.len {
				tc.statement_nodes[idx] = true
			}
		}
		node := tc.a.node(current)
		if node.kind != .expr_stmt || node.children_count != 1 {
			return
		}
		current = tc.a.child(node, 0)
	}
}

fn (tc &TypeChecker) is_statement_node(id flat.NodeId) bool {
	idx := int(id)
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			return idx < tc.statement_nodes.len && tc.statement_nodes[idx]
		}
		return tc.sparse_statement_nodes[idx]
	}
	return idx >= 0 && idx < tc.statement_nodes.len && tc.statement_nodes[idx]
}

fn (tc &TypeChecker) expression_node_used_as_value(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 64 {
		idx := int(current)
		if idx >= 0 && idx < tc.value_used_nodes.len && tc.value_used_nodes[idx] {
			return true
		}
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) || parent_id == current {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr, .comptime_for] {
			return false
		}
		if parent.kind == .expr_stmt {
			current = parent_id
			continue
		}
		if parent.kind in [.block, .match_branch] {
			if tc.branch_tail_expr_id(parent_id) != id {
				return false
			}
			current = parent_id
			continue
		}
		if parent.kind in [.if_expr, .match_stmt, .comptime_if] {
			if parent.children_count == 0 || tc.a.child(parent, 0) == current {
				return false
			}
			current = parent_id
			continue
		}
		return true
	}
	return false
}

@[direct_array_access]
fn (mut tc TypeChecker) check_statement_sequence(node flat.Node, body_start int, value_tail bool) {
	had_saved_smartcasts := tc.smartcasts.len > 0
	saved_smartcasts := if had_saved_smartcasts {
		clone_smartcasts(tc.smartcasts)
	} else {
		tc.smartcasts
	}
	defer {
		if had_saved_smartcasts {
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
		} else {
			tc.smartcasts.clear()
		}
	}
	last_idx := int(node.children_count) - 1
	mut sequence_exited := false
	mut unreachable_id := flat.empty_node
	for i in body_start .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .label_stmt {
			sequence_exited = false
			unreachable_id = flat.empty_node
		} else if sequence_exited && !tc.valid_node_id(unreachable_id) {
			unreachable_id = child_id
		}
		is_value_tail := value_tail && i == last_idx
		if is_value_tail {
			tc.check_node(child_id)
		} else {
			tc.check_stmt_node(child_id)
		}
		tc.apply_post_if_exit_smartcasts(child_id)
		tc.apply_post_assert_smartcasts(child_id)
		$if ownership ? {
			if !is_value_tail {
				tc.ownership_flush_value_branch_moves()
			}
		}
		if tc.statement_exits_sequence(child_id, child) {
			sequence_exited = true
		}
	}
	if tc.valid_node_id(unreachable_id) && tc.should_diagnose(unreachable_id) {
		tc.record_error_at(.return_mismatch, 'unreachable code', unreachable_id,
			tc.unreachable_statement_diagnostic_pos(unreachable_id))
	}
}

fn (mut tc TypeChecker) initialize_pointer_alias_goto_targets() {
	tc.fn_context.pointer_alias_goto_states = map[string][]map[string][]string{}
	tc.fn_context.pointer_alias_backward_goto_targets = map[string]bool{}
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return
	}
	mut label_offsets := map[string]int{}
	mut goto_offsets := map[string][]int{}
	tc.collect_pointer_alias_goto_offsets(fn_id, mut label_offsets, mut goto_offsets)
	for label, offsets in goto_offsets {
		label_offset := label_offsets[label] or { continue }
		if offsets.any(it > label_offset) {
			tc.fn_context.pointer_alias_backward_goto_targets[label] = true
		}
	}
}

fn (tc &TypeChecker) collect_pointer_alias_goto_offsets(id flat.NodeId, mut label_offsets map[string]int, mut goto_offsets map[string][]int) {
	node := tc.a.node(id)
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		child := tc.a.node(child_id)
		if child.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			continue
		}
		if child.kind == .label_stmt {
			label_offsets[child.value] = child.pos.offset
		} else if child.kind == .goto_stmt {
			mut offsets := goto_offsets[child.value] or { []int{} }
			offsets << child.pos.offset
			goto_offsets[child.value] = offsets
		}
		tc.collect_pointer_alias_goto_offsets(child_id, mut label_offsets, mut goto_offsets)
	}
}

fn (mut tc TypeChecker) update_pointer_alias_goto_state(id flat.NodeId) {
	node := tc.a.node(id)
	if node.kind == .goto_stmt {
		mut states := tc.fn_context.pointer_alias_goto_states[node.value] or {
			[]map[string][]string{}
		}
		states << clone_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
		tc.fn_context.pointer_alias_goto_states[node.value] = states
		return
	}
	if node.kind != .label_stmt {
		return
	}
	if tc.fn_context.pointer_alias_backward_goto_targets[node.value] {
		for key, _ in tc.fn_context.pointer_binding_value_keys {
			tc.fn_context.pointer_binding_value_keys[key] = [
				pointer_binding_unknown_value(key),
			]
		}
		return
	}
	incoming := tc.fn_context.pointer_alias_goto_states[node.value] or { return }
	mut paths := [
		clone_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys),
	]
	for state in incoming {
		paths << clone_pointer_binding_value_keys(state)
	}
	tc.fn_context.pointer_binding_value_keys = merge_pointer_binding_value_states(paths,
		tc.fn_context.pointer_binding_value_keys)
	tc.fn_context.pointer_alias_goto_states.delete(node.value)
}

fn (tc &TypeChecker) unreachable_statement_diagnostic_pos(id flat.NodeId) token.Pos {
	node := tc.a.node(id)
	if node.kind == .return_stmt {
		return tc.previous_source_line_matching(node.pos, 'return')
	}
	if node.kind in [.assign, .decl_assign, .selector_assign, .index_assign]
		&& node.children_count >= 2 {
		return tc.assignment_operator_pos(*node, tc.a.child(node, 0), tc.a.child(node, 1))
	}
	if node.kind == .expr_stmt && node.children_count > 0 {
		return tc.a.node(tc.a.child(node, 0)).pos
	}
	return node.pos
}

fn (mut tc TypeChecker) check_unused_expression_statement(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	stmt := tc.a.node(id)
	if stmt.kind != .expr_stmt || stmt.children_count != 1 {
		return
	}
	expr_id := tc.a.child(stmt, 0)
	expr := tc.a.node(expr_id)
	if tc.expr_subtree_has_error(expr_id) {
		return
	}
	if tc.expression_node_used_as_value(expr_id) {
		return
	}
	if tc.expr_is_multi_assignment_tail_value(expr_id) {
		return
	}
	if tc.expr_is_inside_string_interpolation(id) {
		return
	}
	if expr.kind == .empty {
		return
	}
	if expr.kind == .call {
		tc.check_must_use_call(expr_id, expr)
		return
	}
	if expr.kind in [.spawn_expr, .dump_expr, .or_expr, .if_expr, .match_stmt, .lock_expr,
		.select_stmt, .sql_expr, .fn_literal, .lambda_expr] {
		return
	}
	if expr.kind == .selector && expr.children_count > 0 {
		if tc.resolve_type(tc.a.child(expr, 0)) is Void {
			return
		}
	}
	if expr.kind == .postfix && expr.op in [.inc, .dec] {
		return
	}
	if expr.kind == .prefix && expr.op == .arrow {
		if tc.node_is_inside_for_statement(id) {
			return
		}
		tc.record_error_at(.unknown_ident, 'expression evaluated but not used', expr_id, token.new_span(expr.pos.id,
			expr.pos.offset, expr.pos.offset + 2))
		return
	}
	if expr.kind == .infix && expr.op == .arrow {
		return
	}
	if expr.kind == .infix && expr.op == .left_shift && expr.children_count > 0 {
		receiver := unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(expr, 0))))
		if receiver is Array
			|| (receiver is OptionType && unalias_type(receiver.base_type) is Array) {
			return
		}
	}
	if expr.kind == .infix && expr.op in [.left_shift, .right_shift, .right_shift_unsigned] {
		return
	}
	if tc.resolve_type(expr_id) is Void {
		return
	}
	if expr.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal] {
		if tc.unused_literal_has_trailing_token(*stmt, *expr) {
			tc.record_error_at(.unknown_ident, 'expression evaluated but not used', expr_id,
				expr.pos)
		} else {
			tc.record_warning_at(.unknown_ident, 'expression evaluated but not used', expr_id,
				expr.pos)
		}
		return
	}
	if expr.kind == .ident {
		tc.record_error_at(.unknown_ident, '`${expr.value}` evaluated but not used', expr_id,
			expr.pos)
		return
	}
	mut pos := expr.pos
	if expr.kind == .infix && expr.children_count > 0 {
		lhs_id := tc.a.child(expr, 0)
		lhs := tc.a.node(lhs_id)
		if lhs.kind == .selector {
			start := tc.node_value_diagnostic_pos(lhs_id)
			pos = token.new_span(pos.id, start.offset, pos.end)
		}
	}
	tc.record_error_at(.unknown_ident, 'expression evaluated but not used', expr_id, pos)
}

fn (tc &TypeChecker) unused_literal_has_trailing_token(stmt flat.Node, expr flat.Node) bool {
	if stmt.pos.id != expr.pos.id || stmt.pos.end <= expr.pos.end {
		return false
	}
	file := tc.a.source_files[expr.pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	start := int_max(0, int_min(expr.pos.end, source.len))
	end := int_max(start, int_min(stmt.pos.end, source.len))
	for c in source[start..end] {
		if c !in [` `, `\t`, `\r`, `\n`, `;`] {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) expr_is_inside_string_interpolation(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 32 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .string_interp {
			return true
		}
		if parent.kind == .fn_decl {
			return false
		}
		current = parent_id
	}
	return false
}

fn (tc &TypeChecker) expr_is_nested_value_tail(stmt_id flat.NodeId) bool {
	mut current := stmt_id
	mut passed_branch := false
	for _ in 0 .. 32 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		match parent.kind {
			.block, .match_branch {
				if parent.children_count == 0
					|| tc.a.child(parent, parent.children_count - 1) != current {
					return false
				}
			}
			.expr_stmt, .paren {
				if parent.children_count != 1 || tc.a.child(parent, 0) != current {
					return false
				}
			}
			.if_expr, .match_stmt {
				mut is_value_branch := false
				for i in 1 .. parent.children_count {
					if tc.a.child(parent, i) == current {
						is_value_branch = true
						break
					}
				}
				if !is_value_branch {
					return false
				}
				passed_branch = true
			}
			.comptime_if {
				mut is_value_branch := false
				for i in 0 .. parent.children_count {
					if tc.a.child(parent, i) == current {
						is_value_branch = true
						break
					}
				}
				if !is_value_branch {
					return false
				}
				passed_branch = true
			}
			.lock_expr {
				if parent.children_count == 0
					|| tc.a.child(parent, parent.children_count - 1) != current {
					return false
				}
				// The lock body's tail is a value only when the lock expression itself
				// is consumed. Continue through its parent so a statement-form lock does
				// not turn the final branch's array append into an expression append.
				passed_branch = true
			}
			.return_stmt, .decl_assign, .assign, .selector_assign, .index_assign, .field_init,
			.call, .infix, .prefix, .postfix, .selector, .index, .array_literal, .array_init,
			.map_init, .struct_init, .string_interp, .cast_expr, .as_expr, .or_expr, .spawn_expr,
			.assert_stmt {
				return passed_branch
			}
			else {
				return false
			}
		}
		current = parent_id
	}
	return false
}

fn (tc &TypeChecker) expr_is_direct_call_argument(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	if parent.kind != .call || parent.children_count < 2 {
		return false
	}
	for i in 1 .. parent.children_count {
		if tc.call_arg_value(tc.a.child(parent, i)) == id {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) node_is_inside_for_statement(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind in [.for_stmt, .for_in_stmt] {
			return true
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		current = parent_id
	}
	return false
}

fn (tc &TypeChecker) expr_is_multi_assignment_tail_value(expr_id flat.NodeId) bool {
	mut parent_id := tc.direct_parent_id(expr_id)
	for tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .return_stmt && parent.children_count == 1 {
			mut expected := unalias_type(tc.fn_context.return_type)
			if expected is OptionType {
				expected = unalias_type(expected.base_type)
			} else if expected is ResultType {
				expected = unalias_type(expected.base_type)
			}
			if expected is MultiReturn {
				rhs_id := tc.a.child(parent, 0)
				groups := tc.multi_expr_tail_value_groups(rhs_id, expected.types.len, false) or {
					return false
				}
				for group in groups {
					if expr_id in group {
						return true
					}
				}
				return false
			}
		}
		if parent.kind in [.assign, .decl_assign] {
			lhs_ids := tc.multi_assign_lhs_ids(parent)
			if lhs_ids.len < 2 || tc.multi_assign_rhs_count(parent) != 1 {
				return false
			}
			rhs_id := tc.a.child(parent, 1)
			groups := tc.multi_expr_tail_value_groups(rhs_id, lhs_ids.len, false) or {
				return false
			}
			for group in groups {
				if expr_id in group {
					return true
				}
			}
			return false
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		parent_id = tc.direct_parent_id(parent_id)
	}
	return false
}

fn (mut tc TypeChecker) check_must_use_call(id flat.NodeId, node flat.Node) {
	info := tc.resolve_call_info(id, node) or { return }
	decl_module := tc.fn_type_modules[info.name] or { tc.cur_module }
	decl := tc.visible_mutation_fn_decl(info.name, decl_module) or { return }
	decl_id := flat.NodeId(decl.idx)
	if !tc.declaration_has_attribute(decl_id, 'must_use') {
		return
	}
	callee := tc.a.child_node(&node, 0)
	is_method := info.has_receiver && callee.kind == .selector
	name := if is_method { callee.value } else { info.name.all_after_last('.') }
	pos := if is_method {
		tc.method_call_name_pos(node, callee)
	} else {
		node.pos
	}
	kind := if is_method { 'method' } else { 'function' }
	tc.record_warning_at(.unknown_ident,
		'return value must be used, ${kind} `${name}` was tagged with `@[must_use]`', id, pos)
}

@[direct_array_access]
fn (mut tc TypeChecker) check_branch_node(id flat.NodeId, value_tail bool) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(id)
		}
		tc.check_statement_sequence(node, 0, value_tail)
		tc.pop_scope()
		return
	}
	if value_tail {
		tc.check_node(id)
	} else {
		tc.check_stmt_node(id)
	}
}

fn (mut tc TypeChecker) apply_post_if_exit_smartcasts(id flat.NodeId) {
	for binding in tc.post_if_exit_smartcasts(id) {
		if valid_string_data(binding.name) {
			tc.smartcasts[binding.name] = binding.typ
		}
	}
}

fn (mut tc TypeChecker) apply_post_assert_smartcasts(id flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind != .assert_stmt || node.children_count == 0 {
		return
	}
	condition_id := tc.a.child(node, 0)
	condition := tc.a.node(condition_id)
	if condition.kind == .infix && condition.op in [.eq, .ne]
		&& tc.option_none_cmp_binding(condition) != none {
		return
	}
	for binding in tc.extract_smartcasts(condition_id) {
		if valid_string_data(binding.name) {
			tc.smartcasts[binding.name] = binding.typ
		}
	}
}

fn (tc &TypeChecker) post_if_exit_smartcasts(id flat.NodeId) []LocalBinding {
	if !tc.valid_node_id(id) {
		return []LocalBinding{}
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .if_expr || node.children_count < 2 {
		return []LocalBinding{}
	}
	cond_id := tc.a.child(&node, 0)
	then_id := tc.a.child(&node, 1)
	if tc.stmt_definitely_returns(then_id) {
		false_branch_smartcasts := tc.extract_else_branch_smartcasts(cond_id)
		if false_branch_smartcasts.len > 0 {
			return false_branch_smartcasts
		}
	}
	if node.children_count >= 3 {
		else_id := tc.a.child(&node, 2)
		if tc.stmt_definitely_returns(else_id) {
			return tc.extract_smartcasts(cond_id)
		}
	}
	return []LocalBinding{}
}

fn (tc &TypeChecker) negated_is_smartcast(cond_id flat.NodeId) ?LocalBinding {
	if !tc.valid_node_id(cond_id) {
		return none
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind != .prefix || cond.op != .not || cond.children_count == 0 {
		return none
	}
	inner_id := tc.a.child(&cond, 0)
	if !tc.valid_node_id(inner_id) {
		return none
	}
	inner := tc.a.nodes[int(inner_id)]
	if inner.kind != .is_expr || inner.children_count == 0 {
		return none
	}
	expr_id := tc.a.child(&inner, 0)
	key := tc.expr_key(expr_id)
	if key.len == 0 || !valid_string_data(key) || inner.value.len == 0 {
		return none
	}
	return LocalBinding{
		name: key
		typ:  tc.smartcast_target_type_for_is_expr(expr_id, inner.value)
	}
}

// branch_has_value_tail converts branch has value tail data for types.
fn (tc &TypeChecker) branch_has_value_tail(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		if node.children_count == 0 {
			return false
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return false
		}
		last := tc.a.nodes[int(last_id)]
		return last.kind in [.expr_stmt, .if_expr, .match_stmt]
			|| (last.kind == .block && tc.branch_has_value_tail(last_id))
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return false
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return false
		}
		last := tc.a.nodes[int(last_id)]
		return last.kind in [.expr_stmt, .if_expr, .match_stmt]
			|| (last.kind == .block && tc.branch_has_value_tail(last_id))
	}
	return node.kind !in [.assign, .decl_assign, .selector_assign, .index_assign, .return_stmt,
		.block]
}

// check_condition validates check condition state for types.
@[direct_array_access]
fn (mut tc TypeChecker) check_condition(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .decl_assign {
		return tc.check_if_guard(cond_id, cond)
	}
	if cond.kind == .infix && cond.op in [.logical_and, .logical_or] && cond.children_count >= 2 {
		lhs_id := tc.a.child(&cond, 0)
		rhs_id := tc.a.child(&cond, 1)
		// Only `&&` guarantees both operands hold when the then-branch runs, so its
		// guard bindings may be exported into that scope. An `||` short-circuits: the
		// branch is taken when either side is true, so a guard operand may never have
		// produced a payload. Its bindings must not become bindings of the whole
		// condition, or the body would read an unwrapped value that does not exist.
		exports_bindings := cond.op == .logical_and
		mut bindings := tc.check_condition(lhs_id)
		lhs_smartcasts := if cond.op == .logical_and {
			tc.extract_smartcasts(lhs_id)
		} else {
			tc.extract_else_branch_smartcasts(lhs_id)
		}
		if tc.valid_resolution_fast {
			saved_smartcasts := clone_smartcasts(tc.smartcasts)
			for sc in lhs_smartcasts {
				if valid_string_data(sc.name) {
					tc.smartcasts[sc.name] = sc.typ
				}
			}
			bindings << tc.check_condition(rhs_id)
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
			tc.check_infix(cond_id, cond)
			return if exports_bindings { bindings } else { []LocalBinding{} }
		}
		unsafe_alias_skipped_rhs := tc.fn_context.unsafe_reference_alias_owners.clone()
		pointer_alias_skipped_rhs :=
			clone_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
		saved_smartcasts := clone_smartcasts(tc.smartcasts)
		for sc in lhs_smartcasts {
			if valid_string_data(sc.name) {
				tc.smartcasts[sc.name] = sc.typ
			}
		}
		rhs_bindings := tc.check_condition(rhs_id)
		bindings << rhs_bindings
		tc.merge_unsafe_reference_alias_short_circuit_state(cond.op, lhs_id,
			unsafe_alias_skipped_rhs)
		tc.merge_pointer_binding_short_circuit_state(cond.op, lhs_id, pointer_alias_skipped_rhs)
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
		tc.check_infix(cond_id, cond)
		has_unresolved_generic_name := tc.expr_has_unresolved_generic_name_ident(cond_id)
		cond_type := tc.resolve_type(cond_id)
		if (!tc.condition_type_is_bool_like(cond_type) || has_unresolved_generic_name)
			&& tc.should_diagnose(cond_id) {
			cond_name := if has_unresolved_generic_name {
				'void'
			} else {
				tc.diagnostic_expr_type_name(cond_id, cond_type)
			}
			tc.record_error(.condition_mismatch,
				'non-bool type `${cond_name}` used as if condition', cond_id)
		}
		return if exports_bindings { bindings } else { []LocalBinding{} }
	}
	tc.check_bool_condition(cond_id)
	return []LocalBinding{}
}

// check_bool_condition validates check bool condition state for types.
fn (mut tc TypeChecker) check_bool_condition(cond_id flat.NodeId) {
	tc.check_node(cond_id)
	if tc.valid_resolution_fast {
		return
	}
	cond_type := tc.resolve_type(cond_id)
	if (cond_type is Unknown && cond_type.reason == 'invalid variable')
		|| tc.expr_contains_invalid_variable_binding(cond_id) {
		tc.record_error(.condition_mismatch, 'non-bool type `void` used as if condition', cond_id)
		return
	}
	if !tc.condition_type_is_bool_like(cond_type) && tc.should_diagnose(cond_id) {
		cond_name := tc.diagnostic_expr_type_name(cond_id, cond_type)
		message := if unalias_type(cond_type) is Pointer {
			'non-bool type `${cond_name}` used as if condition'
		} else {
			'if condition must be `bool`, not `${cond_type.name()}`'
		}
		tc.record_error(.condition_mismatch, message, cond_id)
	}
}

fn (tc &TypeChecker) expr_contains_invalid_variable_binding(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		if typ := tc.non_file_scope_type(node.value) {
			if typ is Unknown && typ.reason == 'invalid variable' {
				return true
			}
		}
	}
	for i in 0 .. node.children_count {
		if tc.expr_contains_invalid_variable_binding(tc.a.child(node, i)) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) condition_type_is_bool_like(typ Type) bool {
	if tc.type_compatible(typ, Type(bool_)) {
		return true
	}
	return false
}

fn (mut tc TypeChecker) check_for_condition(cond_id flat.NodeId, _node flat.Node) {
	if tc.valid_resolution_fast {
		tc.check_node(cond_id)
		return
	}
	condition := tc.a.node(cond_id)
	if condition.kind == .match_stmt {
		tc.check_stmt_node(cond_id)
		file := tc.a.source_files[condition.pos.id] or { &token.File{} }
		source := tc.source_texts_by_file[file.name] or { '' }
		pos := closest_identifier_span(source, 'match', condition.pos.offset, condition.pos.id) or {
			condition.pos
		}
		tc.record_error_at(.condition_mismatch, 'cannot use `match` in `for` loop', cond_id, pos)
		return
	}
	tc.check_node(cond_id)
	cond_type := tc.resolve_type(cond_id)
	if tc.condition_type_is_bool_like(cond_type) {
		return
	}
	if _node.value == 'c_style' && unalias_type(cond_type) is Pointer {
		return
	}
	if tc.should_diagnose(cond_id) {
		tc.record_error(.condition_mismatch,
			'if condition must be `bool`, not `${cond_type.name()}`', cond_id)
	}
}

// check_if_guard validates check if guard state for types.
fn (mut tc TypeChecker) check_if_guard(id flat.NodeId, node flat.Node) []LocalBinding {
	if node.children_count < 2 {
		return []LocalBinding{}
	}
	rhs_id := tc.a.child(&node, 1)
	error_start := tc.errors.len
	tc.fn_context.undefined_variable_context_depth++
	tc.check_node(rhs_id)
	tc.fn_context.undefined_variable_context_depth--
	lhs_ids := tc.if_guard_lhs_ids(node)
	if tc.errors[error_start..].any(it.kind == .unknown_ident) {
		return tc.if_guard_unknown_bindings(lhs_ids, node.is_mut)
	}
	mut rhs_type := tc.resolve_type(rhs_id)
	rhs_node := tc.a.node(rhs_id)
	if rhs_node.kind == .prefix && rhs_node.op == .amp && rhs_node.children_count > 0 {
		mut address_child := tc.a.child_node(rhs_node, 0)
		if address_child.kind == .paren && address_child.children_count > 0 {
			address_child = tc.a.child_node(address_child, 0)
		}
		if address_child.kind == .selector {
			if declared := tc.selector_declared_value_type(*address_child) {
				if unalias_type(declared) is OptionType {
					return []LocalBinding{}
				}
			}
		}
	}
	if rhs_node.kind == .ident {
		if base := tc.mut_param_base_for_current_ident(rhs_node.value, rhs_type) {
			rhs_type = base
		}
	}
	if rhs_node.kind == .selector {
		if declared := tc.selector_declared_value_type(*rhs_node) {
			if declared is OptionType || declared is ResultType {
				rhs_type = declared
			}
		}
	}
	mut payload := Type(void_)
	is_optional_result := rhs_type is OptionType || rhs_type is ResultType
	if rhs_type is OptionType {
		payload = rhs_type.base_type
	} else if rhs_type is ResultType {
		payload = rhs_type.base_type
	} else {
		rhs := tc.a.nodes[int(rhs_id)]
		if rhs.kind == .index && rhs.children_count > 0 {
			base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&rhs, 0)))
			if base_type is Map {
				payload = base_type.value_type
			} else if base_type is Array {
				payload = base_type.elem_type
			} else if base_type is ArrayFixed {
				payload = base_type.elem_type
			} else if base_type is String {
				payload = Type(u8_)
			}
		} else if rhs.kind == .call && rhs.children_count > 0 {
			fn_node := tc.a.child_node(&rhs, 0)
			if fn_node.kind == .selector && fn_node.value == 'get' {
				if arr := tc.call_receiver_array_type(rhs) {
					payload = arr.elem_type
				}
			}
		} else if rhs.kind == .prefix && rhs.op == .arrow && rhs.children_count > 0 {
			source_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&rhs, 0)))
			if source_type is Channel {
				payload = source_type.elem_type
			}
		}
	}
	if payload is Void && !is_optional_result {
		if tc.should_diagnose(id) {
			tc.record_error_at(.condition_mismatch,
				'if guard expression must be optional or result; expression should either return an Option or a Result',
				rhs_id, rhs_node.pos)
		}
		payload = rhs_type
	}
	if payload is Void {
		if is_optional_result && lhs_ids.all(tc.a.node(it).value == '_') {
			return []LocalBinding{}
		}
		if is_optional_result && tc.should_diagnose(id) {
			tc.record_error_at(.condition_mismatch,
				'if guard expects non-propagate option or result', id,
				tc.if_guard_diagnostic_pos(node))
		}
		return tc.if_guard_unknown_bindings(lhs_ids, node.is_mut)
	}
	if payload is MultiReturn {
		if lhs_ids.len != payload.types.len && tc.should_diagnose(id) {
			tc.record_error_at(.condition_mismatch,
				'if guard expects ${payload.types.len} variables, but got ${lhs_ids.len}', id,
				tc.if_guard_diagnostic_pos(node))
		}
		mut result := []LocalBinding{}
		for i, lhs_id in lhs_ids {
			lhs := tc.a.nodes[int(lhs_id)]
			if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
				result << LocalBinding{
					name:   lhs.value
					typ:    if i < payload.types.len {
						payload.types[i]
					} else {
						unknown_type('extra if guard binding')
					}
					is_mut: lhs.is_mut || node.is_mut
				}
			}
		}
		return result
	}
	if lhs_ids.len != 1 && tc.should_diagnose(id) {
		tc.record_error_at(.condition_mismatch,
			'if guard expects a single variable, but got ${lhs_ids.len}', id,
			tc.if_guard_diagnostic_pos(node))
	}
	if lhs_ids.len > 0 {
		mut result := []LocalBinding{}
		for i, lhs_id in lhs_ids {
			lhs := tc.a.nodes[int(lhs_id)]
			if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
				continue
			}
			result << LocalBinding{
				name:   lhs.value
				typ:    if i == 0 { payload } else { unknown_type('extra if guard binding') }
				is_mut: lhs.is_mut || node.is_mut
			}
		}
		return result
	}
	return []LocalBinding{}
}

fn (tc &TypeChecker) if_guard_unknown_bindings(lhs_ids []flat.NodeId, is_mut bool) []LocalBinding {
	mut result := []LocalBinding{}
	for lhs_id in lhs_ids {
		lhs := tc.a.node(lhs_id)
		if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
			result << LocalBinding{
				name:   lhs.value
				typ:    unknown_type('invalid if guard')
				is_mut: lhs.is_mut || is_mut
			}
		}
	}
	return result
}

fn (tc &TypeChecker) if_guard_diagnostic_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start_limit := int_max(0, int_min(node.pos.offset, source.len))
	line_start := if idx := source[..start_limit].last_index('\n') { idx + 1 } else { 0 }
	prefix := source[line_start..start_limit]
	start := if idx := prefix.last_index('if ') {
		line_start + idx
	} else {
		start_limit
	}
	mut end := int_max(start, int_min(node.pos.end, source.len))
	for end > start && source[end - 1] in [` `, `\t`, `{`] {
		end--
	}
	return token.new_span(node.pos.id, start, end)
}

fn (tc &TypeChecker) if_guard_lhs_ids(node flat.Node) []flat.NodeId {
	if node.children_count < 2 {
		return []flat.NodeId{}
	}
	mut lhs_ids := []flat.NodeId{cap: int(node.children_count) - 1}
	lhs_ids << tc.a.child(&node, 0)
	for i in 2 .. node.children_count {
		lhs_ids << tc.a.child(&node, i)
	}
	return lhs_ids
}

// check_match_stmt validates check match stmt state for types.
fn (mut tc TypeChecker) check_match_stmt(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	trailing_or := tc.match_trailing_or_parent(id)
	value_context := (!tc.is_statement_node(id) && tc.expression_node_used_as_value(id))
		&& trailing_or == none
	mut has_value_tail := false
	if value_context {
		for i in 1 .. node.children_count {
			if tc.branch_has_value_tail(tc.a.child(&node, i)) {
				has_value_tail = true
				break
			}
		}
	}
	subject_id := tc.a.child(&node, 0)
	saved_mut_local_owners := tc.fn_context.mut_local_owners.clone()
	defer {
		tc.fn_context.mut_local_owners = saved_mut_local_owners.clone()
	}
	tc.enable_explicit_mut_smartcast_target(subject_id)
	tc.check_node(subject_id)
	subject := tc.a.node(subject_id)
	mut subject_declared_type := tc.resolve_type(subject_id)
	if trailing_or != none {
		clean_subject_type := unalias_type(subject_declared_type)
		if clean_subject_type is OptionType {
			subject_declared_type = clean_subject_type.base_type
		} else if clean_subject_type is ResultType {
			subject_declared_type = clean_subject_type.base_type
		}
	}
	if subject_declared_type is Unknown && subject.kind == .ident
		&& tc.errors.any(it.kind == .unknown_ident && it.node == subject_id) {
		subject_declared_type = Type(void_)
	}
	if subject.kind == .paren {
		tc.record_warning_at(.condition_mismatch,
			'unnecessary `()` in `match` condition, use `match expr {` instead of `match (expr) {`.',
			subject_id, subject.pos)
	}
	if subject.kind == .none_expr {
		tc.record_error_at(.condition_mismatch, '`none` cannot be a match condition', id,
			tc.match_header_pos(node))
	}
	if subject.kind == .ident
		&& ((unalias_type(subject_declared_type) is Pointer && unalias_type((unalias_type(subject_declared_type) as Pointer).base_type) !is Interface
		&& unalias_type((unalias_type(subject_declared_type) as Pointer).base_type) !is SumType
		&& !tc.mut_param_binding_matches_lvalue(subject.value)
		&& !tc.current_fn_param_is_receiver(subject.value))
		|| tc.current_binding_is_shared(subject.value)) {
		tc.record_error_at(.condition_mismatch,
			'missing `*` dereferencing `${subject.value}` in match statement', subject_id,
			tc.node_value_diagnostic_pos(subject_id))
		return
	}
	missing_non_else := tc.check_match_branch_structure(id, node)
	subject_key := tc.expr_key(subject_id)
	subject_type := unalias_and_unwrap_pointer_type(subject_declared_type)
	mut seen_match_values := map[int]int{}
	mut seen_match_ranges := []MatchSeenRange{}
	mut seen_match_patterns := map[string]int{}
	unsafe_alias_base := tc.fn_context.unsafe_reference_alias_owners.clone()
	mut unsafe_alias_paths := []map[string]bool{}
	pointer_alias_base := clone_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
	mut pointer_alias_paths := []map[string][]string{}
	// One in-place scan for the whole match: the old per-condition
	// source_text_for_node(...).contains(...) copied the entire match span
	// (thousands of lines for dispatch tables) once per condition.
	match_source_has_typeof := tc.node_source_contains(id, 'typeof(')
	$if ownership ? {
		if value_context {
			tc.ownership_begin_value_branch_group()
		} else {
			tc.ownership_begin_branch_group()
		}
	}
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.child_node(&node, i)
		tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_base.clone()
		tc.fn_context.pointer_binding_value_keys =
			clone_pointer_binding_value_keys(pointer_alias_base)
		if branch.kind != .match_branch {
			tc.check_node(branch_id)
			continue
		}
		n_conds := if branch.value == 'else' { 0 } else { branch.value.int() }
		if subject_type is OptionType && n_conds > 0 {
			mut has_value_condition := false
			for j in 0 .. n_conds {
				if tc.a.child_node(branch, j).kind != .none_expr {
					has_value_condition = true
					break
				}
			}
			if has_value_condition {
				first := tc.a.child_node(branch, 0)
				last := tc.a.child_node(branch, n_conds - 1)
				tc.record_error_at(.condition_mismatch,
					'`match` expression with Option type only checks against `none`, to match its value you must unwrap it first `var?`',
					branch_id, token.new_span(first.pos.id, first.pos.offset, last.pos.end))
			}
		}
		if branch.value == 'else' && i < int(node.children_count) - 1 {
			tc.record_error_at(.condition_mismatch, '`else` must be the last branch of `match`',
				branch_id, token.new_span(branch.pos.id, branch.pos.offset, branch.pos.offset + 4))
		}
		for j in 0 .. n_conds {
			cond_id := tc.a.child(branch, j)
			tc.check_match_range_types(subject_id, subject_type, cond_id)
			tc.check_match_condition_type(subject_type, cond_id)
			tc.check_match_alias_condition(subject_declared_type, cond_id)
			tc.check_match_type_pattern_subject(subject_id, subject_type, cond_id)
			tc.check_duplicate_match_condition(branch, i, j, cond_id, mut seen_match_values, mut
				seen_match_ranges, mut seen_match_patterns)
			if !match_source_has_typeof {
				tc.record_constant_match_condition(subject_id, cond_id,
					i < int(node.children_count) - 1)
			}
		}
		$if ownership ? {
			if branch.value == 'else' {
				tc.ownership_note_branch_group_else()
			}
		}
		if subject_type is SumType {
			for j in 0 .. n_conds {
				cond_id := tc.a.child(branch, j)
				cond := tc.a.node(cond_id)
				if pattern := tc.match_type_pattern(cond) {
					if _ := tc.sum_variant_type_for_pattern(subject_type.name, pattern) {
					} else {
						base := tc.sum_base_name(subject_type.name)
						raw_variants := tc.sum_types[base] or { []string{} }
						diagnostic_variants :=
							raw_variants.map(tc.sum_variant_diagnostic_name(tc.concrete_sum_variant_name(subject_type.name, it)))
						mut variants := []string{cap: diagnostic_variants.len}
						for variant in diagnostic_variants {
							if variant.contains('.') {
								variants << variant
							}
						}
						for variant in diagnostic_variants {
							if !variant.contains('.') {
								variants << variant
							}
						}
						diagnostic_pattern := tc.sum_variant_diagnostic_name(pattern)
						base_message := '`${subject_type.name}` has no variant `${diagnostic_pattern}`'
						mut message :=
							util.new_suggestion(diagnostic_pattern, variants).say(base_message)
						if message.contains('\n${variants.len} possibilities:') {
							quoted := variants.map('`${it}`')
							message = '${base_message}.\n${variants.len} possibilities: ${quoted.join(', ')}.'
						}
						tc.record_error_at(.condition_mismatch, message, cond_id,
							tc.node_value_diagnostic_pos(cond_id))
					}
				}
			}
		} else if is_ierror_type(subject_type) {
			for j in 0 .. n_conds {
				cond_id := tc.a.child(branch, j)
				cond := tc.a.node(cond_id)
				if pattern := tc.match_type_pattern(cond) {
					if _ := tc.resolve_ierror_match_pattern(pattern) {
					} else if tc.should_diagnose(cond_id) {
						tc.record_error(.condition_mismatch,
							'`${pattern}` is not compatible with `IError`', cond_id)
					}
				}
			}
		} else if subject_type is Interface {
			for j in 0 .. n_conds {
				cond_id := tc.a.child(branch, j)
				cond := tc.a.node(cond_id)
				if pattern := tc.match_type_pattern(cond) {
					if interface_pattern_is_collapsed_container(pattern) {
						container_type := tc.parse_type(pattern)
						if !tc.type_implements_interface(container_type, subject_type)
							&& tc.should_diagnose(cond_id) {
							tc.record_error(.condition_mismatch,
								'`${pattern}` is not compatible with interface `${subject_type.name}`',
								cond_id)
						}
						continue
					}
					if target_iface := tc.resolve_interface_pattern_interface(pattern) {
						if !tc.interface_runtime_pattern_allowed(subject_type.name, target_iface)
							&& tc.should_diagnose(cond_id) {
							tc.record_error(.condition_mismatch,
								'`${pattern}` is not compatible with interface `${subject_type.name}`',
								cond_id)
						}
						continue
					}
					if concrete := tc.resolve_interface_match_pattern(pattern) {
						concrete_type := unalias_type(unwrap_pointer(tc.parse_type(concrete)))
						if concrete_type !is Interface
							&& !tc.named_type_implements_interface(concrete, subject_type.name)
							&& tc.should_diagnose(cond_id) {
							tc.record_error_at(.condition_mismatch,
								'`${pattern}` is not compatible with interface `${subject_type.name}`',
								cond_id, tc.match_condition_diagnostic_pos(cond_id))
						}
					} else if tc.should_diagnose(cond_id) {
						tc.record_error(.condition_mismatch, 'unknown type `${pattern}`', cond_id)
					}
				}
			}
		}
		$if ownership ? {
			tc.ownership_begin_branch()
		}
		saved_smartcasts := clone_smartcasts(tc.smartcasts)
		if subject_key.len > 0 && valid_string_data(subject_key) && n_conds > 1
			&& subject_type is SumType
			&& tc.match_branch_all_sum_type_patterns(subject_type, branch, n_conds) {
			tc.record_multi_pattern_return_mismatch(subject_type, branch, n_conds, subject_key)
			$if ownership ? {
				tc.ownership_mark_scope_node(branch_id)
			}
			for j in 0 .. n_conds {
				tc.fn_context.unsafe_reference_alias_owners = unsafe_alias_base.clone()
				tc.fn_context.pointer_binding_value_keys =
					clone_pointer_binding_value_keys(pointer_alias_base)
				cond := tc.a.node(tc.a.child(branch, j))
				pattern := tc.match_type_pattern(cond) or { continue }
				smartcast_type := tc.sum_variant_type_for_pattern(subject_type.name, pattern) or {
					continue
				}
				tc.smartcasts = clone_smartcasts(saved_smartcasts)
				tc.smartcasts[subject_key] = tc.parse_type(smartcast_type)
				tc.push_scope()
				tc.check_statement_sequence(branch, n_conds, value_context)
				tc.pop_scope()
			}
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
			$if ownership ? {
				tc.ownership_end_branch(branch_id)
			}
			if !tc.match_branch_definitely_returns(branch) {
				unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
				pointer_alias_paths << clone_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
			}
			continue
		}
		if subject_key.len > 0 && valid_string_data(subject_key) && n_conds == 1
			&& branch.children_count > 0 && (subject_type is SumType || is_ierror_type(subject_type)
			|| subject_type is Interface) {
			cond_id := tc.a.child(branch, 0)
			cond := tc.a.node(cond_id)
			if pattern := tc.match_type_pattern(cond) {
				smartcast_type := if subject_type is SumType {
					tc.sum_variant_type_for_pattern(subject_type.name, pattern) or { pattern }
				} else if is_ierror_type(subject_type) {
					tc.resolve_ierror_match_pattern(pattern) or { pattern }
				} else if subject_type is Interface {
					tc.resolve_interface_match_pattern(pattern) or { pattern }
				} else {
					pattern
				}
				tc.smartcasts[subject_key] = tc.parse_type(smartcast_type)
			}
		} else if subject_key.len > 0 && valid_string_data(subject_key) && n_conds > 1
			&& subject_type is SumType {
			for sc in tc.multi_match_common_field_smartcasts(subject_type, branch, n_conds,
				subject_key) {
				tc.smartcasts[sc.name] = sc.typ
			}
		} else if subject_key.len > 0 && valid_string_data(subject_key) && n_conds > 1
			&& subject_type is Interface {
			if common_iface := tc.multi_interface_match_common_interface(subject_type, branch,
				n_conds)
			{
				tc.smartcasts[subject_key] = tc.parse_type(common_iface)
			}
		}
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(branch_id)
		}
		tc.check_statement_sequence(branch, n_conds, value_context)
		tc.pop_scope()
		if value_context && has_value_tail && !tc.branch_has_value_tail(branch_id)
			&& !tc.match_branch_definitely_returns(branch)
			&& !tc.branch_tail_never_returns(branch_id) {
			tc.record_error_at(.if_branch_mismatch,
				'`match` expression requires an expression as the last statement of every branch',
				branch_id, branch.pos)
		}
		tc.smartcasts = clone_smartcasts(saved_smartcasts)
		$if ownership ? {
			tc.ownership_end_branch(branch_id)
		}
		if !tc.match_branch_definitely_returns(branch) {
			unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
			pointer_alias_paths << clone_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
		}
	}
	if !tc.match_has_else_or_exhaustive_coverage(node) {
		unsafe_alias_paths << unsafe_alias_base.clone()
		pointer_alias_paths << clone_pointer_binding_value_keys(pointer_alias_base)
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states(unsafe_alias_paths,
		unsafe_alias_base)
	tc.fn_context.pointer_binding_value_keys = merge_pointer_binding_value_states(pointer_alias_paths,
		pointer_alias_base)
	if missing_non_else {
		tc.record_match_requires_non_else(node)
	}
	tc.check_match_sumtype_exhaustiveness(id, node, subject_type)
	tc.check_match_flag_enum_exhaustiveness(id, node, subject_type)
	tc.check_match_bool_exhaustiveness(id, node, subject_type)
	if value_context {
		tc.check_match_branch_tail_type_diagnostics(id, node)
	}
	if value_context && tc.match_expr_all_tails_are_none(node) {
		tc.record_error_at(.if_branch_mismatch,
			'invalid match expression, must supply at least one value other than `none`', id,
			tc.match_header_pos(node))
	}
	$if ownership ? {
		if !tc.match_covers_all_variants(node) {
			tc.ownership_add_branch_group_base_if_no_else()
		}
		tc.ownership_end_branch_group()
	}
}

fn (tc &TypeChecker) match_trailing_or_parent(id flat.NodeId) ?flat.NodeId {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return none
	}
	parent := tc.a.node(parent_id)
	if parent.kind != .or_expr || parent.children_count < 2 || tc.a.child(parent, 0) != id {
		return none
	}
	node := tc.a.node(id)
	if node.children_count == 0 || !tc.or_expr_source_can_fail(tc.a.child(node, 0)) {
		return none
	}
	return parent_id
}

fn (mut tc TypeChecker) check_match_branch_structure(id flat.NodeId, node flat.Node) bool {
	mut else_ids := []flat.NodeId{}
	mut non_else_count := 0
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.node(branch_id)
		if branch.kind != .match_branch {
			continue
		}
		if branch.value == 'else' {
			else_ids << branch_id
		} else {
			non_else_count++
		}
	}
	if else_ids.len == 0 && non_else_count == 0 {
		tc.record_error_at(.condition_mismatch,
			'`match` must have at least two branches including `else`, or an exhaustive set of branches',
			id, tc.match_keyword_pos(node))
		return false
	}
	if else_ids.len > 1 {
		first_else := tc.a.node(else_ids[0])
		tc.record_error_at(.condition_mismatch, '`match` can have only one `else` branch',
			else_ids[0], token.new_span(first_else.pos.id, first_else.pos.offset,

			first_else.pos.offset + 4))
	}
	return non_else_count == 0 && else_ids.len > 0
}

fn (mut tc TypeChecker) record_match_requires_non_else(node flat.Node) {
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.node(branch_id)
		if branch.kind == .match_branch && branch.value == 'else' {
			tc.record_error_at(.condition_mismatch,
				'`match` must have at least one non `else` branch', branch_id, token.new_span(branch.pos.id,
				branch.pos.offset, branch.pos.offset + 4))
			return
		}
	}
}

fn (tc &TypeChecker) match_keyword_pos(node flat.Node) token.Pos {
	return token.new_span(node.pos.id, node.pos.offset, node.pos.offset + 5)
}

fn (mut tc TypeChecker) check_match_type_pattern_subject(subject_id flat.NodeId, subject_type Type, cond_id flat.NodeId) {
	if tc.errors.any(it.node == cond_id && it.msg.starts_with('cannot match alias type `')) {
		return
	}
	if tc.match_type_pattern(*tc.a.node(cond_id)) == none || subject_type is SumType
		|| subject_type is Interface || subject_type is Alias || is_ierror_type(subject_type)
		|| subject_type is Unknown {
		return
	}
	if tc.valid_node_id(subject_id) {
		subject := tc.a.node(subject_id)
		if subject.kind == .ident {
			declared := unalias_and_unwrap_pointer_type(tc.cur_scope.lookup(subject.value) or {
				subject_type
			})
			if declared is SumType || declared is Interface || is_ierror_type(declared) {
				return
			}
		}
	}
	if unalias_and_unwrap_pointer_type(subject_type) is Struct {
		tc.record_error_at(.condition_mismatch,
			'struct instances cannot be matched by type name, they can only be matched to other instances of the same struct type',
			cond_id, tc.match_condition_diagnostic_pos(cond_id))
		return
	}
	subject_name := tc.source_text_for_node(subject_id)
	tc.record_error_at(.condition_mismatch,
		'matching by type can only be done for sum types, generics, interfaces, `${subject_name}` is none of those',
		cond_id, tc.match_condition_diagnostic_pos(cond_id))
}

fn (mut tc TypeChecker) check_match_alias_condition(subject_type Type, cond_id flat.NodeId) {
	if subject_type !is Alias {
		return
	}
	if unalias_type(subject_type) is SumType {
		return
	}
	pattern := tc.match_type_pattern(*tc.a.node(cond_id)) or { return }
	tc.record_error_at(.condition_mismatch,
		'cannot match alias type `${short_type_name(subject_type.name())}` with `${short_type_name(pattern)}`',
		cond_id, tc.match_condition_diagnostic_pos(cond_id))
}

fn (mut tc TypeChecker) check_match_flag_enum_exhaustiveness(id flat.NodeId, node flat.Node, subject_type Type) {
	clean_subject := unalias_type(subject_type)
	if clean_subject !is Enum
		|| (!clean_subject.is_flag && clean_subject.name() !in tc.flag_enums)
		|| !tc.should_diagnose(id) {
		return
	}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind == .match_branch && branch.value == 'else' {
			return
		}
	}
	tc.record_error_at(.condition_mismatch, 'match must be exhaustive (add `else {}` at the end)',
		id, tc.match_header_pos(node))
}

fn (mut tc TypeChecker) check_match_bool_exhaustiveness(id flat.NodeId, node flat.Node, subject_type Type) {
	if unalias_type(subject_type).name() != 'bool' || !tc.should_diagnose(id) {
		return
	}
	mut covered_true := false
	mut covered_false := false
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		if branch.value == 'else' {
			return
		}
		n_conds := branch.value.int()
		for j in 0 .. n_conds {
			cond := tc.a.child_node(branch, j)
			if cond.kind == .bool_literal {
				if cond.value == 'true' {
					covered_true = true
				} else {
					covered_false = true
				}
			}
		}
	}
	mut missing := []string{}
	if !covered_true {
		missing << '`true`'
	}
	if !covered_false {
		missing << '`false`'
	}
	if missing.len > 0 && node.children_count > 1 {
		tc.record_error_at(.condition_mismatch,
			'match must be exhaustive (add match branches for: ${missing.join(', ')} or `else {}` at the end)',
			id, tc.match_header_pos(node))
	}
}

fn (mut tc TypeChecker) check_match_sumtype_exhaustiveness(id flat.NodeId, node flat.Node, subject_type Type) {
	clean_subject := unalias_type(subject_type)
	if clean_subject !is SumType || !tc.should_diagnose(id) {
		return
	}
	sum_subject := clean_subject as SumType
	raw_variants := tc.sum_types[tc.sum_base_name(sum_subject.name)] or { return }
	if raw_variants.len == 0 {
		return
	}
	mut variants := tc.sum_exhaustive_leaf_variants(sum_subject.name, 0)
	if variants.len == 0 {
		variants = raw_variants.map(tc.concrete_sum_variant_name(sum_subject.name, it))
	}
	mut covered := map[string]bool{}
	mut else_ids := []flat.NodeId{}
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(node, i)
		branch := tc.a.node(branch_id)
		if branch.kind != .match_branch {
			continue
		}
		if branch.value == 'else' {
			else_ids << branch_id
			continue
		}
		for j in 0 .. branch.value.int() {
			cond := tc.a.child_node(branch, j)
			pattern := tc.match_type_pattern(cond) or { continue }
			qpattern := tc.qualify_name(pattern)
			if matched := tc.sum_variant_type_for_pattern(sum_subject.name, pattern) {
				matched_type := tc.parse_type(matched)
				matched_sum_name := if matched_type is SumType {
					matched_type.name
				} else if matched_type is Alias && matched_type.base_type is SumType {
					(matched_type.base_type as SumType).name
				} else {
					''
				}
				leaves := if matched_sum_name.len > 0 {
					tc.sum_exhaustive_leaf_variants(matched_sum_name, 0)
				} else {
					[]string{}
				}
				if leaves.len > 0 {
					for leaf in leaves {
						covered[leaf] = true
					}
				} else {
					for variant in variants {
						if variant == matched {
							covered[variant] = true
						}
					}
				}
			}
			for variant in variants {
				uses_type_arguments := variant.contains('[') || pattern.contains('[')
					|| qpattern.contains('[')
				if variant == pattern || variant == qpattern
					|| (uses_type_arguments && (tc.generic_type_name_matches(variant, pattern)
					|| tc.generic_type_name_matches(variant, qpattern))) {
					covered[variant] = true
				}
			}
			pattern_short := short_type_name(pattern)
			mut short_match := ''
			mut short_count := 0
			for variant in variants {
				if short_type_name(variant) == pattern_short {
					short_count++
					short_match = variant
				}
			}
			if short_count == 1 {
				covered[short_match] = true
			}
		}
	}
	mut missing := []string{}
	for variant in variants {
		if variant !in covered {
			missing << short_type_name(variant)
		}
	}
	if missing.len > 0 && else_ids.len == 0 {
		quoted_missing := missing.map('`${it}`')
		tc.record_error_at(.condition_mismatch,
			'match must be exhaustive (add match branches for: ${quoted_missing.join(', ')} or `else {}` at the end)',
			id, tc.match_header_pos(node))
		return
	}
	if missing.len == 0 {
		for i in 1 .. node.children_count {
			branch := tc.a.child_node(node, i)
			if branch.kind != .match_branch || branch.value == 'else' {
				continue
			}
			for j in 0 .. branch.value.int() {
				pattern := tc.match_type_pattern(tc.a.child_node(branch, j)) or { continue }
				if tc.parse_type(pattern) is Alias {
					return
				}
			}
		}
		for else_id in else_ids {
			else_branch := tc.a.node(else_id)
			tc.record_error_at(.condition_mismatch,
				'match expression is exhaustive, `else` is unnecessary', else_id, token.new_span(else_branch.pos.id,
				else_branch.pos.offset, else_branch.pos.offset + 4))
		}
	}
}

fn (tc &TypeChecker) sum_exhaustive_leaf_variants(sum_name string, depth int) []string {
	if depth >= 16 {
		return []string{}
	}
	raw_variants := tc.sum_types[tc.sum_base_name(sum_name)] or { return []string{} }
	mut leaves := []string{}
	for raw_variant in raw_variants {
		concrete := tc.concrete_sum_variant_name(sum_name, raw_variant)
		typ := tc.parse_type(concrete)
		nested_name := if typ is SumType {
			typ.name
		} else if typ is Alias && typ.base_type is SumType {
			(typ.base_type as SumType).name
		} else {
			''
		}
		if nested_name.len > 0 && tc.sum_base_name(nested_name) != tc.sum_base_name(sum_name) {
			nested := tc.sum_exhaustive_leaf_variants(nested_name, depth + 1)
			if nested.len > 0 {
				for leaf in nested {
					if leaf !in leaves {
						leaves << leaf
					}
				}
				continue
			}
		}
		if concrete !in leaves {
			leaves << concrete
		}
	}
	return leaves
}

fn (mut tc TypeChecker) check_match_condition_type(subject_type Type, cond_id flat.NodeId) {
	cond := tc.a.node(cond_id)
	if cond.kind == .range || tc.match_type_pattern(*cond) != none {
		return
	}
	condition_type := tc.resolve_type(cond_id)
	clean_subject := unalias_type(subject_type)
	if clean_subject is OptionType {
		return
	}
	if cond.kind == .enum_val && clean_subject is Enum {
		return
	}
	clean_condition := unalias_type(condition_type)
	if clean_condition is Unknown || tc.type_compatible(condition_type, subject_type)
		|| tc.type_compatible(subject_type, condition_type) {
		return
	}
	tc.record_error_at(.condition_mismatch, 'cannot match `${clean_subject.name()}` with `${tc.diagnostic_expr_type_name(cond_id,
		condition_type)}`', cond_id, cond.pos)
}

fn (mut tc TypeChecker) check_match_branch_tail_type_diagnostics(id flat.NodeId, node flat.Node) {
	mut tails := []flat.NodeId{}
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.node(branch_id)
		if branch.kind != .match_branch {
			continue
		}
		tail_id := tc.branch_tail_expr_id(branch_id)
		if tc.valid_node_id(tail_id) {
			tails << tail_id
		}
	}
	if tails.len < 2 {
		return
	}
	mut context_type := tc.expected_context_for_expr(id) or { Type(void_) }
	parent_id := tc.direct_parent_id(id)
	if context_type is Void && tc.valid_node_id(parent_id)
		&& tc.a.node(parent_id).kind == .return_stmt {
		context_type = tc.fn_context.return_type
	}
	mut multi_context := unalias_type(context_type)
	if multi_context is OptionType {
		multi_context = unalias_type(multi_context.base_type)
	} else if multi_context is ResultType {
		multi_context = unalias_type(multi_context.base_type)
	}
	if multi_context is MultiReturn {
		if context_type is OptionType || context_type is ResultType {
			if _ := tc.wrapped_multi_return_value_groups(id, multi_context.types.len, false,
				context_type)
			{
				return
			}
		} else {
			if _ := tc.multi_expr_tail_value_groups(id, multi_context.types.len, false) {
				return
			}
		}
	}
	if context_type !is Void && tc.branches_compatible_with(id, context_type) {
		return
	}
	mut first_cast_index := -1
	for i, tail_id in tails {
		if tc.a.node(tail_id).kind == .cast_expr {
			first_cast_index = i
			break
		}
	}
	if first_cast_index < 0 {
		tc.check_general_match_branch_tail_types(id, node, tails)
		return
	}
	cast_id := tails[first_cast_index]
	cast_node := tc.a.node(cast_id)
	cast_type := tc.parse_type(cast_node.value)
	if !infix_power_type_is_numeric(cast_type) {
		return
	}
	if first_cast_index > 0 {
		first_tail := tc.a.node(tails[0])
		if first_tail.kind == .int_literal {
			tc.record_error_at(.if_branch_mismatch,
				'the type of the last expression of the first match branch was `int literal`, which is not compatible with `${cast_type.name()}`',
				cast_id, cast_node.pos)
		}
		return
	}
	for i in 1 .. tails.len {
		tail_id := tails[i]
		tail := tc.a.node(tail_id)
		if tail.kind == .cast_expr {
			tail_type := tc.parse_type(tail.value)
			if tail_type.name() != cast_type.name() {
				tc.record_error_at(.if_branch_mismatch,
					'the type of the last expression in the first match branch was an explicit `${cast_type.name()}`, not `${tail_type.name()}`',
					tail_id, tail.pos)
				return
			}
			continue
		}
		if literal := tc.integer_literal_source(tail_id) {
			if type_range := integer_type_range(cast_type) {
				if integer_literal_outside_range(literal.replace('_', ''), type_range) {
					tc.record_error_at(.if_branch_mismatch,
						'${literal} does not fit the range of `${cast_type.name()}`', tail_id,
						tail.pos)
					return
				}
			}
		}
	}
}

fn (mut tc TypeChecker) check_general_match_branch_tail_types(id flat.NodeId, node flat.Node, tails []flat.NodeId) {
	subject_id := tc.a.child(&node, 0)
	subject_key := tc.expr_key(subject_id)
	subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
	mut tail_types := []Type{cap: tails.len}
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.node(branch_id)
		if branch.kind != .match_branch {
			continue
		}
		tail_id := tc.branch_tail_expr_id(branch_id)
		if tc.valid_node_id(tail_id) {
			tail_types << tc.match_branch_tail_diagnostic_type(subject_key, subject_type, branch,
				tail_id)
		}
	}
	if tail_types.len != tails.len {
		return
	}
	mut context_type := tc.expected_context_for_expr(id) or { Type(void_) }
	parent_id := tc.direct_parent_id(id)
	if context_type is Void && tc.valid_node_id(parent_id)
		&& tc.a.node(parent_id).kind == .return_stmt {
		context_type = tc.fn_context.return_type
	}
	if context_type !is Void {
		if tc.branches_compatible_with(id, context_type) {
			return
		}
		mut clean_expected := unalias_type(context_type)
		mut wrapped_context := false
		mut expected_is_option := false
		mut expected_is_result := false
		if clean_expected is OptionType {
			wrapped_context = true
			expected_is_option = true
			context_type = clean_expected.base_type
			clean_expected = unalias_type(clean_expected.base_type)
		} else if clean_expected is ResultType {
			wrapped_context = true
			expected_is_result = true
			context_type = clean_expected.base_type
			clean_expected = unalias_type(clean_expected.base_type)
		}
		if wrapped_context {
			for i, tail_id in tails {
				actual := tail_types[i]
				clean_actual := unalias_type(actual)
				same_wrapper := if expected_is_option && clean_actual is OptionType {
					tc.type_compatible(clean_actual.base_type, context_type)
				} else if expected_is_result && clean_actual is ResultType {
					tc.type_compatible(clean_actual.base_type, context_type)
				} else {
					false
				}
				if actual is Void || actual is Unknown || is_ierror_type(actual)
					|| tc.type_compatible_with_ierror_payload(actual)
					|| tc.expr_never_returns(tail_id)
					|| tc.type_compatible(actual, context_type) || same_wrapper
					|| (expected_is_option && tc.branch_tail_is_none_literal(tail_id)) {
					continue
				}
				tc.record_match_branch_return_type_mismatch(tail_id, context_type, actual)
				return
			}
			return
		}
		if clean_expected is SumType {
			for i, tail_id in tails {
				actual := tail_types[i]
				if tc.type_name_is_direct_sum_variant(actual, clean_expected) {
					continue
				}
				tc.record_match_branch_return_type_mismatch(tail_id, context_type, actual)
				return
			}
			return
		}
	}
	mut expected := tail_types[0]
	if expected is Void || expected is Unknown {
		return
	}
	for i in 1 .. tails.len {
		tail_id := tails[i]
		actual := tail_types[i]
		if inferred := inferred_contextual_if_type(expected, actual) {
			expected = inferred
			continue
		}
		if actual is Void || actual is Unknown
			|| tc.if_branch_type_compatible_with_context(actual, tail_id, expected)
			|| (tc.type_compatible(actual, expected) && tc.type_compatible(expected, actual)) {
			continue
		}
		tc.record_match_branch_return_type_mismatch(tail_id, expected, actual)
		return
	}
}

fn (tc &TypeChecker) match_branch_tail_diagnostic_type(subject_key string, subject_type Type, branch flat.Node, tail_id flat.NodeId) Type {
	n_conds := if branch.value == 'else' { 0 } else { branch.value.int() }
	if subject_key.len > 0 && n_conds == 1 && branch.children_count > 0 {
		cond := tc.a.child_node(&branch, 0)
		if pattern := tc.match_type_pattern(cond) {
			smartcast_name := if subject_type is SumType {
				tc.sum_variant_type_for_pattern(subject_type.name, pattern) or { '' }
			} else if subject_type is Interface {
				tc.resolve_interface_match_pattern(pattern) or { '' }
			} else {
				''
			}
			if smartcast_name.len > 0 {
				smartcast_type := tc.parse_type(smartcast_name)
				tail := tc.a.node(tail_id)
				if tail.kind == .ident && tc.expr_key(tail_id) == subject_key {
					return smartcast_type
				}
				if tail.kind == .prefix && tail.op == .amp && tail.children_count > 0 {
					child_id := tc.a.child(tail, 0)
					if tc.expr_key(child_id) == subject_key {
						if subject_type is Interface {
							return smartcast_type
						}
						return Type(Pointer{
							base_type: smartcast_type
						})
					}
				}
			}
		}
	}
	return tc.resolve_type(tail_id)
}

fn (tc &TypeChecker) type_name_is_direct_sum_variant(actual Type, expected SumType) bool {
	actual_name := actual.name()
	base := tc.sum_base_name(expected.name)
	for variant in tc.sum_types[base] or { []string{} } {
		if actual_name == variant || short_type_name(actual_name) == short_type_name(variant) {
			return true
		}
	}
	return false
}

fn (mut tc TypeChecker) record_match_branch_return_type_mismatch(id flat.NodeId, expected Type, actual Type) {
	if expected.name() == tc.diagnostic_expr_type_name(id, actual) {
		return
	}
	expected_multi := multi_return_payload_type(expected)
	actual_multi := multi_return_payload_type(actual)
	if expected_multi != none && actual_multi != none {
		expected_types := expected_multi.types
		actual_types := actual_multi.types
		if expected_types.len == actual_types.len {
			mut compatible := true
			for i, actual_type in actual_types {
				if tc.promoted_multi_tail_type(expected_types[i], actual_type) == none {
					compatible = false
					break
				}
			}
			if compatible {
				return
			}
		}
	}
	node := tc.a.node(id)
	pos := if node.kind == .prefix && node.op == .amp {
		tc.prefix_operator_pos(id, '&')
	} else if node.kind == .selector {
		tc.node_value_diagnostic_pos(id)
	} else {
		node.pos
	}
	tc.record_error_at(.if_branch_mismatch, 'return type mismatch, it should be `${expected.name()}`, but it is instead `${tc.diagnostic_expr_type_name(id,
		actual)}`; cannot return `${actual.name()}` as `${expected.name()}`', id, pos)
}

fn (tc &TypeChecker) expr_has_match_branch_type_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	root := tc.a.node(id)
	return tc.errors.any(it.kind == .if_branch_mismatch && it.pos.id == root.pos.id
		&& it.pos.offset >= root.pos.offset && it.pos.end <= root.pos.end
		&& it.msg.starts_with('return type mismatch, it should be '))
}

fn (mut tc TypeChecker) check_match_range_types(subject_id flat.NodeId, subject_type Type, cond_id flat.NodeId) {
	cond := tc.a.node(cond_id)
	if cond.kind != .range || cond.children_count < 2 {
		return
	}
	low_id := tc.a.child(cond, 0)
	high_id := tc.a.child(cond, 1)
	low_type := unalias_type(tc.resolve_type(low_id))
	high_type := unalias_type(tc.resolve_type(high_id))
	if low_type is Unknown || high_type is Unknown {
		return
	}
	mut range_type := low_type
	low_is_literal := tc.range_endpoint_is_literal(low_id)
	high_is_literal := tc.range_endpoint_is_literal(high_id)
	if low_type.name() != high_type.name() {
		if low_is_literal && !high_is_literal && low_type.is_integer() && high_type.is_integer() {
			range_type = high_type
		} else if high_is_literal && !low_is_literal && low_type.is_integer()
			&& high_type.is_integer() {
			range_type = low_type
		} else if low_is_literal && high_is_literal && low_type.is_integer()
			&& high_type.is_integer() {
			range_type = if low_type is Rune {
				low_type
			} else if high_type is Rune {
				high_type
			} else {
				low_type
			}
		} else {
			tc.record_error_with_details_at(.condition_mismatch,
				'the low and high parts of a range expression, should have matching types',
				cond_id, tc.match_condition_diagnostic_pos(cond_id), [
				'\n low part type: ${tc.match_range_endpoint_type_name(low_id, low_type)}',
				'high part type: ${tc.match_range_endpoint_type_name(high_id, high_type)}',
			])
		}
	}
	clean_subject := unalias_type(subject_type)
	rune_range_matches_byte := range_type is Rune && clean_subject.name() in ['u8', 'char']
	integer_literal_range_matches_integer_subject := low_is_literal && high_is_literal
		&& range_type.is_integer() && clean_subject.is_integer()
	integer_literal_range_matches_enum_subject := low_is_literal && high_is_literal
		&& range_type.is_integer() && clean_subject is Enum
	if clean_subject !is Unknown && range_type !is Unknown
		&& clean_subject.name() != range_type.name() && !rune_range_matches_byte
		&& !integer_literal_range_matches_integer_subject
		&& !integer_literal_range_matches_enum_subject {
		tc.record_error_with_details_at(.condition_mismatch,
			'the range type and the match condition type should match', cond_id,
			tc.match_condition_diagnostic_pos(cond_id), [
			'\nmatch condition type: ${clean_subject.name()}',
			'          range type: ${range_type.name()}',
		])
	}
	tc.check_match_range_values(cond_id, low_id, high_id)
	_ = subject_id
}

fn (tc &TypeChecker) match_range_endpoint_type_name(id flat.NodeId, typ Type) string {
	if tc.range_endpoint_is_literal(id) && typ.is_integer() && typ !is Rune {
		return 'int literal'
	}
	return typ.name()
}

fn (mut tc TypeChecker) check_match_range_values(cond_id flat.NodeId, low_id flat.NodeId, high_id flat.NodeId) {
	low := tc.match_condition_int_value(low_id) or {
		tc.record_error_at(.condition_mismatch,
			'match branch range expressions need the start value to be known at compile time (only enums, const or literals are supported)',
			cond_id, tc.a.node(low_id).pos)
		return
	}
	high := tc.match_condition_int_value(high_id) or {
		tc.record_error_at(.condition_mismatch,
			'match branch range expressions need the end value to be known at compile time (only enums, const or literals are supported)',
			cond_id, tc.a.node(high_id).pos)
		return
	}
	if low > high {
		tc.record_error_at(.condition_mismatch,
			'the start value `${low}` should be lower than the end value `${high}`', cond_id,
			tc.match_condition_diagnostic_pos(cond_id))
	}
}

fn (tc &TypeChecker) match_expr_all_tails_are_none(node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	mut saw_branch := false
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.node(branch_id)
		if branch.kind != .match_branch {
			continue
		}
		saw_branch = true
		if !tc.branch_tail_is_none_literal(tc.branch_tail_expr_id(branch_id)) {
			return false
		}
	}
	return saw_branch
}

fn (tc &TypeChecker) match_header_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := node.pos.offset
	if start < 0 || start >= source.len {
		return node.pos
	}
	if relative := source[start..int_min(node.pos.end, source.len)].index('{') {
		return token.new_span(node.pos.id, start, start + relative + 1)
	}
	return node.pos
}

fn (mut tc TypeChecker) record_constant_match_condition(subject_id flat.NodeId, cond_id flat.NodeId, has_following_branch bool) {
	value := if subject_value := tc.constant_bool_value(subject_id) {
		if condition_value := tc.constant_bool_value(cond_id) {
			subject_value == condition_value
		} else {
			tc.constant_comparison_value(subject_id, cond_id, .eq) or { return }
		}
	} else {
		tc.constant_comparison_value(subject_id, cond_id, .eq) or { return }
	}
	cond := tc.a.node(cond_id)
	tc.record_notice_at(.condition_mismatch, if value {
		'match is always true'
	} else {
		'match is always false'
	}, cond_id, cond.pos)
	if value && has_following_branch && tc.same_simple_comparison_expr(subject_id, cond_id) {
		tc.record_warning_at(.condition_mismatch,
			'self-comparison match branch is always true; following branches may be unreachable',
			cond_id, cond.pos)
	}
}

fn (mut tc TypeChecker) check_duplicate_match_condition(branch &flat.Node, branch_index int, cond_index int, cond_id flat.NodeId, mut seen_values map[int]int, mut seen_ranges []MatchSeenRange, mut seen_patterns map[string]int) {
	cond := tc.a.node(cond_id)
	if cond.kind == .range && cond.children_count >= 2 {
		start := tc.match_condition_int_value(tc.a.child(cond, 0)) or { return }
		end := tc.match_condition_int_value(tc.a.child(cond, 1)) or { return }
		if start > end {
			return
		}
		low := int_min(start, end)
		high := int_max(start, end)
		mut duplicate_values := map[int]int{}
		for value, owner_branch in seen_values {
			if value >= low && value <= high {
				duplicate_values[value] = owner_branch
			}
		}
		for seen_range in seen_ranges {
			overlap_start := int_max(low, seen_range.start)
			overlap_end := int_min(high, seen_range.end)
			if overlap_start > overlap_end {
				continue
			}
			for value in overlap_start .. overlap_end + 1 {
				if value !in duplicate_values {
					duplicate_values[value] = seen_range.branch
				}
			}
		}
		mut ordered := duplicate_values.keys()
		ordered.sort()
		for value in ordered {
			owner_branch := duplicate_values[value]
			tc.record_match_duplicate(branch, branch_index, cond_index, cond_id, owner_branch,
				value.str())
		}
		seen_ranges << MatchSeenRange{
			start:  low
			end:    high
			branch: branch_index
		}
		return
	}
	if value := tc.match_condition_int_value(cond_id) {
		mut owner_branch := -1
		if owner := seen_values[value] {
			owner_branch = owner
		} else {
			for seen_range in seen_ranges {
				if value >= seen_range.start && value <= seen_range.end {
					owner_branch = seen_range.branch
					break
				}
			}
		}
		if owner_branch >= 0 {
			tc.record_match_duplicate(branch, branch_index, cond_index, cond_id, owner_branch,
				value.str())
		} else {
			seen_values[value] = branch_index
		}
		return
	}
	key, display := tc.match_condition_pattern_key(cond_id)
	if key.len == 0 {
		return
	}
	if owner_branch := seen_patterns[key] {
		tc.record_match_duplicate(branch, branch_index, cond_index, cond_id, owner_branch, display)
	} else {
		seen_patterns[key] = branch_index
	}
}

fn (mut tc TypeChecker) record_match_duplicate(branch &flat.Node, branch_index int, cond_index int, cond_id flat.NodeId, owner_branch int, display string) {
	if !tc.should_diagnose(cond_id) {
		return
	}
	mut pos := tc.match_condition_diagnostic_pos(cond_id)
	if owner_branch == branch_index && cond_index > 0 {
		first_pos := tc.match_condition_diagnostic_pos(tc.a.child(branch, 0))
		if first_pos.id == pos.id {
			pos = token.new_span(pos.id, first_pos.offset, pos.end)
		}
	}
	tc.record_error_at(.condition_mismatch, 'match case `${display}` is handled more than once',
		cond_id, pos)
}

fn (tc &TypeChecker) match_condition_pattern_key(id flat.NodeId) (string, string) {
	node := tc.a.node(id)
	if pattern := tc.match_type_pattern(node) {
		return 'type:${pattern}', pattern
	}
	if node.kind == .enum_val {
		return 'enum:${node.value}', node.value
	}
	if node.kind == .string_literal {
		return 'string:${node.value}', node.value
	}
	if node.kind == .bool_literal {
		return 'bool:${node.value}', node.value
	}
	if node.kind == .selector {
		key := tc.expr_key(id)
		if key.len > 0 {
			return 'selector:${key}', key
		}
	}
	text := tc.source_text_for_node(id)
	if text.len == 0 {
		return '', ''
	}
	return '${node.kind}:${text}', text
}

fn (tc &TypeChecker) match_condition_int_value(id flat.NodeId) ?int {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .char_literal {
		return match_char_literal_value(node.value)
	}
	return tc.const_int_expr(id, tc.cur_module, []string{})
}

fn match_char_literal_value(value string) ?int {
	if value.len == 0 {
		return none
	}
	if value[0] != `\\` {
		runes := value.runes()
		if runes.len == 0 {
			return none
		}
		return int(runes[0])
	}
	if value.len < 2 {
		return none
	}
	if value[1] == `x` && value.len > 2 {
		return v_int_literal_value('0x${value[2..]}')
	}
	return match value[1] {
		`n` { int(`\n`) }
		`t` { int(`\t`) }
		`r` { int(`\r`) }
		`\\` { int(`\\`) }
		`'` { int(`'`) }
		`"` { int(`"`) }
		`$` { int(`$`) }
		`0` { 0 }
		`a` { 7 }
		`b` { 8 }
		`f` { 12 }
		`v` { 11 }
		else { int(value[1]) }
	}
}

fn (tc &TypeChecker) match_condition_diagnostic_pos(id flat.NodeId) token.Pos {
	if !tc.valid_node_id(id) {
		return token.Pos{}
	}
	node := tc.a.node(id)
	if node.kind == .range && node.children_count >= 2 {
		start := tc.match_condition_diagnostic_pos(tc.a.child(node, 0))
		end := tc.match_condition_diagnostic_pos(tc.a.child(node, 1))
		if start.id == end.id {
			return token.new_span(start.id, start.offset, end.end)
		}
	}
	if node.kind == .enum_val {
		pos := tc.node_value_diagnostic_pos(id)
		file := tc.a.source_files[pos.id] or { return pos }
		source := tc.source_texts_by_file[file.name] or { return pos }
		if pos.offset > 0 && pos.offset <= source.len && source[pos.offset - 1] == `.` {
			return token.new_span(pos.id, pos.offset - 1, pos.end)
		}
		return pos
	}
	if node.kind == .ident {
		return tc.node_value_diagnostic_pos(id)
	}
	return node.pos
}

fn (tc &TypeChecker) match_branch_all_sum_type_patterns(subject SumType, branch &flat.Node, n_conds int) bool {
	for i in 0 .. n_conds {
		cond := tc.a.node(tc.a.child(branch, i))
		pattern := tc.match_type_pattern(cond) or { return false }
		if _ := tc.sum_variant_type_for_pattern(subject.name, pattern) {
			continue
		}
		return false
	}
	return true
}

fn (mut tc TypeChecker) record_multi_pattern_return_mismatch(subject SumType, branch &flat.Node, n_conds int, subject_key string) {
	expected_type := unalias_type(tc.fn_context.return_type)
	if expected_type !is SumType {
		return
	}
	expected_sum := expected_type as SumType
	mut variants := []string{cap: n_conds}
	mut incompatible := false
	for i in 0 .. n_conds {
		cond := tc.a.node(tc.a.child(branch, i))
		pattern := tc.match_type_pattern(cond) or { return }
		variant_name := tc.sum_variant_type_for_pattern(subject.name, pattern) or { return }
		variant_type := tc.parse_type(variant_name)
		variants << short_type_name(variant_type.name())
		if !tc.sum_type_contains_variant(expected_sum, variant_type) {
			incompatible = true
		}
	}
	if !incompatible {
		return
	}
	actual_name := '(${variants.join(' | ')})'
	for i in n_conds .. branch.children_count {
		return_id := tc.a.child(branch, i)
		return_node := tc.a.node(return_id)
		if return_node.kind != .return_stmt || return_node.children_count != 1 {
			continue
		}
		value_id := tc.a.child(return_node, 0)
		if tc.expr_key(value_id) != subject_key {
			continue
		}
		tc.record_error_at(.return_mismatch,
			'cannot use `${actual_name}` as type `${expected_sum.name}` in return argument',
			value_id, tc.a.node(value_id).pos)
	}
}

fn (tc &TypeChecker) multi_match_common_field_smartcasts(subject SumType, branch &flat.Node, n_conds int, subject_key string) []LocalBinding {
	mut common := []LocalBinding{}
	for i in 0 .. n_conds {
		cond := tc.a.node(tc.a.child(branch, i))
		pattern := tc.match_type_pattern(cond) or { return []LocalBinding{} }
		variant_name := tc.sum_variant_type_for_pattern(subject.name, pattern) or {
			return []LocalBinding{}
		}
		variant_type := unalias_type(tc.parse_type(variant_name))
		if variant_type !is Struct {
			return []LocalBinding{}
		}
		variant := variant_type as Struct
		if i == 0 {
			for field in tc.structs[variant.name] or { return []LocalBinding{} } {
				common << LocalBinding{
					name: '${subject_key}.${field.name}'
					typ:  field.typ
				}
			}
			continue
		}
		mut intersection := []LocalBinding{cap: common.len}
		for candidate in common {
			field_name := candidate.name.all_after_last('.')
			field_type := tc.struct_field_type(variant.name, field_name) or { continue }
			if tc.type_compatible(field_type, candidate.typ)
				&& tc.type_compatible(candidate.typ, field_type) {
				intersection << candidate
			}
		}
		common = intersection.clone()
		if common.len == 0 {
			break
		}
	}
	return common
}

fn (tc &TypeChecker) multi_interface_match_common_interface(subject Interface, branch &flat.Node, n_conds int) ?string {
	mut variants := []string{cap: n_conds}
	for i in 0 .. n_conds {
		cond := tc.a.node(tc.a.child(branch, i))
		pattern := tc.match_type_pattern(cond) or { return none }
		variant_name := tc.resolve_interface_match_pattern(pattern) or { return none }
		variants << variant_name
	}
	subject_name := tc.interface_metadata_name(subject.name)
	mut best := ''
	mut best_score := -1
	for iface_name, _ in tc.interface_names {
		name := tc.interface_metadata_name(iface_name)
		if name == subject_name || tc.interface_has_no_requirements(name) {
			continue
		}
		mut implements_all := true
		for variant in variants {
			if !tc.named_type_implements_interface(variant, name) {
				implements_all = false
				break
			}
		}
		if !implements_all {
			continue
		}
		score := tc.interface_abstract_method_names(name).len + tc.interface_field_list(name).len
		if score > best_score || (score == best_score && (best.len == 0 || name < best)) {
			best = name
			best_score = score
		}
	}
	if best.len == 0 {
		return none
	}
	return best
}

fn (tc &TypeChecker) resolve_interface_match_pattern(pattern string) ?string {
	for candidate in tc.interface_match_pattern_candidates(pattern) {
		if interface_pattern_is_collapsed_container(candidate) {
			container_type := tc.parse_type(candidate)
			if container_type is Array || container_type is Map {
				return container_type.name()
			}
			continue
		}
		if is_builtin_type_name(candidate) {
			return candidate
		}
		if tc.pattern_type_known(candidate) {
			return candidate
		}
		if tc.type_symbol_known(candidate) {
			return candidate
		}
	}
	return none
}

fn interface_pattern_is_collapsed_container(pattern string) bool {
	clean := trimmed_space(pattern)
	return clean.starts_with('[]') || clean.starts_with('map[')
}

fn (tc &TypeChecker) resolve_interface_pattern_interface(pattern string) ?string {
	for candidate in tc.interface_match_pattern_candidates(pattern) {
		name := tc.interface_metadata_name(candidate)
		if name in tc.interface_names {
			return name
		}
		typ := unalias_type(tc.parse_type(candidate))
		if typ is Interface {
			return typ.name
		}
	}
	return none
}

fn (tc &TypeChecker) interface_runtime_pattern_allowed(subject_iface string, target_iface string) bool {
	subject := tc.interface_metadata_name(subject_iface)
	target := tc.interface_metadata_name(target_iface)
	if subject == target {
		return true
	}
	if tc.interface_implements_interface(subject, target)
		|| tc.interface_implements_interface(target, subject) {
		return true
	}
	for concrete in tc.interface_impl_names(subject) {
		if concrete in tc.interface_names {
			if tc.interface_implements_interface(concrete, target) {
				return true
			}
			continue
		}
		resolved := tc.interface_metadata_name(concrete)
		if resolved in tc.interface_names {
			if tc.interface_implements_interface(resolved, target) {
				return true
			}
			continue
		}
		if tc.named_type_implements_interface(concrete, target) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) pattern_type_known(pattern string) bool {
	clean := trimmed_space(pattern)
	if clean.starts_with('[]') || clean.starts_with('map[') || clean.starts_with('[')
		|| clean.starts_with('fn ') || clean.starts_with('fn(') {
		return tc.parse_type(clean) !is Unknown
	}
	return false
}

fn (tc &TypeChecker) resolve_ierror_match_pattern(pattern string) ?string {
	for candidate in tc.interface_match_pattern_candidates(pattern) {
		if tc.named_type_compatible_with_ierror(candidate) {
			return candidate
		}
	}
	return none
}

fn (tc &TypeChecker) interface_match_pattern_candidates(pattern string) []string {
	mut candidates := []string{}
	if !pattern.contains('.') {
		mut has_scoped_candidate := false
		if resolved := tc.resolve_selective_import_type_symbol(pattern) {
			candidates << resolved
			has_scoped_candidate = true
		}
		if tc.source_declares_type_in_scope(pattern, tc.cur_file, tc.cur_module) {
			candidates << pattern
			has_scoped_candidate = true
		}
		if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
			local := '${tc.cur_module}.${pattern}'
			if tc.type_symbol_known(local) {
				candidates << local
				has_scoped_candidate = true
			}
		}
		if !has_scoped_candidate {
			candidates << pattern
		}
	} else if resolved := tc.resolve_import_alias_pattern(pattern) {
		candidates << resolved
		candidates << pattern
	} else {
		candidates << pattern
	}
	qpattern := tc.qualify_name(pattern)
	if qpattern != pattern {
		candidates << qpattern
	}
	mut result := []string{}
	mut seen := map[string]bool{}
	for candidate in candidates {
		if candidate.len == 0 || candidate in seen {
			continue
		}
		seen[candidate] = true
		result << candidate
	}
	return result
}

fn (tc &TypeChecker) resolve_import_alias_pattern(pattern string) ?string {
	dot := pattern.index_u8(`.`)
	if dot <= 0 {
		return none
	}
	alias := pattern[..dot]
	resolved := tc.resolve_import_alias(alias) or { return none }
	return '${resolved}.${pattern[dot + 1..]}'
}

// check_is_expr validates check is expr state for types.
fn (mut tc TypeChecker) check_is_expr(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	expr_id := tc.a.child(&node, 0)
	tc.check_node(expr_id)
	expr_node := tc.a.node(expr_id)
	if expr_node.is_mut && expr_node.kind == .ident && !tc.ident_is_mutable_lvalue(expr_node.value) {
		tc.record_error_at(.assignment_mismatch,
			'`${expr_node.value}` is immutable, declare it with `mut` to make it mutable', expr_id,
			tc.node_value_diagnostic_pos(expr_id))
	}
	if expr_node.kind == .none_expr && node.value == 'none' {
		mut op := 'is'
		mut op_pos := node.pos
		mut full_pos := node.pos
		if file := tc.a.source_files[expr_node.pos.id] {
			source := tc.source_texts_by_file[file.name] or { '' }
			search_start := int_min(int_max(expr_node.pos.end, 0), source.len)
			search_end := int_min(source.len, search_start + 16)
			if search_start < search_end {
				tail := source[search_start..search_end]
				relative := if idx := tail.index('!is') {
					op = '!is'
					idx
				} else {
					tail.index('is') or { -1 }
				}
				if relative >= 0 {
					op_start := search_start + relative
					op_pos = token.new_span(expr_node.pos.id, op_start, op_start + op.len)
					target_relative := source[op_start + op.len..search_end].index('none') or { -1 }
					if target_relative >= 0 {
						full_pos = token.new_span(expr_node.pos.id, expr_node.pos.offset,

							op_start + op.len + target_relative + 4)
					}
				}
			}
		}
		tc.record_error_at(.condition_mismatch, 'invalid operator `${op}` to `none` and `none`',
			id, full_pos)
		tc.record_error_at(.condition_mismatch,
			'`${op}` can only be used with interfaces and sum types', id, op_pos)
		return
	}
	if node.value.contains('.') {
		module_alias := node.value.all_before('.')
		if module_alias.len > 0 && module_alias[0] >= `a` && module_alias[0] <= `z`
			&& module_alias.bytes().all(it.is_letter() || it.is_digit() || it == `_`)
			&& module_alias != tc.cur_module
			&& tc.current_file_import_path_for_alias(module_alias) == none {
			tc.record_error_at(.unknown_type, 'unknown module `${module_alias}`', id, tc.type_diagnostic_pos(id,
				module_alias))
			return
		}
	}
	// `x is T` in a generic template stays undecided until monomorphization.
	// Only defer inside an actual generic function: in a non-generic one an
	// unknown single-letter pattern is a real error and must be validated.
	if node.value.len > 0 && tc.type_text_has_generic_placeholder(node.value)
		&& tc.cur_fn_is_generic_template() {
		return
	}
	// A `$for v in T.variants` loop variable is substituted by the comptime
	// unroll; `val is v` cannot be validated against the raw name.
	if node.value in tc.cur_comptime_variant_loop_vars || (node.value.contains('.')
		&& node.value.all_after_last('.') in tc.cur_comptime_variant_loop_vars) {
		return
	}
	raw_expr_type := tc.resolve_type(expr_id)
	if raw_expr_type is OptionType {
		tc.record_error_at(.condition_mismatch,
			'${tc.source_text_for_node(expr_id)} is an Optional, it needs to be unwrapped first',
			expr_id, tc.node_value_diagnostic_pos(expr_id))
		return
	}
	mut expr_type := unalias_type(unwrap_pointer(raw_expr_type))
	if expr_type is Interface && node.value != 'none'
		&& tc.nonmut_mutable_interface_smartcast(expr_id) {
		if tc.interface_has_no_requirements(expr_type.name) {
			tc.record_notice_at(.condition_mismatch,
				'smartcasting requires either an immutable value, or an explicit mut keyword before the value',
				expr_id, expr_node.pos)
		}
		tc.record_error_at(.condition_mismatch,
			'smart casting a mutable interface value requires `if mut ${tc.source_text_for_node(expr_id)} is ...`',
			expr_id, expr_node.pos)
	}
	// A previous branch can narrow a variable to one variant and then assign it
	// another value. A later `is` still applies to the variable's declared sum
	// type, not only to the stale narrowed variant.
	if expr_type !is SumType && expr_type !is Interface {
		mut declared_type := Type(Unknown{})
		if expr_node.kind == .ident {
			declared_type = tc.cur_scope.lookup(expr_node.value) or { Type(Unknown{}) }
		} else if expr_node.kind == .selector {
			declared_type = tc.selector_type(expr_id, expr_node) or { Type(Unknown{}) }
		}
		declared_type = unalias_and_unwrap_pointer_type(declared_type)
		if declared_type is SumType || declared_type is Interface {
			expr_type = declared_type
		}
	}
	if expr_type is SumType {
		if node.value.len > 0 {
			pointer_sum_variant := node.value.starts_with('&') && raw_expr_type is Pointer
				&& tc.sum_variant_type_for_pattern(expr_type.name, node.value[1..]) != none
			sum_base, _, concrete_sum := generic_type_application_parts(expr_type.name)
			open_generic_pattern := concrete_sum && node.value in tc.sum_params_for_base(sum_base)
			if (tc.sum_variant_type_for_pattern(expr_type.name, node.value) == none
				|| open_generic_pattern) && !pointer_sum_variant {
				if tc.should_diagnose(id) {
					diagnostic_pattern := if node.value.contains('.') {
						node.value
					} else if tc.cur_module !in ['', 'main', 'builtin'] {
						'${tc.cur_module}.${node.value}'
					} else {
						node.value
					}
					diagnostic_pos := if node.value.contains('.') {
						tc.type_diagnostic_pos(id, node.value.all_before('.'))
					} else {
						tc.node_value_diagnostic_pos(id)
					}
					if !tc.type_name_known(node.value) && !(tc.checker_fixture_mode
						&& node.value.len == 1 && node.value[0].is_capital()) {
						tc.record_error_at(.unknown_type,
							'is: type `${diagnostic_pattern}` does not exist', id, diagnostic_pos)
					}
					tc.record_error_at(.condition_mismatch, if tc.comptime_static_depth > 0 {
						'`${expr_type.name}` has no variant `${diagnostic_pattern}`'
					} else {
						'`${diagnostic_pattern}` is not a variant of sum type `${expr_type.name}`'
					}, id, diagnostic_pos)
				}
			}
		}
		return
	}
	if is_ierror_type(expr_type) {
		if node.value.len > 0 {
			if node.value == 'none' {
			} else if target_iface := tc.resolve_interface_pattern_interface(node.value) {
				if !tc.interface_implements_interface(target_iface, 'IError')
					&& tc.should_diagnose(id) {
					tc.record_error(.condition_mismatch,
						'`${node.value}` is not compatible with `IError`', id)
				}
			} else if _ := tc.resolve_ierror_match_pattern(node.value) {
			} else if concrete := tc.resolve_interface_match_pattern(node.value) {
				pos := tc.node_value_diagnostic_pos(id)
				concrete_type := tc.parse_type(concrete)
				tc.record_interface_implementation_error(.condition_mismatch, concrete_type, Interface{
					name: 'IError'
				}, id, pos)
				tc.record_error_at(.condition_mismatch,
					'`${node.value}` doesn\'t implement interface `IError`', id, pos)
			} else if tc.should_diagnose(id) {
				tc.record_error(.condition_mismatch,
					'`${node.value}` is not compatible with `IError`', id)
			}
		}
		return
	}
	if expr_type is Interface {
		if node.value.len > 0 {
			if interface_pattern_is_collapsed_container(node.value) {
				container_type := tc.parse_type(node.value)
				if !tc.type_implements_interface(container_type, expr_type)
					&& tc.should_diagnose(id) {
					tc.record_error(.condition_mismatch,
						'`${node.value}` is not compatible with interface `${expr_type.name}`', id)
				}
			} else if target_iface := tc.resolve_interface_pattern_interface(node.value) {
				if !tc.interface_runtime_pattern_allowed(expr_type.name, target_iface)
					&& tc.should_diagnose(id) {
					tc.record_error(.condition_mismatch,
						'`${node.value}` is not compatible with interface `${expr_type.name}`', id)
				}
			} else if concrete := tc.resolve_interface_match_pattern(node.value) {
				concrete_type := unalias_type(unwrap_pointer(tc.parse_type(concrete)))
				if concrete_type !is Interface
					&& !tc.named_type_implements_interface(concrete, expr_type.name)
					&& tc.should_diagnose(id) {
					pos := tc.node_value_diagnostic_pos(id)
					tc.record_error_at(.condition_mismatch,
						'`${node.value}` is not compatible with interface `${expr_type.name}`', id,
						pos)
				}
			} else if tc.should_diagnose(id) {
				tc.record_error(.condition_mismatch, 'unknown type `${node.value}`', id)
			}
		}
		return
	}
	if expr_type is Unknown {
		return
	}
	if tc.should_diagnose(id) {
		tc.record_error(.condition_mismatch,
			'`is` can only be used with sum type or interface values, not `${expr_type.name()}`',
			id)
	}
}

// branch_tail_type supports branch tail type handling for TypeChecker.
fn (tc &TypeChecker) branch_tail_type(id flat.NodeId) Type {
	if !tc.valid_node_id(id) {
		return Type(void_)
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .if_expr {
		return tc.if_expr_tail_type(id)
	}
	if node.kind == .block {
		if node.children_count == 0 {
			return Type(void_)
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return Type(void_)
		}
		last := tc.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			return tc.branch_tail_value_type(tc.a.child(&last, 0))
		}
		return tc.branch_tail_value_type(last_id)
	}
	if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return Type(void_)
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return Type(void_)
		}
		last := tc.a.nodes[int(last_id)]
		if last.kind == .expr_stmt && last.children_count > 0 {
			return tc.branch_tail_value_type(tc.a.child(&last, 0))
		}
		return tc.branch_tail_value_type(last_id)
	}
	return tc.branch_tail_value_type(id)
}

fn (tc &TypeChecker) branch_tail_value_type(id flat.NodeId) Type {
	if smart_type := tc.smartcast_type(id) {
		return smart_type
	}
	typ := tc.expr_type(id) or { tc.resolve_type(id) }
	if tc.valid_node_id(id) {
		node := tc.a.node(id)
		if node.kind == .ident {
			if base := tc.mut_param_base_for_current_ident(node.value, typ) {
				return base
			}
		}
	}
	return typ
}

fn (tc &TypeChecker) branch_tail_type_with_smartcasts(id flat.NodeId, smartcasts []LocalBinding) Type {
	if smartcasts.len == 0 {
		return tc.branch_tail_type(id)
	}
	mut scoped := tc.fork_smartcast_query_view()
	for sc in smartcasts {
		if valid_string_data(sc.name) {
			scoped.smartcasts[sc.name] = sc.typ
		}
	}
	return scoped.branch_tail_type(id)
}

// if_expr_tail_type supports if expr tail type handling for TypeChecker.
fn (tc &TypeChecker) if_expr_tail_type(id flat.NodeId) Type {
	mut cur_id := id
	mut result := Type(void_)
	for tc.valid_node_id(cur_id) {
		node := tc.a.nodes[int(cur_id)]
		if node.kind != .if_expr {
			return tc.choose_if_tail_type(result, tc.branch_tail_type(cur_id))
		}
		if node.children_count > 1 {
			smartcasts := tc.extract_smartcasts(tc.a.child(&node, 0))
			then_type := tc.branch_tail_type_with_smartcasts(tc.a.child(&node, 1), smartcasts)
			result = tc.choose_if_tail_type(result, then_type)
		}
		if node.children_count <= 2 {
			return result
		}
		else_id := tc.a.child(&node, 2)
		if !tc.valid_node_id(else_id) {
			return result
		}
		else_node := tc.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			cur_id = else_id
			continue
		}
		else_type := tc.branch_tail_type(else_id)
		return tc.choose_if_tail_type(result, else_type)
	}
	return result
}

// match_expr_tail_type supports match expression value type handling for TypeChecker.
fn (tc &TypeChecker) match_expr_tail_type(id flat.NodeId) Type {
	if !tc.valid_node_id(id) {
		return Type(void_)
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .match_stmt || node.children_count < 2 {
		return Type(void_)
	}
	subject_id := tc.a.child(&node, 0)
	subject_key := tc.expr_key(subject_id)
	subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
	mut result := Type(void_)
	mut incompatible_wrapper_void := Type(void_)
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		if !tc.valid_node_id(branch_id) {
			continue
		}
		branch := tc.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			continue
		}
		branch_type := if subject_key.len > 0 && valid_string_data(subject_key) {
			// Only the `smartcasts` binding needs isolation here (see
			// branch_tail_never_returns); avoid the ~11KB full struct copy.
			mut mtc := unsafe { &TypeChecker(voidptr(tc)) }
			mut saved_smartcasts := mtc.smartcasts.move()
			mtc.smartcasts = clone_smartcasts(saved_smartcasts)
			mtc.apply_match_branch_context_smartcasts(subject_key, subject_type, branch)
			bt := tc.branch_tail_type(branch_id)
			mtc.smartcasts = saved_smartcasts.move()
			bt
		} else {
			tc.branch_tail_type(branch_id)
		}
		tail := tc.branch_tail_expr_id(branch_id)
		if (is_option_void_type(branch_type) && !tc.branch_tail_is_none_literal(tail))
			|| (is_result_void_type(branch_type) && !tc.branch_tail_is_error_literal(tail)) {
			incompatible_wrapper_void = branch_type
		}
		result = tc.choose_if_tail_type(result, branch_type)
	}
	if incompatible_wrapper_void !is Void {
		return incompatible_wrapper_void
	}
	return result
}

// choose_if_tail_type supports choose if tail type handling for types.
fn (tc &TypeChecker) choose_if_tail_type(current Type, next Type) Type {
	if current is Void {
		return next
	}
	if next is Void {
		return current
	}
	if inferred := inferred_contextual_if_type(current, next) {
		return inferred
	}
	if current is SumType && tc.sum_variant_type_for_pattern(current.name, next.name()) != none {
		return current
	}
	if next is SumType && tc.sum_variant_type_for_pattern(next.name, current.name()) != none {
		return next
	}
	if current !is Primitive {
		return current
	}
	if next !is Primitive {
		return next
	}
	return current
}

// branch_tail_expr_id returns the value-producing tail expression of a branch
// body (a `block` or `match_branch`), unwrapping a trailing `expr_stmt`. Returns
// `empty_node` when the branch has no expression tail.
fn (tc &TypeChecker) branch_tail_expr_id(id flat.NodeId) flat.NodeId {
	if !tc.valid_node_id(id) {
		return flat.empty_node
	}
	node := tc.a.nodes[int(id)]
	mut last_id := flat.empty_node
	if node.kind == .block {
		if node.children_count == 0 {
			return flat.empty_node
		}
		last_id = tc.a.child(&node, node.children_count - 1)
	} else if node.kind == .match_branch {
		body_start := if node.value == 'else' { 0 } else { node.value.int() }
		if node.children_count <= body_start {
			return flat.empty_node
		}
		last_id = tc.a.child(&node, node.children_count - 1)
	} else {
		return id
	}
	if !tc.valid_node_id(last_id) {
		return flat.empty_node
	}
	last := tc.a.nodes[int(last_id)]
	if last.kind == .expr_stmt {
		if last.children_count > 0 {
			return tc.a.child(&last, 0)
		}
		return flat.empty_node
	}
	if last.kind == .block {
		return tc.branch_tail_expr_id(last_id)
	}
	return last_id
}

// branches_compatible_with propagates `expected` into every value-producing tail
// of a match/if expression (so context-dependent tails such as enum shorthand
// `.foo`, `none`, or fn literals type against it instead of defaulting to e.g.
// `int`). Returns true when the node is a match/if expression and every branch
// tail is compatible with `expected`.
fn (mut tc TypeChecker) branches_compatible_with(id flat.NodeId, expected Type) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .match_stmt {
		subject_id := tc.a.child(&node, 0)
		subject_key := tc.expr_key(subject_id)
		subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
		mut saw_branch := false
		for i in 1 .. node.children_count {
			branch_id := tc.a.child(&node, i)
			if !tc.valid_node_id(branch_id) {
				continue
			}
			branch := tc.a.nodes[int(branch_id)]
			if branch.kind != .match_branch {
				continue
			}
			tail := tc.branch_tail_expr_id(branch_id)
			if !tc.valid_node_id(tail) {
				return false
			}
			saw_branch = true
			if !tc.branch_failure_literal_matches_context(tail, expected) {
				return false
			}
			saved_smartcasts := clone_smartcasts(tc.smartcasts)
			tc.apply_match_branch_context_smartcasts(subject_key, subject_type, branch)
			actual := tc.resolve_expr(tail, expected)
			tc.smartcasts = clone_smartcasts(saved_smartcasts)
			if !tc.if_branch_type_compatible_with_context(actual, tail, expected) {
				return false
			}
		}
		return saw_branch
	}
	if node.kind == .if_expr {
		// A value if-expression needs an else branch (child 2). children: cond,
		// then-block, else (block or nested if_expr).
		if node.children_count <= 2 {
			return false
		}
		if tc.constant_if_selected_branch_compatible_with_expected(node, expected) {
			return true
		}
		then_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
		if !tc.valid_node_id(then_tail) {
			return false
		}
		if !tc.branch_failure_literal_matches_context(then_tail, expected) {
			return false
		}
		then_actual := tc.resolve_expr(then_tail, expected)
		if !tc.if_branch_type_compatible_with_context(then_actual, then_tail, expected) {
			return false
		}
		else_id := tc.a.child(&node, 2)
		if !tc.valid_node_id(else_id) {
			return false
		}
		if tc.a.nodes[int(else_id)].kind == .if_expr {
			return tc.branches_compatible_with(else_id, expected)
		}
		else_tail := tc.branch_tail_expr_id(else_id)
		if !tc.valid_node_id(else_tail) {
			return false
		}
		if !tc.branch_failure_literal_matches_context(else_tail, expected) {
			return false
		}
		else_actual := tc.resolve_expr(else_tail, expected)
		return tc.if_branch_type_compatible_with_context(else_actual, else_tail, expected)
	}
	return false
}

fn (mut tc TypeChecker) apply_match_branch_context_smartcasts(subject_key string, subject_type Type, branch flat.Node) {
	if subject_key.len == 0 || !valid_string_data(subject_key) || branch.kind != .match_branch {
		return
	}
	n_conds := if branch.value == 'else' { 0 } else { branch.value.int() }
	if n_conds != 1 || branch.children_count == 0 {
		return
	}
	cond := tc.a.node(tc.a.child(&branch, 0))
	pattern := tc.match_type_pattern(cond) or { return }
	smartcast_type := if subject_type is SumType {
		tc.sum_variant_type_for_pattern(subject_type.name, pattern) or { pattern }
	} else if is_ierror_type(subject_type) {
		tc.resolve_ierror_match_pattern(pattern) or { pattern }
	} else if subject_type is Interface {
		tc.resolve_interface_match_pattern(pattern) or { pattern }
	} else {
		return
	}
	tc.smartcasts[subject_key] = tc.parse_type(smartcast_type)
}

// extract_smartcasts supports extract smartcasts handling for TypeChecker.
fn (tc &TypeChecker) extract_smartcasts(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .paren && cond.children_count > 0 {
		return tc.extract_smartcasts(tc.a.child(&cond, 0))
	}
	if cond.kind == .ident {
		if key := tc.visible_binding_storage_key(cond.value) {
			if source_id := tc.fn_context.bool_condition_exprs[key] {
				if source_id != cond_id {
					return tc.extract_smartcasts(source_id)
				}
			}
		}
	}
	if cond.kind == .is_expr && cond.children_count > 0 {
		expr_id := tc.a.child(&cond, 0)
		key := tc.expr_key(expr_id)
		if key.len > 0 && valid_string_data(key) && cond.value.len > 0 {
			mut result := []LocalBinding{}
			result << LocalBinding{
				name: key
				typ:  tc.smartcast_target_type_for_is_expr(expr_id, cond.value)
			}
			return result
		}
	}
	// `x != none` (or `!= nil` for `?&T`) unwraps the option expr inside the then-branch.
	if cond.kind == .infix && cond.op == .ne && cond.children_count >= 2 {
		if binding := tc.option_none_cmp_binding(cond) {
			return [binding]
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		mut result := tc.extract_smartcasts(tc.a.child(&cond, 0))
		result << tc.extract_smartcasts(tc.a.child(&cond, 1))
		return result
	}
	return []LocalBinding{}
}

fn (tc &TypeChecker) nonmut_mutable_interface_smartcast(expr_id flat.NodeId) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	if tc.expr_has_explicit_mut_marker(expr_id) {
		return false
	}
	root_id := tc.lvalue_root_ident(expr_id) or { return false }
	root := tc.a.node(root_id)
	if root.kind != .ident || !tc.ident_is_mutable_lvalue(root.value) {
		return false
	}
	declared := unalias_and_unwrap_pointer_type(tc.cur_scope.lookup(root.value) or { return false })
	return declared is Interface
}

// expr_has_explicit_mut_marker reports whether `mut` was written before an
// lvalue. The parser sees the prefix before postfix selectors and indexes are
// attached, so the marker can live on the root identifier rather than the
// completed expression node.
fn (tc &TypeChecker) expr_has_explicit_mut_marker(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	if tc.a.node(id).is_mut {
		return true
	}
	root_id := tc.lvalue_root_ident(id) or { return false }
	return tc.valid_node_id(root_id) && tc.a.node(root_id).is_mut
}

fn (tc &TypeChecker) struct_init_is_interface_return(id flat.NodeId) bool {
	if unalias_and_unwrap_pointer_type(tc.fn_context.return_type) !is Interface {
		return false
	}
	mut current := id
	for _ in 0 .. 128 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .return_stmt {
			return parent.children_count == 1
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		current = parent_id
	}
	return false
}

// option_none_cmp_binding matches a `x != none` / `x == none` (also `nil`)
// comparison where x is an option and returns the unwrapped binding for x.
fn (tc &TypeChecker) option_none_cmp_binding(cond flat.Node) ?LocalBinding {
	lhs_id := tc.a.child(&cond, 0)
	rhs_id := tc.a.child(&cond, 1)
	lhs := tc.a.nodes[int(lhs_id)]
	rhs := tc.a.nodes[int(rhs_id)]
	mut opt_id := flat.NodeId(-1)
	if rhs.kind == .none_expr || rhs.kind == .nil_literal {
		opt_id = lhs_id
	} else if lhs.kind == .none_expr || lhs.kind == .nil_literal {
		opt_id = rhs_id
	}
	if int(opt_id) < 0 {
		return none
	}
	key := tc.expr_key(opt_id)
	if key.len == 0 || !valid_string_data(key) {
		return none
	}
	mut opt_type := tc.expr_type(opt_id) or { tc.resolve_type(opt_id) }
	opt_node := tc.a.node(opt_id)
	if opt_node.kind == .selector {
		if declared := tc.selector_declared_value_type(*opt_node) {
			opt_type = declared
		}
	} else if opt_node.kind == .ident {
		if mut_base := tc.mut_param_base_for_current_ident(opt_node.value, opt_type) {
			opt_type = mut_base
		}
	}
	if opt_type is OptionType {
		base := opt_type.base_type
		if base !is Void && base !is Unknown {
			return LocalBinding{
				name: key
				typ:  base
			}
		}
	}
	return none
}

// extract_else_branch_smartcasts returns bindings that apply to the ELSE branch:
// `if x == none { ... } else { <x unwrapped here> }`.
fn (tc &TypeChecker) extract_else_branch_smartcasts(cond_id flat.NodeId) []LocalBinding {
	if int(cond_id) < 0 {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind == .paren && cond.children_count > 0 {
		return tc.extract_else_branch_smartcasts(tc.a.child(&cond, 0))
	}
	if cond.kind == .ident {
		if key := tc.visible_binding_storage_key(cond.value) {
			if source_id := tc.fn_context.bool_condition_exprs[key] {
				if source_id != cond_id {
					return tc.extract_else_branch_smartcasts(source_id)
				}
			}
		}
	}
	if binding := tc.negated_is_smartcast(cond_id) {
		return [binding]
	}
	if cond.kind == .infix && cond.op == .eq && cond.children_count >= 2 {
		if binding := tc.option_none_cmp_binding(cond) {
			return [binding]
		}
	}
	if cond.kind == .infix && cond.op == .logical_or && cond.children_count >= 2 {
		rhs_id := tc.a.child(&cond, 1)
		mut result := []LocalBinding{}
		for binding in tc.extract_else_branch_smartcasts(tc.a.child(&cond, 0)) {
			// The right operand is evaluated after the left, so if it can write the
			// binding's storage (for example `!(x is Foo) || retag_false(mut x)`), the
			// reconstructed left-side narrowing is stale once the else branch runs.
			if !tc.subtree_assigns_key(rhs_id, binding.name) {
				result << binding
			}
		}
		result << tc.extract_else_branch_smartcasts(rhs_id)
		return result
	}
	return []LocalBinding{}
}

// check_struct_init validates check struct init state for types.
fn (mut tc TypeChecker) check_struct_init(id flat.NodeId, node flat.Node) {
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .directive && child.value.starts_with('embed_invalid_compression:') {
			compression_type := child.value.all_after(':')
			tc.record_error_at(.unknown_type,
				'not supported compression type: .${compression_type}. supported: .none, .zlib',
				child_id, child.pos)
			return
		}
	}
	if node.value == 'any' {
		tc.record_error(.unknown_type, 'cannot use type `any` here', id)
		return
	}
	if node.value.starts_with('chan ') {
		elem_type := trimmed_space(node.value[5..])
		if should_check_named_type(elem_type) && !tc.type_name_known(elem_type) {
			tc.record_unknown_decl_type(elem_type, id)
		}
		tc.remember_expr_type(id, tc.parse_type(node.value))
		return
	}
	is_optional_init := node.value.starts_with('?')
	init_type_text := if is_optional_init { node.value[1..] } else { node.value }
	raw_source_type_text := tc.source_text_for_node(id).all_before('{').trim_space().trim_left('?')
	// Struct literals in select send conditions can start their span one byte after the
	// qualified type. Repair that narrow parser offset without treating synthesized
	// qualified types (for example `$embed_file`) as source module references.
	source_type_text := if raw_source_type_text.len > 0
		&& init_type_text.len == raw_source_type_text.len + 1
		&& init_type_text.ends_with(raw_source_type_text) {
		init_type_text
	} else {
		raw_source_type_text
	}
	if source_type_text.contains('.') && !source_type_text.starts_with('C.') {
		module_alias := source_type_text.all_before('.')
		if module_alias.len > 0 && module_alias[0] >= `a` && module_alias[0] <= `z`
			&& module_alias.bytes().all(it.is_letter() || it.is_digit() || it == `_`)
			&& module_alias != tc.cur_module
			&& tc.current_file_import_path_for_alias(module_alias) == none {
			tc.record_error_at(.unknown_type, 'unknown module `${module_alias}`', id, tc.type_diagnostic_pos(id,
				module_alias))
			for i in 0 .. node.children_count {
				tc.check_node(tc.a.child(&node, i))
			}
			tc.register_synth_type(id, Type(void_))
			return
		}
	}
	parsed_init_type := tc.parse_type(init_type_text)
	clean_parsed_init_type := unalias_type(parsed_init_type)
	if clean_parsed_init_type is FnType {
		tc.record_error_at(.unknown_type,
			'functions must be defined, not instantiated like structs', id, node.pos)
		qualified_name := tc.qualify_name(init_type_text)
		if visibility := tc.declaration_visibility[qualified_name] {
			if !visibility.is_pub {
				tc.record_error_at(.unknown_type, 'type `${init_type_text}` is private', id,
					node.pos)
			}
		}
		tc.remember_expr_type(id, parsed_init_type)
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		return
	}
	if clean_parsed_init_type is Array || clean_parsed_init_type is ArrayFixed {
		elem_type := array_like_elem_type(clean_parsed_init_type) or { Type(void_) }
		init_expr_type := if is_optional_init {
			Type(OptionType{
				base_type: parsed_init_type
			})
		} else {
			parsed_init_type
		}
		tc.remember_expr_type(id, init_expr_type)
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .field_init || field.children_count == 0 {
				continue
			}
			value_id := tc.a.child(field, 0)
			expected := if field.value in ['len', 'cap'] {
				Type(int_)
			} else if field.value in ['', 'init'] {
				elem_type
			} else {
				tc.record_error_at(.unknown_field,
					'wrong field `${field.value}`, expecting `len`, `cap`, or `init`',
					tc.a.child(&node, i), tc.node_value_diagnostic_pos(tc.a.child(&node, i)))
				continue
			}
			tc.check_node_with_expected_context(value_id, expected)
			actual := tc.resolve_expr(value_id, expected)
			if actual is Unknown || tc.expr_compatible(value_id, actual, expected) {
				continue
			}
			tc.record_error_at(.assignment_mismatch, 'invalid array element: expected `${expected.name()}`, not `${tc.diagnostic_expr_type_name(value_id,
				actual)}`', value_id, tc.array_element_diagnostic_pos(value_id))
		}
		return
	}
	generic_base, generic_args, has_generic_args := generic_type_application_parts(init_type_text)
	if has_generic_args {
		qualified_base := tc.qualify_name(generic_base)
		if (generic_base in tc.structs || qualified_base in tc.structs)
			&& generic_base !in tc.struct_generic_params
			&& qualified_base !in tc.struct_generic_params {
			if generic_args.any(is_bare_generic_param(it)) {
				tc.record_error_at(.unsupported_generic,
					'a non generic struct `${generic_base.all_after_last('.')}` used like a generic struct',
					id, tc.type_diagnostic_pos(id, generic_base))
			} else {
				tc.record_error_at(.unsupported_generic,
					'struct `${generic_base.all_after_last('.')}` is not a generic struct, cannot instantiate to the concrete types',
					id, tc.explicit_generic_args_diagnostic_pos(id))
			}
			tc.register_synth_type(id, tc.parse_type(generic_base))
			for i in 0 .. node.children_count {
				tc.check_node(tc.a.child(&node, i))
			}
			return
		}
		params := tc.struct_generic_params[generic_base] or {
			tc.struct_generic_params[qualified_base] or { []string{} }
		}
		if params.len > 0 && generic_args.len != params.len {
			message := if tc.fn_context.generic_params.len > 0 {
				'generic struct init expects ${params.len} generic parameter, but got ${generic_args.len}'
			} else {
				'the number of generic types of struct `${generic_base.all_after_last('.')}` is inconsistent with the concrete types'
			}
			pos := if tc.fn_context.generic_params.len > 0 {
				tc.struct_init_head_pos(node)
			} else {
				tc.explicit_generic_args_diagnostic_pos(id)
			}
			tc.record_error_at(.unsupported_generic, message, id, pos)
			tc.register_synth_type(id, tc.parse_type(generic_base))
			for i in 0 .. node.children_count {
				tc.check_node(tc.a.child(&node, i))
			}
			return
		}
		if params.len > 0 && tc.fn_context.generic_params.len > 0 {
			for arg in generic_args {
				if is_bare_generic_param(arg) && arg !in tc.fn_context.generic_params {
					current := '(${tc.fn_context.generic_params.join(',')})'
					tc.record_error_at(.unsupported_generic,
						'generic struct init type parameter `${arg}` must be within the parameters `${current}` of the current generic function',
						id, tc.struct_init_head_pos(node))
					tc.register_synth_type(id, tc.parse_type(generic_base))
					for i in 0 .. node.children_count {
						tc.check_node(tc.a.child(&node, i))
					}
					return
				}
			}
		}
	}
	if init_type_text != 'struct' && !is_anonymous_struct_name(init_type_text)
		&& (!tc.type_name_known(init_type_text) || (init_type_text.starts_with('C.')
		&& !tc.type_symbol_known(init_type_text))) {
		if is_bare_generic_param(init_type_text)
			&& tc.unmentioned_generic_type_was_reported(init_type_text, id) {
			for i in 0 .. node.children_count {
				tc.check_node(tc.a.child(&node, i))
			}
			tc.remember_expr_type(id, unknown_type('unmentioned generic `${init_type_text}`'))
			return
		}
		if init_type_text.starts_with('C.') {
			tc.record_error_at(.unknown_type, 'unknown type `${node.value}`', id, node.pos)
		} else if tc.struct_init_has_positional_fields(node) {
			tc.record_error_at(.unknown_type, 'unknown type `${node.value}`', id, node.pos)
		} else if init_type_text.contains('.')
			&& tc.current_file_import_path_for_alias(init_type_text.all_before('.')) != none {
			mut message := tc.unknown_type_message(init_type_text, id)
			if !message.contains('\nDid you mean ') && tc.expected_expr_type !is Unknown
				&& tc.expected_expr_type !is Void {
				message += '.\nDid you mean `${tc.expected_expr_type.name()}`?'
			}
			tc.record_error_at(.unknown_type, message, id,
				tc.unknown_qualified_struct_init_pos(node))
		} else if candidates := tc.selective_import_candidates(init_type_text) {
			if candidates.len == 1 {
				tc.record_error_at(.unknown_type, tc.unknown_type_message(candidates[0], id), id,
					node.pos)
			} else {
				tc.record_error(.unknown_type, 'unknown struct `${node.value}`', id)
			}
		} else {
			tc.record_error(.unknown_type, 'unknown struct `${node.value}`', id)
		}
		tc.record_invalid_struct_init_print_error(id)
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		recovery_type := if tc.expected_expr_type !is Unknown && tc.expected_expr_type !is Void {
			tc.expected_expr_type
		} else {
			Type(void_)
		}
		tc.register_synth_type(id, recovery_type)
		return
	}
	mut init_type := tc.parse_type(init_type_text)
	if is_contextual_anonymous_struct_literal(init_type_text) && tc.expected_expr_id >= 0
		&& tc.expr_is_value_tail_of(flat.NodeId(tc.expected_expr_id), id) {
		expected := unalias_type(tc.expected_expr_type)
		if expected is Struct && is_anonymous_struct_name(expected.name) {
			init_type = expected
			tc.remember_expr_type(id, expected)
		}
	}
	if is_optional_init {
		tc.remember_expr_type(id, Type(OptionType{
			base_type: init_type
		}))
	}
	if unalias_type(init_type) is Enum {
		tc.record_error_at(.assignment_mismatch, 'cannot initialize enums', id, node.pos)
		return
	}
	clean_init_type := unalias_type(init_type)
	if clean_init_type is Interface {
		if node.children_count == 0 && tc.interface_has_no_requirements(clean_init_type.name) {
			tc.register_synth_type(id, init_type)
			return
		}
		tc.record_error_at(.assignment_mismatch,
			'cannot instantiate interface `${clean_init_type.name.all_after_last('.')}`', id,
			node.pos)
		return
	}
	if init_struct := struct_type_from_type(init_type) {
		is_synthetic_embed_file := node.value == 'embed_file.EmbedFileData'
		if _ := tc.private_declaration(init_struct.name) {
			inside_module := if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
			tc.record_error_at(.unknown_type,
				'struct `${init_struct.name}` was declared as private to module `${init_struct.name.all_before_last('.')}`, so it can not be used inside module `${inside_module}`',
				id, node.pos)
			tc.record_error_at(.unknown_type, 'type `${init_struct.name}` is private', id, node.pos)
		}
		if deprecation := tc.deprecated_symbols[init_struct.name] {
			tc.record_deprecation(id, 'struct', deprecation, tc.struct_init_deprecation_pos(node))
		}
		if init_type !is Alias {
			if inferred_type := tc.infer_generic_struct_init_type(node) {
				tc.remember_expr_type(id, inferred_type)
			} else if generic_name := tc.bare_generic_decl_type_name(init_type_text) {
				qualified := tc.qualify_name(generic_name)
				params := tc.struct_generic_params[generic_name] or {
					tc.struct_generic_params[qualified] or { []string{} }
				}
				if params.len > 0
					&& !tc.struct_init_has_related_bare_generic_return_error(id, generic_name) {
					inferred := tc.infer_generic_struct_init_param_texts(node, generic_name, params)
					mut missing_param := params[0]
					for param in params {
						if param !in inferred {
							missing_param = param
							break
						}
					}
					tc.record_error_at(.unknown_type,
						'could not infer generic type `${missing_param}` in generic struct `${generic_name}[${params.join(', ')}]`',
						id, node.pos)
				}
			}
		}
		init_name := tc.struct_init_field_lookup_name(init_type_text, init_struct.name)
		fields := tc.struct_fields_for_init(init_name)
		mut positional_fields := 0
		mut named_fields := 0
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .field_init {
				continue
			}
			if field.value.len == 0 {
				positional_fields++
			} else {
				named_fields++
			}
		}
		if positional_fields > 0 && named_fields == 0 && positional_fields != fields.len {
			amount := if positional_fields < fields.len { 'few' } else { 'many' }
			tc.record_error_at(.assignment_mismatch,
				'too ${amount} fields in `${init_type_text.all_after_last('.')}` literal (expecting ${fields.len}, got ${positional_fields})',
				id, node.pos)
		}
		if unknown_message := tc.generic_struct_field_unknown_type_message(init_name) {
			tc.record_error_at(.unknown_type, unknown_message, id, tc.struct_init_head_pos(node))
		}
		mut supplied_fields := map[string]bool{}
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .field_init {
				continue
			}
			if field.value.len > 0 {
				supplied_fields[field.value] = true
			} else if i < fields.len {
				supplied_fields[fields[i].name] = true
			}
		}
		tc.check_union_struct_init_fields(id, node, init_name, supplied_fields)
		for i in 0 .. node.children_count {
			raw_spread_id := tc.a.child(&node, i)
			spread := tc.a.child_node(&node, i)
			spread_id := if spread.kind == .prefix && spread.value == '...'
				&& spread.children_count > 0 {
				tc.a.child(spread, 0)
			} else if tc.node_has_ellipsis_prefix(raw_spread_id) {
				raw_spread_id
			} else {
				continue
			}
			spread_type := tc.resolve_type(spread_id)
			spread_struct := struct_type_from_type(unwrap_pointer(spread_type)) or {
				tc.record_error_at(.assignment_mismatch,
					'expected struct, found `${spread_type.name()}`', spread_id,
					tc.a.node(spread_id).pos)
				continue
			}
			spread_fields := tc.struct_fields_for_init(spread_struct.name)
			mut compatible := true
			for target_field in fields {
				if supplied_fields[target_field.name] {
					continue
				}
				mut found := false
				for source_field in spread_fields {
					if source_field.name == target_field.name
						&& tc.type_compatible(source_field.typ, target_field.typ)
						&& tc.type_compatible(target_field.typ, source_field.typ) {
						found = true
						break
					}
				}
				if !found {
					compatible = false
					break
				}
			}
			if !compatible {
				tc.record_error_at(.assignment_mismatch,
					'struct `${spread_struct.name.all_after_last('.')}` is not compatible with struct `${init_struct.name.all_after_last('.')}`',
					spread_id, tc.a.node(spread_id).pos)
			}
			for source_field in spread_fields {
				supplied_fields[source_field.name] = true
			}
		}
		for field in fields {
			if supplied_fields[field.name] || field.has_default || field.typ is OptionType {
				continue
			}
			context_type := tc.expected_context_for_expr(id) or { Type(void_) }
			context_expects_interface := unalias_and_unwrap_pointer_type(context_type) is Interface
			if unalias_type(field.typ) is Interface && !context_expects_interface
				&& !tc.struct_init_is_interface_return(id) {
				tc.record_notice_at(.assignment_mismatch,
					'interface field `${init_struct.name}.${field.name}` must be initialized', id,
					tc.struct_init_head_pos(node))
			}
		}
		for missing in tc.missing_required_struct_fields(init_name, supplied_fields, []string{}) {
			tc.record_error_at(.assignment_mismatch, 'field `${missing}` must be initialized', id,
				tc.struct_init_head_pos(node))
		}
		mut seen_missing_references := map[string]bool{}
		// Match V1's generic recheck behavior: a concrete generic struct literal can
		// acquire pointer fields only after substituting its type arguments, so those
		// fields retain their zero/default initialization unless explicitly supplied.
		if !init_type_text.contains('[') {
			for missing in tc.missing_reference_struct_fields(init_name, supplied_fields,
				[]string{}) {
				if seen_missing_references[missing.path] {
					continue
				}
				seen_missing_references[missing.path] = true
				message := if missing.has_part {
					'reference field `${missing.path}` must be initialized (part of struct `${missing.owner}`)'
				} else {
					'reference field `${missing.path}` must be initialized'
				}
				tc.record_error_at(.assignment_mismatch, message, id, tc.struct_init_head_pos(node))
			}
		}
		for i in 0 .. node.children_count {
			field_id := tc.a.child(&node, i)
			field := tc.a.nodes[int(field_id)]
			if field.kind != .field_init || field.children_count == 0 {
				tc.check_node(field_id)
				continue
			}
			if deprecation := tc.deprecated_symbols['${init_name}.${field.value}'] {
				tc.record_deprecation(field_id, 'field', deprecation,
					tc.struct_init_field_deprecation_pos(field))
			}
			value_id := tc.a.child(&field, 0)
			mut expected := Type(void_)
			if field.value.len > 0 {
				mut found := false
				if typ := tc.struct_field_type(init_name, field.value) {
					expected = typ
					found = true
				}
				if found && tc.unsafe_depth == 0 && !is_synthetic_embed_file
					&& tc.resolve_selective_import_type_symbol(init_type_text) == none {
					owner_base := strip_generic_args_name(init_name)
					decl_mod := tc.struct_modules[owner_base] or { '' }
					same_main_module := decl_mod in ['', 'main'] && tc.cur_module in ['', 'main']
					if decl_mod.len > 0 && decl_mod != tc.cur_module && !same_main_module
						&& !is_anonymous_struct_name(init_name) {
						is_public := tc.visible_mutation_struct_field_is_public(init_name,
							field.value, decl_mod) or { true }
						if !is_public {
							tc.record_error_at(.unknown_field,
								'cannot access private field `${field.value}` on `${init_type_text}`',
								field_id, tc.struct_init_field_deprecation_pos(field))
						}
					}
				}
				if !found && tc.should_diagnose(field_id) && fields.len > 0 {
					tc.record_error_at(.unknown_field, tc.struct_literal_unknown_field_message(init_name,
						field.value, fields), field_id, tc.struct_init_field_deprecation_pos(field))
				}
			} else if i < fields.len {
				expected = fields[i].typ
			}
			field_is_mut := struct_init_field_is_mut(fields, field, i)
			source_actual := if expected !is Void {
				tc.resolve_type(value_id)
			} else {
				Type(void_)
			}
			if expected !is Pointer && tc.type_has_declaration_attribute(expected, 'nocopy') {
				tc.record_error_at(.assignment_mismatch,
					'cannot copy @[nocopy] struct: use a reference instead', value_id,
					tc.a.node(value_id).pos)
			}
			if expected is Pointer {
				tc.record_non_heap_pointer_param_escape(value_id)
			}
			// A method value stored in a struct field escapes the evaluation site (several
			// instances from the same `Foo{cb: obj.method}` site would share one receiver).
			if !tc.stored_method_value_matches_voidptr_callback(value_id, expected) {
				tc.reject_stored_method_value(value_id)
			}
			tc.reject_stored_or_returned_capturing_fn_literal(value_id)
			if expected !is Void {
				warning_pos := if field.value.len > 0 {
					tc.struct_init_field_deprecation_pos(field)
				} else {
					tc.a.nodes[int(value_id)].pos
				}
				tc.warn_if_integer_literal_outside_known_type_range(value_id, expected, warning_pos)
				tc.check_node_with_expected_context(value_id, expected)
			} else {
				tc.check_node(value_id)
			}
			$if ownership ? {
				if !tc.ownership_aggregate_consumption_deferred(id) {
					tc.ownership_consume_expr(value_id, 'struct field', value_id)
				}
			}
			value_node := tc.a.node(value_id)
			if type_is_unsigned_integer(expected) && tc.expr_is_negative_integer_literal(value_id) {
				tc.record_error_at(.assignment_mismatch,
					'cannot assign negative value to unsigned integer type', value_id,
					value_node.pos)
			}
			if tc.unsafe_depth == 0 && !tc.translated_files[tc.cur_file]
				&& unalias_type(expected) is Pointer && value_node.kind == .int_literal
				&& value_node.value == '0' && expected.name() != 'voidptr' {
				pos := if field.value.len > 0 {
					tc.struct_init_field_value_pos(field, value_id)
				} else {
					value_node.pos
				}
				tc.record_error_at(.assignment_mismatch,
					'assigning `0` to a reference field is only allowed in `unsafe` blocks',
					field_id, pos)
			}
			if tc.unsafe_depth == 0 && unalias_type(expected) is Array
				&& (field_is_mut || tc.slice_expr_base_is_mutable(value_id)) {
				tc.record_implicit_slice_clone_notice(value_id)
			}
			if tc.unsafe_depth == 0 && field_is_mut && unalias_type(expected) is Array
				&& value_node.kind == .ident && tc.const_key_for_name(value_node.value) != none {
				tc.record_error_at(.assignment_mismatch,
					'cannot assign a const array to mut struct field, call `clone` method (or use `unsafe`)',
					value_id, value_node.pos)
			}
			if tc.unsafe_depth == 0 && field_is_mut && unalias_type(expected) is Map
				&& value_node.kind == .ident && tc.const_key_for_name(value_node.value) != none {
				tc.record_error_at(.assignment_mismatch,
					'cannot assign a const map to mut struct field, call `clone` method (or use a reference)',
					value_id, value_node.pos)
			}
			if tc.unsafe_depth == 0 && field_is_mut {
				if addressed_id := tc.addressed_ident(value_id) {
					addressed := tc.a.node(addressed_id)
					addressed_type := tc.non_file_scope_type(addressed.value) or { Type(Unknown{}) }
					// Taking the address of an immutable pointer variable for a
					// pointer-to-pointer field is valid. The reference preserves the
					// existing pointee; it does not make the pointer binding mutable.
					if !tc.ident_is_mutable_lvalue(addressed.value)
						&& unalias_type(addressed_type) !is Pointer {
						tc.record_error_at(.assignment_mismatch,
							'`${addressed.value}` is immutable, cannot have a mutable reference to an immutable object',
							addressed_id, addressed.pos)
					}
				}
			}
			if expected !is Void {
				mut actual := tc.resolve_expr(value_id, expected)
				if value_node.kind == .call {
					if call_info := tc.resolve_call_info(value_id, value_node) {
						if call_info.return_type !is Unknown && call_info.return_type !is Void {
							actual = call_info.return_type
						}
					}
				}
				if !tc.expr_compatible(value_id, actual, expected) {
					if semantic_mut_param := tc.mut_param_expr_base(value_id, actual) {
						actual = semantic_mut_param
					}
				}
				pointer_to_value_fixed_array := actual is Pointer
					&& unalias_type(actual.base_type) is ArrayFixed
					&& unalias_type(expected) is ArrayFixed
				clean_expected := unalias_type(expected)
				clean_actual := unalias_type(actual)
				if clean_expected is None {
					continue
				}
				if tc.translated_c_string_fixed_array_compatible(value_id, expected) {
					continue
				}
				if value_node.kind == .map_init && tc.map_literal_has_element_diagnostic(value_id) {
					continue
				}
				optional_pointer_nil := tc.expr_is_unsafe_nil(value_id)
					&& clean_expected is OptionType
					&& unalias_type(clean_expected.base_type) is Pointer
				optional_fn_nil := tc.expr_is_unsafe_nil(value_id) && clean_expected is OptionType
					&& unalias_type(clean_expected.base_type) is FnType
				if tc.expr_is_unsafe_nil(value_id) && clean_expected !is Pointer
					&& clean_expected !is FnType && !optional_pointer_nil && !optional_fn_nil {
					if expected is String {
						tc.record_error_at(.assignment_mismatch,
							'cannot assign to field `${field.value}`: expected `string`, not `voidptr`',
							field_id, tc.struct_init_unsafe_nil_type_mismatch_pos(field, value_id))
					}
					tc.record_error_at(.assignment_mismatch,
						'cannot assign `nil` to struct field `${field.value}` with type `${clean_expected.name()}`',
						value_id, tc.struct_init_unsafe_nil_value_pos(value_id))
					continue
				}
				if clean_actual is ResultType && clean_expected !is ResultType {
					tc.record_unhandled_result_call(value_id, clean_actual)
					continue
				}
				if clean_actual is Void {
					tc.record_error_at(.assignment_mismatch,
						'`${tc.source_text_for_node(value_id)}` (no value) used as value',
						field_id, tc.struct_init_field_deprecation_pos(field))
					continue
				}
				if tc.fn_storage_voidptr_mismatch(value_id, source_actual, expected) {
					tc.record_error_at(.assignment_mismatch,
						'cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${source_actual.name()}`',
						field_id, tc.struct_init_field_value_pos(field, value_id))
					continue
				}
				if clean_expected is FnType {
					expected_fn_text, expected_alias := tc.struct_field_diagnostic_fn_type(init_name,
						field.value, i) or { '', '' }
					actual_fn_text := tc.expr_diagnostic_fn_type(value_id) or { '' }
					if expected_fn_text.len > 0 && actual_fn_text.len > 0
						&& expected_fn_text != actual_fn_text&& (fn_diagnostic_parameter_modes(expected_fn_text) != fn_diagnostic_parameter_modes(actual_fn_text)
						|| !tc.fn_types_match_ignoring_module_qualification(clean_expected, clean_actual))
						&& !tc.expr_compatible(value_id, actual, expected)
						&& !tc.method_value_matches_voidptr_callback(value_id, actual, expected)
						&& !tc.fn_callback_adapter_compatible(source_actual, clean_expected) {
						details := tc.fn_assignment_mismatch_details(expected_fn_text,
							expected_alias, actual_fn_text, value_id)
						message := 'cannot assign to field `${field.value}`: expected `${expected_fn_text}`, not `${actual_fn_text}`'
						if details.len > 0 {
							tc.record_error_with_details_at(.assignment_mismatch, message,
								field_id, tc.struct_init_field_value_pos(field, value_id), details)
						} else {
							tc.record_error_at(.assignment_mismatch, message, field_id, tc.struct_init_field_value_pos(field,
								value_id))
						}
						continue
					}
				}
				if value_node.kind == .fn_literal && value_node.typ == '?'
					&& clean_expected is FnType {
					expected_name := expected.name().replace_once('fn(', 'fn (')
					tc.record_error_at(.assignment_mismatch,
						'cannot assign to field `${field.value}`: expected `${expected_name}`, not `${expected_name} ?`',
						field_id, tc.struct_init_field_deprecation_pos(field))
				} else if clean_expected is OptionType && clean_actual is Pointer
					&& unalias_type(clean_expected.base_type) !is Pointer {
					tc.record_error_at(.assignment_mismatch,
						'cannot assign a pointer to option struct field', field_id,
						tc.struct_init_pointer_field_pos(field))
				} else if clean_expected is OptionType
					&& unalias_type(clean_expected.base_type) is Pointer
					&& clean_actual is OptionType
					&& unalias_type(clean_actual.base_type) !is Pointer
					&& !tc.optional_pointer_expr_compatible(value_id, actual, expected) {
					tc.record_error_at(.assignment_mismatch,
						'cannot assign to field `${field.value}`: expected a pointer `${expected.name()}`, but got `${actual.name()}`',
						field_id, tc.struct_init_field_deprecation_pos(field))
				} else if tc.anonymous_struct_assignment_mismatch(value_id, actual, expected) {
					tc.record_error_at(.assignment_mismatch,
						'cannot assign anonymous `struct` to a typed `struct`', value_id,
						tc.anonymous_struct_literal_brace_pos(value_id))
				} else if value_node.kind == .map_init && value_node.children_count == 0
					&& clean_expected is Struct && is_anonymous_struct_name(clean_expected.name) {
					tc.record_error_at(.assignment_mismatch,
						'`{}` cannot be used to initialize anonymous structs. Use `struct{}` instead.',
						value_id, tc.empty_map_literal_diagnostic_pos(value_id))
				} else if clean_actual is OptionType && clean_expected !is OptionType {
					tc.record_error_at(.assignment_mismatch,
						'cannot assign an Option value to a non-option struct field', field_id,
						tc.struct_init_field_deprecation_pos(field))
				} else if clean_actual is FnType && clean_expected is FnType
					&& actual.name() != expected.name()
					&& !tc.fn_types_match_ignoring_module_qualification(clean_expected, clean_actual)
					&& !tc.expr_compatible(value_id, actual, expected)
					&& !tc.method_value_matches_voidptr_callback(value_id, actual, expected)
					&& !tc.fn_callback_adapter_compatible(source_actual, clean_expected) {
					tc.record_error_at(.assignment_mismatch,
						'cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${actual.name()}`',
						field_id, tc.struct_init_field_deprecation_pos(field))
				} else if clean_expected is Interface
					&& !tc.type_implements_interface(actual, clean_expected)
					&& tc.record_interface_implementation_error(.assignment_mismatch, actual, clean_expected, field_id, tc.struct_init_field_value_pos(field, value_id)) {
					continue
				} else if pointer_to_value_fixed_array
					|| tc.distinct_alias_primitive_mismatch(source_actual, expected)
					|| (clean_expected is Map && clean_actual is Map
					&& clean_expected.name() != clean_actual.name())
					|| (!tc.expr_compatible(value_id, actual, expected)
					&& !tc.method_value_matches_voidptr_callback(value_id, actual, expected)
					&& !tc.pointer_value_compatible(actual, expected)) {
					diagnostic_actual := if tc.distinct_alias_primitive_mismatch(source_actual,
						expected)
					{
						source_actual
					} else {
						actual
					}
					actual_name := tc.diagnostic_expr_type_name(value_id, diagnostic_actual)
					field_pos := if pointer_to_value_fixed_array {
						tc.struct_init_pointer_field_pos(field)
					} else {
						tc.struct_init_field_value_pos(field, value_id)
					}
					if clean_expected is Pointer && clean_actual !is Pointer {
						if expected.name() == 'voidptr' {
							if clean_actual is Struct {
								tc.record_error_at(.assignment_mismatch,
									'allocate `${clean_actual.name}` on the heap for use in other functions',
									field_id, field_pos)
							} else if tc.should_diagnose(value_id) {
								source_value := tc.source_text_for_node(value_id)
								tc.record_notice_at(.assignment_mismatch,
									'voidptr variables may only be assigned voidptr values (e.g. unsafe { voidptr(${source_value}) })',
									value_id, value_node.pos)
							}
							if !clean_actual.is_integer() {
								tc.record_error_at(.assignment_mismatch,
									'cannot assign to field `${field.value}`: expected a pointer `voidptr`, but got `${actual_name}`',
									field_id, field_pos)
							}
						} else if clean_actual.name() == 'bool' {
							tc.record_error_at(.assignment_mismatch,
								'cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${actual_name}`',
								field_id, field_pos)
							tc.record_error_at(.assignment_mismatch,
								'reference field must be initialized with reference', field_id,
								field_pos)
						} else {
							tc.record_error_at(.assignment_mismatch,
								'reference field must be initialized with reference', field_id,
								field_pos)
						}
					} else {
						tc.record_error_at(.assignment_mismatch,
							'cannot initialize field `${field.value}` with `${actual_name}`; expected `${expected.name()}`; cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${actual_name}`',
							field_id, field_pos)
					}
				}
			}
		}
		return
	}
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	_ = id
}

fn (tc &TypeChecker) struct_init_has_related_bare_generic_return_error(id flat.NodeId, generic_name string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	init_pos := tc.a.node(id).pos
	mut enclosing_id := flat.empty_node
	mut enclosing_offset := -1
	for index in tc.top_level_idx {
		decl := tc.a.node(flat.NodeId(index))
		if decl.pos.id != init_pos.id || decl.pos.offset > init_pos.offset
			|| decl.pos.offset <= enclosing_offset {
			continue
		}
		enclosing_id = flat.NodeId(index)
		enclosing_offset = decl.pos.offset
	}
	if !tc.valid_node_id(enclosing_id) {
		return false
	}
	enclosing := tc.a.node(enclosing_id)
	if enclosing.kind != .fn_decl || enclosing.generic_params().len == 0 {
		return false
	}
	return_name := tc.bare_generic_decl_type_name(enclosing.typ) or { return false }
	return return_name == generic_name
}

fn (tc &TypeChecker) fn_types_match_ignoring_module_qualification(expected FnType, actual Type) bool {
	if actual !is FnType || expected.params.len != actual.params.len {
		return false
	}
	actual_fn := actual as FnType
	for i in 0 .. expected.params.len {
		if !fn_param_modes_compatible(actual_fn, expected, i)
			|| !tc.types_match_ignoring_module_qualification(fn_compatible_param_type(expected, i), fn_compatible_param_type(actual_fn, i)) {
			return false
		}
	}
	return tc.fn_return_compatible(actual.return_type, expected.return_type)
}

fn (tc &TypeChecker) fn_callback_adapter_compatible(actual Type, expected Type) bool {
	actual_fn := fn_type_from_type(actual) or { return false }
	expected_fn := fn_type_from_type(expected) or { return false }
	if actual_fn.params.len != expected_fn.params.len
		|| !tc.fn_return_compatible(actual_fn.return_type, expected_fn.return_type) {
		return false
	}
	mut needs_adapter := false
	for i in 0 .. actual_fn.params.len {
		actual_param := fn_compatible_param_type(actual_fn, i)
		expected_param := fn_compatible_param_type(expected_fn, i)
		if !fn_param_modes_compatible(actual_fn, expected_fn, i)
			&& !fn_param_can_cast_userdata_param(actual_param, expected_param) {
			return false
		}
		if tc.types_match_ignoring_module_qualification(expected_param, actual_param) {
			continue
		}
		if !fn_param_can_cast_userdata_param(actual_param, expected_param) {
			return false
		}
		needs_adapter = true
	}
	return needs_adapter
}

fn (tc &TypeChecker) types_match_ignoring_module_qualification(expected Type, actual Type) bool {
	left := unalias_type(expected)
	right := unalias_type(actual)
	if left.name() == right.name() {
		return true
	}
	if left is Struct && right is Struct {
		return short_type_name(left.name) == short_type_name(right.name)
	}
	if left is Pointer && right is Pointer {
		return tc.types_match_ignoring_module_qualification(left.base_type, right.base_type)
	}
	if left is OptionType && right is OptionType {
		return tc.types_match_ignoring_module_qualification(left.base_type, right.base_type)
	}
	if left is ResultType && right is ResultType {
		return tc.types_match_ignoring_module_qualification(left.base_type, right.base_type)
	}
	return false
}

fn (mut tc TypeChecker) check_union_struct_init_fields(id flat.NodeId, node flat.Node, init_name string, supplied_fields map[string]bool) {
	if init_name in tc.unions && tc.initialized_union_field_count(init_name, supplied_fields) > 1 {
		display := init_name.all_after_last('.')
		tc.record_error_at(.assignment_mismatch,
			'union `${display}` can have only one field initialised', id,
			tc.struct_init_head_pos(node))
		return
	}
	for field in tc.structs[init_name] or { []StructField{} } {
		embedded := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(unalias_type(embedded)))
		if embedded_name !in tc.unions
			|| tc.initialized_union_field_count(embedded_name, supplied_fields) <= 1 {
			continue
		}
		display := embedded_name.all_after_last('.')
		tc.record_error_at(.assignment_mismatch,
			'embed union `${display}` can have only one field initialised', id,
			tc.struct_init_head_pos(node))
		return
	}
}

fn (tc &TypeChecker) initialized_union_field_count(union_name string, supplied_fields map[string]bool) int {
	mut count := 0
	for name, supplied in supplied_fields {
		if supplied && tc.struct_field_type(union_name, name) != none {
			count++
		}
	}
	return count
}

fn (mut tc TypeChecker) record_invalid_struct_init_print_error(id flat.NodeId) {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return
	}
	parent := tc.a.node(parent_id)
	if parent.kind != .call || parent.children_count == 0 {
		return
	}
	callee := tc.a.child_node(parent, 0)
	if callee.kind == .ident && callee.value in ['print', 'println', 'eprint', 'eprintln'] {
		tc.record_error_at(.call_arg_mismatch, '`${callee.value}` can not print void expressions',
			parent_id, parent.pos)
	}
}

fn struct_init_field_is_mut(fields []StructField, field flat.Node, index int) bool {
	if field.value.len > 0 {
		for candidate in fields {
			if candidate.name == field.value {
				return candidate.is_mut
			}
		}
		return false
	}
	return index >= 0 && index < fields.len && fields[index].is_mut
}

fn (tc &TypeChecker) node_has_ellipsis_prefix(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	file := tc.a.source_files[node.pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	mut cursor := int_min(int_max(node.pos.offset, 0), source.len)
	for cursor > 0 && source[cursor - 1] in [` `, `\t`, `\n`, `\r`] {
		cursor--
	}
	return cursor >= 3 && source[cursor - 3..cursor] == '...'
}

fn (tc &TypeChecker) struct_init_has_positional_fields(node flat.Node) bool {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind == .field_init && field.value.len == 0 {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) distinct_alias_primitive_mismatch(actual Type, expected Type) bool {
	if actual is Alias && unalias_type(expected) is Primitive {
		return actual.base_type.name() != unalias_type(expected).name()
	}
	return false
}

fn (tc &TypeChecker) generic_struct_field_unknown_type_message(struct_name string) ?string {
	decl := tc.source_struct_decl_for_name(struct_name) or { return none }
	base_name, _, is_generic := generic_type_application_parts(struct_name)
	lookup_name := if is_generic { base_name } else { struct_name }
	decl_file := tc.struct_files[lookup_name] or { tc.cur_file }
	decl_module := tc.struct_modules[lookup_name] or { tc.cur_module }
	for i in 0 .. decl.children_count {
		field_id := tc.a.child(&decl, i)
		field := tc.a.node(field_id)
		if field.kind != .field_decl || !generic_type_application(field.typ) {
			continue
		}
		for diagnostic in tc.errors {
			if diagnostic.node == field_id && diagnostic.kind == .unknown_type
				&& diagnostic.msg.starts_with('unknown type `') {
				return diagnostic.msg
			}
		}
		if unknown_name := tc.first_unknown_type_name_in_scope(field.typ, decl_file, decl_module,
			decl.generic_params())
		{
			return tc.unknown_type_message(unknown_name, field_id)
		}
	}
	return none
}

fn (tc &TypeChecker) first_unknown_type_name_in_scope(raw string, file string, mod_name string, generic_params []string) ?string {
	clean := trimmed_space(raw)
	if clean.len == 0 {
		return none
	}
	for prefix in ['?', '!', '&', '[]', 'shared ', 'mut '] {
		if clean.starts_with(prefix) {
			return tc.first_unknown_type_name_in_scope(clean[prefix.len..], file, mod_name,
				generic_params)
		}
	}
	base, args, is_generic := generic_type_application_parts(clean)
	if is_generic {
		if should_check_named_type(base) && base !in generic_params
			&& !tc.type_name_known_in_scope(base, file, mod_name) {
			return base
		}
		for arg in args {
			if unknown := tc.first_unknown_type_name_in_scope(arg, file, mod_name, generic_params) {
				return unknown
			}
		}
		return none
	}
	if clean !in generic_params && should_check_named_type(clean)
		&& !tc.type_name_known_in_scope(clean, file, mod_name) {
		return clean
	}
	return none
}

fn (tc &TypeChecker) type_name_known_in_scope(name string, file string, mod_name string) bool {
	if is_builtin_type_name(name) || name in ['map', 'unknown'] || name.starts_with('C.') {
		return true
	}
	if name.contains('.') {
		return tc.type_symbol_known(tc.resolve_imported_type_text_in_file(name, file))
	}
	if mod_name !in ['', 'main', 'builtin'] && tc.type_symbol_known('${mod_name}.${name}') {
		return true
	}
	if resolved := tc.resolve_selective_import_type_symbol_in_file(name, file) {
		return tc.type_symbol_known(resolved)
	}
	return tc.type_symbol_known(name)
}

fn struct_field_has_attr(field flat.Node, name string) bool {
	meta := field.generic_params()
	if meta.len < 2 {
		return false
	}
	for attr in meta[1..] {
		if attr.all_before(':').trim_space() == name {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) source_struct_decl_for_name(name string) ?flat.Node {
	id := tc.source_struct_decl_id_for_name(name)?
	return *tc.a.node(id)
}

fn (tc &TypeChecker) source_struct_decl_id_for_name(name string) ?flat.NodeId {
	raw_target := trimmed_space(name)
	parsed_target, _, is_generic := generic_type_application_parts(raw_target)
	target := if is_generic { parsed_target } else { raw_target }
	for index in tc.type_declaration_ids[target] {
		node := tc.a.nodes[index]
		if node.kind == .struct_decl {
			return flat.NodeId(index)
		}
	}
	return none
}

fn (tc &TypeChecker) struct_field_diagnostic_fn_type(struct_name string, field_name string, field_index int) ?(string, string) {
	key := '${struct_name}\x00${field_name}\x00${field_index}'
	mut cache := tc.type_cache
	if !isnil(cache) {
		if cached := cache.struct_field_fn_diagnostics[key] {
			separator := cached.index_u8(0)
			if separator <= 0 {
				return none
			}
			return cached[..separator], cached[separator + 1..]
		}
	}
	expected, alias := tc.struct_field_diagnostic_fn_type_uncached(struct_name, field_name,
		field_index) or {
		if !isnil(cache) {
			cache.struct_field_fn_diagnostics[key] = '\x00'
		}
		return none
	}
	if !isnil(cache) {
		cache.struct_field_fn_diagnostics[key] = '${expected}\x00${alias}'
	}
	return expected, alias
}

fn (tc &TypeChecker) struct_field_diagnostic_fn_type_uncached(struct_name string, field_name string, field_index int) ?(string, string) {
	decl := tc.source_struct_decl_for_name(struct_name) or { return none }
	base, concrete_args, is_generic := generic_type_application_parts(struct_name)
	struct_params := if is_generic {
		tc.struct_generic_params[base] or {
			tc.struct_generic_params[base.all_after_last('.')] or { []string{} }
		}
	} else {
		[]string{}
	}
	mut ordinal := 0
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl {
			continue
		}
		if (field_name.len > 0 && field.value == field_name)
			|| (field_name.len == 0 && ordinal == field_index) {
			source_type := tc.source_fn_field_type_text(field) or { field.typ }
			raw := if is_generic && concrete_args.len == struct_params.len {
				subst_fn_diagnostic_type_text(source_type, concrete_args, struct_params)
			} else {
				trimmed_space(source_type)
			}
			if raw.starts_with('fn(') || raw.starts_with('fn (') {
				return tc.diagnostic_fn_type_text(raw), ''
			}
			if alias_raw := tc.source_fn_alias_type_text(raw) {
				return tc.diagnostic_fn_type_text(alias_raw), raw
			}
			return none
		}
		ordinal++
	}
	return none
}

fn (tc &TypeChecker) source_fn_field_type_text(field flat.Node) ?string {
	file := tc.a.source_files[field.pos.id] or { return none }
	source := tc.source_texts_by_file[file.name] or { return none }
	offset := int_min(int_max(field.pos.offset, 0), source.len)
	line_start := if offset > 0 {
		relative := last_index_between(source, '\n', 0, offset)
		if relative >= 0 {
			relative + 1
		} else {
			0
		}
	} else {
		0
	}
	line_end := source.index_after('\n', offset) or { source.len }
	line := source[line_start..line_end]
	name_start := line.index(field.value) or { return none }
	type_start := line.index_after('fn', name_start + field.value.len) or { return none }
	mut raw := trimmed_space(line[type_start - 2..])
	for separator in [' @[', '\t@[', ' = '] {
		if end := raw.index(separator) {
			raw = trimmed_space(raw[..end])
		}
	}
	if !raw.starts_with('fn(') && !raw.starts_with('fn (') {
		return none
	}
	return raw
}

fn subst_fn_diagnostic_type_text(raw string, args []string, params []string) string {
	clean := trimmed_space(raw)
	if !clean.starts_with('fn(') && !clean.starts_with('fn (') {
		return subst_generic_text(clean, args, params)
	}
	fn_params, suffix := fn_diagnostic_type_parts(raw)
	mut rendered := []string{cap: fn_params.len}
	for param in fn_params {
		rendered << subst_generic_text(param.typ, args, params)
	}
	ret := trimmed_space(suffix)
	return 'fn (${rendered.join(', ')})${if ret.len > 0 {
		' ${subst_generic_text(ret, args, params)}'
	} else {
		''
	}}'
}

fn fn_diagnostic_parameter_modes(raw string) []string {
	clean := trimmed_space(raw)
	open := clean.index_u8(`(`)
	if open < 0 {
		return []string{}
	}
	mut depth := 1
	mut close := open + 1
	for close < clean.len && depth > 0 {
		if clean[close] == `(` {
			depth++
		} else if clean[close] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		close++
	}
	if close >= clean.len {
		return []string{}
	}
	mut modes := []string{}
	for part in split_params(clean[open + 1..close]) {
		param := trimmed_space(part)
		modes << if param.starts_with('mut ') {
			'mut'
		} else if param.starts_with('&') {
			'&'
		} else {
			''
		}
	}
	return modes
}

fn (tc &TypeChecker) source_fn_alias_type_text(name string) ?string {
	mut target := trimmed_space(name)
	for _ in 0 .. 8 {
		target_base, target_args, target_is_generic := generic_type_application_parts(target)
		lookup_target := if target_is_generic { target_base } else { target }
		mut module_name := ''
		mut found := ''
		mut found_name := ''
		for index in tc.top_level_idx {
			node := tc.a.node(flat.NodeId(index))
			if node.kind == .module_decl {
				module_name = node.value
				continue
			}
			if node.kind != .type_decl || node.children_count > 0 {
				continue
			}
			qualified := qualify_decl_name_in_module(node.value, module_name)
			if node.value == lookup_target || qualified == lookup_target {
				mut source_type := tc.source_type_alias_rhs(tc.cur_file, node.value) or { '' }
				if source_type.len == 0 {
					if file := tc.a.source_files[node.pos.id] {
						source_type = tc.source_type_alias_rhs(file.name, node.value) or { '' }
					}
				}
				found = if source_type.starts_with('fn(') || source_type.starts_with('fn (') {
					source_type
				} else {
					node.typ
				}
				found_name = if qualified == lookup_target { qualified } else { node.value }
				break
			}
		}
		if found.len == 0 {
			return none
		}
		clean := trimmed_space(found)
		if clean.starts_with('fn(') || clean.starts_with('fn (') {
			if target_is_generic {
				params := tc.type_alias_generic_params[found_name] or {
					tc.type_alias_generic_params[found_name.all_after_last('.')] or { []string{} }
				}
				if params.len == target_args.len {
					return subst_generic_diagnostic_fn_text(clean, target_args, params)
				}
			}
			return clean
		}
		target = clean
	}
	return none
}

fn subst_generic_diagnostic_fn_text(raw string, args []string, params []string) string {
	clean := trimmed_space(raw)
	open := clean.index_u8(`(`)
	if open < 0 {
		return subst_generic_text(clean, args, params)
	}
	mut depth := 1
	mut close := open + 1
	for close < clean.len && depth > 0 {
		if clean[close] == `(` {
			depth++
		} else if clean[close] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		close++
	}
	if close >= clean.len {
		return subst_generic_text(clean, args, params)
	}
	mut rendered_params := []string{}
	for part in split_params(clean[open + 1..close]) {
		rendered_params << subst_generic_text(part, args, params)
	}
	suffix := trimmed_space(clean[close + 1..])
	return 'fn (${rendered_params.join(', ')})${if suffix.len > 0 {
		' ${subst_generic_text(suffix, args, params)}'
	} else {
		''
	}}'
}

fn (tc &TypeChecker) source_type_alias_rhs(file_name string, alias_name string) ?string {
	if file_name.len == 0 || alias_name.len == 0 {
		return none
	}
	source := tc.source_texts_by_file[file_name] or { os.read_file(file_name) or { return none } }
	decl_start := source.index('type ${alias_name}') or { return none }
	line_end := source.index_after('\n', decl_start) or { source.len }
	assign := source[decl_start..line_end].index('=') or { return none }
	return source[decl_start + assign + 1..line_end].trim_space()
}

fn (tc &TypeChecker) expr_diagnostic_fn_type(id flat.NodeId) ?string {
	raw := tc.expr_raw_fn_type_text(id) or { return none }
	return tc.diagnostic_fn_type_text(raw)
}

fn (tc &TypeChecker) expr_raw_fn_type_text(id flat.NodeId) ?string {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	if node.kind == .fn_literal {
		return tc.fn_node_source_type_text(node)
	}
	if node.kind == .selector && node.children_count > 0 && tc.expr_is_method_value(id) {
		base_type := unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(node, 0))))
		for candidate in receiver_method_name_candidates(base_type, node.value, tc.cur_module) {
			mut module_name := ''
			for index in tc.top_level_idx {
				decl := tc.a.node(flat.NodeId(index))
				if decl.kind == .module_decl {
					module_name = decl.value
					continue
				}
				if decl.kind != .fn_decl {
					continue
				}
				if decl.value == candidate
					|| checker_qualified_fn_name(module_name, decl.value) == candidate {
					return tc.bound_method_node_source_type_text(decl)
				}
			}
		}
	}
	if node.kind == .ident {
		if local_type := tc.non_file_scope_type(node.value) {
			if fn_type := fn_type_from_type(local_type) {
				return Type(fn_type).name()
			}
		}
		if index := tc.fn_decl_short_name_ids[node.value] {
			return tc.fn_node_source_type_text(tc.a.node(flat.NodeId(index)))
		}
		// Declarations appended after collect (monomorphization) miss the
		// index; scan them with an allocation-free suffix compare.
		for index in tc.top_level_idx {
			decl := tc.a.node(flat.NodeId(index))
			if decl.kind == .fn_decl && embedded_name_matches(node.value, decl.value) {
				return tc.fn_node_source_type_text(decl)
			}
		}
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.expr_raw_fn_type_text(tc.a.child(node, 0))
	}
	return none
}

fn (tc &TypeChecker) bound_method_node_source_type_text(node flat.Node) string {
	mut params := []string{}
	mut skipped_receiver := false
	for i in 0 .. node.children_count {
		param := tc.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		if !skipped_receiver {
			skipped_receiver = true
			continue
		}
		mut typ := param.typ
		if param.op == .dot && !typ.starts_with('...') {
			typ = '...${typ}'
		}
		prefix := if param.is_mut && !typ.starts_with('mut ') { 'mut ' } else { '' }
		if param.value.len > 0 {
			params << '${prefix}${param.value} ${typ}'
		} else {
			params << '${prefix}${typ}'
		}
	}
	return 'fn (${params.join(', ')})${if node.typ.len > 0 && node.typ != 'void' {
		' ${node.typ}'
	} else {
		''
	}}'
}

fn (tc &TypeChecker) fn_node_source_type_text(node flat.Node) string {
	mut params := []string{}
	for i in 0 .. node.children_count {
		param := tc.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		mut typ := param.typ
		if param.op == .dot && !typ.starts_with('...') {
			typ = '...${typ}'
		}
		prefix := if param.is_mut && !typ.starts_with('mut ') { 'mut ' } else { '' }
		if param.value.len > 0 {
			params << '${prefix}${param.value} ${typ}'
		} else {
			params << '${prefix}${typ}'
		}
	}
	return 'fn (${params.join(', ')})${if node.typ.len > 0 && node.typ != 'void' {
		' ${node.typ}'
	} else {
		''
	}}'
}

fn (tc &TypeChecker) diagnostic_fn_type_text(raw string) string {
	params, suffix := fn_diagnostic_type_parts(raw)
	mut display := []string{cap: params.len}
	for param in params {
		display << param.typ
	}
	return 'fn (${display.join(', ')})${suffix}'
}

fn fn_diagnostic_type_parts(raw string) ([]FnDiagnosticParam, string) {
	clean := trimmed_space(raw)
	open := clean.index_u8(`(`)
	if open < 0 {
		return []FnDiagnosticParam{}, ''
	}
	mut depth := 1
	mut close := open + 1
	for close < clean.len && depth > 0 {
		if clean[close] == `(` {
			depth++
		} else if clean[close] == `)` {
			depth--
		}
		if depth == 0 {
			break
		}
		close++
	}
	if close >= clean.len {
		return []FnDiagnosticParam{}, ''
	}
	mut params := []FnDiagnosticParam{}
	for raw_param in split_params(clean[open + 1..close]) {
		mut text := trimmed_space(raw_param)
		if text.len == 0 {
			continue
		}
		is_mut := text.starts_with('mut ')
		if is_mut {
			text = trimmed_space(text[4..])
		}
		mut name := ''
		space := top_level_space_index(text)
		if space > 0 {
			head := trimmed_space(text[..space])
			tail := trimmed_space(text[space + 1..])
			if fn_type_param_head_is_name(head, tail) {
				name = head
				text = tail
			}
		}
		is_variadic := text.starts_with('...')
		mut display_type := text
		if is_mut {
			display_type = 'mut ${display_type.trim_left('&')}'
		}
		params << FnDiagnosticParam{
			name:       name
			typ:        display_type
			is_mut:     is_mut
			is_pointer: !is_variadic && (is_mut || text.starts_with('&')
				|| text in ['voidptr', '&void'])
		}
	}
	suffix_text := trimmed_space(clean[close + 1..])
	suffix := if suffix_text.len > 0 { ' ${suffix_text}' } else { '' }
	return params, suffix
}

fn (tc &TypeChecker) fn_assignment_mismatch_details(expected_text string, expected_alias string, actual_text string, actual_id flat.NodeId) []string {
	expected_params, _ := fn_diagnostic_type_parts(expected_text)
	actual_params, _ := fn_diagnostic_type_parts(actual_text)
	mut expected_named := []FnDiagnosticParam{}
	if expected_alias.len > 0 {
		if raw := tc.source_fn_alias_type_text(expected_alias) {
			expected_named, _ = fn_diagnostic_type_parts(raw)
		}
	}
	mut actual_named := []FnDiagnosticParam{}
	if raw := tc.expr_raw_fn_type_text(actual_id) {
		actual_named, _ = fn_diagnostic_type_parts(raw)
	}
	for i in 0 .. int_min(expected_params.len, actual_params.len) {
		if expected_params[i].is_pointer == actual_params[i].is_pointer {
			continue
		}
		expected_pointer := if expected_params[i].is_pointer {
			'to be a pointer'
		} else {
			'to be NOT a pointer'
		}
		actual_pointer := if actual_params[i].is_pointer {
			'is a pointer'
		} else {
			'is NOT a pointer'
		}
		if expected_alias.len > 0 && i < expected_named.len && i < actual_named.len
			&& expected_named[i].name.len > 0 && actual_named[i].name.len > 0 {
			mut alias_name := tc.qualify_name(expected_alias)
			if !alias_name.contains('.') {
				alias_name = '${if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }}.${alias_name}'
			}
			return [
				'`${alias_name}`\'s expected argument `${expected_named[i].name}` ${expected_pointer}, but the passed argument `${actual_named[i].name}` ${actual_pointer}',
			]
		}
		return [
			'expected argument ${i + 1} ${expected_pointer}, but the passed argument ${i + 1} ${actual_pointer}',
		]
	}
	return []string{}
}

fn (tc &TypeChecker) missing_reference_struct_fields(struct_name string, supplied map[string]bool, path []string) []MissingReferenceField {
	clean_name := trimmed_space(struct_name)
	if clean_name.len == 0 || clean_name in path || path.len >= 16 {
		return []MissingReferenceField{}
	}
	decl := tc.source_struct_decl_for_name(clean_name) or { return []MissingReferenceField{} }
	display_name := decl.value.all_after_last('.')
	mut next_path := path.clone()
	next_path << clean_name
	mut missing := []MissingReferenceField{}
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_text := if field.typ.len > 0 { field.typ } else { field.value }
		field_type := unalias_type(tc.struct_field_type(clean_name, field.value) or {
			tc.parse_type(field_type_text)
		})
		is_embed := source_field_decl_is_embed(field, field_type_text)
		if field_type is Pointer {
			if tc.struct_field_has_shared_elements(clean_name, field.value) {
				continue
			}
			if field_type_text in ['charptr', 'byteptr', 'voidptr'] {
				continue
			}
			if unalias_type(field_type.base_type) is Void {
				continue
			}
			if is_embed {
				base_type := unalias_type(field_type.base_type)
				if base_type is Struct {
					mut promoted_field_supplied := false
					for promoted_field in tc.struct_fields_for_init(base_type.name) {
						if promoted_field.name in supplied {
							promoted_field_supplied = true
							break
						}
					}
					if promoted_field_supplied {
						continue
					}
				}
			}
			if field.children_count == 0 && field.value !in supplied {
				missing << MissingReferenceField{
					path:  '${display_name}.${field.value}'
					owner: display_name
				}
			}
			continue
		}
		if field_type !is Struct || field.children_count > 0 {
			continue
		}
		if !is_embed && field.value in supplied {
			continue
		}
		child_supplied := if is_embed {
			supplied
		} else {
			map[string]bool{}
		}
		child_missing := tc.missing_reference_struct_fields(field_type.name(), child_supplied,
			next_path)
		for nested in child_missing {
			suffix := nested.path.all_after('.')
			outer_path := if is_embed {
				'${display_name}.${suffix}'
			} else {
				'${display_name}.${field.value}.${suffix}'
			}
			missing << MissingReferenceField{
				path:     outer_path
				owner:    nested.owner
				has_part: !is_embed
			}
			missing << nested
		}
	}
	return missing
}

fn (tc &TypeChecker) missing_required_struct_fields(struct_name string, supplied map[string]bool, path []string) []string {
	clean_name := trimmed_space(struct_name)
	if clean_name.len == 0 || clean_name in path || path.len >= 16 {
		return []string{}
	}
	decl := tc.source_struct_decl_for_name(clean_name) or { return []string{} }
	display_name := decl.value.all_after_last('.')
	mut next_path := path.clone()
	next_path << clean_name
	mut missing := []string{}
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_text := if field.typ.len > 0 { field.typ } else { field.value }
		is_embed := source_field_decl_is_embed(field, field_type_text)
		if struct_field_has_attr(*field, 'required') && field.value !in supplied {
			missing << '${display_name}.${field.value}'
		}
		field_type := unalias_type(tc.struct_field_type(clean_name, field.value) or {
			tc.parse_type(field_type_text)
		})
		if field_type !is Struct || field.children_count > 0 {
			continue
		}
		if !is_embed && field.value in supplied {
			continue
		}
		child_supplied := if is_embed {
			supplied
		} else {
			map[string]bool{}
		}
		child_missing := tc.missing_required_struct_fields(field_type.name(), child_supplied,
			next_path)
		if is_embed {
			for nested in child_missing {
				leaf := nested.all_after_last('.')
				outer := '${display_name}.${leaf}'
				if outer !in missing {
					missing << outer
				}
				missing << nested
			}
		} else {
			missing << child_missing
		}
	}
	return missing
}

fn (tc &TypeChecker) struct_init_head_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	if start < end {
		if relative := source[start..end].index('{') {
			mut head_end := start + relative + 1
			if head_end < end && source[head_end] == `}` {
				head_end++
			}
			return token.new_span(node.pos.id, start, head_end)
		}
	}
	return node.pos
}

fn (tc &TypeChecker) unknown_qualified_struct_init_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, int_min(node.pos.offset, source.len))
	end := int_max(start, int_min(node.pos.end, source.len))
	text := source[start..end]
	dot := text.last_index_u8(`.`)
	if dot >= 0 {
		return token.new_span(node.pos.id, start + dot + 1, end)
	}
	return node.pos
}

fn (tc &TypeChecker) struct_init_deprecation_pos(node flat.Node) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(node.pos.end, source.len)
	if end <= start {
		return node.pos
	}
	text := source[start..end]
	if dot := text.last_index('.') {
		return token.new_span(node.pos.id, start + dot + 1, end)
	}
	return node.pos
}

fn (tc &TypeChecker) struct_init_field_deprecation_pos(field flat.Node) token.Pos {
	file := tc.a.source_files[field.pos.id] or { return field.pos }
	source := tc.source_texts_by_file[file.name] or { return field.pos }
	offset := int_min(int_max(field.pos.offset, 0), source.len)
	mut line_start := offset
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	line_end := source.index_after('\n', line_start) or { source.len }
	line := source[line_start..line_end].trim_right('\r\n')
	if relative := line.index(field.value) {
		start := line_start + relative
		return token.new_span(field.pos.id, start, line_start + line.len)
	}
	return field.pos
}

fn (tc &TypeChecker) struct_init_field_value_pos(field flat.Node, value_id flat.NodeId) token.Pos {
	value := tc.a.node(value_id)
	file := tc.a.source_files[value.pos.id] or {
		return tc.struct_init_field_deprecation_pos(field)
	}
	source := tc.source_texts_by_file[file.name] or {
		return tc.struct_init_field_deprecation_pos(field)
	}
	value_start := int_max(0, int_min(value.pos.offset, source.len))
	mut line_start := value_start
	for line_start > 0 && source[line_start - 1] != `\n` {
		line_start--
	}
	line_end := source.index_after('\n', line_start) or { source.len }
	if relative := source[line_start..line_end].index(field.value) {
		start := line_start + relative
		end := int_max(start, int_min(value.pos.end, source.len))
		return token.new_span(value.pos.id, start, end)
	}
	return tc.struct_init_field_deprecation_pos(field)
}

fn (tc &TypeChecker) struct_init_unsafe_nil_type_mismatch_pos(field flat.Node, value_id flat.NodeId) token.Pos {
	value := tc.a.node(value_id)
	field_name_pos := tc.struct_init_field_deprecation_pos(field)
	end := int_min(value.pos.end, value.pos.offset + 'unsafe'.len)
	return token.new_span(field_name_pos.id, field_name_pos.offset, end)
}

fn (tc &TypeChecker) struct_init_unsafe_nil_value_pos(value_id flat.NodeId) token.Pos {
	value := tc.a.node(value_id)
	return token.new_span(value.pos.id, value.pos.offset, int_max(value.pos.offset,
		value.pos.end - 1))
}

fn (tc &TypeChecker) struct_init_pointer_field_pos(field flat.Node) token.Pos {
	pos := tc.struct_init_field_deprecation_pos(field)
	file := tc.a.source_files[pos.id] or { return pos }
	source := tc.source_texts_by_file[file.name] or { return pos }
	start := int_min(int_max(pos.offset, 0), source.len)
	end := int_min(int_max(pos.end, start), source.len)
	if start < end {
		if relative := source[start..end].index('&') {
			return token.new_span(pos.id, start, start + relative + 1)
		}
	}
	return pos
}

fn (mut tc TypeChecker) infer_generic_struct_init_type(node flat.Node) ?Type {
	base_name := trimmed_space(node.value)
	if base_name.len == 0 || base_name.contains('[') {
		return none
	}
	qualified_name := tc.qualify_name(base_name)
	params := tc.struct_generic_params[qualified_name] or {
		// A concrete declaration in the active module shadows an unrelated
		// bare generic declaration collected from another module.
		if qualified_name != base_name && qualified_name in tc.structs {
			return none
		}
		tc.struct_generic_params[base_name] or { return none }
	}
	if params.len == 0 {
		return none
	}
	inferred := tc.infer_generic_struct_init_param_texts(node, qualified_name, params)
	if inferred.len == 0 {
		return none
	}
	mut args := []string{cap: params.len}
	for param in params {
		arg := inferred[param] or { return none }
		args << arg
	}
	return tc.parse_type('${qualified_name}[${args.join(', ')}]')
}

fn (mut tc TypeChecker) infer_generic_struct_init_param_texts(node flat.Node, base_name string, params []string) map[string]string {
	init_name := tc.qualify_name(base_name)
	fields := tc.structs[init_name] or { tc.structs[base_name] or { return map[string]string{} } }
	source_fields := tc.source_struct_field_decls(init_name)
	mut inferred := map[string]string{}
	for param in params {
		if param in tc.fn_context.generic_params {
			inferred[param] = param
		}
	}
	for i in 0 .. node.children_count {
		field_node := tc.a.child_node(&node, i)
		if field_node.kind != .field_init || field_node.children_count == 0 {
			continue
		}
		value_id := tc.a.child(field_node, 0)
		mut field_type := Type(void_)
		mut field_type_text := ''
		if field_node.value.len > 0 {
			for field_idx, field in fields {
				if field.name == field_node.value {
					field_type = field.typ
					if field_idx < source_fields.len {
						field_type_text = source_fields[field_idx].typ
					}
					break
				}
			}
		} else if i < fields.len {
			field_type = fields[i].typ
			if i < source_fields.len {
				field_type_text = source_fields[i].typ
			}
		}
		if field_type is Void {
			continue
		}
		if field_type_text.len == 0 {
			field_type_text = field_type.name()
		}
		actual := tc.resolve_type(value_id)
		tc.infer_generic_type_text_from_type(field_type_text, actual, params, mut inferred)
	}
	return inferred
}

fn (tc &TypeChecker) source_struct_field_decls(struct_name string) []SourceStructFieldDecl {
	base_name, _, is_generic := generic_type_application_parts(struct_name)
	lookup := if is_generic { base_name } else { struct_name }
	mut candidates := []string{}
	push_type_name_candidate(mut candidates, lookup)
	push_type_name_candidate(mut candidates, lookup.all_after_last('.'))
	push_type_name_candidate(mut candidates, tc.qualify_name(lookup))
	mut wanted_file := ''
	for candidate in candidates {
		if file := tc.struct_files[candidate] {
			wanted_file = file
			break
		}
	}
	mut cur_file := ''
	mut cur_module := ''
	for idx in tc.top_level_idx {
		if idx < 0 || idx >= tc.a.nodes.len {
			continue
		}
		decl := tc.a.nodes[idx]
		match decl.kind {
			.file {
				cur_file = decl.value
			}
			.module_decl {
				cur_module = decl.value
			}
			.struct_decl {
				decl_qname := if cur_module.len > 0 && !decl.value.contains('.') {
					'${cur_module}.${decl.value}'
				} else {
					decl.value
				}
				if decl.value !in candidates && decl_qname !in candidates {
					continue
				}
				if wanted_file.len > 0 && cur_file.len > 0 && cur_file != wanted_file {
					continue
				}
				mut fields := []SourceStructFieldDecl{}
				for i in 0 .. decl.children_count {
					field := tc.a.child_node(&decl, i)
					if field.kind != .field_decl {
						continue
					}
					field_typ := if field.typ.len > 0 { field.typ } else { field.value }
					fields << SourceStructFieldDecl{
						name: field.value
						typ:  field_typ
					}
				}
				return fields
			}
			else {}
		}
	}
	return []SourceStructFieldDecl{}
}

// check_deferred_generic_receiver_comparisons checks pointer/value ordering
// mismatches in an open generic receiver method when a concrete receiver
// instance is known elsewhere in the selected source file.
fn (mut tc TypeChecker) check_deferred_generic_receiver_comparisons(node flat.Node) {
	if node.children_count == 0 {
		return
	}
	receiver := tc.a.child_node(&node, 0)
	if receiver.kind != .param || receiver.op != .dot {
		return
	}
	receiver_base, receiver_params, is_generic :=
		generic_type_application_parts(comptime_static_unwrap_type_text(receiver.typ))
	if !is_generic || receiver_params.len == 0 {
		return
	}
	mut param_types := map[string]string{}
	for i in 0 .. node.children_count {
		param := tc.a.child_node(&node, i)
		if param.kind == .param && param.value.len > 0 {
			param_types[param.value] = if param.is_mut && param.op != .amp
				&& param.typ.starts_with('&') {
				param.typ[1..]
			} else {
				param.typ
			}
		}
	}
	mut comparisons := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			tc.collect_deferred_generic_pointer_comparisons(child_id, param_types, receiver_params, mut
				comparisons)
		}
	}
	if comparisons.len == 0 {
		return
	}
	concrete_args := tc.concrete_generic_receiver_args(receiver_base, receiver_params.len) or {
		return
	}
	for comparison_id in comparisons {
		comparison := tc.a.node(comparison_id)
		lhs_id := tc.a.child(comparison, 0)
		rhs_id := tc.a.child(comparison, 1)
		lhs_raw := tc.deferred_generic_expr_type_text(lhs_id, param_types)
		rhs_raw := tc.deferred_generic_expr_type_text(rhs_id, param_types)
		lhs := subst_generic_text(lhs_raw, concrete_args, receiver_params)
		rhs := subst_generic_text(rhs_raw, concrete_args, receiver_params)
		if !generic_pointer_value_mismatch(lhs, rhs) {
			continue
		}
		tc.record_error_at(.condition_mismatch, 'mismatched types `${lhs}` and `${rhs}`',
			comparison_id, comparison.pos)
	}
}

fn (tc &TypeChecker) collect_deferred_generic_pointer_comparisons(id flat.NodeId, param_types map[string]string, generic_params []string, mut comparisons []flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .infix && node.op in [.lt, .gt, .le, .ge] && node.children_count >= 2 {
		lhs := tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types)
		rhs := tc.deferred_generic_expr_type_text(tc.a.child(node, 1), param_types)
		if type_text_mentions_any_generic_param(lhs, generic_params)
			&& type_text_mentions_any_generic_param(rhs, generic_params)
			&& generic_pointer_value_mismatch(lhs, rhs) {
			comparisons << id
		}
	}
	for i in 0 .. node.children_count {
		tc.collect_deferred_generic_pointer_comparisons(tc.a.child(node, i), param_types,
			generic_params, mut comparisons)
	}
}

fn (tc &TypeChecker) deferred_generic_expr_type_text(id flat.NodeId, param_types map[string]string) string {
	if !tc.valid_node_id(id) {
		return ''
	}
	node := tc.a.node(id)
	match node.kind {
		.ident {
			return param_types[node.value] or { node.typ }
		}
		.paren {
			if node.children_count > 0 {
				return tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types)
			}
		}
		.selector {
			if node.children_count == 0 {
				return node.typ
			}
			receiver_type := tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types)
			base, args, is_generic :=
				generic_type_application_parts(comptime_static_unwrap_type_text(receiver_type))
			lookup := if is_generic { base } else { comptime_static_unwrap_type_text(receiver_type) }
			fields := tc.source_struct_field_decls(lookup)
			for field in fields {
				if field.name != node.value {
					continue
				}
				if is_generic {
					params := tc.struct_generic_params[base] or {
						tc.struct_generic_params[base.all_after_last('.')] or { []string{} }
					}
					if params.len == args.len && params.len > 0 {
						return subst_generic_text(field.typ, args, params)
					}
				}
				return field.typ
			}
		}
		.index {
			if node.children_count == 0 {
				return node.typ
			}
			container := tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types)
			clean := trimmed_space(container)
			if clean.starts_with('[]') {
				return trimmed_space(clean[2..])
			}
			if clean.starts_with('[') {
				close := find_matching_bracket(clean, 0)
				if close > 0 && close + 1 < clean.len {
					return trimmed_space(clean[close + 1..])
				}
			}
		}
		else {}
	}
	return node.typ
}

fn (tc &TypeChecker) concrete_generic_receiver_args(receiver_base string, arg_count int) ?[]string {
	for node in tc.a.nodes {
		if node.kind != .call || node.children_count == 0 {
			continue
		}
		callee := tc.a.child_node(&node, 0)
		if callee.kind != .index || callee.children_count < 2 {
			continue
		}
		base_node := tc.a.child_node(callee, 0)
		fn_name := tc.generic_call_base_name(base_node) or { continue }
		call_args := tc.generic_call_type_arg_names(callee)
		fn_params := tc.fn_generic_params[fn_name] or { continue }
		if call_args.len == 0 || call_args.len != fn_params.len {
			continue
		}
		ret_text := tc.fn_ret_type_texts[fn_name] or { continue }
		ret_base, ret_args, is_generic :=
			generic_type_application_parts(comptime_static_unwrap_type_text(ret_text))
		if !is_generic || ret_args.len != arg_count
			|| ret_base.all_after_last('.') != receiver_base.all_after_last('.') {
			continue
		}
		mut concrete := []string{cap: ret_args.len}
		for ret_arg in ret_args {
			concrete << subst_generic_text(ret_arg, call_args, fn_params)
		}
		return concrete
	}
	return none
}

fn type_text_mentions_any_generic_param(typ string, params []string) bool {
	clean := trimmed_space(typ)
	for param in params {
		if clean == param || clean.starts_with('&${param}') || clean.ends_with('[${param}]')
			|| clean.contains('[${param},') || clean.contains(', ${param}]') {
			return true
		}
	}
	return false
}

fn generic_pointer_value_mismatch(lhs string, rhs string) bool {
	left := trimmed_space(lhs)
	right := trimmed_space(rhs)
	return (left.starts_with('&') && trimmed_space(left[1..]) == right)
		|| (right.starts_with('&') && left == trimmed_space(right[1..]))
}

// expr_is_method_value reports whether `id` is a selector that resolves to a *method
// value* — a struct/interface method used as a value (`obj.draw`), not a field access or a method
// call. cgen backs such values with per-instance closure contexts.
pub fn (tc &TypeChecker) expr_is_method_value(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.nodes[int(id)]
	// A local bound to a method value (`cb := c.report`) must remain identifiable so
	// mutable-receiver escape checks can follow aliases of the bare selector.
	if node.kind == .ident {
		return tc.current_binding_is_method_value_local(node.value)
	}
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	receiver := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	clean := unalias_type(receiver)
	if receiver is Alias {
		underlying := unalias_type(receiver)
		if underlying is Struct && tc.struct_field_type(underlying.name, node.value) != none {
			return false
		}
		if _ := tc.alias_method_value_decl_key(receiver, node.value) {
			return true
		}
	} else if clean is Struct {
		sname := clean.name
		if tc.struct_field_type(sname, node.value) != none {
			return false
		}
		if '${sname}.${node.value}' in tc.fn_param_types {
			return true
		}
		if receiver is Alias && '${receiver.name}.${node.value}' in tc.fn_param_types {
			return true
		}
		if _ := tc.resolve_generic_struct_method(sname, node.value) {
			return true
		}
	} else if clean is Interface {
		iname := clean.name
		if tc.interface_field_type(iname, node.value) != none {
			return false
		}
		if receiver is Alias && '${receiver.name}.${node.value}' in tc.fn_param_types {
			return true
		}
		if _ := tc.interface_receiver_method_call_info(iname, node.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) alias_method_value_decl_key(alias Alias, method string) ?string {
	for receiver in [Type(alias), alias.base_type] {
		for candidate in receiver_method_name_candidates(receiver, method, tc.cur_module) {
			if candidate in tc.fn_param_types || candidate in tc.fn_ret_types {
				return candidate
			}
		}
		type_name := resolve_type_name_for_method(receiver)
		if type_name.len > 0 {
			if info := tc.resolve_generic_struct_method(type_name, method) {
				return info.name
			}
		}
	}
	return none
}

fn (tc &TypeChecker) method_value_has_stack_mut_receiver(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr, .expr_stmt] && node.children_count == 1 {
		return tc.method_value_has_stack_mut_receiver(tc.a.child(&node, 0))
	}
	if node.kind == .ident {
		return tc.current_method_value_local_has_stack_mut_receiver(node.value)
	}
	if node.kind != .selector || node.children_count == 0 || !tc.expr_is_method_value(id) {
		return false
	}
	base_id := tc.a.child(&node, 0)
	base_type := tc.cached_expr_type(base_id) or { tc.resolve_type(base_id) }
	// A pointer receiver already has caller-managed storage. A non-addressable
	// rvalue is copied into the per-instance closure context by cgen.
	if tc.type_is_pointer_receiver(base_type) || !tc.expr_can_take_address(base_id)
		|| tc.expr_root_is_global_binding(base_id) {
		return false
	}
	clean := unwrap_all_pointers(base_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return false
	}
	if info := tc.resolve_generic_struct_method(type_name, node.value) {
		if tc.mut_receiver_methods[info.name] {
			return true
		}
	}
	for method_name in receiver_method_name_candidates(clean, node.value, tc.cur_module) {
		if tc.mut_receiver_methods[method_name] {
			return true
		}
	}
	return false
}

// reject_stored_method_value rejects the remaining unsafe method-value escape:
// a mutable pointer receiver bound to addressable stack storage. Cgen must borrow
// that local for in-scope calls, but the borrow would dangle after a return/store.
// Per-instance contexts make value receivers, pointer receivers, and copied
// non-addressable rvalues safe to store.
fn (mut tc TypeChecker) reject_stored_method_value(id flat.NodeId) {
	if tc.method_value_has_stack_mut_receiver(id) && tc.should_diagnose(id) {
		tc.record_error(.assignment_mismatch,
			'a method value with a mutable local receiver cannot escape its call site', id)
	}
}

fn (tc &TypeChecker) struct_literal_unknown_field_message(struct_name string, field_name string, fields []StructField) string {
	display_name := struct_name.all_after_last('.')
	base := 'unknown field `${field_name}` in struct literal of type `${display_name}`'
	mut candidates := []string{}
	for field in fields {
		if field.name.len > 0 && field.name !in candidates {
			candidates << field.name
		}
	}
	suggested := util.new_suggestion(field_name, candidates).say(base)
	if suggested != base {
		return suggested
	}
	if candidates.len == 0 {
		return base
	}
	grammar := if candidates.len == 1 { 'possibility' } else { 'possibilities' }
	quoted := candidates.map('`${it}`')
	return '${base}.\n${candidates.len} ${grammar}: ${quoted.join(', ')}.'
}

fn (tc &TypeChecker) stored_method_value_matches_voidptr_callback(id flat.NodeId, expected Type) bool {
	if !tc.expr_is_method_value(id) {
		return false
	}
	actual := tc.resolve_type(id)
	return tc.method_value_matches_voidptr_callback(id, actual, expected)
}

fn (tc &TypeChecker) method_value_matches_voidptr_callback(id flat.NodeId, actual Type, expected Type) bool {
	if !tc.expr_is_method_value(id) {
		return false
	}
	actual_fn := fn_type_from_type(actual) or { return false }
	expected_fn := fn_type_from_type(expected) or { return false }
	if expected_fn.params.len != actual_fn.params.len + 1 {
		return false
	}
	if expected_fn.params.len == 0 || !fn_param_is_voidptr_type(expected_fn.params[0]) {
		return false
	}
	for i in 0 .. actual_fn.params.len {
		if !fn_param_modes_compatible_at(actual_fn, i, expected_fn, i + 1)
			|| !tc.fn_param_compatible(fn_compatible_param_type(actual_fn, i), fn_compatible_param_type(expected_fn, i + 1)) {
			return false
		}
	}
	return tc.fn_return_compatible(actual_fn.return_type, expected_fn.return_type)
}

fn (tc &TypeChecker) expr_is_capturing_fn_literal_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return tc.ident_is_capturing_fn_literal_alias(node.value)
		}
		.fn_literal {
			return tc.fn_literal_has_captures(node)
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_is_capturing_fn_literal_value(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) ident_is_capturing_fn_literal_alias(name string) bool {
	binding_key := tc.visible_binding_storage_key(name) or { return false }
	return tc.capturing_fn_literal_locals[binding_key]
}

fn (tc &TypeChecker) visible_binding_storage_key(name string) ?string {
	if tc.cur_scope == unsafe { nil } {
		return none
	}
	owner := tc.cur_scope.lookup_owner(name) or { return none }
	binding_key := owner.storage_key()
	if binding_key.len == 0 {
		return none
	}
	return binding_key
}

fn (tc &TypeChecker) expr_is_unsupported_returned_capturing_fn_literal_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return tc.ident_is_unsupported_returned_capturing_fn_literal_alias(node.value)
		}
		.fn_literal {
			return tc.fn_literal_has_unsupported_return_capture(node)
		}
		.cast_expr, .paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_is_unsupported_returned_capturing_fn_literal_value(tc.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (tc &TypeChecker) ident_is_unsupported_returned_capturing_fn_literal_alias(name string) bool {
	binding_key := tc.visible_binding_storage_key(name) or { return false }
	if !tc.capturing_fn_literal_locals[binding_key] {
		return false
	}
	return tc.capturing_fn_literal_return_unsupported[binding_key] or { true }
}

fn (tc &TypeChecker) fn_literal_has_captures(node flat.Node) bool {
	if node.kind != .fn_literal {
		return false
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_literal_has_unsupported_return_capture(node flat.Node) bool {
	if node.kind != .fn_literal {
		return false
	}
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0
			&& !tc.fn_literal_return_capture_is_supported(child) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) fn_literal_return_capture_is_supported(capture flat.Node) bool {
	if !capture.is_mut || capture.value.len == 0 {
		return false
	}
	if typ := tc.cur_scope.lookup(capture.value) {
		return unalias_type(typ) is Pointer
	}
	return false
}

fn (mut tc TypeChecker) reject_returned_capturing_fn_literal(id flat.NodeId) {
	if tc.expr_is_unsupported_returned_capturing_fn_literal_value(id) {
		tc.reject_capturing_fn_literal_escape(id,
			'capturing fn literal cannot be stored or returned')
	}
}

fn (mut tc TypeChecker) reject_stored_capturing_fn_literal(id flat.NodeId) {
	tc.reject_capturing_fn_literal_escape(id,
		'capturing fn literal cannot be stored in a container')
}

fn (mut tc TypeChecker) reject_stored_or_returned_capturing_fn_literal(id flat.NodeId) {
	tc.reject_capturing_fn_literal_escape(id, 'capturing fn literal cannot be stored or returned')
}

fn (mut tc TypeChecker) reject_capturing_fn_literal_escape(id flat.NodeId, message string) {
	_ = id
	_ = message
}

// check_selector validates check selector state for types.
fn (mut tc TypeChecker) check_selector(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	if tc.valid_resolution_fast {
		tc.check_valid_selector(id, node)
		return
	}
	if node.value == '$' {
		tc.check_comptime_field_selector(id, node, '', ComptimeStaticFieldCases{})
		return
	}
	if typ := tc.enum_selector_type(&node) {
		tc.register_synth_type(id, typ)
		return
	}
	base_id := tc.a.child(&node, 0)
	base := tc.a.nodes[int(base_id)]
	if base.kind == .selector && base.children_count > 0 {
		module_node := tc.a.child_node(&base, 0)
		if module_node.kind == .ident && tc.has_active_import(module_node.value)
			&& base.value.len > 0 && base.value[0].is_capital() {
			module_name := tc.resolve_import_alias(module_node.value) or { module_node.value }
			display_module_name := tc.current_file_import_path_for_alias(module_node.value) or {
				module_name
			}
			enum_name := '${display_module_name}.${base.value}'
			if resolved_enum_name := tc.resolve_enum_name(enum_name) {
				tc.register_synth_type(base_id, Type(Enum{
					name:    resolved_enum_name
					is_flag: resolved_enum_name in tc.flag_enums
				}))
				if _ := tc.private_declaration(enum_name) {
					start_pos := tc.node_value_diagnostic_pos(base_id)
					tc.record_error_at(.unknown_type, 'enum `${enum_name}` is private', id, token.new_span(node.pos.id,
						start_pos.offset, node.pos.end))
				}
			} else {
				qualified_type_name := '${module_name}.${base.value}'
				if !tc.type_name_known(qualified_type_name)
					&& !tc.type_symbol_known(qualified_type_name) {
					start_pos := tc.node_value_diagnostic_pos(base_id)
					tc.record_error_at(.unknown_type, 'unknown enum `${enum_name}` (type_idx=0)',
						id, token.new_span(node.pos.id, start_pos.offset, node.pos.end))
					tc.register_synth_type(id, Type(void_))
					return
				}
			}
		}
	}
	if typ := tc.enum_selector_type(&node) {
		tc.register_synth_type(id, typ)
		return
	}
	if base.kind == .prefix && base.op == .amp && base.children_count > 0 {
		addressed := tc.a.child_node(&base, 0)
		if addressed.kind == .struct_init {
			tc.check_node(base_id)
			tc.record_error_at(.assignment_mismatch,
				'should not create object instance on the heap to simply access a member', id, token.new_span(node.pos.id,
				base.pos.offset, node.pos.end))
			return
		}
	}
	if tc.is_namespace_selector(node, base) {
		module_name := tc.resolve_import_alias(base.value) or { base.value }
		display_module_name := tc.current_file_import_path_for_alias(base.value) or { module_name }
		semantic_type_name := '${module_name}.${node.value}'
		display_type_name := '${display_module_name}.${node.value}'
		if base.value == 'C' {
			qname := 'C.${node.value}'
			if qname !in tc.c_globals && qname !in tc.const_types && qname !in tc.fn_ret_types
				&& node.value.len > 0 && node.value[0] >= `a` && node.value[0] <= `z`
				&& !ascii_name_has_upper(node.value) {
				if expected := tc.expected_context_for_expr(id) {
					tc.c_globals[qname] = expected
					tc.register_synth_type(id, expected)
					return
				}
				tc.record_error_at(.unknown_ident, 'undefined C identifier: `${qname}`', id,
					tc.node_value_diagnostic_pos(id))
				tc.register_synth_type(id, Type(int_))
				return
			}
			if c_upper_constant_is_pointer(qname) {
				tc.register_synth_type(id, Type(voidptr_))
				return
			}
			if node.value.len > 0 && node.value[0].is_capital()
				&& !tc.static_assoc_type_known(qname) {
				tc.register_synth_type(id, Type(int_))
				return
			}
			// C preprocessor constants do not have V declarations. Like V1, infer
			// conventional all-uppercase macro names as integers.
			if node.value.len > 0 && !ascii_name_has_lower(node.value) {
				tc.register_synth_type(id, Type(int_))
				return
			}
		}
		mut is_known_type := tc.type_name_known(semantic_type_name)
		if !is_known_type {
			for candidate in [
				'${base.value}.${node.value}',
				'${display_module_name.all_after_last('.')}.${node.value}',
			] {
				if tc.type_symbol_known(candidate) {
					is_known_type = true
					break
				}
			}
		}
		if node.value.len > 0 && node.value[0].is_capital() && is_known_type {
			if tc.resolve_enum_name(semantic_type_name) != none
				|| tc.resolve_enum_name(display_type_name) != none {
				parent_id := tc.direct_parent_id(id)
				if tc.valid_node_id(parent_id) {
					parent := tc.a.node(parent_id)
					if parent.kind == .selector && parent.children_count > 0
						&& tc.a.child(parent, 0) == id {
						tc.register_synth_type(id, tc.parse_type(semantic_type_name))
						return
					}
				}
			}
			if key := tc.selector_fn_value_key(node) {
				if typ := tc.fn_type_from_key(key) {
					tc.remember_resolved_fn_value(id, key)
					tc.register_synth_type(id, typ)
					return
				}
			}
			tc.record_error_at(.assignment_mismatch, '`${display_type_name}` must be initialized',
				id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, Type(void_))
			return
		}
		if node.value.len > 0 && node.value[0].is_capital() {
			if enum_name := tc.resolve_enum_name('${display_module_name}.${node.value}') {
				tc.register_synth_type(id, Type(Enum{
					name:    enum_name
					is_flag: enum_name in tc.flag_enums
				}))
				return
			}
		}
		if base.value != 'C' && node.value.len > 0 && node.value[0].is_capital() && is_known_type {
			tc.record_error_at(.assignment_mismatch, '`${display_type_name}` must be initialized',
				id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, Type(void_))
			return
		}
		if deprecation := tc.deprecated_symbols['${module_name}.${node.value}'] {
			tc.record_deprecation(id, 'const', deprecation, tc.node_value_diagnostic_pos(id))
		}
		if key := tc.selector_fn_value_key(node) {
			if typ := tc.fn_type_from_key(key) {
				tc.remember_resolved_fn_value(id, key)
				tc.register_synth_type(id, typ)
			}
		}
		if typ := tc.enum_selector_type(&node) {
			tc.register_synth_type(id, typ)
		}
		if typ := tc.global_type_for_selector(node) {
			qname := '${display_module_name}.${node.value}'
			if _ := tc.private_declaration('${module_name}.${node.value}') {
				tc.record_error_at(.unknown_ident, 'global `${qname}` is private', id,
					tc.node_value_diagnostic_pos(id))
			}
			tc.register_synth_type(id, typ)
			return
		}
		if typ := tc.const_type_for_selector(node) {
			qname := '${display_module_name}.${node.value}'
			if _ := tc.private_declaration(qname) {
				tc.record_error_at(.unknown_type, 'constant `${qname}` is private', id,
					tc.node_value_diagnostic_pos(id))
			}
			tc.register_synth_type(id, typ)
			return
		}
		if base.value == 'C' && c_int_selector_name(node.value) {
			tc.register_synth_type(id, Type(int_))
			return
		}
		if base.value == 'C' && node.value.len > 0 && node.value[0].is_capital() {
			constant_type := if c_upper_constant_is_pointer('C.${node.value}') {
				Type(voidptr_)
			} else {
				Type(int_)
			}
			tc.register_synth_type(id, constant_type)
			return
		}
		if tc.unknown_import_selector(node) {
			mut candidates := []string{}
			prefix := '${module_name}.'
			for name, _ in tc.const_types {
				if name.starts_with(prefix) {
					candidates << name
				}
			}
			name := '${module_name}.${node.value}'
			message := util.new_suggestion(name, candidates).say('undefined ident: `${name}`')
			tc.record_error_at(.unknown_ident, message, id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, Type(void_))
		}
		return
	}
	tc.check_storage_path_base_node(base_id)
	mut base_type := tc.smartcast_type(base_id) or {
		tc.lexical_match_smartcast_type(base_id) or { tc.resolve_type(base_id) }
	}
	if base.kind == .ident {
		if mut_base := tc.mut_param_base_for_current_ident(base.value, base_type) {
			base_type = mut_base
		}
	}
	tc.register_synth_type(base_id, base_type)
	if base_type is Void {
		if tc.fn_context.undefined_variable_context_depth > 0 && tc.errors.any(it.node == base_id
			&& it.kind == .unknown_ident) {
			tc.register_synth_type(id, Type(void_))
			return
		}
		tc.record_error_at(.unknown_field,
			'`${tc.source_text_for_node(base_id)}` does not return a value', id,
			tc.node_value_diagnostic_pos(id))
		tc.record_enclosing_print_void(id)
		tc.register_synth_type(id, Type(void_))
		return
	}
	if base_type is OptionType && node.value !in ['ok', 'value'] {
		tc.record_error_at(.unknown_field,
			'cannot access fields of an Option, handle the error with `or {...}` or propagate it with `?`',
			id, tc.node_value_diagnostic_pos(id))
		tc.register_synth_type(id, Type(void_))
		return
	}
	if smart_type := tc.smartcast_type(id) {
		tc.register_synth_type(id, smart_type)
		return
	}
	if tc.expr_is_method_value(id) && !tc.ident_is_call_callee_or_generic_base(id) {
		tc.check_pointer_receiver_method_value_safety(id, node, base_type)
		receiver := unwrap_pointer(base_type)
		mut generic_method_key := ''
		if receiver is Alias {
			generic_method_key = tc.alias_method_value_decl_key(receiver, node.value) or { '' }
		} else {
			clean_receiver := unalias_type(receiver)
			if clean_receiver is Struct {
				direct_key := '${clean_receiver.name}.${node.value}'
				if direct_key in tc.fn_generic_params {
					generic_method_key = direct_key
				} else if info := tc.resolve_generic_struct_method(clean_receiver.name, node.value) {
					generic_method_key = info.name
				}
			}
		}
		if generic_method_key.len > 0
			&& (tc.fn_generic_params[generic_method_key] or { []string{} }).len > 0 {
			if receiver !is Struct
				|| tc.resolve_generic_struct_method(receiver.name(), node.value) == none {
				tc.record_error_at(.unsupported_generic,
					'cannot use `${tc.source_text_for_node(id)}` as a generic function value', id,
					tc.node_value_diagnostic_pos(id))
				tc.register_synth_type(id, tc.fn_type_from_key(generic_method_key) or {
					Type(void_)
				})
				return
			}
		}
	}
	// A value-context selector whose name is a method (not a field) of a struct
	// receiver is a method value; record the concrete `Type.method` so it survives
	// dead-code elimination (cgen emits a wrapper that calls it).
	union_receiver := unalias_type(unwrap_pointer(base_type))
	if union_receiver is Struct && union_receiver.name in tc.unions && tc.unsafe_depth == 0
		&& !tc.translated_files[tc.cur_file] && !tc.selector_is_assignment_lhs(id) {
		tc.record_warning_at(.unknown_field,
			'reading a union field (or its address) requires `unsafe`', id, tc.selector_field_diagnostic_pos(id,
			node.value))
	}
	clean_recv := unwrap_pointer(base_type)
	selector_is_method_value := tc.expr_is_method_value(id)
		&& !tc.ident_is_call_callee_or_generic_base(id)
	if clean_recv is Struct {
		if deprecation := tc.deprecated_symbols['${clean_recv.name}.${node.value}'] {
			tc.record_deprecation(id, 'field', deprecation, tc.node_value_diagnostic_pos(id))
		}
		field_candidates := tc.embedded_field_candidates(clean_recv.name, node.value)
		if field_candidates.len > 1 {
			pos := tc.node_value_diagnostic_pos(id)
			tc.record_error_at(.unknown_field, 'ambiguous field `${node.value}`', id, pos)
			mut possibilities := field_candidates.clone()
			possibilities.reverse_in_place()
			quoted_possibilities := possibilities.map('`${it}`')
			tc.record_error_at(.unknown_field,
				'type `${clean_recv.name}` has no field named `${node.value}`.\n${possibilities.len} possibilities: ${quoted_possibilities.join(', ')}.',
				id, pos)
			tc.register_synth_type(id, Type(void_))
			return
		}
		if selector_is_method_value && tc.struct_field_type(clean_recv.name, node.value) == none {
			mut mkey := '${clean_recv.name}.${node.value}'
			mut is_generic_method_value := false
			if mkey !in tc.fn_param_types {
				// Generic receiver methods are registered under the open key
				// (`Box[T].method`); mark that one reachable for the wrapper, and stash
				// the substituted signature for cgen (the open form is gone by cgen time).
				if ci := tc.resolve_generic_struct_method(clean_recv.name, node.value) {
					tc.generic_method_value_info['${clean_recv.name}.${node.value}'] = ci
					mkey = ci.name
					is_generic_method_value = true
				}
			}
			if (mkey in tc.fn_param_types || is_generic_method_value) && tc.fn_context.node_id >= 0 {
				// Record per enclosing function so markused marks it only when that
				// function is reachable; over-marking an unreachable method value can pull
				// in (and fail to compile) an otherwise-unused specialization. A method
				// value can only appear inside a function body — escaping to a const/global
				// is rejected elsewhere — so a non-fn context needs no recording here.
				tc.method_values_by_fn[tc.fn_context.node_id] << mkey
				// Also record the concrete instance key (`Box[int].report`) — distinct from the
				// open key (`Box[T].report`) above — so monomorphize can gate a generic method's
				// specialization on *this* instance's method value being reachable (it shares the
				// open key with every other instance, e.g. `Box[Pair]`).
				concrete_mkey := '${clean_recv.name}.${node.value}'
				if concrete_mkey != mkey {
					tc.method_values_by_fn[tc.fn_context.node_id] << concrete_mkey
				}
			}
		}
	} else if selector_is_method_value && clean_recv is Alias {
		underlying := unalias_type(clean_recv)
		has_field := underlying is Struct
			&& tc.struct_field_type(underlying.name, node.value) != none
		if !has_field && tc.fn_context.node_id >= 0 {
			if mkey := tc.alias_method_value_decl_key(clean_recv, node.value) {
				tc.method_values_by_fn[tc.fn_context.node_id] << mkey
			}
		}
	} else if selector_is_method_value && clean_recv is Interface {
		if tc.interface_field_type(clean_recv.name, node.value) == none
			&& tc.fn_context.node_id >= 0 {
			if _ := tc.interface_receiver_method_call_info(clean_recv.name, node.value) {
				tc.method_values_by_fn[tc.fn_context.node_id] << '${clean_recv.name}.${node.value}'
			}
		}
	}
	if typ := tc.selector_type(id, node) {
		tc.register_synth_type(id, typ)
		return
	} else {
		if tc.should_diagnose(id) {
			if base_type is Unknown {
				return
			}
			clean_base := unalias_type(unwrap_pointer(base_type))
			if clean_base is Interface {
				tc.record_error_at(.unknown_field,
					'type `${clean_base.name}` has no field named `${node.value}`', id,
					tc.node_value_diagnostic_pos(id))
			} else if clean_base is Struct {
				if tc.ident_is_multi_pattern_match_subject(base_id) {
					tc.record_error_at(.unknown_field,
						'type `${clean_base.name}` has no field or method `${node.value}`', id,
						tc.node_value_diagnostic_pos(id))
					tc.register_synth_type(id, Type(void_))
					return
				}
				mut candidates := []string{}
				for field in tc.struct_fields_for_init(clean_base.name) {
					if field.name.len > 0 && field.name !in candidates {
						candidates << field.name
					}
				}
				message := 'unknown field `${node.value}` on `${clean_base.name}`; ' +
					util.new_suggestion(node.value, candidates).say('type `${clean_base.name}` has no field named `${node.value}`')
				tc.record_error_at(.unknown_field, message, id, tc.node_value_diagnostic_pos(id))
			} else if clean_base is SumType {
				tc.record_error_at(.unknown_field,
					'unknown field `${node.value}` on `${clean_base.name}`; field `${node.value}` does not exist or have the same type in these sumtype `${clean_base.name}` variants:',
					id, tc.node_value_diagnostic_pos(id))
			} else {
				tc.record_error_at(.unknown_field,
					'`${base_type.name()}` has no property `${node.value}`', id,
					tc.node_value_diagnostic_pos(id))
			}
		}
		tc.register_synth_type(id, Type(void_))
	}
}

fn ascii_name_has_upper(name string) bool {
	for ch in name {
		if ch >= `A` && ch <= `Z` {
			return true
		}
	}
	return false
}

fn ascii_name_has_lower(name string) bool {
	for ch in name {
		if ch >= `a` && ch <= `z` {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) selector_is_assignment_lhs(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	return parent.kind in [.assign, .selector_assign, .index_assign] && parent.children_count > 0
		&& tc.a.child(parent, 0) == id
}

fn (tc &TypeChecker) ident_is_multi_pattern_match_subject(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	ident := tc.a.node(id)
	if ident.kind != .ident {
		return false
	}
	mut current := id
	for _ in 0 .. 32 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .match_branch {
			if parent.value == 'else' || parent.value.int() < 2 {
				return false
			}
			match_id := tc.direct_parent_id(parent_id)
			if !tc.valid_node_id(match_id) {
				return false
			}
			match_node := tc.a.node(match_id)
			if match_node.kind != .match_stmt || match_node.children_count == 0 {
				return false
			}
			subject := tc.a.child_node(match_node, 0)
			return subject.kind == .ident && subject.value == ident.value
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		current = parent_id
	}
	return false
}

fn (tc &TypeChecker) current_fn_has_invalid_defer_mode() bool {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	fn_node := tc.a.node(fn_id)
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		stack << tc.a.child(fn_node, i)
	}
	for stack.len > 0 {
		id := stack.pop()
		node := tc.a.node(id)
		if node.kind == .defer_stmt && node.value.starts_with('invalid:') {
			return true
		}
		for i in 0 .. node.children_count {
			stack << tc.a.child(node, i)
		}
	}
	return false
}

fn (tc &TypeChecker) current_fn_is_specialized_generic() bool {
	fn_id := tc.fn_context.node_id
	return fn_id >= 0
		&& ((fn_id < tc.a.specialized_fn_nodes.len && tc.a.specialized_fn_nodes[fn_id])
		|| (fn_id < tc.a.nodes.len && tc.specialized_generic_fns[tc.a.nodes[fn_id].value]))
}

fn (mut tc TypeChecker) record_enclosing_print_void(id flat.NodeId) {
	mut current := id
	for _ in 0 .. 4 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .call && parent.children_count > 0 {
			callee := tc.a.child_node(parent, 0)
			if callee.kind == .ident && callee.value in ['print', 'println', 'eprint', 'eprintln'] {
				tc.record_error(.call_arg_mismatch,
					'`${callee.value}` can not print void expressions', parent_id)
			}
			return
		}
		current = parent_id
	}
}

fn (mut tc TypeChecker) record_enclosing_dump_void(id flat.NodeId) {
	parent_id := tc.direct_parent_id(id)
	if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .dump_expr {
		node := tc.a.node(id)
		if node.kind == .call && node.children_count > 0 {
			callee := tc.a.child_node(node, 0)
			if callee.kind == .selector {
				tc.record_error_at(.assignment_mismatch, 'dump expression can not be void', id, tc.method_call_name_pos(node,
					callee))
				return
			}
		}
		tc.record_error(.assignment_mismatch, 'dump expression can not be void', id)
	}
}

fn (tc &TypeChecker) defer_mode_diagnostic_pos(node flat.Node, mode string) token.Pos {
	file := tc.a.source_files[node.pos.id] or { return node.pos }
	source := tc.source_texts_by_file[file.name] or { return node.pos }
	start := int_max(0, node.pos.offset)
	end := int_min(source.len, node.pos.end)
	if relative := source[start..end].index('(${mode})') {
		mode_start := start + relative + 1
		return token.new_span(node.pos.id, mode_start, mode_start + mode.len)
	}
	return node.pos
}

fn (tc &TypeChecker) invalid_interface_selector_print_arg(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_type := unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(node, 0))))
	return base_type is Interface && tc.selector_type(id, node) == none
}

fn (tc &TypeChecker) invalid_struct_selector_print_arg(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_type := unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(node, 0))))
	return base_type is Struct && tc.selector_type(id, node) == none
}

fn (tc &TypeChecker) unknown_import_selector(node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base := tc.a.child_node(&node, 0)
	return base.kind == .ident && tc.has_active_import(base.value)
		&& tc.global_type_for_selector(node) == none && tc.const_type_for_selector(node) == none
		&& tc.selector_fn_value_key(node) == none && tc.enum_selector_type(&node) == none
}

// struct_embed_receiver_names returns the embedded receiver type names of a
// struct, from the shared index built after collect. Structs registered later
// (monomorphization) miss the index and use the field walk directly.
fn (tc &TypeChecker) struct_embed_receiver_names(struct_name string) []string {
	return tc.struct_embed_receivers[struct_name] or {
		tc.compute_struct_embed_receivers(struct_name)
	}
}

fn (tc &TypeChecker) compute_struct_embed_receivers(struct_name string) []string {
	fields := tc.structs[struct_name] or { return []string{} }
	mut receivers := []string{}
	for field in fields {
		embedded_type := embedded_field_type(field) or { continue }
		receiver := method_type_name(unwrap_pointer(embedded_type))
		if receiver.len == 0 {
			continue
		}
		receivers << receiver
	}
	return receivers
}

fn (mut tc TypeChecker) build_struct_embed_index() {
	tc.struct_embed_receivers = map[string][]string{}
	for struct_name, _ in tc.structs {
		tc.struct_embed_receivers[struct_name] = tc.compute_struct_embed_receivers(struct_name)
	}
}

fn (tc &TypeChecker) embedded_method_candidates(struct_name string, method string) []string {
	if tc.struct_embed_receiver_names(struct_name).len == 0 {
		return []string{}
	}
	mut candidates := []string{}
	mut seen := map[string]bool{}
	tc.collect_embedded_method_candidates(struct_name, method, mut candidates, mut seen)
	return candidates
}

fn (tc &TypeChecker) collect_embedded_method_candidates(struct_name string, method string, mut candidates []string,
	mut seen map[string]bool) {
	if struct_name in seen {
		return
	}
	seen[struct_name] = true
	for receiver in tc.struct_embed_receiver_names(struct_name) {
		if '${receiver}.${method}' in tc.fn_ret_types && receiver !in candidates {
			candidates << receiver
		}
		tc.collect_embedded_method_candidates(receiver, method, mut candidates, mut seen)
	}
}

fn (tc &TypeChecker) embedded_field_candidates(struct_name string, field_name string) []string {
	if tc.struct_embed_receiver_names(struct_name).len == 0 {
		return []string{}
	}
	mut candidates := []string{}
	mut seen := map[string]bool{}
	tc.collect_embedded_field_candidates(struct_name, field_name, mut candidates, mut seen)
	return candidates
}

fn (tc &TypeChecker) collect_embedded_field_candidates(struct_name string, field_name string, mut candidates []string,
	mut seen map[string]bool) {
	if struct_name in seen {
		return
	}
	seen[struct_name] = true
	for receiver in tc.struct_embed_receiver_names(struct_name) {
		for embedded_field in tc.structs[receiver] or { []StructField{} } {
			if embedded_field.name == field_name && embedded_field_type(embedded_field) == none
				&& receiver !in candidates {
				candidates << receiver
				break
			}
		}
		tc.collect_embedded_field_candidates(receiver, field_name, mut candidates, mut seen)
	}
}

// is_namespace_selector reports whether is namespace selector applies in types.
fn (tc &TypeChecker) is_namespace_selector(node flat.Node, base flat.Node) bool {
	if base.kind != .ident {
		return false
	}
	if tc.ident_resolves_to_value(base.value) {
		return false
	}
	if base.value == 'C' || tc.has_active_import(base.value) {
		return true
	}
	if base.value == 'main' || base.value == tc.cur_module {
		if node.value in tc.const_types || '${base.value}.${node.value}' in tc.const_types {
			return true
		}
	}
	if resolved := tc.resolve_selective_import_type_symbol(base.value) {
		if resolved in tc.structs || resolved in tc.enum_names || resolved in tc.flag_enums
			|| resolved in tc.sum_types || resolved in tc.interface_names {
			return true
		}
	}
	qbase := tc.qualify_name(base.value)
	if qbase in tc.structs || qbase in tc.enum_names || qbase in tc.sum_types
		|| qbase in tc.interface_names || qbase in tc.type_aliases {
		return true
	}
	// An alias of an enum (`type Col = Color`) is a namespace for its members: `Col.member`.
	if _ := tc.resolve_enum_name(base.value) {
		return true
	}
	if _ := tc.static_assoc_fn_key_for_base(base.value, node.value) {
		return true
	}
	qname := '${qbase}.${node.value}'
	return qname in tc.const_types || qname in tc.fn_ret_types || qname in tc.enum_names
}

// selector_type supports selector type handling for TypeChecker.
fn (tc &TypeChecker) selector_type(_id flat.NodeId, node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	base_id := tc.a.child(&node, 0)
	base_node := tc.a.nodes[int(base_id)]
	if base_node.kind == .ident && base_node.value == 'C' && c_int_selector_name(node.value) {
		return Type(int_)
	}
	if typ := tc.enum_selector_type(&node) {
		return typ
	}
	if base_node.kind == .typeof_expr {
		if node.value == 'name' {
			return Type(String{})
		}
		if node.value == 'idx' {
			return Type(int_)
		}
		if node.value == 'indirections' {
			return Type(u8_)
		}
	}
	mut has_smartcast := false
	mut base_type := tc.resolve_type(base_id)
	if smartcast := tc.smartcast_type(base_id) {
		base_type = smartcast
		has_smartcast = true
	} else if smartcast := tc.lexical_match_smartcast_type(base_id) {
		base_type = smartcast
		has_smartcast = true
	}
	if base_node.kind == .ident && !has_smartcast {
		if scoped_type := tc.cur_scope.lookup(base_node.value) {
			base_type = scoped_type
		}
		if mut_base := tc.mut_param_base_for_current_ident(base_node.value, base_type) {
			base_type = mut_base
		}
	}
	clean0 := unwrap_pointer(base_type)
	mut alias_receiver_name := ''
	if clean0 is Alias {
		alias_receiver_name = clean0.name
	}
	clean := unalias_and_unwrap_pointer_type(base_type)
	clean_name := clean.name()
	if typ := option_result_selector_type(clean, node.value) {
		return typ
	}
	if node.value == 'len' {
		if clean is Array || clean is Map || clean is String || clean is ArrayFixed {
			return Type(int_)
		}
	}
	if clean is Channel && node.value == 'closed' {
		return Type(bool_)
	}
	if clean is Channel && node.value in ['len', 'cap'] {
		return Type(int_)
	}
	if clean is Struct {
		if typ := tc.struct_field_type(clean_name, node.value) {
			return typ
		}
		if typ := tc.method_value_type(clean_name, node.value) {
			return typ
		}
	}
	if base_node.kind == .string_literal {
		method_name := 'string.${node.value}'
		if method_name in tc.fn_param_types || method_name in tc.fn_ret_types {
			return tc.method_value_type('string', node.value)
		}
	}
	if clean is Array || clean is Map || clean is String {
		// A declared field (e.g. `string.str &u8`) shadows the builtin method
		// of the same name for selector access.
		sname := if clean is Array {
			'array'
		} else if clean is Map {
			'map'
		} else {
			'string'
		}
		for f in tc.structs[sname] or { []StructField{} } {
			if f.name == node.value {
				return f.typ
			}
		}
	}
	if typ := tc.builtin_method_value_type(base_type, node.value) {
		return typ
	}
	if alias_receiver_name.len > 0 {
		if typ := tc.method_value_type(alias_receiver_name, node.value) {
			return typ
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.name, node.value) {
			return typ
		}
		if typ := tc.method_value_type(clean.name, node.value) {
			return typ
		}
	}
	if clean is MultiReturn {
		if typ := multi_return_selector_type(clean, node.value) {
			return typ
		}
	}
	if is_builtin_ierror_name(clean_name) {
		if node.value == 'message' {
			return Type(String{})
		}
		if node.value == '_object' {
			return tc.parse_type('voidptr')
		}
	}
	if clean is SumType {
		if typ := tc.sum_shared_field_type(clean, node.value) {
			return typ
		}
		if base_node.kind == .index {
			if typ := tc.sum_unique_variant_field_type(clean, node.value) {
				return typ
			}
		}
		if typ := tc.lowered_sum_selector_type(clean, node.value) {
			return typ
		}
	}
	if clean is Array || clean is Map || clean is String {
		sname := if clean is Array {
			'array'
		} else if clean is Map {
			'map'
		} else {
			'string'
		}
		if fields := tc.structs[sname] {
			for f in fields {
				if f.name == node.value {
					return f.typ
				}
			}
		}
	}
	return none
}

// multi_return_selector_type supports multi return selector type handling for types.
fn multi_return_selector_type(typ MultiReturn, field string) ?Type {
	if !field.starts_with('arg') || field.len <= 3 {
		return none
	}
	idx_str := field[3..]
	idx := idx_str.int()
	if idx_str != idx.str() || idx < 0 || idx >= typ.types.len {
		return none
	}
	return typ.types[idx]
}

// lowered_sum_selector_type supports lowered sum selector type handling for TypeChecker.
fn (tc &TypeChecker) lowered_sum_selector_type(sum SumType, field string) ?Type {
	if field == 'typ' {
		return Type(int_)
	}
	variants := tc.sum_types[sum.name] or { return none }
	for variant in variants {
		short := if variant.contains('.') { variant.all_after_last('.') } else { variant }
		if field == variant || field == short || field == tc.cached_c_name(variant) {
			return tc.parse_type(variant)
		}
	}
	return none
}

// sum_shared_field_type supports sum shared field type handling for TypeChecker.
fn (tc &TypeChecker) sum_shared_field_type(sum SumType, field string) ?Type {
	mut visited := map[string]bool{}
	return tc.sum_shared_field_type_inner(sum.name, field, mut visited)
}

fn (tc &TypeChecker) sum_shared_field_type_inner(sum_name string, field string, mut visited map[string]bool) ?Type {
	base := tc.sum_base_name(sum_name)
	if visited[base] {
		return none
	}
	visited[base] = true
	defer {
		visited.delete(base)
	}
	variants := tc.sum_types[base] or { return none }
	if variants.len == 0 {
		return none
	}
	mut has_common := false
	mut common_typ := Type(void_)
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(sum_name, variant)
		variant_type := unalias_type(tc.parse_type(concrete))
		variant_field := if variant_type is SumType {
			tc.sum_shared_field_type_inner(variant_type.name, field, mut visited) or { return none }
		} else if variant_type is Struct {
			tc.struct_field_type(variant_type.name, field) or { return none }
		} else {
			return none
		}
		if !has_common {
			common_typ = variant_field
			has_common = true
			continue
		}
		if variant_field.name() != common_typ.name() {
			return none
		}
	}
	return common_typ
}

fn (tc &TypeChecker) sum_unique_variant_field_type(sum SumType, field string) ?Type {
	mut visited := map[string]bool{}
	return tc.sum_unique_variant_field_type_inner(sum.name, field, mut visited)
}

fn (tc &TypeChecker) sum_unique_variant_field_type_inner(sum_name string, field string, mut visited map[string]bool) ?Type {
	base := tc.sum_base_name(sum_name)
	if visited[base] {
		return none
	}
	visited[base] = true
	defer {
		visited.delete(base)
	}
	variants := tc.sum_types[base] or { return none }
	mut found := Type(void_)
	mut found_count := 0
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(sum_name, variant)
		variant_type := unalias_type(tc.parse_type(concrete))
		variant_field := if variant_type is SumType {
			tc.sum_unique_variant_field_type_inner(variant_type.name, field, mut visited) or {
				continue
			}
		} else if variant_type is Struct {
			tc.struct_field_type(variant_type.name, field) or { continue }
		} else {
			continue
		}
		found = variant_field
		found_count++
		if found_count > 1 {
			return none
		}
	}
	if found_count == 1 {
		return found
	}
	return none
}

fn (tc &TypeChecker) sum_type_contains_variant(sum SumType, target Type) bool {
	mut visited := map[string]bool{}
	return tc.sum_type_contains_variant_seen(sum, target, mut visited)
}

fn (tc &TypeChecker) sum_type_contains_variant_seen(sum SumType, target Type, mut visited map[string]bool) bool {
	base := tc.sum_base_name(sum.name)
	if visited[base] {
		return false
	}
	visited[base] = true
	for variant in tc.sum_types[base] or { []string{} } {
		concrete := tc.parse_type(tc.concrete_sum_variant_name(sum.name, variant))
		if concrete.name() == target.name()
			|| (target is Alias && tc.type_compatible(concrete, target.base_type)) {
			return true
		}
		if concrete is SumType && tc.sum_type_contains_variant_seen(concrete, target, mut visited) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) sum_variant_diagnostic_name(name string) string {
	clean := trimmed_space(name)
	if !clean.contains('.') {
		return clean
	}
	module_name := clean.all_before_last('.')
	short := clean.all_after_last('.')
	if module_name in ['', 'main', 'builtin', tc.cur_module] {
		return short
	}
	if resolved := tc.resolve_import_alias(module_name) {
		return '${resolved.all_after_last('.')}.${short}'
	}
	return '${module_name.all_after_last('.')}.${short}'
}

// option_result_selector_type supports option result selector type handling for types.
fn option_result_selector_type(typ Type, field string) ?Type {
	if typ is OptionType {
		if field == 'ok' {
			return Type(bool_)
		}
		if field == 'value' {
			return typ.base_type
		}
	}
	if typ is ResultType {
		if field == 'ok' {
			return Type(bool_)
		}
		if field == 'value' {
			return typ.base_type
		}
	}
	return none
}

pub fn (tc &TypeChecker) index_operator_call_info(base_type Type, op string) ?CallInfo {
	clean := unwrap_pointer(base_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name.len == 0 {
		return none
	}
	if info := tc.resolve_generic_struct_method(type_name, op) {
		if info.params.len == 0
			|| tc.method_receiver_compatible(base_type, info.params[0], info.name) {
			return info
		}
	}
	for method_name in receiver_method_name_candidates(clean, op, tc.cur_module) {
		for candidate in [method_name, tc.cached_c_name(method_name)] {
			if candidate !in tc.fn_ret_types && candidate !in tc.fn_param_types {
				continue
			}
			info := tc.call_info(candidate, true)
			if info.params.len > 0
				&& !tc.method_receiver_compatible(base_type, info.params[0], candidate) {
				continue
			}
			return info
		}
	}
	return none
}

fn (mut tc TypeChecker) check_index_overload_arg(id flat.NodeId, node flat.Node, info CallInfo, op string) bool {
	if node.children_count != 2 {
		if tc.should_diagnose(id) {
			tc.record_error(.cannot_index,
				'overloaded `${op}` expression accepts one index, got ${node.children_count - 1}',
				id)
		}
		return false
	}
	if info.params.len < 2 {
		if tc.should_diagnose(id) {
			tc.record_error(.cannot_index, 'overloaded `${op}` is missing an index parameter', id)
		}
		return false
	}
	index_id := tc.a.child(&node, 1)
	tc.check_node(index_id)
	expected_key := info.params[1]
	actual_key := tc.resolve_expr(index_id, expected_key)
	if !tc.type_compatible(actual_key, expected_key) {
		tc.type_mismatch(.cannot_index,
			'index must be `${expected_key.name()}`, not `${actual_key.name()}`', index_id)
		return false
	}
	return true
}

// check_index validates check index state for types.
fn (mut tc TypeChecker) check_index(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	generic_base_node := tc.a.child_node(&node, 0)
	if name := tc.generic_call_base_name(generic_base_node) {
		type_args := tc.generic_call_type_arg_names(node)
		mut has_unresolved_generic := false
		for type_arg in type_args {
			if is_bare_generic_param(type_arg) && !tc.active_generic_param(type_arg) {
				has_unresolved_generic = true
				break
			}
		}
		if has_unresolved_generic && (tc.fn_generic_params[name] or { []string{} }).len > 0 {
			base_id := tc.a.child(&node, 0)
			tc.record_error_at(.unsupported_generic,
				'a generic fn with generic types, cannot be used outside of another generic fn',
				base_id, tc.node_value_diagnostic_pos(base_id))
			tc.register_synth_type(id, tc.fn_type_from_key(name) or { Type(void_) })
			return
		}
	}
	if generic_fn_type := tc.explicit_generic_fn_value_type(node) {
		tc.register_synth_type(id, generic_fn_type)
		return
	}
	base_id := tc.a.child(&node, 0)
	tc.check_storage_path_base_node(base_id)
	mut base_type_raw := tc.resolve_type(base_id)
	if base_type_raw is OptionType && node.op == .gated_index {
		base_type_raw = base_type_raw.base_type
	} else if base_type_raw is OptionType {
		base_node := tc.a.node(base_id)
		base_text := tc.source_text_for_node(base_id)
		base_pos := if base_node.kind == .selector {
			tc.node_value_diagnostic_pos(base_id)
		} else {
			base_node.pos
		}
		message := if base_node.kind == .call {
			'type `?${base_type_raw.base_type.name()}` is an Option, it must be unwrapped with `func()?`, or use `func() or {default}`'
		} else if tc.direct_parent_kind(id) == .index_assign {
			'type `?${base_type_raw.base_type.name()}` is an Option, it must be unwrapped first; use `var?[]` to do it'
		} else {
			'type `?${base_type_raw.base_type.name()}` is an Option, it must be unwrapped first; use `${base_text}?` to do it'
		}
		tc.record_error_at(.cannot_index, message, base_id, base_pos)
		if base_node.kind == .selector && !tc.index_is_handled_by_guard_or_or_block(id) {
			tc.record_error_at(.cannot_index,
				'field `${base_node.value}` is an Option, so it should have either an `or {}` block, or `?` at the end',
				base_id, base_pos)
		}
		tc.register_synth_type(id, Type(void_))
		return
	}
	mut base_type := unalias_and_unwrap_pointer_type(base_type_raw)
	if map_alias := tc.map_type_from_alias_target(base_type_raw) {
		base_type = Type(map_alias)
	} else if map_alias := tc.map_type_from_alias_target(base_type) {
		base_type = Type(map_alias)
	}
	if base_type_raw.name() in ['voidptr', 'nil'] {
		type_name := tc.pointer_diagnostic_binding_type_name(base_id, base_type_raw)
		tc.record_error_at(.cannot_index, 'cannot index `${type_name}`', id,
			tc.index_brackets_pos(node))
		tc.register_synth_type(id, Type(void_))
		return
	}
	pointer_base_node := tc.a.node(base_id)
	mut_param_base := if pointer_base_node.kind == .ident {
		unalias_type(tc.fn_context.mut_param_base_types[pointer_base_node.value] or { Type(void_) })
	} else {
		Type(void_)
	}
	// Ordinary `mut x T` parameters are represented as pointers in the ABI but retain
	// value semantics in the function body. An explicitly pointer-typed `mut x &T`
	// parameter retains pointer semantics, including indexing.
	explicit_mut_param_pointer := pointer_base_node.kind == .ident
		&& tc.current_fn_param_is_explicit_mut_pointer(pointer_base_node.value)
	implicit_mut_param_pointer := mut_param_base !is Void && mut_param_base !is Pointer
		&& !explicit_mut_param_pointer
	outside_unsafe := tc.unsafe_depth == 0 && !tc.expr_is_inside_unsafe_block(id)
	if node.value != 'range' && base_type_raw is Pointer && implicit_mut_param_pointer
		&& base_type is Struct && outside_unsafe {
		if tc.fn_context.generic_params.len > 0 && base_type.name.contains('[') {
			tc.register_synth_type(id, tc.resolve_index_type(node))
			return
		}
		tc.record_error_at(.cannot_index, 'type `mut ${base_type.name}` does not support slicing',
			id, tc.index_brackets_pos(node))
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if node.value != 'range' && base_type_raw is Pointer && !implicit_mut_param_pointer
		&& !explicit_mut_param_pointer && mut_param_base !is Pointer && base_type !is Array
		&& base_type !is ArrayFixed && base_type !is Map && base_type !is String && outside_unsafe {
		tc.record_error_at(.cannot_index, 'pointer indexing is only allowed in `unsafe` blocks',
			id, tc.index_brackets_pos(node))
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if info := tc.index_overload_call_info(base_type_raw, false) {
		tc.check_index_overload_args(id, node, info)
		tc.register_synth_type(id, info.return_type)
		return
	}
	if node.value == 'range' {
		mut range_out_of_bounds_reported := false
		if !(base_type is Array || base_type is ArrayFixed || base_type is String)
			&& tc.should_diagnose(id) {
			base_node := tc.a.node(base_id)
			type_name := if base_node.kind == .cast_expr
				&& base_node.value in ['byteptr', 'charptr', 'voidptr'] {
				base_node.value
			} else {
				base_type_raw.name()
			}
			action := if base_type_raw is Pointer { 'slicing' } else { 'indexing' }
			tc.record_error_at(.cannot_index, 'type `${type_name}` does not support ${action}', id,
				tc.index_brackets_pos(node))
		}
		for i in 1 .. node.children_count {
			bound_id := tc.a.child(&node, i)
			if int(bound_id) >= 0 {
				bound := tc.a.nodes[int(bound_id)]
				if bound.kind == .empty {
					continue
				}
				tc.check_node(bound_id)
				bound_type := unalias_type(tc.resolve_type(bound_id))
				if bound_type !is Unknown && !bound_type.is_integer() {
					type_name := tc.diagnostic_expr_type_name(bound_id, bound_type)
					message := if base_type is String {
						'non-integer string index `${type_name}`'
					} else {
						'non-integer index `${type_name}` (array type `${base_type.name()}`)'
					}
					tc.type_mismatch(.cannot_index, message, bound_id)
				} else {
					if node.op != .gated_index {
						if value := tc.index_literal_value(bound_id) {
							if value < 0 {
								tc.record_error(.cannot_index, 'negative index `${value}`',
									bound_id)
							} else if base_type is ArrayFixed {
								if length := tc.fixed_array_len_value(base_type) {
									if value > length && !range_out_of_bounds_reported {
										tc.record_error(.cannot_index,
											'index out of range (index: ${value}, len: ${length})',
											bound_id)
										range_out_of_bounds_reported = true
									}
								}
							}
						}
					}
				}
			}
		}
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if node.children_count > 2 {
		for i in 2 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.record_error(.cannot_index,
			'index expression accepts one index, got ${node.children_count - 1}', id)
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if node.children_count > 1 {
		index_id := tc.a.child(&node, 1)
		index_error_count := tc.errors.len
		tc.check_node(index_id)
		if base_type is Map {
			actual_key := tc.resolve_expr(index_id, base_type.key_type)
			if actual_key is OptionType {
				index_node := tc.a.node(index_id)
				pos := if index_node.kind == .call && index_node.children_count > 0 {
					callee := tc.a.child_node(index_node, 0)
					if callee.kind == .selector {
						tc.method_call_name_pos(*index_node, callee)
					} else {
						index_node.pos
					}
				} else {
					index_node.pos
				}
				tc.record_error_at(.cannot_index,
					'invalid key: cannot use `?${actual_key.base_type.name()}` as `${base_type.key_type.name()}`, it must be unwrapped first',
					index_id, pos)
			} else if !tc.expr_subtree_has_undefined_variable_error(index_id)
				&& !tc.map_key_type_compatible(actual_key, base_type.key_type) {
				actual_name := tc.diagnostic_expr_type_name(index_id, actual_key)
				mut message := 'invalid key: expected `${base_type.key_type.name()}`, not `${actual_name}`'
				clean_actual_key := unalias_type(actual_key)
				if clean_actual_key is Pointer {
					if tc.type_compatible(clean_actual_key.base_type, base_type.key_type) {
						base_text := tc.source_text_for_node(base_id)
						index_text := tc.source_text_for_node(index_id)
						message += '; did you mean `${base_text}[*${index_text}]`?'
					}
				}
				tc.record_error_at(.cannot_index, message, index_id, token.new_span(node.pos.id,
					tc.a.node(base_id).pos.end, node.pos.end))
			}
			if unalias_type(base_type.value_type) is SumType
				&& !tc.index_is_handled_by_guard_or_or_block(id)
				&& !tc.index_is_assignment_target(id) {
				tc.record_warning_at(.cannot_index,
					'`or {}` block required when indexing a map with sum type value', id, token.new_span(node.pos.id,
					tc.a.node(base_id).pos.end, node.pos.end))
			}
			if tc.cur_file in tc.strict_map_index_files
				&& !tc.index_is_handled_by_guard_or_or_block(id)
				&& !tc.index_is_assignment_target(id) {
				tc.record_error_at(.cannot_index,
					'`@[strict_map_index]` requires handling missing map keys with `or {}` or `if value := map[key] {}`',
					id, tc.index_brackets_pos(node))
			}
			if tc.unsafe_depth == 0 && !tc.index_is_handled_by_guard_or_or_block(id)
				&& !tc.index_is_assignment_target(id) {
				value_type := unalias_type(base_type.value_type)
				index_pos := token.new_span(node.pos.id, tc.a.node(base_id).pos.end, node.pos.end)
				if value_type is Pointer {
					tc.record_warning_at(.cannot_index,
						'accessing a pointer map value requires an `or {}` block outside `unsafe`',
						id, index_pos)
				}
				mut visited := map[string]bool{}
				if tc.type_contains_pointer(value_type, mut visited) {
					tc.record_warning_at(.cannot_index,
						'accessing map value that contain pointers requires an `or {}` block outside `unsafe`',
						id, index_pos)
				}
			}
			tc.register_synth_type(id, base_type.value_type)
			return
		}
		if getter := tc.index_operator_call_info(base_type_raw, '[]') {
			tc.check_index_overload_arg(id, node, getter, '[]')
			tc.register_synth_type(id, getter.return_type)
			return
		}
		index_type := unalias_type(tc.resolve_type(index_id))
		if index_type is Unknown && tc.new_error_kind_since(index_error_count, .unknown_ident)
			&& (base_type is Array || base_type is ArrayFixed) {
			tc.type_mismatch(.cannot_index,
				'non-integer index `void` (array type `${base_type.name()}`)', index_id)
		} else if index_type !is Unknown && index_type !is Enum && !index_type.is_integer() {
			if index_type is OptionType || index_type is ResultType {
				tc.type_mismatch(.cannot_index,
					'cannot use Option or Result as index (array type `${base_type.name()}`)',
					index_id)
			} else {
				type_name := tc.diagnostic_expr_type_name(index_id, index_type)
				message := if base_type is String {
					'non-integer string index `${type_name}`'
				} else {
					'non-integer index `${type_name}` (array type `${base_type.name()}`)'
				}
				tc.type_mismatch(.cannot_index, message, index_id)
			}
		} else if node.op != .gated_index {
			if value := tc.index_literal_value(index_id) {
				if value < 0 {
					tc.record_error(.cannot_index, 'negative index `${value}`', index_id)
				} else if base_type is ArrayFixed {
					if length := tc.fixed_array_len_value(base_type) {
						if value >= length {
							tc.record_error(.cannot_index,
								'index out of range (index: ${value}, len: ${length})', index_id)
						}
					}
				}
			}
		}
	}
	if !(base_type is Array || base_type is ArrayFixed || base_type is String
		|| base_type is Map || base_type is Unknown
		|| (base_type_raw is Pointer && !implicit_mut_param_pointer)) && tc.should_diagnose(id) {
		tc.record_error_at(.cannot_index, 'cannot index `${base_type.name()}`', id,
			tc.index_brackets_pos(node))
	}
	tc.register_synth_type(id, tc.resolve_index_type(node))
}

fn (mut tc TypeChecker) check_valid_selector(id flat.NodeId, node flat.Node) {
	if node.value == '$' {
		tc.check_comptime_field_selector(id, node, '', ComptimeStaticFieldCases{})
		return
	}
	if typ := tc.enum_selector_type(&node) {
		tc.register_synth_type(id, typ)
		return
	}
	base_id := tc.a.child(&node, 0)
	base := tc.a.nodes[int(base_id)]
	if base.kind == .selector && base.children_count > 0 {
		module_node := tc.a.child_node(&base, 0)
		if module_node.kind == .ident && tc.has_active_import(module_node.value)
			&& base.value.len > 0 && base.value[0].is_capital() {
			module_name := tc.resolve_import_alias(module_node.value) or { module_node.value }
			if resolved_enum_name := tc.resolve_enum_name('${module_name}.${base.value}') {
				tc.register_synth_type(base_id, Type(Enum{
					name:    resolved_enum_name
					is_flag: resolved_enum_name in tc.flag_enums
				}))
			}
		}
	}
	if typ := tc.enum_selector_type(&node) {
		tc.register_synth_type(id, typ)
		return
	}
	if tc.is_namespace_selector(node, base) {
		module_name := tc.resolve_import_alias(base.value) or { base.value }
		if base.value == 'C' {
			qname := 'C.${node.value}'
			if qname !in tc.c_globals && qname !in tc.const_types && qname !in tc.fn_ret_types
				&& node.value.len > 0 && node.value[0] >= `a` && node.value[0] <= `z`
				&& !ascii_name_has_upper(node.value) {
				if expected := tc.expected_context_for_expr(id) {
					tc.c_globals[qname] = expected
					tc.register_synth_type(id, expected)
				} else {
					tc.register_synth_type(id, Type(int_))
				}
				return
			}
			if c_upper_constant_is_pointer(qname) {
				tc.register_synth_type(id, Type(voidptr_))
				return
			}
		}
		if key := tc.selector_fn_value_key(node) {
			if typ := tc.fn_type_from_key(key) {
				tc.remember_resolved_fn_value(id, key)
				tc.register_synth_type(id, typ)
				return
			}
		}
		if typ := tc.enum_selector_type(&node) {
			tc.register_synth_type(id, typ)
			return
		}
		if typ := tc.global_type_for_selector(node) {
			tc.register_synth_type(id, typ)
			return
		}
		if typ := tc.const_type_for_selector(node) {
			tc.register_synth_type(id, typ)
			return
		}
		if base.value == 'C' {
			tc.register_synth_type(id, if c_upper_constant_is_pointer('C.${node.value}') {
				Type(voidptr_)
			} else {
				Type(int_)
			})
		}
		_ = module_name
		return
	}
	tc.check_storage_path_base_node(base_id)
	mut base_type := tc.smartcast_type(base_id) or { tc.resolve_type(base_id) }
	if base.kind == .ident {
		if mut_base := tc.mut_param_base_for_current_ident(base.value, base_type) {
			base_type = mut_base
		}
	}
	tc.register_synth_type(base_id, base_type)
	if smart_type := tc.smartcast_type(id) {
		tc.register_synth_type(id, smart_type)
		return
	}
	tc.record_valid_method_value(id, node, base_type)
	if typ := tc.selector_type(id, node) {
		tc.register_synth_type(id, typ)
	}
}

fn (mut tc TypeChecker) record_valid_method_value(id flat.NodeId, node flat.Node, base_type Type) {
	if !tc.expr_is_method_value(id) || tc.ident_is_call_callee_or_generic_base(id)
		|| tc.fn_context.node_id < 0 {
		return
	}
	clean_recv := unwrap_pointer(base_type)
	if clean_recv is Struct {
		if tc.struct_field_type(clean_recv.name, node.value) != none {
			return
		}
		mut mkey := '${clean_recv.name}.${node.value}'
		mut is_generic_method_value := false
		if mkey !in tc.fn_param_types {
			if ci := tc.resolve_generic_struct_method(clean_recv.name, node.value) {
				tc.generic_method_value_info[mkey] = ci
				mkey = ci.name
				is_generic_method_value = true
			}
		}
		if mkey in tc.fn_param_types || is_generic_method_value {
			tc.method_values_by_fn[tc.fn_context.node_id] << mkey
			concrete_mkey := '${clean_recv.name}.${node.value}'
			if concrete_mkey != mkey {
				tc.method_values_by_fn[tc.fn_context.node_id] << concrete_mkey
			}
		}
		return
	}
	if clean_recv is Alias {
		underlying := unalias_type(clean_recv)
		has_field := underlying is Struct
			&& tc.struct_field_type(underlying.name, node.value) != none
		if !has_field {
			if mkey := tc.alias_method_value_decl_key(clean_recv, node.value) {
				tc.method_values_by_fn[tc.fn_context.node_id] << mkey
			}
		}
		return
	}
	if clean_recv is Interface && tc.interface_field_type(clean_recv.name, node.value) == none {
		if _ := tc.interface_receiver_method_call_info(clean_recv.name, node.value) {
			tc.method_values_by_fn[tc.fn_context.node_id] << '${clean_recv.name}.${node.value}'
		}
	}
}

fn (tc &TypeChecker) map_key_type_compatible(actual Type, expected Type) bool {
	clean_actual := unalias_type(actual)
	clean_expected := unalias_type(expected)
	if clean_actual is Rune && clean_expected !is Rune {
		return false
	}
	if clean_actual is Pointer && clean_expected !is Pointer {
		return false
	}
	return tc.type_compatible(actual, expected)
}

fn (tc &TypeChecker) type_contains_pointer(typ Type, mut visited map[string]bool) bool {
	clean := unalias_type(typ)
	if clean is Pointer {
		return true
	}
	if clean is Array {
		return tc.type_contains_pointer(clean.elem_type, mut visited)
	}
	if clean is ArrayFixed {
		return tc.type_contains_pointer(clean.elem_type, mut visited)
	}
	if clean is Map {
		return tc.type_contains_pointer(clean.key_type, mut visited)
			|| tc.type_contains_pointer(clean.value_type, mut visited)
	}
	if clean is Struct {
		if visited[clean.name] {
			return false
		}
		visited[clean.name] = true
		for field in tc.struct_fields_for_init(clean.name) {
			if tc.type_contains_pointer(field.typ, mut visited) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) index_is_assignment_target(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	if parent.kind !in [.assign, .index_assign, .selector_assign] {
		return false
	}
	for i := 0; i < parent.children_count; i += 2 {
		if tc.a.child(parent, i) == id {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) index_is_handled_by_guard_or_or_block(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	if parent.kind == .or_expr && parent.children_count > 0 && tc.a.child(parent, 0) == id {
		return true
	}
	if parent.kind != .decl_assign {
		return false
	}
	grandparent_id := tc.direct_parent_id(parent_id)
	if !tc.valid_node_id(grandparent_id) {
		return false
	}
	grandparent := tc.a.node(grandparent_id)
	return grandparent.kind == .if_expr && grandparent.children_count > 0
		&& tc.a.child(grandparent, 0) == parent_id
}

fn (tc &TypeChecker) index_literal_value(id flat.NodeId) ?int {
	node := tc.a.nodes[int(id)]
	if node.kind == .int_literal {
		return v_int_literal_value(node.value)
	}
	if node.kind == .prefix && node.op == .minus && node.children_count > 0 {
		value := tc.index_literal_value(tc.a.child(&node, 0))?
		return -value
	}
	return none
}

fn (mut tc TypeChecker) check_index_overload_args(id flat.NodeId, node flat.Node, info CallInfo) {
	if info.params.len < 2 {
		return
	}
	expected := info.params[1]
	accepts_slice := is_builtin_slice_index_type(expected)
	accepts_slice_array := is_builtin_slice_index_array_type(expected)
	part_count := if node.value == 'range' { 1 } else { node.children_count - 1 }
	has_range := tc.index_overload_has_range_part(node)
	if part_count > 1 && !accepts_slice_array {
		if tc.should_diagnose(id) {
			tc.record_error(.cannot_index,
				'multi-index expressions on overloaded `[]` require a `[]SliceIndex` parameter', id)
		}
		return
	}
	if has_range && !accepts_slice && !accepts_slice_array {
		if tc.should_diagnose(id) {
			tc.record_error(.cannot_index,
				'slice expressions on overloaded `[]` require `SliceIndex` or `[]SliceIndex` parameters',
				id)
		}
		return
	}
	if node.value == 'range' {
		for i in 1 .. node.children_count {
			tc.check_index_overload_bound(tc.a.child(&node, i))
		}
	} else if !accepts_slice && !accepts_slice_array && node.children_count == 2 {
		tc.check_index_overload_part_with_expected(tc.a.child(&node, 1), expected)
	} else {
		for i in 1 .. node.children_count {
			part_id := tc.a.child(&node, i)
			tc.check_index_overload_part(part_id)
		}
	}
}

fn (mut tc TypeChecker) check_index_overload_args_ok(id flat.NodeId, node flat.Node, info CallInfo) bool {
	error_count := tc.errors.len
	tc.check_index_overload_args(id, node, info)
	return tc.errors.len == error_count
}

fn (mut tc TypeChecker) check_index_overload_part_with_expected(id flat.NodeId, expected Type) {
	if int(id) < 0 {
		return
	}
	part := tc.a.nodes[int(id)]
	if part.kind == .range {
		for i in 0 .. part.children_count {
			tc.check_index_overload_bound(tc.a.child(&part, i))
		}
		return
	}
	tc.check_node(id)
	actual := tc.resolve_expr(id, expected)
	if !tc.expr_receiver_compatible(id, actual, expected)
		&& !tc.expr_compatible(id, actual, expected) {
		tc.type_mismatch(.cannot_index,
			'cannot use `${actual.name()}` as overloaded index; expected `${expected.name()}`', id)
	}
}

fn (mut tc TypeChecker) check_index_overload_part(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	part := tc.a.nodes[int(id)]
	if part.kind == .range {
		for i in 0 .. part.children_count {
			tc.check_index_overload_bound(tc.a.child(&part, i))
		}
		return
	}
	tc.check_node(id)
	part_type := unalias_type(tc.resolve_type(id))
	if part_type !is Unknown && !part_type.is_integer() {
		tc.type_mismatch(.cannot_index, 'index must be integer, not `${part_type.name()}`', id)
	}
}

fn (mut tc TypeChecker) check_index_overload_bound(id flat.NodeId) {
	if int(id) < 0 {
		return
	}
	bound := tc.a.nodes[int(id)]
	if bound.kind == .empty {
		return
	}
	tc.check_node(id)
	bound_type := unalias_type(tc.resolve_type(id))
	if bound_type !is Unknown && !bound_type.is_integer() {
		tc.type_mismatch(.cannot_index, 'slice bound must be integer, not `${bound_type.name()}`',
			id)
	}
}

fn (tc &TypeChecker) index_overload_has_range_part(node flat.Node) bool {
	if node.value == 'range' {
		return true
	}
	for i in 1 .. node.children_count {
		part_id := tc.a.child(&node, i)
		if int(part_id) >= 0 && tc.a.nodes[int(part_id)].kind == .range {
			return true
		}
	}
	return false
}

fn is_builtin_slice_index_type(typ Type) bool {
	clean0 := unalias_type(typ)
	clean := unwrap_pointer(clean0)
	return clean.name() in ['SliceIndex', 'builtin.SliceIndex']
}

fn is_builtin_slice_index_array_type(typ Type) bool {
	clean := unalias_type(typ)
	if clean is Array {
		return is_builtin_slice_index_type(clean.elem_type)
	}
	return false
}

// check_ident validates check ident state for types.
fn (mut tc TypeChecker) check_ident(id flat.NodeId, node flat.Node) {
	if node.value.len == 0 {
		return
	}
	if tc.fn_context.closure_forbidden_captures[node.value]
		&& tc.fn_context.closure_scope != unsafe { nil } {
		if owner := tc.cur_scope.lookup_owner(node.value) {
			if !owner.belongs_to_scope_chain_until(tc.cur_scope, tc.fn_context.closure_scope) {
				if tc.fn_context.lambda_no_captures {
					tc.record_error_at(.unknown_ident, 'undefined variable `${node.value}`', id,
						tc.node_value_diagnostic_pos(id))
					tc.register_synth_type(id, Type(void_))
					return
				}
				tc.record_error_with_details_at(.unknown_ident,
					'`${node.value}` must be explicitly listed as inherited variable to be used inside a closure',
					id, tc.node_value_diagnostic_pos(id), [
					'use `fn [${node.value}] () {` instead of `fn () {`',
				])
				parent_id := tc.direct_parent_id(id)
				if tc.valid_node_id(parent_id) {
					parent := tc.a.node(parent_id)
					if parent.kind == .call && parent.children_count > 0 {
						callee := tc.a.child_node(parent, 0)
						if callee.kind == .ident
							&& callee.value in ['print', 'println', 'eprint', 'eprintln'] {
							tc.record_error_at(.call_arg_mismatch,
								'`${callee.value}` can not print void expressions', parent_id,
								parent.pos)
							tc.record_error_at(.unknown_ident, 'undefined ident: `${node.value}`',
								id, tc.node_value_diagnostic_pos(id))
						}
					}
				}
				tc.register_synth_type(id, Type(void_))
				return
			}
		}
	}
	if node.is_mut {
		mut parent_id := tc.direct_parent_id(id)
		for tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .paren {
			parent_id = tc.direct_parent_id(parent_id)
		}
		mut explicit_mut_expr_id := id
		mut selector_parent_id := parent_id
		for tc.valid_node_id(selector_parent_id)
			&& tc.a.node(selector_parent_id).kind in [.selector, .index, .paren] {
			explicit_mut_expr_id = selector_parent_id
			selector_parent_id = tc.direct_parent_id(selector_parent_id)
		}
		selector_is_guard := tc.valid_node_id(selector_parent_id)
			&& tc.a.node(selector_parent_id).kind == .is_expr
		selector_is_match := tc.valid_node_id(selector_parent_id)
			&& tc.a.node(selector_parent_id).kind == .match_stmt
		mut is_mut_decl_alias := false
		if tc.valid_node_id(selector_parent_id) {
			decl := tc.a.node(selector_parent_id)
			if decl.kind == .decl_assign {
				mut i := 0
				for i + 1 < decl.children_count {
					if tc.a.child(decl, i + 1) == explicit_mut_expr_id
						&& tc.decl_lhs_is_mut(decl, tc.a.child(decl, i)) {
						is_mut_decl_alias = true
						break
					}
					i += 2
				}
			}
		}
		mut is_option_guard := false
		if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .infix {
			if typ := tc.non_file_scope_type(node.value) {
				is_option_guard = unalias_type(typ) is OptionType
			}
		}
		if !tc.valid_node_id(parent_id)
			|| (tc.a.node(parent_id).kind !in [.call, .is_expr, .match_stmt] && !is_option_guard
			&& !selector_is_guard && !selector_is_match && !is_mut_decl_alias) {
			mut pos := node.pos
			if file := tc.a.source_files[node.pos.id] {
				source := tc.source_texts_by_file[file.name] or { '' }
				search_start := int_max(0, node.pos.offset - 4)
				if search_start < node.pos.offset && node.pos.offset <= source.len {
					if relative := source[search_start..node.pos.offset].last_index('mut') {
						start := search_start + relative
						pos = token.new_span(node.pos.id, start, start + 3)
					}
				}
			}
			message := if tc.valid_node_id(parent_id)
				&& tc.a.node(parent_id).kind in [.assign, .decl_assign] {
				'unexpected `mut` on right-hand side of assignment'
			} else {
				'the `mut` keyword is invalid here'
			}
			tc.record_error_at(.assignment_mismatch, message, id, pos)
		}
	}
	if node.value == '_' {
		tc.record_error_at(.unknown_ident,
			'undefined ident: `_` (may only be used in assignments)', id,
			tc.node_value_diagnostic_pos(id))
		tc.register_synth_type(id, Type(void_))
		return
	}
	$if ownership ? {
		tc.ownership_check_ident(id, node)
	}
	if is_bare_generic_param(node.value) {
		active_generic_ident := node.value in tc.fn_context.generic_params
			|| tc.active_generic_param(node.value)
			|| tc.node_has_enclosing_generic_param(id, node.value)
			|| tc.source_enclosing_fn_has_generic_param(id, node.value)
		if active_generic_ident {
			tc.register_synth_type(id, unknown_type('generic placeholder `${node.value}`'))
			return
		}
	}
	if typ := tc.non_file_scope_type(node.value) {
		if typ is Unknown && typ.reason == 'invalid variable' {
			tc.record_error_at(.unknown_ident, 'invalid variable `${node.value}`', id,
				tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, Type(void_))
			tc.fn_context.continue_after_unknown_ident = true
			return
		}
		tc.register_synth_type(id, typ)
		return
	}
	qname := tc.qualify_name(node.value)
	if qname != node.value {
		if typ := tc.file_scope.lookup(qname) {
			tc.register_synth_type(id, typ)
			return
		}
	}
	if node.value == 'err' {
		tc.register_synth_type(id, tc.parse_type('IError'))
		return
	}
	if typ := tc.const_types[qname] {
		tc.register_synth_type(id, typ)
		return
	}
	// A module-local const shadows an unrelated bare global collected from
	// another module (notably `time.seconds_per_minute` vs a main global).
	if typ := tc.file_scope.lookup(node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.const_types[node.value] {
		tc.register_synth_type(id, typ)
		return
	}
	if key := tc.generic_fn_value_key(node.value) {
		if !tc.ident_is_call_callee_or_generic_base(id) && !tc.expr_is_direct_call_argument(id)
			&& tc.resolved_fn_value_name(id) == none {
			message := '`${node.value}` is a generic fn, you should pass its concrete types, e.g. ${node.value}[int]'
			tc.record_error_at(.unsupported_generic, message, id, tc.node_value_diagnostic_pos(id))
			if tc.direct_parent_kind(id) == .decl_assign {
				tc.record_error_at(.unsupported_generic, message, id,
					tc.node_value_diagnostic_pos(id))
			}
			tc.register_synth_type(id, tc.fn_type_from_key(key) or { Type(void_) })
			return
		}
	}
	if typ := tc.fn_value_type(node.value) {
		if key := tc.ident_fn_value_key(node.value) {
			tc.remember_resolved_fn_value(id, key)
		}
		tc.register_synth_type(id, typ)
		return
	}
	if node.value[0].is_capital() && tc.type_name_known(node.value) {
		tc.record_error_at(.assignment_mismatch, '`${node.value}` must be initialized', id,
			tc.node_value_diagnostic_pos(id))
		tc.register_synth_type(id, Type(void_))
		return
	}
	if tc.selective_import_symbol_is_ambiguous(node.value) {
		tc.register_synth_type(id, unknown_type('ambiguous selective import `${node.value}`'))
		return
	}
	if node.value in tc.fn_ret_types || tc.qualify_fn_name(node.value) in tc.fn_ret_types {
		return
	}
	if tc.has_active_import(node.value) || qname in tc.structs || qname in tc.enum_names
		|| qname in tc.sum_types || qname in tc.interface_names {
		return
	}
	if typ := tc.enclosing_array_dsl_ident_type(id, node.value) {
		tc.register_synth_type(id, typ)
		return
	}
	if tc.should_diagnose(id) {
		if _ := tc.future_local_decl_id(node.value, id) {
			tc.record_error(.unknown_ident,
				'undefined variable `${node.value}` (used before declaration)', id)
			if tc.fn_context.undefined_variable_context_depth > 0 {
				tc.record_error(.unknown_ident, 'unresolved variable: `${node.value}`', id)
			}
			return
		}
		parent_id := tc.direct_parent_id(id)
		if tc.fn_context.node_id >= 0 && tc.valid_node_id(parent_id)
			&& tc.a.node(parent_id).kind == .expr_stmt {
			tc.record_error(.unknown_ident, '`${node.value}` evaluated but not used', id)
			tc.register_synth_type(id, Type(void_))
			return
		}
		is_match_subject := tc.ident_is_match_subject(id)
		message := if tc.fn_context.undefined_variable_context_depth > 0 && !is_match_subject {
			'undefined variable: `${node.value}`'
		} else {
			'undefined ident: `${node.value}` (unknown identifier `${node.value}`)'
		}
		tc.record_error(.unknown_ident, message, id)
		tc.register_synth_type(id, Type(void_))
	}
}

fn (tc &TypeChecker) ident_uses_forbidden_closure_capture(name string) bool {
	if !tc.fn_context.closure_forbidden_captures[name]
		|| tc.fn_context.closure_scope == unsafe { nil } {
		return false
	}
	owner := tc.cur_scope.lookup_owner(name) or { return false }
	return !owner.belongs_to_scope_chain_until(tc.cur_scope, tc.fn_context.closure_scope)
}

fn (tc &TypeChecker) ident_is_match_subject(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	return parent.kind == .match_stmt && parent.children_count > 0 && tc.a.child(parent, 0) == id
}

fn (tc &TypeChecker) generic_fn_value_key(name string) ?string {
	mut key := tc.local_bare_fn_key(name) or { '' }
	if key.len == 0 {
		key = tc.resolve_selective_import_symbol(name) or { '' }
	}
	if key.len == 0 && name in tc.fn_ret_types {
		key = name
	}
	if key.len == 0 || (tc.fn_generic_params[key] or { []string{} }).len == 0 {
		return none
	}
	return key
}

fn (tc &TypeChecker) ident_is_call_callee_or_generic_base(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	if parent.children_count == 0 || tc.a.child(parent, 0) != id {
		return false
	}
	return parent.kind in [.call, .index]
}

fn (tc &TypeChecker) future_local_decl_id(name string, use_id flat.NodeId) ?flat.NodeId {
	if name.len == 0 || !tc.valid_node_id(use_id) {
		return none
	}
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		use_pos := tc.a.node(use_id).pos
		for i, current in tc.a.nodes {
			if current.kind != .decl_assign || current.pos.id != use_pos.id
				|| current.pos.offset <= use_pos.offset {
				continue
			}
			current_id := flat.NodeId(i)
			if tc.node_has_enclosing_function(current_id) {
				continue
			}
			for child_idx := 0; child_idx < current.children_count; child_idx += 2 {
				lhs_id := tc.a.child(&current, child_idx)
				lhs := tc.a.node(lhs_id)
				if lhs.kind == .ident && lhs.value == name && lhs.pos.offset > use_pos.offset {
					return lhs_id
				}
			}
		}
		return none
	}
	use_pos := tc.a.node(use_id).pos
	mut stack := []flat.NodeId{}
	fn_node := tc.a.node(fn_id)
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(fn_node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		current_id := stack.pop()
		current := tc.a.node(current_id)
		if current.kind == .decl_assign {
			for i := 0; i < current.children_count; i += 2 {
				lhs_id := tc.a.child(current, i)
				lhs := tc.a.node(lhs_id)
				if lhs.kind == .ident && lhs.value == name && lhs.pos.id == use_pos.id
					&& lhs.pos.offset > use_pos.offset {
					return lhs_id
				}
			}
		}
		if current.kind in [.fn_literal, .lambda_expr] {
			continue
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return none
}

fn (tc &TypeChecker) node_has_enclosing_function(id flat.NodeId) bool {
	mut parent_id := tc.direct_parent_id(id)
	for tc.valid_node_id(parent_id) {
		if tc.a.node(parent_id).kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return true
		}
		parent_id = tc.direct_parent_id(parent_id)
	}
	return false
}

fn (mut tc TypeChecker) check_defer_result(id flat.NodeId, node flat.Node) {
	if node.typ == 'invalid' {
		tc.register_synth_type(id, Type(void_))
		return
	}
	idx := defer_result_index(node) or {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_type, 'invalid `res` expression', id)
		}
		tc.register_synth_type(id, unknown_type('invalid `res` expression'))
		return
	}
	ret := tc.fn_context.return_type
	if ret is Void {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_type,
				'`res` can only be used in functions that returns something', id)
		}
		tc.register_synth_type(id, Type(void_))
		return
	}
	if ret is ResultType {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_type,
				'`res` cannot be used in functions that returns a Result', id)
		}
		tc.register_synth_type(id, Type(void_))
		return
	}
	if msg := defer_result_index_error_message(ret, idx) {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_type, msg, id)
		}
		tc.register_synth_type(id, Type(void_))
		return
	}
	tc.register_synth_type(id, tc.defer_result_type(node) or {
		unknown_type('invalid `res` expression')
	})
}

fn (tc &TypeChecker) non_file_scope_type(name string) ?Type {
	owner := tc.cur_scope.lookup_owner(name) or { return none }
	if owner.belongs_to_scope(tc.file_scope) {
		return none
	}
	if owner.scope == unsafe { nil } || owner.index < 0 || owner.index >= owner.scope.types.len {
		return none
	}
	return owner.scope.types[owner.index]
}

// defer_result_index returns -1 for an unindexed `$res()` node and the non-negative
// index for a `$res(index)` node.
pub fn defer_result_index(node flat.Node) ?int {
	if node.kind != .defer_result {
		return none
	}
	if node.value.len == 0 {
		return -1
	}
	if !node.value.bytes().all(it >= `0` && it <= `9`) {
		return none
	}
	return node.value.int()
}

fn defer_result_index_error_message(ret Type, idx int) ?string {
	if ret is MultiReturn {
		if idx < 0 {
			return '`res` requires an index of the returned value'
		}
		if idx >= ret.types.len {
			return 'index ${idx} out of range of ${ret.types.len} return types'
		}
		return none
	}
	if idx >= 0 {
		return '`res` index can only be used with multi-return functions'
	}
	return none
}

fn (tc &TypeChecker) defer_result_type(node flat.Node) ?Type {
	if node.typ == 'invalid' {
		return Type(void_)
	}
	idx := defer_result_index(node) or { return none }
	ret := tc.fn_context.return_type
	if ret is Void || ret is ResultType {
		return Type(void_)
	}
	if _ := defer_result_index_error_message(ret, idx) {
		return Type(void_)
	}
	if ret is MultiReturn {
		return ret.types[idx]
	}
	return ret
}

// resolve_expr resolves resolve expr information for types.
fn (mut tc TypeChecker) resolve_expr(id flat.NodeId, expected Type) Type {
	if int(id) < 0 {
		return unknown_type('missing expression')
	}
	expected_raw := expected
	node := tc.a.nodes[int(id)]
	clean_expected := unalias_type(expected)
	if clean_expected.is_float()
		&& (tc.is_untyped_float_literal_expr(id) || node.kind == .int_literal) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .int_literal && clean_expected.is_integer() {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .field_init && node.children_count > 0 {
		return tc.resolve_expr(tc.a.child(&node, 0), expected)
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count == 1 {
		actual := tc.resolve_expr(tc.a.child(&node, 0), expected)
		if tc.type_compatible(actual, expected) {
			tc.register_synth_type(id, expected)
			return expected
		}
		return actual
	}
	if node.kind == .none_expr {
		if expected is OptionType || expected is ResultType || is_ierror_type(expected) {
			tc.register_synth_type(id, expected)
			return expected
		}
		if is_ierror_type(expected) {
			tc.register_synth_type(id, expected)
			return expected
		}
	}
	if node.kind == .or_expr && node.children_count >= 2
		&& (expected is OptionType || expected is ResultType) {
		payload := match expected {
			OptionType { expected.base_type }
			ResultType { expected.base_type }
			else { Type(void_) }
		}
		source_type := tc.resolve_type(tc.a.child(&node, 0))
		body_tail := tc.branch_tail_expr_id(tc.a.child(&node, 1))
		if tc.type_compatible(source_type, payload) && tc.valid_node_id(body_tail) {
			body_type := tc.resolve_expr(body_tail, expected)
			if tc.type_compatible(body_type, expected) || tc.type_compatible(body_type, payload) {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if payload := contextual_payload_type(expected) {
		actual := tc.resolve_expr(id, payload)
		if tc.type_compatible(actual, payload) {
			return actual
		}
	}
	if node.kind == .char_literal && node.value.starts_with('c:')
		&& unalias_type(expected).is_integer() && c_char_literal_scalar_byte(node.value[2..]) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .enum_val {
		// The expected type may be the enum directly, or an option/result wrapper around
		// it (`?Enum` / `!Enum`), e.g. `mut field ?LoggingMode` assigned `field = .debug`.
		// Unwrap the option/result payload so the shorthand resolves against the inner
		// enum. On success the node is typed as the *inner* enum (not the wrapper): a bare
		// enum value assigned into an option is auto-wrapped by the assignment/return
		// machinery, exactly like any other bare value assigned into an option, so the
		// node itself must stay unwrapped for codegen to emit the wrap.
		mut enum_expected := unalias_type(expected)
		if payload := contextual_payload_type(expected) {
			enum_expected = unalias_type(payload)
		}
		if enum_expected !is Enum {
			if enum_name := tc.resolve_enum_name(enum_expected.name()) {
				enum_expected = Type(Enum{
					name:    enum_name
					is_flag: enum_name in tc.flag_enums
				})
			}
		}
		if enum_expected is Enum {
			if tc.enum_value_matches(node.value, enum_expected.name) {
				tc.register_synth_type(id, enum_expected)
				return enum_expected
			}
			tc.type_mismatch(.assignment_mismatch,
				'unknown enum field `${node.value}` for `${enum_expected.name}`', id)
			return Type(int_)
		}
	}
	// When several anonymous structs share the same field names, or a contextual type is
	// declared after the literal, the parser may leave or infer the literal too early.
	// Its call/assignment/return context supplies the exact anonymous struct here.
	if node.kind == .struct_init && is_contextual_anonymous_struct_literal(node.value)
		&& expected !is Pointer && tc.anonymous_struct_literal_compatible(node, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	// A bare generic struct literal (`Box{...}` / `&Box{...}`) adopts a matching concrete
	// expected instance (`Box[int]` / `&Box[int]`), so `fn make() Box[int] { return
	// Box{...} }` and bare literals passed/assigned where a concrete instance is expected
	// type-check and carry the concrete type into codegen. A *value* literal only adopts a
	// *value* expectation: `bare_generic_literal_adopts` unwraps the pointer, so without
	// the `expected !is Pointer` guard `return Box{...}` would be accepted for an expected
	// `&Box[int]`, and cgen would emit a `Box_int` value where a `Box_int*` is required.
	// The pointer case is the `prefix .amp` (`&Box{...}`) path below.
	if node.kind == .struct_init && expected !is Pointer
		&& tc.bare_generic_literal_adopts(node.value, expected)
		&& tc.generic_literal_fields_compatible(node, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 && expected is Pointer {
		child_id := tc.a.child(&node, 0)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .struct_init && is_contextual_anonymous_struct_literal(child.value)
			&& tc.anonymous_struct_literal_compatible(child, expected.base_type) {
			tc.register_synth_type(child_id, expected.base_type)
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		if child.kind == .struct_init && tc.bare_generic_literal_adopts(child.value, expected)
			&& tc.generic_literal_fields_compatible(child, expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .postfix && node.children_count == 1 {
		child_id := tc.a.child(&node, 0)
		child := tc.a.nodes[int(child_id)]
		if node.op == .not && child.kind == .array_literal {
			array_expected := if expected is OptionType {
				expected.base_type
			} else if expected is ResultType {
				expected.base_type
			} else {
				expected
			}
			actual := tc.resolve_expr(child_id, array_expected)
			if tc.receiver_compatible(actual, array_expected) {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			mut fixed_expected := array_expected
			if fixed_expected is Pointer {
				fixed_expected = fixed_expected.base_type
			}
			if fixed_expected is ArrayFixed {
				fixed_actual := tc.resolve_expr(child_id, fixed_expected)
				if tc.receiver_compatible(fixed_actual, fixed_expected) {
					tc.register_synth_type(id, expected_raw)
					return expected_raw
				}
				if fixed_actual is ArrayFixed && fixed_actual.len <= fixed_expected.len
					&& tc.receiver_compatible(fixed_actual.elem_type, fixed_expected.elem_type) {
					tc.register_synth_type(id, expected_raw)
					return expected_raw
				}
			}
			elem_type := if child.children_count > 0 {
				tc.array_literal_elem_type(child)
			} else {
				Type(int_)
			}
			array_actual := Type(ArrayFixed{
				elem_type: elem_type
				len:       child.children_count
			})
			tc.register_synth_type(id, array_actual)
			return array_actual
		}
	}
	if node.kind == .array_literal {
		mut elem_expected := Type(void_)
		mut expected_is_array := false
		if expected is Array {
			elem_expected = array_elem_type(expected)
			expected_is_array = true
		} else if expected is ArrayFixed {
			elem_expected = fixed_array_elem_type(expected)
			expected_is_array = true
		}
		if expected_is_array {
			// Empty literal `[]` simply adopts the expected array type.
			if node.children_count == 0 {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			// Non-empty literal: propagate the expected element type down to each
			// element so context-dependent elements (enum shorthand `[.foo]`, `none`,
			// fn literals) type against it instead of defaulting to a fixed `int[N]`.
			// Adopt the expected array type when every element fits.
			mut all_ok := true
			for i in 0 .. node.children_count {
				child_id := tc.a.child(&node, i)
				elem_actual := tc.resolve_expr(child_id, elem_expected)
				if !tc.expr_receiver_compatible(child_id, elem_actual, elem_expected) {
					all_ok = false
					break
				}
			}
			// A fixed-array expectation additionally requires the literal to have
			// exactly the expected number of elements; otherwise `[1, 2]` would be
			// accepted as e.g. `[4]int` and the C backend would copy/read past the
			// compound literal. Element-type propagation above still happens either
			// way; only the type adoption is gated. Unresolvable const lengths stay
			// lenient (we cannot verify them here).
			if all_ok && expected is ArrayFixed {
				if expected_len := tc.fixed_array_len_value(expected) {
					if node.children_count != expected_len {
						all_ok = false
					}
				}
			}
			if all_ok {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if node.kind == .map_init {
		if expected_map := map_type_from_receiver(expected) {
			if node.children_count == 0 {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			mut all_ok := true
			mut i := 0
			for i < node.children_count {
				child_id := tc.a.child(&node, i)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
					update_actual := map_type_from_receiver(tc.resolve_type(tc.a.child(&child, 0))) or {
						all_ok = false
						break
					}
					if !tc.receiver_compatible(update_actual.key_type, expected_map.key_type)
						|| !tc.receiver_compatible(update_actual.value_type, expected_map.value_type) {
						all_ok = false
						break
					}
					i += 2
					continue
				}
				if i + 1 >= node.children_count {
					all_ok = false
					break
				}
				key_actual := tc.resolve_expr(tc.a.child(&node, i), expected_map.key_type)
				value_actual := tc.resolve_expr(tc.a.child(&node, i + 1), expected_map.value_type)
				if !tc.expr_receiver_compatible(tc.a.child(&node, i), key_actual, expected_map.key_type)
					|| !tc.expr_receiver_compatible(tc.a.child(&node, i + 1), value_actual, expected_map.value_type) {
					all_ok = false
					break
				}
				i += 2
			}
			if all_ok {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
		}
	}
	if node.kind == .match_stmt || node.kind == .if_expr {
		// Match/if used as a value expression: propagate the expected type into
		// each branch tail so enum-shorthand / `none` / fn-literal tails type
		// against it (e.g. `return match s { 'a' { .foo } ... }` with an enum
		// return type), then adopt the expected type when every branch fits.
		if expected !is Void && expected !is Unknown && tc.branches_compatible_with(id, expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if _ := fn_type_from_type(expected) {
		if _ := tc.resolve_fn_value_name_for_expected(id, expected_raw) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .fn_literal || node.kind == .lambda_expr {
		if _ := fn_type_from_type(expected) {
			actual_fn := tc.resolve_type(id)
			if tc.fn_value_signature_compatible(actual_fn, expected) {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			return actual_fn
		}
	}
	if tc.expr_is_unsafe_nil(id) {
		if _ := fn_type_from_type(expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		if expected is Pointer {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		if expected is OptionType && unalias_type(expected.base_type) is Pointer {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .call {
		if call_info := tc.resolve_call_info(id, node) {
			if call_info.return_type !is Unknown && call_info.return_type !is Void {
				tc.register_synth_type(id, call_info.return_type)
				return call_info.return_type
			}
		}
	}
	actual := tc.resolve_type(id)
	if tc.branch_tail_is_error_literal(id) && expected is ResultType && is_ierror_type(actual) {
		return actual
	}
	if tc.type_compatible(actual, expected) {
		actual_w0, actual_w1, _ := type_value_words(&actual)
		expected_w0, expected_w1, _ := type_value_words(&expected)
		if (tc.raw_type_equality && actual_w0 == expected_w0 && actual_w1 == expected_w1)
			|| tc.type_name(actual) == tc.type_name(expected) {
			tc.register_synth_type(id, expected)
			return expected
		}
		if expected is OptionType || expected is ResultType {
			if actual is OptionType || actual is ResultType {
				tc.register_synth_type(id, expected_raw)
				return expected_raw
			}
			return actual
		}
		if expected is SumType || expected is Enum {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if tc.expr_generic_expected_match(id, actual, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	return actual
}

fn c_char_literal_scalar_byte(value string) bool {
	if value.len == 1 {
		return true
	}
	if value.len < 2 || value[0] != `\\` {
		return false
	}
	if value.len == 2 {
		return true
	}
	if value[1] == `x` {
		if value.len <= 2 {
			return false
		}
		parsed := v_int_literal_value('0x${value[2..]}') or { return false }
		return parsed <= 0xff
	}
	if value[1] < `0` || value[1] > `7` || value.len > 4 {
		return false
	}
	mut parsed := 0
	for digit in value[1..].bytes() {
		if digit < `0` || digit > `7` {
			return false
		}
		parsed = parsed * 8 + int(digit - `0`)
	}
	return parsed <= 0xff
}

// fn_value_match_key returns the single exact function declaration key accepted for a function value.
fn (tc &TypeChecker) fn_value_match_key(node flat.Node, expected Type) ?string {
	key := tc.fn_value_key(node) or { return none }
	actual := tc.fn_type_from_key(key) or { return none }
	if tc.fn_value_signature_compatible(actual, expected) {
		return key
	}
	return none
}

fn (tc &TypeChecker) fn_value_signature_compatible(actual Type, expected Type) bool {
	actual_fn := fn_type_from_type(actual) or { return false }
	expected_fn := fn_type_from_type(expected) or { return false }
	if actual_fn.params.len != expected_fn.params.len {
		return false
	}
	for i in 0 .. actual_fn.params.len {
		if !fn_param_modes_compatible(actual_fn, expected_fn, i) {
			return false
		}
		actual_param := fn_compatible_param_type(actual_fn, i)
		expected_param := fn_compatible_param_type(expected_fn, i)
		if !tc.fn_param_compatible(actual_param, expected_param) {
			return false
		}
	}
	return tc.fn_return_compatible(actual_fn.return_type, expected_fn.return_type)
}

fn c_upper_constant_is_pointer(qname string) bool {
	return qname == 'C.NULL' || qname == 'C.SIG_DFL' || qname == 'C.SIG_ERR' || qname == 'C.SIG_IGN'
}

fn c_int_selector_name(name string) bool {
	return name in ['errno', 'EINTR', 'STDOUT_FILENO', 'STDERR_FILENO', 'EINVAL', 'SOMAXCONN']
}

// fn_value_key resolves a function value expression to one exact function declaration key.
fn (tc &TypeChecker) fn_value_key(node flat.Node) ?string {
	if node.kind == .ident {
		return tc.ident_fn_value_key(node.value)
	}
	if node.kind == .selector {
		return tc.selector_fn_value_key(node)
	}
	if node.kind in [.cast_expr, .paren, .expr_stmt] && node.children_count > 0 {
		return tc.fn_value_key(tc.a.child_node(&node, 0))
	}
	return none
}

fn (tc &TypeChecker) ident_fn_value_key(name string) ?string {
	if local_name := tc.local_bare_fn_signature_key(name) {
		return local_name
	}
	if imported_name := tc.resolve_selective_import_symbol(name) {
		return imported_name
	}
	if tc.fn_signature_known(name) {
		return name
	}
	return none
}

fn (tc &TypeChecker) selector_fn_value_key(node flat.Node) ?string {
	if node.children_count == 0 || !valid_string_data(node.value) {
		return none
	}
	base := tc.a.child_node(&node, 0)
	if base.kind == .ident {
		if base.value == 'C' {
			key := 'C.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			return none
		}
		if tc.ident_resolves_to_value(base.value) {
			return none
		}
		if resolved_mod := tc.resolve_import_alias(base.value) {
			key := '${resolved_mod}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			return none
		}
		if key := tc.static_assoc_fn_key_for_base(base.value, node.value) {
			return key
		}
		return none
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			key := '${mod_name}.${base.value}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			if static_key := tc.static_assoc_fn_key_for_base('${mod_name}.${base.value}',
				node.value)
			{
				return static_key
			}
		}
	}
	return none
}

fn (tc &TypeChecker) static_assoc_fn_key_for_base(type_ident string, method string) ?string {
	if method.len == 0 {
		return none
	}
	for type_name in [type_ident, tc.qualify_name(type_ident)] {
		key := '${type_name}.${method}'
		if tc.fn_signature_known(key) && tc.fn_key_is_static_associated(key) {
			return key
		}
	}
	for type_name in tc.static_assoc_type_candidates(type_ident) {
		key := '${type_name}.${method}'
		if (tc.fn_signature_known(key) || key in tc.fn_ret_types)
			&& tc.fn_key_is_static_associated(key) {
			return key
		}
	}
	return none
}

fn (tc &TypeChecker) fn_key_is_static_associated(key string) bool {
	return tc.static_associated_fn_keys[key]
}

fn (tc &TypeChecker) static_assoc_type_candidates(type_ident string) []string {
	if type_ident.len == 0 {
		return []string{}
	}
	mut candidates := []string{}
	tc.add_static_assoc_type_candidate(mut candidates, type_ident)
	if resolved := tc.resolve_selective_import_type_symbol(type_ident) {
		tc.add_static_assoc_type_candidate(mut candidates, resolved)
	}
	if resolved := tc.resolve_import_alias(type_ident) {
		tc.add_static_assoc_type_candidate(mut candidates, resolved)
	}
	tc.add_static_assoc_type_candidate(mut candidates, tc.qualify_name(type_ident))
	mut result := []string{}
	for candidate in candidates {
		if tc.static_assoc_type_known(candidate) && candidate !in result {
			result << candidate
		}
	}
	return result
}

fn (tc &TypeChecker) add_static_assoc_type_candidate(mut candidates []string, name string) {
	clean := trimmed_space(name)
	if clean.len == 0 {
		return
	}
	if clean !in candidates {
		candidates << clean
	}
	if target := tc.type_aliases[clean] {
		if target !in candidates {
			candidates << target
		}
	}
	qname := tc.qualify_name(clean)
	if qname != clean && qname !in candidates {
		candidates << qname
	}
	if target := tc.type_aliases[qname] {
		if target !in candidates {
			candidates << target
		}
	}
}

fn (tc &TypeChecker) static_assoc_type_known(type_name string) bool {
	return type_name in tc.structs || type_name in tc.enum_names || type_name in tc.sum_types
		|| type_name in tc.interface_names || type_name in tc.type_aliases
}

fn (tc &TypeChecker) fn_signature_known(key string) bool {
	return key in tc.fn_ret_types && key in tc.fn_param_types
}

fn (tc &TypeChecker) expr_is_unsafe_nil(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		return node.value == 'unsafe' && tc.expr_tail_is_nil(id)
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.expr_is_unsafe_nil(tc.a.child(&node, 0))
	}
	return false
}

fn (tc &TypeChecker) expr_tail_is_nil(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.nil_literal {
			return true
		}
		.expr_stmt, .paren {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_nil(tc.a.child(&node, 0))
		}
		.block {
			if node.children_count == 0 {
				return false
			}
			return tc.expr_tail_is_nil(tc.a.child(&node, node.children_count - 1))
		}
		else {
			return false
		}
	}
}

// fn_value_type supports fn value type handling for TypeChecker.
fn (tc &TypeChecker) fn_value_type(name string) ?Type {
	if local_name := tc.local_bare_fn_key(name) {
		return tc.fn_type_from_key(local_name)
	}
	if imported_name := tc.resolve_selective_import_symbol(name) {
		return tc.fn_type_from_key(imported_name)
	}
	if name in tc.fn_ret_types {
		return tc.fn_type_from_key(name)
	}
	return none
}

// fn_type_from_key converts fn type from key data for types.
fn (tc &TypeChecker) fn_type_from_key(key string) ?Type {
	params := tc.fn_param_types[key] or { return none }
	ret := tc.fn_ret_types[key] or { return none }
	return Type(FnType{
		params:      params.clone()
		params_mut:  (tc.declaration_param_mutability[key] or { []bool{} }).clone()
		return_type: ret
	})
}

fn (tc &TypeChecker) translated_c_string_fixed_array_compatible(id flat.NodeId, expected Type) bool {
	if !tc.translated_files[tc.cur_file] && !tc.node_is_in_translated_file(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .char_literal || !node.value.starts_with('c:') {
		return false
	}
	clean_expected := unalias_type(expected)
	if clean_expected is ArrayFixed {
		if !fixed_array_has_c_char_elements(clean_expected) {
			return false
		}
		payload_len := c_string_literal_payload_len(node.value[2..]) or { return false }
		// C permits an exact-size character array initializer without the implicit NUL.
		return payload_len <= clean_expected.len
	}
	mut pointer_type := clean_expected
	if pointer_type is OptionType {
		pointer_type = unalias_type(pointer_type.base_type)
	}
	if pointer_type is Pointer {
		pointee := unalias_type(pointer_type.base_type)
		return pointee is ArrayFixed && fixed_array_has_c_char_elements(pointee)
	}
	return false
}

fn fixed_array_has_c_char_elements(array ArrayFixed) bool {
	elem := unalias_type(array.elem_type)
	return elem is Char || (elem is Primitive && elem.size == 8 && elem.props.has(.integer))
}

fn c_string_literal_payload_len(value string) ?int {
	mut payload_len := 0
	mut i := 0
	for i < value.len {
		if value[i] != `\\` {
			payload_len++
			i++
			continue
		}
		if i + 1 >= value.len {
			return none
		}
		escape := value[i + 1]
		match escape {
			`'`, `"`, `?`, `\\`, `a`, `b`, `e`, `f`, `n`, `r`, `t`, `v` {
				i += 2
				payload_len++
			}
			`0`...`7` {
				i += 2
				mut digits := 1
				for digits < 3 && i < value.len && value[i] >= `0` && value[i] <= `7` {
					i++
					digits++
				}
				payload_len++
			}
			`x` {
				i += 2
				start := i
				for i < value.len && ((value[i] >= `0` && value[i] <= `9`)
					|| (value[i] >= `a` && value[i] <= `f`)
					|| (value[i] >= `A` && value[i] <= `F`)) {
					i++
				}
				if i == start {
					return none
				}
				payload_len++
			}
			else {
				return none
			}
		}
	}
	return payload_len
}

// struct_field_c_abi_fn_ptr_type returns the C ABI function-pointer type for a struct field.
pub fn (tc &TypeChecker) struct_field_c_abi_fn_ptr_type(struct_name string, field_name string) ?string {
	key := struct_field_c_abi_key(struct_name, field_name)
	if typ := tc.struct_field_c_abi_fns[key] {
		return typ
	}
	return none
}

// enum_value_matches supports enum value matches handling for TypeChecker.
fn (tc &TypeChecker) enum_value_matches(value string, enum_name string) bool {
	if value.starts_with('.') {
		return tc.enum_has_field(enum_name, value[1..])
	}
	if value.contains('.') {
		prefix := value.all_before_last('.')
		field := value.all_after_last('.')
		if prefix != enum_name && short_type_name(prefix) != short_type_name(enum_name) {
			return false
		}
		return tc.enum_has_field(enum_name, field)
	}
	return tc.enum_has_field(enum_name, value)
}

// enum_has_field converts enum has field data for types.
fn (tc &TypeChecker) enum_has_field(enum_name string, field string) bool {
	fields := tc.enum_fields[enum_name] or { return false }
	return field in fields
}

// resolve_enum_name resolves resolve enum name information for types.
fn (tc &TypeChecker) resolve_enum_name(name string) ?string {
	if name in tc.enum_names {
		return name
	}
	mut shortened := name
	for shortened.contains('.') {
		tail := shortened.all_after('.')
		if !tail.contains('.') {
			break
		}
		shortened = tail
		if shortened in tc.enum_names {
			return shortened
		}
	}
	qname := tc.qualify_name(name)
	if qname in tc.enum_names {
		return qname
	}
	if !name.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(name) {
			if resolved in tc.enum_names || resolved in tc.flag_enums {
				return resolved
			}
			if target := tc.resolve_enum_alias_target(resolved) {
				return target
			}
		}
	}
	if target := tc.resolve_enum_alias_target(name) {
		return target
	}
	if target := tc.resolve_enum_alias_target(qname) {
		return target
	}
	return none
}

fn (tc &TypeChecker) resolve_enum_alias_target(name string) ?string {
	mut cur := name
	for _ in 0 .. 16 {
		target := tc.alias_target_type_text(cur) or { return none }
		if target == cur {
			return none
		}
		if target in tc.enum_names || target in tc.flag_enums {
			return target
		}
		cur = target
	}
	return none
}

// enum_selector_type supports enum selector type handling for TypeChecker.
fn (tc &TypeChecker) enum_selector_type(node &flat.Node) ?Type {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base := tc.a.child_node(node, 0)
	mut enum_name := ''
	if base.kind == .ident {
		if base.value.len == 0 || !base.value[0].is_capital() {
			return none
		}
		enum_name = tc.resolve_enum_name(base.value) or { '' }
	} else if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident && base.value.len > 0 && base.value[0].is_capital() {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			enum_name = tc.resolve_enum_name('${mod_name}.${base.value}') or { '' }
		}
	}
	if enum_name.len == 0 || !tc.enum_has_field(enum_name, node.value) {
		return none
	}
	return Type(Enum{
		name:    enum_name
		is_flag: enum_name in tc.flag_enums
	})
}

// type_compatible returns type compatible data for TypeChecker.
fn (tc &TypeChecker) type_compatible(actual Type, expected Type) bool {
	actual_raw := actual
	expected_raw := expected
	// Canonical types dominate this path. Identical Type value bytes mean the
	// same tag and payload, so avoid both spelling-cache lookups entirely.
	if tc.raw_type_equality {
		actual_w0, actual_w1, _ := type_value_words(&actual)
		expected_w0, expected_w1, _ := type_value_words(&expected)
		if actual_w0 == expected_w0 && actual_w1 == expected_w1 {
			return true
		}
	}
	// One memoized spelling per side: the former per-comparison name() calls
	// composed fresh strings up to eight times per invocation (1.3M+ calls per
	// self-host build), and equal compound types now compare via the shared
	// memo instance's pointer fast path.
	actual_name := tc.type_name(actual)
	expected_name := tc.type_name(expected)
	if actual_name == expected_name {
		return true
	}
	if thread_handle_type_names_match(actual_name, expected_name) {
		return true
	}
	if fn_param_is_voidptr_type(actual) && fn_param_is_voidptr_type(expected) {
		return true
	}
	if actual is ResultType && expected is OptionType {
		return false
	}
	if actual is Unknown || expected is Unknown {
		return true
	}
	if type_contains_unknown(actual) || type_contains_unknown(expected) {
		return true
	}
	if actual is FnType && expected is FnType {
		return tc.fn_value_signature_compatible(Type(actual), Type(expected))
	}
	if (actual_name.contains('[') || expected_name.contains('['))
		&& tc.generic_type_name_matches(actual_name, expected_name) {
		return true
	}
	if tc.sum_variant_type_for_pattern(expected_name, actual_name) != none {
		return true
	}
	if actual is Alias && expected is Alias {
		return tc.type_compatible(actual.base_type, expected.base_type)
	}
	if actual is Alias {
		if expected is Interface && tc.type_implements_interface(actual, expected) {
			return true
		}
		return tc.type_compatible(actual.base_type, expected)
	}
	if expected is Alias {
		if tc.alias_type_is_shared(expected) && actual is Pointer {
			expected_shared := if expected.base_type is Pointer {
				expected.base_type.base_type
			} else {
				expected.base_type
			}
			if tc.type_compatible(actual.base_type, expected_shared) {
				return true
			}
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected is String && is_ierror_type(actual) {
		return true
	}
	if actual is Array && is_runtime_array_type(expected) {
		return true
	}
	if actual is Array && expected is Array && actual.elem_type is Void {
		return true
	}
	if is_runtime_array_type(actual) && expected is Array {
		return true
	}
	if actual is None {
		return expected is OptionType || expected is ResultType || is_ierror_type(expected)
	}
	if is_option_void_type(actual) && is_ierror_type(expected) {
		return true
	}
	if expected is OptionType {
		if actual is OptionType {
			actual_base := option_alias_payload_type(actual.base_type)
			expected_base := option_alias_payload_type(expected.base_type)
			return tc.type_compatible(actual_base, expected_base)
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected is ResultType {
		if actual is ResultType {
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
		return tc.type_compatible(actual, expected.base_type)
	}
	if expected_name in tc.interface_names {
		return tc.type_implements_interface(actual, Interface{
			name: expected_name
		})
	}
	if expected is SumType {
		return tc.type_matches_sum(actual_raw, expected_raw)
	}
	if expected is Interface {
		return tc.type_implements_interface(actual, expected)
	}
	if expected is Enum && actual is Primitive && actual.props.has(.integer) {
		return true
	}
	if actual is Interface {
		if expected is Interface {
			return tc.interface_implements_interface(actual.name, expected.name)
		}
		return false
	}
	if actual.is_integer() && expected.is_integer() {
		return true
	}
	if expected is Primitive {
		if actual is Primitive {
			if expected.props.has(.boolean) || actual.props.has(.boolean) {
				return expected.props.has(.boolean) && actual.props.has(.boolean)
			}
			if expected.props.has(.float) && actual.props.has(.integer) {
				return true
			}
			if expected.props.has(.integer) && actual.props.has(.integer) {
				return true
			}
			if expected.props.has(.float) && actual.props.has(.float) {
				return true
			}
		}
		if expected.props.has(.integer) && actual.is_integer() {
			return true
		}
	}
	if actual is Primitive && actual.props.has(.integer) && expected.is_integer() {
		return true
	}
	if expected is String {
		return actual is String || is_ierror_type(actual)
	}
	if expected is Char {
		return actual is Char || actual.name() == 'u8'
	}
	if expected is Pointer {
		if actual is Nil {
			return true
		}
		if expected.base_type is Void && actual is FnType {
			return true
		}
		if actual is Pointer {
			if expected.base_type is Interface
				&& tc.type_implements_interface(actual.base_type, expected.base_type) {
				return true
			}
			expected_base_name := expected.base_type.name()
			if expected_base_name in tc.interface_names && tc.type_implements_interface(actual.base_type, Interface{
				name: expected_base_name
			}) {
				return true
			}
			if expected.base_type is Void || actual.base_type is Void {
				return true
			}
			// C interop: `&char` and `&u8` share representation (`tos_clone(C.strdup(s))`).
			if (actual.base_type is Char && expected.base_type.name() == 'u8')
				|| (actual.base_type.name() == 'u8' && expected.base_type is Char) {
				return true
			}
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
	}
	if expected is Array {
		if actual is Array {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
		if actual is ArrayFixed {
			return tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is ArrayFixed {
		if actual is ArrayFixed {
			return tc.fixed_array_lengths_compatible(actual, expected)
				&& tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Channel {
		if actual is Channel {
			return actual.is_mut == expected.is_mut
				&& tc.type_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Map {
		if actual is Map {
			return tc.type_compatible(actual.key_type, expected.key_type)
				&& tc.type_compatible(actual.value_type, expected.value_type)
		}
	}
	if expected is FnType {
		if actual is FnType {
			if actual.params.len != expected.params.len {
				return false
			}
			for i in 0 .. actual.params.len {
				if !fn_param_modes_compatible(actual, expected, i) {
					return false
				}
				actual_param := fn_compatible_param_type(actual, i)
				expected_param := fn_compatible_param_type(expected, i)
				if !tc.fn_param_compatible(actual_param, expected_param) {
					return false
				}
			}
			return tc.fn_return_compatible(actual.return_type, expected.return_type)
		}
	}
	return false
}

fn option_alias_payload_type(typ Type) Type {
	if typ is Alias {
		clean := unalias_type(typ)
		if clean is OptionType {
			return clean.base_type
		}
	}
	return typ
}

fn (tc &TypeChecker) alias_type_is_shared(alias Alias) bool {
	mut name := alias.name
	for _ in 0 .. 16 {
		mut target := tc.type_aliases[name] or { '' }
		if target.len == 0 && !name.contains('.') {
			target = tc.type_aliases[tc.qualify_name(name)] or { '' }
		}
		target = trimmed_space(target)
		if target.starts_with('shared ') {
			return true
		}
		if target.len == 0 || target == name || target.contains('[') || target.contains(']')
			|| target.contains('?') || target.contains('&') || target.contains('!')
			|| target.contains(' ') {
			return false
		}
		name = target
	}
	return false
}

fn thread_handle_type_names_match(actual string, expected string) bool {
	if !actual.starts_with('thread') || !expected.starts_with('thread') {
		return false
	}
	return canonical_thread_handle_type_name(actual) == canonical_thread_handle_type_name(expected)
}

fn canonical_thread_handle_type_name(name string) string {
	clean := trimmed_space(name)
	if clean == 'thread void' {
		return 'thread'
	}
	if clean == 'thread !' {
		return 'thread !void'
	}
	if clean == 'thread ?' {
		return 'thread ?void'
	}
	return clean
}

fn type_contains_unknown(typ Type) bool {
	if typ is Unknown {
		return true
	}
	if typ is Alias {
		return type_contains_unknown(typ.base_type)
	}
	if typ is Array {
		return type_contains_unknown(typ.elem_type)
	}
	if typ is ArrayFixed {
		return type_contains_unknown(typ.elem_type)
	}
	if typ is Map {
		return type_contains_unknown(typ.key_type) || type_contains_unknown(typ.value_type)
	}
	if typ is Pointer {
		return type_contains_unknown(typ.base_type)
	}
	if typ is OptionType {
		return type_contains_unknown(typ.base_type)
	}
	if typ is ResultType {
		return type_contains_unknown(typ.base_type)
	}
	if typ is FnType {
		for param in typ.params {
			if type_contains_unknown(param) {
				return true
			}
		}
		return type_contains_unknown(typ.return_type)
	}
	if typ is MultiReturn {
		for part in typ.types {
			if type_contains_unknown(part) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) fn_param_compatible(actual Type, expected Type) bool {
	if actual is Unknown || expected is Unknown {
		return false
	}
	if tc.raw_type_equality {
		actual_w0, actual_w1, _ := type_value_words(&actual)
		expected_w0, expected_w1, _ := type_value_words(&expected)
		if actual_w0 == expected_w0 && actual_w1 == expected_w1 {
			return true
		}
	}
	// V's platform `int` and a fixed-width integer such as `i64`/`i32` may share an
	// emitted C spelling on a given target, but they remain distinct, target-
	// independent source types. Function-parameter identity must not depend on the C
	// width, so never let the c_type shortcut below collapse `int` with `i64`/`i32`
	// (on 64-bit both spell `i64`; on 32-bit `int`/`i32` both spell `i32`).
	if fn_param_is_platform_int(actual) != fn_param_is_platform_int(expected)
		&& fn_param_unalias_type(actual) is Primitive
		&& fn_param_unalias_type(expected) is Primitive {
		return false
	}
	if tc.c_type(actual) == tc.c_type(expected) {
		return true
	}
	return fn_param_can_cast_userdata_param(actual, expected)
}

fn fn_param_is_platform_int(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	if clean is Primitive {
		return clean.size == 0 && clean.props.has(.integer) && !clean.props.has(.unsigned)
	}
	return false
}

fn (tc &TypeChecker) fn_return_compatible(actual Type, expected Type) bool {
	if tc.raw_type_equality {
		actual_w0, actual_w1, _ := type_value_words(&actual)
		expected_w0, expected_w1, _ := type_value_words(&expected)
		if actual_w0 == expected_w0 && actual_w1 == expected_w1 {
			return true
		}
	}
	if tc.type_name(actual) == tc.type_name(expected) {
		return true
	}
	if fn_param_is_voidptr_type(expected) && fn_param_is_nonvoid_pointer_type(actual) {
		return true
	}
	if tc.c_type(actual) == tc.c_type(expected) && tc.type_compatible(actual, expected) {
		return true
	}
	return fn_return_canonical_type_name(actual) == fn_return_canonical_type_name(expected)
}

fn fn_param_can_cast_userdata_param(actual Type, expected Type) bool {
	return (fn_param_is_voidptr_type(expected) && fn_param_is_nonvoid_pointer_type(actual))
		|| (fn_param_is_nonvoid_pointer_type(expected) && fn_param_is_voidptr_type(actual))
}

fn fn_param_is_voidptr_type(typ Type) bool {
	if typ.name() in ['voidptr', '&void'] {
		return true
	}
	clean := fn_param_unalias_type(typ)
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		return base is Void
	}
	return false
}

fn fn_param_is_nonvoid_pointer_type(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	if clean is Pointer {
		base := fn_param_unalias_type(clean.base_type)
		return base !is Void
	}
	return false
}

fn fn_param_unalias_type(typ Type) Type {
	if typ is Alias {
		return fn_param_unalias_type(typ.base_type)
	}
	return typ
}

fn call_arg_integer_type(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	return clean.is_integer()
		|| clean.name() in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32', 'u64', 'usize', 'rune']
}

fn call_arg_numeric_type(typ Type) bool {
	clean := fn_param_unalias_type(typ)
	return call_arg_integer_type(clean) || clean.is_float()
}

fn call_arg_implicit_signed_widening(actual Type, expected Type) bool {
	actual_name := fn_param_unalias_type(actual).name()
	expected_name := fn_param_unalias_type(expected).name()
	return (actual_name in ['int', 'i32'] && expected_name in ['i64', 'isize'])
		|| (actual_name == 'f32' && expected_name == 'f64')
}

fn escaped_identifier_name(name string) string {
	return if name.starts_with('@') { name[1..] } else { name }
}

fn fn_return_canonical_type_name(typ Type) string {
	if typ is Alias {
		return fn_return_canonical_type_name(typ.base_type)
	}
	return typ.name()
}

// is_ierror_type reports whether is ierror type applies in types.
fn is_builtin_ierror_name(name string) bool {
	return name == 'IError' || name == 'builtin.IError'
}

fn is_ierror_type(t Type) bool {
	if t is Alias {
		return is_builtin_ierror_name(t.name) || is_ierror_type(t.base_type)
	}
	if t is Pointer {
		return is_ierror_type(t.base_type)
	}
	if t is Struct {
		return is_builtin_ierror_name(t.name)
	}
	if t is Interface {
		return is_builtin_ierror_name(t.name)
	}
	return false
}

fn (tc &TypeChecker) type_embeds_error(t Type) bool {
	clean := unwrap_pointer(t)
	if clean is Alias {
		return tc.type_embeds_error(clean.base_type)
	}
	if clean is Struct {
		struct_name := clean.name
		if struct_name == 'Error' || struct_name.ends_with('.Error') {
			return true
		}
		return tc.receiver_embeds(clean, Type(Struct{
			name: 'Error'
		}))
	}
	return false
}

// is_runtime_array_type reports whether is runtime array type applies in types.
fn is_runtime_array_type(t Type) bool {
	if t is Alias {
		return is_runtime_array_type(t.base_type)
	}
	if t is Struct {
		return t.name == 'array'
	}
	return false
}

// fixed_array_lengths_compatible supports fixed array lengths compatible handling for TypeChecker.
fn (tc &TypeChecker) fixed_array_lengths_compatible(actual ArrayFixed, expected ArrayFixed) bool {
	if actual.len > 0 && expected.len > 0 {
		return actual.len == expected.len
	}
	actual_len := tc.fixed_array_len_value(actual) or {
		return actual.len_expr == expected.len_expr
	}
	expected_len := tc.fixed_array_len_value(expected) or {
		return actual.len_expr == expected.len_expr
	}
	return actual_len == expected_len
}

// fixed_array_len_value returns the evaluated fixed-array length when it can be resolved.
pub fn (tc &TypeChecker) fixed_array_len_value(arr ArrayFixed) ?int {
	if arr.len > 0 {
		return arr.len
	}
	if arr.len_expr.len == 0 {
		return none
	}
	return tc.const_int_value(arr.len_expr, []string{})
}

// const_expr_paren_wraps_whole reports whether `s` is a single parenthesised group that
// encloses the entire string (`(a + b)`), as opposed to one that only covers part of it
// (`(a) + (b)`), so a const length wrapped in redundant parentheses can be unwrapped.
fn const_expr_paren_wraps_whole(s string) bool {
	if s.len < 2 || s[0] != `(` || s[s.len - 1] != `)` {
		return false
	}
	mut depth := 0
	for i := 0; i < s.len; i++ {
		if s[i] == `(` {
			depth++
		} else if s[i] == `)` {
			depth--
			if depth == 0 {
				return i == s.len - 1
			}
		}
	}
	return false
}

@[ignore_overflow]
fn const_int_power(base int, exponent int) int {
	mut exp := exponent
	mut power := base
	mut value := 1
	if exp < 0 {
		if base == 0 {
			return -1
		}
		if base != 1 && base != -1 {
			return 0
		}
		return if exp & 1 != 0 { base } else { 1 }
	}
	for exp > 0 {
		if exp & 1 != 0 {
			value *= power
		}
		power *= power
		exp >>= 1
	}
	return value
}

// const_int_value supports const int value handling for TypeChecker.
pub fn (tc &TypeChecker) const_int_value(name string, seen []string) ?int {
	return tc.const_int_value_in_module(name, tc.cur_module, seen)
}

// const_int_value_in_module supports const int value handling for a specific module.
pub fn (tc &TypeChecker) const_int_value_in_module(name string, module_name string, seen []string) ?int {
	if name in seen {
		return none
	}
	mut candidates := []string{}
	candidates << name
	if module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
		&& !name.contains('.') {
		candidates << '${module_name}.${name}'
	}
	qname := tc.qualify_name(name)
	if qname != name {
		candidates << qname
	}
	if key := tc.const_key_for_suffix(name) {
		candidates << key
	}
	for key in candidates {
		if key in seen {
			continue
		}
		if expr_id := tc.const_exprs[key] {
			mut next_seen := seen.clone()
			next_seen << key
			const_module := tc.const_modules[key] or { module_name }
			return tc.const_int_expr(expr_id, const_module, next_seen)
		}
	}
	if v := v_int_literal_value(name) {
		return v
	}
	if v := tc.const_int_cast_text_value(name, module_name, seen) {
		return v
	}
	if v := tc.const_int_enum_selector_value(name) {
		return v
	}
	// Simple const arithmetic in string form, e.g. a fixed-array size `[SEGS + 1]`,
	// `[SEGS+1]`, `[segs / 2]`, `[segs % 4]` or `[2 * (segs + 1)]`. A length wrapped
	// wholly in parentheses is the inner expression, so strip the outer pair and
	// re-evaluate. Otherwise split on the rightmost operator of the lowest precedence
	// level present (`+ -`, then `* / %`) that sits OUTSIDE any parentheses, so a nested
	// operator (the `+` inside `2 * (segs + 1)`) is not chosen; precedence and left
	// associativity hold and each side is trimmed and resolved recursively. A leading `-`
	// (unary) leaves an empty lhs and is skipped.
	expr := trimmed_space(name)
	if const_expr_paren_wraps_whole(expr) {
		return tc.const_int_value(trimmed_space(expr[1..expr.len - 1]), seen)
	}
	// Operators are grouped by the same precedence levels as token.left_binding_power:
	// `+ - | ^` share sum, while `* / % << >> >>> &` share product. Scanning the
	// rightmost operator at a level preserves left associativity. Power scans its first
	// operator instead because it is right-associative.
	// Longer operators match first (`>>>` before `>>`, two-char before one) and `idx + op.len`
	// skips the operator.
	for level in [['+', '-', '|', '^'], ['*', '/', '%', '&', '<<', '>>', '>>>'],
		['**']] {
		mut idx := -1
		mut op := ''
		mut depth := 0
		mut i := 0
		for i < expr.len {
			ch := expr[i..i + 1]
			if ch == '(' {
				depth++
				i++
				continue
			}
			if ch == ')' {
				depth--
				i++
				continue
			}
			if depth == 0 {
				three := if i + 3 <= expr.len { expr[i..i + 3] } else { '' }
				if three.len == 3 && three in level {
					idx = i
					op = three
					i += 3
					continue
				}
				two := if i + 2 <= expr.len { expr[i..i + 2] } else { '' }
				if two == '**' {
					if two in level && idx < 0 {
						idx = i
						op = two
					}
					i += 2
					continue
				}
				if two.len == 2 && two in level {
					idx = i
					op = two
					i += 2
					continue
				}
				if ch in level {
					idx = i
					op = ch
				}
			}
			i++
		}
		if idx <= 0 {
			continue
		}
		lhs := trimmed_space(expr[..idx])
		rhs := trimmed_space(expr[idx + op.len..])
		if lhs.len == 0 || rhs.len == 0 {
			continue
		}
		// Prefix operators bind below power in the parser. Strip leading signs
		// from the power base and apply them to the complete power expression:
		// `-2 ** 2` is `-(2 ** 2)`, while `(-2) ** 2` keeps the signed base.
		mut lhs_text := lhs
		mut power_sign := 1
		if op == '**' {
			for lhs_text.len > 0 && lhs_text[0] in [`+`, `-`] {
				if lhs_text[0] == `-` {
					power_sign = -power_sign
				}
				lhs_text = trimmed_space(lhs_text[1..])
			}
			if lhs_text.len == 0 {
				return none
			}
		}
		l := tc.const_int_value_in_module(lhs_text, module_name, seen) or { return none }
		r := tc.const_int_value_in_module(rhs, module_name, seen) or { return none }
		if (op == '/' || op == '%') && r == 0 {
			return none
		}
		if (op == '<<' || op == '>>' || op == '>>>') && (r < 0 || r >= 64) {
			return none
		}
		value := match op {
			'+' { l + r }
			'-' { l - r }
			'*' { l * r }
			'/' { l / r }
			'%' { l % r }
			'|' { l | r }
			'^' { l ^ r }
			'&' { l & r }
			'<<' { int(u64(l) << r) }
			'>>' { l >> r }
			'**' { const_int_power(l, r) }
			else { int(u64(l) >> r) }
		}
		return if op == '**' && power_sign < 0 { -value } else { value }
	}
	if expr.len > 1 && expr[0] in [`+`, `-`] {
		value := tc.const_int_value_in_module(trimmed_space(expr[1..]), module_name, seen) or {
			return none
		}
		return if expr[0] == `-` { -value } else { value }
	}
	return none
}

fn (tc &TypeChecker) const_int_cast_text_value(text string, module_name string, seen []string) ?int {
	expr := trimmed_space(text)
	if !expr.ends_with(')') {
		return none
	}
	open := expr.index_u8(`(`)
	if open <= 0 {
		return none
	}
	cast_type_name := trimmed_space(expr[..open])
	if cast_type_name.len == 0 {
		return none
	}
	cast_type := unalias_type(tc.parse_type(cast_type_name))
	if !cast_type.is_integer() {
		return none
	}
	inner := trimmed_space(expr[open + 1..expr.len - 1])
	if inner.len == 0 {
		return none
	}
	return tc.const_int_value_in_module(inner, module_name, seen)
}

fn (tc &TypeChecker) const_int_enum_selector_value(text string) ?int {
	expr := trimmed_space(text)
	dot := expr.last_index_u8(`.`)
	if dot <= 0 || dot >= expr.len - 1 {
		return none
	}
	enum_name := tc.resolve_enum_name(trimmed_space(expr[..dot])) or { return none }
	field := trimmed_space(expr[dot + 1..])
	for item in tc.comptime_static_enum_decl_value_cases(enum_name) {
		if item.name == field && item.has_value {
			return item.value
		}
	}
	fields := tc.enum_fields[enum_name] or { return none }
	for idx, name in fields {
		if name == field {
			return if enum_name in tc.flag_enums { 1 << idx } else { idx }
		}
	}
	return none
}

// const_int_expr supports const int expr handling for TypeChecker.
fn (tc &TypeChecker) const_int_expr(id flat.NodeId, module_name string, seen []string) ?int {
	if int(id) < 0 {
		return none
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.int_literal {
			if v := v_int_literal_value(node.value) {
				return v
			}
		}
		.char_literal {
			return match_char_literal_value(node.value)
		}
		.ident {
			return tc.const_int_value_in_module(node.value, module_name, seen)
		}
		.paren {
			if node.children_count > 0 {
				return tc.const_int_expr(tc.a.child(&node, 0), module_name, seen)
			}
		}
		.cast_expr {
			if node.children_count == 0 {
				return none
			}
			cast_type := unalias_type(tc.parse_type(node.value))
			if !cast_type.is_integer() {
				return none
			}
			return tc.const_int_expr(tc.a.child(&node, 0), module_name, seen)
		}
		.enum_val {
			return tc.const_int_enum_selector_value(node.value)
		}
		.selector {
			return tc.const_int_enum_selector_value(tc.source_text_for_node(id))
		}
		.sizeof_expr {
			return tc.const_sizeof_type_value(node.value)
		}
		.prefix {
			if node.children_count == 0 {
				return none
			}
			value := tc.const_int_expr(tc.a.child(&node, 0), module_name, seen) or { return none }
			return match node.op {
				.minus { -value }
				.plus { value }
				.bit_not { ~value }
				else { none }
			}
		}
		.infix {
			if node.children_count < 2 {
				return none
			}
			left := tc.const_int_expr(tc.a.child(&node, 0), module_name, seen) or { return none }
			right := tc.const_int_expr(tc.a.child(&node, 1), module_name, seen) or { return none }
			match node.op {
				.plus {
					return left + right
				}
				.minus {
					return left - right
				}
				.mul {
					return left * right
				}
				.power {
					return const_int_power(left, right)
				}
				.div {
					if right == 0 {
						return none
					}
					return left / right
				}
				.mod {
					if right == 0 {
						return none
					}
					return left % right
				}
				.amp {
					return left & right
				}
				.pipe {
					return left | right
				}
				.xor {
					return left ^ right
				}
				.left_shift {
					if right < 0 || right >= 64 {
						return none
					}
					return int(u64(left) << right)
				}
				.right_shift {
					if right < 0 || right >= 64 {
						return none
					}
					return left >> right
				}
				.right_shift_unsigned {
					if right < 0 || right >= 64 {
						return none
					}
					return int(u64(left) >> right)
				}
				else {
					return none
				}
			}
		}
		else {}
	}

	return none
}

fn (tc &TypeChecker) const_sizeof_type_value(type_name string) ?int {
	typ := unalias_type(tc.parse_type(type_name))
	if typ is Primitive {
		if typ.size > 0 {
			return int(typ.size) / 8
		}
	}
	if typ is Char {
		return 1
	}
	if typ is Rune {
		return 4
	}
	if typ is ISize || typ is USize {
		return 8
	}
	return none
}

// type_implements_interface returns type implements interface data for TypeChecker.
fn (tc &TypeChecker) type_implements_interface(actual Type, expected Interface) bool {
	clean := unwrap_pointer(actual)
	if clean is Unknown {
		return true
	}
	if tc.interface_has_no_requirements(expected.name) {
		return true
	}
	if clean is Interface {
		return tc.interface_implements_interface(clean.name, expected.name)
	}
	if clean is Array {
		if tc.interface_abstract_method_names(expected.name).len > 0 {
			return false
		}
		for field in tc.interface_field_list(expected.name) {
			actual_field := tc.builtin_array_field_type(field.name) or { return false }
			if !tc.type_compatible(actual_field, field.typ)
				|| !tc.type_compatible(field.typ, actual_field) {
				return false
			}
		}
		return true
	}
	concrete_name := method_type_name(clean)
	if concrete_name.len == 0 {
		return false
	}
	return tc.named_type_implements_interface(concrete_name, expected.name)
}

fn (tc &TypeChecker) builtin_array_field_type(name string) ?Type {
	return match name {
		'element_size', 'offset', 'len', 'cap' { Type(int_) }
		'data' { Type(voidptr_) }
		'flags' { tc.parse_type('ArrayFlags') }
		else { none }
	}
}

// interface_implements_interface supports interface implements interface handling for TypeChecker.
pub fn (tc &TypeChecker) interface_implements_interface(actual_name string, expected_name string) bool {
	actual := tc.interface_metadata_name(actual_name)
	expected := tc.interface_metadata_name(expected_name)
	if actual == expected {
		return true
	}
	for method in tc.interface_method_names(expected) {
		actual_key := tc.interface_method_signature_key(actual, method) or { '${actual}.${method}' }
		expected_key := tc.interface_method_signature_key(expected, method) or {
			'${expected}.${method}'
		}
		if actual_key !in tc.fn_param_types
			|| !tc.method_signature_compatible(actual_key, expected_key) {
			return false
		}
	}
	for field in tc.interface_field_list(expected) {
		actual_field := tc.interface_field_type(actual, field.name) or { return false }
		if !tc.type_compatible(actual_field, field.typ)
			|| !tc.type_compatible(field.typ, actual_field) {
			return false
		}
	}
	return true
}

pub fn (tc &TypeChecker) interface_metadata_name(name string) string {
	if name.len == 0 {
		return name
	}
	base, _, is_generic := generic_type_application_parts(name)
	lookup := if is_generic { base } else { name }
	if lookup in tc.interface_names || lookup in tc.interface_abstract_methods
		|| lookup in tc.interface_embeds || lookup in tc.interface_fields {
		return lookup
	}
	if !lookup.contains('.') {
		qname := tc.qualify_name(lookup)
		if qname in tc.interface_names || qname in tc.interface_abstract_methods
			|| qname in tc.interface_embeds || qname in tc.interface_fields {
			return qname
		}
	}
	short := lookup.all_after_last('.')
	mut match_name := ''
	for candidate, _ in tc.interface_names {
		if candidate.all_after_last('.') != short {
			continue
		}
		if match_name.len > 0 && match_name != candidate {
			return lookup
		}
		match_name = candidate
	}
	if match_name.len > 0 {
		return match_name
	}
	return lookup
}

// named_type_implements_interface
// supports helper handling in types.
pub fn (tc &TypeChecker) named_type_implements_interface(concrete_name string, iface_name string) bool {
	if tc.interface_has_no_requirements(iface_name) {
		return true
	}
	// Only the abstract (declared) methods must be provided by the concrete type.
	// Methods defined directly on the interface (default implementations) are
	// inherited and need not be reimplemented.
	for method in tc.interface_abstract_method_names(iface_name) {
		expected_key := tc.interface_method_signature_key(iface_name, method) or {
			'${iface_name}.${method}'
		}
		if concrete_name == 'char' && method == 'str'
			&& tc.interface_method_is_str_requirement(expected_key) {
			return false
		}
		if info := tc.resolve_generic_struct_method(concrete_name, method) {
			if !tc.method_call_info_signature_compatible_for_interface(info, expected_key,
				iface_name) {
				return false
			}
			continue
		}
		if concrete_key := tc.concrete_method_signature_key(concrete_name, method) {
			if !tc.method_signature_compatible_for_interface(concrete_key, expected_key, iface_name) {
				return false
			}
			continue
		}
		if info := tc.resolve_generic_sum_method(concrete_name, method) {
			if !tc.method_call_info_signature_compatible_for_interface(info, expected_key,
				iface_name) {
				return false
			}
			continue
		}
		if method == 'str' && tc.interface_method_is_str_requirement(expected_key)
			&& tc.type_has_implicit_str_method(concrete_name) {
			continue
		}
		return false
	}
	for field in tc.interface_field_list(iface_name) {
		actual_field := tc.interface_actual_field(concrete_name, field.name) or { return false }
		if !tc.type_compatible(actual_field.typ, field.typ)
			|| !tc.type_compatible(field.typ, actual_field.typ)
			|| (field.is_mut && !actual_field.is_mut) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) interface_method_is_str_requirement(expected_key string) bool {
	ret := tc.fn_ret_types[expected_key] or { return false }
	if ret.name() != 'string' {
		return false
	}
	params := tc.fn_param_types[expected_key] or { return false }
	return params.len == 1
}

pub fn (tc &TypeChecker) type_has_implicit_str_method(name string) bool {
	clean := trimmed_space(name)
	if clean.len == 0 {
		return false
	}
	mut candidates := [clean]
	if !clean.contains('.') {
		qname := tc.qualify_name(clean)
		if qname != clean {
			candidates << qname
		}
	}
	for candidate in candidates {
		if tc.type_name_resolves_to_sum_type(candidate) {
			continue
		}
		base_name := generic_base_name(candidate)
		if candidate in tc.structs || base_name in tc.structs || candidate in tc.enum_names
			|| candidate in tc.flag_enums || candidate in tc.interface_names {
			return true
		}
		if is_builtin_type_name(candidate) {
			mut seen := map[string]bool{}
			if tc.type_supports_implicit_str(tc.parse_type(candidate), mut seen) {
				return true
			}
		}
		if candidate in tc.type_aliases {
			mut seen := map[string]bool{}
			seen[candidate] = true
			if tc.type_supports_implicit_str(tc.parse_type(tc.type_aliases[candidate]), mut seen) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) type_supports_implicit_str(typ Type, mut seen map[string]bool) bool {
	if typ is Alias {
		if seen[typ.name] {
			return false
		}
		seen[typ.name] = true
		return tc.type_supports_implicit_str(typ.base_type, mut seen)
	}
	if typ is Primitive {
		return typ.props.has(.boolean) || typ.props.has(.integer) || typ.props.has(.float)
	}
	return typ is String || typ is Rune || typ is ISize || typ is USize || typ is Pointer
		|| typ is Array || typ is ArrayFixed || typ is Map || typ is Enum || typ is Struct
}

fn (tc &TypeChecker) type_name_resolves_to_sum_type(name string) bool {
	mut cur := trimmed_space(name)
	mut seen := map[string]bool{}
	for cur.len > 0 {
		if cur in tc.sum_types {
			return true
		}
		if !cur.contains('.') {
			qname := tc.qualify_name(cur)
			if qname in tc.sum_types {
				return true
			}
		}
		if seen[cur] {
			return false
		}
		seen[cur] = true
		mut next := tc.type_aliases[cur] or { '' }
		if next.len == 0 && !cur.contains('.') {
			next = tc.type_aliases[tc.qualify_name(cur)] or { '' }
		}
		cur = trimmed_space(next)
	}
	return false
}

fn (tc &TypeChecker) interface_has_no_requirements(iface_name string) bool {
	return tc.interface_abstract_method_names(iface_name).len == 0
		&& tc.interface_field_list(iface_name).len == 0
}

fn struct_decl_implements_from_typ(typ string) []string {
	mut out := []string{}
	for part in struct_decl_typ_parts(typ) {
		if !part.starts_with('implements=') {
			continue
		}
		for iface in part['implements='.len..].split('|') {
			clean := trimmed_space(iface)
			if clean.len > 0 {
				out << clean
			}
		}
	}
	return out
}

fn struct_decl_typ_parts(typ string) []string {
	mut parts := []string{}
	mut start := 0
	mut depth := 0
	for i, ch in typ {
		if ch == `[` {
			depth++
		} else if ch == `]` {
			if depth > 0 {
				depth--
			}
		} else if ch == `,` && depth == 0 {
			parts << typ[start..i]
			start = i + 1
		}
	}
	parts << typ[start..]
	return parts
}

fn marker_type_name(name string) string {
	mut clean := trimmed_space(name)
	base, _, ok := generic_type_application_parts(clean)
	if ok {
		clean = base
	}
	return clean
}

fn interface_marker_matches(name string, target string) bool {
	clean := marker_type_name(name)
	target_clean := marker_type_name(target)
	return clean == target_clean || clean.all_after_last('.') == target_clean
		|| clean == target_clean.all_after_last('.')
}

pub fn (tc &TypeChecker) named_type_implements_marker(concrete_name string, target string) bool {
	mut name := trimmed_space(concrete_name)
	if name.starts_with('&') {
		name = trimmed_space(name[1..])
	}
	name = marker_type_name(name)
	mut candidates := [name]
	if !name.contains('.') {
		qname := tc.qualify_name(name)
		if qname != name {
			candidates << qname
		}
	} else if name.starts_with('main.') {
		candidates << name['main.'.len..]
	}
	for candidate in candidates {
		impls := tc.struct_implements[candidate] or { continue }
		for impl_name in impls {
			if interface_marker_matches(impl_name, target) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) type_has_compiler_default_clone(t Type) bool {
	if t is Struct {
		return tc.named_type_implements_marker(t.name, 'IClone')
	}
	return false
}

// interface_impl_names returns the concrete type names (structs and type
// aliases) that implement `iface_name`. Once a snapshot is frozen, its names
// stay first so transform-emitted interface IDs are preserved; later
// implementers are appended in deterministic discovery order.
pub fn (tc &TypeChecker) interface_impl_names(iface_name string) []string {
	mut cache := tc.type_cache
	if !isnil(cache) {
		if cached := cache.interface_impl_entries[iface_name] {
			return cached
		}
	}
	mut impls := []string{}
	if snapshot := tc.interface_impl_name_snapshots[iface_name] {
		impls = snapshot.clone()
		mut seen := map[string]bool{}
		for name in impls {
			seen[name] = true
		}
		// Generic monomorphization can add concrete implementers after the
		// transform has already emitted `_typ` literals. Preserve the earlier
		// ids, and append any later implementers so cgen can still box them.
		for name in tc.interface_impl_names_uncached_after(iface_name,
			tc.interface_impl_candidates_at_snapshot) {
			if name !in seen {
				seen[name] = true
				impls << name
			}
		}
	} else {
		impls = tc.interface_impl_names_uncached(iface_name)
	}
	if !isnil(cache) {
		// reserve also initializes zero-value maps in lightweight cache overlays.
		cache.interface_impl_entries.reserve(1)
		cache.interface_impl_entries[iface_name] = impls
	}
	return impls
}

// pre_transform_interface_impl_names returns the immutable implementer snapshot
// prepared after semantic checking and before generic monomorphization.
pub fn (tc &TypeChecker) pre_transform_interface_impl_names(iface_name string) ?[]string {
	index := tc.interface_impl_indexes[iface_name] or { return none }
	return index.names
}

// interface_type_ids returns the `_typ` dispatch IDs for an interface, preserving
// any snapshot IDs emitted before late generic implementers were discovered.
pub fn (tc &TypeChecker) interface_type_ids(iface_name string) map[string]int {
	if snapshot := tc.interface_impl_name_snapshots[iface_name] {
		return stable_interface_type_ids_preserving_prefix(snapshot,
			tc.interface_impl_names(iface_name))
	}
	return stable_interface_type_ids(tc.interface_impl_names(iface_name))
}

// freeze_interface_impl_names snapshots the interface implementation order used
// by transform-generated `_typ` checks before later metadata cleanup can remove
// unused generic declarations and shift cgen's ids.
pub fn (mut tc TypeChecker) freeze_interface_impl_names() {
	mut snapshots := map[string][]string{}
	for iface_name in tc.interface_names.keys() {
		snapshots[iface_name] = tc.interface_impl_names_uncached(iface_name)
	}
	tc.interface_impl_name_snapshots = snapshots.move()
	tc.interface_impl_candidates_at_snapshot = tc.interface_impl_candidate_names()
	// Earlier transform queries may have cached an implementer list before all
	// lowered method signatures were available. Cgen must rebuild from the frozen
	// snapshot so its dispatch table uses the same IDs as transformed interface boxes.
	tc.clear_interface_impl_cache()
}

// freeze_pre_transform_interface_impl_names freezes the immutable implementer
// indexes prepared before transform. Transform does not add declarations; later
// generic implementers remain discoverable because the matching candidate set is
// frozen with the indexes.
pub fn (mut tc TypeChecker) freeze_pre_transform_interface_impl_names() {
	mut snapshots := map[string][]string{}
	for iface_name in tc.interface_names.keys() {
		if index := tc.interface_impl_indexes[iface_name] {
			snapshots[iface_name] = index.names.clone()
		} else {
			snapshots[iface_name] = tc.interface_impl_names_uncached(iface_name)
		}
	}
	tc.interface_impl_name_snapshots = snapshots.move()
	tc.interface_impl_candidates_at_snapshot = tc.interface_impl_candidates_at_index.clone()
	tc.clear_interface_impl_cache()
}

fn (tc &TypeChecker) interface_impl_names_uncached(iface_name string) []string {
	return tc.interface_impl_names_uncached_after(iface_name, map[string]bool{})
}

fn add_interface_impl_candidate(mut candidates map[string]bool, name string, excluded map[string]bool) {
	if name !in excluded {
		candidates[name] = true
	}
}

fn (tc &TypeChecker) interface_impl_candidate_names() map[string]bool {
	mut candidates := map[string]bool{}
	for name in implicit_str_builtin_type_names() {
		candidates[name] = true
	}
	for name, _ in tc.structs {
		candidates[interface_impl_candidate_name(name)] = true
	}
	for name, _ in tc.enum_names {
		candidates[interface_impl_candidate_name(name)] = true
	}
	for name, target in tc.type_aliases {
		candidates[interface_impl_candidate_name(name)] = true
		typ := tc.parse_type(target)
		if typ is FnType {
			candidates[Type(typ).name()] = true
		}
	}
	for name, _ in tc.interface_names {
		candidates[name] = true
		short_name := interface_impl_candidate_name(name)
		if short_name != name {
			candidates[short_name] = true
		}
	}
	return candidates
}

fn (tc &TypeChecker) interface_impl_names_uncached_after(iface_name string, excluded map[string]bool) []string {
	mut candidate_set := map[string]bool{}
	has_no_requirements := tc.interface_has_no_requirements(iface_name)
	accepts_implicit_str := tc.interface_accepts_implicit_str(iface_name)
	if has_no_requirements || accepts_implicit_str {
		for name in implicit_str_builtin_type_names() {
			add_interface_impl_candidate(mut candidate_set, name, excluded)
		}
	}
	for name, _ in tc.structs {
		add_interface_impl_candidate(mut candidate_set, interface_impl_candidate_name(name),
			excluded)
	}
	if has_no_requirements || accepts_implicit_str {
		for name, _ in tc.enum_names {
			add_interface_impl_candidate(mut candidate_set, interface_impl_candidate_name(name),
				excluded)
		}
	}
	for name, target in tc.type_aliases {
		candidate_name := interface_impl_candidate_name(name)
		add_interface_impl_candidate(mut candidate_set, candidate_name, excluded)
		if has_no_requirements && candidate_name !in excluded {
			typ := tc.parse_type(target)
			if typ is FnType {
				add_interface_impl_candidate(mut candidate_set, Type(typ).name(), excluded)
			}
		}
	}
	for name, _ in tc.interface_names {
		if name != iface_name && tc.interface_implements_interface(name, iface_name) {
			add_interface_impl_candidate(mut candidate_set, name, excluded)
		}
		short_name := interface_impl_candidate_name(name)
		if short_name != name && short_name != iface_name
			&& tc.interface_implements_interface(name, iface_name) {
			add_interface_impl_candidate(mut candidate_set, short_name, excluded)
		}
	}
	mut candidates := candidate_set.keys()
	candidates.sort()
	mut impls := []string{}
	for name in candidates {
		if name in tc.interface_names {
			if tc.interface_implements_interface(name, iface_name) {
				impls << name
			}
			continue
		}
		resolved_name := tc.interface_metadata_name(name)
		if resolved_name in tc.interface_names {
			if tc.interface_implements_interface(resolved_name, iface_name) {
				impls << name
			}
			continue
		}
		if tc.named_type_implements_interface(name, iface_name) {
			impls << name
		}
	}
	return impls
}

// stable_interface_type_ids assigns deterministic nonzero `_typ` dispatch IDs to
// interface implementers in caller-supplied order. Hash collisions are resolved
// with linear probing after earlier names keep their IDs, so late generic
// implementers appended after transform cannot shift IDs already emitted for
// existing interface values/checks.
pub fn stable_interface_type_ids(impl_names []string) map[string]int {
	mut ids := map[string]int{}
	mut used := map[int]bool{}
	for name in impl_names {
		if name in ids {
			continue
		}
		mut id := stable_interface_type_id_hash(name)
		for used[id] {
			if id == 0x7fffffff {
				id = 1
			} else {
				id++
			}
		}
		used[id] = true
		ids[name] = id
	}
	return ids
}

// stable_interface_type_ids_preserving_prefix assigns IDs for `impl_names` while
// keeping all prefix IDs exactly as they would be when assigned alone.
pub fn stable_interface_type_ids_preserving_prefix(prefix []string, impl_names []string) map[string]int {
	mut ids := stable_interface_type_ids(prefix)
	mut used := map[int]bool{}
	mut seen := map[string]bool{}
	for name, id in ids {
		used[id] = true
		seen[name] = true
	}
	mut late_names := []string{}
	for name in impl_names {
		if name in seen {
			continue
		}
		seen[name] = true
		late_names << name
	}
	late_names.sort()
	assign_stable_interface_type_ids(mut ids, mut used, late_names)
	return ids
}

fn assign_stable_interface_type_ids(mut ids map[string]int, mut used map[int]bool, names []string) {
	for name in names {
		if name in ids {
			continue
		}
		mut id := stable_interface_type_id_hash(name)
		for used[id] {
			if id == 0x7fffffff {
				id = 1
			} else {
				id++
			}
		}
		used[id] = true
		ids[name] = id
	}
}

// interface_impl_set_signature returns the complete deterministic interface implementer set
// that controls collision-resolved dispatch IDs for the current program.
pub fn (tc &TypeChecker) interface_impl_set_signature() string {
	mut iface_names := tc.interface_names.keys()
	iface_names.sort()
	mut lines := []string{cap: iface_names.len}
	for iface_name in iface_names {
		impl_names := if iface_name in ['IError', 'builtin.IError'] {
			tc.ierror_impl_names()
		} else {
			tc.interface_impl_names(iface_name)
		}
		lines << '${iface_name}=${impl_names.join(',')}'
	}
	return lines.join('\n')
}

// interface_concrete_method_keys returns generated interface dispatch methods and
// concrete method declarations that can be called by those dispatch functions.
pub fn (tc &TypeChecker) interface_concrete_method_keys() []string {
	mut iface_names := tc.interface_names.keys()
	iface_names.sort()
	mut keys := []string{}
	mut seen := map[string]bool{}
	for iface_name in iface_names {
		impl_names := if iface_name in ['IError', 'builtin.IError'] {
			tc.ierror_impl_names()
		} else {
			tc.interface_impl_names(iface_name)
		}
		for method in tc.interface_abstract_method_names(iface_name) {
			dispatch_key := '${iface_name}.${method}'
			if dispatch_key !in seen {
				seen[dispatch_key] = true
				keys << dispatch_key
			}
			for concrete_name in impl_names {
				key := tc.concrete_method_signature_key(concrete_name, method) or { continue }
				if key !in seen {
					seen[key] = true
					keys << key
				}
			}
		}
	}
	keys.sort()
	return keys
}

fn stable_interface_type_id_hash(name string) int {
	mut hash := u32(2166136261)
	for c in name.bytes() {
		hash = (hash ^ u32(c)) * u32(16777619)
	}
	id := int(hash & u32(0x7fffffff))
	return if id == 0 { 1 } else { id }
}

// stable_type_index returns the deterministic nonzero runtime-index seed for a named type.
// Use stable_type_indexes when assigning indexes across a complete program type set.
pub fn stable_type_index(name string) int {
	mut type_idx := stable_interface_type_id_hash(name) & ~(0xff << 16)
	if type_idx < 65536 {
		// Bit 24 is the first available bit above the reserved indirection byte.
		type_idx |= 1 << 24
	}
	return type_idx
}

// stable_type_indexes assigns deterministic, collision-free runtime indexes to
// the complete caller-supplied program type set.
pub fn stable_type_indexes(type_names []string) map[string]int {
	mut indexes := map[string]int{}
	mut used := map[int]bool{}
	mut names := type_names.clone()
	names.sort()
	for raw_name in names {
		name := raw_name.trim_space()
		if name.len == 0 || name in indexes {
			continue
		}
		mut type_idx := stable_type_index(name)
		for used[type_idx] {
			type_idx = next_stable_type_index(type_idx)
		}
		indexes[name] = type_idx
		used[type_idx] = true
	}
	return indexes
}

// extend_stable_type_indexes assigns deterministic, collision-free runtime indexes
// to new names without changing indexes that have already been used during lowering.
pub fn extend_stable_type_indexes(mut indexes map[string]int, type_names []string) {
	extend_stable_type_indexes_ref(mut indexes, &type_names)
}

// extend_stable_type_indexes_ref is the pointer-ABI form used by native compiler stages.
pub fn extend_stable_type_indexes_ref(mut indexes map[string]int, type_names &[]string) {
	mut used := map[int]bool{}
	for _, type_idx in indexes {
		used[type_idx] = true
	}
	mut names := type_names.clone()
	names.sort()
	for raw_name in names {
		name := trimmed_space(raw_name)
		if name.len == 0 || name in indexes {
			continue
		}
		mut type_idx := stable_type_index(name)
		for used[type_idx] {
			type_idx = next_stable_type_index(type_idx)
		}
		indexes[name] = type_idx
		used[type_idx] = true
	}
}

fn next_stable_type_index(type_idx int) int {
	if type_idx & 0xffff < 0xffff {
		return type_idx + 1
	}
	high_bits := type_idx & 0x7f000000
	return if high_bits < 0x7f000000 { high_bits + (1 << 24) } else { 1 << 24 }
}

// runtime_type_index_names returns the canonical program type names that can
// participate in runtime interface/sum type_idx() lowering.
pub fn (tc &TypeChecker) runtime_type_index_names() []string {
	mut names := map[string]bool{}
	for name, _ in tc.structs {
		names[name] = true
	}
	for name, variants in tc.sum_types {
		names[name] = true
		for variant in variants {
			names[variant] = true
		}
	}
	for name, _ in tc.enum_names {
		names[name] = true
	}
	for name, _ in tc.type_aliases {
		names[name] = true
	}
	for name, _ in tc.interface_names {
		names[name] = true
	}
	for _, index in tc.interface_impl_indexes {
		for name in index.names {
			names[name] = true
		}
	}
	return names.keys()
}

pub fn (tc &TypeChecker) interface_accepts_implicit_str(iface_name string) bool {
	for method in tc.interface_abstract_method_names(iface_name) {
		if method != 'str' {
			continue
		}
		expected_key := tc.interface_method_signature_key(iface_name, method) or {
			'${iface_name}.${method}'
		}
		if tc.interface_method_is_str_requirement(expected_key) {
			return true
		}
	}
	return false
}

fn implicit_str_builtin_type_names() []string {
	return ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'byte', 'u16', 'u32',
		'u64', 'f32', 'f64', 'string', 'rune']
}

fn interface_impl_candidate_name(name string) string {
	if name.starts_with('builtin.') {
		return name['builtin.'.len..]
	}
	return name
}

// ierror_impl_names returns the concrete struct names that can be boxed as `IError`.
pub fn (tc &TypeChecker) ierror_impl_names() []string {
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		if cache.ierror_impl_names_set {
			return cache.ierror_impl_names.clone()
		}
		mut fallback := cache.base
		for !isnil(fallback) {
			if fallback.ierror_impl_names_set {
				return fallback.ierror_impl_names.clone()
			}
			fallback = fallback.base
		}
	}
	mut struct_names := []string{}
	for name, _ in tc.structs {
		struct_names << name
	}
	struct_names.sort()
	mut impls := []string{}
	for name in struct_names {
		if tc.named_type_compatible_with_ierror(name) {
			impls << name
		}
	}
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		cache.ierror_impl_names = impls.clone()
		cache.ierror_impl_names_set = true
	}
	return impls
}

pub fn (tc &TypeChecker) concrete_method_signature_key(concrete_name string, method string) ?string {
	mut seen_aliases := map[string]bool{}
	return tc.concrete_method_signature_key_seen(concrete_name, method, mut seen_aliases)
}

fn (tc &TypeChecker) concrete_method_signature_key_seen(concrete_name string, method string, mut seen_aliases map[string]bool) ?string {
	if seen_aliases[concrete_name] {
		return none
	}
	seen_aliases[concrete_name] = true
	key := '${concrete_name}.${method}'
	if key in tc.fn_param_types || key in tc.fn_ret_types {
		return key
	}
	qualified_name := tc.qualify_name(concrete_name)
	if concrete_name !in tc.type_aliases && qualified_name != concrete_name {
		qualified_key := '${qualified_name}.${method}'
		if qualified_key in tc.fn_param_types || qualified_key in tc.fn_ret_types {
			return qualified_key
		}
	}
	// The current module can make parse_type(`Connection`) select the imported
	// `orm.Connection` interface even when the implementation candidate is a
	// same-named alias from another module. Consult the alias table by its
	// collected concrete name before doing suffix-based method lookup.
	for alias_name in [concrete_name, qualified_name] {
		if alias_target := tc.type_aliases[alias_name] {
			if alias_target != alias_name && alias_target != concrete_name {
				if inherited := tc.concrete_method_signature_key_seen(alias_target, method, mut
					seen_aliases)
				{
					return inherited
				}
			}
		}
	}
	receiver_type := tc.parse_type(concrete_name)
	receiver_candidates := receiver_method_name_candidates(receiver_type, method, tc.cur_module)
	// Prefer a method declared directly on the alias when one exists, but do not
	// let an unqualified alias name resolve through the suffix index to an
	// unrelated interface with the same name. Aliases otherwise inherit the
	// methods of their base type.
	for candidate in receiver_candidates {
		if candidate in tc.fn_param_types || candidate in tc.fn_ret_types {
			return candidate
		}
	}
	if receiver_type is Alias {
		for candidate in receiver_method_name_candidates(receiver_type.base_type, method,
			tc.cur_module) {
			if candidate in tc.fn_param_types || candidate in tc.fn_ret_types {
				return candidate
			}
			if indexed := tc.receiver_method_suffix_index[candidate] {
				if indexed != receiver_method_suffix_ambiguous {
					return indexed
				}
			}
		}
	}
	for candidate in receiver_candidates {
		if indexed := tc.receiver_method_suffix_index[candidate] {
			if indexed != receiver_method_suffix_ambiguous {
				return indexed
			}
		}
	}
	for candidate in tc.concrete_generic_method_signature_candidates(concrete_name, method) {
		if candidate in tc.fn_param_types || candidate in tc.fn_ret_types {
			return candidate
		}
		if indexed := tc.receiver_method_suffix_index[candidate] {
			if indexed != receiver_method_suffix_ambiguous {
				return indexed
			}
		}
	}
	if indexed := tc.receiver_method_suffix_index[key] {
		if indexed != receiver_method_suffix_ambiguous {
			return indexed
		}
	}
	if info := tc.embedded_method_call_info(concrete_name, method) {
		if info.name.len > 0 {
			return info.name
		}
	}
	return none
}

fn (tc &TypeChecker) concrete_generic_method_signature_candidates(concrete_name string, method string) []string {
	base, args, ok := generic_type_application_parts(concrete_name)
	if !ok || args.len == 0 || method.len == 0 {
		return []string{}
	}
	short_args := generic_type_args_short_for_signature(args)
	suffix := generic_type_suffix_for_signature(args)
	short_base := base.all_after_last('.')
	mut candidates := []string{}
	for receiver in [base, short_base] {
		candidates << '${receiver}[${short_args}].${method}'
		candidates << '${receiver}_${suffix}.${method}'
		candidates << tc.cached_c_name('${receiver}[${short_args}].${method}')
		candidates << tc.cached_c_name('${receiver}_${suffix}.${method}')
		candidates << '${tc.cached_c_name(receiver)}_${suffix}__${tc.cached_c_name(method)}'
	}
	return candidates
}

fn generic_type_args_short_for_signature(args []string) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << generic_type_arg_short_for_signature(arg)
	}
	return parts.join(', ')
}

fn generic_type_suffix_for_signature(args []string) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << naming.c_name(generic_type_arg_short_for_signature(arg).replace('[]', 'Array_').replace('&',
			'ptr_'))
	}
	return parts.join('_')
}

fn generic_type_arg_short_for_signature(type_arg string) string {
	clean := trimmed_space(type_arg)
	if clean.starts_with('[]') {
		return 'Array_${generic_type_arg_short_for_signature(clean[2..])}'
	}
	if clean.starts_with('&') {
		return 'ptr_${generic_type_arg_short_for_signature(clean[1..])}'
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len - 1 {
			key := generic_type_arg_short_for_signature(clean[4..bracket_end])
			value := generic_type_arg_short_for_signature(clean[bracket_end + 1..])
			return 'Map_${key}_${value}'
		}
	}
	if clean.starts_with('?') {
		return 'Option_${generic_type_arg_short_for_signature(clean[1..])}'
	}
	if clean.starts_with('!') {
		return 'Result_${generic_type_arg_short_for_signature(clean[1..])}'
	}
	base, args, is_generic := generic_type_application_parts(clean)
	if is_generic {
		mut parts := [generic_type_arg_short_for_signature(base)]
		for arg in args {
			parts << generic_type_arg_short_for_signature(arg)
		}
		return parts.join('_')
	}
	if clean.contains('.') {
		return clean
	}
	return clean
}

pub fn (tc &TypeChecker) named_type_compatible_with_ierror(concrete_name string) bool {
	cache_key := tc.ierror_compat_cache_key(concrete_name)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		mut fallback := cache.base
		for !isnil(fallback) {
			if cached := fallback.ierror_compat_entries[cache_key] {
				return cached > 0
			}
			fallback = fallback.base
		}
		if cached := cache.ierror_compat_entries[cache_key] {
			return cached > 0
		}
	}
	mut seen := map[string]bool{}
	result := tc.named_type_compatible_with_ierror_inner(concrete_name, mut seen)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		cache.ierror_compat_entries[cache_key] = if result { 1 } else { -1 }
	}
	return result
}

fn (tc &TypeChecker) ierror_compat_cache_key(concrete_name string) string {
	if concrete_name in ['Error', 'MessageError'] {
		return '${tc.cur_file}\n${tc.cur_module}\n${concrete_name}'
	}
	if concrete_name in tc.structs {
		return concrete_name
	}
	qname := tc.qualify_name(concrete_name)
	if qname in tc.structs {
		return qname
	}
	return concrete_name
}

// resolve_ierror_payload_name resolves scoped `Error`/`MessageError` names before
// falling back to the builtin error structs.
pub fn (tc &TypeChecker) resolve_ierror_payload_name(name string) string {
	if name !in ['Error', 'MessageError'] {
		return name
	}
	if resolved := tc.resolve_selective_import_type_symbol(name) {
		if resolved in tc.structs {
			return resolved
		}
	}
	if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
		local := '${tc.cur_module}.${name}'
		if local in tc.structs {
			return local
		}
	}
	return name
}

fn (tc &TypeChecker) type_compatible_with_ierror_payload(actual Type) bool {
	clean := tc.ierror_payload_concrete_type(actual)
	concrete_name := method_type_name(clean)
	if concrete_name.len == 0 {
		return false
	}
	return tc.named_type_compatible_with_ierror(concrete_name)
}

fn (tc &TypeChecker) ierror_payload_concrete_type(t Type) Type {
	mut clean := t
	mut seen := map[string]bool{}
	for {
		clean = unwrap_pointer(clean)
		if clean is Alias {
			if seen[clean.name] {
				return clean
			}
			seen[clean.name] = true
			clean = clean.base_type
			continue
		}
		return clean
	}
	return clean
}

fn (tc &TypeChecker) named_type_compatible_with_ierror_inner(concrete_name string, mut seen map[string]bool) bool {
	mut lookup := tc.resolve_ierror_payload_name(concrete_name)
	if lookup !in tc.structs {
		qname := tc.qualify_name(lookup)
		if qname in tc.structs {
			lookup = qname
		}
	}
	if lookup in ['Error', 'MessageError'] && tc.unqualified_type_name_shadows_builtin(lookup) {
		return false
	}
	if lookup in seen {
		return false
	}
	seen[lookup] = true
	if tc.is_builtin_error_struct_name(lookup) {
		return true
	}
	if tc.named_type_has_non_builtin_error_embed(lookup) {
		return false
	}
	if tc.named_type_implements_ierror_methods(lookup) {
		return true
	}
	struct_module := tc.struct_modules[lookup] or { tc.cur_module }
	struct_file := tc.struct_files[lookup] or { tc.cur_file }
	for field in tc.structs[lookup] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name.len == 0 {
			continue
		}
		if tc.embedded_field_is_scoped_builtin_error(field, embedded_name, struct_file,
			struct_module)
		{
			return true
		}
		if embedded_name in ['Error', 'MessageError'] && field.name == embedded_name {
			continue
		}
		if tc.is_builtin_error_struct_name(embedded_name) {
			return true
		}
		if tc.named_type_compatible_with_ierror_inner(embedded_name, mut seen) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) named_type_implements_ierror_methods(concrete_name string) bool {
	iface_name := if 'builtin.IError' in tc.interface_names { 'builtin.IError' } else { 'IError' }
	return tc.named_type_implements_interface(concrete_name, iface_name)
}

fn (tc &TypeChecker) named_type_has_non_builtin_error_embed(concrete_name string) bool {
	mut lookup := concrete_name
	if lookup !in tc.structs {
		qname := tc.qualify_name(lookup)
		if qname in tc.structs {
			lookup = qname
		}
	}
	if tc.struct_error_embeds_shadow_builtin[lookup] {
		return true
	}
	struct_module := tc.struct_modules[lookup] or { tc.cur_module }
	struct_file := tc.struct_files[lookup] or { tc.cur_file }
	if tc.source_struct_has_non_builtin_error_embed(lookup, struct_file, struct_module) {
		return true
	}
	for field in tc.structs[lookup] or { []StructField{} } {
		if field.name in ['Error', 'MessageError']
			&& tc.unqualified_type_name_shadows_builtin_in_scope(field.name, struct_file, struct_module) {
			return true
		}
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name.len == 0 {
			continue
		}
		if tc.embedded_field_is_scoped_builtin_error(field, embedded_name, struct_file,
			struct_module)
		{
			continue
		}
		if embedded_name in ['Error', 'MessageError'] && field.name == embedded_name {
			return true
		}
		if embedded_name.all_after_last('.') in ['Error', 'MessageError']
			&& !tc.is_builtin_error_struct_name(embedded_name) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) source_struct_has_non_builtin_error_embed(concrete_name string, file string, mod_name string) bool {
	if isnil(tc.a) {
		return false
	}
	key := source_error_embed_lookup_key(concrete_name, file, mod_name)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		if !cache.source_error_embed_indexed {
			mut fallback := cache.base
			for !isnil(fallback) {
				if fallback.source_error_embed_indexed {
					return fallback.source_error_embed_entries[key] > 0
				}
				fallback = fallback.base
			}
			cache.source_error_embed_entries = tc.collect_source_error_embed_entries()
			cache.source_error_embed_indexed = true
		}
		return cache.source_error_embed_entries[key] > 0
	}
	entries := tc.collect_source_error_embed_entries()
	return entries[key] > 0
}

// precompute_source_error_embed_index builds the immutable source-error embedding
// index once before parallel consumers fork their private memoization overlays.
pub fn (tc &TypeChecker) precompute_source_error_embed_index() {
	if isnil(tc.type_cache) {
		return
	}
	mut cache := unsafe { tc.type_cache }
	if cache.source_error_embed_indexed {
		return
	}
	mut fallback := cache.base
	for !isnil(fallback) {
		if fallback.source_error_embed_indexed {
			return
		}
		fallback = fallback.base
	}
	cache.source_error_embed_entries = tc.collect_source_error_embed_entries()
	cache.source_error_embed_indexed = true
}

fn (tc &TypeChecker) collect_source_error_embed_entries() map[string]int {
	mut entries := map[string]int{}
	if isnil(tc.a) {
		return entries
	}
	mut cur_file := ''
	mut cur_module := ''
	if tc.top_level_idx.len > 0 && tc.a.nodes.len == tc.top_level_idx_nodes_len {
		// struct_decl nodes only occur at the top level, and the AST has not
		// grown since collect built the index.
		for i in tc.top_level_idx {
			node := tc.a.nodes[i]
			match node.kind {
				.file {
					cur_file = node.value
					cur_module = ''
				}
				.module_decl {
					cur_module = node.value
				}
				.struct_decl {
					if !tc.source_struct_decl_has_non_builtin_error_embed(node, cur_file,
						cur_module) {
						continue
					}
					target := node.value.all_after_last('.')
					module_key := source_error_embed_module_key(cur_module)
					entries[source_error_embed_entry_key(target, '', module_key)] = 1
					entries[source_error_embed_entry_key(target, cur_file, module_key)] = 1
				}
				else {}
			}
		}
		return entries
	}
	for node in tc.a.nodes {
		match node.kind {
			.file {
				cur_file = node.value
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.struct_decl {
				if !tc.source_struct_decl_has_non_builtin_error_embed(node, cur_file, cur_module) {
					continue
				}
				target := node.value.all_after_last('.')
				module_key := source_error_embed_module_key(cur_module)
				entries[source_error_embed_entry_key(target, '', module_key)] = 1
				entries[source_error_embed_entry_key(target, cur_file, module_key)] = 1
			}
			else {}
		}
	}
	return entries
}

fn (tc &TypeChecker) source_struct_decl_has_non_builtin_error_embed(node flat.Node, cur_file string, cur_module string) bool {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl {
			continue
		}
		field_typ := if field.typ.len > 0 { field.typ } else { field.value }
		if !source_field_decl_is_embed(field, field_typ) {
			continue
		}
		if field_typ in ['Error', 'MessageError']
			&& tc.unqualified_type_name_shadows_builtin_in_scope(field_typ, cur_file, cur_module) {
			return true
		}
		resolved := tc.resolve_imported_type_text_in_file(field_typ, cur_file)
		if resolved.all_after_last('.') in ['Error', 'MessageError']
			&& !tc.is_builtin_error_struct_name_in_scope(resolved, cur_file, cur_module) {
			return true
		}
	}
	return false
}

fn source_error_embed_lookup_key(concrete_name string, file string, mod_name string) string {
	target := concrete_name.all_after_last('.')
	target_module := if concrete_name.contains('.') {
		concrete_name.all_before_last('.')
	} else {
		mod_name
	}
	file_key := if target_module.len > 0 { '' } else { file }
	return source_error_embed_entry_key(target, file_key,
		source_error_embed_module_key(target_module))
}

fn source_error_embed_entry_key(target string, file string, module_key string) string {
	return '${file}\n${module_key}\n${target}'
}

fn source_error_embed_module_key(mod_name string) string {
	return if mod_name == 'main' { '' } else { mod_name }
}

fn source_field_decl_is_embed(field flat.Node, field_typ string) bool {
	return field.typ.len == 0 || field.value.len == 0 || field.value == field_typ
		|| (field_typ.contains('.') && field.value == field_typ.all_after_last('.'))
}

fn (tc &TypeChecker) embedded_field_is_scoped_builtin_error(field StructField, embedded_name string, struct_file string, struct_module string) bool {
	if embedded_name !in ['Error', 'MessageError'] || field.name != embedded_name {
		return false
	}
	return !tc.unqualified_type_name_shadows_builtin_in_scope(embedded_name, struct_file,
		struct_module)
}

fn (tc &TypeChecker) resolve_unqualified_builtin_error_struct_name(name string) ?string {
	if name !in ['Error', 'MessageError'] {
		return none
	}
	if tc.unqualified_type_name_shadows_builtin(name) {
		return none
	}
	if mod_name := tc.struct_modules[name] {
		if mod_name == 'builtin' {
			return name
		}
	}
	if tc.has_builtins {
		return name
	}
	return none
}

fn (tc &TypeChecker) unqualified_type_name_shadows_builtin(name string) bool {
	return tc.unqualified_type_name_shadows_builtin_in_scope(name, tc.cur_file, tc.cur_module)
}

fn (tc &TypeChecker) unqualified_type_name_shadows_builtin_in_scope(name string, file string, mod_name string) bool {
	local_name := if mod_name.len > 0 && mod_name !in ['', 'main', 'builtin'] {
		'${mod_name}.${name}'
	} else {
		name
	}
	if local_name == name && mod_name != 'builtin'
		&& tc.source_declares_type_in_scope(name, file, mod_name) {
		return true
	}
	if local_name != name && tc.type_symbol_known(local_name) {
		return true
	}
	if file_import_key(file, name) in tc.file_selective_imports {
		if resolved := tc.resolve_selective_import_type_symbol_in_file(name, file) {
			return !tc.is_builtin_error_struct_name(resolved)
		}
		return true
	}
	if local_name != name {
		return false
	}
	if name in tc.structs {
		return tc.struct_modules[name] or { '' } != 'builtin'
	}
	if name in tc.type_aliases || name in tc.sum_types || name in tc.interface_names
		|| name in tc.enum_names {
		return true
	}
	return false
}

fn (tc &TypeChecker) source_declares_type_in_scope(name string, file string, mod_name string) bool {
	if file.len == 0 || isnil(tc.a) {
		return false
	}
	return scope_type_key(file, mod_name, name) in tc.declared_type_scope_keys
}

// scope_type_key builds the lookup key used by `declared_type_scope_keys`.
// The module is normalized so that '' and 'main' collapse to the same bucket,
// matching the old `module_names_match` semantics.
fn scope_type_key(file string, mod_name string, name string) string {
	norm_mod := if mod_name == '' || mod_name == 'main' { 'main' } else { mod_name }
	return '${file}\x01${norm_mod}\x01${name}'
}

fn (tc &TypeChecker) resolve_selective_import_type_symbol_in_file(name string, file string) ?string {
	candidates := tc.file_selective_imports[file_import_key(file, name)] or { return none }
	for candidate in candidates {
		if tc.type_symbol_known(candidate) {
			return candidate
		}
	}
	return none
}

pub fn (tc &TypeChecker) resolve_imported_type_text_in_file(typ string, file string) string {
	if !typ.contains('.') || typ.starts_with('C.') {
		return typ
	}
	dot := typ.index_u8(`.`)
	if dot <= 0 {
		return typ
	}
	alias := typ[..dot]
	if resolved := tc.file_imports[file_import_key(file, alias)] {
		if resolved != alias {
			return resolved + typ[dot..]
		}
	}
	return typ
}

fn (tc &TypeChecker) is_builtin_error_struct_name(name string) bool {
	return tc.is_builtin_error_struct_name_in_scope(name, tc.cur_file, tc.cur_module)
}

fn (tc &TypeChecker) is_builtin_error_struct_name_in_scope(name string, file string, mod_name string) bool {
	if name in ['builtin.Error', 'builtin.MessageError'] {
		return true
	}
	if name in ['Error', 'MessageError'] {
		if tc.unqualified_type_name_shadows_builtin_in_scope(name, file, mod_name) {
			return false
		}
		if mod := tc.struct_modules[name] {
			if mod == 'builtin' {
				return true
			}
		}
		return tc.has_builtins
	}
	return false
}

// prepare_interface_query_indexes publishes immutable interface requirements for
// transform workers. Interface declarations no longer change after semantic
// checking, so compatibility scans can reuse these lists safely across threads.
pub fn (mut tc TypeChecker) prepare_interface_requirement_indexes() {
	tc.interface_query_indexes_ready = false
	tc.interface_method_names_index = map[string][]string{}
	tc.interface_abstract_index = map[string][]string{}
	tc.interface_field_list_index = map[string][]StructField{}
	mut direct_methods := map[string][]string{}
	for iface_name in tc.interface_names.keys() {
		name := tc.interface_metadata_name(iface_name)
		if name !in direct_methods {
			direct_methods[name] = []string{}
		}
	}
	// Default interface methods are stored with ordinary function signatures.
	// Index them once instead of scanning the full signature table separately
	// for every interface (and again for each embedded-interface traversal).
	for key, _ in tc.fn_ret_types {
		dot := key.last_index_u8(`.`)
		if dot <= 0 {
			continue
		}
		receiver := tc.interface_metadata_name(key[..dot])
		mut methods := direct_methods[receiver] or { continue }
		method := key[dot + 1..]
		if method !in methods {
			methods << method
			direct_methods[receiver] = methods
		}
	}
	for iface_name in tc.interface_names.keys() {
		mut seen_methods := map[string]bool{}
		methods := tc.interface_method_names_indexed(iface_name, direct_methods, mut seen_methods)
		mut seen_abstract := map[string]bool{}
		abstract_methods := tc.interface_abstract_method_names_inner(iface_name, mut seen_abstract)
		mut seen_fields := map[string]bool{}
		fields := tc.interface_field_list_inner(iface_name, mut seen_fields)
		resolved := tc.interface_metadata_name(iface_name)
		for key in [iface_name, resolved] {
			if key.len == 0 {
				continue
			}
			tc.interface_method_names_index[key] = methods
			tc.interface_abstract_index[key] = abstract_methods
			tc.interface_field_list_index[key] = fields
		}
	}
	tc.interface_query_indexes_ready = true
}

fn (tc &TypeChecker) interface_method_names_indexed(iface_name string, direct_methods map[string][]string, mut seen map[string]bool) []string {
	name := tc.interface_metadata_name(iface_name)
	if name in seen {
		return []string{}
	}
	seen[name] = true
	mut methods := []string{}
	for embed in tc.interface_embeds[name] or { []string{} } {
		for method in tc.interface_method_names_indexed(embed, direct_methods, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	for method in direct_methods[name] or { []string{} } {
		if method !in methods {
			methods << method
		}
	}
	return methods
}

// prepare_interface_query_indexes publishes immutable interface requirements and
// implementer lists for transform workers.
pub fn (mut tc TypeChecker) prepare_interface_query_indexes() {
	tc.prepare_interface_requirement_indexes()
	tc.interface_impl_indexes = map[string]&InterfaceImplIndex{}
	tc.interface_impl_candidates_at_index = tc.interface_impl_candidate_names()
	iface_names := tc.interface_names.keys()
	if !tc.prepare_interface_impl_indexes_parallel(iface_names) {
		for iface_name in iface_names {
			impls := if is_builtin_ierror_name(iface_name) {
				tc.ierror_impl_names()
			} else {
				tc.interface_impl_names(iface_name)
			}
			tc.interface_impl_indexes[iface_name] = &InterfaceImplIndex{
				names: impls
				ids:   stable_interface_type_ids(impls)
			}
		}
	}
	// The immutable transform indexes above preserve the pre-monomorphization
	// order. Do not leave the same lists in the mutable query cache: generic
	// materialization can add implementers before cgen queries it again.
	tc.clear_interface_impl_cache()
}

// interface_method_names supports interface method names handling for TypeChecker.
fn (tc &TypeChecker) interface_method_names(iface_name string) []string {
	if tc.interface_query_indexes_ready {
		if methods := tc.interface_method_names_index[iface_name] {
			return methods
		}
	}
	mut seen := map[string]bool{}
	return tc.interface_method_names_inner(iface_name, mut seen)
}

// interface_abstract_method_names returns the methods an implementer must provide:
// the interface's own declared (abstract) methods plus those of any embedded
// interfaces. Default methods defined directly on the interface are excluded.
pub fn (tc &TypeChecker) interface_abstract_method_names(iface_name string) []string {
	if tc.interface_query_indexes_ready {
		if methods := tc.interface_abstract_index[iface_name] {
			return methods
		}
	}
	mut seen := map[string]bool{}
	return tc.interface_abstract_method_names_inner(iface_name, mut seen)
}

// interface_abstract_method_names_inner supports interface_abstract_method_names_inner handling.
fn (tc &TypeChecker) interface_abstract_method_names_inner(iface_name string, mut seen map[string]bool) []string {
	name := tc.interface_metadata_name(iface_name)
	if name in seen {
		return []string{}
	}
	seen[name] = true
	mut methods := []string{}
	for embed in tc.interface_embeds[name] or { []string{} } {
		for method in tc.interface_abstract_method_names_inner(embed, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	for method in tc.interface_abstract_methods[name] or { []string{} } {
		if method !in methods {
			methods << method
		}
	}
	return methods
}

// interface_method_names_inner supports interface method names inner handling for TypeChecker.
fn (tc &TypeChecker) interface_method_names_inner(iface_name string, mut seen map[string]bool) []string {
	name := tc.interface_metadata_name(iface_name)
	if name in seen {
		return []string{}
	}
	seen[name] = true
	mut methods := []string{}
	for embed in tc.interface_embeds[name] or { []string{} } {
		for method in tc.interface_method_names_inner(embed, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	prefix := '${name}.'
	params := tc.interface_generic_params[name] or { []string{} }
	open_prefix := if params.len > 0 { '${name}[${params.join(', ')}].' } else { '' }
	for key, _ in tc.fn_ret_types {
		if key.starts_with(prefix) || (open_prefix.len > 0 && key.starts_with(open_prefix)) {
			method := if key.starts_with(prefix) {
				key[prefix.len..]
			} else {
				key[open_prefix.len..]
			}
			if method !in methods {
				methods << method
			}
		}
	}
	return methods
}

pub fn (tc &TypeChecker) interface_method_signature_key(iface_name string, method string) ?string {
	name := tc.interface_metadata_name(iface_name)
	key := '${name}.${method}'
	if key in tc.fn_ret_types || key in tc.fn_param_types {
		return key
	}
	params := tc.interface_generic_params[name] or { []string{} }
	if params.len > 0 {
		open_key := '${name}[${params.join(', ')}].${method}'
		if open_key in tc.fn_ret_types || open_key in tc.fn_param_types {
			return open_key
		}
	}
	for embed in tc.interface_embeds[name] or { []string{} } {
		if found := tc.interface_method_signature_key(embed, method) {
			return found
		}
	}
	return none
}

fn (tc &TypeChecker) interface_receiver_method_call_info(iface_name string, method string) ?CallInfo {
	metadata_name := tc.interface_metadata_name(iface_name)
	if metadata_name !in tc.interface_names {
		return none
	}
	decl_key := tc.interface_method_signature_key(iface_name, method) or { return none }
	decl_params, return_type := tc.specialized_interface_method_signature(iface_name, decl_key)
	if decl_params.len == 0 {
		return none
	}
	mut params := []Type{cap: decl_params.len}
	params << Type(Pointer{
		base_type: Type(Interface{
			name: iface_name
		})
	})
	if decl_params.len > 1 {
		for i in 1 .. decl_params.len {
			params << decl_params[i]
		}
	}
	call_name := '${iface_name}.${method}'
	return CallInfo{
		name:          call_name
		params:        params
		shared_params: tc.fn_shared_params[decl_key] or { []bool{} }
		return_type:   return_type
		has_receiver:  true
		params_known:  true
	}
}

// interface_field_list supports interface field list handling for TypeChecker.
pub fn (tc &TypeChecker) interface_field_list(iface_name string) []StructField {
	if tc.interface_query_indexes_ready {
		if fields := tc.interface_field_list_index[iface_name] {
			return fields
		}
	}
	mut seen := map[string]bool{}
	return tc.interface_field_list_inner(iface_name, mut seen)
}

// interface_field_list_inner supports interface field list inner handling for TypeChecker.
fn (tc &TypeChecker) interface_field_list_inner(iface_name string, mut seen map[string]bool) []StructField {
	name := tc.interface_metadata_name(iface_name)
	if name in seen {
		return []StructField{}
	}
	seen[name] = true
	mut fields := []StructField{}
	mut field_indexes := map[string]int{}
	for embed in tc.interface_embeds[name] or { []string{} } {
		for field in tc.interface_field_list_inner(embed, mut seen) {
			if idx := field_indexes[field.name] {
				fields[idx] = field
			} else {
				field_indexes[field.name] = fields.len
				fields << field
			}
		}
	}
	for field in tc.interface_fields[name] or { []StructField{} } {
		if idx := field_indexes[field.name] {
			fields[idx] = field
		} else {
			field_indexes[field.name] = fields.len
			fields << field
		}
	}
	return fields
}

// interface_field_type supports interface field type handling for TypeChecker.
fn (tc &TypeChecker) interface_field_type(iface_name string, field_name string) ?Type {
	for field in tc.interface_field_list(iface_name) {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

// struct_field_type supports struct field type handling for TypeChecker.
fn (tc &TypeChecker) struct_init_field_lookup_name(literal_name string, parsed_name string) string {
	clean := trimmed_space(literal_name)
	if clean.len == 0 {
		return parsed_name
	}
	if generic_type_application(clean) {
		_, _, parsed_is_generic := generic_type_application_parts(parsed_name)
		if parsed_is_generic {
			return parsed_name
		}
		bracket := clean.index_u8(`[`)
		if bracket > 0 {
			base := if parsed_name.len > 0 { parsed_name } else { trimmed_space(clean[..bracket]) }
			return base + clean[bracket..]
		}
	}
	if parsed_name.len > 0 {
		return parsed_name
	}
	return clean
}

fn (tc &TypeChecker) struct_fields_for_init(struct_name string) []StructField {
	base_name, generic_args, is_generic := generic_type_application_parts(struct_name)
	raw_lookup_name := if is_generic { base_name } else { struct_name }
	lookup_name := if raw_lookup_name in tc.structs {
		raw_lookup_name
	} else if raw_lookup_name.all_after_last('.') in tc.structs {
		raw_lookup_name.all_after_last('.')
	} else if canonical := tc.canonical_qualified_type_name(raw_lookup_name) {
		canonical
	} else {
		raw_lookup_name
	}
	fields := tc.structs[lookup_name] or { return []StructField{} }
	if !is_generic {
		return fields
	}
	params := tc.struct_generic_params[lookup_name] or {
		tc.struct_generic_params[base_name] or { return fields }
	}
	if params.len != generic_args.len {
		return fields
	}
	mut concrete_fields := []StructField{cap: fields.len}
	for field in fields {
		concrete_fields << StructField{
			name:        field.name
			typ:         tc.substitute_generic_type(field.typ, generic_args, params)
			has_default: field.has_default
			is_embed:    field.is_embed
			is_mut:      field.is_mut
			is_volatile: field.is_volatile
		}
	}
	return concrete_fields
}

// struct_fields_for_type returns the fields of `struct_name`, with generic
// parameters substituted when `struct_name` is a concrete generic instance.
pub fn (tc &TypeChecker) struct_fields_for_type(struct_name string) []StructField {
	return tc.struct_fields_for_init(struct_name)
}

fn (tc &TypeChecker) struct_field_type(struct_name string, field_name string) ?Type {
	if !isnil(tc.type_cache) {
		cache := tc.type_cache
		if cache.struct_field_last_state != 0
			&& cache.struct_field_last_struct == usize(struct_name.str)
			&& cache.struct_field_last_field == usize(field_name.str)
			&& cache.struct_field_last_struct_n == struct_name.len
			&& cache.struct_field_last_field_n == field_name.len {
			if cache.struct_field_last_state > 0 {
				return cache.struct_field_last_value
			}
			return none
		}
	}
	cache_key := '${struct_name}\n${field_name}'
	if !isnil(tc.type_cache) {
		mut fallback := tc.type_cache.base
		for !isnil(fallback) {
			if typ := fallback.struct_field_entries[cache_key] {
				tc.remember_struct_field_type(struct_name, field_name, typ, true)
				return typ
			}
			if fallback.struct_field_misses[cache_key] {
				tc.remember_struct_field_type(struct_name, field_name, Type(void_), false)
				return none
			}
			fallback = fallback.base
		}
		if typ := tc.type_cache.struct_field_entries[cache_key] {
			tc.remember_struct_field_type(struct_name, field_name, typ, true)
			return typ
		}
		if tc.type_cache.struct_field_misses[cache_key] {
			tc.remember_struct_field_type(struct_name, field_name, Type(void_), false)
			return none
		}
	}
	mut seen := map[string]bool{}
	if typ := tc.struct_field_type_inner(struct_name, field_name, mut seen) {
		if !isnil(tc.type_cache) {
			mut cache := tc.type_cache
			cache.struct_field_entries[cache_key] = typ
		}
		tc.remember_struct_field_type(struct_name, field_name, typ, true)
		return typ
	}
	if !isnil(tc.type_cache) {
		mut cache := tc.type_cache
		cache.struct_field_misses[cache_key] = true
	}
	tc.remember_struct_field_type(struct_name, field_name, Type(void_), false)
	return none
}

fn (tc &TypeChecker) remember_struct_field_type(struct_name string, field_name string, typ Type, found bool) {
	if isnil(tc.type_cache) {
		return
	}
	mut cache := tc.type_cache
	struct_ptr := usize(struct_name.str)
	field_ptr := usize(field_name.str)
	cache.struct_field_last_struct = struct_ptr
	cache.struct_field_last_field = field_ptr
	cache.struct_field_last_struct_n = struct_name.len
	cache.struct_field_last_field_n = field_name.len
	cache.struct_field_last_value = typ
	cache.struct_field_last_state = if found { i8(1) } else { i8(-1) }
}

// struct_field_type_name returns the canonical type name for a struct field.
pub fn (tc &TypeChecker) struct_field_type_name(struct_name string, field_name string) ?string {
	typ := tc.struct_field_type(struct_name, field_name) or { return none }
	return typ.name()
}

@[direct_array_access]
fn (tc &TypeChecker) struct_field_type_inner(struct_name string, field_name string, mut seen map[string]bool) ?Type {
	base_name, generic_args, is_generic := generic_type_application_parts(struct_name)
	raw_lookup_name := if is_generic { base_name } else { struct_name }
	lookup_name := if raw_lookup_name in tc.structs {
		raw_lookup_name
	} else if raw_lookup_name.all_after_last('.') in tc.structs {
		raw_lookup_name.all_after_last('.')
	} else if canonical := tc.canonical_qualified_type_name(raw_lookup_name) {
		canonical
	} else {
		raw_lookup_name
	}
	if lookup_name in seen {
		return none
	}
	seen[lookup_name] = true
	fields := tc.structs[lookup_name] or { []StructField{} }
	// The struct's own fields shadow promoted/embedded ones regardless of
	// declaration order, so scan all direct fields before any embed.
	for field in fields {
		if field.name == field_name {
			if is_generic {
				return tc.substitute_generic_type(field.typ, generic_args, tc.struct_generic_params[base_name] or {
					[]string{}
				})
			}
			return field.typ
		}
	}
	embeds_indexed := lookup_name in tc.struct_embed_receivers
	for field in fields {
		mut embedded_type := field.typ
		if embeds_indexed {
			if !field.is_embed {
				continue
			}
		} else {
			embedded_type = embedded_field_type(field) or { continue }
		}
		embedded_type = if is_generic {
			tc.substitute_generic_type(embedded_type, generic_args, tc.struct_generic_params[base_name] or {
				[]string{}
			})
		} else {
			embedded_type
		}
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		embedded_lookup_type := if embedded_type is Alias {
			embedded_type.base_type
		} else {
			embedded_type
		}
		embedded_lookup_name := method_type_name(unwrap_pointer(embedded_lookup_type))
		if embedded_name.len == 0 {
			continue
		}
		// A `mod.Inner` embed is promoted under its short name: `o.Inner`.
		// Same-module embeds already match the direct-field pass above.
		if embedded_name != field_name && embedded_name.all_after_last('.') == field_name {
			return embedded_type
		}
		embedded_base_name, _, embedded_is_generic := generic_type_application_parts(embedded_name)
		if embedded_is_generic && embedded_base_name.all_after_last('.') == field_name {
			return embedded_type
		}
		if embedded_lookup_name.len > 0 && embedded_lookup_name != field_name
			&& embedded_lookup_name.all_after_last('.') == field_name {
			return embedded_type
		}
		if typ := tc.struct_field_type_inner(embedded_lookup_name, field_name, mut seen) {
			return typ
		}
	}
	return none
}

// substitute_generic_type replaces generic placeholders in `typ` with the concrete
// `args`. When `param_names` (the struct/fn's declared type parameters, e.g.
// `['L', 'R']`) is provided, a placeholder is matched to its arg by its declared
// position — so `Pair[L, R]`'s `R` resolves to `args[1]`, not the letter-based
// `generic_param_index` guess (which maps any unrecognised name to 0). The
// letter-based fallback is kept only for callers that have no declared names.
fn (tc &TypeChecker) substitute_generic_type(typ Type, args []string, param_names []string) Type {
	if args.len == 0 {
		return typ
	}
	if typ is Unknown {
		if name := generic_placeholder_from_unknown(typ) {
			mut idx := if param_names.len > 0 { param_names.index(name) } else { -1 }
			if idx < 0 {
				idx = generic_param_index(name)
			}
			if idx >= 0 && idx < args.len {
				return tc.parse_type(trimmed_space(args[idx]))
			}
		}
		return typ
	}
	if typ is Array {
		return Type(Array{
			elem_type: tc.substitute_generic_type(typ.elem_type, args, param_names)
		})
	}
	if typ is ArrayFixed {
		return Type(ArrayFixed{
			elem_type: tc.substitute_generic_type(typ.elem_type, args, param_names)
			len:       typ.len
			len_expr:  typ.len_expr
		})
	}
	if typ is Map {
		return Type(Map{
			key_type:   tc.substitute_generic_type(typ.key_type, args, param_names)
			value_type: tc.substitute_generic_type(typ.value_type, args, param_names)
		})
	}
	if typ is Pointer {
		return Type(Pointer{
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is OptionType {
		base_type := tc.substitute_generic_type(typ.base_type, args, param_names)
		if base_type is OptionType {
			return base_type
		}
		return Type(OptionType{
			base_type: base_type
		})
	}
	if typ is ResultType {
		base_type := tc.substitute_generic_type(typ.base_type, args, param_names)
		if base_type is ResultType {
			return base_type
		}
		return Type(ResultType{
			base_type: base_type
		})
	}
	if typ is Struct {
		return Type(Struct{
			name: subst_generic_text(typ.name, args, param_names)
		})
	}
	if typ is Interface {
		return Type(Interface{
			name: subst_generic_text(typ.name, args, param_names)
		})
	}
	if typ is Alias {
		return Type(Alias{
			name:      subst_generic_text(typ.name, args, param_names)
			base_type: tc.substitute_generic_type(typ.base_type, args, param_names)
		})
	}
	if typ is SumType {
		if typ.name.contains('[') {
			return tc.parse_type(subst_generic_text(typ.name, args, param_names))
		}
		return typ
	}
	if typ is FnType {
		mut params := []Type{}
		for param in typ.params {
			params << tc.substitute_generic_type(param, args, param_names)
		}
		return Type(FnType{
			params:      params
			params_mut:  typ.params_mut.clone()
			return_type: tc.substitute_generic_type(typ.return_type, args, param_names)
		})
	}
	if typ is MultiReturn {
		mut parts := []Type{}
		for part in typ.types {
			parts << tc.substitute_generic_type(part, args, param_names)
		}
		return Type(MultiReturn{
			types: parts
		})
	}
	return typ
}

// substitute_generic_type_values replaces generic placeholders with already
// resolved semantic types. Unlike the text-based variant, it cannot reinterpret
// a caller-local type name in the generic declaration's module.
fn (tc &TypeChecker) substitute_generic_type_values(typ Type, args []Type, param_names []string) Type {
	if args.len == 0 {
		return typ
	}
	if typ is Unknown {
		if name := generic_placeholder_from_unknown(typ) {
			idx := if param_names.len > 0 {
				param_names.index(name)
			} else {
				generic_param_index(name)
			}
			if idx >= 0 && idx < args.len {
				return args[idx]
			}
		}
		return typ
	}
	if typ is Array {
		return Type(Array{
			elem_type: tc.substitute_generic_type_values(typ.elem_type, args, param_names)
		})
	}
	if typ is ArrayFixed {
		return Type(ArrayFixed{
			elem_type: tc.substitute_generic_type_values(typ.elem_type, args, param_names)
			len:       typ.len
			len_expr:  typ.len_expr
		})
	}
	if typ is Map {
		return Type(Map{
			key_type:   tc.substitute_generic_type_values(typ.key_type, args, param_names)
			value_type: tc.substitute_generic_type_values(typ.value_type, args, param_names)
		})
	}
	if typ is Pointer {
		return Type(Pointer{
			base_type: tc.substitute_generic_type_values(typ.base_type, args, param_names)
		})
	}
	if typ is OptionType {
		base_type := tc.substitute_generic_type_values(typ.base_type, args, param_names)
		if base_type is OptionType {
			return base_type
		}
		return Type(OptionType{
			base_type: base_type
		})
	}
	if typ is ResultType {
		base_type := tc.substitute_generic_type_values(typ.base_type, args, param_names)
		if base_type is ResultType {
			return base_type
		}
		return Type(ResultType{
			base_type: base_type
		})
	}
	if typ is Struct {
		return Type(Struct{
			name: substitute_generic_named_type_values(typ.name, args, param_names)
		})
	}
	if typ is Interface {
		return Type(Interface{
			name: substitute_generic_named_type_values(typ.name, args, param_names)
		})
	}
	if typ is SumType {
		return Type(SumType{
			name: substitute_generic_named_type_values(typ.name, args, param_names)
		})
	}
	if typ is FnType {
		mut params := []Type{cap: typ.params.len}
		for param in typ.params {
			params << tc.substitute_generic_type_values(param, args, param_names)
		}
		return Type(FnType{
			params:      params
			params_mut:  typ.params_mut.clone()
			return_type: tc.substitute_generic_type_values(typ.return_type, args, param_names)
		})
	}
	if typ is MultiReturn {
		mut parts := []Type{cap: typ.types.len}
		for part in typ.types {
			parts << tc.substitute_generic_type_values(part, args, param_names)
		}
		return Type(MultiReturn{
			types: parts
		})
	}
	return typ
}

fn substitute_generic_named_type_values(name string, args []Type, param_names []string) string {
	mut arg_names := []string{cap: args.len}
	for arg in args {
		arg_names << arg.name()
	}
	return subst_generic_text(name, arg_names, param_names)
}

fn (tc &TypeChecker) embedded_method_call_info(struct_name string, method string) ?CallInfo {
	mut seen := map[string]bool{}
	return tc.embedded_method_call_info_inner(struct_name, method, mut seen)
}

fn (tc &TypeChecker) embedded_method_call_info_inner(struct_name string, method string, mut seen map[string]bool) ?CallInfo {
	if seen[struct_name] {
		return none
	}
	seen[struct_name] = true
	for field in tc.struct_fields_for_type(struct_name) {
		embedded_type := embedded_field_type(field) or { continue }
		receiver := method_type_name(unwrap_pointer(embedded_type))
		if receiver.len == 0 {
			continue
		}
		// An embedded concrete generic receiver (`Collector[int]`) promotes the
		// method registered on its open declaration (`Collector[T].use`).
		if info := tc.resolve_generic_struct_method(receiver, method) {
			return info
		}
		mut method_names := ['${receiver}.${method}']
		base_name, _, is_generic := generic_type_application_parts(receiver)
		if is_generic {
			method_names << '${base_name}.${method}'
		}
		for method_name in method_names {
			if method_name in tc.fn_ret_types {
				info := tc.call_info(method_name, true)
				return CallInfo{
					name:          info.name
					params:        info.params
					shared_params: info.shared_params
					return_type:   info.return_type
					has_receiver:  info.has_receiver
					is_variadic:   info.is_variadic
					is_c_variadic: info.is_c_variadic
					params_known:  if receiver.contains('[') { false } else { info.params_known }
				}
			}
		}
		if info := tc.embedded_method_call_info_inner(receiver, method, mut seen) {
			return info
		}
	}
	return none
}

fn (tc &TypeChecker) struct_has_middleware_receiver(struct_name string) bool {
	if is_middleware_type_name(struct_name) {
		return true
	}
	for field in tc.struct_fields_for_type(struct_name) {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if is_middleware_type_name(embedded_name) {
			return true
		}
	}
	return false
}

fn is_middleware_type_name(name string) bool {
	base := if name.contains('[') { name.all_before('[') } else { name }
	return base == 'veb.Middleware'
}

fn (tc &TypeChecker) receiver_embeds(actual Type, expected Type) bool {
	actual_name := method_type_name(unalias_and_unwrap_pointer_type(actual))
	expected_name := method_type_name(unalias_and_unwrap_pointer_type(expected))
	if actual_name.len == 0 || expected_name.len == 0 {
		return false
	}
	mut seen := map[string]bool{}
	return tc.receiver_embeds_inner(actual_name, expected_name, mut seen)
}

fn (tc &TypeChecker) receiver_embeds_inner(actual_name string, expected_name string, mut seen map[string]bool) bool {
	if seen[actual_name] {
		return false
	}
	seen[actual_name] = true
	for field in tc.struct_fields_for_type(actual_name) {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name == expected_name {
			return true
		}
		if tc.receiver_embeds_inner(embedded_name, expected_name, mut seen) {
			return true
		}
	}
	return false
}

// trimmed_space is an allocation-free fast path for trim_space: type texts on
// the checker's hot paths are almost always already clean, and builtin trim
// clones even when there is nothing to trim.
@[inline]
fn trimmed_space(s string) string {
	if s.len == 0 {
		return s
	}
	c0 := s[0]
	cl := s[s.len - 1]
	if c0 != ` ` && c0 != `\n` && c0 != `\t` && c0 != `\v` && c0 != `\f` && c0 != `\r` && cl != ` `
		&& cl != `\n` && cl != `\t` && cl != `\v` && cl != `\f` && cl != `\r` {
		return s
	}
	return s.trim_space()
}

fn embedded_field_type(field StructField) ?Type {
	if field.is_embed {
		return field.typ
	}
	field_type_name := method_type_name(unwrap_pointer(field.typ))
	if field_type_name.len == 0 {
		return none
	}
	if field.name.len == 0 {
		return field.typ
	}
	if embedded_name_matches(field.name, field_type_name) {
		return field.typ
	}
	base_name, _, is_generic := generic_type_application_parts(field_type_name)
	if is_generic && embedded_name_matches(field.name, base_name) {
		return field.typ
	}
	return none
}

// embedded_name_matches reports whether `field_name` equals `type_name` or its
// last dotted segment. Field names cannot contain `.`, so a suffix match with a
// `.` boundary is exactly the old `all_after_last('.')` comparison without the
// substring allocation (this runs per struct field on very hot lookup paths).
@[direct_array_access]
fn embedded_name_matches(field_name string, type_name string) bool {
	if field_name == type_name {
		return true
	}
	if type_name.len > field_name.len && type_name.ends_with(field_name) {
		return type_name[type_name.len - field_name.len - 1] == `.`
	}
	return false
}

// method_signature_compatible supports method signature compatible handling for TypeChecker.
fn (tc &TypeChecker) method_signature_compatible(actual_key string, expected_key string) bool {
	actual_params := tc.fn_param_types[actual_key] or { return false }
	expected_params := tc.fn_param_types[expected_key] or { return false }
	if actual_params.len != expected_params.len {
		return false
	}
	expected_receiver_mut, expected_receiver_shared := tc.method_receiver_flags(expected_key)
	_, actual_receiver_shared := tc.method_receiver_flags(actual_key)
	if expected_receiver_mut && actual_receiver_shared && !expected_receiver_shared {
		return false
	}
	for i in 1 .. actual_params.len {
		if !tc.method_param_signature_compatible(actual_params[i], expected_params[i]) {
			return false
		}
	}
	actual_ret := tc.fn_ret_types[actual_key] or { Type(void_) }
	expected_ret := tc.fn_ret_types[expected_key] or { Type(void_) }
	return tc.method_return_signature_compatible(actual_ret, expected_ret)
}

pub fn (tc &TypeChecker) specialized_interface_method_signature(iface_name string, expected_key string) ([]Type, Type) {
	params := tc.fn_param_types[expected_key] or { []Type{} }
	ret := tc.fn_ret_types[expected_key] or { Type(void_) }
	base, args, is_generic := generic_type_application_parts(iface_name)
	if !is_generic || args.len == 0 {
		return params, ret
	}
	meta_base := tc.interface_metadata_name(base)
	param_names := tc.interface_generic_params[meta_base] or {
		tc.interface_generic_params[base] or {
			tc.interface_generic_params[base.all_after_last('.')] or { return params, ret }
		}
	}
	if param_names.len != args.len {
		return params, ret
	}
	mut concrete_types := []Type{cap: args.len}
	for arg in args {
		concrete_types << tc.parse_type(trimmed_space(arg))
	}
	mut specialized_params := []Type{cap: params.len}
	for param in params {
		specialized_params << tc.substitute_generic_type_values(param, concrete_types, param_names)
	}
	return specialized_params, tc.substitute_generic_type_values(ret, concrete_types, param_names)
}

fn (tc &TypeChecker) method_signature_compatible_for_interface(actual_key string, expected_key string, iface_name string) bool {
	actual_params := tc.fn_param_types[actual_key] or { return false }
	expected_params, expected_ret := tc.specialized_interface_method_signature(iface_name,
		expected_key)
	if actual_params.len != expected_params.len {
		return false
	}
	expected_receiver_mut, expected_receiver_shared := tc.method_receiver_flags(expected_key)
	_, actual_receiver_shared := tc.method_receiver_flags(actual_key)
	if expected_receiver_mut && actual_receiver_shared && !expected_receiver_shared {
		return false
	}
	for i in 1 .. actual_params.len {
		if !tc.method_param_signature_compatible(actual_params[i], expected_params[i]) {
			return false
		}
	}
	actual_ret := tc.fn_ret_types[actual_key] or { Type(void_) }
	return tc.method_return_signature_compatible(actual_ret, expected_ret)
}

fn (tc &TypeChecker) method_call_info_signature_compatible(actual CallInfo, expected_key string) bool {
	expected_params := tc.fn_param_types[expected_key] or { return false }
	if actual.params.len != expected_params.len {
		return false
	}
	for i in 1 .. actual.params.len {
		if !tc.method_param_signature_compatible(actual.params[i], expected_params[i]) {
			return false
		}
	}
	expected_ret := tc.fn_ret_types[expected_key] or { Type(void_) }
	return tc.method_return_signature_compatible(actual.return_type, expected_ret)
}

fn (tc &TypeChecker) method_call_info_signature_compatible_for_interface(actual CallInfo, expected_key string, iface_name string) bool {
	expected_params, expected_ret := tc.specialized_interface_method_signature(iface_name,
		expected_key)
	if actual.params.len != expected_params.len {
		return false
	}
	for i in 1 .. actual.params.len {
		if !tc.method_param_signature_compatible(actual.params[i], expected_params[i]) {
			return false
		}
	}
	return tc.method_return_signature_compatible(actual.return_type, expected_ret)
}

fn (tc &TypeChecker) method_return_signature_compatible(actual Type, expected Type) bool {
	if tc.raw_type_equality {
		actual_w0, actual_w1, _ := type_value_words(&actual)
		expected_w0, expected_w1, _ := type_value_words(&expected)
		if actual_w0 == expected_w0 && actual_w1 == expected_w1 {
			return true
		}
	}
	if fn_return_canonical_type_name(actual) == fn_return_canonical_type_name(expected) {
		return true
	}
	actual_unaliased := unalias_type(actual)
	expected_unaliased := unalias_type(expected)
	if actual_unaliased is MultiReturn && expected_unaliased is MultiReturn {
		if actual_unaliased.types.len != expected_unaliased.types.len {
			return false
		}
		for i, typ in actual_unaliased.types {
			if !tc.method_return_signature_compatible(typ, expected_unaliased.types[i]) {
				return false
			}
		}
		return true
	}
	if actual_unaliased is OptionType && expected_unaliased is OptionType {
		return tc.method_wrapped_return_signature_compatible(actual_unaliased.base_type,
			expected_unaliased.base_type)
	}
	if actual_unaliased is ResultType && expected_unaliased is ResultType {
		return tc.method_wrapped_return_signature_compatible(actual_unaliased.base_type,
			expected_unaliased.base_type)
	}
	return tc.method_interface_return_signature_compatible(actual_unaliased, expected_unaliased)
}

fn (tc &TypeChecker) method_wrapped_return_signature_compatible(actual Type, expected Type) bool {
	actual_unaliased := unalias_type(actual)
	expected_unaliased := unalias_type(expected)
	if actual_unaliased is MultiReturn && expected_unaliased is MultiReturn {
		return tc.method_return_signature_compatible(actual_unaliased, expected_unaliased)
	}
	if actual_unaliased is FnType && expected_unaliased is FnType {
		return Type(actual_unaliased).name() == Type(expected_unaliased).name()
	}
	return tc.method_interface_return_signature_compatible(actual_unaliased, expected_unaliased)
}

fn (tc &TypeChecker) method_interface_return_signature_compatible(actual Type, expected Type) bool {
	expected_name := expected.name()
	if expected is Interface {
		return tc.type_implements_interface(actual, expected)
	}
	if expected_name in tc.interface_names {
		return tc.type_implements_interface(actual, Interface{
			name: expected_name
		})
	}
	return false
}

fn (tc &TypeChecker) method_param_signature_compatible(actual Type, expected Type) bool {
	if type_pointer_depth(actual) != type_pointer_depth(expected) {
		return false
	}
	if actual_iface := tc.method_param_interface_name(actual) {
		expected_iface := tc.method_param_interface_name(expected) or { return false }
		return actual_iface == expected_iface
			|| tc.interface_implements_interface(actual_iface, expected_iface)
	}
	if _ := tc.method_param_interface_name(expected) {
		return false
	}
	return tc.type_compatible(actual, expected) && tc.type_compatible(expected, actual)
}

fn (tc &TypeChecker) method_param_interface_name(typ Type) ?string {
	clean := unwrap_pointer(typ)
	if clean is Interface {
		return tc.interface_metadata_name(clean.name)
	}
	name := clean.name()
	if name in tc.interface_names {
		return tc.interface_metadata_name(name)
	}
	return none
}

fn type_pointer_depth(t Type) int {
	if t is Pointer {
		return 1 + type_pointer_depth(t.base_type)
	}
	if t is Alias {
		return type_pointer_depth(t.base_type)
	}
	return 0
}

// method_type_name supports method type name handling for types.
fn method_type_name(t Type) string {
	if t is Alias {
		return t.name
	}
	if t is Struct {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if t is SumType {
		return t.name
	}
	if t is Enum {
		return t.name
	}
	if t is String {
		return 'string'
	}
	if t is Char {
		return 'char'
	}
	if t is Primitive {
		return prim_name(t)
	}
	if t is ISize {
		return 'isize'
	}
	if t is USize {
		return 'usize'
	}
	if t is Rune {
		return 'rune'
	}
	return ''
}

fn (tc &TypeChecker) sum_base_name(sum_name string) string {
	base, _, ok := generic_type_application_parts(sum_name)
	if ok {
		return base
	}
	if sum_name in tc.sum_types {
		return sum_name
	}
	qname := tc.qualify_name(sum_name)
	if qname in tc.sum_types {
		return qname
	}
	if sum_name.contains('.') {
		resolved := tc.resolve_imported_type_text(sum_name)
		if resolved in tc.sum_types {
			return resolved
		}
		if unique := tc.unique_qualified_type_name(sum_name.all_after_last('.')) {
			if unique in tc.sum_types {
				return unique
			}
		}
	}
	return sum_name
}

fn (tc &TypeChecker) sum_params_for_base(base string) []string {
	if params := tc.sum_generic_params[base] {
		return params
	}
	short := base.all_after_last('.')
	if params := tc.sum_generic_params[short] {
		return params
	}
	return []string{}
}

fn (tc &TypeChecker) concrete_sum_variant_name(sum_name string, variant string) string {
	base, args, ok := generic_type_application_parts(sum_name)
	if !ok {
		return variant
	}
	params := tc.sum_params_for_base(base)
	if params.len == 0 || params.len != args.len {
		return variant
	}
	return subst_generic_text(variant, args, params)
}

pub fn (tc &TypeChecker) generic_type_name_matches(a string, b string) bool {
	if a == b {
		return true
	}
	if tc.fixed_array_type_name_matches(a, b) {
		return true
	}
	a_base, a_args, a_ok := generic_type_application_parts(a)
	b_base, b_args, b_ok := generic_type_application_parts(b)
	if a_ok || b_ok {
		if !a_ok || !b_ok || a_args.len != b_args.len {
			return false
		}
		if !tc.generic_type_base_matches(a_base, b_base) {
			return false
		}
		for i in 0 .. a_args.len {
			if !tc.generic_type_arg_matches(a_args[i], b_args[i]) {
				return false
			}
		}
		return true
	}
	return tc.generic_type_base_matches(a, b)
}

fn (tc &TypeChecker) fixed_array_type_name_matches(a string, b string) bool {
	if !fixed_array_type_name_may_be(a) || !fixed_array_type_name_may_be(b) {
		return false
	}
	a_type := tc.parse_type(a)
	b_type := tc.parse_type(b)
	if a_type is ArrayFixed && b_type is ArrayFixed {
		return tc.fixed_array_lengths_compatible(a_type, b_type)
			&& tc.generic_type_name_matches(a_type.elem_type.name(), b_type.elem_type.name())
	}
	return false
}

fn fixed_array_type_name_may_be(s string) bool {
	clean := trimmed_space(s)
	if clean.len == 0 || clean.starts_with('[]') || clean.starts_with('map[') {
		return false
	}
	if clean.starts_with('[') {
		end := find_matching_bracket(clean, 0)
		return end > 0 && end + 1 < clean.len
	}
	if !clean.ends_with(']') {
		return false
	}
	bracket := clean.last_index_u8(`[`)
	bracket_end := clean.last_index_u8(`]`)
	if bracket <= 0 || bracket_end <= bracket {
		return false
	}
	return is_fixed_array_len_text(clean[bracket + 1..bracket_end])
}

fn (tc &TypeChecker) generic_type_base_matches(a string, b string) bool {
	a_clean := trimmed_space(a)
	b_clean := trimmed_space(b)
	if a_clean == b_clean {
		return true
	}
	a_resolved := tc.resolve_generic_match_base(a_clean)
	b_resolved := tc.resolve_generic_match_base(b_clean)
	if a_resolved == b_resolved {
		return true
	}
	if a_clean.contains('.') || b_clean.contains('.') || a_resolved.contains('.')
		|| b_resolved.contains('.') {
		return false
	}
	return short_type_name(a_clean) == short_type_name(b_clean)
}

fn (tc &TypeChecker) resolve_generic_match_base(base string) string {
	if base.len == 0 {
		return base
	}
	if base.contains('.') {
		return tc.resolve_imported_type_text(base)
	}
	if resolved := tc.resolve_selective_import_type_symbol(base) {
		return resolved
	}
	qbase := tc.qualify_name(base)
	if qbase != base && tc.type_symbol_known(qbase) {
		return qbase
	}
	return base
}

fn (tc &TypeChecker) generic_type_arg_matches(a string, b string) bool {
	a_clean := trimmed_space(a)
	b_clean := trimmed_space(b)
	if a_clean == b_clean {
		return true
	}
	if tc.generic_match_arg_is_open_param(a_clean) || tc.generic_match_arg_is_open_param(b_clean) {
		return true
	}
	if a_clean.starts_with('&') || b_clean.starts_with('&') {
		return a_clean.starts_with('&') && b_clean.starts_with('&')
			&& tc.generic_type_arg_matches(a_clean[1..], b_clean[1..])
	}
	if a_clean.starts_with('mut ') || b_clean.starts_with('mut ') {
		return a_clean.starts_with('mut ') && b_clean.starts_with('mut ')
			&& tc.generic_type_arg_matches(a_clean[4..], b_clean[4..])
	}
	if a_clean.starts_with('?') || b_clean.starts_with('?') {
		return a_clean.starts_with('?') && b_clean.starts_with('?')
			&& tc.generic_type_arg_matches(a_clean[1..], b_clean[1..])
	}
	if a_clean.starts_with('!') || b_clean.starts_with('!') {
		return a_clean.starts_with('!') && b_clean.starts_with('!')
			&& tc.generic_type_arg_matches(a_clean[1..], b_clean[1..])
	}
	if a_clean.starts_with('...') || b_clean.starts_with('...') {
		return a_clean.starts_with('...') && b_clean.starts_with('...')
			&& tc.generic_type_arg_matches(a_clean[3..], b_clean[3..])
	}
	if a_clean.starts_with('[]') || b_clean.starts_with('[]') {
		return a_clean.starts_with('[]') && b_clean.starts_with('[]')
			&& tc.generic_type_arg_matches(a_clean[2..], b_clean[2..])
	}
	if a_clean.starts_with('map[') || b_clean.starts_with('map[') {
		if !a_clean.starts_with('map[') || !b_clean.starts_with('map[') {
			return false
		}
		a_bracket_end := find_matching_bracket(a_clean, 3)
		b_bracket_end := find_matching_bracket(b_clean, 3)
		if a_bracket_end >= a_clean.len || b_bracket_end >= b_clean.len {
			return false
		}
		return tc.generic_type_arg_matches(a_clean[4..a_bracket_end], b_clean[4..b_bracket_end])
			&& tc.generic_type_arg_matches(a_clean[a_bracket_end + 1..], b_clean[b_bracket_end + 1..])
	}
	if a_clean.starts_with('[') || b_clean.starts_with('[') {
		if !a_clean.starts_with('[') || !b_clean.starts_with('[') {
			return false
		}
		a_bracket_end := find_matching_bracket(a_clean, 0)
		b_bracket_end := find_matching_bracket(b_clean, 0)
		if a_bracket_end >= a_clean.len || b_bracket_end >= b_clean.len
			|| a_clean[..a_bracket_end + 1] != b_clean[..b_bracket_end + 1] {
			return false
		}
		return tc.generic_type_arg_matches(a_clean[a_bracket_end + 1..],
			b_clean[b_bracket_end + 1..])
	}
	a_base, a_args, a_ok := generic_type_application_parts(a_clean)
	b_base, b_args, b_ok := generic_type_application_parts(b_clean)
	if a_ok || b_ok {
		if !a_ok || !b_ok || a_args.len != b_args.len {
			return false
		}
		if !tc.generic_type_base_matches(a_base, b_base) {
			return false
		}
		for i in 0 .. a_args.len {
			if !tc.generic_type_arg_matches(a_args[i], b_args[i]) {
				return false
			}
		}
		return true
	}
	a_resolved := tc.resolve_generic_match_arg(a_clean)
	b_resolved := tc.resolve_generic_match_arg(b_clean)
	if a_resolved == b_resolved {
		return true
	}
	return false
}

fn (tc &TypeChecker) generic_match_arg_is_open_param(arg string) bool {
	return is_bare_generic_param(arg) && !tc.is_known_type_text(arg)
}

fn (tc &TypeChecker) resolve_generic_match_arg(arg string) string {
	if arg.len == 0 {
		return arg
	}
	if !arg.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(arg) {
			return resolved
		}
	}
	qarg := tc.qualify_type_text(arg)
	if qarg.len > 0 {
		return qarg
	}
	return arg
}

fn (tc &TypeChecker) bare_variant_name_matches(candidate string, declared string, concrete string) bool {
	if generic_type_application(candidate) {
		return false
	}
	candidate_base := trimmed_space(candidate)
	declared_base := strip_generic_args_name(declared).trim_space()
	concrete_base := strip_generic_args_name(concrete).trim_space()
	candidate_resolved := tc.resolve_generic_match_base(candidate_base)
	declared_resolved := tc.resolve_generic_match_base(declared_base)
	concrete_resolved := tc.resolve_generic_match_base(concrete_base)
	if candidate_resolved == declared_resolved || candidate_resolved == concrete_resolved {
		return true
	}
	if candidate_base.contains('.') || candidate_resolved.contains('.') {
		return false
	}
	return (!declared_base.contains('.') && candidate_base == short_type_name(declared_base))
		|| (!concrete_base.contains('.') && candidate_base == short_type_name(concrete_base))
}

// type_matches_sum returns type matches sum data for TypeChecker.
fn (tc &TypeChecker) type_matches_sum(actual Type, expected Type) bool {
	if expected is SumType {
		actual_name := tc.type_name(actual)
		if tc.sum_variant_type_for_pattern(expected.name, actual_name) != none {
			return true
		}
		base := tc.sum_base_name(expected.name)
		variants := tc.sum_types[base] or { return false }
		for variant in variants {
			concrete := tc.concrete_sum_variant_name(expected.name, variant)
			variant_type := tc.parse_type(concrete)
			if tc.type_name(variant_type) == expected.name {
				continue
			}
			if tc.type_compatible(actual, variant_type) {
				return true
			}
		}
	}
	return false
}

// sum_has_variant converts sum has variant data for types.
fn (tc &TypeChecker) sum_has_variant(sum_name string, variant_name string) bool {
	return tc.sum_variant_type_for_pattern(sum_name, variant_name) != none
}

pub fn (tc &TypeChecker) sum_variant_type_for_pattern(sum_name string, variant_name string) ?string {
	if isnil(tc.type_cache) {
		return tc.sum_variant_type_for_pattern_depth(sum_name, variant_name, 0)
	}
	mut cache := tc.type_cache
	key := '${tc.cur_file}\x01${tc.cur_module}\x01${sum_name}\x01${variant_name}'
	if cached := cache.sum_variant_pattern_entries[key] {
		if cached.len == 0 {
			return none
		}
		return cached
	}
	result := tc.sum_variant_type_for_pattern_depth(sum_name, variant_name, 0) or {
		cache.sum_variant_pattern_entries[key] = ''
		return none
	}
	cache.sum_variant_pattern_entries[key] = result
	return result
}

fn (tc &TypeChecker) sum_variant_type_for_pattern_depth(sum_name string, variant_name string, depth int) ?string {
	if depth >= 16 {
		return none
	}
	base := tc.sum_base_name(sum_name)
	variants := tc.sum_types[base] or { return none }
	mut candidates := [variant_name]
	qvariant := tc.qualify_name(variant_name)
	if qvariant != variant_name {
		candidates << qvariant
	}
	if variant_name.contains('.') {
		resolved := tc.resolve_imported_type_text(variant_name)
		if resolved != variant_name {
			candidates << resolved
		}
		// `all_after_last('.')` on a container pattern (`map[string]ast.Value`)
		// strips the container and would resolve to the sum type itself.
		if !variant_name.contains('[') && !variant_name.contains(']') {
			if unique := tc.unique_qualified_type_name(variant_name.all_after_last('.')) {
				candidates << unique
			}
		}
	}
	for candidate in candidates.clone() {
		candidates << tc.alias_target_type_names(candidate)
	}
	for candidate in candidates {
		if tc.generic_type_name_matches(candidate, sum_name)
			|| tc.generic_type_name_matches(candidate, base) {
			return sum_name
		}
	}
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(sum_name, variant)
		for candidate in candidates {
			if tc.generic_type_name_matches(candidate, concrete)
				|| tc.bare_variant_name_matches(candidate, variant, concrete) {
				return concrete
			}
			if is_bare_generic_param(concrete) && !tc.is_known_type_text(concrete) {
				return candidate
			}
		}
		if nested := tc.nested_sum_variant_type_for_pattern(concrete, candidates, depth) {
			return nested
		}
	}
	return none
}

fn (tc &TypeChecker) alias_target_type_names(name string) []string {
	mut result := []string{}
	mut cur := name
	for _ in 0 .. 16 {
		target := tc.alias_target_type_text(cur) or { break }
		typ := tc.parse_type(target)
		target_name := typ.name()
		if target_name.len == 0 || target_name == cur || target_name in result {
			break
		}
		result << target_name
		cur = target_name
	}
	return result
}

fn (tc &TypeChecker) alias_target_type_text(name string) ?string {
	if target := tc.type_aliases[name] {
		return target
	}
	qname := tc.qualify_name(name)
	if target := tc.type_aliases[qname] {
		return target
	}
	if name.contains('.') {
		resolved := tc.resolve_imported_type_text(name)
		if target := tc.type_aliases[resolved] {
			return target
		}
		if unique := tc.unique_qualified_type_name(name.all_after_last('.')) {
			if target := tc.type_aliases[unique] {
				return target
			}
		}
	}
	return none
}

fn (tc &TypeChecker) nested_sum_variant_type_for_pattern(concrete string, candidates []string, depth int) ?string {
	typ := tc.parse_type(concrete)
	nested_name := if typ is SumType {
		typ.name
	} else if typ is Alias && typ.base_type is SumType {
		(typ.base_type as SumType).name
	} else {
		''
	}
	if nested_name.len == 0 {
		return none
	}
	for candidate in candidates {
		if nested := tc.sum_variant_type_for_pattern_depth(nested_name, candidate, depth + 1) {
			return nested
		}
	}
	return none
}

fn (tc &TypeChecker) match_sum_variant_type(subject Type, pattern string) ?Type {
	if subject is SumType {
		variants := tc.sum_types[subject.name] or { return none }
		variant_short := short_type_name(pattern)
		for variant in variants {
			if variant == pattern || short_type_name(variant) == variant_short {
				return tc.parse_type(variant)
			}
		}
		qpattern := tc.qualify_name(pattern)
		if qpattern != pattern {
			for variant in variants {
				if variant == qpattern || short_type_name(variant) == variant_short {
					return tc.parse_type(variant)
				}
			}
		}
		return none
	}
	if tc.type_symbol_known(pattern) {
		return tc.parse_type(pattern)
	}
	if is_builtin_type_name(pattern) {
		return builtin_type_value(pattern)
	}
	return none
}

// match_type_pattern supports match type pattern handling for TypeChecker.
fn (tc &TypeChecker) match_type_pattern(node &flat.Node) ?string {
	if node.kind == .array_init && node.typ.len > 0 {
		return node.typ
	}
	if node.kind == .ident {
		if node.value.starts_with('?') || node.value.starts_with('!') {
			return node.value
		}
		if node.typ.starts_with('?') || node.typ.starts_with('!') {
			return node.typ
		}
		if node.value.len > 0 && node.pos.offset > 0 {
			if file := tc.a.source_files[node.pos.id] {
				if source := tc.source_texts_by_file[file.name] {
					if node.pos.offset < source.len && source[node.pos.offset] in [`?`, `!`] {
						return source[node.pos.offset..node.pos.offset + 1] + node.value
					}
					if node.pos.offset <= source.len && source[node.pos.offset - 1] in [`?`, `!`] {
						return source[node.pos.offset - 1..node.pos.offset] + node.value
					}
				}
			}
		}
		if node.value.starts_with('fn(') || node.value.starts_with('fn (') {
			return tc.qualify_type_text(node.value)
		}
		if is_builtin_type_name(node.value) || tc.type_symbol_known(node.value)
			|| tc.pattern_type_known(node.value)
			|| (node.value.len > 0 && node.value[0].is_capital()) {
			return node.value
		}
		return none
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(node, 0)
		if base.kind == .ident && !tc.ident_resolves_to_value(base.value) {
			pattern := '${base.value}.${node.value}'
			if (base.value != 'C' && node.value.len > 0 && node.value[0].is_capital())
				|| tc.type_symbol_known(pattern) || tc.pattern_type_known(pattern) {
				return pattern
			}
		}
	}
	return none
}

// short_type_name supports short type name handling for types.
fn short_type_name(name string) string {
	if name.contains('.') {
		return name.all_after_last('.')
	}
	return name
}

// type_mismatch returns type mismatch data for TypeChecker.
fn (mut tc TypeChecker) type_mismatch(kind TypeErrorKind, msg string, node flat.NodeId) {
	if tc.should_diagnose(node) {
		tc.record_error(kind, msg, node)
	}
}

// expr_key supports expr key handling for TypeChecker.
fn (tc &TypeChecker) expr_key(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	if node.kind in [.as_expr, .paren] && node.children_count > 0 {
		return tc.expr_key(tc.a.child(&node, 0))
	}
	if node.kind == .ident {
		if valid_string_data(node.value) {
			return node.value
		}
		return ''
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.expr_key(tc.a.child(&node, 0))
		if base.len > 0 && node.value.len > 0 && valid_string_data(node.value) {
			return '${base}.${node.value}'
		}
	}
	if node.kind == .index && node.children_count >= 2 {
		base := tc.expr_key(tc.a.child(&node, 0))
		index_key := tc.expr_key_part(tc.a.child(&node, 1))
		if base.len > 0 && index_key.len > 0 {
			return '${base}[${index_key}]'
		}
	}
	return ''
}

fn (tc &TypeChecker) expr_key_part(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.int_literal, .string_literal, .char_literal, .enum_val {
			return node.value
		}
		else {
			return tc.expr_key(id)
		}
	}
}

// smartcast_type supports smartcast type handling for TypeChecker.
@[direct_array_access]
fn (tc &TypeChecker) smartcast_type(id flat.NodeId) ?Type {
	idx := int(id)
	if idx < 0 || idx >= tc.a.nodes.len || tc.a.nodes[idx].kind !in [.ident, .selector, .index] {
		return none
	}
	mut cache := tc.type_cache
	// Most checker contexts have no active dynamic smartcast. Consult the
	// node-indexed lexical cache first so repeated type queries do not rebuild an
	// expression key merely to rediscover the same lexical hit or miss.
	if tc.smartcasts.len == 0 && idx >= 0 && idx < tc.lexical_smartcast_misses.len
		&& tc.lexical_smartcast_misses[idx] {
		return none
	}
	if tc.smartcasts.len == 0 && idx >= 0 && idx < tc.direct_parent_ids.len && !isnil(cache) {
		if typ := cache.lexical_smartcast_entries[idx] {
			return typ
		}
		if cache.lexical_smartcast_misses[idx] {
			return none
		}
	}
	key := tc.expr_key(id)
	if key.len == 0 {
		return none
	}
	if !valid_string_data(key) {
		return none
	}
	if typ := tc.smartcasts[key] {
		return typ
	}
	// Lexical smartcasts belong to parsed source nodes. Transform-created nodes
	// are appended after the parent index was built and carry explicit/synthetic
	// types; walking the full arena to rediscover a parent for each such lookup
	// makes self-host transformation quadratic.
	if idx < 0 || idx >= tc.direct_parent_ids.len {
		return none
	}
	if idx < tc.lexical_smartcast_misses.len && tc.lexical_smartcast_misses[idx] {
		return none
	}
	if !isnil(cache) {
		if typ := cache.lexical_smartcast_entries[idx] {
			return typ
		}
		if cache.lexical_smartcast_misses[idx] {
			return none
		}
	}
	result := tc.lexical_smartcast_type(id, key) or {
		if !tc.resolution_type_mode && idx < tc.lexical_smartcast_misses.len
			&& (!tc.parallel_check_sparse || (idx >= tc.check_range_lo && idx <= tc.check_range_hi)) {
			mut writable := unsafe { tc }
			writable.lexical_smartcast_misses[idx] = true
		}
		if !isnil(cache) {
			cache.lexical_smartcast_misses[idx] = true
		}
		return none
	}
	if !isnil(cache) {
		cache.lexical_smartcast_entries[idx] = result
	}
	return result
}

struct LexicalSmartcastCandidate {
	typ   Type
	depth int
}

fn (tc &TypeChecker) lexical_smartcast_type(id flat.NodeId, key string) ?Type {
	mut best := LexicalSmartcastCandidate{
		depth: max_int
	}
	mut found := false
	if candidate := tc.lexical_if_smartcast_candidate(id, key) {
		best = candidate
		found = true
	}
	if candidate := tc.lexical_for_smartcast_candidate(id, key) {
		if !found || candidate.depth < best.depth {
			best = candidate
			found = true
		}
	}
	if candidate := tc.lexical_match_smartcast_candidate(id, key) {
		if !found || candidate.depth < best.depth {
			best = candidate
			found = true
		}
	}
	if found {
		return best.typ
	}
	return none
}

fn (tc &TypeChecker) lexical_for_smartcast_candidate(id flat.NodeId, key string) ?LexicalSmartcastCandidate {
	mut current := id
	mut parent_id := tc.direct_parent_id(current)
	mut depth := 1
	for tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .for_stmt && parent.children_count > 3 {
			mut body_index := -1
			for i in 3 .. parent.children_count {
				if tc.a.child(parent, i) == current {
					body_index = i
					break
				}
			}
			// The loop condition re-narrows `key` at the top of every iteration, but a
			// write to `key` earlier in the same iteration drops that narrowing for the
			// rest of the body (the dynamic pass deletes the smartcast on assignment).
			// Only reconstruct the condition smartcast when no preceding body statement,
			// nor the statement holding `id`, writes `key`.
			if body_index >= 3 && !tc.for_body_writes_key_before(parent, body_index, current, key) {
				for binding in tc.extract_smartcasts(tc.a.child(parent, 1)) {
					if binding.name == key {
						return LexicalSmartcastCandidate{
							typ:   binding.typ
							depth: depth
						}
					}
				}
			}
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			break
		}
		current = parent_id
		parent_id = tc.direct_parent_id(current)
		depth++
	}
	return none
}

// for_body_writes_key_before reports whether any loop-body statement up to and
// including `current` (the statement holding the checked node) assigns `key`.
// Such a write invalidates the loop-condition narrowing for the rest of that
// iteration, so the lexical fallback must not restore it.
fn (tc &TypeChecker) for_body_writes_key_before(parent flat.Node, body_index int, current flat.NodeId, key string) bool {
	for i in 3 .. body_index {
		if tc.subtree_assigns_key(tc.a.child(parent, i), key) {
			return true
		}
	}
	return tc.subtree_assigns_key(current, key)
}

// branch_writes_key_before reports whether an earlier statement in a branch
// invalidates `key` before `target` is evaluated.
fn (tc &TypeChecker) branch_writes_key_before(branch_id flat.NodeId, target flat.NodeId, key string) bool {
	if !tc.valid_node_id(branch_id) || !tc.valid_node_id(target) {
		return false
	}
	branch := tc.a.node(branch_id)
	if branch.kind != .block {
		return false
	}
	mut direct_child := target
	for _ in 0 .. 128 {
		parent_id := tc.direct_parent_id(direct_child)
		if parent_id == branch_id {
			break
		}
		if !tc.valid_node_id(parent_id) {
			return false
		}
		direct_child = parent_id
	}
	for i in 0 .. branch.children_count {
		child_id := tc.a.child(branch, i)
		if child_id == direct_child {
			return false
		}
		if tc.subtree_assigns_key(child_id, key) {
			return true
		}
	}
	return false
}

// write_key_invalidates_key reports whether assigning `write_key` invalidates the
// narrowing tracked for `key`. Besides an exact match, reassigning an ancestor
// (`holder` or `items[0]`) replaces the storage a narrowed descendant
// (`holder.value`) reads, so its runtime tag no longer holds — mirroring how
// invalidate_smartcasts_for_write_key drops descendant smartcasts.
fn write_key_invalidates_key(write_key string, key string) bool {
	if write_key.len == 0 {
		return false
	}
	if write_key == key {
		return true
	}
	return key.len > write_key.len && key.starts_with(write_key)
		&& (key[write_key.len] == `.` || key[write_key.len] == `[`)
}

// canonical_storage_alias_key rewrites the base identifier of `key` to its scope
// storage key, following a single pointer-binding alias (`p := &holder` records
// `p -> holder` in pointer_binding_value_keys). A write to `holder.value` and a
// narrowed `p.value` then resolve to the same canonical key. Returns `key`
// unchanged when the base cannot be resolved to a storage key.
fn (tc &TypeChecker) canonical_storage_alias_key(key string) string {
	if key.len == 0 {
		return key
	}
	mut base_len := key.len
	for i in 0 .. key.len {
		if key[i] == `.` || key[i] == `[` {
			base_len = i
			break
		}
	}
	base := key[..base_len]
	owner := tc.cur_scope.lookup_owner(base) or { return key }
	storage := owner.storage_key()
	if storage.len == 0 {
		return key
	}
	suffix := key[base_len..]
	if values := tc.fn_context.pointer_binding_value_keys[storage] {
		// A single unambiguous, resolvable pointee (not an `@unknown/@parameter/@global`
		// marker) means the base is a pure alias of that storage.
		if values.len == 1 && values[0].len > 0 && !values[0].starts_with('@') {
			return values[0] + suffix
		}
		return key
	}
	return storage + suffix
}

// write_invalidates_key extends write_key_invalidates_key with pointer-alias
// provenance: a write to `holder.value` also invalidates a narrowed `p.value` when
// `p` aliases `holder` (recorded in pointer_binding_value_keys).
fn (tc &TypeChecker) write_invalidates_key(write_key string, key string) bool {
	if write_key_invalidates_key(write_key, key) {
		return true
	}
	if tc.fn_context.pointer_binding_value_keys.len == 0 {
		return false
	}
	canon_write := tc.canonical_storage_alias_key(write_key)
	canon_key := tc.canonical_storage_alias_key(key)
	return (canon_write != write_key || canon_key != key)
		&& write_key_invalidates_key(canon_write, canon_key)
}

// subtree_assigns_key reports whether `root` contains an assignment, short
// declaration, or a call passing `key` to a `mut` parameter whose target
// resolves to `key` (or an ancestor of it). A mutating call is treated as a write
// because the callee can reassign the argument (for example `replace(mut x)`
// swapping a sum's active variant), which changes the runtime tag just like a
// direct assignment.
fn (tc &TypeChecker) subtree_assigns_key(root flat.NodeId, key string) bool {
	if !tc.valid_node_id(root) {
		return false
	}
	node := tc.a.node(root)
	if node.kind == .assign {
		mut i := 0
		for i + 1 < node.children_count {
			if tc.write_invalidates_key(tc.expr_key(tc.a.child(node, i)), key) {
				return true
			}
			i += 2
		}
	} else if node.kind == .decl_assign {
		// decl_assign children are `lhs0, rhs, lhs1, lhs2, ...` for multi-return
		// targets; a redeclaration of `key` (or an ancestor of it) also shadows the
		// narrowed binding.
		if node.children_count > 0
			&& tc.write_invalidates_key(tc.expr_key(tc.a.child(node, 0)), key) {
			return true
		}
		for i in 2 .. node.children_count {
			if tc.write_invalidates_key(tc.expr_key(tc.a.child(node, i)), key) {
				return true
			}
		}
	} else if node.kind == .call {
		// Child 0 is the callee; the arguments follow. A `mut` argument lets the callee
		// overwrite the passed lvalue (for example `replace(mut x)` swapping a sum's
		// active variant), which changes the runtime tag just like a direct assignment.
		// Passing `mut holder` likewise writes a narrowed descendant `holder.value`, so
		// use the ancestor-aware relationship, not an exact-key match.
		for i in 1 .. node.children_count {
			if arg_key := tc.expr_mut_arg_key(tc.a.child(node, i)) {
				if tc.write_invalidates_key(arg_key, key) {
					return true
				}
			}
		}
		// A `mut` receiver is stored under the selector callee rather than among the
		// arguments. Match the receiver on its full expression key so a nested
		// `holder.value.replace()` counts as a write to `holder.value`, and an ancestor
		// receiver `holder.reset()` counts as a write to a narrowed `holder.value`.
		// Resolve the receiver's declared type with scope/field lookups only — this scan
		// must stay free of smartcast resolution, which would recurse back through here.
		if node.children_count > 0 {
			callee := tc.a.node(tc.a.child(node, 0))
			if callee.kind == .selector && callee.children_count > 0 {
				recv_id := tc.a.child(callee, 0)
				recv_key := tc.expr_key(recv_id)
				if tc.write_invalidates_key(recv_key, key) {
					if declared := tc.declared_receiver_expr_type(recv_id) {
						// An exact-key sum receiver can re-tag itself; an ancestor receiver
						// only needs a `mut` receiver to reassign the narrowed descendant.
						has_write := if recv_key == key {
							tc.sum_type_has_mut_receiver_method(declared, callee.value)
						} else {
							tc.type_has_mut_receiver_method(declared, callee.value)
						}
						if has_write {
							return true
						}
					}
				}
			}
		}
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(node, i)
		// Assignments and mutating calls always have children. Avoid recursing into
		// the much more numerous ident/literal leaves, which cannot invalidate a
		// smartcast on their own.
		if tc.a.node(child_id).children_count > 0 && tc.subtree_assigns_key(child_id, key) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) lexical_if_smartcast_candidate(id flat.NodeId, key string) ?LexicalSmartcastCandidate {
	idx := int(id)
	if idx < 0 || idx >= tc.direct_parent_ids.len {
		return none
	}
	mut current := id
	mut parent_id := tc.direct_parent_id(current)
	mut depth := 1
	for tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind == .if_expr && parent.children_count >= 2 {
			mut branch_index := -1
			for i in 1 .. parent.children_count {
				if tc.a.child(parent, i) == current {
					branch_index = i
					break
				}
			}
			if branch_index >= 1 {
				cond_id := tc.a.child(parent, 0)
				bindings := if branch_index == 1 {
					tc.extract_smartcasts(cond_id)
				} else {
					tc.extract_else_branch_smartcasts(cond_id)
				}
				for binding in bindings {
					branch_id := tc.a.child(parent, branch_index)
					if binding.name == key && !tc.branch_writes_key_before(branch_id, id, key) {
						return LexicalSmartcastCandidate{
							typ:   binding.typ
							depth: depth
						}
					}
				}
			}
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			break
		}
		current = parent_id
		parent_id = tc.direct_parent_id(current)
		depth++
	}
	return none
}

fn (tc &TypeChecker) lexical_match_smartcast_type(id flat.NodeId) ?Type {
	if candidate := tc.lexical_match_smartcast_candidate(id, tc.expr_key(id)) {
		return candidate.typ
	}
	return none
}

fn (tc &TypeChecker) lexical_match_smartcast_candidate(id flat.NodeId, key string) ?LexicalSmartcastCandidate {
	idx := int(id)
	if idx < 0 || idx >= tc.direct_parent_ids.len {
		return none
	}
	if key.len == 0 || !valid_string_data(key) {
		return none
	}
	mut current := id
	mut branch_id := flat.NodeId(-1)
	mut depth := 0
	for _ in 0 .. 64 {
		depth++
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return none
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .match_branch {
			branch_id = parent_id
		} else if parent.kind == .match_stmt && tc.valid_node_id(branch_id) {
			branch := tc.a.node(branch_id)
			if typ := tc.lexical_match_branch_smartcast_type(parent, branch, key) {
				return LexicalSmartcastCandidate{
					typ:   typ
					depth: depth
				}
			}
			// An unrelated nested match does not cancel a narrowing established by
			// an enclosing match on the same expression.
			branch_id = flat.empty_node
		} else if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return none
		}
		current = parent_id
	}
	return none
}

fn (tc &TypeChecker) lexical_match_branch_smartcast_type(parent &flat.Node, branch &flat.Node, key string) ?Type {
	if branch.value == 'else' || branch.value.int() != 1 || branch.children_count == 0
		|| parent.children_count == 0 {
		return none
	}
	subject_id := tc.a.child(parent, 0)
	if tc.expr_key(subject_id) != key {
		return none
	}
	subject_type := unalias_and_unwrap_pointer_type(tc.resolve_type(subject_id))
	cond := tc.a.child_node(branch, 0)
	pattern := tc.match_type_pattern(cond) or { return none }
	name := if subject_type is SumType {
		tc.sum_variant_type_for_pattern(subject_type.name, pattern) or { return none }
	} else if is_ierror_type(subject_type) {
		tc.resolve_ierror_match_pattern(pattern) or { return none }
	} else if subject_type is Interface {
		tc.resolve_interface_match_pattern(pattern) or { return none }
	} else if short_type_name(subject_type.name()) == short_type_name(pattern) {
		// During the branch check, the dynamic smartcast already exposes the
		// matched variant. Keep recognizing the lexical match so declarations
		// inside nested sum matches infer that concrete variant.
		subject_type.name()
	} else {
		return none
	}
	return tc.parse_type(name)
}

fn (tc &TypeChecker) smartcast_target_type_for_is_expr(expr_id flat.NodeId, pattern string) Type {
	raw_subject := fn_param_unalias_type(tc.resolve_type(expr_id))
	subject := unalias_and_unwrap_pointer_type(raw_subject)
	if subject is SumType {
		if variant := tc.sum_variant_type_for_pattern(subject.name, pattern) {
			return tc.parse_type(variant)
		}
	}
	if is_ierror_type(subject) {
		if variant := tc.resolve_ierror_match_pattern(pattern) {
			return tc.parse_type(variant)
		}
	}
	if subject is Interface {
		if variant := tc.resolve_interface_match_pattern(pattern) {
			variant_type := tc.parse_type(variant)
			if tc.expr_has_explicit_mut_marker(expr_id) && raw_subject is Pointer {
				return Type(Pointer{
					base_type: variant_type
				})
			}
			return variant_type
		}
	}
	return tc.parse_type(pattern)
}

fn (mut tc TypeChecker) invalidate_smartcasts_for_write_key(key string) {
	if key.len == 0 {
		return
	}
	tc.smartcasts.delete(key)
	prefix := '${key}.'
	for child_key in tc.smartcasts.keys() {
		if child_key.starts_with(prefix) {
			tc.smartcasts.delete(child_key)
		}
	}
}

// cached_c_name memoizes naming.c_name results in the type cache (falling
// back to the frozen base cache read-only, like every other entry kind).
// c_name is pure and called on hot resolution paths in every phase.
pub fn (tc &TypeChecker) cached_c_name(name string) string {
	if isnil(tc.type_cache) {
		return naming.c_name(name)
	}
	mut cache := unsafe { tc.type_cache }
	mut fallback := cache.base
	for !isnil(fallback) {
		if cached := fallback.c_name_entries[name] {
			return cached
		}
		fallback = fallback.base
	}
	if cached := cache.c_name_entries[name] {
		return cached
	}
	result := naming.c_name(name)
	cache.c_name_entries[name] = result
	return result
}

// parse_type converts a V type string (from parser) to a structured Type.
// type_text_contains_typeof is a fast substring probe for `typeof(`: type
// texts are overwhelmingly short scalar/container names, so the length
// early-out plus a first-byte scan beats string.contains (KMP table build and
// call overhead) on hot paths that must screen every text.
@[direct_array_access]
pub fn type_text_contains_typeof(s string) bool {
	if s.len < 8 {
		return false
	}
	for i := 0; i + 7 <= s.len; i++ {
		if s[i] == `t` && s[i + 1] == `y` && s[i + 2] == `p` && s[i + 3] == `e` && s[i + 4] == `o`
			&& s[i + 5] == `f` && s[i + 6] == `(` {
			return true
		}
	}
	return false
}

pub fn (tc &TypeChecker) parse_type(typ string) Type {
	// Do this before the memoization lookup. The outer alias expansion is not
	// cached until its complete semantic type exists; caching this symbolic
	// recursive edge would otherwise leave the shallow placeholder as the
	// permanent result for the alias.
	if !isnil(tc.type_cache) && tc.type_cache.alias_parse_stack.len > 0 {
		if recursive_alias := tc.recursive_alias_reference(typ) {
			return recursive_alias
		}
	}
	if tc.type_cache != unsafe { nil } && tc.type_cache.parse_enabled {
		mut cache := unsafe { tc.type_cache }
		if cached := parse_type_cache_get_mode(mut cache, tc.cur_file, tc.cur_module, typ,
			tc.fn_context.generic_params, tc.resolution_type_mode, tc.fast_parse_recent)
		{
			cache.parse_hits++
			return cached
		}
		// typeof(...) resolves against expression context, so its result must
		// never enter the cache. Checking only on the miss path keeps the
		// substring scan off the ~2M cache hits (typeof text can never hit:
		// it is never put).
		if type_text_contains_typeof(typ) {
			return tc.parse_type_uncached(typ)
		}
		cache.parse_misses++
		_, result := tc.intern_type(tc.parse_type_uncached(typ))
		if tc.fast_parse_recent {
			parse_type_cache_put_recent(mut cache, tc.cur_file, tc.cur_module, typ,
				tc.fn_context.generic_params, tc.resolution_type_mode, result)
		} else {
			parse_type_cache_put(mut cache, tc.cur_file, tc.cur_module, typ,
				tc.fn_context.generic_params, tc.resolution_type_mode, result)
		}
		return result
	}
	if type_text_contains_typeof(typ) {
		return tc.parse_type_uncached(typ)
	}
	_, result := tc.intern_type(tc.parse_type_uncached(typ))
	return result
}

// parse_type_ref resolves a node annotation through its exact canonical text
// identity. Context remains part of the key because an unqualified spelling
// can denote different types in different files/modules.
pub fn (tc &TypeChecker) parse_type_ref(typ string, text_id u16) Type {
	if text_id == 0 || !tc.fast_type_text_refs || tc.type_cache == unsafe { nil }
		|| !tc.type_cache.parse_enabled {
		return tc.parse_type(typ)
	}
	// Recursive aliases must observe the active expansion chain before any
	// memoized value, matching parse_type's ordering.
	if tc.type_cache.alias_parse_stack.len > 0 {
		if recursive_alias := tc.recursive_alias_reference(typ) {
			return recursive_alias
		}
	}
	mut cache := unsafe { tc.type_cache }
	if cache.parse_text_id_set.len == 0 {
		cache.parse_text_id_context = []u64{len: 65536}
		cache.parse_text_id_values = unsafe { []Type{len: 65536} }
		cache.parse_text_id_set = []bool{len: 65536}
	}
	slot := int(text_id)
	context_hash := parse_type_cache_context_hash(mut cache, tc.cur_file, tc.cur_module,
		tc.fn_context.generic_params, tc.resolution_type_mode)
	if cache.parse_text_id_set[slot] && cache.parse_text_id_context[slot] == context_hash {
		cache.parse_hits++
		return cache.parse_text_id_values[slot]
	}
	result := tc.parse_type(typ)
	cache.parse_text_id_context[slot] = context_hash
	cache.parse_text_id_values[slot] = result
	cache.parse_text_id_set[slot] = true
	return result
}

fn (tc &TypeChecker) recursive_alias_reference(typ string) ?Type {
	if isnil(tc.type_cache) || tc.type_cache.alias_parse_stack.len == 0 {
		return none
	}
	clean := typ.trim_space()
	if clean.len == 0 || clean[0] in [`&`, `?`, `!`, `[`, `(`] || clean.starts_with('mut ')
		|| clean.starts_with('shared ') || clean.starts_with('atomic ')
		|| clean.starts_with('chan ') || clean.starts_with('map[') || clean.starts_with('fn(') {
		return none
	}
	mut qualified := clean
	if !clean.contains('.') {
		qualified = tc.qualify_name(clean)
	}
	for i := tc.type_cache.alias_parse_stack.len - 1; i >= 0; i-- {
		name := tc.type_cache.alias_parse_stack[i]
		if clean == name || qualified == name || clean == name.all_after_last('.') {
			return Type(Alias{
				name:      name
				base_type: Type(Unknown{
					reason: 'recursive alias `${name}`'
				})
			})
		}
	}
	return none
}

fn (tc &TypeChecker) parse_alias_type(name string, target string) Type {
	mut cache := unsafe { tc.type_cache }
	if isnil(cache) {
		return Type(Alias{
			name:      name
			base_type: tc.parse_alias_target_type(name, target)
		})
	}
	cache.alias_parse_stack << name
	base_type := tc.parse_alias_target_type(name, target)
	cache.alias_parse_stack.delete_last()
	return Type(Alias{
		name:      name
		base_type: base_type
	})
}

fn (tc &TypeChecker) parse_alias_target_type(name string, target string) Type {
	clean := trimmed_space(target)
	if clean.starts_with('shared ') {
		return Type(Pointer{
			base_type: tc.parse_alias_target_type(name, trimmed_space(clean[7..]))
		})
	}
	decl_module := tc.type_alias_modules[name] or {
		tc.type_alias_modules[name.all_before('[')] or { tc.cur_module }
	}
	if decl_module != tc.cur_module {
		mut scoped := tc.fork_type_parse_view(tc.cur_file, decl_module)
		return scoped.parse_type(target)
	}
	return tc.parse_type(target)
}

// parse_canonical_type parses compiler-produced type text while preserving an
// exact known qualified symbol before consulting the current file's import
// aliases. Source text must continue to use parse_type, where aliases take
// precedence; this entry point is for semantic names carried between phases.
pub fn (tc &TypeChecker) parse_canonical_type(typ string) Type {
	clean := trimmed_space(typ)
	if clean.starts_with('&') {
		_, result := tc.intern_type(Type(Pointer{
			base_type: tc.parse_canonical_type(clean[1..])
		}))
		return result
	}
	if clean.starts_with('mut ') {
		_, result := tc.intern_type(Type(Pointer{
			base_type: tc.parse_canonical_type(clean[4..])
		}))
		return result
	}
	if clean.starts_with('shared ') {
		return tc.parse_canonical_type(clean[7..])
	}
	if clean.starts_with('...') {
		_, result := tc.intern_type(Type(Array{
			elem_type: tc.parse_canonical_type(clean[3..])
		}))
		return result
	}
	if clean.starts_with('[]') {
		_, result := tc.intern_type(Type(Array{
			elem_type: tc.parse_canonical_type(clean[2..])
		}))
		return result
	}
	if clean.starts_with('?') {
		base_type := tc.parse_canonical_type(clean[1..])
		if base_type is Alias && unalias_type(base_type) is OptionType {
			return base_type
		}
		_, result := tc.intern_type(Type(OptionType{
			base_type: base_type
		}))
		return result
	}
	if clean.starts_with('!') {
		_, result := tc.intern_type(Type(ResultType{
			base_type: tc.parse_canonical_type(clean[1..])
		}))
		return result
	}
	if known := tc.type_from_known_symbol(clean) {
		_, result := tc.intern_type(known)
		return result
	}
	return tc.parse_type(clean)
}

fn (tc &TypeChecker) probe_intern_type(t Type) ?Type {
	if isnil(tc.type_interner) {
		return none
	}
	return tc.type_interner.probe(t)
}

fn (tc &TypeChecker) intern_type(t Type) (TypeId, Type) {
	if isnil(tc.type_interner) {
		// Only hand-built compatibility checkers can reach this path. Production
		// checkers are created with one compilation-wide interner.
		return TypeId(0), t
	}
	mut interner := unsafe { tc.type_interner }
	return interner.canonicalize(t)
}

// type_count reports the number of unique canonical semantic types observed by
// this compilation.
pub fn (tc &TypeChecker) type_count() int {
	if isnil(tc.type_interner) {
		return 0
	}
	mut interner := unsafe { tc.type_interner }
	return interner.len()
}

// type_name lazily formats and memoizes the canonical spelling of a semantic
// type. Hot compiler paths should prefer this to repeated recursive Type.name
// construction.
pub fn (tc &TypeChecker) type_name(t Type) string {
	// Stored-name variants are free; interning them would only add hashing on
	// top of returning the field.
	if t is Struct {
		return t.name
	}
	if t is Alias {
		return t.name
	}
	if t is Enum {
		return t.name
	}
	if t is SumType {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if isnil(tc.type_interner) {
		return t.name()
	}
	mut cache := unsafe { tc.type_cache }
	if !isnil(cache) {
		hash, slot := type_recent_hash_slot(t)
		if cache.name_recent_set[slot] && cache.name_recent_hashes[slot] == hash
			&& semantic_types_equal(cache.name_recent_types[slot], t) {
			return cache.name_recent_vals[slot]
		}
		id, _ := tc.intern_type(t)
		mut interner := unsafe { tc.type_interner }
		result := interner.name(id)
		cache.name_recent_hashes[slot] = hash
		cache.name_recent_types[slot] = clone_owned_type(t)
		cache.name_recent_vals[slot] = result
		cache.name_recent_set[slot] = true
		return result
	}
	id, _ := tc.intern_type(t)
	mut interner := unsafe { tc.type_interner }
	return interner.name(id)
}

// type_recent_hash_slot chooses a recent-cache slot from semantic identity.
@[inline]
fn type_recent_hash_slot(typ Type) (u64, int) {
	hash := semantic_type_hash(typ)
	return hash, int(hash & 2047)
}

// type_value_words exposes the transient Type representation for immediate
// equality checks. Do not retain these words as a cache key: payload addresses
// can be reused after the compared values leave scope.
@[inline]
fn type_value_words(typ &Type) (u64, u64, int) {
	words := unsafe { &u64(voidptr(typ)) }
	w0 := unsafe { words[0] }
	w1 := unsafe { words[1] }
	return w0, w1, int(((w0 >> 4) ^ w1) & 2047)
}

// type_cache_stats returns cache counters accumulated by this checker.
pub fn (tc &TypeChecker) type_cache_stats() TypeCacheStats {
	if isnil(tc.type_cache) {
		return TypeCacheStats{}
	}
	return TypeCacheStats{
		parse_hits:   tc.type_cache.parse_hits
		parse_misses: tc.type_cache.parse_misses
		c_hits:       tc.type_cache.c_hits
		c_misses:     tc.type_cache.c_misses
	}
}

fn parse_type_cache_context_hash(mut cache TypeCache, file string, module_name string, generic_params []string, resolution bool) u64 {
	if cache.parse_context_valid && cache.parse_context_resolution == resolution
		&& parse_type_cache_string_matches(cache.parse_context_file, file)
		&& parse_type_cache_string_matches(cache.parse_context_module, module_name)
		&& parse_type_cache_strings_match(cache.parse_context_generics, generic_params) {
		return cache.parse_context_hash
	}
	mut hash := u64(14_695_981_039_346_656_037)
	if resolution {
		hash = (hash ^ u64(1)) * u64(1_099_511_628_211)
	}
	hash = parse_type_cache_hash_part(hash, file)
	hash = parse_type_cache_hash_part(hash, module_name)
	hash = (hash ^ u64(generic_params.len)) * u64(1_099_511_628_211)
	for param in generic_params {
		hash = parse_type_cache_hash_part(hash, param)
	}
	cache.parse_context_file = file
	cache.parse_context_module = module_name
	cache.parse_context_generics = generic_params.clone()
	cache.parse_context_resolution = resolution
	cache.parse_context_hash = hash
	cache.parse_context_valid = true
	return hash
}

fn parse_type_cache_hash_part(initial u64, part string) u64 {
	mut hash := initial ^ u64(part.len)
	hash *= u64(1_099_511_628_211)
	for i in 0 .. part.len {
		hash ^= u64(part[i])
		hash *= u64(1_099_511_628_211)
	}
	return hash
}

fn parse_type_cache_next_key(key u64) u64 {
	return key * u64(2_862_933_555_777_941_757) + u64(3_037_000_493)
}

@[inline]
fn parse_type_cache_key(mut cache TypeCache, context_hash u64, text string) u64 {
	text_ptr := usize(voidptr(text.str))
	slot := int(((u64(text_ptr) >> 4) ^ u64(text.len) ^ context_hash) & 511)
	if cache.parse_key_recent_set[slot] && cache.parse_key_recent_ptrs[slot] == text_ptr
		&& cache.parse_key_recent_lens[slot] == text.len
		&& cache.parse_key_recent_context[slot] == context_hash {
		return cache.parse_key_recent_values[slot]
	}
	key := parse_type_cache_hash_part(context_hash, text)
	cache.parse_key_recent_ptrs[slot] = text_ptr
	cache.parse_key_recent_lens[slot] = text.len
	cache.parse_key_recent_context[slot] = context_hash
	cache.parse_key_recent_values[slot] = key
	cache.parse_key_recent_set[slot] = true
	return key
}

@[inline]
fn parse_type_cache_string_matches(a string, b string) bool {
	if a.len != b.len {
		return false
	}
	if unsafe { a.str == b.str } {
		return true
	}
	return a == b
}

fn parse_type_cache_strings_match(a []string, b []string) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		if !parse_type_cache_string_matches(a[i], b[i]) {
			return false
		}
	}
	return true
}

fn parse_type_cache_entry_matches(entry ParseTypeCacheEntry, file string, module_name string, text string, generic_params []string, resolution bool) bool {
	return entry.resolution == resolution && parse_type_cache_string_matches(entry.text, text)
		&& parse_type_cache_string_matches(entry.file, file)
		&& parse_type_cache_string_matches(entry.module, module_name)
		&& parse_type_cache_strings_match(entry.generic_params, generic_params)
}

fn parse_type_cache_get(mut cache TypeCache, file string, module_name string, text string, generic_params []string, resolution bool) ?Type {
	if cache.parse_last_valid
		&& parse_type_cache_entry_matches(cache.parse_last_entry, file, module_name, text, generic_params, resolution) {
		return cache.parse_last_entry.typ
	}
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params,
		resolution)
	mut key := parse_type_cache_key(mut cache, context_hash, text)
	for {
		if entry := cache.parse_entries[key] {
			if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params,
				resolution)
			{
				cache.parse_last_entry = entry
				cache.parse_last_valid = true
				return entry.typ
			}
			key = parse_type_cache_next_key(key)
			continue
		}
		mut fallback := cache.base
		mut fallback_collision := false
		for !isnil(fallback) {
			if entry := fallback.parse_entries[key] {
				if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params,
					resolution)
				{
					cache.parse_last_entry = entry
					cache.parse_last_valid = true
					return entry.typ
				}
				fallback_collision = true
				break
			}
			fallback = fallback.base
		}
		if fallback_collision {
			key = parse_type_cache_next_key(key)
			continue
		}
		return none
	}
	return none
}

@[inline]
fn parse_type_cache_get_mode(mut cache TypeCache, file string, module_name string, text string, generic_params []string, resolution bool, fast_recent bool) ?Type {
	if fast_recent {
		return parse_type_cache_get_recent(mut cache, file, module_name, text, generic_params,
			resolution)
	}
	return parse_type_cache_get(mut cache, file, module_name, text, generic_params, resolution)
}

@[inline]
fn parse_type_cache_value_recent_slot(text string, context_hash u64) int {
	return int(((u64(voidptr(text.str)) >> 4) ^ u64(text.len) ^ context_hash) & 2047)
}

fn parse_type_cache_get_recent(mut cache TypeCache, file string, module_name string, text string, generic_params []string, resolution bool) ?Type {
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params,
		resolution)
	text_ptr := usize(voidptr(text.str))
	slot := parse_type_cache_value_recent_slot(text, context_hash)
	if cache.parse_value_recent_set[slot] && cache.parse_value_recent_ptrs[slot] == text_ptr
		&& cache.parse_value_recent_lens[slot] == text.len
		&& cache.parse_value_recent_context[slot] == context_hash {
		return cache.parse_value_recent_values[slot]
	}
	result := parse_type_cache_get(mut cache, file, module_name, text, generic_params, resolution) or {
		return none
	}
	cache.parse_value_recent_ptrs[slot] = text_ptr
	cache.parse_value_recent_lens[slot] = text.len
	cache.parse_value_recent_context[slot] = context_hash
	cache.parse_value_recent_values[slot] = result
	cache.parse_value_recent_set[slot] = true
	return result
}

fn parse_type_cache_put(mut cache TypeCache, file string, module_name string, text string, generic_params []string, resolution bool, typ Type) {
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params,
		resolution)
	mut key := parse_type_cache_key(mut cache, context_hash, text)
	for {
		if entry := cache.parse_entries[key] {
			if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params,
				resolution)
			{
				return
			}
			key = parse_type_cache_next_key(key)
			continue
		}
		mut fallback := cache.base
		mut fallback_collision := false
		for !isnil(fallback) {
			if entry := fallback.parse_entries[key] {
				if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params,
					resolution)
				{
					return
				}
				fallback_collision = true
				break
			}
			fallback = fallback.base
		}
		if fallback_collision {
			key = parse_type_cache_next_key(key)
			continue
		}
		entry := ParseTypeCacheEntry{
			file:           file
			module:         module_name
			text:           text
			generic_params: generic_params.clone()
			resolution:     resolution
			typ:            typ
		}
		cache.parse_entries[key] = entry
		cache.parse_last_entry = entry
		cache.parse_last_valid = true
		return
	}
}

fn parse_type_cache_put_recent(mut cache TypeCache, file string, module_name string, text string, generic_params []string, resolution bool, typ Type) {
	parse_type_cache_put(mut cache, file, module_name, text, generic_params, resolution, typ)
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params,
		resolution)
	slot := parse_type_cache_value_recent_slot(text, context_hash)
	cache.parse_value_recent_ptrs[slot] = usize(voidptr(text.str))
	cache.parse_value_recent_lens[slot] = text.len
	cache.parse_value_recent_context[slot] = context_hash
	cache.parse_value_recent_values[slot] = typ
	cache.parse_value_recent_set[slot] = true
}

// parse_scope_param_type preserves open generic struct applications for local parameter
// lookup. The global parser deliberately collapses `Box[T]` to `Box` in signatures, but
// inside a generic function/method body the parameter still needs the open application so
// field lookup and generic receiver method resolution can substitute `T`.
fn (tc &TypeChecker) parse_scope_param_type(typ string) Type {
	clean := trimmed_space(typ)
	if clean.starts_with('...') {
		return Type(Array{
			elem_type: tc.parse_scope_param_type(clean[3..])
		})
	}
	if preserved := tc.parse_open_generic_struct_type(typ) {
		return preserved
	}
	return tc.parse_type(typ)
}

fn (tc &TypeChecker) parse_open_generic_struct_type(typ string) ?Type {
	clean := trimmed_space(typ)
	if clean.len == 0 {
		return none
	}
	if clean.starts_with('&') {
		base := tc.parse_open_generic_struct_type(clean[1..]) or { return none }
		return Type(Pointer{
			base_type: base
		})
	}
	if clean.starts_with('mut ') {
		base := tc.parse_open_generic_struct_type(clean[4..]) or { return none }
		return Type(Pointer{
			base_type: base
		})
	}
	if clean.starts_with('shared ') {
		return tc.parse_open_generic_struct_type(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return tc.parse_open_generic_struct_type(clean[7..])
	}
	if clean.starts_with('?') {
		base := tc.parse_open_generic_struct_type(clean[1..]) or { return none }
		return Type(OptionType{
			base_type: base
		})
	}
	if clean.starts_with('!') {
		base := tc.parse_open_generic_struct_type(clean[1..]) or { return none }
		return Type(ResultType{
			base_type: base
		})
	}
	if clean.starts_with('...') {
		elem := tc.parse_open_generic_struct_type(clean[3..]) or { return none }
		return Type(Array{
			elem_type: elem
		})
	}
	if clean.starts_with('[]') {
		elem := tc.parse_open_generic_struct_type(clean[2..]) or { return none }
		return Type(Array{
			elem_type: elem
		})
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := tc.parse_scope_param_type(clean[4..bracket_end])
			value := tc.parse_open_generic_struct_type(clean[bracket_end + 1..]) or { return none }
			return Type(Map{
				key_type:   key
				value_type: value
			})
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			elem := tc.parse_open_generic_struct_type(clean[bracket_end + 1..]) or { return none }
			len_text := trimmed_space(clean[1..bracket_end])
			return Type(ArrayFixed{
				elem_type: elem
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	base, args, ok := generic_type_application_parts(clean)
	if !ok || tc.generic_args_are_concrete(args) {
		return none
	}
	mut qbase := base
	if !base.contains('.') {
		resolved := tc.qualify_name(base)
		if resolved in tc.structs || resolved in tc.struct_generic_params {
			qbase = resolved
		}
	}
	if qbase !in tc.structs && qbase !in tc.struct_generic_params && base !in tc.structs
		&& base !in tc.struct_generic_params {
		return none
	}
	mut preserved_args := []string{cap: args.len}
	for arg in args {
		trimmed := trimmed_space(arg)
		preserved_args << if is_generic_placeholder_type(trimmed) {
			trimmed.all_after_last('.')
		} else {
			trimmed
		}
	}
	suffix := '[${preserved_args.join(', ')}]'
	return Type(Struct{
		name: qbase + suffix
	})
}

fn (tc &TypeChecker) parse_generic_alias_application(name string, args []string, suffix string) Type {
	target := tc.type_aliases[name]
	params := tc.type_alias_generic_params[name] or {
		tc.type_alias_generic_params[name.all_after_last('.')] or { []string{} }
	}
	if params.len == args.len && params.len > 0 {
		return Type(Alias{
			name:      name + suffix
			base_type: tc.parse_type(subst_generic_text(target, args, params))
		})
	}
	return Type(Alias{
		name:      name
		base_type: tc.parse_type(target)
	})
}

// parse_type_uncached reads parse type uncached input for types.
fn (tc &TypeChecker) parse_type_uncached(typ string) Type {
	if typ.len == 0 {
		return Type(void_)
	}
	if resolved := tc.type_from_typeof_type_text(typ) {
		return resolved
	}
	if trimmed_space(typ).starts_with('typeof(') {
		return unknown_type('unresolved typeof type `${typ}`')
	}
	// Preserve open generic struct applications wherever they occur in a signature or
	// expression type. Qualifying their placeholder (`T` -> `module.T`) makes it look
	// concrete to later stages and can emit invalid C types such as `Box_module__T`.
	if preserved := tc.parse_open_generic_struct_type(typ) {
		return preserved
	}
	if typ.ends_with('.typ') {
		return tc.parse_type(typ[..typ.len - 4])
	}
	if is_generic_placeholder_type(typ) && !tc.is_known_type_text(typ) {
		return unknown_type('generic placeholder `${typ}`')
	}
	// `main.Foo` is an explicit reference to a program-module type. It is used to
	// lock a bare concrete generic argument against being rebased into a callee
	// module that declares a same-named type (see explicit_generic_concrete_arg_text
	// / lock_colliding_main_generic_type_text). Resolve it directly to the bare-keyed
	// program symbol so the resulting type name matches the struct/method tables,
	// unless `main` is an actual import alias in this file.
	if typ.starts_with('main.') && !typ['main.'.len..].contains('.') {
		if _ := tc.resolve_import_alias('main') {
		} else {
			rest := typ['main.'.len..]
			if is_builtin_type_name(rest) {
				return builtin_type_value(rest)
			}
			if known := tc.type_from_known_symbol(rest) {
				return known
			}
			if generic_type_application(rest) {
				base, args, _ := generic_type_application_parts(rest)
				if !base.contains('.') {
					suffix := tc.qualified_generic_suffix(args)
					if base in tc.struct_generic_params || base in tc.structs {
						return Type(Struct{
							name: base + suffix
						})
					}
					if base in tc.sum_generic_params || base in tc.sum_types {
						return Type(SumType{
							name: base + suffix
						})
					}
					if base in tc.interface_names {
						return Type(Interface{
							name: base + suffix
						})
					}
					if base in tc.type_aliases {
						return tc.parse_generic_alias_application(base, args, suffix)
					}
				}
			}
		}
	}
	if typ.starts_with('&') {
		return Type(Pointer{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('mut ') {
		return Type(Pointer{
			base_type: tc.parse_type(typ[4..])
		})
	}
	if typ.starts_with('shared ') {
		return tc.parse_type(typ[7..])
	}
	if typ.starts_with('atomic ') {
		return tc.parse_type(typ[7..])
	}
	if typ.starts_with('?') {
		base_type := tc.parse_type(typ[1..])
		if base_type is Alias && unalias_type(base_type) is OptionType {
			return base_type
		}
		return Type(OptionType{
			base_type: base_type
		})
	}
	if typ.starts_with('!') {
		return Type(ResultType{
			base_type: tc.parse_type(typ[1..])
		})
	}
	if typ.starts_with('chan ') {
		elem_text := typ[5..]
		return Type(Channel{
			elem_type: tc.parse_type(elem_text)
			is_mut:    elem_text.starts_with('mut ')
		})
	}
	if typ == 'chan' {
		return Type(Channel{
			elem_type: Type(void_)
		})
	}
	if typ.starts_with('thread ') {
		// A thread handle. The element type (the spawned fn's return type) is kept
		// in the struct name (`thread T`) so `array_of_threads.wait()` can recover
		// `[]T`. Canonicalize concrete payload names just like spawn return types do,
		// while preserving unresolved generic placeholders for later substitution.
		payload_text := typ[7..]
		payload_type := tc.parse_type(payload_text)
		payload_name := if payload_type is Unknown { payload_text } else { payload_type.name() }
		return Type(Struct{
			name: 'thread ${payload_name}'
		})
	}
	if typ == 'thread' {
		// The handle itself lowers to `void*` in C (see c_type).
		return Type(Struct{
			name: typ
		})
	}
	if typ.starts_with('...') {
		return Type(Array{
			elem_type: tc.parse_type(typ[3..])
		})
	}
	if typ.starts_with('[]') {
		return Type(Array{
			elem_type: tc.parse_type(typ[2..])
		})
	}
	if typ.starts_with('map[') {
		bracket_end := find_matching_bracket(typ, 3)
		if bracket_end >= typ.len {
			return Type(Unknown{
				reason: 'malformed map type'
			})
		}
		key_str := typ[4..bracket_end]
		val_str := typ[bracket_end + 1..]
		return Type(Map{
			key_type:   tc.parse_type(key_str)
			value_type: tc.parse_type(val_str)
		})
	}
	if fixed_array_type_name_may_be(typ) && !typ.starts_with('[') {
		bracket := typ.last_index_u8(`[`)
		bracket_end := typ.last_index_u8(`]`)
		len_text := trimmed_space(typ[bracket + 1..bracket_end])
		base_text := typ[..bracket]
		if is_decimal_int_literal(len_text) || base_text.contains('[') {
			return Type(ArrayFixed{
				elem_type: tc.parse_type(base_text)
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	if typ.starts_with('[') {
		idx := typ.index_u8(`]`)
		if idx > 0 {
			len_text := trimmed_space(typ[1..idx])
			return Type(ArrayFixed{
				elem_type: tc.parse_type(typ[idx + 1..])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	if typ.starts_with('(') && typ.contains(',') {
		inner := typ[1..typ.len - 1]
		parts := split_params(inner)
		mut tuple_types := []Type{}
		for p in parts {
			tuple_types << tc.parse_type(trimmed_space(p))
		}
		return Type(MultiReturn{
			types: tuple_types
		})
	}
	if typ.starts_with('fn(') || typ.starts_with('fn (') {
		return tc.parse_fn_type(typ)
	}
	qtyp := if tc.resolution_type_mode {
		tc.qualify_resolution_type_name(typ)
	} else {
		tc.qualify_name(typ)
	}
	allow_bare_symbol := qtyp == typ
	if typ == 'array' && tc.has_builtins && typ in tc.structs {
		return Type(Struct{
			name: typ
		})
	}
	if typ == 'map' && tc.has_builtins {
		return Type(Struct{
			name: typ
		})
	}
	if typ == 'array' && tc.has_builtins {
		return Type(Struct{
			name: typ
		})
	}
	if is_builtin_type_name(typ) {
		return builtin_type_value(typ)
	}
	if typ == 'unknown' {
		return Type(Unknown{
			reason: 'unknown'
		})
	}
	if qtyp != typ {
		if scoped_type := tc.type_from_known_symbol(qtyp) {
			return scoped_type
		}
	}
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			if resolved_type := tc.type_from_known_symbol(resolved) {
				return resolved_type
			}
		}
	}
	if builtin_error_name := tc.resolve_unqualified_builtin_error_struct_name(typ) {
		return Type(Struct{
			name: builtin_error_name
		})
	}
	if typ.starts_with('C.') {
		if typ in tc.type_aliases {
			return tc.parse_alias_type(typ, tc.type_aliases[typ])
		}
		return Type(Struct{
			name: typ
		})
	}
	if qtyp in tc.type_aliases {
		return tc.parse_alias_type(qtyp, tc.type_aliases[qtyp])
	}
	if allow_bare_symbol && typ in tc.type_aliases {
		return tc.parse_alias_type(typ, tc.type_aliases[typ])
	}
	if qtyp in tc.interface_names {
		return Type(Interface{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.interface_names {
		return Type(Interface{
			name: typ
		})
	}
	if qtyp in tc.structs {
		return Type(Struct{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.structs {
		return Type(Struct{
			name: typ
		})
	}
	if qtyp in tc.flag_enums {
		return Type(Enum{
			name:    qtyp
			is_flag: true
		})
	}
	if allow_bare_symbol && typ in tc.flag_enums {
		return Type(Enum{
			name:    typ
			is_flag: true
		})
	}
	if typ.contains('.') {
		short := typ.all_after_last('.')
		if qtyp !in tc.flag_enums && typ !in tc.flag_enums && short in tc.flag_enums {
			return Type(Enum{
				name:    short
				is_flag: true
			})
		}
	}
	if qtyp in tc.enum_names {
		return Type(Enum{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.enum_names {
		return Type(Enum{
			name: typ
		})
	}
	if typ.contains('.') {
		short := typ.all_after_last('.')
		if qtyp !in tc.enum_names && typ !in tc.enum_names && short in tc.enum_names {
			return Type(Enum{
				name: short
			})
		}
	}
	if qtyp in tc.sum_types {
		return Type(SumType{
			name: qtyp
		})
	}
	if allow_bare_symbol && typ in tc.sum_types {
		return Type(SumType{
			name: typ
		})
	}
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			if resolved_type := tc.type_from_known_symbol(resolved) {
				return resolved_type
			}
		}
	}
	if allow_bare_symbol && !typ.contains('.') {
		if resolved := tc.unique_qualified_type_name(typ) {
			if resolved in tc.type_aliases {
				return Type(Alias{
					name:      resolved
					base_type: tc.parse_type(tc.type_aliases[resolved])
				})
			}
			if resolved in tc.structs {
				return Type(Struct{
					name: resolved
				})
			}
			if resolved in tc.interface_names {
				return Type(Interface{
					name: resolved
				})
			}
			if resolved in tc.flag_enums {
				return Type(Enum{
					name:    resolved
					is_flag: true
				})
			}
			if resolved in tc.enum_names {
				return Type(Enum{
					name: resolved
				})
			}
			if resolved in tc.sum_types {
				return Type(SumType{
					name: resolved
				})
			}
		}
	}
	if generic_type_application(typ) {
		bracket := typ.index_u8(`[`)
		base := typ[..bracket]
		resolved_base := tc.resolve_imported_type_text(base)
		generic_suffix := typ[bracket..]
		_, generic_args, _ := generic_type_application_parts(typ)
		is_concrete_generic := tc.generic_args_are_concrete(generic_args)
		struct_generic_suffix := if is_concrete_generic {
			tc.qualified_generic_suffix(generic_args)
		} else {
			generic_suffix
		}
		sum_generic_suffix := if is_concrete_generic {
			tc.qualified_generic_suffix(generic_args)
		} else {
			generic_suffix
		}
		mut qbase := if tc.resolution_type_mode {
			tc.qualify_resolution_type_name(resolved_base)
		} else {
			tc.qualify_name(resolved_base)
		}
		if !resolved_base.contains('.') {
			if resolved := tc.resolve_selective_import_type_symbol(resolved_base) {
				qbase = resolved
			}
		}
		if qbase == resolved_base && resolved_base.contains('.') {
			qbase = tc.resolve_imported_type_text(resolved_base)
		}
		allow_bare_generic_base := qbase == resolved_base
		if qbase in tc.struct_generic_params {
			mut sgp_base := qbase
			if allow_bare_generic_base && qbase !in tc.structs && !resolved_base.contains('.') {
				// The bare key is only a generic-params shadow of an imported struct
				// (e.g. main referencing `Vec2` from `import math.vec { Vec2 }`, which is
				// keyed `vec.Vec2`). Resolve it to the real qualified struct so field/method
				// lookup and c_type match `vec.Vec2[int]`, not a bogus bare `Vec2[int]`.
				if resolved := tc.resolve_selective_import_type_symbol(resolved_base) {
					sgp_base = resolved
				} else if resolved := tc.unique_qualified_type_name(resolved_base) {
					sgp_base = resolved
				}
			}
			return Type(Struct{
				name: sgp_base + struct_generic_suffix
			})
		}
		if qbase in tc.type_aliases {
			return tc.parse_generic_alias_application(qbase, generic_args, struct_generic_suffix)
		}
		if qbase in tc.structs {
			return Type(Struct{
				name: qbase + struct_generic_suffix
			})
		}
		if qbase in tc.interface_names {
			return Type(Interface{
				name: qbase + struct_generic_suffix
			})
		}
		if qbase in tc.sum_types {
			return Type(SumType{
				name: qbase + sum_generic_suffix
			})
		}
		if short := tc.imported_type_short_name(resolved_base) {
			// A colliding main-module alias can make legacy module collection retain
			// the imported generic declaration under its semantic short name. Prefer
			// that generic declaration over the same-named alias.
			if short in tc.struct_generic_params || short in tc.structs {
				return Type(Struct{
					name: short + struct_generic_suffix
				})
			}
			if short in tc.sum_generic_params || short in tc.sum_types {
				return Type(SumType{
					name: short + sum_generic_suffix
				})
			}
			if short in tc.interface_names {
				return Type(Interface{
					name: short + struct_generic_suffix
				})
			}
			if short in tc.type_aliases {
				return tc.parse_generic_alias_application(short, generic_args,
					struct_generic_suffix)
			}
		}
		if !resolved_base.contains('.') {
			if resolved := tc.resolve_selective_import_type_symbol(resolved_base) {
				if resolved in tc.type_aliases {
					return tc.parse_generic_alias_application(resolved, generic_args,
						struct_generic_suffix)
				}
				if resolved in tc.structs {
					return Type(Struct{
						name: resolved + struct_generic_suffix
					})
				}
				if resolved in tc.interface_names {
					return Type(Interface{
						name: resolved + struct_generic_suffix
					})
				}
				if resolved in tc.sum_types {
					return Type(SumType{
						name: resolved + sum_generic_suffix
					})
				}
			}
		}
		if allow_bare_generic_base && resolved_base in tc.type_aliases {
			return tc.parse_generic_alias_application(resolved_base, generic_args,
				struct_generic_suffix)
		}
		if allow_bare_generic_base && resolved_base in tc.structs {
			return Type(Struct{
				name: resolved_base + struct_generic_suffix
			})
		}
		if allow_bare_generic_base && resolved_base in tc.interface_names {
			return Type(Interface{
				name: resolved_base + struct_generic_suffix
			})
		}
		if allow_bare_generic_base && resolved_base in tc.sum_types {
			return Type(SumType{
				name: resolved_base + sum_generic_suffix
			})
		}
		if is_concrete_generic && !is_builtin_type_name(resolved_base) {
			// A concrete generic instance (`Vec4[f32]`) is a monomorphized struct, even
			// when the generic base decl has been erased after monomorphization. It is
			// never a fixed array, so don't fall through to the `[N]T` handler below.
			// Qualify an imported base (`Vec4` -> `vec.Vec4`) so its c_type matches the
			// materialized struct (`vec__Vec4_f32`) everywhere it appears. A builtin base
			// (`int[seg_count]`) cannot be a generic application, so let it fall through to
			// the fixed-array handler — its bracket is a const/expression length.
			mut full := qbase + generic_suffix
			if allow_bare_generic_base && !resolved_base.contains('.') {
				if resolved := tc.unique_qualified_type_name(resolved_base) {
					full = resolved + generic_suffix
				}
			}
			return Type(Struct{
				name: full
			})
		}
	}
	if is_generic_placeholder_type(typ) {
		return unknown_type('generic type parameter `${typ}`')
	}
	if typ.contains('[') && !typ.starts_with('[') {
		// Postfix fixed-array name (`ArrayFixed.name()`): the element comes first and
		// each dimension is appended, so the OUTERMOST dimension is the trailing `[N]`
		// (`int[3][2]` is `[2][3]int`). Split on the last bracket pair so a nested fixed
		// array recovers the outer length and recurses into the inner element, instead of
		// taking the first `[N]` and dropping the rest. For a single dimension the last
		// and first brackets coincide, so this matches the previous behaviour.
		bracket := typ.last_index_u8(`[`)
		bracket_end := typ.last_index_u8(`]`)
		if bracket >= 0 && bracket_end > bracket {
			len_text := trimmed_space(typ[bracket + 1..bracket_end])
			return Type(ArrayFixed{
				elem_type: tc.parse_type(typ[..bracket])
				len:       if is_decimal_int_literal(len_text) { len_text.int() } else { 0 }
				len_expr:  if is_decimal_int_literal(len_text) { '' } else { len_text }
			})
		}
	}
	if qtyp != typ {
		return Type(Struct{
			name: qtyp
		})
	}
	return Type(Struct{
		name: typ
	})
}

fn (tc &TypeChecker) type_from_typeof_type_text(typ string) ?Type {
	clean := trimmed_space(typ)
	if !clean.starts_with('typeof(') || !clean.ends_with(')') {
		return none
	}
	inner := trimmed_space(clean[7..clean.len - 1])
	if inner.len == 0 {
		return none
	}
	if inner.ends_with(']') {
		open := inner.last_index_u8(`[`)
		if open > 0 {
			base_text := trimmed_space(inner[..open])
			if base_type := tc.type_from_simple_expr_text(base_text) {
				return tc.resolve_index_base_value_type(unalias_type(base_type))
			}
		}
	}
	return tc.type_from_simple_expr_text(inner)
}

fn (tc &TypeChecker) type_from_simple_expr_text(expr string) ?Type {
	clean := trimmed_space(expr)
	if clean.len == 0 {
		return none
	}
	if typ := tc.cur_scope.lookup(clean) {
		return typ
	}
	if typ := tc.file_scope.lookup(clean) {
		return typ
	}
	qname := tc.qualify_name(clean)
	if qname != clean {
		if typ := tc.file_scope.lookup(qname) {
			return typ
		}
		if typ := tc.const_types[qname] {
			return typ
		}
	}
	if typ := tc.const_types[clean] {
		return typ
	}
	return none
}

fn (tc &TypeChecker) qualified_generic_suffix(args []string) string {
	mut qualified_args := []string{cap: args.len}
	for arg in args {
		// Generic application args may name types from the instantiating
		// module (main's `Foo` inside a json2 specialization).
		qualified_args << tc.qualify_resolution_type_text(arg)
	}
	return '[' + qualified_args.join(', ') + ']'
}

fn (tc &TypeChecker) array_literal_elem_type(node flat.Node) Type {
	if node.children_count == 0 {
		return Type(int_)
	}
	elem_type := tc.array_literal_child_elem_type(tc.a.child(&node, 0))
	mut all_numeric := true
	mut has_f32 := false
	mut has_f64 := false
	mut has_explicit_f64 := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child_type := tc.array_literal_child_elem_type(child_id)
		if !(child_type.is_integer() || child_type.is_float()) {
			all_numeric = false
		}
		if child_type.name() == 'f32' {
			has_f32 = true
		}
		if child_type.name() == 'f64' {
			has_f64 = true
			if !tc.is_untyped_float_literal_expr(child_id) {
				has_explicit_f64 = true
			}
		}
	}
	if all_numeric && has_explicit_f64 {
		return Type(f64_)
	}
	if all_numeric && has_f32 {
		return tc.parse_type('f32')
	}
	if all_numeric && has_f64 {
		return Type(f64_)
	}
	return elem_type
}

fn (tc &TypeChecker) array_literal_child_elem_type(child_id flat.NodeId) Type {
	child := tc.a.nodes[int(child_id)]
	if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
		spread_type := unalias_type(tc.resolve_type(tc.a.child(&child, 0)))
		if spread_type is Array {
			return spread_type.elem_type
		}
		if spread_type is ArrayFixed {
			return spread_type.elem_type
		}
	}
	if alias_type := tc.explicit_alias_constructor_type(child_id) {
		return alias_type
	}
	return tc.array_literal_child_fn_value_type(child_id) or {
		actual := tc.resolve_type(child_id)
		tc.mut_param_expr_base(child_id, actual) or { actual }
	}
}

fn (tc &TypeChecker) array_literal_child_fn_value_type(child_id flat.NodeId) ?Type {
	if int(child_id) < 0 || int(child_id) >= tc.a.nodes.len {
		return none
	}
	child := tc.a.nodes[int(child_id)]
	if tc.fn_value_shadowed_by_value(child) {
		return none
	}
	key := tc.fn_value_key(child) or { return none }
	return tc.fn_type_from_key(key)
}

fn (tc &TypeChecker) explicit_alias_constructor_type(id flat.NodeId) ?Type {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	mut type_name := ''
	if node.kind == .cast_expr {
		type_name = node.value
	} else if node.kind == .call && node.children_count > 0 {
		fn_node_id := tc.a.child(&node, 0)
		type_name = tc.type_expr_name(fn_node_id)
	}
	if type_name.len == 0 {
		return none
	}
	qname := tc.qualify_name(type_name)
	if type_name in tc.sum_types {
		return tc.parse_type(type_name)
	}
	if qname in tc.sum_types {
		return tc.parse_type(qname)
	}
	if type_name in tc.type_aliases || qname in tc.type_aliases {
		return tc.parse_type(type_name)
	}
	return none
}

fn (tc &TypeChecker) is_untyped_float_literal_expr(id flat.NodeId) bool {
	known, has_float := tc.untyped_numeric_literal_expr_info(id, 0)
	return known && has_float
}

fn (tc &TypeChecker) untyped_numeric_literal_expr_info(id flat.NodeId, depth int) (bool, bool) {
	mut current_id := id
	for {
		if !tc.valid_node_id(current_id) {
			return false, false
		}
		current := tc.a.node(current_id)
		if current.kind !in [.paren, .expr_stmt] {
			break
		}
		if current.children_count == 0 {
			return false, false
		}
		current_id = tc.a.child(current, 0)
	}
	if depth > 16 {
		return false, false
	}
	node := tc.a.node(current_id)
	match node.kind {
		.float_literal {
			return true, true
		}
		.int_literal {
			return true, false
		}
		.prefix {
			if node.op !in [.plus, .minus] || node.children_count == 0 {
				return false, false
			}
			return tc.untyped_numeric_literal_expr_info(tc.a.child(node, 0), depth + 1)
		}
		.infix {
			if node.op !in [.plus, .minus, .mul, .div, .mod] || node.children_count < 2 {
				return false, false
			}
			left_known, left_float := tc.untyped_numeric_literal_expr_info(tc.a.child(node, 0),

				depth + 1)
			right_known, right_float := tc.untyped_numeric_literal_expr_info(tc.a.child(node, 1),

				depth + 1)
			return left_known && right_known, left_float || right_float
		}
		.ident {
			key := tc.const_key_for_name(node.value) or { return false, false }
			expr_id := tc.const_exprs[key] or { return false, false }
			return tc.untyped_numeric_literal_expr_info(expr_id, depth + 1)
		}
		.selector {
			if node.children_count == 0 {
				return false, false
			}
			base := tc.a.child_node(node, 0)
			if base.kind != .ident {
				return false, false
			}
			file := tc.a.source_files[node.pos.id] or { return false, false }
			module_name := tc.file_imports[file_import_key(file.name, base.value)] or { base.value }
			expr_id := tc.const_exprs['${module_name}.${node.value}'] or { return false, false }
			return tc.untyped_numeric_literal_expr_info(expr_id, depth + 1)
		}
		else {
			return false, false
		}
	}
}

fn (tc &TypeChecker) is_known_type_text(typ string) bool {
	qtyp := tc.qualify_name(typ)
	if !typ.contains('.') {
		if resolved := tc.resolve_selective_import_type_symbol(typ) {
			return tc.type_symbol_known(resolved)
		}
	}
	return qtyp in tc.structs || qtyp in tc.interface_names || qtyp in tc.enum_names
		|| qtyp in tc.sum_types || qtyp in tc.type_aliases
}

// canonical_qualified_type_name resolves a possibly bare or partially qualified
// type name (e.g. `PoolProcessor` or `pool.PoolProcessor`) to its unique fully
// qualified spelling (`sync.pool.PoolProcessor`). It returns none when the name
// is unknown or when the short name is ambiguous across modules. Backends use
// this to rebuild method/type C names when a receiver type carries only the
// import-local module qualifier instead of the full module path.
pub fn (tc &TypeChecker) canonical_qualified_type_name(name string) ?string {
	return tc.unique_qualified_type_name(name.all_after_last('.'))
}

// unique_qualified_type_name supports unique qualified type name handling for TypeChecker.
fn (tc &TypeChecker) unique_qualified_type_name(short_name string) ?string {
	if short_name.len == 0 {
		return none
	}
	// A full scan of the five type-name maps (with an all_after_last allocation
	// per entry) is far too expensive for a per-expression helper — this is
	// called from qualify_name and sum-variant pattern checks. Build the
	// short-name index once per checker (memoized in the heap-allocated
	// type_cache, so forked parallel workers each build their own).
	mut cache := tc.type_cache
	if isnil(cache) {
		return tc.unique_qualified_type_name_scan(short_name)
	}
	if !cache.short_type_name_index_built {
		mut fallback := cache.base
		for !isnil(fallback) {
			if fallback.short_type_name_index_built {
				found := fallback.short_type_name_index[short_name] or { return none }
				if found.len == 0 {
					return none
				}
				return found
			}
			fallback = fallback.base
		}
		cache.short_type_name_index_built = true
		tc.build_short_type_name_index(mut cache.short_type_name_index)
	}
	found := cache.short_type_name_index[short_name] or { return none }
	// An empty entry marks an ambiguous short name (several qualified types).
	if found.len == 0 {
		return none
	}
	return found
}

// invalidate_short_type_name_index drops memoized type-name-derived indexes;
// callers that add or remove entries in the type-name maps after the checker ran
// (the monomorphizer specializing generic structs/sum types) must invalidate them.
pub fn (tc &TypeChecker) invalidate_short_type_name_index() {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	cache.ierror_compat_entries.clear()
	cache.ierror_impl_names.clear()
	cache.ierror_impl_names_set = false
	if cache.short_type_name_index_built {
		cache.short_type_name_index_built = false
		cache.short_type_name_index.clear()
	}
}

// register_short_type_name extends an already-built short-name index after
// monomorphization adds a concrete type. A scoped transform must not rebuild
// the entire index from maps whose strings may belong to the checked arena.
pub fn (tc &TypeChecker) register_short_type_name(name string) {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	if !cache.short_type_name_index_built {
		mut fallback := cache.base
		for !isnil(fallback) && !fallback.short_type_name_index_built {
			fallback = fallback.base
		}
		if isnil(fallback) {
			return
		}
		cache.short_type_name_index = fallback.short_type_name_index.clone()
		cache.short_type_name_index_built = true
	}
	index_short_type_name(name, mut cache.short_type_name_index)
}

// unregister_short_type_name removes an exact cached entry when a generic
// template is erased. Ambiguous entries stay conservative until the next
// compilation-wide index build.
pub fn (tc &TypeChecker) unregister_short_type_name(name string) {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	if !cache.short_type_name_index_built {
		mut fallback := cache.base
		for !isnil(fallback) && !fallback.short_type_name_index_built {
			fallback = fallback.base
		}
		if isnil(fallback) {
			return
		}
		cache.short_type_name_index = fallback.short_type_name_index.clone()
		cache.short_type_name_index_built = true
	}
	short := name.all_after_last('.')
	if cached := cache.short_type_name_index[short] {
		if cached == name {
			cache.short_type_name_index.delete(short)
		}
	}
}

fn (tc &TypeChecker) build_short_type_name_index(mut index map[string]string) {
	for name, _ in tc.type_aliases {
		index_short_type_name(name, mut index)
	}
	for name, _ in tc.structs {
		index_short_type_name(name, mut index)
	}
	for name, _ in tc.interface_names {
		index_short_type_name(name, mut index)
	}
	for name, _ in tc.enum_names {
		index_short_type_name(name, mut index)
	}
	for name, _ in tc.sum_types {
		index_short_type_name(name, mut index)
	}
}

fn index_short_type_name(name string, mut index map[string]string) {
	short := name.all_after_last('.')
	if short in index {
		prev := index[short]
		if prev != name {
			index[short] = ''
		}
	} else {
		index[short] = name
	}
}

// unique_qualified_type_name_scan is the uncached fallback used when no
// type_cache is attached to the checker.
fn (tc &TypeChecker) unique_qualified_type_name_scan(short_name string) ?string {
	mut found := ''
	for name, _ in tc.type_aliases {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.structs {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.interface_names {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.enum_names {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	for name, _ in tc.sum_types {
		if name.all_after_last('.') == short_name {
			if found.len > 0 && found != name {
				return none
			}
			found = name
		}
	}
	if found.len == 0 {
		return none
	}
	return found
}

// is_generic_placeholder_type reports whether is generic placeholder type applies in types.
fn is_generic_placeholder_type(typ string) bool {
	if typ.contains('.') {
		last := typ.all_after_last('.')
		return is_generic_placeholder_type(last)
	}
	return is_bare_generic_param(typ)
}

// parse_fn_type reads parse fn type input for types.
fn (tc &TypeChecker) parse_fn_type(typ string) Type {
	params_start := typ.index_u8(`(`) + 1
	if params_start <= 0 || params_start >= typ.len {
		return unknown_type('malformed fn type `${typ}`')
	}
	mut depth := 1
	mut params_end := params_start
	for params_end < typ.len {
		if typ[params_end] == `(` {
			depth++
		} else if typ[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= typ.len || depth != 0 {
		return unknown_type('malformed fn type `${typ}`')
	}
	params_str := typ[params_start..params_end]
	ret_str := typ[params_end + 1..].trim_left(' ')
	mut params := []Type{}
	mut params_mut := []bool{}
	if params_str.trim_space().len > 0 {
		param_parts := split_params(params_str)
		for p in param_parts {
			trimmed := trimmed_space(p)
			param_type := normalize_fn_type_param_text(trimmed)
			params << tc.parse_type(param_type)
			params_mut << trimmed.starts_with('mut ')
		}
	}
	mut ret_type := Type(Void{})
	if ret_str.len > 0 {
		ret_type = tc.parse_type(ret_str)
	}
	return Type(FnType{
		params:      params
		params_mut:  params_mut
		return_type: ret_type
	})
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_from_text(typ string) ?string {
	clean := trimmed_space(typ)
	if !clean.starts_with('fn(') && !clean.starts_with('fn (') {
		return none
	}
	params_start := clean.index_u8(`(`) + 1
	mut depth := 1
	mut params_end := params_start
	for params_end < clean.len {
		if clean[params_end] == `(` {
			depth++
		} else if clean[params_end] == `)` {
			depth--
			if depth == 0 {
				break
			}
		}
		params_end++
	}
	if params_end >= clean.len {
		return none
	}
	params_str := clean[params_start..params_end]
	ret_str := clean[params_end + 1..].trim_left(' ')
	mut params := []string{}
	mut has_c_abi_param := false
	if trimmed_space(params_str).len > 0 {
		for part in split_params(params_str) {
			ct, is_c_abi := tc.c_abi_fn_param_type(part)
			params << ct
			if is_c_abi {
				has_c_abi_param = true
			}
		}
	}
	if !has_c_abi_param {
		return none
	}
	ret_type := if ret_str.len > 0 { tc.parse_type(ret_str) } else { Type(Void{}) }
	ret_ct := tc.fn_ptr_return_c_type(ret_type)
	params_ct := if params.len == 0 { 'void' } else { params.join(', ') }
	return 'fn_ptr:${ret_ct}|${params_ct}'
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text(typ string) ?string {
	mut seen := map[string]bool{}
	return tc.c_abi_fn_ptr_type_for_type_text_inner(trimmed_space(typ), mut seen)
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text_inner(typ string, mut seen map[string]bool) ?string {
	if typ.len == 0 || seen[typ] {
		return none
	}
	seen[typ] = true
	if c_abi_fn := tc.c_abi_fn_ptr_type_from_text(typ) {
		return c_abi_fn
	}
	for name in [tc.qualify_name(typ), typ] {
		if name.len == 0 {
			continue
		}
		if c_abi_fn := tc.type_alias_c_abi_fns[name] {
			return c_abi_fn
		}
		if target := tc.type_aliases[name] {
			if c_abi_fn := tc.c_abi_fn_ptr_type_for_type_text_inner(target, mut seen) {
				return c_abi_fn
			}
		}
	}
	return none
}

fn (tc &TypeChecker) c_abi_fn_param_type(param string) (string, bool) {
	clean := trimmed_space(param)
	param_type := normalize_fn_type_param_text(clean)
	if c_abi_fn_param_name(clean).starts_with('const_') && param_type.starts_with('&') {
		base_type := tc.parse_type(param_type[1..])
		return 'const ${tc.c_type(base_type)}*', true
	}
	if param_type.starts_with('&') {
		if ct := tc.c_abi_alias_pointer_param_c_type(param_type[1..]) {
			if clean.starts_with('mut ') {
				return '${ct}*', true
			}
			return 'const ${ct}*', true
		}
	}
	return tc.c_type(tc.parse_type(param_type)), false
}

fn (tc &TypeChecker) c_abi_alias_pointer_param_c_type(typ string) ?string {
	t := tc.parse_type(typ)
	if t is Alias {
		base := c_abi_alias_c_base_type(t.base_type) or { return none }
		return tc.c_type(base)
	}
	return none
}

fn c_abi_alias_c_base_type(t Type) ?Type {
	if t is Alias {
		return c_abi_alias_c_base_type(t.base_type)
	}
	if t is Struct && t.name.starts_with('C.') {
		return t
	}
	return none
}

fn c_abi_fn_param_name(param string) string {
	mut text := trimmed_space(param)
	if text.starts_with('mut ') {
		text = trimmed_space(text[4..])
	}
	space := top_level_space_index(text)
	if space <= 0 {
		return ''
	}
	head := trimmed_space(text[..space])
	tail := trimmed_space(text[space + 1..])
	if fn_type_param_head_is_name(head, tail) {
		return head
	}
	return ''
}

fn struct_field_c_abi_key(struct_name string, field_name string) string {
	return '${struct_name}\n${field_name}'
}

fn (tc &TypeChecker) comptime_static_type_expr_name(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .array_init && node.value == '__v3_comptime_type_array'
		&& node.children_count == 1 {
		elem := tc.comptime_static_type_expr_name(tc.a.child(&node, 0)) or { return none }
		return '[]${elem}'
	}
	if node.kind == .typeof_expr {
		if node.value.len > 0 && !tc.type_text_has_generic_placeholder(node.value) {
			return node.value
		}
		if node.children_count == 1 {
			typ := tc.resolve_type(tc.a.child(&node, 0))
			if typ !is Unknown && !tc.type_text_has_generic_placeholder(typ.name()) {
				return typ.name()
			}
		}
		return none
	}
	if node.kind == .selector && node.children_count == 1 {
		base_name := tc.comptime_static_type_expr_name(tc.a.child(&node, 0)) or { return none }
		mut base_type := tc.parse_type(base_name)
		if base_type is Unknown {
			return none
		}
		if node.value == 'typ' {
			return base_name
		}
		if node.value == 'unaliased_typ' {
			return unalias_type(base_type).name()
		}
		if node.value in ['payload_type', 'pointee_type'] {
			base_type = unalias_type(base_type)
			if base_type is OptionType {
				base_type = unalias_type(base_type.base_type)
			} else if base_type is ResultType {
				base_type = unalias_type(base_type.base_type)
			}
			if node.value == 'payload_type' {
				return base_type.name()
			}
			if base_type is Pointer {
				return base_type.base_type.name()
			}
			return none
		}
		base_type = unalias_type(base_type)
		if node.value == 'element_type' {
			if base_type is Array {
				return base_type.elem_type.name()
			}
			if base_type is ArrayFixed {
				return base_type.elem_type.name()
			}
			return none
		}
		if base_type is Map {
			if node.value == 'key_type' {
				return base_type.key_type.name()
			}
			if node.value == 'value_type' {
				return base_type.value_type.name()
			}
		}
		return none
	}
	if node.kind != .ident || node.value.len == 0
		|| tc.type_text_has_generic_placeholder(node.value) {
		return none
	}
	return node.value
}

// resolve_type resolves resolve type information for types.
// BodyResolveMemo caches resolve_type results for the node range owned by the
// check work item currently being verified. Inside one function body every
// node's resolution context is fixed (its smartcast set and scope bindings are
// determined by its position), while the checker resolves the same subtrees
// repeatedly (call info, argument checks, and child traversal each re-resolve
// the same expressions). The memo lives per checker fork and is reset for
// every work item, so it never crosses function or phase boundaries.
@[heap]
struct BodyResolveMemo {
mut:
	active           bool
	lo               int
	hi               int
	types            []Type
	filled           []u8
	call_generation  u32 = 1
	call_ids         [2048]int
	call_generations [2048]u32
	call_infos       [2048]CallInfo
}

// arm_body_resolve_memo (re)activates the per-item resolve memo for one work
// item's node range. The transformer arms it per lowered function exactly like
// the checker does per checked item; node-write helpers invalidate rewritten
// slots (see invalidate_checked_expr_type).
pub fn (tc &TypeChecker) arm_body_resolve_memo(lo int, hi int) {
	mut wtc := unsafe { tc }
	if isnil(wtc.body_resolve_memo) {
		wtc.body_resolve_memo = &BodyResolveMemo{}
	}
	wtc.body_resolve_memo.begin(lo, hi)
}

// disarm_body_resolve_memo deactivates the per-item resolve memo.
pub fn (tc &TypeChecker) disarm_body_resolve_memo() {
	if !isnil(tc.body_resolve_memo) {
		mut memo := tc.body_resolve_memo
		memo.active = false
	}
}

// reset_body_resolve_memo detaches the memo entirely. The transformer arms it
// inside a disposable stage arena, so the master checker must drop the pointer
// before that arena is released — later phases would otherwise dereference a
// freed allocation just to see that the memo is inactive.
pub fn (tc &TypeChecker) reset_body_resolve_memo() {
	mut wtc := unsafe { tc }
	wtc.body_resolve_memo = unsafe { nil }
}

fn (mut memo BodyResolveMemo) begin(lo int, hi int) {
	if lo < 0 || hi < lo {
		memo.active = false
		return
	}
	span := hi - lo + 1
	memo.lo = lo
	memo.hi = hi
	memo.call_generation++
	if memo.types.len < span {
		memo.types = []Type{len: span, init: Type(void_)}
		memo.filled = []u8{len: span}
	} else {
		unsafe { vmemset(memo.filled.data, 0, span) }
	}
	memo.active = true
}

@[direct_array_access]
pub fn (tc &TypeChecker) resolve_type(id flat.NodeId) Type {
	if tc.trust_checked_expr_types {
		// Post-check phases re-resolve mostly unchanged subtrees the checker
		// already typed. Serve those straight from the dense per-node cache:
		// dense in-range entries are checker-authored, transform's node-write
		// helpers invalidate every id they rewrite, and appended nodes lie
		// beyond the dense range so they resolve normally below. Cached
		// unknowns stay excluded — a later registration may resolve them.
		tidx := int(id)
		if tidx >= 0 && tidx < tc.expr_type_set.len && tc.expr_type_set[tidx]
			&& (!tc.parallel_check_sparse || tc.in_check_range(tidx)) {
			typ := tc.expr_type_values[tidx]
			if !type_contains_unknown(typ) {
				return typ
			}
		}
	}
	memo := tc.body_resolve_memo
	idx := int(id)
	if isnil(memo) || !memo.active || idx < memo.lo || idx > memo.hi {
		return tc.resolve_type_uncached(id)
	}
	mi := idx - memo.lo
	if memo.filled[mi] != 0 {
		return memo.types[mi]
	}
	typ := tc.resolve_type_uncached(id)
	// Unknowns can be provisional (cycle guards, generic placeholders that a
	// later registration resolves); never memoize them, including when they are
	// nested inside a collection or wrapper.
	if !type_contains_unknown(typ) {
		mut m := unsafe { &BodyResolveMemo(memo) }
		m.types[mi] = typ
		m.filled[mi] = 1
	}
	return typ
}

@[direct_array_access]
fn (tc &TypeChecker) resolve_type_uncached(id flat.NodeId) Type {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return unknown_type('missing node')
	}
	node_ref := tc.a.node(id)
	node := tc.a.nodes[int(id)]
	if node.kind == .ident && tc.errors.any(it.node == id && it.kind == .unknown_ident) {
		return Type(void_)
	}
	if node.kind == .selector && tc.errors.any(it.node == id
		&& it.msg.contains(' does not return a value')) {
		return Type(void_)
	}
	if node.kind == .call && tc.errors.any(it.node == id && it.msg.starts_with('unknown enum `')) {
		return Type(void_)
	}
	if node.kind == .directive && node.value == '$res' {
		return_type := tc.fn_context.return_type
		if return_type is ResultType || return_type is Void {
			return Type(void_)
		}
		if return_type is MultiReturn {
			if node.children_count == 0 {
				return Type(void_)
			}
			index_node := tc.a.child_node(node_ref, 0)
			if index_node.kind != .int_literal {
				return Type(void_)
			}
			index := index_node.value.int()
			if index < 0 || index >= return_type.types.len {
				return Type(void_)
			}
			return return_type.types[index]
		}
		return return_type
	}
	if node.kind == .string_literal && node.children_count == 1
		&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
		if type_name := tc.comptime_static_type_expr_name(tc.a.child(&node, 0)) {
			target := tc.parse_type(type_name)
			if target !is Unknown {
				return if node.value == '__v3_comptime_new' {
					Type(Pointer{
						base_type: target
					})
				} else {
					target
				}
			}
		}
	}
	if node.kind == .paren && node.children_count > 0 {
		return tc.resolve_type(tc.a.child(&node, 0))
	}
	if node.kind == .expr_stmt {
		if node.children_count == 0 {
			return Type(void_)
		}
		if node.children_count == 1 {
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		mut expr_types := []Type{cap: node.children_count}
		for i in 0 .. node.children_count {
			value_id := tc.a.child(&node, i)
			typ := tc.expr_type(value_id) or { tc.resolve_type(value_id) }
			expr_types << typ
		}
		return Type(MultiReturn{
			types: expr_types
		})
	}
	kind_id := node_kind_id(node)
	if kind_id == 1 {
		return Type(int_)
	}
	if kind_id == 2 {
		return Type(f64_)
	}
	if kind_id == 3 {
		return Type(bool_)
	}
	if kind_id == 4 {
		if node.value.starts_with('c:') {
			return Type(Pointer{
				base_type: Type(u8_)
			})
		}
		return Type(rune_)
	}
	if kind_id == 5 || kind_id == 6 {
		return Type(string_)
	}
	if node.kind in [.sizeof_expr, .offsetof_expr] {
		return Type(USize{})
	}
	if kind_id == 28 {
		return Type(voidptr_)
	}
	if kind_id == 29 {
		return Type(OptionType{
			base_type: Type(void_)
		})
	}
	if kind_id == 21 {
		return tc.fn_literal_type(node)
	}
	if kind_id == 32 {
		return tc.lambda_expr_type(node)
	}
	if kind_id == 12 && node.typ.len > 0 && node.typ !in ['int', 'array', 'map', 'unknown'] {
		return tc.parse_type(node.typ)
	}
	if t := tc.resolved_call_type(id) {
		return t
	}
	if kind_id == 22 {
		if aggregate_type := tc.sql_aggregate_or_expr_type(node) {
			return aggregate_type
		}
		if value_type := tc.match_trailing_or_value_type(tc.a.child(&node, 0)) {
			return value_type
		}
		if payload := tc.or_expr_payload_type(tc.a.child(&node, 0)) {
			return payload
		}
		inner := tc.resolve_type(tc.a.child(&node, 0))
		if inner is OptionType {
			return inner.base_type
		}
		if inner is ResultType {
			return inner.base_type
		}
		return inner
	}
	if smart_type := tc.smartcast_type(id) {
		return smart_type
	}
	if kind_id == 14 {
		return tc.resolve_index_type(node)
	}
	if node.kind == .infix && node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
		return Type(bool_)
	}
	if !(tc.smartcasts.len > 0 && (kind_id == 7 || kind_id == 13)) {
		if typ := tc.cached_expr_type(id) {
			// A resolution-only annotation can cache `void` for a method whose
			// receiver is a local alias that is only known during body checking.
			// Re-resolve calls in that case instead of hiding the real return type.
			if node.kind != .call || typ !is Void {
				return typ
			}
		}
	}
	if node.kind == .selector {
		if typ := tc.global_type_for_selector(node) {
			return typ
		}
		if typ := tc.const_type_for_selector(node) {
			return typ
		}
	}
	if node.kind == .defer_result && node.typ == 'invalid' {
		return Type(void_)
	}
	if node.typ.len > 0 && node.typ != 'unknown' && !(kind_id == 12
		&& node.typ in ['int', 'array', 'map']) {
		return tc.parse_type(node.typ)
	}
	match node.kind {
		.int_literal {
			return Type(int_)
		}
		.float_literal {
			return Type(f64_)
		}
		.bool_literal {
			return Type(bool_)
		}
		.char_literal {
			return Type(rune_)
		}
		.string_literal, .string_interp {
			return Type(string_)
		}
		.nil_literal {
			return Type(voidptr_)
		}
		.none_expr {
			return Type(OptionType{
				base_type: Type(void_)
			})
		}
		.spawn_expr {
			if node.children_count == 0 {
				return tc.parse_type('thread')
			}
			child_id := tc.a.child(&node, 0)
			child_node := tc.a.nodes[int(child_id)]
			mut spawn_ret := if child_node.kind == .call {
				tc.spawn_child_call_return_type(child_node) or { tc.resolve_type(child_id) }
			} else {
				tc.resolve_type(child_id)
			}
			if spawn_ret is Unknown && child_node.kind == .call {
				spawn_ret = tc.resolve_type(child_id)
			}
			if spawn_ret is Void || spawn_ret is Unknown {
				return tc.parse_type('thread')
			}
			return tc.parse_type('thread ${spawn_ret.name()}')
		}
		.enum_val {
			return Type(int_)
		}
		.defer_result {
			return tc.defer_result_type(node) or { unknown_type('invalid `res` expression') }
		}
		.ident {
			if node.value == '_' {
				return Type(void_)
			}
			if is_bare_generic_param(node.value) {
				active_generic_ident := node.value in tc.fn_context.generic_params
					|| tc.active_generic_param(node.value)
					|| tc.node_has_enclosing_generic_param(id, node.value)
					|| tc.source_enclosing_fn_has_generic_param(id, node.value)
				if active_generic_ident {
					return unknown_type('generic placeholder `${node.value}`')
				}
			}
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.non_file_scope_type(node.value) {
				return typ
			}
			qname := tc.qualify_name(node.value)
			if qname != node.value {
				if typ := tc.file_scope.lookup(qname) {
					return typ
				}
			}
			if typ := tc.const_types[qname] {
				return typ
			}
			if typ := tc.file_scope.lookup(node.value) {
				return typ
			}
			if typ := tc.const_types[node.value] {
				return typ
			}
			if typ := tc.fn_value_type(node.value) {
				return typ
			}
			if tc.selective_import_symbol_is_ambiguous(node.value) {
				return unknown_type('ambiguous selective import `${node.value}`')
			}
			if node.value == 'err' {
				return tc.parse_type('IError')
			}
			if is_bare_generic_param(node.value) {
				return Type(void_)
			}
			return unknown_type('unknown identifier `${node.value}`')
		}
		.call {
			fn_node := tc.a.child_node(node_ref, 0)
			if _ := tc.builtin_isreftype_call_arg(node) {
				return Type(bool_)
			}
			if arg_id := tc.builtin_addr_call_arg(node) {
				return Type(Pointer{
					base_type: tc.resolve_type(arg_id)
				})
			}
			if fn_node.kind !in [.ident, .selector] {
				fn_type := tc.resolve_type(tc.a.child(&node, 0))
				if fn_typ := fn_type_from_type(fn_type) {
					return fn_typ.return_type
				}
			}
			if fn_node.kind == .ident {
				if typ := tc.cur_scope.lookup(fn_node.value) {
					if typ is FnType {
						return typ.return_type
					}
				}
			}
			if fn_node.kind == .selector {
				base_node := tc.a.child_node(fn_node, 0)
				if base_node.kind == .ident && base_node.value == 'C' {
					c_fn_name := 'C.${fn_node.value}'
					if ret := tc.fn_ret_types[c_fn_name] {
						return ret
					}
					if ret := tc.fn_ret_types[fn_node.value] {
						return ret
					}
					return Type(Struct{
						name: c_fn_name
					})
				}
				if base_node.kind == .ident {
					if !tc.ident_resolves_to_value(base_node.value) {
						if resolved := tc.resolve_import_alias(base_node.value) {
							if imported_name := tc.imported_fn_key(resolved, fn_node.value) {
								return tc.fn_ret_types[imported_name] or {
									unknown_type('unknown return type for `${imported_name}`')
								}
							}
							mod_name := '${resolved}.${fn_node.value}'
							if mod_name in tc.sum_types {
								return Type(SumType{
									name: mod_name
								})
							}
							if mod_name in tc.structs {
								return Type(Struct{
									name: mod_name
								})
							}
							if mod_name in tc.enum_names {
								return Type(Enum{
									name: mod_name
								})
							}
						}
					}
					if base_node.value in tc.structs || base_node.value in tc.enum_names {
						qname := tc.qualify_name(base_node.value)
						sname := '${qname}.${fn_node.value}'
						if ret := tc.fn_ret_types[sname] {
							return ret
						}
					} else {
						if static_name := tc.static_assoc_fn_key_for_base(base_node.value,
							fn_node.value)
						{
							return tc.fn_ret_types[static_name] or {
								unknown_type('unknown return type for `${static_name}`')
							}
						}
					}
				} else if base_node.kind == .selector {
					inner := tc.a.child_node(base_node, 0)
					if inner.kind == .ident {
						mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
						full_name := '${mod_name}.${base_node.value}.${fn_node.value}'
						if ret := tc.fn_ret_types[full_name] {
							return ret
						}
						if static_name := tc.static_assoc_fn_key_for_base('${mod_name}.${base_node.value}',
							fn_node.value)
						{
							return tc.fn_ret_types[static_name] or {
								unknown_type('unknown return type for `${static_name}`')
							}
						}
					}
				}
				if fn_typ := tc.selector_const_fn_type(fn_node) {
					return fn_typ.return_type
				}
				base_id := tc.a.child(fn_node, 0)
				base_type := tc.selector_fn_base_type(base_id) or { tc.resolve_type(base_id) }
				if fn_typ := tc.selector_field_fn_type(fn_node, base_type) {
					return fn_typ.return_type
				}
				clean_type := unwrap_pointer(base_type)
				if fn_node.value == 'wait' {
					if ret_type := tc.thread_wait_return_type(base_type) {
						return ret_type
					}
				}
				if clean_type is ArrayFixed && fn_node.value == 'clone' {
					return Type(Array{
						elem_type: clean_type.elem_type
					})
				}
				if clean_type is Alias {
					candidates := receiver_method_name_candidates(clean_type, fn_node.value,
						tc.cur_module)
					for mname in candidates {
						if ret := tc.fn_ret_types[mname] {
							return tc.alias_return_type_from_text(mname) or { ret }
						}
					}
					if mname := tc.unique_receiver_method_suffix_match(candidates) {
						return tc.alias_return_type_from_text(mname) or {
							tc.fn_ret_types[mname] or {
								unknown_type('unknown return type for `${mname}`')
							}
						}
					}
				}
				if clean_array := array_like_type_for_method(clean_type, fn_node.value) {
					if fn_node.value in ['clone', 'move', 'reverse'] {
						return clean_type
					}
					if fn_node.value == 'filter' || fn_node.value == 'sorted' {
						if receiver_is_fixed_array(clean_type) {
							// filtering a fixed array yields a dynamic array
							return Type(Array{
								elem_type: clean_array.elem_type
							})
						}
						return clean_type
					}
					if fn_node.value in ['any', 'all'] {
						return Type(bool_)
					}
					if fn_node.value == 'count' {
						return Type(int_)
					}
					if fn_node.value == 'sort' {
						return Type(void_)
					}
					if fn_node.value == 'last' || fn_node.value == 'first' || fn_node.value == 'pop' {
						return array_elem_type(clean_array)
					}
					if fn_node.value == 'contains' {
						return Type(bool_)
					}
					if fn_node.value == 'repeat' || fn_node.value == 'repeat_to_depth' {
						return base_type
					}
					if fn_node.value == 'index' {
						return Type(int_)
					}
					if fn_node.value == 'join' || fn_node.value == 'str' {
						return Type(string_)
					}
					if fn_node.value == 'map' {
						return Type(Array{
							elem_type: Type(Unknown{
								reason: 'array.map'
							})
						})
					}
					if fn_node.value == 'wait' {
						// `[]thread T`.wait() joins all threads and returns `[]T`. A bare
						// `[]thread` (threads with no return value) joins to `void`. Any
						// optional/result payloads remain per-thread, so
						// `[]thread !T`.wait()` is `[]!T`.
						// other array element type is not a thread and `.wait()` is
						// unsupported, so reject it rather than mis-typing the call as the
						// receiver array (which would emit invalid C joining non-handles).
						elem := array_elem_type(clean_array)
						if elem is Struct {
							if elem.name == 'thread' {
								return Type(void_)
							}
							if elem.name.starts_with('thread ') {
								return tc.thread_array_wait_return_type(elem.name[7..])
							}
						}
						return unknown_type('`.wait()` requires an array of threads')
					}
					if fn_node.value == 'clone' {
						return clean_type
					}
					elem_type := array_elem_type(clean_array)
					elem_name := elem_type.name()
					mut short_elem := elem_name
					mut mod_prefix := ''
					if elem_name.contains('.') {
						short_elem = elem_name.all_after_last('.')
						mod_prefix = elem_name.all_before_last('.')
					}
					arr_mname1 := '[]${short_elem}.${fn_node.value}'
					if mod_prefix.len > 0 {
						arr_mkey := '${mod_prefix}.${arr_mname1}'
						if ret := tc.fn_ret_types[arr_mkey] {
							return ret
						}
					}
					if ret := tc.fn_ret_types[arr_mname1] {
						return ret
					}
					array_mname := 'array.${fn_node.value}'
					if ret := tc.fn_ret_types[array_mname] {
						return ret
					}
					return unknown_type('unknown array method `${fn_node.value}`')
				}
				if clean_type is Map {
					if fn_node.value in ['clone', 'move'] {
						return base_type
					}
					if fn_node.value in ['delete', 'clear', 'free'] {
						return Type(void_)
					}
					if fn_node.value == 'keys' {
						return Type(Array{
							elem_type: clean_type.key_type
						})
					}
					if fn_node.value == 'values' {
						return Type(Array{
							elem_type: clean_type.value_type
						})
					}
					for mname in receiver_method_name_candidates(clean_type, fn_node.value,
						tc.cur_module) {
						if checker_is_raw_collection_method_name(mname, 'map.') {
							continue
						}
						if ret := tc.fn_ret_types[mname] {
							return ret
						}
					}
					map_mname := 'map.${fn_node.value}'
					if ret := tc.fn_ret_types[map_mname] {
						return ret
					}
					return unknown_type('unknown map method `${fn_node.value}`')
				}
				if clean_type is String {
					mname := 'string.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
				if fn_node.value == 'str'
					&& (clean_type is Primitive || clean_type is Char || clean_type is Rune) {
					return Type(string_)
				}
				if (clean_type is Void || clean_type is Primitive)
					&& fn_node.value in ['vstring', 'vstring_with_len'] {
					return Type(string_)
				}
				if clean_type is Alias {
					mname := '${clean_type.name}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
					base_name := resolve_type_name_for_method(clean_type.base_type)
					if base_name.len > 0 {
						for base_mname in receiver_method_name_candidates(clean_type.base_type,
							fn_node.value, tc.cur_module) {
							if ret := tc.fn_ret_types[base_mname] {
								return ret
							}
						}
					}
				}
				if clean_type is Struct {
					mname := '${clean_type.name}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
					// A method on a concrete generic instance (`Box[int].clone`) is
					// registered under the open form (`Box[T].clone`); resolve it so the
					// call types as the substituted return (`Box[int]`) rather than the
					// bare base the collapsed open signature would yield.
					if ci := tc.resolve_generic_struct_method(clean_type.name, fn_node.value) {
						return ci.return_type
					}
				}
				if clean_type is Interface {
					mname := '${clean_type.name}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
				if clean_type is SumType {
					mname := '${clean_type.name}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
				if clean_type is Enum {
					if fn_node.value == 'str' {
						return Type(string_)
					}
					mname := '${clean_type.name}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
				if clean_type is Primitive {
					mname := '${prim_c_type_from(clean_type.props, clean_type.size)}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
			}
			if local_name := tc.local_bare_fn_key(fn_node.value) {
				return tc.fn_ret_types[local_name] or {
					unknown_type('unknown return type for `${local_name}`')
				}
			}
			if imported_name := tc.resolve_selective_import_symbol(fn_node.value) {
				if ret := tc.fn_ret_types[imported_name] {
					return ret
				}
			}
			if ret := tc.fn_ret_types[fn_node.value] {
				return ret
			}
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			$if debug {
				if tc.verbose {
					eprintln('warning: unknown fn return type `${fn_node.value}`')
				}
			}
			return unknown_type('unknown function `${fn_node.value}`')
		}
		.infix {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return Type(bool_)
			}
			lhs_id := tc.a.child(&node, 0)
			rhs_id := tc.a.child(&node, 1)
			lt := tc.infix_read_type(lhs_id)
			lt_raw := lt
			rt := tc.infix_read_type(rhs_id)
			rt_raw := rt
			if lt is Void || rt is Void {
				return Type(void_)
			}
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned]
				&& (!unalias_type(lt).is_integer() || !unalias_type(rt).is_integer()) {
				return Type(void_)
			}
			if node.op == .left_shift && array_type_from_receiver(lt) != none {
				return Type(void_)
			}
			if node.op in [.plus, .minus] {
				if node.op == .minus && lt is Pointer && rt is Pointer {
					return Type(int_)
				}
				if lt is Pointer && rt.is_integer() {
					return lt_raw
				}
				if node.op == .plus && rt is Pointer && lt.is_integer() {
					return rt_raw
				}
			}
			if operator_ret := tc.infix_operator_return_type(node.op, lt, rt) {
				return operator_ret
			}
			if node.op == .right_shift_unsigned {
				return unsigned_shift_result_type(lt)
			}
			if node.op == .plus {
				if lt is String && optional_payload_is_string(rt) {
					return rt_raw
				}
				if rt is String && optional_payload_is_string(lt) {
					return lt_raw
				}
			}
			if lt is String {
				return lt_raw
			}
			if rt is String {
				return rt_raw
			}
			if int_promoted := tc.int_literal_promoted_infix_type(lhs_id, rhs_id, rt) {
				return int_promoted
			}
			if int_promoted := tc.int_literal_promoted_infix_type(rhs_id, lhs_id, lt) {
				return int_promoted
			}
			if lt.is_float() || rt.is_float() {
				if type_is_f32(lt) && (type_is_f32(rt) || unalias_type(rt).is_integer()
					|| tc.is_untyped_float_literal_expr(rhs_id)) {
					return Type(f32_)
				}
				if type_is_f32(rt) && (type_is_f32(lt) || unalias_type(lt).is_integer()
					|| tc.is_untyped_float_literal_expr(lhs_id)) {
					return Type(f32_)
				}
				return Type(f64_)
			}
			return lt
		}
		.prefix {
			if node.op == .amp && node.children_count > 0 {
				child_id := tc.a.child(&node, 0)
				if inner := tc.smartcast_type(child_id) {
					return Type(Pointer{
						base_type: inner
					})
				}
			}
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			if node.op == .not {
				return Type(bool_)
			}
			if node.op == .amp {
				child_id := tc.a.child(&node, 0)
				child := tc.a.nodes[int(child_id)]
				if child.kind == .or_expr && child.children_count > 0 {
					source := tc.a.child_node(&child, 0)
					if source.kind == .index && source.children_count > 0 {
						base_type :=
							unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(source, 0)))
						if base_type is Map {
							return Type(Pointer{
								base_type: base_type.value_type
							})
						}
					}
				}
				if child.kind == .ident && child.value.len > 0 {
					if base := tc.fn_context.mut_param_base_types[child.value] {
						if tc.mut_param_binding_matches_lvalue(child.value) {
							return Type(Pointer{
								base_type: base
							})
						}
					}
				}
				inner := tc.smartcast_type(child_id) or { tc.resolve_type(child_id) }
				if inner is Void && tc.expr_subtree_has_error(child_id) {
					return Type(void_)
				}
				if inner is OptionType {
					return Type(OptionType{
						base_type: Type(Pointer{
							base_type: inner.base_type
						})
					})
				}
				if inner is ResultType {
					return Type(ResultType{
						base_type: Type(Pointer{
							base_type: inner.base_type
						})
					})
				}
				return Type(Pointer{
					base_type: inner
				})
			}
			if node.op == .mul {
				inner := unalias_type(tc.resolve_type(tc.a.child(&node, 0)))
				if inner is Pointer {
					return inner.base_type
				}
				return inner
			}
			if node.op == .arrow {
				inner := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&node, 0)))
				if inner is Channel {
					return inner.elem_type
				}
			}
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		.paren {
			return tc.resolve_type(tc.a.child(&node, 0))
		}
		.dump_expr {
			if node.children_count > 0 {
				return tc.resolve_type(tc.a.child(&node, 0))
			}
			return Type(void_)
		}
		.or_expr {
			if aggregate_type := tc.sql_aggregate_or_expr_type(node) {
				return aggregate_type
			}
			if node.children_count > 0 {
				if value_type := tc.match_trailing_or_value_type(tc.a.child(&node, 0)) {
					return value_type
				}
				if payload := tc.or_expr_payload_type(tc.a.child(&node, 0)) {
					return payload
				}
			}
			inner := tc.resolve_type(tc.a.child(&node, 0))
			if inner is OptionType {
				return inner.base_type
			}
			if inner is ResultType {
				return inner.base_type
			}
			return inner
		}
		.struct_init {
			return tc.parse_type(node.value)
		}
		.assoc {
			if node.value.len > 0 {
				return tc.parse_type(node.value)
			}
			if node.children_count > 0 {
				return tc.resolve_type(tc.a.child(&node, 0))
			}
			return unknown_type('missing assoc base')
		}
		.sizeof_expr {
			return Type(USize{})
		}
		.offsetof_expr {
			return Type(USize{})
		}
		.cast_expr {
			return tc.parse_type(node.value)
		}
		.selector {
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.enum_selector_type(node_ref) {
				return typ
			}
			if key := tc.selector_fn_value_key(node) {
				return tc.fn_type_from_key(key) or { unknown_type('unknown function `${key}`') }
			}
			if tc.unknown_import_selector(node) {
				return Type(void_)
			}
			base_node := tc.a.child_node(node_ref, 0)
			if base_node.kind == .typeof_expr {
				if node.value == 'name' {
					return Type(String{})
				}
				if node.value == 'idx' {
					return Type(int_)
				}
				if node.value == 'indirections' {
					return Type(u8_)
				}
			}
			if base_node.kind == .ident {
				if base_node.value == 'os' && node.value == 'args' {
					return Type(Array{
						elem_type: Type(String{})
					})
				}
				if gt := tc.file_scope.lookup(node.value) {
					if gt !is Unknown {
						return gt
					}
				}
				resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
				qname := '${resolved}.${node.value}'
				if qname.starts_with('C.') {
					if c_int_selector_name(node.value) {
						return Type(int_)
					}
					if c_upper_constant_is_pointer(qname) {
						return Type(voidptr_)
					}
					if gt := tc.c_globals[qname] {
						return gt
					}
					if node.value.len > 0 && node.value[0].is_capital() {
						return Type(int_)
					}
				}
				if qname in tc.const_types {
					typ := tc.const_types[qname] or { unknown_type('unknown const `${qname}`') }
					return tc.const_type_from_initializer(qname, typ)
				}
				if key := tc.const_key_for_suffix(qname) {
					typ := tc.const_types[key] or { unknown_type('unknown const `${key}`') }
					return tc.const_type_from_initializer(key, typ)
				}
			}
			base_type := tc.resolve_type(tc.a.child(&node, 0))
			clean0 := unalias_and_unwrap_pointer_type(base_type)
			mut clean := clean0
			if clean0 is Alias {
				clean = clean0.base_type
			}
			if typ := option_result_selector_type(clean, node.value) {
				return typ
			}
			if node.value == 'len' {
				if clean is Array || clean is Map || clean is String || clean is ArrayFixed {
					return Type(int_)
				}
			}
			if clean is Struct {
				if typ := tc.struct_field_type(clean.name, node.value) {
					return typ
				}
				if typ := tc.method_value_type(clean.name, node.value) {
					return typ
				}
			}
			if clean is Interface {
				if typ := tc.interface_field_type(clean.name, node.value) {
					return typ
				}
			}
			if clean is MultiReturn {
				if typ := multi_return_selector_type(clean, node.value) {
					return typ
				}
			}
			if clean is SumType {
				if typ := tc.sum_shared_field_type(clean, node.value) {
					return typ
				}
				if typ := tc.lowered_sum_selector_type(clean, node.value) {
					return typ
				}
			}
			if clean is Array || clean is Map || clean is String {
				sname := if clean is Array {
					'array'
				} else if clean is Map {
					'map'
				} else {
					'string'
				}
				if sname in tc.structs {
					for f in tc.structs[sname] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			if clean is Primitive && base_node.kind == .selector {
				vname := base_node.value.replace('__', '.')
				if vname in tc.structs {
					for f in tc.structs[vname] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			return unknown_type('unknown selector `${node.value}`')
		}
		.array_literal {
			if node.typ.len > 0 {
				typ := tc.parse_type(node.typ)
				if typ is ArrayFixed {
					return typ
				}
			}
			if node.children_count > 0 {
				elem_type := tc.array_literal_elem_type(node)
				return Type(Array{
					elem_type: elem_type
				})
			}
			return Type(Array{
				elem_type: Type(int_)
			})
		}
		.postfix {
			if node.children_count == 0 {
				return unknown_type('missing postfix expression')
			}
			child_id := tc.a.child(&node, 0)
			child := tc.a.nodes[int(child_id)]
			if node.op == .not && child.kind == .array_literal {
				if child.typ.len > 0 {
					typ := tc.parse_type(child.typ)
					if typ is ArrayFixed {
						return typ
					}
				}
				elem_type := if child.children_count > 0 {
					tc.array_literal_elem_type(child)
				} else {
					Type(int_)
				}
				return Type(ArrayFixed{
					elem_type: elem_type
					len:       child.children_count
				})
			}
			return tc.resolve_type(child_id)
		}
		.index {
			return tc.resolve_index_type(node)
		}
		.array_init {
			if node.typ.len > 0 {
				return tc.parse_type(node.typ)
			}
			t := tc.parse_type(node.value)
			raw_t := t
			if t is ArrayFixed {
				return raw_t
			}
			return Type(Array{
				elem_type: t
			})
		}
		.map_init {
			if node.value.len > 0 {
				return tc.parse_type(node.value)
			}
			if node.children_count >= 2 {
				first_id := tc.a.child(&node, 0)
				first := tc.a.nodes[int(first_id)]
				if first.kind == .prefix && first.value == '...' && first.children_count > 0 {
					update_type := tc.resolve_type(tc.a.child(&first, 0))
					if map_type_from_receiver(update_type) != none {
						return update_type
					}
					for i := 2; i + 1 < node.children_count; i += 2 {
						key_id := tc.a.child(&node, i)
						key := tc.a.node(key_id)
						if key.kind == .prefix && key.value == '...' {
							continue
						}
						value_id := tc.a.child(&node, i + 1)
						resolved_value_type := tc.resolve_type(value_id)
						mut value_type := tc.mut_param_expr_base(value_id, resolved_value_type) or {
							resolved_value_type
						}
						if value_type is ArrayFixed && !fixed_array_type_contains_map(value_type) {
							value_type = Type(Array{
								elem_type: value_type.elem_type
							})
						}
						return Type(Map{
							key_type:   tc.resolve_type(key_id)
							value_type: value_type
						})
					}
					return Type(Map{
						key_type:   Type(string_)
						value_type: Type(int_)
					})
				}
				key_type := tc.resolve_type(first_id)
				value_id := tc.a.child(&node, 1)
				resolved_value_type := tc.resolve_type(value_id)
				mut value_type := tc.mut_param_expr_base(value_id, resolved_value_type) or {
					resolved_value_type
				}
				if value_type is ArrayFixed && !fixed_array_type_contains_map(value_type) {
					value_type = Type(Array{
						elem_type: value_type.elem_type
					})
				}
				return Type(Map{
					key_type:   key_type
					value_type: value_type
				})
			}
			return Type(Map{
				key_type:   Type(string_)
				value_type: Type(int_)
			})
		}
		.comptime_if {
			take_then := tc.comptime_threads_condition_value(node.value) or {
				return unknown_type('unresolved compile-time expression condition `${node.value}`')
			}
			branch_index := if take_then { 0 } else { 1 }
			if branch_index >= node.children_count {
				return Type(void_)
			}
			return tc.resolve_type(tc.a.child(&node, branch_index))
		}
		.if_expr {
			return tc.if_expr_tail_type(id)
		}
		.lock_expr {
			if node.children_count == 0 {
				return Type(void_)
			}
			return tc.resolve_type(tc.a.child(&node, node.children_count - 1))
		}
		.match_stmt {
			return tc.match_expr_tail_type(id)
		}
		.in_expr {
			return Type(bool_)
		}
		.block {
			if node.children_count > 0 {
				last_id := tc.a.child(&node, node.children_count - 1)
				last := tc.a.nodes[int(last_id)]
				if last.kind == .expr_stmt {
					return tc.resolve_type(tc.a.child(&last, 0))
				}
				return tc.resolve_type(last_id)
			}
			return Type(void_)
		}
		.as_expr {
			return tc.parse_type(node.value)
		}
		.is_expr {
			return Type(bool_)
		}
		else {
			$if debug {
				if tc.verbose {
					eprintln('warning: unhandled node kind .${node.kind} in resolve_type')
				}
			}
			return unknown_type('unhandled node kind .${node.kind}')
		}
	}
}

// fn_literal_type supports fn literal type handling for TypeChecker.
fn (tc &TypeChecker) fn_literal_type(node flat.Node) Type {
	mut params := []Type{}
	mut params_mut := []bool{}
	mut reached_params := false
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind != .param {
			// Captures precede parameters in a function literal's flat children, so a
			// prefix-only scan must skip those identifiers, then stop once the body begins.
			if tc.prefix_param_scan && (reached_params || child.kind != .ident) {
				break
			}
			continue
		}
		reached_params = true
		params_mut << child.is_mut
		parsed := tc.parse_type(normalize_fn_type_param_text(child.typ))
		if child.value.len == 0 && child.typ.len > 0 && parsed is Unknown {
			params << Type(Struct{
				name: child.typ
			})
		} else {
			params << parsed
		}
	}
	return Type(FnType{
		params:      params
		params_mut:  params_mut
		return_type: tc.parse_type(node.typ)
	})
}

// lambda_expr_type supports lambda expr type handling for TypeChecker.
fn (tc &TypeChecker) lambda_expr_type(node flat.Node) Type {
	mut params := []Type{}
	mut params_mut := []bool{}
	if node.children_count > 0 {
		for i in 0 .. node.children_count - 1 {
			param := tc.a.child_node(&node, i)
			if param.typ.len > 0 {
				params << tc.parse_type(normalize_fn_type_param_text(param.typ))
			} else {
				params << unknown_type('lambda parameter')
			}
			params_mut << param.is_mut
		}
	}
	ret_type := if node.children_count > 0 {
		tc.resolve_type(tc.a.child(&node, node.children_count - 1))
	} else {
		Type(void_)
	}
	return Type(FnType{
		params:      params
		params_mut:  params_mut
		return_type: ret_type
	})
}

// resolve_index_type resolves resolve index type information for types.
fn (tc &TypeChecker) resolve_index_type(node flat.Node) Type {
	if generic_fn_type := tc.explicit_generic_fn_value_type(node) {
		return generic_fn_type
	}
	base_type0 := tc.resolve_type(tc.a.child(&node, 0))
	if node.value == 'range' {
		if sliced_alias := range_slice_alias_type(base_type0) {
			return sliced_alias
		}
	}
	base_type := unalias_type(base_type0)
	if base_type is OptionType {
		inner := unalias_type(base_type.base_type)
		result := tc.resolve_index_base_type(inner, node)
		if result is Unknown {
			return result
		}
		return Type(OptionType{
			base_type: result
		})
	}
	return tc.resolve_index_base_type(base_type, node)
}

// explicit_generic_fn_value_type resolves `generic_fn[ConcreteType]` when the
// specialization is used as a function value rather than called immediately.
fn (tc &TypeChecker) explicit_generic_fn_value_type(node flat.Node) ?Type {
	if node.kind != .index || node.children_count < 2 || node.value == 'range' {
		return none
	}
	base_node := tc.a.child_node(&node, 0)
	if base_node.kind == .ident && tc.ident_resolves_to_value(base_node.value) {
		return none
	}
	name := tc.generic_call_base_name(base_node) or { return none }
	type_args := tc.generic_call_type_arg_names(node)
	if type_args.len == 0 {
		return none
	}
	info := tc.explicit_generic_call_info(name, false, type_args) or { return none }
	return Type(FnType{
		params:      info.params.clone()
		return_type: info.return_type
	})
}

fn range_slice_alias_type(base_type Type) ?Type {
	if base_type is Alias {
		inner := unalias_type(base_type.base_type)
		if inner is Array || inner is String {
			return base_type
		}
	}
	if base_type is Pointer && base_type.base_type is Alias {
		inner := unalias_type(base_type.base_type.base_type)
		if inner is Array || inner is String {
			return base_type.base_type
		}
	}
	return none
}

fn (tc &TypeChecker) resolve_index_base_type(base_type Type, node flat.Node) Type {
	if node.value == 'range' {
		if base_type is Array {
			return base_type
		}
		if base_type is ArrayFixed {
			return Type(Array{
				elem_type: fixed_array_elem_type(base_type)
			})
		}
		if base_type is Pointer {
			inner0 := pointer_base_type(base_type)
			mut inner := inner0
			if inner0 is Alias {
				inner = inner0.base_type
			}
			if inner is Array {
				return inner
			}
			if inner is ArrayFixed {
				return Type(Array{
					elem_type: fixed_array_elem_type(inner)
				})
			}
		}
		return Type(string_)
	}
	return tc.resolve_index_base_value_type(base_type)
}

fn (tc &TypeChecker) resolve_index_base_value_type(base_type Type) Type {
	if base_type is Map {
		return map_value_type(base_type)
	}
	if base_type is Array {
		return array_elem_type(base_type)
	}
	if base_type is ArrayFixed {
		return fixed_array_elem_type(base_type)
	}
	if base_type is Pointer {
		inner0 := pointer_base_type(base_type)
		mut inner := inner0
		if inner0 is Alias {
			inner = inner0.base_type
		}
		if inner is Map {
			// `mut m map[K]V` params resolve to a pointer-to-map; indexing still
			// yields the value type V, not the whole map.
			return map_value_type(inner)
		}
		if inner is Array {
			return array_elem_type(inner)
		}
		if inner is ArrayFixed {
			return fixed_array_elem_type(inner)
		}
		return inner
	}
	if base_type is String {
		return Type(u8_)
	}
	if info := tc.index_overload_call_info(base_type, false) {
		return info.return_type
	}
	return unknown_type('cannot index `${base_type.name()}`')
}

// c_type supports c type handling for TypeChecker.
// c_extern_abi_type spells `t` the way a C extern declaration lowers it for ABI
// comparison: V's platform `int` stays C `int` (via prim_c_type) instead of
// widening to its value spelling (`i64` on 64-bit). This mirrors the codegen
// rule that C extern declarations keep C `int`, so two decls of the same C
// function that mix `int` and `i32` remain ABI-compatible (both are 32-bit C
// int), while a genuine `int` vs `i64` mismatch is still rejected. It recurses
// through pointers, aliases, and function types so `int` is treated as C `int`
// anywhere inside a C ABI signature. For every non-int shape it produces exactly
// what c_type would, so it never widens the set of "compatible" signatures.
fn (tc &TypeChecker) c_extern_abi_type(t Type) string {
	if t is Primitive {
		return prim_c_type(t)
	}
	if t is Pointer {
		return tc.c_extern_abi_type(t.base_type) + '*'
	}
	if t is Alias {
		return tc.c_extern_abi_type(t.base_type)
	}
	if t is FnType {
		ret := tc.c_extern_abi_type(t.return_type)
		if t.params.len == 0 {
			return 'fn_ptr:${ret}|void'
		}
		mut params := []string{}
		for i in 0 .. t.params.len {
			mut param_type := fn_param_type(t, i)
			if fn_param_is_mut(t, i) && param_type !is Pointer {
				param_type = Type(Pointer{
					base_type: param_type
				})
			}
			params << tc.c_extern_abi_type(param_type)
		}
		return 'fn_ptr:${ret}|${params.join(', ')}'
	}
	return tc.c_type(t)
}

pub fn (tc &TypeChecker) c_type(t Type) string {
	if tc.type_cache == unsafe { nil } || isnil(tc.type_interner) {
		return tc.c_type_uncached(t)
	}
	mut cache := unsafe { tc.type_cache }
	// Key on the interned semantic identity, not t.name(): distinct semantic types
	// can share a source spelling (`[size]int` with different resolved `size`, or
	// same-named aliases over different resolved bases) yet fold to different C
	// representations, so a textual key would hand back the wrong layout.
	id, canonical := tc.intern_type(t)
	slot := int(u32(id) & 2047)
	if tc.fast_c_type_recent {
		if cache.c_recent_set[slot] && cache.c_recent_ids[slot] == id {
			cache.c_hits++
			return cache.c_recent_vals[slot]
		}
	}
	if cached := cache.c_entries[id] {
		cache.c_hits++
		if tc.fast_c_type_recent {
			cache.c_recent_ids[slot] = id
			cache.c_recent_vals[slot] = cached
			cache.c_recent_set[slot] = true
		}
		return cached
	}
	cache.c_misses++
	result := tc.c_type_uncached(canonical)
	cache.c_entries[id] = result
	if tc.fast_c_type_recent {
		cache.c_recent_ids[slot] = id
		cache.c_recent_vals[slot] = result
		cache.c_recent_set[slot] = true
	}
	return result
}

// c_type_uncached supports c type uncached handling for TypeChecker.
fn (tc &TypeChecker) c_type_uncached(t Type) string {
	if t is Void {
		return 'void'
	}
	if t is Unknown {
		return 'int'
	}
	if t is Nil {
		return 'void*'
	}
	if t is None {
		return 'Optional'
	}
	if t is String {
		return 'string'
	}
	if t is Char {
		return 'char'
	}
	if t is Rune {
		return 'u32'
	}
	if t is ISize {
		return 'ptrdiff_t'
	}
	if t is USize {
		return 'size_t'
	}
	if t is Primitive {
		// V's platform `int` (a signed integer of unset size) lowers to the
		// target-width C spelling: `i64` on 64-bit targets, `i32` on 32-bit.
		// Only the emitted C type changes; `int` stays named `int` for
		// diagnostics and keeps distinct methods from `i64` (prim_c_type is
		// still `int`, so method-name mangling never collapses the two).
		if t.size == 0 && t.props.has(.integer) && !t.props.has(.unsigned) {
			return platform_int_c_type
		}
		return prim_c_type(t)
	}
	if t is Array {
		return 'Array'
	}
	if t is ArrayFixed {
		// The typedef name preserves the source length text while
		// the emitted C dimension is folded separately by fixed_array_len_value.
		len_text := if v := tc.fixed_array_len_value(t) {
			v.str()
		} else if t.len_expr.len > 0 {
			t.len_expr
		} else {
			t.len.str()
		}
		len_name := naming.type_name_part(len_text)
		return 'Array_fixed_${naming.type_name_part(tc.fixed_array_elem_c_type(t.elem_type))}_${len_name}'
	}
	if t is Channel {
		return 'chan'
	}
	if t is Map {
		return 'map'
	}
	if t is Pointer {
		return tc.c_type(t.base_type) + '*'
	}
	if t is FnType {
		ret := tc.fn_ptr_return_c_type(t.return_type)
		if t.params.len == 0 {
			return 'fn_ptr:${ret}|void'
		}
		mut params := []string{}
		for i in 0 .. t.params.len {
			mut param_type := fn_param_type(t, i)
			if fn_param_is_mut(t, i) && param_type !is Pointer {
				param_type = Type(Pointer{
					base_type: param_type
				})
			}
			if param_type is OptionType {
				params << tc.optional_c_type_name(param_type.base_type)
			} else if param_type is ResultType {
				params << tc.optional_c_type_name(param_type.base_type)
			} else {
				params << tc.c_type(param_type)
			}
		}
		return 'fn_ptr:${ret}|${params.join(', ')}'
	}
	if t is OptionType {
		return 'Optional'
	}
	if t is ResultType {
		return 'Optional'
	}
	if t is Struct {
		if t.name == 'thread' || t.name.ends_with('.thread') || t.name.starts_with('thread ') {
			return '__v_thread'
		}
		if t.name.starts_with('C.') {
			raw := t.name[2..]
			if raw.starts_with('builtin__closure__') {
				closure_name := 'closure.${raw['builtin__closure__'.len..]}'
				if closure_name in tc.structs {
					return tc.c_struct_type_name(closure_name)
				}
			}
			// A struct declared `@[typedef] struct C.foo {}` is referenced by its
			// typedef name (`foo`), never as `struct foo` — the C header (and v3's own
			// emitted `typedef struct {...} foo;`) has no matching `struct foo` tag, so
			// a `struct foo` reference would stay an incomplete type.
			if t.name in tc.c_typedef_structs {
				return raw
			}
			if raw.ends_with('_s')
				|| (raw.len > 0 && raw[0] >= `a` && raw[0] <= `z` && !raw.ends_with('_t')) {
				return 'struct ${raw}'
			}
			return raw
		}
		base, _, is_generic := generic_type_application_parts(t.name)
		if is_generic {
			if !base.contains('.') && base in tc.type_aliases {
				if module_name := tc.struct_modules[base] {
					if module_name !in ['', 'main', 'builtin'] {
						return tc.c_struct_type_name('${module_name}.${t.name}')
					}
				}
			}
		}
		if t.name.contains('.') && t.name !in tc.structs && t.name !in tc.type_aliases {
			// An import-alias prefix (`json.Any` for `import x.json2 as json`)
			// can survive in recorded types; resolve by unique short name.
			if resolved := tc.unique_qualified_type_name(t.name.all_after_last('.')) {
				return tc.c_struct_type_name(resolved)
			}
		}
		return tc.c_struct_type_name(t.name)
	}
	if t is Interface {
		base, _, is_generic := generic_type_application_parts(t.name)
		if is_generic {
			// Generic interface applications all use the base interface's runtime
			// box. Their type arguments specialize method signatures, but do not
			// introduce a distinct C struct.
			return naming.c_name(base)
		}
		return naming.c_name(t.name)
	}
	if t is Enum {
		return 'int'
	}
	if t is SumType {
		return naming.c_name(t.name)
	}
	if t is Alias {
		if tc.autofree_mode && t.name in tc.type_alias_modules
			&& tc.type_alias_modules[t.name] in ['', 'main'] && !t.name.contains('.')
			&& t.name.len > 0 && t.name[0] >= `A` && t.name[0] <= `Z` {
			return naming.c_name('main.${t.name}')
		}
		if t.base_type is Unknown && t.base_type.reason.starts_with('recursive alias `') {
			if target := tc.type_aliases[t.name] {
				clean_target := target.trim_space()
				if clean_target.starts_with('map[') {
					return 'map'
				}
				if clean_target.starts_with('[]') || clean_target.starts_with('...') {
					return 'Array'
				}
				if clean_target.starts_with('chan ') {
					return 'chan'
				}
			}
		}
		// Follow the alias chain iteratively. A self-referential / cyclic alias (whose
		// base resolves back to itself) would otherwise recurse forever here — the cache
		// is only populated after the recursive call returns — overflowing the stack.
		mut cur := Type(t)
		for _ in 0 .. 1000 {
			if cur is Alias {
				cur = cur.base_type
			} else {
				return tc.c_type(cur)
			}
		}
		return 'void*'
	}
	if t is MultiReturn {
		mut parts := []string{}
		for ty in t.types {
			parts << naming.type_name_part(tc.c_type(ty))
		}
		return 'multi_return_${parts.join('_')}'
	}
	return 'int'
}

fn (tc &TypeChecker) c_struct_type_name(name string) string {
	base, args, ok := generic_type_application_parts(name)
	if !ok {
		mut qualified_name := name
		if !name.contains('.') && name in tc.struct_modules {
			module_name := tc.struct_modules[name]
			if module_name in ['', 'main'] {
				qualified_name = 'main.${name}'
			}
		}
		cname := naming.c_name(qualified_name)
		if tc.struct_c_name_collides_with_v3_runtime(name, cname) {
			return '_v_${cname}'
		}
		return cname
	}
	mut qualified_base := base
	if !base.contains('.') && base in tc.struct_modules {
		module_name := tc.struct_modules[base]
		if module_name in ['', 'main'] {
			qualified_base = 'main.${base}'
		}
	}
	mut normalized_args := []string{cap: args.len}
	for arg in args {
		normalized_args << tc.c_generic_struct_arg_name(arg)
	}
	return naming.c_name('${qualified_base}[${normalized_args.join(', ')}]')
}

fn (tc &TypeChecker) struct_c_name_collides_with_v3_runtime(name string, cname string) bool {
	if cname !in export_v3_reserved_c_symbols || name.starts_with('C.') {
		return false
	}
	if module_name := tc.struct_modules[name] {
		return module_name != 'builtin'
	}
	return false
}

fn (tc &TypeChecker) c_generic_struct_arg_name(arg string) string {
	clean := trimmed_space(arg)
	// `voidptr` round-trips through the type model as `&void`. Keep the source ABI
	// spelling for generic instance names so materialized structs and methods agree.
	if clean == '&void' {
		return 'voidptr'
	}
	if fixed := tc.c_generic_struct_fixed_array_arg_name(clean) {
		return fixed
	}
	if target := tc.type_aliases[clean] {
		if fixed := tc.c_generic_struct_fixed_array_arg_name(target) {
			return fixed
		}
	}
	if clean.contains('.') {
		short := clean.all_after_last('.')
		if target := tc.type_aliases[clean] {
			if fixed := tc.c_generic_struct_fixed_array_arg_name(target) {
				return fixed
			}
		}
		if clean !in tc.structs && clean !in tc.struct_generic_params
			&& clean !in tc.interface_names && clean !in tc.sum_types && clean !in tc.enum_names
			&& clean !in tc.flag_enums && clean !in tc.type_aliases {
			if short in tc.structs || short in tc.struct_generic_params
				|| short in tc.interface_names || short in tc.sum_types || short in tc.enum_names
				|| short in tc.flag_enums || short in tc.type_aliases {
				return short
			}
		}
	}
	return clean
}

fn (tc &TypeChecker) c_generic_struct_fixed_array_arg_name(arg string) ?string {
	clean := trimmed_space(arg)
	if !clean.starts_with('[') {
		return none
	}
	typ := tc.parse_type(clean)
	match typ {
		ArrayFixed {
			len_text := if typ.len_expr.len > 0 { typ.len_expr } else { typ.len.str() }
			len_name := naming.type_name_part(len_text)
			elem_name := naming.type_name_part(tc.c_type(typ.elem_type))
			return '${elem_name}_${len_name}'
		}
		else {
			return none
		}
	}
}

fn (tc &TypeChecker) fixed_array_elem_c_type(t Type) string {
	if t is OptionType {
		return tc.optional_c_type_name(t.base_type)
	}
	if t is ResultType {
		return tc.optional_c_type_name(t.base_type)
	}
	if t is Pointer && t.base_type is Void {
		return 'voidptr'
	}
	return tc.c_type(t)
}

fn (tc &TypeChecker) fn_ptr_return_c_type(t Type) string {
	if t is Void {
		return 'void'
	}
	if t is OptionType {
		return tc.optional_c_type_name(t.base_type)
	}
	if t is ResultType {
		return tc.optional_c_type_name(t.base_type)
	}
	return tc.c_type(t)
}

fn (tc &TypeChecker) optional_c_type_name(base_type Type) string {
	if base_type is Void {
		return 'Optional'
	}
	mut inner_ct := tc.c_type(base_type)
	if inner_ct.starts_with('fn_ptr:') {
		inner_ct = naming.fn_ptr_type_name(inner_ct)
	}
	if inner_ct == 'int' {
		return 'Optional'
	}
	return 'Optional_${inner_ct.replace('*', 'ptr').replace(' ', '_')}'
}

// resolve_type_name_for_method resolves resolve type name for method information for types.
struct GenericReceiverMethodPatternMatch {
	key         string
	params      []string
	args        []string
	specificity int
	is_exact    bool
}

// resolve_generic_struct_method resolves a method call on a generic-struct
// instance (e.g. `Vec4[f32].r_sqrt`). The method is registered against the
// generic form (`Vec4[T].r_sqrt`); this maps the instance's concrete type
// arguments onto the generic parameters and substitutes them into the method's
// signature, so the pre-transform checker accepts the call. The transformer's
// monomorphize pass later materialises the concrete method body.
pub fn (tc &TypeChecker) resolve_generic_struct_method(type_name string, method string) ?CallInfo {
	lookup_type_name := tc.generic_struct_method_alias_target(type_name)
	bracket := lookup_type_name.index_u8(`[`)
	has_type_args := bracket > 0 && lookup_type_name.ends_with(']')
	if bracket > 0 && !has_type_args {
		return none
	}
	base := if has_type_args { lookup_type_name[..bracket] } else { lookup_type_name }
	mut concrete_args := []string{}
	if has_type_args {
		args_str := lookup_type_name[bracket + 1..lookup_type_name.len - 1]
		for a in split_generic_arg_list(args_str) {
			// Keep caller-local concrete arguments anchored to the caller. Method
			// signature text is parsed in the declaring module below, where a bare
			// `Context` would otherwise become `veb.Context` instead of
			// `main.Context` for an embedded `veb.Middleware[Context]`.
			concrete_args << tc.explicit_generic_concrete_arg_text(a.trim_space())
		}
	}
	mut generic_base := ''
	mut params := []string{}
	mut generic_key := ''
	if has_type_args {
		if matched := tc.generic_receiver_method_pattern_match(base, concrete_args, method) {
			if matched.is_exact {
				return tc.call_info(matched.key, true)
			}
			generic_base = base
			params = matched.params.clone()
			concrete_args = matched.args.clone()
			generic_key = matched.key
		}
	}
	if generic_key.len == 0 {
		for candidate in tc.generic_struct_method_base_candidates(base) {
			candidate_params := tc.struct_generic_params[candidate] or {
				tc.type_alias_generic_params[candidate] or { continue }
			}
			candidate_concrete_args := if has_type_args {
				concrete_args.clone()
			} else {
				candidate_params.clone()
			}
			if candidate_params.len == 0 || candidate_params.len != candidate_concrete_args.len {
				continue
			}
			mut method_bases := [candidate]
			module_name := tc.struct_modules[candidate] or {
				tc.type_alias_modules[candidate] or { '' }
			}
			if module_name !in ['', 'main', 'builtin'] && !candidate.contains('.') {
				method_bases << '${module_name}.${candidate}'
			}
			for method_base in method_bases {
				for method_spelling in [method, '@${method}'] {
					candidate_key := '${method_base}[${candidate_params.join(', ')}].${method_spelling}'
					if candidate_key in tc.fn_ret_types {
						generic_base = candidate
						params = generic_method_receiver_params_from_key(candidate_key,
							candidate_params)
						concrete_args = candidate_concrete_args.clone()
						generic_key = candidate_key
						break
					}
					plain_candidate_key := '${method_base}.${method_spelling}'
					if plain_candidate_key in tc.fn_ret_types {
						generic_base = candidate
						params = candidate_params.clone()
						concrete_args = candidate_concrete_args.clone()
						generic_key = plain_candidate_key
						break
					}
				}
				if generic_key.len > 0 {
					break
				}
				for method_spelling in [method, '@${method}'] {
					if indexed := tc.receiver_method_suffix_index['${method_base}.${method_spelling}'] {
						if indexed != receiver_method_suffix_ambiguous && indexed in tc.fn_ret_types
							&& indexed.all_before_last('.').contains('[') {
							mut indexed_params := generic_method_receiver_params_from_key(indexed,
								candidate_params)
							mut indexed_args := candidate_concrete_args.clone()
							if has_type_args {
								indexed_params, indexed_args = tc.generic_method_receiver_pattern_args(indexed,
									candidate_concrete_args) or { continue }
								if indexed_args.any(tc.generic_receiver_arg_is_voidptr(it)) {
									continue
								}
							}
							generic_base = candidate
							params = indexed_params.clone()
							concrete_args = indexed_args.clone()
							generic_key = indexed
							break
						}
					}
				}
				if generic_key.len > 0 {
					break
				}
			}
			if generic_key.len > 0 {
				break
			}
		}
	}
	if generic_base.len == 0 {
		return none
	}
	if params.len == 0 || params.len != concrete_args.len {
		return none
	}
	// Resolve arguments in the caller's file/module before parsing any part of
	// the generic declaration's signature. A caller-local alias such as `Pair`
	// must not be reinterpreted as `gr.Pair` in the method's declaring module.
	mut concrete_types := []Type{cap: concrete_args.len}
	for arg in concrete_args {
		concrete_types << tc.parse_type(arg)
	}
	ret := tc.fn_ret_types[generic_key] or { return none }
	// Prefer substituting the original signature TEXT: parsing `Box[T]` collapses the
	// non-concrete application to the bare `Box`, so substituting the already-parsed type
	// cannot recover `Box[int]`. Re-substituting the text and re-parsing does.
	mut sub_ret := tc.substitute_generic_type(ret, concrete_args, params)
	if generic_semantic_type_has_placeholder(ret) {
		sub_ret = tc.substitute_generic_type_values(ret, concrete_types, params)
	} else if ret_text := tc.fn_ret_type_texts[generic_key] {
		sub_ret = tc.parse_fn_signature_type(generic_key, subst_generic_text(ret_text,
			concrete_args, params))
	}
	mut sub_params := []Type{}
	if param_texts := tc.fn_param_type_texts[generic_key] {
		param_types := tc.fn_param_types[generic_key] or { []Type{} }
		for i, pt in param_texts {
			receiver_clean := trimmed_space(pt).trim_left('&').trim_space()
			receiver_base := if receiver_clean.contains('[') {
				receiver_clean.all_before('[')
			} else {
				receiver_clean
			}
			if i == 0 && (receiver_base == generic_base
				|| receiver_base == generic_base.all_after_last('.')
				|| receiver_base == base
				|| receiver_base == base.all_after_last('.')) {
				sub_params << tc.generic_method_receiver_param(type_name, pt)
				continue
			}
			if i < param_types.len && generic_semantic_type_has_placeholder(param_types[i]) {
				sub_params << tc.substitute_generic_type_values(param_types[i], concrete_types,
					params)
				continue
			}
			sub_params << tc.parse_fn_signature_type(generic_key, subst_generic_text(pt,
				concrete_args, params))
		}
	} else if ptypes := tc.fn_param_types[generic_key] {
		for pt in ptypes {
			sub_params << tc.substitute_generic_type(pt, concrete_args, params)
		}
	}
	return CallInfo{
		name:          generic_key
		params:        sub_params
		shared_params: tc.fn_shared_params[generic_key] or { []bool{} }
		return_type:   sub_ret
		has_receiver:  true
		is_variadic:   tc.fn_variadic[generic_key] or { false }
		params_known:  true
	}
}

fn (tc &TypeChecker) generic_struct_method_alias_target(type_name string) string {
	mut current := type_name
	mut seen := map[string]bool{}
	for _ in 0 .. 16 {
		if seen[current] {
			break
		}
		seen[current] = true
		qualified := tc.qualify_name(current)
		target := tc.type_aliases[current] or { tc.type_aliases[qualified] or { break } }
		if target == current {
			break
		}
		current = target
	}
	return current
}

fn (tc &TypeChecker) generic_receiver_method_pattern_match(base string, actual_args []string, method string) ?GenericReceiverMethodPatternMatch {
	cache_key := '${tc.cur_file}\x00${tc.cur_module}\x00${tc.fn_context.generic_params.join(',')}\x00${base}\x00${actual_args.join(',')}\x00${method}'
	if !isnil(tc.type_cache) {
		cache := tc.type_cache
		if matched := cache.recv_pattern_entries[cache_key] {
			return matched
		}
		if cache.recv_pattern_misses[cache_key] {
			return none
		}
	}
	mut best := GenericReceiverMethodPatternMatch{}
	mut found := false
	for method_spelling in [method, '@${method}'] {
		candidates := tc.generic_receiver_method_index[method_spelling] or { continue }
		for key in candidates {
			receiver := key.all_before_last('.')
			pattern_base, patterns, is_generic := generic_type_application_parts(receiver)
			if !is_generic || patterns.len != actual_args.len
				|| !tc.generic_type_base_matches(pattern_base, base) {
				continue
			}
			mut is_exact := true
			for i, pattern in patterns {
				if tc.parse_type(trimmed_space(pattern)).name() != tc.parse_type(trimmed_space(actual_args[i])).name() {
					is_exact = false
					break
				}
			}
			mut params := []string{}
			mut inferred_args := []string{}
			mut specificity := 1_000_000
			if !is_exact {
				if patterns.all(is_bare_generic_param(trimmed_space(it))) {
					continue
				}
				params, inferred_args = tc.generic_method_receiver_pattern_args(key, actual_args) or {
					continue
				}
				specificity = 0
				for pattern in patterns {
					if !is_bare_generic_param(trimmed_space(pattern)) {
						specificity += 10_000 + pattern.len
					}
				}
			}
			if !found || specificity > best.specificity
				|| (specificity == best.specificity && key < best.key) {
				best = GenericReceiverMethodPatternMatch{
					key:         key
					params:      params
					args:        inferred_args
					specificity: specificity
					is_exact:    is_exact
				}
				found = true
			}
		}
	}
	if !found {
		if !isnil(tc.type_cache) {
			mut cache := tc.type_cache
			cache.recv_pattern_misses[cache_key] = true
		}
		return none
	}
	if !isnil(tc.type_cache) {
		mut cache := tc.type_cache
		cache.recv_pattern_entries[cache_key] = best
	}
	return best
}

fn (tc &TypeChecker) generic_method_receiver_pattern_args(key string, actual_args []string) ?([]string, []string) {
	receiver := key.all_before_last('.')
	_, patterns, is_generic := generic_type_application_parts(receiver)
	if !is_generic || patterns.len != actual_args.len {
		return none
	}
	mut counts := map[string]int{}
	for pattern in patterns {
		tc.collect_generic_param_candidates(pattern, mut counts)
	}
	if counts.len == 0 {
		return none
	}
	mut params := counts.keys()
	params.sort()
	mut inferred := map[string]string{}
	for i, pattern in patterns {
		tc.infer_generic_type_text_from_text(pattern, actual_args[i], params, mut inferred)
	}
	mut concrete_args := []string{cap: params.len}
	for param in params {
		concrete_args << inferred[param] or { return none }
	}
	for i, pattern in patterns {
		substituted := subst_generic_text(pattern, concrete_args, params)
		if tc.parse_type(substituted).name() != tc.parse_type(actual_args[i]).name() {
			return none
		}
	}
	return params, concrete_args
}

fn (tc &TypeChecker) generic_receiver_arg_is_voidptr(arg string) bool {
	clean := trimmed_space(arg)
	if clean == 'voidptr' {
		return true
	}
	return tc.parse_type(clean).name() in ['voidptr', '&void']
}

fn (tc &TypeChecker) generic_receiver_has_structured_method_pattern(type_name string, method string) bool {
	base, _, is_generic := generic_type_application_parts(type_name)
	if !is_generic {
		return false
	}
	candidates := tc.generic_receiver_method_index[method] or { return false }
	for key in candidates {
		receiver := key.all_before_last('.')
		pattern_base, patterns, is_pattern := generic_type_application_parts(receiver)
		if !is_pattern || !tc.generic_type_base_matches(pattern_base, base) {
			continue
		}
		if patterns.any(!is_bare_generic_param(trimmed_space(it))) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) generic_receiver_method_rejects_voidptr(type_name string, method string) bool {
	base, actual_args, is_generic := generic_type_application_parts(type_name)
	if !is_generic {
		return false
	}
	candidates := tc.generic_receiver_method_index[method] or { return false }
	for key in candidates {
		receiver := key.all_before_last('.')
		pattern_base, _, is_pattern := generic_type_application_parts(receiver)
		if !is_pattern || !tc.generic_type_base_matches(pattern_base, base) {
			continue
		}
		_, inferred := tc.generic_method_receiver_pattern_args(key, actual_args) or { continue }
		if inferred.any(tc.generic_receiver_arg_is_voidptr(it)) {
			return true
		}
	}
	return false
}

fn generic_method_receiver_params_from_key(key string, fallback []string) []string {
	receiver := key.all_before_last('.')
	bracket := receiver.index_u8(`[`)
	if bracket <= 0 || !receiver.ends_with(']') {
		return fallback.clone()
	}
	mut params := []string{}
	for param in split_generic_arg_list(receiver[bracket + 1..receiver.len - 1]) {
		params << trimmed_space(param)
	}
	if params.len != fallback.len {
		return fallback.clone()
	}
	return params
}

fn generic_semantic_type_has_placeholder(typ Type) bool {
	match typ {
		Unknown {
			return generic_placeholder_from_unknown(typ) != none
		}
		Array {
			return generic_semantic_type_has_placeholder(typ.elem_type)
		}
		ArrayFixed {
			return generic_semantic_type_has_placeholder(typ.elem_type)
		}
		Channel {
			return generic_semantic_type_has_placeholder(typ.elem_type)
		}
		Map {
			return generic_semantic_type_has_placeholder(typ.key_type)
				|| generic_semantic_type_has_placeholder(typ.value_type)
		}
		Pointer {
			return generic_semantic_type_has_placeholder(typ.base_type)
		}
		OptionType {
			return generic_semantic_type_has_placeholder(typ.base_type)
		}
		ResultType {
			return generic_semantic_type_has_placeholder(typ.base_type)
		}
		FnType {
			for param in typ.params {
				if generic_semantic_type_has_placeholder(param) {
					return true
				}
			}
			return generic_semantic_type_has_placeholder(typ.return_type)
		}
		MultiReturn {
			for part in typ.types {
				if generic_semantic_type_has_placeholder(part) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn (tc &TypeChecker) generic_struct_method_base_candidates(base string) []string {
	mut candidates := []string{}
	push_receiver_method_candidate(mut candidates, base)
	if base.contains('.') {
		resolved := tc.resolve_imported_type_text(base)
		push_receiver_method_candidate(mut candidates, resolved)
		push_receiver_method_candidate(mut candidates, short_name_view(resolved))
		push_receiver_method_candidate(mut candidates, short_name_view(base))
	} else {
		if resolved := tc.resolve_selective_import_type_symbol(base) {
			push_receiver_method_candidate(mut candidates, resolved)
			push_receiver_method_candidate(mut candidates, short_name_view(resolved))
		}
		qualified := tc.qualify_name(base)
		push_receiver_method_candidate(mut candidates, qualified)
	}
	return candidates
}

pub fn (tc &TypeChecker) resolve_generic_sum_method(type_name string, method string) ?CallInfo {
	mut base := type_name
	mut concrete_args := []string{}
	parsed_base, parsed_args, is_generic := generic_type_application_parts(type_name)
	if is_generic {
		base = parsed_base
		for arg in parsed_args {
			concrete_args << trimmed_space(arg)
		}
	}
	params := tc.sum_params_for_base(base)
	if params.len == 0 {
		return none
	}
	if !is_generic {
		concrete_args = params.clone()
	}
	if params.len != concrete_args.len {
		return none
	}
	mut generic_key := ''
	for method_spelling in [method, '@${method}'] {
		candidate := '${base}[${params.join(', ')}].${method_spelling}'
		if candidate in tc.fn_ret_types {
			generic_key = candidate
			break
		}
	}
	if generic_key.len == 0 {
		return none
	}
	ret := tc.fn_ret_types[generic_key] or { return none }
	mut sub_ret := tc.substitute_generic_type(ret, concrete_args, params)
	if ret_text := tc.fn_ret_type_texts[generic_key] {
		sub_ret = tc.parse_fn_signature_type(generic_key, subst_generic_text(ret_text,
			concrete_args, params))
	}
	mut sub_params := []Type{}
	if param_texts := tc.fn_param_type_texts[generic_key] {
		for pt in param_texts {
			sub_params << tc.parse_fn_signature_type(generic_key, subst_generic_text(pt,
				concrete_args, params))
		}
	} else if ptypes := tc.fn_param_types[generic_key] {
		for pt in ptypes {
			sub_params << tc.substitute_generic_type(pt, concrete_args, params)
		}
	}
	return CallInfo{
		name:          generic_key
		params:        sub_params
		shared_params: tc.fn_shared_params[generic_key] or { []bool{} }
		return_type:   sub_ret
		has_receiver:  true
		is_variadic:   tc.fn_variadic[generic_key] or { false }
		params_known:  true
	}
}

fn (tc &TypeChecker) generic_method_receiver_param(type_name string, param_text string) Type {
	receiver_type := tc.parse_type(type_name)
	if trimmed_space(param_text).starts_with('&') {
		return Type(Pointer{
			base_type: receiver_type
		})
	}
	return receiver_type
}

// subst_generic_text textually substitutes the generic parameter names `params` with the
// concrete argument texts `args` inside a type text, preserving the generic application
// form so a method signature mentioning the receiver type (`Box[T]`) becomes the concrete
// instance (`Box[int]`) when re-parsed, instead of collapsing to the bare base. Prefix and
// container forms (`&`, `mut`, `?`, `!`, `...`, `shared`, `atomic`, `chan`, `thread`, `[]`,
// `map[`, `[N]`) recurse into the element type; a bare parameter name is replaced with its
// argument.
fn subst_generic_text(typ string, args []string, params []string) string {
	clean := trimmed_space(typ)
	if clean.len == 0 || args.len == 0 || params.len != args.len {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + subst_generic_text(clean[1..], args, params)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + subst_generic_text(clean[4..], args, params)
	}
	if clean.starts_with('?') {
		inner := subst_generic_text(clean[1..], args, params)
		return if inner.starts_with('?') { inner } else { '?' + inner }
	}
	if clean.starts_with('!') {
		inner := subst_generic_text(clean[1..], args, params)
		return if inner.starts_with('!') { inner } else { '!' + inner }
	}
	if clean.starts_with('...') {
		return '...' + subst_generic_text(clean[3..], args, params)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + subst_generic_text(clean[7..], args, params)
	}
	for prefix in ['atomic ', 'chan ', 'thread '] {
		if clean.starts_with(prefix) {
			return prefix + subst_generic_text(clean[prefix.len..], args, params)
		}
	}
	if clean.starts_with('[]') {
		return '[]' + subst_generic_text(clean[2..], args, params)
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in split_params(clean[1..clean.len - 1]) {
			parts << subst_generic_text(part, args, params)
		}
		return '(${parts.join(', ')})'
	}
	if clean.starts_with('map[') {
		bracket_end := find_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := subst_generic_text(clean[4..bracket_end], args, params)
			val := subst_generic_text(clean[bracket_end + 1..], args, params)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := find_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + subst_generic_text(clean[bracket_end +
				1..], args, params)
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		// A function-type parameter (`fn (T) int`) carries the generic params in its own
		// signature; substitute each parameter and the return type so a `Box[string].apply`
		// callback is expected as `fn (string) int`, not the unsubstituted `fn (T) int`.
		params_start := clean.index_u8(`(`) + 1
		mut depth := 1
		mut params_end := params_start
		for params_end < clean.len {
			if clean[params_end] == `(` {
				depth++
			} else if clean[params_end] == `)` {
				depth--
				if depth == 0 {
					break
				}
			}
			params_end++
		}
		if params_end < clean.len {
			mut fn_parts := []string{}
			params_str := clean[params_start..params_end]
			if trimmed_space(params_str).len > 0 {
				for part in split_params(params_str) {
					fn_parts << subst_generic_text(normalize_fn_type_param_text(part), args, params)
				}
			}
			ret_str := trimmed_space(clean[params_end + 1..])
			if ret_str.len > 0 {
				return 'fn(${fn_parts.join(', ')}) ${subst_generic_text(ret_str, args, params)}'
			}
			return 'fn(${fn_parts.join(', ')})'
		}
	}
	bracket := clean.index_u8(`[`)
	if bracket > 0 {
		bracket_end := find_matching_bracket(clean, bracket)
		if bracket_end < clean.len {
			mut parts := []string{}
			for part in split_params(clean[bracket + 1..bracket_end]) {
				parts << subst_generic_text(part, args, params)
			}
			return clean[..bracket] + '[' + parts.join(', ') + ']' + clean[bracket_end + 1..]
		}
	}
	for i, p in params {
		if clean == p {
			return args[i]
		}
	}
	return clean
}

// split_generic_arg_list splits a comma-separated generic argument list at the
// top bracket level (so nested types like `map[int]string` stay intact).
fn split_generic_arg_list(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c == `[` || c == `(` {
			depth++
		} else if c == `]` || c == `)` {
			depth--
		} else if c == `,` && depth == 0 {
			parts << s[start..i]
			start = i + 1
		}
	}
	parts << s[start..]
	return parts
}

fn resolve_type_name_for_method(t Type) string {
	if t is Alias {
		return t.name
	}
	if t is Struct {
		return t.name
	}
	if t is Interface {
		return t.name
	}
	if t is SumType {
		return t.name
	}
	if t is Enum {
		return t.name
	}
	if t is String {
		return 'string'
	}
	if t is Char {
		return 'char'
	}
	if t is Rune {
		return 'rune'
	}
	if t is ISize {
		return 'isize'
	}
	if t is USize {
		return 'usize'
	}
	if t is Array {
		return '[]${nested_type_name(t.elem_type)}'
	}
	if t is ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		return '${nested_type_name(t.elem_type)}[${len_text}]'
	}
	if t is Map {
		return 'map[${nested_type_name(t.key_type)}]${nested_type_name(t.value_type)}'
	}
	if t is Primitive {
		return prim_name(t)
	}
	return ''
}

// ownership_type_has_clone_method reports whether typ declares a handwritten clone method.
// It is kept in the always-built checker surface because ownership transform support is
// compiled into the V executable even when the executable itself is built without ownership.
pub fn (tc &TypeChecker) ownership_type_has_clone_method(typ Type) bool {
	name := resolve_type_name_for_method(typ)
	if name.len == 0 {
		return false
	}
	if _ := tc.resolve_generic_struct_method(name, 'clone') {
		return true
	}
	for method_name in receiver_method_name_candidates(typ, 'clone', tc.cur_module) {
		if method_name in tc.fn_ret_types {
			return true
		}
	}
	return false
}

fn receiver_type_name_variant(t Type, fixed_array_prefix bool, shorten_modules bool) string {
	if t is Alias {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Struct {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Interface {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is SumType {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is Enum {
		return receiver_leaf_type_name(t.name, shorten_modules)
	}
	if t is String {
		return 'string'
	}
	if t is Array {
		return '[]${receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules)}'
	}
	if t is ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		elem := receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules)
		if fixed_array_prefix {
			return '[${len_text}]${elem}'
		}
		return '${elem}[${len_text}]'
	}
	if t is Map {
		key := receiver_type_name_variant(t.key_type, fixed_array_prefix, shorten_modules)
		value := receiver_type_name_variant(t.value_type, fixed_array_prefix, shorten_modules)
		return 'map[${key}]${value}'
	}
	if t is Primitive {
		return prim_c_type_from(t.props, t.size)
	}
	return receiver_leaf_type_name(t.name(), shorten_modules)
}

fn receiver_leaf_type_name(name string, shorten_modules bool) string {
	if shorten_modules && name.contains('.') {
		return name.all_after_last('.')
	}
	return name
}

fn receiver_type_name_variants(t Type) []string {
	mut names := []string{}
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, false, false))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, false, true))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, true, false))
	push_receiver_method_candidate(mut names, receiver_type_name_variant(t, true, true))
	return names
}

fn receiver_type_module_names(t Type) []string {
	mut names := []string{}
	if t is Array {
		for name in receiver_type_module_names(t.elem_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else if t is ArrayFixed {
		for name in receiver_type_module_names(t.elem_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else if t is Map {
		for name in receiver_type_module_names(t.key_type) {
			push_receiver_method_candidate(mut names, name)
		}
		for name in receiver_type_module_names(t.value_type) {
			push_receiver_method_candidate(mut names, name)
		}
	} else {
		name := t.name()
		if name.contains('.') {
			push_receiver_method_candidate(mut names, name.all_before_last('.'))
		}
	}
	return names
}

fn push_receiver_method_candidate(mut names []string, name string) {
	if name.len > 0 && name !in names {
		names << name
	}
}

fn (tc &TypeChecker) unique_receiver_method_suffix_match(candidates []string) ?string {
	mut found := ''
	for candidate in candidates {
		name := tc.receiver_method_suffix_index[candidate] or { continue }
		if name == receiver_method_suffix_ambiguous {
			return none
		}
		if found.len > 0 && found != name {
			return none
		}
		found = name
	}
	if found.len == 0 {
		return none
	}
	return found
}

fn module_can_prefix_collection_receiver(module_name string) bool {
	return module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
}

fn exact_array_receiver_method_candidates(t Array, method string, module_name string) []string {
	mut names := []string{}
	append_array_receiver_method_candidates(mut names, t, method, module_name)
	return names
}

fn append_array_receiver_method_candidates(mut names []string, t Array, method string, module_name string) {
	elem_types := receiver_type_name_variants(t.elem_type)
	if elem_types.len == 0 {
		return
	}
	for elem_type in elem_types {
		push_receiver_method_candidate(mut names, '[]${elem_type}.${method}')
	}
	mut module_names := receiver_type_module_names(t.elem_type)
	if module_can_prefix_collection_receiver(module_name) {
		push_receiver_method_candidate(mut module_names, module_name)
	}
	for mod_name in module_names {
		for elem_type in elem_types {
			push_receiver_method_candidate(mut names, '${mod_name}.[]${elem_type}.${method}')
		}
	}
}

fn append_map_receiver_method_candidates(mut names []string, t Map, method string, module_name string) {
	key_types := receiver_type_name_variants(t.key_type)
	value_types := receiver_type_name_variants(t.value_type)
	if key_types.len == 0 || value_types.len == 0 {
		return
	}
	mut map_types := []string{}
	for key_type in key_types {
		for value_type in value_types {
			push_receiver_method_candidate(mut map_types, 'map[${key_type}]${value_type}')
		}
	}
	for map_type in map_types {
		push_receiver_method_candidate(mut names, '${map_type}.${method}')
	}
	mut module_names := []string{}
	if module_can_prefix_collection_receiver(module_name) {
		push_receiver_method_candidate(mut module_names, module_name)
	}
	for mod_name in receiver_type_module_names(t.key_type) {
		push_receiver_method_candidate(mut module_names, mod_name)
	}
	for mod_name in receiver_type_module_names(t.value_type) {
		push_receiver_method_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			push_receiver_method_candidate(mut names, '${mod_name}.${map_type}.${method}')
		}
	}
}

fn receiver_method_name_candidates(t Type, method string, module_name string) []string {
	mut names := []string{}
	type_name := resolve_type_name_for_method(t)
	if type_name.len > 0 {
		push_receiver_method_candidate(mut names, '${type_name}.${method}')
		push_receiver_method_candidate(mut names, '${type_name}.@${method}')
	}
	mut clean := t
	if clean is Alias {
		clean = clean.base_type
	}
	if clean is Array {
		append_array_receiver_method_candidates(mut names, clean, method, module_name)
		array_name := 'array.${method}'
		push_receiver_method_candidate(mut names, array_name)
	}
	if clean is Map {
		append_map_receiver_method_candidates(mut names, clean, method, module_name)
		map_name := 'map.${method}'
		push_receiver_method_candidate(mut names, map_name)
	}
	return names
}

struct InfixOperatorSignature {
	return_type Type
	param_type  Type
	param_count int
}

fn (tc &TypeChecker) infix_operator_signature(op flat.Op, lhs Type) ?InfixOperatorSignature {
	op_name := infix_operator_name(op) or { return none }
	mut receiver_types := [unwrap_pointer(lhs)]
	if receiver_types[0] is Alias {
		alias_type := receiver_types[0] as Alias
		if target := tc.alias_target_type_text(alias_type.name) {
			receiver_types << tc.parse_type(target)
		}
		receiver_types << alias_type.base_type
	}
	for receiver_type in receiver_types {
		lhs_name := resolve_type_name_for_method(receiver_type)
		if lhs_name.len == 0 {
			continue
		}
		_, _, is_concrete_generic := generic_type_application_parts(lhs_name)
		if is_concrete_generic {
			if info := tc.resolve_generic_struct_method(lhs_name, op_name) {
				if info.params.len > 0 && (tc.receiver_compatible(lhs, info.params[0])
					|| tc.receiver_compatible(receiver_type, info.params[0])) {
					return InfixOperatorSignature{
						return_type: info.return_type
						param_type:  if info.params.len > 1 { info.params[1] } else { Type(void_) }
						param_count: info.params.len
					}
				}
			}
		}
		method_name := '${lhs_name}.${op_name}'
		if ret := tc.fn_ret_types[method_name] {
			if params := tc.fn_param_types[method_name] {
				if params.len > 0 && (tc.receiver_compatible(lhs, params[0])
					|| tc.receiver_compatible(receiver_type, params[0])) {
					return InfixOperatorSignature{
						return_type: ret
						param_type:  if params.len > 1 { params[1] } else { Type(void_) }
						param_count: params.len
					}
				}
			}
		}
		if !is_concrete_generic {
			info := tc.resolve_generic_struct_method(lhs_name, op_name) or { continue }
			if info.params.len > 0 && (tc.receiver_compatible(lhs, info.params[0])
				|| tc.receiver_compatible(receiver_type, info.params[0])) {
				return InfixOperatorSignature{
					return_type: info.return_type
					param_type:  if info.params.len > 1 { info.params[1] } else { Type(void_) }
					param_count: info.params.len
				}
			}
		}
	}
	return none
}

fn (tc &TypeChecker) infix_operator_return_type(op flat.Op, lhs Type, rhs Type) ?Type {
	signature := tc.infix_operator_signature(op, lhs) or { return none }
	if signature.param_count < 2 || !tc.type_compatible(rhs, signature.param_type) {
		return none
	}
	return signature.return_type
}

fn (tc &TypeChecker) type_has_infix_operator_method(typ Type, op flat.Op) bool {
	op_name := infix_operator_name(op) or { return false }
	type_name := resolve_type_name_for_method(unwrap_pointer(typ))
	if type_name.len == 0 {
		return false
	}
	if _ := tc.resolve_generic_struct_method(type_name, op_name) {
		return true
	}
	method_name := '${type_name}.${op_name}'
	return method_name in tc.fn_ret_types
}

fn (tc &TypeChecker) int_literal_promoted_infix_type(lit_id flat.NodeId, other_id flat.NodeId, other_type Type) ?Type {
	if tc.int_literal_value(lit_id) == none || tc.int_literal_value(other_id) != none {
		return none
	}
	value := tc.int_literal_value(lit_id)?
	clean_type := unalias_type(other_type)
	if unsigned_type_accepts_int_literal(clean_type, value) {
		return clean_type
	}
	// An untyped integer literal adopts the other integer operand's concrete storage
	// type. In particular, `24 * time.hour` is `i64`, not `int`; map literal
	// inference relies on that distinction when it chooses its value type.
	if clean_type.is_integer() && clean_type.name() != Type(int_).name() {
		return clean_type
	}
	return none
}

fn (tc &TypeChecker) int_literal_value(id flat.NodeId) ?int {
	node := tc.a.node(id)
	if node.kind == .int_literal {
		return v_int_literal_value(node.value)
	}
	if node.kind == .prefix && node.op in [.minus, .plus] && node.children_count > 0 {
		value := tc.int_literal_value(tc.a.child(node, 0))?
		return if node.op == .minus { -value } else { value }
	}
	return none
}

fn unsigned_type_accepts_int_literal(t Type, value int) bool {
	if value < 0 {
		return false
	}
	if t is Primitive {
		if !t.props.has(.integer) || !t.props.has(.unsigned) {
			return false
		}
		max := match t.size {
			8 { 255 }
			16 { 65535 }
			else { return true }
		}

		return value <= max
	}
	return false
}

fn type_is_f32(t Type) bool {
	if t is Primitive {
		return t.props.has(.float) && t.size == 32
	}
	return false
}

fn infix_operator_name(op flat.Op) ?string {
	match op {
		.plus { return '+' }
		.minus { return '-' }
		.mul { return '*' }
		.power { return '**' }
		.div { return '/' }
		.mod { return '%' }
		.amp { return '&' }
		.pipe { return '|' }
		.xor { return '^' }
		.left_shift { return '<<' }
		.right_shift { return '>>' }
		.right_shift_unsigned { return '>>>' }
		.eq { return '==' }
		.ne { return '!=' }
		.lt { return '<' }
		.gt { return '>' }
		.le { return '<=' }
		.ge { return '>=' }
		.logical_and { return '&&' }
		.logical_or { return '||' }
		.arrow { return '<-' }
		else {}
	}

	return none
}

// prim_c_type_from supports prim c type from handling for types.
fn prim_c_type_from(props Properties, size u8) string {
	if props.has(.boolean) {
		return 'bool'
	}
	if props.has(.integer) {
		if props.has(.unsigned) {
			return match size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${size}' }
			}
		}
		return match size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${size}' }
		}
	}
	if props.has(.float) {
		return match size {
			32 { 'float' }
			64 { 'double' }
			else { 'double' }
		}
	}
	return 'int'
}

// prim_c_type supports prim c type handling for types.
fn prim_c_type(p Primitive) string {
	if p.props.has(.boolean) {
		return 'bool'
	}
	if p.props.has(.integer) {
		if p.props.has(.unsigned) {
			return match p.size {
				8 { 'u8' }
				16 { 'u16' }
				32 { 'u32' }
				64 { 'u64' }
				else { 'u${p.size}' }
			}
		}
		return match p.size {
			0 { 'int' }
			8 { 'i8' }
			16 { 'i16' }
			32 { 'i32' }
			64 { 'i64' }
			else { 'i${p.size}' }
		}
	}
	if p.props.has(.float) {
		return match p.size {
			32 { 'float' }
			64 { 'double' }
			else { 'double' }
		}
	}
	return 'int'
}

// find_matching_bracket resolves find matching bracket information for types.
fn find_matching_bracket(s string, start int) int {
	mut depth := 1
	for i := start + 1; i < s.len; i++ {
		if s[i] == `[` {
			depth++
		}
		if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

// split_params supports split params handling for types.
fn split_params(s string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			`,` {
				if depth == 0 {
					parts << s[start..i]
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < s.len {
		parts << s[start..]
	}
	return parts
}

// normalize_fn_type_param_text transforms normalize fn type param text data for types.
fn normalize_fn_type_param_text(param string) string {
	mut text := trimmed_space(param)
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = trimmed_space(text[4..])
	}
	space := top_level_space_index(text)
	if space > 0 {
		head := trimmed_space(text[..space])
		tail := trimmed_space(text[space + 1..])
		if fn_type_param_head_is_name(head, tail) {
			text = tail
		}
	}
	if text.starts_with('mut ') {
		is_mut = true
		text = trimmed_space(text[4..])
	}
	if text.len > 0 {
		for marker in ['[]', '&', 'map[', 'fn(', 'fn ('] {
			marker_idx := text.index(marker) or { continue }
			if marker_idx <= 0 {
				continue
			}
			head := trimmed_space(text[..marker_idx])
			tail := trimmed_space(text[marker_idx..])
			if fn_type_param_head_is_name(head, tail) {
				text = tail
			}
			break
		}
	}
	if is_mut && text.len > 0 && !text.starts_with('&') {
		return '&' + text
	}
	return text
}

// top_level_space_index supports top level space index handling for types.
fn top_level_space_index(s string) int {
	mut depth := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`(`, `[` {
				depth++
			}
			`)`, `]` {
				depth--
			}
			` ` {
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return -1
}

// fn_type_param_head_is_name supports fn type param head is name handling for types.
fn fn_type_param_head_is_name(head string, tail string) bool {
	if head.len == 0 || tail.len == 0 {
		return false
	}
	if head.starts_with('fn') || head.starts_with('&') || head.starts_with('[') {
		return false
	}
	if head in ['shared', 'atomic', 'chan', 'thread', 'map'] || head.contains('.') {
		return false
	}
	if is_builtin_type_name(head) {
		return false
	}
	return (head[0] >= `a` && head[0] <= `z`) || head[0] == `_`
}
