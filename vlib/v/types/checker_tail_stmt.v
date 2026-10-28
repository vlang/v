module types

import os
import strconv
import strings
import v.flat
import v.gen.c.naming
import v.token
import v.util

const ierror_source_error_name = flat.source_type_id('Error')
const ierror_source_message_error_name = flat.source_type_id('MessageError')
const ierror_source_builtin_error_name = flat.source_type_id('builtin.Error')
const ierror_source_builtin_message_error_name = flat.source_type_id('builtin.MessageError')
const ierror_source_interface_name = flat.source_type_id('IError')
const ierror_source_builtin_interface_name = flat.source_type_id('builtin.IError')
const checker_source_slice_index = flat.source_type_id('SliceIndex')
const checker_source_builtin_slice_index = flat.source_type_id('builtin.SliceIndex')
const checker_source_thread = flat.source_type_id('thread')
const checker_source_void_reference = flat.source_type_id('&void')
const checker_source_option_marker = flat.source_type_id('?')
const checker_tail_source_struct = flat.source_type_id('struct')
const checker_tail_source_embed_file_data = flat.source_type_id('embed_file.EmbedFileData')
const checker_tail_source_chan = flat.source_type_id('chan')
const checker_tail_source_thread_void = flat.source_type_id('thread void')
const checker_tail_source_thread_result_marker = flat.source_type_id('thread !')
const checker_tail_source_thread_result_void = flat.source_type_id('thread !void')
const checker_tail_source_thread_option_marker = flat.source_type_id('thread ?')
const checker_tail_source_thread_option_void = flat.source_type_id('thread ?void')

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

@[direct_array_access]
fn (mut tc TypeChecker) mark_statement_context(id flat.NodeId) {
	mut current := id
	for tc.valid_node_id(current) {
		idx := int(current)
		if !isnil(tc.storage_query_probe) {
			tc.sparse_statement_nodes[idx] = true
		} else if tc.parallel_check_sparse {
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
	if !isnil(tc.storage_query_probe) {
		if recorded := tc.sparse_statement_nodes[idx] { return recorded }
		if !isnil(tc.storage_query_probe.read_base) {
			return tc.storage_query_probe.read_base.is_statement_node(id)
		}
		return false
	}
	if tc.parallel_check_sparse {
		if tc.in_check_range(idx) {
			return idx < tc.statement_nodes.len && tc.statement_nodes[idx]
		}
		if recorded := tc.sparse_statement_nodes[idx] { return recorded }
		return false
	}
	return idx >= 0 && idx < tc.statement_nodes.len && tc.statement_nodes[idx]
}

fn (tc &TypeChecker) expression_node_used_as_value(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 64 {
		idx := int(current)
		current_kind := tc.a.node(current).kind
		if current_kind !in [.paren, .block, .match_branch, .if_expr, .match_stmt, .or_expr]
			&& idx >= 0
			&& idx < tc.value_used_nodes.len && tc.value_used_nodes[idx] {
			return true
		}
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) || parent_id == current {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .lambda_expr {
			// `|x| if c { x } else { 0 }`: the body is the lambda's result, unless the
			// lambda returns nothing and its body was checked as a statement.
			return parent.children_count > 0
				&& tc.a.child(parent, parent.children_count - 1) == current
				&& !tc.is_statement_node(current)
		}
		if parent.kind in [.fn_decl, .fn_literal, .comptime_for] {
			return false
		}
		if parent.kind in [.paren, .expr_stmt] {
			current = parent_id
			continue
		}
		if parent.kind in [.block, .match_branch] {
			if !tc.expr_is_value_tail_of(parent_id, id) {
				return false
			}
			current = parent_id
			continue
		}
		if parent.kind in [.if_expr, .match_stmt, .comptime_if] {
			if parent.children_count == 0 {
				return false
			}
			if tc.a.child(parent, 0) == current {
				return parent.kind in [.if_expr, .match_stmt]
			}
			current = parent_id
			continue
		}
		if parent.kind == .or_expr {
			if parent.children_count == 0 || tc.a.child(parent, 0) == current {
				return true
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
			tc.smartcasts = saved_smartcasts
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
		// A loop jump is a statement even at the tail of a value branch. Its
		// ownership hook must record the snapshot consumed by code generation.
		is_value_tail := value_tail && i == last_idx && child.kind !in [.break_stmt, .continue_stmt]
		if is_value_tail {
			if node.kind == .match_branch {
				if expected := tc.match_branch_enum_context(child_id) {
					_ = tc.resolve_expr(child_id, expected)
				}
			}
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
		tc.record_error_at(.return_mismatch, 'unreachable code', unreachable_id, tc.unreachable_statement_diagnostic_pos(unreachable_id))
	}
}

// match_branch_enum_context uses the first branch's enum result to type later
// shorthands before checking their parenthesized and bitwise expressions.
fn (tc &TypeChecker) match_branch_enum_context(tail_id flat.NodeId) ?Type {
	branch_id := tc.direct_parent_id(tail_id)
	match_id := tc.direct_parent_id(branch_id)
	if !tc.valid_node_id(match_id) {
		return none
	}
	match_node := tc.a.node(match_id)
	if match_node.kind != .match_stmt || match_node.children_count < 2 {
		return none
	}
	first_branch_id := tc.a.child(match_node, 1)
	first_tail := tc.branch_tail_expr_id(first_branch_id)
	if !tc.valid_node_id(first_tail) {
		return none
	}
	subject_id := tc.a.child(match_node, 0)
	subject_type := unalias_type(unwrap_pointer(tc.declared_receiver_expr_type(subject_id) or {
		tc.resolve_type(subject_id)
	}))
	expected := tc.match_branch_tail_diagnostic_type(tc.expr_key(subject_id), subject_type,
		*tc.a.node(first_branch_id), first_tail)
	if unalias_type(expected) is Enum {
		return expected
	}
	return none
}

fn (mut tc TypeChecker) initialize_pointer_alias_goto_targets() {
	tc.fn_context.pointer_alias_goto_states.clear()
	tc.fn_context.pointer_alias_backward_goto_targets.clear()
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
		states << snapshot_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
		tc.fn_context.pointer_alias_goto_states[node.value] = states
		return
	}
	if node.kind != .label_stmt {
		return
	}
	if tc.fn_context.pointer_alias_backward_goto_targets[node.value] {
		tc.prepare_pointer_binding_write()
		for key, _ in tc.fn_context.pointer_binding_value_keys {
			tc.fn_context.pointer_binding_value_keys[key] = [
				pointer_binding_unknown_value(key),
			]
		}
		return
	}
	incoming := tc.fn_context.pointer_alias_goto_states[node.value] or { return }
	mut paths := [
		snapshot_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys),
	]
	for state in incoming {
		paths << snapshot_pointer_binding_value_keys(state)
	}
	tc.fn_context.pointer_binding_value_keys = merge_pointer_binding_snapshots(paths, tc.fn_context.pointer_binding_value_keys)
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
	has_embed_file_value := tc.expr_source_contains_embed_file(expr_id)
	mut semantic_id := expr_id
	mut semantic := tc.a.node(expr_id)
	for semantic.kind == .paren && semantic.children_count == 1 {
		inner_id := tc.a.child(semantic, 0)
		inner := tc.a.node(inner_id)
		if inner.kind == .empty {
			break
		}
		semantic_id = inner_id
		semantic = inner
	}
	if tc.errors.any(it.node == expr_id || it.node == semantic_id) && !has_embed_file_value {
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
	if semantic.kind == .empty {
		return
	}
	if semantic.kind == .call {
		tc.check_must_use_call(semantic_id, semantic)
		return
	}
	if tc.node_is_in_translated_file(id) {
		return
	}
	if semantic.kind == .or_expr {
		if has_embed_file_value {
			if embed_id := tc.or_fallback_embed_file_value(semantic_id) {
				tc.record_error_at(.unknown_ident, 'expression evaluated but not used', expr_id,
					tc.a.node(embed_id).pos)
			}
		}
		return
	}
	if semantic.kind in [.spawn_expr, .dump_expr, .if_expr, .match_stmt, .lock_expr, .select_stmt,
		.sql_expr, .fn_literal, .lambda_expr] {
		return
	}
	if semantic.kind == .selector && semantic.children_count > 0 {
		if tc.resolve_type(tc.a.child(semantic, 0)) is Void && !has_embed_file_value {
			return
		}
	}
	if semantic.kind == .postfix && semantic.op in [.inc, .dec] {
		return
	}
	if semantic.kind == .prefix && semantic.op == .arrow {
		if tc.node_is_inside_for_statement(id) {
			return
		}
		tc.record_error_at(.unknown_ident, 'expression evaluated but not used', expr_id, token.new_span(semantic.pos.id, semantic.pos.offset, semantic.pos.offset + 2))
		return
	}
	if semantic.kind == .infix && semantic.op == .arrow {
		return
	}
	if semantic.kind == .infix && semantic.op == .left_shift && semantic.children_count > 0 {
		receiver := unalias_type(unwrap_pointer(tc.resolve_type(tc.a.child(semantic, 0))))
		if receiver is Array
			|| (receiver is OptionType && unalias_type(receiver.base_type) is Array) {
			return
		}
	}
	if semantic.kind == .infix && semantic.op in [.left_shift, .right_shift, .right_shift_unsigned] {
		return
	}
	if tc.resolve_type(expr_id) is Void && !has_embed_file_value {
		return
	}
	if semantic.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal] {
		if tc.unused_literal_has_trailing_token(*stmt, *semantic)
			&& !(tc.unused_literal_is_nonfinal_function_block_tail(id)
				&& tc.unused_literal_starts_source_line(*semantic)) {
			tc.record_error_at(.unknown_ident, 'expression evaluated but not used', expr_id, semantic.pos)
		} else {
			tc.record_warning_at(.unknown_ident, 'expression evaluated but not used', expr_id, semantic.pos)
		}
		return
	}
	if semantic.kind == .ident {
		tc.record_error_at(.unknown_ident, '`${semantic.value}` evaluated but not used', expr_id,
			semantic.pos)
		return
	}
	pos := tc.unused_expression_diagnostic_pos(expr_id, semantic_id, *semantic,
		has_embed_file_value)
	tc.record_error_at(.unknown_ident, 'expression evaluated but not used', expr_id, pos)
}

fn (tc &TypeChecker) expr_source_contains_embed_file(id flat.NodeId) bool {
	return tc.source_text_for_node(id).contains('$embed_file')
}

fn (tc &TypeChecker) nested_embed_file_value(id flat.NodeId) ?flat.NodeId {
	if !tc.valid_node_id(id) {
		return none
	}
	mut pending := [id]
	mut index := 0
	for index < pending.len && index < 512 {
		current_id := pending[index]
		index++
		current := tc.a.node(current_id)
		if current.kind == .struct_init
			&& current.source_value_id() == checker_tail_source_embed_file_data {
			return current_id
		}
		pending << tc.a.children_of(current)
	}
	return none
}

// or_fallback_embed_file_value returns the `$embed_file` value produced by the
// `or { ... }` block of an `or_expr`, i.e. the value that is discarded when the
// whole `expr or { ... }` is unused. `$embed_file` values inside the guarded
// expression (for example call arguments in `f($embed_file('x')) or { ... }`) or
// in earlier statements of the block are consumed, so they are not reported.
fn (tc &TypeChecker) or_fallback_embed_file_value(id flat.NodeId) ?flat.NodeId {
	node := tc.a.node(id)
	if node.kind != .or_expr || node.children_count < 2 {
		return none
	}
	mut value_id := tc.a.child(node, 1)
	mut value := tc.a.node(value_id)
	for _ in 0 .. 64 {
		if value.kind == .block && value.children_count > 0 {
			value_id = tc.a.child(value, value.children_count - 1)
		} else if value.kind in [.expr_stmt, .paren] && value.children_count == 1 {
			value_id = tc.a.child(value, 0)
		} else if value.kind == .or_expr && value.children_count >= 2 {
			value_id = tc.a.child(value, 1)
		} else {
			break
		}
		value = tc.a.node(value_id)
	}
	if value.kind == .struct_init
		&& value.source_value_id() == checker_tail_source_embed_file_data {
		return value_id
	}
	return none
}

fn (tc &TypeChecker) unused_expression_diagnostic_pos(expr_id flat.NodeId, semantic_id flat.NodeId, semantic flat.Node,
	has_embed_file_value bool) token.Pos {
	mut pos := if expr_id == semantic_id && semantic.kind == .selector {
		tc.selector_field_diagnostic_pos(semantic_id, semantic.value)
	} else {
		tc.a.node(expr_id).pos
	}
	if expr_id == semantic_id && semantic.kind == .infix && semantic.children_count >= 2 {
		lhs_id := tc.a.child(&semantic, 0)
		lhs := tc.a.node(lhs_id)
		if lhs.kind == .selector {
			start := tc.selector_field_diagnostic_pos(lhs_id, lhs.value)
			return token.new_span(pos.id, start.offset, pos.end)
		}
	}
	if expr_id != semantic_id || !has_embed_file_value {
		return pos
	}
	if semantic.kind == .array_init && pos.end > pos.offset {
		return token.new_span(pos.id, pos.offset, pos.end - 1)
	}
	expr_source := tc.source_text_for_node(expr_id)
	if expr_source.starts_with('.') && pos.offset < pos.end {
		return token.new_span(pos.id, pos.offset + 1, pos.end)
	}
	if semantic.kind == .selector && semantic.children_count > 0 {
		mut base_id := tc.a.child(&semantic, 0)
		mut base := tc.a.node(base_id)
		for base.kind == .paren && base.children_count == 1 {
			base_id = tc.a.child(base, 0)
			base = tc.a.node(base_id)
		}
		if base.kind == .or_expr {
			if embed_id := tc.or_fallback_embed_file_value(base_id) {
				return tc.a.node(embed_id).pos
			}
		} else if base.kind == .lock_expr {
			if embed_id := tc.nested_embed_file_value(base_id) {
				return tc.a.node(embed_id).pos
			}
		}
	}
	if semantic.kind != .infix || semantic.children_count < 2 {
		return pos
	}
	lhs_id := tc.a.child(&semantic, 0)
	rhs_id := tc.a.child(&semantic, 1)
	lhs := tc.a.node(lhs_id)
	rhs := tc.a.node(rhs_id)
	if rhs.kind == .selector && rhs.children_count > 0 {
		rhs_base := tc.a.child_node(rhs, 0)
		if rhs_base.kind == .call && rhs_base.children_count > 0
			&& tc.a.child_node(rhs_base, 0).kind == .fn_literal {
			return tc.selector_field_diagnostic_pos(rhs_id, rhs.value)
		}
	}
	if lhs.kind == .or_expr {
		if embed_id := tc.or_fallback_embed_file_value(lhs_id) {
			return tc.a.node(embed_id).pos
		}
	}
	if lhs.kind == .call && lhs.children_count > 0
		&& tc.a.child_node(lhs, 0).value == '__v3_isreftype' {
		if embed_id := tc.nested_embed_file_value(lhs_id) {
			embed_pos := tc.a.node(embed_id).pos
			return token.new_span(pos.id, embed_pos.offset, pos.end)
		}
	}
	if lhs.kind == .sizeof_expr && lhs.children_count > 0 {
		if embed_id := tc.nested_embed_file_value(lhs_id) {
			embed_pos := tc.a.node(embed_id).pos
			return token.new_span(pos.id, embed_pos.offset, pos.end)
		}
	}
	return pos
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

fn (tc &TypeChecker) unused_literal_starts_source_line(expr flat.Node) bool {
	file := tc.a.source_files[expr.pos.id] or { return false }
	source := tc.source_texts_by_file[file.name] or { return false }
	end := int_max(0, int_min(expr.pos.offset, source.len))
	mut start := end
	for start > 0 && source[start - 1] != `\n` {
		start--
	}
	for c in source[start..end] {
		if c !in [` `, `\t`, `\r`] {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) unused_literal_is_nonfinal_function_block_tail(stmt_id flat.NodeId) bool {
	block_id := tc.direct_parent_id(stmt_id)
	if !tc.valid_node_id(block_id) {
		return false
	}
	block := tc.a.node(block_id)
	if block.kind != .block || block.children_count == 0
		|| tc.a.child(block, block.children_count - 1) != stmt_id {
		return false
	}
	parent_id := tc.direct_parent_id(block_id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	return parent.kind == .fn_decl && parent.children_count > 0
		&& tc.a.child(parent, parent.children_count - 1) != block_id
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
			.or_expr {
				// The `or {}` block of a call that returns no value only runs statements.
				if parent.children_count >= 2 && tc.a.child(parent, 1) == current {
					source_type := unalias_type(tc.resolve_type(tc.a.child(parent, 0)))
					if is_result_void_type(source_type) || is_option_void_type(source_type) {
						return false
					}
				}
				return passed_branch
			}
			.return_stmt, .decl_assign, .assign, .selector_assign, .index_assign, .field_init,
			.call, .infix, .prefix, .postfix, .selector, .index, .array_literal, .array_init,
			.map_init, .struct_init, .string_interp, .cast_expr, .as_expr, .spawn_expr,
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
	tc.record_warning_at(.unknown_ident, 'return value must be used, ${kind} `${name}` was tagged with `@[must_use]`', id, pos)
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

// none_guard_fallthrough_unwraps reports whether the option `key` read at `id`
// is unwrapped by an earlier `if key == none { return }` statement. V1 exposes
// that unwrap to new declarations (`y := key` gets the option's base type), unlike
// the other fallthrough narrowings (`!is` guards, asserts, `||` conditions).
fn (tc &TypeChecker) none_guard_fallthrough_unwraps(id flat.NodeId, key string) bool {
	if key.len == 0 || !valid_string_data(key) {
		return false
	}
	mut current := id
	mut parent_id := tc.direct_parent_id(current)
	for _ in 0 .. 1024 {
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind in [.block, .fn_decl, .match_branch, .for_stmt, .for_in_stmt, .lock_expr,
			.defer_stmt, .select_branch] {
			mut index := -1
			for i in 0 .. parent.children_count {
				if tc.a.child(parent, i) == current {
					index = i
					break
				}
			}
			for i := index - 1; i >= 0; i-- {
				sibling_id := tc.a.child(parent, i)
				if tc.is_none_guard_exit_for_key(sibling_id, key) {
					return true
				}
				if tc.subtree_assigns_key(sibling_id, key) {
					return false
				}
			}
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return false
		}
		current = parent_id
		parent_id = tc.direct_parent_id(current)
	}
	return false
}

// is_none_guard_exit_for_key reports whether `id` is `if key == none { return }`
// (the condition may be parenthesized; the then-branch must always exit).
fn (tc &TypeChecker) is_none_guard_exit_for_key(id flat.NodeId, key string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .if_expr || node.children_count < 2 {
		return false
	}
	cond_id := tc.unwrap_paren_expr_id(tc.a.child(node, 0))
	if !tc.valid_node_id(cond_id) {
		return false
	}
	cond := tc.a.node(cond_id)
	if cond.kind != .infix || cond.op != .eq || cond.children_count < 2 {
		return false
	}
	lhs := tc.a.child_node(cond, 0)
	rhs := tc.a.child_node(cond, 1)
	if lhs.kind != .ident || lhs.value != key || rhs.kind != .none_expr {
		return false
	}
	return tc.stmt_definitely_returns(tc.a.child(node, 1))
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
	if tc.nonmut_mutable_interface_smartcast(expr_id, inner.source_value_id()) {
		return none
	}
	key := tc.expr_key(expr_id)
	if key.len == 0 || !valid_string_data(key) || inner.source_value_id() == 0 {
		return none
	}
	return LocalBinding{
		name: key
		typ:  tc.smartcast_target_type_for_is_expr(expr_id, inner.source_value_id())
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
		return last.kind in [.expr_stmt, .if_expr, .match_stmt, .comptime_if]
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
		return last.kind in [.expr_stmt, .if_expr, .match_stmt, .comptime_if]
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
			tc.smartcasts = saved_smartcasts
			tc.check_infix(cond_id, cond)
			return if exports_bindings { bindings } else { []LocalBinding{} }
		}
		unsafe_alias_skipped_rhs := tc.fn_context.unsafe_reference_alias_owners.clone()
		pointer_alias_skipped_rhs :=
			snapshot_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
		saved_smartcasts := clone_smartcasts(tc.smartcasts)
		for sc in lhs_smartcasts {
			if valid_string_data(sc.name) {
				tc.smartcasts[sc.name] = sc.typ
			}
		}
		rhs_bindings := tc.check_condition(rhs_id)
		bindings << rhs_bindings
		tc.merge_unsafe_reference_alias_short_circuit_state(cond.op, lhs_id, unsafe_alias_skipped_rhs)
		tc.merge_pointer_binding_short_circuit_state(cond.op, lhs_id, pointer_alias_skipped_rhs)
		tc.smartcasts = saved_smartcasts
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
			tc.record_error(.condition_mismatch, 'non-bool type `${cond_name}` used as if condition', cond_id)
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
	if !tc.condition_type_is_bool_like(cond_type)
		&& !tc.translated_condition_compatible(cond_id, cond_type) && tc.should_diagnose(cond_id) {
		cond_name := tc.diagnostic_expr_type_name(cond_id, cond_type)
		tc.record_error(.condition_mismatch, 'non-bool type `${cond_name}` used as if condition', cond_id)
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

fn (tc &TypeChecker) condition_type_is_bool_like(typ &Type) bool {
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
	if _node.value == 'c_style' && condition.kind == .postfix
		&& condition.op in [.inc, .dec] && condition.children_count > 0 {
		child_id := tc.a.child(condition, 0)
		tc.check_node(child_id)
		op := if condition.op == .inc { '++' } else { '--' }
		source := tc.source_text_for_node(cond_id)
		operand_end := int_max(condition.pos.offset + 1, condition.pos.end - op.len)
		operand_pos := token.new_span(condition.pos.id, condition.pos.offset, operand_end)
		tc.record_error_at(.condition_mismatch, 'cannot use ${source} as value', cond_id,
			operand_pos)
		return
	}
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
	if tc.condition_type_is_bool_like(cond_type)
		|| tc.translated_condition_compatible(cond_id, cond_type) {
		return
	}
	if _node.value == 'c_style' && unalias_type(cond_type) is Pointer {
		return
	}
	if tc.should_diagnose(cond_id) {
		tc.record_error(.condition_mismatch, 'if condition must be `bool`, not `${cond_type.name()}`', cond_id)
	}
}

fn (tc &TypeChecker) postfix_is_c_style_for_condition(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .paren {
			current = parent_id
			continue
		}
		return parent.kind == .for_stmt && parent.value == 'c_style'
			&& parent.children_count > 1 && tc.a.child(parent, 1) == current
	}
	return false
}

fn (tc &TypeChecker) postfix_is_direct_return_value(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .paren {
			current = parent_id
			continue
		}
		return parent.kind == .return_stmt && parent.children_count == 1
			&& tc.a.child(parent, 0) == current
	}
	return false
}

fn (tc &TypeChecker) postfix_is_direct_unsafe_decl_value(id flat.NodeId) bool {
	if !tc.expr_is_inside_unsafe_block(id) {
		return false
	}
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .paren {
			current = parent_id
			continue
		}
		return parent.kind == .decl_assign && parent.children_count > 1
			&& tc.a.child(parent, parent.children_count - 1) == current
	}
	return false
}

fn (tc &TypeChecker) postfix_is_struct_field_default(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .paren {
			current = parent_id
			continue
		}
		return parent.kind == .field_decl && parent.children_count > 0
			&& tc.a.child(parent, parent.children_count - 1) == current
	}
	return false
}

fn (mut tc TypeChecker) check_postfix_value_uses_preflight() {
	saved_file := tc.cur_file
	saved_module := tc.cur_module
	defer {
		tc.cur_file = saved_file
		tc.cur_module = saved_module
	}
	for index in tc.preflight_nodes(.postfix) {
		id := flat.NodeId(index)
		node := tc.a.nodes[index]
		if tc.node_is_from_translated_file(node) {
			continue
		}
		if node.op !in [.inc, .dec] || tc.expr_is_standalone_statement(id)
			|| tc.postfix_is_c_style_for_condition(id) || tc.postfix_is_direct_return_value(id)
			|| tc.postfix_is_direct_unsafe_decl_value(id) || tc.postfix_is_struct_field_default(id) {
			continue
		}
		file := tc.a.source_files[node.pos.id] or { continue }
		if tc.diagnostic_files.len > 0 && file.name !in tc.diagnostic_files {
			continue
		}
		if tc.file_has_global_receiver(file.name) {
			continue
		}
		op := if node.op == .inc { '++' } else { '--' }
		op_pos := tc.prefix_operator_pos(id, op)
		// Like V1, only an operator that closes a call argument or an index, as in
		// `f(x++)` or `a[x--]`, is reported; `n := count++` is a plain post-increment.
		source := tc.source_texts_by_file[file.name] or { '' }
		if !postfix_operator_closes_group(source, op_pos.end) {
			continue
		}
		tc.cur_file = file.name
		tc.cur_module = tc.file_modules[file.name] or { 'main' }
		// V1 reports this from the parser, where only an explicit `-W` turns the
		// warning into an error; `-prod` alone does not.
		tc.record_warning_or_error_at(.assignment_mismatch, '`${op}` operator can only be used as a statement',
			id, op_pos, tc.explicit_warns_are_errors)
	}
}

// postfix_operator_closes_group reports whether the next token after a postfix
// `++`/`--` ending at `end` is `)` or `]`, skipping whitespace and comments as V1's
// parser does: `f(x++ )` and `f(x++ /* c */)` close the call like `f(x++)`.
fn postfix_operator_closes_group(source string, end int) bool {
	if end < 0 {
		return false
	}
	mut i := end
	for i < source.len {
		c := source[i]
		if c in [` `, `\t`, `\n`, `\r`] {
			i++
		} else if c == `/` && i + 1 < source.len && source[i + 1] in [`/`, `*`] {
			i = skip_non_code_at(source, i)
		} else {
			return c in [`)`, `]`]
		}
	}
	return false
}

// if_guard_operand_declared_optional reports whether the `if x := a` operand `a`, a name
// or a field, is declared as an option or a result (a `mut` parameter through its
// pointer), so that only a smartcast made its type a payload.
fn (tc &TypeChecker) if_guard_operand_declared_optional(rhs_node flat.Node) bool {
	if rhs_node.kind == .ident {
		if declared := tc.cur_scope.lookup(rhs_node.value) {
			clean := unalias_type(unwrap_pointer(declared))
			return clean is OptionType || clean is ResultType
		}
	} else if rhs_node.kind == .selector {
		if declared := tc.selector_declared_value_type(rhs_node) {
			clean := unalias_type(declared)
			return clean is OptionType || clean is ResultType
		}
	}
	return false
}

// check_if_guard validates check if guard state for types.
fn (mut tc TypeChecker) check_if_guard(id flat.NodeId, node flat.Node) []LocalBinding {
	if node.children_count < 2 {
		return []LocalBinding{}
	}
	for lhs_id in tc.if_guard_lhs_ids(node) {
		tc.check_local_binding_global_shadowing(lhs_id)
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
	mut rhs_node := tc.a.node(rhs_id)
	for rhs_node.kind == .paren && rhs_node.children_count == 1 {
		rhs_node = tc.a.child_node(rhs_node, 0)
	}
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
		// `if x == none {} else if y := x {`: as in V 0.5.2, the none check does
		// not narrow `x` in the condition of the `else if`, so the guard unwraps
		// the declared option.
		if rhs_type !is OptionType && rhs_type !is ResultType
			&& tc.if_guard_follows_none_check(id, rhs_node.value) {
			if declared := tc.cur_scope.lookup(rhs_node.value) {
				if declared is OptionType {
					rhs_type = declared
				}
			}
		}
	}
	if rhs_node.kind == .selector {
		if declared := tc.selector_declared_value_type(*rhs_node) {
			if declared is OptionType || declared is ResultType {
				rhs_type = declared
			}
		}
	}
	mut payload := tc.if_guard_payload_type(rhs_id, rhs_type)
	is_optional_result := rhs_type is OptionType || rhs_type is ResultType
	if payload is Void && !is_optional_result {
		if tc.should_diagnose(id) {
			tc.record_error_at(.condition_mismatch, 'expression should either return an Option or a Result', rhs_id, rhs_node.pos)
		}
		if tc.checking_concrete_members && tc.if_guard_operand_declared_optional(*rhs_node) {
			tc.concrete_guard_options[rhs_id] = true
		}
		payload = rhs_type
	}
	if payload is Void {
		if is_optional_result && lhs_ids.all(tc.a.node(it).value == '_') {
			return []LocalBinding{}
		}
		if is_optional_result && tc.should_diagnose(id) {
			tc.record_error_at(.condition_mismatch, 'if guard expects non-propagate option or result', id, tc.if_guard_diagnostic_pos(node))
		}
		return tc.if_guard_unknown_bindings(lhs_ids, node.is_mut)
	}
	if payload is MultiReturn {
		if lhs_ids.len != payload.types.len && tc.should_diagnose(id) {
			tc.record_error_at(.condition_mismatch, 'if guard expects ${payload.types.len} variables, but got ${lhs_ids.len}', id, tc.if_guard_diagnostic_pos(node))
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
		tc.record_error_at(.condition_mismatch, 'if guard expects a single variable, but got ${lhs_ids.len}', id, tc.if_guard_diagnostic_pos(node))
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

// if_guard_follows_none_check reports whether the guard `guard_id` is the
// condition of an `else if` in a chain whose earlier condition, such as
// `x == none`, narrows the variable `name` for its `else` branch.
fn (tc &TypeChecker) if_guard_follows_none_check(guard_id flat.NodeId, name string) bool {
	mut current := tc.direct_parent_id(guard_id)
	if !tc.valid_node_id(current) || tc.a.node(current).kind != .if_expr
		|| tc.a.child(tc.a.node(current), 0) != guard_id {
		return false
	}
	mut parent_id := tc.direct_parent_id(current)
	for tc.valid_node_id(parent_id) {
		parent := tc.a.node(parent_id)
		if parent.kind != .if_expr || parent.children_count <= 2
			|| tc.a.child(parent, 2) != current {
			return false
		}
		for binding in tc.extract_else_branch_smartcasts(tc.a.child(parent, 0)) {
			if binding.name == name {
				return true
			}
		}
		current = parent_id
		parent_id = tc.direct_parent_id(current)
	}
	return false
}

// if_guard_payload_type returns the value an `if x := rhs` guard unwraps from
// `rhs` (of type `rhs_type`), or `void` when `rhs` cannot be guarded.
fn (tc &TypeChecker) if_guard_payload_type(rhs_id flat.NodeId, rhs_type Type) Type {
	if rhs_type is OptionType {
		return *rhs_type.base_type
	}
	if rhs_type is ResultType {
		return *rhs_type.base_type
	}
	rhs := tc.a.nodes[int(rhs_id)]
	if rhs.kind == .index && rhs.children_count > 0 {
		base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&rhs, 0)))
		if base_type is Map {
			return *base_type.value_type
		} else if base_type is Array {
			return *base_type.elem_type
		} else if base_type is ArrayFixed {
			return *base_type.elem_type
		} else if base_type is String {
			return builtin_u8_type
		}
	} else if rhs.kind == .call && rhs.children_count > 0 {
		fn_node := tc.a.child_node(&rhs, 0)
		if fn_node.kind == .selector && fn_node.value == 'get' {
			if arr := tc.call_receiver_array_type(rhs) {
				return *arr.elem_type
			}
		}
	} else if rhs.kind == .prefix && rhs.op == .arrow && rhs.children_count > 0 {
		source_type := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(&rhs, 0)))
		if source_type is Channel {
			return *source_type.elem_type
		}
	}
	return builtin_void_type
}

// if_guard_query_bindings types the variables bound by the guard `cond` without
// checking it. Type queries made outside the guard's checked scope need them,
// e.g. const initializers, which are typed before any body is checked.
fn (tc &TypeChecker) if_guard_query_bindings(cond flat.Node) []LocalBinding {
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return []LocalBinding{}
	}
	rhs_id := tc.a.child(&cond, 1)
	if !tc.valid_node_id(rhs_id) {
		return []LocalBinding{}
	}
	mut rhs_type := tc.resolve_type(rhs_id)
	rhs_node := tc.a.node(rhs_id)
	if rhs_node.kind == .selector {
		if declared := tc.selector_declared_value_type(*rhs_node) {
			if declared is OptionType || declared is ResultType {
				rhs_type = declared
			}
		}
	}
	payload := tc.if_guard_payload_type(rhs_id, rhs_type)
	if payload is Void || type_contains_unknown(payload) {
		return []LocalBinding{}
	}
	mut result := []LocalBinding{}
	for i, lhs_id in tc.if_guard_lhs_ids(cond) {
		lhs := tc.a.node(lhs_id)
		if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_'
			|| !valid_string_data(lhs.value) {
			continue
		}
		if payload is MultiReturn {
			if i < payload.types.len {
				result << LocalBinding{
					name:   lhs.value
					typ:    payload.types[i]
					is_mut: lhs.is_mut || cond.is_mut
				}
			}
		} else if i == 0 {
			result << LocalBinding{
				name:   lhs.value
				typ:    payload
				is_mut: lhs.is_mut || cond.is_mut
			}
		}
	}
	return result
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
	saved_mut_local_owners_shared := tc.fn_context.mut_local_owners_shared
	saved_mut_local_owners := tc.share_mut_local_owners()
	defer {
		tc.fn_context.mut_local_owners = saved_mut_local_owners
		tc.fn_context.mut_local_owners_shared = saved_mut_local_owners_shared
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
		subject_declared_type = builtin_void_type
	}
	if declared := tc.match_retests_smartcast_subject(subject_id, node) {
		subject_declared_type = declared
	}
	if subject.kind == .paren {
		tc.record_warning_at(.condition_mismatch, 'unnecessary `()` in `match` condition, use `match expr {` instead of `match (expr) {`.', subject_id, subject.pos)
	}
	if subject.kind == .none_expr {
		tc.record_error_at(.condition_mismatch, '`none` cannot be a match condition', id, tc.match_header_pos(node))
	}
	if subject.kind == .ident
		&& ((unalias_type(subject_declared_type) is Pointer && unalias_type((unalias_type(subject_declared_type) as Pointer).base_type) !is Interface
			&& unalias_type((unalias_type(subject_declared_type) as Pointer).base_type) !is SumType
			&& !tc.mut_param_binding_matches_lvalue(subject.value)
			&& !tc.current_fn_param_is_receiver(subject.value))
			|| tc.current_binding_is_shared(subject.value)) {
		tc.record_error_at(.condition_mismatch, 'missing `*` dereferencing `${subject.value}` in match statement', subject_id, tc.node_value_diagnostic_pos(subject_id))
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
	pointer_alias_base := snapshot_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
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
			snapshot_pointer_binding_value_keys(pointer_alias_base)
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
				tc.record_error_at(.condition_mismatch, '`match` expression with Option type only checks against `none`, to match its value you must unwrap it first `var?`', branch_id, token.new_span(first.pos.id, first.pos.offset, last.pos.end))
			}
		}
		if branch.value == 'else' && i < int(node.children_count) - 1 {
			tc.record_error_at(.condition_mismatch, '`else` must be the last branch of `match`', branch_id, token.new_span(branch.pos.id, branch.pos.offset, branch.pos.offset + 4))
		}
		for j in 0 .. n_conds {
			cond_id := tc.a.child(branch, j)
			// A condition that is an expression, as in `match true { a < b {} }`
			// or `!(a < b)`, gets the checks an `if` condition gets.
			if tc.match_condition_is_expression(cond_id) {
				tc.check_node(cond_id)
			}
			tc.check_match_range_types(subject_id, subject_type, cond_id)
			tc.check_match_condition_type(subject_type, cond_id)
			tc.check_match_alias_condition(subject_declared_type, cond_id)
			tc.check_match_type_pattern_subject(subject_id, subject_type, cond_id)
			tc.check_duplicate_match_condition(branch, i, j, cond_id, mut seen_match_values, mut seen_match_ranges, mut seen_match_patterns)
			if !match_source_has_typeof {
				tc.record_constant_match_condition(subject_id, cond_id, i < int(node.children_count) - 1)
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
				if pattern_source_id := tc.match_type_pattern(cond) {
					pattern_source_id_3 := pattern_source_id

					if _ := tc.sum_variant_type_for_pattern(subject_type.source_type_id(), pattern_source_id_3) {
					} else {
						pattern := flat.source_type_name(pattern_source_id_3)
						base := tc.sum_base_name(subject_type.source_type_id())
						raw_variants := tc.sum_types.type_ids(base) or { []u32{} }
						diagnostic_variants :=
							raw_variants.map(tc.sum_variant_diagnostic_name(flat.source_type_name(tc.concrete_sum_variant_name(subject_type.source_type_id(), it))))
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
						base_message := '`${subject_type.name()}` has no variant `${diagnostic_pattern}`'
						mut message :=
							util.new_suggestion(diagnostic_pattern, variants).say(base_message)
						if message.contains('\n${variants.len} possibilities:') {
							quoted := variants.map('`${it}`')
							message = '${base_message}.\n${variants.len} possibilities: ${quoted.join(', ')}.'
						}
						tc.record_error_at(.condition_mismatch, message, cond_id, tc.node_value_diagnostic_pos(cond_id))
					}
				}
			}
		} else if is_ierror_type(subject_type) {
			for j in 0 .. n_conds {
				cond_id := tc.a.child(branch, j)
				cond := tc.a.node(cond_id)
				if pattern_source_id := tc.match_type_pattern(cond) {
					if _ := tc.resolve_ierror_match_pattern(pattern_source_id) {
					} else if _ := tc.resolve_interface_pattern_interface(pattern_source_id) {
						// Another interface is tested at run time, like on other interfaces.
					} else if tc.should_diagnose(cond_id) {
						pattern := flat.source_type_name(pattern_source_id)
						tc.record_error(.condition_mismatch, '`${pattern}` is not compatible with `IError`', cond_id)
					}
				}
			}
		} else if subject_type is Interface {
			for j in 0 .. n_conds {
				cond_id := tc.a.child(branch, j)
				cond := tc.a.node(cond_id)
				if pattern_source_id := tc.match_type_pattern(cond) {
					pattern_source_id_5 := pattern_source_id
					pattern := flat.source_type_name(pattern_source_id_5)

					if interface_pattern_is_collapsed_container_id(pattern_source_id_5) {
						container_type := tc.parse_type(pattern_source_id_5)
						if !tc.type_implements_interface(container_type, subject_type)
							&& tc.should_diagnose(cond_id) {
							tc.record_error(.condition_mismatch, '`${pattern}` is not compatible with interface `${subject_type.name()}`', cond_id)
						}
						continue
					}
					// An interface pattern is tested at run time against the stored value's
					// type. It is valid even when no type in the program implements both
					// interfaces; the test is then always false.
					if _ := tc.resolve_interface_pattern_interface(pattern_source_id_5) {
						continue
					}
					if concrete_source_id := tc.resolve_interface_match_pattern(pattern_source_id_5) {
						concrete_type := unwrap_pointer(tc.parse_type(concrete_source_id))
						concrete_name_id := method_type_name(concrete_type)
						if concrete_type !is Interface
							&& !tc.named_type_implements_interface(concrete_name_id, subject_type.source_type_id())
							&& tc.should_diagnose(cond_id) {
							pos := tc.match_condition_diagnostic_pos(cond_id)
							if !tc.record_interface_implementation_error(.condition_mismatch,
								concrete_type, subject_type, cond_id, pos) {
								tc.record_error_at(.condition_mismatch, '`${pattern}` is not compatible with interface `${subject_type.name()}`', cond_id, pos)
							}
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
					snapshot_pointer_binding_value_keys(pointer_alias_base)
				cond := tc.a.node(tc.a.child(branch, j))
				pattern_source_id_6 := tc.match_type_pattern(cond) or { continue }

				smartcast_type_source_id := tc.sum_variant_type_for_pattern(subject_type.source_type_id(), pattern_source_id_6) or {
					continue
				}

				tc.smartcasts = clone_smartcasts(saved_smartcasts)
				tc.smartcasts[subject_key] = tc.parse_type(smartcast_type_source_id)
				tc.push_scope()
				tc.check_statement_sequence(branch, n_conds, value_context)
				tc.pop_scope()
			}
			tc.smartcasts = saved_smartcasts
			$if ownership ? {
				tc.ownership_end_branch(branch_id)
			}
			if !tc.match_branch_definitely_returns(branch) {
				unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
				pointer_alias_paths << snapshot_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
			}
			continue
		}
		if subject_key.len > 0 && valid_string_data(subject_key) && n_conds == 1
			&& branch.children_count > 0 && (subject_type is SumType || is_ierror_type(subject_type)
			|| subject_type is Interface) {
			cond_id := tc.a.child(branch, 0)
			cond := tc.a.node(cond_id)
			if pattern_source_id := tc.match_type_pattern(cond) {
				smartcast_type_id := if subject_type is SumType {
					tc.sum_variant_type_for_pattern(subject_type.source_type_id(), pattern_source_id) or { pattern_source_id }
				} else if is_ierror_type(subject_type) {
					tc.resolve_ierror_match_pattern(pattern_source_id) or { pattern_source_id }
				} else if subject_type is Interface {
					tc.resolve_interface_match_pattern(pattern_source_id) or { pattern_source_id }
				} else {
					pattern_source_id
				}
				variant_type := tc.parse_type(smartcast_type_id)
				tc.smartcasts[subject_key] = if subject_type is Interface {
					interface_smartcast_variant_type(variant_type)
				} else {
					*variant_type
				}
			}
		} else if subject_key.len > 0 && valid_string_data(subject_key) && n_conds > 1
			&& subject_type is SumType {
			for sc in tc.multi_match_common_field_smartcasts(subject_type, branch, n_conds, subject_key) {
				tc.smartcasts[sc.name] = sc.typ
			}
		} else if subject_key.len > 0 && valid_string_data(subject_key) && n_conds > 1
			&& subject_type is Interface {
			if common_iface_source_id := tc.multi_interface_match_common_interface(subject_type, branch, n_conds) {
				tc.smartcasts[subject_key] = tc.parse_type(common_iface_source_id)
			}
		}
		tc.push_scope()
		$if ownership ? {
			tc.ownership_mark_scope_node(branch_id)
		}
		tc.declare_named_variant_binding(subject_type, branch, n_conds)
		tc.check_statement_sequence(branch, n_conds, value_context)
		tc.pop_scope()
		if value_context && has_value_tail && !tc.branch_has_value_tail(branch_id)
			&& !tc.match_branch_definitely_returns(branch)
			&& !tc.branch_tail_never_returns(branch_id) {
			tc.record_error_at(.if_branch_mismatch, '`match` expression requires an expression as the last statement of every branch', branch_id, branch.pos)
		}
		tc.smartcasts = saved_smartcasts
		$if ownership ? {
			tc.ownership_end_branch(branch_id)
		}
		if !tc.match_branch_definitely_returns(branch) {
			unsafe_alias_paths << tc.fn_context.unsafe_reference_alias_owners.clone()
			pointer_alias_paths << snapshot_pointer_binding_value_keys(tc.fn_context.pointer_binding_value_keys)
		}
	}
	if !tc.match_has_else_or_exhaustive_coverage(node) {
		unsafe_alias_paths << unsafe_alias_base.clone()
		pointer_alias_paths << snapshot_pointer_binding_value_keys(pointer_alias_base)
	}
	tc.fn_context.unsafe_reference_alias_owners = intersect_unsafe_reference_alias_states(unsafe_alias_paths, unsafe_alias_base)
	tc.fn_context.pointer_binding_value_keys = merge_pointer_binding_snapshots(pointer_alias_paths, pointer_alias_base)
	if missing_non_else {
		tc.record_match_requires_non_else(node)
	}
	tc.check_match_sumtype_exhaustiveness(id, node, subject_type)
	tc.check_match_flag_enum_exhaustiveness(id, node, subject_type)
	tc.check_match_bool_exhaustiveness(id, node, subject_type)
	if value_context {
		tc.check_match_branch_tail_type_diagnostics(id, node)
	}
	if value_context && tc.match_expr_all_tails_are_none(node)
		&& !tc.expr_has_explicit_option_context(id) {
		tc.record_error_at(.if_branch_mismatch, 'invalid match expression, must supply at least one value other than `none`', id, tc.match_header_pos(node))
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

// match_condition_is_expression reports whether the match condition `id` is an
// expression, such as a comparison, a call or a literal. A name, qualified or
// not, can stand for a type, and ranges, enum shorthands and `none` have checks
// of their own. A field of an expression, as in `f(a < b).x`, is an expression.
fn (tc &TypeChecker) match_condition_is_expression(id flat.NodeId) bool {
	mut node := tc.a.node(id)
	for node.kind == .selector && node.children_count > 0 {
		node = tc.a.child_node(node, 0)
	}
	if node.kind in [.ident, .selector, .range, .enum_val, .none_expr] {
		return false
	}
	return tc.match_type_pattern(node) == none
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
		tc.record_error_at(.condition_mismatch, '`match` must have at least two branches including `else`, or an exhaustive set of branches', id, tc.match_keyword_pos(node))
		return false
	}
	if else_ids.len > 1 {
		first_else := tc.a.node(else_ids[0])
		tc.record_error_at(.condition_mismatch, '`match` can have only one `else` branch', else_ids[0], token.new_span(first_else.pos.id, first_else.pos.offset, first_else.pos.offset + 4))
	}
	return non_else_count == 0 && else_ids.len > 0
}

fn (mut tc TypeChecker) record_match_requires_non_else(node flat.Node) {
	for i in 1 .. node.children_count {
		branch_id := tc.a.child(&node, i)
		branch := tc.a.node(branch_id)
		if branch.kind == .match_branch && branch.value == 'else' {
			tc.record_error_at(.condition_mismatch, '`match` must have at least one non `else` branch', branch_id, token.new_span(branch.pos.id, branch.pos.offset, branch.pos.offset + 4))
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
		tc.record_error_at(.condition_mismatch, 'struct instances cannot be matched by type name, they can only be matched to other instances of the same struct type', cond_id, tc.match_condition_diagnostic_pos(cond_id))
		return
	}
	subject_name := tc.source_text_for_node(subject_id)
	tc.record_error_at(.condition_mismatch, 'matching by type can only be done for sum types, generics, interfaces, `${subject_name}` is none of those', cond_id, tc.match_condition_diagnostic_pos(cond_id))
}

fn (mut tc TypeChecker) check_match_alias_condition(subject_type Type, cond_id flat.NodeId) {
	if subject_type !is Alias {
		return
	}
	if unalias_and_unwrap_pointer_type(subject_type) is SumType {
		return
	}
	pattern := flat.source_type_name(tc.match_type_pattern(*tc.a.node(cond_id)) or { return })
	tc.record_error_at(.condition_mismatch, 'cannot match alias type `${short_type_name(subject_type.name())}` with `${short_type_name(pattern)}`', cond_id, tc.match_condition_diagnostic_pos(cond_id))
}

fn (mut tc TypeChecker) check_match_flag_enum_exhaustiveness(id flat.NodeId, node flat.Node, subject_type Type) {
	clean_subject := unalias_type(subject_type)
	if clean_subject !is Enum
		|| (!clean_subject.is_flag && clean_subject.source_type_id() !in tc.flag_enums)
		|| !tc.should_diagnose(id) {
		return
	}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind == .match_branch && branch.value == 'else' {
			return
		}
	}
	tc.record_error_at(.condition_mismatch, 'match must be exhaustive (add `else {}` at the end)', id, tc.match_header_pos(node))
}

fn (mut tc TypeChecker) check_match_bool_exhaustiveness(id flat.NodeId, node flat.Node, subject_type Type) {
	if unalias_type(subject_type).source_type_id() != flat.source_type_bool || !tc.should_diagnose(id) {
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
		tc.record_error_at(.condition_mismatch, 'match must be exhaustive (add match branches for: ${missing.join(', ')} or `else {}` at the end)', id, tc.match_header_pos(node))
	}
}

fn (mut tc TypeChecker) check_match_sumtype_exhaustiveness(id flat.NodeId, node flat.Node, subject_type Type) {
	clean_subject := unalias_type(subject_type)
	if clean_subject !is SumType || !tc.should_diagnose(id) {
		return
	}
	sum_subject := clean_subject as SumType
	raw_variants := tc.sum_types.type_ids(tc.sum_base_name(sum_subject.source_type_id())) or { return }
	if raw_variants.len == 0 {
		return
	}
	mut variants := tc.sum_exhaustive_leaf_variants(sum_subject.source_type_id(), 0)
	if variants.len == 0 {
		variants = raw_variants.map(tc.concrete_sum_variant_name(sum_subject.source_type_id(), it))
	}
	mut covered := map[u32]bool{}
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
			pattern_source_id_9 := tc.match_type_pattern(cond) or { continue }
			qpattern_id := tc.qualify_type_name(pattern_source_id_9)
			if matched_source_id := tc.sum_variant_type_for_pattern(sum_subject.source_type_id(), pattern_source_id_9) {
				matched_source_id_2 := matched_source_id

				matched_type := tc.parse_type(matched_source_id_2)
				matched_sum_name := if matched_type is SumType {
					matched_type.name_id
				} else if matched_type is Alias && matched_type.base_type is SumType {
					(matched_type.base_type as SumType).name_id
				} else {
					u32(0)
				}
				leaves := if matched_sum_name != 0 {
					tc.sum_exhaustive_leaf_variants(matched_sum_name, 0)
				} else {
					[]u32{}
				}
				if leaves.len > 0 {
					for leaf in leaves {
						covered[leaf] = true
					}
				} else {
					for variant in variants {
						if variant == matched_source_id_2 {
							covered[variant] = true
						}
					}
				}
			}
			for variant in variants {
				uses_type_arguments := flat.source_type_shape(variant).has(.bracketed)
					|| flat.source_type_shape(pattern_source_id_9).has(.bracketed)
					|| flat.source_type_shape(qpattern_id).has(.bracketed)
				if variant == pattern_source_id_9 || variant == qpattern_id
					|| (uses_type_arguments && (tc.generic_type_name_matches(variant, pattern_source_id_9)
						|| tc.generic_type_name_matches(variant, qpattern_id))) {
					covered[variant] = true
				}
			}
			pattern_short_id := flat.source_type_syntax(pattern_source_id_9).unqualified()
			mut short_match := u32(0)
			mut short_count := 0
			for variant in variants {
				if flat.source_type_syntax(variant).unqualified() == pattern_short_id {
					short_count++
					short_match = variant
				}
			}
			if short_count == 1 {
				covered[short_match] = true
			}
		}
	}
	mut missing := []u32{}
	for variant in variants {
		if variant !in covered {
			missing << variant
		}
	}
	if missing.len > 0 && else_ids.len == 0 {
		quoted_missing := missing.map('`${short_type_name(flat.source_type_name(it))}`')
		tc.record_error_at(.condition_mismatch, 'match must be exhaustive (add match branches for: ${quoted_missing.join(', ')} or `else {}` at the end)', id, tc.match_header_pos(node))
		return
	}
	if missing.len == 0 {
		for i in 1 .. node.children_count {
			branch := tc.a.child_node(node, i)
			if branch.kind != .match_branch || branch.value == 'else' {
				continue
			}
			for j in 0 .. branch.value.int() {
				pattern_source_id_10 := tc.match_type_pattern(tc.a.child_node(branch, j)) or { continue }

				if tc.parse_type(pattern_source_id_10) is Alias {
					return
				}
			}
		}
		for else_id in else_ids {
			else_branch := tc.a.node(else_id)
			tc.record_error_at(.condition_mismatch, 'match expression is exhaustive, `else` is unnecessary', else_id, token.new_span(else_branch.pos.id, else_branch.pos.offset, else_branch.pos.offset + 4))
		}
	}
}

fn (tc &TypeChecker) sum_exhaustive_leaf_variants(sum_name_id u32, depth int) []u32 {
	if depth >= 16 { return []u32{} }
	raw_variants := tc.sum_types.type_ids(tc.sum_base_name(sum_name_id)) or { return []u32{} }
	mut leaves := []u32{}
	for raw_variant in raw_variants {
		concrete := tc.concrete_sum_variant_name(sum_name_id, raw_variant)
		typ := tc.parse_type(concrete)
		nested_name := if typ is SumType {
			typ.source_type_id()
		} else if typ is Alias && typ.base_type is SumType {
			(typ.base_type as SumType).source_type_id()
		} else {
			u32(0)
		}
		if nested_name != 0 && tc.sum_base_name(nested_name) != tc.sum_base_name(sum_name_id) {
			nested := tc.sum_exhaustive_leaf_variants(nested_name, depth + 1)
			if nested.len > 0 {
				for leaf in nested { if leaf !in leaves { leaves << leaf } }
				continue
			}
		}
		if concrete !in leaves { leaves << concrete }
	}
	return leaves
}

fn (mut tc TypeChecker) check_match_condition_type(subject_type Type, cond_id flat.NodeId) {
	cond := tc.a.node(cond_id)
	if cond.kind == .range || tc.match_type_pattern(*cond) != none {
		return
	}
	clean_subject := unalias_type(subject_type)
	if clean_subject is OptionType {
		return
	}
	// Qualified enum values are names, so the expression-condition pass skips
	// them. Validate their selector before accepting a compatible enum type.
	if clean_subject is Enum && cond.kind == .selector
		&& !tc.match_condition_is_expression(cond_id) {
		tc.check_node(cond_id)
	}
	condition_type := tc.resolve_type(cond_id)
	if cond.kind == .enum_val && clean_subject is Enum {
		enum_name_id := tc.resolve_enum_name(clean_subject.source_type_id()) or { clean_subject.source_type_id() }
		if !tc.enum_value_matches(cond.value, enum_name_id) {
			enum_name := flat.source_type_name(enum_name_id)
			tc.record_error_at(.unknown_field, 'unknown enum field `${cond.value.all_after_last('.')}` for `${enum_name}`', cond_id, cond.pos)
		}
		return
	}
	clean_condition := unalias_type(condition_type)
	if clean_condition is Unknown || tc.type_compatible(condition_type, subject_type)
		|| tc.type_compatible(subject_type, condition_type) {
		return
	}
	tc.record_error_at(.condition_mismatch, 'cannot match `${clean_subject.name()}` with `${tc.diagnostic_expr_type_name(cond_id, condition_type)}`', cond_id, cond.pos)
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
	mut context_type := tc.expected_context_for_expr(id) or { builtin_void_type }
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
			if _ := tc.wrapped_multi_return_value_groups(id, multi_context.types.len, false, context_type) {
				return
			}
		} else {
			if _ := tc.multi_expr_tail_value_groups(id, multi_context.types.len, false) {
				return
			}
			// A branch can hand over the whole tuple at once — a call that returns it,
			// say — instead of listing its parts, and then there are no separate value
			// nodes to collect from that branch. Its types still line up with the ones
			// the other branches list, which is all this diagnostic needs to know.
			if _ := tc.multi_expr_tail_type_groups(id, multi_context.types.len) {
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
	cast_type := tc.parse_type(cast_node.source_value_id())
	if !infix_power_type_is_numeric(cast_type) {
		return
	}
	if first_cast_index > 0 {
		first_tail := tc.a.node(tails[0])
		// An int literal branch defaults to `int`, so a later `int` (or `i32`) cast fits.
		if first_tail.kind == .int_literal && cast_type.source_type_id() !in [
			flat.source_type_int,
			flat.source_type_i32,
		] {
			tc.record_error_at(.if_branch_mismatch, 'the type of the last expression of the first match branch was `int literal`, which is not compatible with `${cast_type.name()}`', cast_id, cast_node.pos)
		}
		return
	}
	for i in 1 .. tails.len {
		tail_id := tails[i]
		tail := tc.a.node(tail_id)
		if tail.kind == .cast_expr {
			tail_type := tc.parse_type(tail.source_value_id())
			if tail_type.source_type_id() != cast_type.source_type_id() {
				tc.record_error_at(.if_branch_mismatch, 'the type of the last expression in the first match branch was an explicit `${cast_type.name()}`, not `${tail_type.name()}`', tail_id, tail.pos)
				return
			}
			continue
		}
		if literal := tc.integer_literal_source(tail_id) {
			if type_range := integer_type_range(cast_type) {
				if integer_literal_outside_range(literal.replace('_', ''), type_range) {
					tc.record_error_at(.if_branch_mismatch, '${literal} does not fit the range of `${cast_type.name()}`', tail_id, tail.pos)
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
			if comma_types := tc.branch_explicit_comma_tail_types(branch_id) {
				tail_types << Type(MultiReturn{ types: comma_types })
			} else {
				tail_types << tc.match_branch_tail_diagnostic_type(subject_key, subject_type, branch, tail_id)
			}
		}
	}
	if tail_types.len != tails.len {
		return
	}
	mut context_type := tc.expected_context_for_expr(id) or { builtin_void_type }
	parent_id := tc.direct_parent_id(id)
	if context_type is Void && tc.valid_node_id(parent_id)
		&& tc.a.node(parent_id).kind == .return_stmt {
		context_type = tc.fn_context.return_type
	}
	if context_type !is Void {
		if tc.branches_compatible_with(id, context_type) {
			return
		}
		wrapper := unalias_type(context_type)
		expected_is_option := wrapper is OptionType
		expected_is_result := wrapper is ResultType
		wrapped_context := expected_is_option || expected_is_result
		expected_type := match wrapper {
			OptionType, ResultType { wrapper.base_type }
			else { &context_type }
		}
		clean_expected := unalias_type(expected_type)
		if wrapped_context {
			for i, tail_id in tails {
				actual := tail_types[i]
				clean_actual := unalias_type(actual)
				same_wrapper := if expected_is_option && clean_actual is OptionType {
					tc.type_compatible(clean_actual.base_type, expected_type)
				} else if expected_is_result && clean_actual is ResultType {
					tc.type_compatible(clean_actual.base_type, expected_type)
				} else {
					false
				}
				if actual is Void || actual is Unknown || is_ierror_type(actual)
					|| tc.type_compatible_with_ierror_payload(actual)
					|| tc.expr_never_returns(tail_id)
					|| tc.type_compatible(actual, expected_type) || same_wrapper
					|| (expected_is_option && tc.branch_tail_is_none_literal(tail_id)) {
					continue
				}
				tc.record_match_branch_return_type_mismatch(tail_id, expected_type, actual)
				return
			}
			return
		}
		if clean_expected is SumType {
			for i, tail_id in tails {
				actual := tail_types[i]
				if tc.type_is_same_sum_type(actual, clean_expected)
					|| tc.type_name_is_direct_sum_variant(actual, clean_expected) {
					continue
				}
				tc.record_match_branch_return_type_mismatch(tail_id, expected_type, actual)
				return
			}
			return
		}
	}
	mut expected := tail_types[0]
	if expected is MultiReturn {
		if _ := tc.multi_expr_tail_types(id, expected.types.len) {
			return
		}
	}
	if expected is Void || expected is Unknown {
		return
	}
	for i in 1 .. tails.len {
		tail_id := tails[i]
		actual := tail_types[i]
		clean_expected := unalias_type(expected)
		clean_actual := unalias_type(actual)
		if clean_expected is MultiReturn && clean_actual is MultiReturn
			&& clean_expected.types.len == clean_actual.types.len {
			mut promoted := []Type{cap: clean_expected.types.len}
			for j, current in clean_expected.types {
				if !multi_tail_wrappers_match(current, clean_actual.types[j]) {
					tc.record_match_branch_return_type_mismatch(tail_id, expected, actual)
					return
				}
				promoted << tc.promoted_multi_tail_type(current, clean_actual.types[j]) or { break }
			}
			if promoted.len == clean_expected.types.len {
				expected = Type(MultiReturn{ types: promoted })
				continue
			}
		}
		if inferred := inferred_contextual_if_type(expected, actual) {
			expected = inferred
			continue
		}
		if actual is Void || actual is Unknown
			|| tc.if_branch_type_compatible_with_context(actual, tail_id, expected)
			|| (tc.type_compatible(actual, expected) && tc.type_compatible(expected, actual))
			|| tc.translated_char_pointer_expr_compatible(tail_id, actual, expected) {
			continue
		}
		tc.record_match_branch_return_type_mismatch(tail_id, expected, actual)
		return
	}
}

fn (tc &TypeChecker) match_branch_tail_diagnostic_type(subject_key string, subject_type Type, branch flat.Node, tail_id flat.NodeId) Type {
	n_conds := if branch.value == 'else' { 0 } else { branch.value.int() }
	if subject_key != '' && n_conds == 1 && branch.children_count > 0 {
		cond := tc.a.child_node(&branch, 0)
		if pattern_source_id := tc.match_type_pattern(cond) {
			smartcast_type_id := if subject_type is SumType {
				tc.sum_variant_type_for_pattern(subject_type.source_type_id(), pattern_source_id) or { u32(0) }
			} else if subject_type is Interface {
				tc.resolve_interface_match_pattern(pattern_source_id) or { u32(0) }
			} else {
				u32(0)
			}
			if smartcast_type_id != 0 {
				smartcast_type := tc.parse_type(smartcast_type_id)
				tail := tc.a.node(tail_id)
				// `match key.handle { Variant { key.handle } }` names the subject like an ident.
				if tail.kind in [.ident, .selector] && tc.expr_key_matches(tail_id, subject_key) {
					return *smartcast_type
				}
				if tail.kind == .prefix && tail.op == .amp && tail.children_count > 0 {
					child_id := tc.a.child(tail, 0)
					if tc.expr_key_matches(child_id, subject_key) {
						if subject_type is Interface {
							return *smartcast_type
						}
						return Type(Pointer{
							base_type: tc.intern_type_reference(smartcast_type)
						})
					}
				}
			}
		}
	}
	return tc.resolve_type(tail_id)
}

fn (tc &TypeChecker) type_name_is_direct_sum_variant(actual &Type, expected SumType) bool {
	actual_id := actual.source_type_id()
	base := tc.sum_base_name(expected.source_type_id())
	variants := tc.sum_types.type_ids(base) or { []u32{} }
	if actual_id in variants {
		return true
	}
	actual_short_id := flat.source_type_syntax(actual_id).unqualified()
	for variant_source_id in variants {
		if actual_short_id == flat.source_type_syntax(variant_source_id).unqualified() {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) type_is_same_sum_type(actual &Type, expected SumType) bool {
	clean := unalias_type(actual)
	clean_ref := clean
	if clean !is SumType {
		return false
	}
	return tc.type_compatible(clean_ref, Type(expected))
		&& tc.type_compatible(Type(expected), clean_ref)
}

fn (mut tc TypeChecker) record_match_branch_return_type_mismatch(id flat.NodeId, expected Type, actual Type) {
	if tc.type_name(expected) == tc.diagnostic_expr_type_id(id, actual) {
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
	tc.record_error_at(.if_branch_mismatch, 'return type mismatch, it should be `${expected.name()}`, but it is instead `${tc.diagnostic_expr_type_name(id, actual)}`', id, pos)
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
	mut range_type := *low_type
	low_is_literal := tc.range_endpoint_is_literal(low_id)
	high_is_literal := tc.range_endpoint_is_literal(high_id)
	if low_type.source_type_id() != high_type.source_type_id() {
		if low_is_literal && !high_is_literal && low_type.is_integer() && high_type.is_integer() {
			range_type = high_type
		} else if high_is_literal && !low_is_literal && low_type.is_integer()
			&& high_type.is_integer() {
			range_type = low_type
		} else if low_is_literal && high_is_literal && low_type.is_integer()
			&& high_type.is_integer() {
			range_type = if low_type is Rune || high_type is Rune {
				builtin_rune_type
			} else {
				*low_type
			}
		} else {
			tc.record_error_with_details_at(.condition_mismatch, 'the low and high parts of a range expression, should have matching types', cond_id, tc.match_condition_diagnostic_pos(cond_id), [
				'\n low part type: ${tc.match_range_endpoint_type_name(low_id, low_type)}',
				'high part type: ${tc.match_range_endpoint_type_name(high_id, high_type)}',
			])
		}
	}
	clean_subject := unalias_type(subject_type)
	rune_range_matches_byte := range_type is Rune && clean_subject.source_type_id() in [
		flat.source_type_u8,
		flat.source_type_char,
	]
	integer_literal_range_matches_integer_subject := low_is_literal && high_is_literal
		&& range_type.is_integer() && range_type !is Rune && clean_subject.is_integer()
	integer_literal_range_matches_enum_subject := low_is_literal && high_is_literal
		&& range_type.is_integer() && range_type !is Rune && clean_subject is Enum
	if clean_subject !is Unknown && range_type !is Unknown
		&& clean_subject.source_type_id() != range_type.source_type_id() && !rune_range_matches_byte
		&& !integer_literal_range_matches_integer_subject
		&& !integer_literal_range_matches_enum_subject {
		tc.record_error_with_details_at(.condition_mismatch, 'the range type and the match condition type should match', cond_id, tc.match_condition_diagnostic_pos(cond_id), [
			'\nmatch condition type: ${clean_subject.name()}',
			'          range type: ${range_type.name()}',
		])
	}
	tc.check_match_range_values(cond_id, low_id, high_id)
	_ = subject_id
}

fn (tc &TypeChecker) match_range_endpoint_type_name(id flat.NodeId, typ &Type) string {
	if tc.range_endpoint_is_literal(id) && typ.is_integer() && typ !is Rune {
		return 'int literal'
	}
	return typ.name()
}

fn (mut tc TypeChecker) check_match_range_values(cond_id flat.NodeId, low_id flat.NodeId, high_id flat.NodeId) {
	low := tc.match_condition_int_value(low_id) or {
		tc.record_error_at(.condition_mismatch, 'match branch range expressions need the start value to be known at compile time (only enums, const or literals are supported)', cond_id, tc.a.node(low_id).pos)
		return
	}
	high := tc.match_condition_int_value(high_id) or {
		tc.record_error_at(.condition_mismatch, 'match branch range expressions need the end value to be known at compile time (only enums, const or literals are supported)', cond_id, tc.a.node(high_id).pos)
		return
	}
	if low > high {
		tc.record_error_at(.condition_mismatch, 'the start value `${low}` should be lower than the end value `${high}`', cond_id, tc.match_condition_diagnostic_pos(cond_id))
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
	// Same test-file rationale as in record_constant_condition_diagnostics:
	// constant match branches in tests are usually deliberate. The
	// self-comparison warning below is kept.
	if !is_regular_v_test_file(tc.cur_file) {
		tc.record_notice_at(.condition_mismatch, if value {
			'match is always true'
		} else {
			'match is always false'
		}, cond_id, cond.pos)
	}
	if value && has_following_branch && tc.same_simple_comparison_expr(subject_id, cond_id) {
		tc.record_warning_at(.condition_mismatch, 'self-comparison match branch is always true; following branches may be unreachable', cond_id, cond.pos)
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
			tc.record_match_duplicate(branch, branch_index, cond_index, cond_id, owner_branch, value.str())
		}
		seen_ranges << MatchSeenRange{
			start:  low
			end:    high
			branch: branch_index
		}
		return
	}
	if value := tc.match_condition_literal_int_value(cond_id) {
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
			tc.record_match_duplicate(branch, branch_index, cond_index, cond_id, owner_branch, value.str())
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
	tc.record_error_at(.condition_mismatch, 'match case `${display}` is handled more than once', cond_id, pos)
}

fn (tc &TypeChecker) match_condition_pattern_key(id flat.NodeId) (string, string) {
	node := tc.a.node(id)
	if pattern_source_id := tc.match_type_pattern(node) {
		pattern := flat.source_type_name(pattern_source_id)

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
		key := tc.match_selector_condition_key(id)
		if key.len > 0 {
			return 'selector:${key}', key
		}
	}
	if node.kind in [.cast_expr, .as_expr] && node.children_count == 1 {
		operand, _ := tc.match_condition_pattern_key(tc.a.child(node, 0))
		if operand.len > 0 {
			target_text := node.value_text()
			return '${node.kind}:${target_text.len}:${target_text}${operand}', tc.source_text_for_node(id)
		}
	}
	if node.kind == .infix && node.children_count == 2 {
		left, _ := tc.match_condition_pattern_key(tc.a.child(node, 0))
		right, _ := tc.match_condition_pattern_key(tc.a.child(node, 1))
		if left.len > 0 && right.len > 0 {
			// Selector spans can start at the final field; keep the full operands in the key.
			return 'infix:${node.op}:${left.len}:${left}${right}', tc.source_text_for_node(id)
		}
	}
	text := tc.source_text_for_node(id)
	if text.len == 0 {
		return '', ''
	}
	return '${node.kind}:${text}', text
}

fn (tc &TypeChecker) match_selector_condition_key(id flat.NodeId) string {
	if !tc.valid_node_id(id) {
		return ''
	}
	node := tc.a.node(id)
	if node.kind == .paren && node.children_count > 0 {
		return tc.match_selector_condition_key(tc.a.child(node, 0))
	}
	if node.kind == .as_expr && node.children_count > 0 {
		base := tc.match_selector_condition_key(tc.a.child(node, 0))
		return if base.len > 0 { '${base} as ${node.value_text()}' } else { '' }
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.match_selector_condition_key(tc.a.child(node, 0))
		return if base.len > 0 { '${base}.${node.value}' } else { '' }
	}
	if node.kind == .index && node.children_count >= 2 {
		base := tc.match_selector_condition_key(tc.a.child(node, 0))
		mut part_id := tc.a.child(node, 1)
		mut part_node := tc.a.node(part_id)
		for part_node.kind == .paren && part_node.children_count > 0 {
			part_id = tc.a.child(part_node, 0)
			part_node = tc.a.node(part_id)
		}
		part := if part_node.kind in [.selector, .index, .as_expr] {
			tc.match_selector_condition_key(part_id)
		} else {
			tc.expr_key_part(part_id)
		}
		if base.len > 0 && part.len > 0 {
			return '${base}[${part_node.kind}:${part.len}:${part}]'
		}
		return ''
	}
	return tc.expr_key(id)
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

// A single value case is compared by number only when it is spelled as a literal.
// Named consts are compared by name, so `bsp_char {}` before `0...0x1F {}` is the
// usual "specific case first" idiom, and two consts that share a value stay distinct.
fn (tc &TypeChecker) match_condition_literal_int_value(id flat.NodeId) ?int {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	match node.kind {
		.int_literal, .char_literal, .enum_val {
			return tc.match_condition_int_value(id)
		}
		.paren, .cast_expr, .prefix {
			if node.children_count == 0 {
				return none
			}
			_ = tc.match_condition_literal_int_value(tc.a.child(node, 0)) or { return none }
			return tc.match_condition_int_value(id)
		}
		else {
			return none
		}
	}
}

fn match_char_literal_value(value string) ?int {
	if value == '' {
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
	// A rune names its code point in hex, in 16- or 32-bit unicode, or in octal, and
	// every one of those spellings has to be read as the number it is. Taking only
	// the letter after the backslash would make `\u2028` and `\u000c` both the letter
	// `u`, and two cases with nothing in common would be read as the same one.
	if value[1] in [`x`, `u`, `U`] && value.len > 2 {
		return v_int_literal_value('0x${value[2..]}')
	}
	if value[1].is_oct_digit() {
		return v_int_literal_value('0o${value[1..]}')
	}
	return match value[1] {
		`n` { int(`\n`) }
		`t` { int(`\t`) }
		`r` { int(`\r`) }
		`e` { 27 }
		`\\` { int(`\\`) }
		`'` { int(`'`) }
		`"` { int(`"`) }
		`$` { int(`$`) }
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
		pattern_source_id_13 := tc.match_type_pattern(cond) or { return false }

		if _ := tc.sum_variant_type_for_pattern(subject.source_type_id(), pattern_source_id_13) {
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
		pattern_source_id_14 := tc.match_type_pattern(cond) or { return }

		variant_name_source_id := tc.sum_variant_type_for_pattern(subject.source_type_id(), pattern_source_id_14) or { return }

		variant_type := tc.parse_type(variant_name_source_id)
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
		tc.record_error_at(.return_mismatch, 'cannot use `${actual_name}` as type `${expected_sum.name()}` in return argument', value_id, tc.a.node(value_id).pos)
	}
}

fn (tc &TypeChecker) multi_match_common_field_smartcasts(subject SumType, branch &flat.Node, n_conds int, subject_key string) []LocalBinding {
	mut common := []LocalBinding{}
	for i in 0 .. n_conds {
		cond := tc.a.node(tc.a.child(branch, i))
		pattern_source_id_15 := tc.match_type_pattern(cond) or { return []LocalBinding{} }

		variant_name_source_id := tc.sum_variant_type_for_pattern(subject.source_type_id(), pattern_source_id_15) or {
			return []LocalBinding{}
		}

		variant_type := unalias_type(tc.parse_type(variant_name_source_id))
		if variant_type !is Struct {
			return []LocalBinding{}
		}
		variant := variant_type as Struct
		if i == 0 {
			for field in tc.structs[variant.source_type_id()] or { return []LocalBinding{} } {
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
			field_type := tc.struct_field_type(variant.source_type_id(), field_name) or { continue }
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

fn (tc &TypeChecker) multi_interface_match_common_interface(subject Interface, branch &flat.Node, n_conds int) ?u32 {
	mut variants := []u32{cap: n_conds}
	for i in 0 .. n_conds {
		cond := tc.a.node(tc.a.child(branch, i))
		pattern_id := tc.match_type_pattern(cond) or { return none }
		variant_id := tc.resolve_interface_match_pattern(pattern_id) or { return none }
		variants << variant_id
	}
	subject_id := tc.interface_metadata_name(subject.source_type_id())
	mut best_id := u32(0)
	mut best_score := -1
	for iface_name, _ in tc.interface_names {
		name_id := tc.interface_metadata_name(iface_name)
		if name_id == subject_id || tc.interface_has_no_requirements(name_id) {
			continue
		}
		mut implements_all := true
		for variant_id in variants {
			concrete_id := method_type_name(unwrap_pointer(tc.parse_type(variant_id)))
			if concrete_id == 0 || !tc.named_type_implements_interface(concrete_id, name_id) {
				implements_all = false
				break
			}
		}
		if !implements_all {
			continue
		}
		score := tc.interface_abstract_method_names(name_id).len + tc.interface_field_list(name_id).len
		if score > best_score || (score == best_score
			&& (best_id == 0 || flat.source_type_name(name_id) < flat.source_type_name(best_id))) {
			best_id = name_id
			best_score = score
		}
	}
	if best_id == 0 {
		return none
	}
	return best_id
}

fn (tc &TypeChecker) resolve_interface_match_pattern(pattern_id u32) ?u32 {
	for candidate_id in tc.interface_match_pattern_candidates(pattern_id) {
		if interface_pattern_is_collapsed_container_id(candidate_id) {
			container_type := tc.parse_type(candidate_id)
			if container_type is Array || container_type is Map {
				return container_type.source_type_id()
			}
			continue
		}
		if is_builtin_type_name_id(candidate_id) {
			return candidate_id
		}
		if tc.pattern_type_known(candidate_id) {
			return candidate_id
		}
		if tc.type_symbol_known(candidate_id) {
			return candidate_id
		}
	}
	return none
}

fn interface_pattern_is_collapsed_container_id(pattern_id u32) bool {
	shape := flat.source_type_shape(trimmed_source_type_id(pattern_id))
	return shape.has(.array_type) || shape.has(.map_type)
}

fn (tc &TypeChecker) resolve_interface_pattern_interface(pattern_id u32) ?u32 {
	for candidate_id in tc.interface_match_pattern_candidates(pattern_id) {
		name_id := tc.interface_metadata_name(candidate_id)
		if name_id in tc.interface_names {
			return name_id
		}
		typ := unalias_type(tc.parse_type(candidate_id))
		if typ is Interface {
			return typ.source_type_id()
		}
	}
	return none
}

fn (tc &TypeChecker) pattern_type_known(pattern_id u32) bool {
	clean_id := trimmed_source_type_id(pattern_id)
	shape := flat.source_type_shape(clean_id)
	if shape.has(.array_type) || shape.has(.map_type) || shape.has(.leading_bracket)
		|| shape.has(.pointer) || flat.source_type_syntax(clean_id).function_space_prefix()
		|| shape.has(.function_compact) {
		return tc.parse_type(clean_id) !is Unknown
	}
	return false
}

fn (tc &TypeChecker) resolve_ierror_match_pattern(pattern_id u32) ?u32 {
	for candidate_id in tc.interface_match_pattern_candidates(pattern_id) {
		candidate_type := unwrap_pointer(tc.parse_type(candidate_id))
		candidate_name_id := method_type_name(candidate_type)
		if candidate_name_id != 0 && tc.named_type_compatible_with_ierror(candidate_name_id) {
			return candidate_id
		}
	}
	return none
}

fn (tc &TypeChecker) interface_match_pattern_candidates(pattern_id u32) []u32 {
	clean_id := trimmed_source_type_id(pattern_id)
	if flat.source_type_shape(clean_id).has(.pointer) {
		base_id := trimmed_source_type_id(flat.source_type_syntax(clean_id).raw_base())
		if base_id == 0 {
			return [clean_id]
		}
		return tc.interface_match_pattern_candidates(base_id).map(flat.source_type_composition_one(.pointer, it))
	}
	mut candidates := []u32{}
	if !flat.source_type_shape(pattern_id).has(.dotted) {
		mut has_scoped_candidate := false
		if resolved_id := tc.resolve_selective_import_type_symbol_id(pattern_id) {
			candidates << resolved_id
			has_scoped_candidate = true
		}
		if tc.source_declares_type_in_scope(pattern_id, tc.cur_file, tc.cur_module) {
			candidates << pattern_id
			has_scoped_candidate = true
		}
		if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
			local_id := flat.source_type_composition_two(.qualified, flat.source_type_id(tc.cur_module), pattern_id)
			if tc.type_symbol_known(local_id) {
				candidates << local_id
				has_scoped_candidate = true
			}
		}
		if !has_scoped_candidate {
			candidates << pattern_id
		}
	} else if resolved_id := tc.resolve_import_alias_pattern(pattern_id) {
		candidates << resolved_id
		candidates << pattern_id
	} else {
		candidates << pattern_id
	}
	qpattern_id := tc.qualify_type_name(pattern_id)
	if qpattern_id != pattern_id {
		candidates << qpattern_id
	}
	mut result := []u32{}
	mut seen := map[u32]bool{}
	for candidate_id in candidates {
		if candidate_id == 0 || candidate_id in seen {
			continue
		}
		seen[candidate_id] = true
		result << candidate_id
	}
	return result
}

fn (tc &TypeChecker) resolve_import_alias_pattern(pattern_id u32) ?u32 {
	if !flat.source_type_shape(pattern_id).has(.dotted) {
		return none
	}
	syntax := flat.source_type_syntax(pattern_id)
	if syntax.first_dot_prefix() == 0 { return none }
	resolved := tc.resolve_import_alias(flat.source_type_name(syntax.first_dot_prefix())) or { return none }
	return flat.source_type_composition_two(.qualified, flat.source_type_id(resolved), syntax.first_dot_suffix())
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
		tc.record_error_at(.assignment_mismatch, '`${expr_node.value}` is immutable, declare it with `mut` to make it mutable', expr_id, tc.node_value_diagnostic_pos(expr_id))
	}
	if expr_node.kind == .none_expr && node.value_text() == 'none' {
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
						full_pos = token.new_span(expr_node.pos.id, expr_node.pos.offset, op_start + op.len + target_relative + 4)
					}
				}
			}
		}
		tc.record_error_at(.condition_mismatch, 'invalid operator `${op}` to `none` and `none`', id, full_pos)
		tc.record_error_at(.condition_mismatch, '`${op}` can only be used with interfaces and sum types', id, op_pos)
		return
	}
	pattern_id := node.source_value_id()
	pattern_syntax := flat.source_type_syntax(pattern_id)
	if flat.source_type_shape(pattern_id).has(.dotted) {
		module_alias := flat.source_type_name(pattern_syntax.first_dot_prefix())
		if flat.source_type_syntax(pattern_syntax.first_dot_prefix()).lowercase_value_ident()
			&& module_alias != tc.cur_module
			&& tc.current_file_import_path_for_alias(module_alias) == none {
			tc.record_error_at(.unknown_type, 'unknown module `${module_alias}`', id, tc.type_diagnostic_pos(id, module_alias))
			return
		}
	}
	// `x is T` in a generic template stays undecided until monomorphization.
	// Only defer inside an actual generic function: in a non-generic one an
	// unknown single-letter pattern is a real error and must be validated.
	if node.source_value_id() != 0 && tc.type_text_has_generic_placeholder(node.source_value_id())
		&& tc.cur_fn_is_generic_template() {
		return
	}
	// A `$for v in T.variants` loop variable is substituted by the comptime
	// unroll; `val is v` cannot be validated against the raw name.
	if node.value_text() in tc.cur_comptime_variant_loop_vars || (flat.source_type_shape(pattern_id).has(.dotted)
		&& flat.source_type_name(pattern_syntax.unqualified()) in tc.cur_comptime_variant_loop_vars) {
		return
	}
	raw_expr_type := tc.resolve_type(expr_id)
	if raw_expr_type is OptionType {
		tc.record_error_at(.condition_mismatch, '${tc.source_text_for_node(expr_id)} is an Optional, it needs to be unwrapped first', expr_id, tc.node_value_diagnostic_pos(expr_id))
		return
	}
	mut expr_type := *unalias_type(unwrap_pointer(raw_expr_type))
	if expr_type is Interface && node.value_text() != 'none'
		&& tc.is_expr_used_for_branch_smartcast(id)
		&& tc.nonmut_mutable_interface_smartcast(expr_id, node.source_value_id()) {
		if tc.interface_has_no_requirements(expr_type.source_type_id()) {
			tc.record_notice_at(.condition_mismatch, 'smartcasting requires either an immutable value, or an explicit mut keyword before the value', expr_id, expr_node.pos)
		}
		tc.record_error_at(.condition_mismatch, 'smart casting a mutable interface value requires `if mut ${tc.source_text_for_node(expr_id)} is ...`', expr_id, expr_node.pos)
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
		if node.source_value_id() != 0 {
			pointer_sum_variant := flat.source_type_shape(pattern_id).has(.pointer) && raw_expr_type is Pointer
				&& tc.sum_variant_type_for_pattern(expr_type.source_type_id(), pattern_syntax.raw_base()) != none
			sum_syntax := flat.source_type_syntax(expr_type.source_type_id())
			open_generic_pattern := sum_syntax.application_valid() && node.value_text() in tc.sum_params_for_base(sum_syntax.application_base())
			if (tc.sum_variant_type_for_pattern(expr_type.source_type_id(), node.source_value_id()) == none
				|| open_generic_pattern) && !pointer_sum_variant {
				if tc.should_diagnose(id) {
					diagnostic_pattern := if node.value_text().contains('.') {
						node.value_text()
					} else if tc.cur_module !in ['', 'main', 'builtin'] {
						'${tc.cur_module}.${node.value_text()}'
					} else {
						node.value_text()
					}
					diagnostic_pos := if node.value_text().contains('.') {
						tc.type_diagnostic_pos(id, node.value_text().all_before('.'))
					} else {
						tc.node_value_diagnostic_pos(id)
					}
					if !tc.type_name_known(node.source_value_id()) && !(tc.checker_fixture_mode
						&& node.value_text().len == 1 && node.value_text()[0].is_capital()) {
						tc.record_error_at(.unknown_type, 'is: type `${diagnostic_pattern}` does not exist', id, diagnostic_pos)
					}
					tc.record_error_at(.condition_mismatch, '`${expr_type.name()}` has no variant `${diagnostic_pattern}`', id, diagnostic_pos)
				}
			}
		}
		return
	}
	if is_ierror_type(expr_type) {
		if node.source_value_id() != 0 {
			if node.value_text() == 'none' {
			} else if _ := tc.resolve_interface_pattern_interface(node.source_value_id()) {
				// Another interface is tested at run time, like on other interfaces.
			} else if _ := tc.resolve_ierror_match_pattern(node.source_value_id()) {
			} else if concrete_source_id := tc.resolve_interface_match_pattern(node.source_value_id()) {
				pos := tc.node_value_diagnostic_pos(id)
				concrete_type := tc.parse_type(concrete_source_id)
				tc.record_interface_implementation_error(.condition_mismatch, concrete_type, Interface{
					name_id: ierror_source_interface_name
				}, id, pos)
				tc.record_error_at(.condition_mismatch, "`${node.value_text()}` doesn't implement interface `IError`", id, pos)
			} else if tc.should_diagnose(id) {
				tc.record_error(.condition_mismatch, '`${node.value_text()}` is not compatible with `IError`', id)
			}
		}
		return
	}
	if expr_type is Interface {
		if node.source_value_id() != 0 {
			if interface_pattern_is_collapsed_container_id(node.source_value_id()) {
				container_type := tc.parse_type(node.source_value_id())
				if !tc.type_implements_interface(container_type, expr_type)
					&& tc.should_diagnose(id) {
					tc.record_error(.condition_mismatch, '`${node.value_text()}` is not compatible with interface `${expr_type.name()}`', id)
				}
			} else if _ := tc.resolve_interface_pattern_interface(node.source_value_id()) {
				// `iface is OtherIface` is a run-time test, valid even when no type in the
				// program implements both interfaces.
			} else if concrete_source_id := tc.resolve_interface_match_pattern(node.source_value_id()) {
				concrete_type := unwrap_pointer(tc.parse_type(concrete_source_id))
				concrete_name_id := method_type_name(concrete_type)
				if concrete_type !is Interface
					&& !tc.named_type_implements_interface(concrete_name_id, expr_type.source_type_id())
					&& tc.should_diagnose(id) {
					pos := tc.node_value_diagnostic_pos(id)
					tc.record_interface_implementation_error(.condition_mismatch, concrete_type,
						expr_type, id, pos)
					tc.record_error_at(.condition_mismatch, "`${node.value_text()}` doesn't implement interface `${expr_type.name()}`", id, pos)
				}
			} else if tc.should_diagnose(id) {
				tc.record_error(.condition_mismatch, 'unknown type `${node.value_text()}`', id)
			}
		}
		return
	}
	if expr_type is Unknown {
		return
	}
	if tc.should_diagnose(id) {
		tc.record_error(.condition_mismatch, '`is` can only be used with sum type or interface values, not `${expr_type.name()}`', id)
	}
}

fn (tc &TypeChecker) is_expr_used_for_branch_smartcast(id flat.NodeId) bool {
	mut current := id
	mut positive := true
	for _ in 0 .. 128 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) { return false }
		parent := tc.a.node(parent_id)
		if parent.kind == .paren {
			current = parent_id
			continue
		}
		if parent.kind == .prefix && parent.op == .not {
			positive = !positive
			current = parent_id
			continue
		}
		if parent.kind == .infix && parent.op in [.logical_and, .logical_or] {
			// Only a true AND operand or a false OR operand can narrow the value.
			if (parent.op == .logical_and) != positive { return false }
			if parent.children_count > 0 && tc.a.child(parent, 0) == current {
				return true
			}
			current = parent_id
			continue
		}
		if parent.kind == .if_expr {
			if parent.children_count == 0 || tc.a.child(parent, 0) != current { return false }
			if positive { return true }
			return (parent.children_count > 2 && tc.a.child_node(parent, 2).kind != .empty)
				|| (parent.children_count > 1 && tc.stmt_definitely_returns(tc.a.child(parent, 1)))
		}
		if parent.kind == .for_stmt {
			return positive && parent.children_count > 1 && tc.a.child(parent, 1) == current
		}
		return false
	}
	return false
}

// branch_tail_type supports branch tail type handling for TypeChecker.
fn (tc &TypeChecker) branch_tail_type(id flat.NodeId) Type {
	if !tc.valid_node_id(id) {
		return builtin_void_type
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .if_expr {
		return tc.if_expr_tail_type(id)
	}
	if node.kind == .block {
		if node.children_count == 0 {
			return builtin_void_type
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return builtin_void_type
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
			return builtin_void_type
		}
		last_id := tc.a.child(&node, node.children_count - 1)
		if !tc.valid_node_id(last_id) {
			return builtin_void_type
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
	mut result := builtin_void_type
	for tc.valid_node_id(cur_id) {
		node := tc.a.nodes[int(cur_id)]
		if node.kind != .if_expr {
			return tc.choose_translated_if_tail_type(id, result, tc.branch_tail_type(cur_id))
		}
		if node.children_count > 1 {
			cond_id := tc.a.child(&node, 0)
			then_id := tc.a.child(&node, 1)
			mut smartcasts := tc.extract_smartcasts(cond_id)
			mut then_type := tc.branch_tail_type_with_smartcasts(then_id, smartcasts)
			if type_contains_unknown(then_type) && tc.valid_node_id(cond_id) {
				// Outside the checked scope (e.g. a const initializer) the guard's
				// `x` in `if x := opt { x }` is unbound; type it from `opt`.
				guard_bindings := tc.if_guard_query_bindings(tc.a.nodes[int(cond_id)])
				if guard_bindings.len > 0 {
					smartcasts << guard_bindings
					then_type = tc.branch_tail_type_with_smartcasts(then_id, smartcasts)
				}
			}
			result = tc.choose_translated_if_tail_type(id, result, then_type)
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
		return tc.choose_translated_if_tail_type(id, result, else_type)
	}
	return result
}

fn (tc &TypeChecker) choose_translated_if_tail_type(id flat.NodeId, current Type, next Type) Type {
	if tc.translated_numeric_expr_compatible(id, current, next) {
		return tc.translated_common_numeric_type(current, next)
	}
	return tc.choose_if_tail_type(current, next)
}

// translated_integer_literal_type returns the C type of an integer literal in a translated file:
// `int`, then `unsigned int` (nondecimal only), then a signed or unsigned 64-bit type.
pub fn (tc &TypeChecker) translated_integer_literal_type(id flat.NodeId) ?Type {
	literal := (tc.integer_literal_source(id) or { return none }).replace('_', '').to_lower()
	value, parse_error := strconv.common_parse_uint2(literal, util.v_literal_parse_base(literal), 64)
	if parse_error != 0 {
		return none
	}
	// C considers unsigned int before wider signed types for nondecimal literals.
	// Decimal literals move directly from int to a wider signed type.
	if value <= u64(0x7fff_ffff) {
		return builtin_int_type
	}
	if (literal.starts_with('0x') || literal.starts_with('0o') || literal.starts_with('0b'))
		&& value <= u64(0xffff_ffff) {
		return builtin_u32_type
	}
	if value <= u64(0x7fff_ffff_ffff_ffff) {
		return builtin_i64_type
	}
	return builtin_u64_type
}

// translated_promoted_numeric_type applies C integral promotions in translated code.
pub fn (tc &TypeChecker) translated_promoted_numeric_type(typ Type) Type {
	clean := unalias_type(typ)
	if clean is Enum {
		backing := unalias_type(tc.inline_asm_enum_backing_type(clean.source_type_id()))
		return if translated_integer_bit_width(backing) < 32 { builtin_int_type } else { *backing }
	}
	if clean is Char || *clean == Type(bool_) {
		return Type(int_)
	}
	if clean is Primitive && clean.props.has(.integer) && clean.size > 0 && clean.size < 32 {
		return builtin_int_type
	}
	return *clean
}

// translated_promoted_shift_type applies integral promotion using C's int width.
pub fn (tc &TypeChecker) translated_promoted_shift_type(typ Type) Type {
	promoted := tc.translated_promoted_numeric_type(typ)
	return if promoted == builtin_int_type { builtin_i32_type } else { promoted }
}

fn translated_integer_bit_width(typ &Type) int {
	if typ is Primitive {
		return if typ.size == 0 { 32 } else { int(typ.size) }
	}
	if typ is ISize || typ is USize {
		return platform_int_bits()
	}
	return 32
}

fn translated_integer_is_unsigned(typ &Type) bool {
	if typ is Primitive {
		return typ.props.has(.unsigned)
	}
	return typ is USize || typ is Rune
}

// translated_common_numeric_type selects the arithmetic type after C numeric promotions.
pub fn (tc &TypeChecker) translated_common_numeric_type(lhs Type, rhs Type) Type {
	left := tc.translated_promoted_numeric_type(lhs)
	right := tc.translated_promoted_numeric_type(rhs)
	if left.is_float() || right.is_float() {
		if left.is_float() && !type_is_f32(left) {
			return builtin_f64_type
		}
		if right.is_float() && !type_is_f32(right) {
			return builtin_f64_type
		}
		return builtin_f32_type
	}
	left_bits := translated_integer_bit_width(left)
	right_bits := translated_integer_bit_width(right)
	left_unsigned := translated_integer_is_unsigned(left)
	right_unsigned := translated_integer_is_unsigned(right)
	if left_bits > right_bits {
		return left
	}
	if right_bits > left_bits {
		return right
	}
	if left_unsigned != right_unsigned {
		return if left_unsigned { left } else { right }
	}
	return left
}

// match_expr_tail_type supports match expression value type handling for TypeChecker.
fn (tc &TypeChecker) match_expr_tail_type(id flat.NodeId) Type {
	if !tc.valid_node_id(id) {
		return builtin_void_type
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .match_stmt || node.children_count < 2 {
		return builtin_void_type
	}
	subject_id := tc.a.child(&node, 0)
	subject_key := tc.expr_key(subject_id)
	subject_type := unalias_type(unwrap_pointer(tc.resolve_type(subject_id)))
	mut result := builtin_void_type
	mut incompatible_wrapper_void := builtin_void_type
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
	if current is SumType && tc.sum_variant_type_for_pattern(current.source_type_id(), next.source_type_id()) != none {
		return current
	}
	if next is SumType && tc.sum_variant_type_for_pattern(next.source_type_id(), current.source_type_id()) != none {
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
			// A branch that leaves the expression has no value to constrain. Its
			// return payload is checked against the enclosing function separately.
			if tc.branch_tail_never_returns(branch_id) {
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
			actual, unbound := tc.resolve_branch_tail_expr(branch_id, tail, expected,
				[]LocalBinding{})
			tc.smartcasts = saved_smartcasts
			if unbound || !tc.if_branch_type_compatible_with_context(actual, tail, expected) {
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
		then_id := tc.a.child(&node, 1)
		if !tc.branch_tail_never_returns(then_id) {
			guard_bindings := tc.if_guard_branch_bindings(tc.a.child(&node, 0))
			then_actual, unbound := tc.resolve_branch_tail_expr(then_id, then_tail, expected,
				guard_bindings)
			if unbound
				|| !tc.if_branch_type_compatible_with_context(then_actual, then_tail, expected) {
				return false
			}
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
		if tc.branch_tail_never_returns(else_id) {
			return true
		}
		if !tc.branch_failure_literal_matches_context(else_tail, expected) {
			return false
		}
		else_actual, unbound := tc.resolve_branch_tail_expr(else_id, else_tail, expected,
			[]LocalBinding{})
		return !unbound
			&& tc.if_branch_type_compatible_with_context(else_actual, else_tail, expected)
	}
	return false
}

// resolve_branch_tail_expr resolves the value `tail` of the branch `branch_id`
// against `expected` before the branch is checked, with the names the branch
// binds before its value in scope: the variables of an `if x := opt {` guard
// (`guard_bindings`), and the locals declared by the statements of the branch
// before `tail`. Without them the check of a call receiver in `tail`
// (`x.node().is_connected()`) reports those names as undefined. When `tail`
// uses one of them that cannot be typed yet, `tail` is not resolved, and the
// second result is true.
fn (mut tc TypeChecker) resolve_branch_tail_expr(branch_id flat.NodeId, tail flat.NodeId, expected Type, guard_bindings []LocalBinding) (Type, bool) {
	saved_scope := tc.cur_scope
	saved_mut_local_owners_shared := tc.fn_context.mut_local_owners_shared
	saved_mut_local_owners := tc.share_mut_local_owners()
	defer {
		tc.cur_scope = saved_scope
		tc.fn_context.mut_local_owners = saved_mut_local_owners
		tc.fn_context.mut_local_owners_shared = saved_mut_local_owners_shared
	}
	mut untyped := []string{}
	if guard_bindings.len > 0 {
		tc.cur_scope = new_scope(saved_scope)
		for binding in guard_bindings {
			owner := tc.cur_scope.insert_with_owner(binding.name, binding.typ)
			if binding.is_mut {
				tc.prepare_mut_local_owner_write()
				tc.fn_context.mut_local_owners[binding.name] = owner
			}
			if type_contains_unknown(binding.typ) {
				untyped << binding.name
			}
		}
	}
	tc.insert_branch_tail_locals(branch_id, saved_scope, mut untyped)
	for name in untyped {
		if tc.expr_uses_ident(tail, name) {
			return unknown_type('branch value uses an untyped branch variable'), true
		}
	}
	actual := tc.resolve_expr(tail, expected)
	return actual, false
}

// if_guard_branch_bindings returns the variables the guard `cond_id` of an
// `if x := opt {` binds, typed without checking the guard; a variable that
// cannot be typed that way is unknown. It returns nothing for other conditions.
fn (tc &TypeChecker) if_guard_branch_bindings(cond_id flat.NodeId) []LocalBinding {
	if !tc.valid_node_id(cond_id) {
		return []LocalBinding{}
	}
	cond := tc.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return []LocalBinding{}
	}
	mut bindings := tc.if_guard_query_bindings(cond)
	for lhs_id in tc.if_guard_lhs_ids(cond) {
		lhs := tc.a.node(lhs_id)
		if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_'
			|| bindings.any(it.name == lhs.value) {
			continue
		}
		bindings << LocalBinding{
			name:   lhs.value
			typ:    unknown_type('if guard variable before the guard is checked')
			is_mut: lhs.is_mut || cond.is_mut
		}
	}
	return bindings
}

// insert_branch_tail_locals inserts into a scope over `outer` the locals that
// the statements of the branch block or match branch `branch_id` declare
// before its value, typed from their initializers. The names that cannot be
// typed are added to `untyped`.
fn (mut tc TypeChecker) insert_branch_tail_locals(branch_id flat.NodeId, outer &Scope, mut untyped []string) {
	if !tc.valid_node_id(branch_id) {
		return
	}
	node := tc.a.nodes[int(branch_id)]
	if node.kind != .block && node.kind != .match_branch {
		return
	}
	first := if node.kind == .match_branch && node.value != 'else' { node.value.int() } else { 0 }
	if node.children_count <= first {
		return
	}
	last := int(node.children_count) - 1
	for i in first .. last {
		stmt := tc.a.child_node(&node, i)
		if stmt.kind != .decl_assign {
			continue
		}
		lhs_ids := tc.multi_assign_lhs_ids(*stmt)
		rhs_count := tc.multi_assign_rhs_count(*stmt)
		mut multi := []Type{}
		if lhs_ids.len > 1 && rhs_count == 1 {
			rhs_type := tc.resolve_type(tc.multi_assign_rhs_id(*stmt, 0))
			if rhs_type is MultiReturn {
				multi = rhs_type.types.clone()
			}
		}
		mut types := []Type{cap: lhs_ids.len}
		for j in 0 .. lhs_ids.len {
			mut typ := unknown_type('untyped branch local')
			if lhs_ids.len == 1 && rhs_count == 1 && stmt.typ != 0 {
				typ = tc.parse_type(stmt.typ)
			} else if j < multi.len {
				typ = multi[j]
			} else if rhs_count == lhs_ids.len {
				typ = tc.resolve_type(tc.multi_assign_rhs_id(*stmt, j))
			}
			types << typ
		}
		for j, lhs_id in lhs_ids {
			lhs := tc.a.node(lhs_id)
			if lhs.kind != .ident || lhs.value.len == 0 || lhs.value == '_' {
				continue
			}
			if voidptr(tc.cur_scope) == voidptr(outer) {
				tc.cur_scope = new_scope(outer)
			}
			typ := types[j]
			tc.cur_scope.insert(lhs.value, typ)
			if typ is Void || type_contains_unknown(typ) {
				untyped << lhs.value
			}
		}
	}
	last_id := tc.a.child(&node, last)
	if tc.valid_node_id(last_id) && tc.a.nodes[int(last_id)].kind == .block {
		tc.insert_branch_tail_locals(last_id, outer, mut untyped)
	}
}

fn (mut tc TypeChecker) apply_match_branch_context_smartcasts(subject_key string, subject_type Type, branch flat.Node) {
	if subject_key == '' || !valid_string_data(subject_key) || branch.kind != .match_branch {
		return
	}
	n_conds := if branch.value == 'else' { 0 } else { branch.value.int() }
	if n_conds != 1 || branch.children_count == 0 {
		return
	}
	cond := tc.a.node(tc.a.child(&branch, 0))
	pattern_id := tc.match_type_pattern(cond) or { return }
	smartcast_type_id := if subject_type is SumType {
		tc.sum_variant_type_for_pattern(subject_type.source_type_id(), pattern_id) or { pattern_id }
	} else if is_ierror_type(subject_type) {
		tc.resolve_ierror_match_pattern(pattern_id) or { pattern_id }
	} else if subject_type is Interface {
		tc.resolve_interface_match_pattern(pattern_id) or { pattern_id }
	} else {
		return
	}
	variant_type := tc.parse_type(smartcast_type_id)
	tc.smartcasts[subject_key] = if subject_type is Interface {
		interface_smartcast_variant_type(variant_type)
	} else {
		*variant_type
	}
}

// match_retests_smartcast_subject returns the declared interface type of a match
// subject that a smartcast narrowed to one concrete type, when the match tests its
// type again: after `if a is Dog { ... } else { panic('') }`, a
// `match a { Dog {} else {} }` dispatches on the value `a` holds, as in V 0.5.2.
// (A narrowed sum type value is not a reference, and is matched as it is.)
fn (tc &TypeChecker) match_retests_smartcast_subject(subject_id flat.NodeId, node flat.Node) ?Type {
	declared := tc.declared_smartcast_receiver_type(subject_id) or { return none }
	if unalias_and_unwrap_pointer_type(declared) !is Interface {
		return none
	}
	narrowed := unalias_and_unwrap_pointer_type(tc.resolve_type(subject_id))
	if narrowed is Interface || narrowed is SumType || narrowed is Unknown {
		return none
	}
	for i in 1 .. node.children_count {
		branch := tc.a.child_node(&node, i)
		if branch.kind != .match_branch || branch.value == 'else' {
			continue
		}
		for j in 0 .. branch.value.int() {
			if _ := tc.match_type_pattern(tc.a.child_node(branch, j)) {
				return declared
			}
		}
	}
	return none
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
		if tc.nonmut_mutable_interface_smartcast(expr_id, cond.source_value_id()) {
			return []LocalBinding{}
		}
		key := tc.expr_key(expr_id)
		if key.len > 0 && valid_string_data(key) && cond.source_value_id() != 0 {
			mut result := []LocalBinding{}
			result << LocalBinding{
				name: key
				typ:  tc.smartcast_target_type_for_is_expr(expr_id, cond.source_value_id())
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

fn (tc &TypeChecker) nonmut_mutable_interface_smartcast(expr_id flat.NodeId, pattern_id u32) bool {
	if !tc.valid_node_id(expr_id) {
		return false
	}
	if tc.resolve_interface_pattern_interface(pattern_id) != none {
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
		base := *opt_type.base_type
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
	if flat.source_type_syntax(node.source_value_id()).named_variant() && !tc.check_named_variant_init(id, node) {
		return
	}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child := tc.a.node(child_id)
		if child.kind == .directive && child.value.starts_with('embed_invalid_compression:') {
			compression_type := child.value.all_after(':')
			tc.record_error_at(.unknown_type, 'not supported compression type: .${compression_type}. supported: .none, .zlib', child_id, child.pos)
			return
		}
	}
	if node.source_value_id() == checker_tail_source_any {
		tc.record_error(.unknown_type, 'cannot use type `any` here', id)
		return
	}
	if flat.source_type_shape(node.source_value_id()).has(.channel_type) {
		elem_id := trimmed_source_type_id(flat.source_type_syntax(node.source_value_id()).raw_base())
		if flat.source_type_syntax(elem_id).named_type() && !tc.type_name_known(elem_id) {
			tc.record_unknown_decl_type(flat.source_type_name(elem_id), id)
		}
		tc.remember_expr_type(id, tc.parse_type(node.source_value_id()))
		return
	}
	is_optional_init := flat.source_type_shape(node.source_value_id()).has(.option_type)
	init_type_id := if is_optional_init {
		flat.source_type_syntax(node.source_value_id()).raw_base()
	} else {
		node.source_value_id()
	}
	init_type_text := flat.source_type_name(init_type_id)
	// Synthesized literals have no source spelling of their type: `$embed_file`, and
	// `Expr.Count(x)`, which the parser lowers to a hidden variant struct literal.
	raw_source_type_text := if node.source_value_id() == checker_tail_source_embed_file_data
		|| flat.source_type_syntax(node.source_value_id()).named_variant() {
		''
	} else {
		tc.source_text_for_node(id).all_before('{').trim_space().trim_left('?')
	}
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
	if tc.record_invalid_type_module_qualifier(source_type_text, id) {
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	parsed_init_type := tc.parse_type(init_type_id)
	clean_parsed_init_type := unalias_type(parsed_init_type)
	if clean_parsed_init_type is FnType {
		tc.record_error_at(.unknown_type, 'functions must be defined, not instantiated like structs', id, node.pos)
		// Declaration visibility is keyed by symbol strings.
		qualified_name := flat.source_type_name(tc.qualify_type_name(init_type_id))
		if visibility := tc.declaration_visibility[qualified_name] {
			if !visibility.is_pub {
				tc.record_error_at(.unknown_type, 'type `${init_type_text}` is private', id, node.pos)
			}
		}
		tc.remember_expr_type(id, parsed_init_type)
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		return
	}
	if clean_parsed_init_type is Array || clean_parsed_init_type is ArrayFixed {
		elem_type := array_like_elem_type(clean_parsed_init_type) or { builtin_void_type }
		init_expr_type := if is_optional_init {
			Type(OptionType{
				base_type: tc.intern_type_reference(parsed_init_type)
			})
		} else {
			*parsed_init_type
		}
		tc.remember_expr_type(id, init_expr_type)
		for i in 0 .. node.children_count {
			field := tc.a.child_node(&node, i)
			if field.kind != .field_init || field.children_count == 0 {
				continue
			}
			value_id := tc.a.child(field, 0)
			expected := if field.value in ['len', 'cap'] {
				builtin_int_type
			} else if field.value in ['', 'init'] {
				elem_type
			} else {
				tc.record_error_at(.unknown_field, 'wrong field `${field.value}`, expecting `len`, `cap`, or `init`', tc.a.child(&node, i), tc.node_value_diagnostic_pos(tc.a.child(&node, i)))
				continue
			}
			tc.check_node_with_expected_context(value_id, expected)
			actual := tc.resolve_expr(value_id, expected)
			if actual is Unknown || tc.expr_compatible(value_id, actual, expected) {
				continue
			}
			tc.record_error_at(.assignment_mismatch, 'invalid array element: expected `${expected.name()}`, not `${tc.diagnostic_expr_type_name(value_id, actual)}`', value_id, tc.array_element_diagnostic_pos(value_id))
		}
		return
	}
	init_syntax := flat.source_type_syntax(init_type_id)
	if init_syntax.application_valid() {
		generic_base_id := init_syntax.application_base()
		generic_base := flat.source_type_name(generic_base_id)
		generic_args := init_syntax.application_args()
		qualified_base := tc.qualify_type_name(generic_base_id)
		for arg_id in generic_args {
			if unalias_type(tc.parse_type(arg_id)) is MultiReturn {
				tc.record_error_at(.unsupported_generic, 'cannot use multi return as generic concrete type',
					id, tc.type_diagnostic_pos(id, flat.source_type_name(arg_id)))
				tc.register_synth_type(id, tc.parse_type(generic_base_id))
				for i in 0 .. node.children_count {
					tc.check_node(tc.a.child(&node, i))
				}
				return
			}
		}
		if (generic_base_id in tc.structs || qualified_base in tc.structs)
			&& generic_base_id !in tc.struct_generic_params && qualified_base !in tc.struct_generic_params {
			if generic_args.any(flat.source_type_syntax(it).bare_generic()) {
				tc.record_error_at(.unsupported_generic, 'a non generic struct `${generic_base.all_after_last('.')}` used like a generic struct', id, tc.type_diagnostic_pos(id, generic_base))
			} else {
				tc.record_error_at(.unsupported_generic, 'struct `${generic_base.all_after_last('.')}` is not a generic struct, cannot instantiate to the concrete types', id, tc.explicit_generic_args_diagnostic_pos(id))
			}
			tc.register_synth_type(id, tc.parse_type(generic_base_id))
			for i in 0 .. node.children_count {
				tc.check_node(tc.a.child(&node, i))
			}
			return
		}
		params := tc.struct_generic_params[generic_base_id] or {
			tc.struct_generic_params[qualified_base] or { []string{} }
		}
		if params.len > 0 && generic_args.len != params.len {
			in_generic_fn := tc.fn_context.generic_params.len > 0
				|| tc.current_fn_is_specialized_generic()
			message := if in_generic_fn {
				'generic struct init expects ${params.len} generic parameter, but got ${generic_args.len}'
			} else {
				'the number of generic types of struct `${generic_base.all_after_last('.')}` is inconsistent with the concrete types'
			}
			pos := if in_generic_fn {
				tc.struct_init_head_pos(node)
			} else {
				tc.explicit_generic_args_diagnostic_pos(id)
			}
			tc.record_error_at(.unsupported_generic, message, id, pos)
			tc.register_synth_type(id, tc.parse_type(generic_base_id))
			for i in 0 .. node.children_count {
				tc.check_node(tc.a.child(&node, i))
			}
			return
		}
		if params.len > 0 && tc.fn_context.generic_params.len > 0 {
			for arg_id in generic_args {
				if !flat.source_type_syntax(arg_id).bare_generic() { continue }
				arg := flat.source_type_name(arg_id)
				if arg !in tc.fn_context.generic_params {
					current := '(${tc.fn_context.generic_params.join(',')})'
					tc.record_error_at(.unsupported_generic, 'generic struct init type parameter `${arg}` must be within the parameters `${current}` of the current generic function', id, tc.struct_init_head_pos(node))
					tc.register_synth_type(id, tc.parse_type(generic_base_id))
					for i in 0 .. node.children_count {
						tc.check_node(tc.a.child(&node, i))
					}
					return
				}
			}
		}
		if params.len > 0 {
			tc.check_generic_type_constraints(id, node.pos, generic_base_id, generic_args, TypeParamScope{})
		}
	}
	if init_type_id != checker_tail_source_struct && !is_anonymous_struct_id(init_type_id)
		&& (!tc.type_name_known(init_type_id) || (init_syntax.c_qualified()
			&& !tc.type_symbol_known(init_type_id))) {
		if init_syntax.trimmed() == init_type_id && init_syntax.bare_generic()
			&& tc.unmentioned_generic_type_was_reported(init_type_text, id) {
			for i in 0 .. node.children_count {
				tc.check_node(tc.a.child(&node, i))
			}
			tc.remember_expr_type(id, unknown_type_quoting('unmentioned generic', init_type_text))
			return
		}
		if init_syntax.c_qualified() {
			tc.record_error_at(.unknown_type, 'unknown type `${node.value_text()}`', id, node.pos)
		} else if flat.source_type_shape(init_type_id).has(.dotted)
			&& tc.current_file_import_path_for_alias(flat.source_type_name(init_syntax.first_dot_prefix())) != none {
			mut message := tc.unknown_type_message(init_type_text, id)
			if !message.contains('\nDid you mean ') && tc.expected_expr_type !is Unknown
				&& tc.expected_expr_type !is Void {
				message += '.\nDid you mean `${tc.expected_expr_type.name()}`?'
			}
			tc.record_error_at(.unknown_type, message, id, tc.unknown_qualified_struct_init_pos(node))
		} else if tc.struct_init_has_positional_fields(node) {
			tc.record_error_at(.unknown_type, 'unknown type `${node.value_text()}`', id, node.pos)
		} else if candidates := tc.selective_import_candidates(init_type_id) {
			if candidates.len == 1 {
				// The selected declaration is reported in the diagnostic.
				tc.record_error_at(.unknown_type, tc.unknown_type_message(flat.source_type_name(candidates[0]),
					id), id, node.pos)
			} else {
				tc.record_error(.unknown_type, 'unknown struct `${node.value_text()}`', id)
			}
		} else {
			tc.record_error(.unknown_type, 'unknown struct `${node.value_text()}`', id)
		}
		tc.record_invalid_struct_init_print_error(id)
		for i in 0 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		recovery_type := if tc.expected_expr_type !is Unknown && tc.expected_expr_type !is Void {
			tc.expected_expr_type
		} else {
			builtin_void_type
		}
		tc.register_synth_type(id, recovery_type)
		return
	}
	mut init_type := *tc.parse_type(init_type_id)
	if is_contextual_anonymous_struct_literal(init_type_id) && tc.expected_expr_id >= 0
		&& tc.expr_is_value_tail_of(flat.NodeId(tc.expected_expr_id), id) {
		expected := unalias_type(tc.expected_expr_type)
		// Only a type the parser itself made up may be adopted here. A user-declared
		// `AnonStruct_Secret` is an ordinary private type, and adopting it would let a
		// bare literal stand in for a name the caller is not allowed to write.
		if expected is Struct && tc.is_synthesized_anon_struct(expected.source_type_id()) {
			init_type = expected
			tc.remember_expr_type(id, expected)
		}
	}
	if is_optional_init {
		tc.remember_expr_type(id, Type(OptionType{
			base_type: tc.intern_type_reference(init_type)
		}))
	}
	if unalias_type(init_type) is Enum {
		if tc.node_is_in_translated_file(id) && node.children_count == 0 {
			if _ := tc.private_declaration(init_type.name()) {
				tc.record_error_at(.unknown_type, 'type `${init_type.name()}` is private', id,
					node.pos)
				return
			}
			if !is_optional_init {
				tc.remember_expr_type(id, init_type)
			}
			return
		}
		tc.record_error_at(.assignment_mismatch, 'cannot initialize enums', id, node.pos)
		return
	}
	clean_init_type := unalias_type(init_type)
	if clean_init_type is Interface {
		tc.record_error_at(.assignment_mismatch, 'cannot instantiate interface `${clean_init_type.name().all_after_last('.')}`', id, node.pos)
		return
	}
	if init_struct := struct_type_from_type(init_type) {
		is_synthetic_embed_file := node.source_value_id() == checker_tail_source_embed_file_data
		decl_module := tc.struct_module_for_type(init_struct.source_type_id())
		noinit_same_main_module := decl_module in ['', 'main'] && tc.cur_module in ['', 'main']
		if decl_module != tc.cur_module && !noinit_same_main_module {
			if decl_id := tc.source_struct_decl_id_for_name(init_struct.source_type_id()) {
				if tc.declaration_has_attribute(decl_id, 'noinit') {
					display_name := tc.diagnostic_type_name(init_type)
					tc.record_error_at(.assignment_mismatch, 'struct `${display_name}` is declared with a `@[noinit]` attribute, so it cannot be initialized with `${display_name}{}`', id, node.pos)
				}
			}
		}
		// An attribute schema is inert metadata: unlike `@[noinit]`, which only
		// restricts cross-module initialization, a schema cannot be initialized
		// anywhere (vlang/rfcs#35, slice 1).
		if decl_id := tc.source_struct_decl_id_for_name(init_struct.source_type_id()) {
			if tc.declaration_has_attribute(decl_id, 'attribute') {
				display_name := tc.diagnostic_type_name(init_type)
				tc.record_error_at(.assignment_mismatch, 'cannot initialize struct `${display_name}` marked with `@[attribute]`', id, node.pos)
			}
		}
		// A `struct { ... }` literal is parsed into a name the parser synthesized for it
		// (or left as a bare `struct` when its field types cannot be inferred), which the
		// checker then resolves to whichever anonymous type the context expects.
		// The literal names nothing of its own, so there is no declaration whose privacy
		// it could violate - `cli.Command.defaults` is initialized exactly this way from
		// another module. Naming an anonymous declaration outright is a different thing
		// and stays subject to the check, as does a type a user happened to call
		// `AnonStruct_...`: neither is a name the parser made up for a literal.
		if init_type_id != checker_tail_source_struct && !tc.a.contextual_anon_struct_types[init_type_id] {
			if _ := tc.private_declaration(init_struct.name()) {
				inside_module := if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
				tc.record_error_at(.unknown_type, 'struct `${init_struct.name()}` was declared as private to module `${init_struct.name().all_before_last('.')}`, so it can not be used inside module `${inside_module}`', id, node.pos)
				tc.record_error_at(.unknown_type, 'type `${init_struct.name()}` is private', id, node.pos)
			}
		}
		if deprecation := tc.deprecated_symbols[init_struct.name()] {
			tc.record_deprecation(id, 'struct', deprecation, tc.struct_init_deprecation_pos(node))
		}
		if init_type !is Alias {
			if inferred_type := tc.infer_generic_struct_init_type(node) {
				tc.remember_expr_type(id, inferred_type)
				inferred_syntax := flat.source_type_syntax(inferred_type.source_type_id())
				tc.check_generic_type_constraints(id, node.pos, inferred_syntax.application_base(), inferred_syntax.application_args(), TypeParamScope{})
			} else if generic_name_id := tc.bare_generic_decl_type_name_id(init_type_id) {
				params := tc.struct_generic_params[generic_name_id] or {
					tc.struct_generic_params[tc.qualify_type_name(generic_name_id)] or { []string{} }
				}
				if params.len > 0
					&& !tc.struct_init_has_related_bare_generic_return_error(id, generic_name_id) {
					inferred := tc.infer_generic_struct_init_param_texts(node, generic_name_id, params)
					mut missing_param := params[0]
					for param in params {
						if param !in inferred {
							missing_param = param
							break
						}
					}
					generic_name := flat.source_type_name(generic_name_id)
					tc.record_error_at(.unknown_type, 'could not infer generic type `${missing_param}` in generic struct `${generic_name}[${params.join(', ')}]`', id, node.pos)
				}
			}
		}
		init_name_id := tc.struct_init_field_lookup_name(init_type_id, init_struct.source_type_id())
		init_name := flat.source_type_name(init_name_id)
		fields := tc.struct_fields_for_init(init_name_id)
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
			tc.record_error_at(.assignment_mismatch, 'too ${amount} fields in `${init_type_text.all_after_last('.')}` literal (expecting ${fields.len}, got ${positional_fields})', id, node.pos)
		}
		if unknown_message := tc.generic_struct_field_unknown_type_message(init_name_id) {
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
		tc.check_union_struct_init_fields(id, node, init_name_id, supplied_fields)
		for i in 0 .. node.children_count {
			raw_spread_id := tc.a.child(&node, i)
			spread := tc.a.child_node(&node, i)
			spread_id := if spread.kind == .prefix && spread.value == '...'
				&& spread.children_count > 0 {
				tc.a.child(spread, 0)
			} else if node.kind == .assoc && i == 0 {
				// `Type{...base, f: v}`: the parser stores `base` bare as the first child.
				raw_spread_id
			} else {
				continue
			}
			spread_type := tc.resolve_type(spread_id)
			spread_struct := struct_type_from_type(unwrap_pointer(spread_type)) or {
				tc.record_error_at(.assignment_mismatch, 'expected struct, found `${spread_type.name()}`', spread_id, tc.a.node(spread_id).pos)
				continue
			}
			spread_fields := tc.struct_fields_for_init(spread_struct.source_type_id())
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
				tc.record_error_at(.assignment_mismatch, 'struct `${spread_struct.name().all_after_last('.')}` is not compatible with struct `${init_struct.name().all_after_last('.')}`', spread_id, tc.a.node(spread_id).pos)
			}
			for source_field in spread_fields {
				supplied_fields[source_field.name] = true
			}
		}
		for field in fields {
			if supplied_fields[field.name] || field.has_default || field.typ is OptionType {
				continue
			}
			context_type := tc.expected_context_for_expr(id) or { builtin_void_type }
			context_expects_interface := unalias_and_unwrap_pointer_type(context_type) is Interface
			if unalias_type(field.typ) is Interface && !context_expects_interface
				&& !tc.struct_init_is_interface_return(id) {
				tc.record_notice_at(.assignment_mismatch, 'interface field `${init_struct.name()}.${field.name}` must be initialized', id, tc.struct_init_head_pos(node))
			}
		}
		for missing in tc.missing_required_struct_fields(init_name_id, supplied_fields, []u32{}) {
			tc.record_error_at(.assignment_mismatch, 'field `${missing}` must be initialized', id, tc.struct_init_head_pos(node))
		}
		mut seen_missing_references := map[string]bool{}
		// Match V1's generic recheck behavior: a concrete generic struct literal can
		// acquire pointer fields only after substituting its type arguments, so those
		// fields retain their zero/default initialization unless explicitly supplied.
		if !flat.source_type_shape(init_type_id).has(.bracketed) && !tc.node_is_in_translated_file(id) {
			for diagnostic_index, missing in tc.missing_reference_struct_fields(init_name_id, supplied_fields, []u32{}) {
				if seen_missing_references[missing.path] {
					continue
				}
				seen_missing_references[missing.path] = true
				message := if missing.has_part {
					'reference field `${missing.path}` must be initialized (part of struct `${missing.owner_text()}`)'
				} else {
					'reference field `${missing.path}` must be initialized'
				}
				tc.record_ordered_error_at(.assignment_mismatch, message, id,
					tc.struct_init_head_pos(node), diagnostic_index + 1)
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
				tc.record_deprecation(field_id, 'field', deprecation, tc.struct_init_field_deprecation_pos(field))
			}
			value_id := tc.a.child(&field, 0)
			mut expected := builtin_void_type
			if field.value.len > 0 {
				mut found := false
				if typ := tc.struct_field_type(init_name_id, field.value) {
					expected = typ
					found = true
				}
				if found && tc.unsafe_depth == 0 && !is_synthetic_embed_file
					&& tc.resolve_selective_import_type_symbol_id(init_type_id) == none {
					owner_base_id := if flat.source_type_shape(init_name_id).has(.leading_bracket) {
						init_name_id
					} else {
						flat.source_type_syntax(init_name_id).generic_base()
					}
					decl_mod := tc.struct_modules[owner_base_id] or { '' }
					same_main_module := decl_mod in ['', 'main'] && tc.cur_module in ['', 'main']
					if decl_mod.len > 0 && decl_mod != tc.cur_module && !same_main_module {
						// Anonymous fields and aliases retain their declared field visibility.
						// An inline parameter instead defines the fields a caller must supply.
						// Its name encodes the source path, so the module identifies it instead.
						if tc.is_synthesized_anon_struct(init_name_id)
							&& !tc.anonymous_struct_is_inline_parameter_type(init_name_id) {
							if !tc.anonymous_struct_field_is_public(init_name_id, field.value, decl_mod) {
								tc.record_error_at(.unknown_field, 'cannot access private field `${field.value}` of an anonymous struct from module `${tc.diagnostic_module_display_name(decl_mod)}`', field_id, tc.struct_init_field_deprecation_pos(field))
							}
						} else if !is_anonymous_struct_id(init_name_id) {
							is_public := tc.visible_mutation_struct_field_is_public(init_name_id, field.value, decl_mod) or { true }
							if !is_public {
								tc.record_error_at(.unknown_field, 'cannot access private field `${field.value}` on `${init_type_text}`', field_id, tc.struct_init_field_deprecation_pos(field))
							}
						}
					}
				}
				if !found && tc.should_diagnose(field_id) && fields.len > 0 {
					tc.record_error_at(.unknown_field, tc.struct_literal_unknown_field_message(init_name, field.value, fields), field_id, tc.struct_init_field_deprecation_pos(field))
				}
			} else if i < fields.len {
				expected = fields[i].typ
			}
			value_node := tc.a.node(value_id)
			if unalias_type(expected) is Array && value_node.kind == .map_init {
				tc.record_error_at(.assignment_mismatch, 'cannot use `{}` for array field `${field.value}`; use `[]` instead',
					value_id, token.new_span(value_node.pos.id, value_node.pos.offset,
						value_node.pos.offset + 1))
				continue
			}
			field_is_mut := struct_init_field_is_mut(fields, field, i)
			source_actual := if expected !is Void {
				tc.resolve_type(value_id)
			} else {
				builtin_void_type
			}
			if expected !is Pointer && tc.type_has_declaration_attribute(expected, 'nocopy') {
				tc.record_error_at(.assignment_mismatch, 'cannot copy @[nocopy] struct: use a reference instead', value_id, tc.a.node(value_id).pos)
			}
			// Like V1, a `@[heap]` struct may retain a pointer parameter in its fields.
			if expected is Pointer && !tc.type_has_declaration_attribute(Type(init_struct), 'heap') {
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
				if !tc.node_is_in_translated_file(value_id) {
					tc.warn_if_integer_literal_outside_known_type_range(value_id, expected, warning_pos)
				}
				tc.check_node_with_expected_context(value_id, expected)
			} else {
				tc.check_node(value_id)
			}
			$if ownership ? {
				if !tc.ownership_aggregate_consumption_deferred(id) {
					tc.ownership_consume_expr(value_id, 'struct field', value_id)
				}
			}
			if !tc.node_is_in_translated_file(value_id) && type_is_unsigned_integer(expected)
				&& tc.expr_is_negative_integer_literal(value_id) {
				tc.record_error_at(.assignment_mismatch, 'cannot assign negative value to unsigned integer type', value_id, value_node.pos)
			}
			if tc.unsafe_depth == 0 && !tc.translated_files[tc.cur_file]
				&& !flat.source_type_syntax(init_name_id).c_qualified()
				&& unalias_type(expected) is Pointer && value_node.kind == .int_literal
				&& value_node.value == '0' && expected.source_type_id() != flat.source_type_voidptr {
				pos := if field.value.len > 0 {
					tc.struct_init_field_value_pos(field, value_id)
				} else {
					value_node.pos
				}
				tc.record_error_at(.assignment_mismatch, 'assigning `0` to a reference field is only allowed in `unsafe` blocks', field_id, pos)
			} else if !flat.source_type_syntax(init_name_id).c_qualified() {
				tc.record_null_constant_reference_error(.assignment_mismatch, value_id, expected)
			}
			if tc.unsafe_depth == 0 && unalias_type(expected) is Array
				&& (field_is_mut || tc.slice_expr_base_is_mutable(value_id)) {
				tc.record_implicit_slice_clone_notice(value_id)
			}
			if tc.unsafe_depth == 0 && field_is_mut && unalias_type(expected) is Array
				&& value_node.kind == .ident && tc.const_key_for_name(value_node.value) != none {
				tc.record_error_at(.assignment_mismatch, 'cannot assign a const array to mut struct field, call `clone` method (or use `unsafe`)', value_id, value_node.pos)
			}
			if tc.unsafe_depth == 0 && field_is_mut && unalias_type(expected) is Map
				&& value_node.kind == .ident && tc.const_key_for_name(value_node.value) != none {
				tc.record_error_at(.assignment_mismatch, 'cannot assign a const map to mut struct field, call `clone` method (or use a reference)', value_id, value_node.pos)
			}
			if tc.unsafe_depth == 0 && field_is_mut && tc.mut_binding_value_id >= 0
				&& tc.expr_is_value_tail_of(flat.NodeId(tc.mut_binding_value_id), id) {
				if addressed_id := tc.addressed_ident(value_id) {
					addressed := tc.a.node(addressed_id)
					addressed_type := tc.non_file_scope_type(addressed.value) or { Type(Unknown{}) }
					// Taking the address of an immutable pointer variable for a
					// pointer-to-pointer field is valid. The reference preserves the
					// existing pointee; it does not make the pointer binding mutable.
					if !tc.ident_is_mutable_lvalue(addressed.value)
						&& unalias_type(addressed_type) !is Pointer {
						tc.record_error_at(.assignment_mismatch, '`${addressed.value}` is immutable, cannot have a mutable reference to an immutable object', addressed_id, addressed.pos)
					}
				}
			}
			if expected !is Void {
				mut actual := tc.resolve_expr(value_id, expected)
				if value_node.kind == .call {
					if info := tc.resolve_call_info(value_id, value_node) {
						// An implicit generic call resolves to its declared signature, so
						// specialize it from the arguments first. A return type that still
						// carries a placeholder (`map[T]int` -> `map[unknown]int`, an open
						// `Box[T]` or `chan T` spelling) must not replace the type inferred
						// from the expected field type.
						call_info := tc.specialized_plain_generic_call_info(value_node, info)
						if !type_contains_unknown(call_info.return_type)
							&& call_info.return_type !is Void
							&& !generic_semantic_type_has_placeholder(call_info.return_type)
							&& !tc.type_text_has_generic_placeholder(call_info.return_type.source_type_id()) {
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
				if tc.nil_interface_field_expr_compatible(value_id, expected) {
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
						tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected `string`, not `voidptr`', field_id, tc.struct_init_unsafe_nil_type_mismatch_pos(field, value_id))
					}
					tc.record_error_at(.assignment_mismatch, 'cannot assign `nil` to struct field `${field.value}` with type `${clean_expected.name()}`', value_id, tc.struct_init_unsafe_nil_value_pos(value_id))
					continue
				}
				if clean_actual is ResultType && clean_expected !is ResultType {
					tc.record_unhandled_result_call(value_id, clean_actual)
					continue
				}
				if clean_actual is Void {
					tc.record_error_at(.assignment_mismatch, '`${tc.source_text_for_node(value_id)}` (no value) used as value', field_id, tc.struct_init_field_deprecation_pos(field))
					continue
				}
				if tc.fn_storage_voidptr_mismatch(value_id, source_actual, expected) {
					tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${source_actual.name()}`', field_id, tc.struct_init_field_value_pos(field, value_id))
					continue
				}
				if clean_expected is FnType {
					expected_fn_text, expected_alias := tc.struct_field_diagnostic_fn_type(init_name_id, field.value, i) or { '', '' }
					actual_fn_text := tc.expr_diagnostic_fn_type(value_id) or { '' }
					if expected_fn_text.len > 0 && actual_fn_text.len > 0
						&& expected_fn_text != actual_fn_text && (fn_diagnostic_parameter_modes(expected_fn_text) != fn_diagnostic_parameter_modes(actual_fn_text)
						|| !tc.fn_types_match_ignoring_module_qualification(clean_expected, clean_actual))
						&& !tc.method_value_matches_voidptr_callback(value_id, actual, expected)
						&& !tc.fn_callback_adapter_compatible(source_actual, clean_expected) {
						details := tc.fn_assignment_mismatch_details(expected_fn_text, expected_alias, actual_fn_text, value_id)
						message := 'cannot assign to field `${field.value}`: expected `${expected_fn_text}`, not `${actual_fn_text}`'
						if details.len > 0 {
							tc.record_error_with_details_at(.assignment_mismatch, message, field_id, tc.struct_init_field_value_pos(field, value_id), details)
						} else {
							tc.record_error_at(.assignment_mismatch, message, field_id, tc.struct_init_field_value_pos(field, value_id))
						}
						continue
					}
				}
				// Before the checks below, some of which end with `continue`.
				if tc.warn_about_allocs {
					tc.warn_implicit_interface_conversion(value_id, actual, expected)
				}
				if value_node.kind == .fn_literal && value_node.typ == checker_source_option_marker
					&& clean_expected is FnType && clean_expected.return_type !is OptionType {
					expected_name := expected.name().replace_once('fn(', 'fn (')
					tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected `${expected_name}`, not `${expected_name} ?`', field_id, tc.struct_init_field_deprecation_pos(field))
				} else if clean_expected is OptionType && clean_actual is Pointer
					&& (unalias_type(clean_expected.base_type) !is Pointer
						|| (expected is OptionType && expected.base_type is Alias)) {
					tc.record_error_at(.assignment_mismatch, 'cannot assign a pointer to option struct field', field_id, tc.struct_init_pointer_field_pos(field))
				} else if clean_expected is OptionType
					&& unalias_type(clean_expected.base_type) is Pointer
					&& clean_actual is OptionType
					&& unalias_type(clean_actual.base_type) !is Pointer
					&& !tc.optional_pointer_expr_compatible(value_id, actual, expected) {
					tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected a pointer `${expected.name()}`, but got `${actual.name()}`', field_id, tc.struct_init_field_deprecation_pos(field))
				} else if tc.anonymous_struct_assignment_mismatch(value_id, actual, expected) {
					tc.record_error_at(.assignment_mismatch, 'cannot assign anonymous `struct` to a typed `struct`', value_id, tc.anonymous_struct_literal_brace_pos(value_id))
				} else if value_node.kind == .map_init && value_node.children_count == 0
					&& clean_expected is Struct && is_anonymous_struct_id(clean_expected.source_type_id()) {
					tc.record_error_at(.assignment_mismatch, '`{}` cannot be used to initialize anonymous structs. Use `struct{}` instead.', value_id, tc.empty_map_literal_diagnostic_pos(value_id))
				} else if clean_actual is OptionType && clean_expected !is OptionType {
					tc.record_error_at(.assignment_mismatch, 'cannot assign an Option value to a non-option struct field', field_id, tc.struct_init_field_deprecation_pos(field))
				} else if clean_actual is FnType && clean_expected is FnType
					&& actual.source_type_id() != expected.source_type_id()
					&& !tc.fn_types_match_ignoring_module_qualification(clean_expected, clean_actual)
					&& !tc.expr_compatible(value_id, actual, expected)
					&& !tc.method_value_matches_voidptr_callback(value_id, actual, expected)
					&& !tc.fn_callback_adapter_compatible(source_actual, clean_expected) {
					tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${actual.name()}`', field_id, tc.struct_init_field_deprecation_pos(field))
				} else if clean_expected is Interface
					&& !tc.type_implements_interface(actual, clean_expected)
					&& tc.record_interface_implementation_error(.assignment_mismatch, actual, clean_expected, field_id, tc.struct_init_field_value_pos(field, value_id)) {
					continue
				} else if pointer_to_value_fixed_array
					|| tc.distinct_alias_primitive_mismatch(source_actual, expected)
					|| (clean_expected is Map && clean_actual is Map
						&& clean_expected.source_type_id() != clean_actual.source_type_id())
					|| (!tc.expr_compatible(value_id, actual, expected)
						&& !tc.method_value_matches_voidptr_callback(value_id, actual, expected)
						&& !tc.pointer_value_compatible(actual, expected)) {
					diagnostic_actual := if tc.distinct_alias_primitive_mismatch(source_actual, expected) {
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
						if expected.source_type_id() == flat.source_type_voidptr {
							if clean_actual is Struct {
								tc.record_error_at(.assignment_mismatch, 'allocate `${clean_actual.name()}` on the heap for use in other functions', field_id, field_pos)
							} else if tc.should_diagnose(value_id) {
								source_value := tc.source_text_for_node(value_id)
								tc.record_notice_at(.assignment_mismatch, 'voidptr variables may only be assigned voidptr values (e.g. unsafe { voidptr(${source_value}) })', value_id, value_node.pos)
							}
							if !clean_actual.is_integer() {
								tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected a pointer `voidptr`, but got `${actual_name}`', field_id, field_pos)
							}
						} else if clean_actual.source_type_id() == flat.source_type_bool {
							tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${actual_name}`', field_id, field_pos)
							tc.record_error_at(.assignment_mismatch, 'reference field must be initialized with reference', field_id, field_pos)
						} else {
							tc.record_error_at(.assignment_mismatch, 'reference field must be initialized with reference', field_id, field_pos)
						}
					} else {
						tc.record_error_at(.assignment_mismatch, 'cannot assign to field `${field.value}`: expected `${expected.name()}`, not `${actual_name}`', field_id, field_pos)
					}
				}
			}
		}
		return
	}
	if clean_init_type is SumType && node.children_count == 0
		&& !tc.node_is_in_translated_file(id) {
		base := tc.sum_base_name(clean_init_type.source_type_id())
		variants := tc.sum_types.type_ids(base) or { []u32{} }
		if variants.len > 0 {
			first_variant_source_id := tc.concrete_sum_variant_name(clean_init_type.source_type_id(), variants[0])

			if first_struct := struct_type_from_type(unalias_type(tc.parse_type(first_variant_source_id))) {
				for diagnostic_index, missing in tc.missing_reference_struct_fields(first_struct.source_type_id(),
					map[string]bool{}, []u32{}) {
					message := if missing.has_part {
						'reference field `${missing.path}` must be initialized (part of struct `${missing.owner_text()}`)'
					} else {
						'reference field `${missing.path}` must be initialized'
					}
					tc.record_ordered_error_at(.assignment_mismatch, message, id,
						tc.struct_init_head_pos(node), diagnostic_index + 1)
				}
			}
		}
	}
	for i in 0 .. node.children_count {
		tc.check_node(tc.a.child(&node, i))
	}
	_ = id
}

fn (tc &TypeChecker) struct_init_has_related_bare_generic_return_error(id flat.NodeId, generic_name_id u32) bool {
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
	return_name_id := tc.bare_generic_decl_type_name_id(enclosing.typ) or { return false }
	return return_name_id == generic_name_id
}

fn (tc &TypeChecker) fn_types_match_ignoring_module_qualification(expected FnType, actual &Type) bool {
	if actual !is FnType {
		return false
	}
	actual_fn := actual as FnType
	if expected.params.len != actual_fn.params.len {
		return false
	}
	for i in 0 .. expected.params.len {
		if !fn_param_modes_compatible(actual_fn, expected, i)
			|| !tc.types_match_ignoring_module_qualification(fn_compatible_param_type(expected, i), fn_compatible_param_type(actual_fn, i)) {
			return false
		}
	}
	return tc.fn_signature_return_compatible(actual_fn.return_type, expected.return_type)
}

fn (tc &TypeChecker) fn_signature_return_compatible(actual &Type, expected &Type) bool {
	if actual is ResultType && expected is ResultType
		&& actual.base_type.source_type_id() != expected.base_type.source_type_id() {
		return false
	}
	if actual is OptionType && expected is OptionType
		&& actual.base_type.source_type_id() != expected.base_type.source_type_id() {
		return false
	}
	return tc.fn_return_compatible(actual, expected)
}

fn (tc &TypeChecker) fn_callback_adapter_compatible(actual &Type, expected &Type) bool {
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

fn (tc &TypeChecker) types_match_ignoring_module_qualification(expected &Type, actual &Type) bool {
	left := unalias_type(expected)
	right := unalias_type(actual)
	if left.source_type_id() == right.source_type_id() {
		return true
	}
	if left is Struct && right is Struct {
		return flat.source_type_syntax(left.source_type_id()).unqualified() == flat.source_type_syntax(right.source_type_id()).unqualified()
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

fn (mut tc TypeChecker) check_union_struct_init_fields(id flat.NodeId, node flat.Node, init_name_id u32, supplied_fields map[string]bool) {
	if init_name_id in tc.unions && tc.initialized_union_field_count(init_name_id, supplied_fields) > 1 {
		display := flat.source_type_name(init_name_id).all_after_last('.')
		tc.record_error_at(.assignment_mismatch, 'union `${display}` can have only one field initialised', id, tc.struct_init_head_pos(node))
		return
	}
	for field in tc.structs[init_name_id] or { []StructField{} } {
		embedded := embedded_field_type(field) or { continue }
		embedded_name_id := method_type_name(unwrap_pointer(unalias_type(embedded)))
		if embedded_name_id !in tc.unions || tc.initialized_union_field_count(embedded_name_id, supplied_fields) <= 1 {
			continue
		}
		display := flat.source_type_name(embedded_name_id).all_after_last('.')
		tc.record_error_at(.assignment_mismatch, 'embed union `${display}` can have only one field initialised', id, tc.struct_init_head_pos(node))
		return
	}
}

fn (tc &TypeChecker) initialized_union_field_count(union_name_id u32, supplied_fields map[string]bool) int {
	mut count := 0
	for name, supplied in supplied_fields {
		if supplied && tc.struct_field_type(union_name_id, name) != none {
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
		tc.record_error_at(.call_arg_mismatch, '`${callee.value}` can not print void expressions', parent_id, parent.pos)
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

fn (tc &TypeChecker) struct_init_has_positional_fields(node flat.Node) bool {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind == .field_init && field.value.len == 0 {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) distinct_alias_primitive_mismatch(actual &Type, expected &Type) bool {
	if actual is Alias && unalias_type(expected) is Primitive {
		return actual.base_type.source_type_id() != unalias_type(expected).source_type_id()
	}
	return false
}

fn (tc &TypeChecker) generic_struct_field_unknown_type_message(struct_name_id u32) ?string {
	decl := tc.source_struct_decl_for_name(struct_name_id) or { return none }
	syntax := flat.source_type_syntax(struct_name_id)
	lookup_id := if syntax.application_valid() {
		syntax.application_base()
	} else {
		struct_name_id
	}
	decl_file := tc.struct_files[lookup_id] or { tc.cur_file }
	decl_module := tc.struct_modules[lookup_id] or { tc.cur_module }
	generic_params := flat.TypeTextList.from_names(decl.generic_params()).ids
	for i in 0 .. decl.children_count {
		field_id := tc.a.child(&decl, i)
		field := tc.a.node(field_id)
		if field.kind != .field_decl || !flat.source_type_syntax(field.typ).application_valid() {
			continue
		}
		for diagnostic in tc.errors {
			if diagnostic.node == field_id && diagnostic.kind == .unknown_type
				&& diagnostic.msg.starts_with('unknown type `') {
				return diagnostic.msg
			}
		}
		if unknown_id := tc.first_unknown_type_name_in_scope(field.typ, decl_file, decl_module, generic_params) {
			return tc.unknown_type_message(flat.source_type_name(unknown_id), field_id)
		}
	}
	return none
}

fn (tc &TypeChecker) first_unknown_type_name_in_scope(raw_id u32, file string, mod_name string, generic_params []u32) ?u32 {
	clean_id := trimmed_source_type_id(raw_id)
	if clean_id == 0 {
		return none
	}
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.optional) || shape.has(.pointer) || shape.has(.array_type)
		|| shape.has(.shared_type) || shape.has(.mutable_type) {
		return tc.first_unknown_type_name_in_scope(syntax.raw_base(), file, mod_name, generic_params)
	}
	if syntax.application_valid() {
		base_id := syntax.application_base()
		if flat.source_type_syntax(base_id).named_type() && base_id !in generic_params
			&& !tc.type_name_known_in_scope_id(base_id, file, mod_name) {
			return base_id
		}
		for arg_id in syntax.application_args() {
			if unknown := tc.first_unknown_type_name_in_scope(arg_id, file, mod_name, generic_params) {
				return unknown
			}
		}
		return none
	}
	if clean_id !in generic_params && syntax.named_type()
		&& !tc.type_name_known_in_scope_id(clean_id, file, mod_name) {
		return clean_id
	}
	return none
}

fn (tc &TypeChecker) type_name_known_in_scope_id(name_id u32, file string, mod_name string) bool {
	if is_builtin_type_name_id(name_id) || name_id == flat.source_type_unknown
		|| flat.source_type_syntax(name_id).c_qualified() {
		return true
	}
	if flat.source_type_shape(name_id).has(.dotted) {
		return tc.type_symbol_known(tc.resolve_imported_type_text_in_file(name_id, file))
	}
	if mod_name !in ['', 'main', 'builtin'] {
		qualified_id := flat.source_type_composition_two(.qualified, flat.source_type_id(mod_name), name_id)
		if tc.type_symbol_known(qualified_id) { return true }
	}
	// A selected symbol is only returned when it is a known type symbol.
	if _ := tc.resolve_selective_import_type_symbol_in_file_id(name_id, file) {
		return true
	}
	return tc.type_symbol_known(name_id)
}

fn struct_field_has_attr(field flat.Node, name string) bool {
	meta := field.generic_params()
	if meta.len < 2 {
		return false
	}
	for attr in meta[1..] {
		if attribute_name_is(attr, name) {
			return true
		}
	}
	return false
}

// source_struct_decl_for_name returns the source struct declaration for a spelling ID.
fn (tc &TypeChecker) source_struct_decl_for_name(name_id u32) ?flat.Node {
	id := tc.source_struct_decl_id_for_name(name_id)?
	return *tc.a.node(id)
}

// anonymous_struct_is_inline_parameter_type distinguishes an inline parameter's
// structural contract from an anonymous type declared by a struct field or alias.
fn (tc &TypeChecker) anonymous_struct_is_inline_parameter_type(name_id u32) bool {
	short_id := flat.source_type_syntax(name_id).unqualified()
	decl_id := tc.source_struct_decl_id_for_name(short_id) or { return false }
	decl := tc.a.nodes[int(decl_id)]
	for index in tc.top_level_idx {
		fn_node := tc.a.nodes[index]
		if fn_node.kind != .fn_decl || fn_node.pos.id != decl.pos.id {
			continue
		}
		for i in 0 .. fn_node.children_count {
			child_id := tc.a.child(&fn_node, i)
			child := tc.a.nodes[int(child_id)]
			// An inline type is parsed after the parameter name, before its node.
			// Referring to an existing generated name does not declare a parameter type.
			if child.kind == .param
				&& child.pos.offset < decl.pos.offset && int(decl_id) < int(child_id)
				&& flat.source_type_syntax(flat.source_type_syntax(child.typ).source_wrapper_base()).unqualified() == short_id {
				return true
			}
		}
	}
	return false
}

// source_struct_decl_id_for_name looks up a trimmed generic base in the source declaration index.
fn (tc &TypeChecker) source_struct_decl_id_for_name(name_id u32) ?flat.NodeId {
	raw_target_id := trimmed_source_type_id(name_id)
	syntax := flat.source_type_syntax(raw_target_id)
	target_id := if syntax.application_valid() {
		syntax.application_base()
	} else {
		raw_target_id
	}
	for index in tc.type_declaration_ids[target_id] {
		node := tc.a.nodes[index]
		if node.kind == .struct_decl {
			return flat.NodeId(index)
		}
	}
	return none
}

fn (tc &TypeChecker) struct_field_diagnostic_fn_type(struct_name_id u32, field_name string, field_index int) ?(string, string) {
	key := '${struct_name_id}\x00${field_name}\x00${field_index}'
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
	expected, alias := tc.struct_field_diagnostic_fn_type_uncached(struct_name_id, field_name, field_index) or {
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

fn (tc &TypeChecker) struct_field_diagnostic_fn_type_uncached(struct_name_id u32, field_name string, field_index int) ?(string, string) {
	decl := tc.source_struct_decl_for_name(struct_name_id) or { return none }
	syntax := flat.source_type_syntax(struct_name_id)
	concrete_args := syntax.application_args()
	struct_params := if syntax.application_valid() {
		tc.struct_generic_params[syntax.application_base()] or {
			tc.struct_generic_params[flat.source_type_syntax(syntax.application_base()).unqualified()] or { []string{} }
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
		if (field_name != '' && field.value == field_name)
			|| (field_name == '' && ordinal == field_index) {
			source_type_id := if source_type := tc.source_fn_field_type_text(field) {
				flat.source_type_id(source_type)
			} else {
				field.typ
			}
			raw_id := if syntax.application_valid() && concrete_args.len == struct_params.len {
				subst_fn_diagnostic_type_id(source_type_id, concrete_args, struct_params)
			} else {
				trimmed_source_type_id(source_type_id)
			}
			raw := flat.source_type_name(raw_id)
			shape := flat.source_type_shape(raw_id)
			if shape.has(.function_compact) || shape.has(.function_spaced) {
				return tc.diagnostic_fn_type_text(raw), ''
			}
			if alias_raw := tc.source_fn_alias_type_text_id(raw_id) {
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

fn subst_fn_diagnostic_type_id(raw_id u32, args []u32, params []string) u32 {
	clean_id := trimmed_source_type_id(raw_id)
	shape := flat.source_type_shape(clean_id)
	if !shape.has(.function_compact) && !shape.has(.function_spaced) {
		return subst_generic_text(clean_id, args, params)
	}
	fn_params, _ := fn_diagnostic_type_parts_id(raw_id)
	mut rendered := []u32{cap: fn_params.len + 1}
	for param in fn_params {
		rendered << subst_generic_text(param.typ, args, params)
	}
	ret_id := flat.source_type_syntax(clean_id).function_result()
	if ret_id != 0 {
		rendered << subst_generic_text(ret_id, args, params)
		return flat.source_type_composition(.function_type_spaced, rendered)
	}
	return flat.source_type_composition(.void_function_type_spaced, rendered)
}

fn fn_diagnostic_parameter_modes(raw string) []string {
	return fn_diagnostic_parameter_modes_id(flat.source_type_id(raw))
}

fn fn_diagnostic_parameter_modes_id(raw_id u32) []string {
	syntax := flat.source_type_syntax(raw_id)
	if !syntax.function_closed() {
		return []string{}
	}
	mut modes := []string{}
	for param_id in syntax.checker_function_params() {
		shape := flat.source_type_shape(param_id)
		modes << if shape.has(.mutable_type) {
			'mut'
		} else if shape.has(.pointer) {
			'&'
		} else {
			''
		}
	}
	return modes
}

// CheckDeclIndexes holds indexes over the declarations of the AST for lookups
// that scanned every top-level declaration, or every node, on each query.
// check_semantics_opt builds them before it checks anything, and retires them
// when it returns; the check does not change the nodes in between. They are
// never written after they are built, so forks and views share them, and a view
// that is kept past the check finds them retired and scans as before.
@[heap]
struct CheckDeclIndexes {
mut:
	live           bool
	top_level_len  int
	top_level_data voidptr
	nodes_len      int
	// alias_decls maps the name, and the name qualified by the module declared
	// before it, of each childless top-level `type_decl` to the first such
	// declaration in top_level_idx order (see first_childless_type_decl).
	alias_decls map[string]flat.NodeId
	// enum_decls maps the qualified spelling ID of each top-level `enum_decl` to
	// the first such declaration in top_level_idx order, and its module.
	enum_decls map[u32]CheckEnumDeclRef
	// enum_decls_complete is false when the qualified spelling of an enum was not
	// interned yet: the scan that enum_decls replaces interns each one it passes.
	enum_decls_complete bool
	// decl_assign_names holds every identifier that a `decl_assign` declares.
	decl_assign_names map[string]bool
	// struct_decls maps the name, and the name qualified by the module declared
	// before it, of each top-level `struct_decl` to those declarations, in
	// top_level_idx order (see source_struct_field_decls).
	struct_decls map[string][]CheckStructDeclRef
	// enum_field_sets holds the members of each enum with many of them, for the
	// field list in enum_fields at the start of the check (see enum_field_name).
	enum_field_sets map[u32]CheckEnumFieldSet
}

struct CheckEnumFieldSet {
	data  voidptr
	len   int
	names map[string]bool
}

struct CheckEnumDeclRef {
	idx         int
	module_name string
}

struct CheckStructDeclRef {
	order int // the position in top_level_idx
	idx   int
	file  string
}

// new_check_decl_indexes builds the declaration indexes of the current AST.
fn (tc &TypeChecker) new_check_decl_indexes() &CheckDeclIndexes {
	mut indexes := &CheckDeclIndexes{
		live:                true
		top_level_len:       tc.top_level_idx.len
		top_level_data:      tc.top_level_idx.data
		nodes_len:           tc.a.nodes.len
		enum_decls_complete: true
	}
	// The alias and struct scans keep the module of the last `module` declaration
	// across files, the enum scan clears it at each file (see the scans replaced).
	mut alias_module := ''
	mut enum_module := ''
	mut struct_file := ''
	for order, index in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(index))
		match node.kind {
			.file {
				enum_module = ''
				struct_file = node.value
			}
			.struct_decl {
				if index >= 0 && index < tc.a.nodes.len {
					ref := CheckStructDeclRef{
						order: order
						idx:   int(index)
						file:  struct_file
					}
					mut refs := indexes.struct_decls[node.value] or { []CheckStructDeclRef{} }
					refs << ref
					indexes.struct_decls[node.value] = refs
					qualified := if alias_module.len > 0 && !node.value.contains('.') {
						'${alias_module}.${node.value}'
					} else {
						node.value
					}
					if qualified != node.value {
						mut qualified_refs := indexes.struct_decls[qualified] or {
							[]CheckStructDeclRef{}
						}
						qualified_refs << ref
						indexes.struct_decls[qualified] = qualified_refs
					}
				}
			}
			.module_decl {
				alias_module = node.value
				enum_module = node.value
			}
			.type_decl {
				if node.children_count == 0 {
					if node.value !in indexes.alias_decls {
						indexes.alias_decls[node.value] = flat.NodeId(index)
					}
					qualified := qualify_decl_name_in_module(node.value, alias_module)
					if qualified !in indexes.alias_decls {
						indexes.alias_decls[qualified] = flat.NodeId(index)
					}
				}
			}
			.enum_decl {
				if indexes.enum_decls_complete {
					qualified := if enum_module != '' && enum_module != 'main'
						&& enum_module != 'builtin' {
						'${enum_module}.${node.value}'
					} else {
						node.value
					}
					if enum_id := flat.source_type_existing_id(qualified) {
						if enum_id !in indexes.enum_decls {
							indexes.enum_decls[enum_id] = CheckEnumDeclRef{
								idx:         int(index)
								module_name: enum_module
							}
						}
					} else {
						indexes.enum_decls_complete = false
					}
				}
			}
			else {}
		}
	}
	for enum_id, fields in tc.enum_fields {
		if fields.len < min_enum_field_set_len {
			continue
		}
		mut names := map[string]bool{}
		for field in fields {
			names[field] = true
		}
		indexes.enum_field_sets[enum_id] = CheckEnumFieldSet{
			data:  fields.data
			len:   fields.len
			names: names
		}
	}
	for i in 0 .. tc.a.nodes.len {
		node := tc.a.node(flat.NodeId(i))
		if node.kind != .decl_assign {
			continue
		}
		for child_idx := 0; child_idx < node.children_count; child_idx += 2 {
			lhs := tc.a.node(tc.a.child(node, child_idx))
			if lhs.kind == .ident && lhs.value !in indexes.decl_assign_names {
				indexes.decl_assign_names[lhs.value] = true
			}
		}
	}
	return indexes
}

// min_enum_field_set_len is the fewest members of an enum whose members
// new_check_decl_indexes puts in a set: a short list is searched as fast.
const min_enum_field_set_len = 16

// enum_fields_contain reports whether `field` is in `fields`, the members of the enum
// `enum_name_id`.
@[inline]
fn (tc &TypeChecker) enum_fields_contain(enum_name_id u32, fields []string, field string) bool {
	if fields.len >= min_enum_field_set_len {
		indexes := tc.check_decl_indexes
		if !isnil(indexes) && indexes.live {
			if set := indexes.enum_field_sets[enum_name_id] {
				if set.data == fields.data && set.len == fields.len {
					return field in set.names
				}
			}
		}
	}
	return field in fields
}

// retire_check_decl_indexes ends the use of the declaration indexes, also by the
// forks and views that still point to them, and drops the source text memo.
fn (mut tc TypeChecker) retire_check_decl_indexes() {
	mut indexes := tc.check_decl_indexes
	if !isnil(indexes) {
		indexes.live = false
	}
	tc.check_decl_indexes = unsafe { nil }
	tc.source_type_keyword_memo = unsafe { nil }
}

// top_level_check_decl_indexes returns the live declaration indexes when they
// describe the current top-level declaration index, or nil.
@[inline]
fn (tc &TypeChecker) top_level_check_decl_indexes() &CheckDeclIndexes {
	indexes := tc.check_decl_indexes
	if isnil(indexes) || !indexes.live || indexes.top_level_len != tc.top_level_idx.len
		|| indexes.top_level_data != tc.top_level_idx.data {
		return unsafe { nil }
	}
	return indexes
}

// first_childless_type_decl returns the first childless top-level `type_decl`, in
// top_level_idx order, that `name` names unqualified or qualified by the module
// declared before it, or empty_node.
fn (tc &TypeChecker) first_childless_type_decl(name string) flat.NodeId {
	indexes := tc.top_level_check_decl_indexes()
	if !isnil(indexes) {
		return indexes.alias_decls[name] or { flat.empty_node }
	}
	mut module_name := ''
	for index in tc.top_level_idx {
		node := tc.a.node(flat.NodeId(index))
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .type_decl || node.children_count > 0 {
			continue
		}
		if node.value == name || qualify_decl_name_in_module(node.value, module_name) == name {
			return flat.NodeId(index)
		}
	}
	return flat.empty_node
}

fn (tc &TypeChecker) source_fn_alias_type_text(name string) ?string {
	return tc.source_fn_alias_type_text_id(flat.source_type_id(name))
}

// source_fn_alias_type_text_id is source_fn_alias_type_text for a spelling ID.
fn (tc &TypeChecker) source_fn_alias_type_text_id(name_id u32) ?string {
	mut target_id := trimmed_source_type_id(name_id)
	for _ in 0 .. 8 {
		syntax := flat.source_type_syntax(target_id)
		lookup_target_id := if syntax.application_valid() {
			syntax.application_base()
		} else {
			target_id
		}
		// Declaration names are compared with the AST text of each type declaration.
		lookup_target := flat.source_type_name(lookup_target_id)
		mut found_id := u32(0)
		decl_id := tc.first_childless_type_decl(lookup_target)
		if decl_id != flat.empty_node {
			node := tc.a.node(decl_id)
			mut source_type := tc.source_type_alias_rhs(tc.cur_file, node.value) or { '' }
			if source_type.len == 0 {
				if file := tc.a.source_files[node.pos.id] {
					source_type = tc.source_type_alias_rhs(file.name, node.value) or { '' }
				}
			}
			source_id := flat.source_type_id(source_type)
			source_shape := flat.source_type_shape(source_id)
			found_id = if source_shape.has(.function_compact) || source_shape.has(.function_spaced) {
				source_id
			} else {
				node.typ
			}
		}
		if found_id == 0 {
			return none
		}
		clean_id := trimmed_source_type_id(found_id)
		shape := flat.source_type_shape(clean_id)
		if shape.has(.function_compact) || shape.has(.function_spaced) {
			if syntax.application_valid() {
				// The matched declaration name is exactly the lookup target.
				params := tc.type_alias_generic_params[lookup_target_id] or {
					tc.type_alias_generic_params[flat.source_type_syntax(lookup_target_id).unqualified()] or {
						[]string{}
					}
				}
				if params.len == syntax.application_args().len {
					return subst_generic_diagnostic_fn_text(clean_id, syntax.application_args(), params)
				}
			}
			return flat.source_type_name(clean_id)
		}
		target_id = clean_id
	}
	return none
}

fn subst_generic_diagnostic_fn_text(raw_id u32, args []u32, params []string) string {
	clean_id := trimmed_source_type_id(raw_id)
	syntax := flat.source_type_syntax(clean_id)
	if !syntax.function_closed() {
		return flat.source_type_name(subst_generic_text(clean_id, args, params))
	}
	mut rendered_params := []string{}
	for part_id in syntax.checker_function_params() {
		rendered_params << flat.source_type_name(subst_generic_text(part_id, args, params))
	}
	return 'fn (${rendered_params.join(', ')})${if syntax.function_result() != 0 {
		' ${flat.source_type_name(subst_generic_text(syntax.function_result(), args, params))}'
	} else {
		''
	}}'
}

fn (tc &TypeChecker) source_type_alias_rhs(file_name string, alias_name string) ?string {
	if file_name == '' || alias_name == '' {
		return none
	}
	mut source := ''
	mut decl_start := 0
	if known := tc.source_texts_by_file[file_name] {
		source = known
		decl_start = tc.source_type_keyword_index(file_name, source, alias_name) or { return none }
	} else {
		source = os.read_file(file_name) or { return none }
		decl_start = source.index('type ${alias_name}') or { return none }
	}
	line_end := source.index_after('\n', decl_start) or { source.len }
	assign := source[decl_start..line_end].index('=') or { return none }
	return source[decl_start + assign + 1..line_end].trim_space()
}

// SourceTypeKeywordMemo records where `type ` occurs in the source texts that
// source_type_alias_rhs searched. A checker that checks owns a private one (see
// check_semantics_opt and fork_for_parallel_check); views have none.
@[heap]
struct SourceTypeKeywordMemo {
mut:
	files map[string]SourceTypeKeywordOffsets
}

struct SourceTypeKeywordOffsets {
	text    voidptr
	len     int
	offsets []int
}

// source_type_keyword_index returns the first index of `type ${alias_name}` in
// `source`, the text of `file_name`.
fn (tc &TypeChecker) source_type_keyword_index(file_name string, source string, alias_name string) ?int {
	mut memo := tc.source_type_keyword_memo
	if isnil(memo) {
		return source.index('type ${alias_name}')
	}
	mut entry := memo.files[file_name] or { SourceTypeKeywordOffsets{} }
	if entry.text != voidptr(source.str) || entry.len != source.len {
		mut offsets := []int{}
		mut start := 0
		for {
			offset := source.index_after_('type ', start)
			if offset < 0 {
				break
			}
			offsets << offset
			start = offset + 1
		}
		entry = SourceTypeKeywordOffsets{
			text:    source.str
			len:     source.len
			offsets: offsets
		}
		memo.files[file_name] = entry
	}
	for offset in entry.offsets {
		name_start := offset + 5
		if alias_name.len <= source.len - name_start
			&& unsafe { vmemcmp(source.str + name_start, alias_name.str, alias_name.len) } == 0 {
			return offset
		}
	}
	return none
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
		// The short-name index also holds methods (`EventLoop.did_change`), but a
		// bare name never refers to one, so look past a same-named method.
		if index := tc.fn_decl_short_name_ids[node.value] {
			decl := tc.a.node(flat.NodeId(index))
			if !tc.fn_has_receiver_param(decl) {
				return tc.fn_node_source_type_text(decl)
			}
		}
		// Declarations appended after collect (monomorphization) miss the
		// index; scan them with an allocation-free suffix compare.
		for index in tc.top_level_idx {
			decl := tc.a.node(flat.NodeId(index))
			if decl.kind == .fn_decl && embedded_name_matches(node.value, decl.value)
				&& !tc.fn_has_receiver_param(decl) {
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
		param := tc.a.node(tc.a.child(&node, i))
		if param.kind != .param {
			continue
		}
		if !skipped_receiver {
			skipped_receiver = true
			continue
		}
		mut typ := param.typ_text()
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
	return 'fn (${params.join(', ')})${if node.typ != 0 && node.typ != flat.source_type_void {
		' ${node.typ_text()}'
	} else {
		''
	}}'
}

fn (tc &TypeChecker) fn_node_source_type_text(node flat.Node) string {
	mut params := []string{}
	for i in 0 .. node.children_count {
		param := tc.a.node(tc.a.child(&node, i))
		if param.kind != .param {
			continue
		}
		mut typ := param.typ_text()
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
	return 'fn (${params.join(', ')})${if node.typ != 0 && node.typ != flat.source_type_void {
		' ${node.typ_text()}'
	} else {
		''
	}}'
}

fn (tc &TypeChecker) diagnostic_fn_type_text(raw string) string {
	params, suffix := fn_diagnostic_type_parts(raw)
	mut display := []string{cap: params.len}
	for param in params {
		display << param.typ_text()
	}
	return 'fn (${display.join(', ')})${suffix}'
}

fn fn_diagnostic_type_parts(raw string) ([]FnDiagnosticParam, string) {
	return fn_diagnostic_type_parts_id(flat.source_type_id(raw))
}

fn fn_diagnostic_type_parts_id(raw_id u32) ([]FnDiagnosticParam, string) {
	syntax := flat.source_type_syntax(raw_id)
	if !syntax.function_closed() {
		return []FnDiagnosticParam{}, ''
	}
	mut params := []FnDiagnosticParam{}
	for param_id in syntax.function_params() {
		param_syntax := flat.source_type_syntax(param_id)
		is_mut := flat.source_type_shape(param_id).has(.mutable_type)
		unnamed_id := if is_mut {
			trimmed_source_type_id(param_syntax.raw_base())
		} else {
			param_id
		}
		text_id := param_syntax.legacy_parameter_type()
		name := if text_id != unnamed_id {
			flat.source_type_name(flat.source_type_syntax(unnamed_id).top_space_head())
		} else {
			''
		}
		shape := flat.source_type_shape(text_id)
		mut display_type_id := text_id
		if is_mut {
			base_id := if shape.has(.pointer) {
				flat.source_type_syntax(text_id).pointer_raw_base()
			} else {
				text_id
			}
			display_type_id = flat.source_type_composition_one(.mutable_parameter, base_id)
		}
		params << FnDiagnosticParam{
			name:       name
			typ:        display_type_id
			is_mut:     is_mut
			is_pointer: !shape.has(.variadic) && (is_mut || shape.has(.pointer)
				|| text_id in [flat.source_type_voidptr, checker_source_void_reference])
		}
	}
	suffix := if syntax.function_result() != 0 {
		' ${flat.source_type_name(syntax.function_result())}'
	} else {
		''
	}
	return params, suffix
}

fn (tc &TypeChecker) fn_assignment_mismatch_details(expected_text string, expected_alias string, actual_text string, actual_id flat.NodeId) []string {
	expected_params, _ := fn_diagnostic_type_parts(expected_text)
	actual_params, _ := fn_diagnostic_type_parts(actual_text)
	mut expected_named := []FnDiagnosticParam{}
	if expected_alias != '' {
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
		if expected_alias != '' && i < expected_named.len && i < actual_named.len
			&& expected_named[i].name.len > 0 && actual_named[i].name.len > 0 {
			mut alias_name := tc.qualify_name(expected_alias)
			if !alias_name.contains('.') {
				alias_name = '${if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }}.${alias_name}'
			}
			return [
				"`${alias_name}`'s expected argument `${expected_named[i].name}` ${expected_pointer}, but the passed argument `${actual_named[i].name}` ${actual_pointer}",
			]
		}
		return [
			'expected argument ${i + 1} ${expected_pointer}, but the passed argument ${i + 1} ${actual_pointer}',
		]
	}
	return []string{}
}

// supplied_fields_for_embeds returns the names in `supplied` that can still refer to a
// promoted field of an embedded struct of `decl`. An explicitly initialized embedded
// field names one specific embedding path, so it is consumed by the current literal
// level: keeping it would let `Outer{Base: ...}` also satisfy an unrelated
// `Outer.Middle.Base` that has a different embedding path but the same field name.
// Plain field names keep being passed down, because V1 also treats a supplied name as
// initialization of a same-named promoted field (see struct_embed_required_field_err).
fn (tc &TypeChecker) supplied_fields_for_embeds(decl flat.Node, supplied map[string]bool) map[string]bool {
	if supplied.len == 0 {
		return map[string]bool{}
	}
	mut embed_fields := map[string]bool{}
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		if source_field_decl_is_embed(field, field_type_id) {
			embed_fields[field.value] = true
			embed_fields[struct_decl_field_init_name(field, true)] = true
		}
	}
	mut promoted := map[string]bool{}
	for name, is_supplied in supplied {
		if is_supplied && name !in embed_fields {
			promoted[name] = true
		}
	}
	return promoted
}

// struct_decl_field_init_name returns the name a struct literal initializes the declared
// field with: an embedded `mod.Type[T]` is initialized as `Type: ...`.
fn struct_decl_field_init_name(field &flat.Node, is_embed bool) string {
	if !is_embed {
		return field.value
	}
	field_id := field.source_value_id()
	base_id := if flat.source_type_shape(field_id).has(.leading_bracket) {
		field_id
	} else {
		flat.source_type_syntax(field_id).generic_base()
	}
	return flat.source_type_name(flat.source_type_syntax(base_id).unqualified())
}

fn (tc &TypeChecker) missing_reference_struct_fields(struct_name_id u32, supplied map[string]bool, path_ids []u32) []MissingReferenceField {
	clean_name_id := trimmed_source_type_id(struct_name_id)
	if clean_name_id == 0 || clean_name_id in path_ids || path_ids.len >= 16 {
		return []MissingReferenceField{}
	}
	decl := tc.source_struct_decl_for_name(clean_name_id) or { return []MissingReferenceField{} }
	if decl.value.starts_with('C.') || comma_attr_text_has(decl.typ_text(), 'c_anon') {
		return []MissingReferenceField{}
	}
	display_name := decl.value.all_after_last('.')
	mut embed_supplied := map[string]bool{}
	mut embed_supplied_ready := false
	mut next_path := path_ids.clone()
	next_path << clean_name_id
	mut missing := []MissingReferenceField{}
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		field_type := unalias_type(tc.struct_field_type(clean_name_id, field.value) or {
			*tc.parse_type(field_type_id)
		})
		is_embed := source_field_decl_is_embed(field, field_type_id)
		init_name := struct_decl_field_init_name(field, is_embed)
		if field_type is Pointer {
			if tc.struct_field_has_shared_elements(clean_name_id, field.value) {
				continue
			}
			if field_type_id in [flat.source_type_charptr, flat.source_type_byteptr,
				flat.source_type_voidptr] {
				continue
			}
			if unalias_type(field_type.base_type) is Void {
				continue
			}
			if is_embed {
				base_type := unalias_type(field_type.base_type)
				if base_type is Struct {
					mut promoted_field_supplied := false
					for promoted_field in tc.struct_fields_for_init(base_type.source_type_id()) {
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
			if field.children_count == 0 && init_name !in supplied {
				missing << MissingReferenceField{
					path:  '${display_name}.${field.value}'
					owner: flat.source_type_id(display_name)
				}
			}
			continue
		}
		if field_type !is Struct || field.children_count > 0 {
			continue
		}
		if init_name in supplied {
			// An explicitly initialized field is checked on its own, and an embedded
			// struct literal reports its own missing reference fields. Re-checking the
			// embedded struct here would duplicate those diagnostics at the outer literal.
			continue
		}
		if is_embed && !embed_supplied_ready {
			embed_supplied = tc.supplied_fields_for_embeds(decl, supplied)
			embed_supplied_ready = true
		}
		child_supplied := if is_embed {
			embed_supplied
		} else {
			map[string]bool{}
		}
		child_missing := tc.missing_reference_struct_fields(field_type.source_type_id(), child_supplied, next_path)
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

fn (tc &TypeChecker) missing_required_struct_fields(struct_name_id u32, supplied map[string]bool, path_ids []u32) []string {
	if !tc.has_required_fields && tc.required_fields_ast_nodes == tc.a.nodes.len {
		return []string{}
	}
	clean_name_id := trimmed_source_type_id(struct_name_id)
	if clean_name_id == 0 || clean_name_id in path_ids || path_ids.len >= 16 {
		return []string{}
	}
	decl := tc.source_struct_decl_for_name(clean_name_id) or { return []string{} }
	display_name := decl.value.all_after_last('.')
	mut embed_supplied := map[string]bool{}
	mut embed_supplied_ready := false
	mut next_path := path_ids.clone()
	next_path << clean_name_id
	mut missing := []string{}
	for i in 0 .. decl.children_count {
		field := tc.a.child_node(&decl, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		is_embed := source_field_decl_is_embed(field, field_type_id)
		init_name := struct_decl_field_init_name(field, is_embed)
		if struct_field_has_attr(*field, 'required') && init_name !in supplied {
			missing << '${display_name}.${field.value}'
		}
		field_type := unalias_type(tc.struct_field_type(clean_name_id, field.value) or {
			*tc.parse_type(field_type_id)
		})
		if field_type !is Struct || field.children_count > 0 {
			continue
		}
		if init_name in supplied {
			// An explicitly initialized field is checked on its own, and an embedded
			// struct literal reports its own missing required fields. Re-checking the
			// embedded struct here would duplicate those diagnostics at the outer literal.
			continue
		}
		if is_embed && !embed_supplied_ready {
			embed_supplied = tc.supplied_fields_for_embeds(decl, supplied)
			embed_supplied_ready = true
		}
		child_supplied := if is_embed {
			embed_supplied
		} else {
			map[string]bool{}
		}
		child_missing := tc.missing_required_struct_fields(field_type.source_type_id(), child_supplied, next_path)
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
	// Only the type name holds the module qualifier; the field values and generic
	// arguments after it can contain dots of their own (`baz.Foo{bar.MyData{}}`).
	type_name := source[start..end].all_before('{').all_before('[')
	dot := type_name.last_index_u8(`.`)
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
	return token.new_span(value.pos.id, value.pos.offset, int_max(value.pos.offset, value.pos.end - 1))
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
	base_name_id := trimmed_source_type_id(node.source_value_id())
	if base_name_id == 0 || flat.source_type_shape(base_name_id).has(.bracketed) {
		return none
	}
	qualified_name_id := tc.qualify_type_name(base_name_id)
	params := tc.struct_generic_params[qualified_name_id] or {
		// A concrete declaration in the active module shadows an unrelated
		// bare generic declaration collected from another module.
		if qualified_name_id != base_name_id && qualified_name_id in tc.structs {
			return none
		}
		tc.struct_generic_params[base_name_id] or { return none }
	}
	if params.len == 0 {
		return none
	}
	inferred := tc.infer_generic_struct_init_param_texts(node, qualified_name_id, params)
	if inferred.len == 0 {
		return none
	}
	mut parts := []u32{cap: params.len + 1}
	parts << qualified_name_id
	for param in params {
		parts << inferred[param] or { return none }
	}
	return *tc.parse_type(flat.source_type_composition(.generic, parts))
}

fn (mut tc TypeChecker) infer_generic_struct_init_param_texts(node flat.Node, base_name_id u32, params []string) map[string]u32 {
	init_name_id := tc.qualify_type_name(base_name_id)
	fields := tc.structs[init_name_id] or { tc.structs[base_name_id] or { return map[string]u32{} } }
	source_fields := tc.source_struct_field_decls(init_name_id)
	mut inferred := map[string]u32{}
	for param in params {
		if param in tc.fn_context.generic_params {
			inferred[param] = flat.source_type_id(param)
		}
	}
	for i in 0 .. node.children_count {
		field_node := tc.a.child_node(&node, i)
		if field_node.kind != .field_init || field_node.children_count == 0 {
			continue
		}
		value_id := tc.a.child(field_node, 0)
		mut field_type := builtin_void_type
		mut field_type_text := u32(0)
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
		if field_type_text == 0 {
			field_type_text = field_type.source_type_id()
		}
		actual := tc.resolve_type(value_id)
		tc.infer_generic_type_text_from_type(field_type_text, actual, params, mut inferred)
	}
	return inferred
}

// push_type_name_candidate_id is push_type_name_candidate for spelling IDs.
fn push_type_name_candidate_id(mut candidates []u32, name_id u32) {
	clean_id := trimmed_source_type_id(name_id)
	if clean_id != 0 && clean_id !in candidates {
		candidates << clean_id
	}
}

fn (tc &TypeChecker) source_struct_field_decls(struct_name_id u32) []SourceStructFieldDecl {
	syntax := flat.source_type_syntax(struct_name_id)
	lookup_id := if syntax.application_valid() { syntax.application_base() } else { struct_name_id }
	mut candidate_ids := []u32{cap: 3}
	push_type_name_candidate_id(mut candidate_ids, lookup_id)
	push_type_name_candidate_id(mut candidate_ids, flat.source_type_syntax(lookup_id).unqualified())
	push_type_name_candidate_id(mut candidate_ids, tc.qualify_type_name(lookup_id))
	mut wanted_file := ''
	for candidate_id in candidate_ids {
		if file := tc.struct_files[candidate_id] {
			wanted_file = file
			break
		}
	}
	// Struct declarations are matched by the AST text of their names.
	candidates := candidate_ids.map(flat.source_type_name(it))
	indexes := tc.top_level_check_decl_indexes()
	if !isnil(indexes) {
		// The first declaration, in top_level_idx order, that a candidate names
		// and that is in the wanted file.
		mut found := CheckStructDeclRef{
			order: -1
		}
		for candidate in candidates {
			for ref in indexes.struct_decls[candidate] or { continue } {
				if found.order >= 0 && ref.order >= found.order {
					break
				}
				if wanted_file.len > 0 && ref.file.len > 0 && ref.file != wanted_file {
					continue
				}
				found = ref
				break
			}
		}
		if found.order < 0 {
			return []SourceStructFieldDecl{}
		}
		return tc.source_struct_decl_fields(tc.a.nodes[found.idx])
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
				return tc.source_struct_decl_fields(decl)
			}
			else {}
		}
	}
	return []SourceStructFieldDecl{}
}

// source_struct_decl_fields lists the field declarations of the struct `decl`.
fn (tc &TypeChecker) source_struct_decl_fields(decl flat.Node) []SourceStructFieldDecl {
	mut fields := []SourceStructFieldDecl{}
	for i in 0 .. decl.children_count {
		field := tc.a.node(tc.a.child(&decl, i))
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 {
			field.typ
		} else {
			flat.source_type_id(field.value)
		}
		fields << SourceStructFieldDecl{
			name: field.value
			typ:  field_type_id
		}
	}
	return fields
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
	receiver_syntax := flat.source_type_syntax(comptime_static_unwrap_type_id(receiver.typ))
	if !receiver_syntax.application_valid() || receiver_syntax.application_args().len == 0 {
		return
	}
	receiver_params := flat.TypeTextList{ ids: receiver_syntax.application_args() }.names()
	mut param_types := map[string]u32{}
	for i in 0 .. node.children_count {
		param := tc.a.child_node(&node, i)
		if param.kind == .param && param.value.len > 0 {
			param_types[param.value] = if param.is_mut && param.op != .amp
				&& flat.source_type_shape(param.typ).has(.pointer) {
				flat.source_type_syntax(param.typ).raw_base()
			} else {
				param.typ
			}
		}
	}
	mut comparisons := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			tc.collect_deferred_generic_pointer_comparisons(child_id, param_types, receiver_params, mut comparisons)
		}
	}
	if comparisons.len == 0 {
		return
	}
	concrete_args_source_ids := (tc.concrete_generic_receiver_args(receiver_syntax.application_base(), receiver_params.len) or {
		return
	}).clone()

	for comparison_id in comparisons {
		comparison := tc.a.node(comparison_id)
		lhs_id := tc.a.child(comparison, 0)
		rhs_id := tc.a.child(comparison, 1)
		lhs_raw_id := tc.deferred_generic_expr_type_text(lhs_id, param_types)
		rhs_raw_id := tc.deferred_generic_expr_type_text(rhs_id, param_types)
		lhs := subst_generic_text(lhs_raw_id, concrete_args_source_ids, receiver_params)
		rhs := subst_generic_text(rhs_raw_id, concrete_args_source_ids, receiver_params)
		if !generic_pointer_value_mismatch(lhs, rhs) {
			continue
		}
		tc.record_error_at(.condition_mismatch, 'mismatched types `${flat.source_type_name(lhs)}` and `${flat.source_type_name(rhs)}`', comparison_id, comparison.pos)
	}
}

fn (tc &TypeChecker) collect_deferred_generic_pointer_comparisons(id flat.NodeId, param_types map[string]u32, generic_params []string, mut comparisons []flat.NodeId) {
	if !tc.valid_node_id(id) {
		return
	}
	node := tc.a.node(id)
	if node.kind == .infix && node.op in [.lt, .gt, .le, .ge] && node.children_count >= 2 {
		lhs := tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types)
		rhs := tc.deferred_generic_expr_type_text(tc.a.child(node, 1), param_types)
		if type_id_mentions_any_generic_param(lhs, generic_params)
			&& type_id_mentions_any_generic_param(rhs, generic_params)
			&& generic_pointer_value_mismatch(lhs, rhs) {
			comparisons << id
		}
	}
	for i in 0 .. node.children_count {
		tc.collect_deferred_generic_pointer_comparisons(tc.a.child(node, i), param_types, generic_params, mut comparisons)
	}
}

fn (tc &TypeChecker) deferred_generic_expr_type_text(id flat.NodeId, param_types map[string]u32) u32 {
	if !tc.valid_node_id(id) { return 0 }
	node := tc.a.node(id)
	match node.kind {
		.ident { return param_types[node.value] or { node.typ } }
		.paren {
			if node.children_count > 0 {
				return tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types)
			}
		}
		.selector {
			if node.children_count == 0 { return node.typ }
			receiver_type_id := comptime_static_unwrap_type_id(tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types))
			syntax := flat.source_type_syntax(receiver_type_id)
			lookup_id := if syntax.application_valid() {
				syntax.application_base()
			} else {
				receiver_type_id
			}
			fields := tc.source_struct_field_decls(lookup_id)
			for field in fields {
				if field.name != node.value { continue }
				if syntax.application_valid() {
					params := tc.struct_generic_params[lookup_id] or {
						tc.struct_generic_params[flat.source_type_syntax(lookup_id).unqualified()] or { []string{} }
					}
					if params.len == syntax.application_args().len && params.len > 0 {
						return subst_generic_text(field.typ, syntax.application_args(), params)
					}
				}
				return field.typ
			}
		}
		.index {
			if node.children_count == 0 { return node.typ }
			clean_id := trimmed_source_type_id(tc.deferred_generic_expr_type_text(tc.a.child(node, 0), param_types))
			shape := flat.source_type_shape(clean_id)
			syntax := flat.source_type_syntax(clean_id)
			if shape.has(.array_type) { return trimmed_source_type_id(syntax.raw_base()) }
			if shape.has(.leading_bracket) && syntax.bracket_closed() && syntax.bracket_suffix() != 0 {
				return syntax.bracket_base()
			}
		}
		else {}
	}
	return node.typ
}

fn (tc &TypeChecker) concrete_generic_receiver_args(receiver_base_id u32, arg_count int) ?[]u32 {
	receiver_short_id := flat.source_type_syntax(receiver_base_id).unqualified()
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
		call_args_source_ids := tc.generic_call_type_arg_names(callee).clone()

		fn_params := tc.fn_generic_params[fn_name] or { continue }
		if call_args_source_ids.len == 0 || call_args_source_ids.len != fn_params.len {
			continue
		}
		ret_id := tc.fn_ret_type_texts.type_id(fn_name) or { continue }
		syntax := flat.source_type_syntax(comptime_static_unwrap_type_id(ret_id))
		if !syntax.application_valid() || syntax.application_args().len != arg_count
			|| flat.source_type_syntax(syntax.application_base()).unqualified() != receiver_short_id {
			continue
		}
		mut concrete := []u32{cap: syntax.application_args().len}
		for ret_arg_id in syntax.application_args() {
			concrete << subst_generic_text(ret_arg_id, call_args_source_ids, fn_params)
		}
		return concrete
	}
	return none
}

fn type_id_mentions_any_generic_param(typ u32, params []string) bool {
	for param in params {
		if flat.source_type_legacy_generic_param_matches(typ, flat.source_type_id(param)) {
			return true
		}
	}
	return false
}

fn generic_pointer_value_mismatch(lhs u32, rhs u32) bool {
	left_id := trimmed_source_type_id(lhs)
	right_id := trimmed_source_type_id(rhs)
	return (flat.source_type_shape(left_id).has(.pointer)
		&& trimmed_source_type_id(flat.source_type_syntax(left_id).raw_base()) == right_id)
		|| (flat.source_type_shape(right_id).has(.pointer)
			&& left_id == trimmed_source_type_id(flat.source_type_syntax(right_id).raw_base()))
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
	base_type := tc.resolve_type(tc.a.child(&node, 0))
	receiver := unwrap_all_pointers(base_type)
	clean := unalias_type(receiver)
	if receiver is Alias {
		underlying := unalias_type(receiver)
		if underlying is Struct && tc.struct_field_type(underlying.source_type_id(), node.value) != none {
			return false
		}
		if _ := tc.alias_method_value_decl_key(receiver, node.value) {
			return true
		}
		if _ := tc.promoted_method_owner(receiver, node.value) {
			return true
		}
	} else if clean is Struct {
		sname_id := clean.source_type_id()
		if tc.struct_field_type(sname_id, node.value) != none {
			return false
		}
		if _ := tc.c_struct_receiver_method_name(base_type, node.value) {
			return true
		}
		sname := flat.source_type_name(sname_id)
		if '${sname}.${node.value}' in tc.fn_param_types {
			return true
		}
		if receiver is Alias && '${receiver.name()}.${node.value}' in tc.fn_param_types {
			return true
		}
		if _ := tc.resolve_generic_struct_method(sname_id, node.value) {
			return true
		}
		if _ := tc.embedded_method_value_owner(sname_id, node.value) {
			return true
		}
	} else if clean is Interface {
		iname_id := clean.source_type_id()
		if tc.interface_field_type(iname_id, node.value) != none {
			return false
		}
		if receiver is Alias && '${receiver.name()}.${node.value}' in tc.fn_param_types {
			return true
		}
		if _ := tc.interface_receiver_method_call_info(iname_id, node.value) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) alias_method_value_decl_key(alias Alias, method string) ?string {
	if name := tc.c_struct_receiver_method_name(Type(alias), method) {
		return name
	}
	for receiver in [Type(alias), *alias.base_type] {
		for candidate in receiver_method_name_candidates(receiver, method, tc.cur_module) {
			if candidate in tc.fn_param_types || candidate in tc.fn_ret_types {
				return candidate
			}
		}
		type_name := resolve_type_name_for_method(receiver)
		if type_name != 0 {
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
	if type_name == 0 {
		return false
	}
	// A method promoted from an embedded struct borrows a part of this storage.
	owner := tc.promoted_method_owner(base_type, node.value) or { u32(0) }
	receiver_name := if owner != 0 { owner } else { type_name }
	if info := tc.resolve_generic_struct_method(receiver_name, node.value) {
		if tc.mut_receiver_methods[info.name] {
			return true
		}
	}
	if method_name := tc.c_struct_receiver_method_name(clean, node.value) {
		return tc.mut_receiver_methods[method_name]
	}
	method_names := if owner != 0 {
		['${flat.source_type_name(owner)}.${node.value}']
	} else {
		receiver_method_name_candidates(clean, node.value, tc.cur_module)
	}
	for method_name in method_names {
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
		tc.record_error(.assignment_mismatch, 'a method value with a mutable local receiver cannot escape its call site', id)
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

fn (tc &TypeChecker) stored_method_value_matches_voidptr_callback(id flat.NodeId, expected &Type) bool {
	if !tc.expr_is_method_value(id) {
		return false
	}
	actual := tc.resolve_type(id)
	return tc.method_value_matches_voidptr_callback(id, actual, expected)
}

fn (tc &TypeChecker) method_value_matches_voidptr_callback(id flat.NodeId, actual &Type, expected &Type) bool {
	if !tc.expr_is_method_value(id) {
		return false
	}
	actual_fn := fn_type_from_type(actual) or { return false }
	expected_fn := fn_type_from_type(expected) or { return false }
	if expected_fn.params.len != actual_fn.params.len + 1 {
		return false
	}
	if expected_fn.params.len == 0 || !fn_param_is_voidptr_type(expected_fn.params[0].typ) {
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
		tc.reject_capturing_fn_literal_escape(id, 'capturing fn literal cannot be stored or returned')
	}
}

fn (mut tc TypeChecker) reject_stored_capturing_fn_literal(id flat.NodeId) {
	tc.reject_capturing_fn_literal_escape(id, 'capturing fn literal cannot be stored in a container')
}

fn (mut tc TypeChecker) reject_stored_or_returned_capturing_fn_literal(id flat.NodeId) {
	tc.reject_capturing_fn_literal_escape(id, 'capturing fn literal cannot be stored or returned')
}

fn (mut tc TypeChecker) reject_capturing_fn_literal_escape(id flat.NodeId, message string) {
	_ = id
	_ = message
}

// InferredCGlobal is a C identifier that V has no declaration for, with the
// type that the context of its first use expects.
struct InferredCGlobal {
	name string
	typ  Type
}

// c_identifier_is_undeclared reports whether `C.<name>` is a C variable that V
// has no type for: it has no V declaration, and no checked use gave it one (see
// remember_inferred_c_global). Only a lower case name is a variable: the others
// are the macros and enum values of C, which V takes for integers.
pub fn (tc &TypeChecker) c_identifier_is_undeclared(name string) bool {
	if name.len == 0 || name[0] < `a` || name[0] > `z` || ascii_name_has_upper(name) {
		return false
	}
	qname := 'C.${name}'
	return qname !in tc.c_globals && qname !in tc.const_types && qname !in tc.fn_ret_types
}

// inferred_c_global_type returns the type that the undeclared C identifier
// `node` takes from the type `expected` of its context. A `mut` argument names
// the object that its parameter takes a reference to.
fn inferred_c_global_type(node flat.Node, expected Type) Type {
	if node.is_mut && expected is Pointer {
		return *expected.base_type
	}
	return expected
}

// remember_inferred_c_global gives the undeclared C identifier `qname` the
// type `typ` for the rest of the compilation. `c_globals` is shared with every
// other checker view, and a body is checked by a fork whose allocations are
// released with its batch: an entry added in place would leave the shared map
// pointing into that storage. So the checker replaces its own map by a copy
// that holds the entry, and notes it for the checker it is merged into (see
// merge_parallel_check_worker_scoped), which owns what it keeps.
fn (mut tc TypeChecker) remember_inferred_c_global(qname string, typ Type) {
	tc.c_globals = tc.c_globals.clone()
	tc.c_globals[qname] = typ
	tc.inferred_c_globals << InferredCGlobal{
		name: qname
		typ:  typ
	}
}

// module_type_spelling_id returns the spelling ID of `module_name.name`, composed
// from the module name and the declaration name without joining their text.
fn module_type_spelling_id(module_name string, name string) u32 {
	return flat.source_type_composition_two(.qualified, flat.source_type_id(module_name),
		flat.source_type_id(name))
}

// check_selector validates check selector state for types.
fn (mut tc TypeChecker) check_selector(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	if tc.reject_nonkeyword_enum_selector_escape(id, node) {
		return
	}
	if tc.valid_resolution_fast {
		tc.check_valid_selector(id, node)
		return
	}
	if node.value == '\$' {
		tc.check_comptime_field_selector(id, node, '', ComptimeStaticFieldCases{})
		return
	}
	if tc.check_unbound_instance_method_value(id, node) {
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
			&& tc.module_member_may_be_type(module_node.value, base.value) {
			module_name := tc.resolve_import_alias(module_node.value) or { module_node.value }
			display_module_name := tc.current_file_import_path_for_alias(module_node.value) or {
				module_name
			}
			enum_name_id := module_type_spelling_id(display_module_name, base.value)
			if resolved_enum_name_source_id := tc.resolve_enum_name(enum_name_id) {
				tc.register_synth_type(base_id, Type(Enum{
					name_id: resolved_enum_name_source_id
					is_flag: resolved_enum_name_source_id in tc.flag_enums
				}))
				// Declaration visibility is keyed by symbol strings.
				enum_name := '${display_module_name}.${base.value}'
				if _ := tc.private_declaration(enum_name) {
					start_pos := tc.node_value_diagnostic_pos(base_id)
					tc.record_error_at(.unknown_type, 'enum `${enum_name}` is private', id, token.new_span(node.pos.id, start_pos.offset, node.pos.end))
				}
			} else {
				qualified_type_id := module_type_spelling_id(module_name, base.value)
				if !tc.type_name_known(qualified_type_id) && !tc.type_symbol_known(qualified_type_id) {
					start_pos := tc.node_value_diagnostic_pos(base_id)
					enum_name := '${display_module_name}.${base.value}'
					tc.record_error_at(.unknown_type, 'unknown enum `${enum_name}` (type_idx=0)', id, token.new_span(node.pos.id, start_pos.offset, node.pos.end))
					tc.register_synth_type(id, builtin_void_type)
					return
				}
			}
		}
	}
	if typ := tc.enum_selector_type(&node) {
		tc.register_synth_type(id, typ)
		return
	}
	// `Color.nope`: the enum is a namespace below, which accepts any member, so a
	// value the enum does not declare would otherwise pass unnoticed.
	if base.kind == .ident && tc.ident_may_be_type(base.value) {
		if enum_name_source_id := tc.resolve_enum_name(base.source_value_id()) {
			if !tc.enum_has_field(enum_name_source_id, node.value)
				&& !tc.enum_member_is_callable(enum_name_source_id, base.source_value_id(), node.value) {
				enum_name := flat.source_type_name(enum_name_source_id)
				tc.record_error_at(.unknown_field, 'unknown enum field `${node.value}` for `${enum_name}`', id, tc.node_value_diagnostic_pos(id))
				tc.register_synth_type(id, Type(Enum{
					name_id: enum_name_source_id
					is_flag: enum_name_source_id in tc.flag_enums
				}))
				return
			}
		}
	}
	if base.kind == .prefix && base.op == .amp && base.children_count > 0 {
		addressed := tc.a.child_node(&base, 0)
		if addressed.kind == .struct_init {
			tc.check_node(base_id)
			tc.record_error_at(.assignment_mismatch, 'should not create object instance on the heap to simply access a member', id, token.new_span(node.pos.id, base.pos.offset, node.pos.end))
			return
		}
	}
	if tc.is_namespace_selector(node, base) {
		module_name := tc.resolve_import_alias(base.value) or { base.value }
		display_module_name := tc.current_file_import_path_for_alias(base.value) or { module_name }
		display_type_name := '${display_module_name}.${node.value}'
		if base.value == 'C' {
			qname := 'C.${node.value}'
			if tc.c_identifier_is_undeclared(node.value) {
				// C enum members may use lowercase names. Their integer type comes
				// from the enum initializer, including nested arithmetic expressions.
				if tc.enum_field_value_depth > 0 {
					tc.register_synth_type(id, tc.c_integer_constant_context_type(id))
					return
				}
				if expected := tc.expected_context_for_expr(id) {
					inferred := inferred_c_global_type(node, expected)
					tc.remember_inferred_c_global(qname, inferred)
					tc.register_synth_type(id, inferred)
					return
				}
				tc.record_error_at(.unknown_ident, 'undefined C identifier: `${qname}`', id, tc.node_value_diagnostic_pos(id))
				tc.register_synth_type(id, builtin_int_type)
				return
			}
			if c_upper_constant_is_pointer(qname) {
				tc.register_synth_type(id, builtin_voidptr_type)
				return
			}
			if node.value.len > 0 && node.value[0].is_capital()
				&& !tc.static_assoc_type_known(flat.source_type_id(qname)) {
				tc.register_synth_type(id, tc.c_integer_constant_context_type(id))
				return
			}
			// C preprocessor constants do not have V declarations. Like V1, infer
			// conventional all-uppercase macro names as integers.
			if node.value.len > 0 && !ascii_name_has_lower(node.value) {
				tc.register_synth_type(id, tc.c_integer_constant_context_type(id))
				return
			}
		}
		semantic_type_id := module_type_spelling_id(module_name, node.value)
		mut is_known_type := tc.type_name_known(semantic_type_id)
		if !is_known_type {
			for candidate in [
				'${base.value}.${node.value}',
				'${display_module_name.all_after_last('.')}.${node.value}',
			] {
				if tc.type_symbol_spelling_known(candidate) {
					is_known_type = true
					break
				}
			}
		}
		if tc.module_member_may_be_type(base.value, node.value) && is_known_type {
			if tc.resolve_enum_name(semantic_type_id) != none
				|| tc.resolve_enum_name(module_type_spelling_id(display_module_name, node.value)) != none {
				parent_id := tc.direct_parent_id(id)
				if tc.valid_node_id(parent_id) {
					parent := tc.a.node(parent_id)
					if parent.kind == .selector && parent.children_count > 0
						&& tc.a.child(parent, 0) == id {
						tc.register_synth_type(id, tc.parse_type(semantic_type_id))
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
			tc.record_error_at(.assignment_mismatch, '`${display_type_name}` must be initialized', id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, builtin_void_type)
			return
		}
		if node.value.len > 0 && node.value[0].is_capital() {
			display_type_id := module_type_spelling_id(display_module_name, node.value)
			if enum_name_source_id := tc.resolve_enum_name(display_type_id) {
				tc.register_synth_type(id, Type(Enum{
					name_id: enum_name_source_id
					is_flag: enum_name_source_id in tc.flag_enums
				}))
				return
			}
		}
		if base.value != 'C' && tc.module_member_may_be_type(base.value, node.value)
			&& is_known_type {
			tc.record_error_at(.assignment_mismatch, '`${display_type_name}` must be initialized', id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, builtin_void_type)
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
				tc.record_error_at(.unknown_ident, 'global `${qname}` is private', id, tc.node_value_diagnostic_pos(id))
			}
			tc.register_synth_type(id, typ)
			return
		}
		if typ := tc.const_type_for_selector(node) {
			qname := '${display_module_name}.${node.value}'
			if _ := tc.private_declaration(qname) {
				tc.record_error_at(.unknown_type, 'constant `${qname}` is private', id, tc.node_value_diagnostic_pos(id))
			}
			tc.register_synth_type(id, typ)
			return
		}
		if base.value == 'C' && c_int_selector_name(node.value) {
			tc.register_synth_type(id, builtin_int_type)
			return
		}
		if base.value == 'C' && node.value.len > 0 && node.value[0].is_capital() {
			constant_type := if c_upper_constant_is_pointer('C.${node.value}') {
				builtin_voidptr_type
			} else {
				builtin_int_type
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
			display_name := '${tc.diagnostic_module_display_name(module_name)}.${node.value}'
			message := util.new_suggestion(name, candidates).say('undefined ident: `${display_name}`')
			tc.record_error_at(.unknown_ident, message, id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, builtin_void_type)
		}
		return
	}
	if key := tc.selector_fn_value_key(node) {
		if typ := tc.fn_type_from_key(key) {
			tc.remember_resolved_fn_value(id, key)
			tc.register_synth_type(id, typ)
			return
		}
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
	if is_unresolved_name_type(base_type) {
		tc.register_synth_type(id, base_type)
		return
	}
	if base_type is Void {
		if tc.fn_context.undefined_variable_context_depth > 0 && tc.errors.any(it.node == base_id
			&& it.kind == .unknown_ident) {
			tc.register_synth_type(id, builtin_void_type)
			return
		}
		tc.record_error_at(.unknown_field, '`${tc.source_text_for_node(base_id)}` does not return a value', id, tc.node_value_diagnostic_pos(id))
		tc.record_enclosing_print_void(id)
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	if base_type is OptionType && node.value !in ['ok', 'value'] {
		tc.record_error_at(.unknown_field, 'cannot access fields of an Option, handle the error with `or {...}` or propagate it with `?`', id, tc.node_value_diagnostic_pos(id))
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	if smart_type := tc.smartcast_type(id) {
		tc.register_synth_type(id, smart_type)
		return
	}
	method_receiver := unalias_and_unwrap_pointer_type(base_type)
	if method_receiver is Struct && flat.source_type_syntax(method_receiver.source_type_id()).unqualified() == checker_tail_source_struct {
		if field_type := tc.inferred_anonymous_struct_field_type(base_id, node.value) {
			tc.register_synth_type(id, field_type)
			return
		}
	}
	// A method value resolved as a C receiver method has its privacy checked here already.
	mut c_receiver_method_value := ''
	if method_receiver is Struct && tc.struct_field_type(method_receiver.source_type_id(), node.value) == none {
		method_name, ambiguous := tc.lookup_c_struct_receiver_method(base_type, node.value)
		if ambiguous {
			tc.record_error_at(.unknown_field, 'ambiguous method `${node.value}` on `${method_receiver.name()}`', id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, builtin_void_type)
			return
		}
		if method_name.len == 0 && flat.source_type_syntax(method_receiver.source_type_id()).c_qualified()
			&& ((node.value == 'hex' && tc.type_is_pointer_receiver(base_type))
				|| tc.fn_key_is_static_associated('${method_receiver.name()}.${node.value}')
				|| tc.fn_key_is_static_associated(checker_qualified_fn_name(tc.cur_module, '${method_receiver.name()}.${node.value}')))
			&& !tc.ident_is_call_callee_or_generic_base(id) {
			tc.record_error_at(.unknown_field, 'unknown method `${node.value}` on `${base_type.name()}`', id, tc.node_value_diagnostic_pos(id))
			tc.register_synth_type(id, builtin_void_type)
			return
		}
		if method_name.len > 0 && !tc.ident_is_call_callee_or_generic_base(id) {
			if _ := tc.private_declaration(method_name) {
				tc.record_error_at(.unknown_field, 'method `${method_name}` is private', id, tc.node_value_diagnostic_pos(id))
			}
			c_receiver_method_value = method_name
			tc.remember_resolved_call(id, method_name)
			if tc.fn_context.node_id >= 0 {
				tc.method_values_by_fn[tc.fn_context.node_id] << method_name
			}
		}
	}
	if tc.expr_is_method_value(id) && !tc.ident_is_call_callee_or_generic_base(id) {
		tc.check_pointer_receiver_method_value_safety(id, node, base_type)
		receiver := unwrap_pointer(base_type)
		mut generic_method_key := ''
		if imported := tc.c_struct_receiver_method_name(base_type, node.value) {
			generic_method_key = imported
		} else if receiver is Alias {
			generic_method_key = tc.alias_method_value_decl_key(receiver, node.value) or { '' }
		} else {
			clean_receiver := unalias_type(receiver)
			if clean_receiver is Struct {
				direct_key := '${clean_receiver.name()}.${node.value}'
				if direct_key in tc.fn_generic_params {
					generic_method_key = direct_key
				} else if info := tc.resolve_generic_struct_method(clean_receiver.source_type_id(), node.value) {
					generic_method_key = info.name
				}
			}
		}
		if generic_method_key.len > 0
			&& (tc.fn_generic_params[generic_method_key] or { []string{} }).len > 0 {
			if receiver !is Struct
				|| tc.resolve_generic_struct_method(receiver.source_type_id(), node.value) == none {
				tc.record_error_at(.unsupported_generic, 'cannot use `${tc.source_text_for_node(id)}` as a generic function value', id, tc.node_value_diagnostic_pos(id))
				tc.register_synth_type(id, tc.fn_type_from_key(generic_method_key) or {
					builtin_void_type
				})
				return
			}
		}
	}
	// A value-context selector whose name is a method (not a field) of a struct
	// receiver is a method value; record the concrete `Type.method` so it survives
	// dead-code elimination (cgen emits a wrapper that calls it).
	union_receiver := unalias_type(unwrap_pointer(base_type))
	// Like the other `unsafe` checks, look at the enclosing blocks too: a selector
	// can be checked again by a later pass, outside the block's `unsafe_depth`.
	if union_receiver is Struct && union_receiver.source_type_id() in tc.unions && tc.unsafe_depth == 0
		&& !tc.expr_is_inside_unsafe_block(id) && !tc.translated_files[tc.cur_file]
		&& !tc.selector_is_assignment_lhs(id) {
		tc.record_warning_at(.unknown_field, 'reading a union field (or its address) requires `unsafe`', id, tc.selector_field_diagnostic_pos(id, node.value))
	}
	clean_recv := unwrap_all_pointers(base_type)
	selector_is_method_value := tc.expr_is_method_value(id)
		&& !tc.ident_is_call_callee_or_generic_base(id)
	// Selectors see through every pointer layer (`pp.field` with `pp` of type `&&Box`),
	// so the visibility checks use the receiver without any of them.
	visibility_recv := unwrap_all_pointers(base_type)
	if visibility_recv is Alias && !tc.selector_is_call_callee(id) {
		if tc.alias_struct_field_is_private_outside_module(visibility_recv, node.value) {
			tc.record_error_at(.unknown_field, 'field `${tc.diagnostic_type_name(Type(visibility_recv))}.${node.value}` is not public', id,
				tc.node_value_diagnostic_pos(id))
		} else if base.kind == .index {
			// `aliases[0].radius`: a field of a single variant, selected through an alias of a sum type.
			alias_target := unalias_and_unwrap_pointer_type(Type(visibility_recv))
			if alias_target is SumType && tc.sum_shared_field_type(alias_target, node.value) == none {
				if owner := tc.sum_unique_field_private_variant(alias_target, node.value) {
					tc.record_error_at(.unknown_field, 'field `${tc.diagnostic_type_name(owner)}.${node.value}` is not public', id,
						tc.node_value_diagnostic_pos(id))
				}
			}
		}
	}
	// A field selected through a sum type is a field of its struct variants.
	if visibility_recv is SumType && !tc.selector_is_call_callee(id) {
		if owner := tc.sum_type_private_field_owner(visibility_recv, node.value,
			base.kind == .index)
		{
			tc.record_error_at(.unknown_field, 'field `${tc.diagnostic_type_name(owner)}.${node.value}` is not public', id,
				tc.node_value_diagnostic_pos(id))
		}
	}
	if visibility_recv is Struct {
		if !tc.expr_is_rooted_in_c_namespace(base_id) {
			if visibility := tc.private_declaration(visibility_recv.name()) {
				if tc.is_synthesized_anon_struct(visibility_recv.source_type_id()) {
					// An anonymous struct has no name of its own that could be private: another
					// module can only reach it through a field or value of some other declaration.
					// What still applies there is the `pub` section of the field used here. The
					// field is named through the expression, since its struct has no name.
					if !tc.anonymous_struct_field_is_public(visibility_recv.source_type_id(), node.value,
						visibility.module_name) {
						tc.record_error_at(.unknown_field, 'field `${tc.source_text_for_node(base_id)}.${node.value}` is not public', id,
							tc.node_value_diagnostic_pos(id))
					}
				} else if flat.source_type_query(visibility_recv.source_type_id()).has(.c_prefix) || flat.source_type_query(visibility_recv.source_type_id()).has(.js_prefix)
					|| tc.struct_field_is_private_outside_module(visibility_recv.source_type_id(), node.value) {
					display_name := tc.diagnostic_type_name(Type(visibility_recv))
					decl_module := tc.diagnostic_module_display_name(visibility.module_name)
					inside_module := if tc.cur_module.len > 0 { tc.cur_module } else { 'main' }
					tc.record_error_at(.unknown_type, 'struct `${display_name}` was declared as private to module `${decl_module}`, so it can not be used inside module `${inside_module}`', id,
						tc.node_value_diagnostic_pos(id))
				}
			} else if !tc.selector_is_call_callee(id)
				&& tc.struct_field_is_private_outside_module(visibility_recv.source_type_id(), node.value) {
				tc.record_error_at(.unknown_field, 'field `${tc.diagnostic_type_name(Type(visibility_recv))}.${node.value}` is not public', id,
					tc.node_value_diagnostic_pos(id))
			}
		}
	}
	if selector_is_method_value && c_receiver_method_value.len == 0 && clean_recv is Struct
		&& tc.struct_field_type(clean_recv.source_type_id(), node.value) == none {
		mut declaration_key := '${clean_recv.name()}.${node.value}'
		if info := tc.resolve_generic_struct_method(clean_recv.source_type_id(), node.value) {
			declaration_key = info.name
		}
		if tc.private_declaration(declaration_key) != none {
			method_name := '${tc.diagnostic_type_name(Type(clean_recv))}.${node.value}'
			tc.record_error_at(.unknown_fn, 'method `${method_name}` is private', id,
				tc.node_value_diagnostic_pos(id))
		}
	}
	if selector_is_method_value && c_receiver_method_value.len == 0 && clean_recv is Alias
		&& tc.struct_field_type(clean_recv.source_type_id(), node.value) == none {
		if declaration_key := tc.concrete_method_signature_key(clean_recv.source_type_id(), node.value) {
			if tc.private_declaration(declaration_key) != none {
				method_name := '${tc.diagnostic_type_name(Type(clean_recv))}.${node.value}'
				tc.record_error_at(.unknown_fn, 'method `${method_name}` is private', id,
					tc.node_value_diagnostic_pos(id))
			}
		}
	}
	if clean_recv is Struct {
		if deprecation := tc.deprecated_symbols['${clean_recv.name()}.${node.value}'] {
			tc.record_deprecation(id, 'field', deprecation, tc.node_value_diagnostic_pos(id))
		}
		field_candidates := flat.TypeTextList{ ids: tc.embedded_field_candidates(clean_recv.source_type_id(), node.value) }.names()
		if field_candidates.len > 1 {
			pos := tc.node_value_diagnostic_pos(id)
			tc.record_error_at(.unknown_field, 'ambiguous field `${node.value}`', id, pos)
			mut possibilities := field_candidates.clone()
			possibilities.reverse_in_place()
			quoted_possibilities := possibilities.map('`${it}`')
			tc.record_error_at(.unknown_field, 'type `${clean_recv.name()}` has no field named `${node.value}`.\n${possibilities.len} possibilities: ${quoted_possibilities.join(', ')}.', id, pos)
			tc.register_synth_type(id, builtin_void_type)
			return
		}
		if selector_is_method_value && tc.struct_field_type(clean_recv.source_type_id(), node.value) == none {
			recv_name_id := tc.method_value_receiver_name(clean_recv.source_type_id(), node.value)
			recv_name := flat.source_type_name(recv_name_id)
			mut mkey := '${recv_name}.${node.value}'
			mut is_generic_method_value := false
			if mkey !in tc.fn_param_types {
				// Generic receiver methods are registered under the open key
				// (`Box[T].method`); mark that one reachable for the wrapper, and stash
				// the substituted signature for cgen (the open form is gone by cgen time).
				if ci := tc.resolve_generic_struct_method(recv_name_id, node.value) {
					tc.generic_method_value_info[recv_name_id][node.value] = ci
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
				concrete_mkey := '${recv_name}.${node.value}'
				if concrete_mkey != mkey {
					tc.method_values_by_fn[tc.fn_context.node_id] << concrete_mkey
				}
			}
		}
	} else if selector_is_method_value && clean_recv is Alias {
		underlying := unalias_type(clean_recv)
		has_field := underlying is Struct
			&& tc.struct_field_type(underlying.source_type_id(), node.value) != none
		if !has_field && tc.fn_context.node_id >= 0 {
			if mkey := tc.alias_method_value_decl_key(clean_recv, node.value) {
				tc.method_values_by_fn[tc.fn_context.node_id] << mkey
			} else if owner_source_id := tc.promoted_method_owner(clean_recv, node.value) {
				tc.record_promoted_method_value(owner_source_id, node.value)
			}
		}
	} else if selector_is_method_value && clean_recv is Interface {
		if tc.interface_field_type(clean_recv.source_type_id(), node.value) == none
			&& tc.fn_context.node_id >= 0 {
			if _ := tc.interface_receiver_method_call_info(clean_recv.source_type_id(), node.value) {
				tc.method_values_by_fn[tc.fn_context.node_id] << '${clean_recv.name()}.${node.value}'
			}
		}
	}
	if typ := tc.selector_type(id, node) {
		tc.register_synth_type(id, typ)
		return
	} else {
		if tc.should_diagnose(id) {
			// An unresolved base (`unknown`, or a pointer to it such as the result of a
			// generic call inferred from a comptime field) is resolved per instance.
			if base_type is Unknown || unwrap_pointer(base_type) is Unknown {
				return
			}
			clean_base := unalias_type(unwrap_pointer(base_type))
			if clean_base is Interface {
				tc.record_error_at(.unknown_field, 'type `${clean_base.name()}` has no field named `${node.value}`', id, tc.node_value_diagnostic_pos(id))
			} else if clean_base is Struct {
				if tc.ident_is_multi_pattern_match_subject(base_id) {
					tc.record_error_at(.unknown_field, 'type `${clean_base.name()}` has no field or method `${node.value}`', id, tc.node_value_diagnostic_pos(id))
					tc.register_synth_type(id, builtin_void_type)
					return
				}
				if tc.embedded_method_candidates(clean_base.source_type_id(), node.value).len > 1 {
					// Two embedded structs declare the method, as its call reports.
					tc.record_error_at(.unknown_field, 'ambiguous method `${node.value}`', id, tc.node_value_diagnostic_pos(id))
					tc.register_synth_type(id, builtin_void_type)
					return
				}
				mut candidates := []string{}
				for field in tc.struct_fields_for_init(clean_base.source_type_id()) {
					if field.name.len > 0 && field.name !in candidates {
						candidates << field.name
					}
				}
				message := util.new_suggestion(node.value, candidates).say('type `${clean_base.name()}` has no field named `${node.value}`')
				tc.record_error_at(.unknown_field, message, id, tc.node_value_diagnostic_pos(id))
			} else if clean_base is SumType {
				tc.record_error_at(.unknown_field, 'field `${node.value}` does not exist or have the same type in these sumtype `${clean_base.name()}` variants:', id, tc.node_value_diagnostic_pos(id))
			} else {
				tc.record_error_at(.unknown_field, '`${base_type.name()}` has no property `${node.value}`', id, tc.node_value_diagnostic_pos(id))
			}
		}
		tc.register_synth_type(id, builtin_void_type)
	}
}

fn (mut tc TypeChecker) reject_nonkeyword_enum_selector_escape(id flat.NodeId, node flat.Node) bool {
	if !node.value.starts_with('@')
		|| token.Token.from_string_tinyv(node.value[1..]).is_keyword() {
		return false
	}
	base := tc.a.child_node(&node, 0)
	mut enum_name_id := u32(0)
	if base.kind == .ident {
		enum_name_id = tc.resolve_enum_name(base.source_value_id()) or { u32(0) }
	} else if base.kind == .selector && base.children_count > 0 {
		module_node := tc.a.child_node(base, 0)
		if module_node.kind == .ident && tc.has_active_import(module_node.value) {
			module_name := tc.resolve_import_alias(module_node.value) or { module_node.value }
			enum_name_id = tc.resolve_enum_name(module_type_spelling_id(module_name, base.value)) or {
				u32(0)
			}
		}
	}
	if enum_name_id == 0 {
		return false
	}
	tc.record_error_at(.unknown_field, '`@` can only escape keyword enum members', id,
		tc.node_value_diagnostic_pos(id))
	tc.register_synth_type(id, builtin_void_type)
	return true
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

fn (mut tc TypeChecker) check_generic_fn_struct_init_type_args(node flat.Node) {
	mut stack := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		if tc.a.node(child_id).kind != .param {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := tc.a.node(id)
		if child.kind in [.fn_decl, .fn_literal] {
			continue
		}
		if child.kind == .struct_init {
			tc.check_generic_struct_init_type_args(id, child)
		}
		for i in 0 .. child.children_count {
			stack << tc.a.child(child, i)
		}
	}
}

fn (mut tc TypeChecker) check_generic_struct_init_type_args(id flat.NodeId, node flat.Node) {
	syntax := flat.source_type_syntax(node.source_value_id())
	if !syntax.application_valid() {
		return
	}
	base := syntax.application_base()
	args := syntax.application_args()
	qualified := tc.qualify_type_name(base)
	params := tc.struct_generic_params[base] or {
		tc.struct_generic_params[qualified] or { return }
	}
	if params.len == 0 {
		return
	}
	if args.len != params.len {
		tc.record_error_at(.unsupported_generic, 'generic struct init expects ${params.len} generic parameter, but got ${args.len}', id, tc.struct_init_head_pos(node))
		return
	}
	for arg_id in args {
		if !flat.source_type_syntax(arg_id).bare_generic() {
			continue
		}
		// The generic parameters of the current function are names.
		arg := flat.source_type_name(arg_id)
		if arg !in tc.fn_context.generic_params {
			current := '(${tc.fn_context.generic_params.join(',')})'
			tc.record_error_at(.unsupported_generic, 'generic struct init type parameter `${arg}` must be within the parameters `${current}` of the current generic function', id, tc.struct_init_head_pos(node))
			return
		}
	}
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
				tc.record_error(.call_arg_mismatch, '`${callee.value}` can not print void expressions', parent_id)
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
				tc.record_error_at(.assignment_mismatch, 'dump expression can not be void', id, tc.method_call_name_pos(node, callee))
				return
			}
		}
		if node.kind == .selector {
			tc.record_error_at(.assignment_mismatch, 'dump expression can not be void', id,
				tc.node_value_diagnostic_pos(id))
			return
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
fn (tc &TypeChecker) struct_embed_receiver_names(struct_name_id u32) []u32 {
	if receivers := tc.struct_embed_receivers.type_ids(struct_name_id) {
		return receivers.clone()
	}
	return tc.compute_struct_embed_receivers(struct_name_id)
}

fn (tc &TypeChecker) compute_struct_embed_receivers(struct_name_id u32) []u32 {
	fields := tc.structs[struct_name_id] or { return []u32{} }
	mut receivers := []u32{}
	for field in fields {
		embedded_type := embedded_field_type(field) or { continue }
		receiver := method_type_name(unwrap_pointer(embedded_type))
		if receiver == 0 {
			continue
		}
		receivers << receiver
	}
	return receivers
}

fn (mut tc TypeChecker) build_struct_embed_index() {
	tc.struct_embed_receivers = flat.SourceTypeListBindings{}
	for struct_name, _ in tc.structs {
		tc.struct_embed_receivers.set_ids(struct_name, tc.compute_struct_embed_receivers(struct_name))
	}
}

// embedded_method_candidates returns the embedded structs that a call of `method` on a
// `struct_name` could mean: more than one is an ambiguity. A method of the struct itself
// hides those of the structs it embeds, and so does a method of an embedded struct for
// the structs that one embeds in turn.
fn (tc &TypeChecker) embedded_method_candidates(struct_name_id u32, method string) []u32 {
	if tc.struct_embed_receiver_names(struct_name_id).len == 0
		|| '${flat.source_type_name(struct_name_id)}.${method}' in tc.fn_ret_types {
		return []u32{}
	}
	mut candidates := []u32{}
	mut seen := map[u32]bool{}
	tc.collect_embedded_method_candidates(struct_name_id, method, mut candidates, mut seen)
	return candidates
}

fn (tc &TypeChecker) collect_embedded_method_candidates(struct_name_id u32, method string, mut candidates []u32,
	mut seen map[u32]bool) {
	if struct_name_id in seen {
		return
	}
	seen[struct_name_id] = true
	for receiver in tc.struct_embed_receiver_names(struct_name_id) {
		if '${flat.source_type_name(receiver)}.${method}' in tc.fn_ret_types {
			if receiver !in candidates {
				candidates << receiver
			}
			continue
		}
		tc.collect_embedded_method_candidates(receiver, method, mut candidates, mut seen)
	}
}

fn (tc &TypeChecker) embedded_field_candidates(struct_name_id u32, field_name string) []u32 {
	if tc.struct_embed_receiver_names(struct_name_id).len == 0 {
		return []u32{}
	}
	mut candidates := []u32{}
	mut seen := map[u32]bool{}
	tc.collect_embedded_field_candidates(struct_name_id, field_name, mut candidates, mut seen)
	return candidates
}

fn (tc &TypeChecker) collect_embedded_field_candidates(struct_name_id u32, field_name string, mut candidates []u32,
	mut seen map[u32]bool) {
	if struct_name_id in seen {
		return
	}
	seen[struct_name_id] = true
	for receiver in tc.struct_embed_receiver_names(struct_name_id) {
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
	if base.value == 'main' || base.value == tc.cur_module
		|| base.value == short_name_view(tc.cur_module) {
		if (node.value in tc.const_types && tc.bare_const_key_is_visible(node.value))
			|| '${base.value}.${node.value}' in tc.const_types {
			return true
		}
		if tc.global_type_for_selector(node) != none
			|| tc.local_bare_fn_signature_key(node.value) != none {
			return true
		}
	}
	if resolved_id := tc.resolve_selective_import_type_symbol_text_id(base.value) {
		if resolved_id in tc.structs || resolved_id in tc.enum_names || resolved_id in tc.flag_enums
			|| tc.sum_types.contains(resolved_id) || resolved_id in tc.interface_names {
			return true
		}
	}
	qbase_id := tc.qualify_type_name(flat.source_type_id(base.value))
	if qbase_id in tc.structs || qbase_id in tc.enum_names || tc.sum_types.contains(qbase_id)
		|| qbase_id in tc.interface_names || tc.type_aliases.contains(qbase_id) {
		return true
	}
	// An alias of an enum (`type Col = Color`) is a namespace for its members: `Col.member`.
	if _ := tc.resolve_enum_name(base.source_value_id()) {
		return true
	}
	if _ := tc.static_assoc_fn_key_for_base(base.source_value_id(), node.value) {
		return true
	}
	// Const and fn keys are symbol strings.
	qname := '${flat.source_type_name(qbase_id)}.${node.value}'
	if qname in tc.const_types || qname in tc.fn_ret_types {
		return true
	}
	name_id := flat.source_type_existing_id(qname) or { return false }
	return name_id in tc.enum_names
}

fn (tc &TypeChecker) expr_is_rooted_in_c_namespace(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		return node.value == 'C'
	}
	if node.kind in [.selector, .index, .paren, .call] && node.children_count > 0 {
		return tc.expr_is_rooted_in_c_namespace(tc.a.child(node, 0))
	}
	return false
}

// selector_type supports selector type handling for TypeChecker.
fn (tc &TypeChecker) selector_type(_id flat.NodeId, node flat.Node) ?Type {
	if node.children_count == 0 {
		return none
	}
	base_id := tc.a.child(&node, 0)
	base_node := tc.a.nodes[int(base_id)]
	if base_node.kind == .ident && base_node.value == 'C' && c_int_selector_name(node.value) {
		return builtin_int_type
	}
	if node.kind == .selector {
		if typ := tc.enum_selector_type_from_base(base_id, node.value) {
			return typ
		}
	}
	if base_node.kind == .typeof_expr {
		if node.value == 'name' {
			return builtin_string_type
		}
		if node.value == 'idx' {
			return builtin_int_type
		}
		if node.value == 'indirections' {
			return builtin_u8_type
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
	mut alias_receiver_id := u32(0)
	if clean0 is Alias {
		alias_receiver_id = clean0.source_type_id()
	}
	clean := unalias_and_unwrap_pointer_type(base_type)
	if typ := option_result_selector_type(clean, node.value) {
		return typ
	}
	if node.value == 'len' {
		if clean is Array || clean is Map || clean is String || clean is ArrayFixed {
			return builtin_int_type
		}
	}
	if clean is Channel && node.value == 'closed' {
		return builtin_bool_type
	}
	if clean is Channel && node.value in ['len', 'cap'] {
		return builtin_int_type
	}
	if clean is Struct {
		clean_name_id := clean.source_type_id()
		if typ := tc.struct_field_type(clean_name_id, node.value) {
			return typ
		}
		// A method of the alias comes before the ones its struct has or promotes.
		if alias_receiver_id != 0
			&& '${flat.source_type_name(alias_receiver_id)}.${node.value}' in tc.fn_param_types {
			if typ := tc.method_value_type(alias_receiver_id, node.value) {
				return typ
			}
		}
		if method_name := tc.c_struct_receiver_method_name(base_type, node.value) {
			return tc.method_value_type(flat.source_type_id(method_name.all_before_last('.')), method_name.all_after_last('.'))
		}
		if typ := tc.method_value_type(clean_name_id, node.value) {
			return typ
		}
	}
	if clean is Array || clean is Map || clean is String {
		// A declared field (e.g. `string.str &u8`) shadows the builtin method
		// of the same name for selector access, including on string literals.
		sname_id := if clean is Array {
			qualify_source_type_array
		} else if clean is Map {
			qualify_source_type_map
		} else {
			flat.source_type_string
		}
		for f in tc.structs[sname_id] or { []StructField{} } {
			if f.name == node.value {
				return f.typ
			}
		}
	}
	if base_node.kind == .string_literal {
		method_name := 'string.${node.value}'
		if method_name in tc.fn_param_types || method_name in tc.fn_ret_types {
			return tc.method_value_type(flat.source_type_string, node.value)
		}
	}
	if typ := tc.builtin_method_value_type(base_type, node.value) {
		return typ
	}
	if alias_receiver_id != 0 {
		if typ := tc.method_value_type(alias_receiver_id, node.value) {
			return typ
		}
	}
	if clean is Interface {
		if typ := tc.interface_field_type(clean.source_type_id(), node.value) {
			return typ
		}
		if typ := tc.method_value_type(clean.source_type_id(), node.value) {
			return typ
		}
	}
	if clean is MultiReturn {
		if typ := multi_return_selector_type(clean, node.value) {
			return typ
		}
	}
	if node.value in ['message', '_object'] && is_builtin_ierror_name(clean.source_type_id()) {
		if node.value == 'message' {
			return builtin_string_type
		}
		if node.value == '_object' {
			return *tc.parse_type(flat.source_type_voidptr)
		}
	}
	if clean is SumType {
		if typ := tc.sum_shared_field_type(clean, node.value) {
			return typ
		}
		// Like V1, a field that only one variant has is read through that variant
		// (an implicit, checked `as` cast) when the sum value cannot be smartcast,
		// e.g. `shapes[0].radius` or `def.range().entries`.
		if base_node.kind == .index || tc.expr_key(base_id).len == 0 {
			if typ := tc.sum_unique_variant_field_type(clean, node.value) {
				return typ
			}
		}
		if typ := tc.lowered_sum_selector_type(clean, node.value) {
			return typ
		}
	}
	if clean is Array || clean is Map || clean is String {
		sname_id := if clean is Array {
			qualify_source_type_array
		} else if clean is Map {
			qualify_source_type_map
		} else {
			flat.source_type_string
		}
		if fields := tc.structs[sname_id] {
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
		return builtin_int_type
	}
	variants := tc.sum_types.type_ids(sum.source_type_id()) or { return none }
	// A field name that was never interned equals no variant spelling.
	mut field_id := u32(0)
	mut field_interned := false
	if existing_id := flat.source_type_existing_id(field) {
		field_id = existing_id
		field_interned = true
	}
	for variant_id in variants {
		if (field_interned && (field_id == variant_id
			|| field_id == flat.source_type_syntax(variant_id).unqualified()))
			|| field == tc.cached_c_name(flat.source_type_name(variant_id)) {
			return *tc.parse_type(variant_id)
		}
	}
	return none
}

// sum_shared_field_type supports sum shared field type handling for TypeChecker.
fn (tc &TypeChecker) sum_shared_field_type(sum SumType, field string) ?Type {
	mut visited := map[u32]bool{}
	return tc.sum_shared_field_type_inner(sum.source_type_id(), field, mut visited)
}

fn (tc &TypeChecker) sum_shared_field_type_inner(sum_name_id u32, field string, mut visited map[u32]bool) ?Type {
	base_id := tc.sum_base_name(sum_name_id)
	if visited[base_id] {
		return none
	}
	visited[base_id] = true
	defer {
		visited.delete(base_id)
	}
	variants := tc.sum_types.type_ids(base_id) or { return none }
	if variants.len == 0 {
		return none
	}
	mut has_common := false
	mut common_typ := builtin_void_type
	for variant_id in variants {
		concrete_source_id_2 := tc.concrete_sum_variant_name(sum_name_id, variant_id)

		variant_type := unalias_type(tc.parse_type(concrete_source_id_2))
		variant_field := if variant_type is SumType {
			tc.sum_shared_field_type_inner(variant_type.source_type_id(), field, mut visited) or { return none }
		} else if variant_type is Struct {
			tc.struct_field_type(variant_type.source_type_id(), field) or { return none }
		} else {
			return none
		}
		if !has_common {
			common_typ = variant_field
			has_common = true
			continue
		}
		if variant_field.source_type_id() != common_typ.source_type_id() {
			return none
		}
	}
	return common_typ
}

fn (tc &TypeChecker) sum_unique_variant_field_type(sum SumType, field string) ?Type {
	mut visited := map[u32]bool{}
	return tc.sum_unique_variant_field_type_inner(sum.source_type_id(), field, mut visited)
}

fn (tc &TypeChecker) sum_unique_variant_field_type_inner(sum_name_id u32, field string, mut visited map[u32]bool) ?Type {
	base_id := tc.sum_base_name(sum_name_id)
	if visited[base_id] {
		return none
	}
	visited[base_id] = true
	defer {
		visited.delete(base_id)
	}
	variants := tc.sum_types.type_ids(base_id) or { return none }
	mut found := builtin_void_type
	mut found_count := 0
	for variant_id in variants {
		concrete_source_id_3 := tc.concrete_sum_variant_name(sum_name_id, variant_id)

		variant_type := unalias_type(tc.parse_type(concrete_source_id_3))
		variant_field := if variant_type is SumType {
			tc.sum_unique_variant_field_type_inner(variant_type.source_type_id(), field, mut visited) or {
				continue
			}
		} else if variant_type is Struct {
			tc.struct_field_type(variant_type.source_type_id(), field) or { continue }
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

fn (tc &TypeChecker) sum_type_contains_variant(sum SumType, target &Type) bool {
	mut visited := map[u32]bool{}
	return tc.sum_type_contains_variant_seen(sum, target, mut visited)
}

fn (tc &TypeChecker) sum_type_contains_variant_seen(sum SumType, target &Type, mut visited map[u32]bool) bool {
	base_id := tc.sum_base_name(sum.source_type_id())
	if visited[base_id] {
		return false
	}
	visited[base_id] = true
	for variant_source_id_2 in tc.sum_types.type_ids(base_id) or { []u32{} } {
		variant_source_id_4 := variant_source_id_2

		concrete := tc.parse_type(tc.concrete_sum_variant_name(sum.source_type_id(), variant_source_id_4))
		if concrete.source_type_id() == target.source_type_id()
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
			return builtin_bool_type
		}
		if field == 'value' {
			return *typ.base_type
		}
	}
	if typ is ResultType {
		if field == 'ok' {
			return builtin_bool_type
		}
		if field == 'value' {
			return *typ.base_type
		}
	}
	return none
}

// index_operator_call_info resolves a declared index operator for the receiver type.
pub fn (tc &TypeChecker) index_operator_call_info(base_type Type, op string) ?CallInfo {
	if tc.method_suffix_prescreen && tc.top_level_idx.len > 0
		&& op !in tc.receiver_method_suffix_index
		&& '@${op}' !in tc.receiver_method_suffix_index {
		return none
	}
	clean := unwrap_pointer(base_type)
	type_name := resolve_type_name_for_method(clean)
	if type_name == 0 {
		return none
	}
	c_method_name, ambiguous := tc.lookup_c_struct_receiver_method(base_type, op)
	if ambiguous {
		return none
	}
	if c_method_name.len > 0 {
		if _ := tc.private_declaration(c_method_name) {
			return none
		}
		info := tc.call_info(c_method_name, true)
		if info.params.len == 0 || tc.method_receiver_compatible(base_type, info.params[0], c_method_name) {
			return info
		}
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
			tc.record_error(.cannot_index, 'overloaded `${op}` expression accepts one index, got ${node.children_count - 1}', id)
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
		tc.type_mismatch(.cannot_index, 'index must be `${expected_key.name()}`, not `${actual_key.name()}`', index_id)
		return false
	}
	return true
}

fn js_index_type_requires_int_cast(typ &Type) bool {
	return unalias_type(typ).source_type_id() in [flat.source_type_i64, flat.source_type_u64,
		flat.source_type_isize, flat.source_type_uint]
}

fn (mut tc TypeChecker) check_js_index_type(id flat.NodeId, typ Type, base_type Type) bool {
	if !tc.is_js_backend || !js_index_type_requires_int_cast(typ) {
		return false
	}
	type_name := tc.diagnostic_expr_type_name(id, typ)
	container := if base_type is String { 'string ' } else { '' }
	tc.record_error_at(.cannot_index, 'cannot use `${type_name}` as ${container}index type `int`, use an explicit cast like `int(expr)`', id, tc.a.node(id).pos)
	return true
}

// check_index validates check index state for types.
// fixed_array_slice_is_view reports whether `fixed[a..b]` at `id` is written
// to, which makes the transform slice the fixed array's own storage instead of
// a copy: a `mut` argument, the receiver of an in-place sort or reverse, or
// the destination of copy().
fn (tc &TypeChecker) fixed_array_slice_is_view(id flat.NodeId) bool {
	mut current := id
	mut parent := tc.direct_parent_id(current)
	for tc.valid_node_id(parent) && parent != current && tc.a.node(parent).kind == .paren {
		current = parent
		parent = tc.direct_parent_id(current)
	}
	if tc.a.node(id).is_mut || tc.a.node(current).is_mut {
		return true
	}
	if !tc.valid_node_id(parent) || parent == current {
		return false
	}
	outer := tc.a.node(parent)
	// A selector's only child is its base.
	if outer.kind == .selector && outer.value in ['sort', 'sort_with_compare', 'reverse_in_place'] {
		return true
	}
	if outer.kind == .call && outer.children_count > 1 && tc.a.child(outer, 1) == current {
		callee := tc.a.child_node(outer, 0)
		return callee.kind == .ident && callee.value == 'copy'
	}
	return false
}

fn (mut tc TypeChecker) check_index(id flat.NodeId, node flat.Node) {
	if node.children_count == 0 {
		return
	}
	generic_base_node := tc.a.child_node(&node, 0)
	// A generic method named on a value with its type arguments and without a
	// call, `h.first[int]`, as `first[int]` names a function.
	if generic_base_node.kind == .selector && !tc.ident_is_call_callee_or_generic_base(id) {
		if method_type := tc.check_generic_method_value(id, node) {
			tc.register_synth_type(id, method_type)
			return
		}
	}
	if name := tc.generic_call_base_name(generic_base_node) {
		type_args := flat.TypeTextList{ ids: tc.generic_call_type_arg_names(node) }.names()
		mut has_unresolved_generic := false
		for type_arg in type_args {
			if is_bare_generic_param(type_arg) && !tc.active_generic_param(type_arg) {
				has_unresolved_generic = true
				break
			}
		}
		if has_unresolved_generic && (tc.fn_generic_params[name] or { []string{} }).len > 0 {
			base_id := tc.a.child(&node, 0)
			tc.record_error_at(.unsupported_generic, 'a generic fn with generic types, cannot be used outside of another generic fn', base_id, tc.node_value_diagnostic_pos(base_id))
			tc.register_synth_type(id, tc.fn_type_from_key(name) or { builtin_void_type })
			return
		}
	}
	if generic_fn_type := tc.explicit_generic_fn_value_type(node) {
		tc.check_generic_fn_value_constraints(id, node)
		tc.register_synth_type(id, generic_fn_type)
		return
	}
	base_id := tc.a.child(&node, 0)
	tc.check_storage_path_base_node(base_id)
	mut base_type_raw := tc.resolve_type(base_id)
	if is_unresolved_name_type(base_type_raw) {
		for i in 1 .. node.children_count {
			tc.check_node(tc.a.child(&node, i))
		}
		tc.register_synth_type(id, base_type_raw)
		return
	}
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
			tc.record_error_at(.cannot_index, 'field `${base_node.value}` is an Option, so it should have either an `or {}` block, or `?` at the end', base_id, base_pos)
		}
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	mut base_type := unalias_and_unwrap_pointer_type(base_type_raw)
	if map_alias := tc.map_type_from_alias_target(base_type_raw) {
		base_type = Type(map_alias)
	} else if map_alias := tc.map_type_from_alias_target(base_type) {
		base_type = Type(map_alias)
	}
	if base_type_raw.source_type_id() in [flat.source_type_voidptr, flat.source_type_nil] {
		type_name := tc.pointer_diagnostic_binding_type_name(base_id, base_type_raw)
		tc.record_error_at(.cannot_index, 'type `${type_name}` does not support indexing', id, tc.index_brackets_pos(node))
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	pointer_base_node := tc.a.node(base_id)
	mut_param_base := if pointer_base_node.kind == .ident {
		*unalias_type(tc.mut_param_expr_base(base_id, base_type_raw) or { Type(void_) })
	} else {
		builtin_void_type
	}
	// Ordinary `mut x T` parameters are represented as pointers in the ABI but retain
	// value semantics in the function body. An explicitly pointer-typed `mut x &T`
	// parameter retains pointer semantics, including indexing.
	explicit_mut_param_pointer := pointer_base_node.kind == .ident
		&& tc.current_fn_param_is_explicit_mut_pointer(pointer_base_node.value)
	implicit_mut_param_pointer := mut_param_base !is Void && mut_param_base !is Pointer
		&& !explicit_mut_param_pointer
	pointer_container_param := pointer_base_node.kind == .ident
		&& tc.current_fn_param_type_text(pointer_base_node.value) != none
		&& (base_type is Array || base_type is ArrayFixed || base_type is Map
			|| base_type is String)
	locked_shared_pointer_container := pointer_base_node.kind == .ident
		&& tc.current_binding_is_shared(pointer_base_node.value)
		&& tc.current_shared_lock_mode(pointer_base_node.value) != 0
		&& (base_type is Array || base_type is ArrayFixed || base_type is Map
			|| base_type is String)
	outside_unsafe := tc.unsafe_depth == 0 && !tc.expr_is_inside_unsafe_block(id)
	if node.value != 'range' && implicit_mut_param_pointer && base_type is Struct
		&& outside_unsafe {
		if tc.fn_context.generic_params.len > 0 && flat.source_type_shape(base_type.source_type_id()).has(.bracketed) {
			tc.register_synth_type(id, tc.resolve_index_type(node))
			return
		}
		tc.record_error_at(.cannot_index, 'type `mut ${base_type.name()}` does not support slicing', id, tc.index_brackets_pos(node))
		tc.register_synth_type(id, mut_param_base)
		return
	}
	if node.value != 'range' && base_type_raw is Pointer && !implicit_mut_param_pointer
		&& !explicit_mut_param_pointer && mut_param_base !is Pointer && !pointer_container_param
		&& !locked_shared_pointer_container && outside_unsafe
		&& !tc.translated_files[tc.cur_file] && !tc.node_is_in_translated_file(id) {
		tc.record_error_at(.cannot_index, 'pointer indexing is only allowed in `unsafe` blocks', id, tc.index_brackets_pos(node))
		tc.register_synth_type(id, tc.resolve_index_type(node))
		return
	}
	if info := tc.index_overload_call_info(base_type_raw, false) {
		tc.remember_resolved_call(id, info.name)
		tc.check_index_overload_args(id, node, info)
		tc.register_synth_type(id, info.return_type)
		return
	}
	if node.value == 'range' {
		// Slicing a string or a fixed array copies it; slicing an array shares
		// the array's buffer.
		if base_type is String {
			tc.warn_alloc('string slice', id, node.pos)
		} else if base_type is ArrayFixed && !tc.fixed_array_slice_is_view(id) {
			tc.warn_alloc('slice of a fixed array', id, node.pos)
		}
		mut range_out_of_bounds_reported := false
		if !(base_type is Array || base_type is ArrayFixed || base_type is String)
			&& tc.should_diagnose(id) {
			base_node := tc.a.node(base_id)
			type_name := if base_node.kind == .cast_expr
				&& base_node.source_value_id() in [flat.source_type_byteptr, flat.source_type_charptr,
					flat.source_type_voidptr] {
				base_node.value_text()
			} else {
				base_type_raw.name()
			}
			action := if base_type_raw is Pointer { 'slicing' } else { 'indexing' }
			tc.record_error_at(.cannot_index, 'type `${type_name}` does not support ${action}', id, tc.index_brackets_pos(node))
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
				} else if !tc.check_js_index_type(bound_id, bound_type, base_type) {
					if node.op != .gated_index {
						if value := tc.index_literal_value(bound_id) {
							if value < 0 {
								tc.record_error(.cannot_index, 'negative index `${value}`', bound_id)
							} else if base_type is ArrayFixed {
								if length := tc.fixed_array_len_value(base_type) {
									if value > length && !range_out_of_bounds_reported {
										tc.record_error(.cannot_index, 'index out of range (index: ${value}, len: ${length})', bound_id)
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
		tc.record_error(.cannot_index, 'index expression accepts one index, got ${node.children_count - 1}', id)
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
				tc.record_error_at(.cannot_index, 'invalid key: cannot use `?${actual_key.base_type.name()}` as `${base_type.key_type.name()}`, it must be unwrapped first', index_id, pos)
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
				tc.record_error_at(.cannot_index, message, index_id, token.new_span(node.pos.id, tc.a.node(base_id).pos.end, node.pos.end))
			}
			if tc.unsafe_depth == 0 && unalias_type(base_type.value_type) is SumType
				&& !tc.index_is_handled_by_guard_or_or_block(id)
				&& !tc.index_is_assignment_target(id) {
				tc.record_warning_at(.cannot_index, '`or {}` block required when indexing a map with sum type value', id, token.new_span(node.pos.id, tc.a.node(base_id).pos.end, node.pos.end))
			}
			if tc.cur_file in tc.strict_map_index_files
				&& !tc.index_is_handled_by_guard_or_or_block(id)
				&& !tc.index_is_assignment_target(id) {
				tc.record_error_at(.cannot_index, '`@[strict_map_index]` requires handling missing map keys with `or {}` or `if value := map[key] {}`', id, tc.index_brackets_pos(node))
			}
			if tc.unsafe_depth == 0 && !tc.index_is_handled_by_guard_or_or_block(id)
				&& !tc.index_is_assignment_target(id) {
				value_type := unalias_type(base_type.value_type)
				index_pos := token.new_span(node.pos.id, tc.a.node(base_id).pos.end, node.pos.end)
				if value_type is Pointer {
					tc.record_warning_at(.cannot_index, 'accessing a pointer map value requires an `or {}` block outside `unsafe`', id, index_pos)
				}
				mut visited := map[u32]bool{}
				if tc.type_contains_pointer(value_type, mut visited) {
					tc.record_warning_at(.cannot_index, 'accessing map value that contain pointers requires an `or {}` block outside `unsafe`', id, index_pos)
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
			tc.type_mismatch(.cannot_index, 'non-integer index `void` (array type `${base_type.name()}`)', index_id)
		} else if index_type !is Unknown && index_type !is Enum && !index_type.is_integer()
			&& !(tc.node_is_in_translated_file(index_id) && translated_integer_type(index_type)) {
			if index_type is OptionType || index_type is ResultType {
				tc.type_mismatch(.cannot_index, 'cannot use Option or Result as index (array type `${base_type.name()}`)', index_id)
			} else {
				type_name := tc.diagnostic_expr_type_name(index_id, index_type)
				message := if base_type is String {
					'non-integer string index `${type_name}`'
				} else {
					'non-integer index `${type_name}` (array type `${base_type.name()}`)'
				}
				tc.type_mismatch(.cannot_index, message, index_id)
			}
		} else if tc.check_js_index_type(index_id, index_type, base_type) {
			// The JavaScript backend requires explicit narrowing to its `int` index type.
		} else if node.op != .gated_index {
			if value := tc.index_literal_value(index_id) {
				translated_pointer := base_type_raw is Pointer && !implicit_mut_param_pointer
					&& tc.node_is_in_translated_file(id)
				if value < 0 && !translated_pointer {
					tc.record_error(.cannot_index, 'negative index `${value}`', index_id)
				} else if base_type is ArrayFixed {
					if length := tc.fixed_array_len_value(base_type) {
						if value >= length {
							tc.record_error(.cannot_index, 'index out of range (index: ${value}, len: ${length})', index_id)
						}
					}
				}
			}
		}
	}
	if !(base_type is Array || base_type is ArrayFixed || base_type is String
		|| base_type is Map || base_type is Unknown
		|| (base_type_raw is Pointer && !implicit_mut_param_pointer)) && tc.should_diagnose(id) {
		tc.record_error_at(.cannot_index, 'type `${base_type.name()}` does not support indexing', id, tc.index_brackets_pos(node))
	}
	tc.register_synth_type(id, tc.resolve_index_type(node))
}

fn (mut tc TypeChecker) check_valid_selector(id flat.NodeId, node flat.Node) {
	if node.value == '\$' {
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
			&& tc.module_member_may_be_type(module_node.value, base.value) {
			module_name := tc.resolve_import_alias(module_node.value) or { module_node.value }
			enum_name_id := module_type_spelling_id(module_name, base.value)
			if resolved_enum_name_source_id := tc.resolve_enum_name(enum_name_id) {
				tc.register_synth_type(base_id, Type(Enum{
					name_id: resolved_enum_name_source_id
					is_flag: resolved_enum_name_source_id in tc.flag_enums
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
			if tc.c_identifier_is_undeclared(node.value) {
				// C enum members may use lowercase names. Their integer type comes
				// from the enum initializer, including nested arithmetic expressions.
				if tc.enum_field_value_depth > 0 {
					tc.register_synth_type(id, tc.c_integer_constant_context_type(id))
					return
				}
				if expected := tc.expected_context_for_expr(id) {
					inferred := inferred_c_global_type(node, expected)
					tc.remember_inferred_c_global(qname, inferred)
					tc.register_synth_type(id, inferred)
				} else {
					tc.register_synth_type(id, builtin_int_type)
				}
				return
			}
			if c_upper_constant_is_pointer(qname) {
				tc.register_synth_type(id, builtin_voidptr_type)
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
				builtin_voidptr_type
			} else {
				tc.c_integer_constant_context_type(id)
			})
		}
		_ = module_name
		return
	}
	if key := tc.selector_fn_value_key(node) {
		if typ := tc.fn_type_from_key(key) {
			tc.remember_resolved_fn_value(id, key)
			tc.register_synth_type(id, typ)
			return
		}
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
	clean_recv := unwrap_all_pointers(base_type)
	if clean_recv is Struct {
		if tc.struct_field_type(clean_recv.source_type_id(), node.value) != none {
			return
		}
		recv_name_id := tc.method_value_receiver_name(clean_recv.source_type_id(), node.value)
		recv_name := flat.source_type_name(recv_name_id)
		mut mkey := '${recv_name}.${node.value}'
		mut is_generic_method_value := false
		if mkey !in tc.fn_param_types {
			if ci := tc.resolve_generic_struct_method(recv_name_id, node.value) {
				tc.generic_method_value_info[recv_name_id][node.value] = ci
				mkey = ci.name
				is_generic_method_value = true
			}
		}
		if mkey in tc.fn_param_types || is_generic_method_value {
			tc.method_values_by_fn[tc.fn_context.node_id] << mkey
			concrete_mkey := '${recv_name}.${node.value}'
			if concrete_mkey != mkey {
				tc.method_values_by_fn[tc.fn_context.node_id] << concrete_mkey
			}
		}
		return
	}
	if clean_recv is Alias {
		underlying := unalias_type(clean_recv)
		has_field := underlying is Struct
			&& tc.struct_field_type(underlying.source_type_id(), node.value) != none
		if !has_field {
			if mkey := tc.alias_method_value_decl_key(clean_recv, node.value) {
				tc.method_values_by_fn[tc.fn_context.node_id] << mkey
			} else if owner_source_id := tc.promoted_method_owner(clean_recv, node.value) {
				tc.record_promoted_method_value(owner_source_id, node.value)
			}
		}
		return
	}
	if clean_recv is Interface && tc.interface_field_type(clean_recv.source_type_id(), node.value) == none {
		if _ := tc.interface_receiver_method_call_info(clean_recv.source_type_id(), node.value) {
			tc.method_values_by_fn[tc.fn_context.node_id] << '${clean_recv.name()}.${node.value}'
		}
	}
}

fn (tc &TypeChecker) map_key_type_compatible(actual &Type, expected &Type) bool {
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

fn (tc &TypeChecker) type_contains_pointer(typ &Type, mut visited map[u32]bool) bool {
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
		if visited[clean.name_id] {
			return false
		}
		visited[clean.name_id] = true
		decl_mod := tc.struct_module_for_type(clean.source_type_id())
		same_main_module := decl_mod in ['', 'main'] && tc.cur_module in ['', 'main']
		for field in tc.struct_fields_for_init(clean.source_type_id()) {
			if decl_mod.len > 0 && decl_mod != tc.cur_module && !same_main_module {
				is_public := tc.visible_mutation_struct_field_is_public(clean.source_type_id(), field.name,
					decl_mod) or { true }
				if !is_public {
					continue
				}
			}
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
			tc.record_error(.cannot_index, 'multi-index expressions on overloaded `[]` require a `[]SliceIndex` parameter', id)
		}
		return
	}
	if has_range && !accepts_slice && !accepts_slice_array {
		if tc.should_diagnose(id) {
			tc.record_error(.cannot_index, 'slice expressions on overloaded `[]` require `SliceIndex` or `[]SliceIndex` parameters', id)
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
		tc.type_mismatch(.cannot_index, 'cannot use `${actual.name()}` as overloaded index; expected `${expected.name()}`', id)
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
		tc.type_mismatch(.cannot_index, 'slice bound must be integer, not `${bound_type.name()}`', id)
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

fn is_builtin_slice_index_type(typ &Type) bool {
	clean0 := unalias_type(typ)
	clean := unwrap_pointer(clean0)
	return clean.source_type_id() in [checker_source_slice_index, checker_source_builtin_slice_index]
}

fn is_builtin_slice_index_array_type(typ &Type) bool {
	clean := unalias_type(typ)
	if clean is Array {
		return is_builtin_slice_index_type(clean.elem_type)
	}
	return false
}

// check_ident validates check ident state for types.
fn (mut tc TypeChecker) check_ident(id flat.NodeId, node flat.Node) {
	name := node.value_text()
	if name.len == 0 {
		return
	}
	if tc.fn_context.closure_forbidden_captures[name]
		&& tc.fn_context.closure_scope != unsafe { nil } {
		if owner := tc.cur_scope.lookup_owner(name) {
			if !owner.belongs_to_scope_chain_until(tc.cur_scope, tc.fn_context.closure_scope) {
				if tc.fn_context.lambda_no_captures {
					tc.record_error_at(.unknown_ident, 'undefined variable `${name}`', id, tc.node_value_diagnostic_pos(id))
					tc.register_synth_type(id, builtin_void_type)
					return
				}
				tc.record_error_with_details_at(.unknown_ident, '`${name}` must be explicitly listed as inherited variable to be used inside a closure', id, tc.node_value_diagnostic_pos(id), [
					'use `fn [${name}] () {` instead of `fn () {`',
				])
				parent_id := tc.direct_parent_id(id)
				if tc.valid_node_id(parent_id) {
					parent := tc.a.node(parent_id)
					if parent.kind == .call && parent.children_count > 0 {
						callee := tc.a.child_node(parent, 0)
						if callee.kind == .ident
							&& callee.value in ['print', 'println', 'eprint', 'eprintln'] {
							tc.record_error_at(.call_arg_mismatch, '`${callee.value}` can not print void expressions', parent_id, parent.pos)
							tc.record_error_at(.unknown_ident, 'undefined ident: `${name}`', id, tc.node_value_diagnostic_pos(id))
						}
					}
				}
				tc.register_synth_type(id, builtin_void_type)
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
			&& tc.a.node(selector_parent_id).kind in [.selector, .index, .paren, .as_expr] {
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
			if typ := tc.non_file_scope_type(name) {
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
	if name == '_' {
		tc.record_error_at(.unknown_ident, 'undefined ident: `_` (may only be used in assignments)', id, tc.node_value_diagnostic_pos(id))
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	if name[0] == `@` && !(name.len > 1
		&& token.Token.from_string_tinyv(name[1..]).is_keyword()) {
		pos := token.new_pos(node.pos.id, int_max(node.pos.offset, node.pos.end - 1))
		tc.record_error_with_details_at(.unknown_ident, '@ must be used before keywords or compile time variables (e.g. `@type string` or `@FN`)', id, pos, [
			'available compile time variables: @VROOT, @VMODROOT, @VEXEROOT, @FN, @METHOD, @MOD,\n@STRUCT, @VEXE, @FILE, @DIR, @LINE, @COLUMN, @VHASH, @VCURRENTHASH, @VMOD_FILE, @VMODHASH,\n@FILE_LINE, @LOCATION, @BUILD_DATE, @BUILD_TIME, @BUILD_TIMESTAMP, @OS, @CCOMPILER,\n@BACKEND, @PLATFORM',
		])
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	$if ownership ? {
		tc.ownership_check_ident(id, node)
	}
	if is_bare_generic_param(name) {
		active_generic_ident := name in tc.fn_context.generic_params
			|| tc.active_generic_param(name)
			|| tc.node_has_enclosing_generic_param(id, name)
			|| tc.source_enclosing_fn_has_generic_param(id, name)
		if active_generic_ident {
			tc.register_synth_type(id, generic_placeholder_type(node.source_value_id()))
			return
		}
	}
	if !tc.bare_name_is_const_owned_over_global(name) {
		if typ := tc.non_file_scope_type(name) {
			if typ is Unknown && typ.reason == 'invalid variable' {
				tc.record_error_at(.unknown_ident, 'invalid variable `${name}`', id, tc.node_value_diagnostic_pos(id))
				tc.register_synth_type(id, builtin_void_type)
				tc.fn_context.continue_after_unknown_ident = true
				return
			}
			tc.register_synth_type(id, typ)
			return
		}
	}
	qname := tc.qualify_name(name)
	if qname != name {
		if typ := tc.file_scope.lookup(qname) {
			tc.register_synth_type(id, typ)
			return
		}
	}
	// The `err` of a block that handles an error, where the check of that block
	// kept no scope for it, as the check of an instance of a generic function.
	if name == 'err' && tc.has_ierror_interface() && tc.err_block_holds(id) {
		tc.register_synth_type(id, tc.parse_type(ierror_source_interface_name))
		return
	}
	if key := tc.shadowed_builtin_const_key(name) {
		tc.register_synth_type(id, tc.const_types[key] or { builtin_void_type })
		return
	}
	if typ := tc.const_types[qname] {
		if tc.bare_const_key_is_visible(qname) {
			tc.register_synth_type(id, typ)
			return
		}
	}
	// A module-local const shadows an unrelated bare global collected from
	// another module (notably `time.seconds_per_minute` vs a main global).
	if typ := tc.file_scope.lookup(name) {
		tc.register_synth_type(id, typ)
		return
	}
	if typ := tc.const_types[name] {
		if tc.bare_const_key_is_visible(name) {
			tc.register_synth_type(id, typ)
			return
		}
	}
	if key := tc.vsh_os_const_key(name) {
		tc.register_synth_type(id, tc.const_type_from_initializer(key, tc.const_types[key] or {
			builtin_void_type
		}))
		return
	}
	if key := tc.generic_fn_value_key(name) {
		if !tc.ident_is_call_callee_or_generic_base(id) && !tc.expr_is_direct_call_argument(id)
			&& tc.resolved_fn_value_name(id) == none {
			message := '`${name}` is a generic fn, you should pass its concrete types, e.g. ${name}[int]'
			tc.record_error_at(.unsupported_generic, message, id, tc.node_value_diagnostic_pos(id))
			if tc.direct_parent_kind(id) == .decl_assign {
				tc.record_error_at(.unsupported_generic, message, id, tc.node_value_diagnostic_pos(id))
			}
			tc.register_synth_type(id, tc.fn_type_from_key(key) or { builtin_void_type })
			return
		}
	}
	if typ := tc.fn_value_type(name) {
		if key := tc.ident_fn_value_key(name) {
			tc.remember_resolved_fn_value(id, key)
		}
		tc.register_synth_type(id, typ)
		return
	}
	if (name[0].is_capital() && tc.type_name_known(node.source_value_id()))
		|| (tc.generated_files.len > 0 && tc.type_name_known_in_current_module(node.source_value_id())) {
		tc.record_error_at(.assignment_mismatch, '`${name}` must be initialized', id, tc.node_value_diagnostic_pos(id))
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	if tc.selective_import_symbol_is_ambiguous(name) {
		tc.register_synth_type(id, unknown_type_quoting('ambiguous selective import', name))
		return
	}
	if name in tc.fn_ret_types || tc.qualify_fn_name(name) in tc.fn_ret_types {
		return
	}
	if tc.has_active_import(name) {
		return
	}
	// `qname` also keys the scope and const lookups above; probe the type tables once.
	qname_id := flat.source_type_id(qname)
	if qname_id in tc.structs || qname_id in tc.enum_names || tc.sum_types.contains(qname_id)
		|| qname_id in tc.interface_names {
		return
	}
	if typ := tc.enclosing_array_dsl_ident_type(id, name) {
		tc.register_synth_type(id, typ)
		return
	}
	if tc.should_diagnose(id) {
		if _ := tc.future_local_decl_id(name, id) {
			tc.record_error(.unknown_ident, 'undefined variable `${name}` (used before declaration)', id)
			if tc.fn_context.undefined_variable_context_depth > 0 {
				tc.record_error(.unknown_ident, 'unresolved variable: `${name}`', id)
			}
			return
		}
		parent_id := tc.direct_parent_id(id)
		if tc.valid_node_id(parent_id) && tc.a.node(parent_id).kind == .expr_stmt {
			message := if tc.fn_context.node_id >= 0 {
				'unexpected name `${name}`'
			} else {
				'`${name}` evaluated but not used'
			}
			tc.record_error(.unknown_ident, message, id)
			tc.register_synth_type(id, builtin_void_type)
			return
		}
		is_match_subject := tc.ident_is_match_subject(id)
		message := if tc.fn_context.undefined_variable_context_depth > 0 && !is_match_subject
			&& !name.starts_with('_') {
			'undefined variable: `${name}`'
		} else {
			'undefined ident: `${name}`'
		}
		tc.record_error_at(.unknown_ident, message, id,
			tc.checker_fixture_template_ident_pos(id, node.pos))
		tc.register_synth_type(id, unresolved_name_type())
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

// selector_is_call_callee reports whether selector `id` names the function of a call,
// like `x.f` in `x.f()`. Such a callee is a method, or a fn-typed field; like V1, the
// visibility of a fn-typed field is not checked when the field is called.
fn (tc &TypeChecker) selector_is_call_callee(id flat.NodeId) bool {
	parent_id := tc.direct_parent_id(id)
	if !tc.valid_node_id(parent_id) {
		return false
	}
	parent := tc.a.node(parent_id)
	return parent.kind == .call && parent.children_count > 0 && tc.a.child(parent, 0) == id
}

fn (tc &TypeChecker) future_local_decl_id(name string, use_id flat.NodeId) ?flat.NodeId {
	if name == '' || !tc.valid_node_id(use_id) {
		return none
	}
	// Only an identifier that some `decl_assign` declares can be found below.
	indexes := tc.check_decl_indexes
	if !isnil(indexes) && indexes.live && indexes.nodes_len == tc.a.nodes.len
		&& name !in indexes.decl_assign_names {
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
	if node.typ == qualify_source_type_invalid {
		tc.register_synth_type(id, builtin_void_type)
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
			tc.record_error(.unknown_type, '`res` can only be used in functions that returns something', id)
		}
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	if ret is ResultType {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_type, '`res` cannot be used in functions that returns a Result', id)
		}
		tc.register_synth_type(id, builtin_void_type)
		return
	}
	if msg := defer_result_index_error_message(ret, idx) {
		if tc.should_diagnose(id) {
			tc.record_error(.unknown_type, msg, id)
		}
		tc.register_synth_type(id, builtin_void_type)
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

fn (tc &TypeChecker) bare_name_is_const_owned_over_global(name string) bool {
	if tc.global_name_bloom.excludes(name, u64(tc.global_names.len)) || name.contains('.')
		|| name !in tc.global_names {
		return false
	}
	qname := tc.qualify_name(name)
	if qname !in tc.const_types {
		return false
	}
	// A local/param (function scope, parent != nil) out-ranks both; the global is root-scope.
	if owner := tc.cur_scope.lookup_owner(name) {
		if owner.scope != unsafe { nil } && owner.scope.parent != unsafe { nil } {
			return false
		}
	}
	return true
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

fn defer_result_index_error_message(ret &Type, idx int) ?string {
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
	if node.typ == qualify_source_type_invalid {
		return builtin_void_type
	}
	idx := defer_result_index(node) or { return none }
	ret := tc.fn_context.return_type
	if ret is Void || ret is ResultType {
		return builtin_void_type
	}
	if _ := defer_result_index_error_message(ret, idx) {
		return builtin_void_type
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
		&& (tc.is_untyped_float_literal_expr(id) || tc.integer_literal_source(id) != none) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .int_literal && clean_expected.is_integer() {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if tc.raw_string_literal_pointer_compatible(id, expected) {
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
	if node.kind == .or_expr && node.children_count >= 2 && node.value !in ['!', '?']
		&& expected !is Void && expected !is Unknown
		&& tc.a.nodes[int(tc.a.child(&node, 0))].kind == .match_stmt {
		// The `or` block of `match x {...} or {...}` unwraps `x`; the match is the value.
		actual := tc.resolve_expr(tc.a.child(&node, 0), expected)
		if tc.type_compatible(actual, expected) {
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
	}
	if node.kind == .or_expr && node.children_count >= 2
		&& (expected is OptionType || expected is ResultType) {
		payload := match expected {
			OptionType { *expected.base_type }
			ResultType { *expected.base_type }
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
	if node.kind == .infix && node.op in [.pipe, .amp, .xor] && node.children_count >= 2 {
		mut enum_expected := *unalias_type(expected)
		if payload := contextual_payload_type(expected) {
			enum_expected = unalias_type(payload)
		}
		if enum_expected is Enum && enum_expected.is_flag {
			lhs_id := tc.a.child(&node, 0)
			rhs_id := tc.a.child(&node, 1)
			lhs_type := tc.resolve_expr(lhs_id, enum_expected)
			rhs_type := tc.resolve_expr(rhs_id, enum_expected)
			if tc.type_compatible(lhs_type, enum_expected)
				&& tc.type_compatible(rhs_type, enum_expected) {
				tc.register_synth_type(id, enum_expected)
				return enum_expected
			}
		}
	}
	if node.kind == .enum_val {
		// The expected type may be the enum directly, or an option/result wrapper around
		// it (`?Enum` / `!Enum`), e.g. `mut field ?LoggingMode` assigned `field = .debug`.
		// Unwrap the option/result payload so the shorthand resolves against the inner
		// enum. On success the node is typed as the *inner* enum (not the wrapper): a bare
		// enum value assigned into an option is auto-wrapped by the assignment/return
		// machinery, exactly like any other bare value assigned into an option, so the
		// node itself must stay unwrapped for codegen to emit the wrap.
		mut enum_expected := *unalias_type(expected)
		if payload := contextual_payload_type(expected) {
			enum_expected = unalias_type(payload)
		}
		if enum_expected !is Enum {
			if enum_name_source_id := tc.resolve_enum_name(enum_expected.source_type_id()) {
				enum_expected = Type(Enum{
					name_id: enum_name_source_id
					is_flag: enum_name_source_id in tc.flag_enums
				})
			}
		}
		if enum_expected is Enum {
			if tc.enum_value_matches(node.value, enum_expected.source_type_id()) {
				tc.register_synth_type(id, enum_expected)
				return enum_expected
			}
			tc.type_mismatch(.assignment_mismatch, 'unknown enum field `${node.value}` for `${enum_expected.name()}`', id)
			return builtin_int_type
		}
	}
	// When several anonymous structs share the same field names, or a contextual type is
	// declared after the literal, the parser may leave or infer the literal too early.
	// Its call/assignment/return context supplies the exact anonymous struct here.
	if node.kind == .struct_init && is_contextual_anonymous_struct_literal(node.source_value_id())
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
		&& tc.bare_generic_literal_adopts(node.source_value_id(), expected)
		&& tc.generic_literal_fields_compatible(node, expected) {
		tc.register_synth_type(id, expected_raw)
		return expected_raw
	}
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 && expected is Pointer {
		child_id := tc.a.child(&node, 0)
		child := tc.a.nodes[int(child_id)]
		if child.kind == .struct_init && is_contextual_anonymous_struct_literal(child.source_value_id())
			&& tc.anonymous_struct_literal_compatible(child, expected.base_type) {
			tc.register_synth_type(child_id, expected.base_type)
			tc.register_synth_type(id, expected_raw)
			return expected_raw
		}
		if child.kind == .struct_init && tc.bare_generic_literal_adopts(child.source_value_id(), expected)
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
				*expected.base_type
			} else if expected is ResultType {
				*expected.base_type
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
				builtin_int_type
			}
			array_actual := Type(ArrayFixed{
				elem_type: tc.intern_type_reference(elem_type)
				len:       child.children_count
			})
			tc.register_synth_type(id, array_actual)
			return array_actual
		}
	}
	if node.kind == .array_literal && tc.explicit_array_literal_type_fits(node, expected) {
		mut elem_expected := builtin_void_type
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
	if node.kind == .map_init && tc.explicit_map_literal_type_fits(node, expected) {
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
						|| !tc.receiver_compatible(update_actual.value_type, expected_map.value_type)
						|| tc.container_elem_layouts_differ(update_actual.key_type, expected_map.key_type)
						|| tc.container_elem_layouts_differ(update_actual.value_type, expected_map.value_type) {
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
		if info := tc.resolve_call_info(id, node) {
			call_info := tc.specialized_plain_generic_call_info(node, info)
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
		if semantic_types_equal(actual, expected)
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

fn (tc &TypeChecker) raw_string_literal_pointer_compatible(id flat.NodeId, expected &Type) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .string_literal || !node.typ_text().starts_with('raw:') {
		return false
	}
	string_data_type := tc.struct_field_type(flat.source_type_string, 'str') or { return false }
	return tc.type_compatible(string_data_type, expected)
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
fn (tc &TypeChecker) fn_value_match_key(node flat.Node, expected &Type) ?string {
	key := tc.fn_value_key(node) or { return none }
	actual := tc.fn_type_from_key(key) or { return none }
	if tc.fn_value_signature_compatible(actual, expected) {
		return key
	}
	return none
}

fn (tc &TypeChecker) fn_value_signature_compatible(actual &Type, expected &Type) bool {
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

fn (tc &TypeChecker) c_integer_constant_context_type(id flat.NodeId) Type {
	if expected := tc.expected_context_for_expr(id) {
		if unalias_type(expected).is_integer() {
			return expected
		}
	}
	return builtin_int_type
}

fn c_upper_constant_is_pointer(qname string) bool {
	return qname == 'C.NULL' || qname == 'C.INVALID_HANDLE_VALUE' || qname == 'C.SIG_DFL'
		|| qname == 'C.SIG_ERR' || qname == 'C.SIG_IGN'
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
		return tc.fn_value_key(tc.a.node(tc.a.child(&node, 0)))
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
	// A namespace value can share a function's name; only call syntax selects
	// that function. Keep the stored value's type and identity for bare selectors.
	// A namespace is an identifier that names no value. Past that test, few selectors
	// name a global or a constant: look those up before deciding whether the base is
	// a namespace, which probes every kind of type and import.
	if base.kind == .ident && !tc.ident_resolves_to_value(base.value)
		&& (tc.global_type_for_selector(node) != none || tc.const_type_for_selector(node) != none)
		&& tc.is_namespace_selector(node, base) {
		return none
	}
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
		if base.value == tc.cur_module || base.value == short_name_view(tc.cur_module) {
			return tc.local_bare_fn_signature_key(node.value)
		}
		if key := tc.static_assoc_fn_key_for_base(base.source_value_id(), node.value) {
			return key
		}
		for type_id in tc.static_assoc_type_candidates(base.source_value_id()) {
			key := '${flat.source_type_name(type_id)}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
		}
		if key := tc.unbound_instance_method_key(base.source_value_id(), node.value) {
			return key
		}
		return none
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident {
			if tc.ident_resolves_to_value(inner.value) || inner.value in tc.const_types
				|| tc.qualify_name(inner.value) in tc.const_types {
				return none
			}
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			type_id := module_type_spelling_id(mod_name, base.value)
			if static_key := tc.static_assoc_fn_key_for_base(type_id, node.value) {
				return static_key
			}
			key := '${mod_name}.${base.value}.${node.value}'
			if tc.fn_signature_known(key) {
				return key
			}
			if instance_key := tc.unbound_instance_method_key(type_id, node.value) {
				return instance_key
			}
		}
	}
	return none
}

// ident_may_be_type_id is ident_may_be_type for a spelling ID.
fn (tc &TypeChecker) ident_may_be_type_id(name_id u32) bool {
	if name_id == 0 {
		return false
	}
	if flat.source_type_syntax(name_id).first_capital() {
		return true
	}
	if tc.generated_files.len == 0 || !tc.generated_files[tc.cur_file] {
		return false
	}
	// Scope bindings are keyed by their names.
	if _ := tc.non_file_scope_type(flat.source_type_name(name_id)) {
		return false
	}
	return tc.type_name_known(name_id)
}

fn (tc &TypeChecker) unbound_instance_method_key(type_name_id u32, method string) ?string {
	if !tc.ident_may_be_type_id(type_name_id) && !tc.type_symbol_known(type_name_id) {
		return none
	}
	receiver := tc.parse_type(type_name_id)
	for receiver_type in [*receiver, unalias_and_unwrap_pointer_type(receiver)] {
		for candidate in receiver_method_name_candidates(receiver_type, method, tc.cur_module) {
			if tc.fn_signature_known(candidate) && !tc.fn_key_is_static_associated(candidate) {
				return candidate
			}
		}
	}
	return none
}

fn (mut tc TypeChecker) check_unbound_instance_method_value(id flat.NodeId, node flat.Node) bool {
	if tc.ident_is_call_callee_or_generic_base(id) {
		return false
	}
	key := tc.selector_fn_value_key(node) or { return false }
	if tc.fn_key_is_static_associated(key) || !key.contains('.') || key.starts_with('C.') {
		return false
	}
	if tc.unbound_instance_method_key(flat.source_type_id(key.all_before_last('.')), node.value) == none {
		return false
	}
	if _ := tc.private_declaration(key) {
		tc.record_error_at(.unknown_field, 'method `${key}` is private', id, tc.node_value_diagnostic_pos(id))
	}
	typ := tc.fn_type_from_key(key) or { return false }
	tc.remember_resolved_fn_value(id, key)
	tc.register_synth_type(id, typ)
	return true
}

fn (tc &TypeChecker) static_assoc_fn_key_for_base(type_ident_id u32, method string) ?string {
	if method == '' {
		return none
	}
	// During checking the collected signatures are fixed. Most receiver calls
	// cannot name a static method at all; avoid allocating candidate type names.
	// Late registrations and post-check rewriting retain the general resolver.
	if !tc.resolution_type_mode && tc.static_associated_signature_count == tc.fn_ret_types.len
		&& !tc.static_associated_method_names[method] {
		return none
	}
	// The same test against the signatures a transform batch started with. Every key
	// probed below is a static name of `method`, so no listed signature can match.
	if tc.transform_static_method_count == tc.fn_ret_types.len
		&& !tc.transform_static_method_names[method] {
		return none
	}
	mut direct_candidates := []u32{}
	tc.add_static_assoc_type_candidate(mut direct_candidates, type_ident_id)
	tc.add_static_assoc_type_candidate(mut direct_candidates,
		tc.qualify_type_name(type_ident_id))
	for type_id in direct_candidates {
		key := flat.encode_static_type_method_name(flat.source_type_name(type_id), method)
		if tc.fn_signature_known(key) {
			return key
		}
	}
	for type_id in tc.static_assoc_type_candidates(type_ident_id) {
		key := flat.encode_static_type_method_name(flat.source_type_name(type_id), method)
		if tc.fn_signature_known(key) || key in tc.fn_ret_types {
			return key
		}
	}
	return none
}

// refresh_transform_static_method_names lists the methods of the static type method
// signatures registered so far, for the checkers forked from this one. Signatures are
// only added between two refreshes, so the list is current exactly while their count is
// unchanged; forget_transform_static_method_names covers a removal. The caller owns
// this checker: no other thread may be resolving through it.
pub fn (tc &TypeChecker) refresh_transform_static_method_names() {
	if tc.transform_static_method_count == tc.fn_ret_types.len {
		return
	}
	mut names := map[string]bool{}
	for name, _ in tc.fn_ret_types {
		if _, method := flat.decode_static_type_method_name(name) {
			names[method] = true
		}
	}
	mut owner := unsafe { tc }
	owner.transform_static_method_names = names.move()
	owner.transform_static_method_count = tc.fn_ret_types.len
}

// forget_transform_static_method_names drops the list: a signature was removed, or the
// arena holding the list is about to be released.
pub fn (tc &TypeChecker) forget_transform_static_method_names() {
	mut owner := unsafe { tc }
	owner.transform_static_method_names = map[string]bool{}
	owner.transform_static_method_count = -1
}

fn (tc &TypeChecker) fn_key_is_static_associated(key string) bool {
	return tc.static_associated_fn_keys[key]
}

fn (tc &TypeChecker) static_assoc_type_candidates(type_ident_id u32) []u32 {
	if type_ident_id == 0 {
		return []u32{}
	}
	// Import aliases are keyed by symbol strings.
	type_ident := flat.source_type_name(type_ident_id)
	mut candidates := []u32{}
	tc.add_static_assoc_type_candidate(mut candidates, type_ident_id)
	if resolved_id := tc.resolve_selective_import_type_symbol_id(type_ident_id) {
		tc.add_static_assoc_type_candidate(mut candidates, resolved_id)
	}
	if resolved := tc.resolve_import_alias(type_ident) {
		tc.add_static_assoc_type_candidate(mut candidates, flat.source_type_id(resolved))
	}
	tc.add_static_assoc_type_candidate(mut candidates, tc.qualify_type_name(type_ident_id))
	mut result := []u32{}
	for candidate_id in candidates {
		if tc.static_assoc_type_known(candidate_id) && candidate_id !in result {
			result << candidate_id
		}
	}
	return result
}

fn (tc &TypeChecker) add_static_assoc_type_candidate(mut candidates []u32, name_id u32) {
	clean_id := trimmed_source_type_id(name_id)
	if clean_id == 0 {
		return
	}
	if clean_id !in candidates {
		candidates << clean_id
	}
	clean_syntax := flat.source_type_syntax(clean_id)
	if clean_syntax.qualifier_prefixes().len >= 2 {
		declared_module_name := flat.source_type_syntax(clean_syntax.qualifier()).unqualified()
		short_qualified_id := flat.source_type_composition_two(.qualified, declared_module_name, clean_syntax.unqualified())
		if short_qualified_id !in candidates {
			candidates << short_qualified_id
		}
	}
	if canonical_id := tc.canonical_qualified_type_name(clean_id) {
		if canonical_id !in candidates {
			candidates << canonical_id
		}
	}
	if target_id := tc.type_aliases.type_id(clean_id) {
		if target_id !in candidates {
			candidates << target_id
		}
	}
	qname_id := tc.qualify_type_name(clean_id)
	if qname_id != clean_id && qname_id !in candidates {
		candidates << qname_id
	}
	if target_id := tc.type_aliases.type_id(qname_id) {
		if target_id !in candidates {
			candidates << target_id
		}
	}
}

fn (tc &TypeChecker) static_assoc_type_known(type_name_id u32) bool {
	return type_name_id in tc.structs || type_name_id in tc.enum_names || tc.sum_types.contains(type_name_id)
		|| type_name_id in tc.interface_names || tc.type_aliases.contains(type_name_id)
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

fn (tc &TypeChecker) expr_is_explicit_unsafe_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return false
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .block {
		return node.value == 'unsafe'
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return tc.expr_is_explicit_unsafe_value(tc.a.child(&node, 0))
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

// null_constant_expr_text returns how `id` spells a compile-time null pointer: `nil`,
// `0` or `voidptr(0)`, with or without parentheses around it. Anything else, including
// `unsafe { nil }`, yields an empty string.
fn (tc &TypeChecker) null_constant_expr_text(id flat.NodeId) string {
	value_id := tc.unwrap_paren_expr_id(id)
	if !tc.valid_node_id(value_id) {
		return ''
	}
	value := tc.a.node(value_id)
	if value.kind == .nil_literal {
		return 'nil'
	}
	if value.kind == .int_literal && numeric_literal_is_zero(value.value) {
		return '0'
	}
	if value.kind == .cast_expr && value.source_value_id() == flat.source_type_voidptr && value.children_count == 1
		&& tc.is_zero_literal(tc.unwrap_paren_expr_id(tc.a.child(value, 0))) {
		return 'voidptr(0)'
	}
	return ''
}

// record_null_constant_reference_error rejects a compile-time null constant that is
// converted implicitly to the non-option reference `expected` outside `unsafe`, like the
// explicit cast `&T(0)` already is. `unsafe { nil }` stays the way to write a null
// reference. `voidptr` targets, `.c.v` files and translated files are left alone, as
// they are by the cast rule.
fn (mut tc TypeChecker) record_null_constant_reference_error(kind TypeErrorKind, expr_id flat.NodeId, expected Type) bool {
	if tc.unsafe_depth > 0 || unalias_type(expected) !is Pointer {
		return false
	}
	text := tc.null_constant_expr_text(expr_id)
	if text.len == 0 {
		return false
	}
	if fn_param_is_voidptr_type(expected) || tc.translated_files[tc.cur_file]
		|| tc.cur_file.ends_with('.c.v') || tc.expr_is_inside_unsafe_block(expr_id) {
		return false
	}
	msg := if text == 'nil' {
		'`nil` is only allowed in `unsafe` code'
	} else {
		'cannot use `${text}` as `${call_argument_type_name(expected)}` outside `unsafe`, use `unsafe { nil }`'
	}
	tc.record_error_at(kind, msg, expr_id, tc.a.node(expr_id).pos)
	return true
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
	return Type(tc.variadic_fn_type(params, tc.intern_type_reference(ret), (tc.declaration_param_mutability[key] or { []bool{} }), tc.fn_variadic[key] or { false }))
}

// fn_value_decl_key returns the declaration key of a named function used as a value.
fn (tc &TypeChecker) fn_value_decl_key(expr flat.Node) ?string {
	match expr.kind {
		.ident {
			if tc.ident_resolves_to_value(expr.value) {
				return none
			}
			if local_name := tc.local_bare_fn_key(expr.value) {
				return local_name
			}
			if imported_name := tc.resolve_selective_import_symbol(expr.value) {
				return imported_name
			}
			if expr.value in tc.fn_ret_types {
				return expr.value
			}
		}
		.selector {
			if expr.children_count == 0 {
				return none
			}
			base := tc.a.child_node(&expr, 0)
			if base.kind != .ident || tc.ident_resolves_to_value(base.value) {
				return none
			}
			mod_name := tc.resolve_import_alias(base.value) or { base.value }
			key := '${mod_name}.${expr.value}'
			if key in tc.fn_ret_types {
				return key
			}
		}
		else {}
	}
	return none
}

// fn_decl_value_mut_ref_slot_compatible accepts a named function where an expected
// `fn (mut &T)` type spells out the `&&T` slot of its explicit `mut x &T` parameters.
// The declaration's own value type keeps `&T`, so that it still adapts to `fn (mut T)`.
fn (tc &TypeChecker) fn_decl_value_mut_ref_slot_compatible(expr_id flat.NodeId, actual &Type, expected &Type) bool {
	if fn_type_from_type(expected) == none {
		return false
	}
	actual_fn := fn_type_from_type(actual) or { return false }
	key := tc.fn_value_decl_key(tc.a.node(expr_id)) or { return false }
	mut slots := actual_fn.params.map(*it.typ)
	mut has_slot := false
	for i, parameter in actual_fn.params {
		typ := parameter.typ
		if parameter.is_mut && typ is Pointer
			&& tc.call_param_requires_mut_pointer_slot(CallInfo{ name: key }, i) {
			slots[i] = Type(Pointer{
				base_type: tc.intern_type_reference(typ)
			})
			has_slot = true
		}
	}
	if !has_slot {
		return false
	}
	mutability := actual_fn.params.map(it.is_mut)
	signature := tc.variadic_fn_type(slots, actual_fn.return_type, mutability, actual_fn.is_variadic)
	return tc.type_compatible(Type(signature), expected)
}

fn (tc &TypeChecker) translated_c_string_fixed_array_compatible(id flat.NodeId, expected &Type) bool {
	if !tc.translated_files[tc.cur_file] && !tc.node_is_in_translated_file(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .char_literal || !node.value.starts_with('c:') {
		return false
	}
	if expected is Alias && unalias_type(expected.base_type) is OptionType {
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
	mut pointer_type := *clean_expected
	if expected is OptionType {
		if expected.base_type !is Pointer {
			return false
		}
		pointer_type = expected.base_type
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

// struct_field_c_abi_fn_ptr_type returns a field C ABI encoding using its owner spelling ID.
pub fn (tc &TypeChecker) struct_field_c_abi_fn_ptr_type(struct_name_id u32, field_name string) ?string {
	if typ := tc.struct_field_c_abi_fns[struct_name_id][field_name] {
		return typ
	}
	return none
}

// enum_value_matches supports enum value matches handling for TypeChecker.
fn (tc &TypeChecker) enum_value_matches(value string, enum_name_id u32) bool {
	if value.starts_with('.') {
		return tc.enum_has_field(enum_name_id, value[1..])
	}
	if value.contains('.') {
		prefix := value.all_before_last('.')
		field := value.all_after_last('.')
		prefix_id := flat.source_type_id(prefix)
		if prefix_id != enum_name_id && flat.source_type_syntax(prefix_id).unqualified() !=
			flat.source_type_syntax(enum_name_id).unqualified() {
			return false
		}
		return tc.enum_has_field(enum_name_id, field)
	}
	return tc.enum_has_field(enum_name_id, value)
}

// enum_has_field converts enum has field data for types.
fn (tc &TypeChecker) enum_has_field(enum_name_id u32, field string) bool {
	if _ := tc.enum_field_name(enum_name_id, field) {
		return true
	}
	return false
}

fn (tc &TypeChecker) enum_field_name(enum_name_id u32, field string) ?string {
	fields := tc.enum_fields[enum_name_id] or { return none }
	if field.starts_with('@') && !token.Token.from_string_tinyv(field[1..]).is_keyword() {
		return none
	}
	if tc.enum_fields_contain(enum_name_id, fields, field) {
		return field
	}
	plain := escaped_identifier_name(field)
	if token.Token.from_string_tinyv(plain).is_keyword() {
		alternate := if field.starts_with('@') { plain } else { '@' + field }
		if tc.enum_fields_contain(enum_name_id, fields, alternate) {
			return alternate
		}
	}
	return none
}

// enum_member_is_callable reports whether `Enum.member` names a function rather
// than a value: a static one declared for the enum (`fn Color.first()`) or a
// method passed as a value (`colors.map(Color.label)`).
fn (tc &TypeChecker) enum_member_is_callable(enum_name_id u32, base_id u32, member string) bool {
	if _ := tc.static_assoc_fn_key_for_base(base_id, member) {
		return true
	}
	enum_type := Type(Enum{
		name_id: enum_name_id
		is_flag: enum_name_id in tc.flag_enums
	})
	for mname in receiver_method_name_candidates(enum_type, member, tc.cur_module) {
		if mname in tc.fn_ret_types {
			return true
		}
	}
	return false
}

// resolve_enum_name resolves resolve enum name information for types.
fn (tc &TypeChecker) resolve_enum_name(name_id u32) ?u32 {
	qname_id := tc.qualify_type_name(name_id)
	if qname_id in tc.enum_names {
		return qname_id
	}
	// A module's enum or alias shadows a same-named program-module enum.
	if qname_id != name_id {
		if target_source_id := tc.resolve_enum_alias_target(qname_id) {
			return target_source_id
		}
	}
	if name_id in tc.enum_names {
		return name_id
	}
	for tail_id in flat.source_type_syntax(name_id).qualifier_suffixes() {
		if !flat.source_type_shape(tail_id).has(.dotted) {
			break
		}
		if tail_id in tc.enum_names {
			return tail_id
		}
	}
	if !flat.source_type_shape(name_id).has(.dotted) {
		if resolved_id := tc.resolve_selective_import_type_symbol_id(name_id) {
			if resolved_id in tc.enum_names || resolved_id in tc.flag_enums {
				return resolved_id
			}
			if target_source_id := tc.resolve_enum_alias_target(resolved_id) {
				return target_source_id
			}
		}
	}
	if target_source_id := tc.resolve_enum_alias_target(name_id) {
		return target_source_id
	}
	if target_source_id := tc.resolve_enum_alias_target(qname_id) {
		return target_source_id
	}
	return none
}

fn (tc &TypeChecker) resolve_enum_alias_target(name_id u32) ?u32 {
	mut cur := name_id
	for _ in 0 .. 16 {
		target := tc.alias_target_type_text(cur) or { return none }
		if target == cur { return none }
		if target in tc.enum_names || target in tc.flag_enums { return target }
		cur = target
	}
	return none
}

// enum_selector_type supports enum selector type handling for TypeChecker.
fn (tc &TypeChecker) enum_selector_type(node &flat.Node) ?Type {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	return tc.enum_selector_type_from_base(tc.a.child(node, 0), node.value)
}

fn (tc &TypeChecker) enum_selector_type_from_base(base_id flat.NodeId, field string) ?Type {
	base := tc.a.node(base_id)
	mut enum_name := u32(0)
	if base.kind == .ident {
		if !tc.ident_may_be_type(base.value) {
			return none
		}
		enum_name = (tc.resolve_enum_name(base.source_value_id()) or { u32(0) })
	} else if base.kind == .selector && base.children_count > 0 {
		inner := tc.a.child_node(base, 0)
		if inner.kind == .ident && tc.module_member_may_be_type(inner.value, base.value) {
			mod_name := tc.resolve_import_alias(inner.value) or { inner.value }
			enum_name = (tc.resolve_enum_name(module_type_spelling_id(mod_name, base.value)) or {
				u32(0)
			})
		}
	}
	if enum_name == 0 || !tc.enum_has_field(enum_name, field) {
		return none
	}
	return Type(Enum{
		name_id: enum_name
		is_flag: enum_name in tc.flag_enums
	})
}

// type_compatible returns type compatible data for TypeChecker.
// slot_value_compatible exposes the checker's assignment-compatibility rule to
// the transform. The transform lowers value slots long after checking, and a
// generic specialization is the one case where it sees concrete types the
// checker never judged, so it has to ask the same question the checker would.
pub fn (tc &TypeChecker) slot_value_compatible(actual &Type, expected &Type) bool {
	return tc.type_compatible(actual, expected)
}

@[inline]
fn (tc &TypeChecker) type_compatible(actual &Type, expected &Type) bool {
	// Equal semantic values have equal spellings even when their immutable
	// payloads were allocated separately. Compare them before interning names.
	if semantic_types_equal(actual, expected) {
		return true
	}
	return tc.type_compatible_unequal(actual, expected)
}

fn (tc &TypeChecker) type_compatible_unequal(actual &Type, expected &Type) bool {
	actual_raw := actual
	expected_raw := expected
	// One memoized spelling per side: the former per-comparison name() calls
	// composed fresh strings up to eight times per invocation (1.3M+ calls per
	// self-host build), and equal compound types now compare via the shared
	// memo instance's pointer fast path.
	actual_name_comparison_type_id := tc.type_name(actual)
	expected_name_source_id := tc.type_name(expected)
	expected_name_comparison_type_id := expected_name_source_id
	if actual_name_comparison_type_id == expected_name_comparison_type_id {
		return true
	}
	if flat.source_type_shape(actual_name_comparison_type_id).has(.dotted)
		&& flat.source_type_shape(expected_name_comparison_type_id).has(.dotted) {
		actual_syntax := flat.source_type_syntax(actual_name_comparison_type_id)
		expected_syntax := flat.source_type_syntax(expected_name_comparison_type_id)
		if actual_syntax.unqualified() == expected_syntax.unqualified()
			&& actual_syntax.qualified_modifiers() == expected_syntax.qualified_modifiers() {
			actual_prefix := flat.source_type_name(actual_syntax.qualifier())
			expected_prefix := flat.source_type_name(expected_syntax.qualifier())
			actual_module := tc.resolve_import_alias(actual_prefix) or { actual_prefix }
			expected_module := tc.resolve_import_alias(expected_prefix) or { expected_prefix }
			if actual_module == expected_module {
				return true
			}
			if actual_canonical_source_id := tc.canonical_qualified_type_name(actual_name_comparison_type_id) {
				actual_canonical_comparison_type_id := actual_canonical_source_id

				if expected_canonical_source_id := tc.canonical_qualified_type_name(expected_name_comparison_type_id) {
					expected_canonical_comparison_type_id := expected_canonical_source_id

					if actual_canonical_comparison_type_id == expected_canonical_comparison_type_id {
						return true
					}
				}
			}
		}
	}
	if thread_handle_type_ids_match(actual_name_comparison_type_id, expected_name_comparison_type_id) {
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
		return tc.fn_value_signature_compatible(actual_raw, expected_raw)
	}
	if (flat.source_type_shape(actual_name_comparison_type_id).has(.bracketed)
		|| flat.source_type_shape(expected_name_comparison_type_id).has(.bracketed))
		&& tc.generic_type_name_matches(actual_name_comparison_type_id, expected_name_comparison_type_id) {
		return true
	}
	if tc.sum_variant_type_for_pattern(expected_name_comparison_type_id, actual_name_comparison_type_id) != none {
		return true
	}
	if actual is Alias && expected is Alias {
		return tc.type_compatible(actual.base_type, expected.base_type)
	}
	if actual is Alias {
		if expected is Interface && tc.type_implements_interface(actual_raw, expected) {
			return true
		}
		return tc.type_compatible(actual.base_type, expected_raw)
	}
	if expected is Alias {
		if tc.alias_type_is_shared(expected) && actual is Pointer {
			expected_shared := if expected.base_type is Pointer {
				*expected.base_type.base_type
			} else {
				*expected.base_type
			}
			if tc.type_compatible(actual.base_type, expected_shared) {
				return true
			}
		}
		return tc.type_compatible(actual_raw, expected.base_type)
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
		if is_ierror_type(actual) && unalias_type(expected.base_type) is String {
			return false
		}
		if actual is OptionType {
			actual_base := option_alias_payload_type(actual.base_type)
			expected_base := option_alias_payload_type(expected.base_type)
			return tc.type_compatible(actual_base, expected_base)
		}
		return tc.type_compatible(actual_raw, expected.base_type)
	}
	if expected is ResultType {
		if actual is ResultType {
			return tc.type_compatible(actual.base_type, expected.base_type)
		}
		return tc.type_compatible(actual_raw, expected.base_type)
	}
	if expected_name_source_id in tc.interface_names {
		return tc.type_implements_interface(actual_raw, Interface{
			name_id: expected_name_source_id
		})
	}
	if expected is SumType {
		return tc.type_matches_sum(actual_raw, expected_raw)
	}
	if expected is Interface {
		return tc.type_implements_interface(actual_raw, expected)
	}
	if expected is Enum && expected.is_flag && actual is Primitive && actual.props.has(.integer) {
		return true
	}
	if actual is Interface {
		if expected is Interface {
			return tc.interface_implements_interface(actual.source_type_id(), expected.source_type_id())
		}
		return false
	}
	if actual_raw.is_integer() && expected_raw.is_integer() {
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
		if expected.props.has(.integer) && actual_raw.is_integer() {
			return true
		}
	}
	if actual is Primitive && actual.props.has(.integer) && expected_raw.is_integer() {
		return true
	}
	if expected is String {
		return actual is String || is_ierror_type(actual)
	}
	if expected is Char {
		return actual is Char || actual.source_type_id() == flat.source_type_u8
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
			expected_base_name := expected.base_type.source_type_id()
			if expected_base_name in tc.interface_names && tc.type_implements_interface(actual.base_type, Interface{
				name_id: expected.base_type.source_type_id()
			}) {
				return true
			}
			if expected.base_type is Void || actual.base_type is Void {
				return true
			}
			expected_base := unalias_type(expected.base_type)
			if expected_base is SumType {
				actual_base := unalias_type(actual.base_type)
				return semantic_types_equal(actual_base, expected_base)
			}
			// C interop: `&char` and `&u8` share representation (`tos_clone(C.strdup(s))`).
			if (actual.base_type is Char && expected.base_type.source_type_id() == flat.source_type_u8)
				|| (actual.base_type.source_type_id() == flat.source_type_u8 && expected.base_type is Char) {
				return true
			}
			if !tc.type_compatible(actual.base_type, expected.base_type) {
				return false
			}
			// A referenced array or map is shared, never converted: a `&[]Item` is not
			// a `&[]Named`, and a `&[]u8` is not a `&[]u64`.
			if is_container_type(actual.base_type) && is_container_type(expected.base_type) {
				return !tc.in_place_container_layouts_differ(actual.base_type, expected.base_type)
			}
			// A referenced number is read in place, never converted either: a `&u8` is
			// not a `&u64`, and a `&u64` is not a `&f64`.
			if tc.pointee_number_storage_checked()
				&& pointee_number_storage_differs(actual.base_type, expected.base_type) {
				return false
			}
			return true
		}
	}
	if expected is Array {
		if actual is Array {
			return tc.dynamic_array_elem_types_compatible(actual.elem_type, expected.elem_type)
		}
		if actual is ArrayFixed {
			return tc.dynamic_array_elem_types_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is ArrayFixed {
		if actual is ArrayFixed {
			return tc.fixed_array_lengths_compatible(actual, expected)
				&& tc.container_elem_types_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Channel {
		if actual is Channel {
			// A channel is shared, never converted, so its elements must be stored the
			// same way: a `chan u8` is not a `chan u64`, and a `chan [2]int` is not a
			// `chan []int`.
			return actual.is_mut == expected.is_mut
				&& tc.container_elem_types_compatible(actual.elem_type, expected.elem_type)
		}
	}
	if expected is Map {
		if actual is Map {
			return tc.container_elem_types_compatible(actual.key_type, expected.key_type)
				&& tc.container_elem_types_compatible(actual.value_type, expected.value_type)
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

// explicit_map_literal_type_fits reports whether the map literal `node` may take
// the `expected` map type. An untyped `{...}` literal may; an explicitly typed
// `map[K]V{...}` keeps its own type unless that is the expected one (up to aliases).
fn (tc &TypeChecker) explicit_map_literal_type_fits(node flat.Node, expected Type) bool {
	if node.source_value_id() == 0 || map_type_from_receiver(expected) == none {
		return true
	}
	return tc.type_compatible(tc.parse_type(node.source_value_id()), expected)
}

// explicit_array_literal_type_fits reports whether the array literal `node` may take
// the `expected` array type. An untyped `[...]` literal may; an explicitly typed
// `[N]T[...]` keeps its own type unless that is the expected one (up to aliases).
fn (tc &TypeChecker) explicit_array_literal_type_fits(node flat.Node, expected Type) bool {
	if node.typ == 0 || (expected !is Array && expected !is ArrayFixed) {
		return true
	}
	return tc.type_compatible(tc.parse_type(node.typ), expected)
}

// container_elem_types_compatible reports whether a fixed array with element type
// `actual` (or a map with key or value type `actual`, or a channel with element type
// `actual`) can be used where one with `expected` is required.
fn (tc &TypeChecker) container_elem_types_compatible(actual Type, expected Type) bool {
	return tc.type_compatible(actual, expected) && !tc.container_elem_layouts_differ(actual, expected)
}

// dynamic_array_elem_types_compatible reports whether a dynamic array with element
// type `actual` can be used where one with `expected` is required. The elements must
// have the same layout, except that concrete values may be boxed into the interface or
// sum type they implement: the transform converts such an array element by element
// (`transform_array_value_for_dynamic_target`), also inside nested dynamic arrays,
// whose elements the recursive type_compatible call has already checked this way.
fn (tc &TypeChecker) dynamic_array_elem_types_compatible(actual Type, expected Type) bool {
	if !tc.type_compatible(actual, expected) {
		return false
	}
	clean_actual := unalias_type(actual)
	clean_expected := unalias_type(expected)
	if clean_expected is Interface || clean_expected is SumType
		|| (clean_expected is Array && clean_actual is Array) {
		return true
	}
	return !tc.container_elem_layouts_differ(actual, expected)
}

// pointee_number_storage_checked reports whether numbers behind pointers must be stored
// the same way (pointee_number_storage_differs). C calls (c_call_arg_compatible), `unsafe`
// blocks and translated C code skip this check and keep accepting the number pointees
// they accepted before.
fn (tc &TypeChecker) pointee_number_storage_checked() bool {
	return tc.unsafe_depth == 0 && !tc.translated_files[tc.cur_file]
}

// pointee_number_storage_differs reports whether the numbers `actual` and `expected`,
// read through a pointer, are stored differently: with another size, or as an integer
// and a float. `int`, `isize` and `usize` have the size of a machine word, which is
// not that of any sized integer on every target. The sign does not count, so `&char`,
// `&u8` and `&i8` stay interchangeable.
fn pointee_number_storage_differs(actual Type, expected Type) bool {
	actual_storage := number_storage_class(actual)
	expected_storage := number_storage_class(expected)
	return actual_storage != 0 && expected_storage != 0 && actual_storage != expected_storage
}

// number_storage_class returns how a number type is stored: its size in bits, plus one
// for a float, or 1 for a word sized integer. Other types return 0.
fn number_storage_class(typ Type) int {
	clean := unalias_type(typ)
	match clean {
		Primitive {
			if clean.props.has(.float) {
				return int(clean.size) + 1
			}
			if clean.props.has(.integer) {
				return if clean.size == 0 { 1 } else { int(clean.size) }
			}
		}
		Rune {
			return 32
		}
		Char {
			return 8
		}
		ISize, USize {
			return 1
		}
		else {}
	}
	return 0
}

fn is_container_type(typ Type) bool {
	clean := unalias_type(typ)
	return clean is Array || clean is ArrayFixed || clean is Map
}

// mut_arg_container_layouts_differ reports whether the array or map `actual`, passed
// as `mut` or `shared`, stores its elements differently from the container behind the
// parameter type `expected`. The callee works on the caller's container, so it cannot
// be converted element by element like a value argument: a `[]Item` is not a
// `mut []Named` (or `mut ?[]Named`, or `shared []Named`). The same holds for the
// payload of an option: a `?[2]int` is not a `mut ?[]int`, and a `?Item` is not a
// `mut ?Named`.
fn (tc &TypeChecker) mut_arg_container_layouts_differ(actual Type, expected Type) bool {
	mut clean_actual := unalias_type(actual)
	if clean_actual is Pointer {
		clean_actual = unalias_type(clean_actual.base_type)
	}
	mut clean_expected := unalias_type(expected)
	if clean_expected is Pointer {
		clean_expected = unalias_type(clean_expected.base_type)
	}
	if clean_expected is OptionType && clean_actual is OptionType {
		actual_payload := unalias_type(clean_actual.base_type)
		mut expected_payload := unalias_type(clean_expected.base_type)
		// A `mut ?T` parameter is a `?&T` slot.
		if expected_payload is Pointer && actual_payload !is Pointer {
			expected_payload = unalias_type(expected_payload.base_type)
		}
		return tc.container_elem_layouts_differ(actual_payload, expected_payload)
	}
	if clean_expected is OptionType && clean_actual !is OptionType {
		clean_expected = unalias_type(clean_expected.base_type)
		if clean_expected is Pointer {
			clean_expected = unalias_type(clean_expected.base_type)
		}
	}
	if !is_container_type(clean_actual) || !is_container_type(clean_expected) {
		return false
	}
	return tc.in_place_container_layouts_differ(clean_actual, clean_expected)
}

// in_place_container_layouts_differ reports whether the array or map `actual`, used in
// place (behind a reference, or as a `mut` or `shared` argument), stores its elements
// differently from the one `expected`. A fixed array used as a dynamic array is viewed
// in place: it keeps its elements as they are, so only they have to match.
fn (tc &TypeChecker) in_place_container_layouts_differ(actual Type, expected Type) bool {
	clean_actual := unalias_type(actual)
	clean_expected := unalias_type(expected)
	if clean_actual is ArrayFixed && clean_expected is Array {
		return tc.container_elem_layouts_differ(clean_actual.elem_type, clean_expected.elem_type)
	}
	return tc.container_elem_layouts_differ(clean_actual, clean_expected)
}

// container_elem_layouts_differ reports whether otherwise compatible array element,
// map key or map value types store their elements differently. A container is not
// converted element by element, so unlike plain values the types must match: aliases
// are transparent, but `u32` is not a `u64`, `int` is not a `?int` or a flag enum, a
// fixed array is not a dynamic one, and a concrete type (or a sum type) is not the
// interface or sum type that would box it.
fn (tc &TypeChecker) container_elem_layouts_differ(actual Type, expected Type) bool {
	return tc.container_elem_layouts_differ_with_fn_scalar_abi(actual, expected, false)
}

// Function values may share scalar representations such as rune/u32; ordinary
// container assignment still requires the original element identity.
fn (tc &TypeChecker) container_elem_layouts_differ_with_fn_scalar_abi(actual Type, expected Type, allow_fn_scalar_abi bool) bool {
	clean_actual := unalias_type(actual)
	clean_expected := unalias_type(expected)
	if type_contains_unknown(clean_actual) || type_contains_unknown(clean_expected) {
		return false
	}
	if (clean_actual.is_integer() || clean_actual.is_float())
		&& (clean_expected.is_integer() || clean_expected.is_float()) {
		if allow_fn_scalar_abi && tc.fn_param_compatible(clean_actual, clean_expected) {
			return false
		}
		return clean_actual.source_type_id() != clean_expected.source_type_id()
	}
	if (clean_actual is Enum) != (clean_expected is Enum) {
		return true
	}
	match clean_expected {
		OptionType {
			if clean_actual is OptionType {
				return tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.base_type, clean_expected.base_type, allow_fn_scalar_abi)
			}
			return true
		}
		ResultType {
			if clean_actual is ResultType {
				return tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.base_type, clean_expected.base_type, allow_fn_scalar_abi)
			}
			return true
		}
		Array {
			if clean_actual is Array {
				return tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.elem_type, clean_expected.elem_type, allow_fn_scalar_abi)
			}
			// A fixed array stores its elements in place, a dynamic one behind a pointer.
			return clean_actual is ArrayFixed
		}
		ArrayFixed {
			if clean_actual is ArrayFixed {
				return tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.elem_type, clean_expected.elem_type, allow_fn_scalar_abi)
			}
			return clean_actual is Array
		}
		Map {
			if clean_actual is Map {
				return tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.key_type, clean_expected.key_type, allow_fn_scalar_abi)
					|| tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.value_type, clean_expected.value_type, allow_fn_scalar_abi)
			}
		}
		Pointer {
			if clean_actual is Pointer && clean_actual.base_type !is Void
				&& clean_expected.base_type !is Void {
				return tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.base_type, clean_expected.base_type, allow_fn_scalar_abi)
			}
		}
		Channel {
			if clean_actual is Channel {
				return tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual.elem_type, clean_expected.elem_type, allow_fn_scalar_abi)
			}
		}
		Interface {
			return clean_actual !is Interface
		}
		SumType {
			// A sum type that is a variant of another one (`Inner` in `Outer`) is
			// boxed into it, not stored as it is.
			if clean_actual is SumType {
				return !tc.type_compatible(clean_expected, clean_actual)
			}
			return true
		}
		else {}
	}
	return false
}

fn option_alias_payload_type(typ Type) Type {
	if typ is Alias {
		clean := unalias_type(typ)
		if clean is OptionType {
			return *clean.base_type
		}
	}
	return typ
}

fn (tc &TypeChecker) alias_type_is_shared(alias Alias) bool {
	mut name_id := alias.source_type_id()
	for _ in 0 .. 16 {
		mut target_id := tc.type_aliases.type_id(name_id) or { u32(0) }
		if target_id == 0 && !flat.source_type_shape(name_id).has(.dotted) {
			target_id = tc.type_aliases.value_id(tc.qualify_type_name(name_id))
		}
		target_id = trimmed_source_type_id(target_id)
		if flat.source_type_shape(target_id).has(.shared_type) {
			return true
		}
		if target_id == 0 || target_id == name_id || !flat.source_type_syntax(target_id).plain_alias_target() {
			return false
		}
		name_id = target_id
	}
	return false
}

fn thread_handle_type_ids_match(actual_id u32, expected_id u32) bool {
	if !source_type_has_thread_prefix(actual_id) || !source_type_has_thread_prefix(expected_id) {
		return false
	}
	return canonical_thread_handle_type_id(actual_id) == canonical_thread_handle_type_id(expected_id)
}

fn source_type_has_thread_prefix(type_id u32) bool {
	// The original grammar predicate also accepts spellings such as `thread_name`.
	return flat.source_type_query(type_id).has(.thread_prefix)
}

fn canonical_thread_handle_type_id(type_id u32) u32 {
	clean_id := trimmed_source_type_id(type_id)
	if clean_id == checker_tail_source_thread_void {
		return checker_source_thread
	}
	if clean_id == checker_tail_source_thread_result_marker {
		return checker_tail_source_thread_result_void
	}
	if clean_id == checker_tail_source_thread_option_marker {
		return checker_tail_source_thread_option_void
	}
	return clean_id
}

@[inline]
fn type_contains_unknown(typ &Type) bool {
	// These dominate checked expression queries and cannot contain another type.
	match typ {
		Primitive, Struct, Void, String, Char, Rune, ISize, USize, Nil, None, Interface, Enum, SumType {
			return false
		}
		else { return type_contains_unknown_nested(typ) }
	}
}

@[direct_array_access]
fn type_contains_unknown_nested(typ &Type) bool {
	match typ {
		Unknown { return true }
		Alias { return type_contains_unknown(typ.base_type) }
		Array { return type_contains_unknown(typ.elem_type) }
		ArrayFixed { return type_contains_unknown(typ.elem_type) }
		Map { return type_contains_unknown(typ.key_type) || type_contains_unknown(typ.value_type) }
		Channel { return type_contains_unknown(typ.elem_type) }
		Pointer { return type_contains_unknown(typ.base_type) }
		OptionType { return type_contains_unknown(typ.base_type) }
		ResultType { return type_contains_unknown(typ.base_type) }
		FnType {
			for i in 0 .. typ.params.len {
				if type_contains_unknown(typ.params[i].typ) { return true }
			}
			return type_contains_unknown(typ.return_type)
		}
		MultiReturn {
			for i in 0 .. typ.types.len {
				if type_contains_unknown(&typ.types[i]) { return true }
			}
			return false
		}
		else { return false }
	}
}

// fn_param_compatible compares parameter representations, preserving source-level integer distinctions.
pub fn (tc &TypeChecker) fn_param_compatible(actual &Type, expected &Type) bool {
	if actual is Unknown || expected is Unknown {
		return false
	}
	if semantic_types_equal(actual, expected) {
		return true
	}
	// V's platform `int` and a fixed-width integer such as `i64`/`i32` may share an
	// emitted C spelling on a given target, but they remain distinct, target-
	// independent source types. Function-parameter identity must not depend on the C
	// width, so never let the c_type shortcut below collapse `int` with `i64`/`i32`
	// (on 64-bit both spell `i64`; on 32-bit `int`/`i32` both spell `i32`).
	if fn_param_is_platform_int(actual) != fn_param_is_platform_int(expected)
		&& unalias_type(actual) is Primitive
		&& unalias_type(expected) is Primitive {
		return false
	}
	if tc.fn_value_layouts_differ(actual, expected) {
		return false
	}
	if tc.c_type(actual) == tc.c_type(expected) {
		return true
	}
	return fn_param_can_cast_userdata_param(actual, expected)
}

// fn_value_layouts_differ reports whether a parameter or result type `actual` of a
// function value stores its value differently from the `expected` one, although they may
// have one C type: all options and results share one, as do all dynamic arrays, all maps
// and all channels. A function value is called as it is, so nothing converts its
// arguments or its result: a `fn () ?[2]int` is not a `fn () ?[]int`, a `fn ([]u64)` is
// not a `fn ([]u8)`, and a `fn () []Item` is not a `fn () []Named`.
fn (tc &TypeChecker) fn_value_layouts_differ(actual &Type, expected &Type) bool {
	if type_contains_unknown(actual) || type_contains_unknown(expected) {
		return false
	}
	mut clean_actual := unalias_type(actual)
	mut clean_expected := unalias_type(expected)
	if clean_actual is Pointer && clean_expected is Pointer {
		clean_actual = unalias_type(clean_actual.base_type)
		clean_expected = unalias_type(clean_expected.base_type)
	}
	shared_c_type := (clean_actual is OptionType && clean_expected is OptionType)
		|| (clean_actual is ResultType && clean_expected is ResultType)
		|| (clean_actual is Channel && clean_expected is Channel)
		|| (is_container_type(clean_actual) && is_container_type(clean_expected))
	return shared_c_type && tc.container_elem_layouts_differ_with_fn_scalar_abi(clean_actual, clean_expected, true)
}

// fn_value_signature_layouts_differ reports whether a parameter or the result of the
// function value type `actual` is stored differently from the one of `expected`
// (fn_value_layouts_differ).
fn (tc &TypeChecker) fn_value_signature_layouts_differ(actual FnType, expected FnType) bool {
	if actual.params.len != expected.params.len {
		return false
	}
	for i in 0 .. actual.params.len {
		if tc.fn_value_layouts_differ(fn_compatible_param_type(actual, i), fn_compatible_param_type(expected,
			i)) {
			return true
		}
	}
	return tc.fn_value_layouts_differ(actual.return_type, expected.return_type)
}

fn fn_param_is_platform_int(typ &Type) bool {
	clean := unalias_type(typ)
	if clean is Primitive {
		return clean.size == 0 && clean.props.has(.integer) && !clean.props.has(.unsigned)
	}
	return false
}

fn (tc &TypeChecker) fn_return_compatible(actual &Type, expected &Type) bool {
	if semantic_types_equal(actual, expected) {
		return true
	}
	if tc.type_name(actual) == tc.type_name(expected) {
		return true
	}
	if fn_param_is_voidptr_type(expected) && fn_param_is_nonvoid_pointer_type(actual) {
		return true
	}
	if tc.fn_value_layouts_differ(actual, expected) {
		return false
	}
	if tc.c_type(actual) == tc.c_type(expected) && tc.type_compatible(actual, expected) {
		return true
	}
	return fn_return_canonical_type_name(actual) == fn_return_canonical_type_name(expected)
}

fn fn_param_can_cast_userdata_param(actual &Type, expected &Type) bool {
	return (fn_param_is_voidptr_type(expected) && fn_param_is_nonvoid_pointer_type(actual))
		|| (fn_param_is_nonvoid_pointer_type(expected) && fn_param_is_voidptr_type(actual))
		|| fn_param_pointer_slot_compatible(expected, actual)
		|| fn_param_pointer_slot_compatible(actual, expected)
}

// fn_param_pointer_slot_compatible reports whether `slot` is `&voidptr` (`&&voidptr`...)
// and `other` points, at the same depth, to a slot of the same kind: a pointer or a
// function value. C translated by c2v declares a `void (**pxFunc)(...)` parameter as
// `&voidptr` in a function definition and as `&fn (...)` in a struct field. A `&voidptr`
// does not stand for `&i32`: the callee would store a pointer in an `i32`.
fn fn_param_pointer_slot_compatible(slot Type, other Type) bool {
	clean_slot := unalias_type(slot)
	clean_other := unalias_type(other)
	if clean_slot !is Pointer || clean_other !is Pointer {
		return false
	}
	slot_base := unalias_type((clean_slot as Pointer).base_type)
	other_base := unalias_type((clean_other as Pointer).base_type)
	if fn_param_is_voidptr_type(slot_base) {
		if other_base is FnType {
			return true
		}
		if other_base is Pointer {
			pointee := unalias_type(other_base.base_type)
			return pointee !is Pointer && pointee !is FnType
		}
		return false
	}
	return fn_param_pointer_slot_compatible(slot_base, other_base)
}

const fn_param_void_pointer_spelling = flat.source_type_id('&void')

fn fn_param_is_voidptr_type(typ &Type) bool {
	// Only nominal types and pointers can have either spelling. Formatting a
	// callback or container here creates names that cannot match.
	match typ {
		Struct, Alias, Enum, Interface, SumType {
			name_id := typ.source_type_id()
			if name_id == flat.source_type_voidptr || name_id == fn_param_void_pointer_spelling {
				return true
			}
		}
		Pointer {
			match typ.base_type {
				Struct, Alias, Enum, Interface, SumType {
					if typ.base_type.source_type_id() == flat.source_type_void {
						return true
					}
				}
				else {}
			}
		}
		else {}
	}
	clean := unalias_type(typ)
	if clean is Pointer {
		base := unalias_type(clean.base_type)
		return base is Void
	}
	return false
}

fn fn_param_is_nonvoid_pointer_type(typ &Type) bool {
	clean := unalias_type(typ)
	if clean is Pointer {
		base := unalias_type(clean.base_type)
		return base !is Void
	}
	return false
}

fn call_arg_integer_type(typ &Type) bool {
	clean := unalias_type(typ)
	if clean.is_integer() {
		return true
	}
	return match clean {
		Primitive {
			// A primitive with no numeric or boolean property is spelled `int`.
			!clean.props.has(.boolean) && !clean.props.has(.float)
		}
		Struct, Enum, Interface, SumType {
			clean.source_type_id() in [flat.source_type_int, flat.source_type_i8, flat.source_type_i16,
				flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
				flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
				flat.source_type_uint, flat.source_type_rune]
		}
		else { false }
	}
}

fn call_arg_numeric_type(typ &Type) bool {
	clean := unalias_type(typ)
	return call_arg_integer_type(clean) || clean.is_float()
}

// implicit_integer_to_float_compatible reports whether an `actual` integer
// converts to an `expected` float without a cast: every integer but `i64` and
// `u64` does.
pub fn implicit_integer_to_float_compatible(actual &Type, expected &Type) bool {
	clean_actual := unalias_type(actual)
	clean_expected := unalias_type(expected)
	return clean_actual.is_integer() && clean_actual.source_type_id() !in [
		flat.source_type_i64,
		flat.source_type_u64,
	]
		&& clean_expected.is_float()
}

fn call_arg_numeric_promotion_index(name_id u32) int {
	return match name_id {
		flat.source_type_i8 { 5 }
		flat.source_type_i16 { 6 }
		flat.source_type_i32 { 7 }
		flat.source_type_int { 8 }
		flat.source_type_i64 { 9 }
		flat.source_type_isize { 10 }
		flat.source_type_u8 { 11 }
		flat.source_type_u16 { 12 }
		flat.source_type_u32 { 13 }
		flat.source_type_u64 { 14 }
		flat.source_type_uint { 15 }
		// 128-bit ranks above the 64-bit types, so a narrower integer widens into
		// it. Without these rows the index is -1, no widening is offered, and an
		// expression like `wide + 1` is typed `int` and cut to 64 bits.
		qualify_source_type_i128 { 16 }
		qualify_source_type_u128 { 17 }
		flat.source_type_rune { 22 }
		else { -1 }
	}
}

fn call_arg_implicit_numeric_widening(actual &Type, expected &Type) bool {
	actual_name_id := unalias_type(actual).source_type_id()
	expected_name_id := unalias_type(expected).source_type_id()
	if actual_name_id == flat.source_type_f32 && expected_name_id == flat.source_type_f64 {
		return true
	}
	actual_index := call_arg_numeric_promotion_index(actual_name_id)
	expected_index := call_arg_numeric_promotion_index(expected_name_id)
	if actual_index < 0 || expected_index < 0 {
		return false
	}
	mut high := actual_index
	mut low := expected_index
	if high < low {
		high, low = low, high
	}
	promoted := if low >= 11 {
		high
	} else if low >= 5 && (high <= 10 || high == 22) {
		if low == 9 { low } else { high }
	} else if high == 11 && low > 5 {
		low
	} else if high == 12 && low > 6 {
		low
	} else if high == 13 && low > 8 {
		low
	} else if high == 14 && low >= 9 {
		low
	} else if high == 15 && low >= 10 {
		low
	} else {
		-1
	}
	return promoted == expected_index
}

fn call_arg_byte_rune_compatible(actual &Type, expected &Type) bool {
	actual_name_id := unalias_type(actual).source_type_id()
	expected_name_id := unalias_type(expected).source_type_id()
	return (actual_name_id == flat.source_type_u8 && expected_name_id == flat.source_type_rune)
		|| (actual_name_id == flat.source_type_rune && expected_name_id == flat.source_type_u8)
}

fn escaped_identifier_name(name string) string {
	return if name.starts_with('@') { name[1..] } else { name }
}

fn fn_return_canonical_type_name(typ &Type) u32 {
	if typ is Alias {
		return fn_return_canonical_type_name(typ.base_type)
	}
	return typ.source_type_id()
}

// is_ierror_type reports whether is ierror type applies in types.
fn is_builtin_ierror_name(name_id u32) bool {
	return name_id == ierror_source_interface_name || name_id == ierror_source_builtin_interface_name
}

fn is_ierror_type(t &Type) bool {
	if t is Alias {
		return is_builtin_ierror_name(t.source_type_id()) || is_ierror_type(t.base_type)
	}
	if t is Pointer {
		return is_ierror_type(t.base_type)
	}
	if t is Struct {
		return is_builtin_ierror_name(t.source_type_id())
	}
	if t is Interface {
		return is_builtin_ierror_name(t.source_type_id())
	}
	return false
}

fn (tc &TypeChecker) type_embeds_error(t &Type) bool {
	clean := unwrap_pointer(t)
	if clean is Alias {
		return tc.type_embeds_error(clean.base_type)
	}
	if clean is Struct {
		if flat.source_type_syntax(clean.source_type_id()).unqualified() == checker_source_error {
			return true
		}
		return tc.receiver_embeds(clean, Type(Struct{
			name_id: checker_source_error
		}))
	}
	return false
}

// is_runtime_array_type reports whether is runtime array type applies in types.
fn is_runtime_array_type(t &Type) bool {
	if t is Alias {
		return is_runtime_array_type(t.base_type)
	}
	if t is Struct {
		return t.source_type_id() == qualify_source_type_array
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

fn (tc &TypeChecker) const_import_alias_key(alias string, field string) (string, bool) {
	// Code generation can evaluate a stored type after leaving its source file.
	// Recover the import only when the alias names one module declaring the const
	// across the program; an alias of an unrelated module elsewhere cannot conflict.
	// Report conflicting meanings separately from the absence of an import alias.
	mut found := ''
	if tc.const_import_files_count == tc.file_imports_by_file.len {
		for module_name in tc.const_import_modules[alias] {
			key := '${module_name}.${field}'
			if key !in tc.const_exprs {
				continue
			}
			if found.len > 0 && found != key {
				return '', true
			}
			found = key
		}
		return found, false
	}
	for _, info in tc.file_imports_by_file {
		if isnil(info) {
			continue
		}
		module_name := info.imports[alias] or { continue }
		key := '${module_name}.${field}'
		if key !in tc.const_exprs {
			continue
		}
		if found.len > 0 && found != key {
			return '', true
		}
		found = key
	}
	return found, false
}

// Bare ASCII identifiers cannot be numeric, arithmetic, casts or enum selectors.
// Keep underscore-prefixed inputs on the fallback: the literal parser accepts `_123`.
@[inline]
fn const_eval_plain_identifier(name string) bool {
	if name.len == 0 || !((name[0] >= `a` && name[0] <= `z`)
		|| (name[0] >= `A` && name[0] <= `Z`)) {
		return false
	}
	for ch in name {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_`) {
			return false
		}
	}
	return true
}

// const_int_value_in_module supports const int value handling for a specific module.
pub fn (tc &TypeChecker) const_int_value_in_module(name string, module_name string, seen []string) ?int {
	if name in seen {
		return none
	}
	if tc.const_expr_short_names_count == tc.const_exprs.len
		&& const_eval_plain_identifier(name) && name !in tc.const_expr_short_names {
		return none
	}
	// Literals cannot name declarations. Underscore-prefixed identifiers still
	// need lookup first: the integer parser also accepts their stripped digits.
	if name.len > 0 && name[0] >= `0` && name[0] <= `9` {
		if value := v_int_literal_value(name) {
			return value
		}
	}
	has_dot := name.contains('.')
	if module_name != '' && module_name != 'main' && module_name != 'builtin' && !has_dot {
		// This is the first candidate for an unqualified name in its module.
		// Evaluate it before preparing fallbacks, including an unresolved value.
		key := '${module_name}.${name}'
		if key !in seen {
			if expr_id := tc.const_exprs[key] {
				mut next_seen := seen.clone()
				next_seen << key
				const_module := tc.const_modules[key] or { module_name }
				return tc.const_int_expr(expr_id, const_module, next_seen)
			}
		}
	}
	mut candidates := []string{}
	if has_dot {
		alias_key, ambiguous := tc.const_import_alias_key(name.all_before_last('.'),
			name.all_after_last('.'))
		if ambiguous {
			return none
		}
		if alias_key.len > 0 {
			// An import alias takes precedence over a module with the same name.
			candidates << alias_key
		}
	}
	if key := tc.shadowed_builtin_const_key_in_module(name, module_name) {
		candidates << key
	}
	if tc.bare_const_key_is_visible_in_module(name, module_name) {
		candidates << name
	}
	qname := tc.qualify_name(name)
	if qname != name {
		candidates << qname
	}
	if key := tc.const_key_for_suffix(name) {
		candidates << key
	}
	for key in candidates {
		if key in seen || !tc.bare_const_key_is_visible_in_module(key, module_name) {
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
	// Without an operator character neither the split below nor a leading sign can
	// apply. Most texts that get here are plain names; skip the per-character scan.
	if !const_int_text_has_operator(expr) {
		return none
	}
	// Operators are grouped by the same precedence levels as token.left_binding_power:
	// `+ - | ^` share sum, while `* / % << >> >>> &` share product. Scanning the
	// rightmost operator at a level preserves left associativity. Power scans its first
	// operator instead because it is right-associative.
	// Longer operators match first (`>>>` before `>>`, two-char before one) and `idx + op.len`
	// skips the operator.
	for level in [['+', '-', '|', '^'], ['*', '/', '%', '&', '<<', '>>', '>>>'], ['**']] {
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

// const_int_text_has_operator reports whether `expr` contains a character of an
// operator that const_int_value_in_module can split on or apply as a sign.
fn const_int_text_has_operator(expr string) bool {
	for ch in expr {
		match ch {
			`+`, `-`, `|`, `^`, `*`, `/`, `%`, `&`, `<`, `>` { return true }
			else {}
		}
	}
	return false
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
	cast_type := unalias_type(tc.parse_type(flat.source_type_id(cast_type_name)))
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
	enum_name_id := tc.resolve_enum_name(flat.source_type_id(trimmed_space(expr[..dot]))) or { return none }
	field := tc.enum_field_name(enum_name_id, trimmed_space(expr[dot + 1..])) or { return none }
	for item in tc.comptime_static_enum_decl_value_cases(enum_name_id) {
		if item.name == field && item.has_value {
			if item.value_from_c {
				return none
			}
			return item.value
		}
	}
	fields := tc.enum_fields[enum_name_id] or { return none }
	for idx, name in fields {
		if name == field {
			return if enum_name_id in tc.flag_enums { 1 << idx } else { idx }
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
			cast_type := unalias_type(tc.parse_type(node.source_value_id()))
			if !cast_type.is_integer() {
				return none
			}
			return tc.const_int_expr(tc.a.child(&node, 0), module_name, seen)
		}
		.enum_val {
			return tc.const_int_enum_selector_value(node.value)
		}
		.selector {
			return tc.const_int_value_in_module(tc.source_text_for_node(id), module_name,
				seen)
		}
		.sizeof_expr {
			return tc.const_sizeof_type_value(node.source_value_id())
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

fn (tc &TypeChecker) const_sizeof_type_value(type_name_id u32) ?int {
	typ := unalias_type(tc.parse_type(type_name_id))
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

// InterfaceSatisfactionPair records an ordered obligation in an active interface proof.
struct InterfaceSatisfactionPair {
	actual   u32
	expected u32
}

// type_implements_interface returns type implements interface data for TypeChecker.
fn (tc &TypeChecker) type_implements_interface(actual Type, expected Interface) bool {
	return type_implements_interface_guarded(tc, actual, expected, []InterfaceSatisfactionPair{})
}

fn type_implements_interface_guarded(tc &TypeChecker, actual &Type, expected Interface, visited []InterfaceSatisfactionPair) bool {
	clean := unwrap_pointer(actual)
	if clean is Unknown {
		return true
	}
	if tc.interface_has_no_requirements(expected.source_type_id()) {
		return true
	}
	if clean is Interface {
		return interface_implements_interface_guarded(tc, clean.source_type_id(), expected.source_type_id(), visited)
	}
	if clean is Array {
		if tc.interface_abstract_method_names(expected.source_type_id()).len > 0 {
			return false
		}
		for field in tc.interface_field_list(expected.source_type_id()) {
			actual_field := tc.builtin_array_field_type(field.name) or { return false }
			if !tc.type_compatible(actual_field, field.typ)
				|| !tc.type_compatible(field.typ, actual_field) {
				return false
			}
		}
		return true
	}
	if clean is Map {
		if tc.interface_abstract_method_names(expected.source_type_id()).len > 0 {
			return false
		}
		for field in tc.interface_field_list(expected.source_type_id()) {
			if field.name != 'len' || !tc.type_compatible(builtin_int_type, field.typ)
				|| !tc.type_compatible(field.typ, builtin_int_type) {
				return false
			}
		}
		return true
	}
	concrete_name_id := method_type_name(clean)
	if concrete_name_id == 0 {
		return false
	}
	return named_type_implements_interface_guarded(tc, concrete_name_id, expected.source_type_id(), visited)
}

fn (tc &TypeChecker) builtin_array_field_type(name string) ?Type {
	return match name {
		'element_size', 'offset', 'len', 'cap' { builtin_int_type }
		'data' { builtin_voidptr_type }
		'flags' { *tc.parse_type(checker_source_array_flags) }
		else { none }
	}
}

// interface_implements_interface supports interface implements interface handling for TypeChecker.
pub fn (tc &TypeChecker) interface_implements_interface(actual_name_id u32, expected_name_id u32) bool {
	return interface_implements_interface_guarded(tc, actual_name_id, expected_name_id, []InterfaceSatisfactionPair{})
}

// interface_implements_interface_guarded carries the same set of in-progress pairs as
// named_type_implements_interface_guarded, for the interface-to-interface case. Two
// distinct interfaces whose like-named methods each return their own interface type --
// `clone() First` against `clone() Second` -- ask this question again while answering it,
// and the name equality fast path above cannot stop that cycle because the names differ.
// The pair is ordered: `First <: Second` and `Second <: First` are separate obligations
// and both are asked, so each is recorded and discharged on its own. As for concrete
// types, a revisited pair is not proven, so `First` does not implement `Second`.
fn interface_implements_interface_guarded(tc &TypeChecker, actual_name_id u32, expected_name_id u32, visited []InterfaceSatisfactionPair) bool {
	actual := tc.interface_metadata_name(actual_name_id)
	expected := tc.interface_metadata_name(expected_name_id)
	if actual == expected {
		return true
	}
	guard_key := InterfaceSatisfactionPair{ actual: actual, expected: expected }
	if guard_key in visited {
		return false
	}
	mut in_progress := visited.clone()
	in_progress << guard_key
	for method in tc.interface_method_names(expected) {
		actual_key := tc.interface_method_signature_key(actual, method) or {
			'${flat.source_type_name(actual)}.${method}'
		}
		expected_key := tc.interface_method_signature_key(expected, method) or {
			'${flat.source_type_name(expected)}.${method}'
		}
		if actual_key !in tc.fn_param_types
			|| !method_signature_compatible_guarded(tc, actual_key, expected_key, in_progress) {
			return false
		}
	}
	for field in tc.interface_field_list(expected) {
		actual_field := tc.interface_field_type(actual, field.name) or { return false }
		if !interface_field_types_compatible_guarded(tc, actual_field, field.typ, in_progress) {
			return false
		}
	}
	return true
}

// interface_field_types_compatible_guarded compares a pair of field types while an
// interface satisfaction proof is in progress. An interface-typed field can refer back to
// the pair being proved, and the general type_compatible path re-enters the proof through
// its own entry points with an empty active set, dropping the guard. Interface-typed
// fields are therefore compared directly, and everything else defers to type_compatible
// as before.
fn interface_field_types_compatible_guarded(tc &TypeChecker, actual Type, expected Type, visited []InterfaceSatisfactionPair) bool {
	actual_iface := guarded_interface_name(tc, actual)
	expected_iface := guarded_interface_name(tc, expected)
	if actual_iface != 0 && expected_iface != 0 {
		return interface_implements_interface_guarded(tc, actual_iface, expected_iface, visited)
			&& interface_implements_interface_guarded(tc, expected_iface, actual_iface, visited)
	}
	if expected_iface != 0 {
		return type_implements_interface_guarded(tc, actual, Interface{
			name_id: expected_iface
		}, visited) && tc.type_compatible(expected, actual)
	}
	if actual_iface != 0 {
		return type_implements_interface_guarded(tc, expected, Interface{
			name_id: actual_iface
		}, visited) && tc.type_compatible(actual, expected)
	}
	return tc.type_compatible(actual, expected) && tc.type_compatible(expected, actual)
}

// guarded_interface_name returns the interface spelling ID a field type denotes, or zero.
fn guarded_interface_name(tc &TypeChecker, typ Type) u32 {
	clean := unalias_type(unwrap_pointer(typ))
	if clean is Interface {
		return clean.source_type_id()
	}
	metadata := tc.interface_metadata_name(clean.source_type_id())
	if metadata in tc.interface_names {
		return metadata
	}
	return 0
}

// interface_metadata_name resolves a source spelling ID to the interface declaration spelling ID.
pub fn (tc &TypeChecker) interface_metadata_name(name_id u32) u32 {
	if name_id == 0 {
		return name_id
	}
	mut lookup_id := name_id
	if flat.source_type_shape(name_id).has(.bracketed) {
		syntax := flat.source_type_syntax(name_id)
		if syntax.application_valid() {
			lookup_id = syntax.application_base()
		}
	}
	if lookup_id in tc.interface_names || lookup_id in tc.interface_abstract_methods
		|| tc.interface_embeds.contains(lookup_id) || lookup_id in tc.interface_fields {
		return lookup_id
	}
	if !flat.source_type_shape(lookup_id).has(.dotted) {
		qname_id := tc.qualify_type_name(lookup_id)
		if qname_id in tc.interface_names || qname_id in tc.interface_abstract_methods
			|| tc.interface_embeds.contains(qname_id) || qname_id in tc.interface_fields {
			return qname_id
		}
		if qname_id != lookup_id && tc.non_interface_type_known(qname_id) {
			return lookup_id
		}
	}
	// A struct, enum or sum type never names an interface. Without this guard the
	// short-name match below maps e.g. `csv.Reader` onto `io.Reader`, making the
	// struct an implementer of that interface and its methods interface methods.
	if tc.non_interface_type_known(lookup_id) {
		return lookup_id
	}
	// Only a plain, possibly module-qualified, name can name an interface. In an
	// array, map, option, pointer or function type the text after the last `.` is
	// an element or parameter type (`[]bar.MyData` -> `MyData`), which must not
	// match a same-named interface of another module.
	lookup_syntax := flat.source_type_syntax(lookup_id)
	if !lookup_syntax.named_type() {
		return lookup_id
	}
	short_id := lookup_syntax.unqualified()
	mut match_id := u32(0)
	for candidate, _ in tc.interface_names {
		candidate_id := candidate
		if flat.source_type_syntax(candidate_id).unqualified() != short_id {
			continue
		}
		if match_id != 0 && match_id != candidate_id {
			return lookup_id
		}
		match_id = candidate_id
	}
	if match_id != 0 {
		return match_id
	}
	return lookup_id
}

fn (tc &TypeChecker) non_interface_type_known(name_id u32) bool {
	return name_id in tc.structs || name_id in tc.enum_names || tc.sum_types.contains(name_id)
}

// named_type_implements_interface
// supports helper handling in types.
pub fn (tc &TypeChecker) named_type_implements_interface(concrete_name_id u32, iface_name_id u32) bool {
	return named_type_implements_interface_guarded(tc, concrete_name_id, iface_name_id, []InterfaceSatisfactionPair{})
}

// named_type_implements_interface_guarded carries the set of (concrete type, interface)
// pairs whose proof is still in progress. A self-referential interface -- one with a
// method whose return type is the interface itself, as in `clone() Cloner` -- makes the
// satisfaction check re-enter with a pair it is already proving, which otherwise recurses
// until the checker's stack is exhausted. A pair that is already in progress is not
// proven: a method that names the interface itself matches exactly and never gets here,
// while assuming the pair would let any type with `clone() Self`, such as `string`,
// implement `Cloner` through its covariant return, which cgen cannot dispatch.
fn named_type_implements_interface_guarded(tc &TypeChecker, concrete_name_id u32, iface_name_id u32, visited []InterfaceSatisfactionPair) bool {
	if tc.interface_has_no_requirements(iface_name_id) {
		return true
	}
	guard_key := InterfaceSatisfactionPair{ actual: concrete_name_id, expected: iface_name_id }
	if guard_key in visited {
		return false
	}
	mut in_progress := visited.clone()
	in_progress << guard_key
	// Only the abstract (declared) methods must be provided by the concrete type.
	// Methods defined directly on the interface (default implementations) are
	// inherited and need not be reimplemented.
	for method in tc.interface_abstract_method_names(iface_name_id) {
		expected_key := tc.interface_method_signature_key(iface_name_id, method) or {
			'${flat.source_type_name(iface_name_id)}.${method}'
		}
		if concrete_name_id == flat.source_type_char && method == 'str'
			&& tc.interface_method_is_str_requirement(expected_key) {
			return false
		}
		if info := tc.resolve_generic_struct_method(concrete_name_id, method) {
			if !method_call_info_signature_compatible_for_interface_guarded(tc, info, expected_key,
				iface_name_id, in_progress) {
				return false
			}
			continue
		}
		if concrete_key := tc.concrete_method_signature_key(concrete_name_id, method) {
			if !method_signature_compatible_for_interface_guarded(tc, concrete_key, expected_key,
				iface_name_id, in_progress) {
				return false
			}
			continue
		}
		if info := tc.resolve_generic_sum_method(concrete_name_id, method) {
			if !method_call_info_signature_compatible_for_interface_guarded(tc, info, expected_key,
				iface_name_id, in_progress) {
				return false
			}
			continue
		}
		if method == 'str' && tc.interface_method_is_str_requirement(expected_key)
			&& tc.type_has_implicit_str_method(concrete_name_id) {
			continue
		}
		return false
	}
	for field in tc.interface_field_list(iface_name_id) {
		actual_field := tc.interface_actual_field(concrete_name_id, field.name) or { return false }
		if !interface_field_types_compatible_guarded(tc, actual_field.typ, field.typ, in_progress)
			|| (field.is_mut && !actual_field.is_mut) {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) interface_method_is_str_requirement(expected_key string) bool {
	ret := tc.fn_ret_types[expected_key] or { return false }
	if ret.source_type_id() != flat.source_type_string {
		return false
	}
	params := tc.fn_param_types[expected_key] or { return false }
	return params.len == 1
}

// type_has_implicit_str_method reports whether a source spelling ID has an implicit string method.
pub fn (tc &TypeChecker) type_has_implicit_str_method(name_id u32) bool {
	clean_id := trimmed_source_type_id(name_id)
	if clean_id == 0 {
		return false
	}
	mut candidates := [clean_id]
	if !flat.source_type_shape(clean_id).has(.dotted) {
		qname_id := tc.qualify_type_name(clean_id)
		if qname_id != clean_id {
			candidates << qname_id
		}
	}
	for candidate_id in candidates {
		if tc.type_name_resolves_to_sum_type(candidate_id) {
			continue
		}
		base_name := flat.source_type_syntax(candidate_id).generic_base()
		if candidate_id in tc.structs || base_name in tc.structs || candidate_id in tc.enum_names
			|| candidate_id in tc.flag_enums || candidate_id in tc.interface_names {
			return true
		}
		if is_builtin_type_name_id(candidate_id) {
			mut seen := map[u32]bool{}
			if tc.type_supports_implicit_str(tc.parse_type(candidate_id), mut seen) {
				return true
			}
		}
		if tc.type_aliases.contains(candidate_id) {
			mut seen := map[u32]bool{}
			seen[candidate_id] = true
			if tc.type_supports_implicit_str(tc.parse_type(tc.type_aliases.value_id(candidate_id)), mut seen) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) type_supports_implicit_str(typ &Type, mut seen map[u32]bool) bool {
	if typ is Alias {
		if seen[typ.source_type_id()] {
			return false
		}
		seen[typ.source_type_id()] = true
		return tc.type_supports_implicit_str(typ.base_type, mut seen)
	}
	if typ is Primitive {
		return typ.props.has(.boolean) || typ.props.has(.integer) || typ.props.has(.float)
	}
	return typ is String || typ is Rune || typ is ISize || typ is USize || typ is Pointer
		|| typ is Array || typ is ArrayFixed || typ is Map || typ is Enum || typ is Struct
}

fn (tc &TypeChecker) type_name_resolves_to_sum_type(name_id u32) bool {
	mut current_id := trimmed_source_type_id(name_id)
	mut seen := map[u32]bool{}
	for current_id != 0 {
		if tc.sum_types.contains(current_id) {
			return true
		}
		if !flat.source_type_shape(current_id).has(.dotted) {
			qualified := tc.qualify_type_name(current_id)
			if tc.sum_types.contains(qualified) {
				return true
			}
		}
		if seen[current_id] {
			return false
		}
		seen[current_id] = true
		mut next_id := tc.type_aliases.type_id(current_id) or { u32(0) }
		if next_id == 0 && !flat.source_type_shape(current_id).has(.dotted) {
			next_id = tc.type_aliases.value_id(tc.qualify_type_name(current_id))
		}
		current_id = trimmed_source_type_id(next_id)
	}
	return false
}

fn (tc &TypeChecker) interface_has_no_requirements(iface_name_id u32) bool {
	return tc.interface_abstract_method_names(iface_name_id).len == 0
		&& tc.interface_field_list(iface_name_id).len == 0
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

fn marker_type_name(name_id u32) u32 {
	clean_id := trimmed_source_type_id(name_id)
	syntax := flat.source_type_syntax(clean_id)
	return if syntax.application_valid() { syntax.application_base() } else { clean_id }
}

fn interface_marker_matches(name_id u32, target_id u32) bool {
	clean := marker_type_name(name_id)
	target_clean := marker_type_name(target_id)
	return clean == target_clean || flat.source_type_syntax(clean).unqualified() == target_clean
		|| clean == flat.source_type_syntax(target_clean).unqualified()
}

// named_type_implements_marker checks the source type and marker spelling IDs.
pub fn (tc &TypeChecker) named_type_implements_marker(concrete_name_id u32, target_id u32) bool {
	mut name_id := trimmed_source_type_id(concrete_name_id)
	if flat.source_type_shape(name_id).has(.pointer) {
		name_id = trimmed_source_type_id(flat.source_type_syntax(name_id).raw_base())
	}
	name_id = marker_type_name(name_id)
	mut candidates := [name_id]
	if !flat.source_type_shape(name_id).has(.dotted) {
		qname_id := tc.qualify_type_name(name_id)
		if qname_id != name_id {
			candidates << qname_id
		}
	} else if flat.source_type_query(name_id).has(.main_prefix) {
		candidates << flat.source_type_syntax(name_id).main_base()
	}
	for candidate_id in candidates {
		impls := tc.struct_implements.type_ids(candidate_id) or { continue }
		for impl_id in impls {
			if interface_marker_matches(impl_id, target_id) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) type_has_compiler_default_clone(t &Type) bool {
	if t is Struct {
		return tc.named_type_implements_marker(t.source_type_id(), checker_source_iclone)
	}
	return false
}

// interface_impl_names returns source spelling IDs of concrete types (structs and
// aliases) that implement the interface spelling ID. Once a snapshot is frozen, its names
// stay first so transform-emitted interface IDs are preserved; later
// implementers are appended in deterministic discovery order.
pub fn (tc &TypeChecker) interface_impl_names(iface_name_id u32) []u32 {
	mut cache := tc.type_cache
	if !isnil(cache) {
		if cached := cache.interface_impl_entries[iface_name_id] {
			return cached.ids.clone()
		}
	}
	mut impls := []u32{}
	if snapshot := tc.interface_impl_name_snapshots[iface_name_id] {
		impls = snapshot.ids.clone()
		mut seen := map[u32]bool{}
		for name_id in impls { seen[name_id] = true }
		// Preserve transform-emitted dispatch IDs; append later generic implementers.
		for name_id in tc.interface_impl_names_uncached_after(iface_name_id, tc.interface_impl_candidates_at_snapshot) {
			if name_id !in seen {
				seen[name_id] = true
				impls << name_id
			}
		}
	} else {
		impls = tc.interface_impl_names_uncached(iface_name_id)
	}
	if !isnil(cache) {
		cache.interface_impl_entries.reserve(1)
		cache.interface_impl_entries[iface_name_id] = flat.TypeTextList{ ids: impls.clone() }
	}
	return impls
}

// pre_transform_interface_impl_names returns the immutable implementer snapshot
// prepared after semantic checking and before generic monomorphization.
pub fn (tc &TypeChecker) pre_transform_interface_impl_names(iface_name_id u32) ?[]u32 {
	index := tc.interface_impl_indexes[iface_name_id] or { return none }
	return index.names.ids.clone()
}

// interface_type_ids maps implementer spelling IDs to runtime `_typ` dispatch IDs, preserving
// any snapshot IDs emitted before late generic implementers were discovered.
pub fn (tc &TypeChecker) interface_type_ids(iface_name_id u32) map[u32]int {
	if snapshot := tc.interface_impl_name_snapshots[iface_name_id] {
		return stable_interface_type_ids_preserving_prefix(snapshot.ids, tc.interface_impl_names(iface_name_id))
	}
	return stable_interface_type_ids(tc.interface_impl_names(iface_name_id))
}

// freeze_interface_impl_names snapshots the interface implementation order used
// by transform-generated `_typ` checks before later metadata cleanup can remove
// unused generic declarations and shift cgen's ids.
pub fn (mut tc TypeChecker) freeze_interface_impl_names() {
	mut snapshots := map[u32]flat.TypeTextList{}
	for iface_name in tc.interface_names.keys() {
		snapshots[iface_name] = flat.TypeTextList{ ids: tc.interface_impl_names_uncached(iface_name) }
	}
	tc.interface_impl_name_snapshots = snapshots
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
	mut snapshots := map[u32]flat.TypeTextList{}
	for iface_name in tc.interface_names.keys() {
		iface_id := iface_name
		if index := tc.interface_impl_indexes[iface_id] {
			snapshots[iface_id] = flat.TypeTextList{ ids: index.names.ids.clone() }
		} else {
			snapshots[iface_id] = flat.TypeTextList{ ids: tc.interface_impl_names_uncached(iface_id) }
		}
	}
	tc.interface_impl_name_snapshots = snapshots
	tc.interface_impl_candidates_at_snapshot = tc.interface_impl_candidates_at_index.clone()
	tc.clear_interface_impl_cache()
}

fn (tc &TypeChecker) interface_impl_names_uncached(iface_name_id u32) []u32 {
	return tc.interface_impl_names_uncached_after(iface_name_id, map[u32]bool{})
}

fn add_interface_impl_candidate(mut candidates map[u32]bool, name u32, excluded map[u32]bool) {
	if name !in excluded {
		candidates[name] = true
	}
}

fn (tc &TypeChecker) interface_impl_candidate_names() map[u32]bool {
	mut candidates := map[u32]bool{}
	for name in implicit_str_builtin_type_names() {
		candidates[name] = true
	}
	for name, _ in tc.structs {
		candidate := interface_impl_candidate_name(name)
		// builtin.VMapData is private map storage and cannot be an interface value.
		if name != checker_source_vmap_data || tc.struct_modules[name] != 'builtin' {
			candidates[candidate] = true
		}
	}
	for name, _ in tc.enum_names {
		candidates[interface_impl_candidate_name(name)] = true
	}
	for name, target_id in tc.type_aliases.ids {
		candidates[interface_impl_candidate_name(name)] = true
		typ := tc.parse_type(target_id)
		if typ is FnType {
			candidates[Type(typ).source_type_id()] = true
		}
	}
	for name, _ in tc.interface_names {
		name_id := name
		candidates[name_id] = true
		short_name := interface_impl_candidate_name(name_id)
		if short_name != name_id {
			candidates[short_name] = true
		}
	}
	return candidates
}

fn (tc &TypeChecker) interface_impl_names_uncached_after(iface_name_id u32, excluded map[u32]bool) []u32 {
	candidates := if excluded.len == 0 && !isnil(tc.interface_impl_memo) {
		tc.memo_interface_impl_candidates(iface_name_id)
	} else {
		tc.interface_impl_candidates(iface_name_id, excluded)
	}
	mut impls := []u32{}
	for name_id in candidates {
		if name_id in tc.interface_names {
			if tc.interface_implements_interface(name_id, iface_name_id) {
				impls << name_id
			}
			continue
		}
		resolved_name_id := tc.interface_metadata_name(name_id)
		if resolved_name_id in tc.interface_names {
			if tc.interface_implements_interface(resolved_name_id, iface_name_id) {
				impls << name_id
			}
			continue
		}
		if tc.named_type_implements_interface(name_id, iface_name_id) {
			impls << name_id
		}
	}
	return impls
}

// interface_impl_candidates lists, sorted by spelling, the types that are
// tested for implementing `iface_name_id`, except the `excluded` ones.
fn (tc &TypeChecker) interface_impl_candidates(iface_name_id u32, excluded map[u32]bool) []u32 {
	mut candidate_set := map[u32]bool{}
	has_no_requirements := tc.interface_has_no_requirements(iface_name_id)
	accepts_implicit_str := tc.interface_accepts_implicit_str(iface_name_id)
	if has_no_requirements || accepts_implicit_str {
		for name in implicit_str_builtin_type_names() {
			add_interface_impl_candidate(mut candidate_set, name, excluded)
		}
	}
	for name, _ in tc.structs {
		candidate := interface_impl_candidate_name(name)
		if name != checker_source_vmap_data || tc.struct_modules[name] != 'builtin' {
			add_interface_impl_candidate(mut candidate_set, candidate, excluded)
		}
	}
	// Enums can declare methods, so like structs they may implement any interface.
	for name, _ in tc.enum_names {
		add_interface_impl_candidate(mut candidate_set, interface_impl_candidate_name(name), excluded)
	}
	for name, target_id in tc.type_aliases.ids {
		candidate_name := interface_impl_candidate_name(name)
		add_interface_impl_candidate(mut candidate_set, candidate_name, excluded)
		if has_no_requirements && candidate_name !in excluded {
			typ := tc.parse_type(target_id)
			if typ is FnType {
				add_interface_impl_candidate(mut candidate_set, Type(typ).source_type_id(), excluded)
			}
		}
	}
	for name, _ in tc.interface_names {
		name_id := name
		if name_id != iface_name_id && tc.interface_implements_interface(name_id, iface_name_id) {
			add_interface_impl_candidate(mut candidate_set, name_id, excluded)
		}
		short_name := interface_impl_candidate_name(name_id)
		if short_name != name_id && short_name != iface_name_id
			&& tc.interface_implements_interface(name_id, iface_name_id) {
			add_interface_impl_candidate(mut candidate_set, short_name, excluded)
		}
	}
	mut candidates := candidate_set.keys()
	candidates.sort_with_compare(compare_interface_impl_spelling_ids)
	return candidates
}

// memo_interface_impl_candidates is interface_impl_candidates without excluded
// types. The structs, enums and aliases, which every interface tests, are
// sorted once; the candidates that depend on the interface are merged in.
// Sorting by spelling orders the IDs totally, since an ID names one spelling.
fn (tc &TypeChecker) memo_interface_impl_candidates(iface_name_id u32) []u32 {
	mut memo := tc.interface_impl_memo
	if !memo.base_candidates_ready {
		mut base_set := map[u32]bool{}
		for name, _ in tc.structs {
			candidate := interface_impl_candidate_name(name)
			if name != flat.source_type_id('VMapData') || tc.struct_modules[name] != 'builtin' {
				base_set[candidate] = true
			}
		}
		for name, _ in tc.enum_names {
			base_set[interface_impl_candidate_name(name)] = true
		}
		for name, _ in tc.type_aliases.ids {
			base_set[interface_impl_candidate_name(name)] = true
		}
		mut base := base_set.keys()
		base.sort_with_compare(compare_interface_impl_spelling_ids)
		memo.base_candidates = base
		memo.base_candidates_ready = true
	}
	mut extra_set := map[u32]bool{}
	has_no_requirements := tc.interface_has_no_requirements(iface_name_id)
	accepts_implicit_str := tc.interface_accepts_implicit_str(iface_name_id)
	if has_no_requirements || accepts_implicit_str {
		for name in implicit_str_builtin_type_names() {
			extra_set[name] = true
		}
	}
	if has_no_requirements {
		if !memo.alias_fn_candidates_ready {
			mut alias_fn_ids := []u32{}
			for _, target_id in tc.type_aliases.ids {
				typ := tc.parse_type(target_id)
				if typ is FnType {
					alias_fn_ids << Type(typ).source_type_id()
				}
			}
			memo.alias_fn_candidates = alias_fn_ids
			memo.alias_fn_candidates_ready = true
		}
		for name in memo.alias_fn_candidates {
			extra_set[name] = true
		}
	}
	for name, _ in tc.interface_names {
		name_id := name
		if name_id != iface_name_id && tc.interface_implements_interface(name_id, iface_name_id) {
			extra_set[name_id] = true
		}
		short_name := interface_impl_candidate_name(name_id)
		if short_name != name_id && short_name != iface_name_id
			&& tc.interface_implements_interface(name_id, iface_name_id) {
			extra_set[short_name] = true
		}
	}
	base := memo.base_candidates
	if extra_set.len == 0 {
		return base.clone()
	}
	mut extras := extra_set.keys()
	extras.sort_with_compare(compare_interface_impl_spelling_ids)
	mut candidates := []u32{cap: base.len + extras.len}
	mut i := 0
	mut j := 0
	for i < base.len && j < extras.len {
		base_id := base[i]
		extra_id := extras[j]
		order := compare_interface_impl_spelling_ids(&base_id, &extra_id)
		if order < 0 {
			candidates << base[i]
			i++
		} else if order > 0 {
			candidates << extras[j]
			j++
		} else {
			candidates << base[i]
			i++
			j++
		}
	}
	for i < base.len {
		candidates << base[i]
		i++
	}
	for j < extras.len {
		candidates << extras[j]
		j++
	}
	return candidates
}

// stable_interface_type_ids maps source spelling IDs to deterministic nonzero `_typ` dispatch IDs for
// interface implementers in caller-supplied order. Hash collisions are resolved
// with linear probing after earlier names keep their IDs, so late generic
// implementers appended after transform cannot shift IDs already emitted for
// existing interface values/checks.
pub fn stable_interface_type_ids(impl_names []u32) map[u32]int {
	mut ids := map[u32]int{}
	mut used := map[int]bool{}
	assign_stable_interface_type_ids(mut ids, mut used, impl_names)
	return ids
}

// stable_interface_type_ids_preserving_prefix assigns runtime IDs for implementer spelling IDs while
// keeping all prefix IDs exactly as they would be when assigned alone.
pub fn stable_interface_type_ids_preserving_prefix(prefix []u32, impl_names []u32) map[u32]int {
	mut ids := stable_interface_type_ids(prefix)
	mut used := map[int]bool{}
	mut seen := map[u32]bool{}
	for name_id, id in ids {
		used[id] = true
		seen[name_id] = true
	}
	mut late_names := []u32{}
	for name_id in impl_names {
		if name_id in seen {
			continue
		}
		seen[name_id] = true
		late_names << name_id
	}
	late_names.sort_with_compare(compare_interface_impl_spelling_ids)
	assign_stable_interface_type_ids(mut ids, mut used, late_names)
	return ids
}

fn assign_stable_interface_type_ids(mut ids map[u32]int, mut used map[int]bool, names []u32) {
	for name_id in names {
		if name_id in ids {
			continue
		}
		mut id := interface_dispatch_id(stable_interface_type_id_hash(flat.source_type_name(name_id)))
		for used[id] {
			id = interface_dispatch_id(if id == 0x7fffffff { 1 } else { id + 1 })
		}
		used[id] = true
		ids[name_id] = id
	}
}

// interface_dispatch_id_marker is the byte that runtime type indexes reserve for
// pointer indirections (see stable_type_index). A value type's index leaves it zero.
pub const interface_dispatch_id_marker = 0xff << 16

// interface_dispatch_id keeps every interface `_typ` dispatch ID disjoint from
// value type indexes by setting a bit of interface_dispatch_id_marker. A boxed
// container that has no dispatch ID stores its type index as the tag instead, so
// `type_idx()` can still recover it from the 31-bit `_typ` field.
fn interface_dispatch_id(id int) int {
	return if id & interface_dispatch_id_marker == 0 { id | (1 << 16) } else { id }
}

fn compare_interface_impl_spelling_ids(a &u32, b &u32) int {
	left := flat.source_type_name(*a)
	right := flat.source_type_name(*b)
	if left < right {
		return -1
	}
	if left > right {
		return 1
	}
	return 0
}

// interface_impl_set_signature returns the complete deterministic interface implementer set
// that controls collision-resolved dispatch IDs for the current program: a line
// `name=impl,impl` for every interface.
// The module cache keys the object of a module by the interfaces that the code of that module
// can reach, and needs two more facts for it, which follow as lines of their own:
// `#module name=module` for an interface or an implementer whose name has no module prefix
// (`builtin` and the program share that spelling), and `#reach impl=name,name` for the
// interfaces among the fields of an implementer.
pub fn (tc &TypeChecker) interface_impl_set_signature() string {
	mut iface_names := tc.interface_names.keys()
	iface_names.sort_with_compare(compare_interface_impl_spelling_ids)
	mut lines := []string{cap: iface_names.len}
	mut bare_names := []u32{}
	mut impl_seen := map[u32]bool{}
	mut reach_lines := []string{}
	mut seen := map[u32]bool{}
	mut found := map[u32]bool{}
	for iface_name in iface_names {
		iface_name_id := iface_name
		impl_names := if iface_name in [ierror_source_interface_name,
			ierror_source_builtin_interface_name] {
			tc.ierror_impl_names()
		} else {
			tc.interface_impl_names(iface_name_id)
		}
		lines << '${flat.source_type_name(iface_name)}=${impl_names.map(flat.source_type_name(it)).join(',')}'
		if !impl_seen[iface_name_id] {
			impl_seen[iface_name_id] = true
			if !flat.source_type_shape(flat.source_type_syntax(iface_name_id).generic_base()).has(.dotted) {
				bare_names << iface_name_id
			}
		}
		for impl_name_id in impl_names {
			if impl_seen[impl_name_id] {
				continue
			}
			impl_seen[impl_name_id] = true
			if !flat.source_type_shape(flat.source_type_syntax(impl_name_id).generic_base()).has(.dotted) {
				bare_names << impl_name_id
			}
			if impl_name_id in tc.interface_names {
				continue
			}
			seen.clear()
			tc.collect_interfaces_in_type_structure(tc.parse_type(impl_name_id), mut seen, mut
				found)
			if found.len > 0 {
				mut reached := found.keys()
				reached.sort_with_compare(compare_interface_impl_spelling_ids)
				reach_lines << '#reach ${flat.source_type_name(impl_name_id)}=${reached.map(flat.source_type_name(it)).join(',')}'
				found.clear()
			}
		}
	}
	bare_names.sort_with_compare(compare_interface_impl_spelling_ids)
	for name_id in bare_names {
		name := flat.source_type_name(name_id)
		module_name := if visibility := tc.declaration_visibility[flat.source_type_name(flat.source_type_syntax(name_id).generic_base())] {
			if visibility.module_name == '' { 'main' } else { visibility.module_name }
		} else {
			'?'
		}
		lines << '#module ${name}=${module_name}'
	}
	reach_lines.sort()
	lines << reach_lines
	return lines.join('\n')
}

// collect_interfaces_in_type_structure adds to `found` every interface that a value of
// `typ` can hold in one of its fields, elements or variants, at any depth.
fn (tc &TypeChecker) collect_interfaces_in_type_structure(typ Type, mut seen map[u32]bool, mut found map[u32]bool) {
	match typ {
		Alias {
			tc.collect_interfaces_in_type_structure(typ.base_type, mut seen, mut found)
		}
		Pointer {
			tc.collect_interfaces_in_type_structure(typ.base_type, mut seen, mut found)
		}
		OptionType {
			tc.collect_interfaces_in_type_structure(typ.base_type, mut seen, mut found)
		}
		ResultType {
			tc.collect_interfaces_in_type_structure(typ.base_type, mut seen, mut found)
		}
		Array {
			tc.collect_interfaces_in_type_structure(typ.elem_type, mut seen, mut found)
		}
		ArrayFixed {
			tc.collect_interfaces_in_type_structure(typ.elem_type, mut seen, mut found)
		}
		Channel {
			tc.collect_interfaces_in_type_structure(typ.elem_type, mut seen, mut found)
		}
		Map {
			tc.collect_interfaces_in_type_structure(typ.key_type, mut seen, mut found)
			tc.collect_interfaces_in_type_structure(typ.value_type, mut seen, mut found)
		}
		MultiReturn {
			for part in typ.types {
				tc.collect_interfaces_in_type_structure(part, mut seen, mut found)
			}
		}
		Struct {
			if seen[typ.source_type_id()] {
				return
			}
			seen[typ.source_type_id()] = true
			for field in tc.struct_fields_for_type(typ.source_type_id()) {
				tc.collect_interfaces_in_type_structure(field.typ, mut seen, mut found)
			}
		}
		Interface {
			name_id := tc.interface_metadata_name(typ.source_type_id())
			found[name_id] = true
			if seen[name_id] {
				return
			}
			seen[name_id] = true
			for field in tc.interface_fields[name_id] or { []StructField{} } {
				tc.collect_interfaces_in_type_structure(field.typ, mut seen, mut found)
			}
		}
		SumType {
			if seen[typ.source_type_id()] {
				return
			}
			seen[typ.source_type_id()] = true
			base := tc.sum_base_name(typ.source_type_id())
			for variant_source_id_3 in tc.sum_types.type_ids(base) or { []u32{} } {
				variant_source_id_5 := variant_source_id_3

				variant_type := tc.parse_type(tc.concrete_sum_variant_name(typ.source_type_id(), variant_source_id_5))
				tc.collect_interfaces_in_type_structure(variant_type, mut seen, mut found)
			}
		}
		else {}
	}
}

// interface_concrete_method_keys returns generated interface dispatch methods and
// concrete method declarations that can be called by those dispatch functions.
pub fn (tc &TypeChecker) interface_concrete_method_keys() []string {
	mut iface_names := tc.interface_names.keys()
	iface_names.sort_with_compare(compare_interface_impl_spelling_ids)
	mut keys := []string{}
	mut seen := map[string]bool{}
	for iface_name in iface_names {
		iface_name_id := iface_name
		impl_ids := if iface_name in [ierror_source_interface_name,
			ierror_source_builtin_interface_name] {
			tc.ierror_impl_names()
		} else {
			tc.interface_impl_names(iface_name_id)
		}
		for method in tc.interface_abstract_method_names(iface_name_id) {
			dispatch_key := '${flat.source_type_name(iface_name)}.${method}'
			if dispatch_key !in seen {
				seen[dispatch_key] = true
				keys << dispatch_key
			}
			for concrete_id in impl_ids {
				key := tc.concrete_method_signature_key(concrete_id, method) or { continue }
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
	for c in name {
		hash = (hash ^ u32(c)) * u32(16777619)
	}
	id := int(hash & u32(0x7fffffff))
	return if id == 0 { 1 } else { id }
}

// stable_type_index returns the byte-hashed runtime-index seed for a source spelling ID.
// Use stable_type_indexes when assigning indexes across a complete program type set.
pub fn stable_type_index(name_id u32) int {
	mut type_idx := stable_interface_type_id_hash(flat.source_type_name(name_id)) & ~(0xff << 16)
	if type_idx < 65536 {
		// Bit 24 is the first available bit above the reserved indirection byte.
		type_idx |= 1 << 24
	}
	return type_idx
}

// stable_type_indexes assigns deterministic, collision-free runtime indexes to
// the complete caller-supplied source spelling ID set.
pub fn stable_type_indexes(type_names []u32) map[u32]int {
	mut indexes := map[u32]int{}
	extend_stable_type_indexes(mut indexes, type_names)
	return indexes
}

// extend_stable_type_indexes assigns deterministic, collision-free runtime indexes
// to new source spelling IDs without changing indexes already used during lowering.
pub fn extend_stable_type_indexes(mut indexes map[u32]int, type_names []u32) {
	extend_stable_type_indexes_ref(mut indexes, type_names)
}

// extend_stable_type_indexes_ref updates the detached index table through the native pointer ABI.
pub fn extend_stable_type_indexes_ref(mut indexes map[u32]int, type_names []u32) {
	mut names := []u32{}
	for raw_name_id in type_names {
		name_id := trimmed_source_type_id(raw_name_id)
		if name_id != 0 && name_id !in indexes {
			names << raw_name_id
		}
	}
	if names.len == 0 {
		return
	}
	names.sort_with_compare(compare_interface_impl_spelling_ids)
	indexes = indexes.clone()
	mut used := map[int]bool{}
	for _, type_idx in indexes {
		used[type_idx] = true
	}
	for raw_name_id in names {
		name_id := trimmed_source_type_id(raw_name_id)
		if name_id in indexes {
			continue
		}
		mut type_idx := stable_type_index(name_id)
		for used[type_idx] {
			type_idx = next_stable_type_index(type_idx)
		}
		indexes[name_id] = type_idx
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

// runtime_type_index_names returns canonical program source spelling IDs for
// runtime interface/sum type_idx() lowering.
pub fn (tc &TypeChecker) runtime_type_index_names() []u32 {
	mut names := []u32{}
	if !isnil(tc.type_interner) {
		tc.type_interner.lock.lock()
		for index, typ in tc.type_interner.types {
			cached := tc.type_interner.names[index]
			name := if cached != 0 { cached } else { typ.source_type_id() }
			names << name
		}
		tc.type_interner.lock.unlock()
	}
	for name, _ in tc.structs {
		names << name
	}
	for name, variants_id in tc.sum_types.ids {
		names << name
		for variant_id in variants_id {
			names << variant_id
		}
	}
	for name, _ in tc.enum_names {
		names << name
	}
	for name, _ in tc.type_aliases.ids {
		names << name
	}
	for name, _ in tc.interface_names {
		names << name
	}
	for _, index in tc.interface_impl_indexes {
		for name_id in index.names.ids {
			names << name_id
		}
	}
	return names
}

// interface_accepts_implicit_str checks the str requirement of an interface spelling ID.
pub fn (tc &TypeChecker) interface_accepts_implicit_str(iface_name_id u32) bool {
	for method in tc.interface_abstract_method_names(iface_name_id) {
		if method != 'str' {
			continue
		}
		expected_key := tc.interface_method_signature_key(iface_name_id, method) or {
			'${flat.source_type_name(iface_name_id)}.${method}'
		}
		if tc.interface_method_is_str_requirement(expected_key) {
			return true
		}
	}
	return false
}

fn implicit_str_builtin_type_names() []u32 {
	return [flat.source_type_bool, flat.source_type_int, flat.source_type_i8, flat.source_type_i16,
		flat.source_type_i32, flat.source_type_i64, flat.source_type_isize, flat.source_type_uint,
		flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
		flat.source_type_f32, flat.source_type_f64, flat.source_type_string, flat.source_type_rune]
}

fn interface_impl_candidate_name(name_id u32) u32 {
	if !flat.source_type_shape(name_id).has(.dotted) {
		return name_id
	}
	syntax := flat.source_type_syntax(name_id)
	return if syntax.builtin_prefix() { syntax.builtin_base() } else { name_id }
}

// ierror_impl_names returns source spelling IDs of concrete structs that can be boxed as `IError`.
pub fn (tc &TypeChecker) ierror_impl_names() []u32 {
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		if cache.ierror_impl_names_set {
			return cache.ierror_impl_names.ids.clone()
		}
		mut fallback := cache.base
		for !isnil(fallback) {
			if fallback.ierror_impl_names_set {
				return fallback.ierror_impl_names.ids.clone()
			}
			fallback = fallback.base
		}
	}
	mut struct_names := []u32{}
	for name, _ in tc.structs {
		struct_names << name
	}
	struct_names.sort_with_compare(compare_interface_impl_spelling_ids)
	mut impls := []u32{}
	for name in struct_names {
		if tc.named_type_compatible_with_ierror(name) {
			impls << name
		}
	}
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		cache.ierror_impl_names = flat.TypeTextList{ ids: impls.clone() }
		cache.ierror_impl_names_set = true
	}
	return impls
}

// concrete_method_signature_key finds a method declaration key for a source spelling ID.
pub fn (tc &TypeChecker) concrete_method_signature_key(concrete_name_id u32, method string) ?string {
	mut seen_aliases := map[u32]bool{}
	return tc.concrete_method_signature_key_seen(concrete_name_id, method, mut seen_aliases)
}

fn (tc &TypeChecker) concrete_method_signature_key_seen(concrete_name_id u32, method string, mut seen_aliases map[u32]bool) ?string {
	if seen_aliases[concrete_name_id] {
		return none
	}
	seen_aliases[concrete_name_id] = true
	// Method keys stay `Type.method` text; only their receiver spelling is decoded.
	key := '${flat.source_type_name(concrete_name_id)}.${method}'
	if key in tc.fn_param_types || key in tc.fn_ret_types {
		return key
	}
	qualified_id := tc.memo_qualify_type_name(concrete_name_id)
	if !tc.type_aliases.contains(concrete_name_id) && qualified_id != concrete_name_id {
		qualified_key := '${flat.source_type_name(qualified_id)}.${method}'
		if qualified_key in tc.fn_param_types || qualified_key in tc.fn_ret_types {
			return qualified_key
		}
	}
	// The current module can make parse_type(`Connection`) select the imported
	// `orm.Connection` interface even when the implementation candidate is a
	// same-named alias from another module. Consult the alias table by its
	// collected concrete name before doing suffix-based method lookup.
	for alias_target_id in tc.concrete_alias_targets(concrete_name_id, qualified_id) {
		if inherited := tc.concrete_method_signature_key_seen(alias_target_id, method, mut
			seen_aliases)
		{
			return inherited
		}
	}
	receiver_type := tc.memo_parse_type(concrete_name_id)
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
		for candidate in receiver_method_name_candidates(receiver_type.base_type, method, tc.cur_module) {
			if candidate in tc.fn_param_types || candidate in tc.fn_ret_types {
				return candidate
			}
			if indexed := tc.receiver_method_suffix_index[candidate] {
				if indexed != receiver_method_suffix_ambiguous
					&& tc.suffix_indexed_method_fits_receiver(receiver_type.base_type, indexed) {
					return indexed
				}
			}
		}
	}
	for candidate in receiver_candidates {
		if indexed := tc.receiver_method_suffix_index[candidate] {
			if indexed != receiver_method_suffix_ambiguous
				&& tc.suffix_indexed_method_fits_receiver(receiver_type, indexed) {
				return indexed
			}
		}
	}
	for candidate in tc.concrete_generic_method_signature_candidates(concrete_name_id, method) {
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
		if indexed != receiver_method_suffix_ambiguous
			&& tc.suffix_indexed_method_fits_receiver(receiver_type, indexed) {
			return indexed
		}
	}
	if info := tc.embedded_method_call_info(concrete_name_id, method) {
		if info.name.len > 0 {
			return info.name
		}
	}
	return none
}

// memo_qualify_type_name is qualify_type_name(name_id).
fn (tc &TypeChecker) memo_qualify_type_name(name_id u32) u32 {
	mut memo := tc.interface_impl_memo
	if isnil(memo) {
		return tc.qualify_type_name(name_id)
	}
	if qualified_id := memo.qualified_names[name_id] {
		return qualified_id
	}
	qualified_id := tc.qualify_type_name(name_id)
	memo.qualified_names[name_id] = qualified_id
	return qualified_id
}

// concrete_alias_targets lists the types whose methods concrete_method_signature_key
// looks up for an alias `concrete_name_id`, or its qualified spelling `qualified_id`,
// before its own.
fn (tc &TypeChecker) concrete_alias_targets(concrete_name_id u32, qualified_id u32) []u32 {
	mut memo := tc.interface_impl_memo
	if !isnil(memo) {
		if targets := memo.alias_targets[concrete_name_id] {
			return targets
		}
	}
	mut targets := []u32{}
	for alias_id in [concrete_name_id, qualified_id] {
		if alias_target_source_id := tc.type_aliases.type_id(alias_id) {
			// An alias of a reference (`type Refs = &&Names`) has the methods of the
			// referenced type, which a call reaches through automatic dereferencing.
			alias_target_id := if flat.source_type_shape(alias_target_source_id).has(.pointer) {
				flat.source_type_syntax(alias_target_source_id).pointer_raw_base()
			} else {
				alias_target_source_id
			}
			if alias_target_id != alias_id && alias_target_id != concrete_name_id {
				targets << alias_target_id
			}
		}
	}
	if !isnil(memo) {
		memo.alias_targets[concrete_name_id] = targets
	}
	return targets
}

// memo_parse_type is parse_type(name_id).
fn (tc &TypeChecker) memo_parse_type(name_id u32) &Type {
	mut memo := tc.interface_impl_memo
	if isnil(memo) {
		return tc.parse_type(name_id)
	}
	if typ := memo.parsed_types[name_id] {
		return typ
	}
	typ := tc.parse_type(name_id)
	memo.parsed_types[name_id] = typ
	return typ
}

// concrete_generic_method_signature_candidates returns the registered spellings of a specialized method.
pub fn (tc &TypeChecker) concrete_generic_method_signature_candidates(concrete_name_id u32, method string) []string {
	syntax := flat.source_type_syntax(concrete_name_id)
	if !syntax.application_valid() || syntax.application_args().len == 0 || method == '' {
		return []string{}
	}
	short_args := generic_type_args_short_for_signature(syntax.application_args())
	suffix := generic_type_suffix_for_signature(syntax.application_args())
	base := flat.source_type_name(syntax.application_base())
	short_base := flat.source_type_name(flat.source_type_syntax(syntax.application_base()).unqualified())
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

fn generic_type_args_short_for_signature(args []u32) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << generic_type_arg_short_for_signature(arg)
	}
	return parts.join(', ')
}

fn generic_type_suffix_for_signature(args []u32) string {
	mut parts := []string{cap: args.len}
	for arg in args {
		parts << naming.c_name(generic_type_arg_short_for_signature(arg).replace('[]', 'Array_').replace('&', 'ptr_'))
	}
	return parts.join('_')
}

fn generic_type_arg_short_for_signature(type_arg_id u32) string {
	clean_id := trimmed_source_type_id(type_arg_id)
	shape := flat.source_type_shape(clean_id)
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.array_type) {
		return 'Array_${generic_type_arg_short_for_signature(syntax.raw_base())}'
	}
	if shape.has(.pointer) {
		return 'ptr_${generic_type_arg_short_for_signature(syntax.raw_base())}'
	}
	if shape.has(.map_type) {
		if syntax.bracket_closed() && syntax.bracket_base() != 0 {
			key := generic_type_arg_short_for_signature(syntax.bracket_inner())
			value := generic_type_arg_short_for_signature(syntax.bracket_base())
			return 'Map_${key}_${value}'
		}
	}
	if shape.has(.option_type) {
		return 'Option_${generic_type_arg_short_for_signature(syntax.raw_base())}'
	}
	if shape.has(.result_type) {
		return 'Result_${generic_type_arg_short_for_signature(syntax.raw_base())}'
	}
	if syntax.application_valid() {
		mut parts := [generic_type_arg_short_for_signature(syntax.application_base())]
		for arg in syntax.application_args() {
			parts << generic_type_arg_short_for_signature(arg)
		}
		return parts.join('_')
	}
	return flat.source_type_name(clean_id)
}

// named_type_compatible_with_ierror checks a source spelling ID against the IError requirements.
pub fn (tc &TypeChecker) named_type_compatible_with_ierror(concrete_name_id u32) bool {
	cache_key := tc.ierror_compat_cache_key(concrete_name_id)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		mut fallback := cache.base
		for !isnil(fallback) {
			if cached := fallback.cached_ierror_compat_entry(cache_key) {
				return cached > 0
			}
			fallback = fallback.base
		}
		if cached := cache.cached_ierror_compat_entry(cache_key) {
			return cached > 0
		}
	}
	mut seen := map[u32]bool{}
	result := tc.named_type_compatible_with_ierror_inner(concrete_name_id, mut seen)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		cache.remember_ierror_compat_entry(cache_key, if result { 1 } else { -1 })
	}
	return result
}

fn type_query_scope_entry(entries map[u32]&TypeQueryScopeEntries, key SourceTypeQueryKey) ?int {
	contexts := entries[key.owner_id] or { return none }
	return contexts.values[key.scope] or { return none }
}

fn remember_type_query_scope_entry(mut entries map[u32]&TypeQueryScopeEntries, key SourceTypeQueryKey, value int) {
	mut contexts := entries[key.owner_id] or {
		created := &TypeQueryScopeEntries{}
		entries[key.owner_id] = created
		created
	}
	contexts.values[key.scope] = value
}

fn clear_type_query_scope_entries(mut entries map[u32]&TypeQueryScopeEntries) {
	for owner_id, _ in entries {
		unsafe { entries[owner_id].values.free() }
	}
	entries.clear()
}

fn free_type_query_scope_entries(mut entries map[u32]&TypeQueryScopeEntries) {
	clear_type_query_scope_entries(mut entries)
	unsafe { entries.free() }
}

fn (cache &TypeCache) cached_ierror_compat_entry(key SourceTypeQueryKey) ?int {
	if key.scope.len > 0 {
		return type_query_scope_entry(cache.ierror_scoped_compat_entries, key)
	}
	return cache.ierror_compat_entries[key.owner_id] or { return none }
}

fn (mut cache TypeCache) remember_ierror_compat_entry(key SourceTypeQueryKey, value int) {
	if key.scope.len > 0 {
		remember_type_query_scope_entry(mut cache.ierror_scoped_compat_entries, key, value)
	} else {
		cache.ierror_compat_entries[key.owner_id] = value
	}
}

fn (mut cache TypeCache) clear_ierror_compat_entries() {
	cache.ierror_compat_entries.clear()
	clear_type_query_scope_entries(mut cache.ierror_scoped_compat_entries)
}

fn (mut cache TypeCache) free_ierror_compat_entries() {
	unsafe { cache.ierror_compat_entries.free() }
	free_type_query_scope_entries(mut cache.ierror_scoped_compat_entries)
}

fn (mut cache TypeCache) clear_source_error_embed_entries() {
	if cache.source_error_embed_shared {
		// Frozen source indexes are borrowed by workers; only detach their view.
		cache.source_error_embed_entries = map[u32]&TypeQueryScopeEntries{}
	} else {
		clear_type_query_scope_entries(mut cache.source_error_embed_entries)
	}
	cache.source_error_embed_shared = false
	cache.source_error_embed_indexed = false
}

fn (mut cache TypeCache) free_source_error_embed_entries() {
	cache.clear_source_error_embed_entries()
	unsafe { cache.source_error_embed_entries.free() }
}

fn (tc &TypeChecker) ierror_compat_cache_key(concrete_name_id u32) SourceTypeQueryKey {
	if concrete_name_id in [ierror_source_error_name, ierror_source_message_error_name] {
		return source_error_embed_entry_key(concrete_name_id, tc.cur_file, tc.cur_module)
	}
	if concrete_name_id in tc.structs {
		return SourceTypeQueryKey{ owner_id: concrete_name_id }
	}
	qualified_id := tc.qualify_type_name(concrete_name_id)
	if qualified_id in tc.structs {
		return SourceTypeQueryKey{ owner_id: qualified_id }
	}
	return SourceTypeQueryKey{ owner_id: concrete_name_id }
}

// resolve_ierror_payload_name resolves scoped `Error`/`MessageError` names before
// falling back to the builtin error structs.
pub fn (tc &TypeChecker) resolve_ierror_payload_name(name_id u32) u32 {
	if name_id !in [ierror_source_error_name, ierror_source_message_error_name] {
		return name_id
	}
	if resolved_id := tc.resolve_selective_import_type_symbol_id(name_id) {
		if resolved_id in tc.structs {
			return resolved_id
		}
	}
	if tc.cur_module.len > 0 && tc.cur_module != 'main' && tc.cur_module != 'builtin' {
		// Probe `module.Name` without interning a spelling that names no struct.
		local := '${tc.cur_module}.${flat.source_type_name(name_id)}'
		if local_id := flat.source_type_existing_id(local) {
			if local_id in tc.structs {
				return local_id
			}
		}
	}
	return name_id
}

fn (tc &TypeChecker) type_compatible_with_ierror_payload(actual &Type) bool {
	clean := tc.ierror_payload_concrete_type(actual)
	concrete_name_id := method_type_name(clean)
	if concrete_name_id == 0 {
		return false
	}
	return tc.named_type_compatible_with_ierror(concrete_name_id)
}

fn (tc &TypeChecker) ierror_payload_concrete_type(t Type) Type {
	mut clean := t
	mut seen := map[u32]bool{}
	for {
		clean = unwrap_pointer(clean)
		if clean is Alias {
			if seen[clean.source_type_id()] {
				return clean
			}
			seen[clean.source_type_id()] = true
			clean = clean.base_type
			continue
		}
		return clean
	}
	return clean
}

fn (tc &TypeChecker) named_type_compatible_with_ierror_inner(concrete_name_id u32, mut seen map[u32]bool) bool {
	mut lookup_id := tc.resolve_ierror_payload_name(concrete_name_id)
	if lookup_id !in tc.structs {
		qualified_id := tc.qualify_type_name(lookup_id)
		if qualified_id in tc.structs {
			lookup_id = qualified_id
		}
	}
	if lookup_id in [ierror_source_error_name, ierror_source_message_error_name]
		&& tc.unqualified_type_name_shadows_builtin(lookup_id) {
		return false
	}
	if lookup_id in seen {
		return false
	}
	seen[lookup_id] = true
	if tc.is_builtin_error_struct_type_id(lookup_id) {
		return true
	}
	if tc.named_type_has_non_builtin_error_embed(lookup_id) {
		return false
	}
	if tc.named_type_implements_ierror_methods(lookup_id) {
		return true
	}
	struct_module := tc.struct_modules[lookup_id] or { tc.cur_module }
	struct_file := tc.struct_files[lookup_id] or { tc.cur_file }
	for field in tc.structs[lookup_id] or { []StructField{} } {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name_id := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name_id == 0 {
			continue
		}
		if tc.embedded_field_is_scoped_builtin_error(field, embedded_name_id, struct_file, struct_module) {
			return true
		}
		if embedded_field_names_bare_error(field.name, embedded_name_id) {
			continue
		}
		if tc.is_builtin_error_struct_type_id(embedded_name_id) {
			return true
		}
		if tc.named_type_compatible_with_ierror_inner(embedded_name_id, mut seen) {
			return true
		}
	}
	return false
}

// embedded_field_names_bare_error reports whether a field named `field_name` embeds
// the bare `Error` or `MessageError` spelling `name_id` under its own name.
fn embedded_field_names_bare_error(field_name string, name_id u32) bool {
	return (name_id == ierror_source_error_name && field_name == 'Error')
		|| (name_id == ierror_source_message_error_name && field_name == 'MessageError')
}

fn (tc &TypeChecker) named_type_implements_ierror_methods(concrete_name_id u32) bool {
	// Without the builtin module (`-no-builtin`) no `IError` is declared at all.
	// An undeclared interface lists no requirements, so every type would satisfy
	// it, and plain values like enums or ints would pass for error payloads. Only
	// the exact global names count: a module-local `pkg.IError` would otherwise be
	// picked up by the short-name fallback of `interface_metadata_name`, but it is
	// not the builtin error protocol.
	iface_name_id := if ierror_source_builtin_interface_name in tc.interface_names {
		ierror_source_builtin_interface_name
	} else if ierror_source_interface_name in tc.interface_names {
		ierror_source_interface_name
	} else {
		return false
	}
	return tc.named_type_implements_interface(concrete_name_id, iface_name_id)
}

fn (tc &TypeChecker) named_type_has_non_builtin_error_embed(concrete_name_id u32) bool {
	mut lookup_id := concrete_name_id
	if lookup_id !in tc.structs {
		qualified_id := tc.qualify_type_name(lookup_id)
		if qualified_id in tc.structs {
			lookup_id = qualified_id
		}
	}
	if tc.struct_error_embeds_shadow_builtin[lookup_id] {
		return true
	}
	struct_module := tc.struct_modules[lookup_id] or { tc.cur_module }
	struct_file := tc.struct_files[lookup_id] or { tc.cur_file }
	if tc.source_struct_has_non_builtin_error_embed(lookup_id, struct_file, struct_module) {
		return true
	}
	for field in tc.structs[lookup_id] or { []StructField{} } {
		if field.name in ['Error', 'MessageError']
			&& tc.unqualified_type_name_shadows_builtin_in_scope(field.name, struct_file, struct_module) {
			return true
		}
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name_id := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name_id == 0 {
			continue
		}
		if tc.embedded_field_is_scoped_builtin_error(field, embedded_name_id, struct_file, struct_module) {
			continue
		}
		if embedded_field_names_bare_error(field.name, embedded_name_id) {
			return true
		}
		if flat.source_type_syntax(embedded_name_id).unqualified() in [
			ierror_source_error_name,
			ierror_source_message_error_name,
		]
			&& !tc.is_builtin_error_struct_type_id(embedded_name_id) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) source_struct_has_non_builtin_error_embed(concrete_name_id u32, file string, mod_name string) bool {
	if isnil(tc.a) {
		return false
	}
	key := source_error_embed_lookup_key(concrete_name_id, file, mod_name)
	if tc.type_cache != unsafe { nil } {
		mut cache := unsafe { tc.type_cache }
		if !cache.source_error_embed_indexed {
			mut fallback := cache.base
			for !isnil(fallback) {
				if fallback.source_error_embed_indexed {
					return (type_query_scope_entry(fallback.source_error_embed_entries, key) or { 0 }) > 0
				}
				fallback = fallback.base
			}
			cache.source_error_embed_entries = tc.collect_source_error_embed_entries()
			cache.source_error_embed_indexed = true
		}
		return (type_query_scope_entry(cache.source_error_embed_entries, key) or { 0 }) > 0
	}
	mut entries := tc.collect_source_error_embed_entries()
	result := (type_query_scope_entry(entries, key) or { 0 }) > 0
	free_type_query_scope_entries(mut entries)
	return result
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

fn (tc &TypeChecker) collect_source_error_embed_entries() map[u32]&TypeQueryScopeEntries {
	mut entries := map[u32]&TypeQueryScopeEntries{}
	if isnil(tc.a) {
		return entries
	}
	mut cur_file := ''
	mut cur_module := ''
	mut tail_start := 0
	if tc.top_level_idx.len > 0 && tc.a.nodes.len >= tc.top_level_idx_nodes_len {
		// struct_decl nodes only occur at the top level, and the index visits the
		// file and module nodes exactly as a full scan would. Only nodes appended
		// after collect built the index (by transform) still need scanning.
		for i in tc.top_level_idx {
			node := tc.a.nodes[i]
			cur_file, cur_module = tc.note_source_error_embed(node, cur_file, cur_module, mut
				entries)
		}
		tail_start = tc.top_level_idx_nodes_len
	}
	tail_ids, use_ids := tc.tail_decl_ids_for(tail_start, tc.a.nodes.len)
	tail_count := if use_ids { tail_ids.len } else { tc.a.nodes.len - tail_start }
	for k in 0 .. tail_count {
		i := if use_ids { int(tail_ids[k]) } else { tail_start + k }
		kind := tc.a.nodes[i].kind
		if kind != .file && kind != .module_decl && kind != .struct_decl {
			continue
		}
		cur_file, cur_module = tc.note_source_error_embed(tc.a.nodes[i], cur_file, cur_module, mut
			entries)
	}
	return entries
}

// note_source_error_embed tracks the file/module context of a declaration scan
// and records struct declarations that embed a non-builtin Error.
fn (tc &TypeChecker) note_source_error_embed(node flat.Node, cur_file string, cur_module string, mut entries map[u32]&TypeQueryScopeEntries) (string, string) {
	match node.kind {
		.file {
			return node.value, ''
		}
		.module_decl {
			return cur_file, node.value
		}
		.struct_decl {
			if tc.source_struct_decl_has_non_builtin_error_embed(node, cur_file, cur_module) {
				// The last segment of a non-empty declaration name; an empty one has none.
				target_id := if node.value.len == 0 {
					u32(0)
				} else {
					scope_unqualified_type_name(node.source_value_id())
				}
				module_key := source_error_embed_module_key(cur_module)
				remember_type_query_scope_entry(mut entries, source_error_embed_entry_key(target_id, '', module_key), 1)
				remember_type_query_scope_entry(mut entries, source_error_embed_entry_key(target_id, cur_file, module_key), 1)
			}
		}
		else {}
	}
	return cur_file, cur_module
}

fn (tc &TypeChecker) source_struct_decl_has_non_builtin_error_embed(node flat.Node, cur_file string, cur_module string) bool {
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		if !source_field_decl_is_embed(field, field_type_id) {
			continue
		}
		if field_type_id in [ierror_source_error_name, ierror_source_message_error_name]
			&& tc.unqualified_type_name_shadows_builtin_in_scope_id(field_type_id, cur_file, cur_module) {
			return true
		}
		resolved_id := tc.resolve_imported_type_text_in_file(field_type_id, cur_file)
		if flat.source_type_syntax(resolved_id).unqualified() in [
			ierror_source_error_name,
			ierror_source_message_error_name,
		]
			&& !tc.is_builtin_error_struct_type_id_in_scope(resolved_id, cur_file, cur_module) {
			return true
		}
	}
	return false
}

fn source_error_embed_lookup_key(concrete_name_id u32, file string, mod_name string) SourceTypeQueryKey {
	syntax := flat.source_type_syntax(concrete_name_id)
	target_id := syntax.unqualified()
	target_module := if flat.source_type_shape(concrete_name_id).has(.dotted) {
		flat.source_type_name(syntax.qualifier())
	} else {
		mod_name
	}
	file_key := if target_module.len > 0 { '' } else { file }
	return source_error_embed_entry_key(target_id, file_key, source_error_embed_module_key(target_module))
}

fn source_error_embed_entry_key(target_id u32, file string, module_key string) SourceTypeQueryKey {
	return SourceTypeQueryKey{
		owner_id: target_id
		// This is only file/module context; source type bytes are never serialized here.
		scope:    '${file}\n${module_key}'
	}
}

fn source_error_embed_module_key(mod_name string) string {
	return if mod_name == 'main' { '' } else { mod_name }
}

fn source_field_decl_is_embed(field flat.Node, field_type_id u32) bool {
	meta := field.generic_params()
	if (meta.len > 0 && meta[0].contains('e')) || field.typ == 0 || field.value.len == 0 {
		return true
	}
	// Preserve the invalid-embed diagnostic for uppercase named fields.
	field_name_id := field.source_value_id()
	return field.value[0].is_capital() && (field_name_id == field_type_id
		|| (flat.source_type_shape(field_type_id).has(.dotted)
			&& field_name_id == flat.source_type_syntax(field_type_id).unqualified()))
}

fn (tc &TypeChecker) embedded_field_is_scoped_builtin_error(field StructField, embedded_name_id u32, struct_file string, struct_module string) bool {
	if !embedded_field_names_bare_error(field.name, embedded_name_id) {
		return false
	}
	return !tc.unqualified_type_name_shadows_builtin_in_scope_id(embedded_name_id, struct_file, struct_module)
}

fn (tc &TypeChecker) resolve_unqualified_builtin_error_struct_name(name string) ?string {
	name_id := match name {
		'Error' { ierror_source_error_name }
		'MessageError' { ierror_source_message_error_name }
		else { return none }
	}
	tc.resolve_unqualified_builtin_error_struct_name_id(name_id) or { return none }
	return name
}

// resolve_unqualified_builtin_error_struct_name_id returns the bare `Error` or
// `MessageError` spelling ID when it names the builtin error struct here.
fn (tc &TypeChecker) resolve_unqualified_builtin_error_struct_name_id(name_id u32) ?u32 {
	if name_id !in [ierror_source_error_name, ierror_source_message_error_name] {
		return none
	}
	if tc.unqualified_type_name_shadows_builtin(name_id) {
		return none
	}
	if mod_name := tc.struct_modules[name_id] {
		if mod_name == 'builtin' {
			return name_id
		}
	}
	if tc.has_builtins {
		return name_id
	}
	return none
}

fn (tc &TypeChecker) unqualified_type_name_shadows_builtin(name_id u32) bool {
	return tc.unqualified_type_name_shadows_builtin_in_scope_id(name_id, tc.cur_file, tc.cur_module)
}

fn (tc &TypeChecker) unqualified_type_name_shadows_builtin_in_scope(name string, file string, mod_name string) bool {
	return tc.unqualified_type_name_shadows_builtin_in_scope_id(flat.source_type_id(name), file, mod_name)
}

// unqualified_type_name_shadows_builtin_in_scope_id reports whether the bare type
// spelling ID names a declaration of `file`/`mod_name` instead of the builtin one.
fn (tc &TypeChecker) unqualified_type_name_shadows_builtin_in_scope_id(name_id u32, file string, mod_name string) bool {
	module_scoped := mod_name !in ['', 'main', 'builtin']
	if !module_scoped && mod_name != 'builtin'
		&& tc.source_declares_type_in_scope(name_id, file, mod_name) {
		return true
	}
	// The `module.Name` probe uses symbol text; it must not intern a spelling
	// that names no type.
	name := flat.source_type_name(name_id)
	if module_scoped && tc.type_symbol_spelling_known('${mod_name}.${name}') {
		return true
	}
	if _ := tc.file_selective_import_ids(file, name_id) {
		if resolved_id := tc.resolve_selective_import_type_symbol_in_file_id(name_id, file) {
			return !tc.is_builtin_error_struct_type_id(resolved_id)
		}
		return true
	}
	if module_scoped {
		return false
	}
	if name_id in tc.structs {
		return tc.struct_modules[name_id] or { '' } != 'builtin'
	}
	if tc.type_aliases.contains(name_id) || tc.sum_types.contains(name_id) || name_id in tc.interface_names
		|| name_id in tc.enum_names {
		return true
	}
	return false
}

fn (tc &TypeChecker) source_declares_type_in_scope(name_id u32, file string, mod_name string) bool {
	if file == '' || isnil(tc.a) {
		return false
	}
	norm_mod := if mod_name == '' || mod_name == 'main' { 'main' } else { mod_name }
	context_id := flat.source_type_existing_id('${file}\x01${norm_mod}') or { return false }
	key := (u64(context_id) << 32) | u64(name_id)
	return key in tc.declared_type_scope_keys
}

// scope_type_key builds the lookup key used by `declared_type_scope_keys`.
// The module is normalized so that '' and 'main' collapse to the same bucket,
// matching the old `module_names_match` semantics.
fn scope_type_key(file string, mod_name string, name_id u32) u64 {
	norm_mod := if mod_name == '' || mod_name == 'main' { 'main' } else { mod_name }
	context_id := flat.source_type_id('${file}\x01${norm_mod}')
	return (u64(context_id) << 32) | u64(name_id)
}

// scope_unqualified_type_name keeps plain declaration names out of the syntax cache.
fn scope_unqualified_type_name(name_id u32) u32 {
	return if flat.source_type_shape(name_id).has(.dotted) {
		flat.source_type_syntax(name_id).unqualified()
	} else {
		name_id
	}
}

// qualify_type_name_at resolves an interned bare type identifier in a source node's caller scope,
// preserving enclosing generic parameters and consulting only that file's selective imports.
pub fn (tc &TypeChecker) qualify_type_name_at(name_id u32, id flat.NodeId, module_name string) u32 {
	name := flat.source_type_name(name_id)
	if flat.source_type_shape(name_id).has(.dotted) || is_builtin_type_name_id(name_id) || tc.source_enclosing_fn_has_generic_param(id, name) {
		return name_id
	}
	if tc.valid_node_id(id) {
		node := tc.a.node(id)
		if file := tc.a.source_files[node.pos.id] {
			if resolved_id := tc.resolve_selective_import_type_symbol_in_file_id(name_id, file.name) {
				return resolved_id
			}
		}
	}
	if module_name in ['', 'main'] && tc.type_symbol_known(name_id) {
		owner := tc.struct_modules[name_id] or { tc.type_alias_modules[name_id] or { '' } }
		if owner in ['', 'main'] && tc.main_type_name_has_non_main_collision(name_id) {
			return flat.source_type_composition_two(.qualified, checker_source_main_module, name_id)
		}
	}
	if module_name !in ['', 'main', 'builtin'] {
		qualified := '${module_name}.${name}'
		if qualified_id := flat.source_type_existing_id(qualified) {
			if tc.type_symbol_known(qualified_id) {
				return qualified_id
			}
		}
	}
	return name_id
}

fn (tc &TypeChecker) main_type_name_has_non_main_collision(name_id u32) bool {
	mut seen := map[u32]bool{}
	return tc.main_type_name_has_non_main_collision_inner(name_id, mut seen)
}

fn (tc &TypeChecker) main_type_name_has_non_main_collision_inner(name_id u32, mut seen map[u32]bool) bool {
	if seen[name_id] {
		return false
	}
	seen[name_id] = true
	if target_source_id := tc.type_aliases.type_id(name_id) {
		if tc.main_alias_target_has_non_main_collision(target_source_id, mut seen) {
			return true
		}
	}
	if _ := tc.unique_qualified_type_name(name_id) {
		return false
	}
	for candidate, _ in tc.structs {
		if type_name_collides_with_main(candidate, name_id) { return true }
	}
	for candidate, _ in tc.sum_types.ids {
		if type_name_collides_with_main(candidate, name_id) { return true }
	}
	for candidate, _ in tc.enum_names {
		if type_name_collides_with_main(candidate, name_id) { return true }
	}
	for candidate, _ in tc.type_aliases.ids {
		if type_name_collides_with_main(candidate, name_id) { return true }
	}
	for candidate, _ in tc.interface_names {
		if type_name_collides_with_main(candidate, name_id) { return true }
	}
	return false
}

fn (tc &TypeChecker) main_alias_target_has_non_main_collision(target_id u32, mut seen map[u32]bool) bool {
	syntax := flat.source_type_syntax(target_id)
	for index, part_id in syntax.symbol_parts() {
		if !syntax.symbol_is_name()[index] || flat.source_type_shape(part_id).has(.dotted)
			|| is_builtin_type_name_id(part_id) {
			continue
		}
		if !tc.type_symbol_known(part_id) { continue }
		owner := tc.struct_modules[part_id] or { tc.type_alias_modules[part_id] or { '' } }
		if owner in ['', 'main'] && tc.main_type_name_has_non_main_collision_inner(part_id, mut seen) {
			return true
		}
	}
	return false
}

fn type_name_collides_with_main(candidate_id u32, name_id u32) bool {
	if !flat.source_type_shape(candidate_id).has(.dotted) { return false }
	syntax := flat.source_type_syntax(candidate_id)
	return syntax.unqualified() == name_id
		&& syntax.qualifier() !in [u32(0), checker_source_main_module, checker_source_builtin]
}

// specialize_generic_factory_return substitutes explicit method and receiver arguments
// in one pass, preserving caller parameters that share a declaration parameter's name.
pub fn (tc &TypeChecker) specialize_generic_factory_return(return_type_id u32, name string, receiver_type_id u32, explicit_args_ids []u32) u32 {
	method_params := tc.fn_generic_params[name] or { []string{} }
	mut actual_receiver := tc.generic_struct_method_alias_target(comptime_static_unwrap_type_id(receiver_type_id))
	mut receiver_pattern_id := flat.source_type_id(name.all_before_last('.'))
	if !flat.source_type_syntax(receiver_pattern_id).application_valid() {
		if texts := tc.fn_param_type_texts.type_ids(name) {
			if texts.len > 0 {
				receiver_pattern_id = comptime_static_unwrap_type_id(texts[0])
			}
		}
	}
	pattern := flat.source_type_syntax(receiver_pattern_id)
	if pattern.application_valid() {
		if promoted := tc.promoted_generic_factory_receiver(actual_receiver, pattern.application_base()) {
			actual_receiver = promoted
		}
	}
	method_args := if method_params.len > 0 {
		tc.explicit_generic_receiver_method_args(actual_receiver, name, explicit_args_ids)
	} else {
		[]u32{}
	}
	if method_params.len > 0 && (method_args.len == 0 || method_args.len > method_params.len) {
		return return_type_id
	}
	mut params := method_params[method_params.len - method_args.len..].clone()
	mut args := method_args.clone()
	if pattern.application_valid() {
		actual := flat.source_type_syntax(actual_receiver)
		if actual.application_valid() && tc.generic_type_base_matches(pattern.application_base(), actual.application_base()) {
			concrete_receiver_args := if method_params.len == 0 && explicit_args_ids.len == actual.application_args().len {
				explicit_args_ids
			} else {
				actual.application_args()
			}
			pattern_key := '${flat.source_type_name(receiver_pattern_id)}.${name.all_after_last('.')}'
			if receiver_params, concrete_args := tc.generic_method_receiver_pattern_args(pattern_key, concrete_receiver_args) {
				for i, param in receiver_params {
					if param !in params {
						params << param
						args << concrete_args[i]
					}
				}
			}
		}
	}
	return subst_generic_text(return_type_id, args, params)
}

fn (tc &TypeChecker) promoted_generic_factory_receiver(receiver_id u32, declaration_base u32) ?u32 {
	mut seen := map[u32]bool{}
	return tc.promoted_generic_factory_receiver_inner(receiver_id, declaration_base, mut seen)
}

fn (tc &TypeChecker) promoted_generic_factory_receiver_inner(receiver_id u32, declaration_base u32, mut seen map[u32]bool) ?u32 {
	syntax := flat.source_type_syntax(receiver_id)
	if syntax.application_valid() && tc.generic_type_base_matches(syntax.application_base(), declaration_base) {
		return receiver_id
	}
	if seen[receiver_id] { return none }
	seen[receiver_id] = true
	for field in tc.struct_fields_for_type(receiver_id) {
		embedded := embedded_field_type(field) or { continue }
		embedded_name := method_type_name(unwrap_pointer(unalias_type(embedded)))
		if embedded_name == 0 { continue }
		if found := tc.promoted_generic_factory_receiver_inner(embedded_name, declaration_base, mut seen) {
			return found
		}
	}
	return none
}

// resolve_selective_import_type_symbol_in_file_id resolves a name selected by
// `file` to the spelling ID of the first candidate that names a collected type.
fn (tc &TypeChecker) resolve_selective_import_type_symbol_in_file_id(name_id u32, file string) ?u32 {
	candidates := tc.file_selective_import_ids(file, name_id) or { return none }
	for candidate_id in candidates {
		if tc.type_symbol_known(candidate_id) {
			return candidate_id
		}
	}
	return none
}

// resolve_imported_type_text_in_file resolves a source spelling ID through the writing file imports.
pub fn (tc &TypeChecker) resolve_imported_type_text_in_file(typ_id u32, file string) u32 {
	if !flat.source_type_shape(typ_id).has(.dotted) || flat.source_type_query(typ_id).has(.c_prefix) {
		return typ_id
	}
	syntax := flat.source_type_syntax(typ_id)
	if syntax.first_dot_prefix() == 0 {
		return typ_id
	}
	alias := flat.source_type_name(syntax.first_dot_prefix())
	if resolved := tc.file_imports[file_import_key(file, alias)] {
		if resolved != alias {
			return flat.source_type_composition_two(.qualified, flat.source_type_id(resolved), syntax.first_dot_suffix())
		}
	}
	return typ_id
}

fn (tc &TypeChecker) is_builtin_error_struct_type_id(name_id u32) bool {
	return tc.is_builtin_error_struct_type_id_in_scope(name_id, tc.cur_file, tc.cur_module)
}

fn (tc &TypeChecker) is_builtin_error_struct_type_id_in_scope(name_id u32, file string, mod_name string) bool {
	if name_id in [ierror_source_builtin_error_name, ierror_source_builtin_message_error_name] {
		return true
	}
	if name_id in [ierror_source_error_name, ierror_source_message_error_name] {
		if tc.unqualified_type_name_shadows_builtin_in_scope_id(name_id, file, mod_name) {
			return false
		}
		if mod := tc.struct_modules[name_id] {
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
	tc.interface_method_names_index = map[u32][]string{}
	tc.interface_abstract_index = map[u32][]string{}
	tc.interface_field_list_index = map[u32][]StructField{}
	mut direct_methods := map[u32][]string{}
	for iface_name in tc.interface_names.keys() {
		name_id := tc.interface_metadata_name(iface_name)
		if name_id !in direct_methods {
			direct_methods[name_id] = []string{}
		}
	}
	// Default interface methods are stored with ordinary function signatures.
	// Index them once instead of scanning the full signature table separately
	// for every interface (and again for each embedded-interface traversal).
	mut receiver_names := map[u32]u32{}
	for key, _ in tc.fn_ret_types {
		dot := key.last_index_u8(`.`)
		if dot <= 0 {
			continue
		}
		raw_receiver_id := flat.source_type_id(key[..dot])
		// Most signatures repeat an ordinary struct receiver. Resolve its
		// interface metadata once for this immutable declaration table.
		receiver_id := receiver_names[raw_receiver_id] or {
			resolved := tc.interface_metadata_name(raw_receiver_id)
			receiver_names[raw_receiver_id] = resolved
			resolved
		}
		mut methods := direct_methods[receiver_id] or { continue }
		method := key[dot + 1..]
		if method !in methods {
			methods << method
			direct_methods[receiver_id] = methods
		}
	}
	for iface_name in tc.interface_names.keys() {
		iface_name_id := iface_name
		mut seen_methods := map[u32]bool{}
		methods := tc.interface_method_names_indexed(iface_name_id, direct_methods, mut seen_methods)
		mut seen_abstract := map[u32]bool{}
		abstract_methods := tc.interface_abstract_method_names_inner(iface_name_id, mut seen_abstract)
		mut seen_fields := map[u32]bool{}
		fields := tc.interface_field_list_inner(iface_name_id, mut seen_fields)
		resolved := tc.interface_metadata_name(iface_name_id)
		for key in [iface_name_id, resolved] {
			if key == 0 {
				continue
			}
			tc.interface_method_names_index[key] = methods
			tc.interface_abstract_index[key] = abstract_methods
			tc.interface_field_list_index[key] = fields
		}
	}
	tc.interface_query_indexes_ready = true
}

fn (tc &TypeChecker) interface_method_names_indexed(iface_name_id u32, direct_methods map[u32][]string, mut seen map[u32]bool) []string {
	name_id := tc.interface_metadata_name(iface_name_id)
	if name_id in seen {
		return []string{}
	}
	seen[name_id] = true
	mut methods := []string{}
	for embed_source_id in tc.interface_embeds.type_ids(name_id) or { []u32{} } {
		for method in tc.interface_method_names_indexed(embed_source_id, direct_methods, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	for method in direct_methods[name_id] or { []string{} } {
		if method !in methods {
			methods << method
		}
	}
	return methods
}

// prepare_interface_query_indexes publishes immutable interface requirements and
// implementer lists for transform workers.
// InterfaceImplMemo keeps what prepare_interface_query_indexes looks up for a
// candidate type again for every interface it tests the type against. Nothing
// it depends on changes during that loop, which runs in one checker context.
@[heap]
struct InterfaceImplMemo {
mut:
	embedded_receivers map[u32][]u32
	qualified_names    map[u32]u32
	alias_targets      map[u32][]u32
	parsed_types       map[u32]&Type
	// base_candidates are the sorted candidates of every interface, and
	// alias_fn_candidates the function types that aliases name (see
	// memo_interface_impl_candidates).
	base_candidates           []u32
	base_candidates_ready     bool
	alias_fn_candidates       []u32
	alias_fn_candidates_ready bool
}

pub fn (mut tc TypeChecker) prepare_interface_query_indexes() {
	tc.prepare_interface_requirement_indexes()
	tc.interface_impl_indexes = map[u32]&InterfaceImplIndex{}
	tc.interface_impl_candidates_at_index = tc.interface_impl_candidate_names()
	iface_names := tc.interface_names.keys()
	if !tc.prepare_interface_impl_indexes_parallel(iface_names) {
		tc.interface_impl_memo = &InterfaceImplMemo{}
		for iface_name in iface_names {
			impls := if is_builtin_ierror_name(iface_name) {
				tc.ierror_impl_names()
			} else {
				tc.interface_impl_names(iface_name)
			}
			tc.interface_impl_indexes[iface_name] = &InterfaceImplIndex{
				names: flat.TypeTextList{ ids: impls }
				ids:   stable_interface_type_ids(impls)
			}
		}
		tc.interface_impl_memo = unsafe { nil }
	}
	// The immutable transform indexes above preserve the pre-monomorphization
	// order. Do not leave the same lists in the mutable query cache: generic
	// materialization can add implementers before cgen queries it again.
	tc.clear_interface_impl_cache()
}

// interface_method_names supports interface method names handling for TypeChecker.
fn (tc &TypeChecker) interface_method_names(iface_name_id u32) []string {
	if tc.interface_query_indexes_ready {
		if methods := tc.interface_method_names_index[iface_name_id] {
			return methods
		}
	}
	mut seen := map[u32]bool{}
	return tc.interface_method_names_inner(iface_name_id, mut seen)
}

// has_ierror_interface reports whether the builtin `IError` interface is declared.
// It is absent with `-no-builtin`: option/result wrappers then carry no `err`
// field, and `or {}` blocks and if-guard `else` branches bind no implicit `err`.
pub fn (tc &TypeChecker) has_ierror_interface() bool {
	return ierror_source_interface_name in tc.interface_names || ierror_source_builtin_interface_name in tc.interface_names
}

// interface_abstract_method_names returns the methods an implementer must provide:
// the interface's own declared (abstract) methods plus those of any embedded
// interfaces. Default methods defined directly on the interface are excluded.
pub fn (tc &TypeChecker) interface_abstract_method_names(iface_name_id u32) []string {
	if tc.interface_query_indexes_ready {
		if methods := tc.interface_abstract_index[iface_name_id] {
			return methods
		}
	}
	mut seen := map[u32]bool{}
	return tc.interface_abstract_method_names_inner(iface_name_id, mut seen)
}

// interface_abstract_method_names_inner supports interface_abstract_method_names_inner handling.
fn (tc &TypeChecker) interface_abstract_method_names_inner(iface_name_id u32, mut seen map[u32]bool) []string {
	name_id := tc.interface_metadata_name(iface_name_id)
	if name_id in seen {
		return []string{}
	}
	seen[name_id] = true
	mut methods := []string{}
	for embed_source_id_2 in tc.interface_embeds.type_ids(name_id) or { []u32{} } {
		for method in tc.interface_abstract_method_names_inner(embed_source_id_2, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	for method in tc.interface_abstract_methods[name_id] or { []string{} } {
		if method !in methods {
			methods << method
		}
	}
	return methods
}

// interface_method_names_inner supports interface method names inner handling for TypeChecker.
fn (tc &TypeChecker) interface_method_names_inner(iface_name_id u32, mut seen map[u32]bool) []string {
	name_id := tc.interface_metadata_name(iface_name_id)
	name := flat.source_type_name(name_id)
	if name_id in seen {
		return []string{}
	}
	seen[name_id] = true
	mut methods := []string{}
	for embed_source_id_3 in tc.interface_embeds.type_ids(name_id) or { []u32{} } {
		for method in tc.interface_method_names_inner(embed_source_id_3, mut seen) {
			if method !in methods {
				methods << method
			}
		}
	}
	prefix := '${name}.'
	params := tc.interface_generic_params[name_id] or { []string{} }
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

// interface_method_signature_key finds a method declaration key for an interface spelling ID.
pub fn (tc &TypeChecker) interface_method_signature_key(iface_name_id u32, method string) ?string {
	name_id := tc.interface_metadata_name(iface_name_id)
	name := flat.source_type_name(name_id)
	key := '${name}.${method}'
	if key in tc.fn_ret_types || key in tc.fn_param_types {
		return key
	}
	params := tc.interface_generic_params[name_id] or { []string{} }
	if params.len > 0 {
		open_key := '${name}[${params.join(', ')}].${method}'
		if open_key in tc.fn_ret_types || open_key in tc.fn_param_types {
			return open_key
		}
	}
	for embed_source_id_4 in tc.interface_embeds.type_ids(name_id) or { []u32{} } {
		if found := tc.interface_method_signature_key(embed_source_id_4, method) {
			return found
		}
	}
	return none
}

fn (tc &TypeChecker) interface_receiver_method_call_info(iface_name_id u32, method string) ?CallInfo {
	metadata_id := tc.interface_metadata_name(iface_name_id)
	if metadata_id !in tc.interface_names {
		return none
	}
	decl_key := tc.interface_method_signature_key(iface_name_id, method) or { return none }
	decl_params, return_type := tc.specialized_interface_method_signature(iface_name_id, decl_key)
	if decl_params.len == 0 {
		return none
	}
	mut params := []Type{cap: decl_params.len}
	params << Type(Pointer{
		base_type: tc.intern_type_reference(Type(Interface{
			name_id: iface_name_id
		}))
	})
	if decl_params.len > 1 {
		for i in 1 .. decl_params.len {
			params << decl_params[i]
		}
	}
	call_name := '${flat.source_type_name(iface_name_id)}.${method}'
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
pub fn (tc &TypeChecker) interface_field_list(iface_name_id u32) []StructField {
	if tc.interface_query_indexes_ready {
		if fields := tc.interface_field_list_index[iface_name_id] {
			return fields
		}
	}
	mut seen := map[u32]bool{}
	return tc.interface_field_list_inner(iface_name_id, mut seen)
}

// interface_field_list_inner supports interface field list inner handling for TypeChecker.
fn (tc &TypeChecker) interface_field_list_inner(iface_name_id u32, mut seen map[u32]bool) []StructField {
	name_id := tc.interface_metadata_name(iface_name_id)
	if name_id in seen {
		return []StructField{}
	}
	seen[name_id] = true
	mut fields := []StructField{}
	mut field_indexes := map[string]int{}
	for embed_source_id_5 in tc.interface_embeds.type_ids(name_id) or { []u32{} } {
		for field in tc.interface_field_list_inner(embed_source_id_5, mut seen) {
			if idx := field_indexes[field.name] {
				fields[idx] = field
			} else {
				field_indexes[field.name] = fields.len
				fields << field
			}
		}
	}
	for field in tc.interface_fields[name_id] or { []StructField{} } {
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
fn (tc &TypeChecker) interface_field_type(iface_name_id u32, field_name string) ?Type {
	for field in tc.interface_field_list(iface_name_id) {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

// struct_field_type supports struct field type handling for TypeChecker.
fn (tc &TypeChecker) struct_init_field_lookup_name(literal_name_id u32, parsed_name_id u32) u32 {
	clean_id := trimmed_source_type_id(literal_name_id)
	if clean_id == 0 {
		return parsed_name_id
	}
	clean_syntax := flat.source_type_syntax(clean_id)
	if clean_syntax.application_valid() {
		if flat.source_type_syntax(parsed_name_id).application_valid() {
			return parsed_name_id
		}
		if clean_syntax.generic_base() != 0 {
			base := if parsed_name_id != 0 {
				parsed_name_id
			} else {
				trimmed_source_type_id(clean_syntax.generic_base())
			}
			return flat.source_type_composition_two(.concat, base, clean_syntax.bracket_from())
		}
	}
	if parsed_name_id != 0 {
		return parsed_name_id
	}
	return clean_id
}

fn (tc &TypeChecker) struct_fields_for_init(struct_name_id u32) []StructField {
	syntax := flat.source_type_syntax(struct_name_id)
	is_generic := syntax.application_valid()
	generic_args := syntax.application_args()
	// Concrete fields survive template erasure and already contain substituted types.
	if is_generic {
		if fields := tc.structs[struct_name_id] {
			return fields
		}
	}
	raw_lookup_id := if is_generic { syntax.application_base() } else { struct_name_id }
	short_lookup_id := flat.source_type_syntax(raw_lookup_id).unqualified()
	if flat.source_type_query(raw_lookup_id).has(.c_prefix) {
		scoped_c_name := c_struct_module_key(tc.cur_module, raw_lookup_id)
		if fields := tc.c_struct_scoped_fields[scoped_c_name] {
			return fields
		}
	}
	lookup_id := if raw_lookup_id in tc.structs {
		raw_lookup_id
	} else if short_lookup_id in tc.structs {
		short_lookup_id
	} else if canonical_source_id := tc.canonical_qualified_type_name(raw_lookup_id) {
		canonical_source_id
	} else {
		raw_lookup_id
	}
	fields := tc.structs[lookup_id] or { return []StructField{} }
	if !is_generic {
		return fields
	}
	params := tc.struct_generic_params[lookup_id] or {
		tc.struct_generic_params[syntax.application_base()] or { return fields }
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

// struct_fields_for_type returns fields for a source spelling ID, substituting concrete generics.
pub fn (tc &TypeChecker) struct_fields_for_type(struct_name_id u32) []StructField {
	return tc.struct_fields_for_init(struct_name_id)
}

@[direct_array_access; inline]
fn (tc &TypeChecker) struct_field_type(struct_name_id u32, field_name string) ?Type {
	if !isnil(tc.type_cache) {
		cache := tc.type_cache
		slot := struct_field_cache_slot(struct_name_id, field_name)
		if cache.struct_field_recent_unguarded[slot]
			&& cache.struct_field_recent_states[slot] != 0
			&& cache.struct_field_recent_structs[slot] == struct_name_id
			&& cache.struct_field_recent_fields[slot] == field_name {
			if cache.struct_field_recent_states[slot] > 0 {
				return *cache.struct_field_recent_values[slot]
			}
			return none
		}
	}
	return tc.struct_field_type_uncached(struct_name_id, field_name)
}

@[direct_array_access]
fn (tc &TypeChecker) struct_field_type_uncached(struct_name_id u32, field_name string) ?Type {
	is_c_struct := flat.source_type_query(struct_name_id).has(.c_prefix)
	has_escape_prefix := field_name.starts_with('@')
	if is_c_struct {
		if typ := tc.c_struct_module_field_type(struct_name_id, field_name) {
			return typ
		}
	}
	if has_escape_prefix && field_name.len > 1
		&& token.Token.from_string_tinyv(field_name[1..]).is_keyword() {
		mut exact_seen := map[u32]bool{}
		if exact := tc.struct_field_type_inner(struct_name_id, field_name, mut exact_seen) {
			return exact
		}
		plain_name := escaped_identifier_name(field_name)
		mut seen := map[u32]bool{}
		if typ := tc.c_struct_plain_keyword_field_type(struct_name_id, plain_name, mut seen) {
			return typ
		}
	}
	if !isnil(tc.type_cache) {
		cache := tc.type_cache
		slot := struct_field_cache_slot(struct_name_id, field_name)
		if cache.struct_field_recent_states[slot] != 0
			&& cache.struct_field_recent_structs[slot] == struct_name_id
			&& cache.struct_field_recent_fields[slot] == field_name {
			if cache.struct_field_recent_states[slot] > 0 {
				return *cache.struct_field_recent_values[slot]
			}
			return none
		}
	}
	unguarded := !is_c_struct && !has_escape_prefix
	if !isnil(tc.type_cache) {
		mut fallback := tc.type_cache.base
		for !isnil(fallback) {
			if typ := cached_struct_field_type(fallback.struct_field_shared, struct_name_id, field_name) {
				tc.remember_struct_field_type(struct_name_id, field_name, typ, true, unguarded)
				return *typ
			}
			if typ := cached_struct_field_type(fallback.struct_field_entries, struct_name_id, field_name) {
				tc.remember_struct_field_type(struct_name_id, field_name, typ, true, unguarded)
				return *typ
			}
			if cached_struct_field_miss(fallback.struct_field_misses, struct_name_id, field_name) {
				tc.remember_struct_field_type(struct_name_id, field_name, empty_type, false, unguarded)
				return none
			}
			if fallback.struct_field_complete[struct_name_id] {
				tc.remember_struct_field_type(struct_name_id, field_name, empty_type, false, unguarded)
				return none
			}
			fallback = fallback.base
		}
		if typ := cached_struct_field_type(tc.type_cache.struct_field_entries, struct_name_id, field_name) {
			tc.remember_struct_field_type(struct_name_id, field_name, typ, true, unguarded)
			return *typ
		}
		if typ := cached_struct_field_type(tc.type_cache.struct_field_shared, struct_name_id, field_name) {
			tc.remember_struct_field_type(struct_name_id, field_name, typ, true, unguarded)
			return *typ
		}
		if cached_struct_field_miss(tc.type_cache.struct_field_misses, struct_name_id, field_name)
			|| tc.type_cache.struct_field_complete[struct_name_id] {
			tc.remember_struct_field_type(struct_name_id, field_name, empty_type, false, unguarded)
			return none
		}
	}
	mut seen := map[u32]bool{}
	if typ := tc.struct_field_type_inner(struct_name_id, field_name, mut seen) {
		if !isnil(tc.type_cache) {
			mut cache := tc.type_cache
			canonical := tc.intern_type_reference(typ)
			cache.remember_struct_field_entry(struct_name_id, field_name, canonical)
			tc.remember_struct_field_type(struct_name_id, field_name, canonical, true, unguarded)
		}
		return typ
	}
	if !isnil(tc.type_cache) {
		mut cache := tc.type_cache
		cache.remember_struct_field_miss(struct_name_id, field_name)
	}
	tc.remember_struct_field_type(struct_name_id, field_name, empty_type, false, unguarded)
	return none
}

// c_struct_module_field_type returns the type of a field of the current module's own
// declaration of a C struct. Modules can mirror one C struct with different fields, or
// the same fields with different types (`C.sigaction` in `os` and in a translated C
// library); each module's code uses the fields as it declares them, as its struct
// literals already do.
fn (tc &TypeChecker) c_struct_module_field_type(struct_name_id u32, field_name string) ?Type {
	fields := tc.c_struct_scoped_fields[c_struct_module_key(tc.cur_module, struct_name_id)] or {
		return none
	}
	for field in fields {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

fn (tc &TypeChecker) c_struct_plain_keyword_field_type(struct_name_id u32, field_name string, mut seen map[u32]bool) ?Type {
	owner := unalias_and_unwrap_pointer_type(tc.parse_type(struct_name_id))
	if owner !is Struct {
		return none
	}
	if seen[owner.source_type_id()] {
		return none
	}
	seen[owner.source_type_id()] = true
	fields := tc.struct_fields_for_init(owner.source_type_id())
	for field in fields {
		if field.name == field_name && flat.source_type_query(owner.source_type_id()).has(.c_prefix) {
			return field.typ
		}
	}
	for field in fields {
		if field.is_embed {
			if typ := tc.c_struct_plain_keyword_field_type(field.typ.source_type_id(), field_name, mut seen) {
				return typ
			}
		}
	}
	return none
}

// Hash the source spelling ID and sample the field name; cache hits verify both keys.
@[direct_array_access; inline]
fn struct_field_cache_slot(struct_name_id u32, field_name string) int {
	mut hash := struct_name_id
	hash = hash * 16777619 ^ u32(field_name.len)
	if field_name != '' {
		hash = hash * 16777619 ^ u32(field_name[0])
		hash = hash * 16777619 ^ u32(field_name[field_name.len - 1])
	}
	return int(hash & 255)
}

@[direct_array_access]
fn (tc &TypeChecker) remember_struct_field_type(struct_name_id u32, field_name string, typ &Type, found bool, unguarded bool) {
	if isnil(tc.type_cache) {
		return
	}
	mut cache := tc.type_cache
	slot := struct_field_cache_slot(struct_name_id, field_name)
	cache.struct_field_recent_structs[slot] = struct_name_id
	cache.struct_field_recent_fields[slot] = field_name
	cache.struct_field_recent_values[slot] = typ
	cache.struct_field_recent_unguarded[slot] = unguarded
	cache.struct_field_recent_states[slot] = if found { i8(1) } else { i8(-1) }
}

fn cached_struct_field_type(entries map[u32]&StructFieldTypeEntries, owner_id u32, field_name string) ?&Type {
	fields := entries[owner_id] or { return none }
	return fields.values[field_name] or { return none }
}

fn cached_struct_field_miss(entries map[u32]&StructFieldMissEntries, owner_id u32, field_name string) bool {
	fields := entries[owner_id] or { return false }
	return fields.values[field_name]
}

fn (mut cache TypeCache) remember_struct_field_entry(owner_id u32, field_name string, typ &Type) {
	mut fields := cache.struct_field_entries[owner_id] or {
		entry := &StructFieldTypeEntries{}
		cache.struct_field_entries[owner_id] = entry
		entry
	}
	fields.values[field_name] = typ
}

fn (mut cache TypeCache) remember_struct_field_miss(owner_id u32, field_name string) {
	mut fields := cache.struct_field_misses[owner_id] or {
		entry := &StructFieldMissEntries{}
		cache.struct_field_misses[owner_id] = entry
		entry
	}
	fields.values[field_name] = true
}

// Only the private owner tables are released; shared declaration tables stay borrowed.
fn (mut cache TypeCache) clear_struct_field_entries() {
	for owner_id, _ in cache.struct_field_entries {
		unsafe { cache.struct_field_entries[owner_id].values.free() }
	}
	cache.struct_field_entries.clear()
	cache.clear_struct_field_misses()
}

fn (mut cache TypeCache) clear_struct_field_misses() {
	for owner_id, _ in cache.struct_field_misses {
		unsafe { cache.struct_field_misses[owner_id].values.free() }
	}
	cache.struct_field_misses.clear()
}

fn (mut cache TypeCache) free_struct_field_entries() {
	cache.clear_struct_field_entries()
	unsafe {
		cache.struct_field_entries.free()
		cache.struct_field_misses.free()
	}
}

// struct_field_type_name returns the canonical type name for a struct field.
pub fn (tc &TypeChecker) struct_field_type_name(struct_name_id u32, field_name string) ?u32 {
	typ := tc.struct_field_type(struct_name_id, field_name) or { return none }
	return tc.type_name(typ)
}

@[direct_array_access]
fn (tc &TypeChecker) struct_field_type_inner(struct_name_id u32, field_name string, mut seen map[u32]bool) ?Type {
	syntax := flat.source_type_syntax(struct_name_id)
	is_generic := syntax.application_valid()
	generic_args := syntax.application_args()
	base_name := syntax.application_base()
	raw_lookup_id := if is_generic { syntax.application_base() } else { struct_name_id }
	short_lookup_id := flat.source_type_syntax(raw_lookup_id).unqualified()
	lookup_id := if raw_lookup_id in tc.structs {
		raw_lookup_id
	} else if short_lookup_id in tc.structs {
		short_lookup_id
	} else if canonical_source_id := tc.canonical_qualified_type_name(raw_lookup_id) {
		canonical_source_id
	} else {
		raw_lookup_id
	}
	if seen.len > 0 && lookup_id in seen {
		return none
	}
	fields := tc.structs[lookup_id] or { []StructField{} }
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
	mut marked := false
	embeds_indexed := tc.struct_embed_receivers.contains(lookup_id)
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
		embedded_name_id := method_type_name(unwrap_pointer(embedded_type))
		embedded_lookup_type := if embedded_type is Alias {
			*embedded_type.base_type
		} else {
			embedded_type
		}
		embedded_lookup_name_id := method_type_name(unwrap_pointer(embedded_lookup_type))
		if embedded_name_id == 0 {
			continue
		}
		field_name_id := flat.source_type_id(field_name)
		// A `mod.Inner` embed is promoted under its short name: `o.Inner`.
		// Same-module embeds already match the direct-field pass above.
		if embedded_name_id != field_name_id && flat.source_type_syntax(embedded_name_id).unqualified() == field_name_id {
			return embedded_type
		}
		embedded_syntax := flat.source_type_syntax(embedded_name_id)
		if embedded_syntax.application_valid() && flat.source_type_syntax(embedded_syntax.application_base()).unqualified() == field_name_id {
			return embedded_type
		}
		if embedded_lookup_name_id != 0 && embedded_lookup_name_id != field_name_id
			&& flat.source_type_syntax(embedded_lookup_name_id).unqualified() == field_name_id {
			return embedded_type
		}
		// Direct fields and promoted embed names return without needing a cycle set.
		if !marked {
			seen[lookup_id] = true
			marked = true
		}
		if typ := tc.struct_field_type_inner(embedded_lookup_name_id, field_name, mut seen) {
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
fn (tc &TypeChecker) substitute_generic_type(typ Type, args_ids []u32, param_names []string) Type {
	if args_ids.len == 0 {
		return typ
	}
	if typ is Unknown {
		if name := generic_placeholder_from_unknown(typ) {
			mut idx := if param_names.len > 0 { param_names.index(name) } else { -1 }
			if idx < 0 {
				idx = generic_param_index(name)
			}
			if idx >= 0 && idx < args_ids.len {
				return *tc.parse_type(trimmed_source_type_id(args_ids[idx]))
			}
		}
		return typ
	}
	if typ is Array {
		return Type(Array{
			elem_type: tc.intern_type_reference(tc.substitute_generic_type(typ.elem_type, args_ids, param_names))
		})
	}
	if typ is ArrayFixed {
		return Type(ArrayFixed{
			elem_type: tc.intern_type_reference(tc.substitute_generic_type(typ.elem_type, args_ids, param_names))
			len:       typ.len
			len_expr:  subst_generic_const_expr(typ.len_expr, args_ids, param_names)
		})
	}
	if typ is Map {
		return Type(Map{
			key_type:   tc.intern_type_reference(tc.substitute_generic_type(typ.key_type, args_ids, param_names))
			value_type: tc.intern_type_reference(tc.substitute_generic_type(typ.value_type, args_ids, param_names))
		})
	}
	if typ is Pointer {
		return Type(Pointer{
			base_type: tc.intern_type_reference(tc.substitute_generic_type(typ.base_type, args_ids, param_names))
		})
	}
	if typ is OptionType {
		base_type := tc.substitute_generic_type(typ.base_type, args_ids, param_names)
		if base_type is OptionType {
			return base_type
		}
		return Type(OptionType{
			base_type: tc.intern_type_reference(base_type)
		})
	}
	if typ is ResultType {
		base_type := tc.substitute_generic_type(typ.base_type, args_ids, param_names)
		if base_type is ResultType {
			return base_type
		}
		return Type(ResultType{
			base_type: tc.intern_type_reference(base_type)
		})
	}
	if typ is Struct {
		return Type(Struct{
			name_id: subst_generic_text(typ.source_type_id(), args_ids, param_names)
		})
	}
	if typ is Interface {
		return Type(Interface{
			name_id: subst_generic_text(typ.source_type_id(), args_ids, param_names)
		})
	}
	if typ is Alias {
		return Type(Alias{
			name_id:   subst_generic_text(typ.source_type_id(), args_ids, param_names)
			base_type: tc.intern_type_reference(tc.substitute_generic_type(typ.base_type, args_ids, param_names))
		})
	}
	if typ is SumType {
		if flat.source_type_shape(typ.source_type_id()).has(.bracketed) {
			return *tc.parse_type(subst_generic_text(typ.source_type_id(), args_ids, param_names))
		}
		return typ
	}
	if typ is FnType {
		mut params := []FnParam{cap: typ.params.len}
		for param in typ.params {
			substituted := tc.substitute_generic_type(param.typ, args_ids, param_names)
			slot := if param.is_mut { mut_param_semantic_type(substituted) } else { substituted }
			params << FnParam{
				typ:    tc.intern_type_reference(slot)
				is_mut: param.is_mut
			}
		}
		result := tc.substitute_generic_type(typ.return_type, args_ids, param_names)
		signature := FnType{
			params:      params
			is_variadic: typ.is_variadic
			return_type: tc.intern_type_reference(result)
		}
		return Type(signature)
	}
	if typ is MultiReturn {
		mut parts := []Type{}
		for part in typ.types {
			parts << tc.substitute_generic_type(part, args_ids, param_names)
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
			elem_type: tc.intern_type_reference(tc.substitute_generic_type_values(typ.elem_type, args, param_names))
		})
	}
	if typ is ArrayFixed {
		mut arg_ids := []u32{cap: args.len}
		for arg in args {
			arg_ids << arg.source_type_id()
		}
		return Type(ArrayFixed{
			elem_type: tc.intern_type_reference(tc.substitute_generic_type_values(typ.elem_type, args, param_names))
			len:       typ.len
			len_expr:  subst_generic_const_expr(typ.len_expr, arg_ids, param_names)
		})
	}
	if typ is Map {
		return Type(Map{
			key_type:   tc.intern_type_reference(tc.substitute_generic_type_values(typ.key_type, args, param_names))
			value_type: tc.intern_type_reference(tc.substitute_generic_type_values(typ.value_type, args, param_names))
		})
	}
	if typ is Channel {
		return Type(Channel{
			elem_type: tc.intern_type_reference(tc.substitute_generic_type_values(typ.elem_type, args, param_names))
			is_mut:    typ.is_mut
		})
	}
	if typ is Pointer {
		return Type(Pointer{
			base_type: tc.intern_type_reference(tc.substitute_generic_type_values(typ.base_type, args, param_names))
		})
	}
	if typ is OptionType {
		base_type := tc.substitute_generic_type_values(typ.base_type, args, param_names)
		if base_type is OptionType {
			return base_type
		}
		return Type(OptionType{
			base_type: tc.intern_type_reference(base_type)
		})
	}
	if typ is ResultType {
		base_type := tc.substitute_generic_type_values(typ.base_type, args, param_names)
		if base_type is ResultType {
			return base_type
		}
		return Type(ResultType{
			base_type: tc.intern_type_reference(base_type)
		})
	}
	if typ is Struct {
		return Type(Struct{
			name_id: substitute_generic_named_type_values(typ.source_type_id(), args, param_names)
		})
	}
	if typ is Interface {
		return Type(Interface{
			name_id: substitute_generic_named_type_values(typ.source_type_id(), args, param_names)
		})
	}
	if typ is SumType {
		return Type(SumType{
			name_id: substitute_generic_named_type_values(typ.source_type_id(), args, param_names)
		})
	}
	if typ is FnType {
		mut params := []FnParam{cap: typ.params.len}
		for param in typ.params {
			substituted := tc.substitute_generic_type_values(param.typ, args, param_names)
			slot := if param.is_mut { mut_param_semantic_type(substituted) } else { substituted }
			params << FnParam{
				typ:    tc.intern_type_reference(slot)
				is_mut: param.is_mut
			}
		}
		result := tc.substitute_generic_type_values(typ.return_type, args, param_names)
		signature := FnType{
			params:      params
			is_variadic: typ.is_variadic
			return_type: tc.intern_type_reference(result)
		}
		return Type(signature)
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

fn substitute_generic_named_type_values(name_id u32, args []Type, param_names []string) u32 {
	mut arg_ids := []u32{cap: args.len}
	for arg in args {
		arg_ids << arg.source_type_id()
	}
	return subst_generic_text(name_id, arg_ids, param_names)
}

// subst_generic_const_expr replaces generic parameter identifiers in fixed-array length
// expressions, such as `sizeof(T)`, without changing identifiers that merely contain the
// same letters.
fn subst_generic_const_expr(expr string, args []u32, params []string) string {
	if expr.len == 0 || args.len == 0 || params.len != args.len {
		return expr
	}
	mut out := strings.new_builder(expr.len)
	mut i := 0
	for i < expr.len {
		ch := expr[i]
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || ch == `_` {
			start := i
			i++
			for i < expr.len {
				part := expr[i]
				if !((part >= `a` && part <= `z`) || (part >= `A` && part <= `Z`)
					|| (part >= `0` && part <= `9`) || part == `_`) {
					break
				}
				i++
			}
			ident := expr[start..i]
			idx := params.index(ident)
			out.write_string(if idx >= 0 { flat.source_type_name(args[idx]) } else { ident })
			continue
		}
		out.write_u8(ch)
		i++
	}
	return out.str()
}

fn (tc &TypeChecker) embedded_method_call_info(struct_name_id u32, method string) ?CallInfo {
	mut seen := map[u32]bool{}
	return tc.embedded_method_call_info_inner(struct_name_id, method, mut seen)
}

fn (tc &TypeChecker) embedded_method_call_info_inner(struct_name_id u32, method string, mut seen map[u32]bool) ?CallInfo {
	if seen[struct_name_id] {
		return none
	}
	seen[struct_name_id] = true
	for receiver_id in tc.embedded_receiver_ids(struct_name_id) {
		// An embedded concrete generic receiver (`Collector[int]`) promotes the
		// method registered on its open declaration (`Collector[T].use`).
		if info := tc.resolve_generic_struct_method(receiver_id, method) {
			return info
		}
		receiver := flat.source_type_name(receiver_id)
		mut method_names := ['${receiver}.${method}']
		receiver_syntax := flat.source_type_syntax(receiver_id)
		if receiver_syntax.application_valid() {
			method_names << '${flat.source_type_name(receiver_syntax.application_base())}.${method}'
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
					params_known:  if flat.source_type_shape(receiver_id).has(.bracketed) {
						false
					} else {
						info.params_known
					}
				}
			}
		}
		if info := tc.embedded_method_call_info_inner(receiver_id, method, mut seen) {
			return info
		}
	}
	return none
}

// embedded_receiver_ids lists, in field order, the method receivers of the
// embedded fields of `struct_name_id`.
fn (tc &TypeChecker) embedded_receiver_ids(struct_name_id u32) []u32 {
	mut memo := tc.interface_impl_memo
	if !isnil(memo) {
		if ids := memo.embedded_receivers[struct_name_id] {
			return ids
		}
	}
	mut ids := []u32{}
	for field in tc.struct_fields_for_type(struct_name_id) {
		embedded_type := embedded_field_type(field) or { continue }
		receiver_id := method_type_name(unwrap_pointer(embedded_type))
		if receiver_id != 0 {
			ids << receiver_id
		}
	}
	if !isnil(memo) {
		memo.embedded_receivers[struct_name_id] = ids
	}
	return ids
}

// method_value_receiver_name is the struct that declares the method a method value
// of `struct_name` names: that struct, or the embedded one for a promoted method.
fn (tc &TypeChecker) method_value_receiver_name(struct_name_id u32, method string) u32 {
	struct_name := flat.source_type_name(struct_name_id)
	if '${struct_name}.${method}' in tc.fn_param_types
		|| tc.resolve_generic_struct_method(struct_name_id, method) != none {
		return struct_name_id
	}
	return tc.embedded_method_value_owner(struct_name_id, method) or { struct_name_id }
}

// promoted_method_owner is the embedded struct that declares `method` for a receiver
// of `receiver_type` that does not declare it: `Base` for a `Plain` that embeds it.
// An alias that declares the method, or whose struct does, promotes none.
fn (tc &TypeChecker) promoted_method_owner(receiver_type Type, method string) ?u32 {
	receiver := unwrap_pointer(receiver_type)
	if receiver is Alias {
		if _ := tc.alias_method_value_decl_key(receiver, method) {
			return none
		}
	}
	clean := unalias_type(receiver)
	if clean is Struct {
		owner := tc.method_value_receiver_name(clean.source_type_id(), method)
		if owner != clean.name_id {
			return owner
		}
	}
	return none
}

// promoted_method_value_owner is promoted_method_owner for the receiver of the
// selector `id`, a method named without a call: `Base` for `p.describe`.
pub fn (tc &TypeChecker) promoted_method_value_owner(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	return tc.promoted_method_owner(tc.resolve_type(tc.a.child(&node, 0)), node.value)
}

// method_value_selector_type is the type of the selector `id` that names a method
// without a call, as selector_type gives it: `fn () User` for `s.get` with
// `s Shelf[User]`, the method of an alias before the ones of its struct.
pub fn (tc &TypeChecker) method_value_selector_type(id flat.NodeId) ?Type {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_type := tc.resolve_type(tc.a.child(&node, 0))
	receiver := unwrap_pointer(base_type)
	if receiver is Alias && '${receiver.name()}.${node.value}' in tc.fn_param_types {
		return tc.method_value_type(receiver.source_type_id(), node.value)
	}
	clean := unalias_and_unwrap_pointer_type(base_type)
	if clean is Struct {
		return tc.method_value_type(clean.source_type_id(), node.value)
	}
	if clean is Interface {
		return tc.method_value_type(clean.source_type_id(), node.value)
	}
	return none
}

// alias_method_value_receiver is the alias whose own method the selector `id` names
// without a call: `Alias` for `a.describe` with `fn (a Alias) describe()`, and none
// when the method is the one of the aliased type.
pub fn (tc &TypeChecker) alias_method_value_receiver(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	receiver := unwrap_pointer(tc.resolve_type(tc.a.child(&node, 0)))
	if receiver !is Alias {
		return none
	}
	alias := receiver as Alias
	key := tc.alias_method_value_decl_key(alias, node.value) or { return none }
	alias_short_id := flat.source_type_syntax(alias.source_type_id()).unqualified()
	// The receiver of the declaration key must end in the alias' last segment. That
	// segment is interned already, so probe the key text without interning it.
	key_short_id := flat.source_type_existing_id(key.all_before_last('.').all_after_last('.')) or {
		return none
	}
	if key_short_id != alias_short_id {
		return none
	}
	return alias.source_type_id()
}

// record_promoted_method_value records the method of the embedded struct `owner`
// that a method value of an alias promotes, as the method values of a struct are.
fn (mut tc TypeChecker) record_promoted_method_value(owner_id u32, method string) {
	owner := flat.source_type_name(owner_id)
	mkey := '${owner}.${method}'
	if mkey !in tc.fn_param_types {
		ci := tc.resolve_generic_struct_method(owner_id, method) or { return }
		tc.generic_method_value_info[owner_id][method] = ci
		tc.method_values_by_fn[tc.fn_context.node_id] << ci.name
	}
	tc.method_values_by_fn[tc.fn_context.node_id] << mkey
}

// embedded_method_value_owner is the embedded struct whose method a struct promotes,
// `Base` for `Plain.describe` or `Holder[int]` for `IntHolder.get`, found as
// embedded_method_call_info finds it. An ambiguous method has none, as its call.
fn (tc &TypeChecker) embedded_method_value_owner(struct_name_id u32, method string) ?u32 {
	if tc.embedded_method_candidates(struct_name_id, method).len > 1 {
		return none
	}
	mut seen := map[u32]bool{}
	return tc.embedded_method_value_owner_inner(struct_name_id, method, mut seen)
}

fn (tc &TypeChecker) embedded_method_value_owner_inner(struct_name_id u32, method string, mut seen map[u32]bool) ?u32 {
	if seen[struct_name_id] {
		return none
	}
	seen[struct_name_id] = true
	for field in tc.struct_fields_for_type(struct_name_id) {
		embedded_type := embedded_field_type(field) or { continue }
		receiver := method_type_name(unwrap_pointer(embedded_type))
		if receiver == 0 {
			continue
		}
		if _ := tc.resolve_generic_struct_method(receiver, method) {
			return receiver
		}
		if '${flat.source_type_name(receiver)}.${method}' in tc.fn_ret_types {
			return receiver
		}
		if owner_source_id := tc.embedded_method_value_owner_inner(receiver, method, mut seen) {
			return owner_source_id
		}
	}
	return none
}

fn (tc &TypeChecker) struct_has_middleware_receiver(struct_name_id u32) bool {
	if is_middleware_type_name(struct_name_id) {
		return true
	}
	for field in tc.struct_fields_for_type(struct_name_id) {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name_id := method_type_name(unwrap_pointer(embedded_type))
		if is_middleware_type_name(embedded_name_id) {
			return true
		}
	}
	return false
}

fn is_middleware_type_name(name_id u32) bool {
	base_id := if flat.source_type_shape(name_id).has(.bracketed) {
		flat.source_type_syntax(name_id).generic_base()
	} else {
		name_id
	}
	return base_id == checker_source_veb_middleware
}

fn (tc &TypeChecker) receiver_embeds(actual &Type, expected &Type) bool {
	actual_name_id := method_type_name(unalias_and_unwrap_pointer_type(actual))
	expected_name_id := method_type_name(unwrap_pointer(expected))
	if actual_name_id == 0 || expected_name_id == 0 {
		return false
	}
	mut seen := map[u32]bool{}
	return tc.receiver_embeds_inner(actual_name_id, expected_name_id, mut seen)
}

fn (tc &TypeChecker) receiver_embeds_inner(actual_name_id u32, expected_name_id u32, mut seen map[u32]bool) bool {
	if seen[actual_name_id] {
		return false
	}
	seen[actual_name_id] = true
	for field in tc.struct_fields_for_type(actual_name_id) {
		embedded_type := embedded_field_type(field) or { continue }
		embedded_name_id := method_type_name(unwrap_pointer(embedded_type))
		if embedded_name_id == expected_name_id {
			return true
		}
		if tc.receiver_embeds_inner(embedded_name_id, expected_name_id, mut seen) {
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
	if s == '' {
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

// trimmed_source_type_id keeps the original identity when no trimming is needed.
@[inline]
fn trimmed_source_type_id(id u32) u32 {
	if !flat.source_type_shape(id).has(.padded) {
		return id
	}
	return flat.source_type_syntax(id).trimmed()
}

@[inline]
fn trimmed_source_type_shape(id u32) flat.SourceTypeShape {
	shape := flat.source_type_shape(id)
	if !shape.has(.padded) {
		return shape
	}
	return flat.source_type_shape(flat.source_type_syntax(id).trimmed())
}

fn embedded_field_type(field StructField) ?Type {
	if field.is_embed {
		return field.typ
	}
	field_type_id := method_type_name(unwrap_pointer(field.typ))
	if field_type_id == 0 {
		return none
	}
	if field.name.len == 0 {
		return field.typ
	}
	field_name_id := flat.source_type_id(field.name)
	if embedded_type_name_matches(field_name_id, field_type_id) {
		return field.typ
	}
	syntax := flat.source_type_syntax(field_type_id)
	if syntax.application_valid() && embedded_type_name_matches(field_name_id, syntax.application_base()) {
		return field.typ
	}
	return none
}

// embedded_name_matches adapts declaration names to the numeric type query.
fn embedded_name_matches(field_name string, type_name string) bool {
	return embedded_type_name_matches(flat.source_type_id(field_name), flat.source_type_id(type_name))
}

fn embedded_type_name_matches(field_name_id u32, type_name_id u32) bool {
	return field_name_id == type_name_id
		|| field_name_id in flat.source_type_syntax(type_name_id).qualifier_suffixes()
}

// method_signature_compatible supports method signature compatible handling for TypeChecker.
fn (tc &TypeChecker) method_signature_compatible(actual_key string, expected_key string) bool {
	return method_signature_compatible_guarded(tc, actual_key, expected_key, []InterfaceSatisfactionPair{})
}

fn method_signature_compatible_guarded(tc &TypeChecker, actual_key string, expected_key string, visited []InterfaceSatisfactionPair) bool {
	actual_params := tc.fn_param_types[actual_key] or { return false }
	expected_params := tc.fn_param_types[expected_key] or { return false }
	if actual_params.len != expected_params.len {
		return false
	}
	expected_receiver_mut, expected_receiver_shared := tc.method_receiver_flags(expected_key)
	actual_receiver_mut, actual_receiver_shared := tc.method_receiver_flags(actual_key)
	if (!expected_receiver_mut && actual_receiver_mut)
		|| (expected_receiver_mut && actual_receiver_shared && !expected_receiver_shared) {
		return false
	}
	for i in 1 .. actual_params.len {
		if !method_param_signature_compatible_guarded(tc, actual_params[i], expected_params[i],
			visited) {
			return false
		}
	}
	actual_ret := tc.fn_ret_types[actual_key] or { builtin_void_type }
	expected_ret := tc.fn_ret_types[expected_key] or { builtin_void_type }
	return method_return_signature_compatible_guarded(tc, actual_ret, expected_ret, visited)
}

// specialized_interface_method_signature specializes a method for an interface spelling ID.
pub fn (tc &TypeChecker) specialized_interface_method_signature(iface_name_id u32, expected_key string) ([]Type, Type) {
	params := tc.fn_param_types[expected_key] or { []Type{} }
	ret := tc.fn_ret_types[expected_key] or { builtin_void_type }
	syntax := flat.source_type_syntax(iface_name_id)
	args := syntax.application_args()
	if !syntax.application_valid() || args.len == 0 {
		return params, ret
	}
	meta_base := tc.interface_metadata_name(syntax.application_base())
	base := syntax.application_base()
	param_names := tc.interface_generic_params[meta_base] or {
		tc.interface_generic_params[base] or {
			tc.interface_generic_params[flat.source_type_syntax(syntax.application_base()).unqualified()] or { return params, ret }
		}
	}
	if param_names.len != args.len {
		return params, ret
	}
	mut concrete_types := []Type{cap: args.len}
	for arg in args {
		concrete_types << tc.parse_type(trimmed_source_type_id(arg))
	}
	param_texts := tc.fn_param_type_texts.type_ids(expected_key) or { []u32{} }
	mut specialized_params := []Type{cap: params.len}
	for i, param in params {
		param_source_id := if param_texts.len == params.len {
			param_texts[i]
		} else {
			u32(0)
		}
		specialized_params << tc.substitute_generic_param_type(param, param_source_id, concrete_types,
			param_names)
	}
	return specialized_params, tc.substitute_generic_type_values(ret, concrete_types, param_names)
}

fn (tc &TypeChecker) method_signature_compatible_for_interface(actual_key string, expected_key string, iface_name_id u32) bool {
	return method_signature_compatible_for_interface_guarded(tc, actual_key, expected_key,
		iface_name_id, []InterfaceSatisfactionPair{})
}

fn method_signature_compatible_for_interface_guarded(tc &TypeChecker, actual_key string, expected_key string, iface_name_id u32, visited []InterfaceSatisfactionPair) bool {
	actual_params := tc.fn_param_types[actual_key] or { return false }
	expected_params, expected_ret := tc.specialized_interface_method_signature(iface_name_id, expected_key)
	if actual_params.len != expected_params.len {
		return false
	}
	expected_receiver_mut, expected_receiver_shared := tc.method_receiver_flags(expected_key)
	actual_receiver_mut, actual_receiver_shared := tc.method_receiver_flags(actual_key)
	if (!expected_receiver_mut && actual_receiver_mut)
		|| (expected_receiver_mut && actual_receiver_shared && !expected_receiver_shared) {
		return false
	}
	for i in 1 .. actual_params.len {
		if !method_param_signature_compatible_guarded(tc, actual_params[i], expected_params[i],
			visited) {
			return false
		}
	}
	actual_ret := tc.fn_ret_types[actual_key] or { builtin_void_type }
	return method_return_signature_compatible_guarded(tc, actual_ret, expected_ret, visited)
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
	expected_ret := tc.fn_ret_types[expected_key] or { builtin_void_type }
	return tc.method_return_signature_compatible(actual.return_type, expected_ret)
}

fn (tc &TypeChecker) method_call_info_signature_compatible_for_interface(actual CallInfo, expected_key string, iface_name_id u32) bool {
	return method_call_info_signature_compatible_for_interface_guarded(tc, actual, expected_key,
		iface_name_id, []InterfaceSatisfactionPair{})
}

fn method_call_info_signature_compatible_for_interface_guarded(tc &TypeChecker, actual CallInfo, expected_key string, iface_name_id u32, visited []InterfaceSatisfactionPair) bool {
	expected_params, expected_ret := tc.specialized_interface_method_signature(iface_name_id, expected_key)
	if actual.params.len != expected_params.len {
		return false
	}
	for i in 1 .. actual.params.len {
		if !method_param_signature_compatible_guarded(tc, actual.params[i], expected_params[i],
			visited) {
			return false
		}
	}
	return method_return_signature_compatible_guarded(tc, actual.return_type, expected_ret,
		visited)
}

fn (tc &TypeChecker) method_return_signature_compatible(actual &Type, expected &Type) bool {
	return method_return_signature_compatible_guarded(tc, actual, expected, []InterfaceSatisfactionPair{})
}

fn method_return_signature_compatible_guarded(tc &TypeChecker, actual &Type, expected &Type, visited []InterfaceSatisfactionPair) bool {
	if !tc.fn_type_callconv_compatible(actual, expected) {
		return false
	}
	if semantic_types_equal(actual, expected) {
		return true
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
			if !method_return_signature_compatible_guarded(tc, typ, expected_unaliased.types[i],
				visited) {
				return false
			}
		}
		return true
	}
	if actual_unaliased is OptionType && expected_unaliased is OptionType {
		return method_wrapped_return_signature_compatible_guarded(tc, actual_unaliased.base_type,
			expected_unaliased.base_type, visited)
	}
	if actual_unaliased is ResultType && expected_unaliased is ResultType {
		return method_wrapped_return_signature_compatible_guarded(tc, actual_unaliased.base_type,
			expected_unaliased.base_type, visited)
	}
	if actual_fn := fn_type_from_type(actual) {
		expected_fn := fn_type_from_type(expected) or { return false }
		if type_pointer_depth(actual) != type_pointer_depth(expected)
			|| actual_fn.params.len != expected_fn.params.len {
			return false
		}
		for i, param in actual_fn.params {
			if !method_param_signature_compatible_guarded(tc, param.typ, expected_fn.params[i].typ,
				visited) {
				return false
			}
		}
		return method_return_signature_compatible_guarded(tc, actual_fn.return_type, expected_fn.return_type,
			visited)
	}
	return method_interface_return_signature_compatible_guarded(tc, actual_unaliased,
		expected_unaliased, visited)
}

fn (tc &TypeChecker) method_wrapped_return_signature_compatible(actual &Type, expected &Type) bool {
	return method_wrapped_return_signature_compatible_guarded(tc, actual, expected, []InterfaceSatisfactionPair{})
}

fn method_wrapped_return_signature_compatible_guarded(tc &TypeChecker, actual &Type, expected &Type, visited []InterfaceSatisfactionPair) bool {
	actual_unaliased := unalias_type(actual)
	expected_unaliased := unalias_type(expected)
	if actual_unaliased is MultiReturn && expected_unaliased is MultiReturn {
		return method_return_signature_compatible_guarded(tc, actual_unaliased, expected_unaliased,
			visited)
	}
	if actual_unaliased is FnType && expected_unaliased is FnType {
		return Type(actual_unaliased).source_type_id() == Type(expected_unaliased).source_type_id()
	}
	return method_interface_return_signature_compatible_guarded(tc, actual_unaliased,
		expected_unaliased, visited)
}

fn (tc &TypeChecker) method_interface_return_signature_compatible(actual &Type, expected &Type) bool {
	return method_interface_return_signature_compatible_guarded(tc, actual, expected, []InterfaceSatisfactionPair{})
}

fn method_interface_return_signature_compatible_guarded(tc &TypeChecker, actual &Type, expected &Type, visited []InterfaceSatisfactionPair) bool {
	if expected is Interface {
		return type_implements_interface_guarded(tc, actual, expected, visited)
	}
	expected_name_id := expected.source_type_id()
	if expected_name_id in tc.interface_names {
		return type_implements_interface_guarded(tc, actual, Interface{
			name_id: expected_name_id
		}, visited)
	}
	return false
}

fn (tc &TypeChecker) method_param_signature_compatible(actual &Type, expected &Type) bool {
	return method_param_signature_compatible_guarded(tc, actual, expected, []InterfaceSatisfactionPair{})
}

fn method_param_signature_compatible_guarded(tc &TypeChecker, actual &Type, expected &Type, visited []InterfaceSatisfactionPair) bool {
	if type_pointer_depth(actual) != type_pointer_depth(expected) {
		return false
	}
	if !tc.fn_type_callconv_compatible(actual, expected) {
		return false
	}
	if actual_iface := tc.method_param_interface_name(actual) {
		expected_iface := tc.method_param_interface_name(expected) or { return false }
		return actual_iface == expected_iface
			|| interface_implements_interface_guarded(tc, actual_iface, expected_iface, visited)
	}
	if _ := tc.method_param_interface_name(expected) {
		return false
	}
	return tc.type_compatible(actual, expected) && tc.type_compatible(expected, actual)
}

// fn_type_callconv_compatible checks calling conventions recursively for function types.
pub fn (tc &TypeChecker) fn_type_callconv_compatible(actual &Type, expected &Type) bool {
	actual_name_id := actual.source_type_id()
	expected_name_id := expected.source_type_id()
	if flat.source_type_shape(actual_name_id).has(.thread_type)
		&& flat.source_type_shape(expected_name_id).has(.thread_type) {
		return tc.fn_type_callconv_compatible(tc.parse_type(flat.source_type_syntax(actual_name_id).raw_base()),
			tc.parse_type(flat.source_type_syntax(expected_name_id).raw_base()))
	}
	if actual is Pointer && expected is Pointer {
		return tc.fn_type_callconv_compatible(actual.base_type, expected.base_type)
	}
	if actual is OptionType && expected is OptionType {
		return tc.fn_type_callconv_compatible(actual.base_type, expected.base_type)
	}
	if actual is ResultType && expected is ResultType {
		return tc.fn_type_callconv_compatible(actual.base_type, expected.base_type)
	}
	if actual is Array && expected is Array {
		return tc.fn_type_callconv_compatible(actual.elem_type, expected.elem_type)
	}
	if actual is ArrayFixed && expected is ArrayFixed {
		return tc.fixed_array_lengths_compatible(actual, expected)
			&& tc.fn_type_callconv_compatible(actual.elem_type, expected.elem_type)
	}
	if actual is Map && expected is Map {
		return tc.fn_type_callconv_compatible(actual.key_type, expected.key_type)
			&& tc.fn_type_callconv_compatible(actual.value_type, expected.value_type)
	}
	if actual is Channel && expected is Channel {
		return tc.fn_type_callconv_compatible(actual.elem_type, expected.elem_type)
	}
	if actual is MultiReturn && expected is MultiReturn {
		if actual.types.len != expected.types.len {
			return false
		}
		for i, typ in actual.types {
			if !tc.fn_type_callconv_compatible(typ, expected.types[i]) {
				return false
			}
		}
		return true
	}
	if actual is Alias && fn_type_from_type(actual) == none {
		return tc.fn_type_callconv_compatible(actual.base_type, expected)
	}
	if expected is Alias && fn_type_from_type(expected) == none {
		return tc.fn_type_callconv_compatible(actual, expected.base_type)
	}
	if actual is Struct || actual is Interface || actual is SumType {
		actual_syntax := flat.source_type_syntax(actual_name_id)
		expected_syntax := flat.source_type_syntax(expected_name_id)
		if actual_syntax.application_valid() && expected_syntax.application_valid() {
			if actual_syntax.application_args().len != expected_syntax.application_args().len {
				return false
			}
			for i, arg in actual_syntax.application_args() {
				if !tc.fn_type_callconv_compatible(tc.parse_type(arg), tc.parse_type(expected_syntax.application_args()[i])) {
					return false
				}
			}
		}
	}
	actual_fn := fn_type_from_type(actual) or { return true }
	expected_fn := fn_type_from_type(expected) or { return true }
	if tc.fn_type_callconv(actual) != tc.fn_type_callconv(expected)
		|| actual_fn.params.len != expected_fn.params.len {
		return false
	}
	for i, parameter in actual_fn.params {
		param := parameter.typ
		if !tc.fn_type_callconv_compatible(param, expected_fn.params[i].typ) {
			return false
		}
	}
	return tc.fn_type_callconv_compatible(actual_fn.return_type, expected_fn.return_type)
}

fn (tc &TypeChecker) fn_type_callconv(typ &Type) string {
	if typ is Alias {
		if fn_type_from_type(typ) != none {
			if value := tc.type_alias_attribute_value(typ.source_type_id(), 'callconv') {
				return value
			}
			return 'cdecl'
		}
		return tc.fn_type_callconv(typ.base_type)
	}
	if typ is Pointer {
		return tc.fn_type_callconv(typ.base_type)
	}
	return 'cdecl'
}

fn (tc &TypeChecker) type_alias_attribute_value(name_id u32, attribute string) ?string {
	short_id := flat.source_type_syntax(name_id).unqualified()
	indices := tc.type_declaration_ids[name_id] or {
		tc.type_declaration_ids[short_id] or { return none }
	}
	for index in indices {
		node := tc.a.nodes[index]
		if node.kind != .type_decl || flat.source_type_syntax(node.source_value_id()).unqualified() != short_id {
			continue
		}
		if value := tc.declaration_attribute_value(flat.NodeId(index), attribute) {
			return value
		}
	}
	return none
}

fn (tc &TypeChecker) method_param_interface_name(typ &Type) ?u32 {
	clean := unwrap_pointer(typ)
	if clean is Interface {
		return tc.interface_metadata_name(clean.source_type_id())
	}
	if clean.source_type_id() in tc.interface_names {
		return tc.interface_metadata_name(clean.source_type_id())
	}
	return none
}

fn type_pointer_depth(t &Type) int {
	if t is Pointer {
		return 1 + type_pointer_depth(t.base_type)
	}
	if t is Alias {
		return type_pointer_depth(t.base_type)
	}
	return 0
}

// method_type_name returns the spelling ID of a method receiver type, or zero.
fn method_type_name(t &Type) u32 {
	if t is Alias || t is Struct || t is Interface || t is SumType || t is Enum
		|| t is String || t is Char || t is Primitive || t is ISize || t is USize || t is Rune {
		return t.source_type_id()
	}
	return 0
}

fn (tc &TypeChecker) sum_base_name(sum_name_id u32) u32 {
	shape := flat.source_type_shape(sum_name_id)
	if shape.has(.bracketed) {
		syntax := flat.source_type_syntax(sum_name_id)
		if syntax.application_valid() { return syntax.application_base() }
	}
	if tc.sum_types.contains(sum_name_id) { return sum_name_id }
	qname_id := tc.qualify_type_name(sum_name_id)
	if tc.sum_types.contains(qname_id) { return qname_id }
	if shape.has(.dotted) {
		resolved_id := tc.resolve_imported_type_text(sum_name_id)
		if tc.sum_types.contains(resolved_id) { return resolved_id }
		if unique_id := tc.unique_qualified_type_name(flat.source_type_syntax(sum_name_id).unqualified()) {
			if tc.sum_types.contains(unique_id) { return unique_id }
		}
	}
	return sum_name_id
}

fn (tc &TypeChecker) sum_params_for_base(base_id u32) []string {
	if params := tc.sum_generic_params[base_id] { return params }
	if params := tc.sum_generic_params[flat.source_type_syntax(base_id).unqualified()] {
		return params
	}
	return []string{}
}

fn (tc &TypeChecker) concrete_sum_variant_name(sum_name_id u32, variant_id u32) u32 {
	if !flat.source_type_shape(sum_name_id).has(.bracketed) { return variant_id }
	syntax := flat.source_type_syntax(sum_name_id)
	if !syntax.application_valid() { return variant_id }
	params := tc.sum_params_for_base(syntax.application_base())
	if params.len == 0 || params.len != syntax.application_args().len { return variant_id }
	return subst_generic_text(variant_id, syntax.application_args(), params)
}

// generic_type_name_matches compares source spelling IDs inside the current resolution context.
pub fn (tc &TypeChecker) generic_type_name_matches(a_id u32, b_id u32) bool {
	if a_id == b_id { return true }
	a := flat.source_type_syntax(a_id)
	b := flat.source_type_syntax(b_id)
	if a.paren_prefix() || b.paren_prefix() { return tc.multi_return_type_names_match(a_id, b_id) }
	if tc.fixed_array_type_name_matches(a_id, b_id) { return true }
	if a.wrapper_prefix() != 0 || b.wrapper_prefix() != 0 {
		return a.wrapper_prefix() == b.wrapper_prefix()
			&& tc.generic_type_name_matches(trimmed_source_type_id(a.wrapper_base()), trimmed_source_type_id(b.wrapper_base()))
	}
	if a.application_valid() || b.application_valid() {
		if !a.application_valid() || !b.application_valid() || a.application_args().len != b.application_args().len {
			return false
		}
		if !tc.generic_type_base_matches(a.application_base(), b.application_base()) {
			return false
		}
		for i, arg in a.application_args() {
			if !tc.generic_type_arg_matches(arg, b.application_args()[i]) { return false }
		}
		return true
	}
	return tc.generic_type_base_matches(a_id, b_id)
}

// multi_return_type_names_match reports whether the multi-return type names `a` and `b`,
// spelled `(A, B, ...)`, have as many slots and every slot matches.
fn (tc &TypeChecker) multi_return_type_names_match(a_id u32, b_id u32) bool {
	a := flat.source_type_syntax(a_id)
	b := flat.source_type_syntax(b_id)
	if !a.tuple_like() || !b.tuple_like() || a.checker_tuple_components().len != b.checker_tuple_components().len {
		return false
	}
	for i, arg in a.checker_tuple_components() {
		if !tc.generic_type_name_matches(arg, b.checker_tuple_components()[i]) { return false }
	}
	return true
}

fn (tc &TypeChecker) fixed_array_type_name_matches(a_id u32, b_id u32) bool {
	if !flat.source_type_shape(a_id).has(.bracketed) || !flat.source_type_shape(b_id).has(.bracketed) {
		return false
	}
	if !flat.source_type_syntax(a_id).checker_fixed_array_may_be() || !flat.source_type_syntax(b_id).checker_fixed_array_may_be() {
		return false
	}
	a_type := tc.parse_type(a_id)
	b_type := tc.parse_type(b_id)
	if a_type is ArrayFixed && b_type is ArrayFixed {
		return tc.fixed_array_lengths_compatible(a_type, b_type)
			&& tc.generic_type_name_matches(a_type.elem_type.source_type_id(), b_type.elem_type.source_type_id())
	}
	return false
}

fn (tc &TypeChecker) generic_type_base_matches(a_id u32, b_id u32) bool {
	a := trimmed_source_type_id(a_id)
	b := trimmed_source_type_id(b_id)
	if a == b { return true }
	a_resolved := tc.resolve_generic_match_base(a)
	b_resolved := tc.resolve_generic_match_base(b)
	if a_resolved == b_resolved { return true }
	if flat.source_type_shape(a).has(.dotted) || flat.source_type_shape(b).has(.dotted)
		|| flat.source_type_shape(a_resolved).has(.dotted) || flat.source_type_shape(b_resolved).has(.dotted) {
		return false
	}
	return flat.source_type_syntax(a).unqualified() == flat.source_type_syntax(b).unqualified()
}

fn (tc &TypeChecker) resolve_generic_match_base(base_id u32) u32 {
	if base_id == 0 { return base_id }
	if flat.source_type_shape(base_id).has(.dotted) {
		return tc.resolve_imported_type_text(base_id)
	}
	if resolved_id := tc.resolve_selective_import_type_symbol_id(base_id) {
		return resolved_id
	}
	qbase := tc.qualify_type_name(base_id)
	if qbase != base_id && tc.type_symbol_known(qbase) { return qbase }
	return base_id
}

fn (tc &TypeChecker) generic_type_arg_matches(a_id u32, b_id u32) bool {
	a_clean := trimmed_source_type_id(a_id)
	b_clean := trimmed_source_type_id(b_id)
	if a_clean == b_clean { return true }
	a := flat.source_type_syntax(a_clean)
	b := flat.source_type_syntax(b_clean)
	if (a.bare_generic() && !tc.is_known_type_text(a_clean)) || (b.bare_generic() && !tc.is_known_type_text(b_clean)) {
		return true
	}
	if a.wrapper_prefix() != 0 || b.wrapper_prefix() != 0 {
		return a.wrapper_prefix() == b.wrapper_prefix() && tc.generic_type_arg_matches(a.wrapper_base(), b.wrapper_base())
	}
	a_shape := flat.source_type_shape(a_clean)
	b_shape := flat.source_type_shape(b_clean)
	if a_shape.has(.map_type) || b_shape.has(.map_type) {
		if !a_shape.has(.map_type) || !b_shape.has(.map_type) || !a.bracket_closed() || !b.bracket_closed() {
			return false
		}
		return tc.generic_type_arg_matches(a.bracket_raw_inner(), b.bracket_raw_inner())
			&& tc.generic_type_arg_matches(a.bracket_suffix(), b.bracket_suffix())
	}
	if a_shape.has(.leading_bracket) || b_shape.has(.leading_bracket) {
		if !a_shape.has(.leading_bracket) || !b_shape.has(.leading_bracket) || !a.bracket_closed() || !b.bracket_closed() || a.bracket_prefix() != b.bracket_prefix() {
			return false
		}
		return tc.generic_type_arg_matches(a.bracket_suffix(), b.bracket_suffix())
	}
	if a.application_valid() || b.application_valid() {
		if !a.application_valid() || !b.application_valid() || a.application_args().len != b.application_args().len {
			return false
		}
		if !tc.generic_type_base_matches(a.application_base(), b.application_base()) {
			return false
		}
		for i, arg in a.application_args() {
			if !tc.generic_type_arg_matches(arg, b.application_args()[i]) { return false }
		}
		return true
	}
	return tc.resolve_generic_match_arg(a_clean) == tc.resolve_generic_match_arg(b_clean)
}

fn (tc &TypeChecker) resolve_generic_match_arg(arg_id u32) u32 {
	if arg_id == 0 { return arg_id }
	if !flat.source_type_shape(arg_id).has(.dotted) {
		if resolved_id := tc.resolve_selective_import_type_symbol_id(arg_id) {
			return resolved_id
		}
	}
	qarg := tc.qualify_type_text(arg_id)
	return if qarg != 0 { qarg } else { arg_id }
}

fn (tc &TypeChecker) bare_variant_name_matches(candidate_id u32, declared_id u32, concrete_id u32) bool {
	if flat.source_type_syntax(candidate_id).application_valid() { return false }
	candidate_base := trimmed_source_type_id(candidate_id)
	declared_syntax := flat.source_type_syntax(declared_id)
	concrete_syntax := flat.source_type_syntax(concrete_id)
	declared_base := trimmed_source_type_id(if declared_syntax.generic_base() != 0 {
		declared_syntax.generic_base()
	} else {
		declared_id
	})
	concrete_base := trimmed_source_type_id(if concrete_syntax.generic_base() != 0 {
		concrete_syntax.generic_base()
	} else {
		concrete_id
	})
	candidate_resolved := tc.resolve_generic_match_base(candidate_base)
	if candidate_resolved == tc.resolve_generic_match_base(declared_base)
		|| candidate_resolved == tc.resolve_generic_match_base(concrete_base) {
		return true
	}
	if flat.source_type_shape(candidate_base).has(.dotted) || flat.source_type_shape(candidate_resolved).has(.dotted) {
		return false
	}
	return (!flat.source_type_shape(declared_base).has(.dotted) && candidate_base == flat.source_type_syntax(declared_base).unqualified())
		|| (!flat.source_type_shape(concrete_base).has(.dotted) && candidate_base == flat.source_type_syntax(concrete_base).unqualified())
}

// type_matches_sum returns type matches sum data for TypeChecker.
fn (tc &TypeChecker) type_matches_sum(actual &Type, expected &Type) bool {
	if expected is SumType {
		actual_name_source_id := tc.type_name(actual)

		if tc.sum_variant_type_for_pattern(expected.source_type_id(), actual_name_source_id) != none {
			return true
		}
		base := tc.sum_base_name(expected.source_type_id())
		variants := tc.sum_types.type_ids(base) or { return false }
		for variant in variants {
			concrete_source_id_4 := tc.concrete_sum_variant_name(expected.source_type_id(), variant)

			variant_type := tc.parse_type(concrete_source_id_4)
			if tc.type_name(variant_type) == expected.name_id {
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
fn (tc &TypeChecker) sum_has_variant(sum_name_id u32, variant_name_id u32) bool {
	return tc.sum_variant_type_for_pattern(sum_name_id, variant_name_id) != none
}

// sum_variant_type_for_pattern resolves a sum pattern using source spelling IDs.
pub fn (tc &TypeChecker) sum_variant_type_for_pattern(sum_name_id u32, variant_name_id u32) ?u32 {
	if isnil(tc.type_cache) {
		return tc.sum_variant_type_for_pattern_depth(sum_name_id, variant_name_id, 0)
	}
	mut cache := tc.type_cache
	key := (u64(sum_name_id) << 32) | u64(variant_name_id)
	mut context := cache.sum_variant_pattern_recent
	if isnil(context) || cache.sum_variant_pattern_recent_count != cache.sum_variant_pattern_entries.len
		|| context.file != tc.cur_file || context.module != tc.cur_module {
		context = unsafe { nil }
		if file_entries := cache.sum_variant_pattern_entries[tc.cur_file] {
			if module_entries := file_entries.modules[tc.cur_module] {
				context = module_entries
				cache.sum_variant_pattern_recent = context
				cache.sum_variant_pattern_recent_count = cache.sum_variant_pattern_entries.len
			}
		}
	}
	if !isnil(context) {
		if result := context.values[key] {
			if result == 0 { return none }
			return result
		}
	}
	result := tc.sum_variant_type_for_pattern_depth(sum_name_id, variant_name_id, 0) or { u32(0) }
	// Resolution can populate this context recursively; acquire its current tables again.
	mut file_entries := cache.sum_variant_pattern_entries[tc.cur_file] or {
		created := &SumVariantPatternFileEntries{ file: tc.cur_file.clone() }
		cache.sum_variant_pattern_entries[created.file] = created
		created
	}
	mut module_entries := file_entries.modules[tc.cur_module] or {
		created := &SumVariantPatternModuleEntries{ file: file_entries.file, module: tc.cur_module.clone() }
		file_entries.modules[created.module] = created
		created
	}
	module_entries.values[key] = result
	cache.sum_variant_pattern_recent = module_entries
	cache.sum_variant_pattern_recent_count = cache.sum_variant_pattern_entries.len
	if result == 0 { return none }
	return result
}

fn (tc &TypeChecker) sum_variant_type_for_pattern_depth(sum_name_id u32, variant_name_id u32, depth int) ?u32 {
	if depth >= 16 { return none }
	base_id := tc.sum_base_name(sum_name_id)
	variants := tc.sum_types.type_ids(base_id) or { return none }
	variant_shape := flat.source_type_shape(variant_name_id)
	mut candidates := [variant_name_id]
	qvariant := tc.qualify_type_name(variant_name_id)
	if qvariant != variant_name_id { candidates << qvariant }
	if variant_shape.has(.dotted) {
		resolved := tc.resolve_imported_type_text(variant_name_id)
		if resolved != variant_name_id { candidates << resolved }
		// `all_after_last('.')` on a container pattern (`map[string]ast.Value`)
		// strips the container and would resolve to the sum type itself.
		if !variant_shape.has(.bracketed) {
			if unique := tc.unique_qualified_type_name(flat.source_type_syntax(variant_name_id).unqualified()) {
				candidates << unique
			}
		}
	}
	for candidate in candidates.clone() { candidates << tc.alias_target_type_names(candidate) }
	for candidate in candidates {
		if tc.generic_type_name_matches(candidate, sum_name_id)
			|| tc.generic_type_name_matches(candidate, base_id) {
			return sum_name_id
		}
	}
	for variant in variants {
		concrete := tc.concrete_sum_variant_name(sum_name_id, variant)
		for candidate in candidates {
			if tc.generic_type_name_matches(candidate, concrete)
				|| tc.bare_variant_name_matches(candidate, variant, concrete) {
				return concrete
			}
			concrete_syntax := flat.source_type_syntax(concrete)
			if concrete_syntax.trimmed() == concrete && concrete_syntax.bare_generic()
				&& !tc.is_known_type_text(concrete) {
				return candidate
			}
		}
		if nested := tc.nested_sum_variant_type_for_pattern(concrete, candidates, depth) {
			return nested
		}
	}
	return none
}

fn (tc &TypeChecker) alias_target_type_names(name_id u32) []u32 {
	mut result := []u32{}
	mut cur := name_id
	for _ in 0 .. 16 {
		target := tc.alias_target_type_text(cur) or { break }
		target_name := tc.parse_type(target).source_type_id()
		if target_name == 0 || target_name == cur || target_name in result { break }
		result << target_name
		cur = target_name
	}
	return result
}

fn (tc &TypeChecker) alias_target_type_text(name_id u32) ?u32 {
	if target := tc.type_aliases.type_id(name_id) { return target }
	qname := tc.qualify_type_name(name_id)
	if target := tc.type_aliases.type_id(qname) { return target }
	if flat.source_type_shape(name_id).has(.dotted) {
		resolved := tc.resolve_imported_type_text(name_id)
		if target := tc.type_aliases.type_id(resolved) { return target }
		if unique_id := tc.unique_qualified_type_name(flat.source_type_syntax(name_id).unqualified()) {
			if target := tc.type_aliases.type_id(unique_id) { return target }
		}
	}
	return none
}

fn (tc &TypeChecker) nested_sum_variant_type_for_pattern(concrete_id u32, candidates_ids []u32, depth int) ?u32 {
	typ := tc.parse_type(concrete_id)
	nested_name := if typ is SumType {
		typ.source_type_id()
	} else if typ is Alias && typ.base_type is SumType {
		(typ.base_type as SumType).source_type_id()
	} else {
		u32(0)
	}
	if nested_name == 0 { return none }
	for candidate in candidates_ids {
		if nested := tc.sum_variant_type_for_pattern_depth(nested_name, candidate, depth + 1) {
			return nested
		}
	}
	return none
}

// match_type_pattern supports match type pattern handling for TypeChecker.
fn (tc &TypeChecker) match_type_pattern(node &flat.Node) ?u32 {
	if node.kind == .array_init && node.typ != 0 {
		return node.typ
	}
	if node.kind == .ident {
		if flat.source_type_shape(node.source_value_id()).has(.optional) {
			return node.source_value_id()
		}
		if flat.source_type_shape(node.typ).has(.option_type) || flat.source_type_shape(node.typ).has(.result_type) {
			return node.typ
		}
		if node.source_value_id() != 0 && node.pos.offset > 0 {
			if file := tc.a.source_files[node.pos.id] {
				if source := tc.source_texts_by_file[file.name] {
					if node.pos.offset < source.len && source[node.pos.offset] in [
						`?`,
						`!`,
					] {
						return flat.source_type_composition_one(if source[node.pos.offset] == `?` {
							.option
						} else {
							.result
						}, node.source_value_id())
					}
					if node.pos.offset <= source.len && source[node.pos.offset - 1] in [
						`?`,
						`!`,
					] {
						return flat.source_type_composition_one(if source[node.pos.offset - 1] == `?` {
							.option
						} else {
							.result
						}, node.source_value_id())
					}
				}
			}
		}
		if flat.source_type_shape(node.source_value_id()).has(.function_compact) || flat.source_type_shape(node.source_value_id()).has(.function_spaced) {
			return tc.qualify_type_text(node.source_value_id())
		}
		if is_builtin_type_name_id(node.source_value_id()) || tc.type_symbol_known(node.source_value_id())
			|| tc.pattern_type_known(node.source_value_id())
			|| flat.source_type_syntax(node.source_value_id()).named_variant()
			|| flat.source_type_syntax(node.source_value_id()).first_capital()
			|| (tc.generated_files.len > 0 && tc.type_name_known_in_current_module(node.source_value_id())) {
			return node.source_value_id()
		}
		return none
	}
	if node.kind == .selector && node.children_count > 0 {
		base := tc.a.child_node(node, 0)
		if base.kind == .ident && !tc.ident_resolves_to_value(base.value_text()) {
			pattern_id := flat.source_type_composition_two(.qualified, base.source_value_id(), node.source_value_id())
			if (base.source_value_id() != checker_source_c && flat.source_type_syntax(node.source_value_id()).first_capital())
				|| tc.type_symbol_known(pattern_id) || tc.pattern_type_known(pattern_id)
				|| flat.source_type_syntax(pattern_id).named_variant() {
				return pattern_id
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
@[inline]
fn (tc &TypeChecker) expr_key(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.node(id)
	if node.kind == .ident {
		value := node.value_text()
		return if valid_string_data(value) { value } else { '' }
	}
	return tc.expr_key_composite(id)
}

fn (tc &TypeChecker) expr_key_composite(id flat.NodeId) string {
	node := tc.a.nodes[int(id)]
	if node.kind in [.as_expr, .paren] && node.children_count > 0 {
		return tc.expr_key(tc.a.child(&node, 0))
	}
	if node.kind == .ident {
		value := node.value_text()
		if valid_string_data(value) {
			return value
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

// expr_key_matches reports whether `tc.expr_key(id) == key`. It compares ident
// and selector keys in place instead of building them.
fn (tc &TypeChecker) expr_key_matches(id flat.NodeId, key string) bool {
	if key.len == 0 {
		return tc.expr_key(id).len == 0
	}
	return tc.expr_key_matches_prefix(id, key, key.len)
}

// expr_key_matches_prefix reports whether `tc.expr_key(id)` equals `key[..end]`,
// for `0 < end <= key.len`.
@[direct_array_access]
fn (tc &TypeChecker) expr_key_matches_prefix(id flat.NodeId, key string, end int) bool {
	if int(id) < 0 {
		return false
	}
	node := tc.a.node(id)
	if node.kind in [.as_expr, .paren] && node.children_count > 0 {
		return tc.expr_key_matches_prefix(tc.a.child(node, 0), key, end)
	}
	if node.kind == .ident {
		value := node.value_text()
		return valid_string_data(value) && value.len == end
			&& string_bytes_equal_at(key, 0, value)
	}
	if node.kind == .selector && node.children_count > 0 {
		field := node.value
		base_end := end - field.len - 1
		if field.len == 0 || base_end <= 0 || !valid_string_data(field) || key[base_end] != `.`
			|| !string_bytes_equal_at(key, base_end + 1, field) {
			return false
		}
		return tc.expr_key_matches_prefix(tc.a.child(node, 0), key, base_end)
	}
	if node.kind == .index && node.children_count >= 2 {
		part := tc.a.child_node(node, 1)
		if part.kind in [.ident, .int_literal, .string_literal, .char_literal, .enum_val] {
			part_value := if part.kind == .ident { part.value_text() } else { part.value }
			base_end := end - part_value.len - 2
			if part_value.len == 0 || base_end <= 0 || key[base_end] != `[` || key[end - 1] != `]`
				|| !string_bytes_equal_at(key, base_end + 1, part_value) {
				return false
			}
			return tc.expr_key_matches_prefix(tc.a.child(node, 0), key, base_end)
		}
		full := tc.expr_key(id)
		return full.len == end && string_bytes_equal_at(key, 0, full)
	}
	return false
}

// string_bytes_equal_at reports whether `s` holds `part` at byte offset `at`.
@[direct_array_access; inline]
fn string_bytes_equal_at(s string, at int, part string) bool {
	if at < 0 || at + part.len > s.len {
		return false
	}
	for i in 0 .. part.len {
		if s[at + i] != part[i] {
			return false
		}
	}
	return true
}

fn (tc &TypeChecker) expr_key_part(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := tc.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value_text()
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
@[direct_array_access; inline]
fn (tc &TypeChecker) smartcast_type(id flat.NodeId) ?Type {
	idx := int(id)
	if idx < 0 || idx >= tc.a.nodes.len || tc.a.nodes[idx].kind !in [.ident, .selector, .index] {
		return none
	}
	mut cache := tc.type_cache
	// Most checker contexts have no active dynamic smartcast. Consult the
	// node-indexed lexical cache first so repeated type queries do not rebuild an
	// expression key merely to rediscover the same lexical hit or miss.
	if tc.smartcasts.len == 0 && idx < tc.lexical_smartcast_misses.len
		&& tc.lexical_smartcast_misses[idx] {
		return none
	}
	// Lexical smartcasts belong to parsed source nodes. Transform-created nodes
	// are appended after the parent index was built and carry explicit/synthetic
	// types; walking the full arena to rediscover a parent for each such lookup
	// makes self-host transformation quadratic.
	parsed := idx < tc.direct_parent_ids.len
	// Neither the expression key nor the dynamic smartcast lookup below changes
	// the cache, so one read of the cached state serves both of its checks.
	state := if parsed && !isnil(cache) { cache.lexical_smartcast_state(idx) } else { u32(0) }
	if tc.smartcasts.len == 0 && state != 0 {
		if state == 1 {
			return none
		}
		return *cache.lexical_smartcast_types[int(state) - 2]
	}
	return tc.smartcast_type_miss(id, parsed, state)
}

@[direct_array_access]
fn (tc &TypeChecker) smartcast_type_miss(id flat.NodeId, parsed bool, state u32) ?Type {
	idx := int(id)
	mut cache := tc.type_cache
	if tc.smartcasts.len == 0 && parsed && tc.lexical_smartcasts_unused(idx) {
		tc.remember_lexical_smartcast_miss(idx)
		return none
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
	if !parsed {
		return none
	}
	if idx < tc.lexical_smartcast_misses.len && tc.lexical_smartcast_misses[idx] {
		return none
	}
	if state == 1 {
		return none
	}
	if state != 0 {
		return *cache.lexical_smartcast_types[int(state) - 2]
	}
	mut found := ?Type(none)
	if !tc.lexical_smartcast_key_unused(idx, key) {
		found = tc.lexical_smartcast_type(id, key)
	}
	result := found or {
		tc.remember_lexical_smartcast_miss(idx)
		return none
	}
	if !isnil(cache) {
		cache.lexical_smartcast_types << tc.intern_type_reference(result)
		cache.set_lexical_smartcast_state(idx, u32(cache.lexical_smartcast_types.len + 1))
	}
	return result
}

fn (tc &TypeChecker) remember_lexical_smartcast_miss(idx int) {
	if isnil(tc.storage_query_probe) && !tc.resolution_type_mode && idx < tc.lexical_smartcast_misses.len
		&& (!tc.parallel_check_sparse || (idx >= tc.check_range_lo && idx <= tc.check_range_hi)) {
		mut writable := unsafe { tc }
		writable.lexical_smartcast_misses[idx] = true
	}
	if !isnil(tc.type_cache) {
		mut cache := tc.type_cache
		cache.set_lexical_smartcast_state(idx, 1)
	}
}

fn (tc &TypeChecker) lexical_smartcast_type(id flat.NodeId, key string) ?Type {
	return tc.lexical_smartcast_type_in_parents(id, key, false)
}

// Reject unrelated conditions before resolving their types or allocating bindings.
// Identifier conditions can refer to a saved boolean expression, so stay conservative.
fn (tc &TypeChecker) condition_may_smartcast_key(id flat.NodeId, key string) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	match node.kind {
		.ident {
			return true
		}
		.paren, .prefix {
			return node.children_count > 0
				&& tc.condition_may_smartcast_key(tc.a.child(node, 0), key)
		}
		.is_expr {
			return node.children_count > 0 && tc.expr_key_matches(tc.a.child(node, 0), key)
		}
		.infix {
			if node.children_count < 2 {
				return false
			}
			lhs := tc.a.child(node, 0)
			rhs := tc.a.child(node, 1)
			if node.op in [.logical_and, .logical_or] {
				return tc.condition_may_smartcast_key(lhs, key) || tc.condition_may_smartcast_key(rhs, key)
			}
			if node.op in [.eq, .ne] {
				if tc.a.node(rhs).kind in [.none_expr, .nil_literal] {
					return tc.expr_key_matches(lhs, key)
				}
				if tc.a.node(lhs).kind in [.none_expr, .nil_literal] {
					return tc.expr_key_matches(rhs, key)
				}
			}
		}
		else {}
	}
	return false
}

// Walk the parent chain once, returning the nearest valid narrowing. Separate
// if/for/match walks repeated the same parent lookups and evaluated outer
// conditions even when a closer branch already determined the type.
fn (tc &TypeChecker) lexical_smartcast_type_in_parents(id flat.NodeId, key string, match_only bool) ?Type {
	idx := int(id)
	if idx < 0 || idx >= tc.direct_parent_ids.len || key == '' || !valid_string_data(key) {
		return none
	}
	mut current := id
	mut parent_id := flat.empty_node
	mut match_branch := flat.NodeId(-1)
	mut depth := 1
	// Every query resolves its narrowing from the current scope. Only the
	// parent links are shared across repeated queries and function bodies.
	use_parent_links := tc.cache_lexical_parents && tc.direct_parent_index_trusted
	if use_parent_links {
		linked_parent, linked_child, linked_depth := tc.lexical_parent_link(id)
		parent_id = linked_parent
		current = linked_child
		depth = linked_depth
	} else {
		parent_id = tc.direct_parent_id(current)
	}
	for tc.valid_node_id(parent_id) {
		if match_only && depth > 64 {
			return none
		}
		parent := tc.a.node(parent_id)
		if !match_only && parent.kind == .if_expr && parent.children_count >= 2 {
			mut branch_index := -1
			for i in 1 .. parent.children_count {
				if tc.a.child(parent, i) == current {
					branch_index = i
					break
				}
			}
			if branch_index >= 1 && tc.condition_may_smartcast_key(tc.a.child(parent, 0), key) {
				cond_id := tc.a.child(parent, 0)
				bindings := if branch_index == 1 {
					tc.extract_smartcasts(cond_id)
				} else {
					tc.extract_else_branch_smartcasts(cond_id)
				}
				for binding in bindings {
					branch_id := tc.a.child(parent, branch_index)
					if binding.name == key
						&& !tc.branch_writes_key_before(branch_id, id, key, binding.typ) {
						return binding.typ
					}
				}
			}
		} else if !match_only && parent.kind == .for_stmt && parent.children_count > 3 {
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
			// Only reconstruct the condition smartcast when no statement before `id`
			// in this iteration writes `key`.
			if body_index >= 3 && tc.condition_may_smartcast_key(tc.a.child(parent, 1), key) {
				for binding in tc.extract_smartcasts(tc.a.child(parent, 1)) {
					if binding.name == key
						&& !tc.for_body_writes_key_before(parent, body_index, current, id, key,
							binding.typ) {
						return binding.typ
					}
				}
			}
		} else if depth <= 64 && parent.kind == .match_branch {
			match_branch = parent_id
		} else if depth <= 64 && parent.kind == .match_stmt && tc.valid_node_id(match_branch) {
			branch := tc.a.node(match_branch)
			if typ := tc.lexical_match_branch_smartcast_type(parent, branch, key) {
				if tc.branch_writes_key_before(match_branch, id, key, typ) {
					return none
				}
				return typ
			}
			// An unrelated inner match leaves an enclosing narrowing available.
			match_branch = flat.empty_node
		}
		if parent.kind in [.fn_decl, .fn_literal, .lambda_expr] {
			return none
		}
		current = parent_id
		if use_parent_links {
			linked_parent, linked_child, linked_depth := tc.lexical_parent_link(current)
			parent_id = linked_parent
			current = linked_child
			depth += linked_depth
		} else {
			parent_id = tc.direct_parent_id(current)
			depth++
		}
	}
	return none
}

// for_body_writes_key_before reports whether any loop-body statement before
// the checked `target` within `current` ends the narrowing of `key` to `narrowed`
// (see subtree_ends_narrowing). Such a write invalidates the loop-condition
// narrowing for the rest of that iteration, so the lexical fallback must not restore it.
fn (tc &TypeChecker) for_body_writes_key_before(parent flat.Node, body_index int, current flat.NodeId, target flat.NodeId, key string, narrowed Type) bool {
	for i in 3 .. body_index {
		if tc.subtree_ends_narrowing(tc.a.child(parent, i), key, narrowed, true) {
			return true
		}
	}
	// The current assignment has not stored its value while its RHS is resolved.
	// Resolving it here would query the same loop smartcast recursively.
	return tc.subtree_writes_key_before(current, target, key, narrowed, true)
}

// branch_writes_key_before reports whether a statement that runs before `target`
// in a branch ends the narrowing of `key` to `narrowed`: an earlier statement of
// the branch, or an assignment in an earlier statement of a block in the branch
// that holds `target` (`if c { x = 'a' ... }` inside `if mut x is int`). A call
// that may write `key` through a `mut` argument or receiver ends it only as a
// statement of an `if` branch itself, as it always did: code relies on the
// narrowing surviving such calls in nested blocks and in `match` branches.
fn (tc &TypeChecker) branch_writes_key_before(branch_id flat.NodeId, target flat.NodeId, key string, narrowed Type) bool {
	if !tc.valid_node_id(branch_id) || !tc.valid_node_id(target) {
		return false
	}
	branch := tc.a.node(branch_id)
	if branch.kind !in [.block, .match_branch] {
		return false
	}
	return tc.subtree_writes_key_before(branch_id, target, key, narrowed, branch.kind == .block)
}

fn (tc &TypeChecker) subtree_writes_key_before(root flat.NodeId, target flat.NodeId, key string, narrowed Type, calls bool) bool {
	root_kind := tc.a.node(root).kind
	mut child := target
	if child == root {
		return false
	}
	for _ in 0 .. 128 {
		parent_id := tc.direct_parent_id(child)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		if parent_id == root {
			return root_kind in [.block, .match_branch]
				&& tc.statements_before_end_narrowing(parent_id, child, key, narrowed, calls)
		}
		if tc.a.node(parent_id).kind in [.block, .match_branch, .for_stmt, .for_in_stmt, .lock_expr,
			.defer_stmt, .select_branch]
			&& tc.statements_before_end_narrowing(parent_id, child, key, narrowed, false) {
			return true
		}
		child = parent_id
	}
	return tc.statements_before_end_narrowing(root, flat.empty_node, key, narrowed, calls)
}

// statements_before_end_narrowing reports whether a child of `parent_id` before
// `child` ends the narrowing of `key` to `narrowed` (see subtree_ends_narrowing).
fn (tc &TypeChecker) statements_before_end_narrowing(parent_id flat.NodeId, child flat.NodeId, key string, narrowed Type, calls bool) bool {
	parent := tc.a.node(parent_id)
	for i in 0 .. parent.children_count {
		child_id := tc.a.child(parent, i)
		if child_id == child {
			return false
		}
		if tc.subtree_ends_narrowing(child_id, key, narrowed, calls) {
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
	if write_key == '' {
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
	if key == '' {
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
	return tc.subtree_ends_narrowing(root, key, none, true)
}

// write_keeps_narrowing reports whether the assignment `node`, pair `i`, which
// writes the narrowed value itself (`exact`), keeps it narrowed to `narrowed`.
fn (tc &TypeChecker) write_keeps_narrowing(node &flat.Node, i int, exact bool, narrowed ?Type) bool {
	if !exact {
		return false
	}
	typ := narrowed or { return false }
	return tc.assignment_keeps_narrowing(node.op, tc.a.child(node, i), tc.a.child(node, i + 1),
		typ)
}

// subtree_ends_narrowing is subtree_assigns_key for a `key` narrowed to `narrowed`:
// an assignment to `key` itself that keeps it narrowed (see
// assignment_keeps_narrowing) is not a write. Without `narrowed`, every
// assignment is one. Mutating calls count only with `calls`.
fn (tc &TypeChecker) subtree_ends_narrowing(root flat.NodeId, key string, narrowed ?Type, calls bool) bool {
	if !tc.valid_node_id(root) {
		return false
	}
	node := tc.a.node(root)
	// The parser gives `b.v = x` and `a[i] = x` their own kinds.
	if node.kind in [.assign, .selector_assign, .index_assign] {
		mut i := 0
		for i + 1 < node.children_count {
			write_key := tc.expr_key(tc.a.child(node, i))
			if tc.write_invalidates_key(write_key, key)
				&& !tc.write_keeps_narrowing(node, i, write_key == key, narrowed) {
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
	} else if node.kind == .call && calls {
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
		if tc.a.node(child_id).children_count > 0
			&& tc.subtree_ends_narrowing(child_id, key, narrowed, calls) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) lexical_match_smartcast_type(id flat.NodeId) ?Type {
	return tc.lexical_smartcast_type_in_parents(id, tc.expr_key(id), true)
}

fn (tc &TypeChecker) lexical_match_branch_smartcast_type(parent &flat.Node, branch &flat.Node, key string) ?Type {
	if branch.value == 'else' || branch.value.int() != 1 || branch.children_count == 0
		|| parent.children_count == 0 {
		return none
	}
	subject_id := tc.a.child(parent, 0)
	if !tc.expr_key_matches(subject_id, key) {
		return none
	}
	subject_type := unalias_and_unwrap_pointer_type(tc.resolve_type(subject_id))
	cond := tc.a.child_node(branch, 0)
	pattern_id := tc.match_type_pattern(cond) or { return none }
	name_id := if subject_type is SumType {
		tc.sum_variant_type_for_pattern(subject_type.source_type_id(), pattern_id) or { return none }
	} else if is_ierror_type(subject_type) {
		tc.resolve_ierror_match_pattern(pattern_id) or { return none }
	} else if subject_type is Interface {
		tc.resolve_interface_match_pattern(pattern_id) or { return none }
	} else if flat.source_type_syntax(subject_type.source_type_id()).unqualified() == flat.source_type_syntax(pattern_id).unqualified() {
		// During the branch check, the dynamic smartcast already exposes the
		// matched variant. Keep recognizing the lexical match so declarations
		// inside nested sum matches infer that concrete variant.
		subject_type.source_type_id()
	} else {
		return none
	}
	return *tc.parse_type(name_id)
}

fn (tc &TypeChecker) smartcast_target_type_for_is_expr(expr_id flat.NodeId, pattern_id u32) Type {
	raw_subject := unalias_type(tc.resolve_type(expr_id))
	subject := unalias_and_unwrap_pointer_type(raw_subject)
	if subject is SumType {
		if variant_source_id := tc.sum_variant_type_for_pattern(subject.source_type_id(), pattern_id) {
			return *tc.parse_type(variant_source_id)
		}
	}
	if is_ierror_type(subject) {
		if variant_source_id := tc.resolve_ierror_match_pattern(pattern_id) {
			return *tc.parse_type(variant_source_id)
		}
	}
	if subject is Interface {
		if variant_source_id := tc.resolve_interface_match_pattern(pattern_id) {
			variant_type := tc.parse_type(variant_source_id)
			if tc.expr_has_explicit_mut_marker(expr_id) && raw_subject is Pointer {
				return Type(Pointer{
					base_type: tc.intern_type_reference(variant_type)
				})
			}
			return interface_smartcast_variant_type(variant_type)
		}
	}
	return *tc.parse_type(pattern_id)
}

fn interface_smartcast_variant_type(variant_type Type) Type {
	if unalias_type(variant_type) is Struct {
		return Type(Pointer{ base_type: new_type_reference(Type(variant_type)) })
	}
	return variant_type
}

fn (mut tc TypeChecker) invalidate_smartcasts_for_write_key(key string) {
	if key == '' {
		return
	}
	tc.smartcasts.delete(key)
	tc.invalidate_smartcast_descendants_for_write_key(key)
}

// invalidate_smartcast_descendants_for_write_key ends the narrowings of the fields
// of `key` (`b.v` for `b`) after an assignment that keeps `key` itself narrowed:
// the new value has the same variant, but its fields can hold any of theirs.
fn (mut tc TypeChecker) invalidate_smartcast_descendants_for_write_key(key string) {
	if key == '' {
		return
	}
	prefix := '${key}.'
	for child_key in tc.smartcasts.keys() {
		if child_key.starts_with(prefix) {
			tc.smartcasts.delete(child_key)
		}
	}
}

// store_c_name records `c_name`, naming.c_name(name) computed elsewhere, the way
// cached_c_name would, and returns it.
fn (tc &TypeChecker) store_c_name(name string, c_name string) string {
	if !isnil(tc.type_cache) {
		mut cache := unsafe { tc.type_cache }
		cache.c_name_entries[name] = c_name
	}
	return c_name
}

// cached_c_name memoizes naming.c_name results in the type cache (falling
// back to the frozen base cache read-only, like every other entry kind).
// c_name is pure and called on hot resolution paths in every phase.
pub fn (tc &TypeChecker) cached_c_name(name string) string {
	if isnil(tc.type_cache) {
		return naming.c_name(name)
	}
	mut cache := unsafe { tc.type_cache }
	if cached := cache.c_name_entries[name] {
		return cached
	}
	mut fallback := cache.base
	for !isnil(fallback) {
		if cached := fallback.c_name_entries[name] {
			return cached
		}
		fallback = fallback.base
	}
	result := naming.c_name(name)
	cache.c_name_entries[name] = result
	return result
}

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

// parse_type resolves an interned source spelling in the current checker context.
@[inline]
pub fn (tc &TypeChecker) parse_type(source_id u32) &Type {
	if tc.type_param_texts.ids.len > 0 {
		if resolved := tc.resolved_inferred_type(source_id) {
			return tc.parse_type(resolved)
		}
		return tc.intern_type_reference(tc.parse_type_as_instance(source_id))
	}
	// Recursive aliases must observe the active expansion chain before memoization.
	if !isnil(tc.type_cache) && tc.type_cache.alias_parse_stack.len > 0 {
		if recursive_alias := tc.recursive_alias_reference(source_id) {
			return tc.intern_type_reference(recursive_alias)
		}
	}
	if tc.type_cache != unsafe { nil } && tc.type_cache.parse_enabled {
		mut cache := unsafe { tc.type_cache }
		if scalar := tc.parsed_scalar_type(source_id) {
			cache.parse_hits++
			return scalar
		}
		if cached := parse_type_cache_get_mode(mut cache, tc.cur_file, tc.cur_module, source_id, tc.fn_context.generic_params, tc.resolution_type_mode, tc.fast_parse_recent) {
			cache.parse_hits++
			return cached
		}
	}
	return tc.parse_type_miss(source_id)
}

fn (tc &TypeChecker) parse_type_miss(source_id u32) &Type {
	if resolved := tc.resolved_inferred_type(source_id) {
		return tc.parse_type(resolved)
	}
	if tc.type_cache != unsafe { nil } && tc.type_cache.parse_enabled {
		mut cache := unsafe { tc.type_cache }
		// typeof depends on expression scope.
		if flat.source_type_shape(source_id).has(.typeof_expr) {
			return tc.intern_type_reference(tc.parse_type_uncached(source_id))
		}
		cache.parse_misses++
		result := tc.intern_type_reference(tc.parse_type_uncached(source_id))
		if scalar_builtin_type_name_id(source_id) {
			id, _ := tc.intern_type(*result)
			tc.remember_parsed_scalar_type(source_id, id, result)
		}
		if tc.fast_parse_recent {
			parse_type_cache_put_recent(mut cache, tc.cur_file, tc.cur_module, source_id, tc.fn_context.generic_params, tc.resolution_type_mode, result)
		} else {
			parse_type_cache_put(mut cache, tc.cur_file, tc.cur_module, source_id, tc.fn_context.generic_params, tc.resolution_type_mode, result)
		}
		return result
	}
	if flat.source_type_shape(source_id).has(.typeof_expr) {
		return tc.intern_type_reference(tc.parse_type_uncached(source_id))
	}
	result := tc.intern_type_reference(tc.parse_type_uncached(source_id))
	return result
}

// parse_type_ref resolves a node annotation through its exact canonical text
// identity. Context remains part of the key because an unqualified spelling
// can denote different types in different files/modules.
@[inline]
pub fn (tc &TypeChecker) parse_type_ref(text_id u32) &Type {
	if text_id == 0 || !tc.fast_type_text_refs || tc.type_cache == unsafe { nil }
		|| !tc.type_cache.parse_enabled {
		return tc.parse_type(text_id)
	}
	// Recursive aliases must observe the active expansion chain before any
	// memoized value, matching parse_type's ordering.
	if tc.type_cache.alias_parse_stack.len > 0 {
		if recursive_alias := tc.recursive_alias_reference(text_id) {
			return tc.intern_type_reference(recursive_alias)
		}
	}
	mut cache := unsafe { tc.type_cache }
	if cache.parse_text_ids.len > 0 {
		slot := int(text_id) & 4095
		context_hash := parse_type_cache_context_hash(mut cache, tc.cur_file, tc.cur_module, tc.fn_context.generic_params, tc.resolution_type_mode)
		if cache.parse_text_ids[slot] == text_id && cache.parse_text_id_context[slot] == context_hash {
			cache.parse_hits++
			return cache.parse_text_id_values[slot]
		}
	}
	return tc.parse_type_ref_miss(text_id)
}

fn (tc &TypeChecker) parse_type_ref_miss(text_id u32) &Type {
	// `typeof(expr)` types depend on lexical bindings, unlike ordinary
	// canonical type texts. Collection may see them before those locals exist,
	// so they never enter the text-ID cache.
	if inferred_type_source(text_id) {
		return tc.parse_type(text_id)
	}
	mut cache := unsafe { tc.type_cache }
	if cache.parse_text_ids.len == 0 {
		cache.parse_text_id_context = []u64{len: 4096}
		cache.parse_text_id_values = unsafe { []&Type{len: 4096} }
		cache.parse_text_ids = []u32{len: 4096}
	}
	slot := int(text_id) & 4095
	context_hash := parse_type_cache_context_hash(mut cache, tc.cur_file, tc.cur_module, tc.fn_context.generic_params, tc.resolution_type_mode)
	result := tc.parse_type(text_id)
	cache.parse_text_id_context[slot] = context_hash
	cache.parse_text_id_values[slot] = result
	cache.parse_text_ids[slot] = text_id
	return result
}

// parse_canonical_type_cached is parse_canonical_type memoized per parse
// context, like parse_type's own cache.
pub fn (tc &TypeChecker) parse_canonical_type_cached(source_id u32) &Type {
	if source_id == 0 || !tc.fast_type_text_refs || tc.type_cache == unsafe { nil }
		|| !tc.type_cache.parse_enabled || tc.type_cache.alias_parse_stack.len > 0 {
		return tc.intern_type_reference(tc.parse_canonical_type(source_id))
	}
	mut cache := unsafe { tc.type_cache }
	if cache.canonical_texts.len == 0 {
		cache.canonical_texts = []u32{len: canonical_type_cache_slots}
		cache.canonical_contexts = []u64{len: canonical_type_cache_slots}
		cache.canonical_values = unsafe { []&Type{len: canonical_type_cache_slots} }
	}
	context_hash := parse_type_cache_context_hash(mut cache, tc.cur_file, tc.cur_module, tc.fn_context.generic_params, tc.resolution_type_mode)
	slot := canonical_type_cache_slot(source_id, context_hash)
	if cache.canonical_contexts[slot] == context_hash && cache.canonical_texts[slot] == source_id {
		return cache.canonical_values[slot]
	}
	// Scope-dependent typeof texts never enter the cache.
	if flat.source_type_shape(source_id).has(.typeof_expr) {
		return tc.intern_type_reference(tc.parse_canonical_type(source_id))
	}
	result := tc.intern_type_reference(tc.parse_canonical_type(source_id))
	if !type_contains_unknown(result) {
		cache.canonical_texts[slot] = source_id
		cache.canonical_contexts[slot] = context_hash
		cache.canonical_values[slot] = result
	}
	return result
}

const canonical_type_cache_slots = 4096

// canonical_type_cache_slot combines an exact spelling identity with its resolution context.
@[direct_array_access; inline]
fn canonical_type_cache_slot(source_id u32, context_hash u64) int {
	hash := (u64(source_id) * 1_099_511_628_211) ^ context_hash
	return int((hash ^ (hash >> 29)) & u64(canonical_type_cache_slots - 1))
}

fn (tc &TypeChecker) recursive_alias_reference(typ_id u32) ?Type {
	if isnil(tc.type_cache) || tc.type_cache.alias_parse_stack.len == 0 { return none }
	clean := trimmed_source_type_id(typ_id)
	shape := flat.source_type_shape(clean)
	if clean == 0 || shape.has(.pointer) || shape.has(.optional) || shape.has(.leading_bracket)
		|| flat.source_type_syntax(clean).paren_prefix() || shape.has(.mutable_type) || shape.has(.shared_type)
		|| shape.has(.atomic_type) || shape.has(.channel_type) || shape.has(.map_type) || shape.has(.function_compact) {
		return none
	}
	qualified := if shape.has(.dotted) { clean } else { tc.qualify_type_name(clean) }
	for i := tc.type_cache.alias_parse_stack.len - 1; i >= 0; i-- {
		name_id := tc.type_cache.alias_parse_stack[i]
		if clean == name_id || qualified == name_id || clean == flat.source_type_syntax(name_id).unqualified() {
			return Type(Alias{
				name_id:   name_id
				base_type: tc.intern_type_reference(unknown_type_quoting_id('recursive alias', name_id))
			})
		}
	}
	return none
}

fn (tc &TypeChecker) parse_alias_type(name_id u32, target_id u32) Type {
	mut cache := unsafe { tc.type_cache }
	if isnil(cache) {
		return Type(Alias{
			name_id:   name_id
			base_type: tc.intern_type_reference(tc.parse_alias_target_type(name_id, target_id))
		})
	}
	cache.alias_parse_stack << name_id
	base_type := tc.parse_alias_target_type(name_id, target_id)
	cache.alias_parse_stack.delete_last()
	return Type(Alias{
		name_id:   name_id
		base_type: tc.intern_type_reference(base_type)
	})
}

fn (tc &TypeChecker) parse_alias_target_type(name_id u32, target_id u32) Type {
	if context_independent_type(target_id) { return *tc.parse_type(target_id) }
	clean := trimmed_source_type_id(target_id)
	// Collected alias targets preserve their declaration's exact qualified symbol.
	if tc.qualify_candidate_type_exists(clean) {
		return tc.parse_canonical_type(clean)
	}
	if flat.source_type_shape(clean).has(.shared_type) {
		return Type(Pointer{ base_type: tc.intern_type_reference(tc.parse_alias_target_type(name_id, trimmed_source_type_id(flat.source_type_syntax(clean).raw_base()))) })
	}
	decl_module := tc.type_alias_modules[name_id] or {
		tc.type_alias_modules[flat.source_type_syntax(name_id).generic_base()] or { tc.cur_module }
	}
	if decl_module != tc.cur_module {
		mut scoped := tc.fork_type_parse_view(tc.cur_file, decl_module)
		return *scoped.parse_type(target_id)
	}
	return *tc.parse_type(target_id)
}

// parse_canonical_generic_type preserves the semantic base and recursively
// canonicalizes arguments of a compiler-produced generic application.
fn (tc &TypeChecker) parse_canonical_generic_type(typ u32) ?Type {
	syntax := flat.source_type_syntax(typ)
	if !syntax.application_valid() || !syntax.bracket_closed() || syntax.bracket_suffix() != 0 {
		return none
	}
	mut canonical_parts := []u32{cap: syntax.application_args().len + 1}
	canonical_parts << syntax.application_base()
	for arg in syntax.application_args() {
		clean_arg_id := trimmed_source_type_id(arg)
		parsed_arg := tc.parse_canonical_type(clean_arg_id)
		canonical_arg_id := if parsed_arg is Unknown {
			clean_arg_id
		} else {
			tc.type_name(parsed_arg)
		}
		canonical_parts << canonical_arg_id
	}
	canonical_name_id := flat.source_type_composition(.generic, canonical_parts)
	base := syntax.application_base()
	mut result := Type(Unknown{})
	if tc.type_aliases.contains(base) {
		params := tc.type_alias_generic_params[base] or {
			tc.type_alias_generic_params[flat.source_type_syntax(syntax.application_base()).unqualified()] or { []string{} }
		}
		target_source_id := tc.type_aliases.value_id(base)
		result = Type(Alias{
			name_id:   canonical_name_id
			base_type: tc.intern_type_reference(if params.len == syntax.application_args().len && params.len > 0 {
				tc.parse_alias_target_type(syntax.application_base(), subst_generic_text(target_source_id, canonical_parts[1..], params))
			} else {
				tc.parse_alias_target_type(syntax.application_base(), target_source_id)
			})
		})
	} else if base in tc.structs || base in tc.struct_generic_params {
		result = Type(Struct{
			name_id: canonical_name_id
		})
	} else if base in tc.interface_names {
		result = Type(Interface{
			name_id: canonical_name_id
		})
	} else if tc.sum_types.contains(base) || base in tc.sum_generic_params {
		result = Type(SumType{
			name_id: canonical_name_id
		})
	} else {
		return none
	}
	_, canonical := tc.intern_type(result)
	return *canonical
}

// parse_canonical_type parses compiler-produced type text while preserving an
// exact known qualified symbol before consulting the current file's import
// aliases. Source text must continue to use parse_type, where aliases take
// precedence; this entry point is for semantic names carried between phases.
pub fn (tc &TypeChecker) parse_canonical_type(source_id u32) Type {
	clean_id := trimmed_source_type_id(source_id)
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) {
		_, result := tc.intern_type(Type(Pointer{
			base_type: tc.intern_type_reference(tc.parse_canonical_type(flat.source_type_syntax(clean_id).raw_base()))
		}))
		return *result
	}
	if shape.has(.mutable_type) {
		_, result := tc.intern_type(Type(Pointer{
			base_type: tc.intern_type_reference(tc.parse_canonical_type(flat.source_type_syntax(clean_id).raw_base()))
		}))
		return *result
	}
	if shape.has(.shared_type) {
		return tc.parse_canonical_type(flat.source_type_syntax(clean_id).raw_base())
	}
	if shape.has(.variadic) {
		_, result := tc.intern_type(Type(Array{
			elem_type: tc.intern_type_reference(tc.parse_canonical_type(flat.source_type_syntax(clean_id).raw_base()))
		}))
		return *result
	}
	if shape.has(.array_type) {
		_, result := tc.intern_type(Type(Array{
			elem_type: tc.intern_type_reference(tc.parse_canonical_type(flat.source_type_syntax(clean_id).raw_base()))
		}))
		return *result
	}
	if shape.has(.option_type) {
		base_type := tc.parse_canonical_type(flat.source_type_syntax(clean_id).raw_base())
		if base_type is Alias && unalias_type(base_type) is OptionType {
			return base_type
		}
		_, result := tc.intern_type(Type(OptionType{
			base_type: tc.intern_type_reference(base_type)
		}))
		return *result
	}
	if shape.has(.result_type) {
		_, result := tc.intern_type(Type(ResultType{
			base_type: tc.intern_type_reference(tc.parse_canonical_type(flat.source_type_syntax(clean_id).raw_base()))
		}))
		return *result
	}
	if shape.has(.bracketed) {
		if generic := tc.parse_canonical_generic_type(clean_id) {
			return generic
		}
	}
	if is_builtin_type_name_id(clean_id) {
		return *tc.parse_type(clean_id)
	}
	if known := tc.type_from_known_symbol(clean_id) {
		_, result := tc.intern_type(known)
		return *result
	}
	return *tc.parse_type(clean_id)
}

@[inline]
fn (tc &TypeChecker) probe_intern_type(t &Type) ?&Type {
	if isnil(tc.type_interner) {
		return none
	}
	return tc.type_interner.probe(t)
}

@[inline]
fn (tc &TypeChecker) intern_type(t &Type) (TypeId, &Type) {
	mut interner := unsafe { tc.type_interner }
	return interner.canonicalize(t)
}

// intern_type_reference returns the owned canonical reference for a semantic type.
@[inline]
pub fn (tc &TypeChecker) intern_type_reference(t &Type) &Type {
	mut interner := unsafe { tc.type_interner }
	_, value := interner.canonicalize(t)
	return value
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

// type_name returns the interned canonical spelling of a semantic type.
// Composite spellings are constructed once and memoized by semantic identity.
@[inline]
pub fn (tc &TypeChecker) type_name(t &Type) u32 {
	match t {
		Primitive, Void, Unknown, Nil, None, String, Char, Rune, ISize, USize,
		Struct, Alias, Enum, SumType, Interface {
			return t.source_type_id()
		}
		else { return tc.composite_type_name(t) }
	}
}

fn (tc &TypeChecker) composite_type_name(t &Type) u32 {
	if isnil(tc.type_interner) {
		return t.source_type_id()
	}
	mut cache := unsafe { tc.type_cache }
	if !isnil(cache) {
		hash, slot := type_recent_hash_slot(t)
		if cache.name_recent_set[slot] && cache.name_recent_hashes[slot] == hash
			&& semantic_types_equal(cache.name_recent_types[slot], t) {
			return cache.name_recent_vals[slot]
		}
		mut interner := unsafe { tc.type_interner }
		id, canonical := interner.canonicalize(t)
		result := interner.source_type_id(id)
		cache.name_recent_hashes[slot] = hash
		cache.name_recent_types[slot] = canonical
		cache.name_recent_vals[slot] = result
		cache.name_recent_set[slot] = true
		return result
	}
	id, _ := tc.intern_type(t)
	mut interner := unsafe { tc.type_interner }
	return interner.source_type_id(id)
}

// type_recent_hash_slot uses a cheap fingerprint; callers verify structural equality.
@[inline]
fn type_recent_hash_slot(typ &Type) (u64, int) {
	hash := canonical_intern_cache_key(typ)
	return hash, int(hash & u64(type_cache_recent_slots - 1))
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

@[inline]
fn parse_type_cache_context_hash(mut cache TypeCache, file string, module_name string, generic_params []string, resolution bool) u64 {
	if cache.parse_context_valid && cache.parse_context_resolution == resolution
		&& parse_type_cache_string_matches(cache.parse_context_file, file)
		&& parse_type_cache_string_matches(cache.parse_context_module, module_name)
		&& (if generic_params.len == 0 {
			cache.parse_context_generics.len == 0
		} else {
			parse_type_cache_strings_match(cache.parse_context_generics, generic_params)
		}) {
		return cache.parse_context_hash
	}
	return parse_type_cache_context_hash_miss(mut cache, file, module_name, generic_params, resolution)
}

fn parse_type_cache_context_hash_miss(mut cache TypeCache, file string, module_name string, generic_params []string, resolution bool) u64 {
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

@[direct_array_access]
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

@[direct_array_access; inline]
fn parse_type_cache_key(context_hash u64, source_id u32) u64 {
	// Exact ID/context verification below retains collision and overlay semantics.
	return (context_hash ^ u64(source_id)) * u64(1_099_511_628_211)
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

@[direct_array_access]
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

@[inline]
fn parse_type_cache_entry_matches(entry &ParseTypeCacheEntry, file string, module_name string, text u32, generic_params []string, resolution bool) bool {
	return entry.resolution == resolution && entry.text == text
		&& parse_type_cache_string_matches(entry.file, file)
		&& parse_type_cache_string_matches(entry.module, module_name)
		&& parse_type_cache_strings_match(entry.generic_params, generic_params)
}

fn parse_type_cache_get(mut cache TypeCache, file string, module_name string, text u32, generic_params []string, resolution bool) ?&Type {
	if cache.parse_last_valid
		&& parse_type_cache_entry_matches(cache.parse_last_entry, file, module_name, text, generic_params, resolution) {
		return cache.parse_last_entry.typ
	}
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params, resolution)
	mut key := parse_type_cache_key(context_hash, text)
	for {
		if entry := cache.parse_entries[key] {
			if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params, resolution) {
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
				if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params, resolution) {
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
fn parse_type_cache_get_mode(mut cache TypeCache, file string, module_name string, text u32, generic_params []string, resolution bool, fast_recent bool) ?&Type {
	if fast_recent {
		return parse_type_cache_get_recent(mut cache, file, module_name, text, generic_params, resolution)
	}
	return parse_type_cache_get(mut cache, file, module_name, text, generic_params, resolution)
}

@[inline]
fn parse_type_cache_value_recent_slot(text u32, context_hash u64) int {
	return int((u64(text) ^ context_hash) & u64(type_cache_recent_slots - 1))
}

@[direct_array_access]
fn parse_type_cache_get_recent(mut cache TypeCache, file string, module_name string, text u32, generic_params []string, resolution bool) ?&Type {
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params, resolution)
	slot := parse_type_cache_value_recent_slot(text, context_hash)
	if cache.parse_value_recent_set[slot] && cache.parse_value_recent_ids[slot] == text
		&& cache.parse_value_recent_context[slot] == context_hash {
		return cache.parse_value_recent_values[slot]
	}
	result := parse_type_cache_get(mut cache, file, module_name, text, generic_params, resolution) or {
		return none
	}
	cache.parse_value_recent_ids[slot] = text
	cache.parse_value_recent_context[slot] = context_hash
	cache.parse_value_recent_values[slot] = result
	cache.parse_value_recent_set[slot] = true
	return result
}

// Keep the borrowed parameter's canonical pointer explicit for older bootstrap compilers.
@[inline]
fn cache_type_reference(value &Type) &Type {
	return value
}

fn parse_type_cache_put(mut cache TypeCache, file string, module_name string, text u32, generic_params []string, resolution bool, typ &Type) {
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params, resolution)
	mut key := parse_type_cache_key(context_hash, text)
	for {
		if entry := cache.parse_entries[key] {
			if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params, resolution) {
				return
			}
			key = parse_type_cache_next_key(key)
			continue
		}
		mut fallback := cache.base
		mut fallback_collision := false
		for !isnil(fallback) {
			if entry := fallback.parse_entries[key] {
				if parse_type_cache_entry_matches(entry, file, module_name, text, generic_params, resolution) {
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
			typ:            cache_type_reference(typ)
		}
		cache.parse_entries[key] = entry
		cache.parse_last_entry = entry
		cache.parse_last_valid = true
		return
	}
}

@[direct_array_access]
fn parse_type_cache_put_recent(mut cache TypeCache, file string, module_name string, text u32, generic_params []string, resolution bool, typ &Type) {
	parse_type_cache_put(mut cache, file, module_name, text, generic_params, resolution, typ)
	context_hash := parse_type_cache_context_hash(mut cache, file, module_name, generic_params, resolution)
	slot := parse_type_cache_value_recent_slot(text, context_hash)
	cache.parse_value_recent_ids[slot] = text
	cache.parse_value_recent_context[slot] = context_hash
	cache.parse_value_recent_values[slot] = typ
	cache.parse_value_recent_set[slot] = true
}

// parse_scope_param_type preserves open generic struct applications for local parameter
// lookup. The global parser deliberately collapses `Box[T]` to `Box` in signatures, but
// inside a generic function/method body the parameter still needs the open application so
// field lookup and generic receiver method resolution can substitute `T`.
fn (tc &TypeChecker) parse_scope_param_type(typ_id u32) Type {
	clean_id := trimmed_source_type_id(typ_id)
	if flat.source_type_shape(clean_id).has(.variadic) {
		return Type(Array{
			elem_type: tc.intern_type_reference(tc.parse_scope_param_type(flat.source_type_syntax(clean_id).raw_base()))
		})
	}
	if preserved := tc.parse_open_generic_struct_type(typ_id) {
		return preserved
	}
	return *tc.parse_type(typ_id)
}

fn (tc &TypeChecker) parse_open_generic_struct_type(typ_id u32) ?Type {
	clean_id := trimmed_source_type_id(typ_id)
	if clean_id == 0 {
		return none
	}
	shape := flat.source_type_shape(clean_id)
	if shape.has(.pointer) {
		base := tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base()) or { return none }
		return Type(Pointer{
			base_type: tc.intern_type_reference(base)
		})
	}
	if shape.has(.mutable_type) {
		base := tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base()) or { return none }
		return Type(Pointer{
			base_type: tc.intern_type_reference(base)
		})
	}
	if shape.has(.shared_type) {
		return tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base())
	}
	if shape.has(.atomic_type) {
		return tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base())
	}
	if shape.has(.option_type) {
		base := tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base()) or { return none }
		return Type(OptionType{
			base_type: tc.intern_type_reference(base)
		})
	}
	if shape.has(.result_type) {
		base := tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base()) or { return none }
		return Type(ResultType{
			base_type: tc.intern_type_reference(base)
		})
	}
	if shape.has(.variadic) {
		elem := tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base()) or { return none }
		return Type(Array{
			elem_type: tc.intern_type_reference(elem)
		})
	}
	if shape.has(.array_type) {
		elem := tc.parse_open_generic_struct_type(flat.source_type_syntax(clean_id).raw_base()) or { return none }
		return Type(Array{
			elem_type: tc.intern_type_reference(elem)
		})
	}
	if !shape.has(.bracketed) { return none }
	syntax := flat.source_type_syntax(clean_id)
	if shape.has(.map_type) {
		if syntax.bracket_closed() {
			key := tc.parse_scope_param_type(syntax.bracket_raw_inner())
			value := tc.parse_open_generic_struct_type(syntax.bracket_suffix()) or { return none }
			return Type(Map{
				key_type:   tc.intern_type_reference(key)
				value_type: tc.intern_type_reference(value)
			})
		}
	}
	if shape.has(.leading_bracket) {
		if syntax.bracket_closed() {
			elem := tc.parse_open_generic_struct_type(syntax.bracket_suffix()) or { return none }
			len_id := syntax.bracket_inner()
			len_syntax := flat.source_type_syntax(len_id)
			return Type(ArrayFixed{
				elem_type: tc.intern_type_reference(elem)
				len:       len_syntax.decimal_value()
				len_expr:  if len_syntax.decimal() { '' } else { flat.source_type_name(len_id) }
			})
		}
	}
	if !syntax.application_valid() || tc.generic_args_are_concrete(syntax.application_args()) {
		return none
	}
	base_id := syntax.application_base()
	mut qbase := base_id
	if !flat.source_type_shape(base_id).has(.dotted) {
		resolved := tc.qualify_type_name(base_id)
		if resolved in tc.structs || resolved in tc.struct_generic_params {
			qbase = resolved
		}
	}
	if qbase !in tc.structs && qbase !in tc.struct_generic_params && base_id !in tc.structs && base_id !in tc.struct_generic_params {
		return none
	}
	mut preserved_parts := []u32{cap: syntax.application_args().len + 1}
	preserved_parts << qbase
	for arg in syntax.application_args() {
		arg_id := trimmed_source_type_id(arg)
		arg_syntax := flat.source_type_syntax(arg_id)
		preserved_parts << if arg_syntax.named_type() && flat.source_type_syntax(arg_syntax.unqualified()).bare_generic() {
			arg_syntax.unqualified()
		} else {
			arg_id
		}
	}
	return Type(Struct{
		name_id: flat.source_type_composition(.generic, preserved_parts)
	})
}

fn (tc &TypeChecker) parse_generic_alias_application(name_id u32, args []u32, suffix u32) Type {
	target_source_id := tc.type_aliases.value_id(name_id)
	params := tc.type_alias_generic_params[name_id] or {
		tc.type_alias_generic_params[flat.source_type_syntax(name_id).unqualified()] or { []string{} }
	}
	if params.len == args.len && params.len > 0 {
		return Type(Alias{
			name_id:   flat.source_type_composition_two(.concat, name_id, suffix)
			base_type: tc.parse_type(subst_generic_text(target_source_id, args, params))
		})
	}
	return Type(Alias{
		name_id:   name_id
		base_type: tc.parse_type(target_source_id)
	})
}

// parse_type_uncached reads parse type uncached input for types.
fn (tc &TypeChecker) parse_type_uncached(source_id u32) Type {
	if source_id == 0 {
		return builtin_void_type
	}
	// Exact scalar spellings cannot carry wrappers or nominal type arguments.
	if scalar_builtin_type_name_id(source_id) {
		return builtin_type_value_id(source_id)
	}
	syntax := flat.source_type_syntax(source_id)
	shape := flat.source_type_shape(source_id)
	if resolved := tc.type_from_typeof_type_id(source_id) {
		return resolved
	}
	if flat.source_type_syntax(syntax.trimmed()).typeof_prefix() {
		return unknown_type_quoting_id('unresolved typeof type', source_id)
	}
	// Preserve open generic struct applications wherever they occur in a signature or
	// expression type. Qualifying their placeholder (`T` -> `module.T`) makes it look
	// concrete to later stages and can emit invalid C types such as `Box_module__T`.
	if preserved := tc.parse_open_generic_struct_type(source_id) {
		return preserved
	}
	if syntax.typ_suffix() {
		return *tc.parse_type(syntax.typ_suffix_base())
	}
	// `main.Foo` is an explicit reference to a program-module type. It is used to
	// lock a bare concrete generic argument against being rebased into a callee
	// module that declares a same-named type (see explicit_generic_concrete_arg_text
	// / lock_colliding_main_generic_type_text). Resolve it directly to the bare-keyed
	// program symbol so the resulting type name matches the struct/method tables,
	// unless `main` is an actual import alias in this file.
	if flat.source_type_query(source_id).has(.main_prefix)
		&& !flat.source_type_shape(syntax.main_base()).has(.dotted) {
		if _ := tc.resolve_import_alias('main') {
		} else {
			rest_id := syntax.main_base()
			if is_builtin_type_name_id(rest_id) { return builtin_type_value_id(rest_id) }
			if known := tc.type_from_known_symbol(rest_id) { return known }
			rest := flat.source_type_syntax(rest_id)
			if rest.application_valid() && !flat.source_type_shape(rest.application_base()).has(.dotted) {
				base_id := rest.application_base()
				suffix := tc.qualified_generic_suffix(rest.application_args())
				instance := flat.source_type_composition_two(.concat, base_id, suffix)
				if base_id in tc.struct_generic_params || base_id in tc.structs {
					return Type(Struct{ name_id: instance })
				}
				if base_id in tc.sum_generic_params || tc.sum_types.contains(base_id) {
					return Type(SumType{ name_id: instance })
				}
				if base_id in tc.interface_names { return Type(Interface{ name_id: instance }) }
				if tc.type_aliases.contains(base_id) {
					return tc.parse_generic_alias_application(base_id, rest.application_args(), suffix)
				}
			}
		}
	}
	if shape.has(.pointer) {
		return Type(Pointer{
			base_type: tc.parse_type(syntax.raw_base())
		})
	}
	if shape.has(.mutable_type) {
		return Type(Pointer{
			base_type: tc.parse_type(syntax.raw_base())
		})
	}
	if shape.has(.shared_type) {
		return *tc.parse_type(syntax.raw_base())
	}
	if shape.has(.atomic_type) {
		return *tc.parse_type(syntax.raw_base())
	}
	if shape.has(.option_type) {
		base_type := tc.parse_type(syntax.raw_base())
		if base_type is Alias && unalias_type(base_type) is OptionType {
			return base_type
		}
		return Type(OptionType{
			base_type: tc.intern_type_reference(base_type)
		})
	}
	if shape.has(.result_type) {
		return Type(ResultType{
			base_type: tc.parse_type(syntax.raw_base())
		})
	}
	if shape.has(.channel_type) {
		elem_id := syntax.raw_base()
		return Type(Channel{
			elem_type: tc.parse_type(elem_id)
			is_mut:    flat.source_type_shape(elem_id).has(.mutable_type)
		})
	}
	if source_id == checker_tail_source_chan {
		return Type(Channel{})
	}
	if shape.has(.thread_type) {
		// A thread handle. The element type (the spawned fn's return type) is kept
		// in the struct name (`thread T`) so `array_of_threads.wait()` can recover
		// `[]T`. Canonicalize concrete payload names just like spawn return types do,
		// while preserving unresolved generic placeholders for later substitution.
		payload_id := syntax.raw_base()
		payload_type := tc.parse_type(payload_id)
		payload_name := if payload_type is Unknown {
			payload_id
		} else {
			payload_type.source_type_id()
		}
		return Type(Struct{ name_id: flat.source_type_composition_one(.thread, payload_name) })
	}
	if source_id == checker_source_thread {
		// The handle itself lowers to `void*` in C (see c_type).
		return Type(Struct{
			name_id: source_id
		})
	}
	if shape.has(.variadic) {
		return Type(Array{
			elem_type: tc.parse_type(syntax.raw_base())
		})
	}
	if shape.has(.array_type) {
		return Type(Array{
			elem_type: tc.parse_type(syntax.raw_base())
		})
	}
	if shape.has(.map_type) {
		if !syntax.bracket_closed() { return Type(Unknown{ reason: 'malformed map type' }) }
		return Type(Map{ key_type: tc.parse_type(syntax.bracket_raw_inner()), value_type: tc.parse_type(syntax.raw_base()) })
	}
	if syntax.checker_fixed_array_may_be() && !shape.has(.leading_bracket) {
		length_id := trimmed_source_type_id(syntax.last_bracket_span_inner())
		length := flat.source_type_syntax(length_id)
		if length.decimal() || flat.source_type_shape(syntax.last_bracket_prefix()).has(.bracketed) {
			return Type(ArrayFixed{
				elem_type: tc.parse_type(syntax.last_bracket_prefix())
				len:       if length.decimal() { length.decimal_value() } else { 0 }
				len_expr:  if length.decimal() { '' } else { flat.source_type_name(length_id) }
			})
		}
	}
	if shape.has(.leading_bracket) && syntax.first_bracket_closed() {
		length_id := syntax.first_bracket_length()
		length := flat.source_type_syntax(length_id)
		return Type(ArrayFixed{
			elem_type: tc.parse_type(syntax.first_bracket_base())
			len:       if length.decimal() { length.decimal_value() } else { 0 }
			len_expr:  if length.decimal() { '' } else { flat.source_type_name(length_id) }
		})
	}
	if syntax.paren_prefix() && syntax.contains_comma() {
		mut tuple_types := []Type{cap: syntax.raw_tuple_parse_components().len}
		for part in syntax.raw_tuple_parse_components() {
			tuple_types << tc.parse_type(trimmed_source_type_id(part))
		}
		return Type(MultiReturn{ types: tuple_types })
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		return tc.parse_fn_type(source_id)
	}
	// Inspect a nominal name only after parsing its wrappers. Otherwise the last
	// component of `!module.M` looks generic while the complete wrapper is absent
	// from the declared-type tables, even though `module.M` is concrete.
	if syntax.named_type() && source_type_is_bare_generic_param(syntax.unqualified())
		&& (tc.active_generic_param(flat.source_type_name(source_id)) || !tc.is_known_type_text(source_id)) {
		return generic_placeholder_type(source_id)
	}
	qtyp_id := if tc.resolution_type_mode {
		tc.qualify_resolution_type_name(source_id)
	} else {
		tc.qualify_type_name(source_id)
	}
	allow_bare_symbol := qtyp_id == source_id
	if source_id == qualify_source_type_array && tc.has_builtins && source_id in tc.structs {
		return Type(Struct{
			name_id: source_id
		})
	}
	if source_id == qualify_source_type_map && tc.has_builtins {
		return Type(Struct{
			name_id: source_id
		})
	}
	if source_id == qualify_source_type_array && tc.has_builtins {
		return Type(Struct{
			name_id: source_id
		})
	}
	if is_builtin_type_name_id(source_id) {
		return builtin_type_value_id(source_id)
	}
	if source_id == flat.source_type_unknown {
		return Type(Unknown{
			reason: 'unknown'
		})
	}
	if qtyp_id != source_id {
		if scoped_type := tc.type_from_known_symbol(qtyp_id) {
			return scoped_type
		}
	}
	if !shape.has(.dotted) {
		if resolved_id := tc.resolve_selective_import_type_symbol_id(source_id) {
			if resolved_type := tc.type_from_known_symbol(resolved_id) {
				return resolved_type
			}
		}
	}
	if builtin_error_id := tc.resolve_unqualified_builtin_error_struct_name_id(source_id) {
		return Type(Struct{
			name_id: builtin_error_id
		})
	}
	if flat.source_type_query(source_id).has(.c_prefix) {
		if tc.type_aliases.contains(source_id) {
			return tc.parse_alias_type(source_id, tc.type_aliases.value_id(source_id))
		}
		return Type(Struct{
			name_id: source_id
		})
	}
	if tc.type_aliases.contains(qtyp_id) {
		return tc.parse_alias_type(qtyp_id, tc.type_aliases.value_id(qtyp_id))
	}
	if allow_bare_symbol && tc.type_aliases.contains(source_id) {
		return tc.parse_alias_type(source_id, tc.type_aliases.value_id(source_id))
	}
	if qtyp_id in tc.interface_names {
		return Type(Interface{
			name_id: qtyp_id
		})
	}
	if allow_bare_symbol && source_id in tc.interface_names {
		return Type(Interface{
			name_id: source_id
		})
	}
	if qtyp_id in tc.structs {
		return Type(Struct{
			name_id: qtyp_id
		})
	}
	if allow_bare_symbol && source_id in tc.structs {
		return Type(Struct{
			name_id: source_id
		})
	}
	if qtyp_id in tc.flag_enums {
		return Type(Enum{
			name_id: qtyp_id
			is_flag: true
		})
	}
	if allow_bare_symbol && source_id in tc.flag_enums {
		return Type(Enum{
			name_id: source_id
			is_flag: true
		})
	}
	if shape.has(.dotted) {
		short_id := syntax.unqualified()
		if qtyp_id !in tc.flag_enums && source_id !in tc.flag_enums && short_id in tc.flag_enums {
			return Type(Enum{
				name_id: short_id
				is_flag: true
			})
		}
	}
	if qtyp_id in tc.enum_names {
		return Type(Enum{
			name_id: qtyp_id
		})
	}
	if allow_bare_symbol && source_id in tc.enum_names {
		return Type(Enum{
			name_id: source_id
		})
	}
	if shape.has(.dotted) {
		short_id := syntax.unqualified()
		if qtyp_id !in tc.enum_names && source_id !in tc.enum_names && short_id in tc.enum_names {
			return Type(Enum{
				name_id: short_id
			})
		}
	}
	if tc.sum_types.contains(qtyp_id) {
		return Type(SumType{
			name_id: qtyp_id
		})
	}
	if allow_bare_symbol && tc.sum_types.contains(source_id) {
		return Type(SumType{
			name_id: source_id
		})
	}
	if !shape.has(.dotted) {
		if resolved_id := tc.resolve_selective_import_type_symbol_id(source_id) {
			if resolved_type := tc.type_from_known_symbol(resolved_id) {
				return resolved_type
			}
		}
	}
	if allow_bare_symbol && !shape.has(.dotted) {
		if resolved_source_id := tc.unique_qualified_type_name(source_id) {
			if tc.type_aliases.contains(resolved_source_id) {
				return Type(Alias{
					name_id:   resolved_source_id
					base_type: tc.parse_type(tc.type_aliases.value_id(resolved_source_id))
				})
			}
			if resolved_source_id in tc.structs {
				return Type(Struct{
					name_id: resolved_source_id
				})
			}
			if resolved_source_id in tc.interface_names {
				return Type(Interface{
					name_id: resolved_source_id
				})
			}
			if resolved_source_id in tc.flag_enums {
				return Type(Enum{
					name_id: resolved_source_id
					is_flag: true
				})
			}
			if resolved_source_id in tc.enum_names {
				return Type(Enum{
					name_id: resolved_source_id
				})
			}
			if tc.sum_types.contains(resolved_source_id) {
				return Type(SumType{
					name_id: resolved_source_id
				})
			}
		}
	}
	if syntax.application_valid() {
		resolved_base_source_id := tc.resolve_imported_type_text(syntax.application_base())
		generic_suffix := syntax.bracket_from()
		generic_arg_ids := syntax.application_args()
		is_concrete_generic := tc.generic_args_are_concrete(generic_arg_ids)
		struct_generic_suffix := if is_concrete_generic {
			tc.qualified_generic_suffix(generic_arg_ids)
		} else {
			generic_suffix
		}
		sum_generic_suffix := struct_generic_suffix
		mut qbase_source_id := if tc.resolution_type_mode {
			tc.qualify_resolution_type_name(resolved_base_source_id)
		} else {
			tc.qualify_type_name(resolved_base_source_id)
		}
		if !flat.source_type_shape(resolved_base_source_id).has(.dotted) {
			if resolved_id := tc.resolve_selective_import_type_symbol_id(resolved_base_source_id) {
				qbase_source_id = resolved_id
			}
		}
		if qbase_source_id == resolved_base_source_id && flat.source_type_shape(resolved_base_source_id).has(.dotted) {
			qbase_source_id = tc.resolve_imported_type_text(resolved_base_source_id)
		}
		allow_bare_generic_base := qbase_source_id == resolved_base_source_id
		if qbase_source_id in tc.struct_generic_params {
			mut sgp_base_id := qbase_source_id
			if allow_bare_generic_base && qbase_source_id !in tc.structs && !flat.source_type_shape(resolved_base_source_id).has(.dotted) {
				// The bare key is only a generic-params shadow of an imported struct
				// (e.g. main referencing `Vec2` from `import math.vec { Vec2 }`, which is
				// keyed `vec.Vec2`). Resolve it to the real qualified struct so field/method
				// lookup and c_type match `vec.Vec2[int]`, not a bogus bare `Vec2[int]`.
				if resolved_id := tc.resolve_selective_import_type_symbol_id(resolved_base_source_id) {
					sgp_base_id = resolved_id
				} else if resolved_source_id := tc.unique_qualified_type_name(resolved_base_source_id) {
					sgp_base_id = resolved_source_id
				}
			}
			return Type(Struct{
				name_id: flat.source_type_composition_two(.concat, sgp_base_id, struct_generic_suffix)
			})
		}
		if tc.type_aliases.contains(qbase_source_id) {
			return tc.parse_generic_alias_application(qbase_source_id, generic_arg_ids, struct_generic_suffix)
		}
		if qbase_source_id in tc.structs {
			return Type(Struct{
				name_id: flat.source_type_composition_two(.concat, qbase_source_id, struct_generic_suffix)
			})
		}
		if qbase_source_id in tc.interface_names {
			return Type(Interface{
				name_id: flat.source_type_composition_two(.concat, qbase_source_id, struct_generic_suffix)
			})
		}
		if tc.sum_types.contains(qbase_source_id) {
			return Type(SumType{
				name_id: flat.source_type_composition_two(.concat, qbase_source_id, sum_generic_suffix)
			})
		}
		if short_source_id := tc.imported_type_short_name(resolved_base_source_id) {
			// A colliding main-module alias can make legacy module collection retain
			// the imported generic declaration under its semantic short name. Prefer
			// that generic declaration over the same-named alias.
			if short_source_id in tc.struct_generic_params || short_source_id in tc.structs {
				return Type(Struct{
					name_id: flat.source_type_composition_two(.concat, short_source_id, struct_generic_suffix)
				})
			}
			if short_source_id in tc.sum_generic_params || tc.sum_types.contains(short_source_id) {
				return Type(SumType{
					name_id: flat.source_type_composition_two(.concat, short_source_id, sum_generic_suffix)
				})
			}
			if short_source_id in tc.interface_names {
				return Type(Interface{
					name_id: flat.source_type_composition_two(.concat, short_source_id, struct_generic_suffix)
				})
			}
			if tc.type_aliases.contains(short_source_id) {
				return tc.parse_generic_alias_application(short_source_id, generic_arg_ids, struct_generic_suffix)
			}
		}
		if !flat.source_type_shape(resolved_base_source_id).has(.dotted) {
			if resolved_id := tc.resolve_selective_import_type_symbol_id(resolved_base_source_id) {
				if tc.type_aliases.contains(resolved_id) {
					return tc.parse_generic_alias_application(resolved_id, generic_arg_ids, struct_generic_suffix)
				}
				if resolved_id in tc.structs {
					return Type(Struct{
						name_id: flat.source_type_composition_two(.concat, resolved_id, struct_generic_suffix)
					})
				}
				if resolved_id in tc.interface_names {
					return Type(Interface{
						name_id: flat.source_type_composition_two(.concat, resolved_id, struct_generic_suffix)
					})
				}
				if tc.sum_types.contains(resolved_id) {
					return Type(SumType{
						name_id: flat.source_type_composition_two(.concat, resolved_id, sum_generic_suffix)
					})
				}
			}
		}
		if allow_bare_generic_base && tc.type_aliases.contains(resolved_base_source_id) {
			return tc.parse_generic_alias_application(resolved_base_source_id, generic_arg_ids, struct_generic_suffix)
		}
		if allow_bare_generic_base && resolved_base_source_id in tc.structs {
			return Type(Struct{
				name_id: flat.source_type_composition_two(.concat, resolved_base_source_id, struct_generic_suffix)
			})
		}
		if allow_bare_generic_base && resolved_base_source_id in tc.interface_names {
			return Type(Interface{
				name_id: flat.source_type_composition_two(.concat, resolved_base_source_id, struct_generic_suffix)
			})
		}
		if allow_bare_generic_base && tc.sum_types.contains(resolved_base_source_id) {
			return Type(SumType{
				name_id: flat.source_type_composition_two(.concat, resolved_base_source_id, sum_generic_suffix)
			})
		}
		if is_concrete_generic && !is_builtin_type_name_id(resolved_base_source_id) {
			// A concrete generic instance (`Vec4[f32]`) is a monomorphized struct, even
			// when the generic base decl has been erased after monomorphization. It is
			// never a fixed array, so don't fall through to the `[N]T` handler below.
			// Qualify an imported base (`Vec4` -> `vec.Vec4`) so its c_type matches the
			// materialized struct (`vec__Vec4_f32`) everywhere it appears. A builtin base
			// (`int[seg_count]`) cannot be a generic application, so let it fall through to
			// the fixed-array handler — its bracket is a const/expression length.
			mut full := flat.source_type_composition_two(.concat, qbase_source_id, generic_suffix)
			if allow_bare_generic_base && !flat.source_type_shape(resolved_base_source_id).has(.dotted) {
				if resolved_source_id := tc.unique_qualified_type_name(resolved_base_source_id) {
					full = flat.source_type_composition_two(.concat, resolved_source_id, generic_suffix)
				}
			}
			return Type(Struct{
				name_id: full
			})
		}
	}
	if syntax.named_type() && source_type_is_bare_generic_param(syntax.unqualified()) {
		return unknown_type_quoting_id('generic type parameter', source_id)
	}
	if shape.has(.bracketed) && !shape.has(.leading_bracket) && syntax.last_bracket_span_valid() {
		// ArrayFixed renders the outermost dimension last, so recurse through
		// the final bracket pair, retaining source expressions as expressions.
		length_id := trimmed_source_type_id(syntax.last_bracket_span_inner())
		length := flat.source_type_syntax(length_id)
		return Type(ArrayFixed{
			elem_type: tc.parse_type(syntax.last_bracket_prefix())
			len:       if length.decimal() { length.decimal_value() } else { 0 }
			len_expr:  if length.decimal() { '' } else { flat.source_type_name(length_id) }
		})
	}
	if qtyp_id != source_id {
		return Type(Struct{
			name_id: qtyp_id
		})
	}
	return Type(Struct{
		name_id: source_id
	})
}

// inferred_type_source reports whether an interned spelling holds a
// `typeof(expr)` type annotation. The shape check keeps other spellings byte-free.
@[inline]
fn inferred_type_source(source_id u32) bool {
	return flat.source_type_shape(source_id).has(.typeof_expr)
		&& flat.source_type_query(source_id).has(.inferred_witness)
}

// resolved_inferred_type returns a `typeof(expr)` annotation with its
// expressions resolved, or none when the spelling has none or none resolve.
fn (tc &TypeChecker) resolved_inferred_type(source_id u32) ?u32 {
	if !inferred_type_source(source_id) { return none }
	resolved := tc.resolve_inferred_type_id(source_id)
	if resolved == source_id { return none }
	return resolved
}

// check_node_inferred_type_witnesses checks the `typeof(expr)` expressions of
// the type a node is annotated with, or of the type spelling in its value.
fn (mut tc TypeChecker) check_node_inferred_type_witnesses(node flat.Node) {
	tc.check_inferred_type_witnesses(node.typ)
	if node.kind in [.string_literal, .char_literal, .string_interp] { return }
	if node.value.len > 15 && node.value.contains('typeof(__expr_') {
		// Legacy raw operands cross the source boundary here; ordinary names
		// should never enter the source type interner for this probe.
		value_id := node.source_value_id()
		if inferred_type_source(value_id) { tc.check_inferred_type_witnesses(value_id) }
	} else if node.value.len == 0 && inferred_type_source(node.source_value)
		&& flat.source_type_syntax(node.source_value).byte_len() > 15 {
		tc.check_inferred_type_witnesses(node.source_value)
	}
}

// check_inferred_type_witnesses checks each expression of a `typeof(expr)`
// type in the current lexical scope like any other expression, so an invalid
// call or operand in type position is reported. The expression is never
// evaluated. Checking a call first also infers its generic arguments, so the
// witness names the instantiated return type. An expression whose type still
// names an unresolved generic placeholder is rejected.
fn (mut tc TypeChecker) check_inferred_type_witnesses(source_id u32) {
	if !inferred_type_source(source_id) {
		return
	}
	for expression_bits in flat.source_type_syntax(source_id).inferred_expr_ids() {
		expression_id := flat.NodeId(expression_bits)
		if !tc.valid_node_id(expression_id) {
			continue
		}
		errors_before := tc.errors.len
		// A type names its expression without capturing it into a closure.
		saved_closure_scope := tc.fn_context.closure_scope
		tc.fn_context.closure_scope = unsafe { nil }
		tc.check_node(expression_id)
		tc.fn_context.closure_scope = saved_closure_scope
		if tc.errors.len != errors_before {
			continue
		}
		value := tc.expr_type(expression_id) or { tc.resolve_type(expression_id) }
		if placeholder := tc.unresolved_generic_placeholder_in(value) {
			tc.record_error(.unsupported_generic, 'cannot infer the type of `typeof(...)`: `${placeholder}` is not resolved here', expression_id)
		}
	}
}

// unresolved_generic_placeholder_in returns the first generic placeholder in
// `typ` that the current function does not declare.
fn (tc &TypeChecker) unresolved_generic_placeholder_in(typ Type) ?string {
	if typ is Unknown {
		if placeholder := generic_placeholder_from_unknown(typ) {
			if placeholder !in tc.fn_context.generic_params { return placeholder }
		}
		return none
	}
	type_id := typ.source_type_id()
	if !flat.source_type_query(type_id).has(.generic_token) { return none }
	for word in flat.source_type_syntax(type_id).words() {
		if !flat.source_type_syntax(word).bare_generic() { continue }
		name := flat.source_type_name(word)
		if name !in tc.fn_context.generic_params && !tc.is_known_type_text(word) { return name }
	}
	return none
}

// `typeof(expr)` annotations can infer array items and nested generic payloads.
// Resolve the expression before generic specialization records concrete names.
pub fn (tc &TypeChecker) resolve_inferred_type_text(typ string) string {
	return flat.source_type_name(tc.resolve_inferred_type_id(flat.source_type_id(typ)))
}

// resolve_inferred_type_id resolves the expression witnesses in a source type.
// Components stay interned through replacement and are composed only on change.
pub fn (tc &TypeChecker) resolve_inferred_type_id(type_id u32) u32 {
	if !inferred_type_source(type_id) { return type_id }
	syntax := flat.source_type_syntax(type_id)
	mut parts := []u32{cap: syntax.inferred_parts().len + syntax.inferred_annotations().len}
	mut changed := false
	mut blocked := false
	for i, annotation in syntax.inferred_annotations() {
		parts << syntax.inferred_parts()[i]
		if blocked {
			parts << annotation
			continue
		}
		value := tc.type_from_typeof_type_id(annotation) or {
			blocked = true
			parts << annotation
			continue
		}
		if value is Unknown {
			blocked = true
			parts << annotation
			continue
		}
		name_id := value.source_type_id()
		if name_id == annotation {
			blocked = true
			parts << annotation
			continue
		}
		changed = true
		parts << name_id
	}
	if !changed { return type_id }
	parts << syntax.inferred_parts()[syntax.inferred_parts().len - 1]
	return flat.source_type_composition(.concat, parts)
}

fn (tc &TypeChecker) type_from_typeof_type_id(type_id u32) ?Type {
	syntax := flat.source_type_syntax(trimmed_source_type_id(type_id))
	if !syntax.typeof_valid() { return none }
	inner_id := syntax.typeof_inner()
	inner := flat.source_type_syntax(inner_id)
	if inner.expr_prefix() {
		expression_id := flat.NodeId(inner.expr_id())
		if int(expression_id) >= 0 && int(expression_id) < tc.a.nodes.len {
			// Resolve closure annotations in their lexical scope. Never publish a
			// transform scratch-arena Type into the source-node cache.
			witness_id := tc.a.nodes[int(expression_id)].typ
			if witness_id != 0 && !flat.source_type_query(witness_id).has(.inferred_witness)
				&& witness_id !in [qualify_source_type_array, flat.source_type_unknown,
					flat.source_type_void] {
				return tc.parse_resolution_type(witness_id)
			}
			// Detached witnesses may need resolution before a checked cache exists.
			value := tc.expr_type(expression_id) or { tc.resolve_type(expression_id) }
			if value is Void || value is Unknown || type_contains_unknown(value)
				|| flat.source_type_query(value.source_type_id()).has(.inferred_witness) {
				return none
			}
			return value
		}
		return none
	}
	if inner_id == 0 { return none }
	if flat.source_type_query(inner_id).has(.bracket_suffix) && inner.last_bracket_prefix() != 0 {
		if base_type := tc.type_from_simple_expr_id(trimmed_source_type_id(inner.last_bracket_prefix())) {
			return tc.resolve_index_base_value_type(unalias_type(base_type))
		}
	}
	return tc.type_from_simple_expr_id(inner_id)
}

fn (tc &TypeChecker) type_from_simple_expr_id(expr_id u32) ?Type {
	clean_id := trimmed_source_type_id(expr_id)
	if clean_id == 0 { return none }
	// Scope and constant registries use source declaration names.
	clean := flat.source_type_name(clean_id)
	if typ := tc.cur_scope.lookup(clean) { return typ }
	if typ := tc.file_scope.lookup(clean) { return typ }
	if key := tc.shadowed_builtin_const_key(clean) {
		return tc.const_types[key] or { builtin_void_type }
	}
	qname_id := tc.qualify_type_name(clean_id)
	if qname_id != clean_id {
		qname := flat.source_type_name(qname_id)
		if typ := tc.file_scope.lookup(qname) { return typ }
		if typ := tc.const_types[qname] { return typ }
	}
	if typ := tc.const_types[clean] { return typ }
	return none
}

// qualified_generic_suffix reuses an argument-ID sequence after resolution qualification.
fn (tc &TypeChecker) qualified_generic_suffix(args []u32) u32 {
	mut parts := []u32{cap: args.len + 1}
	parts << u32(0)
	for arg in args { parts << tc.qualify_resolution_type_text(arg) }
	return flat.source_type_composition(.generic, parts)
}

fn (tc &TypeChecker) array_literal_elem_type(node flat.Node) Type {
	if node.children_count == 0 {
		return builtin_int_type
	}
	elem_type := tc.array_literal_child_elem_type(tc.a.child(&node, 0))
	mut all_numeric := true
	mut has_f32 := false
	mut has_f64 := false
	mut has_explicit_f64 := false
	for i in 0 .. node.children_count {
		child_id := tc.a.child(&node, i)
		child_type := tc.array_literal_child_elem_type(child_id)
		child_is_float := child_type.is_float()
		if !(child_type.is_integer() || child_is_float) {
			all_numeric = false
		}
		if child_is_float {
			child_type_id := child_type.source_type_id()
			if child_type_id == flat.source_type_f32 {
				has_f32 = true
			}
			if child_type_id == flat.source_type_f64 {
				has_f64 = true
				if !tc.is_untyped_float_literal_expr(child_id) {
					has_explicit_f64 = true
				}
			}
		}
	}
	if all_numeric && has_explicit_f64 {
		return builtin_f64_type
	}
	if all_numeric && has_f32 {
		return *tc.parse_type(flat.source_type_f32)
	}
	if all_numeric && has_f64 {
		return builtin_f64_type
	}
	return elem_type
}

fn (tc &TypeChecker) array_literal_child_elem_type(child_id flat.NodeId) Type {
	child := tc.a.nodes[int(child_id)]
	if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
		spread_type := unalias_type(tc.resolve_type(tc.a.child(&child, 0)))
		if spread_type is Array {
			return *spread_type.elem_type
		}
		if spread_type is ArrayFixed {
			return *spread_type.elem_type
		}
	}
	if alias_type := tc.explicit_alias_constructor_type(child_id) {
		return alias_type
	}
	// Generic calls are initially resolvable only to their declaration spelling
	// (for example `Arc[T]`). Once the call has been checked, prefer its cached
	// specialized return so an inferred array becomes `[]Arc[Resource]`.
	if child.kind == .call {
		if checked := tc.expr_type(child_id) {
			if !tc.type_contains_open_generic_placeholder(checked) {
				return checked
			}
		}
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
	mut type_name_id := u32(0)
	if node.kind == .cast_expr {
		type_name_id = node.source_value_id()
	} else if node.kind == .call && node.children_count > 0 {
		fn_node_id := tc.a.child(&node, 0)
		type_name_id = tc.type_expr_name(fn_node_id)
	}
	if type_name_id == 0 {
		return none
	}
	qualified_id := tc.qualify_type_name(type_name_id)
	if tc.sum_types.contains(type_name_id) {
		return *tc.parse_type(type_name_id)
	}
	if tc.sum_types.contains(qualified_id) {
		return *tc.parse_type(qualified_id)
	}
	if tc.type_aliases.contains(type_name_id) || tc.type_aliases.contains(qualified_id) {
		return *tc.parse_type(type_name_id)
	}
	return none
}

fn (tc &TypeChecker) is_untyped_float_literal_expr(id flat.NodeId) bool {
	mut states := map[flat.NodeId]u8{}
	known, has_float := tc.untyped_numeric_literal_expr_info(id, mut states)
	return known && has_float
}

// States distinguish an active constant reference from completed integer/float expressions.
// This detects cycles and reuses shared subexpressions without a semantic nesting limit.
fn (tc &TypeChecker) untyped_numeric_literal_expr_info(id flat.NodeId, mut states map[flat.NodeId]u8) (bool, bool) {
	if !tc.valid_node_id(id) {
		return false, false
	}
	if state := states[id] {
		return state in [u8(2), 3], state == 3
	}
	states[id] = 1
	known, has_float := tc.untyped_numeric_literal_node_info(id, mut states)
	states[id] = if !known {
		u8(4)
	} else if has_float {
		u8(3)
	} else {
		u8(2)
	}
	return known, has_float
}

fn (tc &TypeChecker) untyped_numeric_literal_node_info(id flat.NodeId, mut states map[flat.NodeId]u8) (bool, bool) {
	node := tc.a.node(id)
	match node.kind {
		.paren, .expr_stmt {
			if node.children_count == 0 {
				return false, false
			}
			return tc.untyped_numeric_literal_expr_info(tc.a.child(node, 0), mut states)
		}
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
			return tc.untyped_numeric_literal_expr_info(tc.a.child(node, 0), mut states)
		}
		.infix {
			if node.op !in [.plus, .minus, .mul, .div, .mod] || node.children_count < 2 {
				return false, false
			}
			left_known, left_float := tc.untyped_numeric_literal_expr_info(tc.a.child(node, 0), mut states)
			right_known, right_float := tc.untyped_numeric_literal_expr_info(tc.a.child(node, 1), mut states)
			return left_known && right_known, left_float || right_float
		}
		.ident {
			key := tc.const_key_for_name(node.value) or { return false, false }
			expr_id := tc.const_exprs[key] or { return false, false }
			return tc.untyped_numeric_literal_expr_info(expr_id, mut states)
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
			return tc.untyped_numeric_literal_expr_info(expr_id, mut states)
		}
		else {
			return false, false
		}
	}
}

fn (tc &TypeChecker) is_known_type_text(typ_id u32) bool {
	qtyp := tc.qualify_type_name(typ_id)
	if !flat.source_type_shape(typ_id).has(.dotted) {
		if resolved_id := tc.resolve_selective_import_type_symbol_id(typ_id) {
			return tc.type_symbol_known(resolved_id)
		}
	}
	return qtyp in tc.structs || qtyp in tc.interface_names || qtyp in tc.enum_names
		|| tc.sum_types.contains(qtyp) || tc.type_aliases.contains(qtyp)
}

// canonical_qualified_type_name resolves a possibly bare or partially qualified
// type name (e.g. `PoolProcessor` or `pool.PoolProcessor`) to its unique fully
// qualified spelling ID (`sync.pool.PoolProcessor`). The input is a source spelling ID.
// It returns none when the name
// is unknown or when the short name is ambiguous across modules. Backends use
// this to rebuild method/type C names when a receiver type carries only the
// import-local module qualifier instead of the full module path.
pub fn (tc &TypeChecker) canonical_qualified_type_name(name_id u32) ?u32 {
	return tc.unique_qualified_type_name(flat.source_type_syntax(name_id).unqualified())
}

// unique_qualified_type_name supports unique qualified type name handling for TypeChecker.
fn (tc &TypeChecker) unique_qualified_type_name(short_id u32) ?u32 {
	if short_id == 0 {
		return none
	}
	// A full scan of the five type-name maps (with an all_after_last allocation
	// per entry) is far too expensive for a per-expression helper — this is
	// called from qualify_name and sum-variant pattern checks. Build the
	// short-name index once per checker (memoized in the heap-allocated
	// type_cache, so forked parallel workers each build their own).
	mut cache := tc.type_cache
	if isnil(cache) {
		return tc.unique_qualified_type_name_scan(short_id)
	}
	if !cache.short_type_name_index_built {
		mut fallback := cache.base
		for !isnil(fallback) {
			if fallback.short_type_name_index_built {
				found_source_id := fallback.short_type_name_index.type_id(short_id) or { return none }

				if found_source_id == 0 {
					return none
				}
				return found_source_id
			}
			fallback = fallback.base
		}
		cache.short_type_name_index_built = true
		tc.build_short_type_name_index(mut cache.short_type_name_index)
	}
	found_source_id_2 := cache.short_type_name_index.type_id(short_id) or { return none }

	// An empty entry marks an ambiguous short name (several qualified types).
	if found_source_id_2 == 0 {
		return none
	}
	return found_source_id_2
}

// invalidate_short_type_name_index drops memoized type-name-derived indexes;
// callers that add or remove entries in the type-name maps after the checker ran
// (the monomorphizer specializing generic structs/sum types) must invalidate them.
pub fn (tc &TypeChecker) invalidate_short_type_name_index() {
	mut cache := tc.type_cache
	if isnil(cache) {
		return
	}
	cache.clear_ierror_compat_entries()
	cache.ierror_impl_names = flat.TypeTextList{}
	cache.ierror_impl_names_set = false
	if cache.short_type_name_index_built {
		cache.short_type_name_index_built = false
		cache.short_type_name_index.clear()
	}
}

// register_short_type_name extends an already-built short-name index with a
// source spelling ID after monomorphization adds a concrete type. A scoped
// transform copies the frozen index before extending its private entries.
pub fn (tc &TypeChecker) register_short_type_name(name_id u32) {
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
	index_short_type_name(name_id, mut cache.short_type_name_index)
}

// unregister_short_type_name removes an exact cached source spelling ID when a
// generic template is erased. Ambiguous entries stay conservative until the
// next compilation-wide index build.
pub fn (tc &TypeChecker) unregister_short_type_name(name_id u32) {
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
	short := flat.source_type_syntax(name_id).unqualified()
	if cached_source_id := cache.short_type_name_index.type_id(short) {
		if cached_source_id == name_id {
			cache.short_type_name_index.delete(short)
		}
	}
}

fn (tc &TypeChecker) build_short_type_name_index(mut index flat.SourceTypeBindings) {
	for name, _ in tc.type_aliases.ids {
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
	for name, _ in tc.sum_types.ids {
		index_short_type_name(name, mut index)
	}
}

fn index_short_type_name(id u32, mut index flat.SourceTypeBindings) {
	short := flat.source_type_syntax(id).unqualified()
	if previous := index.type_id(short) {
		if previous != id { index.set_id(short, 0) }
	} else {
		index.set_id(short, id)
	}
}

// unique_qualified_type_name_scan is the uncached fallback used when no
// type_cache is attached to the checker.
fn (tc &TypeChecker) unique_qualified_type_name_scan(short_id u32) ?u32 {
	mut found := u32(0)
	for name, _ in tc.type_aliases.ids {
		name_id := name
		if flat.source_type_syntax(name_id).unqualified() == short_id {
			if found != 0 && found != name_id { return none }
			found = name_id
		}
	}
	for name, _ in tc.structs {
		name_id := name
		if flat.source_type_syntax(name_id).unqualified() == short_id {
			if found != 0 && found != name_id { return none }
			found = name_id
		}
	}
	for name, _ in tc.interface_names {
		name_id := name
		if flat.source_type_syntax(name_id).unqualified() == short_id {
			if found != 0 && found != name_id { return none }
			found = name_id
		}
	}
	for name, _ in tc.enum_names {
		name_id := name
		if flat.source_type_syntax(name_id).unqualified() == short_id {
			if found != 0 && found != name_id { return none }
			found = name_id
		}
	}
	for name, _ in tc.sum_types.ids {
		name_id := name
		if flat.source_type_syntax(name_id).unqualified() == short_id {
			if found != 0 && found != name_id { return none }
			found = name_id
		}
	}
	if found == 0 { return none }
	return found
}

// parse_fn_type reads parse fn type input for types.
fn (tc &TypeChecker) parse_fn_type(type_id u32) Type {
	syntax := flat.source_type_syntax(type_id)
	if !syntax.function_closed() {
		return unknown_type_quoting_id('malformed fn type', type_id)
	}
	mut params := []FnParam{cap: syntax.checker_function_params().len}
	mut is_variadic := false
	if syntax.checker_parse_function_has_params() {
		for part in syntax.checker_function_params() {
			trimmed := trimmed_source_type_id(part)
			param_type := flat.source_type_syntax(trimmed).parameter_type()
			is_variadic = flat.source_type_shape(param_type).has(.variadic)
			parsed := tc.parse_type(param_type)
			is_mut := flat.source_type_shape(trimmed).has(.mutable_type)
			slot := if is_mut { mut_param_semantic_type(*parsed) } else { *parsed }
			params << FnParam{ typ: tc.intern_type_reference(slot), is_mut: is_mut }
		}
	}
	ret_type := if syntax.checker_parse_function_result() != 0 {
		*tc.parse_type(syntax.checker_parse_function_result())
	} else {
		Type(Void{})
	}
	return Type(FnType{ params: params, is_variadic: is_variadic, return_type: tc.intern_type_reference(ret_type) })
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_from_id(typ_id u32) ?string {
	clean_id := trimmed_source_type_id(typ_id)
	shape := flat.source_type_shape(clean_id)
	if !shape.has(.function_compact) && !shape.has(.function_spaced) { return none }
	syntax := flat.source_type_syntax(clean_id)
	if !syntax.function_closed() { return none }
	mut params := []string{}
	mut has_c_abi_param := false
	for part in syntax.checker_function_params() {
		ct, is_c_abi := tc.c_abi_fn_param_type(part)
		params << ct
		if is_c_abi { has_c_abi_param = true }
	}
	if !has_c_abi_param { return none }
	ret_type := if syntax.function_space_result() != 0 {
		tc.parse_type(syntax.function_space_result())
	} else {
		empty_type
	}
	return naming.fn_ptr_encoded(tc.fn_ptr_return_c_type(ret_type), params)
}

// c_abi_fn_ptr_type_for_type_text returns the C ABI function-pointer encoding retained
// by a source function type or alias, including `const_` pointer parameters.
pub fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text(typ_id u32) ?string {
	mut seen := map[u32]bool{}
	return tc.c_abi_fn_ptr_type_for_type_text_inner(trimmed_source_type_id(typ_id), mut seen)
}

fn (tc &TypeChecker) c_abi_fn_ptr_type_for_type_text_inner(typ_id u32, mut seen map[u32]bool) ?string {
	if typ_id == 0 || seen[typ_id] { return none }
	seen[typ_id] = true
	if c_abi_fn := tc.c_abi_fn_ptr_type_from_id(typ_id) { return c_abi_fn }
	for name_id in [tc.qualify_type_name(typ_id), typ_id] {
		if name_id == 0 { continue }
		if c_abi_fn := tc.type_alias_c_abi_fns[name_id] { return c_abi_fn }
		if target_source_id := tc.type_aliases.type_id(name_id) {
			if c_abi_fn := tc.c_abi_fn_ptr_type_for_type_text_inner(target_source_id, mut seen) {
				return c_abi_fn
			}
		}
	}
	return none
}

fn (tc &TypeChecker) c_abi_fn_param_type(param_id u32) (string, bool) {
	clean_id := trimmed_source_type_id(param_id)
	param_type := flat.source_type_syntax(clean_id).parameter_type()
	is_pointer := flat.source_type_shape(param_type).has(.pointer)
	if is_pointer && flat.source_type_syntax(c_abi_fn_param_name(clean_id)).const_name_prefix() {
		base_type := tc.parse_type(flat.source_type_syntax(param_type).raw_base())
		return 'const ${tc.c_type(base_type)}*', true
	}
	if is_pointer {
		if ct := tc.c_abi_alias_pointer_param_c_type(flat.source_type_syntax(param_type).raw_base()) {
			if flat.source_type_shape(clean_id).has(.mutable_type) { return '${ct}*', true }
			return 'const ${ct}*', true
		}
	}
	return tc.c_type(tc.parse_type(param_type)), false
}

fn (tc &TypeChecker) c_abi_alias_pointer_param_c_type(typ_id u32) ?string {
	t := tc.parse_type(typ_id)
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
	if t is Struct && flat.source_type_query(t.source_type_id()).has(.c_prefix) {
		return t
	}
	return none
}

fn c_abi_fn_param_name(param_id u32) u32 {
	return flat.source_type_syntax(param_id).parameter_name()
}

fn (tc &TypeChecker) comptime_static_type_expr_name(id flat.NodeId) ?u32 {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return none
	}
	node := tc.a.nodes[int(id)]
	if node.kind == .array_init && node.source_value_id() == checker_source_v3_comptime_type_array
		&& node.children_count == 1 {
		elem := tc.comptime_static_type_expr_name(tc.a.child(&node, 0)) or { return none }
		return flat.source_type_composition_one(.array, elem)
	}
	if node.kind == .typeof_expr {
		if node.source_value_id() != 0 && !tc.type_text_has_generic_placeholder(node.source_value_id()) {
			return node.source_value_id()
		}
		if node.children_count == 1 {
			typ := tc.resolve_type(tc.a.child(&node, 0))
			if typ !is Unknown && !tc.type_text_has_generic_placeholder(typ.source_type_id()) {
				return typ.source_type_id()
			}
		}
		return none
	}
	if node.kind == .selector && node.children_count == 1 {
		base_name := tc.comptime_static_type_expr_name(tc.a.child(&node, 0)) or { return none }
		mut base_type := *tc.parse_type(base_name)
		if base_type is Unknown {
			return none
		}
		if node.value == 'typ' {
			return base_name
		}
		if node.value == 'unaliased_typ' {
			return unalias_type(base_type).source_type_id()
		}
		if node.value in ['payload_type', 'pointee_type'] {
			base_type = unalias_type(base_type)
			if base_type is OptionType {
				base_type = unalias_type(base_type.base_type)
			} else if base_type is ResultType {
				base_type = unalias_type(base_type.base_type)
			}
			if node.value == 'payload_type' {
				return base_type.source_type_id()
			}
			if base_type is Pointer {
				return base_type.base_type.source_type_id()
			}
			return none
		}
		base_type = unalias_type(base_type)
		if node.value == 'element_type' {
			if base_type is Array {
				return base_type.elem_type.source_type_id()
			}
			if base_type is ArrayFixed {
				return base_type.elem_type.source_type_id()
			}
			return none
		}
		if base_type is Map {
			if node.value == 'key_type' {
				return base_type.key_type.source_type_id()
			}
			if node.value == 'value_type' {
				return base_type.value_type.source_type_id()
			}
		}
		return none
	}
	if node.kind != .ident || node.source_value_id() == 0
		|| tc.type_text_has_generic_placeholder(node.source_value_id()) {
		return none
	}
	return node.source_value_id()
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
	types            []&Type
	filled           []u8
	type_generation  u8  = 1
	call_generation  u32 = 1
	call_ids         [2048]int
	call_generations [2048]u32
	// A slot is read only after its node id and generation match. Initialize
	// entries when publishing a call result, avoiding 2048 unused defaults.
	call_infos []CallInfo = empty_call_info_memo()
}

fn empty_call_info_memo() []CallInfo {
	mut slots := []CallInfo{cap: 2048}
	// Expose reserved slots without constructing their unused default payloads.
	// Node and generation guards prevent reading a slot before its first write.
	unsafe { slots.len = 2048 }
	return slots
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
	wtc.lexical_parent_memo = unsafe { nil }
}

fn (mut memo BodyResolveMemo) begin(lo int, hi int) {
	if lo < 0 || hi < lo {
		memo.active = false
		return
	}
	span := hi - lo + 1
	memo.lo = lo
	memo.hi = hi
	memo.advance_call_generation()
	memo.type_generation = 1
	if memo.types.len < span {
		// A type is read only after its filled byte is set for this function.
		memo.types = unsafe { []&Type{len: span} }
		memo.filled = []u8{len: span}
	} else {
		unsafe { vmemset(memo.filled.data, 0, span) }
	}
	memo.active = true
}

// Pre-check reachability enters scopes before their bindings are checked. A
// new epoch rejects resolutions from the previous scope or binding state without
// clearing the whole function range on every local declaration.
fn (mut memo BodyResolveMemo) advance_context() {
	memo.type_generation++
	if memo.type_generation == 0 {
		unsafe { vmemset(memo.filled.data, 0, memo.hi - memo.lo + 1) }
		memo.type_generation = 1
	}
	memo.advance_call_generation()
}

@[inline]
fn (mut memo BodyResolveMemo) advance_call_generation() {
	memo.call_generation++
	if memo.call_generation == 0 {
		memo.call_generations = [2048]u32{}
		memo.call_generation = 1
	}
}

// resolve_type returns the type of an expression in the current checker context.
@[direct_array_access; inline]
pub fn (tc &TypeChecker) resolve_type(id flat.NodeId) Type {
	if tc.trust_checked_expr_types && isnil(tc.storage_query_probe) {
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
				return tc.widen_mixed_integer_expr_type(id, typ)
			}
		}
	}
	return tc.resolve_type_slow(id)
}

// resolve_type_reference returns the canonical reference for an expression's resolved type.
// It follows resolve_type's checked-cache visibility and mixed-integer widening.
// The reference remains valid while the owning checker arena is live.
@[direct_array_access; inline]
pub fn (tc &TypeChecker) resolve_type_reference(id flat.NodeId) &Type {
	if tc.trust_checked_expr_types && isnil(tc.storage_query_probe) {
		tidx := int(id)
		if tidx >= 0 && tidx < tc.expr_type_set.len && tc.expr_type_set[tidx]
			&& (!tc.parallel_check_sparse || tc.in_check_range(tidx)) {
			typ := tc.expr_type_values[tidx]
			if !type_contains_unknown(typ) {
				// Non-arithmetic nodes retain their existing canonical identity.
				if !mixed_integer_expr_type_can_widen(typ) || tidx >= tc.a.nodes.len
					|| tc.a.nodes[tidx].kind != .infix {
					return typ
				}
				return tc.intern_type_reference(tc.widen_mixed_integer_infix_type(id, typ))
			}
		}
	}
	return tc.intern_type_reference(tc.resolve_type_slow(id))
}

@[direct_array_access]
fn (tc &TypeChecker) resolve_type_slow(id flat.NodeId) Type {
	if tc.trust_checked_expr_types && !isnil(tc.storage_query_probe) {
		cached_type_ref0 := tc.cached_expr_type_ref(id)
		if !isnil(cached_type_ref0) {
			if !type_contains_unknown(cached_type_ref0) {
				return tc.widen_mixed_integer_expr_type(id, cached_type_ref0)
			}
		}
	}
	if !isnil(tc.storage_query_probe) {
		if typ := tc.storage_query_probe_checked_type(id) {
			return tc.widen_mixed_integer_expr_type(id, typ)
		}
	}
	// Literal types are fixed here after checked/probed types have had precedence.
	// Return them before allocating or probing the per-body semantic memo.
	if int(id) >= 0 && int(id) < tc.a.nodes.len {
		node := tc.a.node(id)
		match node.kind {
			.int_literal {
				if tc.translated_files.len == 0 { return builtin_int_type }
			}
			.float_literal { return builtin_f64_type }
			.bool_literal { return builtin_bool_type }
			.char_literal {
				if !node.value.starts_with('c:') { return builtin_rune_type }
			}
			.string_literal {
				if node.children_count != 1
					|| node.value !in ['__v3_comptime_zero', '__v3_comptime_new'] {
					return builtin_string_type
				}
			}
			.string_interp { return builtin_string_type }
			.sizeof_expr, .offsetof_expr { return builtin_u32_type }
			.nil_literal { return builtin_voidptr_type }
			.none_expr { return Type(OptionType{}) }
			else {}
		}
	}
	memo := tc.body_resolve_memo
	idx := int(id)
	if isnil(memo) || !memo.active || idx < memo.lo || idx > memo.hi {
		return tc.resolve_type_uncached(id)
	}
	mi := idx - memo.lo
	if memo.filled[mi] == memo.type_generation {
		return tc.widen_mixed_integer_expr_type(id, memo.types[mi])
	}
	typ := tc.resolve_type_uncached(id)
	// Unknowns can be provisional (cycle guards, generic placeholders that a
	// later registration resolves); never memoize them, including when they are
	// nested inside a collection or wrapper.
	if !type_contains_unknown(typ) {
		mut m := unsafe { &BodyResolveMemo(memo) }
		m.types[mi] = tc.intern_type_reference(typ)
		m.filled[mi] = memo.type_generation
	}
	return tc.widen_mixed_integer_expr_type(id, typ)
}

const narrow_integer_type_names = ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32',
	'u64', 'uint', 'rune', 'char']

@[inline]
fn narrow_integer_type_for_widening(typ Type) bool {
	if typ is Primitive {
		if typ.props.has(.boolean) {
			return false
		}
		if typ.props.has(.integer) {
			return typ.size in [8, 16, 32, 64] || (typ.size == 0 && !typ.props.has(.unsigned))
		}
		// Primitive names without an integer/float property default to `int`.
		return !typ.props.has(.float)
	}
	return flat.source_type_syntax(typ.source_type_id()).unqualified() in [
		flat.source_type_int,
		flat.source_type_i8,
		flat.source_type_i16,
		flat.source_type_i32,
		flat.source_type_i64,
		flat.source_type_isize,
		flat.source_type_u8,
		flat.source_type_u16,
		flat.source_type_u32,
		flat.source_type_u64,
		flat.source_type_uint,
		flat.source_type_rune,
		flat.source_type_char,
	]
}

@[inline]
fn wide_integer_type_for_widening(typ Type) bool {
	if typ is Primitive {
		return !typ.props.has(.boolean) && typ.props.has(.integer) && typ.size == 128
	}
	return flat.source_type_syntax(typ.source_type_id()).unqualified() in [
		qualify_source_type_u128,
		qualify_source_type_i128,
	]
}

@[inline]
fn mixed_integer_expr_type_can_widen(typ &Type) bool {
	return match typ {
		Primitive { typ.props.has(.integer) && typ.size < 128 }
		ISize, USize, Rune, Char { true }
		else { false }
	}
}

// widen_mixed_integer_expr_type gives an arithmetic node the 128-bit type of its
// widest operand. The type recorded for an infix in argument position is the
// narrower operand's, while the same expression assigned to a variable gets the
// 128-bit type from the promotion ladder. Printing, `typeof` and interpolation all
// read the recorded type, so they cut a mixed expression to 64 bits without this.
@[direct_array_access; inline]
fn (tc &TypeChecker) widen_mixed_integer_expr_type(id flat.NodeId, typ &Type) Type {
	if !mixed_integer_expr_type_can_widen(typ) {
		return *typ
	}
	tidx := int(id)
	if tidx < 0 || tidx >= tc.a.nodes.len || tc.a.nodes[tidx].kind != .infix {
		return *typ
	}
	return tc.widen_mixed_integer_infix_type(id, typ)
}

fn (tc &TypeChecker) widen_mixed_integer_infix_type(id flat.NodeId, typ &Type) Type {
	node := tc.a.node(id)
	if !narrow_integer_type_for_widening(*typ) {
		return *typ
	}
	if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
		return *typ
	}
	// A shift result is as wide as its left operand: the right one is a count, so
	// a wide count does not make `u64(4) << count` a 128-bit expression, and the
	// generator still emits a 64-bit operation for it.
	shift := node.op in [.left_shift, .right_shift, .right_shift_unsigned]
	child_limit := if shift { 1 } else { node.children_count }
	for i in 0 .. child_limit {
		child_type := tc.resolve_type(tc.a.child(node, i))
		if wide_integer_type_for_widening(child_type) {
			return child_type
		}
	}
	return *typ
}

@[direct_array_access; inline]
fn (tc &TypeChecker) resolve_type_uncached(id flat.NodeId) Type {
	if int(id) < 0 || int(id) >= tc.a.nodes.len {
		return unknown_type('missing node')
	}
	node := tc.a.node(id)
	match node.kind {
		.int_literal {
			if tc.translated_files.len == 0 { return builtin_int_type }
		}
		.float_literal { return builtin_f64_type }
		.bool_literal { return builtin_bool_type }
		.char_literal {
			if !node.value.starts_with('c:') { return builtin_rune_type }
		}
		.string_literal {
			if node.children_count != 1
				|| node.value !in ['__v3_comptime_zero', '__v3_comptime_new'] {
				return builtin_string_type
			}
		}
		.string_interp { return builtin_string_type }
		.sizeof_expr, .offsetof_expr { return builtin_u32_type }
		.nil_literal { return builtin_voidptr_type }
		.none_expr { return Type(OptionType{}) }
		else {}
	}
	return tc.resolve_type_nonliteral(id)
}

@[direct_array_access]
fn (tc &TypeChecker) resolve_type_nonliteral(id flat.NodeId) Type {
	node := tc.a.node(id)
	match node.kind {
		.int_literal {
			if tc.node_is_in_translated_file(id) {
				if typ := tc.translated_integer_literal_type(id) {
					return typ
				}
			}
			return builtin_int_type
		}
		.ident {
			if tc.errors.len > 0 {
				if typ := tc.ident_error_type(id) {
					return typ
				}
			}
		}
		.selector {
			if tc.errors.len > 0 && tc.errors.any(it.node == id
				&& it.msg.contains(' does not return a value')) {
				return builtin_void_type
			}
		}
		.infix {
			if tc.errors.len > 0
				&& tc.errors.any(it.node == id && it.msg.starts_with('expected type is not an enum (')) {
				return builtin_void_type
			}
		}
		.call {
			if tc.errors.len > 0 && tc.errors.any(it.node == id && it.msg.starts_with('unknown enum `')) {
				return builtin_void_type
			}
			if node.typ != 0 && !call_type_annotation_is_placeholder(node.typ) {
				return *tc.parse_type(node.typ)
			}
			if t := tc.resolved_call_type(id) {
				return t
			}
		}
		.directive {
			if node.value == '\$res' {
				return_type := tc.fn_context.return_type
				if return_type is ResultType || return_type is Void {
					return builtin_void_type
				}
				if return_type is MultiReturn {
					if node.children_count == 0 {
						return builtin_void_type
					}
					index_node := tc.a.child_node(node, 0)
					if index_node.kind != .int_literal {
						return builtin_void_type
					}
					index := index_node.value.int()
					if index < 0 || index >= return_type.types.len {
						return builtin_void_type
					}
					return return_type.types[index]
				}
				return return_type
			}
		}
		.string_literal {
			if node.children_count == 1
				&& node.value in ['__v3_comptime_zero', '__v3_comptime_new'] {
				if type_name_source_id := tc.comptime_static_type_expr_name(tc.a.child(node, 0)) {
					target := tc.parse_type(type_name_source_id)
					if target !is Unknown {
						return if node.value == '__v3_comptime_new' {
							Type(Pointer{
								base_type: tc.intern_type_reference(target)
							})
						} else {
							*target
						}
					}
				}
			}
			return builtin_string_type
		}
		.paren {
			if node.children_count > 0 {
				return tc.resolve_type(tc.a.child(node, 0))
			}
		}
		.expr_stmt {
			if node.children_count == 0 {
				return builtin_void_type
			}
			if node.children_count == 1 {
				return tc.resolve_type(tc.a.child(node, 0))
			}
			mut expr_types := []Type{cap: node.children_count}
			for i in 0 .. node.children_count {
				value_id := tc.a.child(node, i)
				typ := tc.expr_type(value_id) or { tc.resolve_type(value_id) }
				expr_types << typ
			}
			return Type(MultiReturn{
				types: expr_types
			})
		}
		.float_literal {
			return builtin_f64_type
		}
		.bool_literal {
			return builtin_bool_type
		}
		.string_interp {
			return builtin_string_type
		}
		.sizeof_expr, .offsetof_expr {
			return builtin_u32_type
		}
		.nil_literal {
			return builtin_voidptr_type
		}
		.none_expr {
			return Type(OptionType{})
		}
		.fn_literal {
			return tc.fn_literal_type(*node)
		}
		.lambda_expr {
			return tc.lambda_expr_type(*node)
		}
		.char_literal {
			if node.value.starts_with('c:') {
				return Type(Pointer{
					base_type: tc.intern_type_reference(Type(u8_))
				})
			}
			return builtin_rune_type
		}
		.or_expr {
			if aggregate_type := tc.sql_aggregate_or_expr_type(*node) {
				return aggregate_type
			}
			if value_type := tc.match_trailing_or_value_type(tc.a.child(node, 0)) {
				return value_type
			}
			if payload := tc.or_expr_payload_type(tc.a.child(node, 0)) {
				return payload
			}
			inner := tc.resolve_type(tc.a.child(node, 0))
			if inner is OptionType {
				return *inner.base_type
			}
			if inner is ResultType {
				return *inner.base_type
			}
			return inner
		}
		else {}
	}
	kind_id := int(node.kind)
	if smart_type := tc.smartcast_type(id) {
		return smart_type
	}
	if kind_id == 14 {
		// A generic method named with its type arguments, `h.first[int]`: the
		// type its check gave it (see check_generic_method_value), which the
		// resolution of an index cannot tell from an index of the method value.
		if node.children_count > 0 && tc.a.child_node(node, 0).kind == .selector {
			cached_type_ref1 := tc.cached_expr_type_ref(id)
			if !isnil(cached_type_ref1) {
				typ_raw := *cached_type_ref1
				if cached_type_ref1 is FnType {
					return typ_raw
				}
			}
		}
		return tc.resolve_index_type(*node)
	}
	if node.kind == .infix && node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
		return builtin_bool_type
	}
	if !(tc.smartcasts.len > 0 && (kind_id == 7 || kind_id == 13)) {
		cached_type_ref2 := tc.cached_expr_type_ref(id)
		if !isnil(cached_type_ref2) {
			// A resolution-only annotation can cache `void` for a method whose
			// receiver is a local alias that is only known during body checking.
			// Re-resolve calls in that case instead of hiding the real return type.
			if node.kind != .call || cached_type_ref2 !is Void {
				return *cached_type_ref2
			}
		}
	}
	if node.kind == .selector {
		if typ := tc.global_type_for_selector(*node) {
			return typ
		}
		if typ := tc.const_type_for_selector(*node) {
			return typ
		}
	}
	if node.kind == .defer_result && node.typ == qualify_source_type_invalid {
		return builtin_void_type
	}
	if node.typ != 0 && node.typ != flat.source_type_unknown && !(kind_id == 12
		&& call_type_annotation_is_placeholder(node.typ)) {
		return *tc.parse_type(node.typ)
	}
	match node.kind {
		.int_literal {
			return builtin_int_type
		}
		.float_literal {
			return builtin_f64_type
		}
		.bool_literal {
			return builtin_bool_type
		}
		.char_literal {
			return builtin_rune_type
		}
		.string_literal, .string_interp {
			return builtin_string_type
		}
		.nil_literal {
			return builtin_voidptr_type
		}
		.none_expr {
			return Type(OptionType{})
		}
		.spawn_expr {
			if node.children_count == 0 {
				return *tc.parse_type(checker_source_thread)
			}
			child_id := tc.a.child(node, 0)
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
				return *tc.parse_type(checker_source_thread)
			}
			return *tc.parse_type(flat.source_type_composition_one(.thread, spawn_ret.source_type_id()))
		}
		.enum_val {
			return builtin_int_type
		}
		.defer_result {
			return tc.defer_result_type(*node) or { unknown_type('invalid `res` expression') }
		}
		.ident {
			name := node.value_text()
			if name == '_' {
				return builtin_void_type
			}
			if is_bare_generic_param(name) {
				active_generic_ident := name in tc.fn_context.generic_params
					|| tc.active_generic_param(name)
					|| tc.node_has_enclosing_generic_param(id, name)
					|| tc.source_enclosing_fn_has_generic_param(id, name)
				if active_generic_ident {
					return generic_placeholder_type(node.source_value_id())
				}
			}
			// The smartcast of this ident was looked up before the cached and
			// declared types above, and none of those lookups narrow it.
			if !tc.bare_name_is_const_owned_over_global(name) {
				if typ := tc.non_file_scope_type(name) {
					return typ
				}
			}
			qname := tc.qualify_name(name)
			if qname != name {
				if typ := tc.file_scope.lookup(qname) {
					return typ
				}
			}
			if key := tc.shadowed_builtin_const_key(name) {
				return tc.const_types[key] or { builtin_void_type }
			}
			if typ := tc.const_types[qname] {
				if tc.bare_const_key_is_visible(qname) {
					return typ
				}
			}
			if typ := tc.file_scope.lookup(name) {
				return typ
			}
			if typ := tc.const_types[name] {
				if tc.bare_const_key_is_visible(name) {
					return typ
				}
			}
			// A `.vsh` script uses `os` constants such as `args` unqualified.
			if key := tc.vsh_os_const_key(name) {
				if typ := tc.const_types[key] {
					return tc.const_type_from_initializer(key, typ)
				}
			}
			if typ := tc.fn_value_type(name) {
				return typ
			}
			if tc.selective_import_symbol_is_ambiguous(name) {
				return unknown_type_quoting('ambiguous selective import', name)
			}
			if is_bare_generic_param(name) {
				return builtin_void_type
			}
			return unknown_type_quoting('unknown identifier', name)
		}
		.call {
			if type_name_id := tc.translated_named_cast_call_name_id(id, *node) {
				return *tc.parse_type(type_name_id)
			}
			fn_node := tc.a.child_node(node, 0)
			if _ := tc.builtin_isreftype_call_arg(*node) {
				return builtin_bool_type
			}
			if arg_id := tc.builtin_addr_call_arg(*node) {
				return Type(Pointer{
					base_type: tc.intern_type_reference(tc.resolve_type(arg_id))
				})
			}
			if fn_node.kind !in [.ident, .selector] {
				fn_type := tc.resolve_type(tc.a.child(node, 0))
				if fn_typ := fn_type_from_type(fn_type) {
					return *fn_typ.return_type
				}
			}
			if fn_node.kind == .ident {
				if typ := tc.cur_scope.lookup(fn_node.value) {
					if typ is FnType {
						return *typ.return_type
					}
					if typ is Alias || typ is Pointer {
						if fn_typ := fn_type_from_type(typ) {
							return *fn_typ.return_type
						}
					}
				}
			}
			if fn_node.kind == .selector {
				base_node := tc.a.child_node(fn_node, 0)
				if base_node.kind == .ident && base_node.value == 'C' {
					c_fn_name := 'C.${fn_node.value}'
					// `C.X(v)` with a declared `type C.X = T` is a cast to that alias,
					// as check_c_alias_cast_call checks it; it is not an opaque C struct.
					if node.children_count == 2 {
						if c_name_id := flat.source_type_existing_id(c_fn_name) {
							if tc.type_aliases.contains(c_name_id) {
								return *tc.parse_type(c_name_id)
							}
						}
					}
					// Modules may declare the same C function with different, but
					// compatible, signatures; a call uses the one visible to its module,
					// as call_info does, not whichever declaration was registered last.
					if module_name := tc.visible_c_fn_module(c_fn_name) {
						module_key := c_fn_module_signature_key(module_name, c_fn_name)
						if ret := tc.c_fn_module_ret_types[module_key] {
							return ret
						}
					}
					if ret := tc.fn_ret_types[c_fn_name] {
						return ret
					}
					if ret := tc.fn_ret_types[fn_node.value] {
						return ret
					}
					return Type(Struct{
						name_id: flat.source_type_id(c_fn_name)
					})
				}
				if base_node.kind == .ident {
					if !tc.ident_resolves_to_value(base_node.value) {
						if resolved := tc.resolve_import_alias(base_node.value) {
							if imported_name := tc.imported_fn_key(resolved, fn_node.value) {
								return tc.fn_ret_types[imported_name] or {
									unknown_type_quoting('unknown return type for', imported_name)
								}
							}
							mod_name := '${resolved}.${fn_node.value}'
							if mod_name_id := flat.source_type_existing_id(mod_name) {
								if tc.sum_types.contains(mod_name_id) {
									return Type(SumType{
										name_id: mod_name_id
									})
								}
								if mod_name_id in tc.structs {
									return Type(Struct{
										name_id: mod_name_id
									})
								}
								if mod_name_id in tc.enum_names {
									return Type(Enum{
										name_id: mod_name_id
									})
								}
							}
						}
					}
					if base_node.source_value_id() in tc.structs || base_node.source_value_id() in tc.enum_names {
						qname := tc.qualify_name(base_node.value)
						sname := '${qname}.${fn_node.value}'
						if ret := tc.fn_ret_types[sname] {
							return ret
						}
					} else {
						if static_name := tc.static_assoc_fn_key_for_base(base_node.source_value_id(), fn_node.value) {
							return tc.fn_ret_types[static_name] or {
								unknown_type_quoting('unknown return type for', static_name)
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
						if static_name := tc.static_assoc_fn_key_for_base(flat.source_type_id('${mod_name}.${base_node.value}'), fn_node.value) {
							return tc.fn_ret_types[static_name] or {
								unknown_type_quoting('unknown return type for', static_name)
							}
						}
					}
				}
				if fn_typ := tc.selector_const_fn_type(fn_node) {
					return *fn_typ.return_type
				}
				base_id := tc.a.child(fn_node, 0)
				base_type := tc.selector_fn_base_type(base_id) or { tc.resolve_type(base_id) }
				// A method call on a value whose type is not known yet has no known
				// return type either. Falling through would type it as a free function
				// that shares the method's name (C `write` for `res.write()` with
				// -target-libc-headers), and that wrong type would be memoized.
				if base_type is Unknown {
					return unknown_type('unknown receiver type for `.${fn_node.value}()`')
				}
				if fn_typ := tc.selector_field_fn_type(fn_node, base_type) {
					return *fn_typ.return_type
				}
				clean_type := unwrap_pointer(base_type)
				clean_type_raw := clean_type
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
					candidates := receiver_method_name_candidates(clean_type_raw, fn_node.value, tc.cur_module)
					for mname in candidates {
						if ret := tc.fn_ret_types[mname] {
							return tc.alias_return_type_from_text(mname) or { ret }
						}
					}
					if mname := tc.unique_receiver_method_suffix_match(clean_type_raw, candidates) {
						return tc.alias_return_type_from_text(mname) or {
							tc.fn_ret_types[mname] or {
								unknown_type_quoting('unknown return type for', mname)
							}
						}
					}
					// A method that the alias inherits through another alias (`type
					// WrappedNames = Names` with `fn (n Names) clone()`) wins over the
					// builtin collection method of the same name, as in a direct call.
					if mname := tc.concrete_method_signature_key(clean_type.source_type_id(),
						fn_node.value)
					{
						if owner_id := flat.source_type_existing_id(mname.all_before_last('.')) {
							if owner_id in tc.type_aliases.ids {
								if ret := tc.fn_ret_types[mname] {
									return tc.alias_return_type_from_text(mname) or { ret }
								}
							}
						}
					}
				}
				if clean_array := array_like_type_for_method(clean_type, fn_node.value) {
					// Dispatch by length before comparing method names on this hot path.
					match fn_node.value.len {
						3 {
							if fn_node.value == 'any' || fn_node.value == 'all' {
								return builtin_bool_type
							}
							if fn_node.value == 'pop' {
								return array_elem_type(clean_array)
							}
							if fn_node.value == 'str' {
								return builtin_string_type
							}
							if fn_node.value == 'map' {
								return tc.array_map_result_type_from_receiver(clean_type, Type(Unknown{
									reason: 'array.map'
								}))
							}
						}
						4 {
							if fn_node.value == 'move' {
								return clean_type
							}
							if fn_node.value == 'last' {
								return array_elem_type(clean_array)
							}
							if fn_node.value == 'sort' {
								return builtin_void_type
							}
							if fn_node.value == 'join' {
								return builtin_string_type
							}
							if fn_node.value == 'wait' {
								// `[]thread T`.wait() joins all threads and returns `[]T`. A bare
								// `[]thread` (threads with no return value) joins to `void`. Any
								// optional/result payloads remain per-thread, so
								// `[]thread !T`.wait()` is `[]!T`.
								// other array element type is not a thread and `.wait()` is
								// unsupported, so reject it rather than mis-typing the call as the
								// receiver array (which would emit invalid C joining non-handles).
								elem := unalias_type(array_elem_type(clean_array))
								if elem is Struct {
									if elem.source_type_id() == checker_source_thread {
										return builtin_void_type
									}
									if flat.source_type_shape(elem.source_type_id()).has(.thread_type) {
										return tc.thread_array_wait_return_type(flat.source_type_syntax(elem.source_type_id()).raw_base())
									}
								}
								return unknown_type('`.wait()` requires an array of threads')
							}
						}
						5 {
							if fn_node.value == 'clone' {
								return clean_type
							}
							if fn_node.value == 'first' {
								return array_elem_type(clean_array)
							}
							if fn_node.value == 'count' || fn_node.value == 'index' {
								return builtin_int_type
							}
						}
						6 {
							if fn_node.value == 'filter' || fn_node.value == 'sorted' {
								if receiver_is_fixed_array(clean_type) {
									// Filtering a fixed array yields a dynamic array.
									return Type(Array{
										elem_type: clean_array.elem_type
									})
								}
								return clean_type
							}
							if fn_node.value == 'repeat' {
								return base_type
							}
						}
						7 {
							if fn_node.value == 'reverse' {
								return clean_type
							}
						}
						8 {
							if fn_node.value == 'contains' {
								return builtin_bool_type
							}
						}
						15 {
							if fn_node.value == 'repeat_to_depth' {
								return base_type
							}
						}
						19 {
							if fn_node.value == 'sorted_with_compare' {
								if receiver_is_fixed_array(clean_type) {
									return Type(Array{
										elem_type: clean_array.elem_type
									})
								}
								return clean_type
							}
						}
						else {}
					}
					elem_type := array_elem_type(clean_array)
					elem_name_id := elem_type.source_type_id()
					mut short_elem_id := elem_name_id
					mut mod_prefix_id := u32(0)
					if flat.source_type_shape(elem_name_id).has(.dotted) {
						elem_syntax := flat.source_type_syntax(elem_name_id)
						short_elem_id = elem_syntax.unqualified()
						mod_prefix_id = elem_syntax.qualifier()
					}
					arr_mname1 := '[]${flat.source_type_name(short_elem_id)}.${fn_node.value}'
					if mod_prefix_id != 0 {
						arr_mkey := '${flat.source_type_name(mod_prefix_id)}.${arr_mname1}'
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
					return unknown_type_quoting('unknown array method', fn_node.value)
				}
				if clean_type is Map {
					if fn_node.value in ['clone', 'move'] {
						return base_type
					}
					if fn_node.value in ['delete', 'clear', 'free'] {
						return builtin_void_type
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
					for mname in receiver_method_name_candidates(clean_type_raw, fn_node.value, tc.cur_module) {
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
					return unknown_type_quoting('unknown map method', fn_node.value)
				}
				if clean_type is String {
					mname := 'string.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
				if fn_node.value == 'str'
					&& (clean_type is Primitive || clean_type is Char || clean_type is Rune) {
					return builtin_string_type
				}
				if (clean_type is Void || clean_type is Primitive)
					&& fn_node.value in ['vstring', 'vstring_with_len'] {
					return builtin_string_type
				}
				if clean_type is Alias {
					mname := '${clean_type.name()}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
					base_name := resolve_type_name_for_method(clean_type.base_type)
					if base_name != 0 {
						for base_mname in receiver_method_name_candidates(clean_type.base_type, fn_node.value, tc.cur_module) {
							if ret := tc.fn_ret_types[base_mname] {
								return ret
							}
						}
					}
				}
				if clean_type is Struct {
					mname := '${clean_type.name()}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
					// A method on a concrete generic instance (`Box[int].clone`) is
					// registered under the open form (`Box[T].clone`); resolve it so the
					// call types as the substituted return (`Box[int]`) rather than the
					// bare base the collapsed open signature would yield.
					if ci := tc.resolve_generic_struct_method(clean_type.source_type_id(), fn_node.value) {
						return ci.return_type
					}
				}
				if clean_type is Interface {
					mname := '${clean_type.name()}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
				if clean_type is SumType {
					mname := '${clean_type.name()}.${fn_node.value}'
					if ret := tc.fn_ret_types[mname] {
						return ret
					}
				}
				if clean_type is Enum {
					if fn_node.value == 'str' {
						return builtin_string_type
					}
					mname := '${clean_type.name()}.${fn_node.value}'
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
					unknown_type_quoting('unknown return type for', local_name)
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
			if node.typ != 0 {
				return *tc.parse_type(node.typ)
			}
			$if debug {
				if tc.verbose {
					eprintln('warning: unknown fn return type `${fn_node.value}`')
				}
			}
			return unknown_type_quoting('unknown function', fn_node.value)
		}
		.infix {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return builtin_bool_type
			}
			lhs_id := tc.a.child(node, 0)
			rhs_id := tc.a.child(node, 1)
			lt := tc.infix_read_type(lhs_id)
			lt_raw := lt
			rt := tc.infix_read_type(rhs_id)
			rt_raw := rt
			if lt is Void || rt is Void {
				return builtin_void_type
			}
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned]
				&& (!unalias_type(lt).is_integer() || !unalias_type(rt).is_integer())
				&& !(tc.node_is_in_translated_file(id) && translated_integer_type(lt)
					&& translated_integer_type(rt)) {
				return builtin_void_type
			}
			if node.op == .left_shift && array_type_from_receiver(lt) != none {
				return builtin_void_type
			}
			if node.op in [.plus, .minus] {
				if node.op == .minus && lt is Pointer && rt is Pointer {
					return builtin_int_type
				}
				if lt is Pointer && (rt_raw.is_integer()
					|| (tc.node_is_in_translated_file(id) && translated_integer_type(rt_raw))) {
					return lt_raw
				}
				if node.op == .plus && rt is Pointer && (lt_raw.is_integer()
					|| (tc.node_is_in_translated_file(id) && translated_integer_type(lt_raw))) {
					return rt_raw
				}
			}
			if operator_ret := tc.infix_operator_return_type(node.op, lt, rt) {
				return operator_ret
			}
			if node.op == .right_shift_unsigned {
				if tc.node_is_in_translated_file(id) {
					return unsigned_shift_result_type(tc.translated_promoted_shift_type(lt))
				}
				// Untyped integer literals retain the language's 32-bit default. An
				// explicit `int(...)` uses the target-width `int` instead.
				if tc.integer_literal_source(lhs_id) != none {
					return builtin_u32_type
				}
				return unsigned_shift_result_type(lt)
			}
			if node.op in [.left_shift, .right_shift] {
				if tc.node_is_in_translated_file(id)
					&& (tc.integer_shift_bit_size(lt) < 32 || *unalias_type(lt) == Type(bool_)) {
					return Type(i32_)
				}
				return lt_raw
			}
			if tc.node_is_in_translated_file(id) && translated_numeric_type(lt)
				&& translated_numeric_type(rt)
				&& node.op in [.plus, .minus, .mul, .div, .mod, .amp, .pipe, .xor] {
				return tc.translated_common_numeric_type(lt, rt)
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
			if int_promoted := tc.int_literal_promoted_infix_type(lhs_id, rhs_id, lt,
				rt) {
				return int_promoted
			}
			if int_promoted := tc.int_literal_promoted_infix_type(rhs_id, lhs_id, rt,
				lt) {
				return int_promoted
			}
			if lt.is_float() || rt.is_float() {
				if type_is_f32(lt) && (type_is_f32(rt) || unalias_type(rt).is_integer()
					|| tc.is_untyped_float_literal_expr(rhs_id)) {
					return builtin_f32_type
				}
				if type_is_f32(rt) && (type_is_f32(lt) || unalias_type(lt).is_integer()
					|| tc.is_untyped_float_literal_expr(lhs_id)) {
					return builtin_f32_type
				}
				return builtin_f64_type
			}
			if promoted := infix_integer_promotion_type(lt, rt) {
				return promoted
			}
			return lt
		}
		.prefix {
			if node.op == .amp && node.children_count > 0 {
				if cast := tc.translated_pointer_cast_node(id, *node) {
					return *tc.parse_type(cast.source_value_id())
				}
				child_id := tc.a.child(node, 0)
				if inner := tc.smartcast_type(child_id) {
					return Type(Pointer{
						base_type: tc.intern_type_reference(inner)
					})
				}
			}
			if node.children_count > 0 && tc.node_is_in_translated_file(id)
				&& node.op in [.plus, .minus, .bit_not] {
				child_type := tc.resolve_type(tc.a.child(node, 0))
				if (node.op == .bit_not && translated_integer_type(child_type))
					|| (node.op in [.plus, .minus] && translated_numeric_type(child_type)) {
					return tc.translated_promoted_numeric_type(child_type)
				}
			}
			if node.typ != 0 {
				return *tc.parse_type(node.typ)
			}
			if node.op == .not {
				return builtin_bool_type
			}
			if node.op == .amp {
				child_id := tc.a.child(node, 0)
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
								base_type: tc.intern_type_reference(base)
							})
						}
					}
				}
				inner := tc.smartcast_type(child_id) or { tc.resolve_type(child_id) }
				if inner is Void && tc.expr_subtree_has_error(child_id) {
					return builtin_void_type
				}
				if inner is OptionType {
					return Type(OptionType{
						base_type: tc.intern_type_reference(Type(Pointer{
							base_type: inner.base_type
						}))
					})
				}
				if inner is ResultType {
					return Type(ResultType{
						base_type: tc.intern_type_reference(Type(Pointer{
							base_type: inner.base_type
						}))
					})
				}
				return Type(Pointer{
					base_type: tc.intern_type_reference(inner)
				})
			}
			if node.op == .mul {
				inner := unalias_type(tc.resolve_type(tc.a.child(node, 0)))
				if inner is Pointer {
					return *inner.base_type
				}
				return *inner
			}
			if node.op == .arrow {
				inner := unalias_and_unwrap_pointer_type(tc.resolve_type(tc.a.child(node, 0)))
				if inner is Channel {
					return *inner.elem_type
				}
			}
			return tc.resolve_type(tc.a.child(node, 0))
		}
		.paren {
			return tc.resolve_type(tc.a.child(node, 0))
		}
		.dump_expr {
			if node.children_count > 0 {
				return tc.resolve_type(tc.a.child(node, 0))
			}
			return builtin_void_type
		}
		.or_expr {
			if aggregate_type := tc.sql_aggregate_or_expr_type(*node) {
				return aggregate_type
			}
			if node.children_count > 0 {
				if value_type := tc.match_trailing_or_value_type(tc.a.child(node, 0)) {
					return value_type
				}
				if payload := tc.or_expr_payload_type(tc.a.child(node, 0)) {
					return payload
				}
			}
			inner := tc.resolve_type(tc.a.child(node, 0))
			if inner is OptionType {
				return *inner.base_type
			}
			if inner is ResultType {
				return *inner.base_type
			}
			return inner
		}
		.struct_init {
			// A literal of a generic struct without its type arguments, `Box{ item: 1 }`,
			// has the ones that the types of its fields give it (see
			// infer_generic_struct_init_type): the array or the selector around it asks
			// for its type before the check of the literal records it. The inference
			// reads the checker and parses types, as this function does.
			if !flat.source_type_shape(node.source_value_id()).has(.bracketed) {
				mut checker := unsafe { tc }
				if inferred := checker.infer_generic_struct_init_type(*node) {
					return inferred
				}
			}
			return *tc.parse_type(node.source_value_id())
		}
		.assoc {
			if node.source_value_id() != 0 {
				return *tc.parse_type(node.source_value_id())
			}
			if node.children_count > 0 {
				return tc.resolve_type(tc.a.child(node, 0))
			}
			return unknown_type('missing assoc base')
		}
		.sizeof_expr {
			return builtin_u32_type
		}
		.offsetof_expr {
			return builtin_u32_type
		}
		.cast_expr {
			return *tc.parse_type(node.source_value_id())
		}
		.selector {
			if smart_type := tc.smartcast_type(id) {
				return smart_type
			}
			if typ := tc.enum_selector_type(node) {
				return typ
			}
			if key := tc.selector_fn_value_key(*node) {
				return tc.fn_type_from_key(key) or { unknown_type_quoting('unknown function', key) }
			}
			if tc.unknown_import_selector(*node) {
				return builtin_void_type
			}
			base_node := tc.a.child_node(node, 0)
			if base_node.kind == .typeof_expr {
				if node.value == 'name' {
					return builtin_string_type
				}
				if node.value == 'idx' {
					return builtin_int_type
				}
				if node.value == 'indirections' {
					return builtin_u8_type
				}
			}
			if base_node.kind == .ident {
				if base_node.value == 'os' && node.value == 'args' {
					return Type(Array{
						elem_type: tc.intern_type_reference(Type(String{}))
					})
				}
				resolved := tc.resolve_import_alias(base_node.value) or { base_node.value }
				qname := '${resolved}.${node.value}'
				if qname.starts_with('C.') {
					if c_int_selector_name(node.value) {
						return builtin_int_type
					}
					if c_upper_constant_is_pointer(qname) {
						return builtin_voidptr_type
					}
					if gt := tc.c_globals[qname] {
						return gt
					}
					if node.value.len > 0 && node.value[0].is_capital() {
						return builtin_int_type
					}
				}
				if qname in tc.const_types {
					typ := tc.const_types[qname] or { unknown_type_quoting('unknown const', qname) }
					return tc.const_type_from_initializer(qname, typ)
				}
				if key := tc.const_key_for_suffix(qname) {
					typ := tc.const_types[key] or { unknown_type_quoting('unknown const', key) }
					return tc.const_type_from_initializer(key, typ)
				}
			}
			base_type := tc.resolve_type(tc.a.child(node, 0))
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
					return builtin_int_type
				}
			}
			if clean is Struct {
				if flat.source_type_syntax(clean.source_type_id()).unqualified() == checker_tail_source_struct {
					if typ := tc.inferred_anonymous_struct_field_type(tc.a.child(node, 0), node.value) {
						return typ
					}
				}
				if typ := tc.struct_field_type(clean.source_type_id(), node.value) {
					return typ
				}
				if typ := tc.method_value_type(clean.source_type_id(), node.value) {
					return typ
				}
			}
			if clean is Interface {
				if typ := tc.interface_field_type(clean.source_type_id(), node.value) {
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
				sname_id := if clean is Array {
					qualify_source_type_array
				} else if clean is Map {
					qualify_source_type_map
				} else {
					flat.source_type_string
				}
				if sname_id in tc.structs {
					for f in tc.structs[sname_id] {
						if f.name == node.value {
							return f.typ
						}
					}
				}
			}
			if clean is Primitive && base_node.kind == .selector {
				vname := base_node.value.replace('__', '.')
				if vname_id := flat.source_type_existing_id(vname) {
					if fields := tc.structs[vname_id] {
						for f in fields {
							if f.name == node.value {
								return f.typ
							}
						}
					}
				}
			}
			return unknown_type_quoting('unknown selector', node.value)
		}
		.array_literal {
			if node.typ != 0 {
				typ := tc.parse_type(node.typ)
				typ_raw := typ
				if typ is ArrayFixed {
					return *typ_raw
				}
			}
			if node.children_count > 0 {
				elem_type := tc.array_literal_elem_type(*node)
				return Type(Array{
					elem_type: tc.intern_type_reference(elem_type)
				})
			}
			return Type(Array{
				elem_type: tc.intern_type_reference(Type(int_))
			})
		}
		.postfix {
			if node.children_count == 0 {
				return unknown_type('missing postfix expression')
			}
			child_id := tc.a.child(node, 0)
			child := tc.a.nodes[int(child_id)]
			if node.op == .not && child.kind == .array_literal {
				if child.typ != 0 {
					typ := tc.parse_type(child.typ)
					typ_raw := typ
					if typ is ArrayFixed {
						return *typ_raw
					}
				}
				elem_type := if child.children_count > 0 {
					tc.array_literal_elem_type(child)
				} else {
					builtin_int_type
				}
				return Type(ArrayFixed{
					elem_type: tc.intern_type_reference(elem_type)
					len:       child.children_count
				})
			}
			return tc.resolve_type(child_id)
		}
		.index {
			return tc.resolve_index_type(*node)
		}
		.array_init {
			if node.typ != 0 {
				return *tc.parse_type(node.typ)
			}
			t := tc.parse_type(node.source_value_id())
			raw_t := t
			if t is ArrayFixed {
				return *raw_t
			}
			return Type(Array{
				elem_type: tc.intern_type_reference(t)
			})
		}
		.map_init {
			if node.source_value_id() != 0 {
				return *tc.parse_type(node.source_value_id())
			}
			if node.children_count >= 2 {
				first_id := tc.a.child(node, 0)
				first := tc.a.nodes[int(first_id)]
				if first.kind == .prefix && first.value == '...' && first.children_count > 0 {
					update_type := tc.resolve_type(tc.a.child(&first, 0))
					if map_type_from_receiver(update_type) != none {
						return update_type
					}
					for i := 2; i + 1 < node.children_count; i += 2 {
						key_id := tc.a.child(node, i)
						key := tc.a.node(key_id)
						if key.kind == .prefix && key.value == '...' {
							continue
						}
						value_id := tc.a.child(node, i + 1)
						resolved_value_type := tc.resolve_type(value_id)
						value_type := tc.mut_param_expr_base(value_id, resolved_value_type) or {
							resolved_value_type
						}
						return Type(Map{
							key_type:   tc.intern_type_reference(tc.resolve_type(key_id))
							value_type: tc.intern_type_reference(value_type)
						})
					}
					return Type(Map{
						key_type:   tc.intern_type_reference(Type(string_))
						value_type: tc.intern_type_reference(Type(int_))
					})
				}
				key_type := tc.resolve_type(first_id)
				value_id := tc.a.child(node, 1)
				resolved_value_type := tc.resolve_type(value_id)
				value_type := tc.mut_param_expr_base(value_id, resolved_value_type) or {
					resolved_value_type
				}
				return Type(Map{
					key_type:   tc.intern_type_reference(key_type)
					value_type: tc.intern_type_reference(value_type)
				})
			}
			return Type(Map{
				key_type:   tc.intern_type_reference(Type(string_))
				value_type: tc.intern_type_reference(Type(int_))
			})
		}
		.comptime_if {
			take_then := tc.comptime_type_condition_value(node.value) or {
				return unknown_type_quoting('unresolved compile-time expression condition', node.value)
			}
			branch_index := if take_then { 0 } else { 1 }
			if branch_index >= node.children_count {
				return builtin_void_type
			}
			return tc.resolve_type(tc.a.child(node, branch_index))
		}
		.if_expr {
			return tc.if_expr_tail_type(id)
		}
		.lock_expr {
			if node.children_count == 0 {
				return builtin_void_type
			}
			return tc.resolve_type(tc.a.child(node, node.children_count - 1))
		}
		.select_stmt {
			// as an expression, `select` is `false` once every channel is closed
			return builtin_bool_type
		}
		.match_stmt {
			return tc.match_expr_tail_type(id)
		}
		.return_stmt {
			return builtin_void_type
		}
		.in_expr {
			return builtin_bool_type
		}
		.block {
			if node.children_count > 0 {
				last_id := tc.a.child(node, node.children_count - 1)
				last := tc.a.nodes[int(last_id)]
				if last.kind == .expr_stmt {
					return tc.resolve_type(tc.a.child(&last, 0))
				}
				return tc.resolve_type(last_id)
			}
			return builtin_void_type
		}
		.as_expr {
			return *tc.parse_type(node.source_value_id())
		}
		.is_expr {
			return builtin_bool_type
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
	mut params := []FnParam{}
	mut reached_params := false
	mut is_variadic := false
	for i in 0 .. node.children_count {
		child := tc.a.node(tc.a.child(&node, i))
		if child.kind != .param {
			// Captures precede parameters in a function literal's flat children, so a
			// prefix-only scan must skip those identifiers, then stop once the body begins.
			if tc.prefix_param_scan && (reached_params || child.kind != .ident) {
				break
			}
			continue
		}
		reached_params = true
		is_variadic = trimmed_source_type_shape(child.typ).has(.variadic)
		parsed := tc.parse_type(normalize_fn_type_param_type(child.typ))
		resolved := if child.value.len == 0 && child.typ != 0 && parsed is Unknown {
			Type(Struct{
				name_id: child.typ
			})
		} else {
			*parsed
		}
		slot := if child.is_mut && child.op != .amp {
			mut_param_semantic_type(resolved)
		} else {
			explicit_mut_ref_param_slot_type(child, resolved)
		}
		params << FnParam{ typ: tc.intern_type_reference(slot), is_mut: child.is_mut }
	}
	return Type(FnType{
		params:      params
		is_variadic: is_variadic
		return_type: tc.fn_literal_return_type(node)
	})
}

// fn_literal_return_type parses a function literal's return type. A
// `typeof(expr)` return type is resolved in a scope with the literal's
// parameters bound, since they shadow outer bindings of the same name.
fn (tc &TypeChecker) fn_literal_return_type(node flat.Node) &Type {
	if !inferred_type_source(node.typ) {
		return tc.parse_type(node.typ)
	}
	// Plain bindings: this only names the parameters' types, so it records no
	// diagnostics and leaves the function context untouched.
	mut writer := unsafe { &TypeChecker(voidptr(tc)) }
	writer.push_scope()
	for i in 0 .. node.children_count {
		child := tc.a.child_node(&node, i)
		if child.kind == .param && child.value.len > 0 {
			parsed := tc.parse_scope_param_type(child.typ)
			writer.cur_scope.insert(child.value, mut_param_binding_type(parsed, child.is_mut,
				child.op == .amp))
		}
	}
	typ := tc.parse_type(node.typ)
	writer.pop_scope()
	return typ
}

// lambda_expr_type supports lambda expr type handling for TypeChecker.
fn (tc &TypeChecker) lambda_expr_type(node flat.Node) Type {
	mut params := []FnParam{}
	param_count := int(node.children_count) - 1
	for i in 0 .. param_count {
		param := tc.a.child_node(&node, i)
		typ := if param.typ != 0 {
			tc.parse_type(normalize_fn_type_param_type(param.typ))
		} else {
			tc.intern_type_reference(unknown_type('lambda parameter'))
		}
		params << FnParam{ typ: typ, is_mut: param.is_mut }
	}
	ret_type := if node.children_count > 0 {
		tc.resolve_type(tc.a.child(&node, node.children_count - 1))
	} else {
		builtin_void_type
	}
	return Type(FnType{ params: params, return_type: tc.intern_type_reference(ret_type) })
}

// resolve_index_type resolves resolve index type information for types.
fn (tc &TypeChecker) resolve_index_type(node flat.Node) Type {
	if generic_fn_type := tc.explicit_generic_fn_value_type(node) {
		return generic_fn_type
	}
	base_id := tc.a.child(&node, 0)
	base_type0 := tc.resolve_type(base_id)
	if node.value == 'range' {
		if sliced_alias := range_slice_alias_type(base_type0) {
			return sliced_alias
		}
	}
	mut base_type := unalias_type(base_type0)
	if base_type is Pointer {
		// A `mut` binding (`for mut word in words`) is a pointer to the value it names,
		// and indexing it indexes that value: `word[0]` is a byte of the string.
		if mut_base := tc.mut_param_expr_base(base_id, base_type0) {
			if unalias_type(mut_base) !is Pointer {
				base_type = unalias_type(mut_base)
			}
		}
	}
	if base_type is OptionType {
		inner := unalias_type(base_type.base_type)
		result := tc.resolve_index_base_type(inner, node)
		if result is Unknown {
			return result
		}
		return Type(OptionType{
			base_type: tc.intern_type_reference(result)
		})
	}
	return tc.resolve_index_base_type(base_type, node)
}

// check_generic_method_value returns the type of the generic method that the
// index `node` names on a value with its type arguments and without a call,
// `h.first[int]`: the method of those types without its receiver, which the
// value binds, as `first[int]` is the function of those types. Its type
// arguments are checked as those of a call are (see resolve_generic_call_info):
// their count, those its receiver fixes, and their constraints.
fn (mut tc TypeChecker) check_generic_method_value(id flat.NodeId, node flat.Node) ?Type {
	selector := tc.a.child_node(&node, 0)
	if selector.children_count == 0 || tc.generic_call_type_arg_names(node).len == 0 {
		return none
	}
	// Before the dot, a module or a type names a function, not a value: a local,
	// a constant or a global is a value.
	receiver := tc.a.child_node(selector, 0)
	if receiver.kind == .ident && !tc.ident_resolves_to_value(receiver.value)
		&& (receiver.value in tc.imports || tc.type_symbol_known(receiver.source_value_id())) {
		return none
	}
	info := tc.resolve_generic_call_info(id, node) or { return none }
	if !info.has_receiver || info.params.len == 0
		|| (tc.fn_generic_params[info.name] or { []string{} }).len == 0 {
		return none
	}
	tc.check_generic_method_value_constraints(id, node, info.name)
	// The generic method it names, for markused and the monomorphization.
	tc.remember_resolved_call(id, info.name)
	// The receiver is bound, so its mutability is dropped with it.
	mutability := tc.declaration_param_mutability_for(info.name)
	return Type(tc.variadic_fn_type(info.params[1..], tc.intern_type_reference(info.return_type), if mutability.len > 0 {
		mutability[1..]
	} else {
		mutability
	}, info.is_variadic))
}

// check_generic_method_value_constraints checks the type arguments of the
// generic method value `node` (see check_generic_method_value) against the
// constraints of the type parameters of the method `name`, as
// check_generic_fn_value_constraints checks those of a function value.
fn (mut tc TypeChecker) check_generic_method_value_constraints(id flat.NodeId, node flat.Node, name string) {
	decl := tc.top_level_fn_decl(name) or { return }
	params := decl.generic_params()
	constraints := decl.generic_constraint_ids()
	type_args := tc.generic_call_type_arg_names(node)
	if constraints.len != params.len || type_args.len != params.len {
		return
	}
	concrete := type_args.map(tc.explicit_generic_concrete_arg_text(it))
	for i, text_id in constraints {
		if text_id == 0 {
			continue
		}
		constraint := tc.generic_constraint_with_args(decl, text_id, params, concrete) or { continue }
		actual := tc.parse_type(concrete[i])
		if tc.generic_constraint_accepts(constraint, actual) {
			continue
		}
		tc.record_generic_constraint_error(constraint, params[i], actual, id, tc.explicit_type_arg_pos(node,
			i) or { node.pos })
	}
}

// top_level_fn_decl returns the declaration of the function or method `name`.
fn (tc &TypeChecker) top_level_fn_decl(name string) ?flat.Node {
	short := name.all_after_last('.')
	for idx in tc.top_level_idx {
		node := tc.a.nodes[idx]
		if node.kind == .fn_decl && node.value.all_after_last('.') == short
			&& (node.value == name || name.ends_with('.${node.value}')) {
			return node
		}
	}
	return none
}

// explicit_generic_fn_value_type resolves `generic_fn[ConcreteType]` when the
// specialization is used as a function value rather than called immediately.
fn (tc &TypeChecker) explicit_generic_fn_value_type(node flat.Node) ?Type {
	if node.kind != .index || node.children_count < 2 || node.value == 'range' {
		return none
	}
	base_node := tc.a.node(tc.a.child(&node, 0))
	if base_node.kind == .ident && tc.ident_resolves_to_value(base_node.value) {
		return none
	}
	name := tc.generic_call_base_name(base_node) or { return none }
	type_args_source_ids := tc.generic_call_type_arg_names(node).clone()

	if type_args_source_ids.len == 0 {
		return none
	}
	info := tc.explicit_generic_call_info(name, false, type_args_source_ids) or { return none }
	return Type(tc.variadic_fn_type(info.params, tc.intern_type_reference(info.return_type), tc.declaration_param_mutability_for(info.name), info.is_variadic))
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
		if info := tc.index_overload_call_info(base_type, false) {
			return info.return_type
		}
		if base_type is Array {
			return base_type
		}
		if base_type is ArrayFixed {
			return Type(Array{
				elem_type: tc.intern_type_reference(fixed_array_elem_type(base_type))
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
					elem_type: tc.intern_type_reference(fixed_array_elem_type(inner))
				})
			}
		}
		return builtin_string_type
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
		return builtin_u8_type
	}
	if info := tc.index_overload_call_info(base_type, false) {
		return info.return_type
	}
	return unknown_type_quoting('cannot index', base_type.name())
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
fn (tc &TypeChecker) c_extern_abi_type(t &Type) string {
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
		mut params := []string{}
		for i in 0 .. t.params.len {
			mut param_type := fn_param_type(t, i)
			if fn_param_is_mut(t, i) && param_type !is Pointer {
				param_type = Type(Pointer{
					base_type: tc.intern_type_reference(param_type)
				})
			}
			params << tc.c_extern_abi_type(param_type)
		}
		return naming.fn_ptr_encoded(ret, params)
	}
	return tc.c_type(t)
}

// c_type returns the C representation of a semantic type.
pub fn (tc &TypeChecker) c_type(t &Type) string {
	// These representations are constant or depend only on primitive flags.
	// Interning a collection recursively walks its element types even though
	// every dynamic array/map/channel uses the same C container.
	has_simple_c_type := match t {
		Primitive, Void, Unknown, Nil, None, String, Char, Rune, ISize, USize, Array, Channel, Map {
			true
		}
		else { false }
	}
	if has_simple_c_type {
		return tc.c_type_uncached(t)
	}
	if tc.type_cache == unsafe { nil } || isnil(tc.type_interner) {
		return tc.c_type_uncached(t)
	}
	// Key on the interned semantic identity, not t.name(): distinct semantic types
	// can share a source spelling (`[size]int` with different resolved `size`, or
	// same-named aliases over different resolved bases) yet fold to different C
	// representations, so a textual key would hand back the wrong layout.
	id, canonical := tc.intern_type(t)
	return tc.canonical_c_type(id, canonical)
}

fn (tc &TypeChecker) cached_c_type_by_id(id TypeId) ?string {
	if isnil(tc.type_cache) {
		return none
	}
	mut cache := unsafe { tc.type_cache }
	slot := int(u32(id) & u32(type_cache_recent_slots - 1))
	if tc.fast_c_type_recent {
		if cache.c_recent_set[slot] && cache.c_recent_ids[slot] == id {
			cache.c_hits++
			return flat.source_type_name(cache.c_recent_vals[slot])
		}
	}
	if cached := cache.c_entries[id] {
		cache.c_hits++
		if tc.fast_c_type_recent {
			cache.c_recent_ids[slot] = id
			cache.c_recent_vals[slot] = cached
			cache.c_recent_set[slot] = true
		}
		return flat.source_type_name(cached)
	}
	return none
}

fn (tc &TypeChecker) canonical_c_type(id TypeId, canonical Type) string {
	if cached := tc.cached_c_type_by_id(id) {
		return cached
	}
	if isnil(tc.type_cache) {
		return tc.c_type_uncached(canonical)
	}
	mut cache := unsafe { tc.type_cache }
	cache.c_misses++
	result := tc.c_type_uncached(canonical)
	result_id := flat.source_type_id(result)
	cache.c_entries[id] = result_id
	if tc.fast_c_type_recent {
		slot := int(u32(id) & u32(type_cache_recent_slots - 1))
		cache.c_recent_ids[slot] = id
		cache.c_recent_vals[slot] = result_id
		cache.c_recent_set[slot] = true
	}
	return result
}

// c_type_uncached supports c type uncached handling for TypeChecker.
fn (tc &TypeChecker) c_type_uncached(t &Type) string {
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
		return '__v_option'
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
		if t.base_type is OptionType || t.base_type is ResultType {
			// `&?T`, the type of a `mut a ?T` parameter. Spelled as cgen spells it
			// (`optional_type_name`), so that fn types and declarations agree.
			return tc.optional_c_type_name(t.base_type) + '*'
		}
		return tc.c_type(t.base_type) + '*'
	}
	if t is FnType {
		ret := tc.fn_ptr_return_c_type(t.return_type)
		mut params := []string{}
		for i in 0 .. t.params.len {
			mut param_type := fn_param_type(t, i)
			if fn_param_is_mut(t, i) && param_type !is Pointer {
				param_type = Type(Pointer{
					base_type: tc.intern_type_reference(param_type)
				})
			}
			if param_type is OptionType {
				params << tc.optional_c_type_name(param_type)
			} else if param_type is ResultType {
				params << tc.optional_c_type_name(param_type)
			} else {
				params << tc.c_type(param_type)
			}
		}
		return naming.fn_ptr_encoded(ret, params)
	}
	if t is OptionType {
		return '__v_option'
	}
	if t is ResultType {
		return '__v_result'
	}
	if t is Struct {
		name_id := t.source_type_id()
		shape := flat.source_type_shape(name_id)
		query := flat.source_type_query(name_id)
		if name_id == checker_source_thread || shape.has(.thread_type) || query.has(.thread_suffix) {
			return '__v_thread'
		}
		name := flat.source_type_name(name_id)
		if query.has(.c_prefix) {
			raw := name[2..]
			// A struct declared `@[typedef] struct C.foo {}` is referenced by its
			// typedef name (`foo`), never as `struct foo` — the C header (and v3's own
			// emitted `typedef struct {...} foo;`) has no matching `struct foo` tag, so
			// a `struct foo` reference would stay an incomplete type.
			if name_id in tc.c_typedef_structs {
				return raw
			}
			// Winsock exposes WSAData only as a struct tag; its typedef is WSADATA.
			if raw == 'WSAData' || raw.starts_with('_') {
				return 'struct ${raw}'
			}
			if raw.starts_with('builtin__closure__') {
				closure_name := 'closure.${raw['builtin__closure__'.len..]}'
				if closure_id := flat.source_type_existing_id(closure_name) {
					if closure_id in tc.structs {
						return tc.c_struct_type_name(closure_id)
					}
				}
			}
			if raw.ends_with('_s')
				|| (raw.len > 0 && raw[0] >= `a` && raw[0] <= `z` && !raw.ends_with('_t')) {
				return 'struct ${raw}'
			}
			return raw
		}
		if shape.has(.bracketed) {
			syntax := flat.source_type_syntax(name_id)
			if syntax.application_valid() {
				base := syntax.application_base()
				if !flat.source_type_shape(syntax.application_base()).has(.dotted)
					&& tc.type_aliases.contains(base) {
					if module_name := tc.struct_modules[base] {
						if module_name !in ['', 'main', 'builtin'] {
							return tc.c_struct_type_name(flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), name_id))
						}
					}
				}
			}
		}
		if shape.has(.dotted) && name_id !in tc.structs && name_id !in tc.type_aliases.ids {
			// An import-alias prefix (`json.Any` for `import x.json2 as json`)
			// can survive in recorded types; resolve by unique short name.
			if resolved_source_id := tc.unique_qualified_type_name(flat.source_type_syntax(name_id).unqualified()) {
				return tc.c_struct_type_name(resolved_source_id)
			}
		}
		return tc.c_struct_type_name(name_id)
	}
	if t is Interface {
		syntax := flat.source_type_syntax(t.source_type_id())
		if syntax.application_valid() {
			// Generic interface applications all use the base interface's runtime
			// box. Their type arguments specialize method signatures, but do not
			// introduce a distinct C struct.
			return naming.c_name(flat.source_type_name(syntax.application_base()))
		}
		return naming.c_name(t.name())
	}
	if t is Enum {
		return 'int'
	}
	if t is SumType {
		return naming.c_name(t.name())
	}
	if t is Alias {
		alias_id := t.source_type_id()
		if tc.autofree_mode && t.source_type_id() in tc.type_alias_modules
			&& tc.type_alias_modules[t.source_type_id()] in ['', 'main']
			&& !flat.source_type_shape(alias_id).has(.dotted)
			&& flat.source_type_syntax(alias_id).first_capital() {
			return naming.c_name('main.${t.name()}')
		}
		if t.base_type is Unknown && t.base_type.reason.starts_with('recursive alias `') {
			if target_source_id := tc.type_aliases.type_id(t.source_type_id()) {
				target_shape := trimmed_source_type_shape(target_source_id)
				if target_shape.has(.map_type) {
					return 'map'
				}
				if target_shape.has(.array_type) || target_shape.has(.variadic) {
					return 'Array'
				}
				if target_shape.has(.channel_type) {
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

fn (tc &TypeChecker) c_struct_type_name(name_id u32) string {
	syntax := flat.source_type_syntax(name_id)
	if !syntax.application_valid() {
		mut qualified_id := name_id
		if !flat.source_type_shape(name_id).has(.dotted) {
			if module_name := tc.struct_modules[name_id] {
				if module_name in ['', 'main'] {
					qualified_id = flat.source_type_composition_two(.qualified, checker_source_main_module, name_id)
				}
			}
		}
		cname := naming.c_name(flat.source_type_name(qualified_id))
		if tc.struct_c_name_collides_with_v3_runtime(name_id, cname) {
			return '_v_${cname}'
		}
		return cname
	}
	mut qualified_base := syntax.application_base()
	if !flat.source_type_shape(qualified_base).has(.dotted) {
		if module_name := tc.struct_modules[qualified_base] {
			if module_name in ['', 'main'] {
				qualified_base = flat.source_type_composition_two(.qualified, checker_source_main_module, qualified_base)
			}
		}
	}
	mut parts := [qualified_base]
	for arg in syntax.application_args() {
		parts << tc.c_generic_struct_arg_name(arg)
	}
	return naming.c_name(flat.source_type_name(flat.source_type_composition(.generic, parts)))
}

fn (tc &TypeChecker) struct_c_name_collides_with_v3_runtime(name_id u32, cname string) bool {
	if cname !in export_v3_reserved_c_symbols || flat.source_type_query(name_id).has(.c_prefix) {
		return false
	}
	if module_name := tc.struct_modules[name_id] {
		return module_name != 'builtin'
	}
	return false
}

fn (tc &TypeChecker) c_generic_struct_arg_name(arg_id u32) u32 {
	clean_id := trimmed_source_type_id(arg_id)
	// `voidptr` round-trips through the type model as `&void`. Keep the source ABI
	// spelling for generic instance names so materialized structs and methods agree.
	if clean_id == checker_source_void_reference {
		return flat.source_type_voidptr
	}
	if fixed := tc.c_generic_struct_fixed_array_arg_name(clean_id) {
		return flat.source_type_id(fixed)
	}
	if target_id := tc.type_aliases.type_id(clean_id) {
		if fixed := tc.c_generic_struct_fixed_array_arg_name(target_id) {
			return flat.source_type_id(fixed)
		}
	}
	if flat.source_type_shape(clean_id).has(.dotted) {
		short_id := flat.source_type_syntax(clean_id).unqualified()
		if clean_id !in tc.structs && clean_id !in tc.struct_generic_params && clean_id !in tc.interface_names && !tc.sum_types.contains(clean_id) && clean_id !in tc.enum_names && clean_id !in tc.flag_enums && !tc.type_aliases.contains(clean_id) {
			if short_id in tc.structs || short_id in tc.struct_generic_params
				|| short_id in tc.interface_names || tc.sum_types.contains(short_id) || short_id in tc.enum_names
				|| short_id in tc.flag_enums || tc.type_aliases.contains(short_id) {
				return short_id
			}
		}
	}
	return clean_id
}

fn (tc &TypeChecker) c_generic_struct_fixed_array_arg_name(arg_id u32) ?string {
	clean_id := trimmed_source_type_id(arg_id)
	if !flat.source_type_shape(clean_id).has(.leading_bracket) {
		return none
	}
	typ := tc.parse_type(clean_id)
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

fn (tc &TypeChecker) fixed_array_elem_c_type(t &Type) string {
	if t is OptionType {
		return tc.optional_c_type_name(t)
	}
	if t is ResultType {
		return tc.optional_c_type_name(t)
	}
	if t is Pointer && t.base_type is Void {
		return 'voidptr'
	}
	return tc.c_type(t)
}

fn (tc &TypeChecker) fn_ptr_return_c_type(t &Type) string {
	if t is Void {
		return 'void'
	}
	if t is OptionType {
		return tc.optional_c_type_name(t)
	}
	if t is ResultType {
		return tc.optional_c_type_name(t)
	}
	return tc.c_type(t)
}

fn (tc &TypeChecker) optional_c_type_name(typ &Type) string {
	base_type := match typ {
		OptionType, ResultType { *typ.base_type }
		else { return tc.c_type(typ) }
	}
	prefix := if typ is ResultType { '__v_result' } else { '__v_option' }
	if base_type is Void {
		return prefix
	}
	mut inner_ct := tc.c_type(base_type)
	if inner_ct.starts_with('fn_ptr:') {
		inner_ct = naming.fn_ptr_type_name(inner_ct)
	}
	if inner_ct == 'int' {
		return prefix
	}
	return '${prefix}_${inner_ct.replace('*', 'ptr').replace(' ', '_')}'
}

// resolve_type_name_for_method resolves resolve type name for method information for types.
struct GenericReceiverMethodPatternMatch {
	key         string
	params      []string
	args        flat.TypeTextList
	specificity int
	is_exact    bool
}

// resolve_generic_struct_method resolves a method call on a generic-struct
// instance (e.g. `Vec4[f32].r_sqrt`). The method is registered against the
// generic form (`Vec4[T].r_sqrt`); this maps the instance's concrete type
// arguments onto the generic parameters and substitutes them into the method's
// signature, so the pre-transform checker accepts the call. The transformer's
// monomorphize pass later materialises the concrete method body.
pub fn (tc &TypeChecker) resolve_generic_struct_method(type_name_id u32, method string) ?CallInfo {
	if tc.struct_generic_params.len == 0 && tc.type_alias_generic_params.len == 0
		&& tc.generic_receiver_method_index.len == 0 {
		return none
	}
	lookup_id := tc.generic_struct_method_alias_target(type_name_id)
	syntax := flat.source_type_syntax(lookup_id)
	has_type_args := syntax.receiver_app_valid()
	if flat.source_type_shape(lookup_id).has(.bracketed) && syntax.generic_base() != 0 && !has_type_args {
		return none
	}
	base_id := if has_type_args { syntax.receiver_app_base() } else { lookup_id }
	mut concrete_args := []u32{}
	if has_type_args {
		for arg in syntax.receiver_app_args() {
			// Keep caller-local concrete arguments anchored to the caller. Method
			// signature text is parsed in the declaring module below, where a bare
			// `Context` would otherwise become `veb.Context` instead of
			// `main.Context` for an embedded `veb.Middleware[Context]`.
			concrete_args << tc.explicit_generic_concrete_arg_text(trimmed_source_type_id(arg))
		}
	}
	mut generic_base := u32(0)
	mut params := []string{}
	mut generic_key := ''
	if has_type_args {
		if matched := tc.generic_receiver_method_pattern_match(base_id, concrete_args, method) {
			if matched.is_exact {
				return tc.call_info(matched.key, true)
			}
			if matched.args.ids.any(tc.generic_receiver_arg_is_voidptr(it)) {
				return none
			}
			generic_base = base_id
			params = matched.params.clone()
			concrete_args = matched.args.ids.clone()
			generic_key = matched.key
		}
	}
	if generic_key.len == 0 {
		for candidate_id in tc.generic_struct_method_base_candidates(base_id) {
			candidate := flat.source_type_name(candidate_id)
			candidate_params := tc.struct_generic_params[candidate_id] or {
				tc.type_alias_generic_params[candidate_id] or { continue }
			}
			candidate_concrete_args := if has_type_args {
				concrete_args.clone()
			} else {
				flat.TypeTextList.from_names(candidate_params).ids
			}
			if candidate_params.len == 0 || candidate_params.len != candidate_concrete_args.len {
				continue
			}
			mut method_bases := [candidate]
			module_name := tc.struct_modules[candidate_id] or {
				tc.type_alias_modules[candidate_id] or { '' }
			}
			if module_name !in ['', 'main', 'builtin'] && !flat.source_type_shape(candidate_id).has(.dotted) {
				method_bases << '${module_name}.${candidate}'
			}
			for method_base in method_bases {
				for method_spelling in [method, '@${method}'] {
					candidate_key := '${method_base}[${candidate_params.join(', ')}].${method_spelling}'
					if candidate_key in tc.fn_ret_types {
						generic_base = candidate_id
						params = generic_method_receiver_params_from_key(candidate_key, candidate_params)
						concrete_args = candidate_concrete_args.clone()
						generic_key = candidate_key
						break
					}
					plain_candidate_key := '${method_base}.${method_spelling}'
					if plain_candidate_key in tc.fn_ret_types {
						generic_base = candidate_id
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
							mut indexed_params := generic_method_receiver_params_from_key(indexed, candidate_params)
							mut indexed_args := candidate_concrete_args.clone()
							if has_type_args {
								indexed_params, indexed_args = tc.generic_method_receiver_pattern_args(indexed, candidate_concrete_args) or { continue }
								if indexed_args.any(tc.generic_receiver_arg_is_voidptr(it)) {
									continue
								}
							}
							generic_base = candidate_id
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
	if generic_base == 0 {
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
	} else if ret_text_source_id := tc.fn_ret_type_texts.type_id(generic_key) {
		sub_ret = tc.parse_fn_signature_type(generic_key, subst_generic_text(ret_text_source_id, concrete_args, params))
	}
	mut sub_params := []Type{}
	if param_texts_source_ids := tc.fn_param_type_texts.type_ids(generic_key) {
		param_types := tc.fn_param_types[generic_key] or { []Type{} }
		for i, pt_source_id in param_texts_source_ids {
			receiver_clean := trimmed_source_type_id(flat.source_type_syntax(trimmed_source_type_id(pt_source_id)).pointer_raw_base())
			receiver_base := flat.source_type_syntax(receiver_clean).generic_base()
			if i == 0 && (receiver_base == generic_base
				|| receiver_base == flat.source_type_syntax(generic_base).unqualified()
				|| receiver_base == base_id
				|| receiver_base == flat.source_type_syntax(base_id).unqualified()) {
				sub_params << tc.generic_method_receiver_param(type_name_id, pt_source_id)
				continue
			}
			if i < param_types.len && generic_semantic_type_has_placeholder(param_types[i]) {
				sub_params << tc.substitute_generic_param_type(param_types[i], pt_source_id, concrete_types,
					params)
				continue
			}
			sub_params << tc.parse_fn_signature_type(generic_key, subst_generic_signature_param_type(pt_source_id, concrete_args, params))
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

fn (tc &TypeChecker) generic_struct_method_alias_target(type_name_id u32) u32 {
	mut current := type_name_id
	// The chain is at most 16 deep; retain its existing source identities directly.
	mut seen := [16]u32{}
	mut seen_len := 0
	for _ in 0 .. 16 {
		mut cycle := false
		for i in 0 .. seen_len {
			if seen[i] == current {
				cycle = true
				break
			}
		}
		if cycle {
			break
		}
		seen[seen_len] = current
		seen_len++
		qualified := tc.qualify_type_name(current)
		target := tc.type_aliases.type_id(current) or {
			tc.type_aliases.type_id(qualified) or { break }
		}
		if target == current {
			break
		}
		current = target
	}
	return current
}

@[inline]
fn write_receiver_pattern_cache_u32(mut key strings.Builder, value u32) {
	key.write_u8(u8(value >> 24))
	key.write_u8(u8((value >> 16) & 0xff))
	key.write_u8(u8((value >> 8) & 0xff))
	key.write_u8(u8(value & 0xff))
}

// generic_receiver_pattern_cache_key retains context names and encodes ordered source IDs.
fn (tc &TypeChecker) generic_receiver_pattern_cache_key(base_id u32, actual_args []u32, method string) string {
	prefix := '${tc.cur_file}\x00${tc.cur_module}\x00${tc.fn_context.generic_params.join(',')}\x00'
	mut key := strings.new_builder(prefix.len + actual_args.len * 4 + method.len + 8)
	key.write_string(prefix)
	write_receiver_pattern_cache_u32(mut key, base_id)
	write_receiver_pattern_cache_u32(mut key, u32(actual_args.len))
	for arg in actual_args {
		write_receiver_pattern_cache_u32(mut key, arg)
	}
	key.write_u8(0)
	key.write_string(method)
	return key.str()
}

fn (tc &TypeChecker) generic_receiver_method_pattern_match(base_id u32, actual_args_ids []u32, method string) ?GenericReceiverMethodPatternMatch {
	cache_key := tc.generic_receiver_pattern_cache_key(base_id, actual_args_ids, method)
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
			pattern_syntax := flat.source_type_syntax(flat.source_type_id(receiver))
			patterns := pattern_syntax.application_args()
			if !pattern_syntax.application_valid() || patterns.len != actual_args_ids.len
				|| !tc.generic_type_base_matches(pattern_syntax.application_base(), base_id) {
				continue
			}
			mut is_exact := true
			for i, pattern in patterns {
				if tc.parse_type(trimmed_source_type_id(pattern)).source_type_id() != tc.parse_type(trimmed_source_type_id(actual_args_ids[i])).source_type_id() {
					is_exact = false
					break
				}
			}
			mut params := []string{}
			mut inferred_args := []u32{}
			mut specificity := 1_000_000
			if !is_exact {
				if patterns.all(flat.source_type_syntax(trimmed_source_type_id(it)).bare_generic()) {
					continue
				}
				params, inferred_args = tc.generic_method_receiver_pattern_args(key, actual_args_ids) or { continue }
				specificity = 0
				for pattern in patterns {
					if !flat.source_type_syntax(trimmed_source_type_id(pattern)).bare_generic() {
						specificity += 10_000 + flat.source_type_syntax(pattern).byte_len()
					}
				}
			}
			if !found || specificity > best.specificity
				|| (specificity == best.specificity && key < best.key) {
				best = GenericReceiverMethodPatternMatch{
					key:         key
					params:      params
					args:        flat.TypeTextList{ ids: inferred_args }
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

fn (tc &TypeChecker) generic_method_receiver_pattern_args(key string, actual_args_ids []u32) ?([]string, []u32) {
	receiver := key.all_before_last('.')
	syntax := flat.source_type_syntax(flat.source_type_id(receiver))
	patterns := syntax.application_args()
	if !syntax.application_valid() || patterns.len != actual_args_ids.len { return none }
	mut counts := map[string]int{}
	for pattern in patterns {
		tc.collect_generic_param_candidates(pattern, mut counts)
	}
	if counts.len == 0 { return none }
	mut params := counts.keys()
	params.sort()
	mut inferred := map[string]u32{}
	for i, pattern in patterns {
		tc.infer_generic_type_text_from_text(pattern, actual_args_ids[i], params, mut inferred)
	}
	mut concrete_args := []u32{cap: params.len}
	for param in params { concrete_args << inferred[param] or { return none } }
	for i, pattern in patterns {
		substituted_id := subst_generic_text(pattern, concrete_args, params)
		if tc.parse_type(substituted_id).source_type_id() != tc.parse_type(actual_args_ids[i]).source_type_id() {
			return none
		}
	}
	return params, concrete_args
}

fn (tc &TypeChecker) generic_receiver_arg_is_voidptr(arg_id u32) bool {
	clean := trimmed_source_type_id(arg_id)
	if clean == flat.source_type_voidptr { return true }
	return tc.parse_type(clean).source_type_id() in [flat.source_type_voidptr,
		checker_source_void_reference]
}

fn (tc &TypeChecker) generic_receiver_has_structured_method_pattern(type_name_id u32, method string) bool {
	syntax := flat.source_type_syntax(type_name_id)
	if !syntax.application_valid() {
		return false
	}
	candidates := tc.generic_receiver_method_index[method] or { return false }
	for key in candidates {
		receiver := key.all_before_last('.')
		pattern_syntax := flat.source_type_syntax(flat.source_type_id(receiver))
		if !pattern_syntax.application_valid() || !tc.generic_type_base_matches(pattern_syntax.application_base(), syntax.application_base()) {
			continue
		}
		if pattern_syntax.application_args().any(!flat.source_type_syntax(trimmed_source_type_id(it)).bare_generic()) {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) generic_receiver_method_rejects_voidptr(type_name_id u32, method string) bool {
	syntax := flat.source_type_syntax(type_name_id)
	if !syntax.application_valid() {
		return false
	}
	candidates := tc.generic_receiver_method_index[method] or { return false }
	for key in candidates {
		receiver := key.all_before_last('.')
		pattern_syntax := flat.source_type_syntax(flat.source_type_id(receiver))
		if !pattern_syntax.application_valid() || !tc.generic_type_base_matches(pattern_syntax.application_base(), syntax.application_base()) {
			continue
		}
		_, inferred := tc.generic_method_receiver_pattern_args(key, syntax.application_args()) or { continue }
		if inferred.any(tc.generic_receiver_arg_is_voidptr(it)) {
			return true
		}
	}
	return false
}

fn generic_method_receiver_params_from_key(key string, fallback []string) []string {
	receiver := key.all_before_last('.')
	syntax := flat.source_type_syntax(flat.source_type_id(receiver))
	if !syntax.receiver_app_valid() {
		return fallback.clone()
	}
	mut params := []string{}
	for param in syntax.receiver_app_args() {
		params << flat.source_type_name(trimmed_source_type_id(param))
	}
	if params.len != fallback.len {
		return fallback.clone()
	}
	return params
}

fn generic_semantic_type_has_placeholder(typ &Type) bool {
	match typ {
		Unknown {
			return typ.placeholder != 0
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
			for parameter in typ.params {
				param := parameter.typ
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

fn (tc &TypeChecker) generic_struct_method_base_candidates(base_id u32) []u32 {
	mut candidates := []u32{}
	push_generic_method_base_candidate(mut candidates, base_id)
	if flat.source_type_shape(base_id).has(.dotted) {
		resolved := tc.resolve_imported_type_text(base_id)
		push_generic_method_base_candidate(mut candidates, resolved)
		push_generic_method_base_candidate(mut candidates, flat.source_type_syntax(resolved).unqualified())
		push_generic_method_base_candidate(mut candidates, flat.source_type_syntax(base_id).unqualified())
	} else {
		if resolved_id := tc.resolve_selective_import_type_symbol_id(base_id) {
			push_generic_method_base_candidate(mut candidates, resolved_id)
			push_generic_method_base_candidate(mut candidates, flat.source_type_syntax(resolved_id).unqualified())
		}
		push_generic_method_base_candidate(mut candidates, tc.qualify_type_name(base_id))
	}
	return candidates
}

fn push_generic_method_base_candidate(mut candidates []u32, candidate u32) {
	if candidate != 0 && candidate !in candidates {
		candidates << candidate
	}
}

// resolve_generic_sum_method looks up a method for a sum identified by its source spelling ID.
pub fn (tc &TypeChecker) resolve_generic_sum_method(type_name_id u32, method string) ?CallInfo {
	syntax := flat.source_type_syntax(type_name_id)
	base_id := if syntax.application_valid() { syntax.application_base() } else { type_name_id }
	mut concrete_args := []u32{}
	if syntax.application_valid() {
		for arg in syntax.application_args() {
			concrete_args << trimmed_source_type_id(arg)
		}
	}
	params := tc.sum_params_for_base(base_id)
	if params.len == 0 {
		return none
	}
	if !syntax.application_valid() {
		concrete_args = flat.TypeTextList.from_names(params).ids
	}
	if params.len != concrete_args.len {
		return none
	}
	mut generic_key := ''
	base := flat.source_type_name(base_id)
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
	if ret_text_source_id := tc.fn_ret_type_texts.type_id(generic_key) {
		sub_ret = tc.parse_fn_signature_type(generic_key, subst_generic_text(ret_text_source_id, concrete_args, params))
	}
	mut sub_params := []Type{}
	if param_texts_source_ids := tc.fn_param_type_texts.type_ids(generic_key) {
		for pt_source_id in param_texts_source_ids {
			sub_params << tc.parse_fn_signature_type(generic_key, subst_generic_signature_param_type(pt_source_id, concrete_args, params))
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

fn (tc &TypeChecker) generic_method_receiver_param(type_name_id u32, param_type_id u32) Type {
	receiver_type := tc.parse_type(type_name_id)
	if trimmed_source_type_shape(param_type_id).has(.pointer) {
		return Type(Pointer{
			base_type: tc.intern_type_reference(receiver_type)
		})
	}
	return *receiver_type
}

// subst_generic_text textually substitutes the generic parameter names `params` with the
// source spelling IDs `args` inside `type_id`, preserving the generic application
// form so a method signature mentioning the receiver type (`Box[T]`) becomes the concrete
// instance (`Box[int]`) when re-parsed, instead of collapsing to the bare base. Prefix and
// container forms (`&`, `mut`, `?`, `!`, `...`, `shared`, `atomic`, `chan`, `thread`, `[]`,
// `map[`, `[N]`) recurse into the element type; a bare parameter name is replaced with its
// argument.
pub fn subst_generic_text(type_id u32, args []u32, params []string) u32 {
	clean := trimmed_source_type_id(type_id)
	if clean == 0 || args.len == 0 || params.len != args.len {
		return clean
	}
	shape := flat.source_type_shape(clean)
	if shape.has(.pointer) {
		return flat.source_type_composition_one(.pointer, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	if shape.has(.mutable_type) {
		return flat.source_type_composition_one(.mutable_parameter, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	if shape.has(.option_type) || shape.has(.result_type) {
		inner := subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params)
		inner_shape := flat.source_type_shape(inner)
		if shape.has(.option_type) {
			return if inner_shape.has(.option_type) {
				inner
			} else {
				flat.source_type_composition_one(.option, inner)
			}
		}
		return if inner_shape.has(.result_type) {
			inner
		} else {
			flat.source_type_composition_one(.result, inner)
		}
	}
	if shape.has(.variadic) {
		return flat.source_type_composition_one(.variadic_parameter, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	if shape.has(.shared_type) {
		return flat.source_type_composition_one(.shared_type, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	if shape.has(.atomic_type) {
		return flat.source_type_composition_one(.atomic_type, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	if shape.has(.channel_type) {
		return flat.source_type_composition_one(.channel, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	if shape.has(.thread_type) {
		return flat.source_type_composition_one(.thread, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	if shape.has(.array_type) {
		return flat.source_type_composition_one(.array, subst_generic_text(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	syntax := flat.source_type_syntax(clean)
	if syntax.tuple_like() && syntax.contains_comma() {
		mut parts := []u32{cap: syntax.checker_tuple_components().len}
		for part in syntax.checker_tuple_components() {
			parts << subst_generic_text(part, args, params)
		}
		return flat.source_type_composition(.multi_return, parts)
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		return flat.source_type_composition_two(.map,
			subst_generic_text(syntax.bracket_raw_inner(), args, params),
			subst_generic_text(syntax.bracket_suffix(), args, params))
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix(),
			subst_generic_text(syntax.bracket_suffix(), args, params))
	}
	if (shape.has(.function_compact) || shape.has(.function_spaced)) && syntax.function_closed() {
		mut parts := []u32{cap: syntax.checker_function_params().len + 1}
		for part in syntax.checker_function_params() {
			parts << subst_generic_text(flat.source_type_syntax(part).parameter_mut_type(), args, params)
		}
		if syntax.function_result() != 0 {
			parts << subst_generic_text(syntax.function_result(), args, params)
			return flat.source_type_composition(.function_type, parts)
		}
		return flat.source_type_composition(.void_function_type, parts)
	}
	if shape.has(.bracketed) && syntax.generic_base() != 0 && syntax.bracket_closed() {
		mut parts := []u32{cap: syntax.checker_bracket_components().len + 1}
		parts << syntax.generic_base()
		for part in syntax.checker_bracket_components() {
			parts << subst_generic_text(part, args, params)
		}
		return flat.source_type_composition_two(.concat,
			flat.source_type_composition(.generic, parts), syntax.bracket_suffix())
	}
	for i, param in params {
		if clean == flat.source_type_id(param) {
			return args[i]
		}
	}
	return clean
}

// subst_generic_signature_param_type substitutes a signature parameter's spelling ID.
fn subst_generic_signature_param_type(type_id u32, args []u32, params []string) u32 {
	clean := trimmed_source_type_id(type_id)
	if clean == 0 {
		return clean
	}
	if flat.source_type_shape(clean).has(.mutable_type) {
		return flat.source_type_composition_one(.mutable_parameter,
			subst_generic_signature_param_type(flat.source_type_syntax(clean).raw_base(), args, params))
	}
	return subst_generic_text(flat.source_type_syntax(clean).generic_parameter_payload(), args, params)
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

fn resolve_type_name_for_method(t &Type) u32 {
	if t is Alias {
		return t.source_type_id()
	}
	if t is Struct {
		return t.source_type_id()
	}
	if t is Interface {
		return t.source_type_id()
	}
	if t is SumType {
		return t.source_type_id()
	}
	if t is Enum {
		return t.source_type_id()
	}
	if t is String {
		return flat.source_type_string
	}
	if t is Char {
		return flat.source_type_char
	}
	if t is Rune {
		return flat.source_type_rune
	}
	if t is ISize {
		return flat.source_type_isize
	}
	if t is USize {
		return flat.source_type_uint
	}
	if t is Array {
		return t.source_type_id()
	}
	if t is ArrayFixed {
		return flat.source_type_fixed_array(t.len, t.len_expr, t.elem_type.source_type_id(), true)
	}
	if t is Map {
		return t.source_type_id()
	}
	if t is Primitive {
		return t.source_type_id()
	}
	return u32(0)
}

// ownership_type_has_clone_method reports whether typ declares a compatible handwritten clone method.
// It is kept in the always-built checker surface because ownership transform support is
// compiled into the V executable even when the executable itself is built without ownership.
pub fn (tc &TypeChecker) ownership_type_has_clone_method(typ &Type) bool {
	receiver_type := unwrap_pointer(typ)
	name := resolve_type_name_for_method(receiver_type)
	if name == 0 {
		return false
	}
	if info := tc.resolve_generic_struct_method(name, 'clone') {
		if tc.ownership_clone_method_matches_type(info, typ) {
			return true
		}
	}
	for method_name in receiver_method_name_candidates(receiver_type, 'clone', tc.cur_module) {
		if method_name in tc.fn_ret_types {
			if tc.ownership_clone_method_matches_type(tc.call_info(method_name, true), typ) {
				return true
			}
		}
	}
	return false
}

fn (tc &TypeChecker) ownership_clone_method_matches_type(info CallInfo, typ &Type) bool {
	return info.params_known && tc.min_required_arg_count(info) == 1
		&& semantic_types_equal(unalias_type(info.return_type), unalias_type(typ))
}

fn receiver_type_name_variant(t &Type, fixed_array_prefix bool, shorten_modules bool) u32 {
	if t is Alias {
		return receiver_leaf_type_name(t.source_type_id(), shorten_modules)
	}
	if t is Struct {
		return receiver_leaf_type_name(t.source_type_id(), shorten_modules)
	}
	if t is Interface {
		return receiver_leaf_type_name(t.source_type_id(), shorten_modules)
	}
	if t is SumType {
		return receiver_leaf_type_name(t.source_type_id(), shorten_modules)
	}
	if t is Enum {
		return receiver_leaf_type_name(t.source_type_id(), shorten_modules)
	}
	if t is String {
		return flat.source_type_string
	}
	if t is Array {
		return flat.source_type_composition_one(.array, receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules))
	}
	if t is ArrayFixed {
		elem := receiver_type_name_variant(t.elem_type, fixed_array_prefix, shorten_modules)
		return flat.source_type_fixed_array(t.len, t.len_expr, elem, !fixed_array_prefix)
	}
	if t is Map {
		key := receiver_type_name_variant(t.key_type, fixed_array_prefix, shorten_modules)
		value := receiver_type_name_variant(t.value_type, fixed_array_prefix, shorten_modules)
		return flat.source_type_composition_two(.map, key, value)
	}
	if t is Primitive {
		return prim_c_type_id_from(t.props, t.size)
	}
	return receiver_leaf_type_name(t.source_type_id(), shorten_modules)
}

// prim_c_type_id_from is the spelling ID of prim_c_type_from: the V spelling of
// booleans and integers, and `float`/`double` for floats.
fn prim_c_type_id_from(props Properties, size u8) u32 {
	if props.has(.float) && !props.has(.boolean) && !props.has(.integer) {
		return if size == 32 { checker_source_float } else { checker_source_double }
	}
	return primitive_source_type_id(props, size)
}

fn receiver_leaf_type_name(name u32, shorten_modules bool) u32 {
	if shorten_modules && flat.source_type_shape(name).has(.dotted) {
		return flat.source_type_syntax(name).unqualified()
	}
	return name
}

fn receiver_type_name_variants(t Type) []string {
	mut names := []string{}
	push_receiver_method_candidate(mut names, flat.source_type_name(receiver_type_name_variant(t, false, false)))
	push_receiver_method_candidate(mut names, flat.source_type_name(receiver_type_name_variant(t, false, true)))
	push_receiver_method_candidate(mut names, flat.source_type_name(receiver_type_name_variant(t, true, false)))
	push_receiver_method_candidate(mut names, flat.source_type_name(receiver_type_name_variant(t, true, true)))
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
		name := t.source_type_id()
		if flat.source_type_shape(name).has(.dotted) {
			push_receiver_method_candidate(mut names, flat.source_type_name(flat.source_type_syntax(name).qualifier()))
		}
	}
	return names
}

fn push_receiver_method_candidate(mut names []string, name string) {
	if name != '' && name !in names {
		names << keep_string(name)
	}
}

fn (tc &TypeChecker) unique_receiver_method_suffix_match(receiver &Type, candidates []string) ?string {
	mut found := ''
	for candidate in candidates {
		name := tc.receiver_method_suffix_index[candidate] or { continue }
		if name == receiver_method_suffix_ambiguous {
			return none
		}
		if !tc.suffix_indexed_method_fits_receiver(receiver, name) {
			continue
		}
		if found != '' && found != name {
			return none
		}
		found = name
	}
	if found.len == 0 {
		return none
	}
	return found
}

// suffix_indexed_method_fits_receiver reports whether `indexed`, a method found
// through the short-name `receiver_method_suffix_index`, can belong to `receiver`.
// The index drops module prefixes, so a `[]toml.Any` receiver also reaches
// `json2.[]Any.str`; a method of a same-named type from another module must not
// bind to it.
pub fn (tc &TypeChecker) suffix_indexed_method_fits_receiver(receiver &Type, indexed string) bool {
	owner := tc.receiver_owner_module(receiver) or { return true }
	return owner == type_owner_module(indexed.all_before_last('.'))
}

// receiver_owner_module returns the module declaring the struct, sum type or enum
// whose methods `receiver` uses (the element or value type of arrays and maps).
// Aliases, interfaces and structs with embedded fields can inherit methods that
// are declared in other modules, so they report none.
fn (tc &TypeChecker) receiver_owner_module(receiver &Type) ?string {
	mut t := receiver
	for {
		if t is Pointer {
			t = t.base_type
		} else if t is Array {
			t = t.elem_type
		} else if t is ArrayFixed {
			t = t.elem_type
		} else if t is Map {
			t = t.value_type
		} else {
			break
		}
	}
	if t is Struct {
		if tc.struct_fields_for_type(t.source_type_id()).any(it.is_embed) {
			return none
		}
		return type_owner_module_id(t.source_type_id())
	}
	if t is SumType {
		return type_owner_module_id(t.source_type_id())
	}
	if t is Enum {
		return type_owner_module_id(t.source_type_id())
	}
	return none
}

// type_owner_module returns the module part of a type or method receiver name
// (`json2.Any`, `json2.[]Any`, `json2.Box[T]` -> `json2`). Main and builtin
// declarations are not module-qualified, so they all map to ''.
fn type_owner_module(name string) string {
	return type_owner_module_id(flat.source_type_id(name))
}

fn type_owner_module_id(name u32) string {
	head := flat.source_type_syntax(name).generic_base()
	if !flat.source_type_shape(head).has(.dotted) {
		return ''
	}
	module_name := flat.source_type_syntax(flat.source_type_syntax(head).qualifier()).unqualified()
	return if module_name in [checker_source_main_module, checker_source_builtin] {
		''
	} else {
		flat.source_type_name(module_name)
	}
}

fn module_can_prefix_collection_receiver(module_name string) bool {
	return module_name != '' && module_name != 'main' && module_name != 'builtin'
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
	mut clean := t
	if clean is Alias {
		clean = clean.base_type
	}
	type_name := resolve_type_name_for_method(t)
	if clean !is Array && clean !is Map {
		// The common receiver: its own method, plain or with an escaped name.
		if type_name == 0 {
			return []string{}
		}
		type_text := flat.source_type_name(type_name)
		return ['${type_text}.${method}', '${type_text}.@${method}']
	}
	mut names := []string{}
	if type_name != 0 {
		push_receiver_method_candidate(mut names, '${flat.source_type_name(type_name)}.${method}')
		push_receiver_method_candidate(mut names, '${flat.source_type_name(type_name)}.@${method}')
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

// c_struct_operator_call_info resolves a visible C receiver operator in its source context.
pub fn (tc &TypeChecker) c_struct_operator_call_info(receiver Type, method string, file string, module_name string) ?CallInfo {
	underlying := unalias_and_unwrap_pointer_type(receiver)
	if underlying !is Struct || !flat.source_type_query(underlying.source_type_id()).has(.c_prefix) {
		return none
	}
	view := if tc.cur_file == file && tc.cur_module == module_name {
		tc
	} else {
		tc.fork_type_parse_view(file, module_name)
	}
	name, ambiguous := view.lookup_c_struct_receiver_method(receiver, method)
	if ambiguous || name.len == 0 {
		return none
	}
	if _ := view.private_declaration(name) { return none }
	info := view.call_info(name, true)
	if info.params.len == 0 || !view.method_receiver_compatible(receiver, info.params[0], name) {
		return none
	}
	return info
}

fn (tc &TypeChecker) c_struct_infix_operator_call_info(op flat.Op, lhs Type) ?CallInfo {
	op_name := infix_operator_name(op) or { return none }
	if info := tc.c_struct_operator_call_info(lhs, op_name, tc.cur_file, tc.cur_module) {
		return info
	}
	fallback := match op {
		.gt, .ge, .le { '<' }
		.ne { '==' }
		else { return none }
	}
	return tc.c_struct_operator_call_info(lhs, fallback, tc.cur_file, tc.cur_module)
}

fn (tc &TypeChecker) infix_operator_signature(op flat.Op, lhs Type) ?InfixOperatorSignature {
	op_name := infix_operator_name(op) or { return none }
	underlying := unalias_and_unwrap_pointer_type(lhs)
	if underlying is Struct && flat.source_type_query(underlying.source_type_id()).has(.c_prefix) {
		info := tc.c_struct_infix_operator_call_info(op, lhs) or { return none }
		return InfixOperatorSignature{
			return_type: tc.intern_type_reference(info.return_type)
			param_type:  if info.params.len > 1 { info.params[1] } else { Type(void_) }
			param_count: info.params.len
		}
	}
	if lhs is Pointer && lhs.base_type !is Alias && unalias_type(lhs.base_type) !is Struct {
		return none
	}
	mut receiver_types := [unwrap_pointer(lhs), Type(void_), Type(void_)]!
	mut receiver_count := 1
	if receiver_types[0] is Alias {
		alias_type := receiver_types[0] as Alias
		if target_source_id := tc.alias_target_type_text(alias_type.source_type_id()) {
			target_source_id_2 := target_source_id

			receiver_types[receiver_count] = tc.parse_type(target_source_id_2)
			receiver_count++
		}
		receiver_types[receiver_count] = alias_type.base_type
		receiver_count++
	}
	for receiver_index in 0 .. receiver_count {
		receiver_type := receiver_types[receiver_index]
		lhs_name := resolve_type_name_for_method(receiver_type)
		if lhs_name == 0 {
			continue
		}
		is_concrete_generic := flat.source_type_syntax(lhs_name).application_valid()
		if is_concrete_generic {
			if info := tc.resolve_generic_struct_method(lhs_name, op_name) {
				if info.params.len > 0 && (tc.receiver_compatible(lhs, info.params[0])
					|| tc.receiver_compatible(receiver_type, info.params[0])) {
					return InfixOperatorSignature{
						return_type: tc.intern_type_reference(info.return_type)
						param_type:  if info.params.len > 1 { info.params[1] } else { Type(void_) }
						param_count: info.params.len
					}
				}
			}
		}
		method_name := '${flat.source_type_name(lhs_name)}.${op_name}'
		if ret := tc.fn_ret_types[method_name] {
			if params := tc.fn_param_types[method_name] {
				if params.len > 0 && (tc.receiver_compatible(lhs, params[0])
					|| tc.receiver_compatible(receiver_type, params[0])) {
					return InfixOperatorSignature{
						return_type: ret
						param_type:  if params.len > 1 { params[1] } else { builtin_void_type }
						param_count: params.len
					}
				}
			}
		}
		if !is_concrete_generic {
			// Primitive receivers retain any exact declared operator above. Only
			// named generic structs/aliases can supply the remaining fallback.
			if receiver_type is Primitive && is_builtin_type_name_id(lhs_name)
				&& !tc.primitive_infix_has_generic_receiver_candidate(lhs_name) {
				continue
			}
			info := tc.resolve_generic_struct_method(lhs_name, op_name) or { continue }
			if info.params.len > 0 && (tc.receiver_compatible(lhs, info.params[0])
				|| tc.receiver_compatible(receiver_type, info.params[0])) {
				return InfixOperatorSignature{
					return_type: tc.intern_type_reference(info.return_type)
					param_type:  if info.params.len > 1 { info.params[1] } else { Type(void_) }
					param_count: info.params.len
				}
			}
		}
	}
	return none
}

// Builtin primitive names stay unqualified in qualify_name, including the
// alias redirect lookup. Preserve raw alias metadata and both selected-import
// owner spellings before skipping generic_struct_method_base_candidates.
fn (tc &TypeChecker) primitive_infix_has_generic_receiver_candidate(name_id u32) bool {
	if name_id in tc.struct_generic_params || name_id in tc.type_alias_generic_params
		|| tc.type_aliases.contains(name_id) {
		return true
	}
	if imported_id := tc.resolve_selective_import_type_symbol_id(name_id) {
		if imported_id in tc.struct_generic_params || imported_id in tc.type_alias_generic_params {
			return true
		}
		short_id := scope_unqualified_type_name(imported_id)
		if short_id in tc.struct_generic_params || short_id in tc.type_alias_generic_params {
			return true
		}
	}
	return false
}

fn (tc &TypeChecker) infix_operator_return_type(op flat.Op, lhs Type, rhs Type) ?Type {
	signature := tc.infix_operator_signature(op, lhs) or { return none }
	if signature.param_count < 2 || !tc.infix_operator_operand_compatible(rhs, lhs) {
		return none
	}
	return signature.return_type
}

fn (tc &TypeChecker) infix_operator_operand_compatible(actual &Type, expected &Type) bool {
	if actual.source_type_id() == expected.source_type_id() {
		return true
	}
	// Distinct aliases and structs do not become interchangeable just because
	// their storage or fields match. Untyped primitive literals can still be
	// promoted to the operator's declared alias type.
	if actual is Alias {
		if expected is Alias {
			return false
		}
		return tc.type_compatible(actual.base_type, expected)
	}
	clean_actual := unalias_type(unwrap_pointer(actual))
	clean_expected := unalias_type(unwrap_pointer(expected))
	if clean_actual is Struct && clean_expected is Struct
		&& clean_actual.name_id != clean_expected.name_id {
		return false
	}
	return tc.type_compatible(actual, expected)
}

fn (tc &TypeChecker) type_has_infix_operator_method(typ &Type, op flat.Op) bool {
	op_name := infix_operator_name(op) or { return false }
	underlying := unalias_and_unwrap_pointer_type(typ)
	if underlying is Struct && flat.source_type_query(underlying.source_type_id()).has(.c_prefix) {
		return tc.c_struct_infix_operator_call_info(op, typ) != none
	}
	type_name := resolve_type_name_for_method(unwrap_pointer(typ))
	if type_name == 0 {
		return false
	}
	if _ := tc.resolve_generic_struct_method(type_name, op_name) {
		return true
	}
	method_name := '${flat.source_type_name(type_name)}.${op_name}'
	return method_name in tc.fn_ret_types
}

fn (tc &TypeChecker) int_literal_promoted_infix_type(lit_id flat.NodeId, other_id flat.NodeId, lit_type Type, other_type Type) ?Type {
	if tc.int_literal_value(other_id) != none {
		return none
	}
	clean_type := unalias_type(other_type)
	// Reject ineligible storage before recursively evaluating the constant.
	is_float := clean_type.is_float()
	if !is_float && (!clean_type.is_integer() || tc.type_name(clean_type) == flat.source_type_int) {
		return none
	}
	value := tc.implicit_integer_constant_value(lit_id, lit_type)?
	// Untyped integer constants also adopt a float operand, including aliases.
	if is_float {
		return other_type
	}
	if clean_type is Primitive && clean_type.props.has(.integer)
		&& clean_type.size in [u8(8), 16] {
		if small_integer_type_accepts_int_literal(clean_type, value) {
			return clean_type
		}
		return builtin_int_type
	}
	// An untyped integer literal adopts the other integer operand's concrete storage
	// type. In particular, `24 * time.hour` is `i64`, not `int`; map literal
	// inference relies on that distinction when it chooses its value type.
	return *clean_type
}

fn infix_integer_promotion_type(lhs Type, rhs Type) ?Type {
	if lhs.source_type_id() == rhs.source_type_id() {
		return lhs
	}
	clean_lhs := unalias_type(lhs)
	clean_rhs := unalias_type(rhs)
	if !call_arg_integer_type(clean_lhs) || !call_arg_integer_type(clean_rhs) {
		return none
	}
	if call_arg_implicit_numeric_widening(clean_lhs, clean_rhs) {
		return *clean_rhs
	}
	if call_arg_implicit_numeric_widening(clean_rhs, clean_lhs) {
		return *clean_lhs
	}
	return none
}

fn (tc &TypeChecker) implicit_integer_constant_value(id flat.NodeId, typ &Type) ?int {
	if unalias_type(typ) is USize {
		node := tc.a.node(id)
		if node.kind == .ident && !tc.ident_is_mutable_lvalue(node.value) {
			rhs_id := tc.local_decl_rhs_before(node.value, id) or { return none }
			rhs := tc.a.node(rhs_id)
			if rhs.kind in [.sizeof_expr, .offsetof_expr] {
				return tc.const_int_expr(rhs_id, tc.cur_module, []string{})
			}
		}
	}
	// Composite spellings cannot be `int`; avoid constructing them for ordinary
	// non-integer call arguments. Nominal spellings retain their exact identity.
	is_int := match typ {
		Primitive { primitive_source_type_id(typ.props, typ.size) == flat.source_type_int }
		Struct, Alias, Interface, Enum, SumType { typ.name_id == flat.source_type_int }
		else { false }
	}
	if !is_int {
		return none
	}
	return tc.const_int_expr(id, tc.cur_module, []string{})
}

// ident_is_integer_literal_range_var preserves the reference checker's untyped integer
// semantics for `for value in 0 .. limit`: the iteration variable can adopt an integer
// parameter type at a call boundary even though its storage is emitted as `int`.
fn (tc &TypeChecker) ident_is_integer_literal_range_var(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	ident := tc.a.node(id)
	if ident.kind != .ident {
		return false
	}
	mut current := id
	for _ in 0 .. 128 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) {
			return false
		}
		parent := tc.a.node(parent_id)
		if parent.kind == .fn_decl {
			return false
		}
		if parent.kind == .for_in_stmt && parent.children_count >= 3 {
			key := tc.a.child_node(parent, 0)
			if key.kind == .ident && key.value == ident.value {
				header := parent.value.int()
				mut low_id := flat.empty_node
				if header == 4 && parent.children_count >= 4 {
					low_id = tc.a.child(parent, 2)
				} else if header == 3 {
					container := tc.a.child_node(parent, 2)
					if container.kind == .range && container.children_count >= 2 {
						low_id = tc.a.child(container, 0)
					}
				}
				return tc.integer_literal_source(low_id) != none
			}
		}
		current = parent_id
	}
	return false
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

fn small_integer_type_accepts_int_literal(t Primitive, value int) bool {
	if t.size !in [u8(8), 16] || !t.props.has(.integer) {
		return false
	}
	if t.props.has(.unsigned) {
		max := if t.size == 8 { 255 } else { 65535 }
		return value >= 0 && value <= max
	}
	max := if t.size == 8 { 127 } else { 32767 }
	return value >= -max - 1 && value <= max
}

fn type_is_f32(t &Type) bool {
	if t is Primitive {
		return t.props.has(.float) && t.size == 32
	}
	return false
}

fn infix_operator_name(op flat.Op) ?string {
	match op {
		.plus {
			return '+'
		}
		.minus {
			return '-'
		}
		.mul {
			return '*'
		}
		.power {
			return '**'
		}
		.div {
			return '/'
		}
		.mod {
			return '%'
		}
		.amp {
			return '&'
		}
		.pipe {
			return '|'
		}
		.xor {
			return '^'
		}
		.left_shift {
			return '<<'
		}
		.right_shift {
			return '>>'
		}
		.right_shift_unsigned {
			return '>>>'
		}
		.eq {
			return '=='
		}
		.ne {
			return '!='
		}
		.lt {
			return '<'
		}
		.gt {
			return '>'
		}
		.le {
			return '<='
		}
		.ge {
			return '>='
		}
		.logical_and {
			return '&&'
		}
		.logical_or {
			return '||'
		}
		.arrow {
			return '<-'
		}
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

// explicit_mut_ref_param_slot_type applies the mutable caller slot that a param
// node's own type text still lacks. The parser folds mut x T into &T but
// records mut x &T as &T with op == .amp. Only the explicit pointer form
// still needs the slot here.
fn explicit_mut_ref_param_slot_type(param flat.Node, typ Type) Type {
	if !param.is_mut || param.op != .amp || typ !is Pointer {
		return typ
	}
	return Type(Pointer{
		base_type: new_type_reference(Type(typ))
	})
}

fn normalize_fn_type_param_type(type_id u32) u32 {
	return flat.source_type_syntax(type_id).parameter_type()
}

// normalize_fn_type_param_text transforms normalize fn type param text data for types.
fn normalize_fn_type_param_text(param string) string {
	return normalize_fn_type_param_text_with_mut_mode(param, false)
}

fn normalize_fn_type_param_text_with_mut_mode(param string, preserve_mut bool) string {
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
	if is_mut && text.len > 0 {
		if preserve_mut {
			return 'mut ' + text
		}
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
	if head == '' || tail == '' {
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

fn (tc &TypeChecker) failure_has_error(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .decl_assign && node.children_count > 1 {
		return tc.failure_has_error(tc.a.child(node, 1))
	}
	if node.children_count > 0
		&& node.kind in [.match_stmt, .paren, .expr_stmt, .or_expr] {
		return tc.failure_has_error(tc.a.child(node, 0))
	}
	if node.kind == .prefix && node.op == .arrow {
		return true
	}
	if node.kind == .call {
		name := tc.resolved_call_name(id) or { '' }
		if typ := tc.fn_ret_types[name] {
			return unalias_type(typ) is ResultType
		}
		if typ := tc.direct_call_return_type(*node) {
			return unalias_type(typ) is ResultType
		}
	}
	if node.kind != .infix {
		return unalias_type(tc.resolve_type(id)) is ResultType
	}
	mut found := false
	for child in tc.a.children_of(node) {
		if !tc.or_expr_source_can_fail(child) {
			continue
		}
		if !tc.failure_has_error(child) {
			return false
		}
		found = true
	}
	return found
}

// args_text resolves concrete type arguments at a text integration boundary.
fn (value &GenericReceiverMethodPatternMatch) args_text() []string {
	return value.args.names()
}
