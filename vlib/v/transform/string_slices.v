module transform

import v.flat

// Only immediate consumers may borrow. Deferred/spawned calls and ownership
// cleanup need separate lifetime proofs, so retain the ordinary owning slice.
fn (t &Transformer) immediate_string_slice_context(id flat.NodeId) bool {
	$if ownership ? {
		return false
	}
	if isnil(t.tc) || t.tc.autofree_enabled() || t.tc.is_js_backend || t.in_spawn_expr {
		return false
	}
	mut parent := int(id)
	for parent >= 0 && parent < t.source_parent_ids.len {
		parent = t.source_parent_ids[parent]
		if parent < 0 { break }
		node := t.a.node(flat.NodeId(parent))
		if node.kind in [.defer_stmt, .spawn_expr] { return false }
		if node.kind in [.fn_decl, .fn_literal, .lambda_expr] { return true }
	}
	return false
}

fn (t &Transformer) immediate_string_slice(id flat.NodeId) ?flat.Node {
	node := t.a.nodes[int(t.unwrap_parens(id))]
	if node.kind != .index || node.value != 'range' || node.op == .gated_index
		|| node.children_count < 2 || node.children_count > 3 {
		return none
	}
	// Alias operators and custom indexing retain their own lowering.
	if t.node_type(t.a.child(&node, 0)) != flat.source_type_string {
		return none
	}
	return node
}

fn (mut t Transformer) snapshot_string_slice_operand(id flat.NodeId, typ u32, prefix string) flat.NodeId {
	// An unsafe/value block must not capture an earlier operand's temporaries.
	outer := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.transform_expr_for_type(id, typ)
	pending := t.pending_stmts.clone()
	t.pending_stmts = outer
	t.pending_stmts << pending
	return t.snapshot_transformed_expr_for_reuse(value, typ, prefix)
}

fn (mut t Transformer) lower_immediate_string_slice(node flat.Node) flat.NodeId {
	// Snapshot the header, start and end in source order, before checking bounds.
	source := t.snapshot_string_slice_operand(t.a.child(&node, 0), flat.source_type_string,
		'slice_source')
	start_id := t.a.child(&node, 1)
	start := if t.a.node(start_id).kind == .empty {
		t.make_int_literal(0)
	} else {
		t.snapshot_string_slice_operand(start_id, flat.source_type_int, 'slice_start')
	}
	end := if node.children_count == 3 {
		t.snapshot_string_slice_operand(t.a.child(&node, 2), flat.source_type_int, 'slice_end')
	} else {
		t.make_selector(source, 'len', flat.source_type_int)
	}
	t.mark_fn_used('string.substr_borrowed')
	return t.make_call_typed('string__substr_borrowed', [source, start, end], flat.source_type_string)
}

fn (mut t Transformer) lower_immediate_string_slice_comparison(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.op !in [.eq, .ne] || node.children_count != 2 {
		return none
	}
	lhs := t.a.child(&node, 0)
	rhs := t.a.child(&node, 1)
	lhs_literal := t.a.nodes[int(t.unwrap_parens(lhs))].kind == .string_literal
	rhs_literal := t.a.nodes[int(t.unwrap_parens(rhs))].kind == .string_literal
	if !lhs_literal && !rhs_literal { return none }
	range_id := if lhs_literal { rhs } else { lhs }
	range := t.immediate_string_slice(range_id) or { return none }
	if !t.immediate_string_slice_context(id) { return none }
	// A literal has no effects and cannot invalidate the view before string.eq
	// consumes it. Arbitrary comparison operands keep the snapshot allocation.
	view := t.lower_immediate_string_slice(range)
	// This lowering bypasses transform_expr(range_id), so keep the borrowed value
	// available to the assert reporter without evaluating the slice again.
	if t.assert_watch.active {
		for i, watched in t.assert_watch.ids {
			if watched == int(range_id) {
				t.assert_watch.results[i] = int(view)
			}
		}
	}
	literal := t.transform_expr(if lhs_literal { lhs } else { rhs })
	equal := t.make_call_typed('string__eq', [view, literal], flat.source_type_bool)
	return if node.op == .ne { t.make_prefix(.not, equal) } else { equal }
}

fn (mut t Transformer) lower_immediate_string_slice_write(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count != 2 { return none }
	callee := t.a.nodes[int(t.a.child(&node, 0))]
	if callee.kind != .selector || callee.value != 'write_string' || callee.children_count != 1 {
		return none
	}
	range := t.immediate_string_slice(t.a.child(&node, 1)) or { return none }
	if !t.immediate_string_slice_context(id)
		|| !t.call_resolved_to_method(id, 'strings.Builder.write_string')
		|| !t.is_known_fn_name('strings.Builder.write_string_borrowed') {
		return none
	}
	receiver_id := t.a.child(&callee, 0)
	if t.normalize_type_alias(t.trim_pointer_type(t.node_type(receiver_id))) != flat.source_type_id('[]u8') {
		return none
	}
	// Capture the receiver's address before evaluating the source and bounds.
	// The private consumer copies overlapping storage before changing the builder.
	pointer_type := flat.source_type_id('&strings.Builder')
	receiver := t.transform_call_arg_for_param(receiver_id, pointer_type)
	stable_receiver := t.snapshot_transformed_expr_for_reuse(receiver, pointer_type,
		'slice_builder')
	view := t.lower_immediate_string_slice(range)
	return t.make_receiver_method_call_typed(node, 'strings.Builder.write_string_borrowed',
		[stable_receiver, view], flat.source_type_void)
}
