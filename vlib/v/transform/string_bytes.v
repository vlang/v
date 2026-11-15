module transform

import v.flat

// Only the builtin conversion is removable; aliases may define their own bytes method.
fn (t &Transformer) builtin_string_bytes_source(id flat.NodeId) ?flat.NodeId {
	call_id := t.unwrap_parens(id)
	call := t.a.node(call_id)
	if call.kind != .call || call.children_count != 1 {
		return none
	}
	method := t.a.child_node(call, 0)
	if method.kind != .selector || method.value != 'bytes' || method.children_count != 1 {
		return none
	}
	source := t.a.child(method, 0)
	if t.node_type(source) != flat.source_type_string
		|| !t.call_resolved_to_method(call_id, 'string.bytes') {
		return none
	}
	return source
}

fn (mut t Transformer) lower_literal_bytes_membership(base_id flat.NodeId, needle_id flat.NodeId) ?flat.NodeId {
	source := t.builtin_string_bytes_source(base_id) or { return none }
	literal := t.a.nodes[int(t.unwrap_parens(source))]
	if literal.kind != .string_literal {
		return none
	}
	// Snapshot even an empty set's needle: its side effects still have to run once.
	needle := t.snapshot_transformed_expr_for_reuse(t.transform_expr_for_type(needle_id,
		flat.source_type_u8), flat.source_type_u8, 'byte_needle')
	if literal.value.len > 16 {
		// Bound generated code size for large sets. contains_u8 scans the literal directly.
		t.mark_fn_used('string.contains_u8')
		return t.make_call_typed('string__contains_u8', [
			t.make_string_literal(literal.value),
			needle,
		], flat.source_type_bool)
	}
	mut seen := [256]bool{}
	mut result := t.make_bool_literal(false)
	for byte in literal.value {
		if seen[byte] {
			continue
		}
		seen[byte] = true
		value := t.make_int_literal(int(byte))
		t.set_node_typ(int(value), flat.source_type_u8)
		comparison := t.make_infix(.eq, needle, value)
		result = t.make_infix(.logical_or, result, comparison)
	}
	return result
}

fn (t &Transformer) bytes_loop_has_unsafe(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.node(id)
	if node.kind == .block && node.value == 'unsafe' {
		return true
	}
	for child in t.a.children_of(node) {
		if t.bytes_loop_has_unsafe(child) {
			return true
		}
	}
	return false
}

// A string parameter cannot refer to this invocation's local scalar storage. Keep
// copying for other sources: a local string may be a view into a mutable buffer.
fn (t &Transformer) bytes_loop_has_immutable_parameter(loop_id flat.NodeId, source flat.NodeId) bool {
	name := t.a.node(t.unwrap_parens(source))
	if name.kind != .ident || t.mut_param_values[name.value] {
		return false
	}
	mut parent := int(loop_id)
	for parent >= 0 && parent < t.source_parent_ids.len {
		parent = t.source_parent_ids[parent]
		if parent < 0 {
			break
		}
		node := t.a.node(flat.NodeId(parent))
		if node.kind in [.fn_literal, .lambda_expr] {
			return false
		}
		if node.kind != .fn_decl {
			continue
		}
		if t.tc.declaration_has_attribute(flat.NodeId(parent), 'unsafe') {
			return false
		}
		for child in t.a.children_of(node) {
			param := t.a.node(child)
			if param.kind == .param && param.value == name.value && !param.is_mut
				&& param.typ == flat.source_type_string {
				return !t.bytes_loop_has_unsafe(flat.NodeId(parent))
			}
		}
		return false
	}
	return false
}

fn (t &Transformer) bytes_loop_scalar(typ u32) bool {
	return typ == flat.source_type_bool || is_numeric_type_name(typ)
}

// Unknown calls and indirect writes can change storage aliased by the source string.
// Admit only scalar computations and writes to ordinary local value bindings.
fn (t &Transformer) bytes_loop_body_is_readonly(id flat.NodeId) bool {
	if int(id) < 0 {
		return true
	}
	node := t.a.node(id)
	match node.kind {
		.empty, .int_literal, .float_literal, .bool_literal, .char_literal, .string_literal,
		.break_stmt, .continue_stmt {
		}
		.ident {
			return t.bytes_loop_scalar(t.node_type(id))
		}
		.block {
			if node.value == 'unsafe' { return false }
		}
		.expr_stmt, .paren, .if_expr, .return_stmt {}
		.infix, .prefix, .cast_expr {
			if !t.bytes_loop_scalar(t.node_type(id)) || node.op == .amp { return false }
		}
		.call {
			callee := t.a.child_node(node, 0)
			if node.children_count != 2 || callee.kind != .ident
				|| !is_numeric_type_name(flat.source_type_id(callee.value)) {
				return false
			}
			return t.bytes_loop_body_is_readonly(t.a.child(node, 1))
		}
		.assign, .postfix {
			lhs := t.a.child_node(node, 0)
			if lhs.kind != .ident || t.var_type(lhs.value) == 0
				|| !t.bytes_loop_scalar(t.var_type(lhs.value)) || t.mut_param_values[lhs.value]
				|| t.pointer_value_lvalues[lhs.value] || t.heaped_amp_locals[lhs.value] {
				return false
			}
		}
		else { return false }
	}
	for child in t.a.children_of(node) {
		if !t.bytes_loop_body_is_readonly(child) { return false }
	}
	return true
}

fn (mut t Transformer) bytes_iteration_source(loop_id flat.NodeId, node flat.Node, container_id flat.NodeId, body_ids []flat.NodeId) ?flat.NodeId {
	if node.op == .amp {
		return none
	}
	source := t.builtin_string_bytes_source(container_id) or { return none }
	literal := t.a.node(t.unwrap_parens(source)).kind == .string_literal
	if !literal {
		// Ownership/autofree must also prove that a borrowed header survives all drops.
		$if ownership ? {
			return none
		}
		if isnil(t.tc) || t.tc.autofree_enabled() || t.tc.translated_files[t.cur_file]
			|| !t.bytes_loop_has_immutable_parameter(loop_id, source) {
			return none
		}
		for child in body_ids {
			if !t.bytes_loop_body_is_readonly(child) { return none }
		}
	}
	return t.snapshot_transformed_expr_for_reuse(t.transform_expr_for_type(source,
		flat.source_type_string), flat.source_type_string, 'byte_source')
}
