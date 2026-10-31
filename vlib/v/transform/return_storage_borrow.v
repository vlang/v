module transform

import v.flat
import v.types

// EscapeCallStorageBorrow holds a call's already-proven storage-only summary.
// The borrowed slice belongs to the transformer's immutable summary table.
struct EscapeCallStorageBorrow {
	summary []bool
	offset  int
}

fn (b EscapeCallStorageBorrow) borrows(child_index int) bool {
	if child_index == 0 && b.offset != 1 {
		return false
	}
	index := if child_index == 0 { 0 } else { child_index - 1 + b.offset }
	return index >= 0 && index < b.summary.len && b.summary[index]
}

// escape_call_storage_borrow uses the existing storage-only proof for an
// exactly aligned direct call. It does not prove that the value's payload is
// unrelated to the result: aggregate copies can carry pointers elsewhere.
fn (t &Transformer) escape_call_storage_borrow(id flat.NodeId, call flat.Node) EscapeCallStorageBorrow {
	if isnil(t.tc) || call.children_count == 0 {
		return EscapeCallStorageBorrow{}
	}
	callee := t.a.child_node(&call, 0)
	// Wrapped generic/function-value callees need an independent identity proof.
	if callee.kind !in [.ident, .selector] {
		return EscapeCallStorageBorrow{}
	}
	name := t.call_name_for_node(id, call)
	decl := t.fixed_array_call_param_ref(name, call, 0) or { return EscapeCallStorageBorrow{} }
	summary := t.fixed_array_borrow_params[decl.decl] or { return EscapeCallStorageBorrow{} }
	params := t.call_param_types_decl_cache[decl.decl] or { return EscapeCallStorageBorrow{} }
	offset := t.call_param_offset(name, call, params)
	// Optional/context arguments and named or spread forms need their own ABI
	// alignment proof. Leave those calls conservative.
	if call.children_count - 1 != params.len - offset {
		return EscapeCallStorageBorrow{}
	}
	for i in 1 .. call.children_count {
		arg := t.a.node(t.a.child(&call, i))
		if arg.kind == .field_init || t.call_arg_is_spread(t.a.child(&call, i)) {
			return EscapeCallStorageBorrow{}
		}
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base_id := t.a.child(callee, 0)
		base := types.unalias_type(types.unwrap_all_pointers(t.tc.resolve_type(base_id)))
		if base is types.Interface {
			return EscapeCallStorageBorrow{}
		}
		base_type := t.normalize_type_alias(t.trim_pointer_type(t.lvalue_type(base_id)))
		if t.receiver_selector_is_fn_field(base_type, callee.value) {
			return EscapeCallStorageBorrow{}
		}
	}
	return EscapeCallStorageBorrow{
		summary: summary
		offset:  offset
	}
}

// escape_storage_borrow_ident restricts the new storage projection to locals
// whose complete recorded payload provenance is available by name. Dereferences
// and scalar fields/indexes need their original conservative address provenance.
fn (t &Transformer) escape_storage_borrow_ident(id flat.NodeId) bool {
	return t.a.node(t.fixed_array_unwrap_arg(id)).kind == .ident
}
