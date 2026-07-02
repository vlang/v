module transform

import v3.flat
import v3.types

// is_interface_type checks if a type name is a known interface.
fn (t &Transformer) is_interface_type(name string) bool {
	return t.resolve_interface_type_name(name).len > 0
}

// resolve_interface_type_name resolves resolve interface type name information for transform.
fn (t &Transformer) resolve_interface_type_name(name string) string {
	if name.len == 0 || isnil(t.tc) {
		return ''
	}
	clean := t.trim_pointer_type(t.normalize_type_alias(name))
	if clean in t.tc.interface_names {
		return clean
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.tc.interface_names {
			return qname
		}
	}
	return ''
}

// transform_interface_value_for_type supports transform_interface_value_for_type handling.
fn (mut t Transformer) transform_interface_value_for_type(id flat.NodeId, target_type string) ?flat.NodeId {
	if int(id) < 0 || target_type.len == 0 || isnil(t.tc) {
		return none
	}
	target_is_ptr := target_type.starts_with('&')
	iface_name := t.resolve_interface_type_name(target_type)
	if iface_name.len == 0 {
		return none
	}
	// IError has bespoke handling (built via `error()`, fields accessed directly);
	// do not route it through the generic interface boxing.
	if iface_name.all_after_last('.') == 'IError' {
		return none
	}
	source_type := t.node_type(id)
	source_iface := t.resolve_interface_type_name(source_type)
	if source_iface == iface_name {
		return t.transform_expr(id)
	}
	if target_is_ptr && source_type.starts_with('&')
		&& t.resolve_interface_type_name(source_type[1..]) == iface_name {
		return t.transform_expr(id)
	}
	literal := t.make_interface_literal_from_expr(id, iface_name, target_is_ptr) or { return none }
	if !target_is_ptr {
		return literal
	}
	// A `&Interface` value must outlive the current scope (it is commonly returned
	// or stored in a global, e.g. `default_rng = &PRNG(rng)`). Heap-allocate the
	// interface box rather than taking the address of a local temporary, which
	// would dangle.
	tmp_name := t.new_temp('iface_box')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, literal, iface_name)
	addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	size := t.make_sizeof_type(iface_name)
	dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
	cast := t.make_cast(target_type, dup, target_type)
	t.a.nodes[int(cast)].typ = target_type
	return cast
}

// transform_global_amp_interface_cast supports transform_global_amp_interface_cast handling.
fn (mut t Transformer) transform_global_amp_interface_cast(node flat.Node, target_type string) ?flat.NodeId {
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind != .cast_expr || child.children_count == 0 {
		return none
	}
	iface_name := t.resolve_interface_type_name(child.value)
	if iface_name.len == 0 || iface_name.all_after_last('.') == 'IError' {
		return none
	}
	old_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	literal := t.make_interface_literal_from_expr(t.a.child(&child, 0), iface_name, true) or {
		t.pending_stmts = old_pending
		return none
	}
	has_pending := t.pending_stmts.len > 0
	t.pending_stmts.clear()
	t.pending_stmts = old_pending
	if has_pending {
		return none
	}
	start := t.a.children.len
	t.a.children << literal
	ptr_type := if target_type.len > 0 { target_type } else { '&${iface_name}' }
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             .amp
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            ptr_type
	})
}

// make_interface_literal_from_expr converts make interface literal from expr data for transform.
fn (mut t Transformer) make_interface_literal_from_expr(id flat.NodeId, iface_name string, prefer_ref bool) ?flat.NodeId {
	fields := t.tc.interface_fields[iface_name] or { []types.StructField{} }
	source_type := t.node_type(id)
	if source_type.len == 0 {
		return none
	}
	source_expr := t.transform_expr(id)
	source := t.stable_transformed_expr_for_reuse(source_expr, source_type, 'iface_src')
	is_ptr := source_type.starts_with('&')
	concrete_type := if is_ptr { source_type[1..] } else { source_type }
	// `_object` is a pointer to the boxed concrete value; method dispatch reads it
	// back and casts it to the concrete type. For pointer sources we store the
	// pointer directly; for value sources we heap-copy so the box can outlive the
	// source scope. The pointer is typed (`&Concrete`) so codegen can recover the
	// concrete type and emit the matching `_typ` dispatch id.
	object_expr := if is_ptr {
		source
	} else if prefer_ref && t.expr_can_take_address(source) {
		addr := t.make_prefix(.amp, source)
		t.a.nodes[int(addr)].typ = '&${concrete_type}'
		addr
	} else {
		addr := t.make_prefix(.amp, source)
		size := t.make_sizeof_type(concrete_type)
		dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
		t.make_cast('&${concrete_type}', dup, '&${concrete_type}')
	}
	field_base := if is_ptr {
		base := t.make_prefix(.mul, source)
		t.a.nodes[int(base)].typ = concrete_type
		base
	} else {
		source
	}
	mut field_ids := []flat.NodeId{cap: fields.len + 1}
	field_ids << t.make_sum_literal_field('_object', object_expr, '&${concrete_type}')
	for field in fields {
		field_type := t.normalize_type_alias(field.typ.name())
		field_value := t.make_selector(field_base, field.name, field_type)
		field_ids << t.make_sum_literal_field(field.name, field_value, field_type)
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value:          iface_name
		typ:            iface_name
	})
}

// transform_interface_cast transforms interface-to-concrete type casts.
// This is a hook for future interface dispatch lowering where interface
// values need to be unwrapped to their concrete types.
// Currently passes through unchanged.
fn (mut t Transformer) transform_interface_cast(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_interface_method_call transforms method calls on interface values.
// This is a hook for vtable dispatch lowering where `iface.method(args)`
// needs to be rewritten to indirect calls through the interface vtable.
// Currently passes through unchanged.
fn (mut t Transformer) transform_interface_method_call(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}
