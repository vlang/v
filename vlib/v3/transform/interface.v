module transform

import v3.flat

// is_interface_type checks if a type name is a known interface.
fn (t &Transformer) is_interface_type(name string) bool {
	return t.resolve_interface_type_name(name).len > 0
}

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

fn (mut t Transformer) transform_interface_value_for_type(id flat.NodeId, target_type string) ?flat.NodeId {
	if int(id) < 0 || target_type.len == 0 || isnil(t.tc) {
		return none
	}
	target_is_ptr := target_type.starts_with('&')
	iface_name := t.resolve_interface_type_name(target_type)
	if iface_name.len == 0 {
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
	literal := t.make_interface_literal_from_expr(id, iface_name) or { return none }
	if !target_is_ptr {
		return literal
	}
	tmp_name := t.new_temp('iface_arg')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, literal, iface_name)
	ptr := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.a.nodes[int(ptr)].typ = target_type
	return ptr
}

fn (mut t Transformer) make_interface_literal_from_expr(id flat.NodeId, iface_name string) ?flat.NodeId {
	fields := t.tc.interface_fields[iface_name] or { return none }
	source_type := t.node_type(id)
	if source_type.len == 0 {
		return none
	}
	source_expr := t.transform_expr(id)
	source := if fields.len > 1 || !t.is_stable_expr_for_reuse(source_expr) {
		t.stable_transformed_expr_for_reuse(source_expr, source_type, 'iface_src')
	} else {
		source_expr
	}
	field_base := if source_type.starts_with('&') {
		base := t.make_prefix(.mul, source)
		t.a.nodes[int(base)].typ = source_type[1..]
		base
	} else {
		source
	}
	mut field_ids := []flat.NodeId{cap: fields.len}
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
