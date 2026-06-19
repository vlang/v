module transform

import v3.flat

// transform_struct_fields transforms struct initialization fields with enum resolution.
// For each .field_init child, transforms the value expression. If the struct field type
// is a known enum, resolves shorthand enum values (e.g. `.red` -> `Color.red`).
fn (mut t Transformer) transform_struct_fields(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.add_missing_struct_defaults(id, node)
	}
	info := t.lookup_struct_info(node.value) or {
		// Unknown struct: fall back to generic child transform
		return t.transform_struct_children(id, node)
	}
	// Build a field name -> type lookup from the struct definition
	mut field_types := map[string]string{}
	for f in info.fields {
		field_types[f.name] = f.typ
	}
	mut field_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.children_count > 0 {
			val_id := t.a.child(&child, 0)
			val_node := t.a.nodes[int(val_id)]
			field_type := field_types[child.value] or { '' }
			if val_node.kind == .array_literal && is_fixed_array_type(field_type) {
				t.a.nodes[int(val_id)].typ = field_type
			}
			// Check if the value is an enum shorthand and the field type is an enum
			new_val := if val_node.kind == .enum_val && field_type.len > 0
				&& field_type in t.enum_types {
				t.transform_enum_shorthand(val_id, val_node, field_type)
			} else {
				t.transform_expr(val_id)
			}
			fi_start := t.a.children.len
			t.a.children << new_val
			field_ids << t.a.add_node(flat.Node{
				kind:           .field_init
				op:             child.op
				children_start: fi_start
				children_count: 1
				pos:            child.pos
				value:          child.value
				typ:            child.typ
			})
		} else {
			field_ids << child_id
		}
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return t.add_missing_struct_defaults(new_id, t.a.nodes[int(new_id)])
}

// transform_struct_children is a fallback for struct inits where the struct type is unknown.
// Transforms all field_init value expressions without enum resolution.
fn (mut t Transformer) transform_struct_children(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut field_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.children_count > 0 {
			val_id := t.a.child(&child, 0)
			new_val := t.transform_expr(val_id)
			fi_start := t.a.children.len
			t.a.children << new_val
			field_ids << t.a.add_node(flat.Node{
				kind:           .field_init
				op:             child.op
				children_start: fi_start
				children_count: 1
				pos:            child.pos
				value:          child.value
				typ:            child.typ
			})
		} else {
			field_ids << child_id
		}
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// add_missing_struct_defaults checks if any fields with default values are missing
// from the struct initialization. This is a hook point for future default-fill logic.
// Currently returns the node unchanged because StructInfo does not yet store default values.
fn (mut t Transformer) add_missing_struct_defaults(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len == 0 {
		return id
	}
	info := t.lookup_struct_info(node.value) or { return id }
	mut provided := map[string]bool{}
	mut field_ids := []flat.NodeId{cap: int(node.children_count) + info.fields.len}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init {
			provided[child.value] = true
		}
		field_ids << child_id
	}
	old_module := t.cur_module
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	mut added := false
	for field in info.fields {
		if field.name in provided || int(field.default_expr) < 0 {
			continue
		}
		default_node := t.a.nodes[int(field.default_expr)]
		new_val := if default_node.kind == .enum_val && field.typ.len > 0
			&& field.typ in t.enum_types {
			t.transform_enum_shorthand(field.default_expr, default_node, field.typ)
		} else {
			t.transform_expr(field.default_expr)
		}
		fi_start := t.a.children.len
		t.a.children << new_val
		field_ids << t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: fi_start
			children_count: 1
			value:          field.name
			typ:            field.typ
		})
		provided[field.name] = true
		added = true
	}
	t.cur_module = old_module
	if !added {
		return id
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (t &Transformer) lookup_struct_info(name string) ?StructInfo {
	if name in t.structs {
		return t.structs[name]
	}
	if name.contains('.') {
		short_name := name.all_after_last('.')
		if short_name in t.structs {
			return t.structs[short_name]
		}
		return none
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.structs {
			return t.structs[qname]
		}
	}
	return none
}

fn (mut t Transformer) transform_assoc_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .assoc || node.children_count == 0 {
		return id
	}
	base_id := t.a.child(&node, 0)
	mut assoc_type := node.value
	if assoc_type.len == 0 {
		assoc_type = t.node_type(base_id)
	}
	mut field_types := map[string]string{}
	if info := t.lookup_struct_info(assoc_type) {
		for field in info.fields {
			field_types[field.name] = field.typ
		}
	}

	tmp_name := t.new_temp('assoc')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	base := t.transform_expr(base_id)
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(tmp_name, base, assoc_type)

	for i in 1 .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := t.a.child(&field, 0)
		value_node := t.a.nodes[int(value_id)]
		field_type := field_types[field.value] or { '' }
		if value_node.kind == .array_literal && is_fixed_array_type(field_type) {
			t.a.nodes[int(value_id)].typ = field_type
		}
		value := if value_node.kind == .enum_val && field_type.len > 0 && field_type in t.enum_types {
			t.transform_enum_shorthand(value_id, value_node, field_type)
		} else {
			t.transform_expr(value_id)
		}
		t.drain_pending(mut prelude)
		prelude << t.make_assign(t.make_selector(t.make_ident(tmp_name), field.value, field_type),
			value)
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	result := t.make_ident(tmp_name)
	t.a.nodes[int(result)].typ = assoc_type
	return result
}

// transform_array_init_expr transforms .array_init nodes (e.g. `[]int{len: n}`).
// Recursively transforms any child expressions (len, cap, init values).
fn (mut t Transformer) transform_array_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	lowered := t.lower_array_init_to_runtime(id, node)
	if lowered != id {
		return lowered
	}
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
		kind:           .array_init
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_map_init_expr transforms .map_init nodes.
// Recursively transforms all child key/value expressions.
fn (mut t Transformer) transform_map_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.starts_with('map[') || node.typ.starts_with('map[') {
		return t.lower_map_init_to_runtime(id, node)
	}
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
		kind:           .map_init
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}
