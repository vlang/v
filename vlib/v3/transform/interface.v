module transform

import v3.flat
import v3.types

// is_interface_type checks if a type name is a known interface.
fn (t &Transformer) is_interface_type(name string) bool {
	return t.resolve_interface_type_name(name).len > 0
}

fn (t &Transformer) is_builtin_ierror_interface_name(name string) bool {
	clean := t.trim_pointer_type(t.normalize_type_alias(name))
	return clean == 'IError' || clean == 'builtin.IError'
}

fn (t &Transformer) interface_cast_matches_target(cast_type string, iface_name string) bool {
	cast_iface := t.resolve_interface_type_name(cast_type)
	if cast_iface.len > 0 {
		return cast_iface == iface_name
	}
	clean_cast := t.trim_pointer_type(t.normalize_type_alias(cast_type))
	clean_iface := t.trim_pointer_type(t.normalize_type_alias(iface_name))
	if clean_cast == clean_iface {
		return true
	}
	return !clean_cast.contains('.') && clean_cast == clean_iface.all_after_last('.')
}

fn (mut t Transformer) heap_copy_interface_expr(expr flat.NodeId, iface_name string, target_type string) flat.NodeId {
	tmp_name := t.new_temp('iface_box')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, iface_name)
	addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	size := t.make_sizeof_type(iface_name)
	dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
	cast := t.make_cast(target_type, dup, target_type)
	t.set_node_typ(int(cast), target_type)
	return cast
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
	if !clean.contains('.') && t.cur_file.len > 0 {
		for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, clean)] or {
			[]string{}
		} {
			if candidate in t.tc.interface_names {
				return candidate
			}
		}
	}
	if !clean.contains('.') && t.cur_module.len > 0 {
		qname := '${t.cur_module}.${clean}'
		if qname in t.tc.interface_names {
			return qname
		}
	}
	if !clean.contains('.') {
		main_name := 'main.${clean}'
		if main_name in t.tc.interface_names {
			return main_name
		}
	}
	return ''
}

// transform_interface_value_for_type supports transform_interface_value_for_type handling.
// `share_source` makes the boxed `_object` point at the source lvalue instead of a
// heap copy; it is only safe when the source is guaranteed to outlive the box
// (mut/reference call arguments, global initializers).
fn (mut t Transformer) transform_interface_value_for_type(id flat.NodeId, target_type string, share_source bool) ?flat.NodeId {
	if int(id) < 0 || target_type.len == 0 || isnil(t.tc) {
		return none
	}
	target_is_ptr := target_type.starts_with('&')
	iface_name := t.resolve_interface_type_name(target_type)
	if iface_name.len == 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if target_is_ptr && node.kind == .cast_expr
		&& t.resolve_interface_type_name(node.value) == iface_name {
		if node.children_count == 1 {
			child := t.a.nodes[int(t.a.child(&node, 0))]
			if child.kind == .call && child.children_count > 0 {
				callee := t.a.child_node(&child, 0)
				if callee.kind == .ident && callee.value == 'memdup' {
					return id
				}
			}
		}
		return t.transform_expr(id)
	}
	if target_is_ptr && node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		child := t.a.nodes[int(t.a.child(&node, 0))]
		if child.kind == .cast_expr && t.resolve_interface_type_name(child.value) == iface_name {
			return t.transform_expr(id)
		}
	}
	// IError has bespoke handling (built via `error()`, fields accessed directly);
	// do not route it through the generic interface boxing.
	if t.is_builtin_ierror_interface_name(iface_name) {
		return none
	}
	if node.kind == .nil_literal {
		return none
	}
	if target_is_ptr && node.kind == .ident
		&& t.ident_is_global_pointer_to_interface(node.value, iface_name) {
		return t.transform_expr(id)
	}
	source_type := t.node_type(id)
	if target_is_ptr && node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		if child.kind == .cast_expr && child.children_count > 0
			&& t.interface_cast_matches_target(child.value, iface_name) {
			cast_arg_id := t.a.child(&child, 0)
			cast_arg := t.a.nodes[int(cast_arg_id)]
			if cast_arg.kind != .nil_literal {
				return t.transform_interface_value_for_type(cast_arg_id, target_type, share_source)
			}
		}
	}
	if target_is_ptr && node.kind == .cast_expr && node.children_count > 0
		&& t.interface_cast_matches_target(node.value, iface_name) {
		cast_arg_id := t.a.child(&node, 0)
		cast_arg := t.a.nodes[int(cast_arg_id)]
		cast_arg_type := t.node_type(cast_arg_id)
		if cast_arg_type == 'voidptr' {
			return id
		}
		if cast_arg.kind != .nil_literal {
			return t.transform_interface_value_for_type(cast_arg_id, target_type, share_source)
		}
	}
	if target_is_ptr && source_type.starts_with('&')
		&& t.resolve_interface_type_name(source_type[1..]) == iface_name {
		if node.kind == .cast_expr {
			return id
		}
		expr := t.transform_expr(id)
		if source_type.len > 0 && int(expr) >= 0 {
			t.set_node_typ(int(expr), source_type)
		}
		return expr
	}
	source_iface := t.resolve_interface_type_name(source_type)
	if source_iface == iface_name {
		expr := t.transform_expr(id)
		if source_type.len > 0 && int(expr) >= 0 {
			t.set_node_typ(int(expr), source_type)
		}
		if !target_is_ptr || source_type.starts_with('&') {
			return expr
		}
		if share_source && t.expr_can_take_address(id) {
			addr := t.make_prefix(.amp, expr)
			t.set_node_typ(int(addr), target_type)
			return addr
		}
		return t.heap_copy_interface_expr(expr, iface_name, target_type)
	}
	literal := t.make_interface_literal_from_expr(id, iface_name, share_source) or { return none }
	if !target_is_ptr {
		return literal
	}
	// A `&Interface` value must outlive the current scope (it is commonly returned
	// or stored in a global, e.g. `default_rng = &PRNG(rng)`). Heap-allocate the
	// interface box rather than taking the address of a local temporary, which
	// would dangle.
	return t.heap_copy_interface_expr(literal, iface_name, target_type)
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
	if iface_name.len == 0 || t.is_builtin_ierror_interface_name(iface_name) {
		return none
	}
	old_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	literal := t.make_interface_literal_from_expr(t.a.child(&child, 0), iface_name, false) or {
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

fn (t &Transformer) mut_param_address_source_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	child := t.a.nodes[int(t.a.child(&node, 0))]
	if child.kind != .ident || child.value.len == 0 || !t.mut_param_values[child.value] {
		return none
	}
	mut base_type := t.var_type(child.value)
	if base_type.len == 0 {
		base_type = t.raw_var_type(child.value)
	}
	base_type = base_type.trim_space()
	if base_type.starts_with('mut ') {
		base_type = base_type[4..].trim_space()
	}
	if base_type.len == 0 {
		return none
	}
	clean := t.normalize_type_alias(base_type)
	if clean.starts_with('&') {
		return clean
	}
	return '&${clean}'
}

// make_interface_literal_from_expr converts make interface literal from expr data for transform.
fn (mut t Transformer) make_interface_literal_from_expr(id flat.NodeId, iface_name string, share_source bool) ?flat.NodeId {
	fields := t.tc.interface_fields[iface_name] or { []types.StructField{} }
	mut source_type := t.node_type(id)
	// Type propagation normalizes aliases for operations, but interface `_typ` must
	// retain the declared alias so `is Alias` does not become `is Base`.
	if source_type.len > 0 {
		node := t.a.nodes[int(id)]
		if node.kind == .ident {
			raw_type := t.raw_var_type(node.value)
			if raw_type.len > 0 && t.normalize_type_alias(raw_type) == source_type {
				raw_parsed := t.tc.parse_type(raw_type)
				if raw_parsed is types.Alias
					|| (raw_parsed is types.Pointer && raw_parsed.base_type is types.Alias) {
					source_type = raw_type
				}
			}
		}
	}
	if adjusted := t.mut_param_address_source_type(id) {
		source_type = adjusted
	}
	if source_type.len == 0 {
		return none
	}
	source_expr := t.transform_expr(id)
	mut source := t.stable_transformed_expr_for_reuse(source_expr, source_type, 'iface_src')
	is_ptr := source_type.starts_with('&')
	concrete_type := if is_ptr { source_type[1..] } else { source_type }
	t.mark_interface_boxed_type(iface_name, concrete_type)
	if !is_ptr && !t.expr_can_take_address(source) {
		tmp_name := t.new_temp('iface_src')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, source, source_type)
		source = t.make_ident(tmp_name)
	}
	// `_object` is a pointer to the boxed concrete value; method dispatch reads it
	// back and casts it to the concrete type. For pointer sources we store the
	// pointer directly; for value sources we heap-copy so the box can outlive the
	// source scope, unless the caller passed `share_source` (mut/reference call
	// args) where the box must alias the original value.
	// The pointer is typed (`&Concrete`) so codegen can recover the concrete
	// type and emit the matching `_typ` dispatch id.
	object_expr := if is_ptr {
		source
	} else if share_source && t.expr_can_take_address(source) {
		addr := t.make_prefix(.amp, source)
		t.set_node_typ(int(addr), '&${concrete_type}')
		addr
	} else {
		addr := t.make_prefix(.amp, source)
		size := t.make_sizeof_type(concrete_type)
		dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
		t.make_cast('&${concrete_type}', dup, '&${concrete_type}')
	}
	field_base := if is_ptr {
		base := t.make_prefix(.mul, source)
		t.set_node_typ(int(base), concrete_type)
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

fn (t &Transformer) ident_is_global_pointer_to_interface(name string, iface_name string) bool {
	if name.len == 0 || iface_name.len == 0 || isnil(t.tc) || t.var_type(name).len > 0 {
		return false
	}
	if typ := t.tc.file_scope.lookup(name) {
		if t.type_is_pointer_to_interface(typ, iface_name) {
			return true
		}
	}
	if t.cur_module.len > 0 {
		qname := '${t.cur_module}.${name}'
		if qname != name {
			if typ := t.tc.file_scope.lookup(qname) {
				if t.type_is_pointer_to_interface(typ, iface_name) {
					return true
				}
			}
		}
	}
	if typ := t.globals[name] {
		return t.type_text_is_pointer_to_interface(typ, iface_name)
	}
	if t.cur_module.len > 0 {
		qname := '${t.cur_module}.${name}'
		if typ := t.globals[qname] {
			return t.type_text_is_pointer_to_interface(typ, iface_name)
		}
	}
	return false
}

fn (t &Transformer) type_is_pointer_to_interface(typ types.Type, iface_name string) bool {
	if typ is types.Pointer {
		return t.resolve_interface_type_name(typ.base_type.name()) == iface_name
	}
	if typ is types.Alias {
		return t.type_is_pointer_to_interface(typ.base_type, iface_name)
	}
	return false
}

fn (t &Transformer) type_text_is_pointer_to_interface(typ string, iface_name string) bool {
	clean := typ.trim_space()
	if clean.starts_with('&') {
		return t.resolve_interface_type_name(clean[1..]) == iface_name
	}
	if clean.starts_with('mut ') {
		return t.resolve_interface_type_name(clean[4..]) == iface_name
	}
	return false
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
