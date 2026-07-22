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

fn (t &Transformer) interface_pointer_source_needs_heap_copy(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return false
	}
	child_id := t.a.child(&node, 0)
	if int(child_id) < 0 || int(child_id) >= t.a.nodes.len {
		return false
	}
	return t.interface_pointer_source_root_is_local(child_id)
}

fn (t &Transformer) interface_pointer_source_root_is_local(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value.len > 0 && t.var_type(node.value).len > 0
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			base_id := t.a.child(&node, 0)
			if address_expr_base_is_indirect_storage(t.address_expr_type_name(base_id)) {
				return false
			}
			return t.interface_pointer_source_root_is_local(base_id)
		}
		.index {
			if node.children_count == 0 {
				return false
			}
			base_id := t.a.child(&node, 0)
			base_type := t.normalize_type_alias(t.address_expr_type_name(base_id))
			if !t.is_fixed_array_type(base_type) {
				return false
			}
			return t.interface_pointer_source_root_is_local(base_id)
		}
		.paren {
			if node.children_count == 0 {
				return false
			}
			return t.interface_pointer_source_root_is_local(t.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn (t &Transformer) interface_target_should_share_source(id flat.NodeId, target_type string) bool {
	if int(id) < 0 || target_type.len == 0 || isnil(t.tc) {
		return false
	}
	iface_name := t.resolve_interface_type_name(target_type)
	if iface_name.len == 0 {
		return false
	}
	if !t.in_return_expr && t.interface_pointer_source_needs_heap_copy(id) {
		return true
	}
	return false
}

// resolve_interface_type_name resolves resolve interface type name information for transform.
fn (t &Transformer) resolve_interface_type_name(name string) string {
	if name.len == 0 || isnil(t.tc) {
		return ''
	}
	if isnil(t.interface_type_cache) {
		return t.resolve_interface_type_name_uncached(name)
	}
	mut cache := t.interface_type_cache
	if cache.module != t.cur_module || cache.file != t.cur_file {
		cache.module = t.cur_module
		cache.file = t.cur_file
		cache.entries.clear()
	}
	if resolved := cache.entries[name] {
		return resolved
	}
	resolved := t.resolve_interface_type_name_uncached(name)
	// An empty value is a cached miss. Map lookup guards distinguish a present
	// empty string from an absent key, avoiding a second map probe for misses.
	cache.entries[name] = resolved
	return resolved
}

fn (t &Transformer) resolve_interface_type_name_uncached(name string) string {
	mut clean := t.trim_pointer_type(t.normalize_type_alias(name))
	base, _, is_generic := generic_app_parts(clean)
	if is_generic {
		clean = base
	}
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
	if target_is_ptr && t.expr_is_nil_like(id) {
		expr := t.transform_expr(id)
		if int(expr) >= 0 {
			t.set_node_typ(int(expr), target_type)
		}
		return expr
	}
	// IError has bespoke handling (built via `error()`, fields accessed directly);
	// do not route it through the generic interface boxing.
	if t.is_builtin_ierror_interface_name(iface_name) {
		return none
	}
	// An explicit `source as TargetInterface` cast already carries the exact
	// interface conversion requested by the program. Transform it before using
	// the checker's source type below; otherwise a smartcasted selector can be
	// converted a second time as though it still had its storage-interface type.
	if !target_is_ptr && node.kind == .as_expr
		&& t.interface_cast_matches_target(node.value, iface_name) {
		return t.transform_as_expr(id, node)
	}
	if node.kind == .nil_literal {
		if target_is_ptr {
			expr := t.transform_expr(id)
			t.set_node_typ(int(expr), target_type)
			return expr
		}
		return none
	}
	if target_is_ptr && node.kind == .ident
		&& t.ident_is_global_pointer_to_interface(node.value, iface_name) {
		return t.transform_expr(id)
	}
	mut source_type := t.node_type(id)
	if target_is_ptr && node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		mut child_type := t.node_type(child_id)
		if child_type.len == 0 {
			child_type = t.checker_node_type(child_id)
		}
		if child_type.len == 0 {
			child_type = t.resolve_expr_type(child_id)
		}
		if child_type.len > 0
			&& t.resolve_interface_type_name(t.trim_pointer_type(child_type)) != iface_name {
			source_type = '&${t.trim_pointer_type(child_type)}'
		}
	}
	if source_type.len == 0 {
		source_type = t.checker_node_type(id)
	}
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
	if source_iface.len > 0 {
		expr := t.transform_expr(id)
		if converted := t.convert_interface_expr_to_interface(expr, source_type, iface_name) {
			if target_is_ptr {
				return t.heap_copy_interface_expr(converted, iface_name, target_type)
			}
			return converted
		}
		return none
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

fn (mut t Transformer) convert_interface_expr_to_interface(source_expr flat.NodeId, source_type string, target_iface string) ?flat.NodeId {
	source_iface := t.resolve_interface_type_name(source_type)
	if source_iface.len == 0 || target_iface.len == 0 || isnil(t.tc) {
		return none
	}
	if source_iface == target_iface {
		return source_expr
	}
	matching := t.interface_conversion_impl_mappings(source_iface, target_iface)
	if matching.len == 0 {
		return none
	}
	source_carries_fields := t.source_interface_carries_target_fields(source_iface, target_iface)
	base := t.stable_transformed_expr_for_reuse(source_expr, source_type, 'iface_cast')
	op := if source_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	object := t.make_selector_op(base, '_object', 'voidptr', op)
	init := if source_carries_fields {
		mut fields := [
			t.make_sum_literal_field('_typ', t.make_int_literal(0), 'int'),
			t.make_sum_literal_field('_object', object, 'voidptr'),
		]
		for field in t.interface_runtime_field_list(target_iface) {
			field_type := t.normalize_type_alias(field.typ.name())
			field_value := t.make_selector_op(base, field.name, field_type, op)
			fields << t.make_sum_literal_field(field.name, field_value, field_type)
		}
		t.make_interface_conversion_init(target_iface, fields)
	} else {
		t.make_struct_init(target_iface)
	}
	out_name := t.new_temp('iface_cast')
	t.pending_stmts << t.make_decl_assign_typed(out_name, init, target_iface)
	source_typ := t.make_selector_op(base, '_typ', 'int', op)
	for mapping in matching {
		cond := t.make_infix(.eq, source_typ, t.make_int_literal(mapping.source_id))
		body := if source_carries_fields {
			out_typ := t.make_selector(t.make_ident(out_name), '_typ', 'int')
			[t.make_assign(out_typ, t.make_int_literal(mapping.target_id))]
		} else {
			impl_name := t.interface_concrete_impl_name(mapping.impl) or { mapping.impl }
			impl_ptr_type := '&${impl_name}'
			impl_ptr := t.make_cast(impl_ptr_type, object, impl_ptr_type)
			mut fields := [
				t.make_sum_literal_field('_typ', t.make_int_literal(mapping.target_id), 'int'),
				t.make_sum_literal_field('_object', object, 'voidptr'),
			]
			for field in t.interface_runtime_field_list(target_iface) {
				field_type := t.normalize_type_alias(field.typ.name())
				impl_field := t.make_selector_op(impl_ptr, field.name, field_type, .arrow)
				field_value := t.null_safe_interface_pointer_field(object, impl_field, field_type)
				fields << t.make_sum_literal_field(field.name, field_value, field_type)
			}
			mapping_init := t.make_interface_conversion_init(target_iface, fields)
			[t.make_assign(t.make_ident(out_name), mapping_init)]
		}
		t.pending_stmts << t.make_if(cond, t.make_block(body), t.make_empty())
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), target_iface)
	return result
}

fn (mut t Transformer) make_interface_conversion_init(iface string, fields []flat.NodeId) flat.NodeId {
	start := t.a.children.len
	for field in fields {
		t.a.children << field
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(fields.len)
		value:          iface
		typ:            iface
	})
}

fn (t &Transformer) source_interface_carries_target_fields(source_iface string, target_iface string) bool {
	target_fields := t.interface_runtime_field_list(target_iface)
	if target_fields.len == 0 {
		return true
	}
	source_fields := t.tc.interface_field_list(source_iface)
	for target in target_fields {
		mut found := false
		for source in source_fields {
			if source.name == target.name {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}

fn (t &Transformer) interface_runtime_field_list(iface string) []types.StructField {
	mut fields := []types.StructField{}
	for field in t.tc.interface_field_list(iface) {
		if !transform_interface_field_type_contains_self_by_value(field.typ, iface) {
			fields << field
		}
	}
	return fields
}

struct InterfaceImplMapping {
	impl      string
	source_id int
	target_id int
}

fn (mut t Transformer) interface_conversion_impl_mappings(source_iface string, target_iface string) []InterfaceImplMapping {
	mut result := []InterfaceImplMapping{}
	if source_iface.len == 0 || target_iface.len == 0 || isnil(t.tc) {
		return result
	}
	source_index := t.interface_impl_index_for_transform(source_iface)
	target_index := t.interface_impl_index_for_transform(target_iface)
	for impl in source_index.names {
		source_id := source_index.ids[impl] or { continue }
		target_id := target_index.ids[impl] or {
			// Alias spellings can differ between equivalent interface snapshots.
			if !t.interface_impl_satisfies_target(impl, target_iface) {
				continue
			}
			t.interface_impl_type_id(target_iface, impl) or { continue }
		}
		result << InterfaceImplMapping{
			impl:      impl
			source_id: source_id
			target_id: target_id
		}
	}
	return result
}

fn (mut t Transformer) make_interface_target_is_check(source_expr flat.NodeId, source_type string, source_iface string, target_iface string) ?flat.NodeId {
	mappings := t.interface_conversion_impl_mappings(source_iface, target_iface)
	if mappings.len == 0 {
		return none
	}
	op := if source_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	typ := t.make_selector_op(source_expr, '_typ', 'int', op)
	mut result := flat.empty_node
	for mapping in mappings {
		cmp := t.make_infix(.eq, typ, t.make_int_literal(mapping.source_id))
		if int(result) < 0 {
			result = cmp
		} else {
			result = t.make_infix(.logical_or, result, cmp)
		}
	}
	return result
}

fn (t &Transformer) interface_impl_satisfies_target(impl string, target_iface string) bool {
	if impl in t.tc.interface_names {
		return t.tc.interface_implements_interface(impl, target_iface)
	}
	resolved := t.tc.interface_metadata_name(impl)
	if resolved in t.tc.interface_names {
		return t.tc.interface_implements_interface(resolved, target_iface)
	}
	if t.is_builtin_ierror_interface_name(target_iface) {
		return t.tc.named_type_compatible_with_ierror(impl)
	}
	return t.tc.named_type_implements_interface(impl, target_iface)
}

fn (t &Transformer) resolve_interface_pattern_interface(pattern string) ?string {
	for candidate in t.interface_pattern_candidates(pattern) {
		iface := t.resolve_interface_type_name(candidate)
		if iface.len > 0 {
			return iface
		}
	}
	return none
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

// heaped_amp_local_address_child unwraps `&v` when `v` was already moved to the heap
// because its address escapes. The interface box must store `v`, not `&*v`.
fn (t &Transformer) heaped_amp_local_address_child(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .prefix || node.op != .amp || node.children_count == 0 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind == .ident && child.value in t.heaped_amp_locals {
		return child_id
	}
	return none
}

fn (mut t Transformer) null_safe_interface_pointer_field(source flat.NodeId, value flat.NodeId, field_type string) flat.NodeId {
	cond := t.make_infix(.ne, source, t.a.add(.nil_literal))
	then_block := t.make_block(arr1(t.make_expr_stmt(value)))
	else_block := t.make_block(arr1(t.make_expr_stmt(t.zero_value_for_type(field_type))))
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            field_type
	})
}

// make_interface_literal_from_expr converts make interface literal from expr data for transform.
fn (mut t Transformer) make_interface_literal_from_expr(id flat.NodeId, iface_name string, share_source bool) ?flat.NodeId {
	fields := t.interface_runtime_field_list(iface_name)
	mut source_id := id
	mut source_type := t.node_type(id)
	mut source_is_heaped_amp_child := false
	if heaped_child_id := t.heaped_amp_local_address_child(id) {
		child := t.a.nodes[int(heaped_child_id)]
		heap_type := t.var_type(child.value)
		if heap_type.len > 0 {
			source_id = heaped_child_id
			source_type = heap_type
			source_is_heaped_amp_child = true
		}
	}
	// Type propagation normalizes aliases for operations, but interface `_typ` must
	// retain the declared alias so `is Alias` does not become `is Base`.
	if source_type.len > 0 {
		node := t.a.nodes[int(source_id)]
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
	source_expr := if source_is_heaped_amp_child {
		source := t.a.nodes[int(source_id)]
		had_rvalue := source.kind == .ident && source.value in t.pointer_value_rvalues
		if had_rvalue {
			t.pointer_value_rvalues.delete(source.value)
		}
		transformed := t.transform_expr(source_id)
		if had_rvalue {
			t.pointer_value_rvalues[source.value] = true
		}
		transformed
	} else {
		t.transform_expr(source_id)
	}
	mut source := t.stable_transformed_expr_for_reuse(source_expr, source_type, 'iface_src')
	normalized_source_type := t.normalize_type_alias(source_type)
	source_is_pointer_alias := !source_type.starts_with('&')
		&& normalized_source_type.starts_with('&')
	if source_is_pointer_alias && !share_source
		&& t.interface_pointer_alias_source_needs_heap_copy(id) {
		pointee_type := normalized_source_type[1..]
		dup := t.make_memdup_call_for_type(source, pointee_type)
		copied := t.make_cast(normalized_source_type, dup, normalized_source_type)
		tmp_name := t.new_temp('iface_ptr')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, copied, source_type)
		source = t.make_ident(tmp_name)
	}
	is_ptr := source_type.starts_with('&')
	concrete_type := if is_ptr { source_type[1..] } else { source_type }
	t.mark_interface_boxed_type(iface_name, concrete_type)
	if impl_name := t.interface_concrete_impl_name(concrete_type) {
		if impl_name != concrete_type {
			t.mark_interface_boxed_type(iface_name, impl_name)
		}
	}
	if !is_ptr && !t.expr_can_take_address(source) {
		tmp_name := t.new_temp('iface_src')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, source, source_type)
		source = t.make_ident(tmp_name)
	}
	// `_object` is a pointer to the boxed concrete value; method dispatch reads it
	// back and casts it to the concrete type. For pointer sources (`Iface(&local)`)
	// we store the pointer directly so interface calls observe the original value
	// while it is in scope. Value sources are heap-copied so the box can outlive
	// the source scope, unless the caller passed `share_source` (mut/reference
	// call args) where the box must alias the original value.
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
		if t.interface_box_object_cast_needs_raw_call(concrete_type) {
			t.set_node_typ(int(dup), '&${concrete_type}')
			dup
		} else {
			t.make_cast('&${concrete_type}', dup, '&${concrete_type}')
		}
	}
	field_base := if is_ptr {
		base := t.make_prefix(.mul, source)
		t.set_node_typ(int(base), concrete_type)
		base
	} else {
		source
	}
	mut field_ids := []flat.NodeId{cap: fields.len + 2}
	type_id := t.interface_impl_type_id(iface_name, concrete_type) or {
		if interface_pattern_is_collapsed_container_type(concrete_type) {
			t.interface_container_cast_type_id(iface_name, concrete_type) or { 0 }
		} else {
			0
		}
	}
	if type_id != 0 {
		field_ids << t.make_sum_literal_field('_typ', t.make_int_literal(type_id), 'int')
	}
	field_ids << t.make_sum_literal_field('_object', object_expr, '&${concrete_type}')
	for field in fields {
		field_type := t.normalize_type_alias(field.typ.name())
		mut field_value := t.make_selector(field_base, field.name, field_type)
		if is_ptr {
			field_value = t.null_safe_interface_pointer_field(source, field_value, field_type)
		}
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

fn (t &Transformer) interface_box_object_cast_needs_raw_call(concrete_type string) bool {
	if isnil(t.tc) {
		return false
	}
	target := t.tc.type_aliases[concrete_type] or {
		if !concrete_type.contains('.') {
			t.tc.type_aliases[t.tc.qualify_name(concrete_type)] or { return false }
		} else {
			return false
		}
	}
	return t.normalize_type_alias(target).starts_with('map[')
}

fn (t &Transformer) interface_pointer_alias_source_needs_heap_copy(id flat.NodeId) bool {
	if t.interface_pointer_source_needs_heap_copy(id) {
		return true
	}
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .cast_expr || node.children_count == 0 || node.value.starts_with('&')
		|| !t.normalize_type_alias(node.value).starts_with('&') {
		return false
	}
	arg_id := t.a.child(&node, 0)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind != .prefix || arg.op != .amp || arg.children_count == 0 {
		return false
	}
	return t.interface_pointer_source_root_is_local(t.a.child(&arg, 0))
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
