module transform

import v.flat
import v.types

// is_interface_type checks if a type name is a known interface.
fn (t &Transformer) is_interface_type(name_id u32) bool {
	return (t.resolve_interface_type_name(name_id)) != 0
}

// has_ierror_interface reports whether option/result wrappers carry an `err`
// field. cgen omits it when `IError` is not declared (`-no-builtin`), so lowering
// must neither bind the implicit `err` nor copy `.err` between wrappers then.
fn (t &Transformer) has_ierror_interface() bool {
	return isnil(t.tc) || t.tc.has_ierror_interface()
}

fn (t &Transformer) is_builtin_ierror_interface_name(name_id u32) bool {
	clean := flat.source_type_name(t.trim_pointer_type(t.normalize_type_alias(name_id)))
	return clean == 'IError' || clean == 'builtin.IError'
}

fn (t &Transformer) interface_cast_matches_target(cast_type_id u32, iface_name_id u32) bool {
	iface_name_comparison_type_id := iface_name_id

	cast_iface_source_id := t.resolve_interface_type_name(cast_type_id)
	cast_iface_comparison_type_id := cast_iface_source_id

	if cast_iface_source_id != 0 {
		return cast_iface_comparison_type_id == iface_name_comparison_type_id
	}
	clean_cast_comparison_type_id := t.trim_pointer_type(t.normalize_type_alias(cast_type_id))
	clean_cast := flat.source_type_name(clean_cast_comparison_type_id)
	clean_iface_comparison_type_id := t.trim_pointer_type(t.normalize_type_alias(iface_name_id))
	clean_iface := flat.source_type_name(clean_iface_comparison_type_id)
	if clean_cast_comparison_type_id == clean_iface_comparison_type_id {
		return true
	}
	return !clean_cast.contains('.') && clean_cast == clean_iface.all_after_last('.')
}

fn (mut t Transformer) heap_copy_interface_expr(expr flat.NodeId, iface_name_id u32, target_type_id u32) flat.NodeId {
	iface_name := flat.source_type_name(iface_name_id)
	target_depth, _ := pointer_type_depth_and_base(flat.source_type_name(t.normalize_type_alias(target_type_id)))
	if target_depth > 1 {
		mut current := t.heap_copy_interface_expr(expr, iface_name_id, flat.source_type_id('&${iface_name}'))
		mut current_type := '&${iface_name}'
		for _ in 1 .. target_depth {
			tmp_name := t.new_temp('iface_ref')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, current, flat.source_type_id(current_type))
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			dup := t.make_memdup_call_for_type(addr, flat.source_type_id(current_type))
			current_type = '&${current_type}'
			current = t.make_cast(flat.source_type_id(current_type), dup, flat.source_type_id(current_type))
		}
		return t.make_cast(target_type_id, current, target_type_id)
	}
	if t.a.nodes[int(expr)].kind == .struct_init {
		addr := t.make_prefix(.amp, expr)
		cast := t.make_cast(target_type_id, addr, target_type_id)
		t.set_node_typ(int(cast), target_type_id)
		return cast
	}
	tmp_name := t.new_temp('iface_box')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, iface_name_id)
	addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	size := t.make_sizeof_type(iface_name_id)
	dup := t.make_non_aliasing_allocation_call('memdup', [addr, size], flat.source_type_voidptr)
	cast := t.make_cast(target_type_id, dup, target_type_id)
	t.set_node_typ(int(cast), target_type_id)
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
			return node.value.len > 0 && (t.var_type(node.value)) != 0
		}
		.selector {
			if node.children_count == 0 {
				return false
			}
			base_id := t.a.child(&node, 0)
			if address_expr_base_is_indirect_storage(flat.source_type_name(t.address_expr_type_name(base_id))) {
				return false
			}
			return t.interface_pointer_source_root_is_local(base_id)
		}
		.index {
			if node.children_count == 0 {
				return false
			}
			base_id := t.a.child(&node, 0)
			base_type_source_id := t.normalize_type_alias(t.address_expr_type_name(base_id))

			if !t.is_fixed_array_type(base_type_source_id) {
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

fn (t &Transformer) interface_target_should_share_source(id flat.NodeId, target_type_id u32) bool {
	if int(id) < 0 || target_type_id == u32(0) || isnil(t.tc) {
		return false
	}
	iface_name_source_id := t.resolve_interface_type_name(target_type_id)
	iface_name := flat.source_type_name(iface_name_source_id)
	if iface_name_source_id == 0 {
		return false
	}
	// Interfaces with mutable fields are views onto the source object. The checker
	// already rejects immutable value sources for these casts, so an addressable
	// value must be shared rather than copied into an independent interface box.
	if t.tc.interface_field_list(iface_name).any(it.is_mut) && t.expr_can_take_address(id) {
		return true
	}
	if !t.in_return_expr
		&& (t.interface_pointer_source_needs_heap_copy(id)
			|| t.interface_pointer_alias_source_needs_heap_copy(id)) {
		return true
	}
	return false
}

// resolve_interface_type_name resolves resolve interface type name information for transform.
fn (t &Transformer) resolve_interface_type_name(name_id u32) u32 {
	if name_id == 0 || isnil(t.tc) { return 0 }
	if isnil(t.interface_type_cache) { return t.resolve_interface_type_name_uncached(name_id) }
	mut cache := t.interface_type_cache
	if !same_transform_text(cache.module, t.cur_module) || !same_transform_text(cache.file, t.cur_file) {
		cache.module = t.cur_module
		cache.file = t.cur_file
		cache.entries.clear()
	}
	if resolved := cache.entries[name_id] { return resolved }
	resolved := t.resolve_interface_type_name_uncached(name_id)
	// Zero is a cached miss; the lookup guard preserves its presence.
	cache.entries[name_id] = resolved
	return resolved
}

fn (t &Transformer) resolve_interface_type_name_uncached(name_id u32) u32 {
	raw_clean_source_id := t.trim_all_pointer_type(name_id)
	raw_clean := flat.source_type_name(raw_clean_source_id)
	raw_base, raw_args, raw_is_generic := generic_app_parts(raw_clean)
	if raw_is_generic && raw_base in t.tc.interface_names {
		return flat.source_type_id('${raw_base}[${raw_args.join(', ')}]')
	}
	if raw_clean in t.tc.interface_names {
		return raw_clean_source_id
	}
	mut clean := flat.source_type_name(t.trim_all_pointer_type(t.normalize_type_alias(name_id)))
	base, args, is_generic := generic_app_parts(clean)
	if is_generic {
		clean = base
	}
	if t.is_builtin_ierror_interface_name(flat.source_type_id(clean)) {
		if 'builtin.IError' in t.tc.interface_names {
			return flat.source_type_id('builtin.IError')
		}
		return flat.source_type_id('IError')
	}
	if clean in t.tc.interface_names {
		return flat.source_type_id(if is_generic { '${clean}[${args.join(', ')}]' } else { clean })
	}
	if !clean.contains('.') && t.cur_file.len > 0 {
		for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, clean)] or {
			[]string{}
		} {
			if candidate in t.tc.interface_names {
				return flat.source_type_id(if is_generic {
					'${candidate}[${args.join(', ')}]'
				} else {
					candidate
				})
			}
		}
	}
	if !clean.contains('.') && t.cur_module.len > 0 {
		qname := '${t.cur_module}.${clean}'
		if qname in t.tc.interface_names {
			return flat.source_type_id(if is_generic {
				'${qname}[${args.join(', ')}]'
			} else {
				qname
			})
		}
	}
	if !clean.contains('.') {
		main_name := 'main.${clean}'
		if main_name in t.tc.interface_names {
			return flat.source_type_id(if is_generic {
				'${main_name}[${args.join(', ')}]'
			} else {
				main_name
			})
		}
	}
	return u32(0)
}

fn interface_type_with_pointer_depth(source_type_id u32, interface_name_id u32) u32 {
	source_type := flat.source_type_name(source_type_id)
	interface_name := flat.source_type_name(interface_name_id)
	depth, _ := pointer_type_depth_and_base(source_type)
	return flat.source_type_id('&'.repeat(depth) + interface_name)
}

// transform_interface_value_for_type supports transform_interface_value_for_type handling.
// `share_source` makes the boxed `_object` point at the source lvalue instead of a
// heap copy; it is only safe when the source is guaranteed to outlive the box
// (mut/reference call arguments, global initializers).
fn (mut t Transformer) transform_interface_value_for_type(id flat.NodeId, target_type_id u32, share_source bool) ?flat.NodeId {
	if int(id) < 0 || target_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	target_is_ptr := flat.source_type_shape(t.normalize_type_alias(target_type_id)).has(.pointer)
	iface_name_source_id := t.resolve_interface_type_name(target_type_id)
	iface_name_comparison_type_id_3 := iface_name_source_id
	iface_name := flat.source_type_name(iface_name_comparison_type_id_3)
	if iface_name_source_id == 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if !target_is_ptr && node.kind == .paren && node.children_count == 1
		&& t.expr_is_bare_nil(id) {
		// Keep the interface target on a wrapped nil instead of boxing its voidptr
		// type. Lower the inner block normally so its statements still run.
		return t.transform_expr_for_type(t.a.child(&node, 0), target_type_id)
	}
	// Only pointer casts already have the expected representation. A value cast
	// such as Iface(ptr) still needs the pointer-target conversion below.
	if target_is_ptr && node.kind == .cast_expr && node.value.starts_with('&')
		&& flat.source_type_name(t.resolve_interface_type_name(flat.source_type_id(node.value))) == iface_name {
		if node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			if child.kind == .call && child.children_count > 0 {
				callee := t.a.child_node(&child, 0)
				if callee.kind == .ident && callee.value == 'memdup' {
					return id
				}
			}
			mut child_type := flat.source_type_name(t.node_type(child_id))
			if child_type.len == 0 {
				child_type = flat.source_type_name(t.checker_node_type(child_id))
			}
			// `&Iface(unsafe { nil })` is a null interface pointer, as in V1: box a
			// voidptr value, but never nil itself.
			if flat.source_type_name(t.normalize_type_alias(flat.source_type_id(child_type))) in [
				'voidptr',
				'&void',
			]
				&& !t.expr_is_bare_nil(child_id) {
				literal := t.make_interface_literal_from_expr(child_id, iface_name_comparison_type_id_3, false) or {
					return none
				}
				return t.heap_copy_interface_expr(literal, iface_name_comparison_type_id_3, target_type_id)
			}
		}
		return t.transform_expr(id)
	}
	if target_is_ptr && t.expr_is_nil_like(id) {
		expr := t.transform_expr(id)
		if int(expr) >= 0 {
			t.set_node_typ(int(expr), target_type_id)
		}
		return expr
	}
	if node.kind == .block {
		// Preserve the target interface type through value-yielding blocks such as
		// `unsafe { receiver }`. Boxing the block itself loses the receiver's pointer
		// storage and copies the concrete value instead of sharing its identity.
		if lowered := t.transform_block_expr_for_type(id, node, target_type_id) {
			return lowered
		}
	}
	// IError has bespoke handling (built via `error()`, fields accessed directly);
	// do not route it through the generic interface boxing.
	if t.is_builtin_ierror_interface_name(iface_name_source_id) {
		return none
	}
	// An explicit `source as TargetInterface` cast already carries the exact
	// interface conversion requested by the program. Transform it before using
	// the checker's source type below; otherwise a smartcasted selector can be
	// converted a second time as though it still had its storage-interface type.
	if !target_is_ptr && node.kind == .as_expr
		&& t.interface_cast_matches_target(flat.source_type_id(node.value), iface_name_source_id) {
		return t.transform_as_expr(id, node)
	}
	if node.kind == .nil_literal {
		if target_is_ptr {
			expr := t.transform_expr(id)
			t.set_node_typ(int(expr), target_type_id)
			return expr
		}
		return none
	}
	if target_is_ptr && node.kind == .ident
		&& t.ident_is_global_pointer_to_interface(node.value, iface_name_comparison_type_id_3) {
		return t.transform_expr(id)
	}
	mut source_type := t.node_type(id)
	if node.kind == .ident && t.pointer_value_rvalues[node.value] {
		storage_type := flat.source_type_name(t.var_type(node.value))
		if storage_type.starts_with('&&') {
			// A mutable []&Iface element (or mut p &Iface parameter) has &&Iface
			// storage, but transform_expr reads it as &Iface. Do not restore the
			// storage type on that dereference and make cgen box it again.
			source_type = flat.source_type_id(storage_type[1..])
		} else if node.value in t.heaped_amp_locals && storage_type.starts_with('&') {
			// A local moved to the heap is stored as `&Iface` but read as its `Iface`
			// value; keeping the storage type would dereference that value again.
			source_type = flat.source_type_id(storage_type[1..])
		}
	}
	mut source_is_smartcast_interface := false
	if t.expr_has_smartcast(id) {
		raw_source_type := if node.kind == .index {
			flat.source_type_name(t.resolve_index_elem_type(node))
		} else {
			flat.source_type_name(t.raw_expr_type_without_smartcast(id))
		}
		if (t.resolve_interface_type_name(flat.source_type_id(raw_source_type))) != 0 {
			source_type = flat.source_type_id(raw_source_type)
			source_is_smartcast_interface = true
		}
	}
	if t.expr_has_option_unwrap_smartcast(id) {
		if smartcast := t.find_smartcast(t.expr_key(id)) {
			unwrapped_type_source_id := t.smartcast_target_type(smartcast)
			unwrapped_type_source_id_3 := unwrapped_type_source_id

			if unwrapped_type_source_id != 0 {
				source_type = unwrapped_type_source_id_3
			}
		}
	}
	if node.kind == .ident && (t.var_type(node.value)) == 0 {
		if global_type_source_id := t.current_module_global_type(node.value) {
			global_type_source_id_2 := global_type_source_id

			source_type = global_type_source_id_2
		} else if const_type_source_id := t.raw_const_type_name_for_expr(id) {
			const_type_source_id_2 := const_type_source_id

			source_type = const_type_source_id_2
		}
	}
	if node.kind == .call {
		if fn_return_type_source_id := t.fn_value_call_return_type(node) {
			fn_return_type_source_id_2 := fn_return_type_source_id
			fn_return_type := flat.source_type_name(fn_return_type_source_id_2)

			resolved_return_type := if t.active_specialization_args.ids.len > 0 {
				flat.source_type_name(t.subst_type(fn_return_type_source_id_2, t.active_specialization_args.ids))
			} else {
				fn_return_type
			}
			if !t.generic_arg_is_unresolved(flat.source_type_id(resolved_return_type)) {
				source_type = t.call_return_type_name(flat.source_type_id(resolved_return_type), node)
			}
		} else {
			concrete_return_type_source_id := t.concrete_generic_call_return_type(id, node)
			concrete_return_type_source_id_2 := concrete_return_type_source_id

			if concrete_return_type_source_id != 0 {
				source_type = concrete_return_type_source_id_2
			}
		}
	}
	if target_is_ptr && node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		mut child_type := flat.source_type_name(t.node_type(child_id))
		if child_type.len == 0 {
			child_type = flat.source_type_name(t.checker_node_type(child_id))
		}
		if child_type.len == 0 {
			child_type = flat.source_type_name(t.resolve_expr_type(child_id))
		}
		if child_type.len > 0
			&& flat.source_type_name(t.resolve_interface_type_name(t.trim_pointer_type(flat.source_type_id(child_type)))) != iface_name {
			source_type = flat.source_type_id('&${flat.source_type_name(t.trim_pointer_type(flat.source_type_id(child_type)))}')
		}
	}
	if source_type == u32(0) || t.generic_arg_is_unresolved(source_type) {
		checker_type_source_id := t.checker_node_type(id)
		checker_type_source_id_2 := checker_type_source_id

		if checker_type_source_id != 0 && !t.generic_arg_is_unresolved(checker_type_source_id) {
			source_type = checker_type_source_id_2
		}
	}
	if target_is_ptr && flat.source_type_name(source_type) in ['nil', 'voidptr', '&void'] {
		expr := t.transform_expr(id)
		if int(expr) >= 0 {
			t.set_node_typ(int(expr), target_type_id)
		}
		return expr
	}
	if target_is_ptr && node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		child_id := t.a.child(&node, 0)
		child := t.a.nodes[int(child_id)]
		if child.kind == .cast_expr && child.children_count > 0
			&& t.interface_cast_matches_target(flat.source_type_id(child.value), iface_name_source_id) {
			cast_arg_id := t.a.child(&child, 0)
			cast_arg := t.a.nodes[int(cast_arg_id)]
			if cast_arg.kind != .nil_literal {
				return t.transform_interface_value_for_type(cast_arg_id, target_type_id, share_source)
			}
		}
	}
	if target_is_ptr && node.kind == .cast_expr && node.children_count > 0
		&& t.interface_cast_matches_target(flat.source_type_id(node.value), iface_name_source_id) {
		cast_arg_id := t.a.child(&node, 0)
		cast_arg := t.a.nodes[int(cast_arg_id)]
		cast_arg_type_source_id := t.node_type(cast_arg_id)

		if cast_arg_type_source_id == flat.source_type_voidptr {
			return id
		}
		if cast_arg.kind != .nil_literal {
			return t.transform_interface_value_for_type(cast_arg_id, target_type_id, share_source)
		}
	}
	if target_is_ptr && flat.source_type_shape(source_type).has(.pointer)
		&& flat.source_type_name(t.resolve_interface_type_name(flat.source_type_id(flat.source_type_name(source_type)[1..]))) == iface_name {
		if node.kind == .cast_expr {
			return id
		}
		expr := t.transform_expr(id)
		if source_type != 0 && int(expr) >= 0 {
			t.set_node_typ(int(expr), source_type)
		}
		return expr
	}
	source_iface_source_id := t.resolve_interface_type_name(source_type)
	source_iface_comparison_type_id := source_iface_source_id

	if !target_is_ptr && source_iface_source_id == 0
		&& t.borrowed_projection_clone_required(id, source_type) {
		// Clone the concrete projection before boxing it. Interface boxing only copies the
		// concrete bytes, so cloning the finished interface would leave its payload aliased.
		source := t.transform_expr(id)
		cloned := t.clone_borrowed_projection(id, source, source_type)
		t.set_node_typ(int(cloned), source_type)
		return t.transform_interface_value_for_type(cloned, target_type_id, false)
	}
	if source_iface_comparison_type_id == iface_name_comparison_type_id_3 {
		// The target needs the complete interface value, including its type tag,
		// rather than the concrete payload selected by a smartcast.
		expr := if source_is_smartcast_interface {
			t.make_plain_expr_for_smartcast(id)
		} else {
			t.transform_expr(id)
		}
		if source_type != 0 && int(expr) >= 0 {
			t.set_node_typ(int(expr), source_type)
		}
		if !target_is_ptr || flat.source_type_shape(source_type).has(.pointer) {
			return expr
		}
		if share_source && t.expr_can_take_address(id) {
			addr := t.make_prefix(.amp, expr)
			t.set_node_typ(int(addr), target_type_id)
			return addr
		}
		return t.heap_copy_interface_expr(expr, iface_name_comparison_type_id_3, target_type_id)
	}
	if source_iface_source_id != 0 {
		expr := if source_is_smartcast_interface {
			t.make_plain_expr_for_smartcast(id)
		} else {
			t.transform_expr(id)
		}
		if converted := t.convert_interface_expr_to_interface(expr, source_type, iface_name_comparison_type_id_3) {
			if target_is_ptr {
				return t.heap_copy_interface_expr(converted, iface_name_comparison_type_id_3, target_type_id)
			}
			return converted
		}
		return none
	}
	literal := t.make_interface_literal_from_expr(id, iface_name_comparison_type_id_3, share_source) or { return none }
	if !target_is_ptr {
		return literal
	}
	// A `&Interface` value must outlive the current scope (it is commonly returned
	// or stored in a global, e.g. `default_rng = &PRNG(rng)`). Heap-allocate the
	// interface box rather than taking the address of a local temporary, which
	// would dangle.
	return t.heap_copy_interface_expr(literal, iface_name_comparison_type_id_3, target_type_id)
}

fn (mut t Transformer) convert_interface_expr_to_interface(source_expr flat.NodeId, source_type_id_2 u32, target_iface_id u32) ?flat.NodeId {
	target_iface_comparison_type_id := target_iface_id
	target_iface := flat.source_type_name(target_iface_comparison_type_id)
	source_type_shape_2 := flat.source_type_shape(source_type_id_2)
	source_iface_source_id_2 := t.resolve_interface_type_name(source_type_id_2)
	source_iface_comparison_type_id_2 := source_iface_source_id_2

	if source_iface_source_id_2 == 0 || target_iface_id == u32(0) || isnil(t.tc) {
		return none
	}
	if source_iface_comparison_type_id_2 == target_iface_comparison_type_id {
		return source_expr
	}
	matching := t.interface_conversion_impl_mappings(source_iface_source_id_2, target_iface_id)
	if matching.len == 0 {
		return none
	}
	source_carries_fields := t.source_interface_carries_target_fields(source_iface_source_id_2, target_iface_id)
	base := t.stable_transformed_expr_for_reuse(source_expr, source_type_id_2, 'iface_cast')
	op := if source_type_shape_2.has(.pointer) { flat.Op.arrow } else { flat.Op.dot }
	object := t.make_selector_op(base, '_object', flat.source_type_voidptr, op)
	init := if source_carries_fields {
		mut fields := [
			t.make_sum_literal_field('_typ', t.make_int_literal(0), flat.source_type_int),
			t.make_sum_literal_field('_object', object, flat.source_type_voidptr),
		]
		for field in t.interface_runtime_field_list(target_iface) {
			field_type_source_id := t.normalize_type_alias(field.typ.source_type_id())

			field_value := t.make_selector_op(base, field.name, field_type_source_id, op)
			fields << t.make_sum_literal_field(field.name, field_value, field_type_source_id)
		}
		t.make_interface_conversion_init(target_iface, fields)
	} else {
		t.make_struct_init(target_iface_id)
	}
	out_name := t.new_temp('iface_cast')
	t.pending_stmts << t.make_decl_assign_typed(out_name, init, target_iface_id)
	source_typ := t.make_selector_op(base, '_typ', flat.source_type_int, op)
	for mapping in matching {
		cond := t.make_infix(.eq, source_typ, t.make_int_literal(mapping.source_id))
		body := if source_carries_fields {
			out_typ := t.make_selector(t.make_ident(out_name), '_typ', flat.source_type_int)
			[t.make_assign(out_typ, t.make_int_literal(mapping.target_id))]
		} else {
			impl_name := flat.source_type_name(t.interface_concrete_impl_name(flat.source_type_id(mapping.impl)) or { flat.source_type_id(mapping.impl) })
			impl_ptr_type := '&${impl_name}'
			impl_ptr := t.make_cast(flat.source_type_id(impl_ptr_type), object, flat.source_type_id(impl_ptr_type))
			mut fields := [
				t.make_sum_literal_field('_typ', t.make_int_literal(mapping.target_id), flat.source_type_int),
				t.make_sum_literal_field('_object', object, flat.source_type_voidptr),
			]
			for field in t.interface_runtime_field_list(target_iface) {
				field_type_source_id_2 := t.normalize_type_alias(field.typ.source_type_id())

				impl_field := t.make_selector_op(impl_ptr, field.name, field_type_source_id_2, .arrow)
				field_value := t.null_safe_interface_pointer_field(object, impl_field, field_type_source_id_2)
				fields << t.make_sum_literal_field(field.name, field_value, field_type_source_id_2)
			}
			mapping_init := t.make_interface_conversion_init(target_iface, fields)
			[t.make_assign(t.make_ident(out_name), mapping_init)]
		}
		t.pending_stmts << t.make_if(cond, t.make_block(body), t.make_empty())
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), target_iface_id)
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
		typ:            flat.source_type_id(iface)
	})
}

fn (t &Transformer) source_interface_carries_target_fields(source_iface_id u32, target_iface_id u32) bool {
	source_iface := flat.source_type_name(source_iface_id)
	target_iface := flat.source_type_name(target_iface_id)
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

fn (mut t Transformer) interface_conversion_impl_mappings(source_iface_id u32, target_iface_id u32) []InterfaceImplMapping {
	mut result := []InterfaceImplMapping{}
	if source_iface_id == u32(0) || target_iface_id == u32(0) || isnil(t.tc) {
		return result
	}
	source_index := t.interface_impl_index_for_transform(source_iface_id)
	target_index := t.interface_impl_index_for_transform(target_iface_id)
	for impl in source_index.names_text() {
		source_id := source_index.ids[impl] or { continue }
		target_id := target_index.ids[impl] or {
			// Alias spellings can differ between equivalent interface snapshots.
			if !t.interface_impl_satisfies_target(flat.source_type_id(impl), target_iface_id) {
				continue
			}
			t.interface_impl_type_id(target_iface_id, flat.source_type_id(impl)) or { continue }
		}
		result << InterfaceImplMapping{
			impl:      impl
			source_id: source_id
			target_id: target_id
		}
	}
	return result
}

fn (mut t Transformer) make_interface_target_is_check(source_expr flat.NodeId, source_type_id u32, source_iface_id u32, target_iface_id u32) ?flat.NodeId {
	source_type_shape_3 := flat.source_type_shape(source_type_id)
	mappings := t.interface_conversion_impl_mappings(source_iface_id, target_iface_id)
	if mappings.len == 0 {
		return none
	}
	op := if source_type_shape_3.has(.pointer) { flat.Op.arrow } else { flat.Op.dot }
	typ := t.make_selector_op(source_expr, '_typ', flat.source_type_int, op)
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

fn (t &Transformer) interface_impl_satisfies_target(impl_id u32, target_iface_id u32) bool {
	impl := flat.source_type_name(impl_id)
	target_iface := flat.source_type_name(target_iface_id)
	if impl in t.tc.interface_names {
		return t.tc.interface_implements_interface(impl, target_iface)
	}
	resolved := t.tc.interface_metadata_name(impl)
	if resolved in t.tc.interface_names {
		return t.tc.interface_implements_interface(resolved, target_iface)
	}
	if t.is_builtin_ierror_interface_name(target_iface_id) {
		return t.tc.named_type_compatible_with_ierror(impl)
	}
	return t.tc.named_type_implements_interface(impl, target_iface)
}

fn (t &Transformer) resolve_interface_pattern_interface(pattern_id u32) ?u32 {
	for candidate_source_id in t.interface_pattern_candidates(pattern_id) {
		candidate_source_id_2 := candidate_source_id

		iface_source_id := t.resolve_interface_type_name(candidate_source_id_2)

		if iface_source_id != 0 {
			return iface_source_id
		}
	}
	return none
}

// transform_global_amp_interface_cast supports transform_global_amp_interface_cast handling.
fn (mut t Transformer) transform_global_amp_interface_cast(node flat.Node, target_type_id u32) ?flat.NodeId {
	target_type := flat.source_type_name(target_type_id)
	mut cast := node
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		cast = t.a.nodes[int(t.a.child(&node, 0))]
	}
	if cast.kind != .cast_expr || cast.children_count == 0 {
		return none
	}
	iface_name_source_id := t.resolve_interface_type_name(flat.source_type_id(cast.value))
	iface_name := flat.source_type_name(iface_name_source_id)
	if iface_name_source_id == 0 || t.is_builtin_ierror_interface_name(iface_name_source_id) {
		return none
	}
	old_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	literal := t.make_interface_literal_from_expr(t.a.child(&cast, 0), iface_name_source_id, false) or {
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
	ptr_type := if target_type_id != u32(0) { target_type } else { '&${iface_name}' }
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             .amp
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            flat.source_type_id(ptr_type)
	})
}

fn (t &Transformer) mut_param_address_source_type(id flat.NodeId) ?u32 {
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
	if base_type == 0 {
		base_type = t.raw_var_type(child.value)
	}
	base_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(base_type)))
	if flat.source_type_shape(base_type).has(.mutable_type) {
		base_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(base_type)[4..]))
	}
	if base_type == 0 {
		return none
	}
	clean_source_id := t.normalize_type_alias(base_type)
	clean := flat.source_type_name(clean_source_id)
	if clean.starts_with('&') {
		return clean_source_id
	}
	return flat.source_type_id('&${clean}')
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

fn (mut t Transformer) null_safe_interface_pointer_field(source flat.NodeId, value flat.NodeId, field_type_id u32) flat.NodeId {
	cond := t.make_infix(.ne, source, t.a.add(.nil_literal))
	then_block := t.make_block([t.make_expr_stmt(value)])
	else_block := t.make_block([t.make_expr_stmt(t.zero_value_for_type(field_type_id))])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            field_type_id
	})
}

// make_interface_literal_from_expr converts make interface literal from expr data for transform.
fn (mut t Transformer) make_interface_literal_from_expr(id flat.NodeId, iface_name_id u32, share_source bool) ?flat.NodeId {
	iface_name := flat.source_type_name(iface_name_id)
	fields := t.interface_runtime_field_list(iface_name)
	mut source_id := id
	mut source_type := t.node_type(id)
	if t.expr_has_option_unwrap_smartcast(id) {
		if smartcast := t.find_smartcast(t.expr_key(id)) {
			unwrapped_type_source_id_2 := t.smartcast_target_type(smartcast)
			unwrapped_type_source_id_4 := unwrapped_type_source_id_2

			if unwrapped_type_source_id_2 != 0 {
				source_type = unwrapped_type_source_id_4
			}
		}
	}
	mut source_is_heaped_amp_child := false
	if heaped_child_id := t.heaped_amp_local_address_child(id) {
		child := t.a.nodes[int(heaped_child_id)]
		heap_type_source_id := t.var_type(child.value)
		heap_type_source_id_2 := heap_type_source_id

		if heap_type_source_id != 0 {
			source_id = heaped_child_id
			source_type = heap_type_source_id_2
			source_is_heaped_amp_child = true
		}
	}
	// Type propagation normalizes aliases for operations, but interface `_typ` must
	// retain the declared alias so `is Alias` does not become `is Base`.
	if source_type != 0 {
		node := t.a.nodes[int(source_id)]
		if node.kind == .ident {
			raw_type_source_id := t.raw_var_type(node.value)
			raw_type_source_id_2 := raw_type_source_id

			if raw_type_source_id != 0 && t.normalize_type_alias(raw_type_source_id) == source_type {
				raw_parsed := t.tc.parse_type(raw_type_source_id)
				if raw_parsed is types.Alias
					|| (raw_parsed is types.Pointer && raw_parsed.base_type is types.Alias) {
					source_type = raw_type_source_id_2
				}
			}
		}
	}
	if adjusted_source_id := t.mut_param_address_source_type(id) {
		adjusted_source_id_2 := adjusted_source_id

		source_type = adjusted_source_id_2
	}
	source_node := t.a.nodes[int(source_id)]
	source_is_mut_pointer_slot := source_node.kind == .ident
		&& (t.pointer_value_rvalues[source_node.value]
			|| source_node.value in t.mut_param_values)
		&& flat.source_type_shape(t.var_type(source_node.value)).has(.pointer_chain)
	if source_is_mut_pointer_slot {
		// `mut p &T` has `&&T` storage but its expression value is `&T`.
		source_type = flat.source_type_id(flat.source_type_name(t.var_type(source_node.value))[1..])
	}
	if source_type == 0 {
		return none
	}
	source_is_generic_mut_pointer_slot := source_is_mut_pointer_slot
		&& source_node.value !in t.pointer_value_rvalues
	source_expr := if source_is_generic_mut_pointer_slot {
		// A `mut x T` specialization with `T = &U` has `&&U` storage, while the
		// scoped expression type is `&U`. Read the pointer value before boxing it.
		value := t.transform_expr(source_id)
		deref := t.make_prefix(.mul, value)
		t.set_node_typ(int(deref), source_type)
		deref
	} else if source_is_heaped_amp_child || flat.source_type_shape(source_type).has(.pointer) {
		source := t.a.nodes[int(source_id)]
		had_rvalue := source.kind == .ident && source.value in t.pointer_value_rvalues
			&& !source_is_mut_pointer_slot
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
	normalized_source_type_source_id := t.normalize_type_alias(source_type)
	normalized_source_type := flat.source_type_name(normalized_source_type_source_id)
	source_is_pointer_alias := !flat.source_type_shape(source_type).has(.pointer)
		&& normalized_source_type.starts_with('&')
	source_has_pointer_storage := flat.source_type_shape(source_type).has(.pointer) || source_is_pointer_alias
	alias_implements_interface := source_is_pointer_alias && !isnil(t.tc)
		&& t.tc.type_text_implements_interface(flat.source_type_name(source_type), iface_name)
	if source_is_pointer_alias && !share_source
		&& t.interface_pointer_alias_source_needs_heap_copy(id) {
		pointee_type := normalized_source_type[1..]
		dup := t.make_memdup_call_for_type(source, flat.source_type_id(pointee_type))
		copied := t.make_cast(normalized_source_type_source_id, dup, normalized_source_type_source_id)
		tmp_name := t.new_temp('iface_ptr')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, copied, source_type)
		source = t.make_ident(tmp_name)
	}
	is_ptr := source_has_pointer_storage && !alias_implements_interface
	concrete_type := if source_is_pointer_alias && !alias_implements_interface {
		normalized_source_type[1..]
	} else if is_ptr {
		flat.source_type_name(source_type)[1..]
	} else {
		flat.source_type_name(source_type)
	}
	if !is_ptr && !share_source {
		source = t.clone_owned_array_storage_value(source, source_type, t.array_storage_source_is_mut_param(source_id))
	}
	t.mark_interface_boxed_type(iface_name_id, flat.source_type_id(concrete_type))
	if impl_name_source_id := t.interface_concrete_impl_name(flat.source_type_id(concrete_type)) {
		impl_name := flat.source_type_name(impl_name_source_id)

		if impl_name != concrete_type {
			t.mark_interface_boxed_type(iface_name_id, impl_name_source_id)
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
	} else if share_source && !source_is_pointer_alias && t.expr_can_take_address(source) {
		addr := t.make_prefix(.amp, source)
		t.set_node_typ(int(addr), flat.source_type_id('&${concrete_type}'))
		addr
	} else {
		// A pointer alias needs its own boxed slot, even when its pointee is shared.
		// The temporary holding that pointer can expire before the interface does.
		addr := t.make_prefix(.amp, source)
		size := t.make_sizeof_type(flat.source_type_id(concrete_type))
		dup := t.make_non_aliasing_allocation_call('memdup', [addr, size], flat.source_type_voidptr)
		if t.interface_box_object_cast_needs_raw_call(flat.source_type_id(concrete_type)) {
			t.set_node_typ(int(dup), flat.source_type_id('&${concrete_type}'))
			dup
		} else {
			t.make_cast(flat.source_type_id('&${concrete_type}'), dup, flat.source_type_id('&${concrete_type}'))
		}
	}
	field_base := if is_ptr {
		base := t.make_prefix(.mul, source)
		t.set_node_typ(int(base), flat.source_type_id(concrete_type))
		base
	} else {
		source
	}
	mut field_ids := []flat.NodeId{cap: fields.len + 2}
	type_id := t.interface_impl_type_id(iface_name_id, flat.source_type_id(concrete_type)) or {
		if interface_pattern_is_collapsed_container_type(concrete_type) {
			t.interface_container_cast_type_id(iface_name_id, flat.source_type_id(concrete_type)) or { 0 }
		} else {
			0
		}
	}
	if type_id != 0 {
		field_ids << t.make_sum_literal_field('_typ', t.make_int_literal(type_id), flat.source_type_int)
	}
	field_ids << t.make_sum_literal_field('_object', object_expr, flat.source_type_id('&${concrete_type}'))
	for field in fields {
		field_type_source_id := t.normalize_type_alias(field.typ.source_type_id())

		mut field_value := if concrete_type == 'voidptr' {
			t.zero_value_for_type(field_type_source_id)
		} else {
			t.make_selector(field_base, field.name, field_type_source_id)
		}
		if source_has_pointer_storage && concrete_type != 'voidptr' {
			field_value = t.null_safe_interface_pointer_field(source, field_value, field_type_source_id)
		}
		field_ids << t.make_sum_literal_field(field.name, field_value, field_type_source_id)
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
		typ:            iface_name_id
	})
}

fn (t &Transformer) interface_box_object_cast_needs_raw_call(concrete_type_id u32) bool {
	concrete_type := flat.source_type_name(concrete_type_id)
	if isnil(t.tc) {
		return false
	}
	target_source_id := t.tc.type_aliases.type_id(concrete_type) or {
		flat.source_type_id(if !concrete_type.contains('.') {
			flat.source_type_name(t.tc.type_aliases.type_id(t.tc.qualify_name(concrete_type)) or { return false })
		} else {
			return false
		})
	}

	return flat.source_type_shape(t.normalize_type_alias(target_source_id)).has(.map_type)
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
		|| !flat.source_type_shape(t.normalize_type_alias(flat.source_type_id(node.value))).has(.pointer) {
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

fn (t &Transformer) ident_is_global_pointer_to_interface(name string, iface_name_id u32) bool {
	if name == '' || iface_name_id == u32(0) || isnil(t.tc) || (t.var_type(name)) != 0 {
		return false
	}
	if t.cur_module.len > 0 {
		qname := '${t.cur_module}.${name}'
		if qname != name {
			if typ := t.tc.file_scope.lookup(qname) {
				return t.type_is_pointer_to_interface(typ, iface_name_id)
			}
		}
	}
	if typ := t.tc.file_scope.lookup(name) {
		if t.type_is_pointer_to_interface(typ, iface_name_id) {
			return true
		}
	}
	if t.cur_module.len > 0 {
		qname := '${t.cur_module}.${name}'
		if typ_source_id := t.globals.type_id(qname) {
			return t.type_text_is_pointer_to_interface(typ_source_id, iface_name_id)
		}
	}
	if typ_source_id := t.globals.type_id(name) {
		return t.type_text_is_pointer_to_interface(typ_source_id, iface_name_id)
	}
	return false
}

fn (t &Transformer) type_is_pointer_to_interface(typ types.Type, iface_name_id u32) bool {
	iface_name := flat.source_type_name(iface_name_id)
	if typ is types.Pointer {
		return flat.source_type_name(t.resolve_interface_type_name(typ.base_type.source_type_id())) == iface_name
	}
	if typ is types.Alias {
		return t.type_is_pointer_to_interface(typ.base_type, iface_name_id)
	}
	return false
}

fn (t &Transformer) type_text_is_pointer_to_interface(typ_id u32, iface_name_id u32) bool {
	iface_name := flat.source_type_name(iface_name_id)
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if clean.starts_with('&') {
		return flat.source_type_name(t.resolve_interface_type_name(flat.source_type_id(clean[1..]))) == iface_name
	}
	if clean.starts_with('mut ') {
		return flat.source_type_name(t.resolve_interface_type_name(flat.source_type_id(clean[4..]))) == iface_name
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

// transform_interface_method_call transforms the arguments of a vtable-dispatched
// interface call using the abstract method's signature.
fn (mut t Transformer) transform_interface_method_call(id flat.NodeId, node flat.Node) flat.NodeId {
	specialized_ret_type_source_id := t.specialized_interface_method_call_return_type(id, node) or { u32(0) }

	mut interface_name := ''
	mut interface_receiver_type := ''
	mut interface_base := flat.empty_node
	if node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			base_id := t.a.child(callee, 0)
			interface_base = base_id
			mut receiver_source_type := t.original_expr_type(base_id)
			if t.active_specialization_args.ids.len > 0 {
				receiver_source_type = t.subst_type(receiver_source_type, t.active_specialization_args.ids)
			}
			interface_name = flat.source_type_name(t.resolve_interface_type_name(receiver_source_type))
			if interface_name.len == 0 {
				receiver_source_type = t.node_type(base_id)
				if t.active_specialization_args.ids.len > 0 {
					receiver_source_type = t.subst_type(receiver_source_type, t.active_specialization_args.ids)
				}
				interface_name = flat.source_type_name(t.resolve_interface_type_name(receiver_source_type))
			}
			receiver_base, receiver_args, receiver_is_generic := generic_app_parts(flat.source_type_name(t.trim_pointer_type(receiver_source_type)))
			if receiver_is_generic && receiver_args.len > 0
				&& interface_name == receiver_base
				&& !receiver_args.any(t.generic_arg_is_unresolved(flat.source_type_id(it))) {
				interface_name = '${receiver_base}[${receiver_args.join(', ')}]'
			}
			if interface_name != '' {
				interface_receiver_type = flat.source_type_name(interface_type_with_pointer_depth(receiver_source_type, flat.source_type_id(interface_name)))
			}
			if _ := t.raw_const_type_name_for_expr(base_id) {
				// A module-qualified interface constant (`net.err_foo.code()`) is
				// syntactically a selector chain. Lower it to the interface wrapper
				// explicitly so C generation cannot mistake the constant name for a
				// concrete receiver type.
				method_name := t.tc.resolved_call_name(id) or { '' }
				if method_name.len > 0 && t.is_known_fn_name(method_name) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type_source_id := t.receiver_method_return_type(method_name, node.typ)

					t.mark_fn_used_name(method_name)
					return t.make_receiver_method_call_typed(node, method_name, args, ret_type_source_id)
				}
			}
		}
	}
	transformed_id := t.transform_call_args(id, node)
	if specialized_ret_type_source_id != 0 {
		t.set_node_typ(int(transformed_id), specialized_ret_type_source_id)
	}
	if interface_name == '' {
		return transformed_id
	}
	transformed := t.a.nodes[int(transformed_id)]
	if transformed.kind != .call || transformed.children_count == 0 {
		return transformed_id
	}
	callee := t.a.child_node(&transformed, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return transformed_id
	}
	base := t.a.child(callee, 0)
	base_node := t.a.nodes[int(base)]
	original_base := if interface_base != flat.empty_node {
		t.a.nodes[int(interface_base)]
	} else {
		base_node
	}
	typed_receiver := if t.interface_receiver_has_variant_projection(base)
		|| (original_base.kind == .selector && original_base.children_count > 0
			&& t.expr_or_selector_base_has_smartcast(t.a.child(&original_base, 0))) {
		// Keep the containing object's projection when calling through an interface field.
		t.retype_interface_receiver(base, flat.source_type_id(interface_receiver_type))
	} else if t.selector_reads_interface_field(original_base) {
		// A field read through an interface value was lowered to the field of the
		// object that the value holds: the receiver is that field, not the copy of it
		// in the interface value.
		t.retype_interface_receiver(base, flat.source_type_id(interface_receiver_type))
	} else if original_base.kind == .selector && original_base.children_count > 0
		&& t.a.child_node(&original_base, 0).kind == .ident {
		original_ident := t.a.child_node(&original_base, 0)
		ident := t.make_ident(original_ident.value)
		if original_ident.typ != 0 {
			t.set_node_typ(int(ident), original_ident.typ)
		}
		t.make_selector_op(ident, original_base.value, flat.source_type_id(interface_receiver_type), original_base.op)
	} else if original_base.kind == .ident {
		ident := t.make_ident(original_base.value)
		t.set_node_typ(int(ident), flat.source_type_id(interface_receiver_type))
		ident
	} else if base_node.kind == .selector && base_node.children_count > 0 {
		t.make_selector_op(t.a.child(&base_node, 0), base_node.value, flat.source_type_id(interface_receiver_type), base_node.op)
	} else {
		base
	}
	typed_base := t.make_paren(typed_receiver)
	t.set_node_typ(int(typed_base), flat.source_type_id(interface_receiver_type))
	typed_callee := t.make_selector(typed_base, callee.value, callee.typ)
	mut args := []flat.NodeId{cap: int(transformed.children_count) - 1}
	for i in 1 .. transformed.children_count {
		args << t.a.child(&transformed, i)
	}
	return t.make_call_expr_typed(typed_callee, args, transformed.typ)
}

fn (t &Transformer) specialized_interface_method_call_return_type(_id flat.NodeId, node flat.Node) ?u32 {
	if isnil(t.tc) || node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	base_id := t.a.child(callee, 0)
	mut base_type := t.raw_const_type_name_for_expr(base_id) or { u32(0) }
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if base_type == 0 {
		base_type = t.node_type(base_id)
	}
	if t.active_specialization_args.ids.len > 0 {
		base_type = t.subst_type(base_type, t.active_specialization_args.ids)
	}
	iface_name_source_id_2 := t.resolve_interface_type_name(base_type)
	iface_name := flat.source_type_name(iface_name_source_id_2)
	if iface_name_source_id_2 == 0 {
		return none
	}
	decl_key := t.tc.interface_method_signature_key(iface_name, callee.value) or { return none }
	_, ret := t.tc.specialized_interface_method_signature(iface_name, decl_key)
	if ret is types.Unknown || ret is types.Void {
		return none
	}
	ret_name := ret.name()
	if ret_name.len == 0 || ret_name in ['unknown', 'void'] {
		return none
	}
	return flat.source_type_id(ret_name)
}

// selector_reads_interface_field reports whether node selects a field that an
// interface declares from a value of that interface.
fn (t &Transformer) selector_reads_interface_field(node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 || node.value in ['_typ', '_object'] {
		return false
	}
	iface_name_source_id_3 := t.resolve_interface_type_name(t.node_type(t.a.child(&node, 0)))

	if iface_name_source_id_3 == 0 {
		return false
	}
	if _ := t.interface_field_type_name(iface_name_source_id_3, node.value) {
		return true
	}
	return false
}

fn (t &Transformer) interface_receiver_has_variant_projection(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	if _ := t.generated_variant_access_type(id) {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.selector, .prefix, .paren] && node.children_count > 0 {
		return t.interface_receiver_has_variant_projection(t.a.child(&node, 0))
	}
	return false
}

fn (mut t Transformer) retype_interface_receiver(id flat.NodeId, typ_id u32) flat.NodeId {
	node := t.a.nodes[int(id)]
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.a.child(&node, i)
	}
	retyped := t.copy_node_with_children(node, children)
	t.set_node_typ(int(retyped), typ_id)
	return retyped
}
