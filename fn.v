	}
	t.default_clone_expansion_stack << clean
	defer {
		t.default_clone_expansion_stack.delete_last()
	}
	if t.is_sum_type_name(clean) {
		// Call the shared clone helper rather than inlining the variant switch at every read
		// site: a recursive sum type such as `toml.Any` would otherwise expand a large switch
		// (one arm per variant, recursively) at each site and overflow the AST. The helper body
		// is synthesized exactly once (see build_default_clone_helper_fn).
		return t.request_default_clone_helper(source, clean)
	}
	info := t.lookup_struct_info(clean) or { return source }
	mut owning_fields := []FieldInfo{}
	for field in info.fields {
		field_type := t.compiler_default_clone_field_type(clean, field)
		if t.compiler_default_clone_type_needs_work(field_type) {
			owning_fields << field
		}
	}
	if owning_fields.len == 0 {
		return source
	}
	// An addressable source keeps owning its fields, so the aggregate copy below is
	// only a non-owning template. A temporary source transfers its fields into the
	// aggregate and those originals must be destroyed after their clones are saved.
	source_fields_are_owned := !t.expr_can_take_address(source)
	tmp_name := t.new_temp('derived_clone')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, source, clean)
	for field in owning_fields {
		field_type := t.compiler_default_clone_field_type(clean, field)
		source_field := t.make_selector(t.make_ident(tmp_name), field.name, field_type)
		mut cloned_field := t.make_compiler_default_clone_value(source_field, field_type, true)
		if source_fields_are_owned {
			cloned_name := t.new_temp('derived_clone_field')
			t.pending_stmts << t.make_decl_assign_typed(cloned_name, cloned_field, field_type)
			drop_call := t.make_call_typed('drop_owned', [source_field], 'void')
			t.pending_stmts << t.make_expr_stmt(drop_call)
			cloned_field = t.make_ident(cloned_name)
		}
		t.pending_stmts << t.make_assign_after_owned_drop(t.make_selector(t.make_ident(tmp_name), field.name, field_type), cloned_field)
	}
	return t.make_ident(tmp_name)
}

// make_compiler_default_sum_clone_value rebuilds the active variant so its boxed
// payload is independent from the source sum value.
fn (mut t Transformer) make_compiler_default_sum_clone_value(source flat.NodeId, sum_type string) flat.NodeId {
	resolved_sum := t.resolve_sum_name(sum_type)
	variants := t.sum_types[resolved_sum] or { return source }
	if variants.len == 0 {
		return source
	}
	source_is_owned_temporary := !t.expr_can_take_address(source)
	stable_source := t.stable_transformed_expr_for_reuse(source, sum_type, 'derived_clone_sum_source')
	out_name := t.new_temp('derived_clone_sum')
	t.pending_stmts << t.make_decl_assign_typed(out_name, stable_source, sum_type)
	for variant in variants {
		qvariant := t.resolve_variant(resolved_sum, variant)
		if qvariant.len == 0 {
			continue
		}
		use_ptr := t.variant_references_sum(qvariant, resolved_sum)
			&& !t.sum_variant_is_direct_pointer(qvariant)
		field_type := if use_ptr { '&${qvariant}' } else { qvariant }
		mut payload :=
			t.make_selector_op(stable_source, t.sum_field_name(qvariant), field_type, .dot)
		if use_ptr {
			payload = t.make_prefix(.mul, payload)
			t.set_node_typ(int(payload), qvariant)
		}
		pending_start := t.pending_stmts.len
		cloned_payload := t.make_compiler_default_clone_value(payload, qvariant, true)
		mut body := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		wrapped := t.make_sum_literal(resolved_sum, qvariant, cloned_payload)
		body << t.make_assign_without_ownership_drop(t.make_ident(out_name), wrapped)
		cond := t.make_sum_is_check(stable_source, sum_type, resolved_sum, qvariant)
		t.pending_stmts << t.make_if_with_skip_ownership_drops(cond, t.make_block_skip_scope_drops(body), t.make_empty())
	}
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), sum_type)
	return result
}

fn default_clone_helper_name(typ string) string {
	return '__v3_default_clone_${c_name(typ)}'
}

fn (mut t Transformer) request_default_clone_helper(source flat.NodeId, typ string) flat.NodeId {
	helper := default_clone_helper_name(typ)
	if typ !in t.default_clone_types {
		t.default_clone_types[typ] = DefaultCloneRequest{
			module: t.cur_module
			file:   t.cur_file
		}
	}
	t.mark_fn_used_name(helper)
	if t.expr_can_take_address(source) {
		address := t.runtime_addr(source, typ)
		argument := t.make_cast('voidptr', address, 'voidptr')
		return t.make_call_typed(helper, [argument], typ)
	}
	// The helper only borrows its pointer argument. Stabilize an owned rvalue ourselves so
	// the clone is saved before the original temporary is destroyed; runtime_addr's ordinary
	// compiler temporary is not tracked by ownership cleanup.
	source_name := t.new_temp('default_clone_source')
	stable_source := t.make_ident(source_name)
	t.pending_stmts << t.make_decl_assign_typed(source_name, source, typ)
	address := t.runtime_addr(stable_source, typ)
	argument := t.make_cast('voidptr', address, 'voidptr')
	cloned_name := t.new_temp('default_clone_result')
	t.pending_stmts << t.make_decl_assign_typed(cloned_name, t.make_call_typed(helper, [
		argument,
	], typ), typ)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
		stable_source,
	], 'void'))
	return t.make_ident(cloned_name)
}

// synthesize_default_clone_helpers drains recursive compiler-provided IClone
// requests after worker results have been merged. Building a helper can expose
// another recursive aggregate, so requests are processed as a worklist.
fn (mut t Transformer) synthesize_default_clone_helpers() []string {
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	was_log_active := t.used_fns_log_active
	log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	for {
		mut pending := []string{}
		for name, _ in t.default_clone_types {
			if name in t.default_clone_synthesized {
				continue
			}
			if default_clone_helper_name(name) in t.fn_ret_types {
				t.default_clone_synthesized[name] = true
				continue
			}
			pending << name
		}
		if pending.len == 0 {
			break
		}
		pending.sort()
		for name in pending {
			t.default_clone_synthesized[name] = true
			req := t.default_clone_types[name] or { DefaultCloneRequest{} }
			t.cur_module = req.module
			t.cur_file = req.file
			if !isnil(t.tc) {
				t.tc.cur_module = req.module
				t.tc.cur_file = req.file
			}
			t.build_default_clone_helper_fn(name)
		}
	}
	mut new_names := []string{}
	mut seen := map[string]bool{}
	for i in log_start .. t.used_fns_log.len {
		name := t.used_fns_log[i]
		if name.len > 0 && !seen[name] {
			seen[name] = true
			new_names << name
		}
	}
	if !was_log_active {
		t.used_fns_log_active = false
		t.used_fns_log = t.used_fns_log[..log_start].clone()
	}
	t.cur_module = old_module
	t.cur_file = old_file
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return new_names
}

fn (mut t Transformer) build_default_clone_helper_fn(typ string) {
	helper := default_clone_helper_name(typ)
	saved_pending := t.pending_stmts
	saved_vars := t.var_types.clone()
	saved_fn_name := t.cur_fn_name
	saved_ret_type := t.cur_fn_ret_type
	saved_expansion_stack := t.default_clone_expansion_stack.clone()
	t.pending_stmts = []flat.NodeId{}
	t.reset_var_types()
	t.default_clone_expansion_stack = []string{}
	t.cur_fn_name = helper
	t.cur_fn_ret_type = typ
	param_name := '__default_clone_source'
	param := t.a.add_node(flat.Node{
		kind:  .param
		value: param_name
		typ:   'voidptr'
	})
	t.set_var_type(param_name, 'voidptr')
	typed_pointer := t.make_cast('&${typ}', t.make_ident(param_name), '&${typ}')
	source := t.make_prefix(.mul, typed_pointer)
	t.set_node_typ(int(source), typ)
	// The helper body inlines the clone directly; for a sum type that means the variant
	// switch itself (make_compiler_default_clone_value would otherwise route straight back
	// to this helper and never emit a body). Nested owned payloads still recurse through the
	// helper, keeping every use site compact.
	cloned := if t.is_sum_type_name(typ) {
		t.make_compiler_default_sum_clone_value(source, typ)
	} else {
		t.make_compiler_default_clone_value(source, typ, false)
	}
	mut body := t.pending_stmts.clone()
	body << t.make_return(cloned, typ)
	t.pending_stmts = saved_pending
	t.restore_var_types(saved_vars)
	t.default_clone_expansion_stack = saved_expansion_stack
	t.cur_fn_name = saved_fn_name
	t.cur_fn_ret_type = saved_ret_type
	t.add_generated_fn_decl_context('main')
	start := t.a.children.len
	t.a.children << param
	t.a.children << body
	fn_decl := t.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          helper
		typ:            typ
		children_start: i32(start)
		children_count: flat.child_count(1 + body.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, 'main', t.cur_file)
	t.set_fn_ret_type(helper, typ)
	t.mark_fn_used_name(helper)
	if !isnil(t.tc) {
		// Detach the signature tables first: a parallel transform worker still shares
		// the master's maps here, so an in-place write would race with the other
		// workers and publish a key owned by this worker's disposable arena.
		t.tc.ensure_private_transform_signatures()
		t.tc.fn_ret_types[helper] = t.tc.parse_type(typ)
		t.tc.register_generated_fn_param_types(helper, [t.tc.parse_type('voidptr')])
		t.tc.fn_variadic[helper] = false
		t.tc_signature_names_log << helper
	}
}

// make_compiler_default_array_clone_value clones the array storage and then replaces
// each owning element with an independent clone. The initial element copies are not
// owners and are deliberately overwritten without being dropped. The caller classifies
// the source lifetime before transformation can make a temporary addressable.
fn (mut t Transformer) make_compiler_default_array_clone_value(source flat.NodeId, array_type string, source_is_owned_temporary bool) flat.NodeId {
	elem_type := array_type[2..]
	if !t.compiler_default_clone_type_needs_work(elem_type) {
		return t.make_array_clone_value(source, array_type)
	}
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type, 'derived_clone_array_source')
	out_name := t.new_temp('derived_clone_array')
	idx_name := t.new_temp('derived_clone_array_idx')
	t.mark_fn_used('array__clone')
	storage_clone := t.make_call_typed('array__clone', [
		t.runtime_addr(stable_source, array_type),
	], array_type)
	t.pending_stmts << t.make_decl_assign_typed(out_name, storage_clone, array_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(out_name), 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.array_get_value(stable_source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign_without_ownership_drop(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), elem_type), cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// make_compiler_default_fixed_array_clone_value copies the fixed-array storage and
// replaces each owning element with an independent clone. The initial element copies
// are non-owning and are overwritten without being dropped.
fn (mut t Transformer) make_compiler_default_fixed_array_clone_value(source flat.NodeId, raw_fixed_type string) flat.NodeId {
	fixed_type :=
		t.receiver_type_text_source_fixed_spelling(t.resolved_fixed_array_canonical_type(raw_fixed_type))
	elem_type := fixed_array_elem_type(fixed_type)
	if !t.compiler_default_clone_type_needs_work(elem_type) {
		return source
	}
	source_is_owned_temporary := !t.expr_can_take_address(source)
	stable_source := t.stable_transformed_expr_for_reuse(source, fixed_type, 'derived_clone_fixed_array_source')
	out_name := t.new_temp('derived_clone_fixed_array')
	idx_name := t.new_temp('derived_clone_fixed_array_idx')
	t.pending_stmts << t.make_decl_assign_typed(out_name, stable_source, fixed_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_fixed_array_len_expr(fixed_type))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.make_index(stable_source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign_without_ownership_drop(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), elem_type), cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), fixed_type)
	return result
}

// make_compiler_default_map_clone_value constructs a fresh map, recursively clones
// owning non-string keys and values, and lets map key callbacks clone string keys.
// The source lifetime is classified by the caller before transformation can turn a
// temporary literal into an addressable synthetic identifier.
fn (mut t Transformer) make_compiler_default_map_clone_value(source flat.NodeId, map_type string, source_is_owned_temporary bool) flat.NodeId {
	key_type, value_type := t.map_type_parts(map_type)
	clean_key_type := t.normalize_type_alias(key_type).trim_space()
	key_needs_clone := clean_key_type != 'string'
		&& t.compiler_default_clone_type_needs_work(key_type)
	value_needs_clone := t.compiler_default_clone_type_needs_work(value_type)
	stable_source := t.stable_transformed_expr_for_reuse(source, map_type, 'derived_clone_map_source')
	if key_type.len == 0 || value_type.len == 0 || (!key_needs_clone && !value_needs_clone) {
		t.mark_fn_used('map__clone')
		storage_clone := t.make_call_typed('map__clone', [
			t.runtime_addr(stable_source, map_type),
		], map_type)
		if !source_is_owned_temporary {
			return storage_clone
		}
		out_name := t.new_temp('derived_clone_map')
		t.pending_stmts << t.make_decl_assign_typed(out_name, storage_clone, map_type)
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
		result := t.make_ident(out_name)
		t.set_node_typ(int(result), map_type)
		return result
	}
	out_name := t.new_temp('derived_clone_map')
	key_name := t.new_temp('derived_clone_map_key')
	source_value_name := t.new_temp('derived_clone_map_source_value')
	cloned_key_name := t.new_temp('derived_clone_map_cloned_key')
	value_name := t.new_temp('derived_clone_map_value')
	t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_new_map_call(map_type), map_type)
	key_storage_type := t.map_key_storage_type(key_type)
	t.set_var_type(key_name, key_storage_type)
	t.set_var_type(source_value_name, value_type)
	pending_start := t.pending_stmts.len
	cloned_key := if key_needs_clone {
		t.make_compiler_default_clone_value(t.make_ident(key_name), key_type, true)
	} else {
		t.make_ident(key_name)
	}
	cloned_value := if value_needs_clone {
		t.make_compiler_default_clone_value(t.make_ident(source_value_name), value_type, true)
	} else {
		t.make_ident(source_value_name)
	}
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	map_key_name := if key_needs_clone { cloned_key_name } else { key_name }
	if key_needs_clone {
		body << t.make_decl_assign_typed(cloned_key_name, cloned_key, key_storage_type)
	}
	body << t.make_decl_assign_typed(value_name, cloned_value, value_type)
	body << t.make_map_set_stmt(t.make_ident(out_name), map_type, map_key_name, value_name)
	if clean_key_type == 'string' {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(key_name),
		], 'void'))
	}
	start := t.a.children.len
	t.a.children << t.make_ident(key_name)
	t.a.children << t.make_ident(source_value_name)
	t.a.children << stable_source
	for stmt in body {
		t.a.children << stmt
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind:           .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		value:          '3'
		flags:          flat.node_flag_skip_ownership_drops
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), map_type)
	return result
}

fn (t &Transformer) compiler_default_clone_field_type(owner string, field FieldInfo) string {
	return t.lookup_struct_field_type(owner, field.name) or {
		if field.typ.len > 0 { field.typ } else { field.raw_typ }
	}
}

fn (t &Transformer) compiler_default_clone_type_needs_work(typ string) bool {
	return t.compiler_default_clone_type_needs_work_seen(typ, []string{})
}

fn (t &Transformer) compiler_default_clone_type_needs_work_seen(typ string, seen []string) bool {
	clean := t.normalize_type_alias(typ).trim_space()
	if clean.len == 0 || clean.starts_with('&') || clean in seen {
		return false
	}
	if clean.starts_with('!') {
		return true
	}
	if clean.starts_with('?') {
		return true
	}
	if t.is_fixed_array_type(clean) {
		return t.compiler_default_clone_type_needs_work_seen(fixed_array_elem_type(clean), seen)
	}
	if clean == 'string' || clean.starts_with('[]') || clean.starts_with('map[') {
		return true
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean)
		if t.tc.ownership_type_requires_destruction(parsed)
			|| t.tc.named_type_implements_marker(clean, 'IClone') {
			return true
		}
	}
	if clean in t.structs || clean in t.sum_types {
		clone_name := '${clean}.clone'
		if clone_name in t.fn_ret_types || (!isnil(t.tc) && clone_name in t.tc.fn_ret_types) {
			return true
		}
	}
	// Ordinary structs can own collection storage even when ownership checking is
	// disabled. Inspect their fields so an array append does not silently copy a
	// pointer-backed map header instead of cloning the stored value.
	if info := t.lookup_struct_info(clean) {
		mut next_seen := seen.clone()
		next_seen << clean
		for field in info.fields {
			field_type := t.compiler_default_clone_field_type(clean, field)
			if t.compiler_default_clone_type_needs_work_seen(field_type, next_seen) {
				return true
			}
		}
	}
	return false
}

// try_lower_array_method_call supports try lower array method call handling for Transformer.
fn (mut t Transformer) try_lower_array_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	if fn_node.value == 'str' {
		if smartcast_call := t.try_lower_smartcast_target_receiver_method_call(call_id, node) {
			return smartcast_call
		}
		if smartcast_str := t.smartcast_sum_str_call(base_id) {
			return smartcast_str
