module transform

import v3.flat
import v3.types

// make_array_new_call builds make array new call data for transform.
fn (mut t Transformer) make_array_new_call(elem_type string, len_expr flat.NodeId, cap_expr flat.NodeId) flat.NodeId {
	return t.make_call_typed('array_new', arr3(t.make_sizeof_type(elem_type), len_expr, cap_expr),
		'[]${elem_type}')
}

fn shared_array_inner_type_text(raw string) ?string {
	mut clean := raw.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	if !clean.starts_with('[]') {
		return none
	}
	elem := clean[2..].trim_space()
	if elem.starts_with('shared ') {
		return elem[7..].trim_space()
	}
	return none
}

fn (t &Transformer) shared_array_lhs_inner_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return shared_array_inner_type_text(t.raw_var_type(node.value))
	} else if node.kind == .selector && node.children_count > 0 {
		base_id := t.a.child(&node, 0)
		mut base_type := t.raw_expr_type_without_smartcast(base_id)
		if base_type.len == 0 {
			base_type = t.original_expr_type(base_id)
		}
		raw, owner_type := t.lookup_struct_field_raw_type_with_owner(t.trim_pointer_type(base_type),
			node.value) or { return none }
		inner := shared_array_inner_type_text(raw) or { return none }
		return t.normalize_field_type(inner, owner_type)
	} else {
		return shared_array_inner_type_text(node.typ)
	}
}

fn (mut t Transformer) try_lower_array_repeat_call(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value != 'repeat' || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	count_id := t.a.child(&node, 1)
	if expanded := t.try_expand_interface_array_literal_repeat(base_id, count_id, base_type) {
		return expanded
	}
	clean_base_type := t.normalize_type_alias(base_type.trim_left('&'))
	if !isnil(t.tc) && clean_base_type.starts_with('[]') {
		elem_type := clean_base_type[2..]
		elem := t.tc.parse_type(elem_type)
		if t.tc.ownership_type_requires_destruction(elem) {
			// The checker reports the missing clone method. Do not lower the rejected
			// repeat to byte copies while processing the invalid program.
			if _ := t.tc.ownership_default_clone_missing_method(elem) {
				return t.make_empty()
			}
			return t.make_owned_array_repeat_value(base_id, count_id, clean_base_type)
		}
		base_is_owned_temporary := !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
		if base_is_owned_temporary {
			return t.make_plain_array_repeat_value(base_id, count_id, clean_base_type)
		}
	}
	depth := array_repeat_clone_depth(base_type)
	if depth == 0 {
		return none
	}
	base := t.transform_expr(base_id)
	count := t.transform_expr(count_id)
	selector := t.make_selector(base, 'repeat_to_depth', '')
	return t.make_call_expr_typed(selector, arr2(count, t.make_int_literal(depth)), node.typ)
}

// make_plain_array_repeat_value preserves the repeated result before destroying a
// non-addressable source array whose backing storage was materialized after ownership
// analysis.
fn (mut t Transformer) make_plain_array_repeat_value(base_id flat.NodeId, count_id flat.NodeId, array_type string) flat.NodeId {
	source := t.transform_expr(base_id)
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type,
		'plain_array_repeat_source')
	count := t.transform_expr_for_type(count_id, 'int')
	repeat_selector := t.make_selector(stable_source, 'repeat_to_depth', '')
	clone_depth := array_repeat_clone_depth(array_type)
	repeated := t.make_call_expr_typed(repeat_selector,
		arr2(count, t.make_int_literal(clone_depth)), array_type)
	out_name := t.new_temp('plain_array_repeat')
	t.pending_stmts << t.make_decl_assign_typed(out_name, repeated, array_type)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(stable_source), 'void'))
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// make_owned_array_repeat_value replaces every byte-copied element in the runtime repeat
// result with an independent clone before ownership destruction can observe the array.
fn (mut t Transformer) make_owned_array_repeat_value(base_id flat.NodeId, count_id flat.NodeId, array_type string) flat.NodeId {
	elem_type := array_type[2..]
	base_type := t.node_type(base_id)
	// Classify the source before transforming it because literals can lower to addressable
	// synthetic identifiers that still need explicit destruction.
	source_is_owned_temporary := !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
	mut source := t.transform_expr(base_id)
	if base_type.starts_with('&') {
		source = t.make_prefix(.mul, source)
		t.set_node_typ(int(source), array_type)
	}
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type,
		'owned_array_repeat_source')
	count := t.transform_expr_for_type(count_id, 'int')
	repeat_selector := t.make_selector(stable_source, 'repeat_to_depth', '')
	storage_repeat := t.make_call_expr_typed(repeat_selector, arr2(count, t.make_int_literal(0)),
		array_type)
	out_name := t.new_temp('owned_array_repeat')
	idx_name := t.new_temp('owned_array_repeat_idx')
	t.pending_stmts << t.make_decl_assign_typed(out_name, storage_repeat, array_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(out_name),
		'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	shallow_elem := t.array_get_value(t.make_ident(out_name), t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(shallow_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), elem_type),
		cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(stable_source),
			'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

fn (mut t Transformer) try_expand_interface_array_literal_repeat(base_id flat.NodeId, count_id flat.NodeId, base_type string) ?flat.NodeId {
	if !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	if elem_type !in t.tc.interface_names && t.tc.qualify_name(elem_type) !in t.tc.interface_names {
		return none
	}
	base := t.a.nodes[int(base_id)]
	count_node := t.a.nodes[int(count_id)]
	if base.kind != .array_literal || count_node.kind != .int_literal {
		return none
	}
	count := count_node.value.int()
	if count < 0 || count > 32 {
		return none
	}
	if !t.array_repeat_literal_can_duplicate(base) {
		return none
	}
	mut values := []flat.NodeId{cap: int(base.children_count) * count}
	for _ in 0 .. count {
		for i in 0 .. base.children_count {
			values << t.a.child(&base, i)
		}
	}
	lit := t.make_array_literal_typed(values, base_type)
	return t.transform_array_literal(lit, t.a.nodes[int(lit)])
}

fn (t &Transformer) array_repeat_literal_can_duplicate(node flat.Node) bool {
	for i in 0 .. node.children_count {
		if !t.array_repeat_expr_can_duplicate(t.a.child(&node, i)) {
			return false
		}
	}
	return true
}

fn (t &Transformer) array_repeat_expr_can_duplicate(id flat.NodeId) bool {
	node := t.a.nodes[int(id)]
	match node.kind {
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .ident,
		.enum_val, .nil_literal, .none_expr {
			return true
		}
		.paren, .prefix, .postfix, .cast_expr, .as_expr, .field_init, .array_literal, .struct_init {
			for i in 0 .. node.children_count {
				if !t.array_repeat_expr_can_duplicate(t.a.child(&node, i)) {
					return false
				}
			}
			return true
		}
		else {
			return false
		}
	}
}

fn array_repeat_clone_depth(typ string) int {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	mut depth := 0
	for clean.starts_with('[]') {
		depth++
		clean = clean[2..]
	}
	if depth <= 1 {
		return 0
	}
	return depth - 1
}

fn array_nested_eq_depth(typ string) int {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	mut depth := 0
	for clean.starts_with('[]') {
		depth++
		clean = clean[2..]
	}
	if depth <= 1 {
		return 1
	}
	return depth
}

fn (mut t Transformer) make_array_push_many_call(lhs_addr flat.NodeId, rhs flat.NodeId, rhs_type string) flat.NodeId {
	t.mark_fn_used('array__push_many')
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'push_many')
	return t.make_call_typed('array__push_many', arr3(lhs_addr, t.make_selector(rhs_value, 'data',
		'voidptr'), t.make_selector(rhs_value, 'len', 'int')), 'void')
}

fn (mut t Transformer) make_array_insert_many_call(lhs_addr flat.NodeId, index flat.NodeId, rhs flat.NodeId, rhs_type string) flat.NodeId {
	if t.is_fixed_array_type(rhs_type) {
		return t.make_call_typed('array__insert_many', arr4(lhs_addr, index, rhs,
			t.make_fixed_array_len_expr(rhs_type)), 'void')
	}
	rhs_value := t.stable_transformed_expr_for_reuse(rhs, rhs_type, 'insert_many')
	return t.make_call_typed('array__insert_many', arr4(lhs_addr, index, t.make_selector(rhs_value,
		'data', 'voidptr'), t.make_selector(rhs_value, 'len', 'int')), 'void')
}

fn (mut t Transformer) make_array_clone_call(base_id flat.NodeId, base_type string) flat.NodeId {
	t.mark_fn_used('array__clone')
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	// Classify the source before transformation can lower a literal to an addressable temp.
	source_is_owned_temporary := clean_type.starts_with('[]') && !isnil(t.tc)
		&& !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type))
	mut receiver := t.transform_expr(base_id)
	if clean_type.starts_with('[]') && !isnil(t.tc) {
		elem_type := t.tc.parse_type(clean_type[2..])
		if !t.tc.ownership_type_requires_destruction(elem_type) {
			if source_is_owned_temporary {
				return t.make_array_clone_from_owned_temporary(receiver, clean_type)
			}
			return t.make_array_clone_value(receiver, base_type)
		}
		// The checker rejects this call. Do not lower it to the unsafe raw clone while
		// processing the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(elem_type) {
			return receiver
		}
		if base_type.starts_with('&') {
			receiver = t.make_prefix(.mul, receiver)
			t.set_node_typ(int(receiver), clean_type)
		}
		return t.make_compiler_default_array_clone_value(receiver, clean_type,
			source_is_owned_temporary)
	}
	return t.make_array_clone_value(receiver, base_type)
}

// make_array_clone_from_owned_temporary saves a non-addressable source until its backing
// storage has been cloned, then destroys the generated source temp before returning the clone.
fn (mut t Transformer) make_array_clone_from_owned_temporary(source flat.NodeId, array_type string) flat.NodeId {
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type, 'array_clone_source')
	out_name := t.new_temp('array_clone')
	cloned := t.make_array_clone_value(stable_source, array_type)
	t.pending_stmts << t.make_decl_assign_typed(out_name, cloned, array_type)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(stable_source), 'void'))
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// make_array_reverse_call clones array storage and ownership-bearing elements before
// reversing the new array in place, so the source and result never share ownership.
fn (mut t Transformer) make_array_reverse_call(base_id flat.NodeId, base_type string) flat.NodeId {
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if clean_type.starts_with('[]') && !isnil(t.tc) {
		elem_type := t.tc.parse_type(clean_type[2..])
		if t.tc.ownership_type_requires_destruction(elem_type) {
			// The checker rejects this call. Do not mutate the source while processing the
			// invalid program.
			if _ := t.tc.ownership_default_clone_missing_method(elem_type) {
				mut receiver := t.transform_expr(base_id)
				if base_type.starts_with('&') {
					receiver = t.make_prefix(.mul, receiver)
					t.set_node_typ(int(receiver), clean_type)
				}
				return receiver
			}
		}
		clone := t.make_array_clone_call(base_id, base_type)
		stable_clone := t.stable_transformed_expr_for_reuse(clone, clean_type, 'owned_reverse')
		t.mark_fn_used('array__reverse_in_place')
		reverse := t.make_call_typed('array__reverse_in_place', arr1(t.runtime_addr(stable_clone,
			clean_type)), 'void')
		t.pending_stmts << t.make_expr_stmt(reverse)
		return stable_clone
	}
	mut receiver := t.transform_expr(base_id)
	if base_type.starts_with('&') {
		receiver = t.make_prefix(.mul, receiver)
		t.set_node_typ(int(receiver), clean_type)
	}
	return t.make_call_typed('array__reverse', arr1(receiver), clean_type)
}

fn (mut t Transformer) make_array_clone_value(receiver flat.NodeId, base_type string) flat.NodeId {
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	depth := array_repeat_clone_depth(clean_type)
	if depth > 0 {
		return t.make_call_typed('array__clone_to_depth', arr2(t.runtime_addr(receiver, base_type),
			t.make_int_literal(depth)), clean_type)
	}
	return t.make_call_typed('array__clone', arr1(t.runtime_addr(receiver, base_type)), clean_type)
}

fn (mut t Transformer) clone_nested_array_spread_value(spread flat.NodeId, spread_type string) flat.NodeId {
	if array_repeat_clone_depth(t.normalize_type_alias(spread_type)) > 0 {
		return t.make_array_clone_value(spread, spread_type)
	}
	return spread
}

fn (t &Transformer) array_init_elem_type_name(id flat.NodeId, node flat.Node) string {
	if node.value.starts_with('typeof(') {
		array_type := t.node_type(id)
		if array_type.starts_with('[]') {
			return array_type[2..]
		}
	}
	return node.value
}

// lower_array_init_to_runtime converts lower array init to runtime data for transform.
fn (mut t Transformer) lower_array_init_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	elem_value := t.array_init_elem_type_name(id, node)
	if elem_value.len == 0 {
		return id
	}
	clean_value := t.normalize_type_alias(elem_value)
	if t.is_fixed_array_type(clean_value) && !node.typ.starts_with('[]') {
		return id
	}
	elem_type := if node.typ.starts_with('[]') {
		node.typ[2..]
	} else if !elem_value.starts_with('[]') && clean_value.starts_with('[]') {
		clean_value[2..]
	} else {
		elem_value
	}
	mut len_expr := t.make_int_literal(0)
	mut cap_expr := t.make_int_literal(0)
	mut init_expr := flat.empty_node
	mut init_expr_id := flat.empty_node
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .field_init && child.children_count > 0 {
			if child.value == 'len' {
				val := t.transform_expr(t.a.child(child, 0))
				len_expr = val
			} else if child.value == 'cap' {
				val := t.transform_expr(t.a.child(child, 0))
				cap_expr = val
			} else if child.value == 'init' {
				init_expr_id = t.a.child(child, 0)
			}
		}
	}
	new_call := t.make_array_new_call(elem_type, len_expr, cap_expr)
	if node.children_count == 0 {
		return new_call
	}
	if int(init_expr_id) < 0 {
		clean_elem_type := t.normalize_type_alias(elem_type)
		if clean_elem_type.starts_with('[]') {
			init_expr = t.make_array_new_call(clean_elem_type[2..], t.make_int_literal(0),
				t.make_int_literal(0))
		} else if clean_elem_type.starts_with('map[') {
			init_expr = t.zero_value_for_type(clean_elem_type)
		} else if default_value := t.make_struct_runtime_default_value(clean_elem_type) {
			init_expr = default_value
		} else {
			return new_call
		}
	}
	tmp_name := t.new_temp('arr_init')
	idx_name := t.new_temp('arr_idx')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, new_call, '[]${elem_type}')
	init_idx := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(tmp_name),
		'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_lhs := t.make_index(t.make_ident(tmp_name), t.make_ident(idx_name), elem_type)
	// `init:` expressions may reference the magic `index` variable, which V binds to
	// the current element index. Declare it inside the loop body so it resolves to the
	// generated loop counter instead of leaking to an external symbol (e.g. libc `index`).
	index_decl := t.make_decl_assign_typed('index', t.make_ident(idx_name), 'int')
	mut loop_body := []flat.NodeId{}
	loop_body << index_decl
	if int(init_expr_id) >= 0 {
		saved_pending := t.pending_stmts.clone()
		t.pending_stmts.clear()
		indexed_init := t.substitute_ident_expr(init_expr_id, 'index', t.make_ident(idx_name))
		init_expr = t.transform_expr(indexed_init)
		init_pending := t.pending_stmts.clone()
		t.pending_stmts = saved_pending
		for stmt in init_pending {
			loop_body << stmt
		}
	}
	mut assign_value := init_expr
	clean_elem_type := t.normalize_type_alias(elem_type)
	if clean_elem_type.starts_with('[]') {
		if !t.expr_can_take_address(assign_value) {
			value_name := t.new_temp('arr_init_val')
			loop_body << t.make_decl_assign_typed(value_name, assign_value, clean_elem_type)
			assign_value = t.make_ident(value_name)
		}
		assign_value = t.make_array_clone_value(assign_value, clean_elem_type)
	}
	assign := t.make_assign(elem_lhs, assign_value)
	loop_body << assign
	t.pending_stmts << t.make_for_stmt(init_idx, cond, post, loop_body, node)
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), '[]${elem_type}')
	return result
}

fn (mut t Transformer) make_struct_runtime_default_value(struct_type string) ?flat.NodeId {
	mut visited := map[string]bool{}
	return t.make_struct_runtime_default_value_guarded(struct_type, mut visited)
}

fn (mut t Transformer) make_struct_runtime_default_value_guarded(struct_type string, mut visited map[string]bool) ?flat.NodeId {
	// A reference-typed field defaults to nil, which the zeroed element already
	// is; expanding it would build `(T*){<struct defaults>}` — invalid C
	// (lookup_struct_info resolves `&mod.T` texts through its direct fallback).
	if struct_type.starts_with('&') {
		return none
	}
	if t.resolve_sum_name(struct_type) in t.sum_types {
		return none
	}
	// Name lookups can resolve cycles (e.g. same-named types across modules), so guard
	// the current expansion path against re-entering a type.
	if struct_type in visited {
		return none
	}
	visited[struct_type] = true
	defer {
		visited.delete(struct_type)
	}
	info := t.lookup_struct_info(struct_type) or { return none }
	mut field_ids := []flat.NodeId{}
	old_module := t.cur_module
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	defer {
		t.cur_module = old_module
	}
	for field in info.fields {
		field_type := t.lookup_struct_field_type(struct_type, field.name) or {
			if field.typ.len > 0 { field.typ } else { field.raw_typ }
		}
		clean_type := t.normalize_type_alias(field_type)
		mut value := flat.empty_node
		if int(field.default_expr) >= 0 {
			default_node := t.a.nodes[int(field.default_expr)]
			enum_field_type := t.enum_type_name_for_expected(field_type, info.module)
			sum_field_type := t.struct_field_sum_type(field_type, info.module)
			value = if default_node.kind == .enum_val && enum_field_type.len > 0 {
				t.transform_enum_shorthand(field.default_expr, default_node, enum_field_type)
			} else if sum_field_type.len > 0 {
				t.wrap_sum_value(field.default_expr, sum_field_type)
			} else {
				t.transform_expr_for_type(field.default_expr, field_type)
			}
		} else if clean_type.starts_with('map[') || clean_type.starts_with('[]') {
			value = t.zero_value_for_type(clean_type)
		} else if nested := t.make_struct_runtime_default_value_guarded(clean_type, mut visited) {
			value = nested
		}
		if int(value) < 0 {
			continue
		}
		start := t.a.children.len
		t.a.children << value
		field_ids << t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: start
			children_count: 1
			value:          field.name
			typ:            field_type
		})
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value:          struct_type
		typ:            struct_type
	})
}

// lower_array_literal_to_runtime converts lower array literal to runtime data for transform.
fn (mut t Transformer) lower_array_literal_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	if t.in_const_init {
		return id
	}
	array_type := if elem_type := t.array_literal_pointer_value_elem_type(node) {
		'[]${elem_type}'
	} else if checker_alias_type := t.array_literal_checker_alias_type(id) {
		checker_alias_type
	} else if alias_type := t.array_literal_alias_type(node) {
		alias_type
	} else {
		t.node_type(id)
	}
	if !array_type.starts_with('[]') {
		return id
	}
	elem_type := array_type[2..]
	tmp_name := t.new_temp('arr_lit')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(elem_type,
		t.make_int_literal(0), t.make_int_literal(node.children_count)), array_type)
	for i in 0 .. node.children_count {
		elem_id := t.a.child(&node, i)
		elem := t.a.nodes[int(elem_id)]
		if elem.kind == .prefix && elem.value == '...' && elem.children_count > 0 {
			spread_id := t.a.child(&elem, 0)
			t.append_array_literal_spread(tmp_name, spread_id, array_type, elem_type)
			continue
		}
		value_name := t.new_temp('arr_val')
		value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
			t.wrap_sum_value(elem_id, elem_type)
		} else {
			t.transform_expr_for_type(elem_id, elem_type)
		}
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
		call := t.make_call_typed('array_push', arr2(t.make_prefix(.amp, t.make_ident(tmp_name)), t.make_prefix(.amp,
			t.make_ident(value_name))), 'void')
		t.pending_stmts << t.make_expr_stmt(call)
	}
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// append_array_literal_spread appends independent element clones when the destination
// array will own and destroy its elements. Plain-data spreads keep the runtime bulk copy.
fn (mut t Transformer) append_array_literal_spread(out_name string, spread_id flat.NodeId, array_type string, elem_type string) {
	source_is_owned_temporary := !t.expr_can_take_address(spread_id) && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(array_type))
	mut needs_element_clone := false
	if !isnil(t.tc) {
		elem := t.tc.parse_type(elem_type)
		needs_element_clone = t.tc.ownership_type_requires_destruction(elem)
		if needs_element_clone {
			// The checker reports the missing clone method. Do not append shallow element
			// copies while processing the invalid spread.
			if _ := t.tc.ownership_default_clone_missing_method(elem) {
				return
			}
		}
	}
	mut spread := t.transform_expr_for_type(spread_id, array_type)
	spread_type := if t.node_type(spread_id).len > 0 {
		t.node_type(spread_id)
	} else {
		array_type
	}
	if !needs_element_clone {
		spread = t.clone_nested_array_spread_value(spread, spread_type)
		stable_source := t.stable_transformed_expr_for_reuse(spread, array_type,
			'array_spread_source')
		call := t.make_array_push_many_call(t.make_prefix(.amp, t.make_ident(out_name)),
			stable_source, array_type)
		t.pending_stmts << t.make_expr_stmt(call)
		if source_is_owned_temporary {
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned',
				arr1(stable_source), 'void'))
		}
		return
	}
	stable_source := t.stable_transformed_expr_for_reuse(spread, array_type,
		'owned_array_spread_source')
	idx_name := t.new_temp('owned_array_spread_idx')
	value_name := t.new_temp('owned_array_spread_value')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(stable_source, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.array_get_value(stable_source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_decl_assign_typed(value_name, cloned_elem, elem_type)
	body << t.make_expr_stmt(t.make_call_typed('array_push', arr2(t.make_prefix(.amp,
		t.make_ident(out_name)), t.make_prefix(.amp, t.make_ident(value_name))), 'void'))
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(stable_source),
			'void'))
	}
}

fn (t &Transformer) array_literal_checker_alias_type(id flat.NodeId) ?string {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	typ := t.tc.expr_type(id) or { t.tc.resolve_type(id) }
	name := typ.name()
	if !name.starts_with('[]') {
		return none
	}
	elem := name[2..]
	if local_elem := t.array_literal_local_struct_elem_name(t.a.nodes[int(id)]) {
		if elem.all_after_last('.') == local_elem {
			return '[]${local_elem}'
		}
	}
	if !t.generic_arg_is_alias_name(elem, t.cur_module) {
		return none
	}
	return '[]${t.array_literal_qualified_alias_name(elem)}'
}

fn (t &Transformer) array_literal_local_struct_elem_name(node flat.Node) ?string {
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	first_id := t.array_literal_alias_expr_id(t.a.child(&node, 0))
	first := t.a.nodes[int(first_id)]
	if first.kind == .struct_init {
		for candidate in [first.value, first.typ] {
			if t.bare_struct_name_is_local_to_current_module(candidate) {
				return candidate
			}
		}
	}
	if first.kind == .ident {
		raw_type := t.raw_var_type(first.value)
		if t.bare_struct_name_is_local_to_current_module(raw_type) {
			return raw_type
		}
	}
	return none
}

fn (t &Transformer) array_literal_qualified_alias_name(name string) string {
	clean := name.trim_space()
	if clean.len == 0 || isnil(t.tc) {
		return clean
	}
	if clean in t.tc.type_aliases {
		return clean
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.tc.type_aliases {
			return qname
		}
	}
	if clean.contains('.') {
		return clean
	}
	mut found := ''
	for alias, _ in t.tc.type_aliases {
		if alias.all_after_last('.') != clean {
			continue
		}
		if found.len > 0 && found != alias {
			return clean
		}
		found = alias
	}
	if found.len > 0 {
		return found
	}
	return clean
}

fn (t &Transformer) array_literal_alias_type(node flat.Node) ?string {
	if node.kind != .array_literal || node.children_count == 0 {
		return none
	}
	first_id := t.array_literal_alias_expr_id(t.a.child(&node, 0))
	first := t.a.nodes[int(first_id)]
	if first.kind == .prefix && first.value == '...' {
		return none
	}
	mut alias_name := t.raw_alias_type_for_expr(first_id)
	if alias_name.len == 0 {
		alias_name = t.array_literal_alias_expr_name(first)
	}
	if alias_name.len == 0 {
		return none
	}
	if !t.generic_arg_is_alias_name(alias_name, t.cur_module) {
		return none
	}
	return '[]${t.array_literal_qualified_alias_name(alias_name)}'
}

fn (t &Transformer) array_literal_alias_expr_id(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.array_literal_alias_expr_id(t.a.child(&node, 0))
	}
	return id
}

fn (t &Transformer) array_literal_alias_expr_name(node flat.Node) string {
	if node.kind in [.cast_expr, .as_expr] && node.value.len > 0 {
		return node.value
	}
	if node.kind == .call && node.children_count > 0 {
		name := t.generic_call_type_arg_name(t.a.child(&node, 0))
		if name.len > 0 {
			return name
		}
	}
	for candidate in [node.typ, node.value] {
		if candidate.len > 0 && t.generic_arg_is_alias_name(candidate, t.cur_module) {
			return candidate
		}
	}
	return ''
}

// transform_array_literal_for_type transforms transform array literal for type data for transform.
fn (mut t Transformer) transform_array_literal_for_type(id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if t.in_const_init {
		return none
	}
	target_array_type := t.normalize_type_alias(target_type)
	array_type := if target_array_type.starts_with('[]')
		&& t.is_sum_type_name(target_array_type[2..]) {
		target_array_type
	} else if checker_alias_type := t.array_literal_checker_alias_type(id) {
		checker_alias_type
	} else if alias_type := t.array_literal_alias_type(node) {
		alias_type
	} else {
		target_array_type
	}
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	tmp_name := t.new_temp('arr_lit')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(elem_type,
		t.make_int_literal(0), t.make_int_literal(node.children_count)), array_type)
	for i in 0 .. node.children_count {
		elem_id := t.a.child(&node, i)
		elem := t.a.nodes[int(elem_id)]
		if elem.kind == .prefix && elem.value == '...' && elem.children_count > 0 {
			t.append_array_literal_spread(tmp_name, t.a.child(&elem, 0), array_type, elem_type)
			continue
		}
		value_name := t.new_temp('arr_val')
		value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
			t.wrap_sum_value(elem_id, elem_type)
		} else {
			t.transform_expr_for_type(elem_id, elem_type)
		}
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
		call := t.make_call_typed('array_push', arr2(t.make_prefix(.amp, t.make_ident(tmp_name)), t.make_prefix(.amp,
			t.make_ident(value_name))), 'void')
		t.pending_stmts << t.make_expr_stmt(call)
	}
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), array_type)
	return result
}

fn (mut t Transformer) transform_fixed_array_literal_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	fixed_type := t.normalize_type_alias(target_type)
	if !t.is_fixed_array_type(fixed_type) {
		return none
	}
	elem_type := fixed_array_elem_type(fixed_type)
	ordered_temps := t.fixed_array_literal_needs_ordered_temps(node)
	mut values := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		elem_id := t.a.child(&node, i)
		value := t.transform_expr_for_type(elem_id, elem_type)
		if ordered_temps {
			tmp_name := t.new_temp('fixed_arr_val')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, elem_type)
			values << t.make_ident(tmp_name)
			continue
		}
		values << value
	}
	return t.make_array_literal_typed(values, fixed_type)
}

fn (t &Transformer) fixed_array_literal_needs_ordered_temps(node flat.Node) bool {
	for i in 0 .. node.children_count {
		if t.fixed_array_literal_child_needs_ordered_temp(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) fixed_array_literal_child_needs_ordered_temp(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.if_expr, .match_stmt, .block, .or_expr] {
		return true
	}
	for i in 0 .. node.children_count {
		if t.fixed_array_literal_child_needs_ordered_temp(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) transform_fixed_array_init_expr(node flat.Node) ?flat.NodeId {
	fixed_type := t.resolved_fixed_array_canonical_type(t.normalize_type_alias(if node.typ.len > 0 {
		node.typ
	} else {
		node.value
	}))
	if !t.is_fixed_array_type(fixed_type) {
		return none
	}
	len := t.fixed_array_len_value(fixed_type) or { return none }
	elem_type := fixed_array_elem_type(fixed_type)
	if node.children_count == 0 {
		if !t.fixed_array_empty_init_needs_values(elem_type) {
			return none
		}
		return t.make_fixed_array_empty_literal(fixed_type, len, elem_type)
	}
	mut init_id := flat.empty_node
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.value == 'init' && child.children_count > 0 {
			init_id = t.a.child(&child, 0)
			break
		}
	}
	if int(init_id) < 0 {
		return none
	}
	mut values := []flat.NodeId{cap: len}
	for i in 0 .. len {
		indexed_init := t.substitute_ident_expr(init_id, 'index', t.make_int_literal(i))
		values << t.transform_expr_for_type(indexed_init, elem_type)
	}
	return t.make_array_literal_typed(values, fixed_type)
}

fn (mut t Transformer) fixed_array_len_value(fixed_type string) ?int {
	len_text := fixed_array_len_text(fixed_type)
	if is_decimal_text(len_text) {
		return len_text.int()
	}
	if !isnil(t.tc) {
		return t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) or { none }
	}
	return none
}

fn (mut t Transformer) make_fixed_array_empty_literal(fixed_type string, len int, elem_type string) flat.NodeId {
	mut values := []flat.NodeId{cap: len}
	for _ in 0 .. len {
		values << t.fixed_array_empty_elem_value(elem_type)
	}
	return t.make_array_literal_typed(values, fixed_type)
}

fn (mut t Transformer) fixed_array_empty_elem_value(elem_type string) flat.NodeId {
	clean_type := t.normalize_type_alias(elem_type)
	if t.is_fixed_array_type(clean_type) {
		fixed_type := t.resolved_fixed_array_canonical_type(clean_type)
		if len := t.fixed_array_len_value(fixed_type) {
			return t.make_fixed_array_empty_literal(fixed_type, len,
				fixed_array_elem_type(fixed_type))
		}
		return t.make_fixed_array_init(fixed_type)
	}
	if clean_type.starts_with('[]') {
		return t.make_array_init(clean_type[2..])
	}
	if clean_type.starts_with('map[') || clean_type.starts_with('chan ') {
		return t.zero_value_for_type(clean_type)
	}
	if default_value := t.make_struct_runtime_default_value(clean_type) {
		return default_value
	}
	return t.zero_value_for_type(clean_type)
}

fn (mut t Transformer) fixed_array_empty_init_needs_values(elem_type string) bool {
	mut visited := map[string]bool{}
	return t.fixed_array_empty_init_needs_values_guarded(elem_type, mut visited)
}

fn (mut t Transformer) fixed_array_empty_init_needs_values_guarded(elem_type string, mut visited map[string]bool) bool {
	clean_type := t.normalize_type_alias(elem_type)
	if clean_type.starts_with('[]') || clean_type.starts_with('map[')
		|| clean_type.starts_with('chan ') {
		return true
	}
	if t.is_fixed_array_type(clean_type) {
		fixed_type := t.resolved_fixed_array_canonical_type(clean_type)
		return t.fixed_array_empty_init_needs_values_guarded(fixed_array_elem_type(fixed_type), mut
			visited)
	}
	return t.struct_type_needs_runtime_default(clean_type, mut visited)
}

fn (mut t Transformer) struct_type_needs_runtime_default(struct_type string, mut visited map[string]bool) bool {
	if struct_type.starts_with('&') || t.resolve_sum_name(struct_type) in t.sum_types {
		return false
	}
	if struct_type in visited {
		return false
	}
	visited[struct_type] = true
	defer {
		visited.delete(struct_type)
	}
	info := t.lookup_struct_info(struct_type) or { return false }
	old_module := t.cur_module
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	defer {
		t.cur_module = old_module
	}
	for field in info.fields {
		if int(field.default_expr) >= 0 {
			return true
		}
		field_type := t.lookup_struct_field_type(struct_type, field.name) or {
			if field.typ.len > 0 { field.typ } else { field.raw_typ }
		}
		if t.fixed_array_empty_init_needs_values_guarded(field_type, mut visited) {
			return true
		}
	}
	return false
}

// transform_empty_array_init_for_type supports transform_empty_array_init_for_type handling.
fn (mut t Transformer) transform_empty_array_init_for_type(node flat.Node, target_type string) ?flat.NodeId {
	if node.value.len > 0 || node.children_count > 0 {
		return none
	}
	array_type := t.normalize_type_alias(target_type)
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	return t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(0))
}

fn (mut t Transformer) transform_array_value_for_dynamic_target(value_id flat.NodeId, target_type string) ?flat.NodeId {
	if int(value_id) < 0 || target_type.len == 0 || isnil(t.tc) {
		return none
	}
	expected_name := t.normalize_type_alias(target_type).trim_space()
	if !expected_name.starts_with('[]') {
		return none
	}
	actual_name := t.normalize_type_alias(t.node_type(value_id)).trim_space()
	if actual_name.len == 0 {
		return none
	}
	expected_type := t.tc.parse_type(expected_name)
	actual_type := t.tc.parse_type(actual_name)
	expected_base := forwarded_return_unalias_type(expected_type)
	actual_base := forwarded_return_unalias_type(actual_type)
	if expected_base is types.Array {
		if actual_base is types.Array {
			if !t.forwarded_slot_conversion_supported(actual_base.elem_type,
				expected_base.elem_type) {
				return none
			}
			return t.convert_forwarded_array_to_dynamic(value_id, actual_type,
				actual_base.elem_type, expected_type, expected_base.elem_type, false)
		}
		if actual_base is types.ArrayFixed {
			if !t.forwarded_slot_conversion_supported(actual_base.elem_type,
				expected_base.elem_type) {
				return none
			}
			return t.convert_forwarded_array_to_dynamic(value_id, actual_type,
				actual_base.elem_type, expected_type, expected_base.elem_type, true)
		}
	}
	return none
}

// try_lower_array_append_stmt supports try lower array append stmt handling for Transformer.
fn (mut t Transformer) try_lower_array_append_or_stmt(node flat.Node) ?[]flat.NodeId {
	if node.kind != .or_expr || node.children_count < 2 {
		return none
	}
	append_id := t.a.child(&node, 0)
	append := t.a.nodes[int(append_id)]
	if append.kind != .infix || append.op != .left_shift || append.children_count < 2 {
		return none
	}
	rhs_id := t.a.child(&append, 1)
	expr_type, value_type := t.or_expr_types(rhs_id, node.typ)
	if !t.is_optional_type_name(expr_type) || value_type.len == 0 || value_type == 'void' {
		return none
	}
	or_start := t.a.children.len
	t.a.children << rhs_id
	t.a.children << t.a.child(&node, 1)
	rhs_or_id := t.a.add_node(flat.Node{
		kind:           .or_expr
		op:             node.op
		children_start: or_start
		children_count: 2
		pos:            node.pos
		value:          node.value
		typ:            value_type
	})
	pending_start := t.pending_stmts.len
	unwrapped_rhs := t.lower_or_expr_to_temp(rhs_or_id, t.a.nodes[int(rhs_or_id)])
	rhs_pending := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	append_start := t.a.children.len
	t.a.children << t.a.child(&append, 0)
	t.a.children << unwrapped_rhs
	lowered_id := t.a.add_node(flat.Node{
		kind:           .infix
		op:             .left_shift
		children_start: append_start
		children_count: 2
		pos:            append.pos
		value:          append.value
		typ:            append.typ
	})
	if lowered := t.try_lower_map_index_append_stmt_with_prelude(lowered_id, rhs_pending) {
		return lowered
	}
	for stmt in rhs_pending {
		t.pending_stmts << stmt
	}
	if lowered := t.try_lower_shared_array_append_autolock_stmt(lowered_id) {
		return lowered
	}
	if lowered := t.try_lower_array_append_stmt(lowered_id) {
		return lowered
	}
	// This helper is only a probe. A non-array `<<` expression must fall back to
	// normal `or` lowering without retaining the optional RHS prelude here.
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	return none
}

fn (mut t Transformer) try_lower_array_append_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	normalized_id := t.normalize_array_append_add_rhs(id)
	if int(normalized_id) < 0 {
		return none
	}
	node := t.a.nodes[int(normalized_id)]
	if node.kind != .infix || node.op != .left_shift || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	if lowered := t.try_lower_optional_array_append_stmt(node, lhs_id, rhs_id) {
		return lowered
	}
	mut lhs_type := t.lvalue_type(lhs_id)
	if !array_type_has_generic_placeholder(lhs_type) {
		lhs_type = t.normalize_type_alias(lhs_type)
	}
	mut array_type := t.clean_array_append_lhs_type(lhs_type)
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	raw_rhs_type := t.node_type(rhs_id)
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	rhs_node := t.a.nodes[int(rhs_id)]
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	rhs_is_sum_variant := t.array_append_rhs_is_sum_variant_value(rhs_id, raw_rhs_type, elem_type)
	if push_many && rhs_is_sum_variant {
		push_many = false
	}
	if rhs_node.kind == .array_literal && t.array_append_literal_should_push_many(rhs_id, elem_type) {
		// `[]scalar << [a, b, c]` always appends the literal's elements. Retype the
		// literal from the destination so a mis-inferred element type (e.g. `[]int`
		// for `[f32_expr, ..]`) is corrected and the append stays a clean push_many,
		// instead of degrading to a single push of the whole array. (An array-typed
		// element is genuinely ambiguous, so leave that to the inferred decision.)
		push_many = true
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	} else if push_many && rhs_node.kind == .array_literal && !rhs_type.starts_with('[]') {
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	}

	mut result := []flat.NodeId{}
	lhs := t.transform_lvalue(lhs_id)
	t.drain_pending(mut result)
	mut rhs := flat.empty_node
	if !push_many {
		if !rhs_is_sum_variant {
			if t.array_append_elem_is_interface(elem_type) {
				rhs = t.transform_expr_for_type(rhs_id, elem_type)
			} else {
				if converted := t.transform_array_value_for_dynamic_target(rhs_id, array_type) {
					rhs = converted
					rhs_type = array_type
					push_many = true
				} else {
					rhs = if elem_type in t.sum_types
						|| t.resolve_sum_name(elem_type) in t.sum_types {
						t.wrap_sum_value(rhs_id, elem_type)
					} else {
						t.transform_expr_for_type(rhs_id, elem_type)
					}
				}
			}
		} else {
			rhs = if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
				t.wrap_sum_value(rhs_id, elem_type)
			} else {
				t.transform_expr_for_type(rhs_id, elem_type)
			}
		}
	} else {
		rhs = t.transform_expr(rhs_id)
	}
	if !push_many {
		rhs = t.coerce_transformed_expr_to_type(rhs, rhs_id, elem_type)
	}
	t.drain_pending(mut result)
	if rhs_type.len == 0 {
		rhs_type = t.node_type(rhs)
		push_many = t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	}

	lhs_addr := t.runtime_addr(lhs, lhs_type)
	if push_many {
		call := if t.is_fixed_array_type(rhs_type) {
			t.make_call_typed('array_push_many_ptr', arr3(lhs_addr, rhs,
				t.make_fixed_array_len_expr(rhs_type)), 'void')
		} else {
			t.make_array_push_many_call(lhs_addr, rhs, rhs_type)
		}
		t.drain_pending(mut result)
		result << t.make_expr_stmt(call)
		return result
	}
	value_name := t.new_temp('arr_val')
	value_type := t.shared_array_lhs_inner_type(lhs_id) or { elem_type }
	result << t.make_decl_assign_typed(value_name, rhs, value_type)
	push_call := t.make_call_typed('array_push', arr2(lhs_addr, t.make_prefix(.amp,
		t.make_ident(value_name))), 'void')
	if shared_inner := t.shared_array_lhs_inner_type(lhs_id) {
		t.set_node_value(int(push_call), 'shared_array_push:${shared_inner}')
	}
	result << t.make_expr_stmt(push_call)
	return result
}

// normalize_array_append_add_rhs restores the append-specific grouping of
// `items << value + suffix` after `<<` gained its numeric shift precedence.
// Numeric shifts remain grouped before `+`; only a statement whose left-shift
// lhs is known to be an array is rotated to `items << (value + suffix)`.
fn (mut t Transformer) normalize_array_append_add_rhs(id flat.NodeId) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind != .infix || node.children_count < 2 {
		return id
	}
	if node.op == .left_shift {
		lhs_id := t.a.child(&node, 0)
		lhs_type := t.clean_array_append_lhs_type(t.lvalue_type(lhs_id))
		return if lhs_type.starts_with('[]') { id } else { flat.empty_node }
	}
	if node.op !in [.plus, .minus] {
		return flat.empty_node
	}
	lhs_id := t.a.child(&node, 0)
	append_id := t.normalize_array_append_add_rhs(lhs_id)
	if int(append_id) < 0 {
		return flat.empty_node
	}
	append := t.a.nodes[int(append_id)]
	append_rhs := t.a.child(&append, 1)
	new_rhs_start := t.a.children.len
	t.a.children << append_rhs
	t.a.children << t.a.child(&node, 1)
	new_rhs := t.a.add_node(flat.Node{
		kind:           .infix
		op:             node.op
		children_start: new_rhs_start
		children_count: 2
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	new_append_start := t.a.children.len
	t.a.children << t.a.child(&append, 0)
	t.a.children << new_rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             .left_shift
		children_start: new_append_start
		children_count: 2
		pos:            append.pos
		value:          append.value
		typ:            append.typ
	})
}

fn (mut t Transformer) try_lower_optional_array_append_stmt(_node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId) ?[]flat.NodeId {
	if int(lhs_id) < 0 || int(rhs_id) < 0 {
		return none
	}
	lhs_node := t.a.nodes[int(lhs_id)]
	if lhs_node.kind != .or_expr || lhs_node.children_count < 2 {
		return none
	}
	source_id := t.a.child(&lhs_node, 0)
	if !t.optional_selector_lvalue_source(source_id) {
		return none
	}
	expr_type, value_type := t.or_expr_types(source_id, lhs_node.typ)
	if !t.is_optional_type_name(expr_type) {
		return none
	}
	array_type := t.clean_array_append_lhs_type(value_type)
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	raw_rhs_type := t.node_type(rhs_id)
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	rhs_node := t.a.nodes[int(rhs_id)]
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	rhs_is_sum_variant := t.array_append_rhs_is_sum_variant_value(rhs_id, raw_rhs_type, elem_type)
	if push_many && rhs_is_sum_variant {
		push_many = false
	}
	if rhs_node.kind == .array_literal && t.array_append_literal_should_push_many(rhs_id, elem_type) {
		push_many = true
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	} else if push_many && rhs_node.kind == .array_literal && !rhs_type.starts_with('[]') {
		t.set_node_typ(int(rhs_id), array_type)
		rhs_type = array_type
	}

	mut result := []flat.NodeId{}
	source := t.transform_lvalue(source_id)
	t.drain_pending(mut result)
	not_ok := t.make_prefix(.not, t.make_selector(source, 'ok', 'bool'))
	guard_stmts := t.optional_selector_lvalue_guard_stmts(t.a.child(&lhs_node, 1), lhs_node.value,
		source)
	result << t.make_if(not_ok, t.make_block(guard_stmts), t.make_empty())

	mut rhs := flat.empty_node
	if !push_many {
		if !rhs_is_sum_variant {
			if t.array_append_elem_is_interface(elem_type) {
				rhs = t.transform_expr_for_type(rhs_id, elem_type)
			} else {
				if converted := t.transform_array_value_for_dynamic_target(rhs_id, array_type) {
					rhs_type = array_type
					push_many = true
					rhs = converted
				} else {
					rhs = if elem_type in t.sum_types
						|| t.resolve_sum_name(elem_type) in t.sum_types {
						t.wrap_sum_value(rhs_id, elem_type)
					} else {
						t.transform_expr_for_type(rhs_id, elem_type)
					}
				}
			}
		} else {
			rhs = if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
				t.wrap_sum_value(rhs_id, elem_type)
			} else {
				t.transform_expr_for_type(rhs_id, elem_type)
			}
		}
	} else {
		rhs = t.transform_expr(rhs_id)
	}
	if !push_many {
		rhs = t.coerce_transformed_expr_to_type(rhs, rhs_id, elem_type)
	}
	t.drain_pending(mut result)
	if rhs_type.len == 0 {
		rhs_type = t.node_type(rhs)
		push_many = t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	}

	lhs_addr := t.runtime_addr(t.make_selector(source, 'value', array_type), array_type)
	if push_many {
		call := if t.is_fixed_array_type(rhs_type) {
			t.make_call_typed('array_push_many_ptr', arr3(lhs_addr, rhs,
				t.make_fixed_array_len_expr(rhs_type)), 'void')
		} else {
			t.make_array_push_many_call(lhs_addr, rhs, rhs_type)
		}
		t.drain_pending(mut result)
		result << t.make_expr_stmt(call)
		return result
	}
	value_name := t.new_temp('arr_val')
	result << t.make_decl_assign_typed(value_name, rhs, elem_type)
	result << t.make_expr_stmt(t.make_call_typed('array_push', arr2(lhs_addr, t.make_prefix(.amp,
		t.make_ident(value_name))), 'void'))
	return result
}

// clean_array_append_lhs_type transforms clean array append lhs type data for transform.
fn (t &Transformer) clean_array_append_lhs_type(typ string) string {
	mut clean := if array_type_has_generic_placeholder(typ) {
		typ.trim_space()
	} else {
		t.normalize_type_alias(typ).trim_space()
	}
	for {
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('atomic ') {
			clean = clean[7..].trim_space()
			continue
		}
		break
	}
	return clean
}

fn array_type_has_generic_placeholder(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if is_generic_placeholder_type_name(clean) {
		return true
	}
	if clean.starts_with('&') {
		return array_type_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('[]') {
		return array_type_has_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := clean.index(']') or { return false }
		return array_type_has_generic_placeholder(clean[4..bracket_end])
			|| array_type_has_generic_placeholder(clean[bracket_end + 1..])
	}
	if clean.starts_with('[') {
		bracket_end := clean.index(']') or { return false }
		return array_type_has_generic_placeholder(clean[bracket_end + 1..])
	}
	return false
}

// lower_array_prepend_call builds lower array prepend call data for transform.
fn (mut t Transformer) lower_array_prepend_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 2 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	value_id := t.a.child(&node, 1)
	raw_rhs_type := t.node_type(value_id)
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	value_node := t.a.nodes[int(value_id)]
	mut prepend_many := t.array_append_rhs_is_push_many(base_id, value_id, rhs_type, elem_type)
	if prepend_many && t.array_append_rhs_is_sum_variant_value(value_id, raw_rhs_type, elem_type) {
		prepend_many = false
	}
	if value_node.kind == .array_literal
		&& t.array_append_literal_should_push_many(value_id, elem_type) {
		prepend_many = true
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	} else if prepend_many && value_node.kind == .array_literal && !rhs_type.starts_with('[]') {
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	}
	base := t.transform_lvalue(base_id)
	if prepend_many {
		value := t.transform_expr(value_id)
		return t.make_array_insert_many_call(t.runtime_addr(base, base_type),
			t.make_int_literal(0), value, rhs_type)
	}
	value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
		t.wrap_sum_value(value_id, elem_type)
	} else {
		t.transform_expr_for_type(value_id, elem_type)
	}
	value_name := t.new_temp('arr_val')
	t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
	t.mark_fn_used('array__prepend')
	t.mark_fn_used('array__insert')
	t.mark_fn_used('array__needs_unique_shift')
	return t.make_call_typed('array__prepend', arr2(t.runtime_addr(base, base_type), t.make_prefix(.amp,
		t.make_ident(value_name))), 'void')
}

// lower_array_insert_call builds lower array insert call data for transform.
fn (mut t Transformer) lower_array_insert_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 3 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	index_id := t.a.child(&node, 1)
	value_id := t.a.child(&node, 2)
	raw_rhs_type := t.node_type(value_id)
	mut rhs_type := t.normalize_type_alias(raw_rhs_type)
	value_node := t.a.nodes[int(value_id)]
	mut insert_many := t.array_append_rhs_is_push_many(base_id, value_id, rhs_type, elem_type)
	if insert_many && t.array_append_rhs_is_sum_variant_value(value_id, raw_rhs_type, elem_type) {
		insert_many = false
	}
	if value_node.kind == .array_literal
		&& t.array_append_literal_should_push_many(value_id, elem_type) {
		insert_many = true
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	} else if insert_many && value_node.kind == .array_literal && !rhs_type.starts_with('[]') {
		t.set_node_typ(int(value_id), base_type)
		rhs_type = base_type
	}
	base := t.transform_lvalue(base_id)
	index := t.transform_expr_for_type(index_id, 'int')
	if insert_many {
		value := t.transform_expr(value_id)
		return t.make_array_insert_many_call(t.runtime_addr(base, base_type), index, value,
			rhs_type)
	}
	value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
		t.wrap_sum_value(value_id, elem_type)
	} else {
		t.transform_expr_for_type(value_id, elem_type)
	}
	value_name := t.new_temp('arr_val')
	t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
	t.mark_fn_used('array__insert')
	t.mark_fn_used('array__needs_unique_shift')
	return t.make_call_typed('array__insert', arr3(t.runtime_addr(base, base_type), index, t.make_prefix(.amp,
		t.make_ident(value_name))), 'void')
}

fn (mut t Transformer) lower_array_push_many_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 3 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	value_id := t.a.child(&node, 1)
	count_id := t.a.child(&node, 2)
	base := t.transform_lvalue(base_id)
	base_addr := t.runtime_addr(base, base_type)
	t.mark_fn_used('array__push_many')
	if t.push_many_count_is_type_name(count_id) {
		value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
			t.wrap_sum_value(value_id, elem_type)
		} else {
			t.transform_expr_for_type(value_id, elem_type)
		}
		value_name := t.new_temp('arr_val')
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
		return t.make_call_typed('array_push_many_ptr', arr3(base_addr, t.make_prefix(.amp,
			t.make_ident(value_name)), t.make_int_literal(1)), 'void')
	}
	value := t.transform_expr(value_id)
	count := t.transform_expr_for_type(count_id, 'int')
	return t.make_call_typed('array_push_many_ptr', arr3(base_addr, value, count), 'void')
}

fn (t &Transformer) push_many_count_is_type_name(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .ident && node.value.len > 0 && node.value[0] >= `A` && node.value[0] <= `Z`
}

// array_append_rhs_is_push_many supports array append rhs is push many handling for Transformer.
fn (t &Transformer) array_append_rhs_is_push_many(lhs_id flat.NodeId, rhs_id flat.NodeId, rhs_type string, elem_type string) bool {
	rhs_node := t.a.nodes[int(rhs_id)]
	if rhs_node.kind == .spawn_expr {
		return false
	}
	clean_rhs_type := rhs_type.trim_space()
	lhs_elem_is_interface := t.array_append_elem_is_interface(elem_type)
	if clean_rhs_type.starts_with('...') {
		return t.array_append_elem_types_match(clean_rhs_type[3..], elem_type)
	}
	if t.array_append_rhs_is_sum_array_variant(clean_rhs_type, elem_type) {
		return false
	}
	if clean_rhs_type.starts_with('[]') {
		if t.array_append_elem_types_match(clean_rhs_type[2..], elem_type) {
			return true
		}
		if declared_rhs_type := t.array_append_ident_type(rhs_id) {
			if declared_rhs_type.starts_with('...') {
				return t.array_append_elem_types_match(declared_rhs_type[3..], elem_type)
			}
			if declared_rhs_type.starts_with('[]') {
				return t.array_append_elem_types_match(declared_rhs_type[2..], elem_type)
			}
		}
		return false
	}
	if t.is_fixed_array_type(clean_rhs_type) {
		return t.array_append_elem_types_match(fixed_array_elem_type(clean_rhs_type), elem_type)
	}
	if !isnil(t.tc) {
		if rhs_resolved := t.tc.expr_type(rhs_id) {
			rhs_clean := types.unwrap_pointer(rhs_resolved)
			if rhs_clean is types.Array {
				if t.array_append_rhs_is_sum_array_variant(types.Type(rhs_clean).name(), elem_type) {
					return false
				}
				return t.array_append_elem_types_match(rhs_clean.elem_type.name(), elem_type)
			}
			if rhs_clean is types.ArrayFixed {
				if t.array_append_rhs_is_sum_array_variant(types.Type(rhs_clean).name(), elem_type) {
					return false
				}
				return t.array_append_elem_types_match(rhs_clean.elem_type.name(), elem_type)
			}
		}
		if lhs_elem_is_interface {
			return false
		}
		if lhs_resolved := t.tc.expr_type(lhs_id) {
			lhs_clean := types.unwrap_pointer(lhs_resolved)
			if lhs_clean is types.Array && clean_rhs_type in ['array', 'Array'] {
				return t.tc.c_type(lhs_clean.elem_type) == 'void*'
			}
		}
	}
	if lhs_elem_is_interface {
		return false
	}
	if clean_rhs_type in ['array', 'Array'] {
		return t.array_append_elem_c_type(elem_type) !in ['array', 'Array']
	}
	return false
}

fn (t &Transformer) array_append_rhs_is_sum_variant_value(rhs_id flat.NodeId, rhs_type string, elem_type string) bool {
	if !t.is_sum_type_name(elem_type) {
		return false
	}
	mut clean_rhs := rhs_type.trim_space()
	if clean_rhs.starts_with('!') || clean_rhs.starts_with('?') {
		clean_rhs = clean_rhs[1..].trim_space()
	}
	if clean_rhs.starts_with('[]') && t.array_append_elem_types_match(clean_rhs[2..], elem_type) {
		return false
	}
	candidate := t.array_append_rhs_variant_candidate(rhs_id, rhs_type)
	if candidate.len == 0 {
		return false
	}
	resolved_sum := t.resolve_sum_name(elem_type)
	if resolved_sum.len == 0 {
		return false
	}
	if _ := t.sum_variant_name(resolved_sum, candidate) {
		return true
	}
	return false
}

fn (t &Transformer) array_append_rhs_variant_candidate(rhs_id flat.NodeId, rhs_type string) string {
	if int(rhs_id) < 0 {
		return ''
	}
	node := t.a.nodes[int(rhs_id)]
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.array_append_rhs_variant_candidate(t.a.child(&node, 0), rhs_type)
	}
	if node.kind in [.cast_expr, .struct_init, .as_expr, .assoc] && node.value.len > 0 {
		return node.value
	}
	if node.kind == .call && node.children_count > 0 {
		name := t.generic_call_type_arg_name(t.a.child(&node, 0))
		if name.len > 0 {
			return name
		}
	}
	if node.kind == .ident && node.value.len > 0 {
		raw_type := t.raw_var_type(node.value)
		if raw_type.len > 0 {
			return raw_type
		}
	}
	return rhs_type
}

fn (t &Transformer) array_append_literal_should_push_many(rhs_id flat.NodeId, elem_type string) bool {
	if int(rhs_id) < 0 {
		return false
	}
	node := t.a.nodes[int(rhs_id)]
	if node.kind != .array_literal {
		return false
	}
	if t.array_append_elem_is_interface(elem_type) {
		return t.array_append_literal_children_match_elem(rhs_id, elem_type)
	}
	if t.array_append_rhs_is_sum_array_variant(t.node_type(rhs_id), elem_type) {
		return false
	}
	if !elem_type.starts_with('[]') && !t.is_fixed_array_type(elem_type) {
		return true
	}
	// With an array-shaped destination element, an empty literal is the single empty
	// element (`mut a := [][]int{}; a << []`), not a zero-element spread.
	if node.children_count == 0 {
		return false
	}
	return t.array_append_literal_children_match_elem(rhs_id, elem_type)
}

fn (t &Transformer) array_append_rhs_is_sum_array_variant(rhs_type string, elem_type string) bool {
	resolved_sum := t.resolve_sum_name(elem_type)
	variants := t.sum_types[resolved_sum] or { return false }
	mut clean_rhs := rhs_type.trim_space()
	if clean_rhs.starts_with('...') {
		clean_rhs = clean_rhs[3..].trim_space()
	}
	if clean_rhs.len == 0 {
		return false
	}
	// An array with exactly the destination's element type is the push-many
	// form (`[]Value << []Value`), even when `[]Value` also appears recursively
	// as a variant of `Value`. Distinct array variants such as `[]int` appended
	// to `[]Any` remain single sum-type elements.
	if clean_rhs.starts_with('[]') && t.array_append_elem_types_match(clean_rhs[2..], elem_type) {
		return false
	}
	for variant in variants {
		if t.array_append_elem_types_match(clean_rhs, variant) {
			return true
		}
	}
	return false
}

fn (t &Transformer) array_append_elem_is_interface(elem_type string) bool {
	if t.is_builtin_ierror_interface_name(elem_type) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	mut candidates := [elem_type.trim_space(), t.normalize_type_alias(elem_type).trim_space()]
	for candidate in candidates.clone() {
		if candidate.len == 0 || candidate.contains('.') {
			continue
		}
		candidates << t.tc.qualify_name(candidate)
		candidates << 'main.${candidate}'
		candidates << 'builtin.${candidate}'
	}
	for candidate in candidates {
		if candidate.len > 0 && candidate in t.tc.interface_names {
			return true
		}
	}
	return t.is_interface_type(elem_type)
}

fn (t &Transformer) array_append_literal_children_match_elem(rhs_id flat.NodeId, elem_type string) bool {
	node := t.a.nodes[int(rhs_id)]
	clean_elem := t.normalize_type_alias(elem_type)
	if clean_elem.len == 0 {
		return false
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .prefix && child.value == '...' && child.children_count > 0 {
			spread_id := t.a.child(&child, 0)
			spread_type := t.normalize_type_alias(t.node_type(spread_id))
			if spread_type.starts_with('[]')
				&& t.array_append_elem_types_match(spread_type[2..], clean_elem) {
				continue
			}
			return false
		}
		child_type := t.normalize_type_alias(t.node_type(child_id))
		if child.kind in [.array_literal, .array_init] && child.children_count == 0
			&& (clean_elem.starts_with('[]') || t.is_fixed_array_type(clean_elem)) {
			continue
		}
		if child.kind == .array_literal && clean_elem.starts_with('[]')
			&& t.array_append_literal_children_match_elem(child_id, clean_elem[2..]) {
			continue
		}
		if t.array_append_elem_types_match(child_type, clean_elem) {
			continue
		}
		if child_type.starts_with('&')
			&& t.array_append_elem_types_match(child_type[1..], clean_elem) {
			continue
		}
		if child_type.len == 0 && child.kind == .array_literal
			&& (clean_elem.starts_with('[]') || t.is_fixed_array_type(clean_elem)) {
			continue
		}
		return false
	}
	return true
}

// array_append_elem_types_match supports array append elem types match handling for Transformer.
fn (t &Transformer) array_append_elem_types_match(rhs_elem_type string, lhs_elem_type string) bool {
	rhs_raw := rhs_elem_type.trim_space()
	lhs_raw := lhs_elem_type.trim_space()
	if rhs_raw == lhs_raw {
		return true
	}
	rhs_clean := t.normalize_type_alias(rhs_elem_type)
	lhs_clean := t.normalize_type_alias(lhs_elem_type)
	if rhs_clean == lhs_clean {
		return true
	}
	if array_append_type_is_container_shape(rhs_clean)
		|| array_append_type_is_container_shape(lhs_clean) {
		return false
	}
	if isnil(t.tc) {
		return false
	}
	lhs_iface := t.resolve_interface_type_name(lhs_clean)
	if lhs_iface.len > 0 {
		if t.array_append_interface_has_requirements(lhs_iface) {
			rhs_concrete := t.trim_pointer_type(rhs_clean)
			if t.tc.named_type_implements_interface(rhs_concrete, lhs_iface) {
				return true
			}
		}
	}
	return t.array_append_elem_c_type(rhs_clean) == t.array_append_elem_c_type(lhs_clean)
}

fn (t &Transformer) array_append_interface_has_requirements(iface_name string) bool {
	if isnil(t.tc) {
		return false
	}
	if t.tc.interface_abstract_method_names(iface_name).len > 0 {
		return true
	}
	if (t.tc.interface_fields[iface_name] or { []types.StructField{} }).len > 0 {
		return true
	}
	for embed in t.tc.interface_embeds[iface_name] or { []string{} } {
		if t.array_append_interface_has_requirements(embed) {
			return true
		}
	}
	return false
}

fn array_append_type_is_container_shape(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') {
		return array_append_type_is_container_shape(clean[1..])
	}
	if clean.starts_with('shared ') {
		return array_append_type_is_container_shape(clean[7..])
	}
	if clean.starts_with('atomic ') {
		return array_append_type_is_container_shape(clean[7..])
	}
	return clean.starts_with('[]') || clean.starts_with('map[')
		|| (clean.starts_with('[') && clean.contains(']'))
}

// array_append_ident_type supports array append ident type handling for Transformer.
fn (t &Transformer) array_append_ident_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	typ := t.var_type(node.value)
	if typ.len == 0 {
		return none
	}
	return typ
}

// array_append_elem_c_type supports array append elem c type handling for Transformer.
fn (t &Transformer) array_append_elem_c_type(typ string) string {
	if isnil(t.tc) {
		return typ
	}
	clean := typ.trim_space()
	if clean.len == 0 {
		return clean
	}
	if !clean.contains('.') {
		for alias, target in t.tc.type_aliases {
			if alias.all_after_last('.') == clean {
				return t.tc.c_type(t.tc.parse_type(target))
			}
		}
	}
	return t.tc.c_type(t.tc.parse_type(clean))
}

// array_get_value supports array get value handling for Transformer.
fn (mut t Transformer) array_get_value(base flat.NodeId, index flat.NodeId, elem_type string) flat.NodeId {
	if t.array_get_base_is_fixed_array(base) {
		return t.make_index(base, index, elem_type)
	}
	get_call := t.make_call_typed('array_get', arr2(t.array_get_runtime_base(base), index),
		'voidptr')
	ptr := t.make_cast('&${elem_type}', get_call, '&${elem_type}')
	value := t.make_prefix(.mul, ptr)
	t.set_node_typ(int(value), elem_type)
	return value
}

// array_get_ptr supports array get ptr handling for Transformer.
fn (mut t Transformer) array_get_ptr(base flat.NodeId, index flat.NodeId, elem_type string) flat.NodeId {
	if t.array_get_base_is_fixed_array(base) {
		value := t.make_index(base, index, elem_type)
		ptr := t.make_prefix(.amp, value)
		t.set_node_typ(int(ptr), '&${elem_type}')
		return ptr
	}
	get_call := t.make_call_typed('array_get', arr2(t.array_get_runtime_base(base), index),
		'voidptr')
	return t.make_cast('&${elem_type}', get_call, '&${elem_type}')
}

fn (t &Transformer) array_get_base_is_fixed_array(base flat.NodeId) bool {
	node := t.a.node(base)
	mut base_type := if node.kind == .ident && t.var_type(node.value).len > 0 {
		t.var_type(node.value).trim_space()
	} else {
		t.node_type(base).trim_space()
	}
	if base_type.len == 0 {
		base_type = t.original_expr_type(base).trim_space()
	}
	if base_type.starts_with('&') {
		base_type = base_type[1..].trim_space()
	}
	if t.is_fixed_array_type(base_type) {
		return true
	}
	return false
}

fn (mut t Transformer) array_get_runtime_base(base flat.NodeId) flat.NodeId {
	base_type := t.node_type(base).trim_space()
	if base_type.starts_with('&') {
		node := t.a.nodes[int(base)]
		if t.array_get_base_is_shared_value_selector(base, node)
			|| t.array_get_base_is_shared_array_ident(base_type, node) {
			t.set_node_typ(int(base), base_type[1..])
			return base
		}
		value := t.make_prefix(.mul, base)
		t.set_node_typ(int(value), base_type[1..])
		return value
	}
	return base
}

fn (mut t Transformer) array_get_base_is_shared_value_selector(base flat.NodeId, node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 || node.value != 'val' {
		return false
	}
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id).trim_space()
	if base_type.starts_with('shared ') || base_type.starts_with('&shared ')
		|| base_type.contains('__shared__') {
		return true
	}
	// During rlock lowering a shared array value field can be represented as `&[]T`.
	// Passing that field to `array_get` should use the selector storage directly; a
	// dereference would emit `*array->val`, which is not a valid C Array lvalue.
	return t.node_type(base).trim_space().starts_with('&[]')
}

fn (t &Transformer) array_get_base_is_shared_array_ident(base_type string, node flat.Node) bool {
	if !base_type.starts_with('&[]') || node.kind != .ident || node.value.len == 0 {
		return false
	}
	raw_type := t.raw_var_type(node.value).trim_space()
	return raw_type.starts_with('shared ')
}

// lower_array_filter_call builds lower array filter call data for transform.
fn (mut t Transformer) lower_array_filter_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	base_id := t.a.child(&fn_node, 0)
	elem_needs_clone := !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
	if elem_needs_clone {
		// The checker reports the missing clone method. Do not lower the rejected call
		// to a shallow element copy while processing the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(elem_type)) {
			return t.make_empty()
		}
	}
	source_needs_drop := !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	out_name := t.new_temp('filter')
	idx_name := t.new_temp('filter_idx')
	prefix << t.make_decl_assign_typed(out_name, t.make_array_new_call(elem_type,
		t.make_int_literal(0), t.make_selector(base, 'len', 'int')), base_type)
	mut cleanup_guard_name := ''
	if source_needs_drop {
		cleanup_guard_name = t.new_temp('filter_values_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drops := [
			t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void')),
			t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(t.make_ident(out_name)), 'void')),
		]
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name),
			t.make_block(deferred_drops), t.make_empty())
		defer_body := t.make_block(arr1(guarded_drop))
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind:           .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_name_default := t.new_temp('filter_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	predicate_id := t.a.child(&node, 1)
	predicate_node := t.a.nodes[int(predicate_id)]
	mut predicate_expr_id := predicate_id
	mut lambda_param := ''
	mut predicate_fn_name := ''
	if predicate_node.kind == .lambda_expr && predicate_node.children_count > 0 {
		predicate_expr_id = t.a.child(&predicate_node, predicate_node.children_count - 1)
		if predicate_node.children_count > 1 {
			param := t.a.child_node(&predicate_node, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
			}
		}
	} else if fn_name := t.resolve_fn_value_expr(predicate_id, predicate_node) {
		predicate_fn_name = fn_name
	} else if predicate_node.kind == .ident {
		if ret_type := t.fn_value_return_type_name(predicate_id) {
			if ret_type == 'bool' {
				predicate_fn_name = predicate_node.value
			}
		}
	}
	elem_name := if lambda_param.len > 0 { lambda_param } else { elem_name_default }
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	predicate_source := if lambda_param.len > 0 {
		predicate_expr_id
	} else {
		t.substitute_ident(predicate_expr_id, 'it', elem_name)
	}
	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	predicate := if predicate_fn_name.len > 0 {
		t.make_call_typed(predicate_fn_name, arr1(t.make_ident(elem_name)), 'bool')
	} else if predicate_node.kind == .fn_literal {
		fn_value := t.transform_expr(predicate_id)
		fn_value_node := t.a.nodes[int(fn_value)]
		if fn_value_node.kind == .ident {
			t.make_call_typed(fn_value_node.value, arr1(t.make_ident(elem_name)), 'bool')
		} else {
			fn_value
		}
	} else {
		t.transform_expr(predicate_source)
	}
	predicate_pending := t.pending_stmts.clone()
	t.pending_stmts = saved_pending
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	for stmt in predicate_pending {
		loop_body << stmt
	}
	mut then_body := []flat.NodeId{}
	mut pushed_name := elem_name
	if elem_needs_clone {
		pending_start := t.pending_stmts.len
		cloned_elem := t.make_compiler_default_clone_value(t.make_ident(elem_name), elem_type, true)
		then_body = t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		pushed_name = t.new_temp('filter_value')
		then_body << t.make_decl_assign_typed(pushed_name, cloned_elem, elem_type)
	}
	push_call := t.make_call_typed('array_push', arr2(t.make_prefix(.amp, t.make_ident(out_name)), t.make_prefix(.amp,
		t.make_ident(pushed_name))), 'void')
	then_body << t.make_expr_stmt(push_call)
	then_block := t.make_block(then_body)
	loop_body << t.make_if(predicate, then_block, t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_needs_drop {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(out_name)
}

// lower_array_map_call builds lower array map call data for transform.
fn (mut t Transformer) lower_array_map_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	map_expr_id := t.a.child(&node, 1)
	map_expr := t.a.nodes[int(map_expr_id)]
	mut map_source_id := map_expr_id
	mut lambda_param := ''
	if map_expr.kind == .lambda_expr && map_expr.children_count > 0 {
		map_source_id = t.a.child(&map_expr, map_expr.children_count - 1)
		if map_expr.children_count > 1 {
			param := t.a.child_node(&map_expr, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
			}
		}
	}
	mut map_fn_name := ''
	elem_name := if lambda_param.len > 0 { lambda_param } else { t.new_temp('map_it') }
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	mapped_source := if lambda_param.len > 0 {
		map_source_id
	} else {
		t.substitute_ident(map_source_id, 'it', elem_name)
	}
	checker_result_elem_type := t.checker_expr_type_name(map_expr_id) or { '' }
	mut result_elem_type := if checker_result_elem_type.len > 0 {
		checker_result_elem_type
	} else {
		t.node_type(map_expr_id)
	}
	mut direct_selector_type := ''
	mut mapped_source_node := t.a.nodes[int(mapped_source)]
	if mapped_source_node.kind == .map_init {
		inferred_map_type := t.infer_map_init_entry_type(mapped_source_node)
		if inferred_map_type.len > 0 {
			result_elem_type = inferred_map_type
			t.set_node_value(int(mapped_source), inferred_map_type)
			t.set_node_typ(int(mapped_source), inferred_map_type)
			mapped_source_node = t.a.nodes[int(mapped_source)]
		}
	}
	if mapped_source_node.kind == .selector {
		selector_type := t.lookup_struct_field_type(elem_type, mapped_source_node.value) or {
			t.resolve_selector_type(mapped_source_node)
		}
		if selector_type.len > 0 {
			direct_selector_type = selector_type
			result_elem_type = selector_type
		}
	}
	if fn_name := t.resolve_fn_value_expr(map_expr_id, map_expr) {
		map_fn_name = fn_name
		if ret := t.fn_ret_types[fn_name] {
			result_elem_type = ret
		} else if !isnil(t.tc) {
			if ret_type := t.tc.fn_ret_types[fn_name] {
				result_elem_type = t.normalize_type_alias(ret_type.name())
			}
		}
	} else if map_expr.kind == .ident {
		if ret_type := t.fn_value_return_type_name(map_expr_id) {
			map_fn_name = map_expr.value
			result_elem_type = ret_type
		}
	} else if map_expr.kind == .fn_literal || map_expr.kind == .lambda_expr {
		if ret_type := t.fn_value_return_type_name(map_expr_id) {
			result_elem_type = ret_type
		}
	}
	if result_elem_type.len == 0 {
		result_elem_type = t.reliable_stringify_type(map_expr_id)
	}
	bound_method_info := t.array_map_bound_method_info(mapped_source_node, elem_name, elem_type,
		result_elem_type) or { BoundMethodArrayInfo{} }
	has_bound_method_array := bound_method_info.receiver_type.len > 0
	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mapped_expr := if map_fn_name.len > 0 {
		t.make_call_typed(map_fn_name, arr1(t.make_ident(elem_name)), result_elem_type)
	} else if map_expr.kind == .fn_literal {
		fn_value := t.transform_expr(map_expr_id)
		fn_value_node := t.a.nodes[int(fn_value)]
		if fn_value_node.kind == .ident && result_elem_type.len > 0 {
			t.make_call_typed(fn_value_node.value, arr1(t.make_ident(elem_name)), result_elem_type)
		} else {
			fn_value
		}
	} else if has_bound_method_array {
		t.make_cast(result_elem_type, t.make_cast('usize', t.make_ident(elem_name), 'usize'),
			result_elem_type)
	} else if mapped_source_node.kind == .map_init && result_elem_type.starts_with('map[') {
		t.transform_expr_for_type(mapped_source, result_elem_type)
	} else {
		t.transform_expr(mapped_source)
	}
	mapped_pending := t.pending_stmts.clone()
	t.pending_stmts = saved_pending
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	mapped_type := t.node_type(mapped_expr)
	if checker_result_elem_type.len == 0 && mapped_type.len > 0 && mapped_type != 'unknown' {
		result_elem_type = mapped_type
	}
	if direct_selector_type.len > 0 && map_fn_name.len == 0 {
		result_elem_type = direct_selector_type
	}
	if direct_selector_type.len == 0 {
		if mapped_expr_node := t.selector_expr_node(mapped_expr) {
			selector_type := t.resolve_selector_type(mapped_expr_node)
			if selector_type.len > 0 {
				result_elem_type = selector_type
			}
		}
	}
	if result_elem_type.len == 0 {
		result_elem_type = elem_type
	}
	opaque_mapper := map_fn_name.len > 0 || map_expr.kind == .fn_literal
	mapper_returns_owned := !isnil(t.tc)
		&& t.tc.ownership_fn_value_returns_owned(map_expr_id, t.cur_fn_name, t.cur_module)
	mapped_borrows_elem := (opaque_mapper && !mapper_returns_owned)
		|| (t.array_map_expr_references_ident(mapped_source, elem_name)
		&& (isnil(t.tc) || !t.tc.ownership_expr_creates_owned_value(map_source_id)))
	mapped_result_needs_clone := mapped_borrows_elem && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(result_elem_type))
	if mapped_result_needs_clone {
		// The checker reports the missing clone method. Do not leave a shallow mapped
		// owner in the result while transforming the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(result_elem_type)) {
			return t.make_empty()
		}
	}
	out_type := '[]${result_elem_type}'
	base_id := t.a.child(&fn_node, 0)
	source_needs_drop := !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	out_name := t.new_temp('map')
	idx_name := t.new_temp('map_idx')
	prefix << t.make_decl_assign_typed(out_name, t.make_array_new_call(result_elem_type,
		t.make_int_literal(0), t.make_selector(base, 'len', 'int')), out_type)
	mut cleanup_guard_name := ''
	if source_needs_drop {
		cleanup_guard_name = t.new_temp('map_values_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drops := [
			t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void')),
			t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(t.make_ident(out_name)), 'void')),
		]
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name),
			t.make_block(deferred_drops), t.make_empty())
		defer_body := t.make_block(arr1(guarded_drop))
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind:           .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	for stmt in mapped_pending {
		loop_body << stmt
	}
	value_name := t.new_temp('map_val')
	loop_body << t.make_decl_assign_typed(value_name, mapped_expr, result_elem_type)
	mut pushed_name := value_name
	if mapped_result_needs_clone {
		mapped_value := t.make_ident(value_name)
		t.set_node_typ(int(mapped_value), result_elem_type)
		pending_start := t.pending_stmts.len
		cloned_value := t.make_compiler_default_clone_value(mapped_value, result_elem_type, true)
		for stmt in t.pending_stmts[pending_start..].clone() {
			loop_body << stmt
		}
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		pushed_name = t.new_temp('map_cloned_val')
		loop_body << t.make_decl_assign_typed(pushed_name, cloned_value, result_elem_type)
	}
	loop_body << t.make_expr_stmt(t.make_call_typed('array_push', arr2(t.make_prefix(.amp,
		t.make_ident(out_name)), t.make_prefix(.amp, t.make_ident(pushed_name))), 'void'))
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_needs_drop {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), out_type)
	if has_bound_method_array {
		t.bound_method_arrays[t.bound_method_array_key(out_name)] = bound_method_info
	}
	return result
}

// array_map_expr_references_ident reports whether a mapped value reads the synthetic
// element binding. Such values remain borrowed from the consumed source unless their
// expression explicitly creates a fresh owner.
fn (t &Transformer) array_map_expr_references_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 || name.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.array_map_expr_references_ident(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) resolve_fn_value_expr(id flat.NodeId, node flat.Node) ?string {
	if !isnil(t.tc) {
		if name := t.tc.resolved_fn_value_name(id) {
			return name
		}
	}
	if node.kind == .ident {
		return t.resolve_fn_value_ident(node.value)
	}
	if node.kind == .selector {
		return t.resolve_fn_value_selector(node)
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.resolve_fn_value_expr(t.a.child(&node, 0), t.a.child_node(&node, 0))
	}
	return none
}

fn (t &Transformer) resolve_fn_value_selector(node flat.Node) ?string {
	if node.children_count == 0 || node.value.len == 0 || isnil(t.tc) {
		return none
	}
	base := t.a.child_node(&node, 0)
	if base.kind == .ident {
		if t.var_type(base.value).len > 0 {
			return none
		}
		if resolved := t.resolve_static_fn_value_for_type(base.value, node.value) {
			return resolved
		}
		key := '${base.value}.${node.value}'
		if key in t.tc.fn_ret_types {
			return key
		}
		return none
	}
	if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(base, 0)
		if inner.kind == .ident {
			type_name := '${inner.value}.${base.value}'
			if resolved := t.resolve_static_fn_value_for_type(type_name, node.value) {
				return resolved
			}
			key := '${type_name}.${node.value}'
			if key in t.tc.fn_ret_types {
				return key
			}
		}
	}
	return none
}

fn (t &Transformer) resolve_static_fn_value_for_type(type_name string, method string) ?string {
	if type_name.len == 0 || method.len == 0 || isnil(t.tc) {
		return none
	}
	mut candidates := []string{}
	t.add_static_fn_value_type_candidate(mut candidates, type_name)
	if !type_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		t.add_static_fn_value_type_candidate(mut candidates, '${t.cur_module}.${type_name}')
	}
	for candidate in candidates {
		key := '${candidate}.${method}'
		if key in t.tc.fn_ret_types {
			return key
		}
	}
	return none
}

fn (t &Transformer) add_static_fn_value_type_candidate(mut candidates []string, name string) {
	if name.len == 0 || isnil(t.tc) {
		return
	}
	if name !in candidates {
		candidates << name
	}
	if target := t.tc.type_aliases[name] {
		if target !in candidates {
			candidates << target
		}
	}
}

fn (t &Transformer) array_map_bound_method_info(node flat.Node, elem_name string, elem_type string, result_elem_type string) ?BoundMethodArrayInfo {
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	if !(result_elem_type.starts_with('fn()') || result_elem_type.starts_with('fn ()')) {
		return none
	}
	base := t.a.child_node(&node, 0)
	if base.kind != .ident || base.value != elem_name {
		return none
	}
	receiver_type := t.bound_builtin_method_receiver_type(elem_type, node.value) or { return none }
	return_type := t.local_fn_value_return_type_from_type(result_elem_type) or { return none }
	if return_type == 'void' {
		return none
	}
	return BoundMethodArrayInfo{
		receiver_type: receiver_type
		fn_type:       result_elem_type
		method:        node.value
		return_type:   return_type
	}
}

fn (t &Transformer) bound_builtin_method_receiver_type(elem_type string, method string) ?string {
	if method !in ['hex', 'hex_full'] {
		return none
	}
	mut clean := t.normalize_type_alias(elem_type)
	if clean.starts_with('&') {
		return none
	}
	if clean == 'byte' {
		clean = 'u8'
	}
	if clean in ['u8', 'i8', 'u16', 'i16', 'u32', 'int', 'u64', 'i64', 'rune'] {
		return clean
	}
	return none
}

fn (t &Transformer) fn_value_return_type_name(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			if ret := fn_value_return_type_from_type(typ) {
				return t.normalize_type_alias(ret)
			}
		}
		if node.kind == .fn_literal || node.kind == .lambda_expr {
			typ := t.tc.resolve_type(id)
			if ret := fn_value_return_type_from_type(typ) {
				return t.normalize_type_alias(ret)
			}
		}
	}
	mut typ := t.checker_expr_type_name(id) or { '' }
	if typ.len == 0 {
		typ = node.typ
	}
	if typ.len == 0 && node.kind == .ident {
		typ = t.var_type(node.value)
	}
	if typ.len == 0 || isnil(t.tc) {
		return none
	}
	parsed := t.tc.parse_type(typ)
	if parsed is types.FnType {
		name := parsed.return_type.name()
		if name.len > 0 {
			return t.normalize_type_alias(name)
		}
	}
	return none
}

fn fn_value_return_type_from_type(typ types.Type) ?string {
	if typ is types.FnType {
		return typ.return_type.name()
	}
	if typ is types.Alias {
		if typ.base_type is types.FnType {
			return typ.base_type.return_type.name()
		}
	}
	return none
}

// selector_expr_node supports selector expr node handling for Transformer.
fn (t &Transformer) selector_expr_node(id flat.NodeId) ?flat.Node {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector {
		return node
	}
	return none
}

// substitute_ident supports substitute ident handling for Transformer.
fn (mut t Transformer) substitute_ident(id flat.NodeId, name string, replacement string) flat.NodeId {
	if int(id) < 0 || name.len == 0 || replacement.len == 0 || name == replacement {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		new_id := t.make_ident(replacement)
		if t.a.nodes[int(new_id)].typ.len == 0 {
			t.set_node_typ(int(new_id), node.typ)
		}
		return new_id
	}
	if node.kind == .lambda_expr && node.children_count > 1 {
		first := t.a.child_node(&node, 0)
		if first.kind == .ident && first.value == name {
			return id
		}
	}
	if node.kind == .call && node.children_count > 1 {
		fn_id := t.a.child(&node, 0)
		fn_node := t.a.nodes[int(fn_id)]
		if fn_node.kind == .selector && fn_node.value in ['any', 'all', 'count', 'filter', 'map'] {
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << t.substitute_ident(fn_id, name, replacement)
			for i in 1 .. node.children_count {
				new_children << t.a.child(&node, i)
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind:           node.kind
				op:             node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		new_children << t.substitute_ident(t.a.child(&node, i), name, replacement)
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		payload:        flat.node_payload(node.generic_params().clone())
	})
}

fn (mut t Transformer) substitute_ident_expr(id flat.NodeId, name string, replacement flat.NodeId) flat.NodeId {
	if int(id) < 0 || name.len == 0 || int(replacement) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return replacement
	}
	if node.kind == .lambda_expr && node.children_count > 1 {
		first := t.a.child_node(&node, 0)
		if first.kind == .ident && first.value == name {
			return id
		}
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if name == 'index' && node.kind == .array_init && child.kind == .field_init
			&& child.value == 'init' {
			new_children << child_id
			continue
		}
		new_children << t.substitute_ident_expr(child_id, name, replacement)
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		payload:        flat.node_payload(node.generic_params().clone())
	})
}

fn (mut t Transformer) infer_map_init_entry_type(node flat.Node) string {
	if node.kind != .map_init || node.children_count < 2 {
		return ''
	}
	key_type := t.array_literal_child_value_type(t.a.child(&node, 0))
	value_type := t.array_literal_child_value_type(t.a.child(&node, 1))
	if key_type.len == 0 || value_type.len == 0 {
		return ''
	}
	return 'map[${key_type}]${value_type}'
}

fn (t &Transformer) is_array_transform_call(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return false
	}
	if fn_node.value !in ['filter', 'map', 'sorted'] {
		return false
	}
	base_type := t.normalize_type_alias(t.node_type(t.a.child(&fn_node, 0)))
	return base_type.starts_with('[]')
}

// lower_array_count_call builds lower array count call data for transform.
fn (mut t Transformer) lower_array_count_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	base_id := t.a.child(&fn_node, 0)
	source_is_owned_temporary := !t.expr_can_take_address(base_id) && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('count')
	idx_name := t.new_temp('count_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(0), 'int')
	mut cleanup_guard_name := ''
	if source_is_owned_temporary {
		cleanup_guard_name = t.new_temp('count_source_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drop := t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void'))
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name),
			t.make_block(arr1(deferred_drop)), t.make_empty())
		defer_body := t.make_block(arr1(guarded_drop))
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind:           .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	mut elem_name := t.new_temp('count_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	predicate_id := t.a.child(&node, 1)
	predicate_node := t.a.nodes[int(predicate_id)]
	mut predicate_expr_id := predicate_id
	mut lambda_param := ''
	if predicate_node.kind == .lambda_expr && predicate_node.children_count > 0 {
		predicate_expr_id = t.a.child(&predicate_node, predicate_node.children_count - 1)
		if predicate_node.children_count > 1 {
			param := t.a.child_node(&predicate_node, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
			}
		}
	}
	elem_name = if lambda_param.len > 0 { lambda_param } else { elem_name }
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	predicate_source := if lambda_param.len > 0 {
		predicate_expr_id
	} else {
		t.substitute_ident(predicate_expr_id, 'it', elem_name)
	}
	predicate := t.transform_expr(predicate_source)
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	t.drain_pending(mut loop_body)
	inc := t.make_assign_op(t.make_ident(result_name), t.make_int_literal(1), .plus_assign)
	loop_body << t.make_if(predicate, t.make_block(arr1(inc)), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_any_all_call builds lower array any all call data for transform.
fn (mut t Transformer) lower_array_any_all_call(node flat.Node, fn_node flat.Node, base_type string, method string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	base_id := t.a.child(&fn_node, 0)
	source_is_owned_temporary := !t.expr_can_take_address(base_id) && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(base_type))
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp(method)
	idx_name := t.new_temp('${method}_idx')
	default_value := if method == 'all' {
		t.make_bool_literal(true)
	} else {
		t.make_bool_literal(false)
	}
	prefix << t.make_decl_assign_typed(result_name, default_value, 'bool')
	mut cleanup_guard_name := ''
	if source_is_owned_temporary {
		cleanup_guard_name = t.new_temp('${method}_source_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), 'bool')
		deferred_drop := t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void'))
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name),
			t.make_block(arr1(deferred_drop)), t.make_empty())
		defer_body := t.make_block(arr1(guarded_drop))
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind:           .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	mut elem_name := t.new_temp('${method}_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	predicate_id := t.a.child(&node, 1)
	predicate_node := t.a.nodes[int(predicate_id)]
	mut predicate_expr_id := predicate_id
	mut lambda_param := ''
	if predicate_node.kind == .lambda_expr && predicate_node.children_count > 0 {
		predicate_expr_id = t.a.child(&predicate_node, predicate_node.children_count - 1)
		if predicate_node.children_count > 1 {
			param := t.a.child_node(&predicate_node, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
			}
		}
	}
	elem_name = if lambda_param.len > 0 { lambda_param } else { elem_name }
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	predicate_source := if lambda_param.len > 0 {
		predicate_expr_id
	} else {
		t.substitute_ident(predicate_expr_id, 'it', elem_name)
	}
	predicate := t.transform_expr(predicate_source)
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	t.drain_pending(mut loop_body)
	if method == 'all' {
		not_predicate := t.make_prefix(.not, t.make_paren(predicate))
		assign_false := t.make_assign(t.make_ident(result_name), t.make_bool_literal(false))
		loop_body << t.make_if(not_predicate, t.make_block(arr1(assign_false)), t.make_empty())
	} else {
		assign_true := t.make_assign(t.make_ident(result_name), t.make_bool_literal(true))
		loop_body << t.make_if(predicate, t.make_block(arr1(assign_true)), t.make_empty())
	}
	prefix << t.make_for_stmt(init, cond, post, loop_body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', arr1(base), 'void'))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_sort_call builds lower array sort call data for transform.
fn (mut t Transformer) lower_array_sort_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if !clean_base_type.starts_with('[]') && !(clean_base_type.starts_with('&')
		&& clean_base_type[1..].starts_with('[]')) {
		return none
	}
	if node.children_count > 2 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.transform_lvalue(base_id)
	clean_type := if clean_base_type.starts_with('&') {
		clean_base_type[1..]
	} else {
		clean_base_type
	}
	t.set_node_typ(int(base), clean_type)
	elem_type := clean_type[2..]
	cmp_id := if node.children_count > 1 { t.a.child(&node, 1) } else { flat.empty_node }
	t.pending_stmts << t.make_array_default_sort_stmt(base, elem_type, node, cmp_id)
	return t.make_empty()
}

fn transform_unshared_receiver_type(typ string) string {
	mut prefix := ''
	mut clean := typ.trim_space()
	if clean.starts_with('&') {
		prefix = '&'
		clean = clean[1..].trim_space()
	}
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	return prefix + clean
}

// lower_array_sorted_call builds lower array sorted call data for transform.
fn (mut t Transformer) lower_array_sorted_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if node.children_count > 2 || !clean_base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_array_clone_call(base_id, clean_base_type)
	t.set_var_type(clone_name, clean_base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, clean_base_type)
	cmp_id := if node.children_count > 1 { t.a.child(&node, 1) } else { flat.empty_node }
	t.pending_stmts << t.make_array_default_sort_stmt(t.make_ident(clone_name),
		clean_base_type[2..], node, cmp_id)
	return t.make_ident(clone_name)
}

// lower_array_sort_with_compare_call builds lower array sort with compare call data for transform.
fn (mut t Transformer) lower_array_sort_with_compare_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if node.children_count != 2
		|| (!clean_base_type.starts_with('[]') && !(clean_base_type.starts_with('&')
		&& clean_base_type[1..].starts_with('[]'))) {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.transform_lvalue(base_id)
	clean_type := if clean_base_type.starts_with('&') {
		clean_base_type[1..]
	} else {
		clean_base_type
	}
	t.set_node_typ(int(base), clean_type)
	elem_type := clean_type[2..]
	cmp := t.stable_array_compare_fn(t.a.child(&node, 1), elem_type)
	t.pending_stmts << t.make_array_compare_sort_stmt(base, elem_type, node, cmp)
	return t.make_empty()
}

// lower_array_sorted_with_compare_call supports lower_array_sorted_with_compare_call handling.
fn (mut t Transformer) lower_array_sorted_with_compare_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	clean_base_type := transform_unshared_receiver_type(base_type)
	if node.children_count != 2 || !clean_base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_array_clone_call(base_id, clean_base_type)
	elem_type := clean_base_type[2..]
	cmp := t.stable_array_compare_fn(t.a.child(&node, 1), elem_type)
	t.set_var_type(clone_name, clean_base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, clean_base_type)
	t.pending_stmts << t.make_array_compare_sort_stmt(t.make_ident(clone_name), elem_type, node, cmp)
	return t.make_ident(clone_name)
}

// stable_array_compare_fn supports stable array compare fn handling for Transformer.
fn (mut t Transformer) stable_array_compare_fn(cmp_id flat.NodeId, elem_type string) flat.NodeId {
	if int(cmp_id) >= 0 && t.a.nodes[int(cmp_id)].kind == .lambda_expr {
		return cmp_id
	}
	cmp := t.transform_expr(cmp_id)
	cmp_type := 'fn (&${elem_type}, &${elem_type}) int'
	return t.stable_transformed_expr_for_reuse(cmp, cmp_type, 'sort_cmp')
}

// make_array_default_sort_stmt builds make array default sort stmt data for transform.
fn (mut t Transformer) make_array_default_sort_stmt(base flat.NodeId, elem_type string, src flat.Node, cmp_id flat.NodeId) flat.NodeId {
	i_name := t.new_temp('sort_i')
	j_name := t.new_temp('sort_j')
	tmp_name := t.new_temp('sort_tmp')
	t.set_var_type(i_name, 'int')
	t.set_var_type(j_name, 'int')
	t.set_var_type(tmp_name, elem_type)
	init := t.make_decl_assign_typed(i_name, t.make_int_literal(1), 'int')
	cond := t.make_infix(.lt, t.make_ident(i_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(i_name), .inc))
	j_decl := t.make_decl_assign_typed(j_name, t.make_ident(i_name), 'int')
	inner_cond := t.make_infix(.logical_and, t.make_infix(.gt, t.make_ident(j_name),
		t.make_int_literal(0)), t.array_sort_less_expr(base, elem_type, j_name, cmp_id))
	tmp_decl := t.make_decl_assign_typed(tmp_name, t.make_index(base, t.make_ident(j_name),
		elem_type), elem_type)
	prev_idx := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_cur := t.make_index_assign(t.make_index(base, t.make_ident(j_name), elem_type), t.make_index(base,
		prev_idx, elem_type))
	prev_idx2 := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_prev := t.make_index_assign(t.make_index(base, prev_idx2, elem_type),
		t.make_ident(tmp_name))
	dec_j := t.make_expr_stmt(t.make_postfix(t.make_ident(j_name), .dec))
	inner_body := [tmp_decl, assign_cur, assign_prev, dec_j]
	inner_for := t.make_for_stmt(t.make_empty(), inner_cond, t.make_empty(), inner_body, src)
	return t.make_for_stmt(init, cond, post, [j_decl, inner_for], src)
}

// make_array_compare_sort_stmt builds make array compare sort stmt data for transform.
fn (mut t Transformer) make_array_compare_sort_stmt(base flat.NodeId, elem_type string, src flat.Node, cmp flat.NodeId) flat.NodeId {
	i_name := t.new_temp('sort_i')
	j_name := t.new_temp('sort_j')
	tmp_name := t.new_temp('sort_tmp')
	t.set_var_type(i_name, 'int')
	t.set_var_type(j_name, 'int')
	t.set_var_type(tmp_name, elem_type)
	init := t.make_decl_assign_typed(i_name, t.make_int_literal(1), 'int')
	cond := t.make_infix(.lt, t.make_ident(i_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(i_name), .inc))
	j_decl := t.make_decl_assign_typed(j_name, t.make_ident(i_name), 'int')
	inner_cond := t.make_infix(.logical_and, t.make_infix(.gt, t.make_ident(j_name),
		t.make_int_literal(0)), t.array_sort_compare_less_expr(base, elem_type, j_name, cmp))
	tmp_decl := t.make_decl_assign_typed(tmp_name, t.make_index(base, t.make_ident(j_name),
		elem_type), elem_type)
	prev_idx := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_cur := t.make_index_assign(t.make_index(base, t.make_ident(j_name), elem_type), t.make_index(base,
		prev_idx, elem_type))
	prev_idx2 := t.make_infix(.minus, t.make_ident(j_name), t.make_int_literal(1))
	assign_prev := t.make_index_assign(t.make_index(base, prev_idx2, elem_type),
		t.make_ident(tmp_name))
	dec_j := t.make_expr_stmt(t.make_postfix(t.make_ident(j_name), .dec))
	inner_body := [tmp_decl, assign_cur, assign_prev, dec_j]
	inner_for := t.make_for_stmt(t.make_empty(), inner_cond, t.make_empty(), inner_body, src)
	return t.make_for_stmt(init, cond, post, [j_decl, inner_for], src)
}

// array_sort_less_expr supports array sort less expr handling for Transformer.
fn (mut t Transformer) array_sort_less_expr(base flat.NodeId, elem_type string, idx_name string, cmp_id flat.NodeId) flat.NodeId {
	cur := t.make_index(base, t.make_ident(idx_name), elem_type)
	prev := t.make_index(base, t.make_infix(.minus, t.make_ident(idx_name), t.make_int_literal(1)),
		elem_type)
	if int(cmp_id) >= 0 {
		cmp_node := t.a.nodes[int(cmp_id)]
		if cmp_node.kind == .lambda_expr && cmp_node.children_count >= 3 {
			if cmp := t.array_sort_lambda_expr(cmp_node, cur, prev, elem_type) {
				return cmp
			}
		}
		if cmp := t.array_sort_simple_operator_expr(cmp_node, cur, prev, elem_type) {
			return cmp
		}
		old_a := t.var_type('a')
		old_b := t.var_type('b')
		t.set_var_type('a', elem_type)
		t.set_var_type('b', elem_type)
		raw_cmp := t.substitute_array_sort_vars(cmp_id, cur, prev)
		cmp := t.transform_expr(raw_cmp)
		if old_a.len > 0 {
			t.set_var_type('a', old_a)
		} else {
			t.unset_var_type('a')
		}
		if old_b.len > 0 {
			t.set_var_type('b', old_b)
		} else {
			t.unset_var_type('b')
		}
		return cmp
	}
	if elem_type == 'string' {
		return t.make_call_typed('string__lt', arr2(cur, prev), 'bool')
	}
	if cmp := t.array_sort_struct_less_expr(cur, prev, elem_type) {
		return cmp
	}
	return t.make_infix(.lt, cur, prev)
}

fn (mut t Transformer) array_sort_struct_less_expr(cur flat.NodeId, prev flat.NodeId, elem_type string) ?flat.NodeId {
	mut struct_type := t.struct_lookup_name(elem_type)
	if struct_type.len == 0 {
		struct_type = t.generic_struct_instance_name(elem_type)
	}
	if struct_type.len == 0 {
		return none
	}
	call_info := t.struct_operator_call_info(struct_type, .lt) or { return none }
	args := if call_info.reverse { arr2(prev, cur) } else { arr2(cur, prev) }
	t.mark_fn_used_name(call_info.name)
	call := t.make_call_typed(call_info.name, args, 'bool')
	if call_info.negate {
		return t.make_prefix(.not, call)
	}
	return call
}

fn (mut t Transformer) array_sort_simple_operator_expr(node flat.Node, cur flat.NodeId, prev flat.NodeId, elem_type string) ?flat.NodeId {
	if node.kind != .infix || node.children_count < 2 {
		return none
	}
	lhs_node := t.a.child_node(&node, 0)
	rhs_node := t.a.child_node(&node, 1)
	if lhs_node.kind != .ident || rhs_node.kind != .ident {
		return none
	}
	if lhs_node.value !in ['a', 'b'] || rhs_node.value !in ['a', 'b'] {
		return none
	}
	struct_type := t.struct_lookup_name(elem_type)
	if struct_type.len == 0 {
		return none
	}
	call_info := t.struct_operator_call_info(struct_type, node.op) or { return none }
	lhs := if lhs_node.value == 'a' { cur } else { prev }
	rhs := if rhs_node.value == 'a' { cur } else { prev }
	args := if call_info.reverse { arr2(rhs, lhs) } else { arr2(lhs, rhs) }
	t.mark_fn_used_name(call_info.name)
	call := t.make_call_typed(call_info.name, args, node.typ)
	if call_info.negate {
		return t.make_prefix(.not, call)
	}
	return call
}

// array_sort_compare_less_expr supports array sort compare less expr handling for Transformer.
fn (mut t Transformer) array_sort_compare_less_expr(base flat.NodeId, elem_type string, idx_name string, cmp flat.NodeId) flat.NodeId {
	cur := t.make_index(base, t.make_ident(idx_name), elem_type)
	prev := t.make_index(base, t.make_infix(.minus, t.make_ident(idx_name), t.make_int_literal(1)),
		elem_type)
	cmp_elem_type := if elem_type.starts_with('&') { elem_type } else { '&${elem_type}' }
	cur_arg := if elem_type.starts_with('&') { cur } else { t.make_prefix(.amp, cur) }
	prev_arg := if elem_type.starts_with('&') { prev } else { t.make_prefix(.amp, prev) }
	if int(cmp) >= 0 {
		cmp_node := t.a.nodes[int(cmp)]
		if cmp_node.kind == .lambda_expr && cmp_node.children_count >= 3 {
			if call_value := t.array_sort_lambda_expr(cmp_node, cur_arg, prev_arg, cmp_elem_type) {
				return t.make_infix(.lt, call_value, t.make_int_literal(0))
			}
		}
	}
	call := t.make_call_expr_typed(cmp, arr2(cur_arg, prev_arg), 'int')
	return t.make_infix(.lt, call, t.make_int_literal(0))
}

fn (mut t Transformer) array_sort_lambda_expr(node flat.Node, a_expr flat.NodeId, b_expr flat.NodeId, elem_type string) ?flat.NodeId {
	if node.kind != .lambda_expr || node.children_count < 3 {
		return none
	}
	first := t.a.child_node(&node, 0)
	second := t.a.child_node(&node, 1)
	if first.kind != .ident || second.kind != .ident || first.value.len == 0
		|| second.value.len == 0 {
		return none
	}
	body_id := t.a.child(&node, node.children_count - 1)
	old_a := t.var_type(first.value)
	old_b := t.var_type(second.value)
	t.set_var_type(first.value, elem_type)
	t.set_var_type(second.value, elem_type)
	raw_cmp := t.substitute_array_sort_vars_named(body_id, first.value, second.value, a_expr,
		b_expr)
	cmp := t.transform_expr(raw_cmp)
	if old_a.len > 0 {
		t.set_var_type(first.value, old_a)
	} else {
		t.unset_var_type(first.value)
	}
	if old_b.len > 0 {
		t.set_var_type(second.value, old_b)
	} else {
		t.unset_var_type(second.value)
	}
	return cmp
}

// substitute_array_sort_vars supports substitute array sort vars handling for Transformer.
fn (mut t Transformer) substitute_array_sort_vars(id flat.NodeId, a_expr flat.NodeId, b_expr flat.NodeId) flat.NodeId {
	return t.substitute_array_sort_vars_named(id, 'a', 'b', a_expr, b_expr)
}

fn (mut t Transformer) substitute_array_sort_vars_named(id flat.NodeId, a_name string, b_name string, a_expr flat.NodeId, b_expr flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value == a_name {
			return a_expr
		}
		if node.value == b_name {
			return b_expr
		}
		return id
	}
	if node.children_count == 0 {
		return id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.substitute_array_sort_vars_named(t.a.child(&node, i), a_name, b_name, a_expr,
			b_expr)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
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

// make_index_assign builds make index assign data for transform.
fn (mut t Transformer) make_index_assign(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .index_assign
		op:             .assign
		children_start: start
		children_count: 2
	})
}
