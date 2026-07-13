module transform

import v3.flat
import v3.types

// make_array_new_call builds make array new call data for transform.
fn (mut t Transformer) make_array_new_call(elem_type string, len_expr flat.NodeId, cap_expr flat.NodeId) flat.NodeId {
	return t.make_call_typed('array_new', arr3(t.make_sizeof_type(elem_type), len_expr, cap_expr),
		'[]${elem_type}')
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
	depth := array_repeat_clone_depth(base_type)
	if depth == 0 {
		return none
	}
	base := t.transform_expr(base_id)
	count := t.transform_expr(count_id)
	selector := t.make_selector(base, 'repeat_to_depth', '')
	return t.make_call_expr_typed(selector, arr2(count, t.make_int_literal(depth)), node.typ)
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
	receiver := t.transform_expr(base_id)
	return t.make_array_clone_value(receiver, base_type)
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

// lower_array_init_to_runtime converts lower array init to runtime data for transform.
fn (mut t Transformer) lower_array_init_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len == 0 {
		return id
	}
	clean_value := t.normalize_type_alias(node.value)
	if t.is_fixed_array_type(clean_value) && !node.typ.starts_with('[]') {
		return id
	}
	elem_type := if node.typ.starts_with('[]') {
		node.typ[2..]
	} else if !node.value.starts_with('[]') && clean_value.starts_with('[]') {
		clean_value[2..]
	} else {
		node.value
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
	for field in info.fields {
		field_type := if field.typ.len > 0 { field.typ } else { field.raw_typ }
		clean_type := t.normalize_type_alias(field_type)
		mut value := flat.empty_node
		if clean_type.starts_with('map[') || clean_type.starts_with('[]') {
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
	array_type := if checker_alias_type := t.array_literal_checker_alias_type(id) {
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
			spread := t.transform_expr(spread_id)
			spread_type := if t.node_type(spread_id).len > 0 {
				t.node_type(spread_id)
			} else {
				array_type
			}
			call := t.make_array_push_many_call(t.make_prefix(.amp, t.make_ident(tmp_name)),
				spread, spread_type)
			t.pending_stmts << t.make_expr_stmt(call)
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
	if !t.generic_arg_is_alias_name(elem, t.cur_module) {
		return none
	}
	return '[]${t.array_literal_qualified_alias_name(elem)}'
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
			spread := t.transform_expr_for_type(t.a.child(&elem, 0), array_type)
			call := t.make_array_push_many_call(t.make_prefix(.amp, t.make_ident(tmp_name)),
				spread, array_type)
			t.pending_stmts << t.make_expr_stmt(call)
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
	mut values := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		elem_id := t.a.child(&node, i)
		values << t.transform_expr_for_type(elem_id, elem_type)
	}
	return t.make_array_literal_typed(values, fixed_type)
}

fn (mut t Transformer) transform_fixed_array_init_expr(node flat.Node) ?flat.NodeId {
	fixed_type := t.normalize_type_alias(if node.typ.len > 0 { node.typ } else { node.value })
	if !t.is_fixed_array_type(fixed_type) || node.children_count == 0 {
		return none
	}
	len_text := fixed_array_len_text(fixed_type)
	len := if is_decimal_text(len_text) {
		len_text.int()
	} else if !isnil(t.tc) {
		t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) or { return none }
	} else {
		return none
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
	elem_type := fixed_array_elem_type(fixed_type)
	mut values := []flat.NodeId{cap: len}
	for i in 0 .. len {
		indexed_init := t.substitute_ident_expr(init_id, 'index', t.make_int_literal(i))
		values << t.transform_expr_for_type(indexed_init, elem_type)
	}
	return t.make_array_literal_typed(values, fixed_type)
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

// try_lower_array_append_stmt supports try lower array append stmt handling for Transformer.
fn (mut t Transformer) try_lower_array_append_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
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
	mut rhs_type := t.normalize_type_alias(t.node_type(rhs_id))
	rhs_node := t.a.nodes[int(rhs_id)]
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
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
	mut rhs := if !push_many {
		if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
			t.wrap_sum_value(rhs_id, elem_type)
		} else {
			t.transform_expr_for_type(rhs_id, elem_type)
		}
	} else {
		t.transform_expr(rhs_id)
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
	result << t.make_decl_assign_typed(value_name, rhs, elem_type)
	result << t.make_expr_stmt(t.make_call_typed('array_push', arr2(lhs_addr, t.make_prefix(.amp,
		t.make_ident(value_name))), 'void'))
	return result
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
	mut rhs_type := t.normalize_type_alias(t.node_type(rhs_id))
	rhs_node := t.a.nodes[int(rhs_id)]
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
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

	mut rhs := if !push_many {
		if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
			t.wrap_sum_value(rhs_id, elem_type)
		} else {
			t.transform_expr_for_type(rhs_id, elem_type)
		}
	} else {
		t.transform_expr(rhs_id)
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
	mut rhs_type := t.normalize_type_alias(t.node_type(value_id))
	value_node := t.a.nodes[int(value_id)]
	mut prepend_many := t.array_append_rhs_is_push_many(base_id, value_id, rhs_type, elem_type)
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
	mut rhs_type := t.normalize_type_alias(t.node_type(value_id))
	value_node := t.a.nodes[int(value_id)]
	mut insert_many := t.array_append_rhs_is_push_many(base_id, value_id, rhs_type, elem_type)
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
	if clean_rhs_type.starts_with('...') {
		return t.array_append_elem_types_match(clean_rhs_type[3..], elem_type)
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
				return t.array_append_elem_types_match(rhs_clean.elem_type.name(), elem_type)
			}
			if rhs_clean is types.ArrayFixed {
				return t.array_append_elem_types_match(rhs_clean.elem_type.name(), elem_type)
			}
		}
		if lhs_resolved := t.tc.expr_type(lhs_id) {
			lhs_clean := types.unwrap_pointer(lhs_resolved)
			if lhs_clean is types.Array && clean_rhs_type in ['array', 'Array'] {
				return t.tc.c_type(lhs_clean.elem_type) == 'void*'
			}
		}
	}
	if clean_rhs_type in ['array', 'Array'] {
		return t.array_append_elem_c_type(elem_type) !in ['array', 'Array']
	}
	return false
}

fn (t &Transformer) array_append_literal_should_push_many(rhs_id flat.NodeId, elem_type string) bool {
	if int(rhs_id) < 0 {
		return false
	}
	node := t.a.nodes[int(rhs_id)]
	if node.kind != .array_literal {
		return false
	}
	if !elem_type.starts_with('[]') && !t.is_fixed_array_type(elem_type) {
		return true
	}
	return t.array_append_literal_children_match_elem(rhs_id, elem_type)
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
	return t.array_append_elem_c_type(rhs_clean) == t.array_append_elem_c_type(lhs_clean)
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
	get_call := t.make_call_typed('array_get', arr2(t.array_get_runtime_base(base), index),
		'voidptr')
	ptr := t.make_cast('&${elem_type}', get_call, '&${elem_type}')
	value := t.make_prefix(.mul, ptr)
	t.set_node_typ(int(value), elem_type)
	return value
}

// array_get_ptr supports array get ptr handling for Transformer.
fn (mut t Transformer) array_get_ptr(base flat.NodeId, index flat.NodeId, elem_type string) flat.NodeId {
	get_call := t.make_call_typed('array_get', arr2(t.array_get_runtime_base(base), index),
		'voidptr')
	return t.make_cast('&${elem_type}', get_call, '&${elem_type}')
}

fn (mut t Transformer) array_get_runtime_base(base flat.NodeId) flat.NodeId {
	base_type := t.node_type(base).trim_space()
	if base_type.starts_with('&') {
		value := t.make_prefix(.mul, base)
		t.set_node_typ(int(value), base_type[1..])
		return value
	}
	return base
}

// lower_array_filter_call builds lower array filter call data for transform.
fn (mut t Transformer) lower_array_filter_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	base_id := t.a.child(&fn_node, 0)
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	out_name := t.new_temp('filter')
	idx_name := t.new_temp('filter_idx')
	prefix << t.make_decl_assign_typed(out_name, t.make_array_new_call(elem_type,
		t.make_int_literal(0), t.make_selector(base, 'len', 'int')), base_type)
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
	} else if predicate_node.kind == .ident {
		if fn_name := t.resolve_fn_value_ident(predicate_node.value) {
			predicate_fn_name = fn_name
		} else if ret_type := t.fn_value_return_type_name(predicate_id) {
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
	push_call := t.make_call_typed('array_push', arr2(t.make_prefix(.amp, t.make_ident(out_name)), t.make_prefix(.amp,
		t.make_ident(elem_name))), 'void')
	then_block := t.make_block(arr1(t.make_expr_stmt(push_call)))
	loop_body << t.make_if(predicate, then_block, t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
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
		selector_type := t.resolve_selector_type(mapped_source_node)
		if selector_type.len > 0 {
			direct_selector_type = selector_type
			result_elem_type = selector_type
		}
	}
	if map_expr.kind == .ident {
		if fn_name := t.resolve_fn_value_ident(map_expr.value) {
			map_fn_name = fn_name
			if ret := t.fn_ret_types[fn_name] {
				result_elem_type = ret
			} else if !isnil(t.tc) {
				if ret_type := t.tc.fn_ret_types[fn_name] {
					result_elem_type = t.normalize_type_alias(ret_type.name())
				}
			}
		} else if ret_type := t.fn_value_return_type_name(map_expr_id) {
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
	if direct_selector_type.len > 0 {
		result_elem_type = direct_selector_type
	}
	if mapped_expr_node := t.selector_expr_node(mapped_expr) {
		selector_type := t.resolve_selector_type(mapped_expr_node)
		if selector_type.len > 0 {
			result_elem_type = selector_type
		}
	}
	if result_elem_type.len == 0 {
		result_elem_type = elem_type
	}
	out_type := '[]${result_elem_type}'
	base_id := t.a.child(&fn_node, 0)
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	out_name := t.new_temp('map')
	idx_name := t.new_temp('map_idx')
	prefix << t.make_decl_assign_typed(out_name, t.make_array_new_call(result_elem_type,
		t.make_int_literal(0), t.make_selector(base, 'len', 'int')), out_type)
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
	loop_body << t.make_expr_stmt(t.make_call_typed('array_push', arr2(t.make_prefix(.amp,
		t.make_ident(out_name)), t.make_prefix(.amp, t.make_ident(value_name))), 'void'))
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), out_type)
	return result
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
		kind_id:        node.kind_id
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		generic_params: node.generic_params.clone()
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
		new_children << t.substitute_ident_expr(t.a.child(&node, i), name, replacement)
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		kind_id:        node.kind_id
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
		generic_params: node.generic_params.clone()
	})
}

fn (mut t Transformer) infer_map_init_entry_type(node flat.Node) string {
	if node.kind != .map_init || node.children_count < 2 {
		return ''
	}
	key_type := t.node_type(t.a.child(&node, 0))
	value_type := t.node_type(t.a.child(&node, 1))
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
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('count')
	idx_name := t.new_temp('count_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(0), 'int')
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
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
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
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// lower_array_sort_call builds lower array sort call data for transform.
fn (mut t Transformer) lower_array_sort_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if !base_type.starts_with('[]') && !(base_type.starts_with('&')
		&& base_type[1..].starts_with('[]')) {
		return none
	}
	if node.children_count > 2 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.transform_lvalue(base_id)
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	elem_type := clean_type[2..]
	cmp_id := if node.children_count > 1 { t.a.child(&node, 1) } else { flat.empty_node }
	t.pending_stmts << t.make_array_default_sort_stmt(base, elem_type, node, cmp_id)
	return t.make_empty()
}

// lower_array_sorted_call builds lower array sorted call data for transform.
fn (mut t Transformer) lower_array_sorted_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count > 2 || !base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_array_clone_call(base_id, base_type)
	t.set_var_type(clone_name, base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, base_type)
	cmp_id := if node.children_count > 1 { t.a.child(&node, 1) } else { flat.empty_node }
	t.pending_stmts << t.make_array_default_sort_stmt(t.make_ident(clone_name), base_type[2..],
		node, cmp_id)
	return t.make_ident(clone_name)
}

// lower_array_sort_with_compare_call builds lower array sort with compare call data for transform.
fn (mut t Transformer) lower_array_sort_with_compare_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count != 2
		|| (!base_type.starts_with('[]') && !(base_type.starts_with('&')
		&& base_type[1..].starts_with('[]'))) {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.transform_lvalue(base_id)
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	elem_type := clean_type[2..]
	cmp := t.stable_array_compare_fn(t.a.child(&node, 1), elem_type)
	t.pending_stmts << t.make_array_compare_sort_stmt(base, elem_type, node, cmp)
	return t.make_empty()
}

// lower_array_sorted_with_compare_call supports lower_array_sorted_with_compare_call handling.
fn (mut t Transformer) lower_array_sorted_with_compare_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count != 2 || !base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_array_clone_call(base_id, base_type)
	elem_type := base_type[2..]
	cmp := t.stable_array_compare_fn(t.a.child(&node, 1), elem_type)
	t.set_var_type(clone_name, base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, base_type)
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
	return t.make_infix(.lt, cur, prev)
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
	if int(cmp) >= 0 {
		cmp_node := t.a.nodes[int(cmp)]
		if cmp_node.kind == .lambda_expr && cmp_node.children_count >= 3 {
			cur_ptr := t.make_prefix(.amp, cur)
			prev_ptr := t.make_prefix(.amp, prev)
			if call_value := t.array_sort_lambda_expr(cmp_node, cur_ptr, prev_ptr, '&${elem_type}') {
				return t.make_infix(.lt, call_value, t.make_int_literal(0))
			}
		}
	}
	call := t.make_call_expr_typed(cmp, arr2(t.make_prefix(.amp, cur), t.make_prefix(.amp, prev)),
		'int')
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
