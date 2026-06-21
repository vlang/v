module transform

import v3.flat
import v3.types

fn (mut t Transformer) make_array_new_call(elem_type string, len_expr flat.NodeId, cap_expr flat.NodeId) flat.NodeId {
	return t.make_call_typed('array_new', arr3(t.make_sizeof_type(elem_type), len_expr, cap_expr),
		'[]${elem_type}')
}

fn (mut t Transformer) lower_array_init_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len == 0 || is_fixed_array_type(node.value) {
		return id
	}
	elem_type := node.value
	mut len_expr := t.make_int_literal(0)
	mut cap_expr := t.make_int_literal(0)
	mut init_expr := flat.empty_node
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .field_init && child.children_count > 0 {
			val := t.transform_expr(t.a.child(child, 0))
			if child.value == 'len' {
				len_expr = val
			} else if child.value == 'cap' {
				cap_expr = val
			} else if child.value == 'init' {
				init_expr = val
			}
		}
	}
	new_call := t.make_array_new_call(elem_type, len_expr, cap_expr)
	if int(init_expr) < 0 {
		return new_call
	}
	tmp_name := t.new_temp('arr_init')
	idx_name := t.new_temp('arr_idx')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, new_call, '[]${elem_type}')
	init_idx := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(tmp_name),
		'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_lhs := t.make_index(t.make_ident(tmp_name), t.make_ident(idx_name), elem_type)
	assign := t.make_assign(elem_lhs, init_expr)
	t.pending_stmts << t.make_for_stmt(init_idx, cond, post, arr1(assign), node)
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) lower_array_literal_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	array_type := t.node_type(id)
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
			spread := t.transform_expr(t.a.child(&elem, 0))
			call := t.make_call_typed('array_push_many', arr2(t.make_prefix(.amp,
				t.make_ident(tmp_name)), spread), 'void')
			t.pending_stmts << t.make_expr_stmt(call)
			continue
		}
		value_name := t.new_temp('arr_val')
		value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
			t.wrap_sum_value(elem_id, elem_type)
		} else {
			t.transform_expr(elem_id)
		}
		t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
		call := t.make_call_typed('array_push', arr2(t.make_prefix(.amp, t.make_ident(tmp_name)), t.make_prefix(.amp,
			t.make_ident(value_name))), 'void')
		t.pending_stmts << t.make_expr_stmt(call)
	}
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) transform_array_literal_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	array_type := t.normalize_type_alias(target_type)
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
			call := t.make_call_typed('array_push_many', arr2(t.make_prefix(.amp,
				t.make_ident(tmp_name)), spread), 'void')
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
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) try_lower_array_append_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .infix || node.op != .left_shift || node.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	mut lhs_type := t.lvalue_type(lhs_id)
	lhs_type = t.normalize_type_alias(lhs_type)
	mut array_type := t.clean_array_append_lhs_type(lhs_type)
	if !array_type.starts_with('[]') {
		return none
	}
	elem_type := array_type[2..]
	rhs_id := t.a.child(&node, 1)
	mut rhs_type := t.normalize_type_alias(t.node_type(rhs_id))
	rhs_node := t.a.nodes[int(rhs_id)]
	mut push_many := t.array_append_rhs_is_push_many(lhs_id, rhs_id, rhs_type, elem_type)
	if push_many && rhs_node.kind == .array_literal && !rhs_type.starts_with('[]') {
		t.a.nodes[int(rhs_id)].typ = array_type
		rhs_type = array_type
	}

	mut result := []flat.NodeId{}
	lhs := t.transform_lvalue(lhs_id)
	t.drain_pending(mut result)
	mut rhs := if !push_many
		&& (elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types) {
		t.wrap_sum_value(rhs_id, elem_type)
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
		call := if is_fixed_array_type(rhs_type) {
			t.make_call_typed('array_push_many_ptr', arr3(lhs_addr, rhs,
				t.make_fixed_array_len_expr(rhs_type)), 'void')
		} else {
			t.make_call_typed('array_push_many', arr2(lhs_addr, rhs), 'void')
		}
		result << t.make_expr_stmt(call)
		return result
	}
	value_name := t.new_temp('arr_val')
	result << t.make_decl_assign_typed(value_name, rhs, elem_type)
	result << t.make_expr_stmt(t.make_call_typed('array_push', arr2(lhs_addr, t.make_prefix(.amp,
		t.make_ident(value_name))), 'void'))
	return result
}

fn (t &Transformer) clean_array_append_lhs_type(typ string) string {
	mut clean := t.normalize_type_alias(typ).trim_space()
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

fn (mut t Transformer) lower_array_prepend_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 2 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	value_id := t.a.child(&node, 1)
	base := t.transform_lvalue(base_id)
	value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
		t.wrap_sum_value(value_id, elem_type)
	} else {
		t.transform_expr_for_type(value_id, elem_type)
	}
	value_name := t.new_temp('arr_val')
	t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
	return t.make_call_typed('array__prepend', arr2(t.runtime_addr(base, base_type), t.make_prefix(.amp,
		t.make_ident(value_name))), 'void')
}

fn (mut t Transformer) lower_array_insert_call(node flat.Node, fn_node flat.Node, base_type string, elem_type string) ?flat.NodeId {
	if node.children_count < 3 || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	index_id := t.a.child(&node, 1)
	value_id := t.a.child(&node, 2)
	base := t.transform_lvalue(base_id)
	index := t.transform_expr_for_type(index_id, 'int')
	value := if elem_type in t.sum_types || t.resolve_sum_name(elem_type) in t.sum_types {
		t.wrap_sum_value(value_id, elem_type)
	} else {
		t.transform_expr_for_type(value_id, elem_type)
	}
	value_name := t.new_temp('arr_val')
	t.pending_stmts << t.make_decl_assign_typed(value_name, value, elem_type)
	return t.make_call_typed('array__insert', arr3(t.runtime_addr(base, base_type), index, t.make_prefix(.amp,
		t.make_ident(value_name))), 'void')
}

fn (t &Transformer) array_append_rhs_is_push_many(lhs_id flat.NodeId, rhs_id flat.NodeId, rhs_type string, elem_type string) bool {
	clean_rhs_type := rhs_type.trim_space()
	if clean_rhs_type.starts_with('[]') {
		if t.array_append_elem_types_match(clean_rhs_type[2..], elem_type) {
			return true
		}
		if declared_rhs_type := t.array_append_ident_type(rhs_id) {
			if declared_rhs_type.starts_with('[]') {
				return t.array_append_elem_types_match(declared_rhs_type[2..], elem_type)
			}
		}
		return false
	}
	if is_fixed_array_type(clean_rhs_type) {
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
	if isnil(t.tc) {
		return false
	}
	return t.array_append_elem_c_type(rhs_clean) == t.array_append_elem_c_type(lhs_clean)
}

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

fn (mut t Transformer) array_get_value(base flat.NodeId, index flat.NodeId, elem_type string) flat.NodeId {
	get_call := t.make_call_typed('array_get', arr2(base, index), 'voidptr')
	ptr := t.make_cast('&${elem_type}', get_call, '&${elem_type}')
	value := t.make_prefix(.mul, ptr)
	t.a.nodes[int(value)].typ = elem_type
	return value
}

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
	if predicate_node.kind == .lambda_expr && predicate_node.children_count > 0 {
		predicate_expr_id = t.a.child(&predicate_node, predicate_node.children_count - 1)
		if predicate_node.children_count > 1 {
			param := t.a.child_node(&predicate_node, 0)
			if param.kind == .ident && param.value.len > 0 {
				lambda_param = param.value
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
	predicate := t.transform_expr(predicate_source)
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	t.drain_pending(mut loop_body)
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

fn (mut t Transformer) lower_array_map_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count < 2 || !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	map_expr_id := t.a.child(&node, 1)
	map_expr := t.a.nodes[int(map_expr_id)]
	mut map_fn_name := ''
	elem_name := t.new_temp('map_it')
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	mapped_source := t.substitute_ident(map_expr_id, 'it', elem_name)
	mut result_elem_type := t.node_type(map_expr_id)
	mut direct_selector_type := ''
	mapped_source_node := t.a.nodes[int(mapped_source)]
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
		}
	}
	if result_elem_type.len == 0 {
		result_elem_type = t.reliable_stringify_type(map_expr_id)
	}
	saved_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mapped_expr := if map_fn_name.len > 0 {
		t.make_call_typed(map_fn_name, arr1(t.make_ident(elem_name)), result_elem_type)
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
	if mapped_type.len > 0 && mapped_type != 'unknown' {
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
	t.a.nodes[int(result)].typ = out_type
	return result
}

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

fn (mut t Transformer) substitute_ident(id flat.NodeId, name string, replacement string) flat.NodeId {
	if int(id) < 0 || name.len == 0 || replacement.len == 0 || name == replacement {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		new_id := t.make_ident(replacement)
		t.a.nodes[int(new_id)].typ = node.typ
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
	})
}

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
	elem_name := t.new_temp('count_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	predicate_id := t.a.child(&node, 1)
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	predicate := t.transform_expr(t.substitute_ident(predicate_id, 'it', elem_name))
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
	elem_name := t.new_temp('${method}_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	predicate_id := t.a.child(&node, 1)
	old_elem := t.var_type(elem_name)
	t.set_var_type(elem_name, elem_type)
	predicate := t.transform_expr(t.substitute_ident(predicate_id, 'it', elem_name))
	if old_elem.len > 0 {
		t.set_var_type(elem_name, old_elem)
	} else {
		t.unset_var_type(elem_name)
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	t.drain_pending(mut loop_body)
	if method == 'all' {
		not_predicate := t.make_prefix(.not, predicate)
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

fn (mut t Transformer) lower_array_sorted_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count > 2 || !base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_call_typed('array_clone', arr1(t.transform_expr(base_id)), base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, base_type)
	cmp_id := if node.children_count > 1 { t.a.child(&node, 1) } else { flat.empty_node }
	t.pending_stmts << t.make_array_default_sort_stmt(t.make_ident(clone_name), base_type[2..],
		node, cmp_id)
	return t.make_ident(clone_name)
}

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

fn (mut t Transformer) lower_array_sorted_with_compare_call(node flat.Node, fn_node flat.Node, base_type string) ?flat.NodeId {
	if node.children_count != 2 || !base_type.starts_with('[]') {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	clone_name := t.new_temp('sorted')
	clone_call := t.make_call_typed('array_clone', arr1(t.transform_expr(base_id)), base_type)
	t.pending_stmts << t.make_decl_assign_typed(clone_name, clone_call, base_type)
	elem_type := base_type[2..]
	cmp := t.stable_array_compare_fn(t.a.child(&node, 1), elem_type)
	t.pending_stmts << t.make_array_compare_sort_stmt(t.make_ident(clone_name), elem_type, node, cmp)
	return t.make_ident(clone_name)
}

fn (mut t Transformer) stable_array_compare_fn(cmp_id flat.NodeId, elem_type string) flat.NodeId {
	cmp := t.transform_expr(cmp_id)
	cmp_type := 'fn (&${elem_type}, &${elem_type}) int'
	return t.stable_transformed_expr_for_reuse(cmp, cmp_type, 'sort_cmp')
}

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

fn (mut t Transformer) array_sort_less_expr(base flat.NodeId, elem_type string, idx_name string, cmp_id flat.NodeId) flat.NodeId {
	cur := t.make_index(base, t.make_ident(idx_name), elem_type)
	prev := t.make_index(base, t.make_infix(.minus, t.make_ident(idx_name), t.make_int_literal(1)),
		elem_type)
	if int(cmp_id) >= 0 {
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

fn (mut t Transformer) array_sort_compare_less_expr(base flat.NodeId, elem_type string, idx_name string, cmp flat.NodeId) flat.NodeId {
	cur := t.make_index(base, t.make_ident(idx_name), elem_type)
	prev := t.make_index(base, t.make_infix(.minus, t.make_ident(idx_name), t.make_int_literal(1)),
		elem_type)
	call := t.make_call_expr_typed(cmp, arr2(t.make_prefix(.amp, cur), t.make_prefix(.amp, prev)),
		'int')
	return t.make_infix(.lt, call, t.make_int_literal(0))
}

fn (mut t Transformer) substitute_array_sort_vars(id flat.NodeId, a_expr flat.NodeId, b_expr flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		if node.value == 'a' {
			return a_expr
		}
		if node.value == 'b' {
			return b_expr
		}
		return id
	}
	if node.children_count == 0 {
		return id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.substitute_array_sort_vars(t.a.child(&node, i), a_expr, b_expr)
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
