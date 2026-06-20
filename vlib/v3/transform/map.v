module transform

import v3.flat

struct MapIndexInfo {
	base_id    flat.NodeId
	key_id     flat.NodeId
	base_type  string
	key_type   string
	value_type string
}

fn (mut t Transformer) map_type_parts(map_type string) (string, string) {
	clean := t.clean_map_type(map_type)
	if !clean.starts_with('map[') {
		return '', ''
	}
	return t.map_key_type(clean), t.map_value_type(clean)
}

fn (mut t Transformer) make_new_map_call(map_type string) flat.NodeId {
	key_type, value_type := t.map_type_parts(map_type)
	hash_fn, eq_fn, clone_fn, free_fn := map_callback_names(key_type)
	mut args := []flat.NodeId{}
	args << t.make_sizeof_type(key_type)
	args << t.make_sizeof_type(value_type)
	args << t.make_ident(hash_fn)
	args << t.make_ident(eq_fn)
	args << t.make_ident(clone_fn)
	args << t.make_ident(free_fn)
	return t.make_call_typed('new_map', args, map_type)
}

fn map_callback_names(key_type string) (string, string, string, string) {
	if key_type == 'string' {
		return 'v3_map_hash_string', 'v3_map_eq_string', 'v3_map_clone_string', 'v3_map_free_string'
	}
	size_suffix := match key_type {
		'u8', 'i8', 'bool', 'char' { '1' }
		'u16', 'i16' { '2' }
		'i64', 'u64', 'isize', 'usize', 'voidptr' { '8' }
		else { '4' }
	}

	return 'v3_map_hash_int_${size_suffix}', 'v3_map_eq_int_${size_suffix}', 'v3_map_clone_int_${size_suffix}', 'v3_map_free_nop'
}

fn (mut t Transformer) map_index_info(index_id flat.NodeId) ?MapIndexInfo {
	if int(index_id) < 0 {
		return none
	}
	lhs := t.a.nodes[int(index_id)]
	if lhs.kind != .index || lhs.children_count < 2 || lhs.value == 'range' {
		return none
	}
	base_id := t.a.child(&lhs, 0)
	key_id := t.a.child(&lhs, 1)
	base_type := t.node_type(base_id)
	map_type := t.clean_map_type(base_type)
	if !map_type.starts_with('map[') {
		return none
	}
	key_type, value_type := t.map_type_parts(map_type)
	if key_type.len == 0 || value_type.len == 0 {
		return none
	}
	return MapIndexInfo{
		base_id:    base_id
		key_id:     key_id
		base_type:  base_type
		key_type:   key_type
		value_type: value_type
	}
}

fn (mut t Transformer) make_map_get_expr(map_expr flat.NodeId, base_type string, key_name string, zero_name string, value_type string) flat.NodeId {
	call := t.make_call_typed('map__get', arr3(t.runtime_addr(map_expr, base_type), t.make_prefix(.amp,
		t.make_ident(key_name)), t.make_prefix(.amp, t.make_ident(zero_name))), 'voidptr')
	cast := t.make_cast('&${value_type}', call, '&${value_type}')
	return t.make_prefix(.mul, cast)
}

fn (mut t Transformer) make_map_get_check_expr(map_expr flat.NodeId, base_type string, key_name string) flat.NodeId {
	return t.make_call_typed('map__get_check', arr2(t.runtime_addr(map_expr, base_type), t.make_prefix(.amp,
		t.make_ident(key_name))), 'voidptr')
}

fn (mut t Transformer) make_map_set_stmt(map_expr flat.NodeId, base_type string, key_name string, value_name string) flat.NodeId {
	call := t.make_call_typed('map__set', arr3(t.runtime_addr(map_expr, base_type), t.make_prefix(.amp,
		t.make_ident(key_name)), t.make_prefix(.amp, t.make_ident(value_name))), 'void')
	return t.make_expr_stmt(call)
}

fn (mut t Transformer) try_lower_map_index_expr(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.kind != .index || node.children_count < 2 || node.value == 'range' {
		return none
	}
	base_id := t.a.child(&node, 0)
	key_id := t.a.child(&node, 1)
	base_type := t.node_type(base_id)
	map_type := t.clean_map_type(base_type)
	if !map_type.starts_with('map[') {
		return none
	}
	key_type, value_type := t.map_type_parts(map_type)
	if key_type.len == 0 || value_type.len == 0 {
		return none
	}
	map_expr := t.stable_expr_for_reuse(base_id)
	key_name := t.new_temp('map_key')
	zero_name := t.new_temp('map_zero')
	t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr(key_id), key_type)
	t.pending_stmts << t.make_decl_assign_typed(zero_name, t.zero_value_for_type(value_type),
		value_type)
	return t.make_map_get_expr(map_expr, base_type, key_name, zero_name, value_type)
}

fn (mut t Transformer) is_map_index_or_expr(node flat.Node) bool {
	if node.kind != .or_expr || node.children_count < 2 {
		return false
	}
	expr := t.a.child_node(&node, 0)
	if expr.kind != .index || expr.children_count < 2 || expr.value == 'range' {
		return false
	}
	base_id := t.a.child(expr, 0)
	base_type := t.node_type(base_id)
	return t.clean_map_type(base_type).starts_with('map[')
}

fn (mut t Transformer) transform_map_index_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	body_id := t.a.child(&node, 1)
	info := t.map_index_info(expr_id) or { return id }
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	ptr_name := t.new_temp('map_ptr')
	val_name := t.new_temp('map_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	key_expr := t.transform_expr(info.key_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(key_name, key_expr, info.key_type)
	prelude << t.make_decl_assign_typed(ptr_name, t.make_map_get_check_expr(map_expr,
		info.base_type, key_name), 'voidptr')
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.value_type),
		info.value_type)

	ptr_ident := t.make_ident(ptr_name)
	found_cond := t.make_infix(.ne, ptr_ident, t.a.add(.nil_literal))
	ptr_value := t.make_prefix(.mul, t.make_cast('&${info.value_type}', t.make_ident(ptr_name),
		'&${info.value_type}'))
	then_block := t.make_block(arr1(t.make_assign(t.make_ident(val_name), ptr_value)))
	else_block := t.make_block(t.lower_map_or_body_to_stmts(body_id, val_name, info.value_type))
	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	t.pending_stmts << t.make_if(found_cond, then_block, else_block)
	return t.make_ident(val_name)
}

fn (mut t Transformer) lower_map_or_body_to_stmts(body_id flat.NodeId, target_name string, target_type string) []flat.NodeId {
	if int(body_id) < 0 {
		return []flat.NodeId{}
	}
	body := t.a.nodes[int(body_id)]
	if body.kind != .block {
		return arr1(t.make_assign(t.make_ident(target_name), t.transform_expr(body_id)))
	}
	mut result := []flat.NodeId{}
	for i in 0 .. body.children_count {
		child_id := t.a.child(&body, i)
		child := t.a.nodes[int(child_id)]
		is_last := i == body.children_count - 1
		if is_last && child.kind == .expr_stmt && child.children_count > 0 {
			inner_id := t.a.child(&child, 0)
			if t.node_type(inner_id) == 'void' {
				expanded := t.transform_stmt(child_id)
				t.drain_pending(mut result)
				for eid in expanded {
					result << eid
				}
			} else {
				value := t.transform_expr(inner_id)
				t.drain_pending(mut result)
				result << t.make_assign(t.make_ident(target_name), value)
			}
		} else {
			expanded := t.transform_stmt(child_id)
			t.drain_pending(mut result)
			for eid in expanded {
				result << eid
			}
		}
	}
	_ = target_type
	return result
}

fn (mut t Transformer) try_lower_map_index_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .index_assign || node.children_count < 2 {
		return none
	}
	info := t.map_index_info(t.a.child(&node, 0)) or { return none }
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, t.transform_expr(info.key_id), info.key_type)
	rhs_id := t.a.child(&node, 1)
	if node.op == .assign {
		value_name := t.new_temp('map_val')
		result << t.make_decl_assign_typed(value_name, t.transform_expr(rhs_id), info.value_type)
		result << t.make_map_set_stmt(map_expr, info.base_type, key_name, value_name)
		return result
	}
	if node.op == .left_shift_assign && info.value_type.starts_with('[]') {
		t.lower_map_index_append_with_info(info, map_expr, key_name, rhs_id, mut result)
		return result
	}
	op := map_compound_to_infix_op(node.op) or { return none }
	t.lower_map_index_compound_with_info(info, map_expr, key_name, op, rhs_id, mut result)
	return result
}

fn map_compound_to_infix_op(op flat.Op) ?flat.Op {
	match op {
		.plus_assign { return flat.Op.plus }
		.minus_assign { return flat.Op.minus }
		.mul_assign { return flat.Op.mul }
		.div_assign { return flat.Op.div }
		.mod_assign { return flat.Op.mod }
		.amp_assign { return flat.Op.amp }
		.pipe_assign { return flat.Op.pipe }
		.xor_assign { return flat.Op.xor }
		.left_shift_assign { return flat.Op.left_shift }
		.right_shift_assign { return flat.Op.right_shift }
		else { return none }
	}
}

fn (mut t Transformer) load_map_index_current(info MapIndexInfo, map_expr flat.NodeId, key_name string, mut result []flat.NodeId) string {
	zero_name := t.new_temp('map_zero')
	current_name := t.new_temp('map_val')
	result << t.make_decl_assign_typed(zero_name, t.zero_value_for_type(info.value_type),
		info.value_type)
	get_expr := t.make_map_get_expr(map_expr, info.base_type, key_name, zero_name, info.value_type)
	result << t.make_decl_assign_typed(current_name, get_expr, info.value_type)
	return current_name
}

fn (mut t Transformer) lower_map_index_compound_with_info(info MapIndexInfo, map_expr flat.NodeId, key_name string, op flat.Op, rhs_id flat.NodeId, mut result []flat.NodeId) {
	current_name := t.load_map_index_current(info, map_expr, key_name, mut result)
	rhs := t.transform_expr(rhs_id)
	new_value := if info.value_type == 'string' && op == .plus {
		t.make_call_typed('string__plus', arr2(t.make_ident(current_name), rhs), 'string')
	} else {
		t.make_infix(op, t.make_ident(current_name), rhs)
	}
	result << t.make_assign(t.make_ident(current_name), new_value)
	result << t.make_map_set_stmt(map_expr, info.base_type, key_name, current_name)
}

fn (mut t Transformer) lower_map_index_postfix_with_info(info MapIndexInfo, map_expr flat.NodeId, key_name string, op flat.Op, mut result []flat.NodeId) {
	current_name := t.load_map_index_current(info, map_expr, key_name, mut result)
	infix_op := if op == .dec { flat.Op.minus } else { flat.Op.plus }
	new_value := t.make_infix(infix_op, t.make_ident(current_name), t.make_int_literal(1))
	result << t.make_assign(t.make_ident(current_name), new_value)
	result << t.make_map_set_stmt(map_expr, info.base_type, key_name, current_name)
}

fn (mut t Transformer) lower_map_index_append_with_info(info MapIndexInfo, map_expr flat.NodeId, key_name string, rhs_id flat.NodeId, mut result []flat.NodeId) {
	current_name := t.load_map_index_current(info, map_expr, key_name, mut result)
	append := t.make_infix(.left_shift, t.make_ident(current_name), t.transform_expr(rhs_id))
	t.annotate_left_shift(append)
	result << t.make_expr_stmt(append)
	result << t.make_map_set_stmt(map_expr, info.base_type, key_name, current_name)
}

fn (mut t Transformer) try_lower_map_index_postfix_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .postfix || node.children_count == 0 || node.op !in [.inc, .dec] {
		return none
	}
	info := t.map_index_info(t.a.child(&node, 0)) or { return none }
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, t.transform_expr(info.key_id), info.key_type)
	t.lower_map_index_postfix_with_info(info, map_expr, key_name, node.op, mut result)
	return result
}

fn (mut t Transformer) try_lower_map_index_append_stmt(id flat.NodeId) ?[]flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .infix || node.op != .left_shift || node.children_count < 2 {
		return none
	}
	info := t.map_index_info(t.a.child(&node, 0)) or { return none }
	if !info.value_type.starts_with('[]') {
		return none
	}
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, t.transform_expr(info.key_id), info.key_type)
	t.lower_map_index_append_with_info(info, map_expr, key_name, t.a.child(&node, 1), mut result)
	return result
}

fn (mut t Transformer) lower_map_init_to_runtime(id flat.NodeId, node flat.Node) flat.NodeId {
	map_type := if node.value.len > 0 { node.value } else { node.typ }
	if !map_type.starts_with('map[') {
		return id
	}
	init_call := t.make_new_map_call(map_type)
	if node.children_count == 0 {
		return init_call
	}
	tmp_name := t.new_temp('map_lit')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, init_call, map_type)
	key_type, value_type := t.map_type_parts(map_type)
	for i := 0; i + 1 < node.children_count; i += 2 {
		key_name := t.new_temp('map_key')
		value_name := t.new_temp('map_val')
		t.pending_stmts << t.make_decl_assign_typed(key_name,
			t.transform_expr(t.a.child(&node, i)), key_type)
		t.pending_stmts << t.make_decl_assign_typed(value_name, t.transform_expr(t.a.child(&node,

			i + 1)), value_type)
		call := t.make_call_typed('map__set', arr3(t.make_prefix(.amp, t.make_ident(tmp_name)), t.make_prefix(.amp,
			t.make_ident(key_name)), t.make_prefix(.amp, t.make_ident(value_name))), 'void')
		t.pending_stmts << t.make_expr_stmt(call)
	}
	return t.make_ident(tmp_name)
}
