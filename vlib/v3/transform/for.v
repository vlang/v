module transform

import v3.flat
import v3.types

struct IteratorForInInfo {
	elem_type   string
	next_method string
}

// transform_for_body transforms transform for body data for transform.
fn (mut t Transformer) transform_for_body(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count < 3 {
		return arr1(id)
	}
	// child 0: init statement
	init_id := t.a.child(&node, 0)
	mut new_init := init_id
	if int(init_id) >= 0 {
		expanded := t.transform_stmt(init_id)
		if expanded.len > 0 {
			new_init = expanded[0]
		}
	}
	// child 1: condition expression
	cond_id := t.a.child(&node, 1)
	cond_smartcasts := t.extract_all_is_exprs(cond_id)
	new_cond := t.transform_and_chain_smartcasts(cond_id)
	mut cond_prefix := []flat.NodeId{}
	t.drain_pending(mut cond_prefix)
	// child 2: post statement
	post_id := t.a.child(&node, 2)
	mut new_post := post_id
	if int(post_id) >= 0 {
		expanded := t.transform_stmt(post_id)
		if expanded.len > 0 {
			new_post = expanded[0]
		}
	}
	// children 3..n: body statements
	mut body_ids := []flat.NodeId{}
	for i in 3 .. node.children_count {
		body_ids << t.a.child(&node, i)
	}
	for info in cond_smartcasts {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	new_body := t.transform_stmts(body_ids)
	for _ in cond_smartcasts {
		t.pop_smartcast()
	}
	mut loop_cond := new_cond
	mut loop_body := new_body.clone()
	if cond_prefix.len > 0 {
		mut guarded_body := []flat.NodeId{cap: cond_prefix.len + new_body.len + 1}
		for stmt in cond_prefix {
			guarded_body << stmt
		}
		not_cond := t.make_prefix(.not, t.make_paren(new_cond))
		break_block := t.make_block(arr1(t.a.add(.break_stmt)))
		guarded_body << t.make_if(not_cond, break_block, t.make_block([]flat.NodeId{}))
		for stmt in new_body {
			guarded_body << stmt
		}
		loop_cond = t.make_bool_literal(true)
		loop_body = guarded_body.clone()
	}
	// Rebuild the for_stmt with transformed children
	start := t.a.children.len
	t.a.children << new_init
	t.a.children << loop_cond
	t.a.children << new_post
	for bid in loop_body {
		t.a.children << bid
	}
	count := t.a.children.len - start
	new_id := t.a.add_node(flat.Node{
		kind:           .for_stmt
		op:             node.op
		children_start: start
		children_count: flat.child_count(count)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return arr1(new_id)
}

// transform_for_in_body transforms transform for in body data for transform.
fn (mut t Transformer) transform_for_in_body(id flat.NodeId, node flat.Node) []flat.NodeId {
	header_count := node.value.int()
	if header_count < 3 || node.children_count < 3 {
		return arr1(id)
	}
	key_id := t.a.child(&node, 0) // loop var ident — pass through (do not transform a binding)
	val_id := t.a.child(&node, 1) // may be flat.empty_node (-1)
	container_id := t.a.child(&node, 2)
	container_is_range := if int(container_id) >= 0 {
		t.a.nodes[int(container_id)].kind == .range
	} else {
		false
	}
	if header_count == 4 {
		range_end_id := t.a.child(&node, 3)
		body_ids := t.a.children_of(&node)[header_count..].clone()
		return t.lower_range_for_in(id, node, key_id, container_id, range_end_id, body_ids)
	}
	if container_is_range {
		range_node := t.a.nodes[int(container_id)]
		if range_node.children_count >= 2 {
			body_ids := t.a.children_of(&node)[header_count..].clone()
			return t.lower_range_for_in(id, node, key_id, t.a.child(&range_node, 0), t.a.child(&range_node,
				1), body_ids)
		}
	}
	iter_type := t.normalize_type_alias(t.detect_for_in_type(node))
	if t.cur_fn_is_generic
		&& (iter_type.len == 0 || for_iter_type_has_generic_placeholder(iter_type)) {
		return t.rebuild_for_in_stmt(id, node)
	}
	has_index := int(val_id) >= 0
	if iter_type.starts_with('map[') {
		return t.rebuild_for_in_stmt(id, node)
	}
	if pool_iter := t.pool_get_results_iter_type(container_id) {
		body_ids := t.a.children_of(&node)[header_count..].clone()
		return t.lower_indexed_for_in(id, node, key_id, val_id, container_id, pool_iter, has_index,
			body_ids)
	}
	if iter_info := t.iterator_for_in_info(iter_type) {
		body_ids := t.a.children_of(&node)[header_count..].clone()
		return t.lower_iterator_for_in(id, node, key_id, val_id, container_id, iter_type,
			iter_info, has_index, body_ids)
	}
	effective_iter := if iter_type.starts_with('...') {
		'[]' + iter_type[3..]
	} else if iter_type.starts_with('&[]') {
		iter_type[1..]
	} else {
		iter_type
	}
	if effective_iter.starts_with('[]') || effective_iter == 'string'
		|| t.is_fixed_array_type(effective_iter) {
		body_ids := t.a.children_of(&node)[header_count..].clone()
		return t.lower_indexed_for_in(id, node, key_id, val_id, container_id, effective_iter,
			has_index, body_ids)
	}
	return t.rebuild_for_in_stmt(id, node)
}

// pool_get_results_iter_type supports pool get results iter type handling for Transformer.
fn (t &Transformer) pool_get_results_iter_type(container_id flat.NodeId) ?string {
	if int(container_id) < 0 {
		return none
	}
	node := t.a.nodes[int(container_id)]
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee_id := t.a.child(&node, 0)
	if int(callee_id) < 0 {
		return none
	}
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .index || callee.children_count < 2 {
		return none
	}
	base := t.a.child_node(&callee, 0)
	if base.kind != .selector || base.value !in ['get_results', 'get_results_ref'] {
		return none
	}
	elem_type := t.generic_call_type_arg_name(t.a.child(&callee, 1))
	if elem_type.len == 0 {
		return none
	}
	if base.value == 'get_results_ref' {
		return '[]&${elem_type}'
	}
	return '[]${elem_type}'
}

// rebuild_for_in_stmt supports rebuild for in stmt handling for Transformer.
fn (mut t Transformer) rebuild_for_in_stmt(_id flat.NodeId, node flat.Node) []flat.NodeId {
	header_count := node.value.int()
	key_id := t.a.child(&node, 0)
	val_id := t.a.child(&node, 1)
	container_id := t.a.child(&node, 2)
	new_container := t.transform_expr(container_id)
	mut new_range_end := flat.NodeId(-1)
	if header_count == 4 {
		range_end_id := t.a.child(&node, 3)
		new_range_end = t.transform_expr(range_end_id)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	raw_iter_type := t.detect_for_in_type(node)
	iter_type := t.normalize_type_alias(raw_iter_type)
	if int(new_container) >= 0 && iter_type.len > 0 {
		t.set_node_typ(int(new_container), iter_type)
	}
	has_index := int(val_id) >= 0
	container_is_range := if int(container_id) >= 0 {
		t.a.nodes[int(container_id)].kind == .range
	} else {
		false
	}
	if header_count == 4 || container_is_range {
		// range `for i in 0 .. n`: single loop var (child0) follows the lower bound
		if int(key_id) >= 0 {
			key_name := t.a.nodes[int(key_id)].value
			if key_name.len > 0 {
				low_id := if container_is_range {
					range_node := t.a.nodes[int(container_id)]
					t.a.child(&range_node, 0)
				} else {
					container_id
				}
				t.set_var_type(key_name, t.range_loop_var_type_name(low_id))
			}
		}
	} else if has_index {
		// two loop vars: child0 = key/index, child1 = value/element
		key_name := if int(key_id) >= 0 { t.a.nodes[int(key_id)].value } else { '' }
		val_name := if int(val_id) >= 0 { t.a.nodes[int(val_id)].value } else { '' }
		if iter_type.starts_with('map[') {
			// map[K]V: child0 (key) -> key type, child1 (val) -> value type
			bracket_end := iter_type.index(']') or { 0 }
			if key_name.len > 0 && bracket_end > 4 {
				t.set_var_type(key_name, iter_type[4..bracket_end])
			}
		} else if iter_type.starts_with('[]') || iter_type == 'string' {
			// []E: child0 (index) -> 'int'
			if key_name.len > 0 {
				t.set_var_type(key_name, 'int')
			}
		}
		if val_name.len > 0 {
			elem_type := t.infer_for_in_elem_type(iter_type, node)
			if elem_type.len > 0 {
				t.set_var_type(val_name, elem_type)
			}
		}
	} else {
		// single var, child0 is the element
		if int(key_id) >= 0 {
			key_name := t.a.nodes[int(key_id)].value
			if key_name.len > 0 {
				elem_type := t.infer_for_in_elem_type(iter_type, node)
				if elem_type.len > 0 {
					t.set_var_type(key_name, elem_type)
				}
			}
		}
	}

	mut ids := []flat.NodeId{}
	ids << key_id
	ids << val_id
	ids << new_container
	if header_count == 4 {
		ids << new_range_end
	}
	body_ids := t.a.children_of(&node)[header_count..].clone()
	new_body := t.transform_stmts(body_ids)
	for bid in new_body {
		ids << bid
	}
	start := t.a.children.len
	for cid in ids {
		t.a.children << cid
	}
	prefix << t.a.add_node(flat.Node{
		kind:           .for_in_stmt
		op:             node.op
		children_start: start
		children_count: flat.child_count(ids.len)
		pos:            node.pos
		value:          node.value
		typ:            if iter_type.len > 0 { iter_type } else { node.typ }
	})
	return prefix
}

fn (t &Transformer) iterator_for_in_info(iter_type string) ?IteratorForInInfo {
	mut clean := iter_type.trim_space()
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean == 'RunesIterator' || clean == 'builtin.RunesIterator' {
		return IteratorForInInfo{
			elem_type:   'rune'
			next_method: 'RunesIterator.next'
		}
	}
	if isnil(t.tc) || clean.len == 0 {
		return none
	}
	iter_type_value := t.tc.parse_type(clean)
	info := t.tc.iterator_for_in_next_call_info(iter_type_value) or { return none }
	elem_type := t.iterator_for_in_elem_type_from_next_return(info.return_type) or { return none }
	return IteratorForInInfo{
		elem_type:   elem_type
		next_method: info.name
	}
}

fn (t &Transformer) iterator_for_in_elem_type_from_next_return(ret types.Type) ?string {
	if ret is types.OptionType {
		return ret.base_type.name()
	}
	return none
}

// lower_range_for_in builds lower range for in data for transform.
fn (mut t Transformer) lower_range_for_in(id flat.NodeId, node flat.Node, key_id flat.NodeId, low_id flat.NodeId, high_id flat.NodeId, body_ids []flat.NodeId) []flat.NodeId {
	if int(key_id) < 0 {
		return arr1(id)
	}
	key := t.a.nodes[int(key_id)]
	if key.kind != .ident || key.value.len == 0 {
		return arr1(id)
	}
	range_type := t.range_loop_var_type_name(low_id)
	low := t.stable_expr_for_reuse(low_id)
	high := t.stable_expr_for_reuse(high_id)
	loop_name := if key.value == '_' { '__discard_${int(key_id)}' } else { key.value }
	t.set_var_type(loop_name, range_type)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	init := t.make_decl_assign_typed(loop_name, low, range_type)
	cond := t.make_infix(.lt, t.make_ident(loop_name), high)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(loop_name), .inc))
	new_body := t.transform_stmts(body_ids)
	prefix << t.make_for_stmt(init, cond, post, new_body, node)
	return prefix
}

fn (t &Transformer) range_loop_var_type_name(low_id flat.NodeId) string {
	typ := t.node_type(low_id)
	if typ.len == 0 {
		return 'int'
	}
	clean := t.normalize_type_alias(typ)
	if t.is_integer_type_name(clean) {
		return typ
	}
	return 'int'
}

fn (mut t Transformer) lower_iterator_for_in(id flat.NodeId, node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, iter_type string, info IteratorForInInfo, has_index bool, body_ids []flat.NodeId) []flat.NodeId {
	if int(key_id) < 0 {
		return arr1(id)
	}
	key := t.a.nodes[int(key_id)]
	if key.kind != .ident || key.value.len == 0 {
		return arr1(id)
	}
	mut elem_name := key.value
	if has_index {
		if int(val_id) < 0 {
			return arr1(id)
		}
		val := t.a.nodes[int(val_id)]
		if val.kind != .ident || val.value.len == 0 {
			return arr1(id)
		}
		elem_name = val.value
	}
	iter_name := t.new_temp('iter')
	next_name := t.new_temp('iter_next')
	iter_expr := t.transform_expr(container_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	prefix << t.make_decl_assign_typed(iter_name, iter_expr, iter_type)
	init := if has_index {
		t.set_var_type(key.value, 'int')
		t.make_decl_assign_typed(key.value, t.make_int_literal(0), 'int')
	} else {
		t.make_empty()
	}
	elem_type := info.elem_type
	t.set_var_type(elem_name, elem_type)
	next_call := t.make_call_typed(info.next_method, arr1(t.make_prefix(.amp,
		t.make_ident(iter_name))), '?${elem_type}')
	next_decl := t.make_decl_assign_typed(next_name, next_call, '?${elem_type}')
	no_value := t.make_prefix(.not, t.make_selector(t.make_ident(next_name), 'ok', 'bool'))
	break_if_done := t.make_if(no_value, t.make_block(arr1(t.a.add(.break_stmt))), t.make_empty())
	elem_decl := t.make_decl_assign_typed(elem_name, t.make_selector(t.make_ident(next_name),
		'value', elem_type), elem_type)
	mut loop_body := []flat.NodeId{}
	loop_body << next_decl
	loop_body << break_if_done
	loop_body << elem_decl
	loop_body << t.transform_stmts(body_ids)
	post := if has_index {
		t.make_expr_stmt(t.make_postfix(t.make_ident(key.value), .inc))
	} else {
		t.make_empty()
	}
	prefix << t.make_for_stmt(init, t.make_bool_literal(true), post, loop_body, node)
	return prefix
}

// lower_indexed_for_in builds lower indexed for in data for transform.
fn (mut t Transformer) lower_indexed_for_in(id flat.NodeId, node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, iter_type string, has_index bool, body_ids []flat.NodeId) []flat.NodeId {
	if int(key_id) < 0 {
		return arr1(id)
	}
	key := t.a.nodes[int(key_id)]
	if key.kind != .ident || key.value.len == 0 {
		return arr1(id)
	}
	mut container := t.stable_expr_for_reuse(container_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	mut actual_iter_type := iter_type
	container_type := t.node_type(container)
	if container_type.len > 0 && container_type !in ['array', 'map', 'unknown']
		&& for_iter_type_is_container(container_type)
		&& (actual_iter_type.len == 0 || for_iter_type_has_generic_placeholder(actual_iter_type))
		&& !(t.is_fixed_array_type(actual_iter_type) && !t.is_fixed_array_type(container_type)) {
		actual_iter_type = container_type
	}
	if actual_iter_type.starts_with('&[]') {
		container = t.make_prefix(.mul, container)
		t.set_node_typ(int(container), actual_iter_type[1..])
		actual_iter_type = actual_iter_type[1..]
	}
	elem_type := t.infer_for_in_elem_type(actual_iter_type, node)
	if elem_type.len == 0 {
		return arr1(id)
	}
	mut idx_name := key.value
	if !has_index {
		idx_name = t.new_temp('for_idx')
	}
	mut elem_name := key.value
	if has_index {
		if int(val_id) < 0 {
			return arr1(id)
		}
		val := t.a.nodes[int(val_id)]
		if val.kind != .ident || val.value.len == 0 {
			return arr1(id)
		}
		elem_name = val.value
	}
	t.set_var_type(idx_name, 'int')
	elem_is_mut := node.op == .amp && actual_iter_type != 'string'
	elem_needs_ref := elem_is_mut && !elem_type.starts_with('&')
	elem_var_type := if elem_needs_ref { '&${elem_type}' } else { elem_type }
	t.set_var_type(elem_name, elem_var_type)
	len_expr := if t.is_fixed_array_type(actual_iter_type) {
		t.make_for_in_fixed_array_len_expr(actual_iter_type)
	} else {
		t.make_selector(container, 'len', 'int')
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), len_expr)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := if elem_needs_ref && actual_iter_type.starts_with('[]') {
		t.array_get_ptr(container, t.make_ident(idx_name), elem_type)
	} else if elem_needs_ref {
		t.make_prefix(.amp, t.make_index(container, t.make_ident(idx_name), elem_type))
	} else if actual_iter_type.starts_with('[]') {
		t.array_get_value(container, t.make_ident(idx_name), elem_type)
	} else {
		t.make_index(container, t.make_ident(idx_name), elem_type)
	}
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_var_type)
	mut transformed_body := []flat.NodeId{}
	if elem_needs_ref {
		had_pointer_value_lvalue := t.pointer_value_lvalues[elem_name] or { false }
		t.pointer_value_lvalues[elem_name] = true
		transformed_body = t.transform_stmts(body_ids)
		if had_pointer_value_lvalue {
			t.pointer_value_lvalues[elem_name] = true
		} else {
			t.pointer_value_lvalues.delete(elem_name)
		}
	} else {
		transformed_body = t.transform_stmts(body_ids)
	}
	mut new_body := []flat.NodeId{}
	new_body << elem_decl
	new_body << transformed_body
	prefix << t.make_for_stmt(init, cond, post, new_body, node)
	return prefix
}

// make_for_stmt builds make for stmt data for transform.
fn (mut t Transformer) make_for_stmt(init flat.NodeId, cond flat.NodeId, post flat.NodeId, body []flat.NodeId, src flat.Node) flat.NodeId {
	start := t.a.children.len
	t.a.children << init
	t.a.children << cond
	t.a.children << post
	for id in body {
		t.a.children << id
	}
	return t.a.add_node(flat.Node{
		kind:           .for_stmt
		op:             src.op
		children_start: start
		children_count: flat.child_count(3 + body.len)
		pos:            src.pos
		typ:            src.typ
	})
}

// detect_for_in_type resolves detect for in type information for transform.
fn (mut t Transformer) detect_for_in_type(node flat.Node) string {
	header_count := node.value.int()
	container_idx := if header_count >= 3 { header_count - 1 } else { 2 }
	if node.children_count > container_idx {
		iter_id := t.a.child(&node, container_idx)
		if fixed_array_type := t.detect_for_in_global_fixed_array_type(iter_id) {
			return fixed_array_type
		}
		checker_type := t.raw_checker_node_type(iter_id)
		if checker_type.len > 0 && for_iter_type_is_container(checker_type) {
			t.set_node_typ(int(iter_id), checker_type)
			return checker_type
		}
		iter_type := t.node_type(iter_id)
		if iter_type.len > 0
			&& (node.typ.len == 0 || for_iter_type_has_generic_placeholder(node.typ)
			|| for_iter_type_is_container(iter_type)) {
			return iter_type
		}
	}
	if node.typ.len > 0 {
		return node.typ
	}
	return ''
}

fn for_iter_type_is_container(iter_type string) bool {
	clean := iter_type.trim_space()
	return clean == 'string' || clean.starts_with('[]') || clean.starts_with('&[]')
		|| clean.starts_with('...') || clean.starts_with('map[') || clean.starts_with('&map[')
		|| clean.starts_with('[')
}

fn (t &Transformer) detect_for_in_global_fixed_array_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	if t.var_type(node.value).len > 0 {
		return none
	}
	mut candidates := []string{}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${node.value}'
	}
	candidates << node.value
	for candidate in candidates {
		if typ := t.globals[candidate] {
			normalized := t.normalize_type_alias(typ)
			if t.is_fixed_array_type(normalized) {
				return normalized
			}
		}
	}
	if !isnil(t.tc) {
		checker_type := t.normalize_type_alias(t.tc.resolve_type(id).name())
		if t.is_fixed_array_type(checker_type) {
			return checker_type
		}
	}
	return none
}

fn for_iter_type_has_generic_placeholder(iter_type string) bool {
	clean := iter_type.trim_space()
	if clean.len == 0 {
		return false
	}
	if is_generic_placeholder_type_name(clean) {
		return true
	}
	if clean.starts_with('&') {
		return for_iter_type_has_generic_placeholder(clean[1..])
	}
	if clean.starts_with('[]') {
		return for_iter_type_has_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := clean.index(']') or { return false }
		return for_iter_type_has_generic_placeholder(clean[4..bracket_end])
			|| for_iter_type_has_generic_placeholder(clean[bracket_end + 1..])
	}
	if clean.starts_with('[') {
		bracket_end := clean.index(']') or { return false }
		return for_iter_type_has_generic_placeholder(clean[bracket_end + 1..])
	}
	return false
}

// Infer the element type for the loop variable from the iterable type.
fn (t &Transformer) infer_for_in_elem_type(iter_type string, node flat.Node) string {
	if iter_type.starts_with('&[]') {
		return iter_type[3..]
	}
	if iter_type.starts_with('[]') {
		return iter_type[2..]
	}
	if iter_type.starts_with('map[') {
		// map[K]V -> value type is everything after the closing ']'
		bracket_end := iter_type.index(']') or { return '' }
		if bracket_end + 1 < iter_type.len {
			return iter_type[bracket_end + 1..]
		}
		return ''
	}
	if iter_type == 'string' {
		return 'u8'
	}
	if t.is_fixed_array_type(iter_type) {
		return for_in_fixed_array_elem_type(iter_type)
	}
	// Check if the iterable is a range expression
	if node.children_count > 0 {
		iter_id := t.a.child(&node, 0)
		iter_node := t.a.nodes[int(iter_id)]
		if iter_node.kind == .range {
			return 'int'
		}
	}
	return ''
}

fn (mut t Transformer) make_for_in_fixed_array_len_expr(s string) flat.NodeId {
	len_text := for_in_fixed_array_len_text(s)
	if is_decimal_text(len_text) {
		return t.make_int_literal(len_text.int())
	}
	if !isnil(t.tc) {
		if v := t.tc.const_int_value(len_text, []string{}) {
			return t.make_int_literal(v)
		}
	}
	if len_text.contains('.') {
		base := len_text.all_before_last('.')
		field := len_text.all_after_last('.')
		return t.make_selector(t.make_ident(base), field, 'int')
	}
	return t.make_ident(len_text)
}

fn for_in_fixed_array_elem_type(s string) string {
	clean := s.trim_space()
	if clean.starts_with('[') {
		return clean.all_after(']')
	}
	open := clean.last_index_u8(`[`)
	if open < 0 {
		return ''
	}
	return clean[..open]
}

fn for_in_fixed_array_len_text(s string) string {
	clean := s.trim_space()
	if clean.starts_with('[') {
		return clean.all_after('[').all_before(']').trim_space()
	}
	open := clean.last_index_u8(`[`)
	if open < 0 {
		return ''
	}
	close_rel := clean[open + 1..].index_u8(`]`)
	if close_rel < 0 {
		return ''
	}
	return clean[open + 1..open + 1 + close_rel].trim_space()
}
