module transform

import v3.flat
import v3.types

struct IteratorForInInfo {
	elem_type   string
	next_method string
}

struct UnsignedInclusiveForPost {
	cond        flat.NodeId
	cond_prefix []flat.NodeId
	post_body   []flat.NodeId
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
		if expanded.len == 1 {
			new_init = expanded[0]
		} else if expanded.len > 1 {
			new_init = t.make_block(expanded)
			t.set_node_value(int(new_init), 'for_init_expanded')
		}
	}
	// child 1: condition expression
	cond_id := t.a.child(&node, 1)
	// child 2: post statement
	post_id := t.a.child(&node, 2)
	// children 3..n: body statements
	mut body_ids := []flat.NodeId{}
	for i in 3 .. node.children_count {
		body_ids << t.a.child(&node, i)
	}
	cond_smartcasts := t.extract_all_is_exprs(cond_id)
	mut cond_prefix := []flat.NodeId{}
	mut new_cond := flat.empty_node
	mut guarded_post_body := []flat.NodeId{}
	if guarded_post := t.unsigned_inclusive_for_post_body(cond_id, post_id) {
		new_cond = guarded_post.cond
		cond_prefix = guarded_post.cond_prefix.clone()
		guarded_post_body = guarded_post.post_body.clone()
	} else {
		new_cond = t.transform_and_chain_smartcasts(cond_id)
		t.drain_pending(mut cond_prefix)
	}
	mut new_post := post_id
	mut post_body := []flat.NodeId{}
	if int(post_id) >= 0 {
		post_node := t.a.nodes[int(post_id)]
		if guarded_post_body.len > 0 {
			new_post = t.a.add(.empty)
			post_body = guarded_post_body.clone()
		} else if post_node.kind == .assign && post_node.children_count > 2 {
			new_post = t.a.add(.empty)
			if expanded := t.try_expand_multi_return_assign(post_node) {
				post_body = expanded.clone()
			} else {
				post_body = t.lower_for_post_multi_assign(post_node)
			}
		} else {
			expanded := t.transform_stmt(post_id)
			if expanded.len == 1 && t.a.nodes[int(expanded[0])].kind == .block {
				new_post = t.a.add(.empty)
				post_block := t.a.nodes[int(expanded[0])]
				post_body = t.a.children_of(&post_block).clone()
			} else if expanded.len == 1 {
				new_post = expanded[0]
			} else if expanded.len > 1 {
				new_post = t.a.add(.empty)
				post_body = expanded.clone()
			}
		}
	}
	for info in cond_smartcasts {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	mut new_body := t.transform_stmts(body_ids)
	mut synthetic_continue_label := ''
	if post_body.len > 0 {
		mut continue_label := t.existing_for_continue_label(body_ids)
		if continue_label.len == 0 {
			continue_label = t.new_temp('for_post')
			synthetic_continue_label = continue_label
		}
		for i, stmt in new_body {
			new_body[i] = t.rewrite_continue_to_for_post_label(stmt, continue_label)
		}
		if synthetic_continue_label.len > 0 {
			new_body << t.a.add_val(.label_stmt, '${synthetic_continue_label}_continue')
		}
		for stmt in post_body {
			new_body << stmt
		}
	}
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
	if synthetic_continue_label.len > 0 {
		return [
			t.a.add_val(.label_stmt, pending_loop_label_marker + synthetic_continue_label),
			new_id,
		]
	}
	return arr1(new_id)
}

fn (mut t Transformer) unsigned_inclusive_for_post_body(cond_id flat.NodeId, post_id flat.NodeId) ?UnsignedInclusiveForPost {
	if int(cond_id) < 0 || int(post_id) < 0 {
		return none
	}
	cond := t.a.nodes[int(cond_id)]
	post := t.a.nodes[int(post_id)]
	if cond.kind != .infix || post.kind != .expr_stmt || post.children_count == 0 {
		return none
	}
	post_expr_id := t.a.child(&post, 0)
	post_expr := t.a.nodes[int(post_expr_id)]
	if post_expr.kind != .postfix || post_expr.children_count == 0 {
		return none
	}
	loop_id := t.a.child(&post_expr, 0)
	loop_node := t.a.nodes[int(loop_id)]
	if loop_node.kind != .ident {
		return none
	}
	loop_type := t.for_loop_var_unsigned_type(loop_node.value)
	if loop_type.len == 0 {
		return none
	}
	if (post_expr.op == .inc && cond.op != .le) || (post_expr.op == .dec && cond.op != .ge) {
		return none
	}
	lhs_id := t.a.child(&cond, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value != loop_node.value {
		return none
	}
	new_cond := t.transform_and_chain_smartcasts(cond_id)
	mut cond_prefix := []flat.NodeId{}
	t.drain_pending(mut cond_prefix)
	mut post_body := []flat.NodeId{}
	done := t.unsigned_loop_post_would_overflow(loop_node.value, loop_type, post_expr.op)
	t.set_node_typ(int(done), 'bool')
	break_stmt := t.a.add(.break_stmt)
	post_body << t.make_if(done, t.make_block(arr1(break_stmt)), t.make_empty())
	post_body << t.transform_stmt(post_id)
	return UnsignedInclusiveForPost{
		cond:        new_cond
		cond_prefix: cond_prefix
		post_body:   post_body
	}
}

fn (t &Transformer) for_loop_var_unsigned_type(name string) string {
	mut typ := t.var_type(name)
	if typ.len == 0 && !isnil(t.tc) {
		if current := t.tc.cur_scope.lookup(name) {
			typ = current.name()
		}
	}
	for typ.starts_with('&') {
		typ = typ[1..]
	}
	if typ in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
		return typ
	}
	return ''
}

fn (mut t Transformer) unsigned_loop_post_would_overflow(name string, typ string, op flat.Op) flat.NodeId {
	limit := if op == .inc {
		t.make_cast(typ, t.make_int_literal(-1), typ)
	} else {
		t.make_int_literal_typed('0', typ)
	}
	done := t.make_infix(.eq, t.make_ident(name), limit)
	t.set_node_typ(int(done), 'bool')
	return done
}

fn (mut t Transformer) lower_for_post_multi_assign(node flat.Node) []flat.NodeId {
	mut result := []flat.NodeId{}
	mut tmp_names := []string{cap: int(node.children_count) / 2}
	for i := 1; i < node.children_count; i += 2 {
		rhs_id := t.a.child(&node, i)
		rhs := t.transform_expr(rhs_id)
		t.drain_pending(mut result)
		mut typ := t.node_type(rhs_id)
		if typ.len == 0 {
			typ = t.node_type(rhs)
		}
		if typ.len == 0 {
			typ = 'int'
		}
		tmp_name := t.new_temp('for_post')
		result << t.make_decl_assign_typed(tmp_name, rhs, typ)
		tmp_names << tmp_name
	}
	mut tmp_idx := 0
	for i := 0; i < node.children_count && tmp_idx < tmp_names.len; i += 2 {
		lhs_id := t.a.child(&node, i)
		result << t.make_assign_op(t.transform_lvalue(lhs_id), t.make_ident(tmp_names[tmp_idx]),
			node.op)
		tmp_idx++
	}
	return result
}

fn (t &Transformer) existing_for_continue_label(body_ids []flat.NodeId) string {
	if body_ids.len == 0 {
		return ''
	}
	last_id := body_ids.last()
	if int(last_id) < 0 || int(last_id) >= t.a.nodes.len {
		return ''
	}
	last := t.a.nodes[int(last_id)]
	suffix := '_continue'
	if last.kind == .label_stmt && last.value.ends_with(suffix) && last.value.len > suffix.len {
		return last.value[..last.value.len - suffix.len]
	}
	return ''
}

fn (mut t Transformer) rewrite_continue_to_for_post_label(id flat.NodeId, continue_label string) flat.NodeId {
	if continue_label.len == 0 || int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.continue_stmt {
			if node.value.len > 0 {
				return id
			}
			return t.a.add_val(.continue_stmt, continue_label)
		}
		.for_stmt, .for_in_stmt {
			return id
		}
		.fn_literal, .lambda_expr {
			return id
		}
		.block, .if_expr, .match_stmt, .match_branch, .select_stmt, .select_branch, .comptime_if,
		.or_expr, .expr_stmt {
			return t.rewrite_continue_to_for_post_label_in_children(id, node, continue_label)
		}
		else {
			if node.children_count > 0 {
				return t.rewrite_continue_to_for_post_label_in_children(id, node, continue_label)
			}
			return id
		}
	}
}

fn (mut t Transformer) rewrite_continue_to_for_post_label_in_children(id flat.NodeId, node flat.Node, continue_label string) flat.NodeId {
	mut children := []flat.NodeId{cap: int(node.children_count)}
	mut changed := false
	for i in 0 .. node.children_count {
		child := t.a.child(&node, i)
		new_child := t.rewrite_continue_to_for_post_label(child, continue_label)
		if new_child != child {
			changed = true
		}
		children << new_child
	}
	if !changed {
		return id
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		value:          node.value
		typ:            node.typ
		pos:            node.pos
		children_start: start
		children_count: node.children_count
	})
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
	} else if iter_type.starts_with('&[') && t.is_fixed_array_type(iter_type[1..]) {
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
		if iter_type.starts_with('map[') || iter_type.starts_with('&map[') {
			// map[K]V: child0 (key) -> key type, child1 (val) -> value type
			map_type := if iter_type.starts_with('&map[') { iter_type[1..] } else { iter_type }
			bracket_end := map_type.index(']') or { 0 }
			if key_name.len > 0 && bracket_end > 4 {
				t.set_var_type(key_name, map_type[4..bracket_end])
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
				val_type := if node.op == .amp || iter_type.starts_with('&map[') {
					'&${elem_type}'
				} else {
					elem_type
				}
				t.set_var_type(val_name, val_type)
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
	mut new_body := []flat.NodeId{}
	mut pointer_value_name := ''
	if node.op == .amp {
		bind_id := if has_index { val_id } else { key_id }
		if int(bind_id) >= 0 {
			bind := t.a.nodes[int(bind_id)]
			if bind.kind == .ident && bind.value.len > 0 {
				bind_type := t.var_type(bind.value)
				if bind_type.starts_with('&') && !t.is_fixed_array_type(bind_type[1..]) {
					pointer_value_name = bind.value
				}
			}
		}
	}
	if pointer_value_name.len > 0 {
		had_pointer_value_lvalue := t.pointer_value_lvalues[pointer_value_name] or { false }
		had_pointer_value_rvalue := t.pointer_value_rvalues[pointer_value_name] or { false }
		t.pointer_value_lvalues[pointer_value_name] = true
		t.pointer_value_rvalues[pointer_value_name] = true
		new_body = t.transform_stmts(body_ids)
		if had_pointer_value_lvalue {
			t.pointer_value_lvalues[pointer_value_name] = true
		} else {
			t.pointer_value_lvalues.delete(pointer_value_name)
		}
		if had_pointer_value_rvalue {
			t.pointer_value_rvalues[pointer_value_name] = true
		} else {
			t.pointer_value_rvalues.delete(pointer_value_name)
		}
	} else {
		new_body = t.transform_stmts(body_ids)
	}
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
	container_node := if int(container_id) >= 0 { t.a.nodes[int(container_id)] } else { flat.Node{} }
	direct_map_index_container := node.op == .amp && container_node.kind == .index
	mut container := if direct_map_index_container {
		container_id
	} else {
		t.stable_expr_for_reuse(container_id)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	mut actual_iter_type := iter_type
	raw_container_type := t.raw_checker_node_type(container_id)
	mut optional_container := flat.empty_node
	if payload_type := for_iter_optional_payload_type(raw_container_type) {
		if payload_type == actual_iter_type {
			optional_container = container
			container = t.make_selector(container, 'value', actual_iter_type)
		}
	}
	container_type := t.node_type(container)
	if container_type.len > 0 && container_type !in ['array', 'map', 'unknown']
		&& for_iter_type_is_container(container_type)
		&& (actual_iter_type.len == 0 || for_iter_type_has_generic_placeholder(actual_iter_type))
		&& !(t.is_fixed_array_type(actual_iter_type) && !t.is_fixed_array_type(container_type)) {
		actual_iter_type = container_type
	}
	iterates_by_ref := actual_iter_type.starts_with('&[]')
		|| (container_type.starts_with('&[]') && actual_iter_type != 'string')
		|| (container_type.starts_with('&[') && t.is_fixed_array_type(container_type[1..]))
	if actual_iter_type.starts_with('&[]') {
		container = t.make_prefix(.mul, container)
		t.set_node_typ(int(container), actual_iter_type[1..])
		actual_iter_type = actual_iter_type[1..]
	}
	if container_type.starts_with('&[') && t.is_fixed_array_type(container_type[1..]) {
		container = t.make_prefix(.mul, container)
		t.set_node_typ(int(container), container_type[1..])
		actual_iter_type = container_type[1..]
	}
	elem_type := t.infer_for_in_elem_type(actual_iter_type, node)
	if elem_type.len == 0 {
		return arr1(id)
	}
	mut idx_name := key.value
	if !has_index || idx_name == '_' {
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
	elem_is_mut := (node.op == .amp || iterates_by_ref) && actual_iter_type != 'string'
	elem_needs_ref := elem_is_mut && !elem_type.starts_with('&')
	elem_var_type := if elem_needs_ref { '&${elem_type}' } else { elem_type }
	t.set_var_type(elem_name, elem_var_type)
	if elem_needs_ref && t.is_fixed_array_type(actual_iter_type) {
		if direct_container := t.fixed_array_map_index_for_in_container(container_id, mut prefix) {
			container = direct_container
		}
	}
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
		had_pointer_value_rvalue := t.pointer_value_rvalues[elem_name] or { false }
		t.pointer_value_lvalues[elem_name] = true
		t.pointer_value_rvalues[elem_name] = true
		transformed_body = t.transform_stmts(body_ids)
		if had_pointer_value_lvalue {
			t.pointer_value_lvalues[elem_name] = true
		} else {
			t.pointer_value_lvalues.delete(elem_name)
		}
		if had_pointer_value_rvalue {
			t.pointer_value_rvalues[elem_name] = true
		} else {
			t.pointer_value_rvalues.delete(elem_name)
		}
	} else {
		transformed_body = t.transform_stmts(body_ids)
	}
	mut new_body := []flat.NodeId{}
	new_body << elem_decl
	new_body << transformed_body
	for_stmt := t.make_for_stmt(init, cond, post, new_body, node)
	if int(optional_container) >= 0 {
		ok_cond := t.make_selector(optional_container, 'ok', 'bool')
		prefix << t.make_if(ok_cond, t.make_block(arr1(for_stmt)), t.make_empty())
	} else {
		prefix << for_stmt
	}
	return prefix
}

fn (mut t Transformer) fixed_array_map_index_for_in_container(container_id flat.NodeId, mut prefix []flat.NodeId) ?flat.NodeId {
	info := t.map_index_info(container_id) or { return none }
	if !t.is_fixed_array_type(info.value_type) {
		return none
	}
	map_expr := t.stable_expr_for_reuse(info.base_id)
	t.drain_pending(mut prefix)
	key_name := t.new_temp('map_key')
	key_expr := t.transform_expr_for_type(info.key_id, info.key_type)
	t.drain_pending(mut prefix)
	prefix << t.make_decl_assign_typed(key_name, key_expr, info.key_storage_type)
	zero_name := t.new_temp('map_zero')
	prefix << t.make_decl_assign_typed(zero_name, t.zero_value_for_type(info.value_type),
		info.value_type)
	return t.make_map_get_expr(map_expr, info.base_type, key_name, zero_name, info.value_type)
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
		if checker_type.len > 0 {
			checker_payload := for_iter_payload_type(checker_type)
			if for_iter_type_is_container(checker_payload) {
				if checker_payload == checker_type {
					t.set_node_typ(int(iter_id), checker_type)
				}
				return checker_payload
			}
		}
		iter_type := t.node_type(iter_id)
		if iter_type.len > 0 {
			iter_payload := for_iter_payload_type(iter_type)
			if node.typ.len == 0 || for_iter_type_has_generic_placeholder(node.typ)
				|| for_iter_type_is_container(iter_payload) {
				return iter_payload
			}
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

fn for_iter_payload_type(iter_type string) string {
	if payload := for_iter_optional_payload_type(iter_type) {
		return payload
	}
	return iter_type
}

fn for_iter_optional_payload_type(iter_type string) ?string {
	clean := iter_type.trim_space()
	if clean.len > 1 && (clean[0] == `?` || clean[0] == `!`) {
		return clean[1..].trim_space()
	}
	return none
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
	if iter_type.starts_with('&map[') {
		return t.infer_for_in_elem_type(iter_type[1..], node)
	}
	if iter_type.starts_with('map[') {
		// map[K]V -> value type is everything after the closing ']'
		bracket_end := iter_type.index(']') or { return '' }
		if bracket_end + 1 < iter_type.len {
			value_type := iter_type[bracket_end + 1..]
			fixed_type := fixed_array_map_value_type_text(value_type)
			if fixed_type.len > 0 {
				return fixed_type
			}
			return value_type
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
