module transform

import v.flat
import v.types

struct IteratorForInInfo {
	elem_type   u32
	next_method string
}

struct UnsignedInclusiveForPost {
	cond        flat.NodeId
	cond_prefix []flat.NodeId
	post_body   []flat.NodeId
}

// transform_for_body transforms transform for body data for transform.
fn (mut t Transformer) transform_for_body(id flat.NodeId, node flat.Node) []flat.NodeId {
	heaped_state := t.save_heaped_local_state()
	defer { t.restore_heaped_local_state(heaped_state) }
	if node.children_count < 3 {
		return [id]
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
			} else if expanded.len == 1 && t.a.nodes[int(expanded[0])].kind != .lock_expr {
				new_post = expanded[0]
			} else if expanded.len > 0 {
				// a `lock` block (e.g. from `m[k]++` on a `shared` map) is not an expression
				new_post = t.a.add(.empty)
				post_body = expanded.clone()
			}
		}
	}
	for info in cond_smartcasts {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	mut new_body := t.transform_scope_stmts(body_ids)
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
		break_block := t.make_block([t.a.add(.break_stmt)])
		guarded_body << t.make_if(not_cond, break_block, t.make_block([]flat.NodeId{}))
		for stmt in new_body {
			guarded_body << stmt
		}
		loop_cond = t.make_bool_literal(true)
		loop_body = guarded_body.clone()
	}
	// Rebuild the for_stmt with transformed children.
	mut new_children := [new_init, loop_cond, new_post]
	for bid in loop_body {
		new_children << bid
	}
	new_id := if t.rewrite_children_in_place(id, new_children) {
		id
	} else {
		start := t.a.children.len
		t.a.children << new_children
		t.a.add_node(flat.Node{
			kind:           .for_stmt
			op:             node.op
			children_start: start
			children_count: flat.child_count(new_children.len)
			pos:            node.pos
			value:          node.value
			source_value:   node.source_value
			typ:            node.typ
			flags:          flat.node_flags(node.skip_ownership_drops(), false)
		})
	}
	if synthetic_continue_label.len > 0 {
		return [
			t.a.add_val(.label_stmt, pending_loop_label_marker + synthetic_continue_label),
			new_id,
		]
	}
	return [new_id]
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
	loop_type_source_id := t.for_loop_var_unsigned_type(loop_node.value)

	if loop_type_source_id == 0 {
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
	done := t.unsigned_loop_post_would_overflow(loop_node.value, loop_type_source_id, post_expr.op)
	t.set_node_typ(int(done), flat.source_type_bool)
	break_stmt := t.a.add(.break_stmt)
	post_body << t.make_if(done, t.make_block([break_stmt]), t.make_empty())
	post_body << t.transform_stmt(post_id)
	return UnsignedInclusiveForPost{
		cond:        new_cond
		cond_prefix: cond_prefix
		post_body:   post_body
	}
}

fn (t &Transformer) for_loop_var_unsigned_type(name string) u32 {
	mut typ := t.var_type(name)
	if typ == 0 && !isnil(t.tc) {
		if current := t.tc.cur_scope.lookup(name) {
			typ = current.source_type_id()
		}
	}
	typ = t.trim_all_pointer_type(typ)
	if typ in [flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
		flat.source_type_uint] {
		return typ
	}
	return u32(0)
}

fn (mut t Transformer) unsigned_loop_post_would_overflow(name string, typ_id u32, op flat.Op) flat.NodeId {
	limit := if op == .inc {
		t.make_cast(typ_id, t.make_int_literal(-1), typ_id)
	} else {
		t.make_int_literal_typed('0', typ_id)
	}
	done := t.make_infix(.eq, t.make_ident(name), limit)
	t.set_node_typ(int(done), flat.source_type_bool)
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
		if typ == 0 {
			typ = t.node_type(rhs)
		}
		if typ == 0 {
			typ = flat.source_type_int
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
	if continue_label == '' || int(id) < 0 || int(id) >= t.a.nodes.len {
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
	if t.rewrite_children_in_place(id, children) {
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
		source_value:   node.source_value
		payload:        if node.payload != 0 { node.clone_owned().payload } else { u32(0) }
		typ:            node.typ
		pos:            node.pos
		children_start: start
		children_count: node.children_count
	})
}

// transform_for_in_body transforms transform for in body data for transform.
fn (mut t Transformer) transform_for_in_body(id flat.NodeId, node flat.Node) []flat.NodeId {
	heaped_state := t.save_heaped_local_state()
	defer { t.restore_heaped_local_state(heaped_state) }
	header_count := node.value.int()
	if header_count < 3 || node.children_count < 3 {
		return [id]
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
	detected_iter_type_source_id := t.detect_for_in_type(node)
	detected_iter_type_source_id_2 := detected_iter_type_source_id
	mut iter_type := t.comptime_normalize_type_alias_chain(detected_iter_type_source_id)
	if !isnil(t.tc) {
		clean_detected := flat.source_type_syntax(detected_iter_type_source_id_2).pointer_raw_base()
		detected_syntax := flat.source_type_syntax(clean_detected)
		generic_base := detected_syntax.generic_app_base()
		is_generic_interface := detected_syntax.generic_app_valid() && (flat.source_type_name(generic_base) in t.tc.interface_names
			|| flat.source_type_name(t.tc.qualify_type_name(generic_base)) in t.tc.interface_names)
		has_alias_next := flat.source_type_name(flat.source_type_composition_two(.qualified, clean_detected, flat.source_type_id('next'))) in t.tc.fn_ret_types
			|| flat.source_type_name(flat.source_type_composition_two(.qualified, t.tc.qualify_type_name(clean_detected), flat.source_type_id('next'))) in t.tc.fn_ret_types
		if is_generic_interface || has_alias_next {
			iter_type = detected_iter_type_source_id_2
		}
	}
	if t.cur_fn_is_generic
		&& (iter_type == 0 || for_iter_type_has_generic_placeholder(iter_type)) {
		return t.rebuild_for_in_stmt(id, node)
	}
	has_index := int(val_id) >= 0
	if flat.source_type_shape(t.clean_map_type(iter_type)).has(.map_type) {
		return t.rebuild_for_in_stmt(id, node)
	}
	if pool_iter_source_id := t.pool_get_results_iter_type(container_id) {
		pool_iter_source_id_2 := pool_iter_source_id

		body_ids := t.a.children_of(&node)[header_count..].clone()
		return t.lower_indexed_for_in(id, node, key_id, val_id, container_id, pool_iter_source_id_2, has_index,
			body_ids)
	}
	if iter_info := t.iterator_for_in_info(iter_type) {
		body_ids := t.a.children_of(&node)[header_count..].clone()
		return t.lower_iterator_for_in(id, node, key_id, val_id, container_id, iter_type,
			iter_info, has_index, body_ids)
	}
	iter_shape := flat.source_type_shape(iter_type)
	mut effective_iter := if iter_shape.has(.variadic) {
		flat.source_type_composition_one(.array, flat.source_type_syntax(iter_type).raw_base())
	} else if iter_shape.has(.pointer_array) {
		t.trim_pointer_type(iter_type)
	} else if iter_shape.has(.pointer) && !iter_shape.has(.pointer_chain)
		&& flat.source_type_shape(t.trim_pointer_type(iter_type)).has(.leading_bracket) && t.is_fixed_array_type(t.trim_pointer_type(iter_type)) {
		t.trim_pointer_type(iter_type)
	} else if iter_shape.has(.pointer) && t.trim_pointer_type(iter_type) == flat.source_type_string {
		flat.source_type_string
	} else {
		iter_type
	}
	if flat.source_type_shape(effective_iter).has(.array_type) || effective_iter == flat.source_type_string
		|| t.is_fixed_array_type(effective_iter) {
		body_ids := t.a.children_of(&node)[header_count..].clone()
		mut collection := container_id
		literal := t.a.nodes[int(container_id)]
		if node.op != .amp && flat.source_type_shape(effective_iter).has(.array_type)
			&& t.array_literal_can_emit_direct(literal) {
			effective_iter = flat.source_type_fixed_array(literal.children_count, '', flat.source_type_syntax(effective_iter).raw_base(), false)
			values := t.a.children_of(&literal).clone()
			collection = t.make_array_literal_typed(values, effective_iter)
		}
		return t.lower_indexed_for_in(id, node, key_id, val_id, collection, effective_iter,
			has_index, body_ids)
	}
	return t.rebuild_for_in_stmt(id, node)
}

// pool_get_results_iter_type supports pool get results iter type handling for Transformer.
fn (t &Transformer) pool_get_results_iter_type(container_id flat.NodeId) ?u32 {
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
	elem_type_source_id_4 := t.generic_call_type_arg_name(t.a.child(&callee, 1))
	if elem_type_source_id_4 == 0 {
		return none
	}
	if base.value == 'get_results_ref' {
		return flat.source_type_composition_one(.array, flat.source_type_composition_one(.pointer, elem_type_source_id_4))
	}
	return flat.source_type_composition_one(.array, elem_type_source_id_4)
}

fn (t &Transformer) for_in_container_is_pointer_storage(container_id flat.NodeId) bool {
	mut id := container_id
	for int(id) >= 0 {
		node := t.a.nodes[int(id)]
		if node.kind != .paren || node.children_count != 1 {
			return node.kind == .ident
				&& (t.mut_param_values[node.value] || t.pointer_value_rvalues[node.value])
		}
		id = t.a.child(&node, 0)
	}
	return false
}

fn (t &Transformer) for_in_binding_storage_type(binding_id flat.NodeId, elem_type_id u32, is_mut bool, container_yields_ref bool) u32 {
	// Pointer and optional entries of an explicit reference container are emitted by value.
	clean_elem_source_id := t.normalize_type_alias(elem_type_id)
	clean_elem_shape := flat.source_type_shape(clean_elem_source_id)
	keeps_value := clean_elem_shape.has(.pointer) || t.is_optional_type_name(clean_elem_source_id)
	if is_mut && !(container_yields_ref && keeps_value) {
		return flat.source_type_composition_one(.pointer, elem_type_id)
	}
	if container_yields_ref { return t.for_in_declared_ref_type(binding_id, elem_type_id) }
	return elem_type_id
}

fn (t &Transformer) for_in_container_needs_value_load(container_id flat.NodeId) bool {
	if t.for_in_mut_pointer_alias_value_type(container_id) != none {
		return true
	}
	mut id := container_id
	for int(id) >= 0 && int(id) < t.a.nodes.len {
		node := t.a.nodes[int(id)]
		if node.kind != .paren || node.children_count != 1 {
			return node.kind == .ident && t.pointer_value_rvalues[node.value]
		}
		id = t.a.child(&node, 0)
	}
	return false
}

fn (t &Transformer) for_in_mut_pointer_alias_value_type(container_id flat.NodeId) ?u32 {
	mut id := container_id
	for int(id) >= 0 && int(id) < t.a.nodes.len {
		node := t.a.nodes[int(id)]
		if node.kind == .paren && node.children_count == 1 {
			id = t.a.child(&node, 0)
			continue
		}
		if node.kind == .ident && t.mut_param_values[node.value]
			&& !t.pointer_value_rvalues[node.value] {
			slot_type := t.comptime_normalize_type_alias_chain(t.var_type(node.value))
			if flat.source_type_shape(slot_type).has(.pointer_chain) && for_iter_type_is_container(t.trim_pointer_type(t.trim_pointer_type(slot_type))) {
				return t.trim_pointer_type(slot_type)
			}
		}
		break
	}
	return none
}

fn (mut t Transformer) transform_for_in_container_value(container_id flat.NodeId) flat.NodeId {
	if value_type_source_id := t.for_in_mut_pointer_alias_value_type(container_id) {
		value_type_source_id_5 := value_type_source_id

		// `mut values Alias` carries an ABI slot even when Alias hides the
		// container pointer and the parameter has no explicit-pointer marker.
		value := t.make_prefix(.mul, t.transform_expr(container_id))
		t.set_node_typ(int(value), value_type_source_id_5)
		return value
	}
	return t.transform_expr(container_id)
}

// rebuild_for_in_stmt supports rebuild for in stmt handling for Transformer.
fn (mut t Transformer) rebuild_for_in_stmt(_id flat.NodeId, node flat.Node) []flat.NodeId {
	header_count := node.value.int()
	key_id := t.a.child(&node, 0)
	val_id := t.a.child(&node, 1)
	container_id := t.a.child(&node, 2)
	raw_iter_type_source_id := t.detect_for_in_type(node)
	raw_iter_type_shape := flat.source_type_shape(raw_iter_type_source_id)
	iter_type_source_id := t.normalize_type_alias(raw_iter_type_source_id)
	iter_type_source_id_3 := iter_type_source_id
	iter_value_type := if flat.source_type_shape(iter_type_source_id_3).has(.pointer) {
		flat.source_type_syntax(iter_type_source_id_3).raw_base()
	} else {
		iter_type_source_id_3
	}
	map_iter_type_source_id := t.clean_map_type(iter_type_source_id)
	map_iter_is_map := flat.source_type_shape(map_iter_type_source_id).has(.map_type)
	container_is_pointer_storage := t.for_in_container_is_pointer_storage(container_id)
	container_yields_ref := raw_iter_type_shape.has(.pointer) && !container_is_pointer_storage
	body_ids := t.a.children_of(&node)[header_count..].clone()
	source_is_owned_temporary := !raw_iter_type_shape.has(.pointer)
		&& !t.expr_can_take_address(container_id)
		&& !t.for_in_container_is_borrowed_lock_value(container_id)
	// Route a value `match`/`if` for-in container through value lowering (e.g.
	// `for value in (match node { First { get_values(node)! } ... })`); otherwise the
	// propagating arm tail is lowered in a value-less statement context and emits an
	// empty expression. `transform_value_operand` materializes it into a value temp
	// (stable for the loop's repeated use); non-branch containers keep the existing
	// `stable_expr_for_reuse` / `transform_expr` lowering.
	mut new_container := if t.is_value_match_or_if_operand(container_id) {
		t.transform_value_operand(container_id)
	} else if map_iter_is_map && source_is_owned_temporary {
		t.stable_expr_for_reuse(container_id)
	} else {
		t.transform_for_in_container_value(container_id)
	}
	mut cleanup_owned_snapshot := false
	if map_iter_is_map && !isnil(t.tc)
		&& t.for_in_body_contains_map_delete(body_ids, container_id) {
		// C generation takes a structural snapshot for delete-capable map loops, but its
		// values are shallow copies. First clone destructible entries here so deleting from
		// the source map cannot invalidate an unvisited snapshot binding.
		key_type_source_id, value_type_source_id := t.map_type_parts(map_iter_type_source_id)

		key_needs_clone := t.trimmed_type_id(t.normalize_type_alias(key_type_source_id)) != flat.source_type_string
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type_source_id))
		value_needs_clone := t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type_source_id))
		keys_are_cloneable := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(key_type_source_id)) == none
		values_are_cloneable := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(value_type_source_id)) == none
		if (key_needs_clone || value_needs_clone) && keys_are_cloneable && values_are_cloneable {
			mut snapshot_source := new_container
			if raw_iter_type_shape.has(.pointer) {
				snapshot_source = t.make_prefix(.mul, snapshot_source)
				t.set_node_typ(int(snapshot_source), map_iter_type_source_id)
			}
			new_container = t.make_compiler_default_map_clone_value(snapshot_source, map_iter_type_source_id,
				source_is_owned_temporary)
			cleanup_owned_snapshot = true
		}
	}
	mut new_range_end := flat.NodeId(-1)
	if header_count == 4 {
		range_end_id := t.a.child(&node, 3)
		new_range_end = t.transform_expr(range_end_id)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	if int(new_container) >= 0 && iter_type_source_id != 0 {
		new_container_type := (if cleanup_owned_snapshot {
			map_iter_type_source_id
		} else {
			iter_type_source_id_3
		})
		t.set_node_typ(int(new_container), new_container_type)
	}
	has_index := int(val_id) >= 0
	container_is_range := if int(container_id) >= 0 {
		t.a.nodes[int(container_id)].kind == .range
	} else {
		false
	}
	for binding_id in [key_id, val_id] {
		if int(binding_id) >= 0 {
			binding := t.a.nodes[int(binding_id)]
			if binding.kind == .ident {
				t.clear_heaped_local_binding(binding.value)
			}
		}
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
		if map_iter_is_map {
			// map[K]V: child0 (key) -> key type, child1 (val) -> value type
			key_type := flat.source_type_syntax(map_iter_type_source_id).first_bracket_raw_inner()
			if key_name.len > 0 && key_type != 0 { t.set_var_type(key_name, key_type) }
		} else if flat.source_type_shape(iter_value_type).has(.array_type) || iter_value_type == flat.source_type_string {
			// []E: child0 (index) -> 'int'
			if key_name.len > 0 {
				t.set_var_type(key_name, flat.source_type_int)
			}
		}
		if val_name.len > 0 {
			elem_type := if map_iter_is_map {
				t.map_value_type(map_iter_type_source_id)
			} else {
				t.infer_for_in_elem_type(iter_type_source_id, node)
			}
			if elem_type != 0 {
				val_type_source_id := t.for_in_binding_storage_type(val_id, elem_type, node.op == .amp, container_yields_ref)

				t.set_var_type(val_name, val_type_source_id)
			}
		}
	} else {
		// single var, child0 is the element
		if int(key_id) >= 0 {
			key_name := t.a.nodes[int(key_id)].value
			if key_name.len > 0 {
				elem_type := if map_iter_is_map {
					t.map_value_type(map_iter_type_source_id)
				} else {
					t.infer_for_in_elem_type(iter_type_source_id, node)
				}
				if elem_type != 0 {
					value_type_source_id_3 := t.for_in_binding_storage_type(key_id, elem_type, node.op == .amp, container_yields_ref)

					t.set_var_type(key_name, value_type_source_id_3)
				}
			}
		}
	}

	mut header_key_id := key_id
	mut header_val_id := val_id
	mut binding_names := map[string]string{}
	mut binding_types := map[string]u32{}
	for binding_id in [key_id, val_id] {
		if int(binding_id) < 0 { continue }
		binding := t.a.nodes[int(binding_id)]
		if binding.kind != .ident { continue }
		binding_type_source_id := t.var_type(binding.value)
		if !t.guard_value_needs_heap_storage(binding.value, binding_type_source_id) { continue }
		name := t.new_temp('for_value')
		t.set_var_type(name, binding_type_source_id)
		backend_binding := t.make_ident(name)
		t.set_node_typ(int(backend_binding), binding_type_source_id)
		binding_names[binding.value] = name
		binding_types[binding.value] = binding_type_source_id
		if binding_id == key_id {
			header_key_id = backend_binding
		} else {
			header_val_id = backend_binding
		}
	}

	mut binding_clones := []flat.NodeId{}
	if map_iter_is_map {
		key_type_source_id_2, value_type_source_id_2 := t.map_type_parts(map_iter_type_source_id)

		if has_index {
			key_source_name := if int(key_id) >= 0 { t.a.nodes[int(key_id)].value } else { '' }
			key_name := binding_names[key_source_name] or { key_source_name }
			if t.trimmed_type_id(t.normalize_type_alias(key_type_source_id_2)) != flat.source_type_string {
				binding_clones << t.make_for_in_binding_clone(key_name, key_type_source_id_2)
			}
			value_source_name := if int(val_id) >= 0 { t.a.nodes[int(val_id)].value } else { '' }
			value_name := binding_names[value_source_name] or { value_source_name }
			binding_type_source_id_2 := t.for_in_binding_storage_type(val_id, value_type_source_id_2, node.op == .amp, container_yields_ref)

			binding_clones << t.make_for_in_binding_clone(value_name, binding_type_source_id_2)
		} else {
			source_name := if int(key_id) >= 0 { t.a.nodes[int(key_id)].value } else { '' }
			value_name := binding_names[source_name] or { source_name }
			binding_type_source_id_3 := t.for_in_binding_storage_type(key_id, value_type_source_id_2, node.op == .amp, container_yields_ref)

			binding_clones << t.make_for_in_binding_clone(value_name, binding_type_source_id_3)
		}
	} else if flat.source_type_shape(iter_value_type).has(.array_type) || t.is_fixed_array_type(iter_value_type) {
		source_name := if has_index {
			if int(val_id) >= 0 { t.a.nodes[int(val_id)].value } else { '' }
		} else {
			if int(key_id) >= 0 { t.a.nodes[int(key_id)].value } else { '' }
		}
		value_name := binding_names[source_name] or { source_name }
		elem_type_source_id := t.infer_for_in_elem_type(iter_type_source_id, node)

		bind_id := if has_index { val_id } else { key_id }
		value_type_source_id_4 := t.for_in_binding_storage_type(bind_id, elem_type_source_id, node.op == .amp, container_yields_ref)

		binding_clones << t.make_for_in_binding_clone(value_name, value_type_source_id_4)
	}

	mut binding_decls := []flat.NodeId{}
	for binding_id in [key_id, val_id] {
		if int(binding_id) < 0 { continue }
		binding := t.a.nodes[int(binding_id)]
		if backend_name := binding_names[binding.value] {
			binding_decls << t.make_guard_value_decls(binding.value, t.make_ident(backend_name), binding_types[binding.value])
		}
	}
	mut ids := []flat.NodeId{}
	ids << header_key_id
	ids << header_val_id
	ids << new_container
	if header_count == 4 {
		ids << new_range_end
	}
	mut transformed_body := []flat.NodeId{}
	mut pointer_value_name := ''
	mut fixed_array_value_name := ''
	mut element_type_text := u32(0)
	if map_iter_is_map {
		element_type_text = t.map_value_type(map_iter_type_source_id)
	} else if flat.source_type_shape(iter_value_type).has(.array_type) || t.is_fixed_array_type(iter_value_type) {
		element_type_text = t.infer_for_in_elem_type(iter_type_source_id, node)
	}
	normalized_element_type_source_id := t.normalize_type_alias(element_type_text)
	normalized_element_type_shape := flat.source_type_shape(normalized_element_type_source_id)
	element_value_is_indirect := normalized_element_type_shape.has(.pointer)
		|| t.is_optional_type_name(normalized_element_type_source_id)
		|| (map_iter_is_map && t.is_fixed_array_type(normalized_element_type_source_id))
	if node.op == .amp && (!container_yields_ref
		|| (element_value_is_indirect && !normalized_element_type_shape.has(.pointer)
			&& !t.is_optional_type_name(normalized_element_type_source_id))) {
		bind_id := if has_index { val_id } else { key_id }
		if int(bind_id) >= 0 {
			bind := t.a.nodes[int(bind_id)]
			if bind.kind == .ident && bind.value.len > 0 {
				bind_type := t.var_type(bind.value)
				if flat.source_type_shape(bind_type).has(.pointer) {
					if t.is_fixed_array_type(flat.source_type_syntax(bind_type).raw_base()) {
						fixed_array_value_name = bind.value
					} else {
						pointer_value_name = bind.value
					}
				}
			}
		}
	}
	if pointer_value_name.len > 0 {
		had_pointer_value_lvalue := t.pointer_value_lvalues[pointer_value_name] or { false }
		had_pointer_value_rvalue := t.pointer_value_rvalues[pointer_value_name] or { false }
		t.pointer_value_lvalues[pointer_value_name] = true
		t.pointer_value_rvalues[pointer_value_name] = true
		transformed_body = t.transform_scope_stmts(body_ids)
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
	} else if fixed_array_value_name.len > 0 {
		had_fixed_array_value_rvalue := t.fixed_array_value_rvalues[fixed_array_value_name] or {
			false
		}
		t.fixed_array_value_rvalues[fixed_array_value_name] = true
		transformed_body = t.transform_scope_stmts(body_ids)
		if had_fixed_array_value_rvalue {
			t.fixed_array_value_rvalues[fixed_array_value_name] = true
		} else {
			t.fixed_array_value_rvalues.delete(fixed_array_value_name)
		}
	} else {
		transformed_body = t.transform_scope_stmts(body_ids)
	}
	mut new_body := binding_clones.clone()
	new_body << binding_decls
	new_body << transformed_body
	for bid in new_body {
		ids << bid
	}
	start := t.a.children.len
	for cid in ids {
		t.a.children << cid
	}
	// Without ownership lowering, map iteration bindings are borrowed shallow copies.
	// Destroying a temporary container here would invalidate values retained by the body.
	map_container_needs_drop := !isnil(t.tc) && map_iter_is_map
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(map_iter_type_source_id))
	cleanup_owned_container := cleanup_owned_snapshot
		|| (source_is_owned_temporary && map_container_needs_drop)
	mut cleanup_guard_name := ''
	if cleanup_owned_container {
		cleanup_guard_name = t.new_temp('for_map_container_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), flat.source_type_bool)
		deferred_drop := t.make_expr_stmt(t.make_call_typed('drop_owned', [
			new_container,
		], flat.source_type_void))
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name), t.make_block([
			deferred_drop,
		]), t.make_empty())
		defer_body := t.make_block([guarded_drop])
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind:           .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	prefix << t.a.add_node(flat.Node{
		kind:           .for_in_stmt
		op:             node.op
		children_start: start
		children_count: flat.child_count(ids.len)
		pos:            node.pos
		value:          node.value
		source_value:   node.source_value
		typ:            (if iter_type_source_id != 0 {
			iter_type_source_id_3
		} else {
			node.typ
		})
		flags:          flat.node_flags(node.skip_ownership_drops(), false)
	})
	if cleanup_owned_container {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', [new_container], flat.source_type_void))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	return prefix
}

fn (t &Transformer) for_in_container_is_borrowed_lock_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .lock_expr {
		return true
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.for_in_container_is_borrowed_lock_value(t.a.child(&node, 0))
	}
	return false
}

fn (t &Transformer) for_in_body_contains_map_delete(body []flat.NodeId, container_id flat.NodeId) bool {
	container_key := t.for_in_map_storage_key(container_id)
	if container_key.len == 0 {
		return false
	}
	for id in body {
		if t.for_in_node_contains_map_delete(id, container_key) {
			return true
		}
	}
	return false
}

fn (t &Transformer) for_in_node_contains_map_delete(id flat.NodeId, container_key string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return false
	}
	if node.kind == .call && node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.value == 'delete' && fn_node.children_count > 0 {
			receiver_id := t.a.child(fn_node, 0)
			if t.for_in_map_storage_key(receiver_id) == container_key {
				return true
			}
		}
		if fn_node.kind == .ident && fn_node.value in ['map.delete', 'map__delete']
			&& node.children_count > 1 {
			receiver_id := t.a.child(&node, 1)
			if t.for_in_map_storage_key(receiver_id) == container_key {
				return true
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.for_in_node_contains_map_delete(t.a.child(&node, i), container_key) {
			return true
		}
	}
	return false
}

fn (t &Transformer) for_in_map_storage_key(id flat.NodeId) string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .expr_stmt, .cast_expr, .as_expr] && node.children_count > 0 {
		return t.for_in_map_storage_key(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.op in [.amp, .mul] && node.children_count > 0 {
		return t.for_in_map_storage_key(t.a.child(&node, 0))
	}
	return t.expr_key(id)
}

// make_for_in_binding_clone turns a shallow container iteration copy into an independent
// owner before the user loop body runs. The ownership checker records that cloned binding
// for the iteration-tail drop.
fn (mut t Transformer) make_for_in_binding_clone(name string, typ_id u32) []flat.NodeId {
	if name == '' || name == '_' || !t.ownership_for_in_type_needs_clone(typ_id) {
		return []flat.NodeId{}
	}
	pending_start := t.pending_stmts.len
	cloned := t.make_compiler_default_clone_value(t.make_ident(name), typ_id, true)
	mut stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	stmts << t.make_assign(t.make_ident(name), cloned)
	return stmts
}

fn (t &Transformer) ownership_for_in_type_needs_clone(typ_id u32) bool {
	if isnil(t.tc) || typ_id == u32(0) {
		return false
	}
	parsed := t.tc.parse_type(typ_id)
	if !t.tc.ownership_type_requires_destruction(parsed) {
		return false
	}
	if _ := t.tc.ownership_default_clone_missing_method(parsed) {
		return false
	}
	return t.compiler_default_clone_type_needs_work(typ_id)
}

fn (t &Transformer) iterator_for_in_info(iter_type_id u32) ?IteratorForInInfo {
	mut clean := t.trimmed_type_id(iter_type_id)
	if flat.source_type_shape(clean).has(.pointer) {
		clean = flat.source_type_syntax(clean).raw_base()
	}
	if clean in [flat.source_type_id('RunesIterator'), flat.source_type_id('builtin.RunesIterator')] {
		return IteratorForInInfo{ elem_type: flat.source_type_rune, next_method: 'RunesIterator.next' }
	}
	if isnil(t.tc) || clean == 0 { return none }
	info := t.tc.iterator_for_in_next_call_info_text(clean) or { return none }
	elem_type_source_id := t.iterator_for_in_elem_type_from_next_return(info.return_type) or { return none }
	mut next_method := info.name
	syntax := flat.source_type_syntax(clean)
	generic_base := syntax.generic_app_base()
	is_generic_interface := syntax.generic_app_valid() && (flat.source_type_name(generic_base) in t.tc.interface_names
		|| flat.source_type_name(t.tc.qualify_type_name(generic_base)) in t.tc.interface_names)
	// The method registry stores declaration names; a C module qualifier is not
	// part of the receiver type. Receiver components themselves remain interned.
	if !is_generic_interface && !info.name.contains('.C.')
		&& !for_iter_type_has_generic_placeholder(clean) && info.name.contains('.') {
		method_syntax := flat.source_type_syntax(flat.source_type_id(info.name))
		receiver := flat.source_type_syntax(method_syntax.qualifier())
		mut concrete_receiver := clean
		if receiver.generic_app_valid() && syntax.generic_app_args().len > 0 {
			mut parts := [receiver.generic_app_base()]
			parts << syntax.generic_app_args()
			concrete_receiver = flat.source_type_composition(.generic, parts)
		}
		next_method = flat.source_type_name(flat.source_type_composition_two(.qualified, concrete_receiver, method_syntax.unqualified()))
	}
	return IteratorForInInfo{ elem_type: elem_type_source_id, next_method: next_method }
}

fn (t &Transformer) iterator_for_in_elem_type_from_next_return(ret types.Type) ?u32 {
	if ret is types.OptionType {
		return ret.base_type.source_type_id()
	}
	return none
}

// lower_range_for_in builds lower range for in data for transform.
fn (mut t Transformer) lower_range_for_in(id flat.NodeId, node flat.Node, key_id flat.NodeId, low_id flat.NodeId, high_id flat.NodeId, body_ids []flat.NodeId) []flat.NodeId {
	if int(key_id) < 0 {
		return [id]
	}
	key := t.a.nodes[int(key_id)]
	if key.kind != .ident || key.value.len == 0 {
		return [id]
	}
	range_type_source_id := t.range_loop_var_type_name(low_id)

	// Route value `match`/`if` range bounds through value lowering (e.g.
	// `for i in (match node { First { lower_first(node)! } ... }) .. 10`); otherwise a
	// propagating branch tail is lowered in a value-less statement context and emits an
	// empty expression. `transform_value_operand` materializes such a bound into a value
	// temp (stable for reuse in the loop condition); non-branch bounds keep
	// `stable_expr_for_reuse`. The low bound is evaluated before the high bound: if the high
	// bound hoists a value branch whose prelude can mutate a syntactically stable low bound
	// (`for i in low .. (match node { First { change_low(mut low)! } ... })`), snapshot the
	// low bound's source-order value so the loop initializer reads it before that prelude.
	low := if t.is_value_match_or_if_operand(low_id) {
		t.transform_value_operand(low_id)
	} else if t.operand_hoists_value_branch(high_id) && t.operand_needs_ordering_snapshot(low_id) {
		t.snapshot_expr_for_reuse(low_id)
	} else {
		t.stable_expr_for_reuse(low_id)
	}
	high := if t.is_value_match_or_if_operand(high_id) {
		t.transform_value_operand(high_id)
	} else {
		t.stable_expr_for_reuse(high_id)
	}
	escaping_value := t.guard_value_needs_heap_storage(key.value, range_type_source_id)
	loop_name := if escaping_value {
		t.new_temp('range_index')
	} else if key.value == '_' {
		'__discard_${key.pos.id}_${key.pos.offset}_${key.pos.end}'
	} else {
		key.value
	}
	t.clear_heaped_local_binding(loop_name)
	t.set_var_type(loop_name, range_type_source_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	init := t.make_decl_assign_typed(loop_name, low, range_type_source_id)
	cond := t.make_infix(.lt, t.make_ident(loop_name), high)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(loop_name), .inc))
	mut new_body := []flat.NodeId{}
	if escaping_value {
		new_body << t.make_guard_value_decls(key.value, t.make_ident(loop_name), range_type_source_id)
	}
	new_body << t.transform_scope_stmts(body_ids)
	prefix << t.make_for_stmt(init, cond, post, new_body, node)
	return prefix
}

fn (t &Transformer) range_loop_var_type_name(low_id flat.NodeId) u32 {
	typ_source_id := t.node_type(low_id)
	typ_source_id_2 := typ_source_id

	if typ_source_id == 0 {
		return flat.source_type_int
	}
	clean_source_id := t.normalize_type_alias(typ_source_id)

	if t.is_integer_type_name(clean_source_id) {
		return typ_source_id_2
	}
	return flat.source_type_int
}

fn (mut t Transformer) lower_iterator_for_in(id flat.NodeId, node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, iter_type_id u32, info IteratorForInInfo, has_index bool, body_ids []flat.NodeId) []flat.NodeId {
	if int(key_id) < 0 {
		return [id]
	}
	key := t.a.nodes[int(key_id)]
	if key.kind != .ident || key.value.len == 0 {
		return [id]
	}
	mut elem_name := key.value
	if has_index {
		if int(val_id) < 0 {
			return [id]
		}
		val := t.a.nodes[int(val_id)]
		if val.kind != .ident || val.value.len == 0 {
			return [id]
		}
		elem_name = val.value
	}
	iter_name := t.new_temp('iter')
	next_name := t.new_temp('iter_next')
	// Route a value `match`/`if` iterator container through value lowering so a
	// propagating arm tail is materialized as a value (no-op for non-branch containers).
	iter_expr := t.transform_value_operand(container_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	prefix << t.make_decl_assign_typed(iter_name, iter_expr, iter_type_id)
	escaping_index := has_index && t.guard_value_needs_heap_storage(key.value, flat.source_type_int)
	idx_name := if has_index && (key.value == '_' || escaping_index) {
		t.new_temp('for_idx')
	} else {
		key.value
	}
	init := if has_index {
		t.clear_heaped_local_binding(idx_name)
		t.set_var_type(idx_name, flat.source_type_int)
		t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	} else {
		t.make_empty()
	}
	elem_type_source_id := info.elem_type
	optional_elem_type := flat.source_type_composition_one(.option, elem_type_source_id)
	t.mark_fn_used_name(info.next_method)
	next_receiver := if flat.source_type_shape(t.trimmed_type_id(iter_type_id)).has(.pointer) {
		t.make_ident(iter_name)
	} else {
		t.make_prefix(.amp, t.make_ident(iter_name))
	}
	next_call := t.make_call_typed(info.next_method, [
		next_receiver,
	], optional_elem_type)
	next_decl := t.make_decl_assign_typed(next_name, next_call, optional_elem_type)
	no_value := t.make_prefix(.not, t.make_selector(t.make_ident(next_name), 'ok', flat.source_type_bool))
	break_if_done := t.make_if(no_value, t.make_block([t.a.add(.break_stmt)]), t.make_empty())
	elem_decls := t.make_guard_value_decls(elem_name, t.make_selector(t.make_ident(next_name),
		'value', elem_type_source_id), elem_type_source_id)
	mut loop_body := []flat.NodeId{}
	loop_body << next_decl
	loop_body << break_if_done
	if escaping_index {
		loop_body << t.make_guard_value_decls(key.value, t.make_ident(idx_name), flat.source_type_int)
	}
	loop_body << elem_decls
	loop_body << t.transform_scope_stmts(body_ids)
	post := if has_index {
		t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	} else {
		t.make_empty()
	}
	prefix << t.make_for_stmt(init, t.make_bool_literal(true), post, loop_body, node)
	return prefix
}

// lower_indexed_for_in builds lower indexed for in data for transform.
@[direct_array_access]
fn (mut t Transformer) lower_indexed_for_in(id flat.NodeId, node flat.Node, key_id flat.NodeId, val_id flat.NodeId, container_id flat.NodeId, iter_type_id u32, has_index bool, body_ids []flat.NodeId) []flat.NodeId {
	if int(key_id) < 0 {
		return [id]
	}
	key := t.a.nodes[int(key_id)]
	if key.kind != .ident || key.value.len == 0 {
		return [id]
	}
	container_node := if int(container_id) >= 0 {
		t.a.nodes[int(container_id)]
	} else {
		flat.Node{}
	}
	checker_container_type := t.raw_checker_node_type(container_id)
	raw_container_type := if container_node.kind == .ident
		&& flat.source_type_shape(t.trimmed_type_id(t.var_type(container_node.value))).has(.option_type) {
		t.var_type(container_node.value)
	} else {
		checker_container_type
	}
	source_container_type_source_id := t.comptime_normalize_type_alias_chain((if (t.node_type(container_id)) != 0 {
		t.node_type(container_id)
	} else if raw_container_type != 0 {
		raw_container_type
	} else {
		iter_type_id
	}))
	source_container_type_source_id_2 := source_container_type_source_id
	source_container_shape := flat.source_type_shape(source_container_type_source_id_2)
	// `for x in &arr` and `ref := &arr; for x in ref` request by-reference
	// elements, distinct from `node.op == .amp` (`for mut x in arr`). Mutable
	// parameters and other pointer-backed values carry only a storage pointer and
	// must keep ordinary value iteration.
	container_is_reference_binding := source_container_shape.has(.pointer)
		&& !t.for_in_container_is_pointer_storage(container_id)
	container_is_explicit_reference := (container_node.kind == .prefix
		&& container_node.op == .amp) || container_is_reference_binding
	// `for mut` over a range of a fixed array iterates over a view of the fixed array's
	// storage. Slicing a fixed array copies it, so the writes would be lost.
	mut fixed_range_container := flat.empty_node
	if node.op == .amp {
		range_type_source_id_2 := t.unaliased_value_type(container_id)
		range_type_shape := flat.source_type_shape(range_type_source_id_2)
		if range_type_shape.has(.array_type) {
			if view := t.fixed_array_range_view(container_id, range_type_source_id_2) {
				tmp_name := t.new_temp('for_container')
				t.pending_stmts << t.make_decl_assign_typed(tmp_name, view, range_type_source_id_2)
				fixed_range_container = t.make_ident(tmp_name)
			}
		}
	}
	source_is_owned_temporary := int(fixed_range_container) < 0
		&& !source_container_shape.has(.pointer) && !t.expr_can_take_address(container_id)
	direct_map_index_container := node.op == .amp && container_node.kind == .index
	container_has_smartcast := t.expr_has_smartcast(container_id)
	mut container := if int(fixed_range_container) >= 0 {
		fixed_range_container
	} else if direct_map_index_container {
		if flat.source_type_shape(flat.source_type_syntax(source_container_type_source_id_2).source_wrapper_base()).has(.array_type) {
			t.stabilize_original_lvalue_receiver(container_id) or { container_id }
		} else {
			container_id
		}
	} else if t.is_value_match_or_if_operand(container_id) {
		// Route a value `match`/`if` container through value lowering so a propagating
		// arm tail is materialized into a value temp (stable for the loop's repeated use);
		// non-branch containers keep `stable_expr_for_reuse`.
		t.transform_value_operand(container_id)
	} else if container_has_smartcast {
		// The checker-facing iterator type is already the narrowed collection, but
		// taking the original expression as an lvalue would bypass the active sum
		// smartcast and make cgen index the sum wrapper itself.
		t.transform_expr(container_id)
	} else if t.for_in_container_needs_value_load(container_id) {
		// Load an indirect loop binding before dereferencing its container pointer.
		t.transform_for_in_container_value(container_id)
	} else if t.expr_can_take_address(container_id) {
		mut lvalue := t.transform_lvalue(container_id)
		if !t.is_stable_expr_for_reuse(lvalue) {
			lvalue_type := (if (t.node_type(lvalue)) != 0 && t.node_type(lvalue) != flat.source_type_unknown {
				t.node_type(lvalue)
			} else if source_container_type_source_id != 0 && source_container_type_source_id != flat.source_type_unknown {
				source_container_type_source_id_2
			} else {
				iter_type_id
			})
			lvalue_ptr := t.make_prefix(.amp, lvalue)
			t.set_node_typ(int(lvalue_ptr), flat.source_type_composition_one(.pointer, lvalue_type))
			stable_ptr := t.stable_transformed_expr_for_reuse(lvalue_ptr, flat.source_type_composition_one(.pointer, lvalue_type),
				'for_container')
			lvalue = t.make_prefix(.mul, stable_ptr)
			t.set_node_typ(int(lvalue), lvalue_type)
		}
		lvalue
	} else {
		t.stable_expr_for_reuse(container_id)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	mut actual_iter_type := iter_type_id
	mut optional_container := flat.empty_node
	if payload_type := for_iter_optional_payload_type(raw_container_type) {
		// An active option/sum smartcast has already selected the collection payload.
		// Only add the implicit optional guard when `container` is still the wrapper.
		if payload_type == iter_type_id && !container_has_smartcast {
			optional_container = container
			container = t.make_selector(container, 'value', iter_type_id)
		}
	}
	container_type_source_id := t.comptime_normalize_type_alias_chain(t.node_type(container))
	if container_type_source_id != 0 && container_type_source_id !in [
		flat.source_type_id('array'),
		flat.source_type_id('map'),
		flat.source_type_unknown,
	]
		&& for_iter_type_is_container(container_type_source_id)
		&& (actual_iter_type == 0 || for_iter_type_has_generic_placeholder(iter_type_id))
		&& !(t.is_fixed_array_type(iter_type_id) && !t.is_fixed_array_type(container_type_source_id)) {
		actual_iter_type = container_type_source_id
	}
	if flat.source_type_shape(actual_iter_type).has(.pointer_array) || flat.source_type_shape(container_type_source_id).has(.pointer_array) {
		array_type := (if flat.source_type_shape(container_type_source_id).has(.pointer_array) {
			t.trim_pointer_type(container_type_source_id)
		} else {
			t.trim_pointer_type(actual_iter_type)
		})
		if t.for_in_container_is_shared_array_pointer(id, container_id) {
			t.set_node_typ(int(container), array_type)
		} else {
			container = t.make_prefix(.mul, container)
			t.set_node_typ(int(container), array_type)
		}
		actual_iter_type = array_type
	}
	if flat.source_type_shape(container_type_source_id).has(.pointer) && flat.source_type_shape(t.trim_pointer_type(container_type_source_id)).has(.leading_bracket) && t.is_fixed_array_type(t.trim_pointer_type(container_type_source_id)) {
		container = t.make_prefix(.mul, container)
		t.set_node_typ(int(container), t.trim_pointer_type(container_type_source_id))
		actual_iter_type = t.trim_pointer_type(container_type_source_id)
	}
	if container_type_source_id == flat.source_type_id('&string') {
		container = t.make_prefix(.mul, container)
		t.set_node_typ(int(container), flat.source_type_string)
		actual_iter_type = flat.source_type_string
	}
	mut elem_type := t.infer_for_in_elem_type(actual_iter_type, node)
	if elem_type == 0 {
		return [id]
	}
	if declared_elem_source_id := t.declared_for_in_elem_type(container_id, node) {
		declared_elem_source_id_2 := declared_elem_source_id
		declared_elem_source_id_3 := declared_elem_source_id_2

		if t.normalize_type_alias(declared_elem_source_id_2) == t.normalize_type_alias(elem_type) {
			elem_type = declared_elem_source_id_3
		}
	}
	mut idx_name := key.value
	escaping_index := has_index && t.guard_value_needs_heap_storage(key.value, flat.source_type_int)
	if !has_index || key.value == '_' || escaping_index {
		idx_name = t.new_temp('for_idx')
	}
	mut elem_name := key.value
	if has_index {
		if int(val_id) < 0 {
			return [id]
		}
		val := t.a.nodes[int(val_id)]
		if val.kind != .ident || val.value.len == 0 {
			return [id]
		}
		elem_name = val.value
	}
	t.clear_heaped_local_binding(idx_name)
	t.set_var_type(idx_name, flat.source_type_int)
	elem_is_mut := (node.op == .amp || container_is_explicit_reference)
		&& actual_iter_type != flat.source_type_string
	// Interface payload arrays are borrowed from their runtime box. Preserve the
	// element reference so scalar references stringify as addresses, matching
	// direct interface smartcasts instead of silently copying the element.
	interface_smartcast_ref := flat.source_type_shape(actual_iter_type).has(.array_type)
		&& t.for_in_container_is_interface_smartcast(container_id)
	clean_elem_type_source_id := t.normalize_type_alias(elem_type)
	clean_elem_type_shape := flat.source_type_shape(clean_elem_type_source_id)
	elem_keeps_value := clean_elem_type_shape.has(.pointer)
		|| t.is_optional_type_name(clean_elem_type_source_id)
	elem_needs_ref := (elem_is_mut || interface_smartcast_ref)
		&& !(container_is_explicit_reference && elem_keeps_value)
	elem_var_type := (if elem_needs_ref {
		flat.source_type_composition_one(.pointer, elem_type)
	} else {
		elem_type
	})
	retained_fixed_backing := source_is_owned_temporary && elem_needs_ref
		&& t.is_fixed_array_type(iter_type_id)
		&& (elem_name in t.escaping_amp_sources || elem_name in t.escaping_fixed_array_view_sources)
	mut retained_backing_stmts := []flat.NodeId{}
	mut retained_row_backing_stmts := []flat.NodeId{}
	if retained_fixed_backing {
		backing_name := t.new_temp('for_fixed_backing')
		retained_backing_stmts = t.heap_escaping_value_decl(backing_name, actual_iter_type, actual_iter_type, container, false)
		container = t.make_prefix(.mul, t.make_ident(backing_name))
		t.set_node_typ(int(container), actual_iter_type)
	} else if elem_needs_ref && flat.source_type_shape(actual_iter_type).has(.array_type) && !isnil(t.tc)
		&& (elem_name in t.escaping_amp_sources || elem_name in t.escaping_fixed_array_view_sources) {
		if t.cached_escape_value_contains_fixed_array(t.tc.parse_type(elem_type)) {
			// The binding hides its index from argument lowering. Retain each current
			// buffer before exposing a row, including after growth between iterations.
			t.mark_fn_used('array.retain_fixed_array_buffer')
			retained_row_backing_stmts << t.make_expr_stmt(t.make_method_call(container,
				'retain_fixed_array_buffer', []flat.NodeId{}))
		}
	}
	if int(optional_container) < 0 {
		prefix << retained_backing_stmts
	}

	if elem_needs_ref && t.is_fixed_array_type(iter_type_id) {
		if direct_container := t.fixed_array_map_index_for_in_container(container_id, mut prefix) {
			container = direct_container
		}
	}
	len_expr := if t.is_fixed_array_type(iter_type_id) {
		t.make_for_in_fixed_array_len_expr(actual_iter_type)
	} else {
		t.make_selector(container, 'len', flat.source_type_int)
	}
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), len_expr)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	mut elem_expr := if elem_needs_ref && flat.source_type_shape(actual_iter_type).has(.array_type) {
		t.array_get_ptr(container, t.make_ident(idx_name), elem_type)
	} else if elem_needs_ref {
		t.make_prefix(.amp, t.make_index(container, t.make_ident(idx_name), elem_type))
	} else if flat.source_type_shape(actual_iter_type).has(.array_type) {
		t.array_get_value(container, t.make_ident(idx_name), elem_type)
	} else {
		t.make_index(container, t.make_ident(idx_name), elem_type)
	}
	mut binding_clones := []flat.NodeId{}
	if elem_name != '_' && !elem_needs_ref && t.ownership_for_in_type_needs_clone(elem_type) {
		pending_start := t.pending_stmts.len
		elem_expr = t.make_compiler_default_clone_value(elem_expr, elem_type, true)
		binding_clones = t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
	}
	mut index_decls := []flat.NodeId{}
	if escaping_index {
		index_decls << t.make_guard_value_decls(key.value, t.make_ident(idx_name), flat.source_type_int)
	}
	elem_decls := t.make_guard_value_decls(elem_name, elem_expr, elem_var_type)
	mut transformed_body := []flat.NodeId{}
	if elem_is_mut && elem_needs_ref {
		had_pointer_value_lvalue := t.pointer_value_lvalues[elem_name] or { false }
		had_pointer_value_rvalue := t.pointer_value_rvalues[elem_name] or { false }
		t.pointer_value_lvalues[elem_name] = true
		t.pointer_value_rvalues[elem_name] = true
		transformed_body = t.transform_scope_stmts(body_ids)
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
		transformed_body = t.transform_scope_stmts(body_ids)
	}
	mut new_body := []flat.NodeId{}
	new_body << index_decls
	new_body << binding_clones
	new_body << retained_row_backing_stmts
	if elem_name != '_' {
		// `for _ in a {}` only needs the length; loading every element would be a read
		// of the elements that the race detector reports.
		new_body << elem_decls
	}
	new_body << transformed_body
	cleanup_target := if int(optional_container) >= 0 { optional_container } else { container }
	cleanup_type := (if int(optional_container) >= 0 {
		raw_container_type
	} else {
		actual_iter_type
	})
	container_needs_drop := !isnil(t.tc) && cleanup_type != 0
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(cleanup_type))
	cleanup_temporary := source_is_owned_temporary && container_needs_drop
		&& !retained_fixed_backing
	mut cleanup_guard_name := ''
	if cleanup_temporary {
		cleanup_guard_name = t.new_temp('for_container_live')
		prefix << t.make_decl_assign_typed(cleanup_guard_name, t.make_bool_literal(true), flat.source_type_bool)
		deferred_drop := t.make_expr_stmt(t.make_call_typed('drop_owned', [
			cleanup_target,
		], flat.source_type_void))
		guarded_drop := t.make_if_with_skip_ownership_drops(t.make_ident(cleanup_guard_name), t.make_block([
			deferred_drop,
		]), t.make_empty())
		defer_body := t.make_block([guarded_drop])
		defer_start := t.a.children.len
		t.a.children << defer_body
		prefix << t.a.add_node(flat.Node{
			kind:           .defer_stmt
			children_start: defer_start
			children_count: 1
		})
	}
	for_stmt := t.make_for_stmt(init, cond, post, new_body, node)
	if int(optional_container) >= 0 {
		ok_cond := t.make_selector(optional_container, 'ok', flat.source_type_bool)
		mut selected_body := retained_backing_stmts.clone()
		selected_body << for_stmt
		prefix << t.make_if(ok_cond, t.make_block(selected_body), t.make_empty())
	} else {
		prefix << for_stmt
	}
	if cleanup_temporary {
		prefix << t.make_expr_stmt(t.make_call_typed('drop_owned', [cleanup_target], flat.source_type_void))
		prefix << t.make_assign(t.make_ident(cleanup_guard_name), t.make_bool_literal(false))
	}
	return prefix
}

fn (t &Transformer) for_in_container_is_interface_smartcast(container_id flat.NodeId) bool {
	key := t.expr_key(container_id)
	if key.len == 0 || t.find_smartcast(key) == none {
		return false
	}
	source_type_source_id := t.trim_pointer_type(t.original_expr_type(container_id))

	return t.is_interface_type_name(source_type_source_id)
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

fn (t &Transformer) for_in_container_is_shared_array_pointer(for_id flat.NodeId, container_id flat.NodeId) bool {
	if int(container_id) < 0 || int(container_id) >= t.a.nodes.len {
		return false
	}
	container := t.a.nodes[int(container_id)]
	if container.kind != .ident || container.value.len == 0 {
		return false
	}
	raw_type_source_id := t.raw_var_type(container.value)
	typ := if raw_type_source_id != 0 { raw_type_source_id } else { t.var_type(container.value) }
	if flat.source_type_shape(t.trimmed_type_id(typ)).has(.shared_type) {
		return true
	}
	return t.local_decl_is_shared_before(container.value, for_id)
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
		value:          if src.kind == .for_in_stmt { skip_scope_drops_block_value } else { '' }
		op:             src.op
		children_start: start
		children_count: flat.child_count(3 + body.len)
		pos:            src.pos
		typ:            src.typ
		flags:          flat.node_flags(src.skip_ownership_drops() || src.kind !in [
			.for_stmt,
			.for_in_stmt,
		], false)
	})
}

// detect_for_in_type resolves detect for in type information for transform.
fn (mut t Transformer) detect_for_in_type(node flat.Node) u32 {
	header_count := node.value.int()
	container_idx := if header_count >= 3 { header_count - 1 } else { 2 }
	if node.children_count > container_idx {
		iter_id := t.a.child(&node, container_idx)
		if fixed_array_type_source_id := t.detect_for_in_global_fixed_array_type(iter_id) {
			fixed_array_type_source_id_2 := fixed_array_type_source_id

			return fixed_array_type_source_id_2
		}
		mut iter_node := t.a.nodes[int(iter_id)]
		for iter_node.kind == .paren && iter_node.children_count == 1 {
			iter_node = t.a.child_node(&iter_node, 0)
		}
		if iter_node.kind == .ident && iter_node.value.len > 0 {
			mut raw_local_type := t.raw_var_type(iter_node.value)
			if t.pointer_value_rvalues[iter_node.value] && flat.source_type_shape(raw_local_type).has(.pointer) {
				raw_local_type = t.trim_pointer_type(raw_local_type)
			}
			if raw_local_type != 0 && t.iterator_for_in_info(raw_local_type) != none {
				return raw_local_type
			}
			mut local_type := t.comptime_normalize_type_alias_chain(t.var_type(iter_node.value))
			// Mutable parameters and loop bindings use pointers for their storage.
			// Preserve real source `&map` expressions while removing only this
			// implicit storage pointer from the iterable's value type.
			if (t.mut_param_values[iter_node.value] || t.pointer_value_rvalues[iter_node.value])
				&& flat.source_type_shape(local_type).has(.pointer)
				&& for_iter_type_is_container(t.trim_pointer_type(local_type)) {
				local_type = t.trim_pointer_type(local_type)
			}
			if local_type != 0 && for_iter_type_is_container(local_type) {
				t.set_node_typ(int(iter_id), local_type)
				return local_type
			}
		}
		// The checker records selectors rooted in an interface at the interface level.
		// Inside an `if w is Concrete` branch, resolve the selector again while the
		// transformer's smartcast is active so `w.children` keeps its declared `[]Widget`
		// element type instead of degrading to `[]void`.
		if iter_node.kind == .selector {
			raw_selector_type_source_id := t.resolve_selector_type(iter_node)

			selector_type_source_id := t.normalize_type_alias(raw_selector_type_source_id)
			selector_type_source_id_2 := selector_type_source_id
			if selector_type_source_id != 0 && for_iter_type_is_container(selector_type_source_id_2) {
				t.set_node_typ(int(iter_id), selector_type_source_id)
				return selector_type_source_id_2
			}
		}
		if iter_node.kind == .call {
			concrete_call_type_source_id := t.normalize_type_alias(t.concrete_generic_call_return_type(iter_id,
				iter_node))
			concrete_payload := for_iter_payload_type(concrete_call_type_source_id)
			if concrete_payload != 0 && !for_iter_type_has_generic_placeholder(concrete_payload)
				&& for_iter_type_is_container(concrete_payload) {
				t.set_node_typ(int(iter_id), concrete_call_type_source_id)
				return concrete_payload
			}
		}
		if iter_node.kind == .index && (iter_node.value == 'range'
			|| (iter_node.children_count > 1 && t.a.child_node(&iter_node, 1).kind == .range)) {
			// Late generic reachability can leave checker slice metadata resolved
			// outside this function's local scope. Resolve the slice from its current
			// container before mistaking an array of strings for string iteration.
			slice_type := for_iter_payload_type(t.node_type(iter_id))
			if for_iter_type_is_container(slice_type) {
				t.set_node_typ(int(iter_id), slice_type)
				return slice_type
			}
		}
		checker_type_source_id := t.raw_checker_node_type(iter_id)
		if checker_type_source_id != 0 {
			checker_payload := for_iter_payload_type(checker_type_source_id)
			if for_iter_type_is_container(checker_payload)
				|| t.iterator_for_in_info(checker_payload) != none {
				if checker_payload == checker_type_source_id {
					t.set_node_typ(int(iter_id), checker_type_source_id)
				}
				return checker_payload
			}
		}
		iter_type_source_id_2 := t.node_type(iter_id)
		if iter_type_source_id_2 != 0 {
			iter_payload := for_iter_payload_type(iter_type_source_id_2)
			if node.typ == 0 || for_iter_type_has_generic_placeholder(node.typ)
				|| for_iter_type_is_container(iter_payload) {
				return iter_payload
			}
		}
	}
	if node.typ != 0 {
		return node.typ
	}
	return u32(0)
}

fn for_iter_type_is_container(iter_type_id u32) bool {
	clean := transform_trimmed_type_id(iter_type_id)
	shape := flat.source_type_shape(clean)
	if clean == flat.source_type_string || shape.has(.array_type) || shape.has(.variadic)
		|| shape.has(.map_type) || shape.has(.pointer_map) || shape.has(.leading_bracket) {
		return true
	}
	if shape.has(.pointer) {
		base := flat.source_type_syntax(clean).raw_base()
		return base == flat.source_type_string || flat.source_type_shape(base).has(.leading_bracket)
	}
	return false
}

fn for_iter_payload_type(iter_type_id u32) u32 {
	if payload := for_iter_optional_payload_type(iter_type_id) { return payload }
	return iter_type_id
}

fn for_iter_optional_payload_type(iter_type_id u32) ?u32 {
	clean := transform_trimmed_type_id(iter_type_id)
	if !flat.source_type_shape(clean).has(.optional) { return none }
	payload := flat.source_type_syntax(clean).raw_base()
	if payload != 0 { return transform_trimmed_type_id(payload) }
	return none
}

fn (mut t Transformer) detect_for_in_global_fixed_array_type(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident || node.value.len == 0 {
		return none
	}
	if (t.var_type(node.value)) != 0 {
		return none
	}
	if fixed_storage_type_source_id := t.const_array_literal_storage_type_name_for_expr(id) {
		fixed_storage_type_source_id_2 := fixed_storage_type_source_id

		return fixed_storage_type_source_id_2
	}
	mut candidates := []string{}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${node.value}'
	}
	if !isnil(t.tc) {
		if key := t.tc.shadowed_builtin_const_key_in_module(node.value, t.cur_module) {
			candidates << key
		}
	}
	candidates << node.value
	for candidate in candidates {
		if typ_source_id := t.globals.type_id(candidate) {
			normalized_source_id := t.normalize_type_alias(typ_source_id)

			if t.is_fixed_array_type(normalized_source_id) {
				return normalized_source_id
			}
		}
	}
	if !isnil(t.tc) {
		for candidate in candidates {
			if typ := t.tc.const_types[candidate] {
				normalized_source_id_2 := t.normalize_type_alias(typ.source_type_id())

				if t.is_fixed_array_type(normalized_source_id_2) {
					return normalized_source_id_2
				}
			}
		}
		checker_type_source_id := t.normalize_type_alias(t.tc.resolve_type(id).source_type_id())
		checker_type_source_id_2 := checker_type_source_id

		if t.is_fixed_array_type(checker_type_source_id) {
			return checker_type_source_id_2
		}
	}
	return none
}

fn for_iter_type_has_generic_placeholder(iter_type_id u32) bool {
	clean := transform_trimmed_type_id(iter_type_id)
	if clean == 0 { return false }
	if is_generic_placeholder_type_id(clean) { return true }
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if shape.has(.pointer) || shape.has(.array_type) {
		return for_iter_type_has_generic_placeholder(syntax.raw_base())
	}
	if shape.has(.map_type) {
		return syntax.first_bracket_closed()
			&& (for_iter_type_has_generic_placeholder(syntax.first_bracket_raw_inner())
				|| for_iter_type_has_generic_placeholder(syntax.first_bracket_base()))
	}
	if shape.has(.leading_bracket) {
		return syntax.first_bracket_closed() && for_iter_type_has_generic_placeholder(syntax.first_bracket_base())
	}
	return false
}

fn for_iter_type_has_generic_placeholder_text(iter_type string) bool {
	return for_iter_type_has_generic_placeholder(flat.source_type_id(iter_type))
}

// Infer the element type for the loop variable from the iterable type.
fn (t &Transformer) infer_for_in_elem_type(iter_type_id u32, node flat.Node) u32 {
	shape := flat.source_type_shape(iter_type_id)
	syntax := flat.source_type_syntax(iter_type_id)
	if shape.has(.pointer_array) { return flat.source_type_syntax(syntax.raw_base()).raw_base() }
	if shape.has(.array_type) { return syntax.raw_base() }
	if shape.has(.pointer_map) { return t.infer_for_in_elem_type(syntax.raw_base(), node) }
	if shape.has(.map_type) {
		if syntax.first_bracket_closed() && syntax.first_bracket_base() != 0 {
			value := syntax.first_bracket_base()
			fixed := t.fixed_array_map_value_type_text(value)
			return if fixed != 0 { fixed } else { value }
		}
		return 0
	}
	if iter_type_id == flat.source_type_string {
		return flat.source_type_u8
	}
	if t.is_fixed_array_type(iter_type_id) {
		return t.for_in_fixed_array_elem_type(iter_type_id)
	}
	// Check if the iterable is a range expression
	if node.children_count > 0 {
		iter_id := t.a.child(&node, 0)
		iter_node := t.a.nodes[int(iter_id)]
		if iter_node.kind == .range {
			return flat.source_type_int
		}
	}
	return u32(0)
}

// declared_for_in_elem_type retains an array element's declared alias for the
// lowered loop binding. Indexing still uses the normalized container layout,
// while operations such as `${value}` can observe the alias name and methods.
fn (t &Transformer) declared_for_in_elem_type(container_id flat.NodeId, node flat.Node) ?u32 {
	if isnil(t.tc) || int(container_id) < 0 {
		return none
	}
	container := t.a.nodes[int(container_id)]
	mut container_type := (if container.kind == .ident {
		t.trimmed_type_id(t.raw_var_type(container.value))
	} else {
		t.trimmed_type_id(t.raw_checker_node_type(container_id))
	})
	if container_type == 0 {
		return none
	}
	for _ in 0 .. 16 {
		container_type = for_iter_payload_type(container_type)
		if flat.source_type_shape(container_type).has(.pointer) {
			container_type = t.trim_pointer_type(container_type)
			continue
		}
		if target_source_id := t.tc.type_aliases.type_id(flat.source_type_name(container_type)) {
			if target_source_id == container_type {
				break
			}
			container_type = target_source_id
			continue
		}
		if !flat.source_type_shape(container_type).has(.dotted) {
			qualified := flat.source_type_name(t.tc.qualify_type_name(container_type))
			if target_source_id := t.tc.type_aliases.type_id(qualified) {
				if target_source_id == container_type {
					break
				}
				container_type = target_source_id
				continue
			}
		}
		break
	}
	elem_type_source_id_3 := t.infer_for_in_elem_type(container_type, node)
	elem_type_source_id_2 := elem_type_source_id_3

	if elem_type_source_id_3 != 0 {
		return elem_type_source_id_2
	}
	return none
}

fn (t &Transformer) for_in_declared_ref_type(binding_id flat.NodeId, elem_type_id u32) u32 {
	if int(binding_id) >= 0 {
		checker_type := t.trimmed_type_id(t.raw_checker_node_type(binding_id))
		clean_checker_source_id := t.normalize_type_alias(checker_type)
		clean_checker_shape := flat.source_type_shape(clean_checker_source_id)
		if clean_checker_shape.has(.pointer) || t.is_optional_type_name(clean_checker_source_id) {
			return checker_type
		}
	}
	clean_elem_source_id := t.normalize_type_alias(elem_type_id)
	clean_elem_shape_2 := flat.source_type_shape(clean_elem_source_id)
	if clean_elem_shape_2.has(.pointer) || t.is_optional_type_name(clean_elem_source_id) {
		return elem_type_id
	}
	return flat.source_type_composition_one(.pointer, elem_type_id)
}

fn (mut t Transformer) make_for_in_fixed_array_len_expr(source_id u32) flat.NodeId {
	length_id := t.for_in_fixed_array_len_id(source_id)
	length_syntax := flat.source_type_syntax(length_id)
	if length_syntax.decimal() {
		return t.make_int_literal(length_syntax.decimal_value())
	}
	len_text := flat.source_type_name(length_id)
	if !isnil(t.tc) {
		if v := t.tc.const_int_value(len_text, []string{}) {
			return t.make_int_literal(v)
		}
	}
	if flat.source_type_shape(length_id).has(.dotted) {
		syntax := flat.source_type_syntax(length_id)
		return t.make_selector(t.make_ident(flat.source_type_name(syntax.qualifier())), flat.source_type_name(syntax.unqualified()), flat.source_type_int)
	}
	return t.make_ident(len_text)
}

fn (t &Transformer) for_in_fixed_array_elem_type(source_id u32) u32 {
	clean := transform_trimmed_type_id(source_id)
	syntax := flat.source_type_syntax(clean)
	if flat.source_type_shape(clean).has(.leading_bracket) {
		if !syntax.prefix_dimensions_closed() { return u32(0) }
		if t.is_postfix_fixed_array_type(syntax.prefix_dimension_tail())
			&& syntax.backward_bracket_after_prefix_dims() {
			return syntax.backward_bracket_prefix()
		}
		return syntax.first_bracket_base()
	}
	return syntax.last_bracket_prefix()
}

fn (t &Transformer) for_in_fixed_array_elem_type_bytes(s string) string {
	return flat.source_type_name(t.for_in_fixed_array_elem_type(flat.source_type_id(s)))
}

fn (t &Transformer) for_in_fixed_array_len_id(source_id u32) u32 {
	clean := transform_trimmed_type_id(source_id)
	syntax := flat.source_type_syntax(clean)
	if flat.source_type_shape(clean).has(.leading_bracket) {
		if !syntax.prefix_dimensions_closed() { return u32(0) }
		if t.is_postfix_fixed_array_type(syntax.prefix_dimension_tail())
			&& syntax.backward_bracket_after_prefix_dims() {
			return syntax.backward_bracket_inner()
		}
		return syntax.first_bracket_length()
	}
	return syntax.last_bracket_length()
}

fn (t &Transformer) for_in_fixed_array_len_text(s string) string {
	return flat.source_type_name(t.for_in_fixed_array_len_id(flat.source_type_id(s)))
}

// elem_type_text resolves the indexed source type spelling.
@[inline]
fn (value &IteratorForInInfo) elem_type_text() string {
	return flat.source_type_name(value.elem_type)
}
