module transform

import v.flat
import v.types

fn (mut t Transformer) if_guard_optional_type_name(rhs_id flat.NodeId) u32 {
	mut rhs_type := t.optional_result_expr_type_name(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		rhs_type = t.raw_expr_type_without_smartcast(rhs_id)
	}
	return rhs_type
}

fn (mut t Transformer) transform_if_guard_wrapper_source(rhs_id flat.NodeId, rhs_type_id u32) (flat.NodeId, flat.NodeId) {
	if !isnil(t.tc) && t.tc.ownership_guard_read_moves_value(rhs_id) {
		mut source_id := rhs_id
		for t.a.nodes[int(source_id)].kind == .paren
			&& t.a.nodes[int(source_id)].children_count == 1 {
			source_id = t.a.child(&t.a.nodes[int(source_id)], 0)
		}
		if t.selector_chain_has_sum_shared_field(source_id) {
			wrapper := t.transform_optional_wrapper_expr(rhs_id)
			clear := t.make_assign_without_ownership_drop(source_id, t.make_optional_none(rhs_type_id))
			return wrapper, clear
		}
		if t.expr_can_take_address(rhs_id) {
			source := t.stabilize_transformed_lvalue_for_reuse(t.transform_lvalue_without_smartcast(rhs_id))
			t.mark_optional_wrapper_expr(source, rhs_type_id)
			return source, t.make_assign_without_ownership_drop(source, t.make_optional_none(rhs_type_id))
		}
	}
	return t.transform_optional_wrapper_expr(rhs_id), flat.empty_node
}

fn (mut t Transformer) if_guard_source_clear_stmts(clear_id flat.NodeId) []flat.NodeId {
	if clear_id == flat.empty_node {
		return []flat.NodeId{}
	}
	clear := t.a.nodes[int(clear_id)]
	if clear.kind == .selector_assign && clear.children_count == 2
		&& t.selector_chain_has_sum_shared_field(t.a.child(&clear, 0)) {
		return t.transform_stmt(clear_id)
	}
	return [clear_id]
}

// make_guard_value_decls gives an escaping generated binding the same storage as a source local.
fn (mut t Transformer) make_guard_value_decls(name string, value flat.NodeId, value_type_id u32) []flat.NodeId {
	t.clear_heaped_local_binding(name)
	if t.guard_value_needs_heap_storage(name, value_type_id) {
		return t.heap_escaping_value_decl(name, value_type_id, value_type_id, value, false)
	}
	return [t.make_decl_assign_typed(name, value, value_type_id)]
}

// mut_optional_param_payload_ref returns `&a.value` when `rhs_id` is a `mut a ?T`
// parameter whose payload is passed by value (`&?T`; a struct payload is already a
// pointer, `&?&T`). A `mut` guard binding of it refers to the caller's payload, as in
// V 0.5.2, where the parameter is `?&T`: `if mut v := a { v << 1 }` changes the caller's
// array, not a copy.
fn (mut t Transformer) mut_optional_param_payload_ref(rhs_id flat.NodeId, payload_type_id u32) ?flat.NodeId {
	mut source_id := rhs_id
	for t.a.nodes[int(source_id)].kind == .paren && t.a.nodes[int(source_id)].children_count == 1 {
		source_id = t.a.child(&t.a.nodes[int(source_id)], 0)
	}
	source := t.a.nodes[int(source_id)]
	if source.kind != .ident || !t.mut_param_values[source.value] || t.has_smartcast(source.value) {
		return none
	}
	if payload_type_id == 0 || payload_type_id == flat.source_type_void
		|| flat.source_type_shape(payload_type_id).has(.pointer) {
		return none
	}
	param_type_id := t.var_type(source.value)
	param_type := flat.source_type_name(param_type_id)
	// A named fn's parameter has the option type (cgen reads it through the pointer); a
	// lifted fn literal's keeps the pointer (`&?T`).
	is_slot := param_type.starts_with('&?')
	option_type_id := if is_slot { flat.source_type_id(param_type[1..]) } else { param_type_id }
	option_shape := flat.source_type_shape(option_type_id)
	if !option_shape.has(.option_type) || option_shape.has(.pointer) {
		return none
	}
	mut option := t.make_ident(source.value)
	t.set_node_typ(int(option), param_type_id)
	if is_slot {
		option = t.make_prefix(.mul, option)
		t.set_node_typ(int(option), option_type_id)
	}
	return t.make_prefix(.amp, t.make_selector(option, 'value', payload_type_id))
}

// make_payload_ref_binding declares `name` as a pointer to an option payload. Like a
// `for mut x in a` element, the binding keeps its value type in the source, and its reads
// and writes go through the pointer.
fn (mut t Transformer) make_payload_ref_binding(name string, payload_ref flat.NodeId, payload_type_id u32) flat.NodeId {
	t.clear_heaped_local_binding(name)
	decl := t.make_decl_assign_typed(name, payload_ref, flat.source_type_id('&${flat.source_type_name(payload_type_id)}'))
	t.pointer_value_lvalues[name] = true
	t.pointer_value_rvalues[name] = true
	return decl
}

fn (t &Transformer) guard_value_needs_heap_storage(name string, value_type_id u32) bool {
	clean_type_source_id := t.comptime_normalize_type_alias_chain(value_type_id)
	clean_type_shape := flat.source_type_shape(clean_type_source_id)
	is_fixed_array := t.is_fixed_array_type(clean_type_source_id)
	needs_escape_storage := name in t.escaping_amp_sources
		&& (t.heapable_value_type(clean_type_source_id) || is_fixed_array)
	needs_fixed_view_storage := name in t.escaping_fixed_array_view_sources
		&& !clean_type_shape.has(.pointer)
		&& (t.heapable_value_type(clean_type_source_id) || is_fixed_array)
	needs_capture_storage := name in t.mut_fixed_array_capture_sources && is_fixed_array
	return name != '_' && (needs_escape_storage || needs_fixed_view_storage || needs_capture_storage
		|| t.heap_attr_struct_type(value_type_id))
}

// try_expand_if_guard detects an if-guard pattern where the condition is a
// decl_assign whose RHS is a call returning an optional (?T) or result (!T).
// When detected it expands:
//   if val := maybe_call() { body }
// into:
//   __or_tmp_N := maybe_call()
//   if !__or_tmp_N.is_error { val := __or_tmp_N.data; body... }
//
@[direct_array_access]
fn (mut t Transformer) try_expand_if_guard(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	return t.try_expand_if_guard_with_tail(node, GuardReturnTail{})
}

// GuardReturnTail describes the return or multi-assignment supplied by a guarded `if`.
// A guard expansion lowers each branch tail after introducing the unwrapped bindings.
struct GuardReturnTail {
	active            bool
	ret_typ           u32
	extra_return_vals []flat.NodeId
	source_return_id  flat.NodeId
	assign_lhs_ids    []flat.NodeId
	// Staged assignments retain branch-specific moves of the original destinations.
	assign_original_lhs_ids []flat.NodeId
	assign_move_flags       []string
	assign_shadowed_names   []string
}

struct GuardSmartcastState {
	stack       []SmartcastContext
	invalidated map[string]bool
}

fn (t &Transformer) save_guard_smartcasts() GuardSmartcastState {
	return GuardSmartcastState{
		stack:       t.smartcast_stack.clone()
		invalidated: t.invalidated_smartcasts.clone()
	}
}

// Restore the incoming narrowing before lowering a sibling branch.
fn (mut t Transformer) reset_guard_smartcasts(state GuardSmartcastState) map[string]bool {
	invalidated := t.invalidated_smartcasts.clone()
	t.clear_node_type_memo()
	t.smartcast_stack = state.stack.clone()
	t.clear_node_type_memo()
	t.invalidated_smartcasts = state.invalidated.clone()
	return invalidated
}

// A write in either branch invalidates the narrowing after the guard.
fn (mut t Transformer) merge_guard_smartcasts(state GuardSmartcastState, then_invalidated map[string]bool) {
	mut invalidated := then_invalidated.clone()
	for key, value in t.invalidated_smartcasts {
		if value {
			invalidated[key] = true
		}
	}
	t.clear_node_type_memo()
	t.invalidated_smartcasts = invalidated.move()
	t.clear_node_type_memo()
	t.smartcast_stack = t.non_invalidated_smartcasts(state.stack)
}

struct GuardAssignStaging {
	tail  GuardReturnTail
	decls []flat.NodeId
}

// Keep assignment destinations outside the lexical scopes of generated bindings.
fn (mut t Transformer) stage_guard_assignments(tail GuardReturnTail, has_binding_scope bool) GuardAssignStaging {
	if !has_binding_scope || tail.assign_lhs_ids.len == 0 || tail.assign_original_lhs_ids.len > 0 {
		return GuardAssignStaging{ tail: tail }
	}
	mut decls := []flat.NodeId{}
	mut targets := []flat.NodeId{cap: tail.assign_lhs_ids.len}
	mut move_flags := []string{len: tail.assign_lhs_ids.len}
	for i, lhs_id in tail.assign_lhs_ids {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			targets << lhs_id
			continue
		}
		mut typ := t.lvalue_type(lhs_id)
		if typ == 0 {
			typ = t.original_expr_type(lhs_id)
		}
		if lhs.kind == .ident && t.heaped_amp_locals[lhs.value] && flat.source_type_shape(typ).has(.pointer) {
			typ = t.trim_pointer_type(typ)
		}
		name := t.new_temp('guard_assign')
		decls << t.make_staging_value_decl(name, typ)
		targets << t.make_ident(name)
		if !isnil(t.tc) && t.tc.ownership_type_requires_destruction(t.tc.parse_type(typ)) {
			move_flags[i] = t.new_temp('guard_moved')
			decls << t.make_decl_assign_typed(move_flags[i], t.make_bool_literal(false), flat.source_type_bool)
		}
	}
	return GuardAssignStaging{
		tail:  GuardReturnTail{
			...tail
			assign_lhs_ids:          targets
			assign_original_lhs_ids: tail.assign_lhs_ids
			assign_move_flags:       move_flags
		}
		decls: decls
	}
}

fn guard_assignment_tail(tail GuardReturnTail, names []string) GuardReturnTail {
	if tail.assign_lhs_ids.len == 0 {
		return tail
	}
	mut shadowed := tail.assign_shadowed_names.clone()
	for name in names {
		if name.len > 0 && name != '_' && name !in shadowed {
			shadowed << name
		}
	}
	return GuardReturnTail{
		...tail
		assign_shadowed_names: shadowed
	}
}

fn (t &Transformer) multi_if_assign_target_was_moved(tail GuardReturnTail, index int, values []flat.NodeId) bool {
	if isnil(t.tc) {
		return false
	}
	lhs_id := if tail.assign_original_lhs_ids.len > 0 {
		tail.assign_original_lhs_ids[index]
	} else {
		tail.assign_lhs_ids[index]
	}
	if tail.assign_original_lhs_ids.len > 0
		&& t.expr_root_ident_name(lhs_id) in tail.assign_shadowed_names {
		// A read of a generated binding does not move the shadowed destination.
		return false
	}
	for value_id in values {
		if t.tc.ownership_expr_moves_storage(value_id, lhs_id) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) append_multi_if_assign_move_flag(tail GuardReturnTail, index int, moved bool, mut stmts []flat.NodeId) {
	if moved && index < tail.assign_move_flags.len && tail.assign_move_flags[index].len > 0 {
		stmts << t.make_assign(t.make_ident(tail.assign_move_flags[index]), t.make_bool_literal(true))
	}
}

// Call after restoring the outer bindings, keeping the lowered scope's cleanup markers.
fn (mut t Transformer) finish_guard_assignments(scope flat.NodeId, tail GuardReturnTail, staging GuardAssignStaging) flat.NodeId {
	if staging.decls.len == 0 {
		return scope
	}
	mut stmts := staging.decls.clone()
	stmts << scope
	for i, lhs_id in tail.assign_lhs_ids {
		lhs := t.a.nodes[int(lhs_id)]
		if lhs.kind == .ident && lhs.value == '_' {
			continue
		}
		mut lvalue := if lhs.kind == .ident && t.heaped_amp_locals[lhs.value] {
			t.make_prefix(.mul, t.make_ident(lhs.value))
		} else {
			t.transform_lvalue(lhs_id)
		}
		t.drain_pending(mut stmts)
		move_flag := staging.tail.assign_move_flags[i]
		if move_flag.len > 0 {
			lvalue = t.stabilize_transformed_lvalue_for_reuse(lvalue)
			t.drain_pending(mut stmts)
			mut drops := []flat.NodeId{}
			t.append_owned_lvalue_drop_before_assign(lvalue,
				t.lvalue_type(staging.tail.assign_lhs_ids[i]), mut drops)
			stmts << t.make_if_with_skip_ownership_drops(t.make_prefix(.not,
				t.make_ident(move_flag)), t.make_block_skip_scope_drops(drops), t.make_empty())
		}
		stmts << t.make_assign_after_owned_drop(lvalue, staging.tail.assign_lhs_ids[i])
		t.invalidate_smartcast_for_lvalue(lhs_id)
	}
	return t.make_block_skip_scope_drops(stmts)
}

// guard_branch_stmts lowers the body of a guard branch.
fn (mut t Transformer) guard_branch_stmts(branch_id flat.NodeId, tail GuardReturnTail) []flat.NodeId {
	if tail.assign_lhs_ids.len > 0 {
		block_id := t.multi_if_assign_block(branch_id, tail)
		return t.a.children_of(&t.a.nodes[int(block_id)]).clone()
	}
	if tail.active {
		block_id := t.return_block_from_branch(branch_id, tail.ret_typ, tail.extra_return_vals,
			tail.source_return_id)
		return t.a.children_of(&t.a.nodes[int(block_id)]).clone()
	}
	branch := t.a.nodes[int(branch_id)]
	if branch.kind == .block {
		return t.transform_stmts(t.a.children_of(&branch))
	}
	return t.transform_stmt(branch_id)
}

// guard_else_stmts lowers the `else` of a guard, which may continue an else-if chain.
fn (mut t Transformer) guard_else_stmts(else_id flat.NodeId, tail GuardReturnTail) []flat.NodeId {
	else_node := t.a.nodes[int(else_id)]
	if else_node.kind == .if_expr {
		if tail.assign_lhs_ids.len > 0 {
			return t.lower_multi_if_assign(else_node, tail)
		}
		if tail.active {
			return [
				t.build_return_if_chain(else_id, tail.ret_typ, tail.extra_return_vals,
					tail.source_return_id),
			]
		}
		return [t.transform_else_if_expr(else_id, else_node)]
	}
	return t.guard_branch_stmts(else_id, tail)
}

// try_expand_if_guard_with_tail expands a guard as try_expand_if_guard does. With an active
// tail the guard is the value of a `return`, so its binding gets the same storage as in the
// statement form and the branches end in that return.
@[direct_array_access]
fn (mut t Transformer) try_expand_if_guard_with_tail(node flat.Node, tail GuardReturnTail) ?[]flat.NodeId {
	if node.kind != .if_expr || node.children_count < 2 {
		return none
	}
	cond_id := t.a.child(&node, 0)
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	lhs_ids := t.multi_assign_lhs_ids(cond)
	if lhs_ids.len == 1 {
		if info := t.map_index_info(rhs_id) {
			if tail.active {
				// build_return_map_index_if_guard_chain lowers returned map guards.
				return none
			}
			return t.expand_map_index_if_guard(node, lhs.value, info, tail)
		}
		if info := t.array_index_info(rhs_id) {
			return t.expand_array_index_if_guard(node, lhs.value, info, tail)
		}
		if info := t.array_get_call_info(rhs_id) {
			return t.expand_array_index_if_guard(node, lhs.value, info, tail)
		}
		if info := t.channel_receive_info(rhs_id) {
			return t.expand_channel_receive_if_guard(node, lhs.value, info, tail)
		}
	}
	mut rhs_type := t.if_guard_optional_type_name(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		return none
	}
	rhs_type = t.qualify_optional_type(rhs_type)
	value_type_source_id := t.optional_base_type(rhs_type)

	tmp_name := t.new_temp('if_guard')
	mut rhs_expr, source_clear := t.transform_if_guard_wrapper_source(rhs_id, rhs_type)
	if !t.is_optional_type_name(t.node_type(rhs_expr)) {
		rhs_node := t.a.nodes[int(rhs_expr)]
		if rhs_node.kind == .selector && rhs_node.children_count > 0 {
			rhs_expr = t.make_selector_op(t.a.child(&rhs_node, 0), rhs_node.value, rhs_type, rhs_node.op)
		} else {
			t.set_node_typ(int(rhs_expr), rhs_type)
		}
	}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	tmp_decl := t.make_decl_assign_typed(tmp_name, rhs_expr, rhs_type)
	ok_cond := t.make_selector(t.make_ident(tmp_name), 'ok', flat.source_type_bool)
	source_clear_stmts := t.if_guard_source_clear_stmts(source_clear)
	saved_var_types := t.var_types.clone()
	heaped_state := t.save_heaped_local_state()
	smartcasts := t.save_guard_smartcasts()
	defer {
		t.restore_var_types(saved_var_types)
		t.restore_heaped_local_state(heaped_state)
	}
	mut value_decls := []flat.NodeId{}
	mut binding_names := []string{}
	if lhs_ids.len > 1 {
		if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
			for i, lhs_item_id in lhs_ids {
				lhs_item := t.a.nodes[int(lhs_item_id)]
				if lhs_item.kind != .ident || lhs_item.value == '_' {
					continue
				}
				field_type := rhs_types[i].name()
				payload := t.make_selector(t.make_ident(tmp_name), 'value', value_type_source_id)
				field := t.make_selector(payload, 'arg${i}', flat.source_type_id(field_type))
				value_decls << t.make_guard_value_decls(lhs_item.value, field, flat.source_type_id(field_type))
				binding_names << lhs_item.value
			}
		}
	}
	if value_decls.len == 0 && lhs.is_mut && lhs_ids.len == 1 && lhs.value != '_' {
		if payload_ref := t.mut_optional_param_payload_ref(rhs_id, value_type_source_id) {
			value_decls << t.make_payload_ref_binding(lhs.value, payload_ref, value_type_source_id)
			binding_names << lhs.value
		}
	}
	if value_decls.len == 0 && lhs.value != '_' && value_type_source_id != flat.source_type_void {
		value_decls << t.make_guard_value_decls(lhs.value, t.make_selector(t.make_ident(tmp_name), 'value', value_type_source_id), value_type_source_id)
		binding_names << lhs.value
	}

	then_id := t.a.child(&node, 1)
	mut then_children := []flat.NodeId{}
	for value_decl in value_decls {
		then_children << value_decl
	}
	then_children << t.guard_branch_stmts(then_id, guard_assignment_tail(tail, binding_names))
	then_block := t.make_block_prefix_scope_drops(then_children)
	then_invalidated := t.reset_guard_smartcasts(smartcasts)
	// The saved bindings are restored again on return, and the `else` branch changes the
	// restored ones in place (its `err`), so it must not get the saved array itself.
	t.restore_var_types(saved_var_types.clone())
	t.restore_heaped_local_state(heaped_state)

	mut else_block := flat.empty_node
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_block = t.transform_if_guard_else_block(else_id, tmp_name, tail)
	}
	t.merge_guard_smartcasts(smartcasts, then_invalidated)
	mut expanded := []flat.NodeId{cap: prelude.len + 3}
	for stmt in prelude {
		expanded << stmt
	}
	expanded << tmp_decl
	expanded << source_clear_stmts
	expanded << t.make_if(ok_cond, then_block, else_block)
	return expanded
}

fn (mut t Transformer) expand_channel_receive_if_guard(node flat.Node, lhs_name string, info ChannelReceiveInfo, tail GuardReturnTail) ?[]flat.NodeId {
	val_name := t.new_temp('chan_val')
	ok_name := t.new_temp('chan_ok')
	channel_name := t.new_temp('chan_src')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	channel_expr := t.transform_expr(info.channel_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(val_name, t.zero_value_for_type(info.value_type), info.value_type)
	mut channel_source := channel_expr
	if info.needs_deref {
		channel_source = t.make_prefix(.mul, channel_source)
	}
	channel_cast := t.make_cast(flat.source_type_id('&sync.Channel'), channel_source, flat.source_type_id('&sync.Channel'))
	prelude << t.make_decl_assign_typed(channel_name, channel_cast, flat.source_type_id('&sync.Channel'))
	t.mark_fn_used('sync__Channel__pop')
	pop_call := t.make_call_typed('sync__Channel__pop', [t.make_ident(channel_name),
		t.make_prefix(.amp, t.make_ident(val_name))], flat.source_type_bool)
	prelude << t.make_decl_assign_typed(ok_name, pop_call, flat.source_type_bool)

	then_id := t.a.child(&node, 1)
	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	smartcasts := t.save_guard_smartcasts()
	mut then_children := []flat.NodeId{}
	if lhs_name != '_' {
		then_children << t.make_guard_value_decls(lhs_name, t.make_ident(val_name), info.value_type)
	}
	then_children << t.guard_branch_stmts(then_id, guard_assignment_tail(tail, [lhs_name]))
	then_invalidated := t.reset_guard_smartcasts(smartcasts)
	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)
	then_block := t.make_block(then_children)

	mut else_block := flat.empty_node
	if node.children_count >= 3 {
		else_block = t.guard_else_block(t.a.child(&node, 2), tail)
	}
	t.merge_guard_smartcasts(smartcasts, then_invalidated)
	t.pending_stmts = outer_pending
	mut expanded := []flat.NodeId{cap: prelude.len + 1}
	for stmt in prelude {
		expanded << stmt
	}
	expanded << t.make_if(t.make_ident(ok_name), then_block, else_block)
	return expanded
}

// optional_result_expr_type_name supports optional result expr type name handling for Transformer.
fn (mut t Transformer) optional_result_expr_type_name(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	if !isnil(t.tc) {
		node := t.a.nodes[int(id)]
		if node.kind == .call {
			if decode_ret_source_id := t.json_decode_or_expr_type(id, node) {
				decode_ret_source_id_2 := decode_ret_source_id

				return decode_ret_source_id_2
			}
			if specialized_ret_source_id := t.specialized_interface_method_call_return_type(id, node) {
				specialized_ret_source_id_2 := specialized_ret_source_id

				if t.is_optional_type_name(specialized_ret_source_id) {
					return specialized_ret_source_id_2
				}
			}
			concrete_ret_source_id := t.concrete_generic_call_return_type(id, node)
			concrete_ret_source_id_2 := concrete_ret_source_id

			if t.is_optional_type_name(concrete_ret_source_id) {
				return concrete_ret_source_id_2
			}
			if name := t.tc.resolved_call_name(id) {
				if ret := t.tc.fn_ret_types[name] {
					if ret_name_source_id := t.optional_type_name_from_type(ret) {
						return ret_name_source_id
					}
				}
			}
			ret_name_source_id := t.get_call_return_type(id, node)

			if t.is_optional_type_name(ret_name_source_id) {
				return ret_name_source_id
			}
		}
		if typ := t.tc.expr_type(id) {
			if typ_name_source_id := t.optional_type_name_from_type(typ) {
				typ_name_source_id_2 := typ_name_source_id

				return typ_name_source_id_2
			}
		}
	}
	return t.node_type(id)
}

fn (t &Transformer) optional_type_name_from_type(typ types.Type) ?u32 {
	if typ is types.OptionType {
		base_name := flat.source_type_name(t.value_type_name(typ.base_type))
		if typ.base_type is types.Void || base_name == '__v_option' {
			return flat.source_type_id('?void')
		}
		return flat.source_type_id('?${base_name}')
	}
	if typ is types.ResultType {
		base_name := flat.source_type_name(t.value_type_name(typ.base_type))
		if typ.base_type is types.Void || base_name == '__v_option' {
			return flat.source_type_id('!void')
		}
		return flat.source_type_id('!${base_name}')
	}
	typ_name := typ.source_type_id()
	if t.is_optional_type_name(typ_name) {
		return typ_name
	}
	return none
}

// transform_if_guard_else_block transforms transform if guard else block data for transform.
fn (mut t Transformer) transform_if_guard_else_block(else_id flat.NodeId, err_source string, tail GuardReturnTail) flat.NodeId {
	if t.a.nodes[int(else_id)].kind == .if_expr {
		return t.guard_else_block(else_id, tail)
	}
	heaped_state := t.save_heaped_local_state()
	defer {
		t.restore_heaped_local_state(heaped_state)
	}
	mut children := []flat.NodeId{}
	err_expr := if err_source != '' {
		t.result_error_expr(t.make_ident(err_source))
	} else {
		flat.empty_node
	}
	error_tail := if int(err_expr) >= 0 { guard_assignment_tail(tail, ['err']) } else { tail }
	staging := t.stage_guard_assignments(error_tail, int(err_expr) >= 0)
	err_scope := t.enter_implicit_err_scope(int(err_expr) >= 0)
	t.append_implicit_err_decl(mut children, err_expr)
	children << t.guard_else_stmts(else_id, staging.tail)
	t.leave_implicit_err_scope(err_scope)
	return t.finish_guard_assignments(t.make_block(children), tail, staging)
}

// guard_else_block lowers the `else` of a guard that binds no error.
fn (mut t Transformer) guard_else_block(else_id flat.NodeId, tail GuardReturnTail) flat.NodeId {
	heaped_state := t.save_heaped_local_state()
	defer {
		t.restore_heaped_local_state(heaped_state)
	}
	stmts := t.guard_else_stmts(else_id, tail)
	if stmts.len == 1 && t.a.nodes[int(else_id)].kind == .if_expr && !tail.active {
		return stmts[0]
	}
	return t.make_block(stmts)
}

// transform_else_if_expr transforms transform else if expr data for transform.
fn (mut t Transformer) transform_else_if_expr(else_id flat.NodeId, else_node flat.Node) flat.NodeId {
	if expanded := t.try_expand_if_guard(else_id, else_node) {
		return t.make_block(expanded)
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	new_if := t.transform_if_branches_with_smartcast(else_id, else_node)
	mut local_pending := t.pending_stmts.clone()
	t.pending_stmts = outer_pending
	if local_pending.len == 0 {
		return new_if
	}
	local_pending << new_if
	return t.make_block(local_pending)
}

// expand_map_index_if_guard builds expand map index if guard data for transform.
fn (mut t Transformer) expand_map_index_if_guard(node flat.Node, lhs_name string, info MapIndexInfo, tail GuardReturnTail) ?[]flat.NodeId {
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	ptr_name := t.new_temp('map_ptr')
	key_expr := t.transform_expr_for_type(info.key_id, info.key_type)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(key_name, key_expr, info.key_storage_type)
	prelude << t.make_decl_assign_typed(ptr_name, t.make_map_get_check_expr(map_expr, info.base_type, key_name), flat.source_type_voidptr)

	ptr_ident := t.make_ident(ptr_name)
	ptr_found := t.make_infix(.ne, ptr_ident, t.a.add(.nil_literal))
	value_is_optional := t.is_optional_type_name(info.value_type)
	mut guard_value_type := info.value_type_text()
	ptr_value := t.make_prefix(.mul, t.make_cast(flat.source_type_id('&${info.value_type_text()}'), t.make_ident(ptr_name),
		flat.source_type_id('&${info.value_type_text()}')))
	mut value_expr := ptr_value
	mut found_cond := ptr_found
	if value_is_optional {
		guard_value_type = flat.source_type_name(t.optional_base_type(t.qualify_optional_type(info.value_type)))
		found_cond = t.make_infix(.logical_and, ptr_found,
			t.make_selector(ptr_value, 'ok', flat.source_type_bool))
		value_expr = t.make_selector(ptr_value, 'value', flat.source_type_id(guard_value_type))
	}

	then_id := t.a.child(&node, 1)
	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	smartcasts := t.save_guard_smartcasts()
	mut then_children := []flat.NodeId{}
	// A discarded binding (`if _ := m[k]`) only tests key presence; the value is
	// never read, so skip the value dereference. That deref would otherwise cast to
	// `&${info.value_type_text()}` and, when the value type is unresolved, emit an invalid
	// `void __discard = *(void*)ptr`.
	if lhs_name != '_' {
		then_children << t.make_guard_value_decls(lhs_name, value_expr, flat.source_type_id(guard_value_type))
	}
	then_children << t.guard_branch_stmts(then_id, guard_assignment_tail(tail, [lhs_name]))
	then_block := t.make_block_prefix_scope_drops(then_children)
	then_invalidated := t.reset_guard_smartcasts(smartcasts)
	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)

	mut else_block := flat.empty_node
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_block = t.transform_map_index_if_guard_else_block(else_id, ptr_name,
			info.value_type, tail)
	}
	t.merge_guard_smartcasts(smartcasts, then_invalidated)
	t.pending_stmts = outer_pending
	mut expanded := []flat.NodeId{cap: prelude.len + 1}
	for stmt in prelude {
		expanded << stmt
	}
	expanded << t.make_if(found_cond, then_block, else_block)
	return expanded
}

fn (mut t Transformer) transform_map_index_if_guard_else_block(else_id flat.NodeId, ptr_name string, value_type_id u32, tail GuardReturnTail) flat.NodeId {
	value_type := flat.source_type_name(value_type_id)
	if t.a.nodes[int(else_id)].kind == .if_expr {
		return t.guard_else_block(else_id, tail)
	}
	heaped_state := t.save_heaped_local_state()
	defer {
		t.restore_heaped_local_state(heaped_state)
	}
	is_result := flat.source_type_shape(t.normalize_type_alias(value_type_id)).has(.result_type)
	error_tail := if is_result { guard_assignment_tail(tail, ['err']) } else { tail }
	staging := t.stage_guard_assignments(error_tail, is_result)
	err_scope := t.enter_implicit_err_scope(is_result)
	mut children := []flat.NodeId{}
	if is_result && t.has_ierror_interface() {
		zero := t.make_struct_init(flat.source_type_id('IError'))
		t.append_implicit_err_decl(mut children, zero)
		pointer := t.make_ident(ptr_name)
		found := t.make_infix(.ne, pointer, t.a.add(.nil_literal))
		cast := t.make_cast(flat.source_type_id('&${value_type}'), pointer, flat.source_type_id('&${value_type}'))
		stored := t.make_selector(t.make_prefix(.mul, cast), 'err', flat.source_type_id('IError'))
		missing := t.make_call_typed('error', [
			t.make_string_literal('map key does not exist'),
		], flat.source_type_id('IError'))
		err_target := if t.heaped_amp_locals['err'] {
			t.make_prefix(.mul, t.make_ident('err'))
		} else {
			t.make_ident('err')
		}
		present_branch := t.make_block([t.make_assign(err_target, stored)])
		missing_branch := t.make_block([t.make_assign(err_target, missing)])
		children << t.make_if(found, present_branch, missing_branch)
	}
	children << t.guard_else_stmts(else_id, staging.tail)
	t.leave_implicit_err_scope(err_scope)
	return t.finish_guard_assignments(t.make_block(children), tail, staging)
}

// expand_array_index_if_guard builds expand array index if guard data for transform.
fn (mut t Transformer) expand_array_index_if_guard(node flat.Node, lhs_name string, info ArrayIndexInfo, tail GuardReturnTail) ?[]flat.NodeId {
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	array_expr := t.stable_expr_for_reuse(info.base_id)
	index_name := t.new_temp('arr_idx')
	index_expr := t.transform_expr(info.index_id)
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(index_name, index_expr, flat.source_type_int)

	idx_ident := t.make_ident(index_name)
	lower_ok := t.make_infix(.ge, idx_ident, t.make_int_literal(0))
	upper_ok := t.make_infix(.lt, t.make_ident(index_name), t.array_index_len_expr(info, array_expr))
	found_cond := t.make_infix(.logical_and, lower_ok, upper_ok)

	then_id := t.a.child(&node, 1)
	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	smartcasts := t.save_guard_smartcasts()
	mut then_children := []flat.NodeId{}
	mut guard_value_type := info.value_type_text()
	mut value_expr := t.make_index(array_expr, t.make_ident(index_name), info.value_type)
	if t.is_optional_type_name(info.value_type) {
		guard_value_type = flat.source_type_name(t.optional_base_type(t.qualify_optional_type(info.value_type)))
		opt_name := t.new_temp('arr_opt')
		value_expr = t.make_selector(t.make_ident(opt_name), 'value', flat.source_type_id(guard_value_type))
		then_children << t.make_decl_assign_typed(opt_name, t.make_index(array_expr, t.make_ident(index_name), info.value_type), info.value_type)
	}
	then_children << t.make_guard_value_decls(lhs_name, value_expr, flat.source_type_id(guard_value_type))
	then_children << t.guard_branch_stmts(then_id, guard_assignment_tail(tail, [lhs_name]))
	then_invalidated := t.reset_guard_smartcasts(smartcasts)
	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)
	then_block := t.make_block_prefix_scope_drops(then_children)

	mut else_block := flat.empty_node
	if node.children_count >= 3 {
		else_block = t.guard_else_block(t.a.child(&node, 2), tail)
	}
	t.merge_guard_smartcasts(smartcasts, then_invalidated)
	mut selected_block := then_block
	if t.is_optional_type_name(info.value_type) && then_children.len > 0 {
		opt_decl := then_children[0]
		opt_name := t.a.nodes[int(t.a.child(&t.a.nodes[int(opt_decl)], 0))].value
		ok_cond := t.make_selector(t.make_ident(opt_name), 'ok', flat.source_type_bool)
		mut inner_children := []flat.NodeId{}
		for i in 1 .. then_children.len {
			inner_children << then_children[i]
		}
		inner_if := t.make_if(ok_cond, t.make_block_prefix_scope_drops(inner_children), else_block)
		selected_block = t.make_block([opt_decl, inner_if])
	}
	t.pending_stmts = outer_pending
	mut expanded := []flat.NodeId{cap: prelude.len + 1}
	for stmt in prelude {
		expanded << stmt
	}
	expanded << t.make_if(found_cond, selected_block, else_block)
	return expanded
}

// try_expand_if_expr_value detects an if-expression used as a value and
// lowers it to a mutable temporary so that the C backend sees a simple
// variable instead of a gcc statement-expression.
//
//   x := if cond { a } else { b }
// becomes:
//   mut __if_tmp_N := zero_value
//   if cond { __if_tmp_N = a } else { __if_tmp_N = b }
//   x := __if_tmp_N
fn (mut t Transformer) try_expand_if_expr_value(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.kind != .if_expr {
		return none
	}
	// An if-expression used as a value must have both then and else branches.
	if node.children_count < 3 {
		return none
	}
	mut result_type := t.if_expr_result_type(id, node)
	if guard_result_type_source_id := t.if_expr_guard_result_type(node) {
		guard_result_type_source_id_2 := guard_result_type_source_id

		result_type = guard_result_type_source_id_2
	}
	if result_type == 0 || result_type == flat.source_type_void {
		return none
	}
	return t.try_expand_if_expr_value_for_type(id, node, result_type)
}

fn (mut t Transformer) if_expr_guard_result_type(node flat.Node) ?u32 {
	if node.kind != .if_expr || node.children_count < 3 {
		return none
	}
	cond_id := t.a.child(&node, 0)
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	mut rhs_type := t.if_guard_optional_type_name(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		return none
	}
	rhs_type = t.qualify_optional_type(rhs_type)
	value_type_source_id := t.optional_base_type(rhs_type)

	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	if lhs.value != '_' && value_type_source_id != flat.source_type_void {
		t.clear_heaped_local_binding(lhs.value)
		t.set_var_type(lhs.value, value_type_source_id)
	}
	then_type_source_id := t.stmt_value_type(t.a.child(&node, 1))

	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)

	err_scope := t.enter_implicit_err_scope(flat.source_type_shape(rhs_type).has(.result_type))
	else_id := t.a.child(&node, 2)
	else_node := t.a.nodes[int(else_id)]
	else_type := if else_node.kind == .if_expr {
		if nested_source_id := t.if_expr_guard_result_type(else_node) {
			nested_source_id
		} else {
			t.if_expr_result_type(else_id, else_node)
		}
	} else {
		t.stmt_value_type(else_id)
	}
	t.leave_implicit_err_scope(err_scope)

	mut result := u32(0)
	if then_type_source_id != 0 {
		result = t.merge_if_expr_types(result, t.normalize_type_alias(then_type_source_id))
	}
	if else_type != 0 {
		result = t.merge_if_expr_types(result, t.normalize_type_alias(else_type))
	}
	if result == 0 || result == flat.source_type_void || result == flat.source_type_unknown {
		return none
	}
	return result
}

// try_expand_if_expr_value_for_type
// supports helper handling in transform.
fn (mut t Transformer) try_expand_if_expr_value_for_type(id flat.NodeId, node flat.Node, result_type_id u32) ?flat.NodeId {
	result_type := flat.source_type_name(result_type_id)
	if node.kind != .if_expr || node.children_count < 3 || result_type_id == u32(0)
		|| result_type_id == flat.source_type_void {
		return none
	}
	mut actual_result_type := result_type
	branch_type_source_id := t.if_expr_branch_result_type(node)
	branch_type := flat.source_type_name(branch_type_source_id)
	if t.if_expr_branch_overrides_sum_target(branch_type_source_id, result_type_id) {
		actual_result_type = branch_type
	} else if actual_result_type == 'int' && branch_type_source_id != u32(0) && branch_type_source_id != flat.source_type_int
		&& branch_type_source_id != flat.source_type_unknown && branch_type_source_id != flat.source_type_void {
		// In declaration contexts the target type defaults to int before the
		// branch values are inspected. A default int target does not reflect a
		// non-int merged branch type (e.g. an if-expression whose branches are
		// generic f64 calls like math.min), so use the branch type itself.
		actual_result_type = branch_type
	}
	tmp_name := t.new_temp('if_val')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	prelude << t.make_staging_value_decl(tmp_name, flat.source_type_id(actual_result_type))
	for stmt in t.build_if_value_chain(id, tmp_name, flat.source_type_id(actual_result_type)) {
		prelude << stmt
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	tmp := t.make_ident(tmp_name)
	t.set_node_typ(int(tmp), flat.source_type_id(actual_result_type))
	return tmp
}

// if_expr_branch_overrides_sum_target supports if_expr_branch_overrides_sum_target handling.
fn (t &Transformer) if_expr_branch_overrides_sum_target(branch_type_id u32, target_type_id u32) bool {
	branch_type := flat.source_type_name(branch_type_id)
	target_type := flat.source_type_name(target_type_id)
	if branch_type_id == u32(0) || target_type_id == u32(0) {
		return false
	}
	if branch_type.starts_with('[]') {
		branch_elem := branch_type[2..]
		target_short := if target_type.contains('.') {
			target_type.all_after_last('.')
		} else {
			target_type
		}
		branch_short := if branch_elem.contains('.') {
			branch_elem.all_after_last('.')
		} else {
			branch_elem
		}
		if target_short == branch_short {
			return true
		}
	}
	resolved_target_source_id := t.resolve_sum_name(target_type_id)
	resolved_target_comparison_type_id := resolved_target_source_id
	resolved_target := flat.source_type_name(resolved_target_comparison_type_id)
	if resolved_target_source_id == 0 || !t.sum_types.contains(resolved_target) {
		return false
	}
	clean_branch_source_id := t.trim_pointer_type(branch_type_id)

	if t.sum_target_accepts_variant_type(resolved_target_source_id, clean_branch_source_id) {
		return false
	}
	branch_sum_comparison_type_id := t.resolve_sum_name(clean_branch_source_id)

	variant_sum_comparison_type_id := t.resolve_sum_name(t.find_sum_type_for_variant(clean_branch_source_id))

	return branch_sum_comparison_type_id != resolved_target_comparison_type_id && variant_sum_comparison_type_id != resolved_target_comparison_type_id
}

// if_expr_result_type supports if expr result type handling for Transformer.
fn (t &Transformer) if_expr_result_type(id flat.NodeId, node flat.Node) u32 {
	mut node_typ := u32(0)
	if node.typ != 0 {
		typ_source_id := t.normalize_type_alias(node.typ)
		typ := flat.source_type_name(typ_source_id)
		if typ !in ['array', 'map'] {
			node_typ = typ_source_id
		}
	}
	mut checked_typ := u32(0)
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 {
				checked_typ = t.normalize_type_alias(flat.source_type_id(name))
			}
		}
	}
	branch_typ_source_id := t.if_expr_branch_result_type(node)
	branch_typ_comparison_type_id_2 := branch_typ_source_id
	branch_typ := flat.source_type_name(branch_typ_comparison_type_id_2)
	if !isnil(t.tc) {
		if file := t.a.source_files[node.pos.id] {
			if t.tc.translated_files[file.name] {
				resolved_source_id := t.normalize_type_alias(t.tc.resolve_type(id).source_type_id())
				resolved_source_id_2 := resolved_source_id

				if t.translated_if_numeric_type(branch_typ_source_id)
					&& t.translated_if_numeric_type(resolved_source_id_2) {
					return resolved_source_id
				}
			}
		}
	}
	if branch_typ_comparison_type_id_2 in [u32(0), flat.source_type_unknown] && flat.source_type_name(checked_typ) in ['',
		'unknown']
		&& flat.source_type_name(node_typ) in ['', 'unknown'] && t.if_expr_has_c_macro_values(node) {
		return flat.source_type_int
	}
	if branch_typ.starts_with('[]') && t.is_fixed_array_type(checked_typ)
		&& fixed_array_elem_type(checked_typ) == t.array_elem_type(branch_typ_comparison_type_id_2) {
		return branch_typ_source_id
	}
	if branch_typ.starts_with('[]') && t.is_fixed_array_type(node_typ)
		&& fixed_array_elem_type(node_typ) == t.array_elem_type(branch_typ_comparison_type_id_2) {
		return branch_typ_source_id
	}
	if branch_typ.starts_with('[]') && !flat.source_type_shape(checked_typ).has(.array_type) && !flat.source_type_shape(node_typ).has(.array_type) {
		return branch_typ_source_id
	}
	if t.if_expr_branch_type_overrides(branch_typ_source_id, checked_typ) {
		return branch_typ_source_id
	}
	if t.if_expr_branch_type_overrides(branch_typ_source_id, node_typ) {
		return branch_typ_source_id
	}
	if checked_typ != 0 && flat.source_type_name(checked_typ) !in ['array', 'map'] {
		return checked_typ
	}
	if node_typ != 0 {
		return node_typ
	}
	if branch_typ_source_id != 0 {
		return branch_typ_source_id
	}
	return u32(0)
}

fn (t &Transformer) if_expr_has_c_macro_values(node flat.Node) bool {
	if node.kind != .if_expr || node.children_count < 3 {
		return false
	}
	return t.if_branch_is_c_macro_value(t.a.child(&node, 1))
		&& t.if_branch_is_c_macro_value(t.a.child(&node, 2))
}

fn (t &Transformer) if_branch_is_c_macro_value(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .if_expr {
		return t.if_expr_has_c_macro_values(node)
	}
	if node.kind in [.block, .expr_stmt, .paren] && node.children_count > 0 {
		return t.if_branch_is_c_macro_value(t.a.child(&node, node.children_count - 1))
	}
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base := t.a.child_node(&node, 0)
	return base.kind == .ident && base.value == 'C'
}

// if_expr_branch_type_overrides supports if expr branch type overrides handling for Transformer.
fn (t &Transformer) if_expr_branch_type_overrides(branch_typ_id u32, stale_typ_id u32) bool {
	branch_typ_comparison_type_id := branch_typ_id
	branch_typ := flat.source_type_name(branch_typ_comparison_type_id)
	stale_typ_comparison_type_id := stale_typ_id
	stale_typ := flat.source_type_name(stale_typ_comparison_type_id)
	if branch_typ_id == u32(0) || stale_typ_id == u32(0) || branch_typ_comparison_type_id == stale_typ_comparison_type_id {
		return false
	}
	if stale_typ in ['array', 'map', 'unknown'] {
		return true
	}
	if stale_typ_id == flat.source_type_int && (branch_typ in t.structs
		|| '${t.cur_module}.${branch_typ}' in t.structs) {
		return true
	}
	if t.is_optional_type_name(stale_typ_id) && !t.is_optional_type_name(branch_typ_id) {
		stale_payload_source_id := t.optional_base_type(stale_typ_id)

		return t.normalize_type_alias(stale_payload_source_id) == t.normalize_type_alias(branch_typ_id)
	}
	if stale_typ in t.enum_types && branch_typ_id == flat.source_type_int {
		return false
	}
	if stale_typ.starts_with('&') && branch_typ.starts_with('&') {
		stale_inner := stale_typ[1..]
		branch_inner := branch_typ[1..]
		if stale_inner in ['int', 'void', 'unknown'] && branch_inner !in ['int', 'void', 'unknown'] {
			return true
		}
	}
	if stale_typ.starts_with('[]') || t.is_fixed_array_type(stale_typ_id) {
		return !branch_typ.starts_with('[]') && !t.is_fixed_array_type(branch_typ_id)
	}
	if stale_typ.starts_with('map[') {
		return !branch_typ.starts_with('map[')
	}
	return false
}

// if_expr_branch_result_type supports if expr branch result type handling for Transformer.
fn (t &Transformer) if_expr_branch_result_type(node flat.Node) u32 {
	mut result := u32(0)
	if node.children_count >= 2 {
		then_smartcasts :=
			t.smartcast_contexts_from_is_exprs(t.extract_all_is_exprs(t.a.child(&node, 0)))
		then_type_source_id := t.stmt_value_type_with_smartcasts(t.a.child(&node, 1), then_smartcasts)

		if then_type_source_id != 0 {
			result = t.merge_translated_if_expr_types(node, result, t.normalize_type_alias(then_type_source_id))
		}
	}
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		else_node := t.a.nodes[int(else_id)]
		else_type := if else_node.kind == .if_expr {
			t.if_expr_result_type(else_id, else_node)
		} else {
			t.stmt_value_type(else_id)
		}
		if else_type != 0 {
			result = t.merge_translated_if_expr_types(node, result, t.normalize_type_alias(else_type))
		}
	}
	return result
}

fn (t &Transformer) translated_if_numeric_type(name_id u32) bool {
	name_comparison_type_id := name_id
	name_source_id := name_comparison_type_id
	name := flat.source_type_name(name_source_id)
	return is_numeric_type_name(name_source_id) || name_comparison_type_id in [
		flat.source_type_i32,
		flat.source_type_char,
	] || name in t.enum_types
}

fn (t &Transformer) merge_translated_if_expr_types(node flat.Node, current_id u32, next_id u32) u32 {
	if !isnil(t.tc) {
		if file := t.a.source_files[node.pos.id] {
			if t.tc.translated_files[file.name] {
				if current_id == flat.source_type_bool && t.translated_if_numeric_type(next_id) {
					return next_id
				}
				if next_id == flat.source_type_bool && t.translated_if_numeric_type(current_id) {
					return current_id
				}
			}
		}
	}
	return t.merge_if_expr_types(current_id, next_id)
}

// smartcast_contexts_from_is_exprs converts smartcast contexts from is exprs data for transform.
fn (t &Transformer) smartcast_contexts_from_is_exprs(infos []IsExprInfo) []SmartcastContext {
	mut result := []SmartcastContext{cap: infos.len}
	for info in infos {
		result << SmartcastContext{
			type_texts:    t.var_type_texts
			expr_name:     info.expr_name
			variant_name:  info.variant_name
			sum_type_name: info.sum_type_name
		}
	}
	return result
}

// stmt_value_type_with_smartcasts
// supports helper handling in transform.
fn (t &Transformer) stmt_value_type_with_smartcasts(id flat.NodeId, contexts []SmartcastContext) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return u32(0)
		}
		.expr_stmt {
			if node.children_count > 0 {
				return t.node_type_with_smartcasts(t.a.child(&node, node.children_count - 1), contexts)
			}
			return u32(0)
		}
		.block {
			for i := node.children_count - 1; i >= 0; i-- {
				typ_source_id := t.stmt_value_type_with_smartcasts(t.a.child(&node, i), contexts)

				if typ_source_id != 0 {
					return typ_source_id
				}
			}
			return u32(0)
		}
		else {
			return t.node_type_with_smartcasts(id, contexts)
		}
	}
}

// node_type_with_smartcasts supports node type with smartcasts handling for Transformer.
fn (t &Transformer) node_type_with_smartcasts(id flat.NodeId, contexts []SmartcastContext) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.or_expr {
			if node.children_count > 0 {
				source_id := t.a.child(&node, 0)
				source_node := t.a.nodes[int(source_id)]
				source_type_source_id := t.json_decode_or_expr_type(source_id, source_node) or {
					t.node_type_with_smartcasts(source_id, contexts)
				}

				if t.is_optional_type_name(source_type_source_id) {
					value_type_source_id := t.optional_base_type(source_type_source_id)
					value_type_comparison_type_id_2 := value_type_source_id

					if value_type_source_id != 0 && value_type_comparison_type_id_2 !in [
						flat.source_type_unknown,
						flat.source_type_void,
					] {
						return value_type_source_id
					}
				}
			}
			return t.node_type(id)
		}
		.call {
			if node.children_count > 0 {
				callee := t.a.child_node(&node, 0)
				if callee.kind == .selector && callee.value == 'clone' && callee.children_count > 0 {
					base_id := t.a.child(callee, 0)
					raw_base_type_source_id := t.node_type(base_id)

					narrowed_base_type_source_id := t.node_type_with_smartcasts(base_id, contexts)

					call_type_source_id := t.node_type(id)

					if t.is_optional_type_name(raw_base_type_source_id) && t.is_optional_type_name(call_type_source_id)
						&& !t.is_optional_type_name(narrowed_base_type_source_id)
						&& t.normalize_type_alias(t.optional_base_type(call_type_source_id)) == t.normalize_type_alias(narrowed_base_type_source_id) {
						return narrowed_base_type_source_id
					}
				}
			}
			// `node_type` alone leaves a call's return type unresolved until the
			// checker/monomorphizer has seen it, which yields the default `int` for
			// a call to a generic function. Resolve the call's return type instead
			// so branch types (e.g. an if-expression whose branch is `math.min[T]`)
			// keep the called function's real type.
			mut call_ret := t.get_call_return_type(id, node)
			if call_ret == 0 || call_ret in [flat.source_type_unknown, flat.source_type_void]
				|| t.generic_arg_is_unresolved(call_ret) {
				call_ret = t.generic_call_declared_return_type(id, node) or { u32(0) }
			}
			if call_ret != 0 && call_ret !in [flat.source_type_unknown, flat.source_type_void] {
				return call_ret
			}
			return t.node_type(id)
		}
		.ident {
			if sc := t.find_smartcast_in_context(node.value, contexts) {
				return t.smartcast_target_type(sc)
			}
			return t.node_type(id)
		}
		.selector {
			if node.children_count == 0 {
				return t.node_type(id)
			}
			full_key := t.expr_key(id)
			if full_key.len > 0 {
				if sc := t.find_smartcast_in_context(full_key, contexts) {
					return t.smartcast_target_type(sc)
				}
			}
			base_id := t.a.child(&node, 0)
			base_key := t.expr_key(base_id)
			if base_key.len > 0 {
				if sc := t.find_smartcast_in_context(base_key, contexts) {
					variant_type_source_id := t.qualify_variant(t.trim_pointer_type(sc.variant_name), sc.sum_type_name)

					if ftyp_source_id := t.lookup_struct_field_type(variant_type_source_id, node.value) {
						return ftyp_source_id
					}
					if ftyp_source_id := t.lookup_struct_field_type(t.trim_pointer_type(sc.variant_name), node.value) {
						return ftyp_source_id
					}
				}
			}
			base_type_source_id := t.node_type_with_smartcasts(base_id, contexts)
			base_type := flat.source_type_name(base_type_source_id)
			if builtin_type_source_id := t.builtin_selector_type(base_type_source_id, node.value) {
				return builtin_type_source_id
			}
			clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
			if ftyp_source_id := t.lookup_struct_field_type(flat.source_type_id(clean_base), node.value) {
				return ftyp_source_id
			}
			return t.node_type(id)
		}
		.prefix {
			if node.children_count == 0 {
				return t.node_type(id)
			}
			child_id := t.a.child(&node, 0)
			child_type_source_id := t.node_type_with_smartcasts(child_id, contexts)
			child_type := flat.source_type_name(child_type_source_id)
			child := t.a.nodes[int(child_id)]
			if node.op == .amp && child.kind == .ident && child.value in t.heaped_amp_locals
				&& child_type.starts_with('&') {
				// A promoted value already names its heap address, including in an if branch.
				return child_type_source_id
			}
			if node.op == .amp && child_type_source_id != 0 {
				return flat.source_type_id('&${child_type}')
			}
			if node.op == .mul && child_type.starts_with('&') {
				return flat.source_type_id(child_type[1..])
			}
			if node.op == .not {
				return flat.source_type_bool
			}
			return t.node_type(id)
		}
		.paren {
			if node.children_count > 0 {
				return t.node_type_with_smartcasts(t.a.child(&node, 0), contexts)
			}
			return t.node_type(id)
		}
		.array_literal {
			if node.typ != 0 {
				typ_source_id := t.normalize_type_alias(node.typ)
				typ := flat.source_type_name(typ_source_id)
				if typ != 'array' {
					return typ_source_id
				}
			}
			if node.children_count > 0 {
				elem_type_source_id := t.node_type_with_smartcasts(t.a.child(&node, 0), contexts)
				elem_type := flat.source_type_name(elem_type_source_id)
				if elem_type_source_id != 0 {
					return flat.source_type_id('[]${elem_type}')
				}
			}
			return t.node_type(id)
		}
		.index {
			base_type := if node.children_count > 0 {
				flat.source_type_name(t.node_type_with_smartcasts(t.a.child(&node, 0), contexts))
			} else {
				''
			}
			if node.value == 'range' && base_type.starts_with('[]') {
				return flat.source_type_id(base_type)
			}
			if base_type.starts_with('[]') {
				return flat.source_type_id(base_type[2..])
			}
			return t.node_type(id)
		}
		.if_expr {
			return t.if_expr_result_type(id, node)
		}
		.match_stmt {
			return t.match_expr_type(node)
		}
		else {
			return t.node_type(id)
		}
	}
}

// find_smartcast_in_context resolves find smartcast in context information for transform.
fn (t &Transformer) find_smartcast_in_context(expr_name string, contexts []SmartcastContext) ?SmartcastContext {
	mut i := contexts.len - 1
	for i >= 0 {
		if contexts[i].expr_name == expr_name {
			return contexts[i]
		}
		i--
	}
	return t.find_smartcast(expr_name)
}

// merge_if_expr_types supports merge if expr types handling for Transformer.
fn (t &Transformer) merge_if_expr_types(current_id u32, next_id u32) u32 {
	current_comparison_type_id_2 := current_id
	current := flat.source_type_name(current_comparison_type_id_2)
	next_comparison_type_id_2 := next_id
	next := flat.source_type_name(next_comparison_type_id_2)
	if current_id == u32(0) || current_id == flat.source_type_unknown {
		return next_id
	}
	if next_id == u32(0) || next_id == flat.source_type_unknown || current_comparison_type_id_2 == next_comparison_type_id_2 {
		return current_id
	}
	if current == 'array' && next.starts_with('[]') {
		return next_id
	}
	if next == 'array' && current.starts_with('[]') {
		return current_id
	}
	if current.starts_with('[]') && t.is_fixed_array_type(next_id)
		&& t.array_elem_type(current_comparison_type_id_2) == fixed_array_elem_type(next_comparison_type_id_2) {
		return current_id
	}
	if next.starts_with('[]') && t.is_fixed_array_type(current_id)
		&& t.array_elem_type(next_comparison_type_id_2) == fixed_array_elem_type(current_comparison_type_id_2) {
		return next_id
	}
	if current.starts_with('[]') && !next.starts_with('[]') && current[2..] == next {
		return current_id
	}
	if next.starts_with('[]') && !current.starts_with('[]') && next[2..] == current {
		return next_id
	}
	if t.is_optional_type_name(current_id) && !t.is_optional_type_name(next_id) {
		base_source_id := t.optional_base_type(current_id)
		base_comparison_type_id := base_source_id

		if base_comparison_type_id in [flat.source_type_void, flat.source_type_unknown] {
			return flat.source_type_id(current[..1] + next)
		}
		if t.normalize_type_alias(base_source_id) == t.normalize_type_alias(next_id) {
			return current_id
		}
	}
	if t.is_optional_type_name(next_id) && !t.is_optional_type_name(current_id) {
		base_source_id_2 := t.optional_base_type(next_id)
		base_comparison_type_id_2 := base_source_id_2

		if base_comparison_type_id_2 in [flat.source_type_void, flat.source_type_unknown] {
			return flat.source_type_id(next[..1] + current)
		}
		if t.normalize_type_alias(base_source_id_2) == t.normalize_type_alias(current_id) {
			return next_id
		}
	}
	// A `none` branch (`?void`) takes the option type of the other branch, as in
	// `if p.len == 0 { none } else { prefix }` with a `?string` prefix.
	if current.starts_with('?') && next.starts_with('?') {
		if t.optional_base_type(current_id) in [flat.source_type_void, flat.source_type_unknown] {
			return next_id
		}
		if t.optional_base_type(next_id) in [flat.source_type_void, flat.source_type_unknown] {
			return current_id
		}
	}
	return current_id
}

// build_if_value_chain builds if value chain data for transform.
fn (mut t Transformer) build_if_value_chain(if_id flat.NodeId, target_name string, target_type_id u32) []flat.NodeId {
	if_node := t.a.nodes[int(if_id)]
	if if_node.kind != .if_expr || if_node.children_count < 2 {
		return []flat.NodeId{}
	}
	if guard_chain := t.build_if_value_guard_chain(if_node, target_name, target_type_id) {
		return guard_chain
	}
	cond_id := t.a.child(&if_node, 0)
	then_id := t.a.child(&if_node, 1)
	has_else := if_node.children_count >= 3

	all_is := t.extract_all_is_exprs(cond_id)
	else_smartcasts := t.extract_else_branch_smartcasts(cond_id)
	new_cond := t.transform_and_chain_smartcasts(cond_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)

	for info in all_is {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	then_block := t.if_value_branch_block(then_id, target_name, target_type_id)
	for _ in all_is {
		t.pop_smartcast()
	}

	mut else_block := flat.empty_node
	if has_else {
		for info in else_smartcasts {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
		else_id := t.a.child(&if_node, 2)
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			else_block = t.make_block(t.build_if_value_chain(else_id, target_name, target_type_id))
		} else {
			else_block = t.if_value_branch_block(else_id, target_name, target_type_id)
		}
		for _ in else_smartcasts {
			t.pop_smartcast()
		}
	}
	result << t.make_if(new_cond, then_block, else_block)
	return result
}

// build_if_value_guard_chain builds if value guard chain data for transform.
fn (mut t Transformer) build_if_value_guard_chain(if_node flat.Node, target_name string, target_type_id u32) ?[]flat.NodeId {
	if if_node.children_count < 3 {
		return none
	}
	cond_id := t.a.child(&if_node, 0)
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .decl_assign || cond.children_count < 2 {
		return none
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return none
	}
	if info := t.map_index_info(rhs_id) {
		return t.build_map_index_if_value_guard_chain(if_node, lhs.value, info, target_name, target_type_id)
	}
	if info := t.array_index_info(rhs_id) {
		return t.build_array_index_if_value_guard_chain(if_node, lhs.value, info, target_name, target_type_id)
	}
	mut rhs_type := t.if_guard_optional_type_name(rhs_id)
	if !t.is_optional_type_name(rhs_type) {
		return none
	}
	rhs_type = t.qualify_optional_type(rhs_type)
	value_type_source_id := t.optional_base_type(rhs_type)

	tmp_name := t.new_temp('if_guard')
	rhs_expr, source_clear := t.transform_if_guard_wrapper_source(rhs_id, rhs_type)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(tmp_name, rhs_expr, rhs_type)
	result << t.if_guard_source_clear_stmts(source_clear)
	ok_cond := t.make_selector(t.make_ident(tmp_name), 'ok', flat.source_type_bool)
	// An if-guard condition stores the call at child 1 and any extra
	// destructured names after it: [lhs0, rhs, lhs1, lhs2, ...].
	mut lhs_ids := [lhs_id]
	for i in 2 .. cond.children_count {
		lhs_ids << t.a.child(&cond, i)
	}
	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	mut value_decls := []flat.NodeId{}
	if lhs_ids.len > 1 {
		if rhs_types := t.multi_return_types_for_expr(rhs_id, lhs_ids.len) {
			for i, lhs_item_id in lhs_ids {
				lhs_item := t.a.nodes[int(lhs_item_id)]
				if lhs_item.kind != .ident || lhs_item.value == '_' {
					continue
				}
				field_type := rhs_types[i].name()
				payload := t.make_selector(t.make_ident(tmp_name), 'value', value_type_source_id)
				field := t.make_selector(payload, 'arg${i}', flat.source_type_id(field_type))
				value_decls << t.make_guard_value_decls(lhs_item.value, field, flat.source_type_id(field_type))
			}
		}
	}
	if value_decls.len == 0 && lhs.value != '_' && value_type_source_id != flat.source_type_void {
		value_decls << t.make_guard_value_decls(lhs.value, t.make_selector(t.make_ident(tmp_name), 'value', value_type_source_id), value_type_source_id)
	}
	then_id := t.a.child(&if_node, 1)
	then_block0 := t.if_value_branch_block(then_id, target_name, target_type_id)
	mut then_children := []flat.NodeId{cap: int(t.a.nodes[int(then_block0)].children_count) + value_decls.len}
	then_children << value_decls
	then_children << t.a.children_of(&t.a.nodes[int(then_block0)])
	then_block := t.make_block_prefix_scope_drops(then_children)
	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)

	else_id := t.a.child(&if_node, 2)
	else_node := t.a.nodes[int(else_id)]
	mut err_decls := []flat.NodeId{cap: 1}
	err_scope := t.enter_implicit_err_scope(flat.source_type_shape(t.node_type(t.make_ident(tmp_name))).has(.result_type))
	t.append_implicit_err_decl(mut err_decls, t.result_error_expr(t.make_ident(tmp_name)))
	else_block0 := if else_node.kind == .if_expr {
		t.make_block(t.build_if_value_chain(else_id, target_name, target_type_id))
	} else {
		t.if_value_branch_block(else_id, target_name, target_type_id)
	}
	t.leave_implicit_err_scope(err_scope)
	mut else_children := []flat.NodeId{cap: int(t.a.nodes[int(else_block0)].children_count) + 1}
	else_children << err_decls
	else_children << t.a.children_of(&t.a.nodes[int(else_block0)])
	else_block := t.make_block(else_children)
	result << t.make_if(ok_cond, then_block, else_block)
	return result
}

// build_map_index_if_value_guard_chain supports build_map_index_if_value_guard_chain handling.
fn (mut t Transformer) build_map_index_if_value_guard_chain(if_node flat.Node, lhs_name string, info MapIndexInfo, target_name string, target_type_id u32) []flat.NodeId {
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	map_expr := t.stable_expr_for_reuse(info.base_id)
	key_name := t.new_temp('map_key')
	ptr_name := t.new_temp('map_ptr')
	key_expr := t.transform_expr_for_type(info.key_id, info.key_type)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(key_name, key_expr, info.key_storage_type)
	result << t.make_decl_assign_typed(ptr_name, t.make_map_get_check_expr(map_expr, info.base_type, key_name), flat.source_type_voidptr)
	ptr_ident := t.make_ident(ptr_name)
	found_cond := t.make_infix(.ne, ptr_ident, t.a.add(.nil_literal))

	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	mut then_children := []flat.NodeId{}
	if lhs_name != '_' {
		ptr_value := t.make_prefix(.mul, t.make_cast(flat.source_type_id('&${info.value_type_text()}'), t.make_ident(ptr_name), flat.source_type_id('&${info.value_type_text()}')))
		then_children << t.make_guard_value_decls(lhs_name, ptr_value, info.value_type)
	}
	then_id := t.a.child(&if_node, 1)
	then_block0 := t.if_value_branch_block(then_id, target_name, target_type_id)
	then_children << t.a.children_of(&t.a.nodes[int(then_block0)])
	then_block := t.make_block_prefix_scope_drops(then_children)
	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)

	else_id := t.a.child(&if_node, 2)
	else_node := t.a.nodes[int(else_id)]
	else_block := if else_node.kind == .if_expr {
		t.make_block(t.build_if_value_chain(else_id, target_name, target_type_id))
	} else {
		t.if_value_branch_block(else_id, target_name, target_type_id)
	}
	t.pending_stmts = outer_pending
	result << t.make_if(found_cond, then_block, else_block)
	return result
}

// build_array_index_if_value_guard_chain supports build_array_index_if_value_guard_chain handling.
fn (mut t Transformer) build_array_index_if_value_guard_chain(if_node flat.Node, lhs_name string, info ArrayIndexInfo, target_name string, target_type_id u32) []flat.NodeId {
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	array_expr := t.stable_expr_for_reuse(info.base_id)
	index_name := t.new_temp('arr_idx')
	index_expr := t.transform_expr(info.index_id)
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << t.make_decl_assign_typed(index_name, index_expr, flat.source_type_int)
	idx_ident := t.make_ident(index_name)
	lower_ok := t.make_infix(.ge, idx_ident, t.make_int_literal(0))
	upper_ok := t.make_infix(.lt, t.make_ident(index_name), t.array_index_len_expr(info, array_expr))
	found_cond := t.make_infix(.logical_and, lower_ok, upper_ok)

	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	mut then_children := []flat.NodeId{}
	mut opt_decl := flat.empty_node
	mut opt_name := ''
	if lhs_name != '_' {
		mut value_type := info.value_type_text()
		mut value := t.make_index(array_expr, t.make_ident(index_name), info.value_type)
		if t.is_optional_type_name(info.value_type) {
			value_type = flat.source_type_name(t.optional_base_type(t.qualify_optional_type(info.value_type)))
			opt_name = t.new_temp('arr_opt')
			opt_decl = t.make_decl_assign_typed(opt_name, t.make_index(array_expr, t.make_ident(index_name), info.value_type), info.value_type)
			value = t.make_selector(t.make_ident(opt_name), 'value', flat.source_type_id(value_type))
		}
		then_children << t.make_guard_value_decls(lhs_name, value, flat.source_type_id(value_type))
	} else if t.is_optional_type_name(info.value_type) {
		opt_name = t.new_temp('arr_opt')
		opt_decl = t.make_decl_assign_typed(opt_name, t.make_index(array_expr, t.make_ident(index_name), info.value_type), info.value_type)
	}
	then_id := t.a.child(&if_node, 1)
	then_block0 := t.if_value_branch_block(then_id, target_name, target_type_id)
	then_children << t.a.children_of(&t.a.nodes[int(then_block0)])
	then_block := t.make_block_prefix_scope_drops(then_children)
	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)

	else_id := t.a.child(&if_node, 2)
	else_node := t.a.nodes[int(else_id)]
	else_block := if else_node.kind == .if_expr {
		t.make_block(t.build_if_value_chain(else_id, target_name, target_type_id))
	} else {
		t.if_value_branch_block(else_id, target_name, target_type_id)
	}
	t.pending_stmts = outer_pending
	mut selected_block := then_block
	if t.is_optional_type_name(info.value_type) && opt_name.len > 0 {
		ok_cond := t.make_selector(t.make_ident(opt_name), 'ok', flat.source_type_bool)
		inner_if := t.make_if(ok_cond, then_block, else_block)
		selected_block = t.make_block([opt_decl, inner_if])
	}
	result << t.make_if(found_cond, selected_block, else_block)
	return result
}

// if_value_branch_block supports if value branch block handling for Transformer.
fn (mut t Transformer) if_value_branch_block(branch_id flat.NodeId, target_name string, target_type_id u32) flat.NodeId {
	heaped_state := t.save_heaped_local_state()
	defer {
		t.restore_heaped_local_state(heaped_state)
	}
	if int(branch_id) < 0 {
		return t.make_block([]flat.NodeId{})
	}
	branch := t.a.nodes[int(branch_id)]
	if branch.kind == .if_expr {
		return t.make_block(t.build_if_value_chain(branch_id, target_name, target_type_id))
	}
	if branch.kind != .block {
		mut result := []flat.NodeId{}
		if t.if_value_branch_tail_has_no_value(branch_id) {
			value := t.transform_expr(branch_id)
			t.drain_pending(mut result)
			result << t.make_expr_stmt(value)
			return t.make_block(result)
		}
		value := t.transform_if_branch_value(branch_id, target_type_id)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if branch.children_count == 0 {
		return t.make_block([]flat.NodeId{})
	}
	if target_type_id == flat.source_type_string {
		if nested_match := t.split_nested_match_string_interp_block_value(branch) {
			mut result := []flat.NodeId{}
			t.drain_pending(mut result)
			result << t.make_assign(t.make_ident(target_name), nested_match)
			return t.make_block(result)
		}
	}

	mut stmt_ids := []flat.NodeId{cap: int(branch.children_count)}
	for i in 0 .. branch.children_count {
		stmt_ids << t.a.child(&branch, i)
	}
	mut result := []flat.NodeId{}
	if stmt_ids.len > 1 {
		for stmt in t.transform_stmts(stmt_ids[..stmt_ids.len - 1]) {
			result << stmt
		}
	}

	tail_id := stmt_ids[stmt_ids.len - 1]
	tail := t.a.nodes[int(tail_id)]
	if tail.kind == .return_stmt {
		tail_stmts := t.transform_stmt(tail_id)
		t.drain_pending(mut result)
		for stmt in tail_stmts {
			result << stmt
		}
		return t.make_block(result)
	}
	if tail.kind == .expr_stmt && tail.children_count > 0 {
		inner_id := t.a.child(&tail, 0)
		inner := t.a.nodes[int(inner_id)]
		if inner.kind == .call && t.is_noreturn_call(inner_id) {
			tail_stmts := t.transform_stmt(tail_id)
			t.drain_pending(mut result)
			for stmt in tail_stmts {
				result << stmt
			}
			return t.make_block(result)
		}
		if t.node_type(inner_id) == flat.source_type_void {
			tail_stmts := t.transform_stmt(tail_id)
			t.drain_pending(mut result)
			for stmt in tail_stmts {
				result << stmt
			}
			return t.make_block(result)
		}
		if t.if_value_branch_tail_has_no_value(tail_id) {
			value := t.transform_expr(tail_id)
			t.drain_pending(mut result)
			result << t.make_expr_stmt(value)
			return t.make_block(result)
		}
		value := t.transform_if_branch_value(inner_id, target_type_id)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if tail.kind == .block && (t.stmt_value_type(tail_id)) != 0 {
		value := t.transform_if_branch_value(tail_id, target_type_id)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if tail.kind == .match_stmt {
		value := t.transform_if_branch_value(tail_id, target_type_id)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if tail.kind == .if_expr {
		value := t.transform_if_branch_value(tail_id, target_type_id)
		t.drain_pending(mut result)
		result << t.make_assign(t.make_ident(target_name), value)
		return t.make_block(result)
	}
	if t.is_stmt_kind(tail.kind) {
		tail_stmts := t.transform_stmt(tail_id)
		t.drain_pending(mut result)
		for stmt in tail_stmts {
			result << stmt
		}
		return t.make_block(result)
	}
	if t.if_value_branch_tail_has_no_value(tail_id) {
		value := t.transform_expr(tail_id)
		t.drain_pending(mut result)
		result << t.make_expr_stmt(value)
		return t.make_block(result)
	}
	value := t.transform_if_branch_value(tail_id, target_type_id)
	t.drain_pending(mut result)
	result << t.make_assign(t.make_ident(target_name), value)
	return t.make_block(result)
}

fn (mut t Transformer) split_nested_match_string_interp_block_value(branch flat.Node) ?flat.NodeId {
	if branch.kind != .block || branch.children_count != 3 {
		return none
	}
	first_id := t.single_expr_stmt_child(t.a.child(&branch, 0)) or { return none }
	then_id := t.single_expr_stmt_child(t.a.child(&branch, 1)) or { return none }
	tail_id := t.single_expr_stmt_child(t.a.child(&branch, 2)) or { return none }
	first := t.a.nodes[int(first_id)]
	tail := t.a.nodes[int(tail_id)]
	if first.kind != .string_literal || tail.kind != .string_interp || tail.children_count != 3 {
		return none
	}
	middle_id := t.a.child(&tail, 0)
	else_id := t.a.child(&tail, 1)
	last_id := t.a.child(&tail, 2)
	middle := t.a.nodes[int(middle_id)]
	last := t.a.nodes[int(last_id)]
	if middle.kind != .string_literal || last.kind != .string_literal {
		return none
	}
	subject_text, label_text := nested_match_interp_prefix(first.value) or { return none }
	if !nested_match_has_else_separator(middle.value) || !trimmed_transform_text(last.value).ends_with('}}') {
		return none
	}
	subject, _ := t.simple_nested_interp_expr(subject_text) or { return none }
	label, _ := t.simple_nested_interp_expr(label_text) or { return none }
	cond := t.make_infix(.eq, subject, label)
	t.set_node_typ(int(cond), flat.source_type_bool)
	return t.lower_nested_two_branch_string_expr(cond, then_id, else_id)
}

fn (t &Transformer) single_expr_stmt_child(id flat.NodeId) ?flat.NodeId {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .expr_stmt || node.children_count != 1 {
		return none
	}
	return t.a.child(&node, 0)
}

fn (t &Transformer) if_value_branch_tail_has_no_value(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call && t.is_noreturn_call(id) {
		return true
	}
	return t.node_type(id) == flat.source_type_void
}

// transform_if_branch_value transforms transform if branch value data for transform.
fn (mut t Transformer) transform_if_branch_value(id flat.NodeId, target_type_id u32) flat.NodeId {
	if t.is_sum_type_name(target_type_id) {
		return t.clone_borrowed_projection(id, t.wrap_sum_value_for_storage(id, target_type_id), target_type_id)
	}
	if converted := t.fixed_array_value_to_dynamic(id, target_type_id) {
		return converted
	}
	return t.clone_borrowed_storage_projection(id, t.transform_expr_for_type(id, target_type_id), target_type_id)
}

// transform_is_condition transforms an `x is Type` condition node into the
// common sum-type tag comparison used by all C emission paths.
fn (mut t Transformer) transform_is_condition(cond_id flat.NodeId) flat.NodeId {
	if int(cond_id) < 0 {
		return cond_id
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .is_expr {
		return cond_id
	}
	// is_expr: child[0] is the expression being checked, value is the type name.
	if cond.children_count < 1 {
		return cond_id
	}
	variant_name := cond.value
	if variant_name.len == 0 {
		return cond_id
	}
	return t.transform_is_expr(cond_id, cond)
}

// transform_and_chain_smartcasts handles conditions like
// `x is T && x.field > 0` where the second operand should see x
// narrowed to T through a smartcast.
//
// It walks a left-associative chain of .logical_and nodes. When a
// term is an is_expr it pushes a smartcast so subsequent terms
// (and the then-branch) see the narrowed type.
//
// Currently passes through unchanged but is structured to detect
// the pattern for future expansion.
fn (mut t Transformer) transform_and_chain_smartcasts(cond_id flat.NodeId) flat.NodeId {
	if int(cond_id) < 0 {
		return cond_id
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .decl_assign {
		return t.transform_if_guard_condition(cond)
	}
	if cond.kind != .infix || cond.op !in [.logical_and, .logical_or] {
		// Not an && chain -- preserve bare smartcast checks and fully transform
		// ordinary conditions.
		if cond.kind == .is_expr {
			return t.transform_is_condition(cond_id)
		}
		return t.transform_expr_for_type(cond_id, flat.source_type_bool)
	}
	if cond.children_count < 2 {
		return cond_id
	}
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	new_lhs := t.transform_and_chain_smartcasts(lhs_id)
	lhs_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	lhs_smartcasts := if cond.op == .logical_and {
		t.extract_all_is_exprs(lhs_id)
	} else {
		[]IsExprInfo{}
	}
	for info in lhs_smartcasts {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	mut new_rhs := t.transform_and_chain_smartcasts(rhs_id)
	rhs_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	for _ in lhs_smartcasts {
		t.pop_smartcast()
	}
	// The left operand always evaluates, so its pending statements may run before
	// the `&&`/`||`. The right operand is conditional, so any statements its
	// lowering produced (e.g. an `in [...]` / `or {}` temporary) must run only when
	// the right operand is actually reached — wrap them in a `({ ...; value; })`
	// statement-expression to preserve short-circuit evaluation.
	for stmt in lhs_pending {
		t.pending_stmts << stmt
	}
	if rhs_pending.len > 0 {
		mut block_stmts := rhs_pending.clone()
		block_stmts << t.make_expr_stmt(new_rhs)
		new_rhs = t.make_block(block_stmts)
		t.set_node_typ(int(new_rhs), flat.source_type_bool)
	}
	if lhs.kind == .is_expr && new_lhs == lhs_id && new_rhs == rhs_id {
		return cond_id
	}
	return t.make_infix(cond.op, new_lhs, new_rhs)
}

// transform_if_guard_condition transforms transform if guard condition data for transform.
fn (mut t Transformer) transform_if_guard_condition(node flat.Node) flat.NodeId {
	if node.kind != .decl_assign || node.children_count < 2 {
		return flat.empty_node
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 1 {
			new_children << t.transform_expr(child_id)
		} else {
			new_children << t.transform_lvalue(child_id)
		}
	}
	start := t.a.children.len
	for child in new_children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           .decl_assign
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

@[direct_array_access]
fn (mut t Transformer) transform_if_branch_as_block(branch_id flat.NodeId) flat.NodeId {
	if int(branch_id) < 0 {
		return t.make_block([]flat.NodeId{})
	}
	// Auto-heaped locals are lexical bindings. Restore the incoming state after each
	// branch so same-named locals in later branches cannot reuse stale pointer storage.
	heaped_state := t.save_heaped_local_state()
	defer {
		t.restore_heaped_local_state(heaped_state)
	}
	branch := t.a.nodes[int(branch_id)]
	if branch.kind == .block {
		children := t.transform_stmts(t.a.children_of(&branch))
		if t.rewrite_children_in_place(branch_id, children) {
			return branch_id
		}
		return t.make_block(children)
	}
	mut stmts := []flat.NodeId{}
	if t.is_stmt_kind_id(int(branch.kind)) {
		expanded := t.transform_stmt(branch_id)
		t.drain_pending(mut stmts)
		stmts << expanded
		return t.make_block(stmts)
	}
	expr := t.transform_expr(branch_id)
	t.drain_pending(mut stmts)
	if int(expr) >= 0 {
		stmts << t.make_expr_stmt(expr)
	}
	return t.make_block(stmts)
}

fn (mut t Transformer) transform_plain_if_branches(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .if_expr || node.children_count < 2 {
		return id
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	has_else := node.children_count >= 3
	else_id := if has_else { t.a.child(&node, 2) } else { flat.empty_node }

	new_cond_id := t.transform_expr_for_type(cond_id, flat.source_type_bool)
	cond_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut new_then_id := flat.empty_node
	mut new_else_id := flat.empty_node
	if t.smartcast_stack.len == 0 {
		new_then_id = t.transform_if_branch_as_block(then_id)
		if has_else {
			else_node := t.a.nodes[int(else_id)]
			new_else_id = if else_node.kind == .if_expr {
				t.transform_else_if_expr(else_id, else_node)
			} else {
				t.transform_if_branch_as_block(else_id)
			}
		}
	} else {
		base_smartcasts := t.smartcast_stack.clone()
		base_invalidated := t.invalidated_smartcasts.clone()
		new_then_id = t.transform_if_branch_as_block(then_id)
		mut then_invalidated := t.invalidated_smartcasts.clone()
		t.clear_node_type_memo()
		t.smartcast_stack = base_smartcasts.clone()
		t.clear_node_type_memo()
		t.invalidated_smartcasts = base_invalidated.clone()
		if has_else {
			else_node := t.a.nodes[int(else_id)]
			new_else_id = if else_node.kind == .if_expr {
				t.transform_else_if_expr(else_id, else_node)
			} else {
				t.transform_if_branch_as_block(else_id)
			}
		}
		else_invalidated := t.invalidated_smartcasts.clone()
		for key, value in else_invalidated {
			if value {
				then_invalidated[key] = true
			}
		}
		t.clear_node_type_memo()
		t.invalidated_smartcasts = then_invalidated.move()
		t.clear_node_type_memo()
		t.smartcast_stack = t.non_invalidated_smartcasts(base_smartcasts)
	}

	mut child_count := 2
	mut new_children := [new_cond_id, new_then_id]
	if has_else {
		new_children << new_else_id
		child_count = 3
	}
	new_if := if t.rewrite_children_in_place(id, new_children) {
		id
	} else {
		if_start := t.a.children.len
		t.a.children << new_children
		t.a.add_node(flat.Node{
			kind:           .if_expr
			children_start: if_start
			children_count: flat.child_count(child_count)
			typ:            node.typ
			pos:            node.pos
		})
	}
	for pending in cond_pending {
		t.pending_stmts << pending
	}
	return new_if
}

// transform_if_branches_with_smartcast is the main if-expr handler that
// integrates smartcasting. When the condition contains an is_expr, it
// pushes a smartcast for the then-branch, transforms the body under
// that context, pops the smartcast, then transforms the else-block.
@[direct_array_access]
fn (mut t Transformer) transform_if_branches_with_smartcast(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .if_expr || node.children_count < 2 {
		return id
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	has_else := node.children_count >= 3
	else_id := if has_else { t.a.child(&node, 2) } else { flat.empty_node }

	all_is := t.extract_all_is_exprs(cond_id)
	all_none_eq := t.extract_else_branch_smartcasts(cond_id)
	cond_node := t.a.nodes[int(cond_id)]
	if all_is.len == 0 && all_none_eq.len == 0 && cond_node.kind != .decl_assign {
		return t.transform_plain_if_branches(id, node)
	}
	new_cond_id := t.transform_and_chain_smartcasts(cond_id)
	cond_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	// Transform then-block children under the smartcast context.
	saved_var_types := t.var_types.clone()
	saved_heaped_state := t.save_heaped_local_state()
	then_base_smartcasts := t.smartcast_stack.clone()
	base_invalidated := t.invalidated_smartcasts.clone()
	for info in all_is {
		t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
	}
	new_then_id := t.transform_if_branch_as_block(then_id)
	mut then_invalidated := t.invalidated_smartcasts.clone()
	t.clear_node_type_memo()
	t.smartcast_stack = then_base_smartcasts.clone()
	t.clear_node_type_memo()
	t.invalidated_smartcasts = base_invalidated.clone()
	t.restore_var_types(saved_var_types)
	t.restore_heaped_local_state(saved_heaped_state)

	// Transform else-block. A `!is` condition narrows in the else branch, and
	// `x == none` unwraps x there.
	mut new_else_id := flat.empty_node
	if has_else {
		else_base_smartcasts := t.smartcast_stack.clone()
		for info in all_none_eq {
			t.push_smartcast(info.expr_name, info.variant_name, info.sum_type_name)
		}
		else_node := t.a.nodes[int(else_id)]
		if else_node.kind == .if_expr {
			// else-if chain: recurse.
			new_else_id = t.transform_else_if_expr(else_id, else_node)
		} else {
			new_else_id = t.transform_if_branch_as_block(else_id)
		}
		t.clear_node_type_memo()
		t.smartcast_stack = else_base_smartcasts
		t.restore_var_types(saved_var_types)
		t.restore_heaped_local_state(saved_heaped_state)
	}
	else_invalidated := t.invalidated_smartcasts.clone()
	for key, value in else_invalidated {
		if value {
			then_invalidated[key] = true
		}
	}
	t.clear_node_type_memo()
	t.invalidated_smartcasts = then_invalidated.move()
	t.clear_node_type_memo()
	t.smartcast_stack = t.non_invalidated_smartcasts(then_base_smartcasts)

	// Rebuild the if_expr with (possibly) new children.
	mut child_count := 2
	mut new_children := [new_cond_id, new_then_id]
	if has_else {
		new_children << new_else_id
		child_count = 3
	}
	new_if := if t.rewrite_children_in_place(id, new_children) {
		id
	} else {
		if_start := t.a.children.len
		t.a.children << new_children
		t.a.add_node(flat.Node{
			kind:           .if_expr
			children_start: if_start
			children_count: flat.child_count(child_count)
			typ:            node.typ
			pos:            node.pos
		})
	}
	for pending in cond_pending {
		t.pending_stmts << pending
	}
	return new_if
}

fn (t &Transformer) post_if_exit_smartcasts(id flat.NodeId) []IsExprInfo {
	if int(id) < 0 {
		return []IsExprInfo{}
	}
	node := t.a.nodes[int(id)]
	if node.kind != .if_expr || node.children_count < 2 {
		return []IsExprInfo{}
	}
	cond_id := t.a.child(&node, 0)
	then_id := t.a.child(&node, 1)
	if info := t.negated_is_expr_info(cond_id) {
		if t.stmt_definitely_exits(then_id) {
			return [info]
		}
	}
	if t.stmt_definitely_exits(then_id) {
		none_eq := t.extract_none_eq_exprs(cond_id)
		if none_eq.len > 0 {
			return none_eq
		}
	}
	if node.children_count >= 3 {
		else_id := t.a.child(&node, 2)
		if t.stmt_definitely_exits(else_id) {
			return t.extract_all_is_exprs(cond_id)
		}
	}
	return []IsExprInfo{}
}

fn (t &Transformer) post_assert_smartcasts(id flat.NodeId) []IsExprInfo {
	if int(id) < 0 {
		return []IsExprInfo{}
	}
	node := t.a.nodes[int(id)]
	if node.kind != .assert_stmt || node.children_count == 0 {
		return []IsExprInfo{}
	}
	return t.extract_all_is_exprs(t.a.child(&node, 0))
}

fn (t &Transformer) negated_is_expr_info(cond_id flat.NodeId) ?IsExprInfo {
	if int(cond_id) < 0 {
		return none
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind != .prefix || cond.op != .not || cond.children_count == 0 {
		return none
	}
	inner_id := t.a.child(&cond, 0)
	if int(inner_id) < 0 {
		return none
	}
	inner := t.a.nodes[int(inner_id)]
	if inner.kind != .is_expr {
		return none
	}
	info := t.extract_is_expr(inner_id)
	if info.expr_name.len == 0 || info.sum_type_name_text().len == 0 || info.variant_name == 0 {
		return none
	}
	return info
}

fn (t &Transformer) stmt_definitely_exits(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return true
		}
		.expr_stmt {
			// `panic(...)`/`exit(...)` end the branch like the checker's
			// `stmt_definitely_returns`, which already narrowed after them.
			return node.children_count > 0 && t.stmt_definitely_exits(t.a.child(&node, 0))
		}
		.call {
			return !isnil(t.tc) && t.is_noreturn_call(id)
		}
		.assert_stmt {
			if node.children_count == 0 {
				return false
			}
			cond := t.a.nodes[int(t.a.child(&node, 0))]
			return cond.kind == .bool_literal && cond.value == 'false'
		}
		.block {
			for i in 0 .. node.children_count {
				if t.stmt_definitely_exits(t.a.child(&node, i)) {
					return true
				}
			}
			return false
		}
		.if_expr {
			if node.children_count < 3 {
				return false
			}
			return t.stmt_definitely_exits(t.a.child(&node, 1))
				&& t.stmt_definitely_exits(t.a.child(&node, 2))
		}
		.match_stmt {
			if node.children_count < 2 {
				return false
			}
			mut has_else := false
			for i in 1 .. node.children_count {
				branch := t.a.child_node(&node, i)
				if branch.kind != .match_branch {
					return false
				}
				if branch.value == 'else' {
					has_else = true
				}
				body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
				mut branch_exits := false
				for j in body_start .. branch.children_count {
					if t.stmt_definitely_exits(t.a.child(branch, j)) {
						branch_exits = true
						break
					}
				}
				if !branch_exits {
					return false
				}
			}
			return has_else
		}
		else {
			return false
		}
	}
}

// --- helpers ---

// IsExprInfo stores is expr info metadata used by transform.
struct IsExprInfo {
	type_texts    &flat.TypeTextTable = unsafe { nil }
	expr_name     string
	variant_name  u32
	sum_type_name u32
}

@[inline]
fn (info &IsExprInfo) variant_name_text() string {
	if info.variant_name == 0 { return '' }
	return info.type_texts.text(info.variant_name)
}

@[inline]
fn (info &IsExprInfo) sum_type_name_text() string {
	if info.sum_type_name == 0 { return '' }
	return info.type_texts.text(info.sum_type_name)
}

// extract_all_is_exprs supports extract all is exprs handling for Transformer.
fn (t &Transformer) extract_all_is_exprs(cond_id flat.NodeId) []IsExprInfo {
	mut result := []IsExprInfo{}
	t.collect_is_exprs(cond_id, mut result)
	return result
}

// collect_is_exprs updates collect is exprs state for transform.
fn (t &Transformer) collect_is_exprs(cond_id flat.NodeId, mut result []IsExprInfo) {
	if int(cond_id) < 0 {
		return
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .paren && cond.children_count > 0 {
		t.collect_is_exprs(t.a.child(&cond, 0), mut result)
		return
	}
	if cond.kind == .is_expr && cond.children_count >= 1 {
		expr_id := t.a.child(&cond, 0)
		ek := t.expr_key(expr_id)
		if ek.len > 0 && cond.value.len > 0 {
			expr_type_source_id := t.original_expr_type(expr_id)
			expr_type := flat.source_type_name(expr_type_source_id)
			stn_source_id := t.sum_type_for_is_expr(expr_type_source_id, flat.source_type_id(cond.value))
			stn := flat.source_type_name(stn_source_id)
			if stn_source_id != 0 {
				result << IsExprInfo{
					type_texts:    t.var_type_texts
					expr_name:     ek
					variant_name:  t.var_type_text_id(cond.value)
					sum_type_name: t.var_type_text_id(stn)
				}
			} else {
				if t.is_interface_type_name(expr_type_source_id) {
					result << IsExprInfo{
						type_texts:    t.var_type_texts
						expr_name:     ek
						variant_name:  t.var_type_text_id(cond.value)
						sum_type_name: t.var_type_text_id(expr_type)
					}
				}
			}
		}
	}
	// `x != none` (or `x != nil` for `?&T`) unwraps the option expr inside the
	// then-branch: record an option-unwrap context resolving x to its base type.
	if cond.kind == .infix && cond.op == .ne && cond.children_count >= 2 {
		if info := t.option_none_cmp_info(cond) {
			result << info
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		t.collect_is_exprs(t.a.child(&cond, 0), mut result)
		t.collect_is_exprs(t.a.child(&cond, 1), mut result)
	}
}

// option_none_cmp_info matches a `x != none` / `x == none` (also `nil`)
// comparison where x is an option and returns the option-unwrap context.
fn (t &Transformer) option_none_cmp_info(cond flat.Node) ?IsExprInfo {
	lhs_id := t.a.child(&cond, 0)
	rhs_id := t.a.child(&cond, 1)
	lhs := t.a.nodes[int(lhs_id)]
	rhs := t.a.nodes[int(rhs_id)]
	mut opt_id := flat.empty_node
	if rhs.kind == .none_expr || rhs.kind == .nil_literal {
		opt_id = lhs_id
	} else if lhs.kind == .none_expr || lhs.kind == .nil_literal {
		opt_id = rhs_id
	}
	if int(opt_id) < 0 {
		return none
	}
	ek := t.expr_key(opt_id)
	if ek.len == 0 {
		return none
	}
	mut expr_type := t.original_expr_type(opt_id)
	if !t.is_optional_type_name(expr_type) {
		if sc := t.find_smartcast(ek) {
			expr_type = t.smartcast_target_type(sc)
		}
	}
	if !t.is_optional_type_name(expr_type) {
		opt_node := t.a.node(opt_id)
		if opt_node.kind == .ident {
			expr_type = t.raw_var_type(opt_node.value)
		}
	}
	if !t.is_optional_type_name(expr_type) {
		return none
	}
	base_source_id_2 := t.optional_base_type(expr_type)
	base := flat.source_type_name(base_source_id_2)
	if base_source_id_2 == 0 || base_source_id_2 == flat.source_type_void {
		return none
	}
	return IsExprInfo{
		type_texts:    t.var_type_texts
		expr_name:     ek
		variant_name:  t.var_type_text_id(base)
		sum_type_name: t.var_type_text_id(option_unwrap_marker)
	}
}

// extract_else_branch_smartcasts returns smartcast contexts that apply to the
// ELSE branch of an if: `if x !is T { ... } else { <x as T> }`, or
// `if x == none { ... } else { <x unwrapped here> }`.
fn (t &Transformer) extract_else_branch_smartcasts(cond_id flat.NodeId) []IsExprInfo {
	if int(cond_id) < 0 {
		return []IsExprInfo{}
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .paren && cond.children_count > 0 {
		return t.extract_else_branch_smartcasts(t.a.child(&cond, 0))
	}
	if cond.kind == .prefix && cond.op == .not && cond.children_count > 0 {
		inner_id := t.a.child(&cond, 0)
		inner := t.a.nodes[int(inner_id)]
		if inner.kind == .is_expr && inner.children_count > 0 {
			expr_id := t.a.child(&inner, 0)
			ek := t.expr_key(expr_id)
			if ek.len > 0 && inner.value.len > 0 {
				expr_type_source_id := t.original_expr_type(expr_id)

				stn_source_id_2 := t.sum_type_for_is_expr(expr_type_source_id, flat.source_type_id(inner.value))
				stn := flat.source_type_name(stn_source_id_2)
				if stn_source_id_2 != 0 {
					return [
						IsExprInfo{
							type_texts:    t.var_type_texts
							expr_name:     ek
							variant_name:  t.var_type_text_id(inner.value)
							sum_type_name: t.var_type_text_id(stn)
						},
					]
				}
			}
		}
	}
	if cond.kind == .infix && cond.op == .eq && cond.children_count >= 2 {
		if info := t.option_none_cmp_info(cond) {
			return [info]
		}
	}
	return []IsExprInfo{}
}

// extract_none_eq_exprs returns option-unwrap contexts that apply to the ELSE
// branch of an if: `if x == none { ... } else { <x unwrapped here> }`.
fn (t &Transformer) extract_none_eq_exprs(cond_id flat.NodeId) []IsExprInfo {
	if int(cond_id) < 0 {
		return []IsExprInfo{}
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .paren && cond.children_count > 0 {
		return t.extract_none_eq_exprs(t.a.child(&cond, 0))
	}
	if cond.kind == .infix && cond.op == .eq && cond.children_count >= 2 {
		if info := t.option_none_cmp_info(cond) {
			return [info]
		}
	}
	return []IsExprInfo{}
}

// extract_is_expr supports extract is expr handling for Transformer.
fn (t &Transformer) extract_is_expr(cond_id flat.NodeId) IsExprInfo {
	if int(cond_id) < 0 {
		return IsExprInfo{}
	}
	cond := t.a.nodes[int(cond_id)]
	if cond.kind == .is_expr && cond.children_count >= 1 {
		expr_id := t.a.child(&cond, 0)
		ek := t.expr_key(expr_id)
		if ek.len > 0 && cond.value.len > 0 {
			expr_type_source_id := t.original_expr_type(expr_id)

			return IsExprInfo{
				type_texts:    t.var_type_texts
				expr_name:     ek
				variant_name:  t.var_type_text_id(cond.value)
				sum_type_name: t.sum_type_for_is_expr(expr_type_source_id, flat.source_type_id(cond.value))
			}
		}
	}
	if cond.kind == .infix && cond.op == .logical_and && cond.children_count >= 2 {
		// Check left side first (is_expr is typically on the left of &&).
		lhs_id := t.a.child(&cond, 0)
		info := t.extract_is_expr(lhs_id)
		if info.expr_name.len > 0 {
			return info
		}
		// Check right side as fallback.
		rhs_id := t.a.child(&cond, 1)
		return t.extract_is_expr(rhs_id)
	}
	return IsExprInfo{}
}

// sum_type_for_is_expr supports sum type for is expr handling for Transformer.
fn (t &Transformer) sum_type_for_is_expr(expr_type_id u32, variant_id u32) u32 {
	clean_expr_type_source_id := t.trim_all_pointer_type(expr_type_id)

	clean_variant_source_id := t.trim_pointer_type(variant_id)
	clean_variant_source_id_2 := clean_variant_source_id

	if t.is_interface_type_name(clean_expr_type_source_id) {
		if _ := t.resolve_interface_pattern(clean_variant_source_id, clean_expr_type_source_id) {
			return clean_expr_type_source_id
		}
		if _ := t.resolve_interface_pattern_interface(clean_variant_source_id) {
			return clean_expr_type_source_id
		}
	}
	if _ := t.resolve_sum_variant_pattern_for_subject(clean_expr_type_source_id, clean_variant_source_id) {
		return clean_expr_type_source_id
	}
	// Preserve the fully scoped concrete application selected for the pattern.
	// A declaration in `client` can spell `maybe.Maybe[Local]`, while its union
	// field is the qualified `client.Local` (or a selectively imported type).
	for candidate_source_id in t.sum_subject_type_candidates(clean_expr_type_source_id) {
		candidate_source_id_2 := candidate_source_id

		if _ := t.sum_variant_name(candidate_source_id, clean_variant_source_id) {
			return candidate_source_id
		}
		if !isnil(t.tc) {
			if _ := t.tc.sum_variant_type_for_pattern(candidate_source_id_2, clean_variant_source_id_2) {
				return candidate_source_id
			}
		}
	}
	resolved_expr_sum_source_id := t.resolve_sum_name(clean_expr_type_source_id)
	resolved_expr_sum := flat.source_type_name(resolved_expr_sum_source_id)
	if t.sum_types.contains(resolved_expr_sum) {
		if _ := t.sum_variant_name(resolved_expr_sum_source_id, clean_variant_source_id) {
			return clean_expr_type_source_id
		}
		// `x is V` tests a variant of the sum type of `x`, also one that is
		// only found with its `&`, `&int` of `Maybe[&int]`: never a variant of
		// another sum type with the same name, `int` of `strconv.Numeric`.
		return clean_expr_type_source_id
	}
	return t.find_sum_type_for_variant(clean_variant_source_id)
}

// find_sum_type_for_variant returns the sum type name that contains
// the given variant, or '' if none is found.
fn (t &Transformer) find_sum_type_for_variant(variant_id u32) u32 {
	variant := flat.source_type_name(variant_id)
	mut best := ''
	if variant.contains('.') {
		for sum_name, variants_id in t.sum_types.ids {
			variants := variants_id.map(flat.source_type_name(it))
			for v in variants {
				if v == variant {
					if sum_name.contains('.') {
						return flat.source_type_id(sum_name)
					}
					if best == '' {
						best = sum_name
					}
				}
			}
		}
		if best.len > 0 {
			return flat.source_type_id(best)
		}
	}
	for sum_name, variants_id in t.sum_types.ids {
		variants := variants_id.map(flat.source_type_name(it))
		for v in variants {
			if t.variant_names_match(flat.source_type_id(v), variant_id) {
				if sum_name.contains('.') {
					return flat.source_type_id(sum_name)
				}
				if best.len == 0 {
					best = sum_name
				}
			}
		}
	}
	return flat.source_type_id(best)
}

// ret_typ_text resolves the indexed source type spelling.
@[inline]
fn (value &GuardReturnTail) ret_typ_text() string {
	return flat.source_type_name(value.ret_typ)
}
