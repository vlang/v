module types

import v.flat

// A map clone owns its entry storage even when the copied values still contain
// borrowed arrays, maps, or pointers. This proof is only for replacing a whole
// entry; it must not clear immutable_reference_aliases for nested writes.
fn (mut tc TypeChecker) mutable_alias_has_fresh_map_storage(id flat.NodeId) bool {
	if !tc.valid_node_id(id) || unalias_type(tc.resolve_type(id)) !is Map {
		return false
	}
	node := tc.a.node(id)
	if node.kind == .call {
		return tc.is_builtin_map_clone_call(id)
	}
	if node.kind != .ident {
		return false
	}
	rhs_id := tc.local_decl_rhs_before(node.value, id) or { return false }
	if !tc.is_builtin_map_clone_call(rhs_id) {
		return false
	}
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) {
		return false
	}
	// Inspect the whole function, not just statements preceding this use: a
	// later rebind can reach it on the next loop iteration. Ambiguous bindings
	// and escapes conservatively keep the existing alias diagnostic. Closure
	// bodies are scanned like the rest of the function: capturing the clone
	// (`fn [mut copy] () { ... }`) shares only its fresh storage, and a rebind or
	// `mut` escape inside the closure is caught by the same checks.
	fn_node := tc.a.node(fn_id)
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		stack << tc.a.child(fn_node, i)
	}
	mut declarations := 0
	for stack.len > 0 {
		current_id := stack.pop()
		current := tc.a.node(current_id)
		match current.kind {
			.fn_decl, .lambda_expr, .comptime_for, .goto_stmt {
				return false
			}
			.param {
				if current.value == node.value {
					return false
				}
			}
			.decl_assign {
				for i in 0 .. current.children_count {
					child := tc.a.child_node(current, i)
					if child.kind != .ident || child.value != node.value {
						continue
					}
					if current.children_count != 2 || i != 0
						|| tc.a.child(current, 1) != rhs_id {
						return false
					}
					declarations++
				}
			}
			.for_in_stmt {
				for i in 0 .. int_min(2, current.children_count) {
					binding := tc.a.child_node(current, i)
					if binding.kind == .ident && binding.value == node.value {
						return false
					}
				}
			}
			.assign {
				for lhs_id in tc.multi_assign_lhs_ids(*current) {
					if tc.expr_key(lhs_id) == node.value {
						return false
					}
				}
			}
			.prefix {
				if current.op == .amp && current.children_count > 0
					&& tc.expr_key(tc.a.child(current, 0)) == node.value {
					return false
				}
			}
			.selector {
				// Also reject bound method values and generic receiver calls,
				// whose immediate callee need not itself be a selector.
				if current.children_count > 0 && current.value != 'len'
					&& tc.expr_key(tc.a.child(current, 0)) == node.value {
					return false
				}
			}
			.call {
				if current.children_count > 0 {
					callee := tc.a.child_node(current, 0)
					if callee.kind == .selector && callee.children_count > 0
						&& tc.expr_key(tc.a.child(callee, 0)) == node.value {
						return false
					}
				}
				for i in 1 .. current.children_count {
					arg_id := tc.call_arg_value(tc.a.child(current, i))
					if tc.a.node(arg_id).is_mut && tc.expr_key(arg_id) == node.value
						&& tc.call_may_rebind_mut_map_arg(current_id, i) {
						return false
					}
				}
			}
			else {}
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return declarations == 1
}

// call_may_rebind_mut_map_arg reports whether the function called by `call_id` may
// rebind the map passed as `mut` in its argument child `arg_child`, rather than only
// read or replace its entries. A function that cannot be inspected may.
fn (mut tc TypeChecker) call_may_rebind_mut_map_arg(call_id flat.NodeId, arg_child int) bool {
	call := tc.a.node(call_id)
	if call.children_count == 0 || tc.a.child_node(call, 0).kind != .ident {
		return true
	}
	info := tc.resolve_call_info(call_id, *call) or { return true }
	if info.name.len == 0 || info.name.starts_with('C.') || info.has_receiver {
		return true
	}
	decl := tc.visible_mutation_fn_decl(info.name, tc.fn_type_modules[info.name] or {
		tc.cur_module
	}) or { return true }
	fn_node := tc.a.node(flat.NodeId(decl.idx))
	fn_short_name := fn_node.value.all_after_last('.')
	mut param_name := ''
	mut param_index := 0
	mut body := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(fn_node, i)
		child := tc.a.node(child_id)
		if child.kind != .param {
			body << child_id
			continue
		}
		param_index++
		if param_index == arg_child {
			param_name = child.value
		}
	}
	if param_name.len == 0 {
		return true
	}
	mut stack := body.clone()
	for stack.len > 0 {
		current_id := stack.pop()
		current := tc.a.node(current_id)
		match current.kind {
			.fn_decl, .lambda_expr, .comptime_for, .goto_stmt {
				return true
			}
			.fn_literal {
				// It can only reach the map through a captured copy.
				continue
			}
			.assign {
				for lhs_id in tc.multi_assign_lhs_ids(*current) {
					if tc.expr_key(lhs_id) == param_name {
						return true
					}
				}
			}
			.prefix {
				if current.op == .amp && current.children_count > 0
					&& tc.expr_key(tc.a.child(current, 0)) == param_name {
					return true
				}
			}
			.selector {
				if current.children_count > 0 && current.value != 'len'
					&& tc.expr_key(tc.a.child(current, 0)) == param_name {
					return true
				}
			}
			.call {
				// Passing it on as `mut` is only followed into a direct recursive call
				// in the same position, which this scan already covers.
				for i in 1 .. current.children_count {
					arg_id := tc.call_arg_value(tc.a.child(current, i))
					if tc.a.node(arg_id).is_mut && tc.expr_key(arg_id) == param_name {
						callee := tc.a.child_node(current, 0)
						if i != arg_child || callee.kind != .ident || callee.value != fn_short_name {
							return true
						}
					}
				}
			}
			else {}
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return false
}

fn (mut tc TypeChecker) is_builtin_map_clone_call(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	node := tc.a.node(id)
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	info := tc.resolve_call_info(id, *node) or { return false }
	decl_module := tc.fn_type_modules[info.name] or { tc.cur_module }
	if info.name == 'map.clone' && fresh_collection_builtin(info.name, decl_module) {
		return true
	}
	if info.name.len == 0 && info.has_receiver {
		return (tc.receiver_builtin_name(id) or { '' }) == 'map.clone'
	}
	return false
}

// FreshMapField is where `alias.field = m.clone()` was checked.
struct FreshMapField {
	owner      ScopeBindingOwner
	assign_id  flat.NodeId = flat.empty_node
	loop_depth int
}

// track_fresh_map_field_assignment records `alias.field = m.clone()`: until its block
// ends or `alias` (or the field) is assigned again, entry writes to `alias.field`
// reach only the fresh map, not the immutable value `alias` was copied from.
fn (mut tc TypeChecker) track_fresh_map_field_assignment(op flat.Op, lhs_id flat.NodeId, rhs_id flat.NodeId) {
	key := tc.expr_key(lhs_id)
	if key.len == 0 {
		return
	}
	if tc.fn_context.fresh_map_fields.len > 0 {
		prefix := key + '.'
		for tracked in tc.fn_context.fresh_map_fields.keys() {
			if tracked == key || tracked.starts_with(prefix) {
				tc.fn_context.fresh_map_fields.delete(tracked)
			}
		}
	}
	if op != .assign || tc.cur_scope == unsafe { nil } || tc.fn_context.has_goto_nodes
		|| tc.a.node(lhs_id).kind != .selector || unalias_type(tc.resolve_type(lhs_id)) !is Map
		|| !tc.is_builtin_map_clone_call(rhs_id) {
		return
	}
	tc.fn_context.fresh_map_fields[key] = FreshMapField{
		owner:      ScopeBindingOwner{
			scope:    tc.cur_scope
			lifetime: tc.cur_scope.lifetime
		}
		assign_id:  rhs_id
		loop_depth: tc.fn_context.pointer_alias_break_states.len
	}
}

// map_field_has_fresh_storage reports whether the map field `id` was assigned a fresh
// clone earlier in this block, or in an enclosing block of the same loop iteration
// (see track_fresh_map_field_assignment), and nothing in between could rebind it.
fn (mut tc TypeChecker) map_field_has_fresh_storage(id flat.NodeId) bool {
	if tc.fn_context.fresh_map_fields.len == 0 || !tc.valid_node_id(id)
		|| tc.a.node(id).kind != .selector {
		return false
	}
	key := tc.expr_key(id)
	fresh := tc.fn_context.fresh_map_fields[key] or { return false }
	// Loop bodies push a break state: a clone from before the loop does not
	// cover the next iteration, which may follow a rebind later in the body.
	if fresh.loop_depth != tc.fn_context.pointer_alias_break_states.len {
		return false
	}
	mut scope := tc.cur_scope
	mut in_scope := false
	for scope != unsafe { nil } {
		if fresh.owner.belongs_to_scope(scope) {
			in_scope = true
			break
		}
		scope = scope.parent
	}
	return in_scope && !tc.map_field_may_be_rebound_between(key, fresh.assign_id, id)
}

// map_field_may_be_rebound_between reports whether code between the clone `from_id`
// and the write `to_id` takes the address of the field (or of a value holding it),
// passes it as `mut`, calls a method on it, or defines a closure.
fn (tc &TypeChecker) map_field_may_be_rebound_between(key string, from_id flat.NodeId, to_id flat.NodeId) bool {
	fn_id := flat.NodeId(tc.fn_context.node_id)
	if !tc.valid_node_id(fn_id) || !tc.valid_node_id(from_id) {
		return true
	}
	from_pos := tc.a.node(from_id).pos
	to_pos := tc.a.node(to_id).pos
	if from_pos.id != to_pos.id || from_pos.offset >= to_pos.offset {
		return true
	}
	fn_node := tc.a.node(fn_id)
	mut stack := []flat.NodeId{}
	for i in 0 .. fn_node.children_count {
		stack << tc.a.child(fn_node, i)
	}
	for stack.len > 0 {
		current_id := stack.pop()
		current := tc.a.node(current_id)
		if current.pos.id == from_pos.id && current.pos.offset > from_pos.offset
			&& current.pos.offset < to_pos.offset {
			match current.kind {
				.fn_literal, .lambda_expr {
					return true
				}
				.prefix {
					if current.op == .amp && current.children_count > 0
						&& expr_key_overlaps(tc.expr_key(tc.a.child(current, 0)), key) {
						return true
					}
				}
				.call {
					if current.children_count > 0 {
						callee := tc.a.child_node(current, 0)
						if callee.kind == .selector && callee.children_count > 0
							&& expr_key_overlaps(tc.expr_key(tc.a.child(callee, 0)), key) {
							return true
						}
					}
					for i in 1 .. current.children_count {
						arg_id := tc.call_arg_value(tc.a.child(current, i))
						if tc.a.node(arg_id).is_mut && expr_key_overlaps(tc.expr_key(arg_id), key) {
							return true
						}
					}
				}
				else {}
			}
		}
		for i in 0 .. current.children_count {
			stack << tc.a.child(current, i)
		}
	}
	return false
}

// expr_key_overlaps reports whether the lvalue `candidate` is `key`, holds it, or is
// held by it.
fn expr_key_overlaps(candidate string, key string) bool {
	return candidate.len > 0 && (candidate == key || key.starts_with(candidate + '.')
		|| candidate.starts_with(key + '.'))
}
