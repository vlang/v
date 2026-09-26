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
	// and escapes conservatively keep the existing alias diagnostic.
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
			.fn_decl, .fn_literal, .lambda_expr, .comptime_for, .goto_stmt {
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
					if tc.a.node(arg_id).is_mut && tc.expr_key(arg_id) == node.value {
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
