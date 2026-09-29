module types

import v.flat

// SharedMapAutolock is the lock that a single operation on a `shared` map takes by itself.
//
// Outside of `lock`/`rlock` blocks, a `shared` map can be used like a thread-safe map:
// statements that change it (`m[k] = v`, `m[k] += v`, `m[k]++`, `m[k] << v`,
// `m.delete(k)`, `m.clear()`) lock it, and reads (`m[k]`, `m[k] or {...}`, `k in m`,
// `m.len`, `m.keys()`, `m.values()`, `m.clone()`) read-lock it, for the duration of that
// one operation. The checker checks such an operation as if it were written inside the
// `lock m {...}`/`rlock m {...}` block, and the transformer wraps it in one (see
// vlib/v/transform/shared_map.v, which must accept the same forms).
struct SharedMapAutolock {
	key  string
	mode u8 // `r` or `w`
}

// shared_map_autolock_possible reports whether any `shared` storage is visible, so that
// code without it skips the per-node detection below.
@[inline]
fn (tc &TypeChecker) shared_map_autolock_possible() bool {
	return tc.fn_context.shared_owners.len > 0 || tc.shared_global_names.len > 0
		|| tc.struct_shared_fields.len > 0
}

// shared_map_autolock returns the automatic lock that `node` takes on a `shared` map.
fn (tc &TypeChecker) shared_map_autolock(id flat.NodeId, node flat.Node) ?SharedMapAutolock {
	if tc.lock_depth > 0 {
		return none
	}
	return tc.shared_map_autolock_form(id, node)
}

// check_nested_shared_map_autolock reports an operation that would lock another `shared`
// map while one is locked automatically, since nested locks can deadlock.
fn (mut tc TypeChecker) check_nested_shared_map_autolock(id flat.NodeId, node flat.Node) {
	inner := tc.shared_map_autolock_form(id, node) or { return }
	outer := tc.autolocked_map
	if inner.key != outer {
		tc.record_error_at(.assignment_mismatch, '`${inner.key}` is `shared` and cannot be locked automatically while `${outer}` is, use `lock ${outer}, ${inner.key} {...}` or a separate statement', id, node.pos)
	}
}

fn (tc &TypeChecker) shared_map_autolock_form(id flat.NodeId, node flat.Node) ?SharedMapAutolock {
	match node.kind {
		.assign, .selector_assign, .index_assign {
			if node.children_count != 2 {
				return none
			}
			return tc.shared_map_index_autolock(tc.a.child(&node, 0), `w`)
		}
		.postfix {
			if node.op !in [.inc, .dec] || node.children_count != 1 {
				return none
			}
			autolock := tc.shared_map_index_autolock(tc.a.child(&node, 0), `w`)?
			return if tc.expr_is_standalone_statement(id) { autolock } else { none }
		}
		.infix {
			if node.op != .left_shift || node.children_count < 2 {
				return none
			}
			autolock := tc.shared_map_index_autolock(tc.a.child(&node, 0), `w`)?
			return if tc.expr_is_standalone_statement(id) { autolock } else { none }
		}
		.call {
			if node.children_count == 0 {
				return none
			}
			callee := tc.a.child_node(&node, 0)
			if callee.kind != .selector || callee.children_count == 0 {
				return none
			}
			if callee.value in ['delete', 'clear'] {
				autolock := tc.shared_map_base_autolock(tc.a.child(callee, 0), `w`)?
				return if tc.expr_is_standalone_statement(id) { autolock } else { none }
			}
			if callee.value in ['keys', 'values', 'clone'] && node.children_count == 1 {
				return tc.shared_map_base_autolock(tc.a.child(callee, 0), `r`)
			}
		}
		.index {
			if node.is_mut {
				return none
			}
			autolock := tc.shared_map_index_autolock(id, `r`)?
			return if tc.shared_map_index_is_read(id) { autolock } else { none }
		}
		.or_expr {
			if node.children_count == 0 {
				return none
			}
			index_id := tc.a.child(&node, 0)
			if tc.a.node(index_id).is_mut {
				return none
			}
			return tc.shared_map_index_autolock(index_id, `r`)
		}
		.in_expr {
			if node.children_count != 2 {
				return none
			}
			return tc.shared_map_base_autolock(tc.a.child(&node, 1), `r`)
		}
		.selector {
			if node.value != 'len' || node.children_count == 0 {
				return none
			}
			return tc.shared_map_base_autolock(tc.a.child(&node, 0), `r`)
		}
		else {}
	}
	return none
}

// shared_map_index_autolock returns the lock for `m[k]` or `m[k1][k2]...` on a `shared` map `m`.
fn (tc &TypeChecker) shared_map_index_autolock(id flat.NodeId, mode u8) ?SharedMapAutolock {
	if !tc.valid_node_id(id) {
		return none
	}
	mut node := tc.a.node(id)
	for node.kind == .index && node.children_count > 1 && node.op != .gated_index {
		base_id := tc.a.child(node, 0)
		base := tc.a.node(base_id)
		if base.kind != .index {
			return tc.shared_map_base_autolock(base_id, mode)
		}
		node = base
	}
	return none
}

// shared_map_base_autolock returns the lock for an operation on the `shared` map `id`: a
// variable, or a `shared` field reached through a chain of fields of a variable, so that
// locking it does not evaluate anything.
fn (tc &TypeChecker) shared_map_base_autolock(id flat.NodeId, mode u8) ?SharedMapAutolock {
	if !tc.valid_node_id(id) {
		return none
	}
	node := tc.a.node(id)
	mut key := ''
	if node.kind == .ident {
		if tc.fn_context.shared_owners.len == 0 && tc.shared_global_names.len == 0 {
			return none
		}
		if !tc.current_binding_is_shared_map(node.value) {
			return none
		}
		key = node.value
	} else if node.kind == .selector {
		if tc.struct_shared_fields.len == 0 || !tc.is_field_chain_of_ident(id)
			|| !tc.selector_is_shared_arg(node) {
			return none
		}
		field_type := tc.cached_expr_type(id) or { tc.resolve_type(id) }
		if unalias_and_unwrap_pointer_type(field_type) !is Map {
			return none
		}
		key = tc.shared_lock_key(id)
	} else {
		return none
	}
	if key.len == 0 || tc.current_shared_lock_mode(key) != 0 {
		return none
	}
	return SharedMapAutolock{
		key:  key
		mode: mode
	}
}

fn (tc &TypeChecker) is_field_chain_of_ident(id flat.NodeId) bool {
	mut node := tc.a.node(id)
	for node.kind == .selector && node.children_count > 0 {
		node = tc.a.child_node(node, 0)
	}
	return node.kind == .ident
}

// shared_map_index_is_read reports whether the outermost index `id` of a `shared` map
// reads the value, rather than being part of an assignment target, a `mut` argument,
// an `&` or an `if v := m[k]` guard (which are not locked automatically).
fn (tc &TypeChecker) shared_map_index_is_read(id flat.NodeId) bool {
	mut current := id
	for _ in 0 .. 64 {
		parent_id := tc.direct_parent_id(current)
		if !tc.valid_node_id(parent_id) || parent_id == current {
			return true
		}
		parent := tc.a.node(parent_id)
		is_first_child := parent.children_count > 0 && tc.a.child(parent, 0) == current
		match parent.kind {
			.index {
				if !is_first_child {
					return true
				}
				if current == id {
					// `m[k1][k2]` is locked as a whole
					return false
				}
				current = parent_id
			}
			.selector, .paren {
				current = parent_id
			}
			.assign, .selector_assign, .index_assign {
				for i := 0; i < parent.children_count; i += 2 {
					if tc.a.child(parent, i) == current {
						return false
					}
				}
				return true
			}
			.decl_assign {
				grandparent_id := tc.direct_parent_id(parent_id)
				if tc.valid_node_id(grandparent_id) {
					grandparent := tc.a.node(grandparent_id)
					if grandparent.kind == .if_expr && grandparent.children_count > 0
						&& tc.a.child(grandparent, 0) == parent_id {
						return false
					}
				}
				return true
			}
			.or_expr {
				return !(is_first_child && current == id)
			}
			.prefix {
				return parent.op != .amp
			}
			.postfix {
				return false
			}
			.infix {
				return !(is_first_child && parent.op == .left_shift
					&& tc.expr_is_standalone_statement(parent_id))
			}
			.for_in_stmt {
				// `for mut v in m[k] {` changes the elements
				for i in 0 .. 2 {
					if i >= parent.children_count {
						break
					}
					child_id := tc.a.child(parent, i)
					if tc.valid_node_id(child_id) && tc.a.node(child_id).is_mut {
						return false
					}
				}
				return true
			}
			else {
				return !tc.a.node(current).is_mut
			}
		}
	}
	return true
}

// shared_map_read_autolocks reports whether reading `id` read-locks a `shared` map by itself.
fn (tc &TypeChecker) shared_map_read_autolocks(id flat.NodeId) bool {
	if tc.lock_depth > 0 || !tc.valid_node_id(id) || !tc.shared_map_autolock_possible() {
		return false
	}
	autolock := tc.shared_map_autolock(id, *tc.a.node(id)) or { return false }
	return autolock.mode == `r`
}

// unlocked_shared_read_access is unlocked_shared_access for a value that is only read,
// where reads of `shared` maps that lock the map automatically are allowed.
fn (tc &TypeChecker) unlocked_shared_read_access(id flat.NodeId) ?SharedAccessDiagnostic {
	if !tc.shared_map_autolock_possible() {
		return tc.unlocked_shared_access(id)
	}
	return tc.unlocked_shared_access_impl(id, true)
}

fn (mut tc TypeChecker) enter_shared_map_autolock(autolock SharedMapAutolock) {
	mut modes := (tc.fn_context.locked_shared_modes[autolock.key] or { []u8{} }).clone()
	modes << autolock.mode
	tc.fn_context.locked_shared_modes[autolock.key] = modes
	tc.fn_context.locked_shared_names[autolock.key]++
	tc.lock_depth++
	tc.autolocked_map = autolock.key
}

fn (mut tc TypeChecker) leave_shared_map_autolock(key string) {
	tc.autolocked_map = ''
	tc.lock_depth--
	tc.fn_context.locked_shared_names[key]--
	mut modes := (tc.fn_context.locked_shared_modes[key] or { []u8{} }).clone()
	if modes.len > 0 {
		modes.delete_last()
	}
	if modes.len == 0 {
		tc.fn_context.locked_shared_modes.delete(key)
	} else {
		tc.fn_context.locked_shared_modes[key] = modes
	}
}
