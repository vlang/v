module types

import v.flat

// guarded_by_attr_value parses `@[guarded_by: 'mu']` field attribute values.
// The parser preserves the argument quotes (`guarded_by: 'mu'`), so the value
// is unquoted before use. Returns none for any other attribute.
fn guarded_by_attr_value(raw string) ?string {
	if raw.all_before(':').trim_space() != 'guarded_by' {
		return none
	}
	if !raw.contains(':') {
		return none
	}
	value := comptime_static_unquote(raw.all_after(':').trim_space())
	if value.len == 0 {
		return none
	}
	return value
}

// requires_lock_attr_value parses `@[requires_lock: 'mu']` declaration
// attribute values. The parser preserves the argument quotes, so the value
// is unquoted before use. Returns none for any other attribute.
fn requires_lock_attr_value(raw string) ?string {
	if raw.all_before(':').trim_space() != 'requires_lock' {
		return none
	}
	if !raw.contains(':') {
		return none
	}
	value := comptime_static_unquote(raw.all_after(':').trim_space())
	if value.len == 0 {
		return none
	}
	return value
}

// struct_guarded_by_field reports the guard field name for `field_name` of
// the struct `struct_name_id`, following the same spelling candidates as
// struct_field_is_shared (generic base, dotted qualifier, qualified name).
fn (tc &TypeChecker) struct_guarded_by_field(struct_name_id u32, field_name string) ?string {
	if struct_name_id == 0 || field_name == '' || tc.struct_guarded_by.len == 0 {
		return none
	}
	if guard := tc.struct_guarded_by[struct_name_id][field_name] {
		if guard.len > 0 {
			return guard
		}
	}
	syntax := flat.source_type_syntax(struct_name_id)
	mut candidates := []u32{cap: 4}
	if syntax.application_valid() {
		candidates << syntax.application_base()
	}
	if flat.source_type_shape(struct_name_id).has(.dotted) {
		candidates << syntax.unqualified()
	} else {
		qname_id := tc.qualify_type_name(struct_name_id)
		if qname_id != struct_name_id {
			candidates << qname_id
		}
	}
	for candidate_id in candidates {
		if guard := tc.struct_guarded_by[candidate_id][field_name] {
			if guard.len > 0 {
				return guard
			}
		}
	}
	return none
}

// is_mutex_type reports whether `typ` is a `sync.Mutex` or `sync.RwMutex`
// (after unwrapping pointers and aliases). Only these types can be locked
// for RFC 009 guarded fields.
fn (tc &TypeChecker) is_mutex_type(typ &Type) bool {
	clean := unalias_type(unwrap_all_pointers(typ))
	if clean is Struct {
		name := clean.name()
		short := if idx := name.last_index('.') {
			name[idx + 1..]
		} else {
			name
		}
		return short in ['Mutex', 'RwMutex']
			&& (name in ['sync.Mutex', 'sync.RwMutex', 'Mutex', 'RwMutex']
				|| name.ends_with('.Mutex') || name.ends_with('.RwMutex'))
	}
	return false
}

// is_mutex_lock_object reports whether the `lock`/`rlock` object is a mutex
// (a `sync.Mutex`/`sync.RwMutex` variable or struct field).
fn (tc &TypeChecker) is_mutex_lock_object(id flat.NodeId) bool {
	if !tc.valid_node_id(id) {
		return false
	}
	typ := tc.resolve_type(id)
	return tc.is_mutex_type(&typ)
}

// guarded_lock_root_name returns the root identifier of a lock/access path,
// used to qualify lock identity with its scope owner.
fn (tc &TypeChecker) guarded_lock_root_name(id flat.NodeId) string {
	mut current := id
	for _ in 0 .. 32 {
		if !tc.valid_node_id(current) {
			return ''
		}
		node := tc.a.node(current)
		match node.kind {
			.ident {
				return node.value
			}
			.selector, .index {
				if node.children_count == 0 {
					return ''
				}
				current = tc.a.child(node, 0)
			}
			.paren {
				if node.children_count == 0 {
					return ''
				}
				current = tc.a.child(node, 0)
			}
			.prefix {
				if node.children_count == 0 {
					return ''
				}
				current = tc.a.child(node, 0)
			}
			else {
				return ''
			}
		}
	}
	return ''
}

// guarded_lock_identity_key computes the identity key for a held lock object.
// Lock identity is the full expression path, not the guard name: `s.mu` and
// `t.mu` are distinct. Where the root has a scope owner, the owner storage
// key qualifies the path so shadowed variables in nested scopes do not alias.
fn (mut tc TypeChecker) guarded_lock_identity_key(id flat.NodeId) string {
	textual := tc.shared_lock_key(id)
	if textual.len == 0 {
		return ''
	}
	root := tc.guarded_lock_root_name(id)
	if root.len > 0 {
		if owner := tc.cur_scope.lookup_owner(root) {
			storage_key := owner.storage_key()
			if storage_key.len > 0 {
				return textual + '\x00' + storage_key
			}
		}
	}
	return textual
}

// guarded_required_lock_key computes the identity key for the lock required
// to access `base_id`.`guard`. It must match guarded_lock_identity_key for
// the corresponding `lock base.guard { ... }` object.
fn (mut tc TypeChecker) guarded_required_lock_key(base_id flat.NodeId, guard string) string {
	base_textual := tc.shared_lock_key(base_id)
	if base_textual.len == 0 || guard.len == 0 {
		return ''
	}
	textual := '${base_textual}.${guard}'
	root := tc.guarded_lock_root_name(base_id)
	if root.len > 0 {
		if owner := tc.cur_scope.lookup_owner(root) {
			storage_key := owner.storage_key()
			if storage_key.len > 0 {
				return textual + '\x00' + storage_key
			}
		}
	}
	return textual
}

// guarded_lock_held reports whether the given lock identity key is held.
fn (tc &TypeChecker) guarded_lock_held(key string) bool {
	if key.len == 0 {
		return false
	}
	return (tc.fn_context.held_mutex_locks[key] or { 0 }) > 0
}

// check_guarded_field_access diagnoses a read of a `@[guarded_by]` field
// without the guard lock held. Assignment targets are handled by
// check_guarded_assign_lhs; callers skip those via selector_is_assignment_lhs.
fn (mut tc TypeChecker) check_guarded_field_access(id flat.NodeId, node flat.Node, struct_id u32) {
	guard := tc.struct_guarded_by_field(struct_id, node.value) or { return }
	if node.children_count == 0 {
		return
	}
	base_id := tc.a.child(&node, 0)
	required := tc.guarded_required_lock_key(base_id, guard)
	if tc.guarded_lock_held(required) {
		return
	}
	access_text := tc.source_text_for_node(id).trim_space()
	required_text := tc.shared_lock_key(base_id)
	required_display := if required_text.len > 0 { '${required_text}.${guard}' } else { guard }
	tc.record_error_at(.assignment_mismatch, '`${access_text}` is guarded by `${guard}` and needs explicit lock `${required_display}`',
		id, tc.selector_field_diagnostic_pos(id, node.value))
}

// check_guarded_assign_lhs diagnoses a write to a `@[guarded_by]` field
// without the guard lock held. It walks index chains so `s.m[k] = v` and
// `s.m[k] += v` still require `lock s.mu`.
fn (mut tc TypeChecker) check_guarded_assign_lhs(lhs_id flat.NodeId) {
	mut current := tc.unwrap_paren_expr_id(lhs_id)
	for _ in 0 .. 32 {
		if !tc.valid_node_id(current) {
			return
		}
		node := tc.a.node(current)
		match node.kind {
			.selector {
				if node.children_count > 0 && node.value.len > 0 {
					base_id := tc.a.child(node, 0)
					base_type := unalias_and_unwrap_pointer_type(tc.resolve_type(base_id))
					if base_type is Struct {
						if guard := tc.struct_guarded_by_field(base_type.source_type_id(), node.value) {
							required := tc.guarded_required_lock_key(base_id, guard)
							if !tc.guarded_lock_held(required) {
								access_text := tc.source_text_for_node(current).trim_space()
								required_text := tc.shared_lock_key(base_id)
								required_display := if required_text.len > 0 {
									'${required_text}.${guard}'
								} else {
									guard
								}
								tc.record_error_at(.assignment_mismatch, '`${access_text}` is guarded by `${guard}` and needs explicit lock `${required_display}`',
									current, tc.selector_field_diagnostic_pos(current, node.value))
							}
							return
						}
					}
				}
				return
			}
			.index {
				if node.children_count == 0 {
					return
				}
				current = tc.unwrap_paren_expr_id(tc.a.child(node, 0))
			}
			.paren {
				if node.children_count == 0 {
					return
				}
				current = tc.unwrap_paren_expr_id(tc.a.child(node, 0))
			}
			else {
				return
			}
		}
	}
}

// requires_lock_guard_for_fn reports the `@[requires_lock]` guard for a
// resolved call name, checking the short name as well (mirrors deprecation).
fn (tc &TypeChecker) requires_lock_guard_for_fn(name string) ?string {
	if guard := tc.requires_lock_fns[name] {
		if guard.len > 0 {
			return guard
		}
	}
	short := short_name_view(name)
	if short != name {
		if guard := tc.requires_lock_fns[short] {
			if guard.len > 0 {
				return guard
			}
		}
	}
	return none
}

// guarded_call_base_id returns the receiver or struct argument that owns the
// `@[requires_lock]` guard for a call.
fn (mut tc TypeChecker) guarded_call_base_id(node flat.Node, info CallInfo, guard string) flat.NodeId {
	if info.has_receiver && node.children_count > 0 {
		callee := tc.a.node(tc.a.child(&node, 0))
		if callee.kind == .selector && callee.children_count > 0 {
			return tc.a.child(callee, 0)
		}
	}
	// Free functions: prefer the argument whose struct declares this guard.
	for i in 1 .. node.children_count {
		arg_id := tc.call_arg_value(tc.a.child(&node, i))
		if !tc.valid_node_id(arg_id) {
			continue
		}
		arg_type := unalias_and_unwrap_pointer_type(tc.resolve_type(arg_id))
		if arg_type is Struct {
			for field_guard in tc.struct_guarded_by[arg_type.source_type_id()].values() {
				if field_guard == guard {
					return arg_id
				}
			}
			syntax := flat.source_type_syntax(arg_type.source_type_id())
			mut candidates := []u32{cap: 3}
			if syntax.application_valid() {
				candidates << syntax.application_base()
			}
			qname_id := tc.qualify_type_name(arg_type.source_type_id())
			if qname_id != arg_type.source_type_id() {
				candidates << qname_id
			}
			for candidate_id in candidates {
				for field_guard in tc.struct_guarded_by[candidate_id].values() {
					if field_guard == guard {
						return arg_id
					}
				}
			}
		}
	}
	if node.children_count > 1 {
		return tc.call_arg_value(tc.a.child(&node, 1))
	}
	return flat.NodeId(-1)
}

// check_call_requires_lock diagnoses a call to a `@[requires_lock]` function
// without the required mutex held by the caller.
fn (mut tc TypeChecker) check_call_requires_lock(id flat.NodeId, node flat.Node, info CallInfo) {
	if info.name.len == 0 {
		return
	}
	guard := tc.requires_lock_guard_for_fn(info.name) or { return }
	base_id := tc.guarded_call_base_id(node, info, guard)
	if !tc.valid_node_id(base_id) {
		tc.record_error_at(.call_arg_mismatch, 'call to `${info.name}` requires lock `${guard}` to be held',
			id, node.pos)
		return
	}
	required := tc.guarded_required_lock_key(base_id, guard)
	if tc.guarded_lock_held(required) {
		return
	}
	base_textual := tc.shared_lock_key(base_id)
	required_display := if base_textual.len > 0 { '${base_textual}.${guard}' } else { guard }
	mut pos := node.pos
	if node.children_count > 0 {
		callee := tc.a.node(tc.a.child(&node, 0))
		if callee.kind == .selector {
			pos = tc.method_call_name_pos(node, callee)
		}
	}
	tc.record_error_at(.call_arg_mismatch, 'call to `${info.name}` requires lock `${required_display}` to be held',
		id, pos)
}

// seed_requires_lock_for_fn assumes the `@[requires_lock]` guard held for the
// duration of the function body. The guard names a mutex field of the receiver
// (methods) or of the struct parameter (free functions).
fn (mut tc TypeChecker) seed_requires_lock_for_fn(fn_idx int, node flat.Node) {
	attrs := tc.declaration_attributes[fn_idx] or { return }
	mut guard := ''
	for raw in attrs {
		if value := requires_lock_attr_value(raw) {
			guard = value
			break
		}
	}
	if guard.len == 0 {
		return
	}
	mut base_name := ''
	if node.children_count > 0 {
		first := tc.a.node(tc.a.child(&node, 0))
		if first.kind == .param && first.op == .dot && first.value.len > 0 {
			base_name = first.value
		}
	}
	if base_name.len == 0 {
		// Free function: use the parameter whose struct declares this guard.
		for i in 0 .. node.children_count {
			param_id := tc.a.child(&node, i)
			param := tc.a.node(param_id)
			if param.kind != .param || param.value.len == 0 || param.op == .dot {
				continue
			}
			param_type := unalias_and_unwrap_pointer_type(tc.parse_type(param.typ))
			if param_type is Struct {
				if _ := tc.struct_guarded_by_field(param_type.source_type_id(), '') {
					// Fall through to generic match below; empty field never matches.
				}
				mut found := false
				for field_guard in tc.struct_guarded_by[param_type.source_type_id()].values() {
					if field_guard == guard {
						found = true
						break
					}
				}
				if !found {
					syntax := flat.source_type_syntax(param_type.source_type_id())
					if syntax.application_valid() {
						for field_guard in tc.struct_guarded_by[syntax.application_base()].values() {
							if field_guard == guard {
								found = true
								break
							}
						}
					}
				}
				if found {
					base_name = param.value
					break
				}
			}
		}
		if base_name.len == 0 {
			for i in 0 .. node.children_count {
				param_id := tc.a.child(&node, i)
				param := tc.a.node(param_id)
				if param.kind == .param && param.value.len > 0 && param.op != .dot {
					base_name = param.value
					break
				}
			}
		}
	}
	if base_name.len == 0 {
		return
	}
	textual := '${base_name}.${guard}'
	mut key := textual
	if owner := tc.cur_scope.lookup_owner(base_name) {
		storage_key := owner.storage_key()
		if storage_key.len > 0 {
			key = textual + '\x00' + storage_key
		}
	}
	tc.fn_context.held_mutex_locks[key]++
}

// maybe_track_mutex_lock_unlock tracks `m.lock()` / `m.rlock()` and
// `m.unlock()` calls on `sync.Mutex` / `sync.RwMutex` receivers so
// `@[guarded_by]` accesses after an explicit lock are accepted. The C
// backend cannot express `lock s.mu { ... }` for a plain Mutex value,
// so runtime code must use the method form.
fn (mut tc TypeChecker) maybe_track_mutex_lock_unlock(node flat.Node, info CallInfo) {
	if !info.has_receiver || node.children_count == 0 {
		return
	}
	callee := tc.a.node(tc.a.child(&node, 0))
	if callee.kind != .selector || callee.children_count == 0 {
		return
	}
	method := callee.value
	if method !in ['lock', 'rlock', 'unlock'] {
		return
	}
	receiver_id := tc.a.child(callee, 0)
	if !tc.valid_node_id(receiver_id) {
		return
	}
	if !tc.is_mutex_lock_object(receiver_id) {
		return
	}
	key := tc.guarded_lock_identity_key(receiver_id)
	if key.len == 0 {
		return
	}
	if method == 'unlock' {
		if (tc.fn_context.held_mutex_locks[key] or { 0 }) > 1 {
			tc.fn_context.held_mutex_locks[key]--
		} else {
			tc.fn_context.held_mutex_locks.delete(key)
		}
		return
	}
	tc.fn_context.held_mutex_locks[key]++
}
