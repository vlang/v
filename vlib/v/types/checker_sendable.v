module types

import v.flat

const sendable_max_depth = 64

// type_is_sendable reports whether typ can be transferred across thread boundaries.
// It returns true with an empty reason when sendable, otherwise false with a
// human-readable reason naming the offending part. The check is fail-closed:
// pointers, function values, channels, shared types, foreign C types, interfaces,
// and unknown types are never sendable. Arrays, maps, options, results, aliases,
// and structs recurse into their elements. Struct fields marked `shared` fail.
pub fn (tc &TypeChecker) type_is_sendable(typ Type, mut seen map[u32]bool, depth int) (bool, string) {
	if depth > sendable_max_depth {
		return false, 'type nesting too deep (possible recursion)'
	}
	sid := typ.source_type_id()
	if sid != 0 {
		shape := flat.source_type_shape(sid)
		if shape.has(.shared_type) {
			return false, "shared type `${typ.name()}` cannot be transferred (transfer, don't share)"
		}
		if shape.has(.thread_type) {
			return false, 'thread handle `${typ.name()}` cannot be transferred'
		}
		if shape.has(.atomic_type) {
			base_id := flat.source_type_syntax(sid).raw_base()
			if base_id != 0 && base_id != sid {
				base := tc.parse_type(base_id)
				ok, reason := tc.type_is_sendable(base, mut seen, depth + 1)
				if !ok {
					return false, 'atomic `${typ.name()}` wraps non-sendable type: ${reason}'
				}
				return true, ''
			}
		}
		if flat.source_type_query(sid).has(.c_prefix)
			|| flat.source_type_name(sid).starts_with('C.') {
			return false, 'foreign C type `${typ.name()}` is not sendable'
		}
	}
	match typ {
		Primitive, String, Char, Rune, ISize, USize {
			return true, ''
		}
		Nil, None {
			return true, ''
		}
		Enum {
			return true, ''
		}
		Pointer {
			return false, 'reference `${typ.name()}` is not sendable (pointers cannot be transferred)'
		}
		FnType {
			return false, 'function value `${typ.name()}` is not sendable'
		}
		Channel {
			return false, "channel `${typ.name()}` is not sendable (transfer values, don't share channels)"
		}
		Array {
			ok, reason := tc.type_is_sendable(typ.elem_type, mut seen, depth + 1)
			if !ok {
				return false, 'array element of `${typ.name()}` is not sendable: ${reason}'
			}
			return true, ''
		}
		ArrayFixed {
			ok, reason := tc.type_is_sendable(typ.elem_type, mut seen, depth + 1)
			if !ok {
				return false, 'fixed array element of `${typ.name()}` is not sendable: ${reason}'
			}
			return true, ''
		}
		Map {
			ok, reason := tc.type_is_sendable(typ.key_type, mut seen, depth + 1)
			if !ok {
				return false, 'map key of `${typ.name()}` is not sendable: ${reason}'
			}
			ok2, reason2 := tc.type_is_sendable(typ.value_type, mut seen, depth + 1)
			if !ok2 {
				return false, 'map value of `${typ.name()}` is not sendable: ${reason2}'
			}
			return true, ''
		}
		OptionType {
			ok, reason := tc.type_is_sendable(typ.base_type, mut seen, depth + 1)
			if !ok {
				return false, 'option payload of `${typ.name()}` is not sendable: ${reason}'
			}
			return true, ''
		}
		ResultType {
			ok, reason := tc.type_is_sendable(typ.base_type, mut seen, depth + 1)
			if !ok {
				return false, 'result payload of `${typ.name()}` is not sendable: ${reason}'
			}
			return true, ''
		}
		Alias {
			ok, reason := tc.type_is_sendable(typ.base_type, mut seen, depth + 1)
			if !ok {
				return false, 'alias `${typ.name()}` is not sendable: ${reason}'
			}
			return true, ''
		}
		Struct {
			name_id := typ.source_type_id()
			if flat.source_type_query(name_id).has(.c_prefix)
				|| flat.source_type_name(name_id).starts_with('C.') {
				return false, 'foreign C struct `${typ.name()}` is not sendable'
			}
			if seen[name_id] {
				return true, ''
			}
			seen[name_id] = true
			for field in tc.struct_fields_for_type(name_id) {
				if tc.struct_field_is_shared(name_id, field.name) {
					return false, 'field `${field.name}` of `${typ.name()}` is shared and cannot be transferred'
				}
				ok, reason := tc.type_is_sendable(field.typ, mut seen, depth + 1)
				if !ok {
					return false, 'field `${field.name}` of `${typ.name()}` is not sendable: ${reason}'
				}
			}
			return true, ''
		}
		SumType {
			name_id := typ.source_type_id()
			if seen[name_id] {
				return true, ''
			}
			seen[name_id] = true
			base_name := tc.sum_base_name(name_id)
			for variant_id in tc.sum_types.type_ids(base_name) or { []u32{} } {
				concrete := tc.concrete_sum_variant_name(name_id, variant_id)
				concrete_type := tc.parse_type(concrete)
				ok, reason := tc.type_is_sendable(concrete_type, mut seen, depth + 1)
				if !ok {
					return false, 'sumtype variant of `${typ.name()}` is not sendable: ${reason}'
				}
			}
			return true, ''
		}
		Interface {
			return false, 'interface `${typ.name()}` is not sendable (dynamic dispatch cannot be verified)'
		}
		MultiReturn, Unknown, Void {
			return false, 'type `${typ.name()}` is not sendable'
		}
	}
}

// check_sendable_struct_decl verifies a `@[sendable]` struct declaration.
// Every field must be sendable. Generic structs are skipped here because their
// type parameters are verified at instantiation through boundary checks.
fn (mut tc TypeChecker) check_sendable_struct_decl(node_id flat.NodeId, node flat.Node) {
	if node.generic_params().len > 0 {
		return
	}
	if node.value.starts_with('C.') {
		tc.record_error_at(.assignment_mismatch, 'C struct `${node.value}` cannot be marked `@[sendable]`',
			node_id, tc.struct_declaration_name_pos(node))
		return
	}
	for i in 0 .. node.children_count {
		field := tc.a.child_node(&node, i)
		if field.kind != .field_decl {
			continue
		}
		field_type_id := if field.typ != 0 { field.typ } else { field.source_value_id() }
		field_type := tc.parse_type(field_type_id)
		mut seen := map[u32]bool{}
		ok, reason := tc.type_is_sendable(field_type, mut seen, 0)
		if !ok {
			field_id := tc.a.child(&node, i)
			tc.record_error_at(.assignment_mismatch, 'field `${field.value}` of sendable struct `${node.value}` is not sendable: ${reason}',
				field_id, tc.struct_field_type_pos(field))
		}
	}
}

// check_sendable_spawn_arg requires a `spawn`/`go` argument type to be sendable.
// Only types marked `@[sendable]` are checked here; unmarked types are
// unaffected for backward compatibility (including the compiler's own
// internal parallelism). A `shared` field inside a sendable struct still
// fails through type_is_sendable. Explicit `shared`/`atomic` spawn arguments
// share memory with locking and stay exempt.
fn (mut tc TypeChecker) check_sendable_spawn_arg(arg_id flat.NodeId, arg_type Type) {
	arg_node := tc.a.node(arg_id)
	if arg_node.value == 'shared' || arg_node.value.starts_with('shared:')
		|| arg_node.value == 'atomic' || arg_node.value.starts_with('atomic:') {
		return
	}
	if tc.current_binding_is_shared(arg_node.value) {
		return
	}
	if !tc.type_has_declaration_attribute(&arg_type, 'sendable') {
		return
	}
	mut seen := map[u32]bool{}
	ok, reason := tc.type_is_sendable(arg_type, mut seen, 0)
	if !ok {
		tc.record_error_at(.call_arg_mismatch, 'spawn argument of type `${arg_type.name()}` is not sendable: ${reason}',
			arg_id, tc.a.node(arg_id).pos)
	}
}

// check_sendable_channel_push requires a pushed channel value to be sendable.
// Only `@[sendable]`-marked values are checked; unmarked values keep their
// existing channel compatibility diagnostics for backward compatibility.
fn (mut tc TypeChecker) check_sendable_channel_push(rhs_id flat.NodeId, rhs_type Type) {
	if !tc.type_has_declaration_attribute(&rhs_type, 'sendable') {
		return
	}
	mut seen := map[u32]bool{}
	ok, reason := tc.type_is_sendable(rhs_type, mut seen, 0)
	if !ok {
		tc.record_error_at(.assignment_mismatch, 'cannot push non-sendable type `${rhs_type.name()}` on channel: ${reason}',
			rhs_id, tc.a.node(rhs_id).pos)
	}
}

// check_sendable_channel_pop requires a channel element type to be sendable.
// Only `@[sendable]`-marked element types are checked; unmarked channels
// keep their existing behavior for backward compatibility.
fn (mut tc TypeChecker) check_sendable_channel_pop(id flat.NodeId, elem_type Type) {
	if !tc.type_has_declaration_attribute(&elem_type, 'sendable') {
		return
	}
	mut seen := map[u32]bool{}
	ok, reason := tc.type_is_sendable(elem_type, mut seen, 0)
	if !ok {
		tc.record_error_at(.assignment_mismatch, 'cannot receive non-sendable type `${elem_type.name()}` from channel: ${reason}',
			id, tc.a.node(id).pos)
	}
}

// check_sendable_spawn_closure_captures requires explicit closure captures of a
// spawned call to be sendable. Only `fn_literal` captures are checked here;
// ordinary calls are unaffected.
fn (mut tc TypeChecker) check_sendable_spawn_closure_captures(call_id flat.NodeId) {
	call := tc.a.node(call_id)
	mut stack := []flat.NodeId{}
	for i in 0 .. call.children_count {
		stack << tc.a.child(call, i)
	}
	for stack.len > 0 {
		cur_id := stack.pop()
		cur := tc.a.node(cur_id)
		if cur.kind == .fn_literal {
			for j in 0 .. cur.children_count {
				cap_id := tc.a.child(cur, j)
				cap := tc.a.node(cap_id)
				if cap.kind != .ident || cap.value.len == 0 {
					continue
				}
				// Parameters are not captures.
				mut is_param := false
				for k in 0 .. cur.children_count {
					param := tc.a.child_node(cur, k)
					if param.kind == .param && param.value == cap.value {
						is_param = true
						break
					}
				}
				if is_param {
					continue
				}
				if capture_type := tc.cur_scope.lookup(cap.value) {
					// Only `@[sendable]`-marked captures are checked; unmarked
					// captures keep their existing behavior for compatibility.
					if !tc.type_has_declaration_attribute(&capture_type, 'sendable') {
						continue
					}
					mut seen := map[u32]bool{}
					ok, reason := tc.type_is_sendable(capture_type, mut seen, 0)
					if !ok {
						tc.record_error_at(.call_arg_mismatch, 'spawned closure capture `${cap.value}` of type `${capture_type.name()}` is not sendable: ${reason}',
							cap_id, tc.node_value_diagnostic_pos(cap_id))
					}
				}
			}
			continue
		}
		for i in 0 .. cur.children_count {
			stack << tc.a.child(cur, i)
		}
	}
}
