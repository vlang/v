module types

import v.flat

// Verify that the initializer moves an element from a plain mutable array field.
// The caller checks the storage reachable from those elements and the captures.
fn (tc &TypeChecker) ownership_scoped_factory_pool_pop(value &flat.Node, expected Type, receiver Struct, param string, module_name string) bool {
	if value.kind != .call || value.children_count != 1 {
		return false
	}
	callee := tc.a.child_node(value, 0)
	if callee.kind != .selector || callee.value != 'pop' || callee.children_count != 1 {
		return false
	}
	pool := tc.a.node(tc.ownership_unwrap_expr(tc.a.child(callee, 0)))
	if !tc.ownership_scoped_factory_receiver_field(*pool, receiver, param, module_name) {
		return false
	}
	for field in tc.struct_fields_for_type(receiver.name_id) {
		if field.name != pool.value {
			continue
		}
		// A nominal alias can provide its own pop method. Only a plain array
		// supplies the builtin move and ownership semantics.
		if field.is_mut && field.typ is Array {
			candidates := exact_array_receiver_method_candidates(field.typ, 'pop', module_name)
			if candidates.any(it in tc.fn_ret_types)
				|| tc.unique_receiver_method_suffix_match(field.typ, candidates) != none {
				return false
			}
			return semantic_types_equal(field.typ.elem_type, expected)
		}
		return false
	}
	return false
}

fn (tc &TypeChecker) ownership_scoped_factory_receiver_field(node flat.Node, receiver Struct, param string, module_name string) bool {
	if node.kind != .selector || node.children_count != 1
		|| receiver_method_name_candidates(receiver, node.value, module_name).any(it in tc.fn_ret_types) {
		return false
	}
	base := tc.a.child_node(&node, 0)
	return base.kind == .ident && base.value == param
}

// A record with a synchronized field can only live in storage that can hold
// that kind of handle. Check all pool element types after finding the captures.
fn (tc &TypeChecker) ownership_scoped_factory_pool_borrows_disjoint(pool_elements []Type, state_type Struct, borrows []ScopedThreadBorrow) bool {
	mut exclude_mutex := false
	for borrow in borrows {
		match borrow.access {
			.atomic {}
			.mutex { exclude_mutex = true }
			.record {
				field := tc.struct_field_type(state_type.name_id, borrow.field) or { return false }
				fields := tc.ownership_scoped_thread_record_fields(unwrap_pointer(unalias_type(field))) or {
					return false
				}
				accesses := fields.values()
				has_mutex := ScopedThreadAccess.mutex in accesses
				if !has_mutex && ScopedThreadAccess.atomic !in accesses {
					return false
				}
				exclude_mutex = exclude_mutex || has_mutex
			}
			else { return false }
		}
	}
	for elem in pool_elements {
		if !tc.ownership_scoped_factory_pool_excludes_handles(elem, exclude_mutex, 0, mut map[u32]bool{}) {
			return false
		}
	}
	return true
}

// Follow storage the popped value can own, including nested containers and
// the payloads of known owning handles. An opaque field cannot establish that
// its storage is separate from the synchronized handles captured by the callback.
fn (tc &TypeChecker) ownership_scoped_factory_pool_excludes_handles(typ Type, exclude_mutex bool, depth int, mut seen map[u32]bool) bool {
	if depth > 64 || fn_param_is_voidptr_type(typ) {
		return false
	}
	match typ {
		Primitive, Enum, String, Char, Rune, ISize, USize {
			return true
		}
		Alias, OptionType {
			return tc.ownership_scoped_factory_pool_excludes_handles(*typ.base_type, exclude_mutex,
				depth + 1, mut seen)
		}
		Array, ArrayFixed, Channel {
			return tc.ownership_scoped_factory_pool_excludes_handles(*typ.elem_type, exclude_mutex,
				depth + 1, mut seen)
		}
		Map {
			return tc.ownership_scoped_factory_pool_excludes_handles(*typ.key_type, exclude_mutex,
				depth + 1, mut seen)
				&& tc.ownership_scoped_factory_pool_excludes_handles(*typ.value_type, exclude_mutex,
					depth + 1, mut seen)
		}
		Struct {
			name := typ.name_id
			if seen[name] {
				return true
			}
			seen[name] = true
			if flat.source_type_query(name).has(.c_prefix)
				|| tc.ownership_thread_library_type(name, 'sync/arc', ['arc.Atomic', 'sync.arc.Atomic'])
				|| (exclude_mutex
					&& tc.ownership_thread_library_type(name, 'sync/arc', ['arc.Mutex', 'sync.arc.Mutex'])) {
				return false
			}
			if payload := tc.ownership_thread_library_payload(name, 'sync/arc', [
				'arc.Arc',
				'sync.arc.Arc',
				'arc.Weak',
				'sync.arc.Weak',
				'arc.Mutex',
				'sync.arc.Mutex',
				'arc.Once',
				'sync.arc.Once',
			]) {
				return tc.ownership_scoped_factory_pool_excludes_handles(payload, exclude_mutex,
					depth + 1, mut seen)
			}
			source := tc.source_struct_decl_for_name(name) or { return false }
			fields := tc.struct_fields_for_type(name)
			if tc.ownership_fn_decl_file(source).ends_with('.vh')
				|| fields.len == 0 && source.children_count > 0 {
				return false
			}
			for field in fields {
				if !tc.ownership_scoped_factory_pool_excludes_handles(field.typ, exclude_mutex,
					depth + 1, mut seen) {
					return false
				}
			}
			return true
		}
		SumType {
			name := typ.name_id
			if seen[name] {
				return true
			}
			seen[name] = true
			variants := tc.sum_types.type_ids(tc.sum_base_name(name)) or { return false }
			for variant in variants {
				concrete := tc.parse_type(tc.concrete_sum_variant_name(name, variant))
				if !tc.ownership_scoped_factory_pool_excludes_handles(concrete, exclude_mutex,
					depth + 1, mut seen) {
					return false
				}
			}
			return true
		}
		else {
			return false
		}
	}
}
