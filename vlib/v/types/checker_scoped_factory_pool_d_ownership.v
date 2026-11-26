module types

import v.flat

// A factory can move an element out of an owned array without invalidating
// the references held by earlier visitors when the array cannot own their
// referents. A pop changes the array header, so the factory proof allows this
// operation only for atomic captures.
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
				&& tc.ownership_scoped_factory_pool_excludes_atomics(field.typ.elem_type, 0, mut map[u32]bool{})
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

// Follow storage the popped value can own, including nested containers and
// the payloads of known owning handles. An opaque field cannot establish that
// its storage is separate from the atomics captured by the callback.
fn (tc &TypeChecker) ownership_scoped_factory_pool_excludes_atomics(typ Type, depth int, mut seen map[u32]bool) bool {
	if depth > 64 || fn_param_is_voidptr_type(typ) {
		return false
	}
	match typ {
		Primitive, Enum, String, Char, Rune, ISize, USize {
			return true
		}
		Alias, OptionType {
			return tc.ownership_scoped_factory_pool_excludes_atomics(*typ.base_type, depth + 1, mut seen)
		}
		Array, ArrayFixed, Channel {
			return tc.ownership_scoped_factory_pool_excludes_atomics(*typ.elem_type, depth + 1, mut seen)
		}
		Map {
			return tc.ownership_scoped_factory_pool_excludes_atomics(*typ.key_type, depth + 1, mut seen)
				&& tc.ownership_scoped_factory_pool_excludes_atomics(*typ.value_type, depth + 1, mut seen)
		}
		Struct {
			name := typ.name_id
			if seen[name] {
				return true
			}
			seen[name] = true
			if flat.source_type_query(name).has(.c_prefix)
				|| tc.ownership_thread_library_type(name, 'sync/arc', ['arc.Atomic', 'sync.arc.Atomic']) {
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
				return tc.ownership_scoped_factory_pool_excludes_atomics(payload, depth + 1, mut seen)
			}
			source := tc.source_struct_decl_for_name(name) or { return false }
			fields := tc.struct_fields_for_type(name)
			if tc.ownership_fn_decl_file(source).ends_with('.vh')
				|| fields.len == 0 && source.children_count > 0 {
				return false
			}
			for field in fields {
				if !tc.ownership_scoped_factory_pool_excludes_atomics(field.typ, depth + 1, mut seen) {
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
				if !tc.ownership_scoped_factory_pool_excludes_atomics(concrete, depth + 1, mut seen) {
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
