module types

import v.flat

@[flag]
pub enum AllocationLayout {
	aligned
	scanned
}

// requires_aligned_allocation reports whether a value needs over-aligned storage.
pub fn (tc &TypeChecker) requires_aligned_allocation(typ &Type) bool {
	return tc.allocation_layout(typ).has(.aligned)
}

// allocation_layout describes alignment and scanning requirements of a canonical value.
pub fn (tc &TypeChecker) allocation_layout(typ &Type) AllocationLayout {
	match typ {
		Primitive, Char, Rune, ISize, USize, Enum {
			// Zero is the valid empty layout flag set.
			return unsafe { AllocationLayout(0) }
		}
		Alias, ArrayFixed, OptionType, ResultType, Struct, SumType, MultiReturn {}
		else { return .scanned }
	}
	id, canonical := tc.intern_type(typ)
	mut cache := tc.type_cache
	if !isnil(cache) {
		if cached := cache.allocation_layouts[id] {
			return cached
		}
	}
	mut seen := map[u32]bool{}
	layout := tc.value_allocation_layout(canonical, mut seen)
	if !isnil(cache) {
		cache.allocation_layouts[id] = layout
	}
	return layout
}

fn (tc &TypeChecker) value_allocation_layout(typ &Type,
	mut seen map[u32]bool) AllocationLayout {
	// Zero is the valid empty layout flag set.
	mut layout := unsafe { AllocationLayout(0) }
	name_id := match typ {
		Primitive, Char, Rune, ISize, USize, Enum { return layout }
		Alias, OptionType { return tc.value_allocation_layout(typ.base_type, mut seen) }
		ResultType {
			return tc.value_allocation_layout(typ.base_type, mut seen) | .scanned
		}
		ArrayFixed { return tc.value_allocation_layout(typ.elem_type, mut seen) }
		MultiReturn {
			for i in 0 .. typ.types.len {
				layout |= tc.value_allocation_layout(&typ.types[i], mut seen)
			}
			return layout
		}
		Struct, SumType { typ.source_type_id() }
		else { return .scanned }
	}
	if seen[name_id] {
		return .scanned
	}
	seen[name_id] = true
	defer { seen.delete(name_id) }
	if typ is Struct {
		return tc.struct_allocation_layout(name_id, mut seen)
	}
	base := tc.sum_base_name(name_id)
	variants := tc.sum_types.type_ids(flat.source_type_name(base)) or { return .scanned }
	for raw in variants {
		variant := tc.concrete_sum_variant_name(name_id, raw)
		parsed := tc.parse_canonical_type_cached(variant)
		layout |= tc.value_allocation_layout(parsed, mut seen)
	}
	return layout
}

fn (tc &TypeChecker) struct_allocation_layout(name_id u32,
	mut seen map[u32]bool) AllocationLayout {
	name := flat.source_type_name(name_id)
	if name.starts_with('C.') {
		return .aligned | .scanned
	}
	base, _, is_generic := generic_type_application_parts(name)
	key := if is_generic { base } else { name }
	// Zero is the valid empty layout flag set.
	mut layout := unsafe { AllocationLayout(0) }
	if index := tc.first_type_declaration_ids[key] {
		if tc.declaration_has_attribute(flat.NodeId(index), 'aligned') {
			layout |= .aligned
		}
	}
	if key !in tc.structs {
		return layout | .scanned
	}
	for field in tc.struct_fields_for_type(name_id) {
		if tc.struct_field_is_shared(name_id, field.name) {
			layout |= .scanned
			continue
		}
		layout |= tc.value_allocation_layout(field.typ, mut seen)
	}
	return layout
}
