module fastc

import v.flat

// fastc_callable_type_name preserves an unnamed function type through collections and options.
fn fastc_callable_type_name(info FastcFunctionTypeInfo) string {
	mut parts := [info.return_type, info.option_value_type]
	parts << info.parameter_types.ids
	mut name := '__vf_callable_'
	for part in parts {
		encoded := flat.source_type_name(fastc_composite_part_id(part))
		name += '${encoded.len}_${encoded}'
	}
	return name + '_end'
}

fn fastc_callable_signature(typ u32) ?FastcFunctionSignature {
	name := flat.source_type_name(typ)
	if !name.starts_with('__vf_callable_') || !name.ends_with('_end') { return none }
	mut offset := '__vf_callable_'.len
	mut parts := []u32{}
	for offset < name.len - 4 {
		start := offset
		for offset < name.len && name[offset].is_digit() { offset++ }
		if start == offset || offset >= name.len || name[offset] != `_` { return none }
		length := name[start..offset].int()
		offset++
		if length < 0 || length > name.len - offset { return none }
		parts << fastc_decode_ptr_element_type_id(flat.source_type_id(name[offset..offset + length]))
		offset += length
	}
	if parts.len < 2 { return none }
	return FastcFunctionSignature{
		return_type:     parts[0]
		option_type:     parts[1]
		parameter_types: flat.TypeTextList{ ids: parts[2..].clone() }
	}
}

// equality_interface_variants finds concrete interface implementations for value equality.
fn (g &Parser) equality_interface_variants(typ u32) []u32 {
	if variants := g.interface_equality_variants[typ] { return variants }
	key := g.semantic_type_key(typ)
	prefix := flat.source_type_name(key) + '.'
	mut requirements := []string{}
	for name, signature in g.functions {
		if name.starts_with(prefix) && signature.parameter_types.ids.len > 0
			&& fastc_trim_pointer_type(signature.parameter_types.ids[0]) == typ {
			requirements << name
		}
	}
	requirements.sort()
	mut variants := []u32{}
	for candidate, kind in g.declared_kinds {
		if kind != .struct_ { continue }
		mut matches := true
		for requirement in requirements {
			candidate_key := flat.source_type_name(candidate) + '.' + requirement.all_after_last('.')
			if signature := g.functions[candidate_key] {
				if !fastc_interface_method_signatures_match(g.functions[requirement], signature) {
					matches = false
					break
				}
			} else {
				matches = false
				break
			}
		}
		if matches {
			c_type := flat.source_type_id(fastc_c_declared_type_name(flat.source_type_name(candidate)))
			for field_key, field in g.interface_fields {
				if !field_key.starts_with(prefix) { continue }
				if g.struct_member_type(c_type, field.name) != field.typ {
					matches = false
					break
				}
			}
			if matches { variants << c_type }
		}
	}
	variants.sort_with_compare(fn (a &u32, b &u32) int {
		return flat.source_type_name(*a).compare(flat.source_type_name(*b))
	})
	mut w := unsafe { &Parser(g) }
	w.interface_equality_variants[typ] = variants
	return variants
}
