module c

import v.flat
import v.types { unalias_type }
import v.gen.c.naming { sum_field_name }

struct SumUniqueFieldInfo {
	variant string
	typ     types.Type
}

@[heap]
struct SumVariantActualCache {
mut:
	by_sum map[string]map[string]string
}

// emit_sum_type emits emit sum type output for c.
fn (mut g FlatGen) emit_sum_type(name string) {
	g.writeln('struct ${g.cname(name)} {')
	g.writeln('\tint typ;')
	g.writeln('\tunion {')
	for variant in g.tc.sum_types[name] {
		typ := g.tc.parse_canonical_type(variant)
		ct := g.value_c_type(typ)
		field := sum_field_name(variant)
		g.writeln('\t\t${ct} ${field};')
	}
	g.writeln('\t};')
	g.writeln('};')
	g.writeln('')
}

fn (mut g FlatGen) gen_sum_variant_pointer_cast(id flat.NodeId, target types.Pointer, ct string) bool {
	source_type0 := g.sum_cast_actual_type(id)
	source_ptr := match source_type0 {
		types.Pointer { source_type0 }
		else {
			return false
		}
	}

	source_base := match source_ptr.base_type {
		types.Alias { *source_ptr.base_type.base_type }
		else { *source_ptr.base_type }
	}

	source_sum := match source_base {
		types.SumType { source_base }
		else {
			return false
		}
	}

	target_base := match target.base_type {
		types.Alias { *target.base_type.base_type }
		else { *target.base_type }
	}

	sum_name := g.resolve_sum_name(source_sum.name)
	variants := g.tc.sum_types[sum_name] or { return false }
	variant := g.resolve_variant(sum_name, target_base.name())
	if variant !in variants {
		return false
	}
	sum_ct := g.tc.c_type(types.Type(source_sum))
	field := sum_field_name(variant)
	g.write('(${ct})&((${sum_ct}*)(')
	g.gen_expr(id)
	g.write('))->${field}')
	return true
}

// gen_sum_value_expr emits sum value expr output for c.
fn (mut g FlatGen) gen_sum_value_expr(id flat.NodeId, expected types.Type) bool {
	sum_type := g.sum_type_for_expected_value(expected) or { return false }
	sum_type0 := types.Type(sum_type)
	raw_actual0 := g.sum_cast_actual_type(id)
	raw_actual_type := unalias_type(raw_actual0)
	if raw_actual_type is types.SumType {
		// A sum type can itself be a variant of a wider sum type (for example
		// `ast.Stmt` inside `ast.Node`). Only skip wrapping when the value is
		// already the expected sum.
		if g.type_names_match(raw_actual_type, sum_type0) || g.resolve_sum_name(raw_actual_type.name) == g.resolve_sum_name(sum_type.name) || (raw_actual_type.name !in g.tc.sum_types && raw_actual_type.name.all_after_last('.') == sum_type.name.all_after_last('.')) {
			return false
		}
	}
	if declared := g.selector_declared_type(id) {
		declared0 := unalias_type(declared)
		if declared0 is types.SumType && g.type_names_match(declared0, sum_type0) {
			return false
		}
	}
	sum_name := g.resolve_sum_name(sum_type.name)
	variant := g.sum_variant_for_actual(sum_name, raw_actual0) or { return false }
	g.gen_sum_variant_value(sum_type, variant, id)
	return true
}

fn (g &FlatGen) sum_type_for_expected_value(expected &types.Type) ?types.SumType {
	clean := unalias_type(expected)
	if clean is types.SumType {
		return clean
	}
	if clean is types.Struct {
		resolved := g.resolve_sum_name(clean.name)
		if resolved in g.tc.sum_types {
			return types.SumType{
				name: resolved
			}
		}
	}
	return none
}

fn (g &FlatGen) sum_variant_for_actual(sum_name0 string, actual &types.Type) ?string {
	sum_name := g.resolve_sum_name(sum_name0)
	actual_name := actual.name()
	if sum_cache := g.sum_variant_actual_cache.by_sum[sum_name] {
		if cached := sum_cache[actual_name] {
			if cached.len > 0 {
				return cached
			}
			return none
		}
	}
	result := g.sum_variant_for_actual_uncached(sum_name, actual, actual_name) or { '' }
	mut cache := g.sum_variant_actual_cache
	if mut sum_cache := cache.by_sum[sum_name] {
		sum_cache[actual_name] = result
	} else {
		cache.by_sum[sum_name] = {
			actual_name: result
		}
	}
	if result.len == 0 {
		return none
	}
	return result
}

// gen_sum_cast_expr emits sum cast expr output for c.
fn (mut g FlatGen) gen_sum_cast_expr(target types.SumType, value flat.NodeId) {
	actual := g.sum_cast_actual_type(value)
	clean := unalias_type(actual)
	if clean is types.SumType && g.type_names_match(clean, types.Type(target)) {
		g.gen_expr(value)
		return
	}
	variant := g.sum_variant_for_actual(target.name, actual) or {
		g.resolve_variant(target.name, types.unwrap_pointer(actual).name())
	}
	g.gen_sum_variant_value(target, variant, value)
}

fn (mut g FlatGen) gen_sum_variant_value(sum types.SumType, variant string, value flat.NodeId) {
	ct := g.value_c_type(types.Type(sum))
	idx := g.sum_type_index(sum.name, variant)
	field := sum_field_name(variant)
	typ := g.tc.parse_canonical_type(variant)
	if fixed := array_fixed_type(typ) {
		tmp := g.tmp_name()
		g.write('({ ${ct} ${tmp} = {.typ = ${idx}}; memcpy(${tmp}.${field}, ')
		g.gen_fixed_array_copy_source(value, fixed)
		g.write(', sizeof(${tmp}.${field})); ${tmp}; })')
		return
	}
	g.write('(${ct}){.typ = ${idx}, .${field} = ')
	g.gen_expr_with_expected_type(value, typ)
	g.write('}')
}

fn (g &FlatGen) sum_unique_variant_field_info(base_type0 &types.Type, field string) ?SumUniqueFieldInfo {
	sum_name := g.sum_type_name_for_type(base_type0) or { return none }
	variants := g.tc.sum_types[sum_name] or { return none }
	mut found := SumUniqueFieldInfo{}
	mut found_count := 0
	for variant0 in variants {
		variant := g.resolve_variant(sum_name, variant0)
		variant_field_type := g.struct_field_type(variant, field) or { continue }
		found = SumUniqueFieldInfo{
			variant: variant
			typ:     variant_field_type
		}
		found_count++
		if found_count > 1 {
			return none
		}
	}
	if found_count == 1 {
		return found
	}
	return none
}

fn (mut g FlatGen) gen_sum_unique_variant_field_selector(base_id flat.NodeId, base_type0 types.Type, field string) bool {
	info := g.sum_unique_variant_field_info(base_type0, field) or { return false }
	sum_field := sum_field_name(info.variant)
	g.write('(')
	g.gen_expr(base_id)
	g.write(')')
	if base_type0 is types.Pointer {
		g.write('->')
	} else {
		g.write('.')
	}
	variant := unalias_type(g.tc.parse_canonical_type(info.variant))
	op := if variant is types.Pointer { '->' } else { '.' }
	g.write('${sum_field}${op}${g.init_field_c_name(info.variant, field)}')
	return true
}

fn (mut g FlatGen) gen_sum_shared_field_selector(base_id flat.NodeId, base_type0 types.Type, field string) bool {
	sum_name := g.sum_type_name_for_type(base_type0) or { return false }
	common_type := g.sum_shared_field_type(base_type0, field) or { return false }
	ct := g.value_c_type(common_type)
	sum_ct := g.tc.c_type(g.interface_concrete_type(sum_name))
	g.write('({ ${sum_ct} const* __sum = ')
	if base_type0 is types.Pointer {
		g.gen_expr(base_id)
	} else if g.expr_is_addressable(base_id) {
		g.write('&(')
		gen_expr_lvalue(mut g, base_id)
		g.write(')')
	} else {
		g.write('&(${sum_ct}[]){')
		g.gen_expr(base_id)
		g.write('}[0]')
	}
	g.writeln('; ${ct} __field = {0};')
	g.gen_sum_shared_field_switch('(*__sum)', sum_name, field, []string{})
	g.write('__field; })')
	return true
}

fn (mut g FlatGen) gen_sum_type_tag_selector(base_id flat.NodeId, base_type types.Type, op flat.Op) bool {
	if g.sum_type_name_for_type(base_type) == none {
		return false
	}
	g.write('(')
	g.gen_expr(base_id)
	g.write(')')
	g.write(if op == .arrow || base_type is types.Pointer { '->typ' } else { '.typ' })
	return true
}

fn (mut g FlatGen) gen_sum_shared_field_switch(sum_var string, sum_name string, field string, seen []string) {
	if sum_name in seen {
		return
	}
	variants := g.tc.sum_types[sum_name] or { return }
	mut next_seen := seen.clone()
	next_seen << sum_name
	g.writeln('switch ((${sum_var}).typ) {')
	for variant in variants {
		idx := g.sum_type_index(sum_name, variant)
		sum_field := sum_field_name(variant)
		variant_type := unalias_type(g.tc.parse_canonical_type(variant))
		is_pointer := variant_type is types.Pointer
		op := if is_pointer { '->' } else { '.' }
		if _ := g.struct_field_type(variant, field) {
			g.writeln('case ${idx}: __field = ${sum_var}.${sum_field}${op}${c_field_name(field)}; break;')
		} else if suffix := g.struct_promoted_field_suffix(variant, field, is_pointer) {
			g.writeln('case ${idx}: __field = ${sum_var}.${sum_field}${suffix}; break;')
		} else if nested_sum := g.sum_type_name_for_type(g.tc.parse_type(variant)) {
			value := '${sum_var}.${sum_field}'
			nested_var := if is_pointer { '(*(${value}))' } else { value }
			g.writeln('case ${idx}: {')
			g.gen_sum_shared_field_switch(nested_var, nested_sum, field, next_seen)
			g.writeln('} break;')
		}
	}
	g.writeln('default: break; }')
}

// gen_lowered_sum_init emits lowered sum init output for c.
fn (mut g FlatGen) gen_lowered_sum_init(node flat.Node) bool {
	sum_name := g.lowered_sum_init_name(node)
	if sum_name !in g.tc.sum_types || node.children_count == 0 {
		return false
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		variant := g.lowered_sum_field_variant(sum_name, field) or { continue }
		g.gen_sum_variant_value(types.SumType{ name: sum_name }, variant,
			g.a.child(field, 0))
		return true
	}
	return false
}

fn (g &FlatGen) lowered_sum_field_variant(sum_name string, field &flat.Node) ?string {
	if field.value == 'typ' {
		return none
	}
	child_id := g.a.child(field, 0)
	variant := field.typ
	if variant.len > 0 {
		resolved := g.resolve_variant(sum_name, variant)
		if g.sum_type_index(sum_name, resolved) > 0 {
			return resolved
		}
	}
	if int(child_id) >= 0 {
		actual := g.usable_expr_type(child_id)
		if resolved := g.sum_variant_for_actual(sum_name, actual) {
			return resolved
		}
	}
	for v in g.tc.sum_types[sum_name] {
		if sum_field_name(v) == field.value || c_field_name(v) == field.value || v == field.value {
			return v
		}
	}
	return none
}

// gen_lowered_sum_field_value emits lowered sum field value output for c.
fn (mut g FlatGen) gen_lowered_sum_field_value(sum_name string, field &flat.Node) {
	child := g.a.child(field, 0)
	variant := g.lowered_sum_field_variant(sum_name, field) or {
		g.gen_expr(child)
		return
	}
	g.gen_expr_with_expected_type(child, g.tc.parse_canonical_type(variant))
}

fn (mut g FlatGen) gen_recursive_default_sum_value(sum_type types.SumType, mut seen map[string]bool) {
	sum_name := g.resolve_sum_name(sum_type.name)
	ct := g.value_c_type(types.Type(sum_type))
	if seen[sum_name] {
		g.write('(${ct}){0}')
		return
	}
	variants := g.tc.sum_types[sum_name] or {
		g.write('(${ct}){0}')
		return
	}
	if variants.len == 0 {
		g.write('(${ct}){0}')
		return
	}
	seen[sum_name] = true
	defer {
		seen.delete(sum_name)
	}
	variant := variants[0]
	variant_type := unalias_type(g.tc.parse_type(variant))
	if variant_type is types.Pointer {
		g.write('(${ct}){.typ = ${g.sum_type_index(sum_name, variant)}, .${sum_field_name(variant)} = NULL}')
		return
	}
	g.write('(${ct}){.typ = ${g.sum_type_index(sum_name, variant)}, .${sum_field_name(variant)} = ')
	if variant_type is types.SumType {
		g.gen_recursive_default_sum_value(variant_type, mut seen)
	} else {
		g.shallow_default_value_depth++
		g.gen_default_value_for_clean_type(variant_type)
		g.shallow_default_value_depth--
	}
	g.write('}')
}
