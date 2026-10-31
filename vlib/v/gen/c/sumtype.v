module c

import v.flat
import v.types { unalias_type }
import v.gen.c.naming { sum_field_name, type_name_part }

struct SumUniqueFieldInfo {
	variant u32
	typ     types.Type
}

@[heap]
struct SumVariantActualCache {
mut:
	by_sum map[u32]&SumVariantActualEntries
}

@[heap]
struct SumVariantActualEntries {
mut:
	values map[u32]u32
}

fn (g &FlatGen) sum_has_pointer_variant(sum_name_id u32) bool {
	for variant in g.tc.sum_types.type_ids(g.resolve_sum_name(sum_name_id)) or { []u32{} } {
		if unalias_type(g.tc.parse_canonical_type(variant)) is types.Pointer { return true }
	}
	return false
}

// C++ needs the anonymous union's own braces. Flattening its designator into
// an enclosing aggregate can make GCC 9 crash when this sum is nested again.
// The positional fields follow emit_sum_type; the C representation is unchanged.
fn (g &FlatGen) sum_initializer_prefix(sum_name_id u32, ct string, tag int, field string, owned bool) string {
	if g.ccompiler == 'cplusplus' {
		ownership := if g.sum_has_pointer_variant(sum_name_id) { '${owned}, ' } else { '' }
		return '(${ct}){${tag}, ${ownership}{.${field} = '
	}
	return '(${ct}){.typ = ${tag}, .${field} = '
}

fn (g &FlatGen) sum_initializer_suffix(owned bool) string {
	if g.ccompiler == 'cplusplus' {
		return '}}'
	}
	return if owned { ', ._pointer_variant_is_owned = true}' } else { '}' }
}

// A C array needs a complete element type even behind a pointer. Erase only these
// pointer fields so finite recursive sums such as `int | &[2]Self` remain declarable.
fn sum_pointer_needs_erased_storage(typ &types.Type) bool {
	mut target := unalias_type(typ)
	if target !is types.Pointer { return false }
	for target is types.Pointer {
		target = unalias_type(target.base_type)
	}
	return target is types.ArrayFixed
}

fn (mut g FlatGen) sum_pointer_slot_type(typ &types.Type) string {
	mut target := unalias_type(typ)
	mut depth := 0
	for target is types.Pointer {
		depth++
		target = unalias_type(target.base_type)
	}
	return '__v3_sum_fixed_pointer_${naming.type_name_part(g.value_c_type(target))}${'ptr'.repeat(depth)}'
}

// Typed pointer-slot views preserve an addressable projection for mutable borrows.
// may_alias keeps access to the erased void* slot valid under C alias analysis.
fn (mut g FlatGen) sum_pointer_slot_typedefs() {
	needed := g.collect_fixed_array_typedefs_needed()
	for name, info in needed {
		if g.emitted_fixed_array_typedefs[name] {
			g.sum_pointer_slot_typedefs_for_array(info.arr)
		}
	}
}

fn (mut g FlatGen) sum_pointer_slot_typedefs_for_array(arr types.ArrayFixed) {
	old_module := g.tc.cur_module
	defer { g.tc.cur_module = old_module }
	mut pointers := map[string]types.Type{}
	mut depths := map[string]int{}
	mut modules := map[string]string{}
	mut max_depth := 0
	mut sum_names := g.tc.sum_types.keys()
	sum_names.sort_with_compare(compare_reflection_source_type_ids)
	for registry_name_id in sum_names {
		g.tc.cur_module = module_from_source_type_name(flat.source_type_syntax(registry_name_id).generic_base())
		for variant in g.tc.sum_types.type_ids(registry_name_id) or { []u32{} } {
			typ := g.tc.parse_canonical_type(variant)
			if !sum_pointer_needs_erased_storage(typ) { continue }
			mut target := unalias_type(typ)
			mut depth := 0
			for target is types.Pointer {
				depth++
				target = unalias_type(target.base_type)
			}
			if !cgen_types_equal_after_alias_erasure(target, types.Type(arr)) { continue }
			alias := g.sum_pointer_slot_type(typ)
			pointers[alias] = typ
			depths[alias] = depth
			modules[alias] = g.tc.cur_module
			if depth > max_depth { max_depth = depth }
		}
	}
	mut names := pointers.keys()
	names.sort()
	// A deeper pointer may use the alias for its inner slot. Emit that dependency first.
	for depth in 1 .. max_depth + 1 {
		for alias in names {
			if depths[alias] != depth || g.emitted_fixed_array_typedefs[alias] { continue }
			g.tc.cur_module = modules[alias]
			if !g.cached_support_identifiers[alias] {
				typ := pointers[alias]
				g.writeln('typedef ${g.value_c_type(typ)} ${alias} __attribute__((may_alias));')
			}
			g.emitted_fixed_array_typedefs[alias] = true
		}
	}
}

// Keep the alias on every indirection to an erased slot, including mutable
// parameters and returned slot references; a value-only cast loses may_alias there.
fn (mut g FlatGen) sum_pointer_slot_c_type(typ &types.Type) ?string {
	clean := unalias_type(typ)
	if clean !is types.Pointer { return none }
	base := unalias_type(clean.base_type)
	if !sum_pointer_needs_erased_storage(base) { return none }
	old_module := g.tc.cur_module
	defer { g.tc.cur_module = old_module }
	mut sum_names := g.tc.sum_types.keys()
	sum_names.sort_with_compare(compare_reflection_source_type_ids)
	for registry_name_id in sum_names {
		g.tc.cur_module = module_from_source_type_name(flat.source_type_syntax(registry_name_id).generic_base())
		for variant in g.tc.sum_types.type_ids(registry_name_id) or { []u32{} } {
			candidate := g.tc.parse_canonical_type(variant)
			if cgen_types_equal_after_alias_erasure(base, candidate) {
				return '${g.sum_pointer_slot_type(candidate)}*'
			}
		}
	}
	// A reference to a reference still carries the same annotated pointee slot type.
	if inner := g.sum_pointer_slot_c_type(base) { return '${inner}*' }
	return none
}

fn (mut g FlatGen) sum_pointer_payload_expr(typ &types.Type, raw string) string {
	if !sum_pointer_needs_erased_storage(typ) { return raw }
	return '((${g.value_c_type(typ)})(${raw}))'
}

fn (mut g FlatGen) sum_slot_aware_field_type(typ &types.Type) ?string {
	if slot_type := g.sum_pointer_slot_c_type(typ) { return slot_type }
	clean := unalias_type(typ)
	if clean is types.FnType {
		encoded := g.fn_ptr_type_key(clean)
		if encoded.contains('__v3_sum_fixed_pointer_') { return encoded }
	}
	return none
}

fn (mut g FlatGen) erased_sum_pointer_field_type(base_type &types.Type, field string) ?types.Type {
	sum := g.sum_type_name_for_type(base_type) or { return none }
	old_module := g.tc.cur_module
	g.tc.cur_module = module_from_source_type_name(flat.source_type_syntax(sum).generic_base())
	defer { g.tc.cur_module = old_module }
	for variant in g.tc.sum_types.type_ids(sum) or { []u32{} } {
		if sum_field_name(flat.source_type_name(variant)) != field { continue }
		typ := g.tc.parse_canonical_type(variant)
		if sum_pointer_needs_erased_storage(typ) { return typ }
		return none
	}
	return none
}

fn (mut g FlatGen) gen_erased_sum_pointer_selector(node flat.Node, base_id flat.NodeId, base_type &types.Type, lvalue bool) bool {
	typ := g.erased_sum_pointer_field_type(base_type, node.value) or { return false }
	old_expected := g.expected_expr_type
	old_enum := g.expected_enum
	g.expected_expr_type = cgen_void_type
	g.expected_enum = 0
	defer {
		g.expected_expr_type = old_expected
		g.expected_enum = old_enum
	}
	if lvalue {
		g.write('(*(${g.sum_pointer_slot_type(typ)}*)&(')
	} else {
		g.write('((${g.value_c_type(typ)})(')
	}
	g.write('(')
	g.gen_expr(base_id)
	g.write(')')
	g.write(if node.op == .arrow || unalias_type(base_type) is types.Pointer { '->' } else { '.' })
	g.write(node.value)
	g.write('))')
	return true
}

// emit_sum_type emits emit sum type output for c.
fn (mut g FlatGen) emit_sum_type(name_id u32) {
	g.writeln('struct ${g.cname(flat.source_type_name(name_id))} {')
	has_owned_flag := g.sum_has_pointer_variant(name_id)
	variants := g.tc.sum_types.type_ids(name_id) or { []u32{} }
	// An `int` tag alone leaves padding before a union that holds pointers, and the C
	// compiler may copy that padding together with the union, which puts the pointers
	// at offsets that are not a multiple of the pointer size, where the conservative GC
	// does not look for them (see write_optional_typedef). A pointer sized tag fills
	// the padding without changing the layout. The ownership flag already fills it.
	tag_type := if !has_owned_flag && g.sum_variants_hold_aligned_pointer(variants, 0) {
		'intptr_t'
	} else {
		'int'
	}
	g.writeln('\t${tag_type} typ;')
	if has_owned_flag {
		g.writeln('\tu32 _pointer_variant_is_owned;')
	}
	g.writeln('\tunion {')
	for variant in variants {
		typ := g.tc.parse_canonical_type(variant)
		ct := if sum_pointer_needs_erased_storage(typ) { 'void*' } else { g.value_c_type(typ) }
		field := sum_field_name(flat.source_type_name(variant))
		g.writeln('\t\t${ct} ${field};')
	}
	g.writeln('\t};')
	g.writeln('};')
	g.writeln('')
}

// sum_variants_hold_aligned_pointer reports whether a sum's union, made of the
// `variants`, holds a pointer at a pointer aligned offset.
fn (mut g FlatGen) sum_variants_hold_aligned_pointer(variants []u32, depth int) bool {
	for variant in variants {
		typ := g.tc.parse_canonical_type(variant)
		if sum_pointer_needs_erased_storage(typ) || g.type_holds_aligned_pointer(typ, depth) {
			return true
		}
	}
	return false
}

fn (mut g FlatGen) gen_sum_variant_pointer_cast(id flat.NodeId, target types.Pointer, ct string) bool {
	source_type := unalias_type(g.sum_cast_actual_type(id))
	if source_type !is types.Pointer { return false }
	source_base := unalias_type(source_type.base_type)
	if source_base !is types.SumType { return false }
	target_base := unalias_type(target.base_type)
	sum_name := g.resolve_sum_name(source_base.name_id)
	variants := g.tc.sum_types.type_ids(sum_name) or { return false }
	variant := g.resolve_variant(sum_name, target_base.source_type_id())
	if variant !in variants { return false }
	sum_ct := g.tc.c_type(types.Type(source_base))
	field := sum_field_name(flat.source_type_name(variant))
	variant_type := g.tc.parse_canonical_type(variant)
	if sum_pointer_needs_erased_storage(variant_type) {
		g.write('(${ct})&(*(${g.sum_pointer_slot_type(variant_type)}*)&(((${sum_ct}*)(')
		g.gen_expr(id)
		g.write('))->${field}))')
		return true
	}
	g.write('(${ct})&((${sum_ct}*)(')
	g.gen_expr(id)
	g.write('))->${field}')
	return true
}

// gen_sum_value_expr emits sum value expr output for c.
fn (mut g FlatGen) gen_sum_value_expr(id flat.NodeId, expected &types.Type) bool {
	sum_type := g.sum_type_for_expected_value(expected) or { return false }
	sum_type0 := types.Type(sum_type)
	raw_actual0 := g.sum_cast_actual_type(id)
	raw_actual_type := unalias_type(raw_actual0)
	if raw_actual_type is types.SumType {
		// A sum type can itself be a variant of a wider sum type (for example
		// `ast.Stmt` inside `ast.Node`). Only skip wrapping when the value is
		// already the expected sum.
		if g.type_names_match(raw_actual_type, sum_type0) || g.resolve_sum_name(raw_actual_type.name_id) == g.resolve_sum_name(sum_type.name_id) || (!g.tc.sum_types.contains(raw_actual_type.source_type_id()) && flat.source_type_syntax(raw_actual_type.source_type_id()).unqualified() == flat.source_type_syntax(sum_type.source_type_id()).unqualified()) {
			return false
		}
	}
	if declared := g.selector_declared_type(id) {
		declared0 := unalias_type(declared)
		if declared0 is types.SumType && g.type_names_match(declared0, sum_type0) {
			return false
		}
	}
	sum_name := g.resolve_sum_name(sum_type.name_id)
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
		resolved := g.resolve_sum_name(clean.name_id)
		if g.tc.sum_types.contains(resolved) {
			return types.SumType{
				name_id: resolved
			}
		}
	}
	return none
}

fn (g &FlatGen) sum_variant_for_actual(sum_name0_id u32, actual &types.Type) ?u32 {
	sum_id := g.resolve_sum_name(sum_name0_id)
	actual_id := actual.source_type_id()
	if sum_cache := g.sum_variant_actual_cache.by_sum[sum_id] {
		if cached := sum_cache.values[actual_id] {
			if cached != 0 { return cached }
			return none
		}
	}
	result := g.sum_variant_for_actual_uncached(sum_id, actual, actual_id) or {
		mut cache := g.sum_variant_actual_cache
		mut sum_cache := cache.by_sum[sum_id] or {
			created := &SumVariantActualEntries{}
			cache.by_sum[sum_id] = created
			created
		}
		sum_cache.values[actual_id] = 0
		return none
	}
	mut cache := g.sum_variant_actual_cache
	mut sum_cache := cache.by_sum[sum_id] or {
		created := &SumVariantActualEntries{}
		cache.by_sum[sum_id] = created
		created
	}
	sum_cache.values[actual_id] = result
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
	variant := g.sum_variant_for_actual(target.name_id, actual) or {
		g.resolve_variant(target.name_id, types.unwrap_pointer(actual).source_type_id())
	}
	g.gen_sum_variant_value(target, variant, value)
}

fn (mut g FlatGen) gen_sum_variant_value(sum types.SumType, variant u32, value flat.NodeId) {
	ct := g.value_c_type(types.Type(sum))
	idx := g.sum_type_index(sum.name_id, variant)
	field := sum_field_name(flat.source_type_name(variant))
	typ := g.tc.parse_canonical_type(variant)
	if fixed := array_fixed_type(typ) {
		tmp := g.tmp_name()
		g.write('({ ${ct} ${tmp} = {.typ = ${idx}}; memcpy(${tmp}.${field}, ')
		g.gen_fixed_array_copy_source(value, fixed)
		g.write(', sizeof(${tmp}.${field})); ${tmp}; })')
		return
	}
	clean_type := unalias_type(typ)
	owned := clean_type is types.Pointer && g.pointer_variant_expr_creates_owned_value(value)
	g.write(g.sum_initializer_prefix(sum.name_id, ct, idx, field, owned))
	g.gen_expr_with_expected_type(value, typ)
	g.write(g.sum_initializer_suffix(owned))
}

fn (g &FlatGen) sum_unique_variant_field_info(base_type0 &types.Type, field string) ?SumUniqueFieldInfo {
	sum_name := g.sum_type_name_for_type(base_type0) or { return none }
	variants := g.tc.sum_types.type_ids(sum_name) or { return none }
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
	sum_field := sum_field_name(flat.source_type_name(info.variant))
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
	g.gen_sum_shared_field_switch('(*__sum)', sum_name, field, []u32{})
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

fn (mut g FlatGen) gen_sum_shared_field_switch(sum_var string, sum_name u32, field string, seen []u32) {
	if sum_name in seen {
		return
	}
	variants := g.tc.sum_types.type_ids(sum_name) or { return }
	mut next_seen := seen.clone()
	next_seen << sum_name
	g.writeln('switch ((${sum_var}).typ) {')
	for variant in variants {
		idx := g.sum_type_index(sum_name, variant)
		sum_field := sum_field_name(flat.source_type_name(variant))
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
	if !g.tc.sum_types.contains(sum_name) || node.children_count == 0 {
		return false
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		variant := g.lowered_sum_field_variant(sum_name, field) or { continue }
		g.gen_sum_variant_value(types.SumType{ name_id: sum_name }, variant,
			g.a.child(field, 0))
		return true
	}
	return false
}

fn (g &FlatGen) lowered_sum_field_variant(sum_name u32, field &flat.Node) ?u32 {
	if field.value == 'typ' {
		return none
	}
	child_id := g.a.child(field, 0)
	variant := field.typ
	if variant != 0 {
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
	for v in g.tc.sum_types.type_ids(sum_name) or { []u32{} } {
		if sum_field_name(flat.source_type_name(v)) == field.value || c_field_name(flat.source_type_name(v)) == field.value || flat.source_type_name(v) == field.value {
			return v
		}
	}
	return none
}

// gen_lowered_sum_field_value emits lowered sum field value output for c.
fn (mut g FlatGen) gen_lowered_sum_field_value(sum_name u32, field &flat.Node) {
	child := g.a.child(field, 0)
	variant := g.lowered_sum_field_variant(sum_name, field) or {
		g.gen_expr(child)
		return
	}
	g.gen_expr_with_expected_type(child, g.tc.parse_canonical_type(variant))
}

fn (mut g FlatGen) gen_recursive_default_sum_value(sum_type types.SumType, mut seen map[u32]bool) {
	sum_name := g.resolve_sum_name(sum_type.name_id)
	ct := g.value_c_type(types.Type(sum_type))
	if seen[sum_name] {
		g.write('(${ct}){0}')
		return
	}
	variants := g.tc.sum_types.type_ids(sum_name) or {
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
	variant_field := string(sum_field_name(flat.source_type_name(variant)))
	g.write(g.sum_initializer_prefix(sum_name, ct, g.sum_type_index(sum_name, variant), variant_field, false))
	if variant_type is types.Pointer {
		g.write('NULL')
		g.write(g.sum_initializer_suffix(false))
		return
	}
	if variant_type is types.SumType {
		g.gen_recursive_default_sum_value(variant_type, mut seen)
	} else {
		g.shallow_default_value_depth++
		g.gen_default_value_for_clean_type(variant_type)
		g.shallow_default_value_depth--
	}
	g.write(g.sum_initializer_suffix(false))
}
