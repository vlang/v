module transform

import v.flat
import v.types

struct FixedArraySumLiteral {
	variant u32
	expr    flat.NodeId
}

// trim_pointer_type transforms trim pointer type data for transform.
fn (t &Transformer) trim_pointer_type(typ_id u32) u32 {
	if !flat.source_type_shape(typ_id).has(.pointer) { return typ_id }
	if !isnil(t.alias_cache) {
		if base := t.alias_cache.pointer_bases[typ_id] { return base }
	}
	typ := flat.source_type_name(typ_id)
	base := flat.source_type_id(typ[1..])
	if !isnil(t.alias_cache) {
		mut cache := t.alias_cache
		cache.pointer_bases[typ_id] = base
	}
	return base
}

fn (t &Transformer) trim_all_pointer_type(typ_id u32) u32 {
	mut clean := typ_id
	for flat.source_type_shape(clean).has(.pointer) { clean = t.trim_pointer_type(clean) }
	return clean
}

fn interface_pattern_is_collapsed_container_type(name string) bool {
	clean := trimmed_transform_text(name)
	return clean.starts_with('[]') || clean.starts_with('map[')
}

fn (mut t Transformer) pointer_sum_access_expr(expr_id flat.NodeId, expr_type_id u32) (flat.NodeId, u32, flat.Op) {
	expr_type := flat.source_type_name(expr_type_id)
	has_smartcast := t.expr_has_smartcast(expr_id)
	mut access := if has_smartcast {
		// A runtime tag check always starts from the stored sum/interface value.
		// Applying an active smartcast here would inspect the extracted variant.
		t.make_plain_expr_for_smartcast(expr_id)
	} else {
		t.transform_selector_base_expr(expr_id)
	}
	mut access_type := expr_type
	if has_smartcast {
		raw_type_source_id := t.raw_expr_type_without_smartcast(expr_id)
		raw_type := flat.source_type_name(raw_type_source_id)
		if raw_type_source_id != 0 {
			access_type = raw_type
		}
	}
	for access_type.starts_with('&&') {
		access = t.make_prefix(.mul, access)
		access_type = access_type[1..]
		t.set_node_typ(int(access), flat.source_type_id(access_type))
	}
	mut value_type := if has_smartcast {
		access_type
	} else {
		flat.source_type_name(t.node_type(access))
	}
	if value_type.len == 0 {
		value_type = access_type
	}
	op := if value_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	return access, flat.source_type_id(value_type), op
}

// resolve_variant resolves resolve variant information for transform.
fn (t &Transformer) resolve_variant(sum_name_id u32, variant_id u32) u32 {
	sum_name := flat.source_type_name(sum_name_id)
	variant := flat.source_type_name(variant_id)
	for candidate_source_id in t.sum_subject_type_candidates(sum_name_id) {
		if resolved_source_id := t.generic_sum_arg_variant_for_pattern(candidate_source_id, variant_id) {
			return resolved_source_id
		}
		if resolved_variant_source_id := t.sum_variant_name(candidate_source_id, variant_id) {
			return resolved_variant_source_id
		}
	}
	if !isnil(t.tc) {
		if resolved_source_id := t.tc.sum_variant_type_for_pattern(sum_name_id, variant_id) {
			resolved_source_id_2 := resolved_source_id

			return resolved_source_id_2
		}
	}
	if variant.contains('.') {
		return variant_id
	}
	resolved_sum_source_id := t.resolve_sum_name(sum_name_id)
	resolved_sum := flat.source_type_name(resolved_sum_source_id)
	if resolved_variant_source_id := t.sum_variant_name(resolved_sum_source_id, variant_id) {
		return resolved_variant_source_id
	}
	if resolved_sum.contains('.') {
		return flat.source_type_id('${resolved_sum.all_before_last('.')}.${variant}')
	}
	if sum_name.contains('.') {
		return flat.source_type_id('${sum_name.all_before_last('.')}.${variant}')
	}
	return variant_id
}

fn (t &Transformer) generic_sum_arg_variant_for_pattern(sum_name_id u32, variant_id u32) ?u32 {
	sum_name := flat.source_type_name(sum_name_id)
	variant := flat.source_type_name(variant_id)
	base, args, ok := generic_app_parts(sum_name)
	if !ok {
		return none
	}
	params := t.sum_generic_params_for_base(base)
	if params.len == args.len {
		concrete_pattern := substitute_generic_type_text_with_params(variant, flat.TypeTextList.from_names(args).ids, params)
		for concrete_variant_source_id in t.concrete_sum_variants_for_candidate(sum_name_id) {
			if t.variant_names_match(concrete_variant_source_id, flat.source_type_id(concrete_pattern))
				|| t.variant_names_match(concrete_variant_source_id, variant_id) {
				return concrete_variant_source_id
			}
		}
		if concrete_pattern != variant {
			return flat.source_type_id(concrete_pattern)
		}
	}
	_, variant_args, variant_is_generic := generic_app_parts(variant)
	if variant_is_generic && variant_args.len == args.len
		&& t.generic_variant_args_are_open(variant_args) {
		concrete_pattern := substitute_generic_type_text_with_params(variant, flat.TypeTextList.from_names(args).ids, variant_args)
		for concrete_variant_source_id_2 in t.concrete_sum_variants_for_candidate(sum_name_id) {
			if t.variant_names_match(concrete_variant_source_id_2, flat.source_type_id(concrete_pattern))
				|| t.concrete_variant_matches_generic_pattern(concrete_variant_source_id_2, flat.source_type_id(concrete_pattern)) {
				return concrete_variant_source_id_2
			}
		}
		if concrete_pattern != variant {
			return flat.source_type_id(concrete_pattern)
		}
	}
	for arg in args {
		if t.variant_names_match(flat.source_type_id(arg), variant_id) {
			return flat.source_type_id(arg)
		}
	}
	return none
}

// resolve_sum_name resolves resolve sum name information for transform.
fn (t &Transformer) resolve_sum_name(type_id u32) u32 {
	if type_id == 0 { return 0 }
	if isnil(t.sum_cache) {
		return flat.source_type_id(t.resolve_sum_name_uncached(flat.source_type_name(type_id)))
	}
	mut c := t.sum_cache
	if !same_transform_text(c.module, t.cur_module) {
		c.module = t.cur_module
		c.entries.clear()
		c.clear_recent()
	}
	slot := int(type_id & 1023)
	if c.recent_generations[slot] == c.recent_generation && c.recent_types[slot] == type_id {
		return c.recent_results[slot]
	}
	if cached := c.entries[type_id] {
		c.put_recent(type_id, cached)
		return cached
	}
	result := flat.source_type_id(t.resolve_sum_name_uncached(flat.source_type_name(type_id)))
	c.entries[type_id] = result
	c.put_recent(type_id, result)
	return result
}

// resolve_sum_name_uncached resolves resolve sum name uncached information for transform.
fn (t &Transformer) resolve_sum_name_uncached(sum_name string) string {
	if t.sum_types.contains(sum_name) {
		return sum_name
	}
	// Do not let a short/suffix fallback reinterpret an exact concrete type as an
	// unrelated same-named sum type from another module.
	if sum_name in t.structs || (!isnil(t.tc) && (sum_name in t.tc.structs
		|| sum_name in t.tc.interface_names || sum_name in t.tc.enum_names)) {
		return ''
	}
	if resolved_c_name_source_id := t.resolve_sum_name_from_c_name(sum_name) {
		resolved_c_name := flat.source_type_name(resolved_c_name_source_id)

		return resolved_c_name
	}
	// A container or function involving a sum type is not itself a sum type: `[]ast.Value` must
	// not resolve to `ast.Value` (the short-name and generic-application
	// fallbacks below would), or an or/assign lowering boxes the whole array
	// into one sum value.
	if sum_name.starts_with('[]') || sum_name.starts_with('map[') || sum_name.starts_with('[')
		|| sum_name.starts_with('fn(') || sum_name.starts_with('fn (') {
		return ''
	}
	if !isnil(t.tc) && t.tc.sum_types.contains(sum_name) {
		return sum_name
	}
	generic_base, generic_args, is_generic := generic_app_parts(sum_name)
	if is_generic {
		concrete_sum := t.resolve_concrete_generic_sum_name(sum_name, generic_base, generic_args)
		if concrete_sum.len > 0 {
			return concrete_sum
		}
		resolved_base := t.resolve_sum_name_uncached(generic_base)
		if t.sum_types.contains(resolved_base) {
			return resolved_base
		}
		if !isnil(t.tc) && t.tc.sum_types.contains(resolved_base) {
			return resolved_base
		}
	}
	has_dot := sum_name.contains('.')
	if has_dot {
		// Import-aliased module path: `tast.Value` names `sub.tast.Value`.
		// The full-suffix match runs before the bare short-name fallback so an
		// unrelated short `Value` sum cannot shadow the aliased one; ambiguous
		// suffix matches resolve nothing.
		suffix := '.' + sum_name
		mut suffix_match := ''
		mut suffix_ambiguous := false
		for key, _ in t.sum_types.ids {
			if key.ends_with(suffix) {
				if suffix_match != '' && suffix_match != key {
					suffix_ambiguous = true
					break
				}
				suffix_match = key
			}
		}
		if !suffix_ambiguous && !isnil(t.tc) {
			for key, _ in t.tc.sum_types.ids {
				if key.ends_with(suffix) {
					if suffix_match.len > 0 && suffix_match != key {
						suffix_ambiguous = true
						break
					}
					suffix_match = key
				}
			}
		}
		if !suffix_ambiguous && suffix_match.len > 0 {
			return suffix_match
		}
		short_sum := short_name_view(sum_name)
		if t.sum_types.contains(short_sum) {
			return short_sum
		}
	}
	if !has_dot && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qsum := '${t.cur_module}.${sum_name}'
		if t.sum_types.contains(qsum) {
			return qsum
		}
	}
	if !has_dot {
		mut found := ''
		for key, _ in t.sum_types.ids {
			if key.contains('.') && short_name_view(key) == sum_name {
				if found != '' && found != key {
					found = ''
					break
				}
				found = key
			}
		}
		if found.len > 0 {
			return found
		}
	}
	if !isnil(t.tc) {
		if t.tc.sum_types.contains(sum_name) {
			return sum_name
		}
		if has_dot {
			short_sum := short_name_view(sum_name)
			if t.tc.sum_types.contains(short_sum) {
				return short_sum
			}
		}
		if !has_dot && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qsum := '${t.cur_module}.${sum_name}'
			if t.tc.sum_types.contains(qsum) {
				return qsum
			}
		}
		if !has_dot {
			mut found := ''
			for key, _ in t.tc.sum_types.ids {
				if key.contains('.') && short_name_view(key) == sum_name {
					if found != '' && found != key {
						found = ''
						break
					}
					found = key
				}
			}
			if found.len > 0 {
				return found
			}
		}
	}
	return sum_name
}

fn (t &Transformer) resolve_sum_name_from_c_name(sum_name string) ?u32 {
	if sum_name.contains('[') || !sum_name.contains('_') {
		return none
	}
	for key, _ in t.sum_types.ids {
		if key.contains('[') && c_name(key) == sum_name {
			return flat.source_type_id(key)
		}
	}
	if !isnil(t.tc) {
		for key, _ in t.tc.sum_types.ids {
			if key.contains('[') && c_name(key) == sum_name {
				return flat.source_type_id(key)
			}
		}
	}
	return none
}

fn (t &Transformer) resolve_concrete_generic_sum_name(sum_name string, base string, args []string) string {
	args_text := args.join(', ')
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qspec := '${t.cur_module}.${base}[${args_text}]'
		if t.sum_types.contains(qspec) || (!isnil(t.tc) && t.tc.sum_types.contains(qspec)) {
			return qspec
		}
	}
	if !sum_name.contains('.') {
		short_spec_source_id := t.find_sum_type_with_short_name(flat.source_type_id(sum_name))
		short_spec := flat.source_type_name(short_spec_source_id)
		if short_spec_source_id != 0 {
			return short_spec
		}
	}
	resolved_base := t.resolve_sum_name_uncached(base)
	if resolved_base.len > 0 && resolved_base != base {
		resolved_spec := '${resolved_base}[${args_text}]'
		if t.sum_types.contains(resolved_spec) || (!isnil(t.tc) && t.tc.sum_types.contains(resolved_spec)) {
			return resolved_spec
		}
	}
	return ''
}

fn (t &Transformer) find_sum_type_with_short_name(short_name_id u32) u32 {
	short_name := flat.source_type_name(short_name_id)
	mut found := ''
	for key, _ in t.sum_types.ids {
		if key.contains('.') && key.all_after_last('.') == short_name {
			if found != '' && found != key {
				return u32(0)
			}
			found = key
		}
	}
	if !isnil(t.tc) {
		for key, _ in t.tc.sum_types.ids {
			if key.contains('.') && key.all_after_last('.') == short_name {
				if found.len > 0 && found != key {
					return u32(0)
				}
				found = key
			}
		}
	}
	return flat.source_type_id(found)
}

fn (t &Transformer) sum_subject_type_candidates(subject_type_id u32) []u32 {
	clean_id := t.trimmed_type_id(t.trim_pointer_type(t.normalize_type_alias(subject_type_id)))
	if clean_id == 0 { return []u32{} }
	clean := flat.source_type_name(clean_id)
	mut candidates := []u32{}
	mut seen := map[u32]bool{}
	base, args, is_generic := generic_app_parts(clean)
	if is_generic {
		args_text := args.join(', ')
		scoped_args := t.sum_subject_type_args_in_scope(flat.TypeTextList.from_names(args).ids)
		scoped_args_text := flat.TypeTextList{ ids: scoped_args }.names().join(', ')
		if scoped_args_text != args_text {
			push_sum_subject_type_candidate(mut candidates, mut seen, flat.source_type_id('${base}[${scoped_args_text}]'))
		}
		push_sum_subject_type_candidate(mut candidates, mut seen, clean_id)
		if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			push_sum_subject_type_candidate(mut candidates, mut seen, flat.source_type_id('${t.cur_module}.${base}[${args_text}]'))
			if scoped_args_text != args_text {
				push_sum_subject_type_candidate(mut candidates, mut seen, flat.source_type_id('${t.cur_module}.${base}[${scoped_args_text}]'))
			}
		}
		resolved_id := t.resolve_sum_name(flat.source_type_id(base))
		if resolved_id != 0 && flat.source_type_name(resolved_id) != base {
			resolved := flat.source_type_name(resolved_id)
			push_sum_subject_type_candidate(mut candidates, mut seen, flat.source_type_id('${resolved}[${args_text}]'))
			if scoped_args_text != args_text {
				push_sum_subject_type_candidate(mut candidates, mut seen, flat.source_type_id('${resolved}[${scoped_args_text}]'))
			}
		}
	} else {
		push_sum_subject_type_candidate(mut candidates, mut seen, clean_id)
		if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			push_sum_subject_type_candidate(mut candidates, mut seen, flat.source_type_id('${t.cur_module}.${clean}'))
		}
	}
	push_sum_subject_type_candidate(mut candidates, mut seen, t.resolve_sum_name(clean_id))
	return candidates
}

fn (t &Transformer) sum_subject_type_args_in_scope(args_ids []u32) []u32 {
	mut scoped := []u32{cap: args_ids.len}
	for arg_source_id in args_ids {
		scoped << t.sum_subject_type_arg_in_scope(arg_source_id)
	}
	return scoped.clone()
}

fn (t &Transformer) sum_subject_type_arg_in_scope(arg_id u32) u32 {
	arg := flat.source_type_name(arg_id)
	clean := trimmed_transform_text(arg)
	if clean.len == 0 {
		return flat.source_type_id(clean)
	}
	if clean.starts_with('&') {
		return flat.source_type_id('&' + flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[1..]))))
	}
	if clean.starts_with('mut ') {
		return flat.source_type_id('&' + flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[4..]))))
	}
	if clean.starts_with('?') {
		return flat.source_type_id('?' + flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[1..]))))
	}
	if clean.starts_with('!') {
		return flat.source_type_id('!' + flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[1..]))))
	}
	if clean.starts_with('...') {
		return flat.source_type_id('...' + flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[3..]))))
	}
	if clean.starts_with('[]') {
		return flat.source_type_id('[]' + flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[2..]))))
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[4..bracket_end])))
			value := flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[bracket_end + 1..])))
			return flat.source_type_id('map[${key}]${value}')
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return flat.source_type_id(clean[..bracket_end + 1] + flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(clean[bracket_end +
				1..]))))
		}
	}
	base, nested_args, ok := generic_app_parts(clean)
	if ok {
		mut scoped_args := []string{cap: nested_args.len}
		for nested in nested_args {
			scoped_args << flat.source_type_name(t.sum_subject_type_arg_in_scope(flat.source_type_id(nested)))
		}
		qbase := flat.source_type_name(t.sum_subject_type_arg_base_in_scope(flat.source_type_id(base)))
		return flat.source_type_id('${qbase}[${scoped_args.join(', ')}]')
	}
	return t.sum_subject_type_arg_base_in_scope(flat.source_type_id(clean))
}

fn (t &Transformer) sum_subject_type_arg_base_in_scope(name_id u32) u32 {
	name := flat.source_type_name(name_id)
	if name.contains('.') || isnil(t.tc) {
		return t.normalize_sum_variant_type(name_id, t.cur_module, [])
	}
	for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
		[]string{}
	} {
		if t.tc.type_aliases.contains(candidate) || candidate in t.tc.structs
			|| candidate in t.tc.interface_names || candidate in t.tc.flag_enums
			|| candidate in t.tc.enum_names || t.tc.sum_types.contains(candidate) {
			return flat.source_type_id(candidate)
		}
	}
	return t.normalize_sum_variant_type(name_id, t.cur_module, [])
}

fn push_sum_subject_type_candidate(mut candidates []u32, mut seen map[u32]bool, candidate u32) {
	if candidate == 0 || seen[candidate] { return }
	seen[candidate] = true
	candidates << candidate
}

// is_sum_type_name reports whether is sum type name applies in transform.
fn (t &Transformer) is_sum_type_name(name_id u32) bool {
	if name_id == u32(0) {
		return false
	}
	resolved := flat.source_type_name(t.resolve_sum_name(name_id))
	return t.sum_types.contains(resolved)
}

fn (t &Transformer) sum_target_accepts_variant_type(target_type_id u32, variant_type_id u32) bool {
	if target_type_id == u32(0) || variant_type_id == u32(0) {
		return false
	}
	resolved_raw_target := flat.source_type_name(t.resolve_sum_name(t.trim_pointer_type(target_type_id)))
	if t.sum_types.contains(resolved_raw_target) {
		for variant_source_id_3 in t.sum_types.type_ids(resolved_raw_target) or { []u32{} } {
			variant_source_id_4 := variant_source_id_3

			if t.variant_names_match(variant_source_id_4, t.trim_pointer_type(variant_type_id)) {
				return true
			}
			if t.sum_variant_type_accepts_value_type(variant_source_id_4, variant_type_id) {
				return true
			}
		}
	}
	clean_target_source_id := t.trim_pointer_type(t.normalize_type_alias(target_type_id))
	clean_target_comparison_type_id := clean_target_source_id

	clean_variant_source_id := t.trim_pointer_type(t.normalize_type_alias(variant_type_id))

	if clean_target_source_id == 0 || clean_variant_source_id == 0 {
		return false
	}
	resolved_target_source_id := t.resolve_sum_name(clean_target_source_id)
	resolved_target_comparison_type_id := resolved_target_source_id
	resolved_target := flat.source_type_name(resolved_target_comparison_type_id)
	if resolved_target_source_id == 0 || !t.sum_types.contains(resolved_target) {
		return false
	}
	if flat.source_type_name(t.resolve_sum_name(clean_variant_source_id)) == resolved_target {
		return true
	}
	if _ := t.resolve_sum_variant_pattern_for_subject(clean_target_comparison_type_id, clean_variant_source_id) {
		return true
	}
	if clean_target_comparison_type_id != resolved_target_comparison_type_id {
		if _ := t.resolve_sum_variant_pattern_for_subject(resolved_target_comparison_type_id, clean_variant_source_id) {
			return true
		}
	}
	variant_sum_comparison_type_id := t.resolve_sum_name(t.find_sum_type_for_variant(clean_variant_source_id))

	return variant_sum_comparison_type_id == resolved_target_comparison_type_id
}

fn (t &Transformer) sum_variant_type_accepts_value_type(variant_type_id u32, value_type_id u32) bool {
	mut clean_variant_type := flat.source_type_name(t.trim_pointer_type(t.normalize_type_alias(variant_type_id)))
	mut clean_value_type := flat.source_type_name(t.trim_pointer_type(t.normalize_type_alias(value_type_id)))
	if t.variant_names_match(flat.source_type_id(clean_variant_type), flat.source_type_id(clean_value_type)) {
		return true
	}
	if clean_variant_type.starts_with('[]') && clean_value_type.starts_with('[]') {
		clean_variant_type = clean_variant_type[2..]
		clean_value_type = clean_value_type[2..]
		// Keep module-qualified recursive sum element types distinct. `[]a.Any`
		// and `[]b.Any` are not interchangeable merely because both elements have
		// the short name `Any`; only accept them when the target sum really accepts
		// the source element type.
		if t.is_sum_type_name(flat.source_type_id(clean_variant_type)) {
			return t.sum_target_accepts_variant_type(flat.source_type_id(clean_variant_type), flat.source_type_id(clean_value_type))
		}
		return t.variant_names_match(flat.source_type_id(clean_variant_type), flat.source_type_id(clean_value_type))
	}
	return false
}

// is_interface_type_name reports whether is interface type name applies in transform.
fn (t &Transformer) is_interface_type_name(name_id u32) bool {
	if name_id == u32(0) || isnil(t.tc) {
		return false
	}
	return (t.resolve_interface_type_name(name_id)) != 0
}

// interface_variant_type supports interface variant type handling for Transformer.
fn (t &Transformer) interface_variant_type(variant_id u32) u32 {
	variant := flat.source_type_name(variant_id)
	if variant.contains('.') {
		return variant_id
	}
	if variant in t.structs {
		return variant_id
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qvariant := '${t.cur_module}.${variant}'
		if qvariant in t.structs {
			return flat.source_type_id(qvariant)
		}
	}
	for name, _ in t.structs {
		if name.contains('.') && name.all_after_last('.') == variant {
			return flat.source_type_id(name)
		}
	}
	return variant_id
}

// smartcast_target_type supports smartcast target type handling for Transformer.
fn (t &Transformer) smartcast_target_type(sc SmartcastContext) u32 {
	if sc.sum_type_name_text() == option_unwrap_marker {
		return sc.variant_name
	}
	if flat.source_type_shape(sc.variant_name).has(.pointer) {
		return flat.source_type_id('&' + flat.source_type_name(t.resolve_variant(sc.sum_type_name, flat.source_type_id(sc.variant_name_text()[1..]))))
	}
	if t.is_interface_type_name(sc.sum_type_name) {
		return t.interface_variant_type(sc.variant_name)
	}
	return t.resolve_variant(sc.sum_type_name, sc.variant_name)
}

// sum_type_index supports sum type index handling for Transformer.
fn (t &Transformer) sum_type_index(sum_name_id u32, variant_id u32) int {
	variants := t.sum_type_variants_for_index(sum_name_id)
	if variants.len == 0 {
		return 0
	}
	concrete_variant := if t.active_specialization_args.ids.len > 0 {
		t.subst_type(variant_id, t.active_specialization_args.ids)
	} else {
		variant_id
	}
	return t.sum_type_index_in_variants(variants, concrete_variant)
}

fn (t &Transformer) sum_type_variants_for_index(sum_name_id u32) []u32 {
	sum_name := flat.source_type_name(sum_name_id)
	// A bare name the checker registered is the storage name of a main or builtin sum
	// type, also inside an imported module's specialization. The candidate lookup below
	// would rebind it to a same-named sum of the current module (`Any` -> `json2.Any`)
	// and tag the value with that sum's variant indices.
	if !isnil(t.tc) && t.cur_module !in ['', 'main', 'builtin'] && !sum_name.contains('.')
		&& t.tc.sum_types.contains(sum_name) && '${t.cur_module}.${sum_name}' in t.tc.sum_types.ids {
		if variants := t.sum_types.type_ids(sum_name) {
			return variants.clone()
		}
	}
	_, variants_source_ids := t.concrete_sum_name_and_variants(sum_name_id)

	return variants_source_ids
}

fn (t &Transformer) concrete_sum_name_and_variants(sum_name_id u32) (u32, []u32) {
	for candidate in t.sum_subject_type_candidates(sum_name_id) {
		variants := t.concrete_sum_variants_for_candidate(candidate)
		if variants.len > 0 { return candidate, variants }
	}
	resolved := t.resolve_sum_name(sum_name_id)
	name := flat.source_type_name(resolved)
	if variants := t.sum_types.type_ids(name) { return resolved, variants.clone() }
	if !isnil(t.tc) {
		if variants := t.tc.sum_types.type_ids(name) { return resolved, variants.clone() }
	}
	return resolved, []u32{}
}

fn (t &Transformer) concrete_sum_variants_for_candidate(sum_name_id u32) []u32 {
	name := flat.source_type_name(sum_name_id)
	base, args, is_generic := generic_app_parts(name)
	if variants := t.sum_types.type_ids(name) {
		if is_generic {
			params := t.sum_generic_params_for_base(base)
			if params.len == args.len && params.len > 0 {
				return substitute_sum_variant_list(variants, flat.TypeTextList.from_names(args).ids, params)
			}
		}
		return variants.clone()
	}
	if !isnil(t.tc) {
		if variants := t.tc.sum_types.type_ids(name) {
			if is_generic {
				params := t.sum_generic_params_for_base(base)
				if params.len == args.len && params.len > 0 {
					return substitute_sum_variant_list(variants, flat.TypeTextList.from_names(args).ids, params)
				}
			}
			return variants.clone()
		}
	}
	if !is_generic { return []u32{} }
	for candidate in t.sum_subject_type_candidates(flat.source_type_id(base)) {
		candidate_name := flat.source_type_name(candidate)
		params := t.sum_generic_params_for_base(candidate_name)
		if params.len == 0 || params.len != args.len { continue }
		if variants := t.sum_types.type_ids(candidate_name) {
			return substitute_sum_variant_list(variants, flat.TypeTextList.from_names(args).ids, params)
		}
		if !isnil(t.tc) {
			if variants := t.tc.sum_types.type_ids(candidate_name) {
				return substitute_sum_variant_list(variants, flat.TypeTextList.from_names(args).ids, params)
			}
		}
	}
	return []u32{}
}

fn (t &Transformer) sum_generic_params_for_base(base string) []string {
	if isnil(t.tc) {
		return []string{}
	}
	if params := t.tc.sum_generic_params[base] {
		return params
	}
	short := base.all_after_last('.')
	if params := t.tc.sum_generic_params[short] {
		return params
	}
	return []string{}
}

fn substitute_sum_variant_list(variants []u32, args []u32, params []string) []u32 {
	mut concrete := []u32{cap: variants.len}
	for variant in variants {
		concrete << flat.source_type_id(substitute_generic_type_text_with_params(flat.source_type_name(variant), args, params))
	}
	return concrete
}

// sum_type_index_in_variants supports sum type index in variants handling for transform.
fn (t &Transformer) sum_type_index_in_variants(variants_ids []u32, variant_id u32) int {
	for i, variant in variants_ids {
		if t.variant_names_match(variant, variant_id) { return i + 1 }
	}
	return 0
}

// transform_is_expr transforms transform is expr data for transform.
fn (mut t Transformer) transform_is_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	if check := t.smartcasted_sum_is_expr_check(expr_id, flat.source_type_id(node.value)) {
		return check
	}
	expr_type_source_id := t.node_type(expr_id)
	expr_type := flat.source_type_name(expr_type_source_id)
	clean_type0_source_id := t.trim_pointer_type(expr_type_source_id)
	clean_type0 := flat.source_type_name(clean_type0_source_id)
	concrete_expr_type_source_id := t.normalize_type_alias(clean_type0_source_id)

	resolved_expr_sum_source_id := t.resolve_sum_name(concrete_expr_type_source_id)

	// Inside a `$for v in T.variants` body the pattern is the loop variable;
	// the unroll substitutes it later, so there is nothing to validate yet.
	if t.cloning_comptime_for_depth == 0
		&& !t.validate_specialized_is_expr(concrete_expr_type_source_id, resolved_expr_sum_source_id, flat.source_type_id(node.value)) {
		return t.make_bool_literal(false)
	}
	clean_type := if t.sum_types.contains(clean_type0) {
		clean_type0
	} else if _ := t.resolve_sum_variant_pattern_for_subject(clean_type0_source_id, flat.source_type_id(node.value)) {
		clean_type0
	} else {
		flat.source_type_name(t.find_sum_type_for_variant(flat.source_type_id(node.value)))
	}
	if t.is_builtin_ierror_interface_name(clean_type0_source_id) && node.value == 'none' {
		new_expr0, value_type_source_id_2, op := t.pointer_sum_access_expr(expr_id, expr_type_source_id)

		new_expr := t.stable_transformed_expr_for_reuse(new_expr0, value_type_source_id_2, 'ierror_is')
		typ := t.make_selector_op(new_expr, '_typ', flat.source_type_int, op)
		return t.make_ierror_none_type_check(typ, clean_type0)
	}
	if t.is_interface_type_name(clean_type0_source_id) {
		new_expr0, value_type_source_id_3, op := t.pointer_sum_access_expr(expr_id, expr_type_source_id)

		new_expr := t.stable_transformed_expr_for_reuse(new_expr0, value_type_source_id_3, 'iface_is')
		pattern_name := if t.is_builtin_ierror_interface_name(clean_type0_source_id) && node.value == 'none' {
			'None__'
		} else {
			node.value
		}
		// Interface runtime tags describe the stored concrete value, not whether the
		// source pattern asks to expose that value through a pointer smartcast.
		// Resolve `is &Concrete` against `Concrete`, while retaining the original
		// pattern in the smartcast context.
		concrete_pattern_name_source_id := t.trim_all_pointer_type(flat.source_type_id(pattern_name))

		if target_iface_source_id := t.resolve_interface_pattern_interface(concrete_pattern_name_source_id) {
			if check := t.make_interface_target_is_check(new_expr, expr_type_source_id, clean_type0_source_id,
				target_iface_source_id)
			{
				return check
			}
		}
		type_ids := t.interface_impl_type_ids(clean_type0_source_id, concrete_pattern_name_source_id)
		if type_ids.len > 0 {
			stable_expr := if type_ids.len > 1 {
				t.stable_transformed_expr_for_reuse(new_expr, expr_type_source_id, 'iface_is')
			} else {
				new_expr
			}
			typ := t.make_selector_op(stable_expr, '_typ', flat.source_type_int, op)
			mut check := t.make_infix(.eq, typ, t.make_int_literal(type_ids[0]))
			for i in 1 .. type_ids.len {
				next := t.make_infix(.eq, typ, t.make_int_literal(type_ids[i]))
				check = t.make_infix(.logical_or, check, next)
			}
			return check
		}
		if pattern_source_id := t.resolve_interface_pattern(concrete_pattern_name_source_id, clean_type0_source_id) {
			pattern := flat.source_type_name(pattern_source_id)

			is_start := t.a.children.len
			t.a.children << new_expr
			return t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          pattern
				children_start: is_start
				children_count: 1
				typ:            flat.source_type_bool
			})
		}
		object := t.make_selector_op(new_expr, '_object', flat.source_type_voidptr, op)
		return t.make_infix(.ne, object, t.a.add(.nil_literal))
	}
	resolved_clean_type := flat.source_type_name(t.resolve_sum_name(flat.source_type_id(clean_type)))
	if clean_type.len == 0 || !t.sum_types.contains(resolved_clean_type) {
		return t.make_bool_literal(true)
	}
	// Route a value-context `match`/`if` subject (e.g. `(match n { First { make_circle()!
	// } else { make_square()! } }) is Circle`) through value lowering so a propagating
	// branch tail is materialized as a value instead of in a value-less statement context.
	// `transform_value_operand` is a no-op for the common non-branch subjects.
	new_expr := t.transform_value_operand(expr_id)
	// Mutable array/map loop bindings are storage pointers, but their rvalue
	// transform above already loads the sum value. Build the tag/path checks from
	// the transformed storage type so the value is not dereferenced twice.
	mut check_expr_type := flat.source_type_name(t.node_type(new_expr))
	if check_expr_type.len == 0 || check_expr_type == 'unknown' {
		check_expr_type = expr_type
	}
	if node.typ_text() != 'match_exact' {
		if check := t.make_sum_type_alias_pattern_check(new_expr, flat.source_type_id(check_expr_type), flat.source_type_id(clean_type),
			flat.source_type_id(node.value))
		{
			return check
		}
	}
	if check := t.make_sum_type_pattern_check(new_expr, flat.source_type_id(check_expr_type), flat.source_type_id(clean_type), flat.source_type_id(node.value)) {
		return check
	}
	return t.make_bool_literal(true)
}

fn (mut t Transformer) make_sum_type_alias_pattern_check(expr flat.NodeId, expr_type_id u32, sum_name_id u32, pattern_id u32) ?flat.NodeId {
	variants := flat.TypeTextList{ ids: t.sum_alias_equivalent_variants(sum_name_id, pattern_id) }.names()
	if variants.len <= 1 {
		return none
	}
	stable_expr := t.stable_transformed_expr_for_reuse(expr, expr_type_id, 'sum_is')
	mut check := flat.empty_node
	for variant in variants {
		next := t.make_sum_type_pattern_check(stable_expr, expr_type_id, sum_name_id, flat.source_type_id(variant)) or {
			continue
		}
		if int(check) < 0 {
			check = next
		} else {
			check = t.make_infix(.logical_or, check, next)
		}
	}
	if int(check) < 0 {
		return none
	}
	return check
}

fn (t &Transformer) sum_alias_equivalent_variants(sum_name_id u32, pattern_id u32) []u32 {
	resolved_sum_source_id := t.resolve_sum_name(t.trim_pointer_type(sum_name_id))

	variants := flat.TypeTextList{ ids: t.sum_type_variants_for_index(resolved_sum_source_id) }.names()
	if variants.len == 0 {
		return flat.TypeTextList.from_names([]string{}).ids
	}
	mut pattern_forms := []AliasEquivalentForms{}
	for name_source_id in t.interface_alias_equivalent_names(pattern_id) {
		pattern_forms << t.alias_equivalent_forms(name_source_id)
	}
	mut result := []string{}
	for variant in variants {
		if t.type_name_matches_any_alias_equivalent(flat.source_type_id(variant), pattern_forms) {
			result << variant
		}
	}
	return flat.TypeTextList.from_names(result).ids
}

// AliasEquivalentForms holds a type name, its spelling normalized in the current
// module, and the short names of both.
struct AliasEquivalentForms {
	name             u32
	normalized       u32
	short            u32
	normalized_short u32
}

fn (t &Transformer) alias_equivalent_forms(name_id u32) AliasEquivalentForms {
	normalized_source_id := t.normalize_type_in_module(name_id, t.cur_module)
	return AliasEquivalentForms{
		name:             name_id
		normalized:       normalized_source_id
		short:            t.variant_short_name(name_id)
		normalized_short: t.variant_short_name(normalized_source_id)
	}
}

fn (t &Transformer) type_name_matches_any_alias_equivalent(name_id u32, candidates []AliasEquivalentForms) bool {
	n := t.alias_equivalent_forms(name_id)
	for c in candidates {
		if n.name == c.name || n.name == c.normalized || n.normalized == c.name
			|| n.normalized == c.normalized || n.short == c.short || n.short == c.normalized_short
			|| n.normalized_short == c.short || n.normalized_short == c.normalized_short {
			return true
		}
	}
	return false
}

fn (mut t Transformer) smartcasted_sum_is_expr_check(expr_id flat.NodeId, pattern_id u32) ?flat.NodeId {
	key := t.expr_key(expr_id)
	if key.len == 0 {
		return none
	}
	sc := t.find_smartcast(key) or { return none }
	raw_sum_source_id := t.trim_pointer_type(t.raw_expr_type_without_smartcast(expr_id))

	resolved_sum_source_id_4 := t.resolve_sum_name(raw_sum_source_id)
	resolved_sum := flat.source_type_name(resolved_sum_source_id_4)
	if resolved_sum_source_id_4 == 0 || !t.sum_types.contains(resolved_sum)
		|| flat.source_type_name(t.resolve_sum_name(sc.sum_type_name)) != resolved_sum {
		return none
	}
	if _ := t.resolve_sum_variant_pattern_for_subject(raw_sum_source_id, pattern_id) {
		plain := t.make_plain_expr_for_smartcast(expr_id)
		return t.make_sum_type_pattern_check(plain, raw_sum_source_id, raw_sum_source_id, pattern_id)
	}
	return none
}

fn (mut t Transformer) validate_specialized_is_expr(subject_type_id u32, resolved_sum_id u32, pattern_id u32) bool {
	pattern := flat.source_type_name(pattern_id)
	resolved_sum := flat.source_type_name(resolved_sum_id)
	subject_type := flat.source_type_name(subject_type_id)
	if !t.validating_generic_spec || subject_type_id == u32(0)
		|| t.type_text_has_generic_placeholder(subject_type_id, t.cur_module) {
		return true
	}
	if t.sum_types.contains(resolved_sum) {
		if pattern_id == u32(0) || t.type_text_has_generic_placeholder(pattern_id, t.cur_module) {
			return true
		}
		if _ := t.resolve_sum_variant_pattern_for_subject(subject_type_id, pattern_id) {
			return true
		}
		t.record_monomorph_error('`${pattern}` is not a variant of sum type `${resolved_sum}`')
		return false
	}
	if t.is_interface_type_name(subject_type_id) {
		if pattern_id == u32(0) || t.type_text_has_generic_placeholder(pattern_id, t.cur_module) {
			return true
		}
		if t.is_builtin_ierror_interface_name(subject_type_id) && pattern_id == flat.source_type_none {
			return true
		}
		if _ := t.resolve_interface_pattern(pattern_id, subject_type_id) {
			return true
		}
		if target_iface_source_id := t.resolve_interface_pattern_interface(pattern_id) {
			if t.interface_conversion_impl_mappings(subject_type_id, target_iface_source_id).len > 0 {
				return true
			}
		}
		if t.is_builtin_ierror_interface_name(subject_type_id) {
			t.record_monomorph_error('`${pattern}` is not compatible with `IError`')
		} else if t.specialized_is_pattern_known(pattern_id) {
			t.record_monomorph_error('`${pattern}` is not compatible with interface `${subject_type}`')
		} else {
			t.record_monomorph_error('unknown type `${pattern}`')
		}
		return false
	}
	t.record_monomorph_error('`is` can only be used with sum type or interface values, not `${subject_type}`')
	return false
}

fn (t &Transformer) specialized_is_pattern_known(pattern_id u32) bool {
	for candidate_source_id_4 in t.interface_pattern_candidates(pattern_id) {
		candidate := flat.source_type_name(candidate_source_id_4)

		if types.is_builtin_type_name(candidate) || t.interface_pattern_candidate_known(candidate) {
			return true
		}
		clean := trimmed_transform_text(candidate)
		if clean.starts_with('[]') || clean.starts_with('map[') || clean.starts_with('[')
			|| clean.starts_with('fn ') || clean.starts_with('fn(') {
			if t.tc.parse_type(flat.source_type_id(clean)).name() != 'unknown' {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) interface_impl_type_id(iface_name_id u32, concrete_name_id u32) ?int {
	concrete_name := flat.source_type_name(concrete_name_id)

	if iface_name_id == u32(0) || concrete_name_id == u32(0) || isnil(t.tc) {
		return none
	}
	iface_source_id := t.resolve_interface_type_name(iface_name_id)

	if iface_source_id == 0 {
		return none
	}
	concrete := flat.source_type_name(t.interface_concrete_impl_name(concrete_name_id) or { return none })
	requested_qualified := concrete_name.contains('.') || concrete != concrete_name
	iface_candidates := flat.TypeTextList{ ids: t.interface_impl_type_id_iface_candidates(iface_source_id) }.names()
	for impl_iface in iface_candidates {
		if impl_index := t.interface_impl_indexes[impl_iface] {
			if type_id := impl_index.ids[concrete] {
				return type_id
			}
		}
	}
	if !requested_qualified {
		for impl_iface in iface_candidates {
			impl_index := t.interface_impl_indexes[impl_iface] or { continue }
			for impl_name in impl_index.names_text() {
				if impl_name.all_after_last('.') == concrete.all_after_last('.') {
					return impl_index.ids[impl_name]
				}
			}
		}
	}
	// Generic specialization can introduce a concrete implementation after the
	// immutable transform indexes were prepared. Fall back to the live checker
	// only for those late names.
	for impl_iface in iface_candidates {
		impl_names := if t.is_builtin_ierror_interface_name(flat.source_type_id(impl_iface)) {
			t.tc.ierror_impl_names()
		} else {
			t.tc.interface_impl_names(impl_iface)
		}
		type_ids := if t.is_builtin_ierror_interface_name(flat.source_type_id(impl_iface)) {
			types.stable_interface_type_ids(impl_names)
		} else {
			t.tc.interface_type_ids(impl_iface)
		}
		for impl_name in impl_names {
			if impl_name == concrete {
				return type_ids[impl_name]
			}
		}
	}
	if !requested_qualified {
		for impl_iface in iface_candidates {
			impl_names := if t.is_builtin_ierror_interface_name(flat.source_type_id(impl_iface)) {
				t.tc.ierror_impl_names()
			} else {
				t.tc.interface_impl_names(impl_iface)
			}
			type_ids := if t.is_builtin_ierror_interface_name(flat.source_type_id(impl_iface)) {
				types.stable_interface_type_ids(impl_names)
			} else {
				t.tc.interface_type_ids(impl_iface)
			}
			for impl_name in impl_names {
				if impl_name.all_after_last('.') == concrete.all_after_last('.') {
					return type_ids[impl_name]
				}
			}
		}
	}
	return none
}

fn (t &Transformer) interface_container_cast_type_id(iface_name_id u32, target_name_id u32) ?int {
	if type_id := t.interface_impl_type_id(iface_name_id, target_name_id) {
		return type_id
	}
	iface_source_id_2 := t.resolve_interface_type_name(iface_name_id)

	if iface_source_id_2 == 0 {
		return none
	}
	concrete_source_id := t.interface_concrete_impl_name(target_name_id) or { return none }
	concrete := flat.source_type_name(concrete_source_id)
	if interface_pattern_is_collapsed_container_type(concrete) {
		type_idx := t.type_index_for_type_name(concrete_source_id)
		if type_idx <= 0 {
			return none
		}
		// Interface dispatch IDs are always positive. Reserve the sign bit for
		// late containers so type_idx() can recover their stable type index even
		// when specialization introduced them after the immutable index snapshot.
		return int(u32(type_idx) | (u32(1) << 31))
	}
	index := t.interface_impl_index_for_transform(iface_source_id_2)
	mut impls := index.names_text().clone()
	if concrete !in impls {
		impls << concrete
	}
	ids := types.stable_interface_type_ids_preserving_prefix(index.names_text(), impls)
	return ids[concrete] or { none }
}

fn (t &Transformer) interface_impl_type_id_iface_candidates(iface_id u32) []u32 {
	iface := flat.source_type_name(iface_id)
	mut candidates := []string{}
	short_name := iface.all_after_last('.')
	if short_name != iface && (iface.starts_with('main.') || iface.starts_with('builtin.')) {
		candidates << short_name
	}
	candidates << iface
	if !iface.contains('.') {
		qname := t.tc.qualify_name(iface)
		if qname != iface {
			candidates << qname
		}
	}
	mut result := []string{}
	for candidate in candidates {
		if candidate.len == 0 || candidate in result {
			continue
		}
		if t.is_builtin_ierror_interface_name(flat.source_type_id(candidate)) || candidate in t.tc.interface_names {
			result << candidate
		}
	}
	return flat.TypeTextList.from_names(result).ids
}

fn (t &Transformer) interface_impl_type_ids(iface_name_id u32, concrete_name_id u32) []int {
	mut ids := []int{}
	for equivalent_name_source_id in t.interface_alias_equivalent_names(concrete_name_id) {
		id := t.interface_impl_type_id(iface_name_id, equivalent_name_source_id) or { continue }
		if id !in ids {
			ids << id
		}
	}
	return ids
}

// interface_alias_equivalent_names returns `name`, its normalized spelling and the
// aliases equivalent to either, memoized for the current module and file.
fn (t &Transformer) interface_alias_equivalent_names(name_id u32) []u32 {
	if isnil(t.alias_equivalent_names_cache) {
		return t.interface_alias_equivalent_names_uncached(name_id)
	}
	mut cache := t.alias_equivalent_names_cache
	if !same_transform_text(cache.module, t.cur_module) || !same_transform_text(cache.file, t.cur_file) {
		cache.module = t.cur_module
		cache.file = t.cur_file
		cache.entries.clear()
	}
	if names := cache.entries[name_id] { return names.clone() }
	names := t.interface_alias_equivalent_names_uncached(name_id)
	cache.entries[name_id] = names.clone()
	return names
}

fn (t &Transformer) interface_alias_equivalent_names_uncached(name_id u32) []u32 {
	mut names := []u32{}
	mut seen := map[u32]bool{}
	t.push_interface_alias_equivalent_name(mut names, mut seen, name_id)
	if isnil(t.tc) { return names }
	normalized := t.normalize_type_in_module(name_id, t.cur_module)
	t.push_interface_alias_equivalent_name(mut names, mut seen, normalized)
	allow_short_match := !flat.source_type_shape(name_id).has(.dotted) && !flat.source_type_shape(normalized).has(.dotted)
	name_short := t.variant_short_name(name_id)
	normalized_short := t.variant_short_name(normalized)
	for alias, target in t.tc.type_aliases.ids {
		target_normalized := t.normalize_type_in_module(target, t.cur_module)
		target_short := t.variant_short_name(target)
		target_normalized_short := t.variant_short_name(target_normalized)
		if target == name_id || target == normalized || target_normalized == name_id || target_normalized == normalized
			|| (allow_short_match && (target_short == name_short || target_short == normalized_short || target_normalized_short == name_short || target_normalized_short == normalized_short)) {
			t.push_interface_alias_equivalent_name(mut names, mut seen, flat.source_type_id(alias))
			t.push_interface_alias_equivalent_name(mut names, mut seen, target)
			t.push_interface_alias_equivalent_name(mut names, mut seen, target_normalized)
		}
	}
	return names
}

fn (t &Transformer) push_interface_alias_equivalent_name(mut names []u32, mut seen map[u32]bool, name u32) {
	if name == 0 || seen[name] { return }
	seen[name] = true
	names << name
}

fn (t &Transformer) interface_concrete_impl_name(name_id u32) ?u32 {
	name_comparison_type_id_6 := name_id
	name := flat.source_type_name(name_comparison_type_id_6)
	if alias_source_id := t.function_alias_impl_name(name_id) {
		return alias_source_id
	}
	if name.starts_with('[]') {
		typ := t.tc.parse_type(name_id)
		canonical := typ.name()
		if typ is types.Array {
			return flat.source_type_id(canonical)
		}
	}
	if name.starts_with('map[') {
		typ := t.tc.parse_type(name_id)
		canonical := typ.name()
		if typ is types.Map {
			return flat.source_type_id(canonical)
		}
	}
	if name_comparison_type_id_6 in [flat.source_type_bool, flat.source_type_int, flat.source_type_i8,
		flat.source_type_i16, flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
		flat.source_type_usize, flat.source_type_u8, flat.source_type_u16, flat.source_type_u32,
		flat.source_type_u64, flat.source_type_f32, flat.source_type_f64, flat.source_type_string,
		flat.source_type_char, flat.source_type_rune, flat.source_type_voidptr] {
		return name_id
	}
	if name.starts_with('builtin.') {
		short := name['builtin.'.len..]
		if short in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'u16', 'u32',
			'u64', 'f32', 'f64', 'string', 'char', 'rune', 'voidptr'] {
			return flat.source_type_id(short)
		}
	}
	if name.contains('.') {
		short := name.all_after_last('.')
		if short in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'u16', 'u32',
			'u64', 'f32', 'f64', 'string', 'char', 'rune', 'voidptr'] {
			return flat.source_type_id(short)
		}
	}
	base, _, is_generic_app := generic_app_parts(name)
	if is_generic_app && !t.generic_arg_is_unresolved(name_id) {
		if base in t.tc.structs || t.tc.type_aliases.contains(base) {
			return name_id
		}
		if !base.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
			qualified_base := '${t.cur_module}.${base}'
			if qualified_base in t.tc.structs || t.tc.type_aliases.contains(qualified_base) {
				return flat.source_type_id('${qualified_base}${name[base.len..]}')
			}
		}
	}
	if !name.contains('.') {
		if t.cur_file.len > 0 {
			for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
				[]string{}
			} {
				if candidate in t.tc.structs || t.tc.type_aliases.contains(candidate) {
					return flat.source_type_id(candidate)
				}
			}
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${name}'
			if qname in t.tc.structs || t.tc.type_aliases.contains(qname) {
				return flat.source_type_id(qname)
			}
		}
	}
	if name in t.tc.structs || t.tc.type_aliases.contains(name) {
		return name_id
	}
	for struct_name, _ in t.tc.structs {
		if struct_name.all_after_last('.') == name {
			return flat.source_type_id(struct_name)
		}
	}
	for alias_name, _ in t.tc.type_aliases.ids {
		if alias_name.all_after_last('.') == name {
			return flat.source_type_id(alias_name)
		}
	}
	return none
}

fn (t &Transformer) function_alias_impl_name(name_id u32) ?u32 {
	name := flat.source_type_name(name_id)
	clean := trimmed_transform_text(name)
	if !clean.starts_with('fn(') && !clean.starts_with('fn ') {
		return none
	}
	mut aliases := t.tc.type_aliases.keys()
	aliases.sort()
	for alias in aliases {
		target := flat.source_type_name(t.tc.type_aliases.type_id(alias) or { continue })
		if trimmed_transform_text(target) == clean {
			return flat.source_type_id(alias)
		}
	}
	return none
}

// make_sum_is_check builds make sum is check data for transform.
fn (mut t Transformer) make_sum_is_check(expr flat.NodeId, expr_type_id u32, sum_name_id u32, variant_id u32) flat.NodeId {
	expr_type_shape := flat.source_type_shape(expr_type_id)

	tag := t.make_sum_tag_selector(expr, if expr_type_shape.has(.pointer) {
		.arrow
	} else {
		.dot
	})
	idx := t.make_int_literal(t.sum_type_index(sum_name_id, variant_id))
	return t.make_infix(.eq, tag, idx)
}

// sum_variant_path supports sum variant path handling for Transformer.
fn (t &Transformer) sum_variant_path(sum_name_id u32, variant_id u32) []u32 {
	clean_sum_source_id := t.trim_pointer_type(sum_name_id)

	for candidate_source_id_2 in t.sum_subject_type_candidates(clean_sum_source_id) {
		if direct_source_id := t.sum_variant_name(candidate_source_id_2, variant_id) {
			direct := flat.source_type_name(direct_source_id)

			return flat.TypeTextList.from_names([direct]).ids
		}
	}
	mut visited := map[string]bool{}
	return t.sum_variant_path_inner(sum_name_id, variant_id, mut visited).clone()
}

// sum_variant_path_inner supports sum variant path inner handling for Transformer.
fn (t &Transformer) sum_variant_path_inner(sum_name_id u32, variant_id u32, mut visited map[string]bool) []u32 {
	clean_sum_source_id := t.trim_pointer_type(sum_name_id)

	for candidate_source_id_3 in t.sum_subject_type_candidates(clean_sum_source_id) {
		if direct_source_id := t.sum_variant_name(candidate_source_id_3, variant_id) {
			direct := flat.source_type_name(direct_source_id)

			return flat.TypeTextList.from_names([direct]).ids
		}
	}
	resolved_sum_source_id_5 := t.resolve_sum_name(clean_sum_source_id)
	resolved_sum_comparison_type_id_4 := resolved_sum_source_id_5
	resolved_sum := flat.source_type_name(resolved_sum_comparison_type_id_4)
	if resolved_sum_source_id_5 == 0 || resolved_sum in visited {
		return flat.TypeTextList.from_names([]string{}).ids
	}
	visited[resolved_sum] = true
	variants := flat.TypeTextList{ ids: t.concrete_sum_variants_for_candidate(clean_sum_source_id) }.names()
	if variants.len == 0 {
		return flat.TypeTextList.from_names([]string{}).ids
	}
	for direct in variants {
		direct_sum_source_id := t.resolve_sum_name(t.trim_pointer_type(flat.source_type_id(direct)))
		direct_sum_comparison_type_id := direct_sum_source_id
		direct_sum := flat.source_type_name(direct_sum_comparison_type_id)
		if direct_sum_comparison_type_id == resolved_sum_comparison_type_id_4 || !t.sum_types.contains(direct_sum) {
			continue
		}
		nested := flat.TypeTextList{ ids: t.sum_variant_path_inner(direct_sum_source_id, variant_id, mut visited) }.names()
		if nested.len == 0 {
			continue
		}
		mut path := []string{cap: nested.len + 1}
		path << direct
		path << nested
		return flat.TypeTextList.from_names(path).ids
	}
	return flat.TypeTextList.from_names([]string{}).ids
}

// make_sum_type_pattern_check builds make sum type pattern check data for transform.
fn (mut t Transformer) make_sum_type_pattern_check(expr flat.NodeId, expr_type_id u32, sum_name_id u32, variant_id u32) ?flat.NodeId {
	expr_type := flat.source_type_name(expr_type_id)

	clean_sum_source_id := t.trim_pointer_type(sum_name_id)
	clean_sum := flat.source_type_name(clean_sum_source_id)
	path := flat.TypeTextList{ ids: t.sum_variant_path(clean_sum_source_id, variant_id) }.names()
	if path.len == 0 {
		return none
	}
	stable_expr := if path.len > 1 {
		t.stable_transformed_expr_for_reuse(expr, expr_type_id, 'sum_is')
	} else {
		expr
	}
	mut current := stable_expr
	mut current_type := expr_type
	mut current_sum := clean_sum
	mut chain := flat.empty_node
	for i, path_variant in path {
		cmp := t.make_sum_is_check(current, flat.source_type_id(current_type), flat.source_type_id(current_sum), flat.source_type_id(path_variant))
		if int(chain) < 0 {
			chain = cmp
		} else {
			chain = t.make_infix(.logical_and, chain, cmp)
		}
		if i == path.len - 1 {
			break
		}
		qv_source_id := t.resolve_variant(flat.source_type_id(current_sum), flat.source_type_id(path_variant))
		qv := flat.source_type_name(qv_source_id)
		use_ptr := t.variant_references_sum(qv_source_id, flat.source_type_id(current_sum)) && !t.sum_variant_is_direct_pointer(qv_source_id)
		field_type := if use_ptr { '&${qv}' } else { qv }
		current = t.make_selector_op(current, t.sum_field_name(qv_source_id), flat.source_type_id(field_type), if current_type.starts_with('&') {
			.arrow
		} else {
			.dot
		})
		current_type = field_type
		current_sum = flat.source_type_name(t.trim_pointer_type(qv_source_id))
	}
	return chain
}

// transform_as_expr converts transform as expr data for transform.
fn (mut t Transformer) transform_as_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	first_child := t.a.child(&node, 0)
	if t.is_value_match_or_if_operand(first_child) {
		// A `match`/`if` operand of an `as` cast, e.g. `(match x { ... }) as Variant`,
		// is a value expression whose (possibly propagating) branch tails must be
		// lowered as values. Materialize it into a value temp first, then re-run the
		// `as` conversion over that temp (mirrors the option-source path below).
		mut operand_type := flat.source_type_name(t.raw_expr_type_without_smartcast(first_child))
		if operand_type.len == 0 {
			operand_type = flat.source_type_name(t.node_type(first_child))
		}
		if operand_type.len == 0 {
			operand_type = flat.source_type_name(t.resolve_expr_type(first_child))
		}
		value := t.transform_expr_for_type(first_child, flat.source_type_id(operand_type))
		start := t.a.children.len
		t.a.children << value
		return t.transform_as_expr(id, flat.Node{
			kind:           .as_expr
			value:          node.value
			typ:            node.typ
			children_start: start
			children_count: 1
			pos:            node.pos
		})
	}
	mut expr_id := t.a.child(&node, 0)
	mut unchecked_optional_unwrap := false
	expr_node := t.a.nodes[int(expr_id)]
	if expr_node.kind == .or_expr && expr_node.value == '?' && expr_node.children_count > 0 {
		source_id := t.a.child(&expr_node, 0)
		mut source_type := flat.source_type_name(t.raw_expr_type_without_smartcast(source_id))
		if source_type.len == 0 {
			source_type = flat.source_type_name(t.node_type(source_id))
		}
		if t.is_optional_type_name(flat.source_type_id(source_type)) {
			expr_id = source_id
			unchecked_optional_unwrap = true
		}
	}
	// `as` converts from the expression's storage type. Inside an `is` branch,
	// `node_type` reports the smartcast target instead; using that here makes an
	// interface-to-interface cast look like a no-op and leaves the source box on
	// the RHS (for example `layout as Widget`).
	mut expr_type := flat.source_type_name(t.raw_expr_type_without_smartcast(expr_id))
	if expr_type.len == 0 {
		expr_type = flat.source_type_name(t.node_type(expr_id))
	}
	if t.is_optional_type_name(flat.source_type_id(expr_type)) {
		optional_type_source_id := t.qualify_optional_type(flat.source_type_id(expr_type))

		target_type_source_id := t.qualify_optional_type(flat.source_type_id(node.value))

		if t.is_optional_type_name(target_type_source_id)
			&& t.normalize_type_alias(optional_type_source_id) == t.normalize_type_alias(target_type_source_id) {
			return t.make_plain_expr_for_smartcast(expr_id)
		}
		payload_type_source_id := t.optional_base_type(optional_type_source_id)

		// `as` starts from the option's storage value. Inside nested `x != none`
		// and `x is Variant` branches, transforming `x` normally would apply both
		// smartcasts before this code selects `.value`, then extract the variant a
		// second time.
		source := if unchecked_optional_unwrap {
			t.transform_optional_wrapper_expr(expr_id)
		} else if t.expr_has_smartcast(expr_id) {
			t.make_plain_expr_for_smartcast(expr_id)
		} else {
			t.transform_expr(expr_id)
		}
		value := t.make_selector(source, 'value', payload_type_source_id)
		if t.normalize_type_alias(payload_type_source_id) == t.normalize_type_alias(flat.source_type_id(node.value)) {
			return value
		}
		resolved_payload_source_id := t.resolve_sum_name(t.trim_pointer_type(payload_type_source_id))
		resolved_payload := flat.source_type_name(resolved_payload_source_id)
		if t.sum_types.contains(resolved_payload) {
			qv_source_id_2 := t.resolve_variant(resolved_payload_source_id, flat.source_type_id(node.value))
			qv := flat.source_type_name(qv_source_id_2)
			if qv_source_id_2 != 0 && t.sum_target_accepts_variant_type(resolved_payload_source_id, qv_source_id_2) {
				use_ptr := t.variant_references_sum(qv_source_id_2, resolved_payload_source_id)
					&& !t.sum_variant_is_direct_pointer(qv_source_id_2)
				field_type := if use_ptr { '&${qv}' } else { qv }
				field := t.make_selector_op(value, t.sum_field_name(qv_source_id_2), flat.source_type_id(field_type), .dot)
				if use_ptr {
					return t.make_prefix(.mul, field)
				}
				return field
			}
		}
		start := t.a.children.len
		t.a.children << value
		return t.transform_as_expr(id, flat.Node{
			kind:           .as_expr
			value:          node.value
			typ:            node.typ
			children_start: start
			children_count: 1
			pos:            node.pos
		})
	}
	clean_type0_source_id := t.trim_pointer_type(flat.source_type_id(expr_type))
	clean_type0 := flat.source_type_name(clean_type0_source_id)
	if t.is_interface_type_name(clean_type0_source_id) {
		target_is_pointer := node.value.starts_with('&')
		target_pattern_source_id := t.trim_pointer_type(flat.source_type_id(node.value))

		if target_iface_source_id := t.resolve_interface_pattern_interface(target_pattern_source_id) {

			// Use the raw interface expression here. Applying the active smartcast
			// first would build the target interface once, then the explicit `as`
			// conversion would incorrectly treat that value as the original source
			// (including using pointer selectors for a value temporary).
			child := if t.expr_has_smartcast(expr_id) {
				t.make_plain_expr_for_smartcast(expr_id)
			} else {
				t.transform_expr(expr_id)
			}
			if converted := t.convert_interface_expr_to_interface(child, flat.source_type_id(expr_type), target_iface_source_id) {
				if target_is_pointer {
					return t.heap_copy_interface_expr(converted, target_iface_source_id, flat.source_type_id(node.value))
				}
				return converted
			}
		}
		if t.is_builtin_ierror_interface_name(clean_type0_source_id) && target_is_pointer {
			if concrete_source_id := t.resolve_interface_pattern(target_pattern_source_id, clean_type0_source_id) {
				concrete := flat.source_type_name(concrete_source_id)

				// IError stores the concrete object directly. Read the original interface
				// box before applying an active smartcast to its payload.
				child := if t.expr_has_smartcast(expr_id) {
					t.make_plain_expr_for_smartcast(expr_id)
				} else {
					t.transform_expr(expr_id)
				}
				field_op := if expr_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				object := t.make_selector_op(child, '_object', flat.source_type_voidptr, field_op)
				return t.make_cast(flat.source_type_id('&${concrete}'), object, flat.source_type_id('&${concrete}'))
			}
		}
		if !interface_pattern_is_collapsed_container_type(node.value) {
			if qv_source_id := t.resolve_interface_pattern(flat.source_type_id(node.value), clean_type0_source_id) {
				qv := flat.source_type_name(qv_source_id)

				if sc := t.find_smartcast(t.expr_key(expr_id)) {
					target_source_id := t.trim_pointer_type(t.smartcast_target_type(sc))

					if t.variant_names_match(target_source_id, qv_source_id) {
						return t.transform_expr(expr_id)
					}
				}
				child := t.transform_expr(expr_id)
				field_op := if expr_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				object := t.make_selector_op(child, '_object', flat.source_type_voidptr, field_op)
				cast := t.make_cast(flat.source_type_id('&${qv}'), object, flat.source_type_id('&${qv}'))
				current := t.make_prefix(.mul, cast)
				t.set_node_typ(int(current), qv_source_id)
				return current
			}
		}
		if interface_pattern_is_collapsed_container_type(node.value)
			&& t.tc.interface_abstract_method_names(clean_type0).len == 0
			&& t.tc.interface_field_list(clean_type0).len == 0 {
			target_type := t.tc.parse_type(flat.source_type_id(node.value)).name()
			if type_id := t.interface_container_cast_type_id(clean_type0_source_id, flat.source_type_id(target_type)) {
				child := t.transform_expr(expr_id)
				source := t.stable_transformed_expr_for_reuse(child, flat.source_type_id(expr_type), 'iface_as')
				field_op := if expr_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
				actual_type_id := t.make_selector_op(source, '_typ', flat.source_type_int, field_op)
				mismatch := t.make_infix(.ne, actual_type_id, t.make_int_literal(type_id))
				message := 'as cast: cannot cast interface value to `${target_type}`'
				t.pending_stmts << t.make_if(mismatch, t.make_block([
					t.make_panic_stmt(message),
				]), t.make_empty())
				object := t.make_selector_op(source, '_object', flat.source_type_voidptr, field_op)
				cast := t.make_cast(flat.source_type_id('&${target_type}'), object, flat.source_type_id('&${target_type}'))
				current := t.make_prefix(.mul, cast)
				t.set_node_typ(int(current), flat.source_type_id(target_type))
				return current
			}
		}
	}
	clean_type := if t.sum_types.contains(clean_type0) {
		clean_type0
	} else if _ := t.resolve_sum_variant_pattern_for_subject(clean_type0_source_id, flat.source_type_id(node.value)) {
		clean_type0
	} else {
		flat.source_type_name(t.find_sum_type_for_variant(flat.source_type_id(node.value)))
	}
	resolved_clean_type_source_id := t.resolve_sum_name(flat.source_type_id(clean_type))
	resolved_clean_type := flat.source_type_name(resolved_clean_type_source_id)
	if clean_type.len == 0 || !t.sum_types.contains(resolved_clean_type) {
		child := t.transform_expr(expr_id)
		start := t.a.children.len
		t.a.children << child
		return t.a.add_node(flat.Node{
			kind:           .as_expr
			value:          node.value
			typ:            node.typ
			children_start: start
			children_count: 1
			pos:            node.pos
		})
	}
	qv_source_id_3 := t.resolve_variant(flat.source_type_id(clean_type), flat.source_type_id(node.value))
	qv := flat.source_type_name(qv_source_id_3)
	if qv_source_id_3 != 0 && t.normalize_type_alias(clean_type0_source_id) == t.normalize_type_alias(qv_source_id_3) {
		return t.transform_expr(expr_id)
	}
	sc_key := t.expr_key(expr_id)
	if sc_key.len > 0 {
		contexts := t.smartcasts_for(sc_key)
		mut matched_contexts := []SmartcastContext{}
		for i, sc in contexts {
			mut sc_variant := sc.variant_name_text()
			if flat.source_type_shape(sc.variant_name).has(.dotted) {
				sc_variant = sc.variant_name_text().all_after_last('.')
			}
			mut target_variant := node.value
			if node.value.contains('.') {
				target_variant = node.value.all_after_last('.')
			}
			if sc_variant == target_variant {
				end := i + 1
				matched_contexts = contexts[..end].clone()
			}
		}
		if matched_contexts.len > 0 {
			base := t.make_plain_expr_for_smartcast(expr_id)
			return t.apply_smartcast_contexts(base, t.original_expr_type(expr_id), matched_contexts)
		}
	}
	field := t.sum_field_name(qv_source_id_3)
	new_expr := t.transform_expr(expr_id)
	source := t.stable_transformed_expr_for_reuse(new_expr, flat.source_type_id(expr_type), 'sum_as')
	variants := flat.TypeTextList{ ids: t.sum_type_variants_for_index(resolved_clean_type_source_id) }.names()
	if variants.len > 0 {
		mut accepted_variants := flat.TypeTextList{
			ids: t.sum_alias_equivalent_variants(resolved_clean_type_source_id,
				flat.source_type_id(node.value))
		}.names()
		if accepted_variants.len == 0 {
			accepted_variants << qv
		}
		actual_name := t.new_temp('sum_as_type')
		mut mismatch_stmts := [t.make_decl_assign_typed(actual_name,
			t.make_string_literal(t.sum_as_display_type(flat.source_type_id(variants[0]))), flat.source_type_string)]
		for variant in variants {
			is_variant := t.make_infix(.eq, t.make_sum_tag_selector(source, if expr_type.starts_with('&') {
				.arrow
			} else {
				.dot
			}), t.make_int_literal(t.sum_type_index(resolved_clean_type_source_id, flat.source_type_id(variant))))
			mismatch_stmts << t.make_if(is_variant, t.make_block([
				t.make_assign(t.make_ident(actual_name),
					t.make_string_literal(t.sum_as_display_type(flat.source_type_id(variant)))),
			]), t.make_empty())
		}
		check := t.make_call_typed('__as_cast', [
			t.a.add(.nil_literal),
			t.make_sum_tag_selector(source, if expr_type.starts_with('&') { .arrow } else { .dot }),
			t.make_int_literal(t.sum_type_index(resolved_clean_type_source_id, qv_source_id_3)),
			t.make_ident(actual_name),
			t.make_string_literal(t.sum_as_display_type(qv_source_id_3)),
		], flat.source_type_voidptr)
		mismatch_stmts << t.make_expr_stmt(check)
		mut mismatch := flat.empty_node
		for accepted_variant in accepted_variants {
			not_variant := t.make_infix(.ne, t.make_sum_tag_selector(source, if expr_type.starts_with('&') {
				.arrow
			} else {
				.dot
			}), t.make_int_literal(t.sum_type_index(resolved_clean_type_source_id, flat.source_type_id(accepted_variant))))
			mismatch = if int(mismatch) < 0 {
				not_variant
			} else {
				t.make_infix(.logical_and, mismatch, not_variant)
			}
		}
		t.pending_stmts << t.make_if(mismatch, t.make_block(mismatch_stmts), t.make_empty())
	}
	use_ptr := t.variant_references_sum(qv_source_id_3, flat.source_type_id(clean_type)) && !t.sum_variant_is_direct_pointer(qv_source_id_3)
	field_typ := if use_ptr { '&${qv}' } else { qv }
	field_sel := t.make_selector_op(source, field, flat.source_type_id(field_typ), if expr_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	if use_ptr {
		return t.make_prefix(.mul, field_sel)
	}
	return field_sel
}

fn (t &Transformer) sum_as_display_type(name_id u32) string {
	name := flat.source_type_name(name_id)
	if name_id == 0 || name.contains('.') {
		return name
	}
	is_named_type := name in t.structs || t.sum_types.contains(name) || name in t.enum_types
		|| (!isnil(t.tc)
			&& (name in t.tc.interface_names || t.tc.type_aliases.contains(name)))
	if !is_named_type {
		return name
	}
	module_name := if t.cur_module.len > 0 { t.cur_module } else { 'main' }
	return '${module_name}.${name}'
}

// wrap_sum_return_expr transforms wrap sum return expr data for transform.
fn (mut t Transformer) wrap_sum_return_expr(expr_id flat.NodeId) flat.NodeId {
	was_return_expr := t.in_return_expr
	t.in_return_expr = true
	result := if t.cur_fn_ret_type == 0 || t.cur_fn_ret_type_text() !in t.sum_types.ids {
		t.transform_expr(expr_id)
	} else {
		t.wrap_sum_value(expr_id, t.cur_fn_ret_type)
	}
	t.in_return_expr = was_return_expr
	return result
}

fn (mut t Transformer) fixed_array_sum_literal_expr(expr_id flat.NodeId, expr flat.Node, resolved_sum_id u32) ?FixedArraySumLiteral {
	resolved_sum := flat.source_type_name(resolved_sum_id)
	mut literal_id := expr_id
	mut literal := expr
	if expr.kind == .postfix && expr.op == .not && expr.children_count == 1 {
		literal_id = t.a.child(&expr, 0)
		literal = t.a.nodes[int(literal_id)]
	}
	if literal.kind != .array_literal {
		return none
	}
	variants := flat.TypeTextList{ ids: t.sum_types.type_ids(resolved_sum) or { return none } }.names()
	literal_len := int(literal.children_count)
	literal_elem_source_id := t.array_literal_elem_type(literal)

	mut fallback_variant := ''
	mut fallback_type := ''
	mut fallback_count := 0
	for variant in variants {
		fixed_type_source_id := t.normalize_type_alias(flat.source_type_id(variant))
		fixed_type := flat.source_type_name(fixed_type_source_id)
		if !t.is_fixed_array_type(fixed_type_source_id) {
			continue
		}
		len_text := fixed_array_len_text(fixed_type)
		fixed_len := if is_decimal_text(len_text) {
			len_text.int()
		} else if !isnil(t.tc) {
			t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) or { continue }
		} else {
			continue
		}
		if fixed_len != literal_len {
			continue
		}
		elem_type := fixed_array_elem_type(fixed_type)
		if t.fixed_array_sum_literal_elem_matches(literal_elem_source_id, flat.source_type_id(elem_type)) {
			expr2 := t.transform_fixed_array_literal_for_type(literal_id, literal, fixed_type_source_id) or {
				continue
			}
			return FixedArraySumLiteral{
				variant: flat.source_type_id(variant)
				expr:    expr2
			}
		}
		if t.fixed_array_sum_literal_elem_unknown(literal_elem_source_id) {
			fallback_variant = variant
			fallback_type = fixed_type
			fallback_count++
		}
	}
	if fallback_count == 1 {
		expr2 := t.transform_fixed_array_literal_for_type(literal_id, literal, flat.source_type_id(fallback_type)) or {
			return none
		}
		return FixedArraySumLiteral{
			variant: flat.source_type_id(fallback_variant)
			expr:    expr2
		}
	}
	return none
}

fn (t &Transformer) fixed_array_sum_literal_elem_matches(literal_elem_id u32, variant_elem_id u32) bool {
	if t.fixed_array_sum_literal_elem_unknown(literal_elem_id) {
		return true
	}
	clean_literal_source_id := t.normalize_type_alias(literal_elem_id)
	clean_literal_comparison_type_id := clean_literal_source_id

	clean_variant_source_id := t.normalize_type_alias(variant_elem_id)
	clean_variant_comparison_type_id_2 := clean_variant_source_id

	return clean_literal_comparison_type_id == clean_variant_comparison_type_id_2 || t.variant_names_match(clean_literal_source_id, clean_variant_source_id)
}

fn (t &Transformer) fixed_array_sum_literal_elem_unknown(literal_elem_id u32) bool {
	literal_elem := flat.source_type_name(literal_elem_id)
	return literal_elem_id == u32(0) || literal_elem in ['unknown', 'void', 'array']
}

fn (t &Transformer) single_pointer_sum_nil_variant(expr_id flat.NodeId, sum_name_id u32) ?u32 {
	if !t.expr_is_nil_like(expr_id) {
		return none
	}
	_, variants_source_ids_2 := t.concrete_sum_name_and_variants(sum_name_id)

	mut pointer_variant := ''
	for variant_source_id in variants_source_ids_2 {
		variant_source_id_2 := variant_source_id
		variant := flat.source_type_name(variant_source_id_2)

		if !t.sum_variant_is_direct_pointer(variant_source_id_2) {
			continue
		}
		if pointer_variant.len > 0 {
			return none
		}
		pointer_variant = variant
	}
	if pointer_variant.len == 0 {
		return none
	}
	return flat.source_type_id(pointer_variant)
}

// wrap_sum_value transforms wrap sum value data for transform.
fn (mut t Transformer) wrap_sum_value(expr_id flat.NodeId, target_sum_id u32) flat.NodeId {
	return t.wrap_sum_value_with_storage(expr_id, target_sum_id, false)
}

fn (mut t Transformer) wrap_sum_value_for_storage(expr_id flat.NodeId, target_sum_id u32) flat.NodeId {
	return t.wrap_sum_value_with_storage(expr_id, target_sum_id, true)
}

fn (mut t Transformer) wrap_sum_value_with_storage(expr_id flat.NodeId, target_sum_id u32, storage_boundary bool) flat.NodeId {
	resolved_sum_source_id_6 := t.resolve_sum_name(target_sum_id)
	resolved_sum := flat.source_type_name(resolved_sum_source_id_6)
	if resolved_sum_source_id_6 == 0 || !t.sum_types.contains(resolved_sum) {
		return t.transform_expr(expr_id)
	}
	detach_array_payload := storage_boundary && !t.borrowed_projection_clone_required(expr_id, target_sum_id)
	storage_sum_source_id := t.sum_literal_type_name(target_sum_id, resolved_sum_source_id_6)
	storage_sum := flat.source_type_name(storage_sum_source_id)
	if nil_variant_source_id := t.single_pointer_sum_nil_variant(expr_id, resolved_sum_source_id_6) {
		value := t.transform_expr_for_type(expr_id, nil_variant_source_id)
		return t.make_sum_literal(storage_sum_source_id, nil_variant_source_id, value)
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .if_expr {
		branch_type_source_id := t.if_expr_branch_result_type(expr)

		if t.if_expr_branch_overrides_sum_target(branch_type_source_id, storage_sum_source_id) {
			return t.transform_expr(expr_id)
		}
		if lowered := t.try_expand_if_expr_value_for_type(expr_id, expr, storage_sum_source_id) {
			return lowered
		}
	}
	if fixed := t.fixed_array_sum_literal_expr(expr_id, expr, resolved_sum_source_id_6) {
		return t.make_sum_literal(resolved_sum_source_id_6, fixed.variant, fixed.expr)
	}
	mut expr_type := flat.source_type_name(t.node_type(expr_id))
	if expr.typ != 0 && t.sum_target_accepts_variant_type(resolved_sum_source_id_6, expr.typ) {
		expr_type = expr.typ_text()
	}
	if expr.kind in [.ident, .selector] {
		if expr.kind == .ident && expr.value.len > 0 {
			local_type_source_id := t.raw_var_type(expr.value)
			local_type := flat.source_type_name(local_type_source_id)
			if local_type_source_id != 0 && t.sum_target_accepts_variant_type(resolved_sum_source_id_6, local_type_source_id) {
				expr_type = local_type
			}
		}
		if expr.kind == .selector {
			selector_type_source_id := t.raw_selector_type_without_smartcast(expr_id)
			selector_type := flat.source_type_name(selector_type_source_id)
			if flat.source_type_name(t.resolve_sum_name(t.trim_pointer_type(selector_type_source_id))) == resolved_sum {
				// A smartcast narrows the selector's expression type to its active
				// variant, but the field still stores the complete sum value. Preserve
				// that storage when the surrounding context expects the same sum.
				plain := t.make_plain_expr_for_smartcast(expr_id)
				t.set_node_typ(int(plain), storage_sum_source_id)
				return plain
			}
			if selector_type_source_id != 0
				&& t.sum_target_accepts_variant_type(resolved_sum_source_id_6, selector_type_source_id) {
				expr_type = selector_type
			}
		}
		if const_type_source_id := t.raw_const_type_name_for_expr(expr_id) {
			const_type := flat.source_type_name(const_type_source_id)

			if t.sum_target_accepts_variant_type(resolved_sum_source_id_6, const_type_source_id) {
				expr_type = const_type
			}
		}
	}
	mut variant := expr_type
	mut expr_smartcast := SmartcastContext{}
	key := t.expr_key(expr_id)
	if key.len > 0 {
		if sc := t.find_smartcast(key) {
			expr_smartcast = sc
		}
	}
	has_expr_smartcast := expr_smartcast.expr_name.len > 0
	if has_expr_smartcast && flat.source_type_name(t.resolve_sum_name(expr_smartcast.sum_type_name)) == resolved_sum {
		variant = flat.source_type_name(t.resolve_variant(expr_smartcast.sum_type_name, expr_smartcast.variant_name))
	}
	if expr.kind == .prefix && expr.op == .mul && expr.children_count > 0 {
		inner_type := flat.source_type_name(t.node_type(t.a.child(&expr, 0)))
		if inner_type.starts_with('&') {
			variant = inner_type[1..]
		}
	}
	if expr.kind == .struct_init || expr.kind == .cast_expr {
		variant = expr.value
	} else if expr.kind == .as_expr && expr.value.len > 0 {
		variant = expr.value
	} else if expr.kind == .assoc && expr.value.len > 0 {
		variant = expr.value
	} else if expr.kind == .assoc && expr.children_count > 0 {
		variant = flat.source_type_name(t.node_type(t.a.child(&expr, 0)))
	}
	if expr.kind == .ident && expr.value.len > 0 {
		if smartcasted := t.smartcast_ident_value(expr.value) {
			smartcasted_type_source_id := t.node_type(smartcasted)

			if wrapper_value := t.single_value_wrapper_expr_value(smartcasted, smartcasted_type_source_id,
				resolved_sum_source_id_6)
			{
				return wrapper_value
			}
		}
	}
	if expr.kind !in [.assoc, .as_expr, .prefix, .struct_init, .cast_expr] && !has_expr_smartcast
		&& flat.source_type_name(t.resolve_sum_name(flat.source_type_id(expr_type))) == resolved_sum {
		if storage := t.pointer_storage_expr_for_value_target(expr_id, resolved_sum_source_id_6) {
			return storage
		}
		return t.transform_expr(expr_id)
	}
	if !has_expr_smartcast && expr_type.starts_with('&')
		&& flat.source_type_name(t.resolve_sum_name(flat.source_type_id(expr_type[1..]))) == resolved_sum {
		if expr.kind == .prefix && expr.op == .mul {
			return t.transform_expr(expr_id)
		}
		inner := t.transform_expr_preserving_pointer_value(expr_id)
		deref := t.make_prefix(.mul, inner)
		t.set_node_typ(int(deref), resolved_sum_source_id_6)
		return deref
	}
	if variant.len == 0 {
		return t.transform_expr(expr_id)
	}
	mut clean_variant := if variant.starts_with('&') { variant[1..] } else { variant }
	if clean_variant.starts_with('ptr') && clean_variant.len > 3 && clean_variant[3..].contains('.') {
		clean_variant = clean_variant[3..]
	}
	if clean_variant.starts_with('ptr') && clean_variant.len > 3
		&& clean_variant[3..].contains('__') {
		clean_variant = clean_variant[3..].replace('__', '.')
	}
	if expr.kind == .ident && expr.value.len > 0 {
		if smartcasted := t.smartcast_ident_value(expr.value) {
			smartcasted_type_source_id_2 := t.node_type(smartcasted)

			if wrapper_value := t.single_value_wrapper_expr_value(smartcasted, smartcasted_type_source_id_2,
				resolved_sum_source_id_6)
			{
				return wrapper_value
			}
		}
	}
	if has_expr_smartcast {
		smartcast_variant_source_id := t.resolve_variant(expr_smartcast.sum_type_name,
			expr_smartcast.variant_name)

		if wrapper_value := t.single_value_wrapper_sum_value(expr_id, smartcast_variant_source_id,
			resolved_sum_source_id_6)
		{
			return wrapper_value
		}
	}
	if wrapper_value := t.single_value_wrapper_sum_value(expr_id, flat.source_type_id(clean_variant), resolved_sum_source_id_6) {
		return wrapper_value
	}
	short_variant_comparison_type_id := t.variant_short_name(flat.source_type_id(clean_variant))

	container_variant := clean_variant.starts_with('[]') || clean_variant.starts_with('map[')
		|| t.is_fixed_array_type(flat.source_type_id(clean_variant))
	mut matches := false
	mut matched_variant := clean_variant
	for v_source_id in t.sum_types.type_ids(resolved_sum) or { []u32{} } {
		v := flat.source_type_name(v_source_id)

		short_v_comparison_type_id := t.variant_short_name(v_source_id)

		v_is_container := v.starts_with('[]') || v.starts_with('map[') || t.is_fixed_array_type(v_source_id)
		// Container element/value types are storage-significant. Do not make
		// `[]json2.Any` match `[]toml.Any` (or analogous maps/fixed arrays) just
		// because their nested types share the same short name.
		if t.variant_names_match(v_source_id, flat.source_type_id(clean_variant))
			|| (!v_is_container && !container_variant && short_v_comparison_type_id == short_variant_comparison_type_id) {
			matches = true
			matched_variant = v
			break
		}
	}
	if !matches {
		for v_source_id_2 in t.sum_types.type_ids(resolved_sum) or { []u32{} } {
			v := flat.source_type_name(v_source_id_2)

			if t.sum_variant_type_accepts_value_type(v_source_id_2, flat.source_type_id(clean_variant)) {
				matches = true
				matched_variant = v
				break
			}
		}
	}
	if !matches && t.is_integer_type_name(flat.source_type_id(clean_variant)) {
		mut enum_variant := ''
		for v_source_id_3 in t.sum_types.type_ids(resolved_sum) or { []u32{} } {
			v := flat.source_type_name(v_source_id_3)

			if (t.enum_type_name_for_expected(v_source_id_3, t.cur_module)) == 0 {
				continue
			}
			if enum_variant != '' {
				enum_variant = ''
				break
			}
			enum_variant = v
		}
		if enum_variant.len > 0 {
			matches = true
			matched_variant = enum_variant
		}
	}
	if !matches {
		path := flat.TypeTextList{ ids: t.sum_variant_path(resolved_sum_source_id_6, flat.source_type_id(clean_variant)) }.names()
		if path.len == 1 {
			matches = true
			matched_variant = path[0]
		} else if path.len > 1 {
			nested_sum_source_id := t.resolve_sum_name(t.trim_pointer_type(flat.source_type_id(path[0])))
			nested_sum := flat.source_type_name(nested_sum_source_id)
			if nested_sum_source_id != 0 && t.sum_types.contains(nested_sum) {
				nested_value := t.wrap_sum_value_with_storage(expr_id, nested_sum_source_id, detach_array_payload)
				return t.make_sum_literal(resolved_sum_source_id_6, flat.source_type_id(path[0]), nested_value)
			}
		}
	}
	if !matches {
		return t.transform_expr(expr_id)
	}
	ref_variant := t.variant_references_sum(flat.source_type_id(matched_variant), resolved_sum_source_id_6)
		&& !t.sum_variant_is_direct_pointer(flat.source_type_id(matched_variant))
	mut pointer_variant_child := flat.empty_node
	if expr.kind == .prefix && expr.op == .mul && expr.children_count > 0 {
		pointer_variant_child = t.a.child(&expr, 0)
	}
	matched_clean_source_id := t.trim_pointer_type(t.normalize_type_alias(flat.source_type_id(matched_variant)))
	matched_clean_comparison_type_id := matched_clean_source_id

	value_clean_source_id := t.trim_pointer_type(t.normalize_type_alias(flat.source_type_id(clean_variant)))
	value_clean_comparison_type_id := value_clean_source_id

	needs_variant_conversion := matched_clean_comparison_type_id != value_clean_comparison_type_id
		&& t.sum_variant_type_accepts_value_type(matched_clean_source_id, value_clean_source_id)
	mut source_pointer_value := false
	mut source_pointer_type := ''
	if expr.kind == .ident && expr.value.len > 0 {
		source_type := flat.source_type_name(t.var_type(expr.value))
		source_pointer_type = source_type
		source_pointer_value = source_type.starts_with('&')
			&& t.variant_names_match(flat.source_type_id(source_type[1..]), flat.source_type_id(matched_variant))
	}
	mut inner := if int(pointer_variant_child) >= 0 && ref_variant {
		t.transform_expr_preserving_pointer_value(pointer_variant_child)
	} else if ref_variant && source_pointer_value {
		value := t.make_ident(expr.value)
		t.set_node_typ(int(value), flat.source_type_id(source_pointer_type))
		value
	} else if ref_variant && !has_expr_smartcast
		&& (expr_type.starts_with('&') || source_pointer_value) {
		t.transform_expr_preserving_pointer_value(expr_id)
	} else if needs_variant_conversion {
		t.transform_expr_for_type(expr_id, flat.source_type_id(matched_variant))
	} else {
		t.transform_expr(expr_id)
	}
	if detach_array_payload {
		inner = t.sum_owned_value_payload(inner, flat.source_type_id(matched_variant))
		inner = t.clone_owned_array_storage_value(inner, flat.source_type_id(matched_variant), t.array_storage_source_is_mut_param(expr_id))
	}
	if ref_variant {
		return t.make_sum_literal(storage_sum_source_id, flat.source_type_id(matched_variant), inner)
	}
	start := t.a.children.len
	t.a.children << inner
	cast := t.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          storage_sum
		children_start: start
		children_count: 1
		typ:            storage_sum_source_id
	})
	return if t.is_owned_array_storage_value(inner) {
		t.mark_owned_array_storage_value(cast, storage_sum_source_id)
	} else {
		cast
	}
}

fn (t &Transformer) sum_literal_type_name(target_sum_id u32, resolved_sum_id u32) u32 {
	resolved_sum := flat.source_type_name(resolved_sum_id)

	clean_source_id := t.trim_pointer_type(t.normalize_type_alias(target_sum_id))
	clean := flat.source_type_name(clean_source_id)
	if clean_source_id == 0 {
		return resolved_sum_id
	}
	base, _, is_generic := generic_app_parts(clean)
	if is_generic && flat.source_type_name(t.resolve_sum_name(flat.source_type_id(base))) == resolved_sum {
		return clean_source_id
	}
	if t.sum_types.contains(clean) {
		return clean_source_id
	}
	return resolved_sum_id
}

fn (mut t Transformer) single_value_wrapper_sum_value(expr_id flat.NodeId, wrapper_type_id u32, target_sum_id u32) ?flat.NodeId {
	target_sum := flat.source_type_name(target_sum_id)

	if wrapper_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	lookup := t.lookup_struct_info_for_field(wrapper_type_id, 'value') or { return none }
	if lookup.info.fields.len != 1 {
		return none
	}
	value_type_source_id := t.lookup_struct_field_type(wrapper_type_id, 'value') or { return none }

	if flat.source_type_name(t.resolve_sum_name(t.normalize_type_alias(value_type_source_id))) != target_sum {
		return none
	}
	wrapper := t.transform_expr(expr_id)
	value := t.make_selector(wrapper, 'value', value_type_source_id)
	t.set_node_typ(int(value), value_type_source_id)
	return value
}

fn (mut t Transformer) single_value_wrapper_expr_value(wrapper flat.NodeId, wrapper_type_id u32, target_sum_id u32) ?flat.NodeId {
	target_sum := flat.source_type_name(target_sum_id)

	if wrapper_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	lookup := t.lookup_struct_info_for_field(wrapper_type_id, 'value') or { return none }
	if lookup.info.fields.len != 1 {
		return none
	}
	value_type_source_id := t.lookup_struct_field_type(wrapper_type_id, 'value') or { return none }

	if flat.source_type_name(t.resolve_sum_name(t.normalize_type_alias(value_type_source_id))) != target_sum {
		return none
	}
	value := t.make_selector(wrapper, 'value', value_type_source_id)
	t.set_node_typ(int(value), value_type_source_id)
	return value
}

// ensure_sum_variant_ref supports ensure sum variant ref handling for Transformer.
fn (mut t Transformer) ensure_sum_variant_ref(value flat.NodeId, variant_id u32) flat.NodeId {
	variant := flat.source_type_name(variant_id)
	mut value_type := flat.source_type_name(t.node_type(value))
	if value_type.starts_with('&') {
		return value
	}
	clean_variant := if variant.starts_with('&') { variant[1..] } else { variant }
	if value_type.len == 0 {
		value_type = clean_variant
	}
	if t.a.nodes[int(value)].kind != .struct_init && t.expr_can_take_address(value) {
		ref := t.make_prefix(.amp, value)
		t.set_node_typ(int(ref), flat.source_type_id('&${clean_variant}'))
		return ref
	}
	tmp_name := t.new_temp('sum_val')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, flat.source_type_id(value_type))
	ref := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.set_node_typ(int(ref), flat.source_type_id('&${clean_variant}'))
	return ref
}

// make_default_sum_value initializes a sum type with the zero value of its first variant.
fn (mut t Transformer) make_default_sum_value(typ_id u32) ?flat.NodeId {
	resolved_sum_source_id, variants_source_ids_3 := t.concrete_sum_name_and_variants(t.normalize_type_alias(typ_id))

	if variants_source_ids_3.len == 0 {
		return none
	}
	variant_source_id := variants_source_ids_3[0]

	mut value := t.zero_value_for_type(variant_source_id)
	value_node := t.a.nodes[int(value)]
	if value_node.kind == .struct_init && value_node.children_count == 0 {
		// A sum's default variant is a real zero-initialized V value, including
		// defaults declared on the variant's fields. The synthetic empty literal is
		// created after the ordinary expression walk, so expand those defaults here.
		value = t.transform_struct_init(value, value_node)
	}
	return t.make_sum_literal(resolved_sum_source_id, variant_source_id, value)
}

// By-value container variants own their boxes. ABI pointers borrow the caller
// header; nested sums also need independent active payload boxes before boxing.
fn (mut t Transformer) sum_owned_value_payload(value flat.NodeId, variant_id u32) flat.NodeId {
	variant_type_source_id := t.comptime_normalize_type_alias_chain(variant_id)
	variant_type := flat.source_type_name(variant_type_source_id)
	value_type_source_id := t.comptime_normalize_type_alias_chain(t.node_type(value))
	value_type := flat.source_type_name(value_type_source_id)
	mut payload_type := variant_type
	for t.is_optional_type_name(flat.source_type_id(payload_type)) {
		payload_type = flat.source_type_name(t.comptime_normalize_type_alias_chain(t.optional_base_type(flat.source_type_id(payload_type))))
	}
	if value_type == '&${variant_type}' && (payload_type.starts_with('[]') || t.is_sum_type_name(flat.source_type_id(payload_type))) {
		payload := t.array_lvalue_value(value, value_type_source_id)
		t.set_node_typ(int(payload), variant_id)
		if t.is_owned_array_storage_value(value) {
			return t.mark_owned_array_storage_value(payload, variant_id)
		}
		if t.is_sum_type_name(variant_type_source_id) && !isnil(t.tc)
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(variant_type_source_id)) {
			if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(variant_type_source_id)) {
				// An empty borrowed array can still be retained; its runtime guard rejects
				// nonempty uncloneable payloads without destroying the caller's boxes.
				return t.clone_owned_sum_array_views_for_storage(payload, variant_id, false, true)
			}
			cloned := t.make_compiler_default_borrowed_clone_value(payload, variant_id, true)
			return t.mark_owned_array_storage_value(cloned, variant_id)
		}
		return payload
	}
	return value
}

// make_sum_literal builds make sum literal data for transform.
fn (mut t Transformer) make_sum_literal(sum_name_id u32, variant_id u32, value flat.NodeId) flat.NodeId {
	sum_name := flat.source_type_name(sum_name_id)

	qvariant_source_id := t.resolve_variant(sum_name_id, variant_id)
	qvariant := flat.source_type_name(qvariant_source_id)
	typ_field := t.make_sum_literal_field('typ', t.make_int_literal(t.sum_type_index(sum_name_id,
		qvariant_source_id)), flat.source_type_int)
	payload := t.sum_owned_value_payload(value, qvariant_source_id)
	raw_value_type := flat.source_type_name(t.node_type(payload))
	mut value_type := if raw_value_type.starts_with('&') {
		raw_value_type
	} else {
		qvariant
	}
	if t.variant_references_sum(qvariant_source_id, sum_name_id) && !t.sum_variant_is_direct_pointer(qvariant_source_id)
		&& !value_type.starts_with('&') {
		value_type = '&${qvariant}'
	}
	value_field := t.make_sum_literal_field(t.sum_field_name(qvariant_source_id), payload, flat.source_type_id(value_type))
	start := t.a.children.len
	t.a.children << typ_field
	t.a.children << value_field
	literal := t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          sum_name
		typ:            sum_name_id
	})
	return if t.is_owned_array_storage_value(payload) {
		t.mark_owned_array_storage_value(literal, sum_name_id)
	} else {
		literal
	}
}

fn (mut t Transformer) make_sum_ref_literal(sum_name_id u32, variant_id u32, value flat.NodeId) flat.NodeId {
	sum_name := flat.source_type_name(sum_name_id)

	qvariant_source_id := t.resolve_variant(sum_name_id, variant_id)
	qvariant := flat.source_type_name(qvariant_source_id)
	typ_field := t.make_sum_literal_field('typ', t.make_int_literal(t.sum_type_index(sum_name_id,
		qvariant_source_id)), flat.source_type_int)
	value_field := t.make_sum_literal_field(t.sum_field_name(qvariant_source_id), value,
		flat.source_type_id('sum_ref ${qvariant}'))
	start := t.a.children.len
	t.a.children << typ_field
	t.a.children << value_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          sum_name
		typ:            sum_name_id
	})
}

// make_sum_literal_field builds make sum literal field data for transform.
fn (mut t Transformer) make_sum_literal_field(name string, value flat.NodeId, typ_id u32) flat.NodeId {
	start := t.a.children.len
	t.a.children << value
	return t.a.add_node(flat.Node{
		kind:           .field_init
		children_start: start
		children_count: 1
		value:          name
		typ:            typ_id
	})
}

// variant_text resolves the indexed source type spelling.
@[inline]
fn (value &FixedArraySumLiteral) variant_text() string {
	return flat.source_type_name(value.variant)
}

// trimmed_type_id preserves an existing identity unless endpoint whitespace is removed.
fn (t &Transformer) trimmed_type_id(id u32) u32 {
	if !flat.source_type_shape(id).has(.padded) { return id }
	return flat.source_type_id(trimmed_transform_text(flat.source_type_name(id)))
}

fn (t &Transformer) unshared_type_id(id u32) u32 {
	if !flat.source_type_shape(id).has(.shared_type) { return id }
	if !isnil(t.alias_cache) {
		if base := t.alias_cache.shared_bases[id] { return base }
	}
	text := flat.source_type_name(id)
	base := flat.source_type_id(trimmed_transform_text(text[7..]))
	if !isnil(t.alias_cache) {
		mut cache := t.alias_cache
		cache.shared_bases[id] = base
	}
	return base
}

fn (t &Transformer) variadic_array_type_id(id u32) u32 {
	if !flat.source_type_shape(id).has(.variadic) { return id }
	if !isnil(t.alias_cache) {
		if array := t.alias_cache.variadic_arrays[id] { return array }
	}
	text := flat.source_type_name(id)
	elem := trimmed_transform_text(text[3..])
	array := flat.source_type_id('[]' + elem)
	if !isnil(t.alias_cache) {
		mut cache := t.alias_cache
		cache.variadic_arrays[id] = array
	}
	return array
}
