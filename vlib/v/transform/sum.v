module transform

import v.gen.c.naming { sum_field_name }
import v.flat
import v.types

// transform_without_pointer_prefix removes exactly one leading reference marker.
@[inline]
fn transform_without_pointer_prefix(typ u32) u32 {
	return if flat.source_type_shape(typ).has(.pointer) {
		flat.source_type_syntax(typ).raw_base()
	} else {
		typ
	}
}

struct FixedArraySumLiteral {
	variant u32
	expr    flat.NodeId
}

fn (mut t Transformer) mark_sum_moving_read(source flat.NodeId, value flat.NodeId) flat.NodeId {
	if isnil(t.tc) || !t.tc.ownership_sum_read_moves_value(source) { return value }
	typ := t.original_expr_type(source)
	if !t.is_sum_type_name(typ) { return value }
	start := t.a.children.len
	t.a.children << value
	return t.a.add_node(flat.Node{
		kind:           .paren
		children_start: start
		children_count: 1
		value:          '__v3_moved_sum_value'
		typ:            typ
	})
}

// trim_pointer_type transforms trim pointer type data for transform.
fn (t &Transformer) trim_pointer_type(typ_id u32) u32 {
	if !flat.source_type_shape(typ_id).has(.pointer) { return typ_id }
	if !isnil(t.alias_cache) {
		if base := t.alias_cache.pointer_bases[typ_id] { return base }
	}
	base := flat.source_type_syntax(typ_id).raw_base()
	if !isnil(t.alias_cache) {
		mut cache := t.alias_cache
		cache.pointer_bases[typ_id] = base
	}
	return base
}

fn (t &Transformer) trim_all_pointer_type(typ_id u32) u32 {
	mut clean := typ_id
	for {
		next := t.trim_pointer_type(clean)
		if next == clean {
			return clean
		}
		clean = next
	}
}

fn interface_pattern_is_collapsed_container_type(name_id u32) bool {
	shape := flat.source_type_shape(name_id)
	if !shape.has(.padded) {
		return shape.has(.array_type) || shape.has(.map_type)
	}
	clean_shape := flat.source_type_shape(transform_trimmed_type_id(name_id))
	return clean_shape.has(.array_type) || clean_shape.has(.map_type)
}

fn (mut t Transformer) pointer_sum_access_expr(expr_id flat.NodeId, expr_type_id u32) (flat.NodeId, u32, flat.Op) {
	has_smartcast := t.expr_has_smartcast(expr_id)
	mut access := if has_smartcast {
		// A runtime tag check always starts from the stored sum/interface value.
		// Applying an active smartcast here would inspect the extracted variant.
		t.make_plain_expr_for_smartcast(expr_id)
	} else {
		t.transform_selector_base_expr(expr_id)
	}
	mut access_type := expr_type_id
	if has_smartcast {
		raw_type := t.raw_expr_type_without_smartcast(expr_id)
		if raw_type != 0 {
			access_type = raw_type
		}
	}
	for flat.source_type_shape(access_type).has(.pointer_chain) {
		access = t.make_prefix(.mul, access)
		access_type = t.trim_pointer_type(access_type)
		t.set_node_typ(int(access), access_type)
	}
	mut value_type := (if has_smartcast {
		access_type
	} else {
		t.node_type(access)
	})
	if value_type == 0 {
		value_type = access_type
	}
	op := if flat.source_type_shape(value_type).has(.pointer) { flat.Op.arrow } else { flat.Op.dot }
	return access, value_type, op
}

// resolve_variant resolves resolve variant information for transform.
fn (t &Transformer) resolve_variant(sum_name_id u32, variant_id u32) u32 {
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
	if flat.source_type_shape(variant_id).has(.dotted) {
		return variant_id
	}
	resolved_sum_source_id := t.resolve_sum_name(sum_name_id)
	if resolved_variant_source_id := t.sum_variant_name(resolved_sum_source_id, variant_id) {
		return resolved_variant_source_id
	}
	if flat.source_type_shape(resolved_sum_source_id).has(.dotted) {
		return flat.source_type_composition_two(.qualified, flat.source_type_syntax(resolved_sum_source_id).qualifier(), variant_id)
	}
	if flat.source_type_shape(sum_name_id).has(.dotted) {
		return flat.source_type_composition_two(.qualified, flat.source_type_syntax(sum_name_id).qualifier(), variant_id)
	}
	return variant_id
}

fn (t &Transformer) generic_sum_arg_variant_for_pattern(sum_name_id u32, variant_id u32) ?u32 {
	syntax := flat.source_type_syntax(sum_name_id)
	if !syntax.generic_app_valid() {
		return none
	}
	args := syntax.generic_app_args()
	params := t.sum_generic_params_for_base_id(syntax.generic_app_base())
	if params.len == args.len {
		concrete_pattern := substitute_generic_type_text_with_params(variant_id, args, params)
		for concrete_variant_source_id in t.concrete_sum_variants_for_candidate(sum_name_id) {
			if t.variant_names_match(concrete_variant_source_id, concrete_pattern)
				|| t.variant_names_match(concrete_variant_source_id, variant_id) {
				return concrete_variant_source_id
			}
		}
		if concrete_pattern != variant_id {
			return concrete_pattern
		}
	}
	variant_syntax := flat.source_type_syntax(variant_id)
	variant_args := variant_syntax.generic_app_args()
	if variant_syntax.generic_app_valid() && variant_args.len == args.len
		&& t.generic_variant_args_are_open(variant_args) {
		concrete_pattern := substitute_generic_type_text_with_params(variant_id, args, flat.TypeTextList{ ids: variant_args }.names())
		for concrete_variant_source_id_2 in t.concrete_sum_variants_for_candidate(sum_name_id) {
			if t.variant_names_match(concrete_variant_source_id_2, concrete_pattern)
				|| t.concrete_variant_matches_generic_pattern(concrete_variant_source_id_2, concrete_pattern) {
				return concrete_variant_source_id_2
			}
		}
		if concrete_pattern != variant_id {
			return concrete_pattern
		}
	}
	for arg in args {
		if t.variant_names_match(arg, variant_id) {
			return arg
		}
	}
	return none
}

// resolve_sum_name resolves resolve sum name information for transform.
fn (t &Transformer) resolve_sum_name(type_id u32) u32 {
	if type_id == 0 { return 0 }
	if isnil(t.sum_cache) {
		return t.resolve_sum_name_uncached(type_id)
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
	result := t.resolve_sum_name_uncached(type_id)
	c.entries[type_id] = result
	c.put_recent(type_id, result)
	return result
}

// resolve_sum_name_uncached resolves resolve sum name uncached information for transform.
fn (t &Transformer) resolve_sum_name_uncached(sum_name_id u32) u32 {
	if sum_name_id == 0 { return 0 }
	shape := flat.source_type_shape(sum_name_id)
	if shape.has(.pointer) || shape.has(.option_type) || shape.has(.result_type)
		|| shape.has(.leading_bracket) || shape.has(.map_type) {
		return 0
	}
	sum_name := sum_name_id
	if t.sum_types.contains(sum_name) {
		return sum_name_id
	}
	// Do not let a short/suffix fallback reinterpret an exact concrete type as an
	// unrelated same-named sum type from another module.
	if sum_name in t.structs || (!isnil(t.tc) && (sum_name_id in t.tc.structs
		|| sum_name_id in t.tc.interface_names || sum_name_id in t.tc.enum_names)) {
		return 0
	}
	if resolved_c_name_source_id := t.resolve_sum_name_from_c_name(sum_name_id) {
		return resolved_c_name_source_id
	}
	// A container or function involving a sum type is not itself a sum type: `[]ast.Value` must
	// not resolve to `ast.Value` (the short-name and generic-application
	// fallbacks below would), or an or/assign lowering boxes the whole array
	// into one sum value.
	if shape.has(.array_type) || shape.has(.map_type) || shape.has(.leading_bracket)
		|| shape.has(.function_compact) || shape.has(.function_spaced) {
		return 0
	}
	// Neither is an option or result of a sum type: the short-name fallback below
	// would resolve `?css.StyleValue` (or `?&dom.StyleValue`) to the `StyleValue`
	// sum, and the value of an `x := if c { ?T(a) } else { ?T(none) }` would be
	// lowered into a payload-typed temporary.
	if shape.has(.option_type) || shape.has(.result_type) {
		return 0
	}
	if !isnil(t.tc) && t.tc.sum_types.contains(sum_name_id) {
		return sum_name_id
	}
	syntax := flat.source_type_syntax(sum_name_id)
	if syntax.generic_app_valid() {
		generic_base_id := syntax.generic_app_base()
		concrete_sum := t.resolve_concrete_generic_sum_name(sum_name_id, generic_base_id, syntax.generic_app_args())
		if concrete_sum != 0 {
			return concrete_sum
		}
		resolved_base := t.resolve_sum_name_uncached(generic_base_id)
		resolved_base_name := resolved_base
		if t.sum_types.contains(resolved_base_name) {
			return resolved_base
		}
		if !isnil(t.tc) && t.tc.sum_types.contains(resolved_base) {
			return resolved_base
		}
	}
	has_dot := shape.has(.dotted)
	if has_dot {
		// Import-aliased module path: `tast.Value` names `sub.tast.Value`.
		// The full-suffix match runs before the bare short-name fallback so an
		// unrelated short `Value` sum cannot shadow the aliased one; ambiguous
		// suffix matches resolve nothing.
		mut suffix_match := u32(0)
		mut suffix_ambiguous := false
		for key_id, _ in t.sum_types.ids {
			if sum_name_id in flat.source_type_syntax(key_id).qualifier_suffixes() {
				if suffix_match != 0 && suffix_match != key_id {
					suffix_ambiguous = true
					break
				}
				suffix_match = key_id
			}
		}
		if !suffix_ambiguous && !isnil(t.tc) {
			for key_source_id, _ in t.tc.sum_types.ids {
				if sum_name_id in flat.source_type_syntax(key_source_id).qualifier_suffixes() {
					key_id := key_source_id
					if suffix_match != 0 && suffix_match != key_id {
						suffix_ambiguous = true
						break
					}
					suffix_match = key_id
				}
			}
		}
		if !suffix_ambiguous && suffix_match != 0 {
			return suffix_match
		}
		short_sum_id := syntax.unqualified()
		short_sum := short_sum_id
		if t.sum_types.contains(short_sum) {
			return short_sum_id
		}
	}
	if !has_dot && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qsum_id := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), sum_name_id)
		qsum := qsum_id
		if t.sum_types.contains(qsum) {
			return qsum_id
		}
	}
	if !has_dot {
		mut found := u32(0)
		for key_id, _ in t.sum_types.ids {
			if flat.source_type_shape(key_id).has(.dotted)
				&& flat.source_type_syntax(key_id).unqualified() == sum_name_id {
				if found != 0 && found != key_id {
					found = 0
					break
				}
				found = key_id
			}
		}
		if found != 0 {
			return found
		}
	}
	if !isnil(t.tc) {
		if t.tc.sum_types.contains(sum_name_id) {
			return sum_name_id
		}
		if has_dot {
			short_sum_id := syntax.unqualified()
			if t.tc.sum_types.contains(short_sum_id) {
				return short_sum_id
			}
		}
		if !has_dot && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qsum_id := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), sum_name_id)
			if t.tc.sum_types.contains(qsum_id) {
				return qsum_id
			}
		}
		if !has_dot {
			mut found := u32(0)
			for key_source_id, _ in t.tc.sum_types.ids {
				if flat.source_type_shape(key_source_id).has(.dotted)
					&& flat.source_type_syntax(key_source_id).unqualified() == sum_name_id {
					key_id := key_source_id
					if found != 0 && found != key_id {
						found = 0
						break
					}
					found = key_id
				}
			}
			if found != 0 {
				return found
			}
		}
	}
	return sum_name_id
}

fn (t &Transformer) resolve_sum_name_from_c_name(sum_name_id u32) ?u32 {
	if flat.source_type_shape(sum_name_id).has(.bracketed) || !flat.source_type_syntax(sum_name_id).underscored() {
		return none
	}
	for key_id, _ in t.sum_types.ids {
		if flat.source_type_shape(key_id).has(.bracketed)
			&& flat.source_type_id(t.lookup_c_name(flat.source_type_name(key_id))) == sum_name_id {
			return key_id
		}
	}
	if !isnil(t.tc) {
		for key_source_id, _ in t.tc.sum_types.ids {
			key_id := key_source_id
			if flat.source_type_shape(key_id).has(.bracketed)
				&& flat.source_type_id(t.lookup_c_name(flat.source_type_name(key_id))) == sum_name_id {
				return key_id
			}
		}
	}
	return none
}

fn transform_sum_generic_type(base u32, args []u32) u32 {
	mut parts := []u32{cap: args.len + 1}
	parts << base
	parts << args
	return flat.source_type_composition(.generic, parts)
}

fn (t &Transformer) resolve_concrete_generic_sum_name(sum_name_id u32, base_id u32, args []u32) u32 {
	if !flat.source_type_shape(base_id).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base_id)
		qspec_id := transform_sum_generic_type(qualified, args)
		qspec := qspec_id
		if t.sum_types.contains(qspec) || (!isnil(t.tc) && t.tc.sum_types.contains(qspec_id)) {
			return qspec_id
		}
	}
	if !flat.source_type_shape(sum_name_id).has(.dotted) {
		short_spec := t.find_sum_type_with_short_name(sum_name_id)
		if short_spec != 0 {
			return short_spec
		}
	}
	resolved_base := t.resolve_sum_name_uncached(base_id)
	if resolved_base != 0 && resolved_base != base_id {
		resolved_spec_id := transform_sum_generic_type(resolved_base, args)
		resolved_spec := resolved_spec_id
		if t.sum_types.contains(resolved_spec) || (!isnil(t.tc) && t.tc.sum_types.contains(resolved_spec_id)) {
			return resolved_spec_id
		}
	}
	return 0
}

fn (t &Transformer) find_sum_type_with_short_name(short_name_id u32) u32 {
	mut found := u32(0)
	for key_id, _ in t.sum_types.ids {
		if flat.source_type_shape(key_id).has(.dotted)
			&& flat.source_type_syntax(key_id).unqualified() == short_name_id {
			if found != 0 && found != key_id {
				return 0
			}
			found = key_id
		}
	}
	if !isnil(t.tc) {
		for key_source_id, _ in t.tc.sum_types.ids {
			if flat.source_type_shape(key_source_id).has(.dotted)
				&& flat.source_type_syntax(key_source_id).unqualified() == short_name_id {
				key_id := key_source_id
				if found != 0 && found != key_id {
					return 0
				}
				found = key_id
			}
		}
	}
	return found
}

fn (t &Transformer) sum_subject_type_candidates(subject_type_id u32) []u32 {
	clean_id := t.trimmed_type_id(t.trim_pointer_type(t.normalize_type_alias(subject_type_id)))
	if clean_id == 0 { return []u32{} }
	mut candidates := []u32{}
	mut seen := map[u32]bool{}
	syntax := flat.source_type_syntax(clean_id)
	if syntax.generic_app_valid() {
		base := syntax.generic_app_base()
		args := syntax.generic_app_args()
		scoped_args := t.sum_subject_type_args_in_scope(args)
		plain := transform_sum_generic_type(base, args)
		scoped := transform_sum_generic_type(base, scoped_args)
		if scoped != plain { push_sum_subject_type_candidate(mut candidates, mut seen, scoped) }
		push_sum_subject_type_candidate(mut candidates, mut seen, clean_id)
		if !flat.source_type_shape(base).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base)
			push_sum_subject_type_candidate(mut candidates, mut seen, transform_sum_generic_type(qualified, args))
			if scoped != plain {
				push_sum_subject_type_candidate(mut candidates, mut seen, transform_sum_generic_type(qualified, scoped_args))
			}
		}
		resolved := t.resolve_sum_name(base)
		if resolved != 0 && resolved != base {
			push_sum_subject_type_candidate(mut candidates, mut seen, transform_sum_generic_type(resolved, args))
			if scoped != plain {
				push_sum_subject_type_candidate(mut candidates, mut seen, transform_sum_generic_type(resolved, scoped_args))
			}
		}
	} else {
		push_sum_subject_type_candidate(mut candidates, mut seen, clean_id)
		if !flat.source_type_shape(clean_id).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			push_sum_subject_type_candidate(mut candidates, mut seen, flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), clean_id))
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
	clean := transform_trimmed_type_id(arg_id)
	if clean == 0 { return 0 }
	shape := flat.source_type_shape(clean)
	syntax := flat.source_type_syntax(clean)
	if shape.has(.pointer) || shape.has(.mutable_type) {
		return flat.source_type_composition_one(.pointer, t.sum_subject_type_arg_in_scope(syntax.raw_base()))
	}
	if shape.has(.option_type) {
		return flat.source_type_composition_one(.option, t.sum_subject_type_arg_in_scope(syntax.raw_base()))
	}
	if shape.has(.result_type) {
		return flat.source_type_composition_one(.result, t.sum_subject_type_arg_in_scope(syntax.raw_base()))
	}
	if shape.has(.variadic) {
		return flat.source_type_composition_one(.variadic_parameter, t.sum_subject_type_arg_in_scope(syntax.raw_base()))
	}
	if shape.has(.array_type) {
		return flat.source_type_composition_one(.array, t.sum_subject_type_arg_in_scope(syntax.raw_base()))
	}
	if shape.has(.map_type) && syntax.bracket_closed() {
		key := t.sum_subject_type_arg_in_scope(syntax.bracket_raw_inner())
		value := t.sum_subject_type_arg_in_scope(syntax.raw_base())
		return flat.source_type_composition_two(.map, key, value)
	}
	if shape.has(.leading_bracket) && syntax.bracket_closed() {
		return flat.source_type_composition_two(.concat, syntax.bracket_prefix(), t.sum_subject_type_arg_in_scope(syntax.raw_base()))
	}
	if syntax.generic_app_valid() {
		mut args := []u32{cap: syntax.generic_app_args().len + 1}
		args << t.sum_subject_type_arg_base_in_scope(syntax.generic_app_base())
		for nested in syntax.generic_app_args() { args << t.sum_subject_type_arg_in_scope(nested) }
		return flat.source_type_composition(.generic, args)
	}
	return t.sum_subject_type_arg_base_in_scope(clean)
}

fn (t &Transformer) sum_subject_type_arg_base_in_scope(name_id u32) u32 {
	if flat.source_type_shape(name_id).has(.dotted) || isnil(t.tc) {
		return t.normalize_sum_variant_type(name_id, t.cur_module, [])
	}
	for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, flat.source_type_name(name_id))] or {
		[]string{}
	} {
		if t.tc.type_aliases.contains(flat.source_type_id(candidate)) || flat.source_type_id(candidate) in t.tc.structs
			|| flat.source_type_id(candidate) in t.tc.interface_names || flat.source_type_id(candidate) in t.tc.flag_enums
			|| flat.source_type_id(candidate) in t.tc.enum_names || t.tc.sum_types.contains(flat.source_type_id(candidate)) {
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
	resolved := t.resolve_sum_name(name_id)
	return t.sum_types.contains(resolved)
}

fn (t &Transformer) sum_target_accepts_variant_type(target_type_id u32, variant_type_id u32) bool {
	if target_type_id == u32(0) || variant_type_id == u32(0) {
		return false
	}
	resolved_raw_target := t.resolve_sum_name(t.trim_pointer_type(target_type_id))
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
	resolved_target := resolved_target_comparison_type_id
	if resolved_target_source_id == 0 || !t.sum_types.contains(resolved_target) {
		return false
	}
	if t.resolve_sum_name(clean_variant_source_id) == resolved_target_source_id {
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
	mut clean_variant_type := t.trim_pointer_type(t.normalize_type_alias(variant_type_id))
	mut clean_value_type := t.trim_pointer_type(t.normalize_type_alias(value_type_id))
	if t.variant_names_match(clean_variant_type, clean_value_type) {
		return true
	}
	if flat.source_type_shape(clean_variant_type).has(.array_type) && flat.source_type_shape(clean_value_type).has(.array_type) {
		clean_variant_type = flat.source_type_syntax(clean_variant_type).raw_base()
		clean_value_type = flat.source_type_syntax(clean_value_type).raw_base()
		// Keep module-qualified recursive sum element types distinct. `[]a.Any`
		// and `[]b.Any` are not interchangeable merely because both elements have
		// the short name `Any`; only accept them when the target sum really accepts
		// the source element type.
		if t.is_sum_type_name(clean_variant_type) {
			return t.sum_target_accepts_variant_type(clean_variant_type, clean_value_type)
		}
		return t.variant_names_match(clean_variant_type, clean_value_type)
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
	if flat.source_type_shape(variant_id).has(.dotted) { return variant_id }
	if variant_id in t.structs { return variant_id }
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), variant_id)
		if qualified in t.structs { return qualified }
	}
	for candidate, _ in t.structs {
		if flat.source_type_shape(candidate).has(.dotted) && flat.source_type_syntax(candidate).unqualified() == variant_id {
			return candidate
		}
	}
	return variant_id
}

// smartcast_target_type supports smartcast target type handling for Transformer.
fn (t &Transformer) smartcast_target_type(sc SmartcastContext) u32 {
	if sc.sum_type_name == flat.source_type_id(option_unwrap_marker) {
		return sc.variant_name
	}
	if flat.source_type_shape(sc.variant_name).has(.pointer) {
		return flat.source_type_composition_one(.pointer, t.resolve_variant(sc.sum_type_name, flat.source_type_syntax(sc.variant_name).raw_base()))
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
	sum_name := sum_name_id
	// A bare name the checker registered is the storage name of a main or builtin sum
	// type, also inside an imported module's specialization. The candidate lookup below
	// would rebind it to a same-named sum of the current module (`Any` -> `json2.Any`)
	// and tag the value with that sum's variant indices.
	if !isnil(t.tc) && t.cur_module !in ['', 'main', 'builtin'] && !flat.source_type_shape(sum_name_id).has(.dotted)
		&& t.tc.sum_types.contains(sum_name_id) && flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), sum_name_id) in t.tc.sum_types.ids {
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
	name := resolved
	if variants := t.sum_types.type_ids(name) { return resolved, variants.clone() }
	if !isnil(t.tc) {
		if variants := t.tc.sum_types.type_ids(resolved) { return resolved, variants.clone() }
	}
	return resolved, []u32{}
}

fn (t &Transformer) concrete_sum_variants_for_candidate(sum_name_id u32) []u32 {
	name := sum_name_id
	syntax := flat.source_type_syntax(sum_name_id)
	args := syntax.generic_app_args()
	is_generic := syntax.generic_app_valid()
	if variants := t.sum_types.type_ids(name) {
		if is_generic {
			params := t.sum_generic_params_for_base_id(syntax.generic_app_base())
			if params.len == args.len && params.len > 0 {
				return substitute_sum_variant_list(variants, args, params)
			}
		}
		return variants.clone()
	}
	if !isnil(t.tc) {
		if variants := t.tc.sum_types.type_ids(sum_name_id) {
			if is_generic {
				params := t.sum_generic_params_for_base_id(syntax.generic_app_base())
				if params.len == args.len && params.len > 0 {
					return substitute_sum_variant_list(variants, args, params)
				}
			}
			return variants.clone()
		}
	}
	if !is_generic { return []u32{} }
	for candidate in t.sum_subject_type_candidates(syntax.generic_app_base()) {
		candidate_name := candidate
		params := t.sum_generic_params_for_base_id(candidate)
		if params.len == 0 || params.len != args.len { continue }
		if variants := t.sum_types.type_ids(candidate_name) {
			return substitute_sum_variant_list(variants, args, params)
		}
		if !isnil(t.tc) {
			if variants := t.tc.sum_types.type_ids(candidate) {
				return substitute_sum_variant_list(variants, args, params)
			}
		}
	}
	return []u32{}
}

fn (t &Transformer) sum_generic_params_for_base(base string) []string {
	return t.sum_generic_params_for_base_id(flat.source_type_id(base))
}

fn (t &Transformer) sum_generic_params_for_base_id(base u32) []string {
	if isnil(t.tc) { return []string{} }
	if params := t.tc.sum_generic_params[base] { return params }
	short := flat.source_type_syntax(base).unqualified()
	if params := t.tc.sum_generic_params[short] { return params }
	return []string{}
}

fn substitute_sum_variant_list(variants []u32, args []u32, params []string) []u32 {
	mut concrete := []u32{cap: variants.len}
	for variant in variants {
		concrete << substitute_generic_type_text_with_params(variant, args, params)
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
	if check := t.smartcasted_sum_is_expr_check(expr_id, node.source_value_id()) {
		return check
	}
	mut expr_type_source_id := t.node_type(expr_id)
	if t.expr_has_smartcast(expr_id) {
		// Inside an `is` branch, `node_type` can report the smartcast target (an enum or
		// a fixed array) instead of the interface; test the tag of the stored box.
		raw_type := t.raw_expr_type_without_smartcast(expr_id)
		if raw_type != 0 && raw_type != expr_type_source_id
			&& t.is_interface_type_name(t.trim_pointer_type(raw_type)) {
			expr_type_source_id = raw_type
		}
	}
	expr_type_source_id_2 := expr_type_source_id

	clean_type0_source_id := t.trim_pointer_type(expr_type_source_id)
	clean_type0_source_id_2 := clean_type0_source_id
	clean_type0 := clean_type0_source_id_2
	concrete_expr_type_source_id := t.normalize_type_alias(clean_type0_source_id)

	resolved_expr_sum_source_id := t.resolve_sum_name(concrete_expr_type_source_id)

	// Inside a `$for v in T.variants` body the pattern is the loop variable;
	// the unroll substitutes it later, so there is nothing to validate yet.
	if t.cloning_comptime_for_depth == 0
		&& !t.validate_specialized_is_expr(concrete_expr_type_source_id, resolved_expr_sum_source_id, node.source_value_id()) {
		return t.make_bool_literal(false)
	}
	clean_type := (if t.sum_types.contains(clean_type0) {
		clean_type0_source_id_2
	} else if _ := t.resolve_sum_variant_pattern_for_subject(clean_type0_source_id, node.source_value_id()) {
		clean_type0_source_id_2
	} else {
		t.find_sum_type_for_variant(node.source_value_id())
	})
	if t.is_builtin_ierror_interface_name(clean_type0_source_id) && node.source_value_id() == flat.source_type_none {
		new_expr0, value_type_source_id_2, op := t.pointer_sum_access_expr(expr_id, expr_type_source_id)

		new_expr := t.stable_transformed_expr_for_reuse(new_expr0, value_type_source_id_2, 'ierror_is')
		typ := t.make_selector_op(new_expr, '_typ', flat.source_type_int, op)
		return t.make_ierror_none_type_check(typ, clean_type0_source_id)
	}
	if t.is_interface_type_name(clean_type0_source_id) {
		new_expr0, value_type_source_id_3, op := t.pointer_sum_access_expr(expr_id, expr_type_source_id)

		new_expr := t.stable_transformed_expr_for_reuse(new_expr0, value_type_source_id_3, 'iface_is')
		pattern_id := if t.is_builtin_ierror_interface_name(clean_type0_source_id) && node.source_value_id() == flat.source_type_none {
			flat.source_type_id('None__')
		} else {
			node.source_value_id()
		}
		// Interface runtime tags describe the stored concrete value, not whether the
		// source pattern asks to expose that value through a pointer smartcast.
		// Resolve `is &Concrete` against `Concrete`, while retaining the original
		// pattern in the smartcast context.
		concrete_pattern_name_source_id := t.trim_all_pointer_type(pattern_id)

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
			is_start := t.a.children.len
			t.a.children << new_expr
			return t.a.add_node(flat.Node{
				kind:           .is_expr
				source_value:   pattern_source_id
				children_start: is_start
				children_count: 1
				typ:            flat.source_type_bool
			})
		}
		object := t.make_selector_op(new_expr, '_object', flat.source_type_voidptr, op)
		return t.make_infix(.ne, object, t.a.add(.nil_literal))
	}
	resolved_clean_type := t.resolve_sum_name(clean_type)
	if clean_type == 0 || !t.sum_types.contains(resolved_clean_type) {
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
	mut check_expr_type := t.node_type(new_expr)
	if check_expr_type == 0 || check_expr_type == flat.source_type_unknown {
		check_expr_type = expr_type_source_id_2
	}
	if node.typ_text() != 'match_exact' {
		if check := t.make_sum_type_alias_pattern_check(new_expr, check_expr_type, clean_type,
			node.source_value_id())
		{
			return check
		}
	}
	if check := t.make_sum_type_pattern_check(new_expr, check_expr_type, clean_type, node.source_value_id()) {
		return check
	}
	return t.make_bool_literal(true)
}

fn (mut t Transformer) make_sum_type_alias_pattern_check(expr flat.NodeId, expr_type_id u32, sum_name_id u32, pattern_id u32) ?flat.NodeId {
	variants := t.sum_alias_equivalent_variants(sum_name_id, pattern_id)
	if variants.len <= 1 {
		return none
	}
	stable_expr := t.stable_transformed_expr_for_reuse(expr, expr_type_id, 'sum_is')
	mut check := flat.empty_node
	for variant in variants {
		next := t.make_sum_type_pattern_check(stable_expr, expr_type_id, sum_name_id, variant) or {
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

	variants := t.sum_type_variants_for_index(resolved_sum_source_id)
	if variants.len == 0 {
		return []u32{}
	}
	mut pattern_forms := []AliasEquivalentForms{}
	for name_source_id in t.interface_alias_equivalent_names(pattern_id) {
		pattern_forms << t.alias_equivalent_forms(name_source_id)
	}
	mut result := []u32{}
	for variant in variants {
		if t.type_name_matches_any_alias_equivalent(variant, pattern_forms) {
			result << variant
		}
	}
	return result
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
	resolved_sum := resolved_sum_source_id_4
	if resolved_sum_source_id_4 == 0 || !t.sum_types.contains(resolved_sum)
		|| t.resolve_sum_name(sc.sum_type_name) != resolved_sum_source_id_4 {
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
	resolved_sum := resolved_sum_id
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
		t.record_monomorph_error('`${pattern}` is not a variant of sum type `${flat.source_type_name(resolved_sum_id)}`')
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
		if _ := t.resolve_interface_pattern_interface(pattern_id) {
			// A test against another interface is valid even when no type implements both.
			return true
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
	for candidate in t.interface_pattern_candidates(pattern_id) {
		if types.is_builtin_type_name_id(candidate) || t.interface_pattern_candidate_known(candidate) {
			return true
		}
		clean := flat.source_type_syntax(candidate).trimmed()
		shape := flat.source_type_shape(clean)
		if shape.has(.array_type) || shape.has(.map_type) || shape.has(.leading_bracket)
			|| shape.has(.function_spaced) || shape.has(.function_compact) {
			if t.tc.parse_type(clean).source_type_id() != flat.source_type_unknown { return true }
		}
	}
	return false
}

fn (t &Transformer) interface_impl_type_id(iface_name_id u32, concrete_name_id u32) ?int {
	if iface_name_id == 0 || concrete_name_id == 0 || isnil(t.tc) {
		return none
	}
	iface_source_id := t.resolve_interface_type_name(iface_name_id)
	if iface_source_id == 0 {
		return none
	}
	concrete_id := t.interface_concrete_impl_name(concrete_name_id) or { return none }
	requested_qualified := flat.source_type_shape(concrete_name_id).has(.dotted)
		|| concrete_id != concrete_name_id
	iface_candidates := t.interface_impl_type_id_iface_candidates(iface_source_id)
	for impl_iface_id in iface_candidates {
		if impl_index := t.interface_impl_indexes[impl_iface_id] {
			if type_id := impl_index.ids[concrete_id] {
				return type_id
			}
		}
	}
	if !requested_qualified {
		for impl_iface_id in iface_candidates {
			impl_index := t.interface_impl_indexes[impl_iface_id] or { continue }
			for impl_name_id in impl_index.names.ids {
				if flat.source_type_syntax(impl_name_id).unqualified() == flat.source_type_syntax(concrete_id).unqualified() {
					return impl_index.ids[impl_name_id]
				}
			}
		}
	}
	// Generic specialization can introduce a concrete implementation after the
	// immutable transform indexes were prepared. Fall back to the live checker
	// only for those late names.
	for impl_iface_id in iface_candidates {
		is_ierror := t.is_builtin_ierror_interface_name(impl_iface_id)
		impl_names := if is_ierror {
			t.tc.ierror_impl_names()
		} else {
			t.tc.interface_impl_names(impl_iface_id)
		}
		type_ids := if is_ierror {
			types.stable_interface_type_ids(impl_names)
		} else {
			t.tc.interface_type_ids(impl_iface_id)
		}
		for impl_name_id in impl_names {
			if impl_name_id == concrete_id {
				return type_ids[impl_name_id]
			}
		}
	}
	if !requested_qualified {
		for impl_iface_id in iface_candidates {
			is_ierror := t.is_builtin_ierror_interface_name(impl_iface_id)
			impl_names := if is_ierror {
				t.tc.ierror_impl_names()
			} else {
				t.tc.interface_impl_names(impl_iface_id)
			}
			type_ids := if is_ierror {
				types.stable_interface_type_ids(impl_names)
			} else {
				t.tc.interface_type_ids(impl_iface_id)
			}
			for impl_name_id in impl_names {
				if flat.source_type_syntax(impl_name_id).unqualified() == flat.source_type_syntax(concrete_id).unqualified() {
					return type_ids[impl_name_id]
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
	if interface_pattern_is_collapsed_container_type(concrete_source_id) {
		type_idx := t.type_index(concrete_source_id, t.cur_module)
		if type_idx <= 0 {
			return none
		}
		// A late container (introduced by specialization after the immutable index
		// snapshot) has no dispatch ID. Its value type index leaves the
		// interface_dispatch_id_marker byte zero, which no dispatch ID does, so the
		// index itself is an unambiguous tag that type_idx() can return.
		if type_idx & types.interface_dispatch_id_marker != 0 {
			return none
		}
		return type_idx
	}
	index := t.interface_impl_index_for_transform(iface_source_id_2)
	mut impls := index.names.ids.clone()
	if concrete_source_id !in impls {
		impls << concrete_source_id
	}
	ids := types.stable_interface_type_ids_preserving_prefix(index.names.ids, impls)
	return ids[concrete_source_id] or { none }
}

fn (t &Transformer) interface_impl_type_id_iface_candidates(iface_id u32) []u32 {
	mut candidates := []u32{}
	syntax := flat.source_type_syntax(iface_id)
	short := syntax.unqualified()
	if short != iface_id && (flat.source_type_query(iface_id).has(.main_prefix) || syntax.builtin_prefix()) {
		candidates << short
	}
	candidates << iface_id
	if !flat.source_type_shape(iface_id).has(.dotted) {
		qualified := flat.source_type_id(t.tc.qualify_name(flat.source_type_name(iface_id)))
		if qualified != iface_id { candidates << qualified }
	}
	mut result := []u32{}
	for candidate in candidates {
		if candidate == 0 || candidate in result { continue }
		if t.is_builtin_ierror_interface_name(candidate) || candidate in t.tc.interface_names {
			result << candidate
		}
	}
	return result
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
	for alias_source_id, target in t.tc.type_aliases.ids {
		target_normalized := t.normalize_type_in_module(target, t.cur_module)
		target_short := t.variant_short_name(target)
		target_normalized_short := t.variant_short_name(target_normalized)
		if target == name_id || target == normalized || target_normalized == name_id || target_normalized == normalized
			|| (allow_short_match && (target_short == name_short || target_short == normalized_short || target_normalized_short == name_short || target_normalized_short == normalized_short)) {
			t.push_interface_alias_equivalent_name(mut names, mut seen, alias_source_id)
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
	if alias := t.function_alias_impl_name(name_id) { return alias }
	shape := flat.source_type_shape(name_id)
	syntax := flat.source_type_syntax(name_id)
	if shape.has(.array_type) {
		typ := t.tc.parse_type(name_id)
		if typ is types.Array { return typ.source_type_id() }
	}
	if shape.has(.map_type) {
		typ := t.tc.parse_type(name_id)
		if typ is types.Map { return typ.source_type_id() }
	}
	if shape.has(.leading_bracket) {
		typ := t.tc.parse_type(name_id)
		if typ is types.ArrayFixed { return typ.source_type_id() }
	}
	if interface_concrete_primitive_type(name_id) { return name_id }
	if syntax.builtin_prefix() && interface_concrete_primitive_type(syntax.builtin_base()) {
		return syntax.builtin_base()
	}
	if shape.has(.dotted) && interface_concrete_primitive_type(syntax.unqualified()) {
		return syntax.unqualified()
	}
	if syntax.generic_app_valid() && !t.generic_arg_is_unresolved(name_id) {
		base := syntax.generic_app_base()
		if base in t.tc.structs || t.tc.type_aliases.contains(base) { return name_id }
		if !flat.source_type_shape(base).has(.dotted) && t.cur_module.len > 0 && t.cur_module !in [
			'main',
			'builtin',
		] {
			qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), base)
			if qualified in t.tc.structs || t.tc.type_aliases.contains(qualified) {
				return flat.source_type_composition_two(.concat, qualified, syntax.generic_app_legacy_suffix())
			}
		}
	}
	name := flat.source_type_name(name_id)
	if !shape.has(.dotted) {
		if t.cur_file.len > 0 {
			for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or { []string{} } {
				if flat.source_type_id(candidate) in t.tc.structs || t.tc.type_aliases.contains(flat.source_type_id(candidate)) || flat.source_type_id(candidate) in t.tc.enum_names {
					return flat.source_type_id(candidate)
				}
			}
		}
		if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
			qualified := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), name_id)
			if qualified in t.tc.structs || t.tc.type_aliases.contains(qualified) || qualified in t.tc.enum_names {
				return qualified
			}
		}
	}
	if name_id in t.tc.structs || t.tc.type_aliases.contains(name_id) || name_id in t.tc.enum_names {
		return name_id
	}
	for struct_name_source_id, _ in t.tc.structs {
		candidate := struct_name_source_id
		if flat.source_type_syntax(candidate).unqualified() == name_id { return candidate }
	}
	for alias_name_source_id, _ in t.tc.type_aliases.ids {
		candidate := alias_name_source_id
		if flat.source_type_syntax(candidate).unqualified() == name_id { return candidate }
	}
	return none
}

fn interface_concrete_primitive_type(id u32) bool {
	return id in [flat.source_type_bool, flat.source_type_int, flat.source_type_i8,
		flat.source_type_i16, flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
		flat.source_type_uint, flat.source_type_u8, flat.source_type_u16, flat.source_type_u32,
		flat.source_type_u64, flat.source_type_f32, flat.source_type_f64, flat.source_type_string,
		flat.source_type_char, flat.source_type_rune, flat.source_type_voidptr]
}

fn (t &Transformer) function_alias_impl_name(name_id u32) ?u32 {
	clean := flat.source_type_syntax(name_id).trimmed()
	shape := flat.source_type_shape(clean)
	if !shape.has(.function_compact) && !shape.has(.function_spaced) { return none }
	mut aliases := t.tc.type_aliases.keys()
	aliases.sort_with_compare(compare_helper_source_types)
	for alias in aliases {
		target := t.tc.type_aliases.type_id(alias) or { continue }
		if flat.source_type_syntax(target).trimmed() == clean { return alias }
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
			return [direct_source_id]
		}
	}
	mut visited := map[u32]bool{}
	return t.sum_variant_path_inner(sum_name_id, variant_id, mut visited).clone()
}

// sum_variant_path_inner supports sum variant path inner handling for Transformer.
fn (t &Transformer) sum_variant_path_inner(sum_name_id u32, variant_id u32, mut visited map[u32]bool) []u32 {
	clean_sum_source_id := t.trim_pointer_type(sum_name_id)

	for candidate_source_id_3 in t.sum_subject_type_candidates(clean_sum_source_id) {
		if direct_source_id := t.sum_variant_name(candidate_source_id_3, variant_id) {
			return [direct_source_id]
		}
	}
	resolved_sum_source_id_5 := t.resolve_sum_name(clean_sum_source_id)
	resolved_sum_comparison_type_id_4 := resolved_sum_source_id_5
	if resolved_sum_source_id_5 == 0 || resolved_sum_source_id_5 in visited {
		return []u32{}
	}
	visited[resolved_sum_source_id_5] = true
	variants := t.concrete_sum_variants_for_candidate(clean_sum_source_id)
	if variants.len == 0 {
		return []u32{}
	}
	for direct in variants {
		direct_sum_source_id := t.resolve_sum_name(t.trim_pointer_type(direct))
		direct_sum_comparison_type_id := direct_sum_source_id
		direct_sum := direct_sum_comparison_type_id
		if direct_sum_comparison_type_id == resolved_sum_comparison_type_id_4 || !t.sum_types.contains(direct_sum) {
			continue
		}
		nested := t.sum_variant_path_inner(direct_sum_source_id, variant_id, mut visited)
		if nested.len == 0 {
			continue
		}
		mut path := []u32{cap: nested.len + 1}
		path << direct
		path << nested
		return path
	}
	return []u32{}
}

// make_sum_type_pattern_check builds make sum type pattern check data for transform.
fn (mut t Transformer) make_sum_type_pattern_check(expr flat.NodeId, expr_type_id u32, sum_name_id u32, variant_id u32) ?flat.NodeId {
	clean_sum := t.trim_pointer_type(sum_name_id)
	path := t.sum_variant_path(clean_sum, variant_id)
	if path.len == 0 { return none }
	stable_expr := if path.len > 1 {
		t.stable_transformed_expr_for_reuse(expr, expr_type_id, 'sum_is')
	} else {
		expr
	}
	mut current := stable_expr
	mut current_type := expr_type_id
	mut current_sum := clean_sum
	mut chain := flat.empty_node
	for i, path_variant in path {
		cmp := t.make_sum_is_check(current, current_type, current_sum, path_variant)
		if int(chain) < 0 { chain = cmp } else { chain = t.make_infix(.logical_and, chain, cmp) }
		if i == path.len - 1 { break }
		qv := t.resolve_variant(current_sum, path_variant)
		field_type := qv
		current = t.make_selector_op(current, sum_field_name(flat.source_type_name(qv)), field_type, if flat.source_type_shape(current_type).has(.pointer) {
			.arrow
		} else {
			.dot
		})
		current_type = field_type
		current_sum = t.trim_pointer_type(qv)
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
		mut operand_type := t.raw_expr_type_without_smartcast(first_child)
		if operand_type == 0 {
			operand_type = t.node_type(first_child)
		}
		if operand_type == 0 {
			operand_type = t.resolve_expr_type(first_child)
		}
		value := t.transform_expr_for_type(first_child, operand_type)
		start := t.a.children.len
		t.a.children << value
		return t.transform_as_expr(id, flat.Node{
			kind:           .as_expr
			source_value:   node.source_value_id()
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
		mut source_type := t.raw_expr_type_without_smartcast(source_id)
		if source_type == 0 {
			source_type = t.node_type(source_id)
		}
		if t.is_optional_type_name(source_type) {
			expr_id = source_id
			unchecked_optional_unwrap = true
		}
	}
	// `as` converts from the expression's storage type. Inside an `is` branch,
	// `node_type` reports the smartcast target instead; using that here makes an
	// interface-to-interface cast look like a no-op and leaves the source box on
	// the RHS (for example `layout as Widget`).
	mut expr_type := t.raw_expr_type_without_smartcast(expr_id)
	if expr_type == 0 {
		expr_type = t.node_type(expr_id)
	}
	if t.is_optional_type_name(expr_type) {
		optional_type_source_id := t.qualify_optional_type(expr_type)

		target_type_source_id := t.qualify_optional_type(node.source_value_id())

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
		if t.normalize_type_alias(payload_type_source_id) == t.normalize_type_alias(node.source_value_id()) {
			return value
		}
		resolved_payload_source_id := t.resolve_sum_name(t.trim_pointer_type(payload_type_source_id))
		if t.sum_types.contains(resolved_payload_source_id) {
			return t.project_sum_value(value, payload_type_source_id, node.source_value_id())
		}
		start := t.a.children.len
		t.a.children << value
		return t.transform_as_expr(id, flat.Node{
			kind:           .as_expr
			source_value:   node.source_value_id()
			typ:            node.typ
			children_start: start
			children_count: 1
			pos:            node.pos
		})
	}
	clean_type0_source_id := t.trim_pointer_type(expr_type)
	clean_type0_source_id_3 := clean_type0_source_id
	clean_type0 := clean_type0_source_id_3
	if t.is_interface_type_name(clean_type0_source_id) {
		target_is_pointer := flat.source_type_shape(node.source_value_id()).has(.pointer)
		target_pattern_source_id := t.trim_pointer_type(node.source_value_id())

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
			if converted := t.convert_interface_expr_to_interface(child, expr_type, target_iface_source_id,
				.checked)
			{
				if target_is_pointer {
					return t.heap_copy_interface_expr(converted, target_iface_source_id, node.source_value_id())
				}
				return converted
			}
		}
		if !interface_pattern_is_collapsed_container_type(node.source_value_id()) {
			if qv_source_id := t.resolve_interface_pattern(target_pattern_source_id, clean_type0_source_id) {
				return t.interface_as_concrete(expr_id, expr_type, clean_type0_source_id, qv_source_id,
					target_is_pointer)
			}
		}
		if interface_pattern_is_collapsed_container_type(node.source_value_id())
			&& t.tc.interface_abstract_method_names(clean_type0_source_id_3).len == 0
			&& t.tc.interface_field_list(clean_type0_source_id_3).len == 0 {
			target_type_id := t.tc.parse_type(node.source_value_id()).source_type_id()
			if type_id := t.interface_container_cast_type_id(clean_type0_source_id, target_type_id) {
				target_type := flat.source_type_name(target_type_id)
				child := t.transform_expr(expr_id)
				source := t.stable_transformed_expr_for_reuse(child, expr_type, 'iface_as')
				field_op := if flat.source_type_shape(expr_type).has(.pointer) {
					flat.Op.arrow
				} else {
					flat.Op.dot
				}
				actual_type_id := t.make_selector_op(source, '_typ', flat.source_type_int, field_op)
				mismatch := t.make_infix(.ne, actual_type_id, t.make_int_literal(type_id))
				message := 'as cast: cannot cast interface value to `${target_type}`'
				t.pending_stmts << t.make_if(mismatch, t.make_block([
					t.make_panic_stmt(message),
				]), t.make_empty())
				object := t.make_selector_op(source, '_object', flat.source_type_voidptr, field_op)
				pointer_type := flat.source_type_composition_one(.pointer, target_type_id)
				cast := t.make_cast(pointer_type, object, pointer_type)
				current := t.make_prefix(.mul, cast)
				t.set_node_typ(int(current), target_type_id)
				return current
			}
		}
	}
	clean_type := (if t.sum_types.contains(clean_type0) {
		clean_type0_source_id_3
	} else if _ := t.resolve_sum_variant_pattern_for_subject(clean_type0_source_id, node.source_value_id()) {
		clean_type0_source_id_3
	} else {
		t.find_sum_type_for_variant(node.source_value_id())
	})
	resolved_clean_type_source_id := t.resolve_sum_name(clean_type)
	resolved_clean_type := resolved_clean_type_source_id
	if clean_type == 0 || !t.sum_types.contains(resolved_clean_type) {
		child := t.transform_expr(expr_id)
		start := t.a.children.len
		t.a.children << child
		return t.a.add_node(flat.Node{
			kind:           .as_expr
			source_value:   node.source_value_id()
			typ:            node.typ
			children_start: start
			children_count: 1
			pos:            node.pos
		})
	}
	qv_source_id_3 := t.resolve_variant(clean_type, node.source_value_id())
	if qv_source_id_3 != 0 && t.normalize_type_alias(clean_type0_source_id) == t.normalize_type_alias(qv_source_id_3) {
		return t.transform_expr(expr_id)
	}
	sc_key := t.expr_key(expr_id)
	if sc_key.len > 0 {
		contexts := t.smartcasts_for(sc_key)
		mut matched_contexts := []SmartcastContext{}
		for i, sc in contexts {
			sc_variant := flat.source_type_syntax(sc.variant_name).unqualified()
			target_variant := flat.source_type_syntax(node.source_value_id()).unqualified()
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
	new_expr := t.transform_expr(expr_id)
	new_type := t.node_type(new_expr)
	source_type := if t.is_sum_type_name(t.trim_pointer_type(new_type)) {
		new_type
	} else {
		expr_type
	}
	return t.project_sum_value(new_expr, source_type, node.source_value_id())
}

fn (t &Transformer) sum_projection_source_type(id flat.NodeId) u32 {
	mut typ := t.raw_expr_type_without_smartcast(id)
	if typ == 0 { typ = t.node_type(id) }
	typ = t.normalize_type_alias(typ)
	for t.is_optional_type_name(typ) { typ = t.normalize_type_alias(t.optional_base_type(typ)) }
	return typ
}

fn (t &Transformer) sum_projection_has_temporary_storage(id flat.NodeId) bool {
	mut current := id
	mut projection := false
	for {
		node := t.a.node(current)
		if node.kind !in [.paren, .as_expr, .selector, .index] {
			return projection && !t.expr_can_take_address(current)
		}
		source := t.a.child(node, 0)
		indirect := match node.kind {
			.as_expr { !t.is_sum_type_name(t.sum_projection_source_type(source)) }
			.index {
				// Fixed-array elements live inside their sum payload. Slices of
				// that array still borrow it; dynamic arrays and maps have separate storage.
				mut backing := t.unwrap_parens(source)
				for t.is_range_index_expr(backing) {
					backing = t.unwrap_parens(t.a.child(t.a.node(backing), 0))
				}
				if !t.is_fixed_array_type(t.sum_projection_source_type(backing)) {
					return projection && !t.expr_can_take_address(current)
				}
				false
			}
			.selector {
				flat.source_type_shape(t.normalize_type_alias(t.node_type(source))).has(.pointer)
			}
			else { false }
		}
		if indirect { return false }
		projection = projection || node.kind == .as_expr
		current = source
	}
	return false
}

fn (mut t Transformer) project_sum_value(value flat.NodeId, source_type u32, target u32) flat.NodeId {
	mut source := value
	mut current_type := source_type
	if !flat.source_type_shape(source_type).has(.pointer) && !t.is_stable_expr_for_reuse(value)
		&& t.expr_can_take_address(value) {
		source = t.make_prefix(.amp, value)
		current_type = flat.source_type_composition_one(.pointer, source_type)
	}
	source = t.stable_transformed_expr_for_reuse(source, current_type, 'sum_as')
	sum_name := t.trim_pointer_type(source_type)
	if t.normalize_type_alias(sum_name) == t.normalize_type_alias(target) {
		if flat.source_type_shape(current_type).has(.pointer) {
			source = t.make_prefix(.mul, source)
			t.set_node_typ(int(source), target)
		}
		return source
	}
	mut path := t.sum_variant_path(sum_name, target)
	if path.len == 0 { path = [t.resolve_variant(sum_name, target)] }
	for variant in path {
		source = t.checked_sum_variant(source, current_type, variant)
		current_type = variant
	}
	return source
}

fn (mut t Transformer) checked_sum_variant(source flat.NodeId, source_type u32, target u32) flat.NodeId {
	sum_name := t.resolve_sum_name(t.trim_pointer_type(source_type))
	qv := t.resolve_variant(sum_name, target)
	field := sum_field_name(flat.source_type_name(qv))
	variants := t.sum_type_variants_for_index(sum_name)
	op := if flat.source_type_shape(source_type).has(.pointer) {
		flat.Op.arrow
	} else {
		flat.Op.dot
	}
	if variants.len > 0 {
		mut accepted_variants := t.sum_alias_equivalent_variants(sum_name, target)
		if accepted_variants.len == 0 { accepted_variants << qv }
		actual_name := t.new_temp('sum_as_type')
		mut mismatch_stmts := [t.make_decl_assign_typed(actual_name,
			t.make_string_literal(t.sum_as_display_type(variants[0])), flat.source_type_string)]
		for variant in variants {
			is_variant := t.make_infix(.eq, t.make_sum_tag_selector(source, op), t.make_int_literal(t.sum_type_index(sum_name, variant)))
			// These checks introduce no source scope or ownership cleanup entry.
			mismatch_stmts << t.make_if(is_variant, t.make_block_skip_scope_drops([
				t.make_assign(t.make_ident(actual_name), t.make_string_literal(t.sum_as_display_type(variant))),
			]), t.make_empty())
		}
		check := t.make_call_typed('__as_cast', [t.a.add(.nil_literal),
			t.make_sum_tag_selector(source, op), t.make_int_literal(t.sum_type_index(sum_name, qv)),
			t.make_ident(actual_name), t.make_string_literal(t.sum_as_display_type(qv))], flat.source_type_voidptr)
		mismatch_stmts << t.make_expr_stmt(check)
		mut mismatch := flat.empty_node
		for accepted_variant in accepted_variants {
			not_variant := t.make_infix(.ne, t.make_sum_tag_selector(source, op), t.make_int_literal(t.sum_type_index(sum_name, accepted_variant)))
			mismatch = if int(mismatch) < 0 {
				not_variant
			} else {
				t.make_infix(.logical_and, mismatch, not_variant)
			}
		}
		t.pending_stmts << t.make_if(mismatch, t.make_block_skip_scope_drops(mismatch_stmts), t.make_empty())
	}
	return t.make_selector_op(source, field, qv, op)
}

// interface_as_concrete lowers `iface as Concrete` and `iface as &Concrete`. Unless an
// active smartcast already proves the dynamic type, the stored `_typ` is compared with
// the target's ids first, and a mismatch panics like a failed sum type `as` cast.
fn (mut t Transformer) interface_as_concrete(expr_id flat.NodeId, expr_type u32, iface_id u32, concrete_id u32, target_is_pointer bool) flat.NodeId {
	mut proven := false
	if sc := t.find_smartcast_for_expr(expr_id) {
		if t.variant_names_match(t.trim_pointer_type(t.smartcast_target_type(sc)), concrete_id) {
			if !target_is_pointer {
				return t.transform_expr(expr_id)
			}
			proven = true
		}
	}
	// Read the stored box: a smartcast to another interface has other `_typ` ids.
	child := if t.expr_has_smartcast(expr_id) {
		t.make_plain_expr_for_smartcast(expr_id)
	} else {
		t.transform_expr(expr_id)
	}
	field_op := if flat.source_type_shape(expr_type).has(.pointer) {
		flat.Op.arrow
	} else {
		flat.Op.dot
	}
	type_ids := if proven { []int{} } else { t.interface_impl_type_ids(iface_id, concrete_id) }
	source := if type_ids.len > 0 {
		t.stable_transformed_expr_for_reuse(child, expr_type, 'iface_as')
	} else {
		child
	}
	if type_ids.len > 0 {
		actual := t.make_selector_op(source, '_typ', flat.source_type_int, field_op)
		t.pending_stmts << t.make_interface_as_concrete_mismatch_panic(actual, iface_id, concrete_id,
			type_ids)
	}
	concrete_ptr_id := flat.source_type_composition_one(.pointer, concrete_id)
	object := t.make_selector_op(source, '_object', flat.source_type_voidptr, field_op)
	cast := t.make_cast(concrete_ptr_id, object, concrete_ptr_id)
	if target_is_pointer {
		return cast
	}
	current := t.make_prefix(.mul, cast)
	t.set_node_typ(int(current), concrete_id)
	return current
}

fn (t &Transformer) sum_as_display_type(name_id u32) string {
	name := flat.source_type_name(name_id)
	if name_id == 0 || flat.source_type_shape(name_id).has(.dotted) {
		return name
	}
	is_named_type := name_id in t.structs || t.sum_types.contains(name_id) || name_id in t.enum_types
		|| (!isnil(t.tc)
			&& (name_id in t.tc.interface_names || t.tc.type_aliases.contains(name_id)))
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
	result := if t.cur_fn_ret_type == 0 || t.cur_fn_ret_type !in t.sum_types.ids {
		t.transform_expr(expr_id)
	} else {
		t.wrap_sum_value(expr_id, t.cur_fn_ret_type)
	}
	t.in_return_expr = was_return_expr
	return result
}

fn (mut t Transformer) fixed_array_sum_literal_expr(expr_id flat.NodeId, expr flat.Node, resolved_sum_id u32) ?FixedArraySumLiteral {
	resolved_sum := resolved_sum_id
	mut literal_id := expr_id
	mut literal := expr
	if expr.kind == .postfix && expr.op == .not && expr.children_count == 1 {
		literal_id = t.a.child(&expr, 0)
		literal = t.a.nodes[int(literal_id)]
	}
	if literal.kind != .array_literal {
		return none
	}
	variants := t.sum_types.type_ids(resolved_sum) or { return none }
	literal_len := int(literal.children_count)
	literal_elem_source_id := t.array_literal_elem_type(literal)

	mut fallback_variant := u32(0)
	mut fallback_type := u32(0)
	mut fallback_count := 0
	for variant in variants {
		fixed_type_source_id := t.normalize_type_alias(variant)
		fixed_type_source_id_2 := fixed_type_source_id
		if !t.is_fixed_array_type(fixed_type_source_id) {
			continue
		}
		length_id := fixed_array_length_id(fixed_type_source_id_2)
		length_syntax := flat.source_type_syntax(length_id)
		fixed_len := if length_syntax.decimal() {
			length_syntax.decimal_value()
		} else if !isnil(t.tc) {
			t.tc.const_int_value_in_module(flat.source_type_name(length_id), t.cur_module, []string{}) or { continue }
		} else {
			continue
		}
		if fixed_len != literal_len {
			continue
		}
		elem_type := fixed_array_elem_type(fixed_type_source_id_2)
		if t.fixed_array_sum_literal_elem_matches(literal_elem_source_id, elem_type) {
			expr2 := t.transform_fixed_array_literal_for_type(literal_id, literal, fixed_type_source_id) or {
				continue
			}
			return FixedArraySumLiteral{
				variant: variant
				expr:    expr2
			}
		}
		if t.fixed_array_sum_literal_elem_unknown(literal_elem_source_id) {
			fallback_variant = variant
			fallback_type = fixed_type_source_id_2
			fallback_count++
		}
	}
	if fallback_count == 1 {
		expr2 := t.transform_fixed_array_literal_for_type(literal_id, literal, fallback_type) or {
			return none
		}
		return FixedArraySumLiteral{
			variant: fallback_variant
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
	return literal_elem_id == u32(0) || literal_elem_id in [flat.source_type_unknown,
		flat.source_type_void, transform_source_type_array]
}

fn (t &Transformer) single_pointer_sum_nil_variant(expr_id flat.NodeId, sum_name_id u32) ?u32 {
	if !t.expr_is_nil_like(expr_id) {
		return none
	}
	_, variants_source_ids_2 := t.concrete_sum_name_and_variants(sum_name_id)
	// A checked scalar variant keeps its type even when its value is zero.
	// Only a null expression without a matching variant needs pointer inference.
	actual_type := t.normalize_type_alias(t.sum_nil_expr_type(expr_id))
	if actual_type != 0 && variants_source_ids_2.any(t.normalize_type_alias(it) == actual_type) {
		return none
	}
	mut pointer_variant := u32(0)
	for variant_source_id in variants_source_ids_2 {
		variant_source_id_2 := variant_source_id

		if !t.sum_variant_is_direct_pointer(variant_source_id_2) {
			continue
		}
		if pointer_variant != 0 {
			return none
		}
		pointer_variant = variant_source_id_2
	}
	if pointer_variant == 0 {
		return none
	}
	return pointer_variant
}

// sum_nil_expr_type retains scalar literal types beneath an expected sum type.
fn (t &Transformer) sum_nil_expr_type(id flat.NodeId) u32 {
	node := t.a.nodes[int(id)]
	if node.kind == .int_literal { return flat.source_type_int }
	// An explicit cast keeps its scalar/pointer identity even if expected-type
	// propagation annotated the node with the surrounding sum type.
	if node.kind == .cast_expr && node.source_value_id() != 0 {
		return t.normalize_type_alias(node.source_value_id())
	}
	if node.kind in [.paren, .expr_stmt] && node.children_count > 0 {
		return t.sum_nil_expr_type(t.a.child(&node, 0))
	}
	if node.kind == .block && node.children_count > 0 {
		return t.sum_nil_expr_type(t.a.child(&node, node.children_count - 1))
	}
	return t.raw_expr_type_without_smartcast(id)
}

fn (mut t Transformer) sum_storage_value(id flat.NodeId, sum_name u32, variants []u32) ?flat.NodeId {
	node := t.a.nodes[int(id)]
	raw_type := t.raw_expr_type_without_smartcast(id)
	mut source_type := t.normalize_type_alias(raw_type)
	base := t.trim_all_pointer_type(source_type)
	if base != sum_name && t.resolve_sum_name(base) != sum_name {
		return none
	}
	if raw_type in variants
		|| variants.any(t.normalize_type_alias(it) == source_type) {
		return none
	}
	is_storage := node.kind in [.ident, .selector, .index]
	if !is_storage && !flat.source_type_shape(source_type).has(.pointer) {
		return none
	}
	mut value := if is_storage {
		t.make_plain_expr_for_smartcast(id)
	} else {
		t.transform_expr_preserving_pointer_value(id)
	}
	t.set_node_typ(int(value), source_type)
	for flat.source_type_shape(source_type).has(.pointer) {
		source_type = t.trim_pointer_type(source_type)
		value = t.make_prefix(.mul, value)
		t.set_node_typ(int(value), source_type)
	}
	if node.kind == .index {
		value = t.lower_owned_array_index_move(id, value)
	}
	return t.mark_sum_moving_read(id, value)
}

// wrap_sum_value transforms wrap sum value data for transform.
fn (mut t Transformer) wrap_sum_value(expr_id flat.NodeId, target_sum_id u32) flat.NodeId {
	return t.wrap_sum_value_with_storage(expr_id, target_sum_id, false)
}

fn (mut t Transformer) wrap_sum_value_for_storage(expr_id flat.NodeId, target_sum_id u32) flat.NodeId {
	return t.wrap_sum_value_with_storage(expr_id, target_sum_id, true)
}

fn (mut t Transformer) wrap_sum_value_with_storage(expr_id flat.NodeId, target_sum_id u32, storage_boundary bool) flat.NodeId {
	resolved_sum_source_id_6, variants := t.concrete_sum_name_and_variants(target_sum_id)
	if variants.len == 0 {
		return t.transform_expr(expr_id)
	}
	detach_array_payload := storage_boundary && !t.borrowed_projection_clone_required(expr_id, target_sum_id)
	storage_sum_source_id := resolved_sum_source_id_6
	if storage := t.sum_storage_value(expr_id, resolved_sum_source_id_6, variants) {
		return storage
	}
	expr := t.a.nodes[int(expr_id)]
	constructor_type := match expr.kind {
		.cast_expr, .struct_init { t.normalize_type_alias(expr.source_value_id()) }
		else { u32(0) }
	}
	if t.resolve_sum_name(constructor_type) == resolved_sum_source_id_6 {
		return t.transform_expr(expr_id)
	}
	if nil_variant_source_id := t.single_pointer_sum_nil_variant(expr_id, resolved_sum_source_id_6) {
		value := t.transform_expr_for_type(expr_id, nil_variant_source_id)
		return t.make_sum_literal(storage_sum_source_id, nil_variant_source_id, value)
	}
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
	mut expr_type := t.node_type(expr_id)
	if expr.kind in [.ident, .selector] {
		if expr.kind == .ident && expr.value.len > 0 {
			local_type_source_id := t.raw_var_type(expr.value)
			local_type_source_id_2 := local_type_source_id

			if local_type_source_id != 0 && t.sum_target_accepts_variant_type(resolved_sum_source_id_6, local_type_source_id) {
				expr_type = local_type_source_id_2
			}
		}
		if expr.kind == .selector {
			selector_type := t.raw_selector_type_without_smartcast(expr_id)
			if selector_type != 0 && t.sum_target_accepts_variant_type(resolved_sum_source_id_6, selector_type) {
				expr_type = selector_type
			}
		}
		if const_type_source_id := t.raw_const_type_name_for_expr(expr_id) {
			const_type_source_id_2 := const_type_source_id

			if t.sum_target_accepts_variant_type(resolved_sum_source_id_6, const_type_source_id) {
				expr_type = const_type_source_id_2
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
	if has_expr_smartcast {
		variant = t.smartcast_target_type(expr_smartcast)
	}
	if expr.kind == .prefix && expr.op == .mul && expr.children_count > 0 {
		inner_type := t.node_type(t.a.child(&expr, 0))
		if flat.source_type_shape(inner_type).has(.pointer) {
			variant = t.trim_pointer_type(inner_type)
		}
	}
	if expr.kind == .struct_init || expr.kind == .cast_expr {
		variant = expr.source_value_id()
	} else if expr.kind == .as_expr && expr.source_value_id() != 0 {
		variant = expr.source_value_id()
	} else if expr.kind == .assoc && expr.source_value_id() != 0 {
		variant = expr.source_value_id()
	} else if expr.kind == .assoc && expr.children_count > 0 {
		variant = t.node_type(t.a.child(&expr, 0))
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
		&& t.resolve_sum_name(expr_type) == resolved_sum_source_id_6 {
		if storage := t.pointer_storage_expr_for_value_target(expr_id, resolved_sum_source_id_6) {
			return storage
		}
		return t.transform_expr(expr_id)
	}
	if variant == 0 {
		return t.transform_expr(expr_id)
	}
	mut clean_variant := variant
	mut variant_source := variant
	ptr_base := flat.source_type_syntax(variant_source).ptr_name_base()
	if ptr_base != 0 && flat.source_type_shape(ptr_base).has(.dotted) {
		variant_source = ptr_base
		clean_variant = ptr_base
	}
	encoded_base := flat.source_type_syntax(variant_source).ptr_name_base()
	if encoded_base != 0 && flat.source_type_syntax(encoded_base).contains_doubleunderscore() {
		clean_variant = flat.source_type_syntax(encoded_base).doubleunderscore_dotted()
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
	if wrapper_value := t.single_value_wrapper_sum_value(expr_id, clean_variant, resolved_sum_source_id_6) {
		return wrapper_value
	}
	short_variant_comparison_type_id := t.variant_short_name(clean_variant)

	container_variant := flat.source_type_shape(clean_variant).has(.array_type) || flat.source_type_shape(clean_variant).has(.map_type)
		|| t.is_fixed_array_type(clean_variant)
	mut matches := false
	mut matched_variant := clean_variant
	for v_source_id in variants {
		short_v_comparison_type_id := t.variant_short_name(v_source_id)

		v_is_container := flat.source_type_shape(v_source_id).has(.array_type) || flat.source_type_shape(v_source_id).has(.map_type) || t.is_fixed_array_type(v_source_id)
		// Container element/value types are storage-significant. Do not make
		// `[]json2.Any` match `[]toml.Any` (or analogous maps/fixed arrays) just
		// because their nested types share the same short name.
		if t.variant_names_match(v_source_id, clean_variant)
			|| (!v_is_container && !container_variant && short_v_comparison_type_id == short_variant_comparison_type_id) {
			matches = true
			matched_variant = v_source_id
			break
		}
	}
	if !matches {
		for v_source_id_2 in variants {
			if t.sum_variant_type_accepts_value_type(v_source_id_2, clean_variant) {
				matches = true
				matched_variant = v_source_id_2
				break
			}
		}
	}
	if !matches && t.is_integer_type_name(clean_variant) {
		mut enum_variant := u32(0)
		for v_source_id_3 in variants {
			if (t.enum_type_name_for_expected(v_source_id_3, t.cur_module)) == 0 {
				continue
			}
			if enum_variant != 0 {
				enum_variant = 0
				break
			}
			enum_variant = v_source_id_3
		}
		if enum_variant != 0 {
			matches = true
			matched_variant = enum_variant
		}
	}
	if !matches {
		mut path := t.sum_variant_path(resolved_sum_source_id_6, clean_variant)
		if path.len == 0 && t.array_storage_source_is_mut_param(expr_id) {
			// A mutable array parameter borrows its caller's header. Match the
			// stored value before wrapping it in nested sum variants.
			path = t.sum_variant_path(resolved_sum_source_id_6,
				t.trim_pointer_type(clean_variant))
		}
		if path.len == 1 {
			matches = true
			matched_variant = path[0]
		} else if path.len > 1 {
			nested_sum_source_id := t.resolve_sum_name(t.trim_pointer_type(path[0]))
			nested_sum := nested_sum_source_id
			if nested_sum_source_id != 0 && t.sum_types.contains(nested_sum) {
				nested_value := t.wrap_sum_value_with_storage(expr_id, nested_sum_source_id, detach_array_payload)
				return t.make_sum_literal(resolved_sum_source_id_6, path[0], nested_value)
			}
		}
	}
	if !matches {
		return t.transform_expr(expr_id)
	}
	matched_clean_source_id := t.trim_pointer_type(t.normalize_type_alias(matched_variant))
	matched_clean_comparison_type_id := matched_clean_source_id

	value_clean_source_id := t.trim_pointer_type(t.normalize_type_alias(clean_variant))
	value_clean_comparison_type_id := value_clean_source_id

	needs_variant_conversion := matched_clean_comparison_type_id != value_clean_comparison_type_id
		&& t.sum_variant_type_accepts_value_type(matched_clean_source_id, value_clean_source_id)
	mut inner := if needs_variant_conversion {
		// An array literal keeps its own element type (`[u8(1), 2]` holds two `u8`s)
		// and is converted element by element, instead of being retyped as `[]Sum`.
		if expr.kind == .array_literal {
			t.transform_array_value_for_dynamic_target(expr_id, matched_variant) or {
				t.transform_expr_for_type(expr_id, matched_variant)
			}
		} else {
			t.transform_expr_for_type(expr_id, matched_variant)
		}
	} else {
		t.transform_expr(expr_id)
	}
	if detach_array_payload {
		inner = t.sum_owned_value_payload(inner, matched_variant)
		// A pointer variant retains the caller's header; only value variants acquire storage.
		inner = t.clone_owned_array_storage_value(inner, matched_variant,
			t.array_storage_source_is_mut_param(expr_id)
				&& !t.sum_variant_is_direct_pointer(matched_variant))
	}
	return t.make_sum_literal(storage_sum_source_id, matched_variant, inner)
}

fn (mut t Transformer) single_value_wrapper_sum_value(expr_id flat.NodeId, wrapper_type_id u32, target_sum_id u32) ?flat.NodeId {
	if wrapper_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	lookup := t.lookup_struct_info_for_field(wrapper_type_id, 'value') or { return none }
	if lookup.info.fields.len != 1 {
		return none
	}
	value_type_source_id := t.lookup_struct_field_type(wrapper_type_id, 'value') or { return none }

	value_sum, _ := t.concrete_sum_name_and_variants(t.normalize_type_alias(value_type_source_id))
	if value_sum != target_sum_id {
		return none
	}
	wrapper := t.transform_expr(expr_id)
	value := t.make_selector(wrapper, 'value', value_type_source_id)
	t.set_node_typ(int(value), value_type_source_id)
	return value
}

fn (mut t Transformer) single_value_wrapper_expr_value(wrapper flat.NodeId, wrapper_type_id u32, target_sum_id u32) ?flat.NodeId {
	if wrapper_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	lookup := t.lookup_struct_info_for_field(wrapper_type_id, 'value') or { return none }
	if lookup.info.fields.len != 1 {
		return none
	}
	value_type_source_id := t.lookup_struct_field_type(wrapper_type_id, 'value') or { return none }

	value_sum, _ := t.concrete_sum_name_and_variants(t.normalize_type_alias(value_type_source_id))
	if value_sum != target_sum_id {
		return none
	}
	value := t.make_selector(wrapper, 'value', value_type_source_id)
	t.set_node_typ(int(value), value_type_source_id)
	return value
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
	value_type_source_id := t.comptime_normalize_type_alias_chain(t.node_type(value))
	mut payload_type := variant_type_source_id
	for t.is_optional_type_name(payload_type) {
		payload_type = t.comptime_normalize_type_alias_chain(t.optional_base_type(payload_type))
	}
	if flat.source_type_shape(value_type_source_id).has(.pointer)
		&& t.trim_pointer_type(value_type_source_id) == variant_type_source_id
		&& (flat.source_type_shape(payload_type).has(.array_type) || t.is_sum_type_name(payload_type)) {
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
	qvariant := t.resolve_variant(sum_name_id, variant_id)
	typ_field := t.make_sum_literal_field('typ', t.make_int_literal(t.sum_type_index(sum_name_id, qvariant)), flat.source_type_int)
	value_field := t.make_sum_literal_field(sum_field_name(flat.source_type_name(qvariant)), value, qvariant)
	start := t.a.children.len
	t.a.children << typ_field
	t.a.children << value_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		source_value:   sum_name_id
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
	return transform_trimmed_type_id(id)
}

fn transform_trimmed_type_id(id u32) u32 {
	if !flat.source_type_shape(id).has(.padded) { return id }
	return flat.source_type_syntax(id).trimmed()
}

fn (t &Transformer) unshared_type_id(id u32) u32 {
	if !flat.source_type_shape(id).has(.shared_type) { return id }
	if !isnil(t.alias_cache) {
		if base := t.alias_cache.shared_bases[id] { return base }
	}
	base := flat.source_type_syntax(flat.source_type_syntax(id).raw_base()).trimmed()
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
	elem := flat.source_type_syntax(flat.source_type_syntax(id).raw_base()).trimmed()
	array := flat.source_type_composition_one(.array, elem)
	if !isnil(t.alias_cache) {
		mut cache := t.alias_cache
		cache.variadic_arrays[id] = array
	}
	return array
}
