module transform

import v3.flat
import v3.types

struct FixedArraySumLiteral {
	variant string
	expr    flat.NodeId
}

// trim_pointer_type transforms trim pointer type data for transform.
fn (t &Transformer) trim_pointer_type(typ string) string {
	if typ.starts_with('&') {
		return typ[1..]
	}
	return typ
}

fn (t &Transformer) trim_all_pointer_type(typ string) string {
	mut clean := typ
	for clean.starts_with('&') {
		clean = clean[1..]
	}
	return clean
}

fn (mut t Transformer) pointer_sum_access_expr(expr_id flat.NodeId, expr_type string) (flat.NodeId, string, flat.Op) {
	mut access := t.transform_selector_base_expr(expr_id)
	mut access_type := expr_type
	for access_type.starts_with('&&') {
		access = t.make_prefix(.mul, access)
		access_type = access_type[1..]
		t.set_node_typ(int(access), access_type)
	}
	mut value_type := t.node_type(access)
	if value_type.len == 0 {
		value_type = access_type
	}
	op := if value_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	return access, value_type, op
}

// resolve_variant resolves resolve variant information for transform.
fn (t &Transformer) resolve_variant(sum_name string, variant string) string {
	for candidate in t.sum_subject_type_candidates(sum_name) {
		if resolved := t.generic_sum_arg_variant_for_pattern(candidate, variant) {
			return resolved
		}
		if resolved_variant := t.sum_variant_name(candidate, variant) {
			return resolved_variant
		}
	}
	if !isnil(t.tc) {
		if resolved := t.tc.sum_variant_type_for_pattern(sum_name, variant) {
			return resolved
		}
	}
	if variant.contains('.') {
		return variant
	}
	resolved_sum := t.resolve_sum_name(sum_name)
	if resolved_variant := t.sum_variant_name(resolved_sum, variant) {
		return resolved_variant
	}
	if resolved_sum.contains('.') {
		return '${resolved_sum.all_before_last('.')}.${variant}'
	}
	if sum_name.contains('.') {
		return '${sum_name.all_before_last('.')}.${variant}'
	}
	return variant
}

fn (t &Transformer) generic_sum_arg_variant_for_pattern(sum_name string, variant string) ?string {
	base, args, ok := generic_app_parts(sum_name)
	if !ok {
		return none
	}
	params := t.sum_generic_params_for_base(base)
	if params.len == args.len {
		concrete_pattern := substitute_generic_type_text_with_params(variant, args, params)
		for concrete_variant in t.concrete_sum_variants_for_candidate(sum_name) {
			if t.variant_names_match(concrete_variant, concrete_pattern)
				|| t.variant_names_match(concrete_variant, variant) {
				return concrete_variant
			}
		}
		if concrete_pattern != variant {
			return concrete_pattern
		}
	}
	_, variant_args, variant_is_generic := generic_app_parts(variant)
	if variant_is_generic && variant_args.len == args.len
		&& t.generic_variant_args_are_open(variant_args) {
		concrete_pattern := substitute_generic_type_text_with_params(variant, args, variant_args)
		for concrete_variant in t.concrete_sum_variants_for_candidate(sum_name) {
			if t.variant_names_match(concrete_variant, concrete_pattern)
				|| t.concrete_variant_matches_generic_pattern(concrete_variant, concrete_pattern) {
				return concrete_variant
			}
		}
		if concrete_pattern != variant {
			return concrete_pattern
		}
	}
	for arg in args {
		if t.variant_names_match(arg, variant) {
			return arg
		}
	}
	return none
}

// resolve_sum_name resolves resolve sum name information for transform.
fn (t &Transformer) resolve_sum_name(sum_name string) string {
	if sum_name.len == 0 {
		return sum_name
	}
	if isnil(t.sum_cache) {
		return t.resolve_sum_name_uncached(sum_name)
	}
	mut c := t.sum_cache
	if c.module != t.cur_module {
		c.module = t.cur_module
		c.entries.clear()
	}
	if cached := c.entries[sum_name] {
		return cached
	}
	result := t.resolve_sum_name_uncached(sum_name)
	c.entries[sum_name] = result
	return result
}

// resolve_sum_name_uncached resolves resolve sum name uncached information for transform.
fn (t &Transformer) resolve_sum_name_uncached(sum_name string) string {
	if sum_name in t.sum_types {
		return sum_name
	}
	if resolved_c_name := t.resolve_sum_name_from_c_name(sum_name) {
		return resolved_c_name
	}
	// A container of a sum type is not itself a sum type: `[]ast.Value` must
	// not resolve to `ast.Value` (the short-name and generic-application
	// fallbacks below would), or an or/assign lowering boxes the whole array
	// into one sum value.
	if sum_name.starts_with('[]') || sum_name.starts_with('map[') || sum_name.starts_with('[') {
		return ''
	}
	if !isnil(t.tc) && sum_name in t.tc.sum_types {
		return sum_name
	}
	generic_base, generic_args, is_generic := generic_app_parts(sum_name)
	if is_generic {
		concrete_sum := t.resolve_concrete_generic_sum_name(sum_name, generic_base, generic_args)
		if concrete_sum.len > 0 {
			return concrete_sum
		}
		resolved_base := t.resolve_sum_name_uncached(generic_base)
		if resolved_base in t.sum_types {
			return resolved_base
		}
		if !isnil(t.tc) && resolved_base in t.tc.sum_types {
			return resolved_base
		}
	}
	if sum_name.contains('.') {
		// Import-aliased module path: `tast.Value` names `sub.tast.Value`.
		// The full-suffix match runs before the bare short-name fallback so an
		// unrelated short `Value` sum cannot shadow the aliased one; ambiguous
		// suffix matches resolve nothing.
		suffix := '.' + sum_name
		mut suffix_match := ''
		mut suffix_ambiguous := false
		for key, _ in t.sum_types {
			if key.ends_with(suffix) {
				if suffix_match.len > 0 && suffix_match != key {
					suffix_ambiguous = true
					break
				}
				suffix_match = key
			}
		}
		if !suffix_ambiguous && !isnil(t.tc) {
			for key, _ in t.tc.sum_types {
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
		short_sum := sum_name.all_after_last('.')
		if short_sum in t.sum_types {
			return short_sum
		}
	}
	if !sum_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qsum := '${t.cur_module}.${sum_name}'
		if qsum in t.sum_types {
			return qsum
		}
	}
	if !sum_name.contains('.') {
		mut found := ''
		for key, _ in t.sum_types {
			if key.contains('.') && key.all_after_last('.') == sum_name {
				if found.len > 0 && found != key {
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
		if sum_name in t.tc.sum_types {
			return sum_name
		}
		if sum_name.contains('.') {
			short_sum := sum_name.all_after_last('.')
			if short_sum in t.tc.sum_types {
				return short_sum
			}
		}
		if !sum_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			qsum := '${t.cur_module}.${sum_name}'
			if qsum in t.tc.sum_types {
				return qsum
			}
		}
		if !sum_name.contains('.') {
			mut found := ''
			for key, _ in t.tc.sum_types {
				if key.contains('.') && key.all_after_last('.') == sum_name {
					if found.len > 0 && found != key {
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

fn (t &Transformer) resolve_sum_name_from_c_name(sum_name string) ?string {
	if sum_name.contains('[') || !sum_name.contains('_') {
		return none
	}
	for key, _ in t.sum_types {
		if key.contains('[') && c_name(key) == sum_name {
			return key
		}
	}
	if !isnil(t.tc) {
		for key, _ in t.tc.sum_types {
			if key.contains('[') && c_name(key) == sum_name {
				return key
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
		if qspec in t.sum_types || (!isnil(t.tc) && qspec in t.tc.sum_types) {
			return qspec
		}
	}
	if !sum_name.contains('.') {
		short_spec := t.find_sum_type_with_short_name(sum_name)
		if short_spec.len > 0 {
			return short_spec
		}
	}
	resolved_base := t.resolve_sum_name_uncached(base)
	if resolved_base.len > 0 && resolved_base != base {
		resolved_spec := '${resolved_base}[${args_text}]'
		if resolved_spec in t.sum_types || (!isnil(t.tc) && resolved_spec in t.tc.sum_types) {
			return resolved_spec
		}
	}
	return ''
}

fn (t &Transformer) find_sum_type_with_short_name(short_name string) string {
	mut found := ''
	for key, _ in t.sum_types {
		if key.contains('.') && key.all_after_last('.') == short_name {
			if found.len > 0 && found != key {
				return ''
			}
			found = key
		}
	}
	if !isnil(t.tc) {
		for key, _ in t.tc.sum_types {
			if key.contains('.') && key.all_after_last('.') == short_name {
				if found.len > 0 && found != key {
					return ''
				}
				found = key
			}
		}
	}
	return found
}

fn (t &Transformer) sum_subject_type_candidates(subject_type string) []string {
	clean := t.trim_pointer_type(t.normalize_type_alias(subject_type)).trim_space()
	if clean.len == 0 {
		return []string{}
	}
	mut candidates := []string{}
	mut seen := map[string]bool{}
	base, args, is_generic := generic_app_parts(clean)
	if is_generic {
		args_text := args.join(', ')
		scoped_args := t.sum_subject_type_args_in_scope(args)
		scoped_args_text := scoped_args.join(', ')
		if scoped_args_text != args_text {
			push_sum_subject_type_candidate(mut candidates, mut seen,
				'${base}[${scoped_args_text}]')
		}
		push_sum_subject_type_candidate(mut candidates, mut seen, clean)
		if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			push_sum_subject_type_candidate(mut candidates, mut seen,
				'${t.cur_module}.${base}[${args_text}]')
			if scoped_args_text != args_text {
				push_sum_subject_type_candidate(mut candidates, mut seen,
					'${t.cur_module}.${base}[${scoped_args_text}]')
			}
		}
		resolved_base := t.resolve_sum_name(base)
		if resolved_base.len > 0 && resolved_base != base {
			push_sum_subject_type_candidate(mut candidates, mut seen,
				'${resolved_base}[${args_text}]')
			if scoped_args_text != args_text {
				push_sum_subject_type_candidate(mut candidates, mut seen,
					'${resolved_base}[${scoped_args_text}]')
			}
		}
	} else {
		push_sum_subject_type_candidate(mut candidates, mut seen, clean)
		if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			push_sum_subject_type_candidate(mut candidates, mut seen, '${t.cur_module}.${clean}')
		}
	}
	resolved := t.resolve_sum_name(clean)
	if resolved.len > 0 {
		push_sum_subject_type_candidate(mut candidates, mut seen, resolved)
	}
	return candidates
}

fn (t &Transformer) sum_subject_type_args_in_scope(args []string) []string {
	mut scoped := []string{cap: args.len}
	for arg in args {
		scoped << t.sum_subject_type_arg_in_scope(arg)
	}
	return scoped
}

fn (t &Transformer) sum_subject_type_arg_in_scope(arg string) string {
	clean := arg.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.sum_subject_type_arg_in_scope(clean[1..])
	}
	if clean.starts_with('mut ') {
		return '&' + t.sum_subject_type_arg_in_scope(clean[4..])
	}
	if clean.starts_with('?') {
		return '?' + t.sum_subject_type_arg_in_scope(clean[1..])
	}
	if clean.starts_with('!') {
		return '!' + t.sum_subject_type_arg_in_scope(clean[1..])
	}
	if clean.starts_with('...') {
		return '...' + t.sum_subject_type_arg_in_scope(clean[3..])
	}
	if clean.starts_with('[]') {
		return '[]' + t.sum_subject_type_arg_in_scope(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := t.sum_subject_type_arg_in_scope(clean[4..bracket_end])
			value := t.sum_subject_type_arg_in_scope(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + t.sum_subject_type_arg_in_scope(clean[bracket_end +
				1..])
		}
	}
	base, nested_args, ok := generic_app_parts(clean)
	if ok {
		mut scoped_args := []string{cap: nested_args.len}
		for nested in nested_args {
			scoped_args << t.sum_subject_type_arg_in_scope(nested)
		}
		qbase := t.sum_subject_type_arg_base_in_scope(base)
		return '${qbase}[${scoped_args.join(', ')}]'
	}
	return t.sum_subject_type_arg_base_in_scope(clean)
}

fn (t &Transformer) sum_subject_type_arg_base_in_scope(name string) string {
	if name.contains('.') || isnil(t.tc) {
		return t.normalize_sum_variant_type(name, t.cur_module, [])
	}
	for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
		[]string{}
	} {
		if candidate in t.tc.type_aliases || candidate in t.tc.structs
			|| candidate in t.tc.interface_names || candidate in t.tc.flag_enums
			|| candidate in t.tc.enum_names || candidate in t.tc.sum_types {
			return candidate
		}
	}
	return t.normalize_sum_variant_type(name, t.cur_module, [])
}

fn push_sum_subject_type_candidate(mut candidates []string, mut seen map[string]bool, candidate string) {
	clean := candidate.trim_space()
	if clean.len == 0 || clean in seen {
		return
	}
	seen[clean] = true
	candidates << clean
}

// is_sum_type_name reports whether is sum type name applies in transform.
fn (t &Transformer) is_sum_type_name(name string) bool {
	if name.len == 0 {
		return false
	}
	resolved := t.resolve_sum_name(name)
	return resolved in t.sum_types
}

fn (t &Transformer) sum_target_accepts_variant_type(target_type string, variant_type string) bool {
	if target_type.len == 0 || variant_type.len == 0 {
		return false
	}
	resolved_raw_target := t.resolve_sum_name(t.trim_pointer_type(target_type))
	if resolved_raw_target in t.sum_types {
		for variant in t.sum_types[resolved_raw_target] {
			if t.variant_names_match(variant, t.trim_pointer_type(variant_type)) {
				return true
			}
		}
	}
	clean_target := t.trim_pointer_type(t.normalize_type_alias(target_type))
	clean_variant := t.trim_pointer_type(t.normalize_type_alias(variant_type))
	if clean_target.len == 0 || clean_variant.len == 0 {
		return false
	}
	resolved_target := t.resolve_sum_name(clean_target)
	if resolved_target.len == 0 || resolved_target !in t.sum_types {
		return false
	}
	if t.resolve_sum_name(clean_variant) == resolved_target {
		return true
	}
	if _ := t.resolve_sum_variant_pattern_for_subject(clean_target, clean_variant) {
		return true
	}
	if clean_target != resolved_target {
		if _ := t.resolve_sum_variant_pattern_for_subject(resolved_target, clean_variant) {
			return true
		}
	}
	variant_sum := t.resolve_sum_name(t.find_sum_type_for_variant(clean_variant))
	return variant_sum == resolved_target
}

// is_interface_type_name reports whether is interface type name applies in transform.
fn (t &Transformer) is_interface_type_name(name string) bool {
	if name.len == 0 || isnil(t.tc) {
		return false
	}
	return t.resolve_interface_type_name(name).len > 0
}

// interface_variant_type supports interface variant type handling for Transformer.
fn (t &Transformer) interface_variant_type(variant string) string {
	if variant.contains('.') {
		return variant
	}
	if variant in t.structs {
		return variant
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qvariant := '${t.cur_module}.${variant}'
		if qvariant in t.structs {
			return qvariant
		}
	}
	for name, _ in t.structs {
		if name.contains('.') && name.all_after_last('.') == variant {
			return name
		}
	}
	return variant
}

// smartcast_target_type supports smartcast target type handling for Transformer.
fn (t &Transformer) smartcast_target_type(sc SmartcastContext) string {
	if sc.sum_type_name == option_unwrap_marker {
		return sc.variant_name
	}
	if sc.variant_name.starts_with('&') {
		return '&' + t.resolve_variant(sc.sum_type_name, sc.variant_name[1..])
	}
	if t.is_interface_type_name(sc.sum_type_name) {
		return t.interface_variant_type(sc.variant_name)
	}
	return t.resolve_variant(sc.sum_type_name, sc.variant_name)
}

// sum_type_index supports sum type index handling for Transformer.
fn (t &Transformer) sum_type_index(sum_name string, variant string) int {
	variants := t.sum_type_variants_for_index(sum_name)
	if variants.len == 0 {
		return 0
	}
	return t.sum_type_index_in_variants(variants, variant)
}

fn (t &Transformer) sum_type_variants_for_index(sum_name string) []string {
	for candidate in t.sum_subject_type_candidates(sum_name) {
		variants := t.concrete_sum_variants_for_candidate(candidate)
		if variants.len > 0 {
			return variants
		}
	}
	resolved_sum := t.resolve_sum_name(sum_name)
	if variants := t.sum_types[resolved_sum] {
		return variants
	}
	if !isnil(t.tc) {
		if variants := t.tc.sum_types[resolved_sum] {
			return variants
		}
	}
	return []string{}
}

fn (t &Transformer) concrete_sum_variants_for_candidate(sum_name string) []string {
	base, args, is_generic := generic_app_parts(sum_name)
	if variants := t.sum_types[sum_name] {
		if is_generic {
			params := t.sum_generic_params_for_base(base)
			if params.len == args.len && params.len > 0 {
				return substitute_sum_variant_list(variants, args, params)
			}
		}
		return variants
	}
	if !isnil(t.tc) {
		if variants := t.tc.sum_types[sum_name] {
			if is_generic {
				params := t.sum_generic_params_for_base(base)
				if params.len == args.len && params.len > 0 {
					return substitute_sum_variant_list(variants, args, params)
				}
			}
			return variants
		}
	}
	if !is_generic {
		return []string{}
	}
	for base_candidate in t.sum_subject_type_candidates(base) {
		params := t.sum_generic_params_for_base(base_candidate)
		if params.len == 0 || params.len != args.len {
			continue
		}
		if variants := t.sum_types[base_candidate] {
			return substitute_sum_variant_list(variants, args, params)
		}
		if !isnil(t.tc) {
			if variants := t.tc.sum_types[base_candidate] {
				return substitute_sum_variant_list(variants, args, params)
			}
		}
	}
	return []string{}
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

fn substitute_sum_variant_list(variants []string, args []string, params []string) []string {
	mut concrete := []string{cap: variants.len}
	for variant in variants {
		concrete << substitute_generic_type_text_with_params(variant, args, params)
	}
	return concrete
}

// sum_type_index_in_variants supports sum type index in variants handling for transform.
fn (t &Transformer) sum_type_index_in_variants(variants []string, variant string) int {
	for i, v in variants {
		if t.variant_names_match(v, variant) {
			return i + 1
		}
	}
	return 0
}

// transform_is_expr transforms transform is expr data for transform.
fn (mut t Transformer) transform_is_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	mut expr_type := t.node_type(expr_id)
	original_type := t.original_expr_type(expr_id)
	if original_type.starts_with('&&') && !expr_type.starts_with('&&') {
		expr_type = original_type
	}
	expr_node := t.a.nodes[int(expr_id)]
	if expr_node.kind == .ident {
		raw_type := t.raw_var_type(expr_node.value)
		if raw_type.starts_with('&&') && !expr_type.starts_with('&&') {
			expr_type = raw_type
		}
	}
	if expr_node.kind == .ident && t.addr_lvalue_pointer_locals[expr_node.value]
		&& expr_type.starts_with('&') && !expr_type.starts_with('&&') {
		expr_type = '&${expr_type}'
	}
	if folded := t.fold_smartcast_is_expr(expr_id, node.value) {
		return folded
	}
	clean_type0 := t.trim_all_pointer_type(expr_type)
	concrete_expr_type := t.normalize_type_alias(clean_type0)
	resolved_expr_sum := t.resolve_sum_name(concrete_expr_type)
	// Inside a `$for v in T.variants` body the pattern is the loop variable;
	// the unroll substitutes it later, so there is nothing to validate yet.
	if t.cloning_comptime_for_depth == 0
		&& !t.validate_specialized_is_expr(concrete_expr_type, resolved_expr_sum, node.value) {
		return t.make_bool_literal(false)
	}
	clean_type := if clean_type0 in t.sum_types {
		clean_type0
	} else if _ := t.resolve_sum_variant_pattern_for_subject(clean_type0, node.value) {
		clean_type0
	} else {
		t.find_sum_type_for_variant(node.value)
	}
	if t.is_builtin_ierror_interface_name(clean_type0) && node.value == 'none' {
		new_expr0, value_type, op := t.pointer_sum_access_expr(expr_id, expr_type)
		new_expr := t.stable_transformed_expr_for_reuse(new_expr0, value_type, 'ierror_is')
		typ := t.make_selector_op(new_expr, '_typ', 'int', op)
		return t.make_ierror_none_type_check(typ, clean_type0)
	}
	if t.is_interface_type_name(clean_type0) {
		new_expr0, value_type, op := t.pointer_sum_access_expr(expr_id, expr_type)
		new_expr := t.stable_transformed_expr_for_reuse(new_expr0, value_type, 'iface_is')
		pattern_name := if t.is_builtin_ierror_interface_name(clean_type0) && node.value == 'none' {
			'None__'
		} else {
			node.value
		}
		if target_iface := t.resolve_interface_pattern_interface(pattern_name) {
			if check := t.make_interface_target_is_check(new_expr, value_type, clean_type0,
				target_iface)
			{
				return check
			}
		}
		if type_id := t.interface_impl_type_id(clean_type0, pattern_name) {
			typ := t.make_selector_op(new_expr, '_typ', 'int', op)
			return t.make_infix(.eq, typ, t.make_int_literal(type_id))
		}
		if pattern := t.resolve_interface_pattern(pattern_name, clean_type0) {
			is_start := t.a.children.len
			t.a.children << new_expr
			return t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          pattern
				children_start: is_start
				children_count: 1
				typ:            'bool'
			})
		}
		object := t.make_selector_op(new_expr, '_object', 'voidptr', op)
		return t.make_infix(.ne, object, t.a.add(.nil_literal))
	}
	resolved_clean_type := t.resolve_sum_name(clean_type)
	if clean_type.len == 0 || resolved_clean_type !in t.sum_types {
		return t.make_bool_literal(true)
	}
	new_expr0, value_type, _ := t.pointer_sum_access_expr(expr_id, expr_type)
	new_expr := t.stable_transformed_expr_for_reuse(new_expr0, value_type, 'sum_is')
	if check := t.make_sum_type_pattern_check(new_expr, value_type, clean_type, node.value) {
		return check
	}
	return t.make_bool_literal(true)
}

fn (mut t Transformer) fold_smartcast_is_expr(expr_id flat.NodeId, pattern string) ?flat.NodeId {
	key := t.expr_key(expr_id)
	if key.len == 0 {
		return none
	}
	sc := t.find_smartcast(key) or { return none }
	if sc.sum_type_name == option_unwrap_marker || t.is_interface_type_name(sc.sum_type_name) {
		return none
	}
	resolved_sum := t.resolve_sum_name(sc.sum_type_name)
	if resolved_sum.len == 0 || resolved_sum !in t.sum_types {
		return none
	}
	path := t.sum_variant_path(resolved_sum, pattern)
	if path.len != 1 {
		return none
	}
	current_variant := t.resolve_variant(resolved_sum, sc.variant_name)
	return t.make_bool_literal(t.variant_names_match(current_variant, path[0]))
}

fn (mut t Transformer) validate_specialized_is_expr(subject_type string, resolved_sum string, pattern string) bool {
	if !t.validating_generic_spec || subject_type.len == 0
		|| t.type_text_has_generic_placeholder(subject_type, t.cur_module) {
		return true
	}
	if resolved_sum in t.sum_types {
		if pattern.len == 0 || t.type_text_has_generic_placeholder(pattern, t.cur_module) {
			return true
		}
		if _ := t.resolve_sum_variant_pattern_for_subject(subject_type, pattern) {
			return true
		}
		t.record_monomorph_error('`${pattern}` is not a variant of sum type `${resolved_sum}`')
		return false
	}
	if t.is_interface_type_name(subject_type) {
		if pattern.len == 0 || t.type_text_has_generic_placeholder(pattern, t.cur_module) {
			return true
		}
		if t.is_builtin_ierror_interface_name(subject_type) && pattern == 'none' {
			return true
		}
		if _ := t.resolve_interface_pattern(pattern, subject_type) {
			return true
		}
		if target_iface := t.resolve_interface_pattern_interface(pattern) {
			if t.interface_conversion_impl_mappings(subject_type, target_iface).len > 0 {
				return true
			}
		}
		if t.is_builtin_ierror_interface_name(subject_type) {
			t.record_monomorph_error('`${pattern}` is not compatible with `IError`')
		} else if t.specialized_is_pattern_known(pattern) {
			t.record_monomorph_error('`${pattern}` is not compatible with interface `${subject_type}`')
		} else {
			t.record_monomorph_error('unknown type `${pattern}`')
		}
		return false
	}
	t.record_monomorph_error('`is` can only be used with sum type or interface values, not `${subject_type}`')
	return false
}

fn (t &Transformer) specialized_is_pattern_known(pattern string) bool {
	for candidate in t.interface_pattern_candidates(pattern) {
		if types.is_builtin_type_name(candidate) || t.interface_pattern_candidate_known(candidate) {
			return true
		}
		clean := candidate.trim_space()
		if clean.starts_with('[]') || clean.starts_with('map[') || clean.starts_with('[')
			|| clean.starts_with('fn ') || clean.starts_with('fn(') {
			if t.tc.parse_type(clean).name() != 'unknown' {
				return true
			}
		}
	}
	return false
}

fn (t &Transformer) interface_impl_type_id(iface_name string, concrete_name string) ?int {
	if iface_name.len == 0 || concrete_name.len == 0 || isnil(t.tc) {
		return none
	}
	iface := t.resolve_interface_type_name(iface_name)
	if iface.len == 0 {
		return none
	}
	concrete := t.interface_concrete_impl_name(concrete_name) or { return none }
	requested_qualified := concrete_name.contains('.') || concrete != concrete_name
	iface_candidates := t.interface_impl_type_id_iface_candidates(iface)
	for impl_iface in iface_candidates {
		impl_names := if t.is_builtin_ierror_interface_name(impl_iface) {
			t.tc.ierror_impl_names()
		} else {
			t.tc.interface_impl_names(impl_iface)
		}
		type_ids := if t.is_builtin_ierror_interface_name(impl_iface) {
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
			impl_names := if t.is_builtin_ierror_interface_name(impl_iface) {
				t.tc.ierror_impl_names()
			} else {
				t.tc.interface_impl_names(impl_iface)
			}
			type_ids := if t.is_builtin_ierror_interface_name(impl_iface) {
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

fn (t &Transformer) interface_impl_type_id_iface_candidates(iface string) []string {
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
		if t.is_builtin_ierror_interface_name(candidate) || candidate in t.tc.interface_names {
			result << candidate
		}
	}
	return result
}

fn (t &Transformer) interface_concrete_impl_name(name string) ?string {
	clean := name.trim_space()
	if clean.starts_with('[]') {
		return 'array'
	}
	if clean.starts_with('map[') {
		return 'map'
	}
	if name in ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8', 'byte', 'u16',
		'u32', 'u64', 'f32', 'f64', 'string', 'char', 'rune'] {
		return name
	}
	if !name.contains('.') {
		if t.cur_file.len > 0 {
			for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
				[]string{}
			} {
				if candidate in t.tc.structs || candidate in t.tc.type_aliases {
					return candidate
				}
			}
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${name}'
			if qname in t.tc.structs || qname in t.tc.type_aliases {
				return qname
			}
		}
	}
	if name in t.tc.structs || name in t.tc.type_aliases {
		return name
	}
	for struct_name, _ in t.tc.structs {
		if struct_name.all_after_last('.') == name {
			return struct_name
		}
	}
	for alias_name, _ in t.tc.type_aliases {
		if alias_name.all_after_last('.') == name {
			return alias_name
		}
	}
	return none
}

// make_sum_is_check builds make sum is check data for transform.
fn (mut t Transformer) make_sum_is_check(expr flat.NodeId, expr_type string, sum_name string, variant string) flat.NodeId {
	tag := t.make_sum_tag_selector(expr, if expr_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	idx := t.make_int_literal(t.sum_type_index(sum_name, variant))
	return t.make_infix(.eq, tag, idx)
}

// sum_variant_path supports sum variant path handling for Transformer.
fn (t &Transformer) sum_variant_path(sum_name string, variant string) []string {
	clean_sum := t.trim_pointer_type(sum_name)
	// Resolve against the concrete application before its declaration key.
	// `Tree[int]` and `Maybe[int]` may be backed by `Tree`/`Maybe` entries whose
	// stored variants still contain their generic parameter names.
	for candidate in t.sum_subject_type_candidates(clean_sum) {
		if direct := t.sum_variant_name(candidate, variant) {
			return [direct]
		}
	}
	resolved_sum := t.resolve_sum_name(clean_sum)
	if direct := t.sum_variant_name(resolved_sum, variant) {
		return [direct]
	}
	mut visited := map[string]bool{}
	return t.sum_variant_path_inner(sum_name, variant, mut visited)
}

// sum_variant_path_inner supports sum variant path inner handling for Transformer.
fn (t &Transformer) sum_variant_path_inner(sum_name string, variant string, mut visited map[string]bool) []string {
	clean_sum := t.trim_pointer_type(sum_name)
	if direct := t.sum_variant_name(clean_sum, variant) {
		return [direct]
	}
	resolved_sum := t.resolve_sum_name(clean_sum)
	if resolved_sum.len == 0 || resolved_sum in visited {
		return []string{}
	}
	visited[resolved_sum] = true
	if direct := t.sum_variant_name(resolved_sum, variant) {
		return [direct]
	}
	variants := t.concrete_sum_variants_for_candidate(clean_sum)
	if variants.len == 0 {
		return []string{}
	}
	for direct in variants {
		direct_sum := t.resolve_sum_name(t.trim_pointer_type(direct))
		if direct_sum == resolved_sum || direct_sum !in t.sum_types {
			continue
		}
		nested := t.sum_variant_path_inner(direct_sum, variant, mut visited)
		if nested.len == 0 {
			continue
		}
		mut path := []string{cap: nested.len + 1}
		path << direct
		path << nested
		return path
	}
	return []string{}
}

// make_sum_type_pattern_check builds make sum type pattern check data for transform.
fn (mut t Transformer) make_sum_type_pattern_check(expr flat.NodeId, expr_type string, sum_name string, variant string) ?flat.NodeId {
	clean_sum := t.trim_pointer_type(sum_name)
	path := t.sum_variant_path(clean_sum, variant)
	if path.len == 0 {
		return none
	}
	mut current := expr
	mut current_type := expr_type
	mut current_sum := clean_sum
	mut chain := flat.empty_node
	for i, path_variant in path {
		cmp := t.make_sum_is_check(current, current_type, current_sum, path_variant)
		if int(chain) < 0 {
			chain = cmp
		} else {
			chain = t.make_infix(.logical_and, chain, cmp)
		}
		if i == path.len - 1 {
			break
		}
		qv := t.resolve_variant(current_sum, path_variant)
		use_ptr := t.variant_references_sum(qv, current_sum)
		field_type := if use_ptr { '&${qv}' } else { qv }
		current = t.make_selector_op(current, t.sum_field_name(qv), field_type, if current_type.starts_with('&') {
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
	expr_id := t.a.child(&node, 0)
	expr_type := t.node_type(expr_id)
	clean_type0 := t.trim_pointer_type(expr_type)
	if t.is_interface_type_name(clean_type0) {
		if target_iface := t.resolve_interface_pattern_interface(node.value) {
			child := t.transform_expr(expr_id)
			if converted := t.convert_interface_expr_to_interface(child, expr_type, target_iface) {
				return converted
			}
		}
	}
	clean_type := if clean_type0 in t.sum_types {
		clean_type0
	} else if _ := t.resolve_sum_variant_pattern_for_subject(clean_type0, node.value) {
		clean_type0
	} else {
		t.find_sum_type_for_variant(node.value)
	}
	resolved_clean_type := t.resolve_sum_name(clean_type)
	if clean_type.len == 0 || resolved_clean_type !in t.sum_types {
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
	qv := t.resolve_variant(clean_type, node.value)
	if qv.len > 0 && t.normalize_type_alias(clean_type0) == t.normalize_type_alias(qv) {
		return t.transform_expr(expr_id)
	}
	sc_key := t.expr_key(expr_id)
	if sc_key.len > 0 {
		contexts := t.smartcasts_for(sc_key)
		mut matched_contexts := []SmartcastContext{}
		for i, sc in contexts {
			mut sc_variant := sc.variant_name
			if sc.variant_name.contains('.') {
				sc_variant = sc.variant_name.all_after_last('.')
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
	field := t.sum_field_name(qv)
	new_expr := t.transform_expr(expr_id)
	use_ptr := t.variant_references_sum(qv, clean_type)
	field_typ := if use_ptr { '&${qv}' } else { qv }
	field_sel := t.make_selector_op(new_expr, field, field_typ, if expr_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	if use_ptr {
		return t.make_prefix(.mul, field_sel)
	}
	return field_sel
}

// wrap_sum_return_expr transforms wrap sum return expr data for transform.
fn (mut t Transformer) wrap_sum_return_expr(expr_id flat.NodeId) flat.NodeId {
	was_return_expr := t.in_return_expr
	t.in_return_expr = true
	result := if t.cur_fn_ret_type.len == 0 || t.cur_fn_ret_type !in t.sum_types {
		t.transform_expr(expr_id)
	} else {
		t.wrap_sum_value(expr_id, t.cur_fn_ret_type)
	}
	t.in_return_expr = was_return_expr
	return result
}

fn (mut t Transformer) fixed_array_sum_literal_expr(expr_id flat.NodeId, expr flat.Node, resolved_sum string) ?FixedArraySumLiteral {
	mut literal_id := expr_id
	mut literal := expr
	if expr.kind == .postfix && expr.op == .not && expr.children_count == 1 {
		literal_id = t.a.child(&expr, 0)
		literal = t.a.nodes[int(literal_id)]
	}
	if literal.kind != .array_literal {
		return none
	}
	variants := t.sum_types[resolved_sum] or { return none }
	literal_len := int(literal.children_count)
	literal_elem := t.array_literal_elem_type(literal)
	mut fallback_variant := ''
	mut fallback_type := ''
	mut fallback_count := 0
	for variant in variants {
		fixed_type := t.normalize_type_alias(variant)
		if !t.is_fixed_array_type(fixed_type) {
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
		if t.fixed_array_sum_literal_elem_matches(literal_elem, elem_type) {
			expr2 := t.transform_fixed_array_literal_for_type(literal_id, literal, fixed_type) or {
				continue
			}
			return FixedArraySumLiteral{
				variant: variant
				expr:    expr2
			}
		}
		if t.fixed_array_sum_literal_elem_unknown(literal_elem) {
			fallback_variant = variant
			fallback_type = fixed_type
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

fn (t &Transformer) fixed_array_sum_literal_elem_matches(literal_elem string, variant_elem string) bool {
	if t.fixed_array_sum_literal_elem_unknown(literal_elem) {
		return true
	}
	clean_literal := t.normalize_type_alias(literal_elem)
	clean_variant := t.normalize_type_alias(variant_elem)
	return clean_literal == clean_variant || t.variant_names_match(clean_literal, clean_variant)
}

fn (t &Transformer) fixed_array_sum_literal_elem_unknown(literal_elem string) bool {
	return literal_elem.len == 0 || literal_elem in ['unknown', 'void', 'array']
}

// wrap_sum_value transforms wrap sum value data for transform.
fn (mut t Transformer) wrap_sum_value(expr_id flat.NodeId, target_sum string) flat.NodeId {
	resolved_sum := t.resolve_sum_name(target_sum)
	if resolved_sum.len == 0 || resolved_sum !in t.sum_types {
		return t.transform_expr(expr_id)
	}
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .if_expr {
		branch_type := t.if_expr_branch_result_type(expr)
		if t.if_expr_branch_overrides_sum_target(branch_type, resolved_sum) {
			return t.transform_expr(expr_id)
		}
		if lowered := t.try_expand_if_expr_value_for_type(expr_id, expr, resolved_sum) {
			return lowered
		}
	}
	if fixed := t.fixed_array_sum_literal_expr(expr_id, expr, resolved_sum) {
		return t.make_sum_literal(resolved_sum, fixed.variant, fixed.expr)
	}
	mut expr_type := t.node_type(expr_id)
	if expr.typ.len > 0 && t.sum_target_accepts_variant_type(resolved_sum, expr.typ) {
		expr_type = expr.typ
	}
	if expr.kind == .ident && expr.value.len > 0 {
		local_type := t.raw_var_type(expr.value)
		if local_type.len > 0 && t.sum_target_accepts_variant_type(resolved_sum, local_type) {
			expr_type = local_type
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
	if has_expr_smartcast && t.resolve_sum_name(expr_smartcast.sum_type_name) == resolved_sum {
		variant = t.resolve_variant(expr_smartcast.sum_type_name, expr_smartcast.variant_name)
	}
	if expr.kind == .prefix && expr.op == .mul && expr.children_count > 0 {
		inner_type := t.node_type(t.a.child(&expr, 0))
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
		variant = t.node_type(t.a.child(&expr, 0))
	}
	if expr.kind !in [.assoc, .as_expr, .prefix] && !has_expr_smartcast
		&& t.resolve_sum_name(expr_type) == resolved_sum {
		return t.transform_expr(expr_id)
	}
	if expr_type.starts_with('&') && t.resolve_sum_name(expr_type[1..]) == resolved_sum {
		inner := t.transform_expr(expr_id)
		deref := t.make_prefix(.mul, inner)
		t.set_node_typ(int(deref), resolved_sum)
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
	short_variant := t.variant_short_name(clean_variant)
	mut matches := false
	mut matched_variant := clean_variant
	for v in t.sum_types[resolved_sum] {
		short_v := t.variant_short_name(v)
		if t.variant_names_match(v, clean_variant) || short_v == short_variant {
			matches = true
			matched_variant = v
			break
		}
	}
	if !matches && t.is_integer_type_name(clean_variant) {
		mut enum_variant := ''
		for v in t.sum_types[resolved_sum] {
			if t.enum_type_name_for_expected(v, t.cur_module).len == 0 {
				continue
			}
			if enum_variant.len > 0 {
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
		path := t.sum_variant_path(resolved_sum, clean_variant)
		if path.len == 1 {
			matches = true
			matched_variant = path[0]
		} else if path.len > 1 {
			nested_sum := t.resolve_sum_name(t.trim_pointer_type(path[0]))
			if nested_sum.len > 0 && nested_sum in t.sum_types {
				nested_value := t.wrap_sum_value(expr_id, nested_sum)
				return t.make_sum_literal(resolved_sum, path[0], nested_value)
			}
		}
	}
	if !matches {
		return t.transform_expr(expr_id)
	}
	ref_variant := t.variant_references_sum(matched_variant, resolved_sum)
	mut pointer_variant_child := flat.empty_node
	if expr.kind == .prefix && expr.op == .mul && expr.children_count > 0 {
		pointer_variant_child = t.a.child(&expr, 0)
	}
	inner := if int(pointer_variant_child) >= 0 && ref_variant {
		t.transform_expr(pointer_variant_child)
	} else {
		t.transform_expr(expr_id)
	}
	if expr_type.starts_with('&') {
		return t.make_sum_literal(resolved_sum, matched_variant, inner)
	}
	if int(pointer_variant_child) >= 0 && ref_variant {
		return t.make_sum_literal(resolved_sum, matched_variant, inner)
	}
	if ref_variant {
		return t.make_sum_literal(resolved_sum, matched_variant, inner)
	}
	start := t.a.children.len
	t.a.children << inner
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          resolved_sum
		children_start: start
		children_count: 1
		typ:            resolved_sum
	})
}

// ensure_sum_variant_ref supports ensure sum variant ref handling for Transformer.
fn (mut t Transformer) ensure_sum_variant_ref(value flat.NodeId, variant string) flat.NodeId {
	mut value_type := t.node_type(value)
	if value_type.starts_with('&') {
		return value
	}
	clean_variant := if variant.starts_with('&') { variant[1..] } else { variant }
	if value_type.len == 0 {
		value_type = clean_variant
	}
	if t.expr_can_take_address(value) || t.a.nodes[int(value)].kind == .struct_init {
		ref := t.make_prefix(.amp, value)
		t.set_node_typ(int(ref), '&${clean_variant}')
		return ref
	}
	tmp_name := t.new_temp('sum_val')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, value_type)
	ref := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.set_node_typ(int(ref), '&${clean_variant}')
	return ref
}

// make_default_sum_value initializes a sum type with the zero value of its first variant.
fn (mut t Transformer) make_default_sum_value(typ string) ?flat.NodeId {
	resolved_sum := t.resolve_sum_name(t.normalize_type_alias(typ))
	variants := t.sum_types[resolved_sum] or { return none }
	if variants.len == 0 {
		return none
	}
	variant := variants[0]
	return t.make_sum_literal(resolved_sum, variant, t.zero_value_for_type(variant))
}

// make_sum_literal builds make sum literal data for transform.
fn (mut t Transformer) make_sum_literal(sum_name string, variant string, value flat.NodeId) flat.NodeId {
	qvariant := t.resolve_variant(sum_name, variant)
	typ_field := t.make_sum_literal_field('typ', t.make_int_literal(t.sum_type_index(sum_name,
		qvariant)), 'int')
	raw_value_type := t.node_type(value)
	mut value_type := if raw_value_type.starts_with('&') {
		raw_value_type
	} else {
		qvariant
	}
	if t.variant_references_sum(qvariant, sum_name) && !value_type.starts_with('&') {
		value_type = '&${qvariant}'
	}
	value_field := t.make_sum_literal_field(t.sum_field_name(qvariant), value, value_type)
	start := t.a.children.len
	t.a.children << typ_field
	t.a.children << value_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          sum_name
		typ:            sum_name
	})
}

fn (mut t Transformer) make_sum_ref_literal(sum_name string, variant string, value flat.NodeId) flat.NodeId {
	qvariant := t.resolve_variant(sum_name, variant)
	typ_field := t.make_sum_literal_field('typ', t.make_int_literal(t.sum_type_index(sum_name,
		qvariant)), 'int')
	value_field := t.make_sum_literal_field(t.sum_field_name(qvariant), value,
		'sum_ref ${qvariant}')
	start := t.a.children.len
	t.a.children << typ_field
	t.a.children << value_field
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: 2
		value:          sum_name
		typ:            sum_name
	})
}

// make_sum_literal_field builds make sum literal field data for transform.
fn (mut t Transformer) make_sum_literal_field(name string, value flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	t.a.children << value
	return t.a.add_node(flat.Node{
		kind:           .field_init
		children_start: start
		children_count: 1
		value:          name
		typ:            typ
	})
}
