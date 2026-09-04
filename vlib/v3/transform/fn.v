module transform

import v3.flat
import v3.types

const spread_index_expected_type_marker = '__v3_spread_index_expected_type'

// max_stringify_nesting_depth bounds how deeply the inline autostr lowering
// (structs, sum types) recurses through *distinct* aggregate types before it
// defers the remaining expansion to synthesized helpers. The per-type circular
// guards only stop a type repeating on the stack; without this total-depth
// bound a deeply nested distinct-type graph (e.g. v1's ast.Expr/ast.Stmt
// sumtypes referencing dozens of node structs) expands combinatorially at
// every `${x}` site, which blows up node generation (region overflow /
// effectively unbounded work).
// Overridable at runtime with V3_STR_CAP for experiments. Kept low because the
// expansion is combinatorial in this depth: v1's ast graph is unbounded past ~5.
const max_stringify_nesting_depth = 3

// unresolved_interp_expansion_estimate is charged for an interpolation part whose
// value type cannot be resolved at collection time, forcing the function onto the
// serial deferred path in case the transform expands it inline.
const unresolved_interp_expansion_estimate = 1000

// A hoisted interpolation part adds two identifiers, a declaration node, and
// the declaration's two child IDs.
const string_interp_hoisted_part_expansion_estimate = 5

// recursive_pointer_str_expansion_threshold bounds an indirect recursive pointer
// expansion before it is rendered as circular. Small recursive structs retain the
// normal auto-str depth, while large object graphs avoid generating every branch
// before the repeated type is reached.
const recursive_pointer_str_expansion_threshold = 512

// resolve_call_name resolves the function name from a .call node.
// child[0] is the function expression: .ident for plain calls, .selector for method calls.
fn (t &Transformer) resolve_call_name(node flat.Node) string {
	if node.children_count == 0 {
		return ''
	}
	fn_id := t.a.children[node.children_start]
	if int(fn_id) < 0 {
		return ''
	}
	fn_node := t.a.nodes[int(fn_id)]
	match fn_node.kind {
		.ident {
			name := fn_node.value
			if t.var_type(name).len > 0 {
				return name
			}
			// Try qualified with current module
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
				qname := '${t.cur_module}.${name}'
				if t.is_known_fn_name(qname) {
					return qname
				}
			}
			// Try unqualified name after current-module authority.
			if t.is_known_fn_name(name) {
				return name
			}
			return name
		}
		.selector {
			if fn_node.children_count > 0 {
				base_id := t.a.children[fn_node.children_start]
				base := t.a.nodes[int(base_id)]
				if base.kind == .ident {
					full := '${base.value}.${fn_node.value}'
					if t.is_known_fn_name(full) {
						return full
					}
				}
				method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
				if method_name.len > 0 {
					return method_name
				}
				if base.kind == .ident {
					return '${base.value}.${fn_node.value}'
				}
			}
			return ''
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) local_fn_decl_return_type(name string) ?string {
	if name.len == 0 {
		return none
	}
	qname := transform_qualified_fn_name(t.cur_module, name)
	if ret := t.fn_ret_types[qname] {
		return ret
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[qname] {
			return t.semantic_type_name(ret)
		}
	}
	return none
}

fn (t &Transformer) local_fn_value_return_type_from_type(typ string) ?string {
	if typ.len == 0 {
		return none
	}
	normalized := t.normalize_type_alias(typ)
	return fn_type_return_type_text(normalized)
}

fn (t &Transformer) fn_value_call_return_type(node flat.Node) ?string {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee_id := t.a.child(&node, 0)
	if int(callee_id) < 0 {
		return none
	}
	callee := t.a.nodes[int(callee_id)]
	if callee.kind == .ident {
		local_type := t.var_type(callee.value)
		if ret := t.local_fn_value_return_type_from_type(local_type) {
			return ret
		}
	}
	if callee.kind == .selector {
		if raw_type := t.raw_selector_field_type(callee_id) {
			if ret := fn_type_return_type_text(raw_type) {
				return ret
			}
		}
	}
	if !isnil(t.tc) {
		mut callee_type := t.tc.expr_type(callee_id) or { t.tc.resolve_type(callee_id) }
		if fn_type := fn_type_from_type(callee_type) {
			return t.semantic_type_name(fn_type.return_type)
		}
	}
	callee_type_name := t.node_type(callee_id)
	if ret := t.local_fn_value_return_type_from_type(callee_type_name) {
		return ret
	}
	return none
}

fn fn_type_return_type_text(typ string) ?string {
	clean := typ.trim_space()
	if !(clean.starts_with('fn(') || clean.starts_with('fn (')) {
		return none
	}
	open_idx := clean.index_u8(`(`)
	if open_idx < 0 {
		return none
	}
	mut depth := 0
	for i in open_idx .. clean.len {
		if clean[i] == `(` {
			depth++
		} else if clean[i] == `)` {
			depth--
			if depth == 0 {
				ret := clean[i + 1..].trim_space()
				if ret.len == 0 {
					return 'void'
				}
				if ret in ['!', '?'] {
					return '${ret}void'
				}
				return ret
			}
		}
	}
	return none
}

// is_known_fn_name reports whether is known fn name applies in transform.
fn (t &Transformer) is_known_fn_name(name string) bool {
	if name in t.fn_ret_types {
		return true
	}
	if !isnil(t.tc) {
		return name in t.tc.fn_ret_types || name in t.tc.fn_param_types
	}
	return false
}

// resolve_receiver_method_name resolves resolve receiver method name information for transform.
fn (t &Transformer) resolve_receiver_method_name(base_id flat.NodeId, method string) string {
	if method.len == 0 {
		return ''
	}
	if smart_base_type := t.smartcast_receiver_type_name(base_id) {
		if alias_method := t.resolve_alias_receiver_method(smart_base_type, method) {
			return alias_method
		}
		if method_name := t.resolve_receiver_method_for_type(smart_base_type, method) {
			return method_name
		}
		if embedded_method := t.resolve_embedded_receiver_method(smart_base_type, method) {
			return embedded_method
		}
	}
	mut base_type := t.lvalue_type(base_id)
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.len == 0 {
		return ''
	}
	mut raw_var_clean := ''
	if raw_var_type := t.raw_var_type_for_expr(base_id) {
		raw_clean := if raw_var_type.starts_with('&') {
			raw_var_type[1..]
		} else {
			raw_var_type
		}
		raw_var_clean = raw_clean
		if raw_clean.len > 0 && raw_clean != base_type {
			if alias_method := t.resolve_alias_receiver_method(raw_clean, method) {
				if t.receiver_method_matches_type_name(alias_method, raw_clean) {
					return alias_method
				}
			}
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				if t.receiver_method_matches_type_name(method_name, raw_clean) {
					return method_name
				}
			}
		}
	}
	if raw_const_type := t.raw_const_type_name_for_expr(base_id) {
		raw_clean := if raw_const_type.starts_with('&') {
			raw_const_type[1..]
		} else {
			raw_const_type
		}
		if raw_clean.len > 0 && raw_clean != base_type && raw_clean != raw_var_clean {
			if alias_method := t.resolve_alias_receiver_method(raw_clean, method) {
				if t.receiver_method_matches_type_name(alias_method, raw_clean) {
					return alias_method
				}
			}
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				if t.receiver_method_matches_type_name(method_name, raw_clean) {
					return method_name
				}
			}
		}
	}
	if alias_method := t.resolve_alias_receiver_method(base_type, method) {
		if t.receiver_method_matches_base_type(alias_method, base_id) {
			return alias_method
		}
	}
	if method_name := t.resolve_receiver_method_for_type(base_type, method) {
		if t.receiver_method_matches_base_type(method_name, base_id) {
			return method_name
		}
	}
	if embedded_method := t.resolve_embedded_receiver_method(base_type, method) {
		// resolve_embedded_receiver_method already walked the embedding chain from
		// base_type, so the method is reachable by promotion. Its receiver is the
		// embedded type and legitimately differs from base_type (they can even share
		// a short name under a name collision), so the receiver-match guard used for
		// direct methods must not reject it here.
		return embedded_method
	}
	return ''
}

fn (t &Transformer) smartcast_receiver_type_name(base_id flat.NodeId) ?string {
	key := t.expr_key(base_id)
	if key.len == 0 {
		return none
	}
	sc := t.find_smartcast(key) or { return none }
	mut target := t.smartcast_target_type(sc)
	for target.starts_with('&') {
		target = target[1..]
	}
	if target.len == 0 {
		return none
	}
	return target
}

fn (t &Transformer) resolve_collection_receiver_method_name(base_id flat.NodeId, method string, clean_base_type string) string {
	if method.len == 0 {
		return ''
	}
	if raw_var_type := t.raw_var_type_for_expr(base_id) {
		raw_clean := if raw_var_type.starts_with('&') { raw_var_type[1..] } else { raw_var_type }
		if raw_clean.len > 0 && raw_clean != clean_base_type {
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				return method_name
			}
		}
	}
	if raw_const_type := t.raw_const_type_name_for_expr(base_id) {
		raw_clean := if raw_const_type.starts_with('&') {
			raw_const_type[1..]
		} else {
			raw_const_type
		}
		if raw_clean.len > 0 && raw_clean != clean_base_type {
			if method_name := t.resolve_receiver_method_for_type(raw_clean, method) {
				return method_name
			}
		}
	}
	if method_name := t.resolve_receiver_method_for_type(clean_base_type, method) {
		return method_name
	}
	return ''
}

// resolve_receiver_method_for_type resolves resolve_receiver_method_for_type logic in transform.
// Hot: called for every method call lowered during body transforms, with heavy
// repetition of (receiver type, method) pairs. The uncached resolution below
// scans struct tables and builds many candidate strings, so memoize per
// (module, type, method); the cache clears when the fn table grows (closure
// lifting / str-method synthesis can change what resolves).
fn (t &Transformer) resolve_receiver_method_for_type(receiver_type string, method string) ?string {
	if !isnil(t.receiver_method_cache) {
		mut cache := t.receiver_method_cache
		if !same_transform_text(cache.module, t.cur_module) || cache.fn_count != t.fn_ret_types.len {
			cache.module = t.cur_module
			cache.fn_count = t.fn_ret_types.len
			cache.entries.clear()
			cache.misses.clear()
		}
		cache_key := '${receiver_type}\n${method}'
		if cached := cache.entries[cache_key] {
			return cached
		}
		if cache.misses[cache_key] {
			return none
		}
		if resolved := t.resolve_receiver_method_for_type_uncached(receiver_type, method) {
			cache.entries[cache_key] = resolved
			return resolved
		}
		cache.misses[cache_key] = true
		return none
	}
	return t.resolve_receiver_method_for_type_uncached(receiver_type, method)
}

fn (t &Transformer) resolve_receiver_method_for_type_uncached(receiver_type string, method string) ?string {
	mut clean_type := receiver_type
	if clean_type.starts_with('&') {
		clean_type = clean_type[1..]
	}
	if clean_type.starts_with('map[') {
		for candidate in t.map_receiver_method_candidates(clean_type, method) {
			if t.is_known_fn_name(candidate) {
				return candidate
			}
		}
	} else {
		if method_name := t.resolve_specialized_generic_receiver_method(clean_type, method) {
			return method_name
		}
		if method_name := t.resolve_imported_flattened_generic_receiver_method(clean_type, method) {
			return method_name
		}
		if !isnil(t.tc) {
			if method_name := t.tc.concrete_method_signature_key(clean_type, method) {
				if t.is_known_fn_name(method_name) {
					return method_name
				}
			}
		}
		direct := '${clean_type}.${method}'
		if t.is_known_fn_name(direct) {
			return direct
		}
		if declared := t.declared_receiver_method(clean_type, method) {
			return declared
		}
		if clean_type.starts_with('main.') && !clean_type['main.'.len..].contains('.') {
			main_receiver := clean_type['main.'.len..]
			main_method := '${main_receiver}.${method}'
			if t.is_known_fn_name(main_method) {
				return main_method
			}
			// Test files retain their declared module even though their concrete
			// types enter an imported generic specialization as `main.Type`.
			// Resolve the unique module-qualified method for that concrete type.
			mut matched := ''
			suffix := '.${main_receiver}.${method}'
			for candidate, _ in t.fn_ret_types {
				if candidate.ends_with(suffix) {
					if matched.len > 0 && matched != candidate {
						matched = ''
						break
					}
					matched = candidate
				}
			}
			if !isnil(t.tc) {
				for candidate, _ in t.tc.fn_ret_types {
					if candidate.ends_with(suffix) {
						if matched.len > 0 && matched != candidate {
							matched = ''
							break
						}
						matched = candidate
					}
				}
			}
			if matched.len > 0 {
				return matched
			}
		}
		// A bare (unqualified) receiver type reached through a selective import
		// (`import cli { Command }`, then `cmd.add_flag()`): the method is registered
		// under the declaring module's qualified name (`cli.Command.add_flag`). The
		// selective-import table can be unavailable this late, so scan for the unique
		// module-qualified struct that both shares the short name AND declares the
		// method (disambiguating e.g. `cli.Command` from `os.Command`). Only do this
		// when the bare name is NOT a locally-declared type: a real local `Command`
		// that lacks the method is a genuine missing-method error, not an import.
		if !clean_type.contains('.') && !clean_type.contains('[') && !isnil(t.tc)
			&& !t.tc.is_locally_declared_bare_type(clean_type) {
			mut matched := ''
			for sname, _ in t.tc.structs {
				if !sname.contains('.') || sname.contains('[')
					|| short_name_view(sname) != clean_type {
					continue
				}
				qmethod := '${sname}.${method}'
				if t.is_known_fn_name(qmethod) {
					if matched.len > 0 && matched != qmethod {
						matched = ''
						break
					}
					matched = qmethod
				}
			}
			if matched.len > 0 {
				return matched
			}
		}
		for receiver in generic_receiver_flat_type_variants(clean_type) {
			flat_method := '${receiver}.${method}'
			if t.is_known_fn_name(flat_method) {
				return flat_method
			}
		}
		for receiver in flattened_generic_receiver_short_variants(clean_type) {
			flat_method := '${receiver}.${method}'
			if t.is_known_fn_name(flat_method) {
				return flat_method
			}
		}
		if t.is_interface_type_name(clean_type) && !isnil(t.tc)
			&& method !in t.tc.interface_abstract_method_names(clean_type) {
			if embedded_interface_method := t.tc.interface_method_signature_key(clean_type, method) {
				if embedded_interface_method != direct
					&& t.is_known_fn_name(embedded_interface_method) {
					return embedded_interface_method
				}
			}
		}
	}
	if clean_type.starts_with('[]') {
		elem_type := clean_type[2..]
		short_elem := if elem_type.contains('.') {
			elem_type.all_after_last('.')
		} else {
			elem_type
		}
		short_array := '[]${short_elem}.${method}'
		if t.is_known_fn_name(short_array) {
			return short_array
		}
		if elem_type.contains('.') {
			qualified_array := '${elem_type.all_before_last('.')}.[]${short_elem}.${method}'
			if t.is_known_fn_name(qualified_array) {
				return qualified_array
			}
		} else if transform_can_prefix_collection_receiver(t.cur_module) {
			current_module_array := '${t.cur_module}.[]${short_elem}.${method}'
			if t.is_known_fn_name(current_module_array) {
				return current_module_array
			}
		}
	} else if clean_type.contains('.') {
		short_type := clean_type.all_after_last('.')
		short_method := '${short_type}.${method}'
		if t.is_known_fn_name(short_method) {
			return short_method
		}
		qualified_short_method := '${clean_type.all_before_last('.')}.${short_type}.${method}'
		if t.is_known_fn_name(qualified_short_method) {
			return qualified_short_method
		}
	}
	if method_name := t.unique_receiver_method_suffix_match(t.receiver_method_candidates(clean_type, method)) {
		return method_name
	}
	if !isnil(t.tc) {
		if target := t.alias_target_type_preserving_main_lock(clean_type) {
			if target != clean_type {
				if alias_method := t.resolve_receiver_method_for_type(target, method) {
					return alias_method
				}
			}
		}
	}
	return none
}

fn (t &Transformer) resolve_imported_flattened_generic_receiver_method(receiver_type string, method string) ?string {
	if receiver_type.contains('.') || !receiver_type.contains('_')
		|| t.bare_struct_name_is_local_to_current_module(receiver_type) {
		return none
	}
	for short_base, qualified_base in t.qualified_types {
		prefix := '${short_base}_'
		if !receiver_type.starts_with(prefix) || qualified_base == short_base {
			continue
		}
		candidate := '${qualified_base}_${receiver_type[prefix.len..]}.${method}'
		if t.is_known_fn_name(candidate) {
			return candidate
		}
	}
	return none
}

fn (t &Transformer) declared_receiver_method(receiver string, method string) ?string {
	if receiver.len == 0 || method.len == 0 {
		return none
	}
	clean_receiver := if receiver.starts_with('main.') {
		receiver['main.'.len..]
	} else {
		receiver
	}
	target := '${clean_receiver}.${method}'
	target_count := t.declared_fn_name_counts[target]
	lowered := c_name(target)
	if lowered == target {
		return if target_count == 1 { target } else { none }
	}
	lowered_count := t.declared_fn_name_counts[lowered]
	if target_count + lowered_count != 1 {
		return none
	}
	return if target_count == 1 { target } else { lowered }
}

fn (t &Transformer) unique_receiver_method_suffix_match(candidates []string) ?string {
	mut found := ''
	for candidate in candidates {
		name := t.receiver_method_suffix_index[candidate] or { continue }
		if name == receiver_method_suffix_ambiguous {
			return none
		}
		if found.len > 0 && found != name {
			return none
		}
		found = name
	}
	if found.len == 0 {
		return none
	}
	return found
}

fn (t &Transformer) resolve_specialized_generic_receiver_method(receiver_type string, method string) ?string {
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 {
		return none
	}
	// Synthetic calls created after checking can retain the imported receiver's
	// short generic spelling (`QueryBuilder[Foo]`). Resolve its unique owner
	// before enumerating flattened specializations, otherwise the emitted C call
	// loses the `orm__` prefix even though the specialization itself is qualified.
	if !base.contains('.') && !t.bare_struct_name_is_local_to_current_module(base) {
		if qualified := t.qualified_types[base] {
			if qualified != base {
				if resolved := t.resolve_specialized_generic_receiver_method('${qualified}[${args.join(', ')}]', method) {
					return resolved
				}
			}
		}
	}
	short_args := generic_type_args_short(args)
	suffix := generic_type_suffixes(args)
	mut candidates := []string{}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		candidates << '${t.cur_module}.${base}[${short_args}].${method}'
		candidates << '${t.cur_module}.${base}_${suffix}.${method}'
		candidates << c_name('${t.cur_module}.${base}_${suffix}.${method}')
	}
	candidates << '${base}[${short_args}].${method}'
	candidates << '${base}_${suffix}.${method}'
	candidates << c_name('${base}_${suffix}.${method}')
	if base.contains('.') {
		module_name := base.all_before_last('.')
		short_base := base.all_after_last('.')
		candidates << '${short_base}[${short_args}].${method}'
		candidates << '${short_base}_${suffix}.${method}'
		candidates << '${module_name}.${short_base}_${suffix}.${method}'
		candidates << c_name('${module_name}.${short_base}_${suffix}.${method}')
	}
	for candidate in candidates {
		if t.is_known_fn_name(candidate) {
			return candidate
		}
	}
	return none
}

// resolve_alias_receiver_method converts resolve alias receiver method data for transform.
fn (t &Transformer) resolve_alias_receiver_method(base_type string, method string) ?string {
	if isnil(t.tc) || base_type.len == 0 || method.len == 0 {
		return none
	}
	// Key the memo on the raw spelling so cache hits skip the trim/normalize
	// allocations below; the result is a pure function of the spelling anyway.
	cache_key := '${t.cur_module}\n${base_type}\n${method}'
	if !isnil(t.alias_receiver_method_cache) {
		mut cache := t.alias_receiver_method_cache
		if cached := cache.entries[cache_key] {
			return cached
		}
		if cache.misses[cache_key] {
			return none
		}
	}
	raw_base := base_type.trim_space().trim_left('&')
	clean_base := t.normalize_type_alias(base_type)
	mut exact_aliases := [raw_base]
	if !raw_base.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		exact_aliases << '${t.cur_module}.${raw_base}'
	}
	if imported := t.resolve_imported_type_name(raw_base) {
		exact_aliases << imported
	}
	for alias in exact_aliases {
		alias_method := '${alias}.${method}'
		if alias in t.tc.type_aliases && t.is_known_fn_name(alias_method) {
			if !isnil(t.alias_receiver_method_cache) {
				mut cache := t.alias_receiver_method_cache
				cache.entries[cache_key] = alias_method
			}
			return alias_method
		}
	}
	if alias_method := t.alias_methods['${clean_base}.${method}'] {
		if !isnil(t.alias_receiver_method_cache) {
			mut cache := t.alias_receiver_method_cache
			cache.entries[cache_key] = alias_method
		}
		return alias_method
	}
	if !t.is_integer_type_name(clean_base) {
		if !isnil(t.alias_receiver_method_cache) {
			mut cache := t.alias_receiver_method_cache
			cache.misses[cache_key] = true
		}
		return none
	}
	for name, params in t.tc.fn_param_types {
		if !name.ends_with('.${method}') || params.len == 0 {
			continue
		}
		receiver_name := name.all_before_last('.')
		if receiver_name.len == 0 || receiver_name !in t.tc.type_aliases {
			continue
		}
		param_name := t.semantic_type_name(params[0])
		if t.alias_receiver_type_matches(clean_base, param_name) {
			if !isnil(t.alias_receiver_method_cache) {
				mut cache := t.alias_receiver_method_cache
				cache.entries[cache_key] = name
			}
			return name
		}
	}
	if !isnil(t.alias_receiver_method_cache) {
		mut cache := t.alias_receiver_method_cache
		cache.misses[cache_key] = true
	}
	return none
}

// alias_target_type_preserving_main_lock returns the underlying type while retaining
// an explicit `main.` lock introduced by generic specialization. The lock prevents
// an imported generic body from rebasing a program type to a same-named local type.
fn (t &Transformer) alias_target_type_preserving_main_lock(base_type string) ?string {
	if isnil(t.tc) || base_type.len == 0 {
		return none
	}
	mut clean := base_type.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..]
	}
	mut candidates := [clean]
	main_locked := clean.starts_with('main.') && !clean['main.'.len..].contains('.')
	if main_locked {
		candidates << clean['main.'.len..]
	} else {
		if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
			candidates << '${t.cur_module}.${clean}'
		}
		if imported := t.resolve_imported_type_name(clean) {
			if imported !in candidates {
				candidates << imported
			}
		}
	}
	for candidate in candidates {
		target := t.tc.type_aliases[candidate] or { continue }
		if main_locked && !target.contains('.') && !target.contains('[') {
			return 'main.${target}'
		}
		return target
	}
	return none
}

fn (t &Transformer) resolve_embedded_receiver_method(base_type string, method string) ?string {
	if base_type.len == 0 || method.len == 0 {
		return none
	}
	mut lookup_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if alias_target := t.alias_target_type_preserving_main_lock(lookup_type) {
		if alias_target != lookup_type {
			if method_name := t.resolve_embedded_receiver_method(alias_target, method) {
				return method_name
			}
		}
	}
	if lookup_type !in t.structs && lookup_type.contains('.') {
		short_type := lookup_type.all_after_last('.')
		if short_type in t.structs {
			lookup_type = short_type
		}
	}
	fields := t.embedded_fields[lookup_type] or { return none }
	for field in fields {
		field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		clean_field := if field_type.starts_with('&') { field_type[1..] } else { field_type }
		if method_name := t.resolve_receiver_method_for_type(clean_field, method) {
			return method_name
		}
		if method_name := t.resolve_embedded_receiver_method(clean_field, method) {
			return method_name
		}
	}
	return none
}

// alias_receiver_type_matches converts alias receiver type matches data for transform.
fn (t &Transformer) alias_receiver_type_matches(base_type string, alias_type string) bool {
	if base_type.len == 0 || alias_type.len == 0 {
		return false
	}
	clean_alias := if alias_type.starts_with('&') { alias_type[1..] } else { alias_type }
	alias_target := t.normalize_type_alias(clean_alias)
	if alias_target == base_type {
		return true
	}
	if !isnil(t.tc) {
		alias_c_type := t.tc.c_type(t.tc.parse_type(alias_target))
		base_c_type := t.tc.c_type(t.tc.parse_type(base_type))
		if alias_c_type == base_c_type {
			return true
		}
	}
	return t.is_integer_type_name(alias_target) && t.is_integer_type_name(base_type)
}

// is_integer_type_name reports whether is integer type name applies in transform.
fn (t &Transformer) is_integer_type_name(typ string) bool {
	return typ in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'byte', 'u16', 'u32', 'u64', 'rune',
		'isize', 'usize']
}

// raw_var_type_for_expr supports raw var type for expr handling for Transformer.
fn (t &Transformer) raw_var_type_for_expr(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		typ := t.raw_var_type(node.value)
		if typ.len > 0 {
			return typ
		}
	}
	if node.kind == .cast_expr && node.value.len > 0 {
		return node.value
	}
	if node.kind == .selector {
		if raw_type := t.raw_selector_field_type(id) {
			return raw_type
		}
	}
	if node.typ.len > 0 {
		return node.typ
	}
	return none
}

// raw_const_type_name_for_expr supports raw const type name for expr handling for Transformer.
fn (t &Transformer) raw_const_type_name_for_expr(id flat.NodeId) ?string {
	if int(id) < 0 || isnil(t.tc) {
		return none
	}
	node := t.a.nodes[int(id)]
	if t.selector_const_base_is_value(node) {
		return none
	}
	if node.kind == .ident && t.raw_var_type(node.value).len > 0 {
		return none
	}
	name := t.expr_key(id)
	if name.len == 0 {
		return none
	}
	key := t.const_type_key_in_context(name, t.cur_module, t.cur_file) or { return none }
	typ := t.tc.const_types[key] or { return none }
	return t.semantic_type_name(typ)
}

// resolve_method_receiver_type determines the receiver type for method calls.
// For a call where child[0] is a .selector, resolves the type of the selector's base expression.
fn (t &Transformer) resolve_method_receiver_type(call_node flat.Node) string {
	if call_node.children_count == 0 {
		return ''
	}
	fn_id := t.a.children[call_node.children_start]
	if int(fn_id) < 0 {
		return ''
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return ''
	}
	base_id := t.a.children[fn_node.children_start]
	return t.resolve_expr_type(base_id)
}

fn (mut t Transformer) normalize_implicit_receiver_generic_call(id flat.NodeId, node flat.Node) flat.NodeId {
	if t.cur_fn_receiver_name.len == 0 || node.children_count == 0 {
		return id
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value.contains('.') {
		return id
	}
	decls := t.cached_generic_fn_decls()
	receiver_type := t.var_type(t.cur_fn_receiver_name)
	for key in t.generic_receiver_methods_by_name[callee.value] {
		decl := decls[key] or { continue }
		if !t.generic_receiver_decl_matches_type(receiver_type, decl, t.cur_module)
			|| !t.generic_call_arg_count_matches_decl_with_implicit_receiver(node, decl) {
			continue
		}
		mut children := []flat.NodeId{cap: int(node.children_count) + 1}
		children << t.make_ident(key)
		children << t.make_ident(t.cur_fn_receiver_name)
		for i in 1 .. node.children_count {
			children << t.a.child(&node, i)
		}
		start := t.a.children.len
		t.a.children << children
		return t.a.add_node(flat.Node{
			kind: .call
			op: node.op
			children_start: start
			children_count: flat.child_count(children.len)
			pos: node.pos
			value: node.value
			typ: node.typ
		})
	}
	return id
}

// normalize_generic_call_expr transforms normalize generic call expr data for transform.
fn (mut t Transformer) normalize_generic_call_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return id
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return id
	}
	if t.index_callee_is_value_index(fn_node) {
		return id
	}
	base_id := t.a.child(&fn_node, 0)
	base := t.a.nodes[int(base_id)]
	if base.kind !in [.ident, .selector] {
		return id
	}
	type_arg := t.generic_call_type_args_name(fn_node)
	if type_arg.len == 0 {
		return id
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	children << base_id
	for i in 1 .. node.children_count {
		children << t.a.child(&node, i)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	mut resolved_named_call := false
	if !isnil(t.tc) {
		if _ := t.tc.resolved_call_name(id) {
			resolved_named_call = true
		}
	}
	return t.a.add_node(flat.Node{
		kind: .call
		op: node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos: node.pos
		value: if !resolved_named_call && t.generic_call_base_is_fn_value(base_id, base) {
			''
		} else {
			type_arg
		}
		typ: node.typ
	})
}

fn (t &Transformer) generic_call_base_is_fn_value(base_id flat.NodeId, base flat.Node) bool {
	if base.kind != .ident || t.is_known_fn_name(base.value) || t.is_known_type_name(base.value) {
		return false
	}
	for candidate in [t.raw_var_type(base.value), t.var_type(base.value), base.typ,
		t.node_type(base_id)] {
		if t.is_fn_pointer_type_name(candidate) {
			return true
		}
	}
	return false
}

// generic_call_type_arg_name supports generic call type arg name handling for Transformer.
fn (t &Transformer) generic_call_type_arg_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			// Map type arguments are represented by a synthetic `Map_key_value`
			// identifier, while `typ` retains the source-level `map[key]value`.
			// Keep the latter so comptime type groups and generic substitution can
			// still inspect both map components.
			if node.typ.starts_with('map[') {
				return node.typ
			}
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := t.generic_call_type_arg_name(t.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := t.generic_call_type_arg_name(t.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := t.generic_call_type_arg_name(t.a.child(&node, i))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len > 0 {
				if node.value.starts_with('[]') {
					return '[]${node.value}'
				}
				if node.value.starts_with('[') {
					return node.value
				}
				return '[]${node.value}'
			}
			return ''
		}
		.map_init {
			return node.value
		}
		.struct_init {
			return node.value
		}
		.struct_decl {
			return node.value
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := t.generic_call_type_arg_name(t.a.child(&node, 0))
			if child.len == 0 {
				return ''
			}
			if node.op == .amp {
				return '&${child}'
			}
			return child
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) generic_call_type_args_name(index_node flat.Node) string {
	if index_node.kind != .index || index_node.children_count < 2 || index_node.value == 'range' {
		return ''
	}
	if t.index_callee_is_value_index(index_node) {
		return ''
	}
	mut args := []string{}
	for i in 1 .. index_node.children_count {
		arg := t.generic_call_type_arg_name(t.a.child(&index_node, i))
		if arg.len == 0 {
			return ''
		}
		args << arg
	}
	return args.join(', ')
}

fn (t &Transformer) index_callee_is_value_index(index_node flat.Node) bool {
	if index_node.kind != .index || index_node.children_count == 0 || index_node.value == 'range' {
		return false
	}
	base_id := t.a.child(&index_node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := t.a.nodes[int(base_id)]
	if t.type_name_is_indexable(base.typ)
		|| t.type_name_is_indexable(t.raw_checker_node_type(base_id)) {
		return true
	}
	if base.kind == .ident && t.type_name_is_indexable(t.var_type(base.value)) {
		return true
	}
	if base.kind == .ident || base.kind == .selector {
		base_type := t.resolve_expr_type(base_id)
		if t.type_name_is_indexable(base_type) {
			return true
		}
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(base_id) {
			if transform_type_is_indexable(typ) {
				return true
			}
		}
		if transform_type_is_indexable(t.tc.resolve_type(base_id)) {
			return true
		}
	}
	return false
}

fn (t &Transformer) type_name_is_indexable(name string) bool {
	mut clean := t.normalize_type_alias(name)
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') {
		clean = t.normalize_type_alias(clean[1..])
	}
	return clean == 'string' || clean.starts_with('[]') || clean.starts_with('map[')
		|| t.is_fixed_array_type(clean)
}

fn transform_type_is_indexable(typ types.Type) bool {
	match typ {
		types.Array, types.ArrayFixed, types.Map, types.String {
			return true
		}
		types.Alias {
			return transform_type_is_indexable(typ.base_type)
		}
		types.Pointer {
			return transform_type_is_indexable(typ.base_type)
		}
		else {
			return false
		}
	}
}

// transform_call_args transforms all children of a call expression.
// child[0] is the function expression, children[1..n] are arguments.
@[direct_array_access]
fn (mut t Transformer) transform_call_args(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.a.add_node(flat.Node{
			kind: .call
			op: node.op
			pos: node.pos
			value: node.value
			typ: node.typ
		})
	}
	if addr := t.transform_builtin_addr_call(node) {
		return addr
	}
	call_name := t.call_name_for_node(id, node)
	if call_name in ['json.encode', 'json.encode_pretty', 'json.decode'] {
		return t.transform_cgen_json_encode_call(id, node)
	}
	mut params := t.call_param_types_for_node(call_name, node)
	mut param_type_names := t.call_param_type_names(params)
	mut is_generic_variadic := false
	if concrete_params := t.concrete_generic_call_param_types(id, node) {
		params = concrete_params.clone()
		is_generic_variadic = t.concrete_generic_call_is_variadic(id, node)
		if concrete_names := t.concrete_generic_call_param_type_names(id, node) {
			param_type_names = concrete_names.clone()
		} else {
			param_type_names = t.call_param_type_names(params)
		}
	}
	param_offset := if t.call_is_selector_form(node) && t.concrete_generic_call_is_method(id) {
		1
	} else {
		t.call_param_offset_for_node(call_name, node, params)
	}
	explicit_args := int(node.children_count) - 1
	expected_explicit := params.len - param_offset
	variadic_arg_pos := 1 + params.len - 1 - param_offset
	has_spread_at_variadic_slot := variadic_arg_pos > 0 && variadic_arg_pos < node.children_count
		&& t.call_arg_is_spread(t.a.child(&node, variadic_arg_pos))
	is_c_variadic := t.tc.c_variadic_fns[call_name]
	is_variadic := !is_c_variadic && (t.call_is_variadic(call_name)
		|| is_generic_variadic || (params.len > 0 && params[params.len - 1] is types.Array
		&& (explicit_args > expected_explicit || has_spread_at_variadic_slot)))
	variadic_idx := if is_variadic && params.len > 0 && params[params.len - 1] is types.Array {
		params.len - 1
	} else {
		-1
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	saved_in_call_callee := t.in_call_callee
	t.in_call_callee = true
	callee_id := t.a.children[node.children_start]
	immediate_bound_method := t.immediate_bound_method_value_allocates_runtime_closure(callee_id)
	immediate_factory_closure := t.call_returns_exclusive_closure(callee_id)
	immediate_fresh_closure := immediate_bound_method || immediate_factory_closure
	mut immediate_closure_type := ''
	mut immediate_closure_cleanup := ''
	immediate_closure_capture_may_escape := immediate_bound_method || immediate_factory_closure
		|| t.immediate_fn_literal_capture_may_escape(callee_id)
	if immediate_fresh_closure {
		immediate_closure_type = t.fresh_runtime_closure_type(callee_id) or { '' }
		if immediate_closure_type.len > 0 {
			// If/match callback branches otherwise retain the callee's return
			// type on their value temporary instead of the callback type.
			t.set_fresh_runtime_closure_expr_type(callee_id, immediate_closure_type)
		}
	}
	mut transformed_callee := t.const_fn_call_target(callee_id) or {
		if param_offset == 1 && params.len > 0 {
			if converted_callee := t.transform_method_callee_receiver_for_param(callee_id, t.semantic_type_name(params[0])) {
				converted_callee
			} else {
				t.transform_expr(callee_id)
			}
		} else {
			t.transform_expr(callee_id)
		}
	}
	t.in_call_callee = saved_in_call_callee
	if t.fn_literal_has_runtime_captures(callee_id) || immediate_fresh_closure {
		closure_type := if immediate_closure_type.len > 0 {
			immediate_closure_type
		} else {
			t.fresh_runtime_closure_type(callee_id) or { t.node_type(transformed_callee) }
		}
		if closure_type.len > 0 {
			t.set_node_typ(int(transformed_callee), closure_type)
			if immediate_bound_method {
				t.mark_fn_used_name('closure.closure_create_with_data')
				t.mark_fresh_runtime_closure_methods_used(callee_id)
			}
			closure_name := t.new_temp('immediate_closure')
			t.set_var_type(closure_name, closure_type)
			t.pending_stmts << t.make_decl_assign_typed(closure_name, transformed_callee, closure_type)
			if t.in_spawn_expr {
				// The spawn argument packet owns this runtime closure until the worker
				// has invoked it.
				t.mark_fn_used_name('closure.closure_try_destroy')
			} else {
				immediate_closure_cleanup = closure_name
			}
			transformed_callee = t.make_ident(closure_name)
			t.set_node_typ(int(transformed_callee), closure_type)
		}
	}
	new_children << transformed_callee
	mut i := 1
	mut variadic_tail_supplied := false
	for i < node.children_count {
		arg_idx := new_children.len - 1
		param_idx := arg_idx + param_offset
		arg_id := t.a.child(&node, i)
		arg_node := t.a.nodes[int(arg_id)]
		param_type := if param_idx < param_type_names.len {
			param_type_names[param_idx]
		} else {
			''
		}
		if spread_args := t.transform_spread_arg_over_fixed_variadic_tail(arg_node, param_idx, variadic_idx, params) {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				new_children << t.fixed_variadic_spread_args_with_trailing(spread_args, node, i + 1, variadic_type)
			} else {
				new_children << spread_args
			}
			variadic_tail_supplied = true
			i++
			break
		}
		if arg_node.kind == .field_init {
			// Trailing `key: value` args against the variadic `...Struct` slot
			// (surfacing as `[]Struct`) desugar to one element of the elem
			// struct type; a non-variadic `[]Struct` param must not collapse.
			struct_param_type := if variadic_idx >= 0 && param_idx == variadic_idx
				&& param_type.starts_with('[]') {
				param_type[2..]
			} else {
				param_type
			}
			if packed_arg := t.transform_params_struct_call_arg(node, i, struct_param_type) {
				new_children << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
			if packed_arg := t.transform_struct_call_arg(node, i, struct_param_type) {
				new_children << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
		}
		if variadic_idx >= 0 && param_idx == variadic_idx {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				if arg_node.kind == .prefix && arg_node.value == '...'
					&& arg_node.children_count > 0 {
					spread_id := t.a.child(&arg_node, 0)
					new_children << t.transform_variadic_spread_arg_for_param(spread_id, variadic_type, param_type)
					i++
					break
				}
				remaining := int(node.children_count) - i
				if remaining == 1 {
					arg_type := t.node_type(arg_id)
					if arg_type.starts_with('[]')
						&& !t.variadic_interface_single_array_should_box(arg_type, variadic_type) {
						new_children << t.transform_call_arg_for_named_param(arg_id, param_type, call_name)
					} else {
						new_children << t.pack_variadic_args(node, i, variadic_type.elem_type)
					}
				} else {
					new_children << t.pack_variadic_args(node, i, variadic_type.elem_type)
				}
				break
			}
		}
		if variadic_idx < 0 && arg_node.kind == .prefix && arg_node.value == '...'
			&& arg_node.children_count > 0 && params.len > 0 && param_idx < params.len {
			spread_id := t.a.child(&arg_node, 0)
			spread_base := t.stable_expr_for_reuse(spread_id)
			spread_count := params.len - param_idx
			for spread_offset in 0 .. spread_count {
				expected := t.semantic_type_name(params[param_idx + spread_offset])
				index_arg := t.make_spread_index_for_expected_param(spread_base, spread_offset, expected)
				new_children << t.transform_call_arg_for_named_param(index_arg, expected, call_name)
			}
			i++
			continue
		}
		if variadic_idx < 0 && param_idx < params.len {
			arg_type := t.tc.resolve_type(arg_id)
			if arg_type is types.MultiReturn && arg_type.types.len == params.len - param_idx {
				items := arg_type.types
				multi_type := t.multi_return_type_name(items)
				value := t.stable_transformed_expr_for_reuse(t.transform_expr(arg_id), multi_type, 'multi_arg')
				for multi_idx, item_type in items {
					expected_idx := param_idx + multi_idx
					if expected_idx >= params.len {
						break
					}
					field := t.make_selector(value, 'arg${multi_idx}', t.semantic_type_name(item_type))
					new_children << t.transform_call_arg_for_named_param(field, param_type_names[expected_idx], call_name)
				}
				i++
				continue
			}
		}
		new_children << t.transform_call_arg_for_named_param(arg_id, param_type, call_name)
		i++
	}
	if variadic_idx >= 0 && !variadic_tail_supplied && explicit_args == variadic_idx - param_offset {
		variadic_type := params[variadic_idx]
		if variadic_type is types.Array {
			new_children << t.pack_variadic_args(node, int(node.children_count), variadic_type.elem_type)
		}
	}
	t.append_missing_params_struct_args(mut new_children, params, param_offset)
	mut typ := node.typ
	concrete_ret := t.concrete_generic_call_return_type(id, node)
	if concrete_ret.len > 0 {
		typ = concrete_ret
	} else if typ.len == 0 && immediate_closure_type.len > 0 {
		typ = fn_type_return_type_text(immediate_closure_type) or { '' }
	}
	if t.rewrite_children_in_place(id, new_children) {
		if typ != t.a.nodes[int(id)].typ {
			t.set_node_typ(int(id), typ)
		}
		return t.finish_immediate_closure_call(id, immediate_closure_cleanup, typ, immediate_closure_capture_may_escape)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind: .call
		op: node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos: node.pos
		value: if int(id) >= 0 && int(id) < t.a.nodes.len {
			t.a.nodes[int(id)].value
		} else {
			node.value
		}
		typ: typ
	})
	t.copy_cloned_resolution(id, new_id)
	if spec := t.generic_call_spec_cache[int(id)] {
		mut cached_args := spec.args.clone()
		if new_children.len > 0 {
			callee := t.a.nodes[int(new_children[0])]
			if callee.kind == .ident {
				if exact := t.recorded_generic_specialization_args(callee.value) {
					cached_args = exact.clone()
				}
			}
		}
		t.generic_call_spec_cache[int(new_id)] = GenericCallSpec{
			decl_key: spec.decl_key
			args: cached_args
		}
	}
	return t.finish_immediate_closure_call(new_id, immediate_closure_cleanup, typ, immediate_closure_capture_may_escape)
}

fn (mut t Transformer) finish_immediate_closure_call(call_id flat.NodeId, closure_name string, typ string, capture_may_escape bool) flat.NodeId {
	if closure_name.len == 0 {
		return call_id
	}
	if capture_may_escape || t.immediate_closure_result_may_alias_capture(typ) {
		t.pending_stmts << t.make_local_closure_cleanup_defer(closure_name)
		return call_id
	}
	if typ.len == 0 || typ == 'void' {
		t.pending_stmts << t.make_expr_stmt(call_id)
		t.pending_stmts << t.make_local_closure_destroy_stmt(closure_name)
		return t.make_int_literal(0)
	}
	result_name := t.new_temp('immediate_closure_result')
	t.set_var_type(result_name, typ)
	t.pending_stmts << t.make_decl_assign_typed(result_name, call_id, typ)
	t.pending_stmts << t.make_local_closure_destroy_stmt(closure_name)
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), typ)
	return result
}

fn (t &Transformer) immediate_fn_literal_capture_may_escape(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.paren, .cast_expr] && node.children_count == 1 {
		return t.immediate_fn_literal_capture_may_escape(t.a.child(&node, 0))
	}
	if node.kind != .fn_literal {
		return false
	}
	mut captures := map[string]bool{}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .ident && child.value.len > 0 && child.value !in t.active_generic_params {
			captures[child.value] = true
		}
	}
	if captures.len == 0 {
		return false
	}
	mut capture_aliases := captures.clone()
	for {
		alias_count := capture_aliases.len
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			child := t.a.nodes[int(child_id)]
			if child.kind !in [.ident, .param] {
				t.collect_capture_derived_names(child_id, mut capture_aliases)
			}
		}
		if capture_aliases.len == alias_count {
			break
		}
	}
	mut capture_address_aliases := map[string]bool{}
	for {
		alias_count := capture_address_aliases.len
		for i in 0 .. node.children_count {
			child_id := t.a.child(&node, i)
			child := t.a.nodes[int(child_id)]
			if child.kind !in [.ident, .param] {
				t.collect_capture_address_derived_names(child_id, capture_aliases, mut capture_address_aliases)
			}
		}
		if capture_address_aliases.len == alias_count {
			break
		}
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind !in [.ident, .param] && t.expr_may_escape_any_named_capture(child_id, capture_aliases, capture_address_aliases) {
			return true
		}
	}
	return false
}

fn (t &Transformer) collect_capture_derived_names(id flat.NodeId, mut names map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if node.kind in [.decl_assign, .assign] && node.children_count >= 2 {
		mut i := 0
		for i + 1 < node.children_count {
			lhs := t.a.child_node(&node, i)
			rhs_id := t.a.child(&node, i + 1)
			if lhs.kind == .ident && lhs.value.len > 0 && t.expr_mentions_any_name(rhs_id, names) {
				names[lhs.value] = true
			}
			i += 2
		}
	}
	for i in 0 .. node.children_count {
		t.collect_capture_derived_names(t.a.child(&node, i), mut names)
	}
}

fn (t &Transformer) collect_capture_address_derived_names(id flat.NodeId, captures map[string]bool, mut address_names map[string]bool) {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.fn_literal, .lambda_expr, .fn_decl] {
		return
	}
	if node.kind in [.decl_assign, .assign] && node.children_count >= 2 {
		mut i := 0
		for i + 1 < node.children_count {
			lhs := t.a.child_node(&node, i)
			rhs_id := t.a.child(&node, i + 1)
			mut derives_from_address := t.expr_mentions_any_name(rhs_id, address_names)
			if !derives_from_address {
				for name, _ in captures {
					if t.fn_literal_expr_takes_address_of_capture(rhs_id, name) {
						derives_from_address = true
						break
					}
				}
			}
			if lhs.kind == .ident && lhs.value.len > 0 && derives_from_address {
				address_names[lhs.value] = true
			}
			i += 2
		}
	}
	for i in 0 .. node.children_count {
		t.collect_capture_address_derived_names(t.a.child(&node, i), captures, mut address_names)
	}
}

fn (t &Transformer) expr_may_escape_any_named_capture(id flat.NodeId, captures map[string]bool, capture_address_aliases map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	// Calls and channel sends can retain values outside the immediate closure, and
	// assignments can project a capture-derived value into external storage.
	// Conservatively keep the context through the enclosing scope at these boundaries.
	if (node.kind in [.call, .spawn_expr] || (node.kind == .infix && node.op == .arrow))
		&& t.expr_mentions_any_name(id, captures) {
		return true
	}
	if node.kind == .return_stmt {
		if t.expr_mentions_any_name(id, capture_address_aliases) {
			return true
		}
		for name, _ in captures {
			if t.fn_literal_expr_takes_address_of_capture(id, name) {
				return true
			}
		}
	}
	if node.kind in [.assign, .selector_assign, .index_assign] && node.children_count >= 2 {
		for i := 1; i < node.children_count; i += 2 {
			if t.expr_mentions_any_name(t.a.child(&node, i), captures) {
				return true
			}
		}
	}
	for i in 0 .. node.children_count {
		if t.expr_may_escape_any_named_capture(t.a.child(&node, i), captures, capture_address_aliases) {
			return true
		}
	}
	return false
}

fn (t &Transformer) expr_mentions_any_name(id flat.NodeId, names map[string]bool) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value in names {
		return true
	}
	for i in 0 .. node.children_count {
		if t.expr_mentions_any_name(t.a.child(&node, i), names) {
			return true
		}
	}
	return false
}

fn (t &Transformer) immediate_closure_result_may_alias_capture(type_name string) bool {
	if type_name.len == 0 {
		return false
	}
	clean := t.normalize_type_alias(type_name)
	if clean == 'thread' || clean.starts_with('thread ') {
		return true
	}
	if isnil(t.tc) {
		return clean == 'string' || clean.starts_with('!') || clean.starts_with('?')
			|| clean.starts_with('&') || clean.starts_with('[]') || clean.starts_with('map[')
			|| clean.starts_with('chan ') || clean in ['voidptr', 'byteptr', 'charptr']
	}
	mut seen := map[string]bool{}
	return t.closure_result_type_may_alias_capture(t.tc.parse_type(type_name), mut seen)
}

fn (t &Transformer) closure_result_type_may_alias_capture(typ types.Type, mut seen map[string]bool) bool {
	type_name := typ.name()
	if type_name == 'thread' || type_name.starts_with('thread ') {
		return true
	}
	return match typ {
		types.Pointer {
			true
		}
		types.Alias {
			t.closure_result_type_may_alias_capture(typ.base_type, mut seen)
		}
		types.OptionType {
			t.closure_result_type_may_alias_capture(typ.base_type, mut seen)
		}
		types.ResultType {
			// A custom IError implementation can retain a pointer into the capture even
			// when the successful payload is scalar.
			true
		}
		types.String, types.Array, types.Channel, types.Map {
			true
		}
		types.ArrayFixed {
			t.closure_result_type_may_alias_capture(typ.elem_type, mut seen)
		}
		types.Struct {
			if typ.name in seen {
				false
			} else {
				seen[typ.name] = true
				mut may_alias := false
				for field in t.tc.struct_fields_for_type(typ.name) {
					if t.closure_result_type_may_alias_capture(field.typ, mut seen) {
						may_alias = true
						break
					}
				}
				may_alias
			}
		}
		types.SumType {
			if typ.name in seen {
				false
			} else {
				seen[typ.name] = true
				mut may_alias := false
				for variant in t.concrete_sum_variants_for_candidate(typ.name) {
					if t.closure_result_type_may_alias_capture(t.tc.parse_type(variant), mut seen) {
						may_alias = true
						break
					}
				}
				may_alias
			}
		}
		types.MultiReturn {
			mut may_alias := false
			for item in typ.types {
				if t.closure_result_type_may_alias_capture(item, mut seen) {
					may_alias = true
					break
				}
			}
			may_alias
		}
		types.FnType, types.Interface {
			true
		}
		else {
			false
		}
	}
}

fn (mut t Transformer) transform_cgen_json_encode_call(id flat.NodeId, node flat.Node) flat.NodeId {
	mut children := []flat.NodeId{cap: int(node.children_count)}
	call_name := t.call_name_for_node(id, node)
	saved_in_call_callee := t.in_call_callee
	t.in_call_callee = true
	children << t.transform_expr(t.a.child(&node, 0))
	t.in_call_callee = saved_in_call_callee
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 1 && call_name in ['json.encode', 'json.encode_pretty']
			&& t.expr_has_smartcast(child_id) {
			original_type := t.trim_pointer_type(t.original_expr_type(child_id))
			if t.is_sum_type_name(original_type) {
				// JSON sum values need the runtime tag so the encoder can append `_type`.
				// An earlier `assert value is Variant` may otherwise lower this argument
				// to the bare payload and discard that tag before cgen sees it.
				children << t.make_plain_expr_for_smartcast(child_id)
				continue
			}
		}
		children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	t.a.children << children
	new_id := t.a.add_node(flat.Node{
		kind: .call
		op: node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos: node.pos
		value: node.value
		typ: node.typ
	})
	t.copy_cloned_resolution(id, new_id)
	return new_id
}

fn (t &Transformer) call_param_type_names(params []types.Type) []string {
	mut names := []string{cap: params.len}
	for param in params {
		names << if t.memo_call_param_type_names && !isnil(t.tc) {
			t.tc.type_name(param)
		} else {
			param.name()
		}
	}
	return names
}

fn (t &Transformer) variadic_interface_single_array_should_box(arg_type string, variadic_type types.Array) bool {
	if !arg_type.starts_with('[]') {
		return false
	}
	if t.normalize_type_alias(arg_type) == t.normalize_type_alias(t.semantic_type_name(variadic_type)) {
		return false
	}
	elem_type := variadic_type.elem_type
	if elem_type is types.Interface {
		return true
	}
	if elem_type is types.Alias {
		return t.variadic_interface_single_array_elem_should_box(elem_type.base_type)
	}
	return false
}

fn (t &Transformer) variadic_interface_single_array_elem_should_box(elem_type types.Type) bool {
	if elem_type is types.Interface {
		return true
	}
	if elem_type is types.Alias {
		return t.variadic_interface_single_array_elem_should_box(elem_type.base_type)
	}
	return false
}

fn (mut t Transformer) fixed_variadic_spread_args_with_trailing(spread_args []flat.NodeId, node flat.Node, trailing_start int, variadic_type types.Array) []flat.NodeId {
	mut args := spread_args.clone()
	if trailing_start < node.children_count && args.len > 0 {
		tail_idx := args.len - 1
		args[tail_idx] = t.append_trailing_args_to_variadic_tail(args[tail_idx], node, trailing_start, variadic_type)
	}
	return args
}

fn (mut t Transformer) transform_spread_arg_over_fixed_variadic_tail(arg_node flat.Node, param_idx int, variadic_idx int, params []types.Type) ?[]flat.NodeId {
	if variadic_idx < 0 || param_idx < 0 || param_idx >= variadic_idx || param_idx >= params.len {
		return none
	}
	if arg_node.kind != .prefix || arg_node.value != '...' || arg_node.children_count == 0 {
		return none
	}
	variadic_type := params[variadic_idx]
	if variadic_type !is types.Array {
		return none
	}
	spread_id := t.a.child(&arg_node, 0)
	variadic_array_type := t.semantic_type_name(variadic_type)
	spread_type0 := t.node_type(spread_id)
	spread_type := if spread_type0.len > 0 { spread_type0 } else { variadic_array_type }
	spread_expr := t.stable_transformed_expr_for_reuse(t.transform_call_arg_for_param(spread_id, spread_type), spread_type, 'varargs_spread')
	mut args := []flat.NodeId{cap: variadic_idx - param_idx + 1}
	for fixed_idx in param_idx .. variadic_idx {
		elem := t.make_index(spread_expr, t.make_int_literal(fixed_idx - param_idx), t.semantic_type_name(params[fixed_idx]))
		args << elem
	}
	tail_start := variadic_idx - param_idx
	spread_elem_type := t.array_elem_type(t.normalize_type_alias(spread_type))
	if spread_elem_type == t.normalize_type_alias(variadic_array_type) {
		args << t.make_index(spread_expr, t.make_int_literal(tail_start), variadic_array_type)
	} else {
		args << t.make_range_index(spread_expr, t.make_int_literal(tail_start), flat.empty_node, variadic_array_type)
	}
	return args
}

fn (mut t Transformer) transform_call_arg_for_named_param(arg_id flat.NodeId, param_type string, call_name string) flat.NodeId {
	// C declarations often use `voidptr` as an intentionally opaque placeholder
	// for a native by-value type whose full declaration lives in an inserted C or
	// Objective-C source. Match the legacy backend by leaving such C arguments in
	// value form; callers that need an actual pointer spell `&value` explicitly.
	if call_name.starts_with('C.') && transform_param_type_is_void_pointer(param_type) {
		return t.transform_expr(arg_id)
	}
	return t.transform_call_arg_for_param(arg_id, param_type)
}

fn (mut t Transformer) transform_variadic_spread_arg_for_param(spread_id flat.NodeId, variadic_type types.Array, param_type string) flat.NodeId {
	if !isnil(t.tc) && int(spread_id) >= 0 {
		actual := types.unwrap_pointer(t.tc.resolve_type(spread_id))
		if actual is types.Array {
			expected := types.Type(variadic_type)
			actual_elem := t.normalize_type_alias(t.semantic_type_name(actual.elem_type))
			expected_elem := t.normalize_type_alias(t.semantic_type_name(variadic_type.elem_type))
			if actual_elem != expected_elem
				&& t.sum_target_accepts_variant_type(expected_elem, actual_elem) {
				return t.convert_forwarded_array_to_dynamic(spread_id, actual, actual.elem_type, expected, variadic_type.elem_type, false)
			}
		}
	}
	return t.transform_call_arg_for_param(spread_id, param_type)
}

fn (mut t Transformer) make_range_index(base flat.NodeId, start_id flat.NodeId, end_id flat.NodeId, typ string) flat.NodeId {
	mut children := []flat.NodeId{cap: 3}
	children << base
	children << start_id
	if int(end_id) >= 0 {
		children << end_id
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind: .index
		value: 'range'
		children_start: start
		children_count: flat.child_count(children.len)
		typ: typ
	})
}

fn (t &Transformer) call_arg_is_spread(arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	arg := t.a.nodes[int(arg_id)]
	return arg.kind == .prefix && arg.value == '...' && arg.children_count > 0
}

fn (mut t Transformer) const_fn_call_target(callee_id flat.NodeId) ?flat.NodeId {
	if int(callee_id) < 0 || isnil(t.tc) {
		return none
	}
	raw_type := t.raw_const_type_name_for_expr(callee_id) or { return none }
	if !raw_type.starts_with('fn ') {
		return none
	}
	expr_id := t.const_expr_for_arg(callee_id) or { return none }
	expr := t.a.nodes[int(expr_id)]
	if expr.kind !in [.ident, .selector] {
		return none
	}
	return t.transform_expr(expr_id)
}

// try_lower_join_path_call supports try lower join path call handling for Transformer.
fn (mut t Transformer) try_lower_join_path_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	call_name := t.call_name_for_node(id, node)
	if call_name != 'join_path' && call_name != 'os.join_path' {
		return none
	}
	// A spread argument (`...rest`) has a runtime-determined length and cannot be
	// unrolled into nested join_path_single calls at compile time. Defer to the real
	// variadic os.join_path in that case.
	for i in 1 .. node.children_count {
		arg := t.a.child_node(&node, i)
		if arg.kind == .prefix && arg.value == '...' {
			return none
		}
	}
	if node.children_count <= 1 {
		return t.make_string_literal('')
	}
	t.mark_fn_used('os.join_path_single')
	mut result := t.transform_expr(t.a.child(&node, 1))
	for i in 2 .. node.children_count {
		arg := t.transform_expr(t.a.child(&node, i))
		result = t.make_call_typed('os.join_path_single', [result, arg], 'string')
	}
	return result
}

// transform_params_struct_call_arg transforms transform params struct call arg data for transform.
fn (mut t Transformer) transform_params_struct_call_arg(node flat.Node, field_start int, param_type string) ?flat.NodeId {
	struct_type := t.params_struct_type_name(param_type) or { return none }
	return t.transform_trailing_field_init_struct_arg(node, field_start, struct_type)
}

fn (mut t Transformer) transform_struct_call_arg(node flat.Node, field_start int, param_type string) ?flat.NodeId {
	struct_type := t.struct_arg_type_name(param_type) or { return none }
	return t.transform_trailing_field_init_struct_arg(node, field_start, struct_type)
}

fn (mut t Transformer) transform_trailing_field_init_struct_arg(node flat.Node, field_start int, struct_type string) ?flat.NodeId {
	mut field_ids := []flat.NodeId{}
	for i in field_start .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init {
			break
		}
		field_ids << field_id
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	struct_id := t.a.add_node(flat.Node{
		kind: .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value: struct_type
		typ: struct_type
	})
	return t.transform_struct_fields(struct_id, t.a.nodes[int(struct_id)])
}

fn (t &Transformer) struct_arg_type_name(param_type string) ?string {
	if param_type.len == 0 {
		return none
	}
	mut typ := param_type
	if typ.starts_with('&') {
		return none
	}
	typ = t.normalize_type_alias(typ)
	if _ := t.lookup_struct_info(typ) {
		return typ
	}
	return none
}

// next_non_field_init_arg returns next non field init arg data for Transformer.
fn (t &Transformer) next_non_field_init_arg(node flat.Node, field_start int) int {
	mut i := field_start
	for i < node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			break
		}
		i++
	}
	return i
}

// params_struct_type_name supports params struct type name handling for Transformer.
fn (t &Transformer) params_struct_type_name(param_type string) ?string {
	if param_type.len == 0 {
		return none
	}
	mut typ := param_type
	if typ.starts_with('&') {
		typ = typ[1..]
	}
	if !isnil(t.tc) && typ in t.tc.params_structs {
		return typ
	}
	if info := t.lookup_struct_info(typ) {
		if info.is_params {
			return typ
		}
	}
	normalized := t.normalize_type_alias(typ)
	if normalized != typ {
		if !isnil(t.tc) && normalized in t.tc.params_structs {
			return normalized
		}
		if info := t.lookup_struct_info(normalized) {
			if info.is_params {
				return normalized
			}
		}
	}
	return none
}

// call_name_for_node updates call name for node state for Transformer.
fn (t &Transformer) call_name_for_node(id flat.NodeId, node flat.Node) string {
	if !isnil(t.tc) {
		if name := t.tc.resolved_call_name(id) {
			if node.children_count > 0 && t.cur_module.len > 0
				&& t.cur_module !in ['main', 'builtin'] {
				fn_node := t.a.child_node(&node, 0)
				short_name := short_name_view(name)
				if fn_node.kind == .ident && fn_node.value == short_name {
					qname := '${t.cur_module}.${short_name}'
					lowered_qname := c_name(qname)
					if qname in t.fn_ret_types || qname in t.tc.fn_ret_types
						|| lowered_qname in t.fn_ret_types || lowered_qname in t.tc.fn_ret_types {
						return qname
					}
				}
			}
			return name
		}
	}
	return t.resolve_call_name(node)
}

// call_param_offset updates call param offset state for Transformer.
fn (t &Transformer) call_param_offset(call_name string, node flat.Node, params []types.Type) int {
	if params.len == 0 || node.children_count == 0 {
		return 0
	}
	mut fn_node := t.a.nodes[int(t.a.child(&node, 0))]
	if fn_node.kind == .index && fn_node.children_count > 0 && fn_node.value != 'range' {
		index_base := t.a.nodes[int(t.a.child(&fn_node, 0))]
		if index_base.kind == .selector {
			fn_node = index_base
		}
	}
	if fn_node.kind != .selector {
		return 0
	}
	if fn_node.children_count == 0 {
		if t.selector_call_name_has_receiver_param(call_name, fn_node.value, params) {
			return 1
		}
		return 0
	}
	base_id := t.a.child(fn_node, 0)
	base_node := t.a.nodes[int(base_id)]
	if base_node.kind == .ident && (base_node.value == 'C'
		|| t.selector_is_lexical_module_call(base_id, fn_node.value, call_name)
		|| t.is_import_alias_ident(base_id)) {
		return 0
	}
	if base_node.kind == .ident && base_node.value.len > 0 && base_node.value[0] >= `a`
		&& base_node.value[0] <= `z`
		&& t.selector_call_name_has_receiver_param(call_name, fn_node.value, params) {
		return 1
	}
	// `module.Type.fn(...)` / `Type.fn(...)` is a static associated function call, not a
	// method: the base names a type, not a value, so no receiver must be prepended.
	if _ := t.static_assoc_fn_name(base_id, fn_node.value) {
		return 0
	}
	// Generic/comptime clones can lose the receiver's local binding metadata even
	// though the resolved call name and declaration signature remain exact. Use
	// that signature to keep explicit arguments one slot past the receiver.
	if t.selector_call_name_has_receiver_param(call_name, fn_node.value, params) {
		return 1
	}
	method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
	if method_name.len == 0 {
		if t.receiver_method_param_offset(base_id, node, params, '') == 1 {
			return 1
		}
		return 0
	}
	if t.receiver_method_param_offset(base_id, node, params, method_name) == 1 {
		return 1
	}
	if call_name.len == 0 || call_name == method_name || call_name == c_name(method_name) {
		return 1
	}
	return 0
}

fn (t &Transformer) call_param_offset_for_node(call_name string, node flat.Node, params []types.Type) int {
	mut param_offset := t.call_param_offset(call_name, node, params)
	if param_offset != 0 || call_name.len == 0 || params.len == 0 {
		return param_offset
	}
	selector_id := t.call_selector_callee_id(node) or { return param_offset }
	selector := t.a.nodes[int(selector_id)]
	if selector.children_count == 0 {
		return param_offset
	}
	base_id := t.a.child(&selector, 0)
	base := t.a.nodes[int(base_id)]
	first := types.unwrap_all_pointers(params[0])
	base_is_lexical_module := t.selector_is_lexical_module_call(base_id, selector.value, call_name)
	base_is_value := base.kind != .ident || t.raw_var_type(base.value).len > 0
		|| (base.value.len > 0 && base.value[0] >= `a` && base.value[0] <= `z`)
	if base_is_value && !base_is_lexical_module && !t.is_import_alias_ident(base_id)
		&& t.selector_call_name_has_receiver_param(call_name, selector.value, params) {
		param_offset = 1
	} else if first is types.Interface && base_is_value && !base_is_lexical_module
		&& !t.is_import_alias_ident(base_id) {
		// Interface method signatures include the receiver in slot zero. A
		// generic receiver can still be spelled `H` here, so name matching in
		// call_param_offset cannot recognize it; keep explicit args aligned.
		param_offset = 1
	}
	return param_offset
}

fn (t &Transformer) call_selector_callee_id(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 || int(fn_id) >= t.a.nodes.len {
		return none
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind == .selector {
		return fn_id
	}
	if fn_node.kind == .index && fn_node.children_count > 0 && fn_node.value != 'range'
		&& !t.index_callee_is_value_index(fn_node) {
		base_id := t.a.child(&fn_node, 0)
		if int(base_id) >= 0 && int(base_id) < t.a.nodes.len {
			base := t.a.nodes[int(base_id)]
			if base.kind == .selector {
				return base_id
			}
		}
	}
	return none
}

fn (t &Transformer) selector_call_name_has_receiver_param(call_name string, method string, params []types.Type) bool {
	if params.len == 0 || method.len == 0 || call_name.len == 0
		|| !call_name.ends_with('.${method}') {
		return false
	}
	first_type := t.normalize_type_alias(t.semantic_type_name(params[0]))
	clean_first := if first_type.starts_with('&') { first_type[1..] } else { first_type }
	if receiver_param_matches_method_name(clean_first, call_name) {
		return true
	}
	receiver := call_name.all_before_last('.')
	clean_receiver := if receiver.starts_with('&') { receiver[1..] } else { receiver }
	if receiver_param_types_match(clean_first, clean_receiver)
		|| t.normalize_type_alias(clean_first) == t.normalize_type_alias(clean_receiver)
		|| short_name_view(clean_first) == short_name_view(clean_receiver) {
		return true
	}
	return false
}

fn (mut t Transformer) transform_method_callee_receiver_for_param(callee_id flat.NodeId, param_type string) ?flat.NodeId {
	if int(callee_id) < 0 || param_type.len == 0 {
		return none
	}
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	base_id := t.a.child(&callee, 0)
	base_type := t.node_type(base_id)
	if !t.is_interface_type(param_type) || !t.is_interface_type(base_type)
		|| t.resolve_interface_type_name(param_type) == t.resolve_interface_type_name(base_type) {
		return none
	}
	new_base := t.transform_expr_for_type(base_id, param_type)
	start := t.a.children.len
	t.a.children << new_base
	return t.a.add_node(flat.Node{
		kind: .selector
		children_start: start
		children_count: 1
		pos: callee.pos
		value: callee.value
		typ: callee.typ
	})
}

// static_assoc_fn_name returns the name of the static associated function a selector
// call resolves to, if the base names a type (`Type.fn` or `module.Type.fn`) and that
// function exists. Such calls take no receiver, so the base must not be prepended as an
// argument. Returns none for ordinary method calls (base is a value).
fn (t &Transformer) static_assoc_fn_name(base_id flat.NodeId, method string) ?string {
	if method.len == 0 {
		return none
	}
	base := t.a.nodes[int(base_id)]
	if base.kind == .ident {
		if base.value == 'C' || t.is_import_alias_ident(base_id) {
			return none
		}
		if t.var_type(base.value).len > 0 {
			return none
		}
		base_type := t.node_type(base_id)
		clean_base_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
		if clean_base_type.len > 0 && clean_base_type != 'unknown' && clean_base_type != 'void'
			&& base.value != clean_base_type && base.value != clean_base_type.all_after_last('.') {
			return none
		}
		for type_name in t.static_assoc_type_candidates(base.value) {
			name := '${type_name}.${method}'
			if t.is_known_fn_name(name) {
				return name
			}
		}
	} else if base.kind == .selector && base.children_count > 0 {
		inner := t.a.child_node(&base, 0)
		if inner.kind == .ident {
			type_ident := '${inner.value}.${base.value}'
			for type_name in t.static_assoc_type_candidates(type_ident) {
				name := '${type_name}.${method}'
				if t.is_known_fn_name(name) {
					return name
				}
			}
		}
	}
	return none
}

fn (t &Transformer) selector_is_lexical_module_call(base_id flat.NodeId, method string, call_name string) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len || isnil(t.tc) {
		return false
	}
	base := t.a.node(base_id)
	if base.kind != .ident || base.value.len == 0 || method.len == 0 {
		return false
	}
	file := t.a.source_files[base.pos.id] or { return false }
	module_name := t.tc.file_imports[file.name + '\n' + base.value] or { return false }
	return call_name == '${module_name}.${method}'
}

// call_selector_base_is_namespace reports whether the base of a selector callee names a
// compile-time namespace rather than a runtime value: the `C` pseudo-module, an imported module
// (`os.abs_path(...)`, `flat.node_payload(...)`), or a type in a static associated call
// (`Type.make(...)`, `mod.Type.make(...)`). Such a base has no storage, so the call-operand
// ordering guards must not stabilize or snapshot it — spilling it declares an undeclared
// identifier at an unusable type (`unknown __order_snapshot_0 = os;`,
// `void __order_snapshot_0 = mod__Type;`) — and must treat only the arguments as operands.
fn (t &Transformer) call_selector_base_is_namespace(base_id flat.NodeId, method string, call_name string) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len {
		return false
	}
	base := t.a.nodes[int(base_id)]
	// The module and `C` spellings are single identifiers, but a static associated call also
	// reaches its type through a selector (`mod.Type.make(...)`), so that check must see both.
	if base.kind == .ident {
		if base.value == 'C' || t.selector_is_lexical_module_call(base_id, method, call_name)
			|| t.is_import_alias_ident(base_id) {
			return true
		}
	}
	if _ := t.static_assoc_fn_name(base_id, method) {
		return true
	}
	return false
}

fn (t &Transformer) is_import_alias_ident(id flat.NodeId) bool {
	if int(id) < 0 || isnil(t.tc) {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .ident {
		return false
	}
	// A module may export a constant with the same short name as the import
	// alias (`import core.flags`, where that module also exports `flags`). In a
	// selector callee such as `flags.parse()`, the file import remains the
	// syntactic namespace unless a local value shadows it.
	if _ := t.tc.file_imports[file_import_key(t.cur_file, node.value)] {
		return t.raw_var_type(node.value).len == 0
	}
	if node.value !in t.tc.imports {
		return false
	}
	// A local or receiver can shadow an import alias. Generic clones may no longer
	// retain an expression type for the identifier, but their binding table still
	// records the value type.
	if t.raw_var_type(node.value).len > 0 {
		return false
	}
	if typ := t.tc.expr_type(id) {
		name := t.semantic_type_name(typ)
		if name.len > 0 && name != 'unknown' && name != 'void' {
			return false
		}
	}
	return true
}

fn (t &Transformer) static_assoc_type_candidates(type_ident string) []string {
	if type_ident.len == 0 {
		return []string{}
	}
	mut candidates := []string{}
	t.add_static_assoc_type_candidate(mut candidates, type_ident)
	if !type_ident.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		t.add_static_assoc_type_candidate(mut candidates, '${t.cur_module}.${type_ident}')
	}
	if !isnil(t.tc) {
		qname := t.tc.qualify_name(type_ident)
		t.add_static_assoc_type_candidate(mut candidates, qname)
	}
	mut result := []string{}
	for candidate in candidates {
		if t.is_static_assoc_type_name(candidate) && candidate !in result {
			result << candidate
		}
	}
	return result
}

fn (t &Transformer) add_static_assoc_type_candidate(mut candidates []string, name string) {
	if name.len == 0 {
		return
	}
	if name !in candidates {
		candidates << name
	}
	if isnil(t.tc) {
		return
	}
	if target := t.tc.type_aliases[name] {
		if target !in candidates {
			candidates << target
		}
	}
}

fn (t &Transformer) is_static_assoc_type_name(type_name string) bool {
	if type_name in t.structs || type_name in t.enum_types || type_name in t.sum_types {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return type_name in t.tc.structs || type_name in t.tc.enum_names || type_name in t.tc.sum_types
		|| type_name in t.tc.interface_names || type_name in t.tc.type_aliases
}

// try_lower_static_assoc_call lowers `Type.method(args)` to a direct call to
// `Type.method(args)` once the checker/transformer has proven that the base is a
// type, not a value receiver.
fn (mut t Transformer) try_lower_static_assoc_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	mut fn_node := t.a.nodes[int(fn_id)]
	mut generic_args := node.value
	if fn_node.kind == .index && fn_node.value != 'range' && fn_node.children_count > 1 {
		generic_args = t.generic_call_type_args_name(fn_node)
		fn_node = *t.a.child_node(&fn_node, 0)
	}
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	static_fn := t.static_assoc_fn_name(base_id, fn_node.value) or { return none }
	normalized := t.transform_call_args(id, node)
	normalized_node := t.a.nodes[int(normalized)]
	mut args := []flat.NodeId{cap: int(normalized_node.children_count) - 1}
	for i in 1 .. normalized_node.children_count {
		args << t.a.child(&normalized_node, i)
	}
	ret_type := if generic_args.len > 0 && node.typ.len > 0 {
		node.typ
	} else {
		t.receiver_method_return_type(static_fn, node.typ)
	}
	call := t.make_call_typed(static_fn, args, ret_type)
	if generic_args.len > 0 {
		t.set_node_value(int(call), generic_args)
	} else if node.value.len > 0 {
		t.set_node_value(int(call), node.value)
	}
	if node.generic_params().len > 0 {
		t.set_node_generic_params(int(call), node.generic_params())
	}
	return call
}

// call_param_types updates call param types state for Transformer.
fn (mut t Transformer) call_param_types(call_name string) []types.Type {
	if call_name.len == 0 || isnil(t.tc) {
		return []types.Type{}
	}
	if params := t.call_param_types_from_decl(call_name) {
		return params
	}
	params := t.tc.fn_param_types[call_name] or { return []types.Type{} }
	return params
}

fn (mut t Transformer) call_param_types_for_node(call_name string, node flat.Node) []types.Type {
	if params := t.call_param_types_from_decl(call_name) {
		return params
	}
	if node.children_count > 0 {
		fn_id := t.a.child(&node, 0)
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident && t.var_type(fn_node.value).len > 0 {
			if fn_type := t.call_callee_fn_type(fn_id) {
				return fn_type.params.clone()
			}
			return []types.Type{}
		}
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
			if method_name.len > 0 {
				if params := t.call_param_types_from_decl(method_name) {
					return params
				}
			}
			mut base_type := t.lvalue_type(base_id)
			if base_type.starts_with('&') {
				base_type = base_type[1..]
			}
			for candidate in ['${base_type}.${fn_node.value}',
				'${t.normalize_type_in_module(base_type, t.cur_module)}.${fn_node.value}'] {
				if params := t.call_param_types_from_decl(candidate) {
					return params
				}
			}
		}
		if (call_name.len == 0 || call_name !in t.tc.fn_ret_types)
			&& call_name !in t.tc.fn_param_types {
			if fn_type := t.call_callee_fn_type(fn_id) {
				return fn_type.params.clone()
			}
		}
	}
	return t.call_param_types(call_name)
}

fn (t &Transformer) call_callee_fn_type(fn_id flat.NodeId) ?types.FnType {
	if int(fn_id) < 0 || isnil(t.tc) {
		return none
	}
	node := t.a.nodes[int(fn_id)]
	if node.kind == .ident {
		for local_type in [t.raw_var_type(node.value), t.var_type(node.value)] {
			if local_type.len == 0 {
				continue
			}
			if fn_type := transform_fn_type(t.tc.parse_type(local_type)) {
				return fn_type
			}
		}
	}
	if typ := t.tc.expr_type(fn_id) {
		if fn_type := transform_fn_type(typ) {
			return fn_type
		}
	}
	if node.typ.len > 0 {
		if fn_type := transform_fn_type(t.tc.parse_type(node.typ)) {
			return fn_type
		}
	}
	return transform_fn_type(t.tc.resolve_type(fn_id))
}

fn (mut t Transformer) call_param_types_from_decl(call_name string) ?[]types.Type {
	if call_name.len == 0 || isnil(t.tc) {
		return none
	}
	if t.call_param_types_decl_misses[call_name] {
		return none
	}
	t.ensure_call_param_types_decl_index()
	decl := t.call_param_types_decl_index[call_name] or {
		t.call_param_types_decl_misses[call_name] = true
		return none
	}
	if params := t.call_param_types_decl_cache[decl.idx] {
		return params
	}
	node := t.a.nodes[decl.idx]
	mut params := []types.Type{}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		mut param_type := t.parse_decl_param_type(child.typ, decl.module, decl.file)
		if param_type is types.Unknown || (param_type is types.Void && child.typ != 'void') {
			t.call_param_types_decl_misses[call_name] = true
			return none
		}
		if (child.is_mut || child.typ.starts_with('mut ')
			|| child.typ.starts_with('&')) && param_type !is types.Pointer {
			param_type = types.Type(types.Pointer{
				base_type: param_type
			})
		}
		params << param_type
	}
	t.call_param_types_decl_cache[decl.idx] = params.clone()
	return params
}

fn (mut t Transformer) ensure_call_param_types_decl_index() {
	if t.call_param_types_index_ready {
		return
	}
	t.call_param_types_decl_index.clear()
	if !isnil(t.tc) && t.tc.top_level_idx.len > 0 {
		t.call_param_types_decl_index.reserve(u32(t.tc.top_level_idx.len * 3))
	}
	mut file_name := ''
	mut module_name := ''
	use_top_level_index := !isnil(t.tc) && t.tc.top_level_idx.len > 0
		&& t.tc.top_level_idx_nodes_len == t.a.nodes.len
	if use_top_level_index {
		for i in t.tc.top_level_idx {
			node := t.a.nodes[i]
			if node.kind == .file {
				file_name = node.value
				module_name = t.tc.file_modules[file_name] or { '' }
				continue
			}
			if node.kind == .module_decl {
				module_name = node.value
				continue
			}
			if node.kind != .fn_decl {
				continue
			}
			t.add_call_param_types_decl_key(node.value, i, file_name, module_name)
			qname := transform_qualified_fn_name(module_name, node.value)
			if qname != node.value {
				t.add_call_param_types_decl_key(qname, i, file_name, module_name)
			}
		}
		t.call_param_types_index_ready = true
		return
	}
	for i, node in t.a.nodes {
		if node.kind == .file {
			file_name = node.value
			module_name = t.tc.file_modules[file_name] or { '' }
			continue
		}
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if node.kind != .fn_decl {
			continue
		}
		t.add_call_param_types_decl_key(node.value, i, file_name, module_name)
		qname := transform_qualified_fn_name(module_name, node.value)
		if qname != node.value {
			t.add_call_param_types_decl_key(qname, i, file_name, module_name)
		}
	}
	t.call_param_types_index_ready = true
}

fn (mut t Transformer) prepare_parallel_call_param_types() {
	if t.call_param_types_prepared {
		return
	}
	t.ensure_call_param_types_decl_index()
	mut seen := map[int]bool{}
	seen.reserve(u32(t.call_param_types_decl_index.len / 2 + 1))
	for name, decl in t.call_param_types_decl_index {
		if seen[decl.idx] {
			continue
		}
		seen[decl.idx] = true
		_ = t.call_param_types_from_decl(name) or { continue }
	}
	t.call_param_types_prepared = true
}

fn (mut t Transformer) add_call_param_types_decl_key(key string, idx int, file string, module_name string) {
	if key.len == 0 {
		return
	}
	// A later generic specialization can extend the declaration index between
	// parallel batches. Make the next snapshot include its signature too.
	t.call_param_types_prepared = false
	if key !in t.call_param_types_decl_index {
		t.call_param_types_decl_index[key] = FnParamDeclRef{
			idx: idx
			file: file
			module: module_name
		}
	}
	t.call_param_types_decl_misses.delete(key)
	cname := c_name(key)
	if cname != key && cname !in t.call_param_types_decl_index {
		t.call_param_types_decl_index[cname] = FnParamDeclRef{
			idx: idx
			file: file
			module: module_name
		}
	}
	t.call_param_types_decl_misses.delete(cname)
}

fn (mut t Transformer) parse_decl_param_type(typ string, module_name string, file_name string) types.Type {
	scoped := t.decl_param_type_in_module(typ, module_name)
	old_file := t.tc.cur_file
	old_module := t.tc.cur_module
	t.tc.cur_file = file_name
	t.tc.cur_module = module_name
	parsed := t.tc.parse_type(scoped)
	t.tc.cur_file = old_file
	t.tc.cur_module = old_module
	return parsed
}

fn (t &Transformer) decl_param_type_in_module(typ string, module_name string) string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.decl_param_type_in_module(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + t.decl_param_type_in_module(clean[4..], module_name)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + t.decl_param_type_in_module(clean[7..], module_name)
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + t.decl_param_type_in_module(clean[7..], module_name)
	}
	if clean.starts_with('?') {
		return '?' + t.decl_param_type_in_module(clean[1..], module_name)
	}
	if clean.starts_with('!') {
		return '!' + t.decl_param_type_in_module(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + t.decl_param_type_in_module(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + t.decl_param_type_in_module(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := t.decl_param_type_in_module(clean[4..bracket_end], module_name)
			value := t.decl_param_type_in_module(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + t.decl_param_type_in_module(clean[bracket_end + 1..], module_name)
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if scoped_fn_type := t.decl_fn_type_in_module(clean, module_name) {
			return scoped_fn_type
		}
		return clean
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		mut scoped_args := []string{}
		for arg in args {
			scoped_args << t.decl_param_type_in_module(arg, module_name)
		}
		scoped_base := t.decl_param_type_in_module(base, module_name)
		return scoped_base + '[' + scoped_args.join(', ') + ']'
	}
	if clean.contains('.') || module_name.len == 0 || module_name == 'main'
		|| module_name == 'builtin' || types.is_builtin_type_name(clean)
		|| is_generic_placeholder_type_name(clean) {
		return clean
	}
	qualified := '${module_name}.${clean}'
	if t.type_authority_has(qualified) {
		return qualified
	}
	if !isnil(t.tc) && (qualified in t.tc.type_aliases || qualified in t.tc.structs
		|| qualified in t.tc.enum_names || qualified in t.tc.sum_types
		|| qualified in t.tc.interface_names) {
		return qualified
	}
	return clean
}

fn (t &Transformer) decl_fn_type_in_module(typ string, module_name string) ?string {
	params, ret := fn_type_text_parts(typ) or { return none }
	mut scoped_params := []string{cap: params.len}
	for param in params {
		scoped_params << t.decl_fn_type_param_in_module(param, module_name)
	}
	if ret.len > 0 {
		return 'fn(${scoped_params.join(', ')}) ${t.decl_param_type_in_module(ret, module_name)}'
	}
	return 'fn(${scoped_params.join(', ')})'
}

fn (t &Transformer) decl_fn_type_param_in_module(param string, module_name string) string {
	mut text := param.trim_space()
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = text[4..].trim_space()
	}
	space := generic_top_level_space_index(text)
	if space > 0 {
		head := text[..space].trim_space()
		tail := text[space + 1..].trim_space()
		if generic_fn_type_param_head_is_name(head, tail) {
			text = tail
		}
	}
	for marker in ['[]', '&', 'map[', 'fn(', 'fn ('] {
		marker_idx := text.index(marker) or { continue }
		if marker_idx <= 0 {
			continue
		}
		head := text[..marker_idx].trim_space()
		tail := text[marker_idx..].trim_space()
		if generic_fn_type_param_head_is_name(head, tail) {
			text = tail
		}
		break
	}
	scoped := t.decl_param_type_in_module(text, module_name)
	if is_mut && scoped.len > 0 && !scoped.starts_with('&') {
		return '&' + scoped
	}
	return scoped
}

// call_is_variadic updates call is variadic state for Transformer.
fn (t &Transformer) call_is_variadic(call_name string) bool {
	key := '${t.cur_file}\n${call_name}'
	if !isnil(t.call_variadic_cache) {
		mut cache := t.call_variadic_cache
		cached := cache.get(key)
		if cached != 0 {
			return cached > 0
		}
	}
	result := t.call_is_variadic_uncached(call_name)
	if !isnil(t.call_variadic_cache) {
		mut cache := t.call_variadic_cache
		cache.put(key, if result { i8(1) } else { i8(-1) })
	}
	return result
}

fn (t &Transformer) call_is_variadic_uncached(call_name string) bool {
	if call_name.len == 0 || isnil(t.tc) {
		return false
	}
	if t.tc.c_variadic_fns[call_name] {
		return false
	}
	if is_variadic := t.tc.fn_variadic[call_name] {
		return is_variadic
	}
	// Import-aliased call names (`http.new_header` for module `net.http`)
	// register under the full module path; resolve the alias exactly first.
	if call_name.contains('.') {
		resolved_call := t.tc.resolve_imported_type_text_in_file(call_name, t.cur_file)
		if resolved_call != call_name {
			if t.tc.c_variadic_fns[resolved_call] {
				return false
			}
			if is_variadic := t.tc.fn_variadic[resolved_call] {
				return is_variadic
			}
		}
		if suffix_variadic := t.variadic_suffix_index[call_name] {
			return suffix_variadic == 1
		}
	}
	return false
}

fn (t &Transformer) fn_value_call_uses_variadic_tail(call_name string, node flat.Node, params []types.Type, param_offset int) bool {
	if params.len == 0 || param_offset != 0 || isnil(t.tc) {
		return false
	}
	if call_name.len > 0 && call_name in t.tc.fn_ret_types {
		return false
	}
	last := params[params.len - 1]
	if last !is types.Array {
		return false
	}
	explicit_args := int(node.children_count) - 1
	return explicit_args >= params.len - 1
}

// call_param_type_name updates call param type name state for Transformer.
fn (t &Transformer) call_param_type_name(call_name string, idx int) string {
	if idx < 0 || call_name.len == 0 || isnil(t.tc) {
		return ''
	}
	params := t.tc.fn_param_types[call_name] or { return '' }
	if idx >= params.len {
		return ''
	}
	return t.semantic_type_name(params[idx])
}

fn (mut t Transformer) wrap_sum_ref_arg(arg_id flat.NodeId, target_sum string) ?flat.NodeId {
	resolved_sum := t.resolve_sum_name(target_sum)
	if resolved_sum.len == 0 || resolved_sum !in t.sum_types {
		return none
	}
	arg_node := t.a.nodes[int(arg_id)]
	mut arg_type := t.node_type(arg_id)
	if arg_node.kind == .ident && arg_node.value.len > 0 {
		local_type := t.raw_var_type(arg_node.value)
		if local_type.len > 0 {
			arg_type = local_type
		}
	}
	if arg_type.len == 0 {
		return none
	}
	mut clean_arg_type := if arg_type.starts_with('&') { arg_type[1..] } else { arg_type }
	if clean_arg_type.starts_with('ptr') && clean_arg_type.len > 3
		&& clean_arg_type[3..].contains('.') {
		clean_arg_type = clean_arg_type[3..]
	}
	if !t.sum_target_accepts_variant_type(resolved_sum, clean_arg_type) {
		return none
	}
	qvariant := t.resolve_variant(resolved_sum, clean_arg_type)
	if arg_type.starts_with('&') {
		ptr := t.transform_expr_preserving_pointer_value(arg_id)
		t.set_node_typ(int(ptr), '&${qvariant}')
		return t.make_sum_ref_literal(resolved_sum, qvariant, ptr)
	}
	if t.expr_is_overloaded_index_result(arg_id) {
		value := t.transform_expr(arg_id)
		tmp_name := t.new_temp('sum_ref_arg')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, qvariant)
		ptr := t.make_prefix(.amp, t.make_ident(tmp_name))
		t.set_node_typ(int(ptr), '&${qvariant}')
		return t.make_sum_ref_literal(resolved_sum, qvariant, ptr)
	}
	if !t.expr_can_take_address(arg_id) {
		return none
	}
	value := t.transform_expr(arg_id)
	ptr := t.make_prefix(.amp, value)
	t.set_node_typ(int(ptr), '&${qvariant}')
	return t.make_sum_ref_literal(resolved_sum, qvariant, ptr)
}

fn (mut t Transformer) transform_builtin_addr_call(node flat.Node) ?flat.NodeId {
	if node.kind != .call || node.children_count != 2 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value != '__addr' {
		return none
	}
	arg_id := t.a.child(&node, 1)
	mut arg_type := t.node_type(arg_id)
	if arg_type.len == 0 {
		arg_type = t.resolve_expr_type(arg_id)
	}
	addr := t.builtin_addr_expr(arg_id, arg_type)
	if arg_type.len > 0 {
		t.set_node_typ(int(addr), '&${arg_type}')
	}
	return addr
}

fn (mut t Transformer) builtin_addr_expr(arg_id flat.NodeId, arg_type string) flat.NodeId {
	if t.expr_is_overloaded_index_result(arg_id) {
		value := t.transform_expr(arg_id)
		value_type := t.overloaded_index_result_type(arg_id) or { arg_type }
		tmp_name := t.new_temp('addr')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, value_type)
		return t.make_prefix(.amp, t.make_ident(tmp_name))
	}
	if !arg_type.starts_with('&') {
		return t.runtime_addr(arg_id, arg_type)
	}
	if addr := t.redundant_deref_addr(arg_id) {
		return addr
	}
	if !t.expr_can_take_address(arg_id) {
		stable := t.stable_transformed_expr_for_reuse(arg_id, arg_type, 'addr')
		return t.make_prefix(.amp, stable)
	}
	return t.make_prefix(.amp, arg_id)
}

fn (mut t Transformer) transform_implicit_ref_arg(arg_id flat.NodeId, param_type string) ?flat.NodeId {
	mut arg_type := t.node_type(arg_id)
	if arg_type.len == 0 {
		arg_type = t.resolve_expr_type(arg_id)
	}
	actual_depth, actual_base := pointer_type_depth_and_base(arg_type)
	expected_depth, expected_base := pointer_type_depth_and_base(param_type)
	if expected_depth <= actual_depth || expected_depth == 0 || actual_base.len == 0
		|| expected_base.len == 0 {
		return none
	}
	actual_type := t.normalize_type_alias(actual_base)
	expected_type := t.normalize_type_alias(expected_base)
	if actual_type != expected_type
		&& type_text_without_main_locks(actual_type) != type_text_without_main_locks(expected_type) {
		return none
	}
	arg_node := t.a.nodes[int(arg_id)]
	if expected_depth == actual_depth + 1 && arg_node.kind == .ident
		&& t.pointer_value_rvalues[arg_node.value] {
		storage_type := t.var_type(arg_node.value)
		if storage_type == param_type {
			value := t.transform_expr_preserving_pointer_value(arg_id)
			t.set_node_typ(int(value), storage_type)
			return value
		}
	}
	mut current := t.transform_expr(arg_id)
	mut force_materialize := t.expr_is_overloaded_index_result(arg_id)
		|| t.raw_const_type_name_for_expr(arg_id) != none
	mut current_type := arg_type
	mut current_depth := actual_depth
	for current_depth < expected_depth {
		if force_materialize {
			tmp_name := t.new_temp('ref_arg')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, current, current_type)
			current = t.make_ident(tmp_name)
			force_materialize = false
		} else if !t.expr_can_take_address(current) {
			current = t.stable_transformed_expr_for_reuse(current, current_type, 'ref_arg')
		}
		addr_type := '&${current_type}'
		addr := t.make_prefix(.amp, current)
		t.set_node_typ(int(addr), addr_type)
		current_depth++
		if current_depth == expected_depth {
			return addr
		}
		tmp_name := t.new_temp('ref_arg')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, addr, addr_type)
		current = t.make_ident(tmp_name)
		current_type = addr_type
	}
	return none
}

fn (t &Transformer) overloaded_index_result_type(id flat.NodeId) ?string {
	if isnil(t.tc) || int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.index {
			if node.children_count == 0 {
				return none
			}
			base_id := t.a.child(&node, 0)
			method_name := t.resolve_receiver_method_name(base_id, '[]')
			if method_name.len > 0 {
				if ret := t.fn_ret_types[method_name] {
					typ := t.normalize_type_alias(ret)
					if decl_type_is_usable(typ) {
						return typ
					}
				}
				if ret := t.tc.fn_ret_types[method_name] {
					typ := t.normalize_type_alias(t.semantic_type_name(ret))
					if decl_type_is_usable(typ) {
						return typ
					}
				}
			}
			base_type := t.tc.expr_type(base_id) or { t.tc.resolve_type(base_id) }
			if info := t.tc.index_overload_call_info(base_type, false) {
				typ := t.normalize_type_alias(t.semantic_type_name(info.return_type))
				if decl_type_is_usable(typ) {
					return typ
				}
			}
			return none
		}
		.paren, .expr_stmt {
			if node.children_count == 0 {
				return none
			}
			return t.overloaded_index_result_type(t.a.child(&node, 0))
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) expr_is_overloaded_index_result(id flat.NodeId) bool {
	if isnil(t.tc) || int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.index {
			if node.children_count == 0 {
				return false
			}
			base_id := t.a.child(&node, 0)
			if t.resolve_receiver_method_name(base_id, '[]').len > 0 {
				return true
			}
			base_type := t.tc.expr_type(base_id) or { t.tc.resolve_type(base_id) }
			if _ := t.tc.index_overload_call_info(base_type, false) {
				return true
			}
			return false
		}
		.paren, .expr_stmt {
			if node.children_count == 0 {
				return false
			}
			return t.expr_is_overloaded_index_result(t.a.child(&node, 0))
		}
		else {
			return false
		}
	}
}

fn pointer_type_depth_and_base(typ string) (int, string) {
	mut depth := 0
	mut clean := typ.trim_space()
	for clean.starts_with('&') {
		depth++
		clean = clean[1..].trim_space()
	}
	return depth, clean
}

// transform_call_arg_for_param transforms transform call arg for param data for transform.
@[direct_array_access]
fn (mut t Transformer) transform_call_arg_for_param(arg_id flat.NodeId, param_type string) flat.NodeId {
	// Keep prerequisites produced by earlier arguments outside this argument's
	// expression. A later block argument (notably `unsafe { nil }`) transforms its
	// statements eagerly and would otherwise drain an earlier sum-box temporary
	// into the block, after the call has already referenced it.
	outer_pending := t.pending_stmts
	t.pending_stmts = []flat.NodeId{}
	result := t.transform_call_arg_for_param_isolated(arg_id, param_type)
	arg_pending := t.pending_stmts
	t.pending_stmts = outer_pending
	for stmt in arg_pending {
		t.pending_stmts << stmt
	}
	return result
}

@[direct_array_access]
fn (mut t Transformer) transform_call_arg_for_param_isolated(arg_id flat.NodeId, param_type string) flat.NodeId {
	if int(arg_id) < 0 {
		return arg_id
	}
	mut arg_node := &t.a.nodes[int(arg_id)]
	if param_type.starts_with('&') && t.call_arg_is_zero_pointer_literal(arg_id) {
		value := t.transform_expr(arg_id)
		t.set_node_typ(int(value), param_type)
		return value
	}
	if t.in_spawn_expr && t.call_arg_has_shared_marker(arg_id) {
		return t.transform_expr(arg_id)
	}
	if param_type.starts_with('&')
		&& ((arg_node.kind == .int_literal && (arg_node.value == '0' || arg_node.value.len == 0))
			|| arg_node.kind == .nil_literal) {
		// A zero/nil pointer argument is already the C null pointer value. It is
		// not an addressable value to borrow (for example `C.wait(0)`).
		return t.transform_expr(arg_id)
	}
	if param_type.starts_with('&') && arg_node.kind == .or_expr && arg_node.value == '?'
		&& arg_node.children_count > 0 {
		source_id := t.a.child(arg_node, 0)
		source_type := t.node_type(source_id)
		if t.is_optional_type_name(source_type) {
			payload_type := t.optional_base_type(source_type)
			// Evaluate the optional once, keeping it addressable so mutations
			// through the reference still write back to the payload (a stable
			// ident/selector is reused directly, a call is materialized once).
			opt_expr := t.stable_transformed_expr_for_reuse(t.transform_expr(source_id), source_type, 'opt_ref')
			if t.is_optional_type_name(t.cur_fn_ret_type) {
				// `opt?` propagates: return none/err when it is none, instead of
				// unconditionally treating it as present.
				not_ok := t.make_prefix(.not, t.make_selector(opt_expr, 'ok', 'bool'))
				err_expr := t.make_selector(opt_expr, 'err', 'IError')
				else_stmts := t.lower_or_body_to_stmts_with_err_expr(flat.empty_node, '', payload_type, '?', err_expr)
				t.pending_stmts << t.make_if(not_ok, t.make_block_skip_scope_drops(else_stmts), t.make_empty())
			} else {
				// Comptime option-payload-mut (e.g. `decode(mut result.field?)` in a
				// `$for` decoder): the callee fills the payload, so mark it present and
				// expose its address. Normal `?` propagation in a non-option-returning
				// function would have been rejected by the checker, so this is the only
				// way we get here without an option return type.
				ok_target := t.make_selector(opt_expr, 'ok', 'bool')
				t.pending_stmts << t.make_assign(ok_target, t.make_bool_literal(true))
			}
			payload := t.make_selector(opt_expr, 'value', payload_type)
			addr := t.make_prefix(.amp, payload)
			t.set_node_typ(int(addr), param_type)
			return addr
		}
	}
	if arg_node.kind == .lambda_expr {
		if lifted := t.lift_lambda_expr_for_fn_param(arg_id, *arg_node, param_type) {
			return lifted
		}
	}
	if arg_node.kind == .fn_literal {
		if lifted := t.lift_fn_literal_for_fn_param(arg_id, *arg_node, param_type) {
			return lifted
		}
	}
	if arg_node.kind == .array_literal && arg_node.typ.len == 0 && param_type.starts_with('[]') {
		t.set_node_typ(int(arg_id), param_type)
	}
	if param_type.starts_with('&[]') {
		arg_type := t.node_type(arg_id)
		fixed_type := if arg_type.starts_with('&') { arg_type[1..] } else { arg_type }
		if t.is_fixed_array_type(fixed_type) {
			array_type := param_type[1..]
			array_value := t.fixed_array_value_to_array_no_alloc(arg_id, fixed_type, array_type)
			tmp_name := t.new_temp('fixed_array_arg')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, array_value, array_type)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.set_node_typ(int(addr), param_type)
			return addr
		}
	}
	if transform_param_type_is_void_pointer(param_type)
		&& t.call_arg_is_fn_pointer_value(arg_id, *arg_node) {
		return t.transform_expr(arg_id)
	}
	if transform_param_type_is_void_pointer(param_type) {
		// C APIs commonly spell a null callback/data pointer as the integer
		// literal `0`.  It is already a pointer value in that context; taking
		// the address of a temporary integer instead produces a non-null,
		// dangling pointer.
		if t.expr_is_nil_like(arg_id) || (arg_node.kind == .int_literal && arg_node.value == '0') {
			return t.make_cast(param_type, t.transform_expr(arg_id), param_type)
		}
		// A `c'...'` literal denotes a C string pointer even though its flat
		// literal node retains the character type used for byte contexts.
		if arg_node.kind == .char_literal && arg_node.value.starts_with('c:') {
			return t.transform_expr(arg_id)
		}
		arg_type := t.node_type(arg_id)
		clean_arg_type := t.normalize_type_alias(arg_type)
		if clean_arg_type.len > 0 && !clean_arg_type.starts_with('&')
			&& clean_arg_type !in ['voidptr', 'byteptr', 'charptr', 'nil'] {
			value := t.transform_expr(arg_id)
			mut addr := flat.empty_node
			if t.raw_const_type_name_for_expr(arg_id) == none && t.expr_can_take_address(value) {
				addr = t.make_prefix(.amp, value)
			} else {
				tmp_name := t.new_temp('voidptr_arg')
				t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, clean_arg_type)
				addr = t.make_prefix(.amp, t.make_ident(tmp_name))
			}
			return t.make_cast(param_type, addr, param_type)
		}
	}
	if arg_node.kind == .enum_val && param_type in t.enum_types {
		return t.transform_enum_shorthand(arg_id, *arg_node, param_type)
	}
	if param_type.starts_with('&') && arg_node.kind == .prefix && arg_node.op == .amp
		&& arg_node.children_count > 0 {
		child_id := t.a.child(arg_node, 0)
		child := t.a.nodes[int(child_id)]
		if child.kind == .ident && t.has_smartcast(child.value) {
			sc := t.find_smartcast(child.value) or { SmartcastContext{} }
			target := t.smartcast_target_type(sc)
			if target.len > 0
				&& t.normalize_type_alias(t.trim_pointer_type(target)) == t.normalize_type_alias(param_type[1..]) {
				if narrowed := t.smartcast_ident_value(child.value) {
					narrowed_node := t.a.nodes[int(narrowed)]
					if narrowed_node.kind == .prefix && narrowed_node.op == .mul
						&& narrowed_node.children_count > 0 {
						pointer := t.a.child(&narrowed_node, 0)
						t.set_node_typ(int(pointer), param_type)
						return pointer
					}
				}
			}
		}
	}
	if param_type.starts_with('&') && arg_node.kind == .ident && arg_node.is_mut
		&& t.mut_param_values[arg_node.value] {
		storage_type := t.var_type(arg_node.value)
		if storage_type.starts_with('&')
			&& t.normalize_type_alias(storage_type) == t.normalize_type_alias(param_type) {
			// Forwarding `mut val` from a `mut T` parameter reuses the parameter's
			// existing pointer-backed storage. Taking another address would pass `T**`.
			storage := t.make_ident(arg_node.value)
			t.set_node_typ(int(storage), storage_type)
			return storage
		}
	}
	if param_type.starts_with('&') && arg_node.kind == .selector
		&& t.selector_chain_has_sum_variant_field(arg_id) {
		value := t.transform_expr(arg_id)
		mut value_type := t.node_type(arg_id)
		if value_type.len == 0 {
			value_type = t.node_type(value)
		}
		stable := t.stable_transformed_expr_for_reuse(value, value_type, 'addr')
		addr := t.make_prefix(.amp, stable)
		t.set_node_typ(int(addr), param_type)
		return addr
	}
	if param_type.starts_with('&') && t.is_sum_type_name(param_type[1..]) {
		target_sum := param_type[1..]
		resolved_target_sum := t.resolve_sum_name(target_sum)
		arg_type := t.node_type(arg_id)
		arg_key := t.expr_key(arg_id)
		has_smartcast := t.has_smartcast(arg_key)
		if has_smartcast {
			raw_arg_type := t.raw_expr_type_without_smartcast(arg_id)
			if t.resolve_sum_name(t.trim_pointer_type(raw_arg_type)) == resolved_target_sum {
				return t.make_plain_expr_for_smartcast(arg_id)
			}
		}
		if !has_smartcast
			&& t.resolve_sum_name(t.trim_pointer_type(arg_type)) == resolved_target_sum {
			return t.transform_expr(arg_id)
		}
		if arg_node.kind == .prefix && arg_node.op == .amp && arg_node.children_count > 0 {
			inner_id := t.a.child(arg_node, 0)
			inner_key := t.expr_key(inner_id)
			inner_has_smartcast := t.has_smartcast(inner_key)
			if inner_has_smartcast {
				raw_inner_type := t.raw_expr_type_without_smartcast(inner_id)
				if t.resolve_sum_name(t.trim_pointer_type(raw_inner_type)) == resolved_target_sum {
					inner := t.make_plain_expr_for_smartcast(inner_id)
					addr := t.make_prefix(.amp, inner)
					t.set_node_typ(int(addr), param_type)
					return addr
				}
			}
			inner_type := t.node_type(inner_id)
			if t.resolve_sum_name(t.trim_pointer_type(inner_type)) == resolved_target_sum {
				return t.transform_expr(arg_id)
			}
		}
		wrapped := t.wrap_sum_ref_arg(arg_id, target_sum) or {
			t.wrap_sum_value(arg_id, target_sum)
		}
		tmp_name := t.new_temp('sum_arg')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, target_sum)
		return t.make_prefix(.amp, t.make_ident(tmp_name))
	}
	if param_type.starts_with('&') && arg_node.kind == .prefix && arg_node.op == .amp
		&& arg_node.children_count > 0 {
		child_id := t.a.child(arg_node, 0)
		child_type := t.node_type(child_id)
		if child_type.len > 0
			&& t.normalize_type_alias(child_type) == t.normalize_type_alias(param_type[1..]) {
			return t.transform_expr(arg_id)
		}
	}
	if param_type.starts_with('&') && !t.is_sum_type_name(param_type[1..])
		&& !t.is_interface_type(param_type) {
		if arg_node.kind == .ident && t.pointer_value_lvalues[arg_node.value] {
			arg_type := t.node_type(arg_id)
			if arg_type == param_type {
				value := t.transform_expr_preserving_pointer_value(arg_id)
				if t.has_smartcast(arg_node.value) {
					value_node := t.a.nodes[int(value)]
					if value_node.kind == .prefix && value_node.op == .mul
						&& value_node.children_count > 0 {
						pointer := t.a.child(&value_node, 0)
						t.set_node_typ(int(pointer), param_type)
						return pointer
					}
				}
				t.set_node_typ(int(value), param_type)
				return value
			}
			if arg_type.starts_with('&')
				&& t.normalize_type_alias(arg_type[1..]) == t.normalize_type_alias(param_type) {
				value := t.transform_expr_preserving_pointer_value(arg_id)
				storage_type := t.var_type(arg_node.value)
				if storage_type == param_type {
					t.set_node_typ(int(value), storage_type)
					return value
				}
				deref := t.make_prefix(.mul, value)
				t.set_node_typ(int(deref), param_type)
				return deref
			}
		}
		if implicit_ref := t.transform_implicit_ref_arg(arg_id, param_type) {
			return implicit_ref
		}
		arg_type := t.node_type(arg_id)
		if arg_type.len > 0 && !arg_type.starts_with('&')
			&& t.normalize_type_alias(arg_type) == t.normalize_type_alias(param_type[1..])
			&& t.expr_can_take_address(arg_id) && !t.expr_is_overloaded_index_result(arg_id) {
			value := t.transform_expr(arg_id)
			addr := t.make_prefix(.amp, value)
			t.set_node_typ(int(addr), param_type)
			return addr
		}
	}
	if param_type.starts_with('&') && arg_node.kind == .ident
		&& t.pointer_global_arg_matches_param(arg_node.value, param_type) {
		return t.transform_expr(arg_id)
	}
	if !t.in_spawn_expr {
		if ptr_arg := t.transform_pointer_rvalue_arg(arg_id, *arg_node, param_type) {
			return ptr_arg
		}
	}
	if !param_type.starts_with('&') && t.is_sum_type_name(param_type) {
		arg_key := t.expr_key(arg_id)
		if t.has_smartcast(arg_key) {
			raw_arg_type := t.raw_expr_type_without_smartcast(arg_id)
			if t.resolve_sum_name(t.trim_pointer_type(raw_arg_type)) == t.resolve_sum_name(param_type) {
				value := t.make_plain_expr_for_smartcast(arg_id)
				return t.clone_borrowed_projection(arg_id, value, param_type)
			}
		}
		value := t.wrap_sum_value(arg_id, param_type)
		return t.clone_borrowed_projection(arg_id, value, param_type)
	}
	if param_type.starts_with('[]') {
		arg_type := t.node_type(arg_id)
		if converted := t.transform_array_value_for_dynamic_target(arg_id, param_type) {
			return converted
		}
		if t.is_fixed_array_type(arg_type) {
			return t.fixed_array_value_to_owned_array(arg_id, arg_type, param_type)
		}
		if const_arg := t.transform_const_array_arg_for_param(arg_id, param_type) {
			return const_arg
		}
	}
	if param_type.len > 0 && (param_type.contains('unknown')
		|| t.type_text_has_generic_placeholder(param_type, t.cur_module)) {
		return t.transform_expr(arg_id)
	}
	if param_type.starts_with('&') && t.is_interface_type(param_type) {
		// Explicit `mut` arguments must alias the caller's storage. Other value
		// arguments are copied because the callee may retain the `&Interface`
		// after this call (for example `log.set_logger(local_logger)`).
		if boxed := t.transform_interface_value_for_type(arg_id, param_type, arg_node.is_mut) {
			return boxed
		}
	}
	value := t.transform_expr_for_type(arg_id, param_type)
	if param_type.starts_with('&') {
		return value
	}
	// A by-value argument copies a field or slice read the caller keeps owning, so
	// clone it into an independent value the callee owns (see clone_borrowed_projection).
	return t.clone_borrowed_projection(arg_id, value, param_type)
}

fn (t &Transformer) call_arg_is_zero_pointer_literal(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.call_arg_is_zero_pointer_literal(t.a.child(&node, 0))
	}
	return node.kind == .int_literal && (node.value.len == 0 || node.value == '0')
}

fn (t &Transformer) call_arg_has_shared_marker(arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .paren && arg.children_count > 0 {
		return t.call_arg_has_shared_marker(t.a.child(&arg, 0))
	}
	return arg.kind == .prefix && (arg.value == 'shared' || arg.value.starts_with('shared:'))
		&& arg.children_count > 0
}

fn (t &Transformer) pointer_global_arg_matches_param(name string, param_type string) bool {
	arg_type := t.global_ident_type(name) or { return false }
	if !arg_type.starts_with('&') {
		return false
	}
	param_base := t.normalize_type_alias(param_type[1..])
	arg_base := t.normalize_type_alias(arg_type[1..])
	return param_base == arg_base || short_name_view(param_base) == short_name_view(arg_base)
}

fn (t &Transformer) global_ident_type(name string) ?string {
	if typ := t.current_module_global_type(name) {
		return t.normalize_type_alias(typ)
	}
	return none
}

fn (t &Transformer) current_module_global_type(name string) ?string {
	if name.len == 0 {
		return none
	}
	if name.contains('.') {
		return t.globals[name]
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		return t.globals['${t.cur_module}.${name}']
	}
	return t.globals[name]
}

fn (mut t Transformer) lift_lambda_expr_for_fn_param(_id flat.NodeId, node flat.Node, param_type string) ?flat.NodeId {
	if node.kind != .lambda_expr || node.children_count == 0 || param_type.len == 0 || isnil(t.tc) {
		return none
	}
	fn_type := t.fn_type_from_type_text(param_type) or { return none }
	body_id := t.a.child(&node, node.children_count - 1)
	lambda_param_count := int(node.children_count) - 1
	if lambda_param_count > fn_type.params.len {
		return none
	}
	mut lambda_params := map[string]bool{}
	for i in 0 .. lambda_param_count {
		param_id := t.a.child(&node, i)
		param_node := t.a.nodes[int(param_id)]
		if param_node.value.len > 0 {
			lambda_params[param_node.value] = true
		}
	}
	capture_ids := t.lambda_capture_ids(body_id, lambda_params)
	mut children := []flat.NodeId{cap: capture_ids.len + fn_type.params.len + 1}
	for capture_id in capture_ids {
		children << capture_id
	}
	for i in 0 .. lambda_param_count {
		param_id := t.a.child(&node, i)
		param_node := t.a.nodes[int(param_id)]
		param_type_name := t.semantic_type_name(fn_type.params[i])
		children << t.a.add_node(flat.Node{
			kind: .param
			value: param_node.value
			typ: param_type_name
			op: if param_type_name.starts_with('&') { .amp } else { .none }
		})
	}
	for i in lambda_param_count .. fn_type.params.len {
		param_type_name := t.semantic_type_name(fn_type.params[i])
		children << t.a.add_node(flat.Node{
			kind: .param
			value: '_unused_${i}'
			typ: param_type_name
			op: if param_type_name.starts_with('&') { .amp } else { .none }
		})
	}
	ret_type := t.semantic_type_name(fn_type.return_type)
	if t.is_optional_type_name(ret_type) && t.optional_base_type(ret_type) == 'void' {
		body_type := t.qualify_optional_type(t.node_type(body_id))
		ret_type0 := t.qualify_optional_type(ret_type)
		body_node := t.a.nodes[int(body_id)]
		if t.optional_types_match(ret_type0, body_type) {
			children << t.make_direct_optional_forward_return(body_id, ret_type0)
		} else if ret_type0.starts_with('!') && t.is_error_call(body_node) {
			children << t.make_return(body_id, ret_type0)
		} else {
			children << t.make_expr_stmt(body_id)
			children << t.make_return(t.make_optional_some(t.make_empty(), ret_type0), ret_type0)
		}
	} else if ret_type.len > 0 && ret_type != 'void' {
		children << t.make_return(body_id, ret_type)
	} else {
		body := t.a.nodes[int(body_id)]
		if body.kind == .block {
			children << t.a.children_of(&body)
		} else {
			children << t.make_expr_stmt(body_id)
		}
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	fn_id := t.a.add_node(flat.Node{
		kind: .fn_literal
		typ: ret_type
		children_start: start
		children_count: flat.child_count(children.len)
		pos: node.pos
	})
	return t.lift_fn_literal(fn_id, t.a.nodes[int(fn_id)])
}

fn (mut t Transformer) lift_fn_literal_for_fn_param(_id flat.NodeId, node flat.Node, param_type string) ?flat.NodeId {
	if node.kind != .fn_literal || param_type.len == 0 || isnil(t.tc) {
		return none
	}
	fn_type := t.fn_type_from_type_text(param_type) or { return none }
	mut capture_ids := []flat.NodeId{}
	mut param_ids := []flat.NodeId{}
	mut body_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .param {
			param_ids << child_id
		} else if child.kind == .ident {
			capture_ids << child_id
		} else {
			body_ids << child_id
		}
	}
	if param_ids.len >= fn_type.params.len {
		return none
	}
	mut children := []flat.NodeId{cap: capture_ids.len + fn_type.params.len + body_ids.len}
	children << capture_ids
	for i, param_id in param_ids {
		param_type_name := t.semantic_type_name(fn_type.params[i])
		param := t.a.nodes[int(param_id)]
		if param.typ == param_type_name {
			children << param_id
		} else {
			children << t.a.add_node(flat.Node{
				kind: .param
				value: param.value
				typ: param_type_name
				op: if param_type_name.starts_with('&') { .amp } else { param.op }
				is_mut: param.is_mut
			})
		}
	}
	for i in param_ids.len .. fn_type.params.len {
		param_type_name := t.semantic_type_name(fn_type.params[i])
		children << t.a.add_node(flat.Node{
			kind: .param
			value: '_unused_${i}'
			typ: param_type_name
			op: if param_type_name.starts_with('&') { .amp } else { .none }
		})
	}
	children << body_ids
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	ret_type := if node.typ.len > 0 && node.typ != 'void' {
		node.typ
	} else {
		t.semantic_type_name(fn_type.return_type)
	}
	fn_id := t.a.add_node(flat.Node{
		kind: .fn_literal
		typ: ret_type
		children_start: start
		children_count: flat.child_count(children.len)
		pos: node.pos
	})
	return t.lift_fn_literal(fn_id, t.a.nodes[int(fn_id)])
}

fn (mut t Transformer) lambda_capture_ids(body_id flat.NodeId, params map[string]bool) []flat.NodeId {
	mut names := map[string]flat.NodeId{}
	t.collect_lambda_capture_names(body_id, params, mut names)
	mut sorted_names := names.keys()
	sorted_names.sort()
	mut ids := []flat.NodeId{cap: sorted_names.len}
	for name in sorted_names {
		ids << names[name]
	}
	return ids
}

fn (mut t Transformer) collect_lambda_capture_names(id flat.NodeId, locals map[string]bool, mut names map[string]flat.NodeId) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			if node.value.len > 0 && node.value !in locals && t.var_type(node.value).len > 0 {
				names[node.value] = id
			}
			return
		}
		.block {
			t.collect_lambda_capture_sequence(node, 0, locals, mut names)
			return
		}
		.decl_assign {
			mut local_scope := locals.clone()
			t.collect_lambda_decl_assign_captures(node, mut local_scope, mut names)
			return
		}
		.for_in_stmt {
			t.collect_lambda_for_in_captures(node, locals, mut names)
			return
		}
		.for_stmt {
			t.collect_lambda_for_captures(node, locals, mut names)
			return
		}
		.if_expr {
			t.collect_lambda_if_captures(node, locals, mut names)
			return
		}
		.match_stmt {
			t.collect_lambda_match_captures(node, locals, mut names)
			return
		}
		.fn_literal, .lambda_expr {
			return
		}
		.call {
			if node.children_count > 0 {
				callee_id := t.a.child(&node, 0)
				t.collect_lambda_capture_names(callee_id, locals, mut names)
			}
			for i in 1 .. node.children_count {
				t.collect_lambda_capture_names(t.a.child(&node, i), locals, mut names)
			}
			return
		}
		.selector {
			if node.children_count > 0 {
				t.collect_lambda_capture_names(t.a.child(&node, 0), locals, mut names)
			}
			return
		}
		else {}
	}

	for i in 0 .. node.children_count {
		t.collect_lambda_capture_names(t.a.child(&node, i), locals, mut names)
	}
}

fn (mut t Transformer) collect_lambda_capture_sequence(node flat.Node, start int, locals map[string]bool, mut names map[string]flat.NodeId) {
	mut local_scope := locals.clone()
	for i in start .. node.children_count {
		t.collect_lambda_capture_stmt(t.a.child(&node, i), mut local_scope, mut names)
	}
}

fn (mut t Transformer) collect_lambda_capture_stmt(id flat.NodeId, mut locals map[string]bool, mut names map[string]flat.NodeId) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.decl_assign {
			t.collect_lambda_decl_assign_captures(node, mut locals, mut names)
		}
		.for_in_stmt {
			t.collect_lambda_for_in_captures(node, locals, mut names)
		}
		.for_stmt {
			t.collect_lambda_for_captures(node, locals, mut names)
		}
		else {
			t.collect_lambda_capture_names(id, locals, mut names)
		}
	}
}

fn (mut t Transformer) collect_lambda_decl_assign_captures(node flat.Node, mut locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	if node.children_count >= 3 && !isnil(t.tc) {
		rhs_id := t.a.child(&node, 1)
		if _ := t.multi_return_types_for_expr(rhs_id, node.children_count - 1) {
			t.collect_lambda_capture_names(rhs_id, locals, mut names)
			t.note_lambda_local_binding(t.a.child(&node, 0), mut locals)
			for i in 2 .. node.children_count {
				t.note_lambda_local_binding(t.a.child(&node, i), mut locals)
			}
			return
		}
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := t.a.child(&node, i)
		rhs_id := t.a.child(&node, i + 1)
		t.collect_lambda_capture_names(rhs_id, locals, mut names)
		t.note_lambda_local_binding(lhs_id, mut locals)
		i += 2
	}
}

fn (mut t Transformer) collect_lambda_for_in_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count < 3 {
		return
	}
	header := node.value.int()
	container_id := t.a.child(&node, 2)
	t.collect_lambda_capture_names(container_id, locals, mut names)
	if header > 3 && node.children_count > 3 {
		t.collect_lambda_capture_names(t.a.child(&node, 3), locals, mut names)
	}
	mut loop_scope := locals.clone()
	t.note_lambda_local_binding(t.a.child(&node, 0), mut loop_scope)
	t.note_lambda_local_binding(t.a.child(&node, 1), mut loop_scope)
	for i in header .. node.children_count {
		t.collect_lambda_capture_stmt(t.a.child(&node, i), mut loop_scope, mut names)
	}
}

fn (mut t Transformer) collect_lambda_for_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	mut loop_scope := locals.clone()
	if node.children_count > 0 {
		init_id := t.a.child(&node, 0)
		if t.valid_node_id(init_id) {
			init := t.a.nodes[int(init_id)]
			if init.kind == .decl_assign {
				t.collect_lambda_decl_assign_captures(init, mut loop_scope, mut names)
			} else {
				t.collect_lambda_capture_names(init_id, loop_scope, mut names)
			}
		}
	}
	if node.children_count > 1 {
		t.collect_lambda_capture_names(t.a.child(&node, 1), loop_scope, mut names)
	}
	if node.children_count > 2 {
		t.collect_lambda_capture_names(t.a.child(&node, 2), loop_scope, mut names)
	}
	for i in 3 .. node.children_count {
		t.collect_lambda_capture_stmt(t.a.child(&node, i), mut loop_scope, mut names)
	}
}

fn (mut t Transformer) collect_lambda_if_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	cond_id := t.a.child(&node, 0)
	mut then_scope := locals.clone()
	t.collect_lambda_condition_captures(cond_id, locals, mut then_scope, mut names)
	if node.children_count > 1 {
		t.collect_lambda_capture_names(t.a.child(&node, 1), then_scope, mut names)
	}
	if node.children_count > 2 {
		t.collect_lambda_capture_names(t.a.child(&node, 2), locals, mut names)
	}
}

fn (mut t Transformer) collect_lambda_condition_captures(id flat.NodeId, locals map[string]bool, mut then_scope map[string]bool, mut names map[string]flat.NodeId) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .decl_assign {
		mut cond_scope := locals.clone()
		t.collect_lambda_decl_assign_captures(node, mut cond_scope, mut names)
		for name, _ in cond_scope {
			if name !in locals {
				then_scope[name] = true
			}
		}
		return
	}
	if node.kind == .infix && node.op == .logical_and && node.children_count >= 2 {
		mut lhs_scope := locals.clone()
		t.collect_lambda_condition_captures(t.a.child(&node, 0), locals, mut lhs_scope, mut names)
		for name, _ in lhs_scope {
			if name !in locals {
				then_scope[name] = true
			}
		}
		t.collect_lambda_condition_captures(t.a.child(&node, 1), lhs_scope, mut then_scope, mut names)
		return
	}
	t.collect_lambda_capture_names(id, locals, mut names)
}

fn (mut t Transformer) collect_lambda_match_captures(node flat.Node, locals map[string]bool, mut names map[string]flat.NodeId) {
	if node.children_count == 0 {
		return
	}
	t.collect_lambda_capture_names(t.a.child(&node, 0), locals, mut names)
	for i in 1 .. node.children_count {
		branch_id := t.a.child(&node, i)
		if !t.valid_node_id(branch_id) {
			continue
		}
		branch := t.a.nodes[int(branch_id)]
		if branch.kind != .match_branch {
			t.collect_lambda_capture_names(branch_id, locals, mut names)
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { branch.value.int() }
		for j in 0 .. body_start {
			t.collect_lambda_capture_names(t.a.child(&branch, j), locals, mut names)
		}
		mut branch_scope := locals.clone()
		for j in body_start .. branch.children_count {
			t.collect_lambda_capture_stmt(t.a.child(&branch, j), mut branch_scope, mut names)
		}
	}
}

fn (mut t Transformer) note_lambda_local_binding(id flat.NodeId, mut locals map[string]bool) {
	if !t.valid_node_id(id) {
		return
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value.len > 0 && node.value != '_' {
		locals[node.value] = true
	}
}

fn (t &Transformer) valid_node_id(id flat.NodeId) bool {
	return int(id) >= 0 && int(id) < t.a.nodes.len
}

fn (t &Transformer) fn_type_from_type_text(type_text string) ?types.FnType {
	typ := t.tc.parse_type(type_text)
	return fn_type_from_type(typ)
}

fn fn_type_from_type(typ types.Type) ?types.FnType {
	if typ is types.FnType {
		return typ
	}
	if typ is types.Alias {
		return fn_type_from_type(typ.base_type)
	}
	return none
}

fn (mut t Transformer) transform_pointer_rvalue_arg(arg_id flat.NodeId, arg_node flat.Node, param_type string) ?flat.NodeId {
	if !param_type.starts_with('&') {
		return none
	}
	mut value_id := arg_id
	mut value_node := arg_node
	if arg_node.kind == .prefix && arg_node.op == .amp && arg_node.children_count > 0 {
		value_id = t.a.child(&arg_node, 0)
		value_node = t.a.nodes[int(value_id)]
	}
	if !is_pointer_arg_temp_rvalue(value_node) {
		return none
	}
	value_type := param_type[1..]
	if value_type.len == 0 || value_type == 'void' || value_type == 'unknown' {
		return none
	}
	if t.c_type_nil_call_for_type(value_node, value_type) {
		nil_id := t.a.add(.nil_literal)
		t.set_node_typ(int(nil_id), param_type)
		return nil_id
	}
	arg_type := t.node_type(value_id)
	if arg_type.len == 0 || arg_type == 'void' || arg_type == 'unknown'
		|| is_pointer_like_type_name(arg_type) {
		return none
	}
	value := t.transform_expr_for_type(value_id, value_type)
	tmp_name := t.new_temp('ptr_arg')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, value_type)
	addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.set_node_typ(int(addr), param_type)
	return addr
}

fn (t &Transformer) c_type_nil_call_for_type(node flat.Node, value_type string) bool {
	if node.kind != .call || node.children_count != 2 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	arg := t.a.child_node(&node, 1)
	if arg.kind != .nil_literal {
		return false
	}
	short_type := value_type.all_after_last('.')
	if callee.kind == .ident {
		return callee.value == value_type || callee.value == short_type
	}
	if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		return base.kind == .ident && callee.value.len > 0
			&& ((base.value == 'C' && (value_type.starts_with('C.') || callee.value == short_type))
				|| '${base.value}.${callee.value}' == value_type)
	}
	return false
}

fn is_pointer_arg_temp_rvalue(node flat.Node) bool {
	return node.kind == .call || (node.kind == .index && node.value == 'range')
}

fn is_pointer_like_type_name(typ string) bool {
	mut clean := typ
	if clean.starts_with('mut ') {
		clean = clean[4..]
	}
	return clean.starts_with('&') || clean in ['voidptr', 'byteptr', 'charptr']
}

fn (mut t Transformer) append_missing_params_struct_args(mut args []flat.NodeId, params []types.Type, param_offset int) {
	mut param_idx := param_offset
	if args.len > 0 {
		param_idx = args.len - 1 + param_offset
	}
	for param_idx < params.len {
		param_type := t.semantic_type_name(params[param_idx])
		if struct_type := t.params_struct_type_name(param_type) {
			args << t.zero_value_for_type(struct_type)
			t.mark_params_struct_default_calls(struct_type)
		} else if params[param_idx] is types.OptionType {
			args << t.a.add_node(flat.Node{
				kind: .none_expr
				typ: param_type
			})
		} else {
			break
		}
		param_idx++
	}
}

// mark_params_struct_default_calls marks the functions called by the field
// defaults of a `@[params]` struct whose default value is being synthesized for an
// omitted argument. cgen fills those field defaults directly from the struct decl,
// so without this the default-initializer functions (e.g. `default_c_level()` for
// `compression_level int = default_c_level()`) stay unmarked and undefined at link.
fn (mut t Transformer) mark_params_struct_default_calls(struct_type string) {
	info := t.lookup_struct_info(struct_type) or { return }
	// The field defaults live in the struct's declaring module, so qualify the
	// discovered calls against that module (not the module currently being
	// transformed) or the marked name won't match the emitted symbol.
	old_module := t.cur_module
	old_file := t.cur_file
	t.cur_module = info.module
	t.cur_file = ''
	defer {
		t.cur_module = old_module
		t.cur_file = old_file
	}
	for field in info.fields {
		if int(field.default_expr) < 0 {
			continue
		}
		for call_name in t.generated_fn_body_call_names(field.default_expr) {
			t.mark_fn_used_name(call_name)
		}
	}
}

// is_fn_pointer_type_name reports whether is fn pointer type name applies in transform.
fn (t &Transformer) is_fn_pointer_type_name(type_name string) bool {
	if type_name.len == 0 || isnil(t.tc) {
		return false
	}
	typ := t.tc.parse_type(type_name)
	if typ is types.FnType {
		return true
	}
	if typ is types.Alias {
		return typ.base_type is types.FnType
	}
	return false
}

fn transform_param_type_is_void_pointer(type_name string) bool {
	if type_name.len < 5 {
		return false
	}
	// Fast path: nothing to trim (the common spelling), so compare directly
	// instead of allocating a trimmed copy per call.
	if type_name[0] > ` ` && type_name[type_name.len - 1] > ` ` {
		if type_name.len > 7 {
			return false
		}
		return type_name == '&void' || type_name == 'voidptr' || type_name == 'byteptr'
			|| type_name == 'charptr'
	}
	clean := type_name.trim_space()
	return clean == '&void' || clean == 'voidptr' || clean == 'byteptr' || clean == 'charptr'
}

fn (t &Transformer) call_arg_is_fn_pointer_value(arg_id flat.NodeId, arg_node flat.Node) bool {
	for type_name in [arg_node.typ, t.node_type(arg_id), t.resolve_expr_type(arg_id)] {
		if t.is_fn_pointer_type_name(type_name) {
			return true
		}
	}
	if arg_node.kind == .ident {
		type_name := t.var_type(arg_node.value)
		if t.is_fn_pointer_type_name(type_name) {
			return true
		}
	}
	if !isnil(t.tc) {
		typ := t.tc.resolve_type(arg_id)
		if _ := fn_type_from_type(typ) {
			return true
		}
	}
	return false
}

// is_named_fn_value_arg reports whether is named fn value arg applies in transform.
fn (t &Transformer) is_named_fn_value_arg(arg_id flat.NodeId) bool {
	if int(arg_id) < 0 || isnil(t.tc) {
		return false
	}
	node := t.a.nodes[int(arg_id)]
	if node.kind == .ident {
		if _ := t.resolve_fn_value_ident(node.value) {
			return true
		}
		return false
	}
	if node.kind == .selector && node.children_count > 0 {
		key := t.expr_key(arg_id)
		return key in t.tc.fn_ret_types || key in t.tc.fn_param_types
	}
	return false
}

// transform_const_array_arg_for_param supports transform_const_array_arg_for_param handling.
fn (mut t Transformer) transform_const_array_arg_for_param(arg_id flat.NodeId, param_type string) ?flat.NodeId {
	if raw_const_type := t.raw_const_type_name_for_expr(arg_id) {
		if t.normalize_type_alias(raw_const_type) == t.normalize_type_alias(param_type) {
			value := t.transform_expr(arg_id)
			return t.clone_borrowed_projection(arg_id, value, param_type)
		}
	}
	expr_id := t.const_expr_for_arg(arg_id) or { return none }
	expr := t.a.nodes[int(expr_id)]
	elem_type := param_type[2..]
	if expr.kind == .array_init && expr.children_count == 0 {
		return t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(0))
	}
	if expr.kind != .array_literal {
		return none
	}
	if expr.children_count == 0 {
		return t.make_array_new_call(elem_type, t.make_int_literal(0), t.make_int_literal(0))
	}
	mut values := []flat.NodeId{cap: int(expr.children_count)}
	for i in 0 .. expr.children_count {
		values << t.transform_expr(t.a.child(&expr, i))
	}
	return t.make_array_literal_typed(values, param_type)
}

// const_expr_for_arg supports const expr for arg handling for Transformer.
fn (t &Transformer) const_expr_for_arg(arg_id flat.NodeId) ?flat.NodeId {
	if isnil(t.tc) || int(arg_id) < 0 {
		return none
	}
	node := t.a.nodes[int(arg_id)]
	if node.kind == .ident {
		if t.var_type(node.value).len > 0 {
			return none
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			if expr_id := t.const_expr_for_name('${t.cur_module}.${node.value}') {
				return expr_id
			}
		}
		return t.const_expr_for_name(node.value)
	}
	if node.kind == .selector && node.children_count > 0 {
		if !t.selector_const_base_is_value(node) {
			base := t.a.child_node(&node, 0)
			return t.const_expr_for_name_in_context('${base.value}.${node.value}', t.cur_module, t.cur_file)
		}
	}
	return none
}

fn (t &Transformer) selector_const_base_is_value(node flat.Node) bool {
	if node.kind != .selector || node.children_count == 0 || isnil(t.tc) {
		return false
	}
	base_id := t.a.child(&node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := t.a.nodes[int(base_id)]
	if base.kind != .ident {
		return false
	}
	if t.is_import_alias_ident(base_id) {
		return false
	}
	if t.var_type(base.value).len > 0 {
		return true
	}
	if typ := t.tc.expr_type(base_id) {
		name := t.semantic_type_name(typ)
		return name.len > 0 && name !in ['void', 'unknown']
	}
	resolved := t.tc.resolve_type(base_id)
	name := t.semantic_type_name(resolved)
	return name.len > 0 && name !in ['void', 'unknown']
}

// const_expr_for_name supports const expr for name handling for Transformer.
fn (t &Transformer) const_expr_for_name(name string) ?flat.NodeId {
	return t.const_expr_for_name_in_context(name, t.cur_module, t.cur_file)
}

fn (t &Transformer) const_expr_for_name_in_context(name string, module_name string, file string) ?flat.NodeId {
	if isnil(t.tc) || name.len == 0 {
		return none
	}
	if expr_id := t.tc.const_exprs[name] {
		return expr_id
	}
	key := t.const_type_key_in_context(name, module_name, file) or { return none }
	if expr_id := t.tc.const_exprs[key] {
		return expr_id
	}
	return none
}

// pack_variadic_args supports pack variadic args handling for Transformer.
fn (mut t Transformer) pack_variadic_args(node flat.Node, first_arg int, elem_type types.Type) flat.NodeId {
	expected_enum := t.semantic_type_name(elem_type)
	array_type := '[]${expected_enum}'
	if named_arg := t.transform_variadic_struct_fields(node, first_arg, elem_type) {
		if t.in_const_init {
			return t.make_array_literal_typed([named_arg], array_type)
		}
		tmp_name := t.new_temp('varargs')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_enum, t.make_int_literal(0), t.make_int_literal(1)), array_type)
		value_name := t.new_temp('vararg')
		t.pending_stmts << t.make_decl_assign_typed(value_name, named_arg, expected_enum)
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('array_push', [
			t.make_prefix(.amp, t.make_ident(tmp_name)),
			t.make_prefix(.amp, t.make_ident(value_name)),
		], 'void'))
		t.set_var_type(tmp_name, array_type)
		return t.make_ident(tmp_name)
	}
	if t.in_const_init {
		mut values := []flat.NodeId{cap: int(node.children_count) - first_arg}
		mut i := first_arg
		for i < node.children_count {
			if named_arg := t.transform_variadic_struct_fields(node, i, elem_type) {
				values << named_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
			arg_id := t.a.child(&node, i)
			arg := t.a.nodes[int(arg_id)]
			if arg.kind == .enum_val && expected_enum in t.enum_types {
				values << t.transform_enum_shorthand(arg_id, arg, expected_enum)
			} else if t.is_sum_type_name(expected_enum) {
				values << t.wrap_sum_value(arg_id, expected_enum)
			} else {
				values << t.transform_expr(arg_id)
			}
			i++
		}
		return t.make_array_literal_typed(values, array_type)
	}
	tmp_name := t.new_temp('varargs')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_enum, t.make_int_literal(0), t.make_int_literal(int(node.children_count) - first_arg)), array_type)
	mut i := first_arg
	for i < node.children_count {
		if named_arg := t.transform_variadic_struct_fields(node, i, elem_type) {
			t.append_variadic_value_push(tmp_name, named_arg, flat.empty_node, elem_type)
			i = t.next_non_field_init_arg(node, i)
			continue
		}
		t.append_variadic_arg_push(tmp_name, t.a.child(&node, i), elem_type)
		i++
	}
	t.set_var_type(tmp_name, array_type)
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) append_trailing_args_to_variadic_tail(tail flat.NodeId, node flat.Node, first_arg int, variadic_type types.Array) flat.NodeId {
	if first_arg >= node.children_count {
		return tail
	}
	elem_type := variadic_type.elem_type
	expected_elem := t.semantic_type_name(elem_type)
	array_type := '[]${expected_elem}'
	tail_value := t.stable_transformed_expr_for_reuse(tail, array_type, 'varargs_tail')
	tmp_name := t.new_temp('varargs')
	extra_count := int(node.children_count) - first_arg
	cap_expr := t.make_infix(.plus, t.make_selector(tail_value, 'len', 'int'), t.make_int_literal(extra_count))
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_elem, t.make_int_literal(0), cap_expr), array_type)
	t.pending_stmts << t.make_expr_stmt(t.make_array_push_many_call(t.make_prefix(.amp, t.make_ident(tmp_name)), tail_value, array_type))
	mut i := first_arg
	for i < node.children_count {
		if named_arg := t.transform_variadic_struct_fields(node, i, elem_type) {
			t.append_variadic_value_push(tmp_name, named_arg, flat.empty_node, elem_type)
			i = t.next_non_field_init_arg(node, i)
			continue
		}
		t.append_variadic_arg_push(tmp_name, t.a.child(&node, i), elem_type)
		i++
	}
	t.set_var_type(tmp_name, array_type)
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) append_variadic_arg_push(tmp_name string, arg_id flat.NodeId, elem_type types.Type) {
	expected_elem := t.semantic_type_name(elem_type)
	arg := t.a.nodes[int(arg_id)]
	mut value := if specialized := t.specialize_generic_fn_value_arg(arg_id, expected_elem, true) {
		specialized
	} else if arg.kind == .enum_val && expected_elem in t.enum_types {
		t.transform_enum_shorthand(arg_id, arg, expected_elem)
	} else if t.is_sum_type_name(expected_elem) {
		t.wrap_sum_value(arg_id, expected_elem)
	} else if t.resolve_interface_type_name(expected_elem).len > 0 {
		t.transform_expr_for_type(arg_id, expected_elem)
	} else {
		t.transform_expr(arg_id)
	}
	value = t.clone_borrowed_projection(arg_id, value, expected_elem)
	value = t.clone_checker_marked_receiver_alias_arg(arg_id, value, expected_elem)
	t.append_variadic_value_push(tmp_name, value, arg_id, elem_type)
}

fn (mut t Transformer) append_variadic_value_push(tmp_name string, value flat.NodeId, arg_id flat.NodeId, elem_type types.Type) {
	expected_elem := t.semantic_type_name(elem_type)
	value_name := t.new_temp('vararg')
	if int(arg_id) >= 0 && variadic_elem_is_voidptr(elem_type) {
		value_arg := if t.voidptr_variadic_arg_passes_direct(arg_id) {
			if t.node_type(arg_id) == 'voidptr' {
				value
			} else {
				t.make_cast('voidptr', value, 'voidptr')
			}
		} else {
			storage_type := t.voidptr_variadic_storage_type(arg_id)
			storage_value := if storage_type != t.node_type(arg_id) {
				t.make_cast(storage_type, value, storage_type)
			} else {
				value
			}
			storage_name := t.new_temp('vararg_storage')
			t.pending_stmts << t.make_decl_assign_typed(storage_name, storage_value, storage_type)
			t.make_cast('voidptr', t.make_prefix(.amp, t.make_ident(storage_name)), 'voidptr')
		}
		t.pending_stmts << t.make_decl_assign_typed(value_name, value_arg, expected_elem)
	} else {
		// Keep the explicit interface storage type visible to cgen. A direct interface
		// literal is a struct init, whose concrete payload name would otherwise override
		// the declaration annotation.
		storage_value := if elem_type is types.Interface { t.make_paren(value) } else { value }
		t.pending_stmts << t.make_decl_assign_typed(value_name, storage_value, expected_elem)
	}
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(tmp_name)),
		t.make_prefix(.amp, t.make_ident(value_name)),
	], 'void'))
}

fn variadic_elem_is_voidptr(typ types.Type) bool {
	if typ is types.Pointer {
		return typ.base_type is types.Void
	}
	return false
}

fn (t &Transformer) voidptr_variadic_arg_passes_direct(arg_id flat.NodeId) bool {
	if !isnil(t.tc) {
		return voidptr_variadic_type_passes_direct(t.tc.resolve_type(arg_id))
	}
	typ := t.node_type(arg_id)
	return typ == 'voidptr' || typ == 'nil' || typ == 'charptr' || typ == 'byteptr'
		|| typ.starts_with('&')
}

fn voidptr_variadic_type_passes_direct(typ types.Type) bool {
	if typ is types.Alias {
		return voidptr_variadic_type_passes_direct(typ.base_type)
	}
	return typ is types.Pointer || typ is types.Nil
}

fn (t &Transformer) voidptr_variadic_storage_type(arg_id flat.NodeId) string {
	typ := t.normalize_type_alias(t.node_type(arg_id))
	match typ {
		'char', 'i8', 'u8', 'i16', 'u16' {
			return 'int'
		}
		'f32' {
			return 'f64'
		}
		else {
			return typ
		}
	}
}

fn (mut t Transformer) transform_variadic_struct_fields(node flat.Node, field_start int, elem_type types.Type) ?flat.NodeId {
	if field_start >= node.children_count {
		return none
	}
	if elem_type !is types.Struct {
		return none
	}
	first := t.a.child_node(&node, field_start)
	if first.kind != .field_init {
		return none
	}
	mut field_ids := []flat.NodeId{}
	for i in field_start .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init {
			break
		}
		field_ids << field_id
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	struct_id := t.a.add_node(flat.Node{
		kind: .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value: t.semantic_type_name(elem_type)
		typ: t.semantic_type_name(elem_type)
	})
	return t.transform_struct_fields(struct_id, t.a.nodes[int(struct_id)])
}

// make_array_literal_typed builds make array literal typed data for transform.
fn (mut t Transformer) make_array_literal_typed(values []flat.NodeId, typ string) flat.NodeId {
	start := t.a.children.len
	for value in values {
		t.a.children << value
	}
	return t.a.add_node(flat.Node{
		kind: .array_literal
		children_start: start
		children_count: flat.child_count(values.len)
		typ: typ
	})
}

// stringify_expr supports stringify expr handling for Transformer.
fn (mut t Transformer) stringify_expr(expr_id flat.NodeId) flat.NodeId {
	// Transforming a pointer expression can normalize `&Alias` to `&Base`. Keep the
	// checker's source-level alias here so auto-str can still add `Alias(...)` while
	// reading the pointee through the base representation.
	raw_alias_type := t.raw_alias_type_for_expr(expr_id)
	expr := t.transform_expr(expr_id)
	// A smartcasted identifier's transformed expression is the concrete value,
	// while the source binding still has its interface/sum type. Stringify the
	// narrowed value instead of rebuilding the source container's auto-str.
	key := t.expr_key(expr_id)
	mut typ := if key.len > 0 && t.find_smartcast(key) != none {
		t.node_type(expr)
	} else {
		''
	}
	if typ.len == 0 {
		typ = t.raw_var_type_for_expr(expr_id) or { '' }
	}
	if typ.len == 0 {
		typ = t.raw_var_type_for_expr(expr) or { '' }
	}
	if typ.len == 0 {
		typ = t.node_type(expr)
	}
	if typ.len == 0 {
		typ = t.node_type(expr_id)
	}
	if typ.len == 0 || typ == 'unknown' || t.generic_arg_is_unresolved(typ) {
		checker_type := t.raw_checker_node_type(expr_id)
		if checker_type.len > 0 && checker_type != 'unknown'
			&& !t.generic_arg_is_unresolved(checker_type) {
			typ = checker_type
		}
	}
	if typ.len == 0 {
		// Structural fallback for compound arguments (infix, prefix, cast,
		// paren, ...) so e.g. `println(a + b)` for ints is stringified via
		// strconv__format_int instead of being passed to println as a raw
		// number. Mirrors the fallback already used by string interpolation.
		typ = t.reliable_stringify_type(expr)
		if typ.len == 0 {
			typ = t.reliable_stringify_type(expr_id)
		}
	}
	// A selector of a concrete generic instance can retain the declaration's raw
	// placeholder spelling (for example `&Node[T]`) even though its checked node
	// type is already `&Node[int]`. Auto-stringification must use the concrete
	// instance; otherwise the synthesized locals leak `Node_T` into generated C.
	if stringify_type_has_generic_placeholder(typ) {
		mut concrete_typ := t.lvalue_type(expr_id)
		if concrete_typ.len == 0 || stringify_type_has_generic_placeholder(concrete_typ) {
			concrete_typ = t.node_type(expr_id)
		}
		if concrete_typ.len == 0 || stringify_type_has_generic_placeholder(concrete_typ) {
			concrete_typ = t.lvalue_type(expr)
		}
		if concrete_typ.len > 0 && !stringify_type_has_generic_placeholder(concrete_typ) {
			typ = concrete_typ
		}
	}
	if raw_alias_type.len > 0 {
		typ = raw_alias_type
	}
	// `transform_expr` above already auto-dereferenced a `pointer_value_rvalues`
	// ident (mutable for-in bindings, `@[heap]`-promoted locals, ...) to its value,
	// but `typ` was resolved independently and still carries the storage `&T`.
	// Strip it so `wrap_string_conversion` sees a type matching what `expr` (now a
	// plain value read) actually evaluates to, instead of treating it as a
	// genuine nilable pointer.
	if int(expr_id) >= 0 && typ.starts_with('&') {
		raw_node := t.a.nodes[int(expr_id)]
		if raw_node.kind == .ident && t.pointer_value_rvalues[raw_node.value] {
			typ = typ[1..]
		}
	}
	return t.wrap_string_conversion(expr, typ)
}

fn (t &Transformer) declared_selector_pointer_alias_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.declared_selector_pointer_alias_type(t.a.child(&node, 0))
	}
	if node.kind != .selector || node.children_count == 0 || node.value.len == 0 {
		return none
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.lvalue_type(base_id)
	if base_type.len == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type.len == 0 {
		base_type = t.raw_checker_node_type(base_id)
	}
	raw_type := t.lookup_struct_field_raw_type(base_type, node.value) or { return none }
	clean := raw_type.trim_space()
	if clean.starts_with('&') && t.is_type_alias_name(t.trim_pointer_type(clean)) {
		return clean
	}
	return none
}

// reliable_stringify_type supports reliable stringify type handling for Transformer.
fn (t &Transformer) reliable_stringify_type(id flat.NodeId) string {
	mut typ := t.node_type(id)
	if typ.len > 0 {
		return typ
	}
	if int(id) >= 0 {
		node := t.a.nodes[int(id)]
		if node.typ.len > 0 {
			return node.typ
		}
		match node.kind {
			.int_literal {
				return 'int'
			}
			.float_literal {
				return 'f64'
			}
			.bool_literal {
				return 'bool'
			}
			.char_literal {
				return if node.value.starts_with('c:') { '&u8' } else { 'rune' }
			}
			.string_literal, .string_interp {
				return 'string'
			}
			.infix {
				return t.reliable_infix_stringify_type(node)
			}
			.prefix {
				if node.op == .not {
					return 'bool'
				}
				if node.children_count > 0 {
					return t.reliable_stringify_type(t.a.child(&node, 0))
				}
			}
			.paren {
				if node.children_count > 0 {
					return t.reliable_stringify_type(t.a.child(&node, 0))
				}
			}
			.cast_expr {
				if node.value.len > 0 {
					return node.value
				}
				if node.children_count > 0 {
					return t.reliable_stringify_type(t.a.child(&node, 0))
				}
			}
			.call {
				if node.children_count > 0 {
					callee_type := t.reliable_stringify_type(t.a.child(&node, 0))
					if ret := fn_type_return_type_text(callee_type) {
						return ret
					}
				}
			}
			else {}
		}
	}
	return ''
}

// reliable_infix_stringify_type supports reliable infix stringify type handling for Transformer.
fn (t &Transformer) reliable_infix_stringify_type(node flat.Node) string {
	if node.children_count < 2 {
		return ''
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs_type := t.reliable_stringify_type(lhs_id)
	rhs_type := t.reliable_stringify_type(rhs_id)
	if lhs_type == 'string' || rhs_type == 'string' {
		return 'string'
	}
	match node.op {
		.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or {
			return 'bool'
		}
		.right_shift_unsigned {
			if lhs_type.len > 0 {
				return t.unsigned_shift_type_text(lhs_type)
			}
		}
		.left_shift, .right_shift {
			// shifts keep the left operand's type/width
			if lhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type) {
				return lhs_type
			}
		}
		.plus, .minus, .mul, .power, .div, .mod, .amp, .pipe, .xor {
			if lhs_type.len > 0 && rhs_type.len > 0 && t.is_numeric_stringify_type(lhs_type)
				&& t.is_numeric_stringify_type(rhs_type) {
				if promoted := promote_numeric_literal_infix_type(t.a.nodes[int(lhs_id)], lhs_type, t.a.nodes[int(rhs_id)], rhs_type) {
					return promoted
				}
				// Use the promoted result type, not the lhs, so e.g.
				// `1 + u64(x)` formats as unsigned rather than signed int.
				return promote_numeric_stringify_type(lhs_type, rhs_type)
			}
		}
		else {}
	}

	return ''
}

fn (t &Transformer) unsigned_shift_type_text(typ string) string {
	clean := typ.trim_space()
	if !isnil(t.tc) {
		resolved := t.semantic_type_name(types.unsigned_shift_result_type(t.tc.parse_type(clean)))
		if resolved in ['u8', 'u16', 'u32', 'u64', 'usize'] {
			return resolved
		}
	}
	if types.is_builtin_type_name(clean) {
		return t.semantic_type_name(types.unsigned_shift_result_type(types.builtin_type_value(clean)))
	}
	return typ
}

fn promote_numeric_literal_infix_type(lhs flat.Node, lhs_type string, rhs flat.Node, rhs_type string) ?string {
	if promoted := promote_int_literal_infix_type(lhs, rhs, rhs_type) {
		return promoted
	}
	if promoted := promote_int_literal_infix_type(rhs, lhs, lhs_type) {
		return promoted
	}
	if lhs_type == 'f32' && rhs.kind == .float_literal {
		return 'f32'
	}
	if rhs_type == 'f32' && lhs.kind == .float_literal {
		return 'f32'
	}
	return none
}

fn promote_int_literal_infix_type(lit flat.Node, other flat.Node, other_type string) ?string {
	if lit.kind != .int_literal || other.kind == .int_literal {
		return none
	}
	value := decimal_int_literal_value(lit.value) or { return none }
	if unsigned_type_text_accepts_int_literal(other_type, value) {
		return other_type
	}
	return none
}

fn decimal_int_literal_value(text string) ?int {
	if text.len == 0 {
		return none
	}
	clean := text.replace('_', '')
	if clean.len == 0 {
		return none
	}
	for ch in clean {
		if ch < `0` || ch > `9` {
			return none
		}
	}
	return clean.int()
}

fn unsigned_type_text_accepts_int_literal(typ string, value int) bool {
	if value < 0 {
		return false
	}
	max := match typ {
		'u8', 'byte' { 255 }
		'u16' { 65535 }
		'u32', 'u64', 'usize' {
			return true
		}
		else {
			return false
		}
	}

	return value <= max
}

// promote_numeric_stringify_type returns the result type of a binary numeric
// operation for stringify purposes: floats dominate, the wider integer wins,
// and on equal width an explicit type beats the untyped-literal default `int`.
fn promote_numeric_stringify_type(a string, b string) string {
	if a == b {
		return a
	}
	if a == 'f64' || b == 'f64' {
		return 'f64'
	}
	if a == 'f32' || b == 'f32' {
		return 'f32'
	}
	ra := int_stringify_rank(a)
	rb := int_stringify_rank(b)
	if ra > rb {
		return a
	}
	if rb > ra {
		return b
	}
	if a == 'int' {
		return b
	}
	if b == 'int' {
		return a
	}
	return a
}

fn int_stringify_rank(typ string) int {
	return match typ {
		'i8', 'u8', 'byte' { 8 }
		'i16', 'u16' { 16 }
		'i32', 'u32', 'int', 'rune' { 32 }
		'i64', 'u64', 'isize', 'usize' { 64 }
		else { 32 }
	}
}

// is_numeric_stringify_type reports whether is numeric stringify type applies in transform.
fn (t &Transformer) is_numeric_stringify_type(typ string) bool {
	is_number := typ in ['int', 'int literal', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8',
		'byte', 'u16', 'u32', 'u64', 'f32', 'f64', 'float literal', 'rune']
	return is_number || typ in t.enum_types
}

// is_enum_stringify_type reports whether is enum stringify type applies in transform.
fn (t &Transformer) is_enum_stringify_type(typ string) bool {
	mut clean_typ := typ
	if clean_typ.starts_with('&') {
		clean_typ = clean_typ[1..]
	}
	if clean_typ.starts_with('mut ') {
		clean_typ = clean_typ[4..]
	}
	if clean_typ in t.enum_types {
		return true
	}
	mut qtyp := clean_typ
	if !qtyp.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp = '${t.cur_module}.${clean_typ}'
		if qtyp in t.enum_types {
			return true
		}
	}
	if isnil(t.tc) {
		return false
	}
	if alias := t.tc.type_aliases[clean_typ] {
		return t.is_enum_stringify_type(alias)
	}
	if qtyp != clean_typ {
		if alias := t.tc.type_aliases[qtyp] {
			return t.is_enum_stringify_type(alias)
		}
	}
	parsed := t.tc.parse_type(clean_typ)
	if parsed is types.Enum {
		return true
	}
	if qtyp != clean_typ {
		qparsed := t.tc.parse_type(qtyp)
		if qparsed is types.Enum {
			return true
		}
	}
	return false
}

// enum_str_method_name supports enum str method name handling for Transformer.
fn (t &Transformer) enum_str_method_name(typ string) ?string {
	mut candidates := []string{cap: 3}
	candidates << typ
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${typ}'
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(typ)
		if parsed is types.Enum {
			candidates << parsed.name
		}
	}
	for candidate in candidates {
		method := '${candidate}.str'
		if t.is_known_fn_name(method) {
			return method
		}
	}
	return none
}

// enum_autostr_call builds a call to the compiler-synthesized `<Enum>__autostr` helper
// (emitted by cgen's enum_str_defs) which returns the enum field NAME. Used as the default
// `${enum}` stringification when the user has not defined a custom `.str()` — V auto-derives
// one. Mirrors the struct-str qualification so the C name matches cgen's enum_decls naming.
fn (mut t Transformer) enum_autostr_call(expr flat.NodeId, typ string) flat.NodeId {
	qualified := t.enum_autostr_type_name(typ)
	return t.make_call_typed('${c_name(qualified)}__autostr', [expr], 'string')
}

fn (t &Transformer) enum_autostr_type_name(typ string) string {
	mut qualified := typ
	if qualified.starts_with('main.') {
		qualified = qualified[5..]
	} else if !typ.contains('.') {
		q := '${t.cur_module}.${typ}'
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& q in t.enum_types {
			qualified = q
		} else if fields := t.enum_types[typ] {
			// A bare name in a foreign-module expansion (auto-stringified struct
			// fields keep their declaring module's spelling): the bare alias entry
			// shares its field-array backing with the declaring qualified entry,
			// which recovers the exact declared name cgen used for the helper.
			suffix := '.${typ}'
			for k, v in t.enum_types {
				if k.ends_with(suffix) && v.data == fields.data {
					qualified = k
					break
				}
			}
		} else {
			// No bare alias (shadowed): fall back to a unique qualified suffix.
			suffix := '.${typ}'
			mut match_name := ''
			mut matches := 0
			for k, _ in t.enum_types {
				if k.ends_with(suffix) {
					match_name = k
					matches++
					if matches > 1 {
						break
					}
				}
			}
			if matches == 1 {
				qualified = match_name
			}
		}
	}
	short_name := short_name_view(qualified)
	if qualified !in t.enum_types && short_name in t.enum_types {
		return short_name
	}
	if !isnil(t.tc) && qualified !in t.tc.enum_names {
		if short_name in t.tc.enum_names {
			return short_name
		}
	}
	return qualified
}

// wrap_string_conversion transforms wrap string conversion data for transform.
fn (mut t Transformer) wrap_string_conversion(expr flat.NodeId, typ string) flat.NodeId {
	mut clean_typ := typ
	is_ref := clean_typ.starts_with('&') && !t.is_fixed_array_type(clean_typ)
	if is_ref {
		clean_typ = clean_typ[1..]
	}
	for clean_typ.starts_with('shared ') {
		clean_typ = clean_typ[7..].trim_space()
	}
	for clean_typ.starts_with('atomic ') {
		clean_typ = clean_typ[7..].trim_space()
	}
	if clean_typ.starts_with('builtin.') {
		clean_typ = clean_typ['builtin.'.len..]
	}
	if source_typ := t.source_type_name_from_c_name(clean_typ) {
		return t.wrap_string_conversion(expr, source_typ)
	}
	if source_typ := t.fixed_array_source_type_from_c_name(clean_typ) {
		return t.wrap_string_conversion(expr, source_typ)
	}
	if source_typ := t.generic_specialized_source_type_name(clean_typ) {
		if source_typ != clean_typ {
			return t.wrap_string_conversion(expr, source_typ)
		}
	}
	normalized_stringify_type := t.normalize_runtime_array_stringify_type(clean_typ)
	if normalized_stringify_type != clean_typ {
		return t.wrap_string_conversion(expr, normalized_stringify_type)
	}
	if t.is_fixed_array_type(clean_typ) {
		elem_type := fixed_array_elem_type(clean_typ)
		arr := t.fixed_array_value_to_array(expr, clean_typ, '[]${elem_type}')
		return t.wrap_string_conversion(arr, '[]${elem_type}')
	}
	if t.is_optional_type_name(clean_typ) {
		optional_str := t.wrap_optional_string_conversion(expr, clean_typ)
		if is_ref {
			return t.string_plus(t.make_string_literal('&'), optional_str)
		}
		return optional_str
	}
	if clean_typ.len == 0 || clean_typ == 'unknown' {
		inferred := t.resolve_expr_type(expr)
		if inferred.len > 0 && inferred != clean_typ {
			return t.wrap_string_conversion(expr, inferred)
		}
	}
	if !isnil(t.tc) {
		local_struct_shadows_alias := t.bare_struct_name_is_local_to_current_module(clean_typ)
		if !local_struct_shadows_alias {
			if alias_name, alias := t.lookup_str_alias(clean_typ) {
				return t.alias_str_wrap(expr, alias_name, alias, is_ref)
			}
			if alias := t.tc.type_aliases[clean_typ] {
				return t.alias_str_wrap(expr, clean_typ, alias, is_ref)
			}
		}
		mut qtyp := clean_typ
		if !qtyp.contains('.') && t.cur_module.len > 0 {
			qtyp = '${t.cur_module}.${clean_typ}'
		}
		if alias := t.tc.type_aliases[qtyp] {
			return t.alias_str_wrap(expr, clean_typ, alias, is_ref)
		}
		if clean_typ.starts_with('main.') || clean_typ.starts_with('builtin.') {
			short_typ := clean_typ.all_after_first('.')
			if alias := t.tc.type_aliases[short_typ] {
				return t.alias_str_wrap(expr, short_typ, alias, is_ref)
			}
		}
		if !clean_typ.contains('.') && !local_struct_shadows_alias {
			for aname, target in t.tc.type_aliases {
				if short_name_view(aname) == clean_typ {
					return t.alias_str_wrap(expr, clean_typ, target, is_ref)
				}
			}
		}
		if !local_struct_shadows_alias {
			parsed := t.tc.parse_type(clean_typ)
			if parsed is types.FnType {
				return t.make_string_literal(typeof_display_type_text(clean_typ))
			}
			if parsed is types.MultiReturn {
				return t.lower_multi_return_str(expr, parsed, t.multi_return_type_name(parsed.types))
			}
			if parsed is types.Enum {
				if method := t.enum_str_method_name(clean_typ) {
					return t.make_call_typed(method, [expr], 'string')
				}
				return t.enum_autostr_call(expr, clean_typ)
			}
		}
		if qtyp != clean_typ {
			qparsed := t.tc.parse_type(qtyp)
			if qparsed is types.Enum {
				if method := t.enum_str_method_name(qtyp) {
					return t.make_call_typed(method, [expr], 'string')
				}
				return t.enum_autostr_call(expr, qtyp)
			}
		}
	}
	if is_ref && clean_typ == 'string' {
		return t.make_call_typed('ptr_str', [expr], 'string')
	}
	if clean_typ == 'string' {
		return expr
	}
	if clean_typ == 'thread' || clean_typ.starts_with('thread ') {
		payload := if clean_typ.starts_with('thread ') {
			clean_typ[7..].trim_space()
		} else {
			'void'
		}
		return t.make_string_literal('thread(${payload})')
	}
	if clean_typ.starts_with('chan ') {
		channel_value := if is_ref { t.make_prefix(.mul, expr) } else { expr }
		return t.make_call_typed('v3_chan_str', [channel_value,
			t.make_string_literal(clean_typ[5..].trim_space())], 'string')
	}
	if clean_typ.starts_with('fn(') || clean_typ.starts_with('fn (') {
		return t.make_string_literal(stringify_fn_type_display(clean_typ))
	}
	if is_ref {
		expr_node := t.a.nodes[int(expr)]
		if expr_node.kind == .ident && t.string_interp_needs_value_read(expr_node.value, typ) {
			return t.wrap_string_conversion(t.make_prefix(.mul, expr), clean_typ)
		}
		if clean_typ.starts_with('[]') || clean_typ.starts_with('map[')
			|| t.is_fixed_array_type(clean_typ) {
			return t.lower_ref_value_str(expr, typ, '&nil')
		}
		// A `&Struct`/`&SumType` stringifies to the pointee's `.str()` value: the custom
		// method when one exists (no `&` prefix, e.g. map/array elements), otherwise
		// `&nil` for a null pointer or `&` + the value's auto str. Other pointer kinds
		// (voidptr, `&int`, `&string`, ...) keep the raw ptr_str below.
		iface := t.resolve_interface_type_name(clean_typ)
		if iface.len > 0 {
			return t.lower_ref_interface_str(expr, iface)
		}
		if aggregate := t.stringify_aggregate_type_name(clean_typ) {
			return t.lower_ref_str(expr, aggregate)
		}
	}
	iface_name := t.resolve_interface_type_name(clean_typ)
	if !is_ref && iface_name.len > 0 {
		if 'str' in t.tc.interface_abstract_method_names(iface_name) {
			value := t.stable_transformed_expr_for_reuse(expr, clean_typ, 'iface_str')
			method_name := '${iface_name}.str'
			t.mark_fn_used_name(method_name)
			t.mark_interface_method_implementers_used(iface_name, 'str')
			return t.make_call_typed(method_name, [t.make_prefix(.amp, value)], 'string')
		}
	}
	if is_ref || clean_typ in ['voidptr', 'byteptr', 'charptr'] {
		return t.make_call_typed('ptr_str', [expr], 'string')
	}
	if clean_typ == 'IError' || clean_typ == 'builtin.IError' {
		return t.make_call_typed('IError.str', [expr], 'string')
	}
	if iface_name.len > 0 {
		str_key := '${iface_name}.str'
		known_str := str_key in t.fn_ret_types || (!isnil(t.tc) && str_key in t.tc.fn_ret_types)
		if known_str {
			return t.make_call_typed(str_key, [expr], 'string')
		}
		return t.lower_interface_auto_str(expr, iface_name)
	}
	if clean_typ.starts_with('[]') || clean_typ.starts_with('map[') {
		if method_name := t.resolve_receiver_method_for_type(clean_typ, 'str') {
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, [expr], 'string')
		}
	}
	match clean_typ {
		'bool' {
			return t.make_call_typed('bool.str', [expr], 'string')
		}
		'u8', 'byte', 'u16', 'u32', 'usize' {
			// C integer promotion would pass an untruncated `int` into the u64
			// param (`u8(255) + u8(1)` is 256 in C); cast back to the value
			// type first so the arithmetic wraps at the V type's width.
			truncated := t.make_cast(clean_typ, expr, clean_typ)
			return t.make_call_typed('strconv__format_uint', [truncated, t.make_int_literal(10)], 'string')
		}
		'u64' {
			return t.make_call_typed('u64.str', [expr], 'string')
		}
		'int', 'int literal' {
			return t.make_call_typed('int.str', [expr], 'string')
		}
		'i8' {
			return t.make_call_typed('i8.str', [expr], 'string')
		}
		'i16' {
			return t.make_call_typed('i16.str', [expr], 'string')
		}
		'i32' {
			return t.make_call_typed('i32.str', [expr], 'string')
		}
		'i64' {
			return t.make_call_typed('i64.str', [expr], 'string')
		}
		'char' {
			return t.make_call_typed('v3_char_string', [
				t.make_cast('int', expr, 'int'),
			], 'string')
		}
		'isize' {
			return t.make_call_typed('strconv__format_int', [expr, t.make_int_literal(10)], 'string')
		}
		'f32' {
			return t.make_call_typed('f32.str', [expr], 'string')
		}
		'f64', 'float literal' {
			return t.make_call_typed('f64.str', [expr], 'string')
		}
		else {
			if clean_typ in t.enum_types {
				if method := t.enum_str_method_name(clean_typ) {
					return t.make_call_typed(method, [expr], 'string')
				}
				return t.enum_autostr_call(expr, clean_typ)
			}
			mut qenum := clean_typ
			if !clean_typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
				&& t.cur_module != 'builtin' {
				qenum = '${t.cur_module}.${clean_typ}'
			}
			if qenum in t.enum_types {
				if method := t.enum_str_method_name(qenum) {
					return t.make_call_typed(method, [expr], 'string')
				}
				return t.enum_autostr_call(expr, qenum)
			}
			if generic_str := t.generic_receiver_str_call(expr, clean_typ) {
				return generic_str
			}
			if aggregate_type := t.stringify_aggregate_type_name(clean_typ) {
				qualified := aggregate_type
				if str_fn := t.aggregate_str_method_name(qualified) {
					t.mark_fn_used_name(str_fn)
					return t.make_call_typed(str_fn, [expr], 'string')
				}
				if t.building_v && t.auto_str_synthesis_type != qualified {
					return t.request_auto_str_helper(expr, qualified)
				}
				_, sum_variants := t.concrete_sum_name_and_variants(qualified)
				if sum_variants.len > 0 {
					return t.lower_sum_str(expr, qualified)
				}
				if struct_str := t.lower_struct_str(expr, qualified) {
					return struct_str
				}
				return t.make_string_literal('${qualified}{}')
			} else if clean_typ.len > 0 && clean_typ.starts_with('[]') {
				return t.lower_array_str(expr, clean_typ)
			} else if clean_typ.len > 0 && clean_typ.starts_with('map[') {
				return t.lower_map_str(expr, clean_typ)
			} else if clean_typ == 'rune' {
				return t.make_call_typed('rune.str', [expr], 'string')
			} else {
				return expr
			}
		}
	}
}

fn (mut t Transformer) lower_interface_auto_str(expr flat.NodeId, iface_name string) flat.NodeId {
	return t.lower_interface_auto_str_with_nil(expr, iface_name, true)
}

fn (mut t Transformer) lower_interface_auto_str_with_nil(expr flat.NodeId, iface_name string, allow_nil bool) flat.NodeId {
	if t.stringify_stack_count(iface_name) > 0
		|| (t.stringify_stack.len >= t.stringify_depth_cap
			&& !t.stringify_types_match(t.auto_str_synthesis_type, iface_name)) {
		return t.request_auto_str_helper(expr, iface_name)
	}
	t.stringify_stack << iface_name
	defer {
		t.stringify_stack.delete_last()
	}
	display_name := iface_name.all_after_last('.')
	if isnil(t.tc) {
		return t.make_string_literal('${display_name}{}')
	}
	t.refresh_interface_impl_indexes_for_boxed_types()
	value := t.stable_transformed_expr_for_reuse(expr, iface_name, 'iface_str')
	result_name := t.new_temp('iface_str')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_string_literal('unknown interface value'), 'string')
	tag := t.make_selector(value, '_typ', 'int')
	if allow_nil {
		object_is_nil := t.make_infix(.eq, t.make_selector(value, '_object', 'voidptr'), t.make_int_literal(0))
		tag_is_zero := t.make_infix(.eq, tag, t.make_int_literal(0))
		nil_cond := t.make_infix(.logical_and, tag_is_zero, object_is_nil)
		t.pending_stmts << t.make_if(nil_cond, t.make_block([
			t.make_assign(t.make_ident(result_name), t.make_string_literal('nil')),
		]), t.make_empty())
	}
	impl_names := if t.is_builtin_ierror_interface_name(iface_name) {
		t.tc.ierror_impl_names()
	} else {
		t.interface_impl_index_for_transform(iface_name).names
	}
	for impl_name in impl_names {
		// A source-level box scan can only see the generic base (`Text`) for a
		// conversion that becomes `Text[int]` while a concrete default expression
		// is lowered. The base has no C runtime type and must not produce a dispatch
		// branch; the worker-local late-box index supplies the concrete application.
		if t.interface_auto_str_impl_is_open_generic(impl_name) {
			continue
		}
		mut concrete_types := []string{}
		if impl_name in ['array', 'Array'] {
			for concrete in impl_names {
				if concrete.starts_with('[]') {
					concrete_types << concrete
				}
			}
			if concrete_types.len > 0 {
				continue
			}
		}
		if concrete_types.len == 0 {
			if !t.interface_boxed_type_marked(iface_name, impl_name) {
				continue
			}
			concrete_types << impl_name
		}
		for concrete_type in concrete_types {
			type_id := t.interface_impl_type_id(iface_name, concrete_type) or {
				t.interface_impl_type_id(iface_name, impl_name) or { continue }
			}
			object := t.make_cast('&${concrete_type}', t.make_selector(value, '_object', 'voidptr'), '&${concrete_type}')
			concrete := t.make_prefix(.mul, object)
			t.set_node_typ(int(concrete), concrete_type)
			saved := t.pending_stmts.clone()
			t.pending_stmts.clear()
			inner_type := if alias_target := t.tc.type_aliases[concrete_type] {
				if alias_target.starts_with('fn(') || alias_target.starts_with('fn ') {
					alias_target
				} else {
					concrete_type
				}
			} else {
				concrete_type
			}
			wrapped := if t.stringify_type_at_circular_limit(inner_type) {
				t.make_string_literal('<circular>')
			} else {
				mut inner := t.wrap_string_conversion(concrete, inner_type)
				quote_type := t.normalize_type_alias(inner_type)
				if quote_type == 'string' {
					inner = t.string_plus(t.string_plus(t.make_string_literal("'"), inner), t.make_string_literal("'"))
				} else if quote_type == 'rune' {
					inner = t.string_plus(t.string_plus(t.make_string_literal('`'), inner), t.make_string_literal('`'))
				}
				t.string_plus(t.string_plus(t.make_string_literal('${display_name}('), inner), t.make_string_literal(')'))
			}
			mut render_body := []flat.NodeId{}
			t.drain_pending(mut render_body)
			t.pending_stmts = saved
			assign := t.make_assign(t.make_ident(result_name), wrapped)
			render_body << assign
			mut then_body := render_body.clone()
			if t.interface_autostr_impl_needs_address_guard(inner_type) {
				object_addr := t.make_selector(value, '_object', 'voidptr')
				object_type := t.make_int_literal(t.type_index_for_type_name(inner_type))
				mut live_body := [
					t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [
						object_addr,
						object_type,
					], 'void')),
				]
				live_body << render_body
				live_body << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], 'void'))
				seen := t.make_call_typed('autostr_addr_type_in_stack', [object_addr, object_type], 'bool')
				seen_body := t.make_block([
					t.make_assign(t.make_ident(result_name), t.make_string_literal('<circular>')),
				])
				seen_guard := t.make_if(seen, seen_body, t.make_block(live_body))
				is_nil := t.make_infix(.eq, object_addr, t.make_int_literal(0))
				nil_body := t.make_block([
					t.make_assign(t.make_ident(result_name), t.make_string_literal('nil')),
				])
				then_body = [t.make_if(is_nil, nil_body, t.make_block([seen_guard]))]
			}
			cond := t.make_infix(.eq, tag, t.make_int_literal(type_id))
			t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
		}
	}
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), 'string')
	return result
}

fn (t &Transformer) interface_autostr_impl_needs_address_guard(typ string) bool {
	clean := t.normalize_type_alias(typ)
	if _ := t.stringify_aggregate_type_name(clean) {
		return true
	}
	return clean.starts_with('[]') || clean.starts_with('map[')
		|| clean.starts_with('chan ')
		|| (clean.starts_with('[') && clean.contains(']'))
		|| t.is_interface_type(clean)
}

fn (t &Transformer) interface_auto_str_impl_is_open_generic(name string) bool {
	_, _, is_generic_app := generic_app_parts(name)
	if is_generic_app {
		return false
	}
	if _ := t.generic_struct_params_for_base(name) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return name in t.tc.type_alias_generic_params
		|| t.tc.qualify_name(name) in t.tc.type_alias_generic_params
}

fn (mut t Transformer) lower_ref_interface_str(expr flat.NodeId, iface_name string) flat.NodeId {
	ptr_type := '&${iface_name}'
	ptr_name := t.new_temp('iface_ref_str_ptr')
	res_name := t.new_temp('iface_ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('&nil'), 'string')
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), iface_name)
	inner := t.lower_interface_auto_str(value, iface_name)
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	then_body << t.make_assign(t.make_ident(res_name), t.string_plus(t.make_string_literal('&'), inner))
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	result := t.make_ident(res_name)
	t.set_node_typ(int(result), 'string')
	return result
}

// lower_ref_str stringifies a `&Struct`/`&SumType` pointer with the same semantics V uses
// for top-level pointer interpolation: emit `&nil` for a null pointer or `&` plus the
// pointee's custom/automatic string for a live one. Container elements and struct fields
// use lower_ref_value_str, which intentionally omits the top-level prefix.
fn (mut t Transformer) lower_ref_str(expr flat.NodeId, aggregate string) flat.NodeId {
	if str_fn := t.aggregate_str_method_name(aggregate) {
		t.mark_fn_used_name(str_fn)
		return t.lower_ref_str_guarded(expr, aggregate, true, str_fn, '&nil')
	}
	return t.lower_ref_str_prefixed(expr, aggregate)
}

fn (t &Transformer) str_method_has_pointer_receiver(str_fn string) bool {
	if isnil(t.tc) {
		return false
	}
	params := t.tc.fn_param_types[str_fn] or { return false }
	if params.len == 0 {
		return false
	}
	receiver := params[0]
	return receiver is types.Pointer || t.semantic_type_name(receiver).starts_with('&')
}

fn (t &Transformer) aggregate_str_method_name(aggregate string) ?string {
	if aggregate.starts_with('C.') {
		method_name := '${aggregate}.str'
		method_suffix := '.${method_name}'
		for name, _ in t.fn_ret_types {
			if name == method_name || name.ends_with(method_suffix) {
				return name
			}
		}
		if !isnil(t.tc) {
			for name, _ in t.tc.fn_ret_types {
				if name == method_name || name.ends_with(method_suffix) {
					return name
				}
			}
		}
	}
	if method := t.resolve_receiver_method_for_type(aggregate, 'str') {
		if t.receiver_method_matches_type_name(method, aggregate) {
			return method
		}
	}
	// Imported declarations have a short convenience entry in the transformer's
	// struct table. When that short spelling is used through a selective import,
	// resolve its qualified owner before falling back to generated auto-str.
	if !aggregate.contains('.') && !t.bare_struct_name_is_local_to_current_module(aggregate) {
		if qualified := t.qualified_types[aggregate] {
			if method := t.resolve_receiver_method_for_type(qualified, 'str') {
				return method
			}
		}
	}
	c_name_fn := '${c_name(aggregate)}__str'
	v_name_fn := '${aggregate}.str'
	if c_name_fn in t.fn_ret_types || (!isnil(t.tc) && c_name_fn in t.tc.fn_ret_types) {
		return c_name_fn
	}
	if v_name_fn in t.fn_ret_types || (!isnil(t.tc) && v_name_fn in t.tc.fn_ret_types) {
		return v_name_fn
	}
	return none
}

fn auto_str_helper_name(aggregate string) string {
	return '__v3_autostr_${c_name(aggregate)}'
}

fn (t &Transformer) auto_str_helper_owner_module(aggregate string) string {
	if !isnil(t.tc) {
		if module_name := t.tc.struct_modules[aggregate] {
			return module_name
		}
	}
	if !aggregate.starts_with('C.') && aggregate.contains('.') {
		return aggregate.all_before_last('.')
	}
	return if t.cur_module.len > 0 { t.cur_module } else { 'main' }
}

fn (mut t Transformer) request_auto_str_helper(expr flat.NodeId, aggregate string) flat.NodeId {
	helper := auto_str_helper_name(aggregate)
	if aggregate !in t.auto_str_types {
		t.auto_str_types[aggregate] = AutoStrRequest{
			module: t.cur_module
			file: t.cur_file
			// The helper name contains the fully qualified C type, while the module
			// assignment keeps its definition with the cached object that owns it.
			helper_module: t.auto_str_helper_owner_module(aggregate)
		}
	}
	t.mark_fn_used_name(helper)
	if t.stringify_stack.len > 0 || !t.expr_can_take_address(expr) {
		return t.make_call_typed(helper, [expr], 'string')
	}
	value := t.stable_expr_for_reuse(expr)
	address := t.make_cast('voidptr', t.make_prefix(.amp, value), 'voidptr')
	address_type := t.make_int_literal(t.type_index_for_type_name(aggregate))
	result_name := t.new_temp('autostr')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_string_literal('<circular>'), 'string')
	live_body := t.make_block([
		t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [address, address_type], 'void')),
		t.make_assign(t.make_ident(result_name), t.make_call_typed(helper, [value], 'string')),
		t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], 'void')),
	])
	seen := t.make_call_typed('autostr_addr_type_in_stack', [address, address_type], 'bool')
	t.pending_stmts << t.make_if(seen, t.make_empty(), live_body)
	return t.make_ident(result_name)
}

fn (t &Transformer) has_pending_auto_str_helpers() bool {
	for name, _ in t.auto_str_types {
		if !t.auto_str_synthesized[name] && auto_str_helper_name(name) !in t.fn_ret_types {
			return true
		}
	}
	return false
}

fn (mut t Transformer) synthesize_auto_str_helpers() []string {
	old_module := t.cur_module
	old_file := t.cur_file
	old_helper_module := t.auto_str_helper_module
	old_synthesis_type := t.auto_str_synthesis_type
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	was_log_active := t.used_fns_log_active
	log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	for {
		mut pending := []string{}
		for name, _ in t.auto_str_types {
			if name in t.auto_str_synthesized {
				continue
			}
			if auto_str_helper_name(name) in t.fn_ret_types {
				t.auto_str_synthesized[name] = true
				continue
			}
			pending << name
		}
		if pending.len == 0 {
			break
		}
		pending.sort()
		for name in pending {
			t.auto_str_synthesized[name] = true
			req := t.auto_str_types[name] or { AutoStrRequest{} }
			t.cur_module = req.module
			t.cur_file = req.file
			t.auto_str_helper_module = if req.helper_module.len > 0 {
				req.helper_module
			} else {
				'main'
			}
			t.auto_str_synthesis_type = name
			if !isnil(t.tc) {
				t.tc.cur_module = req.module
				t.tc.cur_file = req.file
			}
			t.build_auto_str_helper_fn(name)
		}
	}
	mut new_names := []string{}
	mut seen := map[string]bool{}
	for i in log_start .. t.used_fns_log.len {
		name := t.used_fns_log[i]
		if name.len > 0 && !seen[name] {
			seen[name] = true
			new_names << name
		}
	}
	if !was_log_active {
		t.used_fns_log_active = false
		t.used_fns_log = t.used_fns_log[..log_start].clone()
	}
	t.cur_module = old_module
	t.cur_file = old_file
	t.auto_str_helper_module = old_helper_module
	t.auto_str_synthesis_type = old_synthesis_type
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return new_names
}

fn (mut t Transformer) build_auto_str_helper_fn(aggregate string) {
	helper := auto_str_helper_name(aggregate)
	saved_pending := t.pending_stmts
	saved_vars := t.var_types.clone()
	saved_fn_name := t.cur_fn_name
	saved_ret_type := t.cur_fn_ret_type
	t.pending_stmts = []flat.NodeId{}
	t.reset_var_types()
	t.cur_fn_name = helper
	t.cur_fn_ret_type = 'string'
	param_name := '__auto_str_value'
	param := t.a.add_node(flat.Node{
		kind: .param
		value: param_name
		typ: aggregate
	})
	t.set_var_type(param_name, aggregate)
	value := t.make_ident(param_name)
	t.set_node_typ(int(value), aggregate)
	result := if t.is_interface_type(aggregate) {
		t.lower_interface_auto_str(value, aggregate)
	} else if t.sum_type_variants_for_index(aggregate).len > 0 {
		t.lower_sum_str(value, aggregate)
	} else {
		t.lower_struct_str(value, aggregate) or {
			t.make_string_literal('${struct_string_display_name(aggregate)}{}')
		}
	}
	mut stmts := t.pending_stmts.clone()
	stmts << t.make_return(result, 'string')
	t.pending_stmts = saved_pending
	t.restore_var_types(saved_vars)
	t.cur_fn_name = saved_fn_name
	t.cur_fn_ret_type = saved_ret_type
	t.a.add_node(flat.Node{
		kind: .module_decl
		value: if t.auto_str_helper_module.len > 0 { t.auto_str_helper_module } else { 'main' }
	})
	start := t.a.children.len
	t.a.children << param
	t.a.children << stmts
	t.a.add_node(flat.Node{
		kind: .fn_decl
		value: helper
		typ: 'string'
		children_start: i32(start)
		children_count: flat.child_count(1 + stmts.len)
	})
	helper_key := if t.auto_str_helper_module !in ['', 'main', 'builtin'] {
		'${t.auto_str_helper_module}.${helper}'
	} else {
		helper
	}
	t.set_fn_ret_type(helper, 'string')
	t.set_fn_ret_type(helper_key, 'string')
	t.mark_fn_used_name(helper_key)
	if !isnil(t.tc) {
		t.tc.ensure_private_transform_signatures()
		t.tc.fn_ret_types[helper] = t.tc.parse_type('string')
		t.tc.fn_ret_types[helper_key] = t.tc.parse_type('string')
		t.tc.register_generated_fn_param_types(helper, [t.tc.parse_type(aggregate)])
		t.tc.register_generated_fn_param_types(helper_key, [
			t.tc.parse_type(aggregate),
		])
		t.tc.fn_variadic[helper] = false
		t.tc.fn_variadic[helper_key] = false
		t.tc_signature_names_log << helper
		if helper_key != helper {
			t.tc_signature_names_log << helper_key
		}
	}
}

fn (mut t Transformer) lower_ref_str_prefixed(expr flat.NodeId, aggregate string) flat.NodeId {
	return t.lower_ref_str_guarded(expr, aggregate, true, '', '&nil')
}

fn (t &Transformer) checker_selected_custom_receiver_method(call_id flat.NodeId, method string) bool {
	resolved := t.checker_selected_receiver_method_name(call_id, method) or { return false }
	receiver := resolved.all_before_last('.')
	if receiver.len > 0 && t.is_interface_type(receiver) {
		return false
	}
	return true
}

fn (mut t Transformer) lower_ref_collection_str(expr flat.NodeId, collection_type string) flat.NodeId {
	ptr_type := '&${collection_type}'
	ptr_name := t.new_temp('ref_str_ptr')
	res_name := t.new_temp('ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('&nil'), 'string')
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), collection_type)
	value_str := t.wrap_string_conversion(value, collection_type)
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	then_body << t.make_assign(t.make_ident(res_name), t.string_plus(t.make_string_literal('&'), value_str))
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) lower_ref_str_guarded(expr flat.NodeId, aggregate string, prefix_non_nil bool, str_fn string, nil_text string) flat.NodeId {
	ptr_type := '&${aggregate}'
	ptr_name := t.new_temp('ref_str_ptr')
	res_name := t.new_temp('ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal(nil_text), 'string')
	// Stringifying the pointee may hoist its own prelude (nested arrays/maps dereference the
	// pointer). Capture that prelude and keep it inside the non-nil branch so a null pointer
	// is never dereferenced.
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value_str := if str_fn.len > 0 && t.str_method_has_pointer_receiver(str_fn) {
		t.make_call_typed(str_fn, [t.make_ident(ptr_name)], 'string')
	} else if str_fn.len > 0 {
		value := t.make_prefix(.mul, t.make_ident(ptr_name))
		t.set_node_typ(int(value), aggregate)
		t.make_call_typed(str_fn, [value], 'string')
	} else {
		t.wrap_string_conversion(t.make_prefix(.mul, t.make_ident(ptr_name)), aggregate)
	}
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	non_nil := if prefix_non_nil {
		t.string_plus(t.make_string_literal('&'), value_str)
	} else {
		value_str
	}
	then_body << t.make_assign(t.make_ident(res_name), non_nil)
	if str_fn.len == 0 || str_fn.starts_with('__v3_autostr_') {
		address := t.make_cast('voidptr', t.make_ident(ptr_name), 'voidptr')
		address_type := t.make_int_literal(t.type_index_for_type_name(aggregate))
		mut live_body := [
			t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [address, address_type], 'void')),
		]
		live_body << then_body
		live_body << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], 'void'))
		circular_text := if prefix_non_nil { '&<circular>' } else { '<circular>' }
		seen_body := t.make_block([
			t.make_assign(t.make_ident(res_name), t.make_string_literal(circular_text)),
		])
		seen := t.make_call_typed('autostr_addr_type_in_stack', [address, address_type], 'bool')
		then_body = [t.make_if(seen, seen_body, t.make_block(live_body))]
	}
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) lower_interface_smartcast_ref_str(expr flat.NodeId, interface_expr flat.NodeId, aggregate string, str_fn string) flat.NodeId {
	ptr_type := '&${aggregate}'
	ptr_name := t.new_temp('ref_str_ptr')
	boxed_name := t.new_temp('ref_str_boxed')
	res_name := t.new_temp('ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	boxed := t.make_selector_op(interface_expr, '_object_is_boxed', 'bool', .dot)
	t.pending_stmts << t.make_decl_assign_typed(boxed_name, boxed, 'bool')
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('&nil'), 'string')
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value_str := if t.str_method_has_pointer_receiver(str_fn) {
		t.make_call_typed(str_fn, [t.make_ident(ptr_name)], 'string')
	} else {
		value := t.make_prefix(.mul, t.make_ident(ptr_name))
		t.set_node_typ(int(value), aggregate)
		t.make_call_typed(str_fn, [value], 'string')
	}
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	boxed_value := t.make_assign(t.make_ident(res_name), value_str)
	pointer_value := t.make_assign(t.make_ident(res_name), t.string_plus(t.make_string_literal('&'), value_str))
	then_body << t.make_if(t.make_ident(boxed_name), t.make_block([boxed_value]), t.make_block([
		pointer_value,
	]))
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) lower_array_ref_str(expr flat.NodeId, typ string) flat.NodeId {
	return t.lower_ref_value_str(expr, typ, 'nil')
}

fn (mut t Transformer) lower_ref_value_str(expr flat.NodeId, typ string, nil_text string) flat.NodeId {
	return t.lower_ref_value_str_with_custom_prefix(expr, typ, nil_text, false)
}

fn (mut t Transformer) lower_ref_value_str_with_custom_prefix(expr flat.NodeId, typ string, nil_text string, prefix_custom bool) flat.NodeId {
	if !typ.starts_with('&') {
		return t.wrap_string_conversion(expr, typ)
	}
	elem_type := typ[1..]
	if alias_name, _ := t.lookup_str_alias(elem_type) {
		if str_fn := t.alias_custom_str_method_name(alias_name) {
			t.mark_fn_used_name(str_fn)
			return t.lower_ref_str_guarded(expr, alias_name, prefix_custom, str_fn, nil_text)
		}
	}
	mut normalized_elem := t.normalize_type_alias(elem_type)
	if normalized_elem.starts_with('builtin.') {
		normalized_elem = normalized_elem.all_after_last('.')
	}
	if normalized_elem !in ['string', 'rune', 'bool', 'i8', 'i16', 'i32', 'i64', 'int', 'isize',
		'u8', 'byte', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64'] {
		if aggregate := t.stringify_aggregate_type_name(elem_type) {
			if str_fn := t.aggregate_str_method_name(aggregate) {
				t.mark_fn_used_name(str_fn)
				return t.lower_ref_str_guarded(expr, aggregate, prefix_custom, str_fn, nil_text)
			}
		}
	}
	ptr_name := t.new_temp('arr_ref_str_ptr')
	res_name := t.new_temp('arr_ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, typ)
	t.set_var_type(ptr_name, typ)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal(nil_text), 'string')
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), elem_type)
	mut value_str := t.wrap_string_conversion(value, elem_type)
	mut quote_elem := t.normalize_type_alias(elem_type)
	if quote_elem.starts_with('builtin.') {
		quote_elem = quote_elem.all_after_last('.')
	}
	if quote_elem == 'string' {
		value_str = t.string_plus(t.string_plus(t.make_string_literal("'"), value_str), t.make_string_literal("'"))
	} else if quote_elem == 'rune' {
		value_str = t.string_plus(t.string_plus(t.make_string_literal('`'), value_str), t.make_string_literal('`'))
	}
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	then_body << t.make_assign(t.make_ident(res_name), t.string_plus(t.make_string_literal('&'), value_str))
	if _ := t.stringify_aggregate_type_name(elem_type) {
		address := t.make_cast('voidptr', t.make_ident(ptr_name), 'voidptr')
		address_type := t.make_int_literal(t.type_index_for_type_name(elem_type))
		mut live_body := [
			t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [address, address_type], 'void')),
		]
		live_body << then_body
		live_body << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], 'void'))
		seen_body := t.make_block([
			t.make_assign(t.make_ident(res_name), t.make_string_literal('&<circular>')),
		])
		seen := t.make_call_typed('autostr_addr_type_in_stack', [address, address_type], 'bool')
		then_body = [t.make_if(seen, seen_body, t.make_block(live_body))]
	}
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) ref_value_str_reaches_large_circular_graph(elem_type string) bool {
	if t.stringify_stack.len == 0 {
		return false
	}
	aggregate := t.stringify_aggregate_type_name(elem_type) or { return false }
	if t.stringify_expansion_estimate(aggregate) <= recursive_pointer_str_expansion_threshold {
		return false
	}
	mut seen := map[string]bool{}
	if !t.stringify_type_reaches_stack(aggregate, mut seen) {
		return false
	}
	return !t.struct_autostr_allows_recurse(aggregate)
}

fn (t &Transformer) stringify_type_reaches_stack(typ string, mut seen map[string]bool) bool {
	mut clean := typ.trim_space()
	for {
		mut stripped := false
		for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
			if clean.starts_with(prefix) {
				clean = clean[prefix.len..].trim_space()
				stripped = true
				break
			}
		}
		if !stripped {
			break
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end > 3 && bracket_end < clean.len - 1 {
			return t.stringify_type_reaches_stack(clean[4..bracket_end], mut seen)
				|| t.stringify_type_reaches_stack(clean[bracket_end + 1..], mut seen)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 0 && bracket_end < clean.len - 1 {
			return t.stringify_type_reaches_stack(clean[bracket_end + 1..], mut seen)
		}
	}
	if aggregate := t.stringify_aggregate_type_name(clean) {
		for active in t.stringify_stack {
			if t.stringify_types_match(aggregate, active) {
				return true
			}
		}
		if seen[aggregate] {
			return false
		}
		seen[aggregate] = true
		info := t.lookup_struct_info(aggregate) or {
			t.generic_struct_info_for_stringify(aggregate) or { return false }
		}
		for field in info.fields {
			field_type := if field.typ.len > 0 { field.typ } else { field.raw_typ }
			if t.stringify_type_reaches_stack(field_type, mut seen) {
				return true
			}
		}
		return false
	}
	_, args, is_generic := generic_app_parts(clean)
	if is_generic {
		for arg in args {
			if t.stringify_type_reaches_stack(arg, mut seen) {
				return true
			}
		}
	}
	return false
}

fn stringify_fn_type_display(typ string) string {
	clean := typ.trim_space()
	if clean.starts_with('fn(') {
		return 'fn ${clean[2..]}'
	}
	return clean
}

// alias_str_wrap stringifies an alias value. V wraps the base str with `AliasName(...)` for
// aliases of aggregate types (arrays, maps, structs, sum types), e.g. `Block([1, 2])`,
// but stringifies primitive and enum aliases as the bare underlying value.
fn (mut t Transformer) alias_str_wrap(expr flat.NodeId, alias_name string, base_type string, is_ref bool) flat.NodeId {
	if is_ref {
		if str_fn := t.alias_custom_str_method_name(alias_name) {
			return t.lower_ref_str_guarded(expr, alias_name, true, str_fn, '&nil')
		}
	}
	if custom := t.alias_custom_str_call(expr, alias_name) {
		return custom
	}
	if !is_ref {
		if parent_alias, _ := t.lookup_str_alias(base_type) {
			if parent_alias != alias_name {
				if custom := t.alias_custom_str_call(expr, parent_alias) {
					return custom
				}
			}
		}
		if base_type.starts_with('&') {
			pointee_alias := base_type[1..].trim_space()
			if str_fn := t.alias_custom_str_method_name(pointee_alias) {
				return t.lower_ref_str_guarded(expr, pointee_alias, true, str_fn, '&nil')
			}
		}
	}
	resolved_base := t.alias_str_resolved_base_type(base_type)
	if is_ref && !t.is_optional_type_name(resolved_base)
		&& t.alias_str_needs_name_wrapper(base_type) {
		return t.lower_ref_str_guarded(expr, alias_name, false, '', '&nil')
	}
	inner_type := if is_ref { '&${resolved_base}' } else { resolved_base }
	inner := t.wrap_string_conversion(expr, inner_type)
	if is_ref && t.is_optional_type_name(resolved_base) {
		return inner
	}
	if t.alias_str_suppress_wrapper_for_mut_param_deref(expr) {
		return inner
	}
	if t.is_fn_stringify_type(base_type) || t.is_fn_stringify_type(resolved_base)
		|| t.is_fn_stringify_type(alias_name) {
		return inner
	}
	if int(inner) >= 0 && int(inner) < t.a.nodes.len {
		inner_node := t.a.nodes[int(inner)]
		if inner_node.kind == .string_literal && inner_node.value.starts_with('fn') {
			return inner
		}
	}
	if !t.alias_str_needs_name_wrapper(base_type) {
		return inner
	}
	display := struct_string_display_name(alias_name)
	return t.string_plus(t.string_plus(t.make_string_literal('${display}('), inner), t.make_string_literal(')'))
}

fn (t &Transformer) is_fn_stringify_type(typ string) bool {
	clean := typ.trim_space()
	short := short_name_view(clean)
	return clean.starts_with('fn(') || clean.starts_with('fn (') || clean.starts_with('_fn_ptr_')
		|| short.starts_with('_fn_ptr_') || t.is_fn_pointer_type_name(clean)
}

fn (t &Transformer) fn_stringify_display(typ string) string {
	clean := typ.trim_space()
	if clean.starts_with('_fn_ptr_') || short_name_view(clean).starts_with('_fn_ptr_') {
		return 'fn'
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean)
		if parsed is types.Alias && parsed.base_type is types.FnType {
			return t.fn_stringify_display(t.semantic_type_name(parsed.base_type))
		}
	}
	return typeof_fn_type_display(clean)
}

// alias_custom_str_call builds a call to the alias's own str() method when one exists;
// an alias-level str() overrides the base type's stringification entirely (no name wrapper).
fn (mut t Transformer) alias_custom_str_call(expr flat.NodeId, alias_name string) ?flat.NodeId {
	str_fn := t.alias_custom_str_method_name(alias_name) or { return none }
	mut receiver := expr
	if t.str_method_has_pointer_receiver(str_fn) {
		mut expr_type := t.node_type(expr)
		if expr_type.len == 0 {
			expr_type = t.resolve_expr_type(expr)
		}
		if !expr_type.starts_with('&') && !t.expr_can_take_address(expr) {
			tmp_name := t.new_temp('alias_str')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, alias_name)
			receiver = t.make_ident(tmp_name)
		}
	}
	return t.make_call_typed(str_fn, [receiver], 'string')
}

fn (mut t Transformer) alias_custom_str_method_name(alias_name string) ?string {
	if isnil(t.tc) {
		return none
	}
	mut candidates := [alias_name]
	if !alias_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${alias_name}'
	}
	for qname in candidates {
		mut alias_target := t.tc.type_aliases[qname] or { '' }
		if alias_target.len == 0 {
			alias_target = t.expand_generic_type_alias(qname) or { '' }
		}
		if alias_target.len == 0 {
			continue
		}
		str_fn := '${c_name(qname)}__str'
		v_str_fn := '${qname}.str'
		if method_name, generic_args := t.generic_str_method_specialization(v_str_fn, qname, alias_target) {
			t.mark_generic_str_method_specialization(method_name, generic_args)
			return method_name
		}
		known := str_fn in t.fn_ret_types || v_str_fn in t.fn_ret_types
			|| (!isnil(t.tc) && (str_fn in t.tc.fn_ret_types || v_str_fn in t.tc.fn_ret_types))
		if known {
			t.mark_fn_used_name(v_str_fn)
			t.mark_fn_used_name(str_fn)
			return str_fn
		}
	}
	return none
}

// lookup_str_alias resolves a type name to `(alias_name, base_type)` when it names a type
// alias, mirroring the direct/module-qualified/suffix lookups of wrap_string_conversion.
fn (t &Transformer) lookup_str_alias(clean_typ string) ?(string, string) {
	if isnil(t.tc) || clean_typ.len == 0 {
		return none
	}
	key := '${t.cur_module}\n${clean_typ}'
	if !isnil(t.str_alias_cache) {
		mut cache := t.str_alias_cache
		if target := cache.entries[key] {
			return clean_typ, target
		}
		if cache.misses[key] {
			return none
		}
	}
	if target := t.lookup_str_alias_uncached(clean_typ) {
		if !isnil(t.str_alias_cache) {
			mut cache := t.str_alias_cache
			cache.entries[key] = target
		}
		return clean_typ, target
	}
	if !isnil(t.str_alias_cache) {
		mut cache := t.str_alias_cache
		cache.misses[key] = true
	}
	return none
}

fn (t &Transformer) lookup_str_alias_uncached(clean_typ string) ?string {
	if alias := t.tc.type_aliases[clean_typ] {
		return alias
	}
	if alias := t.expand_generic_type_alias(clean_typ) {
		return alias
	}
	if !clean_typ.contains('.') {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qtyp := '${t.cur_module}.${clean_typ}'
			if alias := t.tc.type_aliases[qtyp] {
				return alias
			}
		}
		for aname, target in t.tc.type_aliases {
			if short_name_view(aname) == clean_typ {
				return target
			}
		}
	}
	return none
}

fn (t &Transformer) array_alias_stringify_type(typ string) ?string {
	mut clean := typ.trim_space()
	is_ref := clean.starts_with('&')
	if is_ref {
		clean = clean[1..]
	}
	alias_name, base_type := t.lookup_str_alias(clean) or { return none }
	base_clean := base_type.trim_space()
	if base_clean.starts_with('[]') || base_clean.starts_with('map[') {
		return if is_ref { '&${alias_name}' } else { alias_name }
	}
	resolved := t.alias_str_resolved_base_type(base_type)
	if resolved.starts_with('[]') || resolved.starts_with('map[') {
		return if is_ref { '&${alias_name}' } else { alias_name }
	}
	return none
}

fn (t &Transformer) alias_str_suppress_wrapper_for_mut_param_deref(expr flat.NodeId) bool {
	if int(expr) < 0 || int(expr) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(expr)]
	if node.kind != .prefix || node.op != .mul || node.children_count == 0 {
		return false
	}
	base := t.a.child_node(&node, 0)
	return base.kind == .ident && t.mut_param_values[base.value]
}

fn (t &Transformer) alias_str_resolved_base_type(base_type string) string {
	mut clean := base_type.trim_space()
	mut seen := []string{}
	for clean.len > 0 && clean !in seen {
		seen << clean
		next := t.normalize_type_alias(clean).trim_space()
		if next == clean {
			break
		}
		clean = next
	}
	return clean
}

fn (t &Transformer) alias_str_needs_name_wrapper(base_type string) bool {
	mut clean := t.alias_str_resolved_base_type(base_type)
	if clean.starts_with('&') {
		return false
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	if t.is_fn_stringify_type(clean) {
		return false
	}
	if t.is_enum_stringify_type(clean) {
		return false
	}
	return clean !in ['string', 'bool', 'rune', 'char', 'i8', 'i16', 'i32', 'i64', 'int', 'isize',
		'u8', 'byte', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal', 'float literal',
		'voidptr', 'byteptr', 'charptr', 'nil', 'void']
}

fn (mut t Transformer) lower_struct_str(expr flat.NodeId, struct_type string) ?flat.NodeId {
	info := t.lookup_struct_info(struct_type) or {
		t.generic_struct_info_for_stringify(struct_type) or { return none }
	}
	if struct_type.contains('.') && struct_type.all_after_last('.') == 'Array_string' {
		return t.make_string_literal('${struct_string_display_name(struct_type)}{}')
	}
	stack_count := t.stringify_stack_count(struct_type)
	recurse_limit := if t.struct_autostr_allows_recurse(struct_type) { 3 } else { 2 }
	if stack_count >= recurse_limit {
		return t.make_string_literal('<circular>')
	}
	if t.stringify_stack.len >= t.stringify_depth_cap && stack_count == 0
		&& !t.stringify_types_match(t.auto_str_synthesis_type, struct_type) {
		return t.request_auto_str_helper(expr, struct_type)
	}
	t.stringify_stack << struct_type
	defer {
		t.stringify_stack.delete_last()
	}
	if info.fields.len == 0 {
		return t.make_string_literal('${struct_string_display_name(struct_type)}{}')
	}
	expr_node := t.a.nodes[int(expr)]
	mut value_expr := expr
	mut deref_address_source := flat.empty_node
	if expr_node.kind == .prefix && expr_node.op == .mul && expr_node.children_count == 1 {
		pointer_id := t.a.child(&expr_node, 0)
		mut pointer_type := t.node_type(pointer_id)
		if pointer_type.len == 0 {
			pointer_type = '&${struct_type}'
		}
		deref_address_source = t.stable_transformed_expr_for_reuse(pointer_id, pointer_type, 'struct_str_ptr')
		value_expr = t.make_prefix(.mul, deref_address_source)
		t.set_node_typ(int(value_expr), struct_type)
	}
	base := t.stable_transformed_expr_for_reuse(value_expr, struct_type, 'struct_str')
	guard_root_address := stack_count == 0 && t.expr_can_take_address(base)
	if guard_root_address {
		mut address_source := t.make_prefix(.amp, base)
		if expr_node.kind == .ident
			&& (t.pointer_value_rvalues[expr_node.value] || t.mut_param_values[expr_node.value]) {
			address_source = t.transform_expr_preserving_pointer_value(expr)
		} else if int(deref_address_source) >= 0 {
			address_source = deref_address_source
		}
		address := t.make_cast('voidptr', address_source, 'voidptr')
		address_type := t.make_int_literal(t.type_index_for_type_name(struct_type))
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [
			address,
			address_type,
		], 'void'))
	}
	display := struct_string_display_name(struct_type)
	mut result := t.make_string_literal('${display}{\n')
	decl_metas := t.struct_field_decl_metas_in_module(comptime_struct_info_cache_key(info), info.module)
	for field in info.fields {
		if meta := decl_metas[field.name] {
			if auto_str_field_is_skipped(meta) {
				continue
			}
		}
		raw_field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		mut field_type := t.lookup_struct_field_type(struct_type, field.name) or {
			t.normalize_type_in_module(raw_field_type, info.module)
		}
		if field_type.len == 0 {
			field_type = field.typ
			if field_type.len == 0 {
				continue
			}
		}
		mut field_str := if field_type == struct_type {
			t.make_string_literal('${struct_string_display_name(field_type)}{}')
		} else {
			t.struct_field_str_value(t.make_selector(base, field.name, field_type), raw_field_type, field_type)
		}
		if raw_field_type == 'string' || raw_field_type == 'builtin.string' {
			field_str = t.string_plus(t.string_plus(t.make_string_literal("'"), field_str), t.make_string_literal("'"))
		}
		if t.struct_str_field_needs_indent(raw_field_type) {
			t.mark_fn_used_name('string.replace')
			t.mark_fn_used_name('string__replace')
			field_str = t.make_call_typed('string.replace', [field_str, t.make_string_literal('\n'),
				t.make_string_literal('\n    ')], 'string')
		}
		result = t.string_plus(result, t.make_string_literal('    ${field.name}: '))
		result = t.string_plus(result, field_str)
		result = t.string_plus(result, t.make_string_literal('\n'))
	}
	final_result := t.string_plus(result, t.make_string_literal('}'))
	if guard_root_address {
		result_name := t.new_temp('struct_str')
		t.pending_stmts << t.make_decl_assign_typed(result_name, final_result, 'string')
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], 'void'))
		return t.make_ident(result_name)
	}
	return final_result
}

fn auto_str_field_is_skipped(meta FieldDeclMeta) bool {
	for attr in comptime_attribute_metas_from_raw(meta.attrs, []int{}) {
		if attr.name == 'str' && attr.has_arg && attr.arg.trim_space() == 'skip' {
			return true
		}
	}
	return false
}

fn (t &Transformer) stringify_stack_count(typ string) int {
	mut n := 0
	for item in t.stringify_stack {
		if t.stringify_types_match(item, typ) {
			n++
		}
	}
	return n
}

fn (t &Transformer) stringify_types_match(a string, b string) bool {
	ak := t.stringify_type_key(a)
	bk := t.stringify_type_key(b)
	return ak == bk || ak.all_after_last('.') == bk.all_after_last('.')
}

fn (t &Transformer) stringify_type_key(typ string) string {
	mut clean := typ.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	normalized := t.normalize_type_alias(clean)
	if normalized != clean {
		clean = normalized
	}
	if aggregate := t.stringify_aggregate_type_name(clean) {
		return aggregate
	}
	return clean
}

fn (t &Transformer) stringify_type_at_circular_limit(typ string) bool {
	aggregate := t.stringify_aggregate_type_name(typ) or { return false }
	limit := if t.struct_autostr_allows_recurse(aggregate) { 3 } else { 2 }
	return t.stringify_stack_count(aggregate) >= limit
}

fn (mut t Transformer) rebuild_struct_autostr_recurse_index() {
	mut allow_nodes := map[int]bool{}
	for node in t.a.nodes {
		if node.kind != .directive || !node.value.starts_with('@attributes:') {
			continue
		}
		mut allows_recurse := false
		for attr in node.generic_params() {
			clean := attr.trim_space()
			if clean.starts_with('autostr') && clean.contains('allowrecurse') {
				allows_recurse = true
				break
			}
		}
		if allows_recurse {
			allow_nodes[node.value['@attributes:'.len..].int()] = true
		}
	}
	mut recurse_types := map[string]bool{}
	mut cur_module := ''
	for idx, node in t.a.nodes {
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind != .struct_decl || !allow_nodes[idx] {
			continue
		}
		qualified := if cur_module.len > 0 && cur_module !in ['main', 'builtin'] {
			'${cur_module}.${node.value}'
		} else {
			node.value
		}
		recurse_types[node.value] = true
		recurse_types[qualified] = true
	}
	t.struct_autostr_recurse_types = recurse_types.move()
}

fn (t &Transformer) struct_autostr_allows_recurse(struct_type string) bool {
	decl_name := generic_base_name_text(struct_type)
	return t.struct_autostr_recurse_types[decl_name]
		|| t.struct_autostr_recurse_types[decl_name.all_after_last('.')]
}

fn struct_string_display_name(typ string) string {
	if typ.all_after_last('.').starts_with('AnonStruct_') {
		return 'struct '
	}
	if typ.starts_with('main.') {
		return typ.all_after_last('.')
	}
	return typ
}

// struct_field_str_value stringifies one struct field for the auto-generated struct str.
// Unlike top-level stringification, V wraps an alias-typed field as `AliasName(value)` even
// when the alias base is primitive (`d: Duration(42)`), unless the alias defines its own
// str() method, which is used bare.
fn (mut t Transformer) struct_field_str_value(expr flat.NodeId, raw_field_type string, field_type string) flat.NodeId {
	mut clean := raw_field_type.trim_space()
	if clean.starts_with('&') {
		return t.lower_ref_value_str_with_custom_prefix(expr, field_type, '&nil', true)
	}
	if clean == 'charptr' || clean == 'builtin.charptr' {
		return t.lower_charptr_struct_field_str(expr)
	}
	// Function types retain source-only parameter metadata such as `mut` and
	// parameter names in StructField.raw_typ. The semantic FnType intentionally
	// drops that metadata for ABI checks, so format the raw declaration here.
	if t.is_fn_stringify_type(clean) {
		if alias_name, base_type := t.lookup_str_alias(clean) {
			if t.normalize_type_alias(t.alias_str_resolved_base_type(base_type)) == t.normalize_type_alias(field_type) {
				if custom := t.alias_custom_str_call(expr, alias_name) {
					return custom
				}
			}
		}
		return t.make_string_literal(t.fn_stringify_display(clean))
	}
	if clean.starts_with('?') || clean.starts_with('!') || clean.starts_with('shared ') {
		return t.wrap_string_conversion(expr, field_type)
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	alias_name, base_type := t.lookup_str_alias(clean) or {
		return t.wrap_string_conversion(expr, field_type)
	}
	if t.normalize_type_alias(t.alias_str_resolved_base_type(base_type)) != t.normalize_type_alias(field_type) {
		// A bare alias spelling can be shared by several modules (`gfx.Color` and
		// `gg.Color`). Ignore a short-name alias that does not describe this field's
		// declaring-module-resolved type.
		return t.wrap_string_conversion(expr, field_type)
	}
	if custom := t.alias_custom_str_call(expr, alias_name) {
		return custom
	}
	resolved_base := t.alias_str_resolved_base_type(base_type)
	inner := t.wrap_string_conversion(expr, resolved_base)
	if t.is_fn_stringify_type(base_type) || t.is_fn_stringify_type(resolved_base)
		|| t.is_fn_stringify_type(alias_name) {
		return inner
	}
	if int(inner) >= 0 && int(inner) < t.a.nodes.len {
		inner_node := t.a.nodes[int(inner)]
		if inner_node.kind == .string_literal && inner_node.value.starts_with('fn') {
			return inner
		}
	}
	display := struct_string_display_name(alias_name)
	return t.string_plus(t.string_plus(t.make_string_literal('${display}('), inner), t.make_string_literal(')'))
}

fn (mut t Transformer) lower_charptr_struct_field_str(expr flat.NodeId) flat.NodeId {
	ptr_name := t.new_temp('charptr_str_ptr')
	res_name := t.new_temp('charptr_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, 'charptr')
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('C""'), 'string')
	t.mark_fn_used_name('charptr.vstring')
	t.mark_fn_used_name('charptr__vstring')
	raw := t.make_call_typed('charptr.vstring', [t.make_ident(ptr_name)], 'string')
	quoted := t.string_plus(t.string_plus(t.make_string_literal('C"'), raw), t.make_string_literal('"'))
	then_body := t.make_block([t.make_assign(t.make_ident(res_name), quoted)])
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, then_body, t.make_empty())
	return t.make_ident(res_name)
}

// struct_str_field_needs_indent reports whether a struct auto-str field value can span
// multiple lines (nested structs, arrays of structs, options, sum types), so its stringified
// text must be re-indented one level deeper, matching V's indent_count handling. Values that
// are always single-line (primitives, plain strings, enums, custom str() output) keep their
// text untouched — V does not re-indent multi-line string content or custom str() results.
fn (mut t Transformer) struct_str_field_needs_indent(field_type string) bool {
	mut clean := field_type.trim_space()
	for clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		clean = clean[1..].trim_space()
	}
	for clean.starts_with('shared ') {
		clean = clean[7..].trim_space()
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	if _, _ := t.lookup_str_alias(clean) {
		return false
	}
	resolved := t.alias_str_resolved_base_type(clean)
	if !t.alias_str_needs_name_wrapper(resolved) {
		return false
	}
	if resolved in t.enum_types {
		return false
	}
	if !resolved.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && '${t.cur_module}.${resolved}' in t.enum_types {
		return false
	}
	if aggregate := t.stringify_aggregate_type_name(resolved) {
		str_fn := '${c_name(aggregate)}__str'
		v_str_fn := '${aggregate}.str'
		if str_fn in t.fn_ret_types || v_str_fn in t.fn_ret_types
			|| (!isnil(t.tc) && (str_fn in t.tc.fn_ret_types || v_str_fn in t.tc.fn_ret_types)) {
			return false
		}
	}
	return true
}

// lower_multi_return_str formats a multi-return value as `(a, b, ...)`.
fn (mut t Transformer) lower_multi_return_str(expr flat.NodeId, multi types.MultiReturn, typ string) flat.NodeId {
	mut item_types := multi.types.clone()
	mut concrete_type := typ
	if int(expr) >= 0 && int(expr) < t.a.nodes.len {
		if concrete_items := t.find_multi_return_call_types(t.a.nodes[int(expr)], multi.types.len) {
			item_types = concrete_items.clone()
			concrete_type = t.multi_return_type_name(concrete_items)
		}
	}
	base := t.stable_transformed_expr_for_reuse(expr, concrete_type, 'multi_ret_str')
	mut result := t.make_string_literal('(')
	for i, item in item_types {
		if i > 0 {
			result = t.string_plus(result, t.make_string_literal(', '))
		}
		item_type := t.semantic_type_name(item)
		mut item_str := t.wrap_string_conversion(t.make_selector(base, 'arg${i}', item_type), item_type)
		if item is types.String {
			item_str = t.string_plus(t.string_plus(t.make_string_literal("'"), item_str), t.make_string_literal("'"))
		} else if item is types.Rune {
			item_str = t.string_plus(t.string_plus(t.make_string_literal('`'), item_str), t.make_string_literal('`'))
		}
		result = t.string_plus(result, item_str)
	}
	return t.string_plus(result, t.make_string_literal(')'))
}

// stringify_expansion_estimate returns an upper bound on the number of AST nodes
// the inline autostr lowering emits when interpolating a value of `typ`, at the
// configured nesting cap. It mirrors the lowering's shape: a scalar or a type
// with a custom str() method is a bounded leaf (0 — its lowering is a single
// call/conversion and never recurses), whereas an auto-generated struct/sum str
// recurses into fields/variants. The bound ignores the per-type circular guard,
// so it only ever over-counts, and it is memoized by (aggregate, depth) so it
// stays cheap even on richly cross-referential graphs like v1's ast package.
// The estimate is 0 for every type v3 self-host interpolates, so self-host cost
// and node numbering are unchanged.
fn (mut t Transformer) stringify_expansion_estimate(typ string) int {
	return t.stringify_expansion_estimate_at(typ, t.stringify_depth_cap)
}

fn (mut t Transformer) stringify_expansion_estimate_at(typ string, depth_left int) int {
	mut clean := typ.trim_space()
	for {
		mut stripped := false
		for prefix in ['mut ', 'shared ', 'atomic ', '...', '?', '!', '[]', '&'] {
			if clean.starts_with(prefix) {
				clean = clean[prefix.len..].trim_space()
				stripped = true
				break
			}
		}
		if !stripped {
			break
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end > 3 && bracket_end < clean.len - 1 {
			return t.stringify_expansion_estimate_at(clean[4..bracket_end], depth_left) + t.stringify_expansion_estimate_at(clean[bracket_end + 1..], depth_left)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 0 && bracket_end < clean.len - 1 {
			return t.stringify_expansion_estimate_at(clean[bracket_end + 1..], depth_left)
		}
	}
	agg := t.stringify_aggregate_type_name(clean) or { return 0 }
	resolved_sum := t.resolve_sum_name(agg)
	is_sum := resolved_sum in t.sum_types
	// A struct with a custom/generated str() method that survives markused lowers
	// to a bounded call, not an inline expansion. Sum types get no such shortcut:
	// interpolating a sum value inside a smartcast branch lowers the *variant's*
	// value directly, which bypasses the sum's own str() and inline-expands the
	// variant struct (this is exactly how v1's `match expr { ... }` bodies blow
	// up on ast.Expr). A str method that markused will prune also falls back to
	// inline expansion, so require the method to still be reachable.
	if !is_sum {
		if str_fn := t.aggregate_str_method_name(agg) {
			if !t.has_used_fn_filter() || t.used_fn_contains_name(str_fn)
				|| t.used_fn_contains_name(c_name(str_fn)) {
				return 0
			}
		}
	}
	if depth_left <= 0 {
		return 1
	}
	key := '${agg}|${depth_left}'
	if cached := t.str_expansion_memo[key] {
		return cached
	}
	// Provisional memo entry so a self-referential type at the same depth does not
	// recurse forever (fields always descend at depth_left-1, so this is belt-and-
	// suspenders against odd alias cycles).
	t.str_expansion_memo[key] = 0
	mut total := 6
	if variants := t.sum_types[resolved_sum] {
		for variant in variants {
			total += 10 + t.stringify_expansion_estimate_at(variant, depth_left - 1)
		}
	} else if info := t.lookup_struct_info(agg) {
		for field in info.fields {
			field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
			if field_type.len == 0 {
				continue
			}
			total += 8 + t.stringify_expansion_estimate_at(field_type, depth_left - 1)
		}
	}
	t.str_expansion_memo[key] = total
	return total
}

// fn_span_interp_estimate sums the expansion reserve over every string
// interpolation in a function's parsed subtree [lo, hi), and separately reports
// whether unresolved or inline auto-str work requires serial deferred lowering.
// A part whose value type cannot be resolved at collection time is treated as
// heavy: the transform can still resolve it to an aggregate (e.g. through a
// smartcast or a method return) and inline-expand it, which must not land in a
// bounded worker region. Scalar / str-method interpolations reserve their bounded
// joins and temporary declarations while remaining eligible for parallel lowering.
fn (mut t Transformer) fn_span_interp_estimate(lo int, hi int) (int, bool) {
	mut est := 0
	mut needs_deferred_lowering := false
	mut has_comptime_for := false
	mut has_interp := false
	for idx in lo .. hi {
		if idx < 0 || idx >= t.a.nodes.len {
			continue
		}
		node := t.a.nodes[idx]
		if node.kind == .comptime_for {
			has_comptime_for = true
		}
		if node.kind != .string_interp {
			continue
		}
		has_interp = true
		interp_est, interp_needs_deferred_lowering := t.string_interp_expansion_estimates(node)
		est += interp_est
		needs_deferred_lowering = needs_deferred_lowering || interp_needs_deferred_lowering
	}
	// A reflected `$for` substitutes its loop metadata after this estimate. An
	// interpolation that looks bounded here can become a large aggregate in every
	// iteration, so keep it out of fixed `.nogrow` worker regions.
	if has_comptime_for && has_interp {
		if est == 0 {
			est = unresolved_interp_expansion_estimate
		}
		return est, true
	}
	return est, needs_deferred_lowering
}

fn (mut t Transformer) string_interp_expansion_estimates(node flat.Node) (int, bool) {
	// transform_string_interp joins every part after the first with string__plus.
	// Each join appends two nodes and three child IDs, so charge the larger pool.
	mut estimate := if node.children_count > 1 { 3 * (int(node.children_count) - 1) } else { 0 }
	mut may_hoist := false
	mut needs_deferred_lowering := false
	for ci in 0 .. int(node.children_count) {
		part_id := t.a.child(&node, ci)
		mut expr_id := part_id
		part := t.a.nodes[int(part_id)]
		mut format := ''
		if part.kind == .directive && part.value == 'string_interp_format'
			&& part.children_count > 0 {
			expr_id = t.a.child(&part, 0)
			format = part.typ
		}
		expr_needs_deferred_lowering := t.string_interp_expr_needs_deferred_lowering(expr_id)
		may_hoist = may_hoist || expr_needs_deferred_lowering
			|| t.string_interp_expr_may_hoist(expr_id)
		needs_deferred_lowering = needs_deferred_lowering || expr_needs_deferred_lowering
		if format == 'p' {
			// Pointer formatting lowers directly to bounded ptr_str work, regardless of
			// the pointee's aggregate auto-string expansion.
			continue
		}
		part_expr := t.a.nodes[int(expr_id)]
		// Literal segments of the interpolation are always plain strings.
		if part_expr.kind in [.string_literal, .int_literal, .float_literal, .bool_literal,
			.char_literal] {
			continue
		}
		typ := t.reliable_stringify_type(expr_id)
		if typ.len == 0 || typ == 'unknown' {
			estimate += unresolved_interp_expansion_estimate
			needs_deferred_lowering = true
		} else {
			if t.is_optional_type_name(typ.trim_space()) {
				// Option/result conversion emits a prelude and conditional through
				// pending_stmts even when its scalar payload has no stringify expansion.
				needs_deferred_lowering = true
			}
			stringify_estimate := t.stringify_expansion_estimate(typ)
			estimate += stringify_estimate
			needs_deferred_lowering = needs_deferred_lowering || stringify_estimate > 0
		}
	}
	if may_hoist {
		// Once one part emits pending statements, transform_string_interp binds
		// every part to a temp so evaluation order remains stable.
		estimate += int(node.children_count) * string_interp_hoisted_part_expansion_estimate
	}
	return estimate, needs_deferred_lowering
}

fn (mut t Transformer) string_interp_expr_needs_deferred_lowering(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal && string_literal_has_unescaped_interp_start(node.value) {
		// Nested interpolation is encoded inside the literal text. Transforming it can
		// synthesize conditionals and pending statements that are absent from this AST.
		return true
	}
	raw_type := t.reliable_stringify_type(id).trim_space()
	clean_type := t.normalize_type_alias(raw_type)
	if clean_type.starts_with('[]') || t.is_fixed_array_type(clean_type) {
		// Collection stringification always synthesizes a loop and temporaries.
		return true
	}
	iface_name := t.resolve_interface_type_name(clean_type)
	if iface_name.len > 0 {
		// Interfaces without a concrete str dispatch expand through every boxed
		// implementation and emit their result tree through pending statements.
		str_key := '${iface_name}.str'
		has_bounded_str := !isnil(t.tc)
			&& ('str' in t.tc.interface_abstract_method_names(iface_name)
				|| str_key in t.fn_ret_types || str_key in t.tc.fn_ret_types)
		if !has_bounded_str {
			return true
		}
	}
	if clean_type.starts_with('map[') {
		if _ := t.resolve_receiver_method_for_type(raw_type, 'str') {
			// A user-defined map str method remains a bounded call.
		} else {
			key_type, raw_value_type := t.map_type_parts(clean_type)
			fixed_value_type := t.fixed_array_map_value_type_text(raw_value_type)
			value_type := if fixed_value_type.len > 0 { fixed_value_type } else { raw_value_type }
			if t.map_str_types_need_typed_lowering(key_type, value_type) {
				return true
			}
		}
	}
	if t.runtime_type_metadata_call_expands(id, node) {
		return true
	}
	if node.kind == .call {
		if info := t.compiler_default_clone_call_info(node) {
			if info.can_lower {
				return true
			}
		}
		if t.compiler_collection_clone_call_expands(node) {
			return true
		}
	}
	if node.kind == .is_expr || t.external_equality_expands_from_type_metadata(node) {
		// These predicates can expand from interface implementations or struct
		// fields that are not represented by their physical AST children.
		return true
	}
	if node.kind == .as_expr && node.children_count > 0 {
		source_id := t.a.child(&node, 0)
		mut source_type := t.raw_expr_type_without_smartcast(source_id)
		if source_type.len == 0 {
			source_type = t.node_type(source_id)
		}
		source_iface := t.resolve_interface_type_name(source_type)
		target_iface := t.resolve_interface_type_name(node.value)
		if source_iface.len > 0 && target_iface.len > 0 && source_iface != target_iface {
			return true
		}
	}
	if node.kind == .selector && t.external_selector_expands_from_type_metadata(node) {
		return true
	}
	for i in 0 .. node.children_count {
		if t.string_interp_expr_needs_deferred_lowering(t.a.child(&node, i)) {
			return true
		}
	}
	return false
}

fn string_literal_has_unescaped_interp_start(value string) bool {
	if value.len < 2 {
		return false
	}
	for i in 0 .. value.len - 1 {
		if value[i] == `$` && value[i + 1] == `{` && !nested_interp_start_is_escaped(value, i) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) runtime_type_metadata_call_expands(id flat.NodeId, node flat.Node) bool {
	if node.kind != .call {
		return false
	}
	if _ := t.enum_from_string_info(id) {
		return true
	}
	if node.children_count != 1 {
		return false
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 || int(fn_id) >= t.a.nodes.len {
		return false
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value !in ['type_name', 'type_idx'] || fn_node.children_count == 0 {
		return false
	}
	base_id := t.a.child(&fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if base_type.len == 0 {
		base_type = t.raw_checker_node_type(base_id)
	}
	if base_type.len == 0 {
		return false
	}
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	_, variants := t.concrete_sum_name_and_variants(clean_type)
	return variants.len > 0 || t.resolve_interface_type_name(clean_type).len > 0
}

fn (t &Transformer) string_interp_expr_may_hoist(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal] {
		return false
	}
	if t.is_optional_type_name(t.reliable_stringify_type(id).trim_space()) {
		return true
	}
	if node.kind == .ident {
		if t.source_parent_ids.len > 0 {
			return t.local_binding_before(node.value, id) or { false }
		}
		return t.raw_var_type(node.value).trim_space().starts_with('shared ')
	}
	if node.kind == .as_expr && interface_pattern_is_collapsed_container_type(node.value) {
		// Interface-to-container casts emit a runtime type check before producing the
		// value, so interpolation lowering hoists every part to preserve evaluation order.
		return true
	}
	if node.kind == .cast_expr {
		target_type := t.normalize_type_alias(node.value)
		if target_type.starts_with('&')
			&& t.is_sum_type_name(t.normalize_type_alias(target_type[1..])) {
			// Taking a pointer to a sum materializes the wrapped value in a temporary.
			return true
		}
	}
	if node.kind in [.paren, .cast_expr, .as_expr, .prefix, .postfix, .selector] {
		for i in 0 .. node.children_count {
			if t.string_interp_expr_may_hoist(t.a.child(&node, i)) {
				return true
			}
		}
		return false
	}
	// Calls, indexing, control-flow expressions, and collection/aggregate lowering
	// can all append pending statements depending on their resolved types.
	return true
}

fn (t &Transformer) stringify_aggregate_type_name(typ string) ?string {
	mut clean := typ.trim_space()
	if clean.len == 0 {
		return none
	}
	// Main-module declarations are stored under their bare names, while checker-resolved
	// generic parameter types can retain a `main.` qualifier after specialization.
	if clean.starts_with('main.') && !clean['main.'.len..].contains('.') {
		short := clean['main.'.len..]
		if short in t.structs || short in t.sum_types
			|| (!isnil(t.tc) && (short in t.tc.structs || short in t.tc.sum_types)) {
			clean = short
		}
	}
	base, args, is_generic := generic_app_parts(clean)
	if is_generic && args.len > 0 && t.generic_aggregate_base_exists(base, args.len) {
		return clean
	}
	if clean.contains('.') {
		if !isnil(t.tc) && (clean in t.tc.structs || clean in t.tc.sum_types) {
			return clean
		}
		return none
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.structs || qname in t.sum_types {
			return qname
		}
		if !isnil(t.tc) && (qname in t.tc.structs || qname in t.tc.sum_types) {
			return qname
		}
	}
	// A bare type in main belongs to main when a main declaration exists. Imported
	// types also populate qualified_types by short name, and consulting that index
	// first can redirect a local `Chunk` to an imported `ui.Chunk` during auto-str
	// lowering.
	if clean in t.structs || clean in t.sum_types {
		return clean
	}
	if !isnil(t.tc) && (clean in t.tc.structs || clean in t.tc.sum_types) {
		return clean
	}
	if qualified := t.qualified_types[clean] {
		if qualified in t.structs || qualified in t.sum_types {
			return qualified
		}
		if !isnil(t.tc) && (qualified in t.tc.structs || qualified in t.tc.sum_types) {
			return qualified
		}
	}
	return none
}

fn (t &Transformer) generic_specialized_source_type_name(typ string) ?string {
	mut clean := typ.trim_space()
	mut pointer_prefix := ''
	for clean.starts_with('&') {
		pointer_prefix += '&'
		clean = clean[1..].trim_space()
	}
	args := t.recorded_generic_specialization_args(clean) or {
		t.decode_single_generic_specialized_type_args(clean) or { return none }
	}
	if args.len == 0 {
		return none
	}
	if !isnil(t.tc) {
		for base, params in t.tc.struct_generic_params {
			if params.len == args.len
				&& generic_specialized_type_matches_flat_name(clean, base, args) {
				return pointer_prefix + generic_specialized_source_type_name_for_base(base, args)
			}
		}
		for base, params in t.tc.sum_generic_params {
			if params.len == args.len
				&& generic_specialized_type_matches_flat_name(clean, base, args) {
				return pointer_prefix + generic_specialized_source_type_name_for_base(base, args)
			}
		}
	}
	return none
}

fn (t &Transformer) decode_single_generic_specialized_type_args(name string) ?[]string {
	if isnil(t.tc) {
		return none
	}
	for base, params in t.tc.struct_generic_params {
		if params.len != 1 {
			continue
		}
		if args := decode_single_generic_specialized_type_args_for_base(name, base) {
			return args
		}
	}
	for base, params in t.tc.sum_generic_params {
		if params.len != 1 {
			continue
		}
		if args := decode_single_generic_specialized_type_args_for_base(name, base) {
			return args
		}
	}
	return none
}

fn decode_single_generic_specialized_type_args_for_base(name string, base string) ?[]string {
	short_base := base.all_after_last('.')
	for prefix in [base, c_name(base), short_base, c_name(short_base)] {
		if !name.starts_with('${prefix}_') {
			continue
		}
		decoded := generic_type_arg_from_suffix_with_containers(name[prefix.len + 1..])
		if decoded.len > 0 && generic_specialized_type_matches_flat_name(name, base, [
			decoded,
		]) {
			return [decoded]
		}
	}
	return none
}

fn (t &Transformer) generic_aggregate_base_exists(base string, arg_count int) bool {
	if isnil(t.tc) {
		return false
	}
	if params := t.tc.struct_generic_params[base] {
		return params.len == arg_count
	}
	if params := t.tc.sum_generic_params[base] {
		return params.len == arg_count
	}
	if base.starts_with('main.') {
		short := base['main.'.len..]
		if params := t.tc.struct_generic_params[short] {
			return params.len == arg_count
		}
		if params := t.tc.sum_generic_params[short] {
			return params.len == arg_count
		}
	}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${base}'
		if params := t.tc.struct_generic_params[qname] {
			return params.len == arg_count
		}
		if params := t.tc.sum_generic_params[qname] {
			return params.len == arg_count
		}
	}
	return false
}

fn generic_specialized_type_matches_flat_name(flat_name string, base string, args []string) bool {
	if flat_name.len == 0 || base.len == 0 || args.len == 0 {
		return false
	}
	mut candidates := []string{}
	source_name := generic_specialized_source_type_name_for_base(base, args)
	add_generic_specialized_type_candidate(mut candidates, source_name)
	source_cname := c_name(source_name)
	add_generic_specialized_type_candidate(mut candidates, source_cname)
	// A specialized callee is decoded from its C spelling before live-argument
	// inference. That restores module separators inside a flattened aggregate
	// (`Box_mod__Type` -> `Box_mod.Type`) without restoring the generic brackets.
	add_generic_specialized_type_candidate(mut candidates, source_cname.replace('__', '.'))
	suffix := generic_type_suffixes(args)
	add_generic_specialized_type_candidate(mut candidates, '${base}_${suffix}')
	add_generic_specialized_type_candidate(mut candidates, c_name('${base}_${suffix}'))
	if base.contains('.') {
		module_name := base.all_before_last('.')
		short_base := base.all_after_last('.')
		if module_name == 'main' {
			add_generic_specialized_type_candidate(mut candidates, '${short_base}_${suffix}')
		}
		add_generic_specialized_type_candidate(mut candidates, '${module_name}.${short_base}_${suffix}')
		add_generic_specialized_type_candidate(mut candidates, c_name('${module_name}.${short_base}_${suffix}'))
	}
	return flat_name in candidates
}

fn add_generic_specialized_type_candidate(mut candidates []string, candidate string) {
	clean := candidate.trim_space()
	if clean.len > 0 && clean !in candidates {
		candidates << clean
	}
}

fn generic_specialized_source_type_name_for_base(base string, args []string) string {
	display_base := if base.starts_with('main.') { base.all_after_last('.') } else { base }
	return '${display_base}[${args.join(', ')}]'
}

fn (t &Transformer) source_type_name_from_c_name(typ string) ?string {
	clean := typ.trim_space()
	if clean.len == 0 || !clean.contains('__') || t.type_name_is_declared(clean) {
		return none
	}
	for name, _ in t.structs {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.sum_types {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.enum_types {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	if isnil(t.tc) {
		return none
	}
	for name, _ in t.tc.structs {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.tc.sum_types {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.tc.enum_names {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	for name, _ in t.tc.type_aliases {
		if source_type_name_matches_c_name(name, clean) {
			return name
		}
	}
	return none
}

struct FixedArrayCNameParts {
	elem string
	len  string
}

fn (t &Transformer) fixed_array_source_type_from_c_name(typ string) ?string {
	clean := typ.trim_space()
	if !clean.starts_with('Array_fixed_') {
		return none
	}
	payload := clean['Array_fixed_'.len..]
	parts := t.split_fixed_array_c_name_payload(payload) or { return none }
	elem_type := t.fixed_array_c_elem_source_type(parts.elem)
	len_text := t.fixed_array_c_name_len_source(parts.len)
	return '${elem_type}[${len_text}]'
}

fn (t &Transformer) fixed_array_c_elem_source_type(elem string) string {
	if fixed := t.fixed_array_source_type_from_c_name(elem) {
		return fixed
	}
	if source := t.source_type_name_from_c_name(elem) {
		return source
	}
	return elem
}

fn (t &Transformer) split_fixed_array_c_name_payload(payload string) ?FixedArrayCNameParts {
	if payload.len == 0 {
		return none
	}
	mut fallback := FixedArrayCNameParts{}
	for i := 0; i < payload.len; i++ {
		if payload[i] != `_` {
			continue
		}
		elem := payload[..i]
		len := payload[i + 1..]
		if elem.len == 0 || len.len == 0 {
			continue
		}
		if fallback.elem.len == 0 {
			fallback = FixedArrayCNameParts{
				elem: elem
				len: len
			}
		}
		if t.fixed_array_c_name_len_is_known(len) {
			return FixedArrayCNameParts{
				elem: elem
				len: len
			}
		}
	}
	if fallback.elem.len > 0 {
		return fallback
	}
	return none
}

fn (t &Transformer) fixed_array_c_name_len_is_known(len_text string) bool {
	if is_decimal_text(len_text) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	if _ := t.const_type_key(len_text) {
		return true
	}
	return t.fixed_array_c_name_len_value(len_text) != none
}

fn (t &Transformer) fixed_array_c_name_len_source(len_text string) string {
	if value := t.fixed_array_c_name_len_value(len_text) {
		return value.str()
	}
	return len_text
}

fn (t &Transformer) fixed_array_c_name_len_value(len_text string) ?int {
	if is_decimal_text(len_text) {
		return len_text.int()
	}
	if isnil(t.tc) {
		return none
	}
	if value := t.tc.const_int_value_in_module(len_text, t.cur_module, []string{}) {
		return value
	}
	return t.tc.const_int_value(len_text, []string{})
}

fn source_type_name_matches_c_name(name string, cname string) bool {
	return name.contains('.') && c_name(name) == cname
}

fn (t &Transformer) normalize_runtime_array_stringify_type(typ string) string {
	clean := typ.trim_space()
	if clean.starts_with('Array_') && !clean.starts_with('Array_fixed_') {
		if clean in t.structs || clean in t.sum_types || clean in t.enum_types {
			return typ
		}
		if !isnil(t.tc) && clean in t.tc.type_aliases {
			return typ
		}
		decoded := t.generic_type_arg_from_suffix(clean)
		if decoded.len > 0 {
			return decoded
		}
	}
	return typ
}

fn (mut t Transformer) mark_interface_method_implementers_used(iface_name string, method string) {
	impls := t.interface_impl_index_for_transform(iface_name).names
	for concrete in impls {
		if t.has_used_fn_filter() && !t.interface_boxed_type_used(iface_name, concrete) {
			continue
		}
		concrete_method := '${concrete}.${method}'
		t.mark_fn_used_name(concrete_method)
		if method_name := t.tc.concrete_method_signature_key(concrete, method) {
			if method_name != concrete_method {
				t.mark_fn_used_name(method_name)
			}
		}
	}
}

fn (t &Transformer) interface_method_implementer_names(iface_name string, method string) []string {
	if isnil(t.tc) {
		return []string{}
	}
	impls := t.interface_impl_index_for_transform(iface_name).names
	mut names := []string{cap: impls.len * 2}
	for concrete in impls {
		concrete_method := '${concrete}.${method}'
		names << concrete_method
		if method_name := t.tc.concrete_method_signature_key(concrete, method) {
			if method_name != concrete_method {
				names << method_name
			}
		}
	}
	return names
}

fn (mut t Transformer) lower_sum_str(expr flat.NodeId, sum_name string) flat.NodeId {
	resolved_sum, variants := t.concrete_sum_name_and_variants(sum_name)
	if variants.len == 0 {
		return t.make_string_literal('${sum_name}{}')
	}
	sum_display := if resolved_sum.contains('.') {
		resolved_sum.all_after_last('.')
	} else {
		resolved_sum
	}
	// V's auto stringifier expands recursive sums far enough to show two nested
	// payload structs, then uses the same text as an invalid/zero runtime tag.
	// Stopping at the first repeated sum loses useful structure (`Expr{}` for
	// every recursive field).
	if t.stringify_stack_count(resolved_sum) >= 3 {
		return t.make_string_literal('unknown sum type value')
	}
	if t.stringify_stack.len >= t.stringify_depth_cap && t.stringify_stack_count(resolved_sum) == 0
		&& !t.stringify_types_match(t.auto_str_synthesis_type, resolved_sum) {
		return t.request_auto_str_helper(expr, resolved_sum)
	}
	t.stringify_stack << resolved_sum
	defer {
		t.stringify_stack.delete_last()
	}
	base := t.stable_transformed_expr_for_reuse(expr, resolved_sum, 'sum_str')
	tag := t.make_selector_op(base, 'typ', 'int', if sum_name.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	return t.build_sum_str_chain(base, tag, resolved_sum, sum_display, variants, 0)
}

fn (mut t Transformer) build_sum_str_chain(base flat.NodeId, tag flat.NodeId, sum_name string, sum_display string, variants []string, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.make_string_literal('unknown sum type value')
	}
	variant := variants[idx]
	field := t.sum_field_name(variant)
	variant_base := t.normalize_type_alias(variant)
	direct_pointer := t.sum_variant_is_direct_pointer(variant)
	field_sel := t.make_selector_op(base, field, if direct_pointer {
		variant
	} else {
		'&${variant}'
	}, .dot)
	t.mark_generated_variant_access(field_sel, variant)
	// Only statements created for THIS branch's payload conversion belong inside
	// the branch; earlier pending statements (e.g. the base temp decl from
	// lower_sum_str) must stay with the caller so they precede the whole if-chain.
	pending_start := t.pending_stmts.len
	mut value_text := if t.is_fixed_array_type(variant_base) {
		elem_type := fixed_array_elem_type(variant_base)
		arr := t.make_call_typed('new_array_from_c_array', [
			t.make_fixed_array_len_expr(variant_base),
			t.make_fixed_array_len_expr(variant_base),
			t.make_sizeof_type(elem_type),
			field_sel,
		], '[]${elem_type}')
		t.wrap_string_conversion(arr, '[]${elem_type}')
	} else if direct_pointer {
		t.wrap_string_conversion(field_sel, if variant_base != variant {
			variant_base
		} else {
			variant
		})
	} else {
		value := t.make_prefix(.mul, field_sel)
		payload_type := if variant_base != variant { variant_base } else { variant }
		t.set_node_typ(int(value), payload_type)
		t.wrap_string_conversion(value, payload_type)
	}
	// V prints a sum value as `SumName(payload_str)` — the payload's own str
	// already carries its type name for structs; string/rune payloads are quoted.
	// An alias variant keeps its alias-name wrapper (`Res(Ints([1, 2]))`).
	if variant_base == 'string' {
		value_text = t.string_plus(t.string_plus(t.make_string_literal("'"), value_text), t.make_string_literal("'"))
	} else if variant_base == 'rune' {
		value_text = t.string_plus(t.string_plus(t.make_string_literal('`'), value_text), t.make_string_literal('`'))
	} else if variant_base != variant {
		display := if variant.contains('.') { variant.all_after_last('.') } else { variant }
		value_text = t.string_plus(t.string_plus(t.make_string_literal('${display}('), value_text), t.make_string_literal(')'))
	}
	value_text = t.string_plus(t.string_plus(t.make_string_literal('${sum_display}('), value_text), t.make_string_literal(')'))
	mut then_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	then_stmts << t.make_expr_stmt(value_text)
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name, variant)))
	then_block := t.make_block(then_stmts)
	else_expr := t.build_sum_str_chain(base, tag, sum_name, sum_display, variants, idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
		typ: 'string'
	})
}

fn (mut t Transformer) wrap_formatted_string_conversion(expr flat.NodeId, typ string, format string) flat.NodeId {
	if format.len == 0 {
		return t.wrap_string_conversion(expr, typ)
	}
	if format.contains('X') {
		// Uppercase hex behaves like lowercase `x` but with A-F. Format the
		// lowercase form first so width/zero-padding flags are honored, then
		// upper-case the whole result.
		lowered := t.wrap_formatted_string_conversion(expr, typ, format.replace('X', 'x'))
		return t.make_call_typed('v3_string_upper_ascii', [lowered], 'string')
	}
	mut clean_typ := typ
	if clean_typ.starts_with('&') {
		clean_typ = clean_typ[1..]
	}
	for clean_typ.starts_with('atomic ') {
		clean_typ = clean_typ[7..].trim_space()
	}
	if clean_typ.starts_with('builtin.') {
		clean_typ = clean_typ.all_after_last('.')
	}
	mut normalized_typ := typ
	for _ in 0 .. 1000 {
		next := t.normalize_type_alias(normalized_typ)
		if next == normalized_typ {
			break
		}
		normalized_typ = next
	}
	if repeat_count, upper := string_repeat_format(format) {
		if clean_typ == 'string' || normalized_typ == 'string' {
			t.mark_fn_used('string__repeat')
			repeated := t.make_call_typed('string__repeat', [expr, t.make_int_literal(repeat_count)], 'string')
			return if upper {
				t.make_call_typed('v3_string_upper_ascii', [repeated], 'string')
			} else {
				repeated
			}
		}
	}
	if format == 'p' && (normalized_typ.starts_with('&')
		|| clean_typ in ['voidptr', 'byteptr', 'charptr']
		|| t.expr_is_mut_param_pointer_value(expr)) {
		return t.make_call_typed('ptr_str', [expr], 'string')
	}
	if normalized_typ.starts_with('&') && format != 'p' {
		elem_type := normalized_typ[1..]
		if t.expr_is_shared_value(expr) {
			return t.wrap_formatted_string_conversion(expr, elem_type, format)
		}
		if format == 's' {
			return t.lower_pointer_format_s(expr, normalized_typ, elem_type)
		}
		value := t.make_prefix(.mul, expr)
		t.set_node_typ(int(value), elem_type)
		return t.wrap_formatted_string_conversion(value, elem_type, format)
	}
	if dyn := t.dynamic_format_conversion(expr, typ, clean_typ, format) {
		return dyn
	}
	// An enum with a base (`x`/`b`/`o`) or decimal (`d`) verb prints its integer
	// value, not its name; format the underlying integer. Unsigned-backed enums use
	// u64 so values above i64.max are not rendered as negative.
	if format[format.len - 1] in [`x`, `b`, `o`, `d`] && t.is_formatted_enum_type(clean_typ) {
		int_type := if t.enum_backing_is_unsigned(clean_typ) { 'u64' } else { 'i64' }
		return t.wrap_formatted_string_conversion(t.make_cast(int_type, expr, int_type), int_type, format)
	}
	if decimal_format := fixed_decimal_format(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast('f64', expr, 'f64')
			}
			mut formatted := t.make_call_typed('v3_f64_fixed', [arg,
				t.make_int_literal(decimal_format.precision)], 'string')
			if decimal_format.width > 0 || decimal_format.left {
				left := if decimal_format.left { 1 } else { 0 }
				formatted = t.make_call_typed('v3_string_pad', [formatted,
					t.make_int_literal(decimal_format.width), t.make_int_literal(left)], 'string')
			}
			return formatted
		}
	}
	if exp_format := exponent_decimal_format(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast('f64', expr, 'f64')
			}
			mut formatted := t.make_call_typed('v3_f64_exp', [arg,
				t.make_int_literal(exp_format.precision), t.make_int_literal(if exp_format.upper {
					1
				} else {
					0
				})], 'string')
			if exp_format.width > 0 || exp_format.left {
				left := if exp_format.left { 1 } else { 0 }
				formatted = t.make_call_typed('v3_string_pad', [formatted,
					t.make_int_literal(exp_format.width), t.make_int_literal(left)], 'string')
			}
			return formatted
		}
	}
	if format in ['g', 'G'] && clean_typ in ['f32', 'f64', 'float_literal'] {
		fn_name := if clean_typ == 'f32' { 'f32__strg' } else { 'f64__strg' }
		arg := if clean_typ == 'float_literal' {
			t.make_cast('f64', expr, 'f64')
		} else {
			expr
		}
		t.mark_fn_used(fn_name)
		formatted := t.make_call_typed(fn_name, [arg], 'string')
		return if format == 'G' {
			t.make_call_typed('v3_string_upper_ascii', [formatted], 'string')
		} else {
			formatted
		}
	}
	if general_format := general_float_format(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast('f64', expr, 'f64')
			}
			mut formatted := t.make_call_typed('v3_f64_general', [arg,
				t.make_int_literal(general_format.precision), t.make_int_literal(if general_format.upper {
					1
				} else {
					0
				})], 'string')
			if general_format.width > 0 || general_format.left {
				left := if general_format.left { 1 } else { 0 }
				formatted = t.make_call_typed('v3_string_pad', [formatted,
					t.make_int_literal(general_format.width), t.make_int_literal(left)], 'string')
			}
			return formatted
		}
	}
	if char_format := character_format(format) {
		if normalized_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'byte', 'u16', 'u32',
			'u64', 'usize', 'char', 'rune'] {
			arg := if normalized_typ == 'int' {
				expr
			} else {
				t.make_cast('int', expr, 'int')
			}
			mut converted := t.make_call_typed('v3_char_string', [arg], 'string')
			if char_format.width > 1 || char_format.left {
				converted = t.make_call_typed('v3_string_pad', [converted,
					t.make_int_literal(char_format.width), t.make_int_literal(if char_format.left {
						1
					} else {
						0
					})], 'string')
			}
			return converted
		}
	}
	if base := integer_format_base(format) {
		if clean_typ in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
			arg := t.widened_unsigned_format_arg(expr, clean_typ)
			formatted := t.make_call_typed('strconv__format_uint', [arg, t.make_int_literal(base)], 'string')
			return if format == 'X' {
				t.make_call_typed('v3_string_upper_ascii', [formatted], 'string')
			} else {
				formatted
			}
		}
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
			formatted := t.make_call_typed('strconv__format_int', [expr, t.make_int_literal(base)], 'string')
			return if format == 'X' {
				t.make_call_typed('v3_string_upper_ascii', [formatted], 'string')
			} else {
				formatted
			}
		}
	}
	if base_format := zero_padded_integer_base_format(format) {
		converted := if clean_typ in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
			arg := t.widened_unsigned_format_arg(expr, clean_typ)
			t.make_call_typed('strconv__format_uint', [arg, t.make_int_literal(base_format.base)], 'string')
		} else if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
			arg := if clean_typ == 'i64' {
				expr
			} else {
				t.make_cast('i64', expr, 'i64')
			}
			t.make_call_typed('strconv__format_int', [arg, t.make_int_literal(base_format.base)], 'string')
		} else {
			t.wrap_string_conversion(expr, typ)
		}
		return t.make_call_typed('v3_string_zpad', [converted, t.make_int_literal(base_format.width)], 'string')
	}
	if width := left_zero_padded_decimal_width(format) {
		converted := t.wrap_formatted_string_conversion(expr, typ, 'd')
		return t.make_call_typed('v3_string_rpad_zero', [converted, t.make_int_literal(width)], 'string')
	}
	if width := zero_padded_decimal_width(format) {
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune', 'usize', 'u8', 'byte',
			'u16', 'u32', 'u64'] {
			if clean_typ in ['u64', 'usize', 'u32', 'u16', 'u8', 'byte'] {
				arg := t.widened_unsigned_format_arg(expr, clean_typ)
				return t.make_call_typed('v3_u64_zpad', [arg, t.make_int_literal(width)], 'string')
			}
			if clean_typ in ['i64', 'isize', 'i32', 'i16', 'i8', 'rune'] {
				arg := if clean_typ == 'i64' {
					expr
				} else {
					t.make_cast('i64', expr, 'i64')
				}
				return t.make_call_typed('v3_i64_zpad', [arg, t.make_int_literal(width)], 'string')
			}
			return t.make_call_typed('v3_int_zpad', [expr, t.make_int_literal(width)], 'string')
		}
	}
	if width := static_format_width(format) {
		mut converted := if base := integer_format_base_suffix(format) {
			if clean_typ in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize'] {
				arg := t.widened_unsigned_format_arg(expr, clean_typ)
				t.make_call_typed('strconv__format_uint', [arg, t.make_int_literal(base)], 'string')
			} else if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
				t.make_call_typed('strconv__format_int', [expr, t.make_int_literal(base)], 'string')
			} else {
				t.wrap_string_conversion(expr, typ)
			}
		} else {
			t.wrap_string_conversion(expr, typ)
		}
		return t.make_call_typed('v3_string_pad', [converted, t.make_int_literal(width),
			t.make_int_literal(0)], 'string')
	}
	return t.wrap_string_conversion(expr, typ)
}

fn (mut t Transformer) widened_unsigned_format_arg(expr flat.NodeId, typ string) flat.NodeId {
	if typ == 'u64' {
		return expr
	}
	name := t.new_temp('fmt_unsigned')
	t.pending_stmts << t.make_decl_assign_typed(name, expr, typ)
	return t.make_cast('u64', t.make_ident(name), 'u64')
}

fn (t &Transformer) expr_is_mut_param_pointer_value(expr flat.NodeId) bool {
	if int(expr) < 0 {
		return false
	}
	node := t.a.nodes[int(expr)]
	return node.kind == .ident && t.mut_param_values[node.value]
}

fn (t &Transformer) expr_is_shared_value(expr flat.NodeId) bool {
	if int(expr) < 0 || int(expr) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(expr)]
	if node.kind == .ident {
		return t.raw_var_type(node.value).trim_space().starts_with('shared ')
	}
	if node.kind == .selector && node.value == 'val' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		return base.kind == .ident && t.raw_var_type(base.value).trim_space().starts_with('shared ')
	}
	return false
}

fn (mut t Transformer) lower_pointer_format_s(expr flat.NodeId, typ string, elem_type string) flat.NodeId {
	ptr_name := t.new_temp('str_fmt_ptr')
	res_name := t.new_temp('str_fmt_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, typ)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal(''), 'string')
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), elem_type)
	value_str := t.string_plus(t.make_string_literal('&'), t.wrap_string_conversion(value, elem_type))
	then_body := t.make_block([t.make_assign(t.make_ident(res_name), value_str)])
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, then_body, t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) dynamic_format_conversion(expr flat.NodeId, typ string, clean_typ string, format string) ?flat.NodeId {
	if width := t.dynamic_width_expr(format) {
		converted := t.wrap_string_conversion(expr, typ)
		return t.make_call_typed('v3_string_pad', [converted, width, t.make_int_literal(0)], 'string')
	}
	if width := t.dynamic_zero_width_expr(format) {
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune', 'usize', 'u8', 'byte',
			'u16', 'u32', 'u64'] {
			converted := t.wrap_formatted_string_conversion(expr, typ, 'd')
			return t.make_call_typed('v3_string_zpad', [converted, width], 'string')
		}
	}
	if width := t.dynamic_plus_width_expr(format) {
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
			converted := t.signed_plus_string(expr, clean_typ)
			return t.make_call_typed('v3_string_pad', [converted, width, t.make_int_literal(0)], 'string')
		}
	}
	if width_name, precision_name := dynamic_float_width_precision(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast('f64', expr, 'f64')
			}
			formatted := t.make_call_typed('v3_f64_fixed', [arg, t.make_ident(precision_name)], 'string')
			return t.make_call_typed('v3_string_pad', [formatted, t.make_ident(width_name),
				t.make_int_literal(0)], 'string')
		}
	}
	return none
}

fn (mut t Transformer) dynamic_width_expr(format string) ?flat.NodeId {
	if format.len >= 3 && format[0] == `(` && format[format.len - 1] == `)` {
		name := format[1..format.len - 1]
		if is_simple_ident_name(name) {
			return t.make_ident(name)
		}
		if name.starts_with('-') && is_simple_ident_name(name[1..]) {
			return t.make_prefix(.minus, t.make_ident(name[1..]))
		}
	}
	if format.len >= 4 && format.starts_with('-(') && format[format.len - 1] == `)` {
		name := format[2..format.len - 1]
		if is_simple_ident_name(name) {
			return t.make_prefix(.minus, t.make_ident(name))
		}
	}
	return none
}

fn (mut t Transformer) dynamic_zero_width_expr(format string) ?flat.NodeId {
	if format.len >= 5 && format.starts_with('0(') && format.ends_with(')d') {
		name := format[2..format.len - 2]
		if is_simple_ident_name(name) {
			return t.make_ident(name)
		}
	}
	return none
}

fn (mut t Transformer) dynamic_plus_width_expr(format string) ?flat.NodeId {
	if format.len >= 5 && format.starts_with('+(') && format.ends_with(')d') {
		name := format[2..format.len - 2]
		if is_simple_ident_name(name) {
			return t.make_ident(name)
		}
	}
	return none
}

fn dynamic_float_width_precision(format string) ?(string, string) {
	if !format.ends_with('f') {
		return none
	}
	dot := format.index(').(') or { return none }
	if format.len < dot + 5 || format[0] != `(` || format[format.len - 2] != `)` {
		return none
	}
	width_name := format[1..dot]
	precision_name := format[dot + 3..format.len - 2]
	if !is_simple_ident_name(width_name) || !is_simple_ident_name(precision_name) {
		return none
	}
	return width_name, precision_name
}

fn (mut t Transformer) signed_plus_string(expr flat.NodeId, typ string) flat.NodeId {
	value := t.stable_transformed_expr_for_reuse(expr, typ, 'fmt_sign')
	text_name := t.new_temp('fmt_sign_text')
	base_text := t.wrap_formatted_string_conversion(value, typ, 'd')
	t.pending_stmts << t.make_decl_assign_typed(text_name, base_text, 'string')
	cond := t.make_infix(.ge, value, t.make_int_literal(0))
	then_body := t.make_block([
		t.make_assign(t.make_ident(text_name), t.string_plus(t.make_string_literal('+'), t.make_ident(text_name))),
	])
	t.pending_stmts << t.make_if(cond, then_body, t.make_empty())
	return t.make_ident(text_name)
}

struct FixedDecimalFormat {
	width     int
	precision int
	left      bool
}

struct ExponentDecimalFormat {
	width     int
	precision int
	left      bool
	upper     bool
}

struct GeneralFloatFormat {
	width     int
	precision int
	left      bool
	upper     bool
}

struct CharacterFormat {
	width int
	left  bool
}

fn character_format(format string) ?CharacterFormat {
	if format.len == 0 || format[format.len - 1] != `c` {
		return none
	}
	mut i := 0
	mut left := false
	if format[i] == `-` {
		left = true
		i++
	}
	mut width := 0
	for i < format.len - 1 {
		if format[i] < `0` || format[i] > `9` {
			return none
		}
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	return CharacterFormat{
		width: width
		left: left
	}
}

fn string_repeat_format(format string) ?(int, bool) {
	if format.len == 0 || format[format.len - 1] !in [`r`, `R`] {
		return none
	}
	mut count := 0
	for i in 0 .. format.len - 1 {
		if format[i] < `0` || format[i] > `9` {
			return none
		}
		count = count * 10 + int(format[i] - `0`)
	}
	return count, format[format.len - 1] == `R`
}

fn fixed_decimal_format(format string) ?FixedDecimalFormat {
	if format.len < 2 {
		return none
	}
	mut i := 0
	mut left := false
	if i < format.len && format[i] == `-` {
		left = true
		i++
	}
	if i < format.len && format[i] == `0` {
		i++
	}
	mut width := 0
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	if i >= format.len || format[i] != `.` {
		return none
	}
	i++
	mut precision := 0
	mut has_precision := false
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		has_precision = true
		precision = precision * 10 + int(format[i] - `0`)
		i++
	}
	if !has_precision {
		return none
	}
	if i < format.len {
		if format[i] != `f` {
			return none
		}
		i++
	} else if precision > 0 {
		precision--
	}
	if i != format.len {
		return none
	}
	return FixedDecimalFormat{
		width: width
		precision: precision
		left: left
	}
}

fn exponent_decimal_format(format string) ?ExponentDecimalFormat {
	if format.len < 2 {
		return none
	}
	mut i := 0
	mut left := false
	if i < format.len && format[i] == `-` {
		left = true
		i++
	}
	if i < format.len && format[i] == `0` {
		i++
	}
	mut width := 0
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	if i >= format.len || format[i] != `.` {
		return none
	}
	i++
	mut precision := 0
	mut has_precision := false
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		has_precision = true
		precision = precision * 10 + int(format[i] - `0`)
		i++
	}
	if !has_precision || i >= format.len || format[i] !in [`e`, `E`] {
		return none
	}
	upper := format[i] == `E`
	i++
	if i != format.len {
		return none
	}
	return ExponentDecimalFormat{
		width: width
		precision: precision
		left: left
		upper: upper
	}
}

fn general_float_format(format string) ?GeneralFloatFormat {
	if format.len < 2 {
		return none
	}
	mut i := 0
	mut left := false
	if format[i] == `-` {
		left = true
		i++
	}
	if i < format.len && format[i] == `0` {
		i++
	}
	mut width := 0
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	if i >= format.len || format[i] != `.` {
		return none
	}
	i++
	mut precision := 0
	mut has_precision := false
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		has_precision = true
		precision = precision * 10 + int(format[i] - `0`)
		i++
	}
	if !has_precision || i >= format.len || format[i] !in [`g`, `G`] {
		return none
	}
	upper := format[i] == `G`
	i++
	if i != format.len {
		return none
	}
	return GeneralFloatFormat{
		width: width
		precision: precision
		left: left
		upper: upper
	}
}

fn (t &Transformer) enum_backing_is_unsigned(clean_typ string) bool {
	mut backing := t.enum_backing_types[clean_typ] or { '' }
	if backing.len == 0 && !clean_typ.contains('.') && t.cur_module.len > 0
		&& t.cur_module !in ['main', 'builtin'] {
		backing = t.enum_backing_types['${t.cur_module}.${clean_typ}'] or { '' }
	}
	return backing in ['u8', 'byte', 'u16', 'u32', 'u64', 'usize']
}

fn (t &Transformer) is_formatted_enum_type(clean_typ string) bool {
	if clean_typ in t.enum_types {
		return true
	}
	if !clean_typ.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		return '${t.cur_module}.${clean_typ}' in t.enum_types
	}
	return false
}

fn integer_format_base(format string) ?int {
	match format {
		'b' {
			return 2
		}
		'x', 'X' {
			return 16
		}
		'o' {
			return 8
		}
		else {}
	}

	return none
}

fn integer_format_base_suffix(format string) ?int {
	if format.len == 0 {
		return none
	}
	match format[format.len - 1] {
		`b` {
			return 2
		}
		`x` {
			return 16
		}
		`o` {
			return 8
		}
		else {}
	}

	return none
}

fn static_format_width(format string) ?int {
	if format.len == 0 || format[0] == `0` {
		return none
	}
	mut i := 0
	mut sign := 1
	if format[i] == `-` {
		sign = -1
		i++
	}
	if i >= format.len || format[i] < `0` || format[i] > `9` {
		return none
	}
	mut width := 0
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	if width <= 0 {
		return none
	}
	if i == format.len {
		return sign * width
	}
	if i == format.len - 1 && format[i] in [`s`, `d`, `b`, `x`, `o`, `p`] {
		return sign * width
	}
	return none
}

struct ZeroPaddedIntegerBaseFormat {
	width int
	base  int
}

fn zero_padded_integer_base_format(format string) ?ZeroPaddedIntegerBaseFormat {
	if format.len < 3 || format[0] != `0` {
		return none
	}
	end := format.len - 1
	base := match format[end] {
		`b` { 2 }
		`x` { 16 }
		`o` { 8 }
		else {
			return none
		}
	}

	mut width := 0
	for i in 1 .. end {
		ch := format[i]
		if ch < `0` || ch > `9` {
			return none
		}
		width = width * 10 + int(ch - `0`)
	}
	if width <= 0 {
		return none
	}
	return ZeroPaddedIntegerBaseFormat{
		width: width
		base: base
	}
}

fn zero_padded_decimal_width(format string) ?int {
	if format.len < 2 || format[0] != `0` {
		return none
	}
	mut end := format.len
	if format[end - 1] == `d` {
		end--
	}
	if end >= 3 && format[end - 2] == `.` && format[end - 1] == `0` {
		end -= 2
	}
	if end <= 1 {
		return none
	}
	mut width := 0
	for i in 1 .. end {
		ch := format[i]
		if ch < `0` || ch > `9` {
			return none
		}
		width = width * 10 + int(ch - `0`)
	}
	if width <= 0 {
		return none
	}
	return width
}

fn left_zero_padded_decimal_width(format string) ?int {
	if format.len < 3 || !format.starts_with('-0') {
		return none
	}
	mut end := format.len
	if format[end - 1] == `d` {
		end--
	}
	mut width := 0
	for i in 2 .. end {
		ch := format[i]
		if ch < `0` || ch > `9` {
			return none
		}
		width = width * 10 + int(ch - `0`)
	}
	if width <= 0 {
		return none
	}
	return width
}

fn (mut t Transformer) generic_receiver_str_call(expr flat.NodeId, typ string) ?flat.NodeId {
	if isnil(t.tc) || !typ.contains('[') {
		return none
	}
	clean_typ := if typ.starts_with('&') { typ[1..] } else { typ }
	if !clean_typ.ends_with(']') {
		return none
	}
	info := t.tc.resolve_generic_struct_method(clean_typ, 'str') or {
		t.tc.resolve_generic_sum_method(clean_typ, 'str') or { return none }
	}
	if t.semantic_type_name(info.return_type) != 'string' {
		return none
	}
	// String interpolation synthesizes this call after the source-context maps
	// have been built. Encode the exact receiver specialization in the callee and
	// call metadata so a later monomorphizer instance can materialize its body.
	_, receiver_args, is_generic := generic_app_parts(clean_typ)
	mut concrete_args := []string{}
	mut inferred_method_name := ''
	if is_generic && receiver_args.len > 0 {
		method_name, inferred_args := t.generic_str_method_specialization(info.name, clean_typ, '') or {
			return none
		}
		inferred_method_name = method_name
		concrete_args = inferred_args.clone()
	}
	method_name := if concrete_args.len > 0 {
		inferred_method_name
	} else {
		'${clean_typ}.str'
	}
	t.mark_generic_str_method_specialization(method_name, concrete_args)
	call := t.make_call_typed(method_name, [expr], 'string')
	if concrete_args.len > 0 {
		t.set_node_value(int(call), concrete_args.join(', '))
	}
	return call
}

// generic_str_method_specialization infers the generic declaration arguments from the exact
// receiver pattern. In particular, `(box Box[[]T])` on `Box[[]int]` specializes T as `int`, not
// `[]int`. V1 also permits a generic str method on a concrete alias receiver; in that case its
// method arguments are taken positionally from the alias target (`Ints = []int` gives T = int).
fn (mut t Transformer) generic_str_method_specialization(fn_name string, receiver_type string, alias_target string) ?(string, []string) {
	decls := t.cached_generic_fn_decls()
	mut decl_key := generic_fn_decl_base_value(fn_name)
	decl := decls[decl_key] or {
		base, _, is_generic := generic_app_parts(receiver_type.trim_left('&'))
		lookup_base := if is_generic { base } else { receiver_type.trim_left('&') }
		decl_key = t.generic_receiver_decl_key(lookup_base, 'str', decls)
		decls[decl_key] or { return none }
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if params.len == 0 {
		return none
	}
	mut inferred := map[string]string{}
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		infer_generic_type_args(generic_inference_param_type(param), receiver_type, mut inferred)
		break
	}
	if inferred.len < params.len && alias_target.len > 0 {
		alias_args := generic_str_alias_target_args(alias_target)
		if alias_args.len == params.len {
			for i, param in params {
				if param !in inferred {
					inferred[param] = alias_args[i]
				}
			}
		}
	}
	mut concrete_args := []string{cap: params.len}
	for param in params {
		arg := inferred[param] or { return none }
		concrete_args << t.generic_arg_for_call_and_decl_module(arg, t.cur_module, decl.module)
	}
	concrete_args = t.canonical_generic_specialization_args(concrete_args)
	if concrete_args.len != params.len || t.generic_args_have_placeholders(concrete_args) {
		return none
	}
	spec_value := specialized_generic_fn_value(decl.node.value, concrete_args)
	return transform_qualified_fn_name(decl.module, spec_value), concrete_args
}

fn generic_str_alias_target_args(target string) []string {
	clean := target.trim_space()
	_, args, is_generic := generic_app_parts(clean)
	if is_generic && args.len > 0 {
		return args
	}
	if clean.starts_with('[]') && clean.len > 2 {
		return [clean[2..].trim_space()]
	}
	return []string{}
}

fn (mut t Transformer) mark_generic_str_method_specialization(method_name string, args []string) {
	t.mark_fn_used_name(method_name)
	if args.len == 0 || isnil(t.tc) {
		return
	}
	t.tc.specialized_generic_fns[method_name] = true
	t.record_generic_specialization_args_for_names([method_name, c_name(method_name)], args)
}

// append_string builds `result = result + piece` using the runtime string concat helper.
// Using string__plus directly (instead of `+=`) keeps the synthesized node independent of
// type resolution for the freshly-introduced temp.
fn (mut t Transformer) append_string(result_name string, piece flat.NodeId) flat.NodeId {
	concat := t.make_call_typed('string__plus', [t.make_ident(result_name), piece], 'string')
	return t.make_assign(t.make_ident(result_name), concat)
}

// lower_array_str expands `${arr}` for a `[]T` into a runtime loop that formats each element
// via wrap_string_conversion, so nested arrays, structs with `str`, enums, etc. all recurse
// correctly. Produces `[e0, e1, ...]`; string elements are wrapped in single quotes to match V.
fn (mut t Transformer) lower_array_str(arr_expr flat.NodeId, base_type string) flat.NodeId {
	src := t.a.nodes[int(arr_expr)]
	mut elem_type := base_type[2..]
	if recorded_elem_type := t.recorded_array_call_elem_type(arr_expr) {
		elem_type = recorded_elem_type
	}
	if src.kind == .ident {
		mut declared_type := t.raw_var_type(src.value)
		if !declared_type.starts_with('[]') {
			declared_type = t.var_type(src.value)
		}
		if declared_type.starts_with('[]') && declared_type.len > 2 {
			elem_type = declared_type[2..]
		}
	}
	// A selector's declaration spelling is more authoritative than the inferred expression
	// type here. In a module that declares `[]Event` and also imports another `Event`, the
	// latter can otherwise leak into the interpolation type and generate an array loop for
	// the wrong C struct.
	if selector := t.selector_expr_node(arr_expr) {
		mut selector_type := t.resolve_selector_type(selector)
		if selector.children_count > 0 {
			base_id := t.a.child(&selector, 0)
			mut field_base_type := t.node_type(base_id)
			if field_base_type.starts_with('&') {
				field_base_type = field_base_type[1..]
			}
			for field_base_type.starts_with('shared ') {
				field_base_type = field_base_type[7..].trim_space()
			}
			if field_type := t.lookup_struct_field_type(field_base_type, selector.value) {
				selector_type = field_type
			} else if raw_type := t.lookup_struct_field_raw_type(field_base_type, selector.value) {
				selector_type = raw_type
			}
		}
		for selector_type.starts_with('shared ') {
			selector_type = selector_type[7..].trim_space()
		}
		if selector_type.starts_with('[]') && selector_type.len > 2 {
			elem_type = selector_type[2..]
		}
	}
	base := if _ := t.generated_variant_access_type(arr_expr) {
		t.stable_transformed_expr_for_reuse(arr_expr, base_type, 'arr')
	} else {
		t.stable_expr_for_reuse(arr_expr)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('arr_str')
	idx_name := t.new_temp('arr_str_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_string_literal('['), 'string')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_name := t.new_temp('arr_str_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), elem_type)
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, elem_type)
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	// `if idx > 0 { result = result + ', ' }`
	sep_cond := t.make_infix(.gt, t.make_ident(idx_name), t.make_int_literal(0))
	sep_stmt := t.append_string(result_name, t.make_string_literal(', '))
	loop_body << t.make_if(sep_cond, t.make_block([sep_stmt]), t.make_empty())
	// element text (recurses; may push its own statements for nested arrays/optionals)
	t.set_var_type(elem_name, elem_type)
	elem_str := if t.array_elem_str_is_direct_circular(elem_type) {
		t.make_string_literal('<circular>')
	} else if elem_type.starts_with('&') {
		t.lower_array_ref_str(t.make_ident(elem_name), elem_type)
	} else {
		t.wrap_string_conversion(t.make_ident(elem_name), elem_type)
	}
	t.unset_var_type(elem_name)
	t.drain_pending(mut loop_body)
	// Quote string/rune elements, including aliases of them (`type Literal = string`).
	quote_elem := t.normalize_type_alias(elem_type)
	if quote_elem == 'string' {
		loop_body << t.append_string(result_name, t.make_string_literal("'"))
		loop_body << t.append_string(result_name, elem_str)
		loop_body << t.append_string(result_name, t.make_string_literal("'"))
	} else if quote_elem == 'rune' {
		loop_body << t.append_string(result_name, t.make_string_literal('`'))
		loop_body << t.append_string(result_name, elem_str)
		loop_body << t.append_string(result_name, t.make_string_literal('`'))
	} else {
		loop_body << t.append_string(result_name, elem_str)
	}
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	prefix << t.append_string(result_name, t.make_string_literal(']'))
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (t &Transformer) array_elem_str_is_direct_circular(elem_type string) bool {
	if t.stringify_stack.len == 0 {
		return false
	}
	aggregate := t.stringify_aggregate_type_name(elem_type) or { return false }
	current := t.stringify_stack.last()
	if !t.stringify_types_match(aggregate, current) {
		return false
	}
	return !t.struct_autostr_allows_recurse(aggregate)
}

fn (t &Transformer) recorded_array_call_elem_type(call_id flat.NodeId) ?string {
	if int(call_id) < 0 || int(call_id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(call_id)]
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind == .ident && callee.value.contains('__') {
		if !callee.value.ends_with('__array') {
			return none
		}
		receiver := callee.value.all_before_last('__')
		if args := t.recorded_generic_specialization_args(receiver) {
			if args.len == 1 {
				return args[0]
			}
		}
	}
	if callee.kind == .selector && callee.children_count > 0 {
		if callee.value != 'array' {
			return none
		}
		base_id := t.a.child(callee, 0)
		mut receiver_type := t.node_type(base_id)
		if receiver_type.starts_with('&') {
			receiver_type = receiver_type[1..]
		}
		if args := t.current_specialized_receiver_args(receiver_type) {
			if args.len == 1 {
				return args[0]
			}
		}
		if args := t.recorded_generic_specialization_args(receiver_type) {
			if args.len == 1 {
				return args[0]
			}
		}
		_, generic_args, is_generic_receiver := generic_app_parts(receiver_type)
		if is_generic_receiver && generic_args.len == 1 {
			if stringify_type_has_generic_placeholder(generic_args[0]) {
				return none
			}
			return generic_args[0]
		}
	}
	return none
}

fn (t &Transformer) current_specialized_receiver_args(receiver_type string) ?[]string {
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 || !t.generic_args_have_placeholders(args) {
		return none
	}
	current_receiver := t.current_fn_receiver_type()
	if current_receiver.len == 0
		|| !current_receiver_matches_open_generic_base(current_receiver, base) {
		return none
	}
	if recorded := t.recorded_generic_specialization_args(current_receiver) {
		if t.generic_args_have_placeholders(recorded) {
			return none
		}
		return recorded
	}
	_, current_args, current_ok := generic_app_parts(current_receiver)
	if current_ok && current_args.len == args.len {
		canonical_args := t.canonical_generic_specialization_args(current_args)
		if t.generic_args_have_placeholders(canonical_args) {
			return none
		}
		return canonical_args
	}
	return none
}

fn (t &Transformer) current_fn_receiver_type() string {
	name := t.cur_fn_name.trim_space()
	if name.len == 0 {
		return ''
	}
	if name.contains('.') {
		return name.all_before_last('.')
	}
	if name.contains('__') {
		return name.all_before_last('__')
	}
	return ''
}

fn current_receiver_matches_open_generic_base(receiver string, base string) bool {
	receiver_base, _, receiver_is_generic := generic_app_parts(receiver)
	clean_receiver := if receiver_is_generic { receiver_base } else { receiver }
	short_base := base.all_after_last('.')
	short_receiver := clean_receiver.all_after_last('.')
	return short_receiver == short_base || short_receiver.starts_with('${short_base}_')
		|| c_name(short_receiver).starts_with('${c_name(short_base)}_')
		|| short_receiver.contains('__${short_base}_')
}

fn (mut t Transformer) lower_map_str(map_expr flat.NodeId, map_type string) flat.NodeId {
	key_type, raw_value_type := t.map_type_parts(map_type)
	fixed_value_type := t.fixed_array_map_value_type_text(raw_value_type)
	value_type := if fixed_value_type.len > 0 { fixed_value_type } else { raw_value_type }
	if key_type.len == 0 || value_type.len == 0 {
		return map_expr
	}
	key_kind := t.map_str_kind_for_type(key_type)
	value_kind := t.map_str_kind_for_type(value_type)
	if t.map_str_types_need_typed_lowering(key_type, value_type) {
		return t.lower_typed_map_str(map_expr, map_type, key_type, value_type)
	}
	lowered := if t.expr_is_transformed_deref(map_expr) {
		t.stable_transformed_expr_for_reuse(map_expr, map_type, 'map_str_base')
	} else {
		t.transform_expr_for_type(map_expr, map_type)
	}
	return t.make_call_typed('v3_map_str', [lowered, t.make_int_literal(key_kind),
		t.make_int_literal(value_kind), t.make_int_literal(t.map_str_fixed_len_for_type(value_type))], 'string')
}

fn (t &Transformer) map_str_types_need_typed_lowering(key_type string, value_type string) bool {
	key_kind := t.map_str_kind_for_type(key_type)
	value_kind := t.map_str_kind_for_type(value_type)
	if key_kind != 0 && value_kind != 0 {
		return false
	}
	key_has_conversion := key_kind != 0 || t.map_str_type_has_transform_conversion(key_type)
	value_has_conversion := value_kind != 0 || t.map_str_type_has_transform_conversion(value_type)
	return key_has_conversion && value_has_conversion
}

fn (t &Transformer) expr_is_transformed_deref(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .prefix && node.op == .mul && node.children_count == 1
}

// lower_typed_map_str handles map interpolation when v3_map_str cannot stringify a typed
// key/value directly. It mirrors lower_array_str by recursing through wrap_string_conversion.
fn (mut t Transformer) lower_typed_map_str(map_expr flat.NodeId, map_type string, key_type string, value_type string) flat.NodeId {
	src := t.a.nodes[int(map_expr)]
	base := t.stable_expr_for_reuse(map_expr)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('map_str')
	keys_name := t.new_temp('map_str_keys')
	idx_name := t.new_temp('map_str_idx')
	key_name := t.new_temp('map_str_key')
	zero_name := t.new_temp('map_str_zero')
	value_name := t.new_temp('map_str_value')
	key_kind := t.map_str_kind_for_type(key_type)
	value_kind := t.map_str_kind_for_type(value_type)
	key_storage_type := t.map_key_storage_type(key_type)
	keys_type := '[]${key_storage_type}'
	keys_call := t.make_call_typed('map__keys', [t.runtime_addr(base, map_type)], keys_type)
	prefix << t.make_decl_assign_typed(result_name, t.make_string_literal('{'), 'string')
	prefix << t.make_decl_assign_typed(keys_name, keys_call, keys_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(keys_name), 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	key_expr := t.array_get_value(t.make_ident(keys_name), t.make_ident(idx_name), key_storage_type)
	key_decl := t.make_decl_assign_typed(key_name, key_expr, key_storage_type)
	zero_decl := t.make_decl_assign_typed(zero_name, t.zero_value_for_type(value_type), value_type)
	value_expr := t.make_map_get_expr(base, map_type, key_name, zero_name, value_type)
	value_decl := t.make_decl_assign_typed(value_name, value_expr, value_type)
	mut loop_body := []flat.NodeId{}
	loop_body << key_decl
	loop_body << zero_decl
	loop_body << value_decl
	sep_cond := t.make_infix(.gt, t.make_ident(idx_name), t.make_int_literal(0))
	sep_stmt := t.append_string(result_name, t.make_string_literal(', '))
	loop_body << t.make_if(sep_cond, t.make_block([sep_stmt]), t.make_empty())
	key_str := t.map_str_loop_piece(key_name, key_type, key_kind, 0)
	t.drain_pending(mut loop_body)
	loop_body << t.append_string(result_name, key_str)
	loop_body << t.append_string(result_name, t.make_string_literal(': '))
	value_str := t.map_str_loop_piece(value_name, value_type, value_kind, t.map_str_fixed_len_for_type(value_type))
	t.drain_pending(mut loop_body)
	loop_body << t.append_string(result_name, value_str)
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	prefix << t.append_string(result_name, t.make_string_literal('}'))
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (mut t Transformer) map_str_loop_piece(name string, typ string, kind int, fixed_len int) flat.NodeId {
	if kind != 0 {
		return t.make_call_typed('v3_map_str_piece', [
			t.make_prefix(.amp, t.make_ident(name)),
			t.make_int_literal(kind),
			t.make_sizeof_type(typ),
			t.make_int_literal(fixed_len),
		], 'string')
	}
	t.set_var_type(name, typ)
	piece := if typ.starts_with('&') {
		t.lower_ref_value_str(t.make_ident(name), typ, 'nil')
	} else {
		t.wrap_string_conversion(t.make_ident(name), typ)
	}
	t.unset_var_type(name)
	return piece
}

fn (t &Transformer) map_str_type_has_transform_conversion(typ string) bool {
	mut clean := t.normalize_type_alias(typ).trim_space()
	if clean.len == 0 {
		return false
	}
	if clean.starts_with('&') {
		return true
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return true
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	if t.is_optional_type_name(clean) {
		return true
	}
	if t.resolve_interface_type_name(clean).len > 0 {
		return true
	}
	if clean in ['string', 'rune', 'bool', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'u8', 'byte',
		'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal', 'float literal', 'voidptr',
		'byteptr', 'charptr', 'IError'] {
		return true
	}
	if clean in t.enum_types || clean in t.structs || clean in t.sum_types {
		return true
	}
	if _ := t.generic_struct_info_for_stringify(clean) {
		return true
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.enum_types || qname in t.structs || qname in t.sum_types {
			return true
		}
	}
	return t.is_fixed_array_type(clean) || clean.starts_with('[]') || clean.starts_with('map[')
}

fn (t &Transformer) map_str_kind_for_type(typ string) int {
	mut clean := t.normalize_type_alias(typ).trim_space()
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	match clean {
		'string' {
			return 1
		}
		'rune' {
			return 4
		}
		'isize', 'char', 'i8', 'i16', 'i32', 'i64', 'int' {
			return 2
		}
		'usize', 'u8', 'byte', 'u16', 'u32', 'u64' {
			return 3
		}
		'f32', 'f64' {
			return 5
		}
		'bool' {
			return 7
		}
		else {
			if clean.starts_with('[]') && clean[2..] in ['f32', 'f64'] {
				return 6
			}
			if transform_type_text_is_fixed_array(clean) {
				elem := t.normalize_type_alias(fixed_array_elem_type(clean))
				if elem == 'f32' {
					return 9
				}
				if elem == 'f64' {
					return 6
				}
			}
			return 0
		}
	}
}

fn (t &Transformer) map_str_fixed_len_for_type(typ string) int {
	clean := t.normalize_type_alias(typ).trim_space()
	if transform_type_text_is_fixed_array(clean) {
		return fixed_array_len(clean)
	}
	return 0
}

// wrap_optional_string_conversion transforms wrap optional string conversion data for transform.
fn (mut t Transformer) wrap_optional_string_conversion(expr flat.NodeId, typ string) flat.NodeId {
	opt_type := t.qualify_optional_type(typ)
	mut value_type := t.optional_base_type(opt_type)
	if value_type.len == 0 || value_type == 'void' {
		value_type = 'int'
	}
	opt_name := t.new_temp('opt_str')
	res_name := t.new_temp('opt_str_text')
	t.pending_stmts << t.make_decl_assign_typed(opt_name, t.transform_optional_wrapper_expr(expr), opt_type)
	pointer_payload := value_type.starts_with('&')
	option_prefix := if pointer_payload { '&Option(' } else { 'Option(' }
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('${option_prefix}none)'), 'string')
	value := t.make_selector(t.make_ident(opt_name), 'value', value_type)
	display_value := if pointer_payload {
		deref := t.make_prefix(.mul, value)
		t.set_node_typ(int(deref), value_type[1..])
		deref
	} else {
		value
	}
	display_type := if pointer_payload { value_type[1..] } else { value_type }
	mut value_str := t.wrap_string_conversion(display_value, display_type)
	if display_type == 'string' {
		value_str = t.string_plus(t.string_plus(t.make_string_literal("'"), value_str), t.make_string_literal("'"))
	}
	some_str := t.string_plus(t.string_plus(t.make_string_literal(option_prefix), value_str), t.make_string_literal(')'))
	assign_some := t.make_assign(t.make_ident(res_name), some_str)
	t.pending_stmts << t.make_if(t.make_selector(t.make_ident(opt_name), 'ok', 'bool'), t.make_block([
		assign_some,
	]), t.make_empty())
	return t.make_ident(res_name)
}

// string_plus supports string plus handling for Transformer.
fn (mut t Transformer) string_plus(left flat.NodeId, right flat.NodeId) flat.NodeId {
	return t.make_call_typed('string__plus', [left, right], 'string')
}

// is_flag_enum_type reports whether is flag enum type applies in transform.
fn (t &Transformer) is_flag_enum_type(typ string) bool {
	return t.resolve_flag_enum_type_name(typ) != none
}

fn (t &Transformer) resolve_flag_enum_type_name(typ string) ?string {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.len == 0 {
		return none
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean)
		if parsed is types.Enum {
			if parsed.is_flag {
				return parsed.name
			}
		}
		if clean in t.tc.flag_enums {
			return clean
		}
		qualified := t.tc.qualify_name(clean)
		if qualified in t.tc.flag_enums {
			return qualified
		}
		if !clean.contains('.') {
			mut found := ''
			for name, _ in t.tc.flag_enums {
				if name.all_after_last('.') != clean {
					continue
				}
				if found.len > 0 && found != name {
					return none
				}
				found = name
			}
			if found.len > 0 {
				return found
			}
		}
	}
	return none
}

fn (t &Transformer) flag_enum_mask_for_type(typ string) int {
	mut clean := typ.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	members := t.comptime_enum_members(clean)
	mut mask := 0
	for member in members {
		mask |= int(member.value)
	}
	return mask
}

fn (t &Transformer) flag_enum_has_backing_type(typ string) bool {
	mut clean := typ.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..].trim_space()
	}
	normalized := t.normalize_type_alias(clean).trim_space()
	for candidate in [normalized, clean] {
		if candidate.len == 0 {
			continue
		}
		if _ := t.enum_backing_types[candidate] {
			return true
		}
		if !candidate.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			if _ := t.enum_backing_types['${t.cur_module}.${candidate}'] {
				return true
			}
		}
	}
	return false
}

// is_runtime_array_flags_selector reports is_runtime_array_flags_selector logic in transform.
fn (t &Transformer) is_runtime_array_flags_selector(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.value != 'flags' || node.children_count == 0 {
		return false
	}
	owner_id := t.a.child(&node, 0)
	owner_type := t.node_type(owner_id).trim_left('&')
	return owner_type.starts_with('[]') || owner_type == 'strings.Builder'
}

// try_lower_flag_enum_stmt supports try lower flag enum stmt handling for Transformer.
fn (mut t Transformer) try_lower_flag_enum_stmt(call_id flat.NodeId) ?flat.NodeId {
	if int(call_id) < 0 {
		return none
	}
	call := t.a.nodes[int(call_id)]
	if call.kind != .call || call.children_count < 1 {
		return none
	}
	fn_id := t.a.children[call.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0
		|| fn_node.value !in ['set', 'clear', 'toggle', 'set_all', 'clear_all'] {
		return none
	}
	if fn_node.value in ['set', 'clear', 'toggle'] && call.children_count < 2 {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	if t.is_runtime_array_flags_selector(base_id) {
		return none
	}
	base_type := t.node_type(base_id)
	if !t.is_flag_enum_type(base_type) {
		return none
	}
	if fn_node.value == 'set_all' && t.flag_enum_has_backing_type(base_type) {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	match fn_node.value {
		'set' {
			arg := t.transform_expr(t.a.children[call.children_start + 1])
			return t.make_assign_op(base, arg, .pipe_assign)
		}
		'clear' {
			arg := t.transform_expr(t.a.children[call.children_start + 1])
			return t.make_assign_op(base, t.make_prefix(.bit_not, arg), .amp_assign)
		}
		'toggle' {
			arg := t.transform_expr(t.a.children[call.children_start + 1])
			return t.make_assign_op(base, arg, .xor_assign)
		}
		'set_all' {
			mask := t.flag_enum_mask_for_type(base_type)
			return t.make_assign(base, t.make_int_literal_typed(mask.str(), base_type))
		}
		'clear_all' {
			return t.make_assign(base, t.make_int_literal_typed('0', base_type))
		}
		else {}
	}

	return none
}

// try_lower_flag_enum_call supports try lower flag enum call handling for Transformer.
fn (mut t Transformer) try_lower_flag_enum_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 1 {
		fn_id := t.a.children[node.children_start]
		fn_node := t.a.nodes[int(fn_id)]
		if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value != 'zero' {
			return none
		}
		base_id := t.a.children[fn_node.children_start]
		base_node := t.a.nodes[int(base_id)]
		if base_node.kind != .ident {
			return none
		}
		if t.var_type(base_node.value).len > 0 {
			return none
		}
		if !isnil(t.tc) {
			if resolved := t.tc.resolved_call_name(call_id) {
				if t.is_known_fn_name(resolved) {
					return none
				}
			}
		}
		if _ := t.static_assoc_fn_name(base_id, fn_node.value) {
			return none
		}
		flag_type := t.resolve_flag_enum_type_name(base_node.value) or { return none }
		return t.make_cast(flag_type, t.make_int_literal(0), flag_type)
	}
	if node.children_count < 2 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value !in [
		'has',
		'all',
	] {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	if t.is_runtime_array_flags_selector(base_id) {
		return none
	}
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if !t.is_flag_enum_type(base_type) {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	arg_id := t.a.children[node.children_start + 1]
	arg := t.transform_expr(arg_id)
	masked := t.make_infix(.amp, base, arg)
	if fn_node.value == 'has' {
		return t.make_infix(.ne, masked, t.make_int_literal(0))
	}
	arg_copy := t.transform_expr(arg_id)
	return t.make_infix(.eq, masked, arg_copy)
}

struct CompilerDefaultCloneCallInfo {
	base_id       flat.NodeId
	raw_base_type string
	base_type     string
	can_lower     bool
}

fn (t &Transformer) compiler_default_clone_call_info(node flat.Node) ?CompilerDefaultCloneCallInfo {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value != 'clone' || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	mut raw_base_type := t.node_type(base_id)
	if raw_base_type.len == 0 {
		raw_base_type = t.lvalue_type(base_id)
	}
	mut base_type := raw_base_type
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.starts_with('[]') || base_type.starts_with('map[') || base_type == 'string'
		|| t.is_fixed_array_type(base_type) {
		return none
	}
	if isnil(t.tc) {
		return none
	}
	parsed_base_type := t.tc.parse_type(base_type)
	is_default_sum_clone := t.is_sum_type_name(base_type)
		&& t.tc.ownership_default_clone_missing_method(parsed_base_type) == none
	if !t.tc.named_type_implements_marker(base_type, 'IClone') && !is_default_sum_clone {
		return none
	}
	// A concrete generic receiver can have a handwritten clone() on its open
	// generic declaration. Leave that call intact for monomorphization instead
	// of replacing it with the compiler-provided field clone.
	if base_type.contains('[') && base_type.ends_with(']') {
		if _ := t.tc.resolve_generic_struct_method(base_type, 'clone') {
			return none
		}
	}
	// A user-defined clone() would have been lowered already; only supply the default clone.
	if t.tc.ownership_type_has_clone_method(parsed_base_type)
		&& t.resolve_receiver_method_name(base_id, 'clone').len > 0 {
		return none
	}
	return CompilerDefaultCloneCallInfo{
		base_id: base_id
		raw_base_type: raw_base_type
		base_type: base_type
		can_lower: t.tc.ownership_default_clone_missing_method(parsed_base_type) == none
	}
}

// try_lower_struct_clone_method_call lowers `x.clone()` on a struct / sum-type value to a
// plain copy of the receiver. Rust's `#[derive(Clone)]` maps to `implements IClone` in the
// ownership translation, whose `clone()` is compiler-provided; V aggregates are value types,
// so the copy is produced simply by evaluating the receiver (it is copied when assigned or
// passed by value). Collection/string clones are lowered earlier, and a user-defined clone()
// method is handled by try_lower_receiver_method_call before this runs.
fn (mut t Transformer) try_lower_struct_clone_method_call(_call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	info := t.compiler_default_clone_call_info(node) or { return none }
	// The checker reports the concrete ownership-bearing field that has no safe clone.
	// Do not turn the rejected default clone into a shallow aggregate copy while
	// transforming the invalid program.
	if !info.can_lower {
		return t.make_empty()
	}
	mut receiver := t.transform_expr(info.base_id)
	if info.raw_base_type.starts_with('&') {
		receiver = t.make_prefix(.mul, receiver)
		t.set_node_typ(int(receiver), info.base_type)
	}
	return t.make_compiler_default_clone_value(receiver, info.base_type, false)
}

// make_compiler_default_borrowed_clone_value prevents a non-owning borrowed projection from
// being cleaned up as though it were an owned rvalue after cloning reads it.
fn (mut t Transformer) make_compiler_default_borrowed_clone_value(source flat.NodeId, typ string, allow_method bool) flat.NodeId {
	mut stable_source := source
	clean := t.normalize_type_alias(typ).trim_space()
	// Transforming a borrowed projection can produce a non-addressable shallow value.
	// Stabilize every such value so clone lowering borrows it rather than mistaking it for
	// an owned rvalue that must be destroyed after cloning.
	if !t.expr_can_take_address(source) {
		source_name := t.new_temp('borrowed_clone_source')
		t.pending_stmts << t.make_decl_assign_typed(source_name, source, clean)
		stable_source = t.make_ident(source_name)
	}
	return t.make_compiler_default_clone_value(stable_source, typ, allow_method)
}

// make_compiler_default_clone_value recursively clones the storage-owning fields of a
// compiler-provided IClone value. The initial aggregate copy preserves scalar/reference
// fields, and each owning field is then replaced with its independent clone.
fn (mut t Transformer) make_compiler_default_clone_value(source flat.NodeId, typ string, allow_method bool) flat.NodeId {
	clean := t.normalize_type_alias(typ).trim_space()
	if clean.len == 0 || clean.starts_with('&') {
		return source
	}
	// Options and results both store successful values behind the `ok` flag.
	if clean.starts_with('?') || clean.starts_with('!') {
		inner := t.optional_base_type(t.qualify_optional_type(clean))
		inner_needs_work := t.compiler_default_clone_type_needs_work(inner)
		source_is_owned_temporary := !t.expr_can_take_address(source)
		stable_source := t.stable_transformed_expr_for_reuse(source, clean, 'derived_clone_opt_source')
		out_name := t.new_temp('derived_clone_opt')
		t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_optional_none(clean), clean)
		pending_start := t.pending_stmts.len
		cloned_value := if inner_needs_work {
			t.make_compiler_default_clone_value(t.make_selector(stable_source, 'value', inner), inner, true)
		} else if inner.len > 0 && inner != 'void' {
			t.make_selector(stable_source, 'value', inner)
		} else {
			t.make_empty()
		}
		mut body := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		body << t.make_assign_without_ownership_drop(t.make_ident(out_name), t.make_optional_some(cloned_value, clean))
		source_err := t.make_selector(stable_source, 'err', 'IError')
		t.mark_fn_used('string__clone')
		if !isnil(t.tc) {
			for concrete in t.tc.ierror_impl_names() {
				if clone_method := t.tc.concrete_method_signature_key(concrete, 'clone') {
					t.mark_fn_used_name(clone_method)
				}
			}
		}
		cloned_err := t.make_call_typed('__v3_clone_owned_ierror', [source_err], 'IError')
		else_branch := t.make_block_skip_scope_drops([
			t.make_assign_without_ownership_drop(t.make_ident(out_name), t.make_optional_none_with_err(clean, cloned_err)),
		])
		t.pending_stmts << t.make_if_with_skip_ownership_drops(t.make_selector(stable_source, 'ok', 'bool'), t.make_block_skip_scope_drops(body), else_branch)
		if source_is_owned_temporary {
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				stable_source,
			], 'void'))
		}
		return t.make_ident(out_name)
	}
	if clean == 'string' {
		t.mark_fn_used('string__clone')
		return t.make_call_typed('string__clone', [source], 'string')
	}
	if clean.starts_with('[]') {
		return t.make_compiler_default_array_clone_value(source, clean, !t.expr_can_take_address(source))
	}
	if t.is_fixed_array_type(clean) {
		return t.make_compiler_default_fixed_array_clone_value(source, clean)
	}
	if clean.starts_with('map[') {
		return t.make_compiler_default_map_clone_value(source, clean, !t.expr_can_take_address(source))
	}
	if allow_method && !isnil(t.tc) && t.tc.ownership_type_has_clone_method(t.tc.parse_type(clean)) {
		if !isnil(t.tc) && clean.contains('[') && clean.ends_with(']') {
			if _ := t.tc.resolve_generic_struct_method(clean, 'clone') {
				call := t.make_method_call(source, 'clone', []flat.NodeId{})
				t.set_node_typ(int(call), clean)
				return call
			}
		}
		method_name := t.resolve_receiver_method_name(source, 'clone')
		if method_name.len > 0 {
			params := t.call_param_types(method_name)
			mut receiver := source
			if params.len > 0 && t.semantic_type_name(params[0]).starts_with('&') {
				receiver = t.runtime_addr(source, clean)
			}
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, [receiver], t.receiver_method_return_type(method_name, clean))
		}
	}
	if isnil(t.tc) || (!t.tc.named_type_implements_marker(clean, 'IClone')
		&& t.tc.ownership_default_clone_missing_method(t.tc.parse_type(clean)) != none) {
		return source
	}
	if clean in t.default_clone_expansion_stack {
		return t.request_default_clone_helper(source, clean)
	}
	t.default_clone_expansion_stack << clean
	defer {
		t.default_clone_expansion_stack.delete_last()
	}
	if t.is_sum_type_name(clean) {
		// Call the shared clone helper rather than inlining the variant switch at every read
		// site: a recursive sum type such as `toml.Any` would otherwise expand a large switch
		// (one arm per variant, recursively) at each site and overflow the AST. The helper body
		// is synthesized exactly once (see build_default_clone_helper_fn).
		return t.request_default_clone_helper(source, clean)
	}
	info := t.lookup_struct_info(clean) or { return source }
	mut owning_fields := []FieldInfo{}
	for field in info.fields {
		field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		if t.compiler_default_clone_type_needs_work(field_type) {
			owning_fields << field
		}
	}
	if owning_fields.len == 0 {
		return source
	}
	// An addressable source keeps owning its fields, so the aggregate copy below is
	// only a non-owning template. A temporary source transfers its fields into the
	// aggregate and those originals must be destroyed after their clones are saved.
	source_fields_are_owned := !t.expr_can_take_address(source)
	tmp_name := t.new_temp('derived_clone')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, source, clean)
	for field in owning_fields {
		field_type := if field.typ.len > 0 { field.typ } else { field.raw_typ }
		source_field := t.make_selector(t.make_ident(tmp_name), field.name, field_type)
		mut cloned_field := t.make_compiler_default_clone_value(source_field, field_type, true)
		if source_fields_are_owned {
			cloned_name := t.new_temp('derived_clone_field')
			t.pending_stmts << t.make_decl_assign_typed(cloned_name, cloned_field, field_type)
			drop_call := t.make_call_typed('drop_owned', [source_field], 'void')
			t.pending_stmts << t.make_expr_stmt(drop_call)
			cloned_field = t.make_ident(cloned_name)
		}
		t.pending_stmts << t.make_assign_after_owned_drop(t.make_selector(t.make_ident(tmp_name), field.name, field_type), cloned_field)
	}
	return t.make_ident(tmp_name)
}

// make_compiler_default_sum_clone_value rebuilds the active variant so its boxed
// payload is independent from the source sum value.
fn (mut t Transformer) make_compiler_default_sum_clone_value(source flat.NodeId, sum_type string) flat.NodeId {
	resolved_sum := t.resolve_sum_name(sum_type)
	variants := t.sum_types[resolved_sum] or { return source }
	if variants.len == 0 {
		return source
	}
	source_is_owned_temporary := !t.expr_can_take_address(source)
	stable_source := t.stable_transformed_expr_for_reuse(source, sum_type, 'derived_clone_sum_source')
	out_name := t.new_temp('derived_clone_sum')
	t.pending_stmts << t.make_decl_assign_typed(out_name, stable_source, sum_type)
	for variant in variants {
		qvariant := t.resolve_variant(resolved_sum, variant)
		if qvariant.len == 0 {
			continue
		}
		use_ptr := t.variant_references_sum(qvariant, resolved_sum)
			&& !t.sum_variant_is_direct_pointer(qvariant)
		field_type := if use_ptr { '&${qvariant}' } else { qvariant }
		mut payload :=
			t.make_selector_op(stable_source, t.sum_field_name(qvariant), field_type, .dot)
		if use_ptr {
			payload = t.make_prefix(.mul, payload)
			t.set_node_typ(int(payload), qvariant)
		}
		pending_start := t.pending_stmts.len
		cloned_payload := t.make_compiler_default_clone_value(payload, qvariant, true)
		mut body := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		wrapped := t.make_sum_literal(resolved_sum, qvariant, cloned_payload)
		body << t.make_assign_without_ownership_drop(t.make_ident(out_name), wrapped)
		cond := t.make_sum_is_check(stable_source, sum_type, resolved_sum, qvariant)
		t.pending_stmts << t.make_if_with_skip_ownership_drops(cond, t.make_block_skip_scope_drops(body), t.make_empty())
	}
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), sum_type)
	return result
}

fn default_clone_helper_name(typ string) string {
	return '__v3_default_clone_${c_name(typ)}'
}

fn (mut t Transformer) request_default_clone_helper(source flat.NodeId, typ string) flat.NodeId {
	helper := default_clone_helper_name(typ)
	if typ !in t.default_clone_types {
		t.default_clone_types[typ] = DefaultCloneRequest{
			module: t.cur_module
			file: t.cur_file
		}
	}
	t.mark_fn_used_name(helper)
	if t.expr_can_take_address(source) {
		address := t.runtime_addr(source, typ)
		argument := t.make_cast('voidptr', address, 'voidptr')
		return t.make_call_typed(helper, [argument], typ)
	}
	// The helper only borrows its pointer argument. Stabilize an owned rvalue ourselves so
	// the clone is saved before the original temporary is destroyed; runtime_addr's ordinary
	// compiler temporary is not tracked by ownership cleanup.
	source_name := t.new_temp('default_clone_source')
	stable_source := t.make_ident(source_name)
	t.pending_stmts << t.make_decl_assign_typed(source_name, source, typ)
	address := t.runtime_addr(stable_source, typ)
	argument := t.make_cast('voidptr', address, 'voidptr')
	cloned_name := t.new_temp('default_clone_result')
	t.pending_stmts << t.make_decl_assign_typed(cloned_name, t.make_call_typed(helper, [
		argument,
	], typ), typ)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
		stable_source,
	], 'void'))
	return t.make_ident(cloned_name)
}

// synthesize_default_clone_helpers drains recursive compiler-provided IClone
// requests after worker results have been merged. Building a helper can expose
// another recursive aggregate, so requests are processed as a worklist.
fn (mut t Transformer) synthesize_default_clone_helpers() []string {
	old_module := t.cur_module
	old_file := t.cur_file
	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	was_log_active := t.used_fns_log_active
	log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	for {
		mut pending := []string{}
		for name, _ in t.default_clone_types {
			if name in t.default_clone_synthesized {
				continue
			}
			if default_clone_helper_name(name) in t.fn_ret_types {
				t.default_clone_synthesized[name] = true
				continue
			}
			pending << name
		}
		if pending.len == 0 {
			break
		}
		pending.sort()
		for name in pending {
			t.default_clone_synthesized[name] = true
			req := t.default_clone_types[name] or { DefaultCloneRequest{} }
			t.cur_module = req.module
			t.cur_file = req.file
			if !isnil(t.tc) {
				t.tc.cur_module = req.module
				t.tc.cur_file = req.file
			}
			t.build_default_clone_helper_fn(name)
		}
	}
	mut new_names := []string{}
	mut seen := map[string]bool{}
	for i in log_start .. t.used_fns_log.len {
		name := t.used_fns_log[i]
		if name.len > 0 && !seen[name] {
			seen[name] = true
			new_names << name
		}
	}
	if !was_log_active {
		t.used_fns_log_active = false
		t.used_fns_log = t.used_fns_log[..log_start].clone()
	}
	t.cur_module = old_module
	t.cur_file = old_file
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return new_names
}

fn (mut t Transformer) build_default_clone_helper_fn(typ string) {
	helper := default_clone_helper_name(typ)
	saved_pending := t.pending_stmts
	saved_vars := t.var_types.clone()
	saved_fn_name := t.cur_fn_name
	saved_ret_type := t.cur_fn_ret_type
	saved_expansion_stack := t.default_clone_expansion_stack.clone()
	t.pending_stmts = []flat.NodeId{}
	t.reset_var_types()
	t.default_clone_expansion_stack = []string{}
	t.cur_fn_name = helper
	t.cur_fn_ret_type = typ
	param_name := '__default_clone_source'
	param := t.a.add_node(flat.Node{
		kind: .param
		value: param_name
		typ: 'voidptr'
	})
	t.set_var_type(param_name, 'voidptr')
	typed_pointer := t.make_cast('&${typ}', t.make_ident(param_name), '&${typ}')
	source := t.make_prefix(.mul, typed_pointer)
	t.set_node_typ(int(source), typ)
	// The helper body inlines the clone directly; for a sum type that means the variant
	// switch itself (make_compiler_default_clone_value would otherwise route straight back
	// to this helper and never emit a body). Nested owned payloads still recurse through the
	// helper, keeping every use site compact.
	cloned := if t.is_sum_type_name(typ) {
		t.make_compiler_default_sum_clone_value(source, typ)
	} else {
		t.make_compiler_default_clone_value(source, typ, false)
	}
	mut body := t.pending_stmts.clone()
	body << t.make_return(cloned, typ)
	t.pending_stmts = saved_pending
	t.restore_var_types(saved_vars)
	t.default_clone_expansion_stack = saved_expansion_stack
	t.cur_fn_name = saved_fn_name
	t.cur_fn_ret_type = saved_ret_type
	t.add_generated_fn_decl_context('main')
	start := t.a.children.len
	t.a.children << param
	t.a.children << body
	fn_decl := t.a.add_node(flat.Node{
		kind: .fn_decl
		value: helper
		typ: typ
		children_start: i32(start)
		children_count: flat.child_count(1 + body.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, 'main', t.cur_file)
	t.set_fn_ret_type(helper, typ)
	t.mark_fn_used_name(helper)
	if !isnil(t.tc) {
		t.tc.fn_ret_types[helper] = t.tc.parse_type(typ)
		t.tc.register_generated_fn_param_types(helper, [t.tc.parse_type('voidptr')])
		t.tc.fn_variadic[helper] = false
		t.tc_signature_names_log << helper
	}
}

// make_compiler_default_array_clone_value clones the array storage and then replaces
// each owning element with an independent clone. The initial element copies are not
// owners and are deliberately overwritten without being dropped. The caller classifies
// the source lifetime before transformation can make a temporary addressable.
fn (mut t Transformer) make_compiler_default_array_clone_value(source flat.NodeId, array_type string, source_is_owned_temporary bool) flat.NodeId {
	elem_type := array_type[2..]
	if !t.compiler_default_clone_type_needs_work(elem_type) {
		return t.make_array_clone_value(source, array_type)
	}
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type, 'derived_clone_array_source')
	out_name := t.new_temp('derived_clone_array')
	idx_name := t.new_temp('derived_clone_array_idx')
	t.mark_fn_used('array__clone')
	storage_clone := t.make_call_typed('array__clone', [
		t.runtime_addr(stable_source, array_type),
	], array_type)
	t.pending_stmts << t.make_decl_assign_typed(out_name, storage_clone, array_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(out_name), 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.array_get_value(stable_source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign_without_ownership_drop(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), elem_type), cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type)
	return result
}

// make_compiler_default_fixed_array_clone_value copies the fixed-array storage and
// replaces each owning element with an independent clone. The initial element copies
// are non-owning and are overwritten without being dropped.
fn (mut t Transformer) make_compiler_default_fixed_array_clone_value(source flat.NodeId, raw_fixed_type string) flat.NodeId {
	fixed_type :=
		t.receiver_type_text_source_fixed_spelling(t.resolved_fixed_array_canonical_type(raw_fixed_type))
	elem_type := fixed_array_elem_type(fixed_type)
	if !t.compiler_default_clone_type_needs_work(elem_type) {
		return source
	}
	source_is_owned_temporary := !t.expr_can_take_address(source)
	stable_source := t.stable_transformed_expr_for_reuse(source, fixed_type, 'derived_clone_fixed_array_source')
	out_name := t.new_temp('derived_clone_fixed_array')
	idx_name := t.new_temp('derived_clone_fixed_array_idx')
	t.pending_stmts << t.make_decl_assign_typed(out_name, stable_source, fixed_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_fixed_array_len_expr(fixed_type))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.make_index(stable_source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign_without_ownership_drop(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), elem_type), cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), fixed_type)
	return result
}

// make_compiler_default_map_clone_value constructs a fresh map, recursively clones
// owning non-string keys and values, and lets map key callbacks clone string keys.
// The source lifetime is classified by the caller before transformation can turn a
// temporary literal into an addressable synthetic identifier.
fn (mut t Transformer) make_compiler_default_map_clone_value(source flat.NodeId, map_type string, source_is_owned_temporary bool) flat.NodeId {
	key_type, value_type := t.map_type_parts(map_type)
	clean_key_type := t.normalize_type_alias(key_type).trim_space()
	key_needs_clone := clean_key_type != 'string'
		&& t.compiler_default_clone_type_needs_work(key_type)
	value_needs_clone := t.compiler_default_clone_type_needs_work(value_type)
	stable_source := t.stable_transformed_expr_for_reuse(source, map_type, 'derived_clone_map_source')
	if key_type.len == 0 || value_type.len == 0 || (!key_needs_clone && !value_needs_clone) {
		t.mark_fn_used('map__clone')
		storage_clone := t.make_call_typed('map__clone', [
			t.runtime_addr(stable_source, map_type),
		], map_type)
		if !source_is_owned_temporary {
			return storage_clone
		}
		out_name := t.new_temp('derived_clone_map')
		t.pending_stmts << t.make_decl_assign_typed(out_name, storage_clone, map_type)
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
		result := t.make_ident(out_name)
		t.set_node_typ(int(result), map_type)
		return result
	}
	out_name := t.new_temp('derived_clone_map')
	key_name := t.new_temp('derived_clone_map_key')
	source_value_name := t.new_temp('derived_clone_map_source_value')
	cloned_key_name := t.new_temp('derived_clone_map_cloned_key')
	value_name := t.new_temp('derived_clone_map_value')
	t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_new_map_call(map_type), map_type)
	key_storage_type := t.map_key_storage_type(key_type)
	t.set_var_type(key_name, key_storage_type)
	t.set_var_type(source_value_name, value_type)
	pending_start := t.pending_stmts.len
	cloned_key := if key_needs_clone {
		t.make_compiler_default_clone_value(t.make_ident(key_name), key_type, true)
	} else {
		t.make_ident(key_name)
	}
	cloned_value := if value_needs_clone {
		t.make_compiler_default_clone_value(t.make_ident(source_value_name), value_type, true)
	} else {
		t.make_ident(source_value_name)
	}
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	map_key_name := if key_needs_clone { cloned_key_name } else { key_name }
	if key_needs_clone {
		body << t.make_decl_assign_typed(cloned_key_name, cloned_key, key_storage_type)
	}
	body << t.make_decl_assign_typed(value_name, cloned_value, value_type)
	body << t.make_map_set_stmt(t.make_ident(out_name), map_type, map_key_name, value_name)
	if clean_key_type == 'string' {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(key_name),
		], 'void'))
	}
	start := t.a.children.len
	t.a.children << t.make_ident(key_name)
	t.a.children << t.make_ident(source_value_name)
	t.a.children << stable_source
	for stmt in body {
		t.a.children << stmt
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind: .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		value: '3'
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), map_type)
	return result
}

fn (t &Transformer) compiler_default_clone_type_needs_work(typ string) bool {
	clean := t.normalize_type_alias(typ).trim_space()
	if clean.len == 0 || clean.starts_with('&') {
		return false
	}
	if clean.starts_with('!') {
		return true
	}
	if clean.starts_with('?') {
		return true
	}
	if t.is_fixed_array_type(clean) {
		return t.compiler_default_clone_type_needs_work(fixed_array_elem_type(clean))
	}
	if clean == 'string' || clean.starts_with('[]') || clean.starts_with('map[') {
		return true
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean)
		if t.tc.ownership_type_requires_destruction(parsed)
			|| t.tc.named_type_implements_marker(clean, 'IClone') {
			return true
		}
	}
	if clean in t.structs || clean in t.sum_types {
		clone_name := '${clean}.clone'
		if clone_name in t.fn_ret_types || (!isnil(t.tc) && clone_name in t.tc.fn_ret_types) {
			return true
		}
	}
	return false
}

// try_lower_array_method_call supports try lower array method call handling for Transformer.
fn (mut t Transformer) try_lower_array_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	if fn_node.value == 'str' {
		if smartcast_call := t.try_lower_smartcast_target_receiver_method_call(call_id, node) {
			return smartcast_call
		}
		if smartcast_str := t.smartcast_sum_str_call(base_id) {
			return smartcast_str
		}
		decoded_value_type := t.or_expr_receiver_unwrapped_type(base_id) or { '' }
		decoded_value_is_concrete := decl_type_is_usable(decoded_value_type)
			&& !t.generic_arg_is_unresolved(decoded_value_type)
			&& !decoded_value_type.starts_with('&')
		if !decoded_value_is_concrete {
			if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, 'str') {
				return exact_call
			}
		}
		if !decoded_value_is_concrete {
			mut raw_base_types := []string{}
			for candidate in [t.raw_var_type_for_expr(base_id) or { '' }, t.node_type(base_id),
				t.lvalue_type(base_id)] {
				clean := candidate.trim_left('&')
				if clean.len > 0 && clean !in raw_base_types {
					raw_base_types << clean
				}
				if !isnil(t.tc) {
					resolved := t.tc.resolve_imported_type_text_in_file(clean, t.cur_file)
					if resolved.len > 0 && resolved !in raw_base_types {
						raw_base_types << resolved
					}
				}
			}
			for raw_base_type in raw_base_types {
				if method_name := t.resolve_receiver_method_for_type(raw_base_type, 'str') {
					if t.receiver_method_matches_type_name(method_name, raw_base_type) {
						args := t.transform_receiver_method_args(node, base_id, method_name)
						ret_type := t.receiver_method_return_type(method_name, node.typ)
						t.mark_fn_used_name(method_name)
						return t.make_call_typed(method_name, args, ret_type)
					}
				}
				method_name := '${raw_base_type}.str'
				if t.is_known_fn_name(method_name)
					&& t.receiver_method_matches_type_name(method_name, raw_base_type) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type := t.receiver_method_return_type(method_name, node.typ)
					t.mark_fn_used_name(method_name)
					return t.make_call_typed(method_name, args, ret_type)
				}
			}
		}
	}
	array_builtin_method := t.array_builtin_method_name(fn_node.value) or { '' }
	if fn_node.value !in ['clone', 'reverse', 'contains', 'index', 'last_index', 'join', 'any',
		'all', 'count', 'equals', 'prepend', 'insert', 'push_many', 'str', 'to_fixed_size', 'wait'] {
		if fn_node.value !in ['filter', 'map', 'sort', 'sorted', 'sort_with_compare',
			'sorted_with_compare'] && array_builtin_method.len == 0 {
			mut early_base_type := t.node_type(base_id)
			if early_base_type.len == 0 {
				early_base_type = t.lvalue_type(base_id)
			}
			early_clean_type := t.normalize_type_alias(if early_base_type.starts_with('&') {
				early_base_type[1..]
			} else {
				early_base_type
			})
			if early_clean_type.starts_with('[]') || t.is_fixed_array_type(early_clean_type) {
				if smartcast_method := t.resolve_smartcast_target_receiver_method(base_id, fn_node.value) {
					if !t.receiver_method_name_is_open_generic(smartcast_method) {
						args := t.transform_receiver_method_args(node, base_id, smartcast_method)
						ret_type := t.receiver_method_return_type(smartcast_method, node.typ)
						t.mark_fn_used_name(smartcast_method)
						return t.make_call_typed(smartcast_method, args, ret_type)
					}
				}
				if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, '') {
					return exact_call
				}
			}
			return none
		}
	}
	mut base_type := t.node_type(base_id)
	raw_base_type := t.raw_checker_node_type(base_id)
	if raw_base_type.len > 0 {
		if _ := t.array_alias_stringify_type(raw_base_type) {
			base_type = raw_base_type
		}
	}
	mut smartcast_container := false
	if sc := t.find_smartcast(t.expr_key(base_id)) {
		variant_type := t.resolve_variant(sc.sum_type_name, sc.variant_name)
		if variant_type.starts_with('[]') || t.is_fixed_array_type(variant_type) {
			base_type = variant_type
			smartcast_container = true
		}
	}
	base_node := t.a.nodes[int(base_id)]
	if base_node.kind == .call {
		concrete_base_type := t.concrete_generic_call_return_type(base_id, base_node)
		if concrete_base_type.starts_with('[]') {
			base_type = concrete_base_type
		}
	}
	if (!base_type.starts_with('[]') && !t.is_fixed_array_type(base_type)) || base_type == 'array' {
		lvalue_base_type := t.lvalue_type(base_id)
		if lvalue_base_type.starts_with('[]') || t.is_fixed_array_type(lvalue_base_type) {
			base_type = lvalue_base_type
		}
	}
	base_type = t.normalize_type_alias(base_type)
	base_type = transform_unshared_receiver_type(base_type)
	if !base_type.starts_with('[]') && !t.is_fixed_array_type(base_type) {
		if base_node.kind in [.ident, .call, .selector, .as_expr] {
			new_base := t.transform_expr(base_id)
			new_base_type := t.node_type(new_base)
			if new_base_type.starts_with('[]') || t.is_fixed_array_type(new_base_type) {
				selector := t.make_selector(new_base, fn_node.value, '')
				mut children := []flat.NodeId{cap: int(node.children_count)}
				children << selector
				for i in 1 .. node.children_count {
					children << t.a.child(&node, i)
				}
				start := t.a.children.len
				for child in children {
					t.a.children << child
				}
				new_node := flat.Node{
					kind: .call
					children_start: start
					children_count: node.children_count
					pos: node.pos
					typ: node.typ
				}
				return t.try_lower_array_method_call(call_id, new_node)
			}
		}
	}
	if fn_node.value == 'str' && base_node.kind == .call {
		new_base := t.transform_expr(base_id)
		mut new_base_type := t.node_type(new_base)
		if alias_type := t.array_alias_stringify_type(new_base_type) {
			return t.wrap_string_conversion(new_base, alias_type)
		}
		if raw_alias_type := t.array_alias_stringify_type(raw_base_type) {
			return t.wrap_string_conversion(new_base, raw_alias_type)
		}
		if recorded_elem_type := t.recorded_array_call_elem_type(base_id) {
			new_base_type = '[]${recorded_elem_type}'
			t.set_node_typ(int(new_base), new_base_type)
		}
		if new_base_type.starts_with('[]') {
			if stringify_type_has_generic_placeholder(new_base_type) {
				return none
			}
			return t.lower_array_str(new_base, new_base_type)
		}
		if new_base_type.starts_with('map[') {
			if stringify_type_has_generic_placeholder(new_base_type) {
				return none
			}
			return t.lower_map_str(new_base, new_base_type)
		}
	}
	clean_base_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if fn_node.value == 'str' && t.is_builder_receiver(base_id, base_type) {
		return none
	}
	if fn_node.value == 'str' && base_type.starts_with('&')
		&& (clean_base_type.starts_with('[]') || t.is_fixed_array_type(clean_base_type)) {
		return t.lower_ref_collection_str(t.transform_expr(base_id), clean_base_type)
	}
	if t.is_fixed_array_type(clean_base_type) && fn_node.value == 'pointers'
		&& array_builtin_method.len > 0 {
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 && method_name != array_builtin_method
			&& t.call_resolved_to_method(call_id, method_name)
			&& !t.receiver_method_name_is_open_generic(method_name) {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type := t.receiver_method_return_type(method_name, node.typ)
			t.mark_fn_used(method_name)
			return t.make_call_typed(method_name, args, ret_type)
		}
		if dynamic_method := t.resolve_fixed_array_dynamic_receiver_method(clean_base_type, fn_node.value) {
			if dynamic_method != array_builtin_method {
				return t.lower_fixed_array_dynamic_receiver_method_call(node, base_id, clean_base_type, dynamic_method)
			}
		}
		// Keep the fixed receiver as a fixed-array expression. Adapting it through
		// the builtin `array` parameter makes `.pointers()` point into a dynamic copy
		// instead of the receiver's original storage.
		args := [t.transform_expr(base_id)]
		ret_type := t.receiver_method_return_type(array_builtin_method, node.typ)
		// Use an internal target so call type propagation does not adapt the fixed
		// receiver to the builtin method's dynamic `array` parameter.
		return t.make_call_typed('__v3_fixed_array_pointers', args, ret_type)
	}
	if t.is_fixed_array_type(clean_base_type) {
		if fn_node.value == 'str' {
			receiver := if smartcast_container {
				t.make_plain_expr_for_smartcast(base_id)
			} else {
				t.transform_expr(base_id)
			}
			raw_alias_type := t.raw_alias_type_for_expr(base_id)
			stringify_type := if raw_alias_type.len > 0 { raw_alias_type } else { clean_base_type }
			return t.wrap_string_conversion(receiver, stringify_type)
		}
		// A fixed-array alias may declare a method with the same name as an array
		// builtin. Honor the checker-selected alias method before adapting the
		// fixed array to a dynamic array for builtin lowering.
		if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, array_builtin_method) {
			return exact_call
		}
		elem_type := fixed_array_elem_type(clean_base_type)
		array_type := '[]${elem_type}'
		tmp_name := t.new_temp('fixed_arr')
		array_value := if fn_node.value in ['reverse_in_place', 'sort', 'sort_with_compare'] {
			t.fixed_array_value_to_array_no_alloc(base_id, clean_base_type, array_type)
		} else {
			t.fixed_array_value_to_array(base_id, clean_base_type, array_type)
		}
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, array_value, array_type)
		selector := t.make_selector(t.make_ident(tmp_name), fn_node.value, '')
		if fn_node.value == 'wait' {
			wait_type := fixed_thread_array_wait_return_type(elem_type)
			if wait_type.len > 0 {
				return t.make_call_expr_typed(selector, []flat.NodeId{}, wait_type)
			}
		}
		mut children := []flat.NodeId{cap: int(node.children_count)}
		children << selector
		for i in 1 .. node.children_count {
			children << t.a.child(&node, i)
		}
		start := t.a.children.len
		for child in children {
			t.a.children << child
		}
		new_node := flat.Node{
			kind: .call
			children_start: start
			children_count: node.children_count
			pos: node.pos
			typ: node.typ
		}
		return t.try_lower_array_method_call(call_id, new_node)
	}
	if !clean_base_type.starts_with('[]') {
		return none
	}
	if !(smartcast_container && fn_node.value == 'str') {
		if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, array_builtin_method) {
			return exact_call
		}
	}
	if fn_node.value == 'str' && stringify_type_has_generic_placeholder(clean_base_type) {
		return none
	}
	elem_type := clean_base_type[2..]
	if fn_node.value == 'prepend' {
		return t.lower_array_prepend_call(node, fn_node, base_type, elem_type)
	}
	if fn_node.value == 'insert' {
		return t.lower_array_insert_call(node, fn_node, base_type, elem_type)
	}
	if fn_node.value == 'push_many' {
		return t.lower_array_push_many_call(node, fn_node, base_type, elem_type)
	}
	if fn_node.value == 'contains' {
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 && (t.call_resolved_to_method(call_id, method_name)
			|| transform_is_exact_array_receiver_method(method_name))
			&& !t.receiver_method_name_is_open_generic(method_name) {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type := t.receiver_method_return_type(method_name, node.typ)
			t.mark_fn_used(method_name)
			return t.make_call_typed(method_name, args, ret_type)
		}
	}
	match fn_node.value {
		'filter' {
			return t.lower_array_filter_call(node, fn_node, clean_base_type)
		}
		'map' {
			return t.lower_array_map_call(node, fn_node, clean_base_type)
		}
		'sort' {
			return t.lower_array_sort_call(node, fn_node, base_type)
		}
		'sorted' {
			return t.lower_array_sorted_call(node, fn_node, clean_base_type)
		}
		'sort_with_compare' {
			return t.lower_array_sort_with_compare_call(node, fn_node, base_type)
		}
		'sorted_with_compare' {
			return t.lower_array_sorted_with_compare_call(node, fn_node, clean_base_type)
		}
		'any', 'all' {
			return t.lower_array_any_all_call(node, fn_node, clean_base_type, fn_node.value)
		}
		'count' {
			return t.lower_array_count_call(node, fn_node, clean_base_type)
		}
		'str' {
			receiver := if smartcast_container {
				t.make_plain_expr_for_smartcast(base_id)
			} else {
				t.transform_expr(base_id)
			}
			raw_alias_type := t.raw_alias_type_for_expr(base_id)
			stringify_type := if raw_alias_type.len > 0 { raw_alias_type } else { clean_base_type }
			return t.wrap_string_conversion(receiver, stringify_type)
		}
		'to_fixed_size' {
			if base_node.kind != .array_literal {
				return none
			}
			fixed_type := if t.is_fixed_array_type(node.typ) {
				node.typ
			} else {
				'[${base_node.children_count}]${elem_type}'
			}
			return t.transform_fixed_array_literal_for_type(base_id, base_node, fixed_type)
		}
		'equals' {
			if node.children_count < 2 {
				return none
			}
			method_name := t.resolve_receiver_method_name(base_id, 'equals')
			if method_name.len > 0 && t.call_resolved_to_method(call_id, method_name)
				&& !t.receiver_method_name_is_open_generic(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				t.mark_fn_used(method_name)
				return t.make_call_typed(method_name, args, 'bool')
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(t.a.children[node.children_start + 1])
			if t.array_elem_needs_element_eq(elem_type) {
				return t.make_array_elementwise_eq_call(receiver, arg, elem_type, clean_base_type, clean_base_type, node)
			}
			if elem_type.starts_with('[]') {
				return t.make_call_typed('array_eq_array', [receiver, arg,
					t.make_int_literal(array_nested_eq_depth(clean_base_type))], 'bool')
			}
			if elem_type == 'string' {
				return t.make_call_typed('array_eq_string', [receiver, arg], 'bool')
			}
			return t.make_call_typed('array_eq_raw', [receiver, arg, t.make_sizeof_type(elem_type)], 'bool')
		}
		else {}
	}

	match fn_node.value {
		'clone' {
			method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type)
			if method_name.len > 0 && method_name != array_builtin_method
				&& t.call_resolved_to_method(call_id, method_name)
				&& !t.receiver_method_name_is_open_generic(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				t.mark_fn_used(method_name)
				return t.make_call_typed(method_name, args, ret_type)
			}
			return t.make_array_clone_call(base_id, base_type)
		}
		'reverse' {
			method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type)
			if method_name.len > 0 && method_name != array_builtin_method
				&& t.call_resolved_to_method(call_id, method_name)
				&& !t.receiver_method_name_is_open_generic(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				t.mark_fn_used(method_name)
				return t.make_call_typed(method_name, args, ret_type)
			}
			return t.make_array_reverse_call(base_id, base_type)
		}
		'contains' {
			if node.children_count < 2 {
				return none
			}
			arg_id := t.a.children[node.children_start + 1]
			if lowered := t.lower_array_membership_expr(base_id, arg_id, base_type, true, node) {
				return lowered
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(arg_id)
			fn_name := if elem_type == 'string' {
				'array_contains_string'
			} else {
				'array_contains_int'
			}
			return t.make_call_typed(fn_name, [receiver, arg], 'bool')
		}
		'index' {
			if node.children_count < 2 {
				return none
			}
			arg_id := t.a.children[node.children_start + 1]
			if lowered := t.lower_array_index_expr(base_id, arg_id, base_type, true, node) {
				return lowered
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(arg_id)
			fn_name := if elem_type == 'string' { 'array_index_string' } else { 'array_index_int' }
			return t.make_call_typed(fn_name, [receiver, arg], 'int')
		}
		'last_index' {
			if node.children_count < 2 {
				return none
			}
			arg_id := t.a.children[node.children_start + 1]
			if lowered := t.lower_array_last_index_expr(base_id, arg_id, base_type, true, node) {
				return lowered
			}
			return none
		}
		'join' {
			if node.children_count < 2 {
				return none
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(t.a.children[node.children_start + 1])
			return t.make_call_typed('Array_string__join', [receiver, arg], 'string')
		}
		else {
			if array_method_stays_in_cgen(fn_node.value) {
				method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type)
				if method_name.len > 0 && method_name != array_builtin_method
					&& t.call_resolved_to_method(call_id, method_name)
					&& !t.receiver_method_name_is_open_generic(method_name) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type := t.receiver_method_return_type(method_name, node.typ)
					t.mark_fn_used(method_name)
					return t.make_call_typed(method_name, args, ret_type)
				}
				if t.validating_generic_spec
					&& !t.validate_cgen_array_method_args(node, base_id, clean_base_type, fn_node.value) {
					return t.make_empty()
				}
				if lowered := t.lower_owned_array_accessor_call(base_id, base_type, elem_type, fn_node.value) {
					return lowered
				}
				if lowered := t.lower_owned_array_removal_call(node, base_id, base_type, elem_type, fn_node.value) {
					return lowered
				}
				if array_method_stays_in_cgen_needs_runtime_mark(fn_node.value) {
					t.mark_fn_used('array__${fn_node.value}')
				}
				return none
			}
			if array_builtin_method.len > 0 {
				method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type)
				if method_name.len > 0 && method_name != array_builtin_method
					&& !t.receiver_method_name_is_open_generic(method_name) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type := t.receiver_method_return_type(method_name, node.typ)
					t.mark_fn_used(method_name)
					return t.make_call_typed(method_name, args, ret_type)
				}
				if lowered := t.lower_owned_array_accessor_call(base_id, base_type, elem_type, fn_node.value) {
					return lowered
				}
				if lowered := t.lower_owned_array_removal_call(node, base_id, base_type, elem_type, fn_node.value) {
					return lowered
				}
				args := t.transform_receiver_method_args(node, base_id, array_builtin_method)
				ret_type := t.receiver_method_return_type(array_builtin_method, node.typ)
				return t.make_call_typed(array_builtin_method, args, ret_type)
			}
			return none
		}
	}
}

// lower_owned_array_accessor_call returns an independent clone for a non-removing array
// accessor. The stored element remains owned by the array and will be destroyed with it.
fn (mut t Transformer) lower_owned_array_accessor_call(base_id flat.NodeId, base_type string, elem_type string, method string) ?flat.NodeId {
	if method !in ['first', 'last'] || isnil(t.tc) {
		return none
	}
	elem := t.tc.parse_type(elem_type)
	if !t.tc.ownership_type_requires_destruction(elem) {
		return none
	}
	if _ := t.tc.ownership_default_clone_missing_method(elem) {
		// The checker rejects this accessor. Do not fall through to the raw shallow
		// first()/last() path while transforming the invalid program.
		return t.make_empty()
	}
	source_is_owned_temporary := !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
	base := t.stable_expr_for_reuse(base_id)
	clean_base_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	mut array_value := base
	if base_type.starts_with('&') {
		array_value = t.make_prefix(.mul, base)
		t.set_node_typ(int(array_value), clean_base_type)
	}
	empty := t.make_infix(.eq, t.make_selector(array_value, 'len', 'int'), t.make_int_literal(0))
	t.pending_stmts << t.make_if(empty, t.make_block([
		t.make_panic_stmt('array.${method}: array is empty'),
	]), t.make_empty())
	index := if method == 'first' {
		t.make_int_literal(0)
	} else {
		t.make_infix(.minus, t.make_selector(array_value, 'len', 'int'), t.make_int_literal(1))
	}
	stored := t.make_index(array_value, index, elem_type)
	cloned := t.make_compiler_default_clone_value(stored, elem_type, true)
	if !source_is_owned_temporary {
		return cloned
	}
	result_name := t.new_temp('array_accessor_result')
	t.pending_stmts << t.make_decl_assign_typed(result_name, cloned, elem_type)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [array_value], 'void'))
	return t.make_ident(result_name)
}

// lower_owned_array_removal_call drops ownership-bearing elements before a raw array
// operation removes them from the range visited by the scope-exit destructor.
fn (mut t Transformer) lower_owned_array_removal_call(node flat.Node, base_id flat.NodeId, base_type string, elem_type string, method string) ?flat.NodeId {
	if method !in ['delete', 'delete_many', 'clear', 'free', 'trim', 'drop', 'delete_last'] || isnil(t.tc) || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type)) {
		return none
	}
	// V1 autofree treats clear() after a bulk append as an ownership transfer:
	// the destination retains the element storage while clear() only resets the
	// source header. Explicit ownership mode instead destroys removed elements.
	if method == 'clear' && t.tc.autofree_enabled() {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	clean_base_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	mut array_value := base
	if base_type.starts_with('&') {
		array_value = t.make_prefix(.mul, base)
		t.set_node_typ(int(array_value), clean_base_type)
	}
	mut args := []flat.NodeId{}
	mut drop_stmts := []flat.NodeId{}
	mut valid_drop_range := flat.empty_node
	match method {
		'delete' {
			if node.children_count < 2 {
				return none
			}
			index := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), 'int'), 'int', 'array_delete_index')
			args << index
			drop_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_index(array_value, index, elem_type),
			], 'void'))
			valid_drop_range = t.make_infix(.logical_and, t.make_infix(.ge, index, t.make_int_literal(0)), t.make_infix(.lt, index, t.make_selector(array_value, 'len', 'int')))
		}
		'delete_many' {
			if node.children_count < 3 {
				return none
			}
			index := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), 'int'), 'int', 'array_delete_index')
			size := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 2), 'int'), 'int', 'array_delete_size')
			args << index
			args << size
			t.append_owned_array_drop_range(array_value, elem_type, index, t.make_infix(.plus, index, size), mut drop_stmts)
			end := t.make_infix(.plus, t.make_cast('i64', index, 'i64'), t.make_cast('i64', size, 'i64'))
			valid_drop_range = t.make_infix(.logical_and, t.make_infix(.ge, index, t.make_int_literal(0)), t.make_infix(.le, end, t.make_cast('i64', t.make_selector(array_value, 'len', 'int'), 'i64')))
		}
		'clear', 'free' {
			t.append_owned_array_drop_range(array_value, elem_type, t.make_int_literal(0), t.make_selector(array_value, 'len', 'int'), mut drop_stmts)
		}
		'trim' {
			if node.children_count < 2 {
				return none
			}
			index := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), 'int'), 'int', 'array_trim_index')
			args << index
			t.append_owned_array_drop_range(array_value, elem_type, index, t.make_selector(array_value, 'len', 'int'), mut drop_stmts)
		}
		'drop' {
			if node.children_count < 2 {
				return none
			}
			count := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), 'int'), 'int', 'array_drop_count')
			args << count
			t.append_owned_array_drop_prefix(array_value, elem_type, count, mut drop_stmts)
		}
		'delete_last' {
			last := t.make_infix(.minus, t.make_selector(array_value, 'len', 'int'), t.make_int_literal(1))
			drop_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_index(array_value, last, elem_type),
			], 'void'))
			valid_drop_range = t.make_infix(.gt, t.make_selector(array_value, 'len', 'int'), t.make_int_literal(0))
		}
		else {
			return none
		}
	}

	if drop_stmts.len > 0 {
		needs_unique_shrink := t.make_method_call(array_value, 'needs_unique_shrink', []flat.NodeId{})
		t.set_node_typ(int(needs_unique_shrink), 'bool')
		t.mark_fn_used('array.needs_unique_shrink')
		mut should_drop := t.make_prefix(.not, needs_unique_shrink)
		if int(valid_drop_range) >= 0 {
			should_drop = t.make_infix(.logical_and, valid_drop_range, should_drop)
		}
		drop_block := t.make_block(drop_stmts)
		start := t.a.children.len
		t.a.children << should_drop
		t.a.children << drop_block
		t.pending_stmts << t.a.add_node(flat.Node{
			kind: .if_expr
			children_start: start
			children_count: 2
			skip_ownership_drops: true
		})
	}

	if array_method_stays_in_cgen_needs_runtime_mark(method) {
		t.mark_fn_used('array__${method}')
	}
	call := t.make_method_call(base, method, args)
	t.set_node_typ(int(call), node.typ)
	if method == 'free' {
		// Keep scope-exit destruction safe even when the raw backend free does not
		// mutate the original header in place.
		t.pending_stmts << t.make_expr_stmt(call)
		t.pending_stmts << t.make_assign(array_value, t.zero_value_for_type(clean_base_type))
		return t.make_empty()
	}
	return call
}

fn (mut t Transformer) append_owned_array_drop_prefix(array_value flat.NodeId, elem_type string, count flat.NodeId, mut stmts []flat.NodeId) {
	idx_name := t.new_temp('array_drop_index')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	below_count := t.make_infix(.lt, t.make_ident(idx_name), count)
	below_len := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(array_value, 'len', 'int'))
	cond := t.make_infix(.logical_and, below_count, below_len)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem := t.make_index(array_value, t.make_ident(idx_name), elem_type)
	drop_stmt := t.make_expr_stmt(t.make_call_typed('drop_owned', [elem], 'void'))
	stmts << t.make_for_stmt(init, cond, post, [drop_stmt], flat.Node{
		skip_ownership_drops: true
	})
}

fn (mut t Transformer) append_owned_array_drop_range(array_value flat.NodeId, elem_type string, start flat.NodeId, end flat.NodeId, mut stmts []flat.NodeId) {
	idx_name := t.new_temp('array_drop_index')
	init := t.make_decl_assign_typed(idx_name, start, 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), end)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem := t.make_index(array_value, t.make_ident(idx_name), elem_type)
	drop_stmt := t.make_expr_stmt(t.make_call_typed('drop_owned', [elem], 'void'))
	stmts << t.make_for_stmt(init, cond, post, [drop_stmt], flat.Node{
		skip_ownership_drops: true
	})
}

// try_lower_ignored_owned_array_pop_stmt destroys an owned array element result when the
// source program ignores it. Pop methods transfer the removed element from owning arrays,
// while slices and arrays with live slice views still alias shared backing storage.
// First/last produce the independent clone supplied by lower_owned_array_accessor_call.
fn (mut t Transformer) try_lower_ignored_owned_array_pop_stmt(call_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['first', 'last', 'pop', 'pop_left'] || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_base_type := t.normalize_type_alias(base_type.trim_left('&'))
	if !clean_base_type.starts_with('[]') {
		return none
	}
	elem_type := clean_base_type[2..]
	if !t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type)) {
		return none
	}
	array_builtin_method := t.array_builtin_method_name(fn_node.value) or { '' }
	method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type)
	if method_name.len > 0 && method_name != array_builtin_method
		&& t.call_resolved_to_method(call_id, method_name)
		&& !t.receiver_method_name_is_open_generic(method_name) {
		return none
	}
	mut result := []flat.NodeId{}
	mut popped := flat.empty_node
	mut array_value := flat.empty_node
	mut drop_result_guard_name := ''
	if fn_node.value in ['pop', 'pop_left'] {
		base := t.stable_expr_for_reuse(base_id)
		t.drain_pending(mut result)
		array_value = base
		if base_type.starts_with('&') {
			array_value = t.make_prefix(.mul, base)
			t.set_node_typ(int(array_value), clean_base_type)
		}
		needs_unique_shrink := t.make_method_call(array_value, 'needs_unique_shrink', []flat.NodeId{})
		t.set_node_typ(int(needs_unique_shrink), 'bool')
		t.mark_fn_used('array.needs_unique_shrink')
		drop_result_guard_name = t.new_temp('ignored_array_pop_can_drop')
		result << t.make_decl_assign_typed(drop_result_guard_name, t.make_prefix(.not, needs_unique_shrink), 'bool')
		popped = t.make_method_call(array_value, fn_node.value, []flat.NodeId{})
		t.set_node_typ(int(popped), elem_type)
		if fn_node.value == 'pop' {
			t.mark_fn_used('array__pop')
		}
	} else {
		popped = t.transform_expr(call_id)
		t.drain_pending(mut result)
	}
	popped_name := t.new_temp('ignored_array_pop')
	result << t.make_decl_assign_typed(popped_name, popped, elem_type)
	drop_result := t.make_expr_stmt(t.make_call_typed('drop_owned', [
		t.make_ident(popped_name),
	], 'void'))
	if fn_node.value in ['pop', 'pop_left'] {
		drop_block := t.make_block([drop_result])
		start := t.a.children.len
		t.a.children << t.make_ident(drop_result_guard_name)
		t.a.children << drop_block
		result << t.a.add_node(flat.Node{
			kind: .if_expr
			children_start: start
			children_count: 2
			skip_ownership_drops: true
		})
	} else {
		result << drop_result
	}
	return result
}

fn (t &Transformer) array_builtin_method_name(method string) ?string {
	method_name := 'array.${method}'
	if t.is_known_fn_name(method_name) {
		return method_name
	}
	return none
}

fn array_method_stays_in_cgen(method string) bool {
	return match method.len {
		3 { method == 'pop' || method == 'str' }
		4 { method == 'last' || method == 'trim' || method == 'free' || method == 'wait' }
		5 { method == 'first' || method == 'clear' }
		6 { method == 'repeat' || method == 'delete' }
		7 { method == 'bytestr' }
		8 { method == 'pop_left' }
		10 { method == 'ensure_cap' }
		11 { method == 'delete_last' }
		15 { method == 'repeat_to_depth' }
		else { false }
	}
}

fn array_method_stays_in_cgen_needs_runtime_mark(method string) bool {
	return match method.len {
		3 { method == 'pop' }
		4 { method == 'trim' }
		5 { method == 'clear' }
		else { false }
	}
}

fn (mut t Transformer) validate_cgen_array_method_args(node flat.Node, base_id flat.NodeId, base_type string, method string) bool {
	base_node := t.a.nodes[int(base_id)]
	base_name := if base_node.kind == .ident && base_node.value.len > 0 {
		base_node.value
	} else {
		base_type
	}
	display_name := '${base_name}.${method}'
	if method == 'bytestr' && base_type !in ['[]u8', '[]byte'] {
		t.record_monomorph_error('unknown function `${display_name}`')
		return false
	}
	if method == 'wait' {
		elem_type := if base_type.starts_with('[]') { base_type[2..].trim_space() } else { '' }
		if elem_type != 'thread' && !elem_type.starts_with('thread ') {
			t.record_monomorph_error('unknown function `${display_name}`')
			return false
		}
	}
	mut expected_types := []string{}
	mut has_signature := false
	if builtin_method := t.array_builtin_method_name(method) {
		params := t.call_param_types(builtin_method)
		if params.len > 0 {
			param_offset := t.receiver_method_param_offset(base_id, node, params, builtin_method)
			for i in param_offset .. params.len {
				expected_types << t.normalize_type_alias(t.semantic_type_name(params[i]))
			}
			has_signature = true
		}
	}
	if !has_signature {
		expected_types = match method {
			'trim', 'repeat', 'delete', 'ensure_cap' { ['int'] }
			'repeat_to_depth' { ['int', 'int'] }
			else { []string{} }
		}
	}
	actual_count := int(node.children_count) - 1
	if actual_count != expected_types.len {
		t.record_monomorph_error('argument count mismatch for `${display_name}`: expected ${expected_types.len}, got ${actual_count}')
		return false
	}
	mut valid := true
	for i, expected in expected_types {
		arg_id := t.a.child(&node, i + 1)
		mut actual := t.resolve_expr_type(arg_id)
		if actual.len == 0 {
			actual = t.node_type(arg_id)
		}
		if actual.len == 0 {
			actual = t.reliable_stringify_type(arg_id)
		}
		actual = t.normalize_type_alias(actual)
		if actual != expected {
			t.record_monomorph_error('cannot use `${actual}` as argument ${i + 1} to `${display_name}`; expected `${expected}`')
			valid = false
		}
	}
	return valid
}

fn thread_array_wait_return_type(elem_type string) ?string {
	name := elem_type.trim_space()
	if name == 'thread' {
		return 'void'
	}
	if !name.starts_with('thread ') {
		return none
	}
	payload := name[7..].trim_space()
	if payload == '?' || payload == '!' {
		return '${payload}void'
	}
	if payload.starts_with('?') || payload.starts_with('!') {
		value_type := payload[1..].trim_space()
		if value_type.len == 0 || value_type == 'void' {
			return '${payload[0].ascii_str()}void'
		}
		return '${payload[0].ascii_str()}[]${value_type}'
	}
	return '[]${payload}'
}

fn fixed_thread_array_wait_return_type(elem_type string) string {
	return thread_array_wait_return_type(elem_type) or { '' }
}

fn transform_is_exact_array_receiver_method(name string) bool {
	if !name.contains('.') {
		return false
	}
	receiver := name.all_before_last('.')
	return receiver.starts_with('[]') || receiver.contains('.[]')
}

// try_lower_map_method_call supports try lower map method call handling for Transformer.
fn (mut t Transformer) try_lower_map_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	is_lowered_map_method := map_method_is_lowered_by_transform(fn_node.value)
	if fn_node.kind != .selector || fn_node.children_count == 0 || !is_lowered_map_method {
		return none
	}
	base_id := t.a.children[fn_node.children_start]
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.checker_node_type(base_id)
	}
	clean_type := t.clean_map_type(base_type)
	if !clean_type.starts_with('map[') {
		return none
	}
	builtin_method := 'map.${fn_node.value}'
	method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
	if method_name.len > 0 && method_name != builtin_method
		&& t.call_resolved_to_method(call_id, method_name)
		&& !t.receiver_method_name_is_open_generic(method_name) {
		args := t.transform_receiver_method_args(node, base_id, method_name)
		ret_type := t.receiver_method_return_type(method_name, node.typ)
		t.mark_fn_used(method_name)
		return t.make_call_typed(method_name, args, ret_type)
	}
	if fn_node.value == 'clone' {
		key_type, value_type := t.map_type_parts(clean_type)
		if key_type.len == 0 || value_type.len == 0 {
			return none
		}
		mut clean_base_type := t.normalize_type_alias(base_type).trim_space()
		for clean_base_type.starts_with('shared ') {
			clean_base_type = clean_base_type[7..].trim_space()
		}
		source_is_owned_temporary := !clean_base_type.starts_with('&')
			&& !t.expr_can_take_address(base_id)
		mut source := t.transform_expr(base_id)
		if isnil(t.tc) || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type)) {
			t.mark_fn_used('map__clone')
			return t.make_call_typed('map__clone', [t.runtime_addr(source, base_type)], clean_type)
		}
		if clean_base_type.starts_with('&') {
			source = t.make_prefix(.mul, source)
			t.set_node_typ(int(source), clean_type)
		}
		// The checker rejects this call. Do not lower it to the unsafe raw clone while
		// processing the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(key_type)) {
			return source
		}
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(value_type)) {
			return source
		}
		return t.make_compiler_default_map_clone_value(source, clean_type, source_is_owned_temporary)
	}
	if fn_node.value == 'delete' {
		if nested_delete := t.try_lower_nested_map_delete_call(node, base_id, clean_type) {
			return nested_delete
		}
	}
	source_is_owned_temporary := !base_type.starts_with('&') && !t.expr_can_take_address(base_id)
	base := t.stable_expr_for_reuse(base_id)
	if map_method_needs_runtime_addr_only(fn_node.value) {
		key_type, value_type := t.map_type_parts(clean_type)
		if key_type.len > 0 && value_type.len > 0 {
			t.append_owned_map_entries_drop_before_reset(base, base_type, key_type, value_type, fn_node.value)
		}
		t.mark_fn_used('map__${fn_node.value}')
		call := t.make_call_typed('map__${fn_node.value}', [
			t.runtime_addr(base, base_type),
		], 'void')
		if fn_node.value == 'free' && !isnil(t.tc)
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type)) {
			mut map_value := base
			if base_type.starts_with('&') {
				map_value = t.make_prefix(.mul, base)
				t.set_node_typ(int(map_value), clean_type)
			}
			// Keep scope-exit destruction safe after the raw runtime free leaves the
			// original map header unchanged.
			t.pending_stmts << t.make_expr_stmt(call)
			t.pending_stmts << t.make_assign(map_value, t.zero_value_for_type(clean_type))
			return t.make_empty()
		}
		return call
	}
	if fn_node.value == 'move' {
		t.mark_fn_used('map__move')
		return t.make_call_typed('map__move', [t.runtime_addr(base, base_type)], clean_type)
	}
	if fn_node.value == 'reserve' {
		if node.children_count < 2 {
			return none
		}
		t.mark_fn_used('map__reserve')
		capacity := t.transform_expr_for_type(t.a.child(&node, 1), 'u32')
		return t.make_call_typed('map__reserve', [t.runtime_addr(base, base_type), capacity], 'void')
	}
	if fn_node.value == 'delete' {
		if node.children_count < 2 {
			return none
		}
		key_type, value_type := t.map_type_parts(clean_type)
		if key_type.len == 0 || value_type.len == 0 {
			return none
		}
		t.mark_fn_used('map__delete')
		key_id := t.a.child(&node, 1)
		cleanup_key := !isnil(t.tc) && t.map_key_expr_creates_owned_value(key_id, key_type)
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type))
		key_name := t.new_temp('map_key')
		key_storage_type := t.map_key_storage_type(key_type)
		t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(key_id, key_type), key_storage_type)
		handled_delete := t.append_owned_map_entry_delete_with_drops(base, base_type, key_name, key_type, value_type)
		if handled_delete {
			if cleanup_key {
				t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
					t.make_ident(key_name),
				], 'void'))
			}
			return t.make_empty()
		}
		delete_call := t.make_call_typed('map__delete', [
			t.runtime_addr(base, base_type),
			t.make_prefix(.amp, t.make_ident(key_name)),
		], 'void')
		if cleanup_key {
			t.pending_stmts << t.make_expr_stmt(delete_call)
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_ident(key_name),
			], 'void'))
			return t.make_empty()
		}
		return delete_call
	}
	if fn_node.value !in ['keys', 'values'] {
		return none
	}
	elem_type := if fn_node.value == 'keys' {
		t.map_key_type(clean_type)
	} else {
		t.map_value_type(clean_type)
	}
	if elem_type.len == 0 {
		return none
	}
	if !isnil(t.tc) && t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
		&& t.compiler_default_clone_type_needs_work(elem_type)
		&& (fn_node.value == 'values' || t.normalize_type_alias(elem_type).trim_space() != 'string') {
		return t.make_owned_map_items_value(base, clean_type, elem_type, fn_node.value == 'keys', source_is_owned_temporary)
	}
	t.mark_fn_used('map__${fn_node.value}')
	items := t.make_call_typed('map__${fn_node.value}', [
		t.runtime_addr(base, base_type),
	], '[]${elem_type}')
	if !source_is_owned_temporary || isnil(t.tc)
		|| !t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type)) {
		return items
	}
	// Preserve the returned array before destroying a map receiver that was materialized
	// after ownership analysis. Raw keys()/values() still allocate independent array storage,
	// even when their scalar items need no per-entry clone work.
	out_name := t.new_temp('map_items')
	t.pending_stmts << t.make_decl_assign_typed(out_name, items, '[]${elem_type}')
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [base], 'void'))
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), '[]${elem_type}')
	return result
}

// make_owned_map_items_value builds an independent keys()/values() array by cloning every
// ownership-bearing item instead of accepting map__values' shallow byte copies.
fn (mut t Transformer) make_owned_map_items_value(source flat.NodeId, map_type string, item_type string, take_keys bool, source_is_owned_temporary bool) flat.NodeId {
	key_type, value_type := t.map_type_parts(map_type)
	stable_source := t.stable_transformed_expr_for_reuse(source, map_type, 'map_items_source')
	out_name := t.new_temp('map_items')
	key_name := t.new_temp('map_items_key')
	value_name := t.new_temp('map_items_value')
	item_name := t.new_temp('map_items_item')
	t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_array_new_call(item_type, t.make_int_literal(0), t.make_selector(stable_source, 'len', 'int')), '[]${item_type}')
	t.set_var_type(key_name, t.map_key_storage_type(key_type))
	t.set_var_type(value_name, value_type)
	source_item := if take_keys { t.make_ident(key_name) } else { t.make_ident(value_name) }
	pending_start := t.pending_stmts.len
	cloned_item := t.make_compiler_default_clone_value(source_item, item_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_decl_assign_typed(item_name, cloned_item, item_type)
	body << t.make_expr_stmt(t.make_call_typed('array_push', [
		t.runtime_addr(t.make_ident(out_name), '[]${item_type}'),
		t.make_prefix(.amp, t.make_ident(item_name)),
	], 'void'))
	t.mark_fn_used('array_push')
	start := t.a.children.len
	if take_keys {
		t.a.children << t.make_ident(key_name)
		t.a.children << t.make_ident(value_name)
	} else {
		t.a.children << t.make_ident(value_name)
		t.a.children << flat.empty_node
	}
	t.a.children << stable_source
	for stmt in body {
		t.a.children << stmt
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind: .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		value: '3'
		skip_ownership_drops: true
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], 'void'))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), '[]${item_type}')
	return result
}

// append_owned_map_entries_drop_before_reset releases live stored entries before clear/free
// makes them unavailable to the normal scope-exit map destructor.
fn (mut t Transformer) append_owned_map_entries_drop_before_reset(map_expr flat.NodeId, map_type string, key_type_name string, value_type_name string, method string) {
	if isnil(t.tc) {
		return
	}
	clean_key_type := t.normalize_type_alias(key_type_name).trim_space()
	key_requires_drop := t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type_name))
		&& !(method == 'free' && clean_key_type == 'string')
	value_requires_drop :=
		t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type_name))
	if !key_requires_drop && !value_requires_drop {
		return
	}
	key_name := t.new_temp('map_reset_key')
	value_name := t.new_temp('map_reset_value')
	t.set_var_type(key_name, t.map_key_storage_type(key_type_name))
	t.set_var_type(value_name, value_type_name)
	mut body := []flat.NodeId{}
	if key_requires_drop {
		t.mark_fn_used('map__get_key_check')
		key_ptr_name := t.new_temp('map_reset_key_ptr')
		body << t.make_decl_assign_typed(key_ptr_name, t.make_map_get_key_check_expr(map_expr, map_type, key_name), 'voidptr')
		stored_key_ptr := t.make_cast('&${key_type_name}', t.make_ident(key_ptr_name), '&${key_type_name}')
		stored_key := t.make_prefix(.mul, stored_key_ptr)
		t.set_node_typ(int(stored_key), key_type_name)
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [stored_key], 'void'))
		if clean_key_type == 'string' {
			body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_ident(key_name),
			], 'void'))
		}
	}
	if value_requires_drop {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(value_name),
		], 'void'))
	}
	start := t.a.children.len
	if key_requires_drop {
		t.a.children << t.make_ident(key_name)
		t.a.children << t.make_ident(value_name)
	} else {
		t.a.children << t.make_ident(value_name)
		t.a.children << flat.empty_node
	}
	t.a.children << map_expr
	for stmt in body {
		t.a.children << stmt
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind: .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		value: '3'
		skip_ownership_drops: true
	})
}

// append_owned_map_entry_delete_with_drops snapshots ownership-bearing stored values,
// removes the entry, and only then destroys the snapshots so key mutation cannot prevent
// map__delete from finding the entry.
fn (mut t Transformer) append_owned_map_entry_delete_with_drops(map_expr flat.NodeId, map_type string, key_name string, key_type_name string, value_type_name string) bool {
	if isnil(t.tc) {
		return false
	}
	clean_key_type := t.normalize_type_alias(key_type_name).trim_space()
	key_requires_drop := clean_key_type != 'string'
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type_name))
	value_requires_drop :=
		t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type_name))
	if !key_requires_drop && !value_requires_drop {
		return false
	}
	value_ptr_name := t.new_temp('map_delete_value')
	value_ptr := t.make_map_get_check_expr(map_expr, map_type, key_name)
	t.pending_stmts << t.make_decl_assign_typed(value_ptr_name, value_ptr, 'voidptr')
	mut body := []flat.NodeId{}
	mut saved_key_name := ''
	if key_requires_drop {
		t.mark_fn_used('map__get_key_check')
		key_ptr_name := t.new_temp('map_delete_key')
		body << t.make_decl_assign_typed(key_ptr_name, t.make_map_get_key_check_expr(map_expr, map_type, key_name), 'voidptr')
		key_ptr := t.make_cast('&${key_type_name}', t.make_ident(key_ptr_name), '&${key_type_name}')
		stored_key := t.make_prefix(.mul, key_ptr)
		t.set_node_typ(int(stored_key), key_type_name)
		saved_key_name = t.new_temp('map_deleted_key')
		body << t.make_decl_assign_typed(saved_key_name, stored_key, key_type_name)
	}
	mut saved_value_name := ''
	if value_requires_drop {
		stored_value_ptr := t.make_cast('&${value_type_name}', t.make_ident(value_ptr_name), '&${value_type_name}')
		stored_value := t.make_prefix(.mul, stored_value_ptr)
		t.set_node_typ(int(stored_value), value_type_name)
		saved_value_name = t.new_temp('map_deleted_value')
		body << t.make_decl_assign_typed(saved_value_name, stored_value, value_type_name)
	}
	body << t.make_expr_stmt(t.make_call_typed('map__delete', [
		t.runtime_addr(map_expr, map_type),
		t.make_prefix(.amp, t.make_ident(key_name)),
	], 'void'))
	if key_requires_drop {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(saved_key_name),
		], 'void'))
	}
	if value_requires_drop {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(saved_value_name),
		], 'void'))
	}
	found := t.make_infix(.ne, t.make_ident(value_ptr_name), t.a.add(.nil_literal))
	body_block := t.make_block(body)
	start := t.a.children.len
	t.a.children << found
	t.a.children << body_block
	t.pending_stmts << t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 2
		skip_ownership_drops: true
	})
	return true
}

fn map_method_is_lowered_by_transform(method string) bool {
	return match method.len {
		4 { method == 'keys' || method == 'free' || method == 'move' }
		5 { method == 'clear' || method == 'clone' }
		6 { method == 'values' || method == 'delete' }
		7 { method == 'reserve' }
		else { false }
	}
}

fn map_method_needs_runtime_addr_only(method string) bool {
	return match method.len {
		4 { method == 'free' }
		5 { method == 'clear' }
		else { false }
	}
}

// try_lower_channel_method_call lowers channel source methods to runtime calls before
// backend selection.
fn (mut t Transformer) try_lower_channel_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0
		|| fn_node.value !in ['close', 'try_push', 'try_pop'] {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(call_id) {
			channel_method := 'chan.${fn_node.value}'
			runtime_method := 'sync__Channel__${fn_node.value}'
			mut clean_resolved_method := resolved_method.trim_space()
			for clean_resolved_method.starts_with('&') {
				clean_resolved_method = clean_resolved_method[1..].trim_space()
			}
			is_typed_channel_method := clean_resolved_method.starts_with('chan ')
				&& clean_resolved_method.ends_with('.${fn_node.value}')
			if resolved_method != channel_method && resolved_method != runtime_method
				&& !is_typed_channel_method {
				if fn_node.value == 'close' {
					if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, 'chan.close') {
						return exact_call
					}
				}
				return none
			}
		}
	}
	mut clean_type := base_type
	mut ptr_depth := 0
	for clean_type.starts_with('&') {
		ptr_depth++
		clean_type = clean_type[1..].trim_space()
	}
	if clean_type != 'chan' && !clean_type.starts_with('chan ') {
		if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, 'chan.close') {
			return exact_call
		}
		return none
	}
	if fn_node.value in ['try_push', 'try_pop'] {
		t.mark_fn_used('sync__Channel__${fn_node.value}')
		return none
	}
	return t.lower_runtime_channel_close(base_id, node, ptr_depth)
}

fn (mut t Transformer) lower_runtime_channel_close(base_id flat.NodeId, node flat.Node, ptr_depth int) flat.NodeId {
	t.mark_fn_used('sync__Channel__close')
	mut err_values := []flat.NodeId{}
	if node.children_count > 1 {
		for i in 1 .. node.children_count {
			err_values << t.a.child(&node, i)
		}
	}
	errs := if err_values.len > 0 {
		lit := t.make_array_literal_typed(err_values, '[]IError')
		t.transform_array_literal(lit, t.a.nodes[int(lit)])
	} else {
		t.make_array_new_call('IError', t.make_int_literal(0), t.make_int_literal(0))
	}
	fn_expr := t.make_selector(t.make_ident('C'), 'sync__Channel__close', '')
	mut receiver := t.transform_expr(base_id)
	for _ in 0 .. ptr_depth {
		receiver = t.make_prefix(.mul, receiver)
	}
	if ptr_depth == 0 {
		receiver = t.make_cast('&sync.Channel', receiver, '&sync.Channel')
	} else {
		t.set_node_typ(int(receiver), '&sync.Channel')
	}
	return t.make_call_expr_typed(fn_expr, [receiver, errs], 'void')
}

// try_lower_move_method_call supports try lower move method call handling for Transformer.
fn (mut t Transformer) try_lower_move_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count != 1 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 || fn_node.value != 'move' {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	clean_array_type := t.membership_container_type(base_type)
	if clean_array_type.starts_with('[]') {
		if !isnil(t.tc) {
			if resolved := t.tc.resolved_call_name(call_id) {
				if !is_builtin_collection_resolved_call(resolved) && t.is_known_fn_name(resolved) {
					return none
				}
			}
		}
		return t.transform_expr(base_id)
	}
	return none
}

fn (t &Transformer) fn_literal_expr_takes_address_of_capture(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len || name.len == 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .prefix && node.op == .amp && node.children_count > 0
		&& t.fn_literal_lvalue_is_rooted_at_capture(t.a.child(&node, 0), name) {
		return true
	}
	for i in 0 .. node.children_count {
		if t.fn_literal_expr_takes_address_of_capture(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) fn_literal_lvalue_is_rooted_at_capture(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value == name
	}
	if node.kind in [.selector, .index, .paren] && node.children_count > 0 {
		return t.fn_literal_lvalue_is_rooted_at_capture(t.a.child(&node, 0), name)
	}
	return false
}

// lift_fn_literal supports lift fn literal handling for Transformer.
fn (mut t Transformer) lift_fn_literal(_id flat.NodeId, node flat.Node) flat.NodeId {
	name := t.new_fn_literal_name()
	// A checked literal can carry its complete function-value type here; the
	// synthesized declaration itself needs only that signature's return type.
	ret_type := if node.typ.len > 0 {
		fn_type_return_type_text(node.typ) or { node.typ }
	} else {
		'void'
	}
	result_may_alias_capture := t.immediate_closure_result_may_alias_capture(ret_type)
	mut param_types := []types.Type{}
	mut param_type_texts := []string{}
	mut param_ids := []flat.NodeId{}
	mut param_names := []string{}
	mut capture_names := []string{}
	mut capture_types := map[string]string{}
	mut context_field_types := map[string]string{}
	mut capture_by_ref := map[string]bool{}
	mut capture_from_context := map[string]bool{}
	mut capture_from_heap := map[string]bool{}
	mut body_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .param {
			param_ids << child_id
			param_type_texts << child.typ
			if child.value.len > 0 {
				param_names << child.value
			}
			if !isnil(t.tc) {
				param_types << t.tc.parse_type(child.typ)
			}
		} else if child.kind == .ident {
			// `fn [T] (...)` inside a generic declaration captures the type
			// parameter for specialization, not a runtime value.
			if child.value in t.active_generic_params {
				continue
			}
			if child.value.len > 0 && child.value !in capture_names {
				mut capture_type := t.raw_var_type(child.value)
				if capture_type.len == 0 {
					capture_type = t.var_type(child.value)
				}
				if capture_type.len == 0 {
					capture_type = t.node_type(child_id)
				}
				if capture_type.len == 0 && !isnil(t.tc) {
					if typ := t.tc.expr_type(child_id) {
						capture_type = t.semantic_type_name(typ)
					}
				}
				if capture_type.len == 0 || capture_type == 'unknown' {
					capture_type = 'int'
				}
				if child.value in t.heaped_amp_locals && capture_type.starts_with('&')
					&& t.is_fixed_array_type(capture_type[1..]) {
					capture_type = capture_type[1..]
					capture_from_heap[child.value] = true
				}
				if t.active_specialization_args.len > 0 {
					specialized_capture_type := t.subst_type(capture_type, t.active_specialization_args)
					if specialized_capture_type.len > 0
						&& !t.generic_arg_is_unresolved(specialized_capture_type) {
						capture_type = specialized_capture_type
					}
				}
				normalized_capture_type := t.normalize_type_alias(capture_type)
				if !capture_type.starts_with('shared ') && !capture_type.starts_with('atomic ')
					&& normalized_capture_type.len > 0 && normalized_capture_type != 'unknown' {
					capture_type = normalized_capture_type
				}
				capture_names << child.value
				is_mut_capture := child.is_mut
				is_ref_capture := is_mut_capture && !capture_type.starts_with('&')
					&& !t.is_fn_pointer_type_name(capture_type)
				capture_types[child.value] = if is_ref_capture {
					'&${capture_type}'
				} else {
					capture_type
				}
				// A mutable value capture belongs to the closure instance. Store the
				// value itself in the heap context and expose a pointer alias only
				// inside the lifted body; storing `&outer_local` would dangle when a
				// closure is returned and would make separate instances interfere.
				context_field_types[child.value] = if is_ref_capture
					&& t.is_fixed_array_type(capture_type) {
					'&${capture_type}'
				} else {
					capture_type
				}
				capture_by_ref[child.value] = is_ref_capture
			}
		} else {
			body_ids << child_id
		}
	}
	if result_may_alias_capture {
		for capture_name in capture_names {
			capture_type := capture_types[capture_name] or { continue }
			if capture_by_ref[capture_name] or { false } || capture_type.starts_with('&') {
				continue
			}
			mut aliases_capture := t.is_fixed_array_type(capture_type)
			if !aliases_capture {
				for body_id in body_ids {
					if t.fn_literal_expr_takes_address_of_capture(body_id, capture_name) {
						aliases_capture = true
						break
					}
				}
			}
			if aliases_capture {
				capture_types[capture_name] = '&${capture_type}'
				capture_from_context[capture_name] = true
			}
		}
	}
	file_module := t.current_source_module()
	generated_module := if file_module.len > 0 { file_module } else { 'main' }
	context_type := '${name}_Ctx'
	context_local := '${name}_ctx'
	if capture_names.len > 0 {
		t.add_fn_literal_capture_context(context_type, generated_module, capture_names, context_field_types)
	}
	saved_fn_name := t.cur_fn_name
	saved_ret_type := t.cur_fn_ret_type
	saved_vars := t.var_types.clone()
	saved_fn_value_locals := t.fn_value_locals.clone()
	saved_mut_param_values := t.mut_param_values.clone()
	saved_fixed_array_param_values := t.fixed_array_param_values.clone()
	saved_local_closure_cleanup_decls := t.local_closure_cleanup_decls.clone()
	saved_local_closure_cleanup_assigns := t.local_closure_cleanup_assigns.clone()
	saved_local_closure_field_cleanups := t.local_closure_field_cleanups.clone()
	t.cur_fn_name = name
	t.cur_fn_ret_type = ret_type
	t.reset_var_types()
	mut saved_param_pointer_flags := map[string]bool{}
	mut saved_param_pointer_rvalue_flags := map[string]bool{}
	for param_id in param_ids {
		param := t.a.nodes[int(param_id)]
		if param.value.len > 0 && param.typ.len > 0 {
			saved_param_pointer_flags[param.value] = t.pointer_value_lvalues[param.value] or {
				false
			}
			saved_param_pointer_rvalue_flags[param.value] = t.pointer_value_rvalues[param.value] or {
				false
			}
			t.pointer_value_lvalues.delete(param.value)
			t.pointer_value_rvalues.delete(param.value)
			t.set_var_type(param.value, param.typ)
			if t.is_fixed_array_type(param.typ) {
				t.fixed_array_param_values[param.value] = true
			}
			if param.is_mut || param.op == .amp || param.typ.starts_with('mut ') {
				t.mut_param_values[param.value] = true
				t.pointer_value_lvalues[param.value] = true
				if param.op == .amp {
					t.pointer_value_rvalues[param.value] = true
				}
			}
		}
	}
	mut lifted_body := []flat.NodeId{cap: capture_names.len + body_ids.len + 1}
	mut saved_capture_pointer_flags := map[string]bool{}
	mut saved_capture_pointer_rvalue_flags := map[string]bool{}
	if capture_names.len > 0 {
		current_data := t.make_call_typed('__v3_closure_current_data', []flat.NodeId{}, 'voidptr')
		context_ptr := t.make_cast('&${context_type}', current_data, '&${context_type}')
		context_decl := t.make_decl_assign_typed(context_local, context_ptr, '&${context_type}')
		t.set_var_type(context_local, '&${context_type}')
		lifted_body << context_decl
	}
	for capture_name in capture_names {
		if capture_name in param_names {
			continue
		}
		capture_type := capture_types[capture_name] or { continue }
		is_ref_capture := capture_by_ref[capture_name] or { false }
		is_context_capture := capture_from_context[capture_name] or { false }
		t.set_var_type(capture_name, capture_type)
		// Captures rewritten into synthetic `&T` pointer-value locals need pointer-value
		// lvalue/rvalue lowering. A mut capture whose original type is already a pointer
		// (`&S`) stays a genuine `&S` local:
		// dereferencing its rvalue uses would corrupt `&S` -> `S` and break calls that
		// expect the pointer (e.g. `takes_ptr(p)`), and its assignments must not become
		// `*p = ...`.
		if is_ref_capture || is_context_capture {
			saved_capture_pointer_flags[capture_name] = t.pointer_value_lvalues[capture_name] or {
				false
			}
			saved_capture_pointer_rvalue_flags[capture_name] = t.pointer_value_rvalues[capture_name] or {
				false
			}
			t.pointer_value_lvalues[capture_name] = true
			t.pointer_value_rvalues[capture_name] = true
		}
		context_ident := t.make_ident(context_local)
		t.set_node_typ(int(context_ident), '&${context_type}')
		context_field_type := context_field_types[capture_name] or { capture_type }
		mut capture_decl_rhs := t.make_selector(context_ident, capture_name, context_field_type)
		if (is_ref_capture || is_context_capture) && !context_field_type.starts_with('&') {
			capture_decl_rhs = t.make_prefix(.amp, capture_decl_rhs)
			t.set_node_typ(int(capture_decl_rhs), capture_type)
		}
		capture_decl := t.make_decl_assign_typed(capture_name, capture_decl_rhs, capture_type)
		if capture_type.starts_with('shared ') {
			t.set_node_value(int(capture_decl), 'shared:alias')
		}
		lifted_body << capture_decl
	}
	for body_id in body_ids {
		lifted_body << body_id
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	t.mark_local_closure_cleanup_decls(lifted_body)
	new_body := t.transform_stmts(lifted_body)
	t.pending_stmts = outer_pending
	for param_name in param_names {
		if saved_param_pointer_flags[param_name] or { false } {
			t.pointer_value_lvalues[param_name] = true
		} else {
			t.pointer_value_lvalues.delete(param_name)
		}
		if saved_param_pointer_rvalue_flags[param_name] or { false } {
			t.pointer_value_rvalues[param_name] = true
		} else {
			t.pointer_value_rvalues.delete(param_name)
		}
	}
	for capture_name in capture_names {
		if (capture_by_ref[capture_name] or { false }) || (capture_from_context[capture_name] or {
			false
		}) {
			if saved_capture_pointer_flags[capture_name] or { false } {
				t.pointer_value_lvalues[capture_name] = true
			} else {
				t.pointer_value_lvalues.delete(capture_name)
			}
			if saved_capture_pointer_rvalue_flags[capture_name] or { false } {
				t.pointer_value_rvalues[capture_name] = true
			} else {
				t.pointer_value_rvalues.delete(capture_name)
			}
		}
	}
	t.restore_var_types(saved_vars)
	t.fn_value_locals = saved_fn_value_locals.clone()
	t.mut_param_values = saved_mut_param_values.clone()
	t.fixed_array_param_values = saved_fixed_array_param_values.clone()
	t.local_closure_cleanup_decls = saved_local_closure_cleanup_decls.clone()
	t.local_closure_cleanup_assigns = saved_local_closure_cleanup_assigns.clone()
	t.local_closure_field_cleanups = saved_local_closure_field_cleanups.clone()
	t.cur_fn_name = saved_fn_name
	t.cur_fn_ret_type = saved_ret_type
	mut all_ids := []flat.NodeId{cap: param_ids.len + new_body.len}
	for param_id in param_ids {
		all_ids << param_id
	}
	for body_id in new_body {
		all_ids << body_id
	}
	// Generated declarations are appended after all parsed files. Preserve both
	// source markers, otherwise C generation can associate a lifted literal with
	// the preceding cached header and omit its program-owned body.
	t.add_generated_fn_decl_context(generated_module)
	start := t.a.children.len
	for child_id in all_ids {
		t.a.children << child_id
	}
	fn_decl := t.a.add_node(flat.Node{
		kind: .fn_decl
		value: name
		typ: ret_type
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, generated_module, t.cur_file)
	t.set_fn_ret_type(name, ret_type)
	if !isnil(t.tc) {
		t.tc.ensure_private_transform_signatures()
		ret := t.tc.parse_type(ret_type)
		t.tc.fn_ret_types[name] = ret
		t.tc.register_generated_fn_param_types(name, param_types.clone())
		t.tc.fn_variadic[name] = false
		t.add_receiver_method_suffix_index(name)
		t.tc_signature_names_log << name
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${name}'
			t.set_fn_ret_type(qname, ret_type)
			t.tc.fn_ret_types[qname] = ret
			t.tc.register_generated_fn_param_types(qname, param_types.clone())
			t.tc.fn_variadic[qname] = false
			t.add_receiver_method_suffix_index(qname)
			t.tc_signature_names_log << qname
		}
	}
	// Fn literals are materialized after markused has already walked the parsed
	// declarations. Root the generated declaration explicitly so module-local
	// callbacks are not referenced without a prototype/body in the C output.
	t.mark_fn_used_name(name)
	mut fn_value_param_type_texts := []string{cap: param_types.len}
	for i, param_type in param_types {
		raw_type := if i < param_type_texts.len { param_type_texts[i] } else { '' }
		fn_value_param_type_texts << if raw_type.contains('main.') {
			raw_type
		} else {
			param_type.name()
		}
	}
	fn_value_type := fn_literal_value_type_text_from_text(fn_value_param_type_texts, ret_type)
	mut ident := flat.empty_node
	if file_module.len > 0 && file_module != 'main' && file_module != 'builtin' {
		ident = t.make_ident('${file_module}.${name}')
	} else {
		ident = t.make_ident(name)
	}
	if fn_value_type.len > 0 {
		t.set_node_typ(int(ident), fn_value_type)
	}
	if capture_names.len == 0 {
		return ident
	}
	mut context_fields := []flat.NodeId{cap: capture_names.len}
	for capture_name in capture_names {
		context_field_type := context_field_types[capture_name] or { continue }
		mut value := t.make_ident(capture_name)
		if capture_from_heap[capture_name] or { false } {
			t.set_node_typ(int(value), context_field_type)
		} else if capture_by_ref[capture_name] or { false } && context_field_type.starts_with('&') {
			value = t.make_prefix(.amp, value)
			t.set_node_typ(int(value), context_field_type)
		}
		context_fields << t.make_named_field_init(capture_name, value, context_field_type)
	}
	context_start := t.a.children.len
	for field in context_fields {
		t.a.children << field
	}
	context_init := t.a.add_node(flat.Node{
		kind: .struct_init
		value: context_type
		typ: context_type
		children_start: context_start
		children_count: flat.child_count(context_fields.len)
	})
	context_ptr := t.make_prefix(.amp, context_init)
	t.set_node_typ(int(context_ptr), '&${context_type}')
	create := t.make_call_typed('closure.closure_create_with_data', [
		t.make_cast('voidptr', ident, 'voidptr'),
		t.make_cast('voidptr', context_ptr, 'voidptr'),
		t.make_bool_literal(true),
	], 'voidptr')
	t.mark_fn_used_name('closure.closure_create_with_data')
	return t.make_cast(fn_value_type, create, fn_value_type)
}

fn fn_literal_value_type_text(params []types.Type, ret_type string) string {
	mut parts := []string{cap: params.len}
	for param in params {
		parts << param.name()
	}
	return fn_literal_value_type_text_from_text(parts, ret_type)
}

fn fn_literal_value_type_text_from_text(params []string, ret_type string) string {
	ret := ret_type.trim_space()
	if ret.len == 0 || ret == 'void' {
		return 'fn(${params.join(', ')})'
	}
	return 'fn(${params.join(', ')}) ${ret}'
}

fn (t &Transformer) current_source_module() string {
	if !isnil(t.tc) {
		if entry_file := t.tc.fn_type_files['main'] {
			if entry_file == t.cur_file {
				return 'main'
			}
		}
	}
	return if t.cur_module.len > 0 { t.cur_module } else { 'main' }
}

fn (mut t Transformer) new_fn_literal_name() string {
	for {
		name := t.new_global_temp('anon_fn')
		if !t.fn_literal_name_exists(name) {
			return name
		}
	}
	return t.new_global_temp('anon_fn')
}

fn (t &Transformer) fn_literal_name_exists(name string) bool {
	if name in t.fn_ret_types {
		return true
	}
	if !isnil(t.tc) {
		return t.tc.has_fn_decl_short_name(name) || name in t.tc.fn_ret_types
	}
	for node in t.a.nodes {
		if node.kind == .fn_decl && (node.value == name || node.value.all_after_last('.') == name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) add_fn_literal_capture_context(name string, module_name string, capture_names []string, capture_types map[string]string) {
	if t.struct_maps_shared {
		t.structs = t.structs.clone()
		t.struct_maps_shared = false
	}
	if !isnil(t.tc) {
		t.tc.ensure_private_transform_structs()
	}
	t.add_generated_fn_decl_context(module_name)
	mut field_ids := []flat.NodeId{cap: capture_names.len}
	mut semantic_fields := []types.StructField{cap: capture_names.len}
	mut transform_fields := []FieldInfo{cap: capture_names.len}
	for capture_name in capture_names {
		capture_type := capture_types[capture_name] or { continue }
		field_ids << t.a.add_node(flat.Node{
			kind: .field_decl
			value: capture_name
			typ: capture_type
		})
		parsed_type := if isnil(t.tc) {
			types.Type(types.Unknown{
				reason: 'capture context without type checker'
			})
		} else {
			t.tc.parse_type(capture_type)
		}
		semantic_fields << types.StructField{
			name: capture_name
			typ: parsed_type
		}
		transform_fields << FieldInfo{
			name: capture_name
			typ: capture_type
			raw_typ: capture_type
			default_expr: flat.empty_node
		}
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	struct_id := t.a.add_node(flat.Node{
		kind: .struct_decl
		value: name
		children_start: start
		children_count: flat.child_count(field_ids.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(struct_id, module_name, t.cur_file)
	info := StructInfo{
		name: name
		module: module_name
		fields: transform_fields
	}
	t.structs[name] = info
	t.generated_capture_contexts << name
	if !isnil(t.tc) {
		t.tc.structs[name] = semantic_fields
		t.tc.struct_modules[name] = module_name
		t.tc.struct_files[name] = t.cur_file
		t.tc.register_short_type_name(name)
	}
	if module_name.len > 0 && module_name !in ['main', 'builtin'] {
		qualified := '${module_name}.${name}'
		t.structs[qualified] = info
		t.generated_capture_contexts << qualified
		if !isnil(t.tc) {
			t.tc.structs[qualified] = semantic_fields
			t.tc.struct_modules[qualified] = module_name
			t.tc.struct_files[qualified] = t.cur_file
			t.tc.register_short_type_name(qualified)
		}
	}
}

fn (mut t Transformer) add_generated_fn_decl_context(module_name string) {
	if t.cur_file.len > 0 {
		t.a.add_node(flat.Node{
			kind: .file
			value: t.cur_file
		})
	}
	if module_name.len > 0 {
		t.a.add_node(flat.Node{
			kind: .module_decl
			value: module_name
		})
	}
}

// try_lower_builtin_call checks if a call is to a builtin that needs special lowering.
// Returns none for most calls so the caller falls through to generic call transform.
fn (mut t Transformer) try_lower_builtin_call(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind == .selector && callee.children_count > 0 {
		base := t.a.child_node(callee, 0)
		if base.kind == .ident && base.value == 'C' {
			return none
		}
		if base.kind == .none_expr && callee.value == 'str' && node.children_count == 1 {
			return t.make_string_literal('<none>')
		}
	}
	if cast_call := t.try_lower_primitive_cast_call(node) {
		return cast_call
	}
	if sum_cast_call := t.try_lower_generic_sum_constructor_call(node) {
		return sum_cast_call
	}
	if named_cast_call := t.try_lower_generic_named_type_cast_call(node) {
		return named_cast_call
	}
	if flag_call := t.try_lower_flag_enum_call(_id, node) {
		return flag_call
	}
	if flag_default := t.try_lower_flag_default_value_call(node) {
		return flag_default
	}
	if pool_call := t.try_lower_pool_generic_method_call(node) {
		return pool_call
	}
	if static_call := t.try_lower_static_assoc_call(_id, node) {
		return static_call
	}
	if move_call := t.try_lower_move_method_call(_id, node) {
		return move_call
	}
	if channel_call := t.try_lower_channel_method_call(_id, node) {
		return channel_call
	}
	if map_call := t.try_lower_map_method_call(_id, node) {
		return map_call
	}
	if array_call := t.try_lower_array_method_call(_id, node) {
		return array_call
	}
	if smartcast_receiver_call := t.try_lower_smartcast_target_receiver_method_call(_id, node) {
		return smartcast_receiver_call
	}
	if pointer_str_call := t.try_lower_pointer_str_method_call(_id, node) {
		return pointer_str_call
	}
	if type_name_call := t.try_lower_sum_type_name_method_call(node) {
		return type_name_call
	}
	if iface_runtime_call := t.try_lower_interface_runtime_method_call(node) {
		return iface_runtime_call
	}
	if !t.validate_specialized_enum_from_call(_id, node) {
		return t.make_empty()
	}
	specialized_enum_type := if t.validating_generic_spec {
		t.builtin_enum_from_type(_id, node) or { '' }
	} else {
		''
	}
	if enum_call := t.try_lower_enum_from_string_call(_id, node) {
		return enum_call
	}
	if specialized_enum_type.len > 0 {
		return t.lower_specialized_enum_from_call(node, specialized_enum_type)
	}
	if receiver_call := t.try_lower_receiver_method_call(_id, node) {
		return receiver_call
	}
	if clone_call := t.try_lower_struct_clone_method_call(_id, node) {
		return clone_call
	}
	if string_call := t.try_lower_string_method_call(node) {
		return string_call
	}
	fn_id := t.a.children[node.children_start]
	if int(fn_id) < 0 {
		return none
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .ident {
		return none
	}
	name := fn_node.value
	if name in ['maxof', 'minof'] && node.value.len > 0 && node.children_count == 1
		&& t.is_std_minmaxof_call(_id, name) {
		if value := t.try_lower_minmaxof_call(name, node.value) {
			return value
		}
	}
	match name {
		'copy' {
			if t.is_builtin_copy_call(_id) {
				return t.try_lower_copy_call(node)
			}
			return none
		}
		'println', 'eprintln', 'print', 'eprint' {
			if node.children_count < 2 {
				return t.transform_call_args(_id, node)
			}
			arg_id := t.a.child(&node, 1)
			arg := t.stringify_expr(arg_id)
			return t.make_call(name, [arg])
		}
		'panic' {
			if node.children_count == 2 {
				arg_id := t.a.child(&node, 1)
				arg_type := t.node_type(arg_id)
				if arg_type == 'IError' {
					arg := t.transform_expr(arg_id)
					return t.make_call('panic', [
						t.make_method_call(arg, 'str', []flat.NodeId{}),
					])
				}
				return t.make_call('panic', [t.stringify_expr(arg_id)])
			}
			return none
		}
		'sizeof' {
			return none
		}
		'typeof' {
			return none
		}
		else {
			return none
		}
	}
}

fn (mut t Transformer) lower_specialized_enum_from_call(node flat.Node, enum_type string) flat.NodeId {
	arg := t.transform_expr(t.a.child(&node, 1))
	base := t.make_ident(enum_type)
	callee := t.make_selector(base, 'from', '')
	return t.make_call_expr_typed(callee, [arg], '!${enum_type}')
}

fn (mut t Transformer) validate_specialized_enum_from_call(call_id flat.NodeId, node flat.Node) bool {
	if !t.validating_generic_spec {
		return true
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind == .selector && callee.value == 'from' && callee.children_count > 0 {
		enum_node := t.a.child_node(callee, 0)
		if enum_node.kind == .ident {
			for i, param in t.active_generic_params {
				if enum_node.value != param || i >= t.active_specialization_args.len {
					continue
				}
				concrete := t.active_specialization_args[i]
				if _ := t.enum_type_from_name(concrete) {
					break
				}
				t.record_monomorph_error('unknown function `${concrete}.from`')
				return false
			}
		}
	}
	if !t.is_builtin_enum_from_call(call_id, node) {
		return true
	}
	enum_id := t.a.child(callee, 0)
	arg_id := t.a.child(&node, 1)
	actual := t.normalize_type_alias(t.specialized_expr_type_name(arg_id))
	if actual.len == 0 || actual == 'unknown' || actual == 'string'
		|| t.is_integer_type_name(actual) {
		return true
	}
	actual_display := typeof_display_type_text(actual)
	enum_node := t.a.nodes[int(enum_id)]
	enum_name := if enum_node.kind == .ident && enum_node.value.len > 0 {
		enum_node.value
	} else {
		t.node_type(enum_id)
	}
	t.record_monomorph_error('cannot use `${actual_display}` as argument 1 to `${enum_name}.from`; expected string or integer')
	return false
}

fn (t &Transformer) is_builtin_enum_from_call(call_id flat.NodeId, node flat.Node) bool {
	if _ := t.builtin_enum_from_type(call_id, node) {
		return true
	}
	return false
}

fn (t &Transformer) builtin_enum_from_type(call_id flat.NodeId, node flat.Node) ?string {
	if node.children_count != 2 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.value != 'from' || callee.children_count == 0
		|| t.enum_from_string_call_uses_user_method(call_id) {
		return none
	}
	enum_id := t.a.child(callee, 0)
	return t.enum_type_from_node(enum_id)
}

fn (mut t Transformer) try_lower_smartcast_target_receiver_method_call(_call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	method_name := t.resolve_smartcast_target_receiver_method(base_id, fn_node.value) or {
		return none
	}
	if !t.validate_resolved_receiver_method_args(node, base_id, method_name) {
		return t.make_empty()
	}
	args := t.transform_receiver_method_args(node, base_id, method_name)
	ret_type := t.receiver_method_return_type(method_name, node.typ)
	t.mark_fn_used_name(method_name)
	if fn_node.value == 'str' {
		if sc := t.find_smartcast(t.expr_key(base_id)) {
			if t.is_interface_type_name(sc.sum_type_name) && args.len > 0 {
				target := t.trim_pointer_type(t.smartcast_target_type(sc))
				if aggregate := t.stringify_aggregate_type_name(target) {
					value_ptr := t.make_prefix(.amp, args[0])
					t.set_node_typ(int(value_ptr), '&${aggregate}')
					interface_expr := t.make_plain_expr_for_smartcast(base_id)
					return t.lower_interface_smartcast_ref_str(value_ptr, interface_expr, aggregate, method_name)
				}
			}
		}
	}
	return t.make_call_typed(method_name, args, ret_type)
}

fn (t &Transformer) is_builtin_copy_call(id flat.NodeId) bool {
	if isnil(t.tc) {
		return true
	}
	resolved := t.tc.resolved_call_name(id) or { return true }
	return resolved in ['copy', 'builtin.copy']
}

fn (t &Transformer) is_std_minmaxof_call(id flat.NodeId, name string) bool {
	if isnil(t.tc) {
		return false
	}
	resolved := t.tc.resolved_call_name(id) or { return false }
	return resolved == 'math.${name}'
}

fn (mut t Transformer) try_lower_minmaxof_call(name string, raw_type string) ?flat.NodeId {
	mut typ := raw_type
	if !isnil(t.tc) {
		typ = t.normalize_type_in_module(raw_type, t.cur_module)
	}
	if typ.starts_with('builtin.') {
		typ = typ.all_after_last('.')
	}
	value := if name == 'maxof' {
		match typ {
			'i8' { '127' }
			'i16' { '32767' }
			'int', 'i32' { '2147483647' }
			'i64' { '9223372036854775807' }
			'u8' { '255' }
			'u16' { '65535' }
			'u32' { '4294967295' }
			'u64' { '18446744073709551615' }
			'f32' { '3.40282346638528859811704183484516925440e+38' }
			'f64' { '1.797693134862315708145274237317043567981e+308' }
			else {
				return none
			}
		}
	} else {
		match typ {
			'i8' { '-128' }
			'i16' { '-32768' }
			'int', 'i32' { '(-2147483647 - 1)' }
			'i64' { '(-9223372036854775807 - 1)' }
			'u8', 'u16', 'u32', 'u64' { '0' }
			'f32' { '-3.40282346638528859811704183484516925440e+38' }
			'f64' { '-1.797693134862315708145274237317043567981e+308' }
			else {
				return none
			}
		}
	}
	if typ in ['f32', 'f64'] {
		return t.make_float_literal_typed(value, typ)
	}
	return t.make_int_literal_typed(value, typ)
}

fn (mut t Transformer) try_lower_pointer_str_method_call(call_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count != 1 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value != 'str' || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	if smartcast_str := t.smartcast_sum_str_call(base_id) {
		return smartcast_str
	}
	if unwrapped_type := t.or_expr_receiver_unwrapped_type(base_id) {
		if decl_type_is_usable(unwrapped_type) && !t.generic_arg_is_unresolved(unwrapped_type)
			&& !unwrapped_type.starts_with('&') {
			return none
		}
	}
	// Mutable for-in bindings and mutable parameters use pointer-backed storage,
	// but an ordinary receiver expression is the auto-dereferenced value. Do not
	// lower their `.str()` call as an explicit pointer stringification.
	if _ := t.pointer_value_expr_type(base_id) {
		return none
	}
	base_type := t.pointer_str_receiver_type(base_id) or { return none }
	raw_alias_type := t.raw_alias_type_for_expr(base_id)
	if raw_alias_type.starts_with('&') {
		if method_name := t.checker_selected_receiver_method_name(call_id, 'str') {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type := t.receiver_method_return_type(method_name, node.typ)
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, args, ret_type)
		}
		return t.wrap_string_conversion(t.transform_expr(base_id), raw_alias_type)
	}
	clean_type := base_type[1..]
	if aggregate := t.stringify_aggregate_type_name(clean_type) {
		if t.checker_selected_custom_receiver_method(call_id, 'str') {
			method_name := t.checker_selected_receiver_method_name(call_id, 'str') or {
				return none
			}
			t.mark_fn_used_name(method_name)
			return t.lower_ref_str_guarded(t.transform_expr(base_id), aggregate, !t.str_method_has_pointer_receiver(method_name), method_name, '&nil')
		}
		return t.lower_ref_str_prefixed(t.transform_expr(base_id), aggregate)
	}
	if clean_type.starts_with('[]') || t.is_fixed_array_type(clean_type) {
		return t.lower_ref_collection_str(t.transform_expr(base_id), clean_type)
	}
	return t.wrap_string_conversion(t.transform_expr(base_id), base_type)
}

fn (mut t Transformer) or_expr_receiver_unwrapped_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.or_expr_receiver_unwrapped_type(t.a.child(&node, 0))
	}
	if node.kind != .or_expr || node.children_count == 0 {
		return none
	}
	expr_id := t.a.child(&node, 0)
	expr := t.a.nodes[int(expr_id)]
	if decode_type := t.json_decode_or_expr_type(expr_id, expr) {
		return t.optional_base_type(decode_type)
	}
	_, unwrapped_type := t.or_expr_types(expr_id, node.typ)
	return unwrapped_type
}

fn (t &Transformer) pointer_str_receiver_type(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.pointer_str_receiver_type(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		child_id := t.a.child(&node, 0)
		mut child_type := t.node_type(child_id)
		if child_type.len == 0 {
			child_type = t.lvalue_type(child_id)
		}
		if child_type.len == 0 {
			child_type = t.raw_checker_node_type(child_id)
		}
		if child_type.len > 0 {
			return '&${child_type}'
		}
	}
	mut typ := t.node_type(id)
	if typ.len == 0 {
		typ = t.raw_checker_node_type(id)
	}
	if typ.starts_with('&') {
		return typ
	}
	return none
}

fn (mut t Transformer) try_lower_copy_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 3 {
		return none
	}
	dst_arg_id := t.a.child(&node, 1)
	src_arg_id := t.a.child(&node, 2)
	dst_id := t.copy_mut_arg_value(dst_arg_id)
	src := t.transform_expr_for_type(src_arg_id, '[]u8')
	if t.copy_destination_is_range(dst_id) {
		slice := t.transform_expr(dst_id)
		tmp_name := t.new_temp('copy_dst')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, slice, '[]u8')
		return t.make_call_typed('copy', [t.make_prefix(.amp, t.make_ident(tmp_name)), src], 'int')
	}
	dst_expr := t.transform_expr(dst_id)
	// a `mut` param destination is already a pointer; taking its address again
	// would hand v_copy an Array** and corrupt the caller's frame
	dst := if t.node_type(dst_id).starts_with('&') {
		dst_expr
	} else {
		t.make_prefix(.amp, dst_expr)
	}
	return t.make_call_typed('copy', [dst, src], 'int')
}

fn (t &Transformer) copy_mut_arg_value(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.copy_mut_arg_value(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		return t.copy_mut_arg_value(t.a.child(&node, 0))
	}
	return id
}

fn (t &Transformer) copy_destination_is_range(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind != .index {
		return false
	}
	if node.value == 'range' {
		return true
	}
	if node.children_count > 1 {
		index := t.a.child_node(&node, 1)
		return index.kind == .range
	}
	return false
}

const primitive_cast_type_names = ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'byte',
	'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'rune', 'char']

// try_lower_primitive_cast_call supports try lower primitive cast call handling for Transformer.
fn (mut t Transformer) try_lower_primitive_cast_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return none
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .ident || fn_node.value !in primitive_cast_type_names {
		return none
	}
	arg_id := t.a.child(&node, 1)
	return t.make_cast(fn_node.value, t.transform_expr(arg_id), fn_node.value)
}

fn (mut t Transformer) try_lower_generic_sum_constructor_call(node flat.Node) ?flat.NodeId {
	target := t.generic_sum_constructor_call_type(node) or { return none }
	arg := t.transform_expr(t.a.child(&node, 1))
	start := t.a.children.len
	t.a.children << arg
	return t.a.add_node(flat.Node{
		kind: .cast_expr
		value: target
		children_start: start
		children_count: 1
		typ: target
	})
}

fn (mut t Transformer) try_lower_generic_named_type_cast_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.node(fn_id)
	// A monomorphized method callee such as `Tree[int].size` contains a generic
	// type spelling, but it is a function name rather than a named-type cast.
	if fn_node.kind == .ident && t.generic_callee_is_specialization(fn_node.value) {
		return none
	}
	target := t.generic_call_type_arg_name(fn_id)
	base, _, is_generic := generic_app_parts(target)
	if !is_generic || !target.ends_with(']') || !t.is_known_type_name(base) {
		return none
	}
	arg := t.transform_expr(t.a.child(&node, 1))
	start := t.a.children.len
	t.a.children << arg
	return t.a.add_node(flat.Node{
		kind: .cast_expr
		value: target
		children_start: start
		children_count: 1
		typ: target
	})
}

fn (t &Transformer) generic_sum_constructor_call_type(node flat.Node) ?string {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	if int(fn_id) < 0 {
		return none
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind == .ident && t.generic_callee_is_specialization(fn_node.value) {
		return none
	}
	mut target := ''
	if node.value.len > 0 && fn_node.kind in [.ident, .selector] {
		base := t.generic_call_type_arg_name(fn_id)
		if base.len > 0 {
			target = '${base}[${node.value}]'
		}
	} else if fn_node.kind == .index {
		target = t.generic_call_type_arg_name(fn_id)
	}
	if target.len == 0 || !target.contains('[') {
		return none
	}
	variants := t.sum_eq_variants(target) or { return none }
	if variants.len == 0 {
		return none
	}
	return target
}

// try_lower_flag_default_value_call
// supports helper handling in transform.
fn (mut t Transformer) try_lower_flag_default_value_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	mut name := ''
	if fn_node.kind == .ident {
		name = fn_node.value
	} else if fn_node.kind == .selector {
		name = fn_node.value
	}
	if name != 'flag_default_value' {
		return none
	}
	arg_id := t.a.child(&node, 1)
	arg := t.transform_expr(arg_id)
	mut arg_type := t.resolve_expr_type(arg_id)
	if arg_type.len == 0 {
		arg_type = t.reliable_stringify_type(arg_id)
	}
	if arg_type.len == 0 {
		arg_type = t.node_type(arg)
	}
	if t.normalize_type_alias(arg_type) == 'string' {
		escaped := t.make_call_typed('escape_default_string', [arg], 'string')
		return t.string_plus(t.string_plus(t.make_string_literal('"'), escaped), t.make_string_literal('"'))
	}
	return t.wrap_string_conversion(arg, arg_type)
}

// try_lower_sum_type_name_method_call supports runtime type metadata on sum values.
fn (mut t Transformer) try_lower_sum_type_name_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 1 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value !in ['type_name', 'type_idx'] || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	resolved_sum, variants := t.concrete_sum_name_and_variants(clean_type)
	if variants.len == 0 {
		return none
	}
	base := t.stable_transformed_expr_for_reuse(t.transform_expr(base_id), base_type, 'sum_type')
	tag := t.make_sum_tag_selector(base, if base_type.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	if fn_node.value == 'type_idx' {
		return t.build_sum_type_idx_chain(tag, resolved_sum, variants, 0)
	}
	return t.build_sum_type_name_chain(tag, resolved_sum, variants, 0)
}

fn (mut t Transformer) try_lower_interface_runtime_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 1 || isnil(t.tc) {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.value !in ['type_name', 'type_idx'] || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if base_type.len == 0 {
		base_type = t.raw_checker_node_type(base_id)
	}
	if base_type.len == 0 {
		return none
	}
	clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	iface_name := t.resolve_interface_type_name(clean_type)
	if iface_name.len == 0 {
		return none
	}
	transformed_base := t.transform_selector_base_expr(base_id)
	mut value_type := t.node_type(transformed_base)
	if value_type.len == 0 {
		value_type = base_type
	}
	value := t.stable_transformed_expr_for_reuse(transformed_base, value_type, 'iface_type')
	op := if value_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	tag := t.make_selector_op(value, '_typ', 'int', op)
	if fn_node.value == 'type_idx' {
		impl_names := if t.is_builtin_ierror_interface_name(iface_name) {
			t.tc.ierror_impl_names()
		} else {
			t.interface_impl_index_for_transform(iface_name).names
		}
		return t.build_interface_type_idx_chain(tag, iface_name, impl_names, 0)
	}
	impl_names := if t.is_builtin_ierror_interface_name(iface_name) {
		t.tc.ierror_impl_names()
	} else {
		t.interface_impl_index_for_transform(iface_name).names
	}
	return t.build_interface_type_name_chain(tag, iface_name, impl_names, 0)
}

fn (mut t Transformer) build_interface_type_idx_chain(tag flat.NodeId, iface_name string, impls []string, idx int) flat.NodeId {
	if idx >= impls.len {
		is_container := t.make_infix(.lt, tag, t.make_int_literal(0))
		container_idx := t.make_infix(.amp, tag, t.make_int_literal(0x7fffffff))
		then_block := t.make_block([t.make_expr_stmt(container_idx)])
		else_block := t.make_block([t.make_expr_stmt(t.make_int_literal(0))])
		start := t.a.children.len
		t.a.children << is_container
		t.a.children << then_block
		t.a.children << else_block
		return t.a.add_node(flat.Node{
			kind: .if_expr
			children_start: start
			children_count: 3
			typ: 'int'
		})
	}
	impl := impls[idx]
	type_id := t.interface_impl_type_id(iface_name, impl) or {
		return t.build_interface_type_idx_chain(tag, iface_name, impls, idx + 1)
	}
	cond := t.make_infix(.eq, tag, t.make_int_literal(type_id))
	then_block := t.make_block([
		t.make_expr_stmt(t.make_int_literal(t.type_index_for_type_name(impl))),
	])
	else_expr := t.build_interface_type_idx_chain(tag, iface_name, impls, idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
		typ: 'int'
	})
}

fn (mut t Transformer) build_interface_type_name_chain(tag flat.NodeId, iface_name string, impls []string, idx int) flat.NodeId {
	if idx >= impls.len {
		return t.make_string_literal('')
	}
	impl := impls[idx]
	type_id := t.interface_impl_type_id(iface_name, impl) or {
		return t.build_interface_type_name_chain(tag, iface_name, impls, idx + 1)
	}
	display := if impl.contains('.') { impl.all_after_last('.') } else { impl }
	cond := t.make_infix(.eq, tag, t.make_int_literal(type_id))
	then_block := t.make_block([t.make_expr_stmt(t.make_string_literal(display))])
	else_expr := t.build_interface_type_name_chain(tag, iface_name, impls, idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
		typ: 'string'
	})
}

// build_sum_type_name_chain builds sum type name chain data for transform.
fn (mut t Transformer) build_sum_type_name_chain(tag flat.NodeId, sum_name string, variants []string, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.make_string_literal('')
	}
	variant := variants[idx]
	display := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name, variant)))
	then_block := t.make_block([t.make_expr_stmt(t.make_string_literal(display))])
	else_expr := t.build_sum_type_name_chain(tag, sum_name, variants, idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
		typ: 'string'
	})
}

fn (mut t Transformer) build_sum_type_idx_chain(tag flat.NodeId, sum_name string, variants []string, idx int) flat.NodeId {
	if idx >= variants.len {
		return t.make_int_literal(0)
	}
	variant := variants[idx]
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name, variant)))
	then_block := t.make_block([
		t.make_expr_stmt(t.make_int_literal(t.type_index_for_type_name(variant))),
	])
	else_expr := t.build_sum_type_idx_chain(tag, sum_name, variants, idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind: .if_expr
		children_start: start
		children_count: 3
		typ: 'int'
	})
}

// try_lower_pool_generic_method_call
// supports helper handling in transform.
fn (mut t Transformer) try_lower_pool_generic_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 || node.value.len == 0 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	method := fn_node.value
	if method !in ['get_item', 'get_results', 'get_results_ref'] {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.lvalue_type(base_id)
	if !is_pool_processor_type(base_type) {
		return none
	}
	elem_type := node.value
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	if method == 'get_item' {
		if node.children_count < 2 {
			return none
		}
		idx := t.transform_expr(t.a.child(&node, 1))
		t.drain_pending(mut prefix)
		items := t.pool_processor_field(base, base_type, 'items', '[]voidptr')
		item := t.make_index(items, idx, 'voidptr')
		cast := t.make_cast('&${elem_type}', item, '&${elem_type}')
		value := t.make_prefix(.mul, cast)
		t.set_node_typ(int(value), elem_type)
		for stmt in prefix {
			t.pending_stmts << stmt
		}
		return value
	}
	result_name := t.new_temp('pool_results')
	idx_name := t.new_temp('pool_idx')
	results := t.pool_processor_field(base, base_type, 'results', '[]voidptr')
	results_len := t.make_selector(results, 'len', 'int')
	is_ref_results := method == 'get_results_ref'
	out_elem_type := if is_ref_results { '&${elem_type}' } else { elem_type }
	out_type := '[]${out_elem_type}'
	prefix << t.make_decl_assign_typed(result_name, t.make_array_new_call(out_elem_type, t.make_int_literal(0), results_len), out_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond_results := t.pool_processor_field(base, base_type, 'results', '[]voidptr')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(cond_results, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	body_results := t.pool_processor_field(base, base_type, 'results', '[]voidptr')
	item := t.make_index(body_results, t.make_ident(idx_name), 'voidptr')
	value := if is_ref_results {
		t.make_cast('&${elem_type}', item, '&${elem_type}')
	} else {
		t.make_prefix(.mul, t.make_cast('&${elem_type}', item, '&${elem_type}'))
	}
	t.set_node_typ(int(value), out_elem_type)
	value_name := t.new_temp('pool_result')
	value_decl := t.make_decl_assign_typed(value_name, value, out_elem_type)
	push_call := t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(result_name)),
		t.make_prefix(.amp, t.make_ident(value_name)),
	], 'void')
	loop_body := [value_decl, t.make_expr_stmt(push_call)]
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// is_pool_processor_type reports whether is pool processor type applies in transform.
fn is_pool_processor_type(typ string) bool {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.starts_with('mut ') {
		clean = clean[4..]
	}
	return clean == 'PoolProcessor' || clean.ends_with('.PoolProcessor')
}

// pool_processor_field supports pool processor field handling for Transformer.
fn (mut t Transformer) pool_processor_field(base flat.NodeId, base_type string, field string, typ string) flat.NodeId {
	op := if base_type.starts_with('&') { flat.Op.arrow } else { flat.Op.dot }
	return t.make_selector_op(base, field, typ, op)
}

// try_lower_receiver_method_call supports try lower receiver method call handling for Transformer.
@[direct_array_access]
fn (mut t Transformer) try_lower_receiver_method_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	method := fn_node.value
	base_id := t.a.child(&fn_node, 0)
	base_node := t.a.nodes[int(base_id)]
	// `C.fn(...)` is a namespaced C call, not a receiver method. In particular,
	// generic specialization must not diagnose the `C` namespace as a value
	// whose method is missing.
	if base_node.kind == .ident && base_node.value == 'C' {
		return none
	}
	if base_node.kind == .ident && !isnil(t.tc) {
		imported_module := t.tc.file_imports[file_import_key(t.cur_file, base_node.value)] or {
			t.tc.imports[base_node.value] or { '' }
		}
		if imported_module.len > 0 {
			if resolved := t.tc.resolved_call_name(id) {
				if resolved == '${imported_module}.${method}' {
					return none
				}
			}
		}
	}
	if t.is_import_alias_ident(base_id) {
		return none
	}
	// `Type.fn(...)` / `module.Type.fn(...)` is a static associated function, not a method:
	// the base names a type, so it must not be lowered into `fn(receiver, ...)`.
	if _ := t.static_assoc_fn_name(base_id, method) {
		return none
	}
	mut base_type := t.raw_const_type_name_for_expr(base_id) or { '' }
	mut recovered_or_value_type := false
	if base_type.len == 0 {
		base_type = if base_node.kind in [.selector, .index] {
			t.lvalue_type(base_id)
		} else {
			t.node_type(base_id)
		}
	}
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if base_type.len == 0 || base_type.trim_space().trim('&') in ['void', 'voidptr']
		|| t.generic_arg_is_unresolved(base_type) {
		if specialized_ret := t.specialized_interface_method_call_return_type(base_id, base_node) {
			base_type = specialized_ret
		}
		if unwrapped_type := t.or_expr_receiver_unwrapped_type(base_id) {
			if decl_type_is_usable(unwrapped_type) && !t.generic_arg_is_unresolved(unwrapped_type) {
				base_type = unwrapped_type
				recovered_or_value_type = true
			}
		}
	}
	if t.active_specialization_args.len > 0 {
		specialized := t.subst_type(base_type, t.active_specialization_args)
		if specialized.len > 0 && !t.generic_arg_is_unresolved(specialized) {
			base_type = specialized
		}
	}
	base_is_pointer := base_type.starts_with('&')
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.len == 0 {
		return none
	}
	if method == 'wait' && (base_type == 'thread' || base_type.starts_with('thread ')) {
		return none
	}
	iface_name := t.resolve_interface_type_name(base_type)
	if iface_name.len > 0 {
		t.mark_fn_used_name('${iface_name}.${method}')
		t.mark_interface_method_implementers_used(iface_name, method)
		if !isnil(t.tc) && method in t.tc.interface_abstract_method_names(iface_name) {
			return t.transform_interface_method_call(id, node)
		}
	}
	if method == 'close' && !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(id) {
			if resolved_method != 'chan.close' && resolved_method != 'sync__Channel__close' {
				if exact_call := t.lower_checker_selected_receiver_method(id, node, base_id, 'chan.close') {
					return exact_call
				}
			}
		}
	}
	if method == 'close' && (base_type == 'chan' || base_type.starts_with('chan ')) {
		return t.lower_runtime_channel_close(base_id, node, if base_is_pointer { 1 } else { 0 })
	}
	if method == 'close' && !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(id) {
			if resolved_method == 'chan.close' || resolved_method == 'sync__Channel__close' {
				return t.lower_runtime_channel_close(base_id, node, if base_is_pointer {
					1
				} else {
					0
				})
			}
		}
	}
	if t.cur_fn_is_generic && base_type.contains('[')
		&& t.type_text_has_generic_placeholder(base_type, t.cur_module) {
		return none
	}
	mut builtin_base_type := t.normalize_type_alias(base_type)
	if builtin_base_type == 'byte' {
		builtin_base_type = 'u8'
	}
	if base_type == '[]rune' && method == 'string' {
		return t.make_call_typed('Array_rune__string', [t.transform_expr(base_id)], 'string')
	}
	if method == 'str' && t.is_array_transform_call(base_id) {
		base := t.transform_expr(base_id)
		transformed_type := t.node_type(base)
		if transformed_type.starts_with('[]') {
			return t.wrap_string_conversion(base, transformed_type)
		}
	}
	if method == 'str' && t.is_builder_receiver(base_id, base_type) {
		method_name := 'strings.Builder.str'
		args := t.transform_receiver_method_args(node, base_id, method_name)
		return t.make_call_typed(method_name, args, 'string')
	}
	if method == 'str' {
		if smartcast_str := t.smartcast_sum_str_call(base_id) {
			return smartcast_str
		}
		if !recovered_or_value_type {
			if exact_call := t.lower_checker_selected_receiver_method(id, node, base_id, 'str') {
				return exact_call
			}
		}
		// Some calls cloned during comptime/generic lowering no longer have the
		// checker's original call-id annotation. Resolve their concrete receiver
		// method before falling back to generated auto-stringification.
		mut method_names := ['${base_type}.${method}']
		mut collection_method := ''
		if !recovered_or_value_type {
			resolved_method := t.resolve_receiver_method_name(base_id, method)
			if resolved_method.len > 0 && resolved_method !in method_names {
				method_names << resolved_method
			}
		}
		if base_type.starts_with('[]') || base_type.starts_with('map[') {
			collection_method = t.resolve_collection_receiver_method_name(base_id, method, base_type)
			if collection_method.len > 0 && collection_method !in method_names {
				method_names << collection_method
			}
		}
		for method_name in method_names {
			if t.is_known_fn_name(method_name)
				&& (t.receiver_method_matches_base_type(method_name, base_id)
					|| (collection_method.len > 0 && method_name == collection_method)) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				t.mark_fn_used_name(method_name)
				return t.make_call_typed(method_name, args, ret_type)
			}
		}
	}
	if method == 'str' && stringify_type_has_generic_placeholder(base_type) {
		return none
	}
	if base_type.starts_with('[]') && method == 'str' {
		return t.wrap_string_conversion(t.transform_expr(base_id), base_type)
	}
	if method == 'str' {
		if t.is_enum_stringify_type(base_type) {
			return none
		}
		// `(&Struct).str()` keeps the reference so the pointee is stringified with V's `&`
		// prefix (or `&nil`); primitive/alias pointers keep their existing ptr_str behavior.
		if base_is_pointer {
			raw_alias_type := t.raw_alias_type_for_expr(base_id)
			if raw_alias_type.starts_with('&') {
				if selected := t.checker_selected_receiver_method_name(id, 'str') {
					args := t.transform_receiver_method_args(node, base_id, selected)
					ret_type := t.receiver_method_return_type(selected, node.typ)
					t.mark_fn_used_name(selected)
					return t.make_call_typed(selected, args, ret_type)
				}
				return t.wrap_string_conversion(t.transform_expr(base_id), raw_alias_type)
			}
			if aggregate := t.stringify_aggregate_type_name(base_type) {
				if selected := t.checker_selected_receiver_method_name(id, 'str') {
					args := t.transform_receiver_method_args(node, base_id, selected)
					ret_type := t.receiver_method_return_type(selected, node.typ)
					t.mark_fn_used_name(selected)
					return t.make_call_typed(selected, args, ret_type)
				}
				return t.lower_ref_str_prefixed(t.transform_expr(base_id), aggregate)
			}
			if base_type.starts_with('[]') || t.is_fixed_array_type(base_type) {
				return t.lower_ref_collection_str(t.transform_expr(base_id), base_type)
			}
			return t.wrap_string_conversion(t.transform_expr(base_id), '&${base_type}')
		}
		mut stringify_type := t.raw_alias_type_for_expr(base_id)
		if stringify_type.len == 0 {
			stringify_type = t.raw_var_type_for_expr(base_id) or { base_type }
		}
		return t.wrap_string_conversion(t.transform_expr(base_id), stringify_type)
	}
	if builtin_base_type == 'string' && method == 'hex' && !base_is_pointer {
		return t.make_call_typed('string.hex', [t.transform_expr(base_id)], 'string')
	}
	if base_type == '[]u8' || base_type == '[]byte' {
		if method == 'bytestr' {
			return t.make_call_typed('Array_u8__bytestr', [t.transform_expr(base_id)], 'string')
		}
		if method == 'hex' && !base_is_pointer {
			return t.make_call_typed('Array_u8__hex', [t.transform_expr(base_id)], 'string')
		}
	}
	mut pointer_method := t.pointer_builtin_vbytes_method(base_is_pointer, builtin_base_type, method) or { '' }
	if method == 'vbytes' && !isnil(t.tc) {
		if resolved := t.tc.resolved_call_name(id) {
			if resolved in ['byteptr.vbytes', 'byteptr__vbytes', 'u8.vbytes', 'u8__vbytes'] {
				pointer_method = 'byteptr.vbytes'
			} else if resolved in ['voidptr.vbytes', 'voidptr__vbytes'] {
				pointer_method = 'voidptr.vbytes'
			}
		}
	}
	if pointer_method.len > 0 {
		args := t.transform_receiver_method_args(node, base_id, pointer_method)
		ret_type := t.receiver_method_return_type(pointer_method, node.typ)
		t.mark_fn_used(pointer_method)
		return t.make_call_typed(pointer_method, args, ret_type)
	}
	if t.is_builder_receiver(base_id, base_type) {
		for method_name in ['strings.Builder.${method}', 'Builder.${method}'] {
			if t.is_known_fn_name(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type := t.receiver_method_return_type(method_name, node.typ)
				return t.make_call_typed(method_name, args, ret_type)
			}
		}
	}
	if builtin_base_type == 'u8' && method in ['is_space', 'is_digit', 'is_hex_digit', 'is_letter'] {
		return t.make_call_typed('u8__${method}', [t.transform_expr(base_id)], 'bool')
	}
	if !base_is_pointer
		&& builtin_base_type in ['u8', 'i8', 'u16', 'i16', 'u32', 'int', 'u64', 'i64', 'rune']
		&& method in ['hex', 'hex_full'] {
		return t.make_call_typed('${builtin_base_type}__${method}', [
			t.transform_expr(base_id),
		], 'string')
	}
	if !base_is_pointer && builtin_base_type == 'voidptr' && method == 'hex_full' {
		return t.make_call_typed('voidptr__hex_full', [t.transform_expr(base_id)], 'string')
	}
	if base_type.starts_with('[]') || base_type.starts_with('map[') {
		if base_type.starts_with('[]') {
			if t.validating_generic_spec {
				if array_method_stays_in_cgen(method) {
					if !t.validate_cgen_array_method_args(node, base_id, base_type, method) {
						return t.make_empty()
					}
				} else if t.resolve_collection_receiver_method_name(base_id, method, base_type).len == 0
					&& !t.receiver_selector_is_fn_field(base_type, method) {
					base_name := if base_node.kind == .ident && base_node.value.len > 0 {
						base_node.value
					} else {
						base_type
					}
					t.record_monomorph_error('unknown function `${base_name}.${method}`')
				}
			}
			return none
		}
	}
	// An active smartcast rebinds the receiver: inside `match v { A {...} }`,
	// `v.m()` must dispatch to `A.m` even when the sum type itself also
	// declares `m` (both the checker resolution and the static resolution
	// below would pick the sum's method).
	smartcast_method := t.resolve_smartcast_sum_receiver_method(base_id, method) or { '' }
	if !isnil(t.tc) && smartcast_method.len == 0 {
		if resolved_method := t.tc.resolved_call_name(id) {
			direct_method := t.resolve_receiver_method_name(base_id, method)
			if direct_method.len > 0 && direct_method != resolved_method {
				// The declared receiver type is newer than a checker call entry captured
				// through a colliding short name. Resolve it through the normal path below.
			} else if t.receiver_method_name_is_open_generic(resolved_method) {
				return none
			} else if t.is_known_fn_name(resolved_method) {
				params := t.call_param_types(resolved_method)
				if t.resolved_call_uses_receiver_type(base_id, base_type, params) {
					if !t.validate_resolved_receiver_method_args(node, base_id, resolved_method) {
						return t.make_empty()
					}
					args := t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id, resolved_method), resolved_method)
					ret_type := t.receiver_method_return_type(resolved_method, node.typ)
					if !t.validate_specialized_call_result(id, ret_type) {
						return t.make_empty()
					}
					return t.make_receiver_method_call_typed(node, resolved_method, args, ret_type)
				}
			}
		}
	}
	mut method_name := smartcast_method
	if method_name.len == 0 {
		method_name = t.resolve_receiver_method_name(base_id, method)
	}
	if method_name.len > 0 {
		if t.receiver_method_name_is_open_generic(method_name) {
			return none
		}
		if !t.validate_resolved_receiver_method_args(node, base_id, method_name) {
			return t.make_empty()
		}
		args := t.transform_receiver_method_args(node, base_id, method_name)
		ret_type := t.receiver_method_return_type(method_name, node.typ)
		if !t.validate_specialized_call_result(id, ret_type) {
			return t.make_empty()
		}
		return t.make_receiver_method_call_typed(node, method_name, args, ret_type)
	}
	if !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(id) {
			if t.receiver_method_name_is_open_generic(resolved_method) {
				return none
			}
			if t.is_known_fn_name(resolved_method) {
				params := t.call_param_types(resolved_method)
				if !t.resolved_call_uses_receiver_type(base_id, base_type, params) {
					return none
				}
				if !t.validate_resolved_receiver_method_args(node, base_id, resolved_method) {
					return t.make_empty()
				}
				args := t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id, resolved_method), resolved_method)
				ret_type := t.receiver_method_return_type(resolved_method, node.typ)
				if !t.validate_specialized_call_result(id, ret_type) {
					return t.make_empty()
				}
				return t.make_receiver_method_call_typed(node, resolved_method, args, ret_type)
			}
		}
	}
	if sum_method := t.resolve_smartcast_sum_receiver_method(base_id, method) {
		if !t.validate_resolved_receiver_method_args(node, base_id, sum_method) {
			return t.make_empty()
		}
		args := t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id, sum_method), sum_method)
		ret_type := t.receiver_method_return_type(sum_method, node.typ)
		if !t.validate_specialized_call_result(id, ret_type) {
			return t.make_empty()
		}
		return t.make_receiver_method_call_typed(node, sum_method, args, ret_type)
	}
	if t.validating_generic_spec {
		if base_node.kind == .ident && base_node.value in ['C', 'JS'] {
			// `C.fn(...)` is an extern call, not a method on an ident.
			return none
		}
		if field_type := t.lookup_struct_field_type(base_type, method) {
			if !t.validate_specialized_fn_field_call(id, node, base_id, field_type) {
				return t.make_empty()
			}
			return none
		}
		if t.receiver_method_has_generic_template(base_type, method) {
			return none
		}
		// A marker-only `IClone` receiver has no declared method for generic
		// validation to resolve. Its call is lowered structurally by
		// try_lower_struct_clone_method_call immediately after this helper.
		if method == 'clone' && !isnil(t.tc)
			&& t.tc.named_type_implements_marker(base_type, 'IClone')
			&& !t.tc.ownership_type_has_clone_method(t.tc.parse_type(base_type)) {
			return none
		}
		base_name := if base_node.kind == .ident && base_node.value.len > 0 {
			base_node.value
		} else {
			base_type
		}
		t.record_monomorph_error('unknown function `${base_name}.${method}`')
	}
	return none
}

fn (mut t Transformer) smartcast_sum_str_call(base_id flat.NodeId) ?flat.NodeId {
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return none }
	sum_name := t.resolve_sum_name(sc.sum_type_name)
	if sum_name.len == 0 || sum_name !in t.sum_types {
		return none
	}
	target_type := t.trim_pointer_type(t.smartcast_target_type(sc))
	if target_type.len == 0 || t.is_sum_type_name(target_type) {
		return none
	}
	receiver := t.apply_smartcast_contexts(t.make_plain_expr_for_smartcast(base_id), t.original_expr_type(base_id), t.smartcasts_for(key))
	return t.wrap_string_conversion(receiver, target_type)
}

fn (mut t Transformer) receiver_method_has_generic_template(receiver_type string, method string) bool {
	if receiver_type.len == 0 || method.len == 0 || t.skip_generics {
		return false
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return false
	}
	method_keys := t.generic_receiver_methods_by_name[method] or { return false }
	for key in method_keys {
		decl := decls[key] or { continue }
		if t.generic_receiver_decl_matches_type(receiver_type, decl, t.cur_module) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) validate_resolved_receiver_method_args(node flat.Node, base_id flat.NodeId, method_name string) bool {
	if !t.validating_generic_spec {
		return true
	}
	if t.receiver_call_uses_comptime_method_selector(node) {
		return true
	}
	params := t.call_param_types(method_name)
	if params.len == 0 {
		return true
	}
	param_offset := t.receiver_method_param_offset(base_id, node, params, method_name)
	if param_offset == 0 {
		return true
	}
	mut field_init_args := 0
	for i in 1 .. node.children_count {
		if t.a.child_node(&node, i).kind == .field_init {
			field_init_args++
		}
	}
	collapsed_fields := if field_init_args > 0 { 1 } else { 0 }
	actual_count := int(node.children_count) - 1 - field_init_args + collapsed_fields
	expected_count := params.len - param_offset
	is_variadic := t.call_is_variadic(method_name) && params[params.len - 1] is types.Array
	mut min_count := expected_count
	mut trailing_idx := params.len - 1
	for trailing_idx >= param_offset {
		if is_variadic && trailing_idx == params.len - 1 {
			min_count--
			trailing_idx--
			continue
		}
		if _ := t.params_struct_type_name(t.semantic_type_name(params[trailing_idx])) {
			min_count--
			trailing_idx--
			continue
		}
		break
	}
	display_name := t.resolved_receiver_call_display_name(node, base_id, method_name)
	if actual_count < min_count || (!is_variadic && actual_count > expected_count) {
		t.record_monomorph_error('argument count mismatch for `${display_name}`: expected ${expected_count}, got ${actual_count}')
		return false
	}
	mut valid := true
	mut arg_idx := 0
	mut child_idx := 1
	for child_idx < node.children_count {
		arg_id := t.a.child(&node, child_idx)
		arg_node := t.a.nodes[int(arg_id)]
		param_idx := param_offset + arg_idx
		mut expected := params[if param_idx < params.len { param_idx } else { params.len - 1 }]
		if arg_node.kind == .field_init {
			expected_name := t.semantic_type_name(expected)
			struct_type := t.params_struct_type_name(expected_name) or {
				t.struct_arg_type_name(expected_name) or { '' }
			}
			if struct_type.len == 0
				|| !t.validate_specialized_struct_field_args(node, child_idx, struct_type) {
				valid = false
			}
			child_idx = t.next_non_field_init_arg(node, child_idx)
			arg_idx++
			continue
		}
		if is_variadic && param_idx >= params.len - 1 {
			variadic_type := params[params.len - 1]
			if variadic_type is types.Array {
				if arg_node.kind == .prefix && arg_node.value == '...' {
					if arg_node.children_count > 0 {
						spread_id := t.a.child(&arg_node, 0)
						actual_name := t.specialized_expr_type_name(spread_id)
						expected_name := t.semantic_type_name(params[params.len - 1])
						if !t.resolved_receiver_arg_compatible(spread_id, actual_name, expected_name) {
							t.record_monomorph_error('cannot use `${actual_name}` as argument ${arg_idx + 1} to `${display_name}`; expected `${expected_name}`')
							valid = false
						}
					}
					child_idx++
					arg_idx++
					continue
				}
				expected = variadic_type.elem_type
			}
		}
		actual_name := t.specialized_expr_type_name(arg_id)
		expected_name := t.semantic_type_name(expected)
		if t.resolved_receiver_arg_compatible(arg_id, actual_name, expected_name) {
			child_idx++
			arg_idx++
			continue
		}
		t.record_monomorph_error('cannot use `${actual_name}` as argument ${arg_idx + 1} to `${display_name}`; expected `${expected_name}`')
		valid = false
		child_idx++
		arg_idx++
	}
	return valid
}

fn (t &Transformer) receiver_call_uses_comptime_method_selector(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_node := t.a.child_node(&node, 0)
	return fn_node.kind == .selector && comptime_method_selector_marker in fn_node.generic_params()
}

fn (mut t Transformer) validate_specialized_struct_field_args(node flat.Node, field_start int, struct_type string) bool {
	mut valid := true
	mut i := field_start
	for i < node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			break
		}
		field_type := t.lookup_struct_field_type(struct_type, field.value) or {
			t.record_monomorph_error('unknown field `${field.value}` in `${struct_type.all_after_last('.')}`')
			valid = false
			i++
			continue
		}
		if field.children_count == 0 {
			i++
			continue
		}
		value_id := t.a.child(field, 0)
		actual_type := t.specialized_expr_type_name(value_id)
		if !t.resolved_receiver_arg_compatible(value_id, actual_type, field_type) {
			t.record_monomorph_error('cannot initialize field `${field.value}` with `${actual_type}`; expected `${field_type}`')
			valid = false
		}
		i++
	}
	return valid
}

fn (mut t Transformer) validate_specialized_fn_field_call(id flat.NodeId, node flat.Node, base_id flat.NodeId, field_type string) bool {
	display_name := t.resolved_receiver_call_display_name(node, base_id, '')
	if isnil(t.tc) {
		return true
	}
	fn_type := transform_fn_type(t.tc.parse_type(field_type)) or {
		t.record_monomorph_error('unknown function `${display_name}`')
		return false
	}
	actual_count := int(node.children_count) - 1
	if actual_count != fn_type.params.len {
		t.record_monomorph_error('argument count mismatch for `${display_name}`: expected ${fn_type.params.len}, got ${actual_count}')
		return false
	}
	mut valid := true
	for i in 0 .. actual_count {
		arg_id := t.a.child(&node, i + 1)
		actual_type := t.specialized_expr_type_name(arg_id)
		expected_type := t.semantic_type_name(fn_type.params[i])
		if t.resolved_receiver_arg_compatible(arg_id, actual_type, expected_type) {
			continue
		}
		if t.fn_field_arg_compatible(actual_type, expected_type) {
			continue
		}
		t.record_monomorph_error('cannot use `${actual_type}` as argument ${i + 1} to `${display_name}`; expected `${expected_type}`')
		valid = false
	}
	if !t.validate_specialized_call_result(id, t.semantic_type_name(fn_type.return_type)) {
		valid = false
	}
	return valid
}

fn transform_fn_type(typ types.Type) ?types.FnType {
	if typ is types.FnType {
		return typ
	}
	if typ is types.Alias {
		return transform_fn_type(typ.base_type)
	}
	if typ is types.Pointer {
		return transform_fn_type(typ.base_type)
	}
	return none
}

fn (mut t Transformer) fn_field_arg_compatible(actual_type string, expected_type string) bool {
	actual := t.normalize_type_alias(actual_type)
	expected := t.normalize_type_alias(expected_type)
	if t.is_integer_type_name(actual) && t.is_integer_type_name(expected) {
		return true
	}
	if actual == expected {
		return true
	}
	if expected == '&void' && actual in ['voidptr', '&void', 'nil'] {
		return true
	}
	if expected == '&void' && (actual.starts_with('&') || actual in ['byteptr', 'charptr']) {
		return true
	}
	if actual == '&void'
		&& (expected in ['voidptr', '&void', 'byteptr', 'charptr'] || expected.starts_with('&')) {
		return true
	}
	return false
}

fn (mut t Transformer) validate_specialized_call_result(id flat.NodeId, actual_type string) bool {
	if !t.validating_generic_spec || t.expected_expr_node != int(id)
		|| t.expected_expr_type.len == 0 || t.expected_expr_type in ['unknown', 'void'] {
		return true
	}
	expected_type := t.expected_expr_type
	// An unsubstituted single-letter generic return still lacks a concrete type
	// at this validation point. The specialized function signature/C compiler
	// will validate it once monomorphization has supplied the concrete argument.
	if expected_type.len == 1 && expected_type[0] >= `A` && expected_type[0] <= `Z` {
		return true
	}
	if t.resolved_receiver_arg_compatible(id, actual_type, expected_type) {
		return true
	}
	// Reflection permits assigning an integer decoder result to the concrete
	// enum selected by `$for field in T.fields` under `$if field.is_enum`.
	// Keep that exception scoped to the unrolled enum-field body; ordinary
	// generic assignments must still report an int-to-enum mismatch.
	if t.allow_comptime_enum_int_assign && t.is_integer_type_name(actual_type)
		&& t.is_formatted_enum_type(expected_type) {
		return true
	}
	if t.in_return_expr {
		t.record_monomorph_error('cannot return `${actual_type}` as `${expected_type}`')
	} else {
		t.record_monomorph_error('cannot use `${actual_type}` as `${expected_type}`')
	}
	return false
}

fn (mut t Transformer) validate_specialized_comparison_operands(node flat.Node, lhs_id flat.NodeId, rhs_id flat.NodeId, transformed_lhs flat.NodeId, transformed_rhs flat.NodeId) bool {
	if !t.validating_generic_spec || node.op !in [.eq, .ne, .lt, .gt, .le, .ge] {
		return true
	}
	lhs_is_call := t.specialized_comparison_operand_is_receiver_call(lhs_id)
	rhs_is_call := t.specialized_comparison_operand_is_receiver_call(rhs_id)
	if !lhs_is_call && !rhs_is_call {
		return true
	}
	lhs_type := t.specialized_expr_type_name(transformed_lhs)
	rhs_type := t.specialized_expr_type_name(transformed_rhs)
	if t.specialized_comparison_types_compatible(transformed_lhs, lhs_type, transformed_rhs, rhs_type) {
		return true
	}
	if lhs_is_call {
		t.record_monomorph_error('cannot use `${lhs_type}` as `${rhs_type}`')
	} else {
		t.record_monomorph_error('cannot use `${rhs_type}` as `${lhs_type}`')
	}
	return false
}

fn (t &Transformer) specialized_comparison_operand_is_receiver_call(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.specialized_comparison_operand_is_receiver_call(t.a.child(&node, 0))
	}
	if node.kind != .call || node.children_count == 0 {
		return false
	}
	callee := t.a.child_node(&node, 0)
	return callee.kind == .selector && callee.children_count > 0
}

fn (mut t Transformer) specialized_comparison_types_compatible(lhs_id flat.NodeId, lhs_type string, rhs_id flat.NodeId, rhs_type string) bool {
	if lhs_type.len == 0 || rhs_type.len == 0 || lhs_type == 'unknown' || rhs_type == 'unknown' {
		return true
	}
	lhs := t.normalize_type_alias(lhs_type)
	rhs := t.normalize_type_alias(rhs_type)
	if lhs == rhs {
		return true
	}
	lhs_is_number := t.is_integer_type_name(lhs) || lhs in ['f32', 'f64']
	rhs_is_number := t.is_integer_type_name(rhs) || rhs in ['f32', 'f64']
	if lhs_is_number && rhs_is_number {
		return true
	}
	return t.resolved_receiver_arg_compatible(lhs_id, lhs_type, rhs_type)
		|| t.resolved_receiver_arg_compatible(rhs_id, rhs_type, lhs_type)
}

fn (mut t Transformer) specialized_expr_type_name(id flat.NodeId) string {
	if t.specialized_expr_is_none(id) {
		return 'none'
	}
	mut typ := t.resolve_expr_type(id)
	if typ.len == 0 {
		typ = t.node_type(id)
	}
	if typ.len == 0 {
		typ = t.reliable_stringify_type(id)
	}
	return typ
}

fn (t &Transformer) resolved_receiver_call_display_name(node flat.Node, base_id flat.NodeId, method_name string) string {
	base := t.a.nodes[int(base_id)]
	base_name := if base.kind == .ident && base.value.len > 0 {
		base.value
	} else {
		t.node_type(base_id)
	}
	if node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .selector && callee.value.len > 0 {
			return '${base_name}.${callee.value}'
		}
	}
	method := if method_name.contains('.') { method_name.all_after_last('.') } else { method_name }
	return '${base_name}.${method}'
}

fn (mut t Transformer) resolved_receiver_arg_compatible(arg_id flat.NodeId, actual_type string, expected_type string) bool {
	if actual_type.len == 0 || actual_type == 'unknown' || expected_type.len == 0 {
		return true
	}
	// An `unknown` expected type means the callee signature still carries an
	// unresolved generic parameter here - nothing can be validated against it
	// (including container forms such as `[]unknown`).
	if expected_type.contains('unknown') {
		return true
	}
	actual := t.normalize_type_alias(actual_type)
	expected := t.normalize_type_alias(expected_type)
	if t.is_integer_type_name(expected) {
		if literal := t.specialized_int_literal(arg_id) {
			return specialized_int_literal_fits_type(literal, expected)
		}
	}
	if t.is_integer_type_name(actual) && t.is_integer_type_name(expected) {
		return true
	}
	if actual == expected {
		return true
	}
	if t.expr_is_nil_like(arg_id)
		&& (expected.starts_with('&') || expected in ['voidptr', 'byteptr', 'charptr']) {
		return true
	}
	// A `mut`/pointer parameter is called with the value form (`r.read(mut
	// buf)` with `buf []u8` against `&[]u8`); cgen auto-refs such args.
	if expected.starts_with('&') && actual == expected[1..] {
		return true
	}
	// V also permits the inverse value coercion. This covers a borrowed value
	// passed to a by-value parameter and `&param` inside a specialized function
	// where a source `mut T` parameter already has `&T` storage after lowering.
	if actual.starts_with('&') && actual[1..] == expected {
		return true
	}
	if expected == '&void'
		&& (actual.starts_with('&') || actual in ['voidptr', 'byteptr', 'charptr', 'nil']) {
		return true
	}
	// Any pointer converts to voidptr.
	if expected in ['voidptr', 'byteptr', 'charptr']
		&& (actual.starts_with('&') || actual in ['voidptr', 'byteptr', 'charptr', 'nil']) {
		return true
	}
	// An empty array literal (`[]`) types as `[]void` and adopts the
	// parameter's element type.
	if actual == '[]void' && expected.starts_with('[]') {
		return true
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .float_literal && expected in ['f32', 'f64'] {
		return true
	}
	// A char literal (`\`x\``) types as rune but coerces to u8 params.
	if arg.kind == .char_literal && expected in ['u8', 'rune', 'char', 'int', 'u32'] {
		return true
	}
	if actual == 'nil'
		&& (expected.starts_with('&') || expected in ['voidptr', 'byteptr', 'charptr']) {
		return true
	}
	if expected.starts_with('?') {
		if actual == expected[1..] || t.specialized_expr_is_none(arg_id) {
			return true
		}
	}
	if expected.starts_with('!') {
		if actual == expected[1..] || t.is_ierror_type(actual) {
			return true
		}
	}
	if t.is_sum_type_name(expected) && t.sum_target_accepts_variant_type(expected, actual) {
		return true
	}
	// The reverse also passes validation: a sum-typed value flows into a
	// variant-typed parameter under an `is`-guard (smartcast) that a not-yet
	// unrolled `$for v in T.variants` body cannot expose to this check.
	if t.is_sum_type_name(actual) && t.sum_target_accepts_variant_type(actual, expected) {
		return true
	}
	if !isnil(t.tc) {
		expected_interface := t.trim_all_pointer_type(expected)
		actual_interface := t.trim_all_pointer_type(actual)
		if expected_interface in t.tc.interface_names
			&& t.tc.type_text_implements_interface(actual_interface, expected_interface) {
			return true
		}
	}
	// Generic applications may differ only in module qualification of their
	// type arguments (`json2.Node[ValueInfo]` vs `json2.Node[json2.ValueInfo]`).
	mut a_app := actual
	mut e_app := expected
	for a_app.starts_with('&') && e_app.starts_with('&') {
		a_app = a_app[1..]
		e_app = e_app[1..]
	}
	if !isnil(t.tc) && t.tc.generic_type_name_matches(a_app, e_app) {
		return true
	}
	return false
}

struct SpecializedIntLiteral {
	negative  bool
	magnitude u64
}

fn (t &Transformer) specialized_int_literal(id flat.NodeId) ?SpecializedIntLiteral {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return t.specialized_int_literal(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.children_count > 0 && node.op in [.plus, .minus] {
		mut literal := t.specialized_int_literal(t.a.child(&node, 0)) or { return none }
		if node.op == .minus && literal.magnitude != 0 {
			literal = SpecializedIntLiteral{
				negative: !literal.negative
				magnitude: literal.magnitude
			}
		}
		return literal
	}
	if node.kind != .int_literal {
		return none
	}
	magnitude := specialized_uint_literal_value(node.value) or { return none }
	return SpecializedIntLiteral{
		magnitude: magnitude
	}
}

fn specialized_uint_literal_value(text string) ?u64 {
	clean := text.replace('_', '')
	if clean.len == 0 {
		return none
	}
	mut base := u64(10)
	mut start := 0
	if clean.len >= 2 && clean[0] == `0` {
		match clean[1] {
			`x`, `X` {
				base = 16
				start = 2
			}
			`o`, `O` {
				base = 8
				start = 2
			}
			`b`, `B` {
				base = 2
				start = 2
			}
			else {}
		}
	}
	if start >= clean.len {
		return none
	}
	mut value := u64(0)
	for i in start .. clean.len {
		ch := clean[i]
		digit := if ch >= `0` && ch <= `9` {
			u64(ch - `0`)
		} else if ch >= `a` && ch <= `f` {
			u64(ch - `a`) + 10
		} else if ch >= `A` && ch <= `F` {
			u64(ch - `A`) + 10
		} else {
			return none
		}
		if digit >= base || value > (max_u64 - digit) / base {
			return none
		}
		value = value * base + digit
	}
	return value
}

fn specialized_int_literal_fits_type(literal SpecializedIntLiteral, typ string) bool {
	return match typ {
		'u8', 'byte' { !literal.negative && literal.magnitude <= 255 }
		'u16' { !literal.negative && literal.magnitude <= 65535 }
		'u32' { !literal.negative && literal.magnitude <= u64(4294967295) }
		'u64', 'usize' { !literal.negative }
		'i8' { specialized_signed_literal_fits(literal, 127) }
		'i16' { specialized_signed_literal_fits(literal, 32767) }
		'int', 'i32', 'rune' { specialized_signed_literal_fits(literal, 2147483647) }
		'i64', 'isize' { specialized_signed_literal_fits(literal, u64(max_i64)) }
		else { true }
	}
}

fn specialized_signed_literal_fits(literal SpecializedIntLiteral, max u64) bool {
	if literal.negative {
		return literal.magnitude <= max + 1
	}
	return literal.magnitude <= max
}

fn (t &Transformer) specialized_expr_is_none(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .none_expr {
		return true
	}
	if node.kind == .paren && node.children_count > 0 {
		return t.specialized_expr_is_none(t.a.child(&node, 0))
	}
	return false
}

fn (t &Transformer) receiver_selector_is_fn_field(base_type string, field string) bool {
	field_type := t.lookup_struct_field_type(base_type, field) or { return false }
	clean := t.normalize_type_alias(field_type)
	return clean.starts_with('fn(') || clean.starts_with('fn (')
}

fn (mut t Transformer) record_monomorph_error(message string) {
	key := '${t.cur_file}:${t.cur_fn_name}:${message}'
	if t.monomorph_error_seen[key] {
		return
	}
	t.monomorph_error_seen[key] = true
	t.monomorph_errors << message
}

fn (t &Transformer) pointer_builtin_vbytes_method(base_is_pointer bool, builtin_base_type string, method string) ?string {
	if method != 'vbytes' {
		return none
	}
	if base_is_pointer && builtin_base_type == 'u8' {
		return 'byteptr.vbytes'
	}
	if builtin_base_type in ['byteptr', 'voidptr'] {
		return '${builtin_base_type}.vbytes'
	}
	return none
}

fn (t &Transformer) receiver_method_name_is_open_generic(method_name string) bool {
	if !isnil(t.tc) {
		if method_name in t.tc.fn_generic_params {
			return true
		}
		if method_name.contains('__') {
			dotted := method_name.replace('__', '.')
			if dotted in t.tc.fn_generic_params {
				return true
			}
		}
	}
	if method_name.contains('.') {
		receiver := owner_name_view(method_name)
		if collection_receiver := receiver_collection_method_type(receiver) {
			return t.generic_arg_is_unresolved_collection_type(collection_receiver)
		}
		_, args, ok := generic_app_parts(receiver)
		return ok && args.len > 0 && t.generic_args_have_placeholders(args)
	}
	return method_name_contains_mangled_open_generic_placeholder(method_name)
}

fn receiver_collection_method_type(receiver string) ?string {
	if receiver.starts_with('[]') || receiver.starts_with('map[') {
		return receiver
	}
	if receiver.contains('.[]') {
		return '[]${receiver.all_after('.[]')}'
	}
	if receiver.contains('.map[') {
		return 'map[${receiver.all_after('.map[')}'
	}
	return none
}

fn (t &Transformer) generic_arg_is_unresolved_collection_type(receiver string) bool {
	if receiver.starts_with('[]') {
		return t.generic_arg_is_unresolved(receiver[2..])
	}
	if receiver.starts_with('map[') {
		return t.generic_arg_is_unresolved(t.map_key_type(receiver))
			|| t.generic_arg_is_unresolved(t.map_value_type(receiver))
	}
	return false
}

fn method_name_contains_mangled_open_generic_placeholder(method_name string) bool {
	if method_name.len < 4 {
		return false
	}
	for i in 0 .. method_name.len - 3 {
		letter := method_name[i + 1]
		if method_name[i] == `_` && letter >= `A` && letter <= `Z` && method_name[i + 2] == `_`
			&& method_name[i + 3] == `_` {
			return true
		}
	}
	return false
}

// is_builder_receiver reports whether is builder receiver applies in transform.
fn (t &Transformer) is_builder_receiver(base_id flat.NodeId, base_type string) bool {
	if is_builder_type_name(base_type) {
		return true
	}
	if raw_type := t.raw_var_type_for_expr(base_id) {
		return is_builder_type_name(raw_type)
	}
	if raw_field_type := t.raw_selector_field_type(base_id) {
		return is_builder_type_name(raw_field_type)
	}
	return false
}

// raw_selector_field_type supports raw selector field type handling for Transformer.
fn (t &Transformer) raw_selector_field_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.original_expr_type(base_id)
	if base_type.len == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	raw_type, owner_type := t.lookup_struct_field_raw_type_with_owner(base_type, node.value) or {
		return none
	}
	// Preserve an alias for auto-stringification, but use the checker's canonical
	// spelling instead of rebinding the declaration's bare source spelling in the
	// caller module. For example, `other.Holder.typ Type` is `other.Type`, even when
	// the importing module declares an unrelated `Type`.
	if canonical := t.checker_struct_field_type_name(owner_type, node.value) {
		if t.is_type_alias_name(t.trim_pointer_type(canonical)) {
			return canonical
		}
	}
	return raw_type
}

// is_builder_type_name reports whether is builder type name applies in transform.
fn is_builder_type_name(typ string) bool {
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	return clean == 'strings.Builder' || clean == 'Builder'
}

// resolved_call_uses_receiver_type
// supports helper handling in transform.
fn (t &Transformer) resolved_call_uses_receiver_type(base_id flat.NodeId, receiver_type string, params []types.Type) bool {
	if params.len == 0 {
		return false
	}
	mut param_type := t.semantic_type_name(params[0])
	if param_type.starts_with('&') {
		param_type = param_type[1..]
	}
	if sc := t.find_smartcast(t.expr_key(base_id)) {
		sc_type := t.trim_pointer_type(t.smartcast_target_type(sc))
		if sc_type.len > 0 && t.normalize_type_alias(sc_type) == t.normalize_type_alias(param_type) {
			return true
		}
		sc_sum := t.trim_pointer_type(t.resolve_sum_name(sc.sum_type_name))
		if sc_sum.len > 0 && t.normalize_type_alias(sc_sum) == t.normalize_type_alias(param_type) {
			return true
		}
		if t.is_sum_type_name(param_type) {
			for parent in t.sum_type_parents_for_variant(sc.variant_name) {
				if t.normalize_type_alias(parent) == t.normalize_type_alias(param_type) {
					return true
				}
			}
		}
	}
	mut base_type := receiver_type
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if raw_type := t.raw_var_type_for_expr(base_id) {
		raw_base_type := t.trim_pointer_type(raw_type)
		if raw_base_type.len > 0 && !t.generic_arg_is_unresolved(raw_base_type)
			&& t.normalize_type_alias(raw_base_type) != t.normalize_type_alias(base_type) {
			if t.normalize_type_alias(raw_base_type) == t.normalize_type_alias(param_type) {
				return true
			}
			if _ := t.embedded_receiver_path(raw_base_type, param_type) {
				return true
			}
			return false
		}
	}
	if base_type.len == 0 {
		return true
	}
	if t.normalize_type_alias(base_type) == t.normalize_type_alias(param_type) {
		return true
	}
	if _ := t.embedded_receiver_path(base_type, param_type) {
		return true
	}
	return false
}

// receiver_base_for_resolved_method
// supports helper handling in transform.
fn (mut t Transformer) receiver_base_for_resolved_method(base_id flat.NodeId, method_name string) flat.NodeId {
	method_receiver := t.trim_pointer_type(owner_name_view(method_name))
	key := t.expr_key(base_id)
	for source_type in [t.raw_var_type_for_expr(base_id) or { '' }, t.original_expr_type(base_id),
		t.node_type(base_id)] {
		clean_source := t.trim_pointer_type(source_type)
		if clean_source.len > 0 && method_receiver.len > 0
			&& t.normalize_type_alias(clean_source) == t.normalize_type_alias(method_receiver) {
			if t.is_sum_type_name(method_receiver) {
				if sc := t.find_smartcast(key) {
					original_type := t.trim_pointer_type(t.original_expr_type(base_id))
					sum_type := t.trim_pointer_type(t.resolve_sum_name(sc.sum_type_name))
					original_matches := original_type.len > 0
						&& t.normalize_type_alias(original_type) == t.normalize_type_alias(method_receiver)
					sum_matches := sum_type.len > 0
						&& t.normalize_type_alias(sum_type) == t.normalize_type_alias(method_receiver)
					if original_matches || sum_matches {
						return t.make_plain_expr_for_smartcast(base_id)
					}
				}
			}
			return t.transform_expr(base_id)
		}
		if alias_target := t.alias_target_type_preserving_main_lock(clean_source) {
			if method_receiver.len > 0
				&& short_name_view(t.trim_pointer_type(alias_target)) == short_name_view(method_receiver) {
				return t.transform_expr(base_id)
			}
		}
	}
	if embedded_base := t.embedded_receiver_base(base_id, method_name) {
		return embedded_base
	}
	sc := t.find_smartcast(key) or { return t.transform_expr(base_id) }
	params := t.call_param_types(method_name)
	if params.len == 0 {
		return t.transform_expr(base_id)
	}
	mut param_type := t.semantic_type_name(params[0])
	if param_type.starts_with('&') {
		param_type = param_type[1..]
	}
	target_type := t.trim_pointer_type(t.smartcast_target_type(sc))
	if target_type.len > 0 {
		smartcast_base := t.apply_smartcast_contexts(t.make_plain_expr_for_smartcast(base_id), t.original_expr_type(base_id), t.smartcasts_for(key))
		if embedded_base := t.embedded_receiver_base_for_type(smartcast_base, target_type, param_type) {
			return embedded_base
		}
	}
	if target_type.len > 0
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(target_type) {
		return t.apply_smartcast_contexts(t.make_plain_expr_for_smartcast(base_id), t.original_expr_type(base_id), t.smartcasts_for(key))
	}
	if method_receiver.len > 0 && t.is_sum_type_name(method_receiver)
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(method_receiver) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	mut sum_type := t.resolve_sum_name(sc.sum_type_name)
	if sum_type.starts_with('&') {
		sum_type = sum_type[1..]
	}
	if sum_type.len > 0 && t.normalize_type_alias(param_type) == t.normalize_type_alias(sum_type) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	original_type := t.trim_pointer_type(t.original_expr_type(base_id))
	if original_type.len > 0
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(original_type) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	return t.transform_expr(base_id)
}

fn (mut t Transformer) embedded_receiver_base(base_id flat.NodeId, method_name string) ?flat.NodeId {
	params := t.call_param_types(method_name)
	if params.len == 0 {
		return none
	}
	mut param_type := t.semantic_type_name(params[0])
	if param_type.starts_with('&') {
		param_type = param_type[1..]
	}
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if t.active_specialization_args.len > 0 {
		specialized := t.subst_type(base_type, t.active_specialization_args)
		if specialized.len > 0 && !t.generic_arg_is_unresolved(specialized) {
			base_type = specialized
		}
	}
	mut is_ptr := false
	if base_type.starts_with('&') {
		is_ptr = true
		base_type = base_type[1..]
	}
	// Check the embedding path before transforming the receiver. Transforming it
	// speculatively can emit pending statements; falling back afterward would
	// transform and evaluate a side-effecting receiver a second time.
	_ := t.embedded_receiver_path(base_type, param_type) or { return none }
	return t.embedded_receiver_base_for_type(t.transform_expr(base_id), if is_ptr {
		'&${base_type}'
	} else {
		base_type
	}, param_type)
}

fn (mut t Transformer) embedded_receiver_base_for_type(base flat.NodeId, base_type0 string, receiver_type string) ?flat.NodeId {
	mut base_type := base_type0
	mut is_ptr := false
	if base_type.starts_with('&') {
		is_ptr = true
		base_type = base_type[1..]
	}
	path := t.embedded_receiver_path(base_type, receiver_type) or { return none }
	mut cur := base
	mut current_is_ptr := is_ptr
	for field in path {
		op := if current_is_ptr { flat.Op.arrow } else { flat.Op.dot }
		field_type := if field.typ.len > 0 { field.typ } else { field.raw_typ }
		cur = t.make_selector_op(cur, field.name, field_type, op)
		current_is_ptr = field_type.starts_with('&')
	}
	return cur
}

fn (t &Transformer) embedded_receiver_field(base_type string, receiver_type string) ?FieldInfo {
	path := t.embedded_receiver_path(base_type, receiver_type) or { return none }
	if path.len == 0 {
		return none
	}
	return path[0]
}

fn (t &Transformer) embedded_receiver_path(base_type string, receiver_type string) ?[]FieldInfo {
	if base_type.len == 0 || receiver_type.len == 0 {
		return none
	}
	mut lookup_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if alias_target := t.alias_target_type_preserving_main_lock(lookup_type) {
		if alias_target != lookup_type {
			if path := t.embedded_receiver_path(alias_target, receiver_type) {
				return path
			}
		}
	}
	if lookup_type !in t.structs && lookup_type.contains('.') {
		short_type := lookup_type.all_after_last('.')
		if short_type in t.structs {
			lookup_type = short_type
		}
	}
	fields := t.embedded_fields[lookup_type] or { return none }
	clean_receiver := t.normalize_type_alias(receiver_type)
	for field in fields {
		// The semantic type of an embedded alias can be its underlying function
		// type. Keep the source alias spelling for promoted receiver matching.
		field_type := if field.raw_typ.len > 0 { field.raw_typ } else { field.typ }
		raw_field := field_type.trim_left('&')
		clean_field := t.normalize_type_alias(raw_field)
		short_field := if clean_field.contains('.') {
			clean_field.all_after_last('.')
		} else {
			clean_field
		}
		short_raw_field := if raw_field.contains('.') {
			raw_field.all_after_last('.')
		} else {
			raw_field
		}
		if field.name in [raw_field, short_raw_field, clean_field, short_field]
			&& clean_field == clean_receiver {
			return [field]
		}
		if sub_path := t.embedded_receiver_path(clean_field, receiver_type) {
			mut path := []FieldInfo{}
			path << field
			path << sub_path
			return path
		}
	}
	return none
}

// receiver_method_return_type supports receiver method return type handling for Transformer.
fn (t &Transformer) receiver_method_return_type(method_name string, fallback string) string {
	if !isnil(t.tc) {
		if typ := t.tc.fn_ret_types[method_name] {
			if typ !is types.Unknown && typ !is types.Void {
				return t.semantic_type_name(typ)
			}
		}
		if ret_text := t.tc.fn_ret_type_texts[method_name] {
			if alias_text := t.fn_return_alias_type_text(method_name, ret_text) {
				return alias_text
			}
		}
	}
	if ret := t.fn_ret_types[method_name] {
		return ret
	}
	return fallback
}

fn (t &Transformer) fn_return_alias_type_text(method_name string, ret_text string) ?string {
	clean := ret_text.trim_space()
	if clean.len == 0 || isnil(t.tc) {
		return none
	}
	if _, _ := t.lookup_str_alias(clean) {
		return clean
	}
	if clean.contains('.') {
		return none
	}
	mod := t.tc.fn_type_modules[method_name] or { '' }
	if mod.len == 0 || mod in ['main', 'builtin'] {
		return none
	}
	qname := '${mod}.${clean}'
	if _, _ := t.lookup_str_alias(qname) {
		return qname
	}
	return none
}

fn (t &Transformer) call_resolved_to_method(call_id flat.NodeId, method_name string) bool {
	if isnil(t.tc) {
		return true
	}
	if resolved := t.tc.resolved_call_name(call_id) {
		return resolved == method_name
	}
	return false
}

fn (mut t Transformer) lower_checker_selected_receiver_method(call_id flat.NodeId, node flat.Node, base_id flat.NodeId, builtin_name string) ?flat.NodeId {
	resolved := t.checker_selected_receiver_method_name(call_id, builtin_name) or { return none }
	if !t.receiver_method_matches_base_type(resolved, base_id) {
		return none
	}
	args := t.transform_receiver_method_args(node, base_id, resolved)
	ret_type := t.receiver_method_return_type(resolved, node.typ)
	t.mark_fn_used_name(resolved)
	return t.make_receiver_method_call_typed(node, resolved, args, ret_type)
}

fn (mut t Transformer) make_receiver_method_call_typed(node flat.Node, method_name string, args []flat.NodeId, typ string) flat.NodeId {
	// Reachability was computed before generic/comptime clones were transformed.
	// Retain methods selected while lowering those generated bodies as well.
	t.mark_fn_used_name(method_name)
	call := t.make_call_typed(method_name, args, typ)
	generic_args := t.explicit_generic_call_arg_text(node)
	if generic_args.len > 0 {
		t.set_node_value(int(call), generic_args)
	}
	return call
}

fn (t &Transformer) explicit_generic_call_arg_text(node flat.Node) string {
	if node.value.len > 0 {
		return node.value
	}
	if node.children_count == 0 {
		return ''
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .index || fn_node.children_count < 2 || fn_node.value == 'range' {
		return ''
	}
	return t.generic_call_type_args_name(fn_node)
}

fn (t &Transformer) receiver_method_matches_base_type(method_name string, base_id flat.NodeId) bool {
	receiver_name := method_name.all_before_last('.')
	if receiver_name.len == 0 {
		return true
	}
	mut checker_base_type := t.raw_checker_node_type(base_id)
	for checker_base_type.starts_with('&') {
		checker_base_type = checker_base_type[1..]
	}
	if receiver_name == checker_base_type {
		return true
	}
	mut base_type := t.node_type(base_id)
	if base_type.len == 0 && !isnil(t.tc) {
		base_type = t.semantic_type_name(t.tc.resolve_type(base_id))
	}
	for base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	base_type = t.normalize_type_alias(base_type)
	if base_type.len == 0 {
		return true
	}
	if base_type.starts_with('[]') || base_type.starts_with('map[') {
		method := method_name.all_after_last('.')
		if method_name in t.receiver_method_candidates(base_type, method) {
			return true
		}
	}
	if receiver_name == base_type {
		return true
	}
	if alias_target := t.alias_target_type_preserving_main_lock(base_type) {
		clean_target := t.trim_pointer_type(alias_target)
		if receiver_name == clean_target
			|| receiver_name.all_after_last('.') == clean_target.all_after_last('.') {
			return true
		}
	}
	if !isnil(t.tc) {
		resolved_base_type := t.tc.resolve_imported_type_text_in_file(base_type, t.cur_file)
		if receiver_name == resolved_base_type {
			return true
		}
	}
	if !base_type.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && receiver_name == '${t.cur_module}.${base_type}' {
		return true
	}
	if receiver_name.all_after_last('.') != base_type.all_after_last('.') {
		return false
	}
	// An unqualified base type is the short form of the qualified receiver when their
	// short names match (a selective import renders the receiver as bare `Command`,
	// but its method is registered under `cli.Command`). A genuine bare/main receiver
	// hits the exact-match branch above, so it never reaches here.
	if !base_type.contains('.') && receiver_name.contains('.')
		&& receiver_name.all_after_last('.') == base_type {
		return true
	}
	return false
}

fn (t &Transformer) receiver_method_matches_type_name(method_name string, typ string) bool {
	receiver_name := method_name.all_before_last('.')
	if receiver_name.len == 0 {
		return true
	}
	mut clean := typ.trim_space()
	for clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.starts_with('[]') || clean.starts_with('map[') {
		method := method_name.all_after_last('.')
		if method_name in t.receiver_method_candidates(clean, method) {
			return true
		}
	}
	mut candidates := [clean]
	if clean.contains('.') {
		candidates << clean.all_after_last('.')
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		candidates << '${t.cur_module}.${clean}'
	}
	normalized := t.normalize_type_alias(clean)
	if normalized.len > 0 && normalized !in candidates {
		candidates << normalized
	}
	if alias_target := t.alias_target_type_preserving_main_lock(clean) {
		clean_target := t.trim_pointer_type(alias_target)
		if clean_target !in candidates {
			candidates << clean_target
		}
		short_target := clean_target.all_after_last('.')
		if short_target !in candidates {
			candidates << short_target
		}
	}
	return receiver_name in candidates
}

fn (t &Transformer) checker_selected_receiver_method_name(call_id flat.NodeId, builtin_name string) ?string {
	if isnil(t.tc) {
		return none
	}
	resolved := t.tc.resolved_call_name(call_id) or { return none }
	if t.receiver_method_name_is_open_generic(resolved) {
		return none
	}
	if resolved == builtin_name {
		return none
	}
	if is_builtin_collection_resolved_call(resolved) || !t.is_known_fn_name(resolved) {
		if known := t.known_collection_receiver_method_alias(resolved) {
			return known
		}
		return none
	}
	return resolved
}

fn (t &Transformer) known_collection_receiver_method_alias(name string) ?string {
	if name.len == 0 || is_builtin_collection_resolved_call(name) {
		return none
	}
	lowered := c_name(name)
	if lowered != name && t.is_known_fn_name(lowered) {
		return lowered
	}
	if !transform_can_prefix_collection_receiver(t.cur_module) {
		return none
	}
	if !(name.starts_with('[]') || name.starts_with('map[')) {
		return none
	}
	qname := '${t.cur_module}.${name}'
	if t.is_known_fn_name(qname) {
		return qname
	}
	qlowered := c_name(qname)
	if qlowered != qname && t.is_known_fn_name(qlowered) {
		return qlowered
	}
	return none
}

fn is_builtin_collection_resolved_call(name string) bool {
	return name.len == 0 || is_raw_collection_method_name(name, 'array.') || name == 'array_clone'
		|| is_runtime_collection_helper_name(name) || is_raw_collection_method_name(name, 'map.')
}

fn receiver_method_name_has_generic_placeholder(name string) bool {
	if !name.contains('.') {
		return false
	}
	receiver := name.all_before_last('.')
	if receiver.contains('[') {
		for marker in ['T', 'U', 'K', 'V', 'A', 'B', 'C', 'X', 'Y', 'Z'] {
			if receiver.contains('[${marker}]') || receiver.contains('[${marker},')
				|| receiver.contains(', ${marker}]') || receiver.contains(',${marker}]') {
				return true
			}
		}
	}
	for part in receiver.split('_') {
		if part in ['T', 'U', 'K', 'V', 'A', 'B', 'C', 'X', 'Y', 'Z'] {
			return true
		}
	}
	return false
}

fn is_raw_collection_method_name(name string, prefix string) bool {
	if !name.starts_with(prefix) {
		return false
	}
	rest := name[prefix.len..]
	return rest.len > 0 && !rest.contains('.')
}

fn is_runtime_collection_helper_name(name string) bool {
	if name.len > 'array__'.len && has_array_runtime_prefix(name) {
		return is_array_runtime_helper_method_name(name, 'array__'.len, name.len - 'array__'.len)
	}
	if name.len > 'map__'.len && has_map_runtime_prefix(name) {
		return is_map_runtime_helper_method_name(name, 'map__'.len, name.len - 'map__'.len)
	}
	return false
}

fn has_array_runtime_prefix(name string) bool {
	return name[0] == `a` && name[1] == `r` && name[2] == `r` && name[3] == `a` && name[4] == `y`
		&& name[5] == `_` && name[6] == `_`
}

fn has_map_runtime_prefix(name string) bool {
	return name[0] == `m` && name[1] == `a` && name[2] == `p` && name[3] == `_` && name[4] == `_`
}

fn is_array_runtime_helper_method_name(name string, start int, len int) bool {
	return match len {
		5 { name_part_eq(name, start, 'clone') }
		6 { name_part_eq(name, start, 'insert') }
		7 { name_part_eq(name, start, 'reverse') || name_part_eq(name, start, 'prepend') }
		9 { name_part_eq(name, start, 'push_many') }
		16 { name_part_eq(name, start, 'reverse_in_place') }
		18 { name_part_eq(name, start, 'needs_unique_shift') }
		else { false }
	}
}

fn is_map_runtime_helper_method_name(name string, start int, len int) bool {
	return match len {
		3 {
			name_part_eq(name, start, 'get') || name_part_eq(name, start, 'set')
		}
		4 {
			name_part_eq(name, start, 'keys') || name_part_eq(name, start, 'move')
				|| name_part_eq(name, start, 'free')
		}
		5 {
			name_part_eq(name, start, 'clear')
		}
		6 {
			name_part_eq(name, start, 'delete') || name_part_eq(name, start, 'values')
				|| name_part_eq(name, start, 'exists')
		}
		7 {
			name_part_eq(name, start, 'reserve')
		}
		9 {
			name_part_eq(name, start, 'get_check')
		}
		else {
			false
		}
	}
}

fn name_part_eq(name string, start int, expected string) bool {
	if name.len - start != expected.len {
		return false
	}
	for i in 0 .. expected.len {
		if name[start + i] != expected[i] {
			return false
		}
	}
	return true
}

// resolve_smartcast_sum_receiver_method supports resolve_smartcast_sum_receiver_method handling.
fn (t &Transformer) resolve_smartcast_sum_receiver_method(base_id flat.NodeId, method string) ?string {
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return none }
	variant := t.resolve_variant(sc.sum_type_name, sc.variant_name)
	mut receiver_types := []string{}
	receiver_types << variant
	original_type := t.trim_pointer_type(t.original_expr_type(base_id))
	if original_type.len > 0 && original_type !in receiver_types {
		receiver_types << original_type
	}
	if sc.sum_type_name.len > 0 && sc.sum_type_name !in receiver_types {
		receiver_types << sc.sum_type_name
	}
	sum_type := t.resolve_sum_name(sc.sum_type_name)
	if sum_type.len > 0 && sum_type !in receiver_types {
		receiver_types << sum_type
	}
	for parent in t.sum_type_parents_for_variant(sc.variant_name) {
		if parent !in receiver_types {
			receiver_types << parent
		}
	}
	for receiver_type in receiver_types {
		if method_name := t.resolve_receiver_method_for_type(receiver_type, method) {
			return method_name
		}
		if method_name := t.resolve_embedded_receiver_method(receiver_type, method) {
			return method_name
		}
	}
	return none
}

fn (t &Transformer) resolve_smartcast_target_receiver_method(base_id flat.NodeId, method string) ?string {
	if method.len == 0 {
		return none
	}
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return none }
	target := t.trim_pointer_type(t.smartcast_target_type(sc))
	if target.len == 0 {
		return none
	}
	mut receiver_types := []string{}
	receiver_types << target
	variant := t.resolve_variant(sc.sum_type_name, sc.variant_name)
	if variant.len > 0 && variant !in receiver_types {
		receiver_types << variant
	}
	for receiver_type in receiver_types {
		if method_name := t.resolve_receiver_method_for_type(receiver_type, method) {
			return method_name
		}
		if alias_method := t.resolve_alias_receiver_method(receiver_type, method) {
			return alias_method
		}
		if embedded_method := t.resolve_embedded_receiver_method(receiver_type, method) {
			return embedded_method
		}
	}
	return none
}

// sum_type_parents_for_variant supports sum type parents for variant handling for Transformer.
fn (t &Transformer) sum_type_parents_for_variant(variant string) []string {
	if parents := t.sum_variant_parents[variant] {
		return parents.clone()
	}
	short := t.variant_short_name(variant)
	if short != variant {
		if parents := t.sum_variant_parents[short] {
			return parents.clone()
		}
	}
	mut result := []string{}
	for sum_name, variants in t.sum_types {
		for v in variants {
			short_v := t.variant_short_name(v)
			if v == variant || short_v == short {
				result << sum_name
				break
			}
		}
	}
	if !isnil(t.tc) {
		for sum_name, variants in t.tc.sum_types {
			if sum_name in result {
				continue
			}
			for v in variants {
				short_v := t.variant_short_name(v)
				if v == variant || short_v == short {
					result << sum_name
					break
				}
			}
		}
	}
	return result
}

// receiver_method_candidates supports receiver method candidates handling for Transformer.
fn (t &Transformer) receiver_method_candidates(receiver_type string, method string) []string {
	mut clean_type := receiver_type
	if clean_type.starts_with('&') {
		clean_type = clean_type[1..]
	}
	if clean_type.starts_with('map[') {
		return t.map_receiver_method_candidates(clean_type, method)
	}
	mut candidates := []string{}
	candidates << '${clean_type}.${method}'
	for receiver in generic_receiver_flat_type_variants(clean_type) {
		candidates << '${receiver}.${method}'
	}
	for receiver in flattened_generic_receiver_short_variants(clean_type) {
		candidates << '${receiver}.${method}'
	}
	if clean_type.starts_with('[]') {
		elem_type := clean_type[2..]
		short_elem := if elem_type.contains('.') {
			elem_type.all_after_last('.')
		} else {
			elem_type
		}
		candidates << '[]${short_elem}.${method}'
		if elem_type.contains('.') {
			candidates << '${elem_type.all_before_last('.')}.[]${short_elem}.${method}'
		} else if transform_can_prefix_collection_receiver(t.cur_module) {
			candidates << '${t.cur_module}.[]${short_elem}.${method}'
		}
	} else if clean_type.contains('.') {
		short_type := clean_type.all_after_last('.')
		candidates << '${short_type}.${method}'
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${clean_type}.${method}'
	}
	return candidates
}

fn generic_receiver_flat_type_variants(receiver_type string) []string {
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 {
		return []string{}
	}
	suffix := generic_type_suffixes(args)
	if suffix.len == 0 {
		return []string{}
	}
	mut receivers := []string{}
	transform_push_receiver_candidate(mut receivers, '${base}_${suffix}')
	if base.contains('.') {
		short_base := base.all_after_last('.')
		transform_push_receiver_candidate(mut receivers, '${short_base}_${suffix}')
		transform_push_receiver_candidate(mut receivers, '${base.all_before_last('.')}.${short_base}_${suffix}')
	}
	return receivers
}

fn flattened_generic_receiver_short_variants(receiver_type string) []string {
	clean := receiver_type.trim_space()
	if clean.len == 0 || !clean.contains('__') || !clean.contains('_') {
		return []string{}
	}
	module_name := if clean.contains('.') { clean.all_before_last('.') } else { '' }
	leaf := if clean.contains('.') { clean.all_after_last('.') } else { clean }
	parts := flattened_generic_receiver_leaf_parts(leaf)
	mut changed := false
	mut short_parts := []string{cap: parts.len}
	for part in parts {
		if part.contains('__') {
			short_parts << part.all_after_last('__')
			changed = true
		} else {
			short_parts << part
		}
	}
	if !changed {
		return []string{}
	}
	short_leaf := short_parts.join('_')
	mut variants := [short_leaf]
	if module_name.len > 0 {
		variants << '${module_name}.${short_leaf}'
	}
	return variants
}

fn flattened_generic_receiver_leaf_parts(leaf string) []string {
	mut parts := []string{}
	mut start := 0
	mut i := 0
	for i < leaf.len {
		if leaf[i] == `_` {
			if i + 1 < leaf.len && leaf[i + 1] == `_` {
				i += 2
				continue
			}
			parts << leaf[start..i]
			i++
			start = i
			continue
		}
		i++
	}
	parts << leaf[start..]
	return parts
}

fn (t &Transformer) resolve_fixed_array_dynamic_receiver_method(fixed_type string, method string) ?string {
	elem_type := fixed_array_outer_elem_type(fixed_type)
	if elem_type.len == 0 {
		return none
	}
	return t.resolve_receiver_method_for_type('[]${elem_type}', method)
}

fn (mut t Transformer) lower_fixed_array_dynamic_receiver_method_call(node flat.Node, base_id flat.NodeId, fixed_type string, method_name string) flat.NodeId {
	elem_type := fixed_array_outer_elem_type(fixed_type)
	array_type := '[]${elem_type}'
	tmp_name := t.new_temp('fixed_arr')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.fixed_array_value_to_array(base_id, fixed_type, array_type), array_type)
	args := t.transform_receiver_method_args_with_base(node, t.make_ident(tmp_name), method_name)
	ret_type := t.receiver_method_return_type(method_name, node.typ)
	t.mark_fn_used(method_name)
	return t.make_call_typed(method_name, args, ret_type)
}

fn fixed_array_outer_elem_type(type_text string) string {
	clean := type_text.trim_space()
	if clean.len == 0 {
		return ''
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[bracket_end + 1..]
		}
		return ''
	}
	elem, dims := transform_postfix_fixed_array_parts(clean)
	if elem.len == 0 || dims.len == 0 {
		return fixed_array_elem_type(clean)
	}
	mut out := elem
	for i := dims.len - 1; i > 0; i-- {
		out += '[${dims[i]}]'
	}
	return out
}

fn transform_push_receiver_candidate(mut candidates []string, candidate string) {
	if candidate.len > 0 && candidate !in candidates {
		candidates << candidate
	}
}

fn transform_can_prefix_collection_receiver(module_name string) bool {
	return module_name.len > 0 && module_name != 'main' && module_name != 'builtin'
}

fn (t &Transformer) receiver_type_text_variants(type_text string) []string {
	clean := type_text.trim_space()
	mut names := []string{}
	transform_push_receiver_candidate(mut names, clean)
	transform_push_receiver_candidate(mut names, receiver_type_text_short_spelling(clean))
	if t.is_fixed_array_type(clean) {
		source := t.receiver_type_text_source_fixed_spelling(clean)
		transform_push_receiver_candidate(mut names, source)
		transform_push_receiver_candidate(mut names, receiver_type_text_short_spelling(source))
	}
	return names
}

fn (t &Transformer) receiver_type_text_source_fixed_spelling(type_text string) string {
	clean := type_text.trim_space()
	if clean.len == 0 || clean.starts_with('[') || !t.is_fixed_array_type(clean) {
		return clean
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean)
		if parsed is types.ArrayFixed {
			return t.semantic_type_name(parsed)
		}
	}
	elem, dims := transform_postfix_fixed_array_parts(clean)
	if elem.len == 0 || dims.len == 0 {
		return clean
	}
	mut source := elem
	for i := dims.len; i > 0; i-- {
		source = '[${dims[i - 1]}]${source}'
	}
	return source
}

fn transform_postfix_fixed_array_parts(type_text string) (string, []string) {
	clean := type_text.trim_space()
	mut end := clean.len
	mut dims := []string{}
	for end > 0 && clean[end - 1] == `]` {
		start := transform_trailing_matching_bracket_start(clean, end)
		if start < 0 {
			break
		}
		dims << clean[start + 1..end - 1].trim_space()
		end = start
	}
	return clean[..end], dims
}

fn transform_trailing_matching_bracket_start(s string, end int) int {
	mut depth := 0
	for i := end - 1; i >= 0; i-- {
		if s[i] == `]` {
			depth++
		} else if s[i] == `[` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn receiver_type_text_short_spelling(type_text string) string {
	clean := type_text.trim_space()
	if clean.starts_with('[]') {
		return '[]' + receiver_type_text_short_spelling(clean[2..])
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + receiver_type_text_short_spelling(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := receiver_type_text_short_spelling(clean[4..bracket_end])
			value := receiver_type_text_short_spelling(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if clean.contains('.') {
		return clean.all_after_last('.')
	}
	return clean
}

fn (t &Transformer) receiver_type_text_module_names(type_text string) []string {
	clean := type_text.trim_space()
	mut names := []string{}
	if clean.starts_with('[]') {
		return t.receiver_type_text_module_names(clean[2..])
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.receiver_type_text_module_names(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('map[') {
		key_type := t.map_key_type(clean)
		value_type := t.map_value_type(clean)
		for name in t.receiver_type_text_module_names(key_type) {
			transform_push_receiver_candidate(mut names, name)
		}
		for name in t.receiver_type_text_module_names(value_type) {
			transform_push_receiver_candidate(mut names, name)
		}
		return names
	}
	if t.is_fixed_array_type(clean) {
		return t.receiver_type_text_module_names(fixed_array_elem_type(clean))
	}
	if clean.contains('.') {
		transform_push_receiver_candidate(mut names, clean.all_before_last('.'))
	}
	return names
}

// map_receiver_method_candidates supports map receiver method candidates handling for Transformer.
fn (t &Transformer) map_receiver_method_candidates(receiver_type string, method string) []string {
	clean_type := t.clean_map_type(receiver_type)
	key_type := t.map_key_type(clean_type)
	value_type := t.map_value_type(clean_type)
	mut candidates := []string{}
	if key_type.len == 0 || value_type.len == 0 {
		transform_push_receiver_candidate(mut candidates, '${clean_type}.${method}')
		return candidates
	}
	key_types := t.receiver_type_text_variants(key_type)
	value_types := t.receiver_type_text_variants(value_type)
	mut map_types := []string{}
	for key in key_types {
		for value in value_types {
			transform_push_receiver_candidate(mut map_types, 'map[${key}]${value}')
		}
	}
	for map_type in map_types {
		transform_push_receiver_candidate(mut candidates, '${map_type}.${method}')
	}
	mut module_names := []string{}
	if transform_can_prefix_collection_receiver(t.cur_module) {
		transform_push_receiver_candidate(mut module_names, t.cur_module)
	}
	for mod_name in t.receiver_type_text_module_names(key_type) {
		transform_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in t.receiver_type_text_module_names(value_type) {
		transform_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			transform_push_receiver_candidate(mut candidates, '${mod_name}.${map_type}.${method}')
		}
	}
	return candidates
}

// transform_receiver_method_args transforms transform receiver method args data for transform.
fn (mut t Transformer) transform_receiver_method_args(node flat.Node, base_id flat.NodeId, method_name string) []flat.NodeId {
	return t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id, method_name), method_name)
}

// expr_root_ident_name walks selector/index/paren chains down to the base identifier and
// returns its name, or an empty string if the root is not a plain identifier.
@[direct_array_access]
fn (t &Transformer) expr_root_ident_name(id flat.NodeId) string {
	mut cur := id
	for int(cur) >= 0 && int(cur) < t.a.nodes.len {
		node := t.a.nodes[int(cur)]
		match node.kind {
			.ident {
				return node.value
			}
			.selector, .index, .paren, .cast_expr, .expr_stmt {
				if node.children_count == 0 {
					return ''
				}
				cur = t.a.child(&node, 0)
			}
			else {
				return ''
			}
		}
	}
	return ''
}

// clone_receiver_aliased_arg clones a by-value argument that reads storage from the retained
// method receiver (its root identifier matches the receiver's). V passes such an argument as a
// shallow copy sharing the receiver's backing, so a callee that moves out of its parameter —
// as `toml`'s recursive `value_` lookup does with its map/array element — would otherwise
// mutate the receiver the caller still holds (e.g. clearing a `Doc` map slot on lookup).
// Cloning gives the callee an independent value. Arguments that do not alias the receiver keep
// their move semantics, so this leaves the ordinary "move out of an owned parameter" path
// untouched.
fn (mut t Transformer) clone_receiver_aliased_arg(recv_root string, arg_id flat.NodeId, value flat.NodeId, param_type string) flat.NodeId {
	if recv_root.len == 0 || t.expr_root_ident_name(arg_id) != recv_root {
		return value
	}
	return t.clone_checker_marked_receiver_alias_arg(arg_id, value, param_type)
}

// clone_checker_marked_receiver_alias_arg materializes the clone decision shared by direct
// arguments and elements packed into a variadic array.
fn (mut t Transformer) clone_checker_marked_receiver_alias_arg(arg_id flat.NodeId, value flat.NodeId, param_type string) flat.NodeId {
	if param_type.len == 0 || param_type.starts_with('&') {
		return value
	}
	// transform_call_arg_for_param already materializes checker-marked borrowed clones
	// and extracts index moves into an owned temporary. Do not clone either result again.
	if !isnil(t.tc) && (t.tc.ownership_expr_is_borrowed_projection(arg_id)
		|| t.tc.ownership_index_read_moves_value(arg_id)) {
		return value
	}
	if isnil(t.tc) || !t.tc.ownership_receiver_alias_arg_is_cloned(arg_id) {
		return value
	}
	if !t.compiler_default_clone_type_needs_work(param_type) {
		return value
	}
	return t.make_compiler_default_borrowed_clone_value(value, param_type, true)
}

// transform_receiver_method_args_with_base transforms helper data for transform.
fn (mut t Transformer) transform_receiver_method_args_with_base(node flat.Node, base flat.NodeId, method_name string) []flat.NodeId {
	mut args := []flat.NodeId{cap: int(node.children_count)}
	args << base
	recv_root := t.expr_root_ident_name(base)
	params := t.call_param_types(method_name)
	param_offset := t.receiver_method_param_offset(base, node, params, method_name)
	explicit_args := int(node.children_count) - 1
	expected_explicit := params.len - param_offset
	variadic_arg_pos := 1 + params.len - 1 - param_offset
	has_spread_at_variadic_slot := variadic_arg_pos > 0 && variadic_arg_pos < node.children_count
		&& t.call_arg_is_spread(t.a.child(&node, variadic_arg_pos))
	is_variadic := t.call_is_variadic(method_name) || (params.len > 0
		&& params[params.len - 1] is types.Array && (explicit_args > expected_explicit
		|| has_spread_at_variadic_slot))
	variadic_idx := if is_variadic && params.len > 0 && params[params.len - 1] is types.Array {
		params.len - 1
	} else {
		-1
	}
	mut i := 1
	mut variadic_tail_supplied := false
	for i < node.children_count {
		param_idx := (args.len - 1) + param_offset
		arg_id := t.a.child(&node, i)
		arg_node := t.a.nodes[int(arg_id)]
		param_type := if param_idx < params.len {
			t.semantic_type_name(params[param_idx])
		} else {
			''
		}
		if spread_args := t.transform_spread_arg_over_fixed_variadic_tail(arg_node, param_idx, variadic_idx, params) {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				args << t.fixed_variadic_spread_args_with_trailing(spread_args, node, i + 1, variadic_type)
			} else {
				args << spread_args
			}
			variadic_tail_supplied = true
			i++
			break
		}
		if arg_node.kind == .field_init {
			struct_param_type := if variadic_idx >= 0 && param_idx == variadic_idx
				&& param_type.starts_with('[]') {
				param_type[2..]
			} else {
				param_type
			}
			if packed_arg := t.transform_params_struct_call_arg(node, i, struct_param_type) {
				args << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
			if packed_arg := t.transform_struct_call_arg(node, i, struct_param_type) {
				args << packed_arg
				i = t.next_non_field_init_arg(node, i)
				continue
			}
		}
		if variadic_idx >= 0 && param_idx == variadic_idx {
			variadic_type := params[variadic_idx]
			if variadic_type is types.Array {
				if arg_node.kind == .prefix && arg_node.value == '...'
					&& arg_node.children_count > 0 {
					spread_id := t.a.child(&arg_node, 0)
					args << t.transform_variadic_spread_arg_for_param(spread_id, variadic_type, param_type)
					i++
					break
				}
				remaining := int(node.children_count) - i
				if remaining == 1 {
					arg_type := t.node_type(arg_id)
					if arg_type.starts_with('[]') {
						args << t.transform_call_arg_for_param(arg_id, param_type)
					} else {
						args << t.pack_variadic_args(node, i, variadic_type.elem_type)
					}
				} else {
					args << t.pack_variadic_args(node, i, variadic_type.elem_type)
				}
				break
			}
		}
		if variadic_idx < 0 && arg_node.kind == .prefix && arg_node.value == '...'
			&& arg_node.children_count > 0 && params.len > 0 && param_idx < params.len {
			spread_id := t.a.child(&arg_node, 0)
			spread_base := t.stable_expr_for_reuse(spread_id)
			spread_count := params.len - param_idx
			for spread_offset in 0 .. spread_count {
				expected := t.semantic_type_name(params[param_idx + spread_offset])
				index_arg := t.make_spread_index_for_expected_param(spread_base, spread_offset, expected)
				args << t.transform_call_arg_for_param(index_arg, expected)
			}
			i++
			continue
		}
		if variadic_idx < 0 && param_idx < params.len {
			arg_type := t.tc.resolve_type(arg_id)
			if arg_type is types.MultiReturn && arg_type.types.len == params.len - param_idx {
				items := arg_type.types
				multi_type := t.multi_return_type_name(items)
				value := t.stable_transformed_expr_for_reuse(t.transform_expr(arg_id), multi_type, 'multi_arg')
				for multi_idx, item_type in items {
					expected_idx := param_idx + multi_idx
					if expected_idx >= params.len {
						break
					}
					field := t.make_selector(value, 'arg${multi_idx}', t.semantic_type_name(item_type))
					args << t.transform_call_arg_for_param(field, t.semantic_type_name(params[expected_idx]))
				}
				i++
				continue
			}
		}
		args << t.clone_receiver_aliased_arg(recv_root, arg_id, t.transform_call_arg_for_param(arg_id, param_type), param_type)
		i++
	}
	if variadic_idx >= 0 && !variadic_tail_supplied && explicit_args == variadic_idx - param_offset {
		variadic_type := params[variadic_idx]
		if variadic_type is types.Array {
			args << t.pack_variadic_args(node, int(node.children_count), variadic_type.elem_type)
		}
	}
	t.append_missing_params_struct_args(mut args, params, param_offset)
	return args
}

fn (mut t Transformer) make_spread_index_for_expected_param(base flat.NodeId, offset int, typ string) flat.NodeId {
	base_type := t.normalize_type_alias(t.node_type(base))
	elem_type := t.array_elem_type(base_type)
	id := t.make_index(base, t.make_int_literal(offset), if elem_type.len > 0 {
		elem_type
	} else {
		typ
	})
	t.set_node_generic_params(int(id), [spread_index_expected_type_marker])
	if elem_type == 'string'
		&& typ in ['bool', 'i8', 'i16', 'i32', 'int', 'i64', 'f32', 'f64', 'u8', 'u16', 'u32', 'u64'] {
		fn_name := 'string__${typ}'
		t.mark_fn_used_name('string.${typ}')
		t.mark_fn_used_name(fn_name)
		return t.make_call_typed(fn_name, [id], typ)
	}
	t.set_node_typ(int(id), typ)
	return id
}

// receiver_method_param_offset supports receiver method param offset handling for Transformer.
fn (t &Transformer) receiver_method_param_offset(base_id flat.NodeId, node flat.Node, params []types.Type, method_name string) int {
	if params.len == 0 {
		return 0
	}
	base_type := t.node_type(base_id)
	first_type := t.normalize_type_alias(t.semantic_type_name(params[0]))
	clean_first := if first_type.starts_with('&') { first_type[1..] } else { first_type }
	clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if receiver_param_types_match(clean_first, clean_base)
		|| receiver_param_matches_method_name(clean_first, method_name)
		|| t.normalize_type_alias(clean_first) == t.normalize_type_alias(clean_base)
		|| short_name_view(clean_first) == short_name_view(clean_base) {
		return 1
	}
	if params.len >= int(node.children_count) {
		return 1
	}
	return 0
}

fn receiver_param_matches_method_name(first string, method_name string) bool {
	if first.len == 0 || method_name.len == 0 || !method_name.contains('.') {
		return false
	}
	mut receiver := method_name.all_before_last('.')
	if receiver.len == 0 {
		return false
	}
	if receiver.starts_with('&') {
		receiver = receiver[1..]
	}
	mut first_base := first
	first_generic_base, _, first_is_generic := generic_app_parts(first)
	if first_is_generic {
		first_base = first_generic_base
	}
	mut receiver_base := receiver
	receiver_generic_base, _, receiver_is_generic := generic_app_parts(receiver)
	if receiver_is_generic {
		receiver_base = receiver_generic_base
	}
	first_short := short_name_view(first_base)
	receiver_short := short_name_view(receiver_base)
	return first_base == receiver_base || first_short == receiver_short
		|| receiver_short.starts_with('${first_short}_')
		|| c_name(receiver_base).starts_with('${c_name(first_base)}_')
}

fn receiver_param_types_match(first string, base string) bool {
	if first == base {
		return true
	}
	first_base, first_args, first_ok := generic_app_parts(first)
	base_base, base_args, base_ok := generic_app_parts(base)
	if !first_ok || !base_ok || first_args.len != base_args.len {
		return false
	}
	if first_base != base_base && first_base.all_after_last('.') != base_base.all_after_last('.') {
		return false
	}
	for i, first_arg in first_args {
		base_arg := base_args[i]
		if first_arg == base_arg {
			continue
		}
		if generic_type_arg_short(first_arg) == generic_type_arg_short(base_arg) {
			continue
		}
		if c_name(first_arg) == c_name(base_arg) {
			continue
		}
		return false
	}
	return true
}

// try_lower_string_method_call supports try lower string method call handling for Transformer.
fn (mut t Transformer) try_lower_string_method_call(node flat.Node) ?flat.NodeId {
	if node.children_count == 0 {
		return none
	}
	fn_id := t.a.children[node.children_start]
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	method := fn_node.value
	if method == 'count' && node.children_count == 2 && t.expr_uses_ident(t.a.child(&node, 1), 'it') {
		return t.lower_string_count_call(node, fn_node)
	}
	if method !in ['replace', 'replace_once', 'trim', 'trim_left', 'trim_right', 'all_before',
		'all_after', 'all_before_last', 'all_after_last', 'contains', 'starts_with', 'ends_with',
		'bytes', 'substr', 'substr_unsafe', 'repeat', 'plus_two', 'count'] {
		return none
	}
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	if base_type != 'string' {
		return none
	}
	mut args := []flat.NodeId{cap: int(node.children_count)}
	args << t.transform_expr(base_id)
	for i in 1 .. node.children_count {
		args << t.transform_expr(t.a.child(&node, i))
	}
	ret_type := match method {
		'contains', 'starts_with', 'ends_with' { 'bool' }
		'bytes' { '[]u8' }
		'count' { 'int' }
		else { 'string' }
	}

	t.mark_fn_used_name('string.${method}')
	return t.make_call_typed('string__${method}', args, ret_type)
}

// lower_string_count_call builds lower string count call data for transform.
fn (mut t Transformer) lower_string_count_call(node flat.Node, fn_node flat.Node) ?flat.NodeId {
	base_id := t.a.child(&fn_node, 0)
	base_type := t.node_type(base_id)
	if base_type != 'string' {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('count')
	idx_name := t.new_temp('count_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(0), 'int')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), 'int')
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', 'int'))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := t.make_index(base, t.make_ident(idx_name), 'u8')
	elem_decl := t.make_decl_assign_typed('it', elem_expr, 'u8')
	predicate_id := t.a.child(&node, 1)
	old_it := t.var_type('it')
	t.set_var_type('it', 'u8')
	predicate := t.transform_expr(predicate_id)
	if old_it.len > 0 {
		t.set_var_type('it', old_it)
	} else {
		t.unset_var_type('it')
	}
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	t.drain_pending(mut loop_body)
	inc := t.make_assign_op(t.make_ident(result_name), t.make_int_literal(1), .plus_assign)
	loop_body << t.make_if(predicate, t.make_block([inc]), t.make_empty())
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// expr_uses_ident supports expr uses ident handling for Transformer.
fn (t &Transformer) expr_uses_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident && node.value == name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.expr_uses_ident(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

// is_method_call checks if a .call node is a method call (child[0] is .selector).
// Returns true for `obj.method(args)` patterns.
fn (mut t Transformer) is_method_call(node flat.Node) bool {
	if node.children_count == 0 {
		return false
	}
	fn_id := t.a.children[node.children_start]
	if int(fn_id) < 0 {
		return false
	}
	fn_node := t.a.nodes[int(fn_id)]
	return fn_node.kind == .selector
}

// get_call_return_type looks up the return type for a resolved call.
// Handles both checker-resolved calls and transform-time name resolution.
fn (t &Transformer) get_call_return_type(id flat.NodeId, node flat.Node) string {
	if ret := t.fn_value_call_return_type(node) {
		return t.call_return_type_name(ret, node)
	}
	if ret := t.current_generic_receiver_call_return_type(node) {
		return ret
	}
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			receiver_type := t.node_type(t.a.child(fn_node, 0)).trim_string_left('&')
			if args := t.current_specialized_receiver_args(receiver_type) {
				receiver_base, _, is_generic_receiver := generic_app_parts(receiver_type)
				if is_generic_receiver {
					mut names := ['${receiver_base}.${fn_node.value}']
					if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] && !receiver_base.contains('.') {
						names << '${t.cur_module}.${receiver_base}.${fn_node.value}'
					}
					for name in names {
						if ret := t.fn_ret_types[name] {
							return substitute_generic_type_text(ret, args)
						}
						if !isnil(t.tc) {
							if ret := t.tc.fn_ret_types[name] {
								return substitute_generic_type_text(t.semantic_type_name(ret), args)
							}
						}
					}
				}
			}
		}
		if fn_node.kind == .ident && t.var_type(fn_node.value).len == 0 {
			qualified_name := if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] && !fn_node.value.contains('.') {
				'${t.cur_module}.${fn_node.value}'
			} else {
				fn_node.value
			}
			for name in [fn_node.value, qualified_name] {
				if !isnil(t.tc) && t.tc.specialized_generic_fns[name] {
					if ret := t.fn_ret_types[name] {
						return t.call_return_type_name(ret, node)
					}
					if ret := t.tc.fn_ret_types[name] {
						return t.call_return_type_name(t.semantic_type_name(ret), node)
					}
				}
			}
		}
	}
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident && t.var_type(fn_node.value).len == 0 {
			if ret := t.local_fn_decl_return_type(fn_node.value) {
				return t.call_return_type_name(ret, node)
			}
		}
	}
	if ret := t.checker_resolved_non_builtin_return_type(id, node) {
		return ret
	}
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident {
			local_type := t.var_type(fn_node.value)
			if local_type.len > 0 {
				if ret := t.local_fn_value_return_type_from_type(local_type) {
					return t.call_return_type_name(ret, node)
				}
			} else {
				if ret := t.local_fn_decl_return_type(fn_node.value) {
					return t.call_return_type_name(ret, node)
				}
			}
		}
		if fn_node.kind == .selector
			&& fn_node.value in ['clone', 'reverse', 'repeat', 'repeat_to_depth']
			&& fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			base_type := t.node_type(base_id)
			clean_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
			if fn_node.value == 'clone' && clean_type.len > 0 {
				return clean_type
			}
			if clean_type.starts_with('[]') {
				return clean_type
			}
		}
		if fn_node.kind == .selector && fn_node.value == 'wait' && fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			base_type := t.node_type(base_id)
			clean_type := t.membership_container_type(base_type)
			if clean_type.starts_with('[]') {
				elem_type := clean_type[2..]
				if wait_type := thread_array_wait_return_type(elem_type) {
					return wait_type
				}
			}
			if t.is_fixed_array_type(clean_type) {
				elem_type := fixed_array_elem_type(clean_type)
				if wait_type := thread_array_wait_return_type(elem_type) {
					return wait_type
				}
			}
			if clean_type == 'thread' {
				return 'void'
			}
			if clean_type.starts_with('thread ') {
				return clean_type[7..]
			}
		}
		if fn_node.kind == .selector && fn_node.value in ['keys', 'values']
			&& fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			mut base_type := t.node_type(base_id)
			if base_type.len == 0 {
				base_type = t.checker_node_type(base_id)
			}
			clean_type := t.clean_map_type(base_type)
			if clean_type.starts_with('map[') {
				elem_type := if fn_node.value == 'keys' {
					t.map_key_type(clean_type)
				} else {
					t.map_value_type(clean_type)
				}
				if elem_type.len > 0 {
					return '[]${elem_type}'
				}
			}
		}
		// A method on a concrete generic instance (`Box[int].clone`) is registered under
		// the open form (`Box[T].clone`), whose stored return type collapsed `Box[T]` to
		// the bare base. Resolve it through the checker, which re-substitutes the concrete
		// arguments from the signature text, so the inferred decl type is `Box[int]`.
		if !isnil(t.tc) && fn_node.kind == .selector && fn_node.children_count > 0 {
			base_type := t.node_type(t.a.child(fn_node, 0))
			clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
			if clean_base.contains('[') && clean_base.ends_with(']') {
				if ci := t.tc.resolve_generic_struct_method(clean_base, fn_node.value) {
					rn := t.semantic_type_name(ci.return_type)
					if rn.len > 0 && rn != 'void' && rn != 'unknown' {
						return t.normalize_type_alias(rn)
					}
				}
			}
		}
	}
	if !isnil(t.tc) {
		if name := t.tc.resolved_call_name(id) {
			if ret := t.tc.fn_ret_types[name] {
				return t.call_return_type_name(t.semantic_type_name(ret), node)
			}
		}
	}
	name := t.resolve_call_name(node)
	if name.len == 0 {
		return ''
	}
	if !isnil(t.tc) {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
			&& !name.contains('.') {
			qname := '${t.cur_module}.${name}'
			if ret := t.tc.fn_ret_types[qname] {
				return t.call_return_type_name(t.semantic_type_name(ret), node)
			}
		}
		if ret := t.tc.fn_ret_types[name] {
			return t.call_return_type_name(t.semantic_type_name(ret), node)
		}
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin'
		&& !name.contains('.') {
		qname := '${t.cur_module}.${name}'
		if ret := t.fn_ret_types[qname] {
			return t.call_return_type_name(ret, node)
		}
	}
	if ret := t.fn_ret_types[name] {
		return t.call_return_type_name(ret, node)
	}
	return ''
}

fn (t &Transformer) current_generic_receiver_call_return_type(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	current_receiver := t.current_fn_receiver_type()
	current_generic_base, generic_args, current_ok := generic_app_parts(current_receiver)
	current_args := if current_ok {
		generic_args
	} else {
		t.recorded_generic_specialization_args(current_receiver) or { return none }
	}
	if current_args.len == 0 || t.generic_args_have_placeholders(current_args) {
		return none
	}
	current_base := if current_ok { current_generic_base } else { current_receiver }
	receiver_type := t.node_type(t.a.child(callee, 0)).trim_string_left('&')
	if receiver_type.len > 0 {
		receiver_base, _, receiver_ok := generic_app_parts(receiver_type)
		if !receiver_ok || !current_receiver_matches_open_generic_base(current_base, receiver_base) {
			return none
		}
	}
	for name, ret in t.fn_ret_types {
		if !name.ends_with('.${callee.value}') {
			continue
		}
		decl_receiver := name.all_before_last('.')
		generic_decl_base, _, decl_ok := generic_app_parts(decl_receiver)
		decl_base := if decl_ok { generic_decl_base } else { decl_receiver }
		if !current_receiver_matches_open_generic_base(current_base, decl_base) {
			continue
		}
		resolved := substitute_generic_type_text(ret, current_args)
		if resolved.len > 0 && !t.generic_args_have_placeholders([resolved]) {
			return resolved
		}
	}
	return none
}

fn (t &Transformer) checker_resolved_non_builtin_return_type(id flat.NodeId, node flat.Node) ?string {
	if isnil(t.resolved_call_return_cache) {
		return t.checker_resolved_non_builtin_return_type_uncached(id, node)
	}
	mut cache := t.resolved_call_return_cache
	slot := int(id) & 1023
	if cache.generations[slot] == cache.generation && cache.keys[slot] == int(id)
		&& cache.value_ptrs[slot] == voidptr(node.value.str)
		&& cache.value_lens[slot] == node.value.len {
		return cache.results[slot]
	}
	result := t.checker_resolved_non_builtin_return_type_uncached(id, node) or { return none }
	cache.keys[slot] = int(id)
	cache.value_ptrs[slot] = voidptr(node.value.str)
	cache.value_lens[slot] = node.value.len
	cache.generations[slot] = cache.generation
	cache.results[slot] = result
	return result
}

fn (t &Transformer) checker_resolved_non_builtin_return_type_uncached(id flat.NodeId, node flat.Node) ?string {
	if isnil(t.tc) {
		return none
	}
	name := t.tc.resolved_call_name(id) or { return none }
	if is_builtin_collection_resolved_call(name) {
		return none
	}
	if node.children_count > 0 && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		fn_node := t.a.child_node(&node, 0)
		short_name := short_name_view(name)
		if fn_node.kind == .ident && fn_node.value == short_name {
			qname := '${t.cur_module}.${short_name}'
			if ret := t.tc.fn_ret_types[qname] {
				candidate := t.call_return_type_name(t.semantic_type_name(ret), node)
				if decl_type_is_usable(candidate) || candidate == 'void' {
					return candidate
				}
			}
			lowered_qname := c_name(qname)
			if ret := t.tc.fn_ret_types[lowered_qname] {
				candidate := t.call_return_type_name(t.semantic_type_name(ret), node)
				if decl_type_is_usable(candidate) || candidate == 'void' {
					return candidate
				}
			}
		}
	}
	// The expression cache can contain a parser-inferred multi-return tail such as
	// `!(m.Match, _)`. Prefer the resolved declaration, whose complete return type
	// is authoritative, before falling back to that provisional expression type.
	decl_module := t.tc.fn_type_modules[name] or { '' }
	if ret := t.tc.fn_ret_types[name] {
		if ret !is types.Unknown && ret !is types.Void {
			candidate := t.call_return_type_name_in_module(t.semantic_type_name(ret), node, decl_module)
			if decl_type_is_usable(candidate) {
				return candidate
			}
		}
	}
	if ret_text := t.tc.fn_ret_type_texts[name] {
		if ret_text.len > 0 {
			candidate := t.call_return_type_name_in_module(ret_text, node, decl_module)
			if decl_type_is_usable(candidate) || candidate == 'void' {
				return candidate
			}
		}
	}
	if typ := t.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			candidate := t.call_return_type_name(t.semantic_type_name(typ), node)
			if decl_type_is_usable(candidate) {
				return candidate
			}
		}
	}
	return none
}

fn (t &Transformer) call_return_type_name_in_module(ret_name string, node flat.Node, module_name string) string {
	mut typ := ret_name
	if node.value.len > 0 {
		generic_arg := t.normalize_type_in_module(node.value, t.cur_module)
		if generic_arg.len > 0 {
			typ = t.specialize_generic_type_name(typ, generic_arg)
		}
	}
	return t.normalize_type_in_module(typ, module_name)
}

// call_return_type_name updates call return type name state for Transformer.
fn (t &Transformer) call_return_type_name(ret_name string, node flat.Node) string {
	mut typ := ret_name
	if node.value.len > 0 {
		generic_arg := t.normalize_type_in_module(node.value, t.cur_module)
		if generic_arg.len > 0 {
			typ = t.specialize_generic_type_name(typ, generic_arg)
		}
	}
	if t.is_optional_type_name(typ) {
		return typ
	}
	return t.normalize_type_alias(typ)
}

// specialize_generic_type_name supports specialize generic type name handling for Transformer.
fn (t &Transformer) specialize_generic_type_name(typ string, generic_arg string) string {
	clean := typ.trim_space()
	if clean.len == 0 || generic_arg.len == 0 {
		return typ
	}
	if clean == 'T' {
		return generic_arg
	}
	if clean.starts_with('&') {
		return '&' + t.specialize_generic_type_name(clean[1..], generic_arg)
	}
	if clean.starts_with('[]') {
		return '[]' + t.specialize_generic_type_name(clean[2..], generic_arg)
	}
	if clean.starts_with('?') {
		return '?' + t.specialize_generic_type_name(clean[1..], generic_arg)
	}
	if clean.starts_with('!') {
		return '!' + t.specialize_generic_type_name(clean[1..], generic_arg)
	}
	if clean.starts_with('...') {
		return '...' + t.specialize_generic_type_name(clean[3..], generic_arg)
	}
	if clean.starts_with('map[') {
		bracket_end := clean.index(']') or { return typ }
		key_type := t.specialize_generic_type_name(clean[4..bracket_end], generic_arg)
		value_type := t.specialize_generic_type_name(clean[bracket_end + 1..], generic_arg)
		return 'map[${key_type}]${value_type}'
	}
	if clean.starts_with('[') {
		bracket_end := clean.index(']') or { return typ }
		return clean[..bracket_end + 1] + t.specialize_generic_type_name(clean[bracket_end + 1..], generic_arg)
	}
	return typ
}
