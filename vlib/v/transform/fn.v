module transform

import v.gen.c.naming { sum_field_name }
import v.flat
import v.token
import v.types

const spread_index_expected_type_marker = '__v3_spread_index_expected_type'

const multi_return_fixed_array_arg_marker = '__v3_multi_return_fixed_array_arg'

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

struct MutOptionalValueCallArg {
	value     flat.NodeId
	writeback flat.NodeId
}

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
			if (t.var_type(name)) != 0 {
				return name
			}
			// Try qualified with current module
			if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
				qname := '${t.cur_module}.${name}'
				if t.is_known_fn_name(qname) {
					return qname
				}
			}
			// Top-level script expressions may not have a recorded checker call.
			if !isnil(t.tc) && t.a.has_vsh_source && t.cur_module == 'main'
				&& t.cur_file.ends_with('.vsh') {
				if declared_file := t.tc.fn_type_files[name] {
					if source_file := t.a.source_files[node.pos.id] {
						if declared_file == source_file.name {
							return name
						}
					}
				}
				for imported in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
					[]string{}
				} {
					if t.is_known_fn_name(imported) {
						return imported
					}
				}
				if name !in t.tc.v_fn_semantic_names {
					qualified := 'os.${name}'
					if t.is_known_fn_name(qualified) {
						return qualified
					}
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

fn (t &Transformer) local_fn_decl_return_type(name string) ?u32 {
	if name == '' {
		return none
	}
	module_key := transform_fn_module_key(t.cur_module, name)
	if ret_source_id := t.fn_ret_types_by_module.type_id(module_key) {
		return ret_source_id
	}
	qname := transform_qualified_fn_name(t.cur_module, name)
	// A `fn C.name()` declaration is also registered under its bare name, but it
	// is not a local function: only `C.name()` calls it. In a `.vsh` script a bare
	// `getenv()` is `os.getenv`, so it must not be typed as `C.getenv`.
	if qname == name && !isnil(t.tc) && name !in t.tc.v_fn_semantic_names
		&& 'C.${name}' in t.tc.fn_ret_types {
		return none
	}
	if ret_source_id := t.fn_ret_types.type_id(qname) {
		return ret_source_id
	}
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[qname] {
			return t.semantic_type_name(ret)
		}
	}
	return none
}

fn (t &Transformer) local_fn_value_return_type_from_type(typ_id u32) ?u32 {
	if typ_id == u32(0) {
		return none
	}
	normalized := t.normalize_type_alias(typ_id)
	return fn_type_return_type_text(normalized)
}

fn (t &Transformer) fn_value_call_return_type(node flat.Node) ?u32 {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee_id := t.a.child(&node, 0)
	if int(callee_id) < 0 {
		return none
	}
	callee := t.a.nodes[int(callee_id)]
	if callee.kind == .ident {
		local_type_source_id := t.var_type(callee.value)

		if ret_source_id := t.local_fn_value_return_type_from_type(local_type_source_id) {
			return ret_source_id
		}
		if local_type_source_id == 0 {
			if _ := t.local_fn_decl_return_type(callee.value) {
				return none
			}
		}
	}
	if callee.kind == .selector {
		if !isnil(t.tc) && callee.children_count > 0 {
			base := t.a.child_node(&callee, 0)
			if base.kind == .ident && base.value == 'C' {
				if _ := t.tc.visible_c_fn_module('C.${callee.value}') {
					// A declared C call uses its module's signature, not a global fn value.
					return none
				}
			}
		}
		if raw_type_source_id := t.raw_selector_field_type(callee_id) {
			if ret := fn_type_return_type_text(raw_type_source_id) {
				// A generic struct field keeps its declaration spelling (`fn () !T`) in
				// raw metadata. A cloned selector already carries the specialized type;
				// use it when the raw return still contains a placeholder.
				if t.generic_arg_is_unresolved(ret) {
					if concrete_ret := fn_type_return_type_text(callee.typ) {
						if !t.generic_arg_is_unresolved(concrete_ret) {
							return concrete_ret
						}
					}
				}
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
	callee_type_name_source_id := t.node_type(callee_id)

	if ret_source_id := t.local_fn_value_return_type_from_type(callee_type_name_source_id) {
		return ret_source_id
	}
	return none
}

fn fn_type_return_type_text(typ_id u32) ?u32 {
	clean := trimmed_transform_text(flat.source_type_name(typ_id))
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
				ret := trimmed_transform_text(clean[i + 1..])
				if ret.len == 0 {
					return flat.source_type_void
				}
				if ret in ['!', '?'] {
					return flat.source_type_id('${ret}void')
				}
				return flat.source_type_id(ret)
			}
		}
	}
	return none
}

// is_known_fn_name reports whether is known fn name applies in transform.
fn (t &Transformer) is_known_fn_name(name string) bool {
	if name in t.fn_ret_types.ids {
		return true
	}
	if !isnil(t.tc) {
		return name in t.tc.fn_ret_types || name in t.tc.fn_param_types
	}
	return false
}

// resolve_receiver_method_name resolves resolve receiver method name information for transform.
fn (t &Transformer) resolve_receiver_method_name(base_id flat.NodeId, method string) string {
	if method == '' {
		return ''
	}
	if smart_base_type_source_id := t.smartcast_receiver_type_name(base_id) {
		smart_base_type_source_id_2 := smart_base_type_source_id

		if alias_method := t.resolve_alias_receiver_method(smart_base_type_source_id_2, method) {
			return alias_method
		}
		if method_name := t.resolve_receiver_method_for_type(smart_base_type_source_id_2, method) {
			return method_name
		}
		if embedded_method := t.resolve_embedded_receiver_method(smart_base_type_source_id_2, method) {
			return embedded_method
		}
	}
	base_type := t.trim_pointer_type(t.lvalue_type(base_id))
	if base_type == 0 {
		return ''
	}
	mut raw_var_clean := u32(0)
	if raw_var_type_source_id := t.raw_var_type_for_expr(base_id) {
		raw_clean := t.trim_pointer_type(raw_var_type_source_id)
		raw_var_clean = raw_clean
		if raw_clean != 0 && raw_clean != base_type {
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
	if raw_const_type_source_id := t.raw_const_type_name_for_expr(base_id) {
		raw_clean := t.trim_pointer_type(raw_const_type_source_id)
		if raw_clean != 0 && raw_clean != base_type && raw_clean != raw_var_clean {
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
		if t.is_interface_type_name(base_type) {
			if embedded_method := t.resolve_embedded_interface_declared_method(base_type, method) {
				if embedded_method == method_name {
					return method_name
				}
			}
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

fn (t &Transformer) smartcast_receiver_type_name(base_id flat.NodeId) ?u32 {
	if t.smartcast_stack.len == 0 { return none }
	key := t.expr_key(base_id)
	if key.len == 0 {
		return none
	}
	sc := t.find_smartcast(key) or { return none }
	mut target := t.smartcast_target_type(sc)
	target = t.trim_all_pointer_type(target)
	if target == 0 {
		return none
	}
	return target
}

fn (t &Transformer) resolve_collection_receiver_method_name(base_id flat.NodeId, method string, clean_base_type_id u32) string {
	clean_base_type := flat.source_type_name(clean_base_type_id)
	if method == '' {
		return ''
	}
	if raw_var_type_source_id := t.raw_var_type_for_expr(base_id) {
		raw_var_type := flat.source_type_name(raw_var_type_source_id)

		raw_clean := if raw_var_type.starts_with('&') { raw_var_type[1..] } else { raw_var_type }
		if raw_clean.len > 0 && raw_clean != clean_base_type {
			if method_name := t.resolve_receiver_method_for_type(flat.source_type_id(raw_clean), method) {
				return method_name
			}
		}
	}
	if raw_const_type_source_id := t.raw_const_type_name_for_expr(base_id) {
		raw_const_type := flat.source_type_name(raw_const_type_source_id)

		raw_clean := if raw_const_type.starts_with('&') {
			raw_const_type[1..]
		} else {
			raw_const_type
		}
		if raw_clean.len > 0 && raw_clean != clean_base_type {
			if method_name := t.resolve_receiver_method_for_type(flat.source_type_id(raw_clean), method) {
				return method_name
			}
		}
	}
	if method_name := t.resolve_receiver_method_for_type(clean_base_type_id, method) {
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
fn (t &Transformer) resolve_receiver_method_for_type(receiver_type_id u32, method string) ?string {
	if !isnil(t.receiver_method_cache) {
		mut cache := t.receiver_method_cache
		if !same_transform_text(cache.module, t.cur_module) || cache.fn_count != t.fn_ret_types.ids.len {
			cache.module = t.cur_module
			cache.fn_count = t.fn_ret_types.ids.len
			cache.entries.clear()
			cache.misses.clear()
		}
		method_id := cache.methods[method] or {
			id := u32(cache.methods.len + 1)
			cache.methods[method] = id
			id
		}
		key := u64(receiver_type_id) << 32 | u64(method_id)
		if cached := cache.entries[key] { return cached }
		if cache.misses[key] { return none }
		if resolved := t.resolve_receiver_method_for_type_uncached(flat.source_type_name(receiver_type_id), method) {
			cache.entries[key] = resolved
			return resolved
		}
		cache.misses[key] = true
		return none
	}
	return t.resolve_receiver_method_for_type_uncached(flat.source_type_name(receiver_type_id), method)
}

fn (t &Transformer) resolve_receiver_method_for_type_uncached(receiver_type string, method string) ?string {
	mut clean_type := receiver_type
	if clean_type.starts_with('&') {
		clean_type = clean_type[1..]
	}
	if clean_type.starts_with('map[') {
		for candidate in t.map_receiver_method_candidates(flat.source_type_id(clean_type), method) {
			if t.is_known_fn_name(candidate) {
				return candidate
			}
		}
	} else {
		if method_name := t.resolve_specialized_generic_receiver_method(flat.source_type_id(clean_type), method) {
			return method_name
		}
		if method_name := t.resolve_imported_flattened_generic_receiver_method(flat.source_type_id(clean_type), method) {
			return method_name
		}
		// A bare receiver type inside a dependency module denotes that module's
		// declaration before any same-named type from the importing program. The
		// global signature index is keyed by short receiver names too, so querying
		// it first can bind (for example) `veb.Context.not_found` to
		// `main.Context.not_found`.
		if !clean_type.contains('.') && t.cur_module.len > 0
			&& t.cur_module !in ['main', 'builtin'] {
			qualified_type := '${t.cur_module}.${clean_type}'
			if isnil(t.tc) || qualified_type in t.tc.structs
				|| qualified_type in t.tc.interface_names || t.tc.type_aliases.contains(qualified_type) {
				qualified_method := '${qualified_type}.${method}'
				if t.is_known_fn_name(qualified_method) {
					return qualified_method
				}
			}
		}
		if !isnil(t.tc) {
			if method_name := t.tc.concrete_method_signature_key(t.local_receiver_type_name(flat.source_type_id(clean_type)),
				method)
			{
				if t.is_known_fn_name(method_name) {
					return method_name
				}
			}
		}
		direct := '${clean_type}.${method}'
		if t.is_known_fn_name(direct) {
			return direct
		}
		if declared := t.declared_receiver_method(flat.source_type_id(clean_type), method) {
			// `declared` is the source spelling (`Any.str`); the suffix index names
			// the module that registered it (`json2.Any.str`).
			registered := t.receiver_method_suffix_index[declared] or { '' }
			if registered == '' || registered == receiver_method_suffix_ambiguous
				|| t.receiver_owns_method(flat.source_type_id(clean_type), registered) {
				return declared
			}
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
			for candidate, _ in t.fn_ret_types.ids {
				if candidate.ends_with(suffix) {
					if matched != '' && matched != candidate {
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
					if matched != '' && matched != qmethod {
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
		if t.is_interface_type_name(flat.source_type_id(clean_type)) && !isnil(t.tc)
			&& method !in t.tc.interface_abstract_method_names(flat.source_type_id(clean_type)) {
			if embedded_interface_method := t.resolve_embedded_interface_declared_method(flat.source_type_id(clean_type),
				method)
			{
				return embedded_interface_method
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
	if method_name := t.unique_receiver_method_suffix_match(t.receiver_method_candidates(flat.source_type_id(clean_type), method)) {
		if t.receiver_owns_method(flat.source_type_id(clean_type), method_name) {
			return method_name
		}
	}
	if !isnil(t.tc) {
		if target_source_id := t.alias_target_type_preserving_main_lock(flat.source_type_id(clean_type)) {
			target := flat.source_type_name(target_source_id)

			if target != clean_type {
				if alias_method := t.resolve_receiver_method_for_type(target_source_id, method) {
					return alias_method
				}
			}
		}
	}
	return none
}

// local_receiver_type_name qualifies a bare receiver type, or the element type of
// a bare array receiver, that names a struct, sum type or enum declared in the
// current dependency module (`Any` -> `toml.Any`), so the type checker does not
// resolve it against a same-named type from another module.
fn (t &Transformer) local_receiver_type_name(clean_type_id u32) u32 {
	clean_type := flat.source_type_name(clean_type_id)
	if isnil(t.tc) || !transform_can_prefix_collection_receiver(t.cur_module) {
		return clean_type_id
	}
	mut elem := clean_type
	for elem.starts_with('[]') {
		elem = elem[2..]
	}
	if elem.len == 0 || elem.contains('.') || elem.contains('[') {
		return clean_type_id
	}
	qualified := '${t.cur_module}.${elem}'
	if qualified in t.tc.structs || t.tc.sum_types.contains(qualified) || qualified in t.tc.enum_names {
		return flat.source_type_id(clean_type[..clean_type.len - elem.len] + qualified)
	}
	return clean_type_id
}

// receiver_owns_method reports whether `method_name`, found by its short receiver
// spelling, is declared for `clean_type` rather than for a same-named type from
// another module (`json2.[]Any.str` must not stringify a `[]toml.Any`).
fn (t &Transformer) receiver_owns_method(clean_type_id u32, method_name string) bool {
	if isnil(t.tc) {
		return true
	}
	receiver := t.tc.parse_type(t.local_receiver_type_name(clean_type_id))
	return t.tc.suffix_indexed_method_fits_receiver(receiver, method_name)
}

fn (t &Transformer) resolve_imported_flattened_generic_receiver_method(receiver_type_id u32, method string) ?string {
	receiver_type := flat.source_type_name(receiver_type_id)
	if receiver_type.contains('.') || !receiver_type.contains('_')
		|| t.bare_struct_name_is_local_to_current_module(receiver_type_id) {
		return none
	}
	for short_base, qualified_base_id in t.qualified_types.ids {
		qualified_base := t.qualified_types.texts.text(qualified_base_id)
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

fn (t &Transformer) declared_receiver_method(receiver_id u32, method string) ?string {
	receiver_comparison_type_id := receiver_id
	receiver := flat.source_type_name(receiver_comparison_type_id)
	if receiver_comparison_type_id == u32(0) || method == '' {
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
		if found != '' && found != name {
			return none
		}
		found = name
	}
	if found.len == 0 {
		return none
	}
	return found
}

fn (t &Transformer) resolve_specialized_generic_receiver_method(receiver_type_id u32, method string) ?string {
	receiver_type := flat.source_type_name(receiver_type_id)
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 {
		return none
	}
	// Synthetic calls created after checking can retain the imported receiver's
	// short generic spelling (`QueryBuilder[Foo]`). Resolve its unique owner
	// before enumerating flattened specializations, otherwise the emitted C call
	// loses the `orm__` prefix even though the specialization itself is qualified.
	if !base.contains('.') && !t.bare_struct_name_is_local_to_current_module(flat.source_type_id(base)) {
		if qualified_source_id := t.qualified_types.type_id(base) {
			qualified := flat.source_type_name(qualified_source_id)

			if qualified != base {
				if resolved := t.resolve_specialized_generic_receiver_method(flat.source_type_id('${qualified}[${args.join(', ')}]'), method) {
					return resolved
				}
			}
		}
	}
	short_args := generic_type_args_short(flat.TypeTextList.from_names(args).ids)
	suffix := generic_type_suffixes(flat.TypeTextList.from_names(args).ids)
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
fn (t &Transformer) resolve_alias_receiver_method(base_type_id u32, method string) ?string {
	// With unchanged declaration tables, no alias receiver can supply a method
	// absent from the suffix index. Avoid constructing type/name candidates for
	// these negative queries, even on the first lookup in a worker batch.
	if !isnil(t.tc) && t.alias_method_candidates_ready
		&& t.alias_receiver_fns_signatures == t.tc.fn_param_types.len
		&& t.alias_receiver_fns_aliases == t.tc.type_aliases.ids.len
		&& t.alias_receiver_fns[short_name_view(method)].len == 0 {
		return none
	}
	if isnil(t.tc) || base_type_id == u32(0) || method == '' || isnil(t.alias_receiver_method_cache) {
		return t.resolve_alias_receiver_method_keyed(base_type_id, method)
	}
	// Answer repeated lookups before building the memo's composite key.
	mut cache := t.alias_receiver_method_cache
	mut recent := cache.recent_cache()
	state, cached := recent.get(t.cur_module, base_type_id, method)
	if state > 0 {
		return cached
	}
	if state < 0 {
		return none
	}
	if resolved := t.resolve_alias_receiver_method_keyed(base_type_id, method) {
		recent.put(t.cur_module, base_type_id, method, 1, resolved)
		return resolved
	}
	recent.put(t.cur_module, base_type_id, method, -1, '')
	return none
}

fn (t &Transformer) resolve_alias_receiver_method_keyed(base_type_id u32, method string) ?string {
	if isnil(t.tc) || base_type_id == u32(0) || method == '' {
		return none
	}
	// Key the memo on the raw spelling so cache hits skip the trim/normalize
	// allocations below; the result is a pure function of the spelling anyway.
	mut cache_key := u64(0)
	if !isnil(t.alias_receiver_method_cache) {
		mut cache := t.alias_receiver_method_cache
		cache_key = cache.key(t.cur_module, base_type_id, method)
		if cached := cache.entries[cache_key] {
			return cached
		}
		if cache.misses[cache_key] {
			return none
		}
	}
	base_type := flat.source_type_name(base_type_id)
	raw_base := trimmed_transform_text(base_type).trim_left('&')
	clean_base_source_id := t.normalize_type_alias(base_type_id)
	clean_base := flat.source_type_name(clean_base_source_id)
	mut exact_aliases := [raw_base]
	if !raw_base.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		exact_aliases << '${t.cur_module}.${raw_base}'
	}
	if imported_source_id := t.resolve_imported_type_name(flat.source_type_id(raw_base)) {
		imported := flat.source_type_name(imported_source_id)

		exact_aliases << imported
	}
	for alias in exact_aliases {
		alias_method := '${alias}.${method}'
		if t.tc.type_aliases.contains(alias) && t.is_known_fn_name(alias_method) {
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
	if !t.is_integer_type_name(clean_base_source_id) {
		if !isnil(t.alias_receiver_method_cache) {
			mut cache := t.alias_receiver_method_cache
			cache.misses[cache_key] = true
		}
		return none
	}
	for name in t.alias_receiver_method_candidates(method) {
		params := t.tc.fn_param_types[name]
		if params.len == 0 {
			continue
		}
		param_name_source_id := t.semantic_type_name(params[0])

		if t.alias_receiver_type_matches(clean_base_source_id, param_name_source_id) {
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

fn (t &Transformer) alias_receiver_method_candidates(method string) []string {
	// Generic-free lowering cannot add source alias receiver methods. Preserve
	// declaration order while avoiding a whole signature-table scan per miss.
	if t.skip_generics && t.alias_method_candidates_ready {
		return t.alias_method_candidates[method]
	}
	mut candidates := []string{}
	method_suffix := '.${method}'
	if t.alias_receiver_fns_signatures == t.tc.fn_param_types.len
		&& t.alias_receiver_fns_aliases == t.tc.type_aliases.ids.len {
		// The same signatures and aliases as listed: the scan below would visit these
		// names in this order. Their parameters can have been registered since.
		for name in t.alias_receiver_fns[short_name_view(method)] {
			if !name.ends_with(method_suffix) {
				continue
			}
			params := t.tc.fn_param_types[name] or { continue }
			if params.len > 0 {
				candidates << name
			}
		}
		return candidates
	}
	for name, params in t.tc.fn_param_types {
		if params.len == 0 || !name.ends_with(method_suffix) {
			continue
		}
		receiver_name := name.all_before_last('.')
		if receiver_name.len > 0 && t.tc.type_aliases.contains(receiver_name) {
			candidates << name
		}
	}
	return candidates
}

// alias_target_type_preserving_main_lock returns the underlying type while retaining
// an explicit `main.` lock introduced by generic specialization. The lock prevents
// an imported generic body from rebasing a program type to a same-named local type.
fn (t &Transformer) alias_target_type_preserving_main_lock(base_type_id u32) ?u32 {
	base_type := flat.source_type_name(base_type_id)
	if isnil(t.tc) || base_type_id == u32(0) {
		return none
	}
	mut clean := trimmed_transform_text(base_type)
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
		if imported_source_id := t.resolve_imported_type_name(flat.source_type_id(clean)) {
			imported := flat.source_type_name(imported_source_id)

			if imported !in candidates {
				candidates << imported
			}
		}
	}
	for candidate in candidates {
		target_source_id := t.tc.type_aliases.type_id(candidate) or { continue }
		target := flat.source_type_name(target_source_id)
		if main_locked && !target.contains('.') && !target.contains('[') {
			return flat.source_type_id('main.${target}')
		}
		return target_source_id
	}
	return none
}

fn (t &Transformer) resolve_embedded_receiver_method(base_type_id u32, method string) ?string {
	if base_type_id == 0 || method == '' {
		return none
	}
	mut lookup_type := t.trim_pointer_type(base_type_id)
	if alias_target := t.alias_target_type_preserving_main_lock(lookup_type) {
		if alias_target != lookup_type {
			if method_name := t.resolve_embedded_receiver_method(alias_target, method) {
				return method_name
			}
		}
	}
	if flat.source_type_name(lookup_type) !in t.structs && flat.source_type_shape(lookup_type).has(.dotted) {
		short_type := flat.source_type_name(lookup_type).all_after_last('.')
		if short_type in t.structs {
			lookup_type = flat.source_type_id(short_type)
		}
	}
	fields := t.embedded_fields[lookup_type] or { return none }
	for field in fields {
		field_type := if field.raw_typ != 0 { field.raw_typ } else { field.typ }
		clean_field := t.trim_pointer_type(field_type)
		if method_name := t.resolve_receiver_method_for_type(clean_field, method) {
			return method_name
		}
		if method_name := t.resolve_embedded_receiver_method(clean_field, method) {
			return method_name
		}
	}
	return none
}

// Embedded method signatures are copied onto child interfaces, but default method bodies
// remain owned by the interface that declared them.
fn (t &Transformer) resolve_embedded_interface_declared_method(iface_name_id u32, method string) ?string {
	if isnil(t.tc) {
		return none
	}
	mut seen := map[u32]bool{}
	return t.resolve_embedded_interface_declared_method_inner(iface_name_id, method, mut seen)
}

fn (t &Transformer) resolve_embedded_interface_declared_method_inner(iface_name_id u32, method string, mut seen map[u32]bool) ?string {
	name := t.tc.interface_metadata_name(iface_name_id)
	if name == 0 || seen[name] {
		return none
	}
	seen[name] = true
	for embed_source_id in t.tc.interface_embeds.type_ids(flat.source_type_name(name)) or { []u32{} } {
		if declared := t.declared_receiver_method(embed_source_id, method) {
			return declared
		}
		if declared := t.resolve_embedded_interface_declared_method_inner(embed_source_id, method,
			mut seen)
		{
			return declared
		}
	}
	return none
}

// alias_receiver_type_matches converts alias receiver type matches data for transform.
fn (t &Transformer) alias_receiver_type_matches(base_type_id u32, alias_type_id u32) bool {
	base_type_comparison_type_id_5 := base_type_id

	alias_type := flat.source_type_name(alias_type_id)
	if base_type_id == u32(0) || alias_type_id == u32(0) {
		return false
	}
	clean_alias := if alias_type.starts_with('&') { alias_type[1..] } else { alias_type }
	alias_target_source_id := t.normalize_type_alias(flat.source_type_id(clean_alias))
	alias_target_comparison_type_id_2 := alias_target_source_id

	if alias_target_comparison_type_id_2 == base_type_comparison_type_id_5 {
		return true
	}
	if !isnil(t.tc) {
		alias_c_type := t.tc.c_type(t.tc.parse_type(alias_target_source_id))
		base_c_type := t.tc.c_type(t.tc.parse_type(base_type_id))
		if alias_c_type == base_c_type {
			return true
		}
	}
	return t.is_integer_type_name(alias_target_source_id) && t.is_integer_type_name(base_type_id)
}

// is_integer_type_name reports whether is integer type name applies in transform.
fn (t &Transformer) is_integer_type_name(typ_id u32) bool {
	return typ_id == flat.source_type_int || typ_id == flat.source_type_i8 || typ_id == flat.source_type_i16 || typ_id == flat.source_type_i32 || typ_id == flat.source_type_i64 || typ_id == flat.source_type_u8 || typ_id == flat.source_type_u16 || typ_id == flat.source_type_u32 || typ_id == flat.source_type_u64 || typ_id == flat.source_type_rune || typ_id == flat.source_type_isize || typ_id == flat.source_type_usize || typ_id == transform_source_type_i128 || typ_id == transform_source_type_u128
}

// raw_var_type_for_expr supports raw var type for expr handling for Transformer.
fn (t &Transformer) raw_var_type_for_expr(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		typ_source_id := t.raw_var_type(node.value)

		if typ_source_id != 0 {
			return typ_source_id
		}
	}
	if node.kind == .cast_expr && node.value.len > 0 {
		return flat.source_type_id(node.value)
	}
	if node.kind == .selector {
		if raw_type_source_id := t.raw_selector_field_type(id) {
			return raw_type_source_id
		}
	}
	if node.typ != 0 {
		return node.typ
	}
	return none
}

// raw_const_type_name_for_expr supports raw const type name for expr handling for Transformer.
fn (t &Transformer) raw_const_type_name_for_expr(id flat.NodeId) ?u32 {
	if int(id) < 0 || isnil(t.tc) {
		return none
	}
	node := t.a.nodes[int(id)]
	if t.selector_const_base_is_value(node) {
		return none
	}
	if node.kind == .ident {
		if (t.raw_var_type(node.value)) != 0 {
			return none
		}
		if t.ident_compiles_to_global_symbol(node.value) {
			return none
		}
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
fn (t &Transformer) resolve_method_receiver_type(call_node flat.Node) u32 {
	if call_node.children_count == 0 {
		return u32(0)
	}
	fn_id := t.a.children[call_node.children_start]
	if int(fn_id) < 0 {
		return u32(0)
	}
	fn_node := t.a.nodes[int(fn_id)]
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return u32(0)
	}
	base_id := t.a.children[fn_node.children_start]
	return t.resolve_expr_type(base_id)
}

fn (mut t Transformer) normalize_implicit_receiver_generic_call(id flat.NodeId, node flat.Node) flat.NodeId {
	if t.cur_fn_receiver_name == 0 || node.children_count == 0 {
		return id
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || callee.value.contains('.')
		|| (t.raw_var_type(callee.value)) != 0 {
		return id
	}
	decls := t.cached_generic_fn_decls()
	receiver_type_source_id := t.var_type(t.cur_fn_receiver_name_text())

	for key in t.generic_receiver_methods_by_name[callee.value] {
		decl := decls[key] or { continue }
		if !t.generic_receiver_decl_matches_type(receiver_type_source_id, decl, t.cur_module)
			|| !t.generic_call_arg_count_matches_decl_with_implicit_receiver(node, decl) {
			continue
		}
		mut children := []flat.NodeId{cap: int(node.children_count) + 1}
		children << t.make_ident(key)
		children << t.make_ident(t.cur_fn_receiver_name_text())
		for i in 1 .. node.children_count {
			children << t.a.child(&node, i)
		}
		start := t.a.children.len
		t.a.children << children
		return t.a.add_node(flat.Node{
			kind:           .call
			op:             node.op
			children_start: start
			children_count: flat.child_count(children.len)
			pos:            node.pos
			value:          node.value
			typ:            node.typ
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
	mut resolved_name := ''
	if !isnil(t.tc) {
		resolved_name = t.tc.resolved_call_name(id) or { '' }
	}
	normalized_id := t.a.add_node(flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
		value:          if resolved_name.len == 0 && t.generic_call_base_is_fn_value(base_id, base) {
			''
		} else {
			type_arg
		}
		typ:            node.typ
	})
	if !isnil(t.tc) && t.tc.c_backed_alias_method_name(resolved_name) {
		t.set_generated_resolved_call(normalized_id, resolved_name)
	}
	return normalized_id
}

fn (t &Transformer) generic_call_base_is_fn_value(base_id flat.NodeId, base flat.Node) bool {
	if base.kind != .ident || t.ident_is_direct_function_callee(base.value)
		|| t.is_known_type_name(flat.source_type_id(base.value)) {
		return false
	}
	for candidate in [flat.source_type_name(t.raw_var_type(base.value)),
		flat.source_type_name(t.var_type(base.value)), base.typ_text(),
		flat.source_type_name(t.node_type(base_id))] {
		if t.is_fn_pointer_type_name(flat.source_type_id(candidate)) {
			return true
		}
	}
	return false
}

// generic_call_type_arg_name supports generic call type arg name handling for Transformer.
fn (t &Transformer) generic_call_type_arg_name(id flat.NodeId) u32 {
	if int(id) < 0 {
		return u32(0)
	}
	node := t.a.node(id)
	match node.kind {
		.ident {
			// Map type arguments are represented by a synthetic `Map_key_value`
			// identifier, while `typ` retains the source-level `map[key]value`.
			// Keep the latter so comptime type groups and generic substitution can
			// still inspect both map components.
			if flat.source_type_shape(node.typ).has(.map_type) {
				return node.typ
			}
			// A generic parameter is lexical: inside `fn outer[T]`, `T` in
			// `inner[T](value)` names the parameter even when the writing file also
			// imports a type spelled `T` (`import pkg { T }`), matching the checker,
			// where the parameter wins over the file's selective imports
			// (`qualify_type_text_impl` checks the generic parameters first).
			// Resolving it through them would specialize the callee for `pkg.T` and
			// lose the substitution of the caller's type argument.
			if node.value in t.active_generic_params
				|| t.node_has_enclosing_generic_param(id, node.value) {
				return flat.source_type_id(node.value)
			}
			// A bare spelling must be resolved in the file that wrote the call:
			// `import model { Context }` makes `Context` mean `model.Context` there
			// even when another imported module declares a same-named type. A global
			// short-name index would pick whichever type was indexed first, so the
			// rewritten call and the emitted specialization would disagree.
			if resolved_source_id := t.selective_import_type_name_for_file(t.node_file_or(int(id), t.cur_file), flat.source_type_id(node.value)) {
				return resolved_source_id
			}
			return flat.source_type_id(node.value)
		}
		.selector {
			if node.children_count == 0 {
				return flat.source_type_id(node.value)
			}
			base := flat.source_type_name(t.generic_call_type_arg_name(t.a.child(node, 0)))
			if base.len == 0 {
				return flat.source_type_id(node.value)
			}
			// Source-qualified arguments belong to the caller's import scope.
			// Substituted generic arguments are identifiers and are already canonical.
			if imported := t.file_import_module(t.node_file_or(int(id), t.cur_file), base) {
				return flat.source_type_id('${imported}.${node.value}')
			}
			return flat.source_type_id('${base}.${node.value}')
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return u32(0)
			}
			base := flat.source_type_name(t.generic_call_type_arg_name(t.a.child(node, 0)))
			if base.len == 0 {
				return u32(0)
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := flat.source_type_name(t.generic_call_type_arg_name(t.a.child(node, i)))
				if arg.len == 0 {
					return u32(0)
				}
				args << arg
			}
			return flat.source_type_id('${base}[${args.join(', ')}]')
		}
		.array_init {
			// The value is the element spelling for a dynamic array, including
			// a fixed-array element. The parser's type retains the outer `[]`.
			if flat.source_type_shape(node.typ).has(.array_type) {
				return node.typ
			}
			if node.value.len > 0 {
				if node.value.starts_with('[]') {
					return flat.source_type_id('[]${node.value}')
				}
				if node.value.starts_with('[') {
					return flat.source_type_id(node.value)
				}
				return flat.source_type_id('[]${node.value}')
			}
			return u32(0)
		}
		.map_init {
			return flat.source_type_id(node.value)
		}
		.struct_init {
			return flat.source_type_id(node.value)
		}
		.struct_decl {
			return flat.source_type_id(node.value)
		}
		.prefix {
			if node.children_count == 0 {
				return u32(0)
			}
			child := flat.source_type_name(t.generic_call_type_arg_name(t.a.child(node, 0)))
			if child.len == 0 {
				return u32(0)
			}
			if node.op == .amp {
				return flat.source_type_id('&${child}')
			}
			return flat.source_type_id(child)
		}
		else {
			return u32(0)
		}
	}
}

// node_enclosing_generic_params returns the generic parameter names of the
// declaration that lexically encloses `id`. Synthesized nodes have no source
// parent entry; a live specialization records its parameters in
// `active_generic_params` instead.
fn (t &Transformer) node_enclosing_generic_params(id flat.NodeId) []string {
	if int(id) < 0 || int(id) >= t.source_parent_ids.len {
		return []string{}
	}
	mut cursor := int(id)
	for _ in 0 .. t.a.nodes.len {
		parent_id := t.source_parent_id(cursor)
		if parent_id < 0 || parent_id == cursor || parent_id >= t.a.nodes.len {
			return []string{}
		}
		parent := t.a.node(flat.NodeId(parent_id))
		if parent.kind in [.fn_decl, .struct_decl, .type_decl, .interface_decl, .c_fn_decl] {
			mut params := parent.generic_params().clone()
			if parent.kind == .fn_decl {
				module_name := t.node_module_or(parent_id, t.cur_module)
				for param in t.declared_generic_receiver_param_names(*parent, module_name) {
					if param !in params {
						params << param
					}
				}
			}
			return params
		}
		cursor = parent_id
	}
	return []string{}
}

// node_has_enclosing_generic_param reports whether `name` is a generic parameter
// of a declaration that lexically encloses `id`. Such a parameter keeps its
// meaning even when the writing file selectively imports a type with the same
// spelling, so resolution has to see it before it consults those imports.
fn (t &Transformer) node_has_enclosing_generic_param(id flat.NodeId, name string) bool {
	if name.len == 0 {
		return false
	}
	return name in t.node_enclosing_generic_params(id)
}

// type_arg_text_has_enclosing_generic_param reports whether `text` names a
// generic parameter of the declaration that lexically encloses `id`. An
// argument that does is not concrete: the enclosing specialization substitutes
// it while it is cloned, so it must not be resolved through the writing file's
// imports here.
fn (t &Transformer) type_arg_text_has_enclosing_generic_param(id flat.NodeId, text string) bool {
	params := t.node_enclosing_generic_params(id)
	return params.len > 0 && generic_text_contains_param(text, params)
}

fn (t &Transformer) generic_call_type_args_name(index_node flat.Node) string {
	if index_node.kind != .index || index_node.children_count < 2 || index_node.value == 'range' {
		return ''
	}
	// What the brackets hold first: `xs[0]` or `xs[i + 1]` name no type, and
	// telling a value index from a generic call resolves types.
	mut args := []string{}
	for i in 1 .. index_node.children_count {
		arg := flat.source_type_name(t.generic_call_type_arg_name(t.a.child(&index_node, i)))
		if arg.len == 0 {
			return ''
		}
		args << arg
	}
	if t.index_callee_is_value_index(index_node) {
		return ''
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
	if base.kind == .selector && base.children_count > 0
		&& t.call_selector_base_is_namespace(t.a.child(&base, 0), base.value, '') {
		// A module/type-qualified generic callee is never a runtime index, even
		// when the checker's cached selector type is itself indexable (for example
		// the `string` parameter type cached for `json2.decode`).
		return false
	}
	if t.type_name_is_indexable(base.typ)
		|| t.type_name_is_indexable(t.raw_checker_node_type(base_id)) {
		return true
	}
	if base.kind == .ident && t.type_name_is_indexable(t.var_type(base.value)) {
		// Bindings are kept by name after the block that declared them, so a local
		// of an earlier block can name the generic function the call resolved to.
		return !t.index_call_resolved_to_fn(base_id, base.value)
	}
	if base.kind == .ident || base.kind == .selector {
		base_type_source_id_6 := t.resolve_expr_type(base_id)

		if t.type_name_is_indexable(base_type_source_id_6) {
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

// index_call_resolved_to_fn reports whether the checker, which resolves names in
// their block scope, resolved the call around `name[...]` to the function `name`.
fn (t &Transformer) index_call_resolved_to_fn(base_id flat.NodeId, name string) bool {
	if isnil(t.tc) || int(base_id) < 0 || int(base_id) >= t.source_parent_ids.len {
		return false
	}
	index_id := int(t.source_parent_ids[int(base_id)])
	if index_id < 0 || index_id >= t.source_parent_ids.len {
		return false
	}
	call_id := int(t.source_parent_ids[index_id])
	if call_id < 0 {
		return false
	}
	call := t.a.nodes[call_id]
	if call.kind != .call || call.children_count == 0 || int(t.a.child(&call, 0)) != index_id {
		return false
	}
	resolved := t.tc.resolved_call_name(flat.NodeId(call_id)) or { return false }
	return resolved.all_before('[').all_after_last('.') == name
}

fn (t &Transformer) type_name_is_indexable(name_id u32) bool {
	mut clean := t.normalize_type_alias(name_id)
	if clean == 0 {
		return false
	}
	if flat.source_type_shape(clean).has(.pointer) {
		clean = t.normalize_type_alias(flat.source_type_id(flat.source_type_name(clean)[1..]))
	}
	return clean == flat.source_type_string || flat.source_type_shape(clean).has(.array_type) || flat.source_type_shape(clean).has(.map_type)
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
			kind:  .call
			op:    node.op
			pos:   node.pos
			value: node.value
			typ:   node.typ
		})
	}
	if addr := t.transform_builtin_addr_call(node) {
		return addr
	}
	t.validate_specialized_fn_value_mut_args(node)
	call_name := t.call_name_for_node(id, node)
	if node.children_count == 2 && (call_name in ['drop_owned', 'builtin.drop_owned']
		|| call_name.starts_with('builtin.drop_owned_T_') || call_name.starts_with('drop_owned_T_')) {
		arg_id := t.unwrap_parens(t.a.child(&node, 1))
		arg := t.a.nodes[int(arg_id)]
		if arg.kind == .ident && arg.value in t.heaped_amp_locals {
			// Promotion adds storage indirection, not a language-level reference.
			// Explicit destruction still targets the original value's elements.
			storage := t.transform_expr_preserving_pointer_value(arg_id)
			value := t.make_prefix(.mul, storage)
			t.set_node_typ(int(value), flat.source_type_id(flat.source_type_name(t.var_type(arg.value))[1..]))
			return t.copy_node_with_children(node, [t.a.child(&node, 0), value])
		}
		if t.tc.ownership_sum_drop_consumes_storage(arg_id) {
			// The checker validated consumption of this exact named sum owner.
			storage_type := t.raw_expr_type_without_smartcast(arg_id)
			value := t.make_plain_expr_for_smartcast(arg_id)
			t.set_node_typ(int(value), storage_type)
			return t.copy_node_with_children(node, [t.a.child(&node, 0), value])
		}
	}
	mut params := t.call_param_types_for_node(call_name, node)
	mut param_type_names := t.call_param_type_names(params)
	mut is_generic_variadic := false
	if concrete_params := t.concrete_generic_call_param_types(id, node) {
		params = concrete_params.clone()
		is_generic_variadic = t.concrete_generic_call_is_variadic(id, node)
		if concrete_names_source_ids := t.concrete_generic_call_param_type_names(id, node) {
			param_type_names = concrete_names_source_ids.clone()
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
	is_variadic := !is_c_variadic && (t.call_is_variadic_for_node(call_name, node)
		|| is_generic_variadic || (params.len > 0 && params[params.len - 1] is types.Array
		&& (explicit_args > expected_explicit || has_spread_at_variadic_slot)))
	variadic_idx := if is_variadic && params.len > 0 && params[params.len - 1] is types.Array {
		params.len - 1
	} else {
		-1
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	mut mut_optional_value_writebacks := []flat.NodeId{}
	// Heap-backed fixed-array views are refreshed after argument evaluation. Keep
	// ordinary operands in source order too, before a later argument's prelude.
	ordered_fixed_args := t.call_has_mut_fixed_array_args(node, params, param_offset, variadic_idx)
	mut snapshotted_args := 1
	saved_in_call_callee := t.in_call_callee
	callee_id := t.a.children[node.children_start]
	immediate_bound_method := t.immediate_bound_method_value_allocates_runtime_closure(callee_id)
	immediate_factory_closure := t.call_returns_exclusive_closure(callee_id)
	immediate_fresh_closure := immediate_bound_method || immediate_factory_closure
	mut immediate_closure_type := u32(0)
	mut immediate_closure_cleanup := ''
	immediate_closure_capture_may_escape := immediate_bound_method || immediate_factory_closure
		|| t.immediate_fn_literal_capture_may_escape(callee_id)
	if immediate_fresh_closure {
		immediate_closure_type = t.fresh_runtime_closure_type(callee_id) or { u32(0) }
		if immediate_closure_type != 0 {
			// If/match callback branches otherwise retain the callee's return
			// type on their value temporary instead of the callback type.
			t.set_fresh_runtime_closure_expr_type(callee_id, immediate_closure_type)
		}
	}
	ordered_callee_id := if ordered_fixed_args {
		t.snapshot_fixed_array_call_callee(id, node, callee_id)
	} else {
		callee_id
	}
	t.in_call_callee = true
	mut transformed_callee := t.const_fn_call_target(ordered_callee_id) or {
		if param_offset == 1 && params.len > 0 {
			if converted_callee := t.transform_method_callee_receiver_for_param(ordered_callee_id, t.semantic_type_name(params[0])) {
				converted_callee
			} else {
				t.transform_expr(ordered_callee_id)
			}
		} else {
			t.transform_expr(ordered_callee_id)
		}
	}
	t.in_call_callee = saved_in_call_callee
	if t.fn_literal_has_runtime_captures(callee_id) || immediate_fresh_closure {
		closure_type := if immediate_closure_type != 0 {
			immediate_closure_type
		} else {
			t.fresh_runtime_closure_type(callee_id) or { t.node_type(transformed_callee) }
		}
		if closure_type != 0 {
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
		if ordered_fixed_args {
			t.snapshot_fixed_array_call_args(mut new_children, snapshotted_args)
			snapshotted_args = new_children.len
		}
		arg_idx := new_children.len - 1
		param_idx := arg_idx + param_offset
		arg_id := t.a.child(&node, i)
		arg_node := t.a.nodes[int(arg_id)]
		param_type := if param_idx < param_type_names.len {
			param_type_names[param_idx]
		} else {
			u32(0)
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
			if variadic_idx >= 0 && param_idx == variadic_idx {
				variadic_type := params[variadic_idx]
				if variadic_type is types.Array {
					elem_type_source_id := t.semantic_type_name(variadic_type.elem_type)
					if flat.source_type_shape(t.normalize_type_alias_chain(elem_type_source_id)).has(.pointer) {
						new_children << t.pack_variadic_args(node, i, variadic_type.elem_type)
						variadic_tail_supplied = true
						break
					}
				}
			}
			// Trailing `key: value` args against the variadic `...Struct` slot
			// (surfacing as `[]Struct`) desugar to one element of the elem
			// struct type; a non-variadic `[]Struct` param must not collapse.
			struct_param_type := if variadic_idx >= 0 && param_idx == variadic_idx
				&& flat.source_type_shape(param_type).has(.array_type) {
				flat.source_type_id(flat.source_type_name(param_type)[2..])
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
				if arg_node.kind == .ident && arg_node.value == t.cur_fn_variadic_param {
					new_children << t.transform_expr_for_type(arg_id,
						t.semantic_type_name(variadic_type))
					variadic_tail_supplied = true
					i++
					break
				}
				if t.call_arg_is_packed_variadic_tail(arg_id, variadic_type) {
					packed_arg := t.transform_expr(arg_id)
					t.packed_variadic_tail_types[int(packed_arg)] = t.semantic_type_name(variadic_type)
					new_children << packed_arg
					variadic_tail_supplied = true
					i++
					break
				}
				if arg_node.kind == .prefix && arg_node.value == '...'
					&& arg_node.children_count > 0 {
					spread_id := t.a.child(&arg_node, 0)
					new_children << t.transform_variadic_spread_arg_for_param(spread_id, variadic_type, param_type)
					i++
					break
				}
				new_children << t.pack_variadic_args(node, i, variadic_type.elem_type)
				break
			}
		}
		if variadic_idx < 0 && arg_node.kind == .prefix && arg_node.value == '...'
			&& arg_node.children_count > 0 && params.len > 0 && param_idx < params.len {
			spread_id := t.a.child(&arg_node, 0)
			spread_base := t.stable_expr_for_reuse(spread_id)
			spread_count := params.len - param_idx
			t.add_spread_length_guard(spread_base, spread_count)
			for spread_offset in 0 .. spread_count {
				expected_source_id := t.semantic_type_name(params[param_idx + spread_offset])

				index_arg := t.make_spread_index_for_expected_param(spread_base, spread_offset, expected_source_id)
				new_children << t.transform_call_arg_for_named_param(index_arg, expected_source_id, call_name)
			}
			i++
			continue
		}
		if variadic_idx < 0 && param_idx < params.len {
			arg_type := t.tc.resolve_type(arg_id)
			if arg_type is types.MultiReturn && arg_type.types.len == params.len - param_idx {
				items := arg_type.types
				multi_type_source_id := t.multi_return_type_name(items)

				value := t.stable_transformed_expr_for_reuse(t.transform_expr(arg_id), multi_type_source_id, 'multi_arg')
				for multi_idx, item_type in items {
					expected_idx := param_idx + multi_idx
					if expected_idx >= params.len {
						break
					}
					field_type_source_id := t.semantic_type_name(item_type)

					field := t.make_selector(value, 'arg${multi_idx}', field_type_source_id)
					clean_field_type_source_id := t.comptime_normalize_type_alias_chain(field_type_source_id)
					clean_field_type_shape := flat.source_type_shape(clean_field_type_source_id)
					if !clean_field_type_shape.has(.pointer) && t.is_fixed_array_type(clean_field_type_source_id) {
						t.set_node_generic_params(int(field), [multi_return_fixed_array_arg_marker])
					}
					new_children << t.transform_call_arg_for_named_param(field, param_type_names[expected_idx], call_name)
				}
				i++
				continue
			}
		}
		if converted := t.transform_mut_optional_pointer_address_call_arg(arg_id, param_type) {
			new_children << converted
		} else if converted := t.transform_mut_optional_value_call_arg(arg_id, param_type) {
			new_children << converted.value
			mut_optional_value_writebacks << converted.writeback
		} else {
			new_children << t.transform_call_arg_for_named_param(arg_id, param_type, call_name)
		}
		i++
	}
	if variadic_idx >= 0 && !variadic_tail_supplied && explicit_args == variadic_idx - param_offset {
		variadic_type := params[variadic_idx]
		if variadic_type is types.Array {
			new_children << t.pack_variadic_args(node, int(node.children_count), variadic_type.elem_type)
		}
	}
	t.append_missing_params_struct_args(mut new_children, params, param_offset)
	if ordered_fixed_args {
		t.snapshot_fixed_array_call_args(mut new_children, snapshotted_args)
	}
	mut typ := node.typ
	concrete_ret_source_id := t.concrete_generic_call_return_type(id, node)
	if concrete_ret_source_id != 0 {
		typ = concrete_ret_source_id
	} else if typ == 0 && immediate_closure_type != 0 {
		typ = fn_type_return_type_text(immediate_closure_type) or { u32(0) }
	}
	if t.rewrite_children_in_place(id, new_children) {
		if typ != t.a.nodes[int(id)].typ {
			t.set_node_typ(int(id), typ)
		}
		call := t.finish_immediate_closure_call(id, immediate_closure_cleanup, typ, immediate_closure_capture_may_escape)
		return t.finish_mut_optional_value_call(call, typ, mut_optional_value_writebacks)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .call
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          if int(id) >= 0 && int(id) < t.a.nodes.len {
			t.a.nodes[int(id)].value
		} else {
			node.value
		}
		typ:            typ
	})
	t.copy_cloned_resolution(id, new_id)
	if spec := t.generic_call_spec_cache[int(id)] {
		mut cached_args := spec.args.ids.clone()
		if new_children.len > 0 {
			callee := t.a.nodes[int(new_children[0])]
			if callee.kind == .ident {
				if exact_source_ids := t.recorded_generic_specialization_args(callee.value) {
					cached_args = exact_source_ids.clone()
				}
			}
		}
		t.generic_call_spec_cache[int(new_id)] = GenericCallSpec{
			decl_key: spec.decl_key
			args:     flat.TypeTextList{ ids: cached_args.clone() }
		}
	}
	call := t.finish_immediate_closure_call(new_id, immediate_closure_cleanup, typ, immediate_closure_capture_may_escape)
	return t.finish_mut_optional_value_call(call, typ, mut_optional_value_writebacks)
}

fn (mut t Transformer) mark_packed_variadic_tail(value flat.NodeId, array_type u32) flat.NodeId {
	t.packed_variadic_tail_types[int(value)] = array_type
	return value
}

fn (t &Transformer) call_arg_is_packed_variadic_tail(arg_id flat.NodeId, variadic_type types.Array) bool {
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	if packed_type := t.packed_variadic_tail_types[int(arg_id)] {
		return packed_type == t.semantic_type_name(variadic_type)
	}
	arg := t.a.nodes[int(arg_id)]
	// Source identifiers can legally use this prefix in translated code. Only
	// positionless transformer temporaries represent an already-packed tail.
	if arg.kind != .ident || arg.pos.is_valid() || !arg.value.starts_with('__varargs_') {
		return false
	}
	actual_type := (if arg.typ != 0 {
		arg.typ
	} else {
		t.var_type(arg.value)
	})
	if actual_type == 0 {
		return false
	}
	expected_type_source_id := t.semantic_type_name(variadic_type)

	return t.normalize_type_alias(actual_type) == t.normalize_type_alias(expected_type_source_id)
}

fn (mut t Transformer) transform_mut_optional_pointer_address_call_arg(arg_id flat.NodeId, param_type_id u32) ?flat.NodeId {
	param_type := flat.source_type_name(param_type_id)
	if int(arg_id) < 0 || param_type.len < 4 || !param_type.starts_with('&?') {
		return none
	}
	arg_node := t.a.nodes[int(arg_id)]
	if !arg_node.is_mut || arg_node.kind != .prefix || arg_node.op != .amp
		|| arg_node.children_count != 1 {
		return none
	}
	option_type_source_id := t.qualify_optional_type(flat.source_type_id(param_type[1..]))

	payload_type_source_id := t.optional_base_type(option_type_source_id)
	payload_type_shape := flat.source_type_shape(payload_type_source_id)
	if !payload_type_shape.has(.pointer) {
		return none
	}
	source_id := t.a.child(&arg_node, 0)
	mut source_type := t.node_type(source_id)
	if source_type == 0 {
		source_type = t.resolve_expr_type(source_id)
	}
	if t.normalize_type_alias(source_type) != t.normalize_type_alias(payload_type_source_id) {
		return none
	}
	tmp_name := t.new_temp('mut_optional_ptr_arg')
	value := t.transform_expr_for_type(source_id, payload_type_source_id)
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_optional_some(value,
		option_type_source_id), option_type_source_id)
	tmp := t.make_ident(tmp_name)
	t.set_node_typ(int(tmp), option_type_source_id)
	addr := t.make_prefix(.amp, tmp)
	t.set_node_typ(int(addr), param_type_id)
	return addr
}

fn (mut t Transformer) transform_mut_optional_value_call_arg(arg_id flat.NodeId, param_type_id u32) ?MutOptionalValueCallArg {
	param_type := flat.source_type_name(param_type_id)
	if int(arg_id) < 0 || param_type.len < 3 || !param_type.starts_with('&?') {
		return none
	}
	arg_node := t.a.nodes[int(arg_id)]
	// The checker only passes an option to a `mut` parameter with `mut`, but the field
	// selector a `$for` body substitutes for `mut s.$(field.name)` no longer carries it.
	if !arg_node.is_mut && !param_type.starts_with('&?&') {
		return none
	}
	if _ := t.unwrapped_mut_optional_view_param(arg_id) {
		// Forwarded as it is (transform_mut_optional_view_forward_arg).
		return none
	}
	// A local moved to the heap is a pointer to its option.
	heaped_local := t.heaped_amp_local_arg_name(arg_id)
	mut arg_type_source_id := t.node_type(arg_id)
	if heaped_local.len > 0 && flat.source_type_name(arg_type_source_id).starts_with('&?') {
		arg_type_source_id = flat.source_type_id(flat.source_type_name(arg_type_source_id)[1..])
	}
	arg_type_source_id = t.qualify_optional_type(arg_type_source_id)
	arg_type_shape := flat.source_type_shape(arg_type_source_id)
	if !arg_type_shape.has(.option_type) {
		return none
	}
	payload_type_source_id := t.optional_base_type(arg_type_source_id)

	param_option_source_id := t.qualify_optional_type(flat.source_type_id(param_type[1..]))
	param_option := flat.source_type_name(param_option_source_id)
	target_payload := flat.source_type_name(t.optional_base_type(param_option_source_id))
	if !target_payload.starts_with('&') {
		return none
	}
	if t.normalize_type_alias(payload_type_source_id) != t.normalize_type_alias(flat.source_type_id(target_payload[1..])) {
		return none
	}
	target_option := '${param_option[..1]}${target_payload}'

	lvalue := if heaped_local.len > 0 {
		deref := t.make_prefix(.mul, t.make_ident(heaped_local))
		t.set_node_typ(int(deref), arg_type_source_id)
		deref
	} else {
		value := if arg_node.kind == .ident {
			t.smartcast_ident_value(arg_node.value) or { t.transform_lvalue(arg_id) }
		} else {
			t.transform_lvalue(arg_id)
		}
		t.stabilize_transformed_lvalue_for_reuse(value)
	}
	tmp_name := t.new_temp('mut_optional_arg')
	initial := t.make_optional_none(flat.source_type_id(target_option))
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, initial, flat.source_type_id(target_option))
	payload := t.make_selector(lvalue, 'value', payload_type_source_id)
	payload_addr := t.make_prefix(.amp, payload)
	t.set_node_typ(int(payload_addr), flat.source_type_id(target_payload))
	set_some := t.make_assign(t.make_ident(tmp_name), t.make_optional_some(payload_addr, flat.source_type_id(target_option)))
	t.pending_stmts << t.make_if(t.make_selector(lvalue, 'ok', flat.source_type_bool), t.make_block([set_some]), t.make_empty())

	tmp := t.make_ident(tmp_name)
	t.set_node_typ(int(tmp), flat.source_type_id(target_option))
	result_payload_ptr := t.make_selector(tmp, 'value', flat.source_type_id(target_payload))
	result_payload := t.make_prefix(.mul, result_payload_ptr)
	t.set_node_typ(int(result_payload), payload_type_source_id)
	write_some := t.make_assign(lvalue, t.make_optional_some(result_payload, arg_type_source_id))
	write_none := t.make_assign(lvalue, t.make_optional_none(arg_type_source_id))
	writeback := t.make_if(t.make_selector(tmp, 'ok', flat.source_type_bool), t.make_block([write_some]), t.make_block([write_none]))

	addr := t.make_prefix(.amp, tmp)
	t.set_node_typ(int(addr), param_type_id)
	t.mark_mut_optional_arg(addr, arg_type_source_id)
	return MutOptionalValueCallArg{
		value:     addr
		writeback: writeback
	}
}

// transform_mut_plain_value_optional_call_arg passes a `mut` value that is not an option
// to a `mut a ?T` parameter, like V 0.5.2: in a temporary option, so that `a = none` or
// `a = value` in the callee only replaces the temporary. The payload is passed by reference
// (`&?&T`, see `types.mut_param_semantic_type`), so the temporary refers to the caller's
// value, and the callee's changes to it (`if mut v := a { v << 1 }`) reach the caller.
fn (mut t Transformer) transform_mut_plain_value_optional_call_arg(arg_id flat.NodeId, param_type_id u32) ?flat.NodeId {
	param_type := flat.source_type_name(param_type_id)
	arg_type_source_id := t.node_type(arg_id)
	if arg_type_source_id == 0 || flat.source_type_shape(arg_type_source_id).has(.option_type) {
		return none
	}
	payload := param_type[2..].trim_string_left('&')
	if payload.len == 0 {
		return none
	}
	payload_type_source_id := flat.source_type_id(payload)
	clean_payload := t.normalize_type_alias(payload_type_source_id)
	if !t.type_is_value_or_reference_of(arg_type_source_id, clean_payload) {
		return none
	}
	value := t.transform_expr(arg_id)
	// A caller's own `mut` parameter is the address of its value; a heap-escaped local
	// is already dereferenced by the transform.
	mut value_type_source_id := t.node_type(value)
	if value_type_source_id == 0 {
		value_type_source_id = arg_type_source_id
	}
	value_is_reference := t.normalize_type_alias(value_type_source_id) != clean_payload
	pointer_type := flat.source_type_id('&${payload}')
	payload_value := if value_is_reference {
		value
	} else {
		t.make_prefix(.amp, value)
	}
	if payload_value != value {
		t.set_node_typ(int(payload_value), pointer_type)
	}
	option_type := flat.source_type_id('?&${payload}')
	tmp_name := t.new_temp('mut_optional_arg')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_optional_some(payload_value,
		option_type), option_type)
	option_addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.set_node_typ(int(option_addr), param_type_id)
	t.mark_mut_optional_arg(option_addr, flat.source_type_id('?${payload}'))
	return option_addr
}

// transform_mut_optional_view_forward_arg passes `mut a` on to another `mut ?T` parameter,
// where `a` is a `mut a ?T` parameter that its body reads as `?T` (mut_optional_view_param).
// Both are `&?&T`, so the callee gets the option that refers to the caller's value.
fn (mut t Transformer) transform_mut_optional_view_forward_arg(arg_id flat.NodeId) ?flat.NodeId {
	view_param := t.unwrapped_mut_optional_view_param(arg_id)?
	slot_type := flat.source_type_id('&${flat.source_type_name(view_param.storage)}')
	addr := t.make_prefix(.amp, t.mut_optional_view_storage(view_param))
	t.set_node_typ(int(addr), slot_type)
	t.mark_mut_optional_arg(addr, view_param.view)
	return addr
}

// mark_mut_optional_arg marks `addr`, the `&?&T` argument built for a `mut a ?T` parameter,
// with the `?T` it stands for, which generic inference matches against the parameter.
fn (mut t Transformer) mark_mut_optional_arg(addr flat.NodeId, view_type u32) {
	if view_type == 0 {
		return
	}
	mut params := t.a.nodes[int(addr)].generic_params().clone()
	params << mut_optional_arg_marker
	params << flat.source_type_name(view_type)
	t.set_node_generic_params(int(addr), params)
}

// mut_optional_arg_view_type returns the `?T` that a `&?&T` argument built for a `mut a ?T`
// parameter stands for (mark_mut_optional_arg).
fn mut_optional_arg_view_type(node flat.Node) ?u32 {
	if node.kind != .prefix || node.op != .amp || node.payload == 0 {
		return none
	}
	params := node.generic_params()
	idx := params.index(mut_optional_arg_marker)
	if idx < 0 || idx + 1 >= params.len {
		return none
	}
	return flat.source_type_id(params[idx + 1])
}

// heaped_amp_local_arg_name returns the name of the local `arg_id` names when the local was
// moved to the heap (its value is a pointer to it), or ''.
fn (t &Transformer) heaped_amp_local_arg_name(arg_id flat.NodeId) string {
	mut source_id := arg_id
	for t.a.nodes[int(source_id)].kind == .paren && t.a.nodes[int(source_id)].children_count == 1 {
		source_id = t.a.child(&t.a.nodes[int(source_id)], 0)
	}
	source := t.a.nodes[int(source_id)]
	if source.kind != .ident || !t.heaped_amp_locals[source.value] {
		return ''
	}
	return source.value
}

// unwrapped_mut_optional_view_param returns the view parameter `arg_id` names, if any.
fn (t &Transformer) unwrapped_mut_optional_view_param(arg_id flat.NodeId) ?MutOptionalViewParam {
	if int(arg_id) < 0 {
		return none
	}
	mut source_id := arg_id
	for t.a.nodes[int(source_id)].kind == .paren && t.a.nodes[int(source_id)].children_count == 1 {
		source_id = t.a.child(&t.a.nodes[int(source_id)], 0)
	}
	source := t.a.nodes[int(source_id)]
	if source.kind != .ident || t.has_smartcast(source.value) {
		return none
	}
	return t.mut_optional_view_param(source.value)
}

// type_is_value_or_reference_of reports whether `type_id` is `clean_type`, or a reference to
// it, after aliases are resolved.
fn (t &Transformer) type_is_value_or_reference_of(type_id u32, clean_type u32) bool {
	if t.normalize_type_alias(type_id) == clean_type {
		return true
	}
	typ := flat.source_type_name(type_id)
	return typ.starts_with('&')
		&& t.normalize_type_alias(flat.source_type_id(typ[1..])) == clean_type
}

fn (mut t Transformer) finish_mut_optional_value_call(call_id flat.NodeId, typ_id u32, writebacks []flat.NodeId) flat.NodeId {
	if writebacks.len == 0 {
		return call_id
	}
	if typ_id == u32(0) || typ_id == flat.source_type_void {
		t.pending_stmts << t.make_expr_stmt(call_id)
		t.pending_stmts << writebacks
		return t.make_int_literal(0)
	}
	result_name := t.new_temp('mut_optional_result')
	t.set_var_type(result_name, typ_id)
	t.pending_stmts << t.make_decl_assign_typed(result_name, call_id, typ_id)
	t.pending_stmts << writebacks
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), typ_id)
	return result
}

fn (mut t Transformer) finish_immediate_closure_call(call_id flat.NodeId, closure_name string, typ_id u32, capture_may_escape bool) flat.NodeId {
	if closure_name == '' {
		return call_id
	}
	if capture_may_escape || t.immediate_closure_result_may_alias_capture(typ_id) {
		t.pending_stmts << t.make_local_closure_cleanup_defer(closure_name)
		return call_id
	}
	if typ_id == u32(0) || typ_id == flat.source_type_void {
		t.pending_stmts << t.make_expr_stmt(call_id)
		t.pending_stmts << t.make_local_closure_destroy_stmt(closure_name)
		return t.make_int_literal(0)
	}
	result_name := t.new_temp('immediate_closure_result')
	t.set_var_type(result_name, typ_id)
	t.pending_stmts << t.make_decl_assign_typed(result_name, call_id, typ_id)
	t.pending_stmts << t.make_local_closure_destroy_stmt(closure_name)
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), typ_id)
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

fn (t &Transformer) immediate_closure_result_may_alias_capture(type_name_id u32) bool {
	if type_name_id == u32(0) {
		return false
	}
	clean_source_id := t.normalize_type_alias(type_name_id)
	clean := flat.source_type_name(clean_source_id)
	if clean == 'thread' || clean.starts_with('thread ') {
		return true
	}
	if isnil(t.tc) {
		return clean_source_id == flat.source_type_string || clean.starts_with('!') || clean.starts_with('?')
			|| clean.starts_with('&') || clean.starts_with('[]') || clean.starts_with('map[')
			|| clean.starts_with('chan ') || clean in ['voidptr', 'byteptr', 'charptr']
	}
	mut seen := map[u32]bool{}
	return t.closure_result_type_may_alias_capture(t.tc.parse_type(type_name_id), mut seen)
}

fn (t &Transformer) closure_result_type_may_alias_capture(typ types.Type, mut seen map[u32]bool) bool {
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
			if typ.source_type_id() in seen {
				false
			} else {
				seen[typ.source_type_id()] = true
				mut may_alias := false
				for field in t.tc.struct_fields_for_type(typ.source_type_id()) {
					if t.closure_result_type_may_alias_capture(field.typ, mut seen) {
						may_alias = true
						break
					}
				}
				may_alias
			}
		}
		types.SumType {
			if typ.source_type_id() in seen {
				false
			} else {
				seen[typ.source_type_id()] = true
				mut may_alias := false
				for variant_source_id in t.concrete_sum_variants_for_candidate(typ.source_type_id()) {
					if t.closure_result_type_may_alias_capture(t.tc.parse_type(variant_source_id), mut seen) {
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

fn (t &Transformer) call_param_type_names(params []types.Type) []u32 {
	// A type name renders main-module types bare. These names are parsed again in
	// the current module, so inside an imported generic specialization a main `Cell`
	// must be locked to `main.Cell`, or it rebinds to the module's own `Cell`.
	lock_main := t.cur_module !in ['', 'main', 'builtin']
	mut names := []u32{cap: params.len}
	for param in params {
		name := if t.memo_call_param_type_names && !isnil(t.tc) {
			t.tc.type_name(param)
		} else {
			param.source_type_id()
		}
		if lock_main {
			names << t.lock_colliding_main_generic_type_text(name, t.cur_module)
		} else {
			names << name
		}
	}
	return names
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
	variadic_array_type_source_id := t.semantic_type_name(variadic_type)
	variadic_array_type_source_id_2 := variadic_array_type_source_id

	spread_type0_source_id := t.node_type(spread_id)
	spread_type0_source_id_2 := spread_type0_source_id

	spread_type := (if spread_type0_source_id != 0 {
		spread_type0_source_id_2
	} else {
		variadic_array_type_source_id_2
	})
	spread_expr := t.stable_transformed_expr_for_reuse(t.transform_call_arg_for_param(spread_id, spread_type), spread_type, 'varargs_spread')
	mut args := []flat.NodeId{cap: variadic_idx - param_idx + 1}
	for fixed_idx in param_idx .. variadic_idx {
		elem := t.make_index(spread_expr, t.make_int_literal(fixed_idx - param_idx), t.semantic_type_name(params[fixed_idx]))
		args << elem
	}
	tail_start := variadic_idx - param_idx
	spread_elem_type := flat.source_type_name(t.array_elem_type(t.normalize_type_alias(spread_type)))
	if spread_elem_type == flat.source_type_name(t.normalize_type_alias(variadic_array_type_source_id)) {
		args << t.make_index(spread_expr, t.make_int_literal(tail_start), variadic_array_type_source_id)
	} else {
		args << t.make_range_index(spread_expr, t.make_int_literal(tail_start), flat.empty_node, variadic_array_type_source_id)
	}
	return args
}

fn (mut t Transformer) transform_call_arg_for_named_param(arg_id flat.NodeId, param_type_id u32, call_name string) flat.NodeId {
	// `free` is declared with a `voidptr` parameter, but aligned allocation metadata
	// belongs to the argument's concrete pointer type and is needed by C codegen.
	if call_name in ['free', 'builtin.free', 'C.free'] && !isnil(t.tc)
		&& escape_type_is_pointer(t.tc.resolve_type(arg_id)) {
		return t.transform_expr(arg_id)
	}
	// C declarations often use `voidptr` as an intentionally opaque placeholder
	// for a native by-value type whose full declaration lives in an inserted C or
	// Objective-C source. Match the legacy backend by leaving such C arguments in
	// value form. An explicit opaque-pointer cast also owns its pointer depth even
	// when an included header refines the parameter to `void**` (for example,
	// `CreatePipe(voidptr(&handle))`).
	if call_name.starts_with('C.') {
		arg_node := t.a.nodes[int(arg_id)]
		if t.transform_param_type_is_void_pointer(param_type_id)
			|| (arg_node.kind == .cast_expr
				&& t.transform_param_type_is_void_pointer(t.node_type(arg_id))) {
			return t.transform_expr(arg_id)
		}
	}
	return t.transform_call_arg_for_param(arg_id, param_type_id)
}

fn (mut t Transformer) transform_variadic_spread_arg_for_param(spread_id flat.NodeId, variadic_type types.Array, param_type_id u32) flat.NodeId {
	mut value := flat.empty_node
	if !isnil(t.tc) && int(spread_id) >= 0 {
		actual := types.unwrap_pointer(t.tc.resolve_type(spread_id))
		if actual is types.Array {
			expected := types.Type(variadic_type)
			actual_elem_source_id := t.normalize_type_alias(t.semantic_type_name(actual.elem_type))
			expected_elem_source_id := t.normalize_type_alias(t.semantic_type_name(variadic_type.elem_type))
			if actual_elem_source_id != expected_elem_source_id
				&& t.sum_target_accepts_variant_type(expected_elem_source_id, actual_elem_source_id) {
				value = t.convert_forwarded_array_to_dynamic(spread_id, actual, actual.elem_type, expected, variadic_type.elem_type, false)
			}
		}
	}
	if int(value) < 0 {
		value = t.transform_call_arg_for_param(spread_id, param_type_id)
	}
	// Preserve the packed tail when interpolation transforms this call again.
	t.packed_variadic_tail_types[int(value)] = t.semantic_type_name(variadic_type)
	return value
}

fn (mut t Transformer) make_range_index(base flat.NodeId, start_id flat.NodeId, end_id flat.NodeId, typ_id u32) flat.NodeId {
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
		kind:           .index
		value:          'range'
		children_start: start
		children_count: flat.child_count(children.len)
		typ:            typ_id
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
	raw_type := flat.source_type_name(t.raw_const_type_name_for_expr(callee_id) or { return none })
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
		result = t.make_call_typed('os.join_path_single', [result, arg], flat.source_type_string)
	}
	return result
}

// transform_params_struct_call_arg transforms transform params struct call arg data for transform.
fn (mut t Transformer) transform_params_struct_call_arg(node flat.Node, field_start int, param_type_id u32) ?flat.NodeId {
	struct_type_source_id := t.params_struct_type_name(param_type_id) or { return none }

	return t.transform_trailing_field_init_struct_arg(node, field_start, struct_type_source_id, param_type_id)
}

fn (mut t Transformer) transform_struct_call_arg(node flat.Node, field_start int, param_type_id u32) ?flat.NodeId {
	struct_type_source_id := t.struct_arg_type_name(param_type_id) or {
		return none
	}

	return t.transform_trailing_field_init_struct_arg(node, field_start, struct_type_source_id, param_type_id)
}

fn (mut t Transformer) transform_trailing_field_init_struct_arg(node flat.Node, field_start int, struct_type_id u32, target_type_id u32) ?flat.NodeId {
	target_type := flat.source_type_name(target_type_id)
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
	normalized_target_source_id := t.normalize_type_alias_chain(flat.source_type_id(trimmed_transform_text(target_type)))
	normalized_target := flat.source_type_name(normalized_target_source_id)
	if normalized_target.starts_with('&&') {
		return none
	}
	literal_type := (if normalized_target.starts_with('&') {
		t.normalize_type_alias(flat.source_type_id(normalized_target[1..]))
	} else {
		struct_type_id
	})
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	struct_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		value:          flat.source_type_name(literal_type)
		typ:            literal_type
	})
	if normalized_target.starts_with('&') {
		amp_id := t.make_prefix(.amp, struct_id)
		return t.transform_amp_struct_init_for_type(amp_id, t.a.nodes[int(amp_id)], normalized_target_source_id)
	}
	return t.transform_struct_fields(struct_id, t.a.nodes[int(struct_id)])
}

fn (t &Transformer) struct_arg_type_name(param_type_id u32) ?u32 {
	param_type := flat.source_type_name(param_type_id)
	if param_type_id == u32(0) {
		return none
	}
	mut typ := t.normalize_type_alias_chain(flat.source_type_id(trimmed_transform_text(param_type)))
	if flat.source_type_shape(typ).has(.pointer) {
		typ = flat.source_type_id(flat.source_type_name(typ)[1..])
	}
	if flat.source_type_shape(typ).has(.pointer) {
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
fn (t &Transformer) params_struct_type_name(param_type_id u32) ?u32 {
	param_type := flat.source_type_name(param_type_id)
	if param_type_id == u32(0) {
		return none
	}
	mut typ := param_type
	if typ.starts_with('&') {
		typ = typ[1..]
	}
	if !isnil(t.tc) && typ in t.tc.params_structs {
		return flat.source_type_id(typ)
	}
	if info := t.lookup_struct_info(flat.source_type_id(typ)) {
		if info.is_params {
			return flat.source_type_id(typ)
		}
	}
	normalized_source_id := t.normalize_type_alias(flat.source_type_id(typ))
	normalized := flat.source_type_name(normalized_source_id)
	if normalized != typ {
		if !isnil(t.tc) && normalized in t.tc.params_structs {
			return normalized_source_id
		}
		if info := t.lookup_struct_info(normalized_source_id) {
			if info.is_params {
				return normalized_source_id
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
					if t.fn_ret_types.contains(qname) || qname in t.tc.fn_ret_types {
						return qname
					}
					// The lowered spelling is only needed when the plain one is not declared.
					lowered_qname := c_name(qname)
					if t.fn_ret_types.contains(lowered_qname) || lowered_qname in t.tc.fn_ret_types {
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
	base_type_source_id := t.normalize_type_alias(t.trim_pointer_type(t.lvalue_type(base_id)))

	if t.receiver_selector_is_fn_field(base_type_source_id, fn_node.value) {
		// Function-valued fields have no implicit receiver parameter.
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
	if call_name == '' || call_name == method_name || call_name == c_name(method_name) {
		return 1
	}
	return 0
}

fn (mut t Transformer) call_param_offset_for_node(call_name string, node flat.Node, params []types.Type) int {
	mut param_offset := t.call_param_offset(call_name, node, params)
	if param_offset != 0 || call_name == '' || params.len == 0 {
		return param_offset + t.implicit_veb_ctx_param_offset(call_name, node, params)
	}
	selector_id := t.call_selector_callee_id(node) or { return param_offset }
	selector := t.a.nodes[int(selector_id)]
	if selector.children_count == 0 {
		return param_offset
	}
	base_id := t.a.child(&selector, 0)
	base_type_source_id := t.normalize_type_alias(t.trim_pointer_type(t.lvalue_type(base_id)))

	if t.receiver_selector_is_fn_field(base_type_source_id, selector.value) {
		return 0
	}

	base := t.a.nodes[int(base_id)]
	first := types.unwrap_all_pointers(params[0])
	base_is_lexical_module := t.selector_is_lexical_module_call(base_id, selector.value, call_name)
	base_is_value := base.kind != .ident || (t.raw_var_type(base.value)) != 0
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
	return param_offset + t.implicit_veb_ctx_param_offset(call_name, node, params)
}

fn (mut t Transformer) implicit_veb_ctx_param_offset(call_name string, node flat.Node, params []types.Type) int {
	abi_params := t.implicit_veb_call_param_types(call_name) or { return 0 }
	if t.receiver_call_uses_comptime_method_selector(node) {
		if params.len < 2 {
			return 0
		}
		args, has_spread := t.logical_call_arg_ids(node)
		mut module_name := t.cur_module
		if !isnil(t.tc) {
			module_name = t.tc.fn_type_modules[call_name] or { t.cur_module }
		}
		if has_spread {
			if args.len > 0 && t.call_arg_matches_abi_type(args[0], params[1], module_name) {
				return 0
			}
			return 1
		}
		declared_count := params.len - 2
		if args.len > declared_count {
			return 0
		}
		// Follow the checker's arity-based omission rule. In the ambiguous range,
		// prefer binding arguments to declared route params, including interfaces
		// implemented by the concrete Context type.
		if t.reflected_veb_route_args_match(args, params, 0, module_name) {
			return 1
		}
		// If that binding is invalid, the first argument may instead be the explicit
		// ctx when every omitted trailing route parameter has a default shape.
		if args.len > 0 && t.call_arg_matches_abi_type(args[0], params[1], module_name)
			&& args.len - 1 >= t.implicit_veb_min_required_route_args(call_name, params)
			&& t.reflected_veb_route_args_match(args, params, 1, module_name) {
			return 0
		}
		return 1
	}
	if params.len != abi_params.len {
		return 0
	}
	return 1
}

fn (t &Transformer) logical_call_arg_ids(node flat.Node) ([]flat.NodeId, bool) {
	mut args := []flat.NodeId{cap: int(node.children_count)}
	mut has_spread := false
	mut added_field_group := false
	for i in 1 .. node.children_count {
		arg_id := t.a.child(&node, i)
		if t.call_arg_is_spread(arg_id) {
			has_spread = true
		}
		if t.a.node(arg_id).kind == .field_init {
			if added_field_group {
				continue
			}
			added_field_group = true
		}
		args << arg_id
	}
	return args, has_spread
}

fn (mut t Transformer) reflected_veb_route_args_match(args []flat.NodeId, params []types.Type, arg_start int, module_name string) bool {
	route_count := args.len - arg_start
	if route_count < 0 || route_count > params.len - 2 {
		return false
	}
	for route_idx in 0 .. route_count {
		arg_id := args[arg_start + route_idx]
		if t.a.node(arg_id).kind == .field_init {
			continue
		}
		mut actual := t.specialized_expr_type_name(arg_id)
		if actual == flat.source_type_unknown {
			actual = t.a.node(arg_id).typ
		}
		mut expected := t.semantic_type_name(params[route_idx + 2])
		if module_name in ['', 'main'] {
			actual = type_text_without_main_locks(actual)
			expected = type_text_without_main_locks(expected)
		}
		if !t.resolved_receiver_arg_compatible(arg_id, actual, expected) {
			return false
		}
	}
	return true
}

fn (t &Transformer) implicit_veb_min_required_route_args(call_name string, params []types.Type) int {
	mut count := params.len - 2
	if count > 0 && t.call_is_variadic(call_name) {
		count--
	}
	for count > 0 {
		param := params[count + 1]
		if param is types.OptionType {
			count--
			continue
		}
		if _ := t.params_struct_type_name(t.semantic_type_name(param)) {
			count--
			continue
		}
		break
	}
	return count
}

fn (t &Transformer) call_arg_matches_abi_type(arg_id flat.NodeId, expected types.Type, expected_module string) bool {
	if int(arg_id) < 0 {
		return false
	}
	arg := t.a.node(arg_id)
	expected_name_source_id := t.semantic_type_name(expected)
	expected_type := type_text_without_main_locks(t.normalize_type_in_module(expected_name_source_id, expected_module))
	mut candidates := [t.node_type(arg_id), arg.typ]
	if arg.kind == .ident {
		candidates << t.raw_var_type(arg.value)
		candidates << t.var_type(arg.value)
	}
	for candidate in candidates {
		if candidate == 0 {
			continue
		}
		arg_type := (if arg.is_mut && !flat.source_type_shape(candidate).has(.pointer)
			&& !flat.source_type_shape(candidate).has(.mutable_type) {
			flat.source_type_id('&${flat.source_type_name(candidate)}')
		} else {
			candidate
		})
		// ABI metadata stores main-module types bare, while an imported generic
		// specialization locks the same type as `main.X` to avoid local collisions.
		if expected_module in ['', 'main'] && flat.source_type_name(arg_type).contains('main.')
			&& type_text_without_main_locks(arg_type) == type_text_without_main_locks(expected_name_source_id) {
			return true
		}
		actual := type_text_without_main_locks(t.normalize_type_in_module(arg_type, t.cur_module))
		if actual != 0 && actual == expected_type {
			return true
		}
	}
	return false
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
	if params.len == 0 || method == '' || call_name == ''
		|| !call_name.ends_with('.${method}') {
		return false
	}
	first_type := flat.source_type_name(t.normalize_type_alias(t.semantic_type_name(params[0])))
	clean_first := if first_type.starts_with('&') { first_type[1..] } else { first_type }
	if receiver_param_matches_method_name(clean_first, call_name) {
		return true
	}
	receiver := call_name.all_before_last('.')
	clean_receiver := if receiver.starts_with('&') { receiver[1..] } else { receiver }
	if receiver_param_types_match(clean_first, clean_receiver)
		|| t.normalize_type_alias(flat.source_type_id(clean_first)) == t.normalize_type_alias(flat.source_type_id(clean_receiver))
		|| short_name_view(clean_first) == short_name_view(clean_receiver) {
		return true
	}
	return false
}

fn (mut t Transformer) transform_method_callee_receiver_for_param(callee_id flat.NodeId, param_type_id u32) ?flat.NodeId {
	if int(callee_id) < 0 || param_type_id == u32(0) {
		return none
	}
	callee := t.a.nodes[int(callee_id)]
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	base_id := t.a.child(&callee, 0)
	base_type_source_id := t.node_type(base_id)

	if !t.is_interface_type(param_type_id) || !t.is_interface_type(base_type_source_id)
		|| t.resolve_interface_type_name(param_type_id) == t.resolve_interface_type_name(base_type_source_id) {
		return none
	}
	new_base := t.transform_expr_for_type(base_id, param_type_id)
	start := t.a.children.len
	t.a.children << new_base
	return t.a.add_node(flat.Node{
		kind:           .selector
		children_start: start
		children_count: 1
		pos:            callee.pos
		value:          callee.value
		typ:            callee.typ
	})
}

// static_assoc_fn_name returns the name of the static associated function a selector
// call resolves to, if the base names a type (`Type.fn` or `module.Type.fn`) and that
// function exists. Such calls take no receiver, so the base must not be prepended as an
// argument. Returns none for ordinary method calls (base is a value).
fn (t &Transformer) static_assoc_fn_name(base_id flat.NodeId, method string) ?string {
	if method == '' {
		return none
	}
	// Most selector calls name instance methods: skip building candidate types
	// when no static method anywhere has this name.
	if t.static_method_names_ready && method !in t.static_method_names {
		return none
	}
	base := t.a.nodes[int(base_id)]
	if base.kind == .ident {
		if base.value == 'C' || t.is_import_alias_ident(base_id) {
			return none
		}
		if t.static_assoc_ident_is_value(base_id) {
			return none
		}
		base_type := flat.source_type_name(t.node_type(base_id))
		clean_base_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
		if clean_base_type.len > 0 && clean_base_type != 'unknown' && clean_base_type != 'void'
			&& base.value != clean_base_type && base.value != clean_base_type.all_after_last('.') {
			return none
		}
		for type_name in t.static_assoc_type_candidates(flat.source_type_id(base.value)) {
			name := flat.encode_static_type_method_name(flat.source_type_name(type_name), method)
			if t.is_known_fn_name(name) {
				return name
			}
		}
	} else if base.kind == .selector && base.children_count > 0 {
		inner_id := t.a.child(&base, 0)
		inner := t.a.node(inner_id)
		if inner.kind == .ident {
			if t.static_assoc_ident_is_value(inner_id) {
				return none
			}
			type_ident := '${inner.value}.${base.value}'
			for type_name in t.static_assoc_type_candidates(flat.source_type_id(type_ident)) {
				name := flat.encode_static_type_method_name(flat.source_type_name(type_name), method)
				if t.is_known_fn_name(name) {
					return name
				}
			}
		}
	}
	return none
}

// static_assoc_ident_is_value distinguishes runtime roots from type namespaces,
// including file-scope bindings that are absent from the local variable table.
fn (t &Transformer) static_assoc_ident_is_value(id flat.NodeId) bool {
	name := t.a.node(id).value
	if (t.var_type(name)) != 0 || t.current_module_global_type(name) != none {
		return true
	}
	const_name := if t.cur_module in ['', 'main', 'builtin'] {
		name
	} else {
		'${t.cur_module}.${name}'
	}
	if !isnil(t.tc) && const_name in t.tc.const_types {
		return true
	}
	if t.is_import_alias_ident(id) {
		return false
	}
	return t.imported_global_name(name) != none
		|| t.const_type_key_in_context(name, t.cur_module, t.cur_file) != none
}

// static_fn_value_name returns the static type method that a `Type.method` or
// `mod.Type.method` selector names when it is used as a function value, not called.
// Generic specializations do not carry the checker's resolution for their cloned
// selectors, so fall back to looking the static method up from the base.
fn (t &Transformer) static_fn_value_name(id flat.NodeId, node flat.Node) ?string {
	if t.in_call_callee || node.children_count != 1 || isnil(t.tc) {
		return none
	}
	if t.static_method_names_ready && node.value !in t.static_method_names {
		return none
	}
	if resolved := t.tc.resolved_fn_value_name(id) {
		flat.decode_static_type_method_name(resolved) or { return none }
		return resolved
	}
	name := t.static_assoc_fn_name(t.a.child(&node, 0), node.value) or { return none }
	receiver, method := flat.decode_static_type_method_name(name) or { return none }
	// Enum fields take precedence over a same-named static method.
	if t.enum_method_name_shadows_field('${receiver}.${method}') {
		return none
	}
	return name
}

// lower_static_fn_value replaces a static type method value selector with an ident
// naming the function, the same form other function values take. Emitting the
// selector as is would produce `Type.method` in C, where `Type` is not a value.
fn (mut t Transformer) lower_static_fn_value(id flat.NodeId, node flat.Node, name string) flat.NodeId {
	mut typ := u32(0)
	if checked := t.tc.expr_type(id) {
		typ = fn_value_type_name_from_type(checked) or { u32(0) }
	}
	if typ == 0 {
		if params := t.tc.fn_param_types[name] {
			ret := t.tc.fn_ret_types[name] or { types.Type(types.void_) }
			typ = fn_literal_value_type_text(params, ret.source_type_id())
		}
	}
	ident := t.a.add_node(flat.Node{
		kind:  .ident
		pos:   node.pos
		value: name
		typ:   typ
	})
	t.set_resolved_fn_value_entry(int(ident), name)
	// Specialized generic bodies are transformed after the initial markused pass.
	t.mark_fn_used_name(name)
	return ident
}

// build_static_method_names records the method part of every static type
// method (`fn Type.method()`) the checker registered. Functions that transform
// adds later, such as generic specializations, reuse the method name of a source
// declaration, so the set stays complete for static_assoc_fn_name.
fn (mut t Transformer) build_static_method_names() {
	t.static_method_names = map[string]bool{}
	for name, _ in t.fn_ret_types.ids {
		t.add_static_method_name(name)
	}
	if !isnil(t.tc) {
		for name, _ in t.tc.fn_ret_types {
			t.add_static_method_name(name)
		}
		for name, _ in t.tc.fn_param_types {
			t.add_static_method_name(name)
		}
	}
	t.static_method_names_ready = true
}

fn (mut t Transformer) add_static_method_name(name string) {
	_, method := flat.decode_static_type_method_name(name) or { return }
	t.static_method_names[method] = true
}

fn (t &Transformer) selector_is_lexical_module_call(base_id flat.NodeId, method string, call_name string) bool {
	if int(base_id) < 0 || int(base_id) >= t.a.nodes.len || isnil(t.tc) {
		return false
	}
	base := t.a.node(base_id)
	if base.kind != .ident || base.value.len == 0 || method == '' {
		return false
	}
	file := t.a.source_files[base.pos.id] or { return false }
	module_name := t.file_import_module(file.name, base.value) or { return false }
	return qualified_name_equals(call_name, module_name, method)
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
	if _ := t.file_import_module(t.cur_file, node.value) {
		return (t.raw_var_type(node.value)) == 0
	}
	if node.value !in t.tc.imports {
		return false
	}
	// A local or receiver can shadow an import alias. Generic clones may no longer
	// retain an expression type for the identifier, but their binding table still
	// records the value type.
	if (t.raw_var_type(node.value)) != 0 {
		return false
	}
	if typ := t.tc.expr_type(id) {
		name_source_id := t.semantic_type_name(typ)

		if name_source_id != 0 && name_source_id != flat.source_type_unknown && name_source_id != flat.source_type_void {
			return false
		}
	}
	return true
}

fn (t &Transformer) static_assoc_type_candidates(type_id u32) []u32 {
	if type_id == 0 {
		return []u32{}
	}
	mut candidates := []u32{}
	if imported := t.selective_import_type_name_for_file(t.cur_file, type_id) {
		t.add_static_assoc_type_candidate(mut candidates, imported)
	}
	t.add_static_assoc_type_candidate(mut candidates, type_id)
	type_ident := flat.source_type_name(type_id)
	if !flat.source_type_shape(type_id).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		t.add_static_assoc_type_candidate(mut candidates, flat.source_type_id('${t.cur_module}.${type_ident}'))
	}
	if !isnil(t.tc) {
		qname := flat.source_type_id(t.tc.qualify_name(type_ident))
		t.add_static_assoc_type_candidate(mut candidates, qname)
	}
	mut result := []u32{}
	for candidate in candidates {
		if t.is_static_assoc_type_name(candidate) && candidate !in result {
			result << candidate
		}
	}
	return result
}

fn (t &Transformer) add_static_assoc_type_candidate(mut candidates []u32, name_id u32) {
	if name_id == 0 {
		return
	}
	if name_id !in candidates {
		candidates << name_id
	}
	name := flat.source_type_name(name_id)
	if name.count('.') >= 2 {
		module_path := name.all_before_last('.')
		declared_module_name := module_path.all_after_last('.')
		short_qualified := flat.source_type_id('${declared_module_name}.${name.all_after_last('.')}')
		if short_qualified !in candidates {
			candidates << short_qualified
		}
	}
	if isnil(t.tc) {
		return
	}
	if target_source_id := t.tc.type_aliases.type_id(name) {
		if target_source_id !in candidates {
			candidates << target_source_id
		}
	}
}

fn (t &Transformer) is_static_assoc_type_name(type_name_id u32) bool {
	type_name := flat.source_type_name(type_name_id)
	if type_name in t.structs || type_name in t.enum_types || t.sum_types.contains(type_name) {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return type_name in t.tc.structs || type_name in t.tc.enum_names || t.tc.sum_types.contains(type_name)
		|| type_name in t.tc.interface_names || t.tc.type_aliases.contains(type_name)
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
	ret_type := (if generic_args.len > 0 && node.typ != 0 {
		node.typ
	} else {
		t.receiver_method_return_type(static_fn, node.typ)
	})
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
	if call_name == '' || isnil(t.tc) {
		return []types.Type{}
	}
	if params := t.implicit_veb_call_param_types(call_name) {
		return params
	}
	if params := t.call_param_types_from_decl(call_name) {
		return params
	}
	params := t.tc.fn_param_types[call_name] or { return []types.Type{} }
	return params
}

fn (mut t Transformer) call_param_types_for_node(call_name string, node flat.Node) []types.Type {
	if params := t.implicit_veb_call_param_types(call_name) {
		return params
	}
	if params := t.call_param_types_from_decl(call_name) {
		return params
	}
	if node.children_count > 0 {
		fn_id := t.a.child(&node, 0)
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident && (t.var_type(fn_node.value)) != 0 {
			if fn_type := t.call_callee_fn_type(fn_id) {
				return fn_type.params.map(*it.typ)
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
			if flat.source_type_shape(base_type).has(.pointer) {
				base_type = flat.source_type_id(flat.source_type_name(base_type)[1..])
			}
			for candidate in ['${flat.source_type_name(base_type)}.${fn_node.value}',
				'${flat.source_type_name(t.normalize_type_in_module(base_type, t.cur_module))}.${fn_node.value}'] {
				if params := t.call_param_types_from_decl(candidate) {
					return params
				}
			}
			if params := t.fn_field_mut_optional_param_types(base_type, fn_node.value) {
				return params
			}
		}
		if (call_name == '' || call_name !in t.tc.fn_ret_types)
			&& call_name !in t.tc.fn_param_types {
			if fn_type := t.call_callee_fn_type(fn_id) {
				return fn_type.params.map(*it.typ)
			}
		}
	}
	return t.call_param_types(call_name)
}

fn (t &Transformer) implicit_veb_call_param_types(call_name string) ?[]types.Type {
	if call_name == '' || isnil(t.tc) || t.tc.fn_implicit_veb_ctx.len == 0 {
		return none
	}
	for name in [call_name, c_name(call_name)] {
		if t.tc.fn_implicit_veb_ctx[name] {
			if params := t.tc.fn_param_types[name] {
				return params
			}
		}
	}
	return none
}

fn (t &Transformer) call_callee_fn_type(fn_id flat.NodeId) ?types.FnType {
	if int(fn_id) < 0 || isnil(t.tc) {
		return none
	}
	node := t.a.nodes[int(fn_id)]
	if node.kind == .ident {
		for local_type in [flat.source_type_name(t.raw_var_type(node.value)),
			flat.source_type_name(t.var_type(node.value))] {
			if local_type.len == 0 {
				continue
			}
			if fn_type := transform_fn_type(t.tc.parse_type(flat.source_type_id(local_type))) {
				return fn_type
			}
		}
	}
	if typ := t.tc.expr_type(fn_id) {
		if fn_type := transform_fn_type(typ) {
			return fn_type
		}
	}
	if node.typ != 0 {
		if fn_type := transform_fn_type(t.tc.parse_type(node.typ)) {
			return fn_type
		}
	}
	if node.kind == .index {
		// `fns[i](...)`: the checker's local scope is gone here, so type the
		// element from the transformer's own view of the indexed container.
		elem_type_source_id := t.node_type(fn_id)

		if elem_type_source_id != 0 && elem_type_source_id != flat.source_type_unknown {
			if fn_type := transform_fn_type(t.tc.parse_type(elem_type_source_id)) {
				return fn_type
			}
		}
	}
	return transform_fn_type(t.tc.resolve_type(fn_id))
}

// Reflected method values acquire their concrete signature while the generic
// body is unrolled, after the checker has visited the template.
fn (mut t Transformer) validate_specialized_fn_value_mut_args(node flat.Node) {
	if !t.validating_generic_spec || isnil(t.tc) || t.tc.disable_explicit_mutability
		|| node.children_count < 2 {
		return
	}
	callee_expr_id := t.a.child(&node, 0)
	callee_id := t.unwrap_parens(callee_expr_id)
	callee := t.a.node(callee_id)
	if callee.kind == .ident {
		// A parenthesized direct symbol is an explicit function-value call. Ordinary
		// method lowering can insert its receiver without a source `mut` argument.
		if (t.raw_var_type(callee.value)) == 0 && callee_expr_id == callee_id {
			return
		}
	} else if callee.kind == .index {
		if !t.index_callee_is_value_index(callee) {
			return
		}
	} else if callee.kind == .selector {
		field_type_source_id := t.raw_selector_field_type(callee_id) or { return }

		if _ := transform_fn_type(t.tc.parse_type(field_type_source_id)) {
		} else {
			return
		}
	}
	fn_type := t.call_callee_fn_type(callee_id) or {
		// Carrier expressions cloned during specialization can outlive the
		// checker scope; recover their signature from the transformer's view.
		transform_fn_type(t.tc.parse_type(t.node_type(callee_id))) or { return }
	}
	for index, parameter in fn_type.params {
		if !parameter.is_mut || index + 1 >= node.children_count {
			continue
		}
		arg := t.a.child_node(&node, index + 1)
		if !arg.is_mut {
			target := if callee.kind == .ident {
				'function `${callee.value}`'
			} else {
				'function value'
			}
			t.record_monomorph_error('${target} parameter ${index + 1} is `mut`, so use a `mut` argument instead')
		}
	}
}

fn (mut t Transformer) ensure_private_call_param_types_decl_cache() {
	if t.call_param_types_decl_shared {
		t.call_param_types_decl_cache = t.call_param_types_decl_cache.clone()
		t.call_param_types_decl_index = t.call_param_types_decl_index.clone()
		t.call_param_types_decl_shared = false
	}
}

fn (mut t Transformer) call_param_types_from_decl(call_name string) ?[]types.Type {
	if call_name == '' || isnil(t.tc) {
		return none
	}
	t.ensure_call_param_types_decl_index()
	decl := t.call_param_types_decl_index[call_name] or { return none }
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
		if param_type is types.Unknown || (param_type is types.Void && child.typ != flat.source_type_void) {
			return none
		}
		if (child.is_mut || flat.source_type_shape(child.typ).has(.mutable_type)
			|| flat.source_type_shape(child.typ).has(.pointer)) && param_type !is types.Pointer {
			param_type = types.Type(types.Pointer{
				base_type: types.new_type_reference(types.Type(param_type))
			})
		}
		// Named calls use the same mutable optional slot as checked fn values.
		params << if child.is_mut {
			types.mut_param_semantic_type(param_type)
		} else {
			param_type
		}
	}
	t.ensure_private_call_param_types_decl_cache()
	t.call_param_types_decl_cache[decl.idx] = params.clone()
	return params
}

fn (mut t Transformer) ensure_call_param_types_decl_index() {
	if t.call_param_types_index_ready {
		return
	}
	t.ensure_private_call_param_types_decl_cache()
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
			// Context indexes can change here, but the AST arena cannot.
			node := unsafe { &t.a.nodes[i] }
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
	// The nodes the top-level index covers are read through it; the declarations
	// appended behind it, as collect_types reads them.
	indexed_count, indexed_nodes := t.top_level_scan_prefix()
	appended, streamed := t.appended_declaration_scan(indexed_nodes)
	for scan_idx in 0 .. indexed_count + appended.len + streamed {
		i := if scan_idx < indexed_count {
			int(t.tc.top_level_idx[scan_idx])
		} else if scan_idx - indexed_count < appended.len {
			appended[scan_idx - indexed_count]
		} else {
			indexed_nodes + scan_idx - indexed_count - appended.len
		}
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[i] }
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
	t.call_param_types_decl_shared = true
}

fn (mut t Transformer) add_call_param_types_decl_key(key string, idx int, file string, module_name string) {
	if key == '' {
		return
	}
	// A later generic specialization can extend the declaration index between
	// parallel batches. Make the next snapshot include its signature too.
	t.call_param_types_prepared = false
	t.ensure_private_call_param_types_decl_cache()
	if key !in t.call_param_types_decl_index {
		t.call_param_types_decl_index[key] = FnParamDeclRef{
			idx:    idx
			file:   file
			module: module_name
		}
	}
	cname := c_name(key)
	if cname != key && cname !in t.call_param_types_decl_index {
		t.call_param_types_decl_index[cname] = FnParamDeclRef{
			idx:    idx
			file:   file
			module: module_name
		}
	}
}

fn (mut t Transformer) parse_decl_param_type(typ_id u32, module_name string, file_name string) types.Type {
	scoped_source_id := t.decl_param_type_in_module(typ_id, module_name)

	old_file := t.tc.cur_file
	old_module := t.tc.cur_module
	t.tc.cur_file = file_name
	t.tc.cur_module = module_name
	parsed := t.tc.parse_type(scoped_source_id)
	t.tc.cur_file = old_file
	t.tc.cur_module = old_module
	return *parsed
}

fn (t &Transformer) decl_param_type_in_module(typ_id u32, module_name string) u32 {
	return t.decl_param_type_in_scope(typ_id, module_name, '')
}

fn (t &Transformer) decl_param_type_in_scope(typ_id u32, module_name string, file_name string) u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	clean_id := if flat.source_type_shape(typ_id).has(.padded) {
		flat.source_type_id(clean)
	} else {
		typ_id
	}
	if clean.len == 0 {
		return clean_id
	}
	if clean.starts_with('&') {
		return flat.source_type_id('&' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[1..]), module_name, file_name)))
	}
	if clean.starts_with('mut ') {
		return flat.source_type_id('mut ' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[4..]), module_name, file_name)))
	}
	if clean.starts_with('shared ') {
		return flat.source_type_id('shared ' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[7..]), module_name, file_name)))
	}
	if clean.starts_with('atomic ') {
		return flat.source_type_id('atomic ' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[7..]), module_name, file_name)))
	}
	if clean.starts_with('?') {
		return flat.source_type_id('?' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[1..]), module_name, file_name)))
	}
	if clean.starts_with('!') {
		return flat.source_type_id('!' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[1..]), module_name, file_name)))
	}
	if clean.starts_with('...') {
		return flat.source_type_id('...' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[3..]), module_name, file_name)))
	}
	if clean.starts_with('[]') {
		return flat.source_type_id('[]' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[2..]), module_name, file_name)))
	}
	if clean.starts_with('chan mut ') {
		return flat.source_type_id('chan mut ' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[9..]), module_name, file_name)))
	}
	if clean.starts_with('chan ') {
		return flat.source_type_id('chan ' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[5..]), module_name, file_name)))
	}
	if clean.starts_with('thread ') {
		return flat.source_type_id('thread ' + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[7..]), module_name, file_name)))
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[4..bracket_end]), module_name, file_name))
			value := flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[bracket_end + 1..]), module_name, file_name))
			return flat.source_type_id('map[${key}]${value}')
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return flat.source_type_id(clean[..bracket_end + 1] + flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(clean[bracket_end + 1..]), module_name, file_name)))
		}
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		if scoped_fn_type_source_id := t.decl_fn_type_in_scope(clean_id, module_name, file_name) {
			return scoped_fn_type_source_id
		}
		return clean_id
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		parts := split_generic_args(clean[1..clean.len - 1])
		if parts.len > 1 {
			mut scoped_parts := []string{cap: parts.len}
			for part in parts {
				scoped_parts << flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(part), module_name, file_name))
			}
			return flat.source_type_id('(' + scoped_parts.join(', ') + ')')
		}
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		mut scoped_args := []string{}
		for arg in args {
			scoped_args << flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(arg), module_name, file_name))
		}
		scoped_base := flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(base), module_name, file_name))
		return flat.source_type_id(scoped_base + '[' + scoped_args.join(', ') + ']')
	}
	if file_name != '' && !isnil(t.tc) {
		if selected := t.selective_signature_type_symbol(file_name, clean) {
			return flat.source_type_id(selected)
		}
		if clean.contains('.') {
			return t.tc.resolve_imported_type_text_in_file(clean_id, file_name)
		}
	}
	if clean.contains('.') || module_name == '' || module_name == 'main'
		|| module_name == 'builtin' || types.is_builtin_type_name(clean)
		|| is_generic_placeholder_type_name(clean) {
		return clean_id
	}
	qualified := '${module_name}.${clean}'
	if t.type_authority_has(qualified) {
		return flat.source_type_id(qualified)
	}
	if !isnil(t.tc) && (t.tc.type_aliases.contains(qualified) || qualified in t.tc.structs
		|| qualified in t.tc.enum_names || t.tc.sum_types.contains(qualified)
		|| qualified in t.tc.interface_names) {
		return flat.source_type_id(qualified)
	}
	return clean_id
}

fn (t &Transformer) decl_fn_type_in_scope(typ_id u32, module_name string, file_name string) ?u32 {
	typ := flat.source_type_name(typ_id)
	params, ret := fn_type_text_parts(typ) or { return none }
	mut scoped_params := []string{cap: params.len}
	for param in params {
		scoped_params << flat.source_type_name(t.decl_fn_type_param_in_scope(param, module_name, file_name))
	}
	if ret.len > 0 {
		return flat.source_type_id('fn(${scoped_params.join(', ')}) ${flat.source_type_name(t.decl_param_type_in_scope(flat.source_type_id(ret), module_name, file_name))}')
	}
	return flat.source_type_id('fn(${scoped_params.join(', ')})')
}

fn (t &Transformer) decl_fn_type_param_in_scope(param string, module_name string, file_name string) u32 {
	mut text := trimmed_transform_text(param)
	mut is_mut := false
	if text.starts_with('mut ') {
		is_mut = true
		text = trimmed_transform_text(text[4..])
	}
	space := generic_top_level_space_index(text)
	if space > 0 {
		head := trimmed_transform_text(text[..space])
		tail := trimmed_transform_text(text[space + 1..])
		if generic_fn_type_param_head_is_name(head, tail) {
			text = tail
		}
	}
	for marker in ['[]', '&', 'map[', 'fn(', 'fn ('] {
		marker_idx := text.index(marker) or { continue }
		if marker_idx <= 0 {
			continue
		}
		head := trimmed_transform_text(text[..marker_idx])
		tail := trimmed_transform_text(text[marker_idx..])
		if generic_fn_type_param_head_is_name(head, tail) {
			text = tail
		}
		break
	}
	scoped_source_id_2 := t.decl_param_type_in_scope(flat.source_type_id(text), module_name, file_name)
	scoped_source_id_3 := scoped_source_id_2
	scoped := flat.source_type_name(scoped_source_id_3)
	if is_mut && scoped_source_id_2 != 0 && !scoped.starts_with('&') {
		return flat.source_type_id('&' + scoped)
	}
	return scoped_source_id_3
}

// call_is_variadic_for_node also follows checker-resolved function-value targets.
fn (t &Transformer) call_is_variadic_for_node(call_name string, node flat.Node) bool {
	if (node.flags & flat.node_flag_variadic_call) != 0 {
		return true
	}
	if t.call_is_variadic(call_name) {
		return true
	}
	if !isnil(t.tc) && node.children_count > 0 {
		if name := t.tc.resolved_fn_value_name(t.a.child(&node, 0)) {
			return t.call_is_variadic(name)
		}
	}
	return false
}

// call_is_variadic updates call is variadic state for Transformer.
fn (t &Transformer) call_is_variadic(call_name string) bool {
	// Generic specializations are re-checked on every call (see below).
	if isnil(t.call_variadic_cache) || call_name.contains('_T_') {
		return t.call_is_variadic_keyed(call_name)
	}
	// Answer repeated lookups before building the memo's composite key.
	mut cache := t.call_variadic_cache
	mut recent := cache.recent_cache()
	state, _ := recent.get(t.cur_file, call_name, '')
	if state != 0 {
		return state > 0
	}
	result := t.call_is_variadic_keyed(call_name)
	recent.put(t.cur_file, call_name, '', if result { i8(1) } else { i8(-1) }, '')
	return result
}

fn (t &Transformer) call_is_variadic_keyed(call_name string) bool {
	// Generic signatures are registered after some early transform probes. Do not
	// let an earlier negative cache entry hide the template's variadic marker.
	if !isnil(t.tc) && call_name.contains('_T_') && t.tc.specialized_generic_fns[call_name]
		&& t.tc.fn_variadic[call_name.all_before('_T_')] {
		return true
	}
	if !isnil(t.call_variadic_cache) {
		mut cache := t.call_variadic_cache
		cached := cache.get(t.cur_file, call_name)
		if cached != 0 {
			return cached > 0
		}
	}
	result := t.call_is_variadic_uncached(call_name)
	if !isnil(t.call_variadic_cache) {
		mut cache := t.call_variadic_cache
		cache.put(t.cur_file, call_name, if result { i8(1) } else { i8(-1) })
	}
	return result
}

fn (t &Transformer) call_is_variadic_uncached(call_name string) bool {
	if call_name == '' || isnil(t.tc) {
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
		resolved_call := flat.source_type_name(t.tc.resolve_imported_type_text_in_file(flat.source_type_id(call_name), t.cur_file))
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
	if call_name != '' && call_name in t.tc.fn_ret_types {
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
fn (t &Transformer) call_param_type_name(call_name string, idx int) u32 {
	if idx < 0 || call_name == '' || isnil(t.tc) {
		return u32(0)
	}
	params := t.tc.fn_param_types[call_name] or { return u32(0) }
	if idx >= params.len {
		return u32(0)
	}
	return t.semantic_type_name(params[idx])
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
	if arg_type == 0 {
		arg_type = t.resolve_expr_type(arg_id)
	}
	addr := t.builtin_addr_expr(arg_id, arg_type)
	if arg_type != 0 {
		t.set_node_typ(int(addr), flat.source_type_id('&${flat.source_type_name(arg_type)}'))
	}
	return addr
}

fn (mut t Transformer) builtin_addr_expr(arg_id flat.NodeId, arg_type_id u32) flat.NodeId {
	arg_type_shape_2 := flat.source_type_shape(arg_type_id)
	if t.expr_is_overloaded_index_result(arg_id) {
		value := t.transform_expr(arg_id)
		value_type_source_id := t.overloaded_index_result_type(arg_id) or { arg_type_id }

		tmp_name := t.new_temp('addr')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, value_type_source_id)
		return t.make_prefix(.amp, t.make_ident(tmp_name))
	}
	if !arg_type_shape_2.has(.pointer) {
		return t.runtime_addr(arg_id, arg_type_id)
	}
	if addr := t.redundant_deref_addr(arg_id) {
		return addr
	}
	if !t.expr_can_take_address(arg_id) {
		stable := t.stable_transformed_expr_for_reuse(arg_id, arg_type_id, 'addr')
		return t.make_prefix(.amp, stable)
	}
	return t.make_prefix(.amp, arg_id)
}

fn (mut t Transformer) transform_implicit_ref_arg(arg_id flat.NodeId, param_type_id u32) ?flat.NodeId {
	param_type_comparison_type_id_6 := param_type_id
	arg_node := t.a.nodes[int(arg_id)]
	mut data_node := arg_node
	for data_node.kind == .paren && data_node.children_count == 1 {
		data_node = t.a.child_node(&data_node, 0)
	}
	if data_node.kind == .selector && data_node.value == 'data' && data_node.children_count > 0 {
		base_type_shape := flat.source_type_shape(t.trim_all_pointer_type(t.normalized_node_type(t.a.child(&data_node, 0))))
		if base_type_shape.has(.array_type) {
			// A dynamic array's data is already the address of its elements.
			return t.transform_expr(arg_id)
		}
	}
	mut arg_type := t.node_type(arg_id)
	if arg_node.kind == .call {
		concrete_type_source_id := t.concrete_generic_call_return_type(arg_id, arg_node)
		concrete_type_source_id_4 := concrete_type_source_id

		if concrete_type_source_id != 0 {
			arg_type = concrete_type_source_id_4
		}
	}
	if arg_type == 0 {
		arg_type = t.resolve_expr_type(arg_id)
	}
	actual_depth, actual_base := t.pointer_type_depth_and_base(arg_type)
	expected_depth, expected_base := t.pointer_type_depth_and_base(param_type_id)
	if expected_depth <= actual_depth || expected_depth == 0 || actual_base == 0
		|| expected_base == 0 {
		return none
	}
	actual_type_source_id := t.normalize_type_alias(actual_base)
	actual_type_comparison_type_id := actual_type_source_id
	expected_type_comparison_type_id := t.normalize_type_alias(expected_base)
	if actual_type_comparison_type_id != expected_type_comparison_type_id
		&& type_text_without_main_locks(actual_type_comparison_type_id) != type_text_without_main_locks(expected_type_comparison_type_id) {
		return none
	}
	// A `voidptr` already is a pointer, accepted for any pointer parameter: passed to
	// `&voidptr` (C's `void **`) it is the pointer to write through, not a value to
	// reference (C translated by c2v: `sqlite3_prepare16(..., voidptr(&z_tail))`).
	if actual_type_source_id == flat.source_type_void {
		return none
	}
	if expected_depth == actual_depth + 1 && arg_node.kind == .ident
		&& t.pointer_value_rvalues[arg_node.value] {
		storage_type_source_id := t.var_type(arg_node.value)
		storage_type_comparison_type_id := storage_type_source_id

		if storage_type_comparison_type_id == param_type_comparison_type_id_6 {
			value := t.transform_expr_preserving_pointer_value(arg_id)
			t.set_node_typ(int(value), storage_type_source_id)
			return value
		}
	}
	mut current := t.transform_expr(arg_id)
	mut force_materialize := t.expr_is_overloaded_index_result(arg_id)
		|| t.raw_const_type_name_for_expr(arg_id) != none
	mut value_node := t.a.nodes[int(current)]
	for value_node.kind == .paren && value_node.children_count == 1 {
		value_node = t.a.child_node(&value_node, 0)
	}
	if value_node.kind in [.cast_expr, .as_expr] {
		// `&T(x)` casts `x` to a pointer: the value of a cast is referenced
		// through a temporary.
		force_materialize = true
	}
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
		addr_type := flat.source_type_id('&${flat.source_type_name(current_type)}')
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

fn (t &Transformer) overloaded_index_result_type(id flat.NodeId) ?u32 {
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
				if ret_source_id := t.fn_ret_types.type_id(method_name) {
					typ_source_id := t.normalize_type_alias(ret_source_id)

					if decl_type_is_usable(typ_source_id) {
						return typ_source_id
					}
				}
				if ret := t.tc.fn_ret_types[method_name] {
					typ_source_id_2 := t.normalize_type_alias(t.semantic_type_name(ret))

					if decl_type_is_usable(typ_source_id_2) {
						return typ_source_id_2
					}
				}
			}
			base_type := t.tc.expr_type(base_id) or { t.tc.resolve_type(base_id) }
			if info := t.tc.index_overload_call_info(base_type, false) {
				typ_source_id_3 := t.normalize_type_alias(t.semantic_type_name(info.return_type))

				if decl_type_is_usable(typ_source_id_3) {
					return typ_source_id_3
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

fn (t &Transformer) pointer_type_depth_and_base(typ u32) (int, u32) {
	mut depth := 0
	mut clean := t.trimmed_type_id(typ)
	for flat.source_type_shape(clean).has(.pointer) {
		depth++
		clean = t.trimmed_type_id(t.trim_pointer_type(clean))
	}
	match clean {
		flat.source_type_voidptr {
			depth++
			clean = flat.source_type_void
		}
		flat.source_type_byteptr {
			depth++
			clean = flat.source_type_u8
		}
		flat.source_type_charptr {
			depth++
			clean = flat.source_type_char
		}
		else {}
	}
	return depth, clean
}

// transform_call_arg_for_param transforms transform call arg for param data for transform.
@[direct_array_access]
fn (mut t Transformer) transform_call_arg_for_param(arg_id flat.NodeId, param_type_id u32) flat.NodeId {

	// Only the call result is returned. Argument conversions keep their call-site
	// semantics even when the enclosing call is part of a return expression.
	outer_return_expr := t.in_return_expr
	t.in_return_expr = false
	defer {
		t.in_return_expr = outer_return_expr
	}
	// Keep prerequisites produced by earlier arguments outside this argument's
	// expression. A later block argument (notably `unsafe { nil }`) transforms its
	// statements eagerly and would otherwise drain an earlier sum-box temporary
	// into the block, after the call has already referenced it.
	source_is_mut := int(arg_id) >= 0 && int(arg_id) < t.a.nodes.len
		&& t.a.nodes[int(arg_id)].is_mut
	outer_pending := t.pending_stmts
	t.pending_stmts = []flat.NodeId{}
	result := t.transform_call_arg_for_param_isolated(arg_id, param_type_id)
	if source_is_mut && int(result) >= 0 && int(result) < t.a.nodes.len
		&& !t.a.nodes[int(result)].is_mut {
		// Lowered lvalues must keep aliasing caller storage, including pointer slots.
		t.a.set_node_is_mut(result, true)
	}
	arg_pending := t.pending_stmts
	t.pending_stmts = outer_pending
	for stmt in arg_pending {
		t.pending_stmts << stmt
	}
	return result
}

// call_arg_is_undeclared_c_identifier reports whether the argument is `C.<name>`,
// a C variable that V has no type for (see c_identifier_is_undeclared). A `mut`
// argument names the object whose address its parameter takes.
fn (t &Transformer) call_arg_is_undeclared_c_identifier(arg_node &flat.Node) bool {
	if isnil(t.tc) || arg_node.is_mut || arg_node.kind != .selector || arg_node.children_count == 0 {
		return false
	}
	base := t.a.child_node(arg_node, 0)
	return base.kind == .ident && base.value == 'C' && t.tc.c_identifier_is_undeclared(arg_node.value)
}

@[direct_array_access]
fn (mut t Transformer) transform_call_arg_for_param_isolated(arg_id flat.NodeId, param_type_id u32) flat.NodeId {
	param_type_comparison_type_id_7 := param_type_id
	param_type_source_id_3 := param_type_comparison_type_id_7
	param_type := flat.source_type_name(param_type_source_id_3)
	if int(arg_id) < 0 {
		return arg_id
	}
	mut arg_node := &t.a.nodes[int(arg_id)]
	if arg_node.is_mut && param_type.starts_with('&?') {
		if forwarded := t.transform_mut_optional_view_forward_arg(arg_id) {
			return forwarded
		}
		if wrapped := t.transform_mut_plain_value_optional_call_arg(arg_id, param_type_id) {
			return wrapped
		}
	}
	if param_type.starts_with('&') && t.call_arg_is_zero_pointer_literal(arg_id) {
		value := t.transform_expr(arg_id)
		t.set_node_typ(int(value), param_type_id)
		return value
	}
	if (param_type.starts_with('&') || t.transform_param_type_is_void_pointer(param_type_id))
		&& t.call_arg_is_undeclared_c_identifier(arg_node) {
		// The checker gives a C variable that V does not declare the type that its
		// first use expects, but the body of a generic instance is not checked. Its
		// argument takes the type of the parameter here, so that no later pass
		// takes it for a value whose address the pointer parameter needs.
		return t.make_cast(param_type_id, t.transform_expr(arg_id), param_type_id)
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
		source_type_source_id := t.node_type(source_id)

		if t.is_optional_type_name(source_type_source_id) {
			payload_type_source_id := t.optional_base_type(source_type_source_id)

			// Evaluate the optional once, keeping it addressable so mutations
			// through the reference still write back to the payload (a stable
			// ident/selector is reused directly, a call is materialized once).
			opt_expr := t.stable_transformed_expr_for_reuse(t.transform_expr(source_id), source_type_source_id, 'opt_ref')
			if t.is_optional_type_name(t.cur_fn_ret_type) {
				// `opt?` propagates: return none/err when it is none, instead of
				// unconditionally treating it as present.
				not_ok := t.make_prefix(.not, t.make_selector(opt_expr, 'ok', flat.source_type_bool))
				err_expr := t.result_error_expr(opt_expr)
				else_stmts := t.lower_or_body_to_stmts_with_err_expr(flat.empty_node, '', payload_type_source_id, '?', err_expr)
				t.pending_stmts << t.make_if(not_ok, t.make_block_skip_scope_drops(else_stmts), t.make_empty())
			} else {
				// Comptime option-payload-mut (e.g. `decode(mut result.field?)` in a
				// `$for` decoder): the callee fills the payload, so mark it present and
				// expose its address. Normal `?` propagation in a non-option-returning
				// function would have been rejected by the checker, so this is the only
				// way we get here without an option return type.
				ok_target := t.make_selector(opt_expr, 'ok', flat.source_type_bool)
				t.pending_stmts << t.make_assign(ok_target, t.make_bool_literal(true))
			}
			payload := t.make_selector(opt_expr, 'value', payload_type_source_id)
			addr := t.make_prefix(.amp, payload)
			t.set_node_typ(int(addr), param_type_id)
			return addr
		}
	}
	if arg_node.kind == .lambda_expr {
		if lifted := t.lift_lambda_expr_for_fn_param(arg_id, *arg_node, param_type_id) {
			return lifted
		}
	}
	if arg_node.kind == .fn_literal {
		if lifted := t.lift_fn_literal_for_fn_param(arg_id, *arg_node, param_type_id) {
			return lifted
		}
	}
	if arg_node.kind == .array_literal && arg_node.typ == 0 && param_type.starts_with('[]') {
		t.set_node_typ(int(arg_id), param_type_id)
	}
	resolved_param_type_source_id := t.comptime_normalize_type_alias_chain(param_type_id)
	resolved_param_type := flat.source_type_name(resolved_param_type_source_id)
	if t.is_optional_type_name(resolved_param_type_source_id) {
		payload_type_source_id_2 := t.optional_base_type(resolved_param_type_source_id)

		mut payload_ref_type := t.comptime_normalize_type_alias_chain(payload_type_source_id_2)
		for t.is_optional_type_name(payload_ref_type) {
			payload_ref_type = t.comptime_normalize_type_alias_chain(t.optional_base_type(payload_ref_type))
		}
		if flat.source_type_shape(payload_ref_type).has(.pointer_array)
			&& t.fixed_array_reference_arg_needs_durable_header(arg_id, flat.source_type_id(flat.source_type_name(payload_ref_type)[1..])) {
			// Create the durable reference before placing it in a successful wrapper.
			// None and already-wrapped values follow the ordinary optional conversion.
			value := t.transform_call_arg_for_param(arg_id, payload_type_source_id_2)
			wrapped := t.make_optional_some(value, resolved_param_type_source_id)
			t.set_node_typ(int(wrapped), param_type_id)
			return wrapped
		}
	}
	array_ref_type := resolved_param_type
	if array_ref_type.starts_with('&[]') {
		arg_type_source_id_5 := t.node_type(arg_id)
		arg_type_shape_5 := flat.source_type_shape(arg_type_source_id_5)
		range_type := (if arg_node.is_mut && arg_type_shape_5.has(.array_type) {
			arg_type_source_id_5
		} else {
			flat.source_type_id(array_ref_type[1..])
		})
		mut range_arg_id := t.unwrap_parens(arg_id)
		range_arg := t.a.nodes[int(range_arg_id)]
		if range_arg.kind == .prefix && range_arg.op == .amp && range_arg.children_count == 1 {
			range_arg_id = t.unwrap_parens(t.a.child(&range_arg, 0))
		}
		if view := t.fixed_array_range_view_for_arg(range_arg_id, range_type, true) {
			return t.fixed_array_mut_arg(view, range_type, param_type_id)
		}
		fixed_type_source_id := t.fixed_array_reference_arg_type(arg_id, flat.source_type_id(array_ref_type[1..]))

		if t.is_fixed_array_type(fixed_type_source_id) {
			array_type := array_ref_type[1..]
			view := t.fixed_array_mut_arg_backing(arg_id, fixed_type_source_id, flat.source_type_id(array_type))
			return t.fixed_array_mut_arg(view, flat.source_type_id(array_type), param_type_id)
		}
	}
	if t.transform_param_type_is_void_pointer(param_type_id)
		&& t.call_arg_is_fn_pointer_value(arg_id, *arg_node) {
		return t.transform_expr(arg_id)
	}
	if t.transform_param_type_is_void_pointer(param_type_id) {
		// C APIs commonly spell a null callback/data pointer as the integer
		// literal `0`.  It is already a pointer value in that context; taking
		// the address of a temporary integer instead produces a non-null,
		// dangling pointer.
		if t.expr_is_nil_like(arg_id) || (arg_node.kind == .int_literal && arg_node.value == '0') {
			return t.make_cast(param_type_id, t.transform_expr(arg_id), param_type_id)
		}
		// A `c'...'` literal denotes a C string pointer even though its flat
		// literal node retains the character type used for byte contexts.
		if arg_node.kind == .char_literal && arg_node.value.starts_with('c:') {
			return t.transform_expr(arg_id)
		}
		mut arg_type := flat.source_type_name(t.node_type(arg_id))
		if arg_node.kind == .call {
			concrete_type_source_id_2 := t.concrete_generic_call_return_type(arg_id, *arg_node)
			concrete_type := flat.source_type_name(concrete_type_source_id_2)
			if concrete_type_source_id_2 != 0 {
				arg_type = concrete_type
			}
		}
		clean_arg_type_source_id := t.normalize_type_alias(flat.source_type_id(arg_type))
		clean_arg_type := flat.source_type_name(clean_arg_type_source_id)
		if clean_arg_type_source_id != 0 && !clean_arg_type.starts_with('&')
			&& clean_arg_type !in ['voidptr', 'byteptr', 'charptr', 'nil'] {
			value := t.transform_expr(arg_id)
			mut addr := flat.empty_node
			if t.raw_const_type_name_for_expr(arg_id) == none && t.expr_can_take_address(value) {
				addr = t.make_prefix(.amp, value)
			} else {
				tmp_name := t.new_temp('voidptr_arg')
				t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, clean_arg_type_source_id)
				addr = t.make_prefix(.amp, t.make_ident(tmp_name))
			}
			return t.make_cast(param_type_id, addr, param_type_id)
		}
	}
	if arg_node.kind == .enum_val && param_type in t.enum_types {
		return t.transform_enum_shorthand(arg_id, *arg_node, param_type_id)
	}
	if param_type.starts_with('&') && arg_node.kind == .prefix && arg_node.op == .amp
		&& arg_node.children_count > 0 {
		child_id := t.a.child(arg_node, 0)
		child := t.a.nodes[int(child_id)]
		if child.kind == .ident && t.has_smartcast(child.value) {
			sc := t.find_smartcast(child.value) or { SmartcastContext{} }
			target_source_id_3 := t.smartcast_target_type(sc)

			if target_source_id_3 != 0
				&& t.normalize_type_alias(t.trim_pointer_type(target_source_id_3)) == t.normalize_type_alias(flat.source_type_id(param_type[1..])) {
				if narrowed := t.smartcast_ident_value(child.value) {
					narrowed_node := t.a.nodes[int(narrowed)]
					if narrowed_node.kind == .prefix && narrowed_node.op == .mul
						&& narrowed_node.children_count > 0 {
						pointer := t.a.child(&narrowed_node, 0)
						t.set_node_typ(int(pointer), param_type_id)
						return pointer
					}
				}
			}
		}
	}
	if param_type.starts_with('&') && arg_node.kind == .ident && arg_node.is_mut
		&& t.mut_param_values[arg_node.value] {
		storage_type_source_id := t.var_type(arg_node.value)
		storage_type := flat.source_type_name(storage_type_source_id)
		if storage_type.starts_with('&')
			&& t.normalize_type_alias(storage_type_source_id) == t.normalize_type_alias(param_type_id) {
			// Forwarding `mut val` from a `mut T` parameter reuses the parameter's
			// existing pointer-backed storage. Taking another address would pass `T**`.
			storage := t.make_ident(arg_node.value)
			t.set_node_typ(int(storage), storage_type_source_id)
			return storage
		}
		if storage_type.starts_with('&')
			&& t.normalize_type_alias(flat.source_type_id('&${storage_type}')) == t.normalize_type_alias(param_type_id) {
			// A `mut x &T` slot (`T**`) needs the address of a pointer holding that
			// storage; `&val` on a `mut T` parameter would still yield `T*`.
			storage := t.make_ident(arg_node.value)
			t.set_node_typ(int(storage), storage_type_source_id)
			tmp_name := t.new_temp('ref_arg')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, storage, storage_type_source_id)
			addr := t.make_prefix(.amp, t.make_ident(tmp_name))
			t.set_node_typ(int(addr), param_type_id)
			return addr
		}
	}
	if param_type.starts_with('&') && arg_node.kind == .selector
		&& t.selector_chain_has_sum_variant_field(arg_id) {
		value := t.transform_expr(arg_id)
		mut value_type := t.node_type(arg_id)
		if value_type == 0 {
			value_type = t.node_type(value)
		}
		stable := t.stable_transformed_expr_for_reuse(value, value_type, 'addr')
		addr := t.make_prefix(.amp, stable)
		t.set_node_typ(int(addr), param_type_id)
		return addr
	}
	if param_type.starts_with('&') && t.is_sum_type_name(flat.source_type_id(param_type[1..])) {
		target_sum := param_type[1..]
		resolved_target_sum := flat.source_type_name(t.resolve_sum_name(flat.source_type_id(target_sum)))
		arg_type_source_id_3 := t.node_type(arg_id)

		arg_key := t.expr_key(arg_id)
		has_smartcast := t.has_smartcast(arg_key)
		if has_smartcast {
			raw_arg_type_source_id := t.raw_expr_type_without_smartcast(arg_id)

			if flat.source_type_name(t.resolve_sum_name(t.trim_pointer_type(raw_arg_type_source_id))) == resolved_target_sum {
				return t.make_plain_expr_for_smartcast(arg_id)
			}
		}
		if has_smartcast && flat.source_type_name(t.resolve_sum_name(t.trim_pointer_type(arg_type_source_id_3))) == resolved_target_sum {
			value := t.transform_expr(arg_id)
			if flat.source_type_shape(t.node_type(value)).has(.pointer) {
				return value
			}
			address := t.make_prefix(.amp, value)
			t.set_node_typ(int(address), param_type_id)
			return address
		}
		if !has_smartcast
			&& flat.source_type_name(t.resolve_sum_name(t.normalize_type_alias(t.trim_pointer_type(arg_type_source_id_3)))) == resolved_target_sum {
			return t.transform_expr(arg_id)
		}
		if arg_node.kind == .prefix && arg_node.op == .amp && arg_node.children_count > 0 {
			inner_id := t.a.child(arg_node, 0)
			inner_key := t.expr_key(inner_id)
			inner_has_smartcast := t.has_smartcast(inner_key)
			if inner_has_smartcast {
				raw_inner_type_source_id := t.raw_expr_type_without_smartcast(inner_id)

				if flat.source_type_name(t.resolve_sum_name(t.trim_pointer_type(raw_inner_type_source_id))) == resolved_target_sum {
					inner := t.make_plain_expr_for_smartcast(inner_id)
					addr := t.make_prefix(.amp, inner)
					t.set_node_typ(int(addr), param_type_id)
					return addr
				}
			}
			inner_type_source_id := t.node_type(inner_id)

			if flat.source_type_name(t.resolve_sum_name(t.normalize_type_alias(t.trim_pointer_type(inner_type_source_id)))) == resolved_target_sum {
				return t.transform_expr(arg_id)
			}
		}
		wrapped := t.wrap_sum_value(arg_id, flat.source_type_id(target_sum))
		tmp_name := t.new_temp('sum_arg')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, wrapped, flat.source_type_id(target_sum))
		return t.make_prefix(.amp, t.make_ident(tmp_name))
	}
	if param_type.starts_with('&') && arg_node.kind == .prefix && arg_node.op == .amp
		&& arg_node.children_count > 0 {
		child_id := t.a.child(arg_node, 0)
		child := t.a.nodes[int(child_id)]
		child_type_source_id := t.node_type(child_id)
		child_type_shape := flat.source_type_shape(child_type_source_id)
		normalized_child_type := flat.source_type_name(t.normalize_type_alias(child_type_source_id))
		// A value local moved to the heap is stored as its address, but it is still a
		// value: `mut &v` names that address, as `&v` and `mut v` do, not a pointer slot.
		heaped_value_local := child.kind == .ident && child.value in t.heaped_amp_locals
			&& !t.has_smartcast(child.value)
		explicit_mut_pointer_slot := arg_node.is_mut && !heaped_value_local
			&& normalized_child_type == flat.source_type_name(t.normalize_type_alias(param_type_id))
		if child_type_source_id != 0
			&& (normalized_child_type == flat.source_type_name(t.normalize_type_alias(flat.source_type_id(param_type[1..])))
				|| explicit_mut_pointer_slot) {
			// An explicit address of a pointer local supplies the caller's pointer
			// slot to a mutable pointer parameter. Generic `&ident` lowering treats
			// pointer-backed identifiers as their stored pointer value, which would
			// otherwise drop this address and pass `T*` where `T**` is required.
			if child.kind == .ident
				&& (child_type_shape.has(.pointer) || flat.source_type_shape(t.raw_var_type(child.value)).has(.pointer)
					|| t.pointer_value_rvalues[child.value]) {
				value := t.transform_expr_preserving_pointer_value(child_id)
				addr := t.make_prefix(.amp, value)
				addr_type := (if explicit_mut_pointer_slot {
					t.node_type(arg_id)
				} else {
					param_type_source_id_3
				})
				t.set_node_typ(int(addr), addr_type)
				return addr
			}
			return t.transform_expr(arg_id)
		}
	}
	if param_type.starts_with('&') && !t.is_sum_type_name(flat.source_type_id(param_type[1..]))
		&& !t.is_interface_type(param_type_id) {
		if arg_node.kind == .ident && t.pointer_value_lvalues[arg_node.value] {
			storage_type := t.var_type(arg_node.value)
			if arg_node.value in t.heaped_amp_locals
				&& t.normalize_type_alias(storage_type) == t.normalize_type_alias(param_type_id) {
				// Promotion owns the source value's slot, including pointer aliases.
				// A reference parameter receives that slot, not its stored pointer.
				storage := t.make_ident(arg_node.value)
				t.set_node_typ(int(storage), storage_type)
				return storage
			}
			arg_type_comparison_type_id_5 := t.node_type(arg_id)
			arg_type := flat.source_type_name(arg_type_comparison_type_id_5)
			if arg_type_comparison_type_id_5 == param_type_comparison_type_id_7 {
				value := t.transform_expr_preserving_pointer_value(arg_id)
				if t.has_smartcast(arg_node.value) {
					value_node := t.a.nodes[int(value)]
					if value_node.kind == .prefix && value_node.op == .mul
						&& value_node.children_count > 0 {
						pointer := t.a.child(&value_node, 0)
						t.set_node_typ(int(pointer), param_type_id)
						return pointer
					}
				}
				t.set_node_typ(int(value), param_type_id)
				return value
			}
			if arg_type.starts_with('&')
				&& t.normalize_type_alias(flat.source_type_id(arg_type[1..])) == t.normalize_type_alias(param_type_id) {
				value := t.transform_expr_preserving_pointer_value(arg_id)
				storage_type_source_id_2 := t.var_type(arg_node.value)
				storage_type_comparison_type_id_3 := storage_type_source_id_2

				if storage_type_comparison_type_id_3 == param_type_comparison_type_id_7 {
					t.set_node_typ(int(value), storage_type_source_id_2)
					return value
				}
				deref := t.make_prefix(.mul, value)
				t.set_node_typ(int(deref), param_type_id)
				return deref
			}
		}
		if implicit_ref := t.transform_implicit_ref_arg(arg_id, param_type_id) {
			return implicit_ref
		}
		arg_type_source_id_2 := t.node_type(arg_id)
		arg_type_shape_4 := flat.source_type_shape(arg_type_source_id_2)
		if arg_type_source_id_2 != 0 && !arg_type_shape_4.has(.pointer)
			&& t.normalize_type_alias(arg_type_source_id_2) == t.normalize_type_alias(flat.source_type_id(param_type[1..]))
			&& t.expr_can_take_address(arg_id) && !t.expr_is_overloaded_index_result(arg_id) {
			value := t.transform_expr(arg_id)
			addr := t.make_prefix(.amp, value)
			t.set_node_typ(int(addr), param_type_id)
			return addr
		}
	}
	if param_type.starts_with('&') && arg_node.kind == .ident
		&& t.pointer_global_arg_matches_param(arg_node.value, param_type_id) {
		return t.transform_expr(arg_id)
	}
	if !t.in_spawn_expr {
		if ptr_arg := t.transform_pointer_rvalue_arg(arg_id, *arg_node, param_type_id) {
			return ptr_arg
		}
	}
	if !param_type.starts_with('&') && t.is_sum_type_name(param_type_id) {
		arg_key := t.expr_key(arg_id)
		if t.has_smartcast(arg_key) {
			raw_arg_type_source_id_2 := t.raw_expr_type_without_smartcast(arg_id)

			if t.resolve_sum_name(t.trim_pointer_type(raw_arg_type_source_id_2)) == t.resolve_sum_name(param_type_id) {
				value := t.make_plain_expr_for_smartcast(arg_id)
				return t.clone_borrowed_projection(arg_id, value, param_type_id)
			}
		}
		value := t.wrap_sum_value(arg_id, param_type_id)
		return t.clone_borrowed_projection(arg_id, value, param_type_id)
	}
	if param_type.starts_with('[]') {
		arg_type_source_id_4 := t.node_type(arg_id)

		if converted := t.transform_array_value_for_dynamic_target(arg_id, param_type_id) {
			return converted
		}
		if t.is_fixed_array_type(arg_type_source_id_4) {
			return t.fixed_array_value_to_owned_array(arg_id, arg_type_source_id_4, param_type_id)
		}
		if const_arg := t.transform_const_array_arg_for_param(arg_id, param_type_id) {
			return const_arg
		}
	}
	if param_type_id != u32(0) && (param_type.contains('unknown')
		|| t.type_text_has_generic_placeholder(param_type_source_id_3, t.cur_module)) {
		return t.transform_expr(arg_id)
	}
	if param_type.starts_with('&') && t.is_interface_type(param_type_id) {
		// Explicit `mut` arguments must alias the caller's storage. Other value
		// arguments are copied because the callee may retain the `&Interface`
		// after this call (for example `log.set_logger(local_logger)`).
		if boxed := t.transform_interface_value_for_type(arg_id, param_type_id, arg_node.is_mut) {
			return boxed
		}
	}
	value := t.transform_expr_for_type(arg_id, param_type_id)
	if param_type.starts_with('&') {
		return value
	}
	// A by-value argument copies a field or slice read the caller keeps owning, so
	// clone it into an independent value the callee owns (see clone_borrowed_projection).
	return t.clone_borrowed_projection(arg_id, value, param_type_id)
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

fn (t &Transformer) pointer_global_arg_matches_param(name string, param_type_id u32) bool {
	param_type := flat.source_type_name(param_type_id)
	arg_type := flat.source_type_name(t.global_ident_type(name) or { return false })
	if !arg_type.starts_with('&') {
		return false
	}
	param_base_comparison_type_id := t.normalize_type_alias(flat.source_type_id(param_type[1..]))
	param_base := flat.source_type_name(param_base_comparison_type_id)
	arg_base_comparison_type_id := t.normalize_type_alias(flat.source_type_id(arg_type[1..]))
	arg_base := flat.source_type_name(arg_base_comparison_type_id)
	return param_base_comparison_type_id == arg_base_comparison_type_id || short_name_view(param_base) == short_name_view(arg_base)
}

fn (t &Transformer) global_ident_type(name string) ?u32 {
	if typ_source_id := t.current_module_global_type(name) {
		return t.normalize_type_alias(typ_source_id)
	}
	return none
}

fn (t &Transformer) current_module_global_type(name string) ?u32 {
	if name == '' {
		return none
	}
	if name.contains('.') {
		if name in t.globals.ids {
			return t.globals.value_id(name)
		}
		return none
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.globals.ids {
			return t.globals.value_id(qname)
		}
		return none
	}
	if name in t.globals.ids {
		return t.globals.value_id(name)
	}
	return none
}

// imported_global_name resolves a bare global through the imports of the active
// source file, or through the program-wide global namespace when the name is
// unique. Do this before the unique-const fallback: all compiled modules
// contribute constants to that fallback, including unrelated transitive modules.
fn (t &Transformer) imported_global_name(name string) ?string {
	if name == '' || name.contains('.') || isnil(t.tc) || t.cur_file.len == 0 {
		return none
	}
	for candidate in t.tc.file_selective_imports[file_import_key(t.cur_file, name)] or {
		[]string{}
	} {
		if candidate in t.globals.ids {
			return candidate
		}
	}
	for candidate in t.global_qualified_names[name] or { []string{} } {
		owner := candidate.all_before_last('.')
		alias := owner.all_after_last('.')
		resolved := t.file_import_module(t.cur_file, alias) or {
			t.tc.imports[alias] or { '' }
		}
		if resolved == owner {
			return candidate
		}
	}
	candidates := t.global_qualified_names[name] or { []string{} }
	if candidates.len == 1 {
		return candidates[0]
	}
	return none
}

// current_module_declares_const: accepts the bare name or the `mod.name`
// spelling transform_ident_expr writes after the const path. Exact keys only —
// const_suffixes sees every transitive module and must not decide ownership.
fn (t &Transformer) current_module_declares_const(name string) bool {
	if name == '' || isnil(t.tc) {
		return false
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		if base in ['main', 'builtin'] {
			// main/builtin consts keep a bare key; `main.name` is only their
			// C-symbol spelling.
			field := short_name_view(name)
			return field in t.tc.const_types
				&& t.tc.const_owner_module(field) in ['', 'main', 'builtin']
		}
		return name in t.tc.const_types && t.tc.const_owner_module(name) == t.cur_module
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		return '${t.cur_module}.${name}' in t.tc.const_types
	}
	return name in t.tc.const_types
}

// ident_compiles_to_global_symbol: cgen emits a global's C symbol. A const of
// the declaring module owns the bare name and its C symbol (docs.md:3645-3653),
// so this must stay in lockstep with transform_ident_expr (it keeps the const's
// symbol iff this is false) or method and symbol come from different
// declarations and read garbage. `t.globals` is keyed by bare name across
// modules, so a main/builtin hit is not proof — a local const still owns it.
fn (t &Transformer) ident_compiles_to_global_symbol(name string) bool {
	if t.current_module_global_type(name) == none {
		if t.current_module_declares_const(name) {
			return false
		}
		return t.imported_global_name(name) != none
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		return true
	}
	return name !in t.tc.const_types
}

fn (mut t Transformer) lift_lambda_expr_for_fn_param(_id flat.NodeId, node flat.Node, param_type_id u32) ?flat.NodeId {
	if node.kind != .lambda_expr || node.children_count == 0 || param_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	fn_type := t.fn_type_from_source_type(param_type_id) or { return none }
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
		param_type_name_source_id := t.semantic_type_name(*fn_type.params[i].typ)
		param_type_name_shape := flat.source_type_shape(param_type_name_source_id)
		children << t.a.add_node(flat.Node{
			kind:  .param
			value: param_node.value
			typ:   param_type_name_source_id
			op:    if param_type_name_shape.has(.pointer) { .amp } else { .none }
		})
	}
	for i in lambda_param_count .. fn_type.params.len {
		param_type_name_source_id_2 := t.semantic_type_name(*fn_type.params[i].typ)
		param_type_name_shape_2 := flat.source_type_shape(param_type_name_source_id_2)
		children << t.a.add_node(flat.Node{
			kind:  .param
			value: '_unused_${i}'
			typ:   param_type_name_source_id_2
			op:    if param_type_name_shape_2.has(.pointer) { .amp } else { .none }
		})
	}
	ret_type_source_id := t.semantic_type_name(fn_type.return_type)

	if t.is_optional_type_name(ret_type_source_id) && t.optional_base_type(ret_type_source_id) == flat.source_type_void {
		body_type_source_id := t.qualify_optional_type(t.node_type(body_id))

		ret_type0_source_id := t.qualify_optional_type(ret_type_source_id)
		ret_type0_source_id_2 := ret_type0_source_id
		ret_type0_shape := flat.source_type_shape(ret_type0_source_id_2)
		body_node := t.a.nodes[int(body_id)]
		if t.optional_types_match(ret_type0_source_id_2, body_type_source_id) {
			children << t.make_direct_optional_forward_return(body_id, ret_type0_source_id)
		} else if ret_type0_shape.has(.result_type) && t.is_error_call(body_node) {
			children << t.make_return(body_id, ret_type0_source_id)
		} else {
			children << t.make_expr_stmt(body_id)
			children << t.make_return(t.make_optional_some(t.make_empty(), ret_type0_source_id), ret_type0_source_id)
		}
	} else if ret_type_source_id != 0 && ret_type_source_id != flat.source_type_void {
		children << t.make_return(body_id, ret_type_source_id)
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
		kind:           .fn_literal
		typ:            ret_type_source_id
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
	})
	return t.lift_fn_literal(fn_id, t.a.nodes[int(fn_id)])
}

fn (mut t Transformer) lift_fn_literal_for_fn_param(_id flat.NodeId, node flat.Node, param_type_id u32) ?flat.NodeId {
	if node.kind != .fn_literal || param_type_id == u32(0) || isnil(t.tc) {
		return none
	}
	fn_type := t.fn_type_from_source_type(param_type_id) or { return none }
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
	if param_ids.len > fn_type.params.len
		|| (param_ids.len == fn_type.params.len && node.generic_params().len == 0) {
		return none
	}
	mut children := []flat.NodeId{cap: capture_ids.len + fn_type.params.len + body_ids.len}
	children << capture_ids
	for i, param_id in param_ids {
		param_type_name_source_id := t.semantic_type_name(*fn_type.params[i].typ)
		param_type_name := flat.source_type_name(param_type_name_source_id)
		param := t.a.nodes[int(param_id)]
		if param.typ_text() == param_type_name {
			children << param_id
		} else {
			children << t.a.add_node(flat.Node{
				kind:   .param
				value:  param.value
				typ:    param_type_name_source_id
				op:     param.op
				is_mut: param.is_mut
			})
		}
	}
	for i in param_ids.len .. fn_type.params.len {
		param_type_name_source_id_2 := t.semantic_type_name(*fn_type.params[i].typ)

		children << t.a.add_node(flat.Node{
			kind:  .param
			value: '_unused_${i}'
			typ:   param_type_name_source_id_2
			op:    .none
		})
	}
	children << body_ids
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	ret_type := (if node.generic_params().len == 0 && node.typ != 0 && node.typ != flat.source_type_void {
		node.typ
	} else {
		t.semantic_type_name(fn_type.return_type)
	})
	fn_id := t.a.add_node(flat.Node{
		kind:           .fn_literal
		typ:            ret_type
		children_start: start
		children_count: flat.child_count(children.len)
		pos:            node.pos
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
			if node.value.len > 0 && node.value !in locals && (t.var_type(node.value)) != 0 {
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

fn (t &Transformer) fn_type_from_source_type(type_id u32) ?types.FnType {
	typ := t.tc.parse_type(type_id)
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

fn (mut t Transformer) transform_pointer_rvalue_arg(arg_id flat.NodeId, arg_node flat.Node, param_type_id u32) ?flat.NodeId {
	param_type := flat.source_type_name(param_type_id)
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
	if t.c_type_nil_call_for_type(value_node, flat.source_type_id(value_type)) {
		nil_id := t.a.add(.nil_literal)
		t.set_node_typ(int(nil_id), param_type_id)
		return nil_id
	}
	mut arg_type := t.node_type(value_id)
	if value_node.kind == .call {
		concrete_type_source_id_3 := t.concrete_generic_call_return_type(value_id, value_node)
		concrete_type_source_id_5 := concrete_type_source_id_3

		if concrete_type_source_id_3 != 0 {
			arg_type = concrete_type_source_id_5
		}
	}
	if arg_type == 0 || arg_type == flat.source_type_void || arg_type == flat.source_type_unknown
		|| t.is_pointer_like_type_name(arg_type) {
		return none
	}
	if t.is_fixed_array_type(t.normalize_type_alias(arg_type))
		&& t.normalize_type_alias(arg_type) != t.normalize_type_alias(flat.source_type_id(value_type)) {
		// Array-to-element-pointer decay needs the whole array, not a scalar
		// temporary of the pointer's base type. CGen handles its ABI storage.
		return t.transform_expr(value_id)
	}
	value := t.transform_expr_for_type(value_id, flat.source_type_id(value_type))
	tmp_name := t.new_temp('ptr_arg')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, value, flat.source_type_id(value_type))
	addr := t.make_prefix(.amp, t.make_ident(tmp_name))
	t.set_node_typ(int(addr), param_type_id)
	return addr
}

fn (t &Transformer) c_type_nil_call_for_type(node flat.Node, value_type_id u32) bool {
	value_type := flat.source_type_name(value_type_id)
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

fn (t &Transformer) is_pointer_like_type_name(typ u32) bool {
	mut clean := typ
	if flat.source_type_shape(clean).has(.mutable_type) {
		clean = flat.source_type_id(flat.source_type_name(clean)[4..])
	}
	return flat.source_type_shape(clean).has(.pointer)
		|| clean in [flat.source_type_voidptr, flat.source_type_byteptr, flat.source_type_charptr]
}

fn (mut t Transformer) append_missing_params_struct_args(mut args []flat.NodeId, params []types.Type, param_offset int) {
	mut param_idx := param_offset
	if args.len > 0 {
		param_idx = args.len - 1 + param_offset
	}
	for param_idx < params.len {
		param_type_source_id := t.semantic_type_name(params[param_idx])

		if struct_type_source_id := t.params_struct_type_name(param_type_source_id) {
			args << t.zero_value_for_type(struct_type_source_id)
			t.mark_params_struct_default_calls(struct_type_source_id)
		} else if params[param_idx] is types.OptionType {
			args << t.a.add_node(flat.Node{
				kind: .none_expr
				typ:  param_type_source_id
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
fn (mut t Transformer) mark_params_struct_default_calls(struct_type_id u32) {
	info := t.lookup_struct_info(struct_type_id) or { return }
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
fn (t &Transformer) is_fn_pointer_type_name(type_name_id u32) bool {
	if type_name_id == u32(0) || isnil(t.tc) {
		return false
	}
	typ := t.tc.parse_type(type_name_id)
	if typ is types.FnType {
		return true
	}
	if typ is types.Alias {
		return typ.base_type is types.FnType
	}
	return false
}

fn (t &Transformer) transform_param_type_is_void_pointer(type_name u32) bool {
	clean := t.trimmed_type_id(type_name)
	return clean in [flat.source_type_voidptr, flat.source_type_byteptr, flat.source_type_charptr]
		|| (flat.source_type_shape(clean).has(.pointer) && t.trim_pointer_type(clean) == flat.source_type_void)
}

fn (t &Transformer) call_arg_is_fn_pointer_value(arg_id flat.NodeId, arg_node flat.Node) bool {
	for type_name in [arg_node.typ_text(), flat.source_type_name(t.node_type(arg_id)),
		flat.source_type_name(t.resolve_expr_type(arg_id))] {
		if t.is_fn_pointer_type_name(flat.source_type_id(type_name)) {
			return true
		}
	}
	if arg_node.kind == .ident {
		type_name_source_id := t.var_type(arg_node.value)

		if t.is_fn_pointer_type_name(type_name_source_id) {
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
fn (mut t Transformer) transform_const_array_arg_for_param(arg_id flat.NodeId, param_type_id u32) ?flat.NodeId {
	param_type := flat.source_type_name(param_type_id)
	if raw_const_type_source_id := t.raw_const_type_name_for_expr(arg_id) {
		if t.normalize_type_alias(raw_const_type_source_id) == t.normalize_type_alias(param_type_id) {
			value := t.transform_expr(arg_id)
			return t.clone_borrowed_projection(arg_id, value, param_type_id)
		}
	}
	expr_id := t.const_expr_for_arg(arg_id) or { return none }
	expr := t.a.nodes[int(expr_id)]
	elem_type := param_type[2..]
	if expr.kind == .array_init && expr.children_count == 0 {
		return t.make_array_new_call(flat.source_type_id(elem_type), t.make_int_literal(0), t.make_int_literal(0))
	}
	if expr.kind != .array_literal {
		return none
	}
	if expr.children_count == 0 {
		return t.make_array_new_call(flat.source_type_id(elem_type), t.make_int_literal(0), t.make_int_literal(0))
	}
	mut values := []flat.NodeId{cap: int(expr.children_count)}
	for i in 0 .. expr.children_count {
		values << t.transform_expr(t.a.child(&expr, i))
	}
	return t.make_array_literal_typed(values, param_type_id)
}

// const_expr_for_arg supports const expr for arg handling for Transformer.
fn (t &Transformer) const_expr_for_arg(arg_id flat.NodeId) ?flat.NodeId {
	if isnil(t.tc) || int(arg_id) < 0 {
		return none
	}
	node := t.a.nodes[int(arg_id)]
	if node.kind == .ident {
		if (t.var_type(node.value)) != 0 {
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
	if (t.var_type(base.value)) != 0 {
		return true
	}
	if typ := t.tc.expr_type(base_id) {
		name_source_id_2 := t.semantic_type_name(typ)
		name_comparison_type_id := name_source_id_2

		return name_source_id_2 != 0 && name_comparison_type_id !in [
			flat.source_type_void,
			flat.source_type_unknown,
		]
	}
	resolved := t.tc.resolve_type(base_id)
	name_source_id_3 := t.semantic_type_name(resolved)
	name_comparison_type_id_2 := name_source_id_3

	return name_source_id_3 != 0 && name_comparison_type_id_2 !in [
		flat.source_type_void,
		flat.source_type_unknown,
	]
}

// const_expr_for_name supports const expr for name handling for Transformer.
fn (t &Transformer) const_expr_for_name(name string) ?flat.NodeId {
	return t.const_expr_for_name_in_context(name, t.cur_module, t.cur_file)
}

fn (t &Transformer) const_expr_for_name_in_context(name string, module_name string, file string) ?flat.NodeId {
	if isnil(t.tc) || name == '' {
		return none
	}
	if key := t.tc.shadowed_builtin_const_key_in_module(name, module_name) {
		return t.tc.const_exprs[key] or { return none }
	}
	if expr_id := t.tc.const_exprs[name] {
		if t.tc.bare_const_key_is_visible_in_module(name, module_name) {
			return expr_id
		}
	}
	key := t.const_type_key_in_context(name, module_name, file) or { return none }
	if expr_id := t.tc.const_exprs[key] {
		return expr_id
	}
	return none
}

// pack_variadic_args supports pack variadic args handling for Transformer.
fn (mut t Transformer) pack_variadic_args(node flat.Node, first_arg int, elem_type types.Type) flat.NodeId {
	expected_enum_source_id := t.semantic_type_name(elem_type)
	expected_enum := flat.source_type_name(expected_enum_source_id)
	array_type := flat.source_type_id('[]${expected_enum}')
	if t.next_non_field_init_arg(node, first_arg) == node.children_count {
		if named_arg := t.transform_variadic_struct_fields(node, first_arg, elem_type) {
			if t.in_const_init {
				return t.mark_packed_variadic_tail(t.make_array_literal_typed([named_arg], array_type), array_type)
			}
			tmp_name := t.new_temp('varargs')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_enum_source_id, t.make_int_literal(0), t.make_int_literal(1)), array_type)
			value_name := t.new_temp('vararg')
			t.pending_stmts << t.make_decl_assign_typed(value_name, named_arg, expected_enum_source_id)
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('array_push', [
				t.make_prefix(.amp, t.make_ident(tmp_name)),
				t.make_prefix(.amp, t.make_ident(value_name)),
			], flat.source_type_void))
			t.set_var_type(tmp_name, array_type)
			return t.mark_packed_variadic_tail(t.make_ident(tmp_name), array_type)
		}
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
				values << t.transform_enum_shorthand(arg_id, arg, expected_enum_source_id)
			} else if t.is_sum_type_name(expected_enum_source_id) {
				values << t.wrap_sum_value(arg_id, expected_enum_source_id)
			} else {
				values << t.transform_expr(arg_id)
			}
			i++
		}
		return t.mark_packed_variadic_tail(t.make_array_literal_typed(values, array_type), array_type)
	}
	tmp_name := t.new_temp('varargs')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_enum_source_id, t.make_int_literal(0), t.make_int_literal(int(node.children_count) - first_arg)), array_type)
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
	return t.mark_packed_variadic_tail(t.make_ident(tmp_name), array_type)
}

fn (mut t Transformer) append_trailing_args_to_variadic_tail(tail flat.NodeId, node flat.Node, first_arg int, variadic_type types.Array) flat.NodeId {
	if first_arg >= node.children_count {
		return tail
	}
	elem_type := *variadic_type.elem_type
	expected_elem_source_id := t.semantic_type_name(elem_type)
	expected_elem := flat.source_type_name(expected_elem_source_id)
	array_type := flat.source_type_id('[]${expected_elem}')
	tail_value := t.stable_transformed_expr_for_reuse(tail, array_type, 'varargs_tail')
	tmp_name := t.new_temp('varargs')
	extra_count := int(node.children_count) - first_arg
	cap_expr := t.make_infix(.plus, t.make_selector(tail_value, 'len', flat.source_type_int), t.make_int_literal(extra_count))
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.make_array_new_call(expected_elem_source_id, t.make_int_literal(0), cap_expr), array_type)
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
	expected_elem_source_id := t.semantic_type_name(elem_type)
	expected_elem := flat.source_type_name(expected_elem_source_id)
	arg := t.a.nodes[int(arg_id)]
	mut value := if specialized := t.specialize_generic_fn_value_arg(arg_id, expected_elem_source_id, true) {
		specialized
	} else if arg.kind == .enum_val && expected_elem in t.enum_types {
		t.transform_enum_shorthand(arg_id, arg, expected_elem_source_id)
	} else if t.is_sum_type_name(expected_elem_source_id) {
		t.wrap_sum_value(arg_id, expected_elem_source_id)
	} else if (t.resolve_interface_type_name(expected_elem_source_id)) != 0 {
		t.transform_expr_for_type(arg_id, expected_elem_source_id)
	} else if !variadic_elem_is_voidptr(elem_type)
		&& escape_type_is_pointer(fixed_array_reference_param_payload(elem_type)) {
		// Reference elements need the same fixed-array view and address conversions
		// as an ordinary reference parameter before they are packed into the tail.
		t.transform_call_arg_for_param(arg_id, expected_elem_source_id)
	} else {
		t.transform_expr(arg_id)
	}
	value = t.clone_borrowed_projection(arg_id, value, expected_elem_source_id)
	value = t.clone_checker_marked_receiver_alias_arg(arg_id, value, expected_elem_source_id)
	t.append_variadic_value_push(tmp_name, value, arg_id, elem_type)
}

fn (mut t Transformer) append_variadic_value_push(tmp_name string, value flat.NodeId, arg_id flat.NodeId, elem_type types.Type) {
	expected_elem_source_id := t.semantic_type_name(elem_type)

	value_name := t.new_temp('vararg')
	if int(arg_id) >= 0 && variadic_elem_is_voidptr(elem_type) {
		value_arg := if t.voidptr_variadic_arg_passes_direct(arg_id) {
			if t.node_type(arg_id) == flat.source_type_voidptr {
				value
			} else {
				t.make_cast(flat.source_type_voidptr, value, flat.source_type_voidptr)
			}
		} else {
			storage_type_source_id_3 := t.voidptr_variadic_storage_type(arg_id)
			storage_type := flat.source_type_name(storage_type_source_id_3)
			storage_value := if storage_type != flat.source_type_name(t.node_type(arg_id)) {
				t.make_cast(storage_type_source_id_3, value, storage_type_source_id_3)
			} else {
				value
			}
			storage_name := t.new_temp('vararg_storage')
			t.pending_stmts << t.make_decl_assign_typed(storage_name, storage_value, storage_type_source_id_3)
			t.make_cast(flat.source_type_voidptr, t.make_prefix(.amp, t.make_ident(storage_name)), flat.source_type_voidptr)
		}
		t.pending_stmts << t.make_decl_assign_typed(value_name, value_arg, expected_elem_source_id)
	} else {
		// Keep the explicit interface storage type visible to cgen. A direct interface
		// literal is a struct init, whose concrete payload name would otherwise override
		// the declaration annotation.
		storage_value := if elem_type is types.Interface { t.make_paren(value) } else { value }
		t.pending_stmts << t.make_decl_assign_typed(value_name, storage_value, expected_elem_source_id)
	}
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(tmp_name)),
		t.make_prefix(.amp, t.make_ident(value_name)),
	], flat.source_type_void))
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
	typ_source_id_5 := t.node_type(arg_id)
	typ := flat.source_type_name(typ_source_id_5)
	return typ_source_id_5 == flat.source_type_voidptr || typ_source_id_5 == flat.source_type_nil || typ == 'charptr' || typ == 'byteptr'
		|| typ.starts_with('&')
}

fn voidptr_variadic_type_passes_direct(typ types.Type) bool {
	if typ is types.Alias {
		return voidptr_variadic_type_passes_direct(typ.base_type)
	}
	return typ is types.Pointer || typ is types.Nil
}

// voidptr_variadic_storage_type returns the type of the temporary that holds one non-pointer
// argument of a `...voidptr` parameter. The callee reads an integer argument as an `int`
// (`*(&int(arg))`), so every integer narrower than the target's `int` is widened to it, like
// the arguments of a C variadic function; an `f32` is widened to `f64`.
fn (t &Transformer) voidptr_variadic_storage_type(arg_id flat.NodeId) u32 {
	typ_source_id := t.normalized_node_type(arg_id)
	typ_source_id_4 := typ_source_id
	typ_source_id_7 := typ_source_id_4
	typ := flat.source_type_name(typ_source_id_7)
	mut scalar := typ
	if !isnil(t.tc) {
		parsed := types.unalias_type(t.tc.parse_type(typ_source_id))
		if parsed is types.Enum {
			// An enum occupies its backing type, or C's 32 bit `int` when it declares none.
			scalar = flat.source_type_name(t.enum_backing_types.type_id(parsed.name()) or { flat.source_type_i32 })
		}
	}
	match scalar {
		'char', 'i8', 'u8', 'i16', 'u16' {
			return flat.source_type_int
		}
		'i32', 'u32', 'rune' {
			// Narrower than `int` only where `int` is 64 bits wide.
			return if types.platform_int_bits() == 64 {
				flat.source_type_int
			} else {
				typ_source_id_7
			}
		}
		'f32' {
			return flat.source_type_f64
		}
		else {
			return typ_source_id_4
		}
	}
}

fn (mut t Transformer) transform_variadic_struct_fields(node flat.Node, field_start int, elem_type types.Type) ?flat.NodeId {
	if field_start >= node.children_count {
		return none
	}
	target_type_source_id := t.semantic_type_name(elem_type)

	struct_type_source_id := t.struct_arg_type_name(target_type_source_id) or { return none }

	return t.transform_trailing_field_init_struct_arg(node, field_start, struct_type_source_id, target_type_source_id)
}

// make_array_literal_typed builds make array literal typed data for transform.
fn (mut t Transformer) make_array_literal_typed(values []flat.NodeId, typ_id u32) flat.NodeId {
	start := t.a.children.len
	for value in values {
		t.a.children << value
	}
	return t.a.add_node(flat.Node{
		kind:           .array_literal
		children_start: start
		children_count: flat.child_count(values.len)
		typ:            typ_id
	})
}

// stringify_expr converts an expression to text, using voidptr.str() for print arguments.
fn (mut t Transformer) stringify_expr(expr_id flat.NodeId, for_print bool) flat.NodeId {
	// Transforming a pointer expression can normalize `&Alias` to `&Base`. Keep the
	// checker's source-level alias here so auto-str can still add `Alias(...)` while
	// reading the pointee through the base representation.
	raw_alias_type_source_id_4 := t.raw_alias_type_for_expr(expr_id)
	raw_alias_type_source_id_7 := raw_alias_type_source_id_4

	expr := t.transform_expr(expr_id)
	// A smartcasted identifier's transformed expression is the concrete value,
	// while the source binding still has its interface/sum type. Stringify the
	// narrowed value instead of rebuilding the source container's auto-str.
	key := t.expr_key(expr_id)
	smartcast := if key.len > 0 { t.find_smartcast(key) } else { none }
	mut typ := (if smartcast != none {
		t.node_type(expr)
	} else {
		u32(0)
	})
	if typ == 0 {
		typ = t.raw_var_type_for_expr(expr_id) or { u32(0) }
	}
	if typ == 0 {
		typ = t.raw_var_type_for_expr(expr) or { u32(0) }
	}
	if typ == 0 {
		typ = t.node_type(expr)
	}
	if typ == 0 {
		typ = t.node_type(expr_id)
	}
	if typ == 0 || typ == flat.source_type_unknown || t.generic_arg_is_unresolved(typ) {
		checker_type_source_id := t.raw_checker_node_type(expr_id)
		checker_type_source_id_2 := checker_type_source_id

		if checker_type_source_id != 0 && checker_type_source_id != flat.source_type_unknown
			&& !t.generic_arg_is_unresolved(checker_type_source_id) {
			typ = checker_type_source_id_2
		}
	}
	if typ == 0 {
		// Structural fallback for compound arguments (infix, prefix, cast,
		// paren, ...) so e.g. `println(a + b)` for ints is stringified via
		// strconv__format_int instead of being passed to println as a raw
		// number. Mirrors the fallback already used by string interpolation.
		typ = t.reliable_stringify_type(expr)
		if typ == 0 {
			typ = t.reliable_stringify_type(expr_id)
		}
	}
	// A selector of a concrete generic instance can retain the declaration's raw
	// placeholder spelling (for example `&Node[T]`) even though its checked node
	// type is already `&Node[int]`. Auto-stringification must use the concrete
	// instance; otherwise the synthesized locals leak `Node_T` into generated C.
	if stringify_type_has_generic_placeholder(flat.source_type_name(typ)) {
		mut concrete_typ := t.lvalue_type(expr_id)
		if concrete_typ == 0 || stringify_type_has_generic_placeholder(flat.source_type_name(concrete_typ)) {
			concrete_typ = t.node_type(expr_id)
		}
		if concrete_typ == 0 || stringify_type_has_generic_placeholder(flat.source_type_name(concrete_typ)) {
			concrete_typ = t.lvalue_type(expr)
		}
		if concrete_typ != 0 && !stringify_type_has_generic_placeholder(flat.source_type_name(concrete_typ)) {
			typ = concrete_typ
		}
	}
	if raw_alias_type_source_id_4 != 0 {
		typ = raw_alias_type_source_id_7
	}
	// `transform_expr` above already auto-dereferenced a `pointer_value_rvalues`
	// ident (mutable for-in bindings, `@[heap]`-promoted locals, ...) to its value,
	// but `typ` was resolved independently and still carries the storage `&T`.
	// Strip it so `wrap_string_conversion` sees a type matching what `expr` (now a
	// plain value read) actually evaluates to, instead of treating it as a
	// genuine nilable pointer.
	if int(expr_id) >= 0 && flat.source_type_shape(typ).has(.pointer) {
		raw_node := t.a.nodes[int(expr_id)]
		if raw_node.kind == .ident
			&& (t.pointer_value_rvalues[raw_node.value]
				|| t.fixed_array_value_rvalues[raw_node.value]) {
			typ = flat.source_type_id(flat.source_type_name(typ)[1..])
		}
	}
	converted := if for_print && flat.source_type_name(typ) in ['voidptr', 'builtin.voidptr'] {
		t.mark_fn_used_name('voidptr.str')
		t.make_call_typed('voidptr.str', [expr], flat.source_type_string)
	} else {
		t.wrap_string_conversion(expr, typ)
	}
	if sc := smartcast {
		// Interface payloads are exposed through their backing address while
		// smartcasted. Preserve the reference marker used by V stringification,
		// even when the concrete payload itself is stored in a boxed allocation.
		// A reference-typed aggregate payload already carries that marker from its
		// own `&Struct`/`&SumType` conversion, so prefixing it again prints `&&`.
		if t.is_interface_type_name(sc.sum_type_name)
			&& !trimmed_transform_text(flat.source_type_name(t.smartcast_target_type(sc))).starts_with('&')
			&& !(flat.source_type_shape(typ).has(.pointer)
				&& t.stringify_aggregate_type_name(t.trim_all_pointer_type(typ)) != none) {
			return t.string_plus(t.make_string_literal('&'), converted)
		}
	}
	return converted
}

const stringify_narrow_integer_types = ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16',
	'u32', 'u64', 'usize', 'rune']

// wide_method_receiver_type returns `u128` or `i128` when the expression is an
// arithmetic node this compiler lowers as a 128-bit value, and an empty string
// otherwise.
fn (t &Transformer) wide_method_receiver_type(id flat.NodeId) u32 {
	return t.wide_operator_result_type(id, 0)
}

// wide_operator_result_type returns `u128` or `i128` when the expression is an
// operator node whose result is that wide. Only an operator widens its result: a
// call, a cast and an index have the type their callee, target or element
// declares, whether or not a 128-bit value sits inside them. Searching through
// those replaced the type of `consume(x)` (an int) and of `u8(x)` (a byte) with
// the type of the operand, which picked the wrong printer and, on the portable
// representation, produced C that does not compile. A shift is the same shape:
// its result is as wide as its left operand, since the right one is a count.
// A logical right shift is the exception to that: it takes its width from the
// left operand but its result is unsigned, so `typeof(i128(-1) >>> 1)` is `u128`.
fn (t &Transformer) wide_operator_result_type(id flat.NodeId, depth int) u32 {
	if depth > 4 || int(id) < 0 || int(id) >= t.a.nodes.len {
		return u32(0)
	}
	node := t.a.nodes[int(id)]
	mut child_limit := node.children_count
	mut unsigned_result := false
	match node.kind {
		.paren {
			if node.children_count == 0 {
				return u32(0)
			}
			return t.wide_operator_result_type(t.a.child(&node, 0), depth + 1)
		}
		.infix {
			if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
				return u32(0)
			}
			if node.op in [.left_shift, .right_shift, .right_shift_unsigned] {
				child_limit = 1
				if node.op == .right_shift_unsigned {
					unsigned_result = true
				}
			}
		}
		.prefix {
			if node.op !in [.minus, .bit_not] {
				return u32(0)
			}
		}
		else {
			return u32(0)
		}
	}
	for i in 0 .. child_limit {
		name := flat.source_type_name(t.raw_checker_node_type(t.a.child(&node, i))).all_after_last('.')
		if name in ['u128', 'i128'] {
			return if unsigned_result {
				flat.source_type_id('u128')
			} else {
				flat.source_type_id(name)
			}
		}
	}
	// An operand that is an operator node of its own widens the same way, and the
	// checker records it under the narrower side just like the outer one.
	for i in 0 .. child_limit {
		nested_source_id := t.wide_operator_result_type(t.a.child(&node, i), depth + 1)
		nested_source_id_2 := nested_source_id

		if nested_source_id != 0 {
			return if unsigned_result { flat.source_type_id('u128') } else { nested_source_id_2 }
		}
	}
	return u32(0)
}

// stringify_wide_integer_operand returns `u128` or `i128` when the expression is
// an operator node whose result is that wide.
fn (t &Transformer) stringify_wide_integer_operand(id flat.NodeId) u32 {
	return t.wide_operator_result_type(id, 0)
}

fn (t &Transformer) declared_selector_pointer_alias_type(id flat.NodeId) ?u32 {
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
	if base_type == 0 {
		base_type = t.node_type(base_id)
	}
	if base_type == 0 {
		base_type = t.raw_checker_node_type(base_id)
	}
	raw_type := flat.source_type_name(t.lookup_struct_field_raw_type(base_type, node.value) or { return none })
	clean := trimmed_transform_text(raw_type)
	if clean.starts_with('&') && t.is_type_alias_name(t.trim_pointer_type(flat.source_type_id(clean))) {
		return flat.source_type_id(clean)
	}
	return none
}

// reliable_stringify_type supports reliable stringify type handling for Transformer.
fn (t &Transformer) reliable_stringify_type(id flat.NodeId) u32 {
	mut typ := t.node_type(id)
	if typ != 0 {
		return typ
	}
	if int(id) >= 0 {
		node := t.a.nodes[int(id)]
		if node.typ != 0 {
			return node.typ
		}
		match node.kind {
			.int_literal {
				return flat.source_type_int
			}
			.float_literal {
				return flat.source_type_f64
			}
			.bool_literal {
				return flat.source_type_bool
			}
			.char_literal {
				return if node.value.starts_with('c:') {
					flat.source_type_id('&u8')
				} else {
					flat.source_type_rune
				}
			}
			.string_literal, .string_interp {
				return flat.source_type_string
			}
			.infix {
				return t.reliable_infix_stringify_type(node)
			}
			.prefix {
				if node.op == .not {
					return flat.source_type_bool
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
					return flat.source_type_id(node.value)
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
	return u32(0)
}

// reliable_infix_stringify_type supports reliable infix stringify type handling for Transformer.
fn (t &Transformer) reliable_infix_stringify_type(node flat.Node) u32 {
	if node.children_count < 2 {
		return u32(0)
	}
	lhs_id := t.a.child(&node, 0)
	rhs_id := t.a.child(&node, 1)
	lhs_type_source_id := t.reliable_stringify_type(lhs_id)
	rhs_type_source_id_2 := t.reliable_stringify_type(rhs_id)
	if lhs_type_source_id == flat.source_type_string || rhs_type_source_id_2 == flat.source_type_string {
		return flat.source_type_string
	}
	match node.op {
		.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or {
			return flat.source_type_bool
		}
		.right_shift_unsigned {
			if lhs_type_source_id != 0 {
				return t.unsigned_shift_type_text(lhs_type_source_id, node)
			}
		}
		.left_shift, .right_shift {
			// shifts keep the left operand's type/width
			if lhs_type_source_id != 0 && t.is_numeric_stringify_type(lhs_type_source_id) {
				return lhs_type_source_id
			}
		}
		.plus, .minus, .mul, .power, .div, .mod, .amp, .pipe, .xor {
			if lhs_type_source_id != 0 && rhs_type_source_id_2 != 0 && t.is_numeric_stringify_type(lhs_type_source_id)
				&& t.is_numeric_stringify_type(rhs_type_source_id_2) {
				if promoted := t.promote_numeric_literal_infix_type(lhs_id, lhs_type_source_id, rhs_id, rhs_type_source_id_2) {
					return promoted
				}
				// Use the promoted result type, not the lhs, so e.g.
				// `1 + u64(x)` formats as unsigned rather than signed int.
				return promote_numeric_stringify_type(lhs_type_source_id, rhs_type_source_id_2)
			}
		}
		else {}
	}

	return u32(0)
}

fn (t &Transformer) unsigned_shift_type_text(typ_id u32, node flat.Node) u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if !isnil(t.tc) {
		mut operand := *t.tc.parse_type(flat.source_type_id(clean))
		file := if source := t.a.source_files[node.pos.id] { source.name } else { t.cur_file }
		if t.tc.translated_files[file] {
			operand = t.tc.translated_promoted_shift_type(operand)
			if node.children_count > 0 {
				lhs_id := t.a.child(&node, 0)
				checked := t.tc.expr_type(lhs_id) or { t.tc.resolve_type(lhs_id) }
				promoted := t.tc.translated_promoted_shift_type(checked)
				// A literal's spelling can require a wider C type than the
				// transformer's generic `int` fallback.
				if promoted.is_integer() {
					operand = promoted
				}
			}
		}
		resolved_source_id := t.semantic_type_name(types.unsigned_shift_result_type(operand))
		resolved_comparison_type_id := resolved_source_id

		if resolved_comparison_type_id in [flat.source_type_u8, flat.source_type_u16,
			flat.source_type_u32, flat.source_type_u64, flat.source_type_usize] {
			return resolved_source_id
		}
	}
	if types.is_builtin_type_name(clean) {
		return t.semantic_type_name(types.unsigned_shift_result_type(types.builtin_type_value(clean)))
	}
	return typ_id
}

// promote_numeric_literal_infix_type returns the type of an arithmetic operation in which an
// untyped literal takes the type of the other operand: an unsigned type for an int literal
// that fits it, or f32 for an untyped float constant such as `1.2`, `-1.2` or `1.2 * 2`.
fn (t &Transformer) promote_numeric_literal_infix_type(lhs_id flat.NodeId, lhs_type u32, rhs_id flat.NodeId, rhs_type u32) ?u32 {
	lhs := t.a.nodes[int(lhs_id)]
	rhs := t.a.nodes[int(rhs_id)]
	if promoted := promote_int_literal_infix_type(lhs, rhs, rhs_type) {
		return promoted
	}
	if promoted := promote_int_literal_infix_type(rhs, lhs, lhs_type) {
		return promoted
	}
	if lhs_type == flat.source_type_f32 && t.is_untyped_float_literal_expr(rhs_id) {
		return flat.source_type_f32
	}
	if rhs_type == flat.source_type_f32 && t.is_untyped_float_literal_expr(lhs_id) {
		return flat.source_type_f32
	}
	return none
}

fn promote_int_literal_infix_type(lit flat.Node, other flat.Node, other_type u32) ?u32 {
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
	if text == '' {
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

fn unsigned_type_text_accepts_int_literal(typ u32, value int) bool {
	if value < 0 {
		return false
	}
	max := match typ {
		flat.source_type_u8 { 255 }
		flat.source_type_u16 { 65535 }
		flat.source_type_u32, flat.source_type_u64, flat.source_type_usize {
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
fn promote_numeric_stringify_type(a u32, b u32) u32 {
	if a == b {
		return a
	}
	if a == flat.source_type_f64 || b == flat.source_type_f64 {
		return flat.source_type_f64
	}
	if a == flat.source_type_f32 || b == flat.source_type_f32 {
		return flat.source_type_f32
	}
	ra := int_stringify_rank(a)
	rb := int_stringify_rank(b)
	if ra > rb {
		return a
	}
	if rb > ra {
		return b
	}
	if a == flat.source_type_int {
		return b
	}
	if b == flat.source_type_int {
		return a
	}
	return a
}

fn int_stringify_rank(typ u32) int {
	return match typ {
		flat.source_type_i8, flat.source_type_u8 { 8 }
		flat.source_type_i16, flat.source_type_u16 { 16 }
		flat.source_type_i32, flat.source_type_u32, flat.source_type_int, flat.source_type_rune {
			32
		}
		flat.source_type_i64, flat.source_type_u64, flat.source_type_isize, flat.source_type_usize {
			64
		}
		else { 32 }
	}
}

// is_numeric_stringify_type reports whether is numeric stringify type applies in transform.
fn (t &Transformer) is_numeric_stringify_type(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	is_number := typ in ['int', 'int literal', 'i8', 'i16', 'i32', 'i64', 'isize', 'usize', 'u8',
		'u16', 'u32', 'u64', 'f32', 'f64', 'float literal', 'rune']
	return is_number || typ in t.enum_types
}

// is_enum_stringify_type reports whether is enum stringify type applies in transform.
fn (t &Transformer) is_enum_stringify_type(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
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
	if alias_source_id := t.tc.type_aliases.type_id(clean_typ) {
		return t.is_enum_stringify_type(alias_source_id)
	}
	if qtyp != clean_typ {
		if alias_source_id := t.tc.type_aliases.type_id(qtyp) {
			return t.is_enum_stringify_type(alias_source_id)
		}
	}
	parsed := t.tc.parse_type(flat.source_type_id(clean_typ))
	if parsed is types.Enum {
		return true
	}
	if qtyp != clean_typ {
		qparsed := t.tc.parse_type(flat.source_type_id(qtyp))
		if qparsed is types.Enum {
			return true
		}
	}
	return false
}

// enum_str_method_name supports enum str method name handling for Transformer.
fn (t &Transformer) enum_str_method_name(typ_id u32) ?string {
	typ := flat.source_type_name(typ_id)
	mut candidates := []string{cap: 3}
	candidates << typ
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << '${t.cur_module}.${typ}'
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(typ_id)
		if parsed is types.Enum {
			candidates << parsed.name()
		}
	}
	for candidate in candidates {
		method := '${candidate}.str'
		if t.fn_ret_types.contains(method)
			|| (!isnil(t.tc) && method in t.tc.fn_param_types) {
			return method
		}
	}
	if !isnil(t.tc) {
		// Aliases inherit methods from intermediate aliases and their base enum.
		// Check those before selecting the generated enum string helper.
		return t.tc.concrete_method_signature_key(typ_id, 'str')
	}
	return none
}

// enum_autostr_call builds a call to the compiler-synthesized `<Enum>__autostr` helper
// (emitted by cgen's enum_str_defs) which returns the enum field NAME. Used as the default
// `${enum}` stringification when the user has not defined a custom `.str()` — V auto-derives
// one. Mirrors the struct-str qualification so the C name matches cgen's enum_decls naming.
fn (mut t Transformer) enum_autostr_call(expr flat.NodeId, typ_id u32) flat.NodeId {
	qualified := flat.source_type_name(t.enum_autostr_type_name(typ_id))
	helper := '${c_name(qualified)}__autostr'
	t.mark_fn_used_name(helper)
	return t.make_call_typed(helper, [expr], flat.source_type_string)
}

// enum_autostr_type_name names the enum whose `<Enum>__autostr` helper formats a value of
// the already resolved type `typ`. It deliberately ignores the current file's imports: a
// value returned by `real_a.make()` has type `a.Kind` even in a file that imports another
// module as `a`. Declaration spellings are resolved by `source_enum_type_name` first.
fn (t &Transformer) enum_autostr_type_name(typ_id u32) u32 {
	// Alias fields retain their alias spelling after checking, while the helper
	// belongs to the underlying enum declaration.
	typ := flat.source_type_name(t.alias_str_resolved_base_type(typ_id))
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
		return flat.source_type_id(short_name)
	}
	if !isnil(t.tc) && qualified !in t.tc.enum_names {
		if short_name in t.tc.enum_names {
			return flat.source_type_id(short_name)
		}
	}
	return flat.source_type_id(qualified)
}

// source_enum_type_name resolves an enum spelling written in `file` through that file's
// imports: a bare `Kind` through its selective imports, `token.Kind` through its import
// aliases (`import toml.token` makes it `toml.token.Kind`). Only raw declaration
// spellings may be resolved this way; see `enum_autostr_type_name`.
fn (t &Transformer) source_enum_type_name(file string, spelling_id u32) ?u32 {
	spelling := flat.source_type_name(spelling_id)
	if isnil(t.tc) || file.len == 0 || spelling_id == 0 {
		return none
	}
	resolved := if spelling.contains('.') {
		module_name := t.file_import_module(file, spelling.all_before('.')) or {
			return none
		}
		'${module_name}.${spelling.all_after('.')}'
	} else {
		flat.source_type_name(t.selective_import_type_name_for_file(file, spelling_id) or { return none })
	}
	if resolved in t.enum_types || resolved in t.tc.enum_names {
		return flat.source_type_id(resolved)
	}
	return none
}

// source_enum_elem_type resolves the enum at the core of an array element spelling
// (`token.Kind`, `[]token.Kind`) that `file` wrote, keeping any other spelling as is.
fn (t &Transformer) source_enum_elem_type(file string, spelling_id u32) u32 {
	spelling := flat.source_type_name(spelling_id)
	mut prefix_len := 0
	for spelling[prefix_len..].starts_with('[]') {
		prefix_len += 2
	}
	if resolved_source_id := t.source_enum_type_name(file, flat.source_type_id(spelling[prefix_len..])) {
		resolved := flat.source_type_name(resolved_source_id)

		return flat.source_type_id(spelling[..prefix_len] + resolved)
	}
	return spelling_id
}

// wrap_string_conversion transforms wrap string conversion data for transform.
fn (mut t Transformer) wrap_string_conversion(expr flat.NodeId, typ_id u32) flat.NodeId {
	typ := flat.source_type_name(typ_id)
	mut clean_type_id := typ_id
	is_ref := flat.source_type_shape(clean_type_id).has(.pointer) && !t.is_fixed_array_type(clean_type_id)
	if is_ref {
		clean_type_id = flat.source_type_id(flat.source_type_name(clean_type_id)[1..])
	}
	for flat.source_type_shape(clean_type_id).has(.shared_type) {
		clean_type_id = flat.source_type_id(trimmed_transform_text(flat.source_type_name(clean_type_id)[7..]))
	}
	for flat.source_type_shape(clean_type_id).has(.atomic_type) {
		clean_type_id = flat.source_type_id(trimmed_transform_text(flat.source_type_name(clean_type_id)[7..]))
	}
	if flat.source_type_name(clean_type_id).starts_with('builtin.') {
		clean_type_id = flat.source_type_id(flat.source_type_name(clean_type_id)['builtin.'.len..])
	}
	mut clean_typ := clean_type_id
	if flat.is_named_variant_type_name(flat.source_type_name(clean_typ)) {
		return t.named_variant_str(expr, clean_type_id, is_ref)
	}
	if flat.source_type_name(clean_typ) in stringify_narrow_integer_types {
		// An arithmetic node with a 128-bit operand resolves to the narrower side
		// here, so the printer picked for it showed the low 64 bits only. The value
		// itself is lowered correctly, so preferring the 128-bit side is enough.
		wide := t.stringify_wide_integer_operand(expr)
		if wide != 0 {
			clean_type_id = wide
			clean_typ = wide
		}
	}
	if map_typ := generic_map_type_arg_from_suffix(flat.source_type_name(clean_typ)) {
		return t.wrap_string_conversion(expr, (if is_ref {
			flat.source_type_id('&${map_typ}')
		} else {
			flat.source_type_id(map_typ)
		}))
	}
	if source_typ_source_id := t.source_type_name_from_c_name(clean_type_id) {
		return t.wrap_string_conversion(expr, source_typ_source_id)
	}
	if source_typ_source_id := t.fixed_array_source_type_from_c_name(clean_type_id) {
		return t.wrap_string_conversion(expr, source_typ_source_id)
	}
	if source_typ_source_id := t.generic_specialized_source_type_name(clean_type_id) {
		source_typ := flat.source_type_name(source_typ_source_id)

		if source_typ != flat.source_type_name(clean_typ) {
			return t.wrap_string_conversion(expr, source_typ_source_id)
		}
	}
	normalized_stringify_type_source_id := t.normalize_runtime_array_stringify_type(clean_type_id)
	normalized_stringify_type := flat.source_type_name(normalized_stringify_type_source_id)
	if normalized_stringify_type != flat.source_type_name(clean_typ) {
		return t.wrap_string_conversion(expr, normalized_stringify_type_source_id)
	}
	if t.is_fixed_array_type(clean_type_id) {
		if is_ref {
			return t.lower_ref_value_str(expr, typ_id, '&nil')
		}
		elem_type := fixed_array_elem_type_bytes(flat.source_type_name(clean_typ))
		arr := t.fixed_array_value_to_array(expr, clean_type_id, flat.source_type_id('[]${elem_type}'))
		return t.lower_fixed_array_str(arr, flat.source_type_id('[]${elem_type}'))
	}
	if t.is_optional_type_name(clean_type_id) {
		mut optional_expr := expr
		if is_ref {
			expr_node := t.a.nodes[int(expr)]
			if expr_node.kind == .prefix && expr_node.op == .amp && expr_node.children_count == 1 {
				// `&option` prints an address prefix around the option value. The option
				// conversion itself must still read the wrapper, rather than its C address.
				optional_expr = t.a.child(&expr_node, 0)
			}
		}
		optional_str := t.wrap_optional_string_conversion(optional_expr, clean_type_id, is_ref)
		return optional_str
	}
	if clean_typ == 0 || clean_typ == flat.source_type_unknown {
		inferred_source_id := t.resolve_expr_type(expr)
		inferred := flat.source_type_name(inferred_source_id)
		if inferred_source_id != 0 && inferred != flat.source_type_name(clean_typ) {
			return t.wrap_string_conversion(expr, inferred_source_id)
		}
	}
	// `string` is the builtin string in every module: a type name declared by a program
	// starts with a capital letter, so no alias, enum or function type below can be
	// spelled that way. Interpolated values are strings more often than anything else.
	if clean_typ == flat.source_type_string && !is_ref {
		return expr
	}
	mut local_struct_shadows_alias := false
	if !isnil(t.tc) {
		local_struct_shadows_alias = t.bare_struct_name_is_local_to_current_module(clean_typ)
		if !local_struct_shadows_alias {
			if alias_name_source_id, alias_source_id_2 := t.lookup_str_alias(clean_type_id) {
				return t.alias_str_wrap(expr, alias_name_source_id, alias_source_id_2, is_ref)
			}
			if alias_source_id := t.tc.type_aliases.type_id(flat.source_type_name(clean_typ)) {
				return t.alias_str_wrap(expr, clean_type_id, alias_source_id, is_ref)
			}
		}
		mut qtyp := flat.source_type_name(clean_typ)
		if !qtyp.contains('.') && t.cur_module.len > 0 {
			qtyp = '${t.cur_module}.${flat.source_type_name(clean_typ)}'
		}
		if alias_source_id := t.tc.type_aliases.type_id(qtyp) {
			return t.alias_str_wrap(expr, clean_type_id, alias_source_id, is_ref)
		}
		if flat.source_type_name(clean_typ).starts_with('main.') || flat.source_type_name(clean_typ).starts_with('builtin.') {
			short_typ := flat.source_type_name(clean_typ).all_after_first('.')
			if alias_source_id := t.tc.type_aliases.type_id(short_typ) {
				return t.alias_str_wrap(expr, flat.source_type_id(short_typ), alias_source_id, is_ref)
			}
		}
		if !flat.source_type_shape(clean_typ).has(.dotted) && !local_struct_shadows_alias {
			aliases := t.type_aliases_with_short_name(flat.source_type_name(clean_typ))
			if aliases.len > 0 {
				return t.alias_str_wrap(expr, clean_type_id, aliases[0].target, is_ref)
			}
		}
		if !local_struct_shadows_alias {
			parsed := t.tc.parse_type(clean_type_id)
			if parsed is types.FnType {
				return t.make_string_literal(typeof_display_type_text(flat.source_type_name(clean_typ)))
			}
			if parsed is types.MultiReturn {
				return t.lower_multi_return_str(expr, parsed, t.multi_return_type_name(parsed.types))
			}
			if parsed is types.Enum {
				if method := t.enum_str_method_name(clean_type_id) {
					return t.make_call_typed(method, [expr], flat.source_type_string)
				}
				return t.enum_autostr_call(expr, clean_type_id)
			}
		}
		if qtyp != flat.source_type_name(clean_typ) {
			qparsed := t.tc.parse_type(flat.source_type_id(qtyp))
			if qparsed is types.Enum {
				if method := t.enum_str_method_name(flat.source_type_id(qtyp)) {
					return t.make_call_typed(method, [expr], flat.source_type_string)
				}
				return t.enum_autostr_call(expr, flat.source_type_id(qtyp))
			}
		}
	}
	if is_ref && clean_typ == flat.source_type_string {
		return t.make_call_typed('ptr_str', [expr], flat.source_type_string)
	}
	if clean_typ == flat.source_type_string {
		return expr
	}
	if flat.source_type_name(clean_typ) == 'thread' || flat.source_type_shape(clean_typ).has(.thread_type) {
		payload := if flat.source_type_shape(clean_typ).has(.thread_type) {
			trimmed_transform_text(flat.source_type_name(clean_typ)[7..])
		} else {
			'void'
		}
		return t.make_string_literal('thread(${payload})')
	}
	if flat.source_type_shape(clean_typ).has(.channel_type) {
		channel_value := if is_ref { t.make_prefix(.mul, expr) } else { expr }
		return t.make_call_typed('v3_chan_str', [channel_value,
			t.make_string_literal(trimmed_transform_text(flat.source_type_name(clean_typ)[5..]))], flat.source_type_string)
	}
	if flat.source_type_shape(clean_typ).has(.function_compact) || flat.source_type_shape(clean_typ).has(.function_spaced) {
		return t.make_string_literal(stringify_fn_type_display(flat.source_type_name(clean_typ)))
	}
	if is_ref {
		expr_node := t.a.nodes[int(expr)]
		if expr_node.kind == .ident && t.string_interp_needs_value_read(expr_node.value, typ_id) {
			return t.wrap_string_conversion(t.make_prefix(.mul, expr), clean_type_id)
		}
		if flat.source_type_shape(clean_typ).has(.pointer) && t.multi_pointer_str_expands_value(clean_type_id) {
			return t.lower_ref_value_str(expr, typ_id,
				'&'.repeat(dump_pointer_depth(typ)) + 'nil')
		}
		if flat.source_type_shape(clean_typ).has(.array_type) || flat.source_type_shape(clean_typ).has(.map_type)
			|| t.is_fixed_array_type(clean_type_id) {
			return t.lower_ref_value_str(expr, typ_id, '&nil')
		}
		// A `&Struct`/`&SumType` stringifies to the pointee's `.str()` value: the custom
		// method when one exists (no `&` prefix, e.g. map/array elements), otherwise
		// `&nil` for a null pointer or `&` + the value's auto str. Other pointer kinds
		// (voidptr, `&int`, `&string`, ...) keep the raw ptr_str below.
		iface_source_id := t.resolve_interface_type_name(clean_type_id)

		if iface_source_id != 0 {
			return t.lower_ref_interface_str(expr, iface_source_id)
		}
		if aggregate_source_id := t.stringify_aggregate_type_name(clean_type_id) {
			return t.lower_ref_str(expr, aggregate_source_id)
		}
	}
	iface_name_source_id := t.resolve_interface_type_name(clean_type_id)
	iface_name := flat.source_type_name(iface_name_source_id)
	if !is_ref && iface_name_source_id != 0 {
		if 'str' in t.tc.interface_abstract_method_names(iface_name_source_id) {
			value := t.stable_transformed_expr_for_reuse(expr, clean_type_id, 'iface_str')
			method_name := '${iface_name}.str'
			t.mark_fn_used_name(method_name)
			t.mark_interface_method_implementers_used(iface_name_source_id, 'str')
			return t.make_call_typed(method_name, [t.make_prefix(.amp, value)], flat.source_type_string)
		}
	}
	if is_ref || clean_typ in [flat.source_type_voidptr, flat.source_type_byteptr,
		flat.source_type_charptr] {
		return t.make_call_typed('ptr_str', [expr], flat.source_type_string)
	}
	if flat.source_type_name(clean_typ) == 'IError' || flat.source_type_name(clean_typ) == 'builtin.IError' {
		return t.make_call_typed('IError.str', [expr], flat.source_type_string)
	}
	if iface_name_source_id != 0 {
		str_key := '${iface_name}.str'
		known_str := t.fn_ret_types.contains(str_key) || (!isnil(t.tc) && str_key in t.tc.fn_ret_types)
		if known_str {
			return t.make_call_typed(str_key, [expr], flat.source_type_string)
		}
		return t.lower_interface_auto_str(expr, iface_name_source_id)
	}
	if flat.source_type_shape(clean_typ).has(.array_type) || flat.source_type_shape(clean_typ).has(.map_type) {
		if method_name := t.resolve_receiver_method_for_type(clean_type_id, 'str') {
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, [expr], flat.source_type_string)
		}
	}
	match flat.source_type_name(clean_typ) {
		'bool' {
			return t.make_call_typed('bool.str', [expr], flat.source_type_string)
		}
		'u8', 'u16', 'u32', 'usize' {
			// C integer promotion would pass an untruncated `int` into the u64
			// param (`u8(255) + u8(1)` is 256 in C); cast back to the value
			// type first so the arithmetic wraps at the V type's width.
			truncated := t.make_cast(clean_type_id, expr, clean_type_id)
			return t.make_call_typed('strconv__format_uint', [truncated, t.make_int_literal(10)], flat.source_type_string)
		}
		'u64' {
			return t.make_call_typed('u64.str', [expr], flat.source_type_string)
		}
		'u128' {
			return t.make_call_typed('u128.str', [expr], flat.source_type_string)
		}
		'int', 'int literal' {
			return t.make_call_typed('int.str', [expr], flat.source_type_string)
		}
		'i8' {
			return t.make_call_typed('i8.str', [expr], flat.source_type_string)
		}
		'i16' {
			return t.make_call_typed('i16.str', [expr], flat.source_type_string)
		}
		'i32' {
			return t.make_call_typed('i32.str', [expr], flat.source_type_string)
		}
		'i64' {
			return t.make_call_typed('i64.str', [expr], flat.source_type_string)
		}
		'i128' {
			return t.make_call_typed('i128.str', [expr], flat.source_type_string)
		}
		'char' {
			return t.make_call_typed('v3_char_string', [
				t.make_cast(flat.source_type_int, expr, flat.source_type_int),
			], flat.source_type_string)
		}
		'isize' {
			return t.make_call_typed('strconv__format_int', [expr, t.make_int_literal(10)], flat.source_type_string)
		}
		'f32' {
			return t.make_call_typed('f32.str', [expr], flat.source_type_string)
		}
		'f64', 'float literal' {
			return t.make_call_typed('f64.str', [expr], flat.source_type_string)
		}
		else {
			if flat.source_type_name(clean_typ) in t.enum_types
				&& !local_struct_shadows_alias
				&& (!isnil(t.tc) || !t.bare_struct_name_is_local_to_current_module(clean_typ)) {
				if method := t.enum_str_method_name(clean_type_id) {
					return t.make_call_typed(method, [expr], flat.source_type_string)
				}
				return t.enum_autostr_call(expr, clean_type_id)
			}
			mut qenum := flat.source_type_name(clean_typ)
			if !flat.source_type_shape(clean_typ).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
				&& t.cur_module != 'builtin' {
				qenum = '${t.cur_module}.${flat.source_type_name(clean_typ)}'
			}
			if qenum in t.enum_types {
				if method := t.enum_str_method_name(flat.source_type_id(qenum)) {
					return t.make_call_typed(method, [expr], flat.source_type_string)
				}
				return t.enum_autostr_call(expr, flat.source_type_id(qenum))
			}
			if generic_str := t.generic_receiver_str_call(expr, clean_type_id) {
				return generic_str
			}
			if aggregate_type_source_id := t.stringify_aggregate_type_name(clean_type_id) {
				aggregate_type := flat.source_type_name(aggregate_type_source_id)

				qualified := aggregate_type
				if str_fn := t.aggregate_str_method_name(flat.source_type_id(qualified)) {
					t.mark_fn_used_name(str_fn)
					return t.make_call_typed(str_fn, [expr], flat.source_type_string)
				}
				if t.building_v && t.auto_str_synthesis_type_text() != qualified {
					return t.request_auto_str_helper(expr, aggregate_type_source_id)
				}
				_, sum_variants_source_ids := t.concrete_sum_name_and_variants(flat.source_type_id(qualified))

				if sum_variants_source_ids.len > 0 {
					return t.lower_sum_str(expr, flat.source_type_id(qualified))
				}
				if struct_str := t.lower_struct_str(expr, flat.source_type_id(qualified)) {
					return struct_str
				}
				return t.make_string_literal('${qualified}{}')
			} else if clean_typ != 0 && flat.source_type_shape(clean_typ).has(.array_type) {
				return t.lower_array_str(expr, clean_type_id)
			} else if clean_typ != 0 && flat.source_type_shape(clean_typ).has(.map_type) {
				return t.lower_map_str(expr, clean_type_id)
			} else if clean_typ == flat.source_type_rune {
				return t.make_call_typed('rune.str', [expr], flat.source_type_string)
			} else {
				return expr
			}
		}
	}
}

fn (mut t Transformer) multi_pointer_str_expands_value(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	mut base := trimmed_transform_text(typ)
	for base.starts_with('&') {
		base = trimmed_transform_text(base[1..])
	}
	base = flat.source_type_name(t.normalize_type_alias(flat.source_type_id(base)))
	return t.is_optional_type_name(flat.source_type_id(base)) || base.starts_with('[]') || base.starts_with('map[')
		|| t.is_fixed_array_type(flat.source_type_id(base)) || (t.resolve_interface_type_name(flat.source_type_id(base))) != 0
		|| t.is_enum_stringify_type(flat.source_type_id(base)) || t.stringify_aggregate_type_name(flat.source_type_id(base)) != none
}

fn (mut t Transformer) lower_interface_auto_str(expr flat.NodeId, iface_name_id u32) flat.NodeId {
	return t.lower_interface_auto_str_with_nil(expr, iface_name_id, true)
}

fn (mut t Transformer) lower_interface_auto_str_with_nil(expr flat.NodeId, iface_name_id u32, allow_nil bool) flat.NodeId {
	iface_name := flat.source_type_name(iface_name_id)
	if t.stringify_stack_count(iface_name_id) > 0
		|| (t.stringify_stack.len >= t.stringify_depth_cap
			&& !t.stringify_types_match(t.auto_str_synthesis_type, iface_name_id)) {
		return t.request_auto_str_helper(expr, iface_name_id)
	}
	t.stringify_stack << iface_name_id
	defer {
		t.stringify_stack.delete_last()
	}
	display_name := iface_name.all_after_last('.')
	if isnil(t.tc) {
		return t.make_string_literal('${display_name}{}')
	}
	t.refresh_interface_impl_indexes_for_boxed_types()
	value := t.stable_transformed_expr_for_reuse(expr, iface_name_id, 'iface_str')
	result_name := t.new_temp('iface_str')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_string_literal('unknown interface value'), flat.source_type_string)
	tag := t.make_selector(value, '_typ', flat.source_type_int)
	if allow_nil {
		object_is_nil := t.make_infix(.eq, t.make_selector(value, '_object', flat.source_type_voidptr), t.make_int_literal(0))
		tag_is_zero := t.make_infix(.eq, tag, t.make_int_literal(0))
		nil_cond := t.make_infix(.logical_and, tag_is_zero, object_is_nil)
		t.pending_stmts << t.make_if(nil_cond, t.make_block([
			t.make_assign(t.make_ident(result_name), t.make_string_literal('nil')),
		]), t.make_empty())
	}
	impl_names := if t.is_builtin_ierror_interface_name(iface_name_id) {
		t.tc.ierror_impl_names()
	} else {
		t.interface_impl_index_for_transform(iface_name_id).names.ids.clone()
	}
	for impl_id in impl_names {
		// A source-level box scan can only see the generic base (`Text`) for a
		// conversion that becomes `Text[int]` while a concrete default expression
		// is lowered. The base has no C runtime type and must not produce a dispatch
		// branch; the worker-local late-box index supplies the concrete application.
		if t.interface_auto_str_impl_is_open_generic(impl_id) {
			continue
		}
		mut concrete_types := []u32{}
		if impl_id in [flat.source_type_id('array'), flat.source_type_id('Array')] {
			for concrete in impl_names {
				if flat.source_type_shape(concrete).has(.array_type) {
					concrete_types << concrete
				}
			}
			if concrete_types.len > 0 {
				continue
			}
		}
		if concrete_types.len == 0 {
			// A comptime-generated method body can be lowered in a worker with no
			// boxed-type snapshot. In that case retain every known implementation:
			// dropping them all makes a live interface stringify as "unknown".
			if !t.interface_boxed_type_marked(iface_name_id, impl_id)
				&& (t.interface_boxed_types.len > 0 || t.interface_boxed_types_late.len > 0) {
				continue
			}
			concrete_types << impl_id
		}
		for concrete_type in concrete_types {
			type_id := t.interface_impl_type_id(iface_name_id, concrete_type) or {
				t.interface_impl_type_id(iface_name_id, impl_id) or { continue }
			}
			concrete_pointer_id := flat.source_type_id('&' + flat.source_type_name(concrete_type))
			object := t.make_cast(concrete_pointer_id, t.make_selector(value, '_object', flat.source_type_voidptr), concrete_pointer_id)
			concrete := t.make_prefix(.mul, object)
			t.set_node_typ(int(concrete), concrete_type)
			saved := t.pending_stmts.clone()
			t.pending_stmts.clear()
			inner_type := (if alias_target_source_id := t.tc.type_aliases.type_id(flat.source_type_name(concrete_type)) {
				alias_target_source_id_2 := alias_target_source_id
				alias_target := flat.source_type_name(alias_target_source_id_2)

				if alias_target.starts_with('fn(') || alias_target.starts_with('fn ') {
					alias_target_source_id_2
				} else {
					concrete_type
				}
			} else {
				concrete_type
			})
			wrapped := if t.stringify_type_at_circular_limit(inner_type) {
				t.make_string_literal('<circular>')
			} else {
				mut inner := if inner_type == flat.source_type_voidptr {
					t.mark_fn_used_name('voidptr.str')
					t.make_call_typed('voidptr.str', [concrete], flat.source_type_string)
				} else {
					t.wrap_string_conversion(concrete, inner_type)
				}
				quote_type_source_id := t.normalize_type_alias(inner_type)

				if quote_type_source_id == flat.source_type_string {
					inner = t.string_plus(t.string_plus(t.make_string_literal("'"), inner), t.make_string_literal("'"))
				} else if quote_type_source_id == flat.source_type_rune {
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
				object_addr := if flat.source_type_shape(t.normalize_type_alias(inner_type)).has(.array_type) {
					// Interface array values own a copied Array header. Track the backing
					// storage so a boxed copy of the array can still be recognized as circular.
					t.make_selector(concrete, 'data', flat.source_type_voidptr)
				} else {
					t.make_selector(value, '_object', flat.source_type_voidptr)
				}
				object_type := t.make_int_literal(t.type_index(inner_type, t.cur_module))
				mut live_body := [
					t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [
						object_addr,
						object_type,
					], flat.source_type_void)),
				]
				live_body << render_body
				live_body << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], flat.source_type_void))
				seen := t.make_call_typed('autostr_addr_type_in_stack', [object_addr, object_type], flat.source_type_bool)
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
	t.set_node_typ(int(result), flat.source_type_string)
	return result
}

fn (t &Transformer) interface_autostr_impl_needs_address_guard(typ_id u32) bool {
	clean_source_id := t.normalize_type_alias(typ_id)
	clean := flat.source_type_name(clean_source_id)
	if _ := t.stringify_aggregate_type_name(clean_source_id) {
		return true
	}
	return clean.starts_with('[]') || clean.starts_with('map[')
		|| clean.starts_with('chan ')
		|| (clean.starts_with('[') && clean.contains(']'))
		|| t.is_interface_type(clean_source_id)
}

fn (t &Transformer) interface_auto_str_impl_is_open_generic(name_id u32) bool {
	name := flat.source_type_name(name_id)
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

fn (mut t Transformer) lower_ref_interface_str(expr flat.NodeId, iface_name_id u32) flat.NodeId {
	iface_name := flat.source_type_name(iface_name_id)
	ptr_type := flat.source_type_id('&${iface_name}')
	ptr_name := t.new_temp('iface_ref_str_ptr')
	res_name := t.new_temp('iface_ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('&nil'), flat.source_type_string)
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), iface_name_id)
	inner := t.lower_interface_auto_str(value, iface_name_id)
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	then_body << t.make_assign(t.make_ident(res_name), t.string_plus(t.make_string_literal('&'), inner))
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	result := t.make_ident(res_name)
	t.set_node_typ(int(result), flat.source_type_string)
	return result
}

// lower_ref_str stringifies a `&Struct`/`&SumType` pointer with the same semantics V uses
// for top-level pointer interpolation: emit `&nil` for a null pointer or `&` plus the
// pointee's custom/automatic string for a live one. Container elements and struct fields
// use lower_ref_value_str, which intentionally omits the top-level prefix.
fn (mut t Transformer) lower_ref_str(expr flat.NodeId, aggregate_id u32) flat.NodeId {
	if str_fn := t.aggregate_str_method_name(aggregate_id) {
		t.mark_fn_used_name(str_fn)
		return t.lower_ref_str_guarded(expr, aggregate_id, true, str_fn, '&nil')
	}
	return t.lower_ref_str_prefixed(expr, aggregate_id)
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
	return receiver is types.Pointer || flat.source_type_shape(t.semantic_type_name(receiver)).has(.pointer)
}

fn (t &Transformer) aggregate_str_method_name(aggregate_id u32) ?string {
	aggregate := flat.source_type_name(aggregate_id)
	if aggregate.starts_with('C.') {
		method_name := '${aggregate}.str'
		method_suffix := '.${method_name}'
		for name, _ in t.fn_ret_types.ids {
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
	if method := t.resolve_receiver_method_for_type(aggregate_id, 'str') {
		if t.receiver_method_matches_type_name(method, aggregate_id) {
			return method
		}
	}
	// Imported declarations have a short convenience entry in the transformer's
	// struct table. When that short spelling is used through a selective import,
	// resolve its qualified owner before falling back to generated auto-str.
	if !aggregate.contains('.') && !t.bare_struct_name_is_local_to_current_module(aggregate_id)
		&& !t.bare_sum_type_name_is_local_to_current_module(aggregate_id) {
		if qualified_source_id := t.qualified_types.type_id(aggregate) {
			if method := t.resolve_receiver_method_for_type(qualified_source_id, 'str') {
				return method
			}
		}
	}
	c_name_fn := '${c_name(aggregate)}__str'
	v_name_fn := '${aggregate}.str'
	if t.fn_ret_types.contains(c_name_fn) || (!isnil(t.tc) && c_name_fn in t.tc.fn_ret_types) {
		return c_name_fn
	}
	if t.fn_ret_types.contains(v_name_fn) || (!isnil(t.tc) && v_name_fn in t.tc.fn_ret_types) {
		return v_name_fn
	}
	return none
}

// compare_helper_source_types preserves the spelling order of generated helper requests.
fn compare_helper_source_types(a &u32, b &u32) int {
	return flat.source_type_name(*a).compare(flat.source_type_name(*b))
}

fn auto_str_helper_name(aggregate_id u32) string {
	return '__v3_autostr_${c_name(flat.source_type_name(aggregate_id))}'
}

fn (t &Transformer) auto_str_helper_owner_module(aggregate_id u32) string {
	aggregate := flat.source_type_name(aggregate_id)
	base_name := aggregate.all_before('[')
	if !isnil(t.tc) {
		if module_name := t.tc.struct_modules[aggregate] {
			return module_name
		}
		if module_name := t.tc.type_alias_modules[aggregate] {
			return module_name
		}
		// Bare interfaces belong to main or builtin, even when a library
		// specialization requests their recursive stringification helper.
		if !base_name.contains('.') {
			for node in t.a.nodes {
				if node.kind != .interface_decl || node.value != base_name {
					continue
				}
				if source := t.a.source_files[node.pos.id] {
					// Generated helper contexts can revisit a source file in a foreign
					// namespace. The parsed file's module remains its declaration owner.
					for file_node in t.a.nodes {
						if file_node.kind == .file && file_node.children_count > 0
							&& (file_node.value == source.name
								|| t.a.real_source_path(file_node.value) == t.a.real_source_path(source.name)) {
							module_name := t.file_module_name(file_node)
							if module_name in ['', 'main', 'builtin'] {
								return if module_name.len > 0 { module_name } else { 'main' }
							}
						}
					}
					module_name := t.tc.file_modules[source.name] or {
						t.tc.file_modules[t.a.real_source_path(source.name)] or { continue }
					}
					if module_name in ['', 'main', 'builtin'] {
						return module_name
					}
				}
			}
		}
	}
	if !base_name.starts_with('C.') && base_name.contains('.') {
		return base_name.all_before_last('.')
	}
	return if t.cur_module.len > 0 { t.cur_module } else { 'main' }
}

fn (mut t Transformer) request_auto_str_helper(expr flat.NodeId, aggregate_id u32) flat.NodeId {
	helper := auto_str_helper_name(aggregate_id)
	if aggregate_id !in t.auto_str_types {
		t.auto_str_types[aggregate_id] = AutoStrRequest{
			module:        t.cur_module
			file:          t.cur_file
			// The helper name contains the fully qualified C type, while the module
			// assignment keeps its definition with the cached object that owns it.
			helper_module: t.auto_str_helper_owner_module(aggregate_id)
		}
	}
	t.mark_fn_used_name(helper)
	if t.stringify_stack.len > 0 || !t.expr_can_take_address(expr) {
		return t.make_call_typed(helper, [expr], flat.source_type_string)
	}
	value := t.stable_expr_for_reuse(expr)
	address := t.make_cast(flat.source_type_voidptr, t.make_prefix(.amp, value), flat.source_type_voidptr)
	address_type := t.make_int_literal(t.type_index(aggregate_id, t.cur_module))
	result_name := t.new_temp('autostr')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_string_literal('<circular>'), flat.source_type_string)
	live_body := t.make_block([
		t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [address, address_type], flat.source_type_void)),
		t.make_assign(t.make_ident(result_name), t.make_call_typed(helper, [value], flat.source_type_string)),
		t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], flat.source_type_void)),
	])
	seen := t.make_call_typed('autostr_addr_type_in_stack', [address, address_type], flat.source_type_bool)
	t.pending_stmts << t.make_if(seen, t.make_empty(), live_body)
	return t.make_ident(result_name)
}

fn (t &Transformer) has_pending_auto_str_helpers() bool {
	for name, _ in t.auto_str_types {
		if !t.auto_str_synthesized[name] && auto_str_helper_name(name) !in t.fn_ret_types.ids {
			return true
		}
	}
	return false
}

fn (mut t Transformer) synthesize_auto_str_helpers() []string {
	old_module := t.cur_module
	old_file := t.cur_file
	old_helper_module := t.auto_str_helper_module
	old_synthesis_type_source_id := t.auto_str_synthesis_type

	old_tc_module := if isnil(t.tc) { '' } else { t.tc.cur_module }
	old_tc_file := if isnil(t.tc) { '' } else { t.tc.cur_file }
	old_temp_counter := t.temp_counter
	was_log_active := t.used_fns_log_active
	log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	for {
		mut pending := []u32{}
		for name, _ in t.auto_str_types {
			if name in t.auto_str_synthesized {
				continue
			}
			if auto_str_helper_name(name) in t.fn_ret_types.ids {
				t.auto_str_synthesized[name] = true
				continue
			}
			pending << name
		}
		if pending.len == 0 {
			break
		}
		pending.sort_with_compare(compare_helper_source_types)
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
			// A helper is a function of its own: number its temporaries from zero,
			// not from whatever the transform lowered before it.
			t.temp_counter = 0
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
	t.temp_counter = old_temp_counter
	t.auto_str_helper_module = old_helper_module
	t.auto_str_synthesis_type = old_synthesis_type_source_id
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return new_names
}

fn (mut t Transformer) build_auto_str_helper_fn(aggregate_id u32) {
	helper := auto_str_helper_name(aggregate_id)
	saved_pending := t.pending_stmts
	saved_vars := t.var_types.clone()
	saved_fn_name := t.cur_fn_name
	saved_ret_type_source_id := t.cur_fn_ret_type

	t.pending_stmts = []flat.NodeId{}
	t.reset_var_types()
	t.cur_fn_name = helper
	t.set_cur_fn_ret_type(flat.source_type_string)
	param_name := '__auto_str_value'
	param := t.a.add_node(flat.Node{
		kind:  .param
		value: param_name
		typ:   aggregate_id
	})
	t.set_var_type(param_name, aggregate_id)
	value := t.make_ident(param_name)
	t.set_node_typ(int(value), aggregate_id)
	result := if t.is_interface_type(aggregate_id) {
		t.lower_interface_auto_str(value, aggregate_id)
	} else if t.sum_type_variants_for_index(aggregate_id).len > 0 {
		t.lower_sum_str(value, aggregate_id)
	} else {
		t.lower_struct_str(value, aggregate_id) or {
			t.make_string_literal('${struct_string_display_name(flat.source_type_name(aggregate_id))}{}')
		}
	}
	mut stmts := t.pending_stmts.clone()
	stmts << t.make_return(result, flat.source_type_string)
	t.pending_stmts = saved_pending
	t.restore_var_types(saved_vars)
	t.cur_fn_name = saved_fn_name
	t.set_cur_fn_ret_type(saved_ret_type_source_id)
	helper_module := if t.auto_str_helper_module.len > 0 {
		t.auto_str_helper_module
	} else {
		'main'
	}
	t.add_generated_fn_decl_context(helper_module)
	start := t.a.children.len
	t.a.children << param
	t.a.children << stmts
	fn_decl := t.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          helper
		typ:            flat.source_type_string
		children_start: i32(start)
		children_count: flat.child_count(1 + stmts.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, helper_module, t.cur_file)
	helper_key := if t.auto_str_helper_module !in ['', 'main', 'builtin'] {
		'${t.auto_str_helper_module}.${helper}'
	} else {
		helper
	}
	t.set_fn_ret_type(helper, flat.source_type_string)
	t.set_fn_ret_type(helper_key, flat.source_type_string)
	t.mark_fn_used_name(helper_key)
	if !isnil(t.tc) {
		t.tc.ensure_private_transform_signatures()
		t.tc.fn_ret_types[helper] = t.tc.parse_type(flat.source_type_string)
		t.tc.fn_ret_types[helper_key] = t.tc.parse_type(flat.source_type_string)
		t.tc.register_generated_fn_param_types(helper, [*t.tc.parse_type(aggregate_id)])
		t.tc.register_generated_fn_param_types(helper_key, [
			*t.tc.parse_type(aggregate_id),
		])
		t.tc.fn_variadic[helper] = false
		t.tc.fn_variadic[helper_key] = false
		t.tc_signature_names_log << helper
		if helper_key != helper {
			t.tc_signature_names_log << helper_key
		}
	}
}

fn (mut t Transformer) lower_ref_str_prefixed(expr flat.NodeId, aggregate_id u32) flat.NodeId {
	return t.lower_ref_str_guarded(expr, aggregate_id, true, '', '&nil')
}

fn (t &Transformer) checker_selected_custom_receiver_method(call_id flat.NodeId, method string) bool {
	resolved := t.checker_selected_receiver_method_name(call_id, method) or { return false }
	receiver := resolved.all_before_last('.')
	if receiver.len > 0 && t.is_interface_type(flat.source_type_id(receiver)) {
		return false
	}
	return true
}

fn (mut t Transformer) lower_ref_collection_str(expr flat.NodeId, collection_type_id u32) flat.NodeId {
	collection_type := flat.source_type_name(collection_type_id)
	ptr_type := flat.source_type_id('&${collection_type}')
	ptr_name := t.new_temp('ref_str_ptr')
	res_name := t.new_temp('ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('&nil'), flat.source_type_string)
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), collection_type_id)
	value_str := t.wrap_string_conversion(value, collection_type_id)
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	then_body << t.make_assign(t.make_ident(res_name), t.string_plus(t.make_string_literal('&'), value_str))
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) lower_ref_str_guarded(expr flat.NodeId, aggregate_id u32, prefix_non_nil bool, str_fn string, nil_text string) flat.NodeId {
	aggregate := flat.source_type_name(aggregate_id)
	ptr_type := flat.source_type_id('&${aggregate}')
	ptr_name := t.new_temp('ref_str_ptr')
	res_name := t.new_temp('ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal(nil_text), flat.source_type_string)
	// Stringifying the pointee may hoist its own prelude (nested arrays/maps dereference the
	// pointer). Capture that prelude and keep it inside the non-nil branch so a null pointer
	// is never dereferenced.
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value_str := if str_fn != '' && t.str_method_has_pointer_receiver(str_fn) {
		t.make_call_typed(str_fn, [t.make_ident(ptr_name)], flat.source_type_string)
	} else if str_fn != '' {
		value := t.make_prefix(.mul, t.make_ident(ptr_name))
		t.set_node_typ(int(value), aggregate_id)
		t.make_call_typed(str_fn, [value], flat.source_type_string)
	} else {
		t.wrap_string_conversion(t.make_prefix(.mul, t.make_ident(ptr_name)), aggregate_id)
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
	if str_fn == '' || str_fn.starts_with('__v3_autostr_') {
		address := t.make_cast(flat.source_type_voidptr, t.make_ident(ptr_name), flat.source_type_voidptr)
		address_type := t.make_int_literal(t.type_index(aggregate_id, t.cur_module))
		mut live_body := [
			t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [address, address_type], flat.source_type_void)),
		]
		live_body << then_body
		live_body << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], flat.source_type_void))
		circular_text := if prefix_non_nil { '&<circular>' } else { '<circular>' }
		seen_body := t.make_block([
			t.make_assign(t.make_ident(res_name), t.make_string_literal(circular_text)),
		])
		seen := t.make_call_typed('autostr_addr_type_in_stack', [address, address_type], flat.source_type_bool)
		then_body = [t.make_if(seen, seen_body, t.make_block(live_body))]
	}
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) lower_interface_smartcast_ref_str(expr flat.NodeId, interface_expr flat.NodeId, aggregate_id u32, str_fn string) flat.NodeId {
	aggregate := flat.source_type_name(aggregate_id)
	ptr_type := flat.source_type_id('&${aggregate}')
	ptr_name := t.new_temp('ref_str_ptr')
	boxed_name := t.new_temp('ref_str_boxed')
	res_name := t.new_temp('ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, ptr_type)
	t.set_var_type(ptr_name, ptr_type)
	boxed := t.make_selector_op(interface_expr, '_object_is_boxed', flat.source_type_bool, .dot)
	t.pending_stmts << t.make_decl_assign_typed(boxed_name, boxed, flat.source_type_bool)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('&nil'), flat.source_type_string)
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value_str := if t.str_method_has_pointer_receiver(str_fn) {
		t.make_call_typed(str_fn, [t.make_ident(ptr_name)], flat.source_type_string)
	} else {
		value := t.make_prefix(.mul, t.make_ident(ptr_name))
		t.set_node_typ(int(value), aggregate_id)
		t.make_call_typed(str_fn, [value], flat.source_type_string)
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

fn (mut t Transformer) lower_array_ref_str(expr flat.NodeId, typ_id u32) flat.NodeId {
	return t.lower_ref_value_str(expr, typ_id, 'nil')
}

fn (mut t Transformer) lower_ref_value_str(expr flat.NodeId, typ_id u32, nil_text string) flat.NodeId {
	return t.lower_ref_value_str_with_custom_prefix(expr, typ_id, nil_text, false)
}

fn (mut t Transformer) lower_ref_value_str_with_custom_prefix(expr flat.NodeId, typ_id u32, nil_text string, prefix_custom bool) flat.NodeId {
	typ := flat.source_type_name(typ_id)
	if !typ.starts_with('&') {
		return t.wrap_string_conversion(expr, typ_id)
	}
	elem_type := typ[1..]
	if alias_name_source_id_2, _ := t.lookup_str_alias(flat.source_type_id(elem_type)) {
		if str_fn := t.alias_custom_str_method_name(alias_name_source_id_2) {
			t.mark_fn_used_name(str_fn)
			return t.lower_ref_str_guarded(expr, alias_name_source_id_2, prefix_custom, str_fn, nil_text)
		}
	}
	mut normalized_elem := t.normalize_type_alias(flat.source_type_id(elem_type))
	if flat.source_type_name(normalized_elem).starts_with('builtin.') {
		normalized_elem = flat.source_type_id(flat.source_type_name(normalized_elem).all_after_last('.'))
	}
	if normalized_elem !in [flat.source_type_string, flat.source_type_rune, flat.source_type_bool,
		flat.source_type_i8, flat.source_type_i16, flat.source_type_i32, flat.source_type_i64,
		flat.source_type_int, flat.source_type_isize, flat.source_type_u8, flat.source_type_u16,
		flat.source_type_u32, flat.source_type_u64, flat.source_type_usize, flat.source_type_f32,
		flat.source_type_f64] {
		if aggregate_source_id := t.stringify_aggregate_type_name(flat.source_type_id(elem_type)) {
			if str_fn := t.aggregate_str_method_name(aggregate_source_id) {
				t.mark_fn_used_name(str_fn)
				return t.lower_ref_str_guarded(expr, aggregate_source_id, prefix_custom, str_fn, nil_text)
			}
		}
	}
	ptr_name := t.new_temp('arr_ref_str_ptr')
	res_name := t.new_temp('arr_ref_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, typ_id)
	t.set_var_type(ptr_name, typ_id)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal(nil_text), flat.source_type_string)
	saved := t.pending_stmts.clone()
	t.pending_stmts.clear()
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), flat.source_type_id(elem_type))
	mut value_str := if elem_type.starts_with('&') {
		nil_suffix := if nil_text.ends_with('0') { '0' } else { 'nil' }
		nested_nil := '&'.repeat(dump_pointer_depth(elem_type)) + nil_suffix
		t.lower_ref_value_str(value, flat.source_type_id(elem_type), nested_nil)
	} else {
		t.wrap_string_conversion(value, flat.source_type_id(elem_type))
	}
	mut quote_elem := t.normalize_type_alias(flat.source_type_id(elem_type))
	if flat.source_type_name(quote_elem).starts_with('builtin.') {
		quote_elem = flat.source_type_id(flat.source_type_name(quote_elem).all_after_last('.'))
	}
	if quote_elem == flat.source_type_string {
		value_str = t.string_plus(t.string_plus(t.make_string_literal("'"), value_str), t.make_string_literal("'"))
	} else if quote_elem == flat.source_type_rune {
		value_str = t.string_plus(t.string_plus(t.make_string_literal('`'), value_str), t.make_string_literal('`'))
	}
	mut then_body := []flat.NodeId{}
	t.drain_pending(mut then_body)
	t.pending_stmts = saved
	t.unset_var_type(ptr_name)
	then_body << t.make_assign(t.make_ident(res_name), t.string_plus(t.make_string_literal('&'), value_str))
	if _ := t.stringify_aggregate_type_name(flat.source_type_id(elem_type)) {
		address := t.make_cast(flat.source_type_voidptr, t.make_ident(ptr_name), flat.source_type_voidptr)
		address_type := t.make_int_literal(t.type_index(flat.source_type_id(elem_type), t.cur_module))
		mut live_body := [
			t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [address, address_type], flat.source_type_void)),
		]
		live_body << then_body
		live_body << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], flat.source_type_void))
		seen_body := t.make_block([
			t.make_assign(t.make_ident(res_name), t.make_string_literal('&<circular>')),
		])
		seen := t.make_call_typed('autostr_addr_type_in_stack', [address, address_type], flat.source_type_bool)
		then_body = [t.make_if(seen, seen_body, t.make_block(live_body))]
	}
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, t.make_block(then_body), t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) ref_value_str_reaches_large_circular_graph(elem_type_id u32) bool {
	if t.stringify_stack.len == 0 {
		return false
	}
	aggregate_source_id := t.stringify_aggregate_type_name(elem_type_id) or { return false }

	if t.stringify_expansion_estimate(aggregate_source_id) <= recursive_pointer_str_expansion_threshold {
		return false
	}
	mut seen := map[u32]bool{}
	if !t.stringify_type_reaches_stack(aggregate_source_id, mut seen) {
		return false
	}
	return !t.struct_autostr_allows_recurse(aggregate_source_id)
}

fn (t &Transformer) stringify_type_reaches_stack(typ_id u32, mut seen map[u32]bool) bool {
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	for {
		mut stripped := false
		for prefix in ['mut ', 'shared ', 'atomic ', '...', '[]', '?', '!', '&'] {
			if clean.starts_with(prefix) {
				clean = trimmed_transform_text(clean[prefix.len..])
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
			return t.stringify_type_reaches_stack(flat.source_type_id(clean[4..bracket_end]), mut seen)
				|| t.stringify_type_reaches_stack(flat.source_type_id(clean[bracket_end + 1..]), mut seen)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 0 && bracket_end < clean.len - 1 {
			return t.stringify_type_reaches_stack(flat.source_type_id(clean[bracket_end + 1..]), mut seen)
		}
	}
	if aggregate_source_id := t.stringify_aggregate_type_name(flat.source_type_id(clean)) {
		aggregate_source_id_3 := aggregate_source_id

		for active_id in t.stringify_stack {
			if t.stringify_types_match(aggregate_source_id_3, active_id) {
				return true
			}
		}
		if seen[aggregate_source_id_3] {
			return false
		}
		seen[aggregate_source_id_3] = true
		info := t.lookup_struct_info(aggregate_source_id_3) or {
			t.generic_struct_info_for_stringify(aggregate_source_id) or { return false }
		}
		for field in info.fields {
			field_type := (if field.typ != 0 {
				field.typ
			} else {
				field.raw_typ
			})
			if t.stringify_type_reaches_stack(field_type, mut seen) {
				return true
			}
		}
		return false
	}
	_, args, is_generic := generic_app_parts(clean)
	if is_generic {
		for arg in args {
			if t.stringify_type_reaches_stack(flat.source_type_id(arg), mut seen) {
				return true
			}
		}
	}
	return false
}

fn stringify_fn_type_display(typ string) string {
	clean := trimmed_transform_text(typ)
	if clean.starts_with('fn(') {
		return 'fn ${clean[2..]}'
	}
	return clean
}

// alias_str_wrap stringifies an alias value. V wraps the base str with `AliasName(...)` for
// aliases of aggregate types (arrays, maps, structs, sum types), e.g. `Block([1, 2])`,
// but stringifies primitive and enum aliases as the bare underlying value.
fn (mut t Transformer) alias_str_wrap(expr flat.NodeId, alias_name_id u32, base_type_id u32, is_ref bool) flat.NodeId {
	alias_name_comparison_type_id_3 := alias_name_id
	base_type := flat.source_type_name(base_type_id)
	if is_ref {
		if str_fn := t.alias_custom_str_method_name(alias_name_comparison_type_id_3) {
			return t.lower_ref_str_guarded(expr, alias_name_comparison_type_id_3, true, str_fn, '&nil')
		}
	}
	if custom := t.alias_custom_str_call(expr, alias_name_comparison_type_id_3) {
		return custom
	}
	if !is_ref {
		if declaring_alias_id, _ := t.alias_custom_str_method_in_chain(alias_name_id) {
			if custom := t.alias_custom_str_call(expr, declaring_alias_id) {
				return custom
			}
		}
		if base_type.starts_with('&') {
			pointee_alias := trimmed_transform_text(base_type[1..])
			if str_fn := t.alias_custom_str_method_name(flat.source_type_id(pointee_alias)) {
				return t.lower_ref_str_guarded(expr, flat.source_type_id(pointee_alias), true, str_fn, '&nil')
			}
		}
	}
	resolved_base_source_id := t.alias_str_option_base_type(base_type_id) or {
		t.alias_str_resolved_base_type(base_type_id)
	}
	resolved_base_source_id_2 := resolved_base_source_id
	resolved_base := flat.source_type_name(resolved_base_source_id_2)
	if is_ref && !t.is_optional_type_name(resolved_base_source_id)
		&& t.alias_str_needs_name_wrapper(base_type_id) {
		return t.lower_ref_str_guarded(expr, alias_name_comparison_type_id_3, false, '', '&nil')
	}
	inner_type := (if is_ref {
		flat.source_type_id('&${resolved_base}')
	} else {
		resolved_base_source_id_2
	})
	inner := t.wrap_string_conversion(expr, inner_type)
	if is_ref && t.is_optional_type_name(resolved_base_source_id) {
		return inner
	}
	if t.alias_str_suppress_wrapper_for_mut_param_deref(expr) {
		return inner
	}
	if t.is_fn_stringify_type(base_type_id) || t.is_fn_stringify_type(resolved_base_source_id)
		|| t.is_fn_stringify_type(alias_name_id) {
		return inner
	}
	if int(inner) >= 0 && int(inner) < t.a.nodes.len {
		inner_node := t.a.nodes[int(inner)]
		if inner_node.kind == .string_literal && inner_node.value.starts_with('fn') {
			return inner
		}
	}
	if !t.alias_str_needs_name_wrapper(base_type_id) {
		return inner
	}
	display := t.alias_string_display_name(alias_name_comparison_type_id_3, base_type_id)
	return t.string_plus(t.string_plus(t.make_string_literal('${display}('), inner), t.make_string_literal(')'))
}

fn (t &Transformer) is_fn_stringify_type(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	short := short_name_view(clean)
	return clean.starts_with('fn(') || clean.starts_with('fn (') || clean.starts_with('_fn_ptr_')
		|| short.starts_with('_fn_ptr_') || t.is_fn_pointer_type_name(flat.source_type_id(clean))
}

fn (t &Transformer) fn_stringify_display(typ_id u32) string {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if clean.starts_with('_fn_ptr_') || short_name_view(clean).starts_with('_fn_ptr_') {
		return 'fn'
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(flat.source_type_id(clean))
		if parsed is types.Alias && parsed.base_type is types.FnType {
			return t.fn_stringify_display(t.semantic_type_name(parsed.base_type))
		}
	}
	return typeof_fn_type_display(clean)
}

// alias_custom_str_call builds a call to the alias's own str() method when one exists;
// an alias-level str() overrides the base type's stringification entirely (no name wrapper).
fn (mut t Transformer) alias_custom_str_call(expr flat.NodeId, alias_name u32) ?flat.NodeId {
	str_fn := t.alias_custom_str_method_name(alias_name) or { return none }
	mut receiver := expr
	if t.str_method_has_pointer_receiver(str_fn) {
		mut expr_type := t.node_type(expr)
		if expr_type == 0 {
			expr_type = t.resolve_expr_type(expr)
		}
		if !flat.source_type_shape(expr_type).has(.pointer) && !t.expr_can_take_address(expr) {
			tmp_name := t.new_temp('alias_str')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, expr, alias_name)
			receiver = t.make_ident(tmp_name)
		}
	}
	return t.make_generic_str_method_call(str_fn, generic_fn_decl_base_value(str_fn), [receiver])
}

fn (mut t Transformer) make_generic_str_method_call(method_name string, decl_key string, args []flat.NodeId) flat.NodeId {
	call := t.make_call_typed(method_name, args, flat.source_type_string)
	generic_args_source_ids := t.recorded_generic_specialization_args(method_name) or { return call }
	generic_args := flat.TypeTextList{ ids: generic_args_source_ids }.names()
	if generic_args_source_ids.len == 0 || t.generic_args_have_placeholders(generic_args_source_ids) {
		return call
	}
	t.set_node_value(int(call), generic_args.join(', '))
	t.generic_call_spec_cache[int(call)] = GenericCallSpec{
		decl_key: decl_key
		args:     flat.TypeTextList{ ids: generic_args_source_ids.clone() }
	}
	return call
}

fn (mut t Transformer) alias_custom_str_method_name(alias_name_id u32) ?string {
	alias_name := flat.source_type_name(alias_name_id)
	if isnil(t.tc) {
		return none
	}
	mut candidates := [alias_name_id]
	if !alias_name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		candidates << flat.source_type_id('${t.cur_module}.${alias_name}')
	}
	for qname_id in candidates {
		qname := flat.source_type_name(qname_id)
		mut alias_target := t.tc.type_aliases.type_id(qname) or { u32(0) }
		if alias_target == 0 {
			alias_target = t.expand_generic_type_alias(qname_id) or { u32(0) }
		}
		if alias_target == 0 {
			continue
		}
		str_fn := '${c_name(qname)}__str'
		v_str_fn := '${qname}.str'
		if method_name, generic_args_source_ids_2, _ := t.generic_str_method_specialization(v_str_fn,
			qname_id, alias_target) {
			t.mark_generic_str_method_specialization(method_name, generic_args_source_ids_2)
			return method_name
		}
		known := t.fn_ret_types.contains(str_fn) || t.fn_ret_types.contains(v_str_fn)
			|| (!isnil(t.tc) && (str_fn in t.tc.fn_ret_types || v_str_fn in t.tc.fn_ret_types))
		if known {
			t.mark_fn_used_name(v_str_fn)
			t.mark_fn_used_name(str_fn)
			// Preserve the declared method key so codegen applies its method name mapping.
			if t.fn_ret_types.contains(v_str_fn) || v_str_fn in t.tc.fn_ret_types {
				return v_str_fn
			}
			return str_fn
		}
	}
	return none
}

// lookup_str_alias resolves a type name to `(alias_name, base_type)` when it names a type
// alias, mirroring the direct/module-qualified/suffix lookups of wrap_string_conversion.
fn (t &Transformer) lookup_str_alias(clean_typ_id u32) ?(u32, u32) {
	if isnil(t.tc) || clean_typ_id == 0 || isnil(t.str_alias_cache) {
		return t.lookup_str_alias_keyed(clean_typ_id)
	}
	mut cache := t.str_alias_cache
	mut recent := cache.recent_cache()
	state, cached := recent.get(t.cur_module, clean_typ_id, '')
	if state > 0 { return clean_typ_id, cached }
	if state < 0 { return none }
	if name, target := t.lookup_str_alias_keyed(clean_typ_id) {
		recent.put(t.cur_module, clean_typ_id, '', 1, target)
		return name, target
	}
	recent.put(t.cur_module, clean_typ_id, '', -1, 0)
	return none
}

fn (t &Transformer) lookup_str_alias_keyed(clean_typ_id u32) ?(u32, u32) {
	if isnil(t.tc) || clean_typ_id == 0 { return none }
	if isnil(t.str_alias_cache) {
		target := t.lookup_str_alias_uncached(clean_typ_id) or { return none }
		return clean_typ_id, target
	}
	mut cache := t.str_alias_cache
	key := cache.key(t.cur_module, clean_typ_id)
	if target := cache.entries[key] { return clean_typ_id, target }
	if cache.misses[key] { return none }
	if target := t.lookup_str_alias_uncached(clean_typ_id) {
		cache.entries[key] = target
		return clean_typ_id, target
	}
	cache.misses[key] = true
	return none
}

fn (t &Transformer) lookup_str_alias_uncached(clean_typ_id u32) ?u32 {
	clean_typ := flat.source_type_name(clean_typ_id)
	if alias_source_id := t.tc.type_aliases.type_id(clean_typ) {
		return alias_source_id
	}
	if alias_source_id := t.expand_generic_type_alias(clean_typ_id) {
		return alias_source_id
	}
	if !flat.source_type_shape(clean_typ_id).has(.dotted) {
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qtyp := '${t.cur_module}.${clean_typ}'
			if alias_source_id := t.tc.type_aliases.type_id(qtyp) {
				return alias_source_id
			}
		}
		for aname, target_id in t.tc.type_aliases.ids {
			if short_name_view(aname) == clean_typ {
				return target_id
			}
		}
	}
	return none
}

fn (t &Transformer) array_alias_stringify_type(typ_id u32) ?u32 {
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	is_ref := clean.starts_with('&')
	if is_ref {
		clean = clean[1..]
	}
	alias_name_source_id_3, base_type_source_id_2 := t.lookup_str_alias(flat.source_type_id(clean)) or { return none }
	alias_name_source_id_7 := alias_name_source_id_3
	alias_name := flat.source_type_name(alias_name_source_id_7)
	base_type := flat.source_type_name(base_type_source_id_2)

	base_clean := trimmed_transform_text(base_type)
	if base_clean.starts_with('[]') || base_clean.starts_with('map[') {
		return if is_ref { flat.source_type_id('&${alias_name}') } else { alias_name_source_id_7 }
	}
	resolved_shape := flat.source_type_shape(t.alias_str_resolved_base_type(base_type_source_id_2))
	if resolved_shape.has(.array_type) || resolved_shape.has(.map_type) {
		return if is_ref { flat.source_type_id('&${alias_name}') } else { alias_name_source_id_7 }
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

fn (t &Transformer) alias_str_resolved_base_type(base_type_id u32) u32 {
	mut clean := t.trimmed_type_id(base_type_id)
	mut seen := []u32{}
	for clean != 0 && clean !in seen {
		seen << clean
		next := t.trimmed_type_id(t.normalize_type_alias(clean))
		if next == clean { break }
		clean = next
	}
	return clean
}

// alias_str_option_base_type peels aliases around an option without resolving aliases in its
// payload, since they may provide a custom `str()` method.
fn (t &Transformer) alias_str_option_base_type(base_type_id u32) ?u32 {
	mut clean := t.trimmed_type_id(base_type_id)
	mut seen := []u32{}
	for clean != 0 && clean !in seen {
		if t.is_optional_type_name(clean) { return clean }
		seen << clean
		_, next := t.lookup_str_alias(clean) or { return none }
		clean = t.trimmed_type_id(next)
	}
	return none
}

// optional_payload_alias_display_name returns the name an alias payload prints
// itself with inside an option. A bare `'${x}'` on an alias of a primitive shows
// just the underlying value, but the same value held in an option keeps the alias
// name -- `Option(MyByte(0))`, not `Option(0)` -- so the wrapper the plain path
// suppresses has to be added back here.
fn (mut t Transformer) optional_payload_alias_display_name(typ_id u32) ?string {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if isnil(t.tc) || clean.len == 0 || clean.starts_with('&') {
		return none
	}
	if t.bare_struct_name_is_local_to_current_module(flat.source_type_id(clean)) {
		return none
	}
	alias_name_source_id_4, base_source_id := t.lookup_str_alias(flat.source_type_id(clean)) or { return none }

	if _ := t.alias_custom_str_method_name(alias_name_source_id_4) {
		return none
	}
	resolved_comparison_type_id_3 := t.alias_str_resolved_base_type(base_source_id)

	if resolved_comparison_type_id_3 !in [flat.source_type_string, flat.source_type_bool,
		flat.source_type_rune, flat.source_type_char, flat.source_type_i8, flat.source_type_i16,
		flat.source_type_i32, flat.source_type_i64, flat.source_type_int, flat.source_type_isize,
		flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
		flat.source_type_usize, flat.source_type_f32, flat.source_type_f64] {
		return none
	}
	return t.alias_string_display_name(alias_name_source_id_4, base_source_id)
}

fn (t &Transformer) alias_str_needs_name_wrapper(base_type_id u32) bool {
	mut clean := t.alias_str_resolved_base_type(base_type_id)
	if flat.source_type_shape(clean).has(.pointer) {
		return false
	}
	// An alias of an option prints as the option it is: `type MaybeAorB = ?AorB`
	// shows `Option(AorB(1))`. Wrapping the alias name around it would both hide
	// the payload's own type name and claim a level of nesting the value does not
	// have (`MaybeAorB(Option(1))`).
	if t.is_optional_type_name(clean) {
		return false
	}
	if flat.source_type_name(clean).starts_with('builtin.') {
		clean = flat.source_type_id(flat.source_type_name(clean).all_after_last('.'))
	}
	if t.is_fn_stringify_type(clean) {
		return false
	}
	if t.is_enum_stringify_type(clean) {
		return false
	}
	return flat.source_type_name(clean) !in ['string', 'bool', 'rune', 'char', 'i8', 'i16', 'i32',
		'i64', 'int', 'isize', 'u8', 'u16', 'u32', 'u64', 'usize', 'f32', 'f64', 'int literal',
		'float literal', 'voidptr', 'byteptr', 'charptr', 'nil', 'void']
}

fn (mut t Transformer) lower_struct_str(expr flat.NodeId, struct_type_id u32) ?flat.NodeId {
	struct_type := flat.source_type_name(struct_type_id)
	info := t.lookup_struct_info(struct_type_id) or {
		t.generic_struct_info_for_stringify(struct_type_id) or { return none }
	}
	if struct_type.contains('.') && struct_type.all_after_last('.') == 'Array_string' {
		return t.make_string_literal('${struct_string_display_name(struct_type)}{}')
	}
	stack_count := t.stringify_stack_count(struct_type_id)
	recurse_limit := if t.struct_autostr_allows_recurse(struct_type_id) { 3 } else { 2 }
	if stack_count >= recurse_limit {
		return t.make_string_literal('<circular>')
	}
	if t.stringify_stack.len >= t.stringify_depth_cap && stack_count == 0 && !info.is_c_anon
		&& !t.stringify_types_match(t.auto_str_synthesis_type, struct_type_id) {
		return t.request_auto_str_helper(expr, struct_type_id)
	}
	t.stringify_stack << struct_type_id
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
		if pointer_type == 0 {
			pointer_type = flat.source_type_id('&${struct_type}')
		}
		deref_address_source = t.stable_transformed_expr_for_reuse(pointer_id, pointer_type, 'struct_str_ptr')
		value_expr = t.make_prefix(.mul, deref_address_source)
		t.set_node_typ(int(value_expr), struct_type_id)
	}
	base := t.stable_transformed_expr_for_reuse(value_expr, struct_type_id, 'struct_str')
	guard_root_address := stack_count == 0 && t.expr_can_take_address(base)
	if guard_root_address {
		mut address_source := t.make_prefix(.amp, base)
		if expr_node.kind == .ident
			&& ((t.pointer_value_rvalues[expr_node.value]
				&& flat.source_type_shape(t.var_type(expr_node.value)).has(.pointer)) || t.mut_param_values[expr_node.value]) {
			address_source = t.transform_expr_preserving_pointer_value(expr)
		} else if int(deref_address_source) >= 0 {
			address_source = deref_address_source
		}
		address := t.make_cast(flat.source_type_voidptr, address_source, flat.source_type_voidptr)
		address_type := t.make_int_literal(t.type_index(struct_type_id, t.cur_module))
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [
			address,
			address_type,
		], flat.source_type_void))
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
		raw_field_type := (if field.raw_typ != 0 {
			field.raw_typ
		} else {
			field.typ
		})
		mut field_type := t.lookup_struct_field_type(struct_type_id, field.name) or {
			t.normalize_type_in_module(raw_field_type, info.module)
		}
		if field_type == 0 {
			field_type = field.typ
			if field_type == 0 {
				continue
			}
		}
		mut field_str := if flat.source_type_name(field_type) == struct_type {
			t.make_string_literal('${struct_string_display_name(flat.source_type_name(field_type))}{}')
		} else {
			t.struct_field_str_value(t.make_selector(base, field.name, field_type), raw_field_type,
				field_type, struct_type.starts_with('C.') || info.is_c_anon)
		}
		if raw_field_type == flat.source_type_string || flat.source_type_name(raw_field_type) == 'builtin.string' {
			field_str = t.string_plus(t.string_plus(t.make_string_literal("'"), field_str), t.make_string_literal("'"))
		}
		if t.struct_str_field_needs_indent(raw_field_type) {
			t.mark_fn_used_name('string.replace')
			t.mark_fn_used_name('string__replace')
			field_str = t.make_call_typed('string.replace', [field_str, t.make_string_literal('\n'),
				t.make_string_literal('\n    ')], flat.source_type_string)
		}
		result = t.string_plus(result, t.make_string_literal('    ${field.name}: '))
		result = t.string_plus(result, field_str)
		result = t.string_plus(result, t.make_string_literal('\n'))
	}
	final_result := t.string_plus(result, t.make_string_literal('}'))
	if guard_root_address {
		result_name := t.new_temp('struct_str')
		t.pending_stmts << t.make_decl_assign_typed(result_name, final_result, flat.source_type_string)
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], flat.source_type_void))
		return t.make_ident(result_name)
	}
	return final_result
}

fn auto_str_field_is_skipped(meta FieldDeclMeta) bool {
	for attr in comptime_attribute_metas_from_raw(meta.attrs, []int{}) {
		if attr.name == 'str' && attr.has_arg && trimmed_transform_text(attr.arg) == 'skip' {
			return true
		}
	}
	return false
}

fn (t &Transformer) stringify_stack_count(typ_id u32) int {
	mut n := 0
	for item_id in t.stringify_stack {
		if t.stringify_types_match(item_id, typ_id) {
			n++
		}
	}
	return n
}

fn (t &Transformer) stringify_types_match(a_id u32, b_id u32) bool {
	ak_id := t.stringify_type_key(a_id)
	bk_id := t.stringify_type_key(b_id)
	if ak_id == bk_id {
		return true
	}
	return flat.source_type_name(ak_id).all_after_last('.') == flat.source_type_name(bk_id).all_after_last('.')
}

fn (t &Transformer) stringify_type_key(typ_id u32) u32 {
	mut clean_id := t.trimmed_type_id(typ_id)
	for flat.source_type_shape(clean_id).has(.pointer) {
		clean_id = t.trimmed_type_id(t.trim_pointer_type(clean_id))
	}
	clean_id = t.normalize_type_alias(clean_id)
	if aggregate_id := t.stringify_aggregate_type_name(clean_id) {
		return aggregate_id
	}
	return clean_id
}

fn (t &Transformer) stringify_type_at_circular_limit(typ_id u32) bool {
	aggregate_source_id := t.stringify_aggregate_type_name(typ_id) or { return false }

	limit := if t.struct_autostr_allows_recurse(aggregate_source_id) { 3 } else { 2 }
	return t.stringify_stack_count(aggregate_source_id) >= limit
}

fn (mut t Transformer) rebuild_struct_autostr_recurse_index() {
	mut allow_nodes := map[int]bool{}
	for node_id in 0 .. t.a.nodes.len {
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[node_id] }
		if node.kind != .directive || !node.value.starts_with('@attributes:') {
			continue
		}
		mut allows_recurse := false
		for attr in node.generic_params() {
			clean := trimmed_transform_text(attr)
			if clean.starts_with('autostr') && clean.contains('allowrecurse') {
				allows_recurse = true
				break
			}
		}
		if allows_recurse {
			allow_nodes[node.value['@attributes:'.len..].int()] = true
		}
	}
	mut recurse_types := map[u32]bool{}
	if allow_nodes.len == 0 {
		// No struct allows recursion; there is nothing to look up below.
		t.struct_autostr_recurse_types = recurse_types.move()
		return
	}
	mut cur_module := ''
	for idx in 0 .. t.a.nodes.len {
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[idx] }
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
		recurse_types[flat.source_type_id(node.value)] = true
		recurse_types[flat.source_type_id(qualified)] = true
	}
	t.struct_autostr_recurse_types = recurse_types.move()
}

fn (t &Transformer) struct_autostr_allows_recurse(struct_type_id u32) bool {
	struct_type := flat.source_type_name(struct_type_id)
	decl_name := generic_base_name_text(struct_type)
	return t.struct_autostr_recurse_types[flat.source_type_id(decl_name)]
		|| t.struct_autostr_recurse_types[flat.source_type_id(decl_name.all_after_last('.'))]
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

fn (t &Transformer) alias_string_display_name(alias_name_id u32, base_type_id u32) string {
	alias_name := flat.source_type_name(alias_name_id)
	if !alias_name.contains('.') && t.cur_module !in ['', 'main', 'builtin'] {
		qualified := '${t.cur_module}.${alias_name}'
		if !isnil(t.tc) && t.tc.type_aliases.contains(qualified) {
			return qualified
		}
	}
	if !alias_name.contains('.') && !isnil(t.tc) {
		resolved_base := flat.source_type_name(t.normalize_type_alias(base_type_id))
		mut matched := ''
		for candidate, target_id in t.tc.type_aliases.ids {
			if candidate.all_after_last('.') != alias_name
				|| flat.source_type_name(t.normalize_type_alias(target_id)) != resolved_base {
				continue
			}
			if matched.len > 0 && matched != candidate {
				return struct_string_display_name(alias_name)
			}
			matched = candidate
		}
		if matched.len > 0 {
			return struct_string_display_name(matched)
		}
	}
	return struct_string_display_name(alias_name)
}

// struct_field_str_value stringifies one struct field for the auto-generated struct str.
// Unlike top-level stringification, V wraps an alias-typed field as `AliasName(value)` even
// when the alias base is primitive (`d: Duration(42)`), unless the alias defines its own
// str() method, which is used bare.
fn (mut t Transformer) struct_field_str_value(expr flat.NodeId, raw_field_type_id u32, field_type_id u32, c_owner bool) flat.NodeId {
	raw_field_type := flat.source_type_name(raw_field_type_id)

	mut clean := trimmed_transform_text(raw_field_type)
	if clean.starts_with('&') {
		if c_owner {
			pointer_depth := dump_pointer_depth(clean)
			mut elem_type := trimmed_transform_text(clean[pointer_depth..])
			elem_type = flat.source_type_name(t.normalize_type_alias(flat.source_type_id(elem_type)))
			if elem_type.starts_with('builtin.') {
				elem_type = elem_type.all_after_last('.')
			}
			if pointer_depth == 1 && elem_type == 'char' {
				return t.string_plus(t.make_string_literal('&'), t.lower_charptr_struct_field_str(expr))
			}
			if pointer_depth > 1 || elem_type in ['u8', 'voidptr'] {
				address := t.make_call_typed('ptr_str', [expr], flat.source_type_string)
				return t.string_plus(t.make_string_literal('&'), address)
			}
		}
		return t.lower_ref_value_str_with_custom_prefix(expr, field_type_id, '&nil', true)
	}
	if clean == 'charptr' || clean == 'builtin.charptr' {
		return t.lower_charptr_struct_field_str(expr)
	}
	// Function types retain source-only parameter metadata such as `mut` and
	// parameter names in StructField.raw_typ_text(). The semantic FnType intentionally
	// drops that metadata for ABI checks, so format the raw declaration here.
	if t.is_fn_stringify_type(flat.source_type_id(clean)) {
		if alias_name_source_id_5, base_type_source_id_3 := t.lookup_str_alias(flat.source_type_id(clean)) {
			if t.normalize_type_alias(t.alias_str_resolved_base_type(base_type_source_id_3)) == t.normalize_type_alias(field_type_id) {
				if custom := t.alias_custom_str_call(expr, alias_name_source_id_5) {
					return custom
				}
			}
		}
		return t.make_string_literal(t.fn_stringify_display(flat.source_type_id(clean)))
	}
	if clean.starts_with('?') || clean.starts_with('!') || clean.starts_with('shared ') {
		if clean.starts_with('?')
			&& flat.source_type_shape(t.optional_base_type(t.qualify_optional_type(field_type_id))).has(.pointer) {
			return t.wrap_optional_string_conversion_with_pointer_none(expr, field_type_id, false,
				false)
		}
		return t.wrap_string_conversion(expr, field_type_id)
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	alias_name_source_id_6, base_type_source_id_4 := t.lookup_str_alias(flat.source_type_id(clean)) or {
		return t.wrap_string_conversion(expr, field_type_id)
	}
	if t.normalize_type_alias(t.alias_str_resolved_base_type(base_type_source_id_4)) != t.normalize_type_alias(field_type_id) {
		// A bare alias spelling can be shared by several modules (`gfx.Color` and
		// `gg.Color`). Ignore a short-name alias that does not describe this field's
		// declaring-module-resolved type.
		return t.wrap_string_conversion(expr, field_type_id)
	}
	if custom := t.alias_custom_str_call(expr, alias_name_source_id_6) {
		return custom
	}
	resolved_base_source_id := t.alias_str_resolved_base_type(base_type_source_id_4)

	inner := t.wrap_string_conversion(expr, resolved_base_source_id)
	if t.is_fn_stringify_type(base_type_source_id_4) || t.is_fn_stringify_type(resolved_base_source_id)
		|| t.is_fn_stringify_type(alias_name_source_id_6) {
		return inner
	}
	if int(inner) >= 0 && int(inner) < t.a.nodes.len {
		inner_node := t.a.nodes[int(inner)]
		if inner_node.kind == .string_literal && inner_node.value.starts_with('fn') {
			return inner
		}
	}
	display := t.alias_string_display_name(alias_name_source_id_6, base_type_source_id_4)
	return t.string_plus(t.string_plus(t.make_string_literal('${display}('), inner), t.make_string_literal(')'))
}

fn (mut t Transformer) lower_charptr_struct_field_str(expr flat.NodeId) flat.NodeId {
	ptr_name := t.new_temp('charptr_str_ptr')
	res_name := t.new_temp('charptr_str_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, flat.source_type_charptr)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('C""'), flat.source_type_string)
	t.mark_fn_used_name('charptr.vstring')
	t.mark_fn_used_name('charptr__vstring')
	raw := t.make_call_typed('charptr.vstring', [t.make_ident(ptr_name)], flat.source_type_string)
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
fn (mut t Transformer) struct_str_field_needs_indent(field_type_id u32) bool {
	field_type := flat.source_type_name(field_type_id)
	mut clean := trimmed_transform_text(field_type)
	for clean.starts_with('&') || clean.starts_with('?') || clean.starts_with('!') {
		clean = trimmed_transform_text(clean[1..])
	}
	for clean.starts_with('shared ') {
		clean = trimmed_transform_text(clean[7..])
	}
	if clean.starts_with('builtin.') {
		clean = clean.all_after_last('.')
	}
	// Fixed-array auto-str already uses the legacy field indentation level for its
	// elements. Re-indenting it here would add four spaces that V1 does not emit.
	if t.is_fixed_array_type(flat.source_type_id(clean)) {
		return false
	}
	if _, _ := t.lookup_str_alias(flat.source_type_id(clean)) {
		return false
	}
	resolved_source_id := t.alias_str_resolved_base_type(flat.source_type_id(clean))
	resolved := flat.source_type_name(resolved_source_id)
	if !t.alias_str_needs_name_wrapper(resolved_source_id) {
		return false
	}
	if resolved in t.enum_types {
		return false
	}
	if !resolved.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && '${t.cur_module}.${resolved}' in t.enum_types {
		return false
	}
	if aggregate_source_id := t.stringify_aggregate_type_name(resolved_source_id) {
		aggregate := flat.source_type_name(aggregate_source_id)

		str_fn := '${c_name(aggregate)}__str'
		v_str_fn := '${aggregate}.str'
		if t.fn_ret_types.contains(str_fn) || t.fn_ret_types.contains(v_str_fn)
			|| (!isnil(t.tc) && (str_fn in t.tc.fn_ret_types || v_str_fn in t.tc.fn_ret_types)) {
			return false
		}
	}
	return true
}

// lower_multi_return_str formats a multi-return value as `(a, b, ...)`.
fn (mut t Transformer) lower_multi_return_str(expr flat.NodeId, multi types.MultiReturn, typ_id u32) flat.NodeId {
	typ := flat.source_type_name(typ_id)
	mut item_types := multi.types.clone()
	mut concrete_type := typ
	if int(expr) >= 0 && int(expr) < t.a.nodes.len {
		if concrete_items := t.find_multi_return_call_types(t.a.nodes[int(expr)], multi.types.len) {
			item_types = concrete_items.clone()
			concrete_type = flat.source_type_name(t.multi_return_type_name(concrete_items))
		}
	}
	base := t.stable_transformed_expr_for_reuse(expr, flat.source_type_id(concrete_type), 'multi_ret_str')
	mut result := t.make_string_literal('(')
	for i, item in item_types {
		if i > 0 {
			result = t.string_plus(result, t.make_string_literal(', '))
		}
		item_type_source_id := t.semantic_type_name(item)

		mut item_str := t.wrap_string_conversion(t.make_selector(base, 'arg${i}', item_type_source_id), item_type_source_id)
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
fn (mut t Transformer) stringify_expansion_estimate(typ_id u32) int {
	return t.stringify_expansion_estimate_at(typ_id, t.stringify_depth_cap)
}

fn (mut t Transformer) stringify_expansion_estimate_at(typ_id u32, depth_left int) int {
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	for {
		mut stripped := false
		for prefix in ['mut ', 'shared ', 'atomic ', '...', '?', '!', '[]', '&'] {
			if clean.starts_with(prefix) {
				clean = trimmed_transform_text(clean[prefix.len..])
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
			return t.stringify_expansion_estimate_at(flat.source_type_id(clean[4..bracket_end]), depth_left) + t.stringify_expansion_estimate_at(flat.source_type_id(clean[bracket_end + 1..]), depth_left)
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end > 0 && bracket_end < clean.len - 1 {
			return t.stringify_expansion_estimate_at(flat.source_type_id(clean[bracket_end + 1..]), depth_left)
		}
	}
	agg_source_id := t.stringify_aggregate_type_name(flat.source_type_id(clean)) or { return 0 }
	agg_source_id_2 := agg_source_id
	resolved_sum := flat.source_type_name(t.resolve_sum_name(agg_source_id))
	is_sum := t.sum_types.contains(resolved_sum)
	// A struct with a custom/generated str() method that survives markused lowers
	// to a bounded call, not an inline expansion. Sum types get no such shortcut:
	// interpolating a sum value inside a smartcast branch lowers the *variant's*
	// value directly, which bypasses the sum's own str() and inline-expands the
	// variant struct (this is exactly how v1's `match expr { ... }` bodies blow
	// up on ast.Expr). A str method that markused will prune also falls back to
	// inline expansion, so require the method to still be reachable.
	if !is_sum {
		if str_fn := t.aggregate_str_method_name(agg_source_id_2) {
			if !t.has_used_fn_filter() || t.used_fn_contains_name(str_fn)
				|| t.used_fn_contains_name(c_name(str_fn)) {
				return 0
			}
		}
	}
	if depth_left <= 0 {
		return 1
	}
	// Preserve both the aggregate identity and every bit of the int depth.
	key := (u64(agg_source_id) << 32) | u64(u32(depth_left))
	if cached := t.str_expansion_memo[key] {
		return cached
	}
	// Provisional memo entry so a self-referential type at the same depth does not
	// recurse forever (fields always descend at depth_left-1, so this is belt-and-
	// suspenders against odd alias cycles).
	t.str_expansion_memo[key] = 0
	mut total := 6
	if variants_source_ids := t.sum_types.type_ids(resolved_sum) {
		for variant_source_id in variants_source_ids {
			variant_source_id_7 := variant_source_id

			total += 10 + t.stringify_expansion_estimate_at(variant_source_id_7, depth_left - 1)
		}
	} else if info := t.lookup_struct_info(agg_source_id_2) {
		for field in info.fields {
			field_type := (if field.raw_typ != 0 {
				field.raw_typ
			} else {
				field.typ
			})
			if field_type == 0 {
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
	// Reserve conservatively for a join and its converted operands. Keeping the
	// former pairwise-join bound also covers late, partially lowered expansions.
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
			format = part.typ_text()
		}
		expr_needs_deferred_lowering := t.string_interp_expr_needs_deferred_lowering(expr_id)
		may_hoist = may_hoist || expr_needs_deferred_lowering
			|| t.string_interp_expr_may_hoist(expr_id)
		if format == 'p' {
			// Pointer formatting lowers directly to bounded ptr_str work, regardless of
			// the pointee's aggregate auto-string expansion.
			continue
		}
		needs_deferred_lowering = needs_deferred_lowering || expr_needs_deferred_lowering
		part_expr := t.a.nodes[int(expr_id)]
		// Literal segments of the interpolation are always plain strings.
		if part_expr.kind in [.string_literal, .int_literal, .float_literal, .bool_literal,
			.char_literal] {
			continue
		}
		typ_source_id_6 := t.reliable_stringify_type(expr_id)
		typ := flat.source_type_name(typ_source_id_6)
		if typ_source_id_6 == 0 || typ_source_id_6 == flat.source_type_unknown {
			estimate += unresolved_interp_expansion_estimate
			needs_deferred_lowering = true
		} else {
			if t.is_optional_type_name(flat.source_type_id(trimmed_transform_text(typ))) {
				// Option/result conversion emits a prelude and conditional through
				// pending_stmts even when its scalar payload has no stringify expansion.
				needs_deferred_lowering = true
			}
			stringify_estimate := t.stringify_expansion_estimate(typ_source_id_6)
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
	raw_type := trimmed_transform_text(flat.source_type_name(t.reliable_stringify_type(id)))
	clean_type_source_id := t.normalize_type_alias(flat.source_type_id(raw_type))
	clean_type_shape := flat.source_type_shape(clean_type_source_id)
	if clean_type_shape.has(.array_type) || t.is_fixed_array_type(clean_type_source_id) {
		// Collection stringification always synthesizes a loop and temporaries.
		return true
	}
	iface_name_source_id_2 := t.resolve_interface_type_name(clean_type_source_id)
	iface_name := flat.source_type_name(iface_name_source_id_2)
	if iface_name_source_id_2 != 0 {
		// Interfaces without a concrete str dispatch expand through every boxed
		// implementation and emit their result tree through pending statements.
		str_key := '${iface_name}.str'
		has_bounded_str := !isnil(t.tc)
			&& ('str' in t.tc.interface_abstract_method_names(iface_name_source_id_2)
				|| t.fn_ret_types.contains(str_key) || str_key in t.tc.fn_ret_types)
		if !has_bounded_str {
			return true
		}
	}
	if clean_type_shape.has(.map_type) {
		if _ := t.resolve_receiver_method_for_type(flat.source_type_id(raw_type), 'str') {
			// A user-defined map str method remains a bounded call.
		} else {
			key_type_source_id, raw_value_type_source_id := t.map_type_parts(clean_type_source_id)

			raw_value_type_source_id_3 := raw_value_type_source_id

			fixed_value_type_source_id := t.fixed_array_map_value_type_text(raw_value_type_source_id)
			fixed_value_type_source_id_3 := fixed_value_type_source_id

			value_type := (if fixed_value_type_source_id != 0 {
				fixed_value_type_source_id_3
			} else {
				raw_value_type_source_id_3
			})
			if t.map_str_types_need_typed_lowering(key_type_source_id, value_type) {
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
		if source_type == 0 {
			source_type = t.node_type(source_id)
		}
		source_iface_source_id := t.resolve_interface_type_name(source_type)
		source_iface_comparison_type_id := source_iface_source_id

		target_iface_source_id := t.resolve_interface_type_name(flat.source_type_id(node.value))
		target_iface_comparison_type_id := target_iface_source_id

		if source_iface_source_id != 0 && target_iface_source_id != 0 && source_iface_comparison_type_id != target_iface_comparison_type_id {
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
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if base_type == 0 {
		base_type = t.raw_checker_node_type(base_id)
	}
	if base_type == 0 {
		return false
	}
	clean_type := (if flat.source_type_shape(base_type).has(.pointer) {
		flat.source_type_id(flat.source_type_name(base_type)[1..])
	} else {
		base_type
	})
	_, variants_source_ids := t.concrete_sum_name_and_variants(clean_type)

	return variants_source_ids.len > 0 || (t.resolve_interface_type_name(clean_type)) != 0
}

fn (t &Transformer) string_interp_expr_may_hoist(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return true
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal] {
		return false
	}
	if t.is_optional_type_name(t.trimmed_type_id(t.reliable_stringify_type(id))) {
		return true
	}
	if node.kind == .ident {
		if t.source_parent_ids.len > 0 {
			return t.local_binding_before(node.value, id) or { false }
		}
		return trimmed_transform_text(flat.source_type_name(t.raw_var_type(node.value))).starts_with('shared ')
	}
	if node.kind == .as_expr && interface_pattern_is_collapsed_container_type(flat.source_type_id(node.value)) {
		// Interface-to-container casts emit a runtime type check before producing the
		// value, so interpolation lowering hoists every part to preserve evaluation order.
		return true
	}
	if node.kind == .cast_expr {
		target_type := flat.source_type_name(t.normalize_type_alias(flat.source_type_id(node.value)))
		if target_type.starts_with('&')
			&& t.is_sum_type_name(t.normalize_type_alias(flat.source_type_id(target_type[1..]))) {
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

fn (t &Transformer) stringify_aggregate_type_name(typ_id u32) ?u32 {
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	if clean.len == 0 {
		return none
	}
	// Main-module declarations are stored under their bare names, while checker-resolved
	// generic parameter types can retain a `main.` qualifier after specialization.
	if clean.starts_with('main.') && !clean['main.'.len..].contains('.') {
		short := clean['main.'.len..]
		if short in t.structs || t.sum_types.contains(short)
			|| (!isnil(t.tc) && (short in t.tc.structs || t.tc.sum_types.contains(short))) {
			clean = short
		}
	}
	base, args, is_generic := generic_app_parts(clean)
	if is_generic && args.len > 0 && t.generic_aggregate_base_exists(base, args.len) {
		return flat.source_type_id(clean)
	}
	if clean.contains('.') {
		if !isnil(t.tc) && (clean in t.tc.structs || t.tc.sum_types.contains(clean)) {
			return flat.source_type_id(clean)
		}
		return none
	}
	if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.structs || t.sum_types.contains(qname) {
			return flat.source_type_id(qname)
		}
		if !isnil(t.tc) && (qname in t.tc.structs || t.tc.sum_types.contains(qname)) {
			return flat.source_type_id(qname)
		}
	}
	// A bare type in main belongs to main when a main declaration exists. Imported
	// types also populate qualified_types by short name, and consulting that index
	// first can redirect a local `Chunk` to an imported `ui.Chunk` during auto-str
	// lowering.
	if clean in t.structs || t.sum_types.contains(clean) {
		return flat.source_type_id(clean)
	}
	if !isnil(t.tc) && (clean in t.tc.structs || t.tc.sum_types.contains(clean)) {
		return flat.source_type_id(clean)
	}
	if qualified_source_id := t.qualified_types.type_id(clean) {
		qualified := flat.source_type_name(qualified_source_id)

		if qualified in t.structs || t.sum_types.contains(qualified) {
			return qualified_source_id
		}
		if !isnil(t.tc) && (qualified in t.tc.structs || t.tc.sum_types.contains(qualified)) {
			return qualified_source_id
		}
	}
	return none
}

fn (t &Transformer) generic_specialized_source_type_name(typ_id u32) ?u32 {
	mut clean_id := t.trimmed_type_id(typ_id)
	mut pointer_depth := 0
	for flat.source_type_shape(clean_id).has(.pointer) {
		pointer_depth++
		clean_id = t.trimmed_type_id(t.trim_pointer_type(clean_id))
	}
	arg_ids := t.recorded_type_specialization_args(clean_id) or {
		t.decode_single_generic_specialized_type_args(clean_id) or { return none }
	}
	if arg_ids.len == 0 { return none }
	// Matching a materialized symbol and rendering its application need spelling bytes.
	clean := flat.source_type_name(clean_id)
	args := flat.TypeTextList{ ids: arg_ids }.names()
	pointer_prefix := '&'.repeat(pointer_depth)
	if !isnil(t.tc) {
		for base, params in t.tc.struct_generic_params {
			if params.len == arg_ids.len && generic_specialized_type_matches_flat_name(clean, base, args) {
				return flat.source_type_id(pointer_prefix + generic_specialized_source_type_name_for_base(base, args))
			}
		}
		for base, params in t.tc.sum_generic_params {
			if params.len == arg_ids.len && generic_specialized_type_matches_flat_name(clean, base, args) {
				return flat.source_type_id(pointer_prefix + generic_specialized_source_type_name_for_base(base, args))
			}
		}
	}
	return none
}

fn (t &Transformer) decode_single_generic_specialized_type_args(name_id u32) ?[]u32 {
	if isnil(t.tc) { return none }
	if !isnil(t.generic_spec_decode_cache) {
		mut cache := t.generic_spec_decode_cache
		if decoded := cache.entries[name_id] { return [decoded] }
		if name_id in cache.misses { return none }
	}
	name := flat.source_type_name(name_id)
	for base, params in t.tc.struct_generic_params {
		if params.len != 1 { continue }
		if args := t.decode_single_generic_specialized_type_args_for_base(name, base) {
			decoded := flat.source_type_id(args[0])
			if !isnil(t.generic_spec_decode_cache) {
				mut cache := t.generic_spec_decode_cache
				cache.entries[name_id] = decoded
			}
			return [decoded]
		}
	}
	for base, params in t.tc.sum_generic_params {
		if params.len != 1 { continue }
		if args := t.decode_single_generic_specialized_type_args_for_base(name, base) {
			decoded := flat.source_type_id(args[0])
			if !isnil(t.generic_spec_decode_cache) {
				mut cache := t.generic_spec_decode_cache
				cache.entries[name_id] = decoded
			}
			return [decoded]
		}
	}
	if !isnil(t.generic_spec_decode_cache) {
		mut cache := t.generic_spec_decode_cache
		cache.misses[name_id] = true
	}
	return none
}

fn (t &Transformer) decode_single_generic_specialized_type_args_for_base(name string, base string) ?[]string {
	short_base := base.all_after_last('.')
	if decoded := decode_single_generic_specialized_type_arg_with_prefix(name, base, base) {
		return [decoded]
	}
	base_cname := t.tc.cached_c_name(base)
	if base_cname != base {
		if decoded := decode_single_generic_specialized_type_arg_with_prefix(name, base_cname, base) {
			return [decoded]
		}
	}
	if short_base != base {
		if decoded := decode_single_generic_specialized_type_arg_with_prefix(name, short_base, base) {
			return [decoded]
		}
	}
	short_cname := t.tc.cached_c_name(short_base)
	if short_cname != base && short_cname != base_cname && short_cname != short_base {
		if decoded := decode_single_generic_specialized_type_arg_with_prefix(name, short_cname, base) {
			return [decoded]
		}
	}
	return none
}

fn decode_single_generic_specialized_type_arg_with_prefix(name string, prefix string, base string) ?string {
	if prefix == '' || name.len <= prefix.len || name[prefix.len] != `_`
		|| !name.starts_with(prefix) {
		return none
	}
	decoded := generic_type_arg_from_suffix_with_containers(name[prefix.len + 1..])
	if decoded.len > 0 && generic_specialized_type_matches_flat_name(name, base, [
		decoded,
	]) {
		return decoded
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
	if flat_name == '' || base == '' || args.len == 0 {
		return false
	}
	if !generic_specialized_type_base_may_match(flat_name, base) {
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
	suffix := generic_type_suffixes(flat.TypeTextList.from_names(args).ids)
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

fn generic_specialized_type_base_may_match(flat_name string, base string) bool {
	// Complex naming cases still use the complete codec below. Ordinary type
	// bases keep their prefix in source, C, and decoded C spellings.
	if base.starts_with('C.') || flat_name.starts_with('__v3_internal_symbol_') {
		return true
	}
	for ch in base {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `.`) {
			return true
		}
	}
	mut prefix := base
	for _ in 0 .. 2 {
		mut offset := 0
		mut matches := true
		for ch in prefix {
			if ch == `.` && offset + 1 < flat_name.len && flat_name[offset] == `_`
				&& flat_name[offset + 1] == `_` {
				offset += 2
			} else if offset < flat_name.len && flat_name[offset] == ch {
				offset++
			} else {
				matches = false
				break
			}
		}
		if matches {
			return true
		}
		if !prefix.starts_with('main.') {
			return false
		}
		prefix = short_name_view(prefix)
	}
	return false
}

fn add_generic_specialized_type_candidate(mut candidates []string, candidate string) {
	clean := trimmed_transform_text(candidate)
	if clean.len > 0 && clean !in candidates {
		candidates << clean
	}
}

fn generic_specialized_source_type_name_for_base(base string, args []string) string {
	display_base := if base.starts_with('main.') { base.all_after_last('.') } else { base }
	return '${display_base}[${args.join(', ')}]'
}

fn (t &Transformer) source_type_name_from_c_name(typ_id u32) ?u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || !clean.contains('__') || t.type_name_is_declared(clean) {
		return none
	}
	for name, _ in t.structs {
		if source_type_name_matches_c_name(name, clean) {
			return flat.source_type_id(name)
		}
	}
	for name, _ in t.sum_types.ids {
		if source_type_name_matches_c_name(name, clean) {
			return flat.source_type_id(name)
		}
	}
	for name, _ in t.enum_types {
		if source_type_name_matches_c_name(name, clean) {
			return flat.source_type_id(name)
		}
	}
	if isnil(t.tc) {
		return none
	}
	for name, _ in t.tc.structs {
		if source_type_name_matches_c_name(name, clean) {
			return flat.source_type_id(name)
		}
	}
	for name, _ in t.tc.sum_types.ids {
		if source_type_name_matches_c_name(name, clean) {
			return flat.source_type_id(name)
		}
	}
	for name, _ in t.tc.enum_names {
		if source_type_name_matches_c_name(name, clean) {
			return flat.source_type_id(name)
		}
	}
	for name, _ in t.tc.type_aliases.ids {
		if source_type_name_matches_c_name(name, clean) {
			return flat.source_type_id(name)
		}
	}
	return none
}

struct FixedArrayCNameParts {
	elem string
	len  string
}

fn (t &Transformer) fixed_array_source_type_from_c_name(typ_id u32) ?u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if !clean.starts_with('Array_fixed_') {
		return none
	}
	payload := clean['Array_fixed_'.len..]
	parts := t.split_fixed_array_c_name_payload(payload) or { return none }
	elem_type := flat.source_type_name(t.fixed_array_c_elem_source_type(parts.elem))
	len_text := t.fixed_array_c_name_len_source(parts.len)
	return flat.source_type_id('${elem_type}[${len_text}]')
}

fn (t &Transformer) fixed_array_c_elem_source_type(elem string) u32 {
	if fixed_source_id := t.fixed_array_source_type_from_c_name(flat.source_type_id(elem)) {
		fixed_source_id_2 := fixed_source_id

		return fixed_source_id_2
	}
	if source_source_id := t.source_type_name_from_c_name(flat.source_type_id(elem)) {
		source_source_id_2 := source_source_id

		return source_source_id_2
	}
	return flat.source_type_id(elem)
}

fn (t &Transformer) split_fixed_array_c_name_payload(payload string) ?FixedArrayCNameParts {
	if payload == '' {
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
				len:  len
			}
		}
		if t.fixed_array_c_name_len_is_known(len) {
			return FixedArrayCNameParts{
				elem: elem
				len:  len
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

fn (t &Transformer) normalize_runtime_array_stringify_type(typ_id u32) u32 {
	typ := flat.source_type_name(typ_id)
	clean := trimmed_transform_text(typ)
	if clean.starts_with('Array_') && !clean.starts_with('Array_fixed_') {
		if clean in t.structs || t.sum_types.contains(clean) || clean in t.enum_types {
			return typ_id
		}
		if !isnil(t.tc) && t.tc.type_aliases.contains(clean) {
			return typ_id
		}
		decoded := t.generic_type_arg_from_suffix(clean)
		if decoded.len > 0 {
			return flat.source_type_id(decoded)
		}
	}
	return typ_id
}

fn (mut t Transformer) mark_interface_method_implementers_used(iface_name_id u32, method string) {
	impls := t.interface_impl_index_for_transform(iface_name_id).names.ids
	for concrete_id in impls {
		if t.has_used_fn_filter() && !t.interface_boxed_type_used(iface_name_id, concrete_id) {
			continue
		}
		concrete := flat.source_type_name(concrete_id)
		concrete_method := '${concrete}.${method}'
		t.mark_fn_used_name(concrete_method)
		if method_name := t.tc.concrete_method_signature_key(concrete_id, method) {
			if method_name != concrete_method {
				t.mark_fn_used_name(method_name)
			}
		}
	}
}

fn (t &Transformer) interface_method_implementer_names(iface_name_id u32, method string) []string {
	if isnil(t.tc) {
		return []string{}
	}
	impls := t.interface_impl_index_for_transform(iface_name_id).names.ids
	mut names := []string{cap: impls.len * 2}
	for concrete_id in impls {
		concrete := flat.source_type_name(concrete_id)
		concrete_method := '${concrete}.${method}'
		names << concrete_method
		if method_name := t.tc.concrete_method_signature_key(concrete_id, method) {
			if method_name != concrete_method {
				names << method_name
			}
		}
	}
	return names
}

fn (mut t Transformer) lower_sum_str(expr flat.NodeId, sum_name_id u32) flat.NodeId {
	sum_name := flat.source_type_name(sum_name_id)
	resolved_sum_source_id_2, variants_source_ids_2 := t.concrete_sum_name_and_variants(sum_name_id)
	resolved_sum := flat.source_type_name(resolved_sum_source_id_2)

	if variants_source_ids_2.len == 0 {
		return t.make_string_literal('${sum_name}{}')
	}
	sum_display := if resolved_sum.contains('.') {
		resolved_sum.all_after_last('.')
	} else {
		resolved_sum
	}
	// Repeated sum types can contain distinct, acyclic payloads. Emit a runtime
	// helper instead of truncating them according to their compile-time nesting.
	if t.stringify_stack_count(resolved_sum_source_id_2) > 0 {
		return t.request_auto_str_helper(expr, resolved_sum_source_id_2)
	}
	if t.stringify_stack.len >= t.stringify_depth_cap && t.stringify_stack_count(resolved_sum_source_id_2) == 0
		&& !t.stringify_types_match(t.auto_str_synthesis_type, resolved_sum_source_id_2) {
		return t.request_auto_str_helper(expr, resolved_sum_source_id_2)
	}
	t.stringify_stack << resolved_sum_source_id_2
	defer {
		t.stringify_stack.delete_last()
	}
	mut base := expr
	storage_type := if sum_name.starts_with('&') {
		flat.source_type_id('&${resolved_sum}')
	} else {
		resolved_sum_source_id_2
	}
	if !t.expr_can_take_address(base) {
		name := t.new_temp('sum_str')
		t.pending_stmts << t.make_decl_assign_typed(name, base, storage_type)
		base = t.make_ident(name)
	}
	base = t.stable_transformed_expr_for_reuse(base, storage_type, 'sum_str')
	tag := t.make_sum_tag_selector(base, if sum_name.starts_with('&') {
		.arrow
	} else {
		.dot
	})
	return t.build_sum_str_chain(base, tag, resolved_sum_source_id_2, sum_display, variants_source_ids_2.clone(), 0)
}

fn (mut t Transformer) build_sum_str_chain(base flat.NodeId, tag flat.NodeId, sum_name_id u32, sum_display string, variants_ids []u32, idx int) flat.NodeId {
	if idx >= variants_ids.len {
		return t.make_string_literal('unknown sum type value')
	}
	variant_comparison_type_id := variants_ids[idx]
	variant_source_id_3 := variant_comparison_type_id
	variant_source_id_9 := variant_source_id_3
	variant := flat.source_type_name(variant_source_id_9)
	field := sum_field_name(variant)
	variant_base_source_id := t.normalize_type_alias(variant_source_id_3)
	variant_base_comparison_type_id := variant_base_source_id
	variant_base_source_id_2 := variant_base_comparison_type_id
	variant_base_source_id_3 := variant_base_source_id_2
	variant_base := flat.source_type_name(variant_base_source_id_3)
	direct_pointer := t.sum_variant_is_direct_pointer(variant_source_id_3)
	field_sel := t.make_selector_op(base, field, variant_source_id_9, .dot)
	t.mark_generated_variant_access(field_sel, variant_source_id_3)
	// Only statements created for THIS branch's payload conversion belong inside
	// the branch; earlier pending statements (e.g. the base temp decl from
	// lower_sum_str) must stay with the caller so they precede the whole if-chain.
	pending_start := t.pending_stmts.len
	mut value_text := if t.is_fixed_array_type(variant_base_source_id) {
		elem_type := fixed_array_elem_type_bytes(variant_base)
		arr := t.make_array_storage_call('new_array_from_c_array', [
			t.make_fixed_array_len_expr(variant_base_source_id_2),
			t.make_fixed_array_len_expr(variant_base_source_id_2),
			t.make_sizeof_type(flat.source_type_id(elem_type)),
			field_sel,
		], flat.source_type_id('[]${elem_type}'))
		t.wrap_string_conversion(arr, flat.source_type_id('[]${elem_type}'))
	} else if direct_pointer {
		t.wrap_string_conversion(field_sel, (if variant_base_comparison_type_id != variant_comparison_type_id {
			variant_base_source_id_3
		} else {
			variant_source_id_9
		}))
	} else if aggregate_source_id := t.stringify_aggregate_type_name(variant_base_source_id) {

		// Inline payload addresses identify an actual cycle when a recursive helper
		// reaches the same value again.
		address := t.make_prefix(.amp, field_sel)
		t.set_node_typ(int(address), flat.source_type_id('&${variant}'))
		t.lower_ref_str_guarded(address, aggregate_source_id, false, '', 'nil')
	} else {
		value := field_sel
		payload_type := (if variant_base_comparison_type_id != variant_comparison_type_id {
			variant_base_source_id_3
		} else {
			variant_source_id_9
		})
		t.set_node_typ(int(value), payload_type)
		if t.stringify_type_at_circular_limit(payload_type)
			&& t.aggregate_str_method_name(payload_type) == none {
			// Repeating a payload type does not imply a repeated object. Continue in
			// a helper, guarding the actual payload address to stop genuine cycles.
			t.request_auto_str_helper(value, payload_type)
			address := t.make_prefix(.amp, field_sel)
			t.set_node_typ(int(address), flat.source_type_id('&${variant}'))
			t.lower_ref_str_guarded(address, payload_type, false,
				auto_str_helper_name(payload_type), 'nil')
		} else {
			t.wrap_string_conversion(value, payload_type)
		}
	}
	// V prints a sum value as `SumName(payload_str)` — the payload's own str
	// already carries its type name for structs; string/rune payloads are quoted.
	// An alias variant keeps its alias-name wrapper (`Res(Ints([1, 2]))`).
	if flat.is_named_variant_type_name(variant_base) {
		// A named variant prints as `Expr.Count(3)` or `Expr.Void`, without the
		// `SumName(...)` wrapper: the variant name already names the sum type.
	} else if variant_base_source_id == flat.source_type_string {
		value_text = t.string_plus(t.string_plus(t.make_string_literal("'"), value_text), t.make_string_literal("'"))
	} else if variant_base_source_id == flat.source_type_rune {
		value_text = t.string_plus(t.string_plus(t.make_string_literal('`'), value_text), t.make_string_literal('`'))
	} else if variant_base_comparison_type_id != variant_comparison_type_id {
		display := if variant.contains('.') { variant.all_after_last('.') } else { variant }
		value_text = t.string_plus(t.string_plus(t.make_string_literal('${display}('), value_text), t.make_string_literal(')'))
	}
	if !flat.is_named_variant_type_name(variant_base) {
		value_text = t.string_plus(t.string_plus(t.make_string_literal('${sum_display}('), value_text), t.make_string_literal(')'))
	}
	mut then_stmts := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	then_stmts << t.make_expr_stmt(value_text)
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name_id, variant_source_id_3)))
	then_block := t.make_block(then_stmts)
	else_expr := t.build_sum_str_chain(base, tag, sum_name_id, sum_display, variants_ids.clone(), idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            flat.source_type_string
	})
}

fn (mut t Transformer) wrap_formatted_string_conversion(expr flat.NodeId, typ_id u32, format string) flat.NodeId {
	typ := flat.source_type_name(typ_id)
	if format == '' {
		return t.wrap_string_conversion(expr, typ_id)
	}
	if format.contains('X') {
		// Uppercase hex behaves like lowercase `x` but with A-F. Format the
		// lowercase form first so width/zero-padding flags are honored, then
		// upper-case the whole result.
		lowered := t.wrap_formatted_string_conversion(expr, typ_id, format.replace('X', 'x'))
		return t.make_call_typed('v3_string_upper_ascii', [lowered], flat.source_type_string)
	}
	mut clean_typ := typ
	if clean_typ.starts_with('&') {
		clean_typ = clean_typ[1..]
	}
	for clean_typ.starts_with('atomic ') {
		clean_typ = trimmed_transform_text(clean_typ[7..])
	}
	if clean_typ.starts_with('builtin.') {
		clean_typ = clean_typ.all_after_last('.')
	}
	mut normalized_typ := typ
	for _ in 0 .. 1000 {
		next := flat.source_type_name(t.normalize_type_alias(flat.source_type_id(normalized_typ)))
		if next == normalized_typ {
			break
		}
		normalized_typ = next
	}
	if repeat_count, upper := string_repeat_format(format) {
		if clean_typ == 'string' || normalized_typ == 'string' {
			t.mark_fn_used('string__repeat')
			repeated := t.make_call_typed('string__repeat', [expr, t.make_int_literal(repeat_count)], flat.source_type_string)
			return if upper {
				t.make_call_typed('v3_string_upper_ascii', [repeated], flat.source_type_string)
			} else {
				repeated
			}
		}
	}
	if format == 'p' && (normalized_typ.starts_with('&')
		|| clean_typ in ['voidptr', 'byteptr', 'charptr']
		|| t.expr_is_mut_param_pointer_value(expr)) {
		return t.make_call_typed('ptr_str', [expr], flat.source_type_string)
	}
	if normalized_typ.starts_with('&') && format != 'p' {
		elem_type := normalized_typ[1..]
		if t.expr_is_shared_value(expr) {
			return t.wrap_formatted_string_conversion(expr, flat.source_type_id(elem_type), format)
		}
		if format == 's' {
			return t.lower_pointer_format_s(expr, flat.source_type_id(normalized_typ), flat.source_type_id(elem_type))
		}
		value := t.make_prefix(.mul, expr)
		t.set_node_typ(int(value), flat.source_type_id(elem_type))
		return t.wrap_formatted_string_conversion(value, flat.source_type_id(elem_type), format)
	}
	if t.is_optional_type_name(flat.source_type_id(normalized_typ)) {
		opt_type_source_id := t.qualify_optional_type(flat.source_type_id(normalized_typ))

		mut value_type := t.optional_base_type(opt_type_source_id)
		if value_type == 0 || value_type == flat.source_type_void {
			value_type = flat.source_type_int
		}
		// Formatted option interpolation follows V's payload formatting semantics:
		// `none` uses the zero-initialized payload, while `some` uses its stored value.
		// Pin the wrapper so a call/index operand is evaluated exactly once.
		opt_name := t.new_temp('fmt_opt')
		t.pending_stmts << t.make_decl_assign_typed(opt_name, t.transform_optional_wrapper_expr(expr),
			opt_type_source_id)
		value := t.make_selector(t.make_ident(opt_name), 'value', value_type)
		return t.wrap_formatted_string_conversion(value, value_type, format)
	}
	if dyn := t.dynamic_format_conversion(expr, typ_id, flat.source_type_id(clean_typ), format) {
		return dyn
	}
	// An enum with a base (`x`/`b`/`o`) or decimal (`d`) verb prints its integer
	// value, not its name; format the underlying integer. Unsigned-backed enums use
	// u64 so values above i64.max are not rendered as negative.
	if format[format.len - 1] in [`x`, `b`, `o`, `d`] && t.is_formatted_enum_type(flat.source_type_id(clean_typ)) {
		int_type := (if t.enum_backing_is_unsigned(flat.source_type_id(clean_typ)) {
			flat.source_type_u64
		} else {
			flat.source_type_i64
		})
		return t.wrap_formatted_string_conversion(t.make_cast(int_type, expr, int_type), int_type, format)
	}
	if plus_format := plus_numeric_format(format) {
		is_float := clean_typ in ['f32', 'f64', 'float_literal']
		is_integer := clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune', 'u8', 'u16',
			'u32', 'u64', 'usize', 'i128', 'u128']
		if is_integer || (is_float && (plus_format.width > 0 || plus_format.core.starts_with('.'))) {
			// A rune's ordinary str() returns a character; numeric flags need its code point.
			converted := if clean_typ == 'rune' {
				t.wrap_formatted_string_conversion(t.make_cast(flat.source_type_i64, expr, flat.source_type_i64), flat.source_type_i64,
					plus_format.core)
			} else {
				t.wrap_formatted_string_conversion(expr, typ_id, plus_format.core)
			}
			mut formatted := t.make_call_typed('v3_string_plus_sign', [converted], flat.source_type_string)
			if plus_format.width > 0 {
				formatted = if plus_format.zero && !plus_format.left {
					t.make_call_typed(if is_float { 'v3_f64_zpad' } else { 'v3_string_zpad' },
						[formatted, t.make_int_literal(plus_format.width)], flat.source_type_string)
				} else {
					t.make_call_typed('v3_string_pad', [formatted,
						t.make_int_literal(plus_format.width),
						t.make_int_literal(if plus_format.left { 1 } else { 0 })], flat.source_type_string)
				}
			}
			return formatted
		}
	}
	if decimal_format := fixed_decimal_format(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast(flat.source_type_f64, expr, flat.source_type_f64)
			}
			fixed_fn := if decimal_format.trim { 'v3_f64_trimmed' } else { 'v3_f64_fixed' }
			mut formatted := t.make_call_typed(fixed_fn, [arg,
				t.make_int_literal(decimal_format.precision)], flat.source_type_string)
			if decimal_format.width > 0 || decimal_format.left {
				left := if decimal_format.left { 1 } else { 0 }
				formatted = if decimal_format.zero && !decimal_format.left {
					t.make_call_typed('v3_f64_zpad', [formatted,
						t.make_int_literal(decimal_format.width)], flat.source_type_string)
				} else {
					t.make_call_typed('v3_string_pad', [formatted,
						t.make_int_literal(decimal_format.width), t.make_int_literal(left)], flat.source_type_string)
				}
			}
			return formatted
		}
	}
	if exp_format := exponent_decimal_format(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast(flat.source_type_f64, expr, flat.source_type_f64)
			}
			mut formatted := t.make_call_typed('v3_f64_exp', [arg,
				t.make_int_literal(exp_format.precision), t.make_int_literal(if exp_format.upper {
					1
				} else {
					0
				})], flat.source_type_string)
			if exp_format.width > 0 || exp_format.left {
				left := if exp_format.left { 1 } else { 0 }
				formatted = t.make_call_typed('v3_string_pad', [formatted,
					t.make_int_literal(exp_format.width), t.make_int_literal(left)], flat.source_type_string)
			}
			return formatted
		}
	}
	if format in ['g', 'G'] && clean_typ in ['f32', 'f64', 'float_literal'] {
		fn_name := if clean_typ == 'f32' { 'f32__strg' } else { 'f64__strg' }
		arg := if clean_typ == 'float_literal' {
			t.make_cast(flat.source_type_f64, expr, flat.source_type_f64)
		} else {
			expr
		}
		t.mark_fn_used(fn_name)
		formatted := t.make_call_typed(fn_name, [arg], flat.source_type_string)
		return if format == 'G' {
			t.make_call_typed('v3_string_upper_ascii', [formatted], flat.source_type_string)
		} else {
			formatted
		}
	}
	if general_format := general_float_format(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ == 'f64' {
				expr
			} else {
				t.make_cast(flat.source_type_f64, expr, flat.source_type_f64)
			}
			mut formatted := t.make_call_typed('v3_f64_general', [arg,
				t.make_int_literal(general_format.precision), t.make_int_literal(if general_format.upper {
					1
				} else {
					0
				})], flat.source_type_string)
			if general_format.width > 0 || general_format.left {
				left := if general_format.left { 1 } else { 0 }
				formatted = t.make_call_typed('v3_string_pad', [formatted,
					t.make_int_literal(general_format.width), t.make_int_literal(left)], flat.source_type_string)
			}
			return formatted
		}
	}
	if is_wide_integer_type_name(clean_typ) {
		return t.wide_integer_format_conversion(expr, typ_id, format)
	}
	if char_format := character_format(format) {
		if normalized_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16', 'u32', 'u64',
			'usize', 'char', 'rune'] {
			arg := if normalized_typ == 'int' {
				expr
			} else {
				t.make_cast(flat.source_type_int, expr, flat.source_type_int)
			}
			mut converted := t.make_call_typed('v3_char_string', [arg], flat.source_type_string)
			if char_format.width > 1 || char_format.left {
				converted = t.make_call_typed('v3_string_pad', [converted,
					t.make_int_literal(char_format.width), t.make_int_literal(if char_format.left {
						1
					} else {
						0
					})], flat.source_type_string)
			}
			return converted
		}
	}
	if format == '+d' && clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
		return t.signed_plus_string(expr, flat.source_type_id(clean_typ))
	}
	if base := integer_format_base(format) {
		if clean_typ in ['u8', 'u16', 'u32', 'u64', 'usize'] {
			arg := t.widened_unsigned_format_arg(expr, flat.source_type_id(clean_typ))
			formatted := t.make_call_typed('strconv__format_uint', [arg, t.make_int_literal(base)], flat.source_type_string)
			return if format == 'X' {
				t.make_call_typed('v3_string_upper_ascii', [formatted], flat.source_type_string)
			} else {
				formatted
			}
		}
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
			formatted := t.make_call_typed('strconv__format_int', [expr, t.make_int_literal(base)], flat.source_type_string)
			return if format == 'X' {
				t.make_call_typed('v3_string_upper_ascii', [formatted], flat.source_type_string)
			} else {
				formatted
			}
		}
	}
	if base_format := zero_padded_integer_base_format(format) {
		converted := if clean_typ in ['u8', 'u16', 'u32', 'u64', 'usize'] {
			arg := t.widened_unsigned_format_arg(expr, flat.source_type_id(clean_typ))
			t.make_call_typed('strconv__format_uint', [arg, t.make_int_literal(base_format.base)], flat.source_type_string)
		} else if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
			arg := if clean_typ == 'i64' {
				expr
			} else {
				t.make_cast(flat.source_type_i64, expr, flat.source_type_i64)
			}
			t.make_call_typed('strconv__format_int', [arg, t.make_int_literal(base_format.base)], flat.source_type_string)
		} else {
			t.wrap_string_conversion(expr, typ_id)
		}
		return t.make_call_typed('v3_string_zpad', [converted, t.make_int_literal(base_format.width)], flat.source_type_string)
	}
	if width := left_zero_padded_decimal_width(format) {
		converted := t.wrap_formatted_string_conversion(expr, typ_id, 'd')
		return t.make_call_typed('v3_string_rpad_zero', [converted, t.make_int_literal(width)], flat.source_type_string)
	}
	if width := zero_padded_decimal_width(format) {
		if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune', 'usize', 'u8', 'u16',
			'u32', 'u64'] {
			if clean_typ in ['u64', 'usize', 'u32', 'u16', 'u8'] {
				arg := t.widened_unsigned_format_arg(expr, flat.source_type_id(clean_typ))
				return t.make_call_typed('v3_u64_zpad', [arg, t.make_int_literal(width)], flat.source_type_string)
			}
			if clean_typ in ['i64', 'isize', 'i32', 'i16', 'i8', 'rune'] {
				arg := if clean_typ == 'i64' {
					expr
				} else {
					t.make_cast(flat.source_type_i64, expr, flat.source_type_i64)
				}
				return t.make_call_typed('v3_i64_zpad', [arg, t.make_int_literal(width)], flat.source_type_string)
			}
			return t.make_call_typed('v3_int_zpad', [expr, t.make_int_literal(width)], flat.source_type_string)
		}
	}
	if width := static_format_width(format) {
		mut converted := if base := integer_format_base_suffix(format) {
			if clean_typ in ['u8', 'u16', 'u32', 'u64', 'usize'] {
				arg := t.widened_unsigned_format_arg(expr, flat.source_type_id(clean_typ))
				t.make_call_typed('strconv__format_uint', [arg, t.make_int_literal(base)], flat.source_type_string)
			} else if clean_typ in ['int', 'i8', 'i16', 'i32', 'i64', 'isize', 'rune'] {
				t.make_call_typed('strconv__format_int', [expr, t.make_int_literal(base)], flat.source_type_string)
			} else {
				t.wrap_string_conversion(expr, typ_id)
			}
		} else {
			t.wrap_string_conversion(expr, typ_id)
		}
		return t.make_call_typed('v3_string_pad', [converted, t.make_int_literal(width),
			t.make_int_literal(0)], flat.source_type_string)
	}
	return t.wrap_string_conversion(expr, typ_id)
}

// is_wide_integer_type_name reports whether a type name is one of the 128-bit
// integers. Their formatting cannot go through the 64-bit helpers: the cast alone
// would cut the value back to its low half.
fn is_wide_integer_type_name(name string) bool {
	return name in ['u128', 'i128']
}

// wide_integer_format_conversion lowers a format specifier on a 128-bit value. The
// base forms print through `u128.str_base`, which formats the bit pattern, and the
// width comes from padding the printed text rather than the number.
fn (mut t Transformer) wide_integer_format_conversion(expr flat.NodeId, typ_id u32, format string) flat.NodeId {
	typ := flat.source_type_name(typ_id)
	mut text := t.wrap_string_conversion(expr, typ_id)
	if format == 'c' {
		// The cast the 64-bit path would use cannot be put on a 128-bit value, so
		// the conversion happens inside the type instead.
		name := if trimmed_transform_text(typ) == 'i128' {
			'i128__char_str'
		} else {
			'u128__char_str'
		}
		return t.make_call_typed(name, [expr], flat.source_type_string)
	}
	mut width := 0
	mut zero := false
	mut base := 0
	if format == 'X' || format == 'x' {
		base = 16
	} else if format == 'o' {
		base = 8
	} else if format == 'b' {
		base = 2
	} else if b := integer_format_base(format) {
		base = b
	} else if padded := zero_padded_integer_base_format(format) {
		base = padded.base
		width = padded.width
		zero = true
	} else if w := left_zero_padded_decimal_width(format) {
		width = w
		zero = true
	} else if w := zero_padded_decimal_width(format) {
		width = w
		zero = true
	} else if w := static_format_width(format) {
		width = w
	}
	if base > 0 {
		text = t.make_call_typed('u128__str_base',
			[
				t.make_cast(flat.source_type_id('u128'), expr, flat.source_type_id('u128')),
				t.make_int_literal(base),
			], flat.source_type_string)
		if format == 'X' {
			text = t.make_call_typed('v3_string_upper_ascii', [text], flat.source_type_string)
		}
	}
	if width > 0 {
		text = if zero {
			t.make_call_typed('v3_string_zpad', [text, t.make_int_literal(width)], flat.source_type_string)
		} else {
			t.make_call_typed('v3_string_pad', [text, t.make_int_literal(width), t.make_int_literal(0)], flat.source_type_string)
		}
	}
	return text
}

fn (mut t Transformer) widened_unsigned_format_arg(expr flat.NodeId, typ_id u32) flat.NodeId {
	if typ_id == flat.source_type_u64 {
		return expr
	}
	name := t.new_temp('fmt_unsigned')
	t.pending_stmts << t.make_decl_assign_typed(name, expr, typ_id)
	return t.make_cast(flat.source_type_u64, t.make_ident(name), flat.source_type_u64)
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
		return trimmed_transform_text(flat.source_type_name(t.raw_var_type(node.value))).starts_with('shared ')
	}
	if node.kind == .selector && node.value == 'val' && node.children_count > 0 {
		base := t.a.child_node(&node, 0)
		return base.kind == .ident && trimmed_transform_text(flat.source_type_name(t.raw_var_type(base.value))).starts_with('shared ')
	}
	return false
}

fn (mut t Transformer) lower_pointer_format_s(expr flat.NodeId, typ_id u32, elem_type_id u32) flat.NodeId {
	ptr_name := t.new_temp('str_fmt_ptr')
	res_name := t.new_temp('str_fmt_text')
	t.pending_stmts << t.make_decl_assign_typed(ptr_name, expr, typ_id)
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal(''), flat.source_type_string)
	value := t.make_prefix(.mul, t.make_ident(ptr_name))
	t.set_node_typ(int(value), elem_type_id)
	value_str := t.string_plus(t.make_string_literal('&'), t.wrap_string_conversion(value, elem_type_id))
	then_body := t.make_block([t.make_assign(t.make_ident(res_name), value_str)])
	cond := t.make_infix(.ne, t.make_ident(ptr_name), t.a.add(.nil_literal))
	t.pending_stmts << t.make_if(cond, then_body, t.make_empty())
	return t.make_ident(res_name)
}

fn (mut t Transformer) dynamic_format_conversion(expr flat.NodeId, typ_id u32, clean_typ_id u32, format string) ?flat.NodeId {
	clean_typ_comparison_type_id := clean_typ_id
	clean_typ := flat.source_type_name(clean_typ_comparison_type_id)
	if width := t.dynamic_width_expr(format) {
		converted := t.wrap_string_conversion(expr, typ_id)
		return t.make_call_typed('v3_string_pad', [converted, width, t.make_int_literal(0)], flat.source_type_string)
	}
	if width := t.dynamic_zero_width_expr(format) {
		if clean_typ_comparison_type_id in [flat.source_type_int, flat.source_type_i8,
			flat.source_type_i16, flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
			flat.source_type_rune, flat.source_type_usize, flat.source_type_u8, flat.source_type_u16,
			flat.source_type_u32, flat.source_type_u64] {
			converted := t.wrap_formatted_string_conversion(expr, typ_id, 'd')
			return t.make_call_typed('v3_string_zpad', [converted, width], flat.source_type_string)
		}
	}
	if width := t.dynamic_plus_width_expr(format) {
		if clean_typ_comparison_type_id in [flat.source_type_int, flat.source_type_i8,
			flat.source_type_i16, flat.source_type_i32, flat.source_type_i64, flat.source_type_isize,
			flat.source_type_rune] {
			converted := t.signed_plus_string(expr, clean_typ_id)
			return t.make_call_typed('v3_string_pad', [converted, width, t.make_int_literal(0)], flat.source_type_string)
		}
	}
	if width_name, precision_name := dynamic_float_width_precision(format) {
		if clean_typ in ['f32', 'f64', 'float_literal'] {
			arg := if clean_typ_id == flat.source_type_f64 {
				expr
			} else {
				t.make_cast(flat.source_type_f64, expr, flat.source_type_f64)
			}
			formatted := t.make_call_typed('v3_f64_fixed', [arg, t.make_ident(precision_name)], flat.source_type_string)
			return t.make_call_typed('v3_string_pad', [formatted, t.make_ident(width_name),
				t.make_int_literal(0)], flat.source_type_string)
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

fn (mut t Transformer) signed_plus_string(expr flat.NodeId, typ_id u32) flat.NodeId {
	value := t.stable_transformed_expr_for_reuse(expr, typ_id, 'fmt_sign')
	text_name := t.new_temp('fmt_sign_text')
	base_text := t.wrap_formatted_string_conversion(value, typ_id, 'd')
	t.pending_stmts << t.make_decl_assign_typed(text_name, base_text, flat.source_type_string)
	cond := t.make_infix(.ge, value, t.make_int_literal(0))
	then_body := t.make_block([
		t.make_assign(t.make_ident(text_name), t.string_plus(t.make_string_literal('+'), t.make_ident(text_name))),
	])
	t.pending_stmts << t.make_if(cond, then_body, t.make_empty())
	return t.make_ident(text_name)
}

struct PlusNumericFormat {
	width int
	zero  bool
	left  bool
	core  string
}

// plus_numeric_format separates the sign and width from the numeric conversion.
// The sign is added before padding so it counts towards the requested width.
fn plus_numeric_format(format string) ?PlusNumericFormat {
	mut i := 0
	mut plus := false
	mut zero := false
	mut left := false
	for i < format.len && format[i] in [`+`, `-`, `0`] {
		match format[i] {
			`+` { plus = true }
			`-` { left = true }
			`0` { zero = true }
			else {}
		}
		i++
	}
	if !plus {
		return none
	}
	mut width := 0
	for i < format.len && format[i] >= `0` && format[i] <= `9` {
		width = width * 10 + int(format[i] - `0`)
		i++
	}
	core := format[i..]
	if core != '' && core != 'd' && core != 'g' && core != 'G'
		&& fixed_decimal_format(core) == none && exponent_decimal_format(core) == none
		&& general_float_format(core) == none {
		return none
	}
	return PlusNumericFormat{
		width: width
		zero:  zero
		left:  left
		core:  core
	}
}

struct FixedDecimalFormat {
	width     int
	precision int
	zero      bool
	left      bool
	trim      bool // no `f`: trim trailing zeros, exponent form outside [1e-5, 999999)
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
	if format == '' || format[format.len - 1] != `c` {
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
		left:  left
	}
}

fn string_repeat_format(format string) ?(int, bool) {
	if format == '' || format[format.len - 1] !in [`r`, `R`] {
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
	mut zero := false
	if i < format.len && format[i] == `0` {
		zero = true
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
	trim := i == format.len
	if !trim {
		if format[i] != `f` {
			return none
		}
		i++
	}
	if i != format.len {
		return none
	}
	return FixedDecimalFormat{
		width:     width
		precision: precision
		zero:      zero
		left:      left
		trim:      trim
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
		width:     width
		precision: precision
		left:      left
		upper:     upper
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
		width:     width
		precision: precision
		left:      left
		upper:     upper
	}
}

fn (t &Transformer) enum_backing_is_unsigned(clean_typ_id u32) bool {
	clean_typ := flat.source_type_name(clean_typ_id)
	mut backing := t.enum_backing_types.type_id(clean_typ) or { u32(0) }
	if backing == 0 && !clean_typ.contains('.') && t.cur_module.len > 0
		&& t.cur_module !in ['main', 'builtin'] {
		backing = t.enum_backing_types.type_id('${t.cur_module}.${clean_typ}') or { u32(0) }
	}
	return backing in [flat.source_type_u8, flat.source_type_u16, flat.source_type_u32,
		flat.source_type_u64, flat.source_type_usize]
}

fn (t &Transformer) is_formatted_enum_type(clean_typ_id u32) bool {
	clean_typ := flat.source_type_name(clean_typ_id)
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
	if format == '' {
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
	if format == '' || format[0] == `0` {
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
	if i == format.len - 1 && format[i] in [`s`, `d`, `u`, `b`, `x`, `o`, `p`] {
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
		base:  base
	}
}

fn zero_padded_decimal_width(format string) ?int {
	if format.len < 2 || format[0] != `0` {
		return none
	}
	mut end := format.len
	// `u` is the decimal verb of an unsigned integer; the checker rejects `d` there.
	if format[end - 1] in [`d`, `u`] {
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
	if format[end - 1] in [`d`, `u`] {
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

fn (mut t Transformer) generic_receiver_str_call(expr flat.NodeId, typ_id u32) ?flat.NodeId {
	typ := flat.source_type_name(typ_id)
	if isnil(t.tc) || !typ.contains('[') {
		return none
	}
	clean_typ := (if typ.starts_with('&') { flat.source_type_id(typ[1..]) } else { typ_id })
	if !flat.source_type_name(clean_typ).ends_with(']') {
		return none
	}
	info := t.tc.resolve_generic_struct_method(clean_typ, 'str') or {
		t.tc.resolve_generic_sum_method(clean_typ, 'str') or { return none }
	}
	if t.semantic_type_name(info.return_type) != flat.source_type_string {
		return none
	}
	// String interpolation synthesizes this call after the source-context maps
	// have been built. Encode the exact receiver specialization in the callee and
	// call metadata so a later monomorphizer instance can materialize its body.
	method_name, concrete_args_source_ids, decl_key := t.generic_str_method_specialization(info.name,
		clean_typ, u32(0)) or { return none }

	t.mark_generic_str_method_specialization(method_name, concrete_args_source_ids)
	return t.make_generic_str_method_call(method_name, decl_key, [expr])
}

// generic_str_method_specialization infers the generic declaration arguments from the exact
// receiver pattern. In particular, `(box Box[[]T])` on `Box[[]int]` specializes T as `int`, not
// `[]int`. V1 also permits a generic str method on a concrete alias receiver; in that case its
// method arguments are taken positionally from the alias target (`Ints = []int` gives T = int).
fn (mut t Transformer) generic_str_method_specialization(fn_name string, receiver_type_id u32, alias_target_id u32) ?(string, []u32, string) {
	receiver_type := flat.source_type_name(receiver_type_id)
	decls := t.cached_generic_fn_decls()
	mut decl_key := fn_name
	decl := decls[decl_key] or {
		decl_key = generic_fn_decl_base_value(fn_name)
		decls[decl_key] or {
			base, _, is_generic := generic_app_parts(receiver_type.trim_left('&'))
			lookup_base := if is_generic { base } else { receiver_type.trim_left('&') }
			decl_key = t.generic_receiver_decl_key(lookup_base, 'str', decls)
			decls[decl_key] or { return none }
		}
	}
	params := t.generic_fn_param_names(decl.node, decl.module)
	if params.len == 0 {
		return none
	}
	mut inferred := map[string]u32{}
	for i in 0 .. decl.node.children_count {
		param := t.a.child_node(&decl.node, i)
		if param.kind != .param {
			if t.prefix_param_scan {
				break
			}
			continue
		}
		infer_generic_type_args(generic_inference_param_type(param), receiver_type_id, mut inferred)
		break
	}
	if inferred.len < params.len && alias_target_id != u32(0) {
		alias_args := generic_str_alias_target_args(alias_target_id)
		if alias_args.len == params.len {
			for i, param in params {
				if param !in inferred {
					inferred[param] = alias_args[i]
				}
			}
		}
	}
	mut concrete_args := []u32{cap: params.len}
	for param in params {
		arg_source_id := inferred[param] or { return none }

		concrete_args << t.generic_arg_for_call_and_decl_module(arg_source_id, t.cur_module, decl.module)
	}
	concrete_args = t.canonical_generic_specialization_args(concrete_args).clone()
	if concrete_args.len != params.len || t.generic_args_have_placeholders(concrete_args) {
		return none
	}
	spec_value := specialized_generic_fn_value(decl.node.value, concrete_args)
	return transform_qualified_fn_name(decl.module, spec_value), concrete_args, decl_key
}

fn generic_str_alias_target_args(target_id u32) []u32 {
	return flat.TypeTextList.from_names(generic_str_alias_target_args_bytes(flat.source_type_name(target_id))).ids
}

fn generic_str_alias_target_args_bytes(target string) []string {
	clean := trimmed_transform_text(target)
	_, args, is_generic := generic_app_parts(clean)
	if is_generic && args.len > 0 {
		return args
	}
	if clean.starts_with('[]') && clean.len > 2 {
		return [trimmed_transform_text(clean[2..])]
	}
	return []string{}
}

fn (mut t Transformer) mark_generic_str_method_specialization(method_name string, args_ids []u32) {
	t.mark_fn_used_name(method_name)
	if args_ids.len == 0 || isnil(t.tc) { return }
	// A parallel transform worker shares the master's signature tables until it
	// detaches them. Writing straight into the shared map races with the other
	// workers, and leaves the master holding a key owned by this worker's
	// disposable arena once the batch is merged and that arena is released.
	t.tc.ensure_private_transform_signatures()
	t.tc.specialized_generic_fns[method_name] = true
	t.tc_signature_names_log << method_name
	t.record_generic_specialization_args_for_names([method_name, c_name(method_name)], args_ids)
}

// append_string builds `result = result + piece` using the runtime string concat helper.
// Using string__plus directly (instead of `+=`) keeps the synthesized node independent of
// type resolution for the freshly-introduced temp.
fn (mut t Transformer) append_string(result_name string, piece flat.NodeId) flat.NodeId {
	concat := t.make_call_typed('string__plus', [t.make_ident(result_name), piece], flat.source_type_string)
	return t.make_assign(t.make_ident(result_name), concat)
}

// lower_array_str expands `${arr}` for a `[]T` into a runtime loop that formats each element
// via wrap_string_conversion, so nested arrays, structs with `str`, enums, etc. all recurse
// correctly. Produces `[e0, e1, ...]`; string elements are wrapped in single quotes to match V.
fn (mut t Transformer) lower_array_str(arr_expr flat.NodeId, base_type_id u32) flat.NodeId {
	return t.lower_array_str_impl(arr_expr, base_type_id, false)
}

fn (mut t Transformer) lower_fixed_array_str(arr_expr flat.NodeId, base_type_id u32) flat.NodeId {
	return t.lower_array_str_impl(arr_expr, base_type_id, true)
}

fn (mut t Transformer) lower_array_str_impl(arr_expr flat.NodeId, base_type_id u32, fixed_pointer_style bool) flat.NodeId {
	base_type := flat.source_type_name(base_type_id)
	src := t.a.nodes[int(arr_expr)]
	mut elem_type := base_type[2..]
	if recorded_elem_type_source_id := t.recorded_array_call_elem_type(arr_expr) {
		recorded_elem_type := flat.source_type_name(recorded_elem_type_source_id)

		elem_type = recorded_elem_type
	}
	if src.kind == .ident {
		mut declared_type := t.raw_var_type(src.value)
		if !flat.source_type_shape(declared_type).has(.array_type) {
			declared_type = t.var_type(src.value)
		}
		if flat.source_type_shape(declared_type).has(.array_type) && flat.source_type_name(declared_type).len > 2 {
			elem_type = flat.source_type_name(declared_type)[2..]
			// A declaration spelling (`tokens []token.Kind`) names its enum through the
			// imports of the file that wrote it. `enum_autostr_type_name` does not read
			// them, so resolve the spelling here before the element is formatted.
			if spelling := t.declared_var_spelling(src.value) {
				if spelling == flat.source_type_name(declared_type) {
					elem_type = flat.source_type_name(t.source_enum_elem_type(t.node_file_or(int(arr_expr),
						t.cur_file), flat.source_type_id(elem_type)))
				}
			}
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
			if flat.source_type_shape(field_base_type).has(.pointer) {
				field_base_type = flat.source_type_id(flat.source_type_name(field_base_type)[1..])
			}
			for flat.source_type_shape(field_base_type).has(.shared_type) {
				field_base_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(field_base_type)[7..]))
			}
			if field_type_source_id := t.lookup_struct_field_type(field_base_type, selector.value) {
				field_type_source_id_3 := field_type_source_id

				selector_type = field_type_source_id_3
			} else if raw_type_source_id := t.lookup_struct_field_raw_type(field_base_type, selector.value) {
				raw_type_source_id_2 := raw_type_source_id

				selector_type = raw_type_source_id_2
			}
		}
		for flat.source_type_shape(selector_type).has(.shared_type) {
			selector_type = flat.source_type_id(trimmed_transform_text(flat.source_type_name(selector_type)[7..]))
		}
		if flat.source_type_shape(selector_type).has(.array_type) && flat.source_type_name(selector_type).len > 2 {
			elem_type = flat.source_type_name(selector_type)[2..]
		}
	}
	base := if _ := t.generated_variant_access_type(arr_expr) {
		t.stable_transformed_expr_for_reuse(arr_expr, base_type_id, 'arr')
	} else {
		t.stable_expr_for_reuse(arr_expr)
	}
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('arr_str')
	idx_name := t.new_temp('arr_str_idx')
	if !fixed_pointer_style {
		address := t.make_selector(base, 'data', flat.source_type_voidptr)
		address_type := t.make_int_literal(t.type_index(base_type_id, t.cur_module))
		prefix << t.make_expr_stmt(t.make_call_typed('autostr_addr_type_push', [
			address,
			address_type,
		], flat.source_type_void))
	}
	prefix << t.make_decl_assign_typed(result_name, t.make_string_literal('['), flat.source_type_string)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', flat.source_type_int))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_name := t.new_temp('arr_str_it')
	elem_expr := t.array_get_value(base, t.make_ident(idx_name), flat.source_type_id(elem_type))
	elem_decl := t.make_decl_assign_typed(elem_name, elem_expr, flat.source_type_id(elem_type))
	mut loop_body := []flat.NodeId{}
	loop_body << elem_decl
	// `if idx > 0 { result = result + ', ' }`
	sep_cond := t.make_infix(.gt, t.make_ident(idx_name), t.make_int_literal(0))
	sep_stmt := t.append_string(result_name, t.make_string_literal(', '))
	loop_body << t.make_if(sep_cond, t.make_block([sep_stmt]), t.make_empty())
	// element text (recurses; may push its own statements for nested arrays/optionals)
	t.set_var_type(elem_name, flat.source_type_id(elem_type))
	elem_str := if t.array_elem_str_is_direct_circular(flat.source_type_id(elem_type)) {
		t.make_string_literal('<circular>')
	} else if fixed_pointer_style && flat.source_type_shape(t.normalize_type_alias(flat.source_type_id(elem_type))).has(.pointer) {
		pointer_type_source_id := t.normalize_type_alias(flat.source_type_id(elem_type))
		pointer_type := flat.source_type_name(pointer_type_source_id)
		t.lower_ref_value_str(t.make_ident(elem_name), pointer_type_source_id,
			'&'.repeat(dump_pointer_depth(pointer_type)) + '0')
	} else if elem_type.starts_with('&') {
		t.lower_array_ref_str(t.make_ident(elem_name), flat.source_type_id(elem_type))
	} else {
		t.wrap_string_conversion(t.make_ident(elem_name), flat.source_type_id(elem_type))
	}
	t.unset_var_type(elem_name)
	t.drain_pending(mut loop_body)
	// Quote string/rune elements, including aliases of them (`type Literal = string`).
	quote_elem_source_id := t.normalize_type_alias(flat.source_type_id(elem_type))

	if quote_elem_source_id == flat.source_type_string {
		loop_body << t.append_string(result_name, t.make_string_literal("'"))
		loop_body << t.append_string(result_name, elem_str)
		loop_body << t.append_string(result_name, t.make_string_literal("'"))
	} else if quote_elem_source_id == flat.source_type_rune {
		loop_body << t.append_string(result_name, t.make_string_literal('`'))
		loop_body << t.append_string(result_name, elem_str)
		loop_body << t.append_string(result_name, t.make_string_literal('`'))
	} else {
		loop_body << t.append_string(result_name, elem_str)
	}
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	prefix << t.append_string(result_name, t.make_string_literal(']'))
	if !fixed_pointer_style {
		prefix << t.make_expr_stmt(t.make_call_typed('autostr_addr_pop', [], flat.source_type_void))
	}
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (t &Transformer) array_elem_str_is_direct_circular(elem_type_id u32) bool {
	if t.stringify_stack.len == 0 {
		return false
	}
	aggregate_source_id := t.stringify_aggregate_type_name(elem_type_id) or { return false }
	current := t.stringify_stack.last()
	if !t.stringify_types_match(aggregate_source_id, current) {
		return false
	}
	return !t.struct_autostr_allows_recurse(aggregate_source_id)
}

fn (t &Transformer) recorded_array_call_elem_type(call_id flat.NodeId) ?u32 {
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
		if args_source_ids := t.recorded_type_specialization_args(flat.source_type_id(receiver)) {
			if args_source_ids.len == 1 {
				return args_source_ids[0]
			}
		}
	}
	if callee.kind == .selector && callee.children_count > 0 {
		if callee.value != 'array' {
			return none
		}
		base_id := t.a.child(callee, 0)
		mut receiver_type := t.node_type(base_id)
		if flat.source_type_shape(receiver_type).has(.pointer) {
			receiver_type = flat.source_type_id(flat.source_type_name(receiver_type)[1..])
		}
		if args_source_ids := t.current_specialized_receiver_args(receiver_type) {
			if args_source_ids.len == 1 {
				return args_source_ids[0]
			}
		}
		if args_source_ids := t.recorded_type_specialization_args(receiver_type) {
			if args_source_ids.len == 1 {
				return args_source_ids[0]
			}
		}
		_, generic_args, is_generic_receiver := generic_app_parts(flat.source_type_name(receiver_type))
		if is_generic_receiver && generic_args.len == 1 {
			if stringify_type_has_generic_placeholder(generic_args[0]) {
				return none
			}
			return flat.source_type_id(generic_args[0])
		}
	}
	return none
}

fn (t &Transformer) current_specialized_receiver_args(receiver_type_id u32) ?[]u32 {
	receiver_type := flat.source_type_name(receiver_type_id)
	base, args, ok := generic_app_parts(receiver_type)
	if !ok || args.len == 0 || !t.generic_args_have_placeholders(flat.TypeTextList.from_names(args).ids) {
		return none
	}
	current_receiver_source_id := t.current_fn_receiver_type()
	current_receiver := flat.source_type_name(current_receiver_source_id)
	if current_receiver_source_id == 0
		|| !current_receiver_matches_open_generic_base(current_receiver, base) {
		return none
	}
	if recorded_source_ids := t.recorded_type_specialization_args(current_receiver_source_id) {
		if t.generic_args_have_placeholders(recorded_source_ids) {
			return none
		}
		return recorded_source_ids.clone()
	}
	_, current_args, current_ok := generic_app_parts(current_receiver)
	if current_ok && current_args.len == args.len {
		canonical_args_source_ids := t.canonical_generic_specialization_args(flat.TypeTextList.from_names(current_args).ids)

		if t.generic_args_have_placeholders(canonical_args_source_ids) {
			return none
		}
		return canonical_args_source_ids.clone()
	}
	return none
}

fn (t &Transformer) current_fn_receiver_type() u32 {
	name := trimmed_transform_text(t.cur_fn_name)
	if name.len == 0 {
		return u32(0)
	}
	if name.contains('.') {
		return flat.source_type_id(name.all_before_last('.'))
	}
	if name.contains('__') {
		return flat.source_type_id(name.all_before_last('__'))
	}
	return u32(0)
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

fn (mut t Transformer) lower_map_str(map_expr flat.NodeId, map_type_id u32) flat.NodeId {
	key_type_source_id_3, raw_value_type_source_id_2 := t.map_type_parts(map_type_id)

	raw_value_type_source_id_4 := raw_value_type_source_id_2

	fixed_value_type_source_id_2 := t.fixed_array_map_value_type_text(raw_value_type_source_id_2)
	fixed_value_type_source_id_4 := fixed_value_type_source_id_2

	value_type := (if fixed_value_type_source_id_2 != 0 {
		fixed_value_type_source_id_4
	} else {
		raw_value_type_source_id_4
	})
	if key_type_source_id_3 == 0 || value_type == 0 {
		return map_expr
	}
	key_kind := t.map_str_kind_for_type(key_type_source_id_3)
	value_kind := t.map_str_kind_for_type(value_type)
	if t.map_str_types_need_typed_lowering(key_type_source_id_3, value_type) {
		return t.lower_typed_map_str(map_expr, map_type_id, key_type_source_id_3, value_type)
	}
	lowered := if t.expr_is_transformed_deref(map_expr) {
		t.stable_transformed_expr_for_reuse(map_expr, map_type_id, 'map_str_base')
	} else {
		t.transform_expr_for_type(map_expr, map_type_id)
	}
	return t.make_call_typed('v3_map_str', [lowered, t.make_int_literal(key_kind),
		t.make_int_literal(value_kind), t.make_int_literal(t.map_str_fixed_len_for_type(value_type))], flat.source_type_string)
}

fn (t &Transformer) map_str_types_need_typed_lowering(key_type_id u32, value_type_id u32) bool {
	key_kind := t.map_str_kind_for_type(key_type_id)
	value_kind := t.map_str_kind_for_type(value_type_id)
	if key_kind != 0 && value_kind != 0 {
		return false
	}
	key_has_conversion := key_kind != 0 || t.map_str_type_has_transform_conversion(key_type_id)
	value_has_conversion := value_kind != 0 || t.map_str_type_has_transform_conversion(value_type_id)
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
fn (mut t Transformer) lower_typed_map_str(map_expr flat.NodeId, map_type_id u32, key_type_id u32, value_type_id u32) flat.NodeId {
	src := t.a.nodes[int(map_expr)]
	base := t.stable_expr_for_reuse(map_expr)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('map_str')
	keys_name := t.new_temp('map_str_keys')
	idx_name := t.new_temp('map_str_idx')
	key_name := t.new_temp('map_str_key')
	value_name := t.new_temp('map_str_value')
	key_kind := t.map_str_kind_for_type(key_type_id)
	value_kind := t.map_str_kind_for_type(value_type_id)
	key_storage_type_source_id := t.map_key_storage_type(key_type_id)
	key_storage_type := flat.source_type_name(key_storage_type_source_id)
	keys_type := flat.source_type_id('[]${key_storage_type}')
	keys_call := t.make_call_typed('map__keys', [t.runtime_addr(base, map_type_id)], keys_type)
	prefix << t.make_decl_assign_typed(result_name, t.make_string_literal('{'), flat.source_type_string)
	prefix << t.make_decl_assign_typed(keys_name, keys_call, keys_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(keys_name), 'len', flat.source_type_int))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	key_expr := t.array_get_value(t.make_ident(keys_name), t.make_ident(idx_name), key_storage_type_source_id)
	key_decl := t.make_decl_assign_typed(key_name, key_expr, key_storage_type_source_id)
	mut loop_body := []flat.NodeId{}
	loop_body << key_decl
	value_expr := t.make_map_lookup_value(base, map_type_id, key_name, value_type_id, mut loop_body)
	value_decl := t.make_decl_assign_typed(value_name, value_expr, value_type_id)
	loop_body << value_decl
	sep_cond := t.make_infix(.gt, t.make_ident(idx_name), t.make_int_literal(0))
	sep_stmt := t.append_string(result_name, t.make_string_literal(', '))
	loop_body << t.make_if(sep_cond, t.make_block([sep_stmt]), t.make_empty())
	key_str := t.map_str_loop_piece(key_name, key_type_id, key_kind, 0)
	t.drain_pending(mut loop_body)
	loop_body << t.append_string(result_name, key_str)
	loop_body << t.append_string(result_name, t.make_string_literal(': '))
	value_str := t.map_str_loop_piece(value_name, value_type_id, value_kind, t.map_str_fixed_len_for_type(value_type_id))
	t.drain_pending(mut loop_body)
	loop_body << t.append_string(result_name, value_str)
	prefix << t.make_for_stmt(init, cond, post, loop_body, src)
	prefix << t.append_string(result_name, t.make_string_literal('}'))
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

fn (mut t Transformer) map_str_loop_piece(name string, typ_id u32, kind int, fixed_len int) flat.NodeId {
	typ_shape := flat.source_type_shape(typ_id)
	if kind != 0 {
		return t.make_call_typed('v3_map_str_piece', [
			t.make_prefix(.amp, t.make_ident(name)),
			t.make_int_literal(kind),
			t.make_sizeof_type(typ_id),
			t.make_int_literal(fixed_len),
		], flat.source_type_string)
	}
	t.set_var_type(name, typ_id)
	piece := if typ_shape.has(.pointer) {
		t.lower_ref_value_str(t.make_ident(name), typ_id, 'nil')
	} else {
		t.wrap_string_conversion(t.make_ident(name), typ_id)
	}
	t.unset_var_type(name)
	return piece
}

fn (t &Transformer) map_str_type_has_transform_conversion(typ_id u32) bool {
	mut clean := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(typ_id)))
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
	if t.is_optional_type_name(flat.source_type_id(clean)) {
		return true
	}
	if (t.resolve_interface_type_name(flat.source_type_id(clean))) != 0 {
		return true
	}
	if clean in ['string', 'rune', 'bool', 'i8', 'i16', 'i32', 'i64', 'int', 'isize', 'u8', 'u16',
		'u32', 'u64', 'usize', 'i128', 'u128', 'f32', 'f64', 'int literal', 'float literal', 'voidptr',
		'byteptr', 'charptr', 'IError'] {
		return true
	}
	if clean in t.enum_types || clean in t.structs || t.sum_types.contains(clean) {
		return true
	}
	if _ := t.generic_struct_info_for_stringify(flat.source_type_id(clean)) {
		return true
	}
	if !clean.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${clean}'
		if qname in t.enum_types || qname in t.structs || t.sum_types.contains(qname) {
			return true
		}
	}
	return t.is_fixed_array_type(flat.source_type_id(clean)) || clean.starts_with('[]') || clean.starts_with('map[')
}

fn (t &Transformer) map_str_kind_for_type(typ_id u32) int {

	// Packed enums have an integer storage type, but map stringification must use
	// their enum names (or custom str method), not the underlying integer values.
	if t.is_enum_stringify_type(typ_id) {
		return 0
	}
	mut clean := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(typ_id)))
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
		'usize', 'u8', 'u16', 'u32', 'u64' {
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
				elem_source_id := t.normalize_type_alias(fixed_array_elem_type(flat.source_type_id(clean)))

				if elem_source_id == flat.source_type_f32 {
					return 9
				}
				if elem_source_id == flat.source_type_f64 {
					return 6
				}
			}
			return 0
		}
	}
}

fn (t &Transformer) map_str_fixed_len_for_type(typ_id u32) int {
	clean := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(typ_id)))
	if transform_type_text_is_fixed_array(clean) {
		return fixed_array_len(clean)
	}
	return 0
}

// wrap_optional_string_conversion transforms wrap optional string conversion data for transform.
fn (mut t Transformer) wrap_optional_string_conversion(expr flat.NodeId, typ_id u32, is_ref bool) flat.NodeId {
	return t.wrap_optional_string_conversion_with_pointer_none(expr, typ_id, is_ref, true)
}

// wrap_optional_string_conversion_with_pointer_none lets struct auto-str retain
// `&Option(none)` for an empty pointer field while standalone `?&T` prints `&Option(&nil)`.
fn (mut t Transformer) wrap_optional_string_conversion_with_pointer_none(expr flat.NodeId, typ_id u32, is_ref bool, pointer_none_as_nil bool) flat.NodeId {
	opt_type_source_id := t.qualify_optional_type(typ_id)

	mut value_type := t.optional_base_type(opt_type_source_id)
	if value_type == 0 || value_type == flat.source_type_void {
		value_type = flat.source_type_int
	}
	opt_name := t.new_temp('opt_str')
	res_name := t.new_temp('opt_str_text')
	t.pending_stmts << t.make_decl_assign_typed(opt_name, t.transform_optional_wrapper_expr(expr), opt_type_source_id)
	pointer_payload := flat.source_type_shape(value_type).has(.pointer)
	option_prefix := if pointer_payload || is_ref { '&Option(' } else { 'Option(' }
	t.pending_stmts << t.make_decl_assign_typed(res_name, t.make_string_literal('${option_prefix}none)'), flat.source_type_string)
	if is_ref || (pointer_payload && pointer_none_as_nil) {
		t.pending_stmts << t.make_assign(t.make_ident(res_name), t.make_string_literal('${option_prefix}&nil)'))
	}
	value := t.make_selector(t.make_ident(opt_name), 'value', value_type)
	display_value := if pointer_payload {
		deref := t.make_prefix(.mul, value)
		t.set_node_typ(int(deref), flat.source_type_id(flat.source_type_name(value_type)[1..]))
		deref
	} else {
		value
	}
	display_type := (if pointer_payload {
		flat.source_type_id(flat.source_type_name(value_type)[1..])
	} else {
		value_type
	})
	// Rendering the payload emits statements of its own -- a struct payload copies
	// itself into a temp, a `&T` payload dereferences first. Those belong inside the
	// `ok` branch: hoisted in front of the check they run even for `none`, and
	// `*opt.value` on a null payload pointer segfaults before anything is printed.
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	mut value_str := t.wrap_string_conversion(display_value, display_type)
	if display_type == flat.source_type_string {
		value_str = t.string_plus(t.string_plus(t.make_string_literal("'"), value_str), t.make_string_literal("'"))
	}
	if alias_display := t.optional_payload_alias_display_name(display_type) {
		value_str = t.string_plus(t.string_plus(t.make_string_literal('${alias_display}('), value_str), t.make_string_literal(')'))
	}
	if !isnil(t.tc) {
		iface_name_source_id_3 := t.resolve_interface_type_name(display_type)
		iface_name := flat.source_type_name(iface_name_source_id_3)
		if iface_name_source_id_3 != 0 && 'str' in t.tc.interface_abstract_method_names(iface_name_source_id_3) {
			display_name := iface_name.all_after_last('.')
			value_str = t.string_plus(t.string_plus(t.make_string_literal('${display_name}('),
				value_str), t.make_string_literal(')'))
		}
	}
	some_str := t.string_plus(t.string_plus(t.make_string_literal(option_prefix), value_str), t.make_string_literal(')'))
	mut some_stmts := []flat.NodeId{}
	t.drain_pending(mut some_stmts)
	some_stmts << t.make_assign(t.make_ident(res_name), some_str)
	t.pending_stmts = outer_pending
	t.pending_stmts << t.make_if(t.make_selector(t.make_ident(opt_name), 'ok', flat.source_type_bool), t.make_block_skip_scope_drops(some_stmts), t.make_empty())
	return t.make_ident(res_name)
}

// string_plus supports string plus handling for Transformer.
fn (mut t Transformer) string_plus(left flat.NodeId, right flat.NodeId) flat.NodeId {
	return t.make_call_typed('string__plus', [left, right], flat.source_type_string)
}

// is_flag_enum_type reports whether is flag enum type applies in transform.
fn (t &Transformer) is_flag_enum_type(typ_id u32) bool {
	return t.resolve_flag_enum_type_name(typ_id) != none
}

fn (t &Transformer) resolve_flag_enum_type_name(typ_id u32) ?u32 {
	typ := flat.source_type_name(typ_id)
	mut clean := typ
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.len == 0 {
		return none
	}
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(flat.source_type_id(clean))
		if parsed is types.Enum {
			if parsed.is_flag {
				return parsed.source_type_id()
			}
			// A known ordinary enum must not inherit another module's flag methods.
			return none
		}
		if clean in t.tc.flag_enums {
			return flat.source_type_id(clean)
		}
		qualified := t.tc.qualify_name(clean)
		if qualified in t.tc.flag_enums {
			return flat.source_type_id(qualified)
		}
		if !clean.contains('.') {
			mut found := ''
			for name, _ in t.tc.flag_enums {
				if name.all_after_last('.') != clean {
					continue
				}
				if found != '' && found != name {
					return none
				}
				found = name
			}
			if found.len > 0 {
				return flat.source_type_id(found)
			}
		}
	}
	return none
}

fn (t &Transformer) flag_enum_mask_for_type(typ_id u32) int {
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	for clean.starts_with('&') {
		clean = trimmed_transform_text(clean[1..])
	}
	members := t.comptime_enum_members(flat.source_type_id(clean))
	mut mask := 0
	for member in members {
		mask |= int(member.value)
	}
	return mask
}

fn (t &Transformer) flag_enum_has_backing_type(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	mut clean := trimmed_transform_text(typ)
	for clean.starts_with('&') {
		clean = trimmed_transform_text(clean[1..])
	}
	normalized := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(flat.source_type_id(clean))))
	for candidate in [normalized, clean] {
		if candidate.len == 0 {
			continue
		}
		if _ := t.enum_backing_types.type_id(candidate) {
			return true
		}
		if !candidate.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
			&& t.cur_module != 'builtin' {
			if _ := t.enum_backing_types.type_id('${t.cur_module}.${candidate}') {
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
	owner_type := flat.source_type_name(t.node_type(owner_id)).trim_left('&')
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
	base_type_source_id := t.node_type(base_id)

	if !t.is_flag_enum_type(base_type_source_id) {
		return none
	}
	if fn_node.value == 'set_all' && t.flag_enum_has_backing_type(base_type_source_id) {
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
			mask := t.flag_enum_mask_for_type(base_type_source_id)
			return t.make_assign(base, t.make_int_literal_typed(mask.str(), base_type_source_id))
		}
		'clear_all' {
			return t.make_assign(base, t.make_int_literal_typed('0', base_type_source_id))
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
		if (t.var_type(base_node.value)) != 0 {
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
		flag_type_source_id := t.resolve_flag_enum_type_name(flat.source_type_id(base_node.value)) or { return none }

		return t.make_cast(flag_type_source_id, t.make_int_literal(0), flag_type_source_id)
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
	if base_type == 0 {
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
	raw_base_type u32
	base_type     u32
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
	if raw_base_type == 0 {
		raw_base_type = t.lvalue_type(base_id)
	}
	mut base_type := flat.source_type_name(raw_base_type)
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if base_type.starts_with('[]') || base_type.starts_with('map[') || base_type == 'string'
		|| t.is_fixed_array_type(flat.source_type_id(base_type)) {
		return none
	}
	if isnil(t.tc) {
		return none
	}
	parsed_base_type := t.tc.parse_type(flat.source_type_id(base_type))
	is_default_sum_clone := t.is_sum_type_name(flat.source_type_id(base_type))
		&& t.tc.ownership_default_clone_missing_method(parsed_base_type) == none
	if !t.tc.named_type_implements_marker(flat.source_type_id(base_type), flat.source_type_id('IClone')) && !is_default_sum_clone {
		return none
	}
	// A concrete generic receiver can have a handwritten clone() on its open
	// generic declaration. Leave that call intact for monomorphization instead
	// of replacing it with the compiler-provided field clone.
	if base_type.contains('[') && base_type.ends_with(']') {
		if _ := t.tc.resolve_generic_struct_method(flat.source_type_id(base_type), 'clone') {
			return none
		}
	}
	// A user-defined clone() would have been lowered already; only supply the default clone.
	if t.tc.ownership_type_has_clone_method(parsed_base_type)
		&& t.resolve_receiver_method_name(base_id, 'clone').len > 0 {
		return none
	}
	return CompilerDefaultCloneCallInfo{
		base_id:       base_id
		raw_base_type: raw_base_type
		base_type:     flat.source_type_id(base_type)
		can_lower:     t.tc.ownership_default_clone_missing_method(parsed_base_type) == none
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
	// Heap-promoted value locals already dereference when read as rvalues.
	// Preserve their storage pointer here so the clone reads the value once.
	mut receiver := t.transform_expr_preserving_pointer_value(t.unwrap_parens(info.base_id))
	if flat.source_type_shape(info.raw_base_type).has(.pointer) {
		receiver = t.make_prefix(.mul, receiver)
		t.set_node_typ(int(receiver), info.base_type)
	}
	return t.make_compiler_default_clone_value(receiver, info.base_type, false)
}

// make_compiler_default_borrowed_clone_value prevents a non-owning borrowed projection from
// being cleaned up as though it were an owned rvalue after cloning reads it.
fn (mut t Transformer) make_compiler_default_borrowed_clone_value(source flat.NodeId, typ_id u32, allow_method bool) flat.NodeId {
	mut stable_source := source
	clean := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(typ_id)))
	// Transforming a borrowed projection can produce a non-addressable shallow value.
	// Stabilize every such value so clone lowering borrows it rather than mistaking it for
	// an owned rvalue that must be destroyed after cloning.
	if !t.expr_can_take_address(source) {
		source_name := t.new_temp('borrowed_clone_source')
		t.pending_stmts << t.make_decl_assign_typed(source_name, source, flat.source_type_id(clean))
		stable_source = t.make_ident(source_name)
	}
	return t.make_compiler_default_clone_value(stable_source, typ_id, allow_method)
}

// make_compiler_default_clone_value recursively clones the storage-owning fields of a
// compiler-provided IClone value. The initial aggregate copy preserves scalar/reference
// fields, and each owning field is then replaced with its independent clone.
fn (mut t Transformer) make_compiler_default_clone_value(source flat.NodeId, typ_id u32, allow_method bool) flat.NodeId {
	clean := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(typ_id)))
	if clean.len == 0 || clean.starts_with('&') {
		return source
	}
	// Enum values are scalars. A different module may also define a struct with
	// the same short name, which the struct lookup fallback could select here.
	if !isnil(t.tc) && types.unalias_type(t.tc.parse_type(flat.source_type_id(clean))) is types.Enum {
		return source
	}
	if clean == 'thread' || clean.starts_with('thread ') || clean == 'chan'
		|| clean.starts_with('chan ') {
		return source
	}
	// Options and results both store successful values behind the `ok` flag.
	if clean.starts_with('?') || clean.starts_with('!') {
		inner_source_id := t.optional_base_type(t.qualify_optional_type(flat.source_type_id(clean)))

		inner_needs_work := t.compiler_default_clone_type_needs_work(inner_source_id)
		source_is_owned_temporary := !t.expr_can_take_address(source)
		stable_source := t.stable_transformed_expr_for_reuse(source, flat.source_type_id(clean), 'derived_clone_opt_source')
		out_name := t.new_temp('derived_clone_opt')
		t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_optional_none(flat.source_type_id(clean)), flat.source_type_id(clean))
		pending_start := t.pending_stmts.len
		cloned_value := if inner_needs_work {
			t.make_compiler_default_clone_value(t.make_selector(stable_source, 'value', inner_source_id), inner_source_id, true)
		} else if inner_source_id != 0 && inner_source_id != flat.source_type_void {
			t.make_selector(stable_source, 'value', inner_source_id)
		} else {
			t.make_empty()
		}
		mut body := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		body << t.make_assign_without_ownership_drop(t.make_ident(out_name), t.make_optional_some(cloned_value, flat.source_type_id(clean)))
		mut else_branch := t.make_empty()
		cloned_err := t.clone_result_error(stable_source)
		if int(cloned_err) >= 0 {
			failed := t.make_optional_none_with_err(flat.source_type_id(clean), cloned_err)
			assignment := t.make_assign_without_ownership_drop(t.make_ident(out_name), failed)
			else_branch = t.make_block_skip_scope_drops([assignment])
		}
		t.pending_stmts << t.make_if_with_skip_ownership_drops(t.make_selector(stable_source, 'ok', flat.source_type_bool), t.make_block_skip_scope_drops(body), else_branch)
		if source_is_owned_temporary {
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				stable_source,
			], flat.source_type_void))
		}
		return t.make_ident(out_name)
	}
	if clean == 'string' {
		t.mark_fn_used('string__clone')
		return t.make_call_typed('string__clone', [source], flat.source_type_string)
	}
	// A borrowed error, such as a module-level `IError` constant, is copied with the
	// dynamic error clone: whoever receives the copy may free it, never the shared original.
	if clean in ['IError', 'builtin.IError'] && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(flat.source_type_id(clean))) {
		return t.clone_owned_ierror(source)
	}
	if clean.starts_with('[]') {
		return t.make_compiler_default_array_clone_value(source, flat.source_type_id(clean), !t.expr_can_take_address(source))
	}
	if t.is_fixed_array_type(flat.source_type_id(clean)) {
		return t.make_compiler_default_fixed_array_clone_value(source, flat.source_type_id(clean))
	}
	if clean.starts_with('map[') {
		return t.make_compiler_default_map_clone_value(source, flat.source_type_id(clean), !t.expr_can_take_address(source))
	}
	if allow_method && !isnil(t.tc) && t.tc.ownership_type_has_clone_method(t.tc.parse_type(flat.source_type_id(clean))) {
		// A private helper can become reachable after generic specialization has
		// already materialized its field's clone method. Use that resolved method
		// before synthesizing a selector that the initial call scan cannot visit.
		method_name := t.resolve_receiver_method_name(source, 'clone')
		if method_name.len > 0 {
			params := t.call_param_types(method_name)
			mut receiver := source
			if params.len > 0 && flat.source_type_shape(t.semantic_type_name(params[0])).has(.pointer) {
				receiver = t.runtime_addr(source, flat.source_type_id(clean))
			}
			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, [receiver], t.receiver_method_return_type(method_name, flat.source_type_id(clean)))
		}
		if clean.contains('[') && clean.ends_with(']') {
			if _ := t.tc.resolve_generic_struct_method(flat.source_type_id(clean), 'clone') {
				call := t.make_method_call(source, 'clone', []flat.NodeId{})
				t.set_node_typ(int(call), flat.source_type_id(clean))
				return call
			}
		}
	}
	// Without a `clone()` of its own (dispatched above), a union is copied bitwise.
	if t.type_is_union(flat.source_type_id(clean)) {
		return source
	}
	if isnil(t.tc) || (!t.tc.named_type_implements_marker(flat.source_type_id(clean), flat.source_type_id('IClone'))
		&& t.tc.ownership_default_clone_missing_method(t.tc.parse_type(flat.source_type_id(clean))) != none) {
		return source
	}
	if flat.source_type_id(clean) in t.default_clone_expansion_stack {
		return t.request_default_clone_helper(source, flat.source_type_id(clean))
	}
	t.default_clone_expansion_stack << flat.source_type_id(clean)
	defer {
		t.default_clone_expansion_stack.delete_last()
	}
	if t.is_sum_type_name(flat.source_type_id(clean)) {
		// Call the shared clone helper rather than inlining the variant switch at every read
		// site: a recursive sum type such as `toml.Any` would otherwise expand a large switch
		// (one arm per variant, recursively) at each site and overflow the AST. The helper body
		// is synthesized exactly once (see build_default_clone_helper_fn).
		return t.request_default_clone_helper(source, flat.source_type_id(clean))
	}
	info := t.lookup_struct_info(flat.source_type_id(clean)) or { return source }
	mut owning_fields := []FieldInfo{}
	for field in info.fields {
		field_type_source_id := t.compiler_default_clone_field_type(flat.source_type_id(clean), field)

		if t.compiler_default_clone_type_needs_work(field_type_source_id) {
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
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, source, flat.source_type_id(clean))
	for field in owning_fields {
		field_type_source_id_2 := t.compiler_default_clone_field_type(flat.source_type_id(clean), field)

		source_field := t.make_selector(t.make_ident(tmp_name), field.name, field_type_source_id_2)
		mut cloned_field := t.make_compiler_default_clone_value(source_field, field_type_source_id_2, true)
		if source_fields_are_owned {
			cloned_name := t.new_temp('derived_clone_field')
			t.pending_stmts << t.make_decl_assign_typed(cloned_name, cloned_field, field_type_source_id_2)
			drop_call := t.make_call_typed('drop_owned', [source_field], flat.source_type_void)
			t.pending_stmts << t.make_expr_stmt(drop_call)
			cloned_field = t.make_ident(cloned_name)
		}
		t.pending_stmts << t.make_assign_after_owned_drop(t.make_selector(t.make_ident(tmp_name), field.name, field_type_source_id_2), cloned_field)
	}
	return t.make_ident(tmp_name)
}

fn (mut t Transformer) make_compiler_default_sum_clone_value(source flat.NodeId, sum_type_id u32) flat.NodeId {
	// A generic sum instance (`Maybe[&Item]`) lists its declaration's variants (`T`);
	// substitute them, or the clone names a `T` payload field the instance lacks.
	resolved_sum_source_id, variants := t.concrete_sum_name_and_variants(sum_type_id)
	if variants.len == 0 {
		return source
	}
	source_is_owned_temporary := !t.expr_can_take_address(source)
	stable_source := t.stable_transformed_expr_for_reuse(source, sum_type_id, 'derived_clone_sum_source')
	out_name := t.new_temp('derived_clone_sum')
	t.pending_stmts << t.make_decl_assign_typed(out_name, stable_source, sum_type_id)
	for variant in variants {
		qvariant_source_id_2 := t.resolve_variant(resolved_sum_source_id, variant)
		qvariant_source_id_3 := qvariant_source_id_2
		qvariant := flat.source_type_name(qvariant_source_id_3)
		if qvariant_source_id_2 == 0 {
			continue
		}
		field_type := qvariant_source_id_3
		mut payload :=
			t.make_selector_op(stable_source, sum_field_name(qvariant), field_type, .dot)
		pending_start := t.pending_stmts.len
		cloned_payload := t.make_compiler_default_clone_value(payload, qvariant_source_id_2, true)
		mut body := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		wrapped := t.make_sum_literal(resolved_sum_source_id, qvariant_source_id_2, cloned_payload)
		body << t.make_assign_without_ownership_drop(t.make_ident(out_name), wrapped)
		cond := t.make_sum_is_check(stable_source, sum_type_id, resolved_sum_source_id, qvariant_source_id_2)
		t.pending_stmts << t.make_if_with_skip_ownership_drops(cond, t.make_block_skip_scope_drops(body), t.make_empty())
	}
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], flat.source_type_void))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), sum_type_id)
	return result
}

fn default_clone_helper_name(typ_id u32) string {
	return '__v3_default_clone_${c_name(flat.source_type_name(typ_id))}'
}

fn (mut t Transformer) request_default_clone_helper(source flat.NodeId, typ_id u32) flat.NodeId {
	helper := default_clone_helper_name(typ_id)
	if typ_id !in t.default_clone_types {
		t.default_clone_types[typ_id] = DefaultCloneRequest{
			module: t.cur_module
			file:   t.cur_file
		}
	}
	t.mark_fn_used_name(helper)
	if t.expr_can_take_address(source) {
		address := t.runtime_addr(source, typ_id)
		argument := t.make_cast(flat.source_type_voidptr, address, flat.source_type_voidptr)
		return t.make_call_typed(helper, [argument], typ_id)
	}
	// The helper only borrows its pointer argument. Stabilize an owned rvalue ourselves so
	// the clone is saved before the original temporary is destroyed; runtime_addr's ordinary
	// compiler temporary is not tracked by ownership cleanup.
	source_name := t.new_temp('default_clone_source')
	stable_source := t.make_ident(source_name)
	t.pending_stmts << t.make_decl_assign_typed(source_name, source, typ_id)
	address := t.runtime_addr(stable_source, typ_id)
	argument := t.make_cast(flat.source_type_voidptr, address, flat.source_type_voidptr)
	cloned_name := t.new_temp('default_clone_result')
	t.pending_stmts << t.make_decl_assign_typed(cloned_name, t.make_call_typed(helper, [
		argument,
	], typ_id), typ_id)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
		stable_source,
	], flat.source_type_void))
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
	old_temp_counter := t.temp_counter
	was_log_active := t.used_fns_log_active
	log_start := t.used_fns_log.len
	t.used_fns_log_active = true
	for {
		mut pending := []u32{}
		for name, _ in t.default_clone_types {
			if name in t.default_clone_synthesized {
				continue
			}
			if default_clone_helper_name(name) in t.fn_ret_types.ids {
				t.default_clone_synthesized[name] = true
				continue
			}
			pending << name
		}
		if pending.len == 0 {
			break
		}
		pending.sort_with_compare(compare_helper_source_types)
		for name in pending {
			t.default_clone_synthesized[name] = true
			req := t.default_clone_types[name] or { DefaultCloneRequest{} }
			t.cur_module = req.module
			t.cur_file = req.file
			if !isnil(t.tc) {
				t.tc.cur_module = req.module
				t.tc.cur_file = req.file
			}
			// A helper is a function of its own: number its temporaries from zero,
			// not from whatever the transform lowered before it.
			t.temp_counter = 0
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
	t.temp_counter = old_temp_counter
	if !isnil(t.tc) {
		t.tc.cur_module = old_tc_module
		t.tc.cur_file = old_tc_file
	}
	return new_names
}

fn (mut t Transformer) build_default_clone_helper_fn(typ_id u32) {
	typ := flat.source_type_name(typ_id)
	helper := default_clone_helper_name(typ_id)
	saved_pending := t.pending_stmts
	saved_vars := t.var_types.clone()
	saved_fn_name := t.cur_fn_name
	saved_ret_type_source_id := t.cur_fn_ret_type

	saved_expansion_stack := t.default_clone_expansion_stack.clone()
	t.pending_stmts = []flat.NodeId{}
	t.reset_var_types()
	t.default_clone_expansion_stack = []u32{}
	t.cur_fn_name = helper
	t.set_cur_fn_ret_type(typ_id)
	param_name := '__default_clone_source'
	param := t.a.add_node(flat.Node{
		kind:  .param
		value: param_name
		typ:   flat.source_type_voidptr
	})
	t.set_var_type(param_name, flat.source_type_voidptr)
	typed_pointer := t.make_cast(flat.source_type_id('&${typ}'), t.make_ident(param_name), flat.source_type_id('&${typ}'))
	source := t.make_prefix(.mul, typed_pointer)
	t.set_node_typ(int(source), typ_id)
	// The helper body inlines the clone directly; for a sum type that means the variant
	// switch itself (make_compiler_default_clone_value would otherwise route straight back
	// to this helper and never emit a body). Nested owned payloads still recurse through the
	// helper, keeping every use site compact.
	cloned := if t.is_sum_type_name(typ_id) {
		t.make_compiler_default_sum_clone_value(source, typ_id)
	} else {
		t.make_compiler_default_clone_value(source, typ_id, false)
	}
	mut body := t.pending_stmts.clone()
	body << t.make_return(cloned, typ_id)
	t.pending_stmts = saved_pending
	t.restore_var_types(saved_vars)
	t.default_clone_expansion_stack = saved_expansion_stack
	t.cur_fn_name = saved_fn_name
	t.set_cur_fn_ret_type(saved_ret_type_source_id)
	helper_module := if t.cur_module.len > 0 { t.cur_module } else { 'main' }
	t.add_generated_fn_decl_context(helper_module)
	start := t.a.children.len
	t.a.children << param
	t.a.children << body
	fn_decl := t.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          helper
		typ:            typ_id
		children_start: i32(start)
		children_count: flat.child_count(1 + body.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, helper_module, t.cur_file)
	t.set_fn_ret_type(helper, typ_id)
	t.mark_fn_used_name(helper)
	if !isnil(t.tc) {
		// Detach the signature tables first: a parallel transform worker still shares
		// the master's maps here, so an in-place write would race with the other
		// workers and publish a key owned by this worker's disposable arena.
		t.tc.ensure_private_transform_signatures()
		t.tc.fn_ret_types[helper] = t.tc.parse_type(typ_id)
		t.tc.register_generated_fn_param_types(helper, [*t.tc.parse_type(flat.source_type_voidptr)])
		t.tc.fn_variadic[helper] = false
		t.tc_signature_names_log << helper
	}
}

// make_compiler_default_array_clone_value clones the array storage and then replaces
// each owning element with an independent clone. The initial element copies are not
// owners and are deliberately overwritten without being dropped. The caller classifies
// the source lifetime before transformation can make a temporary addressable.
fn (mut t Transformer) make_compiler_default_array_clone_value(source flat.NodeId, array_type_id u32, source_is_owned_temporary bool) flat.NodeId {
	array_type := flat.source_type_name(array_type_id)
	elem_type := array_type[2..]
	if !t.compiler_default_clone_type_needs_work(flat.source_type_id(elem_type)) {
		return t.make_array_clone_value(source, array_type_id)
	}
	stable_source := t.stable_transformed_expr_for_reuse(source, array_type_id, 'derived_clone_array_source')
	out_name := t.new_temp('derived_clone_array')
	idx_name := t.new_temp('derived_clone_array_idx')
	t.mark_fn_used('array__clone')
	storage_clone := t.make_call_typed('array__clone', [
		t.runtime_addr(stable_source, array_type_id),
	], array_type_id)
	t.pending_stmts << t.make_decl_assign_typed(out_name, storage_clone, array_type_id)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(t.make_ident(out_name), 'len', flat.source_type_int))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.array_get_value(stable_source, t.make_ident(idx_name), flat.source_type_id(elem_type))
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, flat.source_type_id(elem_type), true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign_without_ownership_drop(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), flat.source_type_id(elem_type)), cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], flat.source_type_void))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type_id)
	return result
}

// make_compiler_default_fixed_array_clone_value copies the fixed-array storage and
// replaces each owning element with an independent clone. The initial element copies
// are non-owning and are overwritten without being dropped.
fn (mut t Transformer) make_compiler_default_fixed_array_clone_value(source flat.NodeId, raw_fixed_type_id u32) flat.NodeId {
	fixed_type_source_id_2 := t.receiver_type_text_source_fixed_spelling(t.resolved_fixed_array_canonical_type(raw_fixed_type_id))
	elem_type := fixed_array_elem_type(fixed_type_source_id_2)
	if !t.compiler_default_clone_type_needs_work(elem_type) {
		return source
	}
	source_is_owned_temporary := !t.expr_can_take_address(source)
	stable_source := t.stable_transformed_expr_for_reuse(source, fixed_type_source_id_2, 'derived_clone_fixed_array_source')
	out_name := t.new_temp('derived_clone_fixed_array')
	idx_name := t.new_temp('derived_clone_fixed_array_idx')
	t.pending_stmts << t.make_decl_assign_typed(out_name, stable_source, fixed_type_source_id_2)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_fixed_array_len_expr(fixed_type_source_id_2))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.make_index(stable_source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_assign_without_ownership_drop(t.make_index(t.make_ident(out_name), t.make_ident(idx_name), elem_type), cloned_elem)
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], flat.source_type_void))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), fixed_type_source_id_2)
	return result
}

// make_compiler_default_map_clone_value constructs a fresh map, recursively clones
// owning non-string keys and values, and lets map key callbacks clone string keys.
// The source lifetime is classified by the caller before transformation can turn a
// temporary literal into an addressable synthetic identifier.
fn (mut t Transformer) make_compiler_default_map_clone_value(source flat.NodeId, map_type_id u32, source_is_owned_temporary bool) flat.NodeId {
	key_type_source_id_4, value_type_source_id_2 := t.map_type_parts(map_type_id)

	clean_key_type := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(key_type_source_id_4)))
	key_needs_clone := clean_key_type != 'string'
		&& t.compiler_default_clone_type_needs_work(key_type_source_id_4)
	value_needs_clone := t.compiler_default_clone_type_needs_work(value_type_source_id_2)
	stable_source := t.stable_transformed_expr_for_reuse(source, map_type_id, 'derived_clone_map_source')
	if key_type_source_id_4 == 0 || value_type_source_id_2 == 0 || (!key_needs_clone && !value_needs_clone) {
		t.mark_fn_used('map__clone')
		storage_clone := t.make_call_typed('map__clone', [
			t.runtime_addr(stable_source, map_type_id),
		], map_type_id)
		if !source_is_owned_temporary {
			return storage_clone
		}
		out_name := t.new_temp('derived_clone_map')
		t.pending_stmts << t.make_decl_assign_typed(out_name, storage_clone, map_type_id)
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], flat.source_type_void))
		result := t.make_ident(out_name)
		t.set_node_typ(int(result), map_type_id)
		return result
	}
	out_name := t.new_temp('derived_clone_map')
	key_name := t.new_temp('derived_clone_map_key')
	source_value_name := t.new_temp('derived_clone_map_source_value')
	cloned_key_name := t.new_temp('derived_clone_map_cloned_key')
	value_name := t.new_temp('derived_clone_map_value')
	t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_new_map_call(map_type_id), map_type_id)
	key_storage_type_source_id := t.map_key_storage_type(key_type_source_id_4)

	t.set_var_type(key_name, key_storage_type_source_id)
	t.set_var_type(source_value_name, value_type_source_id_2)
	pending_start := t.pending_stmts.len
	cloned_key := if key_needs_clone {
		t.make_compiler_default_clone_value(t.make_ident(key_name), key_type_source_id_4, true)
	} else {
		t.make_ident(key_name)
	}
	cloned_value := if value_needs_clone {
		t.make_compiler_default_clone_value(t.make_ident(source_value_name), value_type_source_id_2, true)
	} else {
		t.make_ident(source_value_name)
	}
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	map_key_name := if key_needs_clone { cloned_key_name } else { key_name }
	if key_needs_clone {
		body << t.make_decl_assign_typed(cloned_key_name, cloned_key, key_storage_type_source_id)
	}
	body << t.make_decl_assign_typed(value_name, cloned_value, value_type_source_id_2)
	body << t.make_map_set_stmt(t.make_ident(out_name), map_type_id, map_key_name, value_name)
	if clean_key_type == 'string' {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(key_name),
		], flat.source_type_void))
	}
	start := t.a.children.len
	t.a.children << t.make_ident(key_name)
	t.a.children << t.make_ident(source_value_name)
	t.a.children << stable_source
	for stmt in body {
		t.a.children << stmt
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind:           .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		value:          '3'
		flags:          flat.node_flag_skip_ownership_drops
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], flat.source_type_void))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), map_type_id)
	return result
}

fn (t &Transformer) compiler_default_clone_field_type(owner_id u32, field FieldInfo) u32 {
	return t.lookup_struct_field_type(owner_id, field.name) or {
		(if field.typ != 0 {
			field.typ
		} else {
			field.raw_typ
		})
	}
}

// type_is_union reports whether `typ_id` names a `union`. Its members share their
// storage and none of them is known to be the live one, so a copy is bitwise, unless
// the union declares a `clone()` method: cloning a `string` member would overwrite the
// integer or float payload stored there.
fn (t &Transformer) type_is_union(typ_id u32) bool {
	if isnil(t.tc) || typ_id == 0 {
		return false
	}
	return flat.source_type_name(typ_id) in t.tc.unions
}

// union_has_clone_method reports whether a union declares the `clone()` method that
// copies it instead of the bitwise copy.
fn (t &Transformer) union_has_clone_method(typ_id u32) bool {
	return !isnil(t.tc) && t.tc.ownership_type_has_clone_method(t.tc.parse_type(typ_id))
}

fn (t &Transformer) compiler_default_clone_type_needs_work(typ_id u32) bool {
	return t.compiler_default_clone_type_needs_work_seen(typ_id, []u32{})
}

fn (t &Transformer) compiler_default_clone_type_needs_work_seen(typ_id u32, seen_ids []u32) bool {
	clean := t.trimmed_type_id(t.normalize_type_alias(typ_id))
	shape := flat.source_type_shape(clean)
	if clean == 0 || shape.has(.pointer) || clean in seen_ids { return false }
	if !isnil(t.tc) && types.unalias_type(t.tc.parse_type(clean)) is types.Enum { return false }
	if t.type_is_union(clean) { return t.union_has_clone_method(clean) }
	if clean in [transform_source_type_thread, transform_source_type_chan] || shape.has(.thread_type) || shape.has(.channel_type) {
		return false
	}
	if shape.has(.optional) { return true }
	if t.is_fixed_array_type(clean) {
		return t.compiler_default_clone_type_needs_work_seen(fixed_array_elem_type(clean), seen_ids)
	}
	if clean == flat.source_type_string || shape.has(.array_type) || shape.has(.map_type) {
		return true
	}
	name := flat.source_type_name(clean)
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean)
		if t.tc.ownership_type_requires_destruction(parsed) || t.tc.named_type_implements_marker(clean, flat.source_type_id('IClone')) {
			return true
		}
	}
	if name in t.structs || t.sum_types.contains(name) {
		clone_name := '${name}.clone'
		if t.fn_ret_types.contains(clone_name) || (!isnil(t.tc) && clone_name in t.tc.fn_ret_types) {
			return true
		}
	}
	// Ordinary structs can own collection storage even when ownership checking is
	// disabled. Inspect their fields so an array append does not silently copy a
	// pointer-backed map header instead of cloning the stored value.
	if info := t.lookup_struct_info(clean) {
		mut next_seen := seen_ids.clone()
		next_seen << clean
		for field in info.fields {
			if t.compiler_default_clone_type_needs_work_seen(t.compiler_default_clone_field_type(clean, field), next_seen) {
				return true
			}
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
		decoded_value_type_source_id := t.or_expr_receiver_unwrapped_type(base_id) or { u32(0) }
		decoded_value_type_source_id_2 := decoded_value_type_source_id
		decoded_value_type_shape := flat.source_type_shape(decoded_value_type_source_id_2)
		decoded_value_is_concrete := decl_type_is_usable(decoded_value_type_source_id_2)
			&& !t.generic_arg_is_unresolved(decoded_value_type_source_id)
			&& !decoded_value_type_shape.has(.pointer)
		if !decoded_value_is_concrete {
			if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, 'str') {
				return exact_call
			}
		}
		if !decoded_value_is_concrete {
			mut raw_base_types := []u32{}
			// A method called on an arithmetic expression that has a 128-bit operand
			// belongs to the wider type: the checker recorded the expression under
			// the narrower operand's type, so its `.str()` would call the 64-bit
			// printer on a 128-bit value.
			for candidate in [
				t.wide_method_receiver_type(base_id),
				t.raw_var_type_for_expr(base_id) or { u32(0) },
				t.node_type(base_id),
				t.lvalue_type(base_id),
			] {
				clean := t.trim_all_pointer_type(candidate)
				if clean != 0 && clean !in raw_base_types {
					raw_base_types << clean
				}
				if !isnil(t.tc) {
					resolved_source_id_2 := t.tc.resolve_imported_type_text_in_file(clean, t.cur_file)
					if resolved_source_id_2 != 0 && resolved_source_id_2 !in raw_base_types {
						raw_base_types << resolved_source_id_2
					}
				}
			}
			for raw_base_type in raw_base_types {
				if method_name := t.resolve_receiver_method_for_type(raw_base_type, 'str') {
					if t.receiver_method_matches_type_name(method_name, raw_base_type) {
						args := t.transform_receiver_method_args(node, base_id, method_name)
						ret_type_source_id := t.receiver_method_return_type(method_name, node.typ)

						t.mark_fn_used_name(method_name)
						return t.make_call_typed(method_name, args, ret_type_source_id)
					}
				}
				method_name := '${flat.source_type_name(raw_base_type)}.str'
				if t.is_known_fn_name(method_name)
					&& t.receiver_method_matches_type_name(method_name, raw_base_type) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type_source_id_2 := t.receiver_method_return_type(method_name, node.typ)

					t.mark_fn_used_name(method_name)
					return t.make_call_typed(method_name, args, ret_type_source_id_2)
				}
			}
		}
	}
	if alias_call := t.try_lower_checker_selected_alias_method(call_id, node, base_id) {
		return alias_call
	}
	array_builtin_method := t.array_builtin_method_name(fn_node.value) or { '' }
	if fn_node.value !in ['clone', 'reverse', 'contains', 'index', 'last_index', 'join', 'any',
		'all', 'count', 'equals', 'prepend', 'insert', 'push_many', 'str', 'to_fixed_size', 'wait'] {
		if fn_node.value !in ['filter', 'map', 'sort', 'sorted', 'sort_with_compare',
			'sorted_with_compare'] && array_builtin_method.len == 0 {
			mut early_base_type := t.node_type(base_id)
			if early_base_type == 0 {
				early_base_type = t.lvalue_type(base_id)
			}
			early_clean_type_source_id := t.normalize_type_alias((if flat.source_type_shape(early_base_type).has(.pointer) {
				flat.source_type_id(flat.source_type_name(early_base_type)[1..])
			} else {
				early_base_type
			}))
			early_clean_type_shape := flat.source_type_shape(early_clean_type_source_id)
			if early_clean_type_shape.has(.array_type) || t.is_fixed_array_type(early_clean_type_source_id) {
				if smartcast_method := t.resolve_smartcast_target_receiver_method(base_id, fn_node.value) {
					if !t.receiver_method_name_is_open_generic(smartcast_method) {
						args := t.transform_receiver_method_args(node, base_id, smartcast_method)
						ret_type_source_id_3 := t.receiver_method_return_type(smartcast_method, node.typ)

						t.mark_fn_used_name(smartcast_method)
						return t.make_call_typed(smartcast_method, args, ret_type_source_id_3)
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
	raw_base_type_source_id := t.raw_checker_node_type(base_id)
	raw_base_type := flat.source_type_name(raw_base_type_source_id)
	if raw_base_type_source_id != 0 {
		if _ := t.array_alias_stringify_type(raw_base_type_source_id) {
			base_type = raw_base_type_source_id
		}
	}
	mut smartcast_container := false
	if sc := t.find_smartcast_for_expr(base_id) {
		variant_type_source_id := t.resolve_variant(sc.sum_type_name, sc.variant_name)
		variant_type_source_id_2 := variant_type_source_id
		variant_type_shape := flat.source_type_shape(variant_type_source_id_2)
		if variant_type_shape.has(.array_type) || t.is_fixed_array_type(variant_type_source_id) {
			base_type = variant_type_source_id_2
			smartcast_container = true
		}
	}
	base_node := t.a.nodes[int(base_id)]
	if unwrapped_source_id := t.or_expr_receiver_unwrapped_type(base_id) {
		unwrapped_source_id_2 := unwrapped_source_id
		unwrapped_shape := flat.source_type_shape(unwrapped_source_id_2)

		if unwrapped_shape.has(.array_type) || t.is_fixed_array_type(unwrapped_source_id) {
			base_type = unwrapped_source_id_2
		}
	}
	if base_node.kind == .call {
		concrete_base_type_source_id := t.concrete_generic_call_return_type(base_id, base_node)
		concrete_base_type_shape := flat.source_type_shape(concrete_base_type_source_id)
		if concrete_base_type_shape.has(.array_type) {
			base_type = concrete_base_type_source_id
		}
	}
	if (!flat.source_type_shape(base_type).has(.array_type) && !t.is_fixed_array_type(base_type)) || flat.source_type_name(base_type) == 'array' {
		lvalue_base_type_source_id := t.lvalue_type(base_id)
		lvalue_base_type_source_id_2 := lvalue_base_type_source_id
		lvalue_base_type_shape := flat.source_type_shape(lvalue_base_type_source_id_2)
		if lvalue_base_type_shape.has(.array_type) || t.is_fixed_array_type(lvalue_base_type_source_id) {
			base_type = lvalue_base_type_source_id_2
		}
	}
	base_type = t.normalize_type_alias(base_type)
	base_type = transform_unshared_receiver_type(base_type)
	if !flat.source_type_shape(base_type).has(.array_type) && !t.is_fixed_array_type(base_type) {
		if base_node.kind in [.ident, .call, .selector, .as_expr] {
			new_base := t.transform_expr(base_id)
			new_base_type_source_id := t.node_type(new_base)
			new_base_type_shape := flat.source_type_shape(new_base_type_source_id)
			if new_base_type_shape.has(.array_type) || t.is_fixed_array_type(new_base_type_source_id) {
				selector := t.make_selector(new_base, fn_node.value, u32(0))
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
					kind:           .call
					children_start: start
					children_count: node.children_count
					pos:            node.pos
					typ:            node.typ
				}
				return t.try_lower_array_method_call(call_id, new_node)
			}
		}
	}
	if fn_node.value == 'str' && base_node.kind == .call {
		new_base := t.transform_expr(base_id)
		mut new_base_type := t.node_type(new_base)
		if alias_type_source_id := t.array_alias_stringify_type(new_base_type) {
			return t.wrap_string_conversion(new_base, alias_type_source_id)
		}
		if raw_alias_type_source_id := t.array_alias_stringify_type(raw_base_type_source_id) {
			return t.wrap_string_conversion(new_base, raw_alias_type_source_id)
		}
		if recorded_elem_type_source_id := t.recorded_array_call_elem_type(base_id) {
			recorded_elem_type := flat.source_type_name(recorded_elem_type_source_id)

			new_base_type = flat.source_type_id('[]${recorded_elem_type}')
			t.set_node_typ(int(new_base), new_base_type)
		}
		if flat.source_type_shape(new_base_type).has(.array_type) {
			if stringify_type_has_generic_placeholder(flat.source_type_name(new_base_type)) {
				return none
			}
			return t.lower_array_str(new_base, new_base_type)
		}
		if flat.source_type_shape(new_base_type).has(.map_type) {
			if stringify_type_has_generic_placeholder(flat.source_type_name(new_base_type)) {
				return none
			}
			return t.lower_map_str(new_base, new_base_type)
		}
	}
	clean_base_type := (if flat.source_type_shape(base_type).has(.pointer) {
		flat.source_type_id(flat.source_type_name(base_type)[1..])
	} else {
		base_type
	})
	if fn_node.value == 'str' && t.is_builder_receiver(base_id, base_type) {
		return none
	}
	if fn_node.value == 'str' && flat.source_type_shape(base_type).has(.pointer)
		&& (flat.source_type_shape(clean_base_type).has(.array_type) || t.is_fixed_array_type(clean_base_type)) {
		return t.lower_ref_collection_str(t.transform_expr(base_id), clean_base_type)
	}
	if t.is_fixed_array_type(clean_base_type) && fn_node.value == 'pointers'
		&& array_builtin_method.len > 0 {
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 && method_name != array_builtin_method
			&& t.call_resolved_to_method(call_id, method_name)
			&& !t.receiver_method_name_is_open_generic(method_name) {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type_source_id_4 := t.receiver_method_return_type(method_name, node.typ)

			t.mark_fn_used(method_name)
			return t.make_call_typed(method_name, args, ret_type_source_id_4)
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
		ret_type_source_id_5 := t.receiver_method_return_type(array_builtin_method, node.typ)

		// Use an internal target so call type propagation does not adapt the fixed
		// receiver to the builtin method's dynamic `array` parameter.
		return t.make_call_typed('__v3_fixed_array_pointers', args, ret_type_source_id_5)
	}
	if t.is_fixed_array_type(clean_base_type) {
		if fn_node.value == 'str' {
			receiver := if smartcast_container {
				t.make_plain_expr_for_smartcast(base_id)
			} else {
				t.transform_expr(base_id)
			}
			raw_alias_type_source_id_5 := t.raw_alias_type_for_expr(base_id)
			raw_alias_type := flat.source_type_name(raw_alias_type_source_id_5)
			stringify_type := if raw_alias_type_source_id_5 != 0 {
				raw_alias_type
			} else {
				flat.source_type_name(clean_base_type)
			}
			return t.wrap_string_conversion(receiver, flat.source_type_id(stringify_type))
		}
		// A fixed-array alias may declare a method with the same name as an array
		// builtin. Honor the checker-selected alias method before adapting the
		// fixed array to a dynamic array for builtin lowering.
		if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, array_builtin_method) {
			return exact_call
		}
		if fn_node.value == 'map' {
			return t.lower_array_map_call(node, fn_node, clean_base_type)
		}
		elem_type := fixed_array_elem_type(clean_base_type)
		array_type := flat.source_type_id('[]${flat.source_type_name(elem_type)}')
		tmp_name := t.new_temp('fixed_arr')
		array_value := if fn_node.value in ['reverse_in_place', 'sort', 'sort_with_compare'] {
			t.fixed_array_value_to_array_no_alloc(base_id, clean_base_type, array_type)
		} else {
			t.fixed_array_value_to_array(base_id, clean_base_type, array_type)
		}
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, array_value, array_type)
		selector := t.make_selector(t.make_ident(tmp_name), fn_node.value, u32(0))
		if fn_node.value == 'wait' {
			wait_type := fixed_thread_array_wait_return_type(elem_type)
			if wait_type != 0 {
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
			kind:           .call
			children_start: start
			children_count: node.children_count
			pos:            node.pos
			typ:            node.typ
		}
		return t.try_lower_array_method_call(call_id, new_node)
	}
	if !flat.source_type_shape(clean_base_type).has(.array_type) {
		return none
	}
	if fn_node.value in ['reverse_in_place', 'sort', 'sort_with_compare'] {
		if view := t.fixed_array_range_view(base_id, clean_base_type) {
			tmp_name := t.new_temp('fixed_arr')
			t.pending_stmts << t.make_decl_assign_typed(tmp_name, view, clean_base_type)
			mut children := [t.make_selector(t.make_ident(tmp_name), fn_node.value, u32(0))]
			for i in 1 .. node.children_count {
				children << t.a.child(&node, i)
			}
			start := t.a.children.len
			t.a.children << children
			return t.try_lower_array_method_call(call_id, flat.Node{
				kind:           .call
				children_start: start
				children_count: node.children_count
				pos:            node.pos
				typ:            node.typ
			})
		}
	}
	if !(smartcast_container && fn_node.value == 'str') {
		if exact_call := t.lower_checker_selected_receiver_method(call_id, node, base_id, array_builtin_method) {
			return exact_call
		}
	}
	if fn_node.value == 'str' && stringify_type_has_generic_placeholder(flat.source_type_name(clean_base_type)) {
		return none
	}
	elem_type := flat.source_type_name(clean_base_type)[2..]
	if fn_node.value == 'prepend' {
		return t.lower_array_prepend_call(node, fn_node, base_type, flat.source_type_id(elem_type))
	}
	if fn_node.value == 'insert' {
		return t.lower_array_insert_call(node, fn_node, base_type, flat.source_type_id(elem_type))
	}
	if fn_node.value == 'push_many' {
		return t.lower_array_push_many_call(node, fn_node, base_type, flat.source_type_id(elem_type))
	}
	if fn_node.value == 'contains' {
		method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
		if method_name.len > 0 && (t.call_resolved_to_method(call_id, method_name)
			|| transform_is_exact_array_receiver_method(method_name))
			&& !t.receiver_method_name_is_open_generic(method_name) {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type_source_id_6 := t.receiver_method_return_type(method_name, node.typ)

			t.mark_fn_used(method_name)
			return t.make_call_typed(method_name, args, ret_type_source_id_6)
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
			raw_alias_type_source_id_6 := t.raw_alias_type_for_expr(base_id)
			raw_alias_type := flat.source_type_name(raw_alias_type_source_id_6)
			stringify_type := if raw_alias_type_source_id_6 != 0 {
				raw_alias_type
			} else {
				flat.source_type_name(clean_base_type)
			}
			return t.wrap_string_conversion(receiver, flat.source_type_id(stringify_type))
		}
		'to_fixed_size' {
			if base_node.kind != .array_literal {
				return none
			}
			fixed_type := (if t.is_fixed_array_type(node.typ) {
				node.typ
			} else {
				flat.source_type_id('[${base_node.children_count}]${elem_type}')
			})
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
				return t.make_call_typed(method_name, args, flat.source_type_bool)
			}
			receiver := t.transform_expr(base_id)
			arg := t.transform_expr(t.a.children[node.children_start + 1])
			if t.array_elem_needs_element_eq(flat.source_type_id(elem_type)) {
				return t.make_array_elementwise_eq_call(receiver, arg, flat.source_type_id(elem_type), clean_base_type, clean_base_type, node)
			}
			if elem_type.starts_with('[]') {
				return t.make_call_typed('__v3_array_eq_array', [receiver, arg,
					t.make_int_literal(array_nested_eq_depth(flat.source_type_name(clean_base_type)))], flat.source_type_bool)
			}
			if elem_type == 'string' {
				return t.make_call_typed('__v3_array_eq_string', [receiver, arg], flat.source_type_bool)
			}
			return t.make_call_typed('__v3_array_eq_raw', [receiver, arg,
				t.make_sizeof_type(flat.source_type_id(elem_type))], flat.source_type_bool)
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
				ret_type_source_id_7 := t.receiver_method_return_type(method_name, node.typ)

				t.mark_fn_used(method_name)
				return t.make_call_typed(method_name, args, ret_type_source_id_7)
			}
			return t.make_array_clone_call(base_id, base_type)
		}
		'reverse' {
			method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type)
			if method_name.len > 0 && method_name != array_builtin_method
				&& t.call_resolved_to_method(call_id, method_name)
				&& !t.receiver_method_name_is_open_generic(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type_source_id_8 := t.receiver_method_return_type(method_name, node.typ)

				t.mark_fn_used(method_name)
				return t.make_call_typed(method_name, args, ret_type_source_id_8)
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
				'__v3_array_contains_string'
			} else {
				'__v3_array_contains_int'
			}
			return t.make_call_typed(fn_name, [receiver, arg], flat.source_type_bool)
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
			fn_name := if elem_type == 'string' {
				'__v3_array_index_string'
			} else {
				'__v3_array_index_int'
			}
			return t.make_call_typed(fn_name, [receiver, arg], flat.source_type_int)
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
			return t.make_call_typed('Array_string__join', [receiver, arg], flat.source_type_string)
		}
		else {
			if array_method_stays_in_cgen(fn_node.value) {
				method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type)
				if method_name.len > 0 && method_name != array_builtin_method
					&& t.call_resolved_to_method(call_id, method_name)
					&& !t.receiver_method_name_is_open_generic(method_name) {
					args := t.transform_receiver_method_args(node, base_id, method_name)
					ret_type_source_id_9 := t.receiver_method_return_type(method_name, node.typ)

					t.mark_fn_used(method_name)
					return t.make_call_typed(method_name, args, ret_type_source_id_9)
				}
				if t.validating_generic_spec
					&& !t.validate_cgen_array_method_args(node, base_id, clean_base_type, fn_node.value) {
					return t.make_empty()
				}
				if lowered := t.lower_owned_array_accessor_call(base_id, base_type, flat.source_type_id(elem_type), fn_node.value) {
					return lowered
				}
				if lowered := t.lower_owned_array_detach_call(node, base_id, base_type, flat.source_type_id(elem_type), fn_node.value) {
					return lowered
				}
				if lowered := t.lower_owned_array_removal_call(node, base_id, base_type, flat.source_type_id(elem_type), fn_node.value) {
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
					ret_type_source_id_10 := t.receiver_method_return_type(method_name, node.typ)

					t.mark_fn_used(method_name)
					return t.make_call_typed(method_name, args, ret_type_source_id_10)
				}
				if lowered := t.lower_owned_array_accessor_call(base_id, base_type, flat.source_type_id(elem_type), fn_node.value) {
					return lowered
				}
				if lowered := t.lower_owned_array_detach_call(node, base_id, base_type, flat.source_type_id(elem_type), fn_node.value) {
					return lowered
				}
				if lowered := t.lower_owned_array_removal_call(node, base_id, base_type, flat.source_type_id(elem_type), fn_node.value) {
					return lowered
				}
				args := t.transform_receiver_method_args(node, base_id, array_builtin_method)
				ret_type_source_id_11 := t.receiver_method_return_type(array_builtin_method, node.typ)

				return t.make_call_typed(array_builtin_method, args, ret_type_source_id_11)
			}
			return none
		}
	}
}

fn (mut t Transformer) lower_owned_array_detach_call(node flat.Node, base_id flat.NodeId, base_type_id u32, elem_type_id u32, method string) ?flat.NodeId {
	base_type := flat.source_type_name(base_type_id)

	if method !in ['ensure_cap', 'grow_cap', 'grow_len', 'pop', 'pop_left'] || !t.owned_array_slice_detach_needed(elem_type_id) {
		return none
	}
	base := t.stabilize_transformed_lvalue_for_reuse(t.transform_lvalue(base_id))
	array_value := t.array_lvalue_value(base, base_type_id)
	mut args := []flat.NodeId{}
	changes := if method in ['ensure_cap', 'grow_cap', 'grow_len'] {
		if node.children_count < 2 {
			return none
		}
		capacity := t.snapshot_expr_for_reuse(t.a.child(&node, 1))
		args << capacity
		current_cap := t.make_cast(flat.source_type_i64, t.make_selector(array_value, 'cap', flat.source_type_int), flat.source_type_i64)
		required := if method in ['grow_cap', 'grow_len'] {
			field := if method == 'grow_cap' { 'cap' } else { 'len' }
			t.make_infix(.plus, t.make_cast(flat.source_type_i64, capacity, flat.source_type_i64), t.make_cast(flat.source_type_i64, t.make_selector(array_value, field, flat.source_type_int), flat.source_type_i64))
		} else {
			t.make_cast(flat.source_type_i64, capacity, flat.source_type_i64)
		}
		flags := t.make_selector(array_value, 'flags', flat.source_type_id('ArrayFlags'))
		nogrow := t.make_selector(t.make_ident('ArrayFlags'), 'nogrow', flat.source_type_id('ArrayFlags'))
		can_grow := t.make_infix(.eq, t.make_infix(.amp, flags, nogrow), t.make_int_literal(0))
		valid_size := if method in ['grow_cap', 'grow_len'] {
			field := if method == 'grow_cap' { 'cap' } else { 'len' }
			current := t.make_cast(flat.source_type_i64, t.make_selector(array_value, field, flat.source_type_int), flat.source_type_i64)
			t.make_infix(.le, t.make_cast(flat.source_type_i64, capacity, flat.source_type_i64), t.make_infix(.minus, t.make_int_literal(max_int), current))
		} else {
			t.make_infix(.le, required, t.make_int_literal(max_int))
		}
		t.make_infix(.logical_and, t.make_infix(.logical_and, valid_size, can_grow), t.make_infix(.gt, required, current_cap))
	} else {
		t.make_infix(.gt, t.make_selector(array_value, 'len', flat.source_type_int), t.make_int_literal(0))
	}
	t.detach_owned_array_slice_for_mutation(array_value, flat.source_type_id(base_type.trim_left('&')), changes)
	t.mark_fn_used('array__${method}')
	if method in ['ensure_cap', 'grow_cap', 'grow_len'] {
		return t.make_call_typed('array__${method}', [
			t.runtime_addr(base, base_type_id),
			args[0],
		], flat.source_type_void)
	}
	call := t.make_method_call(base, method, args)
	t.set_node_typ(int(call), node.typ)
	return call
}

// Promoted storage and synthetic borrowed headers have no checker cleanup snapshot.
// Keep concrete destructors reachable for explicit destruction of their element owners.
fn (mut t Transformer) mark_fixed_array_element_drops(typ_id u32, seen_ids []u32) {
	if isnil(t.tc) || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(typ_id)) { return }
	clean := t.trimmed_type_id(t.normalize_type_alias(typ_id))
	shape := flat.source_type_shape(clean)
	if clean == 0 || shape.has(.pointer) || clean in seen_ids { return }
	mut visited := seen_ids.clone()
	visited << clean
	if shape.has(.optional) {
		t.mark_fixed_array_element_drops(t.optional_base_type(clean), visited)
		if shape.has(.result_type) {
			for concrete_source_id_2 in t.tc.ierror_impl_names() {
				t.mark_fixed_array_element_drops(concrete_source_id_2, visited)
			}
		}
		return
	}
	if shape.has(.array_type) || t.is_fixed_array_type(clean) {
		elem_type := if shape.has(.array_type) {
			t.array_elem_type(clean)
		} else {
			fixed_array_elem_type(clean)
		}
		t.mark_fixed_array_element_drops(elem_type, visited)
		return
	}
	if shape.has(.map_type) {
		key_type, value_type := t.map_type_parts(clean)
		t.mark_fixed_array_element_drops(key_type, visited)
		t.mark_fixed_array_element_drops(value_type, visited)
		return
	}
	method := if t.tc.autofree_mode { 'free' } else { 'drop' }
	if signature := t.tc.concrete_method_signature_key(clean, method) {
		t.mark_fn_used_name(signature)
		return
	}
	if t.is_sum_type_name(clean) {
		resolved := t.resolve_sum_name(clean)
		for variant in t.sum_types.type_ids(flat.source_type_name(resolved)) or { []u32{} } {
			t.mark_fixed_array_element_drops(t.resolve_variant(resolved, variant), visited)
		}
		return
	}
	if info := t.lookup_struct_info(clean) {
		for field in info.fields {
			t.mark_fixed_array_element_drops(t.compiler_default_clone_field_type(clean, field), visited)
		}
	}
}

// lower_owned_array_accessor_call returns an independent clone for a non-removing array
// accessor. The stored element remains owned by the array and will be destroyed with it.
fn (mut t Transformer) lower_owned_array_accessor_call(base_id flat.NodeId, base_type_id u32, elem_type_id u32, method string) ?flat.NodeId {
	base_type := flat.source_type_name(base_type_id)

	if method !in ['first', 'last'] || isnil(t.tc) {
		return none
	}
	elem := t.tc.parse_type(elem_type_id)
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
	clean_base_type := (if base_type.starts_with('&') {
		flat.source_type_id(base_type[1..])
	} else {
		base_type_id
	})
	mut array_value := base
	if base_type.starts_with('&') {
		array_value = t.make_prefix(.mul, base)
		t.set_node_typ(int(array_value), clean_base_type)
	}
	empty := t.make_infix(.eq, t.make_selector(array_value, 'len', flat.source_type_int), t.make_int_literal(0))
	t.pending_stmts << t.make_if(empty, t.make_block([
		t.make_panic_stmt('array.${method}: array is empty'),
	]), t.make_empty())
	index := if method == 'first' {
		t.make_int_literal(0)
	} else {
		t.make_infix(.minus, t.make_selector(array_value, 'len', flat.source_type_int), t.make_int_literal(1))
	}
	stored := t.make_index(array_value, index, elem_type_id)
	cloned := t.make_compiler_default_clone_value(stored, elem_type_id, true)
	if !source_is_owned_temporary {
		return cloned
	}
	result_name := t.new_temp('array_accessor_result')
	t.pending_stmts << t.make_decl_assign_typed(result_name, cloned, elem_type_id)
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [array_value], flat.source_type_void))
	return t.make_ident(result_name)
}

// lower_owned_array_removal_call drops ownership-bearing elements before a raw array
// operation removes them from the range visited by the scope-exit destructor.
fn (mut t Transformer) lower_owned_array_removal_call(node flat.Node, base_id flat.NodeId, base_type_id u32, elem_type_id u32, method string) ?flat.NodeId {
	base_type := flat.source_type_name(base_type_id)

	if method !in ['delete', 'delete_many', 'clear', 'free', 'trim', 'drop', 'delete_last'] || isnil(t.tc) || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type_id)) {
		return none
	}
	// A body with V's value semantics shares the elements it removes. Only `free` keeps
	// its header reset below, without destroying the elements.
	if t.cur_fn_value_semantics && method != 'free' {
		return none
	}
	// V1 autofree treats clear() after a bulk append as an ownership transfer:
	// the destination retains the element storage while clear() only resets the
	// source header. Explicit ownership mode instead destroys removed elements.
	if method == 'clear' && t.tc.autofree_enabled() {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	clean_base_type := (if base_type.starts_with('&') {
		flat.source_type_id(base_type[1..])
	} else {
		base_type_id
	})
	mut array_value := base
	if base_type.starts_with('&') {
		array_value = t.make_prefix(.mul, base)
		t.set_node_typ(int(array_value), clean_base_type)
	}
	mut args := []flat.NodeId{}
	mut drop_stmts := []flat.NodeId{}
	mut valid_drop_range := flat.empty_node
	mut removal_call_guard := flat.empty_node
	match method {
		'delete' {
			if node.children_count < 2 {
				return none
			}
			index := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), flat.source_type_int), flat.source_type_int, 'array_delete_index')
			args << index
			drop_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_index(array_value, index, elem_type_id),
			], flat.source_type_void))
			valid_drop_range = t.make_infix(.logical_and, t.make_infix(.ge, index, t.make_int_literal(0)), t.make_infix(.lt, index, t.make_selector(array_value, 'len', flat.source_type_int)))
		}
		'delete_many' {
			if node.children_count < 3 {
				return none
			}
			index := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), flat.source_type_int), flat.source_type_int, 'array_delete_index')
			size := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 2), flat.source_type_int), flat.source_type_int, 'array_delete_size')
			args << index
			args << size
			t.append_owned_array_drop_range(array_value, elem_type_id, index, t.make_infix(.plus, index, size), mut drop_stmts)
			end := t.make_infix(.plus, t.make_cast(flat.source_type_i64, index, flat.source_type_i64), t.make_cast(flat.source_type_i64, size, flat.source_type_i64))
			valid_bounds := t.make_infix(.logical_and, t.make_infix(.ge, index, t.make_int_literal(0)), t.make_infix(.le, end, t.make_cast(flat.source_type_i64, t.make_selector(array_value, 'len', flat.source_type_int), flat.source_type_i64)))
			valid_drop_range = t.make_infix(.logical_and, valid_bounds,
				t.make_infix(.gt, size, t.make_int_literal(0)))
			// A valid empty deletion keeps borrowing. Invalid bounds still reach the
			// builtin diagnostic, and other counts retain their ordinary runtime path.
			removal_call_guard = t.make_infix(.logical_or,
				t.make_infix(.ne, size, t.make_int_literal(0)), t.make_prefix(.not, valid_bounds))
		}
		'clear', 'free' {
			if !t.cur_fn_value_semantics {
				t.append_owned_array_drop_range(array_value, elem_type_id, t.make_int_literal(0), t.make_selector(array_value, 'len', flat.source_type_int), mut drop_stmts)
			}
		}
		'trim' {
			if node.children_count < 2 {
				return none
			}
			index := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), flat.source_type_int), flat.source_type_int, 'array_trim_index')
			args << index
			t.append_owned_array_drop_range(array_value, elem_type_id, index, t.make_selector(array_value, 'len', flat.source_type_int), mut drop_stmts)
			valid_drop_range = t.make_infix(.logical_and, t.make_infix(.ge, index, t.make_int_literal(0)), t.make_infix(.lt, index, t.make_selector(array_value, 'len', flat.source_type_int)))
		}
		'drop' {
			if node.children_count < 2 {
				return none
			}
			count := t.stable_transformed_expr_for_reuse(t.transform_expr_for_type(t.a.child(&node, 1), flat.source_type_int), flat.source_type_int, 'array_drop_count')
			args << count
			t.append_owned_array_drop_prefix(array_value, elem_type_id, count, mut drop_stmts)
		}
		'delete_last' {
			last := t.make_infix(.minus, t.make_selector(array_value, 'len', flat.source_type_int), t.make_int_literal(1))
			drop_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_index(array_value, last, elem_type_id),
			], flat.source_type_void))
			valid_drop_range = t.make_infix(.gt, t.make_selector(array_value, 'len', flat.source_type_int), t.make_int_literal(0))
		}
		else {
			return none
		}
	}

	if drop_stmts.len > 0 {
		if method in ['delete', 'delete_many', 'trim', 'delete_last'] {
			changes := if int(valid_drop_range) >= 0 {
				valid_drop_range
			} else if args.len > 0 {
				t.make_infix(.logical_and, t.make_infix(.ge, args[0], t.make_int_literal(0)),
					t.make_infix(.lt, args[0], t.make_selector(array_value, 'len', flat.source_type_int)))
			} else {
				t.make_bool_literal(true)
			}
			t.detach_owned_array_slice_for_mutation(array_value, clean_base_type, changes)
		}
		needs_unique_shrink := t.make_method_call(array_value, 'needs_unique_shrink', []flat.NodeId{})
		t.set_node_typ(int(needs_unique_shrink), flat.source_type_bool)
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
			kind:           .if_expr
			children_start: start
			children_count: 2
			flags:          flat.node_flag_skip_ownership_drops
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
	if int(removal_call_guard) >= 0 {
		t.pending_stmts << t.make_if_with_skip_ownership_drops(removal_call_guard,
			t.make_block_skip_scope_drops([t.make_expr_stmt(call)]), t.make_empty())
		return t.make_empty()
	}
	return call
}

fn (mut t Transformer) append_owned_array_drop_prefix(array_value flat.NodeId, elem_type_id u32, count flat.NodeId, mut stmts []flat.NodeId) {
	idx_name := t.new_temp('array_drop_index')
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	below_count := t.make_infix(.lt, t.make_ident(idx_name), count)
	below_len := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(array_value, 'len', flat.source_type_int))
	cond := t.make_infix(.logical_and, below_count, below_len)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem := t.make_index(array_value, t.make_ident(idx_name), elem_type_id)
	drop_stmt := t.make_expr_stmt(t.make_call_typed('drop_owned', [elem], flat.source_type_void))
	stmts << t.make_for_stmt(init, cond, post, [drop_stmt], flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
}

fn (mut t Transformer) append_owned_array_drop_range(array_value flat.NodeId, elem_type_id u32, start flat.NodeId, end flat.NodeId, mut stmts []flat.NodeId) {
	idx_name := t.new_temp('array_drop_index')
	init := t.make_decl_assign_typed(idx_name, start, flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), end)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem := t.make_index(array_value, t.make_ident(idx_name), elem_type_id)
	drop_stmt := t.make_expr_stmt(t.make_call_typed('drop_owned', [elem], flat.source_type_void))
	stmts << t.make_for_stmt(init, cond, post, [drop_stmt], flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
}

// try_lower_ignored_owned_array_pop_stmt destroys an owned array element result when the
// source program ignores it. Pop methods transfer the removed element from owning arrays,
// while slices and arrays with live slice views still alias shared backing storage.
// First/last produce the independent clone supplied by lower_owned_array_accessor_call.
fn (mut t Transformer) try_lower_ignored_owned_array_pop_stmt(call_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.kind != .call || node.children_count == 0 || isnil(t.tc) || t.cur_fn_value_semantics {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value !in ['first', 'last', 'pop', 'pop_left'] || fn_node.children_count == 0 {
		return none
	}
	base_id := t.a.child(fn_node, 0)
	mut base_type := t.node_type(base_id)
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_base_type_source_id := t.normalize_type_alias(t.trim_all_pointer_type(base_type))
	clean_base_type := flat.source_type_name(clean_base_type_source_id)
	if !clean_base_type.starts_with('[]') {
		return none
	}
	elem_type := clean_base_type[2..]
	if !t.tc.ownership_type_requires_destruction(t.tc.parse_type(flat.source_type_id(elem_type))) {
		return none
	}
	array_builtin_method := t.array_builtin_method_name(fn_node.value) or { '' }
	method_name := t.resolve_collection_receiver_method_name(base_id, fn_node.value, clean_base_type_source_id)
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
		if flat.source_type_shape(base_type).has(.pointer) {
			array_value = t.make_prefix(.mul, base)
			t.set_node_typ(int(array_value), clean_base_type_source_id)
		}
		t.detach_owned_array_slice_for_mutation(array_value, clean_base_type_source_id,
			t.make_infix(.gt, t.make_selector(array_value, 'len', flat.source_type_int), t.make_int_literal(0)))
		t.drain_pending(mut result)
		needs_unique_shrink := t.make_method_call(array_value, 'needs_unique_shrink', []flat.NodeId{})
		t.set_node_typ(int(needs_unique_shrink), flat.source_type_bool)
		t.mark_fn_used('array.needs_unique_shrink')
		drop_result_guard_name = t.new_temp('ignored_array_pop_can_drop')
		result << t.make_decl_assign_typed(drop_result_guard_name, t.make_prefix(.not, needs_unique_shrink), flat.source_type_bool)
		popped = t.make_method_call(array_value, fn_node.value, []flat.NodeId{})
		t.set_node_typ(int(popped), flat.source_type_id(elem_type))
		if fn_node.value == 'pop' {
			t.mark_fn_used('array__pop')
		}
	} else {
		popped = t.transform_expr(call_id)
		t.drain_pending(mut result)
	}
	popped_name := t.new_temp('ignored_array_pop')
	result << t.make_decl_assign_typed(popped_name, popped, flat.source_type_id(elem_type))
	drop_result := t.make_expr_stmt(t.make_call_typed('drop_owned', [
		t.make_ident(popped_name),
	], flat.source_type_void))
	if fn_node.value in ['pop', 'pop_left'] {
		drop_block := t.make_block([drop_result])
		start := t.a.children.len
		t.a.children << t.make_ident(drop_result_guard_name)
		t.a.children << drop_block
		result << t.a.add_node(flat.Node{
			kind:           .if_expr
			children_start: start
			children_count: 2
			flags:          flat.node_flag_skip_ownership_drops
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
		4 { method == 'last' || method == 'trim' }
		5 { method == 'first' || method == 'clear' }
		11 { method == 'delete_last' }
		else { false }
	}
}

fn (mut t Transformer) validate_cgen_array_method_args(node flat.Node, base_id flat.NodeId, base_type_id u32, method string) bool {
	base_type := flat.source_type_name(base_type_id)
	base_node := t.a.nodes[int(base_id)]
	base_name := if base_node.kind == .ident && base_node.value.len > 0 {
		base_node.value
	} else {
		base_type
	}
	display_name := '${base_name}.${method}'
	if method == 'bytestr' && base_type != '[]u8' {
		t.record_monomorph_error('unknown function `${display_name}`')
		return false
	}
	if method == 'wait' {
		elem_type := if base_type.starts_with('[]') {
			trimmed_transform_text(base_type[2..])
		} else {
			''
		}
		if elem_type != 'thread' && !elem_type.starts_with('thread ') {
			t.record_monomorph_error('unknown function `${display_name}`')
			return false
		}
	}
	mut expected_types := []u32{}
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
			'trim', 'repeat', 'delete', 'ensure_cap' { [flat.source_type_int] }
			'repeat_to_depth' { [flat.source_type_int, flat.source_type_int] }
			else { []u32{} }
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
		if actual == 0 {
			actual = t.node_type(arg_id)
		}
		if actual == 0 {
			actual = t.reliable_stringify_type(arg_id)
		}
		actual = t.normalize_type_alias(actual)
		if actual != expected {
			t.record_monomorph_error('cannot use `${flat.source_type_name(actual)}` as argument ${i + 1} to `${display_name}`; expected `${flat.source_type_name(expected)}`')
			valid = false
		}
	}
	return valid
}

fn thread_array_wait_return_type(elem_type_id u32) ?u32 {
	name := trimmed_transform_text(flat.source_type_name(elem_type_id))
	if name == 'thread' { return flat.source_type_void }
	if !name.starts_with('thread ') { return none }
	payload := trimmed_transform_text(name[7..])
	if payload == '?' || payload == '!' { return flat.source_type_id('${payload}void') }
	if payload.starts_with('?') || payload.starts_with('!') {
		value_type := trimmed_transform_text(payload[1..])
		if value_type.len == 0 || value_type == 'void' {
			return flat.source_type_id('${payload[0].ascii_str()}void')
		}
		return flat.source_type_id('${payload[0].ascii_str()}[]${value_type}')
	}
	return flat.source_type_id('[]${payload}')
}

fn fixed_thread_array_wait_return_type(elem_type_id u32) u32 {
	return thread_array_wait_return_type(elem_type_id) or { u32(0) }
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
	if base_type == 0 {
		base_type = t.checker_node_type(base_id)
	}
	clean_type_source_id := t.clean_map_type(base_type)
	clean_type_shape_2 := flat.source_type_shape(clean_type_source_id)
	if !clean_type_shape_2.has(.map_type) {
		return none
	}
	builtin_method := 'map.${fn_node.value}'
	method_name := t.resolve_receiver_method_name(base_id, fn_node.value)
	if method_name.len > 0 && method_name != builtin_method
		&& t.call_resolved_to_method(call_id, method_name)
		&& !t.receiver_method_name_is_open_generic(method_name) {
		args := t.transform_receiver_method_args(node, base_id, method_name)
		ret_type_source_id_2 := t.receiver_method_return_type(method_name, node.typ)

		t.mark_fn_used(method_name)
		return t.make_call_typed(method_name, args, ret_type_source_id_2)
	}
	if fn_node.value == 'clone' {
		key_type_source_id_6, value_type_source_id_5 := t.map_type_parts(clean_type_source_id)

		if key_type_source_id_6 == 0 || value_type_source_id_5 == 0 {
			return none
		}
		mut clean_base_type := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(base_type)))
		for clean_base_type.starts_with('shared ') {
			clean_base_type = trimmed_transform_text(clean_base_type[7..])
		}
		source_is_owned_temporary := !clean_base_type.starts_with('&')
			&& !t.expr_can_take_address(base_id)
		mut source := t.transform_expr(base_id)
		if isnil(t.tc) || !t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type_source_id)) {
			t.mark_fn_used('map__clone')
			return t.make_call_typed('map__clone', [t.runtime_addr(source, base_type)], clean_type_source_id)
		}
		if clean_base_type.starts_with('&') {
			source = t.make_prefix(.mul, source)
			t.set_node_typ(int(source), clean_type_source_id)
		}
		// The checker rejects this call. Do not lower it to the unsafe raw clone while
		// processing the invalid program.
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(key_type_source_id_6)) {
			return source
		}
		if _ := t.tc.ownership_default_clone_missing_method(t.tc.parse_type(value_type_source_id_5)) {
			return source
		}
		return t.make_compiler_default_map_clone_value(source, clean_type_source_id, source_is_owned_temporary)
	}
	if fn_node.value == 'delete' {
		if nested_delete := t.try_lower_nested_map_delete_call(node, base_id, clean_type_source_id) {
			return nested_delete
		}
	}
	source_is_owned_temporary := !flat.source_type_shape(base_type).has(.pointer) && !t.expr_can_take_address(base_id)
	base := t.stable_expr_for_reuse(base_id)
	if map_method_needs_runtime_addr_only(fn_node.value) {
		key_type_source_id_7, value_type_source_id_6 := t.map_type_parts(clean_type_source_id)

		if key_type_source_id_7 != 0 && value_type_source_id_6 != 0 {
			t.append_owned_map_entries_drop_before_reset(base, base_type, key_type_source_id_7, value_type_source_id_6, fn_node.value)
		}
		t.mark_fn_used('map__${fn_node.value}')
		call := t.make_call_typed('map__${fn_node.value}', [
			t.runtime_addr(base, base_type),
		], flat.source_type_void)
		if fn_node.value == 'free' && !isnil(t.tc)
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type_source_id)) {
			mut map_value := base
			if flat.source_type_shape(base_type).has(.pointer) {
				map_value = t.make_prefix(.mul, base)
				t.set_node_typ(int(map_value), clean_type_source_id)
			}
			// Keep scope-exit destruction safe after the raw runtime free leaves the
			// original map header unchanged.
			t.pending_stmts << t.make_expr_stmt(call)
			t.pending_stmts << t.make_assign(map_value, t.zero_value_for_type(clean_type_source_id))
			return t.make_empty()
		}
		return call
	}
	if fn_node.value == 'move' {
		t.mark_fn_used('map__move')
		return t.make_call_typed('map__move', [t.runtime_addr(base, base_type)], clean_type_source_id)
	}
	if fn_node.value == 'reserve' {
		if node.children_count < 2 {
			return none
		}
		t.mark_fn_used('map__reserve')
		capacity := t.transform_expr_for_type(t.a.child(&node, 1), flat.source_type_u32)
		return t.make_call_typed('map__reserve', [
			t.runtime_addr(base, base_type),
			capacity,
		], flat.source_type_void)
	}
	if fn_node.value == 'delete' {
		if node.children_count < 2 {
			return none
		}
		key_type_source_id_8, value_type_source_id_7 := t.map_type_parts(clean_type_source_id)

		if key_type_source_id_8 == 0 || value_type_source_id_7 == 0 {
			return none
		}
		t.mark_fn_used('map__delete')
		key_id := t.a.child(&node, 1)
		cleanup_key := !isnil(t.tc) && t.map_key_expr_creates_owned_value(key_id, key_type_source_id_8)
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type_source_id_8))
		key_name := t.new_temp('map_key')
		key_storage_type_source_id := t.map_key_storage_type(key_type_source_id_8)

		t.pending_stmts << t.make_decl_assign_typed(key_name, t.transform_expr_for_type(key_id, key_type_source_id_8), key_storage_type_source_id)
		handled_delete := t.append_owned_map_entry_delete_with_drops(base, base_type, key_name, key_type_source_id_8, value_type_source_id_7)
		if handled_delete {
			if cleanup_key {
				t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
					t.make_ident(key_name),
				], flat.source_type_void))
			}
			return t.make_empty()
		}
		delete_call := t.make_call_typed('map__delete', [
			t.runtime_addr(base, base_type),
			t.make_prefix(.amp, t.make_ident(key_name)),
		], flat.source_type_void)
		if cleanup_key {
			t.pending_stmts << t.make_expr_stmt(delete_call)
			t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
				t.make_ident(key_name),
			], flat.source_type_void))
			return t.make_empty()
		}
		return delete_call
	}
	if fn_node.value !in ['keys', 'values'] {
		return none
	}
	elem_type := (if fn_node.value == 'keys' {
		t.map_key_type(clean_type_source_id)
	} else {
		t.map_value_type(clean_type_source_id)
	})
	if elem_type == 0 {
		return none
	}
	if !isnil(t.tc) && t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type))
		&& t.compiler_default_clone_type_needs_work(elem_type)
		&& (fn_node.value == 'values' || trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(elem_type))) != 'string') {
		return t.make_owned_map_items_value(base, clean_type_source_id, elem_type, fn_node.value == 'keys', source_is_owned_temporary)
	}
	t.mark_fn_used('map__${fn_node.value}')
	items := t.make_call_typed('map__${fn_node.value}', [
		t.runtime_addr(base, base_type),
	], flat.source_type_id('[]${flat.source_type_name(elem_type)}'))
	if !source_is_owned_temporary || isnil(t.tc)
		|| !t.tc.ownership_type_requires_destruction(t.tc.parse_type(clean_type_source_id)) {
		return items
	}
	// Preserve the returned array before destroying a map receiver that was materialized
	// after ownership analysis. Raw keys()/values() still allocate independent array storage,
	// even when their scalar items need no per-entry clone work.
	out_name := t.new_temp('map_items')
	t.pending_stmts << t.make_decl_assign_typed(out_name, items, flat.source_type_id('[]${flat.source_type_name(elem_type)}'))
	t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [base], flat.source_type_void))
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), flat.source_type_id('[]${flat.source_type_name(elem_type)}'))
	return result
}

// make_owned_map_items_value builds an independent keys()/values() array by cloning every
// ownership-bearing item instead of accepting map__values' shallow byte copies.
fn (mut t Transformer) make_owned_map_items_value(source flat.NodeId, map_type_id u32, item_type_id u32, take_keys bool, source_is_owned_temporary bool) flat.NodeId {
	item_type := flat.source_type_name(item_type_id)
	key_type_source_id_9, value_type_source_id_8 := t.map_type_parts(map_type_id)

	stable_source := t.stable_transformed_expr_for_reuse(source, map_type_id, 'map_items_source')
	out_name := t.new_temp('map_items')
	key_name := t.new_temp('map_items_key')
	value_name := t.new_temp('map_items_value')
	item_name := t.new_temp('map_items_item')
	t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_array_new_call(item_type_id, t.make_int_literal(0), t.make_selector(stable_source, 'len', flat.source_type_int)), flat.source_type_id('[]${item_type}'))
	t.set_var_type(key_name, t.map_key_storage_type(key_type_source_id_9))
	t.set_var_type(value_name, value_type_source_id_8)
	source_item := if take_keys { t.make_ident(key_name) } else { t.make_ident(value_name) }
	pending_start := t.pending_stmts.len
	cloned_item := t.make_compiler_default_clone_value(source_item, item_type_id, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	body << t.make_decl_assign_typed(item_name, cloned_item, item_type_id)
	body << t.make_expr_stmt(t.make_call_typed('array_push', [
		t.runtime_addr(t.make_ident(out_name), flat.source_type_id('[]${item_type}')),
		t.make_prefix(.amp, t.make_ident(item_name)),
	], flat.source_type_void))
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
		kind:           .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		value:          '3'
		flags:          flat.node_flag_skip_ownership_drops
	})
	if source_is_owned_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			stable_source,
		], flat.source_type_void))
	}
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), flat.source_type_id('[]${item_type}'))
	return result
}

// append_owned_map_entries_drop_before_reset releases live stored entries before clear/free
// makes them unavailable to the normal scope-exit map destructor.
fn (mut t Transformer) append_owned_map_entries_drop_before_reset(map_expr flat.NodeId, map_type_id u32, key_type_name_id u32, value_type_name_id u32, method string) {
	key_type_name := flat.source_type_name(key_type_name_id)

	if isnil(t.tc) || t.cur_fn_value_semantics {
		return
	}
	clean_key_type := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(key_type_name_id)))
	key_requires_drop := t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type_name_id))
		&& !(method == 'free' && clean_key_type == 'string')
	value_requires_drop :=
		t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type_name_id))
	if !key_requires_drop && !value_requires_drop {
		return
	}
	key_name := t.new_temp('map_reset_key')
	value_name := t.new_temp('map_reset_value')
	t.set_var_type(key_name, t.map_key_storage_type(key_type_name_id))
	t.set_var_type(value_name, value_type_name_id)
	mut body := []flat.NodeId{}
	value_ptr_name := t.new_temp('map_reset_value_ptr')
	if value_requires_drop {
		// Keep the slot address before dropping a key that the lookup still needs.
		body << t.make_decl_assign_typed(value_ptr_name, t.make_map_get_check_expr(map_expr, map_type_id, key_name), flat.source_type_voidptr)
	}
	if key_requires_drop {
		t.mark_fn_used('map__get_key_check')
		key_ptr_name := t.new_temp('map_reset_key_ptr')
		body << t.make_decl_assign_typed(key_ptr_name, t.make_map_get_key_check_expr(map_expr, map_type_id, key_name), flat.source_type_voidptr)
		stored_key_ptr := t.make_cast(flat.source_type_id('&${key_type_name}'), t.make_ident(key_ptr_name), flat.source_type_id('&${key_type_name}'))
		stored_key := t.make_prefix(.mul, stored_key_ptr)
		t.set_node_typ(int(stored_key), key_type_name_id)
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [stored_key], flat.source_type_void))
	}
	if clean_key_type == 'string' {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(key_name),
		], flat.source_type_void))
	}
	if value_requires_drop {
		value_type_name := flat.source_type_name(value_type_name_id)
		stored_value_ptr := t.make_cast(flat.source_type_id('&${value_type_name}'), t.make_ident(value_ptr_name), flat.source_type_id('&${value_type_name}'))
		stored_value := t.make_prefix(.mul, stored_value_ptr)
		t.set_node_typ(int(stored_value), value_type_name_id)
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [stored_value], flat.source_type_void))
	}
	start := t.a.children.len
	t.a.children << t.make_ident(key_name)
	t.a.children << t.make_ident(value_name)
	t.a.children << map_expr
	for stmt in body {
		t.a.children << stmt
	}
	t.pending_stmts << t.a.add_node(flat.Node{
		kind:           .for_in_stmt
		children_start: start
		children_count: flat.child_count(3 + body.len)
		value:          '3'
		flags:          flat.node_flag_skip_ownership_drops
	})
}

// append_owned_map_entry_delete_with_drops snapshots ownership-bearing stored values,
// removes the entry, and only then destroys the snapshots so key mutation cannot prevent
// map__delete from finding the entry.
fn (mut t Transformer) append_owned_map_entry_delete_with_drops(map_expr flat.NodeId, map_type_id u32, key_name string, key_type_name_id u32, value_type_name_id u32) bool {
	key_type_name := flat.source_type_name(key_type_name_id)
	value_type_name := flat.source_type_name(value_type_name_id)
	if isnil(t.tc) || t.cur_fn_value_semantics {
		return false
	}
	clean_key_type := trimmed_transform_text(flat.source_type_name(t.normalize_type_alias(key_type_name_id)))
	key_requires_drop := clean_key_type != 'string'
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(key_type_name_id))
	value_requires_drop :=
		t.tc.ownership_type_requires_destruction(t.tc.parse_type(value_type_name_id))
	if !key_requires_drop && !value_requires_drop {
		return false
	}
	value_ptr_name := t.new_temp('map_delete_value')
	value_ptr := t.make_map_get_check_expr(map_expr, map_type_id, key_name)
	t.pending_stmts << t.make_decl_assign_typed(value_ptr_name, value_ptr, flat.source_type_voidptr)
	mut body := []flat.NodeId{}
	mut saved_key_name := ''
	if key_requires_drop {
		t.mark_fn_used('map__get_key_check')
		key_ptr_name := t.new_temp('map_delete_key')
		body << t.make_decl_assign_typed(key_ptr_name, t.make_map_get_key_check_expr(map_expr, map_type_id, key_name), flat.source_type_voidptr)
		key_ptr := t.make_cast(flat.source_type_id('&${key_type_name}'), t.make_ident(key_ptr_name), flat.source_type_id('&${key_type_name}'))
		stored_key := t.make_prefix(.mul, key_ptr)
		t.set_node_typ(int(stored_key), key_type_name_id)
		saved_key_name = t.new_temp('map_deleted_key')
		body << t.make_decl_assign_typed(saved_key_name, stored_key, key_type_name_id)
	}
	mut saved_value_name := ''
	if value_requires_drop {
		stored_value_ptr := t.make_cast(flat.source_type_id('&${value_type_name}'), t.make_ident(value_ptr_name), flat.source_type_id('&${value_type_name}'))
		stored_value := t.make_prefix(.mul, stored_value_ptr)
		t.set_node_typ(int(stored_value), value_type_name_id)
		saved_value_name = t.new_temp('map_deleted_value')
		body << t.make_decl_assign_typed(saved_value_name, stored_value, value_type_name_id)
	}
	body << t.make_expr_stmt(t.make_call_typed('map__delete', [
		t.runtime_addr(map_expr, map_type_id),
		t.make_prefix(.amp, t.make_ident(key_name)),
	], flat.source_type_void))
	if key_requires_drop {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(saved_key_name),
		], flat.source_type_void))
	}
	if value_requires_drop {
		body << t.make_expr_stmt(t.make_call_typed('drop_owned', [
			t.make_ident(saved_value_name),
		], flat.source_type_void))
	}
	found := t.make_infix(.ne, t.make_ident(value_ptr_name), t.a.add(.nil_literal))
	body_block := t.make_block(body)
	start := t.a.children.len
	t.a.children << found
	t.a.children << body_block
	t.pending_stmts << t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 2
		flags:          flat.node_flag_skip_ownership_drops
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
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if !isnil(t.tc) {
		if resolved_method := t.tc.resolved_call_name(call_id) {
			channel_method := 'chan.${fn_node.value}'
			runtime_method := 'sync__Channel__${fn_node.value}'
			mut clean_resolved_method := trimmed_transform_text(resolved_method)
			for clean_resolved_method.starts_with('&') {
				clean_resolved_method = trimmed_transform_text(clean_resolved_method[1..])
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
	mut clean_type := flat.source_type_name(base_type)
	mut ptr_depth := 0
	for clean_type.starts_with('&') {
		ptr_depth++
		clean_type = trimmed_transform_text(clean_type[1..])
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
		lit := t.make_array_literal_typed(err_values, flat.source_type_id('[]IError'))
		t.transform_array_literal(lit, t.a.nodes[int(lit)])
	} else {
		t.make_array_new_call(flat.source_type_id('IError'), t.make_int_literal(0), t.make_int_literal(0))
	}
	fn_expr := t.make_selector(t.make_ident('C'), 'sync__Channel__close', u32(0))
	mut receiver := t.transform_expr(base_id)
	for _ in 0 .. ptr_depth {
		receiver = t.make_prefix(.mul, receiver)
	}
	if ptr_depth == 0 {
		receiver = t.make_cast(flat.source_type_id('&sync.Channel'), receiver, flat.source_type_id('&sync.Channel'))
	} else {
		t.set_node_typ(int(receiver), flat.source_type_id('&sync.Channel'))
	}
	return t.make_call_expr_typed(fn_expr, [receiver, errs], flat.source_type_void)
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
	base_type_source_id := t.node_type(base_id)

	clean_array_type_shape := flat.source_type_shape(t.membership_container_type(base_type_source_id))
	if clean_array_type_shape.has(.array_type) {
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
	if int(id) < 0 || int(id) >= t.a.nodes.len || name == '' {
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
	source_file := if file := t.a.source_files[node.pos.id] {
		file.name
	} else {
		t.node_file_or(int(_id), t.cur_file)
	}
	source_module := if !isnil(t.tc) {
		t.tc.file_modules[source_file] or { t.node_module_or(int(_id), t.cur_module) }
	} else {
		t.node_module_or(int(_id), t.cur_module)
	}
	// A checked literal can carry its complete function-value type here; the
	// synthesized declaration itself needs only that signature's return type.
	raw_ret_type := (if node.typ != 0 {
		fn_type_return_type_text(node.typ) or { node.typ }
	} else {
		flat.source_type_void
	})
	ret_type_source_id := t.decl_param_type_in_scope(raw_ret_type, source_module, source_file)
	result_may_alias_capture := t.immediate_closure_result_may_alias_capture(ret_type_source_id)
	mut param_types := []types.Type{}
	mut param_type_ids := []u32{}
	mut param_ids := []flat.NodeId{}
	mut param_names := []string{}
	mut capture_names := []string{}
	mut capture_types := map[string]u32{}
	mut context_field_types := map[string]u32{}
	mut capture_by_ref := map[string]bool{}
	mut capture_from_context := map[string]bool{}
	mut capture_from_heap := map[string]bool{}
	mut capture_heap_slots := map[string]bool{}
	mut capture_heap_snapshots := map[string]bool{}
	mut capture_array_param_snapshots := map[string]bool{}
	mut capture_pointer_value_aliases := map[string]bool{}
	mut capture_is_ref_param := map[string]bool{}
	mut body_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .param {
			// A default callback can be lifted while constructing an imported
			// struct. Resolve its signature types using the declaration's imports.
			param_type_source_id := t.decl_param_type_in_scope(child.typ, source_module, source_file)
			param_ids << if param_type_source_id == child.typ {
				child_id
			} else {
				t.a.add_node(flat.Node{
					...child
					typ: param_type_source_id
				})
			}
			// mut x &T keeps its own pointer in typ and marks the extra mutable
			// caller slot with op == .amp; mut x T already carries that slot as
			// the folded &T. The lifted signature and the fn-value cast below both
			// describe the slot, so add the level the text is still missing.
			slot_type := explicit_mut_pointer_param_type_text(child, param_type_source_id)
			param_type_ids << slot_type
			if child.value.len > 0 {
				param_names << child.value
			}
			if !isnil(t.tc) {
				slot := *t.tc.parse_type(slot_type)
				// The lifted declaration is called like a named fn: a `mut a ?T` is `&?&T`.
				param_types << if child.is_mut && child.op != .amp {
					types.mut_param_semantic_type(slot)
				} else {
					slot
				}
			}
		} else if child.kind == .ident {
			// `fn [T] (...)` inside a generic declaration captures the type
			// parameter for specialization, not a runtime value.
			if child.value in t.active_generic_params {
				continue
			}
			if child.value.len > 0 && child.value !in capture_names {
				if t.var_is_ref_param(child.value) {
					capture_is_ref_param[child.value] = true
				}
				mut capture_type := t.raw_var_type(child.value)
				if capture_type == 0 {
					capture_type = t.var_type(child.value)
				}
				if capture_type == 0 {
					capture_type = t.node_type(child_id)
				}
				if capture_type == 0 && !isnil(t.tc) {
					if typ := t.tc.expr_type(child_id) {
						capture_type = t.semantic_type_name(typ)
					}
				}
				if capture_type == 0 || capture_type == flat.source_type_unknown {
					capture_type = flat.source_type_int
				}
				// Mutable parameter captures retain the caller's array header reference.
				if !child.is_mut && t.mut_param_values[child.value]
					&& !t.pointer_value_rvalues[child.value]
					&& flat.source_type_shape(capture_type).has(.pointer)
					&& flat.source_type_shape(t.comptime_normalize_type_alias_chain(capture_type)).has(.pointer_array) {
					capture_type = t.trim_pointer_type(capture_type)
					capture_array_param_snapshots[child.value] = true
				}
				if child.value in t.heaped_amp_locals && flat.source_type_shape(capture_type).has(.pointer) {
					capture_type = t.trim_pointer_type(capture_type)
					if t.is_fixed_array_type(capture_type) {
						capture_heap_slots[child.value] = true
						capture_from_heap[child.value] = true
					} else {
						capture_heap_snapshots[child.value] = true
					}
				} else if t.pointer_value_rvalues[child.value] && !t.mut_param_values[child.value]
					&& flat.source_type_shape(capture_type).has(.pointer) {
					// An enclosing lifted closure exposes its own `[mut x]` capture
					// as a synthetic `&T` alias of its context slot. A nested capture
					// still captures the `T` value, so snapshot it. Fixed arrays keep
					// sharing the captured storage, through the same pointer alias.
					pointee := t.trim_pointer_type(capture_type)
					if t.is_fixed_array_type(pointee) {
						capture_pointer_value_aliases[child.value] = true
					} else {
						capture_type = pointee
						capture_heap_snapshots[child.value] = true
					}
				}
				if t.active_specialization_args.ids.len > 0 {
					specialized_capture_type_source_id := t.subst_type(capture_type, t.active_specialization_args.ids)
					if specialized_capture_type_source_id != 0
						&& !t.generic_arg_is_unresolved(specialized_capture_type_source_id) {
						capture_type = specialized_capture_type_source_id
					}
				}
				normalized_capture_type_source_id := t.normalize_type_alias(capture_type)
				if !flat.source_type_shape(capture_type).has(.shared_type) && !flat.source_type_shape(capture_type).has(.atomic_type)
					&& normalized_capture_type_source_id != 0 && normalized_capture_type_source_id != flat.source_type_unknown {
					capture_type = normalized_capture_type_source_id
				}
				capture_names << child.value
				is_mut_capture := child.is_mut
				is_ref_capture := is_mut_capture && !flat.source_type_shape(capture_type).has(.pointer)
					&& !t.is_fn_pointer_type_name(capture_type)
				capture_types[child.value] = if is_ref_capture {
					flat.source_type_id('&${flat.source_type_name(capture_type)}')
				} else {
					capture_type
				}
				// A mutable value capture belongs to the closure instance. Store the
				// value itself in the heap context and expose a pointer alias only
				// inside the lifted body; storing `&outer_local` would dangle when a
				// closure is returned and would make separate instances interfere.
				// A fixed array is the exception: `[mut values]` shares the storage of
				// the captured array, so writes on either side are seen by the other.
				context_field_types[child.value] = if is_ref_capture
					&& t.is_fixed_array_type(capture_type) {
					flat.source_type_id('&${flat.source_type_name(capture_type)}')
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
			if capture_by_ref[capture_name] or { false } || flat.source_type_shape(capture_type).has(.pointer) {
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
				capture_types[capture_name] = flat.source_type_id('&${flat.source_type_name(capture_type)}')
				capture_from_context[capture_name] = true
			}
		}
	}
	file_module := t.current_source_module()
	generated_module := if file_module.len > 0 { file_module } else { 'main' }
	context_type := flat.source_type_id('${name}_Ctx')
	context_local := '${name}_ctx'
	if capture_names.len > 0 {
		t.add_fn_literal_capture_context(flat.source_type_name(context_type), generated_module, capture_names, context_field_types)
	}
	saved_fn_name := t.cur_fn_name
	saved_ret_type_source_id := t.cur_fn_ret_type

	saved_locals := t.detach_function_local_state()
	t.cur_fn_name = name
	t.set_cur_fn_ret_type(ret_type_source_id)
	t.reset_var_types()
	for param_id in param_ids {
		param := t.a.nodes[int(param_id)]
		if param.value.len > 0 && param.typ != 0 {
			// A `mut a ?T` holds a `?&T`, which the body reads as `?T` (see
			// mut_optional_view_param).
			storage_type_source_id := t.lifted_mut_optional_param_type(param)
			param_type_source_id := t.lifted_mut_optional_param_view_type(param)
			resolved_param_type_source_id := t.comptime_normalize_type_alias_chain(param_type_source_id)
			resolved_param_type_shape := flat.source_type_shape(resolved_param_type_source_id)
			t.set_var_type_with_raw(param.value, resolved_param_type_source_id, param_type_source_id)
			if storage_type_source_id != param_type_source_id
				&& flat.source_type_shape(storage_type_source_id).has(.pointer) {
				storage := flat.source_type_id(flat.source_type_name(storage_type_source_id)[1..])
				view := flat.source_type_id(flat.source_type_name(param_type_source_id)[1..])
				if _ := mut_optional_param_storage_view(storage, view) {
					t.set_var_mut_optional_storage(param.value, storage)
				}
			}
			// An immutable `.amp` parameter was inferred for a pipe lambda and retains
			// the source language's auto-dereferenced value semantics. Adapted function
			// literals preserve their source parameter operator instead.
			if !param.is_mut && param.op != .amp && resolved_param_type_shape.has(.pointer) {
				t.mark_var_as_ref_param(param.value)
			}
			if t.is_fixed_array_type(param.typ) {
				t.fixed_array_param_values[param.value] = true
			}
			if param.is_mut || param.op == .amp || flat.source_type_shape(param.typ).has(.mutable_type) {
				t.mut_param_values[param.value] = true
				t.pointer_value_lvalues[param.value] = true
				if param.op == .amp || mut_param_has_builtin_pointer_value(param) {
					t.pointer_value_rvalues[param.value] = true
				}
			}
		}
	}
	mut lifted_body := []flat.NodeId{cap: capture_names.len + body_ids.len + 1}
	if capture_names.len > 0 {
		current_data := t.make_call_typed('__v3_closure_current_data', []flat.NodeId{}, flat.source_type_voidptr)
		context_ptr := t.make_cast(flat.source_type_id('&${flat.source_type_name(context_type)}'), current_data, flat.source_type_id('&${flat.source_type_name(context_type)}'))
		context_decl := t.make_decl_assign_typed(context_local, context_ptr, flat.source_type_id('&${flat.source_type_name(context_type)}'))
		t.set_var_type(context_local, flat.source_type_id('&${flat.source_type_name(context_type)}'))
		lifted_body << context_decl
	}
	for capture_name in capture_names {
		if capture_name in param_names {
			continue
		}
		capture_type := capture_types[capture_name] or { continue }
		is_ref_capture := capture_by_ref[capture_name] or { false }
		is_context_capture := capture_from_context[capture_name] or { false }
		is_heap_slot := capture_heap_slots[capture_name] && flat.source_type_shape(capture_type).has(.pointer)
		is_pointer_value_alias := capture_pointer_value_aliases[capture_name] or { false }
		t.set_var_type(capture_name, capture_type)
		// Captures rewritten into synthetic `&T` pointer-value locals need pointer-value
		// lvalue/rvalue lowering. A mut capture whose original type is already a pointer
		// (`&S`) stays a genuine `&S` local:
		// dereferencing its rvalue uses would corrupt `&S` -> `S` and break calls that
		// expect the pointer (e.g. `takes_ptr(p)`), and its assignments must not become
		// `*p = ...`.
		if is_heap_slot {
			t.heaped_amp_locals[capture_name] = true
		}
		if is_ref_capture || is_context_capture || is_heap_slot || is_pointer_value_alias {
			t.pointer_value_lvalues[capture_name] = true
			t.pointer_value_rvalues[capture_name] = true
		}
		context_ident := t.make_ident(context_local)
		t.set_node_typ(int(context_ident), flat.source_type_id('&${flat.source_type_name(context_type)}'))
		context_field_type := context_field_types[capture_name] or { capture_type }
		mut capture_decl_rhs := t.make_selector(context_ident, capture_name, context_field_type)
		if (is_ref_capture || is_context_capture) && !flat.source_type_shape(context_field_type).has(.pointer) {
			capture_decl_rhs = t.make_prefix(.amp, capture_decl_rhs)
			t.set_node_typ(int(capture_decl_rhs), capture_type)
		}
		capture_decl := t.make_decl_assign_typed(capture_name, capture_decl_rhs, capture_type)
		if flat.source_type_shape(capture_type).has(.shared_type) {
			t.set_node_value(int(capture_decl), 'shared:alias')
		}
		lifted_body << capture_decl
	}
	synthetic_decl_count := lifted_body.len
	for body_id in body_ids {
		lifted_body << body_id
	}
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()
	t.mark_local_closure_cleanup_decls(lifted_body)
	mut new_body := []flat.NodeId{}
	if capture_is_ref_param.len == 0 {
		new_body = t.transform_stmts(lifted_body)
	} else {
		new_body = t.transform_stmts(lifted_body[..synthetic_decl_count])
		// Transforming the synthetic capture declarations recreates their bindings.
		// Restore reference-parameter identity before transforming the lifted body.
		for capture_name, _ in capture_is_ref_param {
			t.mark_var_as_ref_param(capture_name)
		}
		new_body << t.transform_stmts(lifted_body[synthetic_decl_count..])
	}
	t.pending_stmts = outer_pending
	t.restore_function_local_state(saved_locals)
	t.cur_fn_name = saved_fn_name
	t.set_cur_fn_ret_type(saved_ret_type_source_id)
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
		kind:           .fn_decl
		value:          name
		typ:            ret_type_source_id
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(fn_decl, generated_module, t.cur_file)
	t.set_fn_ret_type(name, ret_type_source_id)
	if !isnil(t.tc) {
		t.tc.ensure_private_transform_signatures()
		ret := t.tc.parse_type(ret_type_source_id)
		t.tc.fn_ret_types[name] = ret
		t.tc.register_generated_fn_param_types(name, param_types.clone())
		t.tc.fn_variadic[name] = false
		t.add_receiver_method_suffix_index(name)
		t.tc_signature_names_log << name
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${name}'
			t.set_fn_ret_type(qname, ret_type_source_id)
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
	// Specialization can lift a literal after markused scanned its source body.
	// Its callees are no longer children of the enclosing generated function.
	for callee in t.generated_fn_body_call_names(fn_decl) {
		t.mark_fn_used_name(callee)
	}
	mut fn_value_param_type_ids := []u32{cap: param_types.len}
	for i, param_type in param_types {
		raw_type := if i < param_type_ids.len { param_type_ids[i] } else { u32(0) }
		slot_type := if flat.source_type_name(raw_type).contains('main.') {
			raw_type
		} else {
			param_type.source_type_id()
		}
		param := t.a.nodes[int(param_ids[i])]
		fn_value_param_type_ids << mutable_fn_value_param_type_text(param, slot_type)
	}
	fn_value_type := fn_literal_value_type_text_from_text(fn_value_param_type_ids, ret_type_source_id)
	mut ident := flat.empty_node
	if file_module.len > 0 && file_module != 'main' && file_module != 'builtin' {
		ident = t.make_ident('${file_module}.${name}')
	} else {
		ident = t.make_ident(name)
	}
	if fn_value_type != 0 {
		t.set_node_typ(int(ident), fn_value_type)
	}
	if capture_names.len == 0 {
		return ident
	}
	mut context_fields := []flat.NodeId{cap: capture_names.len}
	for capture_name in capture_names {
		context_field_type := context_field_types[capture_name] or { continue }
		mut value := t.make_ident(capture_name)
		if !(capture_by_ref[capture_name] or { false }) {
			if view_param := t.mut_optional_view_param(capture_name) {
				// A `mut a ?T` parameter is captured as the `?T` its body reads.
				value = t.mut_optional_view_read(view_param)
			}
		}
		if capture_array_param_snapshots[capture_name] or { false } {
			value = t.make_prefix(.mul, value)
			t.set_node_typ(int(value), context_field_type)
		} else if capture_heap_snapshots[capture_name] or { false } {
			value = t.make_prefix(.mul, value)
			t.set_node_typ(int(value), context_field_type)
		} else if capture_from_heap[capture_name] or { false } {
			t.set_node_typ(int(value), context_field_type)
		} else if capture_by_ref[capture_name] or { false } && flat.source_type_shape(context_field_type).has(.pointer) {
			value = t.make_prefix(.amp, value)
			t.set_node_typ(int(value), context_field_type)
		}
		if flat.source_type_shape(t.comptime_normalize_type_alias_chain(context_field_type)).has(.array_type) {
			value = if capture_array_param_snapshots[capture_name] or { false } {
				t.clone_owned_array_value_for_capture(value, context_field_type)
			} else {
				t.clone_owned_array_view_for_storage(value, context_field_type)
			}
		}
		context_fields << t.make_named_field_init(capture_name, value, context_field_type)
	}
	context_start := t.a.children.len
	for field in context_fields {
		t.a.children << field
	}
	context_init := t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          flat.source_type_name(context_type)
		typ:            context_type
		children_start: context_start
		children_count: flat.child_count(context_fields.len)
	})
	context_ptr := t.make_prefix(.amp, context_init)
	t.set_node_typ(int(context_ptr), flat.source_type_id('&${flat.source_type_name(context_type)}'))
	create := t.make_call_typed('closure.closure_create_with_data', [
		t.make_cast(flat.source_type_voidptr, ident, flat.source_type_voidptr),
		t.make_cast(flat.source_type_voidptr, context_ptr, flat.source_type_voidptr),
		t.make_bool_literal(true),
	], flat.source_type_voidptr)
	t.mark_fn_used_name('closure.closure_create_with_data')
	return t.make_cast(fn_value_type, create, fn_value_type)
}

fn fn_literal_value_type_text(params []types.Type, ret_type_id u32) u32 {
	mut parts := []u32{cap: params.len}
	for param in params { parts << param.source_type_id() }
	return fn_literal_value_type_text_from_text(parts, ret_type_id)
}

fn fn_literal_value_type_text_from_text(params_ids []u32, ret_type_id u32) u32 {
	ret_type := flat.source_type_name(ret_type_id)
	params := flat.TypeTextList{ ids: params_ids }.names()
	ret := trimmed_transform_text(ret_type)
	if ret.len == 0 || ret == 'void' {
		return flat.source_type_id('fn(${params.join(', ')})')
	}
	return flat.source_type_id('fn(${params.join(', ')}) ${ret}')
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
	if name in t.fn_ret_types.ids {
		return true
	}
	if !isnil(t.tc) {
		return t.tc.has_fn_decl_short_name(name) || name in t.tc.fn_ret_types
	}
	for node_id in 0 .. t.a.nodes.len {
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[node_id] }
		if node.kind == .fn_decl && (node.value == name || node.value.all_after_last('.') == name) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) add_fn_literal_capture_context(name string, module_name string, capture_names []string, capture_types map[string]u32) {
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
			kind:  .field_decl
			value: capture_name
			typ:   capture_type
		})
		parsed_type := if isnil(t.tc) {
			types.Type(types.Unknown{
				reason: 'capture context without type checker'
			})
		} else {
			*t.tc.parse_type(capture_type)
		}
		semantic_fields << types.StructField{
			name: capture_name
			typ:  parsed_type
		}
		transform_fields << FieldInfo{
			name:         capture_name
			typ:          capture_type
			raw_typ:      capture_type
			default_expr: flat.empty_node
		}
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	struct_id := t.a.add_node(flat.Node{
		kind:           .struct_decl
		value:          name
		children_start: start
		children_count: flat.child_count(field_ids.len)
	})
	t.ensure_node_context_map_capacity()
	t.mark_node_context(struct_id, module_name, t.cur_file)
	info := StructInfo{
		name:   flat.source_type_id(name)
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
	if module_name != '' && module_name !in ['main', 'builtin'] {
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
			kind:  .file
			value: t.cur_file
		})
		// Establish the real source module before switching to a foreign namespace.
		// Main files commonly omit `module main`, so a generated `.file` followed
		// directly by an imported module marker would let later checker passes bind
		// the source file itself to the imported module.
		source_module := if t.cur_module.len > 0 { t.cur_module } else { 'main' }
		if source_module != module_name {
			t.a.add_node(flat.Node{
				kind:  .module_decl
				value: source_module
			})
		}
	}
	if module_name != '' {
		t.a.add_node(flat.Node{
			kind:  .module_decl
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
	if named_cast := t.try_lower_resolved_named_cast_call(_id, node) {
		return named_cast
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
	if pointer_str_call := t.try_lower_pointer_str_method_call(_id, node) {
		return pointer_str_call
	}
	if array_call := t.try_lower_array_method_call(_id, node) {
		return array_call
	}
	if smartcast_receiver_call := t.try_lower_smartcast_target_receiver_method_call(_id, node) {
		return smartcast_receiver_call
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
	specialized_enum_type := (if t.validating_generic_spec {
		(t.builtin_enum_from_type(_id, node) or { u32(0) })
	} else {
		u32(0)
	})
	if enum_call := t.try_lower_enum_from_string_call(_id, node) {
		return enum_call
	}
	if specialized_enum_type != 0 {
		return t.lower_specialized_enum_from_call(node, specialized_enum_type)
	}
	if node.children_count > 0 {
		string_callee := t.a.child_node(&node, 0)
		if string_callee.kind == .selector && string_callee.children_count > 0 {
			if folded := t.try_fold_literal_string_call(node, string_callee) {
				return folded
			}
		}
	}
	if free_call := t.try_lower_owned_string_free_call(_id, node) {
		return free_call
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
		if value := t.try_lower_minmaxof_call(name, flat.source_type_id(node.value)) {
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
			arg := t.stringify_expr(arg_id, true)
			return t.make_call(name, [arg])
		}
		'panic' {
			if node.children_count == 2 {
				arg_id := t.a.child(&node, 1)
				arg_type := flat.source_type_name(t.node_type(arg_id))
				if arg_type == 'IError' {
					arg := t.transform_expr(arg_id)
					call := t.make_call('panic', [
						t.make_method_call(arg, 'str', []flat.NodeId{}),
					])
					t.a.nodes[int(call)].pos = node.pos
					return call
				}
				call := t.make_call('panic', [t.stringify_expr(arg_id, false)])
				t.a.nodes[int(call)].pos = node.pos
				return call
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

fn (mut t Transformer) lower_specialized_enum_from_call(node flat.Node, enum_type_id u32) flat.NodeId {
	enum_type := flat.source_type_name(enum_type_id)
	arg := t.transform_expr(t.a.child(&node, 1))
	base := t.make_ident(enum_type)
	callee := t.make_selector(base, 'from', u32(0))
	return t.make_call_expr_typed(callee, [arg], flat.source_type_id('!${enum_type}'))
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
				if enum_node.value != param || i >= t.active_specialization_args.ids.len {
					continue
				}
				concrete_source_id := t.active_specialization_args.ids[i]
				concrete := flat.source_type_name(concrete_source_id)
				if _ := t.enum_type_from_name(concrete_source_id) {
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
	actual_source_id := t.normalize_type_alias(t.specialized_expr_type_name(arg_id))
	actual := flat.source_type_name(actual_source_id)
	if actual_source_id == 0 || actual_source_id == flat.source_type_unknown || actual_source_id == flat.source_type_string
		|| t.is_integer_type_name(actual_source_id) {
		return true
	}
	actual_display := typeof_display_type_text(actual)
	enum_node := t.a.nodes[int(enum_id)]
	enum_name := if enum_node.kind == .ident && enum_node.value.len > 0 {
		enum_node.value
	} else {
		flat.source_type_name(t.node_type(enum_id))
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

fn (t &Transformer) builtin_enum_from_type(call_id flat.NodeId, node flat.Node) ?u32 {
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
	if array_method_stays_in_cgen(fn_node.value) {
		array_method := t.array_builtin_method_name(fn_node.value) or { '' }
		if method_name == array_method {
			return none
		}
	}
	if !t.validate_resolved_receiver_method_args(node, base_id, method_name) {
		return t.make_empty()
	}
	args := t.transform_receiver_method_args(node, base_id, method_name)
	ret_type_source_id_3 := t.receiver_method_return_type(method_name, node.typ)

	t.mark_fn_used_name(method_name)
	if fn_node.value == 'str' {
		if sc := t.find_smartcast_for_expr(base_id) {
			if t.is_interface_type_name(sc.sum_type_name) && args.len > 0 {
				target_source_id := t.trim_pointer_type(t.smartcast_target_type(sc))

				if aggregate_source_id := t.stringify_aggregate_type_name(target_source_id) {
					aggregate := flat.source_type_name(aggregate_source_id)

					value_ptr := t.make_prefix(.amp, args[0])
					t.set_node_typ(int(value_ptr), flat.source_type_id('&${aggregate}'))
					interface_expr := t.make_plain_expr_for_smartcast(base_id)
					return t.lower_interface_smartcast_ref_str(value_ptr, interface_expr, aggregate_source_id, method_name)
				}
			}
		}
	}
	return t.make_call_typed(method_name, args, ret_type_source_id_3)
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

fn (mut t Transformer) try_lower_minmaxof_call(name string, raw_type_id u32) ?flat.NodeId {
	raw_type := flat.source_type_name(raw_type_id)
	mut typ := raw_type
	if !isnil(t.tc) {
		typ = flat.source_type_name(t.normalize_type_in_module(raw_type_id, t.cur_module))
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
		return t.make_float_literal_typed(value, flat.source_type_id(typ))
	}
	return t.make_int_literal_typed(value, flat.source_type_id(typ))
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
	// A pointer to an interface keeps its declared pointer type inside a match branch,
	// but the active smartcast selects the concrete receiver method. Resolve that first;
	// otherwise the generic pointer stringifier calls the interface extension method
	// again and boxes the already-narrowed value as an interface.
	if smartcast_call := t.try_lower_smartcast_target_receiver_method_call(call_id, node) {
		return smartcast_call
	}
	if smartcast_str := t.smartcast_sum_str_call(base_id) {
		return smartcast_str
	}
	if unwrapped_type_source_id := t.or_expr_receiver_unwrapped_type(base_id) {
		unwrapped_type_source_id_2 := unwrapped_type_source_id
		unwrapped_type_shape := flat.source_type_shape(unwrapped_type_source_id_2)

		if decl_type_is_usable(unwrapped_type_source_id_2) && !t.generic_arg_is_unresolved(unwrapped_type_source_id)
			&& !unwrapped_type_shape.has(.pointer) {
			return none
		}
	}
	// Mutable for-in bindings and mutable parameters use pointer-backed storage,
	// but an ordinary receiver expression is the auto-dereferenced value. Do not
	// lower their `.str()` call as an explicit pointer stringification.
	if _ := t.pointer_value_expr_type(base_id) {
		return none
	}
	base_type_source_id := t.pointer_str_receiver_type(base_id) or { return none }
	base_type := flat.source_type_name(base_type_source_id)
	raw_alias_type_source_id_2 := t.raw_alias_type_for_expr(base_id)
	raw_alias_shape := flat.source_type_shape(raw_alias_type_source_id_2)
	if raw_alias_type_source_id_2 != 0 && !raw_alias_shape.has(.pointer) {
		if direct_alias_call := t.pointer_alias_direct_str_call(base_id, raw_alias_type_source_id_2) {
			return direct_alias_call
		}
	}
	if raw_alias_shape.has(.pointer) {
		if method_name := t.checker_selected_receiver_method_name(call_id, 'str') {
			args := t.transform_receiver_method_args(node, base_id, method_name)
			ret_type_source_id_4 := t.receiver_method_return_type(method_name, node.typ)

			t.mark_fn_used_name(method_name)
			return t.make_call_typed(method_name, args, ret_type_source_id_4)
		}
		return t.wrap_string_conversion(t.transform_expr(base_id), raw_alias_type_source_id_2)
	}
	clean_type := base_type[1..]
	if aggregate_source_id := t.stringify_aggregate_type_name(flat.source_type_id(clean_type)) {
		if t.checker_selected_custom_receiver_method(call_id, 'str') {
			method_name := t.checker_selected_receiver_method_name(call_id, 'str') or {
				return none
			}
			t.mark_fn_used_name(method_name)
			return t.lower_ref_str_guarded(t.transform_expr(base_id), aggregate_source_id,
				!t.str_method_has_pointer_receiver(method_name), method_name, '&nil')
		}
		// Open generic methods are specialized by value stringification rather
		// than the checker-selected call path. Keep an explicit pointer-receiver
		// str() result unchanged while retaining the existing nil guard.
		if !isnil(t.tc) {
			if info := t.tc.resolve_generic_struct_method(aggregate_source_id, 'str') {
				if t.str_method_has_pointer_receiver(info.name) {
					return t.lower_ref_str_guarded(t.transform_expr(base_id), aggregate_source_id,
						false, '', '&nil')
				}
			}
		}
		return t.lower_ref_str_prefixed(t.transform_expr(base_id), aggregate_source_id)
	}
	if clean_type.starts_with('[]') || t.is_fixed_array_type(flat.source_type_id(clean_type)) {
		return t.lower_ref_collection_str(t.transform_expr(base_id), flat.source_type_id(clean_type))
	}
	return t.wrap_string_conversion(t.transform_expr(base_id), base_type_source_id)
}

fn (mut t Transformer) pointer_alias_direct_str_call(base_id flat.NodeId, alias_name_id u32) ?flat.NodeId {
	if declaring_alias_source_id, str_fn := t.alias_custom_str_method_in_chain(alias_name_id) {
		return t.make_pointer_alias_str_call(base_id, declaring_alias_source_id, str_fn)
	}
	mut current := t.trimmed_type_id(alias_name_id)
	mut seen := map[u32]bool{}
	for current != 0 && !seen[current] {
		seen[current] = true
		_, target_id := t.lookup_str_alias(current) or { return none }
		clean_target := t.trimmed_type_id(target_id)
		if flat.source_type_shape(clean_target).has(.pointer) {
			pointee := t.trimmed_type_id(t.trim_pointer_type(clean_target))
			if declaring_alias_source_id_2, str_fn := t.alias_custom_str_method_in_chain(pointee) {
				return t.make_pointer_alias_str_call(base_id, declaring_alias_source_id_2, str_fn)
			}
			return none
		}
		if !t.is_type_alias_name(clean_target) {
			return none
		}
		current = clean_target
	}
	return none
}

fn (mut t Transformer) alias_custom_str_method_in_chain(alias_name_id u32) ?(u32, string) {
	mut current := if flat.source_type_shape(alias_name_id).has(.padded) {
		t.trimmed_type_id(alias_name_id)
	} else {
		alias_name_id
	}
	mut seen := map[u32]bool{}
	for current != 0 && !seen[current] {
		seen[current] = true
		if str_fn := t.alias_custom_str_method_name(current) {
			return current, str_fn
		}
		_, target := t.lookup_str_alias(current) or { return none }
		clean_target := if flat.source_type_shape(target).has(.padded) {
			t.trimmed_type_id(target)
		} else {
			target
		}
		if flat.source_type_shape(clean_target).has(.pointer) || !t.is_type_alias_name(clean_target) {
			return none
		}
		current = clean_target
	}
	return none
}

fn (mut t Transformer) make_pointer_alias_str_call(base_id flat.NodeId, declaring_alias_id u32, str_fn string) flat.NodeId {
	t.mark_fn_used_name(str_fn)
	mut receiver := t.transform_expr(base_id)
	declaring_base_shape := flat.source_type_shape(t.alias_str_resolved_base_type(declaring_alias_id))
	if !t.str_method_has_pointer_receiver(str_fn) && !declaring_base_shape.has(.pointer) {
		receiver = t.make_prefix(.mul, receiver)
		t.set_node_typ(int(receiver), declaring_alias_id)
	}
	return t.make_generic_str_method_call(str_fn, generic_fn_decl_base_value(str_fn), [receiver])
}

fn (mut t Transformer) or_expr_receiver_unwrapped_type(id flat.NodeId) ?u32 {
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
	if decode_type_source_id := t.json_decode_or_expr_type(expr_id, expr) {
		decode_type_source_id_2 := decode_type_source_id

		return t.optional_base_type(decode_type_source_id_2)
	}
	_, unwrapped_type_source_id_4 := t.or_expr_types(expr_id, node.typ)
	unwrapped_type_source_id_5 := unwrapped_type_source_id_4

	return unwrapped_type_source_id_5
}

fn (t &Transformer) pointer_str_receiver_type(id flat.NodeId) ?u32 {
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
		if child_type == 0 {
			child_type = t.lvalue_type(child_id)
		}
		if child_type == 0 {
			child_type = t.raw_checker_node_type(child_id)
		}
		if child_type != 0 {
			return flat.source_type_id('&${flat.source_type_name(child_type)}')
		}
	}
	mut typ := t.node_type(id)
	if typ == 0 {
		typ = t.raw_checker_node_type(id)
	}
	if flat.source_type_shape(typ).has(.pointer) {
		return typ
	}
	return none
}

// try_lower_copy_call lowers the builtin `copy(mut dst, src)` to the runtime `copy`,
// which moves the elements bit by bit. Fixed size arrays and strings are passed as array
// views of their storage, so the elements are written in place. With ownership, elements
// that need destruction are copied one by one instead, see `lower_owned_copy_call`.
fn (mut t Transformer) try_lower_copy_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 3 {
		return none
	}
	dst_arg_id := t.a.child(&node, 1)
	src_arg_id := t.a.child(&node, 2)
	dst_id := t.copy_mut_arg_value(dst_arg_id)
	dst_type_source_id := t.unaliased_value_type(dst_id)
	dst_type := flat.source_type_name(dst_type_source_id)
	elem_type := (if t.is_fixed_array_type(dst_type_source_id) {
		fixed_array_elem_type(dst_type_source_id)
	} else if dst_type.starts_with('[]') {
		flat.source_type_id(dst_type[2..])
	} else {
		flat.source_type_u8
	})
	array_type := flat.source_type_id('[]${flat.source_type_name(elem_type)}')
	dst := if t.is_range_index_expr(dst_id) || t.is_fixed_array_type(dst_type_source_id) {
		view := if t.is_range_index_expr(dst_id) {
			t.fixed_array_range_view(dst_id, array_type) or { t.transform_expr(dst_id) }
		} else {
			t.fixed_array_value_to_array_no_alloc(dst_id, dst_type_source_id, array_type)
		}
		tmp_name := t.new_temp('copy_dst')
		t.pending_stmts << t.make_decl_assign_typed(tmp_name, view, array_type)
		t.make_prefix(.amp, t.make_ident(tmp_name))
	} else if flat.source_type_shape(t.node_type(dst_id)).has(.pointer) {
		// a `mut` param destination is already a pointer; taking its address again
		// would hand v_copy an Array** and corrupt the caller's frame
		t.transform_expr(dst_id)
	} else {
		t.make_prefix(.amp, t.transform_expr(dst_id))
	}
	src := t.copy_source_array(src_arg_id, array_type)
	if !isnil(t.tc) && t.tc.ownership_type_requires_destruction(t.tc.parse_type(elem_type)) {
		return t.lower_owned_copy_call(dst, src, t.copy_source_is_temporary(src_arg_id),
			elem_type, array_type)
	}
	return t.make_call_typed('copy', [dst, src], flat.source_type_int)
}

// lower_owned_copy_call copies elements that need ownership destruction one by one:
// each source element is cloned, and the destination element it replaces is dropped.
// The loop runs backwards when `dst` starts after `src`, so overlapping arrays are
// copied like with `vmemmove`. A temporary source is dropped after it has been cloned.
fn (mut t Transformer) lower_owned_copy_call(dst_ptr flat.NodeId, src flat.NodeId, src_is_temporary bool, elem_type_id u32, array_type_id u32) flat.NodeId {
	dst_value := t.make_prefix(.mul, dst_ptr)
	t.set_node_typ(int(dst_value), array_type_id)
	dst := t.stable_transformed_expr_for_reuse(dst_value, array_type_id, 'copy_dst')
	len_name := t.new_temp('copy_len')
	t.pending_stmts << t.make_decl_assign_typed(len_name, t.make_selector(src, 'len', flat.source_type_int),
		flat.source_type_int)
	t.pending_stmts << t.make_if_with_skip_ownership_drops(t.make_infix(.lt, t.make_selector(dst,
		'len', flat.source_type_int), t.make_ident(len_name)), t.make_block([
		t.make_assign_without_ownership_drop(t.make_ident(len_name), t.make_selector(dst,
			'len', flat.source_type_int)),
	]), t.make_empty())
	backward := t.make_infix(.gt, t.make_selector(dst, 'data', flat.source_type_voidptr), t.make_selector(src,
		'data', flat.source_type_voidptr))
	t.pending_stmts << t.make_if_with_skip_ownership_drops(backward, t.make_block([
		t.make_owned_copy_loop(dst, src, len_name, elem_type_id, true),
	]), t.make_block([t.make_owned_copy_loop(dst, src, len_name, elem_type_id, false)]))
	if src_is_temporary {
		t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [src], flat.source_type_void))
	}
	return t.make_ident(len_name)
}

fn (mut t Transformer) make_owned_copy_loop(dst flat.NodeId, src flat.NodeId, len_name string, elem_type_id u32, backward bool) flat.NodeId {
	idx_name := t.new_temp('copy_idx')
	init := t.make_decl_assign_typed(idx_name, if backward {
		t.make_infix(.minus, t.make_ident(len_name), t.make_int_literal(1))
	} else {
		t.make_int_literal(0)
	}, flat.source_type_int)
	cond := if backward {
		t.make_infix(.ge, t.make_ident(idx_name), t.make_int_literal(0))
	} else {
		t.make_infix(.lt, t.make_ident(idx_name), t.make_ident(len_name))
	}
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), if backward {
		flat.Op.dec
	} else {
		flat.Op.inc
	}))
	pending_start := t.pending_stmts.len
	cloned := t.make_compiler_default_clone_value(t.make_index(src, t.make_ident(idx_name),
		elem_type_id), elem_type_id, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	value_name := t.new_temp('copy_value')
	body << t.make_decl_assign_typed(value_name, cloned, elem_type_id)
	t.append_owned_lvalue_drop_before_assign(t.make_index(dst, t.make_ident(idx_name), elem_type_id),
		elem_type_id, mut body)
	body << t.make_assign_after_owned_drop(t.make_index(dst, t.make_ident(idx_name), elem_type_id),
		t.make_ident(value_name))
	return t.make_for_stmt(init, cond, post, body, flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
}

// copy_source_is_temporary reports whether a `copy` source is an array literal or a call
// result, which nothing else owns, rather than storage or a slice of it.
fn (t &Transformer) copy_source_is_temporary(id flat.NodeId) bool {
	mut node := t.a.nodes[int(id)]
	for node.kind == .paren && node.children_count > 0 {
		node = t.a.child_node(&node, 0)
	}
	return node.kind in [.array_literal, .call]
}

// unaliased_value_type returns the type of an expression without pointers and aliases.
fn (t &Transformer) unaliased_value_type(id flat.NodeId) u32 {
	return t.trim_all_pointer_type(t.alias_str_resolved_base_type(t.trim_all_pointer_type(t.node_type(id))))
}

// fixed_array_reference_arg_type preserves explicit fixed literals when expected
// array-reference context has replaced their checker expression type.
fn (t &Transformer) fixed_array_reference_arg_type(id flat.NodeId, array_type_id u32) u32 {
	array_type := flat.source_type_name(array_type_id)
	clean_id := t.unwrap_parens(id)
	if int(clean_id) >= 0 && int(clean_id) < t.a.nodes.len {
		node := t.a.nodes[int(clean_id)]
		if node.kind == .prefix && node.op == .amp && node.children_count == 1 {
			return t.fixed_array_reference_arg_type(t.a.child(&node, 0), array_type_id)
		}
		if node.kind == .postfix && node.op == .not && node.children_count == 1 {
			literal := t.a.child_node(&node, 0)
			if literal.kind == .array_literal {
				return flat.source_type_id('[${literal.children_count}]${array_type[2..]}')
			}
		}
	}
	return t.unaliased_value_type(id)
}

// fixed_array_reference_arg_needs_durable_header includes ranges over fixed backing storage.
fn (t &Transformer) fixed_array_reference_arg_needs_durable_header(id flat.NodeId, array_type_id u32) bool {
	arg_id := t.unwrap_parens(id)
	if int(arg_id) < 0 || int(arg_id) >= t.a.nodes.len {
		return false
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .prefix && arg.op == .amp && arg.children_count == 1 {
		return t.fixed_array_reference_arg_needs_durable_header(t.a.child(&arg, 0), array_type_id)
	}
	if t.is_fixed_array_type(t.fixed_array_reference_arg_type(arg_id, array_type_id)) {
		return true
	}
	if t.is_range_index_expr(arg_id) {
		base_id := t.a.child(&t.a.nodes[int(arg_id)], 0)
		return t.is_fixed_array_type(t.unaliased_value_type(base_id))
	}
	return false
}

// call_argument_param_type maps each unpacked variadic argument to the tail element type.
fn call_argument_param_type(params []types.Type, param_idx int, variadic_idx int) ?types.Type {
	if param_idx < 0 {
		return none
	}
	if variadic_idx >= 0 && variadic_idx < params.len && param_idx >= variadic_idx {
		variadic_type := params[variadic_idx]
		if variadic_type is types.Array {
			return *variadic_type.elem_type
		}
		return none
	}
	if param_idx < params.len {
		return params[param_idx]
	}
	return none
}

fn (t &Transformer) call_has_mut_fixed_array_args(node flat.Node, params []types.Type, offset int, variadic_idx int) bool {
	for i in 1 .. node.children_count {
		param_idx := i - 1 + offset
		param_type := call_argument_param_type(params, param_idx, variadic_idx) or { continue }
		payload_type := fixed_array_reference_param_payload(param_type)
		array_ref_type := flat.source_type_name(t.comptime_normalize_type_alias_chain(t.semantic_type_name(payload_type)))
		if !array_ref_type.starts_with('&[]') {
			continue
		}
		arg_id := t.unwrap_parens(t.a.child(&node, i))
		if t.fixed_array_reference_arg_needs_durable_header(arg_id, flat.source_type_id(array_ref_type[1..])) {
			return true
		}
	}
	return false
}

// Snapshot the callee before user-defined element clones run. A method keeps its
// receiver's lvalue identity; a function-valued field snapshots the entire callee.
fn (mut t Transformer) snapshot_fixed_array_call_callee(id flat.NodeId, call flat.Node, callee_id flat.NodeId) flat.NodeId {
	callee := t.a.nodes[int(callee_id)]
	if callee.kind == .selector && callee.children_count > 0 {
		base_id := t.a.child(&callee, 0)
		if t.call_selector_base_is_namespace(base_id, callee.value, call.value)
			|| t.callee_base_is_not_a_runtime_value(base_id) {
			return callee_id
		}
		is_fn_field := t.receiver_selector_is_fn_field(t.normalize_type_alias(t.trim_pointer_type(t.lvalue_type(base_id))), callee.value)
		if !is_fn_field {
			if !t.operand_needs_ordering_snapshot(base_id) {
				return callee_id
			}
			selected := t.direct_receiver_method(id, base_id, callee.value)
			new_base := if t.method_receiver_is_reference(id, base_id, callee.value) {
				if stabilized := t.stabilize_original_lvalue_receiver(base_id) {
					stabilized
				} else {
					t.snapshot_expr_for_reuse(base_id)
				}
			} else {
				t.snapshot_expr_for_reuse(base_id)
			}
			t.keep_selected_receiver_alias(new_base, selected)
			if new_base == base_id {
				return callee_id
			}
			start := t.a.children.len
			t.a.children << new_base
			for i in 1 .. callee.children_count {
				t.a.children << t.a.child(&callee, i)
			}
			result := t.a.add_node(flat.Node{
				...callee
				children_start: start
			})
			t.copy_cloned_resolution(callee_id, result)
			return result
		}
	}
	if t.callee_needs_ordering_snapshot(callee_id) {
		return t.snapshot_expr_for_reuse(callee_id)
	}
	return callee_id
}

fn (mut t Transformer) snapshot_fixed_array_call_args(mut args []flat.NodeId, start int) {
	for i in start .. args.len {
		args[i] = t.snapshot_transformed_expr_for_reuse(args[i], t.node_type(args[i]),
			'fixed_array_call_arg')
	}
}

// fixed_array_mut_arg gives an array-reference parameter a separate borrowed header.
// Mutations write through to the original fixed storage; appending or reassigning
// the header follows the usual array slice copy-on-grow behavior.
fn (mut t Transformer) fixed_array_mut_arg(view flat.NodeId, array_type_id u32, param_type_id u32) flat.NodeId {
	arg_name := t.new_temp('fixed_array_arg')
	t.pending_stmts << t.make_decl_assign_typed(arg_name, view, array_type_id)
	// A callee may legally retain `&values`, so the independent header must
	// outlive this frame as well as the fixed elements it borrows.
	return t.make_call_typed('v3_heap_array', [t.make_ident(arg_name)], param_type_id)
}

// fixed_array_mut_arg_backing borrows the fixed array's original storage. Local
// storage is heap-promoted before any aliases form, and globals already outlive
// the call. Borrowed flags prevent a retained array header from destroying its
// source's elements or freeing storage it does not own.
fn (mut t Transformer) fixed_array_mut_arg_backing(value_id flat.NodeId, fixed_type_id u32, array_type_id u32) flat.NodeId {
	fixed_type := flat.source_type_name(fixed_type_id)
	array_type := flat.source_type_name(array_type_id)
	t.mark_fixed_array_element_drops(flat.source_type_id(array_type[2..]), []u32{})
	mut source_id := t.unwrap_parens(value_id)
	source_node := t.a.nodes[int(source_id)]
	if source_node.kind == .prefix && source_node.op == .amp && source_node.children_count == 1 {
		source_id = t.unwrap_parens(t.a.child(&source_node, 0))
	}
	// A temporary has no declaration to promote and no other alias. Move its
	// element bytes to durable owning storage before exposing an array reference.
	source_is_multi_return_value := multi_return_fixed_array_arg_marker in t.a.nodes[int(source_id)].generic_params()
	overloaded_result_type_source_id := t.comptime_normalize_type_alias_chain(t.overloaded_index_result_type(source_id) or { u32(0) })
	overloaded_result_type := flat.source_type_name(overloaded_result_type_source_id)
	overloaded_fixed_reference := overloaded_result_type.starts_with('&')
		&& t.is_fixed_array_type(flat.source_type_id(overloaded_result_type[1..]))
	map_value_source := t.fixed_array_source_map_value(source_id) or { flat.empty_node }
	source_is_map_value := int(map_value_source) >= 0
	accessor_source := t.fixed_array_source_accessor(source_id, false) or { flat.empty_node }
	source_is_array_accessor := int(accessor_source) >= 0
	accessor_type := (if source_is_array_accessor {
		t.lvalue_type(accessor_source)
	} else {
		u32(0)
	})
	mut borrowed := !source_is_multi_return_value
		&& !source_is_map_value
		&& (!t.expr_is_overloaded_index_result(source_id) || overloaded_fixed_reference)
		&& (t.expr_can_take_address(source_id)
			|| (flat.source_type_shape(t.node_type(source_id)).has(.pointer)
				&& t.is_fixed_array_type(t.unaliased_value_type(source_id))))
	mut value := t.transform_expr(source_id)
	if source_is_map_value && !isnil(t.tc)
		&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(fixed_type_id)) {
		cloned := t.clone_borrowed_projection(source_id, value, fixed_type_id)
		if cloned != value {
			value = cloned
		} else if !t.tc.ownership_index_read_moves_value(map_value_source) {
			// A map read passed by reference still borrows the map's element owners.
			// Acquire them through the shared clone/empty/uncloneable storage policy.
			view := t.fixed_array_data_to_array_no_alloc(value, fixed_type_id, array_type_id)
			return t.clone_owned_array_storage_value(view, array_type_id, true)
		}
	}
	if borrowed && source_is_array_accessor
		&& !flat.source_type_shape(t.comptime_normalize_type_alias_chain(t.node_type(value))).has(.pointer)
		&& t.fixed_array_source_accessor(value, true) == none {
		// Ownership accessors can produce a cloned fixed value in a generated local.
		// That value has no original receiver storage to retain, so move it to owning backing.
		borrowed = false
		if accessor_source != source_id && !isnil(t.tc)
			&& t.tc.ownership_type_requires_destruction(t.tc.parse_type(accessor_type)) {
			if projection, owner := t.stabilize_fixed_array_accessor_projection(value, accessor_type) {
				value = t.stabilize_transformed_lvalue_for_reuse(projection)
				cloned := t.clone_borrowed_projection(source_id, value, fixed_type_id)
				if cloned != value {
					value = cloned
				} else {
					// A partial projection cannot move fields out of an aggregate with custom
					// destruction. Acquire the field before destroying the accessor clone.
					view := t.fixed_array_data_to_array_no_alloc(value, fixed_type_id, array_type_id)
					owned_view := t.clone_owned_array_storage_value(view, array_type_id, true)
					t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [owner], flat.source_type_void))
					return owned_view
				}
				t.pending_stmts << t.make_expr_stmt(t.make_call_typed('drop_owned', [owner], flat.source_type_void))
			}
		}
	}
	mut storage_id := value
	if borrowed {
		if overloaded_fixed_reference {
			// The overload returns an existing pointer, even when expected array-reference
			// context changed the index annotation. Keep that original storage identity.
			t.set_node_typ(int(storage_id), overloaded_result_type_source_id)
		}
		storage_id = t.retain_fixed_array_index_containers(storage_id)
	} else {
		value_name := t.new_temp('fixed_array_source')
		t.pending_stmts << t.make_stack_value_decl_assign_typed(value_name, value, fixed_type_id)
		addr := t.make_prefix(.amp, t.make_ident(value_name))
		storage_id = if t.tc.requires_aligned_allocation(t.tc.parse_canonical_type_cached(fixed_type_id)) {
			t.make_cast(flat.source_type_id('&${fixed_type}'), t.make_memdup_call_for_type(addr, fixed_type_id), flat.source_type_id('&${fixed_type}'))
		} else {
			addr
		}
	}
	owned_managed_buffer := !borrowed && !t.tc.requires_aligned_allocation(t.tc.parse_canonical_type_cached(fixed_type_id))
	view := if owned_managed_buffer {
		t.fixed_array_data_to_array(storage_id, fixed_type_id, array_type_id)
	} else {
		t.fixed_array_data_to_array_no_alloc(storage_id, fixed_type_id, array_type_id)
	}
	view_name := t.new_temp('fixed_array_view')
	t.pending_stmts << t.make_decl_assign_typed(view_name, view, array_type_id)
	if borrowed {
		flags := t.make_selector(t.make_ident(view_name), 'flags', flat.source_type_id('ArrayFlags'))
		slice_flag := t.make_selector(t.make_ident('ArrayFlags'), 'is_slice', flat.source_type_id('ArrayFlags'))
		t.pending_stmts << t.make_assign(flags, slice_flag)
	} else if !owned_managed_buffer {
		// Aligned temporaries have no outside owner. Preserve that allocation's
		// lifetime without changing its alignment or allocator family.
		t.mark_fn_used('array.mark_aligned_fixed_array_buffer')
		t.pending_stmts << t.make_expr_stmt(t.make_method_call(t.make_ident(view_name), 'mark_aligned_fixed_array_buffer', []flat.NodeId{}))
	}
	return t.make_ident(view_name)
}

// Map lookups yield values backed by the map or its stack-local missing-key default.
// Inline projections share that lifetime; a pointer or dynamic buffer has its own storage.
fn (mut t Transformer) fixed_array_source_map_value(value flat.NodeId) ?flat.NodeId {
	id := t.unwrap_parens(value)
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	if info := t.map_index_info(id) {
		if !flat.source_type_shape(t.comptime_normalize_type_alias_chain(info.value_type)).has(.pointer) {
			return id
		}
		return none
	}
	node := t.a.nodes[int(id)]
	if node.children_count == 0 || node.kind !in [.selector, .index, .prefix] {
		return none
	}
	base := t.a.child(&node, 0)
	base_type_shape_2 := flat.source_type_shape(t.comptime_normalize_type_alias_chain(t.lvalue_type(base)))
	if base_type_shape_2.has(.pointer) || base_type_shape_2.has(.array_type) {
		return none
	}
	return t.fixed_array_source_map_value(base)
}

// Only inline projections depend on the accessor's aggregate lifetime. Pointer and
// dynamic-array fields lead to independent storage. Borrowed owned accessors may already
// have become ordinary dynamic indexes during selector lowering.
fn (t &Transformer) fixed_array_source_accessor(value flat.NodeId, include_lowered_index bool) ?flat.NodeId {
	id := t.unwrap_parens(value)
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .call && t.array_accessor_call_can_take_address(node) {
		return id
	}
	if node.children_count == 0 || node.kind !in [.selector, .index, .prefix] {
		return none
	}
	base := t.a.child(&node, 0)
	base_type := flat.source_type_name(t.comptime_normalize_type_alias_chain(t.lvalue_type(base)))
	if include_lowered_index && node.kind == .index && !t.is_range_index_expr(id)
		&& base_type.trim_left('&').starts_with('[]') {
		return id
	}
	if base_type.starts_with('&') || base_type.starts_with('[]') {
		return none
	}
	return t.fixed_array_source_accessor(base, include_lowered_index)
}

// Stabilize the cloned accessor aggregate before its inline field is acquired, then
// retain that same value for cleanup rather than evaluating a clone method twice.
fn (mut t Transformer) stabilize_fixed_array_accessor_projection(value flat.NodeId, owner_type_id u32) ?(flat.NodeId, flat.NodeId) {
	if t.comptime_normalize_type_alias_chain(t.lvalue_type(value)) == t.comptime_normalize_type_alias_chain(owner_type_id) {
		owner := t.stable_transformed_expr_for_reuse(value, owner_type_id, 'fixed_accessor_owner')
		return owner, owner
	}
	node := t.a.nodes[int(value)]
	if node.children_count == 0 || node.kind !in [.selector, .index, .paren, .prefix] {
		return none
	}
	projection, owner := t.stabilize_fixed_array_accessor_projection(t.a.child(&node, 0), owner_type_id) or {
		return none
	}
	mut children := t.a.children_of(&node).clone()
	children[0] = projection
	return t.copy_node_with_children(node, children), owner
}

// Retained fixed elements share their indexed owner buffer. Managed slices
// identify that original buffer through their preserved allocation provenance.
fn (mut t Transformer) retain_fixed_array_index_containers(value flat.NodeId) flat.NodeId {
	node := t.a.nodes[int(value)]
	if node.kind == .call && t.array_accessor_call_can_take_address(node) {
		callee := t.a.child_node(&node, 0)
		mut base := t.retain_fixed_array_index_containers(t.a.child(callee, 0))
		base_type_source_id := t.comptime_normalize_type_alias_chain(t.address_expr_type_name(base))
		base_type := flat.source_type_name(base_type_source_id)
		if !base_type.trim_left('&').starts_with('[]') {
			return value
		}
		elem_type_shape := flat.source_type_shape(t.comptime_normalize_type_alias_chain(flat.source_type_id(base_type.trim_left('&')[2..])))
		if !elem_type_shape.has(.pointer) && !elem_type_shape.has(.array_type)
			&& !elem_type_shape.has(.map_type) {
			base = t.retain_fixed_array_container(base, base_type_source_id)
		}
		mut children := t.a.children_of(&node).clone()
		children[0] = t.copy_node_with_children(*callee, [base])
		return t.copy_node_with_children(node, children)
	}
	if node.children_count == 0 || node.kind !in [.index, .selector, .paren, .prefix] {
		return value
	}
	mut children := t.a.children_of(&node).clone()
	children[0] = t.retain_fixed_array_index_containers(children[0])
	if node.kind == .index && !t.is_range_index_expr(value) {
		base_type_source_id_2 := t.comptime_normalize_type_alias_chain(t.node_type(children[0]))
		base_type := flat.source_type_name(base_type_source_id_2)
		value_type_shape := flat.source_type_shape(t.comptime_normalize_type_alias_chain(t.node_type(value)))
		if base_type.trim_left('&').starts_with('[]') && !value_type_shape.has(.pointer)
			&& !value_type_shape.has(.array_type) && !value_type_shape.has(.map_type) {
			children[0] = t.retain_fixed_array_container(children[0], base_type_source_id_2)
			for i in 1 .. children.len {
				children[i] = t.stabilize_transformed_lvalue_component(children[i], 'fixed_array_container_index')
			}
		}
	}
	return t.copy_node_with_children(node, children)
}

fn (mut t Transformer) retain_fixed_array_container(value flat.NodeId, base_type_id u32) flat.NodeId {
	mut base := t.stabilize_transformed_lvalue_for_reuse(value)
	if !t.expr_is_plain_lvalue(base) {
		base = t.stable_transformed_expr_for_reuse(base, base_type_id, 'fixed_array_container')
	}
	array_value := t.array_lvalue_value(base, base_type_id)
	t.mark_fn_used('array.retain_fixed_array_buffer')
	t.pending_stmts << t.make_expr_stmt(t.make_method_call(array_value, 'retain_fixed_array_buffer', []flat.NodeId{}))
	return base
}

// fixed_array_range_view lowers `fixed[a..b]`, where the result is written to, to a slice of
// a view of the fixed array's storage. Slicing a fixed size array would copy it, so the
// writes would be lost. Parentheses around the range are ignored. It returns none for
// other expressions.
fn (mut t Transformer) fixed_array_range_view(id flat.NodeId, array_type_id u32) ?flat.NodeId {
	return t.fixed_array_range_view_for_arg(id, array_type_id, false)
}

fn (mut t Transformer) fixed_array_range_view_for_arg(id flat.NodeId, array_type_id u32, mut_arg bool) ?flat.NodeId {
	range_id := t.unwrap_parens(id)
	if !t.is_range_index_expr(range_id) {
		return none
	}
	node := t.a.nodes[int(range_id)]
	base_id := t.a.child(&node, 0)
	base_type_source_id_5 := t.unaliased_value_type(base_id)

	if !t.is_fixed_array_type(base_type_source_id_5) {
		return none
	}
	view_name := t.new_temp('fixed_array_view')
	view := if mut_arg {
		t.fixed_array_mut_arg_backing(base_id, base_type_source_id_5, array_type_id)
	} else {
		t.fixed_array_value_to_array_no_alloc(base_id, base_type_source_id_5, array_type_id)
	}
	t.pending_stmts << t.make_decl_assign_typed(view_name, view, array_type_id)
	mut children := [t.make_ident(view_name)]
	for i in 1 .. node.children_count {
		children << t.a.child(&node, i)
	}
	start := t.a.children.len
	t.a.children << children
	slice_id := t.a.add_node(flat.Node{
		...node
		children_start: start
		typ:            array_type_id
	})
	return t.transform_expr(slice_id)
}

// copy_source_array lowers a `copy` source to an array. It is kept in a typed temp, since
// the runtime `copy` declares a `[]u8` source that would otherwise retype array literals.
fn (mut t Transformer) copy_source_array(id flat.NodeId, array_type_id u32) flat.NodeId {
	return t.stable_transformed_expr_for_reuse(t.copy_source_array_value(id, array_type_id),
		array_type_id, 'copy_src')
}

// copy_source_array_value views strings and local fixed size arrays in place, and converts
// other fixed size arrays to a new array.
fn (mut t Transformer) copy_source_array_value(id flat.NodeId, array_type_id u32) flat.NodeId {
	src_type_source_id := t.unaliased_value_type(id)

	if src_type_source_id == flat.source_type_string {
		src := t.stable_transformed_expr_for_reuse(t.transform_expr(id), flat.source_type_string, 'copy_src')
		t.mark_fn_used('new_array_from_c_array_no_alloc')
		return t.make_array_storage_call('new_array_from_c_array_no_alloc', [
			t.make_selector(src, 'len', flat.source_type_int),
			t.make_selector(src, 'len', flat.source_type_int),
			t.make_int_literal(1),
			t.make_selector(src, 'str', flat.source_type_id('&u8')),
		], array_type_id)
	}
	if t.is_fixed_array_type(src_type_source_id) {
		if t.copy_source_is_local_lvalue(id) {
			return t.fixed_array_value_to_array_no_alloc(id, src_type_source_id, array_type_id)
		}
		return t.fixed_array_value_to_array(id, src_type_source_id, array_type_id)
	}
	return t.transform_expr_for_type(id, array_type_id)
}

fn (t &Transformer) copy_source_is_local_lvalue(id flat.NodeId) bool {
	if !t.expr_is_plain_lvalue(id) {
		return false
	}
	mut root := t.a.nodes[int(id)]
	for root.kind == .selector {
		root = t.a.child_node(&root, 0)
	}
	return (t.var_type(root.value)) != 0
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

fn (t &Transformer) is_range_index_expr(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	return index_node_is_range(t.a, t.a.nodes[int(id)])
}

// index_node_is_range reports whether `node` is a range index, like `a[1..]`.
fn index_node_is_range(a &flat.FlatAst, node flat.Node) bool {
	if node.kind != .index {
		return false
	}
	if node.value == 'range' {
		return true
	}
	if node.children_count > 1 {
		index := a.child_node(&node, 1)
		return index.kind == .range
	}
	return false
}

const primitive_cast_type_names = ['bool', 'int', 'i8', 'i16', 'i32', 'i64', 'isize', 'u8', 'u16',
	'u32', 'u64', 'usize', 'f32', 'f64', 'rune', 'char']!

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
	return t.make_cast(flat.source_type_id(fn_node.value), t.transform_expr(arg_id), flat.source_type_id(fn_node.value))
}

fn (mut t Transformer) try_lower_generic_sum_constructor_call(node flat.Node) ?flat.NodeId {
	target_source_id := t.generic_sum_constructor_call_type(node) or { return none }
	target := flat.source_type_name(target_source_id)
	arg := t.transform_expr(t.a.child(&node, 1))
	start := t.a.children.len
	t.a.children << arg
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          target
		children_start: start
		children_count: 1
		typ:            target_source_id
	})
}

fn (mut t Transformer) try_lower_resolved_named_cast_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if isnil(t.tc) || node.children_count != 2 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .ident || (t.var_type(callee.value)) != 0 {
		return none
	}
	target := t.tc.resolved_call_name(id) or { return none }
	if !t.tc.type_aliases.contains(target) && target !in t.tc.structs && target !in t.tc.enum_names {
		return none
	}
	return t.make_cast(flat.source_type_id(target), t.transform_expr(t.a.child(&node, 1)), flat.source_type_id(target))
}

fn (mut t Transformer) try_lower_generic_named_type_cast_call(node flat.Node) ?flat.NodeId {
	if node.children_count != 2 {
		return none
	}
	fn_id := t.a.child(&node, 0)
	fn_node := t.a.node(fn_id)
	// A selector's final component determines the end of its type spelling.
	// Ordinary method calls cannot be generic casts; avoid resolving their
	// receiver and imports just to reject a spelling that does not end in ']'.
	if fn_node.kind == .selector && !fn_node.value.ends_with(']') {
		return none
	}
	if fn_node.kind == .ident && !fn_node.value.ends_with(']')
		&& !flat.source_type_shape(fn_node.typ).has(.map_type)
		&& (isnil(t.tc) || t.tc.file_selective_imports.len == 0) {
		return none
	}
	// A monomorphized method callee such as `Tree[int].size` contains a generic
	// type spelling, but it is a function name rather than a named-type cast.
	if fn_node.kind == .ident && t.generic_callee_is_specialization(fn_node.value) {
		return none
	}
	target_source_id_2 := t.generic_call_type_arg_name(fn_id)
	target := flat.source_type_name(target_source_id_2)
	base, _, is_generic := generic_app_parts(target)
	if !is_generic || !target.ends_with(']') || !t.is_known_type_name(flat.source_type_id(base)) {
		return none
	}
	arg := t.transform_expr(t.a.child(&node, 1))
	start := t.a.children.len
	t.a.children << arg
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          target
		children_start: start
		children_count: 1
		typ:            target_source_id_2
	})
}

fn (t &Transformer) generic_sum_constructor_call_type(node flat.Node) ?u32 {
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
	mut target := u32(0)
	if node.value.len > 0 && fn_node.kind in [.ident, .selector] {
		base := t.generic_call_type_arg_name(fn_id)
		if base != 0 {
			target = flat.source_type_id('${flat.source_type_name(base)}[${node.value}]')
		}
	} else if fn_node.kind == .index {
		target = t.generic_call_type_arg_name(fn_id)
	}
	if target == 0 || !flat.source_type_shape(target).has(.bracketed) {
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
	if arg_type == 0 {
		arg_type = t.reliable_stringify_type(arg_id)
	}
	if arg_type == 0 {
		arg_type = t.node_type(arg)
	}
	if t.normalize_type_alias(arg_type) == flat.source_type_string {
		escaped := t.make_call_typed('escape_default_string', [arg], flat.source_type_string)
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
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	clean_type := (if flat.source_type_shape(base_type).has(.pointer) {
		flat.source_type_id(flat.source_type_name(base_type)[1..])
	} else {
		base_type
	})
	resolved_sum_source_id_3, variants_source_ids_3 := t.concrete_sum_name_and_variants(clean_type)

	if variants_source_ids_3.len == 0 {
		return none
	}
	base_expr := t.transform_expr(base_id)
	if flat.source_type_shape(base_type).has(.pointer) {
		// A heap-escaped local is declared as a pointer but read as `*name`, its value.
		expr_type := t.node_type(base_expr)
		if expr_type != 0 && !flat.source_type_shape(expr_type).has(.pointer) {
			base_type = expr_type
		}
	}
	base := t.stable_transformed_expr_for_reuse(base_expr, base_type, 'sum_type')
	tag := t.make_sum_tag_selector(base, if flat.source_type_shape(base_type).has(.pointer) {
		.arrow
	} else {
		.dot
	})
	if fn_node.value == 'type_idx' {
		return t.build_sum_type_idx_chain(tag, resolved_sum_source_id_3, variants_source_ids_3.clone(), 0)
	}
	return t.build_sum_type_name_chain(tag, resolved_sum_source_id_3, variants_source_ids_3.clone(), 0)
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
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if base_type == 0 {
		base_type = t.raw_checker_node_type(base_id)
	}
	if base_type == 0 {
		return none
	}
	clean_type := (if flat.source_type_shape(base_type).has(.pointer) {
		flat.source_type_id(flat.source_type_name(base_type)[1..])
	} else {
		base_type
	})
	iface_name_source_id := t.resolve_interface_type_name(clean_type)

	if iface_name_source_id == 0 {
		return none
	}
	transformed_base := t.transform_selector_base_expr(base_id)
	mut value_type := t.node_type(transformed_base)
	if value_type == 0 {
		value_type = base_type
	}
	value := t.stable_transformed_expr_for_reuse(transformed_base, value_type, 'iface_type')
	op := if flat.source_type_shape(value_type).has(.pointer) { flat.Op.arrow } else { flat.Op.dot }
	tag := t.make_selector_op(value, '_typ', flat.source_type_int, op)
	if fn_node.value == 'type_idx' {
		impl_names := if t.is_builtin_ierror_interface_name(iface_name_source_id) {
			t.tc.ierror_impl_names()
		} else {
			t.interface_impl_index_for_transform(iface_name_source_id).names.ids.clone()
		}
		return t.build_interface_type_idx_chain(tag, iface_name_source_id, impl_names, 0)
	}
	impl_names := if t.is_builtin_ierror_interface_name(iface_name_source_id) {
		t.tc.ierror_impl_names()
	} else {
		t.interface_impl_index_for_transform(iface_name_source_id).names.ids.clone()
	}
	return t.build_interface_type_name_chain(tag, iface_name_source_id, impl_names, 0)
}

fn (mut t Transformer) build_interface_type_idx_chain(tag flat.NodeId, iface_name_id u32, impls_ids []u32, idx int) flat.NodeId {
	if idx >= impls_ids.len {
		// A tag without a dispatch ID marker bit is a late container's type index
		// (see interface_container_cast_type_id); a zero tag stays zero.
		marker := t.make_paren(t.make_infix(.amp, tag, t.make_int_literal(types.interface_dispatch_id_marker)))
		is_container := t.make_infix(.eq, marker, t.make_int_literal(0))
		then_block := t.make_block([t.make_expr_stmt(tag)])
		else_block := t.make_block([t.make_expr_stmt(t.make_int_literal(0))])
		start := t.a.children.len
		t.a.children << is_container
		t.a.children << then_block
		t.a.children << else_block
		return t.a.add_node(flat.Node{
			kind:           .if_expr
			children_start: start
			children_count: 3
			typ:            flat.source_type_int
		})
	}
	impl_source_id := impls_ids[idx]

	type_id := t.interface_impl_type_id(iface_name_id, impl_source_id) or {
		return t.build_interface_type_idx_chain(tag, iface_name_id, impls_ids.clone(), idx + 1)
	}
	cond := t.make_infix(.eq, tag, t.make_int_literal(type_id))
	then_block := t.make_block([
		t.make_expr_stmt(t.make_int_literal(t.type_index(impl_source_id, t.cur_module))),
	])
	else_expr := t.build_interface_type_idx_chain(tag, iface_name_id, impls_ids.clone(), idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            flat.source_type_int
	})
}

fn (mut t Transformer) build_interface_type_name_chain(tag flat.NodeId, iface_name_id u32, impls_ids []u32, idx int) flat.NodeId {
	if idx >= impls_ids.len {
		return t.make_string_literal('')
	}
	impl_source_id := impls_ids[idx]
	impl := flat.source_type_name(impl_source_id)
	type_id := t.interface_impl_type_id(iface_name_id, impl_source_id) or {
		return t.build_interface_type_name_chain(tag, iface_name_id, impls_ids.clone(), idx + 1)
	}
	display := interface_type_name_display(impl)
	cond := t.make_infix(.eq, tag, t.make_int_literal(type_id))
	then_block := t.make_block([t.make_expr_stmt(t.make_string_literal(display))])
	else_expr := t.build_interface_type_name_chain(tag, iface_name_id, impls_ids.clone(), idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            flat.source_type_string
	})
}

// interface_type_name_display shortens every module-qualified name in a type spelling,
// so `[2]shapes.Box` reads `[2]Box`, as `shapes.Box` reads `Box`.
fn interface_type_name_display(name string) string {
	if !name.contains('.') {
		return name
	}
	mut out := []u8{cap: name.len}
	mut word_start := 0
	for i := 0; i <= name.len; i++ {
		c := if i < name.len { name[i] } else { u8(` `) }
		if c.is_letter() || c.is_digit() || c == `_` || c == `.` {
			continue
		}
		if word_start < i {
			out << name[word_start..i].all_after_last('.').bytes()
		}
		if i < name.len {
			out << c
		}
		word_start = i + 1
	}
	return out.bytestr()
}

// build_sum_type_name_chain builds sum type name chain data for transform.
fn (mut t Transformer) build_sum_type_name_chain(tag flat.NodeId, sum_name_id u32, variants_ids []u32, idx int) flat.NodeId {
	if idx >= variants_ids.len {
		return t.make_string_literal('')
	}
	variant_source_id_4 := variants_ids[idx]
	variant := flat.source_type_name(variant_source_id_4)
	display := if flat.is_named_variant_type_name(variant) {
		named_variant_display_short(variant)
	} else if variant.contains('.') {
		variant.all_after_last('.')
	} else {
		variant
	}
	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name_id, variant_source_id_4)))
	then_block := t.make_block([t.make_expr_stmt(t.make_string_literal(display))])
	else_expr := t.build_sum_type_name_chain(tag, sum_name_id, variants_ids.clone(), idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            flat.source_type_string
	})
}

fn (mut t Transformer) build_sum_type_idx_chain(tag flat.NodeId, sum_name_id u32, variants_ids []u32, idx int) flat.NodeId {
	if idx >= variants_ids.len {
		return t.make_int_literal(0)
	}
	variant_source_id_5 := variants_ids[idx]

	cond := t.make_infix(.eq, tag, t.make_int_literal(t.sum_type_index(sum_name_id, variant_source_id_5)))
	then_block := t.make_block([
		t.make_expr_stmt(t.make_int_literal(t.type_index(variant_source_id_5, t.cur_module))),
	])
	else_expr := t.build_sum_type_idx_chain(tag, sum_name_id, variants_ids.clone(), idx + 1)
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	t.a.children << else_block
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 3
		typ:            flat.source_type_int
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
	base_type_source_id := t.lvalue_type(base_id)
	if !is_pool_processor_type(base_type_source_id) {
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
		items := t.pool_processor_field(base, base_type_source_id, 'items', flat.source_type_id('[]voidptr'))
		item := t.make_index(items, idx, flat.source_type_voidptr)
		cast := t.make_cast(flat.source_type_id('&${elem_type}'), item, flat.source_type_id('&${elem_type}'))
		value := t.make_prefix(.mul, cast)
		t.set_node_typ(int(value), flat.source_type_id(elem_type))
		for stmt in prefix {
			t.pending_stmts << stmt
		}
		return value
	}
	result_name := t.new_temp('pool_results')
	idx_name := t.new_temp('pool_idx')
	results := t.pool_processor_field(base, base_type_source_id, 'results', flat.source_type_id('[]voidptr'))
	results_len := t.make_selector(results, 'len', flat.source_type_int)
	is_ref_results := method == 'get_results_ref'
	out_elem_type := (if is_ref_results {
		flat.source_type_id('&${elem_type}')
	} else {
		flat.source_type_id(elem_type)
	})
	out_type := flat.source_type_id('[]${flat.source_type_name(out_elem_type)}')
	prefix << t.make_decl_assign_typed(result_name, t.make_array_new_call(out_elem_type, t.make_int_literal(0), results_len), out_type)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond_results := t.pool_processor_field(base, base_type_source_id, 'results', flat.source_type_id('[]voidptr'))
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(cond_results, 'len', flat.source_type_int))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	body_results := t.pool_processor_field(base, base_type_source_id, 'results', flat.source_type_id('[]voidptr'))
	item := t.make_index(body_results, t.make_ident(idx_name), flat.source_type_voidptr)
	value := if is_ref_results {
		t.make_cast(flat.source_type_id('&${elem_type}'), item, flat.source_type_id('&${elem_type}'))
	} else {
		t.make_prefix(.mul, t.make_cast(flat.source_type_id('&${elem_type}'), item, flat.source_type_id('&${elem_type}')))
	}
	t.set_node_typ(int(value), out_elem_type)
	value_name := t.new_temp('pool_result')
	value_decl := t.make_decl_assign_typed(value_name, value, out_elem_type)
	push_call := t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(result_name)),
		t.make_prefix(.amp, t.make_ident(value_name)),
	], flat.source_type_void)
	loop_body := [value_decl, t.make_expr_stmt(push_call)]
	prefix << t.make_for_stmt(init, cond, post, loop_body, node)
	for stmt in prefix {
		t.pending_stmts << stmt
	}
	return t.make_ident(result_name)
}

// is_pool_processor_type reports whether is pool processor type applies in transform.
fn is_pool_processor_type(typ_id u32) bool {
	mut clean := flat.source_type_name(typ_id)
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.starts_with('mut ') {
		clean = clean[4..]
	}
	return clean == 'PoolProcessor' || clean.ends_with('.PoolProcessor')
}

// pool_processor_field supports pool processor field handling for Transformer.
fn (mut t Transformer) pool_processor_field(base flat.NodeId, base_type_id u32, field string, typ_id u32) flat.NodeId {
	base_type_shape_3 := flat.source_type_shape(base_type_id)

	op := if base_type_shape_3.has(.pointer) { flat.Op.arrow } else { flat.Op.dot }
	return t.make_selector_op(base, field, typ_id, op)
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
		imported_module := t.file_import_module(t.cur_file, base_node.value) or {
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
	mut base_type := t.raw_const_type_name_for_expr(base_id) or { u32(0) }
	mut recovered_or_value_type := false
	if base_type == 0 {
		base_type = (if base_node.kind in [.selector, .index] {
			t.lvalue_type(base_id)
		} else {
			t.node_type(base_id)
		})
	}
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if base_type == 0 || trimmed_transform_text(flat.source_type_name(base_type)).trim('&') in [
		'void',
		'voidptr',
	]
		|| t.generic_arg_is_unresolved(base_type) {
		if specialized_ret_source_id := t.specialized_interface_method_call_return_type(base_id, base_node) {
			specialized_ret_source_id_2 := specialized_ret_source_id

			base_type = specialized_ret_source_id_2
		}
		if unwrapped_type_source_id := t.or_expr_receiver_unwrapped_type(base_id) {
			unwrapped_type_source_id_3 := unwrapped_type_source_id
			unwrapped_type_source_id_6 := unwrapped_type_source_id_3

			if decl_type_is_usable(unwrapped_type_source_id_3) && !t.generic_arg_is_unresolved(unwrapped_type_source_id) {
				base_type = unwrapped_type_source_id_6
				recovered_or_value_type = true
			}
		}
	}
	if t.active_specialization_args.ids.len > 0 {
		specialized_source_id := t.subst_type(base_type, t.active_specialization_args.ids)
		specialized_source_id_3 := specialized_source_id

		if specialized_source_id != 0 && !t.generic_arg_is_unresolved(specialized_source_id) {
			base_type = specialized_source_id_3
		}
	}
	if method == 'str' {
		if value_type_source_id := t.pointer_value_expr_type(base_id) {
			value_type_source_id_9 := value_type_source_id

			base_type = value_type_source_id_9
		}
	}
	base_is_pointer := flat.source_type_shape(base_type).has(.pointer)
	if flat.source_type_shape(base_type).has(.pointer) {
		base_type = flat.source_type_id(flat.source_type_name(base_type)[1..])
	}
	if base_type == 0 {
		return none
	}
	if method == 'wait' && (flat.source_type_name(base_type) == 'thread' || flat.source_type_shape(base_type).has(.thread_type)) {
		return none
	}
	iface_name_source_id_4 := t.resolve_interface_type_name(base_type)
	iface_name := flat.source_type_name(iface_name_source_id_4)
	if iface_name_source_id_4 != 0 {
		t.mark_fn_used_name('${iface_name}.${method}')
		t.mark_interface_method_implementers_used(iface_name_source_id_4, method)
		if !isnil(t.tc) && method in t.tc.interface_abstract_method_names(iface_name_source_id_4) {
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
	if method == 'close' && (flat.source_type_name(base_type) == 'chan' || flat.source_type_shape(base_type).has(.channel_type)) {
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
	if t.cur_fn_is_generic && flat.source_type_shape(base_type).has(.bracketed)
		&& t.type_text_has_generic_placeholder(base_type, t.cur_module) {
		return none
	}
	builtin_base_type_source_id := t.normalize_type_alias(base_type)
	builtin_base_type_comparison_type_id := builtin_base_type_source_id
	builtin_base_type := flat.source_type_name(builtin_base_type_comparison_type_id)
	if method == 'string' && (flat.source_type_name(base_type) == '[]rune'
		|| (t.is_fixed_array_type(base_type) && fixed_array_outer_elem_type(base_type) == flat.source_type_rune)) {
		mut receiver := t.transform_expr(base_id)
		receiver_type := flat.source_type_name(t.normalized_node_type(receiver)).trim_left('&')
		if t.is_fixed_array_type(flat.source_type_id(receiver_type)) {
			// Fixed-array map preserves its shape even when the checker records a
			// dynamic result. Borrow its storage for the rune conversion call.
			receiver = t.fixed_array_data_to_array_no_alloc(receiver, flat.source_type_id(receiver_type), flat.source_type_id('[]rune'))
		}
		return t.make_call_typed('Array_rune__string', [receiver], flat.source_type_string)
	}
	if method == 'str' && t.is_array_transform_call(base_id) {
		base := t.transform_expr(base_id)
		transformed_type_source_id := t.node_type(base)
		transformed_type_shape := flat.source_type_shape(transformed_type_source_id)
		if transformed_type_shape.has(.array_type) {
			return t.wrap_string_conversion(base, transformed_type_source_id)
		}
	}
	if method == 'str' && t.is_builder_receiver(base_id, base_type) {
		method_name := 'strings.Builder.str'
		args := t.transform_receiver_method_args(node, base_id, method_name)
		return t.make_call_typed(method_name, args, flat.source_type_string)
	}
	if method == 'str' {
		if smartcast_str := t.smartcast_sum_str_call(base_id) {
			return smartcast_str
		}
		if t.is_enum_stringify_type(base_type) && t.enum_str_method_name(base_type) == none {
			mut value := t.transform_expr(base_id)
			if base_is_pointer {
				value = t.make_prefix(.mul, value)
				t.set_node_typ(int(value), base_type)
			}
			return t.enum_autostr_call(value, base_type)
		}
		if exact_call := t.lower_checker_selected_receiver_method(id, node, base_id, 'str') {
			return exact_call
		}
		// Some calls cloned during comptime/generic lowering no longer have the
		// checker's original call-id annotation. Resolve their concrete receiver
		// method before falling back to generated auto-stringification.
		mut method_names := ['${flat.source_type_name(base_type)}.${method}']
		mut collection_method := ''
		if !recovered_or_value_type {
			resolved_method := t.resolve_receiver_method_name(base_id, method)
			if resolved_method.len > 0 && resolved_method !in method_names {
				method_names << resolved_method
			}
		}
		if flat.source_type_shape(base_type).has(.array_type) || flat.source_type_shape(base_type).has(.map_type) {
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
				ret_type_source_id := t.receiver_method_return_type(method_name, node.typ)

				t.mark_fn_used_name(method_name)
				return t.make_call_typed(method_name, args, ret_type_source_id)
			}
		}
	}
	if method == 'str' && stringify_type_has_generic_placeholder(flat.source_type_name(base_type)) {
		return none
	}
	if flat.source_type_shape(base_type).has(.array_type) && method == 'str' {
		return t.wrap_string_conversion(t.transform_expr(base_id), base_type)
	}
	if method == 'str' {
		if t.is_enum_stringify_type(base_type) {
			return none
		}
		// `(&Struct).str()` keeps the reference so the pointee is stringified with V's `&`
		// prefix (or `&nil`); primitive/alias pointers keep their existing ptr_str behavior.
		if base_is_pointer {
			raw_alias_type_source_id_3 := t.raw_alias_type_for_expr(base_id)
			raw_alias_type_shape := flat.source_type_shape(raw_alias_type_source_id_3)
			if raw_alias_type_shape.has(.pointer) {
				if selected := t.checker_selected_receiver_method_name(id, 'str') {
					args := t.transform_receiver_method_args(node, base_id, selected)
					ret_type_source_id_2 := t.receiver_method_return_type(selected, node.typ)

					t.mark_fn_used_name(selected)
					return t.make_call_typed(selected, args, ret_type_source_id_2)
				}
				return t.wrap_string_conversion(t.transform_expr(base_id), raw_alias_type_source_id_3)
			}
			if aggregate_source_id := t.stringify_aggregate_type_name(base_type) {
				if selected := t.checker_selected_receiver_method_name(id, 'str') {
					args := t.transform_receiver_method_args(node, base_id, selected)
					ret_type_source_id_3 := t.receiver_method_return_type(selected, node.typ)

					t.mark_fn_used_name(selected)
					return t.make_call_typed(selected, args, ret_type_source_id_3)
				}
				return t.lower_ref_str_prefixed(t.transform_expr(base_id), aggregate_source_id)
			}
			if flat.source_type_shape(base_type).has(.array_type) || t.is_fixed_array_type(base_type) {
				return t.lower_ref_collection_str(t.transform_expr(base_id), base_type)
			}
			return t.wrap_string_conversion(t.transform_expr(base_id), flat.source_type_id('&${flat.source_type_name(base_type)}'))
		}
		mut stringify_type := t.raw_alias_type_for_expr(base_id)
		if stringify_type == 0 {
			stringify_type = t.pointer_value_expr_type(base_id) or {
				t.raw_var_type_for_expr(base_id) or { base_type }
			}
		}
		return t.wrap_string_conversion(t.transform_expr(base_id), stringify_type)
	}
	if builtin_base_type_source_id == flat.source_type_string && method == 'hex' && !base_is_pointer {
		return t.make_call_typed('string.hex', [t.transform_expr(base_id)], flat.source_type_string)
	}
	if flat.source_type_name(base_type) == '[]u8' {
		if method == 'bytestr' {
			return t.make_call_typed('Array_u8__bytestr', [t.transform_expr(base_id)], flat.source_type_string)
		}
		if method == 'hex' && !base_is_pointer {
			return t.make_call_typed('Array_u8__hex', [t.transform_expr(base_id)], flat.source_type_string)
		}
	}
	mut pointer_method := t.pointer_builtin_vbytes_method(base_is_pointer, builtin_base_type_source_id, method) or { '' }
	if method == 'vbytes' && !isnil(t.tc) {
		if resolved := t.tc.resolved_call_name(id) {
			if resolved in ['byteptr.vbytes', 'byteptr__vbytes', 'u8.vbytes', 'u8__vbytes'] {
				pointer_method = 'byteptr.vbytes'
			} else if resolved in ['voidptr.vbytes', 'voidptr__vbytes'] {
				pointer_method = 'voidptr.vbytes'
			}
		}
	}
	if pointer_method != '' {
		args := t.transform_receiver_method_args(node, base_id, pointer_method)
		ret_type_source_id_4 := t.receiver_method_return_type(pointer_method, node.typ)

		t.mark_fn_used(pointer_method)
		return t.make_call_typed(pointer_method, args, ret_type_source_id_4)
	}
	if t.is_builder_receiver(base_id, base_type) {
		for method_name in ['strings.Builder.${method}', 'Builder.${method}'] {
			if t.is_known_fn_name(method_name) {
				args := t.transform_receiver_method_args(node, base_id, method_name)
				ret_type_source_id_5 := t.receiver_method_return_type(method_name, node.typ)

				return t.make_call_typed(method_name, args, ret_type_source_id_5)
			}
		}
	}
	if builtin_base_type_source_id == flat.source_type_u8 && method in ['is_space', 'is_digit',
		'is_hex_digit', 'is_letter'] {
		return t.make_call_typed('u8__${method}', [t.transform_expr(base_id)], flat.source_type_bool)
	}
	if !base_is_pointer
		&& builtin_base_type_comparison_type_id in [flat.source_type_u8, flat.source_type_i8,
			flat.source_type_u16, flat.source_type_i16, flat.source_type_u32, flat.source_type_int,
			flat.source_type_u64, flat.source_type_i64, flat.source_type_rune]
		&& method in ['hex', 'hex_full'] {
		if !isnil(t.tc) {
			if resolved_method := t.tc.resolved_call_name(id) {
				if resolved_method == 'int_literal.${method}' {
					args := t.transform_receiver_method_args(node, base_id, resolved_method)
					return t.make_receiver_method_call_typed(node, resolved_method, args, flat.source_type_string)
				}
			}
		}
		return t.make_call_typed('${builtin_base_type}__${method}', [
			t.transform_expr(base_id),
		], flat.source_type_string)
	}
	if !base_is_pointer && builtin_base_type_source_id == flat.source_type_voidptr && method == 'hex_full' {
		return t.make_call_typed('voidptr__hex_full', [t.transform_expr(base_id)], flat.source_type_string)
	}
	if flat.source_type_shape(base_type).has(.array_type) || flat.source_type_shape(base_type).has(.map_type) {
		if flat.source_type_shape(base_type).has(.array_type) {
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
						flat.source_type_name(base_type)
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
					ret_type_source_id_6 := t.receiver_method_return_type(resolved_method, node.typ)

					if !t.validate_specialized_call_result(id, ret_type_source_id_6) {
						return t.make_empty()
					}
					return t.make_receiver_method_call_typed(node, resolved_method, args, ret_type_source_id_6)
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
		ret_type_source_id_7 := t.receiver_method_return_type(method_name, node.typ)

		if !t.validate_specialized_call_result(id, ret_type_source_id_7) {
			return t.make_empty()
		}
		return t.make_receiver_method_call_typed(node, method_name, args, ret_type_source_id_7)
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
				ret_type_source_id_8 := t.receiver_method_return_type(resolved_method, node.typ)

				if !t.validate_specialized_call_result(id, ret_type_source_id_8) {
					return t.make_empty()
				}
				return t.make_receiver_method_call_typed(node, resolved_method, args, ret_type_source_id_8)
			}
		}
	}
	if sum_method := t.resolve_smartcast_sum_receiver_method(base_id, method) {
		if !t.validate_resolved_receiver_method_args(node, base_id, sum_method) {
			return t.make_empty()
		}
		args := t.transform_receiver_method_args_with_base(node, t.receiver_base_for_resolved_method(base_id, sum_method), sum_method)
		ret_type_source_id_9 := t.receiver_method_return_type(sum_method, node.typ)

		if !t.validate_specialized_call_result(id, ret_type_source_id_9) {
			return t.make_empty()
		}
		return t.make_receiver_method_call_typed(node, sum_method, args, ret_type_source_id_9)
	}
	if t.validating_generic_spec {
		if base_node.kind == .ident && base_node.value in ['C', 'JS'] {
			// `C.fn(...)` is an extern call, not a method on an ident.
			return none
		}
		if field_type_source_id := t.lookup_struct_field_type(base_type, method) {
			if !t.validate_specialized_fn_field_call(id, node, base_id, field_type_source_id) {
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
			&& t.tc.named_type_implements_marker(base_type, flat.source_type_id('IClone'))
			&& !t.tc.ownership_type_has_clone_method(t.tc.parse_type(base_type)) {
			return none
		}
		base_name := if base_node.kind == .ident && base_node.value.len > 0 {
			base_node.value
		} else {
			flat.source_type_name(base_type)
		}
		t.record_monomorph_error('unknown function `${base_name}.${method}`')
	}
	return none
}

fn (mut t Transformer) smartcast_sum_str_call(base_id flat.NodeId) ?flat.NodeId {
	if t.smartcast_stack.len == 0 { return none }
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return none }
	sum_name_source_id := t.resolve_sum_name(sc.sum_type_name)
	sum_name := flat.source_type_name(sum_name_source_id)
	if sum_name_source_id == 0 || !t.sum_types.contains(sum_name) {
		return none
	}
	target_type_source_id_3 := t.trim_pointer_type(t.smartcast_target_type(sc))

	if target_type_source_id_3 == 0 || t.is_sum_type_name(target_type_source_id_3) {
		return none
	}
	receiver := t.apply_smartcast_contexts(t.make_plain_expr_for_smartcast(base_id), t.original_expr_type(base_id), t.smartcasts_for(key))
	return t.wrap_string_conversion(receiver, target_type_source_id_3)
}

fn (mut t Transformer) receiver_method_has_generic_template(receiver_type_id u32, method string) bool {
	if receiver_type_id == u32(0) || method == '' || t.skip_generics {
		return false
	}
	decls := t.cached_generic_fn_decls()
	if decls.len == 0 {
		return false
	}
	method_keys := t.generic_receiver_methods_by_name[method] or { return false }
	for key in method_keys {
		decl := decls[key] or { continue }
		if t.generic_receiver_decl_matches_type(receiver_type_id, decl, t.cur_module) {
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
			expected_name_source_id := t.semantic_type_name(expected)

			struct_type_source_id_5 := t.params_struct_type_name(expected_name_source_id) or {
				t.struct_arg_type_name(expected_name_source_id) or { u32(0) }
			}

			if struct_type_source_id_5 == 0
				|| !t.specialized_struct_field_args_match(node, child_idx, struct_type_source_id_5, true) {
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
						actual_name_source_id := t.specialized_expr_type_name(spread_id)
						actual_name := flat.source_type_name(actual_name_source_id)
						expected_name_source_id_2 := t.semantic_type_name(params[params.len - 1])
						expected_name := flat.source_type_name(expected_name_source_id_2)
						if !t.resolved_receiver_arg_compatible(spread_id, actual_name_source_id, expected_name_source_id_2) {
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
		actual_name_source_id_2 := t.specialized_expr_type_name(arg_id)
		actual_name := flat.source_type_name(actual_name_source_id_2)
		expected_name_source_id_3 := t.semantic_type_name(expected)
		expected_name := flat.source_type_name(expected_name_source_id_3)
		if t.resolved_receiver_arg_compatible(arg_id, actual_name_source_id_2, expected_name_source_id_3) {
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

fn (mut t Transformer) specialized_struct_field_args_match(node flat.Node, field_start int, struct_type_id u32, report_errors bool) bool {
	struct_type := flat.source_type_name(struct_type_id)
	mut valid := true
	mut supplied_fields := map[string]bool{}
	mut i := field_start
	for i < node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init {
			break
		}
		supplied_fields[field.value] = true
		field_type_source_id := t.lookup_struct_field_type(struct_type_id, field.value) or {
			if report_errors {
				t.record_monomorph_error('unknown field `${field.value}` in `${struct_type.all_after_last('.')}`')
			}
			valid = false
			i++
			continue
		}
		field_type := flat.source_type_name(field_type_source_id)
		if field.children_count == 0 {
			i++
			continue
		}
		value_id := t.a.child(field, 0)
		actual_type_source_id_2 := t.specialized_expr_type_name(value_id)
		actual_type := flat.source_type_name(actual_type_source_id_2)
		if !t.resolved_receiver_arg_compatible(value_id, actual_type_source_id_2, field_type_source_id) {
			if report_errors {
				t.record_monomorph_error('cannot initialize field `${field.value}` with `${actual_type}`; expected `${field_type}`')
			}
			valid = false
		}
		i++
	}
	info := t.lookup_struct_info(struct_type_id) or { return false }
	decl_metas := t.struct_field_decl_metas_in_module(comptime_struct_info_cache_key(info), info.module)
	for struct_field in info.fields {
		if struct_field.name in supplied_fields {
			continue
		}
		meta := decl_metas[struct_field.name] or { continue }
		if !field_decl_meta_has_attr(meta, 'required') {
			continue
		}
		if report_errors {
			display_name := info.name_text().all_after_last('.')
			t.record_monomorph_error('field `${display_name}.${struct_field.name}` must be initialized')
		}
		valid = false
	}
	return valid
}

fn field_decl_meta_has_attr(meta FieldDeclMeta, name string) bool {
	for attr in meta.attrs {
		if attr.all_before(':').trim_space() == name {
			return true
		}
	}
	return false
}

fn (mut t Transformer) validate_specialized_fn_field_call(id flat.NodeId, node flat.Node, base_id flat.NodeId, field_type_id u32) bool {
	display_name := t.resolved_receiver_call_display_name(node, base_id, '')
	if isnil(t.tc) {
		return true
	}
	fn_type := transform_fn_type(t.tc.parse_type(field_type_id)) or {
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
		actual_type_source_id_3 := t.specialized_expr_type_name(arg_id)
		actual_type := flat.source_type_name(actual_type_source_id_3)
		expected_type_source_id := t.semantic_type_name(*fn_type.params[i].typ)
		expected_type := flat.source_type_name(expected_type_source_id)
		if t.resolved_receiver_arg_compatible(arg_id, actual_type_source_id_3, expected_type_source_id) {
			continue
		}
		if t.fn_field_arg_compatible(actual_type_source_id_3, expected_type_source_id) {
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

fn (mut t Transformer) fn_field_arg_compatible(actual_type_id u32, expected_type_id u32) bool {
	actual_source_id := t.normalize_type_alias(actual_type_id)
	actual_comparison_type_id_2 := actual_source_id
	actual := flat.source_type_name(actual_comparison_type_id_2)
	expected_source_id := t.normalize_type_alias(expected_type_id)
	expected_comparison_type_id := expected_source_id
	expected := flat.source_type_name(expected_comparison_type_id)
	if t.is_integer_type_name(actual_source_id) && t.is_integer_type_name(expected_source_id) {
		return true
	}
	if actual_comparison_type_id_2 == expected_comparison_type_id {
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

fn (mut t Transformer) validate_specialized_call_result(id flat.NodeId, actual_type_id u32) bool {
	actual_type := flat.source_type_name(actual_type_id)
	if !t.validating_generic_spec || t.expected_expr_node != int(id)
		|| t.expected_expr_type == 0 || t.expected_expr_type in [
		flat.source_type_id('unknown'),
		flat.source_type_void,
	] {
		return true
	}
	expected_type_source_id := t.expected_expr_type
	expected_type := flat.source_type_name(expected_type_source_id)
	// An unsubstituted single-letter generic return still lacks a concrete type
	// at this validation point. The specialized function signature/C compiler
	// will validate it once monomorphization has supplied the concrete argument.
	if expected_type.len == 1 && expected_type[0] >= `A` && expected_type[0] <= `Z` {
		return true
	}
	if t.resolved_receiver_arg_compatible(id, actual_type_id, expected_type_source_id) {
		return true
	}
	// Reflection permits assigning an integer decoder result to the concrete
	// enum selected by `$for field in T.fields` under `$if field.is_enum`.
	// Keep that exception scoped to the unrolled enum-field body; ordinary
	// generic assignments must still report an int-to-enum mismatch.
	if t.allow_comptime_enum_int_assign && t.is_integer_type_name(actual_type_id)
		&& t.is_formatted_enum_type(expected_type_source_id) {
		return true
	}
	if t.in_return_expr {
		t.record_monomorph_error('cannot return `${actual_type}` as `${expected_type}`')
	} else {
		t.record_monomorph_error('cannot use `${actual_type}` as `${expected_type}`')
	}
	return false
}

// record_specialized_slot_mismatch reports a value whose concrete type the
// checker would not accept in the slot it is being placed in, and reports
// whether it did. Open generic templates are never body-checked (`[]T`
// legitimately matches any `[]U` until `T` is known), so a specialization
// returning `[]Foo` from a `[]Bar` function reaches the transform unreported;
// without this the slot was lowered anyway and only failed in the C compiler,
// pointing at generated code instead of the offending return.
//
// The question is compatibility, never whether a lowering exists. The element
// copy will box any struct into an interface, so an element that does not
// implement the target has to be rejected here rather than at `.id = src.id`
// in the generated C.
fn (mut t Transformer) record_specialized_slot_mismatch(actual types.Type, expected types.Type) bool {
	if !t.validating_generic_spec || isnil(t.tc) {
		return false
	}
	actual_name := actual.name()
	expected_name := expected.name()
	if t.generic_arg_is_unresolved(flat.source_type_id(actual_name)) || t.generic_arg_is_unresolved(flat.source_type_id(expected_name)) {
		return false
	}
	// The checker's own rule is the authority here: it already covers registered
	// aliases, integer widths, integer-to-float, float-to-float, integer-to-flag-enum,
	// interfaces and sum variants, recursing through arrays and maps.
	if t.tc.slot_value_compatible(actual, expected) {
		return false
	}
	if t.in_return_expr {
		t.record_monomorph_error('cannot return `${actual_name}` as `${expected_name}`')
	} else {
		t.record_monomorph_error('cannot use `${actual_name}` as `${expected_name}`')
	}
	return true
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
	lhs_type_source_id_2 := t.specialized_expr_type_name(transformed_lhs)
	lhs_type := flat.source_type_name(lhs_type_source_id_2)
	rhs_type_source_id := t.specialized_expr_type_name(transformed_rhs)
	rhs_type := flat.source_type_name(rhs_type_source_id)
	if t.specialized_comparison_types_compatible(transformed_lhs, lhs_type_source_id_2, transformed_rhs, rhs_type_source_id) {
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

fn (mut t Transformer) specialized_comparison_types_compatible(lhs_id flat.NodeId, lhs_type_id u32, rhs_id flat.NodeId, rhs_type_id u32) bool {
	if lhs_type_id == u32(0) || rhs_type_id == u32(0) || lhs_type_id == flat.source_type_unknown || rhs_type_id == flat.source_type_unknown {
		return true
	}
	lhs_source_id := t.normalize_type_alias(lhs_type_id)
	lhs_comparison_type_id := lhs_source_id

	rhs_source_id := t.normalize_type_alias(rhs_type_id)
	rhs_comparison_type_id := rhs_source_id

	if lhs_comparison_type_id == rhs_comparison_type_id {
		return true
	}
	lhs_is_number := t.is_integer_type_name(lhs_source_id) || lhs_comparison_type_id in [
		flat.source_type_f32,
		flat.source_type_f64,
	]
	rhs_is_number := t.is_integer_type_name(rhs_source_id) || rhs_comparison_type_id in [
		flat.source_type_f32,
		flat.source_type_f64,
	]
	if lhs_is_number && rhs_is_number {
		return true
	}
	return t.resolved_receiver_arg_compatible(lhs_id, lhs_type_id, rhs_type_id)
		|| t.resolved_receiver_arg_compatible(rhs_id, rhs_type_id, lhs_type_id)
}

fn (mut t Transformer) specialized_expr_type_name(id flat.NodeId) u32 {
	if t.specialized_expr_is_none(id) {
		return flat.source_type_none
	}
	mut typ := t.resolve_expr_type(id)
	if typ == 0 {
		typ = t.node_type(id)
	}
	if typ == 0 {
		typ = t.reliable_stringify_type(id)
	}
	return typ
}

fn (t &Transformer) resolved_receiver_call_display_name(node flat.Node, base_id flat.NodeId, method_name string) string {
	base := t.a.nodes[int(base_id)]
	base_name := if base.kind == .ident && base.value.len > 0 {
		base.value
	} else {
		flat.source_type_name(t.node_type(base_id))
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

fn (t &Transformer) callback_declared_type_text(raw_id u32) u32 {
	mut declared := t.normalize_type_alias(raw_id)
	for _ in 0 .. 16 {
		next_source_id_2 := t.normalize_type_alias(declared)

		if next_source_id_2 == declared { break }
		declared = next_source_id_2
	}
	return declared
}

fn (mut t Transformer) resolved_receiver_arg_compatible(arg_id flat.NodeId, actual_type_id u32, expected_type_id u32) bool {
	actual_type := flat.source_type_name(actual_type_id)
	expected_type := flat.source_type_name(expected_type_id)
	if actual_type_id == u32(0) || actual_type_id == flat.source_type_unknown || expected_type_id == u32(0) {
		return true
	}
	// An `unknown` expected type means the callee signature still carries an
	// unresolved generic parameter here - nothing can be validated against it
	// (including container forms such as `[]unknown`).
	if expected_type.contains('unknown') {
		return true
	}
	mut actual := t.callback_declared_type_text(actual_type_id)
	mut actual_callconv_type := actual_type
	if raw_source_id := t.raw_var_type_for_expr(arg_id) {
		raw_source_id_2 := raw_source_id
		raw := flat.source_type_name(raw_source_id_2)

		declared_source_id := t.callback_declared_type_text(raw_source_id_2)
		declared := flat.source_type_name(declared_source_id)
		mut corresponds := declared == flat.source_type_name(actual)
		if !corresponds && !isnil(t.tc) {
			raw_type := types.unalias_type(t.tc.parse_type(raw_source_id))
			resolved_type := types.unalias_type(t.tc.parse_type(actual_type_id))
			// Match the stored payload without the variadic call mode, then retain
			// the declaration's modes for the compatibility check below.
			corresponds = raw_type is types.FnType && resolved_type is types.FnType
				&& callback_type_without_variadic_modes(raw_type).name() ==
					callback_type_without_variadic_modes(resolved_type).name()
		}
		if corresponds {
			actual_callconv_type = raw
			actual = declared_source_id
		}
	}
	if !isnil(t.tc) && !t.tc.fn_type_callconv_compatible(t.tc.parse_type(flat.source_type_id(actual_callconv_type)),
		t.tc.parse_type(expected_type_id)) {
		return false
	}
	expected_comparison_type_id_3 := t.callback_declared_type_text(expected_type_id)
	expected_source_id_3 := expected_comparison_type_id_3
	expected := flat.source_type_name(expected_source_id_3)
	if t.is_integer_type_name(expected_source_id_3) {
		if literal := t.specialized_int_literal(arg_id) {
			return specialized_int_literal_fits_type(literal, expected_source_id_3)
		}
	}
	if t.is_integer_type_name(actual) && t.is_integer_type_name(expected_source_id_3) {
		return true
	}
	if flat.source_type_name(actual) == expected {
		return true
	}
	if !isnil(t.tc) && flat.source_type_name(actual).starts_with('fn') && expected.starts_with('fn') {
		if !t.callback_fn_type_modes_compatible(actual, expected_source_id_3) {
			return false
		}
		actual_fn := transform_fn_type(t.tc.parse_type(actual)) or { return false }
		expected_fn := transform_fn_type(t.tc.parse_type(expected_source_id_3)) or { return false }
		if !t.callback_fn_type_payloads_compatible(actual_fn, expected_fn, false) {
			return false
		}
		return t.tc.slot_value_compatible(types.Type(actual_fn), types.Type(expected_fn))
	}
	if t.expr_is_nil_like(arg_id)
		&& (expected.starts_with('&') || expected in ['voidptr', 'byteptr', 'charptr']) {
		return true
	}
	// A `mut`/pointer parameter is called with the value form (`r.read(mut
	// buf)` with `buf []u8` against `&[]u8`); cgen auto-refs such args.
	if expected.starts_with('&') && flat.source_type_name(actual) == expected[1..] {
		return true
	}
	// V also permits the inverse value coercion. This covers a borrowed value
	// passed to a by-value parameter and `&param` inside a specialized function
	// where a source `mut T` parameter already has `&T` storage after lowering.
	if flat.source_type_shape(actual).has(.pointer) && flat.source_type_name(actual)[1..] == expected {
		return true
	}
	if expected == '&void'
		&& (flat.source_type_shape(actual).has(.pointer) || flat.source_type_name(actual) in [
			'voidptr',
			'byteptr',
			'charptr',
			'nil',
		]) {
		return true
	}
	// Any pointer converts to voidptr.
	if expected in ['voidptr', 'byteptr', 'charptr']
		&& (flat.source_type_shape(actual).has(.pointer) || flat.source_type_name(actual) in [
			'voidptr',
			'byteptr',
			'charptr',
			'nil',
		]) {
		return true
	}
	// An empty array literal (`[]`) types as `[]void` and adopts the
	// parameter's element type.
	if flat.source_type_name(actual) == '[]void' && expected.starts_with('[]') {
		return true
	}
	arg := t.a.nodes[int(arg_id)]
	if arg.kind == .float_literal && expected_comparison_type_id_3 in [
		flat.source_type_f32,
		flat.source_type_f64,
	] {
		return true
	}
	// A char literal (`\`x\``) types as rune but coerces to u8 params.
	if arg.kind == .char_literal && expected_comparison_type_id_3 in [
		flat.source_type_u8,
		flat.source_type_rune,
		flat.source_type_char,
		flat.source_type_int,
		flat.source_type_u32,
	] {
		return true
	}
	if actual == flat.source_type_nil
		&& (expected.starts_with('&') || expected in ['voidptr', 'byteptr', 'charptr']) {
		return true
	}
	if expected.starts_with('?') {
		if flat.source_type_name(actual) == expected[1..] || t.specialized_expr_is_none(arg_id) {
			return true
		}
	}
	if expected.starts_with('!') {
		if flat.source_type_name(actual) == expected[1..] || t.is_ierror_type(actual) {
			return true
		}
	}
	if t.is_sum_type_name(expected_source_id_3) && t.sum_target_accepts_variant_type(expected_source_id_3, actual) {
		return true
	}
	// The reverse also passes validation: a sum-typed value flows into a
	// variant-typed parameter under an `is`-guard (smartcast) that a not-yet
	// unrolled `$for v in T.variants` body cannot expose to this check.
	if t.is_sum_type_name(actual) && t.sum_target_accepts_variant_type(actual, expected_source_id_3) {
		return true
	}
	if !isnil(t.tc) {
		expected_interface := flat.source_type_name(t.trim_all_pointer_type(expected_source_id_3))
		actual_interface := flat.source_type_name(t.trim_all_pointer_type(actual))
		if expected_interface in t.tc.interface_names
			&& t.tc.type_text_implements_interface(flat.source_type_id(actual_interface), flat.source_type_id(expected_interface)) {
			return true
		}
	}
	// Generic applications may differ only in module qualification of their
	// type arguments (`json2.Node[ValueInfo]` vs `json2.Node[json2.ValueInfo]`).
	mut a_app := flat.source_type_name(actual)
	mut e_app := expected
	for a_app.starts_with('&') && e_app.starts_with('&') {
		a_app = a_app[1..]
		e_app = e_app[1..]
	}
	if !isnil(t.tc) && t.tc.generic_type_name_matches(flat.source_type_id(a_app), flat.source_type_id(e_app)) {
		return true
	}
	return false
}

// callback_type_without_variadic_modes describes storage while preserving payload types and mut modes.
fn callback_type_without_variadic_modes(typ types.Type) types.Type {
	return match typ {
		types.FnType {
			mut params := []types.FnParam{cap: typ.params.len}
			for param in typ.params {
				params << types.FnParam{ typ: types.new_type_reference(types.Type(callback_type_without_variadic_modes(param.typ))), is_mut: param.is_mut }
			}
			types.Type(types.FnType{
				...typ
				params:      params
				return_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.return_type)))
				is_variadic: false
			})
		}
		types.Array {
			types.Type(types.Array{ ...typ, elem_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.elem_type))) })
		}
		types.ArrayFixed {
			types.Type(types.ArrayFixed{ ...typ, elem_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.elem_type))) })
		}
		types.Channel {
			types.Type(types.Channel{ ...typ, elem_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.elem_type))) })
		}
		types.Map {
			types.Type(types.Map{ ...typ, key_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.key_type))), value_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.value_type))) })
		}
		types.Pointer {
			types.Type(types.Pointer{ ...typ, base_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.base_type))) })
		}
		types.OptionType {
			types.Type(types.OptionType{ ...typ, base_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.base_type))) })
		}
		types.ResultType {
			types.Type(types.ResultType{ ...typ, base_type: types.new_type_reference(types.Type(callback_type_without_variadic_modes(typ.base_type))) })
		}
		types.MultiReturn {
			mut members := []types.Type{cap: typ.types.len}
			for member in typ.types { members << callback_type_without_variadic_modes(member) }
			types.Type(types.MultiReturn{ ...typ, types: members })
		}
		else { typ }
	}
}

fn callback_param_shared_atomic_mode(param string) string {
	payload := generic_fn_type_param_payload(param)
	if payload.starts_with('shared ') {
		return 'shared'
	}
	if payload.starts_with('atomic ') {
		return 'atomic'
	}
	return ''
}

fn (t &Transformer) callback_nested_fn_types(param string) []u32 {
	mut signatures := []u32{}
	t.collect_callback_nested_fn_types(param, mut signatures)
	return signatures
}

fn (t &Transformer) collect_callback_nested_fn_types(param string, mut signatures []u32) {
	mut payload := generic_fn_type_param_payload(param)
	mut payload_id := u32(0)
	for {
		unnamed := if payload.starts_with('map[') {
			payload
		} else {
			generic_fn_type_param_payload(payload)
		}
		if unnamed != payload {
			payload = unnamed
			continue
		}
		payload_id = t.trimmed_type_id(t.normalize_type_alias(flat.source_type_id(payload)))
		normalized := flat.source_type_name(payload_id)
		if normalized != payload {
			payload = normalized
			continue
		}
		if payload.starts_with('?') || payload.starts_with('!') || payload.starts_with('&') {
			payload = trimmed_transform_text(payload[1..])
		} else if payload.starts_with('shared ') {
			payload = trimmed_transform_text(payload[7..])
		} else if payload.starts_with('atomic ') {
			payload = trimmed_transform_text(payload[7..])
		} else if payload.starts_with('[]') {
			payload = trimmed_transform_text(payload[2..])
		} else if payload.starts_with('...') {
			payload = trimmed_transform_text(payload[3..])
		} else if payload.starts_with('chan ') || payload.starts_with('thread ') {
			prefix_len := if payload.starts_with('chan ') { 5 } else { 7 }
			payload = trimmed_transform_text(payload[prefix_len..])
		} else if payload.starts_with('map[') || payload.starts_with('[') {
			open := if payload.starts_with('map[') { 3 } else { 0 }
			mut depth := 0
			mut end := -1
			for i := open; i < payload.len; i++ {
				if payload[i] == `[` {
					depth++
				} else if payload[i] == `]` {
					depth--
					if depth == 0 {
						end = i
						break
					}
				}
			}
			if end < 0 || end + 1 >= payload.len {
				return
			}
			payload = trimmed_transform_text(payload[end + 1..])
		} else if payload.starts_with('(') && payload.ends_with(')') {
			for part in split_generic_args(payload[1..payload.len - 1]) {
				t.collect_callback_nested_fn_types(part, mut signatures)
			}
			return
		} else {
			_, args, is_generic := generic_app_parts(payload)
			if is_generic {
				for arg in args {
					t.collect_callback_nested_fn_types(arg, mut signatures)
				}
				return
			}
			break
		}
	}
	if payload.starts_with('fn(') || payload.starts_with('fn (') {
		signatures << payload_id
	}
}

fn (t &Transformer) callback_fn_type_modes_compatible(actual_id u32, expected_id u32) bool {
	actual_params, actual_return := fn_type_text_parts(flat.source_type_name(actual_id)) or { return false }
	expected_params, expected_return := fn_type_text_parts(flat.source_type_name(expected_id)) or { return false }
	if actual_params.len != expected_params.len {
		return false
	}
	for i, actual_param in actual_params {
		if generic_fn_type_param_payload(actual_param).starts_with('...') != generic_fn_type_param_payload(expected_params[i]).starts_with('...') {
			return false
		}
		if callback_param_shared_atomic_mode(actual_param) != callback_param_shared_atomic_mode(expected_params[i]) {
			return false
		}
		actual_nested := t.callback_nested_fn_types(actual_param)
		expected_nested := t.callback_nested_fn_types(expected_params[i])
		if actual_nested.len != expected_nested.len {
			return false
		}
		for j, nested_actual in actual_nested {
			if !t.callback_fn_type_modes_compatible(nested_actual, expected_nested[j]) {
				return false
			}
		}
	}
	if callback_param_shared_atomic_mode(actual_return) != callback_param_shared_atomic_mode(expected_return) {
		return false
	}
	actual_nested := t.callback_nested_fn_types(actual_return)
	expected_nested := t.callback_nested_fn_types(expected_return)
	if actual_nested.len != expected_nested.len {
		return false
	}
	for i, nested_actual in actual_nested {
		if !t.callback_fn_type_modes_compatible(nested_actual, expected_nested[i]) {
			return false
		}
	}
	return true
}

fn (t &Transformer) callback_fn_type_payloads_compatible(actual types.FnType, expected types.FnType, inside_container bool) bool {
	if actual.params.len != expected.params.len {
		return false
	}
	for i in 0 .. actual.params.len {
		if !types.fn_param_modes_compatible(actual, expected, i) {
			return false
		}
		param := types.unalias_type(types.fn_compatible_param_type(actual, i))
		expected_param := types.unalias_type(types.fn_compatible_param_type(expected, i))
		// The userdata conversion applies to a callback parameter, never to a
		// pointer buried in a parameter container or a nested callback return.
		if param is types.Pointer && expected_param is types.Pointer {
			if types.unalias_type(param.base_type) is types.Void
				|| types.unalias_type(expected_param.base_type) is types.Void {
				continue
			}
		}
		if !t.callback_payload_type_compatible(param, expected_param, inside_container) {
			return false
		}
	}
	if !inside_container && types.unalias_type(actual.return_type) is types.Pointer
		&& types.unalias_type(expected.return_type) is types.Pointer {
		// The ordinary function checker validates direct return covariance.
		return true
	}
	return t.callback_payload_type_compatible(actual.return_type, expected.return_type,
		inside_container)
}

fn (t &Transformer) callback_payload_type_compatible(actual types.Type, expected types.Type, inside_container bool) bool {
	a := types.unalias_type(actual)
	e := types.unalias_type(expected)
	if a is types.Array && e is types.Array {
		return t.callback_payload_type_compatible(a.elem_type, e.elem_type, true)
	}
	if a is types.ArrayFixed && e is types.ArrayFixed {
		actual_len := t.tc.fixed_array_len_value(a) or {
			if a.len_expr != e.len_expr {
				return false
			}
			return t.callback_payload_type_compatible(a.elem_type, e.elem_type, true)
		}
		expected_len := t.tc.fixed_array_len_value(e) or {
			if a.len_expr != e.len_expr {
				return false
			}
			return t.callback_payload_type_compatible(a.elem_type, e.elem_type, true)
		}
		return actual_len == expected_len && t.callback_payload_type_compatible(a.elem_type, e.elem_type,
			true)
	}
	if a is types.Map && e is types.Map {
		return t.callback_payload_type_compatible(a.key_type, e.key_type, true)
			&& t.callback_payload_type_compatible(a.value_type, e.value_type, true)
	}
	if a is types.Channel && e is types.Channel {
		return a.is_mut == e.is_mut
			&& t.callback_payload_type_compatible(a.elem_type, e.elem_type, true)
	}
	if a is types.Pointer && e is types.Pointer {
		return t.callback_payload_type_compatible(a.base_type, e.base_type, true)
	}
	if a is types.OptionType && e is types.OptionType {
		return t.callback_payload_type_compatible(a.base_type, e.base_type, true)
	}
	if a is types.ResultType && e is types.ResultType {
		return t.callback_payload_type_compatible(a.base_type, e.base_type, true)
	}
	if a is types.FnType && e is types.FnType {
		return t.callback_fn_type_payloads_compatible(a, e, true)
	}
	if (a is types.Primitive || a is types.Rune || a is types.Char || a is types.ISize || a is types.USize)
		&& (e is types.Primitive || e is types.Rune || e is types.Char || e is types.ISize || e is types.USize) {
		return t.tc.fn_param_compatible(a, e)
	}
	return a.name() == e.name() || (!inside_container && t.tc.slot_value_compatible(a, e)
		&& t.tc.slot_value_compatible(e, a))
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
				negative:  !literal.negative
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

fn specialized_int_literal_fits_type(literal SpecializedIntLiteral, typ u32) bool {
	return match typ {
		flat.source_type_u8 { !literal.negative && literal.magnitude <= 255 }
		flat.source_type_u16 { !literal.negative && literal.magnitude <= 65535 }
		flat.source_type_u32 { !literal.negative && literal.magnitude <= u64(4294967295) }
		flat.source_type_u64, flat.source_type_usize { !literal.negative }
		flat.source_type_i8 { specialized_signed_literal_fits(literal, 127) }
		flat.source_type_i16 { specialized_signed_literal_fits(literal, 32767) }
		flat.source_type_int, flat.source_type_i32, flat.source_type_rune {
			specialized_signed_literal_fits(literal, 2147483647)
		}
		flat.source_type_i64, flat.source_type_isize {
			specialized_signed_literal_fits(literal, u64(max_i64))
		}
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

// fn_field_mut_optional_param_types returns the parameters of a fn-typed field that has a
// `mut a ?T` parameter. Such an argument is passed like one of a named fn (see
// transform_mut_optional_value_call_arg); the checker records no callee type for a field
// call, so the transform has no parameters for it otherwise.
fn (t &Transformer) fn_field_mut_optional_param_types(base_type_id u32, field string) ?[]types.Type {
	if isnil(t.tc) {
		return none
	}
	receiver_type_id := t.normalize_type_alias(t.trim_pointer_type(base_type_id))
	field_type_id := t.lookup_struct_field_type(receiver_type_id, field)?
	fn_type := transform_fn_type(t.tc.parse_type(t.normalize_type_alias(field_type_id)))?
	for param in fn_type.params {
		typ := *param.typ
		if typ is types.Pointer && typ.base_type is types.OptionType && param.is_mut {
			return fn_type.params.map(*it.typ)
		}
	}
	return none
}

fn (t &Transformer) receiver_selector_is_fn_field(base_type_id u32, field string) bool {
	field_type_source_id := t.lookup_struct_field_type(base_type_id, field) or { return false }

	clean_shape := flat.source_type_shape(t.normalize_type_alias(field_type_source_id))
	return clean_shape.has(.function_compact) || clean_shape.has(.function_spaced)
}

// AllocWarning is an allocation lowering adds where the source shows none, as
// when a local whose address escapes is moved to the heap: the node it is
// reported for, and `pos`, where.
struct AllocWarning {
	node        flat.NodeId
	pos         token.Pos
	description string
}

// warn_alloc records an allocation for -warn-about-allocs, unless `decl`, the
// assignment it is made for, is marked `@[freed]`. That is decided here, from
// the declaration itself: a statement a comptime `$for` expanded is a copy the
// checker never saw. Workers keep their own warnings; the transform hands them
// all to the checker once they are merged, see report_alloc_warnings.
fn (mut t Transformer) warn_alloc(decl flat.Node, id flat.NodeId, pos token.Pos, description string) {
	if !t.tc.warn_about_allocs || decl.is_freed_assignment() {
		return
	}
	t.alloc_warnings << AllocWarning{
		node:        id
		pos:         pos
		description: description
	}
}

// report_alloc_warnings passes what warn_alloc recorded to the checker, which
// reports it with its own allocation warnings. Called on the main thread.
fn (mut t Transformer) report_alloc_warnings() {
	for warning in t.alloc_warnings {
		t.tc.record_transform_alloc_warning(warning.node, warning.pos, warning.description)
	}
	t.alloc_warnings.clear()
}

fn (mut t Transformer) record_monomorph_error(message string) {
	key := '${t.cur_file}:${t.cur_fn_name}:${message}'
	if t.monomorph_error_seen[key] {
		return
	}
	t.monomorph_error_seen[key] = true
	t.monomorph_errors << message
}

fn (t &Transformer) pointer_builtin_vbytes_method(base_is_pointer bool, builtin_base_type_id u32, method string) ?string {
	builtin_base_type := flat.source_type_name(builtin_base_type_id)
	if method != 'vbytes' {
		return none
	}
	if base_is_pointer && builtin_base_type_id == flat.source_type_u8 {
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
		if collection_receiver := receiver_collection_method_type(flat.source_type_id(receiver)) {
			return t.generic_arg_is_unresolved_collection_type(collection_receiver)
		}
		_, args, ok := generic_app_parts(receiver)
		return ok && args.len > 0 && t.generic_args_have_placeholders(flat.TypeTextList.from_names(args).ids)
	}
	return method_name_contains_mangled_open_generic_placeholder(method_name)
}

fn receiver_collection_method_type(receiver_id u32) ?u32 {
	receiver := flat.source_type_name(receiver_id)
	if receiver.starts_with('[]') || receiver.starts_with('map[') { return receiver_id }
	if receiver.contains('.[]') { return flat.source_type_id('[]${receiver.all_after('.[]')}') }
	if receiver.contains('.map[') {
		return flat.source_type_id('map[${receiver.all_after('.map[')}')
	}
	return none
}

fn (t &Transformer) generic_arg_is_unresolved_collection_type(receiver_id u32) bool {
	receiver := flat.source_type_name(receiver_id)
	if receiver.starts_with('[]') {
		return t.generic_arg_is_unresolved(flat.source_type_id(receiver[2..]))
	}
	if receiver.starts_with('map[') {
		return t.generic_arg_is_unresolved(t.map_key_type(receiver_id))
			|| t.generic_arg_is_unresolved(t.map_value_type(receiver_id))
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
fn (t &Transformer) is_builder_receiver(base_id flat.NodeId, base_type_id u32) bool {
	if is_builder_type_name(base_type_id, t.cur_module) {
		return true
	}
	if raw_type_source_id := t.raw_var_type_for_expr(base_id) {
		return is_builder_type_name(raw_type_source_id, t.cur_module)
	}
	if raw_field_type_source_id := t.raw_selector_field_type(base_id) {
		return is_builder_type_name(raw_field_type_source_id, t.cur_module)
	}
	return false
}

// raw_selector_field_type supports raw selector field type handling for Transformer.
fn (t &Transformer) raw_selector_field_type(id flat.NodeId) ?u32 {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.original_expr_type(base_id)
	if base_type == 0 {
		base_type = t.node_type(base_id)
	}
	if flat.source_type_shape(base_type).has(.pointer) {
		base_type = flat.source_type_id(flat.source_type_name(base_type)[1..])
	}
	raw_type_id, owner_type_id := t.lookup_struct_field_raw_type_with_owner(base_type, node.value) or {
		return none
	}

	owner_type := flat.source_type_name(owner_type_id)
	// Preserve an alias for auto-stringification, but use the checker's canonical
	// spelling instead of rebinding the declaration's bare source spelling in the
	// caller module. For example, `other.Holder.typ_text() Type` is `other.Type`, even when
	// the importing module declares an unrelated `Type`.
	if canonical_source_id := t.checker_struct_field_type_name(owner_type_id, node.value) {
		if t.is_type_alias_name(t.trim_pointer_type(canonical_source_id)) {
			return canonical_source_id
		}
		// The declaration's spelling names types through the owner's module, so
		// another module would rebind a bare `[]Row` to its own (or a first-wins) `Row`.
		// A struct declared in `main` has no module prefix, so it maps to `main`.
		owner_base, _, is_generic_owner := generic_app_parts(owner_type)
		owner_name := if is_generic_owner { owner_base } else { owner_type }
		owner_module := if owner_name.contains('.') {
			owner_name.all_before_last('.')
		} else {
			'main'
		}
		cur_module := if t.cur_module.len > 0 { t.cur_module } else { 'main' }
		if owner_module != cur_module {
			return canonical_source_id
		}
	}
	return raw_type_id
}

// is_builder_type_name recognizes strings.Builder, including its local spelling in strings.
fn is_builder_type_name(typ_id u32, module_name string) bool {
	mut clean := flat.source_type_name(typ_id)
	if clean.starts_with('&') {
		clean = clean[1..]
	}
	return clean == 'strings.Builder' || (clean == 'Builder' && module_name == 'strings')
}

// resolved_call_uses_receiver_type
// supports helper handling in transform.
fn (t &Transformer) resolved_call_uses_receiver_type(base_id flat.NodeId, receiver_type_id u32, params []types.Type) bool {
	receiver_type := flat.source_type_name(receiver_type_id)
	if params.len == 0 {
		return false
	}
	mut param_type := t.semantic_type_name(params[0])
	if flat.source_type_shape(param_type).has(.pointer) {
		param_type = flat.source_type_id(flat.source_type_name(param_type)[1..])
	}
	if sc := t.find_smartcast_for_expr(base_id) {
		sc_type_source_id := t.trim_pointer_type(t.smartcast_target_type(sc))

		if sc_type_source_id != 0 && t.normalize_type_alias(sc_type_source_id) == t.normalize_type_alias(param_type) {
			return true
		}
		sc_sum_source_id := t.trim_pointer_type(t.resolve_sum_name(sc.sum_type_name))

		if sc_sum_source_id != 0 && t.normalize_type_alias(sc_sum_source_id) == t.normalize_type_alias(param_type) {
			return true
		}
		if t.is_sum_type_name(param_type) {
			for parent_source_id in t.sum_type_parents_for_variant(sc.variant_name) {
				if t.normalize_type_alias(parent_source_id) == t.normalize_type_alias(param_type) {
					return true
				}
			}
		}
	}
	mut base_type := receiver_type
	if base_type.starts_with('&') {
		base_type = base_type[1..]
	}
	if raw_type_source_id := t.raw_var_type_for_expr(base_id) {
		raw_base_type_source_id := t.trim_pointer_type(raw_type_source_id)

		if raw_base_type_source_id != 0 && !t.generic_arg_is_unresolved(raw_base_type_source_id)
			&& t.normalize_type_alias(raw_base_type_source_id) != t.normalize_type_alias(flat.source_type_id(base_type)) {
			if t.normalize_type_alias(raw_base_type_source_id) == t.normalize_type_alias(param_type) {
				return true
			}
			if _ := t.embedded_receiver_path(raw_base_type_source_id, param_type) {
				return true
			}
			return false
		}
	}
	if base_type.len == 0 {
		return true
	}
	if t.normalize_type_alias(flat.source_type_id(base_type)) == t.normalize_type_alias(param_type) {
		return true
	}
	if _ := t.embedded_receiver_path(flat.source_type_id(base_type), param_type) {
		return true
	}
	return false
}

// receiver_base_for_resolved_method
// supports helper handling in transform.
fn (mut t Transformer) receiver_base_for_resolved_method(base_id flat.NodeId, method_name string) flat.NodeId {
	method_receiver_source_id := t.trim_pointer_type(flat.source_type_id(owner_name_view(method_name)))
	method_receiver := flat.source_type_name(method_receiver_source_id)
	params := t.call_param_types(method_name)
	receiver_param_type := (if params.len > 0 {
		t.semantic_type_name(params[0])
	} else {
		u32(0)
	})
	base_node := t.a.node(base_id)
	if base_node.kind == .ident && base_node.value in t.heaped_amp_locals
		&& t.normalize_type_alias(t.var_type(base_node.value)) == t.normalize_type_alias(receiver_param_type) {
		// A reference receiver borrows the promoted source slot as an argument.
		return t.transform_call_arg_for_param(base_id, receiver_param_type)
	}
	method := method_name.all_after_last('.')
	method_iface_source_id := t.resolve_interface_type_name(method_receiver_source_id)
	method_iface_comparison_type_id := method_iface_source_id

	key := t.expr_key(base_id)
	for source_type in [
		flat.source_type_name(t.raw_var_type_for_expr(base_id) or { u32(0) }),
		flat.source_type_name(t.original_expr_type(base_id)),
		flat.source_type_name(t.node_type(base_id)),
	] {
		clean_source_source_id := t.trim_pointer_type(flat.source_type_id(source_type))

		if clean_source_source_id != 0 && method_receiver_source_id != 0
			&& t.normalize_type_alias(clean_source_source_id) == t.normalize_type_alias(method_receiver_source_id) {
			if t.is_sum_type_name(method_receiver_source_id) {
				if sc := t.find_smartcast(key) {
					// A smartcast to the receiver sum itself (`match g.dir { Style { g.dir.m() } }`
					// with the sum `Style` a variant of `g.dir`'s sum) passes the narrowed value.
					// `original_expr_type` of a selector already reports that narrowed type.
					smartcast_target_source_id := t.trim_pointer_type(t.smartcast_target_type(sc))
					if smartcast_target_source_id != 0
						&& t.normalize_type_alias(smartcast_target_source_id) == t.normalize_type_alias(method_receiver_source_id) {
						return t.transform_expr_for_type(base_id, receiver_param_type)
					}
					original_type_source_id := t.trim_pointer_type(t.original_expr_type(base_id))

					sum_type_source_id := t.trim_pointer_type(t.resolve_sum_name(sc.sum_type_name))

					original_matches := original_type_source_id != 0
						&& t.normalize_type_alias(original_type_source_id) == t.normalize_type_alias(method_receiver_source_id)
					sum_matches := sum_type_source_id != 0
						&& t.normalize_type_alias(sum_type_source_id) == t.normalize_type_alias(method_receiver_source_id)
					if original_matches || sum_matches {
						return t.make_plain_expr_for_smartcast(base_id)
					}
				}
			}
			return t.transform_expr_for_type(base_id, receiver_param_type)
		}
		if method_iface_source_id != 0 {
			source_iface_source_id_2 := t.resolve_interface_type_name(clean_source_source_id)
			source_iface_comparison_type_id_2 := source_iface_source_id_2

			if source_iface_source_id_2 != 0 && source_iface_comparison_type_id_2 != method_iface_comparison_type_id {
				if embedded_method := t.resolve_embedded_interface_declared_method(source_iface_comparison_type_id_2,
					method)
				{
					if embedded_method == method_name {
						return t.transform_expr_for_type(base_id, receiver_param_type)
					}
				}
			}
		}
		if alias_target_source_id := t.alias_target_type_preserving_main_lock(clean_source_source_id) {
			if method_receiver_source_id != 0
				&& short_name_view(flat.source_type_name(t.trim_pointer_type(alias_target_source_id))) == short_name_view(method_receiver) {
				return t.transform_expr_for_type(base_id, receiver_param_type)
			}
		}
	}
	if embedded_base := t.embedded_receiver_base(base_id, method_name) {
		return embedded_base
	}
	sc := t.find_smartcast(key) or { return t.transform_expr(base_id) }
	if params.len == 0 {
		return t.transform_expr(base_id)
	}
	mut param_type := t.semantic_type_name(params[0])
	if flat.source_type_shape(param_type).has(.pointer) {
		param_type = flat.source_type_id(flat.source_type_name(param_type)[1..])
	}
	target_type_source_id := t.trim_pointer_type(t.smartcast_target_type(sc))

	if target_type_source_id != 0 {
		smartcast_base := t.apply_smartcast_contexts(t.make_plain_expr_for_smartcast(base_id), t.original_expr_type(base_id), t.smartcasts_for(key))
		if embedded_base := t.embedded_receiver_base_for_type(smartcast_base, target_type_source_id, param_type) {
			return embedded_base
		}
	}
	if target_type_source_id != 0
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(target_type_source_id) {
		return t.apply_smartcast_contexts(t.make_plain_expr_for_smartcast(base_id), t.original_expr_type(base_id), t.smartcasts_for(key))
	}
	if method_receiver_source_id != 0 && t.is_sum_type_name(method_receiver_source_id)
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(method_receiver_source_id) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	mut sum_type := t.resolve_sum_name(sc.sum_type_name)
	if flat.source_type_shape(sum_type).has(.pointer) {
		sum_type = flat.source_type_id(flat.source_type_name(sum_type)[1..])
	}
	if sum_type != 0 && t.normalize_type_alias(param_type) == t.normalize_type_alias(sum_type) {
		return t.make_plain_expr_for_smartcast(base_id)
	}
	original_type_source_id_2 := t.trim_pointer_type(t.original_expr_type(base_id))

	if original_type_source_id_2 != 0
		&& t.normalize_type_alias(param_type) == t.normalize_type_alias(original_type_source_id_2) {
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
	if flat.source_type_shape(param_type).has(.pointer) {
		param_type = flat.source_type_id(flat.source_type_name(param_type)[1..])
	}
	mut base_type := t.node_type(base_id)
	if base_type == 0 {
		base_type = t.lvalue_type(base_id)
	}
	if t.active_specialization_args.ids.len > 0 {
		specialized_source_id_2 := t.subst_type(base_type, t.active_specialization_args.ids)
		specialized_source_id_4 := specialized_source_id_2

		if specialized_source_id_2 != 0 && !t.generic_arg_is_unresolved(specialized_source_id_2) {
			base_type = specialized_source_id_4
		}
	}
	mut is_ptr := false
	if flat.source_type_shape(base_type).has(.pointer) {
		is_ptr = true
		base_type = flat.source_type_id(flat.source_type_name(base_type)[1..])
	}
	// Check the embedding path before transforming the receiver. Transforming it
	// speculatively can emit pending statements; falling back afterward would
	// transform and evaluate a side-effecting receiver a second time.
	_ := t.embedded_receiver_path(base_type, param_type) or { return none }
	return t.embedded_receiver_base_for_type(t.transform_expr(base_id), (if is_ptr {
		flat.source_type_id('&${flat.source_type_name(base_type)}')
	} else {
		base_type
	}), param_type)
}

fn (mut t Transformer) embedded_receiver_base_for_type(base flat.NodeId, base_type0_id u32, receiver_type_id u32) ?flat.NodeId {
	base_type0 := flat.source_type_name(base_type0_id)
	mut base_type := base_type0
	mut is_ptr := false
	if base_type.starts_with('&') {
		is_ptr = true
		base_type = base_type[1..]
	}
	path := t.embedded_receiver_path(flat.source_type_id(base_type), receiver_type_id) or { return none }
	mut cur := base
	mut current_is_ptr := is_ptr
	for field in path {
		op := if current_is_ptr { flat.Op.arrow } else { flat.Op.dot }
		field_type := (if field.typ != 0 {
			field.typ
		} else {
			field.raw_typ
		})
		cur = t.make_selector_op(cur, field.name, field_type, op)
		current_is_ptr = flat.source_type_shape(field_type).has(.pointer)
	}
	return cur
}

fn (t &Transformer) embedded_receiver_field(base_type_id u32, receiver_type_id u32) ?FieldInfo {
	path := t.embedded_receiver_path(base_type_id, receiver_type_id) or { return none }
	if path.len == 0 {
		return none
	}
	return path[0]
}

fn (t &Transformer) embedded_receiver_path(base_type_id u32, receiver_type_id u32) ?[]FieldInfo {
	if base_type_id == u32(0) || receiver_type_id == u32(0) {
		return none
	}
	mut lookup_type := t.trim_pointer_type(base_type_id)
	if alias_target_source_id := t.alias_target_type_preserving_main_lock(lookup_type) {
		if alias_target_source_id != lookup_type {
			if path := t.embedded_receiver_path(alias_target_source_id, receiver_type_id) {
				return path
			}
		}
	}
	if flat.source_type_name(lookup_type) !in t.structs && flat.source_type_shape(lookup_type).has(.dotted) {
		short_type := flat.source_type_name(lookup_type).all_after_last('.')
		if short_type in t.structs {
			lookup_type = flat.source_type_id(short_type)
		}
	}
	fields := t.embedded_fields[lookup_type] or {
		// The index holds declarations; a generic instance (`Wrap[int]`) has the
		// fields of its declaration with its type arguments.
		if !flat.source_type_shape(lookup_type).has(.bracketed) {
			return none
		}
		info := t.lookup_struct_info(lookup_type) or { return none }
		info.fields.filter(t.is_embedded_field(it))
	}
	clean_receiver_comparison_type_id := t.normalize_type_alias(receiver_type_id)

	for field in fields {
		// The semantic type of an embedded alias can be its underlying function
		// type. Keep the source alias spelling for promoted receiver matching.
		field_type := if field.raw_typ != 0 { field.raw_typ } else { field.typ }
		raw_field_id := t.trim_all_pointer_type(field_type)
		raw_field := flat.source_type_name(raw_field_id)
		clean_field_source_id := t.normalize_type_alias(raw_field_id)
		clean_field_comparison_type_id := clean_field_source_id
		clean_field := flat.source_type_name(clean_field_comparison_type_id)
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
		// The source spelling is relative to the module of the embedding struct
		// (`Base` in `shapes`); the semantic type is qualified (`shapes.Base`).
		semantic_field_comparison_type_id := t.normalize_type_alias(t.trim_all_pointer_type(field.typ))

		if field.name in [raw_field, short_raw_field, clean_field, short_field]
			&& (clean_field_comparison_type_id == clean_receiver_comparison_type_id || semantic_field_comparison_type_id == clean_receiver_comparison_type_id) {
			return [field]
		}
		if sub_path := t.embedded_receiver_path(clean_field_source_id, receiver_type_id) {
			mut path := []FieldInfo{}
			path << field
			path << sub_path
			return path
		}
	}
	return none
}

// receiver_method_return_type supports receiver method return type handling for Transformer.
fn (t &Transformer) receiver_method_return_type(method_name string, fallback_id u32) u32 {
	if !isnil(t.tc) {
		if typ := t.tc.fn_ret_types[method_name] {
			if typ !is types.Unknown && typ !is types.Void {
				return t.semantic_type_name(typ)
			}
		}
		if ret_text_source_id := t.tc.fn_ret_type_texts.type_id(method_name) {
			if alias_text_source_id := t.fn_return_alias_type_text(method_name, ret_text_source_id) {
				alias_text_source_id_2 := alias_text_source_id

				return alias_text_source_id_2
			}
		}
	}
	if ret_source_id := t.fn_ret_types.type_id(method_name) {
		ret_source_id_2 := ret_source_id

		return ret_source_id_2
	}
	return fallback_id
}

fn (t &Transformer) fn_return_alias_type_text(method_name string, ret_text_id u32) ?u32 {
	ret_text := flat.source_type_name(ret_text_id)
	clean := trimmed_transform_text(ret_text)
	if clean.len == 0 || isnil(t.tc) {
		return none
	}
	if _, _ := t.lookup_str_alias(flat.source_type_id(clean)) {
		return flat.source_type_id(clean)
	}
	if clean.contains('.') {
		return none
	}
	mod := t.tc.fn_type_modules[method_name] or { '' }
	if mod.len == 0 || mod in ['main', 'builtin'] {
		return none
	}
	qname := '${mod}.${clean}'
	if _, _ := t.lookup_str_alias(flat.source_type_id(qname)) {
		return flat.source_type_id(qname)
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
	ret_type_source_id_5 := t.receiver_method_return_type(resolved, node.typ)

	t.mark_fn_used_name(resolved)
	return t.make_receiver_method_call_typed(node, resolved, args, ret_type_source_id_5)
}

// try_lower_checker_selected_alias_method lowers a call that the checker resolved to a method
// declared on a type alias, including one inherited through another alias (`type
// WrappedNames = Names`, `fn (n Names) clone()`), before the builtin collection method of the
// same name: `arr[0].clone()` or `(*p).clone()` on such an array alias.
fn (mut t Transformer) try_lower_checker_selected_alias_method(call_id flat.NodeId, node flat.Node, base_id flat.NodeId) ?flat.NodeId {
	if isnil(t.tc) {
		return none
	}
	resolved := t.tc.resolved_call_name(call_id) or { return none }
	receiver := resolved.all_before_last('.')
	if receiver !in t.tc.type_aliases.ids || t.receiver_method_name_is_open_generic(resolved)
		|| !t.is_known_fn_name(resolved) {
		return none
	}
	// Only an alias of an array competes with the builtin array methods. The receiver
	// must have that alias' storage: the checker records one name for a call that a
	// multi-variant match branch repeats for each variant.
	base_type := t.normalize_type_alias(t.unshared_type_id(t.trim_all_pointer_type(t.node_type(base_id))))
	if base_type == 0 || !flat.source_type_shape(base_type).has(.array_type)
		|| t.normalize_type_alias(flat.source_type_id(receiver)) != base_type {
		return none
	}
	args := t.transform_receiver_method_args(node, base_id, resolved)
	return t.make_receiver_method_call_typed(node, resolved, args, t.receiver_method_return_type(resolved,
		node.typ))
}

fn (mut t Transformer) make_receiver_method_call_typed(node flat.Node, method_name string, args []flat.NodeId, typ_id u32) flat.NodeId {

	// Reachability was computed before generic/comptime clones were transformed.
	// Retain methods selected while lowering those generated bodies as well.
	t.mark_fn_used_name(method_name)
	mut call_args := []flat.NodeId{}
	mut borrowed_closure_cleanup := ''
	if node.children_count == 2 && args.len >= 2
		&& closure_lifetime_borrowed_callback_method(method_name) {
		source_arg := t.a.child(&node, 1)
		if t.expr_allocates_fresh_runtime_closure(source_arg) {
			closure_type_source_id := t.fresh_runtime_closure_type(source_arg) or { t.node_type(args[1]) }

			if closure_type_source_id != 0 {
				borrowed_closure_cleanup = t.new_temp('borrowed_closure')
				t.set_var_type(borrowed_closure_cleanup, closure_type_source_id)
				t.pending_stmts << t.make_decl_assign_typed(borrowed_closure_cleanup, args[1],
					closure_type_source_id)
				call_args = args.clone()
				call_args[1] = t.make_ident(borrowed_closure_cleanup)
				t.set_node_typ(int(call_args[1]), closure_type_source_id)
			}
		}
	}
	call := if borrowed_closure_cleanup == '' {
		t.make_call_typed(method_name, args, typ_id)
	} else {
		t.make_call_typed(method_name, call_args, typ_id)
	}
	generic_args := t.explicit_generic_call_arg_text(node)
	if generic_args.len > 0 {
		t.set_node_value(int(call), generic_args)
	}
	return t.finish_borrowed_closure_call(call, borrowed_closure_cleanup, typ_id)
}

fn closure_lifetime_borrowed_callback_method(method_name string) bool {
	if short_name_view(method_name) !in ['frame', 'suspend', 'untracked'] {
		return false
	}
	return owner_name_view(method_name) in ['builtin.closure.Lifetime', 'closure.Lifetime']
}

fn (mut t Transformer) finish_borrowed_closure_call(call flat.NodeId, closure_name string, typ_id u32) flat.NodeId {
	if closure_name == '' {
		return call
	}
	if typ_id == u32(0) || typ_id == flat.source_type_void {
		t.pending_stmts << t.make_expr_stmt(call)
		t.pending_stmts << t.make_local_closure_destroy_stmt(closure_name)
		return t.make_int_literal(0)
	}
	result_name := t.new_temp('borrowed_closure_result')
	t.set_var_type(result_name, typ_id)
	t.pending_stmts << t.make_decl_assign_typed(result_name, call, typ_id)
	t.pending_stmts << t.make_local_closure_destroy_stmt(closure_name)
	result := t.make_ident(result_name)
	t.set_node_typ(int(result), typ_id)
	return result
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
	checker_base_type = t.trim_all_pointer_type(checker_base_type)
	if receiver_name == flat.source_type_name(checker_base_type) {
		return true
	}
	mut base_type := t.node_type(base_id)
	if base_type == 0 && !isnil(t.tc) {
		base_type = t.semantic_type_name(t.tc.resolve_type(base_id))
	}
	base_type = t.trim_all_pointer_type(base_type)
	base_type = t.normalize_type_alias(base_type)
	if base_type == 0 {
		return true
	}
	// Heap promotion can replace a fixed-array alias with its storage type.
	// Keep the checker-selected alias method when its value layout still matches.
	if t.is_fixed_array_type(base_type)
		&& t.normalize_type_alias(flat.source_type_id(receiver_name)) == base_type {
		return true
	}
	if flat.source_type_shape(base_type).has(.array_type) || flat.source_type_shape(base_type).has(.map_type) {
		method := method_name.all_after_last('.')
		if method_name in t.receiver_method_candidates(base_type, method) {
			return true
		}
	}
	if receiver_name == flat.source_type_name(base_type) {
		return true
	}
	for clean_target in t.alias_chain_target_names(base_type) {
		if receiver_name == clean_target
			|| receiver_name.all_after_last('.') == clean_target.all_after_last('.') {
			return true
		}
	}
	if !isnil(t.tc) {
		resolved_base_type := flat.source_type_name(t.tc.resolve_imported_type_text_in_file(base_type, t.cur_file))
		if receiver_name == resolved_base_type {
			return true
		}
	}
	if !flat.source_type_shape(base_type).has(.dotted) && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' && receiver_name == '${t.cur_module}.${flat.source_type_name(base_type)}' {
		return true
	}
	if receiver_name.all_after_last('.') != flat.source_type_name(base_type).all_after_last('.') {
		return false
	}
	// An unqualified base type is the short form of the qualified receiver when their
	// short names match (a selective import renders the receiver as bare `Command`,
	// but its method is registered under `cli.Command`). A genuine bare/main receiver
	// hits the exact-match branch above, so it never reaches here.
	if !flat.source_type_shape(base_type).has(.dotted) && receiver_name.contains('.')
		&& receiver_name.all_after_last('.') == flat.source_type_name(base_type) {
		return true
	}
	return false
}

fn (t &Transformer) receiver_method_matches_type_name(method_name string, typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	receiver_name := method_name.all_before_last('.')
	if receiver_name.len == 0 {
		return true
	}
	mut clean := trimmed_transform_text(typ)
	for clean.starts_with('&') {
		clean = clean[1..]
	}
	if clean.starts_with('[]') || clean.starts_with('map[') {
		method := method_name.all_after_last('.')
		if method_name in t.receiver_method_candidates(flat.source_type_id(clean), method) {
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
	normalized_source_id_4 := t.normalize_type_alias(flat.source_type_id(clean))
	normalized := flat.source_type_name(normalized_source_id_4)
	if normalized_source_id_4 != 0 && normalized !in candidates {
		candidates << normalized
	}
	for clean_target in t.alias_chain_target_names(flat.source_type_id(clean)) {
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

// alias_chain_target_names lists the types an alias chain names, nearest first:
// `B` and `A` for a `C` with `type C = B` and `type B = A`. A method declared on
// any of them is a method of `C`, as in V1.
fn (t &Transformer) alias_chain_target_names(typ_id u32) []string {
	mut names := []string{}
	mut current := t.trim_pointer_type(typ_id)
	for _ in 0 .. 64 {
		target_source_id := t.alias_target_type_preserving_main_lock(current) or { break }
		target_id := t.trim_pointer_type(target_source_id)
		target := flat.source_type_name(target_id)
		if target_id == 0 || target_id == current || target in names {
			break
		}
		names << target
		current = target_id
	}
	return names
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
	if name == '' || is_builtin_collection_resolved_call(name) {
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
	return name == '' || is_raw_collection_method_name(name, 'array.') || name == 'array_clone'
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
	if t.smartcast_stack.len == 0 { return none }
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return none }
	variant := t.resolve_variant(sc.sum_type_name, sc.variant_name)
	mut receiver_types := []u32{}
	receiver_types << variant
	original_type := t.trim_pointer_type(t.original_expr_type(base_id))
	if original_type != 0 && original_type !in receiver_types { receiver_types << original_type }
	if sc.sum_type_name != 0 && sc.sum_type_name !in receiver_types {
		receiver_types << sc.sum_type_name
	}
	sum_type := t.resolve_sum_name(sc.sum_type_name)
	if sum_type != 0 && sum_type !in receiver_types { receiver_types << sum_type }
	for parent in t.sum_type_parents_for_variant(sc.variant_name) {
		if parent !in receiver_types { receiver_types << parent }
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
	if t.smartcast_stack.len == 0 { return none }
	if method == '' { return none }
	key := t.expr_key(base_id)
	sc := t.find_smartcast(key) or { return none }
	target := t.trim_pointer_type(t.smartcast_target_type(sc))
	if target == 0 { return none }
	mut receiver_types := []u32{}
	receiver_types << target
	variant := t.resolve_variant(sc.sum_type_name, sc.variant_name)
	if variant != 0 && variant !in receiver_types { receiver_types << variant }
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
fn (t &Transformer) sum_type_parents_for_variant(variant_id u32) []u32 {
	if parents := t.sum_variant_parents[variant_id] { return parents.clone() }
	short := t.variant_short_name(variant_id)
	if short != variant_id {
		if parents := t.sum_variant_parents[short] { return parents.clone() }
	}
	mut result := []u32{}
	for sum_name, variants in t.sum_types.ids {
		for variant in variants {
			if variant == variant_id || t.variant_short_name(variant) == short {
				result << flat.source_type_id(sum_name)
				break
			}
		}
	}
	if !isnil(t.tc) {
		for sum_name, variants in t.tc.sum_types.ids {
			sum_id := flat.source_type_id(sum_name)
			if sum_id in result { continue }
			for variant in variants {
				if variant == variant_id || t.variant_short_name(variant) == short {
					result << sum_id
					break
				}
			}
		}
	}
	return result
}

// receiver_method_candidates supports receiver method candidates handling for Transformer.
fn (t &Transformer) receiver_method_candidates(receiver_type_id u32, method string) []string {
	receiver_type := flat.source_type_name(receiver_type_id)
	mut clean_type := receiver_type
	if clean_type.starts_with('&') {
		clean_type = clean_type[1..]
	}
	if clean_type.starts_with('map[') {
		return t.map_receiver_method_candidates(flat.source_type_id(clean_type), method)
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
	suffix := generic_type_suffixes(flat.TypeTextList.from_names(args).ids)
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
	clean := trimmed_transform_text(receiver_type)
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

fn (t &Transformer) resolve_fixed_array_dynamic_receiver_method(fixed_type_id u32, method string) ?string {
	fixed_type := flat.source_type_name(fixed_type_id)
	elem_type := fixed_array_outer_elem_type_bytes(fixed_type)
	if elem_type.len == 0 {
		return none
	}
	return t.resolve_receiver_method_for_type(flat.source_type_id('[]${elem_type}'), method)
}

fn (mut t Transformer) lower_fixed_array_dynamic_receiver_method_call(node flat.Node, base_id flat.NodeId, fixed_type_id u32, method_name string) flat.NodeId {
	fixed_type := flat.source_type_name(fixed_type_id)
	elem_type := fixed_array_outer_elem_type_bytes(fixed_type)
	array_type := flat.source_type_id('[]${elem_type}')
	tmp_name := t.new_temp('fixed_arr')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, t.fixed_array_value_to_array(base_id, fixed_type_id, array_type), array_type)
	args := t.transform_receiver_method_args_with_base(node, t.make_ident(tmp_name), method_name)
	ret_type_source_id_6 := t.receiver_method_return_type(method_name, node.typ)

	t.mark_fn_used(method_name)
	return t.make_call_typed(method_name, args, ret_type_source_id_6)
}

fn fixed_array_outer_elem_type(s_id u32) u32 {
	source := flat.source_type_name(s_id)
	result := fixed_array_outer_elem_type_bytes(source)
	return if result == source { s_id } else { flat.source_type_id(result) }
}

fn fixed_array_outer_elem_type_bytes(type_text string) string {
	clean := trimmed_transform_text(type_text)
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
		return fixed_array_elem_type_bytes(clean)
	}
	mut out := elem
	for i := dims.len - 1; i > 0; i-- {
		out += '[${dims[i]}]'
	}
	return out
}

fn transform_push_receiver_candidate(mut candidates []string, candidate string) {
	if candidate != '' && candidate !in candidates {
		candidates << candidate
	}
}

fn transform_can_prefix_collection_receiver(module_name string) bool {
	return module_name != '' && module_name != 'main' && module_name != 'builtin'
}

fn transform_push_receiver_type_candidate(mut candidates []u32, candidate u32) {
	if candidate != 0 && candidate !in candidates {
		candidates << candidate
	}
}

fn (t &Transformer) receiver_type_text_variants(type_id u32) []u32 {
	clean := t.trimmed_type_id(type_id)
	mut names := []u32{}
	transform_push_receiver_type_candidate(mut names, clean)
	transform_push_receiver_type_candidate(mut names, flat.source_type_id(receiver_type_text_short_spelling(flat.source_type_name(clean))))
	if t.is_fixed_array_type(clean) {
		source := t.receiver_type_text_source_fixed_spelling(clean)
		transform_push_receiver_type_candidate(mut names, source)
		transform_push_receiver_type_candidate(mut names, flat.source_type_id(receiver_type_text_short_spelling(flat.source_type_name(source))))
	}
	return names
}

fn (t &Transformer) receiver_type_text_source_fixed_spelling(type_text_id u32) u32 {
	clean_id := t.trimmed_type_id(type_text_id)
	if clean_id == 0 || flat.source_type_shape(clean_id).has(.leading_bracket) || !t.is_fixed_array_type(clean_id) {
		return clean_id
	}
	clean := flat.source_type_name(clean_id)
	if !isnil(t.tc) {
		parsed := t.tc.parse_type(clean_id)
		if parsed is types.ArrayFixed {
			return t.semantic_type_name(parsed)
		}
	}
	elem, dims := transform_postfix_fixed_array_parts(clean)
	if elem.len == 0 || dims.len == 0 {
		return clean_id
	}
	mut source := elem
	for i := dims.len; i > 0; i-- {
		source = '[${dims[i - 1]}]${source}'
	}
	return flat.source_type_id(source)
}

fn transform_postfix_fixed_array_parts(type_text string) (string, []string) {
	clean := trimmed_transform_text(type_text)
	mut end := clean.len
	mut dims := []string{}
	for end > 0 && clean[end - 1] == `]` {
		start := transform_trailing_matching_bracket_start(clean, end)
		if start < 0 {
			break
		}
		dims << trimmed_transform_text(clean[start + 1..end - 1])
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
	clean := trimmed_transform_text(type_text)
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

fn (t &Transformer) receiver_type_text_module_names(type_id u32) []string {
	return t.receiver_type_text_module_names_bytes(flat.source_type_name(type_id))
}

fn (t &Transformer) receiver_type_text_module_names_bytes(type_text string) []string {
	clean := trimmed_transform_text(type_text)
	mut names := []string{}
	if clean.starts_with('[]') {
		return t.receiver_type_text_module_names_bytes(clean[2..])
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return t.receiver_type_text_module_names_bytes(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('map[') {
		key_type := flat.source_type_name(t.map_key_type(flat.source_type_id(clean)))
		value_type := flat.source_type_name(t.map_value_type(flat.source_type_id(clean)))
		for name in t.receiver_type_text_module_names_bytes(key_type) {
			transform_push_receiver_candidate(mut names, name)
		}
		for name in t.receiver_type_text_module_names_bytes(value_type) {
			transform_push_receiver_candidate(mut names, name)
		}
		return names
	}
	if t.is_fixed_array_type(flat.source_type_id(clean)) {
		return t.receiver_type_text_module_names_bytes(fixed_array_elem_type_bytes(clean))
	}
	if clean.contains('.') {
		transform_push_receiver_candidate(mut names, clean.all_before_last('.'))
	}
	return names
}

// map_receiver_method_candidates supports map receiver method candidates handling for Transformer.
fn (t &Transformer) map_receiver_method_candidates(receiver_type_id u32, method string) []string {
	clean_type_source_id := t.clean_map_type(receiver_type_id)
	clean_type := flat.source_type_name(clean_type_source_id)
	key_type_source_id_2 := t.map_key_type(clean_type_source_id)
	value_type_source_id_3 := t.map_value_type(clean_type_source_id)
	mut candidates := []string{}
	if key_type_source_id_2 == 0 || value_type_source_id_3 == 0 {
		transform_push_receiver_candidate(mut candidates, '${clean_type}.${method}')
		return candidates
	}
	key_types := t.receiver_type_text_variants(key_type_source_id_2)
	value_types := t.receiver_type_text_variants(value_type_source_id_3)
	mut map_types := []u32{}
	for key in key_types {
		for value in value_types {
			transform_push_receiver_type_candidate(mut map_types, flat.source_type_id('map[${flat.source_type_name(key)}]${flat.source_type_name(value)}'))
		}
	}
	for map_type in map_types {
		transform_push_receiver_candidate(mut candidates, '${flat.source_type_name(map_type)}.${method}')
	}
	mut module_names := []string{}
	if transform_can_prefix_collection_receiver(t.cur_module) {
		transform_push_receiver_candidate(mut module_names, t.cur_module)
	}
	for mod_name in t.receiver_type_text_module_names(key_type_source_id_2) {
		transform_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in t.receiver_type_text_module_names(value_type_source_id_3) {
		transform_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			transform_push_receiver_candidate(mut candidates, '${mod_name}.${flat.source_type_name(map_type)}.${method}')
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
		node := t.a.node(cur)
		match node.kind {
			.ident {
				return node.value
			}
			.selector, .index, .paren, .cast_expr, .expr_stmt {
				if node.children_count == 0 {
					return ''
				}
				cur = t.a.child(node, 0)
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
fn (mut t Transformer) clone_receiver_aliased_arg(recv_root string, arg_id flat.NodeId, value flat.NodeId, param_type_id u32) flat.NodeId {
	if recv_root == '' || t.expr_root_ident_name(arg_id) != recv_root {
		return value
	}
	return t.clone_checker_marked_receiver_alias_arg(arg_id, value, param_type_id)
}

// clone_checker_marked_receiver_alias_arg materializes the clone decision shared by direct
// arguments and elements packed into a variadic array.
fn (mut t Transformer) clone_checker_marked_receiver_alias_arg(arg_id flat.NodeId, value flat.NodeId, param_type_id u32) flat.NodeId {
	param_type_shape := flat.source_type_shape(param_type_id)
	if param_type_id == u32(0) || param_type_shape.has(.pointer) {
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
	if !t.compiler_default_clone_type_needs_work(param_type_id) {
		return value
	}
	return t.make_compiler_default_borrowed_clone_value(value, param_type_id, true)
}

// transform_receiver_method_args_with_base transforms helper data for transform.
fn (mut t Transformer) transform_receiver_method_args_with_base(node flat.Node, base flat.NodeId, method_name string) []flat.NodeId {
	mut args := []flat.NodeId{cap: int(node.children_count)}
	params := t.call_param_types(method_name)
	mut receiver := base
	if method_name.all_after_last('.') == 'string' && params.len > 0
		&& flat.source_type_name(t.normalize_type_alias(t.semantic_type_name(params[0]))) == '[]rune' {
		receiver_type := flat.source_type_name(t.normalized_node_type(receiver)).trim_left('&')
		if t.is_fixed_array_type(flat.source_type_id(receiver_type)) {
			receiver = t.fixed_array_data_to_array_no_alloc(receiver, flat.source_type_id(receiver_type), flat.source_type_id('[]rune'))
		}
	}
	args << receiver
	recv_root := t.expr_root_ident_name(base)
	param_offset := t.receiver_method_param_offset(base, node, params, method_name)
	mut snapshotted_args := 0
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
	ordered_fixed_args := t.call_has_mut_fixed_array_args(node, params, param_offset, variadic_idx)
	mut i := 1
	mut variadic_tail_supplied := false
	for i < node.children_count {
		if ordered_fixed_args {
			t.snapshot_fixed_array_call_args(mut args, snapshotted_args)
			snapshotted_args = args.len
		}
		param_idx := (args.len - 1) + param_offset
		arg_id := t.a.child(&node, i)
		arg_node := t.a.nodes[int(arg_id)]
		param_type := (if param_idx < params.len {
			t.semantic_type_name(params[param_idx])
		} else {
			u32(0)
		})
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
			struct_param_type := (if variadic_idx >= 0 && param_idx == variadic_idx
				&& flat.source_type_shape(param_type).has(.array_type) {
				flat.source_type_id(flat.source_type_name(param_type)[2..])
			} else {
				param_type
			})
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
				if arg_node.kind == .ident && arg_node.value == t.cur_fn_variadic_param {
					args << t.transform_expr_for_type(arg_id, t.semantic_type_name(variadic_type))
					variadic_tail_supplied = true
					i++
					break
				}
				if arg_node.kind == .prefix && arg_node.value == '...'
					&& arg_node.children_count > 0 {
					spread_id := t.a.child(&arg_node, 0)
					args << t.transform_variadic_spread_arg_for_param(spread_id, variadic_type, param_type)
					i++
					break
				}
				args << t.pack_variadic_args(node, i, variadic_type.elem_type)
				break
			}
		}
		if variadic_idx < 0 && arg_node.kind == .prefix && arg_node.value == '...'
			&& arg_node.children_count > 0 && params.len > 0 && param_idx < params.len {
			spread_id := t.a.child(&arg_node, 0)
			spread_base := t.stable_expr_for_reuse(spread_id)
			spread_count := params.len - param_idx
			t.add_spread_length_guard(spread_base, spread_count)
			for spread_offset in 0 .. spread_count {
				expected_source_id := t.semantic_type_name(params[param_idx + spread_offset])

				index_arg := t.make_spread_index_for_expected_param(spread_base, spread_offset, expected_source_id)
				args << t.transform_call_arg_for_param(index_arg, expected_source_id)
			}
			i++
			continue
		}
		if variadic_idx < 0 && param_idx < params.len {
			arg_type := t.tc.resolve_type(arg_id)
			if arg_type is types.MultiReturn && arg_type.types.len == params.len - param_idx {
				items := arg_type.types
				multi_type_source_id_2 := t.multi_return_type_name(items)

				value := t.stable_transformed_expr_for_reuse(t.transform_expr(arg_id), multi_type_source_id_2, 'multi_arg')
				for multi_idx, item_type in items {
					expected_idx := param_idx + multi_idx
					if expected_idx >= params.len {
						break
					}
					field_type_source_id := t.semantic_type_name(item_type)

					field := t.make_selector(value, 'arg${multi_idx}', field_type_source_id)
					clean_field_type_source_id := t.comptime_normalize_type_alias_chain(field_type_source_id)
					clean_field_type_shape_2 := flat.source_type_shape(clean_field_type_source_id)
					if !clean_field_type_shape_2.has(.pointer) && t.is_fixed_array_type(clean_field_type_source_id) {
						t.set_node_generic_params(int(field), [multi_return_fixed_array_arg_marker])
					}
					args << t.transform_call_arg_for_param(field, t.semantic_type_name(params[expected_idx]))
				}
				i++
				continue
			}
		}
		if converted := t.transform_mut_optional_value_call_arg(arg_id, param_type) {
			args << converted.value
			t.mut_optional_call_writebacks << converted.writeback
			i++
			continue
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
	if ordered_fixed_args {
		t.snapshot_fixed_array_call_args(mut args, snapshotted_args)
	}
	return args
}

fn (mut t Transformer) add_spread_length_guard(base flat.NodeId, needed int) {
	len_expr := t.make_selector(base, 'len', flat.source_type_int)
	too_short := t.make_infix(.lt, len_expr, t.make_int_literal(needed))
	count := t.stringify_expr(t.make_selector(base, 'len', flat.source_type_int), false)
	prefix := t.make_call_typed('string__plus', [
		t.make_string_literal('array decompose: array has '),
		count,
	], flat.source_type_string)
	needed_word := if needed == 1 { 'element is' } else { 'elements are' }
	singular_message := t.make_call_typed('string__plus', [
		prefix,
		t.make_string_literal(' element but ${needed} ${needed_word} needed'),
	], flat.source_type_string)
	plural_message := t.make_call_typed('string__plus', [
		prefix,
		t.make_string_literal(' elements but ${needed} ${needed_word} needed'),
	], flat.source_type_string)
	singular := t.make_infix(.eq, t.make_selector(base, 'len', flat.source_type_int), t.make_int_literal(1))
	report := t.make_if(singular, t.make_block([
		t.make_expr_stmt(t.make_call('panic', [singular_message])),
	]), t.make_block([
		t.make_expr_stmt(t.make_call('panic', [plural_message])),
	]))
	t.pending_stmts << t.make_if(too_short, t.make_block([report]), t.make_empty())
}

fn (mut t Transformer) make_spread_index_for_expected_param(base flat.NodeId, offset int, typ_id u32) flat.NodeId {
	typ_comparison_type_id_42 := typ_id
	typ_source_id_8 := typ_comparison_type_id_42
	typ := flat.source_type_name(typ_source_id_8)
	base_type_source_id := t.normalized_node_type(base)

	elem_type_source_id_2 := t.array_elem_type(base_type_source_id)
	elem_type_source_id_3 := elem_type_source_id_2

	id := t.make_index(base, t.make_int_literal(offset), (if elem_type_source_id_2 != 0 {
		elem_type_source_id_3
	} else {
		typ_source_id_8
	}))
	t.set_node_generic_params(int(id), [spread_index_expected_type_marker])
	if elem_type_source_id_2 == flat.source_type_string
		&& typ_comparison_type_id_42 in [flat.source_type_bool, flat.source_type_i8,
			flat.source_type_i16, flat.source_type_i32, flat.source_type_int, flat.source_type_i64,
			flat.source_type_f32, flat.source_type_f64, flat.source_type_u8, flat.source_type_u16,
			flat.source_type_u32, flat.source_type_u64] {
		fn_name := 'string__${typ}'
		t.mark_fn_used_name('string.${typ}')
		t.mark_fn_used_name(fn_name)
		return t.make_call_typed(fn_name, [id], typ_id)
	}
	t.set_node_typ(int(id), typ_id)
	return id
}

// receiver_method_param_offset supports receiver method param offset handling for Transformer.
fn (t &Transformer) receiver_method_param_offset(base_id flat.NodeId, node flat.Node, params []types.Type, method_name string) int {
	if params.len == 0 {
		return 0
	}
	base_type := flat.source_type_name(t.node_type(base_id))
	first_type := flat.source_type_name(t.normalize_type_alias(t.semantic_type_name(params[0])))
	clean_first := if first_type.starts_with('&') { first_type[1..] } else { first_type }
	clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if receiver_param_types_match(clean_first, clean_base)
		|| receiver_param_matches_method_name(clean_first, method_name)
		|| t.normalize_type_alias(flat.source_type_id(clean_first)) == t.normalize_type_alias(flat.source_type_id(clean_base))
		|| short_name_view(clean_first) == short_name_view(clean_base) {
		return 1
	}
	if params.len >= int(node.children_count) {
		return 1
	}
	return 0
}

fn receiver_param_matches_method_name(first string, method_name string) bool {
	if first == '' || method_name == '' || !method_name.contains('.') {
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
	base_type_source_id_9 := t.node_type(base_id)

	if base_type_source_id_9 != flat.source_type_string {
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
	return t.make_call_typed('string__${method}', args, flat.source_type_id(ret_type))
}

// try_lower_owned_string_free_call resets a string variable or field after an explicit
// `s.free()` in an ownership build. `string.free()` leaves its double-free marker in the
// header, so the ownership drop of that storage (when it is reassigned or goes out of
// scope) reported a double free. An empty string drops as a no-op, like the zero value
// that arrays and maps are reset to after their explicit `free()`.
fn (mut t Transformer) try_lower_owned_string_free_call(id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.children_count != 1 || isnil(t.tc) {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.value != 'free' || callee.children_count == 0 {
		return none
	}
	base_id := t.a.child(callee, 0)
	if !t.owned_string_free_target_is_storage(base_id) {
		return none
	}
	base_type := t.node_type(base_id)
	if base_type != flat.source_type_string {
		return none
	}
	if !t.tc.ownership_type_requires_destruction(t.tc.parse_type(flat.source_type_string)) {
		return none
	}
	if resolved := t.tc.resolved_call_name(id) {
		if resolved != 'string.free' {
			return none
		}
	}
	base := t.stable_expr_for_reuse(base_id)
	t.mark_fn_used_name('string.free')
	call := t.make_call_typed('string__free', [t.runtime_addr(base, base_type)], flat.source_type_void)
	t.pending_stmts << t.make_expr_stmt(call)
	t.pending_stmts << t.make_assign(base, t.make_string_literal(''))
	return t.make_empty()
}

// owned_string_free_target_is_storage reports whether `id` names string storage that a
// later ownership drop can reach: a variable, or a field of one.
fn (t &Transformer) owned_string_free_target_is_storage(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	return match node.kind {
		.ident { node.value.len > 0 && t.var_type(node.value) != 0 }
		.selector, .paren {
			node.children_count > 0 && t.owned_string_free_target_is_storage(t.a.child(&node, 0))
		}
		else { false }
	}
}

// try_fold_literal_string_call emits a literal when a pure string call has static operands.
fn (mut t Transformer) try_fold_literal_string_call(node flat.Node, callee flat.Node) ?flat.NodeId {
	if callee.children_count == 0 {
		return none
	}
	receiver := t.comptime_scalar_expr(t.a.child(&callee, 0), 0) or { return none }
	if receiver.typ != flat.source_type_string {
		return none
	}
	mut args := []string{}
	for i in 1 .. node.children_count {
		arg := t.comptime_scalar_expr(t.a.child(&node, i), 0) or { return none }
		if arg.typ != flat.source_type_string {
			return none
		}
		args << arg.value
	}
	value := comptime_string_scalar(receiver.value, callee.value, args) or { return none }
	return t.make_comptime_scalar_literal(value)
}

// lower_string_count_call builds lower string count call data for transform.
fn (mut t Transformer) lower_string_count_call(node flat.Node, fn_node flat.Node) ?flat.NodeId {
	base_id := t.a.child(&fn_node, 0)
	base_type_source_id_10 := t.node_type(base_id)

	if base_type_source_id_10 != flat.source_type_string {
		return none
	}
	base := t.stable_expr_for_reuse(base_id)
	mut prefix := []flat.NodeId{}
	t.drain_pending(mut prefix)
	result_name := t.new_temp('count')
	idx_name := t.new_temp('count_idx')
	prefix << t.make_decl_assign_typed(result_name, t.make_int_literal(0), flat.source_type_int)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), t.make_selector(base, 'len', flat.source_type_int))
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	elem_expr := t.make_index(base, t.make_ident(idx_name), flat.source_type_u8)
	elem_decl := t.make_decl_assign_typed('it', elem_expr, flat.source_type_u8)
	predicate_id := t.a.child(&node, 1)
	old_it_source_id := t.var_type('it')

	t.set_var_type('it', flat.source_type_u8)
	predicate := t.transform_expr(predicate_id)
	if old_it_source_id != 0 {
		t.set_var_type('it', old_it_source_id)
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
	node := t.a.node(id)
	if node.kind == .ident && node.value == name {
		return true
	}
	for i in 0 .. node.children_count {
		if t.expr_uses_ident(t.a.child(node, i), name) {
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
fn (t &Transformer) get_call_return_type(id flat.NodeId, node flat.Node) u32 {
	if ret_source_id := t.fn_value_call_return_type(node) {
		return t.call_return_type_name(ret_source_id, node)
	}
	if !isnil(t.tc) && t.a.has_vsh_source && t.cur_module == 'main'
		&& t.cur_file.ends_with('.vsh')
		&& node.children_count > 0 {
		callee := t.a.child_node(&node, 0)
		if callee.kind == .ident {
			selected := t.resolve_call_name(node)
			if selected != callee.value {
				if ret := t.tc.fn_ret_types[selected] {
					return t.call_return_type_name(t.semantic_type_name(ret), node)
				}
			}
		}
	}
	if ret_source_id := t.current_generic_receiver_call_return_type(node) {
		return ret_source_id
	}
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .selector && fn_node.children_count > 0 {
			receiver_type := flat.source_type_name(t.node_type(t.a.child(fn_node, 0))).trim_string_left('&')
			if args_source_ids := t.current_specialized_receiver_args(flat.source_type_id(receiver_type)) {
				receiver_base, _, is_generic_receiver := generic_app_parts(receiver_type)
				if is_generic_receiver {
					mut names := ['${receiver_base}.${fn_node.value}']
					if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] && !receiver_base.contains('.') {
						names << '${t.cur_module}.${receiver_base}.${fn_node.value}'
					}
					for name in names {
						if ret_source_id := t.fn_ret_types.type_id(name) {
							return substitute_generic_type_text(ret_source_id, args_source_ids)
						}
						if !isnil(t.tc) {
							if ret := t.tc.fn_ret_types[name] {
								return substitute_generic_type_text(t.semantic_type_name(ret), args_source_ids)
							}
						}
					}
				}
			}
		}
		if fn_node.kind == .ident && (t.var_type(fn_node.value)) == 0 {
			qualified_name := if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] && !fn_node.value.contains('.') {
				'${t.cur_module}.${fn_node.value}'
			} else {
				fn_node.value
			}
			for name in [fn_node.value, qualified_name] {
				if !isnil(t.tc) && t.tc.specialized_generic_fns[name] {
					if ret_source_id := t.fn_ret_types.type_id(name) {
						return t.call_return_type_name(ret_source_id, node)
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
		if fn_node.kind == .ident && (t.var_type(fn_node.value)) == 0 {
			if ret_source_id := t.local_fn_decl_return_type(fn_node.value) {
				return t.call_return_type_name(ret_source_id, node)
			}
		}
	}
	if ret_source_id := t.checker_resolved_non_builtin_return_type(id, node) {
		return ret_source_id
	}
	if node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident {
			local_type_source_id := t.var_type(fn_node.value)

			if local_type_source_id != 0 {
				if ret_source_id := t.local_fn_value_return_type_from_type(local_type_source_id) {
					return t.call_return_type_name(ret_source_id, node)
				}
			} else {
				if ret_source_id := t.local_fn_decl_return_type(fn_node.value) {
					return t.call_return_type_name(ret_source_id, node)
				}
			}
		}
		if fn_node.kind == .selector
			&& fn_node.value in ['clone', 'reverse', 'repeat', 'repeat_to_depth']
			&& fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			base_type_id := t.node_type(base_id)
			base_type := flat.source_type_name(base_type_id)
			clean_type_id := if base_type.starts_with('&') {
				flat.source_type_id(base_type[1..])
			} else {
				base_type_id
			}
			if fn_node.value == 'clone' && clean_type_id != 0 {
				return clean_type_id
			}
			if flat.source_type_shape(clean_type_id).has(.array_type) {
				return clean_type_id
			}
		}
		if fn_node.kind == .selector && fn_node.value == 'wait' && fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			base_type_source_id := t.node_type(base_id)

			clean_type_source_id_2 := t.membership_container_type(base_type_source_id)
			clean_type := flat.source_type_name(clean_type_source_id_2)
			if clean_type.starts_with('[]') {
				elem_type := clean_type[2..]
				if wait_type := thread_array_wait_return_type(flat.source_type_id(elem_type)) {
					return wait_type
				}
			}
			if t.is_fixed_array_type(clean_type_source_id_2) {
				elem_type := fixed_array_elem_type(clean_type_source_id_2)
				if wait_type := thread_array_wait_return_type(elem_type) {
					return wait_type
				}
			}
			if clean_type == 'thread' {
				return flat.source_type_void
			}
			if clean_type.starts_with('thread ') {
				return flat.source_type_id(clean_type[7..])
			}
		}
		if fn_node.kind == .selector && fn_node.value in ['keys', 'values']
			&& fn_node.children_count > 0 {
			base_id := t.a.child(fn_node, 0)
			mut base_type := flat.source_type_name(t.node_type(base_id))
			if base_type.len == 0 {
				base_type = flat.source_type_name(t.checker_node_type(base_id))
			}
			clean_type_source_id := t.clean_map_type(flat.source_type_id(base_type))
			clean_type_shape_3 := flat.source_type_shape(clean_type_source_id)
			if clean_type_shape_3.has(.map_type) {
				elem_type := if fn_node.value == 'keys' {
					flat.source_type_name(t.map_key_type(clean_type_source_id))
				} else {
					flat.source_type_name(t.map_value_type(clean_type_source_id))
				}
				if elem_type.len > 0 {
					return flat.source_type_id('[]${elem_type}')
				}
			}
		}
		// A method on a concrete generic instance (`Box[int].clone`) is registered under
		// the open form (`Box[T].clone`), whose stored return type collapsed `Box[T]` to
		// the bare base. Resolve it through the checker, which re-substitutes the concrete
		// arguments from the signature text, so the inferred decl type is `Box[int]`.
		if !isnil(t.tc) && fn_node.kind == .selector && fn_node.children_count > 0 {
			base_type := flat.source_type_name(t.node_type(t.a.child(fn_node, 0)))
			clean_base := if base_type.starts_with('&') { base_type[1..] } else { base_type }
			if clean_base.contains('[') && clean_base.ends_with(']') {
				if ci := t.tc.resolve_generic_struct_method(flat.source_type_id(clean_base), fn_node.value) {
					rn_source_id := t.semantic_type_name(ci.return_type)

					if rn_source_id != 0 && rn_source_id != flat.source_type_void && rn_source_id != flat.source_type_unknown {
						return t.normalize_type_alias(rn_source_id)
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
		return u32(0)
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
		if ret_source_id := t.fn_ret_types.type_id(qname) {
			return t.call_return_type_name(ret_source_id, node)
		}
	}
	if ret_source_id := t.fn_ret_types.type_id(name) {
		return t.call_return_type_name(ret_source_id, node)
	}
	return u32(0)
}

// generic_call_declared_return_type resolves the concrete return type of a call
// to a generic function from that function's declared signature: it infers the
// generic arguments from the call's argument types and substitutes them into the
// declared return type (e.g. `[T] fn(a T) T` called as `f(1.0)` yields `f64`).
// Returns none when the call is not generic, its signature is unknown, or the
// arguments leave a generic parameter undetermined, so callers keep their
// previous behaviour in those cases.
fn (t &Transformer) generic_call_declared_return_type(id flat.NodeId, node flat.Node) ?u32 {
	if isnil(t.tc) || node.kind != .call || node.children_count == 0 {
		return none
	}
	name := t.tc.resolved_call_name(id) or { t.resolve_call_name(node) }
	if name.len == 0 {
		return none
	}
	params := t.tc.fn_generic_params[name] or { return none }
	if params.len == 0 {
		return none
	}
	param_texts := t.tc.fn_param_type_texts.type_ids(name) or { return none }
	if param_texts.len == 0 {
		return none
	}
	ret_text_source_id_2 := t.tc.fn_ret_type_texts.type_id(name) or { return none }
	if ret_text_source_id_2 == 0 {
		return none
	}
	decl_module := t.tc.fn_type_modules[name] or { t.cur_module }
	mut inferred := map[string]u32{}
	for i, param_text in param_texts {
		arg_idx := i + 1
		if arg_idx >= node.children_count {
			break
		}
		mut arg_id := t.a.child(&node, arg_idx)
		mut inference_param_type := param_text
		if generic_param_text_is_variadic(param_text) {
			param_type, value_id := t.generic_variadic_arg_for_inference(param_text, generic_variadic_elem_param_text(param_text), arg_id)
			inference_param_type = param_type
			arg_id = value_id
		}
		arg_type_source_id_7 := t.expr_value_type(arg_id)
		arg_type := flat.source_type_name(arg_type_source_id_7)
		if arg_type_source_id_7 == 0 || arg_type in ['unknown', 'generic', 'void'] {
			continue
		}
		infer_generic_type_args(inference_param_type, arg_type_source_id_7, mut inferred)
	}
	mut args := []u32{cap: params.len}
	for param in params {
		inferred_arg_source_id := inferred[param] or { return none }
		inferred_arg := flat.source_type_name(inferred_arg_source_id)
		if inferred_arg_source_id == 0 || inferred_arg in ['unknown', 'generic', 'void'] {
			return none
		}
		caller_arg := t.normalize_type_in_module(inferred_arg_source_id, t.cur_module)
		args << t.lock_colliding_main_generic_type_text(caller_arg, decl_module)
	}
	resolved := substitute_generic_type_text_with_params(ret_text_source_id_2, args, params)
	if resolved == 0 || resolved in [flat.source_type_id('unknown'), flat.source_type_id('generic'),
		flat.source_type_void]
		|| is_generic_fn_placeholder_name(flat.source_type_name(resolved)) {
		return none
	}
	// Signature-owned names keep their declaring module; inferred arguments
	// above keep the caller's authority before that qualification happens.
	return t.normalize_type_in_module(resolved, decl_module)
}

fn (t &Transformer) current_generic_receiver_call_return_type(node flat.Node) ?u32 {
	if node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count == 0 {
		return none
	}
	current_receiver_source_id := t.current_fn_receiver_type()
	current_receiver := flat.source_type_name(current_receiver_source_id)
	current_generic_base, generic_args, current_ok := generic_app_parts(current_receiver)
	current_args := if current_ok {
		flat.TypeTextList.from_names(generic_args).ids
	} else {
		t.recorded_type_specialization_args(current_receiver_source_id) or { return none }
	}
	if current_args.len == 0 || t.generic_args_have_placeholders(current_args) {
		return none
	}
	current_base := if current_ok { current_generic_base } else { current_receiver }
	receiver_type := flat.source_type_name(t.node_type(t.a.child(callee, 0))).trim_string_left('&')
	if receiver_type.len > 0 {
		receiver_base, _, receiver_ok := generic_app_parts(receiver_type)
		if !receiver_ok || !current_receiver_matches_open_generic_base(current_base, receiver_base) {
			return none
		}
	}
	for name, ret in t.fn_ret_types.ids {
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
		if resolved != 0 && !t.generic_args_have_placeholders([resolved]) {
			return resolved
		}
	}
	return none
}

fn (t &Transformer) checker_resolved_non_builtin_return_type(id flat.NodeId, node flat.Node) ?u32 {
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
	result_source_id := t.checker_resolved_non_builtin_return_type_uncached(id, node) or { return none }

	cache.keys[slot] = int(id)
	cache.value_ptrs[slot] = voidptr(node.value.str)
	cache.value_lens[slot] = node.value.len
	cache.generations[slot] = cache.generation
	cache.results[slot] = result_source_id
	return result_source_id
}

fn (t &Transformer) checker_resolved_non_builtin_return_type_uncached(id flat.NodeId, node flat.Node) ?u32 {
	if isnil(t.tc) {
		return none
	}
	name := t.tc.resolved_call_name(id) or { return none }
	if is_builtin_collection_resolved_call(name) {
		return none
	}
	if name.starts_with('C.') {
		if module_name := t.tc.visible_c_fn_module(name) {
			if ret := t.tc.c_fn_module_ret_types['${module_name}\x01${name}'] {
				return t.call_return_type_name_in_module(t.semantic_type_name(ret), node, module_name)
			}
		}
	}
	decl_module := t.tc.fn_type_modules[name] or { '' }
	same_decl_module := decl_module == t.cur_module
		|| (decl_module in ['', 'main'] && t.cur_module in ['', 'main'])
	if !same_decl_module && node.children_count > 0 {
		fn_node := t.a.child_node(&node, 0)
		if fn_node.kind == .ident && (t.var_type(fn_node.value)) == 0 {
			if ret_source_id := t.local_fn_decl_return_type(fn_node.value) {
				candidate_source_id := t.call_return_type_name(ret_source_id, node)
				candidate_source_id_8 := candidate_source_id

				if decl_type_is_usable(candidate_source_id_8) || candidate_source_id == flat.source_type_void {
					return candidate_source_id
				}
			}
		}
	}
	if node.children_count > 0 && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		fn_node := t.a.child_node(&node, 0)
		short_name := short_name_view(name)
		if fn_node.kind == .ident && fn_node.value == short_name {
			qname := '${t.cur_module}.${short_name}'
			if ret := t.tc.fn_ret_types[qname] {
				candidate_source_id_2 := t.call_return_type_name(t.semantic_type_name(ret), node)
				candidate_source_id_9 := candidate_source_id_2

				if decl_type_is_usable(candidate_source_id_9) || candidate_source_id_2 == flat.source_type_void {
					return candidate_source_id_2
				}
			}
			lowered_qname := c_name(qname)
			if ret := t.tc.fn_ret_types[lowered_qname] {
				candidate_source_id_3 := t.call_return_type_name(t.semantic_type_name(ret), node)
				candidate_source_id_10 := candidate_source_id_3

				if decl_type_is_usable(candidate_source_id_10) || candidate_source_id_3 == flat.source_type_void {
					return candidate_source_id_3
				}
			}
		}
	}
	// The expression cache can contain a parser-inferred multi-return tail such as
	// `!(m.Match, _)`. Prefer the resolved declaration, whose complete return type
	// is authoritative, before falling back to that provisional expression type.
	if params := t.tc.fn_generic_params[name] {
		if params == ['T'] {
			if ret_text_source_id := t.tc.fn_ret_type_texts.type_id(name) {
				ret_text := flat.source_type_name(ret_text_source_id)

				base, _, applied_alias := generic_app_parts(ret_text)
				if applied_alias && (t.tc.type_aliases.contains(base)
					|| '${decl_module}.${base}' in t.tc.type_aliases.ids) {
					if args_source_ids := t.explicit_generic_call_args(node, t.cur_module) {
						if args_source_ids.len == params.len {
							candidate_source_id_4 := t.call_return_type_name_in_module(ret_text_source_id, node, decl_module)
							candidate_source_id_11 := candidate_source_id_4
							candidate := flat.source_type_name(candidate_source_id_11)
							if decl_type_is_usable(candidate_source_id_11)
								&& !generic_text_contains_param(candidate, params) {
								return candidate_source_id_4
							}
						}
					}
				}
			}
		}
	}
	if ret := t.tc.fn_ret_types[name] {
		if ret !is types.Unknown && ret !is types.Void {
			candidate_source_id_5 := t.call_return_type_name_in_module(t.semantic_type_name(ret), node, decl_module)
			candidate_source_id_12 := candidate_source_id_5

			if decl_type_is_usable(candidate_source_id_12) {
				return candidate_source_id_5
			}
		}
	}
	if ret_text_source_id := t.tc.fn_ret_type_texts.type_id(name) {
		if ret_text_source_id != 0 {
			candidate_source_id_6 := t.call_return_type_name_in_module(ret_text_source_id, node, decl_module)
			candidate_source_id_13 := candidate_source_id_6

			if decl_type_is_usable(candidate_source_id_13) || candidate_source_id_6 == flat.source_type_void {
				return candidate_source_id_6
			}
		}
	}
	if typ := t.tc.expr_type(id) {
		if typ !is types.Unknown && typ !is types.Void {
			candidate_source_id_7 := t.call_return_type_name(t.semantic_type_name(typ), node)
			candidate_source_id_14 := candidate_source_id_7

			if decl_type_is_usable(candidate_source_id_14) {
				return candidate_source_id_7
			}
		}
	}
	return none
}

fn (t &Transformer) call_return_type_name_in_module(ret_name_id u32, node flat.Node, module_name string) u32 {
	mut typ := ret_name_id
	if explicit_source_ids := t.explicit_generic_call_args(node, t.cur_module) {

		// This helper substitutes one T slot. Multiple parameters need the
		// checker's concrete expression type, rather than a comma-joined type.
		if explicit_source_ids.len != 1 {
			return u32(0)
		}
		caller_arg := t.normalize_type_in_module(explicit_source_ids[0], t.cur_module)
		generic_arg := t.lock_colliding_main_generic_type_text(caller_arg, module_name)
		if generic_arg != 0 {
			typ = t.specialize_generic_type_name(typ, generic_arg)
		}
	}
	return t.normalize_type_in_module(typ, module_name)
}

// call_return_type_name updates call return type name state for Transformer.
fn (t &Transformer) call_return_type_name(ret_name_id u32, node flat.Node) u32 {
	mut typ := ret_name_id
	if node.value.len > 0 {
		generic_arg_source_id := t.normalize_type_in_module(flat.source_type_id(node.value), t.cur_module)

		if generic_arg_source_id != 0 {
			typ = t.specialize_generic_type_name(typ, generic_arg_source_id)
		}
	}
	if t.is_optional_type_name(typ) {
		return typ
	}
	return t.normalize_type_alias(typ)
}

// specialize_generic_type_name supports specialize generic type name handling for Transformer.
fn (t &Transformer) specialize_generic_type_name(typ_id u32, generic_arg_id u32) u32 {
	if generic_arg_id == 0 || typ_id == 0 { return typ_id }
	typ := flat.source_type_name(typ_id)
	if trimmed_transform_text(typ) == 'T' { return generic_arg_id }
	result := t.specialize_generic_type_name_uncached(typ, flat.source_type_name(generic_arg_id))
	return if result == typ { typ_id } else { flat.source_type_id(result) }
}

fn (t &Transformer) specialize_generic_type_name_uncached(typ string, generic_arg string) string {
	clean := trimmed_transform_text(typ)
	if clean.len == 0 || generic_arg == '' {
		return typ
	}
	if clean == 'T' {
		return generic_arg
	}
	if clean.starts_with('&') {
		return '&' + t.specialize_generic_type_name_uncached(clean[1..], generic_arg)
	}
	if clean.starts_with('[]') {
		return '[]' + t.specialize_generic_type_name_uncached(clean[2..], generic_arg)
	}
	if clean.starts_with('?') {
		return '?' + t.specialize_generic_type_name_uncached(clean[1..], generic_arg)
	}
	if clean.starts_with('!') {
		return '!' + t.specialize_generic_type_name_uncached(clean[1..], generic_arg)
	}
	if clean.starts_with('...') {
		return '...' + t.specialize_generic_type_name_uncached(clean[3..], generic_arg)
	}
	if clean.starts_with('map[') {
		bracket_end := clean.index(']') or { return typ }
		key_type := t.specialize_generic_type_name_uncached(clean[4..bracket_end], generic_arg)
		value_type := t.specialize_generic_type_name_uncached(clean[bracket_end + 1..], generic_arg)
		return 'map[${key_type}]${value_type}'
	}
	if clean.starts_with('[') {
		bracket_end := clean.index(']') or { return typ }
		return clean[..bracket_end + 1] + t.specialize_generic_type_name_uncached(clean[bracket_end + 1..], generic_arg)
	}
	base, args, is_generic_app := generic_app_parts(clean)
	if is_generic_app {
		mut specialized_args := []string{cap: args.len}
		for arg in args {
			specialized_args << t.specialize_generic_type_name_uncached(arg, generic_arg)
		}
		return '${base}[${specialized_args.join(', ')}]'
	}
	return typ
}

fn (mut t Transformer) clone_result_error(source flat.NodeId) flat.NodeId {
	err := t.result_error_expr(source)
	if int(err) < 0 {
		return flat.empty_node
	}
	return t.clone_owned_ierror(err)
}

// clone_owned_ierror gives `err` an independent error object, preserving its concrete type.
fn (mut t Transformer) clone_owned_ierror(err flat.NodeId) flat.NodeId {
	t.mark_fn_used('string__clone')
	cloned := t.make_call_typed('__v3_clone_owned_ierror', [err], flat.source_type_id('IError'))
	if isnil(t.tc) {
		return cloned
	}
	for concrete_source_id_3 in t.tc.ierror_impl_names() {
		method := t.tc.concrete_method_signature_key(concrete_source_id_3, 'clone') or { continue }
		t.mark_fn_used_name(method)
	}
	return cloned
}

// base_type_text resolves the indexed source type spelling.
@[inline]
fn (value &CompilerDefaultCloneCallInfo) base_type_text() string {
	return flat.source_type_name(value.base_type)
}

// raw_base_type_text resolves the indexed source type spelling.
@[inline]
fn (value &CompilerDefaultCloneCallInfo) raw_base_type_text() string {
	return flat.source_type_name(value.raw_base_type)
}

// lifted_mut_optional_param_type returns the type of a lifted fn literal's parameter in its
// body. The parameter keeps the `&` of its `mut` slot, and like a named fn's (see
// mut_optional_param_value_type), a `mut a ?T` is `&?&T`.
fn (t &Transformer) lifted_mut_optional_param_type(param flat.Node) u32 {
	return t.lifted_mut_optional_param_slot_type(param, t.mut_optional_param_value_type(param.typ))
}

// lifted_mut_optional_param_view_type returns what the body of a lifted fn literal reads
// its parameter as: like lifted_mut_optional_param_type, with the view of a `mut a ?T`
// (`&?T`, see mut_optional_param_view_type).
fn (t &Transformer) lifted_mut_optional_param_view_type(param flat.Node) u32 {
	return t.lifted_mut_optional_param_slot_type(param, t.mut_optional_param_view_type(param.typ))
}

fn (t &Transformer) lifted_mut_optional_param_slot_type(param flat.Node, value_type u32) u32 {
	if !param.is_mut || param.op == .amp {
		return param.typ
	}
	if value_type == param.typ || !flat.source_type_shape(value_type).has(.option_type) {
		return param.typ
	}
	return flat.source_type_id('&${flat.source_type_name(value_type)}')
}
