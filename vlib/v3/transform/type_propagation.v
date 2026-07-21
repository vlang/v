module transform

import v3.flat
import v3.types

// propagate_decl_type infers the declared variable type from a .decl_assign node
// and registers it in var_types so downstream transforms can resolve it.
fn (mut t Transformer) propagate_decl_type(node flat.Node) {
	if node.kind != .decl_assign || node.children_count < 2 {
		return
	}
	if node.children_count >= 4 && node.children_count % 2 == 0 {
		for i := 0; i < node.children_count; i += 2 {
			t.propagate_decl_pair_type(node, i, i + 1, '')
		}
		return
	}
	t.propagate_decl_pair_type(node, 0, 1, node.typ)
}

fn (mut t Transformer) propagate_decl_pair_type(node flat.Node, lhs_idx int, rhs_idx int, fallback_type string) {
	if lhs_idx >= node.children_count || rhs_idx >= node.children_count {
		return
	}
	lhs_id := t.a.child(&node, lhs_idx)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	mut typ := ''
	rhs_id := t.a.child(&node, rhs_idx)
	rhs := t.a.nodes[int(rhs_id)]
	rhs_authority := t.decl_rhs_type(rhs_id)
	if t.is_fn_pointer_type_name(rhs_authority) {
		typ = rhs_authority
	} else if t.decl_type_should_override_fallback(rhs_authority, fallback_type, rhs) {
		typ = rhs_authority
	} else if decl_type_is_usable(fallback_type) {
		typ = fallback_type
	} else {
		if decl_type_is_usable(rhs_authority) {
			typ = rhs_authority
		} else if decl_type_is_usable(lhs.typ) {
			typ = lhs.typ
		}
	}
	if typ.len > 0 {
		t.set_var_type_with_raw(lhs.value, t.normalize_type_alias(typ), typ)
	}
}

fn (t &Transformer) decl_type_should_override_fallback(authority string, fallback string, rhs flat.Node) bool {
	if !decl_type_is_usable(authority) {
		return false
	}
	if !decl_type_is_usable(fallback) {
		return true
	}
	if t.local_struct_type_overrides_imported_alias(authority, fallback) {
		return true
	}
	if rhs.kind == .as_expr {
		return true
	}
	if rhs.kind == .ident && authority != fallback {
		if t.has_smartcast(rhs.value) {
			return true
		}
		if t.var_type(rhs.value) == authority {
			return true
		}
	}
	if rhs.kind == .call && authority != fallback && authority.contains('.')
		&& authority.all_after_last('.') == fallback.all_after_last('.') {
		return true
	}
	if rhs.kind == .spawn_expr && fallback == 'thread' && authority.starts_with('thread ') {
		return true
	}
	if rhs.kind == .infix && rhs.op == .right_shift_unsigned {
		return true
	}
	if map_value_starts_with_fixed_array(authority) && fallback.starts_with('map[') {
		return true
	}
	return authority.starts_with('&') && !fallback.starts_with('&')
}

fn (t &Transformer) local_struct_type_overrides_imported_alias(authority string, fallback string) bool {
	mut local := authority.trim_space()
	mut imported := fallback.trim_space()
	for prefix in ['[]', '&', '?', '!'] {
		for local.starts_with(prefix) && imported.starts_with(prefix) {
			local = local[prefix.len..]
			imported = imported[prefix.len..]
		}
	}
	if !t.bare_struct_name_is_local_to_current_module(local) || !imported.contains('.')
		|| imported.all_after_last('.') != local {
		return false
	}
	return true
}

fn map_value_starts_with_fixed_array(typ string) bool {
	clean := typ.trim_space()
	if !clean.starts_with('map[') {
		return false
	}
	bracket_end := generic_matching_bracket(clean, 3)
	return bracket_end + 1 < clean.len && clean[bracket_end + 1] == `[`
}

fn decl_type_is_usable(typ string) bool {
	if typ.len == 0 || typ in ['unknown', 'array', 'map'] || typ.contains('unknown') {
		return false
	}
	if typ.contains('typeof(') {
		return false
	}
	clean := typ.replace(' ', '')
	return clean !in ['Option', 'Optional', 'Result'] && !clean.starts_with('Option_')
		&& !clean.starts_with('Optional_') && !clean.starts_with('Result_')
}

fn (t &Transformer) checker_expr_type_name(id flat.NodeId) ?string {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	if typ := t.tc.expr_type(id) {
		mut name := t.normalize_type_alias(typ.name())
		if name.contains('typeof(') {
			name = t.normalize_type_alias(t.tc.resolve_type(id).name())
		}
		if decl_type_is_usable(name) && name != 'void' {
			return name
		}
	}
	return none
}

fn (t &Transformer) decl_rhs_type(id flat.NodeId) string {
	if fn_type := t.fn_value_type_name(id) {
		return fn_type
	}
	if map_type := t.map_expr_decl_type(id) {
		return map_type
	}
	if int(id) >= 0 {
		node := t.a.nodes[int(id)]
		if node.kind == .spawn_expr {
			if spawn_type := t.spawn_expr_decl_type(node) {
				return spawn_type
			}
		}
		if node.kind == .as_expr && node.value.len > 0 {
			return t.normalize_type_alias(node.value)
		}
		if node.kind == .cast_expr && node.value.len > 0 {
			target := t.normalize_type_alias(node.value)
			if t.is_sum_type_name(target) {
				return target
			}
		}
		if node.kind == .selector {
			selector_type := t.resolve_selector_type(node)
			if decl_type_is_usable(selector_type) {
				return selector_type
			}
		}
		if node.kind == .call {
			if ret := t.checker_resolved_non_builtin_return_type(id, node) {
				return ret
			}
		}
		if node.kind == .ident {
			local_type := t.resolve_expr_type(id)
			if decl_type_is_usable(local_type) {
				return local_type
			}
		}
		if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			child_type := t.lvalue_type(child_id)
			if child.kind == .struct_init && t.is_optional_type_name(child_type) {
				return '${child_type[..1]}&${t.optional_base_type(child_type)}'
			}
			prefix_type := t.node_type(id)
			if decl_type_is_usable(prefix_type) {
				return prefix_type
			}
			if child_type.len > 0 {
				return '&${child_type}'
			}
		}
	}
	return t.node_type(id)
}

fn (t &Transformer) spawn_expr_decl_type(node flat.Node) ?string {
	if node.children_count == 0 {
		return none
	}
	call_id := t.a.child(&node, 0)
	call := t.a.node(call_id)
	if call.kind != .call || call.children_count == 0 {
		return none
	}
	mut ret_type := t.node_type(call_id)
	if (ret_type.len == 0 || ret_type == 'unknown') && !isnil(t.tc) {
		callee := t.a.child_node(call, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			base := t.a.child_node(callee, 0)
			base_type := if base.kind == .ident {
				t.var_type(base.value)
			} else {
				t.node_type(t.a.child(callee, 0))
			}
			if info := t.tc.resolve_generic_struct_method(base_type, callee.value) {
				ret_type = info.return_type.name()
			}
		}
	}
	if ret_type.len == 0 || ret_type == 'unknown' {
		return none
	}
	if ret_type == 'void' {
		return 'thread'
	}
	return 'thread ${ret_type}'
}

fn (t &Transformer) map_expr_decl_type(id flat.NodeId) ?string {
	if int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .map_init {
		for candidate in [node.value, node.typ, t.node_type(id)] {
			if candidate.starts_with('map[') {
				return t.refine_map_init_fixed_array_value_type(node, candidate)
			}
		}
	}
	if node.kind == .call {
		map_type := t.new_map_call_type(node)
		if map_type.len > 0 {
			return map_type
		}
		if node.typ.starts_with('map[') {
			return node.typ
		}
	}
	return none
}

fn (t &Transformer) fn_value_type_name(id flat.NodeId) ?string {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .fn_literal {
		return t.fn_literal_type_text(node)
	}
	if typ := t.tc.expr_type(id) {
		if name := fn_value_type_name_from_type(typ) {
			return t.normalize_type_alias(name)
		}
	}
	if node.kind == .lambda_expr {
		typ := t.tc.resolve_type(id)
		if name := fn_value_type_name_from_type(typ) {
			return t.normalize_type_alias(name)
		}
	}
	return none
}

fn (t &Transformer) fn_literal_type_text(node flat.Node) string {
	mut params := []string{}
	for i in 0 .. node.children_count {
		child := t.a.child_node(&node, i)
		if child.kind == .param {
			raw := if child.typ.len > 0 { child.typ } else { child.value }
			params << fn_literal_param_type_text(raw)
		}
	}
	ret := node.typ.trim_space()
	if ret.len == 0 || ret == 'void' {
		return 'fn (${params.join(', ')})'
	}
	return 'fn (${params.join(', ')}) ${ret}'
}

fn fn_literal_param_type_text(raw string) string {
	mut text := raw.trim_space()
	if text.starts_with('mut ') {
		text = text[4..].trim_space()
	}
	space := generic_top_level_space_index(text)
	if space <= 0 {
		return text
	}
	head := text[..space].trim_space()
	tail := text[space + 1..].trim_space()
	if generic_fn_type_param_head_is_name(head, tail) {
		return tail
	}
	return text
}

fn fn_value_type_name_from_type(typ types.Type) ?string {
	name := typ.name()
	if name.len == 0 {
		return none
	}
	if typ is types.FnType {
		return name
	}
	if typ is types.Alias {
		if typ.base_type is types.FnType {
			return name
		}
	}
	return none
}

fn (t &Transformer) concrete_node_type_name(node flat.Node) string {
	if node.typ.len == 0 {
		return ''
	}
	typ := t.normalize_type_alias(node.typ)
	if typ.len == 0 || typ in ['array', 'map', 'unknown'] {
		return ''
	}
	if type_text_has_unresolved_generic_placeholder(typ) {
		return ''
	}
	return typ
}

fn type_text_has_unresolved_generic_placeholder(typ string) bool {
	clean := typ.trim_space()
	if clean.len == 0 {
		return false
	}
	if is_generic_fn_placeholder_name(clean) {
		return true
	}
	if clean.starts_with('&') {
		return type_text_has_unresolved_generic_placeholder(clean[1..])
	}
	if clean.starts_with('mut ') {
		return type_text_has_unresolved_generic_placeholder(clean[4..])
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return type_text_has_unresolved_generic_placeholder(clean[1..])
	}
	if clean.starts_with('...') {
		return type_text_has_unresolved_generic_placeholder(clean[3..])
	}
	if clean.starts_with('[]') {
		return type_text_has_unresolved_generic_placeholder(clean[2..])
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return type_text_has_unresolved_generic_placeholder(clean[4..bracket_end])
				|| type_text_has_unresolved_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return type_text_has_unresolved_generic_placeholder(clean[bracket_end + 1..])
		}
	}
	if !type_text_has_generic_placeholder_token(clean) {
		return false
	}
	if !clean.contains('[') {
		return true
	}
	_, args, ok := generic_app_parts(clean)
	if ok {
		for arg in args {
			if type_text_has_unresolved_generic_placeholder(arg) {
				return true
			}
		}
	}
	return false
}

fn type_text_has_generic_placeholder_token(text string) bool {
	for i := 0; i < text.len; i++ {
		ch := text[i]
		if ch < `A` || ch > `Z` {
			continue
		}
		prev_is_ident := i > 0 && type_text_ident_char(text[i - 1])
		next_is_ident := i + 1 < text.len && type_text_ident_char(text[i + 1])
		if !prev_is_ident && !next_is_ident {
			return true
		}
	}
	return false
}

fn type_text_ident_char(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `0` && ch <= `9`) || ch == `_`
}

// resolve_selector_type resolves the type of a .selector node (e.g. `obj.field`).
// Looks up the base expression type, then finds the field in the struct definition.
fn (t &Transformer) resolve_selector_type(node flat.Node) string {
	if node.kind != .selector || node.children_count == 0 {
		return ''
	}
	base_id := t.a.child(&node, 0)
	base_node := t.a.nodes[int(base_id)]
	if base_node.kind == .ident && base_node.value == 'C' {
		// `C.<name>` references a C symbol (e.g. the `C.stdout &C.FILE` global).
		// Resolve it via the recorded C global/const type and never fall through to
		// the V struct field-name heuristics below: an unrelated V struct may have a
		// field with the same name (e.g. `os.Pipe.stdout`), which would otherwise
		// mistype `C.stdout` and trigger a bogus auto-reference (`&stdout`).
		if is_c_int_selector(node.value) {
			return 'int'
		}
		cname := 'C.${node.value}'
		if gt := t.globals[cname] {
			if gt.len > 0 {
				return t.normalize_type_alias(gt)
			}
		}
		if typ := t.const_type_name(cname) {
			return typ
		}
		return ''
	}
	field_name := node.value
	if field_name.len == 0 {
		return ''
	}
	base_selector_name := t.selector_expr_name(base_id)
	if base_selector_name.len > 0 {
		if enum_type := t.enum_type_name_from_selector_name(base_selector_name) {
			if fields := t.enum_types[enum_type] {
				if field_name in fields {
					return enum_type
				}
			}
		}
		if type_name := t.qualified_enum_type_selector_name_from_selector_name(base_selector_name,
			field_name)
		{
			return type_name
		}
	}
	if base_node.kind == .ident {
		if typ := t.const_type_name('${base_node.value}.${field_name}') {
			return typ
		}
		if fields := t.enum_types[base_node.value] {
			if field_name in fields {
				return base_node.value
			}
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qenum := '${t.cur_module}.${base_node.value}'
			if fields := t.enum_types[qenum] {
				if field_name in fields {
					return qenum
				}
			}
		}
	}
	base_type := t.resolve_expr_type(base_id)
	if base_type.len == 0 {
		if ftyp := t.lookup_unique_field_type(field_name) {
			return ftyp
		}
		return ''
	}
	base_key := t.expr_key(base_id)
	if base_key.len > 0 {
		full_key := '${base_key}.${field_name}'
		if sc := t.find_smartcast(full_key) {
			return t.smartcast_target_type(sc)
		}
	}
	if base_key.len > 0 {
		if sc := t.find_smartcast(base_key) {
			variant_type := t.qualify_variant(sc.variant_name, sc.sum_type_name)
			if ftyp := t.lookup_struct_field_type(variant_type, field_name) {
				return ftyp
			}
			if ftyp := t.lookup_struct_field_type(sc.variant_name, field_name) {
				return ftyp
			}
		}
	}
	// Strip pointer prefix if present (e.g. "&MyStruct" -> "MyStruct")
	lookup_type := if base_type.starts_with('&') { base_type[1..] } else { base_type }
	if ftyp := t.lookup_sum_variant_field_type(lookup_type, field_name) {
		return ftyp
	}
	if ftyp := t.lookup_struct_field_type(lookup_type, field_name) {
		return ftyp
	}
	if info := t.lookup_struct_info(lookup_type) {
		if embedded := t.embedded_field_for_promoted_field(info, field_name) {
			if embedded_info := t.lookup_struct_info(embedded.typ) {
				if ftyp := t.struct_field_type(embedded_info, field_name) {
					return ftyp
				}
			}
		}
	}
	if ftyp := t.lookup_unique_field_type(field_name) {
		return ftyp
	}
	return ''
}

fn (t &Transformer) lookup_sum_variant_field_type(sum_type string, field_name string) ?string {
	return t.lookup_sum_variant_field_type_seen(sum_type, field_name, []string{})
}

fn (t &Transformer) lookup_sum_variant_field_type_seen(sum_type string, field_name string, seen []string) ?string {
	resolved := t.resolve_sum_name(t.trim_pointer_type(t.normalize_type_alias(sum_type)))
	if resolved.len == 0 || resolved in seen {
		return none
	}
	variants := t.sum_types[resolved] or { return none }
	mut next_seen := seen.clone()
	next_seen << resolved
	mut found := ''
	for variant in variants {
		mut ftyp := ''
		if vt := t.lookup_struct_field_type(variant, field_name) {
			ftyp = t.normalize_type_alias(vt)
		} else if vt := t.checker_struct_field_type_name(variant, field_name) {
			ftyp = t.normalize_type_alias(vt)
		} else if nested := t.lookup_sum_variant_field_type_seen(variant, field_name, next_seen) {
			ftyp = t.normalize_type_alias(nested)
		}
		if ftyp.len == 0 {
			continue
		}
		if found.len > 0 && found != ftyp {
			return none
		}
		found = ftyp
	}
	if found.len == 0 {
		return none
	}
	return found
}

// is_c_int_selector reports whether is c int selector applies in transform.
fn is_c_int_selector(name string) bool {
	return name in ['errno', 'EINTR', 'STDOUT_FILENO', 'STDERR_FILENO', 'EINVAL']
}

// selector_expr_name supports selector expr name handling for Transformer.
fn (t &Transformer) selector_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count > 0 {
		base := t.selector_expr_name(t.a.child(&node, 0))
		if base.len > 0 && node.value.len > 0 {
			return '${base}.${node.value}'
		}
	}
	return ''
}

// enum_type_name_from_expr converts enum type name from expr data for transform.
fn (t &Transformer) enum_type_name_from_expr(id flat.NodeId) ?string {
	name := t.selector_expr_name(id)
	return t.enum_type_name_from_selector_name(name)
}

fn (t &Transformer) enum_type_name_from_selector_name(name string) ?string {
	if name.len == 0 {
		return none
	}
	if name in t.enum_types {
		return name
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.enum_types {
			return qname
		}
	}
	return none
}

// qualified_enum_type_selector_name
// supports helper handling in transform.
fn (t &Transformer) qualified_enum_type_selector_name(base_id flat.NodeId, field_name string) ?string {
	base := t.selector_expr_name(base_id)
	return t.qualified_enum_type_selector_name_from_selector_name(base, field_name)
}

fn (t &Transformer) qualified_enum_type_selector_name_from_selector_name(base string, field_name string) ?string {
	if base.len == 0 || field_name.len == 0 {
		return none
	}
	name := '${base}.${field_name}'
	if name in t.enum_types {
		return name
	}
	if !name.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.enum_types {
			return qname
		}
	}
	return none
}

// lookup_struct_field_type resolves lookup struct field type information for transform.
fn (t &Transformer) lookup_struct_field_type(type_name string, field_name string) ?string {
	if type_name.len == 0 || field_name.len == 0 {
		return none
	}
	key := t.struct_field_type_cache_key(type_name, field_name)
	if !isnil(t.struct_field_type_cache) {
		mut cache := t.struct_field_type_cache
		if cached := cache.entries[key] {
			return cached
		}
		if cache.misses[key] {
			return none
		}
	}
	resolved := t.lookup_struct_field_type_uncached(type_name, field_name) or {
		if !isnil(t.struct_field_type_cache) {
			mut cache := t.struct_field_type_cache
			cache.misses[key] = true
		}
		return none
	}
	if !isnil(t.struct_field_type_cache) {
		mut cache := t.struct_field_type_cache
		cache.entries[key] = resolved
	}
	return resolved
}

fn (t &Transformer) struct_field_type_cache_key(type_name string, field_name string) string {
	return '${t.cur_module}\n${type_name}\n${field_name}'
}

fn (t &Transformer) lookup_struct_field_type_uncached(type_name string, field_name string) ?string {
	lookup := t.lookup_struct_info_for_field(type_name, field_name) or { return none }
	for f in lookup.info.fields {
		if f.name == field_name {
			if checker_typ := t.checker_struct_field_type_name(lookup.owner_type, field_name) {
				if field_type_needs_checker_authority(f.typ) {
					return checker_typ
				}
			}
			if !lookup.owner_type.contains('[') {
				return f.typ
			}
			return t.normalize_field_type(f.typ, lookup.owner_type)
		}
	}
	return none
}

// lookup_struct_field_raw_type resolves lookup struct field raw type information for transform.
fn (t &Transformer) lookup_struct_field_raw_type(type_name string, field_name string) ?string {
	lookup := t.lookup_struct_info_for_field(type_name, field_name) or { return none }
	for f in lookup.info.fields {
		if f.name == field_name {
			if f.raw_typ.len > 0 {
				return f.raw_typ
			}
			return f.typ
		}
	}
	return none
}

fn (t &Transformer) lookup_struct_field_raw_type_with_owner(type_name string, field_name string) ?(string, string) {
	lookup := t.lookup_struct_info_for_field(type_name, field_name) or { return none }
	for f in lookup.info.fields {
		if f.name == field_name {
			raw := if f.raw_typ.len > 0 { f.raw_typ } else { f.typ }
			owner_type := if lookup.owner_type.contains('.') || lookup.info.module.len == 0
				|| lookup.info.module == 'main' || lookup.info.module == 'builtin' {
				lookup.owner_type
			} else {
				'${lookup.info.module}.${lookup.owner_type}'
			}
			return raw, owner_type
		}
	}
	return none
}

fn field_type_needs_checker_authority(typ string) bool {
	return typ in ['Option', 'Result', 'Optional'] || typ.starts_with('Option_')
		|| typ.starts_with('Result_') || typ.starts_with('Optional_')
}

fn (t &Transformer) checker_struct_field_type_name(type_name string, field_name string) ?string {
	if isnil(t.tc) {
		return none
	}
	mut lookup_type := if type_name.starts_with('&') { type_name[1..] } else { type_name }
	if checker_typ := t.tc.struct_field_type_name(lookup_type, field_name) {
		return t.normalize_type_alias(checker_typ)
	}
	if lookup_type.contains('.') {
		short := lookup_type.all_after_last('.')
		if checker_typ := t.tc.struct_field_type_name(short, field_name) {
			return t.normalize_type_alias(checker_typ)
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qtype := '${t.cur_module}.${lookup_type}'
		if checker_typ := t.tc.struct_field_type_name(qtype, field_name) {
			return t.normalize_type_alias(checker_typ)
		}
	}
	normalized := t.normalize_type_alias(lookup_type)
	if normalized != lookup_type {
		if checker_typ := t.tc.struct_field_type_name(normalized, field_name) {
			return t.normalize_type_alias(checker_typ)
		}
	}
	return none
}

// lookup_struct_info_for_field resolves lookup struct info for field information for transform.
fn (t &Transformer) lookup_struct_info_for_field(type_name string, field_name string) ?StructFieldLookup {
	if type_name.len == 0 || field_name.len == 0 {
		return none
	}
	mut lookup_type := if type_name.starts_with('&') { type_name[1..] } else { type_name }
	owner_type := lookup_type
	base, _, is_generic_app := generic_app_parts(lookup_type)
	if is_generic_app {
		lookup_type = base
	}
	if lookup_type !in t.structs && lookup_type.contains('.') {
		short_type := lookup_type.all_after_last('.')
		if short_type in t.structs {
			lookup_type = short_type
		}
	}
	if !lookup_type.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtype := '${t.cur_module}.${lookup_type}'
		if qtype in t.structs {
			lookup_type = qtype
		} else if !isnil(t.tc) {
			if target := t.tc.type_aliases[qtype] {
				lookup_type = target
			}
		}
	}
	if lookup_type !in t.structs {
		normalized := t.normalize_type_alias(lookup_type)
		if normalized != lookup_type {
			lookup_type = normalized
		}
	}
	info := t.structs[lookup_type] or { return none }
	return StructFieldLookup{
		info:       info
		owner_type: owner_type
	}
}

// lookup_unique_field_type resolves lookup unique field type information for transform.
fn (t &Transformer) lookup_unique_field_type(field_name string) ?string {
	match field_name {
		'name' {
			return 'string'
		}
		'generic_types' {
			return '[]u32'
		}
		'obj' {
			return t.resolve_sum_name('ScopeObject')
		}
		else {}
	}

	if ftyp := t.unique_fields[field_name] {
		if ftyp.len > 0 {
			return ftyp
		}
	}
	return none
}

// normalize_field_type transforms normalize field type data for transform.
fn (t &Transformer) normalize_field_type(typ string, owner_type string) string {
	return t.normalize_field_type_with_owner_substitution(typ, owner_type, true)
}

fn (t &Transformer) normalize_field_type_with_owner_substitution(typ string, owner_type string, allow_owner_substitution bool) string {
	if typ.len == 0 {
		return typ
	}
	if typ.starts_with('mut ') {
		return 'mut ' +
			t.normalize_field_type_with_owner_substitution(typ[4..], owner_type, allow_owner_substitution)
	}
	if typ.starts_with('shared ') {
		return 'shared ' +
			t.normalize_field_type_with_owner_substitution(typ[7..], owner_type, allow_owner_substitution)
	}
	if typ.starts_with('atomic ') {
		return 'atomic ' +
			t.normalize_field_type_with_owner_substitution(typ[7..], owner_type, allow_owner_substitution)
	}
	if typ.starts_with('&') {
		return '&' +
			t.normalize_field_type_with_owner_substitution(typ[1..], owner_type, allow_owner_substitution)
	}
	if typ.starts_with('[]') {
		return '[]' +
			t.normalize_field_type_with_owner_substitution(typ[2..], owner_type, allow_owner_substitution)
	}
	if typ.starts_with('?') {
		return '?' +
			t.normalize_field_type_with_owner_substitution(typ[1..], owner_type, allow_owner_substitution)
	}
	if typ.starts_with('!') {
		return '!' +
			t.normalize_field_type_with_owner_substitution(typ[1..], owner_type, allow_owner_substitution)
	}
	if typ.starts_with('map[') {
		bracket_end := typ.index(']') or { return t.normalize_type_alias(typ) }
		key_type := t.normalize_field_type_with_owner_substitution(typ[4..bracket_end], owner_type,
			allow_owner_substitution)
		value_type := t.normalize_field_type_with_owner_substitution(typ[bracket_end + 1..],
			owner_type, allow_owner_substitution)
		return 'map[${key_type}]${value_type}'
	}
	if typ.starts_with('[') {
		bracket_end := typ.index(']') or { return t.normalize_type_alias(typ) }
		return typ[..bracket_end + 1] +
			t.normalize_field_type_with_owner_substitution(typ[bracket_end +
			1..], owner_type, allow_owner_substitution)
	}
	owner_base, owner_args, owner_is_generic_app := generic_app_parts(owner_type)
	if allow_owner_substitution && owner_is_generic_app {
		if owner_base.len > 0 {
			params := t.generic_struct_param_names_for_base(owner_base)
			substituted := if params.len > 0 {
				substitute_generic_type_text_with_params(typ, owner_args, params)
			} else {
				substitute_generic_type_text(typ, owner_args)
			}
			if substituted != typ {
				return t.normalize_field_type_with_owner_substitution(substituted, owner_type,
					false)
			}
		}
	}
	base, args, type_is_generic_app := generic_app_parts(typ)
	if type_is_generic_app {
		mut field_base := base
		if !field_base.contains('.') && owner_type.contains('.') {
			owner_mod := field_owner_module(owner_type)
			qbase := '${owner_mod}.${field_base}'
			if t.type_authority_has(qbase) {
				field_base = qbase
			}
		}
		mut normalized_args := []string{cap: args.len}
		for arg in args {
			mut normalized_arg := t.normalize_field_type_with_owner_substitution(arg, owner_type,
				allow_owner_substitution)
			if field_base.contains('.') {
				field_mod := field_base.all_before_last('.')
				normalized_arg = strip_field_module_prefix_from_type(normalized_arg, field_mod)
			}
			normalized_args << normalized_arg
		}
		return t.normalize_type_alias('${field_base}[${normalized_args.join(', ')}]')
	}
	if typ.contains('.') || !owner_type.contains('.') {
		return t.normalize_type_alias(typ)
	}
	owner_mod := field_owner_module(owner_type)
	qtyp := '${owner_mod}.${typ}'
	if t.type_authority_has(qtyp) {
		return t.normalize_type_alias(qtyp)
	}
	if !isnil(t.tc) {
		if qtyp in t.tc.type_aliases {
			return t.normalize_type_alias(qtyp)
		}
	}
	return t.normalize_type_alias(typ)
}

fn field_owner_module(owner_type string) string {
	for foreign_prefix in ['.C.', '.JS.'] {
		if idx := owner_type.index(foreign_prefix) {
			return owner_type[..idx]
		}
	}
	return owner_type.all_before_last('.')
}

fn (t &Transformer) generic_struct_param_names_for_base(base string) []string {
	if isnil(t.tc) {
		return []string{}
	}
	if params := t.tc.struct_generic_params[base] {
		return params.clone()
	}
	short := base.all_after_last('.')
	if params := t.tc.struct_generic_params[short] {
		return params.clone()
	}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		if params := t.tc.struct_generic_params['${t.cur_module}.${base}'] {
			return params.clone()
		}
	}
	return []string{}
}

fn (t &Transformer) type_authority_has(name string) bool {
	if name in t.structs || name in t.sum_types || name in t.enum_types {
		return true
	}
	if isnil(t.tc) {
		return false
	}
	return name in t.tc.structs || name in t.tc.sum_types || name in t.tc.enum_names
		|| name in t.tc.interface_names || name in t.tc.type_aliases
}

// normalize_type_alias transforms normalize type alias data for transform.
fn (t &Transformer) normalize_type_alias(typ string) string {
	if typ.len == 0 || isnil(t.tc) {
		return typ
	}
	if is_plain_builtin_alias_type(typ) {
		return typ
	}
	if isnil(t.alias_cache) {
		return t.normalize_type_alias_uncached(typ)
	}
	mut c := t.alias_cache
	if c.module != t.cur_module || c.file != t.cur_file {
		c.module = t.cur_module
		c.file = t.cur_file
		c.entries.clear()
	}
	if cached := c.entries[typ] {
		return cached
	}
	result := t.normalize_type_alias_uncached(typ)
	c.entries[typ] = result
	return result
}

// normalize_type_alias_uncached converts normalize type alias uncached data for transform.
fn (t &Transformer) normalize_type_alias_uncached(typ string) string {
	if is_generic_placeholder_type_name(typ) && !t.is_known_type_name(typ) {
		return typ
	}
	if typ.starts_with('mut ') {
		return '&' + t.normalize_type_alias(typ[4..])
	}
	if typ.starts_with('&void[') && typ.ends_with(']') {
		return 'voidptr' + typ['&void'.len..]
	}
	if typ.starts_with('&') {
		return '&' + t.normalize_type_alias(typ[1..])
	}
	if typ.starts_with('[]') {
		return '[]' + t.normalize_type_alias(typ[2..])
	}
	if typ.starts_with('chan ') {
		return 'chan ' + t.normalize_type_alias(typ[5..])
	}
	if typ.starts_with('[') {
		bracket_end := typ.index(']') or { return typ }
		return typ[..bracket_end + 1] + t.normalize_type_alias(typ[bracket_end + 1..])
	}
	if typ.starts_with('map[') {
		bracket_end := generic_matching_bracket(typ, 3)
		if bracket_end < typ.len {
			key := t.normalize_type_alias(typ[4..bracket_end])
			value := t.normalize_type_alias(typ[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if typ.starts_with('?') {
		return '?' + t.normalize_type_alias(typ[1..])
	}
	if typ.starts_with('!') {
		return '!' + t.normalize_type_alias(typ[1..])
	}
	if expanded := t.expand_generic_type_alias(typ) {
		return t.normalize_type_alias(expanded)
	}
	// A fully qualified type is already canonical. Resolve its exact alias before
	// consulting imports from the current file; otherwise an unrelated import with
	// the same trailing type name can steal the identity (for example
	// `v3.ssa.TypeID` being resolved as `v3.types.Type`).
	if typ.contains('.') {
		if target := t.tc.type_aliases[typ] {
			return t.normalize_type_alias(target)
		}
		if t.type_authority_has(typ) {
			return typ
		}
	}
	// Resolve the importing file's alias before any short-name or suffix fallback.
	// Two packages can both expose `tast.Value`; `other_tast.Value` and
	// `tast.Value` must retain their exact canonical module identities.
	if imported := t.resolve_imported_type_name(typ) {
		if target := t.tc.type_aliases[imported] {
			return t.normalize_type_alias(target)
		}
		if t.type_authority_has(imported) {
			return imported
		}
	}
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := '${t.cur_module}.${typ}'
		if target := t.tc.type_aliases[qtyp] {
			return target
		}
		if t.type_authority_has(qtyp) {
			return qtyp
		}
	}
	// Imported aliases are indexed by their short spelling. A main-module struct
	// with the same name remains authoritative for an unqualified reference.
	if t.bare_struct_name_is_local_to_current_module(typ) {
		return typ
	}
	if target := t.tc.type_aliases[typ] {
		return target
	}
	if typ in t.structs || typ in t.sum_types || typ in t.enum_types {
		return typ
	}
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := '${t.cur_module}.${typ}'
		if target := t.tc.type_aliases[qtyp] {
			return target
		}
	}
	if !typ.contains('.') {
		mut unique_target := ''
		for name, target in t.tc.type_aliases {
			if name == typ || name.ends_with('.${typ}') {
				if unique_target.len > 0 && unique_target != target {
					return typ
				}
				unique_target = target
			}
		}
		if unique_target.len > 0 {
			return unique_target
		}
	}
	return typ
}

fn (t &Transformer) expand_generic_type_alias(typ string) ?string {
	base, args, ok := generic_app_parts(typ)
	if !ok || args.len == 0 {
		return none
	}
	mut candidates := [base]
	if imported := t.resolve_imported_type_name(base) {
		if imported !in candidates {
			candidates << imported
		}
	}
	if !base.contains('.') && t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qualified := '${t.cur_module}.${base}'
		if qualified !in candidates {
			candidates << qualified
		}
	}
	mut has_alias := false
	for candidate in candidates {
		if candidate in t.tc.type_aliases {
			has_alias = true
			break
		}
	}
	if !has_alias {
		return none
	}
	for i, node in t.a.nodes {
		if node.kind != .type_decl || node.children_count > 0 {
			continue
		}
		params := node.generic_params()
		if params.len == 0 || params.len != args.len {
			continue
		}
		module_name := t.node_module_or(i, '')
		qualified_name := if module_name.len > 0 && module_name !in ['main', 'builtin'] {
			'${module_name}.${node.value}'
		} else {
			node.value
		}
		if qualified_name !in candidates && node.value !in candidates {
			continue
		}
		return substitute_generic_type_text_with_params(node.typ, args, params)
	}
	return none
}

fn strip_field_module_prefix_from_type(typ string, module_name string) string {
	clean := typ.trim_space()
	if clean.len == 0 || module_name.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + strip_field_module_prefix_from_type(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + strip_field_module_prefix_from_type(clean[4..], module_name)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return clean[..1] + strip_field_module_prefix_from_type(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + strip_field_module_prefix_from_type(clean[3..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + strip_field_module_prefix_from_type(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := strip_field_module_prefix_from_type(clean[4..bracket_end], module_name)
			value := strip_field_module_prefix_from_type(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${value}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] +
				strip_field_module_prefix_from_type(clean[bracket_end + 1..], module_name)
		}
	}
	base, args, ok := generic_app_parts(clean)
	if ok {
		stripped_base := strip_field_module_prefix_from_base(base, module_name)
		mut stripped_args := []string{cap: args.len}
		for arg in args {
			stripped_args << strip_field_module_prefix_from_type(arg, module_name)
		}
		return '${stripped_base}[${stripped_args.join(', ')}]'
	}
	return strip_field_module_prefix_from_base(clean, module_name)
}

fn strip_field_module_prefix_from_base(name string, module_name string) string {
	prefix := '${module_name}.'
	if !name.starts_with(prefix) {
		return name
	}
	short := name[prefix.len..]
	if short.starts_with('Array_') {
		return name
	}
	return short
}

fn (t &Transformer) is_known_type_name(typ string) bool {
	if typ in t.structs || typ in t.sum_types || typ in t.enum_types {
		return true
	}
	if !isnil(t.tc) {
		if typ in t.tc.type_aliases || typ in t.tc.structs || typ in t.tc.sum_types
			|| typ in t.tc.enum_names || typ in t.tc.interface_names {
			return true
		}
	}
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := '${t.cur_module}.${typ}'
		if qtyp in t.structs || qtyp in t.sum_types || qtyp in t.enum_types {
			return true
		}
		if !isnil(t.tc) {
			return qtyp in t.tc.type_aliases || qtyp in t.tc.structs || qtyp in t.tc.sum_types
				|| qtyp in t.tc.enum_names || qtyp in t.tc.interface_names
		}
	}
	return false
}

// is_plain_builtin_alias_type reports whether is plain builtin alias type applies in transform.
fn is_plain_builtin_alias_type(typ string) bool {
	return match typ {
		'bool', 'string', 'void', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64',
		'f32', 'f64', 'rune', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr' {
			true
		}
		else {
			false
		}
	}
}

// is_generic_placeholder_type_name reports is_generic_placeholder_type_name logic in transform.
fn is_generic_placeholder_type_name(typ string) bool {
	if typ.contains('.') {
		return is_generic_placeholder_type_name(typ.all_after_last('.'))
	}
	return typ in ['T', 'U', 'V', 'K']
}

// normalize_type_in_module transforms normalize type in module data for transform.
fn (t &Transformer) normalize_type_in_module(typ string, mod string) string {
	clean := typ.trim_space()
	if clean.len == 0 {
		return clean
	}
	if clean.starts_with('&') {
		return '&' + t.normalize_type_in_module(clean[1..], mod)
	}
	if clean.starts_with('mut ') {
		return '&' + t.normalize_type_in_module(clean[4..], mod)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + t.normalize_type_in_module(clean[7..], mod)
	}
	if clean.starts_with('atomic ') {
		return 'atomic ' + t.normalize_type_in_module(clean[7..], mod)
	}
	if clean.starts_with('?') {
		return '?' + t.normalize_type_in_module(clean[1..], mod)
	}
	if clean.starts_with('!') {
		return '!' + t.normalize_type_in_module(clean[1..], mod)
	}
	if clean.starts_with('...') {
		return '...' + t.normalize_type_in_module(clean[3..], mod)
	}
	if clean.starts_with('[]') {
		return '[]' + t.normalize_type_in_module(clean[2..], mod)
	}
	if clean.starts_with('chan ') {
		return 'chan ' + t.normalize_type_in_module(clean[5..], mod)
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		params, ret := fn_type_text_parts(clean) or { return clean }
		mut normalized_params := []string{cap: params.len}
		for param in params {
			raw_param := param.trim_space()
			payload := generic_fn_type_param_payload(raw_param)
			param_type := if raw_param.starts_with('mut ') { 'mut ${payload}' } else { payload }
			normalized_params << t.normalize_type_in_module(param_type, mod)
		}
		normalized_ret := t.normalize_type_in_module(ret, mod)
		return if normalized_ret.len > 0 {
			'fn (${normalized_params.join(', ')}) ${normalized_ret}'
		} else {
			'fn (${normalized_params.join(', ')})'
		}
	}
	if clean.contains('.') || mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return t.normalize_type_alias(clean)
	}
	qtyp := '${mod}.${clean}'
	if t.type_authority_has(qtyp) {
		return t.normalize_type_alias(qtyp)
	}
	if !isnil(t.tc) {
		if qtyp in t.tc.type_aliases {
			return t.normalize_type_alias(qtyp)
		}
	}
	return t.normalize_type_alias(clean)
}

// resolve_index_elem_type determines the element type of an .index expression.
// For `[]T` arrays returns `T`, for `map[K]V` returns `V`, for `string` returns `u8`.
fn (t &Transformer) resolve_index_elem_type(node flat.Node) string {
	if node.kind != .index || node.children_count == 0 {
		return ''
	}
	base_id := t.a.child(&node, 0)
	mut base_type := t.resolve_expr_type(base_id)
	if base_type.len == 0 {
		return ''
	}
	base_type = t.normalize_type_alias(base_type)
	if base_type.starts_with('&') {
		ptr_elem_type := t.normalize_type_alias(base_type[1..])
		// A `mut map`/`mut []T` param is a pointer to the container; indexing it must
		// still yield the element/value type, so fall through to the container cases.
		if !ptr_elem_type.starts_with('[]') && !ptr_elem_type.starts_with('map[')
			&& !t.is_fixed_array_type(ptr_elem_type) {
			return ptr_elem_type
		}
		base_type = ptr_elem_type
	}
	is_slice := node.value == 'range'
		|| (node.children_count > 1 && t.a.child_node(&node, 1).kind == .range)
	if is_slice {
		if base_type == 'string' {
			return 'string'
		}
		if base_type.starts_with('[]') {
			return base_type
		}
		// A range/slice of a fixed array (`arr[..]`, `arr[a..b]`) yields a dynamic
		// `[]T`, not the fixed array or a bogus `range` type.
		if t.is_fixed_array_type(base_type) {
			return '[]${t.normalize_type_alias(for_in_fixed_array_elem_type(base_type))}'
		}
	}
	if base_type.starts_with('[]') {
		return t.normalize_type_alias(base_type[2..])
	}
	if t.is_fixed_array_type(base_type) {
		return t.normalize_type_alias(for_in_fixed_array_elem_type(base_type))
	}
	if base_type.starts_with('map[') {
		bracket_end := base_type.index(']') or { return '' }
		if bracket_end + 1 < base_type.len {
			return t.normalize_type_alias(base_type[bracket_end + 1..])
		}
		return ''
	}
	if base_type == 'string' {
		return 'u8'
	}
	return ''
}

fn (t &Transformer) index_expr_type(id flat.NodeId, node flat.Node) string {
	resolved_elem_type := t.resolve_index_elem_type(node)
	if resolved_elem_type == 'u8' {
		return resolved_elem_type
	}
	if t.is_fixed_array_type(resolved_elem_type)
		&& t.fixed_array_type_contains_map(resolved_elem_type) {
		return resolved_elem_type
	}
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 && name != 'unknown' {
				return t.normalize_type_alias(name)
			}
		}
	}
	return resolved_elem_type
}

// node_type returns the v-type string for an expression node, preferring the
// precise type recorded by the type checker (tc.expr_types, populated before
// transform) and falling back to the transformer's local heuristics. This is the
// bridge that makes the transformer type-aware, so type-dependent lowering lives
// here rather than in the backend.
fn (t &Transformer) node_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .fn_literal || node.kind == .lambda_expr {
		if fn_type := t.fn_value_type_name(id) {
			return fn_type
		}
	}
	resolved := t.resolve_expr_type(id)
	if resolved.len > 0 {
		if checked := t.checker_type_over_struct_guess(id, resolved) {
			return checked
		}
		if resolved.contains('typeof(') && !isnil(t.tc) {
			if typ := t.tc.expr_type(id) {
				name := t.normalize_type_alias(typ.name())
				if decl_type_is_usable(name) {
					return name
				}
			}
		}
		if t.generic_arg_is_unresolved(resolved) && node.typ.len > 0 {
			node_typ := t.normalize_type_alias(node.typ)
			if node_typ.len > 0 && !t.generic_arg_is_unresolved(node_typ) {
				return node_typ
			}
		}
		return resolved
	}
	if node.kind == .dump_expr && node.children_count > 0 {
		return t.node_type(t.a.child(&node, 0))
	}
	if node.kind == .prefix && node.op == .amp && node.children_count > 0 {
		if checker_type := t.checker_expr_type_name(id) {
			return checker_type
		}
		if !isnil(t.tc) {
			checker_resolved := t.tc.resolve_type(id).name()
			if decl_type_is_usable(checker_resolved) && checker_resolved != 'void' {
				return t.normalize_type_alias(checker_resolved)
			}
		}
		child_type := t.node_type(t.a.child(&node, 0))
		if child_type.len > 0 {
			if t.is_optional_type_name(child_type) {
				return '${child_type[..1]}&${t.optional_base_type(child_type)}'
			}
			return '&${child_type}'
		}
	}
	mut deferred_call_typ := ''
	if node.typ.len > 0 {
		node_typ := t.normalize_type_alias(node.typ)
		if node.kind == .call && node_typ in ['int', 'array', 'map', 'unknown'] {
			deferred_call_typ = node_typ
		} else {
			return node_typ
		}
	}
	if node.kind == .selector {
		sel_type := t.resolve_selector_type(node)
		if sel_type.len > 0 {
			return sel_type
		}
	}
	if node.kind == .index {
		elem_type := t.index_expr_type(id, node)
		if elem_type.len > 0 {
			return elem_type
		}
	}
	if node.kind == .call {
		if array_type := t.array_call_type_name(node) {
			return array_type
		}
	}
	// NOTE: infix is intentionally not handled here — resolve_expr_type() (called at the top
	// of node_type) already resolves infix types, including struct operator overloads.
	if node.kind == .struct_init && node.value.len > 0 {
		for candidate in [node.typ, node.value] {
			if candidate.len == 0 {
				continue
			}
			normalized := t.normalize_type_alias(candidate)
			if normalized != candidate || normalized.contains('[') {
				return normalized
			}
		}
		if node.value in t.structs {
			return node.value
		}
		if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
			qname := '${t.cur_module}.${node.value}'
			if qname in t.structs {
				return qname
			}
		}
		return node.value
	}
	if !isnil(t.tc) {
		mut name := ''
		if typ := t.tc.expr_type(id) {
			name = typ.name()
		}
		if name.contains('typeof(') {
			name = t.tc.resolve_type(id).name()
		}
		if name.len == 0 || name == 'unknown' {
			name = t.tc.resolve_type(id).name()
		}
		if name.len > 0 && name != 'void' && (name != 'int'
			|| node.kind in [.ident, .int_literal, .infix, .prefix, .paren, .selector, .index, .call]) {
			return t.normalize_type_alias(name)
		}
	}
	if deferred_call_typ.len > 0 {
		return deferred_call_typ
	}
	return ''
}

fn (t &Transformer) checker_type_over_struct_guess(id flat.NodeId, guessed string) ?string {
	if isnil(t.tc) || int(id) < 0 || guessed.len == 0 {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		local_type := t.normalize_type_alias(t.var_type(node.value))
		if local_type.len > 0 && local_type == t.normalize_type_alias(guessed) {
			// Function-local bindings are authoritative after lowering. The checker
			// cache still describes the source node and can retain a mutable
			// parameter's pointer storage type after that node is reused for a value
			// local with the same expression shape.
			return none
		}
	}
	guessed_type := t.tc.parse_type(guessed)
	guessed_base := types.unwrap_all_pointers(guessed_type)
	guessed_is_struct := guessed_base is types.Struct
		|| (guessed_base is types.Alias && guessed_base.base_type is types.Struct)
	if !guessed_is_struct {
		return none
	}
	checked_type := t.tc.expr_type(id) or { return none }
	if checked_type is types.Unknown || checked_type is types.Void {
		return none
	}
	if t.tc.c_type(guessed_type) == t.tc.c_type(checked_type) {
		return none
	}
	checked := t.normalize_type_alias(checked_type.name())
	if !decl_type_is_usable(checked) {
		return none
	}
	return checked
}

fn (t &Transformer) array_call_type_name(node flat.Node) ?string {
	if map_type := t.array_map_call_type_name(node) {
		return map_type
	}
	if node.children_count == 0 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.children_count == 0 {
		return none
	}
	base_type0 := t.node_type(t.a.child(fn_node, 0))
	base_type := if base_type0.starts_with('&') { base_type0[1..] } else { base_type0 }
	if !base_type.starts_with('[]') {
		return none
	}
	elem_type := base_type[2..]
	if elem_type.len == 0 {
		return none
	}
	match fn_node.value {
		'filter', 'clone', 'reverse', 'sorted', 'repeat', 'repeat_to_depth' {
			return base_type
		}
		'first', 'last', 'pop', 'pop_left' {
			return elem_type
		}
		else {
			return none
		}
	}
}

fn (t &Transformer) array_map_call_type_name(node flat.Node) ?string {
	if node.children_count < 2 {
		return none
	}
	fn_node := t.a.child_node(&node, 0)
	if fn_node.kind != .selector || fn_node.value != 'map' || fn_node.children_count == 0 {
		return none
	}
	base_type0 := t.node_type(t.a.child(fn_node, 0))
	base_type := if base_type0.starts_with('&') { base_type0[1..] } else { base_type0 }
	if !base_type.starts_with('[]') {
		return none
	}
	map_expr_id := t.a.child(&node, 1)
	mut elem_type := if callback_ret := t.array_map_callback_return_type_name(map_expr_id) {
		callback_ret
	} else if checker_type := t.checker_expr_type_name(map_expr_id) {
		checker_type
	} else {
		t.node_type(map_expr_id)
	}
	if elem_type.len == 0 || elem_type in ['array', 'map', 'unknown'] {
		elem_type = t.reliable_stringify_type(map_expr_id)
	}
	if elem_type.len == 0 || elem_type in ['array', 'map', 'unknown', 'void'] {
		return none
	}
	return '[]${elem_type}'
}

fn (t &Transformer) array_map_callback_return_type_name(map_expr_id flat.NodeId) ?string {
	if int(map_expr_id) < 0 {
		return none
	}
	map_expr := t.a.nodes[int(map_expr_id)]
	if map_expr.kind == .ident {
		if fn_name := t.resolve_fn_value_ident(map_expr.value) {
			if ret := t.fn_ret_types[fn_name] {
				return t.normalize_type_alias(ret)
			}
			if !isnil(t.tc) {
				if ret_type := t.tc.fn_ret_types[fn_name] {
					return t.normalize_type_alias(ret_type.name())
				}
			}
		} else if ret_type := t.fn_value_return_type_name(map_expr_id) {
			return ret_type
		}
	} else if map_expr.kind == .fn_literal || map_expr.kind == .lambda_expr {
		if ret_type := t.fn_value_return_type_name(map_expr_id) {
			return ret_type
		}
	}
	return none
}

// lvalue_type returns the v-type string for an assignable expression, handling
// idents/calls (via node_type), selectors (struct field types) and index
// expressions (array/map element types). Used by array-append lowering so that
// `obj.field << x` and `m[k] << x` are typed, not just plain `arr << x`.
fn (t &Transformer) lvalue_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector {
		sel_type := t.resolve_selector_type(node)
		if !isnil(t.tc) {
			if checker_type := t.tc.expr_type(id) {
				checker_name := checker_type.name()
				if checker_name.starts_with('&') && !sel_type.starts_with('&') {
					return checker_name
				}
			}
		}
		if sel_type.len > 0 {
			return sel_type
		}
	}
	if node.kind == .index {
		elem_type := t.resolve_index_elem_type(node)
		if elem_type.len > 0 {
			return elem_type
		}
	}
	nt := t.node_type(id)
	if nt.len > 0 {
		return nt
	}
	return ''
}

// is_string_type checks if an expression resolves to string type.
// Handles string literals, interpolations, and ident/call expressions typed as string.
fn (t &Transformer) is_string_type(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := t.a.nodes[int(id)]
	if node.kind == .string_literal || node.kind == .string_interp {
		return true
	}
	if node.kind == .ident {
		raw_var_type := t.raw_var_type(node.value)
		if raw_var_type.len > 0 {
			return t.normalize_type_alias(raw_var_type) == 'string'
		}
	}
	if node.kind == .ident && !isnil(t.tc) {
		if key := t.const_type_key_in_context(node.value, t.cur_module, t.cur_file) {
			if expr_id := t.tc.const_exprs[key] {
				const_expr := t.a.nodes[int(expr_id)]
				if const_expr.kind == .char_literal {
					return false
				}
				if const_expr.kind == .string_literal || const_expr.kind == .string_interp {
					return true
				}
			}
		}
	}
	if raw_const_type := t.raw_const_type_name_for_expr(id) {
		return t.normalize_type_alias(raw_const_type) == 'string'
	}
	mut typ := ''
	if sc := t.find_smartcast(t.expr_key(id)) {
		typ = t.smartcast_target_type(sc)
	}
	if typ.len == 0 {
		typ = t.node_type(id)
	}
	if typ.len == 0 {
		typ = t.raw_checker_node_type(id)
	}
	return t.normalize_type_alias(typ) == 'string'
}

// is_array_type checks if a type string represents an array type (starts with `[]`).
fn (t &Transformer) is_array_type(type_str string) bool {
	return type_str.starts_with('[]')
}

// is_map_type checks if a type string represents a map type (starts with `map[`).
fn (t &Transformer) is_map_type(type_str string) bool {
	return type_str.starts_with('map[')
}

// array_elem_type extracts the element type from an array type string.
// `[]int` -> `int`, `[][]string` -> `[]string`.
fn (t &Transformer) array_elem_type(type_str string) string {
	if type_str.starts_with('[]') {
		return type_str[2..]
	}
	return ''
}

// map_value_type extracts the value type from a map type string.
// `map[string]int` -> `int`, `map[string][]bool` -> `[]bool`.
fn (t &Transformer) map_value_type(type_str string) string {
	if !type_str.starts_with('map[') {
		return ''
	}
	bracket_end := generic_matching_bracket(type_str, 3)
	if bracket_end + 1 < type_str.len {
		return type_str[bracket_end + 1..]
	}
	return ''
}

// map_key_type extracts the key type from a map type string.
// `map[string]int` -> `string`, `map[int]bool` -> `int`.
fn (t &Transformer) map_key_type(type_str string) string {
	if !type_str.starts_with('map[') {
		return ''
	}
	bracket_end := generic_matching_bracket(type_str, 3)
	if bracket_end > 4 {
		return type_str[4..bracket_end]
	}
	return ''
}
