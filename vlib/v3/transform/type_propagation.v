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
	rhs_authority := t.decl_rhs_type(rhs_id)
	if t.is_fn_pointer_type_name(rhs_authority) {
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
		t.set_var_type(lhs.value, t.normalize_type_alias(typ))
	}
}

fn decl_type_is_usable(typ string) bool {
	return typ.len > 0 && typ !in ['unknown', 'array', 'map']
}

fn (t &Transformer) decl_rhs_type(id flat.NodeId) string {
	if fn_type := t.fn_value_type_name(id) {
		return fn_type
	}
	return t.node_type(id)
}

fn (t &Transformer) fn_value_type_name(id flat.NodeId) ?string {
	if isnil(t.tc) || int(id) < 0 {
		return none
	}
	if typ := t.tc.expr_type(id) {
		if name := fn_value_type_name_from_type(typ) {
			return t.normalize_type_alias(name)
		}
	}
	node := t.a.nodes[int(id)]
	if node.kind == .fn_literal || node.kind == .lambda_expr {
		typ := t.tc.resolve_type(id)
		if name := fn_value_type_name_from_type(typ) {
			return t.normalize_type_alias(name)
		}
	}
	return none
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
	if enum_type := t.enum_type_name_from_expr(base_id) {
		if fields := t.enum_types[enum_type] {
			if field_name in fields {
				return enum_type
			}
		}
	}
	if type_name := t.qualified_enum_type_selector_name(base_id, field_name) {
		return type_name
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
	lookup := t.lookup_struct_info_for_field(type_name, field_name) or { return none }
	for f in lookup.info.fields {
		if f.name == field_name {
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
	if typ.len == 0 {
		return typ
	}
	if typ.starts_with('&') {
		return '&' + t.normalize_field_type(typ[1..], owner_type)
	}
	if typ.starts_with('[]') {
		return '[]' + t.normalize_field_type(typ[2..], owner_type)
	}
	if typ.starts_with('?') {
		return '?' + t.normalize_field_type(typ[1..], owner_type)
	}
	if typ.starts_with('!') {
		return '!' + t.normalize_field_type(typ[1..], owner_type)
	}
	if typ.starts_with('map[') {
		bracket_end := typ.index(']') or { return t.normalize_type_alias(typ) }
		key_type := t.normalize_field_type(typ[4..bracket_end], owner_type)
		value_type := t.normalize_field_type(typ[bracket_end + 1..], owner_type)
		return 'map[${key_type}]${value_type}'
	}
	if typ.starts_with('[') {
		bracket_end := typ.index(']') or { return t.normalize_type_alias(typ) }
		return typ[..bracket_end + 1] + t.normalize_field_type(typ[bracket_end + 1..], owner_type)
	}
	owner_base, owner_args, owner_is_generic_app := generic_app_parts(owner_type)
	if owner_is_generic_app {
		if owner_base.len > 0 {
			substituted := substitute_generic_type_text(typ, owner_args)
			if substituted != typ {
				return t.normalize_field_type(substituted, owner_type)
			}
		}
	}
	base, args, type_is_generic_app := generic_app_parts(typ)
	if type_is_generic_app {
		mut field_base := base
		if !field_base.contains('.') && owner_type.contains('.') {
			owner_mod := owner_type.all_before_last('.')
			qbase := '${owner_mod}.${field_base}'
			if t.type_authority_has(qbase) {
				field_base = qbase
			}
		}
		mut normalized_args := []string{cap: args.len}
		for arg in args {
			mut normalized_arg := t.normalize_field_type(arg, owner_type)
			if field_base.contains('.') {
				field_mod := field_base.all_before_last('.')
				if normalized_arg.starts_with('${field_mod}.') {
					normalized_arg = normalized_arg.all_after_last('.')
				}
			}
			normalized_args << normalized_arg
		}
		return t.normalize_type_alias('${field_base}[${normalized_args.join(', ')}]')
	}
	if typ.contains('.') || !owner_type.contains('.') {
		return t.normalize_type_alias(typ)
	}
	owner_mod := owner_type.all_before_last('.')
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
	if c.module != t.cur_module {
		c.module = t.cur_module
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
	if typ.starts_with('&') {
		return '&' + t.normalize_type_alias(typ[1..])
	}
	if typ.starts_with('[]') {
		return '[]' + t.normalize_type_alias(typ[2..])
	}
	if typ.starts_with('?') {
		return '?' + t.normalize_type_alias(typ[1..])
	}
	if typ.starts_with('!') {
		return '!' + t.normalize_type_alias(typ[1..])
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
	if typ in t.structs || typ in t.sum_types || typ in t.enum_types {
		return typ
	}
	if target := t.tc.type_aliases[typ] {
		return target
	}
	if !typ.contains('.') && t.cur_module.len > 0 && t.cur_module != 'main'
		&& t.cur_module != 'builtin' {
		qtyp := '${t.cur_module}.${typ}'
		if target := t.tc.type_aliases[qtyp] {
			return target
		}
	}
	return typ
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
	if node.value == 'range' {
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
	if !isnil(t.tc) {
		if typ := t.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 && name != 'unknown' {
				return t.normalize_type_alias(name)
			}
		}
	}
	return t.resolve_index_elem_type(node)
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
	resolved := t.resolve_expr_type(id)
	if resolved.len > 0 {
		return resolved
	}
	node := t.a.nodes[int(id)]
	if node.typ.len > 0 {
		return t.normalize_type_alias(node.typ)
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
	// NOTE: infix is intentionally not handled here — resolve_expr_type() (called at the top
	// of node_type) already resolves infix types, including struct operator overloads.
	if node.kind == .struct_init && node.value.len > 0 {
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
		if typ := t.tc.expr_type(id) {
			name := typ.name()
			if name.len > 0 && name != 'void' && (name != 'int'
				|| node.kind in [.ident, .int_literal, .infix, .prefix, .paren, .selector, .index, .call]) {
				return name
			}
		}
	}
	return ''
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
	return t.node_type(id) == 'string'
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
	bracket_end := type_str.index(']') or { return '' }
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
	bracket_end := type_str.index(']') or { return '' }
	if bracket_end > 4 {
		return type_str[4..bracket_end]
	}
	return ''
}
