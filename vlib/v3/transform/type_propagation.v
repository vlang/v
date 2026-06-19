module transform

import v3.flat

// propagate_decl_type infers the declared variable type from a .decl_assign node
// and registers it in var_types so downstream transforms can resolve it.
fn (mut t Transformer) propagate_decl_type(node flat.Node) {
	if node.kind != .decl_assign || node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident || lhs.value.len == 0 {
		return
	}
	mut typ := ''
	if node.typ.len > 0 {
		typ = node.typ
	} else {
		rhs_id := t.a.child(&node, 1)
		typ = t.resolve_expr_type(rhs_id)
	}
	if typ.len > 0 {
		t.set_var_type(lhs.value, typ)
	}
}

// resolve_selector_type resolves the type of a .selector node (e.g. `obj.field`).
// Looks up the base expression type, then finds the field in the struct definition.
fn (t &Transformer) resolve_selector_type(node flat.Node) string {
	if node.kind != .selector || node.children_count == 0 {
		return ''
	}
	base_id := t.a.child(&node, 0)
	base_type := t.resolve_expr_type(base_id)
	if base_type.len == 0 {
		return ''
	}
	field_name := node.value
	if field_name.len == 0 {
		return ''
	}
	base_key := t.expr_key(base_id)
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
	return ''
}

fn (t &Transformer) lookup_struct_field_type(type_name string, field_name string) ?string {
	if type_name.len == 0 || field_name.len == 0 {
		return none
	}
	mut lookup_type := if type_name.starts_with('&') { type_name[1..] } else { type_name }
	owner_type := lookup_type
	if lookup_type !in t.structs && lookup_type.contains('.') {
		short_type := lookup_type.all_after_last('.')
		if short_type in t.structs {
			lookup_type = short_type
		}
	}
	if lookup_type !in t.structs && !lookup_type.contains('.') && t.cur_module.len > 0
		&& t.cur_module != 'main' && t.cur_module != 'builtin' {
		qtype := '${t.cur_module}.${lookup_type}'
		if qtype in t.structs {
			lookup_type = qtype
		}
	}
	info := t.structs[lookup_type] or { return none }
	for f in info.fields {
		if f.name == field_name {
			return t.normalize_field_type(f.typ, owner_type)
		}
	}
	return none
}

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
	if typ.contains('.') || !owner_type.contains('.') {
		return t.normalize_type_alias(typ)
	}
	owner_mod := owner_type.all_before_last('.')
	qtyp := '${owner_mod}.${typ}'
	if qtyp in t.structs || qtyp in t.sum_types || qtyp in t.enum_types {
		return t.normalize_type_alias(qtyp)
	}
	if !isnil(t.tc) {
		if qtyp in t.tc.type_aliases {
			return t.normalize_type_alias(qtyp)
		}
	}
	return t.normalize_type_alias(typ)
}

fn (t &Transformer) normalize_type_alias(typ string) string {
	if typ.len == 0 || isnil(t.tc) {
		return typ
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
		if qtyp in t.structs || qtyp in t.sum_types || qtyp in t.enum_types {
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

// resolve_index_elem_type determines the element type of an .index expression.
// For `[]T` arrays returns `T`, for `map[K]V` returns `V`, for `string` returns `u8`.
fn (t &Transformer) resolve_index_elem_type(node flat.Node) string {
	if node.kind != .index || node.children_count == 0 {
		return ''
	}
	base_id := t.a.child(&node, 0)
	base_type := t.resolve_expr_type(base_id)
	if base_type.len == 0 {
		return ''
	}
	if node.value == 'range' {
		if base_type == 'string' {
			return 'string'
		}
		if base_type.starts_with('[]') {
			return base_type
		}
	}
	if base_type.starts_with('[]') {
		return t.normalize_type_alias(base_type[2..])
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
		elem_type := t.resolve_index_elem_type(node)
		if elem_type.len > 0 {
			return elem_type
		}
	}
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
	nt := t.node_type(id)
	if nt.len > 0 {
		return nt
	}
	node := t.a.nodes[int(id)]
	if node.kind == .selector {
		return t.resolve_selector_type(node)
	}
	if node.kind == .index {
		return t.resolve_index_elem_type(node)
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
