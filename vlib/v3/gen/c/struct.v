module c

import v3.flat
import v3.types

fn c_field_name(name string) string {
	if name.starts_with('&') {
		return c_field_name(name[1..])
	}
	if name.starts_with('ptr') && name.len > 3 && name[3..].contains('__') {
		return c_name(name[3..])
	}
	if name.starts_with('ptr') && name.len > 3 && name[3..].contains('.') {
		return c_name(name[3..])
	}
	return c_name(name)
}

fn (mut g FlatGen) gen_struct_init(node flat.Node) {
	if node.value.starts_with('chan ') {
		g.gen_channel_init(node)
		return
	}
	if g.gen_lowered_sum_init(node) {
		return
	}
	name := g.struct_init_c_type_name(node.value)
	if node.children_count == 0 && g.is_scalar_zero_init_type(node.value, name) {
		g.write(g.scalar_zero_init(name))
		return
	}
	g.write('(${name}){')
	mut set_fields := map[string]bool{}
	mut has_field := false
	if g.is_interface_type_name(node.value) {
		if tid := g.interface_init_typ_id(node) {
			g.write('._typ = ${tid}')
			has_field = true
		}
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if has_field {
			g.write(', ')
		}
		value_id := g.a.child(field, 0)
		if field.value.len == 0 {
			if sf := g.struct_field_at(node.value, i) {
				if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(node.value, sf.name,
					value_id)
				{
					inner_ct := g.tc.c_type(heap_copy_type)
					g.write('(${inner_ct}*)memdup(')
					g.gen_expr(value_id)
					g.write(', sizeof(${inner_ct}))')
				} else {
					g.gen_expr_with_expected_type(value_id, sf.typ)
				}
				set_fields[sf.name] = true
			} else {
				g.gen_expr(value_id)
			}
		} else {
			g.write('.${c_field_name(field.value)} = ')
			if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(node.value, field.value,
				value_id)
			{
				inner_ct := g.tc.c_type(heap_copy_type)
				g.write('(${inner_ct}*)memdup(')
				g.gen_expr(value_id)
				g.write(', sizeof(${inner_ct}))')
			} else {
				if ftyp := g.struct_field_type(node.value, field.value) {
					g.gen_expr_with_expected_type(value_id, ftyp)
				} else {
					g.gen_expr(value_id)
				}
			}
			set_fields[field.value] = true
		}
		has_field = true
	}
	qname := g.tc.qualify_name(node.value)
	sname := if qname in g.tc.structs { qname } else { node.value }
	if sname in g.tc.structs {
		has_field = g.gen_struct_default_fields(node.value, mut set_fields, has_field)
		for f in g.tc.structs[sname] {
			if f.name in set_fields {
				continue
			}
			if f.typ is types.Map {
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(f.name)} = ')
				g.write_new_map(f.typ.key_type, f.typ.value_type)
				has_field = true
			} else if f.typ is types.Array {
				c_elem := g.tc.c_type(f.typ.elem_type)
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(f.name)} = array_new(sizeof(${c_elem}), 0, 0)')
				has_field = true
			} else if g.field_needs_default_init(f.typ) {
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(f.name)} = ')
				g.gen_default_value_for_type(f.typ)
				has_field = true
			}
		}
	}
	g.write('}')
}

fn (mut g FlatGen) gen_lowered_sum_init(node flat.Node) bool {
	sum_name := g.resolve_sum_name(node.value)
	if sum_name !in g.tc.sum_types || node.children_count == 0 {
		return false
	}
	name := g.struct_init_c_type_name(node.value)
	g.write('(${name}){')
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if i > 0 {
			g.write(', ')
		}
		g.write('.${c_field_name(field.value)} = ')
		g.gen_lowered_sum_field_value(sum_name, field)
	}
	g.write('}')
	return true
}

fn (mut g FlatGen) gen_lowered_sum_field_value(sum_name string, field &flat.Node) {
	child_id := g.a.child(field, 0)
	if field.value != 'typ' {
		mut variant := ''
		if field.typ.starts_with('&') {
			variant = field.typ[1..]
		} else if field.typ.len > 0 {
			variant = field.typ
		} else {
			for v in g.tc.sum_types[sum_name] {
				if g.sum_field_name(v) == field.value {
					variant = v
					break
				}
			}
		}
		if variant.len > 0 {
			variant = g.resolve_variant(sum_name, variant)
			inner_type := g.tc.parse_type(variant)
			inner_ct := g.tc.c_type(inner_type)
			child_type := g.tc.resolve_type(child_id)
			g.write('(${inner_ct}*)memdup(')
			if child_type is types.Pointer && g.type_names_match(child_type.base_type, inner_type) {
				g.gen_expr(child_id)
			} else {
				g.write('(${inner_ct}[]){')
				g.gen_expr_with_expected_type(child_id, inner_type)
				g.write('}')
			}
			g.write(', sizeof(${inner_ct}))')
			return
		}
	}
	if field.typ.len > 0 {
		g.gen_expr_with_expected_type(child_id, g.tc.parse_type(field.typ))
	} else {
		g.gen_expr(child_id)
	}
}

fn (mut g FlatGen) gen_channel_init(node flat.Node) {
	elem_type := g.tc.parse_type(node.value[5..])
	elem_ct := g.tc.c_type(elem_type)
	g.write('sync__new_channel_st((u32)(')
	if cap_id := channel_init_field(node, g.a, 'cap') {
		g.gen_expr(cap_id)
	} else {
		g.write('0')
	}
	g.write('), (u32)(sizeof(${elem_ct})))')
}

fn channel_init_field(node flat.Node, a &flat.FlatAst, name string) ?flat.NodeId {
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		if field.kind == .field_init && field.value == name && field.children_count > 0 {
			return a.child(field, 0)
		}
	}
	return none
}

fn (mut g FlatGen) gen_heap_struct_init(node flat.Node) {
	name := g.struct_init_c_type_name(node.value)
	sum_name := g.resolve_sum_name(node.value)
	is_sum_literal := sum_name in g.tc.sum_types
	g.write('(${name}*)memdup(&(${name}){')
	mut set_fields := map[string]bool{}
	mut has_field := false
	if g.is_interface_type_name(node.value) {
		if tid := g.interface_init_typ_id(node) {
			g.write('._typ = ${tid}')
			has_field = true
		}
	}
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if has_field {
			g.write(', ')
		}
		g.write('.${c_field_name(field.value)} = ')
		value_id := g.a.child(field, 0)
		if is_sum_literal {
			g.gen_lowered_sum_field_value(sum_name, field)
		} else if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(node.value, field.value,
			value_id)
		{
			inner_ct := g.tc.c_type(heap_copy_type)
			g.write('(${inner_ct}*)memdup(')
			g.gen_expr(value_id)
			g.write(', sizeof(${inner_ct}))')
		} else {
			if ftyp := g.struct_field_type(node.value, field.value) {
				g.gen_expr_with_expected_type(value_id, ftyp)
			} else {
				g.gen_expr(value_id)
			}
		}
		set_fields[field.value] = true
		has_field = true
	}
	qname := g.tc.qualify_name(node.value)
	sname := if qname in g.tc.structs { qname } else { node.value }
	if sname in g.tc.structs {
		has_field = g.gen_struct_default_fields(node.value, mut set_fields, has_field)
		for f in g.tc.structs[sname] {
			if f.name in set_fields {
				continue
			}
			if f.typ is types.Map {
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(f.name)} = ')
				g.write_new_map(f.typ.key_type, f.typ.value_type)
				has_field = true
			} else if f.typ is types.Array {
				c_elem := g.tc.c_type(f.typ.elem_type)
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(f.name)} = array_new(sizeof(${c_elem}), 0, 0)')
				has_field = true
			} else if g.field_needs_default_init(f.typ) {
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(f.name)} = ')
				g.gen_default_value_for_type(f.typ)
				has_field = true
			}
		}
	}
	g.write('}, sizeof(${name}))')
}

fn (g &FlatGen) heap_copy_type_for_sum_pointer_field(type_name string, field_name string, value_id flat.NodeId) ?types.Type {
	resolved_sum := g.resolve_sum_name(type_name)
	if resolved_sum !in g.tc.sum_types || int(value_id) < 0 {
		return none
	}
	value := g.a.nodes[int(value_id)]
	if !g.pointer_variant_arg_needs_heap_copy(value) {
		return none
	}
	for variant in g.tc.sum_types[resolved_sum] {
		if g.sum_field_name(variant) != field_name
			|| !g.variant_references_sum(variant, resolved_sum) {
			continue
		}
		variant_type := g.tc.parse_type(g.resolve_variant(resolved_sum, variant))
		return variant_type
	}
	return none
}

fn (mut g FlatGen) gen_struct_default_fields(type_name string, mut set_fields map[string]bool, has_field bool) bool {
	mut has := has_field
	info := g.find_struct_decl(type_name) or { return has }
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind != .field_decl || field.children_count == 0 || field.value in set_fields {
			continue
		}
		if has {
			g.write(', ')
		}
		g.write('.${c_name(field.value)} = ')
		g.gen_expr_with_expected_type(g.a.child(field, 0), g.tc.parse_type(field.typ))
		set_fields[field.value] = true
		has = true
	}
	g.tc.cur_module = old_module
	return has
}

fn (mut g FlatGen) gen_default_value_for_type(typ types.Type) {
	raw_typ := typ
	if typ is types.Struct && !typ.name.starts_with('C.') {
		ct := g.tc.c_type(raw_typ)
		g.write('(${ct}){')
		mut set_fields := map[string]bool{}
		mut has_field := g.gen_struct_default_fields(typ.name, mut set_fields, false)
		mut sname := g.tc.qualify_name(typ.name)
		if typ.name in g.tc.structs {
			sname = typ.name
		}
		if sname in g.tc.structs {
			for f in g.tc.structs[sname] {
				if f.name in set_fields {
					continue
				}
				if f.typ is types.Map {
					if has_field {
						g.write(', ')
					}
					g.write('.${c_name(f.name)} = ')
					g.write_new_map(f.typ.key_type, f.typ.value_type)
					has_field = true
				} else if f.typ is types.Array {
					c_elem := g.tc.c_type(f.typ.elem_type)
					if has_field {
						g.write(', ')
					}
					g.write('.${c_name(f.name)} = array_new(sizeof(${c_elem}), 0, 0)')
					has_field = true
				} else if g.field_needs_default_init(f.typ) {
					if has_field {
						g.write(', ')
					}
					g.write('.${c_name(f.name)} = ')
					g.gen_default_value_for_type(f.typ)
					has_field = true
				}
			}
		}
		g.write('}')
		return
	}
	ct := g.tc.c_type(typ)
	if g.is_scalar_c_type(ct) {
		g.write(g.scalar_zero_init(ct))
		return
	}
	g.write('(${ct}){0}')
}

// field_needs_default_init reports whether an unset field of type `typ` must be
// explicitly default-initialized in a struct literal — i.e. it is a by-value
// struct whose type carries field defaults that C's `{0}` would not apply
// (e.g. `min_len int = 999999`).
fn (mut g FlatGen) field_needs_default_init(typ types.Type) bool {
	if typ is types.Struct && !typ.name.starts_with('C.') {
		return g.struct_has_field_defaults(typ.name)
	}
	return false
}

// struct_has_field_defaults reports whether building `type_name` as a struct
// literal would set any non-zero field: a field with an explicit default
// (`x int = 5`), or a by-value struct field whose own type has such defaults.
// Returns false for structs with interface/sum-typed field defaults, since the
// codegen default path cannot box those values.
fn (mut g FlatGen) struct_has_field_defaults(type_name string) bool {
	mut visited := map[string]bool{}
	return g.struct_has_field_defaults_inner(type_name, mut visited)
}

fn (mut g FlatGen) struct_has_field_defaults_inner(type_name string, mut visited map[string]bool) bool {
	if type_name in visited {
		return false
	}
	visited[type_name] = true
	info := g.find_struct_decl(type_name) or { return false }
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	defer {
		g.tc.cur_module = old_module
	}
	mut found := false
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind != .field_decl {
			continue
		}
		ftyp := g.tc.parse_type(field.typ)
		if field.children_count > 0 {
			// Defaults for interface/sum-typed fields require boxing the value into
			// the interface/sum representation, which the codegen default path cannot
			// do. Treat the whole struct as unsafe to default-emit (leave it
			// zero-initialized, as before) rather than emit an unboxed value.
			if ftyp is types.SumType || ftyp is types.Interface {
				return false
			}
			found = true
		}
		if ftyp is types.Struct && !ftyp.name.starts_with('C.') {
			if g.struct_has_field_defaults_inner(ftyp.name, mut visited) {
				found = true
			}
		}
	}
	return found
}

// gen_params_struct_arg emits a struct literal for a `@[params]` argument passed as
// trailing `key: value` call args (e.g. `atof64(s, allow_extra_chars: true)`).
// `node` is the call node; field_init children are read from `field_start` onward.
fn (mut g FlatGen) gen_params_struct_arg(typ types.Type, node flat.Node, field_start int) {
	raw_typ := typ
	if typ is types.Struct {
		ct := g.tc.c_type(raw_typ)
		g.write('(${ct}){')
		mut set_fields := map[string]bool{}
		mut has_field := false
		for i in field_start .. node.children_count {
			field := g.a.child_node(&node, i)
			if field.kind != .field_init || field.children_count == 0 {
				continue
			}
			if has_field {
				g.write(', ')
			}
			g.write('.${c_name(field.value)} = ')
			if ftyp := g.struct_field_type(typ.name, field.value) {
				g.gen_expr_with_expected_type(g.a.child(field, 0), ftyp)
			} else {
				g.gen_expr(g.a.child(field, 0))
			}
			set_fields[field.value] = true
			has_field = true
		}
		mut sname := g.tc.qualify_name(typ.name)
		if typ.name in g.tc.structs {
			sname = typ.name
		}
		has_field = g.gen_struct_default_fields(typ.name, mut set_fields, has_field)
		if sname in g.tc.structs {
			for f in g.tc.structs[sname] {
				if f.name in set_fields {
					continue
				}
				if f.typ is types.Map {
					if has_field {
						g.write(', ')
					}
					g.write('.${c_name(f.name)} = ')
					g.write_new_map(f.typ.key_type, f.typ.value_type)
					has_field = true
				} else if f.typ is types.Array {
					c_elem := g.tc.c_type(f.typ.elem_type)
					if has_field {
						g.write(', ')
					}
					g.write('.${c_name(f.name)} = array_new(sizeof(${c_elem}), 0, 0)')
					has_field = true
				} else if g.field_needs_default_init(f.typ) {
					if has_field {
						g.write(', ')
					}
					g.write('.${c_name(f.name)} = ')
					g.gen_default_value_for_type(f.typ)
					has_field = true
				}
			}
		}
		g.write('}')
		return
	}
	g.gen_default_value_for_type(typ)
}

fn (g &FlatGen) is_scalar_zero_init_type(type_name string, c_type string) bool {
	if type_name in g.tc.structs || g.tc.qualify_name(type_name) in g.tc.structs {
		return false
	}
	if _ := g.find_struct_decl(type_name) {
		return false
	}
	return g.is_scalar_c_type(c_type)
}

fn (g &FlatGen) is_scalar_c_type(c_type string) bool {
	if c_type.ends_with('*') {
		return true
	}
	return c_type in ['bool', 'char', 'byte', 'u8', 'i8', 'u16', 'i16', 'u32', 'i32', 'u64', 'i64',
		'int', 'isize', 'usize', 'size_t', 'ptrdiff_t', 'float', 'double', 'voidptr']
}

fn (g &FlatGen) is_aggregate_zero_init_type(typ types.Type, c_type string) bool {
	if g.is_scalar_c_type(c_type) {
		return false
	}
	return match typ {
		types.Alias {
			g.is_aggregate_zero_init_type(typ.base_type, c_type)
		}
		types.Array, types.ArrayFixed, types.Channel, types.Map, types.String, types.Struct,
		types.Interface, types.SumType, types.OptionType, types.ResultType, types.MultiReturn {
			true
		}
		else {
			false
		}
	}
}

fn (g &FlatGen) scalar_zero_init(c_type string) string {
	if c_type in ['float', 'double'] {
		return '0.0'
	}
	return '0'
}

struct StructDeclInfo {
	node      flat.Node
	module    string
	full_name string
}

fn (mut g FlatGen) struct_init_c_type_name(type_name string) string {
	typ := g.tc.parse_type(type_name)
	if typ is types.OptionType || typ is types.ResultType {
		return g.optional_type_name(typ)
	}
	info := g.find_struct_decl(type_name) or { return g.tc.c_type(g.tc.parse_type(type_name)) }
	if info.full_name.starts_with('C.') {
		return g.tc.c_type(g.tc.parse_type(info.full_name))
	}
	return c_name(info.full_name)
}

fn (g &FlatGen) find_struct_decl(type_name string) ?StructDeclInfo {
	short_name := if type_name.contains('.') { type_name.all_after_last('.') } else { type_name }
	preferred_name := if !type_name.contains('.') && g.tc.cur_module.len > 0
		&& g.tc.cur_module != 'main' && g.tc.cur_module != 'builtin' {
		'${g.tc.cur_module}.${type_name}'
	} else {
		type_name
	}
	if info := g.struct_decl_infos[preferred_name] {
		if info.node.value == short_name {
			return info
		}
	}
	if type_name.contains('.') {
		if info := g.struct_decl_infos[type_name] {
			return info
		}
	} else {
		if info := g.struct_decl_short_infos[type_name] {
			return info
		}
	}
	return none
}

fn (g &FlatGen) struct_field_type(type_name string, field_name string) ?types.Type {
	qname := g.tc.qualify_name(type_name)
	if fields := g.tc.structs[qname] {
		for f in fields {
			if f.name == field_name {
				return f.typ
			}
		}
	}
	if fields := g.tc.structs[type_name] {
		for f in fields {
			if f.name == field_name {
				return f.typ
			}
		}
	}
	if info := g.find_struct_decl(type_name) {
		if fields := g.tc.structs[info.full_name] {
			for f in fields {
				if f.name == field_name {
					return f.typ
				}
			}
		}
	}
	return none
}

fn (g &FlatGen) struct_field_at(type_name string, index int) ?types.StructField {
	if index < 0 {
		return none
	}
	qname := g.tc.qualify_name(type_name)
	if fields := g.tc.structs[qname] {
		if index < fields.len {
			return fields[index]
		}
	}
	if fields := g.tc.structs[type_name] {
		if index < fields.len {
			return fields[index]
		}
	}
	if info := g.find_struct_decl(type_name) {
		if fields := g.tc.structs[info.full_name] {
			if index < fields.len {
				return fields[index]
			}
		}
	}
	return none
}

fn (g &FlatGen) struct_field_type_at(type_name string, index int) ?types.Type {
	if field := g.struct_field_at(type_name, index) {
		return field.typ
	}
	return none
}

fn (mut g FlatGen) gen_return_assoc(node flat.Node) {
	ct := g.tc.c_type(g.tc.parse_type(node.value))
	tmp := g.tmp_name()
	g.write('${ct} ${tmp} = ')
	g.gen_expr(g.a.child(&node, 0))
	g.writeln(';')
	for i in 1 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.children_count > 0 {
			g.write('${tmp}.${c_name(field.value)} = ')
			if ftyp := g.struct_field_type(node.value, field.value) {
				g.gen_expr_with_expected_type(g.a.child(field, 0), ftyp)
			} else {
				g.gen_expr(g.a.child(field, 0))
			}
			g.writeln(';')
		}
	}
	g.writeln('return ${tmp};')
}

fn (mut g FlatGen) gen_assoc_expr(node flat.Node) {
	ct := g.tc.c_type(g.tc.parse_type(node.value))
	tmp := g.tmp_name()
	g.write('({${ct} ${tmp} = ')
	g.gen_expr(g.a.child(&node, 0))
	g.write(';')
	for i in 1 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.children_count > 0 {
			g.write(' ${tmp}.${c_name(field.value)} = ')
			if ftyp := g.struct_field_type(node.value, field.value) {
				g.gen_expr_with_expected_type(g.a.child(field, 0), ftyp)
			} else {
				g.gen_expr(g.a.child(field, 0))
			}
			g.write(';')
		}
	}
	g.write(' ${tmp};})')
}

fn (mut g FlatGen) gen_heap_assoc_expr(node flat.Node) {
	ct := g.tc.c_type(g.tc.parse_type(node.value))
	tmp := g.tmp_name()
	g.write('({${ct} ${tmp} = ')
	g.gen_expr(g.a.child(&node, 0))
	g.write(';')
	for i in 1 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.kind == .field_init && field.children_count > 0 {
			g.write(' ${tmp}.${c_name(field.value)} = ')
			if ftyp := g.struct_field_type(node.value, field.value) {
				g.gen_expr_with_expected_type(g.a.child(field, 0), ftyp)
			} else {
				g.gen_expr(g.a.child(field, 0))
			}
			g.write(';')
		}
	}
	g.write(' (${ct}*)memdup(&${tmp}, sizeof(${ct}));})')
}

fn (mut g FlatGen) gen_map_init(id flat.NodeId, node flat.Node) {
	if node.value.len > 0 {
		map_type := g.tc.parse_type(node.value)
		if map_type is types.Map {
			g.write_new_map(map_type.key_type, map_type.value_type)
			return
		}
	}
	if node.typ.len > 0 {
		map_type := g.tc.parse_type(node.typ)
		if map_type is types.Map {
			g.write_new_map(map_type.key_type, map_type.value_type)
			return
		}
	}
	if g.expected_expr_type is types.Map {
		g.write_new_map(g.expected_expr_type.key_type, g.expected_expr_type.value_type)
		return
	}
	resolved_type := g.tc.resolve_type(id)
	if resolved_type is types.Map {
		g.write_new_map(resolved_type.key_type, resolved_type.value_type)
		return
	}
	g.write('new_map(sizeof(int), sizeof(int), 0, 0, 0, 0)')
}

fn (mut g FlatGen) write_new_map(key_type types.Type, value_type types.Type) {
	c_key := g.tc.c_type(key_type)
	c_val := g.tc.c_type(value_type)
	hash_fn, eq_fn, clone_fn, free_fn := g.map_callback_names(key_type)
	g.write('new_map(sizeof(${c_key}), sizeof(${c_val}), ${hash_fn}, ${eq_fn}, ${clone_fn}, ${free_fn})')
}

fn (g &FlatGen) map_callback_names(key_type types.Type) (string, string, string, string) {
	if key_type is types.String {
		return 'v3_map_hash_string', 'v3_map_eq_string', 'v3_map_clone_string', 'v3_map_free_string'
	}
	c_key := g.tc.c_type(key_type)
	size_suffix := match c_key {
		'u8', 'i8', 'bool', 'char' { '1' }
		'u16', 'i16' { '2' }
		'i64', 'u64', 'isize', 'usize', 'voidptr' { '8' }
		else { '4' }
	}

	return 'v3_map_hash_int_${size_suffix}', 'v3_map_eq_int_${size_suffix}', 'v3_map_clone_int_${size_suffix}', 'v3_map_free_nop'
}

fn (g &FlatGen) skip_builtin_struct(name string) bool {
	_ = g
	if name.starts_with('C.') {
		return true
	}
	return false
}

fn (mut g FlatGen) emit_interface_struct(name string) {
	cn := c_name(name)
	g.writeln('struct ${cn} {')
	g.writeln('\tint _typ;')
	if cn == 'IError' {
		g.writeln('\tvoid* _object;')
		g.writeln('\tstring message;')
		g.writeln('\tint code;')
	} else {
		// pointer to the boxed concrete value, used by method dispatch
		g.writeln('\tvoid* _object;')
	}
	for field in g.tc.interface_fields[name] or { []types.StructField{} } {
		ct := g.tc.c_type(field.typ)
		g.writeln('\t${ct} ${c_name(field.name)};')
	}
	g.writeln('};')
	g.writeln('')
}

fn (mut g FlatGen) struct_decls() {
	for name, _ in g.tc.structs {
		if g.skip_builtin_struct(name) {
			continue
		}
		tag := if name in g.tc.unions { 'union' } else { 'struct' }
		g.writeln('typedef ${tag} ${c_name(name)} ${c_name(name)};')
	}
	for name, variants in g.tc.sum_types {
		g.writeln('typedef struct ${c_name(name)} ${c_name(name)};')
		_ = variants
	}
	for name, _ in g.interfaces {
		g.writeln('typedef struct ${c_name(name)} ${c_name(name)};')
	}
	if g.has_builtins {
		g.writeln('typedef array Array;')
	}
	mut emitted := map[string]bool{}
	mut remaining := map[string]bool{}
	mut remaining_cnames := map[string]bool{}
	mut iface_remaining := map[string]bool{}
	for name, _ in g.interfaces {
		iface_remaining[name] = true
		remaining_cnames[c_name(name)] = true
	}
	for name, _ in g.tc.structs {
		if g.skip_builtin_struct(name) {
			continue
		}
		remaining[name] = true
		remaining_cnames[c_name(name)] = true
	}
	mut sum_remaining := map[string]bool{}
	for name, _ in g.tc.sum_types {
		sum_remaining[name] = true
		remaining_cnames[c_name(name)] = true
	}
	if 'string' in remaining {
		g.emit_struct('string')
		emitted['string'] = true
		remaining.delete('string')
		remaining_cnames.delete('string')
	}
	mut has_ierror := false
	for name, _ in iface_remaining {
		if c_name(name) == 'IError' {
			g.emit_interface_struct(name)
			emitted['IError'] = true
			iface_remaining.delete(name)
			remaining_cnames.delete('IError')
			has_ierror = true
			break
		}
	}
	err_field := if has_ierror { 'IError err; ' } else { '' }
	g.writeln('typedef struct Optional { bool ok; ${err_field}int value; } Optional;')
	g.writeln('')
	for _ in 0 .. 30 {
		if remaining.len == 0 && iface_remaining.len == 0 && sum_remaining.len == 0 {
			break
		}
		mut progress := false
		mut emitted_ifaces := []string{}
		for name, _ in iface_remaining {
			cn := c_name(name)
			mut can_emit := true
			if cn == 'IError' {
				if 'string' !in emitted && 'string' in remaining_cnames {
					can_emit = false
				}
			}
			// An interface struct embeds its declared data fields by value, so the
			// field types must be fully defined first (same constraint as structs).
			for field in g.tc.interface_fields[name] or { []types.StructField{} } {
				if field.typ is types.Pointer {
					continue
				}
				fct := g.tc.c_type(field.typ)
				if fct !in emitted && fct != cn && fct in remaining_cnames {
					can_emit = false
					break
				}
			}
			if can_emit {
				g.emit_interface_struct(name)
				emitted[cn] = true
				remaining_cnames.delete(cn)
				emitted_ifaces << name
				progress = true
			}
		}
		for name in emitted_ifaces {
			iface_remaining.delete(name)
		}
		mut emitted_structs := []string{}
		for name, _ in remaining {
			cn := c_name(name)
			if cn in emitted {
				remaining_cnames.delete(cn)
				emitted_structs << name
				progress = true
				continue
			}
			mut can_emit := true
			if name in g.tc.structs {
				for f in g.tc.structs[name] {
					if f.typ is types.Pointer {
						continue
					}
					mut ct := ''
					if f.typ is types.ArrayFixed {
						ct = g.tc.c_type(f.typ.elem_type)
					} else if f.typ is types.OptionType {
						ct = g.tc.c_type(f.typ.base_type)
					} else if f.typ is types.ResultType {
						ct = g.tc.c_type(f.typ.base_type)
					} else {
						ct = g.tc.c_type(f.typ)
					}
					if ct !in emitted && ct != cn && ct in remaining_cnames {
						can_emit = false
						break
					}
				}
			}
			if can_emit {
				if fields := g.tc.structs[name] {
					g.emit_struct_option_typedefs(fields)
				}
				g.emit_struct(name)
				emitted[cn] = true
				remaining_cnames.delete(cn)
				emitted_structs << name
				progress = true
			}
		}
		for name in emitted_structs {
			remaining.delete(name)
		}
		mut emitted_sums := []string{}
		for name, _ in sum_remaining {
			cn := c_name(name)
			mut can_emit_sum := true
			if name in g.tc.sum_types {
				for v in g.tc.sum_types[name] {
					if g.variant_references_sum(v, name) {
						continue
					}
					vt := g.tc.parse_type(v)
					if vt is types.SumType {
						if vt.name in sum_remaining {
							can_emit_sum = false
							break
						}
					}
					vct := g.tc.c_type(vt)
					if vct !in emitted && vct in remaining_cnames {
						can_emit_sum = false
						break
					}
				}
			}
			if can_emit_sum {
				g.emit_sum_type(name)
				emitted[cn] = true
				remaining_cnames.delete(cn)
				emitted_sums << name
				progress = true
			}
		}
		for name in emitted_sums {
			sum_remaining.delete(name)
		}
		if !progress {
			break
		}
	}
	for name, _ in iface_remaining {
		cn := c_name(name)
		g.emit_interface_struct(name)
		emitted[cn] = true
	}
	for name, _ in sum_remaining {
		g.emit_sum_type(name)
	}
	for name, _ in remaining {
		if fields := g.tc.structs[name] {
			g.emit_struct_option_typedefs(fields)
		}
		g.emit_struct(name)
	}
}

fn (mut g FlatGen) type_forward_decls() {
	for name, _ in g.tc.structs {
		if g.skip_builtin_struct(name) {
			continue
		}
		tag := if name in g.tc.unions { 'union' } else { 'struct' }
		g.writeln('typedef ${tag} ${c_name(name)} ${c_name(name)};')
	}
	for name, _ in g.tc.sum_types {
		g.writeln('typedef struct ${c_name(name)} ${c_name(name)};')
	}
	for name, _ in g.interfaces {
		g.writeln('typedef struct ${c_name(name)} ${c_name(name)};')
	}
	if g.has_builtins {
		g.writeln('typedef array Array;')
	}
	g.writeln('')
}

fn (mut g FlatGen) emit_struct(name string) {
	if name in g.tc.structs {
		fields := g.tc.structs[name]
		tag := if name in g.tc.unions { 'union' } else { 'struct' }
		g.writeln('${tag} ${c_name(name)} {')
		if fields.len == 0 {
			g.writeln('\tint _dummy;')
		}
		for f in fields {
			g.write_struct_field(name, f)
		}
		g.writeln('};')
		g.writeln('')
	}
}

fn (g &FlatGen) is_generic_struct(name string) bool {
	if info := g.struct_decl_infos[name] {
		return info.node.typ.contains('generic')
	}
	short_name := if name.contains('.') { name.all_after_last('.') } else { name }
	if info := g.struct_decl_short_infos[short_name] {
		return info.full_name == name && info.node.typ.contains('generic')
	}
	return false
}

fn (mut g FlatGen) write_struct_field(_struct_name string, f types.StructField) {
	mut field_type := f.typ
	if f.typ is types.Alias {
		field_type = f.typ.base_type
	}
	raw_field_type := field_type
	if field_type is types.FnType {
		ct := g.resolve_fn_ptr_type(g.tc.c_type(raw_field_type))
		g.writeln('\t${ct} ${c_name(f.name)};')
	} else if f.typ is types.ArrayFixed {
		c_elem := g.tc.c_type(f.typ.elem_type)
		len_expr := g.fixed_array_len_value(f.typ)
		g.writeln('\t${c_elem} ${c_name(f.name)}[${len_expr}];')
	} else {
		mut ct := if f.typ is types.OptionType || f.typ is types.ResultType {
			g.optional_type_name(f.typ)
		} else {
			g.tc.c_type(f.typ)
		}
		if ct.starts_with('fn_ptr:') {
			ct = g.resolve_fn_ptr_type(ct)
		}
		g.writeln('\t${ct} ${c_name(f.name)};')
	}
}

fn (mut g FlatGen) preseed_struct_fn_ptr_types() {
	for _, fields in g.tc.structs {
		for f in fields {
			mut field_type := f.typ
			if f.typ is types.Alias {
				field_type = f.typ.base_type
			}
			raw_field_type := field_type
			if field_type is types.FnType {
				ct := g.tc.c_type(raw_field_type)
				g.resolve_fn_ptr_type(ct)
			}
		}
	}
}

fn (mut g FlatGen) emit_struct_option_typedefs(fields []types.StructField) {
	mut wrote := false
	for f in fields {
		if f.typ is types.OptionType || f.typ is types.ResultType {
			opt_name := g.optional_type_name(f.typ)
			if opt_name == 'Optional' {
				continue
			}
			if val_type := g.needed_optional_types[opt_name] {
				if g.emit_optional_typedef(opt_name, val_type) {
					wrote = true
				}
			}
		}
	}
	if wrote {
		g.writeln('')
	}
}
