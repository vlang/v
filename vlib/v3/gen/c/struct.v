module c

import v3.flat
import v3.types

// c_field_name supports c field name handling for c.
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

// gen_struct_init emits struct init output for c.
fn (mut g FlatGen) gen_struct_init(node flat.Node) {
	init_module := g.tc.cur_module
	if node.value.starts_with('chan ') {
		g.gen_channel_init(node)
		return
	}
	if g.gen_lowered_sum_init(node) {
		return
	}
	mut name := g.struct_init_c_type_name(node.value)
	// A bare generic struct literal (`Vec4{..}`) carries no type args; when the
	// surrounding expected type fixes them (e.g. a `Vec4[f32]` return), emit the
	// concrete instance name so it matches the materialized struct.
	if inst := g.generic_struct_init_instance_ct(node.value) {
		name = inst
	}
	if node.children_count == 0 && g.is_scalar_zero_init_type(node.value, name) {
		g.write(g.scalar_zero_init(name))
		return
	}
	if !g.is_interface_type_name(node.value) && g.struct_init_has_fixed_array_field(node) {
		g.gen_struct_init_with_fixed_array_fields(node, name, init_module)
		return
	}
	g.write('(${name}){')
	mut allowed_fields := map[string]bool{}
	if fields := g.struct_fields_for_type(node.value) {
		for f in fields {
			allowed_fields[f.name] = true
		}
	}
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
		if field.value.len > 0 && allowed_fields.len > 0 && field.value !in allowed_fields {
			continue
		}
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
					if g.struct_field_value_is_plainly_incompatible(value_id, ftyp) {
						g.gen_default_value_for_type(ftyp)
					} else {
						g.gen_expr_with_expected_type(value_id, ftyp)
					}
				} else {
					g.gen_expr(value_id)
				}
			}
			set_fields[field.value] = true
		}
		has_field = true
	}
	after_fields_module := g.tc.cur_module
	g.tc.cur_module = init_module
	qname := g.tc.qualify_name(node.value)
	sname := if qname in g.tc.structs { qname } else { node.value }
	g.tc.cur_module = after_fields_module
	has_field = g.gen_struct_default_fields(sname, mut set_fields, has_field)
	if sname in g.tc.structs {
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

fn (mut g FlatGen) struct_init_has_fixed_array_field(node flat.Node) bool {
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.value.len == 0 {
			if sf := g.struct_field_at(node.value, i) {
				if sf.typ is types.ArrayFixed {
					return true
				}
			}
			continue
		}
		if ftyp := g.struct_field_type(node.value, field.value) {
			if ftyp is types.ArrayFixed {
				return true
			}
		}
	}
	return false
}

fn (mut g FlatGen) gen_struct_init_with_fixed_array_fields(node flat.Node, name string, init_module string) {
	g.gen_struct_init_with_fixed_array_fields_impl(node, name, init_module, false)
}

// gen_struct_init_with_fixed_array_fields_impl builds a struct that has fixed-array
// fields via a temp + per-field `memcpy` (array members can't be assigned in a
// compound literal). When `heap`, the temp is `memdup`'d and a pointer returned,
// for `&Struct{...}` initializers.
fn (mut g FlatGen) gen_struct_init_with_fixed_array_fields_impl(node flat.Node, name string, init_module string, heap bool) {
	tmp := g.tmp_name()
	if heap {
		g.write('(${name}*)')
	}
	g.write('({${name} ${tmp} = (${name}){')
	mut allowed_fields := map[string]bool{}
	if fields := g.struct_fields_for_type(node.value) {
		for f in fields {
			allowed_fields[f.name] = true
		}
	}
	mut fixed_fields := []string{}
	mut fixed_values := []flat.NodeId{}
	mut fixed_field_types := []types.Type{}
	mut set_fields := map[string]bool{}
	mut has_field := false
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.value.len > 0 && allowed_fields.len > 0 && field.value !in allowed_fields {
			continue
		}
		value_id := g.a.child(field, 0)
		if field.value.len == 0 {
			if sf := g.struct_field_at(node.value, i) {
				if sf.typ is types.ArrayFixed {
					fixed_fields << sf.name
					fixed_values << value_id
					fixed_field_types << sf.typ
					set_fields[sf.name] = true
					continue
				}
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(sf.name)} = ')
				g.gen_expr_with_expected_type(value_id, sf.typ)
				set_fields[sf.name] = true
				has_field = true
			} else {
				if has_field {
					g.write(', ')
				}
				g.gen_expr(value_id)
				has_field = true
			}
		} else {
			ftyp := g.struct_field_type(node.value, field.value) or { types.Type(types.void_) }
			if ftyp is types.ArrayFixed {
				fixed_fields << field.value
				fixed_values << value_id
				fixed_field_types << ftyp
				set_fields[field.value] = true
				continue
			}
			if has_field {
				g.write(', ')
			}
			g.write('.${c_field_name(field.value)} = ')
			if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(node.value, field.value,
				value_id)
			{
				inner_ct := g.tc.c_type(heap_copy_type)
				g.write('(${inner_ct}*)memdup(')
				g.gen_expr(value_id)
				g.write(', sizeof(${inner_ct}))')
			} else if ftyp !is types.Void {
				if g.struct_field_value_is_plainly_incompatible(value_id, ftyp) {
					g.gen_default_value_for_type(ftyp)
				} else {
					g.gen_expr_with_expected_type(value_id, ftyp)
				}
			} else {
				g.gen_expr(value_id)
			}
			set_fields[field.value] = true
			has_field = true
		}
	}
	after_fields_module := g.tc.cur_module
	g.tc.cur_module = init_module
	qname := g.tc.qualify_name(node.value)
	sname := if qname in g.tc.structs { qname } else { node.value }
	g.tc.cur_module = after_fields_module
	has_field = g.gen_struct_default_fields(sname, mut set_fields, has_field)
	if sname in g.tc.structs {
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
	g.write('};')
	for i in 0 .. fixed_fields.len {
		cfield := c_field_name(fixed_fields[i])
		g.write(' memcpy(${tmp}.${cfield}, ')
		g.gen_fixed_array_copy_source(fixed_values[i], fixed_field_types[i])
		g.write(', sizeof(${tmp}.${cfield}));')
	}
	if heap {
		g.write(' memdup(&${tmp}, sizeof(${name}));})')
	} else {
		g.write(' ${tmp};})')
	}
}

// gen_fixed_array_copy_source emits a `memcpy` source for assigning into a fixed
// array. A raw array literal becomes a typed compound literal (a valid expression
// that decays to a pointer); a dynamic array value copies from its `.data` buffer;
// other fixed-array expressions (variables, fields, unwrapped calls) decay as-is.
fn (mut g FlatGen) gen_fixed_array_copy_source(value_id flat.NodeId, field_type types.Type) {
	val_node := g.a.node(value_id)
	if val_node.kind == .array_literal {
		g.write('(${g.tc.c_type(field_type)})')
		g.gen_expr(value_id)
		return
	}
	val_type := types.unwrap_pointer(g.usable_expr_type(value_id))
	if val_type is types.Array {
		g.write('(')
		g.gen_expr(value_id)
		g.write(').data')
		return
	}
	g.gen_expr(value_id)
}

// gen_lowered_sum_init emits lowered sum init output for c.
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

// gen_lowered_sum_field_value emits lowered sum field value output for c.
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

// gen_channel_init emits channel init output for c.
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

// channel_init_field supports channel init field handling for c.
fn channel_init_field(node flat.Node, a &flat.FlatAst, name string) ?flat.NodeId {
	for i in 0 .. node.children_count {
		field := a.child_node(&node, i)
		if field.kind == .field_init && field.value == name && field.children_count > 0 {
			return a.child(field, 0)
		}
	}
	return none
}

// gen_heap_struct_init emits heap struct init output for c.
fn (mut g FlatGen) gen_heap_struct_init(node flat.Node) {
	init_module := g.tc.cur_module
	name := g.struct_init_c_type_name(node.value)
	sum_name := g.resolve_sum_name(node.value)
	is_sum_literal := sum_name in g.tc.sum_types
	if !is_sum_literal && !g.is_interface_type_name(node.value)
		&& g.struct_init_has_fixed_array_field(node) {
		// Fixed-array fields can't be set in the `&(T){...}` compound literal; build
		// via a temp + memcpy and memdup the result.
		g.gen_struct_init_with_fixed_array_fields_impl(node, name, init_module, true)
		return
	}
	g.write('(${name}*)memdup(&(${name}){')
	mut allowed_fields := map[string]bool{}
	if fields := g.struct_fields_for_type(node.value) {
		for f in fields {
			allowed_fields[f.name] = true
		}
	}
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
		if field.value.len > 0 && allowed_fields.len > 0 && field.value !in allowed_fields {
			continue
		}
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
				if g.struct_field_value_is_plainly_incompatible(value_id, ftyp) {
					g.gen_default_value_for_type(ftyp)
				} else {
					g.gen_expr_with_expected_type(value_id, ftyp)
				}
			} else {
				g.gen_expr(value_id)
			}
		}
		set_fields[field.value] = true
		has_field = true
	}
	after_fields_module := g.tc.cur_module
	g.tc.cur_module = init_module
	qname := g.tc.qualify_name(node.value)
	sname := if qname in g.tc.structs { qname } else { node.value }
	g.tc.cur_module = after_fields_module
	has_field = g.gen_struct_default_fields(sname, mut set_fields, has_field)
	if sname in g.tc.structs {
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

// heap_copy_type_for_sum_pointer_field supports heap_copy_type_for_sum_pointer_field handling in c.
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

// gen_struct_default_fields emits struct default fields output for c.
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

// gen_default_value_for_type emits default value for type output for c.
fn (mut g FlatGen) gen_default_value_for_type(typ types.Type) {
	raw_typ := typ
	if typ is types.OptionType || typ is types.ResultType {
		ct := g.optional_type_name(typ)
		g.write('(${ct}){0}')
		return
	}
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

fn (g &FlatGen) struct_field_value_is_plainly_incompatible(value_id flat.NodeId, field_type types.Type) bool {
	value_type := g.tc.resolve_type(value_id)
	if field_type is types.Primitive && value_type is types.Struct {
		return true
	}
	if field_type is types.Primitive && value_type is types.SumType {
		return true
	}
	return false
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

// struct_has_field_defaults_inner converts struct has field defaults inner data for c.
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

// is_scalar_zero_init_type reports whether is scalar zero init type applies in c.
fn (g &FlatGen) is_scalar_zero_init_type(type_name string, c_type string) bool {
	if type_name in g.tc.structs || g.tc.qualify_name(type_name) in g.tc.structs {
		return false
	}
	if _ := g.find_struct_decl(type_name) {
		return false
	}
	return g.is_scalar_c_type(c_type)
}

// is_scalar_c_type reports whether is scalar c type applies in c.
fn (g &FlatGen) is_scalar_c_type(c_type string) bool {
	if c_type.ends_with('*') {
		return true
	}
	return c_type in ['bool', 'char', 'byte', 'u8', 'i8', 'u16', 'i16', 'u32', 'i32', 'u64', 'i64',
		'int', 'isize', 'usize', 'size_t', 'ptrdiff_t', 'float', 'double', 'voidptr']
}

// is_aggregate_zero_init_type reports whether is aggregate zero init type applies in c.
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

// can_use_global_brace_zero_init reports whether can use global brace zero init applies in c.
fn (mut g FlatGen) can_use_global_brace_zero_init(typ types.Type, c_type string) bool {
	return g.is_aggregate_zero_init_type(typ, c_type) && !g.has_zero_sized_leading_init_slot(typ)
}

// has_zero_sized_leading_init_slot reports whether has zero sized leading init slot applies in c.
fn (mut g FlatGen) has_zero_sized_leading_init_slot(typ types.Type) bool {
	mut visited := map[string]bool{}
	return g.has_zero_sized_leading_init_slot_inner(typ, mut visited)
}

// has_zero_sized_leading_init_slot_inner reports has_zero_sized_leading_init_slot_inner logic in c.
fn (mut g FlatGen) has_zero_sized_leading_init_slot_inner(typ types.Type, mut visited map[string]bool) bool {
	return match typ {
		types.Alias {
			g.has_zero_sized_leading_init_slot_inner(typ.base_type, mut visited)
		}
		types.ArrayFixed {
			if g.fixed_array_len_is_zero(typ) {
				true
			} else {
				g.has_zero_sized_leading_init_slot_inner(typ.elem_type, mut visited)
			}
		}
		types.Struct {
			if info := g.find_struct_decl(typ.name) {
				if info.full_name in visited {
					false
				} else {
					visited[info.full_name] = true
					old_module := g.tc.cur_module
					g.tc.cur_module = info.module
					first := g.struct_field_at(info.full_name, 0) or {
						g.tc.cur_module = old_module
						return false
					}
					has := g.has_zero_sized_leading_init_slot_inner(first.typ, mut visited)
					g.tc.cur_module = old_module
					has
				}
			} else {
				if typ.name in visited {
					false
				} else {
					visited[typ.name] = true
					first := g.struct_field_at(typ.name, 0) or { return false }
					g.has_zero_sized_leading_init_slot_inner(first.typ, mut visited)
				}
			}
		}
		else {
			false
		}
	}
}

// scalar_zero_init supports scalar zero init handling for FlatGen.
fn (g &FlatGen) scalar_zero_init(c_type string) string {
	if c_type in ['float', 'double'] {
		return '0.0'
	}
	return '0'
}

// StructDeclInfo stores struct decl info metadata used by c.
struct StructDeclInfo {
	node      flat.Node
	module    string
	full_name string
}

// struct_init_c_type_name supports struct init c type name handling for FlatGen.
// generic_struct_init_instance_ct returns the concrete-instance C type name for a
// bare generic struct literal whose type args are pinned by the surrounding expected
// type (e.g. `Vec4{..}` written where a `Vec4[f32]` is expected). Returns none when
// the literal is already specialized, the base is not a generic struct, or the
// expected type is not a matching concrete instance.
fn (g &FlatGen) generic_struct_init_instance_ct(type_name string) ?string {
	if type_name.contains('[') {
		return none
	}
	short := type_name.all_after_last('.')
	// Prefer the explicit expected type; fall back to the enclosing function's return
	// type, since a bare generic literal in return position carries no
	// expected_expr_type. Only adopt a candidate whose generic base matches the
	// literal's base, so unrelated expected types never rename the struct.
	//
	// Note: `tc.struct_generic_params` is empty by cgen time, so the candidate's
	// shape (a `Base[args]` instance whose base short-name equals the literal's) is
	// the sole evidence that this bare literal is a generic struct instantiation.
	for cand in [g.expected_expr_type, g.cur_fn_ret] {
		cand_name := types.Type(cand).name()
		if !cand_name.contains('[') {
			continue
		}
		cand_base := cand_name.all_before('[')
		if cand_base.len == 0 || cand_base.all_after_last('.') != short {
			continue
		}
		return g.tc.c_type(cand)
	}
	return none
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

// find_struct_decl resolves find struct decl information for c.
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

// struct_field_type supports struct field type handling for FlatGen.
fn (g &FlatGen) struct_field_type(type_name string, field_name string) ?types.Type {
	if fields := g.tc.structs[type_name] {
		for f in fields {
			if f.name == field_name {
				return f.typ
			}
		}
	}
	qname := g.tc.qualify_name(type_name)
	if fields := g.tc.structs[qname] {
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

// precompute_embedded_fields records, per struct type, only its embedded fields (those
// whose field name is the embedded type name). Most structs have none. Done once so the
// per-selector embedded-field resolution doesn't rescan (and re-c_name) every field of
// the receiver struct on every field access — a major cgen cost after #27538.
fn (mut g FlatGen) precompute_embedded_fields() {
	for type_name, fields in g.tc.structs {
		mut emb := []types.StructField{}
		for field in fields {
			if g.embedded_field_type_name(field).len > 0 {
				emb << field
			}
		}
		g.embedded_fields_by_type[type_name] = emb
	}
}

// struct_embedded_fields returns the embedded fields of a type (mirrors
// struct_fields_for_type's key resolution against the precomputed map). Returns an empty
// slice for non-embedding structs, which is the common case.
fn (g &FlatGen) struct_embedded_fields(type_name string) []types.StructField {
	if emb := g.embedded_fields_by_type[type_name] {
		return emb
	}
	qname := g.tc.qualify_name(type_name)
	if emb := g.embedded_fields_by_type[qname] {
		return emb
	}
	if info := g.find_struct_decl(type_name) {
		if emb := g.embedded_fields_by_type[info.full_name] {
			return emb
		}
	}
	if type_name.contains('.') {
		short_name := type_name.all_after_last('.')
		if emb := g.embedded_fields_by_type[short_name] {
			return emb
		}
	}
	return []
}

fn (g &FlatGen) struct_fields_for_type(type_name string) ?[]types.StructField {
	if fields := g.tc.structs[type_name] {
		return fields
	}
	qname := g.tc.qualify_name(type_name)
	if fields := g.tc.structs[qname] {
		return fields
	}
	if info := g.find_struct_decl(type_name) {
		if fields := g.tc.structs[info.full_name] {
			return fields
		}
	}
	if type_name.contains('.') {
		short_name := type_name.all_after_last('.')
		if fields := g.tc.structs[short_name] {
			return fields
		}
	}
	return none
}

fn (g &FlatGen) embedded_field_type_name(field types.StructField) string {
	clean_type := types.unwrap_pointer(field.typ)
	field_type_name := clean_type.name()
	if field_type_name.len == 0 {
		return ''
	}
	mut names := [field_type_name]
	base_name := types.generic_base_name(field_type_name)
	if base_name != field_type_name {
		names << base_name
	}
	short_field := if field.name.contains('.') { field.name.all_after_last('.') } else { field.name }
	for name in names {
		short_type := if name.contains('.') { name.all_after_last('.') } else { name }
		if field.name == name || short_field == short_type || c_name(field.name) == c_name(name) {
			return field_type_name
		}
	}
	return ''
}

fn (g &FlatGen) direct_struct_field_exists(type_name string, field_name string) bool {
	fields := g.struct_fields_for_type(type_name) or { return false }
	for field in fields {
		if field.name == field_name {
			return true
		}
	}
	return false
}

fn (g &FlatGen) embedded_field_for_promoted_field(type_name string, field_name string) ?types.StructField {
	path := g.embedded_field_path_for_promoted_field(type_name, field_name) or { return none }
	if path.len == 0 {
		return none
	}
	return path[0]
}

fn (g &FlatGen) direct_embedded_field_for_selector(base_type types.Type, field_name string) ?types.StructField {
	type_name := g.type_lookup_name(base_type)
	if type_name.len == 0 {
		return none
	}
	// Only the embedded fields (precomputed) can match — no need to scan every field.
	for field in g.struct_embedded_fields(type_name) {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name.len == 0 {
			continue
		}
		short_type := if embedded_type_name.contains('.') {
			embedded_type_name.all_after_last('.')
		} else {
			embedded_type_name
		}
		if field_name == embedded_type_name || field_name == short_type
			|| c_name(field_name) == c_name(embedded_type_name) {
			return field
		}
	}
	return none
}

fn (g &FlatGen) embedded_field_path_for_promoted_field(type_name string, field_name string) ?[]types.StructField {
	for field in g.struct_embedded_fields(type_name) {
		embedded_type_name := g.embedded_field_type_name(field)
		if embedded_type_name.len == 0 {
			continue
		}
		if g.direct_struct_field_exists(embedded_type_name, field_name) {
			return [field]
		}
		if nested := g.embedded_field_path_for_promoted_field(embedded_type_name, field_name) {
			mut path := [field]
			path << nested
			return path
		}
	}
	return none
}

fn (g &FlatGen) embedded_field_path_for_promoted_selector(base_type types.Type, field_name string) ?[]types.StructField {
	type_name := g.type_lookup_name(base_type)
	if type_name.len == 0 {
		return none
	}
	return g.embedded_field_path_for_promoted_field(type_name, field_name)
}

fn (g &FlatGen) embedded_field_for_promoted_selector(base_type types.Type, field_name string) ?types.StructField {
	type_name := g.type_lookup_name(base_type)
	if type_name.len == 0 {
		return none
	}
	return g.embedded_field_for_promoted_field(type_name, field_name)
}

fn (g &FlatGen) type_lookup_name(typ types.Type) string {
	clean_type := types.unwrap_pointer(typ)
	if clean_type is types.Alias {
		return clean_type.base_type.name()
	}
	return clean_type.name()
}

// struct_field_at supports struct field at handling for FlatGen.
fn (g &FlatGen) struct_field_at(type_name string, index int) ?types.StructField {
	if index < 0 {
		return none
	}
	if fields := g.tc.structs[type_name] {
		if index < fields.len {
			return fields[index]
		}
	}
	qname := g.tc.qualify_name(type_name)
	if fields := g.tc.structs[qname] {
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

// struct_field_type_at supports struct field type at handling for FlatGen.
fn (g &FlatGen) struct_field_type_at(type_name string, index int) ?types.Type {
	if field := g.struct_field_at(type_name, index) {
		return field.typ
	}
	return none
}

// gen_return_assoc emits return assoc output for c.
fn (mut g FlatGen) gen_return_assoc(node flat.Node) {
	tmp := g.tmp_name()
	g.gen_assoc_return_tmp(node, tmp)
	g.writeln('return ${tmp};')
}

fn (mut g FlatGen) gen_assoc_return_tmp(node flat.Node, tmp string) {
	ct := g.tc.c_type(g.tc.parse_type(node.value))
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
}

// gen_assoc_expr emits assoc expr output for c.
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

// gen_heap_assoc_expr emits heap assoc expr output for c.
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

// gen_map_init emits map init output for c.
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

// write_new_map writes new map output for c.
fn (mut g FlatGen) write_new_map(key_type types.Type, value_type types.Type) {
	mut c_key := g.tc.c_type(key_type)
	mut c_val := g.tc.c_type(value_type)
	if c_key.starts_with('fn_ptr:') {
		c_key = g.resolve_fn_ptr_type(c_key)
	}
	if c_val.starts_with('fn_ptr:') {
		c_val = g.resolve_fn_ptr_type(c_val)
	}
	hash_fn, eq_fn, clone_fn, free_fn := g.map_callback_names(key_type)
	g.write('new_map(sizeof(${c_key}), sizeof(${c_val}), ${hash_fn}, ${eq_fn}, ${clone_fn}, ${free_fn})')
}

// map_callback_names supports map callback names handling for FlatGen.
fn (g &FlatGen) map_callback_names(key_type types.Type) (string, string, string, string) {
	if key_type is types.String {
		return 'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string'
	}
	c_key := g.tc.c_type(key_type)
	size_suffix := match c_key {
		'u8', 'i8', 'bool', 'char' { '1' }
		'u16', 'i16' { '2' }
		'i64', 'u64', 'isize', 'usize', 'voidptr' { '8' }
		else { '4' }
	}

	return 'map_hash_int_${size_suffix}', 'map_eq_int_${size_suffix}', 'map_clone_int_${size_suffix}', 'map_free_nop'
}

// skip_builtin_struct supports skip builtin struct handling for FlatGen.
fn (g &FlatGen) skip_builtin_struct(name string) bool {
	_ = g
	if name.starts_with('C.') {
		return true
	}
	return false
}

// emit_interface_struct emits emit interface struct output for c.
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

// struct_decls supports struct decls handling for FlatGen.
fn (mut g FlatGen) struct_decls() {
	// Fixed-array typedefs whose element is a struct are emitted interleaved with the
	// structs below (right after the element struct is defined), so struct fields that
	// reference them resolve. Primitive-element ones were already emitted earlier.
	fixed_array_needed := g.collect_fixed_array_typedefs_needed()
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
	if g.has_builtins && 'array' in remaining {
		g.emit_struct('array')
		emitted['array'] = true
		emitted['Array'] = true
		remaining.delete('array')
		remaining_cnames.delete('array')
	}
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
				g.emit_ready_fixed_array_typedefs(fixed_array_needed, emitted)
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

// type_forward_decls returns type forward decls data for FlatGen.
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

// emit_struct emits emit struct output for c.
fn (mut g FlatGen) emit_struct(name string) {
	old_module := g.tc.cur_module
	g.tc.cur_module = module_from_qualified_name(name)
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
	g.tc.cur_module = old_module
}

// is_generic_struct reports whether is generic struct applies in c.
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

// write_struct_field writes struct field output for c.
fn (mut g FlatGen) write_struct_field(_struct_name string, f types.StructField) {
	if f.typ is types.Void {
		g.writeln('\tint ${c_name(f.name)};')
		return
	}
	mut field_type := f.typ
	if f.typ is types.Alias {
		field_type = f.typ.base_type
	}
	raw_field_type := field_type
	if field_type is types.FnType {
		ct := g.resolve_fn_ptr_type(g.tc.c_type(raw_field_type))
		g.writeln('\t${ct} ${c_name(f.name)};')
	} else if f.typ is types.ArrayFixed {
		c_elem, dims := g.fixed_array_decl_parts(f.typ)
		g.writeln('\t${c_elem} ${c_name(f.name)}${dims};')
	} else {
		mut ct := if f.typ is types.OptionType || f.typ is types.ResultType {
			g.optional_type_name(f.typ)
		} else {
			g.tc.c_type(f.typ)
		}
		if ct.starts_with('fn_ptr:') {
			ct = g.resolve_fn_ptr_type(ct)
		}
		if ct == 'void' {
			ct = 'int'
		}
		g.writeln('\t${ct} ${c_name(f.name)};')
	}
}

// preseed_struct_fn_ptr_types supports preseed struct fn ptr types handling for FlatGen.
fn (mut g FlatGen) preseed_struct_fn_ptr_types() {
	for _, fields in g.tc.structs {
		for f in fields {
			g.preseed_fn_ptr_type(f.typ)
		}
	}
}

fn (mut g FlatGen) preseed_global_fn_ptr_types() {
	for _, typ in g.global_types {
		g.preseed_fn_ptr_type(typ)
	}
}

fn (mut g FlatGen) preseed_fn_ptr_type(typ types.Type) {
	if typ is types.Alias {
		g.preseed_fn_ptr_type(typ.base_type)
		return
	}
	if typ is types.FnType {
		ct := g.tc.c_type(typ)
		g.resolve_fn_ptr_type(ct)
		for param in typ.params {
			g.preseed_fn_ptr_type(param)
		}
		g.preseed_fn_ptr_type(typ.return_type)
		return
	}
	if typ is types.Array {
		g.preseed_fn_ptr_type(typ.elem_type)
		return
	}
	if typ is types.ArrayFixed {
		g.preseed_fn_ptr_type(typ.elem_type)
		return
	}
	if typ is types.Map {
		g.preseed_fn_ptr_type(typ.key_type)
		g.preseed_fn_ptr_type(typ.value_type)
		return
	}
	if typ is types.Pointer {
		g.preseed_fn_ptr_type(typ.base_type)
		return
	}
	if typ is types.OptionType {
		g.optional_type_name(typ)
		g.preseed_fn_ptr_type(typ.base_type)
		return
	}
	if typ is types.ResultType {
		g.optional_type_name(typ)
		g.preseed_fn_ptr_type(typ.base_type)
		return
	}
	if typ is types.MultiReturn {
		for item in typ.types {
			g.preseed_fn_ptr_type(item)
		}
	}
}

// emit_struct_option_typedefs emits emit struct option typedefs output for c.
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
