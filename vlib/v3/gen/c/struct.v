module c

import v3.flat
import v3.gen.c.naming
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

// struct_init_fields_key returns the key under which the initialized struct's checked fields
// (and their concrete types) live. For a bare generic literal that adopts a concrete instance
// (`Box{..}` where `Box[int]` is expected) that is the instance key `Box[int]`; the bare `Box`
// entry is removed by monomorphization, so field-type lookups and omitted default-field
// emission must use the instance key or they miss fields like `items []T` (leaving invalid
// zeroed array/map metadata). Falls back to the given key for non-generic structs.
fn (g &FlatGen) struct_init_fields_key(type_name string, fallback string) string {
	if inst := g.generic_struct_init_instance_name(type_name) {
		if inst in g.tc.structs {
			return inst
		}
	}
	// Resolve a module-local alias before consulting an unqualified short-name
	// fallback. Otherwise `sgl_like.Context = C.sgl_context` can borrow omitted
	// defaults from an unrelated imported `gg.Context`.
	if info := g.find_struct_decl(type_name) {
		if info.full_name in g.tc.structs {
			return info.full_name
		}
	}
	if fallback in g.tc.structs {
		return fallback
	}
	if type_name.contains('[') {
		ct := g.tc.c_type(g.tc.parse_type(type_name))
		for candidate, _ in g.tc.structs {
			if !candidate.contains('[') {
				continue
			}
			if g.tc.c_type(g.tc.parse_type(candidate)) == ct {
				return candidate
			}
		}
	}
	return fallback
}

fn (g &FlatGen) struct_init_lookup_type_name(type_name string) string {
	typ := g.tc.parse_type(type_name)
	if typ is types.Struct {
		return typ.name
	}
	return type_name
}

fn (mut g FlatGen) gen_struct_field_expr(value_id flat.NodeId, expected types.Type) {
	if call_name := g.callback_direct_fn_value_name(value_id, expected) {
		g.write(g.callback_c_fn_name(call_name))
		return
	}
	if g.gen_callback_fn_value_for_expected_type(value_id, expected) {
		return
	}
	if g.gen_pointer_value_struct_field(value_id, expected) {
		return
	}
	if g.gen_optional_arg(value_id, expected) {
		return
	}
	g.gen_expr_with_expected_type(value_id, expected)
}

fn (mut g FlatGen) gen_pointer_value_struct_field(value_id flat.NodeId, expected types.Type) bool {
	actual := g.tc.resolve_type(value_id)
	expected0 := if expected is types.Alias { expected.base_type } else { expected }
	if expected0 is types.Pointer {
		return false
	}
	if actual is types.Pointer {
		if g.type_names_match(actual.base_type, expected0) {
			value := g.a.node(value_id)
			if value.kind == .ident {
				g.write('*')
				g.gen_expr(value_id)
			} else {
				g.write('*(')
				g.gen_expr(value_id)
				g.write(')')
			}
			return true
		}
	}
	if pointer_value_type_names_match(actual.name(), expected0.name()) {
		value := g.a.node(value_id)
		if value.kind == .ident {
			g.write('*')
			g.gen_expr(value_id)
		} else {
			g.write('*(')
			g.gen_expr(value_id)
			g.write(')')
		}
		return true
	}
	return false
}

fn (mut g FlatGen) gen_struct_field_expr_for_field(value_id flat.NodeId, struct_name string, field_name string, expected types.Type) {
	if g.gen_embed_file_uncompressed_field(value_id, struct_name, field_name) {
		return
	}
	if c_abi_fn := g.struct_field_c_abi_fn_ptr_type(struct_name, field_name) {
		if g.gen_callback_fn_value_for_field_c_abi(value_id, expected, c_abi_fn) {
			return
		}
	}
	if g.gen_shared_field_expr_for_field(value_id, struct_name, field_name, expected) {
		return
	}
	g.gen_struct_field_expr(value_id, expected)
}

fn (mut g FlatGen) gen_embed_file_uncompressed_field(value_id flat.NodeId, struct_name string, field_name string) bool {
	if field_name != 'uncompressed' || struct_name != 'embed_file.EmbedFileData' {
		return false
	}
	mut data_id := value_id
	value := g.a.node(value_id)
	if value.kind == .cast_expr && value.value == '&u8' && value.children_count > 0 {
		data_id = g.a.child(value, 0)
	}
	data := g.a.node(data_id)
	if data.kind != .string_literal {
		return false
	}
	g.write('(u8*)"${c_byte_string_escape(data.value)}"')
	return true
}

fn default_init_unalias_type(typ types.Type) types.Type {
	if typ is types.Alias {
		return default_init_unalias_type(typ.base_type)
	}
	return typ
}

fn (mut g FlatGen) gen_unset_struct_field_default(struct_name string, field_name string, field_type types.Type, field_c_name string, has_field bool) bool {
	mut has := has_field
	if g.shared_field_info(struct_name, field_name) != none {
		if has {
			g.write(', ')
		}
		g.write('.${field_c_name} = ')
		g.gen_shared_default_value_for_field(struct_name, field_name, field_type)
		return true
	}
	clean_type := default_init_unalias_type(field_type)
	if clean_type is types.Map {
		if has {
			g.write(', ')
		}
		g.write('.${field_c_name} = ')
		g.write_new_map(clean_type.key_type, clean_type.value_type)
		return true
	}
	if clean_type is types.Array {
		c_elem := g.tc.c_type(clean_type.elem_type)
		if has {
			g.write(', ')
		}
		g.write('.${field_c_name} = array_new(sizeof(${c_elem}), 0, 0)')
		return true
	}
	if clean_type is types.String {
		if has {
			g.write(', ')
		}
		g.write('.${field_c_name} = ')
		g.gen_default_value_for_type(clean_type)
		return true
	}
	if clean_type is types.Enum {
		if has {
			g.write(', ')
		}
		g.write('.${field_c_name} = ')
		g.gen_default_value_for_type(clean_type)
		return true
	}
	if clean_type is types.SumType {
		if has {
			g.write(', ')
		}
		g.write('.${field_c_name} = ')
		g.gen_default_value_for_type(clean_type)
		return true
	}
	if g.field_needs_default_init(clean_type) {
		if has {
			g.write(', ')
		}
		g.write('.${field_c_name} = ')
		g.gen_default_value_for_type(clean_type)
		return true
	}
	return has
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
	if inst := g.generic_struct_init_instance_ct_for_node(node) {
		name = inst
	}
	init_type := g.tc.parse_type(node.value)
	is_optional_init := node.value == 'Optional' || init_type is types.OptionType
		|| init_type is types.ResultType
	has_expected_optional := g.expected_expr_type is types.OptionType
		|| g.expected_expr_type is types.ResultType || g.expected_expr_is_optional_struct()
	if node.value == 'Optional'
		&& (g.expected_expr_type is types.OptionType || g.expected_expr_type is types.ResultType) {
		name = g.optional_type_name(g.expected_expr_type)
	} else if node.value == 'Optional' && g.expected_expr_is_optional_struct() {
		name = g.value_c_type(g.expected_expr_type)
	}
	if is_optional_init && !has_expected_optional
		&& g.name_uses_specialized_generic_abi(g.cur_fn_name) {
		name = g.fn_return_type_name(g.cur_fn_ret)
	}
	// A bare generic literal stores its fields under the concrete instance key (`Box[int]`);
	// the bare `node.value` (`Box`) entry is removed by monomorphization, so resolve the
	// instance for the fixed-array-field test, field-type lookups, and omitted-default emission.
	lookup_source_name := if is_optional_init {
		''
	} else {
		g.struct_init_lookup_type_name(node.value)
	}
	lookup_name := if is_optional_init {
		''
	} else {
		g.struct_init_fields_key(lookup_source_name, lookup_source_name)
	}
	if node.children_count == 0 && g.is_scalar_zero_init_type(lookup_source_name, name) {
		g.write(g.scalar_zero_init(name))
		return
	}
	if !g.is_interface_type_name(node.value)
		&& g.struct_init_has_fixed_array_field(node, lookup_name) {
		g.gen_struct_init_with_fixed_array_fields(node, name, init_module)
		return
	}
	g.write('(${name}){')
	mut allowed_fields := map[string]bool{}
	if fields := g.struct_fields_for_type(lookup_name) {
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
			if sf := g.struct_field_at(lookup_name, i) {
				if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(lookup_name, sf.name,
					value_id)
				{
					inner_ct := g.value_c_type(heap_copy_type)
					g.write('(${inner_ct}*)memdup(')
					g.gen_expr(value_id)
					g.write(', sizeof(${inner_ct}))')
				} else {
					g.gen_struct_field_expr_for_field(value_id, lookup_name, sf.name, sf.typ)
				}
				set_fields[sf.name] = true
			} else {
				g.gen_expr(value_id)
			}
		} else {
			g.write('.${c_field_name(field.value)} = ')
			if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(lookup_name, field.value,
				value_id)
			{
				inner_ct := g.value_c_type(heap_copy_type)
				g.write('(${inner_ct}*)memdup(')
				g.gen_expr(value_id)
				g.write(', sizeof(${inner_ct}))')
			} else {
				if ftyp := g.struct_field_type(lookup_name, field.value) {
					if g.struct_field_value_is_plainly_incompatible(value_id, ftyp) {
						g.gen_default_value_for_type(ftyp)
					} else {
						g.gen_struct_field_expr_for_field(value_id, lookup_name, field.value, ftyp)
					}
				} else {
					g.gen_expr(value_id)
				}
			}
			set_fields[field.value] = true
		}
		has_field = true
	}
	if is_optional_init {
		g.write('}')
		return
	}
	after_fields_module := g.tc.cur_module
	g.tc.cur_module = init_module
	sname := g.struct_init_resolved_decl_name(lookup_source_name)
	g.tc.cur_module = after_fields_module
	has_field = g.gen_struct_default_fields(sname, mut set_fields, has_field)
	defaults_key := if lookup_name in g.tc.structs { lookup_name } else { sname }
	if defaults_key in g.tc.structs {
		for f in g.tc.structs[defaults_key] {
			if f.name in set_fields {
				continue
			}
			has_field = g.gen_unset_struct_field_default(defaults_key, f.name, f.typ,
				c_field_name(f.name), has_field)
		}
	}
	g.write('}')
}

fn (mut g FlatGen) struct_init_has_fixed_array_field(node flat.Node, type_name string) bool {
	for i in 0 .. node.children_count {
		field := g.a.child_node(&node, i)
		if field.value.len == 0 {
			if sf := g.struct_field_at(type_name, i) {
				if _ := array_fixed_type(sf.typ) {
					return true
				}
			}
			continue
		}
		if ftyp := g.struct_field_type(type_name, field.value) {
			if _ := array_fixed_type(ftyp) {
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
	// A bare generic literal stores its fields under the concrete instance key (`Box[int]`);
	// the bare `node.value` (`Box`) entry is removed by monomorphization, so resolve the
	// instance for the field lookups and omitted-default emission below.
	lookup_name := g.struct_init_fields_key(node.value, node.value)
	mut allowed_fields := map[string]bool{}
	if fields := g.struct_fields_for_type(lookup_name) {
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
			if sf := g.struct_field_at(lookup_name, i) {
				if _ := array_fixed_type(sf.typ) {
					if g.shared_field_info(lookup_name, sf.name) != none {
						// A shared fixed array is pointer-backed wrapper storage, not an inline
						// array field in the containing struct.
					} else {
						fixed_fields << sf.name
						fixed_values << value_id
						fixed_field_types << sf.typ
						set_fields[sf.name] = true
						continue
					}
				}
				if has_field {
					g.write(', ')
				}
				g.write('.${c_field_name(sf.name)} = ')
				g.gen_struct_field_expr_for_field(value_id, lookup_name, sf.name, sf.typ)
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
			ftyp := g.struct_field_type(lookup_name, field.value) or { types.Type(types.void_) }
			if _ := array_fixed_type(ftyp) {
				if g.shared_field_info(lookup_name, field.value) != none {
					// Emit through gen_shared_field_expr_for_field below.
				} else {
					fixed_fields << field.value
					fixed_values << value_id
					fixed_field_types << ftyp
					set_fields[field.value] = true
					continue
				}
			}
			if has_field {
				g.write(', ')
			}
			g.write('.${c_field_name(field.value)} = ')
			if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(lookup_name, field.value,
				value_id)
			{
				inner_ct := g.value_c_type(heap_copy_type)
				g.write('(${inner_ct}*)memdup(')
				g.gen_expr(value_id)
				g.write(', sizeof(${inner_ct}))')
			} else if ftyp !is types.Void {
				if g.struct_field_value_is_plainly_incompatible(value_id, ftyp) {
					g.gen_default_value_for_type(ftyp)
				} else {
					g.gen_struct_field_expr_for_field(value_id, lookup_name, field.value, ftyp)
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
	sname := g.struct_init_resolved_decl_name(node.value)
	g.tc.cur_module = after_fields_module
	has_field = g.gen_struct_default_fields(sname, mut set_fields, has_field)
	defaults_key := if lookup_name in g.tc.structs { lookup_name } else { sname }
	if defaults_key in g.tc.structs {
		for f in g.tc.structs[defaults_key] {
			if f.name in set_fields {
				continue
			}
			has_field = g.gen_unset_struct_field_default(defaults_key, f.name, f.typ,
				c_field_name(f.name), has_field)
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
		if fixed := array_fixed_type(field_type) {
			literal := g.fixed_array_compound_literal_expr(value_id, fixed)
			if trimmed_space(literal).len > 0 {
				g.write(literal)
				return
			}
			c_elem, dims := g.fixed_array_decl_parts(fixed)
			g.write('(${c_elem}${dims})')
		} else {
			g.write('(${g.tc.c_type(field_type)})')
		}
		g.gen_expr(value_id)
		return
	}
	if val_node.kind in [.cast_expr, .as_expr] && val_node.children_count > 0 {
		child_id := g.a.child(val_node, 0)
		child := g.a.node(child_id)
		if child.kind == .array_literal {
			g.gen_fixed_array_copy_source(child_id, field_type)
			return
		}
		if child.kind == .postfix && child.children_count > 0 {
			post_child_id := g.a.child(child, 0)
			if g.a.node(post_child_id).kind == .array_literal {
				g.gen_fixed_array_copy_source(post_child_id, field_type)
				return
			}
		}
	}
	if val_node.kind == .paren && val_node.children_count > 0 {
		g.gen_fixed_array_copy_source(g.a.child(val_node, 0), field_type)
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
			inner_ct := g.value_c_type(inner_type)
			child_type := g.tc.resolve_type(child_id)
			g.write('(${inner_ct}*)memdup(')
			if child_type is types.Pointer && g.type_names_match(child_type.base_type, inner_type) {
				g.gen_expr(child_id)
			} else {
				g.gen_sum_variant_memdup_source(child_id, inner_type)
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
	mut name := g.struct_init_c_type_name(node.value)
	// A bare generic heap literal (`&Vec4{..}`) carries no type args; when the
	// surrounding expected type fixes them (e.g. a `&Vec4[f32]` return), emit the
	// concrete instance name so the materialized struct matches the value path.
	if inst := g.generic_struct_init_instance_ct_for_node(node) {
		name = inst
	}
	sum_name := g.resolve_sum_name(node.value)
	is_sum_literal := sum_name in g.tc.sum_types
	// A bare generic literal stores its fields under the concrete instance key (`Box[int]`);
	// the bare `node.value` (`Box`) entry is removed by monomorphization, so resolve the
	// instance for the fixed-array-field test, field-type lookups, and omitted-default emission.
	lookup_name := g.struct_init_fields_key(node.value, node.value)
	if !is_sum_literal && !g.is_interface_type_name(node.value)
		&& g.struct_init_has_fixed_array_field(node, lookup_name) {
		// Fixed-array fields can't be set in the `&(T){...}` compound literal; build
		// via a temp + memcpy and memdup the result.
		g.gen_struct_init_with_fixed_array_fields_impl(node, name, init_module, true)
		return
	}
	g.write('(${name}*)memdup(&(${name}){')
	mut allowed_fields := map[string]bool{}
	if fields := g.struct_fields_for_type(lookup_name) {
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
			// Positional initializer (empty field name): emit a positional C value mapped
			// to the field at this index (mirrors gen_struct_init); a `. = v` designator
			// is invalid C.
			if sf := g.struct_field_at(lookup_name, i) {
				if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(lookup_name, sf.name,
					value_id)
				{
					inner_ct := g.value_c_type(heap_copy_type)
					g.write('(${inner_ct}*)memdup(')
					g.gen_expr(value_id)
					g.write(', sizeof(${inner_ct}))')
				} else {
					g.gen_struct_field_expr_for_field(value_id, lookup_name, sf.name, sf.typ)
				}
				set_fields[sf.name] = true
			} else {
				g.gen_expr(value_id)
			}
			has_field = true
			continue
		}
		g.write('.${c_field_name(field.value)} = ')
		if is_sum_literal {
			g.gen_lowered_sum_field_value(sum_name, field)
		} else if heap_copy_type := g.heap_copy_type_for_sum_pointer_field(lookup_name,
			field.value, value_id)
		{
			inner_ct := g.value_c_type(heap_copy_type)
			g.write('(${inner_ct}*)memdup(')
			g.gen_expr(value_id)
			g.write(', sizeof(${inner_ct}))')
		} else {
			if ftyp := g.struct_field_type(lookup_name, field.value) {
				if g.struct_field_value_is_plainly_incompatible(value_id, ftyp) {
					g.gen_default_value_for_type(ftyp)
				} else {
					g.gen_struct_field_expr_for_field(value_id, lookup_name, field.value, ftyp)
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
	sname := g.struct_init_resolved_decl_name(node.value)
	g.tc.cur_module = after_fields_module
	has_field = g.gen_struct_default_fields(sname, mut set_fields, has_field)
	defaults_key := if lookup_name in g.tc.structs { lookup_name } else { sname }
	if defaults_key in g.tc.structs {
		for f in g.tc.structs[defaults_key] {
			if f.name in set_fields {
				continue
			}
			has_field = g.gen_unset_struct_field_default(defaults_key, f.name, f.typ,
				c_field_name(f.name), has_field)
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
	old_file := g.tc.cur_file
	g.tc.cur_module = info.module
	g.tc.cur_file = info.file
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind != .field_decl || field.children_count == 0 || field.value in set_fields {
			continue
		}
		if has {
			g.write(', ')
		}
		g.write('.${g.cname(field.value)} = ')
		g.gen_struct_field_expr_for_field(g.a.child(field, 0), info.full_name, field.value, g.struct_default_field_type(info,
			field))
		set_fields[field.value] = true
		has = true
	}
	g.tc.cur_module = old_module
	g.tc.cur_file = old_file
	return has
}

fn (mut g FlatGen) struct_default_field_type(info StructDeclInfo, field flat.Node) types.Type {
	if field.typ.len > 0 && !field.typ.contains('.') && info.module.len > 0 && info.module != 'main'
		&& info.module != 'builtin' {
		qtyp := '${info.module}.${field.typ}'
		if qtyp in g.tc.enum_names || qtyp in g.tc.structs || qtyp in g.tc.sum_types
			|| qtyp in g.tc.interface_names {
			return g.tc.parse_type(qtyp)
		}
	}
	return g.tc.parse_type(field.typ)
}

// gen_default_value_for_type emits default value for type output for c.
fn (mut g FlatGen) gen_default_value_for_type(typ types.Type) {
	clean_typ := default_init_unalias_type(typ)
	if clean_typ is types.Map {
		g.write_new_map(clean_typ.key_type, clean_typ.value_type)
		return
	}
	if clean_typ is types.Array {
		c_elem := g.value_c_type(clean_typ.elem_type)
		g.write('array_new(sizeof(${c_elem}), 0, 0)')
		return
	}
	if clean_typ is types.String {
		sid := g.intern_string('')
		g.write('_str_${sid}')
		return
	}
	if clean_typ is types.Enum {
		if expr := g.enum_default_value_expr_for_type(clean_typ.name) {
			g.write(expr)
		} else {
			g.write('0')
		}
		return
	}
	if clean_typ is types.SumType {
		sum_name := g.resolve_sum_name(clean_typ.name)
		variants := g.tc.sum_types[sum_name] or { []string{} }
		if variants.len > 0 {
			variant := variants[0]
			variant_type := g.tc.parse_type(variant)
			ct := g.value_c_type(clean_typ)
			inner_ct := g.value_c_type(variant_type)
			g.write('(${ct}){.typ = ${g.sum_type_index(sum_name, variant)}, .${g.sum_field_name(variant)} = (${inner_ct}*)memdup(&(${inner_ct}[]){')
			g.gen_default_value_for_type(variant_type)
			g.write('}, sizeof(${inner_ct}))}')
			return
		}
	}
	raw_typ := clean_typ
	if clean_typ is types.OptionType || clean_typ is types.ResultType {
		ct := g.optional_type_name(clean_typ)
		g.write('(${ct}){0}')
		return
	}
	if clean_typ is types.Struct && !clean_typ.name.starts_with('C.') {
		ct := g.tc.c_type(raw_typ)
		g.write('(${ct}){')
		mut set_fields := map[string]bool{}
		mut has_field := g.gen_struct_default_fields(clean_typ.name, mut set_fields, false)
		mut sname := g.tc.qualify_name(clean_typ.name)
		if clean_typ.name in g.tc.structs {
			sname = clean_typ.name
		}
		if sname in g.tc.structs {
			for f in g.tc.structs[sname] {
				if f.name in set_fields {
					continue
				}
				has_field = g.gen_unset_struct_field_default(sname, f.name, f.typ, g.cname(f.name),
					has_field)
			}
		}
		g.write('}')
		return
	}
	ct := g.value_c_type(clean_typ)
	if g.is_scalar_c_type(ct) {
		g.write(g.scalar_zero_init(ct))
		return
	}
	g.write('(${ct}){0}')
}

fn (g &FlatGen) enum_default_value_expr_for_type(type_name string) ?string {
	fields := g.enum_fields_for_type(type_name) or { return none }
	if fields.len == 0 {
		return none
	}
	if expr := g.enum_value_expr_for_type(type_name, fields[0]) {
		return expr
	}
	return '0'
}

fn (g &FlatGen) enum_fields_for_type(type_name string) ?[]string {
	if fields := g.tc.enum_fields[type_name] {
		return fields
	}
	if !type_name.contains('.') && g.tc.cur_module.len > 0 && g.tc.cur_module != 'main'
		&& g.tc.cur_module != 'builtin' {
		qname := '${g.tc.cur_module}.${type_name}'
		if fields := g.tc.enum_fields[qname] {
			return fields
		}
	}
	if !type_name.contains('.') {
		mut found := []string{}
		mut ok := false
		for ename, fields in g.tc.enum_fields {
			if ename.all_after_last('.') != type_name {
				continue
			}
			if ok {
				return none
			}
			found = fields.clone()
			ok = true
		}
		if ok {
			return found
		}
	}
	return none
}

fn (mut g FlatGen) gen_default_value_addr_for_type(typ types.Type) {
	ct := g.value_c_type(typ)
	g.write('&(${ct}[]){')
	g.gen_default_value_for_type(typ)
	g.write('}')
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
// explicitly default-initialized in a struct literal: either because the by-value
// struct has source defaults, or because its own omitted fields need runtime
// metadata defaults such as dynamic arrays/maps.
fn (mut g FlatGen) field_needs_default_init(typ types.Type) bool {
	clean_type := default_init_unalias_type(typ)
	if clean_type is types.Struct && !clean_type.name.starts_with('C.') {
		return g.struct_needs_default_init(clean_type.name)
	}
	return false
}

// struct_needs_default_init reports whether building `type_name` as a struct
// literal would set any field that C's `{0}` would not: a field with an explicit
// default (`x int = 5`), an omitted dynamic array/map, or a by-value struct field
// whose own type needs those defaults.
// Returns false for structs with interface/sum-typed field defaults, since the
// codegen default path cannot box those values.
fn (mut g FlatGen) struct_needs_default_init(type_name string) bool {
	mut visited := map[string]bool{}
	return g.struct_needs_default_init_inner(type_name, mut visited)
}

fn (mut g FlatGen) struct_needs_default_init_inner(type_name string, mut visited map[string]bool) bool {
	if type_name in visited {
		return false
	}
	visited[type_name] = true
	mut found := false
	if info := g.find_struct_decl(type_name) {
		old_module := g.tc.cur_module
		g.tc.cur_module = info.module
		for i in 0 .. info.node.children_count {
			field := g.a.child_node(&info.node, i)
			if field.kind != .field_decl || field.children_count == 0 {
				continue
			}
			ftyp := g.struct_default_field_type(info, field)
			clean_ftyp := default_init_unalias_type(ftyp)
			// Defaults for interface/sum-typed fields require boxing the value into
			// the interface/sum representation, which the codegen default path cannot
			// do. Treat the whole struct as unsafe to default-emit (leave it
			// zero-initialized, as before) rather than emit an unboxed value.
			if clean_ftyp is types.SumType || clean_ftyp is types.Interface {
				g.tc.cur_module = old_module
				return false
			}
			found = true
		}
		g.tc.cur_module = old_module
	}
	fields := g.struct_fields_for_type(type_name) or { return found }
	for field in fields {
		clean_ftyp := default_init_unalias_type(field.typ)
		if clean_ftyp is types.Array || clean_ftyp is types.Map {
			found = true
			continue
		}
		if clean_ftyp is types.String || clean_ftyp is types.Enum {
			found = true
			continue
		}
		if clean_ftyp is types.Struct && !clean_ftyp.name.starts_with('C.')
			&& g.struct_needs_default_init_inner(clean_ftyp.name, mut visited) {
			found = true
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
			g.write('.${g.cname(field.value)} = ')
			if ftyp := g.struct_field_type(typ.name, field.value) {
				g.gen_struct_field_expr_for_field(g.a.child(field, 0), typ.name, field.value, ftyp)
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
				has_field = g.gen_unset_struct_field_default(sname, f.name, f.typ, g.cname(f.name),
					has_field)
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
	file      string
	full_name string
}

struct SoaFieldInfo {
	name           string
	soa_name       string
	c_type         string
	is_fixed_array bool
}

struct SharedFieldInfo {
	inner   string
	wrapper string
	module  string
}

struct SharedTypeInfo {
	inner  string
	module string
}

fn shared_inner_type_text(raw string) ?string {
	clean := trimmed_space(raw)
	if clean.starts_with('shared ') {
		return trimmed_space(clean[7..])
	}
	return none
}

fn decl_assign_is_shared_marker(value string) bool {
	return value == 'shared' || value.starts_with('shared:')
}

fn shared_generic_app_parts(typ string) (string, []string, bool) {
	if typ.starts_with('fn(') || typ.starts_with('fn (') {
		return '', []string{}, false
	}
	bracket := typ.index_u8(`[`)
	if bracket <= 0 {
		return '', []string{}, false
	}
	bracket_end := shared_generic_matching_bracket(typ, bracket)
	if bracket_end <= bracket || bracket_end >= typ.len {
		return '', []string{}, false
	}
	return typ[..bracket], shared_split_generic_args(typ[bracket + 1..bracket_end]), true
}

fn shared_generic_matching_bracket(s string, start int) int {
	mut depth := 0
	for i in start .. s.len {
		if s[i] == `[` {
			depth++
		} else if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

fn shared_split_generic_args(s string) []string {
	mut parts := []string{}
	mut bracket_depth := 0
	mut paren_depth := 0
	mut start := 0
	for i in 0 .. s.len {
		match s[i] {
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
			}
			`,` {
				if bracket_depth == 0 && paren_depth == 0 {
					parts << trimmed_space(s[start..i])
					start = i + 1
				}
			}
			else {}
		}
	}
	parts << trimmed_space(s[start..])
	return parts
}

fn substitute_shared_generic_type_text(typ string, params []string, args []string) string {
	clean := trimmed_space(typ)
	if clean.len == 0 || args.len == 0 {
		return typ
	}
	for i, param in params {
		if clean == param {
			if i < args.len {
				return args[i]
			}
			return clean
		}
	}
	if clean.starts_with('&') {
		return '&' + substitute_shared_generic_type_text(clean[1..], params, args)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + substitute_shared_generic_type_text(clean[4..], params, args)
	}
	if clean.starts_with('?') {
		return '?' + substitute_shared_generic_type_text(clean[1..], params, args)
	}
	if clean.starts_with('!') {
		return '!' + substitute_shared_generic_type_text(clean[1..], params, args)
	}
	if clean.starts_with('...') {
		return '...' + substitute_shared_generic_type_text(clean[3..], params, args)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + substitute_shared_generic_type_text(clean[7..], params, args)
	}
	if clean.starts_with('[]') {
		return '[]' + substitute_shared_generic_type_text(clean[2..], params, args)
	}
	if clean.starts_with('map[') {
		bracket_end := shared_generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := substitute_shared_generic_type_text(clean[4..bracket_end], params, args)
			val := substitute_shared_generic_type_text(clean[bracket_end + 1..], params, args)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := shared_generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] +
				substitute_shared_generic_type_text(clean[bracket_end + 1..], params, args)
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') && clean.contains(',') {
		mut parts := []string{}
		for part in shared_split_generic_args(clean[1..clean.len - 1]) {
			parts << substitute_shared_generic_type_text(part, params, args)
		}
		return '(' + parts.join(', ') + ')'
	}
	base, nested_args, ok := shared_generic_app_parts(clean)
	if ok {
		mut resolved_args := []string{}
		for arg in nested_args {
			resolved_args << substitute_shared_generic_type_text(arg, params, args)
		}
		return '${base}[${resolved_args.join(', ')}]'
	}
	return clean
}

fn shared_type_text_uses_generic_params(typ string, params []string) bool {
	if params.len == 0 {
		return false
	}
	clean := trimmed_space(typ)
	if clean.len == 0 {
		return false
	}
	for param in params {
		if clean == param {
			return true
		}
	}
	if clean.starts_with('&') {
		return shared_type_text_uses_generic_params(clean[1..], params)
	}
	if clean.starts_with('mut ') {
		return shared_type_text_uses_generic_params(clean[4..], params)
	}
	if clean.starts_with('?') || clean.starts_with('!') {
		return shared_type_text_uses_generic_params(clean[1..], params)
	}
	if clean.starts_with('...') {
		return shared_type_text_uses_generic_params(clean[3..], params)
	}
	if clean.starts_with('shared ') {
		return shared_type_text_uses_generic_params(clean[7..], params)
	}
	if clean.starts_with('atomic ') {
		return shared_type_text_uses_generic_params(clean[7..], params)
	}
	if clean.starts_with('chan ') {
		return shared_type_text_uses_generic_params(clean[5..], params)
	}
	if clean.starts_with('thread ') {
		return shared_type_text_uses_generic_params(clean[7..], params)
	}
	if clean.starts_with('[]') {
		return shared_type_text_uses_generic_params(clean[2..], params)
	}
	if clean.starts_with('map[') {
		bracket_end := shared_generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			return shared_type_text_uses_generic_params(clean[4..bracket_end], params)
				|| shared_type_text_uses_generic_params(clean[bracket_end + 1..], params)
		}
	}
	if clean.starts_with('[') {
		bracket_end := shared_generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return shared_type_text_uses_generic_params(clean[bracket_end + 1..], params)
		}
	}
	if clean.starts_with('(') && clean.ends_with(')') {
		for part in shared_split_generic_args(clean[1..clean.len - 1]) {
			if shared_type_text_uses_generic_params(part, params) {
				return true
			}
		}
		return false
	}
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		params_start := clean.index_u8(`(`) + 1
		mut depth := 1
		mut params_end := params_start
		for params_end < clean.len {
			if clean[params_end] == `(` {
				depth++
			} else if clean[params_end] == `)` {
				depth--
				if depth == 0 {
					break
				}
			}
			params_end++
		}
		if params_end < clean.len {
			for part in shared_split_generic_args(clean[params_start..params_end]) {
				if shared_type_text_uses_generic_params(shared_fn_param_type_text(part), params) {
					return true
				}
			}
			return shared_type_text_uses_generic_params(clean[params_end + 1..], params)
		}
	}
	base, args, ok := shared_generic_app_parts(clean)
	if ok {
		if shared_type_text_uses_generic_params(base, params) {
			return true
		}
		for arg in args {
			if shared_type_text_uses_generic_params(arg, params) {
				return true
			}
		}
	}
	return false
}

fn shared_fn_param_type_text(param string) string {
	clean := trimmed_space(param)
	if clean.starts_with('mut ') {
		return clean[4..]
	}
	parts := clean.split(' ')
	if parts.len > 1 {
		return parts[parts.len - 1]
	}
	return clean
}

fn (g &FlatGen) shared_qualify_type_text(typ string, module_name string) string {
	clean := trimmed_space(typ)
	if clean.len == 0 {
		return typ
	}
	if clean.starts_with('&') {
		return '&' + g.shared_qualify_type_text(clean[1..], module_name)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + g.shared_qualify_type_text(clean[4..], module_name)
	}
	if clean.starts_with('?') {
		return '?' + g.shared_qualify_type_text(clean[1..], module_name)
	}
	if clean.starts_with('!') {
		return '!' + g.shared_qualify_type_text(clean[1..], module_name)
	}
	if clean.starts_with('...') {
		return '...' + g.shared_qualify_type_text(clean[3..], module_name)
	}
	if clean.starts_with('shared ') {
		return 'shared ' + g.shared_qualify_type_text(clean[7..], module_name)
	}
	if clean.starts_with('[]') {
		return '[]' + g.shared_qualify_type_text(clean[2..], module_name)
	}
	if clean.starts_with('map[') {
		bracket_end := shared_generic_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := g.shared_qualify_type_text(clean[4..bracket_end], module_name)
			val := g.shared_qualify_type_text(clean[bracket_end + 1..], module_name)
			return 'map[${key}]${val}'
		}
	}
	if clean.starts_with('[') {
		bracket_end := shared_generic_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] + g.shared_qualify_type_text(clean[bracket_end +
				1..], module_name)
		}
	}
	base, args, ok := shared_generic_app_parts(clean)
	if ok {
		mut qualified_args := []string{}
		for arg in args {
			qualified_args << g.shared_qualify_type_text(arg, module_name)
		}
		return '${g.shared_qualify_leaf_type_text(base, module_name)}[${qualified_args.join(', ')}]'
	}
	return g.shared_qualify_leaf_type_text(clean, module_name)
}

fn (g &FlatGen) shared_qualify_leaf_type_text(name string, module_name string) string {
	if module_name.len == 0 || module_name == 'main' || module_name == 'builtin'
		|| name.contains('.') {
		return name
	}
	candidate := '${module_name}.${name}'
	if candidate in g.tc.structs || candidate in g.tc.type_aliases
		|| candidate in g.tc.interface_names || candidate in g.tc.sum_types
		|| candidate in g.tc.enum_names || candidate in g.tc.flag_enums {
		return candidate
	}
	return name
}

fn shared_generic_base_matches_decl(base string, info StructDeclInfo) bool {
	if base == info.full_name {
		return true
	}
	return !info.full_name.contains('.') && base == info.node.value
}

fn (mut g FlatGen) collect_shared_type_names() {
	g.shared_type_names = map[string]SharedTypeInfo{}
	g.needs_shared_runtime = false
	old_module := g.tc.cur_module
	for _, info in g.struct_decl_infos {
		g.tc.cur_module = info.module
		if info.node.generic_params.len > 0 || info.node.typ.contains('generic') {
			g.collect_generic_shared_type_names(info)
			continue
		}
		g.collect_shared_type_names_from_info(info, []string{})
	}
	g.collect_local_shared_type_names()
	g.tc.cur_module = old_module
}

fn (mut g FlatGen) collect_local_shared_type_names() {
	mut cur_module := 'main'
	for i in 0 .. g.a.nodes.len {
		node := g.a.nodes[i]
		if node.kind == .module_decl {
			cur_module = if node.value.len == 0 { 'main' } else { node.value }
			continue
		}
		if node.kind != .decl_assign || !decl_assign_is_shared_marker(node.value) {
			if node.kind == .fn_decl {
				g.collect_fn_shared_param_type_names(node, cur_module)
			}
			continue
		}
		g.tc.cur_module = cur_module
		for j := 0; j + 1 < node.children_count; j += 2 {
			rhs_id := g.a.child(&node, j + 1)
			inner_type := shared_local_value_type(g.tc.resolve_type(rhs_id))
			if inner_type is types.Unknown || inner_type is types.Void {
				continue
			}
			inner := g.shared_qualify_type_text(inner_type.name(), cur_module)
			g.register_shared_type_name(inner, cur_module)
		}
	}
}

fn shared_local_value_type(typ types.Type) types.Type {
	if typ is types.Pointer {
		return typ.base_type
	}
	return typ
}

fn (mut g FlatGen) collect_fn_shared_param_type_names(node flat.Node, module_name string) {
	g.tc.cur_module = module_name
	for i in 0 .. node.children_count {
		param := g.a.child_node(&node, i)
		if param.kind != .param {
			continue
		}
		inner := shared_inner_type_text(param.typ) or { continue }
		if shared_type_text_uses_generic_params(inner, node.generic_params) {
			continue
		}
		g.register_shared_type_name(g.shared_qualify_type_text(inner, module_name), module_name)
	}
}

fn (mut g FlatGen) register_shared_type_name(inner string, module_name string) {
	if inner.len == 0 || inner == 'unknown' || inner == 'void' {
		return
	}
	wrapper := g.shared_wrapper_c_name(inner)
	g.shared_type_names[wrapper] = SharedTypeInfo{
		inner:  inner
		module: module_name
	}
	g.needs_shared_runtime = true
}

fn (mut g FlatGen) collect_generic_shared_type_names(info StructDeclInfo) {
	for type_name, _ in g.tc.structs {
		base, args, ok := shared_generic_app_parts(type_name)
		if !ok || !shared_generic_base_matches_decl(base, info) {
			continue
		}
		g.collect_shared_type_names_from_info(info, args)
	}
}

fn (mut g FlatGen) collect_shared_type_names_from_info(info StructDeclInfo, args []string) {
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind != .field_decl {
			continue
		}
		inner := shared_inner_type_text(field.typ) or { continue }
		concrete_inner := substitute_shared_generic_type_text(inner, info.node.generic_params, args)
		qualified_inner := g.shared_qualify_type_text(concrete_inner, info.module)
		g.register_shared_type_name(qualified_inner, info.module)
	}
}

fn (g &FlatGen) sorted_shared_wrapper_names() []string {
	mut names := []string{}
	for name, _ in g.shared_type_names {
		names << name
	}
	names.sort()
	return names
}

fn (mut g FlatGen) shared_value_type_name(inner string) string {
	typ := g.tc.parse_type(inner)
	if typ is types.Array {
		return 'Array_${naming.type_name_part(g.tc.c_type(typ.elem_type))}'
	}
	if typ is types.Map {
		key := naming.type_name_part(g.tc.c_type(typ.key_type))
		val := naming.type_name_part(g.tc.c_type(typ.value_type))
		return 'Map_${key}_${val}'
	}
	if typ is types.OptionType || typ is types.ResultType {
		return naming.type_name_part(g.optional_type_name(typ))
	}
	mut ct := g.tc.c_type(typ)
	if ct.starts_with('fn_ptr:') {
		ct = g.resolve_fn_ptr_type(ct)
	}
	if ct == 'void' {
		ct = 'int'
	}
	return naming.type_name_part(ct)
}

fn (mut g FlatGen) shared_value_c_type(inner string) string {
	typ := g.tc.parse_type(inner)
	mut ct := if typ is types.OptionType || typ is types.ResultType {
		g.optional_type_name(typ)
	} else {
		g.tc.c_type(typ)
	}
	if ct.starts_with('fn_ptr:') {
		ct = g.resolve_fn_ptr_type(ct)
	}
	if ct == 'void' {
		ct = 'int'
	}
	return ct
}

fn (mut g FlatGen) shared_wrapper_c_name(inner string) string {
	name := g.shared_value_type_name(inner)
	typ := g.tc.parse_type(inner)
	if typ is types.Struct {
		struct_type := typ as types.Struct
		if info := g.find_struct_decl(struct_type.name) {
			if info.module == 'main' {
				return '__shared__main__${name}'
			}
		}
	}
	return '__shared__${name}'
}

fn (mut g FlatGen) generic_shared_field_info(type_name string, field_name string) ?SharedFieldInfo {
	base, args, ok := shared_generic_app_parts(type_name)
	if !ok {
		return none
	}
	info := g.find_struct_decl(base) or { return none }
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	defer {
		g.tc.cur_module = old_module
	}
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind != .field_decl || field.value != field_name {
			continue
		}
		inner := shared_inner_type_text(field.typ) or { return none }
		concrete_inner := substitute_shared_generic_type_text(inner, info.node.generic_params, args)
		qualified_inner := g.shared_qualify_type_text(concrete_inner, info.module)
		return SharedFieldInfo{
			inner:   qualified_inner
			wrapper: g.shared_wrapper_c_name(qualified_inner)
			module:  info.module
		}
	}
	return none
}

fn (mut g FlatGen) shared_field_info(type_name string, field_name string) ?SharedFieldInfo {
	if wrapper := g.shared_type_names[type_name] {
		return g.shared_field_info(wrapper.inner, field_name)
	}
	info := g.find_struct_decl(type_name) or {
		return g.generic_shared_field_info(type_name, field_name)
	}
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	defer {
		g.tc.cur_module = old_module
	}
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind != .field_decl || field.value != field_name {
			continue
		}
		inner := shared_inner_type_text(field.typ) or { return none }
		qualified_inner := g.shared_qualify_type_text(inner, info.module)
		return SharedFieldInfo{
			inner:   qualified_inner
			wrapper: g.shared_wrapper_c_name(qualified_inner)
			module:  info.module
		}
	}
	return none
}

fn (mut g FlatGen) atomic_selector_type(id flat.NodeId) ?types.Type {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return none
	}
	node := g.a.node(id)
	if node.kind != .selector || node.children_count == 0 {
		return none
	}
	base_id := g.a.child(node, 0)
	mut base_type := types.unwrap_pointer(g.usable_expr_type(base_id))
	if base_type is types.Alias {
		base_type = types.unwrap_pointer(base_type.base_type)
	}
	if base_type !is types.Struct {
		return none
	}
	struct_type := base_type as types.Struct
	info := g.find_struct_decl(struct_type.name) or { return none }
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	defer {
		g.tc.cur_module = old_module
	}
	for i in 0 .. info.node.children_count {
		field := g.a.child_node(&info.node, i)
		if field.kind == .field_decl && field.value == node.value
			&& field.typ.trim_space().starts_with('atomic ') {
			return g.tc.parse_type(field.typ.trim_space()[7..])
		}
	}
	return none
}

fn (g &FlatGen) atomic_helper_suffix(typ types.Type) string {
	ct := g.tc.c_type(typ)
	return match ct {
		'i8', 'u8', 'byte', 'bool', 'char' { 'byte' }
		'i16', 'u16' { 'u16' }
		'i64', 'u64', 'isize', 'usize' { 'u64' }
		else { 'u32' }
	}
}

fn (mut g FlatGen) shared_type_forward_decls() {
	for name in g.sorted_shared_wrapper_names() {
		g.writeln('typedef struct ${name} ${name};')
		g.writeln('static inline void* __dup${name}(void* src, int sz);')
	}
}

fn (mut g FlatGen) shared_struct_decls() {
	names := g.sorted_shared_wrapper_names()
	if names.len == 0 {
		return
	}
	g.writeln('// V shared types:')
	old_module := g.tc.cur_module
	for name in names {
		info := g.shared_type_names[name] or { continue }
		g.tc.cur_module = info.module
		val_ct := g.shared_value_c_type(info.inner)
		g.writeln('struct ${name} {')
		g.writeln('\tsync__RwMutex mtx;')
		g.writeln('\t${val_ct} val;')
		g.writeln('};')
	}
	g.tc.cur_module = old_module
	g.writeln('')
}

fn (mut g FlatGen) shared_dup_fns() {
	names := g.sorted_shared_wrapper_names()
	if names.len == 0 {
		return
	}
	for name in names {
		g.writeln('static inline void* __dup${name}(void* src, int sz) {')
		g.writeln('\t${name}* dest = (${name}*)malloc((size_t)sz);')
		g.writeln('\tmemcpy(dest, src, (size_t)sz);')
		g.writeln('\tsync__RwMutex__init(&dest->mtx);')
		g.writeln('\treturn dest;')
		g.writeln('}')
	}
	g.writeln('')
}

fn (mut g FlatGen) gen_shared_storage_expr(id flat.NodeId) bool {
	if int(id) < 0 || int(id) >= g.a.nodes.len {
		return false
	}
	node := g.a.nodes[int(id)]
	if node.kind == .paren && node.children_count > 0 {
		return g.gen_shared_storage_expr(g.a.child(&node, 0))
	}
	if node.kind == .prefix && node.value == 'shared' && node.children_count > 0 {
		return g.gen_shared_storage_expr(g.a.child(&node, 0))
	}
	if node.kind == .ident && g.local_storage_is_shared(node.value) {
		g.write(g.cname(node.value))
		return true
	}
	if node.kind != .selector || node.children_count == 0 {
		return false
	}
	base_id := g.a.child(&node, 0)
	base_type0 := g.usable_expr_type(base_id)
	return g.gen_shared_field_storage_selector(base_id, base_type0, node.value, node.op)
}

fn (mut g FlatGen) gen_local_shared_value_selector(base_id flat.NodeId, field string) bool {
	if int(base_id) < 0 || int(base_id) >= g.a.nodes.len {
		return false
	}
	base := g.a.nodes[int(base_id)]
	if base.kind != .ident || !g.local_storage_is_shared(base.value) {
		return false
	}
	g.write(g.cname(base.value))
	g.write('->val.')
	g.write(c_field_name(field))
	mut base_type := types.unwrap_pointer(g.usable_expr_type(base_id))
	if base_type is types.Alias {
		base_type = types.unwrap_pointer(base_type.base_type)
	}
	if base_type is types.Struct {
		if _ := g.shared_field_info(base_type.name, field) {
			g.write('->val')
		}
	}
	return true
}

fn (mut g FlatGen) gen_shared_field_storage_selector(base_id flat.NodeId, base_type0 types.Type, field string, op flat.Op) bool {
	mut base_type := types.unwrap_pointer(base_type0)
	if base_type is types.Alias {
		base_type = types.unwrap_pointer(base_type.base_type)
	}
	if base_type !is types.Struct {
		return false
	}
	struct_type := base_type as types.Struct
	_ = g.shared_field_info(struct_type.name, field) or { return false }
	base := g.a.nodes[int(base_id)]
	needs_paren := base.kind !in [.ident, .selector]
	if needs_paren {
		g.write('(')
	}
	g.gen_expr(base_id)
	if needs_paren {
		g.write(')')
	}
	if op == .arrow || base_type0 is types.Pointer {
		g.write('->')
	} else {
		g.write('.')
	}
	g.write(c_field_name(field))
	return true
}

fn (mut g FlatGen) gen_shared_field_value_selector(base_id flat.NodeId, base_type0 types.Type, field string, op flat.Op) bool {
	mut base_type := types.unwrap_pointer(base_type0)
	if base_type is types.Alias {
		base_type = types.unwrap_pointer(base_type.base_type)
	}
	if base_type !is types.Struct {
		return false
	}
	struct_type := base_type as types.Struct
	_ = g.shared_field_info(struct_type.name, field) or { return false }
	if !g.gen_shared_field_storage_selector(base_id, base_type0, field, op) {
		return false
	}
	g.write('->val')
	return true
}

fn (mut g FlatGen) gen_shared_field_expr_for_field(value_id flat.NodeId, struct_name string, field_name string, expected types.Type) bool {
	info := g.shared_field_info(struct_name, field_name) or { return false }
	if g.gen_shared_storage_expr(value_id) {
		return true
	}
	if fixed := array_fixed_type(expected) {
		literal := g.fixed_array_compound_literal_expr(value_id, fixed)
		brace := literal.index_u8(`{`)
		if brace >= 0 {
			g.write('(${info.wrapper}*)__dup${info.wrapper}(&(${info.wrapper}){.mtx = {0}, .val = ')
			g.write(literal[brace..])
			g.write('}, sizeof(${info.wrapper}))')
			return true
		}
		// A non-literal fixed-array value cannot initialize the array member in a
		// compound literal (`.val = arr` is invalid C), so build the wrapper in a
		// temp and memcpy the value in, mirroring the non-shared fixed-array path.
		tmp := g.tmp_name()
		g.write('({ ${info.wrapper} ${tmp} = {.mtx = {0}}; memcpy(${tmp}.val, ')
		g.gen_struct_field_expr(value_id, expected)
		g.write(', sizeof(${tmp}.val)); (${info.wrapper}*)__dup${info.wrapper}(&${tmp}, sizeof(${info.wrapper})); })')
		return true
	}
	g.write('(${info.wrapper}*)__dup${info.wrapper}(&(${info.wrapper}){.mtx = {0}, .val = ')
	g.gen_struct_field_expr(value_id, expected)
	g.write('}, sizeof(${info.wrapper}))')
	return true
}

fn (mut g FlatGen) gen_shared_default_value_for_field(struct_name string, field_name string, field_type types.Type) bool {
	info := g.shared_field_info(struct_name, field_name) or { return false }
	old_module := g.tc.cur_module
	g.tc.cur_module = info.module
	inner_type := g.tc.parse_type(info.inner)
	g.write('(${info.wrapper}*)__dup${info.wrapper}(&(${info.wrapper}){.mtx = {0}, .val = ')
	if inner_type is types.Map {
		g.write_new_map(inner_type.key_type, inner_type.value_type)
	} else if inner_type is types.Array {
		c_elem := g.tc.c_type(inner_type.elem_type)
		g.write('array_new(sizeof(${c_elem}), 0, 0)')
	} else {
		g.gen_default_value_for_type(inner_type)
	}
	g.write('}, sizeof(${info.wrapper}))')
	g.tc.cur_module = old_module
	_ = field_type
	return true
}

// struct_init_c_type_name supports struct init c type name handling for FlatGen.
// generic_struct_init_instance_ct returns the concrete-instance C type name for a
// bare generic struct literal whose type args are pinned by the surrounding expected
// type (e.g. `Vec4{..}` written where a `Vec4[f32]` is expected). Returns none when
// the literal is already specialized, the base is not a generic struct, or the
// expected type is not a matching concrete instance.
fn (g &FlatGen) generic_struct_init_instance_ct(type_name string) ?string {
	return g.tc.c_type(g.generic_struct_init_instance_type(type_name)?)
}

fn (g &FlatGen) generic_struct_init_instance_ct_for_node(node flat.Node) ?string {
	if node.typ.len > 0 {
		candidate := g.tc.parse_type(node.typ)
		base := types.unwrap_pointer(candidate)
		if base !is types.Array && base !is types.ArrayFixed {
			name := base.name()
			if name.contains('[')
				&& name.all_before('[').all_after_last('.') == node.value.all_after_last('.') {
				return g.tc.c_type(base)
			}
		}
	}
	return g.generic_struct_init_instance_ct(node.value)
}

// generic_struct_init_instance_name is the concrete-instance V type name (`Box[int]`)
// for a bare generic struct literal, so field and default lookups use the materialized
// key under which the struct's fields are stored (the bare `Box` entry is removed by
// monomorphization), not just the emitted C type.
fn (g &FlatGen) generic_struct_init_instance_name(type_name string) ?string {
	return g.generic_struct_init_instance_type(type_name)?.name()
}

// generic_struct_init_instance_type resolves the concrete generic instance a bare literal
// adopts from the surrounding expected/return type (e.g. `Vec4{..}` -> `Vec4[f32]`).
// Returns none when the literal is already specialized, the base is not a generic struct,
// or the expected type is not a matching concrete instance.
fn (g &FlatGen) generic_struct_init_instance_type(type_name string) ?types.Type {
	if type_name.contains('[') {
		return none
	}
	short := type_name.all_after_last('.')
	// Prefer the explicit expected type; fall back to the enclosing function's return
	// type ONLY for a literal in return position (a bare generic literal there carries
	// no expected_expr_type) — otherwise a `Box{...}` in a local decl / argument whose
	// expected type is the bare `Box` would be wrongly materialised as `Box_int`. Only
	// adopt a candidate whose generic base matches the literal's base, so unrelated
	// expected types never rename the struct.
	//
	// Note: `tc.struct_generic_params` is empty by cgen time, so the candidate's
	// shape (a `Base[args]` instance whose base short-name equals the literal's) is
	// the sole evidence that this bare literal is a generic struct instantiation.
	mut candidates := [g.expected_expr_type]
	if g.in_return {
		candidates << g.cur_fn_ret
	}
	for cand in candidates {
		// Unwrap a pointer so a `&Box[int]` expected type still matches a bare `Box`
		// literal — the heap path (`&Box{..}`) needs the struct (`Box_int`), not the
		// pointer, type name.
		base_cand := types.unwrap_pointer(cand)
		// A fixed/dynamic array type is not a generic struct instance even though its
		// `.name()` renders like one (`[2]Foo` -> `Foo[2]`); skip it so a `Foo{..}` element
		// of a `[2]Foo` literal keeps its element type instead of adopting the array type.
		if base_cand is types.ArrayFixed || base_cand is types.Array {
			continue
		}
		cand_name := base_cand.name()
		if !cand_name.contains('[') {
			continue
		}
		cand_base := cand_name.all_before('[')
		if cand_base.len == 0 || cand_base.all_after_last('.') != short {
			continue
		}
		return base_cand
	}
	return none
}

fn (mut g FlatGen) struct_init_c_type_name(type_name string) string {
	init_type_name := g.struct_init_import_alias_type_name(type_name)
	typ := g.tc.parse_type(init_type_name)
	if typ is types.OptionType || typ is types.ResultType {
		return g.optional_type_name(typ)
	}
	if typ is types.MultiReturn {
		return g.value_c_type(typ)
	}
	if ct := g.generic_struct_init_app_ct_from_context(init_type_name) {
		return ct
	}
	if init_type_name.contains('[') {
		return g.tc.c_type(typ)
	}
	if ct := g.flattened_generic_struct_init_ct_from_context(init_type_name) {
		return ct
	}
	if ct := g.flattened_generic_struct_init_ct(init_type_name) {
		return ct
	}
	info := g.find_struct_decl(init_type_name) or {
		return g.tc.c_type(g.tc.parse_type(init_type_name))
	}
	if info.full_name.starts_with('C.') {
		return g.tc.c_type(g.tc.parse_type(info.full_name))
	}
	return g.cname(info.full_name)
}

fn (g &FlatGen) struct_init_import_alias_type_name(type_name string) string {
	if !type_name.contains('.') {
		return type_name
	}
	alias := type_name.all_before('.')
	suffix := type_name.all_after('.')
	if alias.len == 0 || suffix.len == 0 {
		return type_name
	}
	if mod := g.import_alias_module(alias) {
		return '${mod}.${suffix}'
	}
	return type_name
}

fn (g &FlatGen) generic_struct_init_app_ct_from_context(type_name string) ?string {
	clean := trimmed_space(type_name)
	base, args, ok := shared_generic_app_parts(clean)
	if !ok || args.len == 0 {
		return none
	}
	base_short := base.all_after_last('.')
	arg_suffix := generic_receiver_type_suffixes(args)
	for candidate in [g.expected_expr_type, g.cur_fn_ret] {
		candidate_type0 := types.unwrap_pointer(candidate)
		candidate_type := if candidate_type0 is types.OptionType {
			candidate_type0.base_type
		} else if candidate_type0 is types.ResultType {
			candidate_type0.base_type
		} else {
			candidate_type0
		}
		if candidate_type is types.Void || candidate_type is types.Unknown {
			continue
		}
		candidate_name := candidate_type.name()
		candidate_base, candidate_args, candidate_ok := shared_generic_app_parts(candidate_name)
		if !candidate_ok || candidate_args.len != args.len {
			continue
		}
		if candidate_base.all_after_last('.') != base_short {
			continue
		}
		if generic_receiver_type_suffixes(candidate_args) == arg_suffix {
			return g.tc.c_type(candidate_type)
		}
	}
	return none
}

fn (g &FlatGen) flattened_generic_struct_init_ct_from_context(type_name string) ?string {
	clean := trimmed_space(type_name)
	if clean.len == 0 || clean.contains('[') || !clean.contains('_') {
		return none
	}
	for candidate in [g.expected_expr_type, g.cur_fn_ret] {
		base := types.unwrap_pointer(candidate)
		if base is types.Void || base is types.Unknown {
			continue
		}
		candidate_name := base.name()
		if !candidate_name.contains('[') {
			continue
		}
		ct := g.tc.c_type(base)
		if flattened_generic_struct_c_type_short_name(ct) == clean {
			return ct
		}
	}
	return none
}

fn (g &FlatGen) flattened_generic_struct_init_ct(type_name string) ?string {
	clean := trimmed_space(type_name)
	if clean.len == 0 || clean.contains('[') || !clean.contains('_') {
		return none
	}
	mut matches := []string{}
	for struct_name, _ in g.tc.structs {
		base, args, ok := shared_generic_app_parts(struct_name)
		if !ok || args.len == 0 {
			continue
		}
		short_base := base.all_after_last('.')
		ct := g.tc.c_type(g.tc.parse_type(struct_name))
		candidates := [
			'${short_base}_${generic_receiver_type_suffixes(args)}',
			flattened_generic_struct_c_type_short_name(ct),
		]
		if clean !in candidates {
			continue
		}
		if ct !in matches {
			matches << ct
		}
	}
	if matches.len == 1 {
		return matches[0]
	}
	if ct := g.same_module_flattened_generic_struct_ct(clean) {
		return ct
	}
	return none
}

fn (g &FlatGen) same_module_flattened_generic_struct_ct(clean string) ?string {
	if g.tc.cur_module.len == 0 || g.tc.cur_module == 'main' || g.tc.cur_module == 'builtin'
		|| !clean.contains('_') {
		return none
	}
	base := clean.all_before('_')
	suffix := clean.all_after('_')
	if base.len == 0 || suffix.len == 0 {
		return none
	}
	info := g.find_struct_decl(base) or { return none }
	if info.module != g.tc.cur_module || info.node.generic_params.len == 0 {
		return none
	}
	return '${g.cname(g.tc.cur_module)}__${g.cname(base)}_${g.cname(g.tc.cur_module)}__${suffix}'
}

fn flattened_generic_struct_c_type_short_name(ct string) string {
	clean := trimmed_space(ct)
	if clean.len == 0 {
		return clean
	}
	mut out := []u8{}
	mut i := 0
	for i < clean.len {
		mut j := i
		for j < clean.len && ((clean[j] >= `a` && clean[j] <= `z`)
			|| (clean[j] >= `0` && clean[j] <= `9`)) {
			j++
		}
		if j > i && j + 1 < clean.len && clean[j] == `_` && clean[j + 1] == `_` {
			i = j + 2
			continue
		}
		out << clean[i]
		i++
	}
	return out.bytestr()
}

// find_struct_decl resolves find struct decl information for c.
fn (g &FlatGen) find_struct_decl(type_name string) ?StructDeclInfo {
	if info := g.find_struct_decl_preferred(type_name) {
		return info
	}
	if alias_target := g.struct_type_alias_target(type_name) {
		if info := g.find_struct_decl_preferred(alias_target) {
			return info
		}
		if info := g.find_struct_decl_fallback(alias_target) {
			return info
		}
	}
	return g.find_struct_decl_fallback(type_name)
}

fn (g &FlatGen) find_struct_decl_preferred(type_name string) ?StructDeclInfo {
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
	}
	return none
}

fn (g &FlatGen) find_struct_decl_fallback(type_name string) ?StructDeclInfo {
	if type_name.contains('.') {
		return none
	}
	if info := g.struct_decl_short_infos[type_name] {
		return info
	}
	return none
}

fn (g &FlatGen) struct_type_alias_target(type_name string) ?string {
	qname := g.tc.qualify_name(type_name)
	if target := g.tc.type_aliases[qname] {
		return target
	}
	if target := g.tc.type_aliases[type_name] {
		return target
	}
	return none
}

fn (g &FlatGen) struct_init_resolved_decl_name(type_name string) string {
	if info := g.find_struct_decl(type_name) {
		return info.full_name
	}
	qname := g.tc.qualify_name(type_name)
	if qname in g.tc.structs {
		return qname
	}
	return type_name
}

// struct_field_type supports struct field type handling for FlatGen.
fn (g &FlatGen) struct_field_type(type_name string, field_name string) ?types.Type {
	fields := g.struct_fields_for_type(type_name) or { return none }
	for f in fields {
		if f.name == field_name {
			return f.typ
		}
	}
	return none
}

fn (g &FlatGen) struct_field_c_abi_fn_ptr_type(type_name string, field_name string) ?string {
	if info := g.find_struct_decl(type_name) {
		if typ := g.tc.struct_field_c_abi_fn_ptr_type(info.full_name, field_name) {
			return typ
		}
	}
	if typ := g.tc.struct_field_c_abi_fn_ptr_type(type_name, field_name) {
		return typ
	}
	if !type_name.contains('.') {
		qname := g.tc.qualify_name(type_name)
		if qname != type_name {
			if typ := g.tc.struct_field_c_abi_fn_ptr_type(qname, field_name) {
				return typ
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
	if info := g.find_struct_decl(type_name) {
		if fields := g.tc.structs[info.full_name] {
			return fields
		}
	}
	if type_name.contains('.') {
		if fields := g.tc.structs[type_name] {
			return fields
		}
	} else {
		qname := g.tc.qualify_name(type_name)
		if qname != type_name {
			if fields := g.tc.structs[qname] {
				return fields
			}
		}
	}
	if fields := g.tc.structs[type_name] {
		return fields
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
		if field.name == name || short_field == short_type
			|| embedded_field_c_names_match(field.name, name) {
			return field_type_name
		}
	}
	return ''
}

fn embedded_field_c_names_match(field_name string, type_name string) bool {
	field_plain := naming.is_plain_identifier(field_name)
	type_plain := naming.is_plain_identifier(type_name)
	if field_plain && type_plain && !naming.is_reserved_word(field_name)
		&& !naming.is_reserved_word(type_name) && !naming.is_libc_collision(field_name)
		&& !naming.is_libc_collision(type_name) {
		return false
	}
	return c_name(field_name) == c_name(type_name)
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
	// A direct (non-embed) field with this exact name shadows the embedded
	// struct itself: in `struct Outer { aa.Inner; Inner int }`, `o.Inner` is
	// the int field - matching the checker's resolution.
	if fields := g.struct_fields_for_type(type_name) {
		for field in fields {
			if field.name == field_name && g.embedded_field_type_name(field).len == 0 {
				return none
			}
		}
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
			|| g.cname(field_name) == g.cname(embedded_type_name) {
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
	// The struct's own field always shadows a same-named promoted field of an
	// embedded struct.
	if g.direct_struct_field_exists(type_name, field_name) {
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
		return g.struct_init_import_alias_type_name(clean_type.base_type.name())
	}
	return g.struct_init_import_alias_type_name(clean_type.name())
}

// struct_field_at supports struct field at handling for FlatGen.
fn (g &FlatGen) struct_field_at(type_name string, index int) ?types.StructField {
	if index < 0 {
		return none
	}
	fields := g.struct_fields_for_type(type_name) or { return none }
	if index < fields.len {
		return fields[index]
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
			g.write('${tmp}.${g.cname(field.value)} = ')
			if ftyp := g.struct_field_type(node.value, field.value) {
				g.gen_struct_field_expr_for_field(g.a.child(field, 0), node.value, field.value,
					ftyp)
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
			g.write(' ${tmp}.${g.cname(field.value)} = ')
			if ftyp := g.struct_field_type(node.value, field.value) {
				g.gen_struct_field_expr_for_field(g.a.child(field, 0), node.value, field.value,
					ftyp)
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
			g.write(' ${tmp}.${g.cname(field.value)} = ')
			if ftyp := g.struct_field_type(node.value, field.value) {
				g.gen_struct_field_expr_for_field(g.a.child(field, 0), node.value, field.value,
					ftyp)
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
			g.gen_map_init_for_type(node, map_type)
			return
		}
	}
	if node.typ.len > 0 {
		map_type := g.tc.parse_type(node.typ)
		if map_type is types.Map {
			g.gen_map_init_for_type(node, map_type)
			return
		}
	}
	if g.expected_expr_type is types.Map {
		g.gen_map_init_for_type(node, g.expected_expr_type)
		return
	}
	resolved_type := g.tc.resolve_type(id)
	if resolved_type is types.Map {
		g.gen_map_init_for_type(node, resolved_type)
		return
	}
	g.write('new_map(sizeof(int), sizeof(int), 0, 0, 0, 0)')
}

fn (mut g FlatGen) gen_map_init_for_type(node flat.Node, map_type types.Map) {
	if node.children_count < 2 {
		g.write_new_map(map_type.key_type, map_type.value_type)
		return
	}
	tmp := g.tmp_name()
	c_key := g.map_key_temp_c_type(map_type.key_type)
	c_val := g.value_c_type(map_type.value_type)
	g.write('({ map ${tmp} = ')
	g.write_new_map(map_type.key_type, map_type.value_type)
	g.write(';')
	for i := 0; i + 1 < node.children_count; i += 2 {
		g.write(' map__set(&${tmp}, &(${c_key}[]){')
		g.gen_expr_with_expected_type(g.a.child(&node, i), map_type.key_type)
		g.write('}, &(${c_val}[]){')
		g.gen_expr_with_expected_type(g.a.child(&node, i + 1), map_type.value_type)
		g.write('});')
	}
	g.write(' ${tmp}; })')
}

// write_new_map writes new map output for c.
fn (mut g FlatGen) write_new_map(key_type types.Type, value_type types.Type) {
	c_key := g.map_key_sizeof_target(key_type)
	c_val := g.value_sizeof_target(value_type)
	hash_fn, eq_fn, clone_fn, free_fn := g.map_callback_names(key_type)
	g.write('new_map(sizeof(${c_key}), sizeof(${c_val}), ${hash_fn}, ${eq_fn}, ${clone_fn}, ${free_fn})')
}

fn (mut g FlatGen) map_key_sizeof_target(key_type types.Type) string {
	if key_type is types.Enum {
		return g.enum_storage_c_type(key_type)
	}
	return g.value_sizeof_target(key_type)
}

fn (mut g FlatGen) map_key_temp_c_type(key_type types.Type) string {
	if key_type is types.Enum {
		return g.enum_storage_c_type(key_type)
	}
	return g.value_c_type(key_type)
}

// map_callback_names supports map callback names handling for FlatGen.
fn (g &FlatGen) map_callback_names(key_type types.Type) (string, string, string, string) {
	if key_type is types.String {
		return 'map_hash_string', 'map_eq_string', 'map_clone_string', 'map_free_string'
	}
	c_key := if key_type is types.Enum {
		g.enum_storage_c_type(key_type)
	} else {
		g.tc.c_type(key_type)
	}
	size_suffix := match c_key {
		'u8', 'i8', 'bool', 'char' { '1' }
		'u16', 'i16' { '2' }
		'i64', 'u64', 'isize', 'usize', 'f64', 'double', 'voidptr' { '8' }
		else { '4' }
	}

	return 'map_hash_int_${size_suffix}', 'map_eq_int_${size_suffix}', 'map_clone_int_${size_suffix}', 'map_free_nop'
}

// skip_builtin_struct supports skip builtin struct handling for FlatGen.
fn (g &FlatGen) skip_builtin_struct(name string) bool {
	if name.starts_with('C.') && g.inlined_c_structs[name[2..]] {
		return true
	}
	if g.cache_split {
		system_name := if name.starts_with('C.') { name[2..] } else { name }
		if system_name in c_cache_system_header_struct_names {
			return true
		}
	}
	return name in c_preamble_defined_structs
}

const c_preamble_defined_structs = {
	'C.DIR':                        true
	'C.FILE':                       true
	'C.CONDITION_VARIABLE':         true
	'C.IError':                     true
	'C.SRWLOCK':                    true
	'C.addrinfo':                   true
	'C.atomic_uintptr_t':           true
	'C.dirent':                     true
	'C.utimbuf':                    true
	'C.epoll_data':                 true
	'C.epoll_data_t':               true
	'C.epoll_event':                true
	'C.Event':                      true
	'C.fd_set':                     true
	'C.CHAR_INFO':                  true
	'C.CONSOLE_SCREEN_BUFFER_INFO': true
	'C.COORD':                      true
	'C.FOCUS_EVENT_RECORD':         true
	'C.INPUT_RECORD':               true
	'C.KEY_EVENT_RECORD':           true
	'C.kevent':                     true
	'C.MENU_EVENT_RECORD':          true
	'C.MOUSE_EVENT_RECORD':         true
	'C.host_t':                     true
	'C.pthread_t':                  true
	'C.pthread_cond_t':             true
	'C.pthread_condattr_t':         true
	'C.pthread_attr_t':             true
	'C.pthread_mutex_t':            true
	'C.pthread_rwlock_t':           true
	'C.pthread_rwlockattr_t':       true
	'C.rusage':                     true
	'C.sem_t':                      true
	'C.SECURITY_ATTRIBUTES':        true
	'C.sigset_t':                   true
	'C.SMALL_RECT':                 true
	'C.OVERLAPPED':                 true
	'C.sockaddr':                   true
	'C.sockaddr_in':                true
	'C.sockaddr_in6':               true
	'C.sockaddr_un':                true
	'C.stat':                       true
	'C.statvfs':                    true
	'C.task_basic_info':            true
	'C.task_t':                     true
	'C.termios':                    true
	'C.timeval':                    true
	'C.timespec':                   true
	'C.tm':                         true
	'C.uChar':                      true
	'C.utsname':                    true
	'C.vm_size_t':                  true
	'C.vm_statistics64_data_t':     true
	'C.wchar_t':                    true
	'C.WINDOW_BUFFER_SIZE_RECORD':  true
	'C.winsize':                    true
}

fn c_struct_needs_typedef(name string) bool {
	if !name.starts_with('C.') {
		return true
	}
	raw := name[2..]
	if raw.len > 0 && raw[0] >= `a` && raw[0] <= `z` && !raw.ends_with('_t') {
		return false
	}
	return true
}

// emit_interface_struct emits emit interface struct output for c.
fn (mut g FlatGen) emit_interface_struct(name string) {
	iface_fields := g.tc.interface_fields[name] or { []types.StructField{} }
	g.emit_struct_option_typedefs(iface_fields)
	cn := g.cname(name)
	g.writeln('struct ${cn} {')
	g.writeln('\tint _typ;')
	if g.is_ierror_type_name(name) {
		g.writeln('\tvoid* _object;')
		g.writeln('\tstring message;')
		g.writeln('\tint code;')
	} else {
		// pointer to the boxed concrete value, used by method dispatch
		g.writeln('\tvoid* _object;')
	}
	for field in iface_fields {
		mut ct := if field.typ is types.OptionType || field.typ is types.ResultType {
			g.optional_type_name(field.typ)
		} else {
			g.tc.c_type(field.typ)
		}
		if ct.starts_with('fn_ptr:') {
			ct = g.resolve_fn_ptr_type(ct)
		}
		g.writeln('\t${ct} ${g.cname(field.name)};')
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
			// An inlined header that defines `struct zip_t` without a typedef
			// leaves V references to the bare name dangling; supply the alias
			// (skipped when the header already typedefs it).
			if name.starts_with('C.') && name !in c_preamble_defined_structs
				&& c_struct_needs_typedef(name) && g.inlined_c_structs[name[2..]]
				&& !g.inlined_c_typedef_names[name[2..]] && !(g.cache_split
				&& name[2..] in c_cache_system_header_struct_names) {
				ityp := if name in g.tc.unions { 'union' } else { 'struct' }
				g.writeln('typedef ${ityp} ${g.cname(name)} ${g.cname(name)};')
			}
			continue
		}
		if !c_struct_needs_typedef(name) {
			continue
		}
		tag := if name in g.tc.unions { 'union' } else { 'struct' }
		g.writeln('typedef ${tag} ${g.cname(name)} ${g.cname(name)};')
	}
	for name, variants in g.tc.sum_types {
		g.writeln('typedef struct ${g.cname(name)} ${g.cname(name)};')
		_ = variants
	}
	for name, _ in g.interfaces {
		g.writeln('typedef struct ${g.cname(name)} ${g.cname(name)};')
	}
	g.shared_type_forward_decls()
	if g.has_builtins {
		g.writeln('typedef array Array;')
		g.flattened_map_type_alias_decls()
	}
	mut emitted := map[string]bool{}
	mut remaining := map[string]bool{}
	mut remaining_cnames := map[string]bool{}
	mut iface_remaining := map[string]bool{}
	for name, _ in g.interfaces {
		iface_remaining[name] = true
		remaining_cnames[g.cname(name)] = true
	}
	for name, _ in g.tc.structs {
		if g.skip_builtin_struct(name) {
			continue
		}
		remaining[name] = true
		remaining_cnames[g.cname(name)] = true
	}
	mut sum_remaining := map[string]bool{}
	for name, _ in g.tc.sum_types {
		sum_remaining[name] = true
		remaining_cnames[g.cname(name)] = true
	}
	if 'string' in remaining {
		g.emit_struct('string')
		emitted['string'] = true
		remaining.delete('string')
		remaining_cnames.delete('string')
	}
	mut has_ierror := false
	for name, _ in iface_remaining {
		if g.is_ierror_type_name(name) {
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
			cn := g.cname(name)
			mut can_emit := true
			if g.is_ierror_type_name(name) {
				if 'string' !in emitted && 'string' in remaining_cnames {
					can_emit = false
				}
			}
			// An interface struct embeds its declared data fields by value, so the
			// field types must be fully defined first (same constraint as structs).
			// Option/result fields embed their payload by value in the Optional_T
			// typedef, so the dependency is the payload type, not the wrapper.
			for field in g.tc.interface_fields[name] or { []types.StructField{} } {
				if field.typ is types.Pointer {
					continue
				}
				mut fct := ''
				if field.typ is types.ArrayFixed {
					fct = g.tc.c_type(field.typ.elem_type)
				} else if field.typ is types.OptionType {
					fct = g.tc.c_type(field.typ.base_type)
				} else if field.typ is types.ResultType {
					fct = g.tc.c_type(field.typ.base_type)
				} else {
					fct = g.tc.c_type(field.typ)
				}
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
			cn := g.cname(name)
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
			cn := g.cname(name)
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
		cn := g.cname(name)
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
	g.soa_companion_decls()
	g.shared_struct_decls()
}

fn (mut g FlatGen) flattened_map_type_alias_decls() {
	mut names := map[string]bool{}
	for node in g.a.nodes {
		if node.kind in [.fn_decl, .c_fn_decl, .fn_literal, .param] {
			g.collect_flattened_map_type_alias(node.typ, mut names)
		}
	}
	for _, ret in g.tc.fn_ret_types {
		if ret is types.Struct {
			g.collect_flattened_map_type_alias(ret.name, mut names)
		}
	}
	for _, params in g.tc.fn_param_types {
		for param in params {
			if param is types.Struct {
				g.collect_flattened_map_type_alias(param.name, mut names)
			}
		}
	}
	mut sorted_names := names.keys()
	sorted_names.sort()
	for name in sorted_names {
		g.writeln('typedef map ${name};')
	}
	if sorted_names.len > 0 {
		g.writeln('')
	}
}

fn (g &FlatGen) collect_flattened_map_type_alias(typ string, mut names map[string]bool) {
	clean := trimmed_space(typ).trim_left('&')
	if clean.starts_with('map_') && !clean.starts_with('map__') && clean !in g.tc.structs
		&& clean !in g.tc.type_aliases {
		names[g.cname(clean)] = true
	}
}

// type_forward_decls returns type forward decls data for FlatGen.
fn (mut g FlatGen) type_forward_decls() {
	for name, _ in g.tc.structs {
		if g.skip_builtin_struct(name) {
			continue
		}
		if !c_struct_needs_typedef(name) {
			continue
		}
		tag := if name in g.tc.unions { 'union' } else { 'struct' }
		g.writeln('typedef ${tag} ${g.cname(name)} ${g.cname(name)};')
	}
	for name, _ in g.tc.sum_types {
		g.writeln('typedef struct ${g.cname(name)} ${g.cname(name)};')
	}
	for name, _ in g.interfaces {
		g.writeln('typedef struct ${g.cname(name)} ${g.cname(name)};')
	}
	g.shared_type_forward_decls()
	if g.has_builtins {
		g.writeln('typedef array Array;')
	}
	g.writeln('')
}

// emit_struct emits emit struct output for c.
fn (mut g FlatGen) emit_struct(name string) {
	old_module := g.tc.cur_module
	g.tc.cur_module = g.fixed_array_typedef_type_module(name, old_module)
	if name in g.tc.structs {
		fields := g.tc.structs[name]
		tag := if name in g.tc.unions { 'union' } else { 'struct' }
		g.writeln('${tag} ${g.cname(name)} {')
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
		return 'generic' in info.node.typ.split(',')
	}
	short_name := if name.contains('.') { name.all_after_last('.') } else { name }
	if info := g.struct_decl_short_infos[short_name] {
		return info.full_name == name && 'generic' in info.node.typ.split(',')
	}
	return false
}

fn (mut g FlatGen) soa_companion_decls() {
	mut names := g.tc.soa_structs.keys()
	names.sort()
	for name in names {
		if name !in g.tc.structs || name in g.tc.unions || g.is_generic_struct(name) {
			continue
		}
		g.emit_soa_companion(name)
	}
}

fn (g &FlatGen) soa_companion_name(struct_name string) string {
	mut source_name := struct_name
	if mod_name := g.tc.struct_modules[struct_name] {
		if mod_name == 'main' || mod_name == 'builtin' {
			source_name = struct_name.all_after_last('.')
		}
	}
	return '${g.cname(source_name)}_SOA'
}

fn (g &FlatGen) soa_companion_has_c_typedef(soa_name string) bool {
	return 'C.${soa_name}' in g.tc.structs
}

fn (g &FlatGen) soa_companion_collision(struct_name string, soa_name string, fields []SoaFieldInfo) ?string {
	c_name := 'C.${soa_name}'
	has_matching_c_decl := c_name in g.tc.structs && g.soa_companion_c_decl_matches(c_name, fields)
	if c_name in g.tc.structs && !has_matching_c_decl {
		return c_name
	}
	for name, _ in g.tc.structs {
		if name == struct_name || name == c_name {
			continue
		}
		if g.cname(name) == soa_name {
			return name
		}
	}
	for name, _ in g.tc.type_aliases {
		if g.cname(name) == soa_name {
			return name
		}
	}
	for name, _ in g.tc.enum_names {
		if g.cname(name) == soa_name {
			return name
		}
	}
	for name, _ in g.tc.sum_types {
		if g.cname(name) == soa_name {
			return name
		}
	}
	for name, _ in g.tc.interface_names {
		if g.cname(name) == soa_name {
			return name
		}
	}
	if !has_matching_c_decl
		&& (soa_name in g.inlined_c_typedef_names || soa_name in g.inlined_c_structs) {
		return c_name
	}
	return none
}

fn (g &FlatGen) soa_companion_c_decl_matches(c_name string, fields []SoaFieldInfo) bool {
	decl_fields := g.tc.structs[c_name] or { return false }
	if decl_fields.len != fields.len + 2 {
		return false
	}
	if !g.soa_companion_c_decl_field_matches(decl_fields[0], 'len', 'int') {
		return false
	}
	if !g.soa_companion_c_decl_field_matches(decl_fields[1], 'cap', 'int') {
		return false
	}
	for i, field in fields {
		if !g.soa_companion_c_decl_field_matches(decl_fields[i + 2], field.soa_name,
			'${field.c_type}*') {
			return false
		}
	}
	return true
}

fn (g &FlatGen) soa_companion_c_decl_field_matches(field types.StructField, name string, c_type string) bool {
	return g.cname(field.name) == name && g.tc.c_type(field.typ) == c_type
}

fn (mut g FlatGen) soa_field_c_type(struct_name string, f types.StructField) string {
	if f.typ is types.Void {
		return 'int'
	}
	if info := g.shared_field_info(struct_name, f.name) {
		return '${info.wrapper}*'
	}
	mut field_type := f.typ
	if f.typ is types.Alias {
		field_type = f.typ.base_type
	}
	if field_type is types.FnType {
		c_abi_fn := g.struct_field_c_abi_fn_ptr_type(struct_name, f.name) or {
			g.tc.c_type(field_type)
		}
		return g.resolve_fn_ptr_type(c_abi_fn)
	}
	if field_type is types.ArrayFixed {
		return g.fixed_array_c_type(field_type)
	}
	mut ct := if field_type is types.OptionType || field_type is types.ResultType {
		g.optional_type_name(field_type)
	} else if field_type is types.Enum {
		g.enum_value_c_type(field_type)
	} else {
		g.tc.c_type(field_type)
	}
	if ct.starts_with('fn_ptr:') {
		ct = g.resolve_fn_ptr_type(ct)
	}
	if ct == 'void' {
		return 'int'
	}
	return ct
}

fn (mut g FlatGen) write_soa_struct_return(base_name string, fields []SoaFieldInfo, prefix string, index string) {
	if fields.len == 0 {
		g.writeln('\treturn (${base_name}){0};')
		return
	}
	mut has_fixed_array := false
	for field in fields {
		if field.is_fixed_array {
			has_fixed_array = true
			break
		}
	}
	if has_fixed_array {
		g.writeln('\t${base_name} val = (${base_name}){0};')
		for field in fields {
			if field.is_fixed_array {
				g.writeln('\tmemmove(val.${field.name}, ${prefix}${field.soa_name}[${index}], sizeof(val.${field.name}));')
			} else {
				g.writeln('\tval.${field.name} = ${prefix}${field.soa_name}[${index}];')
			}
		}
		g.writeln('\treturn val;')
		return
	}
	g.writeln('\treturn (${base_name}){')
	for field in fields {
		g.writeln('\t\t.${field.name} = ${prefix}${field.soa_name}[${index}],')
	}
	g.writeln('\t};')
}

fn (mut g FlatGen) write_soa_field_value_copy(field SoaFieldInfo, prefix string, index string) {
	if field.is_fixed_array {
		g.writeln('\tmemmove(${prefix}${field.soa_name}[${index}], val.${field.name}, sizeof(${prefix}${field.soa_name}[${index}]));')
		return
	}
	g.writeln('\t${prefix}${field.soa_name}[${index}] = val.${field.name};')
}

fn soa_companion_field_name(name string, mut used map[string]bool) string {
	mut candidate := name
	if candidate in used {
		base := '__soa_field_${name}'
		candidate = base
		mut suffix := 2
		for (candidate in used) {
			candidate = '${base}_${suffix}'
			suffix++
		}
	}
	used[candidate] = true
	return candidate
}

fn (mut g FlatGen) emit_soa_companion(struct_name string) {
	fields := g.tc.structs[struct_name] or { return }
	base_name := g.tc.c_type(types.Type(types.Struct{
		name: struct_name
	}))
	soa_name := g.soa_companion_name(struct_name)
	mut soa_fields := []SoaFieldInfo{cap: fields.len}
	mut used_soa_names := {
		'len': true
		'cap': true
	}
	for f in fields {
		field_name := g.cname(f.name)
		is_fixed_array := if _ := array_fixed_type(f.typ) { true } else { false }
		soa_fields << SoaFieldInfo{
			name:           field_name
			soa_name:       soa_companion_field_name(field_name, mut used_soa_names)
			c_type:         g.soa_field_c_type(struct_name, f)
			is_fixed_array: is_fixed_array
		}
	}
	g.writeln('/* SoA companion for ${base_name}. */')
	if conflict := g.soa_companion_collision(struct_name, soa_name, soa_fields) {
		g.writeln('#error SoA companion ${soa_name} for ${base_name} collides with existing type ${conflict}')
		return
	}
	if !g.soa_companion_has_c_typedef(soa_name) {
		g.writeln('typedef struct ${soa_name} {')
		g.writeln('\tint len;')
		g.writeln('\tint cap;')
		for field in soa_fields {
			g.writeln('\t${field.c_type}* ${field.soa_name};')
		}
		g.writeln('} ${soa_name};')
		g.writeln('')
	}
	g.writeln('${soa_name} ${soa_name}_new(int len, int cap) {')
	g.writeln('\tif (len < 0) { len = 0; }')
	g.writeln('\tif (cap < len) { cap = len; }')
	g.writeln('\tif (cap < 0) { cap = 0; }')
	g.writeln('\t${soa_name} soa;')
	g.writeln('\tsoa.len = len;')
	g.writeln('\tsoa.cap = cap;')
	for field in soa_fields {
		g.writeln('\tsoa.${field.soa_name} = (${field.c_type}*)calloc((size_t)cap, sizeof(${field.c_type}));')
	}
	g.writeln('\treturn soa;')
	g.writeln('}')
	g.writeln('')
	g.writeln('${base_name} ${soa_name}_get(${soa_name} soa, int i) {')
	g.write_soa_struct_return(base_name, soa_fields, 'soa.', 'i')
	g.writeln('}')
	g.writeln('')
	g.writeln('void ${soa_name}_set(${soa_name}* soa, int i, ${base_name} val) {')
	for field in soa_fields {
		g.write_soa_field_value_copy(field, 'soa->', 'i')
	}
	g.writeln('}')
	g.writeln('')
	g.writeln('void ${soa_name}_push(${soa_name}* soa, ${base_name} val) {')
	g.writeln('\tif (soa->len >= soa->cap) {')
	g.writeln('\t\tint new_cap = soa->cap < 8 ? 8 : soa->cap * 2;')
	for field in soa_fields {
		g.writeln('\t\tsoa->${field.soa_name} = (${field.c_type}*)realloc(soa->${field.soa_name}, (size_t)new_cap * sizeof(${field.c_type}));')
	}
	g.writeln('\t\tsoa->cap = new_cap;')
	g.writeln('\t}')
	g.writeln('\tint i = soa->len;')
	for field in soa_fields {
		g.write_soa_field_value_copy(field, 'soa->', 'i')
	}
	g.writeln('\tsoa->len++;')
	g.writeln('}')
	g.writeln('')
	g.writeln('${base_name} ${soa_name}_pop(${soa_name}* soa) {')
	g.writeln('\tif (soa->len <= 0) {')
	g.writeln('\t\treturn (${base_name}){0};')
	g.writeln('\t}')
	g.writeln('\tsoa->len--;')
	g.write_soa_struct_return(base_name, soa_fields, 'soa->', 'soa->len')
	g.writeln('}')
	g.writeln('')
	g.writeln('void ${soa_name}_free(${soa_name}* soa) {')
	for field in soa_fields {
		g.writeln('\tfree(soa->${field.soa_name});')
		g.writeln('\tsoa->${field.soa_name} = NULL;')
	}
	g.writeln('\tsoa->len = 0;')
	g.writeln('\tsoa->cap = 0;')
	g.writeln('}')
	g.writeln('')
}

// write_struct_field writes struct field output for c.
fn (mut g FlatGen) write_struct_field(_struct_name string, f types.StructField) {
	if f.typ is types.Void {
		g.writeln('\tint ${g.cname(f.name)};')
		return
	}
	if info := g.shared_field_info(_struct_name, f.name) {
		g.writeln('\t${info.wrapper}* ${g.cname(f.name)};')
		return
	}
	mut field_type := f.typ
	if f.typ is types.Alias {
		field_type = f.typ.base_type
	}
	raw_field_type := field_type
	if field_type is types.FnType {
		c_abi_fn := g.struct_field_c_abi_fn_ptr_type(_struct_name, f.name) or {
			g.tc.c_type(raw_field_type)
		}
		ct := g.resolve_fn_ptr_type(c_abi_fn)
		g.writeln('\t${ct} ${g.cname(f.name)};')
	} else if f.typ is types.ArrayFixed {
		c_elem, dims := g.fixed_array_decl_parts(f.typ)
		g.writeln('\t${c_elem} ${g.cname(f.name)}${dims};')
	} else {
		mut ct := if f.typ is types.OptionType || f.typ is types.ResultType {
			g.optional_type_name(f.typ)
		} else if f.typ is types.Enum {
			g.enum_value_c_type(f.typ)
		} else {
			g.tc.c_type(f.typ)
		}
		if ct.starts_with('fn_ptr:') {
			ct = g.resolve_fn_ptr_type(ct)
		}
		if ct == 'void' {
			ct = 'int'
		}
		g.writeln('\t${ct} ${g.cname(f.name)};')
	}
}

// preseed_struct_fn_ptr_types supports preseed struct fn ptr types handling for FlatGen.
fn (mut g FlatGen) preseed_struct_fn_ptr_types() {
	for struct_name, fields in g.tc.structs {
		for f in fields {
			if c_abi_fn := g.struct_field_c_abi_fn_ptr_type(struct_name, f.name) {
				g.resolve_fn_ptr_type(c_abi_fn)
				continue
			}
			g.preseed_fn_ptr_type(f.typ)
		}
	}
}

fn (mut g FlatGen) preseed_global_fn_ptr_types() {
	for _, typ in g.global_types {
		g.preseed_fn_ptr_type(typ)
	}
}

fn (mut g FlatGen) preseed_fn_signature_fn_ptr_types() {
	for _, params in g.fn_decl_param_types {
		for typ in params {
			g.preseed_fn_ptr_type(typ)
		}
	}
	for _, typ in g.fn_decl_ret_types {
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
		wrote = g.emit_option_typedefs_for_type(f.typ) || wrote
	}
	if wrote {
		g.writeln('')
	}
}

fn (mut g FlatGen) emit_option_typedefs_for_type(typ types.Type) bool {
	if typ is types.OptionType || typ is types.ResultType {
		opt_name := g.optional_type_name(typ)
		if opt_name == 'Optional' {
			return false
		}
		if val_type := g.needed_optional_types[opt_name] {
			return g.emit_optional_typedef(opt_name, val_type)
		}
		return false
	}
	if typ is types.ArrayFixed {
		return g.emit_option_typedefs_for_type(typ.elem_type)
	}
	if typ is types.Alias {
		return g.emit_option_typedefs_for_type(typ.base_type)
	}
	return false
}
