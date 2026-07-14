module transform

import v3.flat

// transform_field_init_expr transforms transform field init expr data for transform.
fn (mut t Transformer) transform_field_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	val_id := t.a.child(&node, 0)
	val_node := t.a.nodes[int(val_id)]
	new_val := if inferred_sum := t.sum_type_for_field_variant(node.value, val_id, val_node) {
		t.wrap_sum_value(val_id, inferred_sum)
	} else {
		t.transform_expr(val_id)
	}
	start := t.a.children.len
	t.a.children << new_val
	return t.a.add_node(flat.Node{
		kind:           .field_init
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_struct_fields transforms struct initialization fields with enum resolution.
// For each .field_init child, transforms the value expression. If the struct field type
// is a known enum, resolves shorthand enum values (e.g. `.red` -> `Color.red`).
fn (mut t Transformer) transform_struct_fields(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.add_missing_struct_defaults(id, node)
	}
	info := t.lookup_struct_info(node.value) or {
		// Unknown struct: fall back to generic child transform
		return t.transform_struct_children(id, node)
	}
	// Build a field name -> type lookup from the struct definition
	mut field_types := map[string]string{}
	mut field_order := []string{cap: info.fields.len}
	for f in info.fields {
		field_types[f.name] = t.lookup_struct_field_type(node.value, f.name) or { f.typ }
		field_order << f.name
	}
	mut field_ids := []flat.NodeId{}
	mut promoted_fields := map[string][]flat.NodeId{}
	mut promoted_types := map[string]string{}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.children_count > 0 {
			val_id := t.a.child(&child, 0)
			val_node := t.a.nodes[int(val_id)]
			field_name := if child.value.len > 0 {
				child.value
			} else if i < field_order.len {
				field_order[i]
			} else {
				''
			}
			mut target_field_name := field_name
			mut field_type := field_types[field_name] or { '' }
			mut promoted_parent := ''
			if field_type.len == 0 {
				// A cross-module embed (`aa.Inner`) is initialized under its
				// short name: `Outer{ Inner: ... }`.
				for f in info.fields {
					if f.name.contains('.') && f.name.all_after_last('.') == field_name {
						target_field_name = f.name
						field_type = field_types[f.name] or { f.typ }
						break
					}
				}
			}
			if field_type.len == 0 {
				if embedded := t.embedded_field_for_promoted_field(info, field_name) {
					promoted_parent = embedded.name
					promoted_types[promoted_parent] = embedded.typ
					if embedded_info := t.lookup_struct_info(embedded.typ) {
						if promoted_type := t.struct_field_type(embedded_info, field_name) {
							field_type = promoted_type
						}
					}
				}
			}
			if val_node.kind == .array_literal && t.is_fixed_array_type(field_type) {
				t.set_node_typ(int(val_id), field_type)
			}
			value_type := t.node_type(val_id)
			sum_field_type := t.struct_field_sum_type(field_type, info.module)
			enum_field_type := t.enum_type_name_for_expected(field_type, info.module)
			// Check if the value is an enum shorthand and the field type is an enum
			mut new_val := if val_node.kind == .enum_val && enum_field_type.len > 0 {
				t.transform_enum_shorthand(val_id, val_node, enum_field_type)
			} else if field_type.starts_with('[]') && t.is_fixed_array_type(value_type) {
				t.fixed_array_value_to_array(val_id, value_type, field_type)
			} else if sum_field_type.len > 0 {
				t.wrap_sum_value(val_id, sum_field_type)
			} else if inferred_sum := t.sum_type_for_field_variant(field_name, val_id, val_node) {
				t.wrap_sum_value(val_id, inferred_sum)
			} else if field_type.starts_with('&') {
				t.transform_expr_for_type(val_id, field_type)
			} else if field_type.len > 0 {
				t.transform_expr_for_type(val_id, field_type)
			} else {
				t.transform_expr(val_id)
			}
			if sum_field_type.len == 0 && field_type.len > 0 {
				new_val = t.coerce_transformed_expr_to_type(new_val, val_id, field_type)
			}
			t.drain_pending(mut prelude)
			fi_start := t.a.children.len
			t.a.children << new_val
			new_field := t.a.add_node(flat.Node{
				kind:           .field_init
				op:             child.op
				children_start: fi_start
				children_count: 1
				pos:            child.pos
				value:          target_field_name
				typ:            child.typ
			})
			if promoted_parent.len > 0 {
				mut promoted := promoted_fields[promoted_parent] or { []flat.NodeId{} }
				promoted << new_field
				promoted_fields[promoted_parent] = promoted
			} else {
				field_ids << new_field
			}
		} else {
			field_ids << child_id
		}
	}
	for parent, promoted in promoted_fields {
		promoted_start := t.a.children.len
		for fid in promoted {
			t.a.children << fid
		}
		embedded_type := promoted_types[parent] or { parent }
		embedded_init := t.a.add_node(flat.Node{
			kind:           .struct_init
			children_start: promoted_start
			children_count: flat.child_count(promoted.len)
			value:          embedded_type
			typ:            embedded_type
		})
		fi_start := t.a.children.len
		t.a.children << embedded_init
		field_ids << t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: fi_start
			children_count: 1
			value:          parent
			typ:            embedded_type
		})
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { node.value }
	})
	final_id := t.add_missing_struct_defaults(new_id, t.a.nodes[int(new_id)])
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	return final_id
}

// transform_struct_children is a fallback for struct inits where the struct type is unknown.
// Transforms all field_init value expressions without enum resolution.
fn (mut t Transformer) transform_struct_children(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut field_ids := []flat.NodeId{}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.children_count > 0 {
			val_id := t.a.child(&child, 0)
			new_val := t.transform_expr(val_id)
			fi_start := t.a.children.len
			t.a.children << new_val
			field_ids << t.a.add_node(flat.Node{
				kind:           .field_init
				op:             child.op
				children_start: fi_start
				children_count: 1
				pos:            child.pos
				value:          child.value
				typ:            child.typ
			})
		} else {
			field_ids << child_id
		}
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { node.value }
	})
}

// add_missing_struct_defaults checks if any fields with default values are missing
// from the struct initialization. This is a hook point for future default-fill logic.
// Currently returns the node unchanged because StructInfo does not yet store default values.
fn (mut t Transformer) add_missing_struct_defaults(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.value.len == 0 {
		return id
	}
	info := t.lookup_struct_info(node.value) or { return id }
	mut provided := map[string]bool{}
	mut field_ids := []flat.NodeId{cap: int(node.children_count) + info.fields.len}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init {
			provided[child.value] = true
		}
		field_ids << child_id
	}
	old_module := t.cur_module
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	mut added := false
	for field in info.fields {
		if field.name in provided || int(field.default_expr) < 0 {
			continue
		}
		field_type := t.lookup_struct_field_type(node.value, field.name) or { field.typ }
		default_node := t.a.nodes[int(field.default_expr)]
		enum_field_type := t.enum_type_name_for_expected(field_type, info.module)
		new_val := if default_node.kind == .enum_val && enum_field_type.len > 0 {
			t.transform_enum_shorthand(field.default_expr, default_node, enum_field_type)
		} else if t.is_sum_type_name(field_type) {
			// A sum-type field default (e.g. `typ_expr Expr = EmptyExpr{}`) must be
			// wrapped into the sum, not emitted as the bare variant. wrap_sum_value
			// is a no-op when the value already is the sum type.
			t.wrap_sum_value(field.default_expr, field_type)
		} else {
			t.transform_expr_for_type(field.default_expr, field_type)
		}
		t.drain_pending(mut prelude)
		fi_start := t.a.children.len
		t.a.children << new_val
		field_ids << t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: fi_start
			children_count: 1
			value:          field.name
			typ:            field_type
		})
		provided[field.name] = true
		added = true
	}
	t.cur_module = old_module
	if !added {
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		// The node passes through untouched, but downstream consumers (e.g.
		// the sum-wrap decision for `return SNull{}`) need its type text.
		if node.typ.len == 0 && node.value.len > 0 {
			t.set_node_typ(int(id), node.value)
		}
		return id
	}
	start := t.a.children.len
	for fid in field_ids {
		t.a.children << fid
	}
	final_id := t.a.add_node(flat.Node{
		kind:           .struct_init
		op:             node.op
		children_start: start
		children_count: flat.child_count(field_ids.len)
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { node.value }
	})
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	return final_id
}

// lookup_struct_info resolves lookup struct info information for transform.
fn (t &Transformer) lookup_struct_info(name string) ?StructInfo {
	base, args, has_generic_args := generic_app_parts(name)
	if has_generic_args {
		if base_info := t.lookup_struct_info_direct(base) {
			params := t.generic_struct_param_names_for_base(base)
			if params.len == args.len && params.len > 0 {
				mut fields := []FieldInfo{cap: base_info.fields.len}
				for field in base_info.fields {
					fields << FieldInfo{
						name:         field.name
						typ:          substitute_generic_type_text_with_params(field.typ, args,
							params)
						raw_typ:      substitute_generic_type_text_with_params(field.raw_typ, args,
							params)
						default_expr: field.default_expr
						is_embedded:  field.is_embedded
					}
				}
				return StructInfo{
					name:      name
					module:    base_info.module
					is_params: base_info.is_params
					fields:    fields
				}
			}
		}
	}
	if info := t.lookup_struct_info_preferred(name) {
		return info
	}
	// Resolve a qualified import alias (`m.Config` -> `some.mod.Config`) before the
	// short-name fallback below, so a same-named local type cannot shadow the imported
	// one. `resolve_imported_type_name` only matches dotted alias names, so bare names
	// fall through to the direct lookup unchanged.
	if imported := t.resolve_imported_type_name(name) {
		if info := t.lookup_struct_info_direct(imported) {
			return info
		}
	}
	// Resolve a module-local alias before the bare short-name fallback. A local
	// `Context = C.sgl_context` must not inherit defaults from an imported
	// `gg.Context` that happens to own the global short-name entry.
	normalized := t.normalize_type_alias(name)
	if normalized != name {
		if info := t.lookup_struct_info_direct(normalized) {
			return info
		}
	}
	// A monomorphized generic instantiated from another module references the concrete
	// type by its bare name (`BoolConfig` from main, while transforming the declaring
	// module); the module-qualified lookup above misses those, so fall back to the
	// direct lookup, which tries the bare name too.
	if info := t.lookup_struct_info_direct(name) {
		return info
	}
	return none
}

fn (t &Transformer) resolve_imported_type_name(name string) ?string {
	if isnil(t.tc) || !name.contains('.') || name.starts_with('C.') {
		return none
	}
	dot := name.index_u8(`.`)
	if dot <= 0 {
		return none
	}
	alias := name[..dot]
	if mod := t.tc.file_imports[file_import_key(t.cur_file, alias)] {
		if mod != alias {
			return mod + name[dot..]
		}
	}
	if mod := t.tc.imports[alias] {
		if mod != alias {
			return mod + name[dot..]
		}
	}
	return none
}

fn (t &Transformer) lookup_struct_info_preferred(name string) ?StructInfo {
	if name.contains('.') {
		if name in t.structs {
			return t.structs[name]
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.structs {
			return t.structs[qname]
		}
	} else if name in t.structs {
		return t.structs[name]
	}
	return none
}

fn (t &Transformer) lookup_struct_info_direct(name string) ?StructInfo {
	if name.contains('.') {
		if name in t.structs {
			return t.structs[name]
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := '${t.cur_module}.${name}'
		if qname in t.structs {
			return t.structs[qname]
		}
	}
	if name in t.structs {
		return t.structs[name]
	}
	if name.contains('.') {
		short_name := name.all_after_last('.')
		if short_name in t.structs {
			return t.structs[short_name]
		}
	} else {
		mut matches := []StructInfo{}
		for qualified, info in t.structs {
			if qualified.contains('.') && qualified.all_after_last('.') == name {
				matches << info
			}
		}
		if matches.len == 1 {
			return matches[0]
		}
	}
	return none
}

// struct_field_type supports struct field type handling for Transformer.
fn (t &Transformer) struct_field_type(info StructInfo, field_name string) ?string {
	for field in info.fields {
		if field.name == field_name {
			return field.typ
		}
	}
	return none
}

// embedded_field_for_promoted_field
// supports helper handling in transform.
fn (t &Transformer) embedded_field_for_promoted_field(info StructInfo, field_name string) ?FieldInfo {
	for field in info.fields {
		if !t.is_embedded_field(field) {
			continue
		}
		embedded_info := t.lookup_struct_info(field.typ) or { continue }
		if _ := t.struct_field_type(embedded_info, field_name) {
			return field
		}
	}
	return none
}

// is_embedded_field reports whether is embedded field applies in transform.
fn (t &Transformer) is_embedded_field(field FieldInfo) bool {
	return field.is_embedded
}

fn field_decl_is_embedded(name string, typ string) bool {
	if name.len == 0 || typ.len == 0 {
		return false
	}
	short_typ := if typ.contains('.') { typ.all_after_last('.') } else { typ }
	short_name := if name.contains('.') { name.all_after_last('.') } else { name }
	return short_name == short_typ
}

// transform_assoc_expr transforms transform assoc expr data for transform.
fn (mut t Transformer) transform_assoc_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .assoc || node.children_count == 0 {
		return id
	}
	base_id := t.a.child(&node, 0)
	base_type := t.node_type(base_id)
	mut assoc_type := node.value
	if assoc_type.len == 0 {
		assoc_type = base_type
	}
	if assoc_type.starts_with('&') {
		assoc_type = assoc_type[1..]
	}
	mut field_types := map[string]string{}
	mut assoc_module := ''
	if info := t.lookup_struct_info(assoc_type) {
		assoc_module = info.module
		for field in info.fields {
			field_types[field.name] = t.lookup_struct_field_type(assoc_type, field.name) or {
				field.typ
			}
		}
	}

	tmp_name := t.new_temp('assoc')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	transformed_base := t.transform_expr(base_id)
	base := if base_type.starts_with('&') {
		t.make_prefix(.mul, transformed_base)
	} else {
		transformed_base
	}
	t.drain_pending(mut prelude)
	prelude << t.make_decl_assign_typed(tmp_name, base, assoc_type)

	for i in 1 .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := t.a.child(&field, 0)
		value_node := t.a.nodes[int(value_id)]
		field_type := field_types[field.value] or { '' }
		if value_node.kind == .array_literal && t.is_fixed_array_type(field_type) {
			t.set_node_typ(int(value_id), field_type)
		}
		value_type := t.node_type(value_id)
		enum_field_type := t.enum_type_name_for_expected(field_type, assoc_module)
		sum_field_type := t.struct_field_sum_type(field_type, assoc_module)
		value := if value_node.kind == .enum_val && enum_field_type.len > 0 {
			t.transform_enum_shorthand(value_id, value_node, enum_field_type)
		} else if field_type.starts_with('[]') && t.is_fixed_array_type(value_type) {
			t.fixed_array_value_to_array(value_id, value_type, field_type)
		} else if sum_field_type.len > 0 {
			t.wrap_sum_value(value_id, sum_field_type)
		} else if field_type.len > 0 {
			t.transform_expr_for_type(value_id, field_type)
		} else {
			t.transform_expr(value_id)
		}
		t.drain_pending(mut prelude)
		prelude << t.make_assign(t.make_selector(t.make_ident(tmp_name), field.value, field_type),
			value)
	}

	t.pending_stmts = outer_pending
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), assoc_type)
	return result
}

// transform_amp_assoc_expr_for_type supports transform_amp_assoc_expr_for_type handling.
fn (mut t Transformer) transform_amp_assoc_expr_for_type(_id flat.NodeId, node flat.Node, target_type string) ?flat.NodeId {
	if node.kind != .prefix || node.op != .amp || node.children_count != 1 {
		return none
	}
	child_id := t.a.child(&node, 0)
	child := t.a.nodes[int(child_id)]
	if child.kind != .assoc {
		return none
	}
	value := t.transform_assoc_expr(child_id, child)
	mut value_type := t.node_type(value)
	if value_type.len == 0 {
		value_type = t.node_type(child_id)
	}
	if value_type.starts_with('&') {
		value_type = value_type[1..]
	}
	if value_type.len == 0 {
		return none
	}
	addr := t.make_prefix(.amp, value)
	size := t.make_sizeof_type(value_type)
	dup := t.make_call_typed('memdup', arr2(addr, size), 'voidptr')
	mut ptr_type := target_type
	if ptr_type.len == 0 {
		ptr_type = node.typ
	}
	if !ptr_type.starts_with('&') {
		ptr_type = '&${value_type}'
	}
	cast := t.make_cast(ptr_type, dup, ptr_type)
	t.set_node_typ(int(cast), ptr_type)
	return cast
}

// struct_field_sum_type supports struct field sum type handling for Transformer.
fn (t &Transformer) struct_field_sum_type(field_type string, owner_module string) string {
	if field_type.starts_with('[]') || field_type.starts_with('map[')
		|| t.is_fixed_array_type(field_type) {
		return ''
	}
	if t.is_sum_type_name(field_type) {
		return field_type
	}
	if field_type.len == 0 || field_type.contains('.') || owner_module.len == 0 {
		return ''
	}
	qname := '${owner_module}.${field_type}'
	if qname in t.sum_types {
		return qname
	}
	return ''
}

// sum_type_for_field_variant supports sum type for field variant handling for Transformer.
fn (t &Transformer) sum_type_for_field_variant(field_name string, val_id flat.NodeId, val_node flat.Node) ?string {
	if field_name != 'info' {
		return none
	}
	mut variant := if val_node.kind in [.struct_init, .cast_expr, .assoc] && val_node.value.len > 0 {
		val_node.value
	} else if val_node.kind == .assoc && val_node.children_count > 0 {
		t.node_type(t.a.child(&val_node, 0))
	} else {
		t.node_type(val_id)
	}
	if variant.starts_with('&') {
		variant = variant[1..]
	}
	if variant.len == 0 {
		return none
	}
	short_variant := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	for sum_name, variants in t.sum_types {
		if sum_name != 'TypeInfo' && !sum_name.ends_with('.TypeInfo') {
			continue
		}
		for v in variants {
			short_v := if v.contains('.') { v.all_after_last('.') } else { v }
			if v == variant || short_v == short_variant {
				return sum_name
			}
		}
	}
	return none
}

// fixed_array_value_to_array converts fixed array value to array data for transform.
fn (mut t Transformer) fixed_array_value_to_array(value_id flat.NodeId, fixed_type string, array_type string) flat.NodeId {
	return t.fixed_array_data_to_array(t.transform_expr(value_id), fixed_type, array_type)
}

fn (mut t Transformer) fixed_array_data_to_array(data_id flat.NodeId, fixed_type string, array_type string) flat.NodeId {
	elem_type := fixed_array_elem_type(fixed_type)
	len_expr := t.make_fixed_array_len_expr(fixed_type)
	return t.make_call_typed('new_array_from_c_array', [
		len_expr,
		len_expr,
		t.make_sizeof_type(elem_type),
		data_id,
	], array_type)
}

// transform_array_init_expr transforms .array_init nodes (e.g. `[]int{len: n}`).
// Recursively transforms any child expressions (len, cap, init values).
fn (mut t Transformer) transform_array_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if fixed := t.transform_fixed_array_init_expr(node) {
		return fixed
	}
	lowered := t.lower_array_init_to_runtime(id, node)
	if lowered != id {
		return lowered
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .array_init
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

// transform_map_init_expr transforms .map_init nodes.
// Recursively transforms all child key/value expressions.
fn (mut t Transformer) transform_map_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	raw_type := if node.value.len > 0 {
		node.value
	} else if node.typ.len > 0 {
		node.typ
	} else {
		t.node_type(id)
	}
	map_type := t.normalize_type_alias(raw_type)
	if map_type.starts_with('map[') {
		mut map_node := node
		map_node.value = map_type
		map_node.typ = map_type
		return t.lower_map_init_to_runtime(id, map_node)
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .map_init
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}
