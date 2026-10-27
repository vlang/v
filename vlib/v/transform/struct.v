module transform

import v.flat
import v.types

// transform_field_init_expr transforms transform field init expr data for transform.
fn (mut t Transformer) transform_field_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	val_id := t.a.child(&node, 0)
	new_val := t.transform_expr(val_id)
	if t.rewrite_one_child_in_place(id, new_val) {
		return id
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
		source_value:   node.source_value
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
	info := t.lookup_struct_info(node.source_value_id()) or {
		// Unknown struct: fall back to generic child transform
		return t.transform_struct_children(id, node)
	}
	struct_type := node.source_value_id()
	// Build a field name -> type lookup from the struct definition
	mut field_types := map[string]u32{}
	if !t.lean_struct_init_fields {
		for f in info.fields {
			field_types[f.name] = t.lookup_struct_field_type(struct_type, f.name) or { f.typ }
		}
	}
	mut field_ids := []flat.NodeId{}
	mut promoted_fields := map[string][]flat.NodeId{}
	mut promoted_paths := map[string][]FieldInfo{}
	mut prelude := []flat.NodeId{}
	t.drain_pending(mut prelude)
	// Source (child) position of the last field whose value hoists a value branch. A preceding
	// field value is snapshotted before that field materializes its branch prelude, so struct
	// fields evaluate in source order — a nested block/`if`/`match` tail otherwise runs its
	// prelude before the struct initializer while earlier field values stay inline.
	mut last_hoisting_field := -1
	for i in 0 .. node.children_count {
		child := t.a.nodes[int(t.a.child(&node, i))]
		if child.kind == .field_init && child.children_count > 0
			&& t.operand_hoists_value_branch(t.a.child(&child, 0)) {
			last_hoisting_field = i
		}
	}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .field_init && child.children_count > 0 {
			val_id := t.a.child(&child, 0)
			val_node := t.a.nodes[int(val_id)]
			mut field_name := if child.value.len > 0 {
				child.value
			} else if i < info.fields.len {
				info.fields[i].name
			} else {
				''
			}
			if child.value.starts_with('@') && flat.source_type_query(info.name).has(.c_prefix)
				&& !info.fields.any(it.name == child.value) {
				plain_name := child.value[1..]
				if info.fields.any(it.name == plain_name) {
					field_name = plain_name
				}
			}
			mut target_field_name := field_name
			mut field_type := if t.lean_struct_init_fields {
				t.lookup_struct_field_type(struct_type, field_name) or {
					struct_info_field_type(info, field_name)
				}
			} else {
				field_types[field_name] or { u32(0) }
			}
			mut promoted_key := ''
			if field_type == 0 {
				// An embed is initialized under its short name, as a field access
				// reads it: a cross-module one (`aa.Inner`) as `Outer{ Inner: ... }`
				// and a generic one (`Holder[int]`) as `IntHolder{ Holder: ... }`.
				for f in info.fields {
					if f.name != field_name && embedded_selector_matches(flat.source_type_id(field_name), flat.source_type_id(f.name)) {
						target_field_name = f.name
						field_type = if t.lean_struct_init_fields {
							t.lookup_struct_field_type(struct_type, f.name) or { f.typ }
						} else {
							field_types[f.name] or { f.typ }
						}
						break
					}
				}
			}
			if field_type == 0 {
				if path := t.struct_field_path_for_field(struct_type, field_name) {
					if path.len > 0 {
						promoted_key = promoted_field_path_key(path)
						promoted_paths[promoted_key] = path
						embedded_owner_source_id := path[path.len - 1].typ
						embedded_owner_source_id_2 := embedded_owner_source_id

						if field_name.starts_with('@') {
							if embedded_info := t.lookup_struct_info(embedded_owner_source_id_2) {
								plain_name := field_name[1..]
								if flat.source_type_query(embedded_info.name).has(.c_prefix)
									&& !embedded_info.fields.any(it.name == field_name)
									&& embedded_info.fields.any(it.name == plain_name) {
									target_field_name = plain_name
								}
							}
						}
						field_type = t.lookup_struct_field_type(embedded_owner_source_id, target_field_name) or {
							t.checker_struct_field_type_name(struct_type, field_name) or { u32(0) }
						}
					}
				}
			}
			if val_node.kind == .array_literal && t.is_fixed_array_type(field_type) {
				t.set_node_typ(int(val_id), field_type)
			}
			value_type_source_id := t.node_type(val_id)
			mut numeric_field := false
			if t.validating_generic_spec && !isnil(t.tc) {
				expected := types.unalias_type(t.tc.parse_type(field_type))
				numeric_field = expected.is_integer() || expected.is_float()
			}
			numeric_payload := if numeric_field {
				t.or_expr_receiver_unwrapped_type(val_id) or { u32(0) }
			} else {
				u32(0)
			}

			sum_field_type_source_id := t.struct_field_sum_type(field_type, info.module)

			enum_field_type_source_id := t.enum_type_name_for_expected(field_type, info.module)
			fixed_to_dynamic := flat.source_type_shape(field_type).has(.array_type) && t.is_fixed_array_type(value_type_source_id)
			shared_interface_source := !isnil(t.tc) && t.is_interface_type(field_type)
				&& t.tc.struct_field_is_shared(node.source_value_id(), target_field_name)
				&& t.expr_is_shared_value(val_id)
			// Check if the value is an enum shorthand and the field type is an enum
			mut new_val := if shared_interface_source {
				// Keep the concrete shared source intact so cgen can make the interface
				// wrapper borrow both its value and its synchronization guard.
				t.transform_expr(val_id)
			} else if val_node.kind == .enum_val && enum_field_type_source_id != 0 {
				t.transform_enum_shorthand(val_id, val_node, enum_field_type_source_id)
			} else if fixed_to_dynamic {
				t.fixed_array_value_to_field_array(val_id, value_type_source_id, field_type)
			} else if sum_field_type_source_id != 0 {
				t.wrap_sum_value(val_id, sum_field_type_source_id)
			} else if flat.source_type_shape(field_type).has(.pointer) {
				t.transform_expr_for_type(val_id, field_type)
			} else if numeric_field {
				// Resolve generic calls before checking their concrete result, while
				// retaining its source type before the numeric field conversion.
				if numeric_payload != 0 && !t.generic_arg_is_unresolved(numeric_payload) {
					t.transform_expr_for_type(val_id, numeric_payload)
				} else {
					t.transform_expr(val_id)
				}
			} else if field_type != 0 {
				t.transform_expr_for_type(val_id, field_type)
			} else {
				t.transform_expr(val_id)
			}
			if numeric_field {
				mut actual := if numeric_payload != 0 && !t.generic_arg_is_unresolved(numeric_payload) {
					numeric_payload
				} else {
					t.specialized_expr_type_name(new_val)
				}
				// A matching reference value is implicitly dereferenced in a value field.
				// Apply that existing conversion before validation, without allowing a
				// pointer to another numeric (or nonnumeric) type to bypass the check.
				if flat.source_type_shape(actual).has(.pointer)
					&& t.normalize_type_alias(flat.source_type_syntax(actual).raw_base()) == t.normalize_type_alias(field_type) {
					new_val = t.coerce_transformed_expr_to_type(new_val, val_id, field_type)
					actual = t.specialized_expr_type_name(new_val)
				}
				if actual != 0 && actual != flat.source_type_unknown && !t.generic_arg_is_unresolved(actual)
					&& !t.tc.expr_subtree_has_node_error(val_id)
					&& !t.tc.expr_subtree_has_node_error(new_val)
					&& !t.tc.expr_compatible(val_id, t.tc.parse_type(actual), t.tc.parse_type(field_type)) {
					t.tc.record_transform_error(child_id, val_node.pos, 'cannot assign to field `${field_name}`: expected `${flat.source_type_name(field_type)}`, not `${flat.source_type_name(actual)}`')
				}
			}
			if !shared_interface_source && sum_field_type_source_id == 0 && field_type != 0 {
				new_val = t.coerce_transformed_expr_to_type(new_val, val_id, field_type)
			}
			if !shared_interface_source && field_type != 0 && !fixed_to_dynamic {
				new_val = t.clone_borrowed_storage_projection(val_id, new_val, field_type)
			}
			// Snapshot a preceding field value before a later field hoists its branch prelude,
			// so this value is read in source order rather than after that prelude.
			if i < last_hoisting_field && !t.is_pure_constant_expr(new_val) {
				mut snap_typ := t.node_type(new_val)
				if snap_typ == 0 {
					snap_typ = field_type
				}
				new_val = t.snapshot_transformed_expr_for_reuse(new_val, snap_typ, 'struct_field')
			}
			t.drain_pending(mut prelude)
			if int(child_id) in t.local_closure_field_cleanups {
				mut closure_type := field_type
				if closure_type == 0 {
					closure_type = t.node_type(new_val)
				}
				closure_name := t.new_temp('field_closure')
				t.set_var_type(closure_name, closure_type)
				prelude << t.make_decl_assign_typed(closure_name, new_val, closure_type)
				prelude << t.make_local_closure_cleanup_defer(closure_name)
				new_val = t.make_ident(closure_name)
				t.set_node_typ(int(new_val), closure_type)
			}
			new_field := if t.inplace_struct_fields && target_field_name == child.value
				&& t.rewrite_one_child_in_place(child_id, new_val) {
				child_id
			} else {
				fi_start := t.a.children.len
				t.a.children << new_val
				t.a.add_node(flat.Node{
					kind:           .field_init
					op:             child.op
					children_start: fi_start
					children_count: 1
					pos:            child.pos
					value:          target_field_name
					typ:            child.typ
				})
			}
			if promoted_key.len > 0 {
				mut promoted := promoted_fields[promoted_key] or { []flat.NodeId{} }
				promoted << new_field
				promoted_fields[promoted_key] = promoted
			} else {
				field_ids << new_field
			}
		} else {
			field_ids << child_id
		}
	}
	if promoted_fields.len > 0 {
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		prelude.clear()
	}
	// Merge sibling paths before wrapping their shared parent. Emitting a whole
	// parent for each path would overwrite fields initialized through other paths.
	for {
		mut depth := 1
		for _, path in promoted_paths {
			if path.len > depth {
				depth = path.len
			}
		}
		if depth == 1 {
			break
		}
		for key in promoted_paths.keys() {
			path := promoted_paths[key]
			if path.len != depth {
				continue
			}
			parent_path := path[..depth - 1].clone()
			parent_key := promoted_field_path_key(parent_path)
			field := t.make_promoted_struct_field_init(path[depth - 1..], promoted_fields[key])
			mut siblings := promoted_fields[parent_key] or { []flat.NodeId{} }
			siblings << field
			promoted_fields[parent_key] = siblings
			promoted_paths[parent_key] = parent_path
			promoted_fields.delete(key)
			promoted_paths.delete(key)
		}
	}
	for key, promoted in promoted_fields {
		path := promoted_paths[key] or { []FieldInfo{} }
		if path.len == 0 {
			continue
		}
		field_ids << t.make_promoted_struct_field_init(path, promoted)
	}
	output_typ := if node.typ != 0 { node.typ } else { struct_type }
	new_id := if t.inplace_struct_fields && t.rewrite_children_in_place(id, field_ids) {
		if output_typ != node.typ {
			t.set_node_typ(int(id), output_typ)
		}
		id
	} else {
		start := t.a.children.len
		for fid in field_ids {
			t.a.children << fid
		}
		t.a.add_node(flat.Node{
			kind:           .struct_init
			op:             node.op
			children_start: start
			children_count: flat.child_count(field_ids.len)
			pos:            node.pos
			value:          node.value
			source_value:   node.source_value
			typ:            output_typ
		})
	}
	final_id := t.add_missing_struct_defaults(new_id, t.a.nodes[int(new_id)])
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	return final_id
}

fn struct_info_field_type(info StructInfo, name string) u32 {
	for field in info.fields {
		if field.name == name {
			return field.typ
		}
	}
	return 0
}

fn promoted_field_path_key(path []FieldInfo) string {
	mut parts := []string{cap: path.len}
	for field in path {
		parts << field.name
	}
	return parts.join('\n')
}

fn (t &Transformer) promoted_struct_init_type(field FieldInfo) u32 {
	return t.trim_pointer_type(t.normalize_type_alias(field.typ))
}

struct PromotedStructDefaultMerge {
	init         flat.NodeId
	materialized bool
}

fn (mut t Transformer) clone_promoted_default_in_decl_scope(id flat.NodeId, module_name string, file string) flat.NodeId {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return id
	}
	node := t.a.nodes[int(id)]
	if node.kind in [.ident, .selector] {
		name := t.expr_key(id)
		if name.len > 0 {
			if key := t.const_type_key_in_context(name, module_name, file) {
				return t.a.add_node(flat.Node{
					kind:   .ident
					op:     node.op
					pos:    node.pos
					value:  key
					typ:    node.typ
					is_mut: node.is_mut
					flags:  flat.node_flags(node.skip_ownership_drops(), false)
				})
			}
		}
	}
	mut children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		children << t.clone_promoted_default_in_decl_scope(t.a.child(&node, i), module_name, file)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		pos:            node.pos
		value:          node.value
		source_value:   node.source_value
		typ:            node.typ
		payload:        node.payload
		is_mut:         node.is_mut
		children_start: start
		children_count: flat.child_count(children.len)
		flags:          flat.node_flags(node.skip_ownership_drops(), false) |
			(node.flags & flat.node_flag_literal_interpolation_text)
	})
}

fn (mut t Transformer) promoted_struct_init_field_value(field FieldInfo, init flat.NodeId, materialized bool) flat.NodeId {
	normalized_type_source_id := t.normalize_type_alias(field.typ)
	normalized_type_shape := flat.source_type_shape(normalized_type_source_id)
	if !normalized_type_shape.has(.pointer) {
		return init
	}
	value := t.make_prefix(.amp, init)
	if materialized {
		pointee_type_source_id := t.trim_pointer_type(normalized_type_source_id)

		dup := t.make_memdup_call_for_type(value, pointee_type_source_id)
		return t.make_cast(normalized_type_source_id, dup, normalized_type_source_id)
	}
	t.set_node_typ(int(value), normalized_type_source_id)
	return value
}

fn (mut t Transformer) merge_promoted_struct_default(init_id flat.NodeId, default_id flat.NodeId, struct_type_id u32) PromotedStructDefaultMerge {
	if int(default_id) < 0 {
		return PromotedStructDefaultMerge{
			init: init_id
		}
	}
	default_init := t.a.nodes[int(default_id)]
	if default_init.kind != .struct_init {
		return PromotedStructDefaultMerge{
			init:         t.materialize_promoted_struct_default(init_id, default_id, struct_type_id)
			materialized: true
		}
	}
	init := t.a.nodes[int(init_id)]
	mut provided := map[string]flat.NodeId{}
	mut merged_provided := map[string]flat.NodeId{}
	info := t.lookup_struct_info(struct_type_id) or { StructInfo{} }
	for i in 0 .. init.children_count {
		field_id := t.a.child(&init, i)
		field := t.a.nodes[int(field_id)]
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			''
		}
		if field_name.len > 0 {
			provided[field_name] = field_id
		}
	}
	mut missing_defaults := []flat.NodeId{}
	old_module := t.cur_module
	old_file := t.cur_file
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	for i in 0 .. default_init.children_count {
		field_id := t.a.child(&default_init, i)
		field := t.a.nodes[int(field_id)]
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			''
		}
		if field_name.len == 0 {
			continue
		}
		if provided_id := provided[field_name] {
			provided_field := t.a.nodes[int(provided_id)]
			if field.kind == .field_init && field.children_count > 0
				&& provided_field.kind == .field_init && provided_field.children_count > 0 {
				// A promoted child initializer is partial; recursively retain fields from
				// the parent aggregate's declared struct-literal default.
				default_value_id := t.a.child(&field, 0)
				provided_value_id := t.a.child(&provided_field, 0)
				default_value := t.a.nodes[int(default_value_id)]
				provided_value := t.a.nodes[int(provided_value_id)]
				if default_value.kind == .struct_init && provided_value.kind == .struct_init {
					field_type_source_id := t.lookup_struct_field_type(struct_type_id, field_name) or {
						field.typ
					}

					child_type_source_id := t.trim_pointer_type(t.normalize_type_alias(field_type_source_id))

					merged_child := t.merge_promoted_struct_default(provided_value_id,
						default_value_id, child_type_source_id)
					child_start := t.a.children.len
					t.a.children << merged_child.init
					merged_provided[field_name] = t.a.add_node(flat.Node{
						kind:           .field_init
						op:             provided_field.op
						children_start: child_start
						children_count: 1
						pos:            provided_field.pos
						value:          field_name
						typ:            (if provided_field.typ != 0 {
							provided_field.typ
						} else {
							field_type_source_id
						})
					})
				}
			}
			continue
		}
		mut missing_id := field_id
		if field.kind == .field_init && field.children_count > 0 {
			field_type_source_id_2 := t.lookup_struct_field_type(struct_type_id, field_name) or { field.typ }

			default_value_id := t.a.child(&field, 0)
			if source_file := t.a.source_files[field.pos.id] {
				t.cur_file = source_file.name
			}
			decl_value_id := t.clone_promoted_default_in_decl_scope(default_value_id, info.module,
				t.cur_file)
			default_value := t.a.nodes[int(decl_value_id)]
			enum_field_type_source_id_2 := t.enum_type_name_for_expected(field_type_source_id_2, info.module)
			new_value := if default_value.kind == .enum_val && enum_field_type_source_id_2 != 0 {
				t.transform_enum_shorthand(decl_value_id, default_value, enum_field_type_source_id_2)
			} else if t.is_sum_type_name(field_type_source_id_2) {
				t.wrap_sum_value(decl_value_id, field_type_source_id_2)
			} else {
				t.transform_expr_for_type(decl_value_id, field_type_source_id_2)
			}
			field_start := t.a.children.len
			t.a.children << new_value
			missing_id = t.a.add_node(flat.Node{
				kind:           .field_init
				op:             field.op
				children_start: field_start
				children_count: 1
				pos:            field.pos
				value:          field_name
				typ:            (if field.typ != 0 {
					field.typ
				} else {
					field_type_source_id_2
				})
			})
		}
		missing_defaults << missing_id
		provided[field_name] = missing_id
	}
	t.cur_module = old_module
	t.cur_file = old_file
	if missing_defaults.len == 0 && merged_provided.len == 0 {
		return PromotedStructDefaultMerge{
			init: init_id
		}
	}
	start := t.a.children.len
	for field_id in missing_defaults {
		t.a.children << field_id
	}
	for i in 0 .. init.children_count {
		field_id := t.a.child(&init, i)
		field := t.a.nodes[int(field_id)]
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			''
		}
		t.a.children << (merged_provided[field_name] or { field_id })
	}
	return PromotedStructDefaultMerge{
		init: t.a.add_node(flat.Node{
			kind:           .struct_init
			op:             init.op
			children_start: start
			children_count: missing_defaults.len + int(init.children_count)
			pos:            init.pos
			value:          init.value
			source_value:   init.source_value
			typ:            init.typ
		})
	}
}

fn (mut t Transformer) materialize_promoted_struct_default(init_id flat.NodeId, default_id flat.NodeId, struct_type_id u32) flat.NodeId {
	init := t.a.nodes[int(init_id)]
	if init.kind != .struct_init {
		return init_id
	}
	info := t.lookup_struct_info(struct_type_id) or { return init_id }
	old_module := t.cur_module
	old_file := t.cur_file
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	default_node := t.a.nodes[int(default_id)]
	if source_file := t.a.source_files[default_node.pos.id] {
		t.cur_file = source_file.name
		if !isnil(t.tc) {
			t.cur_module = t.tc.file_modules[t.cur_file] or { t.cur_module }
		}
	}
	default_value := t.transform_expr_for_type(default_id, struct_type_id)
	t.cur_module = old_module
	t.cur_file = old_file
	tmp_name := t.new_temp('promoted_default')
	t.pending_stmts << t.make_decl_assign_typed(tmp_name, default_value, struct_type_id)
	for i in 0 .. init.children_count {
		field := t.a.child_node(&init, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			''
		}
		if field_name.len == 0 {
			continue
		}
		field_type_source_id := t.lookup_struct_field_type(struct_type_id, field_name) or { field.typ }

		value := t.a.child(field, 0)
		selector := t.make_selector(t.make_ident(tmp_name), field_name, field_type_source_id)
		t.pending_stmts << t.make_assign(selector, value)
	}
	result := t.make_ident(tmp_name)
	t.set_node_typ(int(result), struct_type_id)
	return result
}

fn (mut t Transformer) make_promoted_struct_field_init(path []FieldInfo, leaf_fields []flat.NodeId) flat.NodeId {
	mut init_start := t.a.children.len
	for fid in leaf_fields {
		t.a.children << fid
	}
	mut cur_type := t.promoted_struct_init_type(path[path.len - 1])
	mut cur_init := t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: init_start
		children_count: flat.child_count(leaf_fields.len)
		source_value:   cur_type
		typ:            cur_type
	})
	mut merged := t.merge_promoted_struct_default(cur_init, path[path.len - 1].default_expr,
		cur_type)
	cur_init = merged.init
	mut cur_init_materialized := merged.materialized
	for rev in 0 .. path.len - 1 {
		idx := path.len - 2 - rev
		parent := path[idx]
		child := path[idx + 1]
		field_value := t.promoted_struct_init_field_value(child, cur_init, cur_init_materialized)
		field_start := t.a.children.len
		t.a.children << field_value
		field := t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: field_start
			children_count: 1
			value:          child.name
			typ:            child.typ
		})
		init_start = t.a.children.len
		t.a.children << field
		cur_type = t.promoted_struct_init_type(parent)
		cur_init = t.a.add_node(flat.Node{
			kind:           .struct_init
			children_start: init_start
			children_count: 1
			source_value:   cur_type
			typ:            cur_type
		})
		merged = t.merge_promoted_struct_default(cur_init, parent.default_expr, cur_type)
		cur_init = merged.init
		cur_init_materialized = merged.materialized
	}
	root := path[0]
	root_value := t.promoted_struct_init_field_value(root, cur_init, cur_init_materialized)
	field_start := t.a.children.len
	t.a.children << root_value
	return t.a.add_node(flat.Node{
		kind:           .field_init
		children_start: field_start
		children_count: 1
		value:          root.name
		typ:            root.typ
	})
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
			field_ids << if t.inplace_struct_fields
				&& t.rewrite_one_child_in_place(child_id, new_val) {
				child_id
			} else {
				fi_start := t.a.children.len
				t.a.children << new_val
				t.a.add_node(flat.Node{
					kind:           .field_init
					op:             child.op
					children_start: fi_start
					children_count: 1
					pos:            child.pos
					value:          child.value
					source_value:   child.source_value
					typ:            child.typ
				})
			}
		} else {
			field_ids << child_id
		}
	}
	if t.inplace_struct_fields && t.rewrite_children_in_place(id, field_ids) {
		output_typ := (if node.typ != 0 { node.typ } else { node.source_value_id() })
		if output_typ != node.typ {
			t.set_node_typ(int(id), output_typ)
		}
		return id
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
		source_value:   node.source_value
		typ:            (if node.typ != 0 {
			node.typ
		} else {
			node.source_value_id()
		})
	})
}

fn (mut t Transformer) infer_bare_generic_struct_init_type(node flat.Node) ?u32 {
	if node.source_value_id() == 0 || flat.source_type_shape(node.source_value_id()).has(.bracketed) {
		return none
	}
	params := t.generic_struct_param_names_for_base(node.value_text())
	if params.len == 0 {
		return none
	}
	info := t.lookup_struct_info(node.source_value_id()) or { return none }
	mut inferred := map[string]u32{}
	for i in 0 .. node.children_count {
		field := t.a.child_node(&node, i)
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		field_name := if field.value.len > 0 {
			field.value
		} else if i < info.fields.len {
			info.fields[i].name
		} else {
			continue
		}
		mut field_type_id := u32(0)
		for struct_field in info.fields {
			if struct_field.name == field_name {
				field_type_id = struct_field.typ
				break
			}
		}
		if field_type_id == 0 {
			continue
		}
		value_type_source_id_2 := t.generic_struct_init_value_type(t.a.child(field, 0))

		if value_type_source_id_2 != 0 {
			infer_generic_type_args(field_type_id, value_type_source_id_2, mut inferred)
		}
	}
	mut parts := []u32{cap: params.len + 1}
	parts << node.source_value_id()
	for param in params {
		arg_source_id := inferred[param] or { return none }
		if arg_source_id == 0 || t.generic_arg_is_unresolved(arg_source_id) {
			return none
		}
		parts << arg_source_id
	}
	return flat.source_type_composition(.generic, parts)
}

fn (mut t Transformer) generic_struct_init_value_type(id flat.NodeId) u32 {
	mut typ := t.generic_call_arg_type_for_inference(id)
	if typ == 0 {
		typ = t.node_type(id)
	}
	return match typ {
		transform_identity_int_literal { flat.source_type_int }
		transform_identity_float_literal { flat.source_type_f64 }
		else { typ }
	}
}

fn (mut t Transformer) specialize_struct_default_expr(struct_type_id u32, default_id flat.NodeId) flat.NodeId {
	syntax := flat.source_type_syntax(struct_type_id)
	args := syntax.generic_app_args()
	if !syntax.generic_app_valid() || args.len == 0 {
		return default_id
	}
	params := t.generic_struct_param_names_for_base_id(syntax.generic_app_base())
	if params.len == 0 || params.len != args.len {
		return default_id
	}
	old_params := t.active_generic_params.clone()
	t.clear_node_type_memo()
	t.active_generic_params = params
	clone_id := t.clone_generic_node(default_id, args)
	t.clear_node_type_memo()
	t.active_generic_params = old_params
	return clone_id
}

// nested_generic_defaults_need_lowering finds omitted value fields whose generic defaults
// need specialization before the backend emits the enclosing struct's default value.
fn (t &Transformer) nested_generic_defaults_need_lowering(type_name_id u32, mut visited map[u32]bool) bool {
	shape := flat.source_type_shape(type_name_id)
	if type_name_id == 0 || shape.has(.pointer) || shape.has(.option_type)
		|| shape.has(.result_type) || shape.has(.map_type) || shape.has(.array_type)
		|| type_name_id in visited {
		return false
	}
	normalized_source_id := t.normalize_type_alias(type_name_id)
	normalized_shape := flat.source_type_shape(normalized_source_id)
	if normalized_source_id == 0 || normalized_shape.has(.pointer)
		|| normalized_shape.has(.option_type) || normalized_shape.has(.result_type)
		|| normalized_shape.has(.map_type) || normalized_shape.has(.array_type) {
		return false
	}
	if t.is_fixed_array_type(normalized_source_id) {
		return t.nested_generic_defaults_need_lowering(fixed_array_elem_type(normalized_source_id), mut visited)
	}
	if normalized_source_id in visited {
		return false
	}
	visited[type_name_id] = true
	visited[normalized_source_id] = true
	info := t.lookup_struct_info(normalized_source_id) or { return false }
	syntax := flat.source_type_syntax(normalized_source_id)
	for field in info.fields {
		if int(field.default_expr) >= 0 {
			if syntax.generic_app_valid() && syntax.generic_app_args().len > 0 { return true }
			continue
		}
		field_type_source_id := t.lookup_struct_field_type(normalized_source_id, field.name) or { field.typ }

		if t.nested_generic_defaults_need_lowering(field_type_source_id, mut visited) {
			return true
		}
	}
	return false
}

// add_missing_struct_defaults lowers omitted defaults, including nested generic fields.
fn (mut t Transformer) add_missing_struct_defaults(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.source_value_id() == 0 {
		return id
	}
	info := t.lookup_struct_info(node.source_value_id()) or { return id }
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
	mut missing_defaults := map[string]flat.NodeId{}
	resolved_node_type_source_id := t.normalize_type_alias(node.source_value_id())
	syntax := flat.source_type_syntax(resolved_node_type_source_id)
	mut lower_generic_defaults := syntax.generic_app_valid() && syntax.generic_app_args().len > 0
	for field in info.fields {
		if field.name in provided { continue }
		if int(field.default_expr) >= 0 {
			missing_defaults[field.name] = field.default_expr
			continue
		}
		field_type_source_id := t.lookup_struct_field_type(node.source_value_id(), field.name) or { field.typ }
		mut visited := map[u32]bool{}
		if t.nested_generic_defaults_need_lowering(field_type_source_id, mut visited) {
			normalized_field_type_source_id := t.normalize_type_alias(field_type_source_id)

			missing_defaults[field.name] = if t.is_fixed_array_type(normalized_field_type_source_id) {
				t.make_fixed_array_init(normalized_field_type_source_id)
			} else {
				t.a.add_node(flat.Node{
					kind:         .struct_init
					source_value: field_type_source_id
					typ:          field_type_source_id
					pos:          node.pos
				})
			}
			lower_generic_defaults = true
		}
	}
	if missing_defaults.len == 0 {
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		// The node passes through untouched, but downstream consumers (e.g.
		// the sum-wrap decision for `return SNull{}`) need its type text.
		if node.typ == 0 && node.source_value_id() != 0 {
			t.set_node_typ(int(id), node.source_value_id())
		}
		return id
	}
	old_module := t.cur_module
	old_file := t.cur_file
	// Imported defaults must retain their declaration module while resolving consts, globals,
	// and function names. Cgen can emit ordinary defaults itself, but callable literals need
	// transform-time lowering into named functions before cgen sees them.
	imported_decl := info.module.len > 0 && info.module !in ['main', 'builtin']
		&& info.module != old_module
	mut has_missing_callable_default := false
	if imported_decl {
		for field in info.fields {
			if field.name in provided || int(field.default_expr) < 0 {
				continue
			}
			default_node := t.a.nodes[int(field.default_expr)]
			if default_node.kind in [.fn_literal, .lambda_expr] {
				has_missing_callable_default = true
				break
			}
		}
	}
	if imported_decl && !has_missing_callable_default && !lower_generic_defaults {
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		return id
	}
	if info.module.len > 0 {
		t.cur_module = info.module
	}
	// Field defaults are declaration-scope expressions. Caller locals with the same
	// name as an imported module (for example `seed`) must not turn module calls in a
	// reused default into receiver calls.
	saved_var_types := t.var_types.clone()
	saved_fn_value_locals := t.fn_value_locals.clone()
	saved_mut_param_values := t.mut_param_values.clone()
	saved_fixed_array_param_values := t.fixed_array_param_values.clone()
	saved_interface_var_concrete_types := t.interface_var_concrete_types.clone()
	saved_addr_lvalue_pointer_locals := t.addr_lvalue_pointer_locals.clone()
	saved_orm_initialized_fields := t.orm_initialized_fields.clone()
	saved_sql_query_data_aliases := t.sql_query_data_aliases.clone()
	t.reset_var_types()
	mut added := false
	for field in info.fields {
		if field.name !in missing_defaults {
			continue
		}
		t.cur_file = old_file
		field_type_source_id_2 := t.lookup_struct_field_type(node.source_value_id(), field.name) or { field.typ }

		default_id := t.specialize_struct_default_expr(resolved_node_type_source_id, missing_defaults[field.name])
		default_node := t.a.nodes[int(default_id)]
		if source_file := t.a.source_files[default_node.pos.id] {
			t.cur_file = source_file.name
		}
		if imported_decl && !lower_generic_defaults && default_node.kind !in [
			.fn_literal,
			.lambda_expr,
		] {
			continue
		}
		enum_field_type_source_id_3 := t.enum_type_name_for_expected(field_type_source_id_2, info.module)
		new_val := if default_node.kind == .enum_val && enum_field_type_source_id_3 != 0 {
			t.transform_enum_shorthand(default_id, default_node, enum_field_type_source_id_3)
		} else if t.is_sum_type_name(field_type_source_id_2) {
			// A sum-type field default (e.g. `typ_expr Expr = EmptyExpr{}`) must be
			// wrapped into the sum, not emitted as the bare variant. wrap_sum_value
			// is a no-op when the value already is the sum type.
			t.wrap_sum_value(default_id, field_type_source_id_2)
		} else {
			t.transform_expr_for_type(default_id, field_type_source_id_2)
		}
		t.drain_pending(mut prelude)
		fi_start := t.a.children.len
		t.a.children << new_val
		field_ids << t.a.add_node(flat.Node{
			kind:           .field_init
			children_start: fi_start
			children_count: 1
			value:          field.name
			typ:            field_type_source_id_2
		})
		provided[field.name] = true
		added = true
	}
	t.restore_var_types(saved_var_types)
	t.fn_value_locals = saved_fn_value_locals.clone()
	t.mut_param_values = saved_mut_param_values.clone()
	t.fixed_array_param_values = saved_fixed_array_param_values.clone()
	t.interface_var_concrete_types = saved_interface_var_concrete_types.clone()
	t.addr_lvalue_pointer_locals = saved_addr_lvalue_pointer_locals.clone()
	t.orm_initialized_fields = saved_orm_initialized_fields.clone()
	t.sql_query_data_aliases = saved_sql_query_data_aliases.clone()
	t.cur_module = old_module
	t.cur_file = old_file
	if !added {
		for stmt in prelude {
			t.pending_stmts << stmt
		}
		// The node passes through untouched, but downstream consumers (e.g.
		// the sum-wrap decision for `return SNull{}`) need its type text.
		if node.typ == 0 && node.source_value_id() != 0 {
			t.set_node_typ(int(id), node.source_value_id())
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
		source_value:   node.source_value
		typ:            (if node.typ != 0 {
			node.typ
		} else {
			node.source_value_id()
		})
	})
	for stmt in prelude {
		t.pending_stmts << stmt
	}
	return final_id
}

// lookup_struct_info resolves lookup struct info information for transform.
fn (t &Transformer) lookup_struct_info(name_id u32) ?StructInfo {
	name_comparison_type_id := name_id
	name_syntax := flat.source_type_syntax(name_id)
	// `main.Foo` is the explicit lock spelling for a program-module type (used so a
	// bare concrete generic argument is not rebased into a callee module with a
	// same-named type). Resolve it against the exact bare table key so it keeps its
	// own identity instead of being requalified into the active module.
	main_shape := flat.source_type_shape(name_syntax.main_base())
	if flat.source_type_query(name_id).has(.main_prefix) && !main_shape.has(.dotted)
		&& !main_shape.has(.bracketed) {
		bare := flat.source_type_name(name_syntax.main_base())
		// The bare table is first-wins across modules, so it can hold an imported
		// homonym: the explicit `main.` lock may only accept program-module entries.
		if info := t.structs[bare] {
			if info.module.len == 0 || info.module == 'main' {
				return info
			}
		}
	}
	// A bare spelling belongs to the file that wrote it: `import iam { Token }`
	// must select `iam.Token` even when another imported module declares a
	// same-named type, otherwise field defaults and aliases of the homonym leak
	// into the literal.
	// `Token{}` and the generic application `Token[int]{}` both have to resolve
	// their base through the writing file's imports; the candidate may also be a
	// type alias (`import iam { Alias }`, `type Alias = Real`).
	mut scoped_name := name_id
	if t.cur_file.len > 0 {
		lookup_id := if name_syntax.generic_app_valid() {
			name_syntax.generic_app_base()
		} else {
			name_id
		}
		if !flat.source_type_shape(lookup_id).has(.dotted) {
			if resolved_source_id := t.selective_import_type_name_for_file(t.cur_file, lookup_id) {
				if name_syntax.generic_app_valid() {
					// Keep the arguments; the generic branch below specializes the
					// resolved base with them.
					scoped_name = flat.source_type_composition_two(.concat, resolved_source_id,
						name_syntax.generic_app_legacy_suffix())
				} else {
					if info := t.lookup_struct_info_direct(resolved_source_id) {
						return info
					}
					if alias_target_source_id := t.alias_target_type_preserving_main_lock(resolved_source_id) {
						if info := t.lookup_struct_info_direct(alias_target_source_id) {
							return info
						}
					}
				}
			}
		}
	}
	if alias_target_source_id := t.alias_target_type_preserving_main_lock(name_id) {
		alias_target_comparison_type_id_2 := alias_target_source_id

		if alias_target_comparison_type_id_2 != name_comparison_type_id {
			if info := t.lookup_struct_info(alias_target_source_id) {
				return info
			}
		}
	}
	syntax := flat.source_type_syntax(scoped_name)
	args := syntax.generic_app_args()
	if syntax.generic_app_valid() {
		if base_info := t.lookup_struct_info_direct(syntax.generic_app_base()) {
			params := t.generic_struct_param_names_for_base_id(syntax.generic_app_base())
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
					name:      name_id
					module:    base_info.module
					is_params: base_info.is_params
					is_c_anon: base_info.is_c_anon
					fields:    fields
				}
			}
		}
	}
	if info := t.lookup_struct_info_preferred(name_comparison_type_id) {
		return info
	}
	// Resolve a qualified import alias (`m.Config` -> `some.mod.Config`) before the
	// short-name fallback below, so a same-named local type cannot shadow the imported
	// one. `resolve_imported_type_name` only matches dotted alias names, so bare names
	// fall through to the direct lookup unchanged.
	if imported_source_id := t.resolve_imported_type_name(name_comparison_type_id) {
		if info := t.lookup_struct_info_direct(imported_source_id) {
			return info
		}
	}
	// Resolve a module-local alias before the bare short-name fallback. A local
	// `Context = C.sgl_context` must not inherit defaults from an imported
	// `gg.Context` that happens to own the global short-name entry.
	normalized_comparison_type_id_2 := t.normalize_type_alias(name_id)
	if normalized_comparison_type_id_2 != name_comparison_type_id {
		if info := t.lookup_struct_info_direct(normalized_comparison_type_id_2) {
			return info
		}
	}
	// A monomorphized generic instantiated from another module references the concrete
	// type by its bare name (`BoolConfig` from main, while transforming the declaring
	// module); the module-qualified lookup above misses those, so fall back to the
	// direct lookup, which tries the bare name too.
	if info := t.lookup_struct_info_direct(name_comparison_type_id) {
		return info
	}
	// Mark-used runs before transform and can prune a struct declaration whose value is
	// only observed through an interface box. The checker field table deliberately
	// survives that pruning, so use it as the authoritative fallback for structural
	// operations such as semantic equality.
	if info := t.lookup_checker_struct_info(name_comparison_type_id) {
		return info
	}
	return none
}

fn (t &Transformer) bare_struct_name_is_local_to_current_module(name_id u32) bool {
	if name_id == 0 || flat.source_type_shape(name_id).has(.dotted) {
		return false
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qname := flat.source_type_name(flat.source_type_composition_two(.qualified,
			flat.source_type_id(t.cur_module), name_id))
		return qname in t.structs || (!isnil(t.tc) && qname in t.tc.structs)
	}
	name := flat.source_type_name(name_id)
	if info := t.structs[name] {
		return info.module.len == 0 || info.module in ['main', 'builtin']
	}
	if isnil(t.tc) || name !in t.tc.structs {
		return false
	}
	module_name := t.tc.struct_modules[name] or { '' }
	return module_name.len == 0 || module_name in ['main', 'builtin']
}

// bare_sum_type_name_is_local_to_current_module is the sum type counterpart of
// bare_struct_name_is_local_to_current_module. The transformer also indexes imported
// sum types by their short name, so only the checker's table tells them apart.
fn (t &Transformer) bare_sum_type_name_is_local_to_current_module(name_id u32) bool {
	if name_id == 0 || flat.source_type_shape(name_id).has(.dotted) || isnil(t.tc) {
		return false
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		return flat.source_type_name(flat.source_type_composition_two(.qualified,
			flat.source_type_id(t.cur_module), name_id)) in t.tc.sum_types.ids
	}
	// The checker keys main and builtin declarations by their bare name only.
	return t.tc.sum_types.contains(flat.source_type_name(name_id))
}

fn (t &Transformer) checker_struct_lookup_name(name_id u32) u32 {
	if isnil(t.tc) || name_id == 0 { return 0 }
	name := flat.source_type_name(name_id)
	if name in t.tc.structs { return name_id }
	if flat.source_type_shape(name_id).has(.dotted) {
		syntax := flat.source_type_syntax(name_id)
		short_name := flat.source_type_name(syntax.unqualified())
		module_name := flat.source_type_name(syntax.qualifier())
		short_module := t.tc.struct_modules[short_name] or { '' }
		if short_name in t.tc.structs && short_module == module_name {
			return syntax.unqualified()
		}
		return 0
	}
	if t.cur_module.len > 0 && t.cur_module !in ['main', 'builtin'] {
		qualified_id := flat.source_type_composition_two(.qualified, flat.source_type_id(t.cur_module), name_id)
		if flat.source_type_name(qualified_id) in t.tc.structs { return qualified_id }
	}
	// A bare imported struct short name (e.g. `Vec2` from `import math.vec { Vec2 }`,
	// keyed `vec.Vec2`). The bare name survives only as a generic-params shadow, so
	// resolve it to the unique qualified struct whose short name matches. Without this
	// an operator/eq on the imported generic instance leaves a raw C struct binary op.
	return t.unique_qualified_struct_for_short(name_id) or { u32(0) }
}

// unique_qualified_struct_for_short resolves a bare struct short name to the unique
// module-qualified struct key that shares it (`Vec2` -> `vec.Vec2`), or none when
// absent or ambiguous. Scans the small generic-struct table.
fn (t &Transformer) unique_qualified_struct_for_short(name_id u32) ?u32 {
	if isnil(t.tc) || name_id == 0 || flat.source_type_shape(name_id).has(.dotted) {
		return none
	}
	if t.struct_short_name_index_ready {
		qualified_id := t.struct_short_name_index.type_id(flat.source_type_name(name_id)) or { return none }
		if qualified_id != flat.source_type_id(struct_short_name_ambiguous)
			&& !flat.source_type_shape(qualified_id).has(.bracketed) {
			return qualified_id
		}
		return none
	}
	mut found := u32(0)
	for sname, _ in t.tc.structs {
		// Match a module-qualified struct whose short name equals `name`, excluding
		// generic instances (`vec.Vec2[int]`) — only the plain declaration counts.
		sname_id := flat.source_type_id(sname)
		shape := flat.source_type_shape(sname_id)
		if !shape.has(.dotted) || shape.has(.bracketed)
			|| flat.source_type_syntax(sname_id).unqualified() != name_id {
			continue
		}
		if found != 0 && found != sname_id {
			return none
		}
		found = sname_id
	}
	if found != 0 {
		return found
	}
	return none
}

fn (t &Transformer) lookup_checker_struct_info(name_id u32) ?StructInfo {
	lookup_id := t.checker_struct_lookup_name(name_id)
	if lookup_id == 0 { return none }
	lookup_name := flat.source_type_name(lookup_id)
	checker_fields := t.tc.structs[lookup_name] or { return none }
	mut fields := []FieldInfo{cap: checker_fields.len}
	for field in checker_fields {
		field_type := field.typ.source_type_id()
		fields << FieldInfo{
			name:        field.name
			typ:         field_type
			raw_typ:     field_type
			is_embedded: field.is_embed
		}
	}
	return StructInfo{
		name:   flat.source_type_syntax(lookup_id).unqualified()
		module: t.tc.struct_modules[lookup_name] or { '' }
		fields: fields
	}
}

// struct_info_matches_qualifier reports whether `info` can be the struct that the
// qualified `name` (`mod.Type`, or an import alias of `mod`) refers to.
fn (t &Transformer) struct_info_matches_qualifier(info StructInfo, name_id u32) bool {
	qualifier := flat.source_type_syntax(name_id).qualifier()
	if info.module.len == 0 || info.module == 'main' {
		return qualifier == flat.source_type_id('main')
	}
	module_id := flat.source_type_id(info.module)
	if qualifier == module_id || qualifier == flat.source_type_syntax(module_id).unqualified() {
		return true
	}
	if imported_source_id := t.resolve_imported_type_name(name_id) {
		return flat.source_type_syntax(imported_source_id).qualifier() == module_id
	}
	return false
}

fn (t &Transformer) resolve_imported_type_name(name_id u32) ?u32 {
	if isnil(t.tc) || !flat.source_type_shape(name_id).has(.dotted)
		|| flat.source_type_query(name_id).has(.c_prefix) {
		return none
	}
	syntax := flat.source_type_syntax(name_id)
	if syntax.first_dot_prefix() == 0 { return none }
	alias := flat.source_type_name(syntax.first_dot_prefix())
	if mod := t.file_import_module(t.cur_file, alias) {
		if mod != alias {
			return flat.source_type_composition_two(.qualified, flat.source_type_id(mod),
				syntax.first_dot_suffix())
		}
	}
	if mod := t.tc.imports[alias] {
		if mod != alias {
			return flat.source_type_composition_two(.qualified, flat.source_type_id(mod),
				syntax.first_dot_suffix())
		}
	}
	return none
}

fn (t &Transformer) lookup_struct_info_preferred(name_id u32) ?StructInfo {
	name := flat.source_type_name(name_id)
	if flat.source_type_shape(name_id).has(.dotted) {
		if info := t.structs[name] {
			return info
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := flat.source_type_name(flat.source_type_composition_two(.qualified,
			flat.source_type_id(t.cur_module), name_id))
		if info := t.structs[qname] {
			return info
		}
	} else if info := t.structs[name] {
		return info
	}
	return none
}

fn (t &Transformer) lookup_struct_info_direct(name_id u32) ?StructInfo {
	name := flat.source_type_name(name_id)
	if flat.source_type_shape(name_id).has(.dotted) {
		if info := t.structs[name] {
			return info
		}
	} else if t.cur_module.len > 0 && t.cur_module != 'main' && t.cur_module != 'builtin' {
		qname := flat.source_type_name(flat.source_type_composition_two(.qualified,
			flat.source_type_id(t.cur_module), name_id))
		if info := t.structs[qname] {
			return info
		}
	}
	if info := t.structs[name] {
		return info
	}
	if flat.source_type_shape(name_id).has(.dotted) {
		short_name := flat.source_type_name(flat.source_type_syntax(name_id).unqualified())
		if info := t.structs[short_name] {
			// The bare table is first-wins across modules: its entry may be another
			// module's homonym, which must not answer for this qualified name.
			if t.struct_info_matches_qualifier(info, name_id) {
				return info
			}
		}
	} else if t.struct_short_name_index_ready {
		if qualified_source_id := t.struct_short_name_index.type_id(name) {
			qualified := flat.source_type_name(qualified_source_id)

			if qualified_source_id != flat.source_type_id(struct_short_name_ambiguous) {
				return t.structs[qualified]
			}
		}
	} else {
		mut matches := []StructInfo{}
		for qualified, info in t.structs {
			qualified_id := flat.source_type_id(qualified)
			if flat.source_type_shape(qualified_id).has(.dotted)
				&& flat.source_type_syntax(qualified_id).unqualified() == name_id {
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
fn (t &Transformer) struct_field_type(info StructInfo, field_name string) ?u32 {
	field := info.field(field_name) or { return none }
	return field.typ
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

fn (t &Transformer) embedded_field_for_direct_selector(info StructInfo, field_name string) ?FieldInfo {
	field_name_id := flat.source_type_id(field_name)
	for field in info.fields {
		if !t.is_embedded_field(field) {
			continue
		}
		if embedded_selector_matches(field_name_id, flat.source_type_id(field.name))
			|| embedded_selector_matches(field_name_id, field.typ)
			|| embedded_selector_matches(field_name_id, field.raw_typ) {
			return field
		}
	}
	return none
}

fn embedded_selector_matches(field_name u32, embedded_name u32) bool {
	clean := flat.source_type_syntax(transform_trimmed_type_id(embedded_name)).pointer_raw_base()
	if clean == 0 {
		return false
	}
	syntax := flat.source_type_syntax(clean)
	if clean == field_name || syntax.unqualified() == field_name {
		return true
	}
	return syntax.generic_app_valid() && (syntax.generic_app_base() == field_name
		|| flat.source_type_syntax(syntax.generic_app_base()).unqualified() == field_name)
}

// is_embedded_field reports whether is embedded field applies in transform.
fn (t &Transformer) is_embedded_field(field FieldInfo) bool {
	return field.is_embedded
}

fn field_decl_is_embedded(field flat.Node) bool {
	meta := field.generic_params()
	// An embed with a default value (`Inner = Inner{...}`) is parsed as a field without a
	// type and without the embed marker; the checker treats it as an embed as well
	// (source_field_decl_is_embed).
	return (meta.len > 0 && meta[0].contains('e')) || field.typ == 0 || field.value.len == 0
}

// transform_assoc_expr transforms transform assoc expr data for transform.
fn (mut t Transformer) transform_assoc_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.kind != .assoc || node.children_count == 0 {
		return id
	}
	base_id := t.a.child(&node, 0)
	base_node := t.a.nodes[int(base_id)]
	mut base_type := u32(0)
	if base_node.kind == .ident {
		base_type = t.var_type(base_node.value)
		if base_type == 0 {
			base_type = t.raw_var_type(base_node.value)
		}
	}
	if base_type == 0 {
		base_type = t.node_type(base_id)
	}
	mut assoc_type := node.source_value_id()
	if checked_type_source_id := t.checker_expr_type_name(id) {
		checked_type_source_id_2 := checked_type_source_id
		if assoc_type == 0 || flat.source_type_syntax(checked_type_source_id_2).unqualified() == flat.source_type_syntax(assoc_type).unqualified() {
			assoc_type = checked_type_source_id_2
		}
	}
	if assoc_type == 0 {
		assoc_type = base_type
	}
	if flat.source_type_shape(assoc_type).has(.pointer) {
		assoc_type = t.trim_pointer_type(assoc_type)
	}
	if _ := t.lookup_struct_info(assoc_type) {
		// use the explicit assoc type
	} else {
		mut clean_base_type := base_type
		if flat.source_type_shape(clean_base_type).has(.pointer) {
			clean_base_type = t.trim_pointer_type(clean_base_type)
		}
		if _ := t.lookup_struct_info(clean_base_type) {
			assoc_type = clean_base_type
		}
	}
	mut field_types := map[string]u32{}
	mut assoc_module := ''
	if info := t.lookup_struct_info(assoc_type) {
		assoc_module = info.module
		for field in info.fields {
			field_types[field.name] = t.lookup_struct_field_type(assoc_type, field.name) or { field.typ }
		}
	}

	tmp_name := t.new_temp('assoc')
	outer_pending := t.pending_stmts.clone()
	t.pending_stmts.clear()

	mut prelude := []flat.NodeId{}
	transformed_base := t.transform_expr(base_id)
	transformed_base_type_shape := flat.source_type_shape(t.node_type(transformed_base))
	base_is_loaded_pointer_value := base_node.kind == .ident
		&& t.pointer_value_rvalues[base_node.value]
	base := if flat.source_type_shape(base_type).has(.pointer) && transformed_base_type_shape.has(.pointer)
		&& !base_is_loaded_pointer_value {
		t.make_prefix(.mul, transformed_base)
	} else {
		transformed_base
	}
	t.drain_pending(mut prelude)
	init_base := t.assoc_mapped_base_init(base, base_type, assoc_type, mut prelude) or { base }
	prelude << t.make_decl_assign_typed(tmp_name, init_base, assoc_type)

	for i in 1 .. node.children_count {
		field_id := t.a.child(&node, i)
		field := t.a.nodes[int(field_id)]
		if field.kind != .field_init || field.children_count == 0 {
			continue
		}
		value_id := t.a.child(&field, 0)
		value_node := t.a.nodes[int(value_id)]
		field_type_id := field_types[field.value] or { u32(0) }
		field_type_source_id_3 := field_type_id
		field_type_shape := flat.source_type_shape(field_type_source_id_3)
		if value_node.kind == .array_literal && t.is_fixed_array_type(field_type_source_id_3) {
			t.set_node_typ(int(value_id), field_type_source_id_3)
		}
		value_type_source_id := t.node_type(value_id)

		enum_field_type_source_id_4 := t.enum_type_name_for_expected(field_type_source_id_3, assoc_module)
		sum_field_type_source_id_2 := t.struct_field_sum_type(field_type_source_id_3, assoc_module)

		fixed_to_dynamic := field_type_shape.has(.array_type) && t.is_fixed_array_type(value_type_source_id)
		mut value := if value_node.kind == .enum_val && enum_field_type_source_id_4 != 0 {
			t.transform_enum_shorthand(value_id, value_node, enum_field_type_source_id_4)
		} else if fixed_to_dynamic {
			t.fixed_array_value_to_field_array(value_id, value_type_source_id, field_type_source_id_3)
		} else if sum_field_type_source_id_2 != 0 {
			t.wrap_sum_value(value_id, sum_field_type_source_id_2)
		} else if field_type_source_id_3 != 0 {
			t.transform_expr_for_type(value_id, field_type_source_id_3)
		} else {
			t.transform_expr(value_id)
		}
		if field_type_source_id_3 != 0 && !fixed_to_dynamic {
			value = t.clone_borrowed_storage_projection(value_id, value, field_type_source_id_3)
		}
		t.drain_pending(mut prelude)
		prelude << t.make_assign(t.make_selector(t.make_ident(tmp_name), field.value, field_type_source_id_3),
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

fn (mut t Transformer) assoc_mapped_base_init(base flat.NodeId, base_type_id u32, assoc_type_id u32, mut prelude []flat.NodeId) ?flat.NodeId {
	source_type_source_id := t.trim_pointer_type(base_type_id)
	source_type_source_id_2 := source_type_source_id

	if source_type_source_id == 0 || assoc_type_id == u32(0)
		|| t.normalize_type_alias(source_type_source_id) == t.normalize_type_alias(assoc_type_id) {
		return none
	}
	target_info := t.lookup_struct_info(assoc_type_id) or { return none }
	source_info := t.lookup_struct_info(source_type_source_id_2) or { return none }
	if target_info.name == source_info.name {
		return none
	}
	mut source_fields := map[string]FieldInfo{}
	for field in source_info.fields {
		source_fields[field.name] = field
	}
	source_name := t.new_temp('assoc_src')
	prelude << t.make_decl_assign_typed(source_name, base, source_type_source_id)
	mut field_ids := []flat.NodeId{}
	for field in target_info.fields {
		source_field := source_fields[field.name] or { continue }
		target_type_source_id := t.lookup_struct_field_type(assoc_type_id, field.name) or { field.typ }

		source_field_type_source_id := t.lookup_struct_field_type(source_type_source_id, source_field.name) or {
			source_field.typ
		}

		if t.normalize_type_alias(target_type_source_id) != t.normalize_type_alias(source_field_type_source_id) {
			continue
		}
		value := t.make_selector(t.make_ident(source_name), source_field.name, source_field_type_source_id)
		field_ids << t.make_named_field_init(field.name, value, target_type_source_id)
	}
	if field_ids.len == 0 {
		return none
	}
	start := t.a.children.len
	for field_id in field_ids {
		t.a.children << field_id
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		children_start: start
		children_count: flat.child_count(field_ids.len)
		source_value:   assoc_type_id
		typ:            assoc_type_id
	})
}

// transform_amp_assoc_expr_for_type supports transform_amp_assoc_expr_for_type handling.
fn (mut t Transformer) transform_amp_assoc_expr_for_type(_id flat.NodeId, node flat.Node, target_type_id u32) ?flat.NodeId {
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
	if value_type == 0 {
		value_type = t.node_type(child_id)
	}
	if flat.source_type_shape(value_type).has(.pointer) {
		value_type = t.trim_pointer_type(value_type)
	}
	if value_type == 0 {
		return none
	}
	addr := t.make_prefix(.amp, value)
	dup := t.make_memdup_call_for_type(addr, value_type)
	mut ptr_type := target_type_id
	if ptr_type == 0 {
		ptr_type = node.typ
	}
	if !flat.source_type_shape(ptr_type).has(.pointer) {
		ptr_type = flat.source_type_composition_one(.pointer, value_type)
	}
	cast := t.make_cast(ptr_type, dup, ptr_type)
	t.set_node_typ(int(cast), ptr_type)
	return cast
}

// struct_field_sum_type supports struct field sum type handling for Transformer.
fn (t &Transformer) struct_field_sum_type(field_type_id u32, owner_module string) u32 {
	if flat.source_type_syntax(field_type_id).contains_union() {
		if sum_name_source_id := t.sum_type_for_union_text(field_type_id, owner_module) {
			return sum_name_source_id
		}
	}
	shape := flat.source_type_shape(field_type_id)
	if shape.has(.array_type) || shape.has(.map_type)
		|| t.is_fixed_array_type(field_type_id) {
		return u32(0)
	}
	if t.is_sum_type_name(field_type_id) {
		return field_type_id
	}
	if field_type_id == u32(0) || shape.has(.dotted) || owner_module == '' {
		return u32(0)
	}
	qname := flat.source_type_composition_two(.qualified, flat.source_type_id(owner_module), field_type_id)
	if t.sum_types.contains(flat.source_type_name(qname)) {
		return qname
	}
	return u32(0)
}

fn (t &Transformer) sum_type_for_union_text(field_type_id u32, owner_module string) ?u32 {
	raw_variants := flat.source_type_syntax(field_type_id).union_parts()
	if raw_variants.len < 2 {
		return none
	}
	mut variants := []u32{cap: raw_variants.len}
	for raw in raw_variants {
		variants << t.normalize_sum_variant_type(raw, owner_module, [])
	}
	for sum_name, sum_variants_id in t.sum_types.ids {
		sum_variants := sum_variants_id
		sum_name_id := flat.source_type_id(sum_name)
		if owner_module != '' && flat.source_type_shape(sum_name_id).has(.dotted)
			&& flat.source_type_syntax(sum_name_id).qualifier() != flat.source_type_id(owner_module) {
			continue
		}
		if sum_variants.len != variants.len {
			continue
		}
		mut matched := true
		for variant in variants {
			mut found := false
			for sum_variant in sum_variants {
				if t.union_variant_text_matches(sum_variant, variant) {
					found = true
					break
				}
			}
			if !found {
				matched = false
				break
			}
		}
		if matched {
			return sum_name_id
		}
	}
	return none
}

fn (t &Transformer) union_variant_text_matches(a u32, b u32) bool {
	if t.variant_names_match(a, b) { return true }
	if t.is_fixed_array_type(a) && t.is_fixed_array_type(b) {
		return t.resolved_fixed_array_canonical_type(a) == t.resolved_fixed_array_canonical_type(b)
	}
	return false
}

// fixed_array_value_to_array converts fixed array value to array data for transform.
fn (mut t Transformer) fixed_array_value_to_array(value_id flat.NodeId, fixed_type_id u32, array_type_id u32) flat.NodeId {
	return t.fixed_array_data_to_array(t.transform_expr(value_id), fixed_type_id, array_type_id)
}

// fixed_array_value_to_owned_array converts a fixed-array value whose dynamic destination
// takes ownership. A borrowed projection keeps its fixed source alive, so clone its elements
// directly into the dynamic array instead of cloning and dropping a shallow converted array.
fn (mut t Transformer) fixed_array_value_to_owned_array(value_id flat.NodeId, fixed_type_id u32, array_type_id u32) flat.NodeId {
	if isnil(t.tc) || !t.tc.ownership_expr_is_borrowed_projection(value_id) {
		return t.fixed_array_value_to_array(value_id, fixed_type_id, array_type_id)
	}
	elem_type := fixed_array_elem_type(fixed_type_id)
	len_expr := t.make_fixed_array_len_expr(fixed_type_id)
	source := t.stable_transformed_expr_for_reuse(t.transform_expr(value_id), fixed_type_id,
		'borrowed_fixed_array_source')
	out_name := t.new_temp('borrowed_fixed_array')
	idx_name := t.new_temp('borrowed_fixed_array_idx')
	t.pending_stmts << t.make_decl_assign_typed(out_name, t.make_array_new_call(elem_type,
		t.make_int_literal(0), len_expr), array_type_id)
	init := t.make_decl_assign_typed(idx_name, t.make_int_literal(0), flat.source_type_int)
	cond := t.make_infix(.lt, t.make_ident(idx_name), len_expr)
	post := t.make_expr_stmt(t.make_postfix(t.make_ident(idx_name), .inc))
	source_elem := t.make_index(source, t.make_ident(idx_name), elem_type)
	pending_start := t.pending_stmts.len
	cloned_elem := t.make_compiler_default_clone_value(source_elem, elem_type, true)
	mut body := t.pending_stmts[pending_start..].clone()
	t.pending_stmts = t.pending_stmts[..pending_start].clone()
	value_name := t.new_temp('borrowed_fixed_array_value')
	body << t.make_decl_assign_typed(value_name, cloned_elem, elem_type)
	body << t.make_expr_stmt(t.make_call_typed('array_push', [
		t.make_prefix(.amp, t.make_ident(out_name)),
		t.make_prefix(.amp, t.make_ident(value_name)),
	], flat.source_type_void))
	t.pending_stmts << t.make_for_stmt(init, cond, post, body, flat.Node{
		flags: flat.node_flag_skip_ownership_drops
	})
	result := t.make_ident(out_name)
	t.set_node_typ(int(result), array_type_id)
	return result
}

// fixed_array_value_to_field_array converts the fixed array value of a field initializer
// to the field's dynamic array type. Elements stored differently, such as the `Item` values
// of a `[2]Item` used for a `[]Named` field, are converted one by one, as for an argument;
// the other fixed arrays are copied as they are.
fn (mut t Transformer) fixed_array_value_to_field_array(value_id flat.NodeId, fixed_type_id u32, array_type_id u32) flat.NodeId {
	if converted := t.transform_array_value_for_dynamic_target(value_id, array_type_id) {
		return converted
	}
	return t.fixed_array_value_to_owned_array(value_id, fixed_type_id, array_type_id)
}

fn (mut t Transformer) fixed_array_value_to_array_no_alloc(value_id flat.NodeId, fixed_type_id u32, array_type_id u32) flat.NodeId {
	return t.fixed_array_data_to_array_no_alloc(t.transform_expr(value_id), fixed_type_id, array_type_id)
}

fn (mut t Transformer) fixed_array_data_to_array_no_alloc(data_id flat.NodeId, fixed_type_id u32, array_type_id u32) flat.NodeId {
	elem_type := fixed_array_elem_type(fixed_type_id)
	len_expr := t.make_fixed_array_len_expr(fixed_type_id)
	t.mark_fn_used('new_array_from_c_array_no_alloc')
	return t.make_array_storage_call('new_array_from_c_array_no_alloc', [
		len_expr,
		len_expr,
		t.make_sizeof_type(elem_type),
		data_id,
	], array_type_id)
}

fn (mut t Transformer) fixed_array_data_to_array(data_id flat.NodeId, fixed_type_id u32, array_type_id u32) flat.NodeId {
	elem_type := fixed_array_elem_type(fixed_type_id)
	len_expr := t.make_fixed_array_len_expr(fixed_type_id)
	return t.make_array_storage_call('new_array_from_c_array', [
		len_expr,
		len_expr,
		t.make_sizeof_type(elem_type),
		data_id,
	], array_type_id)
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
	if t.rewrite_children_in_place(id, new_children) {
		return id
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
		source_value:   node.source_value
		typ:            node.typ
	})
}

// transform_map_init_expr transforms .map_init nodes.
// Recursively transforms all child key/value expressions.
fn (mut t Transformer) transform_map_init_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	raw_type := (if node.source_value_id() != 0 {
		node.source_value_id()
	} else if node.typ != 0 {
		node.typ
	} else {
		t.node_type(id)
	})
	map_type_source_id := t.normalize_type_alias(raw_type)
	if t.is_optional_type_name(map_type_source_id) && node.children_count == 0 {
		payload_type_shape := flat.source_type_shape(t.optional_base_type(map_type_source_id))
		if payload_type_shape.has(.map_type) {
			return t.make_optional_none(t.qualify_optional_type(map_type_source_id))
		}
	}
	if flat.source_type_shape(map_type_source_id).has(.map_type) {
		mut map_node := node
		map_node.set_source_value_id(map_type_source_id)
		map_node.typ = map_type_source_id
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
	if t.rewrite_children_in_place(id, new_children) {
		return id
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
		source_value:   node.source_value
		typ:            node.typ
	})
}

fn (t &Transformer) resolve_imported_symbol_name(name string) ?string {
	if isnil(t.tc) || !name.contains('.') || name.starts_with('C.') {
		return none
	}
	dot := name.index_u8(`.`)
	if dot <= 0 {
		return none
	}
	alias := name[..dot]
	if mod := t.file_import_module(t.cur_file, alias) {
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
