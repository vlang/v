module transform

import v3.flat
import v3.types

fn inferred_anonymous_struct_type_exists(tc &types.TypeChecker, name string) bool {
	return name in tc.structs || name in tc.type_aliases || name in tc.sum_types
		|| name in tc.enum_names || name in tc.interface_names
}

// materialize_inferred_anonymous_structs gives a concrete declaration to an
// inferred `struct { field: expression }` literal whose field expressions were
// not syntactically typed by the parser. Semantic checking has resolved those
// expressions by this point. Do this before parallel transform preparation so
// all workers and cgen see one immutable set of aggregate declarations.
fn (mut t Transformer) materialize_inferred_anonymous_structs() bool {
	if isnil(t.tc) {
		return false
	}
	t.tc.ensure_private_transform_structs()
	mut materialized := false
	mut inferred_by_shape := map[string]string{}
	original_node_count := t.a.nodes.len
	for idx in 0 .. original_node_count {
		node := t.a.nodes[idx]
		match node.kind {
			.struct_init {
				if node.value != 'struct' || node.children_count == 0 {
					continue
				}
				source_file := t.a.source_files[node.pos.id] or { continue }
				cur_file := source_file.name
				cur_module := t.tc.file_modules[cur_file] or { '' }
				// Contextual checking may already have selected one of the declared
				// anonymous structs with this field shape. Keep that nominal identity;
				// transform_struct_init will apply it when this literal is lowered.
				if checked_type := t.tc.expr_type(flat.NodeId(idx)) {
					if transform_is_anonymous_struct_name(t.tc.type_name(checked_type)) {
						continue
					}
				}
				mut semantic_fields := []types.StructField{cap: int(node.children_count)}
				mut valid := true
				for fi in 0 .. node.children_count {
					field := t.a.child_node(&node, fi)
					if field.kind != .field_init || field.value.len == 0
						|| field.children_count != 1 {
						valid = false
						break
					}
					value_id := t.a.child(field, 0)
					field_type := t.tc.expr_type(value_id) or { t.tc.resolve_type(value_id) }
					field_type_name := t.tc.type_name(field_type)
					if field_type is types.Unknown || field_type is types.Void
						|| field_type_name in ['', 'unknown', 'void', 'struct'] {
						valid = false
						break
					}
					semantic_fields << types.StructField{
						name: field.value
						typ: field_type
					}
				}
				if !valid {
					continue
				}
				mut shape_parts := []string{cap: semantic_fields.len}
				for semantic_field in semantic_fields {
					field_type_name := t.tc.type_name(semantic_field.typ)
					shape_parts << '${semantic_field.name.len}:${semantic_field.name}:${field_type_name.len}:${field_type_name}'
				}
				shape := '${cur_module.len}:${cur_module}:${shape_parts.join(',')}'
				if semantic_name := inferred_by_shape[shape] {
					t.a.nodes[idx].value = semantic_name.all_after_last('.')
					t.a.nodes[idx].typ = semantic_name
					t.tc.register_synth_type(flat.NodeId(idx), types.Struct{
						name: semantic_name
					})
					continue
				}
				mut field_ids := []flat.NodeId{cap: semantic_fields.len}
				for fi, semantic_field in semantic_fields {
					source_field := t.a.child_node(&node, fi)
					field_ids << t.a.add_node(flat.Node{
						kind: .field_decl
						value: semantic_field.name
						typ: t.tc.type_name(semantic_field.typ)
						pos: source_field.pos
					})
				}
				base_name := 'AnonStruct_v3_inferred_${idx}'
				mut name := base_name
				mut semantic_name := name
				if cur_module.len > 0 && cur_module !in ['main', 'builtin'] {
					semantic_name = '${cur_module}.${name}'
				}
				mut collision_suffix := 1
				for inferred_anonymous_struct_type_exists(t.tc, semantic_name) {
					name = '${base_name}_${collision_suffix}'
					semantic_name = name
					if cur_module.len > 0 && cur_module !in ['main', 'builtin'] {
						semantic_name = '${cur_module}.${name}'
					}
					collision_suffix++
				}
				if cur_file.len > 0 {
					t.a.add_node(flat.Node{
						kind: .file
						value: cur_file
					})
				}
				if cur_module.len > 0 {
					t.a.add_node(flat.Node{
						kind: .module_decl
						value: cur_module
					})
				}
				children_start := t.a.children.len
				for field_id in field_ids {
					t.a.children << field_id
				}
				t.a.add_node(flat.Node{
					kind: .struct_decl
					value: name
					children_start: children_start
					children_count: flat.child_count(field_ids.len)
					pos: node.pos
				})
				t.tc.structs[semantic_name] = semantic_fields
				t.tc.struct_modules[semantic_name] = cur_module
				t.tc.struct_files[semantic_name] = cur_file
				t.tc.register_short_type_name(semantic_name)
				inferred_by_shape[shape] = semantic_name
				t.a.nodes[idx].value = name
				t.a.nodes[idx].typ = semantic_name
				t.tc.register_synth_type(flat.NodeId(idx), types.Struct{
					name: semantic_name
				})
				materialized = true
			}
			else {}
		}
	}
	return materialized
}
