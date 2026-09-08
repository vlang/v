module transform

import v3.flat
import v3.token
import v3.types

fn add_inferred_anonymous_struct(mut a flat.FlatAst, file_id int) (flat.NodeId, flat.NodeId) {
	pos := token.new_pos(file_id, 0)
	value_id := a.add_node(flat.Node{
		kind: .int_literal
		value: '1'
		pos: pos
	})
	field_children_start := a.children.len
	a.children << value_id
	field_id := a.add_node(flat.Node{
		kind: .field_init
		value: 'item'
		children_start: field_children_start
		children_count: 1
		pos: pos
	})
	struct_children_start := a.children.len
	a.children << field_id
	struct_id := a.add_node(flat.Node{
		kind: .struct_init
		value: 'struct'
		children_start: struct_children_start
		children_count: 1
		pos: pos
	})
	return struct_id, value_id
}

fn test_inferred_anonymous_struct_uses_position_source_file() {
	mut a := flat.FlatAst.new()
	dep_file := '/tmp/dep/item.v'
	main_file := '/tmp/project/main.v'
	a.source_files[1] = token.File.unindexed(dep_file, 1)
	a.source_files[2] = token.File.unindexed(main_file, 1)

	// Parser file markers follow all nodes belonging to that file. The ownership
	// of either literal therefore cannot be inferred from allocation order.
	dep_struct_id, dep_value_id := add_inferred_anonymous_struct(mut a, 1)
	a.add_node(flat.Node{
		kind: .file
		value: dep_file
	})
	main_struct_id, main_value_id := add_inferred_anonymous_struct(mut a, 2)
	a.add_node(flat.Node{
		kind: .file
		value: main_file
	})

	mut tc := types.TypeChecker.new(&a)
	tc.file_modules[dep_file] = 'dep'
	tc.file_modules[main_file] = 'main'
	tc.register_synth_type(dep_value_id, types.Type(types.int_))
	tc.register_synth_type(main_value_id, types.Type(types.int_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.materialize_inferred_anonymous_structs()

	dep_name := 'dep.AnonStruct_v3_inferred_${dep_struct_id}'
	main_name := 'AnonStruct_v3_inferred_${main_struct_id}'
	assert tc.struct_files[dep_name] == dep_file
	assert tc.struct_modules[dep_name] == 'dep'
	assert tc.struct_files[main_name] == main_file
	assert tc.struct_modules[main_name] == 'main'
	assert a.nodes[int(dep_struct_id)].typ == dep_name
	assert a.nodes[int(main_struct_id)].typ == main_name
}

fn test_inferred_anonymous_structs_reuse_their_semantic_shape() {
	mut a := flat.FlatAst.new()
	main_file := '/tmp/project/main.v'
	a.source_files[1] = token.File.unindexed(main_file, 1)
	first_struct_id, first_value_id := add_inferred_anonymous_struct(mut a, 1)
	second_struct_id, second_value_id := add_inferred_anonymous_struct(mut a, 1)
	a.add_node(flat.Node{
		kind: .file
		value: main_file
	})

	mut tc := types.TypeChecker.new(&a)
	tc.file_modules[main_file] = 'main'
	tc.register_synth_type(first_value_id, types.Type(types.int_))
	tc.register_synth_type(second_value_id, types.Type(types.int_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.materialize_inferred_anonymous_structs()

	shared_name := 'AnonStruct_v3_inferred_${first_struct_id}'
	assert a.nodes[int(first_struct_id)].typ == shared_name
	assert a.nodes[int(second_struct_id)].typ == shared_name
	mut synthesized_declarations := 0
	for node in a.nodes {
		if node.kind == .struct_decl && node.value == shared_name {
			synthesized_declarations++
		}
	}
	assert synthesized_declarations == 1
}

fn test_inferred_anonymous_struct_name_does_not_replace_user_struct() {
	mut a := flat.FlatAst.new()
	main_file := '/tmp/project/main.v'
	a.source_files[1] = token.File.unindexed(main_file, 1)
	struct_id, value_id := add_inferred_anonymous_struct(mut a, 1)
	colliding_name := 'AnonStruct_v3_inferred_${struct_id}'
	a.add_node(flat.Node{
		kind: .struct_decl
		value: colliding_name
		pos: token.new_pos(1, 0)
	})
	a.add_node(flat.Node{
		kind: .file
		value: main_file
	})

	user_fields := [types.StructField{
		name: 'user_field'
		typ: types.Type(types.string_)
	}]
	mut tc := types.TypeChecker.new(&a)
	tc.file_modules[main_file] = 'main'
	tc.structs[colliding_name] = user_fields
	tc.register_synth_type(value_id, types.Type(types.int_))
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert t.materialize_inferred_anonymous_structs()

	generated_name := '${colliding_name}_1'
	assert a.nodes[int(struct_id)].typ == generated_name
	assert tc.structs[colliding_name] == user_fields
	assert tc.structs[generated_name][0].name == 'item'
}

fn test_prepared_selfhost_transform_materializes_before_repreparing() {
	mut a := flat.FlatAst.new()
	main_file := '/tmp/project/main.v'
	a.source_files[1] = token.File.unindexed(main_file, 1)
	struct_id, value_id := add_inferred_anonymous_struct(mut a, 1)
	a.add_node(flat.Node{
		kind: .file
		value: main_file
	})

	mut tc := types.TypeChecker.new(&a)
	tc.file_modules[main_file] = 'main'
	tc.register_synth_type(value_id, types.Type(types.int_))
	mut prepared := prepare_selfhost_transform(&a, &tc, true)
	_, _, errors, _, _ := transform_prepared_selfhost_owned(mut prepared, mut a, &tc,
		map[string]bool{}, unsafe { nil })

	assert errors.len == 0
	name := 'AnonStruct_v3_inferred_${struct_id}'
	assert a.nodes[int(struct_id)].typ == name
	assert name in prepared.transformer.structs
}
