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
