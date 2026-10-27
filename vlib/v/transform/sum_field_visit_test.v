module transform

import v.flat
import v.types

fn test_sum_field_visit_path_ignores_non_sums_and_restores_nested_paths() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	mut visited := map[u32]bool{}
	for name in ['int', 'string', '[]int', 'map[string]int', 'Missing'] {
		assert t.sum_shared_field_type_name_inner(flat.source_type_id(name), 'field', mut visited) == none
		assert visited.len == 0
	}
	tc.structs[flat.source_type_id('Leaf')] = [types.StructField{ name: 'field', typ: types.Type(types.int_) }]
	t.sum_types.set('Inner', ['Leaf'])
	t.sum_types.set('Left', ['Inner'])
	t.sum_types.set('Right', ['Inner'])
	t.sum_types.set('Diamond', ['Left', 'Right'])
	for _ in 0 .. 2 {
		assert t.sum_shared_field_type_name_inner(flat.source_type_id('Diamond'), 'field', mut visited)? == flat.source_type_int
		assert visited.len == 0
	}
	t.sum_types.set('Cycle', ['Cycle', 'Leaf'])
	assert t.sum_shared_field_type_name_inner(flat.source_type_id('Cycle'), 'field', mut visited) == none
	assert visited.len == 0
	visited[flat.source_type_id('Inner')] = true
	assert t.sum_shared_field_type_name_inner(flat.source_type_id('Inner'), 'field', mut visited) == none
	assert visited.len == 1
}
