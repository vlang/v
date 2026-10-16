module types

import v.flat

fn test_sum_pattern_cache_keeps_file_imports_and_negative_results_separate() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['one.Item'] = []StructField{}
	tc.structs['two.Item'] = []StructField{}
	tc.sum_types.set('one.Choice', ['one.Item'])
	tc.sum_types.set('two.Choice', ['two.Item'])
	tc.cur_module = 'main'
	tc.cur_file = 'one.v'
	tc.register_file_import('dep', 'one')
	tc.cur_file = 'two.v'
	tc.register_file_import('dep', 'two')
	sum_id := flat.source_type_id('dep.Choice')
	one_id := flat.source_type_id('one.Item')
	two_id := flat.source_type_id('two.Item')
	for _ in 0 .. 3 {
		tc.cur_file = 'one.v'
		assert tc.sum_variant_type_for_pattern(sum_id, one_id)? == one_id
		assert tc.sum_variant_type_for_pattern(sum_id, two_id) == none
		tc.cur_file = 'two.v'
		assert tc.sum_variant_type_for_pattern(sum_id, one_id) == none
		assert tc.sum_variant_type_for_pattern(sum_id, two_id)? == two_id
	}
}

fn test_sum_pattern_cache_keeps_module_resolution_separate_in_one_file() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.structs['one.Item'] = []StructField{}
	tc.structs['two.Item'] = []StructField{}
	tc.sum_types.set('one.Choice', ['one.Item'])
	tc.sum_types.set('two.Choice', ['two.Item'])
	tc.cur_file = 'shared.v'
	sum_id := flat.source_type_id('Choice')
	pattern_id := flat.source_type_id('Item')
	one_id := flat.source_type_id('one.Item')
	two_id := flat.source_type_id('two.Item')
	for _ in 0 .. 3 {
		tc.cur_module = 'one'
		assert tc.sum_variant_type_for_pattern(sum_id, pattern_id)? == one_id
		tc.cur_module = 'two'
		assert tc.sum_variant_type_for_pattern(sum_id, pattern_id)? == two_id
	}
}

fn test_pointer_walk_continues_after_a_recursive_struct_field() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	cycle := Type(Struct{ name_id: flat.source_type_id('Cycle') })
	tc.structs['Cycle'] = [
		StructField{ name: 'next', typ: cycle },
		StructField{ name: 'value', typ: Type(Pointer{ base_type: Type(int_) }) },
	]
	mut visited := map[u32]bool{}
	assert tc.type_contains_pointer(cycle, mut visited)
	tc.structs['Cycle'] = [StructField{ name: 'next', typ: cycle }]
	visited.clear()
	assert !tc.type_contains_pointer(cycle, mut visited)
}
