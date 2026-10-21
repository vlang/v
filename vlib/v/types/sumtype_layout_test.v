module types

import v.flat

fn test_sumtype_layout_detects_value_cycles() {
	mut ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.sum_types.set('Tree', ['Branch', 'int'])
	tree := Type(SumType{ name_id: flat.source_type_id('Tree') })
	branch := Type(Struct{ name_id: flat.source_type_id('Branch') })
	tc.structs['Branch'] = [StructField{ name: 'child', typ: tree }]
	mut seen := map[u32]ValueLayoutState{}
	seen[flat.source_type_id('Tree')] = .active
	assert tc.value_type_has_cycle(branch, mut seen)
	pointer := Type(Pointer{ base_type: &Type(tree) })
	tc.structs['Branch'] = [StructField{ name: 'child', typ: pointer }]
	seen.clear()
	seen[flat.source_type_id('Tree')] = .active
	assert !tc.value_type_has_cycle(branch, mut seen)
	array := Type(Array{ elem_type: &Type(tree) })
	tc.structs['Branch'] = [StructField{ name: 'children', typ: array }]
	seen.clear()
	seen[flat.source_type_id('Tree')] = .active
	assert !tc.value_type_has_cycle(branch, mut seen)
}

fn test_recursive_type_metadata_retains_value_equality() {
	first := Type(Array{ elem_type: &Type(int_) })
	second := Type(Array{ elem_type: &Type(int_) })
	assert first == second
	assert semantic_type_hash(first) == semantic_type_hash(second)
	assert first != Type(Array{ elem_type: &Type(string_) })
}

fn test_sumtype_layout_retains_the_closing_field_and_type_path() {
	mut ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.sum_types.set('Tree', ['Branch', 'int'])
	tree := Type(SumType{ name_id: flat.source_type_id('Tree') })
	inner := Type(Struct{ name_id: flat.source_type_id('Inner') })
	leaf := Type(Struct{ name_id: flat.source_type_id('Leaf') })
	tc.structs['Leaf'] = [StructField{ name: 'value', typ: int_ }]
	tc.structs['Branch'] = [StructField{ name: 'leaf', typ: leaf },
		StructField{ name: 'inner', typ: inner }]
	tc.structs['Inner'] = [StructField{ name: 'child', typ: Type(OptionType{ base_type: &tree }) }]
	mut states := {
		flat.source_type_id('Tree'): ValueLayoutState.active
	}
	mut path := ValueLayoutPath{ types: [flat.source_type_id('Tree')] }
	target := tc.value_type_cycle(Type(Struct{ name_id: flat.source_type_id('Branch') }), mut states, mut path) or {
		panic('expected an inline layout cycle')
	}
	assert flat.source_type_name(target) == 'Tree'
	assert path.types.map(flat.source_type_name(it)) == ['Tree', 'Branch', 'Inner', 'Tree']
	assert path.fields.map(it.name) == ['inner', 'child']
	assert flat.source_type_name(path.fields.last().owner) == 'Inner'
}

fn test_sum_references_require_the_same_storage_type() {
	mut ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.structs['Item'] = []StructField{}
	tc.sum_types.set('Value', ['Item', 'int'])
	item := Type(Struct{ name_id: flat.source_type_id('Item') })
	sum := Type(SumType{ name_id: flat.source_type_id('Value') })
	item_ref := Type(Pointer{ base_type: &Type(item) })
	sum_ref := Type(Pointer{ base_type: &Type(sum) })
	assert !tc.type_compatible(item_ref, sum_ref)
	assert !tc.receiver_compatible(item_ref, sum_ref)
	actual := Type(Array{ elem_type: &Type(item_ref) })
	expected := Type(Array{ elem_type: &Type(sum_ref) })
	assert !tc.type_compatible(actual, expected)
}

fn test_layout_distinguishes_finite_nested_arguments_and_expansion() {
	mut ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.sum_types.set('Part', ['T', 'bool'])
	tc.sum_generic_params['Part'] = ['T']
	finite := [
		'Part[Part[int]]',
		'Part[?Part[int]]',
		'Part[[1]Part[int]]',
		'Part[[]Part[int]]',
	]
	for name in finite {
		mut states := map[u32]ValueLayoutState{}
		typ := Type(SumType{ name_id: flat.source_type_id(name) })
		assert !tc.value_type_has_cycle(typ, mut states), name
		assert states[flat.source_type_id(name)] == .finite
	}
	tc.sum_types.set('Outer', ['Part[int]', 'Part[string]'])
	mut states := map[u32]ValueLayoutState{}
	outer := Type(SumType{ name_id: flat.source_type_id('Outer') })
	assert !tc.value_type_has_cycle(outer, mut states)
	tc.sum_types.set('Expanding', ['T', 'Part[Expanding[[]T]]'])
	tc.sum_generic_params['Expanding'] = ['T']
	tc.sum_types.set('Holder', ['int', 'Expanding[int]'])
	states.clear()
	holder := Type(SumType{ name_id: flat.source_type_id('Holder') })
	assert tc.value_type_has_cycle(holder, mut states)
}
