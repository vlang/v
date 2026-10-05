module types

import v.flat

fn test_sumtype_layout_detects_value_cycles() {
	mut ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.sum_types['Tree'] = ['Branch', 'int']
	tree := Type(SumType{ name: 'Tree' })
	branch := Type(Struct{ name: 'Branch' })
	tc.structs['Branch'] = [StructField{ name: 'child', typ: tree }]
	mut seen := {
		'Tree': ValueLayoutState.active
	}
	assert tc.value_type_has_cycle(branch, mut seen)
	pointer := Type(Pointer{ base_type: &Type(tree) })
	tc.structs['Branch'] = [StructField{ name: 'child', typ: pointer }]
	seen.clear()
	seen['Tree'] = .active
	assert !tc.value_type_has_cycle(branch, mut seen)
	array := Type(Array{ elem_type: &Type(tree) })
	tc.structs['Branch'] = [StructField{ name: 'children', typ: array }]
	seen.clear()
	seen['Tree'] = .active
	assert !tc.value_type_has_cycle(branch, mut seen)
}

fn test_recursive_type_metadata_retains_value_equality() {
	first := Type(Array{ elem_type: &Type(int_) })
	second := Type(Array{ elem_type: &Type(int_) })
	assert first == second
	assert semantic_type_hash(first) == semantic_type_hash(second)
	assert first != Type(Array{ elem_type: &Type(string_) })
}

fn test_sum_references_require_the_same_storage_type() {
	mut ast := flat.FlatAst.new()
	mut tc := TypeChecker.new(&ast)
	tc.structs['Item'] = []StructField{}
	tc.sum_types['Value'] = ['Item', 'int']
	item := Type(Struct{ name: 'Item' })
	sum := Type(SumType{ name: 'Value' })
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
	tc.sum_types['Part'] = ['T', 'bool']
	tc.sum_generic_params['Part'] = ['T']
	finite := [
		'Part[Part[int]]',
		'Part[?Part[int]]',
		'Part[[1]Part[int]]',
		'Part[[]Part[int]]',
	]
	for name in finite {
		mut states := map[string]ValueLayoutState{}
		typ := Type(SumType{ name: name })
		assert !tc.value_type_has_cycle(typ, mut states), name
		assert states[name] == .finite
	}
	tc.sum_types['Outer'] = ['Part[int]', 'Part[string]']
	mut states := map[string]ValueLayoutState{}
	outer := Type(SumType{ name: 'Outer' })
	assert !tc.value_type_has_cycle(outer, mut states)
	tc.sum_types['Expanding'] = ['T', 'Part[Expanding[[]T]]']
	tc.sum_generic_params['Expanding'] = ['T']
	tc.sum_types['Holder'] = ['int', 'Expanding[int]']
	states.clear()
	holder := Type(SumType{ name: 'Holder' })
	assert tc.value_type_has_cycle(holder, mut states)
}
