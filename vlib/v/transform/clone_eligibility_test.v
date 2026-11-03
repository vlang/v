module transform

import v.flat
import v.types

fn test_default_clone_eligibility_includes_ordinary_struct_fields() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	for name in ['MapValue', 'NestedValue', 'PlainValue', 'ReferenceValue', 'RecursiveValue'] {
		tc.structs[flat.source_type_id(name)] = []types.StructField{}
	}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs[flat.source_type_id('MapValue')] = StructInfo{
		fields: [FieldInfo{ name: 'm', typ: flat.source_type_id('map[string]i16') }]
	}
	t.structs[flat.source_type_id('NestedValue')] = StructInfo{
		fields: [FieldInfo{ name: 'values', typ: flat.source_type_id('[2]MapValue') }]
	}
	t.structs[flat.source_type_id('PlainValue')] = StructInfo{
		fields: [FieldInfo{ name: 'n', typ: flat.source_type_int }]
	}
	t.structs[flat.source_type_id('ReferenceValue')] = StructInfo{
		fields: [FieldInfo{ name: 'value', typ: flat.source_type_id('&MapValue') }]
	}
	// Even cyclic metadata must not make the classification recurse forever.
	t.structs[flat.source_type_id('RecursiveValue')] = StructInfo{
		fields: [FieldInfo{ name: 'value', typ: flat.source_type_id('RecursiveValue') }]
	}
	assert t.compiler_default_clone_type_needs_work(flat.source_type_id('MapValue'))
	assert t.compiler_default_clone_type_needs_work(flat.source_type_id('NestedValue'))
	assert !t.compiler_default_clone_type_needs_work(flat.source_type_id('PlainValue'))
	assert !t.compiler_default_clone_type_needs_work(flat.source_type_id('ReferenceValue'))
	assert !t.compiler_default_clone_type_needs_work(flat.source_type_id('&MapValue'))
	assert !t.compiler_default_clone_type_needs_work(flat.source_type_id('RecursiveValue'))
}
