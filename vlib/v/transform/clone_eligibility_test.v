module transform

import v.flat
import v.types

fn test_default_clone_eligibility_includes_ordinary_struct_fields() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	for name in ['MapValue', 'NestedValue', 'PlainValue', 'ReferenceValue', 'RecursiveValue'] {
		tc.structs[name] = []types.StructField{}
	}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.structs['MapValue'] = StructInfo{
		fields: [FieldInfo{ name: 'm', typ: 'map[string]i16' }]
	}
	t.structs['NestedValue'] = StructInfo{
		fields: [FieldInfo{ name: 'values', typ: '[2]MapValue' }]
	}
	t.structs['PlainValue'] = StructInfo{
		fields: [FieldInfo{ name: 'n', typ: 'int' }]
	}
	t.structs['ReferenceValue'] = StructInfo{
		fields: [FieldInfo{ name: 'value', typ: '&MapValue' }]
	}
	// Even cyclic metadata must not make the classification recurse forever.
	t.structs['RecursiveValue'] = StructInfo{
		fields: [FieldInfo{ name: 'value', typ: 'RecursiveValue' }]
	}
	assert t.compiler_default_clone_type_needs_work('MapValue')
	assert t.compiler_default_clone_type_needs_work('NestedValue')
	assert !t.compiler_default_clone_type_needs_work('PlainValue')
	assert !t.compiler_default_clone_type_needs_work('ReferenceValue')
	assert !t.compiler_default_clone_type_needs_work('&MapValue')
	assert !t.compiler_default_clone_type_needs_work('RecursiveValue')
}
