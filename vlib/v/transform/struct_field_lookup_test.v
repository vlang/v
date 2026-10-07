module transform

import v.flat
import v.types

fn test_nested_generic_defaults_skip_pointer_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases['BoxPointer'] = '&Box[int]'
	tc.type_aliases['BoxPointerChain'] = 'BoxPointer'
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.structs['Box'] = StructInfo{
		name:   'Box'
		fields: [FieldInfo{ name: 'value', typ: 'int', default_expr: 0 }]
	}
	for alias in ['BoxPointer', 'BoxPointerChain'] {
		mut visited := map[string]bool{}
		assert !t.nested_generic_defaults_need_lowering(alias, mut visited)
	}
}

fn test_struct_field_lookup_preserves_first_field_and_missing_names() {
	for count in [0, 1, 15, 16, 256] {
		mut fields := []FieldInfo{}
		for i in 0 .. count {
			fields << FieldInfo{
				name:    'field_${i}'
				typ:     'int'
				raw_typ: 'Counter'
			}
		}
		if count > 0 {
			fields << FieldInfo{ name: 'field_0', typ: 'string' }
		}
		info := StructInfo{
			fields:        fields
			field_indices: struct_field_indices(fields)
		}
		owned := clone_struct_info_owned(info)
		assert info.field('missing') == none
		assert owned.field('missing') == none
		for i in 0 .. count {
			field := info.field('field_${i}') or { panic('missing field ${i}') }
			assert field.typ == 'int'
			assert field.raw_typ == 'Counter'
			assert owned.field('field_${i}')? == field
		}
	}
}

fn test_owned_field_index_survives_worker_arena_release() {
	$if prealloc {
		// Build the metadata in the same disposable arena used by a worker.
		scope := unsafe { prealloc_scope_begin() }
		mut fields := []FieldInfo{}
		for i in 0 .. 32 {
			fields << FieldInfo{ name: 'field_${i}', typ: 'Type${i}' }
		}
		borrowed := StructInfo{
			fields:        fields
			field_indices: struct_field_indices(fields)
		}
		unsafe { prealloc_scope_leave(scope) }
		owned := clone_struct_info_owned(borrowed)
		unsafe { prealloc_scope_free_after(scope) }
		for i in 0 .. 32 {
			assert owned.field('field_${i}')?.typ == 'Type${i}'
		}
		assert owned.field('missing') == none
	}
}

fn test_foreign_generic_field_arguments_keep_instantiation_module() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params['rt.Cell'] = ['T']
	tc.structs['rt.Type'] = []types.StructField{}
	tc.structs['ck.Type'] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = 'ck'
	assert t.normalize_field_type('T', 'rt.Cell[[]&Type]') == '[]&ck.Type'
	assert t.normalize_field_type('T', 'rt.Cell[map[string]&Type]') == 'map[string]&ck.Type'
	assert t.normalize_field_type('T', 'rt.Cell[[2]&Type]') == '[2]&ck.Type'
	assert t.normalize_field_type('Type', 'rt.Cell[[]&Type]') == 'rt.Type'
	assert t.normalize_field_type('T', 'rt.Cell[[]&rt.Type]') == '[]&rt.Type'
}
