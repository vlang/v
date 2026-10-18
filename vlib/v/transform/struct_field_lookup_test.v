module transform

import v.flat
import v.types

fn test_nested_generic_defaults_skip_pointer_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id('BoxPointer', flat.source_type_id('&Box[int]'))
	tc.type_aliases.set_id('BoxPointerChain', flat.source_type_id('BoxPointer'))
	mut t := new_transformer(mut a, &tc, {
		'main': true
	})
	t.structs['Box'] = StructInfo{
		name:   flat.source_type_id('Box')
		fields: [FieldInfo{ name: 'value', typ: flat.source_type_int, default_expr: 0 }]
	}
	for alias in ['BoxPointer', 'BoxPointerChain'] {
		mut visited := map[u32]bool{}
		assert !t.nested_generic_defaults_need_lowering(flat.source_type_id(alias), mut visited)
	}
}

fn test_struct_field_lookup_preserves_first_field_and_missing_names() {
	for count in [0, 1, 15, 16, 256] {
		mut fields := []FieldInfo{}
		for i in 0 .. count {
			fields << FieldInfo{
				name:    'field_${i}'
				typ:     flat.source_type_int
				raw_typ: flat.source_type_id('Counter')
			}
		}
		if count > 0 {
			fields << FieldInfo{ name: 'field_0', typ: flat.source_type_string }
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
			assert field.typ_text() == 'int'
			assert field.raw_typ_text() == 'Counter'
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
			fields << FieldInfo{ name: 'field_${i}', typ: flat.source_type_id('Type${i}') }
		}
		borrowed := StructInfo{
			fields:        fields
			field_indices: struct_field_indices(fields)
		}
		unsafe { prealloc_scope_leave(scope) }
		owned := clone_struct_info_owned(borrowed)
		unsafe { prealloc_scope_free_after(scope) }
		for i in 0 .. 32 {
			assert owned.field('field_${i}')?.typ_text() == 'Type${i}'
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
	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('T'), flat.source_type_id('rt.Cell[[]&Type]'))) == '[]&ck.Type'
	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('T'), flat.source_type_id('rt.Cell[map[string]&Type]'))) == 'map[string]&ck.Type'
	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('T'), flat.source_type_id('rt.Cell[[2]&Type]'))) == '[2]&ck.Type'
	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('Type'), flat.source_type_id('rt.Cell[[]&Type]'))) == 'rt.Type'
	assert flat.source_type_name(t.normalize_field_type(flat.source_type_id('T'), flat.source_type_id('rt.Cell[[]&rt.Type]'))) == '[]&rt.Type'
}
