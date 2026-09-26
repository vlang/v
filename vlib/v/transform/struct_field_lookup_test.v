module transform

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
