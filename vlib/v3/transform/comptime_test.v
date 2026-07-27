module transform

import v3.flat

fn test_comptime_field_function_type_keeps_declaring_module() {
	mut a := flat.FlatAst.new()
	t := Transformer{
		a: &a
	}
	qualified := t.comptime_field_type_id_key('?fn (mut SSLListener, string) !&SSLCerts', 'mbedtls')
	assert qualified == '?fn(mut mbedtls.SSLListener, string) !&mbedtls.SSLCerts'
	assert t.comptime_field_type_id_key('Registry[string]', 'eventbus') == 'eventbus.Registry[string]'
	assert t.comptime_field_type_id_key('Container[T]', 'eventbus') == 'eventbus.Container[T]'
}

fn test_comptime_field_type_id_keeps_custom_types_above_builtin_range() {
	mut a := flat.FlatAst.new()
	t := Transformer{
		a: &a
	}
	assert comptime_type_id_hash('T207') & ~(0xff << 16) < 65536
	type_id := t.comptime_field_type_id('T207', '')
	assert type_id > 65535
	assert type_id != comptime_builtin_type_idx('isize')
	assert type_id & (0xff << 16) == 0
}

fn test_comptime_for_base_type_unwraps_storage_indirections() {
	mut a := flat.FlatAst.new()
	t := Transformer{
		a: &a
	}
	assert t.comptime_for_base_type('&websocket.Server') == 'websocket.Server'
	assert t.comptime_for_base_type('shared websocket.ClientState') == 'websocket.ClientState'
}

fn test_comptime_condition_distinguishes_pointer_depth_from_logical_and() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a: &a
	}
	is_array := t.eval_field_cond('&&char is $array_dynamic') or {
		assert false, 'double-pointer type condition should be decidable'
		return
	}
	is_pointer := t.eval_field_cond('&&char is $pointer') or {
		assert false, 'double-pointer type condition should be decidable'
		return
	}
	pointer_and_true := t.eval_field_cond('&&char is $pointer && true') or {
		assert false, 'logical AND after a double-pointer type should be decidable'
		return
	}
	assert !is_array
	assert is_pointer
	assert pointer_and_true
}

fn test_mangled_generic_struct_field_metadata_resolves_declaration() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'sample')
	mut field := flat.Node{
		kind:  .field_decl
		value: 'value'
		typ:   'T'
	}
	field.set_generic_params(['mp', 'skip'])
	field_id := a.add_node(field)
	children_start := a.children.len
	a.children << field_id
	mut generic_struct := flat.Node{
		kind:           .struct_decl
		value:          'Box'
		children_start: i32(children_start)
		children_count: 1
	}
	generic_struct.set_generic_params(['T'])
	a.add_node(generic_struct)

	mut t := Transformer{
		a: &a
	}
	t.build_struct_field_decl_metas_cache()
	for name in ['Box_int', 'sample.Box_int', 'sample__Box_int'] {
		metas := t.struct_field_decl_metas(name)
		meta := metas['value'] or {
			assert false, 'missing field metadata for `${name}`'
			continue
		}
		assert meta.is_mut
		assert meta.is_pub
		assert meta.attrs == ['skip']
	}
}

fn test_comptime_field_metadata_cache_uses_resolved_module() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a:                             &a
		comptime_field_metas_cache:    map[string][]FieldMeta{}
		struct_field_decl_metas_cache: {
			'first.Config':  {
				'first': FieldDeclMeta{
					is_pub: true
					attrs:  ['first_attr']
				}
			}
			'second.Config': {
				'second': FieldDeclMeta{
					is_mut: true
					attrs:  ['second_attr']
				}
			}
		}
		structs:                       {
			'first.Config':  StructInfo{
				name:   'Config'
				module: 'first'
				fields: [
					FieldInfo{
						name:    'first'
						typ:     'int'
						raw_typ: 'int'
					},
				]
			}
			'second.Config': StructInfo{
				name:   'Config'
				module: 'second'
				fields: [
					FieldInfo{
						name:    'second'
						typ:     'string'
						raw_typ: 'string'
					},
				]
			}
		}
	}
	t.cur_module = 'first'
	first := t.comptime_field_metas('Config')
	assert first.len == 1
	assert first[0].name == 'first'
	assert first[0].attrs == ['first_attr']
	assert first[0].is_pub

	t.cur_module = 'second'
	second := t.comptime_field_metas('Config')
	assert second.len == 1
	assert second[0].name == 'second'
	assert second[0].attrs == ['second_attr']
	assert second[0].is_mut
}

fn test_comptime_field_metadata_cache_normalizes_main_qualified_name() {
	mut a := flat.FlatAst.new()
	mut t := Transformer{
		a:                             &a
		comptime_field_metas_cache:    map[string][]FieldMeta{}
		struct_field_decl_metas_cache: {
			'Config': {
				'skipped': FieldDeclMeta{
					attrs: ['skip']
				}
			}
		}
		structs:                       {
			'main.Config': StructInfo{
				name:   'main.Config'
				module: 'main'
				fields: [
					FieldInfo{
						name:    'skipped'
						typ:     '&App'
						raw_typ: '&App'
					},
				]
			}
		}
	}
	metas := t.comptime_field_metas('main.Config')
	assert metas.len == 1
	assert metas[0].attrs == ['skip']
}

fn test_comptime_field_metadata_cache_keeps_main_and_builtin_names_distinct() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'builtin')
	mut builtin_field := flat.Node{
		kind:  .field_decl
		value: 'builtin_value'
		typ:   'int'
	}
	builtin_field.set_generic_params(['p', 'builtin_attr'])
	builtin_field_id := a.add_node(builtin_field)
	builtin_children_start := a.children.len
	a.children << builtin_field_id
	a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'FieldData'
		children_start: i32(builtin_children_start)
		children_count: 1
	})

	a.add_val(.module_decl, 'main')
	mut main_field := flat.Node{
		kind:  .field_decl
		value: 'main_value'
		typ:   'string'
	}
	main_field.set_generic_params(['mp', 'main_attr'])
	main_field_id := a.add_node(main_field)
	main_children_start := a.children.len
	a.children << main_field_id
	a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'FieldData'
		children_start: i32(main_children_start)
		children_count: 1
	})

	mut t := Transformer{
		a: &a
	}
	t.build_struct_field_decl_metas_cache()
	main_metas := t.struct_field_decl_metas_in_module('FieldData', 'main')
	main_meta := main_metas['main_value'] or {
		assert false, 'missing main FieldData metadata'
		return
	}
	assert main_meta.is_mut
	assert main_meta.is_pub
	assert main_meta.attrs == ['main_attr']
	assert 'builtin_value' !in main_metas

	builtin_metas := t.struct_field_decl_metas_in_module('FieldData', 'builtin')
	builtin_meta := builtin_metas['builtin_value'] or {
		assert false, 'missing builtin FieldData metadata'
		return
	}
	assert !builtin_meta.is_mut
	assert builtin_meta.is_pub
	assert builtin_meta.attrs == ['builtin_attr']
	assert 'main_value' !in builtin_metas

	bare_metas := t.struct_field_decl_metas('FieldData')
	assert 'main_value' in bare_metas
	assert 'builtin_value' !in bare_metas
}
