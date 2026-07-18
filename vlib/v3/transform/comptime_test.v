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
