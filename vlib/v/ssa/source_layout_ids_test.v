module ssa

import v.flat
import v.types

fn test_registered_options_keep_source_ids_and_existing_layout_queries() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	mut b := Builder{
		a:          &a
		m:          m
		i1_type:    m.type_store.get_int(1)
		i8_type:    m.type_store.get_int(8)
		i64_type:   m.type_store.get_int(64)
		cur_module: 'feature'
	}
	for result in [false, true] {
		spelling := if result { '!i64' } else { '?i64' }
		id := flat.source_type_id(spelling)
		layout := b.option_type_id(flat.source_type_i64, result)
		assert b.option_types[id] == layout
		assert id !in b.struct_types
		assert b.registered_struct_type(id)? == layout
		assert b.resolve_type(id) == layout
		assert b.resolve_type_in_module(id, 'other') == layout
		literal_layout, literal_owner := b.struct_literal_type(id)
		assert literal_layout == layout
		assert literal_owner == id
		cast := a.add_val(.cast_expr, '&' + spelling)
		assert b.pointer_cast_struct_name(cast)? == id
	}
	assert b.registered_struct_type(flat.source_type_id('MissingLayout')) == none
}

fn test_registered_tuple_source_ids_preserve_c_names_and_option_payloads() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut m := Module.new()
	mut b := Builder{
		a:        &a
		tc:       &tc
		m:        m
		i1_type:  m.type_store.get_int(1)
		i8_type:  m.type_store.get_int(8)
		i32_type: m.type_store.get_int(32)
		i64_type: m.type_store.get_int(64)
	}
	ret := types.MultiReturn{
		types: [*tc.parse_type(flat.source_type_int), *tc.parse_type(flat.source_type_i64)]
	}
	source_id := types.Type(ret).source_type_id()
	c_name := b.multi_return_c_type(ret)
	layout := b.register_multi_return_type(ret)
	assert b.multi_return_types[source_id] == layout
	assert source_id !in b.struct_types
	assert b.struct_types[flat.source_type_id(c_name)] == layout
	assert b.register_multi_return_type(ret) == layout
	assert b.registered_struct_type(source_id)? == layout
	assert b.resolve_type(source_id) == layout
	literal_layout, literal_owner := b.struct_literal_type(source_id)
	assert literal_layout == layout
	assert literal_owner == source_id
	assert b.field_type_name(source_id, 'arg0') == flat.source_type_int
	assert flat.source_type_name(source_id) + '.arg0' !in b.struct_field_types.ids
	assert b.field_type_name(flat.source_type_id('&&' + flat.source_type_name(source_id)), 'arg0') == flat.source_type_int
	assert b.field_type_name(flat.source_type_id(c_name), 'arg1') == flat.source_type_i64
	for result in [false, true] {
		wrapper := b.option_type_id_with_payload(source_id, layout, result)
		assert b.option_type_id(source_id, result) == wrapper
		assert b.option_value_type(wrapper) == layout
	}
}

fn test_source_layout_queries_preserve_nominal_module_sum_and_alias_precedence() {
	mut a := flat.FlatAst.new()
	mut m := Module.new()
	mut b := Builder{
		a:          &a
		m:          m
		i32_type:   m.type_store.get_int(32)
		i64_type:   m.type_store.get_int(64)
		cur_module: 'feature'
	}
	fallback := m.type_store.register(Type{ kind: .struct_t })
	local := m.type_store.register(Type{ kind: .struct_t })
	replaced := m.type_store.register(Type{ kind: .struct_t })
	b.struct_types[flat.source_type_id('Record')] = fallback
	b.struct_types[flat.source_type_id('feature.Record')] = local
	id := flat.source_type_id('Record')
	assert b.resolve_type(id) == local
	literal_layout, literal_owner := b.struct_literal_type(id)
	assert literal_layout == local
	assert literal_owner == flat.source_type_id('feature.Record')
	// Direct declaration-index updates must remain visible; no numeric mirror can go stale.
	b.struct_types[flat.source_type_id('feature.Record')] = replaced
	assert b.resolve_type(id) == replaced
	assert b.registered_struct_type(flat.source_type_id('feature.Record'))? == replaced
	b.sum_type_canonical.set(flat.source_type_id('Choice'), 'feature.Record')
	b.sum_type_variants.set(flat.source_type_id('Choice'), ['int'])
	assert b.resolve_type(flat.source_type_id('Choice')) == replaced
	assert b.is_sum_type_id(replaced)
	assert b.sum_name_for_type_id(replaced)? == flat.source_type_id('Choice')
	b.type_aliases[flat.source_type_id('Alias')] = fallback
	assert b.resolve_type(flat.source_type_id('Alias')) == fallback
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('Alias'), flat.source_type_i32)
	b.tc = &tc
	assert b.resolve_type(flat.source_type_id('Alias')) == b.i32_type
}

fn test_enum_registry_ids_preserve_short_module_and_flag_lookups() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut m := Module.new()
	qualified := flat.source_type_id('feature.Mode')
	short := flat.source_type_id('Mode')
	tc.flag_enums[qualified] = true
	mut b := Builder{
		a:          &a
		tc:         &tc
		m:          m
		i32_type:   m.type_store.get_int(32)
		cur_module: 'feature'
	}
	b.register_enum_values(flat.Node{ kind: .enum_decl, value: 'feature.Mode' }, 'feature')
	assert qualified in b.enum_types
	assert short in b.enum_types
	assert qualified in b.flag_enum_types
	assert short in b.flag_enum_types
	assert b.is_enum_type_name(flat.source_type_id('other.Mode'))
	assert b.is_flag_enum_type_name(flat.source_type_id('&feature.Mode'))
	assert b.is_flag_enum_type_name(flat.source_type_id('&other.Mode'))
	assert b.resolve_type(short) == b.i32_type
	assert b.resolve_type_in_module(short, 'feature') == b.i32_type
	assert !b.is_enum_type_name(flat.source_type_id('Missing'))
	assert !b.is_flag_enum_type_name(flat.source_type_id('Missing'))
}

fn test_source_field_queries_keep_nominal_exact_short_and_empty_bindings() {
	mut b := Builder{}
	array_id := flat.source_type_id('[]int')
	map_id := flat.source_type_id('map[string]int')
	b.struct_field_types.set_id('Record.items', array_id)
	b.struct_field_types.set_id('pkg.Record.items', map_id)
	for owner in ['pkg.Record', '&pkg.Record', '&&pkg.Record'] {
		assert b.field_type_name(flat.source_type_id(owner), 'items') == map_id
	}
	for owner in ['Record', '&other.Record'] {
		assert b.field_type_name(flat.source_type_id(owner), 'items') == array_id
	}
	b.struct_field_types.set_id('pkg.Record.empty', u32(0))
	b.struct_field_types.set_id('Record.empty', flat.source_type_int)
	assert b.registered_field_type(flat.source_type_id('&pkg.Record'), 'empty')? == u32(0)
	assert b.registered_field_type(flat.source_type_id('Record'), 'missing') == none
}

fn test_tuple_field_selectors_keep_container_source_types() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut m := Module.new()
	mut b := Builder{
		a:          &a
		tc:         &tc
		m:          m
		array_type: m.type_store.register(Type{ kind: .struct_t })
		map_type:   m.type_store.register(Type{ kind: .struct_t })
	}
	array_id := flat.source_type_id('[]int')
	map_id := flat.source_type_id('map[string]int')
	ret := types.MultiReturn{
		types: [*tc.parse_type(array_id), *tc.parse_type(map_id)]
	}
	b.register_multi_return_type(ret)
	source_id := types.Type(ret).source_type_id()
	b.var_type_names.set_id('pair', source_id)
	base := a.add_val(.ident, 'pair')
	for index, field_id in [array_id, map_id] {
		children := a.begin_children()
		a.add_child(base)
		selector := a.add_node(flat.Node{
			kind:           .selector
			value:          'arg${index}'
			children_start: children
			children_count: 1
		})
		assert b.for_in_container_type_name(selector) == field_id
		assert b.expr_type_name_for_map(selector) == field_id
	}
}
