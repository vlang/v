module c

import v.flat
import v.types

fn test_serial_collection_builds_complete_name_and_field_indexes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('models.Record')] = []types.StructField{}
	tc.type_aliases.set_id(flat.source_type_id('models.Alias'), flat.source_type_int)
	tc.enum_names[flat.source_type_id('models.State')] = true
	tc.flag_enums[flat.source_type_id('models.Flags')] = true
	tc.sum_types.set_ids(flat.source_type_id('models.Choice'), [flat.source_type_int,
		flat.source_type_string])
	tc.interface_names[flat.source_type_id('models.Reader')] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert isnil(g.type_name_short_index)
	assert g.ident_may_name_type('missing')
	g.collect_gen_info(true)
	assert !isnil(g.type_name_short_index)
	assert !isnil(g.struct_field_index_base)
	for name in ['Record', 'Alias', 'State', 'Choice', 'Reader'] {
		assert g.ident_may_name_type(name)
		assert g.ident_may_name_type('other.${name}')
	}
	assert g.name_may_be_type_alias('Alias')
	assert !g.name_may_be_type_alias('Record')
	for name in ['State', 'Flags', 'Alias'] {
		assert g.name_may_reach_enum(name)
	}
	assert !g.ident_may_name_type('missing')
	assert !g.name_may_reach_enum('Record')
	assert g.ident_may_name_type('Unknown[int]')
	assert g.name_may_be_type_alias('Unknown[int]')
	assert g.name_may_reach_enum('Unknown[int]')
	// A later full collection rebuilds membership.
	tc.structs[flat.source_type_id('later.Added')] = []types.StructField{}
	g.collect_gen_info(true)
	assert g.ident_may_name_type('Added')
	// Incremental collection keeps the existing conservative fallback.
	mut incremental := FlatGen.new()
	incremental.a = &a
	incremental.tc = &tc
	incremental.incremental_fn_names['only_new_body'] = true
	incremental.collect_gen_info(true)
	assert isnil(incremental.type_name_short_index)
	assert incremental.ident_may_name_type('missing')
}

fn test_value_c_type_alias_filter_preserves_c_spelling_main_prefix_and_long_keys() {
	for prefix in ['', 'main.'] {
		for suffix in ['Value', 'Value' + 'x'.repeat(600)] {
			mut a := flat.FlatAst.new()
			mut tc := types.TypeChecker.new(&a)
			tc.cur_module = 'main'
			tc.type_aliases.set_id(flat.source_type_id('${prefix}model__${suffix}'), flat.source_type_f64)
			mut g := FlatGen.new()
			g.a = &a
			g.tc = &tc
			value := types.Type(types.Struct{ name_id: flat.source_type_id('model.${suffix}') })
			before := g.value_c_type(&value)
			expected := types.Type(types.f64_)
			assert before == g.tc.c_type(&expected)
			g.type_name_short_index = new_type_name_short_index(&tc)
			assert g.name_may_be_type_alias('model__${suffix}')
			assert g.value_c_type(&value) == before
			plain := types.Type(types.Struct{ name_id: flat.source_type_id('model.Other') })
			assert !g.name_may_be_type_alias('model__Other')
			filtered := g.value_c_type(&plain)
			g.type_name_short_index = unsafe { nil }
			assert g.value_c_type(&plain) == filtered
		}
	}
}

fn test_cast_name_resolution_keeps_bare_priority_qualification_and_local_shadowing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'sample'
	tc.structs[flat.source_type_id('Record')] = []types.StructField{}
	tc.type_aliases.set_id(flat.source_type_id('sample.Record'), flat.source_type_int)
	tc.type_aliases.set_id(flat.source_type_id('sample.Alias'), flat.source_type_int)
	tc.enum_names[flat.source_type_id('sample.State')] = true
	tc.sum_types.set_ids(flat.source_type_id('sample.Choice'), [flat.source_type_int,
		flat.source_type_string])
	tc.interface_names[flat.source_type_id('sample.Reader')] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for with_index in [false, true] {
		if with_index {
			g.type_name_short_index = new_type_name_short_index(&tc)
		}
		assert g.call_ident_cast_type_name('Record', false) or { '' } == 'Record'
		for name in ['Alias', 'State', 'Choice', 'Reader'] {
			assert g.call_ident_cast_type_name(name, false) or { '' } == 'sample.${name}'
			assert g.call_ident_cast_type_name('sample.${name}', false) or { '' } == 'sample.${name}'
		}
		assert g.call_ident_cast_type_name('absent', false) == none
		for name in ['Record', 'Alias', 'State', 'Choice', 'Reader'] {
			assert g.call_ident_cast_type_name(name, true) == none
		}
	}
}
