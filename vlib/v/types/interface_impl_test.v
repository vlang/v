module types

import v.flat

fn test_interface_fixed_array_callback_lengths_resolve_constants() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.const_exprs['n'] = a.add_node(flat.Node{ kind: .int_literal, value: '2' })
	int_type := Type(int_)
	symbolic := Type(ArrayFixed{ elem_type: &Type(int_type), len_expr: 'n' })
	concrete := Type(ArrayFixed{ elem_type: &Type(int_type), len: 2 })
	different := Type(ArrayFixed{ elem_type: &Type(int_type), len: 3 })
	assert tc.fn_type_callconv_compatible(symbolic, concrete)
	assert tc.fn_type_callconv_compatible(concrete, symbolic)
	assert !tc.fn_type_callconv_compatible(symbolic, different)
}

fn test_empty_interface_impl_names_deduplicate_builtin_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases.set_id(flat.source_type_id('byte'), flat.source_type_u8)
	tc.type_aliases.set_id(flat.source_type_id('builtin.byte'), flat.source_type_u8)

	impls := flat.TypeTextList{ ids: tc.interface_impl_names(flat.source_type_id('Any')) }.names()
	assert impls.filter(it == 'byte').len == 1
	assert 'builtin.byte' !in impls
}

fn test_empty_interface_impl_names_include_enums() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.enum_names[flat.source_type_id('Color')] = true

	impls := flat.TypeTextList{ ids: tc.interface_impl_names(flat.source_type_id('Any')) }.names()
	assert 'Color' in impls
	ids := tc.interface_type_ids(flat.source_type_id('Any'))
	assert ids[flat.source_type_id('Color')] > 0
}

fn test_interface_impl_cache_is_explicitly_invalidated_after_type_table_growth() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert 'LateType' !in flat.TypeTextList{ ids: tc.interface_impl_names(flat.source_type_id('Any')) }.names()
	tc.structs[flat.source_type_id('LateType')] = []StructField{}
	tc.invalidate_short_type_name_index()
	tc.clear_interface_impl_cache()
	assert 'LateType' in flat.TypeTextList{ ids: tc.interface_impl_names(flat.source_type_id('Any')) }.names()
}

fn test_prepared_interface_indexes_do_not_stale_late_implementers() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names[flat.source_type_id('Any')] = true
	tc.prepare_interface_query_indexes()
	pre_transform := flat.TypeTextList{
		ids: tc.pre_transform_interface_impl_names(flat.source_type_id('Any')) or {
			panic('missing prepared interface implementer index')
		}
	}.names()
	tc.structs[flat.source_type_id('LateType')] = []StructField{}
	tc.freeze_pre_transform_interface_impl_names()
	assert 'LateType' !in pre_transform
	assert 'LateType' in flat.TypeTextList{ ids: tc.interface_impl_names(flat.source_type_id('Any')) }.names()
}

fn test_stable_interface_type_ids_resolve_hash_collisions() {
	ids := stable_interface_type_ids([flat.source_type_id('main.TZjXQlDs6'),
		flat.source_type_id('main.T2nAMbYQH')])
	assert ids[flat.source_type_id('main.TZjXQlDs6')] != ids[flat.source_type_id('main.T2nAMbYQH')]
}

fn test_stable_interface_type_ids_preserve_existing_ids_after_late_collisions() {
	assert stable_interface_type_id_hash('Tnndxrxb') == stable_interface_type_id_hash('Twvlzleh')
	before := stable_interface_type_ids([flat.source_type_id('Twvlzleh')])
	after := stable_interface_type_ids([flat.source_type_id('Twvlzleh'),
		flat.source_type_id('Tnndxrxb')])
	assert after[flat.source_type_id('Twvlzleh')] == before[flat.source_type_id('Twvlzleh')]
	assert after[flat.source_type_id('Tnndxrxb')] != before[flat.source_type_id('Twvlzleh')]
}

fn test_codegen_fork_keeps_frozen_interface_ids_after_late_collision() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names[flat.source_type_id('Any')] = true
	tc.structs[flat.source_type_id('Twvlzleh')] = []StructField{}
	tc.prepare_interface_query_indexes()
	tc.freeze_pre_transform_interface_impl_names()
	before := tc.interface_type_ids(flat.source_type_id('Any'))

	// The late name sorts first, but must not take the frozen type's ID.
	tc.structs[flat.source_type_id('Tnndxrxb')] = []StructField{}
	tc.invalidate_short_type_name_index()
	tc.clear_interface_impl_cache()
	expected := tc.interface_type_ids(flat.source_type_id('Any'))
	assert expected[flat.source_type_id('Twvlzleh')] == before[flat.source_type_id('Twvlzleh')]
	assert expected[flat.source_type_id('Tnndxrxb')] != expected[flat.source_type_id('Twvlzleh')]
	worker := tc.fork_for_parallel_codegen()
	assert worker.interface_type_ids(flat.source_type_id('Any')) == expected
	assert flat.TypeTextList{ ids: worker.interface_impl_names(flat.source_type_id('Any')) }.names() == flat.TypeTextList{ ids: tc.interface_impl_names(flat.source_type_id('Any')) }.names()
}

fn test_short_type_name_ambiguity_remains_sticky() {
	mut index := flat.SourceTypeBindings{}
	index_short_type_name(flat.source_type_id('first.Event'), mut index)
	index_short_type_name(flat.source_type_id('second.Event'), mut index)
	index_short_type_name(flat.source_type_id('third.Event'), mut index)
	assert index.contains(flat.source_type_id('Event'))
	assert index.value_id(flat.source_type_id('Event')) == 0
}

fn test_interface_concrete_method_keys_include_dispatch_methods() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names[flat.source_type_id('orm.TransactionalConnection')] = true
	tc.interface_abstract_methods[flat.source_type_id('orm.TransactionalConnection')] = ['select']

	keys := tc.interface_concrete_method_keys()
	assert 'orm.TransactionalConnection.select' in keys
}

fn test_generic_interface_signature_substitutes_named_return_types() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_generic_params[flat.source_type_id('BoxedValue')] = generic_param_ids(['T'])
	tc.fn_param_types['BoxedValue.get'] = [
		Type(Interface{
			name_id: flat.source_type_id('BoxedValue[T]')
		}),
	]
	tc.fn_ret_types['BoxedValue.get'] = Type(Struct{
		name_id: flat.source_type_id('Box[T]')
	})

	struct_params, struct_ret := tc.specialized_interface_method_signature(flat.source_type_id('BoxedValue[int]'),
		'BoxedValue.get')
	assert struct_params[0].name() == 'BoxedValue[int]'
	assert struct_ret.name() == 'Box[int]'

	tc.fn_ret_types['BoxedValue.get'] = Type(SumType{
		name_id: flat.source_type_id('Maybe[T]')
	})
	_, sum_ret := tc.specialized_interface_method_signature(flat.source_type_id('BoxedValue[int]'), 'BoxedValue.get')
	assert sum_ret.name() == 'Maybe[int]'

	interface_type := tc.substitute_generic_type(Type(Interface{
		name_id: flat.source_type_id('BoxedValue[T]')
	}), flat.TypeTextList.from_names(['int']).ids, generic_param_ids(['T']))
	assert interface_type.name() == 'BoxedValue[int]'
}

fn test_interface_metadata_name_keeps_same_named_structs_out_of_interfaces() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names[flat.source_type_id('io.Reader')] = true
	tc.structs[flat.source_type_id('csv.Reader')] = []StructField{}
	tc.enum_names[flat.source_type_id('colors.Reader')] = true
	tc.sum_types.set(flat.source_type_id('nodes.Reader'), ['nodes.A', 'nodes.B'])
	assert flat.source_type_name(tc.interface_metadata_name(flat.source_type_id('io.Reader'))) == 'io.Reader'
	// https://github.com/vlang/v/issues/28834
	assert flat.source_type_name(tc.interface_metadata_name(flat.source_type_id('csv.Reader'))) == 'csv.Reader'
	assert flat.source_type_name(tc.interface_metadata_name(flat.source_type_id('colors.Reader'))) == 'colors.Reader'
	assert flat.source_type_name(tc.interface_metadata_name(flat.source_type_id('nodes.Reader'))) == 'nodes.Reader'
	tc.cur_module = 'csv'
	assert flat.source_type_name(tc.interface_metadata_name(flat.source_type_id('Reader'))) == 'Reader'
	// A name that is not a known type still resolves through its short name.
	assert flat.source_type_name(tc.interface_metadata_name(flat.source_type_id('iomod.Reader'))) == 'io.Reader'
}

fn test_interface_metadata_name_keeps_composite_types_out_of_interfaces() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names[flat.source_type_id('baz.MyData')] = true
	tc.structs[flat.source_type_id('bar.MyData')] = []StructField{}
	// https://github.com/vlang/v/issues/28952
	// The text after the last `.` of an array, option, pointer or function type is
	// its element type, not the name of a same-named interface in another module.
	for name in ['[]bar.MyData', '?bar.MyData', '&bar.MyData', '[2]bar.MyData', '[][]bar.MyData',
		'?[]bar.MyData', '[]baz.MyData', 'map[string]bar.MyData', 'fn (bar.MyData) bar.MyData'] {
		assert tc.interface_metadata_name(flat.source_type_id(name)) !in tc.interface_names, name
	}
	assert flat.source_type_name(tc.interface_metadata_name(flat.source_type_id('baz.MyData'))) == 'baz.MyData'
}

fn test_undeclared_ierror_does_not_make_every_type_an_error_payload() {
	// https://github.com/vlang/v/issues/28886
	// With `-no-builtin` no `IError` is declared. A missing interface has no
	// requirements, but that must not turn plain values into error payloads.
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.enum_names[flat.source_type_id('Foo')] = true
	assert !tc.type_compatible_with_ierror_payload(Type(Enum{
		name_id: flat.source_type_id('Foo')
	}))
	assert !tc.type_compatible_with_ierror_payload(Type(int_))
	assert !tc.named_type_compatible_with_ierror(flat.source_type_id('Foo'))
	// A module-local `IError` is not the builtin one, even when it is the only
	// interface with that short name.
	tc.interface_names[flat.source_type_id('pkg.IError')] = true
	assert !tc.type_compatible_with_ierror_payload(Type(Enum{
		name_id: flat.source_type_id('Foo')
	}))
	assert !tc.type_compatible_with_ierror_payload(Type(int_))
	assert !tc.named_type_compatible_with_ierror(flat.source_type_id('Foo'))
}

fn test_prepared_interface_requirements_keep_exact_source_spelling_keys() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names[flat.source_type_id('pkg.Parent')] = true
	tc.interface_names[flat.source_type_id('pkg.Contract')] = true
	// Preparation publishes both the source table spelling and its resolved
	// metadata name; the concrete application remains an exact additional key.
	tc.interface_names[flat.source_type_id('pkg.Contract[int]')] = true
	tc.interface_generic_params[flat.source_type_id('pkg.Contract')] = generic_param_ids(['T'])
	tc.interface_embeds.set(flat.source_type_id('pkg.Contract'), ['pkg.Parent'])
	tc.interface_abstract_methods[flat.source_type_id('pkg.Parent')] = ['parent']
	tc.interface_abstract_methods[flat.source_type_id('pkg.Contract')] = ['required']
	tc.fn_ret_types['pkg.Parent.parent'] = Type(int_)
	tc.fn_ret_types['pkg.Contract[T].required'] = Type(int_)
	tc.fn_ret_types['pkg.Contract[T].defaulted'] = Type(int_)
	tc.interface_fields[flat.source_type_id('pkg.Parent')] = [StructField{ name: 'parent_field', typ: Type(int_) }]
	tc.interface_fields[flat.source_type_id('pkg.Contract')] = [StructField{ name: 'own_field', typ: Type(int_) }]
	queries := flat.TypeTextList.from_names(['pkg.Parent', 'pkg.Contract', 'pkg.Contract[int]',
		'pkg.Contract[string]', '&pkg.Contract', '&pkg.Contract[int]', '[]pkg.Contract', '',
		'unknown.Contract']).ids
	mut expected_methods := [][]string{}
	mut expected_abstract := [][]string{}
	mut expected_fields := [][]string{}
	for query in queries {
		expected_methods << tc.interface_method_names(query)
		expected_abstract << tc.interface_abstract_method_names(query)
		expected_fields << tc.interface_field_list(query).map(it.name)
	}
	tc.prepare_interface_requirement_indexes()
	assert tc.interface_query_indexes_ready
	base_id := flat.source_type_id('pkg.Contract')
	application_id := flat.source_type_id('pkg.Contract[int]')
	for key in [base_id, application_id] {
		assert key in tc.interface_method_names_index
		assert key in tc.interface_abstract_index
		assert key in tc.interface_field_list_index
	}
	assert flat.source_type_id('pkg.Contract[string]') !in tc.interface_field_list_index
	assert flat.source_type_id('&pkg.Contract') !in tc.interface_field_list_index
	assert u32(0) !in tc.interface_field_list_index
	worker := tc.fork_for_parallel_codegen()
	for i, query in queries {
		assert tc.interface_method_names(query) == expected_methods[i]
		assert tc.interface_abstract_method_names(query) == expected_abstract[i]
		assert tc.interface_field_list(query).map(it.name) == expected_fields[i]
		assert worker.interface_method_names(query) == expected_methods[i]
		assert worker.interface_abstract_method_names(query) == expected_abstract[i]
		assert worker.interface_field_list(query).map(it.name) == expected_fields[i]
	}
	// Rebuilding replaces the published maps; prior worker views retain the
	// immutable snapshot instead of sharing the new index under construction.
	tc.interface_fields[flat.source_type_id('pkg.Contract')] = [StructField{ name: 'replacement', typ: Type(int_) }]
	tc.prepare_interface_requirement_indexes()
	assert tc.interface_field_list(application_id).map(it.name) == ['parent_field', 'replacement']
	assert worker.interface_field_list(application_id).map(it.name) == ['parent_field', 'own_field']
}
