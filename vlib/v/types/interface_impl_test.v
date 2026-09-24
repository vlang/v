module types

import v.flat

fn test_empty_interface_impl_names_deduplicate_builtin_aliases() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases['byte'] = 'u8'
	tc.type_aliases['builtin.byte'] = 'u8'

	impls := tc.interface_impl_names('Any')
	assert impls.filter(it == 'byte').len == 1
	assert 'builtin.byte' !in impls
}

fn test_empty_interface_impl_names_include_enums() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.enum_names['Color'] = true

	impls := tc.interface_impl_names('Any')
	assert 'Color' in impls
	ids := tc.interface_type_ids('Any')
	assert ids['Color'] > 0
}

fn test_interface_impl_cache_is_explicitly_invalidated_after_type_table_growth() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert 'LateType' !in tc.interface_impl_names('Any')
	tc.structs['LateType'] = []StructField{}
	tc.invalidate_short_type_name_index()
	tc.clear_interface_impl_cache()
	assert 'LateType' in tc.interface_impl_names('Any')
}

fn test_prepared_interface_indexes_do_not_stale_late_implementers() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names['Any'] = true
	tc.prepare_interface_query_indexes()
	pre_transform := tc.pre_transform_interface_impl_names('Any') or {
		panic('missing prepared interface implementer index')
	}
	tc.structs['LateType'] = []StructField{}
	tc.freeze_pre_transform_interface_impl_names()
	assert 'LateType' !in pre_transform
	assert 'LateType' in tc.interface_impl_names('Any')
}

fn test_stable_interface_type_ids_resolve_hash_collisions() {
	ids := stable_interface_type_ids(['main.TZjXQlDs6', 'main.T2nAMbYQH'])
	assert ids['main.TZjXQlDs6'] != ids['main.T2nAMbYQH']
}

fn test_stable_interface_type_ids_preserve_existing_ids_after_late_collisions() {
	assert stable_interface_type_id_hash('Tnndxrxb') == stable_interface_type_id_hash('Twvlzleh')
	before := stable_interface_type_ids(['Twvlzleh'])
	after := stable_interface_type_ids(['Twvlzleh', 'Tnndxrxb'])
	assert after['Twvlzleh'] == before['Twvlzleh']
	assert after['Tnndxrxb'] != before['Twvlzleh']
}

fn test_codegen_fork_keeps_frozen_interface_ids_after_late_collision() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names['Any'] = true
	tc.structs['Twvlzleh'] = []StructField{}
	tc.prepare_interface_query_indexes()
	tc.freeze_pre_transform_interface_impl_names()
	before := tc.interface_type_ids('Any')

	// The late name sorts first, but must not take the frozen type's ID.
	tc.structs['Tnndxrxb'] = []StructField{}
	tc.invalidate_short_type_name_index()
	tc.clear_interface_impl_cache()
	expected := tc.interface_type_ids('Any')
	assert expected['Twvlzleh'] == before['Twvlzleh']
	assert expected['Tnndxrxb'] != expected['Twvlzleh']
	worker := tc.fork_for_parallel_codegen()
	assert worker.interface_type_ids('Any') == expected
	assert worker.interface_impl_names('Any') == tc.interface_impl_names('Any')
}

fn test_short_type_name_ambiguity_remains_sticky() {
	mut index := map[string]string{}
	index_short_type_name('first.Event', mut index)
	index_short_type_name('second.Event', mut index)
	index_short_type_name('third.Event', mut index)
	assert 'Event' in index
	assert index['Event'] == ''
}

fn test_interface_concrete_method_keys_include_dispatch_methods() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names['orm.TransactionalConnection'] = true
	tc.interface_abstract_methods['orm.TransactionalConnection'] = ['select']

	keys := tc.interface_concrete_method_keys()
	assert 'orm.TransactionalConnection.select' in keys
}

fn test_generic_interface_signature_substitutes_named_return_types() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_generic_params['BoxedValue'] = ['T']
	tc.fn_param_types['BoxedValue.get'] = [
		Type(Interface{
			name: 'BoxedValue[T]'
		}),
	]
	tc.fn_ret_types['BoxedValue.get'] = Type(Struct{
		name: 'Box[T]'
	})

	struct_params, struct_ret := tc.specialized_interface_method_signature('BoxedValue[int]',
		'BoxedValue.get')
	assert struct_params[0].name() == 'BoxedValue[int]'
	assert struct_ret.name() == 'Box[int]'

	tc.fn_ret_types['BoxedValue.get'] = Type(SumType{
		name: 'Maybe[T]'
	})
	_, sum_ret := tc.specialized_interface_method_signature('BoxedValue[int]', 'BoxedValue.get')
	assert sum_ret.name() == 'Maybe[int]'

	interface_type := tc.substitute_generic_type(Type(Interface{
		name: 'BoxedValue[T]'
	}), ['int'], ['T'])
	assert interface_type.name() == 'BoxedValue[int]'
}

fn test_interface_metadata_name_keeps_same_named_structs_out_of_interfaces() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.interface_names['io.Reader'] = true
	tc.structs['csv.Reader'] = []StructField{}
	tc.enum_names['colors.Reader'] = true
	tc.sum_types['nodes.Reader'] = ['nodes.A', 'nodes.B']
	assert tc.interface_metadata_name('io.Reader') == 'io.Reader'
	// https://github.com/vlang/v/issues/28834
	assert tc.interface_metadata_name('csv.Reader') == 'csv.Reader'
	assert tc.interface_metadata_name('colors.Reader') == 'colors.Reader'
	assert tc.interface_metadata_name('nodes.Reader') == 'nodes.Reader'
	tc.cur_module = 'csv'
	assert tc.interface_metadata_name('Reader') == 'Reader'
	// A name that is not a known type still resolves through its short name.
	assert tc.interface_metadata_name('iomod.Reader') == 'io.Reader'
}

fn test_undeclared_ierror_does_not_make_every_type_an_error_payload() {
	// https://github.com/vlang/v/issues/28886
	// With `-no-builtin` no `IError` is declared. A missing interface has no
	// requirements, but that must not turn plain values into error payloads.
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.enum_names['Foo'] = true
	assert !tc.type_compatible_with_ierror_payload(Type(Enum{
		name: 'Foo'
	}))
	assert !tc.type_compatible_with_ierror_payload(Type(int_))
	assert !tc.named_type_compatible_with_ierror('Foo')
}
