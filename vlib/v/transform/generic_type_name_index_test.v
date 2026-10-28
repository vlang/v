module transform

import v.flat
import v.types

fn test_generic_type_name_indexes_refresh_after_late_declarations() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_generic_alias_name_index()

	tc.structs[flat.source_type_id('late.Record')] = []types.StructField{}
	assert t.generic_concrete_name_known(flat.source_type_id('late.Record'))
	assert !t.generic_concrete_name_known(flat.source_type_id('late.Choice'))
	tc.sum_types.set(flat.source_type_id('late.Choice'), ['int', 'string'])
	tc.enum_names[flat.source_type_id('late.Kind')] = true
	tc.interface_names[flat.source_type_id('late.Reader')] = true
	for name in ['late.Record', 'late.Choice', 'late.Kind', 'late.Reader'] {
		assert t.generic_concrete_name_known(flat.source_type_id(name))
		assert t.type_name_is_declared_id(flat.source_type_id(name))
	}
	assert t.generic_checker_sum_name_known(flat.source_type_id('late.Choice'))
	assert t.generic_checker_interface_name_known(flat.source_type_id('late.Reader'))
	assert !t.generic_checker_sum_name_known(flat.source_type_id('late.MoreChoice'))
	assert !t.generic_checker_interface_name_known(flat.source_type_id('late.MoreReader'))
	tc.sum_types.set(flat.source_type_id('late.MoreChoice'), ['int', 'string'])
	tc.interface_names[flat.source_type_id('late.MoreReader')] = true
	assert t.generic_checker_sum_name_known(flat.source_type_id('late.MoreChoice'))
	assert t.generic_checker_interface_name_known(flat.source_type_id('late.MoreReader'))
	assert t.generic_checker_sum_name_known(flat.source_type_id('late.Choice'))
	assert t.generic_checker_interface_name_known(flat.source_type_id('late.Reader'))

	t.structs[flat.source_type_id('Local')] = StructInfo{ name: flat.source_type_id('Local'), module: 'main' }
	t.structs[flat.source_type_id('foreign.Local')] = StructInfo{ name: flat.source_type_id('Local'), module: 'foreign' }
	assert t.is_known_main_struct_type_name_id(flat.source_type_id('main.Local'))
	assert !t.is_known_main_struct_type_name_id(flat.source_type_id('main.foreign.Local'))
	t.sum_types.set(flat.source_type_id('LocalChoice'), ['int', 'string'])
	t.enum_types[flat.source_type_id('LocalKind')] = ['one']
	tc.type_aliases.set_id(flat.source_type_id('late.Alias'), flat.source_type_int)
	assert t.generic_exact_alias_name_known(flat.source_type_id('late.Alias'))
	assert !t.generic_exact_alias_name_known(flat.source_type_id('late.MoreAlias'))
	tc.type_aliases.set_id(flat.source_type_id('late.MoreAlias'), flat.source_type_string)
	assert t.generic_exact_alias_name_known(flat.source_type_id('late.MoreAlias'))
	assert t.generic_exact_alias_name_known(flat.source_type_id('late.Alias'))
	for name in ['Local', 'foreign.Local', 'LocalChoice', 'LocalKind', 'late.Alias'] {
		assert t.type_name_is_declared_id(flat.source_type_id(name))
	}
	assert !t.type_name_is_declared_id(flat.source_type_id('late.Missing'))
}

fn test_generic_type_name_index_refresh_is_private_to_each_worker() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_generic_alias_name_index()
	tc.structs[flat.source_type_id('RootRecord')] = []types.StructField{}
	tc.structs[flat.source_type_id('OtherRootRecord')] = []types.StructField{}
	assert t.generic_concrete_name_known(flat.source_type_id('RootRecord'))

	mut worker_tc := types.TypeChecker.new(&a)
	worker_tc.structs[flat.source_type_id('WorkerRecord')] = []types.StructField{}
	worker := t.fork_program_view(&a, &worker_tc, map[string]bool{}, false)
	assert voidptr(worker.generic_type_name_index_cache) != voidptr(t.generic_type_name_index_cache)
	assert worker.generic_concrete_name_known(flat.source_type_id('WorkerRecord'))
	assert !worker.generic_concrete_name_known(flat.source_type_id('RootRecord'))
	assert t.generic_concrete_name_known(flat.source_type_id('RootRecord'))
	assert !t.generic_concrete_name_known(flat.source_type_id('WorkerRecord'))
}

fn test_generic_type_name_indexes_refresh_struct_enum_and_parameter_views() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.build_generic_alias_name_index()
	tc.structs[flat.source_type_id('foreign.Record[int]')] = []types.StructField{}
	assert t.generic_checker_struct_name_known(flat.source_type_id('foreign.Record[int]'))
	assert !t.generic_checker_struct_name_known(flat.source_type_id('Record'))
	assert t.comptime_struct_type_known(flat.source_type_id('Record'))
	assert !t.comptime_struct_type_known(flat.source_type_id('Other'))
	tc.structs[flat.source_type_id('second.Other')] = []types.StructField{}
	assert t.comptime_struct_type_known(flat.source_type_id('Other'))
	assert t.generic_checker_struct_name_known(flat.source_type_id('second.Other'))

	t.enum_types[flat.source_type_id('LocalOnly')] = ['one']
	assert t.generic_enum_type_name_known(flat.source_type_id('LocalOnly'))
	assert !t.generic_main_arg_checker_declaration_known(flat.source_type_id('LocalOnly'))
	assert !t.generic_enum_type_name_known(flat.source_type_id('LateKind'))
	tc.enum_names[flat.source_type_id('LateKind')] = true
	assert t.generic_enum_type_name_known(flat.source_type_id('LateKind'))
	assert t.generic_main_arg_checker_declaration_known(flat.source_type_id('LateKind'))
	assert !t.generic_main_arg_checker_declaration_known(flat.source_type_id('LaterKind'))
	tc.enum_names[flat.source_type_id('LaterKind')] = true
	assert t.generic_enum_type_name_known(flat.source_type_id('LaterKind'))
	assert t.generic_main_arg_checker_declaration_known(flat.source_type_id('LaterKind'))

	tc.struct_generic_params[flat.source_type_id('foreign.Box')] = ['T']
	assert t.generic_type_base_requires_args(flat.source_type_id('foreign.Box'))
	assert !t.generic_type_base_requires_args(flat.source_type_id('Box'))
	assert !t.generic_type_base_requires_args(flat.source_type_id('Choice'))
	tc.sum_generic_params[flat.source_type_id('Choice')] = ['T']
	assert t.generic_type_base_requires_args(flat.source_type_id('Choice'))
	assert !t.generic_type_base_requires_args(flat.source_type_id('Alias'))
	tc.type_alias_generic_params[flat.source_type_id('Alias')] = ['T']
	assert t.generic_type_base_requires_args(flat.source_type_id('Alias'))
	assert !t.generic_type_base_requires_args(flat.source_type_id('Alias[int]'))
}

fn test_generic_type_name_indexes_refresh_after_template_erasure_and_same_size_growth() {
	for prepared_only in [true, false] {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		tc.sum_types.set(flat.source_type_id('Template'), ['int', 'string'])
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.sum_types.set(flat.source_type_id('Template'), ['int', 'string'])
		t.build_generic_alias_name_index()
		if !prepared_only {
			tc.sum_types.set(flat.source_type_id('Preserved'), ['int', 'string'])
			t.sum_types.set(flat.source_type_id('Preserved'), ['int', 'string'])
		}
		old_count := t.generic_checker_concrete_name_count()
		assert t.generic_concrete_name_known(flat.source_type_id('Template'))
		assert t.generic_checker_sum_name_known(flat.source_type_id('Template'))
		assert t.generic_transform_concrete_name_known(flat.source_type_id('Template'))
		t.erase_generic_sum_templates({
			flat.source_type_id('Template'): GenericSumDecl{
				key:  'Template'
				node: flat.Node{ kind: .type_decl, value: 'Template' }
			}
		})
		assert t.generic_type_indexes_ready
		tc.sum_types.set(flat.source_type_id('Replacement'), ['int', 'string'])
		t.sum_types.set(flat.source_type_id('Replacement'), ['int', 'string'])
		assert t.generic_checker_concrete_name_count() == old_count
		assert !t.generic_concrete_name_known(flat.source_type_id('Template'))
		assert !t.generic_checker_sum_name_known(flat.source_type_id('Template'))
		assert !t.generic_transform_concrete_name_known(flat.source_type_id('Template'))
		assert !t.type_name_is_declared_id(flat.source_type_id('Template'))
		assert t.generic_concrete_name_known(flat.source_type_id('Replacement'))
		assert t.generic_checker_sum_name_known(flat.source_type_id('Replacement'))
		assert t.generic_transform_concrete_name_known(flat.source_type_id('Replacement'))
		assert t.type_name_is_declared_id(flat.source_type_id('Replacement'))
	}
}

fn test_generic_type_name_indexes_refresh_after_prepared_checker_replacement() {
	mut a := flat.FlatAst.new()
	a.add_val(.module_decl, 'main')
	a.add_node(flat.Node{ kind: .struct_decl, value: 'Local' })
	mut tc := types.TypeChecker.new(&a)
	tc.structs[flat.source_type_id('Before')] = []types.StructField{}
	tc.type_aliases.set_id(flat.source_type_id('before.Alias'), flat.source_type_int)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.prepare()
	assert t.generic_concrete_name_known(flat.source_type_id('Before'))
	assert t.generic_arg_is_alias_name_id(flat.source_type_id('Alias'), 'main')
	tc.structs[flat.source_type_id('LateBefore')] = []types.StructField{}
	tc.type_aliases.set_id(flat.source_type_id('before.LateAlias'), flat.source_type_string)
	assert t.generic_concrete_name_known(flat.source_type_id('LateBefore'))
	assert t.generic_exact_alias_name_known(flat.source_type_id('before.LateAlias'))

	mut replacement := types.TypeChecker.new(&a)
	replacement.structs[flat.source_type_id('After')] = []types.StructField{}
	replacement.structs[flat.source_type_id('LateAfter')] = []types.StructField{}
	replacement.type_aliases.set_id(flat.source_type_id('after.OtherAlias'), flat.source_type_int)
	replacement.type_aliases.set_id(flat.source_type_id('after.LateAlias'), flat.source_type_string)
	assert replacement.structs.len == tc.structs.len
	assert replacement.type_aliases.ids.len == tc.type_aliases.ids.len
	t.replace_prepared_type_checker(&replacement)
	assert t.generic_type_indexes_ready
	assert t.generic_concrete_name_known(flat.source_type_id('After'))
	assert t.generic_concrete_name_known(flat.source_type_id('LateAfter'))
	assert !t.generic_concrete_name_known(flat.source_type_id('Before'))
	assert !t.generic_concrete_name_known(flat.source_type_id('LateBefore'))
	assert t.generic_arg_is_alias_name_id(flat.source_type_id('OtherAlias'), 'main')
	assert !t.generic_arg_is_alias_name_id(flat.source_type_id('Alias'), 'main')
	assert !t.generic_exact_alias_name_known(flat.source_type_id('before.LateAlias'))
	t.structs[flat.source_type_id('Local')] = StructInfo{ name: flat.source_type_id('Local'), module: 'foreign' }
	t.build_generic_alias_name_index()
	assert !t.is_known_main_struct_type_name_id(flat.source_type_id('main.Local'))
	t.prepare()
	assert t.is_known_main_struct_type_name_id(flat.source_type_id('main.Local'))
}
