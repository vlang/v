module transform

import v.flat
import v.types

fn test_selective_nominal_probes_skip_missing_names_and_preserve_candidate_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	file := 'nominal_probe_retention.v'
	base_id := flat.source_type_id('__NominalProbeSelected__')
	missing := '__nominal_probe_import__.Missing'
	first := '__nominal_probe_import__.First'
	second := '__nominal_probe_import__.Second'
	first_id := flat.source_type_id(first)
	second_id := flat.source_type_id(second)
	tc.file_selective_imports[file_import_key(file, flat.source_type_name(base_id))] = [
		missing,
		first,
		second,
	]
	tc.structs[first_id] = []types.StructField{}
	tc.structs[second_id] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_file = file
	struct_decls := {
		first_id:  GenericStructDecl{}
		second_id: GenericStructDecl{}
	}
	sum_decls := {
		first_id:  GenericSumDecl{}
		second_id: GenericSumDecl{}
	}
	assert flat.source_type_existing_id(missing) == none
	assert t.comptime_resolve_selective_import_type(base_id) == first_id
	assert t.selective_signature_type_symbol_id(file, base_id)? == first
	assert t.selective_import_generic_struct_base(base_id, file, struct_decls)? == first_id
	assert t.selective_import_generic_sum_base(base_id, file, sum_decls)? == first_id
	assert flat.source_type_existing_id(missing) == none
	tc.file_selective_imports[file_import_key(file, flat.source_type_name(base_id))] = [missing]
	assert t.comptime_resolve_selective_import_type(base_id) == base_id
	assert t.selective_signature_type_symbol_id(file, base_id) == none
	assert t.selective_import_generic_struct_base(base_id, file, struct_decls) == none
	assert t.selective_import_generic_sum_base(base_id, file, sum_decls) == none
	assert flat.source_type_existing_id(missing) == none
}

fn test_selective_nominal_probes_keep_registered_zero_id() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	file := 'nominal_probe_zero.v'
	base_id := flat.source_type_id('__NominalProbeZero__')
	zero_id := u32(0)
	tc.file_selective_imports[file_import_key(file, flat.source_type_name(base_id))] = ['']
	tc.structs[zero_id] = []types.StructField{}
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_file = file
	struct_decls := {
		zero_id: GenericStructDecl{}
	}
	sum_decls := {
		zero_id: GenericSumDecl{}
	}
	assert t.comptime_resolve_selective_import_type(base_id) == zero_id
	assert t.selective_signature_type_symbol_id(file, base_id)? == ''
	assert t.selective_import_generic_struct_base(base_id, file, struct_decls)? == zero_id
	assert t.selective_import_generic_sum_base(base_id, file, sum_decls)? == zero_id
}

fn test_comptime_reflection_and_generic_module_probes_do_not_intern_misses() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	module_name := '__nominal_probe_reflection_module__'
	missing_name := '__NominalProbeReflectionMissing__'
	missing := '${module_name}.${missing_name}'
	function_name := '__NominalProbeReflectionFunction__'
	function := '${module_name}.${function_name}'
	tc.fn_ret_types[function] = types.Type(types.void_)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = module_name
	assert flat.source_type_existing_id(missing) == none
	assert t.comptime_local_reflection_source(missing_name) == none
	assert !t.generic_arg_module_owns_type(missing_name, module_name)
	assert flat.source_type_existing_id(missing) == none
	assert flat.source_type_existing_id(function) == none
	assert t.comptime_local_reflection_source(function_name)? == function
	assert flat.source_type_existing_id(function) == none
	known_id := flat.source_type_id('${module_name}.__NominalProbeReflectionKnown__')
	tc.structs[known_id] = []types.StructField{}
	assert t.comptime_local_reflection_source('__NominalProbeReflectionKnown__')? == flat.source_type_name(known_id)
	assert t.generic_arg_module_owns_type('__NominalProbeReflectionKnown__', module_name)
}
