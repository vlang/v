module types

import v.flat

fn test_checker_selective_type_probes_keep_misses_and_candidate_order() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'checker_lookup_probe.v'
	missing := '__checker_lookup_probe_missing__.Selected'
	first := '__checker_lookup_probe_first__.Selected'
	second := '__checker_lookup_probe_second__.Selected'
	candidates := [missing, first, second]
	tc.file_selective_imports[file_import_key(tc.cur_file, 'Selected')] = candidates
	tc.file_imports_by_file[tc.cur_file] = &FileImportInfo{
		selective_imports: {
			'Selected': candidates
		}
	}
	assert flat.source_type_existing_id(missing) == none
	assert tc.resolve_selective_import_type_symbol('Selected') == none
	assert tc.resolve_selective_import_type_symbol_in_file('Selected', tc.cur_file) == none
	for candidate in candidates {
		assert flat.source_type_existing_id(candidate) == none
	}
	first_id := flat.source_type_id(first)
	second_id := flat.source_type_id(second)
	tc.structs[first_id] = []StructField{}
	tc.enum_names[second_id] = true
	assert tc.resolve_selective_import_type_symbol('Selected')? == first
	assert tc.resolve_selective_import_type_symbol_in_file('Selected', tc.cur_file)? == first
	tc.structs.delete(first_id)
	tc.struct_generic_params[first_id] = ['T']
	assert tc.resolve_selective_import_type_symbol('Selected')? == first
	assert tc.resolve_selective_import_type_symbol_in_file('Selected', tc.cur_file)? == second
	assert flat.source_type_existing_id(missing) == none
}

fn test_checker_type_spelling_probes_preserve_empty_identity_and_registered_kinds() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	missing := '__CheckerLookupProbeUnknownType__'
	assert flat.source_type_existing_id(missing) == none
	assert !tc.type_symbol_spelling_known(missing)
	assert !tc.qualify_candidate_type_spelling_exists(missing)
	assert flat.source_type_existing_id(missing) == none
	tc.structs[u32(0)] = []StructField{}
	assert tc.type_symbol_spelling_known('')
	assert tc.qualify_candidate_type_spelling_exists('')
	for kind in 0 .. 6 {
		name := '__CheckerLookupProbeKind${kind}__'
		name_id := flat.source_type_id(name)
		match kind {
			0 { tc.type_aliases.set_id(name_id, flat.source_type_int) }
			1 { tc.structs[name_id] = []StructField{} }
			2 { tc.interface_names[name_id] = true }
			3 { tc.flag_enums[name_id] = true }
			4 { tc.enum_names[name_id] = true }
			5 { tc.sum_types.set_ids(name_id, [flat.source_type_int]) }
			else {}
		}
		assert tc.type_symbol_spelling_known(name)
	}
}

fn test_checker_scope_queries_do_not_register_absent_contexts() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	file := '__checker_lookup_probe_scope__.v'
	context := '${file}\x01main'
	name_id := flat.source_type_id('__CheckerLookupProbeScoped__')
	assert flat.source_type_existing_id(context) == none
	assert !tc.source_declares_type_in_scope(name_id, file, '')
	assert !tc.source_declares_type_in_scope(name_id, file, 'main')
	assert flat.source_type_existing_id(context) == none
	tc.declared_type_scope_keys[scope_type_key(file, '', name_id)] = true
	assert tc.source_declares_type_in_scope(name_id, file, '')
	assert tc.source_declares_type_in_scope(name_id, file, 'main')
	assert !tc.source_declares_type_in_scope(0, file, 'main')
	tc.declared_type_scope_keys[scope_type_key(file, 'main', 0)] = true
	assert tc.source_declares_type_in_scope(0, file, '')
	other_context := '${file}\x01__checker_lookup_probe_other__'
	assert !tc.source_declares_type_in_scope(name_id, file, '__checker_lookup_probe_other__')
	assert flat.source_type_existing_id(other_context) == none
}

fn test_checker_qualified_type_and_error_probes_keep_original_on_miss() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'checker_lookup_probe_qualify.v'
	tc.cur_module = '__checker_lookup_probe_qualify__'
	name_id := flat.source_type_id('__CheckerLookupProbeBare__')
	qualified := '${tc.cur_module}.__CheckerLookupProbeBare__'
	assert flat.source_type_existing_id(qualified) == none
	assert tc.qualify_type_name_at(name_id, flat.NodeId(-1), tc.cur_module) == name_id
	assert !tc.unqualified_type_symbol_has_scoped_shadow('__CheckerLookupProbeBare__')
	assert flat.source_type_existing_id(qualified) == none
	qualified_id := flat.source_type_id(qualified)
	tc.structs[qualified_id] = []StructField{}
	assert tc.qualify_type_name_at(name_id, flat.NodeId(-1), tc.cur_module) == qualified_id
	assert tc.unqualified_type_symbol_has_scoped_shadow('__CheckerLookupProbeBare__')
	for name_id_ in [ierror_source_error_name, ierror_source_message_error_name] {
		name := flat.source_type_name(name_id_)
		local := '${tc.cur_module}.${name}'
		assert flat.source_type_existing_id(local) == none
		assert tc.resolve_ierror_payload_name(name_id_) == name_id_
		assert !tc.unqualified_type_name_shadows_builtin_in_scope(name, tc.cur_file, tc.cur_module)
		assert flat.source_type_existing_id(local) == none
		local_id := flat.source_type_id(local)
		tc.structs[local_id] = []StructField{}
		assert tc.resolve_ierror_payload_name(name_id_) == local_id
		assert tc.unqualified_type_name_shadows_builtin_in_scope(name, tc.cur_file, tc.cur_module)
	}
}

fn test_checker_c_symbol_and_visibility_probes_keep_value_names_out_of_types() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_file = 'checker_lookup_probe_visibility.v'
	tc.cur_module = '__checker_lookup_probe_consumer__'
	name := 'C.__checker_lookup_probe_function__'
	assert flat.source_type_existing_id(name) == none
	assert !tc.c_symbol_declared_in_module('provider', name)
	assert tc.c_symbol_declared_in_module('', name)
	tc.c_fn_module_ret_types[c_fn_module_signature_key('provider', name)] = builtin_int_type
	assert tc.c_symbol_declared_in_module('provider', name)
	assert flat.source_type_existing_id(name) == none
	assert tc.private_declaration(name) == none
	assert flat.source_type_existing_id(name) == none
	owner := '__checker_lookup_probe_module__'
	assert tc.private_declaration('${owner}.function') == none
	assert flat.source_type_existing_id(owner) == none
	struct_name := 'C.__CheckerLookupProbeStruct__'
	struct_id := flat.source_type_id(struct_name)
	tc.struct_modules[struct_id] = 'provider'
	assert tc.c_symbol_declared_in_module('provider', struct_name)
	tc.declaration_visibility[struct_name] = DeclarationVisibility{
		module_name: 'provider'
		kind:        .struct_decl
	}
	assert tc.private_declaration(struct_name) != none
	tc.c_struct_scoped_fields[c_struct_module_key(tc.cur_module, struct_id)] = []StructField{}
	assert tc.private_declaration(struct_name) == none
}

fn test_checker_hover_and_closure_probes_preserve_misses_and_hits() {
	mut a := flat.FlatAst.new()
	module_id := a.add_val(.ident, '__checker_lookup_probe_hover__')
	a.children << module_id
	receiver := flat.Node{
		kind:           .selector
		value:          '__CheckerLookupProbeEnum__'
		children_count: 1
	}
	mut tc := TypeChecker.new(&a)
	qualified := '__checker_lookup_probe_hover__.__CheckerLookupProbeEnum__'
	assert flat.source_type_existing_id(qualified) == none
	assert tc.vls_enum_receiver(&receiver) == none
	assert flat.source_type_existing_id(qualified) == none
	tc.enum_names[flat.source_type_id(qualified)] = true
	assert tc.vls_enum_receiver(&receiver)? == qualified
	missing := '__CheckerLookupProbeHoverNamed__'
	assert tc.vls_named_type_text(missing) == missing
	assert flat.source_type_existing_id(missing) == none
	closure_name := 'closure.__CheckerLookupProbeClosure__'
	typ := Type(Struct{
		name_id: flat.source_type_id('C.builtin__closure____CheckerLookupProbeClosure__')
	})
	assert flat.source_type_existing_id(closure_name) == none
	assert tc.c_type_uncached(&typ) == 'struct builtin__closure____CheckerLookupProbeClosure__'
	assert flat.source_type_existing_id(closure_name) == none
	closure_id := flat.source_type_id(closure_name)
	tc.structs[closure_id] = []StructField{}
	assert tc.c_type_uncached(&typ) == tc.c_struct_type_name(closure_id)
}
