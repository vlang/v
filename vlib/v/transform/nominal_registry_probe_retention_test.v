module transform

import v.flat
import v.types

fn test_nominal_selective_import_probes_skip_missing_candidates_in_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_file = 'nominal_registry_probe.v'
	item_id := flat.source_type_id('__nominal_probe_Item')
	first_id := flat.source_type_id('__nominal_probe_first.Item')
	second_id := flat.source_type_id('__nominal_probe_second.Item')
	missing := '__nominal_probe_missing.Item'
	t.structs[first_id] = StructInfo{}
	t.structs[second_id] = StructInfo{}
	tc.interface_names[first_id] = true
	tc.interface_names[second_id] = true
	tc.file_selective_imports[file_import_key(t.cur_file, flat.source_type_name(item_id))] = [
		missing,
		flat.source_type_name(first_id),
		flat.source_type_name(second_id),
	]
	assert flat.source_type_existing_id(missing) == none
	assert t.selective_import_struct_lookup_name(item_id) or { u32(0) } == first_id
	assert t.selective_import_type_name_for_file(t.cur_file, item_id) or { u32(0) } == first_id
	assert t.resolve_interface_type_name_uncached(item_id) == first_id
	assert flat.source_type_existing_id(missing) == none
	tc.file_selective_imports[file_import_key(t.cur_file, flat.source_type_name(item_id))] = [missing]
	assert t.selective_import_struct_lookup_name(item_id) == none
	assert t.selective_import_type_name_for_file(t.cur_file, item_id) == none
	assert flat.source_type_existing_id(missing) == none
}

fn test_nominal_expected_enum_probes_keep_module_order_and_global_fallback() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.cur_module = '__nominal_probe_current'
	bare_id := flat.source_type_id('__nominal_probe_ExpectedKind')
	current_id := flat.source_type_id('${t.cur_module}.${flat.source_type_name(bare_id)}')
	owner_id := flat.source_type_id('__nominal_probe_owner.${flat.source_type_name(bare_id)}')
	t.enum_types[current_id] = ['ready']
	t.enum_types[owner_id] = ['ready']
	assert t.enum_type_name_for_expected(bare_id, '__nominal_probe_owner') == owner_id
	missing_owner := '__nominal_probe_no_owner.${flat.source_type_name(bare_id)}'
	assert flat.source_type_existing_id(missing_owner) == none
	assert t.enum_type_name_for_expected(bare_id, '__nominal_probe_no_owner') == current_id
	assert flat.source_type_existing_id(missing_owner) == none
	fallback_bare := flat.source_type_id('__nominal_probe_FallbackKind')
	fallback_id := flat.source_type_id('__nominal_probe_other.${flat.source_type_name(fallback_bare)}')
	t.enum_types[fallback_id] = ['ready']
	assert t.enum_type_name_for_expected(fallback_bare, '__nominal_probe_no_owner') == fallback_id
	assert flat.source_type_existing_id('${t.cur_module}.${flat.source_type_name(fallback_bare)}') == none
	assert flat.source_type_existing_id('__nominal_probe_no_owner.${flat.source_type_name(fallback_bare)}') == none
}

fn test_nominal_sql_enum_probes_keep_qualified_success_after_missing_raw_root() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = '__nominal_probe_sql'
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	root := '__nominal_probe_SqlKind'
	qualified_id := flat.source_type_id('${tc.cur_module}.${root}')
	t.enum_types[qualified_id] = ['ready']
	assert flat.source_type_existing_id(root) == none
	assert t.sql_token_root_is_enum_type(root)
	assert flat.source_type_existing_id(root) == none
	missing := '__nominal_probe_sql_missing'
	assert !t.sql_token_root_is_enum_type(missing)
	assert flat.source_type_existing_id(missing) == none
	assert flat.source_type_existing_id('${tc.cur_module}.${missing}') == none
	missing_id := flat.source_type_id('__nominal_probe_sql_type_missing')
	assert !t.sql_transform_type_is_enum(missing_id)
	assert flat.source_type_existing_id('${tc.cur_module}.${flat.source_type_name(missing_id)}') == none
	t.enum_types[u32(0)] = ['empty']
	assert t.sql_token_root_is_enum_type('')
	assert t.sql_transform_type_is_enum(u32(0))
}

fn test_nominal_struct_query_keeps_exact_spelling_and_zero_id() {
	mut t := Transformer{}
	missing := '__nominal_probe_missing_struct'
	assert t.get_struct_info(missing) == none
	assert flat.source_type_existing_id(missing) == none
	t.structs[u32(0)] = StructInfo{ module: 'empty' }
	empty := t.get_struct_info('') or { panic('missing registered empty spelling') }
	assert empty.module == 'empty'
	name := ' __nominal_probe_padded_struct '
	name_id := flat.source_type_id(name)
	t.structs[name_id] = StructInfo{ name: name_id }
	info := t.get_struct_info(name) or { panic('missing registered exact spelling') }
	assert info.name == name_id
	assert t.get_struct_info(name.trim_space()) == none
	assert flat.source_type_existing_id(name.trim_space()) == none
}

fn test_nominal_alias_method_scans_skip_function_namespace_prefixes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	alias_id := flat.source_type_id('__nominal_probe_Alias')
	tc.type_aliases.set_id(alias_id, flat.source_type_i64)
	missing := '__nominal_probe_function_namespace'
	method := '__nominal_probe_keep'
	known := '${flat.source_type_name(alias_id)}.${method}'
	tc.fn_param_types['${missing}.${method}'] = [types.Type(types.i64_)]
	tc.fn_param_types[known] = [types.Type(types.i64_)]
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	assert flat.source_type_existing_id(missing) == none
	assert t.alias_receiver_method_candidates(method) == [known]
	t.collect_alias_methods()
	assert t.alias_receiver_fns[method] == [known]
	assert flat.source_type_existing_id(missing) == none
}

fn test_nominal_resolved_function_probes_do_not_retain_cast_or_alias_candidates() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.begin_sparse_transform_node_caches(0)
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	call := t.make_call_expr_typed(t.make_ident('__nominal_probe_call'), [t.make_int_literal(1)],
		flat.source_type_int)
	target := '__nominal_probe_call_namespace.run'
	tc.sparse_resolved_call_names[int(call)] = target
	assert flat.source_type_existing_id(target) == none
	assert t.try_lower_resolved_named_cast_call(call, a.nodes[int(call)]) == none
	assert flat.source_type_existing_id(target) == none
	receiver := target.all_before_last('.')
	assert t.try_lower_checker_selected_alias_method(call, a.nodes[int(call)], call) == none
	assert flat.source_type_existing_id(receiver) == none
}
