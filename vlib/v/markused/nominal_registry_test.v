module markused

import v.flat
import v.types

fn test_interface_dispatch_probes_do_not_retain_non_type_receivers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	missing := 'nominal_registry_dispatch_missing.Receiver'
	assert flat.source_type_existing_id(missing) == none
	for _ in 0 .. 2 {
		assert interface_dispatch_dotted_name('nominal_registry_dispatch_missing__Receiver__run', &tc) == none
	}
	assert flat.source_type_existing_id(missing) == none

	iface := flat.source_type_id('nominal_registry_dispatch.Iface')
	alias := flat.source_type_id('nominal_registry_dispatch.Alias')
	tc.interface_names[iface] = true
	tc.type_aliases.set_id(alias, iface)
	assert interface_dispatch_dotted_name('nominal_registry_dispatch__Iface__run', &tc)? == 'nominal_registry_dispatch.Iface.run'
	assert interface_dispatch_dotted_name('nominal_registry_dispatch__Alias__run', &tc)? == 'nominal_registry_dispatch.Iface.run'
	assert interface_dispatch_dotted_name('already.dotted.run', &tc)? == 'already.dotted.run'
}

fn test_struct_registry_keeps_exact_names_and_module_fallback_order() {
	item := flat.source_type_id('Item')
	alpha_item := flat.source_type_id('alpha.Item')
	beta_item := flat.source_type_id('beta.Item')
	legacy := flat.source_type_id('Legacy')
	collector := CallCollector{
		struct_decls: {
			item:       StructDeclInfo{ node_id: 1, module: 'alpha' }
			alpha_item: StructDeclInfo{ node_id: 2, module: 'alpha' }
			beta_item:  StructDeclInfo{ node_id: 3, module: 'beta' }
			legacy:     StructDeclInfo{ node_id: 4, module: 'nested.owner' }
			0:          StructDeclInfo{ node_id: 5 }
		}
	}
	assert collector.struct_lookup_name(alpha_item, 'beta') == alpha_item
	assert collector.struct_lookup_name(beta_item, 'alpha') == beta_item
	assert collector.struct_lookup_name(item, 'alpha') == item
	assert collector.struct_lookup_name(item, 'beta') == beta_item
	assert collector.struct_lookup_name(item, 'other') == 0
	assert collector.struct_lookup_name(item, 'main') == 0
	assert collector.struct_lookup_name(item, 'builtin') == 0
	assert collector.struct_lookup_name(item, '') == 0
	assert collector.struct_lookup_name(flat.source_type_id('nested.owner.Legacy'), '') == legacy
	assert collector.struct_lookup_name(flat.source_type_id('other.Legacy'), '') == 0
	assert collector.struct_lookup_name(0, '') == 0
	assert collector.struct_decl_info(item, 'alpha')?.node_id == 1
	assert collector.struct_decl_info(item, 'beta')?.node_id == 3
	assert collector.struct_decl_info(flat.source_type_id('missing.Item'), 'alpha') == none
	unscoped := CallCollector{
		struct_decls: {
			item: StructDeclInfo{ node_id: 6 }
		}
	}
	assert unscoped.struct_lookup_name(item, 'any') == item
}

fn test_prepared_struct_registry_preserves_first_short_and_last_qualified_declaration() {
	mut a := flat.FlatAst.new()
	alpha := a.add_val(.module_decl, 'prepared_registry_alpha')
	first := a.add_val(.struct_decl, 'Item')
	beta := a.add_val(.module_decl, 'prepared_registry_beta')
	second := a.add_val(.struct_decl, 'Item')
	alpha_again := a.add_val(.module_decl, 'prepared_registry_alpha')
	latest := a.add_val(.struct_decl, 'Item')
	main_module := a.add_val(.module_decl, 'main')
	main_decl := a.add_val(.struct_decl, 'MainOnly')
	builtin_module := a.add_val(.module_decl, 'builtin')
	builtin_decl := a.add_val(.struct_decl, 'BuiltinOnly')
	mut tc := types.TypeChecker.new(&a)
	tc.top_level_idx = [i32(alpha), i32(first), i32(beta), i32(second), i32(alpha_again), i32(latest),
		i32(main_module), i32(main_decl), i32(builtin_module), i32(builtin_decl)]
	mut prepared := prepare_markused_declarations(&a, &tc, true)
	assert prepared.ready
	item := flat.source_type_id('Item')
	alpha_item := flat.source_type_id('prepared_registry_alpha.Item')
	beta_item := flat.source_type_id('prepared_registry_beta.Item')
	assert prepared.struct_decls[item].node_id == first
	assert prepared.struct_decls[alpha_item].node_id == latest
	assert prepared.struct_decls[beta_item].node_id == second
	assert prepared.struct_decls[flat.source_type_id('MainOnly')].node_id == main_decl
	assert prepared.struct_decls[flat.source_type_id('BuiltinOnly')].node_id == builtin_decl
	assert flat.source_type_id('main.MainOnly') !in prepared.struct_decls
	assert flat.source_type_id('builtin.BuiltinOnly') !in prepared.struct_decls
	collector := CallCollector{ struct_decls: prepared.struct_decls }
	assert collector.struct_decl_info(item, 'prepared_registry_beta')?.node_id == second
	assert collector.struct_decl_info(alpha_item, 'prepared_registry_beta')?.node_id == latest
	prepared.release()
	// The helper arena owns the registry, but its source spelling IDs remain global.
	assert flat.source_type_name(alpha_item) == 'prepared_registry_alpha.Item'
	assert flat.source_type_name(beta_item) == 'prepared_registry_beta.Item'
}

fn test_generic_nominal_registry_uses_local_module_and_imported_ids() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.struct_generic_params[flat.source_type_id('owner.Box')] = ['T']
	tc.sum_generic_params[flat.source_type_id('dep.Choice')] = ['T']
	collector := CallCollector{
		a:                  &a
		tc:                 &tc
		generic_type_bases: {
			flat.source_type_id('Local'): true
		}
	}
	imports := {
		'd': 'dep'
	}
	assert collector.generic_type_base_is_known(flat.source_type_id('Local'), 'other', imports)
	assert collector.generic_type_base_is_known(flat.source_type_id('Box'), 'owner', imports)
	assert collector.generic_type_base_is_known(flat.source_type_id('owner.Box'), '', imports)
	assert collector.generic_type_base_is_known(flat.source_type_id('d.Choice'), '', imports)
	assert !collector.generic_type_base_is_known(flat.source_type_id('Box'), 'main', imports)
	assert !collector.generic_type_base_is_known(flat.source_type_id('Box'), 'builtin', imports)
	assert !collector.generic_type_base_is_known(flat.source_type_id('Missing'), 'owner', imports)
}

fn test_selective_type_alias_registry_keeps_target_order_and_ignores_other_symbols() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.type_aliases.set(flat.source_type_id('dep.First'), 'owner.Box[int]')
	tc.type_aliases.set(flat.source_type_id('dep.Second'), 'string')
	tc.type_aliases.set(flat.source_type_id('dep.Same'), 'string')
	tc.type_aliases.set(flat.source_type_id('dep.Third'), 'int')
	tc.type_aliases.set_id(flat.source_type_id('dep.Empty'), 0)
	tc.file_selective_imports['caller.v\nSelected'] = ['dep.First', 'dep.function', 'dep.Second',
		'dep.First', 'dep.Same']
	tc.file_selective_imports['other.v\nSelected'] = ['dep.Second', 'dep.Third']
	tc.file_selective_imports['caller.v\nOnlyFunction'] = ['dep.function']
	tc.file_selective_imports['caller.v\n'] = ['dep.Second', 'dep.First']
	tc.file_selective_imports['caller.v\nEmptyTarget'] = ['dep.Empty', 'dep.Empty']
	tc.file_selective_imports['caller.v\nUnregisteredFunction'] = [
		'dep.nominal_registry_unregistered_function',
	]
	if _ := flat.source_type_existing_id('dep.nominal_registry_unregistered_function') {
		assert false
	}
	targets := markused_selective_alias_targets(&tc)
	selected := flat.source_type_id('Selected')
	assert targets[selected] == [flat.source_type_id('owner.Box[int]'), flat.source_type_string,
		flat.source_type_int]
	assert targets[u32(0)] == [flat.source_type_string, flat.source_type_id('owner.Box[int]')]
	assert targets[flat.source_type_id('EmptyTarget')] == [u32(0)]
	assert flat.source_type_id('OnlyFunction') !in targets
	if _ := flat.source_type_existing_id('dep.nominal_registry_unregistered_function') {
		assert false
	}
	assert flat.source_type_existing_id('UnregisteredFunction') == none
	tc.struct_generic_params[flat.source_type_id('owner.Box')] = ['T']
	collector := CallCollector{ a: &a, tc: &tc, selective_alias_targets: targets }
	assert collector.selective_alias_uses_generics(selected, '', map[string]string{}, 0)
	assert !collector.selective_alias_uses_generics(flat.source_type_id('dep.Selected'), '', map[string]string{}, 0)
	assert !collector.selective_alias_uses_generics(flat.source_type_id('Missing'), '', map[string]string{}, 0)
}
