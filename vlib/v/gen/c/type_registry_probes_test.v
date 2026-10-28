module c

import v.flat
import v.types

fn test_main_alias_probe_does_not_retain_missing_qualified_names() {
	mut aliases := flat.SourceTypeBindings{}
	known := flat.source_type_id('cgen_alias_probe.Known')
	missing := flat.source_type_id('cgen_alias_probe.Missing')
	aliases.set_id(flat.source_type_id('main.cgen_alias_probe.Known'), flat.source_type_int)
	aliases.set_id(flat.source_type_id('main.cgen_alias_probe.Empty'), 0)
	empty := flat.source_type_id('cgen_alias_probe.Empty')
	before := flat.source_type_id('__cgen_alias_probe_before')
	assert (main_type_alias_id(aliases, known) or { panic('known main alias missing') }) == flat.source_type_int
	assert (main_type_alias_id(aliases, empty) or { panic('empty main alias missing') }) == 0
	assert main_type_alias_id(aliases, missing) == none
	assert flat.source_type_existing_id('main.cgen_alias_probe.Missing') == none
	assert flat.source_type_id('__cgen_alias_probe_after') == before + 1
}

fn test_main_alias_probe_reads_stack_and_long_keys_without_interning_misses() {
	mut aliases := flat.SourceTypeBindings{}
	for count in [module_key_view_cap - 5, module_key_view_cap + 7] {
		name := 'x'.repeat(count)
		key := 'main.${name}'
		assert main_type_alias_text_id(aliases, name) == none
		assert flat.source_type_existing_id(key) == none
		aliases.set_id(flat.source_type_id(key), flat.source_type_bool)
		assert (main_type_alias_text_id(aliases, name) or { panic('main alias missing') }) == flat.source_type_bool
		assert flat.source_type_existing_id(name) == none
	}
}

fn test_value_c_type_does_not_retain_failed_main_alias_probes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	typ := types.Type(types.Struct{ name_id: flat.source_type_id('cgen_value_probe.Box') })
	assert g.value_c_type(typ) == 'cgen_value_probe__Box'
	assert flat.source_type_existing_id('main.cgen_value_probe.Box') == none
	assert flat.source_type_existing_id('main.cgen_value_probe__Box') == none
	// C-type caching may retain the rendered type itself; the failed aliases must not be retained.
	tc.type_aliases.set_id(flat.source_type_id('main.cgen_value_probe__Box'), flat.source_type_int)
	assert g.value_c_type(typ) == tc.c_type(types.Type(types.int_))
}

fn test_c_shadow_probes_leave_missing_spellings_absent_and_keep_registered_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	before := flat.source_type_id('__cgen_shadow_probe_before')
	assert g.main_runtime_shadow_fn_c_name('main', 'cgen_shadow_probe_missing') == none
	assert !g.local_name_shadows_c_typedef('cgen_typedef_probe_missing')
	assert flat.source_type_existing_id('C.cgen_shadow_probe_missing') == none
	assert flat.source_type_existing_id('cgen_typedef_probe_missing') == none
	assert flat.source_type_existing_id('C.cgen_typedef_probe_missing') == none
	assert flat.source_type_id('__cgen_shadow_probe_after') == before + 1
	tc.structs[flat.source_type_id('C.cgen_shadow_probe_known')] = []types.StructField{}
	assert (g.main_runtime_shadow_fn_c_name('main', 'cgen_shadow_probe_known') or { '' }) == 'main__cgen_shadow_probe_known'
	// These registries test membership, including a present false marker.
	tc.c_typedef_structs[flat.source_type_id('cgen_typedef_probe_bare')] = false
	tc.c_typedef_structs[flat.source_type_id('C.cgen_typedef_probe_prefixed')] = false
	assert g.local_name_shadows_c_typedef('cgen_typedef_probe_bare')
	assert g.local_name_shadows_c_typedef('cgen_typedef_probe_prefixed')
}

fn test_imported_selector_call_probes_types_without_retaining_function_names() {
	for is_cast in [false, true] {
		mut a := flat.FlatAst.new()
		base := a.add_val(.ident, 'short')
		a.children << base
		name := if is_cast { 'Alias' } else { 'invoke' }
		callee := a.add_node(flat.Node{
			kind:           .selector
			value:          name
			children_start: 0
			children_count: 1
		})
		arg := a.add_val(.int_literal, '7')
		a.children << callee
		a.children << arg
		call := a.add_node(flat.Node{ kind: .call, children_start: 1, children_count: 2 })
		mut tc := types.TypeChecker.new(&a)
		tc.cur_module = 'main'
		tc.cur_file = 'cgen_probe.v'
		tc.file_imports['cgen_probe.v\nshort'] = 'cgen_import_probe'
		tc.imports['short'] = 'cgen_import_probe'
		full_name := 'cgen_import_probe.${name}'
		mut expected := 'cgen_import_probe__invoke(7)'
		if is_cast {
			alias_id := flat.source_type_id(full_name)
			tc.type_aliases.set_id(alias_id, flat.source_type_int)
			expected = '(${tc.c_type(tc.parse_type(alias_id))})(7)'
		} else {
			tc.fn_param_types[full_name] = [types.Type(types.int_)]
			tc.fn_ret_types[full_name] = types.Type(types.int_)
			assert flat.source_type_existing_id(full_name) == none
		}
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.gen_call(call, a.node(call))
		assert g.sb.str() == expected
		if !is_cast {
			assert flat.source_type_existing_id(full_name) == none
		}
	}
}

fn test_interface_signature_probe_does_not_retain_function_prefixes() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	before := flat.source_type_id('__cgen_interface_probe_before')
	assert g.interface_method_param_types('cgen_interface_probe_unknown.invoke') == none
	assert flat.source_type_existing_id('cgen_interface_probe_unknown') == none
	assert flat.source_type_id('__cgen_interface_probe_after') == before + 1
}

fn test_enum_codegen_probes_do_not_retain_absent_names_or_qualifiers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.autofree_mode = true
	tc.cur_module = 'cgen_enum_probe'
	name_id := flat.source_type_id('CgenEnumProbeMissing')
	assert g.enum_type_codegen_name('CgenEnumProbeNeverDeclared') == 'CgenEnumProbeNeverDeclared'
	assert g.enum_typedef_c_name('CgenEnumProbeNeverDeclared') == 'CgenEnumProbeNeverDeclared'
	assert g.enum_autostr_c_name(name_id) == 'CgenEnumProbeMissing'
	assert g.enum_decl_type_name(flat.Node{ kind: .enum_decl, value: 'CgenEnumProbeNoDecl' }, 'cgen_enum_probe') == 'cgen_enum_probe.CgenEnumProbeNoDecl'
	assert flat.source_type_existing_id('CgenEnumProbeNeverDeclared') == none
	assert flat.source_type_existing_id('cgen_enum_probe.CgenEnumProbeMissing') == none
	assert flat.source_type_existing_id('cgen_enum_probe.CgenEnumProbeNoDecl') == none
	// A present empty module denotes main, independently of the absent case.
	known_id := flat.source_type_id('CgenEnumProbeKnown')
	g.enum_modules[known_id] = ''
	assert g.enum_type_codegen_name('CgenEnumProbeKnown') == 'main.CgenEnumProbeKnown'
	assert g.enum_typedef_c_name('CgenEnumProbeKnown') == 'main__CgenEnumProbeKnown'
	tc.enum_names[flat.source_type_id('cgen_enum_probe.CgenEnumProbeMissing')] = true
	assert g.enum_autostr_c_name(name_id) == 'cgen_enum_probe__CgenEnumProbeMissing'
}
