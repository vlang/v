module c

import v.flat
import v.types

fn test_struct_default_registry_misses_do_not_intern_candidate_spellings() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	missing := 'struct_probe_retention.UnknownDefault'
	assert flat.source_type_existing_id(missing) == none
	assert !g.struct_default_type_key_known(missing)
	assert flat.source_type_existing_id(missing) == none
	tc.structs[flat.source_type_id(missing)] = []types.StructField{}
	assert g.struct_default_type_key_known(missing)
	// Zero is a present exact spelling, rather than the result of a failed lookup.
	tc.structs[u32(0)] = []types.StructField{}
	assert g.struct_default_type_key_known('')
	qualified_int := 'struct_probe_default_module.int'
	assert flat.source_type_existing_id(qualified_int) == none
	resolved := g.struct_default_field_type_text(StructDeclInfo{
		module: 'struct_probe_default_module'
	}, flat.source_type_int)
	assert resolved.source_type_id() == flat.source_type_int
	assert flat.source_type_existing_id(qualified_int) == none
}

fn test_struct_selective_and_module_probe_misses_preserve_later_fallbacks() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.cur_file = 'struct_probe_import.v'
	tc.cur_module = 'struct_probe_owner'
	name_id := flat.source_type_id('StructProbeItem')
	known_id := flat.source_type_id('struct_probe_import.StructProbeItem')
	missing := 'struct_probe_missing.StructProbeItem'
	qualified := 'struct_probe_owner.StructProbeItem'
	tc.file_selective_imports['struct_probe_import.v\nStructProbeItem'] = [missing,
		flat.source_type_name(known_id)]
	tc.structs[known_id] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	info := StructDeclInfo{
		node:      flat.Node{ kind: .struct_decl, value: 'StructProbeItem' }
		module:    'struct_probe_import'
		full_name: known_id
	}
	g.struct_decl_infos[known_id] = info
	g.struct_decl_short_infos[name_id] = info
	g.type_name_short_index = new_type_name_short_index(&tc)
	assert flat.source_type_existing_id(missing) == none
	assert g.shared_qualify_leaf_type_text(name_id, tc.cur_module) == known_id
	assert (g.find_struct_decl(name_id) or { panic('selective import fallback missing') }).full_name == known_id
	assert flat.source_type_existing_id(missing) == none
	// With no selective import, the missing preferred module must still allow
	// the short declaration fallback, without matching an unrelated zero key.
	tc.file_selective_imports.clear()
	g.struct_decl_infos[u32(0)] = info
	assert flat.source_type_existing_id(qualified) == none
	assert g.find_struct_decl_preferred_uncached(name_id) == none
	assert (g.find_struct_decl(name_id) or { panic('short declaration fallback missing') }).full_name == known_id
	assert g.shared_qualify_leaf_type_text(name_id, tc.cur_module) == name_id
	assert flat.source_type_existing_id(qualified) == none
}

fn test_enum_and_soa_lookup_misses_do_not_retain_generated_names() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.cur_module = 'struct_probe_enum_owner'
	name_id := flat.source_type_id('StructProbeEnum')
	tc.enum_fields[flat.source_type_id('struct_probe_enum_other.StructProbeEnum')] = ['first']
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	qualified := 'struct_probe_enum_owner.StructProbeEnum'
	assert flat.source_type_existing_id(qualified) == none
	assert (g.enum_fields_for_type(name_id) or { panic('short enum fallback missing') }) == ['first']
	assert flat.source_type_existing_id(qualified) == none
	tc.enum_fields[name_id] = []string{}
	assert (g.enum_fields_for_type(name_id) or { panic('empty enum fields lost') }).len == 0
	soa_name := 'StructProbeMissing_SOA'
	c_name := 'C.${soa_name}'
	assert flat.source_type_existing_id(c_name) == none
	assert !g.soa_companion_has_c_typedef(soa_name)
	assert !g.soa_companion_c_decl_matches(c_name, []SoaFieldInfo{})
	// An absent generated name must not alias the present empty-spelling entry.
	tc.structs[u32(0)] = []types.StructField{}
	assert g.soa_companion_collision('StructProbeOwner', soa_name, []SoaFieldInfo{}) == none
	assert flat.source_type_existing_id(c_name) == none
	tc.structs[flat.source_type_id(c_name)] = [
		types.StructField{ name: 'len', typ: types.Type(types.int_) },
		types.StructField{ name: 'cap', typ: types.Type(types.int_) },
	]
	assert g.soa_companion_has_c_typedef(soa_name)
	assert g.soa_companion_c_decl_matches(c_name, []SoaFieldInfo{})
	assert g.soa_companion_collision('StructProbeOwner', soa_name, []SoaFieldInfo{}) == none
}

fn test_closure_layout_probe_miss_preserves_native_symbol_fallback() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	name := 'C.builtin__closure__StructProbeClosure'
	runtime_name := 'closure.StructProbeClosure'
	g.cache_native_c_symbols['builtin__closure__StructProbeClosure'] = true
	assert flat.source_type_existing_id(runtime_name) == none
	assert g.skip_builtin_struct(name)
	assert flat.source_type_existing_id(runtime_name) == none
	tc.structs[flat.source_type_id(runtime_name)] = []types.StructField{}
	g.cache_native_c_symbols.clear()
	assert g.skip_builtin_struct(name)
}
