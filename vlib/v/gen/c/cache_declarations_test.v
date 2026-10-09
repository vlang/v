module c

import v.flat
import v.types

fn test_cache_declaration_references_ignore_literals_and_comments() {
	mut g := FlatGen.new()
	g.cache_collect_declaration_refs('needed("quoted \\\" ignored", \'x\'); /* hidden */ // hidden_too\nother_2;')
	assert g.cache_decl_refs['needed']
	assert g.cache_decl_refs['other_2']
	for name in ['quoted', 'ignored', 'x', 'hidden', 'hidden_too'] {
		assert !g.cache_decl_refs[name], name
	}
}

fn test_cache_declarations_include_nested_payloads_and_recursive_fields() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['sample.Parent'] = [types.StructField{
		name: 'callback'
		typ:  types.FnType{
			params:      [types.Type(types.Map{
				key_type:   types.Type(types.string_)
				value_type: types.Type(types.OptionType{
					base_type: types.Type(types.Struct{ name: 'sample.Child' })
				})
			})]
			return_type: types.Type(types.void_)
		}
	}]
	tc.structs['sample.Child'] = [types.StructField{
		name: 'parent'
		typ:  types.Pointer{
			base_type: types.Type(types.Struct{ name: 'sample.Parent' })
		}
	}]
	tc.structs['sample.Unused'] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cache_decl_demand = true
	mut seen := map[string]bool{}
	g.cache_require_declaration_type(types.Struct{ name: 'sample.Parent' }, mut seen)
	names := g.c_struct_decl_names()
	assert 'sample.Parent' in names
	assert 'sample.Child' in names
	assert 'sample.Unused' !in names
}

fn test_cache_signature_collection_omits_unused_function_payloads() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['sample.Used'] = []types.StructField{}
	tc.structs['sample.Unused'] = []types.StructField{}
	tc.fn_ret_types['sample.used'] = types.OptionType{
		base_type: types.Type(types.Struct{ name: 'sample.Used' })
	}
	tc.fn_ret_types['sample.unused'] = types.OptionType{
		base_type: types.Type(types.Struct{ name: 'sample.Unused' })
	}
	tc.fn_type_modules['sample.used'] = 'sample'
	tc.fn_type_modules['sample.unused'] = 'sample'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cache_decl_demand = true
	g.cache_decl_refs['sample__used'] = true
	g.collect_checker_declaration_signature_types()
	payloads := g.needed_optional_types.values()
	assert 'sample__Used' in payloads, payloads.str()
	assert 'sample__Unused' !in payloads, payloads.str()
}
