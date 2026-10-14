module c

import v.flat
import v.types

fn test_final_cached_signatures_declare_tuple_payloads_before_wrappers() {
	for is_result in [false, true] {
		mut a := flat.FlatAst.new()
		fn_id := a.add_node(flat.Node{ kind: .fn_decl, value: 'pair' })
		mut tc := types.TypeChecker.new(&a)
		tc.structs['dep.Box'] = [types.StructField{
			name: 'child'
			typ:  types.Struct{ name_id: flat.source_type_id('dep.Child') }
		}]
		tc.structs['dep.Child'] = [types.StructField{
			name: 'value'
			typ:  types.int_
		}]
		tc.structs['dep.Unused'] = []types.StructField{}
		payload := types.Type(types.MultiReturn{
			types: [
				types.Type(types.Struct{ name_id: flat.source_type_id('dep.Box') }),
				types.Type(types.string_),
			]
		})
		tc.fn_ret_types['dep.pair'] = if is_result {
			types.Type(types.ResultType{ base_type: payload })
		} else {
			types.Type(types.OptionType{ base_type: payload })
		}
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.cache_decl_demand = true
		g.decl_types_ready = true
		g.optional_types_ready = true
		g.multi_return_types_ready = true
		g.fn_gen_items = [
			FlatFnGenItem{ node_id: fn_id, module: 'dep', file: 'dep.v', c_name: 'dep__pair' },
		]
		g.gen_type_declaration_block()
		output := g.sb.str()
		child_position := output.index('struct dep__Child {') or { -1 }
		box_position := output.index('struct dep__Box {') or { -1 }
		tuple_position := output.index('struct multi_return_dep__Box_string {') or { -1 }
		wrapper := if is_result { '__v_result' } else { '__v_option' }
		wrapper_position := output.index('typedef struct ${wrapper}_multi_return_dep__Box_string') or {
			-1
		}
		assert child_position >= 0, output
		assert box_position > child_position, output
		assert tuple_position > box_position, output
		assert wrapper_position > tuple_position, output
		assert !output.contains('dep__Unused'), output
	}
}

fn test_cached_header_retains_its_vlib_source_classification() {
	mut a := flat.FlatAst.new()
	a.cached_header_sources['/cache/builtin.vh'] = '/repo/vlib/builtin/array.v'
	a.cached_header_sources['/cache/user.vh'] = '/repo/project/user.v'
	mut g := FlatGen.new()
	g.a = &a
	g.compiler_vroot = '/repo'
	assert g.c_source_file_is_in_vlib('/cache/builtin.vh')
	assert g.c_source_file_is_in_vlib('/repo/vlib/builtin/array.v')
	assert !g.c_source_file_is_in_vlib('/cache/user.vh')
	assert !g.c_source_file_is_in_vlib('/repo/project/user.v')
}

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
					base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Child') })
				})
			})]
			return_type: types.Type(types.void_)
		}
	}]
	tc.structs['sample.Child'] = [types.StructField{
		name: 'parent'
		typ:  types.Pointer{
			base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Parent') })
		}
	}]
	tc.structs['sample.Unused'] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cache_decl_demand = true
	mut seen := map[string]bool{}
	g.cache_require_declaration_type(types.Struct{ name_id: flat.source_type_id('sample.Parent') }, mut seen)
	names := g.c_struct_decl_names()
	assert 'sample.Parent' in names
	assert 'sample.Child' in names
	assert 'sample.Unused' !in names
}

fn test_cache_demand_keeps_fixed_array_alias_element_layouts() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['sample.Box'] = []types.StructField{}
	tc.structs['sample.Unused'] = []types.StructField{}
	tc.type_aliases.set_id('sample.Boxes', flat.source_type_id('[2]sample.Box'))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cache_decl_demand = true
	g.finish_cache_declaration_demand('')
	names := g.c_struct_decl_names()
	assert 'sample.Box' in names, names.str()
	assert 'sample.Unused' !in names, names.str()
}

fn test_cache_demand_keeps_shared_wrapper_payload_layouts() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['sample.Box'] = []types.StructField{}
	tc.struct_modules['sample.Box'] = 'sample'
	tc.structs['sample.Unused'] = []types.StructField{}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cache_decl_demand = true
	g.register_shared_type_name('sample.Box', 'sample')
	g.finish_cache_declaration_demand('')
	names := g.c_struct_decl_names()
	assert 'sample.Box' in names, names.str()
	assert 'sample.Unused' !in names, names.str()
}

fn test_cache_map_key_helpers_follow_their_builtin_prototypes() {
	mut a := flat.FlatAst.new()
	file_id := a.add_val(.file, 'builtin.vh')
	module_id := a.add_val(.module_decl, 'builtin')
	fn_id := a.add_node(flat.Node{
		kind:   .fn_decl
		value:  'fast_string_eq'
		typ:    flat.source_type_bool
		is_mut: true
	})
	mut tc := types.TypeChecker.new(&a)
	tc.fn_ret_types['fast_string_eq'] = types.bool_
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.top_level_node_ids = [int(file_id), int(module_id), int(fn_id)]
	g.cache_decl_demand = true
	g.fixed_array_map_key_types['Array_fixed_string_2'] = types.ArrayFixed{
		elem_type: types.Type(types.string_)
		len:       2
	}
	g.gen_pre_body_support_declarations()
	source := g.sb.str()
	prototype := source.index('bool fast_string_eq(') or { panic(source) }
	definition := source.index('static u64 Array_fixed_string_2_map_key_hash(void* pkey) {') or {
		panic(source)
	}
	assert prototype < definition, source
	assert source.contains('fast_string_eq((*left)[i], (*right)[i])'), source
}

fn test_cache_signature_collection_omits_unused_function_payloads() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['sample.Used'] = []types.StructField{}
	tc.structs['sample.Unused'] = []types.StructField{}
	tc.fn_ret_types['sample.used'] = types.OptionType{
		base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Used') })
	}
	tc.fn_ret_types['sample.unused'] = types.OptionType{
		base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Unused') })
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

fn test_cache_support_demand_collects_enum_and_lifecycle_callees() {
	mut a := flat.FlatAst.new()
	file_id := a.add_node(flat.Node{ kind: .file, value: 'sample.vh' })
	module_id := a.add_node(flat.Node{ kind: .module_decl, value: 'sample' })
	field_id := a.add_node(flat.Node{ kind: .field_decl, value: 'ready' })
	start := a.children.len
	a.children << field_id
	enum_id := a.add_node(flat.Node{
		kind:           .enum_decl
		value:          'Mode'
		children_start: i32(start)
		children_count: 1
	})
	mut tc := types.TypeChecker.new(&a)
	tc.enum_names['sample.Mode'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.top_level_node_ids = [int(file_id), int(module_id), int(enum_id)]
	g.cache_decl_demand = true
	g.module_init_fns = ['sample__init']
	g.module_init_fn_modules['sample__init'] = 'sample'
	g.module_cleanup_fns = ['sample__cleanup']
	g.module_cleanup_fn_modules['sample__cleanup'] = 'sample'
	// Cover both generating the bodies and collecting already prepared bodies.
	for _ in 0 .. 2 {
		g.cache_decl_refs.clear()
		g.cache_collect_support_declaration_refs()
		for callee in ['strconv__format_int', 'sample__init', 'sample__cleanup'] {
			assert g.cache_decl_refs[callee], callee
		}
		assert g.parallel_enum_str_defs.contains('string sample__Mode__autostr(')
		assert g.parallel_init_defs.contains('void _vinit(')
		assert g.parallel_init_defs.contains('void _vcleanup(')
	}
}

fn test_cache_demand_keeps_unused_constant_wrappers_and_their_field_graph_out() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['sample.Used'] = []types.StructField{}
	tc.structs['sample.Unused'] = [types.StructField{
		name: 'child'
		typ:  types.Struct{ name_id: flat.source_type_id('sample.UnusedChild') }
	}]
	tc.structs['sample.UnusedChild'] = [types.StructField{
		name: 'parent'
		typ:  types.Pointer{ base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Unused') }) }
	}]
	tc.const_types['sample.used'] = types.OptionType{
		base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Used') })
	}
	tc.const_types['sample.unused'] = types.OptionType{
		base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Unused') })
	}
	tc.const_types['sample.unused_result'] = types.ResultType{
		base_type: types.Type(types.Struct{ name_id: flat.source_type_id('sample.Unused') })
	}
	// A constant and a function can share a V name but use different C symbols.
	tc.fn_ret_types['sample.used'] = types.int_
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for name, typ in tc.const_types {
		g.const_modules[name] = 'sample'
		cname := g.const_ident_c_name(name)
		g.cache_const_declarations[cname] = 'extern ${tc.c_type(typ)} ${cname};'
	}
	// Preparation discovers all cached signatures before body demand is known.
	g.collect_checker_declaration_signature_types()
	assert 'sample__Unused' in g.needed_optional_types.values()
	g.cache_decl_demand = true
	g.cache_decl_refs[g.const_ident_c_name('sample.used')] = true
	g.finish_cache_declaration_demand('')
	payloads := g.needed_optional_types.values()
	assert 'sample__Used' in payloads, payloads.str()
	assert 'sample__Unused' !in payloads, payloads.str()
	names := g.c_struct_decl_names()
	assert 'sample.Used' in names, names.str()
	assert 'sample.Unused' !in names, names.str()
	assert 'sample.UnusedChild' !in names, names.str()
}

fn test_cache_inline_helpers_keep_transitive_and_macro_references() {
	source := 'static inline int leaf() { return 7; }
static inline int called() {
	/* } */ const char* text = "{ unused }";
	return leaf();
}
static inline int unused() { return 9; }
#define chosen() called()
int main() { return chosen(); }
'
	pruned := cache_prune_generated_support(source)
	assert pruned.contains('static inline int leaf()'), pruned
	assert pruned.contains('static inline int called()'), pruned
	assert !pruned.contains('static inline int unused()'), pruned
	assert pruned.contains('"{ unused }"'), pruned
	assert pruned.contains('#define chosen() called()'), pruned
}

fn test_cache_inline_helpers_preserve_conditional_definitions() {
	source := 'static inline int native() { return 1; }
static inline int portable() { return 2; }
#ifdef NATIVE
static inline int selected() { return native(); }
#else
static inline int selected() { return portable(); }
#endif
static inline int unused() { return 3; }
int main() { return selected(); }
'
	pruned := cache_prune_generated_support(source)
	for name in ['native', 'portable', 'selected'] {
		assert pruned.contains('static inline int ${name}()'), pruned
	}
	assert !pruned.contains('static inline int unused()'), pruned
	assert pruned.contains('#ifdef NATIVE'), pruned
	assert pruned.contains('#else'), pruned
	assert pruned.contains('#endif'), pruned
}

fn test_cache_generated_support_keeps_only_reached_literal_storage() {
	source := 'static const string _v3_lit_used = {"used", 4, 1};
static const string _v3_lit_unused = {"unused", 6, 1};
static string _v3_lit_direct = {"direct", 6, 1};
static inline string used() { return _v3_lit_used; }
static inline string unused() { return _v3_lit_unused; }
int main() { println("_v3_lit_unused"); println(_v3_lit_direct); println(used()); }
'
	pruned := cache_prune_generated_support(source)
	assert pruned.contains('static const string _v3_lit_used'), pruned
	assert pruned.contains('static string _v3_lit_direct'), pruned
	assert !pruned.contains('static const string _v3_lit_unused'), pruned
	assert !pruned.contains('static inline string unused()'), pruned
	assert pruned.contains('println("_v3_lit_unused")'), pruned
}
