module c

import os
import v.flat
import v.pref
import v.types

fn test_used_function_lookup_preserves_source_and_c_spelling() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	mut used := {
		'dep.read': true
		'dep__run': true
		'v_malloc': true
	}
	g.used_fns = &used
	assert g.used_fn_contains_in_module('read', 'dep')
	assert g.used_fn_contains_in_module('run', 'dep')
	assert g.used_fn_contains_in_module('malloc', 'main')
	assert !g.used_fn_contains_in_module('absent', 'dep')
}

fn test_ierror_method_lookup_preserves_source_and_c_spelling() {
	mut g := FlatGen.new()
	assert !g.should_emit_ierror_method('Box.message', 'dep.Box.message')
	g.ierror_method_emit_names['Box.message'] = true
	assert g.should_emit_ierror_method('Box.message', 'dep.Box.message')
	g.ierror_method_emit_names.clear()
	g.ierror_method_emit_names['dep__Box__message'] = true
	assert g.should_emit_ierror_method('Box.message', 'dep.Box.message')
	assert !g.should_emit_ierror_method('Box.code', 'dep.Box.code')
}

fn test_program_specialization_preserves_generated_functions_without_generic_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	node := flat.Node{
		kind:  .fn_decl
		value: 'Box.read'
	}
	assert !g.is_program_specialization_fn_node_with_qfn(node, 0, 'Box__read', 'program.v')
	a.specialized_fn_nodes[0] = true
	assert g.is_program_specialization_fn_node_with_qfn(node, 0, 'Box__read', 'program.v')
	a.specialized_fn_nodes.clear()
	g.cache_program_files['program.v'] = true
	for name in ['__v3_sum_eq_Box', '__v3_autostr_Box', '__v3_default_clone_Box'] {
		generated := flat.Node{
			kind:  .fn_decl
			value: name
		}
		assert g.is_program_specialization_fn_node_with_qfn(generated, 1, name, 'program.v')
		assert !g.is_program_specialization_fn_node_with_qfn(generated, 1, name, 'library.v')
	}
	tc.specialized_generic_fns['Box__read'] = true
	assert g.is_program_specialization_fn_node_with_qfn(node, 0, 'Box__read', 'program.v')
}

// test_c_name_sanitize_operator_overloads validates this v3 regression case.
fn test_c_name_sanitize_operator_overloads() {
	assert c_name('Point.<') == '__v3_internal_symbol_op_Point__lt'
	assert c_name('Point.<=') == '__v3_internal_symbol_op_Point__le'
	assert c_name('Point.>') == '__v3_internal_symbol_op_Point__gt'
	assert c_name('Point.>=') == '__v3_internal_symbol_op_Point__ge'
	assert c_name('Point.[]') == '__v3_internal_symbol_op_Point__op_index'
	assert c_name('Point.[]=') == '__v3_internal_symbol_op_Point__op_index_set'
	assert c_name('Point.**') == '__v3_internal_symbol_op_Point__pow'
}

// An operator's C name never equals the C name of an ordinary method, whether or
// not the receiver has a method named after the operator's C spelling.
fn test_operator_c_names_differ_from_method_c_names() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for op, word in {
		'+':   'plus'
		'-':   'minus'
		'*':   'mul'
		'**':  'mul_'
		'/':   'div'
		'%':   'mod'
		'==':  'eq'
		'!=':  'ne'
		'<':   'lt'
		'>':   'gt'
		'<=':  'le'
		'>=':  'ge'
		'|':   'or'
		'^':   'xor'
		'[]':  'op_index'
		'[]=': 'op_index_set'
	} {
		name := 'Box.${op}'
		ordinary := 'Box.${word}'
		assert g.direct_call_name(name) == g.cname(name)
		assert g.direct_call_name(name) != g.cname(ordinary)
		tc.fn_param_types[ordinary] = []types.Type{}
		assert g.direct_call_name(name) == g.cname(name)
		assert g.direct_call_name(ordinary) == g.cname(ordinary)
		assert g.cname(name) != g.cname(ordinary)
	}
	assert g.cname('Box.**') != g.cname('Box.pow')
	for method in ['get', 'value', 'operator', 'operator[]=', '[]suffix'] {
		name := 'Box.${method}'
		assert g.direct_call_name(name) == g.cname(name)
	}
}

// The builtin `string` operators keep the C names the runtime calls them by.
fn test_builtin_string_operators_keep_runtime_c_names() {
	assert c_name('string.==') == 'string__eq'
	assert c_name('string.<') == 'string__lt'
	assert c_name('string.+') == 'string__plus'
}

fn test_direct_builtin_map_set_aliases_use_the_runtime_symbol() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for name in ['builtin.map__set', 'builtin_map__set', 'builtin__map__set',
		fn_decl_module_key('builtin', 'map.set')] {
		assert g.direct_call_name(name) == 'map__set'
	}
}

fn test_direct_call_abi_overrides_work_after_an_initial_empty_table() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.direct_call_name('dep.call') == 'dep__call'
	g.c_decl_abi_names['dep.call'] = 'custom_call'
	assert g.direct_call_name('dep.call') == 'custom_call'
	g.c_decl_abi_names['dep__other'] = 'custom_other'
	assert g.direct_call_name('dep.other') == 'custom_other'
}

fn test_runtime_shadow_names_keep_declarations_and_uses_consistent() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for name in ['argc', 'argv', 'array_get', 'array_slice', 'int_str', 'new_map', 'string__eq',
		'string__lt', 'string__plus'] {
		declaration := g.local_decl_cname(name)
		assert declaration != g.cname(name)
		assert g.local_cname(name) == declaration
		assert g.debugger_var_expr(name) == declaration
	}
	for name in ['', 'arg', 'args', 'argc2', 'array_set', 'array_get_more', 'new_maps', 'string__ne',
		'string__long'] {
		assert g.local_decl_cname(name) == g.cname(name)
		assert g.local_cname(name) == g.cname(name)
	}
}

fn test_c_name_sanitize_escaped_keywords() {
	assert c_name('@true') == '_v_true'
	assert c_name('@false') == '_v_false'
	assert c_name('Kind.@asm') == 'Kind___v_asm'
}

fn test_c_name_pre_sanitized_classifier() {
	assert c_name_is_pre_sanitized('main__run')
	assert c_name_is_pre_sanitized('foo__Bar__method')
	assert !c_name_is_pre_sanitized('send')
	assert !c_name_is_pre_sanitized('C.printf')
	assert !c_name_is_pre_sanitized('foo__bar-baz')
	assert !c_name_is_pre_sanitized('_str_1')
}

fn test_cached_cname_fast_paths_match_canonical_naming() {
	mut g := FlatGen.new()
	for name in ['run', 'int', 'bool', 'C.bool', 'send', 'malloc', 'int_str', 'exit', '_str_42',
		'_str_value', 'main.run', 'foo.Bar.method', 'C.printf', 'C.SSL_CTX.str', 'Point.<=',
		'pkg.Box[int].value', 'int@static@tag', '__v3_internal_symbol_source'] {
		assert g.cname(name) == c_name(name)
	}
}

fn test_cached_cname_classifier_preserves_special_names_and_bytes() {
	mut g := FlatGen.new()
	for name in ['', '.', '9name', 'a..b', 'a__b.c', '__v3_internal_symbol_x',
		'__v3_internal_symbol_x.y', 'x@static@y', 'x.[]=', 'x.+', '_str_123', 'C.name', 'C.SSL_CTX.str',
		'complex', 'near', 'far', 'pascal', 'cdecl', 'alloca', 'v_alloca',
		'__v3_internal_symbol_libc_alloca', 'C.alloca', 'int_str', 'malloc'] {
		for _ in 0 .. 2 {
			assert g.cname(name) == c_name(name), name
		}
	}
	for byte in 0 .. 256 {
		name := 'prefix' + u8(byte).ascii_str() + 'suffix'
		assert g.cname(name) == c_name(name), byte.str()
		assert g.cname(name.clone()) == c_name(name), byte.str()
	}
}

fn test_cname_recent_cache_checks_content_and_reuses_equal_allocations() {
	mut cache := CNameCache{}
	name := 'pkg.first'
	cache.remember(name, 'pkg__first')
	assert cache.recent(name.clone())? == 'pkg__first'
	// Search for a real slot collision; a collision must never supply another name's value.
	mut collision := ''
	for i in 0 .. 100000 {
		candidate := 'pkg.name_${i}'
		if candidate != name && c_name_recent_slot(candidate) == c_name_recent_slot(name) {
			collision = candidate
			break
		}
	}
	assert collision != ''
	assert cache.recent(collision) == none
	cache.remember(collision, 'pkg__farst')
	assert cache.recent(name) == none
	assert cache.recent(collision.clone())? == 'pkg__farst'
	cache.remember('', '')
	assert cache.recent('')? == ''
}

fn test_c_name_sanitizes_compound_generic_type_arguments() {
	name :=
		c_name('json2.StructKeyDecodeResult[fn(&mbedtls.SSLListener, string) !&mbedtls.SSLCerts]')
	assert name.bytes().all((it >= `a` && it <= `z`) || (it >= `A` && it <= `Z`)
		|| (it >= `0` && it <= `9`) || it == `_`)
}

fn test_c_name_libc_collision_abs() {
	assert c_name('abs') == 'v_abs'
	assert c_name('send') == 'v_send'
	assert c_name('C.abs') == 'abs'
	assert c_name('printf') == 'v_printf'
	assert c_name('raise') == 'v_raise'
	assert c_name('connect') == 'v_connect'
	assert c_name('select') == 'v_select'
	assert c_name('C.printf') == 'printf'
	assert c_name('C.connect') == 'connect'
	assert c_name('C.select') == 'select'
	assert c_name('C.send') == 'send'
	assert c_name('index') == 'v_index'
	assert c_name('log') == 'v_log'
	assert c_name('C.index') == 'index'
	assert c_name('C.log') == 'log'
}

fn test_c_name_preserves_c_receiver_method_namespace() {
	assert c_name('C.SSL_CTX.str') == 'C__SSL_CTX__str'
}

fn test_struct_init_main_type_lock_matches_only_a_type_component() {
	assert struct_init_has_main_type_lock('main.Context')
	assert struct_init_has_main_type_lock('other.Box[map[other.Key]main.Context]')
	assert !struct_init_has_main_type_lock('domain.Context')
	assert !struct_init_has_main_type_lock('some.main.Context')
}

fn test_struct_init_main_alias_target_keeps_declaration_scope() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.structs['Context'] = []types.StructField{}
	tc.struct_modules['Context'] = 'main'
	tc.structs['veb.Context'] = []types.StructField{}
	tc.struct_modules['veb.Context'] = 'veb'
	tc.type_aliases.set_id('AliasContext', flat.source_type_id('Context'))
	tc.type_alias_modules['AliasContext'] = 'main'
	tc.cur_module = 'veb'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc

	assert flat.source_type_name(g.struct_type_alias_target(flat.source_type_id('main.AliasContext')) or { u32(0) }) == 'main.Context'
}

fn test_c_name_generated_string_symbol_collision() {
	assert c_name('_str_1') == 'v__str_1'
	assert c_name('_str_002') == 'v__str_002'
	assert c_name('_str_value') == '_str_value'
	assert c_name('C._str_3') == '_str_3'
}

fn test_direct_call_uses_custom_enum_method_symbol() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.enum_names['token.Kind'] = true
	tc.cur_module = 'ast'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.direct_call_name('token.Kind.str') == 'token__Kind_str'
	assert g.direct_call_name_for_call(flat.empty_node, 'token.Kind.str') == 'token__Kind_str'
	tc.enum_names['ast.Kind'] = true
	assert g.direct_call_name('Kind.str') == 'Kind_str'
}

fn test_direct_call_does_not_prefix_synthetic_helper_with_owner_module() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'ast'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.non_generic_fn_names_by_module['ast\x01__v3_autostr_ast__Comment'] = true
	g.non_generic_fn_names_by_module['ast\x01__v3_default_clone_json2__Any'] = true
	g.non_generic_fn_names_by_module['ast\x01__v3_struct_eq_ast__Comment'] = true

	assert g.direct_call_name_for_call(flat.empty_node, '__v3_autostr_ast__Comment') == '__v3_autostr_ast__Comment'
	assert g.direct_call_name_for_call(flat.empty_node, 'ast.__v3_autostr_ast__Comment') == '__v3_autostr_ast__Comment'
	assert g.direct_call_name_for_call(flat.empty_node, '__v3_default_clone_json2__Any') == '__v3_default_clone_json2__Any'
	assert g.direct_call_name_for_call(flat.empty_node, 'ast.__v3_default_clone_json2__Any') == '__v3_default_clone_json2__Any'
	assert g.fn_c_name_in_module('main', '__v3_default_clone_json2__Any') == '__v3_default_clone_json2__Any'
	assert g.direct_call_name_for_call(flat.empty_node, '__v3_struct_eq_ast__Comment') == '__v3_struct_eq_ast__Comment'
	assert g.direct_call_name_for_call(flat.empty_node, 'ast.__v3_struct_eq_ast__Comment') == '__v3_struct_eq_ast__Comment'
	assert g.fn_c_name_in_module('ast', '__v3_struct_eq_ast__Comment') == '__v3_struct_eq_ast__Comment'
	assert qualified_fn_name_in_module('ast', '__v3_struct_eq_ast__Comment') == '__v3_struct_eq_ast__Comment'
}

fn test_main_function_is_prefixed_when_preserved_c_header_owns_typedef_name() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.inlined_c_typedef_names['sqlite3'] = true

	assert g.fn_c_name_in_module('main', 'sqlite3') == 'main__sqlite3'
	assert g.main_runtime_shadow_fn_c_name('main', 'sqlite3') or { '' } == 'main__sqlite3'
	assert g.fn_c_name_in_module('database', 'sqlite3') == 'database__sqlite3'
}

fn test_main_function_is_prefixed_when_declared_c_type_owns_name() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.structs['C.sqlite3'] = []types.StructField{}

	assert g.fn_c_name_in_module('main', 'sqlite3') == 'main__sqlite3'
	assert g.main_runtime_shadow_fn_c_name('main', 'sqlite3') or { '' } == 'main__sqlite3'
	assert g.fn_c_name_in_module('database', 'sqlite3') == 'database__sqlite3'
}

fn test_main_function_is_prefixed_when_declared_c_function_owns_name() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.fn_ret_types['C.get_value'] = types.Type(types.int_)
	g.fn_decl_ret_types[fn_decl_module_key('main', 'get_value')] = tc.parse_type(flat.source_type_id('int'))

	assert g.fn_c_name_in_module('main', 'get_value') == 'main__get_value'
	assert g.main_runtime_shadow_fn_c_name('main', 'get_value') or { '' } == 'main__get_value'
	assert g.fn_c_name_in_module('database', 'get_value') == 'database__get_value'
}

fn test_main_function_is_prefixed_when_objective_c_owns_id_name() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc

	assert g.fn_c_name_in_module('main', 'id') == 'main__id'
	assert g.main_runtime_shadow_fn_c_name('main', 'id') or { '' } == 'main__id'
	assert g.fn_c_name_in_module('database', 'id') == 'database__id'
}

fn test_main_sync_function_names_use_the_reserved_namespace() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	for name in ['sync', 'syncfs'] {
		internal := '__v3_internal_symbol_libc_${name}'
		assert g.fn_c_name_in_module('main', name) == internal
		assert g.fn_c_name_in_module('', name) == internal
		assert g.direct_call_name(name) == internal
		assert g.direct_call_name('main.${name}') == internal
		assert g.callback_c_fn_name(name) == internal
		assert g.callback_c_fn_name('main.${name}') == internal
		assert g.fn_c_name_in_module('main', 'main__${name}') == 'main__${name}'
		assert g.fn_c_name_in_module('main', internal) == '__v3_internal_symbol_source_${internal}'
		assert g.fn_c_name_in_module('worker', name) == 'worker__${name}'
		assert g.direct_call_name('C.${name}') == name
		assert g.callback_c_fn_name('C.${name}') == name
	}
	tc.cur_module = 'worker'
	assert g.direct_call_name('main.sync') == '__v3_internal_symbol_libc_sync'
	assert g.direct_call_name('main.syncfs') == '__v3_internal_symbol_libc_syncfs'
}

fn test_target_libc_opaque_packed_struct_restores_packing() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	name := 'C.TargetOpaque'
	tc.structs[name] = []types.StructField{}
	node_id := a.add_node(flat.Node{
		kind:  .struct_decl
		value: name
	})
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.set_target_libc_headers(true)
	g.register_struct_decl_info_at(int(node_id), name, name, 'main', '/project/main.v', a.nodes[int(node_id)])
	g.decl_attrs[int(node_id)] = ['packed']
	g.emit_struct(name)
	c_code := g.sb.str()
	assert c_code.contains('#pragma pack(push, 1)\nstruct TargetOpaque;\n#pragma pack(pop)')
}

fn aligned_struct_decl_c(ccompiler string, attrs string, union_decl bool) string {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	name := 'Aligned'
	tc.structs[name] = []types.StructField{}
	if union_decl {
		tc.unions[name] = true
	}
	node_id := a.add_node(flat.Node{
		kind:  .struct_decl
		value: name
		typ:   flat.source_type_id(attrs)
	})
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.set_ccompiler(ccompiler)
	g.register_struct_decl_info_at(int(node_id), name, name, 'main', '/project/main.v', a.nodes[int(node_id)])
	g.emit_struct(name)
	return g.sb.str()
}

fn test_aligned_struct_decls_use_the_msvc_alignment_spelling() {
	msvc := aligned_struct_decl_c('msvc', 'aligned=8', false)
	assert msvc.contains('struct __declspec(align (8)) Aligned {'), msvc
	assert !msvc.contains('__attribute__'), msvc
	msvc_union := aligned_struct_decl_c('msvc', 'aligned=16', true)
	assert msvc_union.contains('union __declspec(align (16)) Aligned {'), msvc_union
	// A bare `@[aligned]` means the target's largest alignment, as with GCC.
	msvc_bare := aligned_struct_decl_c('msvc', 'aligned', false)
	assert msvc_bare.contains('struct __declspec(align (16)) Aligned {'), msvc_bare
	gcc := aligned_struct_decl_c('gcc', 'aligned=8', false)
	assert gcc.contains('struct Aligned {'), gcc
	assert gcc.contains('} __attribute__((aligned(8)));'), gcc
	assert !gcc.contains('__declspec'), gcc
}

fn test_collect_cache_native_c_symbols_only_records_type_declarations() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.structs['C.HeaderTag'] = []types.StructField{}
	tc.structs['C.HeaderAlias'] = []types.StructField{}
	tc.structs['C.HeaderEnum'] = []types.StructField{}
	tc.structs['C.HeaderScalar'] = []types.StructField{}
	tc.structs['C.HeaderOther'] = []types.StructField{}
	tc.structs['C.CommentOnly'] = []types.StructField{}
	tc.structs['C.StringOnly'] = []types.StructField{}
	tc.structs['C.UnrelatedOpaque'] = []types.StructField{}
	tc.structs['C.HEADER_ENUM_VALUE'] = []types.StructField{}
	tc.structs['C.BOOL'] = []types.StructField{}

	g.collect_cache_native_c_symbols('// typedef int CommentOnly;\n"typedef int StringOnly;";\nint f(int UnrelatedOpaque);\ntypedef struct HeaderTag { int field_name; } HeaderAlias;\nenum HeaderEnum { HEADER_ENUM_VALUE };\ntypedef int HeaderScalar, HeaderOther;')

	assert g.cache_native_c_symbols['HeaderTag']
	assert g.cache_native_c_symbols['HeaderAlias']
	assert g.cache_native_c_symbols['HeaderEnum']
	assert g.cache_native_c_symbols['HeaderScalar']
	assert g.cache_native_c_symbols['HeaderOther']
	assert !g.cache_native_c_symbols['CommentOnly']
	assert !g.cache_native_c_symbols['StringOnly']
	assert !g.cache_native_c_symbols['UnrelatedOpaque']
	assert g.cache_native_c_symbols['HEADER_ENUM_VALUE']
	assert !g.cache_native_c_symbols['field_name']
	assert g.skip_builtin_struct('C.HeaderScalar')
	assert !g.skip_builtin_struct('C.UnrelatedOpaque')
	g.inlined_c_typedef_names['BOOL'] = true
	assert g.skip_builtin_struct('C.BOOL')
}

fn test_collect_cache_native_c_symbols_records_sokol_enum_constants() {
	mut g := FlatGen.new()
	header := os.read_file(os.join_path(@VEXEROOT, 'thirdparty', 'sokol', 'sokol_app.h')) or {
		panic(err)
	}
	g.collect_cache_native_c_symbols(header)
	assert g.cache_native_c_symbols['SAPP_MOUSECURSOR_DEFAULT']
}

fn test_voidptr_method_value_arg_does_not_panic_for_alias_to_voidptr() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	alias_to_voidptr := types.Type(types.Alias{
		name_id:   flat.source_type_id('Data')
		base_type: &types.Type(types.Pointer{
			base_type: &types.Type(types.void_)
		})
	})
	assert !g.voidptr_method_value_arg(flat.empty_node, alias_to_voidptr)
}

fn test_same_named_user_context_does_not_route_to_embedded_framework_context() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.cur_module = 'veb'
	tc.structs['main.Context'] = [
		types.StructField{
			name:     'veb.Context'
			typ:      types.Type(types.Struct{
				name_id: flat.source_type_id('veb.Context')
			})
			is_embed: true
		},
	]

	base := types.Type(types.Struct{
		name_id: flat.source_type_id('main.Context')
	})
	expected := types.Type(types.Struct{
		name_id: flat.source_type_id('Context')
	})
	assert g.embedded_receiver_path_for_expected(base, expected) == none
	assert g.emitted_method_belongs_to_receiver(base, 'before_request', 'Context__before_request')
	assert !g.emitted_method_belongs_to_receiver(base, 'before_request', 'veb__Context__before_request')
}

fn test_array_receiver_method_is_not_reselected_as_generic() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.add_generic_method_candidate(flat.source_type_id('jsonrpc'), 'encode_batch',
		generic_method_candidate_for_decl('jsonrpc.[]Request.encode_batch', types.Type(types.string_),
			[]types.Type{}))

	assert g.specialized_generic_method_name_for_call_with_arg_count(flat.empty_node, 'jsonrpc.[]Response.encode_batch', -1) == none
}

fn test_context_lookup_cache_tracks_source_file_imports() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.file_imports['one.v\nkind'] = 'first.token'
	tc.file_imports['two.v\nkind'] = 'second.token'
	tc.enum_names['first.token.Kind'] = true
	tc.enum_names['second.token.Kind'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.cur_file = 'one.v'
	assert g.import_alias_module('kind')? == 'first.token'
	assert g.enum_selector_base_name(flat.source_type_id('kind.Kind'))? == flat.source_type_id('first.token.Kind')
	tc.cur_file = 'two.v'
	assert g.import_alias_module('kind')? == 'second.token'
	assert g.enum_selector_base_name(flat.source_type_id('kind.Kind'))? == flat.source_type_id('second.token.Kind')
}

fn test_transformed_module_call_namespace_arguments() {
	mut a := flat.FlatAst.new()
	mut g := FlatGen.new()
	g.a = &a
	callee := a.add_val(.ident, 'net.flags.read')
	for name in ['net.flags', 'flags', 'net.flags.flags', 'other.flags', 'lags', 'net.flagsxflags'] {
		arg := a.add_val(.ident, name)
		start := a.begin_children()
		a.add_child(callee)
		a.add_child(arg)
		call := flat.Node{ kind: .call, children_start: start, children_count: 2 }
		expected := if name in ['net.flags', 'flags', 'net.flags.flags'] { 2 } else { 1 }
		assert g.target_module_call_arg_start('net.flags.read', call) == expected, name
		a.nodes[int(arg)].kind = .string_literal
		assert g.target_module_call_arg_start('net.flags.read', call) == 1
	}
}

fn test_cgen_flattened_generic_receiver_short_variants() {
	assert cgen_flattened_generic_receiver_short_variants('foo__Bar_baz__Qux') == [
		'Bar_Qux',
	]
	assert cgen_flattened_generic_receiver_short_variants('mod.foo__Bar_baz__Qux') == [
		'Bar_Qux',
		'mod.Bar_Qux',
	]
}

fn test_cgen_typeof_display_canonicalizes_fixed_array_generic_args() {
	assert typeof_display_type_name('Box[fn () int]') == 'Box[fn () int]'
	assert typeof_display_type_name('Box[chan int]') == 'Box[chan int]'
	assert typeof_display_type_name('chan int[3]') == 'chan [3]int'
	assert typeof_display_type_name('Box[int[3]]') == 'Box[[3]int]'
	assert typeof_display_type_name('Pair[int[3], Box[string[2]]]') == 'Pair[[3]int, Box[[2]string]]'
	assert typeof_display_type_name('Box[int][3]') == '[3]Box[int]'
	fixed_maps := types.Type(types.ArrayFixed{
		elem_type: &types.Type(types.Map{
			key_type:   &types.Type(types.String{})
			value_type: &types.Type(types.int_)
		})
		len:       3
	})
	assert typeof_display_resolved_type_name(fixed_maps) == '[3]map[string]int'
}

fn test_fixed_array_typedef_allows_opaque_pointer_elements() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.tc = &tc
	opaque := types.Type(types.Struct{
		name_id: flat.source_type_id('C.Foo')
	})
	assert g.fixed_array_type_has_unknown_struct(opaque)
	assert !g.fixed_array_type_has_unknown_struct(types.Type(types.Pointer{
		base_type: &types.Type(opaque)
	}))
}

fn test_sum_type_index_rejects_ambiguous_qualified_suffix() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types.set('a.tast.Value', ['a.tast.First', 'a.tast.Target'])
	tc.sum_types.set('b.tast.Value', ['b.tast.Target', 'b.tast.Second'])
	mut g := FlatGen.new()
	g.tc = &tc
	assert g.sum_type_index(flat.source_type_id('tast.Value'), flat.source_type_id('b.tast.Target')) == 0
}

fn test_sum_name_lookup_preserves_qualified_type_identity() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.sum_types.set('left.Value', ['left.Item', 'right.Item'])
	tc.sum_types.set('right.Value', ['int', 'string'])
	mut g := FlatGen.new()
	g.tc = &tc
	g.precompute_sum_name_lookup()
	assert g.resolve_sum_name(flat.source_type_id('Value')) == flat.source_type_id('Value')
	assert g.resolve_sum_name(flat.source_type_id('left.Value')) == flat.source_type_id('left.Value')
	assert g.resolve_sum_name(flat.source_type_id('right.Value')) == flat.source_type_id('right.Value')
	assert g.resolve_variant(flat.source_type_id('left.Value'), flat.source_type_id('Item')) == flat.source_type_id('Item')
	assert g.sum_type_index(flat.source_type_id('left.Value'), flat.source_type_id('Item')) == 0
	assert g.sum_type_index(flat.source_type_id('left.Value'), flat.source_type_id('left.Item')) == 1
	assert g.sum_type_index(flat.source_type_id('left.Value'), flat.source_type_id('right.Item')) == 2
	assert g.resolve_variant(flat.source_type_id('left.Value'), flat.source_type_id('left.Item')) == flat.source_type_id('left.Item')
	assert g.resolve_variant(flat.source_type_id('left.Value'), flat.source_type_id('right.Item')) == flat.source_type_id('right.Item')
	tc.sum_types.set('extra.left__Value', ['Raw__Name', 'Raw.Name'])
	g.precompute_sum_name_lookup()
	assert g.resolve_sum_name(flat.source_type_id('left__Value')) == flat.source_type_id('left.Value')
	assert g.resolve_variant(flat.source_type_id('extra.left__Value'), flat.source_type_id('Raw__Name')) == flat.source_type_id('Raw__Name')
}

fn test_sum_type_index_emission_override_is_limited_to_flatgen() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut used := {
		'main': true
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.used_fns = &used
	assert g.should_emit_fn_node_in_module_known(flat.Node{
		kind:  .fn_decl
		value: 'FlatGen.sum_type_index'
	}, -1, 'c', 'interface.v', 'c__FlatGen__sum_type_index', false)
	assert !g.should_emit_fn_node_in_module_known(flat.Node{
		kind:  .fn_decl
		value: 'Transformer.sum_type_index'
	}, -1, 'transform', 'sum.v', 'transform__Transformer__sum_type_index', false)
}

fn test_typeof_type_index_is_independent_of_sum_variant_order() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.sum_types.set('main.Value', ['main.Foo', 'main.Bar'])
	tc.sum_types.set('main.Other', ['main.Baz', 'main.Qux'])
	names := ['main.Foo', 'main.Bar', 'main.Baz', 'main.Qux']
	tc.runtime_type_indexes = types.stable_type_indexes(names.map(flat.source_type_id(it)))
	mut g := FlatGen.new()
	g.tc = &tc
	for name in names {
		short := name.all_after_last('.')
		assert g.type_index_for_type_name(flat.source_type_id(short)) == tc.runtime_type_indexes[flat.source_type_id(name)]
	}
	assert g.type_index_for_type_name(flat.source_type_id('int')) == 8
	assert g.type_index_for_type_name(flat.source_type_id('&int')) == 8 | (1 << 16)
}

fn test_fn_decl_variadic_resolves_alias_before_short_fallback() {
	mut g := FlatGen.new()
	g.modules['http'] = 'b.http'
	g.fn_decl_variadic['b.http.total'] = true
	g.fn_decl_variadic['total'] = false
	g.fn_decl_variadic_short_counts['total'] = 2
	assert g.fn_decl_is_variadic('http.total', 'http.total')
	assert !g.fn_decl_is_variadic('missing.total', 'missing.total')
	assert !g.fn_decl_is_variadic('missing__total', 'total')
	g.fn_decl_variadic_short_counts['total'] = 1
	g.fn_decl_variadic['total'] = true
	assert g.fn_decl_is_variadic('missing.total', 'missing.total')
}

fn test_guarded_preamble_externs_keep_explicit_declarations() {
	mut g := FlatGen.new()
	assert g.should_emit_c_extern_decl('fseeko')
	assert g.should_emit_c_extern_decl('ftello')
	assert g.should_emit_c_extern_decl('mkdir')
	assert g.should_emit_c_extern_decl('chmod')
	assert g.should_emit_c_extern_decl('symlink')
	assert g.should_emit_c_extern_decl('request')
	g.c_directives << CDirective{
		text: '#include <math.h>'
	}
	for name in ['accept', 'accept4', 'bind', 'chdir', 'execve', 'getuid', 'gmtime_r', 'ioctl',
		'pthread_rwlockattr_destroy', 'pthread_sigmask', 'rmdir', 'sigtimedwait', 'syscall'] {
		assert !g.should_emit_c_extern_decl(name)
	}
}

fn test_preinclude_uses_system_libc_preamble() {
	mut g := FlatGen.new()
	g.preinclude_directives << '#include <X11/Xlib.h>'
	assert g.c_directives_use_system_libc()
}

fn test_system_libc_preamble_identifies_glibc_before_manual_stdio_declarations() {
	mut g := FlatGen.new()
	g.c_directives << CDirective{
		text: '#include <math.h>'
	}
	g.preamble()
	preamble := g.sb.str()
	features := preamble.index('#include <features.h>') or { -1 }
	manual_stdio := preamble.index('// c_headers') or { -1 }
	assert features >= 0
	assert manual_stdio >= 0
	assert features < manual_stdio
}

fn test_header_backed_c_declarations_skip_generated_prototypes() {
	source := '/project/user.v'
	header_source := '/project/bindings.c.v'

	// A `fn C.` declaration in a file that includes a header is already declared by
	// that header; a generated prototype would only conflict with it.
	mut g := FlatGen.new()
	g.note_c_include_directive('bindings', header_source)
	assert !g.should_emit_c_extern_decl_from_file('mbedtls_ssl_write', header_source, 'bindings')

	// A file that links a C source or object ships no header, so it keeps its
	// prototype, and so does a module that only links a C library.
	mut linked := FlatGen.new()
	linked.note_c_flag_directive('bindings', header_source, '@VMODROOT/helper.o')
	assert linked.should_emit_c_extern_decl_from_file('helper_fn', header_source, 'bindings')

	mut lib_only := FlatGen.new()
	lib_only.note_c_flag_directive('bindings', source, '-lfoo')
	assert lib_only.should_emit_c_extern_decl_from_file('foo_open', source, 'bindings')
	lib_only.note_c_include_directive('bindings', '/project/other.c.v')
	assert !lib_only.should_emit_c_extern_decl_from_file('foo_open', source, 'bindings')

	// Nothing links and nothing includes: with system headers the symbol has to come
	// from somewhere else, so V3 stays out of the way, exactly like the V1 backend.
	mut with_system_headers := FlatGen.new()
	with_system_headers.c_directives << CDirective{
		text: '#include <stdio.h>'
	}
	assert with_system_headers.c_directives_use_system_libc()
	assert !with_system_headers.should_emit_c_extern_decl_from_file('unrelated_api', source, 'main')

	// A unit without system headers has no other declaration of the binding, so it gets
	// the prototype even for a program's own `fn C.` (#30781).
	mut bare := FlatGen.new()
	assert !bare.c_directives_use_system_libc()
	assert bare.should_emit_c_extern_decl_from_file('unrelated_api', source, 'main')
}

fn test_apple_framework_include_does_not_match_x11() {
	assert c_is_apple_framework_include('<Cocoa/Cocoa.h>')
	assert c_is_apple_framework_include('<CoreFoundation/CFString.h>')
	assert !c_is_apple_framework_include('<X11/Xlib.h>')
	assert !c_is_apple_framework_include('<sys/ptrace.h>')
}

fn test_objective_c_header_detection() {
	assert c_header_text_needs_objective_c('#import <Cocoa/Cocoa.h>\n')
	assert c_header_text_needs_objective_c('@interface AppDelegate : NSObject\n@end\n')
	assert c_header_text_needs_objective_c('@class ForwardDeclaredClass;\n')
	assert c_header_text_needs_objective_c('@protocol ForwardDeclaredProtocol;\n')
	assert c_header_text_needs_objective_c('id value = @"Objective-C string";\n')
	assert c_header_text_needs_objective_c('static inline id<NSCopying> copy(id<NSCopying> value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline id identity(id obj) { return obj; }\n')
	assert c_header_text_needs_objective_c('static inline instancetype identity(instancetype obj) { return obj; }\n')
	assert c_header_text_needs_objective_c('static inline Class identity(Class value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline SEL identity(SEL value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline Protocol *identity(Protocol *value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline NSArray<NSString *> *copy_names(NSArray<NSString *> *value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline NSObject<NSCopying> *copy(NSObject<NSCopying> *value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @42; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @YES; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @[]; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @{}; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(int value) { return @(value); }\n')
	assert c_header_text_needs_objective_c('id value = (__bridge id)pointer;\n')
	assert c_header_text_needs_objective_c('void *value = (__bridge_retained void *)pointer;\n')
	assert c_header_text_needs_objective_c('Object value = (__bridge_transfer Object)pointer;\n')
	assert c_header_text_needs_objective_c('typedef struct objc_class *Class; static __strong Class identity(__strong Class value) { return value; }\n')
	assert c_header_text_needs_objective_c('static __weak Class weak_value; __autoreleasing Class *out_value; __unsafe_unretained Class unsafe_value;\n')
	assert c_header_text_needs_objective_c('static __kindof Class identity(__kindof Class value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline id helper(id obj) { return [obj description]; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (id)value; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (Class *)value; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (const id)value; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (const volatile Class *)value; }\n')
	assert !c_header_text_needs_objective_c('static inline int helper(int *values, int i) { return values[i]; }\nint table[4] = {[0] = 1};\n[[gnu::unused]] static int state;\n// [obj description] @42\nconst char *message = "[obj description] @42";\n')
	assert !c_header_text_needs_objective_c('static inline int id(int obj) { return obj; }\n')
	assert !c_header_text_needs_objective_c('typedef unsigned id; static inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('typedef void *Class; static inline Class identity(Class value) { return value; }\n')
	assert !c_header_text_needs_objective_c('typedef unsigned SEL; static inline SEL identity(SEL value) { return value; }\n')
	assert !c_header_text_needs_objective_c('typedef void *Protocol; static inline Protocol identity(Protocol value) { return value; }\n')
	assert !c_header_text_needs_objective_c('struct Class { int value; }; union id { int value; }; enum SEL { sel_value }; struct Protocol { int value; }; union instancetype { int value; };\nstatic struct Class class_identity(struct Class value) { return value; }\nstatic union id id_identity(union id value) { return value; }\nstatic enum SEL sel_identity(enum SEL value) { return value; }\nstatic struct Protocol protocol_identity(struct Protocol value) { return value; }\nstatic union instancetype instance_identity(union instancetype value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#define id unsigned\n#define Class unsigned\n#define SEL unsigned\n#define Protocol unsigned\n#define instancetype unsigned\nid id_value; Class class_value; SEL selector_value; Protocol protocol_value; instancetype instance_value;\n')
	assert !c_header_text_needs_objective_c('#define id unsigned\nstatic inline void *identity(void *value) { return (const id)value; }\n')
	assert !c_header_text_needs_objective_c('static int id; static inline int consume(int value) { return value; } static inline int use(void) { return consume(id) + 1; }\n')
	assert c_header_text_needs_objective_c('#define id unsigned\n#undef id\nstatic inline void *identity(void *value) { return (id)value; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (id)value; }\n#define id unsigned\n')
	assert c_header_text_needs_objective_c('#define id(value) value\nstatic inline id identity(id value) { return value; }\n')
	assert c_header_text_needs_objective_c('#if 0\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#if __has_attribute(aligned)\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#if __has_attribute(__packed__)\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert c_header_text_needs_objective_c('#if __has_attribute(definitely_nonexistent_codex_attribute)\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert c_header_text_needs_objective_c('#define __has_attribute(x) 0\n#if __has_attribute(aligned)\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#define __has_attribute(x) 1\n#if __has_attribute(definitely_nonexistent_codex_attribute)\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#if __has_attribute(aligned) == 1\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#if !__has_attribute(definitely_nonexistent_codex_attribute)\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert c_header_text_needs_objective_c('#define IGNORE(...)\nIGNORE(typedef unsigned id);\nstatic inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#define __bridge\nvoid *value = (__bridge void *)pointer;\n')
	assert !c_header_text_needs_objective_c('#define __bridge_retained\nvoid *value = (__bridge_retained void *)pointer;\n')
	assert !c_header_text_needs_objective_c('#define __bridge_transfer\nvoid *value = (__bridge_transfer void *)pointer;\n')
	assert !c_header_text_needs_objective_c('#define __strong\n#define __weak\n#define __autoreleasing\n#define __unsafe_unretained\n#define __kindof\n__strong void *strong_value;\n__weak void *weak_value;\n__autoreleasing void **out_value;\n__unsafe_unretained void *unsafe_value;\n__kindof void *kind_value;\n')
	assert !c_header_text_needs_objective_c_for_target('__strong void *value;\n', [
		'-D__strong',
	], false, pref.host_target())
	assert c_header_text_needs_objective_c('#define __bridge\n#undef __bridge\nid value = (__bridge id)pointer;\n')
	assert c_header_text_needs_objective_c('#define __bridge(value) value\nid value = (__bridge id)pointer;\n')
	assert !c_header_text_needs_objective_c('#include <CoreFoundation/CFString.h>\n')
	assert !c_header_text_needs_objective_c('#include <X11/Xlib.h>\n')
	assert !c_header_text_needs_objective_c('// @interface CommentOnly\n/* @implementation CommentOnly\nid value = (__bridge id)pointer;\n#import <Cocoa/Cocoa.h>\n*/\nconst char *description = "@interface string only";\nconst char *cast = "__bridge";\n')
	assert !c_header_text_needs_objective_c('#if 0\n@interface Disabled\n@end\nid value = (__bridge id)pointer;\n#import <Cocoa/Cocoa.h>\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 // disabled\n@interface DisabledByLineComment\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 /* disabled */\n@interface DisabledByBlockComment\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0L\n@interface DisabledByLongZero\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0x0ULL\n@interface DisabledByHexZero\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c("#if '\\0'\n@interface DisabledByNullCharacter\n@end\n#endif\n")
	assert c_header_text_needs_objective_c("#if 'A'\n@interface EnabledByCharacter\n@end\n#endif\n")
	assert !c_header_text_needs_objective_c("#if 'a' - 'a'\n@interface DisabledByCharacterArithmetic\n@end\n#endif\n")
	assert !c_header_text_needs_objective_c('#if 0 == 1\nstatic inline id disabled(id obj) { return [obj description]; }\n#endif\n')
	assert c_header_text_needs_objective_c('#if 2 >= 1\nstatic inline id enabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 1 - 1\nstatic inline id disabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 2 * 3 - 6\nstatic inline id disabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 ? 1 : 0\n@interface DisabledByConditional\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 1 || 0 ? 0 : 0\n@interface DisabledByConditionalPrecedence\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#if 0 ? 0 : 1\n@interface EnabledByConditional\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#if (5 - 1) / 2\nstatic inline id enabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#define ONE 1\n#if ONE - 1\n@class DisabledByMacroArithmetic;\n#endif\n')
	assert !c_header_text_needs_objective_c('#define FOO 1\n#if defined(FOO) - 1\n@class DisabledByDefinedArithmetic;\n#endif\n')
	assert c_header_text_needs_objective_c('#if 2 + 2 == 4\n@class EnabledByArithmeticComparison;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (1 & 0)\n@class DisabledByBitwiseAnd;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (0 | 0)\n@class DisabledByBitwiseOr;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (1 ^ 1)\n@class DisabledByBitwiseXor;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (1 << 3) - 8\n@class DisabledByLeftShift;\n#endif\n')
	assert c_header_text_needs_objective_c('#if (8 >> 2) == 2\n@class EnabledByRightShift;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 & 1 == 0\n@class DisabledByBitwiseComparisonPrecedence;\n#endif\n')
	assert c_header_text_needs_objective_c('#if 1 // enabled\n@interface EnabledByLineComment\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#if 1UL\n@interface EnabledByLongOne\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#ifdef __OBJC__\n@implementation Disabled\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if defined(__OBJC__) && __has_feature(objc_arc)\n@implementation Disabled\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if __has_feature(objc_arc) && defined(__OBJC__)\n@implementation Disabled\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if defined(FEATURE) && \\\ndefined(__OBJC__)\n@interface DisabledByContinuedGuard\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c_for_target('#if FEATURE\n@interface Disabled\n@end\n#endif\n', [
		'-DFEATURE=0',
	], false, pref.host_target())
	assert c_header_text_needs_objective_c_for_target('#if FEATURE\n@interface Enabled\n@end\n#endif\n', [
		'-DFEATURE=1',
	], false, pref.host_target())
	assert !c_header_text_needs_objective_c_for_target('#if FEATURE\n@class DisabledByChainedFlag;\n#endif\n', [
		'-DOFF=0',
		'-DFEATURE=OFF',
	], false, pref.host_target())
	assert !c_header_text_needs_objective_c('#define OFF 0\n#define FEATURE OFF\n#if FEATURE\n@protocol DisabledByChainedDirective;\n#endif\n')
	assert c_header_text_needs_objective_c('#define ON 1\n#define FEATURE ON\n#if FEATURE\n@class EnabledByChainedDirective;\n#endif\n')
	assert c_header_text_needs_objective_c('#define FIRST SECOND\n#define SECOND FIRST\n#if FIRST\n@class ConservativelyEnabledByMacroCycle;\n#endif\n')
	assert !c_header_text_needs_objective_c_for_target('#if !defined(FEATURE)\n@interface Disabled\n@end\n#endif\n', [
		'-DFEATURE(x)=x',
	], false, pref.host_target())
	linux := pref.target_from('linux', 'amd64') or { panic(err) }
	private_platform_header := '#if defined(__APPLE__)\n#define _LIB_MACOS 1\n#elif defined(__linux__)\n#define _LIB_LINUX 1\n#endif\n#if defined(_LIB_MACOS)\n@interface MacOnly\n@end\n#endif\n'
	assert !c_header_text_needs_objective_c_for_target(private_platform_header, []string{}, false, linux)
	assert c_header_text_needs_objective_c_for_target(private_platform_header, [
		'-D_LIB_MACOS=1',
	], false, linux)
	assert c_header_text_needs_objective_c('#if 0\n@interface Disabled\n@end\n#else\n@interface Enabled\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#ifdef COMPILER_MACRO\n@interface PossiblyEnabled\n@end\n#endif\n')
}

fn test_sokol_header_does_not_select_objective_c_on_linux() {
	linux := pref.target_from('linux', 'amd64') or { panic(err) }
	header := os.join_path(@VEXEROOT, 'thirdparty', 'sokol', 'sokol_app.h')
	assert !cache_native_input_path_needs_objective_c(header, [
		'-DSOKOL_GLCORE',
		'-USOKOL_D3D11',
		'-USOKOL_GLES3',
		'-USOKOL_METAL',
		'-USOKOL_VULKAN',
		'-USOKOL_WGPU',
		'-DSOKOL_NO_ENTRY',
	], false, linux)
}

fn test_specialized_generic_abi_name_does_not_classify_array_receivers() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.name_uses_specialized_generic_abi('pick[int]')
	assert !g.name_uses_specialized_generic_abi('cli.[]Flag.get_int')
	assert !g.name_uses_specialized_generic_abi('[]Flag.get_int')
}

// test_scratch_lookup_caches_keep_batch_entries_out_of_the_generator covers the
// crash where a `forward_decls` batch memoized a struct C type into the master's
// cache: both the key and the map node were owned by the batch's scratch arena,
// so the next batch's lookup compared against freed memory.
fn test_scratch_lookup_caches_keep_batch_entries_out_of_the_generator() {
	mut g := FlatGen.new()
	own_unique_struct_ct_cache := g.unique_struct_ct_cache
	own_struct_cname_cache := g.struct_cname_cache
	own_generic_app_cache := g.generic_app_cache
	own_import_type_cache := g.import_type_cache
	saved := g.begin_scratch_lookup_caches()
	assert voidptr(g.unique_struct_ct_cache) != voidptr(own_unique_struct_ct_cache)
	assert voidptr(g.struct_cname_cache) != voidptr(own_struct_cname_cache)
	assert voidptr(g.generic_app_cache) != voidptr(own_generic_app_cache)
	assert voidptr(g.import_type_cache) != voidptr(own_import_type_cache)
	// The frozen entries stay readable through the overlay's base.
	assert voidptr(g.generic_app_cache.base) == voidptr(own_generic_app_cache)
	g.unique_struct_ct_cache.put('Batch', 'main__Batch')
	g.param_types_cache['batch'] = [types.Type(types.void_)]
	g.import_type_cache.unqualified_texts.put('Batch', 'main.Batch')
	mut batch_file := g.import_type_cache.for_file('batch.v')
	batch_file.texts.put('Batch', 'main.Batch')
	batch_file.parsed[flat.source_type_id('Batch')] = types.Type(types.void_)
	g.restore_scratch_lookup_caches(saved)
	assert voidptr(g.unique_struct_ct_cache) == voidptr(own_unique_struct_ct_cache)
	assert voidptr(g.struct_cname_cache) == voidptr(own_struct_cname_cache)
	assert voidptr(g.generic_app_cache) == voidptr(own_generic_app_cache)
	assert voidptr(g.import_type_cache) == voidptr(own_import_type_cache)
	assert g.unique_struct_ct_cache.get('Batch') == none
	assert 'batch' !in g.param_types_cache
	// The per-file child cache the batch created is dropped with it.
	assert g.import_type_cache.unqualified_texts.entries.len == 0
	assert g.import_type_cache.by_file.len == 0
	assert isnil(g.import_type_cache.last)
}

// test_scratch_lookup_caches_leave_disabled_caches_disabled keeps the swap about
// where entries are written, never about whether a generator memoizes at all.
fn test_scratch_lookup_caches_leave_disabled_caches_disabled() {
	mut g := FlatGen.new()
	g.import_type_cache = unsafe { nil }
	assert isnil(g.local_typedef_shadow_facts)
	assert isnil(g.struct_decl_pref_cache)
	saved := g.begin_scratch_lookup_caches()
	assert isnil(g.local_typedef_shadow_facts)
	assert isnil(g.struct_decl_pref_cache)
	assert isnil(g.import_type_cache)
	assert !isnil(g.mut_recv_facts)
	g.restore_scratch_lookup_caches(saved)
	assert isnil(g.local_typedef_shadow_facts)
	assert isnil(g.struct_decl_pref_cache)
	assert isnil(g.import_type_cache)
}

// c_string_literal_decode reads back what a C compiler would make of the escaped
// body produced by c_byte_string_escape, so the tests below can compare bytes
// instead of spelling. Adjacent literals are concatenated, as C does in
// translation phase 6, and every escape is either `\` plus one character or a
// three digit octal value.
fn c_string_literal_decode(escaped string) []u8 {
	mut out := []u8{cap: escaped.len}
	mut i := 0
	for i < escaped.len {
		c := escaped[i]
		if c == `"` {
			// The boundary between two adjacent literals, which also ends the
			// source line. A raw newline can only appear here: one inside the
			// payload is escaped as `\012`.
			assert escaped[i + 1] == `\n`
			assert escaped[i + 2] == `"`
			i += 3
			continue
		}
		if c != `\\` {
			out << c
			i++
			continue
		}
		n := escaped[i + 1]
		if n >= `0` && n <= `7` {
			mut v := 0
			for d in escaped[i + 1..i + 4] {
				assert d >= `0` && d <= `7`
				v = v * 8 + int(d - `0`)
			}
			out << u8(v)
			i += 4
			continue
		}
		out << n
		i += 2
	}
	return out
}

fn test_c_byte_string_escape_keeps_printable_bytes_verbatim() {
	assert c_byte_string_escape('plain ASCII text (no escapes needed)') == 'plain ASCII text (no escapes needed)'
	assert c_byte_string_escape('') == ''
}

fn test_c_byte_string_escape_escapes_only_what_c_would_misread() {
	// A quote would end the literal, a backslash would start an escape, and `??x`
	// would be read back as a trigraph.
	assert c_byte_string_escape('say "hi"') == 'say \\"hi\\"'
	assert c_byte_string_escape('a\\b') == 'a\\\\b'
	assert c_byte_string_escape('what??!') == 'what\\?\\?!'
	// Everything outside printable ASCII becomes a three digit octal escape, which
	// a following digit cannot extend.
	assert c_byte_string_escape('\n') == '\\012'
	assert c_byte_string_escape('\x00') == '\\000'
	assert c_byte_string_escape('\xff') == '\\377'
	assert c_byte_string_escape('\x019') == '\\0019'
}

fn test_c_byte_string_escape_round_trips_every_byte_value() {
	mut raw := []u8{cap: 256}
	for i in 0 .. 256 {
		raw << u8(i)
	}
	source := raw.bytestr()
	assert c_string_literal_decode(c_byte_string_escape(source)) == raw
}

// test_c_byte_string_escape_splits_long_payloads_into_adjacent_literals covers the
// two limits a long `$embed_file` payload would otherwise run into: the maximum
// length of one string literal, and the maximum length of one logical source line.
// MSVC is strict about both, so neither splitting alone is enough.
fn test_c_byte_string_escape_splits_long_payloads_into_adjacent_literals() {
	mut raw := []u8{cap: 40000}
	for i in 0 .. 40000 {
		// A mix of verbatim and escaped bytes, so that splits have to land between
		// escapes rather than at a fixed stride.
		raw << if i % 3 == 0 { u8(200 + i % 40) } else { u8(`a` + i % 26) }
	}
	escaped := c_byte_string_escape(raw.bytestr())
	// Each continuation is both its own literal and its own source line.
	assert escaped.contains('"\n"')
	lines := escaped.split_into_lines()
	assert lines.len > 1
	for line in lines {
		assert line.len <= c_string_literal_chunk_len + 4
		// C requires an implementation to support only 4095 characters in one
		// logical source line, and MSVC stops at 16384.
		assert line.len < 4095
	}
	assert c_string_literal_decode(escaped) == raw
}

// test_embed_payload_needs_blob_switches_at_the_string_literal_limit pins where a
// payload stops being written as a string literal. Adjacent literals join back
// into one, so splitting cannot carry a payload past that maximum; only an array
// object can.
fn test_embed_payload_needs_blob_switches_at_the_string_literal_limit() {
	assert !embed_payload_needs_blob(0)
	assert !embed_payload_needs_blob(c_string_literal_chunk_len)
	assert !embed_payload_needs_blob(c_string_literal_max_total)
	assert embed_payload_needs_blob(c_string_literal_max_total + 1)
	// What C99 5.2.4.1 requires every implementation to accept in a literal after
	// concatenation. Portable output cannot assume more than that.
	assert c_string_literal_max_total == 4095
}

// test_embed_blob_split_keeps_every_object_within_the_c_limit covers the sizes a
// split payload is written at. C only requires an implementation to accept 65535
// bytes in one object, and the tables listing the pieces are objects too.
fn test_embed_blob_split_keeps_every_object_within_the_c_limit() {
	assert embed_blob_part_count(0) == 0
	assert embed_blob_part_count(1) == 1
	assert embed_blob_part_count(c_max_object_size) == 1
	assert embed_blob_part_count(c_max_object_size + 1) == 2
	// One entry is a pointer and an int, 16 bytes where that pair is widest.
	assert embed_chunk_table_entries * 16 <= c_max_object_size
	// A table spends its last entry on the terminator, or on the link onwards.
	per_table := embed_chunk_table_entries - 1
	assert embed_blob_table_count(1) == 1
	assert embed_blob_table_count(per_table) == 1
	assert embed_blob_table_count(per_table + 1) == 2
	assert embed_blob_table_count(per_table * 2) == 2
	assert embed_blob_table_count(per_table * 2 + 1) == 3
	// Linking them is what leaves the representation with no size of its own that
	// it cannot describe.
	parts := embed_blob_part_count(4 * 1024 * 1024 * 1024 - 1)
	assert embed_blob_table_count(parts) * per_table >= parts
}

fn test_path_linked_libraries_keep_headerless_c_prototypes() {
	source := '/project/bindings.c.v'
	for flag in ['@DIR/libext.a', '/opt/libext.so', '/opt/libext.dylib', 'C:/lib/ext.lib',
		'"/opt/library dir/libext.a"', 'linux @DIR/libext.a -lm'] {
		mut g := FlatGen.new()
		g.note_c_flag_directive('bindings', source, flag)
		assert g.should_emit_c_extern_decl_from_file('my_ext_fn', source, 'bindings'), flag
		// A sibling binding file can share the module's library linkage.
		assert g.should_emit_c_extern_decl_from_file('my_ext_fn', '/project/other.c.v',
			'bindings'), flag
		g.note_c_include_directive('bindings', '/project/header.c.v')
		assert !g.should_emit_c_extern_decl_from_file('my_ext_fn', source, 'bindings'), flag
		g.note_c_include_directive('bindings', source)
		assert !g.should_emit_c_extern_decl_from_file('my_ext_fn', source, 'bindings'), flag
	}
}

fn test_library_path_detection_ignores_non_linker_operands() {
	for flag in ['-I/opt/headers.a', '-I "/opt/headers.a"', '-L /opt/libraries.a',
		'-include /opt/header.so', '-isystem /opt/headers.lib', '-DNAME=libext.a',
		'-I /opt/include ## /opt/libext.a'] {
		assert !c_flag_links_c_library(flag), flag
	}
}

fn test_native_source_paths_keep_headerless_c_prototypes() {
	for flag in ['helper.c', 'helper.cpp', 'helper.cc', 'helper.m', 'helper.mm', 'helper.o',
		'helper.obj', '"/project/native sources/helper.m"'] {
		mut g := FlatGen.new()
		g.note_c_flag_directive('bindings', '/project/bindings.c.v', flag)
		assert g.should_emit_c_extern_decl_from_file('native_api', '/project/bindings.c.v',
			'bindings'), flag
	}
	for flag in ['-I /project/include.o', '-L /project/library.o', '-include api.c',
		'-isystem /project/include.m', '-DNAME=helper.cpp', 'helper.cpp.h',
		'-I /project/include ## ignored.o'] {
		assert !c_flag_links_c_source(flag), flag
	}
}

fn test_forced_native_source_language_keeps_headerless_prototypes() {
	root := os.join_path(os.vtmp_dir(), 'forced_native_source_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	input := os.join_path(root, 'helper')
	os.write_file(input, 'int native_api(void) { return 73; }\n') or { panic(err) }
	quoted_input := os.quoted_path(input)
	for flag in ['-x c ${quoted_input} -x none', '-xc ${quoted_input} -xnone'] {
		mut g := FlatGen.new()
		g.note_c_flag_directive('bindings', '/project/bindings.c.v', flag)
		assert g.should_emit_c_extern_decl_from_file('native_api', '/project/bindings.c.v',
			'bindings'), flag
		g.note_c_include_directive('bindings', '/project/bindings.c.v')
		assert !g.should_emit_c_extern_decl_from_file('native_api', '/project/bindings.c.v',
			'bindings'), flag
	}
	for flag in ['${quoted_input}', '-x c -x none ${quoted_input}',
		'-x c -include ${quoted_input} -x none', '-x c -D ${quoted_input} -x none',
		'-x c -I ${quoted_input} -x none'] {
		assert !c_flag_links_c_source(flag), flag
	}
}

fn test_force_loaded_archives_keep_headerless_prototypes() {
	for flag in ['-Wl,-force_load,/project/libnative.a',
		'"-Wl,-force_load,/project/native files/libnative.a"'] {
		mut g := FlatGen.new()
		g.note_c_flag_directive('bindings', '/project/bindings.c.v', flag)
		assert g.should_emit_c_extern_decl_from_file('native_api', '/project/bindings.c.v',
			'bindings'), flag
		g.note_c_include_directive('bindings', '/project/header.c.v')
		assert !g.should_emit_c_extern_decl_from_file('native_api', '/project/bindings.c.v',
			'bindings'), flag
	}
	for flag in ['-Wl,-rpath,/project/libnative.a', '-Wl,-force_load',
		'-D -Wl,-force_load,/project/libnative.a', '-include /project/libnative.a'] {
		assert !c_flag_links_c_library(flag), flag
	}
}

fn test_linker_object_inputs_keep_headerless_prototypes() {
	for flag in ['-Wl,/project/helper.o', '-Wl,/project/helper.obj',
		'"-Wl,/project/native files/helper.o"', '-Wl,-rpath,/project/lib,/project/helper.o'] {
		mut g := FlatGen.new()
		g.note_c_flag_directive('bindings', '/project/bindings.c.v', flag)
		assert g.should_emit_c_extern_decl_from_file('native_api', '/project/bindings.c.v',
			'bindings'), flag
		g.note_c_include_directive('bindings', '/project/bindings.c.v')
		assert !g.should_emit_c_extern_decl_from_file('native_api', '/project/bindings.c.v',
			'bindings'), flag
	}
	for flag in ['-Wl,-rpath,/project/helper.o', '-Wl,-o,/project/helper.o',
		'-Wl,-Map,/project/helper.o', '-Wl,-T,/project/helper.o', '-Wl,-install_name,/project/helper.o',
		'-D -Wl,/project/helper.o'] {
		assert !c_flag_links_c_source(flag), flag
	}
}

fn test_fixed_storage_candidate_short_name_uses_last_dot_before_c_separator() {
	g := FlatGen{}
	cases := {
		'':                  ''
		'plain':             'plain'
		'mod.name':          'name'
		'mod__name':         'name'
		'mod__deep__name':   'name'
		'mod__x.name__tail': 'name__tail'
		'mod.name__tail':    'name__tail'
		'a___b':             'b'
		'a.':                ''
		'a__':               ''
		'a_b':               'a_b'
	}
	for name, expected in cases {
		assert g.fixed_storage_candidate_short_name(name) == expected
	}
}

fn test_fixed_storage_candidate_ident_cache_checks_pointer_length_and_misses() {
	g := FlatGen{}
	idents := {
		'fixed': true
	}
	shorts := {
		'item': true
	}
	ident_filter := fixed_storage_candidate_name_filter(idents)
	short_filter := fixed_storage_candidate_name_filter(shorts)
	mut cache := []FixedStorageCandidateNameCacheEntry{len: 4096}
	long_name := 'fixed_extra'
	short_name := unsafe { long_name.substr_unsafe(0, 5) }
	assert voidptr(long_name.str) == voidptr(short_name.str)
	for _ in 0 .. 3 {
		assert !g.fixed_storage_candidate_ident_matches(long_name, idents, shorts, &ident_filter, &short_filter, mut cache)
		assert g.fixed_storage_candidate_ident_matches(short_name, idents, shorts, &ident_filter, &short_filter, mut cache)
	}
	for name, expected in {
		'':                    false
		'absent':              false
		'fixed':               true
		'item':                false
		'pkg.item':            true
		'pkg__item':           true
		'pkg.item__tail':      false
		'pkg__ignored.item':   true
		'pkg__ignored.item__': false
	} {
		for _ in 0 .. 2 {
			assert g.fixed_storage_candidate_ident_matches(name, idents, shorts, &ident_filter, &short_filter, mut cache) == expected, name
		}
		copy := name.clone()
		assert g.fixed_storage_candidate_ident_matches(copy, idents, shorts, &ident_filter, &short_filter, mut cache) == expected, name
	}
	// A conflicting entry with the same length must miss on its byte pointer.
	for i in 0 .. cache.len {
		cache[i] = FixedStorageCandidateNameCacheEntry{
			data:    voidptr(c'other')
			len:     5
			matches: false
		}
	}
	assert g.fixed_storage_candidate_ident_matches('fixed', idents, shorts, &ident_filter, &short_filter, mut cache)
}

fn test_fixed_storage_candidate_cache_is_reset_between_node_scans() {
	mut a := flat.FlatAst.new()
	id := a.add_node(flat.Node{
		kind:  .ident
		value: 'fixed'
	})
	g := FlatGen{ a: &a }
	for expected in [true, false, true] {
		idents := if expected {
			{
				'fixed': true
			}
		} else {
			map[string]bool{}
		}
		shorts := map[string]bool{}
		ident_filter := fixed_storage_candidate_name_filter(idents)
		short_filter := fixed_storage_candidate_name_filter(shorts)
		mut scan := FixedStorageNodeScanArgs{
			g:                      &g
			start:                  int(id)
			end:                    int(id) + 1
			fixed_candidate_idents: idents
			fixed_candidate_shorts: shorts
			ident_filter:           &ident_filter
			short_filter:           &short_filter
		}
		g.scan_fixed_storage_node_range(mut scan)
		assert (scan.ref_items.len == 1) == expected
		if expected {
			assert scan.ref_items[0].id == id
		}
	}
}

fn test_cname_caches_unchanged_names_in_private_overlay() {
	mut base := &CNameCache{}
	mut g := FlatGen{ c_name_cache: base }
	for name in ['ordinary_identifier', 'pkg__already_sanitized'] {
		assert g.cname(name) == name
		assert base.entries[name]! == name
	}
	g.c_name_cache = &CNameCache{ base: base }
	for name in ['ordinary_identifier', 'pkg__already_sanitized'] {
		assert g.cname(name.clone()) == name
		assert name !in g.c_name_cache.entries
	}
	assert g.cname('other_identifier') == 'other_identifier'
	assert 'other_identifier' !in base.entries
	assert g.c_name_cache.entries['other_identifier']! == 'other_identifier'
}

fn test_local_c_function_shadow_tracks_current_calls() {
	mut g := FlatGen.new()
	assert !g.local_name_shadows_c_function('pkg.call')
	g.cur_c_fn_calls['pkg__call'] = true
	g.cur_c_fn_calls['v_malloc'] = true
	assert g.local_name_shadows_c_function('pkg.call')
	assert g.local_name_shadows_c_function('malloc')
	assert !g.local_name_shadows_c_function('other')
	g.cur_c_fn_calls.clear()
	assert !g.local_name_shadows_c_function('pkg.call')
	assert !g.local_name_shadows_c_function('malloc')
}

fn test_ident_function_value_keeps_local_type_precedence_and_void_fallback() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'handler')
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	tc.fn_param_types['handler'] = []types.Type{}
	tc.fn_ret_types['handler'] = types.Type(types.int_)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	mut scope := types.new_scope(unsafe { nil })
	tc.cur_scope = scope
	owner := scope.insert_with_owner('handler', types.Type(types.string_))
	assert g.ident_fn_value_c_name(id, a.nodes[int(id)], owner) == none
	// A missing owner must retain the exact legacy lookup, including locals.
	assert g.ident_fn_value_c_name(id, a.nodes[int(id)], types.ScopeBindingOwner{}) == none
	void_owner := scope.insert_with_owner('handler', types.Type(types.Void{}))
	assert g.ident_fn_value_c_name(id, a.nodes[int(id)], void_owner)? == 'handler'
	scope.reset(unsafe { nil })
	scope.insert('handler', types.Type(types.string_))
	// A stale owner must read the replacement rather than its former Void type.
	assert g.ident_fn_value_c_name(id, a.nodes[int(id)], void_owner) == none
	scope.reset(unsafe { nil })
	assert g.ident_fn_value_c_name(id, a.nodes[int(id)], types.ScopeBindingOwner{})? == 'handler'
}

fn test_fixed_storage_scan_items_borrows_serial_lists_and_copies_parallel_lists() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	filter := FixedStorageCandidateNameFilter{}
	mut scans := []FixedStorageNodeScanArgs{}
	for i in 0 .. 2 {
		scans << FixedStorageNodeScanArgs{
			g:                &g
			ident_filter:     &filter
			short_filter:     &filter
			address_items:    [FixedStorageConstRefItem{ id: flat.NodeId(i * 10), at: i * 10 + 1 }]
			ref_items:        [FixedStorageConstRefItem{ id: flat.NodeId(i * 10 + 2), at: i * 10 + 3 }]
			call_base_items:  [FixedStorageConstRefItem{ id: flat.NodeId(i * 10 + 4), at: i * 10 + 5 }]
			index_base_items: [FixedStorageConstRefItem{ id: flat.NodeId(i * 10 + 6), at: i * 10 + 7 }]
		}
	}
	serial_a, serial_r, serial_c, serial_i := fixed_storage_scan_items(scans[..1])
	assert serial_a == scans[0].address_items
	assert serial_r == scans[0].ref_items
	assert serial_c == scans[0].call_base_items
	assert serial_i == scans[0].index_base_items
	assert serial_a.data == scans[0].address_items.data
	assert serial_r.data == scans[0].ref_items.data
	assert serial_c.data == scans[0].call_base_items.data
	assert serial_i.data == scans[0].index_base_items.data
	parallel_a, parallel_r, parallel_c, parallel_i := fixed_storage_scan_items(scans)
	assert parallel_a == [scans[0].address_items[0], scans[1].address_items[0]]
	assert parallel_r == [scans[0].ref_items[0], scans[1].ref_items[0]]
	assert parallel_c == [scans[0].call_base_items[0], scans[1].call_base_items[0]]
	assert parallel_i == [scans[0].index_base_items[0], scans[1].index_base_items[0]]
	for scan in scans {
		assert parallel_a.data != scan.address_items.data
		assert parallel_r.data != scan.ref_items.data
		assert parallel_c.data != scan.call_base_items.data
		assert parallel_i.data != scan.index_base_items.data
	}
	// Destroy the input buffers as the caller destroys parallel worker arenas.
	for mut scan in scans {
		scan.address_items[0] = FixedStorageConstRefItem{}
		scan.ref_items[0] = FixedStorageConstRefItem{}
		scan.call_base_items[0] = FixedStorageConstRefItem{}
		scan.index_base_items[0] = FixedStorageConstRefItem{}
	}
	assert parallel_a[1].id == flat.NodeId(10)
	assert parallel_r[1].id == flat.NodeId(12)
	assert parallel_c[1].id == flat.NodeId(14)
	assert parallel_i[1].id == flat.NodeId(16)
	empty_a, empty_r, empty_c, empty_i := fixed_storage_scan_items([]FixedStorageNodeScanArgs{})
	assert empty_a.len == 0 && empty_r.len == 0 && empty_c.len == 0 && empty_i.len == 0
}

fn test_fixed_array_len_operator_classes_cover_every_byte_and_prefix_position() {
	assert !fixed_array_len_has_operator('')
	for value in 0 .. 256 {
		ch := u8(value)
		byte_text := [ch].bytestr()
		everywhere := ch in [`+`, `*`, `/`, `%`, `|`, `^`, `<`, `>`]
		assert fixed_array_len_has_operator(byte_text) == everywhere, value.str()
		assert fixed_array_len_has_operator(byte_text + 'a') == everywhere, value.str()
		assert fixed_array_len_has_operator('a' + byte_text) == (everywhere || ch in [
			`-`,
			`&`,
		]), value.str()
	}
	for text, expected in {
		'&length':      false
		'-1':           false
		'-length':      false
		'&length-name': true
		'-length&mask': true
		'pkg.length':   false
		'αβγ':          false
		'α+β':          true
		'&αβγ':         false
		'α-β':          true
		'-αβγ':         false
		'α&β':          true
	} {
		assert fixed_array_len_has_operator(text) == expected, text
	}
}

fn test_referenced_c_struct_type_operands_preserve_ast_and_checker_context() {
	mut a := flat.FlatAst.new()
	for node in [
		flat.Node{ kind: .ident, value: 'unrelated' },
		flat.Node{ kind: .sizeof_expr, value: 'C.SizeOnly' },
		flat.Node{ kind: .offsetof_expr, value: 'C.OffsetOnly' },
		flat.Node{ kind: .sizeof_expr, value: 'C.Unused', children_count: 1 },
	] {
		a.add_node(node)
	}
	mut tc := types.TypeChecker.new(&a)
	tc.cur_file = 'original.v'
	tc.cur_module = 'original'
	tc.file_modules['body.v'] = 'body'
	for name in ['C.SizeOnly', 'C.OffsetOnly', 'C.Nested', 'C.Unused'] {
		tc.structs[name] = []types.StructField{}
	}
	tc.structs['C.SizeOnly'] = [types.StructField{
		name: 'nested'
		typ:  types.Struct{ name_id: flat.source_type_id('C.Nested') }
	}]
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.cur_fn_source_file = 'body.v'
	before := a.nodes.clone()
	for _ in 0 .. 2 {
		seen := g.referenced_c_structs()
		for name in ['C.SizeOnly', 'C.OffsetOnly', 'C.Nested'] {
			assert seen[flat.source_type_id(name)], name
		}
		assert !seen[flat.source_type_id('C.Unused')]
		assert tc.cur_file == 'original.v'
		assert tc.cur_module == 'original'
		assert a.nodes == before
	}
}

fn ordinary_local_ident_output(name string, mode string, fast bool) string {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, name)
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut file_scope := types.new_scope(unsafe { nil })
	mut scope := types.new_scope(file_scope)
	tc.file_scope = file_scope
	tc.cur_scope = scope
	typ := if mode == 'void' { types.Type(types.Void{}) } else { types.Type(types.int_) }
	owner := scope.insert_with_owner(name, typ)
	other_owner := scope.insert_with_owner('other_local', types.Type(types.int_))
	if mode == 'void' {
		tc.fn_param_types[name] = []types.Type{}
		tc.fn_ret_types[name] = types.Type(types.int_)
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	match mode {
		'shared' { g.local_shared_storage_by_owner[owner.storage_key()] = true }
		'deref' { g.local_implicit_deref_by_owner[owner.storage_key()] = true }
		'shadow' { g.shadowed_global_locals[owner.storage_key()] = true }
		'cfn' { g.cur_c_fn_calls[g.cname(name)] = true }
		'other_shared' { g.local_shared_storage_by_owner[other_owner.storage_key()] = true }
		'other_deref' { g.local_implicit_deref_by_owner[other_owner.storage_key()] = true }
		'other_shadow' { g.shadowed_global_locals[other_owner.storage_key()] = true }
		'other_cfn' { g.cur_c_fn_calls['other_c_call'] = true }
		'suffix' { tc.c_globals['C.${name}'] = types.Type(types.int_) }
		'typedef' { g.inlined_c_typedef_names[g.cname(name)] = true }
		'global' { g.global_modules[name] = 'other' }
		else {}
	}
	g.precompute_local_global_suffix_names()
	g.precompute_c_typedef_shadow_names()
	if fast { g.precompute_local_name_exceptions() }
	g.gen_nonliteral_expr(id, a.node(id))
	return g.sb.str()
}

fn test_ordinary_local_ident_keeps_storage_rename_and_placeholder_output() {
	for name in ['value', 'abs', 'array', 'argv', 'int_str', 'pkg.value', '__v3_internal_symbol_local'] {
		for mode in ['', 'shared', 'deref', 'shadow', 'cfn', 'other_shared', 'other_deref',
			'other_shadow', 'other_cfn', 'suffix', 'typedef', 'global', 'void'] {
			legacy := ordinary_local_ident_output(name, mode, false)
			assert ordinary_local_ident_output(name, mode, true) == legacy, '${name} / ${mode}: ${legacy}'
		}
	}
}

fn test_ordinary_local_ident_ignores_outer_binding_storage_and_unrelated_c_calls() {
	mut a := flat.FlatAst.new()
	id := a.add_val(.ident, 'value')
	mut tc := types.TypeChecker.new(&a)
	mut outer_scope := types.new_scope(unsafe { nil })
	outer := outer_scope.insert_with_owner('value', types.Type(types.int_))
	mut inner_scope := types.new_scope(outer_scope)
	inner := inner_scope.insert_with_owner('value', types.Type(types.int_))
	tc.cur_scope = inner_scope
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.local_shared_storage_by_owner[outer.storage_key()] = true
	g.local_implicit_deref_by_owner[outer.storage_key()] = true
	g.shadowed_global_locals[outer.storage_key()] = true
	g.cur_c_fn_calls['other_c_call'] = true
	g.precompute_local_global_suffix_names()
	g.precompute_c_typedef_shadow_names()
	g.precompute_local_name_exceptions()
	assert g.ordinary_local_cname('value', inner)? == 'value'
	assert g.ordinary_local_cname('value', outer) == none
	g.gen_nonliteral_expr(id, a.node(id))
	assert g.sb.str() == 'value'
	// A recorded C call reserves its emitted name even when the map value is false.
	g.cur_c_fn_calls['value'] = false
	assert g.ordinary_local_cname('value', inner) == none
	g.cur_c_fn_calls.delete('value')
	g.shadowed_global_locals[inner.storage_key()] = false
	assert g.ordinary_local_cname('value', inner) == none
	g.shadowed_global_locals.delete(inner.storage_key())
	g.local_shared_storage_by_owner[inner.storage_key()] = false
	g.local_implicit_deref_by_owner[inner.storage_key()] = false
	assert g.ordinary_local_cname('value', inner)? == 'value'
}

fn test_local_name_exceptions_preserve_raw_and_emitted_names_and_rebuilds() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut scope := types.new_scope(unsafe { nil })
	owner := scope.insert_with_owner('value', types.Type(types.int_))
	abs_owner := scope.insert_with_owner('abs', types.Type(types.int_))
	void_owner := scope.insert_with_owner('placeholder', types.Type(types.Void{}))
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.ordinary_local_cname('value', owner) == none
	g.precompute_local_global_suffix_names()
	g.precompute_c_typedef_shadow_names()
	g.precompute_local_name_exceptions()
	assert g.ordinary_local_cname('value', owner)? == 'value'
	assert g.ordinary_local_cname('abs', abs_owner)? == 'v_abs'
	assert g.ordinary_local_cname('placeholder', void_owner) == none
	g.inlined_c_typedef_names['v_abs'] = true
	g.precompute_c_typedef_shadow_names()
	g.precompute_local_name_exceptions()
	assert g.ordinary_local_cname('abs', abs_owner) == none
	g.global_modules['value'] = 'other'
	g.precompute_local_name_exceptions()
	assert g.ordinary_local_cname('value', owner) == none
	g.local_global_suffix_names_ready = false
	g.precompute_local_name_exceptions()
	assert isnil(g.local_name_exceptions)
	scope.reset(unsafe { nil })
	assert g.ordinary_local_cname('value', owner) == none
}

fn test_shared_wrapper_owner_lookup_preserves_local_and_global_bindings() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert !g.local_ident_is_shared_wrapper('')
	assert !g.local_ident_is_shared_wrapper('missing')
	g.push_scope()
	local := tc.cur_scope.insert_with_owner('value', types.Type(types.int_))
	assert !g.local_ident_is_shared_wrapper('value')
	g.declare_local_c_type(local, 'int')
	assert !g.local_ident_is_shared_wrapper('value')
	g.declare_local_c_type(local, '__shared__int*')
	assert g.local_ident_is_shared_wrapper('value')
	g.push_scope()
	tc.cur_scope.insert('value', types.Type(types.int_))
	assert !g.local_ident_is_shared_wrapper('value')
	g.pop_scope()
	assert g.local_ident_is_shared_wrapper('value')
	g.declare_local_c_type(local, '')
	assert !g.local_ident_is_shared_wrapper('value')
	g.declare_local_shared_storage(local, true)
	assert g.local_ident_is_shared_wrapper('value')
	g.declare_local_shared_storage(local, false)
	assert !g.local_ident_is_shared_wrapper('value')
	g.pop_scope()
	tc.cur_scope.insert('global_value', types.Type(types.int_))
	g.declare_global_raw_type('global_value', flat.source_type_id('shared int'))
	assert g.local_ident_is_shared_wrapper('global_value')
	g.push_scope()
	tc.cur_scope.insert('global_value', types.Type(types.int_))
	assert !g.local_ident_is_shared_wrapper('global_value')
}

fn test_method_value_builtin_annotation_filter_preserves_unresolved_and_function_types() {
	for type_id in [flat.source_type_string, flat.source_type_char, flat.source_type_rune,
		flat.source_type_isize, flat.source_type_usize, flat.source_type_voidptr,
		flat.source_type_byteptr, flat.source_type_charptr, flat.source_type_bool,
		flat.source_type_int, flat.source_type_i8, flat.source_type_i16, flat.source_type_i32,
		flat.source_type_i64, flat.source_type_u8, flat.source_type_u16, flat.source_type_u32,
		flat.source_type_u64, flat.source_type_f32, flat.source_type_f64] {
		assert selector_has_builtin_value_type(type_id)
	}
	for type_id in [u32(0), flat.source_type_void, flat.source_type_unknown, flat.source_type_generic,
		flat.source_type_nil, flat.source_type_none, flat.source_type_id('fn () int'),
		flat.source_type_id('&fn () int'), flat.source_type_id('Callback'),
		flat.source_type_id('pkg.Callback'), flat.source_type_id('?int'), flat.source_type_id('&int')] {
		assert !selector_has_builtin_value_type(type_id)
	}
}

fn test_shared_storage_absence_tracks_globals_and_shadowed_bindings() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.cur_module = 'main'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert !g.global_shared_storage_present
	assert !g.local_storage_is_shared('missing')
	g.push_scope()
	local := tc.cur_scope.insert_with_owner('value', types.Type(types.int_))
	assert !g.local_storage_is_shared('value')
	g.declare_local_shared_storage(local, true)
	assert g.local_storage_is_shared('value')
	g.declare_local_shared_storage(local, false)
	assert !g.local_storage_is_shared('value')
	g.pop_scope()
	tc.cur_scope.insert('global_value', types.Type(types.int_))
	g.declare_global_raw_type('global_value', flat.source_type_int)
	assert !g.global_shared_storage_present
	assert !g.local_storage_is_shared('global_value')
	g.declare_global_raw_type('global_value', flat.source_type_id(' shared int '))
	assert g.global_shared_storage_present
	assert g.local_storage_is_shared('global_value')
	g.push_scope()
	tc.cur_scope.insert('global_value', types.Type(types.int_))
	assert !g.local_storage_is_shared('global_value')
	g.pop_scope()
	assert g.local_storage_is_shared('global_value')
	g.declare_global_raw_type('global_value', flat.source_type_int)
	assert !g.local_storage_is_shared('global_value')
	// A conservative positive survives replacement until the program is reset.
	assert g.global_shared_storage_present
	tc.cur_module = 'dep'
	g.declare_global_raw_type('dep.global_value', flat.source_type_id('shared int'))
	assert g.local_storage_is_shared('global_value')
}

fn test_storage_pointer_argument_names_preserve_candidate_positions_and_indices() {
	mut g := FlatGen.new()
	for name in ['array_push', 'array__push', 'dep__array_push', 'dep__array__push', 'dep.array_push',
		'dep.array__push'] {
		for index in -1 .. 5 {
			expected := index == 1
			assert g.call_arg_is_storage_pointer_arg(index, name, '', '') == expected
			assert g.call_arg_is_storage_pointer_arg(index, '', name, '') == expected
			assert g.call_arg_is_storage_pointer_arg(index, '', '', name) == expected
		}
	}
	for name in ['map__set', 'map__get', 'map__get_and_set', 'dep__map__set', 'dep__map__get',
		'dep__map__get_and_set', 'dep.map__set', 'dep.map__get', 'dep.map__get_and_set'] {
		for index in -1 .. 5 {
			expected := index == 2
			assert g.call_arg_is_storage_pointer_arg(index, name, '', '') == expected
			assert g.call_arg_is_storage_pointer_arg(index, '', name, '') == expected
			assert g.call_arg_is_storage_pointer_arg(index, '', '', name) == expected
		}
	}
	for name in ['', 'array_push_more', 'notarray_push', 'map__get_more', 'notmap__get'] {
		for index in -1 .. 5 {
			assert !g.call_arg_is_storage_pointer_arg(index, name, name, name)
		}
	}
}

fn test_fixed_array_data_argument_rejects_other_names_and_positions_without_ast_access() {
	mut g := FlatGen.new()
	call := flat.Node{ children_count: 8 }
	for name in ['new_array_from_c_array', 'array__new_array_from_c_array'] {
		for index in -1 .. 6 {
			if index == 3 { continue }
			assert !g.gen_new_array_fixed_data_arg(call, 1, index, 0, name, '')
			assert !g.gen_new_array_fixed_data_arg(call, 1, index, 0, '', name)
		}
	}
	for name in ['', 'dep.new_array_from_c_array', 'new_array_from_c_array_more'] {
		assert !g.gen_new_array_fixed_data_arg(call, 1, 3, 0, name, name)
	}
}

fn test_call_args_reuses_function_value_parameter_count_including_empty_signature() {
	for count in 0 .. 3 {
		mut a := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		callee := a.add_val(.ident, 'callback')
		first := a.add_val(.int_literal, '7')
		second := a.add_val(.int_literal, '8')
		tc.register_synth_type(first, types.Type(types.int_))
		tc.register_synth_type(second, types.Type(types.int_))
		start := a.begin_children()
		a.add_child(callee)
		a.add_child(first)
		a.add_child(second)
		call := flat.Node{
			kind:           .call
			children_start: start
			children_count: 3
		}
		mut params := []types.Type{}
		for _ in 0 .. count {
			params << types.Type(types.int_)
		}
		callback_type := types.Type(tc.fn_type(params, &types.Type(types.int_), []bool{}))
		g.cur_param_types['callback'] = tc.intern_type_reference(callback_type)
		// A declaration with the same spelling must not replace the lexical callback.
		tc.fn_param_types['callback'] = [types.Type(types.string_)]
		cached_name := g.call_target_name(callee)
		cached_params := g.fn_value_call_param_types(callee)
		g.gen_call_args('callback', call, 1)
		fresh := g.sb.str()
		g.sb.clear()
		g.gen_call_args_for_callee('callback', call, 1, cached_name, cached_params)
		reused := g.sb.str()
		assert reused == fresh
		assert reused == match count {
			0 { '' }
			1 { '7' }
			else { '7, 8' }
		}
	}
}

fn test_decl_pointer_param_abi_merge_preserves_name_fallback_after_semantic_equality() {
	array_param := types.Type(types.Array{
		elem_type: &types.Type(types.Pointer{ base_type: &types.Type(types.int_) })
	})
	array_decl := types.Type(types.Array{
		elem_type: &types.Type(types.Pointer{ base_type: &types.Type(types.int_) })
	})
	enum_param := types.Type(types.Enum{ name_id: flat.source_type_id('AbiEnum') })
	enum_decl := types.Type(types.Enum{
		name_id: flat.source_type_id('AbiEnum')
		is_flag: true
	})
	assert array_param == array_decl
	assert enum_param != enum_decl
	assert enum_param.name() == enum_decl.name()
	pointer_param := types.Type(types.Pointer{ base_type: &types.Type(types.int_) })
	pointer_decl := types.Type(types.Pointer{ base_type: &types.Type(types.string_) })
	// Repeat each case after the first replacement to cover the lazy merge's tail.
	params := [array_param, enum_param, types.Type(types.int_), types.Type(types.int_), pointer_param,
		array_param, enum_param, types.Type(types.int_), types.Type(types.int_), pointer_param]
	decl_types := [array_decl, enum_decl, types.Type(types.Unknown{}), types.Type(types.Void{}),
		pointer_decl, array_decl, enum_decl, types.Type(types.Unknown{}), types.Type(types.Void{}),
		pointer_decl]
	for lazy in [false, true] {
		g := FlatGen{ lazy_param_abi_merge: lazy }
		merged := g.merge_decl_pointer_param_abi(params, decl_types)
		for i in 0 .. merged.len {
			assert merged[i] == if i == 4 || i == 9 { pointer_decl } else { params[i] }
		}
		assert params[4] == pointer_param
		assert params[9] == pointer_param
		assert g.merge_decl_pointer_param_abi([array_param, enum_param], [array_decl, enum_decl]) == [
			array_param,
			enum_param,
		]
		assert g.merge_decl_pointer_param_abi([]types.Type{}, [pointer_decl]).len == 0
		assert g.merge_decl_pointer_param_abi([pointer_param], []types.Type{}) == [pointer_param]
		assert g.merge_decl_pointer_param_abi([pointer_param], [pointer_decl, array_decl]) == [
			pointer_decl,
			array_decl,
		]
		assert g.merge_decl_pointer_param_abi([pointer_param, array_param], [pointer_decl]) == [
			pointer_param,
			array_param,
		]
	}
}

fn test_sum_type_index_preserves_import_context_and_builtin_variants() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.qualify_name_cache = &types.QualifyNameCache{}
	tc.cur_module = 'main'
	tc.structs['left.Item'] = []types.StructField{}
	tc.structs['right.Item'] = []types.StructField{}
	tc.sum_types.set('left.Value', ['left.Item', 'int'])
	tc.sum_types.set('right.Value', ['int', 'right.Item'])
	tc.cur_file = 'left.v'
	tc.register_file_import('dep', 'left')
	tc.cur_file = 'right.v'
	tc.register_file_import('dep', 'right')
	mut g := FlatGen.new()
	g.tc = &tc
	sum_id := flat.source_type_id('dep.Value')
	variant_id := flat.source_type_id('dep.Item')
	for _ in 0 .. 3 {
		tc.cur_file = 'left.v'
		assert g.sum_type_index(sum_id, variant_id) == 1
		assert g.sum_type_index(sum_id, flat.source_type_int) == 2
		tc.cur_file = 'right.v'
		assert g.sum_type_index(sum_id, variant_id) == 2
		assert g.sum_type_index(sum_id, flat.source_type_int) == 1
	}
}
