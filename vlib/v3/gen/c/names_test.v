module c

import os
import v3.flat
import v3.pref
import v3.types

// test_c_name_sanitize_operator_overloads validates this v3 regression case.
fn test_c_name_sanitize_operator_overloads() {
	assert c_name('Point.<') == 'Point__lt'
	assert c_name('Point.<=') == 'Point__le'
	assert c_name('Point.>') == 'Point__gt'
	assert c_name('Point.>=') == 'Point__ge'
	assert c_name('Point.[]') == 'Point__op_index'
	assert c_name('Point.[]=') == 'Point__op_index_set'
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
	for name in ['run', 'int', 'send', 'malloc', 'int_str', 'exit', '_str_42', '_str_value',
		'main.run', 'foo.Bar.method', 'C.printf', 'C.SSL_CTX.str', 'Point.<=', 'pkg.Box[int].value'] {
		assert g.cname(name) == c_name(name)
	}
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
	assert c_name('C.printf') == 'printf'
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
	tc.type_aliases['AliasContext'] = 'Context'
	tc.type_alias_modules['AliasContext'] = 'main'
	tc.cur_module = 'veb'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc

	assert g.struct_type_alias_target('main.AliasContext') or { '' } == 'main.Context'
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

	assert g.direct_call_name_for_call(flat.empty_node, '__v3_autostr_ast__Comment') == '__v3_autostr_ast__Comment'
	assert g.direct_call_name_for_call(flat.empty_node, 'ast.__v3_autostr_ast__Comment') == '__v3_autostr_ast__Comment'
	assert g.direct_call_name_for_call(flat.empty_node, '__v3_default_clone_json2__Any') == '__v3_default_clone_json2__Any'
	assert g.direct_call_name_for_call(flat.empty_node, 'ast.__v3_default_clone_json2__Any') == '__v3_default_clone_json2__Any'
	assert g.fn_c_name_in_module('main', '__v3_default_clone_json2__Any') == '__v3_default_clone_json2__Any'
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
		name: 'Data'
		base_type: types.Type(types.Pointer{
			base_type: types.Type(types.void_)
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
			name: 'veb.Context'
			typ: types.Type(types.Struct{
				name: 'veb.Context'
			})
			is_embed: true
		},
	]

	base := types.Type(types.Struct{
		name: 'main.Context'
	})
	expected := types.Type(types.Struct{
		name: 'Context'
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
	g.generic_method_candidates[generic_method_candidate_key('jsonrpc', 'encode_batch')] = [
		GenericMethodCandidate{
			name: 'jsonrpc.[]Request.encode_batch'
			ret: types.Type(types.string_)
		},
	]

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
	assert g.enum_selector_base_name('kind.Kind')? == 'first.token.Kind'
	tc.cur_file = 'two.v'
	assert g.import_alias_module('kind')? == 'second.token'
	assert g.enum_selector_base_name('kind.Kind')? == 'second.token.Kind'
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
		elem_type: types.Type(types.Map{
			key_type: types.Type(types.String{})
			value_type: types.Type(types.int_)
		})
		len: 3
	})
	assert typeof_display_resolved_type_name(fixed_maps) == '[3]map[string]int'
}

fn test_fixed_array_typedef_allows_opaque_pointer_elements() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.tc = &tc
	opaque := types.Type(types.Struct{
		name: 'C.Foo'
	})
	assert g.fixed_array_type_has_unknown_struct(opaque)
	assert !g.fixed_array_type_has_unknown_struct(types.Type(types.Pointer{
		base_type: opaque
	}))
}

fn test_sum_type_index_rejects_ambiguous_qualified_suffix() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['a.tast.Value'] = ['a.tast.First', 'a.tast.Target']
	tc.sum_types['b.tast.Value'] = ['b.tast.Target', 'b.tast.Second']
	mut g := FlatGen.new()
	g.tc = &tc
	assert g.sum_type_index('tast.Value', 'b.tast.Target') == 0
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
		kind: .fn_decl
		value: 'FlatGen.sum_type_index'
	}, 'c', 'interface.v', 'c__FlatGen__sum_type_index', false)
	assert !g.should_emit_fn_node_in_module_known(flat.Node{
		kind: .fn_decl
		value: 'Transformer.sum_type_index'
	}, 'transform', 'sum.v', 'transform__Transformer__sum_type_index', false)
}

fn test_typeof_type_index_fallback_uses_matching_sum_variant() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['main.Value'] = ['main.Foo', 'main.Bar']
	tc.sum_types['main.Other'] = ['main.Baz', 'main.Qux']
	mut g := FlatGen.new()
	g.tc = &tc
	assert g.type_index_for_type_name('Foo') == 1
	assert g.type_index_for_type_name('Bar') == 2
	assert g.type_index_for_type_name('Qux') == 2
	assert g.type_index_for_type_name('NotVariant') == 0
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

fn test_preserved_system_include_declarations_are_header_specific() {
	assert c_preserved_system_include_skips_tree_scan('<Cocoa/Cocoa.h>')
	assert c_preserved_system_include_skips_tree_scan('<Foundation/Foundation.h>')
	assert !c_preserved_system_include_skips_tree_scan('<stdio.h>')
	assert c_header_owned_system_include_skips_tree_scan('<Metal/Metal.h>')
	assert c_header_owned_system_include_skips_tree_scan(' <QuartzCore/CAMetalLayer.h> ')
	assert c_header_owned_system_include_skips_tree_scan('<mbedtls/ssl.h>')
	assert 'mbedtls_ssl_context' in c_preserved_system_include_typedef_names('<mbedtls/ssl.h>')
	assert c_header_owned_uses_single_scan('/vroot/thirdparty/sokol/sokol_app.h', '/vroot')
	assert c_header_owned_uses_single_scan('/vroot/thirdparty/sokol/sokol_gfx.h', '/vroot')
	assert c_header_owned_uses_single_scan('/vroot/thirdparty/stb_image/stb_image.h', '/vroot')
	assert !c_header_owned_uses_single_scan('/project/sokol_app.h', '/vroot')
	assert c_preserved_system_include_declared_fns('<stdio.h>').len == 0
	assert 'sqlite3_bind_text' in c_preserved_system_include_declared_fns('"sqlite3.h"')
	assert 'sqlite3_column_name' in c_preserved_system_include_declared_fns('<sqlite3.h>')
	assert 'mbedtls_pk_parse_key' in c_preserved_system_include_declared_fns('<mbedtls/ssl.h>')
	assert 'mbedtls_net_accept' in c_preserved_system_include_declared_fns('<mbedtls/net_sockets.h>')
	assert c_preserved_system_include_declared_fns('<openssl/ssl.h>') == ['X509_free']
	assert c_preserved_system_include_declared_fns('<openssl/x509.h>') == [
		'X509_free',
	]
	assert 'EC_POINT_mul' in c_preserved_system_include_declared_fns('<openssl/ec.h>')
	assert 'OPENSSL_free' in c_preserved_system_include_declared_fns('<openssl/ec.h>')
	assert c_preserved_system_include_declared_fns('<objc/message.h>') == [
		'objc_msgSend',
		'objc_msgSendSuper',
	]
	assert c_preserved_system_include_struct_names('<poll.h>') == ['pollfd']
}

fn test_compiler_header_to_preserve_is_anchored_to_vroot() {
	root := os.join_path(os.vtmp_dir(), 'v3_preserve_header_${os.getpid()}')
	header_dir := os.join_path(root, 'thirdparty', 'sokol')
	os.rmdir_all(root) or {}
	os.mkdir_all(header_dir)!
	defer {
		os.rmdir_all(root) or {}
	}
	header_path := os.join_path(header_dir, 'sokol_app.h')
	os.write_file(header_path, 'typedef int sapp_test;\n')!
	resolved := c_compiler_header_to_preserve('"sokol_app.h"', root, '', [header_dir]) or {
		assert false
		return
	}
	assert os.real_path(resolved) == os.real_path(header_path)
	assert c_compiler_header_to_preserve('"sokol_app.h"', '/different/vroot', '', [
		header_dir,
	]) == none
}

fn test_unresolved_openssl_headers_are_preserved() {
	assert c_should_preserve_uninlined_include('<openssl/ecdsa.h>')
	assert c_should_preserve_uninlined_include('<openssl/obj_mac.h>')
}

fn test_objective_c_message_header_remains_in_generated_source() {
	assert c_include_should_remain_in_inlined_text('<objc/message.h>')
}

fn test_preserved_include_keeps_macro_declared_functions_authoritative() {
	mut g := FlatGen.new()
	g.collect_preserved_include_metadata('<openssl/ssl.h>', '')
	assert !g.should_emit_c_extern_decl('X509_free')
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
	imports :=
		c_header_objective_c_framework_imports('#ifdef _WIN32\n#include <windows.h>\n#endif\n#import <Cocoa/Cocoa.h>\n#include <QuartzCore/QuartzCore.h>\n')
	assert imports == '#import <Cocoa/Cocoa.h>\n#include <QuartzCore/QuartzCore.h>'
	guarded :=
		c_header_objective_c_framework_imports('#ifdef __APPLE__\n#include <Cocoa/Cocoa.h>\n#else\n#include <X11/Xlib.h>\n#endif\n')
	assert guarded == '#ifdef __APPLE__\n#include <Cocoa/Cocoa.h>\n#endif'
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

fn test_x11_system_headers_preserve_external_structs() {
	assert 'XGetWindowAttributes' in c_preserved_system_include_declared_fns('<X11/Xlib.h>')
	assert 'XCreateSimpleWindow' in c_preserved_system_include_declared_fns('<X11/Xlib.h>')
	assert 'WhitePixel' in c_preserved_system_include_declared_fns('<X11/Xlib.h>')
	assert 'XSetWMNormalHints' in c_preserved_system_include_declared_fns('<X11/Xutil.h>')
	assert 'XrmGetResource' in c_preserved_system_include_declared_fns('<X11/Xresource.h>')
	assert 'XkbGetMap' in c_preserved_system_include_declared_fns('<X11/XKBlib.h>')
	assert 'XISelectEvents' in c_preserved_system_include_declared_fns('<X11/extensions/XInput2.h>')
	assert 'XcursorImageCreate' in c_preserved_system_include_declared_fns('<X11/Xcursor/Xcursor.h>')
	assert 'accept4' in c_preserved_system_include_declared_fns('<sys/socket.h>')
	assert 'sendfile' in c_preserved_system_include_declared_fns('<sys/sendfile.h>')
	assert c_should_preserve_uninlined_include('<sys/sendfile.h>')
	assert 'pthread_sigmask' in c_preserved_system_include_declared_fns('<pthread.h>')
	assert 'sigtimedwait' in c_preserved_system_include_declared_fns('<signal.h>')
	assert 'Display' in c_preserved_system_include_struct_names('<X11/Xlib.h>')
	assert 'Display' in c_preserved_system_include_typedef_names('<X11/Xlib.h>')
	assert 'XEvent' in c_preserved_system_include_struct_names('<X11/Xlib.h>')
	assert 'XVisualInfo' in c_preserved_system_include_struct_names('<X11/Xutil.h>')
	assert 'XrmValue' in c_preserved_system_include_struct_names('<X11/Xresource.h>')
	assert 'XkbDescRec' in c_preserved_system_include_struct_names('<X11/XKBlib.h>')
	assert 'XIEventMask' in c_preserved_system_include_struct_names('<X11/extensions/XInput2.h>')
	assert 'XcursorImage' in c_preserved_system_include_struct_names('<X11/Xcursor/Xcursor.h>')
	assert 'XRRCrtcInfo' in c_preserved_system_include_struct_names('<X11/extensions/Xrandr.h>')
	assert 'XRRCrtcInfo' in c_preserved_system_include_typedef_names('<X11/extensions/Xrandr.h>')
}

fn test_apple_framework_typedef_names_are_preserved() {
	names := c_preserved_system_include_typedef_names('<Cocoa/Cocoa.h>')
	assert 'BOOL' in names
	assert 'NSRange' in names
	assert 'NSRect' in names
}

fn test_header_owned_macro_flags_preserve_values() {
	state := c_header_macro_state_for_flags(['-DFEATURE=7', '-D', 'HEADER="types.h"', '-UOLD_FEATURE'])
	assert state.defined['FEATURE']
	assert state.macro_values['FEATURE'] == '7'
	assert state.macro_values['HEADER'] == '"types.h"'
	assert state.undefined['OLD_FEATURE']
}

fn test_header_owned_source_condition_merges_equal_branch_macros() {
	mut g := FlatGen.new()
	g.collect_header_owned_source_macro_directive('#if UNKNOWN_FEATURE')
	g.collect_header_owned_source_macro_directive('#define HAS_ALIAS 3')
	g.collect_header_owned_source_macro_directive('#else')
	g.collect_header_owned_source_macro_directive('#define HAS_ALIAS 3')
	g.collect_header_owned_source_macro_directive('#endif')
	state := g.header_owned_macro_context.state
	assert state.defined['HAS_ALIAS']
	assert state.macro_values['HAS_ALIAS'] == '3'
}

fn test_header_owned_include_aliases_expand_lazily() {
	state := CHeaderMacroState{
		defined: {
			'A': true
			'B': true
		}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: {
			'A': 'B'
			'B': '"two.h"'
		}
	}
	assert c_header_owned_include_args('A', state, '', '/tmp/source.h') == ['"two.h"']
}

fn test_header_owned_typedef_scan_ignores_non_declarations_and_declarator_attributes() {
	clean := c_header_owned_typedef_scan_text('// typedef struct Wrong CommentAlias;\n#define MAKE_ALIAS typedef struct Wrong MacroAlias;\nconst char *example = "typedef struct Wrong StringAlias;";\ntypedef struct Impl RealAlias __attribute__((deprecated));\ntypedef struct Impl __attribute__((deprecated, aligned(8))) PrefixAlias;\ntypedef struct Impl keep__attribute__Alias;\ntypedef struct Impl __attribute__Alias;\ntypedef struct __attribute__((packed)) { int value; } PackedAlias;\ntypedef struct Tagged __attribute__((aligned(8))) { int value; } TaggedAlias;\nstatic inline void helper(void) {\n\ttypedef struct Hidden HiddenAlias;\n\ttypedef int HiddenPlain;\n}\nextern "C" {\n\ttypedef struct External ExternalAlias;\n\ttypedef int ExternalPlain;\n\tstatic inline void linked_helper(void) { typedef int LinkedHidden; }\n}\ntypedef int VisiblePlain;\n')
	aliases := c_typedef_all_aggregate_aliases(clean)
	assert aliases == ['RealAlias', 'PrefixAlias', 'keep__attribute__Alias', '__attribute__Alias',
		'PackedAlias', 'TaggedAlias', 'ExternalAlias']
	assert c_typedef_plain_aliases(clean) == ['ExternalPlain', 'VisiblePlain']
}

fn test_top_level_include_deduplication_resets_after_macro_changes() {
	directives := dedupe_top_level_c_includes(['#include <types.h>', '#include <types.h>',
		'#define FEATURE 1', '#include <types.h>'])
	assert directives == ['#include <types.h>', '#define FEATURE 1', '#include <types.h>']
}

fn test_header_owned_compiler_state_includes_clang_guards() {
	mut g := FlatGen.new()
	g.ccompiler = 'clang'
	state := g.header_owned_initial_macro_state()
	assert state.defined['__clang__']
	assert state.defined['__GNUC__']
}

fn test_header_owned_compiler_macro_probe_uses_effective_compile_context() {
	args := c_header_compiler_predefined_macro_args(['-fopenmp', '-std=c17', '-m32', '-target',
		'riscv64-linux-gnu', '-D', 'FEATURE=1', '-fobjc-arc', '-x', 'none', '-L', '/tmp/lib', '-lssl',
		'-include', 'types.h', '-imacros', 'macros.h', '/tmp/native.c'], false, pref.Target{}, '/tmp/probe.c')
	assert '-fopenmp' in args
	assert '-std=c17' in args
	assert '-m32' in args
	assert args.contains('-target')
	assert args.contains('riscv64-linux-gnu')
	assert args.contains('-D')
	assert args.contains('FEATURE=1')
	assert args.contains('objective-c')
	assert '-L' !in args
	assert '-lssl' !in args
	assert '-include' !in args
	assert 'types.h' !in args
	assert '-imacros' in args
	assert 'macros.h' in args
	assert '/tmp/native.c' !in args
	assert args.last() == '/tmp/probe.c'

	mac_target := pref.Target{
		os: 'macos'
		arch: 'arm64'
	}
	mac_host := pref.Target{
		os: 'macos'
		arch: 'amd64'
	}
	assert c_header_compiler_predefined_target_args(mac_target, mac_host) == ['-arch', 'arm64']
}

fn test_header_owned_scan_resolves_ambient_include_dirs() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_ambient_include_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source_dir := os.join_path(dir, 'source')
	os.mkdir_all(source_dir)!
	os.write_file(os.join_path(dir, 'types.h'), 'typedef struct Ambient AmbientAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.AmbientAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.macro_probe_c_flags = ['-I', dir]
	g.collect_header_owned_c_typedefs('"types.h"', os.join_path(source_dir, 'sample.v'))
	assert g.header_owned_c_typedefs['AmbientAlias']
}

fn test_header_owned_scan_resolves_quote_only_include_dirs() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_quote_include_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source_dir := os.join_path(dir, 'source')
	quote_dir := os.join_path(dir, 'quote')
	os.mkdir_all(source_dir)!
	os.mkdir_all(quote_dir)!
	os.write_file(os.join_path(quote_dir, 'types.h'), 'typedef struct QuoteOnly QuoteOnlyAlias;\n')!
	os.write_file(os.join_path(quote_dir, 'angle.h'), 'typedef struct AngleOnly AngleOnlyAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.QuoteOnlyAlias'] = true
	tc.c_typedef_structs['C.AngleOnlyAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.c_flags = ['-iquote', quote_dir]
	g.collect_header_owned_c_typedefs('"types.h"', os.join_path(source_dir, 'sample.v'))
	g.collect_header_owned_c_typedefs('<angle.h>', os.join_path(source_dir, 'sample.v'))
	assert g.header_owned_c_typedefs['QuoteOnlyAlias']
	assert !g.header_owned_c_typedefs['AngleOnlyAlias']
}

fn test_header_owned_scan_resolves_after_include_dirs_in_order() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_after_include_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	source_dir := os.join_path(dir, 'source')
	ordinary_dir := os.join_path(dir, 'ordinary')
	after_dir := os.join_path(dir, 'after')
	os.mkdir_all(source_dir)!
	os.mkdir_all(ordinary_dir)!
	os.mkdir_all(after_dir)!
	os.write_file(os.join_path(ordinary_dir, 'types.h'), 'typedef struct Ordinary OrdinaryAlias;\n')!
	os.write_file(os.join_path(after_dir, 'types.h'), 'typedef struct WrongAfter WrongAfterAlias;\n')!
	os.write_file(os.join_path(after_dir, 'after_only.h'), 'typedef struct AfterOnly AfterOnlyAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.OrdinaryAlias'] = true
	tc.c_typedef_structs['C.WrongAfterAlias'] = true
	tc.c_typedef_structs['C.AfterOnlyAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.c_flags = ['-idirafter', after_dir, '-I', ordinary_dir]
	g.collect_header_owned_c_typedefs('"types.h"', os.join_path(source_dir, 'sample.v'))
	g.collect_header_owned_c_typedefs('"after_only.h"', os.join_path(source_dir, 'sample.v'))
	assert g.header_owned_c_typedefs['OrdinaryAlias']
	assert !g.header_owned_c_typedefs['WrongAfterAlias']
	assert g.header_owned_c_typedefs['AfterOnlyAlias']
}

fn test_header_owned_scan_resolves_framework_include_dirs() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_framework_include_${os.getpid()}')
	os.rmdir_all(dir) or {}
	headers_dir := os.join_path(dir, 'FrameworkRoot', 'Foo.framework', 'Headers')
	os.mkdir_all(headers_dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(headers_dir, 'Foo.h'), 'typedef struct FrameworkImpl FrameworkAlias;\n')!
	framework_root := os.join_path(dir, 'FrameworkRoot')
	assert c_flag_framework_include_dirs(['-F', framework_root, '-iframework=${framework_root}']) == [
		framework_root,
	]
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.FrameworkAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.c_flags = ['-F', framework_root]
	g.collect_header_owned_c_typedefs('<Foo/Foo.h>', os.join_path(dir, 'sample.v'))
	assert g.header_owned_c_typedefs['FrameworkAlias']
}

fn test_header_owned_compiler_state_preserves_version_values() {
	values := c_header_compiler_predefined_macro_values_from_output('#define unrelated 9\n#define __GNUC__ 12\n#define __GNUC_MINOR__ 2\n#define __clang_major__ 18\n#define __x86_64__ 1\n#define __aarch64__ 1\n#define __STDC_VERSION__ 201710L\n#define __has_builtin(x) __builtin_has_attribute(x)\n')
	assert values == {
		'unrelated':        '9'
		'__GNUC__':         '12'
		'__GNUC_MINOR__':   '2'
		'__clang_major__':  '18'
		'__x86_64__':       '1'
		'__aarch64__':      '1'
		'__STDC_VERSION__': '201710L'
		'__has_builtin':    ''
	}
	mut defined := map[string]bool{}
	for name, _ in values {
		defined[name] = true
	}
	known, active := c_header_objective_c_condition_state('defined(__x86_64__) && __GNUC__ >= 4', defined, map[string]bool{}, map[string]bool{}, values, false, pref.Target{
		os: 'linux'
	})
	assert known
	assert active
	known_function, active_function := c_header_objective_c_condition_state('defined(__has_builtin)', defined, map[string]bool{}, map[string]bool{}, values, false, pref.Target{
		os: 'linux'
	})
	assert known_function
	assert active_function
}

fn test_header_owned_scan_expands_only_invoked_typedef_macros() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define UNUSED typedef struct Wrong UnusedAlias;\n#define DECLARE_ALIAS typedef struct Impl Alias;\nDECLARE_ALIAS\n', state, false, pref.Target{
		os: 'linux'
	})
	assert !scan.typedef_macro_expansions.contains('UnusedAlias')
	assert scan.typedef_macro_expansions.trim_space() == 'typedef struct Impl Alias;'
}

fn test_header_owned_scan_expands_invoked_function_typedef_macros() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define UNUSED(name) typedef struct Wrong name;\n#define DECLARE(name) typedef struct Impl name;\nDECLARE(Alias)\n', state, false, pref.Target{
		os: 'linux'
	})
	assert !scan.typedef_macro_expansions.contains('Wrong')
	assert scan.typedef_macro_expansions.trim_space() == 'typedef struct Impl Alias;'
}

fn test_header_owned_scan_accumulates_multiline_function_typedef_macros() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define DECLARE(name) typedef struct Impl name;\nDECLARE(\nMultilineAlias\n)\n', state, false, pref.Target{
		os: 'linux'
	})
	assert scan.typedef_macro_expansions.trim_space() == 'typedef struct Impl MultilineAlias;'
}

fn test_header_owned_scan_expands_nested_invoked_function_typedef_macros() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define DECLARE_IMPL(name) typedef struct Impl name;\n#define DECLARE(name) DECLARE_IMPL(name)\nDECLARE(Alias)\n', state, false, pref.Target{
		os: 'linux'
	})
	assert scan.typedef_macro_expansions.trim_space() == 'typedef struct Impl Alias;'
}

fn test_header_owned_scan_expands_function_macros_in_conditions() {
	state := CHeaderMacroState{
		defined: {
			'__GNUC__': true
		}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: {
			'__GNUC__': '12'
		}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define VERSION_AT_LEAST(n) (__GNUC__ >= (n))\n#if VERSION_AT_LEAST(4)\ntypedef struct Active ActiveAlias;\n#else\ntypedef struct Inactive InactiveAlias;\n#endif\n', state, false, pref.Target{
		os: 'linux'
	})
	assert c_header_owned_typedef_aliases(scan.text) == ['ActiveAlias'], scan.text
}

fn test_header_owned_scan_expands_macros_inside_typedef_replacements() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define NAME Alias\n#define DECL typedef struct Impl NAME;\nDECL\n', state, false, pref.Target{
		os: 'linux'
	})
	assert scan.typedef_macro_expansions.trim_space() == 'typedef struct Impl Alias;'
}

fn test_header_owned_scan_expands_pasting_macros_inside_typedef_replacements() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define CAT(x) x##_t\n#define DECL(name) typedef struct Impl CAT(name);\nDECL(Alias)\n', state, false, pref.Target{
		os: 'linux'
	})
	assert scan.typedef_macro_expansions.trim_space() == 'typedef struct Impl Alias_t;'
	assert c_header_owned_typedef_aliases(scan.typedef_macro_expansions) == ['Alias_t'], scan.typedef_macro_expansions
}

fn test_header_owned_scan_evaluates_logical_object_macro_conditions() {
	state := CHeaderMacroState{
		defined: {
			'__GNUC__':       true
			'__GNUC_MINOR__': true
		}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: {
			'__GNUC__':       '4'
			'__GNUC_MINOR__': '1'
		}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define SUPPORTED (__GNUC__ >= 4 && __GNUC_MINOR__ >= 1)\n#if SUPPORTED\ntypedef struct Active SupportedAlias;\n#else\ntypedef struct Inactive InactiveAlias;\n#endif\n', state, false, pref.Target{
		os: 'linux'
	})
	assert c_header_owned_typedef_aliases(scan.text) == ['SupportedAlias'], scan.text
}

fn test_header_owned_scan_evaluates_logical_or_object_macro_conditions() {
	state := CHeaderMacroState{
		defined: {
			'__GNUC__':       true
			'__GNUC_MINOR__': true
		}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: {
			'__GNUC__':       '3'
			'__GNUC_MINOR__': '9'
		}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define SUPPORTED (__GNUC__ >= 4 || __GNUC_MINOR__ >= 5)\n#if SUPPORTED\ntypedef struct Active SupportedAlias;\n#else\ntypedef struct Inactive InactiveAlias;\n#endif\n', state, false, pref.Target{
		os: 'linux'
	})
	assert c_header_owned_typedef_aliases(scan.text) == ['SupportedAlias'], scan.text
}

fn test_header_owned_scan_excludes_function_scoped_typedef_macro_invocations() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define DECLARE(name) typedef struct Impl name;\nvoid helper(void) {\nDECLARE(LocalAlias)\n}\nDECLARE(GlobalAlias)\n', state, false, pref.Target{
		os: 'linux'
	})
	assert !scan.typedef_macro_expansions.contains('LocalAlias')
	assert scan.typedef_macro_expansions.trim_space() == 'typedef struct Impl GlobalAlias;'
}

fn test_header_owned_scan_excludes_conditional_function_scoped_typedefs() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
		external_macros_possible: true
	}
	scan := c_header_definitely_active_scan('void helper(void) {\n#if EXTERNAL_LAYOUT\ntypedef struct First LocalAlias;\n#else\ntypedef struct Second LocalAlias;\n#endif\n}\n', state, false, pref.Target{
		os: 'linux'
	})
	assert 'LocalAlias' !in c_header_owned_typedef_aliases(scan.typedef_macro_expansions), scan.typedef_macro_expansions
}

fn test_header_owned_scan_keeps_nested_include_typedefs_function_scoped() {
	dir := os.join_path(os.vtmp_dir(), 'v3_local_include_typedef_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'local.h'), 'typedef struct LocalImpl LocalAlias;\n')!
	os.write_file(os.join_path(dir, 'wrapper.h'), 'static inline void helper(void) {\n#include "local.h"\nLocalAlias value;\n}\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.LocalAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.collect_header_owned_c_typedefs('"wrapper.h"', os.join_path(dir, 'sample.v'))
	assert !g.header_owned_c_typedefs['LocalAlias']
}

fn test_header_owned_scan_restores_pushed_macro_definition() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define SELECT 1\n#pragma push_macro("SELECT")\n#undef SELECT\n#pragma pop_macro("SELECT")\n#if SELECT\ntypedef struct Active RestoredAlias;\n#else\ntypedef struct Wrong WrongAlias;\n#endif\n', state, false, pref.Target{
		os: 'linux'
	})
	assert c_header_owned_typedef_aliases(scan.text) == ['RestoredAlias'], scan.text
}

fn test_header_owned_scan_resolves_compiler_feature_predicates() {
	text := '#if __has_builtin(__builtin_expect)\ntypedef struct Builtin BuiltinAlias;\n#endif\n#if __has_feature(v3_missing_feature) || __has_extension(v3_missing_extension)\ntypedef struct Wrong WrongAlias;\n#endif\n'
	values := c_header_compiler_feature_predicate_values('cc', []string{}, false, pref.Target{}, text)
	assert values['__has_builtin(__builtin_expect)'] == 1
	assert values['__has_feature(v3_missing_feature)'] == -1
	assert values['__has_extension(v3_missing_extension)'] == -1
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan_in_file(text, state, false, pref.Target{
		os: 'linux'
	}, CHeaderIncludeContext{
		feature_predicates: values
	})
	assert c_header_owned_typedef_aliases(scan.text) == ['BuiltinAlias'], scan.text
}

fn test_header_owned_feature_predicate_invocations_probe_macro_wrappers() {
	text := '#define HAS_BUILTIN(x) __has_builtin(x)\n#if HAS_BUILTIN(__builtin_expect)\ntypedef struct Builtin BuiltinAlias;\n#endif\n'
	invocations := c_header_feature_predicate_invocations(text)
	assert '__has_builtin(__builtin_expect)' in invocations, invocations.str()
}

fn test_header_owned_scan_resolves_wrapped_feature_predicates() {
	text := '#define HAS_BUILTIN(x) __has_builtin(x)\n#if HAS_BUILTIN(__builtin_expect)\ntypedef struct Builtin BuiltinAlias;\n#else\ntypedef struct Wrong WrongAlias;\n#endif\n'
	values := c_header_compiler_feature_predicate_values('cc', []string{}, false, pref.Target{}, text)
	assert values['__has_builtin(__builtin_expect)'] == 1
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan_in_file(text, state, false, pref.Target{
		os: 'linux'
	}, CHeaderIncludeContext{
		feature_predicates: values
	})
	assert c_header_owned_typedef_aliases(scan.text) == ['BuiltinAlias'], scan.text
}

fn test_header_owned_compiler_implicit_include_dirs_parser() {
	base := os.join_path(os.vtmp_dir(), 'v3_implicit_incdirs_${os.getpid()}')
	real_a := os.join_path(base, 'a')
	real_b := os.join_path(base, 'b')
	framework := os.join_path(base, 'frameworks')
	after := os.join_path(base, 'after')
	os.mkdir_all(real_a) or { panic(err) }
	os.mkdir_all(real_b) or { panic(err) }
	os.mkdir_all(framework) or { panic(err) }
	os.mkdir_all(after) or { panic(err) }
	defer {
		os.rmdir_all(base) or {}
	}
	missing := os.join_path(base, 'missing')
	output := 'ignored preamble\n#include "..." search starts here:\n ${real_a}\n#include <...> search starts here:\n ${real_b}\n ${framework} (framework directory)\n ${missing}\nEnd of search list.\n ${after}\n'
	dirs := c_header_compiler_implicit_include_dirs_from_output(output)
	// Only existing, non-framework directories inside the search block survive.
	assert os.real_path(real_a) in dirs, dirs.str()
	assert os.real_path(real_b) in dirs, dirs.str()
	assert os.real_path(missing) !in dirs, dirs.str()
	assert os.real_path(framework) !in dirs, dirs.str()
	assert os.real_path(after) !in dirs, dirs.str()
	assert dirs.len == 2, dirs.str()
}

fn test_header_owned_compiler_implicit_include_dirs_are_real() {
	// Live discovery via the platform C compiler; every reported root must exist.
	dirs := c_header_compiler_implicit_include_dirs('cc', []string{}, false, pref.Target{})
	for dir in dirs {
		assert os.is_dir(dir), dir
	}
}

fn test_header_owned_msvc_predefined_macro_values_parser() {
	candidates := c_header_msvc_predefined_macro_candidates()
	// Emulates `cl /EP` output: defined numeric macros expanded, others untouched.
	output := 'V3_MSVC_MACRO_0 1930\nV3_MSVC_MACRO_1 193030705\nV3_MSVC_MACRO_5 1\nV3_MSVC_MACRO_13 __STDC_VERSION__\n'
	values := c_header_msvc_predefined_macro_values_from_output(output, candidates)
	assert values['_MSC_VER'] == '1930', values.str()
	assert values['_MSC_FULL_VER'] == '193030705', values.str()
	assert values['_WIN32'] == '1', values.str()
	// Unexpanded (still-textual) macros are not recorded as numeric values.
	assert '__STDC_VERSION__' !in values, values.str()
}

fn test_header_owned_msvc_predefined_macro_marker_indices_do_not_collide() {
	// Marker 1 must not swallow marker 10..13 despite the shared textual prefix.
	candidates := c_header_msvc_predefined_macro_candidates()
	output := 'V3_MSVC_MACRO_11 42\n'
	values := c_header_msvc_predefined_macro_values_from_output(output, candidates)
	assert values[candidates[11]] == '42', values.str()
	assert candidates[1] !in values, values.str()
}

fn test_header_owned_scan_resolves_msvc_version_guard() {
	// _MSC_VER is defined with a numeric value, so the version guard resolves and
	// only the active-branch typedef is owned.
	state := CHeaderMacroState{
		defined: {
			'_MSC_VER': true
		}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: {
			'_MSC_VER': '1930'
		}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#if _MSC_VER >= 1900\ntypedef struct Modern ModernAlias;\n#else\ntypedef struct Legacy LegacyAlias;\n#endif\n', state, false, pref.Target{
		os: 'windows'
	})
	assert c_header_owned_typedef_aliases(scan.text) == ['ModernAlias'], scan.text
}

fn test_header_owned_scan_expands_variadic_typedef_macros() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
	}
	scan := c_header_definitely_active_scan('#define DECLARE(name, ...) typedef struct Impl name;\n#define DECLARE_ARGS(...) typedef struct Impl __VA_ARGS__;\n#define DECLARE_NAMED(tag, aliases...) typedef struct tag aliases;\nDECLARE(FirstAlias, ignored)\nDECLARE_ARGS(SecondAlias)\nDECLARE_NAMED(Impl, ThirdAlias)\n', state, false, pref.Target{
		os: 'linux'
	})
	assert c_header_owned_typedef_aliases(scan.typedef_macro_expansions) == [
		'FirstAlias',
		'SecondAlias',
		'ThirdAlias',
	], scan.typedef_macro_expansions
}

fn test_header_owned_scan_retains_typedef_alias_common_to_all_possible_arms() {
	state := CHeaderMacroState{
		defined: map[string]bool{}
		undefined: map[string]bool{}
		uncertain: map[string]bool{}
		macro_values: map[string]string{}
		function_macro_values: map[string]string{}
		external_macros_possible: true
	}
	scan := c_header_definitely_active_scan('#if EXTERNAL_LAYOUT\ntypedef struct First CommonAlias;\n#else\ntypedef struct Second CommonAlias;\n#endif\n', state, false, pref.Target{
		os: 'linux'
	})
	assert c_header_owned_typedef_aliases(scan.typedef_macro_expansions) == [
		'CommonAlias',
	], scan.typedef_macro_expansions
}

fn test_header_owned_scan_retains_alias_from_conditional_child_headers() {
	dir := os.join_path(os.vtmp_dir(), 'v3_conditional_header_aliases_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'first.h'), 'typedef struct First CommonAlias;\n')!
	os.write_file(os.join_path(dir, 'second.h'), 'typedef struct Second CommonAlias;\n')!
	os.write_file(os.join_path(dir, 'parent.h'), '#if EXTERNAL_LAYOUT\n#include "first.h"\n#else\n#include "second.h"\n#endif\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.CommonAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.ensure_header_owned_macro_context()
	g.header_owned_macro_context.state.external_macros_possible = true
	g.collect_header_owned_c_typedefs('"parent.h"', os.join_path(dir, 'sample.v'))
	assert g.header_owned_c_typedefs['CommonAlias']
}

fn test_header_owned_scan_resolves_include_next_and_has_include_paths() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_include_next_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	first_dir := os.join_path(dir, 'first')
	second_dir := os.join_path(dir, 'second')
	os.mkdir_all(os.join_path(first_dir, 'api'))!
	os.mkdir_all(os.join_path(second_dir, 'api'))!
	os.write_file(os.join_path(first_dir, 'api', 'wrapper.h'), '#if __has_include("impl.h")
#include "impl.h"
#endif
#if __has_include_next("api/wrapper.h")
typedef struct GuardedNext GuardedNextAlias;
#endif
#if __has_include("missing.h")
typedef struct Missing MissingAlias;
#endif
#include_next "api/wrapper.h"
')!
	os.write_file(os.join_path(second_dir, 'impl.h'), 'typedef struct IncludedImpl IncludedImplAlias;\n')!
	os.write_file(os.join_path(second_dir, 'api', 'wrapper.h'), 'typedef struct NextWrapper NextWrapperAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	for alias in ['GuardedNextAlias', 'IncludedImplAlias', 'MissingAlias', 'NextWrapperAlias'] {
		tc.c_typedef_structs['C.${alias}'] = true
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.c_flags = ['-I', first_dir, '-I', second_dir]
	g.collect_header_owned_c_typedefs('"api/wrapper.h"', os.join_path(dir, 'sample.v'))
	assert g.header_owned_c_typedefs['GuardedNextAlias']
	assert g.header_owned_c_typedefs['IncludedImplAlias']
	assert g.header_owned_c_typedefs['NextWrapperAlias']
	assert !g.header_owned_c_typedefs['MissingAlias']
}

fn test_header_owned_scan_expands_function_macro_include_operand() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_function_include_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'wrapper.h'), '#define INCLUDE_FILE(path) path\n#include INCLUDE_FILE("types.h")\n')!
	os.write_file(os.join_path(dir, 'types.h'), 'typedef struct Impl FunctionIncludeAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.FunctionIncludeAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.collect_header_owned_c_typedefs('"wrapper.h"', os.join_path(dir, 'sample.v'))
	assert g.header_owned_c_typedefs['FunctionIncludeAlias']
}

fn test_header_owned_include_next_uses_exact_overlapping_search_dir() {
	dir := os.join_path(os.vtmp_dir(), 'v3_header_include_next_overlap_${os.getpid()}')
	os.rmdir_all(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	root_dir := os.join_path(dir, 'root')
	nested_dir := os.join_path(root_dir, 'sub')
	third_dir := os.join_path(dir, 'third')
	os.mkdir_all(nested_dir)!
	os.mkdir_all(third_dir)!
	os.write_file(os.join_path(nested_dir, 'wrapper.h'), '#include_next <next.h>\n')!
	os.write_file(os.join_path(nested_dir, 'next.h'), 'typedef struct Wrong WrongAlias;\n')!
	os.write_file(os.join_path(third_dir, 'next.h'), 'typedef struct Exact ExactAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	for alias in ['WrongAlias', 'ExactAlias'] {
		tc.c_typedef_structs['C.${alias}'] = true
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.c_flags = ['-I', root_dir, '-I', nested_dir, '-I', third_dir]
	g.collect_header_owned_c_typedefs('"wrapper.h"', os.join_path(dir, 'sample.v'))
	assert g.header_owned_c_typedefs['ExactAlias']
	assert !g.header_owned_c_typedefs['WrongAlias']
}

fn test_header_owned_scan_retains_alias_from_source_conditional_includes() {
	dir := os.join_path(os.vtmp_dir(), 'v3_source_conditional_header_aliases_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'first.h'), 'typedef struct First CommonAlias;\ntypedef struct FirstOnly FirstOnlyAlias;\n')!
	os.write_file(os.join_path(dir, 'second.h'), 'typedef struct Second CommonAlias;\ntypedef struct SecondOnly SecondOnlyAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	for alias in ['CommonAlias', 'FirstOnlyAlias', 'SecondOnlyAlias'] {
		tc.c_typedef_structs['C.${alias}'] = true
	}
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.ensure_header_owned_macro_context()
	g.header_owned_macro_context.state.external_macros_possible = true
	g.collect_header_owned_source_macro_directive('#if EXTERNAL_LAYOUT')
	g.collect_header_owned_c_typedefs('"first.h"', os.join_path(dir, 'sample.v'))
	g.collect_header_owned_source_macro_directive('#else')
	g.collect_header_owned_c_typedefs('"second.h"', os.join_path(dir, 'sample.v'))
	g.collect_header_owned_source_macro_directive('#endif')
	assert g.header_owned_c_typedefs['CommonAlias']
	assert !g.header_owned_c_typedefs['FirstOnlyAlias']
	assert !g.header_owned_c_typedefs['SecondOnlyAlias']
}

fn test_header_owned_scan_collects_forced_include_typedefs() {
	dir := os.join_path(os.vtmp_dir(), 'v3_forced_header_aliases_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'types.h'), '#ifndef TYPES_H\n#define TYPES_H\ntypedef struct ForcedType ForcedAlias;\n#endif\n')!
	os.write_file(os.join_path(dir, 'macros.h'), 'typedef struct MacrosOnly MacrosOnlyAlias;\n')!
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.c_typedef_structs['C.ForcedAlias'] = true
	tc.c_typedef_structs['C.MacrosOnlyAlias'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.c_flags = ['-I', dir, '-include', 'types.h', '-imacros', 'macros.h']
	g.rebuild_header_owned_c_typedefs()
	assert g.header_owned_c_typedefs['ForcedAlias']
	assert !g.header_owned_c_typedefs['MacrosOnlyAlias']
}

fn test_large_transitive_header_tree_is_preserved() {
	root := os.join_path(os.temp_dir(), 'v3_large_transitive_header_tree_test')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	padding := 'x'.repeat(140_000)
	os.write_file(os.join_path(root, 'a.h'), '/*${padding}*/\n') or { panic(err) }
	os.write_file(os.join_path(root, 'b.h'), '/*${padding}*/\n') or { panic(err) }
	one_path := os.join_path(root, 'one.h')
	two_path := os.join_path(root, 'two.h')
	os.write_file(one_path, '#include "a.h"\n') or { panic(err) }
	os.write_file(two_path, '#include "a.h"\n#include "b.h"\n') or { panic(err) }
	mut one_size := CHeaderTreeSize{}
	assert !c_header_tree_exceeds_inline_limit(one_path, '', []string{}, mut one_size)
	mut two_size := CHeaderTreeSize{}
	assert c_header_tree_exceeds_inline_limit(two_path, '', []string{}, mut two_size)
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
