// vtest build: macos
module cleanc

import os
import strings
import v2.ast
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_c_for_test(code string) string {
	return generate_c_for_test_files([code])
}

fn generate_c_for_test_files(sources []string) string {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_flag_enum_codegen_test_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('failed to create temp dir') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{cap: sources.len}
	for i, code in sources {
		tmp_file := os.join_path(tmp_dir, 'file_${i}.v')
		os.write_file(tmp_file, code) or { panic('failed to write temp file') }
		paths << tmp_file
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	return gen.gen()
}

struct CgenTestSource {
	path string
	code string
}

fn generate_c_for_test_sources_with_emit(sources []CgenTestSource, emit_rel_paths []string) string {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_flag_enum_codegen_test_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	os.mkdir_all(tmp_dir) or { panic('failed to create temp dir') }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	mut paths := []string{cap: sources.len}
	for source in sources {
		tmp_file := os.join_path(tmp_dir, source.path)
		os.mkdir_all(os.dir(tmp_file)) or { panic('failed to create temp source dir') }
		os.write_file(tmp_file, source.code) or { panic('failed to write temp file') }
		paths << tmp_file
	}
	mut emit_files := []string{cap: emit_rel_paths.len}
	for rel_path in emit_rel_paths {
		emit_files << os.join_path(tmp_dir, rel_path)
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files(paths, mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	mut gen := Gen.new_with_env_and_pref(trans.transform_files(files), env, prefs)
	gen.set_emit_modules(['main'])
	gen.set_emit_files(emit_files)
	return gen.gen()
}

fn test_result_value_type_preserves_generic_specialization_ptr_suffix() {
	g := Gen{}
	assert g.result_value_type('_result_sync__ThreadLocalStorage_T_Array_markdown__Nodeptr') == 'sync__ThreadLocalStorage_T_Array_markdown__Nodeptr'
	assert g.result_value_type('_result_sqlite__Sqlite3_vfsptr') == 'sqlite__Sqlite3_vfs*'

	mut g2 := Gen{
		generic_struct_instances: {
			'orm__QueryBuilder':        [
				GenericStructInstance{
					params_key: 'array'
					c_name:     'orm__QueryBuilder_T_array'
				},
			]
			'sync__ThreadLocalStorage': [
				GenericStructInstance{
					params_key: 'Array_markdown__Nodeptr'
					c_name:     'sync__ThreadLocalStorage_T_Array_markdown__Nodeptr'
				},
			]
		}
	}
	assert g2.result_value_type('_result_orm__QueryBuilder_T_arrayptr') == 'orm__QueryBuilder_T_array*'
	assert g2.result_value_type('_result_sync__ThreadLocalStorage_T_Array_markdown__Nodeptr') == 'sync__ThreadLocalStorage_T_Array_markdown__Nodeptr'
}

fn test_generate_c_rewrites_flag_enum_zero_static_call() {
	csrc := generate_c_for_test('
@[flag]
enum Bits {
	a
	b
}

fn main() {
	mut bits := Bits.zero()
	bits.set(.a)
	_ = bits.has(.a)
}
')
	assert csrc.contains('(Bits)(0)')
	assert !csrc.contains('Bits__zero')
}

fn test_generate_c_escapes_keyword_struct_fields_in_map_eq() {
	csrc := generate_c_for_test("
struct Item {
	short string
}

fn main() {
	a := {
		'x': Item{short: 'a'}
	}
	b := {
		'x': Item{short: 'b'}
	}
	_ = a == b
}
")
	assert csrc.contains('string__eq(va._short, vb._short)')
	assert !csrc.contains('string__eq(va.short, vb.short)')
}

fn test_generate_c_uses_concrete_map_method_name_in_generic_comptime_body() {
	code := [
		'fn (m map[string]int) query_item(name string) ?int {',
		'	return m[name]',
		'}',
		'',
		'struct Holder {',
		'	items map[string]int',
		'}',
		'',
		'fn find_item[T](value T) ?int {',
		'	@DLR@if T is Holder {',
		'		return value.items.query_item(@SQ@x@SQ@)',
		'	}',
		'	return none',
		'}',
		'',
		'fn main() {',
		'	_ = find_item[Holder](Holder{})',
		'}',
	].join('\n').replace('@DLR@', '$').replace('@SQ@', "'")
	csrc := generate_c_for_test(code)
	assert csrc.contains('Map_string_int__query_item(')
	assert !csrc.contains('map__query_item(')
}

fn test_generate_c_records_generic_struct_instantiations_inside_returned_call_args() {
	csrc := generate_c_for_test_files([
		'
module api

pub struct ApiSuccessResponse[T] {
pub:
	success bool
	result  T
}
',
		'
module main

import api

struct FileInfo {
	name string
}

fn json[T](j T) int {
	_ = j
	return 0
}

fn handle_bool() int {
	return json(api.ApiSuccessResponse[bool]{
		success: true
		result:  true
	})
}

fn handle_files() int {
	mut files := []FileInfo{}
	return json(api.ApiSuccessResponse[[]FileInfo]{
		success: true
		result:  files
	})
}
',
	])
	assert csrc.contains('\tbool result;')
	assert csrc.contains('\tArray_FileInfo result;')
	assert csrc.contains('json_T_api_ApiSuccessResponse_T_Array_FileInfo(((api__ApiSuccessResponse_T_Array_FileInfo){')
	assert !csrc.contains('json_T_api_ApiSuccessResponse(((api__ApiSuccessResponse){.success = true,.result = files})')
}

fn test_called_specialized_name_does_not_remap_concrete_generic_prefix() {
	g := Gen{
		called_fn_names: {
			'json2__Encoder__encode_value_T_api_ApiSuccessResponse_T_bool': true
		}
	}
	if _ := g.called_specialized_name_for_base('json2__Encoder__encode_value_T_api_ApiSuccessResponse') {
		assert false
	}
	assert g.called_specialized_name_for_base('json2__Encoder__encode_value') or { '' } == 'json2__Encoder__encode_value_T_api_ApiSuccessResponse_T_bool'
}

fn test_generate_c_specializes_untyped_int_generic_arg_as_int() {
	csrc := generate_c_for_test('
fn id[T](value T) T {
	return value
}

fn main() {
	_ = id(0)
}
')
	assert csrc.contains('id_T_int(')
	assert !csrc.contains('id_T_int_literal')
}

fn test_generate_c_resolves_generic_map_for_in_key_casts() {
	mut g := Gen{}
	g.active_generic_types = {
		'T': types.Type(types.string_)
	}
	assert g.expr_type_to_c(ast.Ident{
		name: 'T*'
	}) == 'string*'
	assert g.expr_type_to_c(ast.Ident{
		name: 'T**'
	}) == 'string**'
}

fn test_generate_c_passes_json_decode_type_marker_as_null() {
	mut g := Gen{
		sb:              strings.new_builder(64)
		emitted_types:   {
			'body_Foo': true
		}
		fn_param_is_ptr: {
			'json__decode': [true, false]
		}
	}
	g.gen_call_arg('json__decode', 0, ast.PrefixExpr{
		op:   .amp
		expr: ast.Ident{
			name: 'Foo'
		}
	})
	assert g.sb.str() == 'NULL'
}

fn test_generate_c_uses_argument_lowering_for_json_module_decode_fallback() {
	csrc := generate_c_for_test_files([
		'
module json

pub fn decode(typ voidptr, s string) !voidptr {
	return 0
}
',
		'
module main

import json

struct Foo {}

fn main() {
	_ := json.decode(Foo, "") or { return }
}
',
	])
	assert csrc.contains('json__decode(NULL,')
	assert !csrc.contains('json__decode(&Foo,')
}

fn test_generate_c_wraps_json_decode_result_for_or_default_value() {
	csrc := generate_c_for_test_files([
		'
module json

pub fn decode(typ voidptr, s string) !voidptr {
	return 0
}
',
		'
module main

import json

struct Foo {
	name string
}

fn main() {
	foo := json.decode(Foo, "") or { Foo{} }
	_ = foo
}
',
	])
	assert csrc.contains('_result_voidptr _json_decode_raw_')
	assert csrc.contains('_result_Foo _json_decode_res_')
	assert csrc.contains('_result_ok(_json_decode_ptr_')
	assert !csrc.contains('(*(void**)(((u8*)(&_or_t')
}

fn test_generate_c_uses_lhs_enum_type_for_sumtype_smartcast_enum_shorthand_compare() {
	csrc := generate_c_for_test('
enum MD_BLOCKTYPE {
	md_block_doc
	md_block_code
}

type ParentType = MD_BLOCKTYPE | string

fn main() {
	parent := ParentType(MD_BLOCKTYPE.md_block_code)
	if parent is MD_BLOCKTYPE && parent == .md_block_code {
		_ := true
	}
}
')
	assert csrc.contains('MD_BLOCKTYPE__md_block_code')
	assert !csrc.contains('T__md_block_code')
}

fn test_generate_c_preamble_rwmutex_has_lazy_init_field() {
	prefs := &vpref.Preferences{
		backend: .cleanc
	}
	env := types.Environment.new()
	mut gen := Gen.new_with_env_and_pref([]ast.File{}, env, prefs)
	gen.write_preamble()
	csrc := gen.sb.str()
	assert csrc.contains('typedef struct sync__RwMutex { pthread_rwlock_t mutex; u32 inited; } sync__RwMutex;')
}

fn test_generate_c_specializes_generic_method_from_payload_arg_not_receiver() {
	csrc := generate_c_for_test('
struct App {}
struct Payload {}

fn (mut app App) dispatch[T](id int, payload T) {
	_ = id
	_ = payload
}

fn main() {
	mut app := App{}
	app.dispatch(1, Payload{})
}
')
	assert csrc.contains('App__dispatch_T_Payload(')
	assert !csrc.contains('App__dispatch_T_App(')
}

fn test_generate_c_qualifies_local_module_generic_call_in_specialized_body() {
	csrc := generate_c_for_test('
module veb

pub struct RunParams {}
struct App {}
struct Context {}

pub fn run_at[A, X](mut app A, params RunParams) ! {
	run_new[A, X](mut app, params)!
}

pub fn run_new[A, X](mut app A, params RunParams) ! {
	_ = app
	_ = params
}

fn boot() {
	mut app := App{}
	run_at[App, Context](mut app, RunParams{}) or { return }
}

fn direct() {
	mut app := App{}
	run_new[App, Context](mut app, RunParams{}) or { return }
}
')
	assert csrc.contains('veb__run_new_T_veb_App_veb_Context(app, params)')
	assert !csrc.contains('\trun_new_T_A_X(')
	assert !csrc.contains('veb__run_new(app, params)')
	assert !csrc.contains('veb__run_new_T_A_X(app, params)')
}

fn test_generate_c_uses_map_smartcast_for_map_for_in_key_values() {
	csrc := generate_c_for_test('
type Any = []int | map[string]int

fn values(f Any) []int {
	if f is []int {
		return f
	} else if f is map[string]int {
		mut arr := []int{}
		for _, v in f {
			arr << v
		}
		return arr
	}
	return []int{}
}
')
	assert csrc.contains('Map_string_int _map_map_')
	assert csrc.contains('f._data._Map_string_int')
	assert csrc.contains('Map_string_int*)(f._data._Map_string_int')
	assert !csrc.contains('f._data._Array_int))->key_values')
	assert !csrc.contains('= ((((Array_int*)(f._data._Array_int))')
}

fn test_generate_c_uses_map_smartcast_for_recursive_map_for_in_key_values() {
	csrc := generate_c_for_test('
module json2

type Any = []Any | map[string]Any

fn values(f Any) []Any {
	if f is []Any {
		return f
	} else if f is map[string]Any {
		mut arr := []Any{}
		for _, v in f {
			arr << v
		}
		return arr
	}
	return []Any{}
}
')
	assert csrc.contains('Map_string_json2__Any _map_map_')
	assert csrc.contains('f._data._Map_string_json2__Any')
	assert csrc.contains('Map_string_json2__Any*)(f._data._Map_string_json2__Any')
	assert !csrc.contains('= ((((Array_json2__Any*)(f._data._Array_json2__Any))')
}

fn test_markused_runtime_keep_rules_include_transitive_runtime_dependencies() {
	assert is_builtin_runtime_keep_file('/tmp/vlib/builtin/builtin.c.v')
	assert is_builtin_runtime_keep_file('/tmp/vlib/math/bits/bits.c.v')
	assert should_keep_builtin_string_decl(ast.FnDecl{
		name: 'compare_lower_strings'
	})
	assert should_keep_builtin_string_decl(ast.FnDecl{
		name: 'compare_strings_by_len'
	})
}

fn test_collected_c_directives_follow_emit_modules() {
	mut g := Gen.new([
		ast.File{
			name:  'core.v'
			stmts: [
				ast.ModuleStmt{
					name: 'core'
				},
				ast.Directive{
					name:  'include'
					value: '"core_impl.c"'
				},
			]
		},
		ast.File{
			name:  'feature.v'
			stmts: [
				ast.ModuleStmt{
					name: 'feature'
				},
				ast.Directive{
					name:  'include'
					value: '<feature.h>'
				},
				ast.Directive{
					name:  'include'
					value: '"feature_impl.c"'
				},
			]
		},
	])
	g.set_emit_modules(['core'])
	g.write_preamble()
	csrc := g.sb.str()
	assert csrc.contains('#include "core_impl.c"')
	assert csrc.contains('#include <feature.h>')
	assert !csrc.contains('#include "feature_impl.c"')
}

fn test_gen_call_arg_keeps_ierror_for_ierror_param() {
	mut g := Gen{
		sb:                  strings.new_builder(16)
		fn_param_types:      {
			'uses': ['IError']
		}
		runtime_local_types: {
			'err': 'struct IError'
		}
	}
	g.gen_call_arg('uses', 0, ast.Ident{
		name: 'err'
	})
	assert g.sb.str() == 'err'
}

fn test_header_type_const_emits_extern_ierror() {
	mut g := Gen{
		sb:            strings.new_builder(64)
		emitted_types: {
			'body_IError': true
		}
		fn_owner_file: map[string]int{}
		const_exprs:   map[string]string{}
	}
	g.cur_module = 'net'
	g.gen_const_decl_extern(ast.ConstDecl{
		fields: [
			ast.FieldInit{
				name:  'err_timed_out'
				value: ast.Ident{
					name: 'IError'
				}
			},
		]
	})
	csrc := g.sb.str()
	assert csrc.contains('extern IError net__err_timed_out;')
	assert !csrc.contains('#define net__err_timed_out IError')
}

fn test_header_type_const_emits_extern_qualified_type() {
	mut g := Gen{
		sb:            strings.new_builder(64)
		emitted_types: {
			'body_os__File': true
		}
		fn_owner_file: map[string]int{}
		const_exprs:   map[string]string{}
	}
	g.cur_module = 'log'
	g.gen_const_decl_extern(ast.ConstDecl{
		fields: [
			ast.FieldInit{
				name:  'stderr'
				value: ast.SelectorExpr{
					lhs: ast.Expr(ast.Ident{
						name: 'os'
					})
					rhs: ast.Ident{
						name: 'File'
					}
				}
			},
		]
	})
	csrc := g.sb.str()
	assert csrc.contains('extern os__File log__stderr;')
	assert !csrc.contains('#define log__stderr os__File')
}

fn test_gen_return_propagates_struct_ierror_local_from_result_function() {
	mut g := Gen{
		sb:                  strings.new_builder(64)
		cur_fn_ret_type:     '_result_int'
		runtime_local_types: {
			'e': 'struct IError'
		}
	}
	g.gen_stmt(ast.ReturnStmt{
		exprs: [ast.Expr(ast.Ident{
			name: 'e'
		})]
	})
	assert g.sb.str() == 'return (_result_int){ .is_error=true, .err=e };\n'
}

fn test_get_str_fn_for_type_prefers_concrete_str_before_alias_base() {
	mut g := Gen.new([]ast.File{})
	g.alias_base_types['openssl__SSLConn'] = 'mbedtls__SSLConn'
	g.alias_base_types['mbedtls__SSLConn'] = 'openssl__SSLConn'
	g.fn_return_types['openssl__SSLConn_str'] = 'string'
	g.fn_return_types['mbedtls__SSLConn_str'] = 'string'
	assert g.get_str_fn_for_type('openssl__SSLConn') or { '' } == 'openssl__SSLConn_str'
	assert g.get_str_fn_for_type('mbedtls__SSLConn') or { '' } == 'mbedtls__SSLConn_str'
}

fn test_get_str_fn_for_type_stops_on_alias_cycles_without_str_fn() {
	mut g := Gen.new([]ast.File{})
	g.alias_base_types['openssl__SSLConn'] = 'mbedtls__SSLConn'
	g.alias_base_types['mbedtls__SSLConn'] = 'openssl__SSLConn'
	assert g.get_str_fn_for_type('openssl__SSLConn') or { '' } == ''
}

fn test_generate_c_treats_generic_arg_or_index_on_known_fn_as_fn_value() {
	mut g := Gen{
		sb:              strings.new_builder(64)
		cur_module:      'veb'
		fn_return_types: {
			'veb__handler': 'int'
		}
	}
	g.expr(ast.GenericArgOrIndexExpr{
		lhs:  ast.Ident{
			name: 'handler'
		}
		expr: ast.Ident{
			name: 'A'
		}
	})
	assert g.sb.str() == 'veb__handler'

	g.sb = strings.new_builder(64)
	g.gen_index_expr(ast.IndexExpr{
		lhs:  ast.Ident{
			name: 'handler'
		}
		expr: ast.Ident{
			name: 'A'
		}
	})
	assert g.sb.str() == 'veb__handler'
}

fn test_generate_c_casts_generic_function_type_alias_call_expr() {
	mut g := Gen{
		sb:              strings.new_builder(64)
		cur_module:      'veb'
		fn_type_aliases: {
			'veb__MiddlewareHandler': true
		}
	}
	cast_lhs := ast.GenericArgOrIndexExpr{
		lhs:  ast.Ident{
			name: 'MiddlewareHandler'
		}
		expr: ast.Ident{
			name: 'T'
		}
	}
	g.call_expr(cast_lhs, [ast.Expr(ast.Ident{
		name: 'handler'
	})])
	assert g.sb.str() == '((veb__MiddlewareHandler)(handler))'

	g.sb = strings.new_builder(64)
	g.gen_assign_stmt(ast.AssignStmt{
		op:  .decl_assign
		lhs: [ast.Expr(ast.Ident{
			name: 'func'
		})]
		rhs: [
			ast.Expr(ast.CallExpr{
				lhs:  cast_lhs
				args: [ast.Expr(ast.Ident{
					name: 'handler'
				})]
			}),
		]
	})
	assert g.sb.str() == 'veb__MiddlewareHandler func = ((veb__MiddlewareHandler)(handler));\n'

	g.sb = strings.new_builder(64)
	g.call_expr(ast.Ident{
		name: 'MiddlewareHandler_T'
	}, [ast.Expr(ast.Ident{
		name: 'handler'
	})])
	assert g.sb.str() == '((veb__MiddlewareHandler)(handler))'
}

fn test_generate_c_resolves_specialized_generic_receiver_method() {
	mut g := Gen{
		fn_return_types: {
			'dep__Middleware_T_Context__use': 'void'
		}
		fn_param_is_ptr: {
			'dep__Middleware_T_Context__use': [true, false]
		}
	}
	assert g.resolve_method_on_concrete_type('dep__Middleware', 'use') or { '' } == 'dep__Middleware_T_Context__use'
}

fn test_generate_c_emits_builtin_string_field_selector_as_field_access() {
	mut g := Gen{
		sb:                  strings.new_builder(64)
		runtime_local_types: {
			's': 'string'
		}
	}
	g.expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 's'
		}
		rhs: ast.Ident{
			name: 'str'
		}
	})
	assert g.sb.str() == 's.str'

	csrc := generate_c_for_test("
const digit_pairs = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'

fn first_digit(i int) u8 {
	return unsafe { digit_pairs.str[i] }
}

fn first_byte(s string, i int) u8 {
	return unsafe { s.str[i] }
}

struct RE {
	flag int
}

@[direct_array_access; inline]
fn (re &RE) get_char(in_txt string, i int) (u32, int) {
	ini := unsafe { in_txt.str[i] }
	if (re.flag & 1) != 0 || (ini & 0x80) == 0 {
		return u32(ini), 1
	}
	mut tmp := 0
	mut ch := u32(0)
	for tmp < 2 {
		ch = (ch << 8) | unsafe { in_txt.str[i + tmp] }
		tmp++
	}
	return ch, 2
}
")
	assert csrc.contains('return digit_pairs.str[')
	assert csrc.contains('return (s).str[') || csrc.contains('return s.str[')
	assert csrc.contains('u8 ini = in_txt.str[') || csrc.contains('u8 ini = (in_txt).str[')
	assert csrc.contains('| in_txt.str[') || csrc.contains('| (in_txt).str[')
	assert !csrc.contains('return string__str')
	assert !csrc.contains('u8 ini = string__str')
}

fn test_generate_c_uses_pointer_selector_for_channel_field_len() {
	csrc := generate_c_for_test('
struct DB {}

struct Pool {
	connections chan DB
}

fn (mut pool Pool) close() {
	for _ in 0 .. pool.connections.len {
		_ = <-pool.connections or { break }
	}
	}
')
	assert csrc.contains('atomic_load_u32(&((sync__Channel*)(pool->connections))->read_avail)')
	assert !csrc.contains('pool->connections.len')
	assert !csrc.contains('pool->connections->len')
}

fn test_generate_c_does_not_use_short_embedded_owner_for_qualified_field_type() {
	csrc := generate_c_for_test_files([
		'
module dep

pub struct Config {
pub:
	cert string
}
',
		'
module wrap

import dep

pub struct Config {
	dep.Config
}
',
		'
module app

import dep

pub struct RunParams {
pub:
	ssl_config dep.Config
}

fn ssl_enabled(params RunParams) bool {
	return params.ssl_config.cert != ""
}
',
	])
	assert csrc.contains('params.ssl_config.cert')
	assert !csrc.contains('params.ssl_config.Config.cert')
}

fn test_generate_c_does_not_default_pointer_to_map_with_map_value() {
	csrc := generate_c_for_test('
struct Context {
	custom_mime_types_ref &map[string]string = unsafe { nil }
	custom_mime_types     map[string]string
}

fn make() &Context {
	return &Context{}
}
')
	assert csrc.contains('.custom_mime_types = new_map(')
	assert !csrc.contains('.custom_mime_types_ref = new_map(')
}

fn test_generate_c_initializes_embedded_struct_map_defaults() {
	csrc := generate_c_for_test('
struct StaticHandler {
	static_files map[string]string
}

struct App {
	StaticHandler
	version string
}

fn make() &App {
	return &App{
		version: "dev"
	}
}
')
	assert csrc.contains('.StaticHandler.static_files = new_map(')
}

fn test_generate_c_initializes_imported_embedded_struct_map_defaults() {
	csrc := generate_c_for_test_files([
		'
module web

pub struct StaticHandler {
pub mut:
	static_files map[string]string
}
',
		'
module main

import web

struct App {
	web.StaticHandler
	version string
}

fn make() &App {
	return &App{
		version: "dev"
	}
}
',
	])
	assert csrc.contains('.StaticHandler.static_files = new_map(')
}

fn test_generate_c_declares_generic_struct_literal_with_caller_context_type() {
	csrc := generate_c_for_test_files([
		'
module veb

pub struct Context {}

pub fn make[X]() {
	mut user_context := X{}
	_ = user_context
}
',
		'
module main

import veb

pub struct Context {
	veb.Context
}

fn boot() {
	veb.make[Context]()
}
',
	])
	assert csrc.contains('Context user_context = ((Context)')
	assert !csrc.contains('veb__Context user_context = ((Context)')
}

fn test_generate_c_skips_promoted_embedded_defaults_when_embedded_field_is_initialized() {
	csrc := generate_c_for_test_files([
		'
module framework

pub struct Context {
pub mut:
	content_type string
}

pub fn make[X](ctx &Context) {
	mut user_context := X{
		Context: ctx
	}
	_ = user_context
}
',
		'
module main

import framework

pub struct Context {
	framework.Context
}

fn boot() {
	mut ctx := &framework.Context{}
	framework.make[Context](ctx)
}
',
	])
	mut user_context_line := ''
	for line in csrc.split('\n') {
		if line.contains('Context user_context =') {
			user_context_line = line
			break
		}
	}
	assert user_context_line.contains('Context user_context =')
	assert !user_context_line.contains(',.content_type =')
}

fn test_generate_c_initializes_nontrivial_globals_at_runtime() {
	csrc := generate_c_for_test('
@[has_globals]
module main

const default_name = "tenant_id"

struct Null {}

type Primitive = Null | int

const null_primitive = Primitive(Null{})

struct State {
mut:
	field_name string
	current    Primitive
}

__global state = State{
	field_name: default_name
	current: null_primitive
}

fn main() {
	_ = state.field_name
}
')
	assert csrc.contains('State state;')
	assert !csrc.contains('State state = ((State)')
	assert csrc.contains('void __v_init_consts_main()')
	assert csrc.contains('state = ((State){')
}

fn test_generate_c_initializes_const_map_literals_at_runtime() {
	csrc := generate_c_for_test("
const mime_types = {
	'.css': 'text/css'
	'.js':  'text/javascript'
}

fn main() {
	_ = mime_types
}
")
	assert csrc.contains('Map_string_string mime_types = {0};')
	assert csrc.contains('void __v_init_consts_main()')
	assert csrc.contains('mime_types = new_map_init_noscan_value')
}

fn test_generate_c_lowers_typeof_generic_idx_in_const_init() {
	csrc := generate_c_for_test('
const num64 = [typeof[i64]().idx, typeof[u64]().idx]
const type_string = typeof[string]().idx

fn main() {
	_ := num64.len + type_string
}
')
	assert !csrc.contains('typeof_T_')
	assert csrc.contains('{8, 13}')
	assert csrc.contains('20')
}

fn test_generate_c_passes_empty_array_for_empty_variadic_method_call() {
	csrc := generate_c_for_test('
struct Box {}
struct Err {}

fn (b Box) close(errs ...Err) {}

fn main() {
	b := Box{}
	b.close()
}
')
	assert !csrc.contains('Box__close(b);')
	assert csrc.contains('Box__close(b, ')
	assert csrc.contains('new_array_from_c_array(0, 0, sizeof(Err)')
}

fn test_generate_c_passes_empty_array_for_empty_channel_close() {
	csrc := generate_c_for_test('
fn main() {
	ch := chan bool{cap: 1}
	ch.close()
}
')
	assert !csrc.contains('((void(*)())ch->close)()')
	assert !csrc.contains('chan__close(ch);')
	assert !csrc.contains('sync__Channel__close(ch);')
	assert csrc.contains('sync__Channel__close(ch, ') || csrc.contains('chan__close(ch, ')
	assert csrc.contains('new_array_from_c_array(0, 0, sizeof(IError)')
}

fn test_generate_c_keeps_fixed_array_zero_global_static() {
	csrc := generate_c_for_test('
@[has_globals]
module main

const depth = 4

__global stack = [depth]int{}

fn main() {
	_ = stack[0]
}
')
	assert csrc.contains('Array_fixed_int_4 stack = {0};')
	assert !csrc.contains('memcpy(stack,')
	assert !csrc.contains('(array){0}')
}

fn test_generate_c_resolves_const_backed_fixed_array_global_extern_len() {
	csrc := generate_c_for_test('
@[has_globals]
module main

const depth = 4

__global (
	stack [depth]int
)

fn main() {
	_ = stack[0]
}
')
	assert csrc.contains('extern int stack[4];')
	assert !csrc.contains('extern int stack[depth];')
}

fn test_generate_c_uses_wrapped_fixed_array_return_for_fn_type_params() {
	csrc := generate_c_for_test('
fn call(cb fn () [4]u8) [4]u8 {
	return cb()
}

fn make() [4]u8 {
	return [4]u8{}
}

fn main() {
	_ = call(make)
}
')
	assert csrc.contains('_v_Array_fixed_u8_4 (*cb)(void)')
	assert !csrc.contains('call(Array_fixed_u8_4 (*cb)(void))')
}

fn test_generate_c_filters_lifetime_params_from_generic_struct_binding() {
	csrc := generate_c_for_test('
struct Value {
	n int
}

struct Ref[^a, T] {
	value T
}

struct Holder[^a] {
	item Ref[^a, Value]
}
')
	assert csrc.contains('struct Ref {')
	assert csrc.contains('Value value;')
	assert !csrc.contains('T value;')
}

fn test_generate_c_lowers_pointer_type_params_receivers_fields_and_generics() {
	csrc := generate_c_for_test('
struct Foo {
	value int
}

struct Node[T] {
	value T
}

struct Holder {
	item &Foo
	node &Node[Foo]
}

fn ptr_value(foo &Foo) int {
	return foo.value
}

fn (foo &Foo) method_value() int {
	return foo.value
}

fn main() {
	foo := Foo{}
	_ := ptr_value(&foo)
	_ := foo.method_value()
}
')
	assert csrc.contains('Foo* item;')
	assert csrc.contains('Node* node;')
	assert csrc.contains('Foo value;')
	assert csrc.contains('ptr_value(Foo* foo)')
	assert csrc.contains('Foo__method_value(Foo* foo)')
	assert !csrc.contains('int item;')
	assert !csrc.contains('int ptr_value(int foo)')
}

fn test_generate_c_match_on_enum_does_not_constrain_branch_array_literals() {
	csrc := generate_c_for_test('
enum Choice {
	color
	none
}

fn choices(id Choice) []string {
	return match id {
		.color { ["never", "auto"] }
		.none { []string{} }
	}
}
')
	assert csrc.contains('sizeof(string)')
	assert !csrc.contains('&(Choice[')
}

fn test_generate_c_struct_eq_recurses_into_nested_string_fields() {
	csrc := generate_c_for_test('
struct Encoding {
	label string
}

enum Kind {
	auto
	disabled
	some
}

struct Mode {
	kind     Kind = .auto
	encoding Encoding
}

fn same(a Mode, b Mode) bool {
	return a == b
}
')
	assert csrc.contains('string__eq(')
	assert csrc.contains('.encoding.label')
}

fn test_generate_c_emits_generic_method_helper_specialization_body() {
	csrc := generate_c_for_test('
struct Mapper {}

struct Schema {}

fn (mut m Mapper) helper[T]() int {
	return 1
}

fn (mut m Mapper) parse[T]() int {
	return m.helper[T]()
}

fn main() {
	mut m := Mapper{}
	_ = m.parse[Schema]()
}
')
	assert csrc.contains('int Mapper__parse_T_Schema(Mapper* m) {')
	assert csrc.contains('int Mapper__helper_T_Schema(Mapper* m) {')
}

fn test_generate_c_specializes_implicit_generic_function_calls_from_all_args() {
	csrc := generate_c_for_test('
struct Left {}
struct RightA {}
struct RightB {}

fn pair[T, U](x T, y U) int {
	_ = x
	_ = y
	return 1
}

fn main() {
	left := Left{}
	a := RightA{}
	b := RightB{}
	_ = pair(left, a)
	_ = pair(left, b)
}
')
	assert csrc.contains('int pair_T_Left_RightA(Left x, RightA y);')
	assert csrc.contains('int pair_T_Left_RightB(Left x, RightB y);')
	assert csrc.contains('pair_T_Left_RightA(left, a)')
	assert csrc.contains('pair_T_Left_RightB(left, b)')
	assert !csrc.contains('pair(left, a)')
	assert !csrc.contains('pair(left, b)')
}

fn test_generate_c_does_not_specialize_plain_generic_function_for_struct_fields() {
	csrc := generate_c_for_test('
enum Pattern {
	a
}

struct Thing {
	pattern Pattern
	named bool
}

fn (thing Thing) find_at() int {
	_ = thing
	return 1
}

fn find[M](matcher M) int {
	return matcher.find_at()
}

fn main() {
	thing := Thing{}
	_ = find(thing)
}
')
	assert csrc.contains('int find_T_Thing(Thing matcher) {')
	assert csrc.contains('return Thing__find_at(matcher);')
	assert !csrc.contains('find_T_Pattern')
	assert !csrc.contains('find_T_bool')
	assert !csrc.contains('Pattern__find_at')
	assert !csrc.contains('bool__find_at')
}

fn test_generate_c_specializes_nested_generic_calls_from_active_bindings() {
	csrc := generate_c_for_test('
struct Matcher {}
struct Captures {}

fn outer[M, T](matcher M, mut caps T) {
	inner(matcher, mut caps)
}

fn inner[M, T](matcher M, mut caps T) {
	_ = matcher
	_ = caps
}

fn main() {
	matcher := Matcher{}
	mut caps := Captures{}
	outer(matcher, mut caps)
}
')
	assert csrc.contains('void outer_T_Matcher_Captures(Matcher matcher, Captures* caps) {')
	assert csrc.contains('inner_T_Matcher_Captures(matcher, caps);')
	assert csrc.contains('void inner_T_Matcher_Captures(Matcher matcher, Captures* caps) {')
	assert !csrc.contains('inner(matcher, caps)')
}

fn test_generate_c_emits_value_receiver_methods_used_by_interface_wrappers() {
	csrc := generate_c_for_test('
interface Counter {
	len() int
}

struct Counts {}

fn (counts Counts) len() int {
	_ = counts
	return 1
}

fn use_counter(counter Counter) int {
	return counter.len()
}

fn main() {
	counts := Counts{}
	_ = use_counter(counts)
}
')
	assert csrc.contains('int Counts__len(Counts counts) {')
	assert csrc.contains('static int __iface_wrap_Counter_Counts_len(void* _obj) {')
	assert csrc.contains('return Counts__len(*(((Counts*)_obj)));')
}

fn test_generate_c_preserves_void_result_or_block_side_effects() {
	csrc := generate_c_for_test('
fn may_fail() ? {
	return none
}

fn main() {
	mut saw_error := false
	may_fail() or {
		saw_error = true
	}
	_ = saw_error
}
')
	assert csrc.contains('saw_error = true;')
}

fn test_generate_c_passes_mut_generic_param_address_as_existing_pointer() {
	csrc := generate_c_for_test('
struct Captures {}

fn visit[T](mut value T, cb fn (&T)) {
	cb(&value)
}

fn main() {
	mut captures := Captures{}
	visit(mut captures, fn (_captures &Captures) {})
}
')
	assert csrc.contains('((void (*)(Captures*))cb)(value);')
	assert !csrc.contains('cb(&value);')
}

fn test_generate_c_captures_mut_param_as_existing_pointer() {
	csrc := generate_c_for_test('
fn touch(mut dst []u8) {
	dst << u8(1)
}

fn outer(mut dst []u8) {
	cb := fn [mut dst] () {
		touch(mut dst)
	}
	cb()
}

fn main() {
	mut dst := []u8{}
	outer(mut dst)
}
')
	assert csrc.contains('_capture_0 = dst;')
	assert !csrc.contains('_capture_0 = &dst;')
}

fn test_generate_c_fn_literal_capture_preserves_fn_pointer_return_type() {
	csrc := generate_c_for_test('
fn visit(cb fn (int) bool) bool {
	return cb(1)
}

fn outer(matched fn (int) bool) bool {
	return visit(fn [matched] (n int) bool {
		return matched(n)
	})
}
')
	assert csrc.contains('static bool (*_anon_fn_')
	assert csrc.contains('bool (*matched)(int) = _anon_fn_')
	assert csrc.contains('return matched(n);')
	assert !csrc.contains('((void (*)(int))matched)(n)')
}

fn test_generate_c_rewrites_continue_in_generic_comptime_field_loop() {
	code := [
		'struct Schema {',
		'	skip int',
		'	keep int',
		'}',
		'',
		'fn count_fields[T]() int {',
		'	mut n := 0',
		'	@DLR@for field in T.fields {',
		'		if field.name == @SQ@skip@SQ@ {',
		'			continue',
		'		}',
		'		n++',
		'	}',
		'	return n',
		'}',
		'',
		'fn main() {',
		'	_ = count_fields[Schema]()',
		'}',
	].join('\n').replace('@DLR@', '$').replace('@SQ@', "'")
	csrc := generate_c_for_test(code)
	assert csrc.contains('goto __v_ctf_continue_')
	assert csrc.contains('__v_ctf_continue_')
}

fn test_generate_c_exposes_generic_comptime_field_attrs() {
	code := [
		'struct Schema {',
		'	field int @[repeats; short: u]',
		'}',
		'',
		'fn attrs[T]() []string {',
		'	mut out := []string{}',
		'	@DLR@for field in T.fields {',
		'		out = field.attrs',
		'	}',
		'	return out',
		'}',
		'',
		'fn main() {',
		'	_ = attrs[Schema]()',
		'}',
	].join('\n').replace('@DLR@', '$')
	csrc := generate_c_for_test(code)
	assert csrc.contains('"repeats"')
	assert csrc.contains('"short: u"')
}

fn test_generate_c_struct_default_does_not_write_string_value_to_pointer_field() {
	csrc := generate_c_for_test('
struct Builder[^a] {
	glob &^a string
}

fn make[^a]() &Builder[^a] {
	return &Builder[^a]{}
}

fn (mut b Builder[^a]) touch[^a]() &Builder[^a] {
	return &b
}
')
	assert csrc.contains('string* glob;')
	assert !csrc.contains('.glob = (string)')
}

fn test_generate_c_specializes_plain_method_on_each_generic_receiver_instance() {
	csrc := generate_c_for_test('
enum MatchKind {
	none
	hit
}

struct ValueA {}
struct ValueB {}

struct Match[T] {
	kind  MatchKind = .none
	value T
}

fn first() Match[ValueA] {
	return Match[ValueA]{
		kind: .hit
		value: ValueA{}
	}
}

fn second() Match[ValueB] {
	return Match[ValueB]{
		kind: .hit
		value: ValueB{}
	}
}

fn (m Match[T]) is_hit() bool {
	return m.kind == .hit
}

fn main() {
	_ = first().is_hit()
	_ = second().is_hit()
}
')
	assert csrc.contains('bool Match__is_hit(Match m) {')
	assert csrc.contains('bool Match_T_ValueB__is_hit(Match_T_ValueB m);')
	assert csrc.contains('bool Match_T_ValueB__is_hit(Match_T_ValueB m) {')
	assert csrc.contains('return ((Match_T_ValueB){.kind = MatchKind__hit')
	assert csrc.contains('Match__is_hit(first())')
	assert csrc.contains('Match_T_ValueB__is_hit(second())')
	assert !csrc.contains('Match__is_hit(second())')
}

fn test_generate_c_declares_generic_struct_literal_with_specialized_type() {
	csrc := generate_c_for_test('
struct Other {}
struct Value {}

struct Match[T] {
	value T
}

fn make_other() Match[Other] {
	return Match[Other]{}
}

fn make_match() Match[Value] {
	return Match[Value]{}
}

fn main() {
	_ = make_other()
	mut mat := Match[Value]{}
	mat = make_match()
}
')
	assert csrc.contains('Match_T_Value mat = ((Match_T_Value){0});')
	assert csrc.contains('mat = make_match();')
	assert !csrc.contains('Match mat = ((Match_T_Value){0});')
}

fn test_generate_c_declares_lifetime_generic_struct_literal_with_specialized_type() {
	csrc := generate_c_for_test('
struct Other {}
struct IgnoreMatch[^a] {}

struct Match[T] {
	value T
}

fn make_other() Match[Other] {
	return Match[Other]{}
}

fn matched[^a]() Match[IgnoreMatch[^a]] {
	_ = make_other()
	mut mat := Match[IgnoreMatch[^a]]{}
	return mat
}

fn main() {
	_ = matched()
	}
	')
	assert csrc.contains('Match_T_IgnoreMatch matched()')
	assert !csrc.contains('\tMatch mat = ((Match_T_IgnoreMatch){0});')
}

fn test_generate_c_uses_specialized_lifetime_generic_type_for_if_expr_and_array_literal() {
	csrc := generate_c_for_test('
struct Other {}
struct IgnoreMatch[^a] {}

struct Match[T] {
	value T
}

fn make_other() Match[Other] {
	return Match[Other]{}
}

fn make_match[^a]() Match[IgnoreMatch[^a]] {
	return Match[IgnoreMatch[^a]]{}
}

fn matched[^a](flag bool) Match[IgnoreMatch[^a]] {
	_ = make_other()
	a := make_match()
	b := make_match()
	mut selected := if flag {
		make_match()
	} else {
		Match[IgnoreMatch[^a]]{}
	}
	for mat in [a, b, selected] {
		return mat
	}
	return Match[IgnoreMatch[^a]]{}
}
')
	assert csrc.contains('Match_T_IgnoreMatch selected = ({ Match_T_IgnoreMatch _if_expr_t')
	assert csrc.contains('new_array_from_c_array(3, 3, sizeof(Match_T_IgnoreMatch)')
	assert csrc.contains('Match_T_IgnoreMatch mat = ')
	assert !csrc.contains('new_array_from_c_array(3, 3, sizeof(int)')
}

fn test_generate_c_registers_tuple_match_expr_destructure_type() {
	csrc := generate_c_for_test('
enum Encoding {
	zstd
	gzip
}

fn zstd_bytes() []u8 {
	return [u8(1)]
}

fn gzip_bytes() []u8 {
	return [u8(2)]
}

fn selected(encoding Encoding) string {
	compressed, ext, encoding_name := match encoding {
		.zstd {
			c := zstd_bytes()
			c, ".zst", "zstd"
		}
		.gzip {
			c := gzip_bytes()
			c, ".gz", "gzip"
		}
	}
	_ = compressed
	return ext + encoding_name
}
')
	assert csrc.contains('Tuple_Array_u8_string_string _tuple_tmp_')
	assert csrc.contains('Tuple_Array_u8_string_string _if_expr_t')
	assert csrc.contains('_if_expr_t') && csrc.contains(' = ((Tuple_Array_u8_string_string){')
	assert !csrc.contains('int _if_expr_t')
}

fn test_scan_expr_registers_array_tuple_alias_before_type_emit() {
	mut gen := Gen.new([])
	gen.runtime_local_types['c'] = 'Array_u8'
	gen.emitted_types['body_array'] = true
	gen.emitted_types['body_string'] = true
	gen.scan_expr_for_generic_types(ast.Expr(ast.Tuple{
		exprs: [
			ast.Expr(ast.Ident{
				name: 'c'
			}),
			ast.Expr(ast.StringLiteral{
				value: '".gz"'
			}),
			ast.Expr(ast.StringLiteral{
				value: '"gzip"'
			}),
		]
	}))
	gen.emit_tuple_aliases()
	assert gen.tuple_aliases['Tuple_Array_u8_string_string'] == ['Array_u8', 'string', 'string']
	assert gen.sb.str().contains('typedef struct Tuple_Array_u8_string_string')
}

fn test_generate_c_emits_late_generic_tuple_alias_before_forward_decl() {
	mut gen := Gen.new([])
	gen.emitted_types['body_array'] = true
	gen.tuple_aliases['Tuple_Array_Foo_Array_Foo'] = ['Array_Foo', 'Array_Foo']
	gen.fn_return_types['arrays__partition_T_Foo'] = 'Tuple_Array_Foo_Array_Foo'
	gen.gen_fn_head_with_name(ast.FnDecl{
		name: 'partition'
	}, 'arrays__partition_T_Foo')
	gen.sb.writeln(';')
	csrc := gen.sb.str()
	tuple_decl_idx := csrc.index('typedef struct Tuple_Array_Foo_Array_Foo') or {
		assert false
		return
	}
	partition_decl_idx := csrc.index('Tuple_Array_Foo_Array_Foo arrays__partition_T_Foo') or {
		assert false
		return
	}
	assert tuple_decl_idx < partition_decl_idx
}

fn test_generate_c_keeps_imported_module_generic_call_name() {
	csrc := generate_c_for_test_files([
		'
module arrays

pub fn uniq[T](a []T) []T {
	return a
}
',
		'
module http

import arrays

	pub fn keys(res []string) []string {
	return arrays.uniq(res)
	}
',
	])
	assert csrc.contains('Array_string arrays__uniq_T_string(Array_string a)')
	assert csrc.contains('return arrays__uniq_T_string(res);')
	assert !csrc.contains('http__uniq_T_string')
}

fn test_generate_c_prefers_local_nongeneric_function_over_external_generic_short_name() {
	csrc := generate_c_for_test('
module strings

fn min(a int, b int, c int) int {
	return a
}

pub fn pick(a int, b int, c int) int {
	return min(a, b, c)
}
')
	assert csrc.contains('return strings__min(a, b, c);')
	assert !csrc.contains('min_T_int')
}

fn test_generate_c_fixed_array_index_expr_has_element_type() {
	csrc := generate_c_for_test('
const month_days = [31, 28, 31]!

fn max_day(month int) int {
	value := month_days[month - 1] + 1
	return value
}
')
	assert csrc.contains('static const int month_days[3] = {31, 28, 31};')
	assert !csrc.contains('static const int month_days[3] = (int[3])')
	assert csrc.contains('int value = (month_days')
	assert !csrc.contains('fixed_int_3 value')
}

fn test_generate_c_resolves_const_fixed_array_struct_field_len() {
	csrc := generate_c_for_test('
const max_connection_size = 65536
const buf_size = max_connection_size

struct Conn {
	read_buf [buf_size]u8
}
')
	assert csrc.contains('u8 read_buf[65536];')
	assert !csrc.contains('u8 read_buf[buf_size];')
}

fn test_generate_c_emits_late_generic_map_alias_before_struct_body() {
	prefs := &vpref.Preferences{
		backend: .cleanc
	}
	env := types.Environment.new()
	mut gen := Gen.new_with_env_and_pref([]ast.File{}, env, prefs)
	gen.emit_map_alias_decl('Map_Map_string_string_u8')
	gen.emit_map_alias_decl('Map_Map_string_string_u8')
	gen.emit_map_alias_decl('Map_string_veb__Route*')
	gen.emit_map_alias_decl('Map_string_veb__Route*')
	csrc := gen.sb.str()
	assert csrc.count('typedef map Map_Map_string_string_u8;') == 1
	assert csrc.contains('string Map_Map_string_string_u8_str(Map_Map_string_string_u8 m);')
	assert csrc.contains('bool Map_Map_string_string_u8_map_eq(Map_Map_string_string_u8 a, Map_Map_string_string_u8 b);')
	assert csrc.count('typedef map Map_string_veb__Route;') == 1
	assert !csrc.contains('typedef map Map_string_veb__Route*;')
}

fn test_parse_map_kv_types_prefers_known_nested_map_alias_key() {
	mut gen := Gen.new([])
	gen.map_aliases['Map_string_string'] = true
	key_type, value_type := gen.parse_map_kv_types('Map_string_string_Array_int')
	assert key_type == 'Map_string_string'
	assert value_type == 'Array_int'
}

fn test_emit_map_str_uses_pointer_for_fixed_array_key() {
	mut gen := Gen.new([])
	gen.sb = strings.new_builder(1024)
	gen.map_aliases['Map_Array_fixed_u8_4_int'] = true
	gen.emit_map_str_functions()
	csrc := gen.sb.str()
	assert csrc.contains('u8* key_ptr = (u8*)DenseArray__key(&m.key_values, i);')
	assert !csrc.contains('Array_fixed_u8_4 key = *')
}

fn test_parse_map_kv_types_prefers_longest_known_generic_key() {
	mut gen := Gen.new([])
	gen.emitted_types['body_api__ApiSuccessResponse'] = true
	gen.emitted_types['body_api__ApiSuccessResponse_T_Array_FileInfo'] = true
	key_type, value_type :=
		gen.parse_map_kv_types('api__ApiSuccessResponse_T_Array_FileInfo_Array_int')
	assert key_type == 'api__ApiSuccessResponse_T_Array_FileInfo'
	assert value_type == 'Array_int'
}

fn test_map_alias_filter_accepts_module_qualified_key_type() {
	mut gen := Gen.new([])
	gen.cache_bundle_name = 'v2compiler'
	gen.emit_modules['ssa'] = true
	assert gen.alias_type_belongs_to_emit_modules('Map_ssa__TypeID_bool')
	prefixes := gen.module_prefixes_in_c_name('Map_ssa__TypeID_bool')
	assert prefixes == ['ssa']
}

fn test_generate_c_qualifies_imported_symbol_type() {
	prefs := &vpref.Preferences{
		backend: .cleanc
	}
	env := types.Environment.new()
	file := ast.File{
		name:    'imported_symbol_type_test.v'
		mod:     'markdown'
		imports: [
			ast.ImportStmt{
				name:    'datatypes'
				alias:   'datatypes'
				symbols: [ast.Expr(ast.Ident{
					name: 'Stack'
				})]
			},
		]
	}
	mut gen := Gen.new_with_env_and_pref([file], env, prefs)
	gen.cur_file_name = file.name
	gen.cur_module = file.mod
	assert gen.expr_type_to_c(ast.Ident{
		name: 'Stack'
	}) == 'datatypes__Stack'
	assert gen.expr_type_to_c(ast.Ident{
		name: 'markdown__Stack'
	}) == 'datatypes__Stack'
}

fn test_generate_c_indexes_mut_array_receiver_through_pointer_data() {
	csrc := generate_c_for_test('
fn (mut a []string) touch_each() {
	for mut s in a {
		s = s.clone()
	}
}

fn main() {
	mut items := ["x"]
	items.touch_each()
}
	')
	assert csrc.contains('touch_each')
	assert !csrc.contains('(a).data')
}

fn test_generate_c_indexes_variadic_string_param_as_array() {
	csrc := generate_c_for_test('
fn use_patterns(patterns ...string) {
	for pattern in patterns {
		_ = pattern
	}
}

fn main() {
	use_patterns("*.v")
}
')
	assert csrc.contains('use_patterns')
	assert !csrc.contains('(patterns).str')
}

fn test_generate_c_drops_leaked_static_type_receiver_in_constructor_call() {
	csrc := generate_c_for_test('
struct Match {}

fn Match.new(start int, end int) Match {
	_ = start
	_ = end
	return Match{}
}

fn build(end int) []Match {
	return [Match.new(0, end)]
}
')
	assert csrc.contains('Match__new(0, end)')
	assert !csrc.contains('Match__new(Match,')
}

fn test_generate_c_lowers_map_literal_for_in_before_array_fallback() {
	csrc := generate_c_for_test("
fn collect() []string {
	mut res := []string{}
	for label, v in {
		'days': 24
		'h': 1
	} {
		res << '\${v}\${label}'
	}
	return res
}
")
	assert csrc.contains('DenseArray__key')
	assert csrc.contains('string label =')
	assert !csrc.contains('for (int label = 0;')
	assert !csrc.contains('+ label))')
}

fn test_generate_c_lowers_map_index_assign_after_empty_map_literal_decl() {
	csrc := generate_c_for_test('
struct Inst {
	typ    Kind
	target int
}

enum Kind {
	split
	jmp
	other
}

struct Compiler {
mut:
	prog []Inst
}

fn (mut c Compiler) mark() {
	mut targets := map[int]bool{}
	for inst in c.prog {
		if inst.typ == .split || inst.typ == .jmp {
			targets[inst.target] = true
		}
	}
}
')
	assert csrc.contains('map__set(&targets')
	assert !csrc.contains('cannot resolve map type for index expr')
}

fn test_generate_c_lowers_late_generic_selector_string_slice_and_map_assign() {
	csrc := generate_c_for_test('
struct Info {
	position int
	length   int
}

struct Decoder {
	json string
}

fn (mut decoder Decoder) decode_map[V](mut val map[string]V) {
	key_info := Info{
		position: 0
		length: 3
	}
	key_str := decoder.json[key_info.position + 1..key_info.position + key_info.length - 1]
	mut map_value := V{}
	val[key_str] = map_value
}

fn main() {
	mut decoder := Decoder{
		json: "\'x\'"
	}
	mut vals := map[string]string{}
	decoder.decode_map(mut vals)
}
')
	assert csrc.contains('string key_str = string__substr')
	assert csrc.contains('map__set(')
	assert !csrc.contains('array key_str = array__slice(decoder->json')
	assert !csrc.contains('map__get(&(val)')
}

fn test_generate_c_specializes_comptime_field_generic_call_by_field_type() {
	csrc := generate_c_for_test('
fn struct_field_should_encode[T](val T) bool {
	_ = val
	return true
}

fn encode_fields[T](val T) bool {
	mut ok := true
	$for field in T.fields {
		ok = struct_field_should_encode(val.$(field.name))
	}
	return ok
}

struct Repo {
	id          int
	unix        i64
	description string
}

fn main() {
	_ = struct_field_should_encode(1)
	_ = encode_fields(Repo{
		id: 1
		unix: 2
		description: "x"
	})
}
')
	assert csrc.contains('struct_field_should_encode_T_string(val.description)')
	assert csrc.contains('val._unix')
	assert !csrc.contains('val.unix')
	assert !csrc.contains('struct_field_should_encode_T_Repo(val.description)')
}

fn test_generate_c_uses_statement_temps_for_nested_map_index_assign_defaults() {
	csrc := generate_c_for_test("
fn build() map[string]map[string]string {
	mut res := map[string]map[string]string{}
	lang := 'en'
	key := 'msg'
	val := 'Hello'
	res[lang][key] = val
	return res
}
")
	assert csrc.contains('map__get_and_set(&res')
	assert !csrc.contains('map__get_and_set(&res, ((void*)(&lang)), ((void*)(({')
}

fn test_generate_c_lowers_mut_array_for_in_after_cap_only_array_literal_decl() {
	csrc := generate_c_for_test('
struct Inst {
mut:
	n int
}

fn rewrite() []Inst {
	mut new_prog := []Inst{cap: 4}
	new_prog << Inst{
		n: 1
	}
	for mut inst in new_prog {
		inst.n = 2
	}
	return new_prog
}
')
	assert csrc.contains('Inst* inst')
	assert !csrc.contains('for (; ; )')
}

fn test_generate_c_uses_string_substr_for_array_string_index_slice() {
	csrc := generate_c_for_test("
const names = ['January']!

fn short() string {
	return names[0][0..3]
}

fn main() {
	_ = short()
}
")
	assert csrc.contains('string__substr(')
	assert !csrc.contains('array__slice(names')
}

fn test_array_append_elem_type_accepts_runtime_array_pointer() {
	mut gen := Gen.new([])
	gen.runtime_local_types['field_infos'] = 'array*'
	is_append, elem_type := gen.array_append_elem_type(ast.Expr(ast.Ident{
		name: 'field_infos'
	}), ast.Expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'FieldInfo'
		}
	}))
	assert is_append
	assert elem_type == 'FieldInfo'
}

fn test_generate_c_lowers_map_field_for_in_with_ignored_key() {
	csrc := generate_c_for_test('
struct Def {
	name string
}

struct Builder {
	types map[string]Def
}

fn collect(builder Builder) []Def {
	mut defs := []Def{}
	for _, def in builder.types {
		defs << def
	}
	return defs
}

fn main() {
	_ = collect(Builder{
		types: map[string]Def{}
	})
}
')
	assert csrc.contains('DenseArray__value')
	assert !csrc.contains('cannot resolve map type for index expr')
}

fn test_generate_c_uses_string_methods_after_branch_assignment() {
	csrc := generate_c_for_test('
fn count_matches(files []string, pat string) int {
	mut count := 0
	for file in files {
		mut f := ""
		if file.contains("/") {
			parts := file.split("/")
			f = parts[parts.len - 1]
		} else {
			f = file
		}
		if f.starts_with(pat) || f.ends_with(pat) {
			count++
		}
	}
	return count
}

fn main() {
	_ = count_matches(["abc"], "a")
}
')
	assert csrc.contains('string__starts_with(f, pat)')
	assert csrc.contains('string__ends_with(f, pat)')
	assert !csrc.contains('int__starts_with')
	assert !csrc.contains('int__ends_with')
}

fn test_generate_c_lowers_sort_comparator_on_array_selector() {
	csrc := generate_c_for_test('
struct Def {
mut:
	globs []string
}

fn ordered(def Def) Def {
	mut cloned := def
	cloned.globs.sort(a < b)
	return cloned
}

fn main() {
	_ = ordered(Def{})
}
')
	assert csrc.contains('array__sort_with_compare')
	assert csrc.contains('compare_strings')
	assert !csrc.contains('array__sort(&cloned.globs, (a < b))')
}

fn test_generate_c_emits_sort_comparator_for_file_filtered_main() {
	csrc := generate_c_for_test_sources_with_emit([
		CgenTestSource{
			path: 'cached/issue.v'
			code: '
module main

struct Issue {
	created_at int
}

fn cached_route(mut issues []Issue) {
	issues.sort(a.created_at > b.created_at)
}
'
		},
		CgenTestSource{
			path: 'issue.v'
			code: '
module main

fn root_route(mut issues []Issue) {
	issues.sort(a.created_at > b.created_at)
}

fn main() {
	mut issues := []Issue{}
	root_route(mut issues)
}
'
		},
	], ['issue.v'])
	assert csrc.contains('array__sort_with_compare(&issues, __sort_cmp_Issue_by_created_at_desc)')
	assert csrc.contains('int __sort_cmp_Issue_by_created_at_desc(Issue* a, Issue* b);')
	assert csrc.contains('int __sort_cmp_Issue_by_created_at_desc(Issue* a, Issue* b) {')
}

fn test_generate_c_uses_string_methods_after_if_expr_assignment() {
	csrc := generate_c_for_test('
fn count_matches(files []string, dir string, pat string) int {
	mut count := 0
	for file in files {
		mut fpath := file
		f := if file.contains("/") {
			pathwalk := file.split("/")
			pathwalk[pathwalk.len - 1]
		} else {
			fpath = if dir == "." { file } else { dir + "/" + file }
			file
		}
		if f.starts_with(pat) || f.ends_with(pat) || f.contains(pat) {
			count++
		}
		_ = fpath
	}
	return count
}

fn main() {
	_ = count_matches(["abc"], ".", "a")
}
')
	assert csrc.contains('string__starts_with(f, pat)')
	assert csrc.contains('string__ends_with(f, pat)')
	assert csrc.contains('string__contains(f, pat)')
	assert !csrc.contains('int__starts_with')
	assert !csrc.contains('int__ends_with')
	assert !csrc.contains('int__contains')
}

fn test_generate_c_lowers_promoted_embedded_method_receiver() {
	csrc := generate_c_for_test('
struct Base {}

fn (mut base Base) get_cookie(key string) ?string {
	return key
}

struct Context {
	Base
}

fn read_cookie(mut ctx Context) string {
	return ctx.get_cookie("session") or { "" }
}

fn main() {
	mut ctx := Context{}
	_ = read_cookie(mut ctx)
}
')
	assert csrc.contains('Base__get_cookie(&ctx->Base,')
	assert !csrc.contains('Context__get_cookie(ctx')
}

fn test_generate_c_lowers_module_qualified_static_type_method() {
	csrc := generate_c_for_test_files([
		'
module git

pub struct Git {}

pub fn Git.exec(args []string) int {
	_ = args
	return 0
}
',
		'
module main

import git

fn main() {
	_ = git.Git.exec(["status"])
}
',
	])
	assert csrc.contains('git__Git__exec(new_array_from_c_array')
	assert !csrc.contains('git__Git.exec')
	assert !csrc.contains('git__Git, new_array_from_c_array')
}

fn test_generate_c_adds_implicit_veb_context_param() {
	csrc := generate_c_for_test_files([
		'
module veb

pub struct Result {}

pub struct Context {}

pub fn (mut ctx Context) redirect(path string) Result {
	_ = path
	return Result{}
}
',
		'
module main

import veb

struct App {}

struct Context {
	veb.Context
}

pub fn (mut app App) index() veb.Result {
	return ctx.redirect("/register")
}
',
	])
	assert csrc.contains('veb__Result App__index(App* app, Context* ctx)')
	assert csrc.contains('veb__Context__redirect(&ctx->Context,')
	assert !csrc.contains('veb__Result App__index(App* app) {')
}

fn test_generate_c_passes_implicit_veb_context_to_handler_calls() {
	csrc := generate_c_for_test_files([
		'
module veb

pub struct Result {}

pub struct Context {}
',
		'
module main

import veb

struct App {}

struct Context {
	veb.Context
}

pub fn (mut app App) repo_settings(username string) veb.Result {
	_ = username
	return veb.Result{}
}

pub fn (mut app App) handle(username string) veb.Result {
	return app.repo_settings(mut ctx, username)
}
',
	])
	assert csrc.contains('veb__Result App__repo_settings(App* app, Context* ctx, string username)')
	assert csrc.contains('App__repo_settings(app, ctx, username)')
	assert !csrc.contains('App__repo_settings(app, (*ctx), username)')
}

fn test_generate_c_lowers_promoted_embedded_generic_method_receiver() {
	csrc := generate_c_for_test_files([
		'
module veb

pub struct Result {}

pub struct Context {}

pub fn (mut ctx Context) json[T](j T) Result {
	_ = j
	return Result{}
}
',
		'
module main

import veb

struct Context {
	veb.Context
}

fn handle(mut ctx Context) veb.Result {
	return ctx.json("ok")
}
',
	])
	assert csrc.contains('veb__Context__json_T_string(&ctx->Context,')
	assert !csrc.contains('ctx->json')
}

fn test_generate_c_lowers_promoted_embedded_generic_method_call() {
	csrc := generate_c_for_test_files([
		'
module dep

pub struct Options[T] {
pub:
	handler fn (mut ctx T) bool @[required]
}

pub struct Middleware[T] {
mut:
	handlers []voidptr
}

pub fn (mut m Middleware[T]) use(options Options[T]) {
	_ = T.name
	m.handlers << voidptr(options.handler)
}
',
		'
module main

import dep

struct App {
	dep.Middleware[Context]
}

struct Context {}

fn (mut app App) before_request(mut ctx Context) bool {
	_ = ctx
	return true
}

fn main() {
	mut app := App{}
	app.use(handler: app.before_request)
}
',
	])
	assert csrc.contains('dep__Middleware__use(&app.Middleware,')
	assert csrc.contains('((void*)_bound_method_')
	assert !csrc.contains('app.use')
	assert !csrc.contains('app.before_request')
}

fn test_generate_c_infers_generic_type_name_selector_as_string() {
	csrc := generate_c_for_test('
struct Table {
	name string
}

struct GitHubRepoInfo {}
struct GitHubContributor {}
struct GitHubIssue {}

enum Lang {
	en
}

fn table_from_struct[T]() Table {
	mut table_name := T.name
	if bracket_pos := table_name.index("[") {
		table_name = table_name[..bracket_pos]
	}
	table_name = table_name.to_lower()
	return Table{
		name: table_name
	}
}

fn boot() {
	_ = table_from_struct[GitHubRepoInfo]()
	_ = table_from_struct[[]GitHubContributor]()
	_ = table_from_struct[[]GitHubIssue]()
	_ = table_from_struct[map[string]string]()
	_ = table_from_struct[Lang]()
}
')
	assert csrc.contains('string table_name = (string){.str = "GitHubRepoInfo"')
	assert csrc.contains('string table_name = (string){.str = "[]GitHubContributor"')
	assert csrc.contains('string table_name = (string){.str = "[]GitHubIssue"')
	assert csrc.contains('string table_name = (string){.str = "map[string]string"')
	assert csrc.contains('string table_name = (string){.str = "Lang"')
	assert csrc.contains('string__index(table_name')
	assert csrc.contains('_option_int _or_t')
	assert csrc.contains('.state == 0')
	assert csrc.contains('int bracket_pos =')
	assert csrc.contains('string__to_lower(table_name)')
	assert !csrc.contains('if (string__index(table_name')
	assert !csrc.contains('_option_int bracket_pos = string__index(table_name')
	assert !csrc.contains('GitHubRepoInfo table_name = (string)')
	assert !csrc.contains('Array_GitHubContributor table_name = (string)')
	assert !csrc.contains('Array_GitHubIssue table_name = (string)')
	assert !csrc.contains('Map_string_string table_name = (string)')
	assert !csrc.contains('Lang__str(T)')
}

fn test_generate_c_lowers_comptime_field_metadata_values() {
	csrc := generate_c_for_test('
struct TableField {
	typ int
}

const time_ = -2
const enum_ = -3
const type_idx = {
	"int": 7
}

struct Item {
	name string
	count int
}

fn struct_meta[T]() []TableField {
	mut meta := []TableField{}
	$for field in T.fields {
		if !field.is_embed {
			mut field_type := field.typ
			if typeof(field).name.contains("time.Time") {
				field_type = time_
			} else if field.is_struct {
				field_type = type_idx["int"]
			} else if field.is_enum {
				field_type = enum_
			}
			meta << TableField{
				typ: field_type
			}
		}
	}
	return meta
}

fn boot() {
	_ = struct_meta[Item]()
}
')
	assert csrc.contains('int field_type = 20')
	assert csrc.contains('int field_type = 7')
	assert csrc.contains('if (!false)')
	assert !csrc.contains('string__contains((string){.str = "field"')
	assert !csrc.contains('field.is_embed')
	assert !csrc.contains('field.is_struct')
	assert !csrc.contains('field.is_enum')
	assert !csrc.contains('string field_type =')
}

fn test_generate_c_mangles_unimported_module_const_in_embedded_struct_default() {
	csrc := generate_c_for_test_files([
		'
module dep

pub const default_timeout = 100

pub struct Config {
pub:
	read_timeout int = default_timeout
}
',
		'
module wrap

import dep

pub struct Config {
	dep.Config
}
',
		'
module main

import wrap

fn make() wrap.Config {
	return wrap.Config{}
}
',
	])
	assert csrc.contains('.Config.read_timeout = dep__default_timeout')
	assert !csrc.contains('dep.default_timeout')
}

fn test_generate_c_does_not_rewrap_module_qualified_explicit_sumtype_cast_in_result_return() {
	csrc := generate_c_for_test_files([
		'
module orm

pub struct Null {}

pub type Primitive = Null | bool | []Primitive
',
		'
module pg

import orm

fn make() !orm.Primitive {
	return orm.Primitive(true)
}
',
	])
	assert csrc.contains('orm__Primitive _val = ((orm__Primitive){._tag = 1,._data._bool')
	assert !csrc.contains('orm__Primitive _val = ((orm__Primitive){._tag = 2,._data._Array_orm__Primitive')
}

fn test_generate_c_uses_qualified_array_payload_for_module_sumtype_as_cast() {
	csrc := generate_c_for_test_files([
		'
module orm

pub struct Null {}

pub type Primitive = Null | bool | []Primitive

fn primitive_kind(value Primitive) int {
	return match value {
		Null {
			0
		}
		bool {
			1
		}
		[]Primitive {
			if value.len > 0 {
				primitive_kind(value[0])
			} else {
				0
			}
		}
	}
}
',
	])
	assert csrc.contains('value._data._Array_orm__Primitive')
	assert !csrc.contains('value._data._orm__Array_Primitive')
	assert !csrc.contains('Array_Primitive*')
	assert !csrc.contains('(Array_Primitive){0}')
}

fn test_generate_c_dereferences_heap_float_sumtype_payload_casts() {
	csrc := generate_c_for_test('
struct Null {}

type Primitive = Null | f32 | f64 | int

fn as_int(data Primitive) int {
	return match data {
		Null {
			0
		}
		f32 {
			int(f32(data))
		}
		f64 {
			int(f64(data))
		}
		int {
			data
		}
	}
}
')
	assert csrc.contains('*((f32*)(data._data._f32))')
	assert csrc.contains('*((f64*)(data._data._f64))')
	assert !csrc.contains('(f32)(data._data._f32)')
	assert !csrc.contains('(f64)(data._data._f64)')
}

fn test_generate_c_specializes_generic_call_from_array_generic_param() {
	csrc := generate_c_for_test_files([
		'
module orm

pub struct Null {}

pub type Primitive = Null | []bool
',
		'
module pg

import orm

fn handle_array[T](data []T) {
	_ = data
}

fn handle(data orm.Primitive) {
	match data {
		orm.Null {}
		[]bool {
			handle_array(data)
		}
	}
}
',
	])
	assert csrc.contains('pg__handle_array_T_bool(')
	assert !csrc.contains('pg__handle_array(data')
}

fn test_generate_c_uses_expected_array_payload_for_sumtype_generic_call() {
	csrc := generate_c_for_test_files([
		'
module time

pub struct Time {}
',
		'
module orm

import time

pub struct Null {}
pub struct InfixType {}

pub type Primitive = Null | time.Time | []time.Time | []InfixType

fn array_kind[T](value []T) int {
	return value.len
}

fn primitive_kind(value Primitive) int {
	return match value {
		Null {
			0
		}
		time.Time {
			1
		}
		[]time.Time {
			array_kind(value)
		}
		[]InfixType {
			array_kind(value)
		}
	}
}
',
	])
	assert csrc.contains('orm__array_kind_T_time_Time(')
	assert !csrc.contains('orm__array_kind_T_time_Time(((((time__Time*)')
	assert csrc.contains('orm__array_kind_T_orm_InfixType(')
	assert !csrc.contains('orm__array_kind(((((Array_orm__InfixType*)')
}

fn test_generate_c_keeps_builtin_copy_unspecialized() {
	csrc := generate_c_for_test_files([
		'
module arrays

pub fn copy[T](mut dst []T, src []T) int {
	_ = dst
	_ = src
	return 0
}
',
		'
module main

import arrays

fn copy(mut dst []u8, src []u8) int {
	_ = dst
	_ = src
	return 0
}

fn copy_bytes(mut dst []u8, src []u8) int {
	return copy(mut dst, src)
}
',
	])
	assert csrc.contains('return copy(dst, src)')
	assert !csrc.contains('return copy_T_u8')
}

fn test_generate_c_resolves_private_method_when_public_method_shares_suffix() {
	csrc := generate_c_for_test('
module os

struct Process {}

fn (mut p Process) is_pending() bool {
	return p._is_pending()
}

fn (mut p Process) _is_pending() bool {
	return true
}
')
	assert csrc.contains('return os__Process___is_pending(p);')
	assert !csrc.contains('return os__Process__is_pending(p);')
}

fn test_should_emit_builtin_print_backtrace_with_markused() {
	env := types.Environment.new()
	gen := Gen{
		env:          env
		used_fn_keys: {
			'other': true
		}
	}
	assert gen.should_emit_fn_decl('builtin', ast.FnDecl{
		name: 'print_backtrace'
	})
}

fn test_should_emit_time_duration_methods_with_markused() {
	env := types.Environment.new()
	gen := Gen{
		env:          env
		used_fn_keys: {
			'other': true
		}
	}
	assert gen.should_emit_fn_decl('time', ast.FnDecl{
		name:      'microseconds'
		is_method: true
		receiver:  ast.Parameter{
			name: 'd'
			typ:  ast.Expr(ast.Ident{
				name: 'Duration'
			})
		}
	})
}

fn test_should_emit_sync_channel_helpers_with_markused() {
	env := types.Environment.new()
	gen := Gen{
		env:          env
		used_fn_keys: {
			'other': true
		}
	}
	for helper in ['try_pop_priv', 'try_push_priv', 'new_channel_st', 'new_spin_lock'] {
		assert gen.should_emit_fn_decl('sync', ast.FnDecl{
			name: helper
		})
	}
	assert gen.should_emit_fn_decl('sync', ast.FnDecl{
		name:      'try_wait'
		is_method: true
	})
}

fn test_generate_c_specializes_bare_generic_placeholder_call() {
	csrc := generate_c_for_test('
module datatypes

@[heap]
struct Node[T] {
	value T
	next &Node[T] = unsafe { 0 }
}

fn none_node[T](init bool) &Node[T] {
	return &Node[T]{
		value: T{}
	}
}

fn root_node[T](value T) &Node[T] {
	return &Node[T]{
		value: value
		next: none_node[T](false)
	}
}

fn use() {
	_ = root_node[f64](1.0)
}
')
	assert csrc.contains('datatypes__none_node_T_f64(false)')
	assert !csrc.contains('datatypes__none_node_T(false)')
}

fn test_generate_c_resolves_short_module_generic_token() {
	csrc := generate_c_for_test('
module orm

struct InfixType {}

type Primitive = []InfixType | int

fn array_primitive[T](value []T) int {
	return value.len
}

fn primitive(value Primitive) int {
	if value is []InfixType {
		return array_primitive(value)
	}
	return 0
}
')
	assert csrc.contains('orm__array_primitive_T_orm_InfixType')
	assert !csrc.contains('return array_primitive_T_InfixType')
}

fn test_generate_c_appends_for_in_pointer_value_to_array() {
	csrc := generate_c_for_test('
struct PullRequest {
	id int
}

fn use(prs []PullRequest) []PullRequest {
	mut out := []PullRequest{}
	for mut pr in prs {
		pr.id = pr.id + 1
		out << pr
	}
	return out
}
')
	assert csrc.contains('array__push(((array*)(&out)), pr);')
	assert !csrc.contains('&(PullRequest[1]){pr}')
}

fn test_generate_c_uses_concrete_generic_value_for_sumtype_cast_in_for_in() {
	csrc := generate_c_for_test_files([
		'
module orm

pub struct Null {}

pub type Primitive = Null | bool | string

pub fn primitive_array[T](values []T) []Primitive {
	mut out := []Primitive{cap: values.len}
	for value in values {
		out << Primitive(value)
	}
	return out
}
',
		'
module main

import orm

fn main() {
	_ = orm.primitive_array([true])
	_ = orm.primitive_array(["x"])
}
',
	])
	start := csrc.index('Array_orm__Primitive orm__primitive_array_T_string(Array_string values) {') or {
		panic('missing string specialization')
	}
	tail := csrc[start..]
	end := tail.index('\n}\n') or { panic('missing string specialization end') }
	body := tail[..end]
	assert body.contains('._data._string')
	assert !body.contains('._data._bool')
}

fn test_generate_c_uses_specialized_receiver_for_generic_method_body_call() {
	mut gen := Gen{
		sb:                  strings.new_builder(64)
		fn_return_types:     {
			'Box_T_f64__get_value': 'f64'
		}
		fn_param_is_ptr:     {
			'Box_T_f64__get_value': [true]
		}
		fn_param_types:      {
			'Box_T_f64__get_value': ['Box_T_f64*']
		}
		runtime_local_types: {
			'b': 'Box_T_f64*'
		}
	}
	gen.call_expr(ast.Ident{
		name: 'Box__get_value'
	}, [
		ast.Expr(ast.Ident{
			name: 'b'
		}),
	])
	assert gen.sb.str() == 'Box_T_f64__get_value(b)'
}

fn test_generate_c_uses_specialized_receiver_for_lowered_address_method_call() {
	mut gen := Gen{
		sb:                  strings.new_builder(64)
		fn_return_types:     {
			'Box__get_value':       'f64'
			'Box_T_f64__get_value': 'f64'
		}
		fn_param_is_ptr:     {
			'Box__get_value':       [true]
			'Box_T_f64__get_value': [true]
		}
		fn_param_types:      {
			'Box__get_value':       ['Box*']
			'Box_T_f64__get_value': ['Box_T_f64*']
		}
		runtime_local_types: {
			'b': 'Box_T_f64'
		}
	}
	gen.call_expr(ast.Ident{
		name: 'Box__get_value'
	}, [
		ast.Expr(ast.PrefixExpr{
			op:   token.Token.amp
			expr: ast.Ident{
				name: 'b'
			}
		}),
	])
	assert gen.sb.str() == 'Box_T_f64__get_value(&b)'
}

fn test_generate_c_emits_nested_generic_result_struct_specialization() {
	csrc := generate_c_for_test('
struct Item {
	name string
}

struct DecodeResult[T] {
	matched bool
	value T
}

struct Decoder {}

fn decode_struct_key[T](mut decoder Decoder, val T) !DecodeResult[T] {
	_ = decoder
	return DecodeResult[T]{
		matched: true
		value: val
	}
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	decode_result := decode_struct_key(mut decoder, val)!
	if decode_result.matched {
		val = decode_result.value
	}
}

fn use_decode(mut decoder Decoder, mut item Item) ! {
	decoder.decode_value(mut item)!
}
	')
	assert csrc.contains('\n_result_DecodeResult decode_struct_key_T_Item(Decoder* decoder, Item val) {')
	assert csrc.count('decode_struct_key_T_Item') >= 2
	assert !csrc.contains('decode_struct_key(decoder,')
	assert csrc.contains('_result_DecodeResult _or_')
	assert csrc.contains('DecodeResult decode_result =')
}

fn test_generate_c_inferrs_nested_generic_method_from_mut_map_param() {
	csrc := generate_c_for_test('
struct Decoder {}

fn (mut decoder Decoder) decode_map[V](mut val map[string]V) ! {
	_ = decoder
	_ = val
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T is $map {
		decoder.decode_map(mut val)!
	}
}

fn use_decode(mut decoder Decoder, mut data map[string]string) ! {
	decoder.decode_value(mut data)!
}
')
	assert csrc.contains('\n_result_void Decoder__decode_map_T_string(Decoder* decoder, Map_string_string* val) {')
	assert csrc.contains('_result_void _or_t')
	assert csrc.contains('Decoder__decode_map_T_string(decoder, val)')
	assert !csrc.contains('Decoder__decode_map(decoder, val)')
}

fn test_generate_c_specializes_nested_generic_method_from_active_generic_param() {
	csrc := generate_c_for_test('
struct Decoder {}

fn (mut decoder Decoder) decode_string[T](mut val T) ! {
	_ = decoder
	_ = val
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is voidptr {
		return
	} $else $if T.unaliased_typ is string {
		decoder.decode_string(mut val)!
	}
}

fn use_decode(mut decoder Decoder, mut data string) ! {
	decoder.decode_value(mut data)!
}
')
	assert csrc.contains('\n_result_void Decoder__decode_string_T_string(Decoder* decoder, string* val) {')
	assert csrc.contains('Decoder__decode_string_T_string(decoder, val)')
	assert !csrc.contains('Decoder__decode_string_T_T')
}

fn test_generate_c_matches_module_qualified_comptime_type_selector() {
	csrc := generate_c_for_test('
module time

struct Time {
	unix i64
}

struct Decoder {}

fn (mut decoder Decoder) decode_time(mut val Time) ! {
	_ = decoder
	_ = val
}

fn (mut decoder Decoder) decode_struct(mut val Time) ! {
	_ = decoder
	_ = val
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is time.Time {
		decoder.decode_time(mut val)!
	} $else $if T.unaliased_typ is $struct {
		decoder.decode_struct(mut val)!
	}
}

fn use_decode(mut decoder Decoder, mut t Time) ! {
	decoder.decode_value(mut t)!
}
')
	assert csrc.contains('time__Decoder__decode_time(decoder, val)')
	time_specialization := csrc.all_after('_result_void time__Decoder__decode_value_T_time_Time(time__Decoder* decoder, time__Time* val) {')
		.all_before('_result_void time__Decoder__decode_value_T_')
	assert !time_specialization.contains('time__Decoder__decode_struct(decoder, val)')
}

fn test_generate_c_passes_local_value_address_to_mut_receiver_method() {
	csrc := generate_c_for_test('
module time

struct Time {
	unix i64
}

fn (mut t Time) from_json_string(raw string) ! {
	_ = t
	_ = raw
}

fn decode_time() ! {
	mut decoded_time := Time{}
	decoded_time.from_json_string("1")!
}
')
	assert csrc.contains('time__Time__from_json_string(&decoded_time,')
	assert !csrc.contains('time__Time__from_json_string(decoded_time,')
}

fn test_generate_c_discovers_nested_call_inside_fallback_comptime_specialization() {
	csrc := generate_c_for_test('
struct Decoder {}
struct CJSON {}

fn seed[T](val T) {
	_ = val
}

fn (mut decoder Decoder) decode_string[T](mut val T) ! {
	_ = decoder
	_ = val
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is string {
		decoder.decode_string(mut val)!
	}
}

fn use_cjson(mut decoder Decoder, mut item CJSON) ! {
	decoder.decode_string(mut item)!
}

fn main() {
	seed("abc")
}
')
	assert csrc.contains('\n_result_void Decoder__decode_value_T_string(Decoder* decoder, string* val) {')
	assert csrc.contains('\n_result_void Decoder__decode_string_T_string(Decoder* decoder, string* val) {')
	assert !csrc.contains('decode_string_T_T')
}

fn test_generate_c_specializes_comptime_field_generic_method_call_from_field_type() {
	csrc := generate_c_for_test('
struct NestedConfig {
	name string
}

struct Config {
	repo string
	port int
	nested NestedConfig
	tags []string
}

struct Decoder {}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	$if T.unaliased_typ is string {
		_ = val
	} $else $if T.unaliased_typ is int {
		_ = val
	} $else $if T.unaliased_typ is $struct {
		$for field in T.fields {
			mut decoded_field_value := val.$(field.name)
			decoder.decode_value(mut decoded_field_value)!
			val.$(field.name) = decoded_field_value
		}
	}
}

fn use_config(mut decoder Decoder, mut config Config) ! {
	decoder.decode_value(mut config)!
}
')
	assert csrc.contains('Decoder__decode_value_T_string(decoder, &decoded_field_value)')
	assert csrc.contains('Decoder__decode_value_T_int(decoder, &decoded_field_value)')
	assert csrc.contains('Decoder__decode_value_T_NestedConfig(decoder, &decoded_field_value)')
	assert csrc.contains('Decoder__decode_value_T_Array_string(decoder, &decoded_field_value)')
	assert csrc.contains('\n_result_void Decoder__decode_value_T_NestedConfig(Decoder* decoder, NestedConfig* val) {')
	assert csrc.contains('\n_result_void Decoder__decode_value_T_Array_string(Decoder* decoder, Array_string* val) {')
	assert !csrc.contains('Decoder__decode_value_T_Config(decoder, &decoded_field_value)')
}

fn test_generate_c_keeps_json2_decode_string_string_specialization_with_existing_specs() {
	csrc := generate_c_for_test('
module json2

struct Decoder {}
struct CJSON {}

fn (mut decoder Decoder) decode_string[T](mut val T) ! {
	_ = decoder
	_ = val
}

fn use_cjson(mut decoder Decoder, mut item CJSON) ! {
	decoder.decode_string(mut item)!
}
')
	assert csrc.contains('\n_result_void json2__Decoder__decode_string_T_string(json2__Decoder* decoder, string* val) {')
	assert csrc.contains('json2__Decoder__decode_string_T_json2_CJSON')
}

fn test_generate_c_keeps_json2_decode_struct_key_struct_specialization_with_existing_specs() {
	csrc := generate_c_for_test('
module json2

struct Item {
	name string
}

struct ValueInfo {}

struct StructKeyDecodeResult[T] {
	matched bool
	value T
}

struct Decoder {}

fn decode_struct_key[T](mut decoder Decoder, val T, key_info ValueInfo, prefix string, mut seen_required []string) !StructKeyDecodeResult[T] {
	_ = decoder
	_ = key_info
	_ = prefix
	_ = seen_required
	return StructKeyDecodeResult[T]{
		matched: true
		value: val
	}
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	mut seen_required := []string{}
	decode_result := decode_struct_key(mut decoder, val, ValueInfo{}, "", mut seen_required)!
	if decode_result.matched {
		val = decode_result.value
	}
}

fn seed_f64(mut decoder Decoder, mut seen_required []string) ! {
	_ = decode_struct_key(mut decoder, 1.25, ValueInfo{}, "", mut seen_required)!
}

fn use_decode(mut decoder Decoder, mut item Item) ! {
	decoder.decode_value(mut item)!
}
	')
	assert csrc.contains('\n_result_json2__StructKeyDecodeResult_T_json2__Item json2__decode_struct_key_T_json2_Item(')
	assert csrc.contains('struct _result_json2__StructKeyDecodeResult_T_json2__Item')
	assert csrc.contains('json2__decode_struct_key_T_json2_Item(decoder, (*val)')
	assert !csrc.contains('json2__decode_struct_key(decoder,')
}

fn test_generic_struct_decl_type_counts_as_placeholder_for_fallback_specs() {
	assert type_contains_generic_placeholder(types.Type(types.Struct{
		name:           'api__ApiSuccessResponse'
		generic_params: ['T']
	}))
	assert type_contains_generic_placeholder(types.Type(types.Struct{
		name:           'api__ApiSuccessResponse'
		generic_params: ['T']
		fields:         [
			types.Field{
				name: 'result'
				typ:  types.Type(types.NamedType('T'))
			},
		]
	}))
	assert !type_contains_generic_placeholder(types.Type(types.Struct{
		name:           'api__ApiSuccessResponse'
		generic_params: ['T']
		fields:         [
			types.Field{
				name: 'result'
				typ:  types.Type(types.string_)
			},
		]
	}))
	assert !type_contains_generic_placeholder(types.Type(types.Struct{
		name:           'api__ApiSuccessResponse_T_bool'
		generic_params: ['T']
	}))
}

fn test_generate_c_handles_recursive_heap_struct_generic_placeholder_scan() {
	csrc := generate_c_for_test('
@[heap]
struct Node {
mut:
	children []&Node
}

fn keep[T](value T) T {
	return value
}

fn main() {
	node := &Node{}
	_ = keep(node.children)
}
')
	assert csrc.contains('struct Node')
}

fn test_generate_c_specializes_embedded_generic_method_for_primary_generic_struct_instance() {
	csrc := generate_c_for_test('
struct Result {}

struct BaseContext {}

fn (mut ctx BaseContext) json[T](j T) Result {
	_ = ctx
	_ = j
	return Result{}
}

struct ApiSuccessResponse[T] {
	success bool
	result T
}

struct Context {
	BaseContext
}

fn handler(mut ctx Context) Result {
	return ctx.json(ApiSuccessResponse[string]{
		success: true
		result: "ok"
	})
}
')
	assert csrc.contains('\nResult BaseContext__json_T_ApiSuccessResponse(BaseContext* ctx, ApiSuccessResponse j) {')
	assert csrc.contains('return BaseContext__json_T_ApiSuccessResponse(&ctx->BaseContext, ((ApiSuccessResponse){')
	assert !csrc.contains('BaseContext__json(ctx.BaseContext')
	assert !csrc.contains('BaseContext__json(ctx->BaseContext')
}

fn test_generate_c_uses_primary_generic_struct_as_nested_generic_field_type() {
	csrc := generate_c_for_test('
struct ApiSuccessResponse[T] {
	success bool
	result T
}

struct StructKeyDecodeResult[T] {
	matched bool
	value T
}

fn decode_struct_key[T](val T) !StructKeyDecodeResult[T] {
	return StructKeyDecodeResult[T]{
		matched: true
		value: val
	}
}

fn seed_f64() ! {
	_ = decode_struct_key(1.25)!
}

fn use(mut response ApiSuccessResponse[string]) ! {
	decode_result := decode_struct_key(response)!
	if decode_result.matched {
		response = decode_result.value
	}
}
')
	assert csrc.contains('\nstruct StructKeyDecodeResult_T_ApiSuccessResponse {\n\tbool matched;\n\tApiSuccessResponse value;\n};')
	assert !csrc.contains('\nstruct StructKeyDecodeResult_T_ApiSuccessResponse {\n\tbool matched;\n\tf64 value;\n};')
}

fn test_generate_c_discovers_explicit_generic_method_call_inside_specialized_generic_body() {
	csrc := generate_c_for_test('
struct Decoder {}

struct Context {
	name string
}

fn (mut decoder Decoder) cached_struct_field_infos[T]() []int {
	_ = decoder
	return []int{}
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	_ = val
	_ = decoder.cached_struct_field_infos[T]()
}

fn use_decode(mut decoder Decoder, mut ctx Context) ! {
	decoder.decode_value(mut ctx)!
}
')
	assert csrc.contains('\nArray_int Decoder__cached_struct_field_infos_T_Context(Decoder* decoder) {')
	assert csrc.contains('Decoder__cached_struct_field_infos_T_Context(decoder)')
	assert !csrc.contains('Decoder__cached_struct_field_infos_T(decoder)')
}

fn test_generate_c_emits_json2_cached_field_infos_for_embedded_struct_decode_value() {
	csrc := generate_c_for_test('
module json2

struct Decoder {}

struct Embedded {}

struct Context {
	Embedded
	name string
}

struct Known {
	value int
}

fn (mut decoder Decoder) cached_struct_field_infos[T]() []int {
	mut infos := []int{}
	$for field in T.fields {
		infos << field.name.len
	}
	return infos
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	_ = val
	mut has_embeds := false
	$for field in T.fields {
		$if field.is_embed {
			has_embeds = true
		}
	}
	if has_embeds {
		_ = 1
	} else {
		field_infos := unsafe { decoder.cached_struct_field_infos[T]() }
		_ = field_infos
	}
}

fn seed(mut decoder Decoder) {
	_ = unsafe { decoder.cached_struct_field_infos[Known]() }
}

fn use_decode(mut decoder Decoder, mut ctx Context) ! {
	decoder.decode_value(mut ctx)!
}
')
	assert csrc.contains('json2__Decoder__cached_struct_field_infos_T_json2_Context(json2__Decoder* decoder) {')
	assert csrc.contains('json2__Decoder__cached_struct_field_infos_T_json2_Context(decoder)')
}

fn test_generate_c_seeds_json2_cached_field_infos_from_decode_value_specs() {
	csrc := generate_c_for_test('
module json2

struct Decoder {}

struct Context {
	name string
}

struct Known {
	value int
}

fn (mut decoder Decoder) cached_struct_field_infos[T]() []int {
	mut infos := []int{}
	$for field in T.fields {
		infos << field.name.len
	}
	return infos
}

fn (mut decoder Decoder) decode_value[T](mut val T) ! {
	_ = decoder
	_ = val
}

fn seed(mut decoder Decoder) {
	_ = unsafe { decoder.cached_struct_field_infos[Known]() }
}

fn use_decode(mut decoder Decoder, mut ctx Context) ! {
	decoder.decode_value(mut ctx)!
}
')
	assert csrc.contains('json2__Decoder__cached_struct_field_infos_T_json2_Context(json2__Decoder* decoder) {')
}

fn test_generate_c_keeps_json2_encode_primitive_specialization_with_existing_specs() {
	csrc := generate_c_for_test_files([
		'
module json2

@[params]
pub struct EncoderOptions {}

pub fn encode[T](val T, config EncoderOptions) string {
	_ = val
	_ = config
	return ""
}
',
		'
module main

import json2 as json

fn seed() string {
	return json.encode("x")
}

fn dispatch[T](payload T) string {
	return json.encode(payload)
}

fn use() {
	_ = dispatch(i64(1))
}
',
	])
	assert csrc.contains('string json2__encode_T_i64(i64 val, json2__EncoderOptions config) {')
	assert csrc.contains('json2__encode_T_i64(payload, ((json2__EncoderOptions){0}))')
	assert !csrc.contains('return encode_T_i64(payload')
	assert !csrc.contains('= encode_T_i64(payload')
	assert !csrc.contains('((EncoderOptions){0})')
}

fn test_generate_c_specializes_json2_encode_value_per_encode_specialization() {
	csrc := generate_c_for_test_files([
		'
module json2

@[params]
pub struct EncoderOptions {}

struct Encoder {
mut:
	output []u8
}

pub fn encode[T](val T, config EncoderOptions) string {
	_ = config
	mut encoder := Encoder{}
	return wrap_int(encoder.encode_value[T](val))
}

fn (mut encoder Encoder) encode_value[T](val T) int {
	_ = val
	encoder.output << `x`
	return encoder.output.len
}

fn wrap_int(n int) string {
	_ = n
	return ""
}
',
		'
module main

import json2

struct Request {
	value string
}

fn use() {
	_ = json2.encode(Request{
		value: "x"
	}, json2.EncoderOptions{})
	_ = json2.encode(1, json2.EncoderOptions{})
}
',
	])
	assert csrc.contains('int json2__Encoder__encode_value_T_Request(json2__Encoder* encoder, Request val)')
	assert csrc.contains('int json2__Encoder__encode_value_T_int(json2__Encoder* encoder, int val)')
	assert !csrc.contains('json2__Encoder__encode_value_T_Request_int')
}

fn test_generate_c_skips_json2_void_specialization_from_voidptr_encode_path() {
	csrc := generate_c_for_test('
module json2

struct Encoder {}

fn (mut encoder Encoder) encode_value[T](val T) {
	_ = encoder
	$if T.unaliased_typ is voidptr {
		unsafe {
			encoder.encode_value(*val)
		}
	}
}

fn (mut encoder Encoder) encode_struct_fields[T](val T) bool {
	_ = encoder
	_ = val
	return true
}

fn use(mut encoder Encoder, data voidptr) {
	encoder.encode_value(data)
}
')
	assert csrc.contains('void json2__Encoder__encode_value_T_voidptr(json2__Encoder* encoder, voidptr val)')
	assert !csrc.contains('json2__Encoder__encode_value_T_void(')
	assert !csrc.contains('json2__Encoder__encode_struct_fields_T_void(')
}

fn test_json2_encode_fallback_type_skips_void_but_keeps_voidptr() {
	mut fallback_types := map[string]types.Type{}
	add_json2_encode_value_fallback_type(mut fallback_types, types.Type(types.void_))
	add_json2_encode_value_fallback_type(mut fallback_types, types.Type(types.Pointer{
		base_type: types.Type(types.void_)
	}))
	assert 'void' !in fallback_types
	assert fallback_types.len == 1
}

fn test_generate_c_lowers_generic_map_for_in_with_concrete_value_type() {
	csrc := generate_c_for_test('
module json2

struct Encoder {}

fn (mut encoder Encoder) encode_string(value string) {
	_ = encoder
	_ = value
}

fn (mut encoder Encoder) encode_value[T](val T) {
	$if T.unaliased_typ is $map {
		for key, value in val {
			encoder.encode_string(key)
			encoder.encode_value(value)
		}
	}
}

fn use(mut encoder Encoder, data map[string]string) {
	encoder.encode_value(data)
}
')
	assert csrc.contains('string key = *((string*)DenseArray__key(')
	assert csrc.contains('string value = *((string*)DenseArray__value(')
	assert csrc.contains('json2__Encoder__encode_value_T_string(encoder, value)')
	assert !csrc.contains('for (; ;) {')
}

fn test_generate_c_emits_json2_struct_field_helper_for_array_element_type() {
	csrc := generate_c_for_test('
module json2

struct Item {
	value string
}

struct Encoder {}

fn (mut encoder Encoder) encode_struct_fields[T](val T) bool {
	_ = encoder
	_ = val
	return true
}

fn (mut encoder Encoder) encode_value[T](val T) {
	$if T.unaliased_typ is $array {
		for item in val {
			encoder.encode_value(item)
		}
	} $else $if T.unaliased_typ is $struct {
		_ = encoder.encode_struct_fields[T](val)
	}
}

fn use(mut encoder Encoder, data []Item) {
	encoder.encode_value(data)
}
')
	assert csrc.contains('void json2__Encoder__encode_value_T_Array_json2_Item(json2__Encoder* encoder, Array_json2__Item val)')
	assert csrc.contains('void json2__Encoder__encode_value_T_json2_Item(json2__Encoder* encoder, json2__Item val)')
	assert csrc.contains('bool json2__Encoder__encode_struct_fields_T_json2_Item(json2__Encoder* encoder, json2__Item val)')
}

fn test_generate_c_qualifies_unique_default_param_helper_call() {
	csrc := generate_c_for_test_files([
		'
module zstd

pub fn default_c_level() int {
	return 3
}

@[params]
pub struct CompressParams {
pub:
	compression_level int = default_c_level()
}

pub fn compress(params CompressParams) int {
	return params.compression_level
}
',
		'
module main

import zstd

fn use() {
	_ = zstd.compress(zstd.CompressParams{})
}
',
	])
	assert csrc.contains('.compression_level = zstd__default_c_level()')
	assert !csrc.contains('.compression_level = default_c_level()')
}

fn test_generate_c_qualifies_local_array_generic_sumtype_call() {
	csrc := generate_c_for_test('
module orm

struct InfixType {
	right int
}

type Primitive = InfixType | []InfixType | int

fn tenant_filter_array_primitive_type[T](value []T) int {
	_ = value
	return 1
}

fn tenant_filter_primitive_type(value Primitive) int {
	return match value {
		InfixType {
			2
		}
		[]InfixType {
			tenant_filter_array_primitive_type(value)
		}
		int {
			3
		}
	}
}
')
	assert csrc.contains('orm__tenant_filter_array_primitive_type_T_orm_InfixType(')
	assert !csrc.contains('tenant_filter_array_primitive_type_T_InfixType(')
}

fn test_generate_c_resolves_bare_generic_helper_in_default_generic_receiver_method() {
	csrc := generate_c_for_test('
module datatypes

@[heap]
struct BSTreeNode[T] {
	value T
}

fn new_none_node[T](init bool) &BSTreeNode[T] {
	_ = init
	return &BSTreeNode[T]{}
}

struct BSTree[T] {
	root &BSTreeNode[T] = unsafe { 0 }
}

fn (bst &BSTree[T]) min_node() &BSTreeNode[T] {
	if unsafe { bst.root == 0 } {
		return new_none_node[T](false)
	}
	return bst.root
}

fn use(bst &BSTree[f64]) &BSTreeNode[f64] {
	return bst.min_node()
}
')
	assert csrc.contains('return datatypes__new_none_node_T_f64(false);')
	assert !csrc.contains('return new_none_node_T(false);')
}

fn test_generate_c_keeps_builtin_attribute_kind_enum_shorthand_compare() {
	csrc := generate_c_for_test('
struct Marker {}

type Primitive = Marker | string

enum AttributeKind {
	plain
	string
}

struct Attr {
	name string
	has_arg bool
	arg string
	kind AttributeKind
}

struct Field {
	name string
	attrs []Attr
}

fn attr_is_string(attr Attr) bool {
	return attr.kind == .string
}

fn sql_field_name(field Field) string {
	mut name := field.name
	for attr in field.attrs {
		if attr.name == field.name && attr.has_arg && attr.kind == .string {
			name = attr.arg
			break
		}
	}
	return name
}
')
	assert csrc.contains('(attr.kind == AttributeKind__string)')
	assert !csrc.contains('attr.kind._tag')
}

fn test_gen_comptime_veb_html_returns_result_value() {
	mut gen := Gen.new([])
	gen.gen_comptime_expr(ast.ComptimeExpr{
		expr: ast.Expr(ast.CallExpr{
			lhs:  ast.Expr(ast.SelectorExpr{
				lhs: ast.Expr(ast.Ident{
					name: 'veb'
				})
				rhs: ast.Ident{
					name: 'html'
				}
			})
			args: [
				ast.Expr(ast.StringLiteral{
					value: '"index.html"'
				}),
			]
		})
	})
	assert gen.sb.str() == '(veb__Result){0}'
}

fn test_find_struct_decl_info_prefers_exact_name_before_short_collision() {
	other_file := ast.File{
		mod:   'other'
		name:  'other.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'other'
			}),
			ast.Stmt(ast.StructDecl{
				name: 'Context'
			}),
		]
	}
	main_file := ast.File{
		mod:   'main'
		name:  'main.v'
		stmts: [
			ast.Stmt(ast.ModuleStmt{
				name: 'main'
			}),
			ast.Stmt(ast.StructDecl{
				name: 'Context'
			}),
		]
	}
	mut gen := Gen.new([other_file, main_file])
	gen.cur_module = 'main'
	info := gen.find_struct_decl_info_by_c_name('Context') or { panic('missing Context') }
	assert info.mod == 'main'
	assert info.file_name == 'main.v'
}

fn test_generate_c_orders_c_type_aliases_before_fn_aliases() {
	csrc := generate_c_for_test('
module sqlite

type Sig2 = fn (&Sqlite3_file, &int) int

pub type Sqlite3_file = C.sqlite3_file

type Fn_sqlite3_syscall_ptr = fn ()

pub type Sqlite3_vfs = C.sqlite3_vfs

type Sqlite3_vfs_set_system_call_fn = fn (&Sqlite3_vfs, &char, Fn_sqlite3_syscall_ptr) int

pub struct C.sqlite3_file {}

pub struct C.sqlite3_vfs {}
')
	file_alias_pos := csrc.index('typedef struct sqlite3_file sqlite__Sqlite3_file;') or {
		panic('missing sqlite file alias')
	}
	sig_pos := csrc.index('typedef int (*sqlite__Sig2)(sqlite__Sqlite3_file*, int*);') or {
		panic('missing Sig2 alias')
	}
	assert file_alias_pos < sig_pos
	assert csrc.contains('typedef int (*sqlite__Sqlite3_vfs_set_system_call_fn)(sqlite__Sqlite3_vfs*, char*, sqlite__Fn_sqlite3_syscall_ptr);')
	assert !csrc.contains('Fn_sqlite3_syscall_*')
}

fn test_generate_c_wraps_result_error_from_embedded_message_error() {
	csrc := generate_c_for_test('
interface IError {
	msg() string
	code() int
}

struct MessageError {
	msg string
	code int
}

fn (err MessageError) msg() string {
	return err.msg
}

fn (err MessageError) code() int {
	return err.code
}

struct SQLError {
	MessageError
}

fn connect() !int {
	return &SQLError{
		msg: "bad"
		code: 1
	}
}

fn error_message() IError {
	return SQLError{
		msg: "bad"
		code: 2
	}
}
')
	assert csrc.contains('return (_result_int){ .is_error=true, .err=((IError){._object = (void*)')
	assert csrc.contains('.msg = IError_SQLError_msg_wrapper')
	assert csrc.contains('return MessageError__msg(((SQLError*)_obj)->MessageError);')
	assert csrc.contains('static string __iface_wrap_IError_SQLError_msg(void* _obj)')
	assert csrc.contains('return MessageError__code(((SQLError*)_obj)->MessageError);')
	assert csrc.contains('return ({ SQLError* _ierr_obj')
	assert csrc.contains('.type_name = IError_SQLError_type_name_wrapper')
	assert !csrc.contains('(IError){._object = (void*)&_iface_obj')
}

fn test_generate_c_wraps_ierror_const_from_embedded_error() {
	csrc := generate_c_for_test('
interface IError {
	msg() string
	code() int
}

struct Error {}

fn (err Error) msg() string {
	return ""
}

fn (err Error) code() int {
	return 0
}

struct None__ {
	Error
}

const none__ = IError(&None__{})

fn use_none() IError {
	return none__
}
')
	assert csrc.contains('static string IError_None___msg_wrapper(void* _obj);')
	assert csrc.contains('#define none__ ((IError){')
	assert csrc.contains('.msg = IError_None___msg_wrapper')
	assert csrc.contains('.code = IError_None___code_wrapper')
	assert !csrc.contains('None____msg')
	assert !csrc.contains('None____code')
}

fn test_generate_c_wraps_interface_field_default_expr() {
	csrc := generate_c_for_test('
interface Speaker {
	speak() string
}

struct Person {}

fn (p Person) speak() string {
	return ""
}

const default_person = Person{}

struct Holder {
	speaker Speaker = default_person
	name string
}

fn make() Holder {
	return Holder{}
}
')
	assert csrc.contains('.speaker = ((Speaker){')
	assert csrc.contains('.speak =')
	assert csrc.contains('Person__speak')
	assert !csrc.contains('.speaker = default_person')
}

fn test_generate_c_emits_pack_pragmas_for_packed_struct() {
	csrc := generate_c_for_test("
@[_pack: '1']
struct Packet {
	tag u8
	len u32
}
")
	assert csrc.contains('#pragma pack(push, 1)\nstruct Packet {')
	assert csrc.contains('u32 len;')
	assert csrc.contains('};\n#pragma pack(pop)')
}

fn test_generate_c_keeps_stdatomic_atomicval_on_integer_fallback() {
	stdatomic_src := "
module stdatomic

pub struct AtomicVal[T] {
	val T
}

fn panic(msg string) {}

pub fn new_atomic[T](val T) &AtomicVal[T] {
	return &AtomicVal[T]{
		val: val
	}
}

pub fn (mut a AtomicVal[T]) add(delta T) T {
	@DLR@if T is int {
		return delta
	}
	panic('unreachable')
}
"
	main_src := '
module main

import stdatomic

fn main() {
	mut counter := stdatomic.new_atomic(0)
	counter.add(1)
}
'
	csrc := generate_c_for_test_files([stdatomic_src.replace('@DLR@', '$'), main_src])
	assert csrc.contains('int stdatomic__AtomicVal__add(stdatomic__AtomicVal* a, int delta)')
	assert csrc.contains('return delta;')
	assert csrc.contains('stdatomic__AtomicVal__add(counter, 1)')
	assert !csrc.contains('stdatomic__AtomicVal_T_f64__add')
}

fn test_generate_c_specializes_generic_function_value_in_struct_initializer() {
	transport_src := '
module transport

pub type RequestHandler = fn (req int) int

pub struct ServerConfig {
pub:
	handler RequestHandler
}

pub fn serve(config ServerConfig) int {
	return config.handler(7)
}
'
	web_src := '
module web

import transport

pub fn run_new[A, X](app A) int {
	_ = app
	config := transport.ServerConfig{
		handler: parallel_request_handler[A, X]
	}
	return transport.serve(config)
}

fn parallel_request_handler[A, X](req int) int {
	return route[A, X](req)
}

fn route[A, X](req int) int {
	return req
}
'
	main_src := '
module main

import web

struct App {}
struct Context {}

fn main() {
	_ = web.run_new[App, Context](App{})
}
'
	csrc := generate_c_for_test_files([transport_src, web_src, main_src])
	assert csrc.contains('int web__parallel_request_handler_T_App_Context(int req)')
	assert csrc.contains('return web__route_T_App_Context(req);')
	assert csrc.contains('.handler = ({ int web__parallel_request_handler_T_App_Context(int); web__parallel_request_handler_T_App_Context; })')
	assert !csrc.contains('.handler = web__parallel_request_handler,')
}

fn test_generate_c_lowers_slice_receiver_before_fallback_method_call() {
	csrc := generate_c_for_test('
fn main() {
	text := "abc"
	_ = text[..2].contains("a")
}
')
	assert csrc.contains('string__substr')
}

fn test_generate_c_infers_generic_slice_bytestr_return_type() {
	csrc := generate_c_for_test('
fn read_head[T](req T, buf []u8) string {
	_ = req
	head := buf[..1].bytestr()
	return head
}

fn main() {
	_ = read_head(0, [u8(65)])
}
')
	assert csrc.contains('string head = Array_u8__bytestr(array__slice(buf, 0, 1));')
	assert !csrc.contains('int head = Array_u8__bytestr')
}

fn test_generate_c_infers_unsafe_deref_cast_value_type() {
	csrc := generate_c_for_test('
struct RequestParams {
	port int
}

fn read_params(ptr voidptr) RequestParams {
	params := unsafe { *(&RequestParams(ptr)) }
	return params
}

fn main() {
	params := RequestParams{
		port: 1
	}
	_ = read_params(voidptr(&params))
}
')
	assert csrc.contains('RequestParams params = *(((RequestParams*)(ptr)));')
	assert !csrc.contains('RequestParams* params = *(((RequestParams*)(ptr)));')
}

fn test_generate_c_infers_generic_unsafe_deref_cast_value_type() {
	csrc := generate_c_for_test('
struct RequestParams {
	port int
}

struct Request {
	user_data voidptr
}

fn read_params[T](req Request) RequestParams {
	params := unsafe { *(&RequestParams(req.user_data)) }
	return params
}

fn main() {
	params := RequestParams{
		port: 1
	}
	req := Request{
		user_data: voidptr(&params)
	}
	_ = read_params[int](req)
}
')
	assert csrc.contains('RequestParams params = *(((RequestParams*)(req.user_data)));')
	assert !csrc.contains('RequestParams* params = *(((RequestParams*)(req.user_data)));')
}

fn test_generate_c_keeps_http_header_get_result_in_generic_function() {
	csrc := generate_c_for_test('
struct Header {}

fn (h Header) get(key int) !string {
	_ = h
	_ = key
	return ""
}

struct Request {
	header Header
}

fn read_host[T](req Request) string {
	host := req.header.get(0) or { "" }
	return host
}

fn main() {
	_ = read_host[int](Request{})
}
')
	assert csrc.contains('_result_string _or_')
	assert !csrc.contains('void* _or_')
	assert !csrc.contains('int host =')
}

fn test_generate_c_keeps_imported_http_header_get_result_for_or_default() {
	csrc := generate_c_for_test_files([
		'
module http

pub enum CommonHeader {
	host
	accept
}

pub struct Header {}

pub fn (h Header) get(key CommonHeader) !string {
	_ = h
	_ = key
	return ""
}

pub struct Request {
pub:
	header Header
}
',
		'
module main

import http

fn handle(req http.Request) string {
	host_with_port := req.header.get(.host) or { "" }
	return host_with_port
}
',
	])
	assert csrc.contains('_result_string _or_')
	assert !csrc.contains('void* _or_')
	assert !csrc.contains('int host_with_port =')
}

fn test_generate_c_keeps_imported_http_header_get_result_in_generic_function() {
	csrc := generate_c_for_test_files([
		'
module http

pub enum CommonHeader {
	host
}

pub struct Header {}

pub fn (h Header) get(key CommonHeader) !string {
	_ = h
	_ = key
	return ""
}

pub struct Request {
pub:
	header Header
}
',
		'
module main

import http

fn handle[T](req http.Request) string {
	host_with_port := req.header.get(.host) or { "" }
	return host_with_port
}

fn main() {
	_ = handle[int](http.Request{})
}
',
	])
	assert csrc.contains('_result_string _or_')
	assert csrc.contains('string host_with_port =')
	assert !csrc.contains('void* _or_')
	assert !csrc.contains('int host_with_port =')
}

fn test_generate_c_lowers_generic_promoted_embedded_field_selector() {
	csrc := generate_c_for_test('
struct Base {
	value int
}

struct Wrapper {
	Base
}

fn read_value[T](x &T) int {
	return x.value
}

fn main() {
	w := Wrapper{
		Base: Base{
			value: 7
		}
	}
	_ = read_value[Wrapper](&w)
}
')
	assert csrc.contains('return x->Base.value;')
	assert !csrc.contains('return x->value;')
}

fn test_generate_c_lowers_generic_promoted_embedded_field_with_context_name_collision() {
	csrc := generate_c_for_test_files([
		'
module http

pub enum CommonHeader {
	accept
}

pub struct Header {}

pub fn (h Header) get(key CommonHeader) !string {
	_ = h
	_ = key
	return ""
}

pub struct Request {
pub:
	header Header
}
',
		'
module veb

import http

pub struct Context {
pub:
	req http.Request
}

pub fn serve[X](user_context &X) string {
	accept_header := user_context.req.header.get(.accept) or { "" }
	return accept_header
}
',
		'
module main

import veb

pub struct Context {
	veb.Context
}

fn boot(mut ctx Context) {
	_ = veb.serve[Context](&ctx)
}
',
	])
	assert csrc.contains('http__Header__get(user_context->Context.req.header, http__CommonHeader__accept)')
	assert csrc.contains('_result_string _or_')
	assert csrc.contains('string accept_header =')
	assert !csrc.contains('http__Response accept_header =')
	assert !csrc.contains('http__Header__get(user_context->req.header')
}

fn test_generate_c_keeps_bare_main_context_specialization_for_generic_init() {
	csrc := generate_c_for_test_files([
		'
module veb

pub struct Context {
pub:
	content_type string
}

pub fn new_user_context[X](ctx &Context) X {
	return X{
		Context: ctx
	}
}

pub fn new_user_context_decl[X](ctx &Context) X {
	mut user_context := X{
		Context: ctx
	}
	return user_context
}
',
		'
module main

import veb

pub struct Context {
	veb.Context
pub:
	path string
}

fn boot() {
	base := veb.Context{}
	_ = veb.new_user_context[Context](&base)
	_ = veb.new_user_context_decl[Context](&base)
}
',
	])
	assert csrc.contains('\nContext veb__new_user_context_T_Context(veb__Context* ctx) {')
	assert csrc.contains('return ((Context){.Context = (*(ctx))')
	assert !csrc.contains('return ((veb__Context){.Context = (*(ctx))')
	assert csrc.contains('\nContext veb__new_user_context_decl_T_Context(veb__Context* ctx) {')
	assert csrc.contains('Context user_context = ((Context){.Context = (*(ctx))')
	assert !csrc.contains('Context user_context = ((veb__Context){.Context = (*(ctx))')
}

fn test_generate_c_dereferences_pointer_for_embedded_struct_init_field() {
	csrc := generate_c_for_test('
struct Base {
	value int
}

struct Wrapper {
	Base
}

fn make_wrapper(ctx &Base) Wrapper {
	return Wrapper{
		Base: ctx
	}
}

fn main() {
	base := Base{
		value: 7
	}
	_ = make_wrapper(&base)
}
')
	assert csrc.contains('.Base = (*(ctx))')
	assert !csrc.contains('.Base = ctx')
}

fn test_generate_c_lowers_string_plus_assign() {
	csrc := generate_c_for_test('
fn append_slash() string {
	mut asked_path := "docs"
	asked_path += "/"
	return asked_path
}

fn main() {
	_ = append_slash()
}
')
	assert csrc.contains('asked_path = string__plus(asked_path, (string){.str = "/",')
	assert !csrc.contains('asked_path += (string){.str = "/",')
}

fn test_generate_c_keeps_map_or_pointer_value_type() {
	csrc := generate_c_for_test('
struct Item {
	value int
}

fn read_item(items map[int]&Item) &Item {
	item := items[1] or { return &Item{} }
	return item
}

fn main() {
	mut items := map[int]&Item{}
	items[1] = &Item{
		value: 7
	}
	_ = read_item(items)
}
')
	assert csrc.contains('Item* item = *((Item**)')
	assert !csrc.contains('Item item = *((Item**)')
}

fn test_generate_c_lowers_selector_map_or_without_result_temp() {
	csrc := generate_c_for_test('
struct StaticHandler {
	static_files map[string]string
}

struct Context {
	value int
}

fn serve[X](app StaticHandler, mut ctx X, asked_path string) bool {
	static_handler := app
	static_file := static_handler.static_files[asked_path] or { return false }
	_ = ctx
	return static_file != ""
}

fn main() {
	sh := StaticHandler{
		static_files: map[string]string{}
	}
	mut ctx := Context{}
	_ = serve[Context](sh, mut ctx, "/")
}
')
	assert csrc.contains('map__get_check')
	assert csrc.contains('string static_file = *((string*)')
	assert !csrc.contains('string _or_t')
}

fn test_generate_c_preserves_explicit_generic_arg_for_array_result_forwarder() {
	mut g := Gen{
		sb:                   strings.new_builder(64)
		runtime_local_types:  {
			'rng':   'Rng'
			'items': 'Array_Array_u8'
		}
		active_generic_types: {
			'T': types.Type(types.Array{
				elem_type: types.Type(types.Alias{
					name:      'u8'
					base_type: types.Type(types.Primitive{
						props: .integer | .unsigned
						size:  8
					})
				})
			})
		}
		fn_return_types:      {
			'Rng__elem_T_Array_u8': '_result_Array_u8'
		}
		fn_param_is_ptr:      {
			'Rng__elem_T_Array_u8': [true, false]
		}
	}
	g.call_expr(ast.GenericArgOrIndexExpr{
		lhs:  ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'rng'
			}
			rhs: ast.Ident{
				name: 'elem'
			}
		}
		expr: ast.Ident{
			name: 'T'
		}
	}, [ast.Expr(ast.Ident{
		name: 'items'
	})])
	assert g.sb.str() == 'Rng__elem_T_Array_u8(&rng, items)'
}

fn test_generate_c_wraps_fixed_array_result_with_memcpy() {
	csrc := generate_c_for_test('
fn first(items [][4]u8) ![4]u8 {
	return items[0]
}

fn empty() ![4]u8 {
	return [4]u8{}
}
')
	assert csrc.contains('Array_fixed_u8_4 _val = {0}; memcpy(_val, ((Array_fixed_u8_4*)items.data)[((int)(0))], sizeof(_val));')
	assert !csrc.contains('Array_fixed_u8_4 _val = ((Array_fixed_u8_4*)items.data)[0];')
	assert !csrc.contains('memcpy(_val, {0}, sizeof(_val));')
}

fn test_generate_c_lowers_overloaded_mod_operator() {
	mut g := Gen{
		sb:                  strings.new_builder(64)
		runtime_local_types: {
			'a': 'big__Integer'
			'b': 'big__Integer'
		}
		fn_return_types:     {
			'big__Integer__mod': 'big__Integer'
		}
	}
	g.expr(ast.InfixExpr{
		op:  .mod
		lhs: ast.Ident{
			name: 'a'
		}
		rhs: ast.Ident{
			name: 'b'
		}
	})
	assert g.sb.str() == 'big__Integer__mod(a, b)'
}

fn test_generate_c_keeps_pointer_array_for_in_element_type() {
	csrc := generate_c_for_test('
struct Item {
	name string
}

fn first(items []&Item) string {
	for item in items {
		return item.name
	}
	return ""
}

fn main() {
	item := &Item{
		name: "x"
	}
	_ = first([item])
}
')
	assert csrc.contains('Item* item = ((Item**)(items).data)')
	assert !csrc.contains('void* item = ((void**)(items).data)')
}

fn test_generate_c_specializes_nested_generic_call_with_function_type_arg() {
	json_src := '
module json2

pub struct EncoderOptions {}

pub fn encode[T](val T, config EncoderOptions) string {
	_ = val
	_ = config
	return ""
}
'
	main_src := '
module main

import json2

fn value_fn(n int) string {
	_ = n
	return ""
}

struct App {}

fn (mut app App) dispatch[T](j T) string {
	_ = app
	return json2.encode(j)
}

fn main() {
	mut app := App{}
	_ = app.dispatch(value_fn)
}
'
	csrc := generate_c_for_test_files([json_src, main_src])
	assert csrc.contains('json2__encode_T_fn_')
	assert csrc.contains('App__dispatch_T_fn_')
	assert csrc.contains('return json2__encode_T_fn_')
	assert csrc.contains('((json2__EncoderOptions){0})')
	assert !csrc.contains('((EncoderOptions){0})')
}

fn test_generate_c_renames_anonymous_function_params() {
	csrc := generate_c_for_test('
fn check(_ int, _ int) int {
	return 0
}

fn main() {
	_ = check(1, 2)
}
')
	assert csrc.contains('int check(int _arg0, int _arg1)')
	assert !csrc.contains('int check(int _, int _)')
}

fn test_generate_c_emits_late_generic_result_alias_before_forward_decl() {
	csrc := generate_c_for_test('
fn values[T](x T) ![]T {
	_ = x
	return []T{}
}

fn wrapper[U](x U) ![]U {
	return values[U](x)
}

fn main() {
	_ = wrapper[voidptr](unsafe { nil }) or { return }
}
')
	alias_pos := csrc.index('typedef struct _result_Array_voidptr _result_Array_voidptr;') or {
		assert false
		return
	}
	decl_pos := csrc.index('_result_Array_voidptr values_T_voidptr(void* x);') or {
		assert false
		return
	}
	assert alias_pos < decl_pos
}

fn test_generate_c_emits_late_generic_struct_result_alias_before_forward_decl() {
	csrc := generate_c_for_test('
module json2

struct Label {
	name string
}

struct ValueInfo {}

struct StructKeyDecodeResult[T] {
	matched bool
	value T
}

struct Decoder {}

fn decode_struct_key[T](mut decoder Decoder, val T, key_info ValueInfo, prefix string, mut seen_required []string) !StructKeyDecodeResult[T] {
	_ = decoder
	_ = key_info
	_ = prefix
	_ = seen_required
	return StructKeyDecodeResult[T]{
		matched: true
		value: val
	}
}

fn (mut decoder Decoder) decode_array[T](mut val []T) ! {
	mut seen_required := []string{}
	decode_result := decode_struct_key(mut decoder, val, ValueInfo{}, "", mut seen_required)!
	if decode_result.matched {
		val = decode_result.value
	}
}

fn seed_f64(mut decoder Decoder, mut seen_required []string) ! {
	_ = decode_struct_key(mut decoder, 1.25, ValueInfo{}, "", mut seen_required)!
}

fn use_decode(mut decoder Decoder, mut labels []Label) ! {
	decoder.decode_array(mut labels)!
}
')
	alias_pos := csrc.index('typedef struct _result_json2__StructKeyDecodeResult_T_Array') or {
		assert false
		return
	}
	decl_pos := csrc.index(' json2__decode_struct_key_T_Array') or {
		assert false
		return
	}
	assert alias_pos < decl_pos
}
