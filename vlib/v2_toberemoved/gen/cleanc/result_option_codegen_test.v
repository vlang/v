// vtest build: macos
module cleanc

import os
import strings
import v2.ast
import v2.markused
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_result_option_c_for_test(code string) string {
	return generate_result_option_c_for_test_files([code])
}

fn generate_result_option_c_for_test_files(sources []string) string {
	tmp_dir := os.join_path(os.temp_dir(), 'v2_result_option_codegen_test_${os.getpid()}')
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

fn generate_markused_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_markused_codegen_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	gen_files := trans.transform_files(files)
	used := markused.mark_used(gen_files, env)
	mut gen := Gen.new_with_env_and_pref(gen_files, env, prefs)
	gen.set_used_fn_keys(used)
	return gen.gen()
}

fn generate_ownership_markused_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_ownership_markused_codegen_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: true
		ownership:   true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	gen_files := trans.transform_files(files)
	used := markused.mark_used(gen_files, env)
	mut gen := Gen.new_with_env_and_pref(gen_files, env, prefs)
	gen.set_used_fn_keys(used)
	return gen.gen()
}

fn generate_parallel_worker_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_parallel_worker_codegen_test_${os.getpid()}.v')
	os.write_file(tmp_file, code) or { panic('failed to write temp file') }
	defer {
		os.rm(tmp_file) or {}
	}
	prefs := &vpref.Preferences{
		backend:     .cleanc
		no_parallel: false
		ownership:   true
	}
	mut file_set := token.FileSet.new()
	mut par := parser.Parser.new(prefs)
	files := par.parse_files([tmp_file], mut file_set)
	env := types.Environment.new()
	mut checker := types.Checker.new(prefs, file_set, env)
	checker.check_files(files)
	mut trans := transformer.Transformer.new_with_pref(env, prefs)
	gen_files := trans.transform_files(files)
	used := markused.mark_used(gen_files, env)
	mut gen := Gen.new_with_env_and_pref(gen_files, env, prefs)
	gen.set_used_fn_keys(used)
	gen.gen_passes_1_to_4()
	emit_indices := gen.gen_pass5_pre()
	mut worker := gen.new_pass5_worker(emit_indices, 0)
	worker.gen_pass5_files(emit_indices)
	gen.merge_pass5_worker(worker)
	gen.gen_pass5_post()
	return gen.gen_finalize()
}

fn test_generate_c_hoists_if_expr_temp_before_result_propagation_call() {
	csrc := generate_result_option_c_for_test('
struct Params {
	checksum bool
}

struct Ctx {}

fn (mut c Ctx) set(v int) ! {
}

fn new_ctx(params Params) !Ctx {
	mut c := Ctx{}
	c.set(if params.checksum { 1 } else { 0 })!
	return c
}
')
	if_pos := csrc.index('int _if_t') or { panic('missing if-expression temp') }
	or_pos := csrc.index('_result_void _or_t') or { panic('missing result propagation temp') }
	assert if_pos < or_pos
	assert csrc.contains('Ctx__set(&c, _if_t')
}

fn test_generate_c_keeps_option_wrapper_for_fn_value_if_guard() {
	csrc := generate_result_option_c_for_test("
fn with_name_to_index(name_to_index fn (string) ?int) {
	if index := name_to_index('foo') {
		_ = index
	}
}
")
	assert csrc.contains('_option_int _or_t')
	assert csrc.contains('if ((_or_t')
	assert !csrc.contains('void* _or_t')
}

fn test_generate_c_keeps_option_wrapper_for_or_block_temp() {
	csrc := generate_result_option_c_for_test('
fn maybe_index() ?int {
	return 3
}

fn find_stop() int {
	stop_index := maybe_index() or { -1 }
	return stop_index
}
')
	assert csrc.contains('_option_int _or_t')
	assert csrc.contains('stop_index')
}

fn test_generate_c_ignores_direct_option_or_expr_stmt_payload() {
	csrc := generate_result_option_c_for_test('
struct User {}

fn maybe_user() ?User {
	return User{}
}

fn main() {
	maybe_user() or {}
}
')
	assert csrc.contains('_option_User _or_t')
	assert csrc.contains('.state != 0')
	assert !csrc.contains('User*)(((u8*)(&_or_t')
}

fn test_generate_c_preserves_string_slice_type_for_followup_index_or() {
	csrc := generate_result_option_c_for_test('
fn (s string) find_text(needle string) ?int {
	_ = s
	_ = needle
	return 0
}

fn find_symbol(sframe string) int {
	symbol_start := sframe.find_text("0x") or { -1 }
	rest := sframe[symbol_start..]
	space_after_addr := rest.find_text(" ") or { -1 }
	symbol_and_offset := rest[space_after_addr + 1..]
	plus_pos := symbol_and_offset.find_text(" + ") or { -1 }
	return plus_pos
}
	')
	assert csrc.contains('string symbol_and_offset = string__substr(rest')
	assert !csrc.contains('array symbol_and_offset = array__slice(rest')
	assert csrc.contains('.state != 0')
}

fn test_generate_c_declares_struct_result_or_value_with_struct_type() {
	csrc := generate_result_option_c_for_test('
struct Glob {}

fn new_glob() !Glob {
	return Glob{}
}

fn build() !Glob {
	glob := new_glob() or { return err }
	return glob
}
')
	assert csrc.contains('Glob glob =')
	assert !csrc.contains('int glob =')
}

fn test_generate_c_declares_static_method_result_or_value_with_struct_type() {
	csrc := generate_result_option_c_for_test('
struct Glob {}

fn Glob.new(pattern string) !Glob {
	_ = pattern
	return Glob{}
}

fn build() !Glob {
	glob := Glob.new("*.v") or { return err }
	return glob
}
')
	assert csrc.contains('Glob glob =')
	assert !csrc.contains('int glob =')
	assert !csrc.contains('Glob__new(Glob,')
}

fn test_generate_c_unwraps_module_result_or_inside_if_expr_branch() {
	csrc := generate_result_option_c_for_test_files([
		'
module codec

pub fn decompress(data []u8) ![]u8 {
	return data
}
',
		'
module main

import codec

fn decode(input []u8, flag bool) []u8 {
	mut decoded := input
	decoded = if flag {
		codec.decompress(decoded) or { return decoded }
	} else {
		decoded
	}
	return decoded
}
',
	])
	assert csrc.contains('_result_Array_u8 _or_t')
	assert csrc.contains('(*(Array_u8*)(((u8*)(&_or_t')
	assert !csrc.contains('int _if_expr_t')
}

fn test_generate_c_unwraps_result_or_in_nested_if_expr_branch() {
	csrc := generate_result_option_c_for_test_files([
		'
module codec

pub fn decompress(data []u8, check bool) ![]u8 {
	_ = check
	return data
}

pub fn inflate(data []u8) ![]u8 {
	return data
}
',
		'
module main

import codec

fn decode(input []u8, encoding string) []u8 {
	mut decoded := input
	decoded = if encoding == "gzip" || encoding == "x-gzip" {
		codec.decompress(decoded, true) or { return input }
	} else {
		if encoding == "deflate" {
			codec.inflate(decoded) or { return input }
		} else {
			decoded
		}
	}
	return decoded
}
',
	])
	assert csrc.count('_result_Array_u8 _or_t') >= 2
	assert csrc.contains('(*(Array_u8*)(((u8*)(&_or_t')
	assert !csrc.contains('= ({ int _if_expr_t')
}

fn test_generate_c_recovers_or_payload_when_final_expr_is_empty() {
	mut gen := Gen{
		sb:              strings.new_builder(256)
		fn_return_types: {
			'gzip__decompress': '_result_Array_u8'
		}
	}
	gen.gen_unsafe_expr(ast.UnsafeExpr{
		stmts: [
			ast.Stmt(ast.AssignStmt{
				op:  .decl_assign
				lhs: [ast.Expr(ast.Ident{
					name: '_or_t1'
				})]
				rhs: [
					ast.Expr(ast.CallExpr{
						lhs: ast.Ident{
							name: 'gzip__decompress'
						}
					}),
				]
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.IfExpr{
					cond:  ast.SelectorExpr{
						lhs: ast.Ident{
							name: '_or_t1'
						}
						rhs: ast.Ident{
							name: 'is_error'
						}
					}
					stmts: []ast.Stmt{}
				}
			}),
			ast.Stmt(ast.ExprStmt{
				expr: ast.empty_expr
			}),
		]
	})
	csrc := gen.sb.str()
	assert csrc.contains('_result_Array_u8 _or_t1 = gzip__decompress();')
	assert csrc.contains('(*(Array_u8*)(((u8*)(&_or_t1.err)) + sizeof(IError)))')
}

fn test_generate_c_passes_mut_arg_by_address_to_fn_pointer_param() {
	csrc := generate_result_option_c_for_test('
fn render(replacement fn (string, mut []string)) int {
	mut out := []string{}
	replacement("x", mut out)
	return 0
}
')
	assert csrc.contains('replacement((string){.str = "x"')
	assert csrc.contains(', &out);')
	assert !csrc.contains(', out);')
}

fn test_generate_c_resolves_fn_literal_param_type_for_string_interpolation() {
	csrc := generate_result_option_c_for_test('
fn render(replacement fn (string, mut []string)) {
	mut out := []string{}
	replacement("x", mut out)
}

fn demo() {
	render(fn (name string, mut out []string) {
		out << "<\${name}>"
	})
}
')
	assert csrc.contains('"<%s>"')
	assert !csrc.contains('"<%d>"')
}

fn test_generate_c_resolves_selector_string_field_type_for_string_interpolation() {
	csrc := generate_result_option_c_for_test('
struct Alias {
	name        string
	description string
}

fn render(alias Alias) string {
	return "\${alias.name}:\\"\${alias.description}\\""
}
')
	assert csrc.contains('"%s:\\"%s\\""')
	assert !csrc.contains('"%d:\\"%d\\""')
}

fn test_generate_c_resolves_for_in_selector_string_field_type_for_string_interpolation() {
	csrc := generate_result_option_c_for_test('
struct Alias {
	name        string
	description string
}

fn aliases() []Alias {
	return [
		Alias{
			name: "vscode"
			description: "VS Code scheme (vscode://)"
		},
	]
}

fn render() string {
	mut last := ""
	for alias in aliases() {
		last = "    \${alias.name}:\\"\${alias.description}\\""
	}
	return last
}
')
	assert csrc.contains('"    %s:\\"%s\\""')
	assert !csrc.contains('"    %d:\\"%d\\""')
}

fn test_generate_c_expands_builtin_option_clone_if_guard() {
	csrc := generate_result_option_c_for_test('
interface IClone {}

struct Bag implements IClone {
	name ?string
}

fn copy_bag(b Bag) Bag {
	return b.clone()
}
	')
	assert csrc.contains('builtin__Option_string__clone')
	assert csrc.contains('.state == 0')
	assert !csrc.contains('if (s)')
	assert !csrc.contains('string _val = builtin__Option_string__clone')
}

fn test_generate_c_does_not_wrap_builtin_option_clone_for_multiple_option_fields() {
	csrc := generate_result_option_c_for_test('
interface IClone {}

struct Env implements IClone {
	host ?string
	prefix ?string
}

fn copy_env(e Env) Env {
	return e.clone()
}
	')
	assert csrc.contains('builtin__Option_string__clone')
	assert !csrc.contains('string _val = builtin__Option_string__clone')
}

fn test_generate_c_wraps_struct_field_option_value() {
	csrc := generate_result_option_c_for_test('
struct Ref {
	value int
}

struct Holder {
	item ?&Ref
}

fn make(r &Ref) Holder {
	return Holder{
		item: r
	}
}
')
	assert csrc.contains('_option_Refptr item;')
	assert csrc.contains('_option_Refptr _opt = (_option_Refptr){ .state = 2 }; Ref* _val = r; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt;')
	assert !csrc.contains('.item = r')
}

fn test_generate_c_wraps_module_lifetime_alias_value_for_option_field() {
	csrc := generate_result_option_c_for_test('
module sample

struct Item {}

type ItemRef[^a] = &^a Item

struct Holder[^a] {
	item ?ItemRef[^a]
}

fn make[^a](item ItemRef[^a]) Holder[^a] {
	return Holder[^a]{
		item: item
	}
}
')
	assert csrc.contains('_option_sample__ItemRef item;')
	assert csrc.contains('_option_sample__ItemRef _opt = (_option_sample__ItemRef){ .state = 2 }; sample__ItemRef _val = item; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt;')
	assert !csrc.contains('_option_ItemRef')
	assert !csrc.contains('.item = item')
}

fn test_generate_c_wraps_deref_value_for_option_string_field() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	item ?string
}

struct Parser {
	glob &string
}

fn mk_error(p Parser) Holder {
	return Holder{
		item: *p.glob
	}
}
	')
	assert csrc.contains('_option_string item;')
	assert csrc.contains('_option_string _opt = (_option_string){ .state = 2 }; string _val = *')
	assert csrc.contains('glob; _option_ok(&_val, (_option*)&_opt, sizeof(_val)); _opt;')
	assert !csrc.contains('.item = *(p).glob')
}

fn test_generate_c_wraps_string_literal_for_option_string_field() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	item ?string
}

fn mk_holder() Holder {
	return Holder{
		item: "--"
	}
}
')
	assert csrc.contains('_option_string item;')
	assert csrc.contains('_option_string _opt = (_option_string){ .state = 2 }; string _val = (string){.str = "--"')
	assert !csrc.contains('.item = (string){.str = "--"')
}

fn test_generate_c_interface_cast_strips_pointer_for_method_symbol() {
	csrc := generate_result_option_c_for_test('
interface Handle {
	len() int
}

struct File {}

fn (mut f File) len() int {
	_ = f
	return 1
}

fn consume(h &Handle) int {
	return h.len()
}

fn demo(mut file File) int {
	return consume(&file)
}
	')
	assert csrc.contains('.len = (int (*)(void*))File__len')
	assert !csrc.contains('File*__len')
}

fn test_generate_c_heap_boxes_addr_of_call_or_cast_interface() {
	csrc := generate_result_option_c_for_test('
interface Runner {
	next() int
}

struct Concrete {}

fn (mut c Concrete) next() int {
	return 7
}

fn make() &Runner {
	mut c := &Concrete{}
	return &Runner(c)
}

fn demo() int {
	r := make()
	return r.next()
}
')
	assert csrc.contains('Runner* _iface_t = (Runner*)malloc(sizeof(Runner))')
	assert csrc.contains('.next = (int (*)(void*))Concrete__next')
	assert !csrc.contains('return ((Runner*)(c));')
}

fn test_ierror_concrete_base_prefers_current_module_qualified_method() {
	mut env := types.Environment.new()
	env.set_expr_type(42, types.Type(types.Struct{
		name: 'TimeParseError'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.cur_module = 'time'
	gen.fn_return_types['time__TimeParseError__msg'] = 'string'
	base := gen.ierror_concrete_base_for_expr(ast.Expr(ast.Ident{
		name: 'parse_err'
		pos:  token.Pos{
			id: 42
		}
	}))
	assert base == 'time__TimeParseError'
}

fn test_ierror_concrete_base_finds_qualified_method_outside_current_module() {
	mut env := types.Environment.new()
	env.set_expr_type(46, types.Type(types.Struct{
		name: 'TimeParseError'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.cur_module = 'main'
	gen.fn_return_types['time__TimeParseError__msg'] = 'string'
	base := gen.ierror_concrete_base_for_expr(ast.Expr(ast.Ident{
		name: 'parse_err'
		pos:  token.Pos{
			id: 46
		}
	}))
	assert base == 'time__TimeParseError'
}

fn test_get_expr_type_prefers_explicit_module_qualified_init_expr_type() {
	mut env := types.Environment.new()
	env.set_expr_type(43, types.Type(types.Struct{
		name: 'TimeParseError'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.cur_module = 'time'
	gen.emitted_types['body_time__TimeParseError'] = true
	expr := ast.Expr(ast.InitExpr{
		typ: ast.Ident{
			name: 'TimeParseError'
		}
		pos: token.Pos{
			id: 43
		}
	})
	assert gen.get_expr_type(expr) == 'time__TimeParseError'
}

fn test_get_expr_type_prefers_module_qualified_addr_of_init_expr_type() {
	mut env := types.Environment.new()
	env.set_expr_type(44, types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'TimeParseError'
		}
	}))
	mut gen := Gen.new_with_env([], env)
	gen.cur_module = 'time'
	gen.emitted_types['body_time__TimeParseError'] = true
	expr := ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.InitExpr{
			typ: ast.Ident{
				name: 'TimeParseError'
			}
		}
		pos:  token.Pos{
			id: 44
		}
	})
	assert gen.get_expr_type(expr) == 'time__TimeParseError*'
}

fn test_interface_concrete_type_prefers_module_qualified_addr_of_init_expr_type() {
	mut env := types.Environment.new()
	env.set_expr_type(45, types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'TimeParseError'
		}
	}))
	mut gen := Gen.new_with_env([], env)
	gen.cur_module = 'time'
	gen.emitted_types['body_time__TimeParseError'] = true
	expr := ast.Expr(ast.PrefixExpr{
		op:   .amp
		expr: ast.InitExpr{
			typ: ast.Ident{
				name: 'TimeParseError'
			}
		}
		pos:  token.Pos{
			id: 45
		}
	})
	assert gen.concrete_type_for_interface_value('IError', expr) == 'time__TimeParseError*'
}

fn test_interface_concrete_type_prefers_module_qualified_wrapped_addr_of_init_expr_type() {
	mut gen := Gen.new([])
	gen.cur_module = 'time'
	gen.emitted_types['body_time__TimeParseError'] = true
	expr := ast.Expr(ast.CastExpr{
		typ:  ast.Ident{
			name: 'IError'
		}
		expr: ast.ParenExpr{
			expr: ast.PrefixExpr{
				op:   .amp
				expr: ast.InitExpr{
					typ: ast.Ident{
						name: 'TimeParseError'
					}
				}
			}
		}
	})
	assert gen.concrete_type_for_interface_value('IError', expr) == 'time__TimeParseError*'
}

fn test_generate_c_initializes_omitted_option_struct_fields_to_none() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	item ?int
	name string
}

fn make_named() Holder {
	return Holder{
		name: "x"
	}
}

fn make_empty() Holder {
	return Holder{}
}
	')
	assert csrc.contains('_option_int){ .state = 2 }')
	assert !csrc.contains('return ((Holder){0})')
}

fn test_generate_c_initializes_omitted_option_struct_payload_fields_to_none() {
	csrc := generate_result_option_c_for_test('
interface IClone {}

struct Item implements IClone {
	name string
}

struct Holder implements IClone {
	name string
	item ?Item
}

fn make_named() Holder {
	return Holder{
		name: "x"
	}
}

fn make_empty() Holder {
	return Holder{}
}
	')
	assert csrc.contains('struct _option_Item')
	assert csrc.contains('_option_Item){ .state = 2 }')
	assert !csrc.contains('.item = ((_option_Item){.state = 2,.name =')
}

fn test_generate_c_initializes_omitted_option_struct_field_in_named_literal_to_none() {
	csrc := generate_result_option_c_for_test('
module sample

interface IClone {}

struct Encoding implements IClone {
	label string
}

struct Config implements IClone {
	line_number bool
	encoding ?Encoding
	max_matches ?u64
}

fn make_config() Config {
	return Config{
		line_number: true
		max_matches: none
	}
}
	')
	assert csrc.contains('struct _option_sample__Encoding')
	assert csrc.contains('_option_sample__Encoding){ .state = 2 }')
	assert !csrc.contains('.encoding = ((_option_sample__Encoding){.state = 2,.label =')
}

fn test_generate_c_compares_struct_option_fields_fieldwise() {
	csrc := generate_result_option_c_for_test('
struct Mode {
	after ?usize
}

fn same(a Mode, b Mode) bool {
	return a == b
}
')
	assert csrc.contains('_cmp_l_')
	assert csrc.contains('.after.state == _cmp_r_')
	assert csrc.contains('.after.state != 0 ||')
	assert !csrc.contains('memcmp(&_cmp_l_')
}

fn test_generate_c_initializes_fixed_array_field_from_param_array() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	bitmap [4]u32
}

fn make(bitmap [4]u32) Holder {
	return Holder{
		bitmap: bitmap
	}
}
')
	assert csrc.contains('.bitmap = {bitmap[0], bitmap[1], bitmap[2], bitmap[3]}')
	assert !csrc.contains('.bitmap = bitmap')
}

fn test_generate_c_initializes_fixed_array_field_from_local_array() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	bitmap [4]u32
}

fn make() Holder {
	mut bitmap := [4]u32{}
	return Holder{
		bitmap: bitmap
	}
}
')
	assert csrc.contains('.bitmap = {bitmap[0], bitmap[1], bitmap[2], bitmap[3]}')
	assert !csrc.contains('.bitmap = bitmap')
}

fn test_generate_c_declares_specialized_generic_option_return() {
	csrc := generate_result_option_c_for_test('
module sample

struct Item {}

struct Match[T] {
	value T
	has_value bool
}

fn (m Match[T]) inner() ?T {
	if !m.has_value {
		return none
	}
	return m.value
}

fn use(m Match[Item]) bool {
	if value := m.inner() {
		_ = value
		return true
	}
	return false
}
	')
	assert csrc.contains('typedef struct _option_sample__Item _option_sample__Item;')
	assert csrc.contains('struct _option_sample__Item')
	assert csrc.contains('_option_sample__Item sample__Match_T_sample_Item__inner_T_sample_Item')
}

fn test_generate_c_expands_lifetime_generic_option_field_if_guard() {
	csrc := generate_result_option_c_for_test('
struct Searcher {}

struct Core[^s] {
	searcher &^s Searcher
mut:
	binary_byte_offset_ ?usize
	line_number ?u64
}

fn (core Core[^s]) binary_byte_offset[^s]() ?u64 {
	if offset := core.binary_byte_offset_ {
		return u64(offset)
	}
	return none
}

fn (mut core Core[^s]) detect_binary[^s]() bool {
	if _ := core.binary_byte_offset_ {
		return true
	}
	return false
}

fn (mut core Core[^s]) count_lines[^s]() {
	if line_number := core.line_number {
		core.line_number = line_number + 1
	}
}

fn use_it(searcher &Searcher) {
	mut core := Core{
		searcher: searcher
	}
	_ = core.binary_byte_offset()
	_ = core.detect_binary()
	core.count_lines()
}

fn main() {
	s := Searcher{}
	use_it(&s)
}
')
	assert csrc.contains('_option_usize _or_t')
	assert csrc.contains('usize offset = (*(usize*)')
	assert csrc.contains('u64 line_number = (*(u64*)')
	assert !csrc.contains('if (core.binary_byte_offset_)')
	assert !csrc.contains('if (core->binary_byte_offset_)')
	assert !csrc.contains('if (core->line_number)')
	assert !csrc.contains('u64 _val = ({ _option_u64')
}

fn test_generate_c_expands_result_unwrap_method_receiver_if_guard() {
	csrc := generate_result_option_c_for_test('
struct FallibleUsize {
	has_value bool
	value usize
}

fn FallibleUsize.some(value usize) FallibleUsize {
	return FallibleUsize{
		has_value: true
		value: value
	}
}

fn (opt FallibleUsize) get() ?usize {
	if !opt.has_value {
		return none
	}
	return opt.value
}

struct Core[^s] {
	marker &^s int
}

fn (mut core Core[^s]) shortest_match[^s]() !FallibleUsize {
	_ = core
	return FallibleUsize.some(1)
}

fn (mut core Core[^s]) use_it[^s]() !bool {
	if _ := core.shortest_match()!.get() {
		return true
	}
	return false
}

fn main() {
	x := 1
	mut core := Core{
		marker: &x
	}
	_ = core.use_it() or { false }
}
')
	assert csrc.contains('_result_FallibleUsize _or_t')
	assert csrc.contains('_option_usize _or_t')
	assert csrc.contains('FallibleUsize__get((*(FallibleUsize*)')
	assert !csrc.contains('array__get(((FallibleUsize)')
}

fn test_generate_c_uses_branch_local_type_for_if_expr_result() {
	csrc := generate_result_option_c_for_test('
struct Match {
	start_ usize
	end_ usize
}

fn Match.new(start usize, end usize) Match {
	return Match{
		start_: start
		end_: end
	}
}

fn (m Match) start() usize {
	return m.start_
}

fn (m Match) end() usize {
	return m.end_
}

fn (m Match) is_empty() bool {
	return m.start_ == m.end_
}

struct FallibleMatch {
	has_value bool
	value Match
}

fn (opt FallibleMatch) get() ?Match {
	if !opt.has_value {
		return none
	}
	return opt.value
}

fn use_it(maybe FallibleMatch) bool {
	invert_match := if line := maybe.get() {
		range := Match.new(0, line.start())
		range
	} else {
		range := Match.new(0, 10)
		range
	}
	return invert_match.is_empty()
}

fn main() {
	_ = use_it(FallibleMatch{})
}
')
	assert csrc.contains('Match invert_match = ({')
	assert !csrc.contains('int invert_match = ({')
	assert csrc.contains('Match__is_empty(invert_match)')
	assert !csrc.contains('int__is_empty(invert_match)')
}

fn test_generate_c_emits_c_struct_option_and_result_payload_wrappers() {
	csrc := generate_result_option_c_for_test('
struct C.Widget {
	x int
}

interface IError {}

fn optional_widget(w ?C.Widget) ?C.Widget {
	return none
}

fn result_widget() !C.Widget {
	return C.Widget{}
}
')
	assert csrc.contains('typedef struct _option_struct_Widget _option_struct_Widget;')
	assert csrc.contains('typedef struct _result_struct_Widget _result_struct_Widget;')
	assert csrc.contains('struct _option_struct_Widget { u8 state; IError err; u8 data[sizeof(struct Widget) > 1 ? sizeof(struct Widget) : 1]; };')
	assert csrc.contains('struct _result_struct_Widget { bool is_error; IError err; u8 data[sizeof(struct Widget) > 1 ? sizeof(struct Widget) : 1]; };')
	assert csrc.contains('struct Widget _val = ((struct Widget){0});')
	assert !csrc.contains('struct_Widget _val')
	assert !csrc.contains('sizeof(struct_Widget)')
}

fn test_generate_c_wraps_tuple_call_return_in_option_tuple_return() {
	csrc := generate_result_option_c_for_test('
fn decode() (string, string) {
	return "user", "pass"
}

fn credentials() ?(string, string) {
	return decode()
}
')
	assert csrc.contains('Tuple_string_string _val = decode();')
	assert !csrc.contains('.arg0 = decode()')
}

fn test_generate_c_emits_struct_str_function_when_interpolated() {
	csrc := generate_result_option_c_for_test('
struct Thing {
	x int
}

fn msg(t Thing) string {
	return "\${t}"
}
')
	assert csrc.contains('string Thing__str(Thing s)')
	assert csrc.contains('Thing__str(t).str')
}

fn test_generate_c_does_not_emit_unused_error_method_with_unwalked_str_dependency() {
	csrc := generate_markused_c_for_test('
struct Term {
	x int
}

struct MyError {
	term Term
}

fn (err MyError) msg() string {
	return "\${err.term}"
}

fn (err MyError) code() int {
	_ = err
	return 0
}

fn unused() ! {
	return MyError{}
}

fn test_unused_error() {
	assert true
}
	')
	assert !csrc.contains('string MyError__msg(MyError err)')
	assert !csrc.contains('Term__str((err).term)')
}

fn test_generate_c_does_not_force_emit_unused_interface_candidate_method_body() {
	csrc := generate_markused_c_for_test('
interface Describer {
	msg() string
}

struct Inner {
	text string
}

fn (inner Inner) str() string {
	return inner.text
}

struct ErrorLike {
	inner Inner
}

fn (err ErrorLike) msg() string {
	return err.inner.str()
}

fn main() {
	_ = 1
}
	')
	assert !csrc.contains('string ErrorLike__msg(ErrorLike err)')
	assert !csrc.contains('Inner__str((err).inner)')
}

fn test_generate_c_emits_used_struct_operator_method_body_with_markused() {
	csrc := generate_markused_c_for_test('
struct Stats {
	n int
}

fn (left Stats) + (right Stats) Stats {
	return Stats{
		n: left.n + right.n
	}
}

fn test_stats_plus() {
	left := Stats{
		n: 1
	}
	right := Stats{
		n: 2
	}
	sum := left + right
	assert sum.n == 3
}
')
	assert csrc.contains('Stats Stats__op_plus(Stats left, Stats right)')
	assert csrc.count('Stats__op_plus(') >= 2
}

fn test_generate_c_lowers_struct_operator_compound_assignment() {
	csrc := generate_markused_c_for_test('
struct Stats {
	n int
}

fn (left Stats) * (right Stats) Stats {
	return Stats{
		n: left.n * right.n
	}
}

fn test_stats_mul_assign() {
	mut y := Stats{
		n: 2
	}
	x := Stats{
		n: 3
	}
	y *= x
	assert y.n == 6
}
')
	assert csrc.contains('Stats Stats__op_mul(Stats left, Stats right)')
	assert csrc.contains('y = Stats__op_mul(y, x);')
	assert !csrc.contains('y *= x;')
}

fn test_generate_c_borrows_option_field_unwrap_payload_without_temp() {
	csrc := generate_result_option_c_for_test('
struct Holder {
	value ?string
}

fn (h &Holder) value_ref() ?&string {
	if h.value != none {
		return unsafe { &h.value? }
	}
	return none
}
	')
	assert !csrc.contains('.is_error')
	assert !csrc.contains('_or_t')
	assert csrc.contains('value.state != 0')
	assert csrc.contains('value.err)) + sizeof(IError)))')
}

fn test_generate_c_returns_custom_error_from_result_function_as_error() {
	csrc := generate_result_option_c_for_test('
struct SizeError {}

fn (err SizeError) msg() string {
	return "bad size"
}

fn (err SizeError) code() int {
	return 7
}

fn make_error() SizeError {
	return SizeError{}
}

fn parse() !u64 {
	return make_error()
}
	')
	assert csrc.contains('return (_result_u64){ .is_error=true, .err=')
	assert csrc.contains('IError_SizeError_msg_wrapper')
	assert csrc.contains('SizeError* _ierr_obj')
	assert !csrc.contains('u64 _val = main__make_error()')
}

fn test_generate_c_returns_ierror_local_from_result_function_as_error() {
	csrc := generate_result_option_c_for_test('
interface IError {
	msg() string
	code() int
}

struct MyError {}

fn (err MyError) msg() string {
	return "bad"
}

fn (err MyError) code() int {
	return 1
}

fn make_error() IError {
	return MyError{}
}

fn fail() !int {
	e := make_error()
	return e
}
	')
	assert csrc.contains('return (_result_int){ .is_error=true, .err=e };')
	assert !csrc.contains('int _val = e')
}

fn test_generate_c_returns_option_or_error_fallback_from_result_function_as_error() {
	csrc := generate_result_option_c_for_test('
interface IError {
	msg() string
}

struct MessageError {}

fn (err MessageError) msg() string {
	return "missing"
}

fn error(msg string) IError {
	return MessageError{}
}

struct Payload {
	value int
}

fn maybe_payload() ?Payload {
	return none
}

fn fail() !Payload {
	return maybe_payload() or { error("missing") }
}
	')
	assert csrc.contains('_option_Payload _or_t')
	assert csrc.contains('return (_result_Payload){ .is_error=true, .err=')
	assert !csrc.contains(' = main__error(')
}

fn test_generate_c_keeps_concrete_error_literal_when_context_is_concrete() {
	csrc := generate_result_option_c_for_test('
struct MyError {}

fn (err MyError) msg() string {
	return "bad"
}

fn (err MyError) code() int {
	return 1
}

fn main() {
	mut errors := []MyError{}
	err := MyError{}
	errors << err
}
	')
	assert csrc.contains('MyError err = ((MyError){')
	assert csrc.contains('array__push(((array*)(&errors)), &(MyError[1]){err});')
	assert !csrc.contains('IError err =')
	assert !csrc.contains('&(MyError[1]){((IError)')
}

fn test_generate_c_decl_assign_from_option_err_uses_ierror() {
	csrc := generate_result_option_c_for_test('
struct MyError {}

fn (err MyError) msg() string {
	return "bad"
}

fn (err MyError) code() int {
	return 1
}

fn maybe_value() ?int {
	return MyError{}
}

fn main() {
	_ := maybe_value() or {
		err := err
		_ = err
		return
	}
}
	')
	assert csrc.contains('IError err = _or_t')
	assert !csrc.contains('MyError err = _or_t')
}

fn test_generate_c_keeps_option_if_guard_err_as_concrete_error_ref() {
	csrc := generate_result_option_c_for_test('
struct MyError {}

fn (err MyError) msg() string {
	return "bad"
}

fn maybe_error() ?&MyError {
	return &MyError{}
}

fn read() string {
	if err := maybe_error() {
		return err.msg()
	}
	return ""
}
	')
	assert csrc.contains('MyError__msg((*err))')
	assert !csrc.contains('err->_object')
	assert !csrc.contains('MyError__msg((*err),')
}

fn test_generate_c_casts_concrete_arg_for_mut_interface_param() {
	csrc := generate_result_option_c_for_test('
interface Reader {
mut:
	read(mut []u8) !int
}

struct ByteReader {}

fn (mut rdr ByteReader) read(mut buf []u8) !int {
	return 0
}

fn consume(mut rdr Reader) !int {
	mut buf := []u8{len: 4}
	return rdr.read(mut buf)
}

fn demo() !int {
	mut rdr := ByteReader{}
	return consume(mut rdr)
}
	')
	assert csrc.contains('Reader* _iface_t')
	assert csrc.contains('ByteReader__read')
	assert !csrc.contains('consume(&rdr)')
}

fn test_generate_c_unwraps_interface_method_result_payload_for_propagation() {
	csrc := generate_result_option_c_for_test('
interface Reader {
mut:
	read(mut []u8) !int
}

fn read_full(mut reader Reader, mut buf []u8) ! {
	mut offset := 0
	for offset < buf.len {
		n := reader.read(mut buf[offset..])!
		offset += n
	}
}
	')
	assert csrc.contains('_result_int _or_t')
	assert csrc.contains('int n = (*(int*)')
	assert !csrc.contains('int n = ;')
}

fn test_generate_c_unwraps_generic_comptime_interface_method_result_payload() {
	csrc := generate_result_option_c_for_test_files([
		'
module core

import printer

struct BufferWriter implements printer.WriteColor {}

fn (mut w BufferWriter) write(buf []u8) !int {
	_ = w
	return buf.len
}

fn demo() !int {
	mut standard := printer.Standard.new(BufferWriter{})
	return standard.write([]u8{})
}
',
		'
module printer

pub interface WriteColor {
mut:
	write([]u8) !int
}

pub struct CounterWriter[W] {
mut:
	wtr W
}

pub fn CounterWriter.new[W](wtr W) CounterWriter[W] {
	return CounterWriter[W]{
		wtr: wtr
	}
}

pub fn (mut w CounterWriter[W]) write(buf []u8) !int {
	$if W is WriteColor {
		n := w.wtr.write(buf)!
		return n
	} $else {
		_ = buf
		return 0
	}
}

pub struct Standard[W] {
mut:
	wtr CounterWriter[W]
}

pub fn Standard.new[W](wtr W) Standard[W] {
	return Standard[W]{
		wtr: CounterWriter.new(wtr)
	}
}

pub fn (mut s Standard[W]) write(buf []u8) !int {
	return s.wtr.write(buf)
}
	',
	])
	assert csrc.contains('_result_int printer__CounterWriter_T_core_BufferWriter__write_T_core_BufferWriter')
	assert csrc.contains('core__BufferWriter__write(&w->wtr, buf)')
	assert csrc.contains('int n = (*(int*)')
	assert !csrc.contains('int n = ;')
}

fn test_generate_c_preserves_static_lifetime_constructor_and_interface_pointer_field() {
	csrc := generate_result_option_c_for_test('
interface Reader {
mut:
	read(mut []u8) !int
}

struct ByteReader {}

fn (mut rdr ByteReader) read(mut buf []u8) !int {
	return 0
}

struct Config {}

struct TranscodingReader[^r] {
mut:
	rdr &^r Reader
	config Config
}

fn TranscodingReader.new[^r](rdr &^r Reader, config Config) TranscodingReader[^r] {
	return TranscodingReader[^r]{
		rdr: rdr
		config: config
	}
}

fn (mut rdr TranscodingReader[^r]) read[^r](mut buf []u8) !int {
	return rdr.rdr.read(mut buf)
}

struct LineBuffer {}

struct LineBufferReader[^r, ^b] {
mut:
	rdr &^r Reader
	buf &^b LineBuffer
}

fn LineBufferReader.new[^r, ^b](rdr &^r Reader, buf &^b LineBuffer) LineBufferReader[^r, ^b] {
	return LineBufferReader[^r, ^b]{
		rdr: rdr
		buf: buf
	}
}

fn use_it(mut read_from Reader, config Config, buf &LineBuffer) ! {
	mut decoded := TranscodingReader.new(&read_from, config)
	mut rdr := LineBufferReader.new(&decoded, buf)
	_ = rdr
}
	')
	assert csrc.contains('return ((TranscodingReader){.rdr = rdr,.config = config})')
	assert csrc.contains('return ((LineBufferReader){.rdr = rdr,.buf = buf})')
	assert csrc.contains('LineBufferReader rdr = LineBufferReader__new(')
	assert !csrc.contains('TranscodingReader rdr = TranscodingReader__new')
	assert !csrc.contains('Config__read')
	assert !csrc.contains('LineBuffer__read')
}

fn test_generate_c_preserves_c_pointer_cast_selector_field_access() {
	csrc := generate_result_option_c_for_test('
@[typedef]
struct C.log__Logger {
mut:
	_object voidptr
}

interface Logger {
mut:
	free()
}

fn raw_object(logger &Logger) voidptr {
	unsafe {
		pobject := &C.log__Logger(logger)._object
		return pobject
	}
}
	')
	assert csrc.contains('pobject = ((log__Logger*)(logger))->_object;')
	assert !csrc.contains('voidptr* pobject')
	assert !csrc.contains('&logger->_object')
}

fn test_generate_c_preserves_embedded_error_concrete_type_name() {
	csrc := generate_result_option_c_for_test('
struct Error {}

fn (err Error) msg() string {
	return ""
}

fn (err Error) code() int {
	return 0
}

struct Eof {
	Error
}

fn read() !int {
	return Eof{}
}

fn demo() bool {
	_ := read() or {
		return err is Eof
	}
	return false
}
	')
	assert csrc.contains('IError_Eof_type_name_wrapper')
	assert csrc.contains('IError_Eof_msg_wrapper')
	assert !csrc.contains('.type_name = IError_Error_type_name_wrapper')
}

fn test_generate_c_does_not_emit_nested_generic_structs_with_placeholder_args() {
	csrc := generate_result_option_c_for_test('
struct NoColor[W] {
mut:
	wtr W
}

struct CounterWriter[W] {
mut:
	wtr W
}

struct Summary[W] {
mut:
	wtr CounterWriter[W]
}

fn build_no_color[W](wtr W) Summary[NoColor[W]] {
	_ = wtr
	return Summary[NoColor[W]]{}
}
')
	assert !csrc.contains('struct CounterWriter {\n\tNoColor wtr;')
	assert !csrc.contains('struct Summary {\n\tCounterWriter wtr;')
}

fn test_generate_c_orders_nested_generic_struct_dependencies() {
	csrc := generate_result_option_c_for_test('
struct PlainWriter {}

struct NoColor[W] {
mut:
	wtr W
}

struct CounterWriter[W] {
mut:
	wtr W
}

struct Direct[W] {
mut:
	wtr CounterWriter[W]
}

struct JSON[W] {
mut:
	wtr CounterWriter[NoColor[W]]
}

fn build_direct[W](wtr W) Direct[W] {
	_ = wtr
	return Direct[W]{}
}

fn build[W](wtr W) JSON[W] {
	_ = wtr
	return JSON[W]{}
}

fn demo() {
	_ := build_direct(PlainWriter{})
	_ := build(PlainWriter{})
}
	')
	dep_pos := csrc.index('struct CounterWriter_T_NoColor_T_PlainWriter {') or {
		panic('missing concrete nested generic struct body')
	}
	user_pos := csrc.index('struct JSON_T_PlainWriter {') or {
		panic('missing generic user struct body')
	}
	assert dep_pos < user_pos
}

fn test_generate_c_preserves_primitive_generic_struct_binding() {
	csrc := generate_result_option_c_for_test('
struct Box[T] {
	value T
}

fn make() Box[int] {
	return Box[int]{
		value: 1
	}
}

fn main() {
	box := make()
	_ = box.value
}
')
	assert csrc.contains('struct Box {\n\tint value;')
	assert csrc.contains('Box make()')
	assert !csrc.contains('f64 make()')
	assert !csrc.contains('f64 box = make()')
}

fn test_generate_c_preserves_nested_primitive_generic_struct_binding() {
	csrc := generate_result_option_c_for_test('
struct Inner[T] {
	value T
}

struct Outer[T] {
	inner Inner[T]
}

fn make() Outer[int] {
	return Outer[int]{
		inner: Inner[int]{
			value: 1
		}
	}
}

fn main() {
	outer := make()
	_ = outer.inner.value
}
	')
	assert csrc.contains('struct Inner {\n\tint value;')
	assert csrc.contains('struct Outer {\n\tInner inner;')
		|| csrc.contains('struct Outer {\n\tInner_T_int inner;')
	assert csrc.contains('Outer make()')
	assert !csrc.contains('f64 make()')
	assert !csrc.contains('f64 outer = make()')
	assert !csrc.contains('((f64){.inner = ((f64){.value = 1})})')
}

fn test_generate_c_skips_interface_clone_for_incomplete_generic_implementor() {
	csrc := generate_result_option_c_for_test('
interface Writer {
mut:
	write() !int
}

struct Wrapper[W] {
mut:
	wtr W
}

fn (mut w Wrapper[W]) write() !int {
	_ = w
	return 0
}

fn wrap[W](wtr W) Wrapper[W] {
	return Wrapper[W]{
		wtr: wtr
	}
}
')
	assert !csrc.contains('sizeof(Wrapper)')
	assert !csrc.contains('Wrapper__write(Wrapper* w) {')
}

fn test_generate_c_names_generic_sum_type_variant_fields_from_base_type() {
	csrc := generate_result_option_c_for_test('
struct A[T] {
	x T
}

struct B[T] {
	x T
}

type P = A[int] | B[int]

fn make() P {
	return P(A[int]{
		x: 1
	})
}
')
	assert csrc.contains('void* _A;')
	assert csrc.contains('void* _B;')
	assert csrc.contains('._data._A =')
	assert !csrc.contains('void* _v0;')
	assert !csrc.contains('._data._v0 =')
}

fn test_generate_c_does_not_rewrap_explicit_sumtype_cast_in_result_return() {
	csrc := generate_result_option_c_for_test('
struct Null {}

type Primitive = Null | bool | []Primitive

fn make() !Primitive {
	return Primitive(true)
}
')
	assert csrc.contains('Primitive _val = ((Primitive){._tag = 1, ._data._bool')
	assert !csrc.contains('Primitive _val = ((Primitive){._tag = 2,._data._Array_Primitive')
}

fn test_generate_c_uses_qualified_ierror_wrapper_for_module_error() {
	csrc := generate_result_option_c_for_test('
module sample

struct ParseError {}

fn (err ParseError) msg() string {
	return ""
}

fn (err ParseError) code() int {
	return 0
}

fn read() !int {
	return ParseError{}
}
')
	assert csrc.contains('IError_sample__ParseError_msg_wrapper')
	assert !csrc.contains('IError_ParseError_msg_wrapper')
}

fn test_generate_c_uses_qualified_ierror_wrapper_for_module_ierror_return() {
	csrc := generate_result_option_c_for_test('
module sample

interface IError {
	msg() string
}

struct ParseError {}

fn (err ParseError) msg() string {
	return ""
}

fn parse_error() IError {
	return ParseError{}
}
')
	assert csrc.contains('.msg = IError_sample__ParseError_msg_wrapper')
	assert !csrc.contains('.msg = IError_ParseError_msg_wrapper')
}

fn test_sum_type_variant_field_name_preserves_module_qualified_variant() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Stmt'] = ['ast__FnDecl', 'ast__EmptyStmt']
	assert gen.sum_type_variant_field_name('ast__Stmt', 'FnDecl') == 'ast__FnDecl'
	assert gen.sum_type_variant_field_name('ast__Stmt', 'ast__FnDecl') == 'ast__FnDecl'
	gen.sum_type_variants['json2__Any'] = ['Array_json2__Any', 'Map_string_json2__Any']
	assert gen.sum_type_variant_field_name('json2__Any', 'Array_json2__Any') == 'Array_json2__Any'
	assert gen.sum_type_variant_field_name('json2__Any', 'Map_string_json2__Any') == 'Map_string_json2__Any'
	assert gen.sum_type_variant_field_name('json2__Any', 'Map_string_Any') == 'Map_string_json2__Any'
	gen.sum_type_variants['ast__Type'] = ['ast__FnType']
	assert gen.sum_type_variant_field_name('ast__Type', 'types__FnType') == 'types__FnType'
	if _ := gen.sum_variant_tag_path('ast__Type', 'types__FnType', []string{}) {
		assert false
	} else {
		assert true
	}
}

fn test_sum_data_variant_selector_field_preserves_module_qualified_variant() {
	mut gen := Gen.new([])
	gen.sum_type_variants['ast__Stmt'] = ['ast__FnDecl', 'ast__EmptyStmt']
	gen.remember_runtime_local_type('stmt', 'ast__Stmt')
	sel := ast.SelectorExpr{
		lhs: ast.SelectorExpr{
			lhs: ast.Ident{
				name: 'stmt'
			}
			rhs: ast.Ident{
				name: '_data'
			}
		}
		rhs: ast.Ident{
			name: '_FnDecl'
		}
	}
	assert gen.sum_data_variant_selector_field(sel) or { '' } == '_ast__FnDecl'
}

fn test_sum_narrowed_selector_uses_payload_base_type_for_pointer_env_type() {
	mut env := types.Environment.new()
	env.set_expr_type(51, types.Type(types.Pointer{
		base_type: types.Struct{
			name: 'ast__Ident'
		}
	}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__Expr'] = ['ast__Ident']
	gen.remember_runtime_local_type('rhs', 'ast__Expr')
	gen.expr(ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'rhs'
			pos:  token.Pos{
				id: 51
			}
		}
		rhs: ast.Ident{
			name: 'name'
		}
	}))
	out := gen.sb.str()
	assert out.contains('((ast__Ident*)')
	assert !out.contains('ast__Ident**')
}

fn test_sum_cast_keeps_selector_with_declared_sum_field_type() {
	mut env := types.Environment.new()
	env.set_expr_type(62, types.Type(types.Struct{
		name: 'ast__SelectorExpr'
	}))
	mut gen := Gen.new_with_env([], env)
	gen.sum_type_variants['ast__Expr'] = ['ast__Ident', 'ast__SelectorExpr']
	gen.struct_field_types['ast__CallExpr.lhs'] = 'ast__Expr'
	gen.remember_runtime_local_type('expr', 'ast__CallExpr')
	sel := ast.Expr(ast.SelectorExpr{
		lhs: ast.Ident{
			name: 'expr'
		}
		rhs: ast.Ident{
			name: 'lhs'
		}
		pos: token.Pos{
			id: 62
		}
	})
	gen.gen_type_cast_expr('ast__Expr', sel)
	assert gen.sb.str() == 'expr.lhs'
}

fn test_generate_c_uses_payload_base_type_for_smartcast_map_key_selector() {
	csrc := generate_result_option_c_for_test('
struct Ident {
	name string
}

struct SelectorExpr {
	lhs Expr
}

type Expr = Ident | SelectorExpr

fn lookup(mut const_types map[string]string, rhs Expr) {
	if rhs is Ident {
		if ct := const_types[rhs.name] {
			_ = ct
		}
	}
}
')
	assert csrc.contains('map__get_check')
	assert !csrc.contains('Ident**')
}

fn test_generate_c_uses_nested_sum_smartcast_for_selector_field_methods() {
	csrc := generate_result_option_c_for_test('
struct BasicLiteral {
	kind  int
	value string
}

struct SelectorExpr {
	pos Pos
}

struct Pos {
	id int
}

fn (p Pos) is_valid() bool {
	return p.id > 0
}

type Expr = BasicLiteral | SelectorExpr

struct Field {
	value Expr
}

fn is_float_field(field Field) bool {
	return field.value is BasicLiteral && field.value.kind == 1
		&& field.value.value.contains(".")
}

fn has_valid_pos(expr Expr) bool {
	if expr is SelectorExpr {
		return expr.pos.is_valid()
	}
	return false
}
')
	assert csrc.contains('string__contains(')
	assert !csrc.contains('int__contains(')
	assert !csrc.contains('field.value.value')
	assert csrc.contains('Pos__is_valid(')
	assert !csrc.contains('int__is_valid(')
}

fn test_generate_c_keeps_declared_sum_selector_field_cast_identity() {
	csrc := generate_result_option_c_for_test('
struct Ident {
	name string
}

struct SelectorExpr {
	lhs Expr
}

struct EmptyExpr {}

struct CallExpr {
	lhs Expr
}

type Expr = EmptyExpr | Ident | SelectorExpr

fn keep_lhs(expr CallExpr) Expr {
	if expr.lhs is SelectorExpr {
		return Expr(expr.lhs)
	}
	return Expr(EmptyExpr{})
}
')
	assert !csrc.contains('= expr.lhs; memdup')
}

fn test_generate_c_passes_declared_sum_selector_field_arg_without_rewrap() {
	csrc := generate_result_option_c_for_test('
struct Ident {
	name string
}

struct SelectorExpr {
	lhs Expr
}

struct EmptyExpr {}

struct CallExpr {
	lhs Expr
}

type Expr = EmptyExpr | Ident | SelectorExpr

fn use_expr(arg Expr) {
	_ = arg
}

fn keep_lhs(expr CallExpr) {
	if expr.lhs is SelectorExpr {
		use_expr(expr.lhs)
	}
}
')
	assert !csrc.contains('= expr.lhs; memdup')
}

fn test_generate_c_initializes_fixed_array_struct_field_from_literal() {
	csrc := generate_result_option_c_for_test('
struct Process {
	stdio_fd [3]int
}

fn new_process() &Process {
	return &Process{
		stdio_fd: [-1, -1, -1]!
	}
}
')
	assert csrc.contains('.stdio_fd = {-1, -1, -1}')
	assert !csrc.contains('{{-1, -1, -1}[0]')
}

fn test_generate_c_renames_builtin_panic_call() {
	csrc := generate_result_option_c_for_test('
fn panic(s string) {
	_ = s
}

fn main() {
	panic("x")
}
')
	assert csrc.contains('v_panic((string){.str = "x"')
	assert !csrc.contains('\tpanic((string){.str = "x"')
}

fn test_generate_c_panics_for_bang_in_non_result_main() {
	csrc := generate_result_option_c_for_test('
struct App {}

fn new_app() !&App {
	return &App{}
}

fn main() {
	mut app := new_app()!
	_ = app
}
')
	assert csrc.contains('v_panic(IError__str(err));')
	assert !csrc.contains('return err;')
}

fn test_generate_c_returns_none_for_result_or_none_in_option_return() {
	csrc := generate_result_option_c_for_test('
fn find_path() !string {
	return "git"
}

fn get_path() ?string {
	return find_path() or { none }
}
')
	assert csrc.contains('return (_option_string){ .state = 2 };')
	assert !csrc.contains('/* [TODO] Type */ 0')
}

fn test_generate_c_lowers_sql_create_table_to_orm_driver_call() {
	csrc := generate_result_option_c_for_test_files([
		'
import orm

struct DB {}

fn (db DB) create(table orm.Table, fields []orm.TableField) ! {}

enum RepoStatus {
	open
	closed
}

struct Repo {
	id      int    @[primary; sql: serial]
	name    string @[unique: "repo"]
	status  RepoStatus
	skip_me string @[skip]
}

fn create(mut db DB) ! {
	sql db {
		create table Repo
	}!
}
',
		'
module orm

pub const enum_ = -3
pub const type_string = 20
pub const type_idx = {
	"int": 7
}

pub struct Table {}

pub struct TableField {}
',
	])
	assert csrc.contains('DB__create(')
	assert csrc.contains('orm__Table')
	assert csrc.contains('orm__TableField')
	assert csrc.contains('"repo"')
	assert csrc.contains('"id"')
	assert csrc.contains('"primary"')
	assert csrc.contains('"sql"')
	assert csrc.contains('"serial"')
	assert csrc.contains('"unique"')
	assert csrc.contains('.typ = 7')
	assert csrc.contains('.typ = -3')
	assert !csrc.contains('"skip_me"')
	assert !csrc.contains('((_result_void){0})')
	assert !csrc.contains('/* [TODO] SqlExpr */ 0')
}

fn test_generate_c_sql_or_array_placeholder_has_no_result_probe() {
	csrc := generate_result_option_c_for_test('
struct DB {}
struct TwoFactor {}

fn load(mut db DB) []TwoFactor {
	rows := sql db {
		select from TwoFactor
	} or {
		[]TwoFactor{}
	}
	return rows
}
')
	assert csrc.contains('Array_TwoFactor rows = ((Array_TwoFactor){0});')
	assert !csrc.contains('Array_TwoFactor _or_t')
	assert !csrc.contains('.is_error')
}
