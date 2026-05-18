// vtest build: !linux && !windows
module cleanc

import os
import v2.ast
import v2.markused
import v2.parser
import v2.pref as vpref
import v2.token
import v2.transformer
import v2.types

fn generate_result_option_c_for_test(code string) string {
	tmp_file := os.join_path(os.temp_dir(), 'v2_result_option_codegen_test_${os.getpid()}.v')
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
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
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
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
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
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
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
	mut trans := transformer.Transformer.new_with_pref(files, env, prefs)
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
	mut lines := []string{}
	for alias in aliases() {
		lines << "    \${alias.name}:\\"\${alias.description}\\""
	}
	return lines.join("\\n")
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
	assert csrc.contains('_option_sample__Item sample__Match__inner')
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
	assert csrc.contains('Stats Stats__plus(Stats left, Stats right)')
	assert csrc.count('Stats__plus(') >= 2
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
	dep_pos := csrc.index('struct CounterWriter_T_NoColor {') or {
		panic('missing concrete nested generic struct body')
	}
	user_pos := csrc.index('struct JSON {') or { panic('missing generic user struct body') }
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
