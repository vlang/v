import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn review_v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_review_transform_regressions_test')
}

fn review_v3_ownership_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_review_transform_ownership_regressions_test')
}

fn testsuite_begin() {
	v3_bin := review_v3_bin_path()
	if os.exists(v3_bin) {
		os.rm(v3_bin) or {}
	}
	ownership_bin := review_v3_ownership_bin_path()
	if os.exists(ownership_bin) {
		os.rm(ownership_bin) or {}
	}
	// The test runner executes test functions in parallel. Build both shared
	// compiler fixtures here so workers never race while replacing the same bin.
	_ = build_v3_review_transform()
	_ = build_v3_review_transform_ownership()
}

fn build_v3_review_transform() string {
	v3_bin := review_v3_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${vexe} -prealloc -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn build_v3_review_transform_ownership() string {
	v3_bin := review_v3_ownership_bin_path()
	if os.exists(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${vexe} -prealloc -d ownership -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn run_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} -nocache ${bad_src} -b c -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('C compilation failed'), '${name}: reached C compilation\n${result.output}'
}

fn run_bad_backend(v3_bin string, name string, backend string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	result := os.execute('${v3_bin} -nocache -b ${backend} ${bad_src} -o ${bad_bin}')
	assert result.exit_code != 0, '${name}: expected failure, got success\n${result.output}'
	assert result.output.contains(expected), '${name}: expected `${expected}` in\n${result.output}'
	assert !result.output.contains('build_expr: unsupported expr kind'), '${name}: reached SSA lowering\n${result.output}'
}

fn run_good(v3_bin string, name string, src string) string {
	return run_good_with_flags(v3_bin, name, '', src)
}

fn run_good_with_flags(v3_bin string, name string, flags string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} -nocache ${flags} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn run_good_with_env(v3_bin string, name string, env string, src string) string {
	good_src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${env} ${v3_bin} -nocache ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn test_recursive_interface_equality_stops_expanding_seen_interfaces() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'recursive_interface_equality', 'interface Value {}

struct Box {
	values []Value
}

fn main() {
	left := Value(Box{})
	right := Value(Box{})
	println(left == right)
}
')
	assert out == 'true'
}

fn test_array_method_on_imported_global_keeps_global_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_method_on_imported_global', 'import os

fn main() {
	println(os.args.last().len > 0)
}
')
	assert out == 'true'
}

fn test_explicit_generic_call_keeps_container_and_imported_struct_types() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'explicit_generic_call_result_context', 'import time

fn identity[T](value T) T {
	return value
}

fn generic_map[M]() map[string]M {
	return map[string]M{}
}

struct Box[T] {
	value T
}

fn main() {
	values := identity[map[string]string]({
		"answer": "yes"
	})
	moment := identity[time.Time](time.Time{
		year: 2024
	})
	box := Box[int]{}
	println(values["answer"])
	println(moment.year)
	println(typeof(box.value).name)
	println(typeof(generic_map[string]()).name)
}
')
	assert out == 'yes\n2024\nint\nmap[string]string'
}

fn test_sum_equality_uses_canonical_struct_field_types() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_flags(v3_bin, 'sum_equality_canonical_struct_field_types', '-building-v', 'import v.token

struct Box {
	pos token.Pos
}

type Value = Box | int

fn main() {
	left := Value(Box{
		pos: token.Pos{
			line_nr: 1
		}
	})
	right := Value(Box{
		pos: token.Pos{
			line_nr: 1
		}
	})
	println(left == right)
}
')
	assert out == 'true'
}

fn test_map_interface_equality_keeps_typed_map_get() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_interface_equality_typed_get', 'interface Value {
	n int
}

struct Item {
	n int
}

fn main() {
	left := {
		"item": Value(Item{
			n: 7
		})
	}
	right := {
		"item": Value(Item{
			n: 7
		})
	}
	println(left == right)
}
')
	assert out == 'true'
}

fn test_array_append_to_map_value_struct_field_uses_mutable_map_entry() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_append_to_map_value_struct_field', 'struct Item {
mut:
	values []int
}

fn append_value(mut items map[string]Item, key string, value int) {
	items[key].values << value
}

fn main() {
	mut items := {
		"first": Item{}
	}
	append_value(mut items, "first", 7)
	println(items["first"].values)
}
')
	assert out == '[7]'
}

fn test_or_block_match_with_break_has_no_value_assignment() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'or_block_match_break', 'struct NoIdle {}

fn (_ NoIdle) msg() string {
	return "no idle value"
}

fn (_ NoIdle) code() int {
	return 0
}

fn pop_idle() !int {
	return NoIdle{}
}

fn get() !int {
	for {
		value := pop_idle() or {
			match err {
				NoIdle {
					break
				}
				else {
					return err
				}
			}
		}
		return value
	}
	return 7
}

fn main() {
	println(get() or { panic(err) })
}
')
	assert out == '7'
}

fn test_result_payload_converts_to_interface_and_sum_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'result_interface_and_sum_payload', 'interface Number {
	number() int
}

struct Item {
	value int
}

fn (item Item) number() int {
	return item.value
}

struct ArrayValue {
	value int
}

type Value = ArrayValue | string

fn load_item() !Item {
	return Item{7}
}

fn load_number() !Number {
	return load_item()!
}

fn load_array() !ArrayValue {
	return ArrayValue{9}
}

fn load_value() !Value {
	return load_array()!
}

fn main() {
	number := load_number() or { panic(err) }
	value := load_value() or { panic(err) }
	println(number.number())
	match value {
		ArrayValue { println(value.value) }
		else {}
	}
}
')
	assert out == '7\n9'
}

fn test_interface_pointer_receiver_dispatch_preserves_pointer_depth() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_pointer_receiver_dispatch', 'interface Reader {
	read() int
}

struct Value {
	n int
}

fn (value Value) read() int {
	return value.n
}

fn use(reader &Reader) int {
	return reader.read()
}

fn main() {
	reader := Reader(Value{
		n: 42
	})
	println(use(&reader))
}
')
	assert out == '42'
}

fn test_interface_field_receiver_preserves_smartcast_projection() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_field_receiver_smartcast', 'interface Reader {
	read() int
}

struct Source {
	n int
}

fn (source Source) read() int {
	return source.n
}

struct Holder {
	reader Reader
}

type Value = Holder | string

fn read_value(value Value) int {
	return match value {
		Holder { value.reader.read() }
		else { -1 }
	}
}

fn main() {
	value := Value(Holder{
		reader: Reader(Source{
			n: 37
		})
	})
	println(read_value(value))
}
')
	assert out == '37'
}

fn test_result_c_payload_does_not_use_colliding_interface() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'result_c_payload_interface_collision', {
		'v.mod':     "Module { name: 'result_c_payload_interface_collision' }\n"
		'native.h':  'typedef struct { int value; } Value;\n'
		'pkg/pkg.v': 'module pkg

pub interface Value {
	number() int
}

pub fn read(value Value) int {
	return value.number()
}
'
		'main.v':    'module main

#insert "native.h"

import pkg

@[typedef]
struct C.Value {
	value int
}

struct Box {
	n int
}

fn (box Box) number() int {
	return box.n
}

fn load() !C.Value {
	return C.Value{
		value: 41
	}
}

fn main() {
	native := load() or { panic(err) }
	println(native.value)
	println(pkg.read(Box{
		n: 43
	}))
}
'
	}, 'main.v')
	assert out == '41\n43'
}

fn test_issue_28180_module_collisions_and_embedded_generic_middleware() {
	v3_bin := build_v3_review_transform()
	// The reported regression is a compiler failure; keep unrelated runtime
	// deinitialization outside this coverage.
	_ = compile_good_project(v3_bin, 'issue_28180_module_collisions', '-nocache', {
		'v.mod':     'Module {
	name: "issue_28180"
}
'
		'main.v':    'module main

import app

fn main() {
	_ = app.new()
	println("ok")
}
'
		'app/app.v': 'module app

import context
import veb

pub struct Context {
	veb.Context
}

@[heap]
pub struct App {
	veb.Middleware[Context]
}

struct Params {
	name ?string
}

fn (mut app App) middleware(mut ctx Context) bool {
	return true
}

pub fn new() &App {
	mut base_ctx := context.background()
	_ = base_ctx.done()
	params := Params{
		name: "item"
	}
	if name := params.name {
		assert name == "item"
	}
	mut app := &App{}
	app.use(handler: app.middleware)
	app.route_use("/admin", handler: app.middleware, after: true)
	return app
}
'
	}, '')
}

fn test_return_match_map_lookup_guard_keeps_presence_check() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_flags(v3_bin, 'return_match_map_lookup_guard', '-building-v', 'struct Local {
	typ string
}

struct Parser {
	locals         map[string]Local
	constant_types map[string]string
}

fn (p &Parser) infer(name string) !string {
	return match name {
		"known" {
			if local := p.locals[name] {
				local.typ
			} else if typ := p.constant_types[name] {
				typ
			} else {
				"missing"
			}
		}
		else {
			"other"
		}
	}
}

fn main() {
	local_parser := Parser{
		locals: {
			"known": Local{
				typ: "local"
			}
		}
	}
	constant_parser := Parser{
		constant_types: {
			"known": "constant"
		}
	}
	println(local_parser.infer("known") or { panic(err) })
	println(constant_parser.infer("known") or { panic(err) })
}
')
	assert out == 'local\nconstant'
}

fn test_pointer_interface_field_as_interface_uses_storage_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_interface_field_as_interface_storage', 'interface Layout {
	size() int
}

interface Widget {
	size() int
	draw()
}

interface Application {
	layout &Layout
}

struct Item {}

fn (_ Item) size() int {
	return 7
}

fn (_ Item) draw() {}

struct App {
	layout &Layout
}

fn application_widget(app Application) Widget {
	if app.layout is Widget {
		widget := app.layout as Widget
		return widget
	}
	return Widget(Item{})
}

fn main() {
	layout := Layout(Item{})
	app := Application(App{
		layout: &layout
	})
	println(application_widget(app).size())
}
')
	assert out == '7'
}

fn test_interface_dispatch_reboxes_result_pointer_payload() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_dispatch_result_pointer_payload', 'interface Connection {
	close() !
}

struct TcpConn {}

fn (_ &TcpConn) close() ! {}

interface Dialer {
	dial(string) !Connection
}

struct Proxy {}

fn (_ &Proxy) dial(_ string) !&TcpConn {
	return &TcpConn{}
}

fn main() {
	dialer := Dialer(&Proxy{})
	connection := dialer.dial("") or { panic(err) }
	connection.close() or { panic(err) }
	println("ok")
}
')
	assert out == 'ok'
}

fn test_generic_shared_parameter_value_copy_uses_inner_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_shared_parameter_value_copy', 'struct State {
	label string
}

fn destroy(shared state State) {
	rlock state {
		drop_owned(state)
	}
}

fn main() {
	shared state := State{
		label: "ok"
	}
	destroy(shared state)
	println("done")
}
')
	assert out == 'done'
}

fn test_c_pointer_zero_argument_stays_null() {
	v3_bin := build_v3_review_transform()
	generated := gen_c_from_source(v3_bin, 'c_pointer_zero_argument', 'fn C.wait(&int) int

fn call_wait() int {
	return C.wait(0)
}

fn main() {
	_ = call_wait()
}
')
	assert generated.contains('return wait(0);'), generated
	assert !generated.contains('wait(&0)'), generated
}

fn test_system_libc_mode_preserves_ptrace_header() {
	v3_bin := build_v3_review_transform()
	generated := gen_c_from_source(v3_bin, 'system_libc_ptrace_header', '#include <math.h>
#include <sys/ptrace.h>

fn main() {}
')
	assert generated.contains('#include <sys/ptrace.h>'), generated
}

fn test_for_in_binding_shadows_module_const_during_method_lowering() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'for_in_binding_shadows_module_const', {
		'main.v':        "import loops

const v = 'global'

fn main() {
	println(loops.values())
}
"
		'loops/loops.v': 'module loops

pub fn values() string {
	values := [u16(15), 16]!
	mut parts := []string{}
	for v in values {
		parts << v.hex()
	}
	return parts.join(",")
}
'
	}, 'main.v')
	assert out == 'f,10'
}

fn gen_c_from_source(v3_bin string, name string, src string) string {
	return gen_c_from_source_with_flags(v3_bin, name, '', src)
}

fn gen_c_from_source_with_flags(v3_bin string, name string, flags string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${flags} ${src_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: ${compile.output}'
	assert os.exists(c_path)
	return os.read_file(c_path) or { panic(err) }
}

fn c_fn_body(c_source string, signature string) string {
	start := c_source.index(signature) or { return '' }
	open_rel := c_source[start..].index('{') or { return '' }
	body_start := start + open_rel
	mut depth := 0
	for i in body_start .. c_source.len {
		if c_source[i] == `{` {
			depth++
		} else if c_source[i] == `}` {
			depth--
			if depth == 0 {
				return c_source[start..i + 1]
			}
		}
	}
	return c_source[start..]
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn run_good_project(v3_bin string, name string, files map[string]string, input string) string {
	return run_good_project_with_flags(v3_bin, name, '', files, input)
}

fn run_good_project_with_flags(v3_bin string, name string, flags string, files map[string]string, input string) string {
	good_bin := compile_good_project(v3_bin, name, flags, files, input)
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn compile_good_project(v3_bin string, name string, flags string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	good_bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} ${flags} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	assert !compile.output.contains('gen_node: unsupported node kind'), '${name}: unsupported node reached C generation\n${compile.output}'

	return good_bin
}

fn gen_c_from_project(v3_bin string, name string, files map[string]string, input string) string {
	return gen_c_from_project_with_flags(v3_bin, name, '', files, input)
}

fn gen_c_from_project_with_flags(v3_bin string, name string, flags string, files map[string]string, input string) string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_project')
	if os.exists(root) {
		os.rmdir_all(root) or { panic(err) }
	}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := if input.len == 0 { root } else { os.join_path(root, input) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${flags} ${input_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: C generation failed\n${compile.output}'
	return os.read_file(c_path) or { panic(err) }
}

fn test_lifted_fn_literal_mut_param_interpolation_derefs_value() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'lifted_literal_mut_param_interpolation', 'struct Counter {\n\tvalue int\n}\n\nfn main() {\n\tmut counter := Counter{\n\t\tvalue: 7\n\t}\n\tf := fn (mut value Counter) {\n\t\tprintln("\${value.value}")\n\t}\n\tf(mut counter)\n}\n')
	assert out == '7'
}

fn test_auto_str_preserves_distinct_structs_beyond_inline_depth() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'auto_str_distinct_struct_depth', 'struct D {
	value int
}

struct C {
	d D
}

struct B {
	c C
}

struct A {
	b B
}

fn main() {
	value := A{
		b: B{
			c: C{
				d: D{
					value: 42
				}
			}
		}
	}
	println(value)
}
')
	assert out == 'A{
    b: B{
        c: C{
            d: D{
                value: 42
            }
        }
    }
}'
}

fn test_recursive_interface_auto_str_uses_helper() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'recursive_interface_auto_str', "interface Entry {
	name() string
}

struct Leaf {
	value int
}

fn (Leaf) name() string {
	return 'leaf'
}

struct Branch {
	child Entry
}

fn (Branch) name() string {
	return 'branch'
}

fn main() {
	value := Entry(Branch{
		child: Entry(Leaf{
			value: 42
		})
	})
	println(value)
}
")
	assert out.contains('child: Entry(Leaf{'), out
	assert out.contains('value: 42'), out
}

fn test_auto_str_preserves_distinct_sum_beyond_inline_depth() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'auto_str_distinct_sum_depth', 'struct Leaf {
	answer int
}

struct Other {
	text string
}

type Value = Leaf | Other

struct C {
	value Value
}

struct B {
	c C
}

struct A {
	b B
}

fn main() {
	println(A{
		b: B{
			c: C{
				value: Value(Leaf{
					answer: 42
				})
			}
		}
	})
}
')
	assert out.contains('answer: 42'), out
	assert !out.contains('Value(...)'), out
}

fn test_interface_fn_field_argument_keeps_parameter_offset_zero() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_fn_field_argument_offset', 'interface Value {
	value() int
}

struct Item {
	n int
}

fn (item Item) value() int {
	return item.n
}

struct Handler {
	callback fn (Value)
}

fn print_value(value Value) {
	println(int_str(value.value()))
}

fn main() {
	handler := Handler{
		callback: print_value
	}
	handler.callback(Item{
		n: 7
	})
}
')
	assert out == '7'
}

fn test_interface_method_mut_arguments_use_pointer_storage() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_method_mut_arguments', 'interface Writer {
	write(mut counter Counter, mut bytes []u8)
}

struct IncrementWriter {}

struct Counter {
mut:
	value int
}

fn (_ IncrementWriter) write(mut counter Counter, mut bytes []u8) {
	counter.value++
	bytes << u8(counter.value)
}

fn apply(writer Writer, mut counter Counter, mut bytes []u8) {
	writer.write(mut counter, mut bytes)
}

fn main() {
	mut counter := Counter{
		value: 6
	}
	mut bytes := []u8{}
	apply(IncrementWriter{}, mut counter, mut bytes)
	println(int_str(counter.value))
	println(int_str(bytes[0]))
}
')
	assert out == '7\n7'
}

fn test_folded_string_constant_ifs_keep_branch_scopes() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'folded_string_constant_if_branch_scopes', "fn main() {\n\tif 'left' == 'left' {\n\t\tx := 20\n\t\tprintln(int_str(x))\n\t}\n\tif 'right' == 'right' {\n\t\tx := 22\n\t\tprintln(int_str(x))\n\t}\n}\n")
	assert out == '20\n22'
}

fn test_import_aliased_variadic_call_uses_exact_module() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'import_aliased_variadic_call', {
		'v.mod':         "Module { name: 'import_aliased_variadic_call' }\n"
		'a/http/http.v': 'module http\n\npub fn total(values []int) int {\n\treturn values.len\n}\n'
		'b/http/http.v': 'module http\n\npub fn total(values ...int) int {\n\treturn values.len\n}\n'
		'main.v':        'module main\n\nimport a.http as other_http\nimport b.http as http\n\nfn main() {\n\t_ := other_http.total([1, 2])\n\tprintln(int_str(http.total(3, 4, 5)))\n}\n'
	}, 'main.v')
	assert out == '3'
}

fn test_imported_interface_const_method_uses_interface_dispatch() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_interface_const_method', {
		'v.mod':                     "Module { name: 'imported_interface_const_method' }\n"
		'errorsource/errorsource.v': "module errorsource\n\npub const sentinel = error_with_code('sentinel', 37)\n"
		'main.v':                    'module main\n\nimport errorsource\n\nfn main() {\n\tprintln(int_str(errorsource.sentinel.code()))\n}\n'
	}, 'main.v')
	assert out == '37'
}

fn test_array_field_stringification_prefers_local_type_over_imported_homonym() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'array_field_string_local_type_collision', {
		'v.mod':       "Module { name: 'array_field_string_local_type_collision' }\n"
		'other/mod.v': 'module other\n\npub struct Event {\npub:\n\tname string\n}\n'
		'main.v':      "module main\n\nimport other\n\nstruct Event {\n\tkind int\n}\n\nstruct App {\n\tevents []Event\n}\n\nfn main() {\n\t_ := other.Event{\n\t\tname: 'imported'\n\t}\n\tapp := App{\n\t\tevents: [Event{\n\t\t\tkind: 7\n\t\t}]\n\t}\n\tprintln(app.events)\n}\n"
	}, 'main.v')
	assert out == '[Event{\n    kind: 7\n}]'
}

fn test_array_stringification_prefers_local_struct_over_imported_alias() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'array_str_local_struct_imported_alias_collision', {
		'v.mod':         "Module { name: 'array_str_local_struct_imported_alias_collision' }\n"
		'other/event.v': 'module other\n\npub struct ForeignEvent {\npub:\n\ttouches int\n}\n\npub type Event = ForeignEvent\n'
		'main.v':        "module main\n\nimport other\n\nstruct Event {\n\tkind int\n\targ string\n}\n\nfn main() {\n\t_ := other.Event(other.ForeignEvent{\n\t\ttouches: 3\n\t})\n\tevents := [Event{\n\t\tkind: 7\n\t\targ: 'ok'\n\t}]\n\tprintln(events)\n}\n"
	}, 'main.v')
	assert out == "[Event{\n    kind: 7\n    arg: 'ok'\n}]"
}

fn test_imported_generic_alias_expands_in_declaration_module() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_generic_alias_decl_module', {
		'v.mod':     "Module { name: 'imported_generic_alias_decl_module' }\n"
		'a/types.v': 'module a

pub struct Inner[T] {
pub:
	value T
}

pub type Box[T] = Inner[T]

pub fn make() Box[int] {
	return Inner[int]{
		value: 7
	}
}
'
		'main.v':    'module main

import a

struct Inner[T] {
	wrong T
}

fn read(box a.Box[int]) int {
	return box.value
}

fn main() {
	println(int_str(read(a.make())))
}
'
	}, 'main.v')
	assert out == '7'
}

fn test_imported_struct_zero_fields_use_declaration_module_aliases() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_struct_field_alias_scope', {
		'v.mod':       "Module { name: 'imported_struct_field_alias_scope' }\n"
		'other/mod.v': 'module other

pub type Type = u32

pub struct Holder {
pub:
	typ Type
}
'
		'main.v':      'module main

import other

struct Type {
	sym &int @[required]
}

fn main() {
	holder := other.Holder{}
	println(holder.typ)
}
'
	}, 'main.v')
	assert out == '0'
}

fn test_for_in_smartcast_interface_field_keeps_interface_element_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'for_in_smartcast_interface_field', 'interface Widget {\n\tid int\n}\n\nstruct Stack {\n\tid       int\n\tchildren []Widget\n}\n\nstruct Leaf {\n\tid int\n}\n\nfn total(w Widget) int {\n\tif w is Stack {\n\t\tmut value := w.id\n\t\tfor child in w.children {\n\t\t\tvalue += total(child)\n\t\t}\n\t\treturn value\n\t}\n\treturn w.id\n}\n\nfn main() {\n\tw := Widget(Stack{\n\t\tid: 1\n\t\tchildren: [Widget(Leaf{\n\t\t\tid: 2\n\t\t})]\n\t})\n\tprintln(int_str(total(w)))\n}\n')
	assert out == '3'
}

fn test_interface_smartcast_rebuilds_richer_interface_fields() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_smartcast_richer_fields', 'interface Base {\n\tx int\n}\n\ninterface Extended {\n\tBase\n\ty    int\n\tnext ?Extended\n}\n\nstruct Item {\n\tx    int\n\ty    int\n\tnext ?Extended\n}\n\nfn value(base Base) int {\n\tif base is Extended {\n\t\treturn base.x + base.y\n\t}\n\treturn 0\n}\n\nfn main() {\n\tprintln(int_str(value(Base(Item{\n\t\tx: 2\n\t\ty: 3\n\t}))))\n}\n')
	assert out == '5'
}

fn test_interface_smartcast_nil_pointer_zero_fills_richer_fields() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_smartcast_nil_pointer_richer_fields', 'interface Base {
	value() int
}

interface Rich {
	Base
	x int
}

struct Item {
	x int
}

fn (item Item) value() int {
	return item.x
}

fn read(base Base) int {
	if base is Rich {
		return base.x
	}
	return -1
}

fn main() {
	item := unsafe { &Item(nil) }
	println(int_str(read(Base(item))))
}
')
	assert out == '0'
}

fn test_mut_interface_smartcast_field_assignment_uses_storage_interface() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'mut_interface_smartcast_field_assignment', 'interface Base {
mut:
	x int
}

interface Rich {
	Base
mut:
	y int
}

struct Item {
mut:
	x int
	y int
}

fn update(mut base Base) {
	if mut base is Rich {
		base.y = 9
	}
}

fn main() {
	mut base := Base(Item{
		x: 1
		y: 2
	})
	update(mut base)
	if mut base is Rich {
		println(int_str(base.y))
	}
}
')
	assert out == '9'
}

fn test_mut_interface_argument_shares_concrete_source() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'mut_interface_argument_concrete_source', 'interface Counter {
mut:
	inc()
}

struct State {
mut:
	n int
}

fn (mut s State) inc() {
	s.n++
}

fn bump(mut counter Counter) {
	counter.inc()
}

fn main() {
	mut state := State{}
	bump(mut state)
	println(int_str(state.n))
}
')
	assert out == '1'
}

fn test_nested_generic_main_type_does_not_emit_imported_homonym_specialization() {
	v3_bin := build_v3_review_transform()
	generated := gen_c_from_project(v3_bin, 'nested_generic_main_type_collision', {
		'v.mod':         "Module { name: 'nested_generic_main_type_collision' }\n"
		'codec/codec.v': 'module codec\n\npub struct Decoder {}\n\npub fn decode[T]() T {\n\tmut result := T{}\n\tdecoder := Decoder{}\n\tdecoder.decode_value(mut result)\n\treturn result\n}\n\nfn (decoder Decoder) decode_value[T](mut value T) {\n\t_ = decoder\n\t_ = value\n}\n'
		'other/other.v': 'module other\n\npub struct Item {\npub:\n\tname string\n}\n'
		'main.v':        'module main\n\nimport codec\nimport other\n\nstruct Item {\n\tvalue int\n}\n\nfn main() {\n\t_ := other.Item{}\n\titem := codec.decode[Item]()\n\tprintln(int_str(item.value))\n}\n'
	}, 'main.v')
	assert generated.contains('codec__Decoder_Item__decode_value'), generated
	assert !generated.contains('codec__Decoder_other__Item__decode_value'), generated
}

fn test_same_generic_specialization_name_in_different_modules_keeps_both_bodies() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'generic_specialization_module_collision', {
		'v.mod':  "Module { name: 'generic_specialization_module_collision' }\n"
		'a/a.v':  'module a\n\npub fn id[T](value T) T {\n\treturn value\n}\n'
		'b/b.v':  'module b\n\npub fn id[T](value T) T {\n\treturn value + 10\n}\n'
		'main.v': 'module main\n\nimport a\nimport b\n\nfn main() {\n\tprintln(int_str(a.id[int](1)))\n\tprintln(int_str(b.id[int](2)))\n}\n'
	}, 'main.v')
	assert out == '1\n12'
}

fn test_imported_module_generic_function_value_prefers_local_declaration() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'generic_function_value_module_collision', {
		'v.mod':  "Module { name: 'generic_function_value_module_collision' }\n"
		'a/a.v':  'module a\n\nfn pick[T](value T) T {\n\treturn value + 1\n}\n\npub fn make_picker() fn (int) int {\n\treturn pick[int]\n}\n'
		'main.v': 'module main\n\nimport a\n\nfn pick[T](value T) T {\n\treturn value + 100\n}\n\nfn main() {\n\timported := a.make_picker()\n\tlocal := pick[int]\n\tprintln(int_str(imported(1)))\n\tprintln(int_str(local(1)))\n}\n'
	}, 'main.v')
	assert out == '2\n101'
}

fn test_building_v_function_values_keep_plain_and_module_helpers() {
	v3_bin := build_v3_review_transform()
	out := run_good_project_with_flags(v3_bin, 'building_v_function_value_reachability', '-building-v', {
		'v.mod':           "Module { name: 'building_v_function_value_reachability' }\n"
		'worker/worker.v': 'module worker\n\nfn local_helper() int {\n\treturn 19\n}\n\nfn apply(callback fn () int) int {\n\treturn callback()\n}\n\npub fn local_value() int {\n\treturn apply(local_helper)\n}\n\npub fn selected_helper() int {\n\treturn 23\n}\n'
		'main.v':          'module main\n\nimport worker\n\nfn apply(callback fn () int) int {\n\treturn callback()\n}\n\nfn main() {\n\tprintln(worker.local_value() + apply(worker.selected_helper))\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_imported_selector_generic_function_value_is_specialized() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'selector_generic_function_value', {
		'v.mod':           "Module { name: 'selector_generic_function_value' }\n"
		'worker/worker.v': 'module worker\n\npub fn identity[T](value T) T {\n\treturn value\n}\n'
		'main.v':          'module main\n\nimport worker\n\nfn identity[T](value T) T {\n\treturn value + 100\n}\n\nfn main() {\n\tcallback := worker.identity[int]\n\tlocal := identity[int]\n\tprintln(int_str(callback(42)))\n\tprintln(int_str(local(1)))\n}\n'
	}, 'main.v')
	assert out == '42\n101'
}

fn test_selectively_imported_generic_function_value_is_specialized() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'selective_import_generic_function_value', {
		'v.mod':     "Module { name: 'selective_import_generic_function_value' }\n"
		'lib/lib.v': 'module lib\n\npub fn id[T](value T) T {\n\treturn value + 1\n}\n'
		'main.v':    'module main\n\nimport lib { id }\n\nfn main() {\n\tcallback := id[int]\n\tprintln(int_str(callback(41)))\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_generic_struct_default_for_pointer_type_uses_heap_storage() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_pointer_struct_default', 'struct Item {
	value int = 7
}

fn make_default[T]() T {
	return T{}
}

fn main() {
	item := make_default[&Item]()
	println(item.value)
}
')
	assert out == '7'
}

fn test_optional_if_guard_prefers_local_type_over_imported_homonym() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'optional_if_guard_local_type_collision', {
		'v.mod':         "Module { name: 'optional_if_guard_local_type_collision' }\n"
		'other/other.v': 'module other\n\npub struct Server {\npub:\n\tname string\n}\n'
		'main.v':        "module main\n\nimport other\n\nstruct Server {\n\tid int\n}\n\nfn find_server() ?Server {\n\treturn Server{\n\t\tid: 9\n\t}\n}\n\nfn main() {\n\t_ := other.Server{\n\t\tname: 'imported'\n\t}\n\tif server := find_server() {\n\t\tprintln(int_str(server.id))\n\t}\n}\n"
	}, 'main.v')
	assert out == '9'
}

fn test_fixed_array_alias_is_not_requalified_in_importing_module() {
	v3_bin := build_v3_review_transform()
	generated := gen_c_from_source(v3_bin, 'fixed_array_alias_import_context', 'import gg\nimport sokol.gfx\n\nfn main() {\n\t_ := gg.Color{}\n\t_ := gfx.ImageData{}\n}\n')
	assert generated.contains('Array_fixed_struct_sg_range_16'), generated
	assert !generated.contains('Array_fixed_gg__Range_16'), generated
}

fn test_nested_string_array_literal_keeps_alias_element_type() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'nested_string_array_alias', {
		'v.mod':           "Module { name: 'nested_string_array_alias' }\n"
		'syntax/syntax.v': "module syntax\n\ntype MapArrayStrings = map[string][][]string\n\npub struct Highlighter {\npub mut:\n\tmultiline map[string]MapArrayStrings\n}\n\npub fn (mut h Highlighter) load() {\n\th.multiline['v'] = {\n\t\t'comment': [['/*', '*/']]\n\t}\n}\n"
		'main.v':          "module main\n\nimport syntax\n\nfn main() {\n\tmut h := syntax.Highlighter{}\n\th.load()\n\tprintln(h.multiline['v']['comment'][0][0])\n\tprintln(h.multiline['v']['comment'][0][1])\n}\n"
	}, 'main.v')
	assert out == '/*\n*/'
}

fn test_generic_array_retyping_is_scoped_to_the_lowered_literal() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_array_retype_temp_scope', "enum ChildSize {\n\tcompact\n}\n\nfn delimiters() [][]string {\n\treturn [['/*', '*/']]\n}\n\nfn child_sizes(len int) []ChildSize {\n\treturn [ChildSize.compact].repeat(len)\n}\n\nfn main() {\n\tvalues := delimiters()\n\tsizes := child_sizes(2)\n\tprintln(values[0][0])\n\tprintln(values[0][1])\n\tprintln(int_str(sizes.len))\n}\n")
	assert out == '/*\n*/\n2'
}

fn test_generic_specializations_keep_full_aliased_import_paths() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'generic_specialization_aliased_import_paths', {
		'v.mod':          "Module { name: 'generic_specialization_aliased_import_paths' }\n"
		'a/tast/value.v': 'module tast\n\npub struct Value {\npub:\n\tn int\n}\n'
		'b/tast/value.v': 'module tast\n\npub struct Value {\npub:\n\ttext string\n}\n'
		'main.v':         "module main\n\nimport a.tast as left\nimport b.tast as tast\n\nfn keep[T](value T) T {\n\treturn value\n}\n\nfn main() {\n\tleft_value := keep(left.Value{\n\t\tn: 41\n\t})\n\tright_value := keep(tast.Value{\n\t\ttext: 'ok'\n\t})\n\tprintln(int_str(left_value.n))\n\tprintln(right_value.text)\n}\n"
	}, 'main.v')
	assert out == '41\nok'
}

fn test_nested_inferred_fixed_array_literal_parses() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_inferred_fixed_array_literal', 'fn main() {\n\tvalues := [..][..]int[[1, 2], [3, 4]]\n\tprintln(int_str(values[0][0] + values[0][1] + values[1][0] + values[1][1]))\n}\n')
	assert out == '10'
	run_bad(v3_bin, 'ragged_nested_inferred_fixed_array_literal', 'fn main() {\n\t_ := [..][..]int[[1], [2, 3]]\n}\n', 'inferred fixed-array literal rows must have the same size')
}

fn test_shared_field_without_sync_import_compiles_and_locks() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'shared_field_without_sync_import', 'struct S {\nmut:\n\ta shared int\n}\n\nfn main() {\n\tmut s := S{}\n\tlock s.a {\n\t\ts.a = 7\n\t\tprintln(int_str(s.a))\n\t}\n}\n')
	assert out == '7'
}

fn test_direct_shared_field_lock_allows_mut_receiver_and_branch_tail_append() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'direct_shared_lock_mut_receiver_and_append', 'struct State {
mut:
	values shared []string
	count int
}

fn (mut state State) touch() {
	state.count++
}

fn main() {
	mut state := State{}
	lock state.values {
		state.touch()
		if true {
			state.values << "ok"
		}
	}
	mut len := 0
	rlock state.values {
		len = state.values.len
	}
	println(int_str(state.count))
	println(int_str(len))
}
')
	assert out == '1\n1'
}

fn test_unsafe_block_tail_keeps_lexical_smartcast() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'unsafe_block_tail_lexical_smartcast', 'struct LambdaNode {
mut:
	value int
}

struct OtherNode {}

type Expr = LambdaNode | OtherNode

fn main() {
	expr := Expr(LambdaNode{
		value: 6
	})
	if expr is LambdaNode {
		mut node := unsafe { expr }
		node.value++
		println(int_str(node.value))
	}
}
')
	assert out == '7'
}

fn test_nested_shared_field_lock_allows_member_access() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_shared_field_lock', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nfn main() {\n\tmut coordinator := Coordinator{}\n\tlock coordinator.state {\n\t\tcoordinator.state.value = 7\n\t}\n\tmut value := 0\n\trlock coordinator.state {\n\t\tvalue = coordinator.state.value\n\t}\n\tprintln(int_str(value))\n}\n')
	assert out == '7'
}

fn test_nested_shared_field_lock_rejects_base_reassignment() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_base_reassignment', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut p := &first\n\tlock p.state {\n\t\tp = &second\n\t\tp.state.value = 7\n\t}\n}\n', 'cannot reassign `p` while it is used to locate locked shared value `p.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_index_reassignment', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\titems := [&first, &second]\n\tmut i := 0\n\tlock items[i].state {\n\t\ti = 1\n\t\titems[i].state.value = 7\n\t}\n}\n', 'cannot reassign `i` while it is used to locate locked shared value `items[i].state`')
}

fn test_nested_shared_field_lock_rejects_aliased_index_mutation() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_aliased_index_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut items := [&first, &second]\n\ti := 0\n\tlock items[i].state {\n\t\titems[0] = &second\n\t\titems[i].state.value = 7\n\t}\n}\n', 'may alias locked shared value `items[i].state`')
	run_bad(v3_bin, 'nested_shared_field_lock_equivalent_literal_index_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut items := [&first]\n\tlock items[0].state {\n\t\titems[0x0] = &replacement\n\t\titems[0].state.value++\n\t}\n}\n', 'may alias locked shared value `items[0].state`')
	out := run_good(v3_bin, 'nested_shared_field_lock_distinct_literal_index', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut items := [&first, &second]\n\tlock items[0].state {\n\t\titems[1] = &first\n\t\titems[0].state.value = 7\n\t\tprintln(int_str(items[0].state.value))\n\t}\n}\n')
	assert out == '7'
}

fn test_nested_shared_field_lock_rejects_promoted_field_alias_mutation() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_promoted_field_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator = unsafe { nil }\n}\n\nstruct Wrapper {\nmut:\n\tHolder\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut wrapper := Wrapper{}\n\twrapper.Holder.current = &first\n\tlock wrapper.current.state {\n\t\twrapper.Holder.current = &replacement\n\t\twrapper.current.state.value++\n\t}\n}\n', 'used to locate locked shared value `wrapper.current.state`')
}

fn test_nested_shared_field_lock_rejects_pointer_alias_mutation() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_cross_assignment_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\talias, unrelated = unrelated, alias\n\tlock holder.current.state {\n\t\tunrelated.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_address_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := &holder\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_call_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn passthrough(holder &Holder) &Holder {\n\treturn holder\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := passthrough(holder)\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_selector_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nstruct Box {\n\tholder &Holder\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tbox := Box{\n\t\tholder: holder\n\t}\n\tmut alias := box.holder\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_index_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tholders := [holder]\n\tmut alias := holders[0]\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_selector_base_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nstruct Box {\nmut:\n\tholder &Holder\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut box := Box{\n\t\tholder: holder\n\t}\n\tlock holder.current.state {\n\t\tbox.holder.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_index_base_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut holders := [holder]\n\tlock holder.current.state {\n\t\tholders[0].current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_parameter_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn mutate(mut holder &Holder, mut other &Holder) {\n\tmut replacement := Coordinator{}\n\tmut alias := other\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value = 7\n\t}\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut other := holder\n\tmutate(mut holder, mut other)\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_if_expr_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn true\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := if choose() { holder } else { unrelated }\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_match_expr_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn true\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := match choose() {\n\t\ttrue { holder }\n\t\telse { unrelated }\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_defer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tdefer {\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_mut_call_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn replace(mut holder &Holder, replacement &Coordinator) {\n\tholder.current = replacement\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tlock holder.current.state {\n\t\treplace(mut holder, &replacement)\n\t\tholder.current.state.value++\n\t}\n}\n', 'used to locate locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_mut_receiver_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn (mut holder Holder) replace(replacement &Coordinator) {\n\tholder.current = replacement\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tlock holder.current.state {\n\t\tholder.replace(&replacement)\n\t\tholder.current.state.value++\n\t}\n}\n', 'used to locate locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_mut_pointer_call_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn replace(mut current &Holder, replacement &Holder) {\n\tcurrent = replacement\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := &Holder{\n\t\tcurrent: &first\n\t}\n\treplace(mut holder, alias)\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_or_fallback_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn maybe() ?int {\n\treturn 1\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tmaybe() or {\n\t\talias = unrelated\n\t\t0\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_select_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tch := chan int{cap: 1}\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tselect {\n\t\tch <- 1 {}\n\t\telse {\n\t\t\talias = unrelated\n\t\t}\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_good(v3_bin, 'nested_shared_field_lock_skipped_short_circuit_mut_call', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn maybe_replace(mut current &Holder, replacement &Holder) bool {\n\tcurrent = replacement\n\treturn true\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := &Holder{\n\t\tcurrent: &first\n\t}\n\tif false && maybe_replace(mut alias, holder) {}\n\tskipped := true || maybe_replace(mut alias, holder)\n\tassert skipped\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n')
	run_bad(v3_bin, 'nested_shared_field_lock_conditional_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn false\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tif choose() {\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_match_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn false\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tmatch choose() {\n\t\ttrue { alias = unrelated }\n\t\telse {}\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_loop_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn false\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tfor choose() {\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_for_in_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tvalues := []int{}\n\tfor _ in values {\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	out := run_good(v3_bin, 'nested_shared_field_lock_rebound_pointer_alias', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\talias = unrelated\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 7\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert out == '7'
	address_out := run_good(v3_bin, 'nested_shared_field_lock_distinct_address_alias', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := &unrelated\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 11\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert address_out == '11'
	call_rebound_out := run_good(v3_bin, 'nested_shared_field_lock_rebound_call_alias', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn passthrough(holder &Holder) &Holder {\n\treturn holder\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := passthrough(holder)\n\talias = unrelated\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 12\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert call_rebound_out == '12'
	param_rebound_out := run_good(v3_bin, 'nested_shared_field_lock_rebound_parameter_alias', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn mutate(mut holder &Holder, mut other &Holder) {\n\tmut replacement := Coordinator{}\n\tmut unrelated := &Holder{\n\t\tcurrent: holder.current\n\t}\n\tmut alias := other\n\talias = unrelated\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value = 13\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut other := holder\n\tmutate(mut holder, mut other)\n}\n')
	assert param_rebound_out == '13'
	defer_out := run_good(v3_bin, 'nested_shared_field_lock_defer_rebinds_after_lock', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := unrelated\n\tdefer {\n\t\talias = holder\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value = 14\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert defer_out == '14'
	conditional_out := run_good(v3_bin, 'nested_shared_field_lock_rebound_pointer_alias_all_paths', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn false\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tif choose() {\n\t\talias = unrelated\n\t} else {\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 8\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert conditional_out == '8'
	match_out := run_good(v3_bin, 'nested_shared_field_lock_match_rebound_pointer_alias_all_paths', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn false\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tmatch choose() {\n\t\ttrue { alias = unrelated }\n\t\telse { alias = unrelated }\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 9\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert match_out == '9'
	loop_out := run_good(v3_bin, 'nested_shared_field_lock_loop_rebound_pointer_alias_all_exits', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut second := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tfor {\n\t\talias = unrelated\n\t\tbreak\n\t}\n\tlock holder.current.state {\n\t\talias.current = &second\n\t\tholder.current.state.value = 10\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert loop_out == '10'
}

fn test_nested_shared_field_lock_preserves_continue_pointer_aliases() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_continue_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn choose() bool {\n\treturn true\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := unrelated\n\tfor i := 0; i < 1; i++ {\n\t\tif choose() {\n\t\t\talias = holder\n\t\t\tcontinue\n\t\t}\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
}

fn test_nested_shared_field_lock_rechecks_pointer_aliases_in_loop_post() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_loop_post_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut candidate := unrelated\n\tmut alias := unrelated\n\tmut i := 0\n\tfor ; i < 1; i, alias = i + 1, candidate {\n\t\tcandidate = holder\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
}

fn test_nested_shared_field_lock_tracks_select_receive_pointer_alias() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_select_receive_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tholders := chan &Holder{cap: 1}\n\tholders <- holder\n\tmut alias := unrelated\n\tselect {\n\t\talias = <-holders {}\n\t\telse {}\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
}

fn test_nested_shared_field_lock_treats_pointer_iteration_binding_as_alias() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_pointer_iteration_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut holders := [holder]\n\tfor mut alias in holders {\n\t\tlock holder.current.state {\n\t\t\talias.current = &replacement\n\t\t\tholder.current.state.value++\n\t\t}\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
}

fn test_nested_shared_field_lock_treats_lambda_pointer_parameters_as_aliases() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_lambda_parameter_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn apply(callback fn (&Holder, &Holder) int, locked &Holder, other &Holder) {\n\t_ := callback(locked, other)\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tapply(|mut locked, mut other| if true {\n\t\tmut replacement := Coordinator{}\n\t\tlock locked.current.state {\n\t\t\tother.current = &replacement\n\t\t\tlocked.current.state.value++\n\t\t}\n\t\t0\n\t} else {\n\t\t0\n\t}, holder, alias)\n}\n', 'may alias locked shared value `locked.current.state`')
}

fn test_nested_shared_field_lock_preserves_explicit_capture_pointer_aliases() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_explicit_capture_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tcallback := fn [mut holder, mut alias, replacement] () {\n\t\tlock holder.current.state {\n\t\t\talias.current = &replacement\n\t\t\tholder.current.state.value++\n\t\t}\n\t}\n\tcallback()\n}\n', 'may alias locked shared value `holder.current.state`')
	out := run_good(v3_bin, 'nested_shared_field_lock_distinct_explicit_capture_pointer', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tcallback := fn [mut holder, mut unrelated, replacement] () {\n\t\tlock holder.current.state {\n\t\t\tunrelated.current = &replacement\n\t\t\tholder.current.state.value = 31\n\t\t\tprintln(int_str(holder.current.state.value))\n\t\t}\n\t}\n\tcallback()\n}\n')
	assert out == '31'
}

fn test_nested_shared_field_lock_treats_if_guard_pointer_binding_as_alias() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_if_guard_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn maybe_holder(holder &Holder) ?&Holder {\n\treturn holder\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tif mut alias := maybe_holder(holder) {\n\t\tlock holder.current.state {\n\t\t\talias.current = &replacement\n\t\t\tholder.current.state.value++\n\t\t}\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
}

fn test_nested_shared_field_lock_merges_pointer_aliases_at_goto_target() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_goto_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := holder\n\tunsafe {\n\t\tgoto locked\n\t}\n\talias = unrelated\n\tlocked:\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_backward_goto_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := unrelated\n\tlocked:\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n\talias = holder\n\tunsafe {\n\t\tgoto locked\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
}

fn test_nested_shared_field_lock_tracks_indirect_pointer_writes() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_indirect_pointer_write', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut slot := &holder\n\tlock holder.current.state {\n\t\tunsafe {\n\t\t\t*slot = unrelated\n\t\t}\n\t\tholder.current.state.value++\n\t}\n\t_ = replacement\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_prior_indirect_pointer_write', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := unrelated\n\tmut slot := &alias\n\tunsafe {\n\t\t*slot = holder\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	out := run_good(v3_bin, 'nested_shared_field_lock_distinct_indirect_pointer_write', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut slot := &unrelated\n\tlock holder.current.state {\n\t\tunsafe {\n\t\t\t*slot = &Holder{\n\t\t\t\tcurrent: &replacement\n\t\t\t}\n\t\t}\n\t\tholder.current.state.value = 23\n\t\tprintln(int_str(holder.current.state.value))\n\t}\n}\n')
	assert out == '23'
}

fn test_nested_shared_field_lock_preserves_labelled_loop_exit_pointer_aliases() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_labelled_break_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := unrelated\n\touter: for {\n\t\tfor {\n\t\t\talias = holder\n\t\t\tbreak outer\n\t\t}\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
	run_bad(v3_bin, 'nested_shared_field_lock_labelled_continue_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\nfn main() {\n\tmut first := Coordinator{}\n\tmut replacement := Coordinator{}\n\tmut holder := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut unrelated := &Holder{\n\t\tcurrent: &first\n\t}\n\tmut alias := unrelated\n\touter: for i := 0; i < 1; i++ {\n\t\tfor {\n\t\t\talias = holder\n\t\t\tcontinue outer\n\t\t}\n\t\talias = unrelated\n\t}\n\tlock holder.current.state {\n\t\talias.current = &replacement\n\t\tholder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `holder.current.state`')
}

fn test_nested_shared_field_lock_rejects_global_parameter_alias_mutation() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_global_parameter_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\n__global (\n\tglobal_coordinator = Coordinator{}\n\tglobal_holder = &Holder{\n\t\tcurrent: &global_coordinator\n\t}\n)\n\nfn mutate(mut alias &Holder) {\n\tmut replacement := Coordinator{}\n\tlock global_holder.current.state {\n\t\talias.current = &replacement\n\t\tglobal_holder.current.state.value++\n\t}\n}\n\nfn main() {\n\tmutate(mut global_holder)\n}\n', 'may alias locked shared value `global_holder.current.state`')
	out := run_good(v3_bin, 'nested_shared_field_lock_rebound_global_parameter', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\n__global (\n\tglobal_coordinator = Coordinator{}\n\tglobal_holder = &Holder{\n\t\tcurrent: &global_coordinator\n\t}\n)\n\nfn mutate(mut alias &Holder) {\n\tmut replacement := Coordinator{}\n\tmut local_holder := &Holder{\n\t\tcurrent: &replacement\n\t}\n\talias = local_holder\n\tlock global_holder.current.state {\n\t\talias.current = &replacement\n\t\tglobal_holder.current.state.value = 19\n\t\tprintln(int_str(global_holder.current.state.value))\n\t}\n}\n\nfn main() {\n\tmut incoming := global_holder\n\tmutate(mut incoming)\n}\n')
	assert out == '19'
}

fn test_nested_shared_field_lock_rejects_global_pointer_alias_mutation() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_global_pointer_alias_mutation', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Holder {\nmut:\n\tcurrent &Coordinator\n}\n\n__global (\n\tglobal_coordinator = Coordinator{}\n\tglobal_holder = &Holder{\n\t\tcurrent: &global_coordinator\n\t}\n\tglobal_alias = &Holder{\n\t\tcurrent: &global_coordinator\n\t}\n)\n\nfn connect_globals() {\n\tglobal_alias = global_holder\n}\n\nfn main() {\n\tconnect_globals()\n\tmut replacement := Coordinator{}\n\tlock global_holder.current.state {\n\t\tglobal_alias.current = &replacement\n\t\tglobal_holder.current.state.value++\n\t}\n}\n', 'may alias locked shared value `global_holder.current.state`')
}

fn test_nested_shared_field_lock_rejects_call_base() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_call_base', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nfn pick() &Coordinator {\n\treturn &Coordinator{}\n}\n\nfn main() {\n\tlock pick().state {\n\t\tpick().state.value = 7\n\t}\n}\n', 'selector bases and indices must be stable expressions')
}

fn test_nested_shared_field_lock_rejects_overloaded_index() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'nested_shared_field_lock_overloaded_index', 'struct State {\nmut:\n\tvalue int\n}\n\nstruct Coordinator {\n\tstate shared State\n}\n\nstruct Registry {\n\tentries []&Coordinator\n}\n\nfn (registry Registry) [] (key int) &Coordinator {\n\treturn registry.entries[key]\n}\n\nfn main() {\n\tmut coordinator := Coordinator{}\n\tmut registry := Registry{\n\t\tentries: [&coordinator]\n\t}\n\tkey := 0\n\tlock registry[key].state {\n\t\tregistry[key].state.value = 7\n\t}\n}\n', 'selector bases and indices must be stable expressions')
}

fn test_reassigned_nil_pointer_can_be_dereferenced() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'reassigned_nil_pointer', 'fn main() {\n\tmut pointer := &int(unsafe { nil })\n\tmut value := 7\n\tpointer = &value\n\tprintln(int_str(*pointer))\n}\n')
	assert out == '7'
}

fn test_imported_shared_field_without_sync_import_compiles_and_locks() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_shared_field_without_sync_import', {
		'v.mod':     'Module { name: "imported_shared_field_without_sync_import" }\n'
		'main.v':    'module main\n\nimport bag\n\nfn main() {\n\tprintln(int_str(bag.value()))\n}\n'
		'bag/bag.v': 'module bag\n\nstruct S {\nmut:\n\ta shared int\n}\n\npub fn value() int {\n\tmut s := S{}\n\tmut out := 0\n\tlock s.a {\n\t\ts.a = 9\n\t\tout = s.a\n\t}\n\treturn out\n}\n'
	}, 'main.v')
	assert out == '9'
}

fn test_reject_dynamic_arrays_for_fixed_array_expectations() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'bad_fixed_array_literal_len', 'fn take3(a [3]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take3([1, 2])\n}\n', 'cannot use')
	run_bad(v3_bin, 'bad_dynamic_array_for_fixed_array', 'fn take3(a [3]int) int {\n\treturn a[0]\n}\nfn main() {\n\txs := [1, 2, 3]\n\t_ := take3(xs)\n}\n', 'cannot use')
	out := run_good(v3_bin, 'good_exact_fixed_array_literal', 'fn take3(a [3]int) int {\n\treturn a[0] + a[1] + a[2]\n}\nfn main() {\n\tprintln(int_str(take3([1, 2, 3])))\n}\n')
	assert out == '6'
	indexed := run_good(v3_bin, 'good_fixed_array_init_index', 'fn main() {\n\ta := [4]int{init: index * index}\n\tprintln(int_str(a[0]) + "," + int_str(a[1]) + "," + int_str(a[2]) + "," + int_str(a[3]))\n}\n')
	assert indexed == '0,1,4,9'
	const_indexed := run_good(v3_bin, 'good_fixed_array_const_init_index', 'const n = 4\n\nfn main() {\n\ta := [n]int{init: index * index}\n\tprintln(int_str(a[0]) + "," + int_str(a[1]) + "," + int_str(a[2]) + "," + int_str(a[3]))\n}\n')
	assert const_indexed == '0,1,4,9'
	arg_indexed := run_good(v3_bin, 'good_fixed_array_arg_init_index', 'fn take(a [4]int) int {\n\treturn a[0] + a[1] + a[2] + a[3]\n}\n\nfn main() {\n\tprintln(int_str(take([4]int{init: index * index})))\n}\n')
	assert arg_indexed == '14'
}

fn test_array_equality_uses_semantic_element_comparison() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'semantic_array_equality', "struct Child {\n\tlabel string\n}\n\nstruct Item {\n\tname string\n\tparts []string\n\tnested [][]string\n\tchildren []Child\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := [Item{\n\t\tname: 'hi'.clone()\n\t\tparts: ['ab'.clone()]\n\t\tnested: [[join('n', 'est')]]\n\t\tchildren: [Child{\n\t\t\tlabel: 'kid'.clone()\n\t\t}]\n\t}]\n\tright := [Item{\n\t\tname: join('h', 'i')\n\t\tparts: [join('a', 'b')]\n\t\tnested: [['nest'.clone()]]\n\t\tchildren: [Child{\n\t\t\tlabel: join('k', 'id')\n\t\t}]\n\t}]\n\tmaps_left := [{\n\t\t'k': 'value'.clone()\n\t}]\n\tmaps_right := [{\n\t\t'k': join('val', 'ue')\n\t}]\n\tnested_left := [[join('y', 'o')]]\n\tnested_right := [['yo'.clone()]]\n\tchild_map_left := {\n\t\t'items': [Child{\n\t\t\tlabel: 'mapkid'.clone()\n\t\t}]\n\t}\n\tchild_map_right := {\n\t\t'items': [Child{\n\t\t\tlabel: join('map', 'kid')\n\t\t}]\n\t}\n\tneedle := Item{\n\t\tname: join('h', 'i')\n\t\tparts: [join('a', 'b')]\n\t\tnested: [['nest'.clone()]]\n\t\tchildren: [Child{\n\t\t\tlabel: join('k', 'id')\n\t\t}]\n\t}\n\tprintln(left == right)\n\tprintln(left.equals(right))\n\tprintln(maps_left == maps_right)\n\tprintln(nested_left == nested_right)\n\tprintln(child_map_left == child_map_right)\n\tprintln(needle in left)\n\tprintln(int_str(left.index(needle)))\n}\n")
	assert out == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\n0'
}

fn test_explicitly_dereferenced_array_equality_is_not_double_dereferenced() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	values := [1, 2, 3]
	p := &values
	assert *p == values
	assert values == *p
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'explicit_array_deref_equality_c', source)
	assert !c_source.contains('**p'), c_source
	out := run_good(v3_bin, 'explicit_array_deref_equality', source)
	assert out == 'ok'
}

fn test_mut_array_loop_sort_receiver_is_not_double_dereferenced() {
	v3_bin := build_v3_review_transform()
	source := 'struct Item {
	n int
}

fn split() [][]Item {
	mut buckets := [][]Item{len: 2, init: []Item{}}
	for mut bucket in buckets {
		bucket.sort(a.n < b.n)
	}
	return buckets
}

fn main() {
	println(split().len)
}
'
	c_source := gen_c_from_source(v3_bin, 'mut_array_loop_sort_receiver_c', source)
	assert !c_source.contains('**bucket'), c_source
	out := run_good(v3_bin, 'mut_array_loop_sort_receiver', source)
	assert out == '2'
}

fn test_array_map_fn_value_uses_callback_return_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_map_fn_value_return_type', "fn main() {\n\ti_to_str := fn (i int) string {\n\t\treturn int_str(i)\n\t}\n\ta := [1, 2, 3].map(i_to_str)\n\tassert a == ['1', '2', '3']\n\tprintln(a[0] + a[1] + a[2])\n}\n")
	assert out == '123'
}

fn test_const_array_allows_newline_separators_with_line_comments() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'const_array_line_comments', 'const xs = [\n\t1\n\t// one\n\t2\n\t// two\n\t3\n]\n\nfn main() {\n\tprintln(int_str(xs.len))\n\tprintln(int_str(xs[1]))\n}\n')
	assert out == '3\n2'
}

fn test_const_struct_channel_default_uses_runtime_init() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'const_struct_channel_default_runtime_init', 'struct Holder {
	ch chan int
}

const holder = Holder{}

fn main() {
	holder.ch.close()
	println("ok")
}
')
	assert out == 'ok'
}

fn test_const_nested_struct_channel_default_uses_runtime_init() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'const_nested_struct_channel_default_runtime_init', 'struct Inner {
	ch chan int
}

struct Outer {
	inner Inner
}

const outer = Outer{}

fn main() {
	outer.inner.ch.close()
	println("ok")
}
')
	assert out == 'ok'
}

fn test_heap_attribute_does_not_promote_channel_with_imported_pointer_element() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'heap_attr_imported_pointer_channel', {
		'v.mod':        "Module { name: 'heap_attr_imported_pointer_channel' }\n"
		'items/item.v': 'module items\n\n@[heap]\npub struct Item {\npub:\n\tvalue int\n}\n'
		'main.v':       'module main\n\nimport items\n\nfn main() {\n\tch := chan &items.Item{cap: 1}\n\tch <- &items.Item{\n\t\tvalue: 42\n\t}\n\titem := <-ch\n\tprintln(int_str(item.value))\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_mut_pointer_capture_is_not_over_dereferenced() {
	v3_bin := build_v3_review_transform()
	// A `[mut p]` capture whose original type is already a pointer (`&S`) must stay a
	// genuine `&S` local: its rvalue uses must not be over-dereferenced, so a call that
	// expects the pointer still receives it (regression for gating the pointer-value
	// rvalue/lvalue flags on `capture_by_ref` instead of every `capture_mut`).
	out := run_good(v3_bin, 'mut_pointer_capture', 'struct S {\n\tn int\n}\n\nfn takes_ptr(p &S) int {\n\treturn p.n\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut p := &S{\n\t\tn: 5\n\t}\n\tcall(fn [mut p] () {\n\t\tprintln(int_str(takes_ptr(p)))\n\t})\n}\n')
	assert out == '5'
}

fn test_non_escaping_local_closures_are_reclaimed_in_hot_loop() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	for i in 0 .. 50_000 {
		value := i
		callback := fn [value] () int {
			return value
		}
		assert callback() == value
	}
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'local_closure_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(callback);'), c_source
	out := run_good(v3_bin, 'local_closure_hot_loop', source)
	assert out == 'ok'
}

fn test_branch_selected_local_method_closures_preserve_receiver_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn exercise(flag bool) int {
	mut first := Counter{
		value: 1
	}
	mut second := Counter{
		value: 10
	}
	callback_if := if flag { first.read } else { second.read }
	callback_match := match flag {
		true { first.read }
		else { second.read }
	}
	first.value = 2
	second.value = 20
	return callback_if() + callback_match()
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		total += exercise(i % 2 == 0)
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'branch_selected_local_method_closures_c', source)
	assert c_source.contains('closure__closure_try_destroy(callback_if);'), c_source
	assert c_source.contains('closure__closure_try_destroy(callback_match);'), c_source
	out := run_good(v3_bin, 'branch_selected_local_method_closures', source)
	assert out == '1100000'
}

fn test_discarded_returned_closures_are_reclaimed_in_hot_loop() {
	v3_bin := build_v3_review_transform()
	source := 'fn make_counter() fn () int {
	mut n := 0
	return fn [mut n] () int {
		n++
		return n
	}
}

fn identity(callback fn () int) fn () int {
	return callback
}

fn main() {
	kept := make_counter()
	_ = identity(kept)
	assert kept() == 1
	for _ in 0 .. 50_000 {
		_ = make_counter()
	}
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'discarded_returned_closure_hot_loop_c', source)
	assert !c_source.contains('closure__closure_generation_snapshot();'), c_source
	assert c_source.count('closure__closure_try_destroy(__discarded_closure_') == 1, c_source

	out := run_good(v3_bin, 'discarded_returned_closure_hot_loop', source)
	assert out == 'ok'
}

fn test_discarded_branch_fresh_closure_returns_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'fn make_if(flag bool) fn () int {
	x := 41
	return if flag {
		fn [x] () int {
			return x + 1
		}
	} else {
		fn [x] () int {
			return x + 2
		}
	}
}

fn make_match(flag bool) fn () int {
	x := 40
	return match flag {
		true {
			fn [x] () int {
				return x + 2
			}
		}
		else {
			fn [x] () int {
				return x + 3
			}
		}
	}
}

fn main() {
	for i in 0 .. 50_000 {
		_ = make_if(i % 2 == 0)
		_ = make_match(i % 2 == 0)
	}
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'discarded_branch_fresh_closure_returns_c', source)
	assert c_source.count('closure__closure_try_destroy(__discarded_closure_') == 2, c_source
	out := run_good(v3_bin, 'discarded_branch_fresh_closure_returns', source)
	assert out == 'ok'
}

fn test_discarded_returned_closure_with_retained_alias_is_not_destroyed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Holder {
mut:
	callback fn () int
}

fn factory(mut holder Holder) fn () int {
	mut n := 41
	cb := fn [mut n] () int {
		n++
		return n
	}
	holder.callback = cb
	return cb
}

fn main() {
	mut holder := Holder{}
	_ = factory(mut holder)
	println(int_str(holder.callback()))
}
'
	c_source := gen_c_from_source(v3_bin, 'discarded_returned_closure_retained_alias_c', source)
	assert !c_source.contains('closure__closure_try_destroy(__discarded_closure_'), c_source
	out := run_good(v3_bin, 'discarded_returned_closure_retained_alias', source)
	assert out == '42'
}

fn test_discarded_static_fn_return_does_not_require_closure_runtime() {
	v3_bin := build_v3_review_transform()
	source := 'fn answer() int {
	return 42
}

fn callback() fn () int {
	return answer
}

fn main() {
	_ = callback()
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'discarded_static_fn_return_c', source)
	assert !c_source.contains('closure__closure_try_destroy(__discarded_closure_'), c_source
	out := run_good(v3_bin, 'discarded_static_fn_return', source)
	assert out == 'ok'
}

fn test_non_escaping_bound_method_closures_are_reclaimed_in_hot_loop() {
	v3_bin := build_v3_review_transform()
	source := 'struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn main() {
	for i in 0 .. 50_000 {
		value := Value{
			n: i
		}
		callback := value.get
		assert callback() == i
	}
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'local_bound_method_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(callback);'), c_source
	out := run_good(v3_bin, 'local_bound_method_hot_loop', source)
	assert out == 'ok'
}

fn test_deferred_local_bound_method_closures_remain_scope_owned() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn deferred_read(value int) {
	mut counter := Counter{
		value: value
	}
	callback := counter.read
	defer {
		assert callback() == value + 1
	}
	counter.value++
}

fn main() {
	for i in 0 .. 50_000 {
		deferred_read(i)
	}
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'deferred_local_bound_method_c', source)
	body := c_fn_body(c_source, 'void deferred_read(int value) {')
	assert body.contains('closure__closure_try_destroy(callback);'), body
	out := run_good(v3_bin, 'deferred_local_bound_method', source)
	assert out == 'ok'
}

fn test_locally_aliased_bound_method_closures_remain_scope_owned_until_the_alias_escapes() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn make_callback(value int) fn () int {
	mut counter := Counter{
		value: value
	}
	callback := counter.read
	alias := callback
	counter.value++
	return alias
}

fn main() {
	escaped := make_callback(77)
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		callback := counter.read
		alias := callback
		counter.value++
		total += alias()
		assert counter.value == i + 1
	}
	assert escaped() == 78
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'locally_aliased_bound_method_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(callback);'), c_source
	out := run_good(v3_bin, 'locally_aliased_bound_method_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_fields_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn zero() int {
	return 0
}

struct Holder {
mut:
	callback fn () int
}

fn make_holder(value int) Holder {
	mut counter := Counter{
		value: value
	}
	mut holder := Holder{
		callback: zero
	}
	holder.callback = counter.read
	return holder
}

fn main() {
	escaped := make_holder(77)
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		mut holder := Holder{
			callback: zero
		}
		holder.callback = counter.read
		counter.value++
		total += holder.callback()
	}
	assert escaped.callback() == 77
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_field_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__field_closure_'), c_source
	make_body := c_fn_body(c_source, 'main__Holder main__make_holder(')
	assert !make_body.contains('__field_closure_'), make_body
	out := run_good(v3_bin, 'local_callback_field_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_initializer_fields_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

struct Holder {
	callback fn () int
}

fn make_holder(value int) Holder {
	mut counter := Counter{
		value: value
	}
	holder := Holder{
		callback: counter.read
	}
	counter.value++
	return holder
}

fn main() {
	escaped := make_holder(77)
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		holder := Holder{
			callback: counter.read
		}
		counter.value++
		total += holder.callback()
		assert counter.value == i + 1
	}
	assert escaped.callback() == 78
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_initializer_field_hot_loop_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert main_body.contains('closure__closure_try_destroy(__field_closure_'), main_body
	make_body := c_fn_body(c_source, 'main__Holder main__make_holder(')
	assert !make_body.contains('__field_closure_'), make_body
	out := run_good(v3_bin, 'local_callback_initializer_field_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_whole_aggregate_reassignments_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn zero() int {
	return 0
}

struct Holder {
	callback fn () int
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		mut holder := Holder{
			callback: zero
		}
		holder = Holder{
			callback: counter.read
		}
		counter.value++
		total += holder.callback()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_aggregate_reassign_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__field_closure_'), c_source
	out := run_good(v3_bin, 'local_callback_aggregate_reassign_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_array_initializers_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn make_callbacks(value int) []fn () int {
	mut counter := Counter{
		value: value
	}
	callbacks := [counter.read]
	counter.value++
	return callbacks
}

fn main() {
	escaped := make_callbacks(77)
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		callbacks := [counter.read]
		counter.value++
		total += callbacks[0]()
		assert counter.value == i + 1
	}
	assert escaped[0]() == 78
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_array_initializer_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__array_closure_'), c_source
	out := run_good(v3_bin, 'local_callback_array_initializer_hot_loop', source)
	assert out == '1250025000'
}

fn test_locally_extracted_callback_array_fields_remain_scope_owned() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn make_callback(value int) fn () int {
	counter := Counter{
		value: value
	}
	callbacks := [counter.read]
	callback := callbacks[0]
	return callback
}

fn main() {
	escaped := make_callback(77)
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		callbacks := [counter.read]
		callback := callbacks[0]
		counter.value++
		total += callback()
		assert counter.value == i + 1
	}
	assert escaped() == 77
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_array_field_alias_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__array_closure_'), c_source
	out := run_good(v3_bin, 'local_callback_array_field_alias_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_array_index_assignments_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn zero() int {
	return 0
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		mut callbacks := [zero]
		callbacks[0] = counter.read
		counter.value++
		total += callbacks[0]()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_array_index_assign_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__field_closure_'), c_source
	out := run_good(v3_bin, 'local_callback_array_index_assign_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_dynamic_callback_array_index_assignments_preserve_receiver_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn zero() int {
	return 0
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		mut callbacks := [zero]
		index := i % callbacks.len
		callbacks[index] = counter.read
		counter.value++
		total += counter.value
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_dynamic_callback_array_index_assign_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__field_closure_'), c_source
	assert c_source.contains('.receiver = counter'), c_source
	out := run_good(v3_bin, 'local_dynamic_callback_array_index_assign_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_array_appends_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		mut callbacks := []fn () int{}
		callbacks << counter.read
		counter.value++
		total += callbacks[0]()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_array_append_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__arr_val_'), c_source
	out := run_good(v3_bin, 'local_callback_array_append_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_bulk_callback_array_appends_preserve_receiver_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		mut callbacks := []fn () int{}
		callbacks << [counter.read]
		counter.value++
		total += callbacks[0]()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_bulk_callback_array_append_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__array_closure_'), c_source
	out := run_good(v3_bin, 'local_bulk_callback_array_append_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_fixed_array_initializers_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		callbacks := [counter.read]!
		counter.value++
		total += callbacks[0]()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_fixed_array_initializer_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__array_closure_'), c_source
	out := run_good(v3_bin, 'local_callback_fixed_array_initializer_hot_loop', source)
	assert out == '1250025000'
}

fn test_multi_variable_callback_declarations_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		unused, callback := 0, counter.read
		counter.value++
		total += unused + callback()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_multi_callback_decl_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(callback);'), c_source
	out := run_good(v3_bin, 'local_multi_callback_decl_hot_loop', source)
	assert out == '1250025000'
}

fn test_callback_array_prefix_before_spread_preserves_receiver_identity_and_is_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	other_callbacks := []fn () int{}
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		callbacks := [counter.read, ...other_callbacks]
		counter.value++
		total += callbacks[0]()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_array_spread_prefix_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__array_closure_'), c_source
	out := run_good(v3_bin, 'local_callback_array_spread_prefix_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_map_initializers_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn make_callbacks(value int) map[string]fn () int {
	mut counter := Counter{
		value: value
	}
	callbacks := {
		"read": counter.read
	}
	counter.value++
	return callbacks
}

fn main() {
	escaped := make_callbacks(77)
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		callbacks := {
			"read": counter.read
		}
		counter.value++
		total += callbacks["read"]()
		assert counter.value == i + 1
	}
	assert escaped["read"]() == 78
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_callback_map_initializer_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__map_val_'), c_source
	out := run_good(v3_bin, 'local_callback_map_initializer_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_computed_key_callback_maps_preserve_receiver_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		key := "read"
		callbacks := {
			key: counter.read
		}
		counter.value++
		total += counter.value
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_computed_key_callback_map_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__map_val_'), c_source
	assert c_source.contains('.receiver = counter'), c_source
	out := run_good(v3_bin, 'local_computed_key_callback_map_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_dynamic_callback_map_index_assignments_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn zero() int {
	return 0
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		mut callbacks := {
			"read": zero
		}
		key := "read"
		callbacks[key] = counter.read
		counter.value++
		total += callbacks["read"]()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_dynamic_callback_map_index_assign_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__map_val_'), c_source
	out := run_good(v3_bin, 'local_dynamic_callback_map_index_assign_hot_loop', source)
	assert out == '1250025000'
}

fn test_callback_map_initializer_captures_each_overwritten_entry_for_cleanup() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut first := Counter{
			value: i
		}
		mut second := Counter{
			value: i
		}
		key := "read"
		callbacks := {
			"read": first.read
			key:    second.read
		}
		first.value += 10
		second.value++
		total += callbacks["read"]()
		assert first.value == i + 10
		assert second.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'overwritten_callback_map_entries_hot_loop_c', source)
	assert c_source.count('closure__closure_try_destroy(__map_val_') == 2, c_source
	out := run_good(v3_bin, 'overwritten_callback_map_entries_hot_loop', source)
	assert out == '1250025000'
}

fn test_computed_key_map_nested_callbacks_preserve_receiver_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

struct Holder {
	callback fn () int
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		key := "read"
		callbacks := {
			key: Holder{
				callback: counter.read
			}
		}
		counter.value++
		total += callbacks["read"].callback()
		assert counter.value == i + 1
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'computed_key_nested_callback_map_hot_loop_c', source)
	assert c_source.contains('closure__closure_try_destroy(__field_closure_'), c_source
	out := run_good(v3_bin, 'computed_key_nested_callback_map_hot_loop', source)
	assert out == '1250025000'
}

fn test_reassigned_non_escaping_bound_method_closures_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		first := Value{
			n: i
		}
		second := Value{
			n: i + 1
		}
		mut callback := first.get
		callback = second.get
		total += callback()
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'reassigned_local_bound_method_hot_loop_c', source)
	assert c_source.count('closure__closure_try_destroy(callback);') >= 2, c_source
	out := run_good(v3_bin, 'reassigned_local_bound_method_hot_loop', source)
	assert out == '1250025000'
}

fn test_conditionally_self_reassigned_bound_method_closures_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		first := Value{
			n: i
		}
		second := Value{
			n: i + 1
		}
		mut callback := first.get
		callback = if i % 2 == 0 { callback } else { second.get }
		total += callback()
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'conditionally_reassigned_bound_method_c', source)
	assert c_source.count('closure__closure_try_destroy(callback);') >= 2, c_source
	out := run_good(v3_bin, 'conditionally_reassigned_bound_method', source)
	assert out == '1250000000'
}

fn test_match_self_reassigned_bound_method_closures_remain_scope_owned() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut first := Counter{
			value: i
		}
		other := Counter{
			value: i + 1
		}
		keep := i % 2 == 0
		mut callback := first.read
		callback = match keep {
			true { callback }
			else { other.read }
		}
		first.value += 10
		if keep {
			assert callback() == i + 10
		} else {
			assert callback() == i + 1
		}
		total += callback()
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'match_self_reassigned_bound_method_c', source)
	assert c_source.count('closure__closure_try_destroy(callback);') >= 2, c_source
	out := run_good(v3_bin, 'match_self_reassigned_bound_method', source)
	assert out == '1250250000'
}

fn test_immediately_invoked_bound_method_closures_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Value {
	n int
}

fn (value Value) get() int {
	return value.n
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		value := Value{
			n: i
		}
		total += (value.get)()
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_bound_method_hot_loop_c', source)
	init_body := c_fn_body(c_source, 'void _vinit() {')
	assert init_body.contains('closure__closure_init();'), init_body
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert main_body.contains('_vinit();'), main_body
	assert c_source.contains('closure__closure_try_destroy(__immediate_closure_'), c_source
	out := run_good(v3_bin, 'immediate_bound_method_hot_loop', source)
	assert out == '1249975000'
}

fn test_immediately_invoked_bound_method_keeps_escaped_receiver_alive() {
	v3_bin := build_v3_review_transform()
	source := 'struct Registry {
mut:
	callbacks []fn () int
}

struct State {
	value int
}

fn (state &State) install(registry &Registry) {
	unsafe {
		registry.callbacks << fn [state] () int {
			return state.value
		}
	}
}

fn main() {
	registry := &Registry{}
	(State{
		value: 42
	}.install)(registry)
	println(int_str(registry.callbacks[0]()))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_bound_method_escaped_receiver_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	call_pos := main_body.index('println(int__str') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert call_pos >= 0, main_body
	assert destroy_pos > call_pos, main_body
	out := run_good(v3_bin, 'immediate_bound_method_escaped_receiver', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_aliased_pointer_result_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	x := 41
	p := (fn [x] () &int {
		return unsafe { &x }
	})()
	println(int_str(unsafe { *p + 1 }))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_aliased_pointer_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	deref_pos := main_body.index('*p') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert deref_pos >= 0, main_body
	assert destroy_pos > deref_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_aliased_pointer', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_integer_encoded_capture_address_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	mut value := 41
	address := (fn [mut value] () usize {
		return usize(voidptr(&value))
	})()
	p := unsafe { &int(voidptr(address)) }
	println(int_str(unsafe { *p + 1 }))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_integer_capture_address_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	deref_pos := main_body.index('*p') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert deref_pos >= 0, main_body
	assert destroy_pos > deref_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_integer_capture_address', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_aliased_integer_capture_address_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	mut value := 41
	address := (fn [mut value] () usize {
		encoded := usize(voidptr(&value))
		return encoded
	})()
	p := unsafe { &int(voidptr(address)) }
	println(int_str(unsafe { *p + 1 }))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_aliased_integer_capture_address_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	deref_pos := main_body.index('*p') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert deref_pos >= 0, main_body
	assert destroy_pos > deref_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_aliased_integer_capture_address', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_aliased_result_error_alive() {
	v3_bin := build_v3_review_transform()
	source := 'struct CaptureError {
	ptr &int
}

fn (err CaptureError) msg() string {
	return "capture error"
}

fn main() {
	x := 41
	value := (fn [x] () !int {
		return CaptureError{
			ptr: unsafe { &x }
		}
	})() or {
		if err is CaptureError {
			println(int_str(unsafe { *err.ptr + 1 }))
		}
		return
	}
	println(int_str(value))
}
'
	out := run_good(v3_bin, 'immediate_closure_aliased_result_error', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_aliased_slice_result_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	fixed := [3]int{41, 42, 43}
	slice := (fn [fixed] () []int {
		return fixed[..]
	})()
	println(slice)
}
'
	out := run_good(v3_bin, 'immediate_closure_aliased_slice', source)
	assert out == '[41, 42, 43]'
}

fn test_immediately_invoked_closure_keeps_aliased_string_result_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	mut bytes := [2]u8{}
	bytes[0] = `O`
	bytes[1] = `K`
	text := (fn [bytes] () string {
		return unsafe { tos(&bytes[0], bytes.len) }
	})()
	println(text)
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_aliased_string_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	println_pos := main_body.index('println') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert println_pos >= 0, main_body
	assert destroy_pos > println_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_aliased_string', source)
	assert out == 'OK'
}

fn test_immediately_invoked_closure_keeps_spawned_nested_capture_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	mut value := 41
	worker := (fn [mut value] () thread int {
		return spawn fn [mut value] () int {
			return value + 1
		}()
	})()
	println(int_str(worker.wait()))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_spawned_capture_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	wait_pos := main_body.index('__v_thread_join') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert wait_pos >= 0, main_body
	assert destroy_pos > wait_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_spawned_capture', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_aliased_spawn_capture_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	mut value := 42
	worker := (fn [value] () thread int {
		p := unsafe { &value }
		return spawn fn [p] () int {
			return unsafe { *p }
		}()
	})()
	println(int_str(worker.wait()))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_aliased_spawn_capture_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	wait_pos := main_body.index('__v_thread_join') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert wait_pos >= 0, main_body
	assert destroy_pos > wait_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_aliased_spawn_capture', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_side_effect_capture_escape_alive() {
	v3_bin := build_v3_review_transform()
	source := 'struct Registry {
mut:
	callbacks []fn () int
}

fn install(mut registry Registry, callback fn () int) {
	registry.callbacks << callback
}

fn main() {
	mut registry := &Registry{}
	mut value := 42
	(fn [mut registry, mut value] () {
		install(mut registry, fn [mut value] () int {
			return value
		})
	})()
	println(int_str(registry.callbacks[0]()))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_side_effect_escape_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	println_pos := main_body.index('println') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert println_pos >= 0, main_body
	assert destroy_pos > println_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_side_effect_escape', source)
	assert out == '42'
}

fn test_immediately_invoked_factory_closure_keeps_side_effect_capture_escape_alive() {
	v3_bin := build_v3_review_transform()
	source := 'struct Registry {
mut:
	callbacks []fn () int
}

fn install(mut registry Registry, callback fn () int) {
	registry.callbacks << callback
}

fn make_callback(mut registry Registry) fn () {
	mut value := 42
	return fn [mut registry, mut value] () {
		install(mut registry, fn [mut value] () int {
			return value
		})
	}
}

fn main() {
	mut registry := Registry{}
	make_callback(mut registry)()
	println(int_str(registry.callbacks[0]()))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_factory_closure_side_effect_escape_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	println_pos := main_body.index('println') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert println_pos >= 0, main_body
	assert destroy_pos > println_pos, main_body
	out := run_good(v3_bin, 'immediate_factory_closure_side_effect_escape', source)
	assert out == '42'
}

fn test_immediately_invoked_closure_keeps_projected_capture_escapes_alive() {
	v3_bin := build_v3_review_transform()
	source := 'struct PointerHolder {
mut:
	value &int
}

fn main() {
	external := 0
	mut holder := &PointerHolder{
		value: unsafe { &external }
	}
	mut selector_value := 40
	(fn [mut holder, mut selector_value] () {
		holder.value = unsafe { &selector_value }
	})()

	mut pointers := [unsafe { &external }]!
	mut index_value := 41
	(fn [mut pointers, mut index_value] () {
		pointers[0] = unsafe { &index_value }
	})()

	println(int_str(unsafe { *holder.value }))
	println(int_str(unsafe { *pointers[0] }))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_projected_escapes_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	println_pos := main_body.last_index('println') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert println_pos >= 0, main_body
	assert destroy_pos > println_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_projected_escapes', source)
	assert out == '40\n41'
}

fn test_immediately_invoked_closure_keeps_channel_sent_capture_alive() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	ch := chan &int{cap: 1}
	mut value := 42
	(fn [ch, mut value] () {
		ch <- unsafe { &value }
	})()
	p := <-ch
	println(int_str(unsafe { *p }))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_closure_channel_escape_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	println_pos := main_body.index('println') or { -1 }
	destroy_pos := main_body.index('closure__closure_try_destroy(__immediate_closure_') or { -1 }
	assert println_pos >= 0, main_body
	assert destroy_pos > println_pos, main_body
	out := run_good(v3_bin, 'immediate_closure_channel_escape', source)
	assert out == '42'
}

fn test_disjoint_same_name_closure_bindings_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Value {
mut:
	n int
}

fn (value &Value) read() int {
	return value.n
}

fn main() {
	mut total := 0
	for i in 0 .. 10_000 {
		if i >= 0 {
			mut first := Value{
				n: i
			}
			callback := first.read
			first.n += 10
			total += callback()
		}
		if i < 10_000 {
			mut second := Value{
				n: i + 1
			}
			callback := second.read
			second.n += 20
			total += callback()
		}
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'disjoint_same_name_closure_bindings_c', source)
	assert c_source.count('closure__closure_try_destroy(callback);') >= 2, c_source
	out := run_good(v3_bin, 'disjoint_same_name_closure_bindings', source)
	assert out == '100300000'
}

fn test_branch_produced_immediate_method_closures_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Value {
	n int
}

fn (value &Value) read() int {
	return value.n
}

fn main() {
	mut total := 0
	for i in 0 .. 20_000 {
		first := Value{
			n: i
		}
		second := Value{
			n: i + 100
		}
		keep := i % 2 == 0
		total += (if keep { first.read } else { second.read })()
		total += (match keep {
			true { first.read }
			else { second.read }
		})()
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'branch_immediate_method_closures_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert main_body.count('closure__closure_try_destroy(') >= 2, main_body
	out := run_good(v3_bin, 'branch_immediate_method_closures', source)
	assert out == '401980000'
}

fn test_fixed_array_defer_results_use_semantic_array_storage() {
	v3_bin := build_v3_review_transform()
	source := 'fn fixed_array_result() [2]int {
	defer {
		assert $res()[0] == 1
		assert $res()[1] == 2
	}
	return [1, 2]!
}

fn multi_result() ([2]int, int) {
	defer {
		assert $res(0)[0] == 3
		assert $res(0)[1] == 4
	}
	return [3, 4]!, 5
}

fn main() {
	array := fixed_array_result()
	values, n := multi_result()
	assert array == [1, 2]!
	assert values == [3, 4]!
	assert n == 5
	println("ok")
}
'
	c_source := gen_c_from_source(v3_bin, 'fixed_array_defer_result_c', source)
	assert c_source.contains('(_t1.ret_arr)[0]'), c_source
	assert c_source.contains('(_t1.arg0)[0]'), c_source
	out := run_good(v3_bin, 'fixed_array_defer_result', source)
	assert out == 'ok'
}

fn test_arm64_backend_rejects_defer_results_before_ssa() {
	v3_bin := build_v3_review_transform()
	run_bad_backend(v3_bin, 'arm64_defer_result', 'arm64', 'fn f() int {
	defer {
		assert $res() == 42
	}
	return 42
}

fn main() {
	println(int_str(f()))
}
', '`\$res()` is not supported by the V3 arm64 backend')
}

fn test_eval_backend_rejects_active_defer_results() {
	v3_bin := build_v3_review_transform()
	run_bad_backend(v3_bin, 'eval_defer_result', 'eval', 'fn f() int {
	defer {
		assert $res() == 42
	}
	return 42
}

fn main() {
	println(int_str(f()))
}
', '`\$res()` is not supported by the V3 eval backend')
}

fn test_wasm_backend_rejects_defer_results_before_codegen() {
	v3_bin := build_v3_review_transform()
	run_bad_backend(v3_bin, 'wasm_defer_result', 'wasm', 'fn f() int {
	defer {
		assert $res() == 42
	}
	return 42
}

fn main() {
	println(int_str(f()))
}
', '`\$res()` is not supported by the V3 wasm backend')
}

fn test_thread_handle_equality_uses_platform_comparison() {
	v3_bin := build_v3_review_transform()
	source := 'fn answer() int {
	return 42
}

fn main() {
	worker := spawn answer()
	copy_handle := worker
	assert worker == copy_handle
	assert !(worker != copy_handle)
	println(int_str(worker.wait()))
}
'
	c_source := gen_c_from_source(v3_bin, 'thread_handle_equality_c', source)
	$if windows {
		assert c_source.contains('return a.handle == b.handle;'), c_source
	} $else {
		assert c_source.contains('pthread_equal(a.handle, b.handle) != 0'), c_source
	}
	assert !c_source.contains('memcmp(&__thread_'), c_source
	out := run_good(v3_bin, 'thread_handle_equality', source)
	assert out == '42'
}

fn test_c_flag_d_macro_uses_cli_override() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_flags(v3_bin, 'c_flag_d_override', '-d N=42', "module main

#flag -DCNUMBER=$d('N', 1234)

fn main() {
	println(int_str(int(C.CNUMBER)))
}
")
	assert out == '42'
}

fn test_synthetic_closure_runtime_import_preserves_user_alias() {
	v3_bin := build_v3_review_transform()
	aliased := run_good_project(v3_bin, 'closure_runtime_user_alias', {
		'main.v':                    'module main

import app.callbacks as closure

fn main() {
	value := 3
	callback := fn [value] () int {
		return value
	}
	println(int_str(closure.answer() + callback()))
}
'
		'app/callbacks/callbacks.v': 'module callbacks

pub fn answer() int {
	return 39
}
'
	}, 'main.v')
	assert aliased == '42'
	natural := run_good_project(v3_bin, 'closure_runtime_natural_alias', {
		'main.v':                'module main

import app.closure

fn main() {
	println(int_str(closure.answer()))
}
'
		'app/closure/closure.v': 'module closure

pub fn answer() int {
	return 42
}
'
	}, 'main.v')
	assert natural == '42'
	imported_capture := run_good_project(v3_bin, 'closure_runtime_imported_capture', {
		'main.v':              'module main

import app.worker

fn main() {
	println(int_str(worker.answer()))
}
'
		'app/worker/worker.v': 'module worker

pub fn answer() int {
	value := 42
	callback := fn [value] () int {
		return value
	}
	return callback()
}
'
	}, 'main.v')
	assert imported_capture == '42'
}

fn test_escaping_mut_method_value_rejects_stack_receiver() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (mut counter Counter) next() int {
	counter.value++
	return counter.value
}
'
	run_bad(v3_bin, 'mut_method_value_stack_receiver_direct', source + 'fn make() fn () int {
	mut counter := Counter{}
	return counter.next
}

fn main() {
	_ = make()
}
', 'mutable local receiver cannot escape')
	run_bad(v3_bin, 'mut_method_value_stack_receiver_alias', source + 'fn make() fn () int {
	mut counter := Counter{}
	callback := counter.next
	return callback
}

fn main() {
	_ = make()
}
', 'mutable local receiver cannot escape')
	in_scope := run_good(v3_bin, 'mut_method_value_in_scope_borrows_receiver', source + 'fn main() {
	mut counter := Counter{}
	callback := counter.next
	println(int_str(callback()))
	println(int_str(counter.value))
}
')
	assert in_scope == '1\n1'
	safe := run_good(v3_bin, 'value_method_value_escape', 'struct ValueCounter {
	value int
}

fn (counter ValueCounter) current() int {
	return counter.value
}

fn make(value int) fn () int {
	return ValueCounter{
		value: value
	}.current
}

fn main() {
	first := make(11)
	second := make(22)
	println(int_str(first()))
	println(int_str(second()))
}
')
	assert safe == '11\n22'
}

fn test_local_immutable_pointer_receiver_method_value_borrows_receiver() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'local_immutable_pointer_receiver_borrows', 'struct Foo {
mut:
	value int
}

fn (foo &Foo) read() int {
	return foo.value
}

fn main() {
	mut foo := Foo{
		value: 1
	}
	callback := unsafe { foo.read }
	foo.value = 2
	println(int_str(callback()))
}
')
	assert out == '2'
}

fn test_escaping_pointer_receiver_method_value_copies_addressable_local() {
	v3_bin := build_v3_review_transform()
	source := 'struct Foo {
mut:
	value int
}

fn (foo &Foo) read() int {
	return foo.value
}

fn make(value int) fn () int {
	foo := Foo{
		value: value
	}
	return unsafe { foo.read }
}

fn make_via_pointer_alias(value int) fn () int {
	foo := Foo{
		value: value
	}
	p := &foo
	cb := unsafe { p.read }
	return cb
}

fn make_from_pointer(foo &Foo) fn () int {
	return unsafe { foo.read }
}

fn overwrite_stack() {
	mut values := [512]int{}
	for i in 0 .. values.len {
		values[i] = i
	}
}

fn main() {
	first := make(11)
	second := make(22)
	aliased := make_via_pointer_alias(55)
	mut durable := &Foo{
		value: 33
	}
	third := make_from_pointer(durable)
	durable.value = 44
	for _ in 0 .. 100 {
		overwrite_stack()
	}
	println(int_str(first()))
	println(int_str(second()))
	println(int_str(third()))
	println(int_str(aliased()))
}
'
	out := run_good(v3_bin, 'escaped_pointer_receiver_addressable_local', source)
	assert out == '11\n22\n44\n55'
}

fn test_stored_pointer_receiver_method_value_keeps_local_pointee_alive() {
	v3_bin := build_v3_review_transform()
	source := 'struct Foo {
	value int
}

fn (foo &Foo) read() int {
	return foo.value
}

struct Holder {
mut:
	callback fn () int
}

fn install(mut holder Holder, value int) {
	local := Foo{
		value: value
	}
	p := &local
	holder.callback = unsafe { p.read }
}

fn overwrite_stack() {
	mut values := [512]int{}
	for i in 0 .. values.len {
		values[i] = i
	}
}

fn main() {
	mut holder := Holder{
		callback: fn () int {
			return 0
		}
	}
	install(mut holder, 73)
	for _ in 0 .. 100 {
		overwrite_stack()
	}
	println(int_str(holder.callback()))
}
'
	c_source := gen_c_from_source(v3_bin, 'stored_pointer_receiver_local_pointee_c', source)
	assert !c_source.contains('p = &local;'), c_source
	out := run_good(v3_bin, 'stored_pointer_receiver_local_pointee', source)
	assert out == '73'
}

fn test_callback_argument_method_value_keeps_mutable_local_receiver_alive() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (mut counter Counter) next() int {
	counter.value++
	return counter.value
}

struct Holder {
mut:
	callback fn () int
}

fn install(mut holder Holder, callback fn () int) {
	holder.callback = callback
}

fn make_callback(mut holder Holder) {
	mut counter := Counter{
		value: 40
	}
	install(mut holder, counter.next)
}

fn overwrite_stack() {
	mut values := [512]int{}
	for i in 0 .. values.len {
		values[i] = i
	}
}

fn main() {
	mut holder := Holder{
		callback: fn () int {
			return 0
		}
	}
	make_callback(mut holder)
	for _ in 0 .. 100 {
		overwrite_stack()
	}
	println(int_str(holder.callback()))
	println(int_str(holder.callback()))
}
'
	out := run_good(v3_bin, 'callback_argument_mutable_local_receiver', source)
	assert out == '41\n42'
}

fn test_callback_argument_method_value_preserves_mutable_receiver_identity() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (mut counter Counter) next() int {
	counter.value++
	return counter.value
}

fn invoke(callback fn () int) int {
	return callback()
}

fn main() {
	mut counter := Counter{}
	println(int_str(invoke(counter.next)))
	println(int_str(counter.value))
}
'
	out := run_good(v3_bin, 'callback_argument_mutable_receiver_identity', source)
	assert out == '1\n1'
}

fn test_callback_aggregate_argument_preserves_pointer_receiver_identity() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

struct Holder {
	callback fn () int
}

fn invoke(holder Holder, mut counter Counter) int {
	counter.value++
	return holder.callback()
}

fn main() {
	mut counter := Counter{}
	value := invoke(Holder{
		callback: counter.read
	}, mut counter)
	println(int_str(value))
	println(int_str(counter.value))
}
'
	out := run_good(v3_bin, 'callback_aggregate_argument_receiver_identity', source)
	assert out == '1\n1'
}

fn test_returned_mut_fixed_array_capture_uses_durable_context_storage() {
	v3_bin := build_v3_review_transform()
	source := 'fn make_counter() fn () int {
	mut values := [1, 20]!
	return fn [mut values] () int {
		values[0]++
		return values[0] + values[1]
	}
}

fn overwrite_stack() {
	mut values := [512]int{}
	for i in 0 .. values.len {
		values[i] = i
	}
}

fn main() {
	callback := make_counter()
	for _ in 0 .. 100 {
		overwrite_stack()
	}
	println(int_str(callback()))
	println(int_str(callback()))
}
'
	out := run_good(v3_bin, 'returned_mut_fixed_array_capture', source)
	assert out == '22\n23'
}

fn test_void_installer_mut_fixed_array_capture_uses_durable_storage() {
	v3_bin := build_v3_review_transform()
	source := 'struct Holder {
mut:
	callback fn () int
}

fn install(mut holder Holder) {
	mut values := [1, 20]!
	holder.callback = fn [mut values] () int {
		values[0]++
		return values[0] + values[1]
	}
}

fn overwrite_stack() {
	mut values := [512]int{}
	for i in 0 .. values.len {
		values[i] = i
	}
}

fn main() {
	mut holder := Holder{
		callback: fn () int {
			return 0
		}
	}
	install(mut holder)
	for _ in 0 .. 100 {
		overwrite_stack()
	}
	println(int_str(holder.callback()))
	println(int_str(holder.callback()))
}
'
	out := run_good(v3_bin, 'void_installer_mut_fixed_array_capture', source)
	assert out == '22\n23'
}

fn test_local_mut_fixed_array_capture_is_not_heap_promoted() {
	v3_bin := build_v3_review_transform()
	source := 'fn exercise() int {
	for i in 0 .. 50_000 {
		mut values := [i, 0]!
		callback := fn [mut values] () int {
			values[0]++
			return values[0]
		}
		assert callback() == i + 1
	}
	return 42
}

fn main() {
	println(int_str(exercise()))
}
'
	c_source := gen_c_from_source(v3_bin, 'local_mut_fixed_array_capture_c', source)
	assert c_source.contains('closure__closure_try_destroy(callback);'), c_source
	assert !c_source.contains('memdup(&__esc'), c_source
	out := run_good(v3_bin, 'local_mut_fixed_array_capture', source)
	assert out == '42'
}

fn test_immediately_invoked_mut_fixed_array_capture_is_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'fn exercise() int {
	for i in 0 .. 50_000 {
		mut values := [i, 0]!
		value := fn [mut values] () int {
			values[0]++
			return values[0]
		}()
		assert value == i + 1
	}
	return 42
}

fn main() {
	println(int_str(exercise()))
}
'
	c_source := gen_c_from_source(v3_bin, 'immediate_mut_fixed_array_capture_c', source)
	assert c_source.contains('closure__closure_try_destroy(__immediate_closure_'), c_source
	assert !c_source.contains('memdup(&__esc'), c_source
	out := run_good(v3_bin, 'immediate_mut_fixed_array_capture', source)
	assert out == '42'
}

fn test_mut_fixed_array_capture_shares_durable_outer_storage() {
	v3_bin := build_v3_review_transform()
	source := 'fn main() {
	mut values := [2]int{}
	update := fn [mut values] () int {
		values[0]++
		return values[0]
	}
	assert update() == 1
	assert values[0] == 1
	values[0] = 41
	assert update() == 42
	assert values[0] == 42
	println("ok")
}
'
	out := run_good(v3_bin, 'mut_fixed_array_capture_shared_storage', source)
	assert out == 'ok'
}

fn test_mutable_method_call_rejects_constant_receiver() {
	v3_bin := build_v3_review_transform()
	run_bad(v3_bin, 'mut_method_constant_receiver', 'struct Counter {
mut:
	value int
}

fn (mut counter Counter) increment() {
	counter.value++
}

const counter = Counter{}

fn main() {
	counter.increment()
}
', 'cannot modify constant `counter`')
}

fn test_mut_value_capture_in_call_under_selector_base() {
	v3_bin := build_v3_review_transform()
	// A `[mut s]` value capture used as a call argument nested inside a selector base
	// (`wrap(s).s.n`) must still be lowered to its value; the selector-base deref
	// suppression applies only to the direct receiver ident, not to nested expressions.
	out := run_good(v3_bin, 'mut_capture_selector_base', 'struct S {\n\tn int\n}\n\nstruct Box {\n\ts S\n}\n\nfn wrap(s S) Box {\n\treturn Box{\n\t\ts: s\n\t}\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut s := S{\n\t\tn: 7\n\t}\n\tcall(fn [mut s] () {\n\t\tprintln(int_str(wrap(s).s.n))\n\t})\n}\n')
	assert out == '7'
}

fn test_mut_value_capture_parenthesized_selector_receiver() {
	v3_bin := build_v3_review_transform()
	// A `[mut s]` value capture is a `&S` local; a parenthesized direct receiver
	// (`(s).n`) is still the direct selector receiver and must keep the suppression so
	// the selector emits arrow access. Otherwise the inner `s` is auto-dereferenced to
	// `*s` while the selector still emits `->`, producing an invalid `(*s)->n`.
	out := run_good(v3_bin, 'mut_capture_paren_selector_base', 'struct S {\n\tn int\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut s := S{\n\t\tn: 7\n\t}\n\tcall(fn [mut s] () {\n\t\tprintln(int_str((s).n))\n\t})\n}\n')
	assert out == '7'
}

fn test_heap_escaping_amp_alias_keeps_heap_pointer() {
	v3_bin := build_v3_review_transform()
	// When a local `s` whose address escapes is moved to the heap, `s` becomes the `&S`
	// heap pointer and the alias `p := &s` must stay that pointer (`p := s`), NOT be
	// auto-dereferenced to `*s`. Over-dereferencing here initializes `p`'s `&S` decl from
	// an `S` value (a stale stack copy), reviving the escape/stale-mutation bug the heap
	// move avoids. A later `s = S{n: 2}` must be observable through the returned pointer.
	out := run_good(v3_bin, 'heap_escaping_amp_alias', 'struct S {\n\tn int\n}\n\nfn leak() &S {\n\tmut s := S{\n\t\tn: 1\n\t}\n\tp := &s\n\ts = S{\n\t\tn: 2\n\t}\n\treturn p\n}\n\nfn main() {\n\tp := leak()\n\tprintln(int_str(p.n))\n}\n')
	assert out == '2'
}

fn test_scalar_return_call_does_not_heap_promote_address_argument() {
	v3_bin := build_v3_review_transform()
	c_source := gen_c_from_source(v3_bin, 'scalar_return_address_arg', 'struct Item {
	value int
}

fn inspect(item &Item) int {
	return item.value
}

fn forward() int {
	item := Item{
		value: 7
	}
	return inspect(&item)
}

fn main() {
	println(int_str(forward()))
}
')
	body := c_fn_body(c_source, 'int forward(void) {')
	assert body.contains('main__Item item ='), body
	assert !body.contains('main__Item* item ='), body
	assert !body.contains('memdup(&__esc'), body
}

fn test_smartcast_selector_return_keeps_existing_sum_box() {
	v3_bin := build_v3_review_transform()
	c_source := gen_c_from_source(v3_bin, 'smartcast_selector_sum_return', 'struct First {
	value int
}

struct Second {
	value int
}

type Value = First | Second

struct Holder {
	value Value
}

fn selected(holder &Holder) Value {
	if holder.value is First {
		return holder.value
	}
	return holder.value
}

fn main() {
	holder := Holder{
		value: First{value: 7}
	}
	println(selected(&holder))
}
')
	body := c_fn_body(c_source, 'Value selected(main__Holder* holder) {')
	assert body.contains('return holder->value;'), body
	assert !body.contains('memdup('), body
}

fn test_returned_closure_alias_heap_promotes_captured_pointer_source() {
	v3_bin := build_v3_review_transform()
	source := 'struct Value {
mut:
	n int
}

fn make(initial int) fn () int {
	mut value := Value{
		n: initial
	}
	p := &value
	cb := fn [p] () int {
		return p.n
	}
	value.n++
	return cb
}

fn overwrite_stack(seed int) {
	mut values := [512]int{}
	for i in 0 .. values.len {
		values[i] = seed + i
	}
}

fn main() {
	first := make(10)
	second := make(20)
	for i in 0 .. 100 {
		overwrite_stack(i)
	}
	println(int_str(first()))
	println(int_str(second()))
}
'
	c_source := gen_c_from_source(v3_bin, 'returned_closure_alias_pointer_capture_c', source)
	body := c_fn_body(c_source, ' make(int initial) {')
	assert body.contains('main__Value* value = (main__Value*)memdup'), body
	out := run_good(v3_bin, 'returned_closure_alias_pointer_capture', source)
	assert out == '11\n21'
}

fn test_heap_escaping_amp_reassignment_moves_current_source() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'heap_escaping_amp_reassign_source', 'fn make() &int {\n\tmut a := 10\n\tmut b := 20\n\tmut p := &a\n\tp = &b\n\treturn p\n}\n\nfn main() {\n\tprintln(int_str(*make()))\n}\n')
	assert out == '20'
	c_source := gen_c_from_source(v3_bin, 'heap_escaping_amp_reassign_source_c', 'fn make() &int {\n\tmut a := 10\n\tmut b := 20\n\tmut p := &a\n\tp = &b\n\treturn p\n}\n\nfn main() {\n\t_ := make()\n}\n')
	body := c_fn_body(c_source, 'int* make(void) {')
	assert body.contains('int* b ='), body
	assert !body.contains('p = &b;'), body
}

fn test_map_index_selector_write_retains_local_address() {
	v3_bin := build_v3_review_transform()
	source := 'struct Item {
mut:
	value int
}

struct Slot {
mut:
	item &Item = unsafe { nil }
}

fn make_cache() map[string]Slot {
	mut cache := map[string]Slot{}
	cache["entry"] = Slot{}
	mut local := Item{
		value: 7
	}
	cache["entry"].item = &local
	local.value = 9
	return cache
}

fn main() {
	cache := make_cache()
	println(int_str(cache["entry"].item.value))
}
'
	c_source := gen_c_from_source(v3_bin, 'map_index_selector_write_retains_local_address_c', source)
	body := c_fn_body(c_source, 'map make_cache(void) {')
	assert body.contains('memdup'), body
	out := run_good(v3_bin, 'map_index_selector_write_retains_local_address', source)
	assert out == '9'
}

fn test_return_address_of_pointer_backed_field_preserves_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'return_pointer_backed_field_address', 'struct Node[T] {\nmut:\n\tvalue T\n}\n\nstruct List[T] {\nmut:\n\ttail &Node[T] = unsafe { nil }\n}\n\nfn (list &List[T]) last() &T {\n\treturn &list.tail.value\n}\n\nfn main() {\n\tmut node := &Node[int]{\n\t\tvalue: 1\n\t}\n\tlist := List[int]{\n\t\ttail: node\n\t}\n\tmut last := list.last()\n\tunsafe {\n\t\t*last = 9\n\t}\n\tprintln(int_str(node.value))\n}\n')
	assert out == '9'
}

fn test_imported_result_array_return_or_preserves_success_value() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_result_array_return_or', {
		'v.mod':     "Module { name: 'imported_result_array_return_or' }\n"
		'main.v':    "module main\n\nimport pat\n\nfn main() {\n\tlines := pat.from_path()!\n\tassert lines == ['ok']\n\tprintln(lines[0])\n}\n"
		'pat/pat.v': "module pat\n\nfn source() ![]string {\n\treturn ['ok']\n}\n\npub fn from_path() ![]string {\n\treturn source() or {\n\t\treturn error(err.msg())\n\t}\n}\n"
	}, 'main.v')
	assert out == 'ok'
}

fn test_result_multi_return_match_branch_unwraps_payload_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'result_multi_return_match_branch_unwrap', "enum Kind {\n\tleft\n\tright\n}\n\nfn left_pair() !(int, string) {\n\treturn 1, 'left'\n}\n\nfn right_pair() !(int, string) {\n\treturn 2, 'right'\n}\n\nfn choose(kind Kind) !(int, string) {\n\treturn match kind {\n\t\t.left {\n\t\t\tleft_pair()!\n\t\t}\n\t\t.right {\n\t\t\tright_pair()!\n\t\t}\n\t}\n}\n\nfn main() {\n\tn, label := choose(.right)!\n\tprintln(int_str(n) + ':' + label)\n}\n")
	assert out == '2:right'
}

fn test_result_multi_return_match_branch_unwraps_imported_payload_type() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'result_multi_return_match_imported_payload', {
		'v.mod':  "Module { name: 'result_multi_return_match_imported_payload' }\n"
		'main.v': "module main\n\nimport m\n\nenum Kind {\n\tleft\n\tright\n}\n\nstruct Wrap {\n\tkind  Kind\n\tinner m.Inner\n}\n\nfn (wrap Wrap) choose() !(m.Match, []string) {\n\treturn match wrap.kind {\n\t\t.left {\n\t\t\twrap.inner.pair('left')!\n\t\t}\n\t\t.right {\n\t\t\twrap.inner.pair('right')!\n\t\t}\n\t}\n}\n\nfn main() {\n\twrap := Wrap{\n\t\tkind:  .right\n\t\tinner: m.Inner{\n\t\t\tn: 2\n\t\t}\n\t}\n\tmat, groups := wrap.choose()!\n\tprintln(int_str(mat.n) + ':' + groups[0])\n}\n"
		'm/m.v':  'module m\n\npub struct Match {\npub:\n\tn int\n}\n\npub struct Inner {\npub:\n\tn int\n}\n\npub fn (inner Inner) pair(label string) !(Match, []string) {\n\treturn Match{\n\t\tn: inner.n\n\t}, [label]\n}\n'
	}, 'main.v')
	assert out == '2:right'
}

fn test_string_to_owned_compiles_under_ownership_cgen() {
	v3_bin := build_v3_review_transform_ownership()
	out := run_good_with_flags(v3_bin, 'string_to_owned_ownership_cgen', '-ownership', "fn main() {\n\tname := 'owned'.to_owned()\n\tcopy := name.to_owned()\n\tprintln(copy)\n}\n")
	assert out == 'owned'
}

fn test_owned_value_receiver_method_closure_clones_and_drops_context() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Holder implements IClone {
	text   string
	values []int
}

fn (holder Holder) read() string {
	return holder.text + ":" + int_str(holder.values[0])
}

fn make_holder_reader() fn () string {
	holder := Holder{
		text:   "owned".to_owned()
		values: [7]
	}
	return holder.read
}

fn use_callbacks() {
	holder_cb := make_holder_reader()
	println(holder_cb())
	println(holder_cb())
}

fn main() {
	use_callbacks()
}
'
	src_path := os.join_path(os.temp_dir(), 'v3_owned_value_receiver_method_closure.v')
	c_path := os.join_path(os.temp_dir(), 'v3_owned_value_receiver_method_closure.c')
	os.write_file(src_path, source) or { panic(err) }
	gen := os.execute('${v3_bin} -nocache -ownership ${src_path} -b c -o ${c_path}')
	assert gen.exit_code == 0, gen.output
	c_source := os.read_file(c_path) or { panic(err) }
	assert c_source.contains('closure__closure_create_with_data_and_drop'), c_source
	assert c_source.contains('string__free(&((ctx->receiver).text));'), c_source
	assert c_source.contains('.receiver = __v3_method_receiver_clone_'), c_source
	assert c_source.contains('Holder__read(__v3_method_receiver_clone_'), c_source
	out := run_good_with_flags(v3_bin, 'owned_value_receiver_method_closure', '-ownership', source)
	assert out == 'owned:7\nowned:7'
}

fn test_owned_rvalue_method_receiver_is_materialized_before_cloning() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Holder implements IClone {
	text   string
	values []int
}

fn (holder Holder) read() string {
	return holder.text + ":" + int_str(holder.values[0])
}

fn make_holder() Holder {
	return Holder{
		text:   "owned".to_owned()
		values: [7]
	}
}

fn main() {
	callback := make_holder().read
	println(callback())
	println(callback())
}
'
	src_path := os.join_path(os.temp_dir(), 'v3_owned_rvalue_method_receiver.v')
	c_path := os.join_path(os.temp_dir(), 'v3_owned_rvalue_method_receiver.c')
	os.write_file(src_path, source) or { panic(err) }
	gen := os.execute('${v3_bin} -nocache -ownership ${src_path} -b c -o ${c_path}')
	assert gen.exit_code == 0, gen.output
	c_source := os.read_file(c_path) or { panic(err) }
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert main_body.contains('((void*)&__method_receiver_'), main_body
	assert main_body.contains('string__free(&((__method_receiver_'), main_body
	assert !main_body.contains('&(make_holder())'), main_body
	out := run_good_with_flags(v3_bin, 'owned_rvalue_method_receiver', '-ownership', source)
	assert out == 'owned:7\nowned:7'
}

fn test_owned_fn_literal_capture_context_has_type_aware_drop() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'fn exercise() int {
	mut total := 0
	for i in 0 .. 10 {
		text := ("item" + int_str(i)).to_owned()
		values := [i]
		callback := fn [text, values] () int {
			return text.len + values[0]
		}
		total += callback()
	}
	return total
}

fn main() {
	println(int_str(exercise()))
}
'
	src_path := os.join_path(os.temp_dir(), 'v3_owned_fn_literal_capture_context.v')
	c_path := os.join_path(os.temp_dir(), 'v3_owned_fn_literal_capture_context.c')
	os.write_file(src_path, source) or { panic(err) }
	gen := os.execute('${v3_bin} -nocache -ownership -gc none ${src_path} -b c -o ${c_path}')
	assert gen.exit_code == 0, gen.output
	c_source := os.read_file(c_path) or { panic(err) }
	assert c_source.contains('static void _flctxdrop_'), c_source
	assert c_source.contains('closure__closure_create_with_data_and_drop'), c_source
	assert c_source.contains('string__free(&((*ctx).text));'), c_source
	assert c_source.contains('array__free(&((*ctx).values));'), c_source
	out := run_good_with_flags(v3_bin, 'owned_fn_literal_capture_context', '-ownership -gc none', source)
	assert out == '95'
}

fn test_generic_interface_method_body_marks_log_debug_dispatch() {
	v3_bin := build_v3_review_transform_ownership()
	out := run_good_with_flags(v3_bin, 'generic_interface_log_debug_dispatch', '-ownership', "import log\n\ninterface Sink {\n\tbinary_data()\n}\n\nstruct Box[T] {}\n\nfn (mut b Box[T]) binary_data() {\n\t_ = b\n\tlog.debug('hidden')\n}\n\nstruct Runner {}\n\nfn (mut r Runner) run(mut s Sink) {\n\t_ = r\n\ts.binary_data()\n}\n\nstruct Worker {\nmut:\n\trunner Runner\n}\n\nfn main() {\n\tmut worker := Worker{\n\t\trunner: Runner{}\n\t}\n\tmut b := Box[int]{}\n\tworker.runner.run(mut b)\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_generic_interface_mut_pointer_parameter_uses_erased_dispatch_abi() {
	v3_bin := build_v3_review_transform_ownership()
	out := run_good_with_flags(v3_bin, 'generic_interface_mut_pointer_dispatch', '-ownership', 'interface Writer[T] {
	write(mut value T) !bool
}

struct Text {
mut:
	value string
}

struct Count {
	padding [7]u8
mut:
	value int
}

struct TextWriter {}
struct CountWriter {}

fn (_ TextWriter) write(mut value Text) !bool {
	value.value = "done"
	return true
}

fn (_ CountWriter) write(mut value Count) !bool {
	value.value = 42
	return true
}

fn apply[T](writer &Writer[T], mut value T) !bool {
	return writer.write(mut value)
}

fn main() {
	mut text := Text{
		value: "before"
	}
	mut count := Count{
		value: 1
	}
	assert apply(TextWriter{}, mut text)!
	assert apply(CountWriter{}, mut count)!
	println(text.value + ":" + int_str(count.value))
}
')
	assert out == 'done:42'
}

fn test_generic_interface_implementer_result_uses_dispatch_abi() {
	v3_bin := build_v3_review_transform_ownership()
	out := run_good_with_flags(v3_bin, 'generic_interface_implementer_result_dispatch', '-ownership', 'interface Writer {
mut:
	write(buf []u8) !int
}

struct Buffer {}

fn (mut b Buffer) write(buf []u8) !int {
	_ = b
	return buf.len
}

struct CounterWriter[W] {
mut:
	inner W
}

fn (mut w CounterWriter[W]) write(buf []u8) !int {
	$if W is Writer {
		return w.inner.write(buf)
	} $else {
		return error("not a writer")
	}
}

fn write_len(mut writer Writer) !int {
	return writer.write([u8(1), 2, 3])
}

fn main() {
	mut plain := Buffer{}
	assert write_len(mut plain)! == 3
	mut counter := CounterWriter[Buffer]{
		inner: Buffer{}
	}
	assert write_len(mut counter)! == 3
	println("ok")
}
')
	assert out == 'ok'
}

fn test_materialized_generic_interface_implementer_has_runtime_type_name() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_body_materialized_interface_implementer', 'interface Any {
	str() string
}

struct Box[T] {
	value T
}

fn main() {
	boxed := Any(Box[int]{
		value: 7
	})
	println(boxed.type_name() + ":" + boxed.str())
}
')
	assert out == 'Box[int]:Box[int]{\n    value: 7\n}'
}

fn test_array_literal_separator_handling() {
	v3_bin := build_v3_review_transform()
	// Comma-, newline-, and blank-line-separated element lists parse with the expected length.
	out := run_good(v3_bin, 'array_literal_separators', 'const nl = [\n\t1\n\t2\n\t3\n]\nconst blank = [\n\t4\n\n\t5\n]\n\nfn main() {\n\tcommas := [6, 7, 8]\n\tprintln(int_str(nl.len) + ":" + int_str(blank.len) + ":" + int_str(commas.len))\n}\n')
	assert out == '3:2:3'
	run_bad(v3_bin, 'array_literal_missing_separator', 'fn main() {\n\t_ := [1 2]\n}\n', 'unexpected token `2`, expecting `]`')
	run_bad(v3_bin, 'array_literal_doubled_comma', 'fn main() {\n\t_ := [1,,2]\n}\n', 'unexpected token `,`, expecting `]`')
}

fn test_container_wrapped_import_alias_type_resolves() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'container_import_alias_types', {
		'v.mod':     "Module { name: 'container_import_alias_types' }\n"
		'bar/bar.v': 'module bar\n\npub struct Baz {\npub:\n\tn int\n}\n'
		'foo/foo.v': 'module foo\n\nimport bar as b\n\npub struct Holder {\npub:\n\titems []b.Baz\n\tone   ?b.Baz\n}\n\npub fn make() Holder {\n\treturn Holder{\n\t\titems: [b.Baz{\n\t\t\tn: 1\n\t\t}, b.Baz{\n\t\t\tn: 2\n\t\t}]\n\t\tone: b.Baz{\n\t\t\tn: 7\n\t\t}\n\t}\n}\n'
		'main.v':    'module main\n\nimport foo\n\nfn main() {\n\th := foo.make()\n\tmut out := int_str(h.items[0].n) + int_str(h.items[1].n)\n\tif v := h.one {\n\t\tout += int_str(v.n)\n\t}\n\tprintln(out)\n}\n'
	}, 'main.v')
	assert out == '127'
}

fn test_nested_map_equality_uses_declared_value_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_map_semantic_equality', "struct Item {\n\tname string\n\tparts []string\n}\n\nstruct Holder {\n\titems map[string][]Item\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tmut left_map := map[string][]Item{}\n\tleft_map['items'] = [Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}]\n\tmut right_map := map[string][]Item{}\n\tright_map['items'] = [Item{\n\t\tname: join('a', 'b')\n\t\tparts: [join('x', 'y')]\n\t}]\n\tleft_arr := [left_map]\n\tright_arr := [right_map]\n\tleft_holder := Holder{\n\t\titems: left_map\n\t}\n\tright_holder := Holder{\n\t\titems: right_map\n\t}\n\tprintln(left_arr == right_arr)\n\tprintln(left_holder == right_holder)\n}\n")
	assert out == 'true\ntrue'
}

fn test_pointer_array_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_array_equality', 'struct Node {\n\tvalue int\n}\n\nfn main() {\n\tleft_node := Node{\n\t\tvalue: 5\n\t}\n\tright_node := Node{\n\t\tvalue: 5\n\t}\n\tleft_ptr := &left_node\n\tright_ptr := &right_node\n\tleft := [left_ptr]\n\tright := [right_ptr]\n\tsame := [left_ptr]\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == same)\n\tprintln(right_ptr in left)\n\tprintln(int_str(left.index(right_ptr)))\n}\n')
	assert out == 'false\ntrue\ntrue\nfalse\n-1'
}

fn test_struct_pointer_equality_is_semantic() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_pointer_semantic_equality', "struct Person {\n\tname string\n\ttags []string\n}\n\nfn main() {\n\tleft := &Person{\n\t\tname: 'abc'.clone()\n\t\ttags: ['x'.clone()]\n\t}\n\tright := &Person{\n\t\tname: ('a' + 'bc')\n\t\ttags: [('x' + '')]\n\t}\n\tsame := left\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == same)\n}\n")
	assert out == 'true\nfalse\ntrue'
}

fn test_multilevel_struct_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'multilevel_struct_pointer_identity_equality', "struct Person {\n\tname string\n}\n\nfn main() {\n\tmut left := &Person{\n\t\tname: 'same'\n\t}\n\tmut right := &Person{\n\t\tname: 'same'\n\t}\n\tleft_slot := &left\n\tright_slot := &right\n\tsame_slot := left_slot\n\tprintln(left_slot == right_slot)\n\tprintln(left_slot != right_slot)\n\tprintln(left_slot == same_slot)\n\tprintln(*left_slot == *right_slot)\n}\n")
	assert out == 'false\ntrue\ntrue\ntrue'
}

fn test_struct_equality_with_interface_field_compiles() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_eq_interface_field', "interface Thing {\n\tvalue() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) value() int {\n\treturn i.n\n}\n\nstruct Box {\n\tthing Thing\n\tlabel string\n}\n\nfn main() {\n\titem := Item{\n\t\tn: 7\n\t}\n\tleft := Box{\n\t\tthing: item\n\t\tlabel: 'same'\n\t}\n\tright := left\n\tprintln(left == right)\n}\n")
	assert out == 'true'
}

fn test_array_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_pointer_equality', 'fn main() {\n\tleft := [1, 2]\n\tright := [1, 2]\n\tleft_ptr := &left\n\tright_ptr := &right\n\tsame_ptr := left_ptr\n\tprintln(left_ptr == right_ptr)\n\tprintln(left_ptr != right_ptr)\n\tprintln(left_ptr == same_ptr)\n\tprintln(*left_ptr == *right_ptr)\n}\n')
	assert out == 'false\ntrue\ntrue\ntrue'
}

fn test_pointer_u8_array_bytestr_stays_in_cgen() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_u8_array_bytestr', 'fn show(data &[]u8) string {\n\treturn data.bytestr()\n}\n\nfn main() {\n\tbytes := [u8(104), u8(105)]\n\tprintln(show(&bytes))\n}\n')
	assert out == 'hi'
}

fn test_map_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_pointer_equality', "fn main() {\n\tleft := {\n\t\t'x': 1\n\t}\n\tright := {\n\t\t'x': 1\n\t}\n\tleft_ptr := &left\n\tright_ptr := &right\n\tsame_ptr := left_ptr\n\tprintln(left_ptr == right_ptr)\n\tprintln(left_ptr != right_ptr)\n\tprintln(left_ptr == same_ptr)\n\tprintln(*left_ptr == *right_ptr)\n}\n")
	assert out == 'false\ntrue\ntrue\ntrue'
}

fn test_cyclic_interface_default_does_not_deref_nil_global() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'cyclic_interface_default', {
		'v.mod':         "Module { name: 'cyclic_interface_default' }\n"
		'cycle/cycle.v': "module cycle\n\ninterface Named {\n\tid string\n}\n\nconst empty_stack = stack(id: 'empty')\n\nstruct Stack {\n\tparent Named = empty_stack\n\tid     string\n}\n\nfn stack(id string) &Stack {\n\treturn &Stack{\n\t\tid: id\n\t}\n}\n\npub fn empty_id() string {\n\treturn empty_stack.id\n}\n"
		'main.v':        "module main\n\nimport cycle\n\nfn main() {\n\t_ = cycle.empty_id()\n\tprintln('alive')\n}\n"
	}, 'main.v')
	assert out == 'alive'
}

fn test_fixed_array_values_compare_semantically() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'fixed_array_semantic_equality', "fn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := [[1]string{init: 'ab'.clone()}]\n\tright := [[1]string{init: join('a', 'b')}]\n\tmut map_left := map[string][1]string{}\n\tmap_left['k'] = [1]string{init: 'cd'.clone()}\n\tmut map_right := map[string][1]string{}\n\tmap_right['k'] = [1]string{init: join('c', 'd')}\n\tmut ints_left := map[string][2]i64{}\n\tints_left['k'] = [i64(1), i64(0)]!\n\tmut ints_right := map[string][2]i64{}\n\tints_right['k'] = [i64(2), i64(0)]!\n\tprintln(left == right)\n\tprintln(left.equals(right))\n\tprintln(map_left == map_right)\n\tprintln(ints_left == ints_right)\n}\n")
	assert out == 'true\ntrue\ntrue\nfalse'
}

fn test_const_length_fixed_array_map_values_compare_semantically() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'const_len_fixed_array_map_equality', {
		'main.v':        'module main\n\nimport store\n\nfn main() {\n\tprintln(store.check())\n}\n'
		'store/store.v': "module store\n\nconst n = 2\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\npub fn check() bool {\n\tmut left := map[string][n]string{}\n\tleft['k'] = [n]string{init: 'ab'.clone()}\n\tmut right := map[string][n]string{}\n\tright['k'] = [n]string{init: join('a', 'b')}\n\treturn left == right\n}\n"
	}, 'main.v')
	assert out == 'true'
}

fn test_interface_array_repeat_evaluates_receiver_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_repeat_side_effects', 'interface Thing {\n\tvalue() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) value() int {\n\treturn i.n\n}\n\n__global calls int\n\nfn make_item() Thing {\n\tcalls++\n\treturn Item{\n\t\tn: calls\n\t}\n}\n\nfn main() {\n\titems := [make_item()].repeat(3)\n\tprintln(int_str(calls))\n\tprintln(int_str(items[0].value() + items[1].value() + items[2].value()))\n}\n')
	assert out == '1\n3'
}

fn test_negative_is_return_smartcasts_following_statements() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'negative_is_return_smartcast', 'struct MapKind {\n\tkey_type int\n\tvalue_type int\n}\nstruct OtherKind {}\ntype Kind = MapKind | OtherKind\n\nfn passthrough(k Kind) Kind {\n\treturn k\n}\n\nfn score(k Kind) int {\n\tclean := passthrough(k)\n\tif clean !is MapKind {\n\t\treturn 0\n\t}\n\treturn clean.key_type + clean.value_type\n}\n\nfn main() {\n\tprintln(int_str(score(Kind(MapKind{\n\t\tkey_type: 2\n\t\tvalue_type: 5\n\t}))))\n\tprintln(int_str(score(Kind(OtherKind{}))))\n}\n')
	assert out == '7\n0'
}

fn test_if_expr_smartcast_selector_decl_does_not_smartcast_local() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'if_expr_selector_decl_smartcast_local', 'struct Cat {\n\tage int\n}\nstruct Dog {\n\ttricks int\n}\ntype Animal = Cat | Dog\n\nstruct Ident {\n\tobj Animal\n}\n\nfn has_age(cat Cat) bool {\n\treturn cat.age == 3\n}\n\nfn main() {\n\tleft := Ident{\n\t\tobj: Animal(Cat{\n\t\t\tage: 2\n\t\t})\n\t}\n\tmut obj := if left.obj is Cat {\n\t\tleft.obj\n\t} else {\n\t\tCat{}\n\t}\n\tif true {\n\t\tobj = Cat{\n\t\t\tage: 3\n\t\t}\n\t}\n\tprintln(has_age(obj))\n}\n')
	assert out == 'true'
}

fn test_comptime_type_conditions_handle_logical_ops() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_type_condition_logical_ops', "fn classify[T](x T) int {\n\t_ := x\n\t\$if T !is string && T !is \$int && T !is []u8 {\n\t\treturn 1\n\t} \$else {\n\t\treturn 2\n\t}\n\treturn 0\n}\n\nfn grouped[T](x T) int {\n\t_ := x\n\t\$if (T is int || T is string) && T is bool {\n\t\treturn 1\n\t} \$else {\n\t\treturn 2\n\t}\n\treturn 0\n}\n\nfn main() {\n\tprintln(int_str(classify('abc')))\n\tprintln(int_str(classify(3)))\n\tprintln(int_str(classify([u8(1)])))\n\tprintln(int_str(classify(1.5)))\n\tprintln(int_str(grouped(3)))\n\tprintln(int_str(grouped('abc')))\n}\n")
	assert out == '2\n2\n2\n1\n2\n2'
}

fn test_comptime_type_conditions_keep_prefix_types_compact() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_type_condition_prefix_types', 'struct Foo {}\n\nfn main() {\n\t\$if ?int is ?int {\n\t\tprintln("opt")\n\t} \$else {\n\t\tprintln("badopt")\n\t}\n\t\$if !Foo is !Foo {\n\t\tprintln("res")\n\t} \$else {\n\t\tprintln("badres")\n\t}\n}\n')
	assert out == 'opt\nres'
}

fn test_comptime_type_conditions_qualify_module_aliases() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'comptime_type_condition_module_alias', {
		'main.v':    'module main\n\nimport foo\n\nfn main() {\n\tprintln(foo.check())\n}\n'
		'foo/foo.v': "module foo\n\ntype ID = int\n\npub fn check() string {\n\t\$if ID is \$alias {\n\t\treturn 'alias'\n\t} \$else {\n\t\treturn 'not alias'\n\t}\n}\n"
	}, 'main.v')
	assert out == 'alias'
}

fn test_imported_generic_indirections_conditions_keep_integer_literals() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'generic_indirections_integer_literals', {
		'v.mod':         "Module { name: 'generic_indirections_integer_literals' }\n"
		'probe/probe.v': 'module probe\n\npub fn depth[T](value T) int {\n\t_ = value\n\t\$if T.indirections == 0 {\n\t\treturn 0\n\t} \$else \$if T.indirections == 1 {\n\t\treturn 1\n\t}\n\treturn 2\n}\n'
		'main.v':        'module main\n\nimport probe\n\nfn main() {\n\tn := 7\n\tprintln(int_str(probe.depth(n)))\n\tprintln(int_str(probe.depth(&n)))\n}\n'
	}, 'main.v')
	assert out == '0\n1'
}

fn test_nested_comptime_field_names_do_not_replace_each_other() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_comptime_field_name_prefixes', 'struct Embedded {\n\tn int\n}\n\nstruct Item {\n\tEmbedded\n\tname string\n}\n\nfn normal_fields[T]() int {\n\tmut count := 0\n\t\$for field in T.fields {\n\t\t\$if field.is_embed {\n\t\t\t\$for reserved_field in T.fields {\n\t\t\t\t\$if !reserved_field.is_embed {\n\t\t\t\t\tcount++\n\t\t\t\t}\n\t\t\t}\n\t\t}\n\t}\n\treturn count\n}\n\nfn main() {\n\tprintln(int_str(normal_fields[Item]()))\n}\n')
	assert out == '1'
}

fn test_struct_equality_compares_pointer_fields_as_pointers() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_eq_pointer_field', 'struct Node {\n\tvalue int\n\tnext &Node\n}\n\nfn main() {\n\tleft := Node{\n\t\tvalue: 7\n\t\tnext: unsafe { nil }\n\t}\n\tright := Node{\n\t\tvalue: 7\n\t\tnext: unsafe { nil }\n\t}\n\tprintln([left] == [right])\n}\n')
	assert out == 'true'
}

fn test_single_module_test_file_skips_premodule_attributes() {
	v3_bin := build_v3_review_transform()
	root := os.join_path(os.temp_dir(), 'v3_premodule_attr_module_test')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'tar')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "premodule_attr_module_test" }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'tar', 'reader.v'), 'module tar /* implementation module */\n\nfn reader_value() string {\n\treturn "reader"\n}\n') or {
		panic(err)
	}
	test_file := os.join_path(root, 'tar', 'reader_test.v')
	os.write_file(test_file, '@[has_globals]\n/* block comment before module */\nmodule tar // test module\n\nfn test_reader_value() {\n\tprintln(reader_value())\n}\n') or {
		panic(err)
	}
	bin_path := os.join_path(root, 'reader_test_bin')
	compile := os.execute('${v3_bin} ${test_file} -b c -o ${bin_path}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_path)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'reader'
}

fn test_delete_last_empty_array_panics_before_tail_clear() {
	v3_bin := build_v3_review_transform()
	src := 'fn main() {\n\tmut values := []int{}\n\tvalues.delete_last()\n\tprintln("after")\n}\n'
	good_src := os.join_path(os.temp_dir(), 'v3_delete_last_empty.v')
	os.write_file(good_src, src) or { panic(err) }
	good_bin := os.join_path(os.temp_dir(), 'v3_delete_last_empty')
	compile := os.execute('${v3_bin} ${good_src} -b c -o ${good_bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(good_bin)
	assert run.exit_code != 0, run.output
	assert run.output.contains('array.delete_last: array is empty'), run.output
}

fn test_delete_last_preserves_shared_slice_buffer() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'delete_last_preserves_shared_slice_buffer', "fn main() {\n\tmut a := [1, 2, 3, 4]\n\tb := unsafe { a[..a.len] }\n\told_data := a.data\n\ta.delete_last()\n\tassert a == [1, 2, 3]\n\tassert b == [1, 2, 3, 4]\n\tassert a.data != old_data\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_slice_element_assignment_writes_through() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'slice_element_assignment_writes_through', "fn main() {\n\tmut a := [1, 2, 3, 4]\n\tmut s := unsafe { a[1..3] }\n\ts[0] = 42\n\ts[1] += 5\n\tassert a == [1, 42, 8, 4]\n\tassert s == [42, 8]\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_string_pointer_comparisons_keep_pointer_semantics() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'string_pointer_comparison', "fn main() {\n\tleft := 'same'.clone()\n\tright := 'same'.clone()\n\tpleft := &left\n\tpright := &right\n\tprintln(pleft == pright)\n\tprintln(pleft != pright)\n\tprintln(*pleft == *pright)\n}\n")
	assert out == 'false\ntrue\ntrue'
}

fn test_map_keys_and_values_lower_to_runtime_methods() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_keys_values_lowering', "fn make_lookup() map[string]int {\n\treturn {\n\t\t'one': 1\n\t\t'two': 2\n\t}\n}\n\nfn main() {\n\tlookup := make_lookup()\n\tkeys := lookup.keys()\n\tvalues := make_lookup().values()\n\tmut total := 0\n\tfor value in values {\n\t\ttotal += value\n\t}\n\tsingle := {\n\t\t'only': 9\n\t}\n\tprintln(int_str(keys.len))\n\tprintln(int_str(values.len))\n\tprintln(int_str(total))\n\tprintln(int_str(single.keys().len))\n\tprintln(single.keys()[0])\n\tprintln(int_str(single.values().len))\n\tprintln(int_str(single.values()[0]))\n}\n")
	assert out == '2\n2\n3\n1\nonly\n1\n9'
}

fn test_map_str_preserves_signed_wide_entries() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_str_signed_wide_entries', "fn main() {\n\tvalue_map := {\n\t\t'x': i64(5000000000)\n\t}\n\tkey_map := {\n\t\ti64(-5000000000): 'x'\n\t}\n\tprintln(value_map.str())\n\tprintln(key_map.str())\n}\n")
	assert out == "{'x': 5000000000}\n{-5000000000: 'x'}"
}

fn test_map_str_normalizes_alias_key_and_value_types() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_str_alias_kinds', "type ID = int\n\ntype Amount = f64\n\nfn main() {\n\tids := {\n\t\tID(23): 'id'\n\t}\n\tamounts := {\n\t\t'price': Amount(1.25)\n\t}\n\tprintln('\${ids}')\n\tprintln('\${amounts}')\n}\n")
	assert out == "{23: 'id'}\n{'price': 1.25}"
}

fn test_chained_array_alias_stringification_uses_outer_alias_only() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'chained_array_alias_str', "type A = []int\n\ntype B = A\n\nfn main() {\n\tvalue := B([1, 2])\n\tprintln('\${value}')\n}\n")
	assert out == 'B([1, 2])'
}

fn test_alias_pointer_receiver_str_gets_addressable_value() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'alias_pointer_receiver_str', "type Number = int\n\nfn (n &Number) str() string {\n\treturn 'number:' + int_str(int(*n))\n}\n\nstruct Point {\n\tx int\n}\n\ntype NamedPoint = Point\n\nfn (p &NamedPoint) str() string {\n\treturn 'point:' + int_str(p.x)\n}\n\nfn main() {\n\tn := Number(7)\n\tp := NamedPoint(Point{\n\t\tx: 9\n\t})\n\tprintln('\${n}')\n\tprintln('\${Number(8)}')\n\tprintln('\${p}')\n\tprintln('\${NamedPoint(Point{x: 10})}')\n}\n")
	assert out == 'number:7\nnumber:8\npoint:9\npoint:10'
}

fn test_mut_map_param_interpolation_preserves_pointer() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'mut_map_param_interpolation', "type Scores = map[string]int\n\nfn show(mut m map[string]int) {\n\tprintln('\${m}')\n}\n\nfn show_alias(mut m Scores) {\n\tprintln('\${m}')\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm['x'] = 3\n\tshow(mut m)\n\tmut scores := Scores(map[string]int{})\n\tscores['y'] = 4\n\tshow_alias(mut scores)\n}\n")
	assert out == "{'x': 3}\n{'y': 4}"
}

fn test_map_literal_stringification_evaluates_entries_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_literal_str_side_effects', "__global key_calls int\n__global val_calls int\n\nfn next_key() string {\n\tkey_calls++\n\treturn 'k' + int_str(key_calls)\n}\n\nfn next_val() int {\n\tval_calls++\n\treturn val_calls * 10\n}\n\nfn main() {\n\tprintln({\n\t\tnext_key(): next_val()\n\t})\n\tprintln(int_str(key_calls) + ',' + int_str(val_calls))\n}\n")
	assert out == "{'k1': 10}\n1,1"
}

fn test_map_literal_declaration_evaluates_entries_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_literal_decl_side_effects', "__global key_calls int\n__global val_calls int\n\nfn next_key() string {\n\tkey_calls++\n\treturn 'key'\n}\n\nfn next_val() int {\n\tval_calls++\n\treturn val_calls * 10\n}\n\nfn main() {\n\tvalues := {\n\t\tnext_key(): next_val()\n\t}\n\tprintln(int_str(values['key']))\n\tprintln(int_str(key_calls) + ',' + int_str(val_calls))\n}\n")
	assert out == '10\n1,1'
}

fn test_fn_literal_preserves_mut_param_string_interpolation() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'fn_literal_mut_param_interp', "struct Counter {\n\tvalue int\n}\n\nfn show(mut counter Counter) {\n\t_ := fn () {}\n\tprintln('\${counter.value}')\n}\n\nfn main() {\n\tmut counter := Counter{\n\t\tvalue: 42\n\t}\n\tshow(mut counter)\n}\n")
	assert out == '42'
}

fn test_shadowed_minmaxof_calls_are_not_rewritten() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'shadowed_minmaxof_calls', {
		'main.v':          'module main\n\nimport shadow { maxof, minof }\n\nfn main() {\n\tprintln(int_str(maxof[int]()))\n\tprintln(int_str(minof[int]()))\n}\n'
		'shadow/shadow.v': 'module shadow\n\npub fn maxof[T]() int {\n\treturn 7\n}\n\npub fn minof[T]() int {\n\treturn -7\n}\n'
	}, 'main.v')
	assert out == '7\n-7'
}

fn test_runes_iterator_index_is_loop_scoped() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'runes_iterator_index_scope', "fn main() {\n\tfor i, r in 'ab'.runes_iterator() {\n\t\t_ := r\n\t\tprintln(int_str(i))\n\t}\n\ti := 9\n\tprintln(int_str(i))\n}\n")
	assert out == '0\n1\n9'
}

fn test_array_last_index_uses_element_width() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_last_index_element_width', 'fn main() {\n\twide := [i64(1), i64(5000000000), i64(2), i64(5000000000)]\n\tfloats := [1.25, 2.5, 1.25]\n\tflags := [true, false, true]\n\tprintln(int_str(wide.last_index(i64(5000000000))))\n\tprintln(int_str(floats.last_index(1.25)))\n\tprintln(int_str(flags.last_index(true)))\n}\n')
	assert out == '3\n2\n2'
}

fn test_array_last_index_uses_semantic_element_comparison() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_last_index_semantic_equality', "struct Item {\n\tname string\n\tparts []string\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tnested := [['ab'.clone()], [join('x', 'y')], [join('a', 'b')]]\n\tnested_needle := ['ab'.clone()]\n\titems := [Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}, Item{\n\t\tname: join('a', 'b')\n\t\tparts: [join('x', 'y')]\n\t}]\n\tneedle := Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}\n\tprintln(int_str(nested.last_index(nested_needle)))\n\tprintln(int_str(items.last_index(needle)))\n}\n")
	assert out == '2\n1'
}

fn test_generic_string_literal_matching_typeof_marker_is_preserved() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_marker_string_literal', "fn marker_and_type[T](value T) string {\n\tmarker := '__v3_generic_type_name:T'\n\treturn marker + '|' + typeof(value).name\n}\n\nfn main() {\n\tprintln(marker_and_type(7))\n}\n")
	assert out == '__v3_generic_type_name:T|int'
}

fn test_parallel_monomorphization_grows_uneven_worker_regions() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform()
	mut declarations := []string{cap: 40}
	mut calls := []string{cap: 40}
	for i in 0 .. 40 {
		declarations << 'struct MonoGrow${i} { value int }'
		calls << '\ttotal += outer(MonoGrow${i}{value: ${i}})'
	}
	src := '${declarations.join('\n')}\n\nfn inner[T](value T) int {\n\t_ = value\n\treturn 1\n}\n\nfn outer[T](value T) int {\n\treturn inner(value)\n}\n\nfn main() {\n\tmut total := 0\n${calls.join('\n')}\n\tprintln(total)\n}\n'
	out :=
		run_good_with_env(v3_bin, 'parallel_monomorph_grow', 'VJOBS=4 V3_TEST_MONOMORPH_GROW=1', src)
	assert out == '40'
}

fn test_parallel_monomorphization_expands_single_seed() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform()
	mut declarations := []string{cap: 40}
	mut calls := []string{cap: 40}
	for i in 0 .. 40 {
		declarations << 'struct MonoSeed${i} {}'
		calls << '\ttotal += inner(MonoSeed${i}{})'
	}
	src := '${declarations.join('\n')}\n\nfn inner[T](value T) int {\n\t_ = value\n\treturn 1\n}\n\nfn seed[T]() int {\n\t_ = T{}\n\tmut total := 0\n${calls.join('\n')}\n\treturn total\n}\n\nfn main() {\n\tprintln(seed[MonoSeed0]())\n}\n'
	out := run_good_with_env(v3_bin, 'parallel_monomorph_single_seed', 'VJOBS=4', src)
	assert out == '40'
}

fn test_parallel_monomorphization_registers_worker_fixed_array_signatures() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform()
	mut declarations := []string{cap: 40}
	mut calls := []string{cap: 40}
	for i in 0 .. 40 {
		declarations << 'struct MonoSignature${i} {}'
		calls << '\ttotal += fixed_pair(MonoSignature${i}{})[0]'
	}
	src := '${declarations.join('\n')}\n\nfn fixed_pair[T](value T) [2]int {\n\t_ = value\n\treturn [1, 2]!\n}\n\nfn main() {\n\tmut total := 0\n${calls.join('\n')}\n\tprintln(total)\n}\n'
	out := run_good_with_env(v3_bin, 'parallel_monomorph_signatures', 'VJOBS=4', src)
	assert out == '40'
}

fn test_generic_function_type_arguments_keep_parameter_commas() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_function_type_argument', "fn identity[T](value T) T {
	return value
}

fn accepts(value int, label string) bool {
	return value == label.len
}

fn main() {
	callback := identity(accepts)
	println(callback(3, 'abc'))
}
")
	assert out == 'true'
}

fn test_returned_mut_callback_preserves_pointer_parameter() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'returned_mut_callback_parameter', 'struct Item {
mut:
	value int
}

fn change(mut item Item) {
	item.value = 7
}

fn get_callback() fn (mut Item) {
	return change
}

fn main() {
	mut item := Item{}
	callback := get_callback()
	callback(mut item)
	println(int_str(item.value))
}
')
	assert out == '7'
}

fn test_recursive_closure_assignment_refreshes_self_capture() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'recursive_closure_assignment', 'fn main() {
	one := 1
	mut factorial := fn (n int) int {
		return 1
	}
	factorial = fn [one, factorial] (n int) int {
		if n <= 1 {
			return one
		}
		return n * factorial(n - 1)
	}
	println(int_str(factorial(5)))
}
')
	assert out == '120'
}

fn test_typeof_function_fixed_array_types_keep_function_shape() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'typeof_function_fixed_array_types', 'fn values() [3]int {
	return [1, 2, 3]!
}

fn first_two(input [3]int) [2]int {
	return [input[0], input[1]]!
}

const values_fn_type_name = typeof(values).name
const first_two_fn_type_name = typeof(first_two).name

fn main() {
	values_fn := values
	first_two_fn := first_two
	println(typeof(values_fn).name)
	println(typeof(first_two_fn).name)
	println(values_fn_type_name)
	println(first_two_fn_type_name)
}
')
	assert out == 'fn () [3]int\nfn ([3]int) [2]int\nfn () [3]int\nfn ([3]int) [2]int'
}

fn test_typeof_idx_uses_active_smartcast() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'typeof_idx_smartcast', 'struct Foo {}
struct Bar {}

type Value = Foo | Bar

fn show(value Value) {
	if value is Foo {
		println(typeof(value).name)
		println((typeof(value).idx == typeof[Foo]().idx).str())
	}
	match value {
		Bar {
			println(typeof(value).name)
			println((typeof(value).idx == typeof[Bar]().idx).str())
		}
		else {}
	}
}

fn main() {
	show(Foo{})
	show(Bar{})
}
')
	assert out == 'Foo\ntrue\nBar\ntrue'
}

fn test_generic_typeof_idx_comparison_prunes_dead_branch() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_typeof_idx_comparison', 'fn pick_idx[T]() int {
	$if typeof[T]().idx == typeof[int]().idx {
		return 42
	} $else {
		return T.missing_method()
	}
}

fn concrete_idx() int {
	$if typeof[int]().idx == typeof[int]().idx {
		return 1
	} $else {
		return 2
	}
}

fn main() {
	println(concrete_idx())
	println(pick_idx[int]())
}
')
	assert out == '1\n42'
}

fn test_mut_map_for_in_writeback_survives_continue_and_break() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'mut_map_for_in_writeback_continue_break', 'struct Box {
mut:
	n int
}

fn main() {
	mut items := map[string]Box{}
	items["a"] = Box{n: 1}
	items["b"] = Box{n: 2}
	for _, mut value in items {
		value.n += 10
		continue
	}
	println(items["a"].n)
	println(items["b"].n)
	mut once := map[string]Box{}
	once["x"] = Box{n: 3}
	for _, mut value in once {
		value.n = 9
		break
	}
	println(once["x"].n)
}
')
	assert out == '11\n12\n9'
}

fn test_interface_to_interface_conversion_preserves_fields() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_to_interface_preserves_fields', 'interface Named {
	name string
}

interface Rich {
	name string
	describe() string
}

struct User {
	name string
}

fn (u User) describe() string {
	return "user:" + u.name
}

fn read_named(n Named) string {
	return n.name
}

fn main() {
	rich := Rich(User{
		name: "Ada"
	})
	println(read_named(rich))
	named := Named(rich)
	println(named.name)
}
')
	assert out == 'Ada\nAda'
}

fn test_pointer_interface_conversion_heap_copies_converted_interface() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_interface_conversion_heap_copy', 'interface Named {
	name string
}

interface Rich {
	name string
	describe() string
}

struct User {
	name string
}

fn (u User) describe() string {
	return "user:" + u.name
}

fn make_named(r Rich) &Named {
	return &Named(r)
}

fn read_named(n &Named) string {
	return n.name
}

fn main() {
	rich := Rich(User{
		name: "Ada"
	})
	named := make_named(rich)
	println(read_named(named))
	println(named.name)
}
')
	assert out == 'Ada\nAda'
}

fn test_interface_implicit_str_dispatch_preserves_receiver_values() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_implicit_str_receiver_values', 'interface Printable {
	str() string
}

struct Bar {
	x int
}

struct Custom {
	x int
}

fn (c &Custom) str() string {
	return "custom:" + int_str(c.x)
}

type Name = string

fn (n &Name) str() string {
	return "alias:" + string(*n)
}

struct Foo {
	x int
	bar Bar
	nums []int
	lookup map[string]int
	p &int
	custom &Custom
	name &Name
}

fn main() {
	mut n := 11
	mut custom := Custom{
		x: 12
	}
	mut name := Name("Ada")
	value := Printable(Foo{
		x: 7
		bar: Bar{
			x: 8
		}
		nums: [1, 2]
		lookup: {
			"a": 3
		}
		p: &n
		custom: &custom
		name: &name
	})
	text := value.str()
	println(text.contains("x: 7"))
	println(text.contains("Bar"))
	println(text.contains("x: 8"))
	println(text.contains("[1, 2]"))
	println(text.contains("a"))
	println(text.contains("3"))
	println(text.contains("11"))
	println(text.contains("custom:12"))
	println(text.contains("alias:Ada"))
}
')
	assert out == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue'
}

fn test_interface_implicit_str_dispatch_stringifies_collection_aliases() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_implicit_str_collection_aliases', 'interface Printable {
	str() string
}

type Items = []int
type Counts = map[string]int

fn main() {
	items := Printable(Items([1, 2]))
	mut raw_counts := map[string]int{}
	raw_counts["a"] = 3
	counts := Printable(Counts(raw_counts))
	count_text := counts.str()
	println(items.str())
	println(count_text.contains("a"))
	println(count_text.contains("3"))
}
')
	assert out == '[1, 2]\ntrue\ntrue'
}

fn test_empty_interface_stringification_distinguishes_array_element_types() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'empty_interface_mixed_array_stringification', 'interface Value {}

fn main() {
	values := [Value([1, 2]), Value(["x"])]
	for value in values {
		println(value)
	}
	println(values[0].type_idx() != values[1].type_idx())
}
')
	assert out == "Value([1, 2])\nValue(['x'])\ntrue"
}

fn test_empty_interface_type_idx_maps_raw_containers() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'empty_interface_raw_container_type_idx', 'interface Value {}

fn box[T](value T) Value {
	return value
}

fn main() {
	array_value := box([1, 2])
	map_value := box({
		"a": 3
	})
	int_value := Value(1)
	println(array_value.type_idx() != 0)
	println(map_value.type_idx() != 0)
	println(array_value.type_idx() != map_value.type_idx())
	println(array_value.type_idx() != int_value.type_idx())
}
')
	assert out == 'true\ntrue\ntrue\ntrue'
}

fn test_boxed_container_runtime_type_indexes_resolve_hash_collisions() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'boxed_container_runtime_type_index_hash_collisions', 'interface Value {}

struct Aaxtc {}
struct Abddb {}

fn main() {
	left := Value([Aaxtc{}])
	right := Value([Abddb{}])
	println(left.type_idx() != right.type_idx())
}
')
	assert out == 'true'
}

fn test_runtime_type_indexes_resolve_hash_collisions() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'runtime_type_index_hash_collisions', 'interface Value {}

struct ULz {}
struct AAbA {}
struct Uc {}
struct ACRB {}

type Pair = ULz | AAbA

fn main() {
	left := Pair(ULz{})
	right := Pair(AAbA{})
	first := Value(Uc{})
	second := Value(ACRB{})
	println(left.type_idx() != right.type_idx())
	println(first.type_idx() != second.type_idx())
}
')
	assert out == 'true\ntrue'
}

fn test_late_generic_runtime_type_indexes_resolve_hash_collisions() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'late_generic_runtime_type_index_hash_collisions', 'interface Value {}

struct Dxw {}
struct Kdd {}

struct Box[T] {
	value T
}

fn type_index[T](value T) int {
	boxed := Value(value)
	return boxed.type_idx()
}

fn main() {
	left := type_index(Box[Dxw]{
		value: Dxw{}
	})
	right := type_index(Box[Kdd]{
		value: Kdd{}
	})
	println(left != right)
}
')
	assert out == 'true'
}

fn test_optional_string_equality_uses_payload_equality() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_string_semantic_equality', "fn maybe_text(ok bool) ?string {\n\tif !ok {\n\t\treturn none\n\t}\n\tprefix := 'a'.clone()\n\treturn prefix + 'b'\n}\n\nfn main() {\n\tleft := maybe_text(true)\n\tright := maybe_text(true)\n\tmissing_left := maybe_text(false)\n\tmissing_right := maybe_text(false)\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == missing_left)\n\tprintln(missing_left == missing_right)\n\tprintln(missing_left != missing_right)\n}\n")
	assert out == 'true\nfalse\nfalse\ntrue\nfalse'
}

fn test_optional_nested_array_equality_guards_payload_work() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_nested_array_guarded_equality', "fn maybe_nested(ok bool) ?[][]string {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [['a'.clone()], ['b'.clone()]]\n}\n\nfn main() {\n\tleft := maybe_nested(true)\n\tright := maybe_nested(true)\n\tmissing_left := maybe_nested(false)\n\tmissing_right := maybe_nested(false)\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == missing_left)\n\tprintln(missing_left == missing_right)\n\tprintln(missing_left != missing_right)\n}\n")
	assert out == 'true\nfalse\nfalse\ntrue\nfalse'
}

fn test_optional_assignment_invalidates_payload_smartcast() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_assignment_invalidates_payload_smartcast', 'fn main() {\n\tmut value := ?int(none)\n\tvalue = 1\n\tvalue = none\n\tresolved := value or { 42 }\n\tprintln(int_str(resolved))\n}\n')
	assert out == '42'
}

fn test_optional_variant_to_optional_sum_cast_preserves_wrapper() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_variant_to_optional_sum_cast', "struct Cat {}\n\nstruct Dog {\n\tname string\n}\n\ntype Animal = Cat | Dog\n\nfn maybe_dog(ok bool) ?Dog {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn Dog{\n\t\tname: 'Rex'\n\t}\n}\n\nfn show(ok bool) string {\n\tmaybe_animal := ?Animal(maybe_dog(ok))\n\tanimal := maybe_animal or { return 'missing' }\n\tif animal is Dog {\n\t\treturn animal.name\n\t}\n\treturn 'cat'\n}\n\nfn main() {\n\tprintln(show(true))\n\tprintln(show(false))\n}\n")
	assert out == 'Rex\nmissing'
}

fn test_wrapped_plus_minus_continuations_consume_auto_semicolon() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'wrapped_plus_minus_continuation', 'fn add(total int, delta int) int {\n\treturn total\n\t\t+ delta\n}\n\nfn sub(total int, delta int) int {\n\treturn total\n\t\t- delta\n}\n\nfn main() {\n\tprintln(int_str(add(3, 4)))\n\tprintln(int_str(sub(9, 2)))\n}\n')
	assert out == '7\n7'
}

fn test_gated_optional_array_index_materializes_base_before_wrap() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'gated_optional_array_index_base_order', "fn get_arr(ok bool) ?[]int {\n\tprintln('get')\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [3, 7, 11]\n}\n\nfn main() {\n\tprintln(int_str(get_arr(true)#[-1] or { 40 }))\n\tprintln(int_str(get_arr(true)#[9] or { 41 }))\n\tprintln(int_str(get_arr(false)#[-1] or { 42 }))\n}\n")
	assert out == 'get\n11\nget\n41\nget\n42'
}

fn test_normalized_option_result_fixed_array_names_keep_outer_wrapper() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'normalized_option_result_fixed_array', "struct Foo {\n\tn int\n}\n\nfn opt_values(ok bool) ?[2]int {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [1, 2]!\n}\n\nfn res_values(ok bool) ![2]Foo {\n\tif !ok {\n\t\treturn error('x')\n\t}\n\treturn [Foo{\n\t\tn: 3\n\t}, Foo{\n\t\tn: 4\n\t}]!\n}\n\nfn main() {\n\ta := opt_values(true) or { [0, 0]! }\n\tb := res_values(true) or { [Foo{\n\t\tn: 0\n\t}, Foo{\n\t\tn: 0\n\t}]! }\n\tmissing_a := opt_values(false) or { [5, 6]! }\n\tmissing_b := res_values(false) or { [Foo{\n\t\tn: 7\n\t}, Foo{\n\t\tn: 8\n\t}]! }\n\tprintln(int_str(a[0] + a[1] + b[0].n + b[1].n))\n\tprintln(int_str(missing_a[0] + missing_a[1] + missing_b[0].n + missing_b[1].n))\n}\n")
	assert out == '10\n26'
}

fn test_hierarchical_import_runtime_inits_before_importer_init() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'hierarchical_runtime_init_order', {
		'main.v':            'module main\n\nimport foo.user\nimport bar as shortbar\n\nfn main() {\n\t_ := shortbar.value()\n\tprintln(int_str(user.value()))\n}\n'
		'foo/user/user.v':   'module user\n\nimport foo.bar as foobar\n\n__global seen int\n\nfn init() {\n\tseen = foobar.value() + 1\n}\n\npub fn value() int {\n\treturn seen\n}\n'
		'foo/bar/bar.v':     'module bar\n\n__global flag = make_flag()\n\nfn make_flag() int {\n\treturn 40\n}\n\npub fn value() int {\n\treturn flag\n}\n'
		'bar/bar.v':         'module bar\n\n__global flag = make_flag()\n\nfn make_flag() int {\n\treturn 3\n}\n\npub fn value() int {\n\treturn flag\n}\n'
		'unrelated/other.v': 'module other\n\npub fn value() int {\n\treturn 0\n}\n'
	}, 'main.v')
	assert out == '41'
}

fn test_lowered_generic_operator_call_records_specialization() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'lowered_generic_operator_call_records_specialization', 'struct Box[T] {\n\tv T\n}\n\nfn (a Box[T]) + (b Box[T]) Box[T] {\n\treturn Box[T]{\n\t\tv: a.v + b.v\n\t}\n}\n\nfn main() {\n\tleft := Box[int]{\n\t\tv: 2\n\t}\n\tright := Box[int]{\n\t\tv: 5\n\t}\n\tresult := left + right\n\tprintln(int_str(result.v))\n}\n')
	assert out == '7'
}

fn test_late_inferred_generic_call_emits_specialization() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'late_inferred_generic_call_emits_specialization', 'fn make[T]() T {\n\treturn T(41)\n}\n\nfn use[T](value T) T {\n\treturn value + T(1)\n}\n\nfn main() {\n\tx := make[int]()\n\tprintln(int_str(use(x)))\n}\n')
	assert out == '42'
}

fn test_late_reachable_body_generic_call_emits_specialization() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'late_reachable_body_generic_call_emits_specialization', 'fn identity[T](value T) T {\n\treturn value\n}\n\nfn late_helper() int {\n\treturn identity[int](42)\n}\n\nfn outer[T]() int {\n\treturn late_helper()\n}\n\nfn main() {\n\tprintln(int_str(outer[int]()))\n}\n')
	assert out == '42'
}

fn test_module_qualified_panic_keeps_module_symbol() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'module_qualified_panic_symbol', {
		'v.mod':     "Module { name: 'module_qualified_panic_symbol' }\n"
		'foo/foo.v': "module foo\n\npub fn panic() string {\n\treturn 'module panic'\n}\n"
		'main.v':    'module main\n\nimport foo\n\nfn main() {\n\tprintln(foo.panic())\n}\n'
	}, 'main.v')
	assert out == 'module panic'
}

fn test_vmodroot_c_flag_preserves_project_path_with_spaces() {
	v3_bin := build_v3_review_transform()
	root := os.join_path(os.temp_dir(), 'v3 flag pseudo path project')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	write_project_file(root, 'v.mod', "Module { name: 'flag_pseudo_path' }\n")
	write_project_file(root, 'main.v', 'module main\n\n#flag -I @VMODROOT/include -D FEATURE\n#insert "flag_value.c"\n\nfn C.flag_value() int\n\nfn main() {\n\tprintln(int_str(C.flag_value()))\n}\n')
	write_project_file(root, 'include/flag_value.c', '#include <flag_value.h>\n\nstatic inline int flag_value(void) {\n\treturn flag_value_inner();\n}\n')
	write_project_file(root, 'include/flag_value.h', 'static inline int flag_value_inner(void) {\n\treturn 57;\n}\n')
	bin := os.join_path(os.temp_dir(), 'v3_flag_pseudo_path')
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(os.join_path(root, 'main.v'))} -b c -o ${os.quoted_path(bin)}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(bin))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '57'
}

fn test_unrelated_system_include_keeps_c_extern_declaration() {
	v3_bin := build_v3_review_transform()
	header_name := 'v3_unrelated_system_include.h'
	header_path := os.join_path(os.temp_dir(), header_name)
	os.write_file(header_path, '#include <stdio.h>\n') or { panic(err) }
	defer {
		os.rm(header_path) or {}
	}
	c_source := gen_c_from_source(v3_bin, 'unrelated_system_include_c_extern', '#insert "${header_name}"

fn C.X509_free(voidptr)

fn main() {
	C.X509_free(voidptr(0))
}
')
	assert c_source.contains('void X509_free(void*'), c_source
}

fn test_imported_header_tree_uses_real_stdint_with_inttypes() {
	v3_bin := build_v3_review_transform()
	outer_name := 'v3_import_inttypes_outer.h'
	inner_name := 'v3_import_inttypes_inner.h'
	outer_path := os.join_path(os.temp_dir(), outer_name)
	inner_path := os.join_path(os.temp_dir(), inner_name)
	os.write_file(outer_path, '#import "${inner_name}"\n') or { panic(err) }
	os.write_file(inner_path, '#include <inttypes.h>\n#include <stdint.h>\ntypedef uint64_t ImportedWord;\n') or {
		panic(err)
	}
	defer {
		os.rm(outer_path) or {}
		os.rm(inner_path) or {}
	}
	c_source := gen_c_from_source(v3_bin, 'imported_header_inttypes_scan', '#insert "${outer_name}"

fn main() {}
')
	assert c_source.contains('#include <inttypes.h>'), c_source
	assert c_source.contains('#include <stdint.h>'), c_source
	assert !c_source.contains('#define __V_HEADERLESS_STDINT_H'), c_source
}

fn test_statement_array_append_consumes_rhs_expression() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'statement_array_append_rhs_expression', 'fn main() {\n\tmut value := u32(0x123)\n\tmut values := []u32{}\n\tvalues << value & 0xff\n\tprintln(int_str(int(values[0])))\n}\n')
	assert out == '35'
}

fn test_user_defined_scalar_map_method_append_pushes_one() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'user_defined_scalar_map_append', 'type Bar = int

fn (b Bar) map() int {
	return b + 1
}

fn main() {
	mut values := []int{}
	values << Bar(0).map()
	println(int_str(values.len))
	println(int_str(values[0]))
}
')
	assert out == '1\n1'
}

fn test_builtin_map_nested_array_append_pushes_one() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'builtin_map_nested_array_append', 'fn main() {
	mut values := [][]int{}
	values << [1, 2].map(it)
	println(int_str(values.len))
	println(int_str(values[0][1]))
}
')
	assert out == '1\n2'
}

fn test_array_valued_sum_variant_append_pushes_one() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_valued_sum_variant_append', 'type Value = int | []u8 | [2]u8

fn main() {
	mut values := []Value{}
	bytes := [u8(1), 2]
	values << bytes
	fixed := [u8(3), 4]!
	values << fixed
	println(int_str(values.len))
	inner := values[0] as []u8
	println(int_str(inner.len))
	println(int_str(int(inner[1])))
	inner_fixed := values[1] as [2]u8
	println(int_str(int(inner_fixed[0])))
	println(int_str(int(inner_fixed[1])))
}
')
	assert out == '2\n2\n2\n3\n4'
}

fn test_optional_append_to_map_value_copies_back_absent_entry() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_append_to_map_value_copyback', 'fn next_value() ?int {
	return 7
}

fn append_value(mut values map[string][]int) {
	values["new"] << next_value() or { return }
}

fn main() {
	mut values := map[string][]int{}
	append_value(mut values)
	println("new" in values)
	println(int_str(values["new"][0]))
}
')
	assert out == 'true\n7'
}

fn test_optional_map_append_evaluates_key_before_rhs() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_map_append_evaluation_order', 'struct State {\nmut:\n\tkey   string\n\ttrace string\n}\n\nfn select_key(key string, mut state State) string {\n\tstate.trace += "key"\n\treturn key\n}\n\nfn next_value(mut state State) ?int {\n\tstate.trace += "rhs"\n\tstate.key = "changed"\n\treturn 7\n}\n\nfn main() {\n\tmut state := State{\n\t\tkey: "original"\n\t}\n\tmut values := map[string][]int{}\n\tvalues[select_key(state.key, mut state)] << next_value(mut state) or { return }\n\tprintln(state.trace)\n\tprintln(int_str(values["original"][0]))\n\tprintln("changed" in values)\n}\n')
	assert out == 'keyrhs\n7\nfalse'
}

fn test_optional_append_to_shared_array_is_autolocked() {
	v3_bin := build_v3_review_transform()
	source := 'fn next_value() ?int {
	return 7
}

fn main() {
	shared values := []int{}
	values << next_value() or { return }
}
'
	c_source := gen_c_from_source(v3_bin, 'optional_append_to_shared_array_autolock_c', source)
	body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	push_idx := body.index('array_push(') or { -1 }
	assert push_idx >= 0, body
	lock_idx := body[..push_idx].last_index('sync__RwMutex__lock(') or { -1 }
	assert lock_idx >= 0, body
	unlock_rel := body[push_idx..].index('sync__RwMutex__unlock(') or { -1 }
	assert unlock_rel >= 0, body
}

fn test_failed_optional_append_probe_does_not_evaluate_rhs_twice() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_shift_rhs_evaluated_once', 'struct Counter {
mut:
	value int
}

fn next_value(mut calls Counter) ?int {
	calls.value++
	return 1
}

fn main() {
	mut calls := Counter{}
	flags := 2
	flags << next_value(mut calls) or { return }
	println(int_str(calls.value))
}
')
	assert out == '1'
}

fn test_json2_skipped_pointer_field_does_not_specialize_decoder() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_skipped_pointer_field', 'import gg\nimport x.json2\n\nstruct Config {\n\tcontext &gg.Context @[skip]\n\tname string\n}\n\nfn main() {\n\tconfig := json2.decode[Config]("{\\"name\\":\\"ok\\"}") or { panic(err) }\n\tprintln(config.name)\n}\n')
	assert out == 'ok'
}

fn test_comptime_field_generic_calls_keep_resolved_field_types() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'comptime_field_resolved_types', {
		'v.mod':         "Module { name: 'comptime_field_resolved_types' }\n"
		'model/model.v': 'module model\n\npub enum KeyCode {\n\tenter\n}\n\npub type Callback = fn (int)\n\n@[typedef]\npub struct C.model_event {\npub:\n\tkey KeyCode\n\tcb Callback\n}\n\npub type Event = C.model_event\n'
		'codec/codec.v': 'module codec\n\npub fn visit[T](mut value T) {\n\t\$for field in T.fields {\n\t\ttouch(mut value.\$(field.name))\n\t}\n}\n\nfn touch[T](mut value T) {\n\t_ = value\n}\n'
		'main.v':        'module main\n\nimport codec\nimport model\n\nfn main() {\n\tmut event := model.Event{}\n\tcodec.visit(mut event)\n\tprintln(int_str(int(event.key)))\n}\n'
	}, 'main.v')
	assert out == '0'
}

fn test_comptime_field_generic_call_prefers_shadowing_local_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_field_generic_shadowing_local', 'struct Sample {
	values map[string]int
}

fn inferred_type[T](value T) string {
	_ = value
	return typeof[T]().name
}

fn main() {
	sample := Sample{
		values: {
			"one": 1
		}
	}
	$for field in Sample.fields {
		$if field.is_map {
			for key, value in sample.$(field.name) {
				_ = key
				_ = value
			}
			key := 1.5
			println(inferred_type(key))
		}
	}
}
')
	assert out == 'f64'
}

fn test_comptime_pointer_field_generic_local_uses_call_return_type() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'comptime_pointer_field_call_return_type', {
		'v.mod':         "Module { name: 'comptime_pointer_field_call_return_type' }\n"
		'model/model.v': 'module model\n\npub struct App {\npub mut:\n\tvalue int\n}\n\npub struct Context {\npub mut:\n\tapp &App\n}\n'
		'codec/codec.v': 'module codec\n\npub fn fill[T](mut value T) {\n\t\$for field in T.fields {\n\t\t\$if field.indirections == 1 {\n\t\t\tmut decoded_ptr := create_ptr(value.\$(field.name))\n\t\t\tdecoded_ptr.value = 42\n\t\t\tvalue.\$(field.name) = decoded_ptr\n\t\t}\n\t}\n}\n\nfn create_ptr[T](_ &T) &T {\n\treturn &T{}\n}\n'
		'main.v':        'module main\n\nimport codec\nimport model\n\nfn main() {\n\tmut context := model.Context{\n\t\tapp: &model.App{}\n\t}\n\tcodec.fill(mut context)\n\tprintln(context.app.value)\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_imported_struct_default_qualifies_function_alias_cast() {
	v3_bin := build_v3_review_transform()
	c_source := gen_c_from_project(v3_bin, 'imported_struct_default_fn_alias', {
		'v.mod':           "Module { name: 'imported_struct_default_fn_alias' }\n"
		'widget/widget.v': 'module widget\n\npub type Callback = fn (int)\n\npub struct Config {\npub:\n\tcallback Callback = unsafe { Callback(0) }\n}\n'
		'main.v':          'module main\n\nimport widget\n\nfn main() {\n\tconfig := widget.Config{}\n\tprintln(config.callback == unsafe { nil })\n}\n'
	}, 'main.v')
	assert !c_source.contains('(Callback)'), c_source
}

fn test_imported_struct_defaults_keep_declaring_module_constants() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'imported_struct_default_constants', {
		'v.mod':           "Module { name: 'imported_struct_default_constants' }\n"
		'widget/widget.v': "module widget\n\npub const no_style = 'none'\npub const origin = [0.0, 0.0]\n\npub struct Config {\npub:\n\tstyle  string = no_style\n\torigin []f64  = origin\n}\n"
		'main.v':          'module main\n\nimport widget\n\nfn main() {\n\tconfig := widget.Config{}\n\tprintln(config.style)\n\tprintln(config.origin.len)\n}\n'
	}, 'main.v')
	assert out == 'none\n2'
}

fn test_json2_c_alias_fields_keep_declaring_module_types() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_c_alias_field_types', 'import sokol.sapp\nimport x.json2\n\nfn main() {\n\tevent := json2.decode[sapp.Event]("{}") or { sapp.Event{} }\n\tprintln(int_str(int(event.frame_count)))\n}\n')
	assert out == '0'
}

fn test_json2_reflected_map_alias_infers_value_type() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'json2_reflected_map_alias', {
		'v.mod':         "Module { name: 'json2_reflected_map_alias' }\n"
		'model/model.v': 'module model\n\npub type Values = map[string]int\n\npub struct Config {\npub mut:\n\tvalues Values\n}\n'
		'main.v':        'module main\n\nimport model\nimport x.json2\n\nfn main() {\n\tconfig := json2.decode[model.Config](r\'{"values":{"answer":42}}\')!\n\tprintln(config.values["answer"])\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_json2_reflected_fields_keep_independent_decoder_specializations() {
	v3_bin := build_v3_review_transform()
	src_file := os.join_path(os.temp_dir(), 'v3_json2_reflected_independent_field_decoders.v')
	c_file := os.join_path(os.temp_dir(), 'v3_json2_reflected_independent_field_decoders.c')
	os.write_file(src_file, 'import x.json2\n\nstruct Result {\n\tvalue int\n}\n\nstruct Weather {\n\tlang string\n\tresult Result\n}\n\nfn main() {\n\t_ := json2.decode[Weather](r\'{"lang":"en","result":{"value":42}}\')!\n}\n') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} -nocache ${src_file} -b c -o ${c_file}')
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_file) or { '' }
	assert c_code.contains('json2__Decoder_Result__decode_value(decoder, &decoded_field_value)'), c_code
}

fn test_json2_reflected_main_type_does_not_use_imported_homonym() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'json2_reflected_main_type_collision', {
		'v.mod':             "Module { name: 'json2_reflected_main_type_collision' }\n"
		'discord/discord.v': 'module discord\n\npub struct Discord {\npub:\n\tname string\n}\n'
		'main.v':            'module main\n\nimport discord\nimport x.json2\n\nstruct Discord {\n\tvalue int\n}\n\nstruct Chat {\n\tdiscord_apis []Discord\n}\n\nfn main() {\n\t_ = json2.encode(discord.Discord{})\n\tchat := json2.decode[Chat](r\'{"discord_apis":[{"value":42}]}\')!\n\tencoded := json2.encode(chat)\n\tprintln(json2.decode[Chat](encoded)!.discord_apis[0].value)\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_comptime_main_fielddata_uses_main_field_metadata() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_main_fielddata_metadata', 'struct FieldData {
pub mut:
	@[main_attr]
	value int
}

fn main() {
	$for field in FieldData.fields {
		println(field.name)
		println(field.is_pub)
		println(field.is_mut)
		println(field.attrs.join(","))
	}
}
')
	assert out == 'value\ntrue\ntrue\nmain_attr'
}

fn test_comptime_generic_embedded_field_uses_short_reflected_name() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_generic_embedded_field_name', 'struct Embedded[T] {
	value T
}

struct Container {
	Embedded[int]
}

fn main() {
	$for field in Container.fields {
		$if field.is_embed {
			println(field.name)
		}
	}
}
')
	assert out == 'Embedded'
}

fn test_comptime_main_enum_field_matches_enum_type_group() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_main_enum_field_type_group', 'enum State {
	idle
	running
}

struct Job {
	state State
}

fn enum_fields[T]() int {
	mut count := 0
	$for field in T.fields {
		$if field.unaliased_typ is $enum {
			count++
		}
	}
	return count
}

fn main() {
	println(enum_fields[Job]())
}
')
	assert out == '1'
}

fn test_json2_encode_keeps_independent_array_element_specializations() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_independent_array_element_specializations', 'import x.json2\n\nstruct Event {\n\tvalue int\n}\n\nstruct Payload {\n\tflags [][]bool\n\tevents []Event\n}\n\nfn main() {\n\tpayload := Payload{\n\t\tflags: [[true]]\n\t\tevents: [Event{\n\t\t\tvalue: 42\n\t\t}]\n\t}\n\tencoded := json2.encode(payload)\n\tprintln(json2.decode[Payload](encoded)!.events[0].value)\n}\n')
	assert out == '42'
}

fn test_json2_encode_array_keeps_main_type_with_imported_homonym() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_array_main_type_imported_homonym', 'import gg\nimport x.json2\n\nstruct Event {\n\tvalue int\n}\n\nfn main() {\n\t_ = json2.encode([gg.Event{}])\n\tencoded := json2.encode([Event{\n\t\tvalue: 42\n\t}])\n\tprintln(encoded)\n}\n')
	assert out.contains('"value":42')
}

fn test_json2_decode_keeps_main_type_with_imported_homonym() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_decode_main_type_imported_homonym', 'import gg\nimport x.json2\n\nstruct Event {\n\tvalue int\n}\n\nfn main() {\n\t_ = json2.decode[gg.Event]("{}") or { gg.Event{} }\n\tevent := json2.decode[Event](r\'{"value":42}\')!\n\tprintln(event.value)\n}\n')
	assert out == '42'
}

fn test_json2_encode_shared_struct_field_uses_locked_value_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_shared_struct_field_value', 'import x.json2\n\nstruct State {\n\tvalue int\n}\n\nstruct Client {\n\tstate shared State\n}\n\nfn main() {\n\tclient := Client{}\n\tprintln(json2.encode(client))\n}\n')
	assert out == '{"state":{"value":0}}'
}

fn test_json2_decode_any_map_keeps_sum_value_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_any_map_value', 'import x.json2\n\nfn main() {\n\tvalues := json2.decode[map[string]json2.Any](r\'{"ok":true}\')!\n\tprintln(values["ok"] or { json2.Any(false) })\n}\n')
	assert out == 'true'
}

fn test_json2_callback_field_keeps_declaring_module_and_pointer_depth() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'json2_callback_field_module', {
		'v.mod':         "Module { name: 'json2_callback_field_module' }\n"
		'model/model.v': 'module model\n\npub struct Event {}\n\npub type Callback = fn (voidptr, &Event)\n\npub struct Config {\npub:\n\tcallback Callback\n}\n'
		'main.v':        'module main\n\nimport model\nimport x.json2\n\nstruct Event {}\n\ntype Callback = fn (voidptr, &Event)\n\nstruct Config {\n\tcallback Callback\n}\n\nfn main() {\n\t_ := json2.decode[Config]("{}") or { Config{} }\n\tconfig := json2.decode[model.Config]("{}") or { model.Config{} }\n\tprintln(config.callback == unsafe { nil })\n}\n'
	}, 'main.v')
	assert out == 'true'
}

fn test_json2_explicit_generic_type_keeps_calling_module() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'json2_explicit_generic_calling_module', {
		'v.mod':             "Module { name: 'json2_explicit_generic_calling_module' }\n"
		'discord/discord.v': 'module discord\n\nimport x.json2\n\npub struct Packet {\npub:\n\tvalue int\n}\n\npub fn encode_packet(value int) string {\n\treturn json2.encode[Packet](Packet{\n\t\tvalue: value\n\t})\n}\n\npub fn decode_packet(src string) Packet {\n\treturn json2.decode[Packet](src)!\n}\n'
		'main.v':            'module main\n\nimport discord\n\nfn main() {\n\tsrc := discord.encode_packet(42)\n\tprintln(discord.decode_packet(src).value)\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_parallel_json2_specializations_emit_registered_bodies() {
	v3_bin := build_v3_review_transform()
	mut declarations := []string{cap: 40}
	mut decodes := []string{cap: 40}
	for i in 0 .. 40 {
		declarations << 'struct Payload${i} {\n\tvalue int\n}'
		decodes << '\tvalue${i} := json2.decode[Payload${i}](r\'{"value":${i}}\')!\n\t_ = json2.encode(value${i})'
	}
	src := 'import discord\nimport x.json2\n\n${declarations.join('\n\n')}\n\nstruct Discord {\n\tvalue int\n}\n\nstruct Chat {\n\tdiscord_apis []Discord\n}\n\nfn main() {\n${decodes.join('\n')}\n\t_ = json2.encode(["hello"])\n\t_ = json2.encode(discord.Discord{})\n\t_ = json2.decode[map[string]bool](r\'{"ok":true}\')!\n\tany_values := json2.decode[map[string]json2.Any](r\'{"ok":true}\')!\n\tassert (any_values["ok"] or { json2.Any(false) }).str() == "true"\n\tchat := Chat{\n\t\tdiscord_apis: [Discord{\n\t\t\tvalue: 42\n\t\t}]\n\t}\n\tencoded := json2.encode(chat)\n\tprintln(json2.decode[Chat](encoded)!.discord_apis[0].value)\n}\n'
	out := run_good_project(v3_bin, 'parallel_json2_registered_bodies', {
		'v.mod':             "Module { name: 'parallel_json2_registered_bodies' }\n"
		'discord/discord.v': 'module discord\n\npub struct Discord {\npub:\n\tname string\n}\n'
		'main.v':            src
	}, 'main.v')
	assert out == '42'
}

fn test_parallel_json2_exact_callee_does_not_rebind_main_type_to_imported_homonym() {
	v3_bin := build_v3_review_transform()
	c_source := gen_c_from_project(v3_bin, 'parallel_json2_exact_callee_homonym', {
		'v.mod':             "Module { name: 'parallel_json2_exact_callee_homonym' }\n"
		'discord/discord.v': 'module discord\n\npub struct Discord {\npub:\n\tname string\n}\n'
		'main.v':            'module main\n\nimport discord\nimport x.json2\n\nstruct Discord {\n\tvalue int\n}\n\nfn main() {\n\t_ = json2.encode(discord.Discord{})\n\tvalue := json2.decode[Discord](r\'{"value":42}\')!\n\tprintln(value.value)\n}\n'
	}, 'main.v')
	assert c_source.contains('decode_struct_key_T_Discord(')
	assert !c_source.contains('decode_struct_key_T_discord__Discord(')
}

fn test_module_local_const_array_struct_types_do_not_use_previous_module() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'module_local_const_array_struct_types', 'import encoding.utf8
import hash.crc32

fn main() {
	_ := crc32.sum([u8(1), 2, 3])
	println(utf8.is_number(`7`))
}
')
	assert out == 'true'
}

fn test_moved_module_alias_uses_target_module_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'moved_module_alias_identity', {
		'v.mod':                      "Module { name: 'moved_module_alias_identity' }\n"
		'modules/legacy/alias.v':     "@[alias: '@VMODROOT/modules/canonical'] module legacy\n"
		'modules/canonical/module.v': 'module canonical\n\npub fn answer() int {\n\treturn 42\n}\n'
		'main.v':                     'module main\n\nimport legacy\n\nfn main() {\n\tprintln(int_str(legacy.answer()))\n}\n'
	}, 'main.v')
	assert out == '42'
}

fn test_array_filter_and_map_reuse_capturing_callback_state() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_filter_map_capturing_callback_state', 'fn main() {
	mut filter_calls := 0
	filtered := [1, 2, 3].filter(fn [mut filter_calls] (value int) bool {
		filter_calls++
		return filter_calls > 1
	})
	mut map_calls := 0
	mapped := [10, 20, 30].map(fn [mut map_calls] (value int) int {
		map_calls++
		return value + map_calls
	})
	println(filtered)
	println(mapped)
}
')
	assert out == '[2, 3]\n[11, 22, 33]'
}

fn test_capturing_callback_variable_keeps_declared_parameters() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'capturing_callback_variable_parameters', 'fn report(callback fn (int, string)) {
	callback(42, "ready")
}

fn main() {
	prefix := "progress"
	callback := fn [prefix] (percent int, stage string) {
		println("\${prefix}:\${percent}:\${stage}")
	}
	report(callback)
}
')
	assert out == 'progress:42:ready'
}

fn test_array_filter_and_map_hoist_bound_method_callbacks() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Rule {
	min    int
	offset int
mut:
	calls int
}

fn (mut rule Rule) accept(value int) bool {
	rule.calls++
	return value >= rule.min
}

fn (mut rule Rule) shift(value int) int {
	rule.calls++
	return value + rule.offset
}

fn main() {
	mut total := 0
	for _ in 0 .. 20_000 {
		rule := Rule{
			min: 3
			offset: 10
		}
		filtered := [1, 2, 3, 4].filter(rule.accept)
		mapped := [1, 2, 3].map(rule.shift)
		assert filtered.len == 2
		assert filtered[0] == 3
		assert filtered[1] == 4
		assert mapped[0] == 11
		assert mapped[1] == 12
		assert mapped[2] == 13
		assert rule.calls == 7
		total += filtered[0] + filtered[1] + mapped[0] + mapped[1] + mapped[2]
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'array_bound_method_callbacks_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert main_body.count('closure__closure_create_with_data(') == 2, main_body
	assert main_body.contains('closure__closure_try_destroy(__filter_callback_'), main_body
	assert main_body.contains('closure__closure_try_destroy(__map_callback_'), main_body
	out := run_good(v3_bin, 'array_bound_method_callbacks', source)
	assert out == '860000'
}

fn test_array_filter_and_map_invoke_branch_selected_function_values() {
	v3_bin := build_v3_review_transform()
	source := 'fn is_even(value int) bool {
	return value % 2 == 0
}

fn is_odd(value int) bool {
	return value % 2 != 0
}

fn double(value int) int {
	return value * 2
}

fn increment(value int) int {
	return value + 1
}

fn main() {
	use_even := true
	filtered := [1, 2, 3, 4].filter(if use_even { is_even } else { is_odd })
	mode := 1
	mapped := [1, 2, 3].map(match mode {
		1 { double }
		else { increment }
	})
	assert filtered == [2, 4]
	assert mapped == [2, 4, 6]
	println(int_str(filtered.len + mapped[2]))
}
'
	out := run_good(v3_bin, 'array_branch_selected_function_values', source)
	assert out == '8'
}

fn test_array_map_drops_temporary_source_after_transient_element_address() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "four"
	}]
}

fn text_len(item &Item) int {
	return item.text.len
}

fn main() {
	lengths := make_items().map(text_len(&it))
	println(lengths)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_transient_address_drop_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array lengths = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_transient_address_drop', '-ownership', source)
	assert out == '[4]'
}

fn test_array_map_binds_implicit_reference_receiver_to_source_slot() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	value int
}

fn (item &Item) self() &Item {
	return item
}

fn make_items() []Item {
	return [Item{
		value: 1
	}, Item{
		value: 2
	}]
}

fn main() {
	items := make_items().map(it.self())
	println(items[0].value)
	println(items[1].value)
	println(items[0] == items[1])
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_implicit_reference_receiver_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert !main_body.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_implicit_reference_receiver', '-ownership', source)
	assert out == '1\n2\nfalse'
}

fn test_array_map_keeps_temporary_source_for_generic_pointer_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct Box[T] {
	value T
}

fn make_items() []Item {
	return [Item{
		text: "four"
	}]
}

fn main() {
	boxes := make_items().map(Box[&Item]{
		value: &it
	})
	println(boxes[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_generic_pointer_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert !main_body.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_generic_pointer_result', '-ownership', source)
	assert out == 'four'
}

fn test_array_map_keeps_temporary_source_for_generic_sum_pointer_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct Empty {}

type Maybe[T] = T | Empty

fn make_items() []Item {
	return [Item{
		text: "four"
	}]
}

fn main() {
	values := make_items().map(Maybe[&Item](&it))
	for value in values {
		if value is &Item {
			println(value.text)
		}
	}
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_generic_sum_pointer_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert !main_body.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_generic_sum_pointer_result', '-ownership', source)
	assert out == 'four'
}

fn test_array_map_keeps_temporary_source_through_index_and_selector_results() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "four"
	}]
}

fn main() {
	indexed := make_items().map([&it][0])
	selected := make_items().map(PointerBox{
		value: &it
	}.value)
	println(indexed[0].text)
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_wrapped_pointer_results_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert !main_body.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_wrapped_pointer_results', '-ownership', source)
	assert out == 'four\nfour'
}

fn test_array_map_keeps_temporary_source_through_dump_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "four"
	}]
}

fn main() {
	values := make_items().map(dump(&it))
	println(values[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_dump_pointer_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert !main_body.contains('array__free(&(__map_source_'), main_body
}

fn test_array_map_keeps_temporary_source_through_local_result_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	values := make_items().map(match flag {
		true {
			p := &it
			q := p
			q
		}
		else {
			&external
		}
	})
	println(values[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_local_pointer_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_local_pointer_alias', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_conditional_result_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	values := make_items().map(match flag {
		true {
			mut p := unsafe { &external }
			if flag {
				p = unsafe { &it }
			}
			p
		}
		else {
			&external
		}
	})
	println(values[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_conditional_pointer_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_conditional_pointer_alias', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_branch_local_result_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	values := make_items().map(match flag {
		true {
			mut p := unsafe { &external }
			if flag {
				q := unsafe { &it }
				p = q
			}
			p
		}
		else {
			&external
		}
	})
	println(values[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_branch_local_pointer_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_branch_local_pointer_alias', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_drops_temporary_source_for_shadowed_nested_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	other := Item{
		text: "other"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut p := unsafe { &external }
			if flag {
				mut p := unsafe { &other }
				p = unsafe { &it }
			}
			p
		}
		else {
			&external
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_shadowed_nested_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_shadowed_nested_alias', '-ownership', source)
	assert out == 'external'
}

fn test_array_map_keeps_temporary_source_through_local_aggregate_selector_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			box := PointerBox{
				value: unsafe { &it }
			}
			box.value
		}
		else {
			&external
		}
	})
	indexed := make_items().map(match flag {
		true {
			pointers := [unsafe { &it }]!
			pointers[0]
		}
		else {
			&external
		}
	})
	println(selected[0].text)
	println(indexed[0].text)
}
'
	selected_c := gen_c_from_source_with_flags(v3_bin, 'array_map_local_aggregate_selector_alias_c', '-ownership', source)
	main_body := c_fn_body(selected_c, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_local_aggregate_selector_alias', '-ownership', source)
	assert out == 'source\nsource'
}

fn test_array_map_keeps_temporary_source_through_nested_aggregate_selector_write() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			if flag {
				box = PointerBox{
					value: unsafe { &it }
				}
			}
			box.value
		}
		else {
			&external
		}
	})
	indexed := make_items().map(match flag {
		true {
			mut pointers := [unsafe { &external }]!
			if flag {
				pointers = [unsafe { &it }]!
			}
			pointers[0]
		}
		else {
			&external
		}
	})
	println(selected[0].text)
	println(indexed[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_selector_write_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_selector_write', '-ownership', source)
	assert out == 'source\nsource'
}

fn test_array_map_keeps_temporary_source_through_direct_selector_write() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.value = unsafe { &it }
			box.value
		}
		else {
			&external
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_direct_selector_write_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_direct_selector_write', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_direct_index_write() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut pointers := [unsafe { &external }]!
			pointers[0] = unsafe { &it }
			pointers[0]
		}
		else {
			&external
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_direct_index_write_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_direct_index_write', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_dynamic_index_write_across_origins() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	idx := 1
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut holder := [&local, &saved]
			holder[idx].value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_dyn_index_write_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_dyn_index_write', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_copies_projected_aggregate_pointer_origins() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

struct Holder {
	mut:
	box &PointerBox
}

struct Outer {
	holder Holder
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut selector_saved := PointerBox{
		value: unsafe { &external }
	}
	selector_result := make_items().map(match true {
		true {
			outer := Outer{
				holder: Holder{
					box: &selector_saved
				}
			}
			mut holder := outer.holder
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selector_result[0])
	println(selector_saved.value.text)

	mut index_saved := PointerBox{
		value: unsafe { &external }
	}
	index_result := make_items().map(match true {
		true {
			outers := [Outer{
				holder: Holder{
					box: &index_saved
				}
			}]
			mut holder := outers[0].holder
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(index_result[0])
	println(index_saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_projected_aggregate_origins_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_projected_aggregate_origins', '-ownership', source)
	assert out == '0\nsource\n0\nsource'
}

fn test_array_map_merges_pointer_origins_at_forward_goto_label() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &saved
			unsafe {
				goto store
			}
			alias = &local
			store:
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_forward_goto_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_forward_goto_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_for_helper_forward_goto_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn store_after_goto(mut box PointerBox, value &Item, replacement &Item) {
	mut local := PointerBox{
		value: unsafe { replacement }
	}
	mut alias := &box
	unsafe {
		goto store
	}
	alias = &local
	store:
	alias.value = unsafe { value }
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			store_after_goto(mut saved, unsafe { &it }, unsafe { &external })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_helper_goto_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_helper_goto_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_through_nested_local_selector_chain() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	value &Item
}

struct Outer {
	inner PointerBox
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			outer := Outer{
				inner: PointerBox{
					value: unsafe { &it }
				}
			}
			outer.inner.value
		}
		else {
			&external
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_local_selector_chain_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_local_selector_chain', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_nested_selector_write() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

struct Outer {
	mut:
	inner PointerBox
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut outer := Outer{
				inner: PointerBox{
					value: unsafe { &external }
				}
			}
			outer.inner.value = unsafe { &it }
			outer.inner.value
		}
		else {
			&external
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_selector_write_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_selector_write', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_mutating_method_write() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) set(value &Item) {
	box.value = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.set(unsafe { &it })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutating_method_write_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutating_method_write', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_mutating_function_write() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

fn set(mut box PointerBox, value &Item) {
	box.value = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			set(mut box, unsafe { &it })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutating_function_write_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutating_function_write', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_mutating_method_selector_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) set(value &Item) {
	box.value = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.set(unsafe { &it })
			box.value
		}
		else {
			unsafe { &external }
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutating_method_selector_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutating_method_selector_result', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_mutating_function_index_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn set(mut values [1]&Item, value &Item) {
	values[0] = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut values := [1]&Item{}
			values[0] = unsafe { &external }
			set(mut values, unsafe { &it })
			values[0]
		}
		else {
			unsafe { &external }
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutating_function_index_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutating_function_index_result', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_nested_mutating_call() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

fn store(mut box PointerBox, value &Item) {
	box.value = value
}

fn (mut box PointerBox) set(value &Item) {
	store(mut box, value)
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.set(unsafe { &it })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_mutating_call_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_mutating_call', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_mutator_local_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) set(value &Item) {
	pointer := unsafe { value }
	box.value = pointer
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.set(unsafe { &it })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_local_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutator_local_alias', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_drops_temporary_source_after_mutator_local_alias_overwrite() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) inspect(value &Item) {
	mut pointer := unsafe { value }
	pointer = box.value
	box.value = pointer
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.inspect(unsafe { &it })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_alias_overwrite_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutator_alias_overwrite', '-ownership', source)
	assert out == 'external'
}

fn test_array_map_keeps_temporary_source_when_external_mutator_retains_element() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) set(value &Item) {
	box.value = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			saved.set(unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_mutator_escape_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_mutator_escape', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_when_external_array_append_retains_element() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	mut saved := []&Item{}
	selected := make_items().map(match true {
		true {
			saved << unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_append_escape_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_append_escape', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_through_external_pointer_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut alias := &saved
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_pointer_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_pointer_alias', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_through_external_pointer_in_local_aggregate() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

struct Holder {
	mut:
	box &PointerBox
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := Holder{
				box: unsafe { &saved }
			}
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_pointer_in_local_aggregate_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_pointer_in_local_aggregate', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_through_conditional_local_aggregate_initializers() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

struct Holder {
mut:
	box &PointerBox
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	mut saved_if := PointerBox{
		value: unsafe { &external }
	}
	selected_if := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut holder := if flag {
				Holder{
					box: unsafe { &saved_if }
				}
			} else {
				Holder{
					box: unsafe { &local }
				}
			}
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	mut saved_match := PointerBox{
		value: unsafe { &external }
	}
	selected_match := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut holder := match flag {
				true {
					Holder{
						box: unsafe { &saved_match }
					}
				}
				else {
					Holder{
						box: unsafe { &local }
					}
				}
			}
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected_if[0])
	println(saved_if.value.text)
	println(selected_match[0])
	println(saved_match.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_conditional_local_aggregate_initializers_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_conditional_local_aggregate_initializers', '-ownership', source)
	assert out == '0\nsource\n0\nsource'
}

fn test_array_map_tracks_pointer_origins_through_struct_updates() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

struct Holder {
mut:
	box   &PointerBox
	other int
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved_inherited := PointerBox{
		value: unsafe { &external }
	}
	selected_inherited := make_items().map(match true {
		true {
			base := Holder{
				box: unsafe { &saved_inherited }
			}
			mut holder := Holder{
				...base
				other: 1
			}
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	mut saved_overridden := PointerBox{
		value: unsafe { &external }
	}
	selected_overridden := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			base := Holder{
				box: unsafe { &saved_overridden }
			}
			mut holder := Holder{
				...base
				box: unsafe { &local }
			}
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected_inherited[0])
	println(saved_inherited.value.text)
	println(selected_overridden[0])
	println(saved_overridden.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_struct_update_pointer_origins_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert compact_main.count('array__free(&(__map_source_') == 1, main_body
	out := run_good_with_flags(v3_bin, 'array_map_struct_update_pointer_origins', '-ownership', source)
	assert out == '0\nsource\n0\nexternal'
}

fn test_array_map_drops_temporary_source_for_local_conditional_pointer_initializers() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected_if := make_items().map(match true {
		true {
			mut local_a := PointerBox{
				value: unsafe { &external }
			}
			mut local_b := PointerBox{
				value: unsafe { &external }
			}
			mut alias := if flag { unsafe { &local_a } } else { unsafe { &local_b } }
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	selected_match := make_items().map(match true {
		true {
			mut local_a := PointerBox{
				value: unsafe { &external }
			}
			mut local_b := PointerBox{
				value: unsafe { &external }
			}
			mut alias := match flag {
				true { unsafe { &local_a } }
				else { unsafe { &local_b } }
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected_if[0])
	println(selected_match[0])
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_local_conditional_pointer_initializers_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	if_result_pos := compact_main.index('Arrayselected_if=') or { -1 }
	match_result_pos := compact_main.index('Arrayselected_match=') or { -1 }
	if_source_drop_pos := compact_main.index('array__free(&(__map_source_') or { -1 }
	match_drop_offset := compact_main[if_result_pos..].index('array__free(&(__map_source_') or {
		-1
	}
	match_source_drop_pos := if match_drop_offset >= 0 {
		if_result_pos + match_drop_offset
	} else {
		-1
	}
	assert if_source_drop_pos >= 0 && if_source_drop_pos < if_result_pos, main_body
	assert match_source_drop_pos > if_result_pos && match_source_drop_pos < match_result_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_local_conditional_pointer_initializers', '-ownership', source)
	assert out == '0\n0'
}

fn test_array_map_keeps_temporary_source_through_external_pointer_in_call_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

struct Holder {
	mut:
	primary   &PointerBox
	secondary &PointerBox
}

fn make_holder(box &PointerBox) Holder {
	return Holder{
		primary: box
		secondary: box
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := make_holder(&saved)
			holder.secondary.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_pointer_in_call_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_pointer_in_call_result', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_tracks_call_result_pointer_origins_per_field() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

struct Holder {
	mut:
	primary   &PointerBox
	secondary &PointerBox
}

fn make_holder(primary &PointerBox, secondary &PointerBox) Holder {
	return Holder{
		primary: primary
		secondary: secondary
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut holder := make_holder(&saved, &local)
			holder.secondary.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_call_result_per_field_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_call_result_per_field_origin', '-ownership', source)
	assert out == '0\nexternal'
}

fn test_array_map_keeps_temporary_source_through_global_pointer_call_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := '@[has_globals]
module main

struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

struct Holder {
mut:
	box &PointerBox
}

__global saved &PointerBox

fn get_global_holder() Holder {
	return Holder{
		box: saved
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	saved = &PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := get_global_holder()
			holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_global_pointer_call_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_global_pointer_call_result', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_updates_pointer_origins_through_mut_call() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) {
	target = replacement
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			replace(mut alias, &saved)
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mut_call_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mut_call_pointer_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_updates_pointer_origins_through_nested_calls_and_dereferences() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn consume(changed bool) {
	assert changed
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut nested_saved := PointerBox{
		value: unsafe { &external }
	}
	nested := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			consume(replace(mut alias, &nested_saved))
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	mut dereferenced_saved := PointerBox{
		value: unsafe { &external }
	}
	dereferenced := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			{
				mut slot := &alias
				unsafe {
					*slot = &dereferenced_saved
				}
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	mut direct_saved := unsafe { &external }
	direct := make_items().map(match true {
		true {
			mut slot := &direct_saved
			unsafe {
				*slot = &it
			}
			0
		}
		else {
			0
		}
	})
	println(nested[0])
	println(nested_saved.value.text)
	println(dereferenced[0])
	println(direct[0])
	println(direct_saved.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_call_and_dereference_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_call_and_dereference_pointer_origin', '-ownership', source)
	assert out == '0\nsource\n0\n0\nsource'
}

fn test_array_map_snapshots_call_arguments_and_respects_callback_alias_scopes() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn reset(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn assign(mut target &PointerBox, source &PointerBox, changed bool) {
	if changed {
		target = source
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &saved
			mut target := &local
			assign(mut target, alias, reset(mut alias, &local))
			target.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_call_argument_snapshot_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_call_argument_snapshot', '-ownership', source)
	assert out == '0\nsource'

	files := {
		'v.mod':        "Module { name: 'array_map_callback_alias_scope' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

pub fn invoke(callback fn (&Item), value &Item) {
	alias := value
	unsafe {
		local := Item{
			text: "local"
		}
		alias := &local
		_ = alias
	}
	callback(alias)
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.invoke(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	callback_c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_callback_alias_scope_c', '-ownership', files, 'main.v')
	callback_main_body := c_fn_body(callback_c_source, 'int main(int argc, char** argv) {')
	compact_callback_main := callback_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_callback_main.contains('array__free(&(__map_source_'), callback_main_body
	callback_out := run_good_project_with_flags(v3_bin, 'array_map_callback_alias_scope', '-ownership', files, 'main.v')
	assert callback_out == '0\nsource'
}

fn test_array_map_classifies_mut_targets_after_earlier_argument_effects() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn rebind_and_return(mut target &PointerBox, replacement &PointerBox, value &Item) &Item {
	target = replacement
	return value
}

fn store(value &Item, mut target &PointerBox) {
	target.value = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	result := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			store(rebind_and_return(mut alias, &saved, unsafe { &it }), mut alias)
			0
		}
		else {
			0
		}
	})
	println(result[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_earlier_argument_target_effect_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_earlier_argument_target_effect', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_classifies_later_expression_children_after_origin_effects() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn rebind(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn store(mut target &PointerBox, value &Item) bool {
	target.value = value
	return true
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	result := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			_ := rebind(mut alias, &saved) == store(mut alias, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(result[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_expression_child_origin_effect_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_expression_child_origin_effect', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_updates_pointer_origins_through_declaration_initializer_call() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			changed := replace(mut alias, &saved)
			assert changed
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_decl_initializer_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_decl_initializer_pointer_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_updates_pointer_origins_through_assignment_rhs_call() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			mut changed := false
			changed = replace(mut alias, &saved)
			assert changed
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_assignment_rhs_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_assignment_rhs_pointer_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_snapshots_pointer_origins_before_multi_assignment() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &saved
			mut other := &local
			alias, other = other, alias
			other.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_multi_assign_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_multi_assign_pointer_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_applies_all_rhs_effects_before_multi_assignment_targets() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn rebind(mut target &PointerBox, replacement &PointerBox) int {
	target = replacement
	return 0
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			mut ignored := 0
			ignored, alias.value = rebind(mut alias, &saved), unsafe { &it }
			ignored
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_multi_assign_rhs_effects_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_multi_assign_rhs_effects', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_applies_condition_pointer_alias_updates_before_branches() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			if replace(mut alias, &saved) {
				alias.value = unsafe { &it }
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_condition_pointer_alias_update_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_condition_pointer_alias_update', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_applies_nested_condition_pointer_alias_updates_before_branches() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	flag := true
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			if flag && replace(mut alias, &saved) {
				alias.value = unsafe { &it }
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_condition_pointer_alias_update_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_condition_pointer_alias_update', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_applies_infix_operand_pointer_alias_updates_before_branches() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			if replace(mut alias, &saved) == true {
				alias.value = unsafe { &it }
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_infix_operand_pointer_alias_update_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_infix_operand_pointer_alias_update', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_applies_match_subject_pointer_alias_updates_before_branches() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn replace(mut target &PointerBox, replacement &PointerBox) bool {
	target = replacement
	return true
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			match replace(mut alias, &saved) {
				true { alias.value = unsafe { &it } }
				else {}
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_match_subject_pointer_alias_update_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_match_subject_pointer_alias_update', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_passed_to_globally_storing_call() {
	v3_bin := build_v3_review_transform_ownership()
	source := '@[has_globals]
module main

struct Item {
	text string
}

__global retained &Item

fn save(value &Item) {
	retained = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			save(unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(retained.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_global_call_sink_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_global_call_sink', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_passed_through_global_store_wrapper() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':         "Module { name: 'array_map_global_wrapper_sink' }\n"
		'api/store.v':   '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/wrapper.v': 'module api

pub fn keep(value &Item) {
	save(value)
}
'
		'main.v':        'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.keep(unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_global_wrapper_sink_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_global_wrapper_sink', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_captured_by_callback_wrapper() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':        "Module { name: 'array_map_callback_wrapper_sink' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

pub fn invoke(callback fn ()) {
	callback()
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			p := unsafe { &it }
			api.invoke(fn [p] () {
				api.save(p)
			})
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_callback_wrapper_sink_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_callback_wrapper_sink', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_forwarded_through_callback_wrapper() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':        "Module { name: 'array_map_forwarded_callback_wrapper_sink' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

pub fn invoke(callback fn (&Item), value &Item) {
	callback(value)
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.invoke(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_forwarded_callback_wrapper_sink_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_forwarded_callback_wrapper_sink', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_forwarded_through_callback_wrapper_local_alias() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':        "Module { name: 'array_map_aliased_callback_wrapper_sink' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

pub fn invoke(callback fn (&Item), value &Item) {
	callback_alias := callback
	value_alias := value
	callback_alias(value_alias)
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.invoke(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_aliased_callback_wrapper_sink_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_aliased_callback_wrapper_sink', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_forwarded_through_callback_wrapper_helper_rebind() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':        "Module { name: 'array_map_helper_rebound_callback_alias' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

fn rebind(mut target &Item, value &Item) {
	target = value
}

pub fn invoke(callback fn (&Item), value &Item) {
	mut local := Item{
		text: "local"
	}
	mut alias := &local
	rebind(mut alias, value)
	callback(alias)
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.invoke(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_helper_rebound_callback_alias_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_helper_rebound_callback_alias', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_stored_through_nested_helper_rebind() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

@[heap]
struct PointerBox {
mut:
	value &Item
}

fn rebind(mut alias &PointerBox, target &PointerBox) {
	alias = target
}

fn store(mut target &PointerBox, value &Item) {
	target.value = value
}

fn forward(mut target PointerBox, value &Item) {
	local_item := Item{
		text: "local"
	}
	mut local := PointerBox{
		value: unsafe { &local_item }
	}
	mut alias := &local
	rebind(mut alias, &target)
	store(mut alias, value)
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	result := make_items().map(match true {
		true {
			forward(mut saved, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(result[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_helper_rebind_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_helper_rebind', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_stored_through_explicitly_dereferenced_helper_target() {
	v3_bin := build_v3_review_transform_ownership()
	source := '@[has_globals]
module main

struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn store(mut target &PointerBox, value &Item) {
	unsafe {
		(*target).value = value
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	mut local := Item{
		text: "local"
	}
	mut target := &PointerBox{
		value: &local
	}
	selected := make_items().map(match true {
		true {
			store(mut target, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(target.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_explicit_deref_helper_storage_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_explicit_deref_helper_storage', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_conditional_callback_source_aliases() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':        "Module { name: 'array_map_conditional_callback_alias' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

pub fn invoke(callback fn (&Item), value &Item, use_local bool) {
	mut local := Item{
		text: "local"
	}
	mut alias := value
	if use_local {
		alias = &local
	}
	callback(alias)
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.invoke(api.save, unsafe { &it }, false)
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_conditional_callback_alias_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_conditional_callback_alias', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_forwarded_through_branch_selected_callback_alias() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':        "Module { name: 'array_map_branch_callback_alias' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

fn ignore(_ &Item) {}

fn maybe_callback() ?fn (&Item) {
	return none
}

pub fn invoke(callback fn (&Item), value &Item, use_callback bool) {
	selected := if use_callback { callback } else { ignore }
	chosen := match use_callback {
		true { selected }
		else { ignore }
	}
	chosen(value)
}

pub fn invoke_or(callback fn (&Item), value &Item) {
	chosen := maybe_callback() or { callback }
	chosen(value)
}

pub fn invoke_goto(callback fn (&Item), value &Item) {
	mut alias := callback
	unsafe {
		goto invoke
	}
	alias = ignore
	invoke:
	alias(value)
}

pub fn invoke_container(callback fn (&Item), value &Item) {
	callbacks := [callback]
	callbacks[0](value)
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.invoke(api.save, unsafe { &it }, true)
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
	or_selected := make_items().map(match true {
		true {
			api.invoke_or(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(or_selected[0])
	println(api.retained_text())
	goto_selected := make_items().map(match true {
		true {
			api.invoke_goto(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(goto_selected[0])
	println(api.retained_text())
	container_selected := make_items().map(match true {
		true {
			api.invoke_container(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(container_selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_branch_callback_alias_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_branch_callback_alias', '-ownership', files, 'main.v')
	assert out == '0\nsource\n0\nsource\n0\nsource\n0\nsource'
}

fn test_array_map_keeps_source_forwarded_through_nested_callback_wrappers() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'v.mod':        "Module { name: 'array_map_nested_callback_wrapper_sink' }\n"
		'api/store.v':  '@[has_globals]
module api

pub struct Item {
pub:
	text string
}

__global retained &Item

pub fn save(value &Item) {
	retained = value
}

pub fn retained_text() string {
	return retained.text
}
'
		'api/invoke.v': 'module api

pub fn invoke1(callback fn (&Item), value &Item) {
	callback(value)
}

pub fn invoke2(callback fn (&Item), value &Item) {
	invoke1(callback, value)
}
'
		'main.v':       'module main

import api

fn make_items() []api.Item {
	return [api.Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			api.invoke2(api.save, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(api.retained_text())
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_nested_callback_wrapper_sink_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_nested_callback_wrapper_sink', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_passed_to_unresolved_pointer_call() {
	v3_bin := build_v3_review_transform_ownership()
	source := '@[has_globals]
module main

struct Item {
	text string
}

__global retained &Item

fn save(value &Item) {
	retained = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn map_with(callback fn (&Item)) []int {
	return make_items().map(match true {
		true {
			callback(unsafe { &it })
			0
		}
		else {
			0
		}
	})
}

fn main() {
	selected := map_with(save)
	println(selected[0])
	println(retained.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_indirect_call_sink_c', '-ownership', source)
	map_body := c_fn_body(c_source, 'Array main__map_with(')
	compact_map := map_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_map.contains('array__free(&(__map_source_'), map_body
	out := run_good_with_flags(v3_bin, 'array_map_indirect_call_sink', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_passed_to_external_c_call() {
	v3_bin := build_v3_review_transform_ownership()
	files := {
		'retainer.c': 'static void* retained_item;

static void retain_item(void* value) {
	retained_item = value;
}

static void* get_retained_item(void) {
	return retained_item;
}
'
		'main.v':     '#insert "retainer.c"

fn C.retain_item(voidptr)
fn C.get_retained_item() voidptr

struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	selected := make_items().map(match true {
		true {
			C.retain_item(unsafe { &it })
			0
		}
		else {
			0
		}
	})
	retained := unsafe { &Item(C.get_retained_item()) }
	println(selected[0])
	println(retained.text)
}
'
	}
	c_source := gen_c_from_project_with_flags(v3_bin, 'array_map_external_c_call_sink_c', '-ownership', files, 'main.v')
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_project_with_flags(v3_bin, 'array_map_external_c_call_sink', '-ownership', files, 'main.v')
	assert out == '0\nsource'
}

fn test_array_map_applies_deferred_writes_with_exit_pointer_origins() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	saved := &PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			defer {
				alias.value = unsafe { &it }
			}
			alias = saved
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_deferred_exit_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_deferred_exit_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_return_ignores_defers_registered_after_exit() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn map_or_return(mut saved PointerBox, early bool) []int {
	return make_items().map(match true {
		true {
			match early {
				true { return [7] }
				false { return [8] }
			}
			defer {
				saved.value = unsafe { &it }
			}
			0
		}
		else {
			0
		}
	})
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	result := map_or_return(mut saved, true)
	println(result[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_return_before_late_defer_c', '-ownership', source)
	map_body := c_fn_body(c_source, 'Array map_or_return(main__PointerBox* saved, bool early) {')
	compact_map := map_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert compact_map.contains('array__free(&(__map_source_'), map_body
	out := run_good_with_flags(v3_bin, 'array_map_return_before_late_defer', '-ownership', source)
	assert out == '7\nexternal'
}

fn test_array_map_loop_exit_ignores_defers_registered_after_exit() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := unsafe { &saved }
			for _ in 0 .. 1 {
				if it.text.len > 0 {
					break
				}
				defer {
					alias.value = unsafe { &it }
				}
				alias = unsafe { &local }
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	for exit_kind, exit_source in {
		'break':    source
		'continue': source.replace('\t\t\t\t\tbreak\n', '\t\t\t\t\tcontinue\n')
	} {
		c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_${exit_kind}_before_late_defer_c', '-ownership', exit_source)
		main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
		compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
		assert compact_main.contains('array__free(&(__map_source_'), main_body
		out := run_good_with_flags(v3_bin, 'array_map_${exit_kind}_before_late_defer', '-ownership', exit_source)
		assert out == '0\nexternal'
	}
}

fn test_array_map_keeps_source_when_returning_element_address() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn first_item() &Item {
	_ := make_items().map(match true {
		true {
			return unsafe { &it }
		}
		else {
			0
		}
	})
	return unsafe { nil }
}

fn main() {
	item := first_item()
	println(item.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_return_element_address_c', '-ownership', source)
	first_body := c_fn_body(c_source, 'main__Item* main__first_item(void) {')
	compact_first := first_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_first.contains('array__free(&(__map_source_'), first_body
	out := run_good_with_flags(v3_bin, 'array_map_return_element_address', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_external_pointer_in_local_map() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := {
				"box": unsafe { &saved }
			}
			holder["box"].value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_pointer_in_local_map_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_pointer_in_local_map', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_through_dynamic_local_array_index() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := [1]&PointerBox{}
			holder[0] = unsafe { &saved }
			index := 0
			holder[index].value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_dynamic_local_array_index_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_dynamic_local_array_index', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_through_delimiter_map_key() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := {
				"x]": unsafe { &saved }
			}
			key := "x]"
			holder[key].value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_delimiter_map_key_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_delimiter_map_key', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_dynamic_assignment_replaces_exact_pointer_origin() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut local := PointerBox{
		value: unsafe { &external }
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := [1]&PointerBox{}
			holder[0] = unsafe { &local }
			index := 0
			holder[index] = unsafe { &saved }
			holder[0].value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_dynamic_assignment_replaces_exact_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_dynamic_assignment_replaces_exact_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_through_repeated_local_array_initializer() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut holder := []&PointerBox{len: 1, init: unsafe { &saved }}
			holder[0].value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_repeated_local_array_initializer_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_repeated_local_array_initializer', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_merges_local_pointer_origins_from_select_branches() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	signal := chan bool{cap: 1}
	signal <- true
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := unsafe { &local }
			select {
				<-signal {
					alias = unsafe { &saved }
				}
				else {}
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_select_pointer_origins_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_select_pointer_origins', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_tracks_local_pointer_rebind_inside_select_branch() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	signal := chan bool{cap: 1}
	signal <- true
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := unsafe { &local }
			select {
				<-signal {
					alias = unsafe { &saved }
					alias.value = unsafe { &it }
				}
				else {}
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_select_in_arm_pointer_rebind_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_select_in_arm_pointer_rebind', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_merges_local_pointer_origins_from_loop_break_paths() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := unsafe { &local }
			for {
				alias = unsafe { &saved }
				if true {
					break
				}
				alias = unsafe { &local }
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_loop_break_pointer_origins_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_loop_break_pointer_origins', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_propagates_labeled_pointer_origin_exit_to_outer_loop() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut local := PointerBox{
		value: unsafe { &external }
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut alias := unsafe { &local }
			outer: for {
				for {
					alias = unsafe { &saved }
					break outer
				}
				alias = unsafe { &local }
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_labeled_outer_loop_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_labeled_outer_loop_pointer_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_drops_source_for_unrelated_local_aggregate_pointer_field() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

struct Holder {
	mut:
	external &PointerBox
	local    &PointerBox
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut holder := Holder{
				external: unsafe { &saved }
				local: unsafe { &local }
			}
			holder.local.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_unrelated_local_aggregate_pointer_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_unrelated_local_aggregate_pointer', '-ownership', source)
	assert out == '0'
}

fn test_array_map_keeps_temporary_source_through_conditional_external_pointer_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	flag := true
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			if flag {
				alias = &saved
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_conditional_external_pointer_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_conditional_external_pointer_alias', '-ownership', source)
	assert out == '0\nsource'

	match_source := source.replace('if flag {\n\t\t\t\talias = &saved\n\t\t\t}', 'match flag {\n\t\t\t\ttrue { alias = &saved }\n\t\t\t\telse {}\n\t\t\t}')
	assert match_source != source
	match_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_match_external_pointer_alias_c', '-ownership', match_source)
	match_main_body := c_fn_body(match_c_source, 'int main(int argc, char** argv) {')
	compact_match_main := match_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_match_main.contains('array__free(&(__map_source_'), match_main_body
	match_out := run_good_with_flags(v3_bin, 'array_map_match_external_pointer_alias', '-ownership', match_source)
	assert match_out == '0\nsource'

	loop_source := source.replace('if flag {\n\t\t\t\talias = &saved\n\t\t\t}', 'for flag {\n\t\t\t\talias = &saved\n\t\t\t\tbreak\n\t\t\t}')
	assert loop_source != source
	loop_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_loop_external_pointer_alias_c', '-ownership', loop_source)
	loop_main_body := c_fn_body(loop_c_source, 'int main(int argc, char** argv) {')
	compact_loop_main := loop_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_loop_main.contains('array__free(&(__map_source_'), loop_main_body
	loop_out := run_good_with_flags(v3_bin, 'array_map_loop_external_pointer_alias', '-ownership', loop_source)
	assert loop_out == '0\nsource'

	or_source := source.replace('fn make_items() []Item {', "fn may_fail() ! {
	return error('failed')
}

fn make_items() []Item {").replace('if flag {\n\t\t\t\talias = &saved\n\t\t\t}', 'may_fail() or {\n\t\t\t\talias = &saved\n\t\t\t}')
	assert or_source != source
	or_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_or_external_pointer_alias_c', '-ownership', or_source)
	or_main_body := c_fn_body(or_c_source, 'int main(int argc, char** argv) {')
	compact_or_main := or_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_or_main.contains('array__free(&(__map_source_'), or_main_body
	or_out := run_good_with_flags(v3_bin, 'array_map_or_external_pointer_alias', '-ownership', or_source)
	assert or_out == '0\nsource'
}

fn test_array_map_tracks_pointer_origin_through_selected_comptime_branch() {
	v3_bin := build_v3_review_transform_ownership()
	source_true := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			$if true {
				alias = &saved
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source_true := gen_c_from_source_with_flags(v3_bin, 'array_map_comptime_external_pointer_alias_true_c', '-ownership', source_true)
	main_body_true := c_fn_body(c_source_true, 'int main(int argc, char** argv) {')
	compact_main_true := main_body_true.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main_true.contains('array__free(&(__map_source_'), main_body_true
	out_true := run_good_with_flags(v3_bin, 'array_map_comptime_external_pointer_alias_true', '-ownership', source_true)
	assert out_true == '0\nsource'

	source_false := source_true.replace('\$if true {', '\$if false {')
	c_source_false := gen_c_from_source_with_flags(v3_bin, 'array_map_comptime_external_pointer_alias_false_c', '-ownership', source_false)
	main_body_false := c_fn_body(c_source_false, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body_false.index('array__free(&(') or { -1 }
	result_move_pos := main_body_false.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body_false
	out_false := run_good_with_flags(v3_bin, 'array_map_comptime_external_pointer_alias_false', '-ownership', source_false)
	assert out_false == '0\nexternal'
}

fn test_array_map_keeps_temporary_source_for_indirect_mutator() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

type Setter = fn (mut PointerBox, &Item)

fn store(mut box PointerBox, value &Item) {
	box.value = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn apply(setter Setter) {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			setter(mut saved, unsafe { &it })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}

fn main() {
	apply(store)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_indirect_mutator_c', '-ownership', source)
	apply_body := c_fn_body(c_source, 'void main__apply(main__Setter setter) {')
	compact_apply := apply_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_apply.contains('array__free(&(__map_source_'), apply_body
	out := run_good_with_flags(v3_bin, 'array_map_indirect_mutator', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_for_pointer_sent_to_channel() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	saved := chan &Item{cap: 1}
	selected := make_items().map(match true {
		true {
			saved <- unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	retained := <-saved
	println(retained.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_channel_pointer_sink_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_channel_pointer_sink', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_for_pointer_sent_to_local_channel_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	saved := chan &Item{cap: 1}
	selected := make_items().map(match true {
		true {
			alias := saved
			alias <- unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	retained := <-saved
	println(retained.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_local_channel_alias_sink_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_local_channel_alias_sink', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_for_pointer_passed_to_spawn() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

fn hold(item &Item, gate chan bool, saved chan string) {
	if <-gate {
		saved <- item.text
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	gate := chan bool{cap: 1}
	saved := chan string{cap: 1}
	selected := make_items().map(match true {
		true {
			spawn hold(unsafe { &it }, gate, saved)
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	gate <- true
	println(<-saved)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_spawn_pointer_sink_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_spawn_pointer_sink', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_temporary_source_for_spawned_pointer_receiver() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

fn (item &Item) hold(gate chan bool, saved chan string) {
	if <-gate {
		saved <- item.text
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	gate := chan bool{cap: 1}
	saved := chan string{cap: 1}
	selected := make_items().map(match true {
		true {
			pointer := unsafe { &it }
			spawn pointer.hold(gate, saved)
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	gate <- true
	println(<-saved)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_spawn_pointer_receiver_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_spawn_pointer_receiver', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_drops_temporary_source_after_external_pointer_alias_overwrite() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &saved
			alias = &local
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_pointer_alias_overwrite_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_pointer_alias_overwrite', '-ownership', source)
	assert out == '0\nexternal'
}

fn test_array_map_drops_temporary_source_after_mutator_storage_overwrite() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) set_then_reset(value &Item, replacement &Item) {
	box.value = value
	box.value = replacement
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map(match true {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.set_then_reset(unsafe { &it }, unsafe { &external })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_overwrite_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_overwrite', '-ownership', source)
	assert out == 'external'

	early_return_source := source.replace('box.value = value\n\tbox.value = replacement', 'box.value = value\n\tif true {\n\t\treturn\n\t}\n\tbox.value = replacement')
	assert early_return_source != source
	early_return_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_early_return_c', '-ownership', early_return_source)
	early_return_main_body := c_fn_body(early_return_c_source, 'int main(int argc, char** argv) {')
	compact_early_return_main :=
		early_return_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_early_return_main.contains('array__free(&(__map_source_'), early_return_main_body

	early_return_out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_early_return', '-ownership', early_return_source)
	assert early_return_out == 'source'

	delegated_source := source.replace('fn (mut box PointerBox) set_then_reset(value &Item, replacement &Item) {\n\tbox.value = value', 'fn (mut box PointerBox) store(value &Item) {\n\tbox.value = value\n}\n\nfn (mut box PointerBox) set_then_reset(value &Item, replacement &Item) {\n\tbox.store(value)')
	assert delegated_source != source
	delegated_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_delegated_mutator_storage_overwrite_c', '-ownership', delegated_source)
	delegated_main_body := c_fn_body(delegated_c_source, 'int main(int argc, char** argv) {')
	delegated_source_drop_pos := delegated_main_body.index('array__free(&(') or { -1 }
	delegated_result_move_pos := delegated_main_body.index('Array selected = ') or { -1 }
	assert delegated_source_drop_pos >= 0 && delegated_source_drop_pos < delegated_result_move_pos, delegated_main_body

	delegated_out := run_good_with_flags(v3_bin, 'array_map_delegated_mutator_storage_overwrite', '-ownership', delegated_source)
	assert delegated_out == 'external'

	reverse_delegated_source := source.replace('fn (mut box PointerBox) set_then_reset(value &Item, replacement &Item) {\n\tbox.value = value\n\tbox.value = replacement', 'fn store(mut box PointerBox, value &Item) {\n\tbox.value = value\n}\n\nfn (mut box PointerBox) set_then_reset(value &Item, replacement &Item) {\n\tbox.value = value\n\tstore(mut box, replacement)')
	assert reverse_delegated_source != source
	reverse_delegated_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_reverse_delegated_mutator_storage_overwrite_c', '-ownership', reverse_delegated_source)
	reverse_delegated_main_body := c_fn_body(reverse_delegated_c_source, 'int main(int argc, char** argv) {')
	reverse_delegated_source_drop_pos := reverse_delegated_main_body.index('array__free(&(') or {
		-1
	}
	reverse_delegated_result_move_pos := reverse_delegated_main_body.index('Array selected = ') or {
		-1
	}
	assert reverse_delegated_source_drop_pos >= 0
		&& reverse_delegated_source_drop_pos < reverse_delegated_result_move_pos, reverse_delegated_main_body

	reverse_delegated_out := run_good_with_flags(v3_bin, 'array_map_reverse_delegated_mutator_storage_overwrite', '-ownership', reverse_delegated_source)
	assert reverse_delegated_out == 'external'

	loop_source := source.replace('box.value = value\n\tbox.value = replacement', 'for {\n\t\tbox.value = value\n\t\tif true {\n\t\t\tbreak\n\t\t}\n\t\tbox.value = replacement\n\t\tbreak\n\t}')
	assert loop_source != source
	loop_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_loop_break_c', '-ownership', loop_source)
	loop_main_body := c_fn_body(loop_c_source, 'int main(int argc, char** argv) {')
	compact_loop_main := loop_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_loop_main.contains('array__free(&(__map_source_'), loop_main_body
	loop_out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_loop_break', '-ownership', loop_source)
	assert loop_out == 'source'

	deferred_source := source.replace('box.value = value\n\tbox.value = replacement', 'defer {\n\t\tbox.value = value\n\t}\n\tbox.value = replacement')
	assert deferred_source != source
	deferred_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_deferred_c', '-ownership', deferred_source)
	deferred_main_body := c_fn_body(deferred_c_source, 'int main(int argc, char** argv) {')
	compact_deferred_main := deferred_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_deferred_main.contains('array__free(&(__map_source_'), deferred_main_body
	deferred_out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_deferred', '-ownership', deferred_source)
	assert deferred_out == 'source'

	deferred_exit_alias_source := source.replace('box.value = value\n\tbox.value = replacement', 'mut alias := unsafe { value }\n\tdefer {\n\t\tbox.value = alias\n\t}\n\tif true {\n\t\treturn\n\t}\n\talias = unsafe { replacement }')
	assert deferred_exit_alias_source != source
	deferred_exit_alias_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_deferred_exit_alias_c', '-ownership', deferred_exit_alias_source)
	deferred_exit_alias_main := c_fn_body(deferred_exit_alias_c_source, 'int main(int argc, char** argv) {')
	compact_deferred_exit_alias_main :=
		deferred_exit_alias_main.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_deferred_exit_alias_main.contains('array__free(&(__map_source_'), deferred_exit_alias_main
	deferred_exit_alias_out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_deferred_exit_alias', '-ownership', deferred_exit_alias_source)
	assert deferred_exit_alias_out == 'source'

	nested_deferred_exit_alias_source := source.replace('box.value = value\n\tbox.value = replacement', 'if true {\n\t\tmut local := PointerBox{\n\t\t\tvalue: unsafe { replacement }\n\t\t}\n\t\tmut alias := &local\n\t\tdefer {\n\t\t\talias.value = value\n\t\t}\n\t\talias = &box\n\t}')
	assert nested_deferred_exit_alias_source != source
	nested_deferred_exit_alias_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_nested_deferred_exit_alias_c', '-ownership', nested_deferred_exit_alias_source)
	nested_deferred_exit_alias_main := c_fn_body(nested_deferred_exit_alias_c_source, 'int main(int argc, char** argv) {')
	compact_nested_deferred_exit_alias_main :=
		nested_deferred_exit_alias_main.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_nested_deferred_exit_alias_main.contains('array__free(&(__map_source_'), nested_deferred_exit_alias_main
	nested_deferred_exit_alias_out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_nested_deferred_exit_alias', '-ownership', nested_deferred_exit_alias_source)
	assert nested_deferred_exit_alias_out == 'source'

	late_defer_source := source.replace('box.value = value\n\tbox.value = replacement', 'box.value = value\n\tif value.text.len > 0 {\n\t\treturn\n\t}\n\tdefer {\n\t\tbox.value = replacement\n\t}')
	assert late_defer_source != source
	late_defer_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_late_defer_c', '-ownership', late_defer_source)
	late_defer_main := c_fn_body(late_defer_c_source, 'int main(int argc, char** argv) {')
	compact_late_defer_main := late_defer_main.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_late_defer_main.contains('array__free(&(__map_source_'), late_defer_main
	late_defer_out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_late_defer', '-ownership', late_defer_source)
	assert late_defer_out == 'source'

	select_source := source.replace('box.value = value\n\tbox.value = replacement', 'signal := chan bool{cap: 1}\n\tsignal <- true\n\tselect {\n\t\t<-signal {\n\t\t\tbox.value = value\n\t\t}\n\t\telse {\n\t\t\tbox.value = replacement\n\t\t}\n\t}')
	assert select_source != source
	select_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_storage_select_c', '-ownership', select_source)
	select_main_body := c_fn_body(select_c_source, 'int main(int argc, char** argv) {')
	compact_select_main := select_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_select_main.contains('array__free(&(__map_source_'), select_main_body
	select_out := run_good_with_flags(v3_bin, 'array_map_mutator_storage_select', '-ownership', select_source)
	assert select_out == 'source'

	direct_exit_source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn map_or_return(mut saved PointerBox, early bool) []int {
	return make_items().map(match true {
		true {
			mut local := PointerBox{
				value: saved.value
			}
			mut alias := &saved
			defer {
				alias.value = unsafe { &it }
			}
			if early {
				return [7]
			}
			alias = &local
			0
		}
		else {
			0
		}
	})
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	result := map_or_return(mut saved, true)
	println(result[0])
	println(saved.value.text)
}
'
	direct_exit_out := run_good_with_flags(v3_bin, 'array_map_deferred_direct_exit_alias', '-ownership', direct_exit_source)
	assert direct_exit_out == '7\nsource'
}

fn test_array_map_evaluates_return_expression_before_deferred_origin_snapshot() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn rebind_and_return(mut alias &PointerBox, saved &PointerBox) int {
	alias = saved
	return 7
}

fn map_or_return(saved &PointerBox) int {
	_ := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: saved.value
			}
			mut alias := &local
			defer {
				alias.value = unsafe { &it }
			}
			return rebind_and_return(mut alias, saved)
		}
		else {
			0
		}
	})
	return 0
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	result := map_or_return(&saved)
	println(result)
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_return_expr_deferred_origin_c', '-ownership', source)
	compact_c := c_source.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_c.contains('array__free(&(__map_source_'), c_source
	out := run_good_with_flags(v3_bin, 'array_map_return_expr_deferred_origin', '-ownership', source)
	assert out == '7\nsource'
}

fn test_array_map_keeps_temporary_source_through_mutator_or_success() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

fn may_fail(ok bool) ! {
	if !ok {
		return error("failed")
	}
}

fn (mut box PointerBox) store_or_replace(value &Item, replacement &Item, ok bool) {
	box.value = value
	may_fail(ok) or {
		box.value = replacement
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map(match true {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.store_or_replace(unsafe { &it }, unsafe { &external }, true)
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_or_success_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutator_or_success', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_mutator_target_alias() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) set(value &Item) {
	mut pbox := &box
	pbox.value = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map(match true {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.set(unsafe { &it })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_target_alias_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_mutator_target_alias', '-ownership', source)
	assert out == 'source'

	aggregate_source := source.replace('mut pbox := &box\n\tpbox.value = value', 'tmp := PointerBox{\n\t\tvalue: unsafe { value }\n\t}\n\tbox.value = tmp.value')
	assert aggregate_source != source
	aggregate_c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_mutator_aggregate_source_c', '-ownership', aggregate_source)
	aggregate_main_body := c_fn_body(aggregate_c_source, 'int main(int argc, char** argv) {')
	compact_aggregate_main :=
		aggregate_main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_aggregate_main.contains('array__free(&(__map_source_'), aggregate_main_body
	aggregate_out := run_good_with_flags(v3_bin, 'array_map_mutator_aggregate_source', '-ownership', aggregate_source)
	assert aggregate_out == 'source'
}

fn test_array_map_keeps_temporary_source_for_all_mutator_targets_and_computed_indices() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
mut:
	value &Item
}

struct Pair {
mut:
	first  PointerBox
	second PointerBox
}

struct IndexedBox {
mut:
	values []&Item
}

fn route(mut first PointerBox, mut second PointerBox, value &Item) {
	_ = first
	second.value = value
}

fn pick_index() int {
	return 0
}

fn store_at_computed_index(mut box IndexedBox, value &Item) {
	box.values[pick_index()] = value
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	pairs := make_items().map(match true {
		true {
			mut pair := Pair{
				first: PointerBox{
					value: unsafe { &external }
				}
				second: PointerBox{
					value: unsafe { &external }
				}
			}
			route(mut pair.first, mut pair.second, unsafe { &it })
			pair
		}
		else {
			Pair{}
		}
	})
	indexed := make_items().map(match true {
		true {
			mut box := IndexedBox{
				values: [unsafe { &external }]
			}
			store_at_computed_index(mut box, unsafe { &it })
			box
		}
		else {
			IndexedBox{}
		}
	})
	println(pairs[0].second.value.text)
	println(indexed[0].values[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_all_mutator_targets_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_all_mutator_targets', '-ownership', source)
	assert out == 'source\nsource'
}

fn test_array_map_traces_only_selected_comptime_mutator_branch() {
	v3_bin := build_v3_review_transform_ownership()
	source_true := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn (mut box PointerBox) set(value &Item, fallback &Item) {
	$if true {
		box.value = value
	} $else {
		box.value = fallback
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map(match true {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			box.set(unsafe { &it }, unsafe { &external })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source_true := gen_c_from_source_with_flags(v3_bin, 'array_map_comptime_mutator_true_c', '-ownership', source_true)
	main_body_true := c_fn_body(c_source_true, 'int main(int argc, char** argv) {')
	compact_main_true := main_body_true.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main_true.contains('array__free(&(__map_source_'), main_body_true
	out_true := run_good_with_flags(v3_bin, 'array_map_comptime_mutator_true', '-ownership', source_true)
	assert out_true == 'source'

	source_false := source_true.replace('\$if true {', '\$if false {')
	c_source_false := gen_c_from_source_with_flags(v3_bin, 'array_map_comptime_mutator_false_c', '-ownership', source_false)
	main_body_false := c_fn_body(c_source_false, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body_false.index('array__free(&(') or { -1 }
	result_move_pos := main_body_false.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body_false
	out_false := run_good_with_flags(v3_bin, 'array_map_comptime_mutator_false', '-ownership', source_false)
	assert out_false == 'external'
}

fn test_array_map_traces_only_selected_comptime_if_branch() {
	v3_bin := build_v3_review_transform_ownership()
	source_true := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map($if true {
		unsafe { &it }
	} $else {
		unsafe { &external }
	})
	println(selected[0].text)
}
'
	c_source_true := gen_c_from_source_with_flags(v3_bin, 'array_map_comptime_if_true_c', '-ownership', source_true)
	main_body_true := c_fn_body(c_source_true, 'int main(int argc, char** argv) {')
	compact_main_true := main_body_true.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main_true.contains('array__free(&(__map_source_'), main_body_true
	out_true := run_good_with_flags(v3_bin, 'array_map_comptime_if_true', '-ownership', source_true)
	assert out_true == 'source'

	source_false := source_true.replace('\$if true {', '\$if false {')
	c_source_false := gen_c_from_source_with_flags(v3_bin, 'array_map_comptime_if_false_c', '-ownership', source_false)
	main_body_false := c_fn_body(c_source_false, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body_false.index('array__free(&(') or { -1 }
	result_move_pos := main_body_false.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body_false
	out_false := run_good_with_flags(v3_bin, 'array_map_comptime_if_false', '-ownership', source_false)
	assert out_false == 'external'
}

fn test_array_map_drops_temporary_source_after_transient_mutating_method_argument() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	value &Item
}

fn (mut box PointerBox) inspect(value &Item) int {
	return box.value.text.len + value.text.len
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut box := PointerBox{
				value: unsafe { &external }
			}
			_ = box.inspect(unsafe { &it })
			box
		}
		else {
			PointerBox{
				value: unsafe { &external }
			}
		}
	})
	println(selected[0].value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_transient_mutating_method_arg_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_transient_mutating_method_arg', '-ownership', source)
	assert out == 'external'
}

fn test_array_map_keeps_temporary_source_through_returned_closure_capture() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct Callback {
	run fn () string
}

struct PointerBox {
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			box := PointerBox{
				value: unsafe { &it }
			}
			Callback{
				run: fn [box] () string {
					return box.value.text
				}
			}
		}
		else {
			box := PointerBox{
				value: unsafe { &external }
			}
			Callback{
				run: fn [box] () string {
					return box.value.text
				}
			}
		}
	})
	println(selected[0].run())
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_returned_closure_capture_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_returned_closure_capture', '-ownership', source)
	assert out == 'source'
}

fn test_array_map_keeps_temporary_source_through_invoked_closure_capture() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
text string
}

struct PointerBox {
	mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := &PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			p := unsafe { &it }
			fn [p, mut saved] () {
				saved.value = p
			}()
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_invoked_closure_capture_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_invoked_closure_capture', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_drops_temporary_source_for_projected_passthrough_parameter() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerPair {
	source &Item
	external &Item
}

fn pass(pair PointerPair) PointerPair {
	return pair
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map(pass(PointerPair{
		source: unsafe { &it }
		external: unsafe { &external }
	}).external)
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_projected_passthrough_parameter_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_projected_passthrough_parameter', '-ownership', source)
	assert out == 'external'
}

fn test_array_map_drops_temporary_source_after_nested_alias_overwrite() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	flag := true
	selected := make_items().map(match flag {
		true {
			mut p := unsafe { &external }
			if flag {
				p = unsafe { &it }
				p = unsafe { &external }
			}
			p
		}
		else {
			&external
		}
	})
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_nested_alias_overwrite_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_nested_alias_overwrite', '-ownership', source)
	assert out == 'external'
}

fn test_array_map_keeps_temporary_source_through_inherited_struct_update_field() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
	value &Item
	other int
}

fn make_items() []Item {
	return [Item{
		text: "four"
	}]
}

fn main() {
	values := make_items().map(PointerBox{
		...PointerBox{
			value: &it
		}
		other: 1
	}.value)
	println(values[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_assoc_pointer_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert !main_body.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_assoc_pointer_result', '-ownership', source)
	assert out == 'four'
}

fn test_array_map_drops_source_for_unselected_helper_result_field() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerPair {
	source   &Item
	external &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn pair(source &Item, external &Item) PointerPair {
	return unsafe {
		PointerPair{
			source: source
			external: external
		}
	}
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map(pair(&it, &external).external)
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_helper_external_field_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_helper_external_field', '-ownership', source)
	assert out == 'external'
}

fn test_array_sort_mixed_pointer_depth_comparator_arguments() {
	v3_bin := build_v3_review_transform()
	source := 'struct Thing {
	value int
}

fn main() {
	mut items := [&Thing{value: 3}, &Thing{value: 1}, &Thing{value: 2}]
	compare := fn (a &Thing, b &&Thing) int {
		return a.value - (*b).value
	}
	items.sort_with_compare(compare)
	println(int_str(items[0].value) + "," + int_str(items[1].value) + "," + int_str(items[2].value))
}
'
	out := run_good(v3_bin, 'array_sort_mixed_pointer_depth_comparator', source)
	assert out == '1,2,3'
}

fn test_array_sort_pointer_alias_comparator_arguments() {
	v3_bin := build_v3_review_transform()
	source := 'struct Thing {
	value int
}

type ThingRef = &Thing

fn main() {
	mut first := Thing{value: 3}
	mut second := Thing{value: 1}
	mut third := Thing{value: 2}
	mut items := [ThingRef(&first), ThingRef(&second), ThingRef(&third)]
	compare := fn (a ThingRef, b ThingRef) int {
		return a.value - b.value
	}
	items.sort_with_compare(compare)
	println(int_str(items[0].value) + "," + int_str(items[1].value) + "," + int_str(items[2].value))
}
'
	out := run_good(v3_bin, 'array_sort_pointer_alias_comparator', source)
	assert out == '1,2,3'
}

fn test_array_sort_pointer_value_comparator_expression_shapes() {
	v3_bin := build_v3_review_transform()
	source := 'struct Thing {
	value int
}

fn compare_thing_values(a &Thing, b &Thing) int {
	return a.value - b.value
}

fn get_thing_comparator() fn (&Thing, &Thing) int {
	return compare_thing_values
}

struct ThingComparators {
	compare fn (&Thing, &Thing) int
}

type ThingCompare = fn (&Thing, &Thing) int

fn sorted_values(items []&Thing) string {
	return items.map(it.value.str()).join(",")
}

fn main() {
	mut local_items := [&Thing{value: 3}, &Thing{value: 1}, &Thing{value: 2}]
	local_compare := compare_thing_values
	local_items.sort_with_compare(local_compare)
	println(sorted_values(local_items))

	mut call_items := [&Thing{value: 3}, &Thing{value: 1}, &Thing{value: 2}]
	call_items.sort_with_compare(get_thing_comparator())
	println(sorted_values(call_items))

	mut selector_items := [&Thing{value: 3}, &Thing{value: 1}, &Thing{value: 2}]
	comparators := ThingComparators{
		compare: compare_thing_values
	}
	selector_items.sort_with_compare(comparators.compare)
	println(sorted_values(selector_items))

	mut alias_items := [&Thing{value: 3}, &Thing{value: 1}, &Thing{value: 2}]
	alias_compare := ThingCompare(compare_thing_values)
	alias_items.sort_with_compare(alias_compare)
	println(sorted_values(alias_items))
}
'
	out := run_good(v3_bin, 'array_sort_pointer_value_comparator_expression_shapes', source)
	assert out == '1,2,3\n1,2,3\n1,2,3\n1,2,3'
}

fn test_array_map_drops_source_for_unrelated_pointer_result() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct Result {
	external &int
	length   int
}

fn make_items() []Item {
	return [Item{
		text: "four"
	}]
}

fn text_len(item &Item) int {
	return item.text.len
}

fn main() {
	external := 42
	results := make_items().map(Result{
		external: &external
		length:   text_len(&it)
	})
	println(results[0].length)
	println(*results[0].external)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_unrelated_pointer_result_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array results = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_unrelated_pointer_result', '-ownership', source)
	assert out == '4\n42'
}

fn test_array_map_drops_source_for_unselected_literal_element_pointer() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	selected := make_items().map([&it, &external][1])
	println(selected[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_selected_external_pointer_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_selected_external_pointer', '-ownership', source)
	assert out == 'external'
}

fn test_array_filter_and_map_reclaim_branch_selected_bound_methods() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Rule {
	min    int
	offset int
mut:
	calls int
}

fn (mut rule Rule) accept(value int) bool {
	rule.calls++
	return value >= rule.min
}

fn (mut rule Rule) shift(value int) int {
	rule.calls++
	return value + rule.offset
}

fn main() {
	mut total := 0
	for i in 0 .. 20_000 {
		first := Rule{
			min: 3
			offset: 10
		}
		second := Rule{
			min: 2
			offset: 20
		}
		use_first := i % 2 == 0
		filtered := [1, 2, 3, 4].filter(if use_first { first.accept } else { second.accept })
		mapped := [1, 2, 3].map(match use_first {
			true { first.shift }
			else { second.shift }
		})
		assert first.calls + second.calls == 7
		total += filtered.len + mapped[0]
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'array_branch_bound_method_callbacks_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert main_body.count('closure__closure_try_destroy(') >= 2, main_body
	out := run_good(v3_bin, 'array_branch_bound_method_callbacks', source)
	assert out == '370000'
}

fn test_nested_callback_array_fields_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := '@[heap]
struct Counter {
mut:
	value int
}

fn (counter &Counter) read() int {
	return counter.value
}

struct Holder {
	callbacks []fn () int
}

fn main() {
	mut total := 0
	for i in 0 .. 50_000 {
		mut counter := Counter{
			value: i
		}
		holder := Holder{
			callbacks: [counter.read]
		}
		counter.value++
		total += holder.callbacks[0]()
	}
	println(int_str(total))
}
'
	c_source := gen_c_from_source(v3_bin, 'nested_callback_array_field_hot_loop_c', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	assert main_body.contains('closure__closure_try_destroy(__array_closure_'), main_body
	out := run_good(v3_bin, 'nested_callback_array_field_hot_loop', source)
	assert out == '1250025000'
}

fn test_none_forwarded_to_specialized_generic_method_stays_none() {
	v3_bin := build_v3_review_transform()
	source := 'struct Item {
	value string
}

struct Mapper {}

fn (mapper Mapper) is_none[T](value ?T) bool {
	return value == none
}

fn forward_none[T]() bool {
	mapper := Mapper{}
	return mapper.is_none[T](none)
}

fn main() {
	println(forward_none[Item]())
}
'
	out := run_good(v3_bin, 'generic_method_none_argument', source)
	assert out == 'true'
}

fn test_generic_mut_parameter_typeof_keeps_pointer_shape() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_mut_parameter_typeof', 'struct Item {}

fn type_name[T](mut value T) string {
	_ = value
	return typeof(value).name
}

fn main() {
	mut item := Item{}
	println(type_name(mut item))
}
')
	assert out == '&Item'
}

fn test_specialized_generic_or_uses_alias_struct_storage() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'specialized_generic_or_alias_storage', 'type Label = string

struct Box[T] {
	value T
}

fn make_box[T](value T) !Box[T] {
	return Box[T]{
		value: value
	}
}

fn main() {
	box := make_box[Label](Label("ok")) or { Box[Label]{} }
	println(box.value)
}
')
	assert out == 'ok'
}

fn test_result_unwrapped_sum_collections_compare_semantically() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'result_unwrapped_sum_collection_equality', 'type Value = bool | string

fn values() ![]Value {
	return [Value("ok")]
}

fn value_map() !map[string]Value {
	return {
		"key": Value("ok")
	}
}

fn main() {
	println(values()! == [Value("ok")])
	println(value_map()! == {
		"key": Value("ok")
	})
}
')
	assert out == 'true\ntrue'
}

fn test_recursive_sum_cast_does_not_select_container_variant() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'recursive_sum_same_type_cast', 'type Tree = int | []Tree

fn leaf(value int) Tree {
	return Tree(value)
}

fn main() {
	tree := Tree([leaf(1), leaf(2)])
	assert tree == Tree([Tree(1), Tree(2)])
	println("ok")
}
')
	assert out == 'ok'
}

fn test_explicit_nested_array_generic_argument_keeps_all_dimensions() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'explicit_nested_array_generic_argument', 'fn make[T]() T {
	return T{}
}

fn main() {
	value := make[[][]int]()
	println(typeof(value).name)
}
')
	assert out == '[][]int'
}

fn test_for_in_generic_call_keeps_nested_array_element_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'for_in_generic_call_nested_array_element', 'import arrays

fn main() {
	for part in arrays.chunk("ABCD".bytes(), 2) {
		println(part[0])
	}
}
')
	assert out == '65\n67'
}

fn test_flag_enum_struct_field_defaults_to_zero() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'flag_enum_struct_field_default', '@[flag]
enum Mode {
	read
	write
}

struct Config {
	mode Mode
}

fn main() {
	config := Config{}
	println(int(config.mode))
}
')
	assert out == '0'
}

fn test_array_map_in_sum_smartcast_uses_collection_lowering() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_map_in_sum_smartcast', 'type Value = int | []int

fn normalize(value Value) Value {
	return match value {
		[]int { Value(value.map(it + 1)) }
		else { value }
	}
}

fn main() {
	result := normalize(Value([1, 2]))
	if result is []int {
		println(result)
	}
}
')
	assert out == '[2, 3]'
}

fn test_for_in_sum_array_smartcast_indexes_variant_payload() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'for_in_sum_array_smartcast', 'type Size = []f64 | f64

fn values(size Size) []f32 {
	mut result := []f32{}
	match size {
		[]f64 {
			for _, value in size {
				result << f32(value)
			}
		}
		f64 {
			result << f32(size)
		}
	}
	return result
}

fn main() {
	println(values(Size([1.5, 2.5])))
}
')
	assert out == '[1.5, 2.5]'
}

fn test_autofree_array_map_preserves_v1_sum_value_copy_compatibility() {
	v3_bin := build_v3_review_transform_ownership()
	out := run_good_with_flags(v3_bin, 'autofree_collection_copy_compatibility', '-autofree', 'struct Box {
	value string
}

type Node = Box | int

struct Pos {
	index int
}

fn (mut _ Pos) free() {}

struct Comment {
	text string
	pos  Pos
}

struct RecursiveField {
	name string
	decl RecursiveDecl
}

struct RecursiveDecl {
	fields []RecursiveField
}

fn values() []int {
	return [1, 2]
}

fn has_two() bool {
	if true && values().any(it == 2) {
		return true
	}
	return false
}

fn main() {
	values := [Box{
		value: "ok"
	}]
	nodes := values.map(Node(it))
	println(nodes.len)
	println((nodes[0] as Box).value)
	println(has_two())
	comments := [Comment{
		text: "kept"
	}]
	println(comments.filter(it.text == "kept").len)
	fields := [RecursiveField{
		name: "field"
	}]
	println(fields.filter(it.name == "field").len)
}
')
	assert out == '1\nok\ntrue\n1\n1'
}

fn test_smartcast_sum_value_in_direct_array_literal_is_reboxed() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'smartcast_sum_direct_array_literal', 'type Value = int | string

fn first(values []Value) Value {
	return values[0]
}

fn roundtrip(value Value) Value {
	return match value {
		int { first([value]) }
		string { first([value]) }
	}
}

fn main() {
	println(roundtrip(Value(42)))
	println(roundtrip(Value("ok")))
}
')
	assert out == "Value(42)\nValue('ok')"
}

fn test_sum_variant_field_does_not_become_same_named_method_value() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'sum_variant_field_method_name_collision', 'type Value = int | i32

fn (value Value) i32() i32 {
	return 0
}

fn extract[T](value T) i32 {
	$for variant in T.variants {
		if value is variant {
			$if variant.typ is i32 {
				variant_value := value
				return variant_value
			}
		}
	}
	return -1
}

fn main() {
	println(extract[Value](Value(i32(42))))
}
')
	assert out == '42'
}

fn test_interface_extension_method_uses_match_smartcast_receiver() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_extension_match_smartcast_receiver', 'interface Named {
	number() int
}

struct Alpha {}

fn (_ &Alpha) number() int {
	return 1
}

fn (_ &Alpha) str() string {
	return "alpha"
}

fn (value &Named) str() string {
	match value {
		Alpha { return value.str() }
		else { return "unknown" }
	}
}

fn main() {
	value := Named(&Alpha{})
	println(value.str())
}
')
	assert out == '&alpha'
}

fn test_struct_literal_implicit_reference_and_option_or_mut_receiver() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_literal_ref_and_option_or_mut_receiver', 'import net

struct Reader {
mut:
	value int
}

struct Client {
	reader ?Reader
}

fn (mut reader Reader) next() int {
	reader.value++
	return reader.value
}

fn borrow(reader &Reader) int {
	return reader.value
}

fn main() {
	mut client := Client{
		reader: Reader{
			value: 4
		}
	}
	println(borrow(Reader{
		value: 7
	}))
	println(client.reader or { return }.next())
	protocols := [net.Protocol.icmp, net.Protocol.icmpv6, net.Protocol.raw]
	println(protocols.len)
	unsafe {
		null_char := &char(0)
		println(isnil(null_char))
	}
}
')
	assert out == '7\n5\n3\ntrue'
}

fn test_implicit_voidptr_argument_promotes_local_to_heap() {
	v3_bin := build_v3_review_transform()
	generated := gen_c_from_source(v3_bin, 'implicit_voidptr_argument_heap_escape', 'struct State {
mut:
	value int
}

fn retain(_ voidptr) {}

fn register() {
	mut state := State{}
	retain(state)
	state.value = 7
}

fn main() {
	register()
}
')
	body := c_fn_body(generated, 'void v_register(void) {')
	assert body.contains('main__State* state'), body
	assert body.contains('memdup'), body
}

fn test_interface_pointer_arg_prefers_current_module_global_over_homonymous_const() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'interface_pointer_global_const_collision', {
		'v.mod':               "Module { name: 'interface_pointer_global_const_collision' }\n"
		'api/api.v':           'module api

@[has_globals]

pub interface Logger {
	value() int
}

pub struct Impl {
pub:
	n int
}

pub fn (logger &Impl) value() int {
	return logger.n
}

__global default_logger &Logger

fn init() {
	default_logger = &Impl{
		n: 7
	}
}

fn read(logger &Logger) int {
	return logger.value()
}

pub fn current() int {
	return read(default_logger)
}
'
		'consumer/consumer.v': 'module consumer

import api

pub const default_logger = &api.Impl{
	n: 99
}

pub fn current() int {
	return default_logger.value()
}
'
		'main.v':              'module main

import api
import consumer

fn main() {
	println(api.current())
	println(consumer.current())
}
'
	}, 'main.v')
	assert out == '7\n99'
}

fn test_array_accessors_are_addressable_append_targets() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_accessor_append_target', 'fn main() {
	mut nested := [][]int{}
	nested << []int{}
	nested.last() << [1, 2, 3]
	nested.first() << [4, 5, 6]
	println(nested)
}
')
	assert out == '[[1, 2, 3, 4, 5, 6]]'
}

fn test_selected_comptime_block_preserves_outer_value_tail() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'selected_comptime_block_value_tail', "fn main() {
	value := if true {
		\$if msvc { 'msvc' } \$else { 'other' }
	} else {
		''
	}
	println(value)
}
")
	assert out == 'other'
}

fn test_none_literal_str_method() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'none_literal_str', 'fn main() {
	println(none.str())
}
')
	assert out == '<none>'
}

fn test_smartcast_sum_value_keeps_sum_method_receiver() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'smartcast_sum_method_receiver', 'struct Square {}

struct Circle {}

type Shape = Circle | Square

fn (shape Shape) shape_name() string {
	return match shape {
		Circle { "circle" }
		Square { "square" }
	}
}

fn print_name(shape Shape) {
	if shape is Square {
		println(shape.shape_name())
	}
}

fn main() {
	print_name(Square{})
}
')
	assert out == 'square'
}

fn test_smartcast_nested_sum_uses_nested_sum_method_receiver() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'smartcast_nested_sum_method_receiver', 'struct Integer {}

struct Text {}

struct Empty {}

type Expr = Integer | Text
type Node = Empty | Expr

fn (expr Expr) expr_name() string {
	return match expr {
		Integer { "integer" }
		Text { "text" }
	}
}

fn print_name(node Node) {
	if node is Expr {
		println(node.expr_name())
	}
}

fn main() {
	print_name(Expr(Integer{}))
}
')
	assert out == 'integer'
}

fn test_nested_match_smartcast_declaration_uses_nearest_variant() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_flags(v3_bin, 'nested_match_smartcast_nearest_variant', '-building-v', 'struct Assoc {
	exprs []Expr
}

struct Empty {}
struct Other {}

type Expr = Assoc | Empty
type Node = Expr | Other

fn child_count(node Node) int {
	if node is Expr {
		match node {
			Assoc {
				assoc := node
				return assoc.exprs.len
			}
			Empty {
				return 0
			}
		}
	}
	return 0
}

fn main() {
	println(child_count(Expr(Assoc{
		exprs: [Expr(Empty{})]
	})))
}
')
	assert out == '1'
}

fn test_building_v_keeps_valid_match_and_filtered_array_expression_types() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_flags(v3_bin, 'building_v_valid_expression_types', '-building-v -d valid_exprs', 'struct NumberInfo {
	values []int
}

struct NameInfo {
	names []string
}

type Info = NameInfo | NumberInfo

fn item_count(info Info) int {
	return match info {
		NumberInfo { info.values.len }
		NameInfo { info.names.len }
	}
}

fn filtered_path_count() int {
	$if valid_exprs ? {
		first := "one"
		second := ""
		if first.len > 0 {
			paths := [first, second].filter(it.len != 0)
			return paths.len
		}
	}
	return 0
}

fn main() {
	println(item_count(Info(NumberInfo{
		values: [1, 2]
	})))
	println(filtered_path_count())
}
')
	assert out == '2\n1'
}

fn test_last_lvalue_stabilizes_side_effecting_receiver() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'last_lvalue_side_effecting_receiver', '__global calls int

fn next() int {
	index := calls
	calls++
	return index
}

fn main() {
	mut arrays := [[[1, 2]], [[3, 4]]]
	arrays[next()].last() << 9
	println(int_str(calls))
	println(arrays[0])
	println(arrays[1])
}
')
	assert out == '1\n[[1, 2, 9]]\n[[3, 4]]'
}

fn test_shadowed_allocation_helper_fn_values_preserve_address_escapes() {
	v3_bin := build_v3_review_transform()
	c_source := gen_c_from_source(v3_bin, 'shadowed_allocation_helper_fn_value_escape', 'fn pass(p &int) &int {
	return p
}

fn make_memdup() &int {
	local := 41
	memdup := pass
	return memdup(&local)
}

fn make_memdup_noscan() &int {
	local := 42
	memdup_noscan := pass
	return memdup_noscan(&local)
}

fn make_aligned_memdup() &int {
	local := 43
	v3_aligned_memdup := pass
	return v3_aligned_memdup(&local)
}

fn make_builtin_memdup() &int {
	local := 44
	return unsafe { &int(memdup(&local, sizeof(int))) }
}

fn main() {
	println(unsafe { *make_memdup() })
	println(unsafe { *make_memdup_noscan() })
	println(unsafe { *make_aligned_memdup() })
	println(unsafe { *make_builtin_memdup() })
}
')
	for fn_name in ['make_memdup', 'make_memdup_noscan', 'make_aligned_memdup'] {
		body := c_fn_body(c_source, 'int* ${fn_name}(void) {')
		assert body.contains('int* local ='), body
	}
	builtin_body := c_fn_body(c_source, 'int* make_builtin_memdup(void) {')
	assert builtin_body.contains('i64 local = 44;'), builtin_body
	assert builtin_body.contains('memdup(&local, sizeof(int))'), builtin_body
}

fn test_parallel_monomorph_workers_use_disjoint_lifted_literal_names() {
	v3_bin := build_v3_review_transform()
	out := run_good_with_env(v3_bin, 'parallel_monomorph_literal_names', 'VJOBS=4', 'fn apply[T](value T, callback fn (T) int) int {
	return callback(value)
}

fn alpha[T](value T, offset int) int {
	return apply(value, fn [offset] (_ T) int {
		return offset
	})
}

fn beta[T](value T, label string) int {
	return apply(value, fn [label] (_ T) int {
		return label.len
	})
}

fn gamma[T](value T, enabled bool) int {
	return apply(value, fn [enabled] (_ T) int {
		return if enabled { 1 } else { 0 }
	})
}

fn main() {
	println(alpha(1, 7))
	println(beta("x", "four"))
	println(gamma(u64(3), true))
}
')
	assert out == '7\n4\n1'
}

fn test_parallel_transform_defers_large_const_map_expansion() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform()
	mut entries := []string{cap: 512}
	for i in 0 .. 512 {
		entries << "\t\t'key_${i}': ${i}"
	}
	source := "const large_lookup = {\n\t'group': {\n${entries.join('\n')}\n\t}\n}\n\nfn read_large_lookup(key string) int {\n\treturn large_lookup['group'][key]\n}\n\nfn main() {\n\tprintln(read_large_lookup('key_511'))\n}\n"
	out := run_good_with_env(v3_bin, 'parallel_large_const_map', 'VJOBS=4', source)
	assert out == '511'
}

fn test_parallel_transform_defers_external_const_collection_clone_expansion() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform_ownership()
	mut fields := []string{cap: 256}
	for i in 0 .. 256 {
		fields << '\tfield_${i} string'
	}
	mut readers := []string{cap: 64}
	mut calls := []string{cap: 64}
	for i in 0 .. 64 {
		readers << 'fn read_${i}() int {\n\treturn if clone_lookup.len == 1 { ${i} } else { -10000 }\n}'
		calls << '\ttotal += read_${i}()'
	}
	source := "struct Wide implements IClone {\n${fields.join('\n')}\n}\n\nfn make_items() []Wide {\n\treturn [Wide{\n\t\tfield_255: 'ok'\n\t}]\n}\n\nconst clone_lookup = {\n\t'items': make_items().clone()\n}\n\n${readers.join('\n\n')}\n\nfn main() {\n\tmut total := 0\n${calls.join('\n')}\n\tprintln(total)\n}\n"
	out := run_good_with_env(v3_bin, 'parallel_const_collection_clone', 'VJOBS=4', source)
	assert out == '2016'
}

fn test_parallel_transform_defers_large_const_map_membership_expansion() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform()
	mut entries := []string{cap: 512}
	for i in 0 .. 512 {
		entries << "\t'key_${i}': ${i}"
	}
	source := "const large_lookup = {\n${entries.join('\n')}\n}\n\nfn has_key(key string) bool {\n\treturn key in large_lookup\n}\n\nfn lacks_key(key string) bool {\n\treturn key !in large_lookup\n}\n\nfn main() {\n\tprintln(has_key('key_511'))\n\tprintln(lacks_key('missing'))\n}\n"
	out := run_good_with_env(v3_bin, 'parallel_large_const_map_membership', 'VJOBS=4', source)
	assert out == 'true\ntrue'
}

fn test_parallel_transform_defers_external_const_map_conditional_expansion() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform()
	mut conditionals := []string{cap: 300}
	for i in 0 .. 300 {
		conditionals << 'if false { 0 } else { ${i} }'
	}
	source := "const conditional_lookup = {\n\t'value': [${conditionals.join(',\n\t\t')}]\n}\n\nfn read_conditional_lookup() int {\n\treturn conditional_lookup['value'][299]\n}\n\nfn main() {\n\tprintln(read_conditional_lookup())\n}\n"
	out := run_good_with_env(v3_bin, 'parallel_const_map_conditional', 'VJOBS=4', source)
	assert out == '299'
}

fn test_parallel_transform_defers_external_const_map_match_expansion() {
	$if windows {
		return
	}
	v3_bin := build_v3_review_transform()
	mut arms := []string{cap: 300}
	for i in 0 .. 300 {
		arms << '${i} { ${i} }'
	}
	source := "const match_lookup = {\n\t'value': [match 299 {\n\t\t${arms.join('\n\t\t')}\n\t\telse { -1 }\n\t}]\n}\n\nfn read_match_lookup() int {\n\treturn match_lookup['value'][0]\n}\n\nfn main() {\n\tprintln(read_match_lookup())\n}\n"
	out := run_good_with_env(v3_bin, 'parallel_const_map_match', 'VJOBS=4', source)
	assert out == '299'
}

fn test_parallel_transform_merges_generic_call_metadata() {
	v3_bin := build_v3_review_transform()
	mut source := 'fn identity[T](value T) T {\n\treturn value\n}\n\n'
	mut expected := 0
	for i in 0 .. 300 {
		source += 'fn value_${i}() int {\n\treturn identity(${i})\n}\n\n'
		expected += i
	}
	source += 'fn main() {\n\tmut total := 0\n'
	for i in 0 .. 300 {
		source += '\ttotal += value_${i}()\n'
	}
	source += '\tprintln(total)\n}\n'
	out := run_good_with_env(v3_bin, 'parallel_transform_generic_calls', 'VJOBS=4', source)
	assert out == expected.str()
}

fn test_array_map_keeps_source_for_loop_carried_external_pointer_origin() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			for _ in 0 .. 2 {
				alias.value = unsafe { &it }
				alias = &saved
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_loop_carried_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_loop_carried_pointer_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_for_loop_carried_helper_pointer_origin() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn store_on_second_iteration(mut box PointerBox, value &Item, replacement &Item) {
	mut local := PointerBox{
		value: unsafe { replacement }
	}
	mut alias := &local
	for _ in 0 .. 2 {
		alias.value = unsafe { value }
		alias = &box
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			store_on_second_iteration(mut saved, unsafe { &it }, unsafe { &external })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_loop_carried_helper_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_loop_carried_helper_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_for_conditional_continue_helper_pointer_origin() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn store_after_continue(mut box PointerBox, value &Item, replacement &Item) {
	mut local := PointerBox{
		value: unsafe { replacement }
	}
	mut alias := &local
	for i in 0 .. 2 {
		alias.value = unsafe { value }
		alias = &box
		if i == 0 {
			continue
		}
		alias = &local
	}
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			store_after_continue(mut saved, unsafe { &it }, unsafe { &external })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_conditional_continue_helper_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_conditional_continue_helper_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_keeps_source_for_break_helper_pointer_origin() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn store_after_break(mut box PointerBox, value &Item, replacement &Item) {
	mut local := PointerBox{
		value: unsafe { replacement }
	}
	mut alias := &local
	mut i := 0
	for i = 0; i < 1; alias = &local {
		alias = &box
		break
	}
	alias.value = unsafe { value }
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			store_after_break(mut saved, unsafe { &it }, unsafe { &external })
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_break_helper_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_break_helper_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_labeled_continue_does_not_feed_inner_loop_fixed_point() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			outer: for _ in 0 .. 1 {
				alias = &local
				for _ in 0 .. 1 {
					alias.value = unsafe { &it }
					alias = &saved
					continue outer
				}
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_labeled_continue_inner_fixed_point_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	source_drop_pos := main_body.index('array__free(&(') or { -1 }
	result_move_pos := main_body.index('Array selected = ') or { -1 }
	assert source_drop_pos >= 0 && source_drop_pos < result_move_pos, main_body
	out := run_good_with_flags(v3_bin, 'array_map_labeled_continue_inner_fixed_point', '-ownership', source)
	assert out == '0\nexternal'
}

fn test_array_map_keeps_source_for_external_array_slice_backing() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := []&Item{len: 1}
	saved[0] = unsafe { &external }
	selected := make_items().map(match true {
		true {
			mut alias := unsafe { saved[..] }
			alias[0] = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved[0].text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_external_slice_backing_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_external_slice_backing', '-ownership', source)
	assert out == '0\nsource'
}

fn test_struct_stringification_evaluates_dereferenced_pointer_call_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_str_dereferenced_call_once', 'struct Node {
	value int
}

struct Counter {
mut:
	calls int
}

fn next_node(mut counter Counter) &Node {
	counter.calls++
	return &Node{
		value: counter.calls
	}
}

fn main() {
	mut counter := Counter{}
	println("\${*next_node(mut counter)}")
	println(int_str(counter.calls))
}
')
	assert out == 'Node{\n    value: 1\n}\n1'
}

fn test_specialized_generic_match_expression_ignores_unresolved_branch_types() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'specialized_generic_match_expression_type', "type Value = []u8 | i64 | string

struct Reader {}

fn (Reader) load[T]() !map[string]T {
	\$if T is string {
		return {
			'answer': 'ready'
		}
	}
	return map[string]T{}
}

fn normalize[T](value Value) T {
	\$if T is string {
		result := match value {
			[]u8 { value.bytestr() }
			string { value }
			i64 { value.str() }
		}
		return result
	}
	return T{}
}

fn main() {
	println(normalize[string](Value('ready')))
	result := Reader{}.load[string]() or { panic(err) }
	println(result['answer'])
}
")
	assert out == 'ready\nready'
}

fn test_comptime_reflected_enum_field_accepts_integer_decoder_result() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_reflected_enum_integer_assignment', 'enum Role {
	worker
	manager
}

struct Config {
	role Role
}

fn decoded_number() int {
	return 1
}

fn decode[T]() T {
	mut value := T{}
	$for field in T.fields {
		$if field.is_enum {
			value.$(field.name) = decoded_number()
		}
	}
	return value
}

fn main() {
	println(decode[Config]().role)
}
')
	assert out == 'manager'
}

fn test_imported_generic_default_uses_main_struct_defaults_after_short_name_collision() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'generic_main_default_short_name_collision', {
		'v.mod':       "Module { name: 'generic_main_default_short_name_collision' }\n"
		'factory/x.v': 'module factory\n\nstruct Doc {\n\tast &int\n}\n\npub fn make[T]() T {\n\treturn T{}\n}\n'
		'main.v':      'module main\n\nimport factory\n\nstruct Doc {\n\titems []int\n}\n\nfn main() {\n\tvalue := factory.make[Doc]()\n\tprintln(value.items.len)\n}\n'
	}, 'main.v')
	assert out == '0'
}

fn test_address_of_nil_pointer_cast_provides_pointer_value_storage() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'address_of_nil_pointer_cast_storage', 'struct Holder {
	ptr voidptr
}

fn main() {
	value := Holder{
		ptr: unsafe { &voidptr(nil) }
	}
	println(value.ptr == unsafe { nil })
}
')
	assert out == 'true'
}

fn test_later_block_call_arg_does_not_capture_earlier_sum_temporary() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'call_arg_sum_temp_before_block', 'struct Item {}

type Node = Item

fn inspect(node &Node, data voidptr) {
	println(node is Item)
	println(data == unsafe { nil })
}

fn main() {
	item := &Item{}
	inspect(item, unsafe { nil })
}
')
	assert out == 'true\ntrue'
}

fn test_test_command_skips_incompatible_single_file() {
	v3_bin := build_v3_review_transform()
	test_dir := os.join_path(os.temp_dir(), 'v3_incompatible_test_${os.getpid()}')
	os.rmdir_all(test_dir) or {}
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	test_src := os.join_path(test_dir, 'v3_incompatible_windows_test.v')
	os.write_file(test_src, 'module main\n\nfn test_never_checked() {\n\tmissing_symbol()\n}\n') or {
		panic(err)
	}
	result := os.execute('${v3_bin} -nocache -os linux test ${test_src}')
	assert result.exit_code == 0, result.output
	assert result.output.contains('SKIP ${test_src}'), result.output

	backend_test_src := os.join_path(test_dir, 'v3_incompatible_backend_test.js.v')
	os.write_file(backend_test_src, 'module main\n\nfn test_never_checked() {\n\tmissing_symbol()\n}\n') or { panic(err) }
	backend_result := os.execute('${v3_bin} -nocache test ${backend_test_src}')
	assert backend_result.exit_code == 0, backend_result.output
	assert backend_result.output.contains('SKIP ${backend_test_src}'), backend_result.output

	compatible_test_src := os.join_path(test_dir, 'v3_compatible_backend_test.c.v')
	os.write_file(compatible_test_src, "module main\n\nfn test_test_command_skips_incompatible_single_file() {\n\tprintln('compatible backend test')\n}\n") or {
		panic(err)
	}
	compatible_result := os.execute('${v3_bin} -nocache test ${compatible_test_src}')
	assert compatible_result.exit_code == 0, compatible_result.output
	assert !compatible_result.output.contains('SKIP ${compatible_test_src}'), compatible_result.output
	assert compatible_result.output.contains('compatible backend test'), compatible_result.output
}

fn test_test_command_honors_vtest_build_constraint() {
	v3_bin := build_v3_review_transform()
	test_src := os.join_path(os.temp_dir(), 'v3_constrained_test.v')
	os.write_file(test_src, '// vtest build: windows\nmodule main\n\nfn test_never_checked() {\n\tmissing_symbol()\n}\n') or {
		panic(err)
	}
	result := os.execute('${v3_bin} -nocache -os linux test ${test_src}')
	assert result.exit_code == 0, result.output
	assert result.output.contains('SKIP ${test_src}'), result.output
}

fn test_module_can_import_package_with_its_own_short_name() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'same_named_import', {
		'v.mod':                        "Module { name: 'same_named_import' }\n"
		'nested/time/same_name_test.v': 'module time

import time

struct Time {
	local int
}

fn test_imported_time_type_is_available() {
	assert time.Time{}.year == 0
	assert Time{}.local == 0
}
'
	}, 'nested/time/same_name_test.v')
	assert out == ''
}

fn test_array_map_follows_c_for_execution_order_for_pointer_origins() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			mut local := PointerBox{
				value: unsafe { &external }
			}
			mut alias := &local
			mut i := 0
			for alias = &saved; i < 1; alias = &local {
				alias.value = unsafe { &it }
				i++
			}
			0
		}
		else {
			0
		}
	})
	println(selected[0])
	println(saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_c_for_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_c_for_pointer_origin', '-ownership', source)
	assert out == '0\nsource'
}

fn test_array_map_applies_c_for_post_to_continue_pointer_origins() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn main() {
	external := Item{
		text: "external"
	}
	mut saved := PointerBox{
		value: unsafe { &external }
	}
	selected := make_items().map(match true {
		true {
			local := Item{
				text: "local"
			}
			mut local_box := PointerBox{
				value: unsafe { &local }
			}
			mut alias := &local_box
			mut i := 0
			for i = 0; i < 1; alias = &local_box {
				alias = &saved
				i++
				continue
			}
			alias.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(selected[0])
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_c_for_continue_post_pointer_origin_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_c_for_continue_post_pointer_origin', '-ownership', source)
	assert out == '0'
}

fn test_array_map_tracks_aggregate_pointer_origins_from_or_and_comptime_results() {
	v3_bin := build_v3_review_transform_ownership()
	source := 'struct Item {
	text string
}

struct PointerBox {
mut:
	value &Item
}

struct Holder {
	box &PointerBox
}

fn make_items() []Item {
	return [Item{
		text: "source"
	}]
}

fn fallback_holder() !Holder {
	return error("fallback")
}

fn main() {
	external := Item{
		text: "external"
	}
	mut or_saved := PointerBox{
		value: unsafe { &external }
	}
	mut comptime_saved := PointerBox{
		value: unsafe { &external }
	}
	_ := make_items().map(match true {
		true {
			or_holder := fallback_holder() or {
				Holder{
					box: &or_saved
				}
			}
			comptime_holder := $if true {
				Holder{
					box: &comptime_saved
				}
			} $else {
				Holder{
					box: &or_saved
				}
			}
			or_holder.box.value = unsafe { &it }
			comptime_holder.box.value = unsafe { &it }
			0
		}
		else {
			0
		}
	})
	println(or_saved.value.text)
	println(comptime_saved.value.text)
}
'
	c_source := gen_c_from_source_with_flags(v3_bin, 'array_map_aggregate_join_origins_c', '-ownership', source)
	main_body := c_fn_body(c_source, 'int main(int argc, char** argv) {')
	compact_main := main_body.replace(' ', '').replace('\t', '').replace('\n', '')
	assert !compact_main.contains('array__free(&(__map_source_'), main_body
	out := run_good_with_flags(v3_bin, 'array_map_aggregate_join_origins', '-ownership', source)
	assert out == 'sourcesource'
}

fn test_nested_string_interpolation_restores_outer_scanner_state() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_string_interpolation', "fn main() {\n\tvalue := 'ok'\n\tprintln('outer \${if value.len > 0 { 'inner \${value}' } else { 'empty' }} done')\n}\n")
	assert out == 'outer inner ok done'
}

fn test_json_result_or_tail_keeps_if_expression_struct_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json_result_or_if_struct', 'import x.json2

struct Review {
	id int
}

fn main() {
	review := if true {
		json2.decode[Review]("{\\"id\\":7}") or { Review{id: 1} }
	} else {
		Review{id: 2}
	}
	println(review.id)
}
')
	assert out == '7'
}

fn test_unsafe_postfix_and_pointer_to_pointer_type_parse() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'unsafe_postfix_pointer_to_pointer', 'struct Item {
	value int
}

struct PointerState {
mut:
	list &&char
}

type RawHandle = voidptr

fn (item &Item) read() int {
	return item.value
}

fn main() {
	item := &Item{value: 9}
	state := PointerState{
		list: unsafe { &&char(nil) }
	}
	handle := RawHandle(unsafe { nil })
	println(unsafe { item }.read())
	println(state.list == unsafe { &&char(nil) })
	println(handle == RawHandle(unsafe { nil }))
}
')
	assert out == '9\ntrue\ntrue'
}

fn test_export_wrapper_uses_declared_header_c_abi() {
	v3_bin := build_v3_review_transform()
	header := os.join_path(os.temp_dir(), 'v3_export_wrapper_c_abi.h')
	os.write_file(header, 'extern int v3_exported_int(int value);\n') or { panic(err) }
	out := run_good(v3_bin, 'export_wrapper_c_abi', '#include "${header}"

@[export: "v3_exported_int"]
fn exported_int(value int) int {
	return value + 1
}

fn main() {
	println(exported_int(6))
}
')
	assert out == '7'
}
