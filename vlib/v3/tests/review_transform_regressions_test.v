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
	drop_owned(state)
}

fn main() {
	shared state := State{
		label: "ok"
	}
	destroy(state)
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

fn gen_c_from_source(v3_bin string, name string, src string) string {
	src_path := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.rm(c_path) or {}
	compile := os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
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
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${good_bin}')
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.execute(good_bin)
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn gen_c_from_project(v3_bin string, name string, files map[string]string, input string) string {
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
	compile := os.execute('${v3_bin} ${input_path} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: C generation failed\n${compile.output}'
	return os.read_file(c_path) or { panic(err) }
}

fn test_lifted_fn_literal_mut_param_interpolation_derefs_value() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'lifted_literal_mut_param_interpolation',
		'fn main() {\n\tmut n := 7\n\tf := fn (mut x int) {\n\t\tprintln("\${x}")\n\t}\n\tf(mut n)\n}\n')
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

fn test_folded_string_constant_ifs_keep_branch_scopes() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'folded_string_constant_if_branch_scopes',
		"fn main() {\n\tif 'left' == 'left' {\n\t\tx := 20\n\t\tprintln(int_str(x))\n\t}\n\tif 'right' == 'right' {\n\t\tx := 22\n\t\tprintln(int_str(x))\n\t}\n}\n")
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

fn test_for_in_smartcast_interface_field_keeps_interface_element_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'for_in_smartcast_interface_field',
		'interface Widget {\n\tid int\n}\n\nstruct Stack {\n\tid       int\n\tchildren []Widget\n}\n\nstruct Leaf {\n\tid int\n}\n\nfn total(w Widget) int {\n\tif w is Stack {\n\t\tmut value := w.id\n\t\tfor child in w.children {\n\t\t\tvalue += total(child)\n\t\t}\n\t\treturn value\n\t}\n\treturn w.id\n}\n\nfn main() {\n\tw := Widget(Stack{\n\t\tid: 1\n\t\tchildren: [Widget(Leaf{\n\t\t\tid: 2\n\t\t})]\n\t})\n\tprintln(int_str(total(w)))\n}\n')
	assert out == '3'
}

fn test_interface_smartcast_rebuilds_richer_interface_fields() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'interface_smartcast_richer_fields',
		'interface Base {\n\tx int\n}\n\ninterface Extended {\n\tBase\n\ty    int\n\tnext ?Extended\n}\n\nstruct Item {\n\tx    int\n\ty    int\n\tnext ?Extended\n}\n\nfn value(base Base) int {\n\tif base is Extended {\n\t\treturn base.x + base.y\n\t}\n\treturn 0\n}\n\nfn main() {\n\tprintln(int_str(value(Base(Item{\n\t\tx: 2\n\t\ty: 3\n\t}))))\n}\n')
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
	if base is Rich {
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
	generated := gen_c_from_source(v3_bin, 'fixed_array_alias_import_context',
		'import gg\nimport sokol.gfx\n\nfn main() {\n\t_ := gg.Color{}\n\t_ := gfx.ImageData{}\n}\n')
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
	out := run_good(v3_bin, 'generic_array_retype_temp_scope',
		"enum ChildSize {\n\tcompact\n}\n\nfn delimiters() [][]string {\n\treturn [['/*', '*/']]\n}\n\nfn child_sizes(len int) []ChildSize {\n\treturn [ChildSize.compact].repeat(len)\n}\n\nfn main() {\n\tvalues := delimiters()\n\tsizes := child_sizes(2)\n\tprintln(values[0][0])\n\tprintln(values[0][1])\n\tprintln(int_str(sizes.len))\n}\n")
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
	out := run_good(v3_bin, 'nested_inferred_fixed_array_literal',
		'fn main() {\n\tvalues := [..][..]int[[1, 2], [3, 4]]\n\tprintln(int_str(values[0][0] + values[0][1] + values[1][0] + values[1][1]))\n}\n')
	assert out == '10'
	run_bad(v3_bin, 'ragged_nested_inferred_fixed_array_literal',
		'fn main() {\n\t_ := [..][..]int[[1], [2, 3]]\n}\n',
		'inferred fixed-array literal rows must have the same size')
}

fn test_shared_field_without_sync_import_compiles_and_locks() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'shared_field_without_sync_import',
		'struct S {\nmut:\n\ta shared int\n}\n\nfn main() {\n\tmut s := S{}\n\tlock s.a {\n\t\ts.a = 7\n\t\tprintln(int_str(s.a))\n\t}\n}\n')
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
	run_bad(v3_bin, 'bad_fixed_array_literal_len',
		'fn take3(a [3]int) int {\n\treturn a[0]\n}\nfn main() {\n\t_ := take3([1, 2])\n}\n',
		'cannot use')
	run_bad(v3_bin, 'bad_dynamic_array_for_fixed_array',
		'fn take3(a [3]int) int {\n\treturn a[0]\n}\nfn main() {\n\txs := [1, 2, 3]\n\t_ := take3(xs)\n}\n',
		'cannot use')
	out := run_good(v3_bin, 'good_exact_fixed_array_literal',
		'fn take3(a [3]int) int {\n\treturn a[0] + a[1] + a[2]\n}\nfn main() {\n\tprintln(int_str(take3([1, 2, 3])))\n}\n')
	assert out == '6'
	indexed := run_good(v3_bin, 'good_fixed_array_init_index',
		'fn main() {\n\ta := [4]int{init: index * index}\n\tprintln(int_str(a[0]) + "," + int_str(a[1]) + "," + int_str(a[2]) + "," + int_str(a[3]))\n}\n')
	assert indexed == '0,1,4,9'
	const_indexed := run_good(v3_bin, 'good_fixed_array_const_init_index',
		'const n = 4\n\nfn main() {\n\ta := [n]int{init: index * index}\n\tprintln(int_str(a[0]) + "," + int_str(a[1]) + "," + int_str(a[2]) + "," + int_str(a[3]))\n}\n')
	assert const_indexed == '0,1,4,9'
	arg_indexed := run_good(v3_bin, 'good_fixed_array_arg_init_index',
		'fn take(a [4]int) int {\n\treturn a[0] + a[1] + a[2] + a[3]\n}\n\nfn main() {\n\tprintln(int_str(take([4]int{init: index * index})))\n}\n')
	assert arg_indexed == '14'
}

fn test_array_equality_uses_semantic_element_comparison() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'semantic_array_equality',
		"struct Child {\n\tlabel string\n}\n\nstruct Item {\n\tname string\n\tparts []string\n\tnested [][]string\n\tchildren []Child\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := [Item{\n\t\tname: 'hi'.clone()\n\t\tparts: ['ab'.clone()]\n\t\tnested: [[join('n', 'est')]]\n\t\tchildren: [Child{\n\t\t\tlabel: 'kid'.clone()\n\t\t}]\n\t}]\n\tright := [Item{\n\t\tname: join('h', 'i')\n\t\tparts: [join('a', 'b')]\n\t\tnested: [['nest'.clone()]]\n\t\tchildren: [Child{\n\t\t\tlabel: join('k', 'id')\n\t\t}]\n\t}]\n\tmaps_left := [{\n\t\t'k': 'value'.clone()\n\t}]\n\tmaps_right := [{\n\t\t'k': join('val', 'ue')\n\t}]\n\tnested_left := [[join('y', 'o')]]\n\tnested_right := [['yo'.clone()]]\n\tchild_map_left := {\n\t\t'items': [Child{\n\t\t\tlabel: 'mapkid'.clone()\n\t\t}]\n\t}\n\tchild_map_right := {\n\t\t'items': [Child{\n\t\t\tlabel: join('map', 'kid')\n\t\t}]\n\t}\n\tneedle := Item{\n\t\tname: join('h', 'i')\n\t\tparts: [join('a', 'b')]\n\t\tnested: [['nest'.clone()]]\n\t\tchildren: [Child{\n\t\t\tlabel: join('k', 'id')\n\t\t}]\n\t}\n\tprintln(left == right)\n\tprintln(left.equals(right))\n\tprintln(maps_left == maps_right)\n\tprintln(nested_left == nested_right)\n\tprintln(child_map_left == child_map_right)\n\tprintln(needle in left)\n\tprintln(int_str(left.index(needle)))\n}\n")
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

fn test_array_map_fn_value_uses_callback_return_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_map_fn_value_return_type',
		"fn main() {\n\ti_to_str := fn (i int) string {\n\t\treturn int_str(i)\n\t}\n\ta := [1, 2, 3].map(i_to_str)\n\tassert a == ['1', '2', '3']\n\tprintln(a[0] + a[1] + a[2])\n}\n")
	assert out == '123'
}

fn test_const_array_allows_newline_separators_with_line_comments() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'const_array_line_comments',
		'const xs = [\n\t1\n\t// one\n\t2\n\t// two\n\t3\n]\n\nfn main() {\n\tprintln(int_str(xs.len))\n\tprintln(int_str(xs[1]))\n}\n')
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

fn test_mut_pointer_capture_is_not_over_dereferenced() {
	v3_bin := build_v3_review_transform()
	// A `[mut p]` capture whose original type is already a pointer (`&S`) must stay a
	// genuine `&S` local: its rvalue uses must not be over-dereferenced, so a call that
	// expects the pointer still receives it (regression for gating the pointer-value
	// rvalue/lvalue flags on `capture_by_ref` instead of every `capture_mut`).
	out := run_good(v3_bin, 'mut_pointer_capture',
		'struct S {\n\tn int\n}\n\nfn takes_ptr(p &S) int {\n\treturn p.n\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut p := &S{\n\t\tn: 5\n\t}\n\tcall(fn [mut p] () {\n\t\tprintln(int_str(takes_ptr(p)))\n\t})\n}\n')
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	c_source := gen_c_from_source(v3_bin, 'local_dynamic_callback_array_index_assign_hot_loop_c',
		source)
	assert c_source.contains('closure__closure_try_destroy(__field_closure_'), c_source
	assert c_source.contains('.receiver = &(counter)'), c_source
	out := run_good(v3_bin, 'local_dynamic_callback_array_index_assign_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_callback_array_appends_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	c_source := gen_c_from_source(v3_bin, 'local_callback_fixed_array_initializer_hot_loop_c',
		source)
	assert c_source.contains('closure__closure_try_destroy(__array_closure_'), c_source
	out := run_good(v3_bin, 'local_callback_fixed_array_initializer_hot_loop', source)
	assert out == '1250025000'
}

fn test_multi_variable_callback_declarations_preserve_receiver_identity_and_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	assert c_source.contains('.receiver = &(counter)'), c_source
	out := run_good(v3_bin, 'local_computed_key_callback_map_hot_loop', source)
	assert out == '1250025000'
}

fn test_scope_local_dynamic_callback_map_index_assignments_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Counter {
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
	c_source := gen_c_from_source(v3_bin, 'local_dynamic_callback_map_index_assign_hot_loop_c',
		source)
	assert c_source.contains('closure__closure_try_destroy(__map_val_'), c_source
	out := run_good(v3_bin, 'local_dynamic_callback_map_index_assign_hot_loop', source)
	assert out == '1250025000'
}

fn test_callback_map_initializer_captures_each_overwritten_entry_for_cleanup() {
	v3_bin := build_v3_review_transform()
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	assert c_source.contains('closure__closure_try_destroy(__immediate_closure_'), c_source
	out := run_good(v3_bin, 'immediate_bound_method_hot_loop', source)
	assert out == '1249975000'
}

fn test_disjoint_same_name_closure_bindings_are_reclaimed() {
	v3_bin := build_v3_review_transform()
	source := 'struct Value {
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
	source := 'struct Value {
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
',
		'`$res()` is not supported by the V3 arm64 backend')
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
',
		'`$res()` is not supported by the V3 eval backend')
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
',
		'`$res()` is not supported by the V3 wasm backend')
}

fn test_thread_handle_equality_uses_platform_comparison() {
	v3_bin := build_v3_review_transform()
	source := 'fn answer() int {
	return 42
}

fn main() {
	thread := spawn answer()
	copy := thread
	assert thread == copy
	assert !(thread != copy)
	println(int_str(thread.wait()))
}
'
	c_source := gen_c_from_source(v3_bin, 'thread_handle_equality_c', source)
	assert c_source.contains('pthread_equal(a.handle, b.handle) != 0'), c_source
	assert c_source.contains('return a.handle == b.handle;'), c_source
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
	source := 'struct Counter {
mut:
	value int
}

fn (mut counter Counter) next() int {
	counter.value++
	return counter.value
}
'
	run_bad(v3_bin, 'mut_method_value_stack_receiver_direct', source +
		'fn make() fn () int {
	mut counter := Counter{}
	return counter.next
}

fn main() {
	_ = make()
}
',
		'mutable local receiver cannot escape')
	run_bad(v3_bin, 'mut_method_value_stack_receiver_alias', source +
		'fn make() fn () int {
	mut counter := Counter{}
	callback := counter.next
	return callback
}

fn main() {
	_ = make()
}
',
		'mutable local receiver cannot escape')
	in_scope := run_good(v3_bin, 'mut_method_value_in_scope_borrows_receiver', source +
		'fn main() {
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
	callback := foo.read
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
	return foo.read
}

fn make_via_pointer_alias(value int) fn () int {
	foo := Foo{
		value: value
	}
	p := &foo
	cb := p.read
	return cb
}

fn make_from_pointer(foo &Foo) fn () int {
	return foo.read
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
	holder.callback = p.read
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
	source := 'struct Counter {
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
',
		'cannot modify constant `counter`')
}

fn test_mut_value_capture_in_call_under_selector_base() {
	v3_bin := build_v3_review_transform()
	// A `[mut s]` value capture used as a call argument nested inside a selector base
	// (`wrap(s).s.n`) must still be lowered to its value; the selector-base deref
	// suppression applies only to the direct receiver ident, not to nested expressions.
	out := run_good(v3_bin, 'mut_capture_selector_base',
		'struct S {\n\tn int\n}\n\nstruct Box {\n\ts S\n}\n\nfn wrap(s S) Box {\n\treturn Box{\n\t\ts: s\n\t}\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut s := S{\n\t\tn: 7\n\t}\n\tcall(fn [mut s] () {\n\t\tprintln(int_str(wrap(s).s.n))\n\t})\n}\n')
	assert out == '7'
}

fn test_mut_value_capture_parenthesized_selector_receiver() {
	v3_bin := build_v3_review_transform()
	// A `[mut s]` value capture is a `&S` local; a parenthesized direct receiver
	// (`(s).n`) is still the direct selector receiver and must keep the suppression so
	// the selector emits arrow access. Otherwise the inner `s` is auto-dereferenced to
	// `*s` while the selector still emits `->`, producing an invalid `(*s)->n`.
	out := run_good(v3_bin, 'mut_capture_paren_selector_base',
		'struct S {\n\tn int\n}\n\nfn call(cb fn ()) {\n\tcb()\n}\n\nfn main() {\n\tmut s := S{\n\t\tn: 7\n\t}\n\tcall(fn [mut s] () {\n\t\tprintln(int_str((s).n))\n\t})\n}\n')
	assert out == '7'
}

fn test_heap_escaping_amp_alias_keeps_heap_pointer() {
	v3_bin := build_v3_review_transform()
	// When a local `s` whose address escapes is moved to the heap, `s` becomes the `&S`
	// heap pointer and the alias `p := &s` must stay that pointer (`p := s`), NOT be
	// auto-dereferenced to `*s`. Over-dereferencing here initializes `p`'s `&S` decl from
	// an `S` value (a stale stack copy), reviving the escape/stale-mutation bug the heap
	// move avoids. A later `s = S{n: 2}` must be observable through the returned pointer.
	out := run_good(v3_bin, 'heap_escaping_amp_alias',
		'struct S {\n\tn int\n}\n\nfn leak() &S {\n\tmut s := S{\n\t\tn: 1\n\t}\n\tp := &s\n\ts = S{\n\t\tn: 2\n\t}\n\treturn p\n}\n\nfn main() {\n\tp := leak()\n\tprintln(int_str(p.n))\n}\n')
	assert out == '2'
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
	assert body.contains('Value* value = (Value*)memdup'), body
	out := run_good(v3_bin, 'returned_closure_alias_pointer_capture', source)
	assert out == '11\n21'
}

fn test_heap_escaping_amp_reassignment_moves_current_source() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'heap_escaping_amp_reassign_source',
		'fn make() &int {\n\tmut a := 10\n\tmut b := 20\n\tmut p := &a\n\tp = &b\n\treturn p\n}\n\nfn main() {\n\tprintln(int_str(*make()))\n}\n')
	assert out == '20'
	c_source := gen_c_from_source(v3_bin, 'heap_escaping_amp_reassign_source_c',
		'fn make() &int {\n\tmut a := 10\n\tmut b := 20\n\tmut p := &a\n\tp = &b\n\treturn p\n}\n\nfn main() {\n\t_ := make()\n}\n')
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
	c_source := gen_c_from_source(v3_bin, 'map_index_selector_write_retains_local_address_c',
		source)
	body := c_fn_body(c_source, 'map make_cache(void) {')
	assert body.contains('memdup'), body
	out := run_good(v3_bin, 'map_index_selector_write_retains_local_address', source)
	assert out == '9'
}

fn test_nested_generic_call_preserves_mut_pointer_param_rvalue() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_generic_mut_pointer_param_rvalue', 'struct Item {
	value int
}

fn identity[U](value U) U {
	return value
}

fn keep[T](mut current &T) &T {
	return identity(current)
}

fn main() {
	item := Item{
		value: 17
	}
	mut current := &item
	kept := keep[Item](mut current)
	println((kept == current).str())
	println(int_str(kept.value))
}
')
	assert out == 'true\n17'
}

fn test_return_address_of_pointer_backed_field_preserves_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'return_pointer_backed_field_address',
		'struct Node[T] {\nmut:\n\tvalue T\n}\n\nstruct List[T] {\nmut:\n\ttail &Node[T] = unsafe { nil }\n}\n\nfn (list &List[T]) last() &T {\n\treturn &list.tail.value\n}\n\nfn main() {\n\tmut node := &Node[int]{\n\t\tvalue: 1\n\t}\n\tlist := List[int]{\n\t\ttail: node\n\t}\n\tmut last := list.last()\n\t*last = 9\n\tprintln(int_str(node.value))\n}\n')
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
	out := run_good(v3_bin, 'result_multi_return_match_branch_unwrap',
		"enum Kind {\n\tleft\n\tright\n}\n\nfn left_pair() !(int, string) {\n\treturn 1, 'left'\n}\n\nfn right_pair() !(int, string) {\n\treturn 2, 'right'\n}\n\nfn choose(kind Kind) !(int, string) {\n\treturn match kind {\n\t\t.left {\n\t\t\tleft_pair()!\n\t\t}\n\t\t.right {\n\t\t\tright_pair()!\n\t\t}\n\t}\n}\n\nfn main() {\n\tn, label := choose(.right)!\n\tprintln(int_str(n) + ':' + label)\n}\n")
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
	out := run_good_with_flags(v3_bin, 'string_to_owned_ownership_cgen', '-ownership',
		"fn main() {\n\tname := 'owned'.to_owned()\n\tcopy := name.to_owned()\n\tprintln(copy)\n}\n")
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
	out := run_good_with_flags(v3_bin, 'owned_fn_literal_capture_context', '-ownership -gc none',
		source)
	assert out == '95'
}

fn test_generic_interface_method_body_marks_log_debug_dispatch() {
	v3_bin := build_v3_review_transform_ownership()
	out := run_good_with_flags(v3_bin, 'generic_interface_log_debug_dispatch', '-ownership',
		"import log\n\ninterface Sink {\n\tbinary_data()\n}\n\nstruct Box[T] {}\n\nfn (mut b Box[T]) binary_data() {\n\t_ = b\n\tlog.debug('hidden')\n}\n\nstruct Runner {}\n\nfn (mut r Runner) run(mut s Sink) {\n\t_ = r\n\ts.binary_data()\n}\n\nstruct Worker {\nmut:\n\trunner Runner\n}\n\nfn main() {\n\tmut worker := Worker{\n\t\trunner: Runner{}\n\t}\n\tmut b := Box[int]{}\n\tworker.runner.run(mut b)\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_specialized_generic_body_sees_materialized_interface_implementer() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_body_materialized_interface_implementer', 'interface Any {}

struct Box[T] {
	value T
}

fn render[T](value T) string {
	boxed := Any(value)
	return boxed.type_name() + ":" + boxed.str()
}

fn main() {
	println(render[Box[int]](Box[int]{
		value: 7
	}))
}
')
	assert out == 'Box[int]:Any(Box[int]{\n    value: 7\n})'
}

fn test_array_literal_separator_handling() {
	v3_bin := build_v3_review_transform()
	// Comma-, newline-, and blank-line-separated element lists parse with the
	// expected length. This parser is permissive (it never hard-errors on
	// malformed input), so the guarantee here is that a missing separator
	// (`[9 10]`) or a repeated comma (`[11,,12]`) is *not* merged/collapsed into
	// extra elements: only the leading element is kept, never `len == 2`.
	out := run_good(v3_bin, 'array_literal_separators',
		'const nl = [\n\t1\n\t2\n\t3\n]\nconst blank = [\n\t4\n\n\t5\n]\n\nfn main() {\n\tcommas := [6, 7, 8]\n\tmissing := [9 10]\n\tdoubled := [11,,12]\n\tprintln(int_str(nl.len) + ":" + int_str(blank.len) + ":" + int_str(commas.len) + ":" + int_str(missing.len) + ":" + int_str(doubled.len))\n}\n')
	assert out == '3:2:3:1:1'
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
	out := run_good(v3_bin, 'nested_map_semantic_equality',
		"struct Item {\n\tname string\n\tparts []string\n}\n\nstruct Holder {\n\titems map[string][]Item\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tmut left_map := map[string][]Item{}\n\tleft_map['items'] = [Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}]\n\tmut right_map := map[string][]Item{}\n\tright_map['items'] = [Item{\n\t\tname: join('a', 'b')\n\t\tparts: [join('x', 'y')]\n\t}]\n\tleft_arr := [left_map]\n\tright_arr := [right_map]\n\tleft_holder := Holder{\n\t\titems: left_map\n\t}\n\tright_holder := Holder{\n\t\titems: right_map\n\t}\n\tprintln(left_arr == right_arr)\n\tprintln(left_holder == right_holder)\n}\n")
	assert out == 'true\ntrue'
}

fn test_pointer_array_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_array_equality',
		'struct Node {\n\tvalue int\n}\n\nfn main() {\n\tleft_node := Node{\n\t\tvalue: 5\n\t}\n\tright_node := Node{\n\t\tvalue: 5\n\t}\n\tleft_ptr := &left_node\n\tright_ptr := &right_node\n\tleft := [left_ptr]\n\tright := [right_ptr]\n\tsame := [left_ptr]\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == same)\n\tprintln(right_ptr in left)\n\tprintln(int_str(left.index(right_ptr)))\n}\n')
	assert out == 'false\ntrue\ntrue\nfalse\n-1'
}

fn test_struct_pointer_equality_is_semantic() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_pointer_semantic_equality',
		"struct Person {\n\tname string\n\ttags []string\n}\n\nfn main() {\n\tleft := &Person{\n\t\tname: 'abc'.clone()\n\t\ttags: ['x'.clone()]\n\t}\n\tright := &Person{\n\t\tname: ('a' + 'bc')\n\t\ttags: [('x' + '')]\n\t}\n\tsame := left\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == same)\n}\n")
	assert out == 'true\nfalse\ntrue'
}

fn test_multilevel_struct_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'multilevel_struct_pointer_identity_equality',
		"struct Person {\n\tname string\n}\n\nfn main() {\n\tmut left := &Person{\n\t\tname: 'same'\n\t}\n\tmut right := &Person{\n\t\tname: 'same'\n\t}\n\tleft_slot := &left\n\tright_slot := &right\n\tsame_slot := left_slot\n\tprintln(left_slot == right_slot)\n\tprintln(left_slot != right_slot)\n\tprintln(left_slot == same_slot)\n\tprintln(*left_slot == *right_slot)\n}\n")
	assert out == 'false\ntrue\ntrue\ntrue'
}

fn test_struct_equality_with_interface_field_compiles() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_eq_interface_field',
		"interface Thing {\n\tvalue() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) value() int {\n\treturn i.n\n}\n\nstruct Box {\n\tthing Thing\n\tlabel string\n}\n\nfn main() {\n\titem := Item{\n\t\tn: 7\n\t}\n\tleft := Box{\n\t\tthing: item\n\t\tlabel: 'same'\n\t}\n\tright := left\n\tprintln(left == right)\n}\n")
	assert out == 'true'
}

fn test_array_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_pointer_equality',
		'fn main() {\n\tleft := [1, 2]\n\tright := [1, 2]\n\tleft_ptr := &left\n\tright_ptr := &right\n\tsame_ptr := left_ptr\n\tprintln(left_ptr == right_ptr)\n\tprintln(left_ptr != right_ptr)\n\tprintln(left_ptr == same_ptr)\n\tprintln(*left_ptr == *right_ptr)\n}\n')
	assert out == 'false\ntrue\ntrue\ntrue'
}

fn test_pointer_u8_array_bytestr_stays_in_cgen() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_u8_array_bytestr',
		'fn show(data &[]u8) string {\n\treturn data.bytestr()\n}\n\nfn main() {\n\tbytes := [u8(104), u8(105)]\n\tprintln(show(&bytes))\n}\n')
	assert out == 'hi'
}

fn test_map_pointer_equality_uses_pointer_identity() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_pointer_equality',
		"fn main() {\n\tleft := {\n\t\t'x': 1\n\t}\n\tright := {\n\t\t'x': 1\n\t}\n\tleft_ptr := &left\n\tright_ptr := &right\n\tsame_ptr := left_ptr\n\tprintln(left_ptr == right_ptr)\n\tprintln(left_ptr != right_ptr)\n\tprintln(left_ptr == same_ptr)\n\tprintln(*left_ptr == *right_ptr)\n}\n")
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
	out := run_good(v3_bin, 'fixed_array_semantic_equality',
		"fn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tleft := [[1]string{init: 'ab'.clone()}]\n\tright := [[1]string{init: join('a', 'b')}]\n\tmut map_left := map[string][1]string{}\n\tmap_left['k'] = [1]string{init: 'cd'.clone()}\n\tmut map_right := map[string][1]string{}\n\tmap_right['k'] = [1]string{init: join('c', 'd')}\n\tmut ints_left := map[string][2]i64{}\n\tints_left['k'] = [i64(1), i64(0)]!\n\tmut ints_right := map[string][2]i64{}\n\tints_right['k'] = [i64(2), i64(0)]!\n\tprintln(left == right)\n\tprintln(left.equals(right))\n\tprintln(map_left == map_right)\n\tprintln(ints_left == ints_right)\n}\n")
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
	out := run_good(v3_bin, 'interface_repeat_side_effects',
		'interface Thing {\n\tvalue() int\n}\n\nstruct Item {\n\tn int\n}\n\nfn (i Item) value() int {\n\treturn i.n\n}\n\n__global calls int\n\nfn make_item() Thing {\n\tcalls++\n\treturn Item{\n\t\tn: calls\n\t}\n}\n\nfn main() {\n\titems := [make_item()].repeat(3)\n\tprintln(int_str(calls))\n\tprintln(int_str(items[0].value() + items[1].value() + items[2].value()))\n}\n')
	assert out == '1\n3'
}

fn test_negative_is_return_smartcasts_following_statements() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'negative_is_return_smartcast',
		'struct MapKind {\n\tkey_type int\n\tvalue_type int\n}\nstruct OtherKind {}\ntype Kind = MapKind | OtherKind\n\nfn passthrough(k Kind) Kind {\n\treturn k\n}\n\nfn score(k Kind) int {\n\tclean := passthrough(k)\n\tif clean !is MapKind {\n\t\treturn 0\n\t}\n\treturn clean.key_type + clean.value_type\n}\n\nfn main() {\n\tprintln(int_str(score(Kind(MapKind{\n\t\tkey_type: 2\n\t\tvalue_type: 5\n\t}))))\n\tprintln(int_str(score(Kind(OtherKind{}))))\n}\n')
	assert out == '7\n0'
}

fn test_if_expr_smartcast_selector_decl_does_not_smartcast_local() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'if_expr_selector_decl_smartcast_local',
		'struct Cat {\n\tage int\n}\nstruct Dog {\n\ttricks int\n}\ntype Animal = Cat | Dog\n\nstruct Ident {\n\tobj Animal\n}\n\nfn has_age(cat Cat) bool {\n\treturn cat.age == 3\n}\n\nfn main() {\n\tleft := Ident{\n\t\tobj: Animal(Cat{\n\t\t\tage: 2\n\t\t})\n\t}\n\tmut obj := if left.obj is Cat {\n\t\tleft.obj\n\t} else {\n\t\tCat{}\n\t}\n\tif true {\n\t\tobj = Cat{\n\t\t\tage: 3\n\t\t}\n\t}\n\tprintln(has_age(obj))\n}\n')
	assert out == 'true'
}

fn test_comptime_type_conditions_handle_logical_ops() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_type_condition_logical_ops',
		"fn classify[T](x T) int {\n\t_ := x\n\t\$if T !is string && T !is \$int && T !is []u8 {\n\t\treturn 1\n\t} \$else {\n\t\treturn 2\n\t}\n\treturn 0\n}\n\nfn grouped[T](x T) int {\n\t_ := x\n\t\$if (T is int || T is string) && T is bool {\n\t\treturn 1\n\t} \$else {\n\t\treturn 2\n\t}\n\treturn 0\n}\n\nfn main() {\n\tprintln(int_str(classify('abc')))\n\tprintln(int_str(classify(3)))\n\tprintln(int_str(classify([u8(1)])))\n\tprintln(int_str(classify(1.5)))\n\tprintln(int_str(grouped(3)))\n\tprintln(int_str(grouped('abc')))\n}\n")
	assert out == '2\n2\n2\n1\n2\n2'
}

fn test_comptime_type_conditions_keep_prefix_types_compact() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'comptime_type_condition_prefix_types',
		'struct Foo {}\n\nfn main() {\n\t\$if ?int is ?int {\n\t\tprintln("opt")\n\t} \$else {\n\t\tprintln("badopt")\n\t}\n\t\$if !Foo is !Foo {\n\t\tprintln("res")\n\t} \$else {\n\t\tprintln("badres")\n\t}\n}\n')
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
		'probe/probe.v': 'module probe\n\npub fn depth[T](value T) int {\n\t_ = value\n\t$if T.indirections == 0 {\n\t\treturn 0\n\t} $else $if T.indirections == 1 {\n\t\treturn 1\n\t}\n\treturn 2\n}\n'
		'main.v':        'module main\n\nimport probe\n\nfn main() {\n\tn := 7\n\tprintln(int_str(probe.depth(n)))\n\tprintln(int_str(probe.depth(&n)))\n}\n'
	}, 'main.v')
	assert out == '0\n1'
}

fn test_nested_comptime_field_names_do_not_replace_each_other() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'nested_comptime_field_name_prefixes',
		'struct Embedded {\n\tn int\n}\n\nstruct Item {\n\tEmbedded\n\tname string\n}\n\nfn normal_fields[T]() int {\n\tmut count := 0\n\t$for field in T.fields {\n\t\t$if field.is_embed {\n\t\t\t$for reserved_field in T.fields {\n\t\t\t\t$if !reserved_field.is_embed {\n\t\t\t\t\tcount++\n\t\t\t\t}\n\t\t\t}\n\t\t}\n\t}\n\treturn count\n}\n\nfn main() {\n\tprintln(int_str(normal_fields[Item]()))\n}\n')
	assert out == '1'
}

fn test_struct_equality_compares_pointer_fields_as_pointers() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'struct_eq_pointer_field',
		'struct Node {\n\tvalue int\n\tnext &Node\n}\n\nfn main() {\n\tleft := Node{\n\t\tvalue: 7\n\t\tnext: unsafe { nil }\n\t}\n\tright := Node{\n\t\tvalue: 7\n\t\tnext: unsafe { nil }\n\t}\n\tprintln([left] == [right])\n}\n')
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
	os.write_file(os.join_path(root, 'tar', 'reader.v'),
		'module tar /* implementation module */\n\nfn reader_value() string {\n\treturn "reader"\n}\n') or {
		panic(err)
	}
	test_file := os.join_path(root, 'tar', 'reader_test.v')
	os.write_file(test_file,
		'@[has_globals]\n/* block comment before module */\nmodule tar // test module\n\nfn test_reader_value() {\n\tprintln(reader_value())\n}\n') or {
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
	out := run_good(v3_bin, 'delete_last_preserves_shared_slice_buffer',
		"fn main() {\n\tmut a := [1, 2, 3, 4]\n\tb := unsafe { a[..a.len] }\n\told_data := a.data\n\ta.delete_last()\n\tassert a == [1, 2, 3]\n\tassert b == [1, 2, 3, 4]\n\tassert a.data != old_data\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_slice_element_assignment_writes_through() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'slice_element_assignment_writes_through',
		"fn main() {\n\tmut a := [1, 2, 3, 4]\n\tmut s := unsafe { a[1..3] }\n\ts[0] = 42\n\ts[1] += 5\n\tassert a == [1, 42, 8, 4]\n\tassert s == [42, 8]\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_string_pointer_comparisons_keep_pointer_semantics() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'string_pointer_comparison',
		"fn main() {\n\tleft := 'same'.clone()\n\tright := 'same'.clone()\n\tpleft := &left\n\tpright := &right\n\tprintln(pleft == pright)\n\tprintln(pleft != pright)\n\tprintln(*pleft == *pright)\n}\n")
	assert out == 'false\ntrue\ntrue'
}

fn test_map_keys_and_values_lower_to_runtime_methods() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_keys_values_lowering',
		"fn make_lookup() map[string]int {\n\treturn {\n\t\t'one': 1\n\t\t'two': 2\n\t}\n}\n\nfn main() {\n\tlookup := make_lookup()\n\tkeys := lookup.keys()\n\tvalues := make_lookup().values()\n\tmut total := 0\n\tfor value in values {\n\t\ttotal += value\n\t}\n\tsingle := {\n\t\t'only': 9\n\t}\n\tprintln(int_str(keys.len))\n\tprintln(int_str(values.len))\n\tprintln(int_str(total))\n\tprintln(int_str(single.keys().len))\n\tprintln(single.keys()[0])\n\tprintln(int_str(single.values().len))\n\tprintln(int_str(single.values()[0]))\n}\n")
	assert out == '2\n2\n3\n1\nonly\n1\n9'
}

fn test_map_str_preserves_signed_wide_entries() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_str_signed_wide_entries',
		"fn main() {\n\tvalue_map := {\n\t\t'x': i64(5000000000)\n\t}\n\tkey_map := {\n\t\ti64(-5000000000): 'x'\n\t}\n\tprintln(value_map.str())\n\tprintln(key_map.str())\n}\n")
	assert out == "{'x': 5000000000}\n{-5000000000: 'x'}"
}

fn test_map_str_normalizes_alias_key_and_value_types() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_str_alias_kinds',
		"type ID = int\n\ntype Amount = f64\n\nfn main() {\n\tids := {\n\t\tID(23): 'id'\n\t}\n\tamounts := {\n\t\t'price': Amount(1.25)\n\t}\n\tprintln('\${ids}')\n\tprintln('\${amounts}')\n}\n")
	assert out == "{23: 'id'}\n{'price': 1.25}"
}

fn test_chained_array_alias_stringification_uses_outer_alias_only() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'chained_array_alias_str',
		"type A = []int\n\ntype B = A\n\nfn main() {\n\tvalue := B([1, 2])\n\tprintln('\${value}')\n}\n")
	assert out == 'B([1, 2])'
}

fn test_alias_pointer_receiver_str_gets_addressable_value() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'alias_pointer_receiver_str',
		"type Number = int\n\nfn (n &Number) str() string {\n\treturn 'number:' + int_str(int(*n))\n}\n\nstruct Point {\n\tx int\n}\n\ntype NamedPoint = Point\n\nfn (p &NamedPoint) str() string {\n\treturn 'point:' + int_str(p.x)\n}\n\nfn main() {\n\tn := Number(7)\n\tp := NamedPoint(Point{\n\t\tx: 9\n\t})\n\tprintln('\${n}')\n\tprintln('\${Number(8)}')\n\tprintln('\${p}')\n\tprintln('\${NamedPoint(Point{x: 10})}')\n}\n")
	assert out == 'number:7\nnumber:8\npoint:9\npoint:10'
}

fn test_mut_map_param_interpolation_preserves_pointer() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'mut_map_param_interpolation',
		"type Scores = map[string]int\n\nfn show(mut m map[string]int) {\n\tprintln('\${m}')\n}\n\nfn show_alias(mut m Scores) {\n\tprintln('\${m}')\n}\n\nfn main() {\n\tmut m := map[string]int{}\n\tm['x'] = 3\n\tshow(mut m)\n\tmut scores := Scores(map[string]int{})\n\tscores['y'] = 4\n\tshow_alias(mut scores)\n}\n")
	assert out == "{'x': 3}\n{'y': 4}"
}

fn test_map_literal_stringification_evaluates_entries_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_literal_str_side_effects',
		"__global key_calls int\n__global val_calls int\n\nfn next_key() string {\n\tkey_calls++\n\treturn 'k' + int_str(key_calls)\n}\n\nfn next_val() int {\n\tval_calls++\n\treturn val_calls * 10\n}\n\nfn main() {\n\tprintln({\n\t\tnext_key(): next_val()\n\t})\n\tprintln(int_str(key_calls) + ',' + int_str(val_calls))\n}\n")
	assert out == "{'k1': 10}\n1,1"
}

fn test_map_literal_declaration_evaluates_entries_once() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'map_literal_decl_side_effects',
		"__global key_calls int\n__global val_calls int\n\nfn next_key() string {\n\tkey_calls++\n\treturn 'key'\n}\n\nfn next_val() int {\n\tval_calls++\n\treturn val_calls * 10\n}\n\nfn main() {\n\tvalues := {\n\t\tnext_key(): next_val()\n\t}\n\tprintln(int_str(values['key']))\n\tprintln(int_str(key_calls) + ',' + int_str(val_calls))\n}\n")
	assert out == '10\n1,1'
}

fn test_fn_literal_preserves_mut_param_string_interpolation() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'fn_literal_mut_param_interp',
		"fn show(mut x int) {\n\t_ := fn () {}\n\tprintln('\${x}')\n}\n\nfn main() {\n\tmut n := 42\n\tshow(mut n)\n}\n")
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
	out := run_good(v3_bin, 'runes_iterator_index_scope',
		"fn main() {\n\tfor i, r in 'ab'.runes_iterator() {\n\t\t_ := r\n\t\tprintln(int_str(i))\n\t}\n\ti := 9\n\tprintln(int_str(i))\n}\n")
	assert out == '0\n1\n9'
}

fn test_array_last_index_uses_element_width() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_last_index_element_width',
		'fn main() {\n\twide := [i64(1), i64(5000000000), i64(2), i64(5000000000)]\n\tfloats := [1.25, 2.5, 1.25]\n\tflags := [true, false, true]\n\tprintln(int_str(wide.last_index(i64(5000000000))))\n\tprintln(int_str(floats.last_index(1.25)))\n\tprintln(int_str(flags.last_index(true)))\n}\n')
	assert out == '3\n2\n2'
}

fn test_array_last_index_uses_semantic_element_comparison() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'array_last_index_semantic_equality',
		"struct Item {\n\tname string\n\tparts []string\n}\n\nfn join(a string, b string) string {\n\treturn a + b\n}\n\nfn main() {\n\tnested := [['ab'.clone()], [join('x', 'y')], [join('a', 'b')]]\n\tnested_needle := ['ab'.clone()]\n\titems := [Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}, Item{\n\t\tname: join('a', 'b')\n\t\tparts: [join('x', 'y')]\n\t}]\n\tneedle := Item{\n\t\tname: 'ab'.clone()\n\t\tparts: ['xy'.clone()]\n\t}\n\tprintln(int_str(nested.last_index(nested_needle)))\n\tprintln(int_str(items.last_index(needle)))\n}\n")
	assert out == '2\n1'
}

fn test_generic_string_literal_matching_typeof_marker_is_preserved() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'generic_marker_string_literal',
		"fn marker_and_type[T](value T) string {\n\tmarker := '__v3_generic_type_name:T'\n\treturn marker + '|' + typeof(value).name\n}\n\nfn main() {\n\tprintln(marker_and_type(7))\n}\n")
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
	out := run_good(v3_bin, 'optional_string_semantic_equality',
		"fn maybe_text(ok bool) ?string {\n\tif !ok {\n\t\treturn none\n\t}\n\tprefix := 'a'.clone()\n\treturn prefix + 'b'\n}\n\nfn main() {\n\tleft := maybe_text(true)\n\tright := maybe_text(true)\n\tmissing_left := maybe_text(false)\n\tmissing_right := maybe_text(false)\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == missing_left)\n\tprintln(missing_left == missing_right)\n\tprintln(missing_left != missing_right)\n}\n")
	assert out == 'true\nfalse\nfalse\ntrue\nfalse'
}

fn test_optional_nested_array_equality_guards_payload_work() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_nested_array_guarded_equality',
		"fn maybe_nested(ok bool) ?[][]string {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [['a'.clone()], ['b'.clone()]]\n}\n\nfn main() {\n\tleft := maybe_nested(true)\n\tright := maybe_nested(true)\n\tmissing_left := maybe_nested(false)\n\tmissing_right := maybe_nested(false)\n\tprintln(left == right)\n\tprintln(left != right)\n\tprintln(left == missing_left)\n\tprintln(missing_left == missing_right)\n\tprintln(missing_left != missing_right)\n}\n")
	assert out == 'true\nfalse\nfalse\ntrue\nfalse'
}

fn test_optional_assignment_invalidates_payload_smartcast() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_assignment_invalidates_payload_smartcast',
		'fn main() {\n\tmut value := ?int(none)\n\tvalue = 1\n\tvalue = none\n\tresolved := value or { 42 }\n\tprintln(int_str(resolved))\n}\n')
	assert out == '42'
}

fn test_unannotated_optional_address_preserves_wrapper() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'unannotated_optional_wrapper_address',
		'fn main() {\n\tmut maybe := ?int(7)\n\twrapper := &maybe\n\tprintln(typeof(wrapper).name)\n\t*wrapper = none\n\tvalue := maybe or { 42 }\n\tprintln(int_str(value))\n}\n')
	assert out == '&?int\n42'
}

fn test_pointer_alias_lvalue_preserves_dereference() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'pointer_alias_lvalue_dereference',
		'type IntPtr = &int\n\nfn main() {\n\tmut value := 0\n\tmut p := IntPtr(&value)\n\t*p = 7\n\tprintln(int_str(value))\n}\n')
	assert out == '7'
}

fn test_optional_variant_to_optional_sum_cast_preserves_wrapper() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'optional_variant_to_optional_sum_cast',
		"struct Cat {}\n\nstruct Dog {\n\tname string\n}\n\ntype Animal = Cat | Dog\n\nfn maybe_dog(ok bool) ?Dog {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn Dog{\n\t\tname: 'Rex'\n\t}\n}\n\nfn show(ok bool) string {\n\tmaybe_animal := ?Animal(maybe_dog(ok))\n\tanimal := maybe_animal or { return 'missing' }\n\tif animal is Dog {\n\t\treturn animal.name\n\t}\n\treturn 'cat'\n}\n\nfn main() {\n\tprintln(show(true))\n\tprintln(show(false))\n}\n")
	assert out == 'Rex\nmissing'
}

fn test_wrapped_plus_minus_continuations_consume_auto_semicolon() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'wrapped_plus_minus_continuation',
		'fn add(total int, delta int) int {\n\treturn total\n\t\t+ delta\n}\n\nfn sub(total int, delta int) int {\n\treturn total\n\t\t- delta\n}\n\nfn main() {\n\tprintln(int_str(add(3, 4)))\n\tprintln(int_str(sub(9, 2)))\n}\n')
	assert out == '7\n7'
}

fn test_gated_optional_array_index_materializes_base_before_wrap() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'gated_optional_array_index_base_order',
		"fn get_arr(ok bool) ?[]int {\n\tprintln('get')\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [3, 7, 11]\n}\n\nfn main() {\n\tprintln(int_str(get_arr(true)#[-1] or { 40 }))\n\tprintln(int_str(get_arr(true)#[9] or { 41 }))\n\tprintln(int_str(get_arr(false)#[-1] or { 42 }))\n}\n")
	assert out == 'get\n11\nget\n41\nget\n42'
}

fn test_normalized_option_result_fixed_array_names_keep_outer_wrapper() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'normalized_option_result_fixed_array',
		"struct Foo {\n\tn int\n}\n\nfn opt_values(ok bool) ?[2]int {\n\tif !ok {\n\t\treturn none\n\t}\n\treturn [1, 2]!\n}\n\nfn res_values(ok bool) ![2]Foo {\n\tif !ok {\n\t\treturn error('x')\n\t}\n\treturn [Foo{\n\t\tn: 3\n\t}, Foo{\n\t\tn: 4\n\t}]!\n}\n\nfn main() {\n\ta := opt_values(true) or { [0, 0]! }\n\tb := res_values(true) or { [Foo{\n\t\tn: 0\n\t}, Foo{\n\t\tn: 0\n\t}]! }\n\tmissing_a := opt_values(false) or { [5, 6]! }\n\tmissing_b := res_values(false) or { [Foo{\n\t\tn: 7\n\t}, Foo{\n\t\tn: 8\n\t}]! }\n\tprintln(int_str(a[0] + a[1] + b[0].n + b[1].n))\n\tprintln(int_str(missing_a[0] + missing_a[1] + missing_b[0].n + missing_b[1].n))\n}\n")
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
	out := run_good(v3_bin, 'lowered_generic_operator_call_records_specialization',
		'struct Box[T] {\n\tv T\n}\n\nfn (a Box[T]) + (b Box[T]) Box[T] {\n\treturn Box[T]{\n\t\tv: a.v + b.v\n\t}\n}\n\nfn main() {\n\tleft := Box[int]{\n\t\tv: 2\n\t}\n\tright := Box[int]{\n\t\tv: 5\n\t}\n\tresult := left + right\n\tprintln(int_str(result.v))\n}\n')
	assert out == '7'
}

fn test_late_inferred_generic_call_emits_specialization() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'late_inferred_generic_call_emits_specialization',
		'fn make[T]() T {\n\treturn T(41)\n}\n\nfn use[T](value T) T {\n\treturn value + T(1)\n}\n\nfn main() {\n\tx := make[int]()\n\tprintln(int_str(use(x)))\n}\n')
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
	write_project_file(root, 'main.v',
		'module main\n\n#flag -I @VMODROOT/include\n#insert "flag_value.c"\n\nfn C.flag_value() int\n\nfn main() {\n\tprintln(int_str(C.flag_value()))\n}\n')
	write_project_file(root, 'include/flag_value.c',
		'#include <flag_value.h>\n\nstatic inline int flag_value(void) {\n\treturn flag_value_inner();\n}\n')
	write_project_file(root, 'include/flag_value.h',
		'static inline int flag_value_inner(void) {\n\treturn 57;\n}\n')
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
	os.write_file(inner_path,
		'#include <inttypes.h>\n#include <stdint.h>\ntypedef uint64_t ImportedWord;\n') or {
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
	out := run_good(v3_bin, 'statement_array_append_rhs_expression',
		'fn main() {\n\tmut value := u32(0x123)\n\tmut values := []u32{}\n\tvalues << value & 0xff\n\tprintln(int_str(int(values[0])))\n}\n')
	assert out == '35'
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
	out := run_good(v3_bin, 'optional_map_append_evaluation_order',
		'fn select_key(key string, mut trace string) string {\n\ttrace += "key"\n\treturn key\n}\n\nfn next_value(mut key string, mut trace string) ?int {\n\ttrace += "rhs"\n\tkey = "changed"\n\treturn 7\n}\n\nfn main() {\n\tmut trace := ""\n\tmut key := "original"\n\tmut values := map[string][]int{}\n\tvalues[select_key(key, mut trace)] << next_value(mut key, mut trace) or { return }\n\tprintln(trace)\n\tprintln(int_str(values["original"][0]))\n\tprintln("changed" in values)\n}\n')
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
	out := run_good(v3_bin, 'optional_shift_rhs_evaluated_once', 'fn next_value(mut calls int) ?int {
	calls++
	return 1
}

fn main() {
	mut calls := 0
	flags := 2
	flags << next_value(mut calls) or { return }
	println(int_str(calls))
}
')
	assert out == '1'
}

fn test_json2_skipped_pointer_field_does_not_specialize_decoder() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_skipped_pointer_field',
		'import gg\nimport x.json2\n\nstruct Config {\n\tcontext &gg.Context @[skip]\n\tname string\n}\n\nfn main() {\n\tconfig := json2.decode[Config]("{\\"name\\":\\"ok\\"}") or { Config{} }\n\tprintln(config.name)\n}\n')
	assert out == 'ok'
}

fn test_comptime_field_generic_calls_keep_resolved_field_types() {
	v3_bin := build_v3_review_transform()
	out := run_good_project(v3_bin, 'comptime_field_resolved_types', {
		'v.mod':         "Module { name: 'comptime_field_resolved_types' }\n"
		'model/model.v': 'module model\n\npub enum KeyCode {\n\tenter\n}\n\npub type Callback = fn (int)\n\n@[typedef]\npub struct C.model_event {\npub:\n\tkey KeyCode\n\tcb Callback\n}\n\npub type Event = C.model_event\n'
		'codec/codec.v': 'module codec\n\npub fn visit[T](mut value T) {\n\t$for field in T.fields {\n\t\ttouch(mut value.$(field.name))\n\t}\n}\n\nfn touch[T](mut value T) {\n\t_ = value\n}\n'
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
		'codec/codec.v': 'module codec\n\npub fn fill[T](mut value T) {\n\t$for field in T.fields {\n\t\t$if field.indirections == 1 {\n\t\t\tmut decoded_ptr := create_ptr(value.$(field.name))\n\t\t\tdecoded_ptr.value = 42\n\t\t\tvalue.$(field.name) = decoded_ptr\n\t\t}\n\t}\n}\n\nfn create_ptr[T](_ &T) &T {\n\treturn &T{}\n}\n'
		'main.v':        'module main\n\nimport codec\nimport model\n\nfn main() {\n\tmut context := model.Context{}\n\tcodec.fill(mut context)\n\tprintln(context.app.value)\n}\n'
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
	out := run_good(v3_bin, 'json2_c_alias_field_types',
		'import sokol.sapp\nimport x.json2\n\nfn main() {\n\tevent := json2.decode[sapp.Event]("{}") or { sapp.Event{} }\n\tprintln(int_str(int(event.frame_count)))\n}\n')
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

fn test_json2_encode_keeps_independent_array_element_specializations() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_independent_array_element_specializations',
		'import x.json2\n\nstruct Event {\n\tvalue int\n}\n\nstruct Payload {\n\tflags [][]bool\n\tevents []Event\n}\n\nfn main() {\n\tpayload := Payload{\n\t\tflags: [[true]]\n\t\tevents: [Event{\n\t\t\tvalue: 42\n\t\t}]\n\t}\n\tencoded := json2.encode(payload)\n\tprintln(json2.decode[Payload](encoded)!.events[0].value)\n}\n')
	assert out == '42'
}

fn test_json2_encode_array_keeps_main_type_with_imported_homonym() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_array_main_type_imported_homonym',
		'import gg\nimport x.json2\n\nstruct Event {\n\tvalue int\n}\n\nfn main() {\n\t_ = json2.encode([gg.Event{}])\n\tencoded := json2.encode([Event{\n\t\tvalue: 42\n\t}])\n\tprintln(encoded)\n}\n')
	assert out.contains('"value":42')
}

fn test_json2_decode_keeps_main_type_with_imported_homonym() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_decode_main_type_imported_homonym',
		'import gg\nimport x.json2\n\nstruct Event {\n\tvalue int\n}\n\nfn main() {\n\t_ = json2.decode[gg.Event]("{}") or { gg.Event{} }\n\tevent := json2.decode[Event](r\'{"value":42}\')!\n\tprintln(event.value)\n}\n')
	assert out == '42'
}

fn test_json2_encode_shared_struct_field_uses_locked_value_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_shared_struct_field_value',
		'import x.json2\n\nstruct State {\n\tvalue int\n}\n\nstruct Client {\n\tstate shared State\n}\n\nfn main() {\n\tclient := Client{}\n\tprintln(json2.encode(client))\n}\n')
	assert out == '{"state":{"value":0}}'
}

fn test_json2_decode_any_map_keeps_sum_value_type() {
	v3_bin := build_v3_review_transform()
	out := run_good(v3_bin, 'json2_any_map_value',
		'import x.json2\n\nfn main() {\n\tvalues := json2.decode[map[string]json2.Any](r\'{"ok":true}\')!\n\tprintln(values["ok"] or { json2.Any(false) })\n}\n')
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

fn test_array_filter_and_map_hoist_bound_method_callbacks() {
	v3_bin := build_v3_review_transform()
	source := 'struct Rule {
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

fn test_array_filter_and_map_reclaim_branch_selected_bound_methods() {
	v3_bin := build_v3_review_transform()
	source := 'struct Rule {
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
	source := 'struct Counter {
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
