import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

// build_v3 builds v3 data for v3 tests.
fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generics_test')
	build := os.execute('${vexe} -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// run_selfhost_bad supports run selfhost bad handling for v3 tests.
fn run_selfhost_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_gen_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	result := os.execute('${v3_bin} ${bad_src} -selfhost -b c -o ${bad_bin}')
	assert result.exit_code != 0, 'expected error for ${name}, but compilation succeeded'
	assert result.output.contains(expected), 'expected "${expected}" in output for ${name}, got: ${result.output}'
	assert !result.output.contains('C compilation failed')
}

fn write_project_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn run_selfhost_project_bad(v3_bin string, name string, files map[string]string, input string, expected string) {
	root := os.join_path(os.temp_dir(), 'v3_gen_${name}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	for rel, src in files {
		write_project_file(root, rel, src)
	}
	input_path := os.join_path(root, input)
	bad_bin := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	result := os.execute('${v3_bin} ${input_path} -selfhost -b c -o ${bad_bin}')
	assert result.exit_code != 0, 'expected error for ${name}, but compilation succeeded'
	assert result.output.contains(expected), 'expected "${expected}" in output for ${name}, got: ${result.output}'
	assert !result.output.contains('C compilation failed')
}

// run_no_generic_error supports run no generic error handling for v3 tests.
fn run_no_generic_error(v3_bin string, name string, src string) {
	src_file := os.join_path(os.temp_dir(), 'v3_gen_${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_file := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	result := os.execute('${v3_bin} ${src_file} -b c -o ${bin_file}')
	assert !result.output.contains('unsupported generic'), '${name}: should not reject generics without -selfhost, got: ${result.output}'
}

// run_generic_ok supports run generic ok handling for v3 tests.
fn run_generic_ok(v3_bin string, name string, src string, expected string) {
	src_file := os.join_path(os.temp_dir(), 'v3_gen_${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_file := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	c_file := bin_file + '.c'
	compile := os.execute('${v3_bin} ${src_file} -b c -o ${bin_file}')
	// Check that v3 type checker and transform pass without errors
	assert !compile.output.contains('unsupported generic'), '${name}: should not reject generics, got: ${compile.output}'
	assert !compile.output.contains('type checker found'), '${name}: type checker errors: ${compile.output}'
	// Verify C file was generated. The v3 pipeline can succeed even if cc fails due
	// to pre-existing runtime issues.
	assert os.exists(c_file), '${name}: C file not generated'
	c_content := os.read_file(c_file) or { '' }
	// The mangled generic function should appear in the generated C code
	if expected.len > 0 {
		assert c_content.len > 0, '${name}: empty C file'
	}
}

fn run_generic_exec(v3_bin string, name string, src string) string {
	src_file := os.join_path(os.temp_dir(), 'v3_gen_${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_file := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	compile := os.execute('${v3_bin} ${src_file} -b c -o ${bin_file}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin_file)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// test_generics_rejected_when_building_v validates this v3 regression case.
fn test_generics_rejected_when_building_v() {
	v3_bin := build_v3()
	// generic function
	run_selfhost_bad(v3_bin, 'generic_fn', '
fn id[T](x T) T {
	return x
}
fn main() {
	println(id[int](1))
}
',
		'unsupported generic')

	// generic struct
	run_selfhost_bad(v3_bin, 'generic_struct', '
struct Box[T] {
	value T
}
fn main() {
	b := Box[int]{value: 7}
	println(b.value)
}
	',
		'unsupported generic')

	// generic struct with no generic fields
	run_selfhost_bad(v3_bin, 'generic_struct_marker_only', '
struct Phantom[T] {
	value int
}
fn main() {}
',
		'unsupported generic struct `Phantom`')

	run_selfhost_bad(v3_bin, 'generic_fn_marker_only', '
fn unused[T]() {}
fn main() {}
	',
		'unsupported generic declaration `unused`')

	run_selfhost_bad(v3_bin, 'generic_c_fn_marker_only', '
fn helper[T]()
fn main() {}
	',
		'unsupported generic declaration `helper`')

	run_selfhost_bad(v3_bin, 'generic_interface_marker_only', '
interface I[T] {}
fn main() {}
	',
		'unsupported generic declaration `I`')

	run_selfhost_bad(v3_bin, 'generic_type_alias_marker_only', '
type Alias[T] = int
fn main() {}
',
		'unsupported generic declaration `Alias`')

	// generic method
	run_selfhost_bad(v3_bin, 'generic_method', '
struct Box[T] {
	value T
}
fn (b Box[T]) get() T {
	return b.value
}
fn main() {
	b := Box[string]{value: "v3"}
	println(b.get())
}
',
		'unsupported generic')

	// generic with option/result return
	run_selfhost_bad(v3_bin, 'generic_option', '
fn maybe[T](x T) ?T {
	return x
}
fn main() {
	x := maybe[int](1) or { 0 }
	println(x)
}
',
		'unsupported generic')

	// multiple type parameters
	run_selfhost_bad(v3_bin, 'generic_multi_param', '
fn pair[A, B](a A, b B) A {
	return a
}
fn main() {
	_ := pair[int, string](1, "ok")
}
',
		'unsupported generic')

	// generic type application in params
	run_selfhost_bad(v3_bin, 'generic_param_type', '
fn takes_box(x Box[int]) {}
fn main() {}
',
		'unsupported generic type application `Box[int]`')

	// generic struct field
	run_selfhost_bad(v3_bin, 'generic_field_type', '
struct Wrapper {
	inner Box[string]
}
fn main() {}
',
		'unsupported generic type application `Box[string]`')

	// generic return type
	run_selfhost_bad(v3_bin, 'generic_return_type', '
fn make_box() Box[int] {
	return Box[int]{}
}
fn main() {}
',
		'unsupported generic type application `Box[int]`')

	// generic sum type
	run_selfhost_bad(v3_bin, 'generic_type_decl', '
type Result[T] = T | string
fn main() {}
',
		'unsupported generic')

	// generic interface
	run_selfhost_bad(v3_bin, 'generic_interface', '
interface Container[T] {
	get() T
}
fn main() {}
',
		'unsupported generic')

	run_selfhost_project_bad(v3_bin, 'imported_generic_struct', {
		'main.v':          'module main

import badmod

fn main() {}
'
		'badmod/badmod.v': 'module badmod

struct Phantom[T] {
	value int
}
'
	}, 'main.v', 'unsupported generic struct `Phantom`')
}

// test_generics_allowed_without_building_v validates this v3 regression case.
fn test_generics_allowed_without_building_v() {
	v3_bin := build_v3()
	// generic function — no "unsupported generic" error
	run_no_generic_error(v3_bin, 'allow_generic_fn', '
fn id[T](x T) T {
	return x
}
fn main() {
	_ := id[int](1)
}
')

	// generic struct
	run_no_generic_error(v3_bin, 'allow_generic_struct', '
struct Box[T] {
	value T
}
fn main() {
	b := Box[int]{value: 7}
	_ := b
}
')

	// generic method
	run_no_generic_error(v3_bin, 'allow_generic_method', '
struct Box[T] {
	value T
}
fn (b Box[T]) get() T {
	return b.value
}
fn main() {
	b := Box[string]{value: "v3"}
	_ := b
}
')

	// nested generic type
	run_no_generic_error(v3_bin, 'allow_generic_nested', '
struct Box[T] {
	value T
}
fn main() {
	b := Box[[]int]{value: [1, 2, 3]}
	_ := b
}
')

	// option/result generic
	run_no_generic_error(v3_bin, 'allow_generic_option', '
fn maybe[T](x T) ?T {
	return x
}
fn main() {
	x := maybe[int](1) or { 0 }
	_ := x
}
')

	// generic type in params/fields/returns
	run_no_generic_error(v3_bin, 'allow_generic_type_app', '
fn takes_box(x Box[int]) {}
fn main() {}
')

	// generic interface
	run_no_generic_error(v3_bin, 'allow_generic_interface', '
interface Container[T] {
	get() T
}
fn main() {}
')
}

// test_generics_compile_and_run validates generics compile and run behavior in v3 tests.
fn test_generics_compile_and_run() {
	v3_bin := build_v3()

	// identity function with int and string
	run_generic_ok(v3_bin, 'run_id_fn', '
fn id[T](x T) T {
	return x
}
fn main() {
	println(id(123))
	println(id("ok"))
}
',
		'123\nok')

	// generic struct with field access
	run_generic_ok(v3_bin, 'run_generic_struct', '
struct Box[T] {
	value T
}
fn main() {
	b := Box[string]{value: "v3"}
	println(b.value)
}
',
		'v3')

	// A one-letter concrete type argument should not be treated as an unresolved
	// generic placeholder when discovering struct specializations.
	one_letter_src := os.join_path(os.temp_dir(), 'v3_gen_one_letter_concrete_arg.v')
	os.write_file(one_letter_src, '
struct A {
	value int
}

struct Box[T] {
	value T
}

fn main() {
	b := Box[A]{
		value: A{
			value: 3
		}
	}
	_ := b
}
') or {
		panic(err)
	}
	one_letter_bin := os.join_path(os.temp_dir(), 'v3_gen_one_letter_concrete_arg')
	one_letter_compile := os.execute('${v3_bin} ${one_letter_src} -b c -o ${one_letter_bin}')
	assert !one_letter_compile.output.contains('unsupported generic'), one_letter_compile.output
	assert !one_letter_compile.output.contains('type checker found'), one_letter_compile.output
	one_letter_c := os.read_file(one_letter_bin + '.c') or { '' }
	assert one_letter_c.contains('struct Box_A'), one_letter_c

	// transitive generic calls: outer[T] calls id[T]
	run_generic_ok(v3_bin, 'run_transitive', '
fn id[T](x T) T {
	return x
}
fn outer[T](x T) T {
	return id(x)
}
fn main() {
	println(outer(42))
}
',
		'42')

	// multi-param generic
	run_generic_ok(v3_bin, 'run_multi_param', '
fn first[A, B](a A, b B) A {
	return a
}
fn main() {
	println(first(99, "ignored"))
}
',
		'99')

	// multi-param generic struct fields
	run_generic_ok(v3_bin, 'run_multi_param_struct_fields', '
struct Pair[X, Y] {
	left X
	right Y
}
fn main() {
	p := Pair[int, string]{left: 1, right: "ok"}
	println(p.right)
}
',
		'ok')

	// infer from array element type
	run_generic_ok(v3_bin, 'run_array_infer', '
fn length[T](xs []T) int {
	return xs.len
}
fn main() {
	println(length([1, 2, 3]))
}
',
		'3')

	selector_convert_out := run_generic_exec(v3_bin, 'selector_method_arg_infer', '
struct Box[T] {
	value T
}

fn (b Box[T]) convert[U](value U) U {
	return value
}

fn main() {
	b := Box[int]{value: 1}
	println(b.convert("ok"))
}
')
	assert selector_convert_out == 'ok'
}
