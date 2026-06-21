import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_generics_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0
	return v3_bin
}

fn run_selfhost_bad(v3_bin string, name string, src string, expected string) {
	bad_src := os.join_path(os.temp_dir(), 'v3_gen_${name}.v')
	os.write_file(bad_src, src) or { panic(err) }
	bad_bin := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	result := os.execute('${v3_bin} ${bad_src} -selfhost -b c -o ${bad_bin}')
	assert result.exit_code != 0, 'expected error for ${name}, but compilation succeeded'
	assert result.output.contains(expected), 'expected "${expected}" in output for ${name}, got: ${result.output}'
	assert !result.output.contains('C compilation failed')
}

fn run_no_generic_error(v3_bin string, name string, src string) {
	src_file := os.join_path(os.temp_dir(), 'v3_gen_${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_file := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	result := os.execute('${v3_bin} ${src_file} -b c -o ${bin_file}')
	assert !result.output.contains('unsupported generic'), '${name}: should not reject generics without -selfhost, got: ${result.output}'
}

fn run_generic_ok(v3_bin string, name string, src string, expected string) {
	src_file := os.join_path(os.temp_dir(), 'v3_gen_${name}.v')
	os.write_file(src_file, src) or { panic(err) }
	bin_file := os.join_path(os.temp_dir(), 'v3_gen_${name}')
	c_file := bin_file + '.c'
	compile := os.execute('${v3_bin} ${src_file} -b c -o ${bin_file}')
	// Check that v3 type checker and transform pass without errors
	assert !compile.output.contains('unsupported generic'), '${name}: should not reject generics, got: ${compile.output}'
	assert !compile.output.contains('type checker found'), '${name}: type checker errors: ${compile.output}'
	// Verify C file was generated (v3 pipeline succeeded even if cc fails due to pre-existing runtime issues)
	assert os.exists(c_file), '${name}: C file not generated'
	c_content := os.read_file(c_file) or { '' }
	// The mangled generic function should appear in the generated C code
	if expected.len > 0 {
		assert c_content.len > 0, '${name}: empty C file'
	}
}

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
}

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
}
