module main

import os

fn testsuite_begin() {
	// The MSVC that a Windows job selects through VFLAGS cannot target Linux, and
	// this file generates Linux C; let those builds use the default compiler.
	if os.getenv('VFLAGS').contains('msvc') {
		os.unsetenv('VFLAGS')
	}
}

fn scalar_array_program(flags []string) !string {
	return scalar_array_generate('
type Scalar = i64

fn checked_store(mut a []i64, i int, value i64) {
	a[i] = value
}

fn append_scalar(mut a []i64, value i64) {
	a << value
}

fn append_alias(mut a []Scalar, value Scalar) {
	a << value
}

fn checked_string(mut a []string, i int, value string) {
	a[i] = value
}

fn append_string(mut a []string, value string) {
	a << value
}

fn checked_compound(mut a []i64, i int, value i64) {
	a[i] += value
}

fn main() {
	mut a := []i64{len: 2}
	checked_store(mut a, 0, 42)
	append_scalar(mut a, 43)
	append_alias(mut []Scalar{}, Scalar(44))
	checked_string(mut []string{len: 1}, 0, "a")
	append_string(mut []string{}, "a")
	checked_compound(mut a, 0, 1)
}
', flags)!
}

fn scalar_array_generate(source string, flags []string) !string {
	root := os.join_path(os.vtmp_dir(), 'scalar_array_store_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'input.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(path, source)!

	// V1-hosted unit tests get a shared V3 compiler from their runner.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE, '-new-compiler'] }
	args << ['-gc', 'none', '-os', 'linux', '-arch', 'amd64', '-no-skip-unused']
	args << flags
	args << ['-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	return os.read_file(c_path)!
}

fn test_checked_scalar_store_has_a_typed_success_path() {
	generated := scalar_array_program([])!
	store := scalar_array_body(generated, 'checked_store')
	assert store.contains('->data'), store
	assert store.contains('->len'), store
	assert store.contains('else { array__set('), store
	assert !store.contains('&(i64[]){'), store
	// Composite and compound-assignment lowering keeps its established helpers.
	assert scalar_array_body(generated, 'checked_string').contains('array__set(')
	assert scalar_array_body(generated, 'checked_compound').contains('array__set(')
}

fn test_scalar_append_has_a_typed_spare_capacity_path() {
	generated := scalar_array_program([])!
	for name in ['append_scalar', 'append_alias'] {
		body := scalar_array_body(generated, name)
		assert body.contains('->data'), body
		assert body.contains('->len <'), body
		assert body.contains('->cap'), body
		assert body.contains('ArrayFlags__is_slice'), body
		assert body.contains('else { array_push('), body
		assert !body.contains('[]){'), body
	}
	assert !scalar_array_body(generated, 'append_string').contains('->data')
}

fn test_scalar_store_respects_disabled_bounds_checking() {
	generated := scalar_array_program(['-d', 'no_bounds_checking'])!
	store := scalar_array_body(generated, 'checked_store')
	assert store.contains('->data'), store
	assert !store.contains('->len'), store
	assert !store.contains('array__set('), store
}

fn test_user_array_push_call_keeps_its_qualified_function() {
	generated := scalar_array_generate('
module scalar_callback

fn array_push(mut values []i64, value &i64) {
	values[0] = *value + 1000
}

pub fn check() {
	mut values := []i64{len: 1, cap: 4, init: 1}
	value := i64(42)
	array_push(mut values, &value)
}
', ['-shared'])!
	body := scalar_array_body(generated, 'scalar_callback__check')
	assert body.contains('scalar_callback__array_push('), body
	assert !body.contains('scalar_push_base'), body
}

fn test_scalar_append_outside_main_has_a_typed_spare_capacity_path() {
	generated := scalar_array_generate('
module scalar_library

pub fn append(mut values []i64, value i64) {
	values << value
}
', ['-shared'])!
	body := scalar_array_body(generated, 'scalar_library__append')
	assert body.contains('scalar_push_base'), body
	assert body.contains('->len <'), body
	assert body.contains('ArrayFlags__is_slice'), body
	assert body.contains('else { array_push('), body
	assert !body.contains('[]){'), body
}

fn test_user_generic_array_push_call_keeps_its_qualified_function() {
	generated := scalar_array_generate('
module scalar_generic

fn array_push[T](mut values []T, value &T) {
	values[0] = *value
}

pub fn check() {
	mut values := []i64{len: 1, cap: 4, init: 1}
	value := i64(42)
	array_push(mut values, &value)
}
', ['-shared'])!
	body := scalar_array_body(generated, 'scalar_generic__check')
	assert !body.contains('scalar_push_base'), body
}

fn test_global_array_push_callback_keeps_its_call() {
	generated := scalar_array_generate('
@[has_globals]
module scalar_global

__global array_push = fn (mut values []i64, value &i64) {
	values[0] = *value + 1000
}

pub fn check() {
	mut values := []i64{len: 1, cap: 4, init: 1}
	value := i64(42)
	array_push(mut values, &value)
}

pub fn append(mut values []i64, value i64) {
	values << value
}
', ['-shared'])!
	body := scalar_array_body(generated, 'scalar_global__check')
	assert body.contains('scalar_global__array_push('), body
	assert !body.contains('scalar_push_base'), body
	// An append lowered in that module is still an append, not a call of the global.
	appended := scalar_array_body(generated, 'scalar_global__append')
	assert appended.contains('scalar_push_base'), appended
	assert appended.contains('else { array_push('), appended
	assert !appended.contains('scalar_global__array_push('), appended
}

fn scalar_array_body(generated string, name string) string {
	assert generated.contains('void ${name}('), 'missing generated function ${name}'
	return generated.all_after_last('void ${name}(').all_after('{').all_before('\n}').trim_space()
}
