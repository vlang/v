module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn scalar_array_program(compile_defines []string) !string {
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
', compile_defines)!
}

fn scalar_array_generate(source string, compile_defines []string) !string {
	path := os.join_path(os.vtmp_dir(), 'scalar_array_store_${os.getpid()}.v')
	os.write_file(path, source)!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used, _ := markused.mark_all_used_with_generic_usage(a, tc, [])
	mut g := FlatGen.new()
	g.set_target(pref.target_from('linux', 'amd64') or { panic(err) })
	g.set_compile_defines(compile_defines)
	return g.gen_with_used_options(a, used, &tc, true)
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
	generated := scalar_array_program(['no_bounds_checking'])!
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
', [])!
	body := scalar_array_body(generated, 'scalar_callback__check')
	assert body.contains('scalar_callback__array_push('), body
	assert !body.contains('scalar_push_base'), body
}

fn scalar_array_body(generated string, name string) string {
	assert generated.contains('void ${name}('), 'missing generated function ${name}'
	return generated.all_after_last('void ${name}(').all_after('{').all_before('\n}').trim_space()
}
