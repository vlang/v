module types

import os
import v.parser
import v.pref

fn generic_variadic_inference_checker(source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'generic_variadic_inference_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	return tc
}

fn test_variadic_parameters_supply_generic_inference_sources() {
	tc := generic_variadic_inference_checker('module main
fn accept[T](items ...T) {}
fn prefixed[T](name string, items ...T) {}
fn nested[T](items ...[]T) {}
fn main() {
	accept(1, 2)
	values := [1, 2]
	accept(...values)
	prefixed("values", "a", "b")
	nested([1], [2])
}
')
	assert tc.errors.len == 0, tc.errors.str()
}

fn test_variadic_parameters_do_not_bind_unmentioned_generic_types() {
	tc := generic_variadic_inference_checker('module main
fn accept[T, U](items ...T) {}
fn main() {
	accept(1)
}
')
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == 'could not infer generic type `U` in call to `accept`'
}
