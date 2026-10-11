module ssa

import os
import v.parser
import v.pref

fn test_native_c_variadic_declarations_preserve_fixed_argument_count() {
	path := os.join_path(os.vtmp_dir(), 'native_variadic_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	os.write_file(path, 'fn C.native_variadic(tag int, values ...voidptr) int
fn C.sprintf(&char, &char, ...voidptr) int
fn main() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	m := build(a)
	custom_functions := m.funcs.filter(it.name == 'C.native_variadic')
	assert custom_functions.len == 1
	custom := custom_functions[0]
	assert custom.is_variadic
	assert custom.variadic_start == 1
	for name, fixed in {
		'C.sprintf':  2
		'C.snprintf': 3
		'C.fprintf':  2
	} {
		functions := m.funcs.filter(it.name == name)
		assert functions.len > 0, name
		function := functions[0]
		assert function.is_variadic, name
		assert function.variadic_start == fixed, name
		if name == 'C.fprintf' {
			assert m.type_store.types[function.typ].kind == .int_t
			assert m.type_size(function.typ) == 4
		}
	}
}
