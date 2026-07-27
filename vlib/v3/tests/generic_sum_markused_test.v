import os
import v3.markused
import v3.parser
import v3.pref
import v3.types

fn test_generic_sum_constructor_requires_monomorphization() {
	src := os.join_path(os.temp_dir(), 'v3_generic_sum_markused_test.v')
	os.write_file(src, '
type Name[T] = T | int | string

fn main() {
	_ := Name[int](123)
}
') or {
		panic(err)
	}

	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()

	_, uses_generics := markused.mark_used_with_generic_usage(a, &tc)
	assert uses_generics
}

fn test_inferred_generic_struct_init_requires_monomorphization() {
	src := os.join_path(os.temp_dir(), 'v3_inferred_generic_struct_markused_test.v')
	os.write_file(src, "
struct Foo[T, U] {
	a T
	b U
}

fn main() {
	_ := Foo{'two', 2}
}
") or {
		panic(err)
	}

	prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(src)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.diagnostic_files[src] = true
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()

	_, uses_generics := markused.mark_used_with_generic_usage(a, &tc)
	assert uses_generics
}
