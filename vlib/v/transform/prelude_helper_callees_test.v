module transform

import os
import v.parser
import v.pref
import v.types

// A generic body is lowered after markused. When its lowering calls a C helper of
// the cgen prelude, that call has to keep the V functions the helper calls; the
// map formatter calls `f64.str` even for a map without floats.
@[unsafe]
fn monomorphized_used_fns(name string, source string, used map[string]bool) map[string]bool {
	path := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transformed := transform_with_used(mut a, &tc, used)
	mono_used, errors, _ := monomorphize_with_used_checked_config_scoped_cached(mut a, &tc,
		transformed, false, unsafe { nil }, []MonomorphCacheSpec{}, true)
	assert errors.len == 0, errors.str()
	return mono_used
}

fn test_specialized_map_interpolation_keeps_the_map_formatter_callees() {
	used := monomorphized_used_fns('prelude_helper_map_str', 'struct Box[T] {
	value T
}

fn show[T](b Box[T]) string {
	mut lookup := map[string]Box[T]{}
	lookup["one"] = b
	return "\${lookup}"
}

fn main() {
	println(show(Box[int]{ value: 8 }))
}
',
		{
			'main': true
			'show': true
		})
	for name in ['f64.str', 'f64__str', 'i64.str', 'u64.str', 'rune.str'] {
		assert used[name], name
	}
}

fn test_specialized_fixed_float_interpolation_keeps_f64_str() {
	used := monomorphized_used_fns('prelude_helper_fixed_float', 'fn show[T](value T) string {
	return "\${value:.2f}"
}

fn main() {
	println(show(1.5))
}
',
		{
			'main': true
			'show': true
		})
	assert used['f64.str']
	assert used['f64__str']
}
