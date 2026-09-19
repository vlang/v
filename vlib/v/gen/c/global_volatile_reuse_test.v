module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn generate_global_volatile_source(mut g FlatGen, name string, source string) string {
	source_path := os.join_path(os.temp_dir(), 'v3_global_volatile_${name}_${os.getpid()}.v')
	os.write_file(source_path, source) or { panic(err) }
	defer {
		os.rm(source_path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(source_path)
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used_fns := markused.mark_used(a, tc)
	return g.gen_with_used_options(a, used_fns, &tc, true)
}

// One FlatGen may generate more than one program. Every other per-program global
// map is cleared at the start of a generation; this one was not, so a volatile
// `main.slot` in the first program made an ordinary `main.slot` in the second one
// volatile too -- a qualifier appearing on a declaration whose source never asked
// for it.
fn test_volatile_global_markers_do_not_survive_into_the_next_generation() {
	mut g := FlatGen.new()

	volatile_c := generate_global_volatile_source(mut g, 'first', '__global volatile slot = u64(1)

fn main() {
	slot = slot + 1
}
')
	assert volatile_c.contains('volatile u64 slot'), volatile_c

	plain_c := generate_global_volatile_source(mut g, 'second', '__global slot = u64(1)

fn main() {
	slot = slot + 1
}
')
	assert plain_c.contains('u64 slot'), plain_c
	assert !plain_c.contains('volatile u64 slot'), plain_c
}
