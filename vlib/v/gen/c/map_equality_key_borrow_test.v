module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn test_map_equality_borrows_keys_but_authored_iteration_keeps_clones() {
	path := os.join_path(os.vtmp_dir(), 'map_equality_key_borrow_${os.getpid()}.v')
	os.write_file(path, 'fn compare_maps(left map[string]f64, right map[string]f64) bool {
	return left == right
}
fn first_key(values map[string]int) string {
	for map_eq_key_0, _ in values { return map_eq_key_0 }
	return ""
}
struct Entry {
	value int
}
fn (left Entry) == (right Entry) bool {
	return left.value == right.value
}
fn compare_entries(left map[string][]Entry, right map[string][]Entry) bool {
	return left == right
}
fn main() {
	assert compare_maps({"one": f64(1)}, {"one": f64(1)})
	assert first_key({"one": 1}) == "one"
	assert compare_entries({"one": [Entry{1}]}, {"one": [Entry{1}]})
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used := markused.mark_used(a, tc)
	mut g := FlatGen.new()
	g.set_target(pref.target_from('linux', 'amd64') or { panic(err) })
	generated := g.gen_with_used_options(a, used, &tc, true)
	equality := generated.all_after_last('bool compare_maps(').all_after('{').all_before('\n}')
	assert equality.contains('map__exists'), equality
	assert !equality.contains('string__clone'), equality
	iteration := generated.all_after_last('string first_key(').all_after('{').all_before('\n}')
	assert iteration.contains('string__clone(map_eq_key_0)'), iteration
	custom := generated.all_after_last('bool compare_entries(').all_after('{').all_before('\n}')
	assert custom.contains('string__clone'), custom
}
