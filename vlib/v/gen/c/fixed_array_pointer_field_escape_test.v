module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn test_fixed_array_pointer_field_does_not_heap_promote_value_receiver() {
	path := os.join_path(os.vtmp_dir(), 'fixed_array_pointer_field_${os.getpid()}.v')
	os.write_file(path, 'struct Coordinator {
mut:
	closed bool
	reasons [9]int
}
fn (mut coordinator Coordinator) is_closed() bool {
	return coordinator.closed
}
struct Dispatch {
	coordinator &Coordinator
	generation u64
}
fn (dispatch Dispatch) valid() bool {
	return !dispatch.coordinator.is_closed()
}
fn main() {
	coordinator := &Coordinator{}
	dispatch := Dispatch{coordinator: coordinator}
	assert dispatch.valid()
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
	body := generated.all_after_last('bool Dispatch__valid(').all_after('{').all_before('\n}')
	assert body.contains('Coordinator__is_closed'), body
	assert !body.contains('memdup'), body
	assert !body.contains('__fixed_array_value_param_'), body
}
