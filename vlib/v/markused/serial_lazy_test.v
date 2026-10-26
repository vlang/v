module markused

import os
import v.parser
import v.pref
import v.types

fn test_serial_prepared_reachability_matches_eager_body_collection() {
	root := os.join_path(os.vtmp_dir(), 'markused_serial_lazy_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	dep_path := os.join_path(root, 'dependency.v')
	os.write_file(main_path, 'module main
import dependency
fn main() { _ := dependency.entry() }
')!
	mut source := 'module dependency
const answer = const_helper()
fn const_helper() int { return 7 }
fn default_helper() int { return answer }
struct Item { value int = default_helper() }
fn (item Item) read() int { return item.value }
fn middle() int {
	item := Item{}
	return item.read()
}
fn dispatch(callback fn () int) int { return callback() }
pub fn entry() int { return dispatch(middle) }
fn unreachable_helper() int { return 101 }
'
	// Cross the eager threshold with bodies whose implicit calls and const refs
	// must stay unreachable, despite having been scanned by the eager control.
	for i in 0 .. min_eager_markused_bodies {
		source += 'fn unused_${i}() int { return unreachable_helper() + answer }\n'
	}
	os.write_file(dep_path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([main_path, dep_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert a.worker_count() == 0
	collector := CallCollector{ a: a, tc: &tc }
	assert !markused_can_parallel_precollect(&collector)
	mut prepared := prepare_markused_declarations(a, &tc, true)
	defer { prepared.release() }
	assert prepared.ready && prepared.body_ids.len > min_eager_markused_bodies
	lazy := mark_used_without_generic_detection_prepared(a, &tc, mut prepared)
	if output := os.getenv_opt('MARKUSED_SERIAL_LAZY_RECEIPT') {
		os.write_file(output, lazy.keys().sorted().join('\n'))!
	}
	for name in ['entry', 'dispatch', 'middle', 'Item.read', 'default_helper', 'const_helper'] {
		assert lazy['dependency.${name}'], name
	}
	for name in ['unreachable_helper', 'unused_0', 'unused_${min_eager_markused_bodies - 1}'] {
		assert !lazy['dependency.${name}'], name
	}
	$if !v3_no_parallel ? {
		a.ensure_workers(2)
		defer { a.close_workers() }
		assert markused_can_parallel_precollect(&collector)
		parallel := mark_used_without_generic_detection_prepared(a, &tc, mut prepared)
		assert parallel == lazy
	}
}
