module types

import os
import v.parser
import v.pref

fn test_closure_library_frontier_checks_reached_body_and_preserves_roots() {
	root := os.join_path(os.vtmp_dir(), 'v3_library_closure_defer_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	closure_path := os.join_path(root, 'closure.v')
	sync_path := os.join_path(root, 'sync.v')
	os.write_file(main_path, 'module main
fn main() {}
')!
	os.write_file(closure_path, 'module closure
pub fn dormant() { \$compile_error("reached closure helper") }
fn init() {}
@[markused]
fn marked() {}
fn generic[T](value T) {}
')!
	os.write_file(sync_path, 'module sync
fn runtime_body() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, closure_path, sync_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.skip_library_bodies_for_reachability({
		closure_path: true
		sync_path:    true
	}, []string{}, false)
	assert !tc.reachable_library_fns['closure.dormant']
	for name in ['closure.init', 'closure.marked', 'closure.generic', 'sync.runtime_body'] {
		assert tc.reachable_library_fns[name], name
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.check_reached_library_bodies({
		'closure.dormant': true
	}, false) == 1
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == 'reached closure helper'
	assert tc.check_reached_library_bodies({
		'closure.dormant': true
	}, false) == 0

	mut legacy := TypeChecker.new(a)
	legacy.collect(a)
	legacy.skip_unreachable_library_bodies({
		closure_path: true
	}, []string{}, false)
	assert legacy.reachable_library_fns['closure.dormant']
	legacy.check_semantics()
	assert legacy.errors.len == 1, legacy.errors.str()
	assert legacy.errors[0].msg == 'reached closure helper'
}
