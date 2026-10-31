module types

import os
import v.parser
import v.pref

fn test_library_stringifiers_defer_concrete_methods_and_keep_required_roots() {
	root := os.join_path(os.vtmp_dir(), 'v3_library_stringifiers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	library_path := os.join_path(root, 'dependency.v')
	os.write_file(main_path, 'module main
import dependency
struct Own {}
fn (value Own) str() string { return "own" }
fn main() {
	value := dependency.Hidden{}
	_ := value.str()
}
')!
	os.write_file(library_path, 'module dependency
pub struct Hidden {}
struct Seeded {}
struct Marked {}
struct Box[T] {}
pub fn (value Hidden) str() string {
	\$compile_error("formatter checked late")
	return "hidden"
}
fn (value Seeded) str() string { return "seeded" }
@[markused]
fn (value Marked) str() string { return "marked" }
fn (value Box[T]) str() string { return "generic" }
fn str() string { return "free function" }
fn cleanup() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, library_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.skip_library_bodies_for_reachability({
		library_path: true
	}, ['dependency.Seeded.str'], true)
	assert !tc.reachable_library_fns['dependency.Hidden.str']
	for name in ['Seeded.str', 'Marked.str', 'str', 'cleanup'] {
		assert tc.reachable_library_fns['dependency.${name}'], name
	}
	mut saw_generic_stringifier := false
	for node in a.nodes {
		if node.kind == .fn_decl && node.value.starts_with('Box') && node.value.ends_with('.str') {
			assert tc.reachable_library_fns['dependency.${node.value}']
			saw_generic_stringifier = true
		}
	}
	assert saw_generic_stringifier
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	assert tc.check_reached_library_bodies({
		'dependency.Hidden.str': true
	}, false) == 1
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].msg == 'formatter checked late', tc.errors.str()
	assert tc.library_bodies_checked_late() == 1
	assert tc.check_reached_library_bodies({
		'dependency.Hidden.str': true
	}, false) == 0
	assert tc.library_bodies_checked_late() == 1
}

fn test_library_stringifier_late_hint_keeps_existing_implicit_roots() {
	path := os.join_path(os.vtmp_dir(), 'v3_library_stringifier_late_${os.getpid()}.v')
	os.write_file(path, 'module dependency
struct Hidden {}
fn (value Hidden) str() string { return "hidden" }
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.skip_unreachable_library_bodies({
		path: true
	}, []string{}, false)
	assert tc.reachable_library_fns['dependency.Hidden.str']
}

fn test_library_frontier_node_snapshot_preserves_ranges_order_and_fallback() {
	path := os.join_path(os.vtmp_dir(), 'v3_library_frontier_nodes_${os.getpid()}.v')
	os.write_file(path, 'module dependency
fn first() int {
	value := 11
	\$compile_error("first frontier error")
	return value
}
fn second() int {
	value := 22
	\$compile_error("second frontier error")
	return value
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.skip_library_bodies_for_reachability({
		path: true
	}, []string{}, false)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	tc.enable_scoped_parallel_workers()
	frontier := tc.prepare_library_body_frontier()
	assert frontier.items.len == 2
	first := frontier.items[0].fn_idx
	second := frontier.items[1].fn_idx
	assert tc.check_library_body_frontier_nodes(frontier, [second, first, second], false)? == 2
	assert tc.errors.len == 2, tc.errors.str()
	assert tc.errors[0].msg == 'first frontier error'
	assert tc.errors[1].msg == 'second frontier error'
	assert tc.reachable_library_fns['dependency.first']
	assert tc.reachable_library_fns['dependency.second']
	assert tc.library_bodies_checked_late() == 2
	assert tc.check_library_body_frontier_nodes(frontier, [first, second], false)? == 0
	assert tc.library_bodies_checked_late() == 2
	if _ := tc.check_library_body_frontier_nodes(frontier, [-1], false) {
		assert false, 'an unindexed declaration must use the ordinary fallback'
	}
	a.add(.int_literal)
	if _ := tc.check_library_body_frontier_nodes(frontier, [first], false) {
		assert false, 'an expanded AST must use the ordinary fallback'
	}
}
