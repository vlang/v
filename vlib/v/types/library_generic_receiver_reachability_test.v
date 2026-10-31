module types

import os
import v.parser
import v.pref

// A generic body calls the methods of its type parameters. Until the instances exist
// there is no checked type to resolve such a call with, so the methods that a generic
// body names are reached by name, with what their own bodies name.
fn test_library_methods_named_by_a_generic_body_are_reached() {
	root := os.join_path(os.vtmp_dir(), 'v3_library_generic_receiver_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	lib_path := os.join_path(root, 'lib.v')
	os.write_file(main_path, 'module main
fn main() {}
')!
	os.write_file(lib_path, 'module lib
pub struct Context {
mut:
	sent int
}
pub fn run[X](mut ctx X) {
	ctx.send_file()
}
fn (mut ctx Context) send_file() {
	ctx.sent += compressed_size()
}
fn compressed_size() int {
	return 1
}
fn (mut ctx Context) unrelated() {}
fn send_file() {}
fn dormant() {}
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, lib_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.skip_library_bodies_for_reachability({
		lib_path: true
	}, []string{}, false)
	for name in ['lib.run', 'lib.Context.send_file', 'lib.compressed_size'] {
		assert tc.reachable_library_fns[name], name
	}
	// A member access names a method, not the function with the same name.
	for name in ['lib.Context.unrelated', 'lib.send_file', 'lib.dormant'] {
		assert !tc.reachable_library_fns[name], name
	}
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
}
