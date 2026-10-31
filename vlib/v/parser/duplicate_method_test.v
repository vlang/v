module parser

import os
import v.pref

// The duplicate-method diagnostic is answered from a per-file map instead of
// a scan over every AST node, so it must still see both declarations of one
// file and must forget them when the same parser moves on to the next file.
fn test_duplicate_methods_are_tracked_per_file() {
	root := os.join_path(os.vtmp_dir(), 'v3_duplicate_method_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut p := Parser.new(pref.new_preferences())

	first := os.join_path(root, 'first.v')
	os.write_file(first, 'struct Foo {}\nfn (f Foo) bar() int {\n\treturn 1\n}\nfn (f Foo) bar() int {\n\treturn 2\n}\nfn (f Foo) baz() {}\n')!
	p.parse_file(first)
	assert p.diagnostics.len == 1, p.diagnostics.str()
	assert p.diagnostics[0].message == 'duplicate method `bar`'
	assert p.diagnostics[0].line == 5

	// The same receiver and method name in another file is that file's own
	// first declaration, not a duplicate of the previous file's.
	second := os.join_path(root, 'second.v')
	os.write_file(second, 'struct Foo {}\nfn (f Foo) bar() int {\n\treturn 3\n}\n')!
	p.parse_file(second)
	assert p.diagnostics.len == 1, p.diagnostics.str()

	// Different receivers with the same method name are distinct methods.
	third := os.join_path(root, 'third.v')
	os.write_file(third, 'struct A {}\nstruct B {}\nfn (a A) run() {}\nfn (b B) run() {}\nfn run() {}\n')!
	p.parse_file(third)
	assert p.diagnostics.len == 1, p.diagnostics.str()
}
