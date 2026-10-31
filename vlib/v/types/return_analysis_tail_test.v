module types

import os
import v.parser
import v.pref

fn test_return_analysis_preserves_local_noreturn_shadowing() {
	root := os.join_path(os.vtmp_dir(), 'v3_return_tail_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, 'fn implicit_exit() { value := 1; panic("stop") }
fn shadowed_exit() { panic := fn (_ string) {}; panic("stop") }
fn explicit_return() int { panic := fn (_ string) {}; panic("stop"); return 7 }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.smartcasts['sentinel'] = builtin_int_type
	expected := {
		'implicit_exit':   true
		'shadowed_exit':   false
		'explicit_return': true
	}
	mut checked := 0
	for node in a.nodes {
		if node.kind != .fn_decl { continue }
		returns := expected[node.value] or { continue }
		assert tc.stmt_sequence_definitely_returns(&node, 0) == returns, node.value
		assert tc.smartcasts.len == 1
		assert 'sentinel' in tc.smartcasts
		checked++
	}
	assert checked == expected.len
}
