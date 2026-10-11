module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.types

fn test_native_at_exit_runs_callbacks_after_return_and_exit() {
	$if !macos || !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v_arm64_exit_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	for explicit_exit in [false, true] {
		source := os.join_path(root, 'callbacks.v')
		binary := os.join_path(root, 'callbacks')
		termination := if explicit_exit { 'C.exit(7)' } else { '' }
		os.write_file(source, 'module main
type FnExitCb = fn ()
fn C.exit(int)
fn at_exit(cb FnExitCb) ! {}
fn println(text string) {}
fn first() { println("first") }
fn second() { println("second") }
fn main() {
	at_exit(first) or { C.exit(1) }
	at_exit(second) or { C.exit(2) }
	at_exit(first) or { C.exit(3) }
	println("body")
	${termination}
}
')!
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(source)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.annotate_types()
		assert tc.errors.len == 0, tc.errors.str()
		m := ssa.build_with_used(a, map[string]bool{}, tc)
		mut g := Gen.new(m)
		g.gen()
		g.write_and_link(binary)
		result := os.exec([binary])
		assert result.exit_code == if explicit_exit { 7 } else { 0 }, result.output
		assert result.output.trim_space() == 'body\nfirst\nsecond\nfirst', result.output
	}
}
