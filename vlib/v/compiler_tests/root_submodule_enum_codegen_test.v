// Guard test for the v3 (new) compiler, mirroring the vlib/v regression for
// https://github.com/vlang/v/issues/28074 . A project with a `v.mod` at its
// root may keep submodule source files directly in that root folder (e.g.
// `calculator.v` declaring `module calculator`). Such a submodule that also
// declares an enum used to make the old compiler emit `module ` with no name
// for parser codegen. v3 resolves module names differently and already handles
// this correctly; this test locks that behavior in.
import os
import v.cmdexec
import v.tempname

fn test_v3_compiles_root_submodule_with_enum() {
	root := os.join_path(os.vtmp_dir(), 'v3_issue_28074_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	project := os.join_path(root, 'project')
	os.mkdir(project)!
	out := os.join_path(root, 'app')
	os.write_file(os.join_path(project, 'v.mod'), "Module {\n\tname: 'issue_28074'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'main.v'),
		['module main', '', 'import calculator', '', 'fn main() {',
			'\tprintln(calculator.evaluate(calculator.Op.add))', '}'].join('\n') +
			'\n') or { panic(err) }
	os.write_file(os.join_path(project, 'calculator.v'),
		['module calculator', '', 'pub enum Op {', '\tadd', '\tsub', '}', '',
			'pub fn evaluate(op Op) int {', '\treturn if op == .add { 1 } else { -1 }', '}'].join('\n') +
			'\n') or { panic(err) }
	compile := cmdexec.run_in(@VEXE, ['-o', out, '.'], project)
	assert compile.exit_code == 0, compile.output
	run := cmdexec.run_in(out, [], project)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1', run.output
}
