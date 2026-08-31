// Guard test for the v3 (new) compiler, mirroring the vlib/v regression for
// https://github.com/vlang/v/issues/28074 . A project with a `v.mod` at its
// root may keep submodule source files directly in that root folder (e.g.
// `calculator.v` declaring `module calculator`). Such a submodule that also
// declares an enum used to make the old compiler emit `module ` with no name
// for parser codegen. v3 resolves module names differently and already handles
// this correctly; this test locks that behavior in.
import os

const root_submodule_tests_dir = os.dir(@FILE)
const root_submodule_v3_dir = os.dir(root_submodule_tests_dir)
const root_submodule_vlib_dir = os.dir(root_submodule_v3_dir)
const root_submodule_v3_src = os.join_path(root_submodule_v3_dir, 'v3.v')

fn v3_bin_path() string {
	return os.join_path(os.temp_dir(), 'v3_root_submodule_test_${os.getpid()}')
}

fn build_v3() string {
	v3_bin := v3_bin_path()
	if os.is_file(v3_bin) {
		return v3_bin
	}
	build :=
		os.execute('${os.quoted_path(@VEXE)} -gc none -prealloc -path "${root_submodule_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(root_submodule_v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn testsuite_end() {
	os.rm(v3_bin_path()) or {}
}

fn test_v3_compiles_root_submodule_with_enum() {
	v3_bin := build_v3()
	project := os.join_path(os.vtmp_dir(), 'v3_issue_28074_${os.getpid()}')
	os.rmdir_all(project) or {}
	os.mkdir_all(project) or { panic(err) }
	out := os.join_path(os.vtmp_dir(), 'v3_issue_28074_out_${os.getpid()}')
	defer {
		os.rmdir_all(project) or {}
		os.rm(out) or {}
	}
	os.write_file(os.join_path(project, 'v.mod'), "Module {\n\tname: 'issue_28074'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(project, 'main.v'),
		['module main', '', 'import calculator', '', 'fn main() {', '\tprintln(calculator.evaluate(calculator.Op.add))', '}'].join('\n') +
		'\n') or { panic(err) }
	os.write_file(os.join_path(project, 'calculator.v'),
		['module calculator', '', 'pub enum Op {', '\tadd', '\tsub', '}', '', 'pub fn evaluate(op Op) int {', '\treturn if op == .add { 1 } else { -1 }', '}'].join('\n') +
		'\n') or { panic(err) }
	old_wd := os.getwd()
	os.chdir(project) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
	}
	compile := os.execute('${os.quoted_path(v3_bin)} -o ${os.quoted_path(out)} .')
	assert compile.exit_code == 0, compile.output
	run := os.execute(os.quoted_path(out))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1', run.output
}
