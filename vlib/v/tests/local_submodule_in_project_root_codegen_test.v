// Regression test for https://github.com/vlang/v/issues/28074
// A project with a `v.mod` at its root can keep submodule source files directly
// in that root folder (e.g. `calculator.v` declaring `module calculator`).
// The module name of such a file used to be mis-qualified as `.` (because the
// file path resolves to `<root>/.`). When the file also produced parser codegen
// (any enum does), the generated `module <name>` line was emitted with an empty
// name, so the generated code failed to parse with:
//   `error: unexpected token `@`, expecting name`
import os

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const vexe = @VEXE

fn project_path() string {
	return os.join_path(os.real_path(os.vtmp_dir()), 'issue_28074_root_submodule')
}

fn write_project() {
	basepath := project_path()
	os.rmdir_all(basepath) or {}
	os.mkdir_all(basepath) or { panic(err) }
	os.write_file(os.join_path(basepath, 'v.mod'),
		"Module {\n\tname: 'issue_28074'\n\tversion: '0.0.1'\n}\n") or { panic(err) }
	os.write_file(os.join_path(basepath, 'main.v'),
		['module main', '', 'import calculator', '', 'fn main() {', '\tprintln(calculator.evaluate(calculator.Op.add))', '}'].join('\n') +
		'\n') or { panic(err) }
	// A submodule file living directly in the project root, that also declares an
	// enum, so the parser generates helper code (which triggered the bug).
	os.write_file(os.join_path(basepath, 'calculator.v'),
		['module calculator', '', 'pub enum Op {', '\tadd', '\tsub', '}', '', 'pub fn evaluate(op Op) int {', '\treturn if op == .add { 1 } else { -1 }', '}'].join('\n') +
		'\n') or { panic(err) }
}

fn compile_project(target string) os.Result {
	basepath := project_path()
	write_project()
	out_name := os.join_path(os.vtmp_dir(), 'issue_28074_out')
	old_wd := os.getwd()
	os.chdir(basepath) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
		os.rmdir_all(basepath) or {}
		os.rm(out_name) or {}
	}
	// `-old-compiler` forces the compatibility compiler (the default on non-macOS,
	// a fallback on macOS) where the mis-qualification happened.
	return os.execute('${os.quoted_path(vexe)} -old-compiler -o ${os.quoted_path(out_name)} ${os.quoted_path(target)}')
}

fn test_root_submodule_with_enum_compiles_from_project_dir() {
	res := compile_project('.')
	assert res.exit_code == 0, res.output
}
