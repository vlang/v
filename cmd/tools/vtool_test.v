import os

const vexe = os.quoted_path(@VEXE)

const tfolder = os.join_path(os.vtmp_dir(), 'vtool_test')
const vmodules = os.join_path(tfolder, 'vmodules')

// write_file creates `path` and its parent folders.
fn write_file(path string, content string) ! {
	os.mkdir_all(os.dir(path))!
	os.write_file(path, content)!
}

// write_module lays out a module under `parent`, and returns its root.
fn write_module(parent string, name string, with_main bool) !string {
	root := os.join_path_single(parent, name)
	write_file(os.join_path_single(root, 'v.mod'), "Module {\n\tname: '${name}'\n}\n")!
	if with_main {
		write_file(os.join_path_single(root, 'main.v'), "module main\n\nfn main() {\n\tprintln('${name} ran')\n}\n")!
	} else {
		write_file(os.join_path_single(root, 'lib.v'), "module ${name}\n\npub fn f() int {\n\treturn 1\n}\n")!
	}
	return root
}

// prepare_fixture builds a project beside a tool module and a library module,
// and points VMODULES at a folder holding one more of each.
fn prepare_fixture() ! {
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder)!
	write_file(os.join_path(tfolder, 'app', 'v.mod'), "Module {\n\tname: 'app'\n}\n")!
	write_file(os.join_path(tfolder, 'app', 'main.v'), "module main\n\nfn main() {\n\tprintln('app')\n}\n")!
	write_module(tfolder, 'greet', true)!
	write_module(tfolder, 'libmod', false)!
	write_module(vmodules, 'gtool', true)!
	write_module(vmodules, 'glib', false)!
	os.chdir(os.join_path(tfolder, 'app'))!
	os.setenv('VMODULES', vmodules, true)
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

// test_v_tool_runs_a_module_by_name is the command itself: a sibling tool module
// runs, which is what a developer with a checkout next to the project wants.
fn test_v_tool_runs_a_module_by_name() {
	prepare_fixture()!
	res := os.execute('${vexe} tool greet')
	assert res.exit_code == 0, res.output
	assert res.output.contains('greet ran'), res.output
}

// test_v_tool_runs_a_module_from_the_global_module_folder covers the installed
// case, which is where most tools live once someone has used `v install`.
fn test_v_tool_runs_a_module_from_the_global_module_folder() {
	prepare_fixture()!
	res := os.execute('${vexe} tool gtool')
	assert res.exit_code == 0, res.output
	assert res.output.contains('gtool ran'), res.output
}

// test_v_tool_lists_the_tool_modules lists what can be named from here, which is
// what makes the command discoverable without knowing a module name in advance.
fn test_v_tool_lists_the_tool_modules() {
	prepare_fixture()!
	res := os.execute('${vexe} tool')
	assert res.exit_code == 0, res.output
	names := res.output.trim_space().split_into_lines()
	assert 'app' in names, res.output
	assert 'gtool' in names, res.output
	// `greet` is a sibling, resolvable but not enumerated; the listing covers the
	// project and the global module folders, and says so.
	assert 'greet' !in names, res.output
}

// test_v_tool_leaves_out_modules_without_a_program keeps a library module out of
// the listing, so `v tool` is not a list of every installed module.
fn test_v_tool_leaves_out_modules_without_a_program() {
	prepare_fixture()!
	res := os.execute('${vexe} tool')
	assert !res.output.contains('glib'), res.output
	assert !res.output.contains('libmod'), res.output
}

// test_v_tool_refuses_a_module_with_no_program covers asking to run a library.
fn test_v_tool_refuses_a_module_with_no_program() {
	prepare_fixture()!
	res := os.execute('${vexe} tool libmod')
	assert res.exit_code == 1
	assert res.output.contains('holds no program to run'), res.output
}

fn test_v_tool_reports_an_unknown_module() {
	prepare_fixture()!
	res := os.execute('${vexe} tool nosuchtool')
	assert res.exit_code == 1
	assert res.output.contains('no module named `nosuchtool`'), res.output
}

fn test_v_tool_help_explains_the_rule() {
	res := os.execute('${vexe} tool --help')
	assert res.exit_code == 0, res.output
	assert res.output.contains('Usage: v tool [options] [NAME]'), res.output
	assert res.output.contains('main.v'), res.output
}