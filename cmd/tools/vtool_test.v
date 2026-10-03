import os

const vexe = os.quoted_path(@VEXE)

const tfolder = os.join_path(os.vtmp_dir(), 'vtool_test')
const vmodules = os.join_path(tfolder, '.vmodules')

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
		write_file(os.join_path_single(root, 'lib.v'), 'module ${name}\n\npub fn f() int {\n\treturn 1\n}\n')!
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
	res := os.exec([@VEXE, 'tool', 'greet'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('greet ran'), res.output
}

// test_v_tool_runs_a_module_from_the_global_module_folder covers the installed
// case, which is where most tools live once someone has used `v install`.
fn test_v_tool_runs_a_module_from_the_global_module_folder() {
	prepare_fixture()!
	res := os.exec([@VEXE, 'tool', 'gtool'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('gtool ran'), res.output
}

// test_v_tool_lists_the_tool_modules lists what can be named from here, which is
// what makes the command discoverable without knowing a module name in advance.
fn test_v_tool_lists_the_tool_modules() {
	prepare_fixture()!
	res := os.exec([@VEXE, 'tool'])
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
	res := os.exec([@VEXE, 'tool'])
	assert !res.output.contains('glib'), res.output
	assert !res.output.contains('libmod'), res.output
}

// test_v_tool_refuses_a_module_with_no_program covers asking to run a library.
fn test_v_tool_refuses_a_module_with_no_program() {
	prepare_fixture()!
	res := os.exec([@VEXE, 'tool', 'libmod'])
	assert res.exit_code == 1
	assert res.output.contains('holds no program to run'), res.output
}

fn test_v_tool_reports_an_unknown_module() {
	prepare_fixture()!
	res := os.exec([@VEXE, 'tool', 'nosuchtool'])
	assert res.exit_code == 1
	assert res.output.contains('no module named `nosuchtool`'), res.output
}

fn test_v_tool_help_explains_the_rule() {
	res := os.exec([@VEXE, 'tool', '--help'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('Usage: v tool [options] [NAME]'), res.output
	assert res.output.contains('main.v'), res.output
}

fn test_v_tool_lists_runnable_import_names_without_manifests() {
	prepare_fixture()!
	write_file(os.join_path(vmodules, 'bare', 'main.v'), "module main\nfn main() { println('bare ran') }\n")!
	write_file(os.join_path(vmodules, 'gtool', 'v.mod'), "Module { name: 'different_manifest_name' }\n")!
	listed := os.exec([@VEXE, 'tool'])
	assert listed.exit_code == 0, listed.output
	names := listed.output.trim_space().split_into_lines()
	assert 'bare' in names, listed.output
	assert 'gtool' in names, listed.output
	assert 'different_manifest_name' !in names, listed.output
	for name in ['bare', 'gtool'] {
		res := os.exec([@VEXE, 'tool', '${name}'])
		assert res.exit_code == 0, res.output
		assert res.output.contains('${name} ran'), res.output
	}
}

fn test_v_tool_short_help_explains_module_resolution() {
	res := os.exec([@VEXE, 'tool', '-h'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('main.v'), res.output
	assert res.output.contains('same lookup as'), res.output
}

fn test_v_tool_lists_the_project_directory_when_manifest_name_differs() {
	prepare_fixture()!
	write_file(os.join_path(tfolder, 'app', 'v.mod'), "Module { name: 'custom_name' }\n")!
	listed := os.exec([@VEXE, 'tool'])
	assert listed.exit_code == 0, listed.output
	names := listed.output.trim_space().split_into_lines()
	assert 'app' in names, listed.output
	assert 'custom_name' !in names, listed.output
	res := os.exec([@VEXE, 'tool', 'app'])
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'app', res.output
}

fn test_v_tool_omits_dotted_project_names_that_do_not_resolve_to_the_project() {
	prepare_fixture()!
	project := write_module(tfolder, 'my.project', true)!
	os.chdir(project)!
	checked := os.exec([@VEXE, '-check', '.'])
	assert checked.exit_code == 0, checked.output
	for has_decoy in [false, true] {
		if has_decoy {
			write_file(os.join_path(project, 'my', 'project', 'main.v'),
				"module main\nfn main() { println('decoy ran') }\n")!
		}
		listed := os.exec([@VEXE, 'tool'])
		assert listed.exit_code == 0, listed.output
		names := listed.output.trim_space().split_into_lines()
		assert 'my.project' !in names, listed.output
		assert 'gtool' in names, listed.output
	}
}

fn test_v_tool_preserves_resolvable_space_project_names() {
	prepare_fixture()!
	project := write_module(tfolder, 'my project', true)!
	os.chdir(project)!
	listed := os.exec([@VEXE, 'tool'])
	assert listed.exit_code == 0, listed.output
	assert 'my project' in listed.output.trim_space().split_into_lines(), listed.output
	res := os.exec([@VEXE, 'tool', 'my project'])
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'my project ran', res.output
}

fn test_v_tool_omits_unresolvable_dotted_installed_directory_names() {
	prepare_fixture()!
	write_module(vmodules, 'dotted.tool', true)!
	listed := os.exec([@VEXE, 'tool'])
	assert listed.exit_code == 0, listed.output
	names := listed.output.trim_space().split_into_lines()
	assert 'dotted.tool' !in names, listed.output
	assert 'app' in names, listed.output
	assert 'gtool' in names, listed.output
}
