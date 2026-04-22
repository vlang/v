import os

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
	// TODO: remove this, when after vc/v.c *also* uses `_cache`, instead of `cache`:
	old_cache_path := os.join_path(os.vmodules_dir(), 'cache')
	dump(old_cache_path)
	if os.exists(old_cache_path) {
		os.rmdir_all(old_cache_path)!
	}
}

fn vroot_path(relpath string) string {
	return os.real_path(os.join_path(@VMODROOT, relpath))
}

fn vrun_ok_in_dir(workdir string, options string, path string) string {
	old_wd := os.getwd()
	os.chdir(workdir) or { panic(err) }
	defer {
		os.chdir(old_wd) or { panic(err) }
	}
	return vrun_ok(options, path)
}

fn write_file(path string, content string) {
	os.write_file(path, content) or { panic(err) }
}

fn setup_module_resolution_workdir_fixture() string {
	workspace := os.join_path(os.vtmp_dir(),
		'v_module_resolution_independent_of_workdir_${os.getpid()}')
	interp := r'${'
	os.rmdir_all(workspace) or {}
	os.mkdir_all(os.join_path(workspace, 'app')) or { panic(err) }
	os.mkdir_all(os.join_path(workspace, 'lib')) or { panic(err) }
	write_file(os.join_path(workspace, '.v.mod.stop'), '')
	write_file(os.join_path(workspace, 'app', 'v.mod'),
		"Module {\n\tname: 'app'\n\tdescription: ''\n\tversion: ''\n\tlicense: ''\n\tdependencies: []\n}\n")
	write_file(os.join_path(workspace, 'app', 'main.v'),
		"module main\n\nimport lib\n\nfn main() {\n\tprintln('Hello ${interp}lib.square(4)}!')\n}\n")
	write_file(os.join_path(workspace, 'lib', 'v.mod'),
		"Module {\n\tname: 'lib'\n\tdescription: ''\n\tversion: ''\n\tlicense: ''\n\tdependencies: []\n}\n")
	write_file(os.join_path(workspace, 'lib', 'lib.v'),
		'module lib\n\npub fn square(x int) int {\n\treturn x * x\n}\n')
	return workspace
}

fn vrun_ok(options string, path string) string {
	cmd := '${os.quoted_path(@VEXE)} ${options} ${os.quoted_path(path)}'
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('> failing vrun cmd: ${cmd}')
		eprintln('> output:\n${res.output}')
		assert res.exit_code == 0
	}
	return res.output
}

fn test_projects_should_run() {
	$if windows {
		return
	}
	res := vrun_ok('run', vroot_path('vlib/v/tests/testdata/enum_in_builtin') + os.path_separator)
	assert res.trim_space() == 'v0'
}

fn test_running_subdir_project_with_parent_vmod_works() {
	root := os.join_path(os.vtmp_dir(), 'v_subdir_project_with_parent_vmod')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'hexagonal', 'application'))!
	os.write_file(os.join_path(root, 'v.mod'), 'Module {\nname: "vanilla3"\n}')!
	os.write_file(os.join_path(root, 'hexagonal', 'application', 'auth_usecase.v'),
		'module application\n\npub struct AuthUseCase {}\n')!
	os.write_file(os.join_path(root, 'hexagonal', 'main.v'),
		"module main\n\nimport application\n\nfn main() {\n\t_ := application.AuthUseCase{}\n\tprintln('built')\n}\n")!
	old_dir := os.getwd()
	defer {
		os.chdir(old_dir) or {}
	}
	os.chdir(root)!
	res := os.execute('${os.quoted_path(@VEXE)} run hexagonal')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'built'
}

fn test_running_module_with_same_module_subdirs_setting_works() {
	root := os.join_path(os.vtmp_dir(), 'v_same_module_subdirs_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'app', 'foo', 'internal', 'nested'))!
	os.write_file(os.join_path(root, 'app', 'main.v'),
		'module main\n\nimport foo\n\nfn main() {\n\tprintln(foo.answer())\n}\n')!
	os.write_file(os.join_path(root, 'app', 'foo', 'v.mod'),
		"Module {\n\tname: 'foo'\n\tsubdirs: ['internal']\n}\n")!
	os.write_file(os.join_path(root, 'app', 'foo', 'foo.v'),
		'module foo\n\npub fn answer() int {\n\treturn secret()\n}\n')!
	os.write_file(os.join_path(root, 'app', 'foo', 'internal', 'nested', 'secret.v'),
		'module foo\n\nfn secret() int {\n\treturn 42\n}\n')!
	old_dir := os.getwd()
	defer {
		os.chdir(old_dir) or {}
	}
	os.chdir(root)!
	res := os.execute('${os.quoted_path(@VEXE)} run app/main.v')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '42'
}

fn test_running_project_with_same_module_subdirs_can_find_templates_next_to_vmod() {
	root := os.join_path(os.vtmp_dir(), 'v_same_module_subdirs_veb_templates_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'user'))!
	os.mkdir_all(os.join_path(root, 'ci'))!
	os.mkdir_all(os.join_path(root, 'templates'))!
	os.write_file(os.join_path(root, 'v.mod'),
		"Module {\n\tname: 'vebsubdirs'\n\tsubdirs: ['user', 'ci']\n}\n")!
	os.write_file(os.join_path(root, 'main.v'), 'import veb

pub struct App {}

pub struct Context {
	veb.Context
}

fn main() {}
')!
	os.write_file(os.join_path(root, 'user', 'register.v'), 'module main

import veb

@["/register"]
pub fn (mut app App) register(mut ctx Context) veb.Result {
	return $veb.html()
}

@["/settings"]
pub fn (mut app App) settings(mut ctx Context) veb.Result {
	return $veb.html("templates/settings.html")
}
')!
	os.write_file(os.join_path(root, 'ci', 'ci_routes.v'), 'module main

import veb

@["/ci"]
pub fn (mut app App) ci_runs(mut ctx Context) veb.Result {
	return $veb.html()
}
')!
	os.write_file(os.join_path(root, 'templates', 'register.html'),
		'<html><body><p>Register</p></body></html>\n')!
	os.write_file(os.join_path(root, 'templates', 'ci_runs.html'),
		'<html><body><p>CI Runs</p></body></html>\n')!
	os.write_file(os.join_path(root, 'templates', 'settings.html'),
		'<html><body><p>Settings</p></body></html>\n')!
	old_dir := os.getwd()
	defer {
		os.chdir(old_dir) or {}
	}
	os.chdir(root)!
	res := os.execute('${os.quoted_path(@VEXE)} .')
	assert res.exit_code == 0, res.output
}

fn test_module_resolution_is_independent_of_working_directory() {
	workspace := setup_module_resolution_workdir_fixture()
	defer {
		os.rmdir_all(workspace) or {}
	}
	res_root := vrun_ok_in_dir(workspace, 'run', 'app')
	assert res_root.trim_space() == 'Hello 16!'

	res_app := vrun_ok_in_dir(os.join_path(workspace, 'app'), 'run', '.')
	assert res_app.trim_space() == 'Hello 16!'
}

fn test_custom_print_should_compile_with_no_builtin() {
	source_path := os.join_path(os.vtmp_dir(), 'custom_print_no_builtin_${os.getpid()}.v')
	output_path := os.join_path(os.vtmp_dir(), 'custom_print_no_builtin_${os.getpid()}.c')
	source := [
		'@[markused]',
		'pub fn error() {',
		"\tprint(c'\\n')",
		'}',
		'',
		'pub fn print(fmt voidptr, ...) {}',
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	_ = vrun_ok('-o ${os.quoted_path(output_path)} -no-builtin', source_path)
	assert os.exists(output_path)
}

fn test_generic_recursive_self_method_call_should_compile() {
	source_path := os.join_path(os.vtmp_dir(),
		'generic_recursive_self_method_call_${os.getpid()}.v')
	output_path := os.join_path(os.vtmp_dir(), 'generic_recursive_self_method_call_${os.getpid()}')
	mut expected_output_path := output_path
	$if windows {
		expected_output_path += '.exe'
	}
	source := [
		'struct Decoder {}',
		'',
		'struct StructTypePointer[T] {',
		'mut:',
		'\tval &T',
		'}',
		'',
		'pub fn decode[T](val string) !T {',
		'\tmut decoder := Decoder{}',
		'',
		'\tmut result := T{}',
		'\tdecoder.decode_value(mut result)!',
		'\treturn result',
		'}',
		'',
		'fn (mut decoder Decoder) decode_value[T](mut val T) ! {',
		'\t\$if T.indirections != 0 {',
		'\t\tunsafe {',
		'\t\t\t*val = 2',
		'\t\t}',
		'\t} \$else \$if T is \$struct {',
		'\t\tdecode_value(mut val.val)!',
		'\t}',
		'}',
		'',
		'fn main() {',
		"\tassert *decode[StructTypePointer[int]]('2')!.val == 2",
		'}',
	].join_lines()
	write_file(source_path, source)
	defer {
		os.rm(source_path) or {}
		os.rm(output_path) or {}
		os.rm(expected_output_path) or {}
	}
	_ = vrun_ok('-o ${os.quoted_path(output_path)}', source_path)
	assert os.exists(expected_output_path)
}

fn test_sync_waitgroup_should_check_for_windows() {
	source_path := os.join_path(os.vtmp_dir(), 'sync_waitgroup_issue_14468_${os.getpid()}.v')
	source := [
		'module main',
		'',
		'import sync',
		'',
		'fn main() {',
		'\tmut wg := sync.new_waitgroup()',
		'\twg.add(1)',
		'\twg.done()',
		'\twg.wait()',
		'}',
	].join_lines()
	write_file(source_path, source)
	defer {
		os.rm(source_path) or {}
	}
	_ = vrun_ok('-os windows -check', source_path)
}
