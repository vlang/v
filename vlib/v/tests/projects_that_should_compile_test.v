import os
import v.cmdexec

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
	res := os.exec([@VEXE, ...(os.split_args(options) or { panic(err) }), path])
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
	res := os.exec([@VEXE, 'run', 'hexagonal'])
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
	res := os.exec([@VEXE, 'run', 'app/main.v'])
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
	res := os.exec([@VEXE, '.'])
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

fn test_importing_submodule_through_nested_vmod_works_issue_27138() {
	root := os.join_path(os.vtmp_dir(), 'v_issue_27138_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'dep', 'mymod'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'bug_project'\n}\n")!
	os.write_file(os.join_path(root, 'dep', 'v.mod'), "Module {\n\tname: 'dep'\n}\n")!
	os.write_file(os.join_path(root, 'dep', 'mymod', 'types.v'),
		"module mymod\n\npub enum Color {\n\tred\n\tgreen\n}\n\npub fn color_name(c Color) string {\n\treturn match c {\n\t\t.red { 'red' }\n\t\t.green { 'green' }\n\t}\n}\n")!
	os.write_file(os.join_path(root, 'main.v'),
		'module main\n\nimport dep.mymod\n\nfn main() {\n\tprintln(mymod.color_name(mymod.Color.red))\n}\n')!
	res := vrun_ok_in_dir(root, 'run', '.')
	assert res.trim_space() == 'red'
}

fn test_running_project_from_its_own_vmod_with_parent_vmod_works_issue_26828() {
	root := os.join_path(os.vtmp_dir(), 'v_issue_26828_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	project_root := os.join_path(root, 'outer')
	module_root := os.join_path(project_root, 'cli004')
	os.mkdir_all(os.join_path(module_root, 'sub'))!
	os.write_file(os.join_path(project_root, 'v.mod'), "Module {\n\tname: 'outer'\n}\n")!
	os.write_file(os.join_path(module_root, 'v.mod'), "Module {\n\tname: 'cli004'\n}\n")!
	os.write_file(os.join_path(module_root, 'cli004.v'),
		'module main\n\nimport sub\n\nfn main() {\n\tsub.greetings()\n}\n')!
	os.write_file(os.join_path(module_root, 'sub', 'hey.v'),
		"module sub\n\npub fn greetings() {\n\tprintln('greetings from sub')\n}\n")!

	res := vrun_ok_in_dir(module_root, 'crun', '.')
	assert res.trim_space() == 'greetings from sub'
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

// Without builtin there is no `IError`, so option/result wrappers have no `err`
// field. `or {}` blocks, if-guard `else` branches, `?`/`!` propagation and `none`
// must neither bind nor copy one (issue #28887).
fn test_option_or_blocks_should_compile_and_run_with_no_builtin() {
	$if windows {
		return
	}
	source_path := os.join_path(os.vtmp_dir(), 'option_or_no_builtin_${os.getpid()}.v')
	source := "module main

fn C.printf(fmt &char, ...) int

struct Holder {
mut:
	v ?int
}

fn opt(ok bool) ?int {
	if ok {
		return 7
	}
	return none
}

fn res(ok bool) !int {
	return if ok { 8 } else { 80 }
}

fn pair(ok bool) ?(int, int) {
	if ok {
		return 1, 2
	}
	return none
}

fn prop(ok bool) ?int {
	x := opt(ok)?
	return x + 1
}

fn prop_res(ok bool) !int {
	x := res(ok)!
	return x + 1
}

fn stmt_or(ok bool) int {
	opt(ok) or { return -1 }
	return 1
}

fn guard_value(ok bool) int {
	return if v := opt(ok) { v } else { -1 }
}

fn main() {
	a := opt(true) or { 0 }
	b := opt(false) or { 0 }
	c := res(false) or { 5 }
	d := opt(false) or {
		y := 3
		y + 1
	}
	p, q := pair(false) or { 9, 10 }
	r := prop(true) or { -1 }
	s := prop(false) or { -1 }
	u := prop_res(false) or { -2 }
	mut g := 0
	if v := opt(false) {
		g = v
	} else {
		g = -1
	}
	mut h := Holder{}
	h.v = none
	hv := h.v or { 11 }
	C.printf(c'%d %d %d %d %d %d %d %d %d %d %d %d %d %d\\n', a, b, c, d, p, q, r, s, u, g,
		stmt_or(false), guard_value(true), guard_value(false), hv)
}
"
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
	}
	res := vrun_ok('-gc none -no-builtin run', source_path)
	assert res.trim_space() == '7 0 80 4 9 10 8 -1 81 -1 -1 7 -1 11'
}

// With no `IError` there is no implicit `err` either, so using it is a checker
// error instead of generated C that reads an absent field (issue #28887).
fn test_implicit_err_is_undefined_with_no_builtin() {
	source_path := os.join_path(os.vtmp_dir(), 'implicit_err_no_builtin_${os.getpid()}.v')
	output_path := os.join_path(os.vtmp_dir(), 'implicit_err_no_builtin_${os.getpid()}.c')
	source := [
		'fn foo() ?int {',
		'\treturn 1',
		'}',
		'',
		'fn main() {',
		'\tfoo() or {',
		'\t\t_ = err',
		'\t\treturn',
		'\t}',
		'}',
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	res :=
		os.exec([@VEXE, '-gc', 'none', '-no-builtin', '-o', output_path, source_path])
	assert res.exit_code != 0, res.output
	assert res.output.contains('undefined ident: `err`'), res.output
}

// https://github.com/vlang/v/issues/28886
fn test_enum_shorthand_match_branches_should_compile_with_no_builtin() {
	source_path := os.join_path(os.vtmp_dir(), 'enum_shorthand_match_no_builtin_${os.getpid()}.v')
	output_path := os.join_path(os.vtmp_dir(), 'enum_shorthand_match_no_builtin_${os.getpid()}.c')
	source := [
		'enum Foo {',
		'\tfoo',
		'\tbar',
		'}',
		'',
		'fn foo(value int) Foo {',
		'\treturn match value {',
		'\t\t0 { .foo }',
		'\t\telse { .bar }',
		'\t}',
		'}',
		'',
		'fn take(f Foo) int {',
		'\treturn int(f)',
		'}',
		'',
		'fn main() {',
		'\tmut f := foo(1)',
		'\tf = match int(f) {',
		'\t\t0 { .bar }',
		'\t\telse { .foo }',
		'\t}',
		'\t_ = take(match int(f) {',
		'\t\t0 { .foo }',
		'\t\telse { .bar }',
		'\t})',
		'}',
	].join_lines()
	os.write_file(source_path, source)!
	defer {
		os.rm(source_path) or {}
		os.rm(output_path) or {}
	}
	_ = vrun_ok('-check -no-builtin', source_path)
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

fn test_cached_module_returns_a_sumtype_from_another_module() {
	root := os.join_path(os.vtmp_dir(), 'cached_sumtype_cross_module_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'payload'))!
	os.mkdir_all(os.join_path(root, 'maker'))!
	defer {
		os.rmdir_all(root) or {}
	}
	write_file(os.join_path(root, 'v.mod'), "Module { name: 'ucsmt' }")
	write_file(os.join_path(root, 'payload', 'payload.v'),
		'module payload

pub struct Foo {
pub:
	n int
}

pub struct Bar {
pub:
	s string
}

pub type Value = Foo | Bar
')
	write_file(os.join_path(root, 'maker', 'maker.v'), 'module maker

import payload

pub fn make() payload.Value {
	return payload.Foo{n: 42}
}
')
	source := os.join_path(root, 'main.v')
	write_file(source, 'import maker
import payload

fn main() {
	value := maker.make()
	assert value is payload.Foo
	assert (value as payload.Foo).n == 42
}
')
	mut environment := os.environ()
	environment['V3CACHE'] = os.join_path(root, '.cache')
	for _ in 0 .. 2 {
		result := cmdexec.run_in_merged_with_env(@VEXE, ['run', source], root, environment)
		assert result.exit_code == 0, result.output
	}
}

fn test_readline_raw_mode_methods_should_check_for_windows() {
	source_path := os.join_path(os.vtmp_dir(), 'readline_raw_mode_issue_24686_${os.getpid()}.v')
	source := [
		'module main',
		'',
		'import readline',
		'',
		'fn main() {',
		'\tmut r := readline.Readline{}',
		'\tr.enable_raw_mode()',
		'\tr.disable_raw_mode()',
		'\tr.enable_raw_mode_nosig()',
		'\tprintln(r.read_char()!)',
		'\tr.disable_raw_mode()',
		'}',
	].join_lines()
	write_file(source_path, source)
	defer {
		os.rm(source_path) or {}
	}
	_ = vrun_ok('-os windows -check', source_path)
}
