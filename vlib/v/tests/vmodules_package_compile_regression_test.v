import os

@[markused]
const turn_off_vcolors = os.setenv('VCOLORS', 'never', true)

const issue_20147_vexe = @VEXE

fn issue_20147_env_snapshot(name string) (string, bool) {
	val := os.getenv_opt(name) or { return '', false }
	return val, true
}

fn issue_20147_restore_env(name string, value string, existed bool) {
	if existed {
		os.setenv(name, value, true)
	} else {
		os.unsetenv(name)
	}
}

fn issue_20147_workspace() string {
	return os.join_path(os.vtmp_dir(), 'issue_20147_vmodules_package_compile')
}

fn issue_20147_module_root() string {
	return os.join_path(issue_20147_workspace(), '.vmodules', 'msgpack')
}

fn issue_20147_write_file(path string, contents string) {
	os.write_file(path, contents) or { panic(err) }
}

fn issue_20147_write_project() {
	basepath := issue_20147_module_root()
	vmod_contents := ['Module {', "\tname: 'msgpack'", '}'].join_lines() + '\n'
	config_contents :=
		['module config', '', 'pub struct Config {}', '', 'pub fn default_config() Config {', '\treturn Config{}', '}'].join_lines() +
		'\n'
	decoder_contents := ['module decoder', '', 'pub struct Decoder {}'].join_lines() + '\n'
	to_contents := ['module msgpack.to', '', 'pub fn new_decoder(src []u8) {}'].join_lines() + '\n'
	msgpack_contents :=
		['module msgpack', '', "pub const description = 'an empty module, used as a placeholder, for other modules'"].join_lines() +
		'\n'
	test_contents := ['fn test_a() {', '\tassert true', '}'].join_lines()
	import_config_test_contents := ['import msgpack.config', '', test_contents].join_lines() + '\n'
	import_decoder_test_contents := ['import decoder', '', test_contents].join_lines() + '\n'
	import_to_test_contents := ['import msgpack.config', '', test_contents].join_lines() + '\n'
	os.rmdir_all(issue_20147_workspace()) or {}
	os.mkdir_all(os.join_path(basepath, 'config')) or { panic(err) }
	os.mkdir_all(os.join_path(basepath, 'decoder')) or { panic(err) }
	os.mkdir_all(os.join_path(basepath, 'to')) or { panic(err) }
	issue_20147_write_file(os.join_path(basepath, 'v.mod'), vmod_contents)
	issue_20147_write_file(os.join_path(basepath, 'config', 'config.v'), config_contents)
	issue_20147_write_file(os.join_path(basepath, 'decoder', 'decoder.v'), decoder_contents)
	issue_20147_write_file(os.join_path(basepath, 'to', 'to.v'), to_contents)
	issue_20147_write_file(os.join_path(basepath, 'msgpack.v'), msgpack_contents)
	issue_20147_write_file(os.join_path(basepath, 'import_config_test.v'),
		import_config_test_contents)
	issue_20147_write_file(os.join_path(basepath, 'import_decoder_test.v'),
		import_decoder_test_contents)
	issue_20147_write_file(os.join_path(basepath, 'import_to_test.v'), import_to_test_contents)
}

fn test_issue_20147_vmodules_package_tests_compile() {
	issue_20147_write_project()
	old_wd := os.getwd()
	old_vmodules, had_vmodules := issue_20147_env_snapshot('VMODULES')
	os.setenv('VMODULES', os.join_path(issue_20147_workspace(), '.vmodules'), true)
	os.chdir(issue_20147_module_root()) or { panic(err) }
	defer {
		os.chdir(old_wd) or { panic(err) }
		issue_20147_restore_env('VMODULES', old_vmodules, had_vmodules)
		os.rmdir_all(issue_20147_workspace()) or {}
	}
	res := os.execute('${os.quoted_path(issue_20147_vexe)} test .')
	assert res.exit_code == 0, res.output
}

// Regression test for https://github.com/vlang/v/issues/27391 :
// modules installed as symlinks inside a `.vmodules` namespace folder
// (e.g. `.vmodules/einar_hjortdal/luuid -> /real/luuid`) were resolved to
// their real path via os.real_path and then rejected as belonging to a
// different v.mod project, so `import einar_hjortdal.luuid` could not be found.
fn issue_27391_workspace() string {
	return os.join_path(os.vtmp_dir(), 'issue_27391_symlinked_vmodules')
}

fn issue_27391_write_project() ! {
	workspace := issue_27391_workspace()
	vmodules_ns := os.join_path(workspace, '.vmodules', 'einar_hjortdal')
	real_luuid := os.join_path(workspace, 'real', 'luuid')
	real_firebird := os.join_path(workspace, 'real', 'firebird')
	app_root := os.join_path(workspace, 'app')
	luuid_vmod := ['Module {', "\tname: 'luuid'", '}'].join_lines() + '\n'
	luuid_contents :=
		['module luuid', '', "pub fn hello() string { return 'luuid-ok' }"].join_lines() + '\n'
	firebird_vmod := ['Module {', "\tname: 'firebird'", '}'].join_lines() + '\n'
	firebird_contents :=
		['module firebird', '', 'import einar_hjortdal.luuid', '', 'pub fn run() string { return luuid.hello() }'].join_lines() +
		'\n'
	app_vmod := ['Module {', "\tname: 'app'", '}'].join_lines() + '\n'
	app_contents :=
		['module main', '', 'import einar_hjortdal.luuid', 'import einar_hjortdal.firebird', '', 'fn main() {', '\tprintln(luuid.hello())', '\tprintln(firebird.run())', '}'].join_lines() +
		'\n'
	os.rmdir_all(workspace) or {}
	os.mkdir_all(vmodules_ns)!
	os.mkdir_all(real_luuid)!
	os.mkdir_all(real_firebird)!
	os.mkdir_all(app_root)!
	issue_20147_write_file(os.join_path(real_luuid, 'v.mod'), luuid_vmod)
	issue_20147_write_file(os.join_path(real_luuid, 'luuid.v'), luuid_contents)
	issue_20147_write_file(os.join_path(real_firebird, 'v.mod'), firebird_vmod)
	issue_20147_write_file(os.join_path(real_firebird, 'firebird.v'), firebird_contents)
	// install both real modules as symlinks inside the `einar_hjortdal` namespace
	os.symlink(real_luuid, os.join_path(vmodules_ns, 'luuid'))!
	os.symlink(real_firebird, os.join_path(vmodules_ns, 'firebird'))!
	issue_20147_write_file(os.join_path(app_root, 'v.mod'), app_vmod)
	issue_20147_write_file(os.join_path(app_root, 'main.v'), app_contents)
}

fn test_issue_27391_symlinked_namespaced_vmodules_import_compiles() {
	issue_27391_write_project() or {
		$if windows {
			eprintln('skipping symlinked vmodules import regression test: ${err}')
			return
		} $else {
			panic(err)
		}
	}
	old_vmodules, had_vmodules := issue_20147_env_snapshot('VMODULES')
	os.setenv('VMODULES', os.join_path(issue_27391_workspace(), '.vmodules'), true)
	defer {
		issue_20147_restore_env('VMODULES', old_vmodules, had_vmodules)
		os.rmdir_all(issue_27391_workspace()) or {}
	}
	main_file := os.join_path(issue_27391_workspace(), 'app', 'main.v')
	res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(main_file)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'luuid-ok\nluuid-ok', res.output
	app_dir := os.join_path(issue_27391_workspace(), 'app')
	dir_res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(app_dir)}')
	assert dir_res.exit_code == 0, dir_res.output
	assert dir_res.output.trim_space() == 'luuid-ok\nluuid-ok', dir_res.output
	old_wd := os.getwd()
	os.chdir(app_dir) or { panic(err) }
	dot_res := os.execute('${os.quoted_path(issue_20147_vexe)} run .')
	os.chdir(old_wd) or { panic(err) }
	assert dot_res.exit_code == 0, dot_res.output
	assert dot_res.output.trim_space() == 'luuid-ok\nluuid-ok', dot_res.output
}

fn issue_27281_boundary_workspace(case_name string) string {
	return os.join_path(os.vtmp_dir(), 'issue_27281_boundary_${case_name}')
}

fn issue_27281_write_boundary_project(case_name string, marker string, marker_is_dir bool) string {
	workspace := issue_27281_boundary_workspace(case_name)
	parent := os.join_path(workspace, 'parent')
	repo := os.join_path(parent, 'repo')
	foo_dir := os.join_path(repo, 'foo')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(foo_dir) or { panic(err) }
	parent_vmod := ['Module {', "\tname: 'parent'", '}'].join_lines() + '\n'
	issue_20147_write_file(os.join_path(parent, 'v.mod'), parent_vmod)
	if marker_is_dir {
		os.mkdir_all(os.join_path(repo, marker)) or { panic(err) }
	} else {
		issue_20147_write_file(os.join_path(repo, marker), '')
	}
	issue_20147_write_file(os.join_path(repo, 'main.v'),
		['module main', '', 'import foo', '', 'fn main() {', '\tprintln(foo.value())', '}'].join_lines() +
		'\n')
	issue_20147_write_file(os.join_path(repo, 'main_test.v'),
		['module main', '', 'import foo', '', 'fn test_foo_value() {', "\tassert foo.value() == 'boundary-ok'", '}'].join_lines() +
		'\n')
	issue_20147_write_file(os.join_path(foo_dir, 'foo.v'),
		['module foo', '', 'pub fn value() string {', "\treturn 'boundary-ok'", '}'].join_lines() +
		'\n')
	return workspace
}

fn issue_27281_assert_boundary_marker_stops_parent_vmod(case_name string, marker string, marker_is_dir bool) {
	workspace := issue_27281_write_boundary_project(case_name, marker, marker_is_dir)
	defer {
		os.rmdir_all(workspace) or {}
	}
	main_file := os.real_path(os.join_path(workspace, 'parent', 'repo', 'main.v'))
	res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(main_file)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'boundary-ok', res.output
	repo_dir := os.real_path(os.join_path(workspace, 'parent', 'repo'))
	dir_res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(repo_dir)}')
	assert dir_res.exit_code == 0, dir_res.output
	assert dir_res.output.trim_space() == 'boundary-ok', dir_res.output
	test_res := os.execute('${os.quoted_path(issue_20147_vexe)} test ${os.quoted_path(repo_dir)}')
	assert test_res.exit_code == 0, test_res.output
	old_wd := os.getwd()
	os.chdir(repo_dir) or { panic(err) }
	dot_run_res := os.execute('${os.quoted_path(issue_20147_vexe)} run .')
	dot_test_res := os.execute('${os.quoted_path(issue_20147_vexe)} test .')
	os.chdir(old_wd) or { panic(err) }
	assert dot_run_res.exit_code == 0, dot_run_res.output
	assert dot_run_res.output.trim_space() == 'boundary-ok', dot_run_res.output
	assert dot_test_res.exit_code == 0, dot_test_res.output
}

fn test_issue_27281_git_file_marker_stops_fallback_parent_vmod_scan() {
	issue_27281_assert_boundary_marker_stops_parent_vmod('git_file', '.git', false)
}

fn test_issue_27281_git_dir_marker_stops_fallback_parent_vmod_scan() {
	issue_27281_assert_boundary_marker_stops_parent_vmod('git_dir', '.git', true)
}

fn test_issue_27281_other_vcs_dir_markers_stop_fallback_parent_vmod_scan() {
	issue_27281_assert_boundary_marker_stops_parent_vmod('hg_dir', '.hg', true)
	issue_27281_assert_boundary_marker_stops_parent_vmod('svn_dir', '.svn', true)
}

fn test_issue_27281_vmod_stop_marker_stops_fallback_parent_vmod_scan() {
	issue_27281_assert_boundary_marker_stops_parent_vmod('vmod_stop', '.v.mod.stop', false)
}

fn issue_27281_base_url_workspace() string {
	return os.join_path(os.vtmp_dir(), 'issue_27281_uppercase_base_url')
}

fn issue_27281_write_uppercase_base_url_project() {
	workspace := issue_27281_base_url_workspace()
	source_dir := os.join_path(workspace, 'Source')
	foo_dir := os.join_path(source_dir, 'foo')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(foo_dir) or { panic(err) }
	app_vmod := ['Module {', "\tname: 'app'", "\tbase_url: 'Source'", '}'].join_lines() + '\n'
	issue_20147_write_file(os.join_path(workspace, 'v.mod'), app_vmod)
	issue_20147_write_file(os.join_path(source_dir, 'main.v'),
		['module main', '', 'import foo', '', 'fn main() {', '\tprintln(foo.value())', '}'].join_lines() +
		'\n')
	issue_20147_write_file(os.join_path(source_dir, 'main_test.v'),
		['module main', '', 'import foo', '', 'fn test_foo_value() {', "\tassert foo.value() == 'base-url-ok'", '}'].join_lines() +
		'\n')
	issue_20147_write_file(os.join_path(foo_dir, 'foo.v'),
		['module foo', '', 'pub fn value() string {', "\treturn 'base-url-ok'", '}'].join_lines() +
		'\n')
}

fn test_issue_27281_temp_project_allows_uppercase_base_url() {
	issue_27281_write_uppercase_base_url_project()
	defer {
		os.rmdir_all(issue_27281_base_url_workspace()) or {}
	}
	main_file := os.real_path(os.join_path(issue_27281_base_url_workspace(), 'Source', 'main.v'))
	res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(main_file)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'base-url-ok', res.output
	source_dir := os.real_path(os.join_path(issue_27281_base_url_workspace(), 'Source'))
	dir_res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(source_dir)}')
	assert dir_res.exit_code == 0, dir_res.output
	assert dir_res.output.trim_space() == 'base-url-ok', dir_res.output
	test_res := os.execute('${os.quoted_path(issue_20147_vexe)} test ${os.quoted_path(source_dir)}')
	assert test_res.exit_code == 0, test_res.output
	old_wd := os.getwd()
	os.chdir(source_dir) or { panic(err) }
	dot_run_res := os.execute('${os.quoted_path(issue_20147_vexe)} run .')
	dot_test_res := os.execute('${os.quoted_path(issue_20147_vexe)} test .')
	os.chdir(old_wd) or { panic(err) }
	assert dot_run_res.exit_code == 0, dot_run_res.output
	assert dot_run_res.output.trim_space() == 'base-url-ok', dot_run_res.output
	assert dot_test_res.exit_code == 0, dot_test_res.output
}

fn test_issue_27281_temp_project_allows_session_shaped_entry_folder() {
	workspace := os.join_path(os.vtmp_dir(), 'issue_27281_session_entry')
	defer {
		os.rmdir_all(workspace) or {}
	}
	cmd_dir := os.join_path(workspace, 'tsession_01ABCDEF23456789', 'Cmd')
	module_dir := os.join_path(workspace, 'dep', 'mymod')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(cmd_dir) or { panic(err) }
	os.mkdir_all(module_dir) or { panic(err) }
	root_vmod := ['Module {', "\tname: 'app'", '}'].join_lines() + '\n'
	dep_vmod := ['Module {', "\tname: 'dep'", '}'].join_lines() + '\n'
	main_source :=
		['module main', '', 'import dep.mymod', '', 'fn main() {', '\tprintln(mymod.value())', '}'].join_lines() +
		'\n'
	module_source :=
		['module mymod', '', 'pub fn value() string {', "\treturn 'uppercase-entry-ok'", '}'].join_lines() +
		'\n'
	issue_20147_write_file(os.join_path(workspace, 'v.mod'), root_vmod)
	issue_20147_write_file(os.join_path(workspace, 'dep', 'v.mod'), dep_vmod)
	issue_20147_write_file(os.join_path(cmd_dir, 'main.v'), main_source)
	issue_20147_write_file(os.join_path(module_dir, 'mymod.v'), module_source)
	res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(cmd_dir)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'uppercase-entry-ok', res.output
}

fn test_issue_27281_marker_bounded_module_directory_keeps_prefix() {
	workspace := os.join_path(os.vtmp_dir(), 'issue_27281_module_directory')
	defer {
		os.rmdir_all(workspace) or {}
	}
	project_dir := os.join_path(workspace, 'parent', 'project')
	foo_dir := os.join_path(project_dir, 'foo')
	bar_dir := os.join_path(foo_dir, 'bar')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(bar_dir) or { panic(err) }
	parent_vmod := ['Module {', "\tname: 'parent'", '}'].join_lines() + '\n'
	foo_test_source :=
		['\xef\xbb\xbf/* outer /* nested */ outer */', '@[has_globals]', 'module foo;', '', 'import foo.bar', '', 'fn test_module_names() {', "\tassert @MOD == 'foo'", "\tassert bar.module_name() == 'foo.bar'", '}'].join_lines() +
		'\n'
	bar_source :=
		['module bar', '', 'pub fn module_name() string {', '\treturn @MOD', '}'].join_lines() +
		'\n'
	issue_20147_write_file(os.join_path(workspace, 'parent', 'v.mod'), parent_vmod)
	issue_20147_write_file(os.join_path(project_dir, '.v.mod.stop'), '')
	issue_20147_write_file(os.join_path(foo_dir, 'foo_test.v'), foo_test_source)
	issue_20147_write_file(os.join_path(bar_dir, 'bar.v'), bar_source)
	res := os.execute('${os.quoted_path(issue_20147_vexe)} test ${os.quoted_path(foo_dir)}')
	assert res.exit_code == 0, res.output
	project_link := os.join_path(workspace, 'project_link')
	os.symlink(project_dir, project_link) or {
		$if windows {
			return
		} $else {
			panic(err)
		}
	}
	linked_foo_dir := os.join_path(project_link, 'foo')
	link_res :=
		os.execute('${os.quoted_path(issue_20147_vexe)} test ${os.quoted_path(linked_foo_dir)}')
	assert link_res.exit_code == 0, link_res.output
}

fn test_issue_27281_marker_bounded_external_test_file_keeps_prefix() {
	workspace := os.join_path(os.vtmp_dir(), 'issue_27281_external_test_file')
	defer {
		os.rmdir_all(workspace) or {}
	}
	project_dir := os.join_path(workspace, 'project')
	foo_dir := os.join_path(project_dir, 'foo')
	bar_dir := os.join_path(foo_dir, 'bar')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(bar_dir) or { panic(err) }
	foo_source :=
		['#!/usr/bin/env -S v run', 'module /* before name */ foo/* adjacent comment */', '', 'pub const present = true'].join_lines() +
		'\n'
	foo_test_source :=
		['module foo_test', '', 'import foo.bar', '', 'fn test_nested_module_name() {', "\tassert bar.module_name() == 'foo.bar'", '}'].join_lines() +
		'\n'
	bar_source :=
		['module bar', '', 'pub fn module_name() string {', '\treturn @MOD', '}'].join_lines() +
		'\n'
	issue_20147_write_file(os.join_path(project_dir, '.v.mod.stop'), '')
	issue_20147_write_file(os.join_path(foo_dir, 'foo.v'), foo_source)
	foo_test_file := os.join_path(foo_dir, 'foo_test.c.v')
	issue_20147_write_file(foo_test_file, foo_test_source)
	issue_20147_write_file(os.join_path(bar_dir, 'bar.v'), bar_source)
	dir_res := os.execute('${os.quoted_path(issue_20147_vexe)} test ${os.quoted_path(foo_dir)}')
	assert dir_res.exit_code == 0, dir_res.output
	file_res :=
		os.execute('${os.quoted_path(issue_20147_vexe)} test ${os.quoted_path(foo_test_file)}')
	assert file_res.exit_code == 0, file_res.output
}

fn test_issue_27281_marker_bounded_directory_ignores_inactive_module_sources() {
	workspace := os.join_path(os.vtmp_dir(), 'issue_27281_inactive_module_source')
	defer {
		os.rmdir_all(workspace) or {}
	}
	project_dir := os.join_path(workspace, 'project')
	foo_dir := os.join_path(project_dir, 'foo')
	bar_dir := os.join_path(foo_dir, 'bar')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(bar_dir) or { panic(err) }
	main_source :=
		['module main', '', 'import bar', '', 'fn main() {', '\tprintln(bar.module_name())', '}'].join_lines() +
		'\n'
	inactive_source := 'module foo\n'
	bar_source :=
		['module bar', '', 'pub fn module_name() string {', '\treturn @MOD', '}'].join_lines() +
		'\n'
	issue_20147_write_file(os.join_path(project_dir, '.v.mod.stop'), '')
	issue_20147_write_file(os.join_path(foo_dir, 'main.c.v'), main_source)
	issue_20147_write_file(os.join_path(foo_dir, 'foo.js.v'), inactive_source)
	issue_20147_write_file(os.join_path(bar_dir, 'bar.v'), bar_source)
	res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(foo_dir)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'bar', res.output
}
