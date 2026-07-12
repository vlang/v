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
}
