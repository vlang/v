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

fn vmodules_symlink_workspace() string {
	return os.join_path(os.vtmp_dir(), 'vmodules_symlink_import_compile')
}

fn vmodules_symlink_write_project() ! {
	workspace := vmodules_symlink_workspace()
	vmodules_dir := os.join_path(workspace, '.vmodules')
	real_module_root := os.join_path(workspace, 'real_vmarkdown')
	link_module_root := os.join_path(vmodules_dir, 'vmarkdown')
	app_root := os.join_path(workspace, 'app')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(vmodules_dir)!
	os.mkdir_all(real_module_root)!
	os.mkdir_all(app_root)!
	os.symlink(real_module_root, link_module_root)!
	module_contents :=
		['module vmarkdown', '', "pub fn ok() string { return 'ok' }"].join_lines() + '\n'
	app_vmod_contents := ['Module {', "\tname: 'app'", '}'].join_lines() + '\n'
	app_contents :=
		['module main', '', 'import vmarkdown', '', 'fn main() {', '\tprintln(vmarkdown.ok())', '}'].join_lines() +
		'\n'
	issue_20147_write_file(os.join_path(real_module_root, 'vmarkdown.v'), module_contents)
	issue_20147_write_file(os.join_path(app_root, 'v.mod'), app_vmod_contents)
	issue_20147_write_file(os.join_path(app_root, 'main.v'), app_contents)
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

fn test_vmodules_symlinked_package_import_compiles() {
	vmodules_symlink_write_project() or {
		$if windows {
			eprintln('skipping symlink import regression test: ${err}')
			return
		} $else {
			panic(err)
		}
	}
	old_vmodules, had_vmodules := issue_20147_env_snapshot('VMODULES')
	os.setenv('VMODULES', os.join_path(vmodules_symlink_workspace(), '.vmodules'), true)
	defer {
		issue_20147_restore_env('VMODULES', old_vmodules, had_vmodules)
		os.rmdir_all(vmodules_symlink_workspace()) or {}
	}
	main_file := os.join_path(vmodules_symlink_workspace(), 'app', 'main.v')
	res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(main_file)}')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'ok'
}
