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
		['module config', '', 'pub struct Config {}', '', 'pub fn default_config() Config {',
			'\treturn Config{}', '}'].join_lines() +
			'\n'
	decoder_contents := ['module decoder', '', 'pub struct Decoder {}'].join_lines() + '\n'
	to_contents := ['module msgpack.to', '', 'pub fn new_decoder(src []u8) {}'].join_lines() + '\n'
	msgpack_contents :=
		['module msgpack', '',
			"pub const description = 'an empty module, used as a placeholder, for other modules'"].join_lines() +
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
		['module firebird', '', 'import einar_hjortdal.luuid', '',
			'pub fn run() string { return luuid.hello() }'].join_lines() +
			'\n'
	app_vmod := ['Module {', "\tname: 'app'", '}'].join_lines() + '\n'
	app_contents :=
		['module main', '', 'import einar_hjortdal.luuid', 'import einar_hjortdal.firebird', '',
			'fn main() {', '\tprintln(luuid.hello())', '\tprintln(firebird.run())', '}'].join_lines() +
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

// Regression test for https://github.com/vlang/v/issues/28810 :
// a module installed under a namespace (`.vmodules/smilecat/mod`) is imported
// as `mod.types` from inside itself and as `smilecat.mod.types` from outside.
// A sibling `mod/api/types` directory made the short name ambiguous from
// `mod/api` only, so the same directory was parsed twice, as two modules with
// incompatible types.
fn issue_28810_workspace() string {
	return os.join_path(os.vtmp_dir(), 'issue_28810_namespaced_submodule_collision')
}

fn issue_28810_write_project(api_contents string, app_files map[string]string) {
	workspace := issue_28810_workspace()
	module_root := os.join_path(workspace, '.vmodules', 'smilecat', 'mod')
	vmod_contents := ['Module {', "\tname: 'mod'", "\tversion: '0.0.1'", '}'].join_lines() + '\n'
	types_contents :=
		['module types', '', 'pub struct Thing {', 'pub mut:', '\tvalue int', '}'].join_lines() +
			'\n'
	api_types_contents :=
		['module types', '', 'pub struct Thing {', 'pub mut:', '\ttag int', '}'].join_lines() +
			'\n'
	os.rmdir_all(workspace) or {}
	os.mkdir_all(os.join_path(module_root, 'types')) or { panic(err) }
	os.mkdir_all(os.join_path(module_root, 'api', 'types')) or { panic(err) }
	issue_20147_write_file(os.join_path(module_root, 'v.mod'), vmod_contents)
	issue_20147_write_file(os.join_path(module_root, 'types', 'types.v'), types_contents)
	issue_20147_write_file(os.join_path(module_root, 'api', 'types', 'types.v'),
		api_types_contents)
	issue_20147_write_file(os.join_path(module_root, 'api', 'api.v'), api_contents)
	for rel_path, contents in app_files {
		path := os.join_path(workspace, 'app', rel_path)
		os.mkdir_all(os.dir(path)) or { panic(err) }
		issue_20147_write_file(path, contents)
	}
}

fn issue_28810_run(api_contents string, app_files map[string]string) os.Result {
	issue_28810_write_project(api_contents, app_files)
	old_vmodules, had_vmodules := issue_20147_env_snapshot('VMODULES')
	os.setenv('VMODULES', os.join_path(issue_28810_workspace(), '.vmodules'), true)
	defer {
		issue_20147_restore_env('VMODULES', old_vmodules, had_vmodules)
		os.rmdir_all(issue_28810_workspace()) or {}
	}
	main_file := os.join_path(issue_28810_workspace(), 'app', 'main.v')
	return os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(main_file)}')
}

fn test_issue_28810_namespaced_module_keeps_one_identity_beside_colliding_submodule() {
	api_contents :=
		['module api', '', 'import mod.types', '', 'pub fn take(t types.Thing) int {', '\treturn t.value',
			'}'].join_lines() +
			'\n'
	main_contents :=
		['module main', '', 'import smilecat.mod.types', 'import smilecat.mod.api', '', 'fn main() {',
			'\tprintln(api.take(types.Thing{ value: 42 }))', '}'].join_lines() +
			'\n'
	res := issue_28810_run(api_contents, {
		'main.v': main_contents
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '42', res.output
}

fn test_issue_28810_namespaced_submodules_with_same_short_name_stay_distinct() {
	api_contents :=
		['module api', '', 'import mod.types', 'import mod.api.types as itypes', '',
			'pub fn take(t types.Thing) int {', '\treturn t.value', '}', '',
			'pub fn inner(t itypes.Thing) int {', '\treturn t.tag', '}'].join_lines() +
			'\n'
	main_contents :=
		['module main', '', 'import smilecat.mod.types', 'import smilecat.mod.api',
			'import smilecat.mod.api.types as itypes', '', 'fn main() {',
			'\tprintln(api.take(types.Thing{ value: 42 }))',
			'\tprintln(api.inner(itypes.Thing{ tag: 7 }))', '}'].join_lines() +
			'\n'
	res := issue_28810_run(api_contents, {
		'main.v': main_contents
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '42\n7', res.output
}

fn test_issue_28810_namespaced_module_keeps_one_identity_when_inner_path_is_parsed_first() {
	// `mod/api` (and its `import mod.types`) is parsed before `helper`, the only
	// importer of the `smilecat.mod.types` spelling.
	api_contents :=
		['module api', '', 'import mod.types', '', 'pub fn take(t types.Thing) int {', '\treturn t.value',
			'}'].join_lines() +
			'\n'
	helper_contents :=
		['module helper', '', 'import smilecat.mod.types', '', 'pub fn make(value int) types.Thing {',
			'\treturn types.Thing{', '\t\tvalue: value', '\t}', '}'].join_lines() +
			'\n'
	main_contents :=
		['module main', '', 'import smilecat.mod.api', 'import helper', '', 'fn main() {',
			'\tprintln(api.take(helper.make(42)))', '}'].join_lines() +
			'\n'
	res := issue_28810_run(api_contents, {
		'main.v':                           main_contents
		os.join_path('helper', 'helper.v'): helper_contents
	})
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '42', res.output
}

fn test_issue_28810_symlinked_module_dir_still_checks_module_declarations() {
	workspace := os.join_path(os.vtmp_dir(), 'issue_28810_symlinked_module_alias')
	vmodules := os.join_path(workspace, '.vmodules')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(os.join_path(vmodules, 'foo')) or { panic(err) }
	os.mkdir_all(os.join_path(workspace, 'app')) or { panic(err) }
	defer {
		os.rmdir_all(workspace) or {}
	}
	issue_20147_write_file(os.join_path(vmodules, 'foo', 'foo.v'),
		'module foo\n\npub const answer = 42\n')
	os.symlink(os.join_path(vmodules, 'foo'), os.join_path(vmodules, 'bar')) or {
		$if windows {
			eprintln('skipping symlinked module declaration regression test: ${err}')
			return
		} $else {
			panic(err)
		}
	}
	main_file := os.join_path(workspace, 'app', 'main.v')
	issue_20147_write_file(main_file,
		['module main', '', 'import foo', 'import bar', '', 'fn main() {',
			'\tprintln(foo.answer + bar.answer)', '}'].join_lines() + '\n')
	old_vmodules, had_vmodules := issue_20147_env_snapshot('VMODULES')
	os.setenv('VMODULES', vmodules, true)
	defer {
		issue_20147_restore_env('VMODULES', old_vmodules, had_vmodules)
	}
	// `bar` is the parsed `foo` directory under another name, but its files
	// declare `module foo`, so importing it as `bar` must still be rejected.
	res := os.execute('${os.quoted_path(issue_20147_vexe)} run ${os.quoted_path(main_file)}')
	assert res.exit_code != 0, res.output
	assert res.output.contains('bad module definition'), res.output
	assert res.output.contains('imports module "bar"'), res.output
}
