module driver

import os
import v.parser
import v.pref

fn test_eager_discovery_preserves_initial_module_identity() {
	root := os.join_path(os.vtmp_dir(), 'v3_eager_initial_identity_${os.getpid()}')
	app_file := os.join_path(root, 'app', 'html', 'html.v')
	net_file := os.join_path(root, 'net', 'html', 'html.v')
	os.mkdir_all(os.dir(app_file))!
	os.mkdir_all(os.dir(net_file))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(app_file, 'module html\nimport net.html as net_html\n')!
	os.write_file(net_file, 'module html\n')!

	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(app_file)
	mut parsed_modules := {
		'builtin': true
		'main':    true
	}
	mut identity_dirs := map[string]string{}
	mut dir_identities := map[string]string{}
	initial_imports := imports_from_files(mut a, [app_file])
	seed_initial_modules(mut a, [app_file], initial_imports, mut parsed_modules,
		mut identity_dirs, mut dir_identities)
	assert identity_dirs['html'] == os.real_path(os.dir(app_file))

	prefs := pref.Preferences{
		module_search_paths: [root]
	}
	mut module_path_cache := map[string]string{}
	modules := discover_eager_selfhost_modules(a, &prefs, app_file, root, identity_dirs,
		mut parsed_modules, mut module_path_cache)
	assert modules.len == 1, modules.str()
	assert modules[0].identity == 'net.html', modules.str()
}

fn test_source_imports_fast_tracks_nested_interpolation_strings() {
	path := os.join_path(os.temp_dir(), 'v3_eager_import_scan_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	dollar := '$'
	source := 'module scan_fixture

import real_module

fn interpolate(value string) string {
	return value
}

const text = \'before ${dollar}{interpolate(\'it\\\'s\')}
import fake_module
after\'
'
	expected_interpolation := dollar + "{interpolate('it\\'s')}"
	assert source.contains(expected_interpolation)
	os.write_file(path, source) or { panic(err) }

	assert source_imports_fast(path) == ['real_module']
}
