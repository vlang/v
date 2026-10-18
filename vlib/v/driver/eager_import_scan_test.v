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

fn eager_discovered_paths(root string, app_file string, user_defines []string) []string {
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
	prefs := pref.Preferences{
		module_search_paths: [root]
		user_defines:        user_defines
	}
	mut module_path_cache := map[string]string{}
	modules := discover_eager_selfhost_modules(a, &prefs, app_file, root, identity_dirs,
		mut parsed_modules, mut module_path_cache)
	return modules.map(it.path)
}

fn test_eager_discovery_leaves_skipped_backend_modules_to_the_resolver() {
	root := os.join_path(os.vtmp_dir(), 'v3_eager_skipped_backend_${os.getpid()}')
	app_file := os.join_path(root, 'app.v')
	gate_file := os.join_path(root, 'gate', 'gate.v')
	eval_file := os.join_path(root, 'v', 'eval', 'eval.v')
	os.mkdir_all(os.dir(gate_file))!
	os.mkdir_all(os.dir(eval_file))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(app_file, 'module main\nimport gate\n')!
	// The byte-level import scan cannot evaluate this gate, so it sees the import.
	os.write_file(gate_file, 'module gate\n\n\$if !skip_eval ? {\n\timport v.eval\n}\n')!
	os.write_file(eval_file, 'module eval\n')!

	assert eager_discovered_paths(root, app_file, []string{}) == ['gate', 'v.eval']
	// `skip_eval` drops the gated import, so only the parser may still ask for v.eval.
	assert eager_discovered_paths(root, app_file, ['skip_eval']) == ['gate']
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
