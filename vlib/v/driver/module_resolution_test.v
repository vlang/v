module driver

import os
import v.pref
import v.flat

fn test_manifest_subdir_test_without_local_source_includes_whole_module() {
	root := os.join_path(os.vtmp_dir(), 'v3_module_test_only_subdir_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'src', 'tests'))!
	os.mkdir_all(os.join_path(root, 'src', 'parts'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'sample', base_url: 'src', subdirs: ['tests', 'parts'] }\n")!
	root_file := os.join_path(root, 'src', 'sample.v')
	part_file := os.join_path(root, 'src', 'parts', 'part.v')
	test_file := os.join_path(root, 'src', 'tests', 'sample_test.v')
	os.write_file(root_file, 'module sample\nfn root_value() int { return 40 }\n')!
	os.write_file(part_file, 'module sample\nfn part_value() int { return 2 }\n')!
	os.write_file(test_file, 'module sample\nfn test_value() {}\n')!
	os.write_file(os.join_path(root, 'src', 'parts', 'unselected_test.v'), 'module sample\nfn test_unselected() {}\n')!
	prefs := pref.new_preferences()
	mut a := flat.FlatAst.new()
	files := same_dir_module_source_files(mut a, test_file, 'sample', prefs)
	assert files.len == 2
	assert os.real_path(root_file) in files
	assert os.real_path(part_file) in files
	assert test_file !in files
}

fn test_shadow_explicit_roots_preserves_filesystem_root() {
	root := os.real_path(os.path_separator)
	prefs := pref.Preferences{
		module_search_paths: [root]
	}
	assert shadow_explicit_roots_for(&prefs, []) == [root]
}

fn test_manifest_subdir_probe_stops_at_nested_modules() {
	root := os.join_path(os.vtmp_dir(), 'v3_module_probe_nested_${os.getpid()}')
	os.rmdir_all(root) or {}
	first_search_root := os.join_path(root, 'first')
	second_search_root := os.join_path(root, 'second')
	outer := os.join_path(first_search_root, 'sample')
	valid := os.join_path(second_search_root, 'sample')
	os.mkdir_all(os.join_path(outer, 'parts', 'nested')) or { panic(err) }
	os.mkdir_all(valid) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(outer, 'v.mod'), "Module { name: 'outer', subdirs: ['parts'] }\n")!
	os.write_file(os.join_path(outer, 'parts', 'nested', 'v.mod'), "Module { name: 'nested' }\n")!
	os.write_file(os.join_path(outer, 'parts', 'nested', 'nested.v'), 'module nested\n\npub fn value() int { return 1 }\n')!
	os.write_file(os.join_path(valid, 'sample.v'), 'module sample\n\npub fn value() int { return 2 }\n')!

	prefs := pref.Preferences{
		module_search_paths: [first_search_root, second_search_root]
	}
	assert !module_path_has_v_sources(outer, &prefs)
	assert resolve_global_module_path(&prefs, 'sample', 'sample') == valid
	os.write_file(os.join_path(outer, 'parts', 'outer.v'), 'module outer\n\npub fn value() int { return 2 }\n')!
	assert module_path_has_v_sources(outer, &prefs)
}

fn test_manifest_subdir_probe_uses_configured_source_root() {
	root := os.join_path(os.vtmp_dir(), 'v3_module_probe_base_url_${os.getpid()}')
	os.rmdir_all(root) or {}
	module_root := os.join_path(root, 'sample')
	source_subdir := os.join_path(module_root, 'src', 'core')
	os.mkdir_all(source_subdir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(module_root, 'v.mod'), "Module { name: 'sample', base_url: 'src', subdirs: ['core'] }\n")!
	os.write_file(os.join_path(source_subdir, 'core.v'), 'module sample\n\npub fn value() int { return 1 }\n')!

	prefs := pref.Preferences{
		module_search_paths: [root]
	}
	assert module_path_has_v_sources(module_root, &prefs)
	assert resolve_global_module_path(&prefs, 'sample', 'sample') == module_root
}

fn test_manifest_subdir_probe_filters_sources_for_target() {
	root := os.join_path(os.vtmp_dir(), 'v3_module_probe_target_${os.getpid()}')
	os.rmdir_all(root) or {}
	first_search_root := os.join_path(root, 'first')
	second_search_root := os.join_path(root, 'second')
	incompatible := os.join_path(first_search_root, 'sample')
	parts := os.join_path(incompatible, 'parts')
	valid := os.join_path(second_search_root, 'sample')
	os.mkdir_all(parts) or { panic(err) }
	os.mkdir_all(valid) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(incompatible, 'v.mod'), "Module { name: 'sample', subdirs: ['parts'] }\n")!
	os.write_file(os.join_path(parts, 'sample_windows.c.v'), 'module sample\n')!
	os.write_file(os.join_path(parts, 'sample_d_missing.v'), 'module sample\n')!
	os.write_file(os.join_path(parts, 'sample_test.v'), 'module sample\n')!
	os.write_file(os.join_path(parts, 'sample.js.v'), 'module sample\n')!
	os.write_file(os.join_path(valid, 'sample.v'), 'module sample\n')!
	linux := pref.target_from('linux', 'amd64') or { panic(err) }
	prefs := pref.Preferences{
		target:              linux
		module_search_paths: [first_search_root, second_search_root]
	}

	assert !module_path_has_v_sources(incompatible, &prefs)
	assert resolve_global_module_path(&prefs, 'sample', 'sample') == valid
}

fn test_declared_module_is_read_from_the_head_of_a_file() {
	root := os.join_path(os.vtmp_dir(), 'v3_declared_module_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'source.v')
	body := 'fn main() {\n\tprintln(1)\n}\n'.repeat(200)
	for source, expected in {
		'module alpha\n':                                       'alpha'
		'module alpha':                                         'alpha'
		'\n\n  module beta // the module\n':                    'beta'
		'// a comment\r\n/* one */ module gamma /* two */\r\n': 'gamma'
		'/*\nmodule hidden\n*/\nmodule delta\n':                'delta'
		'@[has_globals]\nmodule epsilon\n':                     'epsilon'
		'@[has_globals;\n  translated]\nmodule zeta\n':         'zeta'
		'// old style line ends\rmodule eta\rfn f() {}\r':      'eta'
		'// only a comment\n':                                  ''
		'fn first() {}\nmodule late\n':                         ''
		'import os\nmodule late\n':                             ''
		'':                                                     ''
	} {
		os.write_file(path, source)!
		assert declared_module_in_file(path) == expected, source
		if source.len == 0 || source[source.len - 1] in [`\n`, `\r`] {
			// The rest of a file changes nothing: the first line of code decides.
			os.write_file(path, source + body)!
			assert declared_module_in_file(path) == expected, source
		}
	}
	assert declared_module_in_file(os.join_path(root, 'missing.v')) == ''
}
