module driver

import os
import v3.pref

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

	assert !module_path_has_v_sources(outer)
	prefs := pref.Preferences{
		module_search_paths: [first_search_root, second_search_root]
	}
	assert resolve_global_module_path(&prefs, 'sample', 'sample') == valid
	os.write_file(os.join_path(outer, 'parts', 'outer.v'), 'module outer\n\npub fn value() int { return 2 }\n')!
	assert module_path_has_v_sources(outer)
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

	assert module_path_has_v_sources(module_root)
	prefs := pref.Preferences{
		module_search_paths: [root]
	}
	assert resolve_global_module_path(&prefs, 'sample', 'sample') == module_root
}
