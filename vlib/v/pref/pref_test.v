module pref

import os

// test_detect_vroot_from_subdir validates detect vroot from subdir behavior in v3 tests.
fn test_detect_vroot_from_subdir() {
	vroot := @VMODROOT
	v3_dir := os.join_path(vroot, 'vlib', 'v3')
	assert detect_vroot_from(v3_dir) == vroot
}

// test_detect_vroot_from_binary_path validates this v3 regression case.
fn test_detect_vroot_from_binary_path() {
	vroot := @VMODROOT
	v3_bin := os.join_path(vroot, 'vlib', 'v3', 'v3')
	assert detect_vroot_from(v3_bin) == vroot
}

fn test_get_module_path_resolves_alias_and_submodule() {
	root := os.join_path(os.temp_dir(), 'v3_pref_module_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	modules_dir := os.join_path_single(root, 'modules')
	canonical_dir := os.join_path_single(modules_dir, 'canonical')
	os.mkdir_all(os.join_path_single(canonical_dir, 'sub')) or { panic(err) }
	os.mkdir_all(os.join_path_single(modules_dir, 'legacy')) or { panic(err) }
	os.write_file(os.join_path_single(root, 'v.mod'), "Module { name: 'alias_test' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path_single(canonical_dir, 'canonical.v'), 'module canonical\n') or {
		panic(err)
	}
	os.write_file(os.join_path(canonical_dir, 'sub', 'sub.v'), 'module sub\n') or { panic(err) }
	os.write_file(os.join_path(modules_dir, 'legacy', 'alias.v'),
		"@[alias: '@VMODROOT/modules/canonical'] module legacy\n") or { panic(err) }
	main_file := os.join_path_single(root, 'main.v')
	os.write_file(main_file, 'module main\n') or { panic(err) }
	prefs := new_preferences()
	assert prefs.get_module_path('modules.legacy', main_file) == os.real_path(canonical_dir)
	assert prefs.get_module_path('modules.legacy.sub', main_file) == os.real_path(os.join_path_single(canonical_dir,
		'sub'))
}

// test_detect_vroot_from_outside_a_checkout pins the twin of the driver walk in
// https://github.com/vlang/v/issues/28583: the walk must reach a filesystem
// root rather than the relative `.` that `os.dir` returns for a bare Windows
// drive, which would otherwise match the current directory.
fn test_detect_vroot_from_outside_a_checkout() {
	outside := os.join_path(os.vtmp_dir(), 'v_pref_outside_${os.getpid()}', 'a', 'b')
	os.mkdir_all(outside) or { panic(err) }
	defer {
		os.rmdir_all(os.dir(os.dir(outside))) or {}
	}
	cwd := os.getwd()
	os.chdir(@VMODROOT) or { panic(err) }
	defer {
		os.chdir(cwd) or {}
	}
	detected := detect_vroot_from(os.join_path(outside, 'hello.v'))
	assert detected != '.'
	if detected.len > 0 {
		assert os.is_dir(os.join_path(detected, 'vlib', 'builtin'))
	}
}

// A `modules` directory is not a lookup root: what it holds is `modules.<name>`,
// even to the files inside it. A directory carrying a `v.mod` is a project root
// whatever it is named, though, and this walk is all some callers have -- the
// FastC backend asks here directly, without a project-root probe of its own.
fn test_get_module_path_skips_a_modules_namespace_but_not_a_project_root() {
	root := os.join_path(os.vtmp_dir(), 'v3_pref_modules_root_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	// A project whose own root happens to be named `modules`, with its entry file
	// a directory further down.
	project_root := os.join_path(root, 'checkout', 'modules')
	entry_dir := os.join_path(project_root, 'src')
	root_module := os.join_path(project_root, 'foo')
	os.mkdir_all(entry_dir) or { panic(err) }
	os.mkdir_all(root_module) or { panic(err) }
	os.write_file(os.join_path(project_root, 'v.mod'), "Module { name: 'modules' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root_module, 'foo.v'), 'module foo\n') or { panic(err) }
	entry_file := os.join_path(entry_dir, 'main.v')
	os.write_file(entry_file, 'module main\n') or { panic(err) }

	// The old namespace level: a project holding modules in `modules/`, one of
	// them importing another.
	legacy_root := os.join_path(root, 'legacy')
	legacy_namespace := os.join_path(legacy_root, 'modules')
	legacy_importer_dir := os.join_path(legacy_namespace, 'foo')
	legacy_neighbour := os.join_path(legacy_namespace, 'bar')
	os.mkdir_all(legacy_importer_dir) or { panic(err) }
	os.mkdir_all(legacy_neighbour) or { panic(err) }
	os.write_file(os.join_path(legacy_root, 'v.mod'), "Module { name: 'legacy' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(legacy_neighbour, 'bar.v'), 'module bar\n') or { panic(err) }
	legacy_importer := os.join_path(legacy_importer_dir, 'foo.v')
	os.write_file(legacy_importer, 'module foo\n') or { panic(err) }

	// A project with no manifest at all, whose root is named `modules`: nothing
	// but the sources in it says where the project is.
	bare_root := os.join_path(root, 'bare', 'modules')
	bare_entry_dir := os.join_path(bare_root, 'src')
	bare_module := os.join_path(bare_root, 'foo')
	os.mkdir_all(bare_entry_dir) or { panic(err) }
	os.mkdir_all(bare_module) or { panic(err) }
	os.write_file(os.join_path(bare_module, 'foo.v'), 'module foo\n') or { panic(err) }
	bare_entry := os.join_path(bare_entry_dir, 'main.v')
	os.write_file(bare_entry, 'module main\n') or { panic(err) }

	// A manifestless project whose entry sources are nested outside `modules/`.
	// The entry root, rather than a shallow scan of its parent, identifies the
	// sibling `modules/` directory as the retired lookup level.
	nested_root := os.join_path(root, 'nested')
	nested_entry_dir := os.join_path(nested_root, 'src')
	nested_namespace := os.join_path(nested_root, 'modules')
	nested_importer_dir := os.join_path(nested_namespace, 'foo')
	nested_neighbour := os.join_path(nested_namespace, 'bar')
	os.mkdir_all(nested_entry_dir) or { panic(err) }
	os.mkdir_all(nested_importer_dir) or { panic(err) }
	os.mkdir_all(nested_neighbour) or { panic(err) }
	os.write_file(os.join_path(nested_entry_dir, 'main.v'), 'module main\n') or { panic(err) }
	nested_importer := os.join_path(nested_importer_dir, 'foo.v')
	os.write_file(nested_importer, 'module foo\n') or { panic(err) }
	os.write_file(os.join_path(nested_neighbour, 'bar.v'), 'module bar\n') or { panic(err) }

	prefs := new_preferences()
	assert prefs.get_module_path('foo', entry_file) == os.real_path(root_module)
	assert prefs.get_module_path('foo', bare_entry) == os.real_path(bare_module)
	assert prefs.get_module_path('bar', legacy_importer) != os.real_path(legacy_neighbour)
	// By the name the layout gives it, the neighbour resolves from the project.
	assert prefs.get_module_path('modules.bar', legacy_importer) == os.real_path(legacy_neighbour)
	mut nested_prefs := new_preferences()
	nested_prefs.module_resolution_root = nested_entry_dir
	assert nested_prefs.get_module_path('bar', nested_importer) != os.real_path(nested_neighbour)
	assert nested_prefs.get_module_path('modules.bar', nested_importer) == os.real_path(nested_neighbour)

	// The rule itself: the namespace is the one with a project around it.
	assert is_retired_modules_namespace(legacy_namespace, legacy_root)
	assert is_retired_modules_namespace(nested_namespace, nested_entry_dir)
	assert !is_retired_modules_namespace(project_root, entry_dir)
	assert !is_retired_modules_namespace(bare_root, bare_entry_dir)
	assert !is_retired_modules_namespace(legacy_root, legacy_root)
}

fn test_retired_modules_namespace_follows_filesystem_case_semantics() {
	root := os.join_path(os.vtmp_dir(), 'v3_pref_modules_case_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	entry_dir := os.join_path(root, 'src')
	namespace := os.join_path(root, 'Modules')
	importer_dir := os.join_path(namespace, 'case_user_28575')
	neighbour := os.join_path(namespace, 'case_peer_28575')
	os.mkdir_all(entry_dir) or { panic(err) }
	os.mkdir_all(importer_dir) or { panic(err) }
	os.mkdir_all(neighbour) or { panic(err) }
	importer := os.join_path(importer_dir, 'user.v')
	os.write_file(importer, 'module case_user_28575\n') or { panic(err) }
	os.write_file(os.join_path(neighbour, 'peer.v'), 'module case_peer_28575\n') or { panic(err) }

	// On a case-sensitive filesystem `Modules` is an ordinary, distinct name.
	// Elsewhere the literal `modules` path reaches this same directory and the
	// retired-namespace rule must therefore apply to its actual spelling too.
	literal_namespace_resolves := os.is_dir(os.join_path(root, 'modules'))
	assert is_retired_modules_namespace(namespace, entry_dir) == literal_namespace_resolves
	if !literal_namespace_resolves {
		return
	}
	mut prefs := new_preferences()
	prefs.module_resolution_root = entry_dir
	assert prefs.get_module_path('case_peer_28575', importer) != os.real_path(neighbour)
	qualified := prefs.get_module_path('modules.case_peer_28575', importer)
	assert qualified.to_lower_ascii() == os.real_path(neighbour).to_lower_ascii()
}
