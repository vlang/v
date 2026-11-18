module main

import os
import rand
import v.vmod

fn make_workspace_fixture() string {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_test_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'packages', 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'packages', 'b')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'tools')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'),
		"Module { name: 'root' workspaces: ['packages/*', 'tools'] dependencies: ['foo'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'),
		"Module { name: 'a' dependencies: ['bar'] }") or { panic(err) }
	os.write_file(os.join_path(root, 'packages', 'b', 'v.mod'), "Module { name: 'b' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'tools', 'v.mod'),
		"Module { name: 'tools' dependencies: ['baz'] }") or { panic(err) }
	return root
}

fn test_workspace_member_dirs_basic() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := vmod.workspace_member_dirs(root, manifest)
	assert dirs.len == 3, 'expected 3 members, got ${dirs}'
	assert dirs == dirs.sorted(), 'member dirs should be sorted'
	assert os.real_path(os.join_path(root, 'packages', 'a')) in dirs
	assert os.real_path(os.join_path(root, 'packages', 'b')) in dirs
	assert os.real_path(os.join_path(root, 'tools')) in dirs
}

fn test_workspace_member_dirs_no_workspaces() {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_nowork_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'root' }") or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := vmod.workspace_member_dirs(root, manifest)
	assert dirs.len == 0
}

fn test_workspace_member_dirs_ignores_empty_and_missing() {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_edge_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'packages', 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'empty_dir')) or { panic(err) }
	// `empty_dir` has no v.mod and must be skipped; missing globs match nothing.
	os.write_file(os.join_path(root, 'v.mod'),
		"Module { name: 'root' workspaces: ['', '  ', 'packages/*', 'nope/*', 'empty_dir'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'), "Module { name: 'a' }") or {
		panic(err)
	}
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := vmod.workspace_member_dirs(root, manifest)
	assert dirs.len == 1, 'expected only packages/a, got ${dirs}'
	assert dirs[0] == os.real_path(os.join_path(root, 'packages', 'a'))
}

fn test_workspace_member_dirs_dedupes_and_excludes_root() {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_dedupe_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'packages', 'a')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'),
		"Module { name: 'root' workspaces: ['packages/*', 'packages/a', 'packages/a', '.'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'), "Module { name: 'a' }") or {
		panic(err)
	}
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := vmod.workspace_member_dirs(root, manifest)
	assert dirs.len == 1, 'duplicate patterns and `.` must collapse to one member, got ${dirs}'
	assert dirs[0] == os.real_path(os.join_path(root, 'packages', 'a'))
}

fn test_nearest_workspace_root() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	member := os.join_path(root, 'packages', 'a')
	assert vmod.nearest_workspace_root(member) == os.real_path(root)
}

fn test_nearest_workspace_root_from_nested_subdir() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	nested := os.join_path(root, 'packages', 'a', 'src', 'nested')
	os.mkdir_all(nested) or { panic(err) }
	assert vmod.nearest_workspace_root(nested) == os.real_path(root)
}

fn test_nearest_workspace_root_none_outside() {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_outside_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	assert vmod.nearest_workspace_root(root) == ''
}

fn test_workspace_root_variants() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	canon := os.real_path(root)
	member := os.join_path(root, 'packages', 'a')
	nested := os.join_path(member, 'src')
	os.mkdir_all(nested) or { panic(err) }
	// Workspace root, member dir, and nested subdir all resolve to the root.
	assert workspace_root(root) == canon
	assert workspace_root(member) == canon
	assert workspace_root(nested) == canon
	// A standalone project resolves to itself.
	plain := os.join_path(os.temp_dir(), 'vpm_workspace_plain_${rand.ulid()}')
	os.mkdir_all(plain) or { panic(err) }
	defer {
		os.rmdir_all(plain) or {}
	}
	os.write_file(os.join_path(plain, 'v.mod'), "Module { name: 'plain' }") or { panic(err) }
	assert workspace_root(plain) == os.real_path(plain)
	// No v.mod anywhere resolves to ''.
	empty := os.join_path(os.temp_dir(), 'vpm_workspace_empty_${rand.ulid()}')
	os.mkdir_all(empty) or { panic(err) }
	defer {
		os.rmdir_all(empty) or {}
	}
	assert workspace_root(empty) == ''
}

fn test_current_project_from_member_dir() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	old := os.getwd()
	member := os.join_path(root, 'packages', 'a')
	os.chdir(member) or { panic(err) }
	defer {
		os.chdir(old) or {}
	}
	assert current_project_root() == os.real_path(root)
	found_root, manifest := current_project()
	assert found_root == os.real_path(root)
	assert manifest.name == 'root'
	assert manifest.workspaces.len == 2
}

fn test_workspace_dependencies() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	deps := workspace_dependencies(root) or { panic(err) }
	assert 'foo' in deps
	assert 'bar' in deps
	assert 'baz' in deps
}

fn test_workspace_dependencies_dedupes_and_orders() {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_order_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'packages', 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'packages', 'b')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'),
		"Module { name: 'root' workspaces: ['packages/*'] dependencies: ['foo', 'shared'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'),
		"Module { name: 'a' dependencies: ['shared', 'bar'] }") or { panic(err) }
	os.write_file(os.join_path(root, 'packages', 'b', 'v.mod'),
		"Module { name: 'b' dependencies: ['baz', 'foo'] }") or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	deps := workspace_dependencies(root) or { panic(err) }
	assert deps == ['foo', 'shared', 'bar', 'baz'], 'got ${deps}'
}

fn test_project_deps_for_manifest_plain_and_workspace() {
	plain := vmod.decode("Module { name: 'plain' dependencies: ['a'] }") or { panic(err) }
	assert project_deps_for_manifest('/tmp/plain', plain) or { panic(err) } == ['a']
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	deps := project_deps_for_manifest(root, manifest) or { panic(err) }
	assert 'foo' in deps
	assert 'bar' in deps
	assert 'baz' in deps
}

fn test_workspace_manifests_includes_root_and_members_in_order() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	members := workspace_manifests(root, manifest) or { panic(err) }
	assert members.len == 4
	assert members[0].dir == root
	assert members[0].manifest.name == 'root'
	assert members.map(it.manifest.name) == ['root', 'a', 'b', 'tools']
}

fn test_workspace_manifests_plain_project_has_only_root() {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_single_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'plain' }") or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	members := workspace_manifests(root, manifest) or { panic(err) }
	assert members.len == 1
	assert members[0].dir == root
	assert members[0].manifest.name == 'plain'
}

fn test_widen_for_latest() {
	assert widen_for_latest('foo', [], map[string]string{}, '') == 'foo@*'
	assert widen_for_latest('foo@^1.0.0', [], map[string]string{}, '') == 'foo@*'
	assert widen_for_latest('foo', ['foo'], {
		'foo': ''
	}, '') == 'foo@*'
	assert widen_for_latest('baz', ['foo'], {
		'foo': ''
	}, '') == 'baz'
	// Exact branch pins are not ranges and do not satisfy `*`: they stay pinned.
	assert widen_for_latest('foo@main', [], map[string]string{}, '') == 'foo@main'
	// A relative member requirement selects via its normalized identity but
	// keeps its original spelling when widened.
	rel_root := os.join_path(os.temp_dir(), 'vpm_widen_relative_${rand.ulid()}')
	os.mkdir_all(os.join_path(rel_root, 'deps', 'review_dep')) or { panic(err) }
	member_dir := os.join_path(rel_root, 'members', 'svc')
	os.mkdir_all(member_dir) or { panic(err) }
	defer {
		os.rmdir_all(rel_root) or {}
	}
	norm_key := lockfile_module_key(workspace_normalize_dep('file://../../deps/review_dep@^1',
		member_dir))
	assert widen_for_latest('file://../../deps/review_dep@^1', ['review_dep'], {
		norm_key: ''
	}, member_dir) == 'file://../../deps/review_dep@*'
	assert widen_for_latest('file://../../deps/review_dep@^1', ['other'], {
		'other': ''
	}, member_dir) == 'file://../../deps/review_dep@^1'
}

fn test_workspace_dependencies_fails_on_malformed_member() {
	root := os.join_path(os.temp_dir(), 'vpm_ws_bad_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'members', 'svc')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'),
		"Module { name: 'root' workspaces: ['members/*'] dependencies: ['foo'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'members', 'svc', 'v.mod'), "Module { name: 'svc' dependencies: ['bar'] }") or {
		panic(err)
	}
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	// Sanity: valid member aggregates.
	deps := workspace_dependencies_for_manifest(root, manifest) or { panic(err) }
	assert 'bar' in deps
	// Corrupting the member must abort instead of silently dropping it.
	os.write_file(os.join_path(root, 'members', 'svc', 'v.mod'), 'Module { name: !!! }') or {
		panic(err)
	}
	mut failed := false
	workspace_dependencies_for_manifest(root, manifest) or { failed = true }
	assert failed, 'malformed member manifest must abort resolution'
	mut manifests_failed := false
	workspace_manifests(root, manifest) or { manifests_failed = true }
	assert manifests_failed, 'malformed member manifest must abort workspace_manifests'
}

fn test_workspace_normalize_dep_preserves_declaring_dir() {
	root := os.join_path(os.temp_dir(), 'vpm_ws_local_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'deps', 'local_repo')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'members', 'svc', 'local_repo')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	root_dep := workspace_normalize_dep('./deps/local_repo@^1', root)
	assert !root_dep.starts_with('./'), 'root local must normalize, got ${root_dep}'
	assert root_dep.ends_with('@^1')
	assert root_dep.contains(os.real_path(os.join_path(root, 'deps', 'local_repo')))
	member_dir := os.join_path(root, 'members', 'svc')
	member_dep := workspace_normalize_dep('./local_repo@^1', member_dir)
	assert !member_dep.starts_with('./'), 'member local must normalize, got ${member_dep}'
	assert member_dep.contains(os.real_path(os.join_path(member_dir, 'local_repo')))
	assert root_dep != member_dep, 'different declaring dirs must not collapse'
	// Registered names and absolute locals pass through unchanged.
	assert workspace_normalize_dep('vsl@^1', root) == 'vsl@^1'
	abs_repo := os.real_path(root)
	assert workspace_normalize_dep(abs_repo + '@^1', root) == abs_repo + '@^1'
}

fn test_workspace_pending_requirements_preserves_member_requiring() {
	root := os.join_path(os.temp_dir(), 'vpm_ws_req_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'members', 'svc')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'root' workspaces: ['members/*'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'members', 'svc', 'v.mod'),
		"Module { name: 'svc' dependencies: ['review_dep@^1.0.0'] }") or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	pending := workspace_pending_requirements(['review_dep@^1.0.0'], root, manifest, 'root',
		'root') or { panic(err) }
	assert pending.len == 1, 'expected one edge, got ${pending}'
	assert pending[0].requiring == 'svc', 'member edge must keep its name, got ${pending[0].requiring}'
}
