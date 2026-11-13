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
	dirs := workspace_member_dirs(root, manifest)
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
	dirs := workspace_member_dirs(root, manifest)
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
	dirs := workspace_member_dirs(root, manifest)
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
	dirs := workspace_member_dirs(root, manifest)
	assert dirs.len == 1, 'duplicate patterns and `.` must collapse to one member, got ${dirs}'
	assert dirs[0] == os.real_path(os.join_path(root, 'packages', 'a'))
}

fn test_nearest_workspace_root() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	member := os.join_path(root, 'packages', 'a')
	assert nearest_workspace_root(member) == os.real_path(root)
}

fn test_nearest_workspace_root_from_nested_subdir() {
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	nested := os.join_path(root, 'packages', 'a', 'src', 'nested')
	os.mkdir_all(nested) or { panic(err) }
	assert nearest_workspace_root(nested) == os.real_path(root)
}

fn test_nearest_workspace_root_none_outside() {
	root := os.join_path(os.temp_dir(), 'vpm_workspace_outside_${rand.ulid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	assert nearest_workspace_root(root) == ''
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
	deps := workspace_dependencies(root)
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
	deps := workspace_dependencies(root)
	assert deps == ['foo', 'shared', 'bar', 'baz'], 'got ${deps}'
}

fn test_project_deps_for_manifest_plain_and_workspace() {
	plain := vmod.decode("Module { name: 'plain' dependencies: ['a'] }") or { panic(err) }
	assert project_deps_for_manifest('/tmp/plain', plain) == ['a']
	root := make_workspace_fixture()
	defer {
		os.rmdir_all(root) or {}
	}
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	deps := project_deps_for_manifest(root, manifest)
	assert 'foo' in deps
	assert 'bar' in deps
	assert 'baz' in deps
}
