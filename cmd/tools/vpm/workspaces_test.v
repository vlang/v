module main

import os
import v.vmod

fn make_workspace_fixture() string {
	root := os.join_path(os.vtmp_dir(), 'vpm_workspace_test_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'packages', 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'packages', 'b')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'tools')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'root' workspaces: ['packages/*', 'tools'] dependencies: ['foo'] }") or { panic(err) }
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'), "Module { name: 'a' dependencies: ['bar'] }") or { panic(err) }
	os.write_file(os.join_path(root, 'packages', 'b', 'v.mod'), "Module { name: 'b' }") or { panic(err) }
	os.write_file(os.join_path(root, 'tools', 'v.mod'), "Module { name: 'tools' dependencies: ['baz'] }") or { panic(err) }
	return root
}

fn test_workspace_member_dirs_basic() {
	root := make_workspace_fixture()
	defer { os.rmdir_all(root) or {} }
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := workspace_member_dirs(root, manifest)
	assert dirs.len == 3
}

fn test_workspace_member_dirs_no_workspaces() {
	root := os.join_path(os.vtmp_dir(), 'vpm_workspace_nowork_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'root' }") or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	manifest := vmod.from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := workspace_member_dirs(root, manifest)
	assert dirs.len == 0
}

fn test_nearest_workspace_root() {
	root := make_workspace_fixture()
	defer { os.rmdir_all(root) or {} }
	member := os.join_path(root, 'packages', 'a')
	assert nearest_workspace_root(member) == os.real_path(root)
}

fn test_workspace_dependencies() {
	root := make_workspace_fixture()
	defer { os.rmdir_all(root) or {} }
	deps := workspace_dependencies(root)
	assert 'foo' in deps
	assert 'bar' in deps
	assert 'baz' in deps
}
