module vmod

import os
import rand

fn test_workspace_member_dirs_expands_globs_and_sorts() {
	root := os.join_path(os.temp_dir(), 'vmod_ws_${rand.ulid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'packages', 'b')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'packages', 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'empty')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'),
		"Module { name: 'root' workspaces: ['packages/*', 'missing/*', '', '.'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'), "Module { name: 'a' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'b', 'v.mod'), "Module { name: 'b' }") or {
		panic(err)
	}
	manifest := from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := workspace_member_dirs(root, manifest)
	assert dirs == [os.real_path(os.join_path(root, 'packages', 'a')),
		os.real_path(os.join_path(root, 'packages', 'b'))]
}

fn test_workspace_member_dirs_skips_root_and_dedupes() {
	root := os.join_path(os.temp_dir(), 'vmod_ws_dedupe_${rand.ulid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'packages', 'a')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'),
		"Module { name: 'root' workspaces: ['packages/*', 'packages/a', '.'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'), "Module { name: 'a' }") or {
		panic(err)
	}
	manifest := from_file(os.join_path(root, 'v.mod')) or { panic(err) }
	dirs := workspace_member_dirs(root, manifest)
	assert dirs == [os.real_path(os.join_path(root, 'packages', 'a'))]
}

fn test_nearest_workspace_root_finds_innermost_and_returns_empty_outside() {
	root := os.join_path(os.temp_dir(), 'vmod_ws_near_${rand.ulid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	nested := os.join_path(root, 'packages', 'a', 'src')
	os.mkdir_all(nested) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'root' workspaces: ['packages/*'] }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'packages', 'a', 'v.mod'), "Module { name: 'a' }") or {
		panic(err)
	}
	assert nearest_workspace_root(nested) == os.real_path(root)
	assert nearest_workspace_root(os.join_path(root, 'packages', 'a')) == os.real_path(root)
	plain := os.join_path(os.temp_dir(), 'vmod_ws_plain_${rand.ulid()}')
	os.mkdir_all(plain) or { panic(err) }
	defer {
		os.rmdir_all(plain) or {}
	}
	assert nearest_workspace_root(plain) == ''
}
