module main

import os
import rand

fn test_package_hash_ignores_vcs_metadata_and_frames_paths_and_contents() {
	root := os.join_path(os.temp_dir(), 'vpm_hash_' + rand.ulid())
	defer { os.rmdir_all(root) or {} }
	a := os.join_path(root, 'a')
	b := os.join_path(root, 'b')
	os.mkdir_all(os.join_path(a, '.git'))!
	os.mkdir_all(os.join_path(b, '.git'))!
	os.write_file(os.join_path(a, '.git', 'config'), 'clone metadata one')!
	os.write_file(os.join_path(b, '.git', 'config'), 'clone metadata two')!
	os.write_file(os.join_path(a, 'one'), 'ab')!
	os.write_file(os.join_path(a, 'two'), 'c')!
	os.write_file(os.join_path(b, 'one'), 'ab')!
	os.write_file(os.join_path(b, 'two'), 'c')!
	assert dir_sha256(a)! == dir_sha256(b)!
	assert dir_sha256(a)!.len == 64
	os.write_file(os.join_path(a, '.package-config'), 'include hidden package files')!
	assert dir_sha256(a)! != dir_sha256(b)!
	os.write_file(os.join_path(b, '.package-config'), 'include hidden package files')!
	assert dir_sha256(a)! == dir_sha256(b)!
	os.write_file(os.join_path(b, 'one'), 'a')!
	os.write_file(os.join_path(b, 'two'), 'bc')!
	assert dir_sha256(a)! != dir_sha256(b)!
	os.write_file(os.join_path(b, 'one'), 'ab')!
	os.write_file(os.join_path(b, 'two'), 'c')!
	os.rename(os.join_path(b, 'one'), os.join_path(b, 'renamed'))!
	assert dir_sha256(a)! != dir_sha256(b)!
	mut failed := false
	dir_sha256(os.join_path(root, 'missing')) or { failed = true }
	assert failed
}
