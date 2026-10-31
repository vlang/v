module version

import os

// `vhash()` is the hash baked into the bootstrap `vc/v.c`; anything else that V
// compiles has an empty one, so the version string must fall back to the
// checked-out hash instead of reporting nothing.
fn test_vhash_is_empty_or_a_full_commit_hash() {
	hash := vhash()
	assert hash.len == 0 || hash.len >= 7
	if hash.len == 0 {
		// Nothing baked in, which is every non-bootstrap build.
		assert !hash.bytes().any(!it.is_hex_digit())
	}
}

fn test_full_hash_falls_back_to_the_current_hash() {
	build_hash := vhash()
	current := vcurrent_hash()
	if build_hash.len < 7 {
		assert full_hash() == current
	} else if current == '' || build_hash[..7] == current {
		assert full_hash() == build_hash
	} else {
		assert full_hash() == '${build_hash}.${current}'
	}
	assert full_hash().len >= 7
}

fn test_full_v_version_names_the_version_and_the_hash_it_reports() {
	assert v_version.contains('.')
	assert v_version.split('.').len == 3
	assert v_version.split('.').all(it.len > 0)
	assert full_v_version(false) == 'V ${v_version} ${vcurrent_hash()}'
	assert full_v_version(true) == 'V ${v_version} ${full_hash()}'
	assert full_v_version(false).starts_with('V ${v_version} ')
	// The only difference between the two forms is which hash they name.
	if full_hash() == vcurrent_hash() {
		assert full_v_version(false) == full_v_version(true)
	}
}

// `githash` walks a real checkout, so a linked worktree and a plain one must
// resolve through the same shared `commondir`.
fn test_githash_falls_back_to_a_worktree_commondir() {
	root := os.join_path(os.vtmp_dir(), 'version_commondir_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	common_dir := os.join_path(root, 'main', '.git')
	worktree_dir := os.join_path(common_dir, 'worktrees', 'linked')
	checkout := os.join_path_single(root, 'linked')
	os.mkdir_all(worktree_dir) or { panic(err) }
	os.mkdir_all(checkout) or { panic(err) }
	os.mkdir_all(os.join_path(common_dir, 'refs', 'heads')) or { panic(err) }
	os.write_file(os.join_path_single(checkout, '.git'), 'gitdir: ../main/.git/worktrees/linked\n') or {
		panic(err)
	}
	os.write_file(os.join_path_single(worktree_dir, 'commondir'), '../..\n') or { panic(err) }
	os.write_file(os.join_path_single(worktree_dir, 'HEAD'), 'ref: refs/heads/main\n') or { panic(err) }
	// The shared refs directory of the main checkout is what answers.
	os.write_file(os.join_path(common_dir, 'refs', 'heads', 'main'), '1234567890abcdef1234567890abcdef12345678\n') or {
		panic(err)
	}
	assert githash(checkout) or { panic(err) } == '1234567'
}
