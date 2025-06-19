module version

import os

pub const v_version = '0.4.11'

pub fn full_hash() string {
	build_hash := vhash()

	if vcurrent_hash() == '' || build_hash[..7] == vcurrent_hash() {
		return build_hash
	}
	return '${build_hash}.${vcurrent_hash()}'
}

// full_v_version() returns the full version of the V compiler
pub fn full_v_version(is_verbose bool) string {
	if is_verbose {
		return 'V ${v_version} ${full_hash()}'
	}
	return 'V ${v_version} ${vcurrent_hash()}'
}

// githash tries to find the current git commit hash for the specified
// project path by parsing the relevant files in its `.git/` folder.
pub fn githash(path string) !string {
	// .git/HEAD
	git_head_file := os.join_path(path, '.git', 'HEAD')
	if !os.exists(git_head_file) {
		return error('failed to find `${git_head_file}`')
	}
	// 'ref: refs/heads/master' ... the current branch name
	head_content := os.read_file(git_head_file) or {
		return error('failed to read `${git_head_file}`')
	}
	current_branch_hash := if head_content.starts_with('ref: ') {
		rev_rel_path := head_content.replace('ref: ', '').trim_space()
		rev_file := os.join_path(path, '.git', rev_rel_path)
		// .git/refs/heads/master
		if !os.exists(rev_file) {
			return error('failed to find revision file `${rev_file}`')
		}
		// get the full commit hash contained in the ref heads file
		os.read_file(rev_file) or { return error('failed to read revision file `${rev_file}`') }
	} else {
		head_content
	}
	desired_hash_length := 7
	return current_branch_hash[0..desired_hash_length] or {
		error('failed to limit hash `${current_branch_hash}` to ${desired_hash_length} characters')
	}
}
