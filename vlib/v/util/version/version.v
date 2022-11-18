module version

import os

pub const v_version = '0.3.2'

// vhash() returns the build string C.V_COMMIT_HASH . See cmd/tools/gen_vc.v .
pub fn vhash() string {
	mut buf := [50]u8{}
	buf[0] = 0
	unsafe {
		bp := &buf[0]
		C.snprintf(&char(bp), 50, c'%s', C.V_COMMIT_HASH)
		return tos_clone(bp)
	}
}

pub fn full_hash() string {
	build_hash := vhash()
	current_hash := githash(false)
	if build_hash == current_hash {
		return build_hash
	}
	return '${build_hash}.${current_hash}'
}

// full_v_version() returns the full version of the V compiler
pub fn full_v_version(is_verbose bool) string {
	if is_verbose {
		return 'V ${version.v_version} ${full_hash()}'
	}
	hash := githash(false)
	return 'V ${version.v_version} ${hash}'
}

// githash(x) returns the current git commit hash.
// When x is false, it is very fast - it just returns a predefined C constant.
// When x is true, it tries to get the current commit hash, by parsing the
// relevant files in the .git/ folder, or if that is not possible
// for example when using a V from a V binary release, that does not have .git/
// defaults to getting the predefined C constant again.
// Note: githash(true) must be called only when v detects that it builds itself.
// For all other programs, githash(false) should be used.
pub fn githash(should_get_from_filesystem bool) string {
	for {
		// The `for` construct here is used as a goto substitute.
		// The code in this function will break out of the `for`
		// if it detects an error and can not continue.
		if should_get_from_filesystem {
			vexe := os.getenv('VEXE')
			vroot := os.dir(vexe)
			// .git/HEAD
			git_head_file := os.join_path(vroot, '.git', 'HEAD')
			if !os.exists(git_head_file) {
				break
			}
			// 'ref: refs/heads/master' ... the current branch name
			head_content := os.read_file(git_head_file) or { break }
			mut current_branch_hash := head_content
			if head_content.starts_with('ref: ') {
				gcbranch_rel_path := head_content.replace('ref: ', '').trim_space()
				gcbranch_file := os.join_path(vroot, '.git', gcbranch_rel_path)
				// .git/refs/heads/master
				if !os.exists(gcbranch_file) {
					break
				}
				// get the full commit hash contained in the ref heads file
				branch_hash := os.read_file(gcbranch_file) or { break }
				current_branch_hash = branch_hash
			}
			desired_hash_length := 7
			if current_branch_hash.len > desired_hash_length {
				return current_branch_hash[0..desired_hash_length]
			}
		}
		break
	}
	mut buf := [50]u8{}
	buf[0] = 0
	unsafe {
		bp := &buf[0]
		C.snprintf(&char(bp), 50, c'%s', C.V_CURRENT_COMMIT_HASH)
		return tos_clone(bp)
	}
}
