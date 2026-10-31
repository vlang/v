module version

import os

pub const v_version = '0.5.2'

pub fn full_hash() string {
	build_hash := vhash()
	current_hash := vcurrent_hash()
	// `C.V_COMMIT_HASH` is only injected into the bootstrap `vc/v.c` by `gen_vc.v`,
	// so anything else compiled by V (`v self`, the `vdoctor`/`vup` tools, ...) sees
	// the empty `#define` fallback. There is no build hash to report then.
	if build_hash.len < 7 {
		return current_hash
	}
	if current_hash == '' || build_hash[..7] == current_hash {
		return build_hash
	}
	return '${build_hash}.${current_hash}'
}

// full_v_version() returns the full version of the V compiler
pub fn full_v_version(is_verbose bool) string {
	if is_verbose {
		return 'V ${v_version} ${full_hash()}'
	}
	return 'V ${v_version} ${vcurrent_hash()}'
}

// githash returns the current seven-character Git commit hash for a checkout.
// It supports ordinary and linked worktrees, with loose or packed branch refs.
pub fn githash(path string) !string {
	git_dir := checkout_git_dir(path)
	// .git/HEAD
	git_head_file := os.join_path(git_dir, 'HEAD')
	if !os.exists(git_head_file) {
		return error('failed to find `${git_head_file}`')
	}
	// 'ref: refs/heads/master' ... the current branch name
	head_content := os.read_file(git_head_file) or {
		return error('failed to read `${git_head_file}`')
	}
	current_branch_hash := if head_content.starts_with('ref: ') {
		rev_rel_path := head_content.replace('ref: ', '').trim_space()
		read_git_revision(git_common_dir(git_dir), rev_rel_path)!
	} else {
		head_content
	}
	desired_hash_length := 7
	return current_branch_hash[0..desired_hash_length] or {
		error('failed to limit hash `${current_branch_hash}` to ${desired_hash_length} characters')
	}
}

// read_git_revision reads a loose ref first, then an exact entry in packed-refs.
fn read_git_revision(common_dir string, reference string) !string {
	rev_file := os.join_path(common_dir, reference)
	if os.exists(rev_file) {
		return os.read_file(rev_file) or {
			error('failed to read revision file `${rev_file}`')
		}
	}
	packed_refs := os.read_file(os.join_path(common_dir, 'packed-refs')) or {
		return error('failed to find revision file `${rev_file}`')
	}
	for line in packed_refs.split_into_lines() {
		fields := line.fields()
		if fields.len == 2 && fields[1] == reference && fields[0].len in [40, 64]
			&& fields[0].bytes().all(it.is_hex_digit()) {
			return fields[0]
		}
	}
	return error('failed to find revision file `${rev_file}`')
}

// checkout_git_dir returns the git directory of the checkout at `path`. In a linked
// worktree, `.git` is a file that names it (`gitdir: ...`).
fn checkout_git_dir(path string) string {
	dot_git := os.join_path(path, '.git')
	if !os.is_file(dot_git) {
		return dot_git
	}
	content := os.read_file(dot_git) or { return dot_git }
	if !content.starts_with('gitdir:') {
		return dot_git
	}
	dir := content['gitdir:'.len..].trim_space()
	return if os.is_abs_path(dir) { dir } else { os.join_path(path, dir) }
}

// git_common_dir returns the directory holding the refs of `git_dir`. A linked worktree
// shares them with its main checkout, which its `commondir` file names.
fn git_common_dir(git_dir string) string {
	common := os.read_file(os.join_path(git_dir, 'commondir')) or { return git_dir }
	dir := common.trim_space()
	return if os.is_abs_path(dir) { dir } else { os.join_path(git_dir, dir) }
}
