module vgit

import os

// vgit_test covers the parsing and path helpers of the `vgit` bootstrapping tool.
// `prepare_vc_source`/`clone_or_pull`/`compile_oldv_if_needed` are not unit
// testable: they exist only to drive `git` and a C compiler.

fn test_line_to_timestamp_and_commit() {
	ts, commit := line_to_timestamp_and_commit('1561805697 5b7a1e84a4d283071d12cb86dc17aeda9b5306a8')
	assert ts == u64(1561805697)
	assert commit == '5b7a1e84a4d283071d12cb86dc17aeda9b5306a8'
}

fn test_line_to_timestamp_and_commit_reads_only_the_first_field() {
	// `git rev-list --timestamp` output is `<ts> <hash>`; a longer line keeps the
	// first field as the commit and drops the rest.
	ts, commit := line_to_timestamp_and_commit('100 a b c')
	assert ts == u64(100)
	assert commit == 'a'
}

// NOTE: a non numeric timestamp is silently coerced to 0 by string.u64(), it is
// not reported as an error.
fn test_line_to_timestamp_and_commit_coerces_unparsable_timestamp() {
	ts, commit := line_to_timestamp_and_commit('notanumber abc')
	assert ts == u64(0)
	assert commit == 'abc'
}

fn test_normalized_workpath_for_commit_names_a_worktree_after_the_commit() {
	assert normalized_workpath_for_commit('/tmp', '5b7a1e8').ends_with('v_at_5b7a1e8')
	assert normalized_workpath_for_commit('/tmp', 'master').ends_with('v_at_master')
}

fn test_normalized_workpath_for_commit_sanitizes_ref_characters() {
	// `^`, `-` and `/` are not valid inside a single path component on Windows.
	name := normalized_workpath_for_commit('/tmp', 'origin/feature-a^b')
	assert name.ends_with('${os.path_separator}v_at_origin_feature_a_b')
}

fn test_normalized_workpath_for_commit_resolves_the_base_dir() {
	workdir := os.join_path(os.vtmp_dir(), 'vgit_workpath_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	normalized := normalized_workpath_for_commit(workdir, 'master')
	assert normalized.starts_with(os.real_path(workdir))
	assert normalized == os.real_path(workdir) + os.path_separator + 'v_at_master'
	assert os.is_dir(workdir)
}

fn test_normalized_workpath_for_commit_resolves_symlinks_before_the_child_exists() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vgit_workpath_symlink_${os.getpid()}')
	workdir := os.join_path(root, 'workdir')
	os.mkdir_all(workdir)!
	defer { os.rmdir_all(root) or {} }
	link := os.join_path(root, 'link')
	os.symlink(workdir, link)!
	child := os.join_path(workdir, 'v_at_master')
	assert !os.exists(child)
	assert normalized_workpath_for_commit(link, 'master') == os.join_path(os.real_path(workdir),
		'v_at_master')
	// Existing worktree symlinks must still resolve to their targets.
	target := os.join_path(root, 'target')
	os.mkdir_all(target)!
	os.symlink(target, child)!
	assert normalized_workpath_for_commit(link, 'master') == os.real_path(target)
}

fn test_check_v_commit_timestamp_before_self_rebuilding_returns_for_both_eras() {
	// The warning branch only prints; neither branch is allowed to exit.
	check_v_commit_timestamp_before_self_rebuilding(1561805697)
	check_v_commit_timestamp_before_self_rebuilding(0)
}
