module main

import os
import rand

fn test_parse_git_version() {
	if _ := parse_git_version('abcd') {
		assert false
	}
	assert parse_git_version('git version 2.44.0.windows.1')! == '2.44.0'
	assert parse_git_version('git version 2.34.0')! == '2.34.0'
	assert parse_git_version('git version 2.39.3 (Apple Git-146)')! == '2.39.3'
	assert parse_git_version('git version 2.51.1.dirty')! == '2.51.1'
}

fn test_clone_args() {
	url := 'https://example.com/mod.git'
	path := '/tmp/mod'
	head_args := VCS.git.clone_args(url, '', path)!
	tag_args := VCS.git.clone_args(url, 'v0.1.0', path)!
	assert tag_args.len == head_args.len + 2
	assert tag_args.contains('--branch=v0.1.0')
	assert tag_args.filter(it == '--branch=v0.1.0').len == 1
	branch := tag_args[tag_args.index('--branch=v0.1.0')]
	assert branch == '--branch=v0.1.0'
	assert !branch.contains(url)
	assert !branch.contains(path)
	assert tag_args.contains(url)
	assert tag_args.contains(path)
	payload := r'$(touch /tmp/v-advisory/pwned)'
	payload_args := VCS.git.clone_args(url, payload, path)!
	assert payload_args.len == head_args.len + 2
	assert payload_args.contains('--branch=${payload}')
	assert payload_args.filter(it.contains(payload)).len == 1
	packed := 'v1 --upload-pack=echo'
	packed_args := VCS.git.clone_args(url, packed, path)!
	assert packed_args.len == head_args.len + 2
	assert packed_args.contains('--branch=${packed}')
	assert !packed_args.contains('--upload-pack=echo')
	assert !packed_args.contains(packed)
	if _ := VCS.git.clone_args(url, 'v0.1.0\n', path) {
		assert false
	}
	// A source that looks like an option is never passed on to git.
	if _ := VCS.git.clone_args('--upload-pack=touch pwned', '', path) {
		assert false
	}
	assert head_args.filter(it.starts_with('--branch')).len == 0
	assert !head_args.contains('-b')
	assert !head_args.contains('--single-branch')
}

fn test_git_clone_contains_historical_blobs() {
	root := os.join_path(os.vtmp_dir(), 'vpm_clone_blobs_${rand.ulid()}')
	repo := os.join_path(root, 'source')
	clone := os.join_path(root, 'clone')
	os.mkdir_all(repo)!
	defer {
		os.rmdir_all(root) or {}
	}
	vcs_test_git(repo, ['init'])
	// Use the file transport so Git honors filtering, unlike a plain local-path clone.
	vcs_test_git(repo, ['config', 'uploadpack.allowFilter', 'true'])
	os.write_file(os.join_path(repo, 'module.v'), 'module example\n\nconst value = 1\n')!
	vcs_test_git(repo, ['add', 'module.v'])
	vcs_test_git(repo, ['commit', '-m', 'original module'])
	os.write_file(os.join_path(repo, 'module.v'), 'module example\n\nconst value = 2\n')!
	vcs_test_git(repo, ['commit', '-am', 'updated module'])
	mut normalized_path := repo.replace('\\', '/')
	if !normalized_path.starts_with('/') {
		normalized_path = '/${normalized_path}'
	}
	url := 'file://${normalized_path}'
	VCS.git.clone(url, '', clone)!
	// Checking out HEAD fetches its blobs even in a partial clone. Inspect all history to
	// detect omitted blobs without allowing Git to fetch them lazily during the check.
	objects := vcs_test_git(clone, ['rev-list', '--objects', '--all', '--missing=print'])
	missing := objects.split_into_lines().filter(it.starts_with('?'))
	assert missing.len == 0, 'clone omitted objects: ${missing}'
}

fn vcs_test_git(repo string, args []string) string {
	mut command := ['git', '-C', repo, '-c', 'user.name=VPM test', '-c',
		'user.email=vpm-test@example.com']
	command << args
	result := os.exec(command)
	assert result.exit_code == 0, result.output
	return result.output
}
