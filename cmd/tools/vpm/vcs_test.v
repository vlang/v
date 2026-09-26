module main

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
	assert head_args.filter(it.starts_with('--branch')).len == 0
	assert !head_args.contains('-b')
	assert !head_args.contains('--single-branch')
}
