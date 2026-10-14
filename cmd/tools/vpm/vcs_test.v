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

// save_git_config_env and restore_git_config_env keep these tests from leaking
// GIT_CONFIG_* variables, or the HOME and XDG_CONFIG_HOME config locations, into the
// rest of the test process.
fn save_git_config_env() map[string]string {
	mut saved := map[string]string{}
	for name in ['GIT_CONFIG_COUNT', 'GIT_CONFIG_KEY_0', 'GIT_CONFIG_VALUE_0', 'GIT_CONFIG_KEY_2',
		'GIT_CONFIG_VALUE_2', 'GIT_CONFIG_PARAMETERS', 'GIT_CONFIG_GLOBAL', 'GIT_CONFIG_NOSYSTEM',
		'HOME', 'XDG_CONFIG_HOME'] {
		saved[name] = os.getenv(name)
	}
	return saved
}

fn restore_git_config_env(saved map[string]string) {
	for name, value in saved {
		if value == '' {
			os.unsetenv(name)
		} else {
			os.setenv(name, value, true)
		}
	}
}

// A count that cannot be extended must be left alone: rewriting it would overwrite
// the user's own config entries, or hide them from git. GIT_CONFIG_PARAMETERS is
// separate, so the setting still reaches git through it.
fn test_enable_git_longpaths_leaves_an_invalid_count_alone() {
	saved := save_git_config_env()
	defer {
		restore_git_config_env(saved)
	}
	for invalid in ['-1', 'two', '2 ', '+2', '99999999999999999999', '2147483647'] {
		os.unsetenv('GIT_CONFIG_KEY_0')
		os.unsetenv('GIT_CONFIG_PARAMETERS')
		os.setenv('GIT_CONFIG_COUNT', invalid, true)
		enable_git_longpaths()
		assert os.getenv('GIT_CONFIG_COUNT') == invalid, invalid
		assert os.getenv('GIT_CONFIG_KEY_0') == '', invalid
		assert os.getenv('GIT_CONFIG_PARAMETERS') == "'core.longpaths=true'", invalid
	}
}

fn test_enable_git_longpaths_appends_after_existing_entries() {
	saved := save_git_config_env()
	defer {
		restore_git_config_env(saved)
	}
	os.unsetenv('GIT_CONFIG_KEY_2')
	os.setenv('GIT_CONFIG_COUNT', '2', true)
	os.setenv('GIT_CONFIG_PARAMETERS', "'core.autocrlf=false'", true)
	enable_git_longpaths()
	assert os.getenv('GIT_CONFIG_COUNT') == '3'
	assert os.getenv('GIT_CONFIG_KEY_2') == 'core.longpaths'
	assert os.getenv('GIT_CONFIG_VALUE_2') == 'true'
	assert os.getenv('GIT_CONFIG_PARAMETERS') == "'core.autocrlf=false' 'core.longpaths=true'"
	os.unsetenv('GIT_CONFIG_COUNT')
	os.unsetenv('GIT_CONFIG_KEY_0')
	os.unsetenv('GIT_CONFIG_PARAMETERS')
	enable_git_longpaths()
	assert os.getenv('GIT_CONFIG_COUNT') == '1'
	assert os.getenv('GIT_CONFIG_KEY_0') == 'core.longpaths'
	assert os.getenv('GIT_CONFIG_PARAMETERS') == "'core.longpaths=true'"
}

// Git before 2.31 ignores GIT_CONFIG_COUNT, but every git version reads
// GIT_CONFIG_PARAMETERS. The COUNT entries are dropped after enabling, so only
// GIT_CONFIG_PARAMETERS can carry core.longpaths to git.
fn test_enable_git_longpaths_reaches_git_without_config_count() {
	saved := save_git_config_env()
	defer {
		restore_git_config_env(saved)
	}
	root := os.join_path(os.vtmp_dir(), 'vpm_longpaths_${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	os.setenv('GIT_CONFIG_PARAMETERS', "'core.autocrlf=false'", true)
	enable_git_longpaths()
	os.unsetenv('GIT_CONFIG_COUNT')
	os.unsetenv('GIT_CONFIG_KEY_0')
	os.unsetenv('GIT_CONFIG_VALUE_0')
	// Isolate git from the machine's own config, which may already set core.longpaths.
	// GIT_CONFIG_GLOBAL is only read by git 2.32 and later, so HOME and XDG_CONFIG_HOME
	// also move the user config into the test root for the older git this test is for.
	empty_config := os.join_path(root, 'empty.gitconfig')
	os.write_file(empty_config, '')!
	os.setenv('HOME', root, true)
	os.setenv('XDG_CONFIG_HOME', root, true)
	os.setenv('GIT_CONFIG_NOSYSTEM', '1', true)
	os.setenv('GIT_CONFIG_GLOBAL', empty_config, true)
	res := os.exec(['git', '-C', root, 'config', '--get', 'core.longpaths'])
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == 'true'
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
