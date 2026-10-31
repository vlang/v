module main

import os
import rand
import test_utils { cmd_ok_args }

const shallow_root = os.join_path(os.vtmp_dir(), 'vpu_${rand.hex(12)}')
const shallow_original_dir = os.getwd()
const shallow_tool = os.join_path(shallow_root, if os.user_os() == 'windows' {
	'vpm.exe'
} else {
	'vpm'
})

fn testsuite_begin() {
	os.mkdir_all(shallow_root)!
	test_utils.set_test_env(os.join_path(shallow_root, 'build'))
	os.setenv('VEXE', @VEXE, true)
	cmd_ok_args(@LOCATION, [@VEXE, '-cc', @CCOMPILER, '-gc', 'none', '-o', shallow_tool, os.dir(@FILE)])
}

fn testsuite_end() {
	os.chdir(shallow_original_dir)!
	os.rmdir_all(shallow_root) or {}
}

fn shallow_git(path string, args []string) string {
	return cmd_ok_args(@LOCATION, ['git', '-C', path, '-c', 'user.name=V CI', '-c',
		'user.email=ci@vlang.io', ...args]).output.trim_space()
}

fn shallow_repo(case string) !string {
	path := os.join_path(shallow_root, case, 'remote')
	os.mkdir_all(path)!
	shallow_git(path, ['init', '-b', 'main'])
	os.write_file(os.join_path(path, 'v.mod'), "Module { name: 'update_pkg' version: '1.0.0' }\n")!
	shallow_commit(path, 'initial')!
	app := os.join_path(shallow_root, case, 'app')
	os.mkdir_all(app)!
	os.chdir(app)!
	test_utils.set_test_env(os.join_path(shallow_root, case, 'store'))
	return path
}

fn shallow_commit(path string, text string) !string {
	os.write_file(os.join_path(path, 'content.txt'), text)!
	shallow_git(path, ['add', '.'])
	shallow_git(path, ['commit', '-m', text])
	return shallow_git(path, ['rev-parse', 'HEAD'])
}

fn shallow_query(repo string, version string) string {
	mut normalized := repo.replace('\\', '/')
	if !normalized.starts_with('/') {
		normalized = '/${normalized}'
	}
	return 'file://' + normalized + if version == '' {
		''
	} else {
		'@' + version
	}
}

fn shallow_install(case string, repo string, version string) string {
	cmd_ok_args(@LOCATION, [shallow_tool, 'install', shallow_query(repo, version)])
	installed := os.join_path(shallow_root, case, 'store', 'update_pkg')
	assert shallow_git(installed, ['rev-parse', '--is-shallow-repository']) == 'true'
	return installed
}

fn test_shallow_update_preserves_uncommitted_work() {
	case := 'dirty'
	repo := shallow_repo(case)!
	installed := shallow_install(case, repo, '')
	before := shallow_git(installed, ['rev-parse', 'HEAD'])
	os.write_file(os.join_path(installed, 'content.txt'), 'local work')!
	shallow_commit(repo, 'upstream work')!
	result := os.exec([shallow_tool, 'update', 'update_pkg'])
	assert os.read_file(os.join_path(installed, 'content.txt'))! == 'local work', result.output
	assert shallow_git(installed, ['rev-parse', 'HEAD']) == before
	assert result.exit_code != 0, result.output
}

fn test_shallow_update_preserves_unpublished_commits() {
	case := 'unpublished'
	repo := shallow_repo(case)!
	installed := shallow_install(case, repo, '')
	local := shallow_commit(installed, 'unpublished work')!
	shallow_commit(repo, 'upstream work')!
	result := os.exec([shallow_tool, 'update', 'update_pkg'])
	assert shallow_git(installed, ['rev-parse', 'HEAD']) == local, result.output
	assert os.read_file(os.join_path(installed, 'content.txt'))! == 'unpublished work'
	assert result.exit_code != 0, result.output
}

fn test_shallow_update_follows_the_configured_nondefault_branch() {
	case := 'topic'
	repo := shallow_repo(case)!
	shallow_git(repo, ['checkout', '-b', 'topic'])
	shallow_commit(repo, 'topic initial')!
	shallow_git(repo, ['checkout', 'main'])
	installed := shallow_install(case, repo, 'topic')
	shallow_git(repo, ['checkout', 'topic'])
	shallow_commit(repo, 'topic second')!
	latest := shallow_commit(repo, 'topic third')!
	shallow_git(repo, ['checkout', 'main'])
	result := cmd_ok_args(@LOCATION, [shallow_tool, 'update', 'update_pkg'])
	assert shallow_git(installed, ['rev-parse', 'HEAD']) == latest, result.output
	assert shallow_git(installed, ['symbolic-ref', '--short', 'HEAD']) == 'topic'
}

fn test_shallow_detached_update_uses_the_fetched_default_branch() {
	case := 'detached'
	repo := shallow_repo(case)!
	shallow_git(repo, ['tag', 'v1.0.0'])
	installed := shallow_install(case, repo, 'v1.0.0')
	shallow_commit(repo, 'main second')!
	latest := shallow_commit(repo, 'main third')!
	result := cmd_ok_args(@LOCATION, [shallow_tool, 'update', 'update_pkg'])
	assert shallow_git(installed, ['rev-parse', 'HEAD']) == latest, result.output
}

fn test_shallow_local_pin_is_preserved_from_a_nested_project_folder() {
	repo := shallow_repo('localpin')!
	tagged := shallow_git(repo, ['rev-parse', 'HEAD'])
	shallow_git(repo, ['tag', 'v1.0.0'])
	app := os.getwd()
	os.write_file(os.join_path(app, 'v.mod'), "Module { name: 'pinned_app' }\n")!
	cmd_ok_args(@LOCATION, [shallow_tool, 'install', '--local', shallow_query(repo, 'v1.0.0')])
	installed := os.join_path(app, 'update_pkg')
	assert shallow_git(installed, ['rev-parse', '--is-shallow-repository']) == 'true'
	before := os.read_file(lockfile_path(app))!
	shallow_commit(repo, 'main second')!
	shallow_commit(repo, 'main third')!
	nested := os.join_path(app, 'nested')
	os.mkdir_all(nested)!
	os.chdir(nested)!
	cmd_ok_args(@LOCATION, [shallow_tool, 'update', '--local', 'update_pkg'])
	assert shallow_git(installed, ['rev-parse', 'HEAD']) == tagged
	assert os.read_file(lockfile_path(app))! == before
}

fn test_shallow_manifest_pins_stay_fixed_without_a_lockfile() {
	for field in ['dependencies', 'dev_dependencies'] {
		case := 'manifest_' + field
		repo := shallow_repo(case)!
		tagged := shallow_git(repo, ['rev-parse', 'HEAD'])
		shallow_git(repo, ['tag', 'v1.0.0'])
		installed := shallow_install(case, repo, 'v1.0.0')
		app := os.getwd()
		os.write_file(os.join_path(app, 'v.mod'), "Module { name: 'pinned_app' ${field}: ['${shallow_query(repo, 'v1.0.0')}'] }\n")!
		os.rm(lockfile_path(app)) or {}
		shallow_commit(repo, 'main second')!
		shallow_commit(repo, 'main third')!
		result := cmd_ok_args(@LOCATION, [shallow_tool, 'update', 'update_pkg'])
		assert shallow_git(installed, ['rev-parse', 'HEAD']) == tagged, result.output
	}
}

fn shallow_project_relative_pin(case string) ! {
	repo := shallow_repo(case)!
	tagged := shallow_git(repo, ['rev-parse', 'HEAD'])
	shallow_git(repo, ['tag', 'v1.0.0'])
	app := os.getwd()
	os.write_file(os.join_path(app, 'v.mod'), "Module { name: 'pinned_app' }\n")!
	cmd_ok_args(@LOCATION, [shallow_tool, 'install', '--local', shallow_query(repo, 'v1.0.0')])
	installed := os.join_path(app, 'update_pkg')
	assert shallow_git(installed, ['rev-parse', '--is-shallow-repository']) == 'true'
	os.rm(lockfile_path(app))!
	shallow_commit(repo, 'main second')!
	shallow_commit(repo, 'main third')!
	nested := os.join_path(app, 'nested')
	os.mkdir_all(nested)!
	mut source := '../remote'
	if case == 'file_relativepin' {
		source = 'file://../remote'
	} else if case == 'tildepin' {
		// Reach the RAM fixture through a tilde path without changing HOME or writing there.
		home_depth := os.home_dir().split('/').filter(it != '').len
		source = '~/' + '../'.repeat(home_depth) + repo.trim_left('/')
	}
	os.write_file(os.join_path(app, 'v.mod'), "Module { name: 'pinned_app' dev_dependencies: ['${source}@v1.0.0'] }\n")!
	os.chdir(nested)!
	result := cmd_ok_args(@LOCATION, [shallow_tool, 'update', '--local', 'update_pkg'])
	assert shallow_git(installed, ['rev-parse', 'HEAD']) == tagged, '${source}: ${result.output}'
}

fn test_shallow_relative_manifest_pin_uses_the_local_project_root() {
	shallow_project_relative_pin('relativepin')!
}

fn test_shallow_relative_file_url_pin_uses_the_local_project_root() {
	shallow_project_relative_pin('file_relativepin')!
}

fn test_shallow_tilde_manifest_pin_expands_home_before_resolving() {
	$if !windows {
		shallow_project_relative_pin('tildepin')!
	}
}
