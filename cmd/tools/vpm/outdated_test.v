// vtest retry: 3
// vtest build: !windows
module main

import os
import rand
import test_utils { cmd_ok_args }

const test_path = os.join_path(os.vtmp_dir(), 'vpm_outdated_test_${rand.ulid()}')

fn testsuite_begin() {
	$if !network ? {
		eprintln('> skipping ${@FILE}, when `-d network` is missing')
		exit(0)
	}
	dump(test_path)
	test_utils.set_test_env(test_path)
	os.mkdir_all(test_path)!
	os.chdir(test_path)!
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_is_outdated_git_module() {
	cmd_ok_args(@LOCATION, ['git', 'clone', 'https://github.com/vlang/libsodium.git'])
	assert !is_outdated('libsodium')
	cmd_ok_args(@LOCATION, ['git', '-C', 'libsodium', 'reset', '--hard', 'HEAD~'])
	assert is_outdated('libsodium')
	cmd_ok_args(@LOCATION, ['git', '-C', 'libsodium', 'pull'])
	assert !is_outdated('libsodium')
}

fn test_is_outdated_hg_module() {
	$if !check_mercurial_works ? {
		return
	}
	os.find_abs_path_of_executable('hg') or {
		eprintln('skipping test, since `hg` is not executable.')
		return
	}
	cmd_ok_args(@LOCATION, ['hg', 'clone', 'https://www.mercurial-scm.org/repo/hello'])
	assert !is_outdated('hello')
	cmd_ok_args(@LOCATION, ['hg', '--config', 'extensions.strip=', '-R', 'hello', 'strip', '-r',
		'tip'])
	assert is_outdated('hello')
	cmd_ok_args(@LOCATION, ['hg', '-R', 'hello', 'pull'])
	assert !is_outdated('hello')
}

// test_project_constraints_reads_the_project_vmod: the constraint on each module
// comes from the project's own v.mod, which is what makes the Upgradable column
// mean something.
fn test_project_constraints_reads_the_project_vmod() {
	chdir(test_path)!
	write_file(os.join_path(test_path, 'v.mod'), "Module {\n\tname: 'app'\n\tdependencies: ['lib@^1.0.0', 'other']\n}\n")!
	constraints := project_constraints()
	assert constraints['lib'] == '^1.0.0'
	assert constraints['other'] == ''
}

// test_project_constraints_without_a_vmod: a directory with no v.mod places no
// constraints, so every column falls back to Latest.
fn test_project_constraints_without_a_vmod() {
	chdir(test_path)!
	constraints := project_constraints()
	assert constraints.len == 0
}

// test_installed_version_reads_the_tag: the Current column has to be the tag the
// module is checked out at, not a branch head.
fn test_installed_version_reads_the_tag() {
	chdir(test_path)!
	os.mkdir_all('repo')!
	os.chdir('repo')!
	cmd_ok_args(@LOCATION, ['git', 'init', '-q'])
	cmd_ok_args(@LOCATION, ['git', 'commit', '-q', '--allow-empty', '-m', 'one'])
	cmd_ok_args(@LOCATION, ['git', 'tag', 'v1.0.0'])
	cmd_ok_args(@LOCATION, ['git', 'commit', '-q', '--allow-empty', '-m', 'two'])
	cmd_ok_args(@LOCATION, ['git', 'tag', 'v2.0.0'])
	v := installed_version('.') or {
		assert false, 'expected a tag'
		return
	}
	assert v == 'v2.0.0', v
}

// test_installed_version_falls_back_to_the_commit: a checkout that is not on a tag
// reports the short commit, so the column is never empty.
fn test_installed_version_falls_back_to_the_commit() {
	chdir(test_path)!
	os.mkdir_all('repo2')!
	os.chdir('repo2')!
	cmd_ok_args(@LOCATION, ['git', 'init', '-q'])
	cmd_ok_args(@LOCATION, ['git', 'commit', '-q', '--allow-empty', '-m', 'one'])
	v := installed_version('.') or {
		assert false, 'expected a commit'
		return
	}
	assert v.len == 7, v
}

fn test_outdated() {
	for m in ['pcre', 'libsodium', 'https://github.com/spytheman/vtray', 'nedpals.args'] {
		cmd_ok_args(@LOCATION, [vexe, 'install', '${m}'])
	}
	// "Outdate" previously installed. Leave out `libsodium`.
	for m in ['pcre', os.join_path('spytheman', 'vtray'), os.join_path('nedpals', 'args')] {
		cmd_ok_args(@LOCATION, ['git', '-C', '${m}', 'fetch', '--all'])
		cmd_ok_args(@LOCATION, ['git', '-C', '${m}', 'reset', '--hard', 'HEAD~'])
		assert is_outdated(m)
	}
	res := cmd_ok_args(@LOCATION, [vexe, 'outdated'])
	output := res.output.all_after('Outdated modules:')
	assert output.len > 0, output
	assert output.contains('pcre'), output
	assert output.contains('spytheman.vtray'), output
	assert output.contains('nedpals.args'), output
	assert !output.contains('libsodium'), output
}
