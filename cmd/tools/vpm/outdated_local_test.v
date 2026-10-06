module main

import os
import rand
import test_utils { cmd_ok_args }

// Every repository and project in this file is local and owned by the test.
const test_path = os.join_path(os.vtmp_dir(), 'vpm_outdated_local_${rand.ulid()}')

fn testsuite_begin() {
	test_utils.set_test_env(test_path)
	os.mkdir_all(test_path)!
}

fn testsuite_end() {
	os.chdir(os.temp_dir()) or {}
	os.rmdir_all(test_path) or {}
}

// test_project_constraints_reads_the_project_vmod: the constraint on each module
// comes from the project's own v.mod, which is what makes the Upgradable column
// mean something.
fn test_project_constraints_reads_the_project_vmod() {
	project := os.join_path(test_path, 'author_constraints')
	os.mkdir_all(project)!
	os.chdir(project)!
	os.write_file(os.join_path(project, 'v.mod'), "Module {\n\tname: 'app'\n\tdependencies: ['lib@^1.0.0', 'other']\n}\n")!
	constraints := project_constraints()
	assert constraints['lib'] == '^1.0.0'
	assert constraints['other'] == ''
	os.write_file(os.join_path(project, 'v.mod'), "Module {\n\tname: 'app'\n\tdependencies: ['git@host:repo.git', 'git@host:versioned.git@^2.0.0']\n}\n")!
	ssh := project_constraints()
	assert ssh['git@host:repo.git'] == ''
	assert ssh['git@host:versioned.git'] == '^2.0.0'
}

// test_project_constraints_without_a_vmod: a directory with no v.mod places no
// constraints, so available release tags are unconstrained.
fn test_project_constraints_without_a_vmod() {
	project := os.join_path(test_path, 'author_no_manifest')
	os.mkdir_all(project)!
	os.chdir(project)!
	constraints := project_constraints()
	assert constraints.len == 0
}

// test_installed_version_reads_the_tag: the Current column has to be the tag the
// module is checked out at, not a branch head.
fn test_installed_version_reads_the_tag() {
	os.chdir(test_path)!
	os.mkdir_all('repo')!
	os.chdir('repo')!
	cmd_ok_args(@LOCATION, ['git', 'init', '-q'])
	cmd_ok_args(@LOCATION, ['git', '-c', 'user.email=ci@vlang.io', '-c', 'user.name=V CI', 'commit',
		'-q', '--allow-empty', '-m', 'one'])
	cmd_ok_args(@LOCATION, ['git', 'tag', 'v1.0.0'])
	cmd_ok_args(@LOCATION, ['git', '-c', 'user.email=ci@vlang.io', '-c', 'user.name=V CI', 'commit',
		'-q', '--allow-empty', '-m', 'two'])
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
	os.chdir(test_path)!
	os.mkdir_all('repo2')!
	os.chdir('repo2')!
	cmd_ok_args(@LOCATION, ['git', 'init', '-q'])
	cmd_ok_args(@LOCATION, ['git', '-c', 'user.email=ci@vlang.io', '-c', 'user.name=V CI', 'commit',
		'-q', '--allow-empty', '-m', 'one'])
	v := installed_version('.') or {
		assert false, 'expected a commit'
		return
	}
	assert v.len == 7, v
}

fn local_outdated_git(path string, args ...string) string {
	return cmd_ok_args(@LOCATION, ['git', '-C', path, '-c', 'user.email=ci@vlang.io', '-c',
		'user.name=V CI', ...args]).output.trim_space()
}

fn local_outdated_repo(name string) !string {
	path := os.join_path(test_path, name)
	os.mkdir_all(path)!
	local_outdated_git(path, 'init', '-q', '-b', 'main')
	os.write_file(os.join_path(path, 'v.mod'), "Module { name: 'lib' }")!
	local_outdated_git(path, 'add', 'v.mod')
	local_outdated_git(path, 'commit', '-q', '-m', 'initial')
	local_outdated_git(path, 'tag', 'v1.0.0')
	return path
}

fn test_rows_use_new_upstream_tags_and_exclude_unrequested_prereleases() {
	os.chdir(test_path)!
	origin := local_outdated_repo('row_origin')!
	checkout := os.join_path(test_path, 'row_checkout')
	cmd_ok_args(@LOCATION, ['git', 'clone', '-q', origin, checkout])
	for tag in ['v2.0.0', 'v10.0.0', 'v11.0.0-alpha'] {
		local_outdated_git(origin, 'commit', '-q', '--allow-empty', '-m', tag)
		local_outdated_git(origin, 'tag', tag)
	}
	before := local_outdated_git(checkout, 'rev-parse', 'HEAD')
	assert !local_outdated_git(checkout, 'tag').contains('v2.0.0')
	row := outdated_row('lib', checkout, {
		'lib': '^2.0.0'
	})
	assert row.current == 'v1.0.0'
	assert row.upgradable == 'v2.0.0'
	assert row.resolvable == 'v2.0.0'
	assert row.latest == 'v10.0.0'
	direct := outdated_row('lib', checkout, {
		origin: '^2.0.0'
	})
	assert direct.upgradable == 'v2.0.0'
	assert direct.resolvable == 'v2.0.0'
	file_url := outdated_row('lib', checkout, {
		'file://${origin}': '^2.0.0'
	})
	assert file_url.upgradable == 'v2.0.0'
	assert file_url.resolvable == 'v2.0.0'
	bare := outdated_row('lib', checkout, map[string]string{})
	assert bare.upgradable == 'v10.0.0'
	assert bare.resolvable == 'v10.0.0'
	assert local_outdated_git(checkout, 'rev-parse', 'HEAD') == before
	assert !local_outdated_git(checkout, 'tag').contains('v2.0.0')
}

fn test_rows_keep_unsatisfied_invalid_and_git_ref_states_separate_from_latest() {
	os.chdir(test_path)!
	repo := local_outdated_repo('row_constraints')!
	for constraint in ['^2.0.0', '^invalid', 'topic'] {
		row := outdated_row('lib', repo, {
			'lib': constraint
		})
		expected := match constraint {
			'^2.0.0' { 'none' }
			'^invalid' { 'invalid' }
			else { 'n/a' }
		}
		assert row.upgradable == expected
		assert row.resolvable == expected
		assert row.latest == 'v1.0.0'
	}
	for constraint in ['', 'v1.0.0', '1.0.0'] {
		row := outdated_row('lib', repo, {
			'lib': constraint
		})
		assert row.upgradable == 'v1.0.0'
		assert row.resolvable == 'v1.0.0'
	}
	local_outdated_git(repo, 'tag', '-d', 'v1.0.0')
	untagged := outdated_row('lib', repo, map[string]string{})
	assert untagged.current == local_outdated_git(repo, 'rev-parse', '--short', 'HEAD')
	assert untagged.latest == 'none'
	assert untagged.upgradable == 'none'
	assert untagged.resolvable == 'none'
	missing := outdated_row('lib', os.join_path(test_path, 'missing_repo'), map[string]string{})
	assert missing.current == 'n/a'
	assert missing.latest == 'n/a'
	assert missing.upgradable == 'n/a'
	assert missing.resolvable == 'n/a'
}

fn test_existing_commit_based_upgrade_detection_handles_branch_and_detached_checkouts() {
	os.chdir(test_path)!
	origin := local_outdated_repo('commit_origin')!
	checkout := os.join_path(test_path, 'commit_checkout')
	cmd_ok_args(@LOCATION, ['git', 'clone', '-q', origin, checkout])
	assert !is_outdated(checkout)
	local_outdated_git(origin, 'commit', '-q', '--allow-empty', '-m', 'new upstream commit')
	assert is_outdated(checkout)
	local_outdated_git(checkout, 'pull', '-q', '--ff-only')
	assert !is_outdated(checkout)
	local_outdated_git(checkout, 'checkout', '-q', '--detach', 'v1.0.0')
	assert is_outdated(checkout)
	pinned := os.join_path(test_path, 'pinned_checkout')
	cmd_ok_args(@LOCATION, ['git', 'clone', '-q', '--single-branch', '--branch', 'v1.0.0', origin,
		pinned])
	assert !is_outdated(pinned)
}
