module main

import os
import rand
import test_utils { cmd_ok_args, cmd_fail_args }

const range_test_path = os.join_path(os.vtmp_dir(), 'vpm_version_range_${rand.ulid()}')
const range_original_dir = os.getwd()
const range_vexe = @VEXE
const range_vpm_exe = os.join_path(range_test_path, if os.user_os() == 'windows' {
	'vpm.exe'
} else {
	'vpm'
})

fn testsuite_begin() {
	os.mkdir_all(range_test_path)!
	os.chdir(range_test_path)!
	test_utils.set_test_env(os.join_path(range_test_path, 'build_store'))
	os.setenv('VEXE', range_vexe, true)
	// Compile once, so changing the isolated module store does not rebuild the tool.
	cmd_ok_args(@LOCATION, [range_vexe, '-new-compiler', '-no-retry-compilation', '-cc', 'clang',
		'-gc', 'none', '-o', range_vpm_exe, os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm')])
}

fn testsuite_end() {
	os.chdir(range_original_dir)!
	os.rmdir_all(range_test_path) or {}
}

fn test_select_highest_semantic_version_tag() {
	tags := ['v1.9.0', 'main', 'v2.0.0', 'v1.10.0', 'v1.11.0-beta.1', 'v1.2.0', '1.3.0', 'v0.4.1',
		'v1.10.0+build.2']
	assert select_version_tag(tags, '^1.0.0')! == 'v1.10.0'
	assert select_version_tag(tags.reverse(), '^1.0.0')! == 'v1.10.0'
	assert select_version_tag(tags, '~1.2.0')! == 'v1.2.0'
	assert select_version_tag(tags, '>=1.2.0 <1.4.0')! == '1.3.0'
	assert select_version_tag(tags, '*')! == 'v2.0.0'
	assert select_version_tag(tags, '1.x')! == 'v1.10.0'
	assert select_version_tag(tags, '^0.4 || ^2')! == 'v2.0.0'
	assert select_version_tag(tags, '>=1.11.0-beta.0 <1.11.0')! == 'v1.11.0-beta.1'
	for constraint in ['^3.0.0', '>=1.0.0 <0.1.0'] {
		if tag := select_version_tag(tags, constraint) {
			assert false, '${constraint} selected ${tag}'
		} else {
			assert err.msg().contains('no semantic-version tag satisfies')
		}
	}
}

fn test_malformed_version_ranges_have_a_distinct_error_even_without_tags() {
	for tags in [[]string{}, ['v1.2.3']] {
		for constraint in ['^invalid', 'not-a-range', '^', '>=1.2 nope'] {
			if _ := select_version_tag(tags, constraint) {
				assert false, 'malformed range ${constraint} was accepted'
			} else {
				assert err.msg() == 'invalid version range `${constraint}`', err.msg()
			}
		}
	}
}

fn test_range_syntax_and_temporary_names() {
	for version in ['main', 'v1.2.3', '1.2.3', 'release/1.0', 'topic/a=b', 'topic/a|b', 'v1.2.3-rc.x',
		'1.2.3+build.x', 'v1.2.3+build.x', 'topic.x', 'release/1.x', '1.2.3.4.x', '1.topic.x',
		'x.branch', '.x'] {
		assert !is_version_range(version)
		assert version_tmp_name(version) == version
		assert VCS.git.resolve_version('--not-used-for-exact-refs', version)! == version
	}
	for constraint in ['^1.0', '~1.2', '>=1.0 <2.0', '*', 'x', 'X', '1.x', '1.X', '1.2.x', '1.2.X',
		'1.2 - 1.4', '^1 || ^2'] {
		assert is_version_range(constraint)
		tmp_name := version_tmp_name(constraint)
		assert !tmp_name.contains_any('<>^~|*/\\ \t')
		assert tmp_name == version_tmp_name(constraint)
	}
	assert version_tmp_name('^1') != version_tmp_name('^2')
	if _ := VCS.hg.resolve_version('unused', '^1') {
		assert false
	}
}

fn test_different_source_destination_conflicts_use_install_paths() {
	first := Module{
		name:          'pkg'
		requested:     'publisher.pkg@^1'
		version:       'v1.9.0'
		version_range: '^1'
		install_path:  os.join_path(range_test_path, 'pkg')
	}
	second := Module{
		name:         'pkg'
		requested:    'https://example.com/pkg@v2.0.0'
		version:      'v2.0.0'
		install_path: os.join_path(range_test_path, 'subdir', '..', 'pkg')
	}
	if _ := validate_resolved_destinations([first, second]) {
		assert false
	} else {
		assert err.msg().contains(first.requested)
		assert err.msg().contains(second.requested)
	}
	validate_resolved_destinations([first, Module{
		install_path: os.join_path(range_test_path, 'other')
	}])!
}

fn range_git(repo string, args []string) string {
	return cmd_ok_args(@LOCATION, ['git', '-C', repo, '-c', 'user.email=ci@vlang.io', '-c',
		'user.name=V CI', ...args]).output.trim_space()
}

fn range_add_tag(repo string, module_name string, tag string) !string {
	os.write_file(os.join_path(repo, 'v.mod'),
		"Module {\nname: '${module_name}'\nversion: '${tag.trim_string_left('v')}'\n}\n")!
	range_git(repo, ['add', 'v.mod'])
	range_git(repo, ['commit', '-m', tag])
	range_git(repo, ['tag', tag])
	return range_git(repo, ['rev-parse', 'HEAD'])
}

fn range_create_repo(name string) !string {
	repo := os.join_path(range_test_path, name + '_repo')
	os.mkdir_all(repo)!
	range_git(repo, ['init', '-b', 'main'])
	for tag in ['v1.2.0', 'v1.9.0', 'v1.10.0', 'v1.11.0-beta.1', 'v2.0.0'] {
		range_add_tag(repo, name, tag)!
	}
	return repo
}

fn range_write_project(project string, dependencies []string) ! {
	os.mkdir_all(project)!
	deps := dependencies.map("'${it.replace('\\', '/')}'").join(', ')
	os.write_file(os.join_path(project, 'v.mod'),
		"Module {\nname: 'range_project'\ndependencies: [${deps}]\n}\n")!
}

fn test_range_install_picks_a_tag_and_reports_unsatisfied_ranges() {
	repo := range_create_repo('range_install')!
	store := os.join_path(range_test_path, 'install_store')
	test_utils.set_test_env(store)
	dep := repo.replace('\\', '/')
	cmd_ok_args(@LOCATION, [range_vpm_exe, 'install', dep + '@>=1.2.0 <2.0.0'])
	installed := os.join_path(store, 'range_install')
	assert range_git(installed, ['rev-parse', 'HEAD']) == range_git(repo,
		['rev-parse', 'refs/tags/v1.10.0'])
	// Reusing a range selection does not require a force flag or a prompt.
	res := cmd_ok_args(@LOCATION, [range_vpm_exe, 'install', dep + '@>=1.2.0 <2.0.0'])
	assert res.output.contains('already installed'), res.output
	res_failed := cmd_fail_args(@LOCATION, [range_vpm_exe, 'install', dep + '@^3.0.0'])
	assert res_failed.output.contains('no semantic-version tag satisfies'), res_failed.output
	// A failed selection does not replace what is already installed.
	assert range_git(installed, ['rev-parse', 'HEAD']) == range_git(repo,
		['rev-parse', 'refs/tags/v1.10.0'])
}

fn test_range_lock_preserves_tag_and_commit_after_new_releases() {
	repo := range_create_repo('range_locked')!
	project := os.join_path(range_test_path, 'locked_project')
	dep := repo.replace('\\', '/') + '@^1.0.0'
	range_write_project(project, [dep])!
	original_dir := os.getwd()
	os.chdir(project)!
	defer {
		os.chdir(original_dir) or {}
	}
	store := os.join_path(range_test_path, 'locked_store')
	test_utils.set_test_env(store)
	cmd_ok_args(@LOCATION, [range_vpm_exe, 'install'])
	lf := read_lockfile(project)!
	entry := lf.modules[lockfile_module_key(dep)] or { panic('missing lock entry') }
	assert entry.requested == dep
	assert entry.resolved == 'v1.10.0'
	assert entry.revision == range_git(repo, ['rev-parse', 'refs/tags/v1.10.0'])
	range_add_tag(repo, 'range_locked', 'v1.20.0')!
	range_git(repo, ['tag', '-d', 'v1.10.0'])
	// The locked commit remains authoritative even if its tag was deleted.
	fresh_store := os.join_path(range_test_path, 'locked_fresh_store')
	test_utils.set_test_env(fresh_store)
	cmd_ok_args(@LOCATION, [range_vpm_exe, 'install', '--locked'])
	assert range_git(os.join_path(fresh_store, 'range_locked'), ['rev-parse', 'HEAD']) == entry.revision
	assert read_lockfile(project)!.modules[lockfile_module_key(dep)].resolved == entry.resolved
	// A changed range cannot silently reuse a lock with --locked.
	range_write_project(project, [repo.replace('\\', '/') + '@^2.0.0'])!
	failed := cmd_fail_args(@LOCATION, [range_vpm_exe, 'install', '--locked'])
	assert failed.output.contains('changed') || failed.output.contains('records'), failed.output
	cmd_ok_args(@LOCATION, [range_vpm_exe, 'install', '-f'])
	assert range_git(os.join_path(fresh_store, 'range_locked'), ['rev-parse', 'HEAD']) == range_git(repo,
		['rev-parse', 'refs/tags/v2.0.0'])
}

fn test_conflicting_range_requirements_fail_before_installing() {
	repo := range_create_repo('range_conflict')!
	project := os.join_path(range_test_path, 'conflict_project')
	dep := repo.replace('\\', '/')
	range_write_project(project, [dep + '@^1.0.0', dep + '@^2.0.0'])!
	original_dir := os.getwd()
	os.chdir(project)!
	defer {
		os.chdir(original_dir) or {}
	}
	store := os.join_path(range_test_path, 'conflict_store')
	test_utils.set_test_env(store)
	failed := cmd_fail_args(@LOCATION, [range_vpm_exe, 'install'])
	assert failed.output.contains('multiple requirements'), failed.output
	assert failed.output.contains('@^1.0.0'), failed.output
	assert failed.output.contains('@^2.0.0'), failed.output
	assert !os.exists(os.join_path(store, 'range_conflict'))
	assert !os.exists(lockfile_path(project))
}

fn test_range_lock_rejects_an_out_of_range_resolved_tag() {
	entry := LockedModule{
		requested: 'pkg@^1.0.0'
		resolved:  'v2.0.0'
		url:       'https://example.com/pkg'
	}
	assert lock_mismatch(entry, entry.requested, entry.url).contains('outside')
	assert lock_mismatch(LockedModule{
		...entry
		resolved: 'v1.2.0'
	}, entry.requested, entry.url) == ''
}

fn test_unsatisfied_range_leaves_project_install_untouched() {
	repo := range_create_repo('range_incomplete')!
	project := os.join_path(range_test_path, 'incomplete_project')
	dep := repo.replace('\\', '/')
	range_write_project(project, [dep + '@v1.2.0', dep + '@^3.0.0'])!
	original_dir := os.getwd()
	os.chdir(project)!
	defer {
		os.chdir(original_dir) or {}
	}
	store := os.join_path(range_test_path, 'incomplete_store')
	test_utils.set_test_env(store)
	failed := cmd_fail_args(@LOCATION, [range_vpm_exe, 'install'])
	assert failed.output.contains('no semantic-version tag satisfies'), failed.output
	assert !os.exists(os.join_path(store, 'range_incomplete'))
	assert !os.exists(lockfile_path(project))
}

fn test_malformed_version_tags_are_ignored() {
	malformed := ['v01.99.0', 'v1.099.0', 'v1.9.09', 'v1.99.0-', 'v1.99.0+', 'v1.99.0-alpha..1',
		'v1.99.0+build..1', 'v1.99.0+.', 'v999999999999999999999.0.0']
	for tag in malformed {
		if _ := version_tag(tag) {
			assert false, '${tag} should not be a version tag'
		}
	}
	assert select_version_tag([...malformed, 'v1.2.3'], '*')! == 'v1.2.3'
	assert select_version_tag(['v1.10.1-beta.01', 'v1.10.1-beta.1'],
		'^1.10.1-beta.0')! == 'v1.10.1-beta.1'
	assert version_tag('v1.2.3+build.01')!.metadata == 'build.01'
}

fn test_exact_refs_with_dot_x_suffixes_keep_the_requested_commit() {
	repo := range_create_repo('range_exact_refs')!
	range_add_tag(repo, 'range_exact_refs', '1.2.3')!
	range_add_tag(repo, 'range_exact_refs', 'v1.2.3-rc.x')!
	range_add_tag(repo, 'range_exact_refs', '1.2.3+build.x')!
	range_git(repo, ['branch', 'topic.x', 'refs/tags/v1.2.0'])
	for i, ref in ['v1.2.3-rc.x', '1.2.3+build.x', 'topic.x'] {
		store := os.join_path(range_test_path, 'exact_ref_store_${i}')
		test_utils.set_test_env(store)
		cmd_ok_args(@LOCATION, [range_vpm_exe, 'install', repo.replace('\\', '/') + '@' + ref])
		installed := os.join_path(store, 'range_exact_refs')
		assert range_git(installed, ['rev-parse', 'HEAD']) == range_git(repo, [
			'rev-parse',
			ref,
		])
	}
}
