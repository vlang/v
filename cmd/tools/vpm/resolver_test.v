module main

import os
import rand
import test_utils { cmd_fail_args, cmd_ok_args }

const joint_root = os.join_path(os.temp_dir(), 'vpm_joint_${rand.ulid()}')
const joint_original_dir = os.getwd()
const joint_vexe = @VEXE
const joint_tool = os.join_path(joint_root, if os.user_os() == 'windows' {
	'vpm.exe'
} else {
	'vpm'
})

fn testsuite_begin() {
	os.mkdir_all(joint_root)!
	test_utils.set_test_env(os.join_path(joint_root, 'build'))
	os.setenv('VEXE', joint_vexe, true)
	os.unsetenv('CI')
	cmd_ok_args(@LOCATION, [joint_vexe, '-cc', @CCOMPILER, '-gc', 'none', '-o', joint_tool,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm')])
}

fn testsuite_end() {
	os.chdir(joint_original_dir)!
	os.rmdir_all(joint_root) or {}
}

fn joint_git(path string, args []string) string {
	return cmd_ok_args(@LOCATION, ['git', '-C', path, '-c', 'user.name=V CI', '-c',
		'user.email=ci@vlang.io', ...args]).output.trim_space()
}

fn joint_repo(case string, name string) !string {
	path := os.join_path(joint_root, case, name + '_repo')
	os.mkdir_all(path)!
	joint_git(path, ['init', '-b', 'main'])
	return path.replace(os.path_separator, '/')
}

fn joint_tag(repo string, name string, tag string, deps []string) !string {
	dependencies := deps.map("'${it}'").join(', ')
	os.write_file(os.join_path(repo, 'v.mod'), "Module { name: '${name}' version: '${tag.trim_string_left('v')}' dependencies: [${dependencies}] }\n")!
	joint_git(repo, ['add', 'v.mod'])
	joint_git(repo, ['commit', '-m', tag])
	joint_git(repo, ['tag', tag])
	return joint_git(repo, ['rev-parse', 'HEAD'])
}

fn joint_project(case string, deps []string) !string {
	project := os.join_path(joint_root, case, 'app')
	os.mkdir_all(project)!
	dependencies := deps.map("'${it}'").join(', ')
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'joint_app' dependencies: [${dependencies}] }\n")!
	test_utils.set_test_env(os.join_path(joint_root, case, 'store'))
	os.chdir(project)!
	return project
}

fn joint_head(case string, name string) string {
	return joint_git(os.join_path(joint_root, case, 'store', name), ['rev-parse', 'HEAD'])
}

fn joint_cli(args []string) string {
	return cmd_ok_args(@LOCATION, [joint_tool, ...args]).output.replace('\r\n', '\n')
}

fn test_joint_overlapping_constraints_choose_highest_common_tag_and_alias_once() {
	repo := joint_repo('overlap', 'shared')!
	joint_tag(repo, 'shared', 'v1.0.0', [])!
	middle := joint_tag(repo, 'shared', 'v1.4.0', [])!
	joint_tag(repo, 'shared', 'v1.9.0', [])!
	joint_project('overlap', [repo + '@^1', 'file://' + repo + '@>=1.0.0 <1.5.0'])!
	joint_cli(['install'])
	assert joint_head('overlap', 'shared') == middle
	assert get_installed_modules_in(os.join_path(joint_root, 'overlap', 'store')) == ['shared']
	// Root order does not determine the selected version.
	joint_project('overlap_reverse', ['file://' + repo + '@>=1.0.0 <1.5.0', repo + '@^1'])!
	joint_cli(['install'])
	assert joint_head('overlap_reverse', 'shared') == middle
}

fn test_joint_backtracks_parent_releases_and_discards_unreachable_dependencies() {
	shared := joint_repo('diamond', 'shared')!
	one := joint_tag(shared, 'shared', 'v1.0.0', [])!
	joint_tag(shared, 'shared', 'v2.0.0', [])!
	unused := joint_repo('diamond', 'unused')!
	joint_tag(unused, 'unused', 'v1.0.0', [])!
	a := joint_repo('diamond', 'a')!
	older := joint_tag(a, 'a', 'v1.0.0', [shared + '@^1'])!
	joint_tag(a, 'a', 'v1.1.0', [shared + '@^2', unused + '@^1'])!
	// The newest release cannot resolve at all, but that must not hide the older one.
	joint_tag(a, 'a', 'v1.2.0', [shared + '@^9'])!
	b := joint_repo('diamond', 'b')!
	joint_tag(b, 'b', 'v1.0.0', [shared + '@^1'])!
	project := joint_project('diamond', [a + '@^1', b + '@^1'])!
	joint_cli(['install'])
	assert joint_head('diamond', 'a') == older
	assert joint_head('diamond', 'shared') == one
	assert !os.exists(os.join_path(joint_root, 'diamond', 'store', 'unused'))
	lf := read_lockfile(project)!
	assert lf.modules.len == 3
	assert unused !in lf.modules
}

fn test_joint_conflict_reports_both_requirement_chains_without_installing() {
	shared := joint_repo('conflict', 'shared')!
	joint_tag(shared, 'shared', 'v1.0.0', [])!
	joint_tag(shared, 'shared', 'v2.0.0', [])!
	a := joint_repo('conflict', 'a')!
	joint_tag(a, 'a', 'v1.0.0', [shared + '@^1'])!
	b := joint_repo('conflict', 'b')!
	joint_tag(b, 'b', 'v1.0.0', [shared + '@^2'])!
	project := joint_project('conflict', [a + '@^1', b + '@^1'])!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'install']).output
	assert failed.contains('a@v1.0.0 -> ' + shared + '@^1'), failed
	assert failed.contains('b@v1.0.0 -> ' + shared + '@^2'), failed
	assert !os.exists(lockfile_path(project))
	assert get_installed_modules_in(os.join_path(joint_root, 'conflict', 'store')).len == 0
}

fn test_joint_cycles_terminate_and_exact_pins_constrain_ranges() {
	a := joint_repo('cycle', 'a')!
	b := joint_repo('cycle', 'b')!
	head := joint_tag(a, 'a', 'v1.0.0', [b + '@^1'])!
	joint_tag(a, 'a', 'v1.1.0', [b + '@^1'])!
	joint_tag(b, 'b', 'v1.0.0', [a + '@^1'])!
	joint_project('cycle', [a + '@v1.0.0', a + '@^1'])!
	joint_cli(['install'])
	assert joint_head('cycle', 'a') == head
	assert get_installed_modules_in(os.join_path(joint_root, 'cycle', 'store')).len == 2
}

fn test_joint_lock_preference_and_frozen_preserve_deleted_tag_revision() {
	repo := joint_repo('locked', 'shared')!
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('locked', [repo + '@^1', repo + '@>=1.0.0 <2'])!
	joint_cli(['install'])
	before := os.read_file(lockfile_path(project))!
	joint_tag(repo, 'shared', 'v1.9.0', [])!
	joint_git(repo, ['tag', '-d', 'v1.0.0'])
	joint_cli(['install', '--frozen'])
	assert joint_head('locked', 'shared') == old
	assert os.read_file(lockfile_path(project))! == before
	// This is a fresh store, so --locked must restore the revision, not just skip it.
	test_utils.set_test_env(os.join_path(joint_root, 'locked', 'fresh'))
	joint_cli(['install', '--locked'])
	assert joint_git(os.join_path(joint_root, 'locked', 'fresh', 'shared'), [
		'rev-parse',
		'HEAD',
	]) == old
}

fn test_joint_update_stays_in_range_and_precise_validates_constraints() {
	repo := joint_repo('update', 'shared')!
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('update', [repo + '@^1'])!
	joint_cli(['install'])
	middle := joint_tag(repo, 'shared', 'v1.4.0', [])!
	new := joint_tag(repo, 'shared', 'v1.9.0', [])!
	joint_tag(repo, 'shared', 'v2.0.0', [])!
	joint_cli(['update'])
	assert joint_head('update', 'shared') == new
	assert read_lockfile(project)!.modules[repo].resolved == 'v1.9.0'
	joint_cli(['update', '-p', 'shared', '--precise', '1.4.0'])
	assert joint_head('update', 'shared') == middle
	before := os.read_file(lockfile_path(project))!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'update', '-p', 'shared', '--precise', 'v2.0.0']).output
	assert failed.contains('@^1'), failed
	assert joint_head('update', 'shared') == middle
	assert os.read_file(lockfile_path(project))! == before
	joint_cli(['update', repo, '--precise', old])
	assert joint_head('update', 'shared') == old
}

fn test_joint_latest_widens_manifest_and_records_the_new_constraint() {
	repo := joint_repo('latest', 'shared')!
	joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('latest', [repo + '@^1'])!
	joint_cli(['install'])
	new := joint_tag(repo, 'shared', 'v2.0.0', [])!
	joint_cli(['update', '--latest'])
	assert joint_head('latest', 'shared') == new
	assert os.read_file(os.join_path(project, 'v.mod'))!.contains(repo + '@^2.0.0')
	assert read_lockfile(project)!.modules[repo].requested == repo + '@^2.0.0'
	joint_cli(['install', '--locked'])
}

fn test_joint_targeted_update_preserves_other_selections_and_local_work() {
	repo := joint_repo('targeted', 'shared')!
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	other := joint_repo('targeted', 'other')!
	other_old := joint_tag(other, 'other', 'v1.0.0', [])!
	project := joint_project('targeted', [repo + '@^1', other + '@^1'])!
	joint_cli(['install'])
	new := joint_tag(repo, 'shared', 'v1.4.0', [])!
	joint_tag(other, 'other', 'v1.9.0', [])!
	joint_cli(['update', '-p', 'shared'])
	assert joint_head('targeted', 'shared') == new
	assert joint_head('targeted', 'other') == other_old
	lf := read_lockfile(project)!
	assert lf.modules.len == 2
	assert lf.modules[other].revision == other_old
	assert lf.modules[repo].revision == new
	installed := os.join_path(joint_root, 'targeted', 'store', 'shared')
	os.write_file(os.join_path(installed, 'local-work.txt'), 'keep this work')!
	before := os.read_file(lockfile_path(project))!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'update', '-p', 'shared', '--precise', old]).output
	assert failed.contains('local git work that would be lost'), failed
	assert joint_head('targeted', 'shared') == new
	assert os.read_file(os.join_path(installed, 'local-work.txt'))! == 'keep this work'
	assert os.read_file(lockfile_path(project))! == before
	joint_git(installed, ['add', 'local-work.txt'])
	joint_git(installed, ['commit', '-m', 'unpublished local work'])
	unpublished := joint_head('targeted', 'shared')
	unpushed := cmd_fail_args(@LOCATION, [joint_tool, 'update', '-p', 'shared', '--precise', old]).output
	assert unpushed.contains('unpushed local commits detected'), unpushed
	assert joint_head('targeted', 'shared') == unpublished
	assert os.read_file(lockfile_path(project))! == before
	// The package selector also works when the manifest has only default-branch dependencies.
	bare_project := joint_project('targeted_bare', [repo, other])!
	joint_cli(['install'])
	bare_other := joint_head('targeted_bare', 'other')
	newest := joint_tag(repo, 'shared', 'v1.9.0', [])!
	joint_tag(other, 'other', 'v1.10.0', [])!
	joint_cli(['update', '-p', 'shared'])
	assert joint_head('targeted_bare', 'shared') == newest
	assert joint_head('targeted_bare', 'other') == bare_other
	assert read_lockfile(bare_project)!.modules[other].revision == bare_other
}

fn test_joint_why_and_graph_attribute_each_parent_constraint() {
	repo := joint_repo('graph', 'shared')!
	joint_tag(repo, 'shared', 'v1.4.0', [])!
	joint_project('graph', [repo + '@^1', repo + '@>=1.0.0 <2'])!
	joint_cli(['install'])
	why := joint_cli(['why', 'shared'])
	assert why.contains('shared (requires ^1 & >=1.0.0 <2, installed v1.4.0)'), why
	graph := joint_cli(['why', '--graph'])
	assert graph.trim_space() == 'joint_app -> shared@v1.4.0 (requires ^1 & >=1.0.0 <2)', graph
}

fn test_joint_outdated_distinguishes_release_constraints_from_full_resolution() {
	shared := joint_repo('outdated', 'shared')!
	joint_tag(shared, 'shared', 'v1.0.0', [])!
	a := joint_repo('outdated', 'a')!
	joint_tag(a, 'a', 'v1.0.0', [shared + '@^1'])!
	project := joint_project('outdated', [a + '@^1', shared + '@^1'])!
	joint_cli(['install'])
	joint_tag(shared, 'shared', 'v2.0.0', [])!
	joint_tag(a, 'a', 'v1.1.0', [shared + '@^2'])!
	joint_tag(a, 'a', 'v2.0.0', [shared + '@^2'])!
	joint_tag(a, 'a', 'v3.0.0-beta.1', [shared + '@^2'])!
	before := os.read_file(lockfile_path(project))!
	outdated := joint_cli(['outdated'])
	assert outdated.contains('Package\tCurrent\tUpgradable\tResolvable\tLatest'), outdated
	assert outdated.contains('a\tv1.0.0\tv1.1.0\tv1.0.0\tv2.0.0'), outdated
	assert outdated.contains('shared\tv1.0.0\tv1.0.0\tv1.0.0\tv2.0.0'), outdated
	assert os.read_file(lockfile_path(project))! == before
	assert joint_head('outdated', 'shared') == joint_git(shared, ['rev-parse', 'v1.0.0'])
}

fn test_joint_distinct_sources_cannot_overwrite_one_destination() {
	a := joint_repo('collision_a', 'same')!
	joint_tag(a, 'same', 'v1.0.0', [])!
	b := joint_repo('collision_b', 'same')!
	joint_tag(b, 'same', 'v1.0.0', [])!
	project := joint_project('collision', [a + '@^1', b + '@^1'])!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'install']).output
	assert failed.contains('different repositories target'), failed
	assert failed.contains(a), failed
	assert failed.contains(b), failed
	assert !os.exists(lockfile_path(project))
	assert get_installed_modules_in(os.join_path(joint_root, 'collision', 'store')).len == 0
}
