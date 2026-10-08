module main

import os
import rand
import test_utils { cmd_fail_args, cmd_ok_args }
import v.vmod

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

fn joint_retracted_tag(repo string, tag string, ranges []string) ! {
	values := ranges.map("'${it}'").join(', ')
	os.write_file(os.join_path(repo, 'v.mod'), "Module { name: 'shared' version: '${tag.trim_string_left('v')}' retracted: [${values}] }\n")!
	joint_git(repo, ['add', 'v.mod'])
	joint_git(repo, ['commit', '-m', tag])
	joint_git(repo, ['tag', tag])
}

fn test_joint_retractions_filter_resolution_and_outdated() {
	repo := joint_repo('retracted', 'shared')!
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	joint_tag(repo, 'shared', 'v1.1.0', [])!
	joint_retracted_tag(repo, 'v1.2.0', ['>=1.1.0 <1.3.0'])!
	joint_project('retracted', [repo + '@^1'])!
	joint_cli(['install'])
	assert joint_head('retracted', 'shared') == old
	output := joint_cli(['outdated'])
	assert output.contains('v1.0.0\tv1.0.0\tv1.0.0\tv1.0.0'), output
}

fn test_joint_retractions_keep_locks_exact_pins_and_precise() {
	repo := joint_repo('retracted_lock', 'shared')!
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	retracted := joint_tag(repo, 'shared', 'v1.1.0', [])!
	project := joint_project('retracted_lock', [repo + '@^1'])!
	joint_cli(['install'])
	before := os.read_file(lockfile_path(project))!
	joint_retracted_tag(repo, 'v1.2.0', ['>=1.1.0 <1.3.0'])!
	joint_cli(['install', '--locked'])
	assert joint_head('retracted_lock', 'shared') == retracted
	assert os.read_file(lockfile_path(project))! == before
	joint_cli(['update'])
	assert joint_head('retracted_lock', 'shared') == old
	joint_cli(['update', '-p', 'shared', '--precise', 'v1.1.0'])
	assert joint_head('retracted_lock', 'shared') == retracted
	joint_project('retracted_pin', [repo + '@v1.1.0'])!
	joint_cli(['install'])
	assert joint_head('retracted_pin', 'shared') == retracted
}

fn test_joint_retractions_read_latest_release_outside_requested_range() {
	repo := joint_repo('retracted_major', 'shared')!
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	joint_tag(repo, 'shared', 'v1.1.0', [])!
	joint_retracted_tag(repo, 'v2.0.0', ['1.1.0'])!
	joint_project('retracted_major', [repo + '@^1'])!
	joint_cli(['install'])
	assert joint_head('retracted_major', 'shared') == old
}

fn test_joint_invalid_retractions_fail_before_publication() {
	repo := joint_repo('retracted_invalid', 'shared')!
	joint_tag(repo, 'shared', 'v1.0.0', [])!
	joint_retracted_tag(repo, 'v1.1.0', ['not a version range'])!
	project := joint_project('retracted_invalid', [repo + '@^1'])!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'install']).output
	assert failed.contains('invalid retracted version range'), failed
	assert !os.exists(lockfile_path(project))
	assert get_installed_modules_in(os.join_path(joint_root, 'retracted_invalid', 'store')).len == 0
}

fn test_joint_invalid_new_retractions_preserve_an_unchanged_lock() {
	repo := joint_repo('retracted_invalid_lock', 'shared')!
	head := joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('retracted_invalid_lock', [repo + '@^1'])!
	joint_cli(['install'])
	before := os.read_file(lockfile_path(project))!
	joint_retracted_tag(repo, 'v1.1.0', ['not a version range'])!
	joint_cli(['install'])
	assert joint_head('retracted_invalid_lock', 'shared') == head
	assert os.read_file(lockfile_path(project))! == before
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'update']).output
	assert failed.contains('invalid retracted version range'), failed
	assert joint_head('retracted_invalid_lock', 'shared') == head
	assert os.read_file(lockfile_path(project))! == before
}

fn test_joint_all_retracted_versions_report_the_requirement_chain() {
	repo := joint_repo('retracted_all', 'shared')!
	joint_retracted_tag(repo, 'v1.0.0', ['*'])!
	project := joint_project('retracted_all', [repo + '@^1'])!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'install']).output
	assert failed.contains('no semantic-version tag satisfies all requirements'), failed
	assert failed.contains(repo + '@^1'), failed
	assert !os.exists(lockfile_path(project))
	assert get_installed_modules_in(os.join_path(joint_root, 'retracted_all', 'store')).len == 0
}

fn test_joint_retractions_allow_backtracking_past_a_manifestless_release() {
	repo := joint_repo('retracted_manifestless', 'shared')!
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	manifest := os.read_file(os.join_path(repo, 'v.mod'))!
	joint_git(repo, ['rm', 'v.mod'])
	joint_git(repo, ['commit', '-m', 'release without a manifest'])
	joint_git(repo, ['tag', 'v1.1.0'])
	os.write_file(os.join_path(repo, 'v.mod'), manifest)!
	joint_git(repo, ['add', 'v.mod'])
	joint_git(repo, ['commit', '-m', 'restore default branch manifest'])
	joint_project('retracted_manifestless', [repo + '@^1'])!
	joint_cli(['install'])
	assert joint_head('retracted_manifestless', 'shared') == old
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
	joint_cli(['install', '--locked'])
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

fn test_joint_bare_requirements_share_exact_branch_pins_in_any_order() {
	repo := joint_repo('branch_pin', 'shared')!
	joint_tag(repo, 'shared', 'v1.0.0', [])!
	joint_git(repo, ['switch', '-c', 'feature'])
	os.write_file(os.join_path(repo, 'feature.txt'), 'feature branch')!
	joint_git(repo, ['add', 'feature.txt'])
	joint_git(repo, ['commit', '-m', 'feature branch'])
	feature := joint_git(repo, ['rev-parse', 'HEAD'])
	joint_git(repo, ['switch', 'main'])
	joint_project('branch_pin', [repo, repo + '@feature'])!
	joint_cli(['install'])
	assert joint_head('branch_pin', 'shared') == feature
	joint_project('branch_pin_reverse', [repo + '@feature', repo])!
	joint_cli(['install'])
	assert joint_head('branch_pin_reverse', 'shared') == feature
	parent := joint_repo('branch_pin', 'parent')!
	joint_tag(parent, 'parent', 'v1.0.0', [repo + '@feature'])!
	joint_project('branch_pin_transitive', [repo, parent + '@^1'])!
	joint_cli(['install'])
	assert joint_head('branch_pin_transitive', 'shared') == feature
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

fn test_joint_latest_widens_every_alias_of_a_direct_dependency() {
	repo := joint_repo('latest_alias', 'shared')!
	joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('latest_alias', [repo + '@^1', 'file://' + repo + '@^1'])!
	joint_cli(['install'])
	new := joint_tag(repo, 'shared', 'v2.0.0', [])!
	joint_cli(['update', '--latest'])
	assert joint_head('latest_alias', 'shared') == new
	manifest := vmod.from_file(os.join_path(project, 'v.mod'))!
	assert manifest.dependencies == [repo + '@^2.0.0', 'file://' + repo + '@^2.0.0']
	joint_cli(['install', '--locked'])
}

fn test_joint_install_prunes_lock_when_the_last_dependency_is_removed() {
	repo := joint_repo('empty_lock', 'shared')!
	joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('empty_lock', [repo + '@^1'])!
	joint_cli(['install'])
	before := os.read_file(lockfile_path(project))!
	joint_project('empty_lock', [])!
	joint_cli(['install', '--frozen'])
	assert os.read_file(lockfile_path(project))! == before
	joint_cli(['install'])
	assert read_lockfile(project)!.modules.len == 0
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

fn test_joint_locked_integrity_verification_preserves_existing_installations() {
	repo := joint_repo('integrity', 'shared')!
	head := joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('integrity', [repo + '@^1'])!
	joint_cli(['install'])
	mut lf := read_lockfile(project)!
	assert lf.modules[repo].hash.len == 64
	// Independent clones must reproduce the same package-content hash.
	test_utils.set_test_env(os.join_path(joint_root, 'integrity', 'fresh'))
	joint_cli(['install', '--locked'])
	installed := os.join_path(joint_root, 'integrity', 'fresh', 'shared')
	assert joint_git(installed, ['rev-parse', 'HEAD']) == head
	entry := lf.modules[repo]
	lf.modules[repo] = LockedModule{ ...entry, hash: '0'.repeat(64) }
	write_lockfile(project, lf)!
	before := os.read_file(lockfile_path(project))!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'install', '--locked']).output
	assert failed.contains('content hash mismatch'), failed
	assert joint_git(installed, ['rev-parse', 'HEAD']) == head
	assert os.read_file(lockfile_path(project))! == before
}

fn test_joint_dry_run_precise_and_latest_leave_checkout_manifest_and_lock_unchanged() {
	repo := joint_repo('dry_run', 'shared')!
	head := joint_tag(repo, 'shared', 'v1.0.0', [])!
	project := joint_project('dry_run', [repo + '@^1'])!
	joint_cli(['install'])
	joint_tag(repo, 'shared', 'v1.4.0', [])!
	joint_tag(repo, 'shared', 'v2.0.0', [])!
	manifest := os.read_file(os.join_path(project, 'v.mod'))!
	lock_before := os.read_file(lockfile_path(project))!
	for args in [['update', '--dry-run'],
		['update', '-p', 'shared', '--precise', '1.4.0', '--dry-run'],
		['update', '--latest', '--dry-run']] {
		output := joint_cli(args)
		assert output.contains('would select'), output
		assert joint_head('dry_run', 'shared') == head
		assert os.read_file(os.join_path(project, 'v.mod'))! == manifest
		assert os.read_file(lockfile_path(project))! == lock_before
	}
}

fn test_joint_vendor_copies_transitive_graph_from_project_root_and_preserves_destination() {
	leaf := joint_repo('vendor', 'leaf')!
	joint_tag(leaf, 'leaf', 'v1.0.0', [])!
	parent := joint_repo('vendor', 'parent')!
	joint_tag(parent, 'parent', 'v1.0.0', [leaf + '@^1'])!
	project := joint_project('vendor', [parent + '@^1'])!
	joint_cli(['install'])
	subdir := os.join_path(project, 'subdir')
	os.mkdir_all(subdir)!
	os.chdir(subdir)!
	joint_cli(['vendor'])
	vendor := os.join_path(project, 'vendor')
	assert os.is_file(os.join_path(vendor, 'leaf', 'v.mod'))
	assert os.is_file(os.join_path(vendor, 'parent', 'v.mod'))
	os.write_file(os.join_path(vendor, 'keep'), 'existing destination')!
	failed := cmd_fail_args(@LOCATION, [joint_tool, 'vendor']).output
	assert failed.contains('refusing to replace'), failed
	assert os.read_file(os.join_path(vendor, 'keep'))! == 'existing destination'
	os.rmdir_all(vendor)!
	os.rmdir_all(os.join_path(joint_root, 'vendor', 'store', 'leaf'))!
	missing := cmd_fail_args(@LOCATION, [joint_tool, 'vendor']).output
	assert missing.contains('not installed'), missing
	assert !os.exists(vendor)
}

fn test_joint_release_cutoff_filters_real_candidate_tags_and_preserves_locked_revision() {
	repo := joint_repo('cutoff', 'shared')!
	old_author := os.getenv('GIT_AUTHOR_DATE')
	old_committer := os.getenv('GIT_COMMITTER_DATE')
	defer {
		if old_author == '' {
			os.unsetenv('GIT_AUTHOR_DATE')
		} else {
			os.setenv('GIT_AUTHOR_DATE', old_author, true)
		}
		if old_committer == '' {
			os.unsetenv('GIT_COMMITTER_DATE')
		} else {
			os.setenv('GIT_COMMITTER_DATE', old_committer, true)
		}
	}
	os.setenv('GIT_AUTHOR_DATE', '2024-01-01T00:00:00Z', true)
	os.setenv('GIT_COMMITTER_DATE', '2024-01-01T00:00:00Z', true)
	old := joint_tag(repo, 'shared', 'v1.0.0', [])!
	os.setenv('GIT_AUTHOR_DATE', '2024-07-01T00:00:00Z', true)
	os.setenv('GIT_COMMITTER_DATE', '2024-07-01T00:00:00Z', true)
	new := joint_tag(repo, 'shared', 'v1.9.0', [])!
	project := joint_project('cutoff', [repo + '@^1'])!
	joint_cli(['install', '--exclude-newer', '2024-06-01'])
	assert joint_head('cutoff', 'shared') == old
	before := os.read_file(lockfile_path(project))!
	joint_cli(['install', '--locked', '--exclude-newer', '2023-01-01'])
	assert joint_head('cutoff', 'shared') == old
	assert os.read_file(lockfile_path(project))! == before
	joint_project('cutoff_exact', [repo + '@v1.9.0'])!
	joint_cli(['install', '--exclude-newer', '2024-06-01'])
	assert joint_head('cutoff_exact', 'shared') == new
	joint_project('cutoff_invalid', [repo + '@^1'])!
	bad := cmd_fail_args(@LOCATION, [joint_tool, 'install', '--minimum-release-age', 'bogus']).output
	assert bad.contains('minimum-release-age'), bad
	assert !os.exists(os.join_path(joint_root, 'cutoff_invalid', 'store', 'shared'))
}

fn test_joint_range_and_exact_branch_alias_can_share_a_tagged_commit() {
	repo := joint_repo('branch_range', 'shared')!
	head := joint_tag(repo, 'shared', 'v1.0.0', [])!
	joint_git(repo, ['branch', 'release'])
	joint_project('branch_range', [repo + '@^1', repo + '@release'])!
	joint_cli(['install'])
	assert joint_head('branch_range', 'shared') == head
}
