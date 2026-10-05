// vtest build: !musl? && !sanitized_job?
module main

import os
import rand
import test_utils { cmd_fail_args, cmd_ok_args }

// The tests in this file are fully offline: they build local git repositories
// under `test_path` and install from those, never touching the network.
const test_path = os.join_path(os.vtmp_dir(), 'vpm_lockfile_test_${rand.ulid()}')
const v_exe = os.getenv('VEXE')

fn testsuite_begin() {
	test_utils.set_test_env(test_path)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

// create_local_git_module creates a git repository with a v.mod for
// `module_name` at `repo_path`, commits it, and returns the sha of its HEAD.
fn create_local_git_module(repo_path string, module_name string) string {
	os.mkdir_all(repo_path) or { panic(err) }
	os.write_file(os.join_path(repo_path, 'v.mod'),
		"Module{\n\tname: '${module_name}'\n\tversion: '0.0.1'\n}\n") or { panic(err) }
	cmd_ok_args(@LOCATION, ['git', 'init', '-b', 'main', repo_path])
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'add', 'v.mod'])
	cmd_ok_args(@LOCATION,
		['git', '-C', repo_path, '-c', 'user.email=ci@vlang.io', '-c', 'user.name=V CI', 'commit',
			'-m', 'initial commit'])
	return git_head(repo_path)
}

// advance_local_git_module adds another commit to the repository at
// `repo_path` and returns the sha of its new HEAD.
fn advance_local_git_module(repo_path string) string {
	// Append, so that every call has a change to commit.
	feature_path := os.join_path(repo_path, 'feature.v')
	content := os.read_file(feature_path) or { 'module feature\n' }
	os.write_file(feature_path, content + '// advanced\n') or { panic(err) }
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'add', 'feature.v'])
	cmd_ok_args(@LOCATION,
		['git', '-C', repo_path, '-c', 'user.email=ci@vlang.io', '-c', 'user.name=V CI', 'commit',
			'-m', 'advance head'])
	return git_head(repo_path)
}

// git_head returns the sha of the current HEAD of the git repository at `repo_path`.
fn git_head(repo_path string) string {
	return cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'rev-parse', 'HEAD']).output.trim_space()
}

// write_project_vmod writes the v.mod of a test project that depends on `deps`.
fn write_project_vmod(project_dir string, deps []string) {
	deps_list := deps.map("'${it}'").join(', ')
	os.write_file(os.join_path(project_dir, 'v.mod'),
		"Module{\n\tname: 'lockfile_project'\n\tversion: '0.0.1'\n\tdependencies: [${deps_list}]\n}\n") or {
		panic(err)
	}
}

// Case: `v install` in a project directory records the resolved revisions of
// its dependencies in `v.mod.lock` next to its `v.mod`.
fn test_install_records_resolved_revisions_in_the_lockfile() {
	repo_path := os.join_path(test_path, 'recorded_dep_repo')
	head := create_local_git_module(repo_path, 'recorded_pkg')
	project_dir := os.join_path(test_path, 'recorded_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_recorded'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	res := cmd_ok_args(@LOCATION, [v_exe, 'install'])
	assert res.output.contains('Installed `recorded_pkg`'), res.output

	lf := read_lockfile(project_dir) or { panic(err) }
	assert lf.version == lockfile_version
	entry := lf.modules[dep] or { panic('no lock entry for `${dep}` in ${lf.modules.keys()}') }
	assert entry.requested == dep
	assert entry.url == dep
	assert entry.revision == head
	assert entry.resolved == pseudo_version(head_commit_unix_ts(repo_path), head)
	// The installed checkout sits at the recorded revision.
	installed_head := git_head(os.join_path(test_path, 'vmodules_recorded', 'recorded_pkg'))
	assert installed_head == head
}

// Case: installing an additional dependency keeps the entries of the modules
// that the run did not touch.
fn test_install_merges_with_existing_lockfile_entries() {
	first_repo_path := os.join_path(test_path, 'merge_first_repo')
	first_head := create_local_git_module(first_repo_path, 'merge_one')
	second_repo_path := os.join_path(test_path, 'merge_second_repo')
	second_head := create_local_git_module(second_repo_path, 'merge_two')
	project_dir := os.join_path(test_path, 'merge_project')
	os.mkdir_all(project_dir) or { panic(err) }
	first_dep := first_repo_path.replace('\\', '/')
	second_dep := second_repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [first_dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_merge_first'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	write_project_vmod(project_dir, [first_dep, second_dep])
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_merge_second'))
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	lf := read_lockfile(project_dir) or { panic(err) }
	assert lf.modules.len == 2
	first := lf.modules[first_dep] or { panic('no lock entry for `${first_dep}`') }
	assert first.revision == first_head
	second := lf.modules[second_dep] or { panic('no lock entry for `${second_dep}`') }
	assert second.revision == second_head
}

// Case: once a dependency is locked, a later install clones the recorded
// revision, not the advanced HEAD of the repository it came from.
fn test_locked_install_reuses_the_recorded_revision() {
	repo_path := os.join_path(test_path, 'locked_dep_repo')
	head := create_local_git_module(repo_path, 'locked_pkg')
	project_dir := os.join_path(test_path, 'locked_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_locked_first'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	new_head := advance_local_git_module(repo_path)
	assert new_head != head

	// The second install runs against a fresh module store, so the module has
	// to be cloned again: from the locked revision, not from the new HEAD.
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_locked_second'))
	res := cmd_ok_args(@LOCATION, [v_exe, 'install'])
	assert res.output.contains('Installing `locked_pkg`'), res.output
	installed_head := git_head(os.join_path(test_path, 'vmodules_locked_second', 'locked_pkg'))
	assert installed_head == head
	assert installed_head != new_head
}

// Case: with a warm module store, a later `v install` keeps the installed
// checkout and the lock entry at the recorded revision, instead of updating
// the module past the lock.
fn test_warm_store_install_honors_the_locked_revision() {
	repo_path := os.join_path(test_path, 'warm_dep_repo')
	head := create_local_git_module(repo_path, 'warm_pkg')
	project_dir := os.join_path(test_path, 'warm_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_warm'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	new_head := advance_local_git_module(repo_path)
	assert new_head != head

	// The second install runs against the SAME module store: the module is
	// already installed, and the lock must hold it at the recorded revision
	// instead of letting it drift to the new HEAD.
	res := cmd_ok_args(@LOCATION, [v_exe, 'install'])
	assert !res.output.contains('Updating module'), res.output
	installed_head := git_head(os.join_path(test_path, 'vmodules_warm', 'warm_pkg'))
	assert installed_head == head
	lf := read_lockfile(project_dir) or { panic(err) }
	entry := lf.modules[dep] or { panic('no lock entry for `${dep}` in ${lf.modules.keys()}') }
	assert entry.revision == head
}

// Case: an installed checkout that drifted from the locked revision is put
// back on the lock by a later `v install`.
fn test_install_restores_a_drifted_checkout_to_the_locked_revision() {
	repo_path := os.join_path(test_path, 'drift_dep_repo')
	head := create_local_git_module(repo_path, 'drift_pkg')
	project_dir := os.join_path(test_path, 'drift_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_drift'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	// Move the installed checkout past the locked revision, the way a plain
	// `git pull` in the module store would.
	installed_path := os.join_path(test_path, 'vmodules_drift', 'drift_pkg')
	new_head := advance_local_git_module(repo_path)
	assert new_head != head
	cmd_ok_args(@LOCATION, ['git', '-C', installed_path, 'pull', '--quiet'])
	assert git_head(installed_path) == new_head

	res := cmd_ok_args(@LOCATION, [v_exe, 'install'])
	assert res.output.contains('Restoring `drift_pkg` to the locked revision'), res.output
	assert git_head(installed_path) == head
	lf := read_lockfile(project_dir) or { panic(err) }
	entry := lf.modules[dep] or { panic('no lock entry for `${dep}` in ${lf.modules.keys()}') }
	assert entry.revision == head
}

// Case: `--locked` is an error for a plain global install, which has no
// project lockfile to check against.
fn test_locked_flag_fails_for_a_global_install() {
	repo_path := os.join_path(test_path, 'locked_global_repo')
	create_local_git_module(repo_path, 'locked_global_pkg')
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_locked_global'))
	run_dir := os.join_path(test_path, 'locked_global_run_dir')
	os.mkdir_all(run_dir) or { panic(err) }
	old_dir := os.getwd()
	os.chdir(run_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	res := cmd_fail_args(@LOCATION, [v_exe, 'install', '--locked', repo_path])
	assert res.output.contains('--locked'), res.output
}

// Case: a locked revision that the clone source no longer holds fails the
// install, instead of silently installing whatever HEAD a fresh clone sits on.
fn test_locked_install_fails_when_the_recorded_revision_is_unreachable() {
	// Note: the directory names in here are kept deliberately short. Under
	// `v test`, VTMP gains a `tsession_*` component, and a clone's deepest
	// `.git/objects/pack/pack-*.idx` path otherwise overflows MAX_PATH on
	// Windows (`Filename too long` mid-clone).
	repo_path := os.join_path(test_path, 'u_repo')
	head := create_local_git_module(repo_path, 'upkg')
	project_dir := os.join_path(test_path, 'u_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vu1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	// Replace the history of the source repository, and purge the objects of
	// the old one: clones of a local repository share its whole object store,
	// so the recorded revision has to be garbage-collected before a fresh
	// clone can no longer provide it.
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'checkout', '--orphan', 'freshroot'])
	os.write_file(os.join_path(repo_path, 'fresh.v'), 'module fresh\n') or { panic(err) }
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'add', '-A'])
	cmd_ok_args(@LOCATION,
		['git', '-C', repo_path, '-c', 'user.email=ci@vlang.io', '-c', 'user.name=V CI', 'commit',
			'-m', 'fresh history'])
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'branch', '-D', 'main'])
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'branch', '-M', 'main'])
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'reflog', 'expire', '--expire=now', '--all'])
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'gc', '--prune=now', '--quiet'])
	new_head := git_head(repo_path)
	assert new_head != head

	// The second install runs against a fresh module store, so the module has
	// to be cloned again: the recorded revision is gone from the source, and
	// the install must fail, even without `--locked`. Each run gets its own
	// store/VTMP, so the leftover tmp clone of a failed run cannot collide
	// with the next one (Windows cannot remove the read-only git objects).
	test_utils.set_test_env(os.join_path(test_path, 'vu2'))
	res := cmd_fail_args(@LOCATION, [v_exe, 'install'])
	assert res.output.contains('failed to install'), res.output
	test_utils.set_test_env(os.join_path(test_path, 'vu3'))
	res_verbose := cmd_fail_args(@LOCATION, [v_exe, 'install', '-v'])
	assert res_verbose.output.contains('failed to checkout'), res_verbose.output
	test_utils.set_test_env(os.join_path(test_path, 'vu4'))
	res_locked := cmd_fail_args(@LOCATION, [v_exe, 'install', '--locked'])
	assert res_locked.output.contains('failed to install'), res_locked.output
}

// Case: `--locked` refuses to install a dependency whose string no longer
// matches the one recorded in the lockfile.
fn test_locked_install_fails_when_the_dependency_changed() {
	repo_path := os.join_path(test_path, 'changed_dep_repo')
	create_local_git_module(repo_path, 'changed_pkg')
	project_dir := os.join_path(test_path, 'changed_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_changed'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	// The project now asks for a tag, while the lockfile records the plain path.
	write_project_vmod(project_dir, ['${dep}@v1.0.0'])
	res := cmd_fail_args(@LOCATION, [v_exe, 'install', '--locked'])
	assert res.output.contains('--locked'), res.output
	assert res.output.contains('records `${dep}` for it'), res.output
}

// Case: `--locked` fails when the project has no lockfile to check against.
fn test_locked_install_fails_without_a_lockfile() {
	repo_path := os.join_path(test_path, 'fresh_dep_repo')
	create_local_git_module(repo_path, 'fresh_pkg')
	project_dir := os.join_path(test_path, 'fresh_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_fresh'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	res := cmd_fail_args(@LOCATION, [v_exe, 'install', '--locked'])
	assert res.output.contains('`--locked` requires a lockfile'), res.output
}

// Case: `--local` installs into the project's own root, so the lockfile is
// recorded there too.
fn test_local_install_locks_at_the_project_root() {
	repo_path := os.join_path(test_path, 'local_dep_repo')
	head := create_local_git_module(repo_path, 'local_locked_pkg')
	project_dir := os.join_path(test_path, 'local_locked_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_locked'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	res := cmd_ok_args(@LOCATION, [v_exe, 'install', '--local', repo_path])
	assert res.output.contains('Installed `local_locked_pkg`'), res.output
	assert os.is_file(os.join_path(project_dir, 'local_locked_pkg', 'v.mod'))

	// The entry is recorded under the dependency string as given on the command
	// line, which on windows is the path with `\` separators.
	lf := read_lockfile(project_dir) or { panic(err) }
	entry := lf.modules[repo_path] or { panic('no lock entry for `${repo_path}` in ${lf.modules.keys()}') }
	assert entry.revision == head
}

// Case: installing a module outside of a project dependency resolution records
// no lockfile at all.
fn test_global_install_does_not_create_a_lockfile() {
	repo_path := os.join_path(test_path, 'unlocked_dep_repo')
	create_local_git_module(repo_path, 'unlocked_pkg')
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_unlocked'))

	// Run from a dedicated directory without a v.mod, independent of whatever
	// directory an earlier test left behind: a plain install is not a project
	// dependency resolution, so no lockfile is recorded anywhere.
	run_dir := os.join_path(test_path, 'unlocked_run_dir')
	os.mkdir_all(run_dir) or { panic(err) }
	os.chdir(run_dir) or { panic(err) }
	res := cmd_ok_args(@LOCATION, [v_exe, 'install', repo_path])
	assert res.output.contains('Installed `unlocked_pkg`'), res.output
	assert !os.exists(os.join_path(run_dir, lockfile_name)), 'no lockfile is recorded for a plain install'
	assert !os.exists(os.join_path(test_path, lockfile_name))
}

// Case: `v update` moves the lock entry of the updated module to its new HEAD.
fn test_update_refreshes_the_lock_entry() {
	repo_path := os.join_path(test_path, 'updated_dep_repo')
	head := create_local_git_module(repo_path, 'updated_pkg')
	project_dir := os.join_path(test_path, 'updated_project')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_updated'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	new_head := advance_local_git_module(repo_path)
	assert new_head != head
	cmd_ok_args(@LOCATION, [v_exe, 'update', 'updated_pkg'])

	lf := read_lockfile(project_dir) or { panic(err) }
	entry := lf.modules[dep] or { panic('no lock entry for `${dep}` in ${lf.modules.keys()}') }
	assert entry.revision == new_head
	assert entry.resolved == pseudo_version(head_commit_unix_ts(repo_path), new_head)
}

// Case: `v remove` drops the lock entry of the removed module, and only that
// one. The lockfile bookkeeping is exercised directly: on Windows hosts,
// removing a git checkout is blocked by the read-only attribute of git object
// files, which `os.rmdir_all` cannot unlink — a limitation of `v remove` that
// predates the lockfile, and that is not what is under test here.
fn test_remove_drops_the_lock_entry() {
	removed_repo_path := os.join_path(test_path, 'removed_dep_repo')
	create_local_git_module(removed_repo_path, 'removed_pkg')
	kept_repo_path := os.join_path(test_path, 'kept_dep_repo')
	kept_head := create_local_git_module(kept_repo_path, 'kept_pkg')
	project_dir := os.join_path(test_path, 'removed_project')
	os.mkdir_all(project_dir) or { panic(err) }
	removed_dep := removed_repo_path.replace('\\', '/')
	kept_dep := kept_repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [removed_dep, kept_dep])

	test_utils.set_test_env(os.join_path(test_path, 'vmodules_removed'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	lf := read_lockfile(project_dir) or { panic(err) }
	assert lf.modules.len == 2

	// What `v remove` does after a successful removal: match the entries of the
	// removed module, by the name it was removed under and by its clone source.
	removed_path := os.join_path(test_path, 'vmodules_removed', 'removed_pkg')
	origin := checkout_origin_url(removed_path)
	remove_lock_entries('removed_pkg', origin)

	mut lf_after := read_lockfile(project_dir) or { panic(err) }
	assert lf_after.modules.len == 1
	assert removed_dep !in lf_after.modules
	kept := lf_after.modules[kept_dep] or { panic('no lock entry for `${kept_dep}`') }
	assert kept.revision == kept_head
}

fn test_pseudo_version_format() {
	// 2006-01-02 15:04:05 UTC, the reference time of the unix timestamp.
	assert pseudo_version(1136214245, '0123456789abcdef0123456789abcdef01234567') == 'v0.0.0-20060102150405-0123456789ab'
}

fn test_lockfile_module_key_strips_the_version_suffix() {
	assert lockfile_module_key('vsl') == 'vsl'
	assert lockfile_module_key('vsl@1.0.0') == 'vsl'
	assert lockfile_module_key('https://github.com/vlang/vsl') == 'https://github.com/vlang/vsl'
	assert lockfile_module_key('https://github.com/vlang/vsl@v0.1.50') == 'https://github.com/vlang/vsl'
	assert lockfile_module_key('git@github.com:vlang/vsl') == 'git@github.com:vlang/vsl'
	assert lockfile_module_key('git@github.com:vlang/vsl@v0.1.50') == 'git@github.com:vlang/vsl'
}

fn test_lockfile_write_read_roundtrip() {
	dir := os.join_path(test_path, 'lockfile_roundtrip')
	os.mkdir_all(dir) or { panic(err) }
	mut lf := LockFile{
		version: lockfile_version
		modules: map[string]LockedModule{}
	}
	lf.upsert(LockedModule{
		requested: 'vsl@1.0.0'
		resolved:  '1.0.0'
		revision:  'a'
		url:       'https://github.com/vlang/vsl'
	}, 'vsl')
	lf.upsert(LockedModule{
		requested: 'https://github.com/vlang/markdown'
		resolved:  'v0.0.0-20240102150405-0123456789ab'
		revision:  'b'
		url:       'https://github.com/vlang/markdown'
	}, 'https://github.com/vlang/markdown')
	write_lockfile(dir, lf) or { panic(err) }

	read := read_lockfile(dir) or { panic(err) }
	assert read.version == lockfile_version
	assert read.modules.len == 2
	vsl := read.modules['vsl'] or { panic('no entry for `vsl`') }
	assert vsl.requested == 'vsl@1.0.0'
	assert vsl.resolved == '1.0.0'
	assert vsl.revision == 'a'
	assert vsl.url == 'https://github.com/vlang/vsl'
	markdown := read.modules['https://github.com/vlang/markdown'] or { panic('no entry for markdown') }
	assert markdown.revision == 'b'

	// A file that is not a lockfile is an error, and so is a missing one.
	os.write_file(lockfile_path(dir), 'not json at all') or { panic(err) }
	if broken := read_lockfile(dir) {
		dump(broken)
		assert false, 'reading an invalid lockfile has to fail'
	}
	if missing := read_lockfile(os.join_path(test_path, 'lockfile_missing_dir')) {
		dump(missing)
		assert false, 'reading a missing lockfile has to fail'
	}
	// A lockfile from a newer format version is rejected too.
	os.write_file(lockfile_path(dir), '{"version": ${lockfile_version + 1}, "modules": {}}') or {
		panic(err)
	}
	if newer := read_lockfile(dir) {
		dump(newer)
		assert false, 'a newer lockfile version has to be rejected'
	}
	// So is one without a valid format version.
	os.write_file(lockfile_path(dir), '{}') or { panic(err) }
	if unversioned := read_lockfile(dir) {
		dump(unversioned)
		assert false, 'a lockfile without a format version has to be rejected'
	}
}

// The modules of a lockfile are written sorted by name, whatever the order
// they were resolved in.
fn test_lockfile_is_written_sorted() {
	dir := os.join_path(test_path, 'lockfile_sorted')
	os.mkdir_all(dir) or { panic(err) }
	mut lf := LockFile{
		version: lockfile_version
		modules: map[string]LockedModule{}
	}
	for name in ['zeta', 'alpha', 'mid'] {
		lf.upsert(LockedModule{
			requested: name
			revision:  name
		}, name)
	}
	write_lockfile(dir, lf) or { panic(err) }
	content := os.read_file(lockfile_path(dir)) or { panic(err) }
	assert content.index('"alpha"') or { -1 } < content.index('"mid"') or { -1 }
	assert content.index('"mid"') or { -1 } < content.index('"zeta"') or { -1 }
}

fn test_lockfile_upsert_and_remove() {
	mut lf := LockFile{
		version: lockfile_version
		modules: map[string]LockedModule{}
	}
	lf.upsert(LockedModule{
		requested: 'a@1.0.0'
		resolved:  '1.0.0'
		revision:  'a'
		url:       'https://example.com/a'
	}, 'a')
	lf.upsert(LockedModule{
		requested: 'b'
		resolved:  'v0.0.0-20240102150405-0123456789ab'
		revision:  'b'
		url:       'https://example.com/b'
	}, 'b')
	assert lf.modules.len == 2
	// Upserting replaces the entry of the same module.
	lf.upsert(LockedModule{
		requested: 'a@2.0.0'
		resolved:  '2.0.0'
		revision:  'a2'
		url:       'https://example.com/a'
	}, 'a')
	assert lf.modules.len == 2
	assert lf.modules['a'].revision == 'a2'
	// Removing drops exactly one entry, and an unknown module is a no-op.
	lf.remove('a')
	assert lf.modules.len == 1
	assert 'a' !in lf.modules
	lf.remove('missing')
	assert lf.modules.len == 1
	assert 'b' in lf.modules
}

fn test_head_revision_reads_the_fixture_head() {
	repo_path := os.join_path(test_path, 'head_revision_repo')
	head := create_local_git_module(repo_path, 'head_pkg')
	assert head.len == 40
	assert head_revision(repo_path) == head
	assert head_commit_unix_ts(repo_path) > 0
	// A directory that is no git checkout reports no revision at all.
	assert head_revision(os.join_path(test_path, 'head_revision_missing')) == ''
	assert head_commit_unix_ts(os.join_path(test_path, 'head_revision_missing')) == 0
}

// Case: `v update` also works on a checkout that a locked install left
// detached: it fetches and moves HEAD to the default branch of the origin,
// instead of failing the way `git pull` does outside of a branch.
fn test_update_works_on_a_locked_clone() {
	repo_path := os.join_path(test_path, 'det_repo')
	head := create_local_git_module(repo_path, 'det_pkg')
	project_dir := os.join_path(test_path, 'det_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vd1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	// Install once more against a fresh module store: the locked install
	// clones and checks out the recorded revision, leaving HEAD detached.
	test_utils.set_test_env(os.join_path(test_path, 'vd2'))
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
	installed_path := os.join_path(test_path, 'vd2', 'det_pkg')
	assert git_head(installed_path) == head

	new_head := advance_local_git_module(repo_path)
	assert new_head != head
	res := cmd_ok_args(@LOCATION, [v_exe, 'update', 'det_pkg'])
	assert res.output.contains('Updated module `det_pkg`.'), res.output
	assert git_head(installed_path) == new_head
	lf := read_lockfile(project_dir) or { panic(err) }
	entry := lf.modules[dep] or { panic('no lock entry for `${dep}` in ${lf.modules.keys()}') }
	assert entry.revision == new_head
}

// Case: when the dependency string of an installed module no longer matches
// the one the lockfile records, `v install` resolves it anew instead of
// pinning the checkout to the stale entry, and records the new resolution.
fn test_install_resolves_anew_when_the_dependency_string_changed() {
	repo_path := os.join_path(test_path, 'anew_repo')
	tagged_head := create_local_git_module(repo_path, 'anew_pkg')
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'tag', 'v1.0.0'])
	project_dir := os.join_path(test_path, 'anew_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, ['${dep}@v1.0.0'])

	test_utils.set_test_env(os.join_path(test_path, 'va1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
	lf := read_lockfile(project_dir) or { panic(err) }
	tagged_entry := lf.modules[dep] or { panic('no lock entry for `${dep}` in ${lf.modules.keys()}') }
	assert tagged_entry.requested == '${dep}@v1.0.0'
	assert tagged_entry.revision == tagged_head

	// Another module store holds a plain install of the repository, made
	// outside of the project while its HEAD was still the tagged revision.
	// `installed_version` is '' for such a checkout, which is what lets a
	// later bare `v install` reach the update path.
	test_utils.set_test_env(os.join_path(test_path, 'va2'))
	run_dir := os.join_path(test_path, 'anew_run_dir')
	os.mkdir_all(run_dir) or { panic(err) }
	os.chdir(run_dir) or { panic(err) }
	cmd_ok_args(@LOCATION, [v_exe, 'install', repo_path])
	installed_path := os.join_path(test_path, 'va2', 'anew_pkg')
	assert git_head(installed_path) == tagged_head
	new_head := advance_local_git_module(repo_path)
	assert new_head != tagged_head

	// The project now asks for the bare repository, without the tag: the
	// stale lock entry must not pin the checkout, the module is updated to
	// the HEAD of the origin, and the lockfile records the new resolution.
	os.chdir(project_dir) or { panic(err) }
	write_project_vmod(project_dir, [dep])
	res := cmd_ok_args(@LOCATION, [v_exe, 'install', '-v'])
	assert res.output.contains('resolving it anew'), res.output
	assert !res.output.contains('Restoring `anew_pkg`'), res.output
	assert git_head(installed_path) == new_head
	lf_after := read_lockfile(project_dir) or { panic(err) }
	entry := lf_after.modules[dep] or {
		panic('no lock entry for `${dep}` in ${lf_after.modules.keys()}')
	}
	assert entry.requested == dep
	assert entry.revision == new_head
}

// Case: a lockfile from a teammate can record a revision that is newer than
// the checkout in the module store. Restoring the checkout to it fetches the
// revision from the origin first.
fn test_install_restores_a_checkout_older_than_the_locked_revision() {
	repo_path := os.join_path(test_path, 'nw_repo')
	head := create_local_git_module(repo_path, 'nw_pkg')
	project_dir := os.join_path(test_path, 'nw_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vnw'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
	installed_path := os.join_path(test_path, 'vnw', 'nw_pkg')
	assert git_head(installed_path) == head

	// The teammate locked a later revision, which the installed clone has never
	// seen: it was made before that commit existed.
	new_head := advance_local_git_module(repo_path)
	mut lf := read_lockfile(project_dir) or { panic(err) }
	lf.upsert(LockedModule{
		requested: dep
		resolved:  pseudo_version(head_commit_unix_ts(repo_path), new_head)
		revision:  new_head
		url:       dep
	}, dep)
	write_lockfile(project_dir, lf) or { panic(err) }
	assert !git_has_commit(installed_path, new_head)

	res := cmd_ok_args(@LOCATION, [v_exe, 'install'])
	assert res.output.contains('Restoring `nw_pkg` to the locked revision'), res.output
	assert git_head(installed_path) == new_head
}

// Case: a checkout that a locked install left detached has no upstream branch,
// yet `v outdated` and `v upgrade` still compare it with the default branch of
// its origin, instead of reporting it as up to date forever.
fn test_outdated_and_upgrade_see_a_locked_checkout() {
	repo_path := os.join_path(test_path, 'od_repo')
	head := create_local_git_module(repo_path, 'od_pkg')
	project_dir := os.join_path(test_path, 'od_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vod1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
	test_utils.set_test_env(os.join_path(test_path, 'vod2'))
	cmd_ok_args(@LOCATION, [v_exe, 'install', '--locked'])
	installed_path := os.join_path(test_path, 'vod2', 'od_pkg')
	assert git_head(installed_path) == head
	assert head_is_detached(installed_path)
	up_to_date := cmd_ok_args(@LOCATION, [v_exe, 'outdated'])
	assert up_to_date.output.contains('Modules are up to date.'), up_to_date.output

	new_head := advance_local_git_module(repo_path)
	outdated := cmd_ok_args(@LOCATION, [v_exe, 'outdated'])
	assert outdated.output.contains('od_pkg'), outdated.output
	cmd_ok_args(@LOCATION, [v_exe, 'upgrade'])
	assert git_head(installed_path) == new_head
	lf := read_lockfile(project_dir) or { panic(err) }
	entry := lf.modules[dep] or { panic('no lock entry for `${dep}` in ${lf.modules.keys()}') }
	assert entry.revision == new_head
	after := cmd_ok_args(@LOCATION, [v_exe, 'outdated'])
	assert after.output.contains('Modules are up to date.'), after.output
}

// Case: when one dependency of a project cannot be installed at its locked
// revision, the others are installed, but the run fails as a whole.
fn test_install_fails_when_one_of_several_locked_dependencies_fails() {
	good_repo_path := os.join_path(test_path, 'pf_good')
	good_head := create_local_git_module(good_repo_path, 'pf_good_pkg')
	bad_repo_path := os.join_path(test_path, 'pf_bad')
	create_local_git_module(bad_repo_path, 'pf_bad_pkg')
	project_dir := os.join_path(test_path, 'pf_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	good_dep := good_repo_path.replace('\\', '/')
	bad_dep := bad_repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [good_dep, bad_dep])

	test_utils.set_test_env(os.join_path(test_path, 'vpf1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
	// Point the lock entry of one dependency at a revision that does not exist.
	mut lf := read_lockfile(project_dir) or { panic(err) }
	bad_entry := lf.modules[bad_dep] or { panic('no lock entry for `${bad_dep}`') }
	lf.upsert(LockedModule{
		...bad_entry
		revision: '0123456789abcdef0123456789abcdef01234567'
	}, bad_dep)
	write_lockfile(project_dir, lf) or { panic(err) }
	lock_before := os.read_file(lockfile_path(project_dir)) or { panic(err) }

	test_utils.set_test_env(os.join_path(test_path, 'vpf2'))
	res := cmd_fail_args(@LOCATION, [v_exe, 'install', '--locked'])
	assert res.output.contains('failed to install'), res.output
	assert git_head(os.join_path(test_path, 'vpf2', 'pf_good_pkg')) == good_head
	assert !os.exists(os.join_path(test_path, 'vpf2', 'pf_bad_pkg'))
	test_utils.set_test_env(os.join_path(test_path, 'vpf3'))
	cmd_fail_args(@LOCATION, [v_exe, 'install'])
	assert os.read_file(lockfile_path(project_dir)) or { panic(err) } == lock_before
}

// Case: a dependency locked at a tag stays at the tagged revision: a locked
// install clones it at that tag, `v update` and `v outdated` leave it there,
// and `v update` of another checkout of the same source does not move its
// lock entry either.
fn test_a_tag_pinned_dependency_stays_at_its_tag() {
	repo_path := os.join_path(test_path, 'tp_repo')
	tagged_head := create_local_git_module(repo_path, 'tp_pkg')
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'tag', 'v1.0.0'])
	advance_local_git_module(repo_path)
	project_dir := os.join_path(test_path, 'tp_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, ['${dep}@v1.0.0'])

	test_utils.set_test_env(os.join_path(test_path, 'vtp1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
	lock_before := os.read_file(lockfile_path(project_dir)) or { panic(err) }

	test_utils.set_test_env(os.join_path(test_path, 'vtp2'))
	cmd_ok_args(@LOCATION, [v_exe, 'install', '--locked'])
	installed_path := os.join_path(test_path, 'vtp2', 'tp_pkg')
	assert git_head(installed_path) == tagged_head
	outdated := cmd_ok_args(@LOCATION, [v_exe, 'outdated'])
	assert outdated.output.contains('Modules are up to date.'), outdated.output
	// Like for a tag install without a lockfile, there is no branch to update to.
	os.exec([v_exe, 'update', 'tp_pkg'])
	assert git_head(installed_path) == tagged_head
	assert os.read_file(lockfile_path(project_dir)) or { panic(err) } == lock_before

	// A plain install of the same repository, which does follow its default
	// branch, is updated, but the lock entry of the project stays at the tag.
	test_utils.set_test_env(os.join_path(test_path, 'vtp3'))
	run_dir := os.join_path(test_path, 'tp_run_dir')
	os.mkdir_all(run_dir) or { panic(err) }
	os.chdir(run_dir) or { panic(err) }
	cmd_ok_args(@LOCATION, [v_exe, 'install', repo_path])
	advance_local_git_module(repo_path)
	os.chdir(project_dir) or { panic(err) }
	res := cmd_ok_args(@LOCATION, [v_exe, 'update', 'tp_pkg'])
	assert res.output.contains('Updated module `tp_pkg`.'), res.output
	assert os.read_file(lockfile_path(project_dir)) or { panic(err) } == lock_before
}

// Case: a lock entry only applies to the source the dependency resolves to. An
// entry that names another source, e.g. after an edit of the lockfile alone,
// is an error with `--locked`, and is resolved anew without it.
fn test_a_lock_entry_from_another_source_does_not_apply() {
	repo_path := os.join_path(test_path, 'os_repo')
	head := create_local_git_module(repo_path, 'os_pkg')
	other_repo_path := os.join_path(test_path, 'os_other')
	create_local_git_module(other_repo_path, 'os_other_pkg')
	project_dir := os.join_path(test_path, 'os_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	dep := repo_path.replace('\\', '/')
	write_project_vmod(project_dir, [dep])

	test_utils.set_test_env(os.join_path(test_path, 'vos1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
	mut lf := read_lockfile(project_dir) or { panic(err) }
	entry := lf.modules[dep] or { panic('no lock entry for `${dep}`') }
	lf.upsert(LockedModule{
		...entry
		url: other_repo_path.replace('\\', '/')
	}, dep)
	write_lockfile(project_dir, lf) or { panic(err) }

	test_utils.set_test_env(os.join_path(test_path, 'vos2'))
	res_locked := cmd_fail_args(@LOCATION, [v_exe, 'install', '--locked'])
	assert res_locked.output.contains('records it from'), res_locked.output
	assert !os.exists(os.join_path(test_path, 'vos2', 'os_other_pkg'))
	res := cmd_ok_args(@LOCATION, [v_exe, 'install', '-v'])
	assert res.output.contains('resolving it anew'), res.output
	assert git_head(os.join_path(test_path, 'vos2', 'os_pkg')) == head
	assert !os.exists(os.join_path(test_path, 'vos2', 'os_other_pkg'))
	lf_after := read_lockfile(project_dir) or { panic(err) }
	entry_after := lf_after.modules[dep] or { panic('no lock entry for `${dep}`') }
	assert entry_after.url == dep
}

fn test_lock_mismatch() {
	entry := LockedModule{
		requested: 'https://github.com/vlang/vsl'
		revision:  'a'
		url:       'https://github.com/vlang/vsl'
	}
	assert lock_mismatch(entry, 'https://github.com/vlang/vsl', 'https://github.com/vlang/vsl.git') == ''
	assert lock_mismatch(entry, 'https://github.com/vlang/vsl@v1.0.0', 'https://github.com/vlang/vsl').contains('records `https://github.com/vlang/vsl` for it')
	assert lock_mismatch(entry, 'https://github.com/vlang/vsl', 'https://github.com/other/vsl').contains('records it from')
	assert lock_mismatch(LockedModule{
		...entry
		url: '--upload-pack=touch pwned'
	}, 'https://github.com/vlang/vsl', 'https://github.com/vlang/vsl') != ''
}

// Case: the submodules of a locked dependency follow the locked revision, so
// that the checkout stays clean, and a later `v install` does not refuse it.
fn test_locked_install_moves_submodules_along() {
	$if windows {
		// The nested `.git/modules` paths of a submodule clone exceed MAX_PATH
		// under the test directories there.
		return
	}
	// Recent git versions refuse to clone submodules from local paths by default.
	os.setenv('GIT_CONFIG_COUNT', '1', true)
	os.setenv('GIT_CONFIG_KEY_0', 'protocol.file.allow', true)
	os.setenv('GIT_CONFIG_VALUE_0', 'always', true)
	defer {
		os.unsetenv('GIT_CONFIG_COUNT')
		os.unsetenv('GIT_CONFIG_KEY_0')
		os.unsetenv('GIT_CONFIG_VALUE_0')
	}
	sub_repo_path := os.join_path(test_path, 'sm_sub')
	create_local_git_module(sub_repo_path, 'sm_sub')
	repo_path := os.join_path(test_path, 'sm_repo')
	create_local_git_module(repo_path, 'sm_pkg')
	git_commit_args := ['-c', 'user.email=ci@vlang.io', '-c', 'user.name=V CI', 'commit', '--quiet']
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'submodule', 'add', '--quiet', sub_repo_path,
		'sub'])
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, ...git_commit_args, '-m', 'add sub'])
	head := git_head(repo_path)
	project_dir := os.join_path(test_path, 'sm_proj')
	os.mkdir_all(project_dir) or { panic(err) }
	write_project_vmod(project_dir, [repo_path])

	test_utils.set_test_env(os.join_path(test_path, 'vsm1'))
	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok_args(@LOCATION, [v_exe, 'install'])

	// The repository moves on to a later commit of its submodule.
	advance_local_git_module(sub_repo_path)
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, 'submodule', 'update', '--quiet', '--remote',
		'sub'])
	cmd_ok_args(@LOCATION, ['git', '-C', repo_path, ...git_commit_args, '-am', 'bump sub'])

	test_utils.set_test_env(os.join_path(test_path, 'vsm2'))
	cmd_ok_args(@LOCATION, [v_exe, 'install', '--locked'])
	installed_path := os.join_path(test_path, 'vsm2', 'sm_pkg')
	assert git_head(installed_path) == head
	status := cmd_ok_args(@LOCATION, ['git', '-C', installed_path, 'status', '--porcelain'])
	assert status.output.trim_space() == ''
	cmd_ok_args(@LOCATION, [v_exe, 'install'])
}

// Case: `--local --locked` outside of a project is an error, the same as a
// plain `--locked` install: there is no lockfile in scope to check against.
fn test_local_locked_outside_a_project_fails() {
	repo_path := os.join_path(test_path, 'll_repo')
	create_local_git_module(repo_path, 'll_pkg')
	test_utils.set_test_env(os.join_path(test_path, 'vll'))
	run_dir := os.join_path(test_path, 'll_run_dir')
	os.mkdir_all(run_dir) or { panic(err) }
	old_dir := os.getwd()
	os.chdir(run_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	res := cmd_fail_args(@LOCATION, [v_exe, 'install', '--local', '--locked', repo_path])
	assert res.output.contains('--locked'), res.output
}
