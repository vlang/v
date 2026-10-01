// vtest build: !musl? && !sanitized_job?
module main

import os
import rand
import test_utils { cmd_fail, cmd_ok }

// The tests in this file are fully offline: they build local git repositories
// under `test_path` and install from those, never touching the network.
const test_path = os.join_path(os.vtmp_dir(), 'vpm_lockfile_test_${rand.ulid()}')

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
	cmd_ok(@LOCATION, 'git init -b main ${os.quoted_path(repo_path)}')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} add v.mod')
	cmd_ok(@LOCATION,
		'git -C ${os.quoted_path(repo_path)} -c user.email="ci@vlang.io" -c user.name="V CI" commit -m "initial commit"')
	return git_head(repo_path)
}

// advance_local_git_module adds another commit to the repository at
// `repo_path` and returns the sha of its new HEAD.
fn advance_local_git_module(repo_path string) string {
	os.write_file(os.join_path(repo_path, 'feature.v'), 'module feature\n') or { panic(err) }
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} add feature.v')
	cmd_ok(@LOCATION,
		'git -C ${os.quoted_path(repo_path)} -c user.email="ci@vlang.io" -c user.name="V CI" commit -m "advance head"')
	return git_head(repo_path)
}

// git_head returns the sha of the current HEAD of the git repository at `repo_path`.
fn git_head(repo_path string) string {
	return cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} rev-parse HEAD').output.trim_space()
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
	res := cmd_ok(@LOCATION, '${vexe} install')
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
	cmd_ok(@LOCATION, '${vexe} install')

	write_project_vmod(project_dir, [first_dep, second_dep])
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_merge_second'))
	cmd_ok(@LOCATION, '${vexe} install')

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
	cmd_ok(@LOCATION, '${vexe} install')

	new_head := advance_local_git_module(repo_path)
	assert new_head != head

	// The second install runs against a fresh module store, so the module has
	// to be cloned again: from the locked revision, not from the new HEAD.
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_locked_second'))
	res := cmd_ok(@LOCATION, '${vexe} install')
	assert res.output.contains('Installing `locked_pkg`'), res.output
	installed_head := git_head(os.join_path(test_path, 'vmodules_locked_second', 'locked_pkg'))
	assert installed_head == head
	assert installed_head != new_head
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
	cmd_ok(@LOCATION, '${vexe} install')

	// Replace the history of the source repository, and purge the objects of
	// the old one: clones of a local repository share its whole object store,
	// so the recorded revision has to be garbage-collected before a fresh
	// clone can no longer provide it.
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} checkout --orphan freshroot')
	os.write_file(os.join_path(repo_path, 'fresh.v'), 'module fresh\n') or { panic(err) }
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} add -A')
	cmd_ok(@LOCATION,
		'git -C ${os.quoted_path(repo_path)} -c user.email="ci@vlang.io" -c user.name="V CI" commit -m "fresh history"')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} branch -D main')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} branch -M main')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} reflog expire --expire=now --all')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} gc --prune=now --quiet')
	new_head := git_head(repo_path)
	assert new_head != head

	// The second install runs against a fresh module store, so the module has
	// to be cloned again: the recorded revision is gone from the source, and
	// the install must fail, even without `--locked`. Each run gets its own
	// store/VTMP, so the leftover tmp clone of a failed run cannot collide
	// with the next one (Windows cannot remove the read-only git objects).
	test_utils.set_test_env(os.join_path(test_path, 'vu2'))
	res := cmd_fail(@LOCATION, '${vexe} install')
	assert res.output.contains('failed to install'), res.output
	test_utils.set_test_env(os.join_path(test_path, 'vu3'))
	res_verbose := cmd_fail(@LOCATION, '${vexe} install -v')
	assert res_verbose.output.contains('failed to checkout'), res_verbose.output
	test_utils.set_test_env(os.join_path(test_path, 'vu4'))
	res_locked := cmd_fail(@LOCATION, '${vexe} install --locked')
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
	cmd_ok(@LOCATION, '${vexe} install')

	// The project now asks for a tag, while the lockfile records the plain path.
	write_project_vmod(project_dir, ['${dep}@v1.0.0'])
	res := cmd_fail(@LOCATION, '${vexe} install --locked')
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
	res := cmd_fail(@LOCATION, '${vexe} install --locked')
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
	res := cmd_ok(@LOCATION, '${vexe} install --local ${os.quoted_path(repo_path)}')
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
	res := cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
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
	cmd_ok(@LOCATION, '${vexe} install')

	new_head := advance_local_git_module(repo_path)
	assert new_head != head
	cmd_ok(@LOCATION, '${vexe} update updated_pkg')

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
	cmd_ok(@LOCATION, '${vexe} install')

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
