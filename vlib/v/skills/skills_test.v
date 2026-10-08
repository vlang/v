module main

import os
import v.skills

const test_root = os.join_path(os.vtmp_dir(), 'v_skills_test_${os.getpid()}')

// fixture_root creates a fake V source tree holding `bundled` skill bundles, and
// returns the vroot that `v.skills` should read them from.
fn fixture_root(bundled []string) !string {
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root)!
	vroot := os.join_path(test_root, 'vroot')
	os.mkdir_all(os.join_path(vroot, 'vlib', 'v', 'skills'))!
	for name in bundled {
		write_bundle(vroot, name)!
	}
	return vroot
}

// write_bundle creates one skill directory holding `SKILL.md` and a reference
// file below `references/`.
fn write_bundle(vroot string, name string) ! {
	dir := os.join_path(vroot, 'vlib', 'v', 'skills', name)
	os.mkdir_all(dir)!
	content := '---\nname: ${name}\ndescription: Test the ${name} skill.\n---\n\n# ${name}\n\nBody.\n'
	os.write_file(os.join_path_single(dir, 'SKILL.md'), content)!
	os.mkdir_all(os.join_path(dir, 'references'))!
	os.write_file(os.join_path(dir, 'references', 'notes.md'), 'notes for ${name}\n')!
}

// scratch_dir returns a fresh empty install directory.
fn scratch_dir(name string) !string {
	dir := os.join_path(test_root, 'target', name)
	os.mkdir_all(dir)!
	return dir
}

fn test_catalog_reads_front_matter_and_nested_files() {
	vroot := fixture_root(['beta', 'alpha'])!
	bundles := skills.catalog(vroot)
	assert bundles.len == 2
	assert bundles[0].name == 'alpha'
	assert bundles[1].name == 'beta'
	assert bundles[0].description == 'Test the alpha skill.'
	// The paths inside a bundle are repository-relative, so they always use `/`.
	assert bundles[0].files == ['SKILL.md', 'references/notes.md'], bundles[0].files.join(', ')
}

fn test_catalog_skips_a_directory_without_an_entry_file() {
	vroot := fixture_root(['good'])!
	os.mkdir_all(os.join_path(vroot, 'vlib', 'v', 'skills', 'incomplete'))!
	bundles := skills.catalog(vroot)
	assert bundles.len == 1
	assert bundles[0].name == 'good'
}

fn test_catalog_is_empty_for_a_tree_without_skills() {
	vroot := fixture_root([])!
	assert skills.catalog(vroot).len == 0
	assert skills.catalog(os.join_path(test_root, 'absent')).len == 0
}

fn test_find_returns_none_for_an_unknown_skill() {
	vroot := fixture_root(['alpha'])!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	assert skill.name == 'alpha'
	assert skills.find(vroot, 'missing') == none
}

fn test_parse_front_matter_requires_a_block_and_both_keys() {
	fields := skills.parse_front_matter('---\nname: a\ndescription: b\n---\n\nbody') or {
		panic('valid front matter was rejected')
	}
	assert fields['name'] == 'a'
	assert fields['description'] == 'b'
	// A missing closing fence still parses: every following line counts.
	open := skills.parse_front_matter('---\nname: a\ndescription: b') or {
		panic('unterminated front matter was rejected')
	}
	assert open['name'] == 'a'
	// No front matter at all.
	assert skills.parse_front_matter('# just a heading\n') == none
	// Missing description.
	assert skills.parse_front_matter('---\nname: a\n---\n') == none
	// Missing name.
	assert skills.parse_front_matter('---\ndescription: b\n---\n') == none
}

fn test_parse_front_matter_unquotes_values_and_ignores_comments() {
	fields := skills.parse_front_matter(
		'\xEF\xBB\xBF---\n# a comment\nname: "quoted"\ndescription: \'single\'\n---\n',
	) or {
		panic('valid front matter was rejected')
	}
	assert fields['name'] == 'quoted'
	assert fields['description'] == 'single'
}

fn test_parse_front_matter_keeps_an_unbalanced_quote() {
	fields := skills.parse_front_matter("---\nname: 'a\ndescription: b\n---\n") or {
		panic('valid front matter was rejected')
	}
	assert fields['name'] == "'a"
}

fn test_install_copies_the_bundle_and_reports_written_files() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('install')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	result := skills.install(skill, dir, skills.InstallOptions{})!
	assert !result.skipped
	assert result.written == ['SKILL.md', 'references/notes.md'], result.written.join(', ')
	assert os.is_file(os.join_path(os.join_path_single(dir, 'alpha'), 'SKILL.md'))
	notes := os.join_path(os.join_path(dir, 'alpha', 'references'), 'notes.md')
	assert os.read_file(notes)! == 'notes for alpha\n'
	assert skills.installed(dir) == ['alpha']
}

fn test_an_install_leaves_no_temporary_directory_behind() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('atomic_clean')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{ force: true })!
	// The copy is made in a sibling directory and moved over, so a failure
	// cannot leave a half-written skill. Nothing dot-prefixed may survive
	// beside the skill, on either side of that move.
	for entry in os.ls(dir)! {
		assert !entry.starts_with('.'), entry
	}
}

fn test_install_skips_an_existing_skill_unless_forced() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('skip')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	// A local edit must survive a plain reinstall.
	entry := os.join_path(os.join_path_single(dir, 'alpha'), 'SKILL.md')
	os.write_file(entry, 'local edit\n')!
	skipped := skills.install(skill, dir, skills.InstallOptions{})!
	assert skipped.skipped
	assert skipped.written.len == 0
	assert os.read_file(entry)! == 'local edit\n'
	// `force` restores the bundled content.
	forced := skills.install(skill, dir, skills.InstallOptions{ force: true })!
	assert !forced.skipped
	assert os.read_file(entry)! != 'local edit\n'
}

fn test_install_refuses_a_regular_file_at_the_skill_destination() {
	vroot := fixture_root(['alpha'])!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	for force in [false, true] {
		dir := scratch_dir('file_destination_${force}')!
		dest := os.join_path_single(dir, 'alpha')
		os.write_file(dest, 'local file')!
		mut failed := false
		skills.install(skill, dir, skills.InstallOptions{ force: force }) or { failed = true }
		assert failed, 'an existing file must not be replaced by a skill directory'
		assert os.read_file(dest)! == 'local file'
		assert os.ls(dir)! == ['alpha']
	}
}

fn test_install_dry_run_writes_nothing() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('dry')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	result := skills.install(skill, dir, skills.InstallOptions{ dry_run: true })!
	assert result.dry_run
	assert result.written.len == 2
	assert skills.installed(dir).len == 0
}

fn test_find_rejects_names_that_resolve_to_a_bundle_through_traversal() {
	vroot := fixture_root(['alpha'])!
	for name in ['../skills/alpha', './alpha', os.join_path(skills.bundled_root(vroot), 'alpha')] {
		assert skills.find(vroot, name) == none, name
	}
	assert skills.find(vroot, 'alpha') != none
}

fn test_install_rejects_traversal_and_mismatched_names_without_writing() {
	vroot := fixture_root(['alpha'])!
	dir := skills.target_dir(.project_root, vroot)
	alpha := skills.find(vroot, 'alpha') or { panic('missing alpha') }
	victim := os.join_path(vroot, 'victim', 'alpha')
	os.mkdir_all(victim)!
	marker := os.join_path(victim, 'keep.txt')
	os.write_file(marker, 'unrelated')!
	for name in ['', '..', '../../victim/alpha', 'a\\..\\alpha', 'beta'] {
		skill := skills.Skill{
			...alpha
			name: name
		}
		for opts in [skills.InstallOptions{}, skills.InstallOptions{ force: true },
			skills.InstallOptions{ force: true, dry_run: true }] {
			assert skills.install(skill, dir, opts) == none, name
			assert os.read_file(marker)! == 'unrelated'
			assert !os.exists(dir)
		}
	}
	skills.install(alpha, dir, skills.InstallOptions{})!
	assert os.is_file(os.join_path(dir, 'alpha', skills.entry_file))
}

fn test_install_validates_all_file_paths_before_replacing_an_installation() {
	vroot := fixture_root(['alpha'])!
	dir := skills.target_dir(.project_root, vroot)
	alpha := skills.find(vroot, 'alpha') or { panic('missing alpha') }
	skills.install(alpha, dir, skills.InstallOptions{})!
	entry := os.join_path(dir, 'alpha', skills.entry_file)
	os.write_file(entry, 'local edit')!
	outside := os.join_path(vroot, 'outside.txt')
	os.write_file(outside, 'unrelated')!
	for relative in ['../../outside.txt', 'references/../../outside.txt', outside, '..\\outside.txt',
		'C:/outside.txt'] {
		skill := skills.Skill{
			...alpha
			files: [skills.entry_file, relative]
		}
		for opts in [skills.InstallOptions{ force: true },
			skills.InstallOptions{ force: true, dry_run: true }] {
			assert skills.install(skill, dir, opts) == none, relative
			assert os.read_file(entry)! == 'local edit'
			assert os.read_file(outside)! == 'unrelated'
		}
	}
	$if !windows {
		link := os.join_path(alpha.directory, 'outside.txt')
		os.symlink(outside, link)!
		skill := skills.Skill{
			...alpha
			files: [skills.entry_file, 'outside.txt']
		}
		assert skills.install(skill, dir, skills.InstallOptions{ force: true }) == none
		assert os.read_file(entry)! == 'local edit'
		assert os.read_file(outside)! == 'unrelated'
		fifo := os.join_path(alpha.directory, 'input.fifo')
		created := os.exec(['mkfifo', fifo])
		assert created.exit_code == 0, created.output
		assert os.stat(fifo)!.get_filetype() == .fifo
		pipe_skill := skills.Skill{
			...alpha
			files: [skills.entry_file, 'input.fifo']
		}
		assert skills.install(pipe_skill, dir, skills.InstallOptions{ force: true }) == none
		assert os.read_file(entry)! == 'local edit'
	}
}

fn test_dry_run_reports_a_skip_for_an_installed_skill() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('dry_skip')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	result := skills.install(skill, dir, skills.InstallOptions{ dry_run: true })!
	assert result.skipped
}

fn test_remove_reports_whether_anything_was_there() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('remove')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	result := skills.remove(dir, 'alpha')!
	assert result.removed
	assert skills.installed(dir).len == 0
	assert !skills.remove(dir, 'alpha')!.removed
}

fn test_remove_rejects_traversal_names_and_preserves_other_directories() {
	root := fixture_root(['alpha'])!
	dir := skills.target_dir(.project_root, root)
	skill := skills.find(root, 'alpha') or { panic('missing alpha') }
	skills.install(skill, dir, skills.InstallOptions{})!
	victim := os.join_path(root, 'victim')
	os.mkdir_all(victim)!
	marker := os.join_path(victim, 'keep.txt')
	os.write_file(marker, 'unrelated')!
	for name in ['', '..', '../..', '../../victim', 'alpha/../../victim', 'a\\..\\victim'] {
		assert skills.remove(dir, name) == none, name
		assert os.read_file(marker)! == 'unrelated'
		assert os.is_file(os.join_path(dir, 'alpha', skills.entry_file))
	}
	$if !windows {
		link := os.join_path(dir, 'linked')
		os.symlink(victim, link)!
		assert skills.remove(dir, 'linked') == none
		assert os.read_file(marker)! == 'unrelated'
		assert os.is_link(link)
	}
}

// `--force` over a symlink used to delete the *target's* contents.
//
// `os.is_dir` follows a link and `os.rmdir_all` follows its own argument, so
// `os.rmdir_all(dest)` on a link lists and removes what the link points at. The
// install reported success, then failed partway with "Not a directory", by which
// point the unrelated directory was already empty.
fn test_forced_install_refuses_a_symlink_target_and_preserves_it() {
	root := fixture_root(['alpha'])!
	dir := skills.target_dir(.project_root, root)
	os.mkdir_all(dir)!
	skill := skills.find(root, 'alpha') or { panic('missing alpha') }
	victim := os.join_path(root, 'victim')
	os.mkdir_all(victim)!
	marker := os.join_path(victim, 'keep.txt')
	os.write_file(marker, 'unrelated')!
	$if !windows {
		link := os.join_path(dir, 'alpha')
		os.symlink(victim, link)!
		for opts in [skills.InstallOptions{ force: true },
			skills.InstallOptions{ force: true, dry_run: true }] {
			assert skills.install(skill, dir, opts) == none, 'a link must not be installed over'
			assert os.read_file(marker)! == 'unrelated', 'the victim lost a file'
			assert os.is_link(link), 'the link itself must survive'
			entries := os.ls(victim)!
			assert entries.len == 1, 'the victim directory lost entries'
		}
		os.rm(link)!
	}
	// An ordinary directory still installs, so the guard is not just refusing
	// every name that exists.
	skills.install(skill, dir, skills.InstallOptions{ force: true })!
	assert os.is_file(os.join_path(dir, 'alpha', skills.entry_file))
}

fn test_installed_ignores_entries_without_an_entry_file() {
	dir := scratch_dir('installed')!
	os.mkdir_all(os.join_path(dir, 'not_a_skill'))!
	os.write_file(os.join_path(dir, 'loose.md'), 'x')!
	assert skills.installed(dir).len == 0
	assert skills.installed(os.join_path(test_root, 'nowhere')).len == 0
}

fn test_refresh_candidates_splits_stale_work_from_a_local_edit() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('refresh_split')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	mut refreshable := []string{}
	mut held := []string{}
	refreshable, held = skills.refresh_candidates(vroot, dir)
	assert refreshable.len == 0 && held.len == 0
	entry := os.join_path(os.join_path_single(dir, 'alpha'), 'SKILL.md')
	os.write_file(entry, 'edited locally\n')!
	refreshable, held = skills.refresh_candidates(vroot, dir)
	// An edit here is not pending work, so it must never be offered as safe to
	// refresh. This is the distinction a content comparison cannot make, and the
	// reason `v skills list` reports these separately.
	assert refreshable.len == 0, refreshable.str()
	assert held == ['alpha'], held.str()
	// Deleting a bundled file is also a local edit, not a stale copy: whether the
	// file was removed on purpose is not something a digest can answer.
	os.write_file(entry, os.read_file(os.join_path(skill.directory, 'SKILL.md'))!)!
	os.rm(os.join_path(os.join_path(dir, 'alpha', 'references'), 'notes.md'))!
	refreshable, held = skills.refresh_candidates(vroot, dir)
	assert refreshable.len == 0, refreshable.str()
	assert held == ['alpha'], held.str()
	// Moving the bundle on, with the installed copy untouched, is the one case
	// that is safe to refresh. The reinstall is forced, because an install over
	// an existing directory is a no-op otherwise and would leave the missing file
	// in place.
	skills.install(skill, dir, skills.InstallOptions{ force: true })!
	os.write_file(os.join_path_single(skill.directory, 'SKILL.md'),
		'---\nname: alpha\ndescription: Test the alpha skill.\n---\n\n# alpha\n\nA newer body.\n')!
	refreshable, held = skills.refresh_candidates(vroot, dir)
	assert refreshable == ['alpha'], refreshable.str()
	assert held.len == 0, held.str()
}

fn test_origin_state_is_current_after_an_install() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_current')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	assert skills.origin_state(vroot, dir, 'alpha') == .current
}

fn test_origin_state_is_stale_when_only_the_bundle_moved_on() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_stale')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	os.write_file(os.join_path_single(skill.directory, 'SKILL.md'),
		'---\nname: alpha\ndescription: Test the alpha skill.\n---\n\n# alpha\n\nA newer body.\n')!
	// The installed copy is untouched, so refreshing it cannot lose anything.
	assert skills.origin_state(vroot, dir, 'alpha') == .stale
}

fn test_origin_state_is_modified_when_the_installed_copy_is_edited() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_modified')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	os.write_file(os.join_path(os.join_path_single(dir, 'alpha'), 'SKILL.md'), 'edited here\n')!
	assert skills.origin_state(vroot, dir, 'alpha') == .modified
}

fn test_origin_state_is_unknown_without_a_record() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_unknown')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	// An installation from before the record existed: the files are there, but
	// nothing says what was installed, so `current` cannot be claimed.
	os.rm(os.join_path_single(dir, skills.origin_file))!
	assert skills.origin_state(vroot, dir, 'alpha') == .unknown
}

fn test_origin_state_is_current_for_a_skill_that_is_no_longer_bundled() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_dropped')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	os.rmdir_all(os.join_path(vroot, 'vlib', 'v', 'skills', 'alpha'))!
	// There is no newer copy to move to, so this is not work waiting to be done.
	assert skills.origin_state(vroot, dir, 'alpha') == .current
}

fn test_the_origin_file_is_neither_a_skill_nor_part_of_one() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_invisible')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	// The record sits beside the skills rather than inside one, so it is not
	// listed as a skill and is never compared as skill content.
	assert os.is_file(os.join_path_single(dir, skills.origin_file))
	assert skills.installed(dir) == ['alpha']
	assert skills.origin_state(vroot, dir, 'alpha') == .current
}

fn test_installing_one_skill_keeps_the_record_of_another() {
	vroot := fixture_root(['alpha', 'beta'])!
	dir := scratch_dir('origin_siblings')!
	alpha := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	beta := skills.find(vroot, 'beta') or { panic('beta is missing') }
	skills.install(alpha, dir, skills.InstallOptions{})!
	skills.install(beta, dir, skills.InstallOptions{})!
	// A record that was rewritten rather than merged would leave alpha with no
	// entry at all, which reads as `unknown` rather than `current`.
	assert skills.origin_state(vroot, dir, 'alpha') == .current
	assert skills.origin_state(vroot, dir, 'beta') == .current
}

fn test_a_dry_run_records_nothing() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_dry')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	result := skills.install(skill, dir, skills.InstallOptions{
		dry_run: true
	})!
	assert result.dry_run
	assert !os.exists(os.join_path_single(dir, skills.origin_file))
	assert skills.installed(dir).len == 0
}

fn test_forget_origin_drops_the_record_and_leaves_the_files() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_forget')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	assert skills.origin_state(vroot, dir, 'alpha') == .current
	skills.forget_origin(dir, 'alpha')
	assert skills.origin_state(vroot, dir, 'alpha') == .unknown
	// Only the record is gone.
	assert skills.installed(dir) == ['alpha']
}

fn test_forget_origin_ignores_a_skill_that_has_no_record() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('origin_forget_absent')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	skills.forget_origin(dir, 'beta')
	assert skills.origin_state(vroot, dir, 'alpha') == .current
}

fn test_remove_forgets_the_record_it_left_behind() {
	vroot := fixture_root(['alpha', 'beta'])!
	dir := scratch_dir('origin_removed')!
	alpha := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	beta := skills.find(vroot, 'beta') or { panic('beta is missing') }
	skills.install(alpha, dir, skills.InstallOptions{})!
	skills.install(beta, dir, skills.InstallOptions{})!
	skills.remove(dir, 'alpha')!
	assert skills.installed(dir) == ['beta']
	// Removing one skill keeps the record the other one needs.
	assert skills.origin_state(vroot, dir, 'beta') == .current
	assert os.is_file(os.join_path_single(dir, skills.origin_file))
	// Put alpha's files back by hand, so the state that comes out is decided by
	// the record rather than by an install writing a fresh one. Identical content
	// under a surviving record would read as `current`, which would be a claim
	// about an installation that no longer exists.
	for relative in skills.list_files(alpha.directory) {
		target := os.join_path(os.join_path_single(dir, 'alpha'), relative)
		os.mkdir_all(os.dir(target))!
		os.write_file(target, os.read_file(os.join_path(alpha.directory, relative))!)!
	}
	assert skills.origin_state(vroot, dir, 'alpha') == .unknown
	// With the last skill gone the record goes too, rather than sitting there
	// empty in a directory that holds no skills.
	skills.remove(dir, 'beta')!
	assert !os.exists(os.join_path_single(dir, skills.origin_file))
}

fn test_content_digest_covers_the_names_and_not_their_order() {
	vroot := fixture_root(['alpha'])!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	files := skills.list_files(skill.directory)
	direct := skills.content_digest(skill.directory, files)!
	assert skills.content_digest(skill.directory, files.clone().reverse())! == direct
	// A different file under the same bytes is a different installation.
	assert skills.content_digest(skill.directory, files[1..])! != direct
	// Content is part of it too, and where the file sits is not: the same name
	// with the same bytes digests the same in any directory.
	first := os.join_path(os.join_path(test_root, 'digest'), 'first')
	second := os.join_path(os.join_path(test_root, 'digest'), 'second')
	os.mkdir_all(first)!
	os.mkdir_all(second)!
	os.write_file(os.join_path_single(first, 'one.md'), 'body\n')!
	os.write_file(os.join_path_single(second, 'one.md'), 'body\n')!
	assert skills.content_digest(first, ['one.md'])! ==
		skills.content_digest(second, ['one.md'])!
	os.write_file(os.join_path_single(second, 'one.md'), 'other body\n')!
	assert skills.content_digest(first, ['one.md'])! !=
		skills.content_digest(second, ['one.md'])!
	// A rename under the same bytes is a different installation, which is what
	// makes a renamed file read as a local change rather than as untouched.
	os.mv(os.join_path_single(second, 'one.md'), os.join_path_single(second, 'renamed.md'))!
	assert skills.content_digest(first, ['one.md'])! !=
		skills.content_digest(second, ['renamed.md'])!
}

fn test_target_dir_selects_the_project_or_the_home_directory() {
	project := os.join_path(test_root, 'proj')
	assert skills.target_dir(.project_root, project) ==
		os.join_path(project, '.agents/skills')
	assert skills.target_dir(.home_dir, project) ==
		os.join_path(os.home_dir(), '.agents/skills')
}

fn test_list_files_puts_the_entry_file_first_and_sorts_the_rest() {
	dir := os.join_path(test_root, 'listed')
	os.mkdir_all(os.join_path(dir, 'scripts'))!
	os.write_file(os.join_path_single(dir, 'SKILL.md'), 'x')!
	os.write_file(os.join_path(dir, 'zeta.md'), 'z')!
	os.write_file(os.join_path(dir, 'alpha.md'), 'a')!
	os.write_file(os.join_path(dir, 'scripts', 'run.sh'), 's')!
	assert skills.list_files(dir) == ['SKILL.md', 'alpha.md', 'scripts/run.sh', 'zeta.md'], skills.list_files(dir).join(', ')
	assert skills.list_files(os.join_path(test_root, 'unlisted')).len == 0
}

fn test_relative_to_strips_a_matching_base() {
	base := os.join_path(test_root, 'base')
	assert skills.relative_to(base, os.join_path(base, 'a', 'b.v')) == 'a/b.v'
	// The base itself has nothing left once the prefix is stripped, so it reads as
	// the current directory rather than as an empty path.
	assert skills.relative_to(base, base) == '.', skills.relative_to(base, base)
	assert skills.relative_to(base, os.join_path(test_root, 'other')) ==
		os.to_slash(os.join_path(test_root, 'other'))
}

fn test_human_size_picks_a_unit() {
	assert skills.human_size(10) == '10 B'
	assert skills.human_size(2048) == '2.0 KiB'
	assert skills.human_size(3 * 1024 * 1024) == '3.0 MiB'
}

// raw_bundle creates a skill directory whose SKILL.md holds exactly `front`, so a
// test can write front matter the generator above would not produce.
fn raw_bundle(vroot string, dir_name string, front string) !string {
	dir := os.join_path(vroot, 'vlib', 'v', 'skills', dir_name)
	os.mkdir_all(dir)!
	os.write_file(os.join_path_single(dir, 'SKILL.md'), front)!
	return dir
}

// accepts_name reports whether `name` satisfies the spec.
fn accepts_name(name string) bool {
	skills.validate_name(name) or {
		return false
	}
	return true
}

// rejects_name reports whether `name` breaks the spec.
fn rejects_name(name string) bool {
	return !accepts_name(name)
}

// accepts_bundle reports whether the bundle at `directory` satisfies the spec.
fn accepts_bundle(directory string) bool {
	skills.validate_bundle(directory) or {
		return false
	}
	return true
}

// bundle_problem returns what is wrong with the bundle at `directory`, or an empty
// string when nothing is.
fn bundle_problem(directory string) string {
	skills.validate_bundle(directory) or {
		return err.msg()
	}
	return ''
}

fn test_validate_name_accepts_what_the_spec_allows() {
	assert accepts_name('v')
	assert accepts_name('v-lang')
	assert accepts_name('go2')
	assert accepts_name('a1-b2-c3')
	assert accepts_name('x'.repeat(skills.max_name_length))
}

fn test_validate_name_rejects_what_the_spec_forbids() {
	assert rejects_name('')
	assert rejects_name('V-lang')
	assert rejects_name('v_lang')
	assert rejects_name('-v')
	assert rejects_name('v-')
	assert rejects_name('v--lang')
	assert rejects_name('v lang')
	assert rejects_name('x'.repeat(skills.max_name_length + 1))
}

fn test_validate_bundle_returns_the_name_of_a_good_bundle() {
	vroot := fixture_root(['v-lang'])!
	dir := os.join_path(vroot, 'vlib', 'v', 'skills', 'v-lang')
	assert skills.validate_bundle(dir)! == 'v-lang'
}

fn test_validate_bundle_rejects_a_name_that_disagrees_with_the_directory() {
	// The name an agent matches on and the name `v skills remove` addresses have to
	// be the same one, so a mismatch is refused rather than installed.
	vroot := fixture_root([])!
	dir := raw_bundle(vroot, 'v-lang', '---\nname: v-language\ndescription: A skill.\n---\n\nBody.\n')!
	problem := bundle_problem(dir)
	assert problem != '', 'a mismatched name must be refused'
	assert problem.contains('v-language') && problem.contains('v-lang'), problem
}

fn test_validate_bundle_rejects_a_missing_description() {
	vroot := fixture_root([])!
	dir := raw_bundle(vroot, 'thin', '---\nname: thin\n---\n\nBody.\n')!
	assert !accepts_bundle(dir), 'a skill with no description is unusable: an agent cannot tell when to load it'
}

fn test_validate_bundle_rejects_a_description_over_the_spec_limit() {
	vroot := fixture_root([])!
	long := 'd'.repeat(skills.max_description_length + 1)
	dir := raw_bundle(vroot, 'wordy', '---\nname: wordy\ndescription: ${long}\n---\n\nBody.\n')!
	assert !accepts_bundle(dir), 'an over-long description must be refused'
	// One character under the limit is still fine.
	ok := 'd'.repeat(skills.max_description_length)
	dir2 := raw_bundle(vroot, 'brief', '---\nname: brief\ndescription: ${ok}\n---\n\nBody.\n')!
	assert accepts_bundle(dir2)
}

fn test_install_refuses_a_bundle_that_fails_validation() {
	vroot := fixture_root([])!
	dir := raw_bundle(vroot, 'broken', '---\nname: wrong-name\ndescription: A skill.\n---\n\nBody.\n')!
	skill := skills.Skill{
		name:        'broken'
		description: 'A skill.'
		directory:   dir
	}
	target := scratch_dir('refused')!
	if _ := skills.install(skill, target, skills.InstallOptions{}) {
		assert false, 'a bundle with a mismatched name must not be installed'
	}
	assert !os.is_dir(os.join_path_single(target, 'broken')), 'nothing may be written'
}

fn test_invalid_bundled_reports_what_is_wrong_with_each_one() {
	vroot := fixture_root(['good'])!
	raw_bundle(vroot, 'bad-name', '---\nname: other\ndescription: A skill.\n---\n\nBody.\n')!
	raw_bundle(vroot, 'thin', '---\nname: thin\n---\n\nBody.\n')!
	problems := skills.invalid_bundled(vroot)
	assert problems.len == 2, problems.join('\n')
	joined := problems.join('\n')
	assert joined.contains('bad-name: ') && joined.contains('other'), 'the mismatch should be reported, got: ' + joined
	assert joined.contains('thin: '), joined
	// The valid bundle is not reported.
	assert !joined.contains('good'), joined
}

fn test_invalid_bundled_is_empty_when_every_bundle_is_valid() {
	vroot := fixture_root(['alpha', 'beta'])!
	assert skills.invalid_bundled(vroot) == []
	assert skills.invalid_bundled(os.join_path(test_root, 'absent')) == []
}

// The bundled skills are the real ones, so the properties that hold for a fixture
// are checked here too.
//
// `.vcheckignore` exempts the bundled `SKILL.md` files from `v check-md`, because
// the spec allows a `description` of up to 1024 characters and the repository's
// limit for ordinary lines is 100. These tests hold the rest of the file to what
// the exemption gives up.
fn test_every_bundled_skill_body_stays_within_the_check_md_line_limit() {
	for skill in skills.catalog(@VEXEROOT) {
		content := os.read_file(os.join_path_single(skill.directory, 'SKILL.md')) or {
			continue
		}
		_, body := split_front_matter(skills.strip_bom(content))
		for line in body.split_into_lines() {
			assert line.len <= 100, '${skill.name}: a body line is ${line.len} ' +
				'characters, over the ${max_line_length} that `v check-md` allows: ${line}'
		}
	}
}

fn test_every_bundled_skill_has_balanced_code_fences() {
	for skill in skills.catalog(@VEXEROOT) {
		content := os.read_file(os.join_path_single(skill.directory, 'SKILL.md')) or {
			continue
		}
		_, body := split_front_matter(skills.strip_bom(content))
		mut fences := 0
		for line in body.split_into_lines() {
			if line.starts_with('```') {
				fences++
			}
		}
		assert fences % 2 == 0, '${skill.name}: ${fences} fence markers, so a code ' +
			'block is left open and everything after it is read as code'
	}
}

// `v check-md` compiles every `v` fence it finds, and a skill's examples are
// fragments: they have no `module main` and reference functions that do not exist.
// They must therefore be marked `v ignore`, or `check-md` tries to build them.
fn test_every_v_fence_in_a_bundled_skill_is_marked_ignore() {
	for skill in skills.catalog(@VEXEROOT) {
		content := os.read_file(os.join_path_single(skill.directory, 'SKILL.md')) or {
			continue
		}
		_, body := split_front_matter(skills.strip_bom(content))
		for line in body.split_into_lines() {
			if !line.starts_with('```v') {
				continue
			}
			assert line.trim_space() != '```v', '${skill.name}: this fence would be ' +
				'compiled as an example, but a skill fragment does not build on its own'
		}
	}
}

fn test_every_bundled_reference_file_is_still_checked_by_check_md() {
	// The exemption is deliberately narrow. A reference file that `check-md` never
	// looks at would let a real line-length or formatting error through.
	ignore := os.join_path(skills.bundled_root(@VEXEROOT), '.vcheckignore')
	content := os.read_file(ignore) or {
		panic('the bundled skills must ship a .vcheckignore explaining the exemption')
	}
	for line in content.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed == '' || trimmed.starts_with('#') {
			continue
		}
		assert !trimmed.contains('references'), 'the exemption must stay narrow; `references/` should still be checked: ${trimmed}'
	}
}

// split_front_matter returns the front matter lines and the body of a SKILL.md.
fn split_front_matter(content string) ([]string, string) {
	lines := content.split_into_lines()
	mut start := -1
	for i, line in lines {
		if line.trim_space() == '---' {
			start = i
			break
		}
	}
	if start < 0 {
		return []string{}, content
	}
	mut end := lines.len
	for i in start + 1 .. lines.len {
		if lines[i].trim_space() == '---' {
			end = i
			break
		}
	}
	return lines[start + 1..end], lines[end..].join('\n')
}

// max_line_length is the limit `v check-md` puts on an ordinary markdown line.
const max_line_length = 100

fn test_install_refuses_symlink_provenance_before_changing_content() {
	$if windows {
		return
	}
	vroot := fixture_root(['alpha'])!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	dir := scratch_dir('origin_link')!
	outside := os.join_path(test_root, 'outside.txt')
	os.write_file(outside, 'keep outside content')!
	origin := os.join_path(dir, skills.origin_file)
	os.symlink(outside, origin)!
	skills.install(skill, dir, skills.InstallOptions{}) or {
		assert err.msg().contains('symlink provenance')
		assert os.read_file(outside)! == 'keep outside content'
		assert !os.exists(os.join_path(dir, 'alpha'))
		return
	}
	assert false, 'installation followed the provenance link'
}

fn test_forget_origin_refuses_a_symlink_provenance_rewrite() {
	$if windows {
		return
	}
	vroot := fixture_root(['alpha', 'beta'])!
	dir := scratch_dir('forget_origin_link')!
	for name in ['alpha', 'beta'] {
		skill := skills.find(vroot, name) or { panic('missing bundle') }
		skills.install(skill, dir, skills.InstallOptions{})!
	}
	origin := os.join_path(dir, skills.origin_file)
	content := os.read_file(origin)!
	outside := os.join_path(test_root, 'outside-origin.json')
	os.write_file(outside, content)!
	os.rm(origin)!
	os.symlink(outside, origin)!
	skills.forget_origin(dir, 'alpha')
	assert os.read_file(outside)! == content
	assert os.is_link(origin)
}

fn test_install_refuses_nonregular_provenance_before_changing_content() {
	vroot := fixture_root(['alpha'])!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	dir := scratch_dir('origin_directory')!
	os.mkdir(os.join_path(dir, skills.origin_file))!
	skills.install(skill, dir, skills.InstallOptions{}) or {
		assert err.msg().contains('not a regular file')
		assert !os.exists(os.join_path(dir, 'alpha'))
		return
	}
	assert false, 'installation accepted a directory as its provenance file'
}

fn test_unreadable_local_edit_cannot_be_classified_as_stale() {
	$if windows {
		return
	}
	$if !windows {
		if os.getuid() == 0 { return }
	}
	vroot := fixture_root(['alpha'])!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	supplementary := os.join_path(skill.directory, 'references', 'notes.md')
	os.write_file(supplementary, '')!
	dir := scratch_dir('unreadable_edit')!
	skills.install(skill, dir, skills.InstallOptions{})!
	installed := os.join_path(dir, 'alpha', 'references', 'notes.md')
	os.write_file(installed, 'local work')!
	os.chmod(installed, 0o000)!
	defer { os.chmod(installed, 0o600) or {} }
	os.write_file(supplementary, 'new bundle')!
	assert skills.origin_state(vroot, dir, 'alpha') == .unknown
	skills.content_digest(os.join_path(dir, 'alpha'), skill.files) or { return }
	assert false, 'unreadable file was hashed as empty'
}
