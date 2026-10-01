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
	assert bundles[0].files == ['SKILL.md', 'references/notes.md'],
		bundles[0].files.join(', ')
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
	forced := skills.install(skill, dir, skills.InstallOptions{force: true})!
	assert !forced.skipped
	assert os.read_file(entry)! != 'local edit\n'
}

fn test_install_dry_run_writes_nothing() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('dry')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	result := skills.install(skill, dir, skills.InstallOptions{dry_run: true})!
	assert result.dry_run
	assert result.written.len == 2
	assert skills.installed(dir).len == 0
}

fn test_dry_run_reports_a_skip_for_an_installed_skill() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('dry_skip')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	result := skills.install(skill, dir, skills.InstallOptions{dry_run: true})!
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

fn test_installed_ignores_entries_without_an_entry_file() {
	dir := scratch_dir('installed')!
	os.mkdir_all(os.join_path(dir, 'not_a_skill'))!
	os.write_file(os.join_path(dir, 'loose.md'), 'x')!
	assert skills.installed(dir).len == 0
	assert skills.installed(os.join_path(test_root, 'nowhere')).len == 0
}

fn test_out_of_date_compares_the_installed_copy_with_the_bundle() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('stale')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	assert skills.out_of_date(vroot, dir).len == 0
	entry := os.join_path(os.join_path_single(dir, 'alpha'), 'SKILL.md')
	os.write_file(entry, 'edited locally\n')!
	assert skills.out_of_date(vroot, dir) == ['alpha']
	// A deleted extra file is stale too.
	os.write_file(entry, os.read_file(os.join_path(skill.directory, 'SKILL.md'))!)!
	os.rm(os.join_path(os.join_path(dir, 'alpha', 'references'), 'notes.md'))!
	assert skills.out_of_date(vroot, dir) == ['alpha']
}

fn test_out_of_date_ignores_a_skill_that_is_not_bundled_anymore() {
	vroot := fixture_root(['alpha'])!
	dir := scratch_dir('dropped')!
	skill := skills.find(vroot, 'alpha') or { panic('alpha is missing') }
	skills.install(skill, dir, skills.InstallOptions{})!
	os.rmdir_all(os.join_path(vroot, 'vlib', 'v', 'skills', 'alpha'))!
	assert skills.out_of_date(vroot, dir).len == 0
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
	assert skills.list_files(dir) == ['SKILL.md', 'alpha.md', 'scripts/run.sh', 'zeta.md'],
		skills.list_files(dir).join(', ')
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