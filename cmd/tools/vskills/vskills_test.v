// Tests for `v skills`, the CLI over the `v.skills` module.
//
// The module tests cover install, remove and the catalog. What is tested here is
// the command line on top of it: the subcommands, the flags, and what each one
// says, because that text is what a user and an agent read.

module main

import os
import v.skills

// test_root is the throwaway tree the tests install into.
const test_root = os.join_path(os.vtmp_dir(), 'v_vskills_test_${os.getpid()}')

// bundle writes one skill directory under `root` and returns the tree root.
//
// The tests build their own bundle rather than using the compiler's, so a change
// to a shipped skill cannot break the command line tests.
fn bundle(root string, name string, description string) {
	dir := os.join_path(skills.bundled_root(root), name)
	os.mkdir_all(os.join_path_single(dir, 'references')) or {
		panic(err)
	}
	os.write_file(os.join_path_single(dir, skills.entry_file),
		'---\nname: ${name}\ndescription: ${description}\n---\n\n# ${name}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'references', 'note.md'), 'note\n') or {
		panic(err)
	}
}

// project builds a V source tree holding two skills and returns its root.
//
// The bundles go under `vlib/v/skills`, which is where `v skills` looks for them:
// the catalog is read from the source tree rather than embedded into the binary.
fn project() string {
	os.rmdir_all(test_root) or {}
	root := os.join_path(test_root, 'vroot')
	os.mkdir_all(root) or {
		panic(err)
	}
	os.write_file(os.join_path_single(root, 'v.mod'), "Module {\n\tname: 'vtest'\n}\n") or {
		panic(err)
	}
	bundle(root, 'alpha', 'The first skill.')
	bundle(root, 'beta', 'The second skill.')
	return root
}

// installed_dir is where a project install of `name` lands.
fn installed_dir(root string, name string) string {
	return os.join_path(skills.target_dir(.project_root, root), name)
}

// entry_of is the installed `SKILL.md` of `name`.
fn entry_of(root string, name string) string {
	return os.join_path_single(installed_dir(root, name), skills.entry_file)
}

// run calls the subcommand with `args` against the tree at `root`.
fn run(root string, args ...string) Output {
	return run_at(root, root, args)
}

// raw_bundle writes a skill directory whose SKILL.md holds exactly `front`, so a
// test can write front matter the helper above would not produce.
fn raw_bundle(root string, dir_name string, front string) {
	dir := os.join_path(skills.bundled_root(root), dir_name)
	os.mkdir_all(dir) or {
		panic(err)
	}
	os.write_file(os.join_path_single(dir, skills.entry_file), front) or {
		panic(err)
	}
}

fn test_catalog_of_the_compiler_tree_lists_the_shipped_skills() {
	catalog := skills.catalog(@VEXEROOT)
	assert catalog.len > 0, 'the compiler ships no skills'
	for skill in catalog {
		assert skill.description != '', '${skill.name} has no description'
		assert skill.files.len > 0, '${skill.name} has no files'
		assert skill.files[0] == skills.entry_file, skill.files.join(', ')
	}
	names := catalog.map(it.name)
	assert 'v-mcp' in names, names.join(', ')
}

fn test_list_prints_every_bundled_skill_with_its_files() {
	root := project()
	out := run(root, 'list')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('alpha'), out.text()
	assert out.lines.join('\n').contains('The first skill.'), out.text()
	assert out.lines.join('\n').contains('references/note.md'), out.text()
	assert out.lines.join('\n').contains('not installed'), out.text()
}

fn test_list_reports_where_a_skill_is_installed() {
	root := project()
	run(root, 'add', 'alpha')
	out := run(root, 'list')
	assert out.lines.join('\n').contains('status: project'), out.text()
}

fn test_list_fails_when_there_is_no_catalog() {
	root := os.join_path(test_root, 'empty')
	os.mkdir_all(root)!
	out := run(root, 'list')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('no bundled skills'), out.text()
}

fn test_add_installs_a_skill_into_the_project() {
	root := project()
	out := run(root, 'add', 'alpha')
	assert out.code == 0, out.text()
	assert os.read_file(entry_of(root, 'alpha'))!.contains('# alpha'), 'the entry file was not copied'
	extra := os.join_path(installed_dir(root, 'alpha'), 'references/note.md')
	assert os.is_file(extra), 'a nested file was not copied'
}

fn test_add_reports_a_second_install_instead_of_overwriting() {
	root := project()
	run(root, 'add', 'alpha')
	os.write_file(entry_of(root, 'alpha'), 'edited locally\n')!
	out := run(root, 'add', 'alpha')
	assert out.lines.join('\n').contains('already installed'), out.text()
	assert os.read_file(entry_of(root, 'alpha'))! == 'edited locally\n', 'a plain add overwrote a local edit'
}

fn test_add_force_overwrites_a_local_edit() {
	root := project()
	run(root, 'add', 'alpha')
	os.write_file(entry_of(root, 'alpha'), 'edited locally\n')!
	out := run(root, 'add', 'alpha', '--force')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('reinstalled'), out.text()
	assert os.read_file(entry_of(root, 'alpha'))!.contains('# alpha'), 'a forced add did not overwrite'
}

fn test_add_dry_run_writes_nothing() {
	root := project()
	out := run(root, 'add', 'alpha', '--dry-run')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('would write'), out.text()
	assert !os.is_dir(installed_dir(root, 'alpha')), 'a dry run created the directory'
}

fn test_add_rejects_an_unknown_skill() {
	root := project()
	out := run(root, 'add', 'nope')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('no bundled skill'), out.text()
}

fn test_add_needs_a_name() {
	root := project()
	out := run(root, 'add')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('name a skill'), out.text()
}

fn test_add_rejects_an_unknown_flag() {
	root := project()
	// A typo must not be read as a real install.
	out := run(root, 'add', 'alpha', '--dryrun')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('unknown option'), out.text()
	assert !os.is_dir(installed_dir(root, 'alpha')), 'a bad flag still installed'
}

fn test_add_accepts_several_names_at_once() {
	root := project()
	out := run(root, 'add', 'alpha', 'beta')
	assert out.code == 0, out.text()
	assert os.is_dir(installed_dir(root, 'alpha'))
	assert os.is_dir(installed_dir(root, 'beta'))
}

fn test_path_prints_where_a_skill_lives() {
	root := project()
	out := run(root, 'path', 'alpha')
	assert out.code == 0, out.text()
	assert out.lines.join('\n') == installed_dir(root, 'alpha'), out.text()
}

fn test_path_rejects_an_unknown_skill_and_a_second_name() {
	root := project()
	assert run(root, 'path', 'nope').code != 0
	assert run(root, 'path', 'alpha', 'beta').code != 0
}

fn test_the_global_scope_is_a_different_directory_than_the_project_one() {
	// The two scopes must not collide: a project install is committed, and a
	// global one is not.
	assert skills.target_dir(.project_root, '/tmp/proj') !=
		skills.target_dir(.home_dir, '/tmp/proj')
}

fn test_remove_deletes_the_installed_skill() {
	root := project()
	run(root, 'add', 'alpha')
	out := run(root, 'remove', 'alpha')
	assert out.code == 0, out.text()
	assert !os.is_dir(installed_dir(root, 'alpha')), 'the skill was not removed'
}

fn test_remove_says_when_there_was_nothing_to_remove() {
	root := project()
	out := run(root, 'remove', 'alpha')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('not installed'), out.text()
}

fn test_remove_needs_a_name() {
	root := project()
	out := run(root, 'remove')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('name a skill'), out.text()
}

fn test_remove_dry_run_preserves_the_directory_and_all_contents() {
	root := project()
	assert run(root, 'add', 'alpha').code == 0
	entry := os.read_file(entry_of(root, 'alpha'))!
	reference := os.join_path(installed_dir(root, 'alpha'), 'references', 'note.md')
	before := os.read_file(reference)!
	out := run(root, 'remove', 'alpha', '--dry-run')
	assert out.code == 0, out.text()
	assert out.text().contains('would remove'), out.text()
	assert os.read_file(entry_of(root, 'alpha'))! == entry
	assert os.read_file(reference)! == before
	assert run(root, 'remove', '../../victim', '--dry-run').code == 1
}

fn test_cached_skills_launcher_lists_and_safely_removes_custom_skills() {
	root := project()
	cache := os.join_path(test_root, 'tool-cache')
	previous_cache := os.getenv_opt('VTOOLS_CACHE_DIR')
	previous_dir := os.getwd()
	defer {
		os.chdir(previous_dir) or { panic(err) }
		if value := previous_cache {
			os.setenv('VTOOLS_CACHE_DIR', value, true)
		} else {
			os.unsetenv('VTOOLS_CACHE_DIR')
		}
	}
	os.setenv('VTOOLS_CACHE_DIR', cache, true)
	os.chdir(root)!
	custom := installed_dir(root, 'custom')
	os.mkdir_all(os.join_path(custom, 'references'))!
	os.write_file(os.join_path(custom, 'SKILL.md'), 'custom entry')!
	os.write_file(os.join_path(custom, 'references', 'keep.txt'), 'keep')!
	victim := os.join_path(root, 'victim')
	os.mkdir_all(victim)!
	os.write_file(os.join_path(victim, 'keep.txt'), 'unrelated')!
	vexe := os.quoted_path(@VEXE)
	listed := os.exec([@VEXE, 'skills', 'list'])
	assert listed.exit_code == 0, listed.output
	assert listed.output.contains('v-mcp'), listed.output
	assert os.is_dir(cache)
	preview := os.exec([@VEXE, 'skills', 'remove', 'custom', '--dry-run'])
	assert preview.exit_code == 0, preview.output
	assert preview.output.contains('would remove'), preview.output
	assert os.read_file(os.join_path(custom, 'SKILL.md'))! == 'custom entry'
	assert os.read_file(os.join_path(custom, 'references', 'keep.txt'))! == 'keep'
	for name in ['..', '../../victim'] {
		bad := os.exec([@VEXE, 'skills', 'remove', '${name}'])
		assert bad.exit_code == 1, bad.output
		assert os.read_file(os.join_path(victim, 'keep.txt'))! == 'unrelated'
		assert os.is_dir(custom)
	}
	removed := os.exec([@VEXE, 'skills', 'remove', 'custom'])
	assert removed.exit_code == 0, removed.output
	assert !os.exists(custom)
	assert os.read_file(os.join_path(victim, 'keep.txt'))! == 'unrelated'
}

fn test_list_reports_a_skill_whose_install_is_out_of_date() {
	root := project()
	run(root, 'add', 'alpha')
	os.write_file(entry_of(root, 'alpha'), 'tampered\n')!
	out := run(root, 'list')
	assert out.lines.join('\n').contains('project (out of date)'), out.text()
}

// rebundle rewrites the bundled `SKILL.md` of `name`, so an installation made
// before it is behind its bundle rather than edited.
fn rebundle(root string, name string) {
	os.write_file(os.join_path_single(os.join_path(skills.bundled_root(root), name),
		skills.entry_file),
		'---\nname: ${name}\ndescription: The newer ${name}.\n---\n\n# ${name}\n\nNew body.\n') or {
		panic(err)
	}
}

fn test_update_refreshes_a_skill_whose_bundle_moved_on() {
	root := project()
	run(root, 'add', 'alpha')
	rebundle(root, 'alpha')
	out := run(root, 'update')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('alpha: updated'), out.text()
	assert os.read_file(entry_of(root, 'alpha'))!.contains('New body.')
	// And the record now describes the refreshed copy, so a second run is quiet.
	assert run(root, 'update').lines.join('\n').contains('already matches its bundle')
}

fn test_update_holds_back_a_skill_that_was_edited_here() {
	root := project()
	run(root, 'add', 'alpha')
	os.write_file(entry_of(root, 'alpha'), 'edited here\n')!
	out := run(root, 'update')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('was edited since it was installed'), out.text()
	assert out.errors.join('\n').contains('--force'), out.text()
	// The edit is still there: refusing is the whole point.
	assert os.read_file(entry_of(root, 'alpha'))! == 'edited here\n'
}

fn test_update_dry_run_reports_without_writing() {
	root := project()
	run(root, 'add', 'alpha')
	rebundle(root, 'alpha')
	out := run(root, 'update', '--dry-run')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('would update'), out.text()
	assert !os.read_file(entry_of(root, 'alpha'))!.contains('New body.')
}

fn test_update_force_overwrites_a_local_edit_and_warns_that_it_is_gone() {
	root := project()
	run(root, 'add', 'alpha')
	os.write_file(entry_of(root, 'alpha'), 'edited here\n')!
	out := run(root, 'update', '--force')
	// Doing what `--force` asks for is not a failure, so the exit code is 0 even
	// though the caution is on standard error.
	assert out.code == 0, out.text()
	assert out.errors.join('\n').contains('overwrote local changes in alpha'), out.text()
	assert !os.read_file(entry_of(root, 'alpha'))!.contains('edited here')
	assert os.read_file(entry_of(root, 'alpha'))!.contains('# alpha')
	assert skills.origin_state(root, skills.target_dir(.project_root, root), 'alpha') ==
		.current
}

fn test_update_holds_back_a_skill_with_no_record() {
	root := project()
	run(root, 'add', 'alpha')
	// An installation made before the record existed. Nothing says what was
	// installed, so `update` must not decide on its own to overwrite it.
	os.rm(os.join_path_single(skills.target_dir(.project_root, root), skills.origin_file)) or {
		panic(err)
	}
	out := run(root, 'update')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('has no record of what was installed'), out.text()
	rebundle(root, 'alpha')
	// Still refused without `--force`, even though it is plainly behind.
	assert run(root, 'update').code != 0
	assert run(root, 'update', '--force').code == 0
	assert skills.origin_state(root, skills.target_dir(.project_root, root), 'alpha') ==
		.current
}

fn test_update_can_be_limited_to_one_skill() {
	root := project()
	run(root, 'add', 'alpha')
	run(root, 'add', 'beta')
	rebundle(root, 'alpha')
	rebundle(root, 'beta')
	dir := skills.target_dir(.project_root, root)
	out := run(root, 'update', 'alpha')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('alpha: updated'), out.text()
	assert !out.text().contains('beta'), out.text()
	// beta was not touched, so it still carries the description its bundle had
	// before `rebundle` rewrote it.
	assert os.read_file(entry_of(root, 'beta'))!.contains('The second skill.')
	assert !os.read_file(entry_of(root, 'beta'))!.contains('The newer beta.')
	assert skills.origin_state(root, dir, 'beta') == .stale
}

fn test_update_rejects_a_skill_that_is_not_installed() {
	root := project()
	out := run(root, 'update', 'alpha')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('is not installed'), out.text()
}

fn test_update_needs_something_installed() {
	root := project()
	out := run(root, 'update')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('nothing is installed'), out.text()
}

fn test_update_rejects_an_unknown_flag() {
	root := project()
	run(root, 'add', 'alpha')
	out := run(root, 'update', '--overwrite')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('unknown option'), out.text()
}

fn test_the_usage_documents_update() {
	assert usage.contains('v skills update'), usage
	assert usage.contains('edited here'), usage
}

fn test_an_unknown_subcommand_is_rejected() {
	root := project()
	out := run(root, 'frobnicate')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('unknown subcommand'), out.text()
}

fn test_help_returns_the_usage_without_a_subcommand() {
	root := project()
	out := run(root, '--help')
	assert out.code == 0, out.text()
	assert out.lines.join('\n').contains('v skills add'), out.text()
}

fn test_list_reports_a_bundle_that_cannot_be_installed() {
	// `catalog` skips a bundle it cannot read, so without this the broken bundle
	// would simply not appear and nobody would learn why.
	root := project()
	raw_bundle(root, 'mismatched', '---\nname: other-name\ndescription: Broken.\n---\n\nBody.\n')
	out := run(root, 'list')
	assert out.code != 0, 'a catalog that cannot be fully installed must not read as clean'
	errors := out.errors.join('\n')
	assert errors.contains('mismatched'), errors
	// The message must name both sides of the disagreement, or it is not actionable.
	assert errors.contains('other-name'), 'the declared name should be named: ' + errors
	assert errors.contains('does not match the directory name'), errors
}

fn test_add_refuses_a_bundle_that_fails_validation() {
	root := project()
	raw_bundle(root, 'mismatched', '---\nname: other-name\ndescription: Broken.\n---\n\nBody.\n')
	out := run(root, 'add', 'mismatched')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('cannot be installed'), out.text()
	assert !os.is_dir(installed_dir(root, 'mismatched')), 'nothing may be written'
	// The valid bundles in the same catalog still install.
	assert run(root, 'add', 'alpha').code == 0
}

fn test_list_does_not_complain_about_the_shipped_bundles() {
	// Every bundle this compiler ships has to pass the rules it enforces.
	out := run(@VEXEROOT, 'list')
	assert out.errors.len == 0, out.errors.join('\n')
}

fn test_update_holds_back_an_unreadable_local_edit() {
	$if windows {
		return
	}
	$if !windows {
		if os.getuid() == 0 { return }
	}
	root := project()
	source := os.join_path(skills.bundled_root(root), 'alpha', 'references', 'note.md')
	os.write_file(source, '')!
	assert run(root, 'add', 'alpha').code == 0
	installed := os.join_path(skills.target_dir(.project_root, root), 'alpha', 'references', 'note.md')
	os.write_file(installed, 'local work')!
	os.chmod(installed, 0o000)!
	defer { os.chmod(installed, 0o600) or {} }
	rebundle(root, 'alpha')
	out := run(root, 'update')
	assert out.code != 0, out.text()
	assert out.errors.join('\n').contains('not updated'), out.text()
	os.chmod(installed, 0o600)!
	assert os.read_file(installed)! == 'local work'
}
