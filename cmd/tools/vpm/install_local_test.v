module main

import os
import rand
import v.vmod
import test_utils { cmd_fail, cmd_ok }

const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_local_test_${rand.ulid()}')

struct LocalInstallCase {
	args        string
	module_name string
	workdir     string
}

fn testsuite_begin() {
	test_utils.set_test_env(test_path)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_install_from_local_git_repository_variants() {
	repo_path := os.join_path(test_path, 'local_repo')
	repo_dot_git_path := os.join_path(test_path, 'local_repo.git')
	create_local_git_module(repo_path, 'local_repo_pkg')
	create_local_git_module(repo_dot_git_path, 'local_repo_dot_git_pkg')

	cases := [
		LocalInstallCase{
			args:        os.quoted_path(repo_path)
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        file_url(repo_path)
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        '--git ${os.quoted_path(repo_path)}'
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        '--git ${file_url(repo_path)}'
			module_name: 'local_repo_pkg'
		},
		LocalInstallCase{
			args:        os.file_name(repo_path)
			module_name: 'local_repo_pkg'
			workdir:     test_path
		},
		LocalInstallCase{
			args:        os.quoted_path(repo_dot_git_path)
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        file_url(repo_dot_git_path)
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        '--git ${os.quoted_path(repo_dot_git_path)}'
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        '--git ${file_url(repo_dot_git_path)}'
			module_name: 'local_repo_dot_git_pkg'
		},
		LocalInstallCase{
			args:        os.file_name(repo_dot_git_path)
			module_name: 'local_repo_dot_git_pkg'
			workdir:     test_path
		},
	]
	for i, c in cases {
		vmodules_path := os.join_path(test_path, 'vmodules_case_${i}')
		test_utils.set_test_env(vmodules_path)
		cmd := '${vexe} install ${c.args}'
		old_dir := os.getwd()
		if c.workdir != '' {
			os.chdir(c.workdir) or { panic(err) }
		}
		res := cmd_ok(@LOCATION, cmd)
		if c.workdir != '' {
			os.chdir(old_dir) or {}
		}
		assert res.output.contains('Installed `${c.module_name}`'), res.output
		manifest := vmod.from_file(os.join_path(vmodules_path, c.module_name, 'v.mod')) or {
			panic('Failed to parse v.mod for `${c.module_name}`. ${err}')
		}
		assert manifest.name == c.module_name
	}
}

// Regression test for https://github.com/vlang/v/issues/27192.
// VPM-registered installs lowercase the on-disk path via `normalize_mod_path`,
// so `v update <Ident>` and `v remove <Ident>` must apply the same
// normalization when looking up existing modules — otherwise users with
// capitalized publisher names (e.g. `Frothy7650.chalk`) cannot update or
// remove the modules they just installed.
fn test_update_and_remove_with_capitalized_ident() {
	vmodules_path := os.join_path(test_path, 'vmodules_capitalized')
	test_utils.set_test_env(vmodules_path)
	// Simulate the post-install state of `v install Frothy7650.chalk`:
	// a real VPM install places the module under the lowercased publisher dir.
	publisher_dir := os.join_path(vmodules_path, 'frothy7650')
	installed_path := os.join_path(publisher_dir, 'chalk')
	os.mkdir_all(installed_path) or { panic(err) }
	os.write_file(os.join_path(installed_path, 'v.mod'),
		"Module{\n\tname: 'Frothy7650.chalk'\n\tversion: '0.0.1'\n}\n") or { panic(err) }
	// Remove with the original (capitalized) ident must succeed and clean up the author dir.
	res := cmd_ok(@LOCATION, '${vexe} remove Frothy7650.chalk')
	assert !res.output.contains('failed to find'), res.output
	assert !os.exists(installed_path)
	assert !os.exists(publisher_dir)
}

// A module named e.g. `my-mod` is installed as `my_mod`, since `-` is not valid
// in a V import path. Make sure the mismatch is reported with the resulting
// import path, instead of leaving users to guess it.
fn test_install_warns_about_normalized_module_name() {
	vmodules_path := os.join_path(test_path, 'vmodules_normalized_name')
	test_utils.set_test_env(vmodules_path)
	repo_path := os.join_path(test_path, 'hyphenated_repo')
	create_local_git_module(repo_path, 'my-mod')

	res := cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
	assert res.output.contains('`my-mod` is not a valid V import path, it was installed as `my_mod`.'), res.output
	assert res.output.contains('Use `my_mod` as the normalized import prefix'), res.output
	assert os.exists(os.join_path(vmodules_path, 'my_mod', 'v.mod'))
}

fn test_install_maps_manifest_dots_to_import_directories() {
	vmodules_path := os.join_path(test_path, 'vmodules_dotted_name')
	test_utils.set_test_env(vmodules_path)
	repo_path := os.join_path(test_path, 'dotted_repo')
	create_local_git_module(repo_path, 'Foo.bar.baz')

	res := cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
	assert res.output.contains('`Foo.bar.baz` is not a valid V import path, it was installed as `foo.bar.baz`.'), res.output

	assert res.output.contains('Use `foo.bar.baz` as the normalized import prefix'), res.output
	assert os.exists(os.join_path(vmodules_path, 'foo', 'bar', 'baz', 'v.mod'))
	assert 'foo.bar.baz' in get_installed_modules_in(vmodules_path)
}

fn test_install_warns_when_repeated_dots_are_collapsed() {
	vmodules_path := os.join_path(test_path, 'vmodules_repeated_dots')
	test_utils.set_test_env(vmodules_path)
	repo_path := os.join_path(test_path, 'repeated_dots_repo')
	create_local_git_module(repo_path, 'foo..bar')

	res := cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
	assert res.output.contains('`foo..bar` is not a valid V import path, it was installed as `foo.bar`.'), res.output

	assert os.exists(os.join_path(vmodules_path, 'foo', 'bar', 'v.mod'))
}

fn test_install_path_containment_supports_filesystem_roots() {
	mut filesystem_root := os.real_path(os.getwd())
	for os.dir(filesystem_root) != filesystem_root {
		filesystem_root = os.dir(filesystem_root)
	}
	assert install_path_is_in_vmodules(os.join_path(filesystem_root, 'vpm_test_module'),
		filesystem_root)
	assert !install_path_is_in_vmodules(filesystem_root, filesystem_root)
}

fn test_dotted_install_does_not_nest_inside_existing_module() {
	vmodules_path := os.join_path(test_path, 'vmodules_nested_module')
	test_utils.set_test_env(vmodules_path)
	create_local_git_module(os.join_path(vmodules_path, 'foo'), 'foo')
	repo_path := os.join_path(test_path, 'nested_dotted_repo')
	create_local_git_module(repo_path, 'foo.bar')

	res := cmd_fail(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
	assert res.output.contains('refusing to install `foo.bar` inside existing module'), res.output
	assert !os.exists(os.join_path(vmodules_path, 'foo', 'bar'))
}

fn test_dotted_install_does_not_nest_inside_git_worktree() {
	vmodules_path := os.join_path(test_path, 'vmodules_worktree_ancestor')
	test_utils.set_test_env(vmodules_path)
	source_repo_path := os.join_path(test_path, 'worktree_ancestor_source')
	create_local_git_module(source_repo_path, 'foo')
	worktree_path := os.join_path(vmodules_path, 'foo')
	os.mkdir_all(vmodules_path) or { panic(err) }
	cmd_ok(@LOCATION,
		'git -C ${os.quoted_path(source_repo_path)} worktree add -b vpm-test ${os.quoted_path(worktree_path)}')
	assert os.is_file(os.join_path(worktree_path, '.git'))
	nested_repo_path := os.join_path(test_path, 'worktree_ancestor_nested')
	create_local_git_module(nested_repo_path, 'foo.bar')

	res := cmd_fail(@LOCATION, '${vexe} install ${os.quoted_path(nested_repo_path)}')
	assert res.output.contains('refusing to install `foo.bar` inside existing module'), res.output
	assert !os.exists(os.join_path(worktree_path, 'bar'))
}

fn test_root_install_does_not_replace_existing_module_namespace() {
	vmodules_path := os.join_path(test_path, 'vmodules_existing_namespace')
	test_utils.set_test_env(vmodules_path)
	nested_repo_path := os.join_path(test_path, 'existing_namespace_nested_repo')
	create_local_git_module(nested_repo_path, 'foo.bar')
	cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(nested_repo_path)}')
	root_repo_path := os.join_path(test_path, 'existing_namespace_root_repo')
	create_local_git_module(root_repo_path, 'foo')

	res := cmd_fail(@LOCATION, '${vexe} install ${os.quoted_path(root_repo_path)}')
	assert res.output.contains('refusing to install `foo`: destination'), res.output
	assert os.exists(os.join_path(vmodules_path, 'foo', 'bar', 'v.mod'))
	entries := os.ls(os.join_path(vmodules_path, 'foo')) or { panic(err) }
	assert entries == ['bar']
}

fn test_direct_install_rejects_different_repository_at_same_path() {
	vmodules_path := os.join_path(test_path, 'vmodules_repository_collision')
	test_utils.set_test_env(vmodules_path)
	first_repo_path := os.join_path(test_path, 'repository_collision_first')
	second_repo_path := os.join_path(test_path, 'repository_collision_second')
	create_local_git_module(first_repo_path, 'foo.bar')
	create_local_git_module(second_repo_path, 'foo.bar')
	cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(first_repo_path)}')
	installed_path := os.join_path(vmodules_path, 'foo', 'bar')
	mut registered_mod := Module{
		name:         'foo.bar'
		url:          second_repo_path
		install_path: installed_path
		vcs:          VCS.git
	}
	registered_mod.get_installed()
	assert !registered_mod.is_installed

	res := cmd_fail(@LOCATION, '${vexe} install ${os.quoted_path(second_repo_path)}')
	assert res.output.contains('refusing to install `foo.bar`: destination'), res.output
	remote :=
		cmd_ok(@LOCATION, 'git -C ${os.quoted_path(installed_path)} remote get-url origin').output.trim_space()
	assert os.real_path(remote) == os.real_path(first_repo_path)
}

fn test_clone_source_identity_preserves_repository_path_case_and_port() {
	base := normalized_clone_source('https://example.com/Owner/Repo.git')
	assert base == normalized_clone_source('git@example.com:Owner/Repo.git')
	assert base != normalized_clone_source('https://example.com/owner/repo.git')
	assert base != normalized_clone_source('https://example.com:8443/Owner/Repo.git')
	github := normalized_clone_source('https://github.com/Owner/Repo.git')
	assert github == normalized_clone_source('git@github.com:owner/repo.git')
}

fn test_dotted_install_does_not_follow_linked_namespace() {
	$if !windows {
		vmodules_path := os.join_path(test_path, 'vmodules_linked_namespace')
		test_utils.set_test_env(vmodules_path)
		os.mkdir_all(vmodules_path) or { panic(err) }
		linked_repo_path := os.join_path(test_path, 'linked_namespace_repo')
		create_local_git_module(linked_repo_path, 'foo')
		os.symlink(linked_repo_path, os.join_path(vmodules_path, 'foo')) or { panic(err) }
		nested_repo_path := os.join_path(test_path, 'linked_namespace_nested_repo')
		create_local_git_module(nested_repo_path, 'foo.bar')

		res := cmd_fail(@LOCATION, '${vexe} install ${os.quoted_path(nested_repo_path)}')
		assert res.output.contains('refusing to install `foo.bar` outside the V modules directory'), res.output

		assert !os.exists(os.join_path(linked_repo_path, 'bar'))
	}
}

fn test_dotted_install_rejects_linked_namespace_inside_vmodules() {
	$if !windows {
		vmodules_path := os.join_path(test_path, 'vmodules_internal_linked_namespace')
		test_utils.set_test_env(vmodules_path)
		real_namespace := os.join_path(vmodules_path, 'real_namespace')
		os.mkdir_all(real_namespace) or { panic(err) }
		os.symlink(real_namespace, os.join_path(vmodules_path, 'foo')) or { panic(err) }
		repo_path := os.join_path(test_path, 'internal_linked_namespace_repo')
		create_local_git_module(repo_path, 'foo.bar')

		res := cmd_fail(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
		assert res.output.contains('refusing to install `foo.bar` inside a symlinked module namespace'), res.output

		assert !os.exists(os.join_path(real_namespace, 'bar'))
	}
}

fn test_remove_prunes_deep_empty_module_namespaces() {
	vmodules_path := os.join_path(test_path, 'vmodules_remove_namespaces')
	test_utils.set_test_env(vmodules_path)
	repo_path := os.join_path(test_path, 'remove_namespaces_repo')
	create_local_git_module(repo_path, 'foo.bar.baz')
	cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')

	cmd_ok(@LOCATION, '${vexe} remove foo.bar.baz')
	assert !os.exists(os.join_path(vmodules_path, 'foo'))
}

fn test_installed_module_discovery_preserves_vcs_links() {
	$if !windows {
		vmodules_path := os.join_path(test_path, 'vmodules_linked_module')
		repo_path := os.join_path(test_path, 'linked_module_repo')
		create_local_git_module(repo_path, 'author.linked')
		publisher_path := os.join_path(vmodules_path, 'author')
		os.mkdir_all(publisher_path) or { panic(err) }
		os.symlink(repo_path, os.join_path(publisher_path, 'linked')) or { panic(err) }
		assert 'author.linked' in get_installed_modules_in(vmodules_path)
	}
}

fn test_installed_module_discovery_follows_linked_namespaces_without_cycles() {
	$if !windows {
		vmodules_path := os.join_path(test_path, 'vmodules_linked_discovery_namespace')
		namespace_path := os.join_path(test_path, 'linked_discovery_namespace')
		create_local_git_module(os.join_path(namespace_path, 'pkg'), 'author.pkg')
		os.symlink(namespace_path, os.join_path(namespace_path, 'cycle')) or { panic(err) }
		os.mkdir_all(vmodules_path) or { panic(err) }
		os.symlink(namespace_path, os.join_path(vmodules_path, 'author')) or { panic(err) }

		modules := get_installed_modules_in(vmodules_path)
		assert 'author.pkg' in modules
		assert modules.len == 1
	}
}

fn test_installed_module_discovery_ignores_unrelated_vcs_directories() {
	vmodules_path := os.join_path(test_path, 'vmodules_unrelated_repository')
	unrelated_path := os.join_path(vmodules_path, 'cache', 'unrelated')
	os.mkdir_all(unrelated_path) or { panic(err) }
	cmd_ok(@LOCATION, 'git init ${os.quoted_path(unrelated_path)}')
	cmd_ok(@LOCATION,
		'git -C ${os.quoted_path(unrelated_path)} remote add origin https://github.com/other/repository')
	assert 'cache.unrelated' !in get_installed_modules_in(vmodules_path)
}

fn test_installed_module_discovery_preserves_manifestless_registered_checkout() {
	vmodules_path := os.join_path(test_path, 'vmodules_manifestless_registered')
	module_path := os.join_path(vmodules_path, 'spytheman', 'regex')
	os.mkdir_all(module_path) or { panic(err) }
	cmd_ok(@LOCATION, 'git init ${os.quoted_path(module_path)}')
	cmd_ok(@LOCATION,
		'git -C ${os.quoted_path(module_path)} remote add origin https://github.com/spytheman/v-regex')
	assert 'spytheman.regex' in get_installed_modules_in(vmodules_path)
}

// A publisher directory added for a direct HTTP install is intentional and does not mean that
// the manifest name itself was normalized.
fn test_publisher_prefix_does_not_look_like_name_normalization() {
	m := Module{
		name:         'hashmap'
		install_path: os.join_path(settings.vmodules_path, 'wertzui123', 'hashmap')
	}
	assert !m.name_was_normalized()
	assert Module{
		name: 'my-mod'
	}.name_was_normalized()
	registered := Module{
		name:     'IsaiahPatton.iui'
		manifest: vmod.Manifest{
			name: 'iui'
		}
	}
	assert !registered.normalized_name_warning_details('isaiahpatton.iui').contains('Consider renaming')
	assert direct_install_mod_path('publisher', 'foo.bar') == os.join_path('publisher', 'foo',
		'bar')
	assert direct_install_mod_path('acme.inc', 'my-mod') == os.join_path('acme', 'inc', 'my_mod')
}

fn test_url_lookup_preserves_legacy_dotted_layout() {
	vmodules_path := os.join_path(test_path, 'vmodules_legacy_url_layout')
	legacy_path := os.join_path(vmodules_path, 'publisher', 'foo.bar')
	os.mkdir_all(legacy_path) or { panic(err) }
	found := get_path_of_existing_url_module(vmodules_path, 'publisher', 'foo.bar') or {
		panic(err)
	}
	assert found == os.real_path(legacy_path)
}

fn test_import_path_canonicalizes_the_installed_leaf() {
	$if !windows {
		real_vmodules := os.join_path(test_path, 'canonical_vmodules')
		linked_vmodules := os.join_path(test_path, 'linked_vmodules')
		installed_path := os.join_path(real_vmodules, 'my_mod')
		os.mkdir_all(installed_path) or { panic(err) }
		os.symlink(real_vmodules, linked_vmodules) or { panic(err) }
		assert import_path_relative_to(os.join_path(linked_vmodules, 'my_mod'), linked_vmodules) == 'my_mod'
	}
}

// Counterpart of the test above: a module name that is already a valid import
// path must not trigger the warning.
fn test_install_does_not_warn_about_valid_module_name() {
	vmodules_path := os.join_path(test_path, 'vmodules_valid_name')
	test_utils.set_test_env(vmodules_path)
	repo_path := os.join_path(test_path, 'valid_name_repo')
	create_local_git_module(repo_path, 'my_mod')

	res := cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
	assert !res.output.contains('is not a valid V import path'), res.output
}

// `v install --local` installs into the project's own module lookup root, the
// folder holding its `v.mod`, so that root is now shared with the modules the
// project writes by hand. Version control does not tell those apart -- a project
// may well keep a module of its own as a submodule or a manual clone -- so a
// destructive command may only touch what VPM recorded installing there.
fn test_local_remove_refuses_a_module_vpm_did_not_install() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_remove'))
	project_dir := os.join_path(test_path, 'local_remove_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'local_remove_project'\n}\n") or {
		panic(err)
	}
	// A module the project wrote itself, and one it vendored as a checkout of its
	// own, the way a submodule or a manual clone sits in a project.
	handwritten := os.join_path(project_dir, 'mymod')
	os.mkdir_all(handwritten) or { panic(err) }
	os.write_file(os.join_path(handwritten, 'mymod.v'), 'module mymod\n') or { panic(err) }
	vendored := os.join_path(project_dir, 'vendored')
	create_local_git_module(vendored, 'vendored')

	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	plain := cmd_fail(@LOCATION, '${vexe} remove --local mymod')
	assert plain.output.contains('refusing to remove `mymod`'), plain.output
	assert os.is_file(os.join_path(handwritten, 'mymod.v'))

	checkout := cmd_fail(@LOCATION, '${vexe} remove --local vendored')
	assert checkout.output.contains('refusing to remove `vendored`'), checkout.output
	assert os.is_file(os.join_path(vendored, 'v.mod'))
	assert os.is_dir(os.join_path(vendored, '.git'))
}

// What VPM installed into that shared root, VPM can still take back, and the
// record of the install goes with the directory.
fn test_local_remove_deletes_what_vpm_installed() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_remove_installed'))
	project_dir := os.join_path(test_path, 'local_remove_installed_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'local_remove_installed'\n}\n") or {
		panic(err)
	}
	// The post-install state of `v install --local <repo>`: the package cloned
	// into the lookup root, and VPM's record that it put it there.
	installed := os.join_path(project_dir, 'local_pkg')
	create_local_git_module(installed, 'local_pkg')
	record_local_install(installed) or { panic(err) }
	assert is_recorded_local_install(installed)

	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok(@LOCATION, '${vexe} remove --local local_pkg')
	assert !os.exists(installed)
	assert os.is_file(os.join_path(project_dir, 'v.mod'))
	assert !is_recorded_local_install(installed)
}

// The record stands for one checkout only, so it can never be read as provenance
// for another one, and the note of it lives outside the project it describes.
fn test_local_install_records_are_per_checkout() {
	vmodules_path := os.join_path(test_path, 'vmodules_local_records')
	test_utils.set_test_env(vmodules_path)
	project_dir := os.join_path(test_path, 'local_records_project')
	installed := os.join_path(project_dir, 'recorded_pkg')
	sibling := os.join_path(project_dir, 'unrecorded_pkg')
	os.mkdir_all(installed) or { panic(err) }
	os.mkdir_all(sibling) or { panic(err) }

	record_local_install(installed) or { panic(err) }
	assert is_recorded_local_install(installed)
	assert !is_recorded_local_install(sibling)
	// The notes are kept where the records directory is configured to be, and that
	// is never inside the project they describe.
	records_dir := local_install_records_dir()
	assert os.is_dir(records_dir)
	assert !records_dir.starts_with(project_dir)
	entries := os.ls(project_dir) or { panic(err) }
	assert entries.sorted() == ['recorded_pkg', 'unrecorded_pkg']

	token := read_local_install_token(installed) or { panic('no token was written') }
	forget_local_install(token)
	assert !is_recorded_local_install(installed)
}

// An install VPM cannot record is one it could never update or remove again, so
// recording has to fail loudly enough for the installation to be undone.
fn test_recording_a_local_install_reports_failure() {
	$if !windows {
		test_utils.set_test_env(os.join_path(test_path, 'vmodules_unwritable_records'))
		// A records directory that cannot be created, because what would hold it
		// is read-only.
		blocked_root := os.join_path(test_path, 'unwritable_records_root')
		os.mkdir_all(blocked_root) or { panic(err) }
		saved_records := os.getenv(local_installs_dir_env)
		os.setenv(local_installs_dir_env, os.join_path(blocked_root, 'records'), true)
		os.chmod(blocked_root, 0o500) or { panic(err) }
		defer {
			os.chmod(blocked_root, 0o700) or {}
			os.setenv(local_installs_dir_env, saved_records, true)
		}
		installed := os.join_path(test_path, 'unrecordable_project', 'pkg')
		os.mkdir_all(installed) or { panic(err) }

		record_local_install(installed) or {
			assert !is_recorded_local_install(installed)
			return
		}
		assert false, 'recording into an unwritable records directory has to fail'
	}
}

// A project can be renamed or moved. Its installs travel with it, answering with
// the token they were given, so the record follows them to the new path.
fn test_a_moved_project_keeps_its_local_installs() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_moved'))
	project_dir := os.join_path(test_path, 'local_moved_project')
	moved_dir := os.join_path(test_path, 'local_moved_project_renamed')
	os.rmdir_all(moved_dir) or {}
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'local_moved'\n}\n") or {
		panic(err)
	}
	installed := os.join_path(project_dir, 'moved_pkg')
	create_local_git_module(installed, 'moved_pkg')
	record_local_install(installed) or { panic(err) }

	os.mv(project_dir, moved_dir) or { panic(err) }
	relocated := os.join_path(moved_dir, 'moved_pkg')
	assert is_recorded_local_install(relocated)
	// Discovery finds it at the new path too, without walking the project: the
	// install kept its place inside the project that moved around it.
	assert local_installed_modules(moved_dir) == ['moved_pkg']
	// What it wrote down about the move is in the one convention records use, so
	// the next pass reads back the same checkout rather than a stranger.
	token := read_local_install_token(relocated) or { panic('no token after the move') }
	note := os.read_file(local_install_record_path(token)) or { panic(err) }
	assert note.trim_space() == canonical_install_path(relocated)
	assert !note.contains('\\'), note
	assert local_installed_modules(moved_dir) == ['moved_pkg']

	old_dir := os.getwd()
	os.chdir(moved_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok(@LOCATION, '${vexe} remove --local moved_pkg')
	assert !os.exists(relocated)
}

// A project renamed in case alone is the same project. On a case-insensitive
// filesystem the old spelling still leads to the same checkout, which answers
// with the same token, and reading that as a copy of itself would leave the
// install unmanageable until something else happened to rewrite its record.
fn test_a_case_only_project_rename_keeps_its_local_installs() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_case_rename'))
	project_dir := os.join_path(test_path, 'case_rename_project')
	renamed_dir := os.join_path(test_path, 'CASE_RENAME_PROJECT')
	os.rmdir_all(renamed_dir) or {}
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'case_rename'\n}\n") or {
		panic(err)
	}
	installed := os.join_path(project_dir, 'case_pkg')
	create_local_git_module(installed, 'case_pkg')
	record_local_install(installed) or { panic(err) }

	// Straight to the other spelling is a move onto itself on a case-insensitive
	// filesystem, so go through a name that is nobody's.
	staging_dir := os.join_path(test_path, 'case_rename_staging')
	os.rmdir_all(staging_dir) or {}
	os.mv(project_dir, staging_dir) or { panic(err) }
	os.mv(staging_dir, renamed_dir) or { panic(err) }
	relocated := os.join_path(renamed_dir, 'case_pkg')
	assert os.is_dir(relocated)
	assert is_recorded_local_install(relocated)
	assert local_installed_modules(renamed_dir) == ['case_pkg']

	old_dir := os.getwd()
	os.chdir(renamed_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	cmd_ok(@LOCATION, '${vexe} remove --local case_pkg')
	assert !os.exists(relocated)
}

// Copying a whole project copies the token in every install it holds, so a copy
// answers exactly as the original does. Discovery has to settle that the same way
// a command asking about one directory does: while the original is there, the
// copy is a copy, and listing it must not quietly hand it the record.
fn test_discovery_in_a_copied_project_does_not_take_the_record() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_copied_project'))
	project_dir := os.join_path(test_path, 'copied_project_original')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'copied_project'\n}\n") or {
		panic(err)
	}
	installed := os.join_path(project_dir, 'copied_project_pkg')
	create_local_git_module(installed, 'copied_project_pkg')
	record_local_install(installed) or { panic(err) }

	// The copy of the project, holding a copy of the checkout: same layout, same
	// token, no claim on the record.
	copy_dir := os.join_path(test_path, 'copied_project_copy')
	os.rmdir_all(copy_dir) or {}
	os.mkdir_all(copy_dir) or { panic(err) }
	os.write_file(os.join_path(copy_dir, 'v.mod'), "Module{\n\tname: 'copied_project'\n}\n") or {
		panic(err)
	}
	copied := os.join_path(copy_dir, 'copied_project_pkg')
	create_local_git_module(copied, 'copied_project_pkg')
	token := read_local_install_token(installed) or { panic('the install has no token') }
	os.write_file(local_install_token_path(copied), token) or { panic(err) }
	assert read_local_install_token(copied) or { '' } == token

	assert local_installed_modules(copy_dir) == []
	assert !is_recorded_local_install(copied)
	// The original is untouched by having been looked at from the copy.
	assert local_installed_modules(project_dir) == ['copied_project_pkg']
	assert is_recorded_local_install(installed)

	old_dir := os.getwd()
	os.chdir(copy_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	res := cmd_fail(@LOCATION, '${vexe} remove --local copied_project_pkg')
	assert res.output.contains('refusing to remove `copied_project_pkg`'), res.output
	assert os.is_dir(installed)
}

// A local install goes to the project whatever `VMODULES` says, so the record of
// it cannot live under the module store: installing with a one-off `VMODULES` and
// managing it without would otherwise lose the package.
fn test_local_install_records_do_not_follow_vmodules() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_records_location'))
	saved_records := os.getenv(local_installs_dir_env)
	saved_cache := os.getenv('XDG_CACHE_HOME')
	os.unsetenv(local_installs_dir_env)
	os.setenv('XDG_CACHE_HOME', os.join_path(test_path, 'records_location_cache'), true)
	defer {
		os.setenv(local_installs_dir_env, saved_records, true)
		if saved_cache == '' {
			os.unsetenv('XDG_CACHE_HOME')
		} else {
			os.setenv('XDG_CACHE_HOME', saved_cache, true)
		}
	}
	first_store := os.join_path(test_path, 'records_store_one')
	second_store := os.join_path(test_path, 'records_store_two')

	os.setenv('VMODULES', first_store, true)
	with_first := local_install_records_dir()
	os.setenv('VMODULES', second_store, true)
	with_second := local_install_records_dir()

	assert with_first == with_second
	assert !with_first.starts_with(first_store)
	assert !with_first.starts_with(second_store)
}

// A copy of an install is not the install: while the original is still there to
// answer with the same token, the duplicate inherits nothing from it.
fn test_a_copy_of_a_local_install_is_not_one() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_copied'))
	project_dir := os.join_path(test_path, 'local_copied_project')
	os.mkdir_all(project_dir) or { panic(err) }
	installed := os.join_path(project_dir, 'copied_pkg')
	create_local_git_module(installed, 'copied_pkg')
	record_local_install(installed) or { panic(err) }

	elsewhere := os.join_path(test_path, 'local_copy_target')
	os.rmdir_all(elsewhere) or {}
	os.mkdir_all(elsewhere) or { panic(err) }
	os.cp_all(installed, os.join_path(elsewhere, 'copied_pkg'), true) or { panic(err) }

	assert !is_recorded_local_install(os.join_path(elsewhere, 'copied_pkg'))
	assert is_recorded_local_install(installed)
}

// A removal that failed has to leave a retry possible: the record only goes once
// the directory is really gone.
fn test_a_failed_removal_keeps_the_local_install_record() {
	$if !windows {
		test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_failed_removal'))
		project_dir := os.join_path(test_path, 'local_failed_removal_project')
		installed := os.join_path(project_dir, 'locked_pkg')
		create_local_git_module(installed, 'locked_pkg')
		record_local_install(installed) or { panic(err) }
		// A read-only parent is a directory whose entries cannot be unlinked.
		os.chmod(project_dir, 0o500) or { panic(err) }
		defer {
			os.chmod(project_dir, 0o700) or {}
		}

		remove_installed_dir(installed) or {
			assert os.is_dir(installed)
			assert is_recorded_local_install(installed)
			return
		}
		assert false, 'removing from a read-only directory has to fail'
	}
}

// Local discovery reads the records, so what it costs is the handful of installs
// they name, not a walk of the project: a checkout buried in `node_modules` or a
// module the project vendored itself is never even looked at, let alone listed.
fn test_local_discovery_lists_the_records_not_the_project() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_discovery'))
	project_dir := os.join_path(test_path, 'local_discovery_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'local_discovery'\n}\n") or {
		panic(err)
	}
	installed := os.join_path(project_dir, 'plain_pkg')
	create_local_git_module(installed, 'plain_pkg')
	record_local_install(installed) or { panic(err) }
	dotted := os.join_path(project_dir, 'dotted', 'pkg')
	create_local_git_module(dotted, 'dotted.pkg')
	record_local_install(dotted) or { panic(err) }

	// Things a walk of the project would have had to visit and weed out.
	vendored := os.join_path(project_dir, 'vendored')
	create_local_git_module(vendored, 'vendored')
	buried := os.join_path(project_dir, 'node_modules', 'dep', 'checkout')
	create_local_git_module(buried, 'checkout')

	assert local_installed_modules(project_dir) == ['dotted.pkg', 'plain_pkg']
}

// A record stands for the checkout VPM made, not for the path it sits at. When
// that install is deleted or moved by hand and the project puts a module of its
// own there, the leftover record may not hand its authority to the replacement.
fn test_a_reused_path_does_not_inherit_a_local_install() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_reused_path'))
	project_dir := os.join_path(test_path, 'local_reused_path_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'local_reused_path'\n}\n") or {
		panic(err)
	}
	installed := os.join_path(project_dir, 'reused_pkg')
	create_local_git_module(installed, 'reused_pkg')
	record_local_install(installed) or { panic(err) }
	assert is_recorded_local_install(installed)

	// The install is gone, and the project writes its own module at that path.
	os.rmdir_all(installed) or { panic(err) }
	os.mkdir_all(installed) or { panic(err) }
	os.write_file(os.join_path(installed, 'reused_pkg.v'), 'module reused_pkg\n') or { panic(err) }
	assert !is_recorded_local_install(installed)

	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	res := cmd_fail(@LOCATION, '${vexe} remove --local reused_pkg')
	assert res.output.contains('refusing to remove `reused_pkg`'), res.output
	assert os.is_file(os.join_path(installed, 'reused_pkg.v'))
}

// Listing and updating go by the same record. A module the project vendored
// itself is a checkout like any other, and `git pull` in it would rewrite source
// VPM never installed, so it is not discovered and not updated by name either.
fn test_local_list_and_update_skip_modules_vpm_did_not_install() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_local_update'))
	project_dir := os.join_path(test_path, 'local_update_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'local_update_project'\n}\n") or {
		panic(err)
	}
	vendored := os.join_path(project_dir, 'vendored')
	create_local_git_module(vendored, 'vendored')

	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	listed := cmd_ok(@LOCATION, '${vexe} list --local')
	assert !listed.output.contains('vendored'), listed.output

	updated := cmd_ok(@LOCATION, '${vexe} update --local')
	assert !updated.output.contains('vendored'), updated.output

	by_name := cmd_fail(@LOCATION, '${vexe} update --local vendored')
	assert by_name.output.contains('refusing to update `vendored`'), by_name.output
	assert os.is_file(os.join_path(vendored, 'v.mod'))
}

// A package an older V installed has no record: it went into the project's
// `modules/` directory, and moving it up beside the v.mod says nothing about
// where it came from. Adoption is what the user says by name, and what VPM
// records; from then on the package is managed like any other install.
fn test_adopting_a_legacy_local_install_makes_it_managed() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_adopt'))
	project_dir := os.join_path(test_path, 'adopt_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'adopt_project'\n}\n") or {
		panic(err)
	}
	// What an older `v install --local` left behind, after the move the compiler
	// asks for: a checkout in the lookup root that nothing records.
	legacy := os.join_path(project_dir, 'legacy_pkg')
	create_local_git_module(legacy, 'legacy_pkg')

	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	refused := cmd_fail(@LOCATION, '${vexe} remove --local legacy_pkg')
	assert refused.output.contains('refusing to remove `legacy_pkg`'), refused.output
	assert os.is_dir(legacy)

	adopted := cmd_ok(@LOCATION, '${vexe} install --local --adopt legacy_pkg')
	assert adopted.output.contains('Adopted `legacy_pkg`'), adopted.output
	assert is_recorded_local_install(legacy)

	listed := cmd_ok(@LOCATION, '${vexe} list --local')
	assert listed.output.contains('legacy_pkg'), listed.output
	cmd_ok(@LOCATION, '${vexe} remove --local legacy_pkg')
	assert !os.exists(legacy)
}

// A name in the lookup root that leads out of the project is not the project's,
// whatever it points at. Adopting it would hand VPM a checkout it has no business
// touching, and `v remove --local` resolves the link before deleting: it would
// take the checkout rather than the link.
fn test_a_symlink_out_of_the_project_is_neither_adopted_nor_removed() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_symlinked_adopt'))
	project_dir := os.join_path(test_path, 'symlinked_adopt_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'symlinked_adopt'\n}\n") or {
		panic(err)
	}
	external := os.join_path(test_path, 'external_checkout')
	create_local_git_module(external, 'external_pkg')
	link_path := os.join_path(project_dir, 'external_pkg')
	os.symlink(external, link_path) or {
		eprintln('Skipping symlink test due to missing privileges: ${err}')
		return
	}

	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	adopt := cmd_fail(@LOCATION, '${vexe} install --local --adopt external_pkg')
	assert adopt.output.contains('refusing to adopt `external_pkg`'), adopt.output
	assert !is_recorded_local_install(external)

	// Even a checkout VPM installed for some other project stays out of reach
	// through a link into it.
	record_local_install(external) or { panic(err) }
	removal := cmd_fail(@LOCATION, '${vexe} remove --local external_pkg')
	assert removal.output.contains('refusing to remove `external_pkg`'), removal.output
	assert os.is_dir(external)
	assert os.is_file(os.join_path(external, 'v.mod'))
	assert os.is_link(link_path)
}

// Adoption is for what VPM would have installed, which is always a checkout, and
// it is meaningless outside a local root, where everything is VPM's already.
fn test_adoption_refuses_what_vpm_could_not_have_installed() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_adopt_refusals'))
	project_dir := os.join_path(test_path, 'adopt_refusals_project')
	own_source := os.join_path(project_dir, 'own_mod')
	os.mkdir_all(own_source) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'adopt_refusals'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(own_source, 'own_mod.v'), 'module own_mod\n') or { panic(err) }

	old_dir := os.getwd()
	os.chdir(project_dir) or { panic(err) }
	defer {
		os.chdir(old_dir) or {}
	}
	plain := cmd_fail(@LOCATION, '${vexe} install --local --adopt own_mod')
	assert plain.output.contains('refusing to adopt `own_mod`'), plain.output
	assert !is_recorded_local_install(own_source)

	missing := cmd_fail(@LOCATION, '${vexe} install --local --adopt nothing_here')
	assert missing.output.contains('failed to find `nothing_here`'), missing.output

	global := os.execute('${vexe} install --adopt own_mod')
	assert global.exit_code == 2, global.output
	assert global.output.contains('`--adopt` is only meaningful together with `--local`'), global.output
}

// Records are absolute and `/`-separated whatever the host, so containment has to
// be judged in that convention too: `os.path_separator` would be `\` on Windows,
// where no record could ever be found under its own root again.
fn test_record_containment_uses_the_record_convention() {
	assert canonical_path_is_below('C:/proj/pkg', 'C:/proj')
	assert canonical_path_is_below('C:/proj/nested/pkg', 'C:/proj')
	assert canonical_path_is_below('/home/me/proj/pkg', '/home/me/proj')
	assert !canonical_path_is_below('C:/proj', 'C:/proj')
	assert !canonical_path_is_below('C:/projected/pkg', 'C:/proj')
	assert !canonical_path_is_below('/elsewhere/pkg', '/home/me/proj')
	// A root that already ends in a separator does not grow a second one.
	assert canonical_path_is_below('C:/pkg', 'C:/')
}

// A bare relative name that exists on disk is a local repository, unless it is a
// module already installed in the store, shadowing a registered name. Under
// `--local` the store is the project itself, so only what VPM installed there
// shadows: `v install --local vendor/dep` still means the repository at that
// path, not a query to VPM for a package called `vendor.dep`.
fn test_a_bare_relative_repository_under_the_local_root_is_still_a_repository() {
	test_utils.set_test_env(os.join_path(test_path, 'vmodules_bare_relative'))
	project_dir := os.join_path(test_path, 'bare_relative_project')
	os.mkdir_all(project_dir) or { panic(err) }
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module{\n\tname: 'bare_relative'\n}\n") or {
		panic(err)
	}
	vendored := os.join_path(project_dir, 'vendor', 'dep')
	create_local_git_module(vendored, 'dep')
	installed := os.join_path(project_dir, 'shadow_pkg')
	create_local_git_module(installed, 'shadow_pkg')
	record_local_install(installed) or { panic(err) }

	root := os.real_path(project_dir)
	// The project's own repository, in the root `--local` points at.
	assert !path_shadows_installed_module(os.real_path(vendored), root, true)
	// What VPM installed there does shadow the registered name.
	assert path_shadows_installed_module(os.real_path(installed), root, true)
	// The global store holds nothing but installed packages, so all of it does.
	assert path_shadows_installed_module(os.real_path(vendored), root, false)
	assert path_shadows_installed_module(root, root, true)
	// Nothing outside the root is the store's to shadow.
	assert !path_shadows_installed_module(os.real_path(test_path), root, false)
}

fn create_local_git_module(repo_path string, module_name string) {
	os.mkdir_all(repo_path) or { panic(err) }
	os.write_file(os.join_path(repo_path, 'v.mod'),
		"Module{\n\tname: '${module_name}'\n\tversion: '0.0.1'\n}\n") or { panic(err) }
	cmd_ok(@LOCATION, 'git init ${os.quoted_path(repo_path)}')
	cmd_ok(@LOCATION, 'git -C ${os.quoted_path(repo_path)} add v.mod')
	cmd_ok(@LOCATION,
		'git -C ${os.quoted_path(repo_path)} -c user.email="ci@vlang.io" -c user.name="V CI" commit -m "initial commit"')
}

fn file_url(path string) string {
	mut normalized_path := path.replace('\\', '/')
	if !normalized_path.starts_with('/') {
		normalized_path = '/${normalized_path}'
	}
	return 'file://${normalized_path}'
}
