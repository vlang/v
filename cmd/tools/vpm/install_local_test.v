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
