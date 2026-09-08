module main

import os
import rand
import v.vmod
import test_utils { cmd_ok }

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
	create_local_git_module(repo_path, 'Foo.bar')

	res := cmd_ok(@LOCATION, '${vexe} install ${os.quoted_path(repo_path)}')
	assert res.output.contains('`Foo.bar` is not a valid V import path, it was installed as `foo.bar`.'), res.output

	assert res.output.contains('Use `foo.bar` as the normalized import prefix'), res.output
	assert os.exists(os.join_path(vmodules_path, 'foo', 'bar', 'v.mod'))
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
