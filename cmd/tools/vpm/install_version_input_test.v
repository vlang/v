// vtest retry: 3
import os
import v.vmod

const vexe = os.quoted_path(@VEXE)
const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_version_input_test')
const expect_tests_path = os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm', 'expect')
const expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
})

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
	os.setenv('VPM_DEBUG', '', true)
	// Explicitly disable fail on prompt.
	os.setenv('VPM_FAIL_ON_PROMPT', '', true)
	os.mkdir_all(test_path) or {}
	os.chdir(test_path)!
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn get_mod_name_and_version(path string) (string, string) {
	mod := vmod.from_file(os.join_path(test_path, path, 'v.mod')) or {
		eprintln(err)
		return '', ''
	}
	return mod.name, mod.version
}

// Test installing another version of a module of which an explicit version is already installed.
fn test_reinstall_mod_with_version_installation() {
	// Install version.
	mod := 'vsl'
	tag := 'v0.1.47'
	os.execute_or_exit('${vexe} install ${mod}@${tag}')
	mut name, mut version := get_mod_name_and_version(mod)
	assert name == mod
	assert version == tag.trim_left('v')

	// Try reinstalling.
	new_tag := 'v0.1.50'
	install_path := os.real_path(os.join_path(test_path, mod))
	expect_args := [vexe, mod, tag, new_tag, install_path].join(' ')

	// Decline.
	decline_test := os.join_path(expect_tests_path, 'decline_reinstall_mod_with_version_installation.expect')
	manifest_path := os.join_path(install_path, 'v.mod')
	last_modified := os.file_last_mod_unix(manifest_path)
	os.execute_or_exit('${expect_exe} ${decline_test} ${expect_args}')
	assert last_modified == os.file_last_mod_unix(manifest_path)

	// Accept.
	accept_test := os.join_path(expect_tests_path, 'accept_reinstall_mod_with_version_installation.expect')
	os.execute_or_exit('${expect_exe} ${accept_test} ${expect_args}')
	name, version = get_mod_name_and_version(mod)
	assert name == mod, name
	assert version == new_tag.trim_left('v'), version
}
