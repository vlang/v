// vtest retry: 3
import os
import rand
import v.vmod
import test_utils { cmd_ok }

const vexe = os.quoted_path(@VEXE)
const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_version_input_test_${rand.ulid()}')
const expect_tests_path = os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm', 'expect')
const expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
})

fn testsuite_begin() {
	$if !network ? {
		eprintln('> skipping ${@FILE}, when `-d network` is missing')
		exit(0)
	}
	test_utils.set_test_env(test_path)
	eprintln('>> test_path: ${test_path}')
	// Explicitly disable fail on prompt.
	os.setenv('VPM_FAIL_ON_PROMPT', '', true)
	os.mkdir_all(test_path) or {}
	os.chdir(test_path)!
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn get_vmod(path string) vmod.Manifest {
	return vmod.from_file(os.join_path(test_path, path, 'v.mod')) or {
		eprintln('Failed to parse v.mod for `${path}`. ${err}')
		exit(1)
	}
}

// Test installing another version of a module of which an explicit version is already installed.
fn test_reinstall_mod_with_version_installation() {
	// Install version.
	ident := 'vsl'
	tag := 'v0.1.47'
	cmd_ok(@LOCATION, '${vexe} install ${ident}@${tag}')
	mut manifest := get_vmod(ident)
	assert manifest.name == ident
	assert manifest.version == tag.trim_left('v')

	// Try reinstalling.
	new_tag := 'v0.1.50'
	install_path := os.real_path(os.join_path(test_path, ident))
	expect_args := [vexe, ident, tag, new_tag, install_path].join(' ')

	// Decline.
	decline_test := os.join_path(expect_tests_path, 'decline_reinstall_mod_with_version_installation.expect')
	manifest_path := os.join_path(install_path, 'v.mod')
	last_modified := os.file_last_mod_unix(manifest_path)
	cmd_ok(@LOCATION, '${expect_exe} ${decline_test} ${expect_args}')
	assert last_modified == os.file_last_mod_unix(manifest_path)

	// Accept.
	accept_test := os.join_path(expect_tests_path, 'accept_reinstall_mod_with_version_installation.expect')
	cmd_ok(@LOCATION, '${expect_exe} ${accept_test} ${expect_args}')
	manifest = get_vmod(ident)
	assert manifest.name == ident
	assert manifest.version == new_tag.trim_left('v')
}
