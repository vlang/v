// vtest retry: 3
module main

import os
import rand
import test_utils { cmd_ok }

const test_path = os.join_path(os.vtmp_dir(), 'vpm_outdated_test_${rand.ulid()}')

fn testsuite_begin() {
	$if !network ? {
		eprintln('> skipping ${@FILE}, when `-d network` is missing')
		exit(0)
	}
	test_utils.set_test_env(test_path)
	os.mkdir_all(test_path)!
	os.chdir(test_path)!
	println('test_path: ${test_path}')
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_is_outdated_git_module() {
	cmd_ok(@LOCATION, 'git clone https://github.com/vlang/libsodium.git')
	assert !is_outdated('libsodium')
	cmd_ok(@LOCATION, 'git -C libsodium reset --hard HEAD~')
	assert is_outdated('libsodium')
	cmd_ok(@LOCATION, 'git -C libsodium pull')
	assert !is_outdated('libsodium')
}

fn test_is_outdated_hg_module() {
	os.find_abs_path_of_executable('hg') or {
		eprintln('skipping test, since `hg` is not executable.')
		return
	}
	cmd_ok(@LOCATION, 'hg clone https://www.mercurial-scm.org/repo/hello')
	assert !is_outdated('hello')
	cmd_ok(@LOCATION, 'hg --config extensions.strip= -R hello strip -r tip')
	assert is_outdated('hello')
	cmd_ok(@LOCATION, 'hg -R hello pull')
	assert !is_outdated('hello')
}

fn test_outdated() {
	for m in ['pcre', 'libsodium', 'https://github.com/spytheman/vtray', 'nedpals.args'] {
		cmd_ok(@LOCATION, '${vexe} install ${m}')
	}
	// "Outdate" previously installed. Leave out `libsodium`.
	for m in ['pcre', 'vtray', os.join_path('nedpals', 'args')] {
		cmd_ok(@LOCATION, 'git -C ${m} fetch --all')
		cmd_ok(@LOCATION, 'git -C ${m} reset --hard HEAD~')
		assert is_outdated(m)
	}
	res := cmd_ok(@LOCATION, '${vexe} outdated')
	output := res.output.all_after('Outdated modules:')
	assert output.len > 0, output
	assert output.contains('pcre'), output
	assert output.contains('vtray'), output
	assert output.contains('nedpals.args'), output
	assert !output.contains('libsodium'), output
}
