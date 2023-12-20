// vtest retry: 3
module main

import os
import rand
import test_utils

const test_path = os.join_path(os.vtmp_dir(), 'vpm_outdated_test_${rand.ulid()}')

fn testsuite_begin() {
	$if !network ? {
		eprintln('> skipping ${@FILE}, when `-d network` is missing')
		exit(0)
	}
	test_utils.set_test_env(test_path)
	os.mkdir_all(test_path)!
	os.chdir(test_path)!
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_is_outdated_git_module() {
	os.execute_or_exit('git clone https://github.com/vlang/libsodium.git')
	assert !is_outdated('libsodium')
	os.execute_or_exit('git -C libsodium reset --hard HEAD~')
	assert is_outdated('libsodium')
	os.execute_or_exit('git -C libsodium pull')
	assert !is_outdated('libsodium')
}

fn test_is_outdated_hg_module() {
	os.find_abs_path_of_executable('hg') or {
		eprintln('skipping test, since `hg` is not executable.')
		return
	}
	os.execute_or_exit('hg clone https://www.mercurial-scm.org/repo/hello')
	assert !is_outdated('hello')
	os.execute_or_exit('hg --config extensions.strip= -R hello strip -r tip')
	assert is_outdated('hello')
	os.execute_or_exit('hg -R hello pull')
	assert !is_outdated('hello')
}

fn test_outdated() {
	for m in ['pcre', 'libsodium', 'https://github.com/spytheman/vtray', 'nedpals.args'] {
		os.execute_or_exit('${vexe} install ${m}')
	}
	// "Outdate" previously installed. Leave out `libsodium`.
	for m in ['pcre', 'vtray', os.join_path('nedpals', 'args')] {
		os.execute_or_exit('git -C ${m} fetch --unshallow')
		os.execute_or_exit('git -C ${m} reset --hard HEAD~')
		assert is_outdated(m)
	}
	res := os.execute('${vexe} outdated')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Outdated modules:'), res.output
	assert res.output.contains('pcre'), res.output
	assert res.output.contains('vtray'), res.output
	assert res.output.contains('nedpals.args'), res.output
	assert !res.output.contains('libsodium'), res.output
}
