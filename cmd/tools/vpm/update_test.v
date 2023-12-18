// vtest retry: 3
module main

import os
import rand
import test_utils

const v = os.quoted_path(@VEXE)
const test_path = os.join_path(os.vtmp_dir(), 'vpm_update_test_${rand.ulid()}')

fn testsuite_begin() {
	$if !network ? {
		eprintln('> skipping ${@FILE}, when `-d network` is missing')
		exit(0)
	}
	test_utils.set_test_env(test_path)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_update() {
	for m in ['pcre', 'https://github.com/spytheman/vtray', 'nedpals.args', 'ttytm.webview'] {
		os.execute_or_exit('${v} install ${m}')
	}
	// "outdate" some modules, by removing their last commit.
	for m in ['pcre', 'vtray', os.join_path('nedpals', 'args')] {
		path := os.join_path(test_path, m)
		os.execute_or_exit('git -C ${path} fetch --unshallow')
		os.execute_or_exit('git -C ${path} reset --hard HEAD~')
		assert is_outdated(path)
	}
	// Case: Run `v update` (without args).
	res := os.execute('${v} update')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `pcre`'), res.output
	assert res.output.contains('Updating module `vtray`'), res.output
	assert res.output.contains('Updating module `nedpals.args`'), res.output
	assert res.output.contains('Skipping download count increment for `pcre`.'), res.output
	assert res.output.contains('Skipping download count increment for `nedpals.args`.'), res.output
	assert !res.output.contains('Updating module `ttytm.webview`'), res.output
	assert !res.output.contains('Skipping download count increment for `ttytm.webview`.'), res.output
	for m in ['pcre', 'vtray', os.join_path('nedpals', 'args')] {
		assert !is_outdated(os.join_path(test_path, m))
	}
}

fn test_update_short_ident() {
	os.execute_or_exit('git -C ${os.join_path(test_path, 'pcre')} reset --hard HEAD~')
	res := os.execute('${v} update pcre')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `pcre`'), res.output
}

fn test_update_ident() {
	os.execute_or_exit('git -C ${os.join_path(test_path, 'nedpals', 'args')} reset --hard HEAD~')
	res := os.execute('${v} update nedpals.args')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `nedpals.args`'), res.output
}

fn test_update_url() {
	os.execute_or_exit('git -C ${os.join_path(test_path, 'vtray')} reset --hard HEAD~')
	res := os.execute('${v} update https://github.com/spytheman/vtray')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `vtray`'), res.output
}

fn test_update_multi_ident() {
	os.execute_or_exit('git -C ${os.join_path(test_path, 'nedpals', 'args')} reset --hard HEAD~')
	os.execute_or_exit('git -C ${os.join_path(test_path, 'vtray')} reset --hard HEAD~')
	res := os.execute('${v} update nedpals.args vtray')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `nedpals.args`'), res.output
	assert res.output.contains('Updating module `vtray`'), res.output
}

fn test_update_not_installed() {
	res := os.execute('${v} update vsl')
	assert res.exit_code == 1, res.str()
	assert res.output.contains('failed to update `vsl`. Not installed.'), res.output
}

fn test_update_mixed_installed_not_installed() {
	os.execute_or_exit('git -C ${os.join_path(test_path, 'pcre')} reset --hard HEAD~')
	res := os.execute('${v} update pcre vsl')
	assert res.exit_code == 1, res.str()
	assert res.output.contains('Updating module `pcre`'), res.output
	assert res.output.contains('failed to update `vsl`. Not installed.'), res.output
}

// TODO: hg tests
// TODO: recursive test
