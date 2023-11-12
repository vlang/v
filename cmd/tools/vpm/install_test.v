// vtest flaky: true
// vtest retry: 3
module main

import os
import v.vmod

const (
	v         = os.quoted_path(@VEXE)
	// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
	// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test-vmodules/`.
	test_path = os.join_path(os.vtmp_dir(), 'vpm_install_test')
)

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	// Skipping download count increment should be the default if vpm runs in CI.
	// Don't set `VPM_NO_INCREMENT` to test this behavior. `CI` is always true in GH CI runs.
	if os.getenv('CI') == '' {
		os.setenv('VPM_NO_INCREMENT', '1', true)
	}
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_install_from_vpm_ident() {
	res := os.execute_or_exit('${v} install nedpals.args')
	assert res.output.contains('Skipping download count increment for `nedpals.args`.'), res.output
	mod := vmod.from_file(os.join_path(test_path, 'nedpals', 'args', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'nedpals.args'
	assert mod.dependencies == []string{}
}

fn test_install_from_vpm_short_ident() {
	os.execute_or_exit('${v} install pcre')
	mod := vmod.from_file(os.join_path(test_path, 'pcre', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'pcre'
	assert mod.description == 'A simple regex library for V.'
}

fn test_install_from_git_url() {
	res := os.execute_or_exit('${v} install https://github.com/vlang/markdown')
	assert res.output.contains('Installing `markdown`'), res.output
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
}

fn test_install_already_existent() {
	mut res := os.execute_or_exit('${v} install https://github.com/vlang/markdown')
	assert res.output.contains('Updating module `markdown` in `${test_path}/markdown`'), res.output
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
	// The same module but with the `.git` extension added.
	os.execute_or_exit('${v} install https://github.com/vlang/markdown.git')
	assert res.output.contains('Updating module `markdown` in `${test_path}/markdown`'), res.output
}

fn test_install_once() {
	// Start with a clean test path.
	$if windows {
		// FIXME: Workaround for failing `rmdir` commands on Windows.
		os.system('rd /s /q ${test_path}')
	} $else {
		os.rmdir_all(test_path) or {}
	}
	os.mkdir_all(test_path) or {}

	// Install markdown module.
	os.execute_or_exit('${v} install markdown')
	// Keep track of the last modified state of the v.mod file of the installed markdown module.
	md_last_modified := os.file_last_mod_unix(os.join_path(test_path, 'markdown', 'v.mod'))

	install_cmd := '${@VEXE} install https://github.com/vlang/markdown https://github.com/vlang/pcre --once -v'
	// Try installing two modules, one of which is already installed.
	mut res := os.execute_or_exit(install_cmd)
	assert res.output.contains("Already installed modules: ['markdown']"), res.output
	mod := vmod.from_file(os.join_path(test_path, 'pcre', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'pcre'
	assert mod.description == 'A simple regex library for V.'
	// Ensure the before installed markdown module wasn't modified.
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))

	// Try installing two modules that are both already installed.
	res = os.execute_or_exit(install_cmd)
	assert res.output.contains('All modules are already installed.'), res.output
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))
}

fn test_missing_repo_name_in_url() {
	incomplete_url := 'https://github.com/vlang'
	res := os.execute('${v} install ${incomplete_url}')
	assert res.exit_code == 1
	assert res.output.contains('failed to retrieve module name for `${incomplete_url}`'), res.output
}

fn test_missing_vmod_in_url() {
	assert has_vmod('https://github.com/vlang/v', '') // head branch == `master`.
	assert has_vmod('https://github.com/v-analyzer/v-analyzer', '') // head branch == `main`.
	assert !has_vmod('https://github.com/octocat/octocat.github.io', '') // not a V module.
	res := os.execute('${v} install https://github.com/octocat/octocat.github.io')
	assert res.exit_code == 1
	assert res.output.contains('failed to find `v.mod` for `https://github.com/octocat/octocat.github.io`'), res.output
}
