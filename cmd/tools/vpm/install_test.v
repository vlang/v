// vtest flaky: true
// vtest retry: 3
module main

import os
import v.vmod

// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test-vmodules/`.
const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_test')

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
	os.setenv('VPM_FAIL_ON_PROMPT', '1', true)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_install_from_vpm_ident() {
	res := os.execute_or_exit('${vexe} install nedpals.args')
	assert res.output.contains('Skipping download count increment for `nedpals.args`.'), res.output
	mod := vmod.from_file(os.join_path(test_path, 'nedpals', 'args', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'nedpals.args'
	assert mod.dependencies == []string{}
}

fn test_install_from_vpm_short_ident() {
	os.execute_or_exit('${vexe} install pcre')
	mod := vmod.from_file(os.join_path(test_path, 'pcre', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'pcre'
	assert mod.description == 'A simple regex library for V.'
}

fn test_install_from_git_url() {
	mut res := os.execute_or_exit('${vexe} install https://github.com/vlang/markdown')
	assert res.output.contains('Installing `markdown`'), res.output
	mut mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
	res = os.execute_or_exit('${vexe} install http://github.com/Wertzui123/HashMap')
	assert res.output.contains('Installing `HashMap`'), res.output
	assert res.output.contains('`http` is deprecated'), res.output
	mod = vmod.from_file(os.join_path(test_path, 'wertzui123', 'hashmap', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	res = os.execute_or_exit('${vexe} install http://github.com/Wertzui123/HashMap')
	assert res.output.contains('Updating module `wertzui123.hashmap`'), res.output
	assert res.output.contains('`http` is deprecated'), res.output
	res = os.execute_or_exit('${vexe} install https://gitlab.com/tobealive/webview')
	assert res.output.contains('Installed `webview`'), res.output
}

fn test_install_already_existent() {
	mut res := os.execute_or_exit('${vexe} install https://github.com/vlang/markdown')
	assert res.output.contains('Updating module `markdown` in `${test_path}/markdown`'), res.output
	mod := vmod.from_file(os.join_path(test_path, 'markdown', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'markdown'
	assert mod.dependencies == []string{}
	// The same module but with the `.git` extension added.
	os.execute_or_exit('${vexe} install https://github.com/vlang/markdown.git')
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
	os.execute_or_exit('${vexe} install markdown')
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
	res := os.execute('${vexe} install ${incomplete_url}')
	assert res.exit_code == 1
	assert res.output.contains('failed to retrieve module name for `${incomplete_url}`'), res.output
}

fn test_manifest_detection() {
	// head branch == `main`.
	mut mod := fetch_manifest('v-analyzer', 'https://github.com/v-analyzer/v-analyzer',
		'', true) or {
		assert false
		return
	}
	assert mod.name == 'v-analyzer'
	assert mod.dependencies == ['https://github.com/v-analyzer/v-tree-sitter']
	// head branch == `master`.
	mod = fetch_manifest('ui', 'https://github.com/pisaiah/ui', '', true) or {
		assert false
		return
	}
	assert mod.name == 'iui'
	// not a V module.
	if v := fetch_manifest('octocat', 'https://github.com/octocat/octocat.github.io',
		'', true)
	{
		assert false, v.str()
		return
	}
	mut res := os.execute('${vexe} install https://github.com/octocat/octocat.github.io')
	assert res.exit_code == 1
	assert res.output.contains('failed to find `v.mod` for `https://github.com/octocat/octocat.github.io`'), res.output
	// No error for vpm modules yet.
	res = os.execute_or_exit('${vexe} install spytheman.regex')
	assert res.output.contains('`spytheman.regex` is missing a manifest file'), res.output
	assert res.output.contains('Installing `spytheman.regex`'), res.output
}

fn test_install_potentially_conflicting() {
	mut res := os.execute('${vexe} install ui')
	assert res.output.contains('Installed `ui`')
	mut mod := vmod.from_file(os.join_path(test_path, 'ui', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'ui'
	res = os.execute('${vexe} install https://github.com/isaiahpatton/ui')
	assert res.output.contains('Installed `iui`')
	mod = vmod.from_file(os.join_path(test_path, 'iui', 'v.mod')) or {
		assert false, err.msg()
		return
	}
	assert mod.name == 'iui'
}
