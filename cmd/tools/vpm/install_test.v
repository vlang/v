// vtest retry: 3
module main

import os
import rand
import v.vmod
import test_utils { cmd_fail, cmd_ok }

// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test-vmodules/`.
const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_test_${rand.ulid()}')

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

fn get_vmod(path string) vmod.Manifest {
	return vmod.from_file(os.join_path(test_path, path, 'v.mod')) or {
		eprintln('Failed to parse v.mod for `${path}`. ${err}')
		exit(1)
	}
}

fn test_install_from_vpm_ident() {
	res := cmd_ok(@LOCATION, '${vexe} install nedpals.args')
	assert res.output.contains('Skipping download count increment for `nedpals.args`.'), res.output
	manifest := get_vmod(os.join_path('nedpals', 'args'))
	assert manifest.name == 'nedpals.args'
	assert manifest.dependencies == []string{}
}

fn test_install_from_vpm_short_ident() {
	cmd_ok(@LOCATION, '${vexe} install pcre')
	manifest := get_vmod('pcre')
	assert manifest.name == 'pcre'
	assert manifest.description == 'A simple regex library for V.'
}

fn test_install_from_git_url() {
	mut res := cmd_ok(@LOCATION, '${vexe} install https://github.com/vlang/markdown')
	assert res.output.contains('Installing `markdown`'), res.output
	mut manifest := get_vmod('markdown')
	assert manifest.name == 'markdown'
	assert manifest.dependencies == []string{}
	res = cmd_ok(@LOCATION, '${vexe} install http://github.com/Wertzui123/HashMap')
	assert res.output.contains('Installing `HashMap`'), res.output
	assert res.output.contains('`http` is deprecated'), res.output
	manifest = get_vmod(os.join_path('wertzui123', 'hashmap'))
	res = cmd_ok(@LOCATION, '${vexe} install http://github.com/Wertzui123/HashMap')
	assert res.output.contains('Updating module `wertzui123.hashmap`'), res.output
	assert res.output.contains('`http` is deprecated'), res.output
	res = cmd_ok(@LOCATION, '${vexe} install https://gitlab.com/tobealive/webview')
	assert res.output.contains('Installed `webview`'), res.output
}

fn test_install_already_existent() {
	mut res := cmd_ok(@LOCATION, '${vexe} install https://github.com/vlang/markdown')
	assert res.output.contains('Updating module `markdown`'), res.output
	manifest := get_vmod('markdown')
	assert manifest.name == 'markdown'
	assert manifest.dependencies == []string{}
	// The same module but with the `.git` extension added.
	res = cmd_ok(@LOCATION, '${vexe} install https://github.com/vlang/markdown.git')
	assert res.output.contains('Updating module `markdown`'), res.output
}

fn test_install_once() {
	// Start with a clean test path.
	rmdir_all(test_path) or {}
	os.mkdir_all(test_path) or {}

	// Install markdown module.
	mut res := cmd_ok(@LOCATION, '${vexe} install markdown')
	// Keep track of the last modified state of the v.mod file of the installed markdown module.
	md_last_modified := os.file_last_mod_unix(os.join_path(test_path, 'markdown', 'v.mod'))

	install_cmd := '${@VEXE} install https://github.com/vlang/markdown https://github.com/vlang/pcre --once -v'
	// Try installing two modules, one of which is already installed.
	res = cmd_ok(@LOCATION, install_cmd)
	assert res.output.contains("Already installed modules: ['markdown']"), res.output
	manifest := get_vmod('pcre')
	assert manifest.name == 'pcre'
	assert manifest.description == 'A simple regex library for V.'
	// Ensure the before installed markdown module wasn't modified.
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))

	// Try installing two modules that are both already installed.
	res = cmd_ok(@LOCATION, install_cmd)
	assert res.output.contains('All modules are already installed.'), res.output
	assert md_last_modified == os.file_last_mod_unix(os.join_path(test_path, 'markdown',
		'v.mod'))
}

fn test_missing_repo_name_in_url() {
	incomplete_url := 'https://github.com/vlang'
	res := cmd_fail(@LOCATION, '${vexe} install ${incomplete_url}')
	assert res.output.contains('failed to retrieve module name for `${incomplete_url}`'), res.output
}

fn test_manifest_detection() {
	mut res := cmd_fail(@LOCATION, '${vexe} install https://github.com/octocat/octocat.github.io')
	assert res.output.contains('failed to find `v.mod` for `https://github.com/octocat/octocat.github.io`'), res.output
	// No error for vpm modules yet.
	res = cmd_ok(@LOCATION, '${vexe} install spytheman.regex')
	assert res.output.contains('`spytheman.regex` is missing a manifest file'), res.output
	assert res.output.contains('Installing `spytheman.regex`'), res.output
}

fn test_install_potentially_conflicting() {
	mut res := os.execute('${vexe} install ui')
	assert res.output.contains('Installed `ui`')
	mut manifest := get_vmod('ui')
	assert manifest.name == 'ui'
	res = os.execute('${vexe} install https://github.com/isaiahpatton/ui')
	assert res.output.contains('Installed `iui`')
	manifest = get_vmod('iui')
	assert manifest.name == 'iui'
}

fn test_get_installed_version() {
	test_project_path := os.join_path(test_path, 'test_project')
	mut res := cmd_ok(@LOCATION, 'git init ${test_project_path}')
	os.chdir(test_project_path)!
	if os.execute('git config user.name').exit_code == 1 {
		os.execute_or_exit('git config user.email "ci@vlang.io"')
		os.execute_or_exit('git config user.name "V CI"')
	}
	os.write_file('v.mod', '')!
	res = cmd_ok(@LOCATION, 'git add .')
	res = cmd_ok(@LOCATION, 'git commit -m "initial commit"')
	mut mod := Module{
		install_path: test_project_path
	}
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == ''

	// Create a tag -> latests commit and tag are at the same state,
	// but it should not be treated as a version installation, when there is another head branch.
	res = cmd_ok(@LOCATION, 'git tag v0.1.0 -m "some tag message"') // note: without a tag message, git will try to start an editor when you run this test locally, which will block
	mod.is_installed = false
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == ''

	cmd_ok(@LOCATION, 'git checkout v0.1.0')
	mod.is_installed = false
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == ''

	cmd_ok(@LOCATION, 'git branch -D master')
	cmd_ok(@LOCATION, 'git reset --hard v0.1.0')
	mod.is_installed = false
	mod.get_installed()
	assert mod.is_installed
	assert mod.installed_version == 'v0.1.0'
}

fn test_install_from_hg_url() ! {
	hg_path := os.find_abs_path_of_executable('hg') or {
		eprintln('skipping test, since `hg` is not executable.')
		return
	}
	test_module_path := os.join_path(os.temp_dir(), rand.ulid(), 'hg_test_module')
	defer {
		os.rmdir_all(test_module_path) or {}
	}
	// Initialize project without manifest file.
	mut res := cmd_ok(@LOCATION, 'hg init ${test_module_path}')

	println('> writing .hg/hgrc to the new mercurial repo ...')
	os.mkdir_all(os.join_path(test_module_path, '.hg'))!
	os.write_file(os.join_path(test_module_path, '.hg/hgrc'), '[ui]\nusername = v_ci <v_ci@example.net>\nverbose = False\n')!
	println('> writing .hg/hgrc done.')

	mut p, mut port := test_utils.hg_serve(hg_path, test_module_path, 2000)
	// Trying to install it should fail.
	res = os.execute('${vexe} install --hg http://127.0.0.1:${port}')
	p.signal_kill()
	assert res.output.contains('failed to find `v.mod`'), res.output
	// Create and commit manifest.
	name := 'my_awesome_v_module'
	version := '1.0.0'
	os.write_file(os.join_path(test_module_path, 'v.mod'), "Module{
	name: '${name}'
	version: '${version}'
}")!
	os.chdir(test_module_path)!
	cmd_ok(@LOCATION, 'hg add')
	cmd_ok(@LOCATION, 'hg commit -m "add v.mod"')
	p, port = test_utils.hg_serve(hg_path, test_module_path, 3000)
	// Trying to install the module should work now.
	res = cmd_ok(@LOCATION, '${vexe} install --hg http://127.0.0.1:${port}')
	p.signal_kill()
	// Get manifest from the vmodules directory.
	manifest := get_vmod(name)
	assert manifest.name == name
	assert manifest.version == version
}
