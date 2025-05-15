// vtest retry: 3
module main

import os
import rand
import v.vmod
import test_utils { cmd_fail, cmd_ok }

const test_path = os.join_path(os.vtmp_dir(), 'vpm_install_version_test_${rand.ulid()}')

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

fn test_install_from_vpm_with_git_version_tag() {
	ident := 'ttytm.webview'
	relative_path := ident.replace('.', os.path_separator)
	mut tag := 'v0.6.0'
	mut res := cmd_ok(@LOCATION, '${vexe} install ${ident}@${tag}')
	assert res.output.contains('Installing `${ident}`'), res.output
	assert res.output.contains('Installed `${ident}`'), res.output
	mut manifest := get_vmod(relative_path)
	assert manifest.name == 'webview'
	assert manifest.version == '0.6.0'
	// Install same version without force flag.
	res = cmd_ok(@LOCATION, '${vexe} install ${ident}@${tag}')
	assert res.output.contains('Module `${ident}@${tag}` is already installed, use --force to overwrite'), res.output
	// Install another version, add force flag to surpass confirmation.
	tag = 'v0.5.0'
	res = cmd_ok(@LOCATION, '${vexe} install -f ${ident}@${tag}')
	assert res.output.contains('Installed `${ident}`'), res.output
	manifest = get_vmod(relative_path)
	assert manifest.name == 'webview'
	assert manifest.version == '0.5.0'
	// Install invalid version.
	tag = '6.0'
	res = cmd_fail(@LOCATION, '${vexe} install -f ${ident}@${tag}')
	assert res.output.contains('failed to install `${ident}`'), res.output

	// Install invalid version verbose.
	res = cmd_fail(@LOCATION, '${vexe} install -f -v ${ident}@${tag}')
	assert res.output.contains('failed to install `${ident}`'), res.output
	assert res.output.contains('Remote branch 6.0 not found in upstream origin'), res.output
	// Install without version tag after a version was installed
	res = cmd_ok(@LOCATION, '${vexe} install -f ${ident}')
	assert res.output.contains('Installing `${ident}`'), res.output
	// Re-install latest version (without a tag). Should trigger an update, force should not be required.
	res = cmd_ok(@LOCATION, '${vexe} install ${ident}')
	assert res.output.contains('Updating module `${ident}`'), res.output
}

fn test_install_from_git_url_with_version_tag() {
	mut url := 'https://github.com/vlang/vsl'
	mut tag := 'v0.1.50'
	mut res := cmd_ok(@LOCATION, '${vexe} install ${url}@${tag}')
	assert res.output.contains('Installing `vsl`'), res.output
	assert res.output.contains('Installed `vsl`'), res.output
	mut manifest := get_vmod('vsl')
	assert manifest.name == 'vsl'
	assert manifest.version == '0.1.50'
	// Install same version without force flag.
	res = cmd_ok(@LOCATION, '${vexe} install ${url}@${tag}')
	assert res.output.contains('Module `vsl@${tag}` is already installed, use --force to overwrite'), res.output
	// Install another version, add force flag to surpass confirmation.
	tag = 'v0.1.47'
	res = cmd_ok(@LOCATION, '${vexe} install -f ${url}@${tag}')
	assert res.output.contains('Installed `vsl`'), res.output
	manifest = get_vmod('vsl')
	assert manifest.name == 'vsl'
	assert manifest.version == '0.1.47'
	// Install invalid version.
	tag = 'abc'
	res = cmd_fail(@LOCATION, '${vexe} install -f ${url}@${tag}')
	// Install invalid version verbose.
	res = cmd_fail(@LOCATION, '${vexe} install -f -v ${url}@${tag}')
	not_found := res.output.contains('Could not find remote branch ${tag} to clone.')
		|| res.output.contains('Remote branch ${tag} not found')
	assert not_found, res.output
	// Install from GitLab.
	url = 'https://gitlab.com/tobealive/webview'
	tag = 'v0.6.0'
	res = cmd_ok(@LOCATION, '${vexe} install ${url}@${tag}')
	assert res.output.contains('Installed `webview`'), res.output
	manifest = get_vmod('webview')
	assert manifest.name == 'webview'
	assert manifest.version == '0.6.0'
}

fn test_install_from_hg_url_with_version_tag() ! {
	hg_path := os.find_abs_path_of_executable('hg') or {
		eprintln('skipping test, since `hg` is not executable.')
		return
	}

	hg_version := cmd_ok(@LOCATION, 'hg version -q')
	dump(hg_version.output.trim_space())

	test_module_path := os.join_path(os.temp_dir(), rand.ulid(), 'hg_test_module')
	defer {
		os.rmdir_all(test_module_path) or {}
	}
	mut res := cmd_ok(@LOCATION, 'hg init ${test_module_path}')
	os.chdir(test_module_path)!

	println('> writing .hg/hgrc to the new mercurial repo ...')
	os.mkdir_all('.hg')!
	os.write_file('.hg/hgrc', '[ui]\nusername = v_ci <v_ci@example.net>\nverbose = False\n')!
	println('> writing .hg/hgrc done.')

	println('> writing v.mod file ...')
	os.write_file('v.mod', "Module{
	name: 'my_awesome_v_module'
	version: '0.1.0'
}")!
	println('> writing v.mod file done.')

	cmd_ok(@LOCATION, 'hg add')
	cmd_ok(@LOCATION, 'hg commit -m "initial commit"')
	println('> writing README.md file ...')
	os.write_file('README.md', 'Hello World!')!
	println('> writing README.md file done.')
	cmd_ok(@LOCATION, 'hg add')
	cmd_ok(@LOCATION, 'hg commit -m "add readme"')
	cmd_ok(@LOCATION, 'hg tag v0.1.0')

	println('> rewriting v.mod ...')
	os.write_file('v.mod', "Module{
	name: 'my_awesome_v_module'
	version: '0.2.0'
}")!
	println('> rewriting v.mod done.')

	cmd_ok(@LOCATION, 'hg add')
	cmd_ok(@LOCATION, 'hg commit -m "bump version to v0.2.0"')

	mut p, port := test_utils.hg_serve(hg_path, test_module_path, 4000)
	res = os.execute('${vexe} install -v --hg http://127.0.0.1:${port}@v0.1.0')
	p.signal_kill()
	if res.exit_code != 0 {
		assert false, 'location: $@LOCATION, res:\n${res}'
	}
	// Get manifest from the vmodules directory.
	manifest := get_vmod('my_awesome_v_module')
	assert manifest.name == 'my_awesome_v_module'
	assert manifest.version == '0.1.0'
}
