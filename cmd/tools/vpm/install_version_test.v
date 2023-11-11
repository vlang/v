// vtest flaky: true
// vtest retry: 3
module main

import os
import v.vmod

const (
	v         = os.quoted_path(@VEXE)
	test_path = os.join_path(os.vtmp_dir(), 'vpm_install_version_test')
)

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
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

fn test_install_from_vpm_with_git_version_tag() {
	ident := 'ttytm.webview'
	mut tag := 'v0.6.0'
	mut res := os.execute_or_exit('${v} install ${ident}@${tag}')
	assert res.output.contains('Installing `${ident}`'), res.output
	assert res.output.contains('Installed `${ident}`'), res.output
	mut name, mut version := get_mod_name_and_version(os.join_path('ttytm', 'webview'))
	assert name == 'webview'
	assert version == '0.6.0'
	// Install same version without force flag.
	res = os.execute_or_exit('${v} install ${ident}@${tag}')
	assert res.output.contains('Module `${ident}@${tag}` is already installed, use --force to overwrite'), res.output
	// Install another version, add force flag to surpass confirmation.
	tag = 'v0.5.0'
	res = os.execute_or_exit('${v} install -f ${ident}@${tag}')
	assert res.output.contains('Installed `${ident}`'), res.output
	name, version = get_mod_name_and_version(os.join_path('ttytm', 'webview'))
	assert name == 'webview'
	assert version == '0.5.0'
	// Install invalid version.
	tag = '6.0'
	res = os.execute('${v} install -f ${ident}@${tag}')
	assert res.exit_code == 1
	assert res.output.contains('failed to install `${ident}`'), res.output
	// Install invalid version verbose.
	res = os.execute('${v} install -f -v ${ident}@${tag}')
	assert res.exit_code == 1
	assert res.output.contains('failed to install `${ident}`'), res.output
	assert res.output.contains('Remote branch 6.0 not found in upstream origin'), res.output
	// Install without version tag after a version was installed
	res = os.execute_or_exit('${v} install -f ${ident}')
	assert res.output.contains('Installing `${ident}`'), res.output
	// Re-install latest version (without a tag). Should trigger an update, force should not be required.
	res = os.execute_or_exit('${v} install ${ident}')
	assert res.output.contains('Updating module `${ident}`'), res.output
}

fn test_install_from_url_with_git_version_tag() {
	url := 'https://github.com/vlang/vsl'
	mut tag := 'v0.1.50'
	mut res := os.execute_or_exit('v install ${url}@${tag}')
	assert res.output.contains('Installing `vsl`'), res.output
	assert res.output.contains('Installed `vsl`'), res.output
	mut name, mut version := get_mod_name_and_version('vsl')
	assert name == 'vsl'
	assert version == '0.1.50'
	// Install same version without force flag.
	res = os.execute_or_exit('${v} install ${url}@${tag}')
	assert res.output.contains('Module `vsl@${tag}` is already installed, use --force to overwrite'), res.output
	// Install another version, add force flag to surpass confirmation.
	tag = 'v0.1.47'
	res = os.execute_or_exit('${v} install -f ${url}@${tag}')
	assert res.output.contains('Installed `vsl`'), res.output
	name, version = get_mod_name_and_version('vsl')
	assert name == 'vsl'
	assert version == '0.1.47'
	// Install invalid version.
	tag = 'abc'
	res = os.execute('${v} install -f ${url}@${tag}')
	assert res.exit_code == 1
	// Install invalid version verbose.
	res = os.execute('${v} install -f -v ${url}@${tag}')
	assert res.exit_code == 1
	assert res.output.contains('Remote branch abc not found in upstream origin'), res.output
}
