// vtest retry: 3
module main

import os
import rand
import v.vmod
import test_utils

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
	mut res := os.execute('${vexe} install ${ident}@${tag}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installing `${ident}`'), res.output
	assert res.output.contains('Installed `${ident}`'), res.output
	mut manifest := get_vmod(relative_path)
	assert manifest.name == 'webview'
	assert manifest.version == '0.6.0'
	// Install same version without force flag.
	res = os.execute('${vexe} install ${ident}@${tag}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Module `${ident}@${tag}` is already installed, use --force to overwrite'), res.output
	// Install another version, add force flag to surpass confirmation.
	tag = 'v0.5.0'
	res = os.execute('${vexe} install -f ${ident}@${tag}')
	assert res.output.contains('Installed `${ident}`'), res.output
	assert res.exit_code == 0, res.str()
	manifest = get_vmod(relative_path)
	assert manifest.name == 'webview'
	assert manifest.version == '0.5.0'
	// Install invalid version.
	tag = '6.0'
	res = os.execute('${vexe} install -f ${ident}@${tag}')
	assert res.exit_code == 1, res.str()
	assert res.output.contains('failed to install `${ident}`'), res.output
	// Install invalid version verbose.
	res = os.execute('${vexe} install -f -v ${ident}@${tag}')
	assert res.exit_code == 1, res.str()
	assert res.output.contains('failed to install `${ident}`'), res.output
	assert res.output.contains('Remote branch 6.0 not found in upstream origin'), res.output
	// Install without version tag after a version was installed
	res = os.execute('${vexe} install -f ${ident}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installing `${ident}`'), res.output
	// Re-install latest version (without a tag). Should trigger an update, force should not be required.
	res = os.execute('${vexe} install ${ident}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Updating module `${ident}`'), res.output
}

fn test_install_from_url_with_git_version_tag() {
	mut url := 'https://github.com/vlang/vsl'
	mut tag := 'v0.1.50'
	mut res := os.execute('${vexe} install ${url}@${tag}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installing `vsl`'), res.output
	assert res.output.contains('Installed `vsl`'), res.output
	mut manifest := get_vmod('vsl')
	assert manifest.name == 'vsl'
	assert manifest.version == '0.1.50'
	// Install same version without force flag.
	res = os.execute('${vexe} install ${url}@${tag}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Module `vsl@${tag}` is already installed, use --force to overwrite'), res.output
	// Install another version, add force flag to surpass confirmation.
	tag = 'v0.1.47'
	res = os.execute('${vexe} install -f ${url}@${tag}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installed `vsl`'), res.output
	manifest = get_vmod('vsl')
	assert manifest.name == 'vsl'
	assert manifest.version == '0.1.47'
	// Install invalid version.
	tag = 'abc'
	res = os.execute('${vexe} install -f ${url}@${tag}')
	assert res.exit_code == 1, res.str()
	// Install invalid version verbose.
	res = os.execute('${vexe} install -f -v ${url}@${tag}')
	assert res.exit_code == 1, res.str()
	assert res.output.contains('failed to find `v.mod` for `${url}@${tag}`'), res.output
	// Install from GitLab.
	url = 'https://gitlab.com/tobealive/webview'
	tag = 'v0.6.0'
	res = os.execute('${vexe} install ${url}@${tag}')
	assert res.exit_code == 0, res.str()
	assert res.output.contains('Installed `webview`'), res.output
	manifest = get_vmod('webview')
	assert manifest.name == 'webview'
	assert manifest.version == '0.6.0'
}
