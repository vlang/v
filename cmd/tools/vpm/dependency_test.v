// vtest flaky: true
// vtest retry: 3
import os
import v.vmod

const (
	v         = os.quoted_path(@VEXE)
	test_path = os.join_path(os.vtmp_dir(), 'vpm_dependency_test')
)

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

// path relative to `test_path`
fn get_mod(path string) vmod.Manifest {
	return vmod.from_file(os.join_path(test_path, path, 'v.mod')) or {
		eprintln(err)
		vmod.Manifest{}
	}
}

// Case: running `v install` without specifying modules in a V project directory.
fn test_install_dependencies_in_module_dir() {
	os.mkdir_all(test_path) or {}
	mod := 'my_module'
	mod_path := os.join_path(test_path, mod)
	os.mkdir(mod_path)!
	os.chdir(mod_path)!
	// Create a v.mod file that lists dependencies.
	vmod_path := os.join_path(mod_path, 'v.mod')
	vmod_contents := "Module {
	name: '${mod}'
	description: ''
	version: '0.0.0'
	license: 'MIT'
	dependencies: ['markdown', 'pcre', 'https://github.com/spytheman/vtray', 'ttytm.webview@v0.6.0',
		'https://github.com/ttytm/vibe@v0.4.0']
}"
	os.write_file(vmod_path, vmod_contents)!
	v_mod := get_mod(mod)
	assert v_mod.dependencies == ['markdown', 'pcre', 'https://github.com/spytheman/vtray',
		'ttytm.webview@v0.6.0', 'https://github.com/ttytm/vibe@v0.4.0']
	// Run `v install`
	res := os.execute_or_exit('${v} install')
	assert res.output.contains('Detected v.mod file inside the project directory. Using it...'), res.output
	assert res.output.contains('Installing `markdown`'), res.output
	assert res.output.contains('Installing `pcre`'), res.output
	assert res.output.contains('Installing `vtray`'), res.output
	assert res.output.contains('Installing `ttytm.webview`'), res.output
	assert res.output.contains('Installing `vibe`'), res.output
	assert get_mod('markdown').name == 'markdown'
	assert get_mod('pcre').name == 'pcre'
	assert get_mod('vtray').name == 'vtray'
	webview := get_mod(os.join_path('ttytm', 'webview'))
	assert webview.name == 'webview'
	assert webview.version == '0.6.0'
	vibe := get_mod(os.join_path('vibe'))
	assert vibe.name == 'vibe'
	assert vibe.version == '0.4.0'
}

fn test_resolve_external_dependencies_during_module_install() {
	res := os.execute_or_exit('${v} install https://github.com/ttytm/emoji-mart-desktop')
	assert res.output.contains('Resolving 2 dependencies'), res.output
	assert res.output.contains('Installing `webview`'), res.output
	assert res.output.contains('Installing `miniaudio`'), res.output
	// The external dependencies should have been installed to `<vmodules_dir>/<dependency_name>`
	assert get_mod('webview').name == 'webview'
	assert get_mod('miniaudio').name == 'miniaudio'
}
