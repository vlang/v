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

fn get_mod_name(path string) string {
	mod := vmod.from_file(path) or {
		eprintln(err)
		return ''
	}
	return mod.name
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
	dependencies: ['markdown', 'pcre', 'https://github.com/spytheman/vtray']
}"
	os.write_file(vmod_path, vmod_contents)!
	v_mod := vmod.from_file(vmod_path) or {
		assert false, err.msg()
		return
	}
	assert v_mod.dependencies == ['markdown', 'pcre', 'https://github.com/spytheman/vtray']
	// Run `v install`
	res := os.execute_or_exit('${v} install')
	assert res.output.contains('Detected v.mod file inside the project directory. Using it...'), res.output
	assert res.output.contains('Installing module `markdown`'), res.output
	assert res.output.contains('Installing module `pcre`'), res.output
	assert res.output.contains('Installing module `vtray`'), res.output
	assert get_mod_name(os.join_path(test_path, 'markdown', 'v.mod')) == 'markdown'
	assert get_mod_name(os.join_path(test_path, 'pcre', 'v.mod')) == 'pcre'
	assert get_mod_name(os.join_path(test_path, 'vtray', 'v.mod')) == 'vtray'
}

fn test_resolve_external_dependencies_during_module_install() {
	res := os.execute_or_exit('${v} install https://github.com/ttytm/emoji-mart-desktop')
	assert res.output.contains('Resolving 2 dependencies'), res.output
	assert res.output.contains('Installing module `webview`'), res.output
	assert res.output.contains('Installing module `miniaudio`'), res.output
	// The external dependencies should have been installed to `<vmodules_dir>/<dependency_name>`
	assert get_mod_name(os.join_path(test_path, 'webview', 'v.mod')) == 'webview'
	assert get_mod_name(os.join_path(test_path, 'miniaudio', 'v.mod')) == 'miniaudio'
}
