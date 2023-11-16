// vtest flaky: true
// vtest retry: 3
import os

const (
	v         = os.quoted_path(@VEXE)
	test_path = os.join_path(os.vtmp_dir(), 'vpm_update_test')
)

fn testsuite_begin() {
	os.setenv('VMODULES', test_path, true)
	os.setenv('VPM_DEBUG', '', true)
	os.setenv('VPM_NO_INCREMENT', '1', true)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

// Tests if `v update` detects installed modules and runs successfully.
fn test_update() {
	os.execute_or_exit('${v} install pcre')
	os.execute_or_exit('${v} install nedpals.args')
	os.execute_or_exit('${v} install https://github.com/spytheman/vtray')
	res := os.execute_opt('${v} update') or { panic(err) }
	assert res.output.contains('Updating module `pcre`'), res.output
	assert res.output.contains('Updating module `nedpals.args`'), res.output
	assert res.output.contains('Updating module `vtray`'), res.output
	assert res.output.contains('Skipping download count increment for `nedpals.args`.'), res.output
	assert res.output.contains('Skipping download count increment for `pcre`.'), res.output
}

fn test_update_idents() {
	mut res := os.execute_or_exit('${v} update pcre')
	assert res.output.contains('Updating module `pcre`'), res.output
	res = os.execute_or_exit('${v} update nedpals.args vtray')
	assert res.output.contains('Updating module `vtray`'), res.output
	assert res.output.contains('Updating module `nedpals.args`'), res.output
	// Update installed module using its url.
	res = os.execute('${v} update https://github.com/spytheman/vtray')
	assert res.output.contains('Updating module `vtray`'), res.output
	// Try update not installed.
	res = os.execute('${v} update vsl')
	assert res.exit_code == 1
	assert res.output.contains('failed to find `vsl`'), res.output
	// Try update mixed.
	res = os.execute('${v} update pcre vsl')
	assert res.exit_code == 1
	assert res.output.contains('Updating module `pcre`'), res.output
	assert res.output.contains('failed to find `vsl`'), res.output
}
