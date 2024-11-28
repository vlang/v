// vtest retry: 3
import os
import rand
import test_utils { cmd_fail, cmd_ok }

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

// Tests if `v update` detects installed modules and runs successfully.
fn test_update() {
	os.execute_or_exit('${v} install pcre')
	os.execute_or_exit('${v} install nedpals.args')
	os.execute_or_exit('${v} install https://github.com/spytheman/vtray')
	res := cmd_ok(@LOCATION, '${v} update')
	assert res.output.contains('Updating module `pcre`'), res.output
	assert res.output.contains('Updating module `nedpals.args`'), res.output
	assert res.output.contains('Updating module `vtray`'), res.output
	assert res.output.contains('Skipping download count increment for `nedpals.args`.'), res.output
	assert res.output.contains('Skipping download count increment for `pcre`.'), res.output
}

fn test_update_idents() {
	mut res := cmd_ok(@LOCATION, '${v} update pcre')
	assert res.output.contains('Updating module `pcre`'), res.output
	res = cmd_ok(@LOCATION, '${v} update nedpals.args vtray')
	assert res.output.contains('Updating module `vtray`'), res.output
	assert res.output.contains('Updating module `nedpals.args`'), res.output
	// Update installed module using its url.
	res = cmd_ok(@LOCATION, '${v} update https://github.com/spytheman/vtray')
	assert res.output.contains('Updating module `vtray`'), res.output
	// Try update not installed.
	res = cmd_fail(@LOCATION, '${v} update vsl')
	assert res.output.contains('failed to find `vsl`'), res.output
	// Try update mixed.
	res = cmd_fail(@LOCATION, '${v} update pcre vsl')
	assert res.output.contains('Updating module `pcre`'), res.output
	assert res.output.contains('failed to find `vsl`'), res.output
}
