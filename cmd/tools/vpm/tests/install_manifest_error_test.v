import os
import v.cmdexec

fn test_install_reports_a_broken_project_v_mod_instead_of_panicking() {
	dir := os.join_path(os.vtmp_dir(), 'vpm_install_manifest_error_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'v.mod'), "Module { name: 'x' dependencies: [broken }")!
	modules := os.join_path(dir, 'modules')

	res := cmdexec.run_in_merged_with_env(@VEXE, ['install'], dir, {
		'VMODULES':           modules
		'VTMP':               os.join_path(modules, 'vtmp')
		'VPM_LOCAL_INSTALLS': os.join_path(modules, 'local_installs')
		'VPM_DEBUG':          ''
		'VPM_NO_INCREMENT':   '1'
		'VPM_FAIL_ON_PROMPT': '1'
	})
	assert res.exit_code != 0, res.output
	assert !res.output.contains('V panic'), res.output
	assert res.output.contains('failed to read `./v.mod`'), res.output
}
