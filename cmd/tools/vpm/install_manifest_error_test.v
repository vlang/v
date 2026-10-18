import os

fn test_install_reports_a_broken_project_v_mod_instead_of_panicking() {
	dir := os.join_path(os.vtmp_dir(), 'vpm_install_manifest_error_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	origin := os.getwd()
	os.chdir(dir)!
	defer {
		os.chdir(origin) or {}
	}
	os.write_file('v.mod', "Module { name: 'x' dependencies: [broken }")!

	res := os.exec([@VEXE, 'install'])
	assert res.exit_code != 0, res.output
	assert !res.output.contains('V panic'), res.output
	assert res.output.contains('failed to read `./v.mod`'), res.output
}
