import os

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

fn test_vexe_exists() {
	assert vexe.len > 0
	assert os.is_file(vexe)
}

fn test_v_profile_works() {
	os.chdir(vroot)
	program_source := os.join_path(vroot, 'vlib/v/tests/profile/profile_test_1.v')
	res := os.exec('"$vexe" -profile - run $program_source') or { exit(1) }
	// eprintln('res: $res')
	assert res.exit_code == 0
	assert res.output.len > 0
	assert res.output.contains(' os__init_os_args')
	assert res.output.contains(' main__main')
	assert res.output.contains(' println')
	assert res.output.contains(' strconv__atoi')
}
