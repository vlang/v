import os

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

fn test_vexe_exists() {
	assert vexe.len > 0
	assert os.is_file(vexe)
}

fn test_v_profile_works() {
	os.chdir(vroot) or {}
	program_source := os.join_path(vroot, 'vlib/v/tests/profile/profile_test_1.v')
	res := os.execute('${os.quoted_path(vexe)} -profile - run ${os.quoted_path(program_source)}')
	// eprintln('res: $res')
	assert res.exit_code == 0
	assert res.output.len > 0
	assert res.output.contains(' os__init_os_args')
	assert res.output.contains(' main__main')
	assert res.output.contains(' println')
	assert res.output.contains(' strconv__atoi')
}

fn test_v_profile_on_off_api_works() {
	os.chdir(vroot) or {}
	program_source := os.join_path(vroot, 'vlib/v/tests/profile/profile_test_2.v')
	res := os.execute('${os.quoted_path(vexe)} -profile - run ${os.quoted_path(program_source)}')
	// eprintln('res: $res')
	assert res.exit_code == 0
	assert res.output.len > 0
	assert res.output.contains(' builtin_init')
	assert res.output.contains(' main__main')
	assert res.output.contains(' main__abc')
	res_lines := res.output.split_into_lines()
	abc_count := res_lines.filter(it.contains('main__abc'))[0].trim_space().all_before(' ').int()
	assert abc_count == 1

	// test that `-d no_profile_startup` *also* works:
	res2 := os.execute('${os.quoted_path(vexe)} -d no_profile_startup -profile - run ${os.quoted_path(program_source)}')
	assert res2.exit_code == 0
	assert res2.output.len > 0
	assert !res2.output.contains(' builtin_init')
	assert res2.output.contains(' main__main')
	assert res2.output.contains(' main__abc')
	res2_lines := res2.output.split_into_lines()
	assert res_lines.len > res2_lines.len
	// dump(res2_lines)
}
