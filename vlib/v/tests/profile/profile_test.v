import os

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

fn test_vexe_exists() {
	assert vexe.len > 0
	assert os.is_file(vexe)
}

fn test_v_profile_works() {
	sfile := 'vlib/v/tests/profile/profile_test_1.v'
	validate_output(@FN, '', sfile, {
		'os__init_os_args': 1
		'main__main':       1
		'println':          1
		'strconv__atoi':    1
	})
}

fn test_v_profile_on_off_api_works() {
	sfile := 'vlib/v/tests/profile/profile_test_2.v'
	res_lines := validate_output(@FN, '', sfile, {
		'builtin_init': 1
		'main__main':   1
		'main__abc':    1
	})
	// test that `-d no_profile_startup` *also* works:
	res2_lines := validate_output(@FN, '-d no_profile_startup', sfile, {
		'builtin_init': -1
		'main__main':   1
		'main__abc':    1
	})
	assert res_lines.len > res2_lines.len
	// dump(res2_lines)
}

fn test_v_profile_fns_option_works() {
	sfile := 'vlib/v/tests/profile/profile_test_3.v'
	validate_output(@FN, '-profile-fns println', sfile, {
		'main__main': -1
		'main__abc':  -1
		'main__xyz':  -1
		'println':    10
	})
	validate_output(@FN, '-profile-fns main__abc', sfile, {
		'main__main': -1
		'main__abc':  1
		'main__xyz':  2
		'println':    10
	})
	validate_output(@FN, '-profile-fns main__xyz', sfile, {
		'main__main': -1
		'main__abc':  -1
		'main__xyz':  2
		'println':    10
	})
}

fn counter_value(lines []string, what string) int {
	res := lines.filter(it.contains(what))
	if res.len == 0 {
		return -1
	}
	return res[0].trim_space().all_before(' ').int()
}

fn validate_output(fn_name string, vopts string, fsource string, expected map[string]int) []string {
	println('> validating ${fn_name} with: `v ${vopts} -profile - run ${fsource}`')
	os.chdir(vroot) or {}
	program_source := os.join_path(vroot, fsource)
	res := os.execute('${os.quoted_path(vexe)} ${vopts} -profile - run ${os.quoted_path(program_source)}')
	assert res.exit_code == 0
	assert res.output.len > 0
	res_lines := res.output.split_into_lines()
	for expected_counter_name, expected_counter_value in expected {
		produced_value := counter_value(res_lines, expected_counter_name)
		println('    counter_name: ${expected_counter_name}')
		assert produced_value == expected_counter_value
	}
	return res_lines
}
