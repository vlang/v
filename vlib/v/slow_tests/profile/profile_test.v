// vtest build: !musl?
import os
import time

const vexe = os.getenv('VEXE')

const vroot = os.dir(vexe)

fn test_vexe_exists() {
	assert vexe.len > 0
	assert os.is_file(vexe)
}

fn test_v_profile_works_when_interrupted() {
	println(@FN)
	$if windows {
		if os.getenv('VTEST_RUN_PROFILE_INTERRUPTION').int() == 0 {
			eprintln('> skipping ${@FN} on windows for now, since reading the output blocks currently')
			return
		}
	}
	sfile := 'vlib/v/slow_tests/profile/profile_test_interrupted.v'
	program_source := os.join_path(vroot, sfile)
	pid := os.getpid()
	program_exe := os.join_path(os.cache_dir(), 'profile_test_interrupted_pid_${pid}.exe')
	program_profile := os.join_path(os.cache_dir(), 'profile_test_interrupted_pid_${pid}.profile')
	os.rm(program_exe) or {}
	os.rm(program_profile) or {}
	os.chdir(vroot) or {}
	compile_cmd := '${os.quoted_path(vexe)} -skip-unused -o ${os.quoted_path(program_exe)} -profile ${os.quoted_path(program_profile)} ${os.quoted_path(program_source)}'
	eprintln('> compiling cmd: ${compile_cmd}')
	compilation_result := os.execute(compile_cmd)
	assert compilation_result.exit_code == 0, compilation_result.output
	eprintln('> compiled ${program_exe}')
	mut p := os.new_process(program_exe)
	p.set_redirect_stdio()
	p.run()
	eprintln('> started  ${program_exe} at: ${time.now().format_ss_micro()}')
	mut lines := []string{}
	for p.is_alive() && lines.len < 5 {
		if data := p.pipe_read(.stdout) {
			lines << data.trim_space().split_into_lines()
		}
		time.sleep(10 * time.millisecond)
	}
	dump(lines)
	assert lines.len > 4, lines.str()
	assert lines.contains('0003 iteration'), lines.str()
	eprintln('> stopping ${program_exe} at: ${time.now().format_ss_micro()}...')
	p.signal_term()
	eprintln('> waiting ${program_exe}')
	p.wait()
	eprintln('> closing ${program_exe}')
	p.close()
	assert p.code == 130
	assert p.status == .closed
	eprintln('> reading profile_content from ${program_profile} ...')
	profile_content := os.read_file(program_profile)!
	assert profile_content.contains('str_intp')
	assert profile_content.contains('println')
	assert profile_content.contains('time__sleep')
	assert profile_content.contains('main__main')
	// dump(profile_content)
	eprintln('-'.repeat(120))
}

fn test_v_profile_works() {
	println(@FN)
	sfile := 'vlib/v/slow_tests/profile/profile_test_1.v'
	validate_output(@FN, '', sfile, {
		'arguments':     1
		'main__main':    1
		'println':       1
		'strconv__atoi': 1
	})
}

fn test_v_profile_on_off_api_works() {
	println(@FN)
	sfile := 'vlib/v/slow_tests/profile/profile_test_2.v'
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
	println(@FN)
	sfile := 'vlib/v/slow_tests/profile/profile_test_3.v'
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
