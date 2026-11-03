module windows

import os

fn test_windows_workflow_validation_plans() {
	root := os.getenv_opt('V_CI_TEST_ROOT') or { @VEXEROOT }
	for compiler in ['tcc', 'gcc', 'msvc'] {
		job := read_job(root, compiler)!
		assert job.env['VFLAGS'].contains('-cc ${compiler}')
		assert job.env['VTEST_FAIL_FAST'] == '1'
		assert job.steps[0].id == 'all_code_is_formatted'
		self_tests := job.steps.filter(it.id == 'self_tests')
		assert self_tests.len == 1
		assert self_tests[0].script.trim_space() == 'v -silent test-self vlib'
		fastc := job.steps.filter(it.id == 'build_and_run_hello_world_with_b_fastc')
		assert fastc.len == 1
		assert 'VFLAGS' in fastc[0].env
		assert fastc[0].env['VFLAGS'] == ''
		assert !job.steps.any(it.id in ['install_dependencies', 'install_openssl', 'build',
			'build_with_makev_bat_tcc'])
		assert !job.steps.any(it.script.contains('v -autofree'))
	}
	if _ := read_job(root, 'unknown') {
		assert false, 'unknown compilers must fail before executing anything'
	}
}

fn test_step_execution_and_failure_propagation() {
	$if !windows {
		return
	}
	shell := os.find_abs_path_of_executable('pwsh')!
	dir := os.join_path(os.temp_dir(), "v ci's fixture ${os.getpid()}")
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	fixture := os.join_path(dir, 'fake v.cmd')
	os.write_file(fixture, '@echo off\r\nif "%1"=="fail" exit /b 7\r\necho %VFLAGS%:%1> observed.txt\r\nexit /b 0\r\n')!
	old_flags := os.getenv_opt('VFLAGS')
	old_marker := os.getenv_opt('V_WINDOWS_CI_FIXTURE')
	old_path := os.getenv('PATH')
	defer {
		if value := old_flags {
			os.setenv('VFLAGS', value, true)
		} else {
			os.unsetenv('VFLAGS')
		}
		if value := old_marker {
			os.setenv('V_WINDOWS_CI_FIXTURE', value, true)
		} else {
			os.unsetenv('V_WINDOWS_CI_FIXTURE')
		}
		os.setenv('PATH', old_path, true)
	}
	os.setenv('VFLAGS', 'job flags', true)
	execute_step(shell, dir, fixture, dir, Step{
		id:     'success'
		name:   'fixture'
		script: 'v hello\nAdd-Content -LiteralPath \$env:GITHUB_ENV -Value "V_WINDOWS_CI_FIXTURE=one=two"\nAdd-Content -LiteralPath \$env:GITHUB_PATH -Value ${ps_quote(dir)}'
		env:    {
			'VFLAGS': 'step flags'
		}
	})!
	assert os.read_file(os.join_path(dir, 'observed.txt'))!.trim_space() == 'step flags:hello'
	assert os.getenv('VFLAGS') == 'job flags'
	assert os.getenv('V_WINDOWS_CI_FIXTURE') == 'one=two'
	assert os.getenv('PATH') == dir + os.path_delimiter + old_path
	for command in ['v fail', '& ${ps_quote(fixture)} fail'] {
		if _ := execute_step(shell, dir, fixture, dir, Step{
			id:     'failure'
			name:   'fixture'
			script: '${command}\nSet-Content -LiteralPath reached -Value yes'
			env:    {
				'VFLAGS': ''
			}
		}) {
			assert false, 'nonzero exit must fail the step'
		}
		assert !os.exists(os.join_path(dir, 'reached'))
		assert os.getenv('VFLAGS') == 'job flags'
	}
}
