module windows

import os
import yaml

fn test_windows_workflow_validation_plans() {
	root := os.getenv_opt('V_CI_TEST_ROOT') or { @VEXEROOT }
	for compiler in ['tcc', 'gcc', 'msvc'] {
		job := read_job(root, compiler)!
		assert job.env['VFLAGS'].contains('-cc ${compiler}')
		assert job.env['VTEST_FAIL_FAST'] == '1'
		assert job.steps[0].id == 'all_code_is_formatted'
		self_tests := job.steps.filter(it.id == 'self_tests')
		if compiler == 'gcc' {
			// GCC's full V self-tests moved to the dedicated sharded job; they
			// no longer run in the regular gcc-windows validation worker.
			assert self_tests.len == 0
			doc := yaml.parse_file(os.join_path(root, '.github', 'workflows',
				'windows_ci_gcc.yml'))!
			shards := doc.value_opt('jobs.gcc-windows-self-tests')!
			mut native_self_tests := 0
			for value in shards.value('steps').array() {
				step := value.as_map()
				if command := step['run'] {
					if command.string().trim_space() == 'v -silent test-self vlib' {
						native_self_tests++
					}
				}
			}
			assert native_self_tests == 1
		} else {
			assert self_tests.len == 1
			assert self_tests[0].script.trim_space() == 'v -silent test-self vlib'
		}
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

fn test_msvc_probe_selection_includes_watchdog() {
	root := os.getenv_opt('V_CI_TEST_ROOT') or { @VEXEROOT }
	job := read_job(root, 'msvc')!
	probe := 'multiwindow_d3d11_warp_regressions_and_runtime_probes'
	preparation := 'prepare_multiwindow_job_object_watchdog'
	selected := job.select_steps(probe)!
	assert selected.map(it.id) == [preparation, probe]
	assert job.select_steps('ci')!.map(it.id) == job.steps.map(it.id)
	assert job.select_steps(preparation)!.map(it.id) == [preparation]
	assert job.select_steps('self_tests')!.map(it.id) == ['self_tests']
	for id in [probe, 'unknown'] {
		missing := Job{
			compiler: 'msvc'
			steps:    job.steps.filter(it.id != preparation)
		}
		if _ := missing.select_steps(id) {
			assert false, 'a missing step must fail before executing anything'
		}
	}
}

fn test_powershell_version_guard() {
	$if !windows {
		return
	}
	shell := os.find_abs_path_of_executable('pwsh')!
	dir := os.join_path(os.temp_dir(), 'v-ci-powershell-version-${os.getpid()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	marker := os.join_path(dir, 'reached')
	path := os.join_path(dir, 'version.ps1')
	for version, supported in {
		'7.3.12': false
		'7.4.0':  true
	} {
		// Override only the child's reported version to exercise the production guard.
		script := '\$PSVersionTable.PSVersion = [version]${ps_quote(version)}\n' +
			step_script(dir, os.join_path(dir, 'unused.exe'), os.join_path(dir, 'environment'),
				os.join_path(dir, 'path'), Step{
					script: 'Set-Content -LiteralPath ${ps_quote(marker)} -Value reached'
				})
		os.write_file(path, script)!
		status := os.system_args([shell, '-NoLogo', '-NoProfile', '-NonInteractive', '-ExecutionPolicy',
			'Bypass', '-File', path])
		assert (status == 0) == supported
		assert os.exists(marker) == supported
		if os.exists(marker) {
			os.rm(marker)!
		}
	}
}

fn test_selected_msvc_probe_prepares_watchdog_in_clean_directory() {
	$if !windows {
		return
	}
	root := os.getenv_opt('V_CI_TEST_ROOT') or { @VEXEROOT }
	job := read_job(root, 'msvc')!
	selected := job.select_steps('multiwindow_d3d11_warp_regressions_and_runtime_probes')!
	shell := os.find_abs_path_of_executable('pwsh')!
	dir := os.join_path(os.temp_dir(), 'v-ci-msvc-probe-${os.getpid()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	assert !os.exists(os.join_path(dir, 'multiwindow_ci_watchdog.exe'))
	for step in selected {
		// Exercise the real selection and execution with tiny scripts, without requiring MSVC.
		script := match step.id {
			'prepare_multiwindow_job_object_watchdog' {
				'Set-Content -LiteralPath (Join-Path \$env:RUNNER_TEMP multiwindow_ci_watchdog.exe) -Value fixture\nAdd-Content -LiteralPath order.txt -Value preparation'
			}
			'multiwindow_d3d11_warp_regressions_and_runtime_probes' {
				"if (-not (Test-Path -LiteralPath (Join-Path \$env:RUNNER_TEMP multiwindow_ci_watchdog.exe))) { throw 'watchdog was not prepared' }\nAdd-Content -LiteralPath order.txt -Value probe"
			}
			else {
				panic('unexpected selected step ${step.id}')
			}
		}
		execute_step(shell, dir, os.join_path(dir, 'unused.exe'), dir, Step{
			id:     step.id
			name:   step.name
			script: script
			env:    {
				'RUNNER_TEMP': dir
			}
		})!
	}
	assert os.read_lines(os.join_path(dir, 'order.txt'))! == ['preparation', 'probe']
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
