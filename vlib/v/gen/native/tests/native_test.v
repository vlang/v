// vtest build: (amd64 || arm64) && !self_sandboxed_packaging? && !gcc-windows && !native-backend-windows && !tcc-windows && !msvc-windows
// NOTE: native-backend-windows passed with the windows-2019 runner, but fails with windows-2022. TODO: fix
@[has_globals]
module main

import os
import time
import benchmark
import term
import log
import os.filelock

const is_verbose = os.getenv('VTEST_SHOW_CMD') != ''
const user_os = os.user_os()
const wrkdir = os.join_path(os.vtmp_dir(), 'native_tests')

const flock_path = os.join_path(os.vtmp_dir(), 'native_tests.lock')

__global flock_singleton = filelock.new(flock_path)

fn testsuite_begin() {
	log.info(@FN)
	if !flock_singleton.try_acquire() {
		log.info('>>>> skipping ${@FILE}, since it has been already started and ${flock_path} is present...')
		exit(0)
	}
	os.mkdir_all(wrkdir) or {}
	os.chdir(wrkdir) or {}
}

fn testsuite_end() {
	os.rmdir_all(wrkdir) or {}
	flock_singleton.release()
	log.info(@FN)
}

// TODO: some logic copy pasted from valgrind_test.v and compiler_test.v, move to a module
fn test_native() {
	$if arm64 {
		eprintln('>> skipping testing on ARM for now')
		return
	}
	if user_os in ['freebsd', 'openbsd'] {
		eprintln('>> skipping testing on FreeBSD/OpenBSD for now')
		return
	}

	mut bench := benchmark.new_benchmark()
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	dir := os.join_path(vroot, 'vlib', 'v', 'gen', 'native', 'tests')
	files := os.ls(dir) or { panic(err) }

	tests := files.filter(it.ends_with('.vv')).sorted()
	if tests.len == 0 {
		println('no native tests found')
		assert false
	}

	skip_vv := os.getenv('VNATIVE_SKIP_LIBC_VV') != ''

	bench.set_total_expected_steps(tests.len)
	for test in tests {
		if skip_vv && test in ['libc.vv', 'linux.vv'] {
			// TODO: remove the skip here, when the native backend is more advanced
			println('>>> SKIPPING ${test} since VNATIVE_SKIP_LIBC_VV is defined')
			continue
		}
		if test == 'fibonacci_native.vv' || test.contains('linux') {
			if user_os == 'windows' {
				println('>>> SKIPPING ${test} on windows for now')
				continue
			}
		}

		bench.step()
		full_test_path := os.real_path(os.join_path(dir, test))
		test_file_name := os.file_name(test)
		relative_test_path := full_test_path.replace(vroot + '/', '')
		work_test_path := os.join_path(wrkdir, test_file_name)
		exe_test_path := os.join_path(wrkdir, test_file_name + '.exe')
		tmperrfile := os.join_path(dir, test + '.tmperr')
		cmd := '${os.quoted_path(vexe)} -o ${os.quoted_path(exe_test_path)} -b native ${os.quoted_path(full_test_path)} -d no_backtrace -d custom_define 2> ${os.quoted_path(tmperrfile)}'
		if is_verbose {
			println(cmd)
		}

		sw_compile := time.new_stopwatch()
		res_native := os.execute(cmd)
		compile_time_ms := sw_compile.elapsed().milliseconds()
		if res_native.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail(cmd))

			if os.exists(tmperrfile) {
				err := os.read_file(tmperrfile) or { panic(err) }
				eprintln(err)
			}
			continue
		}

		sw_run := time.new_stopwatch()
		res := os.execute('${os.quoted_path(exe_test_path)} 2> ${os.quoted_path(tmperrfile)}')
		runtime_ms := sw_run.elapsed().milliseconds()
		if res.exit_code != 0 {
			bench.fail()
			eprintln(bench.step_message_fail('${full_test_path} failed to run, res.exit_code: ${res.exit_code} != 0 '))
			eprintln('> The failed program, produced this output:')
			eprintln('------------------------------------------------')
			eprintln(res.output)
			eprintln('------------------------------------------------')
			eprintln('> tmperrfile: ${tmperrfile}, exists: ${os.exists(tmperrfile)}, content:')
			errstr := os.read_file(tmperrfile) or { '' }
			eprintln('------------------------------------------------')
			eprintln(errstr)
			eprintln('------------------------------------------------')
			eprintln('')
			continue
		}

		mut expected := os.read_file(os.join_path(dir, test + '.out')) or { panic(err) }
		errfile := os.join_path(dir, test + '.err')
		if os.exists(errfile) {
			mut err_expected := os.read_file(errfile) or { panic(err) }
			err_expected = err_expected.trim_right('\r\n').replace('\r\n', '\n')
			errstr := os.read_file(tmperrfile) or {
				panic('${err}: ${os.quoted_path(exe_test_path)} 2> ${os.quoted_path(tmperrfile)}')
			}

			mut err_found := errstr.trim_right('\r\n').replace('\r\n', '\n')
			if err_expected != err_found {
				println(term.red('FAIL'))
				println('============')
				println('stderr expected: "${err_expected}" len=${err_expected.len}')
				println('============')
				println('stderr found:"${err_found}" len=${err_found.len}')
				println('============\n')
				bench.fail()
				continue
			}
		}

		os.rm(tmperrfile) or {}
		expected = expected.trim_right('\r\n').replace('\r\n', '\n')
		mut found := res.output.trim_right('\r\n').replace('\r\n', '\n')
		found = found.trim_space()
		if expected != found {
			eprintln(bench.step_message_fail('${full_test_path} did not match expected output: '))
			println('============')
			println('expected: "${expected}" len=${expected.len}')
			println('============')
			println('found:"${found}" len=${found.len}')
			println('============\n')
			bench.fail()
			continue
		}
		bench.ok()
		eprintln(bench.step_message_ok('${relative_test_path:-45} , took ${compile_time_ms:4}ms to compile, ${runtime_ms:4}ms to run'))
	} // for loop

	bench.stop()
	eprintln(term.h_divider('-'))
	eprintln(bench.total_message('native'))
	if bench.nfail > 0 {
		exit(1)
	}
}

fn test_prevent_could_not_find_symbols_regression() {
	res := os.execute('${os.quoted_path(@VEXE)} -b native ${os.quoted_path(os.join_path(@VROOT,
		'examples/hello_world.v'))}')
	assert !res.output.contains('CaptureStackBackTrace'), 'Test failed system unable to find symbol: CaptureStackBackTrace'
	assert !res.output.contains('__debugbreak'), 'Test failed system unable to find symbol: __debugbreak'
}
