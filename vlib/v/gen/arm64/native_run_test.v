module arm64

import os

const native_test_compiler = os.join_path(os.vtmp_dir(), 'arm64_run_compiler_${os.getpid()}')

fn testsuite_end() {
	os.rm(native_test_compiler) or {}
}

// exec_native runs a compiler command. `cmd/v` leaves the ARM64 backend out by default, so
// when @VEXE reports that, the command runs with a compiler built here that includes it.
fn exec_native(args []string) os.Result {
	if !os.exists(native_test_compiler) {
		result := os.exec(args)
		if !result.output.contains('ARM64 support is not compiled into this executable') {
			return result
		}
		bootstrap := os.exec([@VEXE, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend', 'arm64',
			'-o', native_test_compiler, os.join_path(@VEXEROOT, 'cmd', 'v')])
		assert bootstrap.exit_code == 0, bootstrap.output
	}
	return os.exec(args.map(if it == @VEXE { native_test_compiler } else { it }))
}

fn test_native_run_forwards_arguments_and_exit_status() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_run_dispatch_${os.getpid()}.v')
		output := path.all_before_last('.')
		kept_output := output + '_kept'
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
			os.rm(kept_output) or {}
		}
		os.write_file(path, r'module main
fn main() {
    args := arguments()
    if args.len != 3 || args[1] != "carried" { exit(23) }
    println("native run: " + args[1])
    if args[2] == "fail" { exit(17) }
}
')!
		command := ['env', 'VFLAGS=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc', 'none', '-nocache',
			'-b', 'arm64']
		for expected_exit in [0, 17] {
			mut args := command.clone()
			args << ['run', path, 'carried', if expected_exit == 0 { 'pass' } else { 'fail' }]
			result := exec_native(args)
			assert result.exit_code == expected_exit, result.output
			assert result.output.contains('native run: carried'), result.output
			assert !os.exists(output), 'run must remove its temporary binary'
		}
		mut args := command.clone()
		args << ['-o', kept_output, 'run', path, 'carried', 'pass']
		result := exec_native(args)
		assert result.exit_code == 0, result.output
		assert result.output.contains('native run: carried'), result.output
		assert os.exists(kept_output), 'an explicit output must survive run'
	}
}

fn test_native_test_compilation_runs_the_test_binary() {
	$if macos && arm64 {
		path := os.join_path(os.vtmp_dir(), 'arm64_test_dispatch_${os.getpid()}_test.v')
		defer {
			os.rm(path) or {}
			os.rm(path.all_before_last('.')) or {}
		}
		os.write_file(path, r'fn init() { println("module init") }
fn testsuite_begin() { println("suite begin") }
fn testsuite_end() { println("suite end") }
fn before_each() { println("before each") }
fn after_each() { println("after each") }
fn test_native_entrypoint() {
    println("native test executed")
    assert 7 * 6 == 42
}
fn test_second() { println("second test executed") }
')!
		command := ['env', 'VFLAGS=', 'VTEST_ONLY_FN=', 'V_MACOS_V3_NO_FALLBACK=1', @VEXE, '-gc',
			'none', '-nocache', '-b', 'arm64']
		mut all_tests := command.clone()
		all_tests << path
		result := exec_native(all_tests)
		assert result.exit_code == 0, result.output
		assert result.output.contains('module init\nsuite begin\nbefore each\nnative test executed\nafter each\nbefore each\nsecond test executed\nafter each\nsuite end'), result.output
		for pattern in ['test_native*', 'main.test_native*'] {
			mut selected_tests := command.clone()
			selected_tests << ['-run-only', pattern, path]
			selected := exec_native(selected_tests)
			assert selected.exit_code == 0, selected.output
			assert selected.output.contains('native test executed'), selected.output
			assert !selected.output.contains('second test executed'), selected.output
			assert selected.output.contains('suite end'), selected.output
		}
		os.write_file(path, r'fn test_optional() ? { println("optional passed") }
fn test_result() ! { println("result passed") }
')!
		returned := exec_native(all_tests)
		assert returned.exit_code == 0, returned.output
		assert returned.output.contains('optional passed\nresult passed'), returned.output
		os.write_file(path, r'fn test_optional() ? { return none }
')!
		propagation_failure := exec_native(all_tests)
		assert propagation_failure.exit_code != 0, propagation_failure.output
		assert propagation_failure.output.contains('failed propagation'), propagation_failure.output
		os.write_file(path, r'fn test_result() ! { return error("expected failure") }
')!
		result_failure := exec_native(all_tests)
		assert result_failure.exit_code != 0, result_failure.output
		assert result_failure.output.contains('failed propagation'), result_failure.output
		os.write_file(path, r'fn test_native_entrypoint() { assert 7 * 6 == 41 }
')!
		failure := exec_native(all_tests)
		assert failure.exit_code != 0, failure.output
	}
}

fn test_native_self_launcher_executes_its_helper() {
	$if macos && arm64 {
		result := exec_native(['env', 'VFLAGS=', 'VTOOLS_NO_CACHE=1', 'V_MACOS_V3_NO_FALLBACK=1',
			'VSELF_SHOULD_FAIL=1', @VEXE, '-b', 'arm64', 'self', 'x2'])
		assert result.exit_code != 0, result.output
		assert result.output.contains('v self failed'), result.output
	}
}
