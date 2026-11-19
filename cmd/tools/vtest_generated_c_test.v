import os
import time
import v.cmdexec

// The module cache is not what these tests check, so one cache, outside every
// environment's root, spares each build from compiling builtin from scratch.
const test_c_module_cache = os.join_path(os.vtmp_dir(), 'test c cleanup cache ${os.getpid()}')

fn testsuite_end() {
	os.rmdir_all(test_c_module_cache) or {}
}

struct TestCEnvironment {
	root     string
	previous map[string]string
}

fn new_test_c_environment() !TestCEnvironment {
	root := os.join_path(os.vtmp_dir(), 'test c cleanup ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	mut previous := map[string]string{}
	for name in ['VFLAGS', 'VOSARGS', 'VTMP', 'V3CACHE', 'VTEST_KEEP_SESSION', 'VTEST_ONLY_FN',
		'VTEST_ONLY', 'V_MACOS_V3_NO_FALLBACK'] {
		if value := os.getenv_opt(name) {
			previous[name] = value
		}
		os.unsetenv(name)
	}
	os.setenv('VTMP', os.join_path(root, 'tmp'), true)
	os.setenv('V3CACHE', test_c_module_cache, true)
	os.setenv('VTEST_KEEP_SESSION', '1', true)
	os.setenv('V_MACOS_V3_NO_FALLBACK', '1', true)
	return TestCEnvironment{root, previous}
}

fn (e TestCEnvironment) restore() {
	for name in ['VFLAGS', 'VOSARGS', 'VTMP', 'V3CACHE', 'VTEST_KEEP_SESSION', 'VTEST_ONLY_FN',
		'VTEST_ONLY', 'V_MACOS_V3_NO_FALLBACK'] {
		if value := e.previous[name] {
			os.setenv(name, value, true)
		} else {
			os.unsetenv(name)
		}
	}
	os.rmdir_all(e.root) or { panic(err) }
}

fn run_test_c(args []string) os.Result {
	// Let V select an available compiler, including bundled TCC.
	mut options := ['-new-compiler', '-no-retry-compilation']
	options << args
	return cmdexec.run_with_timeout(@VEXE, options, 120_000)
}

fn test_direct_tests_discard_generated_c_after_success_and_failure() ! {
	for backend_flags in [[]string{}, ['-b', 'c']] {
		for passing in [true, false] {
			e := new_test_c_environment()!
			defer { e.restore() }
			source := os.join_path(e.root, 'example_test.v')
			os.write_file(source, 'fn test_example() { assert ${passing} }\n')!
			mut args := backend_flags.clone()
			args << source
			result := run_test_c(args)
			assert result.exit_code == if passing { 0 } else { 1 }, result.output
			assert !os.exists(source.all_before_last('.v') + '.c'), result.output
			assert os.walk_ext(e.root, '.tmp.c').len == 0, result.output
		}
	}
}

fn test_vtest_discards_generated_c_even_in_kept_sessions() ! {
	for passing in [true, false] {
		e := new_test_c_environment()!
		defer { e.restore() }
		source := os.join_path(e.root, 'example_test.v')
		os.write_file(source, 'fn test_example() { assert ${passing} }\n')!
		target := if passing { e.root } else { source }
		result := run_test_c(['-b', 'c', 'test', target])
		assert result.output.contains(if passing { '1 passed' } else { '1 failed' }), result.output
		assert result.exit_code == if passing { 0 } else { 1 }, result.output
		assert os.walk_ext(os.join_path(e.root, 'tmp'), '.c').len == 0, result.output
		assert !os.exists(source.all_before_last('.v') + '.c'), result.output
	}
}

fn test_test_builds_preserve_explicit_c_requests_and_existing_files() ! {
	e := new_test_c_environment()!
	defer { e.restore() }
	source := os.join_path(e.root, 'example_test.v')
	os.write_file(source, 'fn test_example() { assert true }\n')!
	// The test runner compiles with -skip-running and an explicit binary output.
	binary := os.join_path(e.root, 'test-output')
	built := run_test_c(['-b', 'c', '-skip-running', '-o', binary, source])
	assert built.exit_code == 0, built.output
	assert !os.exists(binary + '.c'), built.output
	os.write_file(binary + '.c', 'existing user file')!
	rebuilt := run_test_c(['-b', 'c', '-skip-running', '-o', binary, source])
	assert rebuilt.exit_code == 0, rebuilt.output
	assert os.read_file(binary + '.c')! == 'existing user file'
	kept := run_test_c(['-keepc', source])
	assert kept.exit_code == 0, kept.output
	assert os.walk_ext(os.join_path(e.root, 'tmp'), '.tmp.c').len > 0, kept.output
	explicit_c := os.join_path(e.root, 'requested.c')
	generated := run_test_c(['-o', explicit_c, source])
	assert generated.exit_code == 0, generated.output
	assert os.is_file(explicit_c), generated.output
}

fn test_explicit_backend_still_retains_c_for_ordinary_programs() ! {
	e := new_test_c_environment()!
	defer { e.restore() }
	source := os.join_path(e.root, 'hello.v')
	os.write_file(source, 'fn main() {}\n')!
	result := run_test_c(['-b', 'c', source])
	assert result.exit_code == 0, result.output
	assert os.is_file(source.all_before_last('.v') + '.c'), result.output
}
