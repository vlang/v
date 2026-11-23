module main

import os

const shared_test_source = os.real_path(os.join_path(@VEXEROOT, 'vlib', 'v', 'v.v'))
const shared_test_vlib = os.dir(os.dir(shared_test_source))
const shared_test_exe_ext = $if windows { '.exe' } $else { '' }

fn shared_test_output(args []string) string {
	return shared_test_compiler_output(args, shared_test_source) or { '' }
}

fn test_plain_builds_of_the_shared_source_are_served() {
	src := shared_test_source
	vlib := shared_test_vlib
	output := os.join_path(os.temp_dir(), 'v3${shared_test_exe_ext}')
	relative_output := 'v3${shared_test_exe_ext}'
	assert shared_test_output(['-gc', 'none', '-path', vlib, '-o', output, src]) == output
	assert shared_test_output(['-gc', 'none', '-path', '${vlib}|@vlib|@vmodules', '-o',
		relative_output, src]) == relative_output
	assert shared_test_output(['-gc', 'none', '-o', output, src]) == output
	// VFLAGS='-gc none' is merged in front of the explicit options.
	assert shared_test_output(['-gc', 'none', '-gc', 'none', '-o', output, src]) == output
}

fn test_builds_that_can_differ_from_the_shared_compiler_are_not_served() {
	src := shared_test_source
	vlib := shared_test_vlib
	output := os.join_path(os.temp_dir(), 'v3${shared_test_exe_ext}')
	assert shared_test_output(['-o', output, src]) == ''
	assert shared_test_output(['-gc', 'boehm', '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-d', 'ownership', '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-prealloc', '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-prod', '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-cc', 'clang', '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-selfhost', '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-path', os.temp_dir(), '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-path', '@vmodules|${vlib}', '-o', output, src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', '/tmp/v3.c', src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', output, '-o', 'v4${shared_test_exe_ext}', src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', output, src, src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', output, os.join_path(vlib, 'v', 'v3.v')]) == ''
	assert shared_test_output(['-gc', 'none', 'run', src]) == ''
	assert shared_test_output(['-gc', 'none', '-o']) == ''
}

fn test_a_served_build_gets_its_own_copy_of_the_shared_compiler() {
	dir := os.join_path(os.vtmp_dir(), 'shared_test_compiler_${os.getpid()}')
	os.mkdir_all(dir)!
	// A test runner may share a compiler with this process; start without one.
	old_compiler := os.getenv_opt(shared_test_compiler_env)
	old_source := os.getenv_opt(shared_test_compiler_source_env)
	os.unsetenv(shared_test_compiler_env)
	os.unsetenv(shared_test_compiler_source_env)
	defer {
		os.rmdir_all(dir) or {}
		if value := old_compiler {
			os.setenv(shared_test_compiler_env, value, true)
		} else {
			os.unsetenv(shared_test_compiler_env)
		}
		if value := old_source {
			os.setenv(shared_test_compiler_source_env, value, true)
		} else {
			os.unsetenv(shared_test_compiler_source_env)
		}
	}
	shared_compiler := os.join_path(dir, 'shared_v3${shared_test_exe_ext}')
	os.write_file(shared_compiler, 'shared compiler')!
	os.chmod(shared_compiler, 0o755)!
	output := os.join_path(dir, 'test_v3${shared_test_exe_ext}')
	args := ['-gc', 'none', '-o', output, shared_test_source]
	assert !reuse_shared_test_compiler(args)
	os.setenv(shared_test_compiler_env, shared_compiler, true)
	os.setenv(shared_test_compiler_source_env, shared_test_source, true)
	// An existing output is replaced, not written through.
	os.link(shared_compiler, output)!
	assert reuse_shared_test_compiler(args)
	assert os.read_file(output)! == 'shared compiler'
	assert os.is_executable(output)
	os.write_file(output, 'changed by the test')!
	assert os.read_file(shared_compiler)! == 'shared compiler'
	assert os.read_lines(shared_compiler + '.reused')! == [output]
	assert !reuse_shared_test_compiler(['-gc', 'none', '-d', 'x', '-o', output, shared_test_source])
	assert os.read_lines(shared_compiler + '.reused')! == [output]
}

fn test_ownership_builds_are_served_only_by_a_shared_ownership_compiler() {
	plain, ownership := shared_test_compiler_variant(['-gc', 'none', '-d', 'ownership', '-o', 'v3',
		'v.v'])
	assert ownership
	assert plain == ['-gc', 'none', '-o', 'v3', 'v.v']
	_, twice := shared_test_compiler_variant(['-d', 'ownership', '-d', 'ownership', 'v.v'])
	assert !twice
	_, other := shared_test_compiler_variant(['-d', 'parallel', 'v.v'])
	assert !other
	dir := os.join_path(os.vtmp_dir(), 'shared_test_ownership_compiler_${os.getpid()}')
	os.mkdir_all(dir)!
	names := [shared_test_compiler_env, shared_test_ownership_compiler_env,
		shared_test_compiler_source_env]
	mut old_values := map[string]string{}
	for name in names {
		if value := os.getenv_opt(name) {
			old_values[name] = value
		}
		os.unsetenv(name)
	}
	defer {
		os.rmdir_all(dir) or {}
		for name in names {
			if value := old_values[name] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	plain_compiler := os.join_path(dir, 'shared_v3${shared_test_exe_ext}')
	ownership_compiler := os.join_path(dir, 'shared_v3_ownership${shared_test_exe_ext}')
	os.write_file(plain_compiler, 'plain compiler')!
	os.write_file(ownership_compiler, 'ownership compiler')!
	os.chmod(plain_compiler, 0o755)!
	os.chmod(ownership_compiler, 0o755)!
	output := os.join_path(dir, 'test_v3${shared_test_exe_ext}')
	args := ['-gc', 'none', '-d', 'ownership', '-o', output, shared_test_source]
	os.setenv(shared_test_compiler_env, plain_compiler, true)
	os.setenv(shared_test_compiler_source_env, shared_test_source, true)
	// The plain compiler never serves an ownership build.
	assert !reuse_shared_test_compiler(args)
	os.setenv(shared_test_ownership_compiler_env, ownership_compiler, true)
	assert reuse_shared_test_compiler(args)
	assert os.read_file(output)! == 'ownership compiler'
	assert reuse_shared_test_compiler(['-gc', 'none', '-o', output, shared_test_source])
	assert os.read_file(output)! == 'plain compiler'
}
