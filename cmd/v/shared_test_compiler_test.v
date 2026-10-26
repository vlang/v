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
	defer {
		os.rmdir_all(dir) or {}
		os.unsetenv(shared_test_compiler_env)
		os.unsetenv(shared_test_compiler_source_env)
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
