module main

import os

const shared_test_source = os.real_path(os.join_path(@VEXEROOT, 'vlib', 'v', 'v.v'))
const shared_test_vlib = os.dir(os.dir(shared_test_source))

fn shared_test_output(args []string) string {
	return shared_test_compiler_output(args, shared_test_source) or { '' }
}

fn test_plain_builds_of_the_shared_source_are_served() {
	src := shared_test_source
	vlib := shared_test_vlib
	assert shared_test_output(['-gc', 'none', '-path', vlib, '-o', '/tmp/v3', src]) == '/tmp/v3'
	assert shared_test_output(['-gc', 'none', '-path', '${vlib}|@vlib|@vmodules', '-o', 'v3', src]) == 'v3'
	assert shared_test_output(['-gc', 'none', '-o', '/tmp/v3', src]) == '/tmp/v3'
	// VFLAGS='-gc none' is merged in front of the explicit options.
	assert shared_test_output(['-gc', 'none', '-gc', 'none', '-o', '/tmp/v3', src]) == '/tmp/v3'
}

fn test_builds_that_can_differ_from_the_shared_compiler_are_not_served() {
	src := shared_test_source
	vlib := shared_test_vlib
	assert shared_test_output(['-o', '/tmp/v3', src]) == ''
	assert shared_test_output(['-gc', 'boehm', '-o', '/tmp/v3', src]) == ''
	assert shared_test_output(['-gc', 'none', '-d', 'ownership', '-o', '/tmp/v3', src]) == ''
	assert shared_test_output(['-gc', 'none', '-prealloc', '-o', '/tmp/v3', src]) == ''
	assert shared_test_output(['-gc', 'none', '-path', os.temp_dir(), '-o', '/tmp/v3', src]) == ''
	assert shared_test_output(['-gc', 'none', '-path', '@vmodules|${vlib}', '-o', '/tmp/v3', src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', '/tmp/v3.c', src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', '/tmp/v3', '-o', '/tmp/v4', src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', '/tmp/v3', src, src]) == ''
	assert shared_test_output(['-gc', 'none', '-o', '/tmp/v3', os.join_path(vlib, 'v', 'v3.v')]) == ''
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
	shared := os.join_path(dir, 'shared_v3')
	os.write_file(shared, 'shared compiler')!
	os.chmod(shared, 0o755)!
	output := os.join_path(dir, 'test_v3')
	args := ['-gc', 'none', '-o', output, shared_test_source]
	assert !reuse_shared_test_compiler(args)
	os.setenv(shared_test_compiler_env, shared, true)
	os.setenv(shared_test_compiler_source_env, shared_test_source, true)
	// An existing output is replaced, not written through.
	os.link(shared, output)!
	assert reuse_shared_test_compiler(args)
	assert os.read_file(output)! == 'shared compiler'
	assert os.is_executable(output)
	os.write_file(output, 'changed by the test')!
	assert os.read_file(shared)! == 'shared compiler'
	assert os.read_lines(shared + '.reused')! == [output]
	assert !reuse_shared_test_compiler(['-gc', 'none', '-d', 'x', '-o', output, shared_test_source])
	assert os.read_lines(shared + '.reused')! == [output]
}
