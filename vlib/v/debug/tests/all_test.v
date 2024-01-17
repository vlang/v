import os

const vroot = @VEXEROOT
// Expect has to be installed for the test.
const expect_exe = os.quoted_path(os.find_abs_path_of_executable('expect') or {
	eprintln('skipping test, since expect is missing')
	exit(0)
})
// Directory that contains the Expect scripts used in the test.
const expect_tests_path = os.join_path(@VEXEROOT, 'vlib', 'v', 'debug', 'tests')
// Running tests appends a tsession path to VTMP, which is automatically cleaned up after the test.
// The following will result in e.g. `$VTMP/tsession_7fe8e93bd740_1612958707536/test_vcreate_input/`.
const test_module_path = os.join_path(os.vtmp_dir(), 'test_vdbg_input')

fn prepare_test_path() ! {
	os.rmdir_all(test_module_path) or {}
	os.mkdir_all(test_module_path) or {}
	os.chdir(test_module_path)!
}

fn test_smartcast() {
	prepare_test_path()!
	test_file := os.join_path(@VEXEROOT, 'vlib', 'v', 'debug', 'tests', 'smartcast.vv')
	output_file := os.join_path(test_module_path, @FN)
	os.execute_opt('${expect_exe} ${os.join_path(expect_tests_path, 'smartcast.expect')} ${vroot} ${test_file} ${output_file}') or {
		assert false, err.msg()
	}
}

fn test_comptime_smartcast() {
	prepare_test_path()!
	test_file := os.join_path(@VEXEROOT, 'vlib', 'v', 'debug', 'tests', 'comptime_smartcast.vv')
	output_file := os.join_path(test_module_path, @FN)
	os.execute_opt('${expect_exe} ${os.join_path(expect_tests_path, 'comptime_smartcast.expect')} ${vroot} ${test_file} ${output_file}') or {
		assert false, err.msg()
	}
}

fn test_comptime_variant() {
	prepare_test_path()!
	test_file := os.join_path(@VEXEROOT, 'vlib', 'v', 'debug', 'tests', 'comptime_variant.vv')
	output_file := os.join_path(test_module_path, @FN)
	os.execute_opt('${expect_exe} ${os.join_path(expect_tests_path, 'comptime_variant.expect')} ${vroot} ${test_file} ${output_file}') or {
		assert false, err.msg()
	}
}