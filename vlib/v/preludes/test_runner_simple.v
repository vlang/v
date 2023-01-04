module main

// Provide a no-frills implementation of the TestRunner interface:

fn vtest_init() {
	change_test_runner(&TestRunner(SimpleTestRunner{}))
}

struct SimpleTestRunner {
mut:
	fname string
	//
	file_test_info   VTestFileMetaInfo
	fn_test_info     VTestFnMetaInfo
	fn_assert_passes u64
	fn_passes        u64
	fn_fails         u64
	//
	total_assert_passes u64
	total_assert_fails  u64
}

fn (mut runner SimpleTestRunner) free() {
	unsafe {
		runner.fname.free()
		runner.fn_test_info.free()
		runner.file_test_info.free()
	}
}

fn normalise_fname(name string) string {
	return 'fn ' + name.replace('__', '.').replace('main.', '')
}

fn (mut runner SimpleTestRunner) start(ntests int) {
	eprintln('SimpleTestRunner testing start; expected: ${ntests} test functions')
}

fn (mut runner SimpleTestRunner) finish() {
	eprintln('SimpleTestRunner testing finish; fn:[passes: ${runner.fn_passes}, fails: ${runner.fn_fails}], assert:[passes: ${runner.total_assert_passes}, fails: ${runner.total_assert_fails}]')
}

fn (mut runner SimpleTestRunner) exit_code() int {
	if runner.fn_fails > 0 {
		return 1
	}
	if runner.total_assert_fails > 0 {
		return 2
	}
	return 0
}

//

fn (mut runner SimpleTestRunner) fn_start() bool {
	runner.fn_assert_passes = 0
	runner.fname = normalise_fname(runner.fn_test_info.name)
	return true
}

fn (mut runner SimpleTestRunner) fn_pass() {
	runner.fn_passes++
}

fn (mut runner SimpleTestRunner) fn_fail() {
	runner.fn_fails++
	eprintln('>>> fail ${runner.fname}')
}

fn (mut runner SimpleTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	eprintln('>>> SimpleTestRunner fn_error ${runner.fname}, line_nr: ${line_nr}, file: ${file}, mod: ${mod}, fn_name: ${fn_name}, errmsg: ${errmsg}')
}

//

fn (mut runner SimpleTestRunner) assert_pass(i &VAssertMetaInfo) {
	runner.total_assert_passes++
	runner.fn_assert_passes++
	unsafe { i.free() }
}

fn (mut runner SimpleTestRunner) assert_fail(i &VAssertMetaInfo) {
	runner.total_assert_fails++
	eprintln('> failed assert ${runner.fn_assert_passes + 1} in ${runner.fname}, assert was in ${normalise_fname(i.fn_name)}, line: ${
		i.line_nr + 1}')
	unsafe { i.free() }
}
