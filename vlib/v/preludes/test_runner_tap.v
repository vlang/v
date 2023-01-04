module main

// TAP, the Test Anything Protocol, is a simple text-based interface
// between testing modules in a test harness.
// TAP started life as part of the test harness for Perl but now has
// implementations in C, C++, Python, PHP, Perl, Java, JavaScript,
// Go, Rust, and others.
// Consumers and producers do not have to be written in the same
// language to interoperate. It decouples the reporting of errors
// from the presentation of the reports.
// For more details: https://testanything.org/

// This file implements a TAP producer for V tests.
// You can use it with:
// `VTEST_RUNNER=tap v run file_test.v`
// or
// `v -test-runner tap run file_test.v`

fn vtest_init() {
	change_test_runner(&TestRunner(TAPTestRunner{}))
}

struct TAPTestRunner {
mut:
	fname        string
	plan_tests   int
	test_counter int
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

fn (mut runner TAPTestRunner) free() {
	unsafe {
		runner.fname.free()
		runner.fn_test_info.free()
		runner.file_test_info.free()
	}
}

fn normalise_fname(name string) string {
	return 'fn ' + name.replace('__', '.').replace('main.', '')
}

fn flush_println(s string) {
	println(s)
	flush_stdout()
}

fn (mut runner TAPTestRunner) start(ntests int) {
	runner.plan_tests = ntests
	flush_println('1..${ntests}')
}

fn (mut runner TAPTestRunner) finish() {
	flush_println('# ${runner.plan_tests} tests, ${runner.total_assert_fails +
		runner.total_assert_passes} assertions, ${runner.total_assert_fails} failures')
}

fn (mut runner TAPTestRunner) exit_code() int {
	if runner.fn_fails > 0 {
		return 1
	}
	if runner.total_assert_fails > 0 {
		return 2
	}
	return 0
}

//

fn (mut runner TAPTestRunner) fn_start() bool {
	runner.fn_assert_passes = 0
	runner.test_counter++
	runner.fname = normalise_fname(runner.fn_test_info.name)
	return true
}

fn (mut runner TAPTestRunner) fn_pass() {
	runner.fn_passes++
	flush_println('ok ${runner.test_counter} - ${runner.fname}')
}

fn (mut runner TAPTestRunner) fn_fail() {
	flush_println('not ok ${runner.test_counter} - ${runner.fname}')
	runner.fn_fails++
}

fn (mut runner TAPTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	flush_println('# test function propagated error: ${runner.fname}, line_nr: ${line_nr}, file: ${file}, mod: ${mod}, fn_name: ${fn_name}, errmsg: ${errmsg}')
}

//

fn (mut runner TAPTestRunner) assert_pass(i &VAssertMetaInfo) {
	runner.total_assert_passes++
	runner.fn_assert_passes++
	unsafe { i.free() }
}

fn (mut runner TAPTestRunner) assert_fail(i &VAssertMetaInfo) {
	runner.total_assert_fails++
	flush_println('# failed assert: ${runner.fn_assert_passes + 1} in ${runner.fname}, assert was in ${normalise_fname(i.fn_name)}, line: ${
		i.line_nr + 1}')
	unsafe { i.free() }
}
