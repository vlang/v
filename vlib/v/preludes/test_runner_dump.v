module main

// This prelude file implements the Dump output producer for V tests.
// It is not useful by its own, but can verify that -test-runner works
// as expected. Use it with:
// `VTEST_RUNNER=dump v run file_test.v`
// or
// `v -test-runner dump run file_test.v`

fn vtest_init() {
	change_test_runner(&TestRunner(DumpTestRunner{}))
}

struct DumpTestRunner {
mut:
	fname        string
	plan_tests   int
	test_counter int

	file_test_info   VTestFileMetaInfo
	fn_test_info     VTestFnMetaInfo
	fn_assert_passes u64
	fn_passes        u64
	fn_fails         u64

	total_assert_passes u64
	total_assert_fails  u64
}

fn (mut runner DumpTestRunner) free() {
	unsafe {
		runner.fname.free()
		runner.fn_test_info.free()
		runner.file_test_info.free()
	}
}

fn (mut runner DumpTestRunner) start(ntests int) {
	runner.plan_tests = ntests
	eprintln('> ${@METHOD} | ntests: ${ntests}')
}

fn (mut runner DumpTestRunner) finish() {
	eprintln('> ${@METHOD} , ${runner.plan_tests} tests, ${runner.total_assert_fails +
		runner.total_assert_passes} assertions, ${runner.total_assert_fails} failures')
}

fn (mut runner DumpTestRunner) exit_code() int {
	eprintln('> ${@METHOD}')
	if runner.fn_fails > 0 {
		return 1
	}
	if runner.total_assert_fails > 0 {
		return 2
	}
	return 0
}

//

fn (mut runner DumpTestRunner) fn_start() bool {
	eprintln('> ${@METHOD}')
	runner.fn_assert_passes = 0
	runner.test_counter++
	runner.fname = runner.fn_test_info.name
	return true
}

fn (mut runner DumpTestRunner) fn_pass() {
	runner.fn_passes++
	eprintln('> ${@METHOD} OK ${runner.test_counter} - ${runner.fname}')
}

fn (mut runner DumpTestRunner) fn_fail() {
	eprintln('> ${@METHOD} NOT OK ${runner.test_counter} - ${runner.fname}')
	runner.fn_fails++
}

fn (mut runner DumpTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	eprintln('> ${@METHOD} test function propagated error: ${runner.fname}, line_nr: ${line_nr}, file: ${file}, mod: ${mod}, fn_name: ${fn_name}, errmsg: ${errmsg}')
}

//

fn (mut runner DumpTestRunner) assert_pass(i &VAssertMetaInfo) {
	eprintln('> ${@METHOD} ASSERT PASS')
	runner.total_assert_passes++
	runner.fn_assert_passes++
	unsafe { i.free() }
}

fn (mut runner DumpTestRunner) assert_fail(i &VAssertMetaInfo) {
	runner.total_assert_fails++
	eprintln('> ${@METHOD} ASSERT FAIL: ${runner.fn_assert_passes + 1} in ${runner.fname}, assert was in ${i.fn_name}, line: ${
		i.line_nr + 1}')
	unsafe { i.free() }
}
