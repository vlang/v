module main

// /////////////////////////////////////////////////////////////////////
// / This file will get compiled as a part of the same module,
// / in which a given _test.v file is, when v is given -stats argument
// / The methods defined here are called back by the test program's
// / main function, so that customizing the look & feel of the results
// / is easy, since it is done in normal V code, instead of in embedded C ...
// /////////////////////////////////////////////////////////////////////
import os
import benchmark

const (
	inner_indent = '     '
)

struct BenchedTests {
mut:
	bench          benchmark.Benchmark
	oks            int
	fails          int
	test_suit_file string
	step_func_name string
}

// ///////////////////////////////////////////////////////////////////
// Called at the start of the test program produced by `v -stats file_test.v`
fn start_testing(total_number_of_tests int, vfilename string) BenchedTests {
	mut b := BenchedTests{
		bench: benchmark.new_benchmark()
	}
	b.bench.set_total_expected_steps(total_number_of_tests)
	b.test_suit_file = vfilename
	println('running tests in: $b.test_suit_file')
	return b
}

// Called before each test_ function, defined in file_test.v
fn (mut b BenchedTests) testing_step_start(stepfunc string) {
	b.step_func_name = stepfunc.replace('main.', '').replace('__', '.')
	b.oks = C.g_test_oks
	b.fails = C.g_test_fails
	b.bench.step()
}

// Called after each test_ function, defined in file_test.v
fn (mut b BenchedTests) testing_step_end() {
	ok_diff := C.g_test_oks - b.oks
	fail_diff := C.g_test_fails - b.fails
	// ////////////////////////////////////////////////////////////////
	if ok_diff == 0 && fail_diff == 0 {
		b.bench.neither_fail_nor_ok()
		println(inner_indent + b.bench.step_message_ok('   NO asserts | ') + b.fn_name())
		return
	}
	// ////////////////////////////////////////////////////////////////
	if ok_diff > 0 {
		b.bench.ok_many(ok_diff)
	}
	if fail_diff > 0 {
		b.bench.fail_many(fail_diff)
	}
	// ////////////////////////////////////////////////////////////////
	if ok_diff > 0 && fail_diff == 0 {
		println(inner_indent + b.bench.step_message_ok(nasserts(ok_diff)) + b.fn_name())
		return
	}
	if fail_diff > 0 {
		println(inner_indent + b.bench.step_message_fail(nasserts(fail_diff)) + b.fn_name())
		return
	}
}

fn (b &BenchedTests) fn_name() string {
	return b.step_func_name + '()'
}

// Called at the end of the test program produced by `v -stats file_test.v`
fn (mut b BenchedTests) end_testing() {
	b.bench.stop()
	println(inner_indent + b.bench.total_message('running V tests in "' +
		os.file_name(b.test_suit_file) + '"'))
}

// ///////////////////////////////////////////////////////////////////
fn nasserts(n int) string {
	if n == 1 {
		return '${n:5d} assert  | '
	}
	return '${n:5d} asserts | '
}
