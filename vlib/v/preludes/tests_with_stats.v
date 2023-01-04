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
	oks            u64
	fails          u64
	fn_fails       u64
	test_suit_file string
	step_func_name string
}

// ///////////////////////////////////////////////////////////////////
// Called at the start of the test program produced by `v -stats file_test.v`
fn start_testing(total_number_of_tests int, vfilename string) BenchedTests {
	mut benched_tests_res := BenchedTests{
		bench: benchmark.new_benchmark()
	}
	benched_tests_res.bench.set_total_expected_steps(total_number_of_tests)
	benched_tests_res.test_suit_file = vfilename
	println('running tests in: ${benched_tests_res.test_suit_file}')
	return benched_tests_res
}

// Called before each test_ function, defined in file_test.v
fn (mut b BenchedTests) testing_step_start(stepfunc string) {
	b.step_func_name = stepfunc.replace('main.', '').replace('__', '.')
	b.oks = test_runner.total_assert_passes
	b.fails = test_runner.total_assert_fails
	b.fn_fails = test_runner.fn_fails
	b.bench.step()
}

// Called after each test_ function, defined in file_test.v
fn (mut b BenchedTests) testing_step_end() {
	ok_diff := int(test_runner.total_assert_passes - b.oks)
	fail_diff := int(test_runner.total_assert_fails - b.fails)
	fn_fail_diff := int(test_runner.fn_fails - b.fn_fails)
	// ////////////////////////////////////////////////////////////////
	if ok_diff == 0 && fn_fail_diff == 0 {
		b.bench.neither_fail_nor_ok()
		println(inner_indent + b.bench.step_message_ok('   NO asserts | ') + b.fn_name())
		return
	}
	// ////////////////////////////////////////////////////////////////
	if ok_diff > 0 {
		b.bench.ok_many(ok_diff)
	}
	if fn_fail_diff > 0 {
		b.bench.fail_many(fn_fail_diff)
	}
	// ////////////////////////////////////////////////////////////////
	if fn_fail_diff > 0 {
		sfail_diff := nasserts(ok_diff + fail_diff)
		println(inner_indent + b.bench.step_message_fail(sfail_diff) + b.fn_name())
		return
	}
	if ok_diff > 0 {
		sok_diff := nasserts(ok_diff)
		println(inner_indent + b.bench.step_message_ok(sok_diff) + b.fn_name())
		return
	}
}

fn (b &BenchedTests) fn_name() string {
	return b.step_func_name + '()'
}

// Called at the end of the test program produced by `v -stats file_test.v`
fn (mut b BenchedTests) end_testing() {
	b.bench.stop()
	fname := os.file_name(b.test_suit_file)
	msg := 'running V tests in "${fname}"'
	final := inner_indent + b.bench.total_message(msg)
	println(final)
}

// ///////////////////////////////////////////////////////////////////
fn nasserts(n int) string {
	if n == 1 {
		return '${n:5d} assert  | '
	}
	return '${n:5d} asserts | '
}
