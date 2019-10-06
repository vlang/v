module main

import os
//import time
import benchmark
import term

struct BenchedTests {
mut:
	oks int
	fails int
	test_suit_file string
	step_func_name string
	bench benchmark.Benchmark
}

fn nasserts(n int) string {
	if n==0 { return '${n:2d} asserts | ' }
	if n==1 { return '${n:2d} assert  | ' }
	return '${n:2d} asserts | '
}

fn ok_text(s string) string {
	if term.can_show_color_on_stdout() {
		return term.bold(term.green('${s:5s}'))
	}
	return s
}
fn fail_text(s string) string {
	if term.can_show_color_on_stdout() {
		return term.bold(term.red('${s:5s}'))
	}
	return s
}

fn (b BenchedTests) str() string {
	return 'BenchedTests{ bench: ' + ptr_str(&b.bench) + ' }'
}
////////////////////////////////////////////////////////////////
fn start_testing() BenchedTests {	
	mut b := BenchedTests{ bench: benchmark.new_benchmark() }
	b.test_suit_file = os.executable() + '.v'
	println('running tests in: $b.test_suit_file')
	return b
}

fn (b mut BenchedTests) testing_step_start(stepfunc string) {
	b.step_func_name = stepfunc.replace('main__','')
	b.oks   = C.g_test_oks
	b.fails = C.g_test_fails
	b.bench.step()
}
fn (b mut BenchedTests) testing_step_end() {
	ok_diff   := C.g_test_oks - b.oks
	fail_diff := C.g_test_fails - b.fails
	//////////////////////////////////////////////////////////////////
	if ok_diff == 0 && fail_diff == 0 {
		b.bench.neither_fail_nor_ok()
		println('     ' + b.bench.step_message('NO asserts | ') + 'fn: ' + b.step_func_name)
		return
	}	
	//////////////////////////////////////////////////////////////////
	if ok_diff   > 0 { b.bench.ok_many(ok_diff) }
	if fail_diff > 0 { b.bench.fail_many(fail_diff) }
	//////////////////////////////////////////////////////////////////	
	if ok_diff   > 0 && fail_diff == 0 {
		println(ok_text('OK') + b.bench.step_message(nasserts(ok_diff)) + 'fn: ' + b.step_func_name )
		return
	}
	if fail_diff > 0 {	
		println(fail_text('FAIL') + b.bench.step_message(nasserts(fail_diff)) + 'fn: ' + b.step_func_name )
		return
	}
}

fn (b mut BenchedTests) end_testing() {
	b.bench.stop()
	println( '     ' + b.bench.total_message('running V tests in $b.test_suit_file') )
}
