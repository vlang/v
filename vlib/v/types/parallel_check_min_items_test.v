module types

import os
import time
import v.parser
import v.pref

// A diagnostics server lowers parallel_check_min_items so that the pool checks
// the bodies of a small program too; the diagnostics are those of one thread.

const small_program = 'module main

fn double(x int) int {
	return x * 2
}

fn label(x int) string {
	return double(x)
}

fn total(xs []int) int {
	mut sum := 0
	for x in xs {
		sum += double(x)
	}
	return sum
}

fn main() {
	println(label(total([1, 2, 3])))
}
'

// larger_program repeats the functions of small_program `copies` times, under
// names of their own: enough work for several workers of the pool.
fn larger_program(copies int) string {
	mut source := small_program
	for i in 0 .. copies {
		for name in ['double', 'label', 'total'] {
			body := small_program.all_after('fn ${name}(').all_before('\n}\n')
			source += '\nfn ${name}_${i}(${body.replace('double(', 'double_${i}(')}\n}\n'
		}
	}
	return source
}

fn check_small_program(min_items int) !(bool, []string) {
	return check_program(small_program, min_items)
}

fn check_program(source string, min_items int) !(bool, []string) {
	old_vjobs := os.getenv_opt('VJOBS')
	os.setenv('VJOBS', '4', true)
	defer {
		if value := old_vjobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
	}
	root := os.join_path(os.vtmp_dir(), 'v3 parallel min items ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, source)!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.diagnose_unknown_calls = true
	tc.parallel_check_min_items = min_items
	was_parallel := tc.check_semantics_opt(true)
	return was_parallel, tc.errors.map(it.msg)
}

fn test_a_small_program_is_checked_on_one_thread_by_default() {
	was_parallel, errors := check_small_program(min_parallel_check_items)!
	assert !was_parallel
	assert errors.any(it.contains('return')), errors.str()
}

fn test_a_lower_minimum_checks_a_program_on_the_pool_with_the_same_errors() {
	source := larger_program(40)
	_, serial_errors := check_program(source, min_parallel_check_items)!
	was_parallel, errors := check_program(source, 2)!
	$if windows {
		assert !was_parallel
	} $else {
		assert was_parallel
	}
	assert errors == serial_errors
}

fn test_a_program_with_little_work_is_checked_on_one_thread_even_with_a_lower_minimum() {
	was_parallel, errors := check_small_program(2)!
	assert !was_parallel
	assert errors.any(it.contains('return')), errors.str()
}

fn test_the_jobs_of_a_parallel_check_are_bounded_by_its_work() {
	items := []CheckWorkItem{len: 10, init: CheckWorkItem{
		cost: 50
	}}
	// 500 pays for one worker.
	assert parallel_check_jobs_for_cost(8, items) == 1
	assert parallel_check_jobs_for_cost(8, []CheckWorkItem{len: 10, init: CheckWorkItem{
		cost: 256
	}}) == 8
	assert parallel_check_jobs_for_cost(4, []CheckWorkItem{len: 3, init: CheckWorkItem{
		cost: 256
	}}) == 3
	assert parallel_check_jobs_for_cost(8, []CheckWorkItem{}) == 1
}
