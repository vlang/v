// vtest build: present_python? // the example only makes sense to be compiled, when python is installed
module test

// Note: compile this with `v -d no_backtrace -shared test.v`
import math

@[export: 'square']
fn square(i int) int {
	return i * i
}

@[export: 'sqrt_of_sum_of_squares']
fn sqrt_of_sum_of_squares(x f64, y f64) f64 {
	return math.sqrt(x * x + y * y)
}

// you do not have to use the same name in the export attribute
@[export: 'process_v_string']
fn work(s string) string {
	return 'v ${s} v'
}
