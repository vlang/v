/*
// TODO: Fix this.
import time

// using a manual temporary intermediate variable should always work:
fn test_call_fn_that_requires_reference_with_function_that_returns_a_struct_manual(){
	t1 := time.random()
	t2 := t1.calc_unix()
	println('tmp: $t2')
	assert true
}

// v should produce temporary intermediate variables in chained calls:
fn test_call_fn_that_requires_reference_with_function_that_returns_a_struct_chained(){
	res := (time.random().calc_unix())
	println('res: $res')
	assert true
}
*/

fn test_dummy(){}
