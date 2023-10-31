import time.misc as tmisc

// using a manual temporary intermediate variable should always work:
fn test_call_fn_that_requires_reference_with_function_that_returns_a_struct_manual() {
	t1 := tmisc.random()
	t2 := t1.unix_time()
	println('res: ${t2}')
	assert true
}

/*
// TODO: Fix this.
// v should produce temporary intermediate variables in chained calls:
fn test_call_fn_that_requires_reference_with_function_that_returns_a_struct_chained() {
	res := (tmisc.random().unix_time())
	println('res: $res')
	assert true
}
*/
