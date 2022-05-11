type FnCb = fn (a string, b int) ?

fn test_calling_an_anon_function_returning_question() {
	create_and_call_anon_function() or { panic(err) }
}

fn create_and_call_anon_function() ? {
	x := fn (a string, b int) ? {
		println('test')
		// Note: the anon function does NOT return explicitly,
		// so V should generate an implicit "OK" value and
		// return it. Previously, it created an implicit optional
		// filled with 0s => .ok was false, and that was treated
		// as a failure, triggering or blocks.
	}
	should_not_call_block(x)?
	assert true
}

fn should_not_call_block(func FnCb) ? {
	func('abc', 123) or {
		println('this should NOT be called')
		assert false
	}
}
