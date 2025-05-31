fn foo() !string {
	return 'abc'
}

// exists call_expr in the if-else branches and the left_expr of call_expr with or_block
fn test_if() {
	res := if false {
		''
	} else {
		foo() or { panic('error') }.trim('')
	}
	assert res == 'abc'
}

// exists call_expr in the match branches and the left_expr of call_expr with or_block
fn test_match() {
	res := match false {
		true {
			''
		}
		else {
			foo() or { panic('error') }.trim('')
		}
	}
	assert res == 'abc'
}
