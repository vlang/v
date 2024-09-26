module main

fn foo() !int {
	return 5
}

fn test_for_c_stmt_with_result_call() {
	mut pos := 0

	for ; pos > 10; pos = foo()! {
		println(pos)
	}

	for pos = foo()!; pos > 10; {
		println(pos)
	}

	assert true
}
