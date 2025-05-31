fn test_lambda_expr_with_result() {
	take_lambda(|| println('abc'))
	assert true
}

fn take_lambda(l fn () !) {
	l() or { panic(err) }
}
