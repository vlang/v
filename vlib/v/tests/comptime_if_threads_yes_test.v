fn abc() {
	go fn () {}()
}

fn test_if_threads() {
	$if threads {
		assert true
	} $else {
		assert false
	}
}
