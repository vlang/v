fn abc() {
	// go fn() {}()
}

fn test_if_threads() {
	$if threads {
		assert false
	} $else {
		assert true
	}
}
