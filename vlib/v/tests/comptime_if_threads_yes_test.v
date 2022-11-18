fn abc() {
	spawn fn () {}()
}

fn test_if_threads() {
	$if threads {
		assert true
	} $else {
		assert false
	}
}
