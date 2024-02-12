fn test_is_windows() {
	$if windows {
		assert true
	} $else {
		assert false
	}
}
