fn test_is_windows() {
	$if windows {
		assert true
	} $else {
		assert false, 'platform-specific test filtering failed'
	}
}
