fn test_is_macos() {
	$if macos {
		assert true
	} $else {
		assert false, 'platform-specific test filtering failed'
	}
}
