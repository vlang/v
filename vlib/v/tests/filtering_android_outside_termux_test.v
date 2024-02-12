fn test_is_macos() {
	$if android_outside_termux {
		assert true
	} $else {
		assert false
	}
}
