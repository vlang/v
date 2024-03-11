fn test_is_android() {
	$if android {
		assert true
	} $else {
		assert false, 'platform-specific test filtering failed'
	}
}
