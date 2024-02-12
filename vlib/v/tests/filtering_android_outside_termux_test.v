fn test_is_android() {
	$if android {
		assert true
	} $else {
		assert false
	}
}
