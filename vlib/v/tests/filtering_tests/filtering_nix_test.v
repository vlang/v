fn test_is_nix() {
	$if !windows {
		assert true
	} $else {
		assert false, 'platform-specific test filtering failed'
	}
}
