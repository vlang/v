// vtest vflags: -scoped-defer

fn test_main() {
	defer {
		$if foo ? {
			assert false
		} $else {
			assert true
		}
	}
}
