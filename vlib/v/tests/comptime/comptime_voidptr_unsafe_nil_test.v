fn encode[T](t T) {
	$if T is voidptr {
		assert true
	} $else $if T is $pointer {
		if voidptr(t) == unsafe { nil } {
			assert false
		}
	}
}

fn test_main() {
	encode(unsafe { nil })
	encode(unsafe { nil })
}
