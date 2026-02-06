fn encode[T](t T) {
	$if T is voidptr {
		assert true
	} $else $if T is $pointer {
		if voidptr(t) == unsafe { nil } {
			assert false
		}
	}
}

// vfmt off
fn test_main() {
	encode(unsafe { voidptr(0) })
	encode(unsafe { nil })
}
// vfmt on
