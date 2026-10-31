module diagserver

fn test_a_quick_digest_holds_its_sum_and_size() {
	digest := quick_sum_digest(0x1234abcd, 42)
	assert digest == 'quick:000000001234abcd:42'
	sum, size := quick_sum_of(digest) or { panic('no quick digest: ${digest}') }
	assert sum == 0x1234abcd
	assert size == 42
	large, _ := quick_sum_of(quick_sum_digest(u64(0xffffffffffffffff), 0)) or { panic('none') }
	assert large == u64(0xffffffffffffffff)
	// A SHA-256, or a digest written otherwise, is none.
	for other in ['0123abcd', 'quick:1234', 'quick:000000001234abcd', 'quick:000000001234abcd:x'] {
		if _, _ := quick_sum_of(other) {
			assert false, other
		}
	}
}
