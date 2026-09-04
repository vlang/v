module quic

// test_fits_within_pmtu_runtime_assertion_at_1200_bytes is the plan's own
// explicitly named test: a runtime assertion that a datagram at exactly
// v1's pinned PMTU (1200 bytes) fits, one byte over does not, and the
// pinned value itself is RFC 9000 §14.1's safe minimum.
fn test_fits_within_pmtu_runtime_assertion_at_1200_bytes() {
	assert max_datagram_size == 1200
	assert fits_within_pmtu(1200)
	assert fits_within_pmtu(1199)
	assert fits_within_pmtu(0)
	assert !fits_within_pmtu(1201)
}
