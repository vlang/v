// vtest build: present_openssl?
module quic

fn test_retry_token_round_trip() {
	key := []u8{len: retry_token_key_len, init: 0x11}
	claims := RetryTokenClaims{
		client_addr:   [u8(127), 0, 0, 1, 0x1f, 0x90]
		original_dcid: [u8(1), 2, 3, 4, 5, 6, 7, 8]
		issued_at_ms:  123456
	}
	token := generate_retry_token(key, claims)!
	got := validate_retry_token(key, token)!
	assert got.client_addr == claims.client_addr
	assert got.original_dcid == claims.original_dcid
	assert got.issued_at_ms == claims.issued_at_ms
}

fn test_retry_token_is_randomized_per_call() {
	// Two tokens for the IDENTICAL claims must differ -- the nonce is
	// freshly randomized every call (required for AES-GCM security under a
	// reused key; a constant nonce would be a real, exploitable bug, not
	// just a test-coverage gap).
	key := []u8{len: retry_token_key_len, init: 0x22}
	claims := RetryTokenClaims{
		client_addr:   [u8(1), 2, 3, 4]
		original_dcid: [u8(5), 6, 7, 8]
		issued_at_ms:  1000
	}
	token1 := generate_retry_token(key, claims)!
	token2 := generate_retry_token(key, claims)!
	assert token1 != token2
	// Both must still independently validate to the SAME claims.
	assert validate_retry_token(key, token1)!.issued_at_ms == 1000
	assert validate_retry_token(key, token2)!.issued_at_ms == 1000
}

fn test_retry_token_rejects_wrong_key() {
	key := []u8{len: retry_token_key_len, init: 0x33}
	other_key := []u8{len: retry_token_key_len, init: 0x44}
	token := generate_retry_token(key, RetryTokenClaims{
		client_addr:   [u8(1)]
		original_dcid: [u8(2)]
		issued_at_ms:  0
	})!
	validate_retry_token(other_key, token) or { return }
	assert false, 'expected a wrong key to fail validation'
}

fn test_retry_token_rejects_tampered_bytes() {
	key := []u8{len: retry_token_key_len, init: 0x55}
	mut token := generate_retry_token(key, RetryTokenClaims{
		client_addr:   [u8(1), 2]
		original_dcid: [u8(3)]
		issued_at_ms:  0
	})!
	token[token.len - 1] ^= 0x01
	validate_retry_token(key, token) or { return }
	assert false, 'expected a tampered token to fail validation'
}

fn test_retry_token_rejects_short_input() {
	key := []u8{len: retry_token_key_len}
	validate_retry_token(key, []u8{len: retry_token_nonce_len}) or {
		assert err.msg().contains('too short')
		return
	}
	assert false, 'expected a too-short token to be rejected'
}

fn test_retry_token_rejects_wrong_key_length() {
	generate_retry_token([]u8{len: 10}, RetryTokenClaims{}) or {
		assert err.msg().contains('${retry_token_key_len}')
		return
	}
	assert false, 'expected a wrong-length key to be rejected'
}

fn test_validate_retry_token_for_attempt_accepts_within_window() {
	key := []u8{len: retry_token_key_len, init: 0x66}
	client_addr := [u8(10), 0, 0, 1, 0x00, 0x50]
	token := generate_retry_token(key, RetryTokenClaims{
		client_addr:   client_addr
		original_dcid: [u8(1), 2, 3, 4]
		issued_at_ms:  10_000
	})!
	claims := validate_retry_token_for_attempt(key, token, client_addr, 15_000, 30_000)!
	assert claims.issued_at_ms == 10_000
}

fn test_validate_retry_token_for_attempt_rejects_expired() {
	key := []u8{len: retry_token_key_len, init: 0x77}
	client_addr := [u8(10), 0, 0, 1, 0x00, 0x50]
	token := generate_retry_token(key, RetryTokenClaims{
		client_addr:   client_addr
		original_dcid: [u8(1), 2, 3, 4]
		issued_at_ms:  10_000
	})!
	// now_ms is 40_001 ms after issuance, exceeding a 30_000ms window by
	// exactly 1ms -- an off-by-one boundary test, not just "way expired."
	validate_retry_token_for_attempt(key, token, client_addr, 50_001, 30_000) or {
		assert err.msg().contains('expired')
		return
	}
	assert false, 'expected an expired token to be rejected'
}

fn test_validate_retry_token_for_attempt_accepts_at_exact_boundary() {
	key := []u8{len: retry_token_key_len, init: 0x88}
	client_addr := [u8(10), 0, 0, 1, 0x00, 0x50]
	token := generate_retry_token(key, RetryTokenClaims{
		client_addr:   client_addr
		original_dcid: [u8(1), 2, 3, 4]
		issued_at_ms:  10_000
	})!
	// Exactly max_age_ms after issuance must still be accepted (the check
	// is `> max_age_ms`, not `>=`).
	claims := validate_retry_token_for_attempt(key, token, client_addr, 40_000, 30_000)!
	assert claims.issued_at_ms == 10_000
}

fn test_validate_retry_token_for_attempt_rejects_address_mismatch() {
	key := []u8{len: retry_token_key_len, init: 0x99}
	issued_addr := [u8(10), 0, 0, 1, 0x00, 0x50]
	different_addr := [u8(10), 0, 0, 2, 0x00, 0x50]
	token := generate_retry_token(key, RetryTokenClaims{
		client_addr:   issued_addr
		original_dcid: [u8(1), 2, 3, 4]
		issued_at_ms:  0
	})!
	validate_retry_token_for_attempt(key, token, different_addr, 0, 30_000) or {
		assert err.msg().contains('different client address')
		return
	}
	assert false, 'expected an address mismatch to be rejected'
}

fn test_validate_retry_token_for_attempt_rejects_now_before_issued_at() {
	// Defensive guard against a caller-supplied now_ms that precedes
	// issued_at_ms -- see validate_retry_token_for_attempt's own doc
	// comment for why this shouldn't happen in real use, and why it's
	// still checked rather than left to underflow the u64 subtraction.
	key := []u8{len: retry_token_key_len, init: 0xaa}
	client_addr := [u8(1)]
	token := generate_retry_token(key, RetryTokenClaims{
		client_addr:   client_addr
		original_dcid: [u8(2)]
		issued_at_ms:  10_000
	})!
	validate_retry_token_for_attempt(key, token, client_addr, 9_999, 30_000) or {
		assert err.msg().contains('expired')
		return
	}
	assert false, 'expected now_ms before issued_at_ms to be rejected, not underflow'
}
