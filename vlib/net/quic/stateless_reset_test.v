module quic

fn make_token(fill u8) []u8 {
	mut t := []u8{len: 16}
	for i in 0 .. 16 {
		t[i] = fill
	}
	return t
}

fn test_record_token_rejects_wrong_length() {
	mut t := new_stateless_reset_tracker()
	t.record_token([u8(1), 2, 3], [u8(1), 2, 3]) or {
		assert err.msg().contains('16 bytes')
		return
	}
	assert false, 'expected a non-16-byte token to be rejected'
}

// test_is_stateless_reset_recognized_only_via_a_matching_token is the
// plan's own explicitly named scope: is_stateless_reset only recognizes a
// reset when the trailing 16 bytes match a token PREVIOUSLY recorded for
// that exact connection ID -- it never fabricates a match, and does not
// depend on anything else in the datagram.
fn test_is_stateless_reset_recognized_only_via_a_matching_token() {
	mut t := new_stateless_reset_tracker()
	cid := [u8(0xaa), 0xbb, 0xcc, 0xdd]
	token := make_token(0x42)
	t.record_token(cid, token)!

	mut datagram := []u8{len: 40, init: 0x99}
	// Plant the token as the trailing 16 bytes.
	for i in 0 .. 16 {
		datagram[datagram.len - 16 + i] = token[i]
	}
	assert t.is_stateless_reset(cid, datagram)

	// A different connection ID has no recorded token -- never matches.
	other_cid := [u8(0x01), 0x02]
	assert !t.is_stateless_reset(other_cid, datagram)

	// A datagram whose trailing bytes don't match the recorded token.
	mut mismatched := []u8{len: 40, init: 0x99}
	assert !t.is_stateless_reset(cid, mismatched)
}

fn test_is_stateless_reset_false_for_datagram_shorter_than_a_token() {
	mut t := new_stateless_reset_tracker()
	cid := [u8(1), 2, 3]
	t.record_token(cid, make_token(0x42))!

	short := []u8{len: 10, init: 0x42}
	assert !t.is_stateless_reset(cid, short)
}
