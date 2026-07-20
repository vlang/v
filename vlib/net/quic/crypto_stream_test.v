module quic

fn test_crypto_stream_reassembler_in_order() {
	mut r := new_crypto_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.add(5, 'world'.bytes())!
	assert r.data().bytestr() == 'helloworld'
	assert r.consumed_len() == 10
}

fn test_crypto_stream_reassembler_out_of_order() {
	mut r := new_crypto_stream_reassembler()
	r.add(5, 'world'.bytes())! // arrives first -> held in pending, gap before it
	assert r.consumed_len() == 0
	r.add(0, 'hello'.bytes())! // closes the gap -> both promoted
	assert r.data().bytestr() == 'helloworld'
	assert r.consumed_len() == 10
}

fn test_crypto_stream_reassembler_tolerates_identical_overlap() {
	mut r := new_crypto_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.add(0, 'hello'.bytes())! // exact duplicate retransmission
	assert r.data().bytestr() == 'hello'
	r.add(3, 'lo world'.bytes())! // overlaps bytes [3,5) with identical "lo", extends past it
	assert r.data().bytestr() == 'hello world'
}

fn test_crypto_stream_reassembler_rejects_mismatched_overlap() {
	mut r := new_crypto_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.add(3, 'XX world'.bytes()) or {
		assert err.msg().contains('retransmission mismatch')
		return
	}
	assert false, 'expected a mismatched overlap to be rejected'
}

fn test_crypto_stream_reassembler_rejects_mismatch_between_two_pending_fragments() {
	// Neither fragment has touched r.received yet when both are added (both
	// offsets are past the current gap) -- the mismatch between them can
	// only be discovered once the gap closes and they get promoted, in
	// sequence, through the same validated-append path.
	mut r := new_crypto_stream_reassembler()
	r.add(10, 'AAAA'.bytes())! // covers stream bytes [10,14)
	r.add(12, 'BBBB'.bytes())! // covers stream bytes [12,16), disagreeing with the
	// first fragment on the shared [12,14) span ('AA' vs 'BB')
	r.add(0, '0123456789'.bytes()) or {
		assert err.msg().contains('retransmission mismatch')
		return
	}
	assert false, 'expected the pending-fragment mismatch to be rejected once promoted'
}

fn test_crypto_stream_reassembler_rejects_data_beyond_buffering_limit() {
	mut r := new_crypto_stream_reassembler()
	offset := max_crypto_stream_buffered_bytes - 2
	r.add(offset, []u8{len: 100}) or {
		assert err.msg().contains('buffering limit')
		return
	}
	assert false, 'expected data past the buffering limit to be rejected'
}

fn test_crypto_stream_reassembler_ignores_zero_length_add() {
	mut r := new_crypto_stream_reassembler()
	r.add(0, []u8{})!
	assert r.consumed_len() == 0
	r.add(5, []u8{})! // even at a far-future offset, zero bytes is a no-op
	assert r.consumed_len() == 0
}

fn test_crypto_stream_reassembler_rejects_too_many_pending_fragments() {
	mut r := new_crypto_stream_reassembler()
	// Each fragment is 1 byte, at a distinct even offset, none contiguous
	// with r.received (which stays empty throughout) or with each other --
	// none ever gets promoted, so they all sit in r.pending simultaneously.
	mut last_err_seen := false
	for i in 0 .. max_crypto_stream_pending_fragments + 10 {
		offset := u64(2 + i * 2)
		r.add(offset, [u8(0x42)]) or {
			assert err.msg().contains('too many out-of-order fragments')
			last_err_seen = true
			break
		}
	}
	assert last_err_seen
}

fn test_crypto_stream_reassembler_three_way_out_of_order() {
	mut r := new_crypto_stream_reassembler()
	r.add(7, 'World!'.bytes())! // 'Hello'(5) + ', '(2) = offset 7
	r.add(5, ', '.bytes())!
	assert r.consumed_len() == 0 // still a gap before offset 5
	r.add(0, 'Hello'.bytes())!
	assert r.data().bytestr() == 'Hello, World!'
}
