// vtest build: present_openssl?
module quic

fn test_crypto_stream_reassembler_in_order() {
	mut r := new_crypto_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.add(5, 'world'.bytes())!
	assert r.data().bytestr() == 'helloworld'
	assert r.consumed_len() == 10
}

fn test_crypto_stream_reassembler_data_returns_independent_copy() {
	// data() must return a COPY, not a shared view into r.received -- V's
	// plain array assignment shares backing storage rather than copying it,
	// so a caller mutating what it believes is an independent snapshot must
	// never be able to corrupt the reassembler's own contiguous transcript.
	mut r := new_crypto_stream_reassembler()
	r.add(0, 'hello'.bytes())!

	mut snapshot := r.data()
	snapshot[0] = u8(0x58) // 'X'

	assert r.data().bytestr() == 'hello'
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
	// Both fragments are still out-of-order (the offset-0 gap stays open)
	// when the second is added -- merge_or_add_pending validates overlap
	// against every existing pending fragment immediately, so the mismatch
	// is caught right at the second add call, not deferred to promotion.
	mut r := new_crypto_stream_reassembler()
	r.add(10, 'AAAA'.bytes())! // covers stream bytes [10,14)
	r.add(12, 'BBBB'.bytes()) or {
		// covers [12,16), disagreeing with the first fragment on the
		// shared [12,14) span ('AA' vs 'BB')
		assert err.msg().contains('retransmission mismatch')
		return
	}
	assert false, 'expected the pending-fragment mismatch to be rejected at merge time'
}

fn test_crypto_stream_reassembler_merges_partially_overlapping_pending_fragments() {
	// RFC 9000 never requires a sender to retransmit CRYPTO data with
	// byte-identical frame boundaries -- a retransmission shifted by even
	// one byte from a previous attempt is ordinary loss-recovery behavior,
	// not an attack, and must not be treated as a brand new distinct
	// fragment every time (that would exhaust
	// max_crypto_stream_pending_fragments on legitimate traffic alone).
	mut r := new_crypto_stream_reassembler()
	count := max_crypto_stream_pending_fragments + 10
	for i in 0 .. count {
		// fragment i covers [100+i, 200+i) -- heavily overlapping its
		// immediate predecessor, never identical, never fully containing
		// or contained by it.
		r.add(u64(100 + i), []u8{len: 100, init: 0x41})!
	}
	assert r.pending.len == 1 // all merged into one entry, never hit the cap
	assert r.consumed_len() == 0 // gap before offset 100 still open

	r.add(0, []u8{len: 100})! // close the gap
	// merged pending range is [100, 200+count-1) -- union of every
	// fragment's span -- plus the 100 bytes explicitly added at offset 0.
	assert r.data().len == 100 + (200 + count - 1 - 100)
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

fn test_crypto_stream_reassembler_rejects_offset_near_u64_max() {
	// offset + u64(data.len) is u64 arithmetic -- an offset this close to
	// u64's own max wraps the sum back down to a small value, which would
	// silently pass an `end > max_crypto_stream_buffered_bytes` check
	// computed from the wrapped result alone (Codex P3,
	// pullrequestreview-4840201604). `add` must reject on `offset` itself
	// before ever computing that sum.
	mut r := new_crypto_stream_reassembler()
	r.add(max_u64, [u8(0x41)]) or {
		assert err.msg().contains('buffering limit')
		return
	}
	assert false, 'expected an offset near u64 max to be rejected, not wrap around'
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

fn test_crypto_stream_reassembler_deduplicates_retransmitted_pending_fragment() {
	// A peer retransmitting the SAME out-of-order CRYPTO fragment while an
	// earlier gap remains open (ordinary loss recovery -- a lost ACK, not
	// malicious behavior) must not count each retransmission as a NEW
	// distinct fragment against the max_crypto_stream_pending_fragments cap,
	// or enough ordinary retransmissions abort an otherwise-healthy
	// handshake. Contrast with
	// test_crypto_stream_reassembler_rejects_too_many_pending_fragments,
	// which uses DISTINCT offsets and must still hit the cap.
	mut r := new_crypto_stream_reassembler()
	for _ in 0 .. max_crypto_stream_pending_fragments + 10 {
		r.add(100, 'retransmitted'.bytes())!
	}
	assert r.consumed_len() == 0 // gap before offset 100 still open
	r.add(0, []u8{len: 100})! // close the gap
	assert r.data()[100..].bytestr() == 'retransmitted'
}

fn test_crypto_stream_reassembler_three_way_out_of_order() {
	mut r := new_crypto_stream_reassembler()
	r.add(7, 'World!'.bytes())! // 'Hello'(5) + ', '(2) = offset 7
	r.add(5, ', '.bytes())!
	assert r.consumed_len() == 0 // still a gap before offset 5
	r.add(0, 'Hello'.bytes())!
	assert r.data().bytestr() == 'Hello, World!'
}
