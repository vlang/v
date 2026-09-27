module quic

fn test_stream_reassembler_in_order() {
	mut r := new_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.add(5, 'world'.bytes())!
	assert r.data().bytestr() == 'helloworld'
	assert r.consumed_len() == 10
}

fn test_stream_reassembler_out_of_order() {
	mut r := new_stream_reassembler()
	r.add(5, 'world'.bytes())!
	assert r.consumed_len() == 0
	r.add(0, 'hello'.bytes())!
	assert r.data().bytestr() == 'helloworld'
}

fn test_stream_reassembler_tolerates_identical_overlap() {
	mut r := new_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.add(0, 'hello'.bytes())!
	assert r.data().bytestr() == 'hello'
	r.add(3, 'lo world'.bytes())!
	assert r.data().bytestr() == 'hello world'
}

fn test_stream_reassembler_rejects_mismatched_overlap() {
	mut r := new_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.add(3, 'XX world'.bytes()) or {
		assert err.msg().contains('retransmission mismatch')
		return
	}
	assert false, 'expected a mismatched overlap to be rejected'
}

fn test_stream_reassembler_rejects_mismatch_between_two_pending_fragments() {
	mut r := new_stream_reassembler()
	r.add(10, 'AAAA'.bytes())!
	r.add(12, 'BBBB'.bytes())!
	r.add(0, '0123456789'.bytes()) or {
		assert err.msg().contains('retransmission mismatch')
		return
	}
	assert false, 'expected the pending-fragment mismatch to be rejected once promoted'
}

fn test_stream_reassembler_discard_frees_held_window_and_preserves_consumed_len() {
	mut r := new_stream_reassembler()
	r.add(0, 'helloworld'.bytes())!
	r.discard(5)!
	assert r.data().bytestr() == 'world'
	assert r.consumed_len() == 10 // unchanged -- discard doesn't un-receive data
}

fn test_stream_reassembler_discard_is_idempotent_for_a_stale_base() {
	mut r := new_stream_reassembler()
	r.add(0, 'helloworld'.bytes())!
	r.discard(5)!
	r.discard(5)! // same value again
	r.discard(2)! // an OLDER value -- also a no-op, not a regression
	assert r.data().bytestr() == 'world'
}

fn test_stream_reassembler_discard_rejects_base_beyond_consumed_len() {
	mut r := new_stream_reassembler()
	r.add(0, 'hello'.bytes())!
	r.discard(10) or {
		assert err.msg().contains('exceeds consumed_len')
		return
	}
	assert false, 'expected discard() past consumed_len to be rejected'
}

fn test_stream_reassembler_tolerates_retransmission_of_already_discarded_data() {
	mut r := new_stream_reassembler()
	r.add(0, 'helloworld'.bytes())!
	r.discard(5)!
	// A retransmission of "hello" (now entirely below base_offset) must be
	// tolerated silently -- there's nothing left to validate it against,
	// and a compliant peer retransmitting already-consumed bytes is normal.
	r.add(0, 'hello'.bytes())!
	assert r.data().bytestr() == 'world'
}

fn test_stream_reassembler_validates_retransmission_straddling_the_discard_boundary() {
	mut r := new_stream_reassembler()
	r.add(0, 'helloworld'.bytes())!
	r.discard(5)!
	// "hello" + "wor" -- the "hello" prefix is below base_offset (skipped),
	// "wor" straddles into the still-held window and must still be
	// validated against it.
	r.add(0, 'helloXXX'.bytes()) or {
		assert err.msg().contains('retransmission mismatch')
		return
	}
	assert false, 'expected a mismatch straddling the discard boundary to be rejected'
}

// test_stream_reassembler_rejects_data_beyond_buffering_limit is a
// Phase-R regression for a Copilot finding on vlang/v#27882
// (pullrequestreview-4888843234): the cap must bound BUFFERED-BUT-NOT-YET-
// CONSUMED bytes, not the wire offset a fragment happens to land at -- a
// single small out-of-order fragment landing near a large offset (the
// scenario this test originally covered) is now correctly ACCEPTED (it's
// a legitimate, cheap-to-buffer fragment regardless of where in the stream
// it lands); only the actual pending-byte SUM exceeding the cap is
// rejected.
fn test_stream_reassembler_rejects_data_beyond_buffering_limit() {
	mut r := new_stream_reassembler()
	offset := max_stream_buffered_bytes - 2
	r.add(offset, []u8{len: 100})! // accepted: a small fragment, regardless of its offset

	mut r2 := new_stream_reassembler()
	r2.add(u64(1) << 30, []u8{len: int(max_stream_buffered_bytes) + 1}) or {
		assert err.msg().contains('buffering limit')
		return
	}
	assert false, 'expected pending data exceeding the buffering limit to be rejected'
}

// test_stream_reassembler_supports_streams_larger_than_the_buffering_limit
// is the Phase-R reproduction proper: with discard() freeing consumed
// bytes as the caller goes, a stream's TOTAL size can exceed
// max_stream_buffered_bytes -- the exact scenario the pre-fix cap (which
// bounded absolute stream position, not buffered memory) made impossible
// even for entirely in-order data.
fn test_stream_reassembler_supports_streams_larger_than_the_buffering_limit() {
	mut r := new_stream_reassembler()
	chunk := []u8{len: 1024, init: 0x41}
	mut offset := u64(0)
	total_chunks := int(max_stream_buffered_bytes / 1024) + 10 // deliberately past the old absolute cap
	for _ in 0 .. total_chunks {
		r.add(offset, chunk)!
		offset += u64(chunk.len)
		r.discard(r.consumed_len())! // simulate the application immediately consuming each chunk
	}
	assert r.consumed_len() == offset
	assert r.consumed_len() > max_stream_buffered_bytes
}

fn test_stream_reassembler_rejects_too_many_pending_fragments() {
	mut r := new_stream_reassembler()
	mut last_err_seen := false
	for i in 0 .. max_stream_pending_fragments + 10 {
		offset := u64(2 + i * 2)
		r.add(offset, [u8(0x42)]) or {
			assert err.msg().contains('too many out-of-order fragments')
			last_err_seen = true
			break
		}
	}
	assert last_err_seen
}

fn test_stream_reassembler_note_final_size_basic_and_is_finished() {
	mut r := new_stream_reassembler()
	assert r.is_finished() == false
	r.add(0, 'hello'.bytes())!
	assert r.is_finished() == false
	r.note_final_size(5)!
	assert r.is_finished() == true
}

fn test_stream_reassembler_note_final_size_before_all_data_arrives() {
	mut r := new_stream_reassembler()
	r.note_final_size(5)!
	assert r.is_finished() == false
	r.add(0, 'hello'.bytes())!
	assert r.is_finished() == true
}

fn test_stream_reassembler_note_final_size_is_idempotent_for_same_value() {
	mut r := new_stream_reassembler()
	r.note_final_size(10)!
	r.note_final_size(10)! // e.g. a retransmitted FIN -- must not error
	assert r.is_finished() == false
}

fn test_stream_reassembler_rejects_final_size_smaller_than_received() {
	mut r := new_stream_reassembler()
	r.add(0, 'hello world'.bytes())! // 11 bytes
	r.note_final_size(5) or {
		assert err.msg().contains('FINAL_SIZE_ERROR')
		return
	}
	assert false, 'expected a final size smaller than already-received data to be rejected'
}

fn test_stream_reassembler_rejects_conflicting_final_size() {
	mut r := new_stream_reassembler()
	r.note_final_size(100)!
	r.note_final_size(200) or {
		assert err.msg().contains('FINAL_SIZE_ERROR')
		return
	}
	assert false, 'expected a changed final size to be rejected'
}

fn test_stream_reassembler_rejects_final_size_conflicting_with_pending_fragment() {
	mut r := new_stream_reassembler()
	r.add(50, [u8(1), 2, 3])! // pending: covers offset [50,53)
	r.note_final_size(52) or {
		assert err.msg().contains('FINAL_SIZE_ERROR')
		return
	}
	assert false, 'expected a final size conflicting with a pending fragment to be rejected'
}

fn test_stream_reassembler_add_rejects_data_beyond_known_final_size() {
	mut r := new_stream_reassembler()
	r.note_final_size(10)!
	r.add(8, [u8(1), 2, 3, 4]) or {
		assert err.msg().contains('FINAL_SIZE_ERROR')
		return
	}
	assert false, 'expected data extending past a known final size to be rejected'
}
