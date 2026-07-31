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

fn test_stream_reassembler_rejects_data_beyond_buffering_limit() {
	mut r := new_stream_reassembler()
	offset := max_stream_buffered_bytes - 2
	r.add(offset, []u8{len: 100}) or {
		assert err.msg().contains('buffering limit')
		return
	}
	assert false, 'expected data past the buffering limit to be rejected'
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
