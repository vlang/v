module quic

fn test_parse_frame_padding_collapses_run() {
	buf := [u8(0x00), 0x00, 0x00, 0x01]
	frame, n := parse_frame(buf)!
	assert n == 3
	match frame {
		PaddingFrame {
			assert frame.length == 3
		}
		else {
			assert false, 'expected a PaddingFrame'
		}
	}
}

fn test_parse_frame_ping() {
	buf := [u8(0x01)]
	frame, n := parse_frame(buf)!
	assert n == 1
	match frame {
		PingFrame {}
		else {
			assert false, 'expected a PingFrame'
		}
	}
}

fn test_parse_frame_crypto_round_trip() {
	data := [u8(0xde), 0xad, 0xbe, 0xef, 0x01, 0x02, 0x03]
	encoded := encode_crypto_frame(1234, data)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		CryptoFrame {
			assert frame.offset == 1234
			assert frame.data == data
		}
		else {
			assert false, 'expected a CryptoFrame'
		}
	}
}

fn test_parse_frame_connection_close_transport_round_trip() {
	encoded := encode_connection_close_frame(false, 0x0a, 0x06, 'crypto frame error')!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		ConnectionCloseFrame {
			assert frame.is_application_error == false
			assert frame.error_code == 0x0a
			assert frame.frame_type == 0x06
			assert frame.reason == 'crypto frame error'
		}
		else {
			assert false, 'expected a ConnectionCloseFrame'
		}
	}
}

fn test_parse_frame_connection_close_application_round_trip() {
	// The application-level variant has no frame_type field on the wire at
	// all -- confirm it decodes back as 0, not whatever nonzero value was
	// passed to the encoder (which the encoder itself should have ignored).
	encoded := encode_connection_close_frame(true, 42, 0x1234, 'goodbye')!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		ConnectionCloseFrame {
			assert frame.is_application_error == true
			assert frame.error_code == 42
			assert frame.frame_type == 0
			assert frame.reason == 'goodbye'
		}
		else {
			assert false, 'expected a ConnectionCloseFrame'
		}
	}
}

fn test_ack_frame_gap_math_matches_hand_derived_example() {
	// Acknowledging packets {10,9,8, 5,4, 1} as three ranges: [8,10],[4,5],[1,1].
	// Hand-derived expected wire values (RFC 9000 §19.3.1's own pseudocode,
	// verified independently before trusting the round trip alone):
	// largest_acknowledged=10, first_ack_range=2 (covers [8,10]); next range
	// [4,5]: gap = 8-5-2=1, length = 5-4=1; next range [1,1]: gap = 4-1-2=1,
	// length = 1-1=0.
	ranges := [
		AckRange{
			smallest: 8
			largest:  10
		},
		AckRange{
			smallest: 4
			largest:  5
		},
		AckRange{
			smallest: 1
			largest:  1
		},
	]
	encoded := encode_ack_frame(ranges, 0, none)!
	// type=0x02(ACK) largest_ack=0x0a(10) ack_delay=0x00 range_count=0x02
	// first_ack_range=0x02 gap1=0x01 len1=0x01 gap2=0x01 len2=0x00
	assert encoded == [u8(0x02), 0x0a, 0x00, 0x02, 0x02, 0x01, 0x01, 0x01, 0x00]

	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		AckFrame {
			assert frame.largest_acknowledged == 10
			assert frame.ranges == ranges
			assert frame.ecn_counts == none
		}
		else {
			assert false, 'expected an AckFrame'
		}
	}
}

fn test_ack_frame_round_trip_single_range() {
	ranges := [
		AckRange{
			smallest: 100
			largest:  105
		},
	]
	encoded := encode_ack_frame(ranges, 7, none)!
	frame, _ := parse_frame(encoded)!
	match frame {
		AckFrame {
			assert frame.ranges == ranges
			assert frame.ack_delay == 7
		}
		else {
			assert false, 'expected an AckFrame'
		}
	}
}

fn test_ack_frame_with_ecn_counts_round_trip() {
	ranges := [
		AckRange{
			smallest: 0
			largest:  0
		},
	]
	ecn := EcnCounts{
		ect0:   3
		ect1:   0
		ecn_ce: 1
	}
	encoded := encode_ack_frame(ranges, 0, ecn)!
	assert encoded[0] == 0x03 // ACK-with-ECN frame type
	frame, _ := parse_frame(encoded)!
	match frame {
		AckFrame {
			counts := frame.ecn_counts or {
				assert false, 'expected ecn_counts to be present'
				return
			}

			assert counts == ecn
		}
		else {
			assert false, 'expected an AckFrame'
		}
	}
}

fn test_encode_ack_frame_rejects_improperly_separated_ranges() {
	// Adjacent ranges with no gap between them (largest of the second range
	// is only 1 less than the smallest of the first) violate ACK's own
	// invariant that at least one packet number between ranges must be
	// unacknowledged -- if there truly were no gap, this should have been
	// encoded as a single wider range instead.
	bad_ranges := [
		AckRange{
			smallest: 5
			largest:  10
		},
		AckRange{
			smallest: 1
			largest:  4
		},
	]
	encode_ack_frame(bad_ranges, 0, none) or {
		assert err.msg().contains('not properly separated')
		return
	}
	assert false, 'expected improperly separated ranges to be rejected'
}

fn test_encode_ack_frame_rejects_empty_ranges() {
	encode_ack_frame([]AckRange{}, 0, none) or {
		assert err.msg().contains('at least one range')
		return
	}
	assert false, 'expected empty ranges to be rejected'
}

fn test_parse_frame_rejects_unimplemented_frame_type() {
	// 0x08 (MAX_DATA) is a real, valid QUIC frame type this module simply
	// doesn't implement yet (Phase 6) -- must be a clear "not implemented"
	// error, not a wire-format error or a panic.
	parse_frame([u8(0x08), 0x00]) or {
		assert err.msg().contains('not yet implemented')
		return
	}
	assert false, 'expected an unimplemented frame type to be rejected'
}

fn test_parse_frame_rejects_empty_buffer() {
	parse_frame([]u8{}) or { return }
	assert false, 'expected an empty buffer to be rejected'
}

fn test_parse_frames_multiple_in_sequence() {
	mut buf := []u8{}
	buf << [u8(0x00), 0x00] // 2 bytes of PADDING
	buf << [u8(0x01)] // PING
	buf << encode_crypto_frame(0, [u8(0xaa), 0xbb])!

	frames := parse_frames(buf)!
	assert frames.len == 3
	match frames[0] {
		PaddingFrame {
			assert frames[0].length == 2
		}
		else {
			assert false, 'expected frames[0] to be PaddingFrame'
		}
	}
	match frames[1] {
		PingFrame {}
		else {
			assert false, 'expected frames[1] to be PingFrame'
		}
	}
	match frames[2] {
		CryptoFrame {
			assert frames[2].offset == 0
			assert frames[2].data == [u8(0xaa), 0xbb]
		}
		else {
			assert false, 'expected frames[2] to be CryptoFrame'
		}
	}
}

fn test_parse_crypto_frame_rejects_length_exceeding_buffer() {
	mut buf := encode_varint(u64(0x06))! // CRYPTO type
	buf << encode_varint(0)! // offset
	buf << encode_varint(100)! // length claims 100 bytes
	buf << [u8(0x01), 0x02] // but only 2 bytes actually follow
	parse_frame(buf) or {
		assert err.msg().contains('exceeds remaining buffer')
		return
	}
	assert false, 'expected a truncated CRYPTO frame to be rejected'
}

fn test_ack_frame_rejects_first_ack_range_exceeding_largest_acknowledged() {
	mut buf := encode_varint(u64(0x02))! // ACK type
	buf << encode_varint(5)! // largest_acknowledged = 5
	buf << encode_varint(0)! // ack_delay
	buf << encode_varint(0)! // ack_range_count
	buf << encode_varint(10)! // first_ack_range = 10 > largest_acknowledged
	parse_frame(buf) or {
		assert err.msg().contains('exceeds largest_acknowledged')
		return
	}
	assert false, 'expected first_ack_range exceeding largest_acknowledged to be rejected'
}

fn test_ack_frame_rejects_range_count_that_cannot_fit_in_buffer() {
	// A single small ACK frame claiming a huge ack_range_count (each range
	// needs at least 2 bytes on the wire) must be rejected before anything
	// sizes an allocation off that attacker-controlled value -- not
	// accepted and then fail later trying to actually read that many
	// ranges. 5_000_000 is deliberately NOT a multiple of 2^32 (unlike an
	// earlier version of this test's value, 1<<40, which happened to wrap
	// to exactly 0 when narrowed to a 32-bit `int` and so accidentally
	// produced a small, harmless allocation regardless of whether the fix
	// this test guards was present) -- large enough to clearly demonstrate
	// the point (a naive `cap:` hint here would attempt an ~80MB
	// allocation for AckRange's two u64 fields) without being large enough
	// to risk actually exhausting memory if the guard this test exists for
	// were ever removed again.
	mut buf := encode_varint(u64(0x02))! // ACK type
	buf << encode_varint(1000)! // largest_acknowledged
	buf << encode_varint(0)! // ack_delay
	buf << encode_varint(u64(5_000_000))! // ack_range_count
	buf << encode_varint(0)! // first_ack_range
	parse_frame(buf) or {
		assert err.msg().contains('cannot fit')
		return
	}
	assert false, 'expected an oversized ack_range_count to be rejected'
}

fn test_scaled_ack_delay_micros() {
	assert scaled_ack_delay_micros(5, 3) == 40
	assert scaled_ack_delay_micros(0, default_ack_delay_exponent) == 0
}
