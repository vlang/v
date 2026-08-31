// vtest build: present_openssl?
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

fn test_parse_frame_handshake_done() {
	buf := [u8(0x1e)]
	frame, n := parse_frame(buf)!
	assert n == 1
	match frame {
		HandshakeDoneFrame {}
		else {
			assert false, 'expected a HandshakeDoneFrame'
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

fn test_encode_ack_frame_rejects_range_with_largest_less_than_smallest() {
	// A single self-inconsistent range (largest < smallest) is not caught
	// by the cross-range separation check above, which only compares
	// ADJACENT ranges to each other -- each range's own internal ordering
	// must also be validated, or range_length's subtraction below would
	// silently underflow (wrap) instead of failing cleanly.
	bad_ranges := [
		AckRange{
			smallest: 10
			largest:  5
		},
	]
	encode_ack_frame(bad_ranges, 0, none) or {
		assert err.msg().contains('largest')
		assert err.msg().contains('smallest')
		return
	}
	assert false, 'expected largest < smallest to be rejected'
}

fn test_scaled_ack_delay_micros_normal_value() {
	assert scaled_ack_delay_micros(100, 3) == 800 // 100 << 3
}

fn test_scaled_ack_delay_micros_saturates_instead_of_wrapping() {
	// A legal wire ACK Delay (raw_ack_delay == 1<<44, well within a single
	// varint) combined with a legal, RFC 9000 §18.2-maximum ack_delay_exponent
	// (20) shifts the value out to bit 64 -- overflowing u64. A naive `<<`
	// silently wraps (here, to exactly 0: 2^44 << 20 == 2^64 == 0 mod 2^64),
	// making an enormous peer-claimed delay look like NO delay at all,
	// defeating any downstream max_ack_delay cap. Must saturate instead.
	result := scaled_ack_delay_micros(u64(1) << 44, 20)
	assert result == max_u64
}

fn test_scaled_ack_delay_micros_saturates_at_exponent_ge_64() {
	// Defensive bound: ack_delay_exponent is validated to <=20 by
	// transport_parameters.v before reaching here, but this function is a
	// public, independently-callable API and must not exhibit C-level
	// undefined behavior (a shift amount >= the type's bit width) for any
	// input.
	assert scaled_ack_delay_micros(5, 64) == max_u64
	assert scaled_ack_delay_micros(0, 64) == 0
}

fn test_parse_frame_rejects_unimplemented_frame_type() {
	// 0x18 (NEW_CONNECTION_ID) is a real, valid QUIC frame type this module
	// simply doesn't implement yet -- connection ID rotation/migration is
	// explicitly out of v1 scope (see stateless_reset.v's own doc comment)
	// -- must be a clear "not implemented" error, not a wire-format error
	// or a panic. (0x08 STREAM and 0x1e HANDSHAKE_DONE, both used here
	// before their respective phases implemented them, would no longer
	// demonstrate this.)
	parse_frame([u8(0x18)]) or {
		assert err.msg().contains('not yet implemented')
		return
	}
	assert false, 'expected an unimplemented frame type to be rejected'
}

fn test_parse_frames_rejects_empty_payload() {
	// RFC 9000 §12.4: "An endpoint MUST treat receipt of a packet
	// containing no frames as a connection error of type
	// PROTOCOL_VIOLATION." Unlike parse_frame (singular), parse_frames'
	// own `for offset < buf.len` loop never calls into parse_frame at all
	// when buf is empty, so this must be its own explicit check.
	parse_frames([]u8{}) or {
		assert err.msg().contains('no frames')
		return
	}
	assert false, 'expected an empty packet payload to be rejected'
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
	frame0 := frames[0]
	match frame0 {
		PaddingFrame {
			assert frame0.length == 2
		}
		else {
			assert false, 'expected frames[0] to be PaddingFrame'
		}
	}
	frame1 := frames[1]
	match frame1 {
		PingFrame {}
		else {
			assert false, 'expected frames[1] to be PingFrame'
		}
	}
	frame2 := frames[2]
	match frame2 {
		CryptoFrame {
			assert frame2.offset == 0
			assert frame2.data == [u8(0xaa), 0xbb]
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

fn test_encode_crypto_frame_rejects_offset_plus_length_exceeding_varint_max() {
	// RFC 9000 §19.6: "The largest offset delivered on a stream -- the sum
	// of the offset and data length -- cannot exceed 2^62-1." Both fields
	// are individually encodable (offset == max_varint, data.len == 1) but
	// their sum overflows the limit.
	encode_crypto_frame(max_varint, [u8(0xaa)]) or {
		assert err.msg().contains('2^62')
		return
	}
	assert false, 'expected offset+length exceeding the varint max to be rejected'
}

fn test_parse_crypto_frame_rejects_offset_plus_length_exceeding_varint_max() {
	// Same invariant as above, enforced on the PARSE (attacker-controlled)
	// side: a peer can send offset == max_varint with a small nonzero
	// length -- each field individually fits in a varint, so only an
	// explicit sum check catches it.
	mut buf := encode_varint(u64(0x06))! // CRYPTO type
	buf << encode_varint(max_varint)! // offset
	buf << encode_varint(1)! // length
	buf << [u8(0xaa)]
	parse_frame(buf) or {
		assert err.msg().contains('2^62')
		return
	}
	assert false, 'expected offset+length exceeding the varint max to be rejected'
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

fn test_encode_ack_frame_rejects_endpoint_exceeding_varint_max() {
	// RFC 9000 §12.3: packet numbers are bounded to 2^62-1 -- an endpoint
	// near max_u64 must be rejected outright, not fed into the
	// separation-check arithmetic (which silently wraps at that magnitude
	// and can accept a self-contradictory ordering as if it were valid).
	bad_ranges := [
		AckRange{
			smallest: 100
			largest:  200
		},
		AckRange{
			smallest: max_u64 - 1
			largest:  max_u64 - 1
		},
	]
	encode_ack_frame(bad_ranges, 0, none) or {
		assert err.msg().contains('2^62')
		return
	}
	assert false, 'expected an endpoint exceeding the varint max to be rejected'
}

fn test_scaled_ack_delay_micros() {
	assert scaled_ack_delay_micros(5, 3) == 40
	assert scaled_ack_delay_micros(0, default_ack_delay_exponent) == 0
}

fn test_stream_frame_round_trip_implicit_offset_and_length() {
	data := [u8(1), 2, 3, 4, 5]
	encoded := encode_stream_frame(4, 0, data, false, true)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		StreamFrame {
			assert frame.stream_id == 4
			assert frame.offset == 0
			assert frame.fin == false
			assert frame.data == data
		}
		else {
			assert false, 'expected a StreamFrame'
		}
	}
}

fn test_stream_frame_with_explicit_offset_and_fin() {
	data := [u8(9), 8, 7]
	encoded := encode_stream_frame(8, 200, data, true, true)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		StreamFrame {
			assert frame.stream_id == 8
			assert frame.offset == 200
			assert frame.fin == true
			assert frame.data == data
		}
		else {
			assert false, 'expected a StreamFrame'
		}
	}
}

fn test_stream_frame_without_length_consumes_rest_of_buffer_and_ends_packet() {
	// A STREAM frame with the LEN bit clear MUST be the last frame in its
	// packet (RFC 9000 §19.8) -- confirm parse_frames correctly treats it
	// as consuming everything remaining, ending the loop cleanly rather
	// than erroring or leaving bytes unconsumed.
	data := [u8(0xaa), 0xbb, 0xcc, 0xdd]
	mut buf := [u8(0x01)] // PING first
	buf << encode_stream_frame(0, 0, data, false, false)!

	frames := parse_frames(buf)!
	assert frames.len == 2
	frame0 := frames[0]
	match frame0 {
		PingFrame {}
		else {
			assert false, 'expected frames[0] to be PingFrame'
		}
	}
	frame1 := frames[1]
	match frame1 {
		StreamFrame {
			assert frame1.data == data
		}
		else {
			assert false, 'expected frames[1] to be StreamFrame'
		}
	}
}

fn test_stream_frame_rejects_length_exceeding_buffer() {
	mut buf := encode_varint(u64(0x0a))! // STREAM type, LEN bit set, no OFF, no FIN
	buf << encode_varint(0)! // stream_id
	buf << encode_varint(100)! // length claims 100 bytes
	buf << [u8(0x01), 0x02] // but only 2 bytes actually follow
	parse_frame(buf) or {
		assert err.msg().contains('exceeds remaining buffer')
		return
	}
	assert false, 'expected a truncated STREAM frame to be rejected'
}

// test_encode_stream_frame_rejects_offset_plus_length_exceeding_varint_max
// and its parse-side sibling below are the STREAM-frame analog of
// test_encode_crypto_frame_rejects_offset_plus_length_exceeding_varint_max
// (RFC 9000 §19.8 mirrors §19.6's identical requirement).
fn test_encode_stream_frame_rejects_offset_plus_length_exceeding_varint_max() {
	encode_stream_frame(0, max_varint, [u8(0xaa)], false, true) or {
		assert err.msg().contains('2^62')
		return
	}
	assert false, 'expected offset+length exceeding the varint max to be rejected'
}

// test_encode_stream_frame_rejects_offset_near_u64_max_without_overflow is a
// Phase-R regression for a Copilot finding on vlang/v#27882
// (pullrequestreview-4888843234): unlike the parse-side check (both
// operands there come from decode_varint, inherently bounded to
// max_varint), `offset` here is caller-supplied with no such bound -- a
// naive `offset + u64(data.len) > max_varint` check would itself overflow
// and wrap to a small value for an offset this close to u64::MAX, silently
// passing the very check meant to reject it.
fn test_encode_stream_frame_rejects_offset_near_u64_max_without_overflow() {
	encode_stream_frame(0, max_u64 - 3, [u8(0xaa), 0xbb, 0xcc, 0xdd, 0xee], false, true) or {
		assert err.msg().contains('2^62')
		return
	}
	assert false, 'expected an offset near u64::MAX to be rejected, not silently accepted via overflow'
}

fn test_parse_stream_frame_rejects_offset_plus_length_exceeding_varint_max() {
	mut buf := encode_varint(u64(0x0e))! // STREAM type, OFF+LEN bits set
	buf << encode_varint(0)! // stream_id
	buf << encode_varint(max_varint)! // offset
	buf << encode_varint(1)! // length
	buf << [u8(0xaa)]
	parse_frame(buf) or {
		assert err.msg().contains('2^62')
		return
	}
	assert false, 'expected offset+length exceeding the varint max to be rejected'
}

// test_parse_stream_frame_without_length_rejects_offset_plus_implicit_length_exceeding_varint_max
// covers the LEN-bit-clear path, where the length is implicit (rest of
// buffer) rather than an explicit wire field.
fn test_parse_stream_frame_without_length_rejects_offset_plus_implicit_length_exceeding_varint_max() {
	mut buf := encode_varint(u64(0x0c))! // STREAM type, OFF bit set, LEN bit clear
	buf << encode_varint(0)! // stream_id
	buf << encode_varint(max_varint)! // offset
	buf << [u8(0xaa)] // implicit-length data: rest of buffer
	parse_frame(buf) or {
		assert err.msg().contains('2^62')
		return
	}
	assert false, 'expected offset+implicit-length exceeding the varint max to be rejected'
}

fn test_reset_stream_frame_round_trip() {
	encoded := encode_reset_stream_frame(4, 7, 1000)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		ResetStreamFrame {
			assert frame.stream_id == 4
			assert frame.error_code == 7
			assert frame.final_size == 1000
		}
		else {
			assert false, 'expected a ResetStreamFrame'
		}
	}
}

fn test_stop_sending_frame_round_trip() {
	encoded := encode_stop_sending_frame(8, 3)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		StopSendingFrame {
			assert frame.stream_id == 8
			assert frame.error_code == 3
		}
		else {
			assert false, 'expected a StopSendingFrame'
		}
	}
}

fn test_max_data_frame_round_trip() {
	encoded := encode_max_data_frame(65536)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		MaxDataFrame {
			assert frame.maximum_data == 65536
		}
		else {
			assert false, 'expected a MaxDataFrame'
		}
	}
}

fn test_max_stream_data_frame_round_trip() {
	encoded := encode_max_stream_data_frame(4, 32768)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		MaxStreamDataFrame {
			assert frame.stream_id == 4
			assert frame.maximum_stream_data == 32768
		}
		else {
			assert false, 'expected a MaxStreamDataFrame'
		}
	}
}

fn test_max_streams_frame_round_trip_both_directions() {
	bidi_encoded := encode_max_streams_frame(.bidirectional, 10)!
	assert bidi_encoded[0] == 0x12
	bidi_frame, _ := parse_frame(bidi_encoded)!
	match bidi_frame {
		MaxStreamsFrame {
			assert bidi_frame.direction == .bidirectional
			assert bidi_frame.maximum_streams == 10
		}
		else {
			assert false, 'expected a MaxStreamsFrame'
		}
	}

	uni_encoded := encode_max_streams_frame(.unidirectional, 5)!
	assert uni_encoded[0] == 0x13
	uni_frame, _ := parse_frame(uni_encoded)!
	match uni_frame {
		MaxStreamsFrame {
			assert uni_frame.direction == .unidirectional
			assert uni_frame.maximum_streams == 5
		}
		else {
			assert false, 'expected a MaxStreamsFrame'
		}
	}
}

// test_encode/parse_max_streams_frame_rejects_value_above_2_pow_60 mirror
// transport_parameters.v's own initial_max_streams_bidi/uni boundary tests
// for the identical RFC 9000 §4.6 limit, now also enforced on the
// MAX_STREAMS FRAME (not just the transport parameter) the RFC names
// alongside it.
fn test_encode_max_streams_frame_rejects_value_above_2_pow_60() {
	encode_max_streams_frame(.bidirectional, max_initial_max_streams + 1) or {
		assert err.msg().contains('2^60')
		return
	}
	assert false, 'expected a MAX_STREAMS value above 2^60 to be rejected'
}

fn test_parse_max_streams_frame_rejects_value_above_2_pow_60() {
	mut buf := encode_varint(u64(0x12))! // MAX_STREAMS bidi type
	buf << encode_varint(max_initial_max_streams + 1)!
	parse_frame(buf) or {
		assert err.msg().contains('2^60')
		return
	}
	assert false, 'expected a MAX_STREAMS value above 2^60 to be rejected'
}

fn test_encode_max_streams_frame_accepts_boundary_2_pow_60() {
	encoded := encode_max_streams_frame(.bidirectional, max_initial_max_streams)!
	frame, _ := parse_frame(encoded)!
	match frame {
		MaxStreamsFrame {
			assert frame.maximum_streams == max_initial_max_streams
		}
		else {
			assert false, 'expected a MaxStreamsFrame'
		}
	}
}

fn test_data_blocked_frame_round_trip() {
	encoded := encode_data_blocked_frame(4096)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		DataBlockedFrame {
			assert frame.maximum_data == 4096
		}
		else {
			assert false, 'expected a DataBlockedFrame'
		}
	}
}

fn test_stream_data_blocked_frame_round_trip() {
	encoded := encode_stream_data_blocked_frame(4, 2048)!
	frame, n := parse_frame(encoded)!
	assert n == encoded.len
	match frame {
		StreamDataBlockedFrame {
			assert frame.stream_id == 4
			assert frame.maximum_stream_data == 2048
		}
		else {
			assert false, 'expected a StreamDataBlockedFrame'
		}
	}
}

fn test_streams_blocked_frame_round_trip_both_directions() {
	bidi_encoded := encode_streams_blocked_frame(.bidirectional, 20)!
	assert bidi_encoded[0] == 0x16
	uni_encoded := encode_streams_blocked_frame(.unidirectional, 15)!
	assert uni_encoded[0] == 0x17

	bidi_frame, _ := parse_frame(bidi_encoded)!
	match bidi_frame {
		StreamsBlockedFrame {
			assert bidi_frame.direction == .bidirectional
			assert bidi_frame.maximum_streams == 20
		}
		else {
			assert false, 'expected a StreamsBlockedFrame'
		}
	}
}
