module quic

fn test_encode_transport_parameters_empty() {
	encoded := encode_transport_parameters(QuicTransportParameters{})!
	assert encoded.len == 0
}

fn test_transport_parameters_round_trip_all_varint_fields() {
	params := QuicTransportParameters{
		max_idle_timeout: 30000
		max_udp_payload_size: 1500
		initial_max_data: 1_000_000
		initial_max_stream_data_bidi_local: 262144
		initial_max_stream_data_bidi_remote: 262144
		initial_max_stream_data_uni: 262144
		initial_max_streams_bidi: 100
		initial_max_streams_uni: 100
		ack_delay_exponent: 3
		max_ack_delay: 25
		active_connection_id_limit: 4
	}
	encoded := encode_transport_parameters(params)!
	decoded := decode_transport_parameters(encoded)!

	assert decoded.max_idle_timeout? == 30000
	assert decoded.max_udp_payload_size? == 1500
	assert decoded.initial_max_data? == 1_000_000
	assert decoded.initial_max_stream_data_bidi_local? == 262144
	assert decoded.initial_max_stream_data_bidi_remote? == 262144
	assert decoded.initial_max_stream_data_uni? == 262144
	assert decoded.initial_max_streams_bidi? == 100
	assert decoded.initial_max_streams_uni? == 100
	assert decoded.ack_delay_exponent? == 3
	assert decoded.max_ack_delay? == 25
	assert decoded.active_connection_id_limit? == 4
}

fn test_transport_parameters_round_trip_bytes_fields() {
	dcid := [u8(0x83), 0x94, 0xc8, 0xf0]
	scid := [u8(0x01), 0x02, 0x03]
	params := QuicTransportParameters{
		initial_source_connection_id: scid
		original_destination_connection_id: dcid
	}
	encoded := encode_transport_parameters(params)!
	decoded := decode_transport_parameters(encoded)!

	assert decoded.initial_source_connection_id? == scid
	assert decoded.original_destination_connection_id? == dcid
}

fn test_transport_parameters_round_trip_disable_active_migration() {
	on := encode_transport_parameters(QuicTransportParameters{ disable_active_migration: true })!
	decoded_on := decode_transport_parameters(on)!
	assert decoded_on.disable_active_migration == true

	off := encode_transport_parameters(QuicTransportParameters{ disable_active_migration: false })!
	decoded_off := decode_transport_parameters(off)!
	assert decoded_off.disable_active_migration == false
	// A false flag is simply omitted from the wire, not encoded as a
	// zero-length-but-present entry -- confirm nothing was written for it.
	assert off.len == 0
}

fn test_transport_parameters_omitted_fields_stay_none() {
	decoded := decode_transport_parameters([]u8{})!
	if decoded.max_idle_timeout != none {
		assert false, 'expected max_idle_timeout to be none for an empty parameter sequence'
	}
	if decoded.initial_source_connection_id != none {
		assert false, 'expected initial_source_connection_id to be none for an empty parameter sequence'
	}
}

fn test_preferred_address_round_trip() {
	pa := PreferredAddress{
		ipv4_address: [u8(192), 0, 2, 1]!
		ipv4_port: 443
		ipv6_address: [u8(0x20), 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]!
		ipv6_port: 443
		connection_id: [u8(0xaa), 0xbb, 0xcc]
		stateless_reset_token: []u8{len: 16, init: 0x42}
	}
	params := QuicTransportParameters{
		preferred_address: pa
	}
	encoded := encode_transport_parameters(params)!
	decoded := decode_transport_parameters(encoded)!

	got := decoded.preferred_address or { panic('expected preferred_address to be present') }
	assert got.ipv4_address == pa.ipv4_address
	assert got.ipv4_port == pa.ipv4_port
	assert got.ipv6_address == pa.ipv6_address
	assert got.ipv6_port == pa.ipv6_port
	assert got.connection_id == pa.connection_id
	assert got.stateless_reset_token == pa.stateless_reset_token
}

fn test_encode_preferred_address_rejects_zero_length_connection_id() {
	pa := PreferredAddress{
		connection_id: []u8{}
		stateless_reset_token: []u8{len: 16}
	}
	encode_preferred_address(pa) or {
		assert err.msg().contains('zero-length')
		return
	}
	assert false, 'expected an error for a zero-length connection ID'
}

// test_encode_preferred_address_rejects_overlong_connection_id is a
// regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4782360314): a connection_id longer than 255 bytes
// wrapped the u8-encoded length byte (u8(256) == 0) while still appending
// the full connection_id, producing wire bytes that mismatch their own
// declared length.
fn test_encode_preferred_address_rejects_overlong_connection_id() {
	pa := PreferredAddress{
		connection_id: []u8{len: 256, init: 0xAB}
		stateless_reset_token: []u8{len: 16}
	}
	encode_preferred_address(pa) or {
		assert err.msg().contains('255')
		return
	}
	assert false, 'expected an error for a connection_id longer than 255 bytes'
}

fn test_decode_preferred_address_rejects_zero_length_connection_id() {
	// Hand-built wire bytes: 25-byte fixed prefix with cid_len=0, then a
	// 16-byte stateless reset token and nothing else.
	mut buf := []u8{len: 25}
	buf << []u8{len: 16}
	decode_preferred_address(buf) or {
		assert err.msg().contains('zero-length')
		return
	}
	assert false, 'expected an error for a zero-length connection ID on the wire'
}

// test_encode_preferred_address_rejects_mismatched_address_port and its
// decode counterpart are regression tests for a Codex finding (vlang/v#27680
// pullrequestreview-4806500473): RFC 9000 §18.2 defines an all-zero
// address+port TOGETHER as "this family not provided", but neither side
// validated that the pair was actually consistent -- an all-zero address
// with a nonzero port (or vice versa) encoded/decoded successfully despite
// being neither a real usable address nor the spec's own "not provided"
// shape.
fn test_encode_preferred_address_rejects_mismatched_address_port() {
	pa_v4 := PreferredAddress{
		ipv4_port: 8080 // nonzero port, but ipv4_address defaults to all-zero
		connection_id: [u8(1), 2, 3, 4]
		stateless_reset_token: []u8{len: 16}
	}
	encode_preferred_address(pa_v4) or {
		assert err.msg().contains('IPv4')
		assert err.msg().contains('mismatch')
		return
	}
	assert false, 'expected an error for an all-zero IPv4 address paired with a nonzero port'
}

fn test_encode_preferred_address_rejects_mismatched_ipv6_address_port() {
	pa_v6 := PreferredAddress{
		ipv6_port: 443 // nonzero port, but ipv6_address defaults to all-zero
		connection_id: [u8(1), 2, 3, 4]
		stateless_reset_token: []u8{len: 16}
	}
	encode_preferred_address(pa_v6) or {
		assert err.msg().contains('IPv6')
		assert err.msg().contains('mismatch')
		return
	}
	assert false, 'expected an error for an all-zero IPv6 address paired with a nonzero port'
}

fn test_decode_preferred_address_rejects_mismatched_address_port() {
	// Hand-built wire bytes: all-zero ipv4_address, nonzero ipv4_port.
	mut buf := []u8{len: 4} // ipv4_address = 0.0.0.0
	buf << [u8(0x1f), 0x90] // ipv4_port = 8080
	buf << []u8{len: 16} // ipv6_address = ::
	buf << [u8(0), 0] // ipv6_port = 0
	buf << u8(4) // cid_len
	buf << [u8(1), 2, 3, 4]
	buf << []u8{len: 16}
	decode_preferred_address(buf) or {
		assert err.msg().contains('IPv4')
		assert err.msg().contains('mismatch')
		return
	}
	assert false, 'expected an error for an all-zero IPv4 address paired with a nonzero port'
}

// test_encode_preferred_address_rejects_v1_cid_over_20_bytes and its decode
// counterpart are regression tests for a Codex finding (vlang/v#27680
// pullrequestreview-4783410111): a 21-255 byte connection_id passes the
// wire format's own 255-byte length-byte limit (already checked, see
// test_encode_preferred_address_rejects_overlong_connection_id above) but
// still violates QUIC v1's OWN, separate 20-byte connection-ID limit (RFC
// 9000 §17.2) -- the same limit header.v's dcid/scid already enforce.
fn test_encode_preferred_address_rejects_v1_cid_over_20_bytes() {
	pa := PreferredAddress{
		connection_id: []u8{len: 21, init: 0xAB}
		stateless_reset_token: []u8{len: 16}
	}
	encode_preferred_address(pa) or {
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a connection_id longer than 20 bytes (QUIC v1 limit)'
}

fn test_decode_preferred_address_rejects_v1_cid_over_20_bytes() {
	mut buf := []u8{len: 25}
	buf[24] = u8(21)
	buf << []u8{len: 21, init: 0xCD}
	buf << []u8{len: 16}
	decode_preferred_address(buf) or {
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a 21-byte connection ID on the wire (QUIC v1 limit)'
}

// The following six tests are regression tests for a gap this session's
// QUIC conformance-matrix sibling sweep found (not Codex, which only
// reported the preferred_address instance above): initial_source_
// connection_id/original_destination_connection_id/retry_source_
// connection_id are ALSO QUIC v1 connection IDs (each is defined as an
// echo of a wire DCID/SCID field already bound by the same 20-byte limit
// when it first appeared on the wire, RFC 9000 §7.3), but none of them had
// ever gotten this check on either direction.
fn test_encode_transport_parameters_rejects_initial_source_connection_id_over_20_bytes() {
	params := QuicTransportParameters{
		initial_source_connection_id: []u8{len: 21, init: 0xAB}
	}
	encode_transport_parameters(params) or {
		assert err.msg().contains('initial_source_connection_id')
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a 21-byte initial_source_connection_id'
}

fn test_decode_transport_parameters_rejects_initial_source_connection_id_over_20_bytes() {
	mut buf := []u8{}
	buf << encode_varint(param_initial_source_connection_id)!
	buf << encode_varint(21)!
	buf << []u8{len: 21, init: 0xAB}
	decode_transport_parameters(buf) or {
		assert err.msg().contains('initial_source_connection_id')
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a 21-byte initial_source_connection_id'
}

fn test_encode_transport_parameters_rejects_original_destination_connection_id_over_20_bytes() {
	params := QuicTransportParameters{
		original_destination_connection_id: []u8{len: 21, init: 0xAB}
	}
	encode_transport_parameters(params) or {
		assert err.msg().contains('original_destination_connection_id')
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a 21-byte original_destination_connection_id'
}

fn test_decode_transport_parameters_rejects_original_destination_connection_id_over_20_bytes() {
	mut buf := []u8{}
	buf << encode_varint(param_original_destination_connection_id)!
	buf << encode_varint(21)!
	buf << []u8{len: 21, init: 0xAB}
	decode_transport_parameters(buf) or {
		assert err.msg().contains('original_destination_connection_id')
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a 21-byte original_destination_connection_id'
}

fn test_encode_transport_parameters_rejects_retry_source_connection_id_over_20_bytes() {
	params := QuicTransportParameters{
		retry_source_connection_id: []u8{len: 21, init: 0xAB}
	}
	encode_transport_parameters(params) or {
		assert err.msg().contains('retry_source_connection_id')
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a 21-byte retry_source_connection_id'
}

fn test_decode_transport_parameters_rejects_retry_source_connection_id_over_20_bytes() {
	mut buf := []u8{}
	buf << encode_varint(param_retry_source_connection_id)!
	buf << encode_varint(21)!
	buf << []u8{len: 21, init: 0xAB}
	decode_transport_parameters(buf) or {
		assert err.msg().contains('retry_source_connection_id')
		assert err.msg().contains('20')
		return
	}
	assert false, 'expected an error for a 21-byte retry_source_connection_id'
}

fn test_decode_transport_parameters_ignores_unknown_ids() {
	// RFC 9000 §18.1's own grease pattern: 31*N+27. Use N=0 (id=27) and
	// N=1 (id=58), each with an arbitrary 2-byte value, interleaved with a
	// real, recognized parameter to confirm the cursor still advances
	// correctly around the ignored entries.
	mut buf := []u8{}
	buf << encode_varint(27)!
	buf << encode_varint(2)!
	buf << [u8(0xde), 0xad]
	buf << encode_varint_tlv(param_max_idle_timeout, 5000)!
	buf << encode_varint(58)!
	buf << encode_varint(2)!
	buf << [u8(0xbe), 0xef]

	decoded := decode_transport_parameters(buf)!
	assert decoded.max_idle_timeout? == 5000
}

fn test_decode_transport_parameters_rejects_duplicate_id() {
	mut buf := []u8{}
	buf << encode_varint_tlv(param_max_idle_timeout, 1000)!
	buf << encode_varint_tlv(param_max_idle_timeout, 2000)!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('duplicate')
		return
	}
	assert false, 'expected an error for a duplicate transport parameter ID'
}

fn test_decode_transport_parameters_rejects_truncated_value() {
	// Declares a length of 4 but supplies only 1 byte of value.
	mut buf := []u8{}
	buf << encode_varint(param_max_idle_timeout)!
	buf << encode_varint(4)!
	buf << [u8(0x01)]
	decode_transport_parameters(buf) or {
		assert err.msg().contains('exceeding')
		return
	}
	assert false, 'expected an error for a value shorter than its declared length'
}

fn test_decode_transport_parameters_rejects_trailing_garbage_in_varint_value() {
	// A 2-byte varint-encodable value (1000 fits in 2 bytes) declared with
	// an oversized length of 4, leaving 2 trailing garbage bytes inside
	// the same TLV entry.
	mut buf := []u8{}
	buf << encode_varint(param_max_idle_timeout)!
	buf << encode_varint(4)!
	buf << encode_varint(1000)! // 2 bytes
	buf << [u8(0), 0] // 2 bytes of trailing garbage, still inside the declared length
	decode_transport_parameters(buf) or {
		assert err.msg().contains('trailing')
		return
	}
	assert false, 'expected an error for trailing garbage inside a varint value field'
}

fn test_decode_transport_parameters_rejects_ack_delay_exponent_above_20() {
	buf := encode_varint_tlv(param_ack_delay_exponent, 21)!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('ack_delay_exponent')
		return
	}
	assert false, 'expected an error for ack_delay_exponent above 20'
}

fn test_decode_transport_parameters_accepts_ack_delay_exponent_boundary_20() {
	buf := encode_varint_tlv(param_ack_delay_exponent, 20)!
	decoded := decode_transport_parameters(buf)!
	assert decoded.ack_delay_exponent? == 20
}

fn test_decode_transport_parameters_rejects_max_udp_payload_size_below_1200() {
	buf := encode_varint_tlv(param_max_udp_payload_size, 1199)!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('max_udp_payload_size')
		return
	}
	assert false, 'expected an error for max_udp_payload_size below 1200'
}

fn test_decode_transport_parameters_accepts_max_udp_payload_size_boundary_1200() {
	buf := encode_varint_tlv(param_max_udp_payload_size, 1200)!
	decoded := decode_transport_parameters(buf)!
	assert decoded.max_udp_payload_size? == 1200
}

fn test_decode_transport_parameters_rejects_max_ack_delay_at_2_pow_14() {
	buf := encode_varint_tlv(param_max_ack_delay, 0x4000)!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('max_ack_delay')
		return
	}
	assert false, 'expected an error for max_ack_delay >= 2^14'
}

fn test_decode_transport_parameters_accepts_max_ack_delay_boundary_below_2_pow_14() {
	buf := encode_varint_tlv(param_max_ack_delay, 0x3fff)!
	decoded := decode_transport_parameters(buf)!
	assert decoded.max_ack_delay? == 0x3fff
}

fn test_decode_transport_parameters_rejects_active_connection_id_limit_below_2() {
	buf := encode_varint_tlv(param_active_connection_id_limit, 1)!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('active_connection_id_limit')
		return
	}
	assert false, 'expected an error for active_connection_id_limit below 2'
}

fn test_decode_transport_parameters_accepts_active_connection_id_limit_boundary_2() {
	buf := encode_varint_tlv(param_active_connection_id_limit, 2)!
	decoded := decode_transport_parameters(buf)!
	assert decoded.active_connection_id_limit? == 2
}

// test_decode_transport_parameters_rejects_initial_max_streams_bidi_above_2_pow_60
// is a regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4781706846): RFC 9000 §4.6 requires
// TRANSPORT_PARAMETER_ERROR for a stream count above 2^60 (a stream ID is
// the count times 4, so anything larger risks exceeding or wrapping the
// 62-bit varint space) -- previously entirely unchecked.
fn test_decode_transport_parameters_rejects_initial_max_streams_bidi_above_2_pow_60() {
	buf := encode_varint_tlv(param_initial_max_streams_bidi, max_initial_max_streams + 1)!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('initial_max_streams_bidi')
		return
	}
	assert false, 'expected an error for initial_max_streams_bidi above 2^60'
}

fn test_decode_transport_parameters_accepts_initial_max_streams_bidi_boundary_2_pow_60() {
	buf := encode_varint_tlv(param_initial_max_streams_bidi, max_initial_max_streams)!
	decoded := decode_transport_parameters(buf)!
	assert decoded.initial_max_streams_bidi? == max_initial_max_streams
}

fn test_decode_transport_parameters_rejects_initial_max_streams_uni_above_2_pow_60() {
	buf := encode_varint_tlv(param_initial_max_streams_uni, max_initial_max_streams + 1)!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('initial_max_streams_uni')
		return
	}
	assert false, 'expected an error for initial_max_streams_uni above 2^60'
}

// The four tests below are regression tests for a Codex finding (vlang/v#27680
// pullrequestreview-4781706846): encode_transport_parameters didn't mirror
// ANY of decode_transport_parameters' own RFC 9000 §18.2 bounds checks, so a
// caller configuration error (e.g. max_udp_payload_size below 1200) silently
// produced an invalid ClientHello instead of failing at encode time.

fn test_encode_transport_parameters_rejects_max_udp_payload_size_below_1200() {
	encode_transport_parameters(QuicTransportParameters{
		max_udp_payload_size: 1199
	}) or {
		assert err.msg().contains('max_udp_payload_size')
		return
	}
	assert false, 'expected an error for encoding max_udp_payload_size below 1200'
}

fn test_encode_transport_parameters_rejects_ack_delay_exponent_above_20() {
	encode_transport_parameters(QuicTransportParameters{
		ack_delay_exponent: 21
	}) or {
		assert err.msg().contains('ack_delay_exponent')
		return
	}
	assert false, 'expected an error for encoding ack_delay_exponent above 20'
}

fn test_encode_transport_parameters_rejects_max_ack_delay_at_2_pow_14() {
	encode_transport_parameters(QuicTransportParameters{
		max_ack_delay: 0x4000
	}) or {
		assert err.msg().contains('max_ack_delay')
		return
	}
	assert false, 'expected an error for encoding max_ack_delay >= 2^14'
}

fn test_encode_transport_parameters_rejects_active_connection_id_limit_below_2() {
	encode_transport_parameters(QuicTransportParameters{
		active_connection_id_limit: 1
	}) or {
		assert err.msg().contains('active_connection_id_limit')
		return
	}
	assert false, 'expected an error for encoding active_connection_id_limit below 2'
}

fn test_encode_transport_parameters_rejects_initial_max_streams_bidi_above_2_pow_60() {
	encode_transport_parameters(QuicTransportParameters{
		initial_max_streams_bidi: max_initial_max_streams + 1
	}) or {
		assert err.msg().contains('initial_max_streams_bidi')
		return
	}
	assert false, 'expected an error for encoding initial_max_streams_bidi above 2^60'
}

fn test_decode_transport_parameters_rejects_wrong_stateless_reset_token_length() {
	buf := encode_bytes_tlv(param_stateless_reset_token, []u8{len: 15})!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('16 bytes')
		return
	}
	assert false, 'expected an error for a stateless_reset_token that is not exactly 16 bytes'
}

fn test_decode_transport_parameters_rejects_nonzero_length_disable_active_migration() {
	buf := encode_bytes_tlv(param_disable_active_migration, [u8(0x01)])!
	decode_transport_parameters(buf) or {
		assert err.msg().contains('zero-length')
		return
	}
	assert false, 'expected an error for a non-zero-length disable_active_migration value'
}
