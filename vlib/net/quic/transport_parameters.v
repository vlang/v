module quic

// RFC 9000 §18 — QUIC transport parameters. This file handles only the
// "Transport Parameters" sequence itself (the (ID, Length, Value) tuples
// defined in §18.2) — i.e. what RFC 9001 §8.2 calls the extension_data of
// the quic_transport_parameters TLS extension (type 0x39). The generic TLS
// extension-list wrapping (extension_type + a 2-byte length prefix, shared
// by every TLS 1.3 extension) is ClientHello/EncryptedExtensions'
// responsibility, not this file's — still pending in PROGRESS.md.

// RFC 9000 §18.2 parameter identifiers.
const param_original_destination_connection_id = u64(0x00)
const param_max_idle_timeout = u64(0x01)
const param_stateless_reset_token = u64(0x02)
const param_max_udp_payload_size = u64(0x03)
const param_initial_max_data = u64(0x04)
const param_initial_max_stream_data_bidi_local = u64(0x05)
const param_initial_max_stream_data_bidi_remote = u64(0x06)
const param_initial_max_stream_data_uni = u64(0x07)
const param_initial_max_streams_bidi = u64(0x08)
const param_initial_max_streams_uni = u64(0x09)
const param_ack_delay_exponent = u64(0x0a)
const param_max_ack_delay = u64(0x0b)
const param_disable_active_migration = u64(0x0c)
const param_preferred_address = u64(0x0d)
const param_active_connection_id_limit = u64(0x0e)
const param_initial_source_connection_id = u64(0x0f)
const param_retry_source_connection_id = u64(0x10)

// RFC 9000 §18.2's own stated validity bounds, referenced from both
// encode-side sanity checks and decode-side validation below.
const max_ack_delay_exponent = u64(20)
const max_ack_delay_upper_bound = u64(0x4000) // 2^14; values >= this are invalid
const min_active_connection_id_limit = u64(2)
const min_max_udp_payload_size = u64(1200)

// PreferredAddress is the server-only preferred_address transport
// parameter's value (RFC 9000 §18.2, Figure 22). Never sent by a client;
// `encode_transport_parameters` doesn't reject a client accidentally
// setting it (see the doc comment there), so callers on the client side
// simply must not populate it.
pub struct PreferredAddress {
pub:
	ipv4_address          [4]u8
	ipv4_port             u16
	ipv6_address          [16]u8
	ipv6_port             u16
	connection_id         []u8
	stateless_reset_token []u8 // exactly 16 bytes
}

// encode_preferred_address serializes the fixed-format Figure 22 layout.
// RFC 9000 §18.2: "A server MUST NOT include a zero-length connection ID
// in this transport parameter" — enforced here so a malformed
// PreferredAddress can never be silently encoded.
pub fn encode_preferred_address(pa PreferredAddress) ![]u8 {
	if pa.connection_id.len == 0 {
		return error('quic: preferred_address must not have a zero-length connection ID')
	}
	if pa.stateless_reset_token.len != 16 {
		return error('quic: preferred_address stateless_reset_token must be exactly 16 bytes, got ${pa.stateless_reset_token.len}')
	}
	mut out := []u8{cap: 4 + 2 + 16 + 2 + 1 + pa.connection_id.len + 16}
	out << pa.ipv4_address[..]
	out << u8(pa.ipv4_port >> 8)
	out << u8(pa.ipv4_port)
	out << pa.ipv6_address[..]
	out << u8(pa.ipv6_port >> 8)
	out << u8(pa.ipv6_port)
	out << u8(pa.connection_id.len)
	out << pa.connection_id
	out << pa.stateless_reset_token
	return out
}

// decode_preferred_address parses the fixed-format Figure 22 layout: a
// 25-byte fixed prefix (4-byte IPv4 address, 2-byte IPv4 port, 16-byte
// IPv6 address, 2-byte IPv6 port, 1-byte connection ID length), followed
// by the connection ID itself and a fixed 16-byte stateless reset token.
pub fn decode_preferred_address(buf []u8) !PreferredAddress {
	if buf.len < 25 {
		return error('quic: preferred_address truncated: need at least 25 bytes, have ${buf.len}')
	}
	mut ipv4_address := [4]u8{}
	for i in 0 .. 4 {
		ipv4_address[i] = buf[i]
	}
	ipv4_port := (u16(buf[4]) << 8) | u16(buf[5])
	mut ipv6_address := [16]u8{}
	for i in 0 .. 16 {
		ipv6_address[i] = buf[6 + i]
	}
	ipv6_port := (u16(buf[22]) << 8) | u16(buf[23])
	cid_len := int(buf[24])
	expected_len := 25 + cid_len + 16
	if buf.len != expected_len {
		return error('quic: preferred_address length mismatch: a ${cid_len}-byte connection ID implies ${expected_len} total bytes, got ${buf.len}')
	}
	if cid_len == 0 {
		return error('quic: preferred_address must not have a zero-length connection ID')
	}
	return PreferredAddress{
		ipv4_address:          ipv4_address
		ipv4_port:             ipv4_port
		ipv6_address:          ipv6_address
		ipv6_port:             ipv6_port
		connection_id:         buf[25..25 + cid_len].clone()
		stateless_reset_token: buf[25 + cid_len..expected_len].clone()
	}
}

// QuicTransportParameters holds every RFC 9000 §18.2 parameter. Fields use
// V's `?T` Optional (not RFC-matching zero-value defaults) so "not
// present" is unambiguous and distinct from "present with a zero/default
// value" — both are wire-legal and spec-equivalent, but this struct
// preserves which one actually happened; applying the spec's stated
// defaults (e.g. max_udp_payload_size's 65527, ack_delay_exponent's 3) to
// an absent field is a later phase's job (Phase 9's `QuicConn`, once it
// actually consumes these values), not this file's.
//
// The four server-only parameters (original_destination_connection_id,
// stateless_reset_token, preferred_address, retry_source_connection_id)
// are included so this struct can represent EITHER side's parameter set
// unchanged in Phase 13's server support, per the plan's `role`-field
// design — encode_transport_parameters does not reject a client
// populating them (RFC 9000 §18.2's "a server MUST treat receipt of any of
// these as TRANSPORT_PARAMETER_ERROR" is the RECEIVING side's
// responsibility to enforce, not the sending side's struct shape); v1's
// client-side caller simply must not populate them.
pub struct QuicTransportParameters {
pub mut:
	original_destination_connection_id  ?[]u8
	max_idle_timeout                    ?u64
	stateless_reset_token               ?[]u8
	max_udp_payload_size                ?u64
	initial_max_data                    ?u64
	initial_max_stream_data_bidi_local  ?u64
	initial_max_stream_data_bidi_remote ?u64
	initial_max_stream_data_uni         ?u64
	initial_max_streams_bidi            ?u64
	initial_max_streams_uni             ?u64
	ack_delay_exponent                  ?u64
	max_ack_delay                       ?u64
	disable_active_migration            bool
	preferred_address                   ?PreferredAddress
	active_connection_id_limit          ?u64
	initial_source_connection_id        ?[]u8
	retry_source_connection_id          ?[]u8
}

fn encode_varint_tlv(id u64, value u64) ![]u8 {
	value_bytes := encode_varint(value)!
	mut out := []u8{}
	out << encode_varint(id)!
	out << encode_varint(u64(value_bytes.len))!
	out << value_bytes
	return out
}

fn encode_bytes_tlv(id u64, value []u8) ![]u8 {
	mut out := []u8{}
	out << encode_varint(id)!
	out << encode_varint(u64(value.len))!
	out << value
	return out
}

// encode_transport_parameters serializes every present field as an
// (ID, Length, Value) tuple (RFC 9000 §18, Figure 21). Absent (`none`)
// Optional fields and a `false` disable_active_migration are simply
// omitted — both omission and explicit-default-value encoding are
// spec-equivalent on decode, so there is no need to special-case "equals
// the default" here.
pub fn encode_transport_parameters(p QuicTransportParameters) ![]u8 {
	mut out := []u8{}
	if v := p.original_destination_connection_id {
		out << encode_bytes_tlv(param_original_destination_connection_id, v)!
	}
	if v := p.max_idle_timeout {
		out << encode_varint_tlv(param_max_idle_timeout, v)!
	}
	if v := p.stateless_reset_token {
		if v.len != 16 {
			return error('quic: stateless_reset_token must be exactly 16 bytes, got ${v.len}')
		}
		out << encode_bytes_tlv(param_stateless_reset_token, v)!
	}
	if v := p.max_udp_payload_size {
		out << encode_varint_tlv(param_max_udp_payload_size, v)!
	}
	if v := p.initial_max_data {
		out << encode_varint_tlv(param_initial_max_data, v)!
	}
	if v := p.initial_max_stream_data_bidi_local {
		out << encode_varint_tlv(param_initial_max_stream_data_bidi_local, v)!
	}
	if v := p.initial_max_stream_data_bidi_remote {
		out << encode_varint_tlv(param_initial_max_stream_data_bidi_remote, v)!
	}
	if v := p.initial_max_stream_data_uni {
		out << encode_varint_tlv(param_initial_max_stream_data_uni, v)!
	}
	if v := p.initial_max_streams_bidi {
		out << encode_varint_tlv(param_initial_max_streams_bidi, v)!
	}
	if v := p.initial_max_streams_uni {
		out << encode_varint_tlv(param_initial_max_streams_uni, v)!
	}
	if v := p.ack_delay_exponent {
		out << encode_varint_tlv(param_ack_delay_exponent, v)!
	}
	if v := p.max_ack_delay {
		out << encode_varint_tlv(param_max_ack_delay, v)!
	}
	if p.disable_active_migration {
		out << encode_bytes_tlv(param_disable_active_migration, []u8{})!
	}
	if v := p.preferred_address {
		encoded := encode_preferred_address(v)!
		out << encode_bytes_tlv(param_preferred_address, encoded)!
	}
	if v := p.active_connection_id_limit {
		out << encode_varint_tlv(param_active_connection_id_limit, v)!
	}
	if v := p.initial_source_connection_id {
		out << encode_bytes_tlv(param_initial_source_connection_id, v)!
	}
	if v := p.retry_source_connection_id {
		out << encode_bytes_tlv(param_retry_source_connection_id, v)!
	}
	return out
}

// decode_varint_tlv_value decodes a TLV's Value field as a single varint
// and requires the varint to consume the ENTIRE value — a value field
// declaring more bytes than a minimally-encoded varint needs (trailing
// garbage within the same TLV entry) is rejected rather than silently
// ignored.
fn decode_varint_tlv_value(id u64, value []u8) !u64 {
	v, consumed := decode_varint(value)!
	if consumed != value.len {
		return error('quic: transport parameter 0x${id:x} has ${value.len - consumed} trailing byte(s) after its varint value')
	}
	return v
}

// decode_transport_parameters parses a full Transport Parameters sequence
// (RFC 9000 §18, Figure 20). Per RFC 9000 §7.4.2 ("An endpoint MUST ignore
// transport parameters that it does not support"), an unrecognized
// parameter ID is skipped, not rejected — this is also how RFC 9000 §18.1's
// reserved "31*N+27" grease IDs are exercised, with no special-casing
// needed beyond the general unknown-ID skip. `ack_delay_exponent`,
// `max_udp_payload_size`, `max_ack_delay`, and `active_connection_id_limit`
// are validated against RFC 9000 §18.2's own stated bounds.
//
// A duplicate parameter ID is rejected outright, even though RFC 9000's
// prose for §18.2 does not spell this out explicitly as a MUST — this
// mirrors established QUIC implementation practice and this project's own
// "singleton wire field extracted in a loop" lesson: silently letting the
// last occurrence win is exactly the failure mode that class of bug warns
// about, for no compensating benefit (a conforming peer never sends the
// same parameter twice).
pub fn decode_transport_parameters(buf []u8) !QuicTransportParameters {
	mut params := QuicTransportParameters{}
	mut seen := map[u64]bool{}
	mut cursor := 0
	for cursor < buf.len {
		id, id_len := decode_varint(buf[cursor..])!
		cursor += id_len
		length, length_len := decode_varint(buf[cursor..])!
		cursor += length_len
		if u64(cursor) + length > u64(buf.len) {
			return error('quic: transport parameter 0x${id:x} declares length ${length} exceeding the remaining buffer')
		}
		value := buf[cursor..cursor + int(length)]
		cursor += int(length)

		if id in seen {
			return error('quic: duplicate transport parameter 0x${id:x}')
		}
		seen[id] = true

		match id {
			param_original_destination_connection_id {
				params.original_destination_connection_id = value.clone()
			}
			param_max_idle_timeout {
				params.max_idle_timeout = decode_varint_tlv_value(id, value)!
			}
			param_stateless_reset_token {
				if value.len != 16 {
					return error('quic: stateless_reset_token must be exactly 16 bytes, got ${value.len}')
				}
				params.stateless_reset_token = value.clone()
			}
			param_max_udp_payload_size {
				v := decode_varint_tlv_value(id, value)!
				if v < min_max_udp_payload_size {
					return error('quic: max_udp_payload_size ${v} is invalid: values below ${min_max_udp_payload_size} are invalid')
				}
				params.max_udp_payload_size = v
			}
			param_initial_max_data {
				params.initial_max_data = decode_varint_tlv_value(id, value)!
			}
			param_initial_max_stream_data_bidi_local {
				params.initial_max_stream_data_bidi_local = decode_varint_tlv_value(id, value)!
			}
			param_initial_max_stream_data_bidi_remote {
				params.initial_max_stream_data_bidi_remote = decode_varint_tlv_value(id, value)!
			}
			param_initial_max_stream_data_uni {
				params.initial_max_stream_data_uni = decode_varint_tlv_value(id, value)!
			}
			param_initial_max_streams_bidi {
				params.initial_max_streams_bidi = decode_varint_tlv_value(id, value)!
			}
			param_initial_max_streams_uni {
				params.initial_max_streams_uni = decode_varint_tlv_value(id, value)!
			}
			param_ack_delay_exponent {
				v := decode_varint_tlv_value(id, value)!
				if v > max_ack_delay_exponent {
					return error('quic: ack_delay_exponent ${v} is invalid: values above ${max_ack_delay_exponent} are invalid')
				}
				params.ack_delay_exponent = v
			}
			param_max_ack_delay {
				v := decode_varint_tlv_value(id, value)!
				if v >= max_ack_delay_upper_bound {
					return error('quic: max_ack_delay ${v} is invalid: values of ${max_ack_delay_upper_bound} (2^14) or greater are invalid')
				}
				params.max_ack_delay = v
			}
			param_disable_active_migration {
				if value.len != 0 {
					return error('quic: disable_active_migration must be zero-length, got ${value.len} bytes')
				}
				params.disable_active_migration = true
			}
			param_preferred_address {
				params.preferred_address = decode_preferred_address(value)!
			}
			param_active_connection_id_limit {
				v := decode_varint_tlv_value(id, value)!
				if v < min_active_connection_id_limit {
					return error('quic: active_connection_id_limit ${v} is invalid: MUST be at least ${min_active_connection_id_limit}')
				}
				params.active_connection_id_limit = v
			}
			param_initial_source_connection_id {
				params.initial_source_connection_id = value.clone()
			}
			param_retry_source_connection_id {
				params.retry_source_connection_id = value.clone()
			}
			else {
				// RFC 9000 §7.4.2: ignore, don't reject. `cursor` has
				// already advanced past this parameter's Value field.
			}
		}
	}
	return params
}
