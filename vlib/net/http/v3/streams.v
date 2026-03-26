// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

import net.quic

// HTTP/3 unidirectional stream types (RFC 9114 §6.2).
pub const control_stream_type = u64(0x00)
pub const push_stream_type = u64(0x01)
pub const qpack_encoder_stream_type = u64(0x02)
pub const qpack_decoder_stream_type = u64(0x03)

// UniStreamManager tracks unidirectional stream IDs for an HTTP/3 connection.
// Each connection opens 3 mandatory streams (control, QPACK encoder, QPACK decoder)
// and receives 3 from the peer. Per RFC 9114 §6.2, duplicate stream types are
// a connection error.
pub struct UniStreamManager {
mut:
	control_stream_id i64 = -1
	encoder_stream_id i64 = -1
	decoder_stream_id i64 = -1
pub mut:
	peer_control_stream_id i64 = -1
	peer_encoder_stream_id i64 = -1
	peer_decoder_stream_id i64 = -1
}

// open_streams opens the 3 required unidirectional streams (control, QPACK
// encoder, QPACK decoder) on the QUIC connection and sends the stream type
// varint as the first data on each.
pub fn (mut m UniStreamManager) open_streams(mut conn quic.Connection) ! {
	// Control stream
	ctrl_id := conn.open_uni_stream() or { return error('failed to open control stream: ${err}') }
	ctrl_type := encode_stream_type(control_stream_type)!
	conn.send(u64(ctrl_id), ctrl_type)!
	m.control_stream_id = ctrl_id

	// QPACK encoder stream
	enc_id := conn.open_uni_stream() or {
		return error('failed to open QPACK encoder stream: ${err}')
	}
	enc_type := encode_stream_type(qpack_encoder_stream_type)!
	conn.send(u64(enc_id), enc_type)!
	m.encoder_stream_id = enc_id

	// QPACK decoder stream
	dec_id := conn.open_uni_stream() or {
		return error('failed to open QPACK decoder stream: ${err}')
	}
	dec_type := encode_stream_type(qpack_decoder_stream_type)!
	conn.send(u64(dec_id), dec_type)!
	m.decoder_stream_id = dec_id
}

// identify_peer_stream registers an incoming unidirectional stream from the
// peer. Returns an error for duplicate stream types (RFC 9114 §6.2) or
// unknown/unsupported types.
pub fn (mut m UniStreamManager) identify_peer_stream(stream_id u64, stream_type u64) ! {
	match stream_type {
		control_stream_type {
			if m.peer_control_stream_id != -1 {
				return error('duplicate control stream (RFC 9114 §6.2)')
			}
			m.peer_control_stream_id = i64(stream_id)
		}
		qpack_encoder_stream_type {
			if m.peer_encoder_stream_id != -1 {
				return error('duplicate QPACK encoder stream (RFC 9114 §6.2)')
			}
			m.peer_encoder_stream_id = i64(stream_id)
		}
		qpack_decoder_stream_type {
			if m.peer_decoder_stream_id != -1 {
				return error('duplicate QPACK decoder stream (RFC 9114 §6.2)')
			}
			m.peer_decoder_stream_id = i64(stream_id)
		}
		else {
			return error('unknown unidirectional stream type: 0x${stream_type:02x}')
		}
	}
}

// encode_stream_type encodes a stream type as a QUIC varint.
pub fn encode_stream_type(stream_type u64) ![]u8 {
	return encode_varint(stream_type)
}

// has_peer_control_stream returns true if the peer's control stream has been identified.
pub fn (m &UniStreamManager) has_peer_control_stream() bool {
	return m.peer_control_stream_id != -1
}

// all_peer_streams_identified returns true when all 3 peer streams have been registered.
pub fn (m &UniStreamManager) all_peer_streams_identified() bool {
	return m.peer_control_stream_id != -1 && m.peer_encoder_stream_id != -1
		&& m.peer_decoder_stream_id != -1
}

// generate_set_capacity_instruction creates a SetDynamicTableCapacity encoder
// stream instruction. Sent on the QPACK encoder stream when the connection
// opens to inform the peer decoder of the dynamic table capacity.
pub fn generate_set_capacity_instruction(capacity int) []u8 {
	instr := SetDynamicTableCapacity{
		capacity: capacity
	}
	return instr.encode()
}

// generate_encoder_instruction creates an encoder stream instruction for
// inserting a header field into the peer's dynamic table. Uses
// InsertWithNameRef when the header name is in the static table, otherwise
// InsertWithoutNameRef.
pub fn generate_encoder_instruction(header HeaderField) []u8 {
	// Check if the name exists in the static table
	if header.name in qpack_static_name_map {
		indices := qpack_static_name_map[header.name]
		if indices.len > 0 {
			instr := InsertWithNameRef{
				is_static:  true
				name_index: indices[0]
				value:      header.value
			}
			return instr.encode()
		}
	}
	// Literal name insertion
	instr := InsertWithoutNameRef{
		name:  header.name
		value: header.value
	}
	return instr.encode()
}
