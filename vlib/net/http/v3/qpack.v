// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

// QPACK: Header Compression for HTTP/3 (RFC 9204)
// QPACK is similar to HPACK but designed for QUIC's out-of-order delivery
import net.http.v2

// decode_prefixed_integer decodes an integer with N-bit prefix.
// data should start at the byte containing the prefix.
fn decode_prefixed_integer(data []u8, prefix_bits int) !(int, int) {
	if data.len == 0 {
		return error('empty data')
	}

	mask := u8((1 << prefix_bits) - 1)
	prefix_val := int(data[0] & mask)

	if prefix_val < int(mask) {
		return prefix_val, 1
	}

	mut m := 0
	mut decoded_int := i64(0)
	mut idx := 1

	for idx < data.len {
		b := data[idx]
		decoded_int += i64(u64(b & 0x7f) << m)
		m += 7
		idx++
		if (b & 0x80) == 0 {
			break
		}
	}

	return prefix_val + int(decoded_int), idx
}

// decode_qpack_string decodes a QPACK string literal with 7-bit length prefix.
// If the Huffman flag (MSB of the first byte) is set, the payload is Huffman
// decoded using the RFC 7541/RFC 9204 static table (via v2.decode_huffman).
fn decode_qpack_string(data []u8) !(string, int) {
	if data.len == 0 {
		return error('No data to decode string')
	}

	is_huffman := (data[0] & 0x80) != 0
	length, hdr_bytes := decode_prefixed_integer(data, 7)!

	end := hdr_bytes + length
	if end > data.len {
		return error('String length exceeds data length')
	}

	payload := data[hdr_bytes..end]

	if is_huffman {
		decoded := v2.decode_huffman(payload)!
		return decoded.bytestr(), end
	}

	return payload.bytestr(), end
}

// encode_qpack_string encodes a string with length prefix, using Huffman encoding
// when it produces a shorter result (RFC 9204 §4.1.2).
@[inline]
fn encode_qpack_string(s string) []u8 {
	bytes := s.bytes()
	bytes_len := bytes.len

	// Check if Huffman encoding is shorter
	huffman_bits := v2.huffman_encoded_length(bytes)
	huffman_len := (huffman_bits + 7) / 8

	if huffman_len < bytes_len {
		huffman_data := v2.encode_huffman(bytes)
		mut result := []u8{cap: 5 + huffman_data.len}
		mut length_prefix := encode_integer(huffman_data.len, 7)
		length_prefix[0] |= 0x80 // Set Huffman flag
		result << length_prefix
		result << huffman_data
		return result
	}

	// Literal encoding (no Huffman)
	mut result := []u8{cap: 5 + bytes_len}
	result << encode_integer(bytes_len, 7)
	if bytes_len > 0 {
		result << bytes
	}
	return result
}

// max_entries computes the maximum number of dynamic table entries for QPACK.
// Per RFC 9204 §4.5.1: MaxEntries = floor(MaxTableCapacity / 32).
fn max_entries(max_table_capacity int) int {
	return max_table_capacity / 32
}

// encode_integer encodes an integer with N-bit prefix (optimized)
@[inline]
fn encode_integer(value int, n int) []u8 {
	// Pre-allocate with capacity for worst case (5 bytes for 32-bit int)
	mut result := []u8{cap: 5}
	max_prefix := (1 << n) - 1

	if value < max_prefix {
		result << u8(value)
	} else {
		result << u8(max_prefix)
		mut remaining := value - max_prefix
		for remaining >= 128 {
			result << u8((remaining % 128) + 128)
			remaining /= 128
		}
		result << u8(remaining)
	}

	return result
}
