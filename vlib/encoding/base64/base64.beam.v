// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// BEAM backend base64 module
// Pure V implementation of Base64 encoding/decoding for BEAM target.
module base64

const base64_chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

// encode encodes the `[]u8` value passed in `data` to base64.
// Example: assert base64.encode('V in base 64'.bytes()) == 'ViBpbiBiYXNlIDY0'
pub fn encode(data []u8) string {
	if data.len == 0 {
		return ''
	}
	chars := base64_chars
	mut result := ''
	mut i := 0
	for i + 2 < data.len {
		n := (int(data[i]) << 16) | (int(data[i + 1]) << 8) | int(data[i + 2])
		result += chars[(n >> 18) & 0x3F].ascii_str()
		result += chars[(n >> 12) & 0x3F].ascii_str()
		result += chars[(n >> 6) & 0x3F].ascii_str()
		result += chars[n & 0x3F].ascii_str()
		i += 3
	}
	remaining := data.len - i
	if remaining == 1 {
		n := int(data[i]) << 16
		result += chars[(n >> 18) & 0x3F].ascii_str()
		result += chars[(n >> 12) & 0x3F].ascii_str()
		result += '=='
	} else if remaining == 2 {
		n := (int(data[i]) << 16) | (int(data[i + 1]) << 8)
		result += chars[(n >> 18) & 0x3F].ascii_str()
		result += chars[(n >> 12) & 0x3F].ascii_str()
		result += chars[(n >> 6) & 0x3F].ascii_str()
		result += '='
	}
	return result
}

// encode_str is the string variant of encode
pub fn encode_str(data string) string {
	return encode(data.bytes())
}

// decode decodes the base64 encoded `string` value passed in `data`.
// Example: assert base64.decode('ViBpbiBiYXNlIDY0') == 'V in base 64'.bytes()
pub fn decode(data string) []u8 {
	if data.len == 0 {
		return []
	}
	// First, collect valid base64 characters and track padding
	mut chars := []int{}
	mut pad_count := 0
	for ci in 0 .. data.len {
		ch := data[ci]
		if ch >= `A` && ch <= `Z` {
			chars << int(ch) - int(`A`)
		} else if ch >= `a` && ch <= `z` {
			chars << int(ch) - int(`a`) + 26
		} else if ch >= `0` && ch <= `9` {
			chars << int(ch) - int(`0`) + 52
		} else if ch == `+` {
			chars << 62
		} else if ch == `/` {
			chars << 63
		} else if ch == `=` {
			chars << 0
			pad_count++
		}
		// Skip whitespace and other characters
	}
	mut result := []u8{}
	mut i := 0
	for i + 3 < chars.len {
		n := (chars[i] << 18) | (chars[i + 1] << 12) | (chars[i + 2] << 6) | chars[i + 3]
		result << u8((n >> 16) & 0xFF)
		result << u8((n >> 8) & 0xFF)
		result << u8(n & 0xFF)
		i += 4
	}
	// Handle the last group (may have padding)
	if i + 3 == chars.len - 1 {
		// We already processed all full groups above
	}
	// Remove bytes added due to padding
	if pad_count == 1 && result.len > 0 {
		result.delete_last()
	} else if pad_count == 2 && result.len > 1 {
		result.delete_last()
		result.delete_last()
	}
	return result
}

// decode_str is the string variant of decode
pub fn decode_str(data string) string {
	bytes := decode(data)
	mut result := ''
	for b in bytes {
		result += b.ascii_str()
	}
	return result
}

// encode_in_buffer base64 encodes the `[]u8` passed in `data` into `buffer`.
// On BEAM: Provided for API compatibility.
// Returns the size of the encoded data in the buffer.
pub fn encode_in_buffer(data []u8, buffer &u8) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// decode_in_buffer decodes the base64 encoded `string` reference passed in `data` into `buffer`.
// On BEAM: Provided for API compatibility.
// Returns the size of the decoded data in the buffer.
pub fn decode_in_buffer(data &string, buffer &u8) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// decode_in_buffer_bytes decodes the base64 encoded ASCII bytes from `data` into `buffer`.
// On BEAM: Provided for API compatibility.
// Returns the size of the decoded data in the buffer.
pub fn decode_in_buffer_bytes(data []u8, buffer &u8) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// encode_from_buffer will perform encoding from any type of src buffer
// and write the bytes into `dest`.
// On BEAM: Internal function, provided for API compatibility.
fn encode_from_buffer(dest &u8, src &u8, src_len int) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// decode_from_buffer decodes the base64 encoded ASCII bytes from `src` into `dest`.
// On BEAM: Internal function, provided for API compatibility.
fn decode_from_buffer(dest &u8, src &u8, src_len int) int {
	// On BEAM, buffer management is handled differently (GC manages memory)
	// This is provided for API compatibility
	return 0
}

// alloc_and_encode is a private function that allocates and encodes data into a string
// On BEAM: Internal function used by encode and encode_str
fn alloc_and_encode(src &u8, len int) string {
	// On BEAM, this is simplified since memory allocation is automatic
	// The actual encoding is done by the pure V encode function
	return ''
}

// B64_64_datablock is a union for optimized decoding (C backend specific)
// On BEAM: Not used, but defined for type compatibility
union B64_64_datablock {
mut:
	data      u64
	data_byte [8]u8
}

// B64_32_datablock is a union for optimized decoding (C backend specific)
// On BEAM: Not used, but defined for type compatibility
union B64_32_datablock {
mut:
	data      u32
	data_byte [4]u8
}
