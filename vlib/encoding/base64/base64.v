// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Based off:   https://github.com/golang/go/blob/master/src/encoding/base64/base64.go
// Last commit: https://github.com/golang/go/commit/9a93baf4d7d13d7d5c67388c93960d78abc8e11e
module base64

const (
	index        = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 62, 63, 62, 62, 63, 52, 53, 54, 55,
		56, 57, 58, 59, 60, 61, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
		13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0, 0, 0, 0, 63, 0, 26, 27, 28, 29,
		30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51]!
	ending_table = [0, 2, 1]!
	enc_table    = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
)

union B64_64_datablock {
mut:
	data      u64
	data_byte [8]byte
}

union B64_32_datablock {
mut:
	data      u32
	data_byte [4]byte
}

// decode decodes the base64 encoded `string` value passed in `data`.
// Please note: If you need to decode many strings repeatedly, take a look at `decode_in_buffer`.
// Example: assert base64.decode('ViBpbiBiYXNlIDY0') == 'V in base 64'
pub fn decode(data string) []byte {
	size := data.len * 3 / 4
	if size <= 0 || data.len % 4 != 0 {
		return []
	}
	unsafe {
		buffer := malloc(size)
		n := decode_in_buffer(data, buffer)
		return buffer.vbytes(n)
	}
}

// decode_str is the string variant of decode
pub fn decode_str(data string) string {
	size := data.len * 3 / 4
	if size <= 0 || data.len % 4 != 0 {
		return ''
	}
	unsafe {
		buffer := malloc_noscan(size + 1)
		buffer[size] = 0
		return tos(buffer, decode_in_buffer(data, buffer))
	}
}

// encode encodes the `[]byte` value passed in `data` to base64.
// Please note: base64 encoding returns a `string` that is ~ 4/3 larger than the input.
// Please note: If you need to encode many strings repeatedly, take a look at `encode_in_buffer`.
// Example: assert base64.encode('V in base 64') == 'ViBpbiBiYXNlIDY0'
pub fn encode(data []byte) string {
	return alloc_and_encode(data.data, data.len)
}

// encode_str is the string variant of encode
pub fn encode_str(data string) string {
	return alloc_and_encode(data.str, data.len)
}

// alloc_and_encode is a private function that allocates and encodes data into a string
// Used by encode and encode_str
fn alloc_and_encode(src &byte, len int) string {
	size := 4 * ((len + 2) / 3)
	if size <= 0 {
		return ''
	}
	unsafe {
		buffer := malloc_noscan(size + 1)
		buffer[size] = 0
		return tos(buffer, encode_from_buffer(buffer, src, len))
	}
}

// url_decode returns a decoded URL `string` version of
// the a base64 url encoded `string` passed in `data`.
pub fn url_decode(data string) []byte {
	mut result := data.replace_each(['-', '+', '_', '/'])
	match result.len % 4 {
		// Pad with trailing '='s
		2 { result += '==' } // 2 pad chars
		3 { result += '=' } // 1 pad char
		else {} // no padding
	}
	return decode(result)
}

// url_decode_str is the string variant of url_decode
pub fn url_decode_str(data string) string {
	mut result := data.replace_each(['-', '+', '_', '/'])
	match result.len % 4 {
		// Pad with trailing '='s
		2 { result += '==' } // 2 pad chars
		3 { result += '=' } // 1 pad char
		else {} // no padding
	}
	return decode_str(result)
}

// url_encode returns a base64 URL encoded `string` version
// of the value passed in `data`.
pub fn url_encode(data []byte) string {
	return encode(data).replace_each(['+', '-', '/', '_', '=', ''])
}

// url_encode_str is the string variant of url_encode
pub fn url_encode_str(data string) string {
	return encode_str(data).replace_each(['+', '-', '/', '_', '=', ''])
}

// assemble64 assembles 8 base64 digits into 6 bytes.
// Each digit comes from the decode map.
// Please note: Invalid base64 digits are not expected and not handled.
fn assemble64(n1 byte, n2 byte, n3 byte, n4 byte, n5 byte, n6 byte, n7 byte, n8 byte) u64 {
	return u64(n1) << 58 | u64(n2) << 52 | u64(n3) << 46 | u64(n4) << 40 | u64(n5) << 34 | u64(n6) << 28 | u64(n7) << 22 | u64(n8) << 16
}

// assemble32 assembles 4 base64 digits into 3 bytes.
// Each digit comes from the decode map.
// Please note: Invalid base64 digits are not expected and not handled.
fn assemble32(n1 byte, n2 byte, n3 byte, n4 byte) u32 {
	return u32(n1) << 26 | u32(n2) << 20 | u32(n3) << 14 | u32(n4) << 8
}

// decode_in_buffer decodes the base64 encoded `string` reference passed in `data` into `buffer`.
// decode_in_buffer returns the size of the decoded data in the buffer.
// Please note: The `buffer` should be large enough (i.e. 3/4 of the data.len, or larger)
// to hold the decoded data.
// Please note: This function does NOT allocate new memory, and is thus suitable for handling very large strings.
pub fn decode_in_buffer(data &string, buffer &byte) int {
	return decode_from_buffer(buffer, data.str, data.len)
}

// decode_from_buffer decodes the base64 encoded ASCII bytes from `data` into `buffer`.
// decode_from_buffer returns the size of the decoded data in the buffer.
// Please note: The `buffer` should be large enough (i.e. 3/4 of the data.len, or larger)
// to hold the decoded data.
// Please note: This function does NOT allocate new memory, and is thus suitable for handling very large strings.
pub fn decode_in_buffer_bytes(data []byte, buffer &byte) int {
	return decode_from_buffer(buffer, data.data, data.len)
}

// decode_from_buffer decodes the base64 encoded ASCII bytes from `src` into `dest`.
// decode_from_buffer returns the size of the decoded data in the buffer.
// Please note: The `dest` buffer should be large enough (i.e. 3/4 of the `src_len`, or larger)
// to hold the decoded data.
// Please note: This function does NOT allocate new memory, and is thus suitable for handling very large strings.
// Please note: This function is for internal base64 decoding
fn decode_from_buffer(dest &byte, src &byte, src_len int) int {
	if src_len < 4 {
		return 0
	}

	mut padding := 0
	if unsafe { src[src_len - 1] == `=` } {
		if unsafe { src[src_len - 2] == `=` } {
			padding = 2
		} else {
			padding = 1
		}
	}

	mut d := unsafe { src }
	mut b := unsafe { dest }

	unsafe {
		mut n_decoded_bytes := 0 // padding bytes are also counted towards this.
		mut si := 0

		mut datablock_64 := B64_64_datablock{
			data: 0
		}
		mut datablock_32 := B64_32_datablock{
			data: 0
		}

		for src_len - si >= 8 {
			// Converting 8 bytes of input into 6 bytes of output. Storing these in the upper bytes of an u64.
			datablock_64.data = assemble64(byte(base64.index[d[si + 0]]), byte(base64.index[d[si + 1]]),
				byte(base64.index[d[si + 2]]), byte(base64.index[d[si + 3]]), byte(base64.index[d[
				si + 4]]), byte(base64.index[d[si + 5]]), byte(base64.index[d[si + 6]]),
				byte(base64.index[d[si + 7]]))

			// Reading out the individual bytes from the u64. Watch out with endianess.
			$if little_endian {
				b[n_decoded_bytes + 0] = datablock_64.data_byte[7]
				b[n_decoded_bytes + 1] = datablock_64.data_byte[6]
				b[n_decoded_bytes + 2] = datablock_64.data_byte[5]
				b[n_decoded_bytes + 3] = datablock_64.data_byte[4]
				b[n_decoded_bytes + 4] = datablock_64.data_byte[3]
				b[n_decoded_bytes + 5] = datablock_64.data_byte[2]
			} $else {
				b[n_decoded_bytes + 0] = datablock_64.data_byte[0]
				b[n_decoded_bytes + 1] = datablock_64.data_byte[1]
				b[n_decoded_bytes + 2] = datablock_64.data_byte[2]
				b[n_decoded_bytes + 3] = datablock_64.data_byte[3]
				b[n_decoded_bytes + 4] = datablock_64.data_byte[4]
				b[n_decoded_bytes + 5] = datablock_64.data_byte[5]
			}

			n_decoded_bytes += 6
			si += 8
		}

		for src_len - si >= 4 {
			datablock_32.data = assemble32(byte(base64.index[d[si + 0]]), byte(base64.index[d[si + 1]]),
				byte(base64.index[d[si + 2]]), byte(base64.index[d[si + 3]]))

			$if little_endian {
				b[n_decoded_bytes + 0] = datablock_32.data_byte[3]
				b[n_decoded_bytes + 1] = datablock_32.data_byte[2]
				b[n_decoded_bytes + 2] = datablock_32.data_byte[1]
				b[n_decoded_bytes + 3] = datablock_32.data_byte[0]
			} $else {
				b[n_decoded_bytes + 0] = datablock_32.data_byte[0]
				b[n_decoded_bytes + 1] = datablock_32.data_byte[1]
				b[n_decoded_bytes + 2] = datablock_32.data_byte[2]
				b[n_decoded_bytes + 3] = datablock_32.data_byte[3]
			}

			n_decoded_bytes += 3
			si += 4
		}

		return n_decoded_bytes - padding
	}
}

// encode_in_buffer base64 encodes the `[]byte` passed in `data` into `buffer`.
// encode_in_buffer returns the size of the encoded data in the buffer.
// Please note: The buffer should be large enough (i.e. 4/3 of the data.len, or larger) to hold the encoded data.
// Please note: The function does NOT allocate new memory, and is suitable for handling very large strings.
pub fn encode_in_buffer(data []byte, buffer &byte) int {
	return encode_from_buffer(buffer, data.data, data.len)
}

// encode_from_buffer will perform encoding from any type of src buffer
// and write the bytes into `dest`.
// Please note: The `dest` buffer should be large enough (i.e. 4/3 of the src_len, or larger) to hold the encoded data.
// Please note: This function is for internal base64 encoding
fn encode_from_buffer(dest &byte, src &byte, src_len int) int {
	if src_len == 0 {
		return 0
	}
	output_length := 4 * ((src_len + 2) / 3)

	mut d := unsafe { src }
	mut b := unsafe { dest }
	etable := base64.enc_table.str

	mut di := 0
	mut si := 0
	n := (src_len / 3) * 3
	for si < n {
		// Convert 3x 8bit source bytes into 4 bytes
		unsafe {
			val := u32(d[si + 0]) << 16 | u32(d[si + 1]) << 8 | u32(d[si + 2])

			b[di + 0] = etable[val >> 18 & 0x3F]
			b[di + 1] = etable[val >> 12 & 0x3F]
			b[di + 2] = etable[val >> 6 & 0x3F]
			b[di + 3] = etable[val & 0x3F]
		}
		si += 3
		di += 4
	}

	remain := src_len - si
	if remain == 0 {
		return output_length
	}

	// Add the remaining small block and padding
	unsafe {
		mut val := u32(d[si + 0]) << 16
		if remain == 2 {
			val |= u32(d[si + 1]) << 8
		}

		b[di + 0] = etable[val >> 18 & 0x3F]
		b[di + 1] = etable[val >> 12 & 0x3F]

		match remain {
			2 {
				b[di + 2] = etable[val >> 6 & 0x3F]
				b[di + 3] = byte(`=`)
			}
			1 {
				b[di + 2] = byte(`=`)
				b[di + 3] = byte(`=`)
			}
			else {
				panic('base64: This case should never occur.')
			}
		}
	}
	return output_length
}
