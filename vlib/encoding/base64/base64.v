// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
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

// decode decodes the base64 encoded `string` value passed in `data`.
// Please note: If you need to decode many strings repeatedly, take a look at `decode_in_buffer`.
// Example: assert base64.decode('ViBpbiBiYXNlIDY0') == 'V in base 64'
pub fn decode(data string) []byte {
	size := data.len * 3 / 4
	if size <= 0 {
		return []
	}
	unsafe {
		buffer := malloc(size)
		n := decode_in_buffer(data, buffer)
		return array{element_size: 1, data: buffer, len: n, cap: size}
	}
}

// decode_str is the string variant of decode
pub fn decode_str(data string) string {
	size := data.len * 3 / 4
	if size <= 0 {
		return ''
	}
	unsafe {
		buffer := malloc(size)
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
fn alloc_and_encode(src byteptr, len int) string {
	size := 4 * ((len + 2) / 3)
	if size <= 0 {
		return ''
	}
	unsafe {
		buffer := malloc(size)
		return tos(buffer, encode_from_buffer(buffer, src, len))
	}
}

// url_decode returns a decoded URL `string` version of
// the a base64 url encoded `string` passed in `data`.
pub fn url_decode(data string) []byte {
	mut result := data.replace_each(['-', '+', '_', '/'])
	match result.len % 4 { // Pad with trailing '='s
		2 { result += '==' } // 2 pad chars
		3 { result += '=' } // 1 pad char
		else {} // no padding
	}
	return decode(result)
}

// url_decode_str is the string variant of url_decode
pub fn url_decode_str(data string) string {
	mut result := data.replace_each(['-', '+', '_', '/'])
	match result.len % 4 { // Pad with trailing '='s
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

// decode_in_buffer decodes the base64 encoded `string` reference passed in `data` into `buffer`.
// decode_in_buffer returns the size of the decoded data in the buffer.
// Please note: The `buffer` should be large enough (i.e. 3/4 of the data.len, or larger)
// to hold the decoded data.
// Please note: This function does NOT allocate new memory, and is thus suitable for handling very large strings.
pub fn decode_in_buffer(data &string, buffer byteptr) int {
	mut padding := 0
	if data.ends_with('=') {
		if data.ends_with('==') {
			padding = 2
		} else {
			padding = 1
		}
	}
	// input_length is the length of meaningful data
	input_length := data.len - padding
	output_length := input_length * 3 / 4

	mut i := 0
	mut j := 0
	mut b := &byte(0)
	mut d := &byte(0)
	unsafe {
		d = byteptr(data.str)
		b = byteptr(buffer)
	}
	for i < input_length {
		mut char_a := 0
		mut char_b := 0
		mut char_c := 0
		mut char_d := 0
		if i < input_length {
			char_a = index[unsafe { d[i] }]
			i++
		}
		if i < input_length {
			char_b = index[unsafe { d[i] }]
			i++
		}
		if i < input_length {
			char_c = index[unsafe { d[i] }]
			i++
		}
		if i < input_length {
			char_d = index[unsafe { d[i] }]
			i++
		}

		decoded_bytes := (char_a << 18) | (char_b << 12) | (char_c << 6) | (char_d << 0)
		unsafe {
			b[j] = byte(decoded_bytes >> 16)
			b[j + 1] = byte((decoded_bytes >> 8) & 0xff)
			b[j + 2] = byte((decoded_bytes >> 0) & 0xff)
		}
		j += 3
	}
	return output_length
}

// encode_in_buffer base64 encodes the `[]byte` passed in `data` into `buffer`.
// encode_in_buffer returns the size of the encoded data in the buffer.
// Please note: The buffer should be large enough (i.e. 4/3 of the data.len, or larger) to hold the encoded data.
// Please note: The function does NOT allocate new memory, and is suitable for handling very large strings.
pub fn encode_in_buffer(data []byte, buffer byteptr) int {
	return encode_from_buffer(buffer, data.data, data.len)
}

// encode_from_buffer will perform encoding from any type of src buffer
// and write the bytes into `dest`.
// Please note: The `dest` buffer should be large enough (i.e. 4/3 of the src_len, or larger) to hold the encoded data.
// Please note: This function is for internal base64 encoding
fn encode_from_buffer(dest byteptr, src byteptr, src_len int) int {
	input_length := src_len
	output_length := 4 * ((input_length + 2) / 3)

	mut i := 0
	mut j := 0

	mut d := src
	mut b := dest
	mut etable := byteptr(enc_table.str)
	for i < input_length {
		mut octet_a := 0
		mut octet_b := 0
		mut octet_c := 0

		if i < input_length {
			octet_a = int(unsafe { d[i] })
			i++
		}
		if i < input_length {
			octet_b = int(unsafe { d[i] })
			i++
		}
		if i < input_length {
			octet_c = int(unsafe { d[i] })
			i++
		}

		triple := ((octet_a << 0x10) + (octet_b << 0x08) + octet_c)

		unsafe {
			b[j] = etable[(triple >> 3 * 6) & 63] // 63 is 0x3F
			b[j + 1] = etable[(triple >> 2 * 6) & 63]
			b[j + 2] = etable[(triple >> 1 * 6) & 63]
			b[j + 3] = etable[(triple >> 0 * 6) & 63]
		}
		j += 4
	}

	padding_length := ending_table[input_length % 3]
	for i = 0; i < padding_length; i++ {
		unsafe {
			b[output_length - 1 - i] = `=`
		}
	}
	return output_length
}
