// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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

// url_decode returns a decoded URL `string` version of
// the a base64 url encoded `string` passed in `data`.
pub fn url_decode(data string) []u8 {
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
pub fn url_encode(data []u8) string {
	return encode(data).replace_each(['+', '-', '/', '_', '=', ''])
}

// url_encode_str is the string variant of url_encode
pub fn url_encode_str(data string) string {
	return encode_str(data).replace_each(['+', '-', '/', '_', '=', ''])
}

// assemble64 assembles 8 base64 digits into 6 bytes.
// Each digit comes from the decode map.
// Please note: Invalid base64 digits are not expected and not handled.
fn assemble64(n1 u8, n2 u8, n3 u8, n4 u8, n5 u8, n6 u8, n7 u8, n8 u8) u64 {
	return u64(n1) << 58 | u64(n2) << 52 | u64(n3) << 46 | u64(n4) << 40 | u64(n5) << 34 | u64(n6) << 28 | u64(n7) << 22 | u64(n8) << 16
}

// assemble32 assembles 4 base64 digits into 3 bytes.
// Each digit comes from the decode map.
// Please note: Invalid base64 digits are not expected and not handled.
fn assemble32(n1 u8, n2 u8, n3 u8, n4 u8) u32 {
	return u32(n1) << 26 | u32(n2) << 20 | u32(n3) << 14 | u32(n4) << 8
}
