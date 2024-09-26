// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Module base32 implements base32 encoding as specified by RFC 4648.
// Based off:   https://github.com/golang/go/blob/master/src/encoding/base32/base32.go
// Last commit: https://github.com/golang/go/commit/e1b62efaf33988a5153510898d37309cee78f26e

// TODO: standardize fn naming conventions & strip newlines on input & clean up an go remnant's

module base32

pub const std_padding = `=` // Standard padding character

pub const no_padding = u8(-1) // No padding

pub const std_alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ234567'.bytes()
pub const hex_alphabet = '0123456789ABCDEFGHIJKLMNOPQRSTUV'.bytes()

struct Encoding {
	padding_char u8
	alphabet     []u8
mut:
	decode_map [256]u8
}

// decode_string_to_string decodes a V string `src` using Base32
// and returns the decoded string or a `corrupt_input_error_msg` error.
pub fn decode_string_to_string(src string) !string {
	return decode_to_string(src.bytes())
}

// decode_to_string decodes a byte array `src` using Base32
// and returns the decoded string or a `corrupt_input_error_msg` error.
pub fn decode_to_string(src []u8) !string {
	res := decode(src)!
	return res.bytestr()
}

// decode decodes a byte array `src` using Base32
// and returns the decoded bytes or a `corrupt_input_error_msg` error.
pub fn decode(src []u8) ![]u8 {
	mut e := new_encoding(std_alphabet)
	return e.decode(src)
}

// encode_string_to_string encodes the V string `src` using Base32
// and returns the encoded bytes as a V string.
pub fn encode_string_to_string(src string) string {
	return encode_to_string(src.bytes())
}

// encode_to_string encodes a byte array `src` using Base32 and
// returns the encoded bytes as a V string.
pub fn encode_to_string(src []u8) string {
	return encode(src).bytestr()
}

// encode encodes a byte array `src` using Base32 and returns the
// encoded bytes.
pub fn encode(src []u8) []u8 {
	e := new_encoding(std_alphabet)
	return e.encode(src)
}

// encode_to_string encodes the Base32 encoding of `src` with
// the encoding `enc` and returns the encoded bytes as a V string.
pub fn (enc &Encoding) encode_to_string(src []u8) string {
	return enc.encode(src).bytestr()
}

// encode_string_to_string encodes a V string `src` using Base32 with
// the encoding `enc` and returns the encoded bytes as a V string.
pub fn (enc &Encoding) encode_string_to_string(src string) string {
	return enc.encode(src.bytes()).bytestr()
}

// new_std_encoding creates a standard Base32 `Encoding` as defined in
// RFC 4648.
pub fn new_std_encoding() Encoding {
	return new_encoding_with_padding(std_alphabet, std_padding)
}

// new_std_encoding creates a standard Base32 `Encoding` identical to
// `new_std_encoding` but with a specified character `padding`,
// or `no_padding` to disable padding.
// The `padding` character must not be '\r' or '\n', must not
// be contained in the `Encoding`'s alphabet and must be a rune equal or
// below '\xff'.
pub fn new_std_encoding_with_padding(padding u8) Encoding {
	return new_encoding_with_padding(std_alphabet, padding)
}

// new_encoding returns a Base32 `Encoding` with standard
// `alphabet`s and standard padding.
pub fn new_encoding(alphabet []u8) Encoding {
	return new_encoding_with_padding(alphabet, std_padding)
}

// new_encoding_with_padding returns a Base32 `Encoding` with specified
// encoding `alphabet`s and a specified `padding_char`.
// The `padding_char` must not be '\r' or '\n', must not
// be contained in the `Encoding`'s alphabet and must be a rune equal or
// below '\xff'.
pub fn new_encoding_with_padding(alphabet []u8, padding_char u8) Encoding {
	if padding_char == `\r` || padding_char == `\n` || padding_char > 0xff {
		panic('invalid padding')
	}

	for i := 0; i < alphabet.len; i++ {
		if alphabet[i] == padding_char {
			panic('padding contained in alphabet')
		}
	}

	mut decode_map := [256]u8{}
	for i in 0 .. alphabet.len {
		decode_map[alphabet[i]] = u8(i)
	}

	return Encoding{
		alphabet:     alphabet
		padding_char: padding_char
		decode_map:   decode_map
	}
}

// encode encodes `src` using the encoding `enc`, writing
// and returning encoded_len(src.len) u8s.
//
// The encoding pads the output to a multiple of 8 u8s,
// so encode is not appropriate for use on individual blocks
// of a large data stream.
fn (enc &Encoding) encode(src []u8) []u8 {
	mut buf := []u8{len: enc.encoded_len(src.len)}
	mut dst := unsafe { buf }
	enc.encode_(src, mut dst)
	return buf
}

// encode_ encodes `src` using the encoding `enc`, writing
// encoded_len(src.len) u8s to `dst`.
//
// The encoding pads the output to a multiple of 8 u8s,
// so encode_ is not appropriate for use on individual blocks
// of a large data stream.
fn (enc &Encoding) encode_(src_ []u8, mut dst []u8) {
	mut src := unsafe { src_ }
	for src.len > 0 {
		mut b := [8]u8{}

		// Unpack 8x 5-bit source blocks into a 5 u8
		// destination quantum
		if src.len > 4 {
			b[7] = src[4] & 0x1F
			b[6] = src[4] >> 5
		}
		if src.len >= 4 {
			b[6] |= (src[3] << 3) & 0x1F
			b[5] = (src[3] >> 2) & 0x1F
			b[4] = src[3] >> 7
		}
		if src.len >= 3 {
			b[4] |= (src[2] << 1) & 0x1F
			b[3] = (src[2] >> 4) & 0x1F
		}
		if src.len >= 2 {
			b[3] |= (src[1] << 4) & 0x1F
			b[2] = (src[1] >> 1) & 0x1F
			b[1] = (src[1] >> 6) & 0x1F
		}
		if src.len >= 1 {
			b[1] |= (src[0] << 2) & 0x1F
			b[0] = src[0] >> 3
		}

		// Encode 5-bit blocks using the base32 alphabet
		if dst.len >= 8 {
			// Common case, unrolled for extra performance
			dst[0] = enc.alphabet[b[0] & 31]
			dst[1] = enc.alphabet[b[1] & 31]
			dst[2] = enc.alphabet[b[2] & 31]
			dst[3] = enc.alphabet[b[3] & 31]
			dst[4] = enc.alphabet[b[4] & 31]
			dst[5] = enc.alphabet[b[5] & 31]
			dst[6] = enc.alphabet[b[6] & 31]
			dst[7] = enc.alphabet[b[7] & 31]
		} else {
			for i := 0; i < dst.len; i++ {
				dst[i] = enc.alphabet[b[i] & 31]
			}
		}

		// Pad the final quantum
		if src.len < 5 {
			if enc.padding_char == no_padding {
				break
			}

			dst[7] = enc.padding_char
			if src.len < 4 {
				dst[6] = enc.padding_char
				dst[5] = enc.padding_char
				if src.len < 3 {
					dst[4] = enc.padding_char
					if src.len < 2 {
						dst[3] = enc.padding_char
						dst[2] = enc.padding_char
					}
				}
			}

			break
		}
		src = unsafe { src[5..] }
		dst = unsafe { dst[8..] }
	}
}

fn (enc &Encoding) encoded_len(n int) int {
	if enc.padding_char == no_padding {
		return (n * 8 + 4) / 5
	}
	return (n + 4) / 5 * 8
}

// decode_string decodes a V string `src` using Base32 with the encoding `enc`
// and returns the decoded bytes or a `corrupt_input_error_msg` error.
pub fn (enc &Encoding) decode_string(src string) ![]u8 {
	return enc.decode(src.bytes())
	// mut buf := strip_newlines(src.bytes())
	// mut dst := unsafe { buf }
	// // l := strip_newlines(mut buf)
	// n, _ := enc.decode_(buf, mut dst)?
	// return buf[..n]
}

// decode_string_to_string decodes a V string `src` using Base32 with the
// encoding `enc` and returns the decoded V string or a `corrupt_input_error_msg` error.
pub fn (enc &Encoding) decode_string_to_string(src string) !string {
	decoded := enc.decode_string(src)!
	return decoded.bytestr()
}

// decode decodes `src` using the encoding `enc`. It returns the decoded bytes
// written or a `corrupt_input_error_msg` error.
// New line characters (\r and \n) are ignored.
pub fn (enc &Encoding) decode(src []u8) ![]u8 {
	mut buf := []u8{len: src.len}
	// mut dst := unsafe { buf }
	// l := strip_newlines(mut dst, src)
	// n, _ := enc.decode_(src[..l], mut dst) or {
	// src := strip_newlines(src_)
	n, _ := enc.decode_(src, mut buf) or { return err }
	return buf[..n]
}

// decode_ returns the number of bytes written and a boolean value, which
// indicates if end-of-message padding was encountered and thus any
// additional data is an error. This method assumes that src has been
// stripped of all supported whitespace (`\r` and `\n`).
fn (enc &Encoding) decode_(src_ []u8, mut dst []u8) !(int, bool) {
	mut src := unsafe { src_ }
	mut n := 0
	mut end := false
	// Lift the nil check outside of the loop.
	// _ = enc.decode_map

	mut dsti := 0
	olen := src.len

	for src.len > 0 && !end {
		// Decode quantum using the base32 alphabet
		mut dbuf := [8]u8{}
		mut dlen := 8

		for j := 0; j < 8; {
			if src.len == 0 {
				if enc.padding_char != no_padding {
					// We have reached the end and are missing padding
					// return n, false, corrupt_input_error(olen - src.len - j)
					return error(corrupt_input_error_msg(olen - src.len - j))
				}
				// We have reached the end and are not expecting any padding
				dlen, end = j, true
				break
			}
			in0 := src[0]
			unsafe {
				src = src[1..]
			}
			if in0 == enc.padding_char && j >= 2 && src.len < 8 {
				// We`ve reached the end and there`s padding
				if src.len + j < 8 - 1 {
					// not enough padding
					// return n, false, corrupt_input_error(olen)
					return error(corrupt_input_error_msg(olen))
				}
				for k := 0; k < 8 - 1 - j; k++ {
					if src.len > k && src[k] != enc.padding_char {
						// incorrect padding
						// return n, false, corrupt_input_error(olen - src.len + k - 1)
						return error(corrupt_input_error_msg(olen - src.len + k - 1))
					}
				}
				dlen, end = j, true
				// 7, 5 and 2 are not valid padding lengths, and so 1, 3 and 6 are not
				// valid dlen values. See RFC 4648 Section 6 'Base 32 Encoding' listing
				// the five valid padding lengths, and Section 9 'Illustrations and
				// Examples' for an illustration for how the 1st, 3rd and 6th base32
				// src u8s do not yield enough information to decode a dst u8.
				if dlen == 1 || dlen == 3 || dlen == 6 {
					// return n, false, corrupt_input_error(olen - src.len - 1)
					return error(corrupt_input_error_msg(olen - src.len - 1))
				}
				break
			}

			dbuf[j] = enc.decode_map[in0]
			if dbuf[j] == 0xFF {
				// return n, false, corrupt_input_error(olen - src.len - 1)
				return error(corrupt_input_error_msg(olen - src.len - 1))
			}
			j++
		}

		// Pack 8x 5-bit source blocks into 5 u8 destination
		// quantum
		if dlen == 8 {
			dst[dsti + 4] = dbuf[6] << 5 | dbuf[7]
			n++
		}
		if dlen >= 7 {
			dst[dsti + 3] = dbuf[4] << 7 | dbuf[5] << 2 | dbuf[6] >> 3
			n++
		}
		if dlen >= 5 {
			dst[dsti + 2] = dbuf[3] << 4 | dbuf[4] >> 1
			n++
		}
		if dlen >= 4 {
			dst[dsti + 1] = dbuf[1] << 6 | dbuf[2] << 1 | dbuf[3] >> 4
			n++
		}
		if dlen >= 2 {
			dst[dsti + 0] = dbuf[0] << 3 | dbuf[1] >> 2
			n++
		}
		dsti += 5
	}
	return n, end
}

// strip_newlines removes newline characters and returns the number
// of non-newline characters copied to dst.
// fn strip_newlines(mut dst []u8, src []u8) int {
// 	mut offset := 0
// 	for b in src {
// 		if b in [`\r`, `\n`] {
// 			continue
// 		}
// 		dst[offset] = b
// 		offset++
// 	}
// 	return offset
// }
fn strip_newlines(src []u8) []u8 {
	mut dst := []u8{}
	for b in src {
		if b in [`\r`, `\n`] {
			continue
		}
		dst << b
	}
	return dst
}

fn corrupt_input_error_msg(e int) string {
	// return error('illegal base32 data at input byte ' + strconv.FormatInt(int64(e), 10)
	return 'illegal base32 data at input byte ${e}'
}
