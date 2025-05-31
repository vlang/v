// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

import math.big
import crypto.internal.subtle

// default_integer_tag is the default of ASN.1 INTEGER type.
pub const default_integer_tag = Tag{.universal, false, int(TagType.integer)}

// ASN.1 INTEGER.
//
// The INTEGER type value can be a positive or negative number.
// There are no limits imposed on the magnitude of INTEGER values in the ASN.1 standard.
// Its handles number arbitrary length of number with support of `math.big` module.
// But, for sake of safety, we limit the INTEGER limit to follow allowed length in
// definite form of Length part, ie, 1008 bit, or 126 bytes
// The encoding of an integer number shall be primitive.

// Limit of length of INTEGER type, in bytes
// Known big RSA keys is 4096 bits, ie, 512 bytes
const max_integer_length = 2048

// Integer represent Universal class of arbitrary length type of ASN.1 INTEGER.
// The encoding of an integer value shall be primitive.
// If the contents octets of an integer value encoding consist of more than one octet,
// then the bits of the first octet and bit 8 of the second octet.
// 	a) shall not all be ones; and.
// 	b) shall not all be zero.
// NOTE – These rules ensure that an integer value is always encoded in
// the smallest possible number of octets.
pub struct Integer {
pub:
	// underlying integer value with support from `i64` and `big.Integer`
	value IntValue
}

// hex returns Integer value as a hex string.
pub fn (v Integer) hex() string {
	match v.value {
		i64 {
			val := v.value as i64
			return val.hex_full()
		}
		big.Integer {
			val := v.value as big.Integer
			return val.hex()
		}
	}
}

fn (v Integer) str() string {
	return 'Integer ${v.value.str()}'
}

// IntValue represents arbitrary integer value, currently we support
// through primitive 164 type for integer value below < max_i64, and
// use `big.Integer` for support arbitrary length of integer values.
type IntValue = big.Integer | i64

fn (v IntValue) str() string {
	match v {
		i64 {
			val := v as i64
			return val.str()
		}
		big.Integer {
			val := v as big.Integer
			return val.str()
		}
	}
}

// bytes get the bytes representation from underlying IntValue.
fn (v IntValue) bytes() []u8 {
	match v {
		i64 {
			return i64_to_bytes(v)
		}
		big.Integer {
			// if v == big.zero_int or similar big.Integer values that produces empty bytes,
			// returns v.bytes() directly can lead to undesired behavior thats doesn't aligned with
			// ASN.1 INTEGER requirement. See the discussion on the discord about the issues
			// at https://discord.com/channels/592103645835821068/592294828432424960/1230460279733620777
			// so, we do some hack to get the correct value
			// TODO: find the correct way to tackle this
			if v == big.zero_int {
				return [u8(0x00)]
			}
			// todo: proper check of 0 bytes length
			if v.bit_len() == 0 {
				return [u8(0x00)]
			}
			// otherwise, we use v.bytes() directly
			b, _ := v.bytes()
			return b
		}
	}
}

// from_i64 creates new a ASN.1 Integer from i64 v.
pub fn Integer.from_i64(v i64) Integer {
	return Integer{
		value: IntValue(v)
	}
}

// from_int creates a new ASN.1 Integer from int v.
pub fn Integer.from_int(v int) Integer {
	return Integer{
		value: IntValue(i64(v))
	}
}

// from_bigint creates a new ASN.1 Integer from big.Integer b
pub fn Integer.from_bigint(b big.Integer) Integer {
	return Integer{
		value: IntValue(b)
	}
}

// from_string creates a new ASN.1 Integer from decimal string s.
// If your string value is below max_i64, use from_i64 instead
pub fn Integer.from_string(s string) !Integer {
	v := big.integer_from_string(s)!
	return Integer{
		value: IntValue(v)
	}
}

// from_hex creates a new ASN.1 Integer from hex string in x
// where x is a valid hex string without `0x` prefix.
// If your string value is below max_i64, use from_i64 instead
pub fn Integer.from_hex(x string) !Integer {
	s := big.integer_from_radix(x, 16)!
	return Integer{
		value: IntValue(s)
	}
}

// from_bytes creates a new ASN.1 Integer from bytes array in b.
// Its try to parse bytes as in two's complement form.
fn Integer.from_bytes(b []u8) !Integer {
	return Integer.unpack_from_twoscomplement_bytes(b)!
}

// unpack_from_twoscomplement_bytes parses the bytes in b into the Integer
// value in the big-endian two's complement way. If b[0]&80 != 0, the number
// is negative. If b is empty it would be error.
fn Integer.unpack_from_twoscomplement_bytes(b []u8) !Integer {
	// FIXME: should we return error instead ?
	if b.len == 0 {
		return error('Integer: null bytes')
	}
	if b.len > 7 {
		mut num := big.integer_from_bytes(b)
		// negative number
		if b.len > 0 && b[0] & 0x80 > 0 {
			sub := big.one_int.left_shift(u32(b.len) * 8)
			num -= sub
		}

		return Integer{
			value: IntValue(num)
		}
	}
	// use i64
	val := read_i64(b)!
	res := Integer.from_i64(val)
	return res
}

// bytes return underlying bytes array
fn (v Integer) bytes() []u8 {
	return v.value.bytes()
}

// bytes_len returns underlying bytes length
fn (v Integer) bytes_len() int {
	b := v.value.bytes()
	return b.len
}

// tag returns the tag of Integer type element
pub fn (v Integer) tag() Tag {
	return default_integer_tag
}

// payload returns the payload of Integer type element.
pub fn (v Integer) payload() ![]u8 {
	bytes, _ := v.pack_into_twoscomplement_form()!
	return bytes
}

// pack_into_twoscomplement_form serialize Integer in two's-complement rules.
// 	- 	The integer value contains the encoded integer if it is positive, or its two's complement if it is negative.
// 	- 	If the integer is positive but the high order bit is set to 1, a leading 0x00 is added to the content
// 		to indicate that the number is not negative.
// 	-	If the number is negative after applying two's-complement rules, and the most-significant-bit of the
// 		the high order bit of the bytes results isn't set, pad it with 0xff in order to keep the number negative.
fn (v Integer) pack_into_twoscomplement_form() !([]u8, int) {
	match v.value {
		i64 {
			val := v.value as i64
			mut bytes := i64_to_bytes(val)
			return bytes, bytes.len
		}
		big.Integer {
			match v.value.signum {
				0 {
					return [u8(0x00)], 1
				}
				1 {
					mut b := v.bytes()
					// handle the zero issues
					if b.len == 0 {
						return [u8(0x00)], 1
					}
					// If the integer is positive but the high order bit is set to 1, a leading 0x00 is added
					// to the content to indicate that the number is not negative
					if b[0] & 0x80 > 0 {
						b.prepend(u8(0x00))
					}
					return b, b.len
				}
				-1 {
					// A negative number has to be converted to two's-complement form.
					// by invert the number and then subtract it with big(1), or with other mean
					// Flip all of the bits in the value and then add one to the resulting value.
					// If the most-significant-bit isn't set then we'll need to pad the
					// beginning with 0xff in order to keep the number negative.
					negv := v.value.neg()
					negvminus1 := negv - big.one_int
					mut bytes, _ := negvminus1.bytes()
					for i, _ in bytes {
						bytes[i] ^= 0xff
					}
					if bytes.len == 0 || bytes[0] & 0x80 == 0 {
						bytes.prepend(u8(0xff))
					}
					return bytes, bytes.len
				}
				else {
					return error('should unreachable')
				}
			}
		}
	}
}

// equal do checking if integer n was equal to integer m.
// ISSUE?: There are some issues when compared n == m directly,
// its fails even internally its a same, so we provide and use equality check
pub fn (n Integer) equal(m Integer) bool {
	nbytes := n.bytes()
	mbytes := m.bytes()
	// todo: check sign equality
	// m.tag() == n.tag() by definition, no need to check
	return subtle.constant_time_compare(nbytes, mbytes) == 1
}

// Integer.unpack_and_validate deserializes bytes in b into Integer
// in two's complement way and perform validation on this bytes to
// meet der requirement.
fn Integer.unpack_and_validate(b []u8) !Integer {
	if !valid_bytes(b, true) {
		return error('Integer: check return false')
	}
	ret := Integer.unpack_from_twoscomplement_bytes(b)!
	return ret
}

// as_bigint casts Integer value to big.Integer or error on fails.
pub fn (v Integer) as_bigint() !big.Integer {
	if v.value is big.Integer {
		val := v.value as big.Integer
		return val
	}
	return error('Integer not hold big.Integer type')
}

// as_i64 casts Integer value to i64 value or error on fails.
pub fn (v Integer) as_i64() !i64 {
	if v.value is i64 {
		val := v.value as i64
		return val
	}
	return error('Integer not hold i64 type')
}

// parse tries to read and parse into Integer type or return error on fails.
fn Integer.parse(mut p Parser) !Integer {
	tag := p.read_tag()!
	if !tag.equal(default_integer_tag) {
		return error('Get unexected non Integer tag')
	}
	length := p.read_length()!
	if length < 1 {
		return error('Get length < 1 for Integer length')
	}
	bytes := p.read_bytes(length)!
	ret := Integer.from_bytes(bytes)!

	return ret
}

// decode tries to decode bytes array into Integer type or error on fails
fn Integer.decode(bytes []u8) !(Integer, int) {
	return Integer.decode_with_rule(bytes, 0, .der)!
}

// decode_with_rule tries to decode bytes back into ASN.1 Integer.
// Its accepts `loc` params, the location (offset) within bytes where the unpack start from.
// If not sure set to 0 to drive unpacking and rule of `Encodingrule`, currently only support`.der`.
fn Integer.decode_with_rule(bytes []u8, loc int, rule EncodingRule) !(Integer, int) {
	if bytes.len < 3 {
		return error('Integer: bad bytes length')
	}
	tag, length_pos := Tag.decode_with_rule(bytes, loc, rule)!
	if !tag.equal(default_integer_tag) {
		return error('Get unexpected Integer tag')
	}
	length, content_pos := Length.decode_with_rule(bytes, length_pos, rule)!
	payload := if length == 0 {
		[]u8{}
	} else {
		if content_pos + length > bytes.len {
			return error('Not enought bytes to read on')
		}
		unsafe { bytes[content_pos..content_pos + length] }
	}

	// buf := trim_bytes(payload)!
	next := content_pos + length
	result := Integer.from_bytes(payload)!

	return result, next
}

// Utility function
//
fn is_highest_bit_set(src []u8) bool {
	if src.len > 0 {
		return src[0] & 0x80 == 0
	}
	return false
}

fn trim_bytes(src []u8) ![]u8 {
	if src.len == 0 {
		return error('bad src')
	}
	// TODO: removes prepended bytes when its meet criteria
	// positive value but its prepended with 0x00
	if src.len > 1 && src[0] == 0x00 && src[1] & 0x80 > 0 {
		bytes := src[1..]
		return bytes
	}
	// TODO: how to do with multiples 0xff
	if src.len > 1 && src[0] == 0xff && src[1] & 0x80 == 0 {
		bytes := src[1..]
		return bytes
	}
	return src
}

// length_i64 gets bytes length needed to reperesent this i64 value
fn length_i64(val i64) int {
	mut i := val
	mut n := 1

	for i > 127 {
		n++
		i >>= 8
	}

	for i < -128 {
		n++
		i >>= 8
	}

	return n
}

// i64_to_bytes transforms i64 value into bytes representation
fn i64_to_bytes(i i64) []u8 {
	mut n := length_i64(i)
	mut dst := []u8{len: n}
	for j := 0; j < n; j++ {
		dst[j] = u8(i >> u32((n - 1 - j) * 8))
	}
	return dst
}

// read_i64 read src as signed i64
fn read_i64(src []u8) !i64 {
	if !valid_bytes(src, true) {
		return error('i64 check return false')
	}
	mut ret := i64(0)

	if src.len > 8 {
		return error('too large integer')
	}
	for i := 0; i < src.len; i++ {
		ret <<= 8
		ret |= i64(src[i])
	}

	ret <<= 64 - u8(src.len) * 8
	ret >>= 64 - u8(src.len) * 8

	// try to serialize back, and check its matching original one
	// and gives a warning when its not match.

	dst := i64_to_bytes(ret)
	if dst != src {
		eprintln('maybe integer bytes not in shortest form')
	}

	return ret
}

// i32 handling
//
// read_i32 read from bytes
fn read_i32(src []u8) !int {
	if !valid_bytes(src, true) {
		return error('i32 check return false')
	}

	ret := read_i64(src)!
	if ret != i64(int(ret)) {
		return error('integer too large')
	}

	return int(ret)
}

fn length_i32(v i32) int {
	return length_i64(i64(v))
}

fn i32_to_bytes(v i32) []u8 {
	return i64_to_bytes(i64(v))
}
