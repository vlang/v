module vlq

import io

const (
	shift                  = u8(5)
	mask                   = u8((1 << shift) - 1)
	continued              = u8(1 << shift)
	max_i64                = u64(9223372036854775807)

	// index start is: byte - vlq.enc_char_special_plus
	enc_index              = [62, 0, 0, 0, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0, 0, 0,
		0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
		22, 23, 24, 25, 0, 0, 0, 0, 0, 0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
		40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51]!

	enc_table              = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

	enc_char_start_au      = 65
	enc_char_end_zu        = 90
	enc_char_start_al      = 97
	enc_char_end_zl        = 122
	enc_char_start_zero    = 48
	enc_char_end_nine      = 57
	enc_char_special_plus  = 43
	enc_char_special_slash = 47
)

[inline]
fn abs64(x i64) u64 {
	return if x < 0 { u64(-x) } else { u64(x) }
}

// Decode a single base64 digit.
[inline]
fn decode64(input u8) u8 {
	$if debug {
		assert input >= vlq.enc_char_special_plus
		assert input <= vlq.enc_char_end_zl
	}
	return u8(vlq.enc_index[input - vlq.enc_char_special_plus])
}

// Decode a single VLQ value from the input stream, returning the value.
//
// # Range
//
// Supports all numbers that can be represented by a sign bit and a 63 bit
// absolute value: `[-(2^63 - 1), 2^63 - 1]`.
//
// Note that `i64::MIN = -(2^63)` cannot be represented in that form, and this
// NOT IMPLEMENTED: function will return `Error::Overflowed` when attempting to decode it.
pub fn decode(mut input io.Reader) ?i64 {
	mut buf := []u8{len: 1}

	mut accum := u64(0)
	mut shifter := 0
	mut digit := u8(0)

	mut keep_going := true
	for keep_going {
		len := input.read(mut buf) or { return error('Unexpected EOF') }
		if len == 0 {
			return error('no content')
		}
		digit = decode64(buf[0])
		keep_going = (digit & vlq.continued) != 0

		digit_value := u64(digit & vlq.mask) << u32(shifter) // TODO: check Overflow

		accum += digit_value
		shifter += vlq.shift
	}

	abs_value := accum / 2
	if abs_value > vlq.max_i64 {
		return error('Overflow')
	}

	// The low bit holds the sign.
	return if (accum & 1) != 0 { (-i64(abs_value)) } else { i64(abs_value) }
}

[inline]
fn encode64(input u8) u8 {
	$if debug {
		assert input < 64
	}
	return vlq.enc_table[input]
}

// Encode a value as Base64 VLQ, sending it to the writer
pub fn encode(value i64, mut output io.Writer) ? {
	signed := value < 0
	mut value_u64 := abs64(value) << 1
	if signed {
		if value_u64 == 0 {
			// Wrapped
			value_u64 = vlq.max_i64 + 1
		}
		value_u64 |= 1
	}
	for {
		mut digit := u8(value_u64) & vlq.mask
		value_u64 >>= vlq.shift
		if value_u64 > 0 {
			digit |= vlq.continued
		}
		bytes := [encode64(digit)]
		output.write(bytes) or { return error('Write failed') }
		if value_u64 == 0 {
			break
		}
	}
}
