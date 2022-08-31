// algorthim is adapted from https://github.com/mr-tron/base58 under the MIT license

module base58

import math

// encode_int encodes any integer type to base58 string with Bitcoin alphabet
pub fn encode_int(input int) ?string {
	return encode_int_walpha(input, btc_alphabet)
}

// encode_int_walpha any integer type to base58 string with custom alphabet
pub fn encode_int_walpha(input int, alphabet Alphabet) ?string {
	if input <= 0 {
		return error(@MOD + '.' + @FN + ': input must be greater than zero')
	}

	mut buffer := []u8{}

	mut i := input
	for i > 0 {
		remainder := i % 58
		buffer << alphabet.encode[i8(remainder)]
		// This needs to be casted so byte inputs can
		// be used. i8 because remainder will never be
		// over 58.
		i = i / 58
	}

	return buffer.reverse().bytestr()
}

// encode encodes the input string to base58 with the Bitcoin alphabet
pub fn encode(input string) string {
	return encode_walpha(input, btc_alphabet)
}

// encode_bytes encodes the input array to base58, with the Bitcoin alphabet
pub fn encode_bytes(input []u8) []u8 {
	return encode_walpha_bytes(input, btc_alphabet)
}

// encode_walpha encodes the input string to base58 with a custom aplhabet
pub fn encode_walpha(input string, alphabet Alphabet) string {
	if input.len == 0 {
		return ''
	}
	bin := input.bytes()
	return encode_walpha_bytes(bin, alphabet).bytestr()
}

// encode_walpha encodes the input array to base58 with a custom aplhabet
pub fn encode_walpha_bytes(input []u8, alphabet Alphabet) []u8 {
	if input.len == 0 {
		return []
	}
	mut sz := input.len

	mut zcount := 0
	for zcount < sz && input[zcount] == 0 {
		zcount++
	}

	// It is crucial to make this as short as possible, especially for
	// the usual case of Bitcoin addresses
	sz = zcount + (sz - zcount) * 555 / 406 + 1
	// integer simplification of
	// ceil(log(256)/log(58))

	mut out := []u8{len: sz}
	mut i := 0
	mut high := 0
	mut carry := u32(0)

	high = sz - 1
	for b in input {
		i = sz - 1
		for carry = u32(b); i > high || carry != 0; i-- {
			carry = carry + 256 * u32(out[i])
			out[i] = u8(carry % 58)
			carry /= 58
		}
		high = 1
	}

	// determine additional "zero-gap" in the buffer, aside from zcount
	for i = zcount; i < sz && out[i] == 0; i++ {}

	// now encode the values with actual alphabet in-place
	val := out[i - zcount..]
	sz = val.len
	for i = 0; i < sz; i++ {
		out[i] = alphabet.encode[val[i]]
	}

	return out[..sz]
}

// decode_int decodes base58 string to an integer with Bitcoin alphabet
pub fn decode_int(input string) ?int {
	return decode_int_walpha(input, btc_alphabet)
}

// decode_int_walpha decodes base58 string to an integer with custom alphabet
pub fn decode_int_walpha(input string, alphabet Alphabet) ?int {
	mut total := 0 // to hold the results
	b58 := input.reverse()
	for i, ch in b58 {
		ch_i := alphabet.encode.bytestr().index_u8(ch)
		if ch_i == -1 {
			return error(@MOD + '.' + @FN +
				': input string contains values not found in the provided alphabet')
		}

		val := ch_i * math.pow(58, i)

		total += int(val)
	}

	return total
}

// decode decodes the base58 input string, using the Bitcoin alphabet
pub fn decode(str string) ?string {
	return decode_walpha(str, btc_alphabet)
}

// decode_bytes decodes the base58 encoded input array, using the Bitcoin alphabet
pub fn decode_bytes(input []u8) ?[]u8 {
	return decode_walpha_bytes(input, btc_alphabet)
}

// decode_walpha decodes the base58 encoded input string, using custom alphabet
pub fn decode_walpha(input string, alphabet Alphabet) ?string {
	if input.len == 0 {
		return ''
	}
	bin := input.bytes()
	res := decode_walpha_bytes(bin, alphabet)?
	return res.bytestr()
}

// decode_walpha_bytes decodes the base58 encoded input array using a custom alphabet
pub fn decode_walpha_bytes(input []u8, alphabet Alphabet) ?[]u8 {
	if input.len == 0 {
		return []
	}

	zero := alphabet.encode[0]
	b58sz := input.len

	mut zcount := 0
	for i := 0; i < b58sz && input[i] == zero; i++ {
		zcount++
	}

	mut t := u64(0)
	mut c := u64(0)

	// the 32-bit algorithm stretches the result up to 2x
	mut binu := []u8{len: 2 * ((b58sz * 406 / 555) + 1)}
	mut outi := []u32{len: (b58sz + 3) / 4}

	for _, r in input {
		if r > 127 {
			panic(@MOD + '.' + @FN +
				': high-bit set on invalid digit; outside of ascii range ($r). This should never happen.')
		}
		if alphabet.decode[r] == -1 {
			return error(@MOD + '.' + @FN + ': invalid base58 digit ($r)')
		}

		c = u64(alphabet.decode[r])

		for j := outi.len - 1; j >= 0; j-- {
			t = u64(outi[j]) * 58 + c
			c = t >> 32
			outi[j] = u32(t & 0xffffffff)
		}
	}

	// initial mask depend on b58sz, on further loops it always starts at 24 bits
	mut mask := (u32(b58sz % 4) * 8)
	if mask == 0 {
		mask = 32
	}
	mask -= 8

	mut out_len := 0
	for j := 0; j < outi.len; j++ {
		for mask < 32 {
			binu[out_len] = u8(outi[j] >> mask)
			mask -= 8
			out_len++
		}
		mask = 24
	}

	// find the most significant byte post-decode, if any
	for msb := zcount; msb < binu.len; msb++ { // loop relies on u32 overflow
		if binu[msb] > 0 {
			return binu[msb - zcount..out_len]
		}
	}

	// it's all zeroes
	return binu[..out_len]
}
