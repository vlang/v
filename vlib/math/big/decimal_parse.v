module big

import math.bits

const decimal_parse_limit = 1152
const decimal_parse_precompute_limit = 16_384

fn integer_from_decimal_string(characters string) Integer {
	negative := characters[0] == `-`
	mut start := if negative || characters[0] == `+` { 1 } else { 0 }
	for start < characters.len && characters[start] == `0` {
		start++
	}
	if start == characters.len {
		return Integer{}
	}
	text := characters[start..]
	mut powers := []DecimalPower{}
	if text.len > decimal_parse_precompute_limit {
		powers = decimal_powers(text.len)
	}
	value := decimal_parse(text, powers, powers.len - 1)
	return Integer{
		digits: value.digits
		signum: if negative { -1 } else { 1 }
	}
}

fn decimal_parse(text string, powers []DecimalPower, level int) Integer {
	if text.len <= decimal_parse_limit || level < 0 {
		return decimal_parse_small(text)
	}
	power := powers[level]
	if text.len <= power.width {
		return decimal_parse(text, powers, level - 1)
	}
	mid := text.len - power.width
	hi := decimal_parse(text[..mid], powers, level - 1)
	lo := decimal_parse(text[mid..], powers, level - 1)
	return hi * power.value + lo
}

@[direct_array_access]
fn decimal_parse_small(text string) Integer {
	mut digits := []u64{cap: text.len / 18 + 1}
	mut width := text.len % 18
	if width == 0 {
		width = 18
	}
	for start := 0; start < text.len; {
		mut carry := u64(0)
		for i in start .. start + width {
			carry = carry * 10 + u64(text[i] - `0`)
		}
		for i in 0 .. digits.len {
			hi, lo := bits.mul_add_64(digits[i], 1_000_000_000_000_000_000, carry)
			digits[i] = lo & max_digit
			carry = (hi << (64 - digit_bits)) | (lo >> digit_bits)
		}
		if carry != 0 {
			digits << carry
		}
		start += width
		width = 18
	}
	return Integer{
		digits: digits
		signum: if digits.len == 0 { 0 } else { 1 }
	}
}
