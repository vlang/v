module strconv

import strings

// exact_float_decimal returns the exact, unsigned decimal significand and exponent.
// Multiplying by powers of five converts a negative binary exponent into a decimal one.
fn exact_float_decimal(f f64) (string, int) {
	mut bits := Uf64{}
	bits.f = f
	u := unsafe { bits.u }
	exponent := int((u >> 52) & 0x7ff)
	mut mantissa := u & 0x000fffffffffffff
	mut binary_exponent := -1074
	if exponent != 0 {
		mantissa |= u64(1) << 52
		binary_exponent = exponent - 1075
	}
	if mantissa == 0 {
		return '0', 0
	}
	for binary_exponent < 0 && mantissa & 1 == 0 {
		mantissa >>= 1
		binary_exponent++
	}
	mut limbs := []u64{}
	for mantissa > 0 {
		limbs << mantissa % 1000000000
		mantissa /= 1000000000
	}
	decimal_exponent := if binary_exponent < 0 { binary_exponent } else { 0 }
	mut remaining := if binary_exponent < 0 { -binary_exponent } else { binary_exponent }
	for remaining > 0 {
		chunk := if binary_exponent < 0 {
			if remaining > 12 { 12 } else { remaining }
		} else {
			if remaining > 29 { 29 } else { remaining }
		}
		mut multiplier := u64(1)
		for _ in 0 .. chunk {
			multiplier *= if binary_exponent < 0 { u64(5) } else { u64(2) }
		}
		mut carry := u64(0)
		for i in 0 .. limbs.len {
			product := limbs[i] * multiplier + carry
			limbs[i] = product % 1000000000
			carry = product / 1000000000
		}
		if carry > 0 {
			limbs << carry
		}
		remaining -= chunk
	}
	mut decimal_digits := strings.new_builder(limbs.len * 9)
	decimal_digits.write_string(limbs.last().str())
	for i := limbs.len - 2; i >= 0; i-- {
		part := limbs[i].str()
		decimal_digits.write_string('0'.repeat(9 - part.len))
		decimal_digits.write_string(part)
	}
	return decimal_digits.str(), decimal_exponent
}

// rounded_float_integer rounds decimal decimal_digits to an integer, half up.
fn rounded_float_integer(decimal_digits string, decimal_exponent int) string {
	if decimal_exponent >= 0 {
		return decimal_digits + '0'.repeat(decimal_exponent)
	}
	keep := decimal_digits.len + decimal_exponent
	if keep < 0 {
		return '0'
	}
	round_up := decimal_digits[keep] >= `5`
	if keep == 0 {
		return if round_up { '1' } else { '0' }
	}
	mut result := decimal_digits[..keep].bytes()
	if round_up {
		for i := result.len - 1; i >= 0; i-- {
			if result[i] < `9` {
				result[i]++
				return result.bytestr()
			}
			result[i] = `0`
		}
		return '1' + result.bytestr()
	}
	return result.bytestr()
}

// format_float_magnitude adds the sign and field padding supplied by BF_param.
fn format_float_magnitude(magnitude string, p BF_param) string {
	decimal_digits := if p.rm_tail_zero { remove_tail_zeros(magnitude) } else { magnitude }
	sign := if !p.positive {
		'-'
	} else if p.sign_flag {
		'+'
	} else {
		''
	}
	padding := if p.len0 > decimal_digits.len + sign.len {
		p.len0 - decimal_digits.len - sign.len
	} else {
		0
	}
	pad := p.pad_ch.ascii_str().repeat(padding)
	if p.align == .left {
		return sign + decimal_digits + pad
	}
	if p.pad_ch == `0` {
		return sign + pad + decimal_digits
	}
	return pad + sign + decimal_digits
}

fn exact_float_fixed(f f64, requested_precision int) string {
	mut bits := Uf64{}
	bits.f = f
	u := unsafe { bits.u }
	if (u >> 52) & 0x7ff == 0x7ff {
		return f64_to_str(f, 17)
	}
	precision := if requested_precision > 0 { requested_precision } else { 0 }
	decimal_digits, exponent := exact_float_decimal(f)
	mut integer := rounded_float_integer(decimal_digits, exponent + precision)
	if integer.len <= precision {
		integer = '0'.repeat(precision + 1 - integer.len) + integer
	}
	point := integer.len - precision
	magnitude := if precision == 0 { integer } else { integer[..point] + '.' + integer[point..] }
	return if u >> 63 != 0 { '-' + magnitude } else { magnitude }
}

fn exact_float_scientific(f f64, requested_precision int) string {
	mut bits := Uf64{}
	bits.f = f
	u := unsafe { bits.u }
	if (u >> 52) & 0x7ff == 0x7ff {
		return f64_to_str(f, 17)
	}
	precision := if requested_precision > 0 { requested_precision } else { 0 }
	decimal_digits, decimal_exponent := exact_float_decimal(f)
	mut exponent := decimal_digits.len + decimal_exponent - 1
	mut rounded := rounded_float_integer(decimal_digits, precision + 1 - decimal_digits.len)
	if rounded.len > precision + 1 {
		exponent++
		rounded = rounded[..precision + 1]
	}
	exp_digits := (if exponent < 0 { -exponent } else { exponent }).str()
	exp_sign := if exponent < 0 { 'e-' } else { 'e+' }
	exp_padding := if exp_digits.len < 2 { '0' } else { '' }
	mantissa := if precision == 0 { rounded } else { rounded[..1] + '.' + rounded[1..] }
	sign := if u >> 63 != 0 { '-' } else { '' }
	return sign + mantissa + exp_sign + exp_padding + exp_digits
}
