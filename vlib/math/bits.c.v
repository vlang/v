module math

// inf returns positive infinity if sign >= 0, negative infinity if sign < 0.
pub fn inf(sign int) f64 {
	v := if sign >= 0 { uvinf } else { uvneginf }
	return f64_from_bits(v)
}

// nan returns an IEEE 754 ``not-a-number'' value.
pub fn nan() f64 {
	return f64_from_bits(uvnan)
}
