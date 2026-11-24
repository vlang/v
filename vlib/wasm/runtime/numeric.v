module runtime

import math
import math.bits

fn integer_binary(op u16, a u64, b u64, wide bool) !u64 {
	x := if wide { a } else { u64(u32(a)) }
	y := if wide { b } else { u64(u32(b)) }
	sx := if wide { i64(x) } else { i64(i32(u32(x))) }
	sy := if wide { i64(y) } else { i64(i32(u32(y))) }
	shift := int(y & (if wide { u64(63) } else { u64(31) }))
	mut result := u64(0)
	match op {
		0 { result = x + y }
		1 { result = x - y }
		2 { result = x * y }
		3, 4, 5, 6 {
			if y == 0 { return error('integer divide by zero') }
			minimum := if wide { i64(-9223372036854775807) - 1 } else { i64(-2147483648) }
			if op == 3 && sx == minimum && sy == -1 { return error('integer overflow') }
			result = match op {
				3 { u64(sx / sy) }
				4 { x / y }
				5 {
					if sy == -1 { u64(0) } else { u64(sx % sy) }
				}
				else { x % y }
			}
		}
		7 { result = x & y }
		8 { result = x | y }
		9 { result = x ^ y }
		10 { result = x << shift }
		11 { result = u64(sx >> shift) }
		12 { result = x >> shift }
		13 {
			result = if wide {
				bits.rotate_left_64(x, shift)
			} else {
				u64(bits.rotate_left_32(u32(x), shift))
			}
		}
		14 {
			result = if wide {
				bits.rotate_left_64(x, -shift)
			} else {
				u64(bits.rotate_left_32(u32(x), -shift))
			}
		}
		else {}
	}
	return if wide { result } else { u64(u32(result)) }
}

fn integer_compare(op u16, a u64, b u64, wide bool) u64 {
	x := if wide { a } else { u64(u32(a)) }
	y := if wide { b } else { u64(u32(b)) }
	sx := if wide { i64(x) } else { i64(i32(u32(x))) }
	sy := if wide { i64(y) } else { i64(i32(u32(y))) }
	return u64(match op {
		0 { x == y }
		1 { x != y }
		2 { sx < sy }
		3 { x < y }
		4 { sx > sy }
		5 { x > y }
		6 { sx <= sy }
		7 { x <= y }
		8 { sx >= sy }
		else { x >= y }
	})
}

fn float_value(value u64, wide bool) f64 {
	return if wide { bits.f64_from_bits(value) } else { f64(bits.f32_from_bits(u32(value))) }
}

fn float_bits(value f64, wide bool) u64 {
	return if wide { bits.f64_bits(value) } else { u64(bits.f32_bits(f32(value))) }
}

fn float_compare(op u16, a u64, b u64, wide bool) u64 {
	x := float_value(a, wide)
	y := float_value(b, wide)
	return u64(match op {
		0 { x == y }
		1 { x != y }
		2 { x < y }
		3 { x > y }
		4 { x <= y }
		else { x >= y }
	})
}

fn float_operation(op u16, a u64, b u64, wide bool) u64 {
	sign := if wide { u64(1) << 63 } else { u64(1) << 31 }
	if op == 0 { return a & ~sign }
	if op == 1 { return a ^ sign }
	if op == 13 { return (a & ~sign) | (b & sign) }
	x := float_value(a, wide)
	y := float_value(b, wide)
	if op in [u16(11), 12] {
		if math.is_nan(x) || math.is_nan(y) { return float_bits(math.nan(), wide) }
		if x == y {
			return if op == 11 { a | b } else { a & b }
		}
		return if (op == 11 && x < y) || (op == 12 && x > y) { a } else { b }
	}
	if !wide && op >= 6 {
		fx := f32(x)
		fy := f32(y)
		z := match op {
			6 { math.sqrtf(fx) }
			7 { fx + fy }
			8 { fx - fy }
			9 { fx * fy }
			else { fx / fy }
		}
		return u64(bits.f32_bits(z))
	}
	z := match op {
		2 { math.ceil(x) }
		3 { math.floor(x) }
		4 { math.trunc(x) }
		5 { math.round_to_even(x) }
		6 { math.sqrt(x) }
		7 { x + y }
		8 { x - y }
		9 { x * y }
		else { x / y }
	}
	return float_bits(z, wide)
}

fn truncate_float(value f64, wide bool, signed bool, saturate bool) !u64 {
	if math.is_nan(value) {
		if saturate { return 0 }
		return error('invalid conversion to integer')
	}
	x := math.trunc(value)
	upper := if wide {
		if signed { 9223372036854775808.0 } else { 18446744073709551616.0 }
	} else {
		if signed { 2147483648.0 } else { 4294967296.0 }
	}
	lower := if signed { -upper } else { 0.0 }
	if x < lower || x >= upper {
		if !saturate { return error('integer overflow') }
		if x < lower {
			return if !signed {
				u64(0)
			} else if wide {
				u64(1) << 63
			} else {
				u64(1) << 31
			}
		}
		return if wide {
			if signed { (u64(1) << 63) - 1 } else { ~u64(0) }
		} else {
			if signed { u64(0x7fffffff) } else { u64(0xffffffff) }
		}
	}
	r := if signed { u64(i64(x)) } else { u64(x) }
	return if wide { r } else { u64(u32(r)) }
}

fn numeric(op u16, a u64, b u64) !u64 {
	if op == 0x45 { return u64(u32(a) == 0) }
	if op == 0x50 { return u64(a == 0) }
	if op >= 0x46 && op <= 0x4f { return integer_compare(op - 0x46, a, b, false) }
	if op >= 0x51 && op <= 0x5a { return integer_compare(op - 0x51, a, b, true) }
	if op >= 0x5b && op <= 0x60 { return float_compare(op - 0x5b, a, b, false) }
	if op >= 0x61 && op <= 0x66 { return float_compare(op - 0x61, a, b, true) }
	if op >= 0x6a && op <= 0x78 { return integer_binary(op - 0x6a, a, b, false) }
	if op >= 0x7c && op <= 0x8a { return integer_binary(op - 0x7c, a, b, true) }
	if op >= 0x8b && op <= 0x98 { return float_operation(op - 0x8b, a, b, false) }
	if op >= 0x99 && op <= 0xa6 { return float_operation(op - 0x99, a, b, true) }
	if (op >= 0xa8 && op <= 0xab) || (op >= 0xae && op <= 0xb1) || (op >= 0x100 && op <= 0x107) {
		i := if op >= 0x100 {
			op - 0x100
		} else if op >= 0xae {
			op - 0xae + 4
		} else {
			op - 0xa8
		}
		return truncate_float(float_value(a, i & 2 != 0), i >= 4, i & 1 == 0, op >= 0x100)
	}
	return match op {
		0x67 { u64(bits.leading_zeros_32(u32(a))) }
		0x68 { u64(bits.trailing_zeros_32(u32(a))) }
		0x69 { u64(bits.ones_count_32(u32(a))) }
		0x79 { u64(bits.leading_zeros_64(a)) }
		0x7a { u64(bits.trailing_zeros_64(a)) }
		0x7b { u64(bits.ones_count_64(a)) }
		0xa7, 0xad { u64(u32(a)) }
		0xac { u64(i64(i32(u32(a)))) }
		0xb2 { u64(bits.f32_bits(f32(i32(u32(a))))) }
		0xb3 { u64(bits.f32_bits(f32(u32(a)))) }
		0xb4 { u64(bits.f32_bits(f32(i64(a)))) }
		0xb5 { u64(bits.f32_bits(f32(a))) }
		0xb6 { float_bits(bits.f64_from_bits(a), false) }
		0xb7 { bits.f64_bits(f64(i32(u32(a)))) }
		0xb8 { bits.f64_bits(f64(u32(a))) }
		0xb9 { bits.f64_bits(f64(i64(a))) }
		0xba { bits.f64_bits(f64(a)) }
		0xbb { bits.f64_bits(f64(bits.f32_from_bits(u32(a)))) }
		0xbc, 0xbd, 0xbe, 0xbf { a }
		0xc0 { u64(u32(int(i8(a)))) }
		0xc1 { u64(u32(int(i16(a)))) }
		0xc2 { u64(i64(i8(a))) }
		0xc3 { u64(i64(i16(a))) }
		0xc4 { u64(i64(i32(u32(a)))) }
		else { return error('unsupported numeric opcode') }
	}
}
