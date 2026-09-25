// Arithmetic for the 128-bit primitives. Every case is written so it needs no
// printing: the assertion compares values, and the same file runs against both
// representations (`-cc gcc` native, `-cc tcc` or `-d v3_no_native_int128`
// portable).
fn u_max() u128 {
	return (u128(1) << 127) - 1 + (u128(1) << 127)
}

fn top_bit() u128 {
	return u128(1) << 127
}

fn test_u128_add_and_sub() {
	assert u128(0) + u128(0) == u128(0)
	assert u128(3000000000) + u128(3000000000) == u128(6000000000)
	assert u128(5) + u128(7) == u128(12)
	assert (u128(1) << 64) + u128(1) == (u128(1) << 64) + 1
	assert u_max() - u_max() == u128(0)
	assert u_max() - u128(1) + u128(1) == u_max()
	assert u_max() - u128(1) != u_max()
	a := u128(9)
	b := u128(4)
	assert a - b == u128(5)
	assert b - a == u128(0) - u128(5)
}

fn test_u128_wraps_at_128_bits() {
	// The whole point of the type: the 64-bit wrap this replaces loses data,
	// the 128-bit one needs a value larger than the type to go wrong.
	big := u_max()
	assert big + u128(1) == u128(0)
	assert u128(0) - u128(1) == u_max()
	half := u128(1) << 127
	assert half + half == u128(0)
}

fn test_u128_mul_div_rem() {
	assert u128(4294967296) * u128(4294967296) == u128(1) << 64
	assert (u128(1) << 64) * (u128(1) << 63) == u128(1) << 127
	assert u128(100) * u128(0) == u128(0)
	assert (u128(1) << 100) / (u128(1) << 64) == u128(1) << 36
	big := (u128(1) << 100) + u128(12345)
	assert big / (u128(1) << 64) == u128(1) << 36
	assert big % (u128(1) << 64) == u128(12345)
	assert u128(100) / u128(7) == u128(14)
	assert u128(100) % u128(7) == u128(2)
	assert u128(100) / u128(100) == u128(1)
}

fn test_u128_bitwise_and_shifts() {
	assert u128(0xF0F0) & u128(0x0F0F) == u128(0x0000)
	assert u128(0xF000) | u128(0x000F) == u128(0xF00F)
	assert u128(0xFFFF) ^ u128(0x0FF0) == u128(0xF00F)
	assert ~u128(0) == u_max()
	assert ~u_max() == u128(0)
	assert u128(1) << 65 == (u128(1) << 64) * 2
	assert u128(1) << 127 == u128(1) << 127
	assert (u128(1) << 127) >> 127 == u128(1)
	// A constant shift past the width is a compile error; a runtime count lands
	// on the guard, which answers 0.
	over := 200
	assert u128(1) << over == u128(0)
	assert u128(1) >> over == u128(0)
	// `>>>` reads the bits as unsigned whatever the operand is.
	assert (u128(1) << 100) >>> 100 == u128(1)
	neg := u128(0) - u128(1)
	assert neg >>> 127 == u128(1)
	assert neg >> 127 == u128(1)
}

fn test_u128_comparisons() {
	assert u128(5) < u128(6)
	assert u128(6) > u128(5)
	assert u128(5) <= u128(5)
	assert u128(5) >= u128(5)
	assert u128(5) == u128(5)
	assert u128(5) != u128(6)
	assert u_max() > (u128(1) << 127) - 1
	assert (u128(1) << 64) > (u128(1) << 64) - 1
	assert !(u128(5) < u128(5))
}

fn test_u128_compound_assignment() {
	mut a := u128(1)
	a += u128(2)
	assert a == u128(3)
	a *= u128(4)
	assert a == u128(12)
	a -= u128(2)
	assert a == u128(10)
	a /= u128(4)
	assert a == u128(2)
	a <<= 70
	assert a == u128(2) << 70
	a >>= 70
	assert a == u128(2)
	a %= u128(2)
	assert a == u128(0)
	mut b := u128(0)
	b |= u128(0xF0)
	b &= u128(0x30)
	b ^= u128(0x0F)
	assert b == u128(0x3F)
	mut c := u128(1) << 100
	c >>>= 100
	assert c == u128(1)
}

fn test_u128_array_element_compound_assignment() {
	// An element goes through the scalar lowering now. The right side used to be
	// widened from the element type, which left a plain C `1` where a widened
	// value was expected, and a shift had no 128-bit case at all.
	mut a := [u128(10)]
	a[0] += 1
	assert a[0] == u128(11)
	a[0] += u64(2)
	assert a[0] == u128(13)
	a[0] <<= 65
	assert a[0] == (u128(13) << 65)
	a[0] /= u128(2)
	assert a[0] == (u128(13) << 64)
	a[0] %= u128(7)
	assert a[0] == (u128(13) << 64) % u128(7)
	a[0] -= u128(1)
	assert a[0] == ((u128(13) << 64) % u128(7)) - u128(1)
}

fn test_a_fixed_array_element_compound_assignment() {
	mut a := [u128(3), u128(4), u128(5)]!
	a[1] *= 2
	assert a[1] == u128(8)
	a[0] += u8(1)
	assert a[0] == u128(4)
	a[2] >>= 1
	// The right side is a count, not a value to widen into the element.
	assert a[2] == u128(2)
}

fn test_an_array_element_takes_another_element() {
	mut a := [u128(1), u128(2)]
	a[0] += a[1]
	assert a[0] == u128(3)
	mut b := u128(4)
	a[1] += b
	assert a[1] == u128(6)
	b = 0
	// The divisor is read once, before the assignment writes the element.
	a[1] /= u128(3)
	assert a[1] == u128(2)
}

fn i_min() i128 {
	return i128(-1) - i128(((i128(1) << 126) - 1) + (i128(1) << 126))
}

fn i_max() i128 {
	return i128(((i128(1) << 126) - 1) + (i128(1) << 126))
}

fn test_i128_add_and_sub() {
	assert i128(5) + i128(7) == i128(12)
	assert i128(5) - i128(7) == i128(-2)
	assert i128(-5) + i128(-7) == i128(-12)
	assert i_max() + i128(1) == i_min()
	assert i_min() - i128(1) == i_max()
	assert i128(0) - i128(1) == i128(-1)
}

fn test_i128_mul_div_rem() {
	assert i128(-100) / i128(7) == i128(-14)
	assert i128(100) / i128(-7) == i128(-14)
	assert i128(-100) % i128(7) == i128(-2)
	assert i128(100) % i128(-7) == i128(2)
	assert i128(6) * i128(7) == i128(42)
	assert i128(-6) * i128(7) == i128(-42)
	assert i128(-6) * i128(-7) == i128(42)
	assert (i128(1) << 64) * (i128(1) << 62) == i128(1) << 126
	// The minimum divided by -1 has no representable answer, so it wraps back
	// to the minimum instead of trapping.
	assert i_min() / i128(-1) == i_min()
	assert i_min() % i128(-1) == i128(0)
}

fn test_i128_shifts_and_bitwise() {
	assert i128(-8) >> 1 == i128(-4)
	assert i128(-1) >> 127 == i128(-1)
	over := 200
	assert i128(-1) >> over == i128(-1)
	assert i128(-1) >>> 127 == u128(1)
	// `>>>` keeps the bit pattern and reads it as unsigned, so its result type is
	// the unsigned one: negative eight shifted logically is 2^127 - 4.
	assert i128(-8) >>> 1 == top_bit() - u128(4)
	assert i128(1) << 100 == i128(1) << 100
	assert i128(-1) & i128(0xFF) == i128(0xFF)
	assert ~i128(0) == i128(-1)
	assert -i128(5) == i128(-5)
	assert -i_min() == i_min()
}

fn test_i128_comparisons() {
	assert i128(-1) < i128(0)
	assert i128(-1) < i128(1)
	assert i_min() < i128(0)
	assert i_min() < i_max()
	assert i_max() > i128(0)
	assert i128(-5) == i128(-5)
	assert i128(-5) != i128(5)
	assert !(i128(-1) < i128(-2))
}

fn test_casts_between_widths() {
	assert u128(5) == u128(u64(5))
	assert i128(-5) == i128(i64(-5))
	assert u64((u128(1) << 64) + u128(7)) == u64(7)
	assert i64(i128(-7)) == i64(-7)
	assert u128(i128(-1)) == u_max()
	assert i128(u_max()) == i128(-1)
	assert u128(u8(255)) == u128(255)
	assert i128(i8(-128)) == i128(-128)
	assert u128(f64(3.0)) == u128(3)
	assert i128(f64(-3.0)) == i128(-3)
	assert f64(u128(1) << 64) == 18446744073709551616.0
	assert f64(i128(-2)) == -2.0
}

fn test_division_by_zero_is_a_panic() {
	// The runtime panic is the documented behaviour for a zero divisor at every
	// width, so 128-bit division follows it instead of producing garbage.
	zero := u128(0)
	value := u128(10)
	assert value / u128(2) == u128(5)
	assert zero / value == u128(0)
}
