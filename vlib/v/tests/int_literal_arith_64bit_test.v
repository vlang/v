const big = 100000 * 100000
const neg_big = -100000 * 100000
const sum_big = 2147483647 + 2147483647
const nested_big = (100000 * 100000) * 10 - 1

// An untyped integer literal expression has the type `int`, which is 64 bits wide on a
// 64-bit target, so arithmetic on two literals below 2^31 must not wrap at 32 bits.
fn test_const_arithmetic_on_literals_does_not_wrap_at_32_bits() {
	$if x64 {
		assert big == 10_000_000_000
		assert neg_big == -10_000_000_000
		assert sum_big == 4_294_967_294
		assert nested_big == 99_999_999_999
	}
}

fn test_arithmetic_on_literals_in_expressions_does_not_wrap_at_32_bits() {
	$if x64 {
		assert 1_000_000 * 1_000_000 == 1_000_000_000_000
		assert 3000000000 * 2 == 6_000_000_000
		assert 2147483647 + 1 == 2_147_483_648
		assert -2147483647 - 10 == -2_147_483_657
		assert (-2147483647 - 10) * 3 == -6_442_450_971
		assert 46341 * 46341 == 2_147_488_281
		x := 100000 * 100000
		assert x == 10_000_000_000
		assert typeof(x).name == 'int'
		println(100000 * 100000)
	}
}

fn test_literal_shifts_use_the_64_bit_int() {
	$if x64 {
		b := 1
		assert b << 40 == 1_099_511_627_776
		assert 1 << 40 == 1_099_511_627_776
	}
}

// An `i32` operand keeps the arithmetic at 32 bits.
fn test_i32_arithmetic_stays_32_bit() {
	a := i32(100000)
	assert a * 100000 == i32(1410065408)
	assert i32(100000 * 100000) == i32(1410065408)
	c := i32(1)
	assert c << 20 == i32(1048576)
}
