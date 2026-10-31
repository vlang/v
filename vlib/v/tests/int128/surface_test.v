// Statements and library surface for the 128-bit types. Each case either failed to
// compile or gave a wrong answer before, on the portable representation in
// particular, where a 128-bit value is a struct and C has no operator for it.
const wide_const = u128(1) << 64

fn test_increment_and_decrement_a_wide_local() {
	mut a := u128(10)
	a++
	a--
	a += u128(5)
	assert a == u128(15)
	mut b := i128(-10)
	b++
	b--
	assert b == i128(-10)
}

fn test_increment_a_wide_field() {
	mut s := Counter{
		value: u128(3)
	}
	s.value++
	s.value--
	s.value += u128(4)
	assert s.value == u128(7)
}

fn test_a_range_loop_over_wide_bounds() {
	mut sum := u128(0)
	for i in u128(0) .. u128(3) {
		sum += i
	}
	assert sum == u128(3)
}

fn test_a_compound_assignment_to_an_element() {
	mut arr := [u128(1), u128(2)]
	arr[0] += u128(5)
	arr[1] *= u128(3)
	assert arr[0] == u128(6)
	assert arr[1] == u128(6)
	mut wide := [i128(-1)]
	wide[0] -= i128(4)
	assert wide[0] == i128(-5)
}

fn test_a_wide_constant_keeps_its_value() {
	assert wide_const == u128(1) << 64
	assert wide_const.hex() == '10000000000000000'
}

fn test_hex_and_binary() {
	x := (u128(0xdeadbeef) << 64) + u128(0x1234)
	assert x.hex() == 'deadbeef0000000000001234'
	assert u128(255).hex() == 'ff'
	assert u128(255).hex_full() == '000000000000000000000000000000ff'
	assert u128(0).hex() == '0'
	assert u128(5).bin() == '101'
	assert u128(0).bin() == '0'
	assert i128(-1).hex() == 'ffffffffffffffffffffffffffffffff'
}

fn test_text_to_a_wide_value() {
	assert '12345'.u128() == u128(12345)
	assert '+12345'.u128() == u128(12345)
	assert '-12345'.i128() == i128(-12345)
	assert '123456789012345678901234567890'.u128() == u128(123456789012345678901234567890)
	assert 'not a number'.u128() == u128(0)
	assert '12x45'.i128() == i128(0)
	assert ''.i128() == i128(0)
}

struct Counter {
mut:
	value u128
}
