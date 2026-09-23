// 128-bit values inside structs, arrays and function signatures. Layout is part
// of the contract, so `sizeof` and alignment are checked here rather than
// assumed from the C representation.
struct Pair {
mut:
	lo u128
	hi i128
}

struct Fixed128 {
	a u128
	b u64
	c u128
}

fn take_by_value(p Pair) u128 {
	return p.lo
}

fn take_by_ref(p &Pair) u128 {
	return p.lo
}

fn swap_values(a &u128, b &u128) {
	tmp := *a
	*a = *b
	*b = tmp
}

fn test_sizeof_is_sixteen_bytes() {
	assert sizeof(u128) == 16
	assert sizeof(i128) == 16
	assert sizeof(u64) == 8
	// The u128 field is 16-byte aligned on both representations, so the u64
	// after it starts at 16 and the struct rounds up past the second u128.
	assert sizeof(Fixed128) == 32
	assert sizeof(Pair) == 32
}

fn test_struct_fields_hold_wide_values() {
	mut p := Pair{
		lo: u128(1) << 100
		hi: i128(-5)
	}
	assert p.lo == u128(1) << 100
	assert p.hi == i128(-5)
	p.lo += u128(1)
	assert p.lo == (u128(1) << 100) + u128(1)
	p.hi *= i128(3)
	assert p.hi == i128(-15)
}

fn test_struct_defaults_are_zero() {
	p := Pair{}
	assert p.lo == u128(0)
	assert p.hi == i128(0)
	assert p.lo == u128(0) - u128(0)
}

fn test_passing_by_value_and_by_reference() {
	p := Pair{
		lo: u128(7) << 90
		hi: i128(-7)
	}
	assert take_by_value(p) == u128(7) << 90
	assert take_by_ref(p) == u128(7) << 90
}

fn test_mutating_through_a_pointer() {
	mut a := u128(1) << 64
	mut b := u128(2) << 64
	swap_values(&a, &b)
	assert a == u128(2) << 64
	assert b == u128(1) << 64
}

fn test_dynamic_arrays() {
	mut values := []u128{len: 3}
	assert values.len == 3
	assert values[0] == u128(0)
	values[0] = u128(1) << 64
	values[1] = u128(2) << 64
	values[2] = u128(3) << 64
	assert values[0] + values[1] == values[2]
	mut total := u128(0)
	for value in values {
		total += value
	}
	assert total == u128(6) << 64
	values << u128(4) << 64
	assert values.len == 4
	assert values[3] == u128(4) << 64
}

fn test_fixed_arrays_and_nested_structs() {
	fixed := [u128(1), u128(2), u128(3)]
	assert fixed.len == 3
	assert fixed[0] + fixed[1] == fixed[2]
	nested := [[u128(1), u128(2)], [u128(3), u128(4)]]
	assert nested[0][0] + nested[1][1] == u128(5)
	holder := [Pair{
		lo: u128(9)
		hi: i128(-9)
	}]
	assert holder[0].lo == u128(9)
	assert holder[0].hi == i128(-9)
}

fn test_i128_field_arithmetic_stays_signed() {
	mut p := Pair{
		hi: i128(-1) << 64
	}
	assert p.hi < i128(0)
	p.hi -= i128(1)
	assert p.hi == (i128(-1) << 64) - i128(1)
	assert p.lo == u128(0)
}
