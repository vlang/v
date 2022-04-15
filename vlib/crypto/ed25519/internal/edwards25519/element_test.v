module edwards25519

import os
import rand
import math.bits
import math.big
import encoding.hex

const github_job = os.getenv('GITHUB_JOB')

fn testsuite_begin() {
	if edwards25519.github_job != '' {
		// ensure that the CI does not run flaky tests:
		rand.seed([u32(0xffff24), 0xabcd])
	}
}

fn (mut v Element) str() string {
	return hex.encode(v.bytes())
}

const mask_low_52_bits = (u64(1) << 52) - 1

fn generate_field_element() Element {
	return Element{
		l0: rand.u64() & edwards25519.mask_low_52_bits
		l1: rand.u64() & edwards25519.mask_low_52_bits
		l2: rand.u64() & edwards25519.mask_low_52_bits
		l3: rand.u64() & edwards25519.mask_low_52_bits
		l4: rand.u64() & edwards25519.mask_low_52_bits
	}
}

// weirdLimbs can be combined to generate a range of edge-case edwards25519 elements.
// 0 and -1 are intentionally more weighted, as they combine well.
const (
	two_to_51      = u64(1) << 51
	two_to_52      = u64(1) << 52
	weird_limbs_51 = [
		u64(0),
		0,
		0,
		0,
		1,
		19 - 1,
		19,
		0x2aaaaaaaaaaaa,
		0x5555555555555,
		two_to_51 - 20,
		two_to_51 - 19,
		two_to_51 - 1,
		two_to_51 - 1,
		two_to_51 - 1,
		two_to_51 - 1,
	]
	weird_limbs_52 = [
		u64(0),
		0,
		0,
		0,
		0,
		0,
		1,
		19 - 1,
		19,
		0x2aaaaaaaaaaaa,
		0x5555555555555,
		two_to_51 - 20,
		two_to_51 - 19,
		two_to_51 - 1,
		two_to_51 - 1,
		two_to_51 - 1,
		two_to_51 - 1,
		two_to_51 - 1,
		two_to_51 - 1,
		two_to_51,
		two_to_51 + 1,
		two_to_52 - 19,
		two_to_52 - 1,
	]
)

fn generate_weird_field_element() Element {
	return Element{
		l0: edwards25519.weird_limbs_52[rand.intn(edwards25519.weird_limbs_52.len) or { 0 }]
		l1: edwards25519.weird_limbs_51[rand.intn(edwards25519.weird_limbs_51.len) or { 0 }]
		l2: edwards25519.weird_limbs_51[rand.intn(edwards25519.weird_limbs_51.len) or { 0 }]
		l3: edwards25519.weird_limbs_51[rand.intn(edwards25519.weird_limbs_51.len) or { 0 }]
		l4: edwards25519.weird_limbs_51[rand.intn(edwards25519.weird_limbs_51.len) or { 0 }]
	}
}

fn (e Element) generate_element() Element {
	if rand.intn(2) or { 0 } == 0 {
		return generate_weird_field_element()
	}
	return generate_field_element()
}

fn is_in_bounds(x Element) bool {
	return bits.len_64(x.l0) <= 52 && bits.len_64(x.l1) <= 52 && bits.len_64(x.l2) <= 52
		&& bits.len_64(x.l3) <= 52 && bits.len_64(x.l4) <= 52
}

fn carry_gen(a [5]u64) bool {
	mut t1 := Element{a[0], a[1], a[2], a[3], a[4]}
	mut t2 := Element{a[0], a[1], a[2], a[3], a[4]}

	t1.carry_propagate_generic()
	t2.carry_propagate_generic()

	return t1 == t2 && is_in_bounds(t2)
}

fn test_carry_propagate_generic() {
	// closures not supported on windows
	for i := 0; i <= 10; i++ {
		els := [rand.u64(), rand.u64(), rand.u64(), rand.u64(),
			rand.u64()]!
		p := carry_gen(els)
		assert p == true
	}
	res := carry_gen([u64(0xffffffffffffffff), 0xffffffffffffffff, 0xffffffffffffffff,
		0xffffffffffffffff, 0xffffffffffffffff]!)
	assert res == true
}

fn test_fe_mul_generic() {
	for i in 0 .. 20 {
		el := Element{}
		a := el.generate_element()
		b := el.generate_element()
		a1 := a
		a2 := a

		b1 := b
		b2 := b

		a1b1 := fe_mul_generic(a1, b1)
		a2b2 := fe_mul_generic(a2, b2)
		assert a1b1 == a2b2 && is_in_bounds(a1b1) && is_in_bounds(a2b2)
	}
}

fn test_fe_square_generic() {
	for i in 0 .. 20 {
		a := generate_field_element()

		a1 := a
		a2 := a

		a11 := fe_square_generic(a1)
		a22 := fe_square_generic(a2)
		assert a11 == a22 && is_in_bounds(a11) && is_in_bounds(a22)
	}
}

struct SqrtRatioTest {
	u          string
	v          string
	was_square int
	r          string
}

fn test_sqrt_ratio() ? {
	// From draft-irtf-cfrg-ristretto255-decaf448-00, Appendix A.4.

	tests := [
		// If u is 0, the function is defined to return (0, TRUE), even if v
		// is zero. Note that where used in this package, the denominator v
		// is never zero.
		SqrtRatioTest{'0000000000000000000000000000000000000000000000000000000000000000', '0000000000000000000000000000000000000000000000000000000000000000', 1, '0000000000000000000000000000000000000000000000000000000000000000'},
		// 0/1 == 0²
		SqrtRatioTest{'0000000000000000000000000000000000000000000000000000000000000000', '0100000000000000000000000000000000000000000000000000000000000000', 1, '0000000000000000000000000000000000000000000000000000000000000000'},
		// If u is non-zero and v is zero, defined to return (0, FALSE).
		SqrtRatioTest{'0100000000000000000000000000000000000000000000000000000000000000', '0000000000000000000000000000000000000000000000000000000000000000', 0, '0000000000000000000000000000000000000000000000000000000000000000'},
		// 2/1 is not square in this edwards25519.
		SqrtRatioTest{'0200000000000000000000000000000000000000000000000000000000000000', '0100000000000000000000000000000000000000000000000000000000000000', 0, '3c5ff1b5d8e4113b871bd052f9e7bcd0582804c266ffb2d4f4203eb07fdb7c54'},
		// 4/1 == 2²
		SqrtRatioTest{'0400000000000000000000000000000000000000000000000000000000000000', '0100000000000000000000000000000000000000000000000000000000000000', 1, '0200000000000000000000000000000000000000000000000000000000000000'},
		// 1/4 == (2⁻¹)² == (2^(p-2))² per Euler's theorem
		SqrtRatioTest{'0100000000000000000000000000000000000000000000000000000000000000', '0400000000000000000000000000000000000000000000000000000000000000', 1, 'f6ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff3f'},
	]

	for i, tt in tests {
		mut elu := Element{}
		mut elv := Element{}
		mut elw := Element{}
		mut elg := Element{}

		u := elu.set_bytes(hex.decode(tt.u) ?) ?
		v := elv.set_bytes(hex.decode(tt.v) ?) ?
		want := elw.set_bytes(hex.decode(tt.r) ?) ?
		mut got, was_square := elg.sqrt_ratio(u, v)

		assert got.equal(want) != 0
		assert was_square == tt.was_square
		// if got.Equal(want) == 0 || wasSquare != tt.wasSquare {
		// 	t.Errorf("%d: got (%v, %v), want (%v, %v)", i, got, wasSquare, want, tt.wasSquare)
		// }
	}
}

fn test_set_bytes_normal() ? {
	for i in 0 .. 15 {
		mut el := Element{}
		mut random_inp := rand.bytes(32) ?

		el = el.set_bytes(random_inp.clone()) ?
		random_inp[random_inp.len - 1] &= (1 << 7) - 1
		// assert f1(random_inp, el) == true

		assert random_inp == el.bytes()
		assert is_in_bounds(el) == true
	}
}

fn test_set_bytes_reduced() {
	mut fe := Element{}
	mut r := Element{}
	mut random_inp := rand.bytes(32) or { return }

	fe.set_bytes(random_inp) or { return }
	r.set_bytes(fe.bytes()) or { return }

	assert fe == r
}

// Check some fixed vectors from dalek
struct FeRTTest {
mut:
	fe Element
	b  []u8
}

fn test_set_bytes_from_dalek_test_vectors() ? {
	mut tests := [
		FeRTTest{
			fe: Element{358744748052810, 1691584618240980, 977650209285361, 1429865912637724, 560044844278676}
			b: [u8(74), 209, 69, 197, 70, 70, 161, 222, 56, 226, 229, 19, 112, 60, 25, 92, 187,
				74, 222, 56, 50, 153, 51, 233, 40, 74, 57, 6, 160, 185, 213, 31]
		},
		FeRTTest{
			fe: Element{84926274344903, 473620666599931, 365590438845504, 1028470286882429, 2146499180330972}
			b: [u8(199), 23, 106, 112, 61, 77, 216, 79, 186, 60, 11, 118, 13, 16, 103, 15, 42,
				32, 83, 250, 44, 57, 204, 198, 78, 199, 253, 119, 146, 172, 3, 122]
		},
	]
	for _, mut tt in tests {
		b := tt.fe.bytes()
		mut el := Element{}
		mut fe := el.set_bytes(tt.b) ?

		assert b == tt.b
		assert fe.equal(tt.fe) == 1
	}
}

fn test_equal() {
	mut x := Element{1, 1, 1, 1, 1}
	y := Element{5, 4, 3, 2, 1}

	mut eq1 := x.equal(x)
	assert eq1 == 1

	eq1 = x.equal(y)
	assert eq1 == 0
}

fn test_invert() ? {
	mut x := Element{1, 1, 1, 1, 1}
	mut one := Element{1, 0, 0, 0, 0}
	mut xinv := Element{}
	mut r := Element{}

	xinv.invert(x)
	r.multiply(x, xinv)
	r.reduce()

	assert one == r
	bytes := rand.bytes(32) or { return err }

	x.set_bytes(bytes) ?

	xinv.invert(x)
	r.multiply(x, xinv)
	r.reduce()

	assert one == r

	zero := Element{}
	x.set(zero)

	xx := xinv.invert(x)
	assert xx == xinv
	assert xinv.equal(zero) == 1
	// s := if num % 2 == 0 { 'even' } else { 'odd' }
}

fn test_mult_32() {
	for j in 0 .. 10 {
		mut x := Element{}
		mut t1 := Element{}
		y := u32(0)
		for i := 0; i < 100; i++ {
			t1.mult_32(x, y)
		}
		mut ty := Element{}
		ty.l0 = u64(y)
		mut t2 := Element{}
		for i := 0; i < 100; i++ {
			t2.multiply(x, ty)
		}
		assert t1.equal(t2) == 1 && is_in_bounds(t1) && is_in_bounds(t2)
	}
}

fn test_selected_and_swap() {
	a := Element{358744748052810, 1691584618240980, 977650209285361, 1429865912637724, 560044844278676}
	b := Element{84926274344903, 473620666599931, 365590438845504, 1028470286882429, 2146499180330972}

	mut c := Element{}
	mut d := Element{}

	c.selected(a, b, 1)
	d.selected(a, b, 0)

	assert c.equal(a) == 1
	assert d.equal(b) == 1

	c.swap(mut d, 0)
	assert c.equal(a) == 1
	assert d.equal(b) == 1

	c.swap(mut d, 1)
	assert c.equal(b) == 1
	assert d.equal(a) == 1
}

// Tests self-consistency between multiply and Square.
fn test_consistency_between_mult_and_square() {
	mut x := Element{1, 1, 1, 1, 1}
	mut x2 := Element{}
	mut x2sq := Element{}

	x2.multiply(x, x)
	x2sq.square(x)

	assert x2 == x2sq

	bytes := rand.bytes(32) or { return }
	x.set_bytes(bytes) or { return }
	x2.multiply(x, x)
	x2sq.square(x)

	assert x2 == x2sq
}

// to_big_integer returns v as a big.Integer.
fn (mut v Element) to_big_integer() big.Integer {
	buf := v.bytes()
	return big.integer_from_bytes(buf)
}

// from_big_integer sets v = n, and returns v. The bit length of n must not exceed 256.
fn (mut v Element) from_big_integer(n big.Integer) ?Element {
	if n.binary_str().len > 32 * 8 {
		return error('invalid edwards25519 element input size')
	}
	mut bytes, _ := n.bytes()
	swap_endianness(mut bytes) // SHOULD I SWAP IT?
	v.set_bytes(bytes) ?

	return v
}

fn (mut v Element) from_decimal_string(s string) ?Element {
	num := big.integer_from_string(s) ?

	v = v.from_big_integer(num) ?
	return v
}

fn test_bytes_big_equivalence() ? {
	mut inp := rand.bytes(32) ?
	el := Element{}
	mut fe := el.generate_element()
	mut fe1 := el.generate_element()

	fe.set_bytes(inp) or { panic(err) }
	inp[inp.len - 1] &= (1 << 7) - 1 // mask the most significant bit

	mut b := big.integer_from_bytes(swap_endianness(mut inp)) // need swap_endianness
	fe1.from_big_integer(b) or { panic(err) } // do swap_endianness internally

	assert fe == fe1

	mut buf := []u8{len: 32} // pad with zeroes
	fedtobig := fe1.to_big_integer()
	mut fedbig_bytes, _ := fedtobig.bytes()
	copy(mut buf, fedbig_bytes) // does not need to do swap_endianness

	assert fe.bytes() == buf && is_in_bounds(fe) && is_in_bounds(fe1)
	// assert big_equivalence(inp, fe, fe1) == true
}

fn test_decimal_constants() ? {
	sqrtm1string := '19681161376707505956807079304988542015446066515923890162744021073123829784752'
	mut el := Element{}
	mut exp := el.from_decimal_string(sqrtm1string) ?

	assert sqrt_m1.equal(exp) == 1

	dstring := '37095705934669439343138083508754565189542113879843219016388785533085940283555'
	exp = el.from_decimal_string(dstring) ?
	mut d := d_const

	assert d.equal(exp) == 1
}

fn test_mul_64_to_128() {
	mut a := u64(5)
	mut b := u64(5)
	mut r := mul_64(a, b)

	assert r.lo == 0x19
	assert r.hi == 0

	a = u64(18014398509481983) // 2^54 - 1
	b = u64(18014398509481983) // 2^54 - 1
	r = mul_64(a, b)

	assert r.lo == 0xff80000000000001 && r.hi == 0xfffffffffff

	a = u64(1125899906842661)
	b = u64(2097155)
	r = mul_64(a, b)
	r = add_mul_64(r, a, b)
	r = add_mul_64(r, a, b)
	r = add_mul_64(r, a, b)
	r = add_mul_64(r, a, b)

	assert r.lo == 16888498990613035 && r.hi == 640
}

fn test_multiply_distributes_over_add() {
	for i in 0 .. 10 {
		el := Element{}
		x := el.generate_element()
		y := el.generate_element()
		z := el.generate_element()
		mut t1 := Element{}
		t1.add(x, y)
		t1.multiply(t1, z)

		// Compute t2 = x*z + y*z
		mut t2 := Element{}
		mut t3 := Element{}
		t2.multiply(x, z)
		t3.multiply(y, z)
		t2.add(t2, t3)
		assert t1.equal(t2) == 1 && is_in_bounds(t1) && is_in_bounds(t2)
	}
}
