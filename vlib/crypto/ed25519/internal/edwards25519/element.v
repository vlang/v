module edwards25519

import math.bits
import math.unsigned
import encoding.binary
import crypto.internal.subtle

// embedded unsigned.Uint128
struct Uint128 {
	unsigned.Uint128
}

// Element represents an element of the edwards25519 GF(2^255-19). Note that this
// is not a cryptographically secure group, and should only be used to interact
// with edwards25519.Point coordinates.
//
// This type works similarly to math/big.Int, and all arguments and receivers
// are allowed to alias.
//
// The zero value is a valid zero element.
pub struct Element {
mut:
	// An element t represents the integer
	//     t.l0 + t.l1*2^51 + t.l2*2^102 + t.l3*2^153 + t.l4*2^204
	//
	// Between operations, all limbs are expected to be lower than 2^52.
	l0 u64
	l1 u64
	l2 u64
	l3 u64
	l4 u64
}

const (
	mask_low_51_bits = u64((1 << 51) - 1)
	fe_zero          = Element{
		l0: 0
		l1: 0
		l2: 0
		l3: 0
		l4: 0
	}
	fe_one = Element{
		l0: 1
		l1: 0
		l2: 0
		l3: 0
		l4: 0
	}
	// sqrt_m1 is 2^((p-1)/4), which squared is equal to -1 by Euler's Criterion.
	sqrt_m1 = Element{
		l0: 1718705420411056
		l1: 234908883556509
		l2: 2233514472574048
		l3: 2117202627021982
		l4: 765476049583133
	}
)

// mul_64 returns a * b.
fn mul_64(a u64, b u64) Uint128 {
	hi, lo := bits.mul_64(a, b)
	return Uint128{
		lo: lo
		hi: hi
	}
}

// add_mul_64 returns v + a * b.
fn add_mul_64(v Uint128, a u64, b u64) Uint128 {
	mut hi, lo := bits.mul_64(a, b)
	low, carry := bits.add_64(lo, v.lo, 0)
	hi, _ = bits.add_64(hi, v.hi, carry)
	return Uint128{
		lo: low
		hi: hi
	}
}

// shift_right_by_51 returns a >> 51. a is assumed to be at most 115 bits.
fn shift_right_by_51(a Uint128) u64 {
	return (a.hi << (64 - 51)) | (a.lo >> 51)
}

fn fe_mul_generic(a Element, b Element) Element {
	a0 := a.l0
	a1 := a.l1
	a2 := a.l2
	a3 := a.l3
	a4 := a.l4

	b0 := b.l0
	b1 := b.l1
	b2 := b.l2
	b3 := b.l3
	b4 := b.l4

	// Limb multiplication works like pen-and-paper columnar multiplication, but
	// with 51-bit limbs instead of digits.
	//
	//                          a4   a3   a2   a1   a0  x
	//                          b4   b3   b2   b1   b0  =
	//                         ------------------------
	//                        a4b0 a3b0 a2b0 a1b0 a0b0  +
	//                   a4b1 a3b1 a2b1 a1b1 a0b1       +
	//              a4b2 a3b2 a2b2 a1b2 a0b2            +
	//         a4b3 a3b3 a2b3 a1b3 a0b3                 +
	//    a4b4 a3b4 a2b4 a1b4 a0b4                      =
	//   ----------------------------------------------
	//      r8   r7   r6   r5   r4   r3   r2   r1   r0
	//
	// We can then use the reduction identity (a * 2²⁵⁵ + b = a * 19 + b) to
	// reduce the limbs that would overflow 255 bits. r5 * 2²⁵⁵ becomes 19 * r5,
	// r6 * 2³⁰⁶ becomes 19 * r6 * 2⁵¹, etc.
	//
	// Reduction can be carried out simultaneously to multiplication. For
	// example, we do not compute r5: whenever the result of a multiplication
	// belongs to r5, like a1b4, we multiply it by 19 and add the result to r0.
	//
	//            a4b0    a3b0    a2b0    a1b0    a0b0  +
	//            a3b1    a2b1    a1b1    a0b1 19×a4b1  +
	//            a2b2    a1b2    a0b2 19×a4b2 19×a3b2  +
	//            a1b3    a0b3 19×a4b3 19×a3b3 19×a2b3  +
	//            a0b4 19×a4b4 19×a3b4 19×a2b4 19×a1b4  =
	//           --------------------------------------
	//              r4      r3      r2      r1      r0
	//
	// Finally we add up the columns into wide, overlapping limbs.

	a1_19 := a1 * 19
	a2_19 := a2 * 19
	a3_19 := a3 * 19
	a4_19 := a4 * 19

	// r0 = a0×b0 + 19×(a1×b4 + a2×b3 + a3×b2 + a4×b1)
	mut r0 := mul_64(a0, b0)
	r0 = add_mul_64(r0, a1_19, b4)
	r0 = add_mul_64(r0, a2_19, b3)
	r0 = add_mul_64(r0, a3_19, b2)
	r0 = add_mul_64(r0, a4_19, b1)

	// r1 = a0×b1 + a1×b0 + 19×(a2×b4 + a3×b3 + a4×b2)
	mut r1 := mul_64(a0, b1)
	r1 = add_mul_64(r1, a1, b0)
	r1 = add_mul_64(r1, a2_19, b4)
	r1 = add_mul_64(r1, a3_19, b3)
	r1 = add_mul_64(r1, a4_19, b2)

	// r2 = a0×b2 + a1×b1 + a2×b0 + 19×(a3×b4 + a4×b3)
	mut r2 := mul_64(a0, b2)
	r2 = add_mul_64(r2, a1, b1)
	r2 = add_mul_64(r2, a2, b0)
	r2 = add_mul_64(r2, a3_19, b4)
	r2 = add_mul_64(r2, a4_19, b3)

	// r3 = a0×b3 + a1×b2 + a2×b1 + a3×b0 + 19×a4×b4
	mut r3 := mul_64(a0, b3)
	r3 = add_mul_64(r3, a1, b2)
	r3 = add_mul_64(r3, a2, b1)
	r3 = add_mul_64(r3, a3, b0)
	r3 = add_mul_64(r3, a4_19, b4)

	// r4 = a0×b4 + a1×b3 + a2×b2 + a3×b1 + a4×b0
	mut r4 := mul_64(a0, b4)
	r4 = add_mul_64(r4, a1, b3)
	r4 = add_mul_64(r4, a2, b2)
	r4 = add_mul_64(r4, a3, b1)
	r4 = add_mul_64(r4, a4, b0)

	// After the multiplication, we need to reduce (carry) the five coefficients
	// to obtain a result with limbs that are at most slightly larger than 2⁵¹,
	// to respect the Element invariant.
	//
	// Overall, the reduction works the same as carryPropagate, except with
	// wider inputs: we take the carry for each coefficient by shifting it right
	// by 51, and add it to the limb above it. The top carry is multiplied by 19
	// according to the reduction identity and added to the lowest limb.
	//
	// The largest coefficient (r0) will be at most 111 bits, which guarantees
	// that all carries are at most 111 - 51 = 60 bits, which fits in a u64.
	//
	//     r0 = a0×b0 + 19×(a1×b4 + a2×b3 + a3×b2 + a4×b1)
	//     r0 < 2⁵²×2⁵² + 19×(2⁵²×2⁵² + 2⁵²×2⁵² + 2⁵²×2⁵² + 2⁵²×2⁵²)
	//     r0 < (1 + 19 × 4) × 2⁵² × 2⁵²
	//     r0 < 2⁷ × 2⁵² × 2⁵²
	//     r0 < 2¹¹¹
	//
	// Moreover, the top coefficient (r4) is at most 107 bits, so c4 is at most
	// 56 bits, and c4 * 19 is at most 61 bits, which again fits in a u64 and
	// allows us to easily apply the reduction identity.
	//
	//     r4 = a0×b4 + a1×b3 + a2×b2 + a3×b1 + a4×b0
	//     r4 < 5 × 2⁵² × 2⁵²
	//     r4 < 2¹⁰⁷
	//

	c0 := shift_right_by_51(r0)
	c1 := shift_right_by_51(r1)
	c2 := shift_right_by_51(r2)
	c3 := shift_right_by_51(r3)
	c4 := shift_right_by_51(r4)

	rr0 := r0.lo & edwards25519.mask_low_51_bits + c4 * 19
	rr1 := r1.lo & edwards25519.mask_low_51_bits + c0
	rr2 := r2.lo & edwards25519.mask_low_51_bits + c1
	rr3 := r3.lo & edwards25519.mask_low_51_bits + c2
	rr4 := r4.lo & edwards25519.mask_low_51_bits + c3

	// Now all coefficients fit into 64-bit registers but are still too large to
	// be passed around as a Element. We therefore do one last carry chain,
	// where the carries will be small enough to fit in the wiggle room above 2⁵¹.
	mut v := Element{
		l0: rr0
		l1: rr1
		l2: rr2
		l3: rr3
		l4: rr4
	}
	// v.carryPropagate()
	// using `carry_propagate_generic()` instead
	v = v.carry_propagate_generic()
	return v
}

// carry_propagate_generic brings the limbs below 52 bits by applying the reduction
// identity (a * 2²⁵⁵ + b = a * 19 + b) to the l4 carry.
fn (mut v Element) carry_propagate_generic() Element {
	c0 := v.l0 >> 51
	c1 := v.l1 >> 51
	c2 := v.l2 >> 51
	c3 := v.l3 >> 51
	c4 := v.l4 >> 51

	v.l0 = v.l0 & edwards25519.mask_low_51_bits + c4 * 19
	v.l1 = v.l1 & edwards25519.mask_low_51_bits + c0
	v.l2 = v.l2 & edwards25519.mask_low_51_bits + c1
	v.l3 = v.l3 & edwards25519.mask_low_51_bits + c2
	v.l4 = v.l4 & edwards25519.mask_low_51_bits + c3
	return v
}

fn fe_square_generic(a Element) Element {
	l0 := a.l0
	l1 := a.l1
	l2 := a.l2
	l3 := a.l3
	l4 := a.l4

	// Squaring works precisely like multiplication above, but thanks to its
	// symmetry we get to group a few terms together.
	//
	//                          l4   l3   l2   l1   l0  x
	//                          l4   l3   l2   l1   l0  =
	//                         ------------------------
	//                        l4l0 l3l0 l2l0 l1l0 l0l0  +
	//                   l4l1 l3l1 l2l1 l1l1 l0l1       +
	//              l4l2 l3l2 l2l2 l1l2 l0l2            +
	//         l4l3 l3l3 l2l3 l1l3 l0l3                 +
	//    l4l4 l3l4 l2l4 l1l4 l0l4                      =
	//   ----------------------------------------------
	//      r8   r7   r6   r5   r4   r3   r2   r1   r0
	//
	//            l4l0    l3l0    l2l0    l1l0    l0l0  +
	//            l3l1    l2l1    l1l1    l0l1 19×l4l1  +
	//            l2l2    l1l2    l0l2 19×l4l2 19×l3l2  +
	//            l1l3    l0l3 19×l4l3 19×l3l3 19×l2l3  +
	//            l0l4 19×l4l4 19×l3l4 19×l2l4 19×l1l4  =
	//           --------------------------------------
	//              r4      r3      r2      r1      r0
	//
	// With precomputed 2×, 19×, and 2×19× terms, we can compute each limb with
	// only three mul_64 and four Add64, instead of five and eight.

	l0_2 := l0 * 2
	l1_2 := l1 * 2

	l1_38 := l1 * 38
	l2_38 := l2 * 38
	l3_38 := l3 * 38

	l3_19 := l3 * 19
	l4_19 := l4 * 19

	// r0 = l0×l0 + 19×(l1×l4 + l2×l3 + l3×l2 + l4×l1) = l0×l0 + 19×2×(l1×l4 + l2×l3)
	mut r0 := mul_64(l0, l0)
	r0 = add_mul_64(r0, l1_38, l4)
	r0 = add_mul_64(r0, l2_38, l3)

	// r1 = l0×l1 + l1×l0 + 19×(l2×l4 + l3×l3 + l4×l2) = 2×l0×l1 + 19×2×l2×l4 + 19×l3×l3
	mut r1 := mul_64(l0_2, l1)
	r1 = add_mul_64(r1, l2_38, l4)
	r1 = add_mul_64(r1, l3_19, l3)

	// r2 = l0×l2 + l1×l1 + l2×l0 + 19×(l3×l4 + l4×l3) = 2×l0×l2 + l1×l1 + 19×2×l3×l4
	mut r2 := mul_64(l0_2, l2)
	r2 = add_mul_64(r2, l1, l1)
	r2 = add_mul_64(r2, l3_38, l4)

	// r3 = l0×l3 + l1×l2 + l2×l1 + l3×l0 + 19×l4×l4 = 2×l0×l3 + 2×l1×l2 + 19×l4×l4
	mut r3 := mul_64(l0_2, l3)
	r3 = add_mul_64(r3, l1_2, l2)
	r3 = add_mul_64(r3, l4_19, l4)

	// r4 = l0×l4 + l1×l3 + l2×l2 + l3×l1 + l4×l0 = 2×l0×l4 + 2×l1×l3 + l2×l2
	mut r4 := mul_64(l0_2, l4)
	r4 = add_mul_64(r4, l1_2, l3)
	r4 = add_mul_64(r4, l2, l2)

	c0 := shift_right_by_51(r0)
	c1 := shift_right_by_51(r1)
	c2 := shift_right_by_51(r2)
	c3 := shift_right_by_51(r3)
	c4 := shift_right_by_51(r4)

	rr0 := r0.lo & edwards25519.mask_low_51_bits + c4 * 19
	rr1 := r1.lo & edwards25519.mask_low_51_bits + c0
	rr2 := r2.lo & edwards25519.mask_low_51_bits + c1
	rr3 := r3.lo & edwards25519.mask_low_51_bits + c2
	rr4 := r4.lo & edwards25519.mask_low_51_bits + c3

	mut v := Element{
		l0: rr0
		l1: rr1
		l2: rr2
		l3: rr3
		l4: rr4
	}
	v = v.carry_propagate_generic()
	return v
}

// zero sets v = 0, and returns v.
pub fn (mut v Element) zero() Element {
	v = edwards25519.fe_zero
	return v
}

// one sets v = 1, and returns v.
pub fn (mut v Element) one() Element {
	v = edwards25519.fe_one
	return v
}

// reduce reduces v modulo 2^255 - 19 and returns it.
pub fn (mut v Element) reduce() Element {
	v = v.carry_propagate_generic()

	// After the light reduction we now have a edwards25519 element representation
	// v < 2^255 + 2^13 * 19, but need v < 2^255 - 19.

	// If v >= 2^255 - 19, then v + 19 >= 2^255, which would overflow 2^255 - 1,
	// generating a carry. That is, c will be 0 if v < 2^255 - 19, and 1 otherwise.
	mut c := (v.l0 + 19) >> 51
	c = (v.l1 + c) >> 51
	c = (v.l2 + c) >> 51
	c = (v.l3 + c) >> 51
	c = (v.l4 + c) >> 51

	// If v < 2^255 - 19 and c = 0, this will be a no-op. Otherwise, it's
	// effectively applying the reduction identity to the carry.
	v.l0 += 19 * c

	v.l1 += v.l0 >> 51
	v.l0 = v.l0 & edwards25519.mask_low_51_bits
	v.l2 += v.l1 >> 51
	v.l1 = v.l1 & edwards25519.mask_low_51_bits
	v.l3 += v.l2 >> 51
	v.l2 = v.l2 & edwards25519.mask_low_51_bits
	v.l4 += v.l3 >> 51
	v.l3 = v.l3 & edwards25519.mask_low_51_bits
	// no additional carry
	v.l4 = v.l4 & edwards25519.mask_low_51_bits

	return v
}

// add sets v = a + b, and returns v.
pub fn (mut v Element) add(a Element, b Element) Element {
	v.l0 = a.l0 + b.l0
	v.l1 = a.l1 + b.l1
	v.l2 = a.l2 + b.l2
	v.l3 = a.l3 + b.l3
	v.l4 = a.l4 + b.l4
	// Using the generic implementation here is actually faster than the
	// assembly. Probably because the body of this function is so simple that
	// the compiler can figure out better optimizations by inlining the carry
	// propagation.
	return v.carry_propagate_generic()
}

// subtract sets v = a - b, and returns v.
pub fn (mut v Element) subtract(a Element, b Element) Element {
	// We first add 2 * p, to guarantee the subtraction won't underflow, and
	// then subtract b (which can be up to 2^255 + 2^13 * 19).
	v.l0 = (a.l0 + 0xFFFFFFFFFFFDA) - b.l0
	v.l1 = (a.l1 + 0xFFFFFFFFFFFFE) - b.l1
	v.l2 = (a.l2 + 0xFFFFFFFFFFFFE) - b.l2
	v.l3 = (a.l3 + 0xFFFFFFFFFFFFE) - b.l3
	v.l4 = (a.l4 + 0xFFFFFFFFFFFFE) - b.l4
	return v.carry_propagate_generic()
}

// negate sets v = -a, and returns v.
pub fn (mut v Element) negate(a Element) Element {
	return v.subtract(edwards25519.fe_zero, a)
}

// invert sets v = 1/z mod p, and returns v.
//
// If z == 0, invert returns v = 0.
pub fn (mut v Element) invert(z Element) Element {
	// Inversion is implemented as exponentiation with exponent p − 2. It uses the
	// same sequence of 255 squarings and 11 multiplications as [Curve25519].
	mut z2 := Element{}
	mut z9 := Element{}
	mut z11 := Element{}
	mut z2_5_0 := Element{}
	mut z2_10_0 := Element{}
	mut z2_20_0 := Element{}
	mut z2_50_0 := Element{}
	mut z2_100_0 := Element{}
	mut t := Element{}

	z2.square(z) // 2
	t.square(z2) // 4
	t.square(t) // 8
	z9.multiply(t, z) // 9
	z11.multiply(z9, z2) // 11
	t.square(z11) // 22
	z2_5_0.multiply(t, z9) // 31 = 2^5 - 2^0

	t.square(z2_5_0) // 2^6 - 2^1
	for i := 0; i < 4; i++ {
		t.square(t) // 2^10 - 2^5
	}
	z2_10_0.multiply(t, z2_5_0) // 2^10 - 2^0

	t.square(z2_10_0) // 2^11 - 2^1
	for i := 0; i < 9; i++ {
		t.square(t) // 2^20 - 2^10
	}
	z2_20_0.multiply(t, z2_10_0) // 2^20 - 2^0

	t.square(z2_20_0) // 2^21 - 2^1
	for i := 0; i < 19; i++ {
		t.square(t) // 2^40 - 2^20
	}
	t.multiply(t, z2_20_0) // 2^40 - 2^0

	t.square(t) // 2^41 - 2^1
	for i := 0; i < 9; i++ {
		t.square(t) // 2^50 - 2^10
	}
	z2_50_0.multiply(t, z2_10_0) // 2^50 - 2^0

	t.square(z2_50_0) // 2^51 - 2^1
	for i := 0; i < 49; i++ {
		t.square(t) // 2^100 - 2^50
	}
	z2_100_0.multiply(t, z2_50_0) // 2^100 - 2^0

	t.square(z2_100_0) // 2^101 - 2^1
	for i := 0; i < 99; i++ {
		t.square(t) // 2^200 - 2^100
	}
	t.multiply(t, z2_100_0) // 2^200 - 2^0

	t.square(t) // 2^201 - 2^1
	for i := 0; i < 49; i++ {
		t.square(t) // 2^250 - 2^50
	}
	t.multiply(t, z2_50_0) // 2^250 - 2^0

	t.square(t) // 2^251 - 2^1
	t.square(t) // 2^252 - 2^2
	t.square(t) // 2^253 - 2^3
	t.square(t) // 2^254 - 2^4
	t.square(t) // 2^255 - 2^5

	return v.multiply(t, z11) // 2^255 - 21
}

// square sets v = x * x, and returns v.
pub fn (mut v Element) square(x Element) Element {
	v = fe_square_generic(x)
	return v
}

// multiply sets v = x * y, and returns v.
pub fn (mut v Element) multiply(x Element, y Element) Element {
	v = fe_mul_generic(x, y)
	return v
}

// mul_51 returns lo + hi * 2⁵¹ = a * b.
fn mul_51(a u64, b u32) (u64, u64) {
	mh, ml := bits.mul_64(a, u64(b))
	lo := ml & edwards25519.mask_low_51_bits
	hi := (mh << 13) | (ml >> 51)
	return lo, hi
}

// pow_22523 set v = x^((p-5)/8), and returns v. (p-5)/8 is 2^252-3.
pub fn (mut v Element) pow_22523(x Element) Element {
	mut t0, mut t1, mut t2 := Element{}, Element{}, Element{}

	t0.square(x) // x^2
	t1.square(t0) // x^4
	t1.square(t1) // x^8
	t1.multiply(x, t1) // x^9
	t0.multiply(t0, t1) // x^11
	t0.square(t0) // x^22
	t0.multiply(t1, t0) // x^31
	t1.square(t0) // x^62
	for i := 1; i < 5; i++ { // x^992
		t1.square(t1)
	}
	t0.multiply(t1, t0) // x^1023 -> 1023 = 2^10 - 1
	t1.square(t0) // 2^11 - 2
	for i := 1; i < 10; i++ { // 2^20 - 2^10
		t1.square(t1)
	}
	t1.multiply(t1, t0) // 2^20 - 1
	t2.square(t1) // 2^21 - 2
	for i := 1; i < 20; i++ { // 2^40 - 2^20
		t2.square(t2)
	}
	t1.multiply(t2, t1) // 2^40 - 1
	t1.square(t1) // 2^41 - 2
	for i := 1; i < 10; i++ { // 2^50 - 2^10
		t1.square(t1)
	}
	t0.multiply(t1, t0) // 2^50 - 1
	t1.square(t0) // 2^51 - 2
	for i := 1; i < 50; i++ { // 2^100 - 2^50
		t1.square(t1)
	}
	t1.multiply(t1, t0) // 2^100 - 1
	t2.square(t1) // 2^101 - 2
	for i := 1; i < 100; i++ { // 2^200 - 2^100
		t2.square(t2)
	}
	t1.multiply(t2, &t1) // 2^200 - 1
	t1.square(t1) // 2^201 - 2
	for i := 1; i < 50; i++ { // 2^250 - 2^50
		t1.square(t1)
	}
	t0.multiply(t1, t0) // 2^250 - 1
	t0.square(t0) // 2^251 - 2
	t0.square(t0) // 2^252 - 4
	return v.multiply(t0, x) // 2^252 - 3 -> x^(2^252-3)
}

// sqrt_ratio sets r to the non-negative square root of the ratio of u and v.
//
// If u/v is square, sqrt_ratio returns r and 1. If u/v is not square, sqrt_ratio
// sets r according to Section 4.3 of draft-irtf-cfrg-ristretto255-decaf448-00,
// and returns r and 0.
pub fn (mut r Element) sqrt_ratio(u Element, v Element) (Element, int) {
	mut a, mut b := Element{}, Element{}

	// r = (u * v3) * (u * v7)^((p-5)/8)
	v2 := a.square(v)
	uv3 := b.multiply(u, b.multiply(v2, v))
	uv7 := a.multiply(uv3, a.square(v2))
	r.multiply(uv3, r.pow_22523(uv7))

	mut check := a.multiply(v, a.square(r)) // check = v * r^2

	mut uneg := b.negate(u)
	correct_sign_sqrt := check.equal(u)
	flipped_sign_sqrt := check.equal(uneg)
	flipped_sign_sqrt_i := check.equal(uneg.multiply(uneg, edwards25519.sqrt_m1))

	rprime := b.multiply(r, edwards25519.sqrt_m1) // r_prime = SQRT_M1 * r
	// r = CT_selected(r_prime IF flipped_sign_sqrt | flipped_sign_sqrt_i ELSE r)
	r.selected(rprime, r, flipped_sign_sqrt | flipped_sign_sqrt_i)

	r.absolute(r) // Choose the nonnegative square root.
	return r, correct_sign_sqrt | flipped_sign_sqrt
}

// mask_64_bits returns 0xffffffff if cond is 1, and 0 otherwise.
fn mask_64_bits(cond int) u64 {
	// in go, `^` operates on bit mean NOT, flip bit
	// in v, its a ~    bitwise NOT
	return ~(u64(cond) - 1)
}

// selected sets v to a if cond == 1, and to b if cond == 0.
pub fn (mut v Element) selected(a Element, b Element, cond int) Element {
	// see above notes
	m := mask_64_bits(cond)
	v.l0 = (m & a.l0) | (~m & b.l0)
	v.l1 = (m & a.l1) | (~m & b.l1)
	v.l2 = (m & a.l2) | (~m & b.l2)
	v.l3 = (m & a.l3) | (~m & b.l3)
	v.l4 = (m & a.l4) | (~m & b.l4)
	return v
}

// is_negative returns 1 if v is negative, and 0 otherwise.
pub fn (mut v Element) is_negative() int {
	return int(v.bytes()[0] & 1)
}

// absolute sets v to |u|, and returns v.
pub fn (mut v Element) absolute(u Element) Element {
	mut e := Element{}
	mut uk := u
	return v.selected(e.negate(uk), uk, uk.is_negative())
}

// set sets v = a, and returns v.
pub fn (mut v Element) set(a Element) Element {
	v = a
	return v
}

// set_bytes sets v to x, where x is a 32-byte little-endian encoding. If x is
// not of the right length, SetUniformBytes returns an error, and the
// receiver is unchanged.
//
// Consistent with RFC 7748, the most significant bit (the high bit of the
// last byte) is ignored, and non-canonical values (2^255-19 through 2^255-1)
// are accepted. Note that this is laxer than specified by RFC 8032.
pub fn (mut v Element) set_bytes(x []byte) ?Element {
	if x.len != 32 {
		return error('edwards25519: invalid edwards25519 element input size')
	}

	// Bits 0:51 (bytes 0:8, bits 0:64, shift 0, mask 51).
	v.l0 = binary.little_endian_u64(x[0..8])
	v.l0 &= edwards25519.mask_low_51_bits
	// Bits 51:102 (bytes 6:14, bits 48:112, shift 3, mask 51).
	v.l1 = binary.little_endian_u64(x[6..14]) >> 3
	v.l1 &= edwards25519.mask_low_51_bits
	// Bits 102:153 (bytes 12:20, bits 96:160, shift 6, mask 51).
	v.l2 = binary.little_endian_u64(x[12..20]) >> 6
	v.l2 &= edwards25519.mask_low_51_bits
	// Bits 153:204 (bytes 19:27, bits 152:216, shift 1, mask 51).
	v.l3 = binary.little_endian_u64(x[19..27]) >> 1
	v.l3 &= edwards25519.mask_low_51_bits
	// Bits 204:251 (bytes 24:32, bits 192:256, shift 12, mask 51).
	// Note: not bytes 25:33, shift 4, to avoid overread.
	v.l4 = binary.little_endian_u64(x[24..32]) >> 12
	v.l4 &= edwards25519.mask_low_51_bits

	return v
}

// bytes returns the canonical 32-byte little-endian encoding of v.
pub fn (mut v Element) bytes() []byte {
	// This function is outlined to make the allocations inline in the caller
	// rather than happen on the heap.
	// out := v.bytes_generic()
	return v.bytes_generic()
}

fn (mut v Element) bytes_generic() []byte {
	mut out := []byte{len: 32}

	v = v.reduce()

	mut buf := []byte{len: 8}
	idxs := [v.l0, v.l1, v.l2, v.l3, v.l4]
	for i, l in idxs {
		bits_offset := i * 51
		binary.little_endian_put_u64(mut buf, l << u32(bits_offset % 8))
		for j, bb in buf {
			off := bits_offset / 8 + j
			if off >= out.len {
				break
			}
			out[off] |= bb
		}
	}

	return out
}

// equal returns 1 if v and u are equal, and 0 otherwise.
pub fn (mut v Element) equal(ue Element) int {
	mut u := ue
	sa := u.bytes()
	sv := v.bytes()
	return subtle.constant_time_compare(sa, sv)
}

// swap swaps v and u if cond == 1 or leaves them unchanged if cond == 0, and returns v.
pub fn (mut v Element) swap(mut u Element, cond int) {
	// mut u := ue
	m := mask_64_bits(cond)
	mut t := m & (v.l0 ^ u.l0)
	v.l0 ^= t
	u.l0 ^= t
	t = m & (v.l1 ^ u.l1)
	v.l1 ^= t
	u.l1 ^= t
	t = m & (v.l2 ^ u.l2)
	v.l2 ^= t
	u.l2 ^= t
	t = m & (v.l3 ^ u.l3)
	v.l3 ^= t
	u.l3 ^= t
	t = m & (v.l4 ^ u.l4)
	v.l4 ^= t
	u.l4 ^= t
}

// mult_32 sets v = x * y, and returns v.
pub fn (mut v Element) mult_32(x Element, y u32) Element {
	x0lo, x0hi := mul_51(x.l0, y)
	x1lo, x1hi := mul_51(x.l1, y)
	x2lo, x2hi := mul_51(x.l2, y)
	x3lo, x3hi := mul_51(x.l3, y)
	x4lo, x4hi := mul_51(x.l4, y)
	v.l0 = x0lo + 19 * x4hi // carried over per the reduction identity
	v.l1 = x1lo + x0hi
	v.l2 = x2lo + x1hi
	v.l3 = x3lo + x2hi
	v.l4 = x4lo + x3hi
	// The hi portions are going to be only 32 bits, plus any previous excess,
	// so we can skip the carry propagation.
	return v
}

fn swap_endianness(mut buf []byte) []byte {
	for i := 0; i < buf.len / 2; i++ {
		buf[i], buf[buf.len - i - 1] = buf[buf.len - i - 1], buf[i]
	}
	return buf
}
