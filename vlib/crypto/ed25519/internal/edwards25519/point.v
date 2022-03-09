module edwards25519

const (
	// d is a constant in the curve equation.
	d_bytes   = [byte(0xa3), 0x78, 0x59, 0x13, 0xca, 0x4d, 0xeb, 0x75, 0xab, 0xd8, 0x41, 0x41,
		0x4d, 0x0a, 0x70, 0x00, 0x98, 0xe8, 0x79, 0x77, 0x79, 0x40, 0xc7, 0x8c, 0x73, 0xfe, 0x6f,
		0x2b, 0xee, 0x6c, 0x03, 0x52]
	id_bytes  = [byte(1), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0]
	gen_bytes = [byte(0x58), 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
		0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66,
		0x66, 0x66, 0x66, 0x66, 0x66]
	d_const   = d_const_generate() or { panic(err.msg) }
	d2_const  = d2_const_generate() or { panic(err.msg) }
	// id_point is the point at infinity.
	id_point  = id_point_generate() or { panic(err.msg) }
	// generator point
	gen_point = generator() or { panic(err.msg) }
)

fn d_const_generate() ?Element {
	mut v := Element{}
	v.set_bytes(edwards25519.d_bytes) ?
	return v
}

fn d2_const_generate() ?Element {
	mut v := Element{}
	v.add(edwards25519.d_const, edwards25519.d_const)
	return v
}

// id_point_generate is the point at infinity.
fn id_point_generate() ?Point {
	mut p := Point{}
	p.set_bytes(edwards25519.id_bytes) ?
	return p
}

// generator is the canonical curve basepoint. See TestGenerator for the
// correspondence of this encoding with the values in RFC 8032.
fn generator() ?Point {
	mut p := Point{}
	p.set_bytes(edwards25519.gen_bytes) ?
	return p
}

// Point types.
struct ProjectiveP1 {
mut:
	x Element
	y Element
	z Element
	t Element
}

struct ProjectiveP2 {
mut:
	x Element
	y Element
	z Element
}

// Point represents a point on the edwards25519 curve.
//
// This type works similarly to math/big.Int, and all arguments and receivers
// are allowed to alias.
//
// The zero value is NOT valid, and it may be used only as a receiver.
pub struct Point {
mut:
	// The point is internally represented in extended coordinates (x, y, z, T)
	// where x = x/z, y = y/z, and xy = T/z per https://eprint.iacr.org/2008/522.
	x Element
	y Element
	z Element
	t Element
	// Make the type not comparable (i.e. used with == or as a map key), as
	// equivalent points can be represented by different values.
	// _ incomparable
}

fn check_initialized(points ...Point) {
	for _, p in points {
		if p.x == fe_zero && p.y == fe_zero {
			panic('edwards25519: use of uninitialized Point')
		}
	}
}

struct ProjectiveCached {
mut:
	ypx Element // y + x
	ymx Element // y - x
	z   Element
	t2d Element
}

struct AffineCached {
mut:
	ypx Element // y + x
	ymx Element // y - x
	t2d Element
}

fn (mut v ProjectiveP2) zero() ProjectiveP2 {
	v.x.zero()
	v.y.one()
	v.z.one()
	return v
}

// set_bytes sets v = x, where x is a 32-byte encoding of v. If x does not
// represent a valid point on the curve, set_bytes returns an error and
// the receiver is unchanged. Otherwise, set_bytes returns v.
//
// Note that set_bytes accepts all non-canonical encodings of valid points.
// That is, it follows decoding rules that match most implementations in
// the ecosystem rather than RFC 8032.
pub fn (mut v Point) set_bytes(x []byte) ?Point {
	// Specifically, the non-canonical encodings that are accepted are
	//   1) the ones where the edwards25519 element is not reduced (see the
	//      (*edwards25519.Element).set_bytes docs) and
	//   2) the ones where the x-coordinate is zero and the sign bit is set.
	//
	// This is consistent with crypto/ed25519/internal/edwards25519. Read more
	// at https://hdevalence.ca/blog/2020-10-04-its-25519am, specifically the
	// "Canonical A, R" section.
	mut el0 := Element{}
	y := el0.set_bytes(x) or { return error('edwards25519: invalid point encoding length') }

	// -x² + y² = 1 + dx²y²
	// x² + dx²y² = x²(dy² + 1) = y² - 1
	// x² = (y² - 1) / (dy² + 1)

	// u = y² - 1
	mut el1 := Element{}
	y2 := el1.square(y)
	mut el2 := Element{}
	u := el2.subtract(y2, fe_one)

	// v = dy² + 1
	mut el3 := Element{}
	mut vv := el3.multiply(y2, edwards25519.d_const)
	vv = vv.add(vv, fe_one)

	// x = +√(u/v)
	mut el4 := Element{}
	mut xx, was_square := el4.sqrt_ratio(u, vv)
	if was_square == 0 {
		return error('edwards25519: invalid point encoding')
	}

	// selected the negative square root if the sign bit is set.
	mut el5 := Element{}
	xx_neg := el5.negate(xx)
	xx.selected(xx_neg, xx, int(x[31] >> 7))

	v.x.set(xx)
	v.y.set(y)
	v.z.one()
	v.t.multiply(xx, y) // xy = T / z

	return v
}

// set sets v = u, and returns v.
pub fn (mut v Point) set(u Point) Point {
	v = u
	return v
}

// new_identity_point returns a new Point set to the identity.
pub fn new_identity_point() Point {
	mut p := Point{}
	return p.set(edwards25519.id_point)
}

// new_generator_point returns a new Point set to the canonical generator.
pub fn new_generator_point() Point {
	mut p := Point{}
	return p.set(edwards25519.gen_point)
}

fn (mut v ProjectiveCached) zero() ProjectiveCached {
	v.ypx.one()
	v.ymx.one()
	v.z.one()
	v.t2d.zero()
	return v
}

fn (mut v AffineCached) zero() AffineCached {
	v.ypx.one()
	v.ymx.one()
	v.t2d.zero()
	return v
}

// Encoding.

// bytes returns the canonical 32-byte encoding of v, according to RFC 8032,
// Section 5.1.2.
pub fn (mut v Point) bytes() []byte {
	// This function is outlined to make the allocations inline in the caller
	// rather than happen on the heap.
	mut buf := [32]byte{}
	return v.bytes_generic(mut buf)
}

fn (mut v Point) bytes_generic(mut buf [32]byte) []byte {
	check_initialized(v)

	mut zinv := Element{}
	mut x := Element{}
	mut y := Element{}
	zinv.invert(v.z) // zinv = 1 / z
	x.multiply(v.x, zinv) // x = x / z
	y.multiply(v.y, zinv) // y = y / z

	mut out := copy_field_element(mut buf, mut y)
	unsafe {
		// out[31] |= byte(x.is_negative() << 7) //original one
		out[31] |= byte(x.is_negative() * 128) // x << 7 == x * 2^7
	}
	return out
}

fn copy_field_element(mut buf [32]byte, mut v Element) []byte {
	// this fail in test
	/*
	copy(mut buf[..], v.bytes())
	return buf[..]
	*/

	// this pass the test
	mut out := []byte{len: 32}
	for i := 0; i <= buf.len - 1; i++ {
		out[i] = v.bytes()[i]
	}

	return out
}

// Conversions.

fn (mut v ProjectiveP2) from_p1(p ProjectiveP1) ProjectiveP2 {
	v.x.multiply(p.x, p.t)
	v.y.multiply(p.y, p.z)
	v.z.multiply(p.z, p.t)
	return v
}

fn (mut v ProjectiveP2) from_p3(p Point) ProjectiveP2 {
	v.x.set(p.x)
	v.y.set(p.y)
	v.z.set(p.z)
	return v
}

fn (mut v Point) from_p1(p ProjectiveP1) Point {
	v.x.multiply(p.x, p.t)
	v.y.multiply(p.y, p.z)
	v.z.multiply(p.z, p.t)
	v.t.multiply(p.x, p.y)
	return v
}

fn (mut v Point) from_p2(p ProjectiveP2) Point {
	v.x.multiply(p.x, p.z)
	v.y.multiply(p.y, p.z)
	v.z.square(p.z)
	v.t.multiply(p.x, p.y)
	return v
}

fn (mut v ProjectiveCached) from_p3(p Point) ProjectiveCached {
	v.ypx.add(p.y, p.x)
	v.ymx.subtract(p.y, p.x)
	v.z.set(p.z)
	v.t2d.multiply(p.t, edwards25519.d2_const)
	return v
}

fn (mut v AffineCached) from_p3(p Point) AffineCached {
	v.ypx.add(p.y, p.x)
	v.ymx.subtract(p.y, p.x)
	v.t2d.multiply(p.t, edwards25519.d2_const)

	mut invz := Element{}
	invz.invert(p.z)
	v.ypx.multiply(v.ypx, invz)
	v.ymx.multiply(v.ymx, invz)
	v.t2d.multiply(v.t2d, invz)
	return v
}

// (Re)addition and subtraction.

// add sets v = p + q, and returns v.
pub fn (mut v Point) add(p Point, q Point) Point {
	check_initialized(p, q)
	mut pc := ProjectiveCached{}
	mut p1 := ProjectiveP1{}
	qcached := pc.from_p3(q)

	result := p1.add(p, qcached)
	return v.from_p1(result)
}

// subtract sets v = p - q, and returns v.
pub fn (mut v Point) subtract(p Point, q Point) Point {
	check_initialized(p, q)
	mut pc := ProjectiveCached{}
	mut p1 := ProjectiveP1{}
	qcached := pc.from_p3(q)
	result := p1.sub(p, qcached)
	return v.from_p1(result)
}

fn (mut v ProjectiveP1) add(p Point, q ProjectiveCached) ProjectiveP1 {
	// var ypx, ymx, pp, mm, tt2d, zz2 edwards25519.Element
	mut ypx := Element{}
	mut ymx := Element{}
	mut pp := Element{}
	mut mm := Element{}
	mut tt2d := Element{}
	mut zz2 := Element{}

	ypx.add(p.y, p.x)
	ymx.subtract(p.y, p.x)

	pp.multiply(ypx, q.ypx)
	mm.multiply(ymx, q.ymx)
	tt2d.multiply(p.t, q.t2d)
	zz2.multiply(p.z, q.z)

	zz2.add(zz2, zz2)

	v.x.subtract(pp, mm)
	v.y.add(pp, mm)
	v.z.add(zz2, tt2d)
	v.t.subtract(zz2, tt2d)
	return v
}

fn (mut v ProjectiveP1) sub(p Point, q ProjectiveCached) ProjectiveP1 {
	mut ypx := Element{}
	mut ymx := Element{}
	mut pp := Element{}
	mut mm := Element{}
	mut tt2d := Element{}
	mut zz2 := Element{}

	ypx.add(p.y, p.x)
	ymx.subtract(p.y, p.x)

	pp.multiply(&ypx, q.ymx) // flipped sign
	mm.multiply(&ymx, q.ypx) // flipped sign
	tt2d.multiply(p.t, q.t2d)
	zz2.multiply(p.z, q.z)

	zz2.add(zz2, zz2)

	v.x.subtract(pp, mm)
	v.y.add(pp, mm)
	v.z.subtract(zz2, tt2d) // flipped sign
	v.t.add(zz2, tt2d) // flipped sign
	return v
}

fn (mut v ProjectiveP1) add_affine(p Point, q AffineCached) ProjectiveP1 {
	mut ypx := Element{}
	mut ymx := Element{}
	mut pp := Element{}
	mut mm := Element{}
	mut tt2d := Element{}
	mut z2 := Element{}

	ypx.add(p.y, p.x)
	ymx.subtract(p.y, p.x)

	pp.multiply(&ypx, q.ypx)
	mm.multiply(&ymx, q.ymx)
	tt2d.multiply(p.t, q.t2d)

	z2.add(p.z, p.z)

	v.x.subtract(pp, mm)
	v.y.add(pp, mm)
	v.z.add(z2, tt2d)
	v.t.subtract(z2, tt2d)
	return v
}

fn (mut v ProjectiveP1) sub_affine(p Point, q AffineCached) ProjectiveP1 {
	mut ypx := Element{}
	mut ymx := Element{}
	mut pp := Element{}
	mut mm := Element{}
	mut tt2d := Element{}
	mut z2 := Element{}

	ypx.add(p.y, p.x)
	ymx.subtract(p.y, p.x)

	pp.multiply(ypx, q.ymx) // flipped sign
	mm.multiply(ymx, q.ypx) // flipped sign
	tt2d.multiply(p.t, q.t2d)

	z2.add(p.z, p.z)

	v.x.subtract(pp, mm)
	v.y.add(pp, mm)
	v.z.subtract(z2, tt2d) // flipped sign
	v.t.add(z2, tt2d) // flipped sign
	return v
}

// Doubling.

fn (mut v ProjectiveP1) double(p ProjectiveP2) ProjectiveP1 {
	// var xx, yy, zz2, xplusysq edwards25519.Element
	mut xx := Element{}
	mut yy := Element{}
	mut zz2 := Element{}
	mut xplusysq := Element{}

	xx.square(p.x)
	yy.square(p.y)
	zz2.square(p.z)
	zz2.add(zz2, zz2)
	xplusysq.add(p.x, p.y)
	xplusysq.square(xplusysq)

	v.y.add(yy, xx)
	v.z.subtract(yy, xx)

	v.x.subtract(xplusysq, v.y)
	v.t.subtract(zz2, v.z)
	return v
}

// Negation.

// negate sets v = -p, and returns v.
pub fn (mut v Point) negate(p Point) Point {
	check_initialized(p)
	v.x.negate(p.x)
	v.y.set(p.y)
	v.z.set(p.z)
	v.t.negate(p.t)
	return v
}

// equal returns 1 if v is equivalent to u, and 0 otherwise.
pub fn (mut v Point) equal(u Point) int {
	check_initialized(v, u)

	mut t1 := Element{}
	mut t2 := Element{}
	mut t3 := Element{}
	mut t4 := Element{}

	t1.multiply(v.x, u.z)
	t2.multiply(u.x, v.z)
	t3.multiply(v.y, u.z)
	t4.multiply(u.y, v.z)

	return t1.equal(t2) & t3.equal(t4)
}

// Constant-time operations

// selected sets v to a if cond == 1 and to b if cond == 0.
fn (mut v ProjectiveCached) selected(a ProjectiveCached, b ProjectiveCached, cond int) ProjectiveCached {
	v.ypx.selected(a.ypx, b.ypx, cond)
	v.ymx.selected(a.ymx, b.ymx, cond)
	v.z.selected(a.z, b.z, cond)
	v.t2d.selected(a.t2d, b.t2d, cond)
	return v
}

// selected sets v to a if cond == 1 and to b if cond == 0.
fn (mut v AffineCached) selected(a AffineCached, b AffineCached, cond int) AffineCached {
	v.ypx.selected(a.ypx, b.ypx, cond)
	v.ymx.selected(a.ymx, b.ymx, cond)
	v.t2d.selected(a.t2d, b.t2d, cond)
	return v
}

// cond_neg negates v if cond == 1 and leaves it unchanged if cond == 0.
fn (mut v ProjectiveCached) cond_neg(cond int) ProjectiveCached {
	mut el := Element{}
	v.ypx.swap(mut v.ymx, cond)
	v.t2d.selected(el.negate(v.t2d), v.t2d, cond)
	return v
}

// cond_neg negates v if cond == 1 and leaves it unchanged if cond == 0.
fn (mut v AffineCached) cond_neg(cond int) AffineCached {
	mut el := Element{}
	v.ypx.swap(mut v.ymx, cond)
	v.t2d.selected(el.negate(v.t2d), v.t2d, cond)
	return v
}

fn check_on_curve(points ...Point) bool {
	for p in points {
		mut xx := Element{}
		mut yy := Element{}
		mut zz := Element{}
		mut zzzz := Element{}
		xx.square(p.x)
		yy.square(p.y)
		zz.square(p.z)
		zzzz.square(zz)
		// -x² + y² = 1 + dx²y²
		// -(X/Z)² + (Y/Z)² = 1 + d(X/Z)²(Y/Z)²
		// (-X² + Y²)/Z² = 1 + (dX²Y²)/Z⁴
		// (-X² + Y²)*Z² = Z⁴ + dX²Y²
		mut lhs := Element{}
		mut rhs := Element{}
		lhs.subtract(yy, xx)
		lhs.multiply(lhs, zz)
		rhs.multiply(edwards25519.d_const, xx)
		rhs.multiply(rhs, yy)
		rhs.add(rhs, zzzz)

		if lhs.equal(rhs) != 1 {
			return false
		}
		/*
		if lhs.equal(rhs) != 1 {
			lg.error('X, Y, and Z do not specify a point on the curve\nX = $p.x \nY = $p.y\nZ = $p.z')
		}*/

		// xy = T/Z
		lhs.multiply(p.x, p.y)
		rhs.multiply(p.z, p.t)
		/*
		if lhs.equal(rhs) != 1 {
			lg.error('point $i is not valid\nX = $p.x\nY = $p.y\nZ = $p.z')
		}*/
		if lhs.equal(rhs) != 1 {
			return false
		}
	}
	return true
}
