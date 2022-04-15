module edwards25519

// extended_coordinates returns v in extended coordinates (X:Y:Z:T) where
// x = X/Z, y = Y/Z, and xy = T/Z as in https://eprint.iacr.org/2008/522.
fn (mut v Point) extended_coordinates() (Element, Element, Element, Element) {
	// This function is outlined to make the allocations inline in the caller
	// rather than happen on the heap. Don't change the style without making
	// sure it doesn't increase the inliner cost.
	mut e := []Element{len: 4}
	x, y, z, t := v.extended_coordinates_generic(mut e)
	return x, y, z, t
}

fn (mut v Point) extended_coordinates_generic(mut e []Element) (Element, Element, Element, Element) {
	check_initialized(v)
	x := e[0].set(v.x)
	y := e[1].set(v.y)
	z := e[2].set(v.z)
	t := e[3].set(v.t)
	return x, y, z, t
}

// Given k > 0, set s = s**(2*i).
fn (mut s Scalar) pow2k(k int) {
	for i := 0; i < k; i++ {
		s.multiply(s, s)
	}
}

// set_extended_coordinates sets v = (X:Y:Z:T) in extended coordinates where
// x = X/Z, y = Y/Z, and xy = T/Z as in https://eprint.iacr.org/2008/522.
//
// If the coordinates are invalid or don't represent a valid point on the curve,
// set_extended_coordinates returns an error and the receiver is
// unchanged. Otherwise, set_extended_coordinates returns v.
fn (mut v Point) set_extended_coordinates(x Element, y Element, z Element, t Element) ?Point {
	if !is_on_curve(x, y, z, t) {
		return error('edwards25519: invalid point coordinates')
	}
	v.x.set(x)
	v.y.set(y)
	v.z.set(z)
	v.t.set(t)
	return v
}

fn is_on_curve(x Element, y Element, z Element, t Element) bool {
	mut lhs := Element{}
	mut rhs := Element{}

	mut xx := Element{}
	xx.square(x)

	mut yy := Element{}
	yy.square(y)

	mut zz := Element{}
	zz.square(z)
	// zz := new(Element).Square(Z)

	mut tt := Element{}
	tt.square(t)
	// tt := new(Element).Square(T)
	// -x² + y² = 1 + dx²y²
	// -(X/Z)² + (Y/Z)² = 1 + d(T/Z)²
	// -X² + Y² = Z² + dT²
	lhs.subtract(yy, xx)
	mut d := d_const
	rhs.multiply(d, tt)
	rhs.add(rhs, zz)
	if lhs.equal(rhs) != 1 {
		return false
	}
	// xy = T/Z
	// XY/Z² = T/Z
	// XY = TZ
	lhs.multiply(x, y)
	rhs.multiply(t, z)
	return lhs.equal(rhs) == 1
}

// bytes_montgomery converts v to a point on the birationally-equivalent
// Curve25519 Montgomery curve, and returns its canonical 32 bytes encoding
// according to RFC 7748.
//
// Note that bytes_montgomery only encodes the u-coordinate, so v and -v encode
// to the same value. If v is the identity point, bytes_montgomery returns 32
// zero bytes, analogously to the X25519 function.
pub fn (mut v Point) bytes_montgomery() []u8 {
	// This function is outlined to make the allocations inline in the caller
	// rather than happen on the heap.
	mut buf := [32]u8{}
	return v.bytes_montgomery_generic(mut buf)
}

fn (mut v Point) bytes_montgomery_generic(mut buf [32]u8) []u8 {
	check_initialized(v)

	// RFC 7748, Section 4.1 provides the bilinear map to calculate the
	// Montgomery u-coordinate
	//
	//              u = (1 + y) / (1 - y)
	//
	// where y = Y / Z.

	mut y := Element{}
	mut recip := Element{}
	mut u := Element{}

	y.multiply(v.y, y.invert(v.z)) // y = Y / Z
	recip.invert(recip.subtract(fe_one, &y)) // r = 1/(1 - y)
	u.multiply(u.add(fe_one, y), recip) // u = (1 + y)*r

	return copy_field_element(mut buf, mut u)
}

// mult_by_cofactor sets v = 8 * p, and returns v.
pub fn (mut v Point) mult_by_cofactor(p Point) Point {
	check_initialized(p)
	mut result := ProjectiveP1{}
	mut pp := ProjectiveP2{}
	pp.from_p3(p)
	result.double(pp)
	pp.from_p1(result)
	result.double(pp)
	pp.from_p1(result)
	result.double(pp)
	return v.from_p1(result)
}

// invert sets s to the inverse of a nonzero scalar v, and returns s.
//
// If t is zero, invert returns zero.
pub fn (mut s Scalar) invert(t Scalar) Scalar {
	// Uses a hardcoded sliding window of width 4.
	mut table := [8]Scalar{}
	mut tt := Scalar{}
	tt.multiply(t, t)
	table[0] = t
	for i := 0; i < 7; i++ {
		table[i + 1].multiply(table[i], tt)
	}
	// Now table = [t**1, t**3, t**7, t**11, t**13, t**15]
	// so t**k = t[k/2] for odd k

	// To compute the sliding window digits, use the following Sage script:

	// sage: import itertools
	// sage: def sliding_window(w,k):
	// ....:     digits = []
	// ....:     while k > 0:
	// ....:         if k % 2 == 1:
	// ....:             kmod = k % (2**w)
	// ....:             digits.append(kmod)
	// ....:             k = k - kmod
	// ....:         else:
	// ....:             digits.append(0)
	// ....:         k = k // 2
	// ....:     return digits

	// Now we can compute s roughly as follows:

	// sage: s = 1
	// sage: for coeff in reversed(sliding_window(4,l-2)):
	// ....:     s = s*s
	// ....:     if coeff > 0 :
	// ....:         s = s*t**coeff

	// This works on one bit at a time, with many runs of zeros.
	// The digits can be collapsed into [(count, coeff)] as follows:

	// sage: [(len(list(group)),d) for d,group in itertools.groupby(sliding_window(4,l-2))]

	// Entries of the form (k, 0) turn into pow2k(k)
	// Entries of the form (1, coeff) turn into a squaring and then a table lookup.
	// We can fold the squaring into the previous pow2k(k) as pow2k(k+1).

	s = table[1 / 2]
	s.pow2k(127 + 1)
	s.multiply(s, table[1 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[9 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[11 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[13 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[15 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[7 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[15 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[5 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[1 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[15 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[15 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[7 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[3 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[11 / 2])
	s.pow2k(5 + 1)
	s.multiply(s, table[11 / 2])
	s.pow2k(9 + 1)
	s.multiply(s, table[9 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[3 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[3 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[3 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[9 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[7 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[3 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[13 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[7 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[9 / 2])
	s.pow2k(3 + 1)
	s.multiply(s, table[15 / 2])
	s.pow2k(4 + 1)
	s.multiply(s, table[11 / 2])

	return s
}

// multi_scalar_mult sets v = sum(scalars[i] * points[i]), and returns v.
//
// Execution time depends only on the lengths of the two slices, which must match.
pub fn (mut v Point) multi_scalar_mult(scalars []Scalar, points []Point) Point {
	if scalars.len != points.len {
		panic('edwards25519: called multi_scalar_mult with different size inputs')
	}
	check_initialized(...points)

	mut sc := scalars.clone()
	// Proceed as in the single-base case, but share doublings
	// between each point in the multiscalar equation.

	// Build lookup tables for each point
	mut tables := []ProjLookupTable{len: points.len}
	for i, _ in tables {
		tables[i].from_p3(points[i])
	}
	// Compute signed radix-16 digits for each scalar
	// digits := make([][64]int8, len(scalars))
	mut digits := [][]i8{len: sc.len, init: []i8{len: 64, cap: 64}}

	for j, _ in digits {
		digits[j] = sc[j].signed_radix16()
	}

	// Unwrap first loop iteration to save computing 16*identity
	mut multiple := ProjectiveCached{}
	mut tmp1 := ProjectiveP1{}
	mut tmp2 := ProjectiveP2{}
	// Lookup-and-add the appropriate multiple of each input point
	for k, _ in tables {
		tables[k].select_into(mut multiple, digits[k][63])
		tmp1.add(v, multiple) // tmp1 = v + x_(j,63)*Q in P1xP1 coords
		v.from_p1(tmp1) // update v
	}
	tmp2.from_p3(v) // set up tmp2 = v in P2 coords for next iteration
	for r := 62; r >= 0; r-- {
		tmp1.double(tmp2) // tmp1 =  2*(prev) in P1xP1 coords
		tmp2.from_p1(tmp1) // tmp2 =  2*(prev) in P2 coords
		tmp1.double(tmp2) // tmp1 =  4*(prev) in P1xP1 coords
		tmp2.from_p1(tmp1) // tmp2 =  4*(prev) in P2 coords
		tmp1.double(tmp2) // tmp1 =  8*(prev) in P1xP1 coords
		tmp2.from_p1(tmp1) // tmp2 =  8*(prev) in P2 coords
		tmp1.double(tmp2) // tmp1 = 16*(prev) in P1xP1 coords
		v.from_p1(tmp1) //    v = 16*(prev) in P3 coords
		// Lookup-and-add the appropriate multiple of each input point
		for s, _ in tables {
			tables[s].select_into(mut multiple, digits[s][r])
			tmp1.add(v, multiple) // tmp1 = v + x_(j,i)*Q in P1xP1 coords
			v.from_p1(tmp1) // update v
		}
		tmp2.from_p3(v) // set up tmp2 = v in P2 coords for next iteration
	}
	return v
}

// vartime_multiscalar_mult sets v = sum(scalars[i] * points[i]), and returns v.
//
// Execution time depends on the inputs.
pub fn (mut v Point) vartime_multiscalar_mult(scalars []Scalar, points []Point) Point {
	if scalars.len != points.len {
		panic('edwards25519: called VarTimeMultiScalarMult with different size inputs')
	}
	check_initialized(...points)

	// Generalize double-base NAF computation to arbitrary sizes.
	// Here all the points are dynamic, so we only use the smaller
	// tables.

	// Build lookup tables for each point
	mut tables := []NafLookupTable5{len: points.len}
	for i, _ in tables {
		tables[i].from_p3(points[i])
	}
	mut sc := scalars.clone()
	// Compute a NAF for each scalar
	// mut nafs := make([][256]int8, len(scalars))
	mut nafs := [][]i8{len: sc.len, init: []i8{len: 256, cap: 256}}
	for j, _ in nafs {
		nafs[j] = sc[j].non_adjacent_form(5)
	}

	mut multiple := ProjectiveCached{}
	mut tmp1 := ProjectiveP1{}
	mut tmp2 := ProjectiveP2{}
	tmp2.zero()

	// Move from high to low bits, doubling the accumulator
	// at each iteration and checking whether there is a nonzero
	// coefficient to look up a multiple of.
	//
	// Skip trying to find the first nonzero coefficent, because
	// searching might be more work than a few extra doublings.
	// k == i, l == j
	for k := 255; k >= 0; k-- {
		tmp1.double(tmp2)

		for l, _ in nafs {
			if nafs[l][k] > 0 {
				v.from_p1(tmp1)
				tables[l].select_into(mut multiple, nafs[l][k])
				tmp1.add(v, multiple)
			} else if nafs[l][k] < 0 {
				v.from_p1(tmp1)
				tables[l].select_into(mut multiple, -nafs[l][k])
				tmp1.sub(v, multiple)
			}
		}

		tmp2.from_p1(tmp1)
	}

	v.from_p2(tmp2)
	return v
}
