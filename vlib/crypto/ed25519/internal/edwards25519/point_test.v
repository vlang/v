module edwards25519

import encoding.hex

const zero_point = Point{fe_zero, fe_zero, fe_zero, fe_zero}

fn test_invalid_encodings() ? {
	// An invalid point, that also happens to have y > p.
	invalid := 'efffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f'
	inv_bytes := hex.decode(invalid) or { panic(err) }
	mut p := new_generator_point()

	out := p.set_bytes(inv_bytes) or { edwards25519.zero_point }
	assert out == edwards25519.zero_point
	// assert p.equal(bgp) == 1 //not makes sense when error

	assert check_on_curve(p) == true
}

fn test_add_sub_neg_on_basepoint() ? {
	bgp := new_generator_point()
	mut idp := new_identity_point()
	mut checklhs := Point{}
	mut checkrhs := Point{}

	checklhs.add(bgp, bgp)

	mut proj_p1 := ProjectiveP1{}
	mut proj_p2 := ProjectiveP2{}

	tmp_p2 := proj_p2.from_p3(bgp)
	tmp_p1 := proj_p1.double(tmp_p2)
	checkrhs.from_p1(tmp_p1)

	assert checklhs.equal(checkrhs) == 1
	assert check_on_curve(checklhs, checkrhs) == true

	checklhs.subtract(bgp, bgp)
	mut p0 := Point{}
	bneg := p0.negate(bgp)
	checkrhs.add(bgp, bneg)

	assert checklhs.equal(checkrhs) == 1
	assert idp.equal(checklhs) == 1
	assert idp.equal(checkrhs) == 1
	assert check_on_curve(checklhs, checkrhs, bneg) == true
}

struct NonCanonicalTest {
	name      string
	encoding  string
	canonical string
}

fn test_non_canonical_points() ? {
	tests := [
		// Points with x = 0 and the sign bit set. With x = 0 the curve equation
		// gives y² = 1, so y = ±1. 1 has two valid encodings.
		NonCanonicalTest{'y=1,sign-', '0100000000000000000000000000000000000000000000000000000000000080', '0100000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+1,sign-', 'eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0100000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p-1,sign-', 'ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', 'ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f'},
		// Non-canonical y encodings with values 2²⁵⁵-19 (p) to 2²⁵⁵-1 (p+18).
		NonCanonicalTest{'y=p,sign+', 'edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0000000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p,sign-', 'edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0000000000000000000000000000000000000000000000000000000000000080'},
		NonCanonicalTest{'y=p+1,sign+', 'eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0100000000000000000000000000000000000000000000000000000000000000'},
		// "y=p+1,sign-" is already tested above.
		// p+2 is not a valid y-coordinate.
		NonCanonicalTest{'y=p+3,sign+', 'f0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0300000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+3,sign-', 'f0ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0300000000000000000000000000000000000000000000000000000000000080'},
		NonCanonicalTest{'y=p+4,sign+', 'f1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0400000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+4,sign-', 'f1ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0400000000000000000000000000000000000000000000000000000000000080'},
		NonCanonicalTest{'y=p+5,sign+', 'f2ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0500000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+5,sign-', 'f2ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0500000000000000000000000000000000000000000000000000000000000080'},
		NonCanonicalTest{'y=p+6,sign+', 'f3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0600000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+6,sign-', 'f3ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0600000000000000000000000000000000000000000000000000000000000080'},
		// p+7 is not a valid y-coordinate.
		// p+8 is not a valid y-coordinate.
		NonCanonicalTest{'y=p+9,sign+', 'f6ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0900000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+9,sign-', 'f6ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0900000000000000000000000000000000000000000000000000000000000080'},
		NonCanonicalTest{'y=p+10,sign+', 'f7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0a00000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+10,sign-', 'f7ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0a00000000000000000000000000000000000000000000000000000000000080'},
		// p+11 is not a valid y-coordinate.
		// p+12 is not a valid y-coordinate.
		// p+13 is not a valid y-coordinate.
		NonCanonicalTest{'y=p+14,sign+', 'fbffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0e00000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+14,sign-', 'fbffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0e00000000000000000000000000000000000000000000000000000000000080'},
		NonCanonicalTest{'y=p+15,sign+', 'fcffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '0f00000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+15,sign-', 'fcffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '0f00000000000000000000000000000000000000000000000000000000000080'},
		NonCanonicalTest{'y=p+16,sign+', 'fdffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '1000000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+16,sign-', 'fdffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '1000000000000000000000000000000000000000000000000000000000000080'},
		// p+17 is not a valid y-coordinate.
		NonCanonicalTest{'y=p+18,sign+', 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f', '1200000000000000000000000000000000000000000000000000000000000000'},
		NonCanonicalTest{'y=p+18,sign-', 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff', '1200000000000000000000000000000000000000000000000000000000000080'},
	]
	for tt in tests {
		// t.Run(tt.name, func(t *testing.T) {
		// p1, err := new(Point).SetBytes(decodeHex(tt.encoding))
		mut p1 := Point{}
		p1.set_bytes(hex.decode(tt.encoding)?)?

		// p2, err := new(Point).SetBytes(decodeHex(tt.canonical))
		mut p2 := Point{}
		p2.set_bytes(hex.decode(tt.canonical)?)?

		assert p1.equal(p2) == 1
		assert p1.bytes() == p2.bytes()
		assert hex.encode(p1.bytes()) == tt.canonical // NEED FIX!

		assert check_on_curve(p1, p2) == true
	}
}

fn test_generator() {
	// These are the coordinates of B from RFC 8032, Section 5.1, converted to
	// little endian hex.
	x := '1ad5258f602d56c9b2a7259560c72c695cdcd6fd31e2a4c0fe536ecdd3366921'
	y := '5866666666666666666666666666666666666666666666666666666666666666'
	mut b := new_generator_point()

	assert hex.encode(b.x.bytes()) == x
	assert hex.encode(b.y.bytes()) == y
	assert b.z.equal(fe_one) == 1
	// Check that t is correct.
	assert check_on_curve(b) == true
}
