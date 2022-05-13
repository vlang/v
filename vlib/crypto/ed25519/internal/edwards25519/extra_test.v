module edwards25519

import os
import rand
import encoding.hex

const github_job = os.getenv('GITHUB_JOB')

fn testsuite_begin() {
	if edwards25519.github_job != '' {
		// ensure that the CI does not run flaky tests:
		rand.seed([u32(0xffff24), 0xabcd])
	}
}

// test_bytes_montgomery tests the set_bytes_with_clamping+bytes_montgomery path
// equivalence to curve25519.X25519 for basepoint scalar multiplications.
//
// Note that you can't actually implement X25519 with this package because
// there is no SetBytesMontgomery, and it would not be possible to implement
// it properly: points on the twist would get rejected, and the Scalar returned
// by set_bytes_with_clamping does not preserve its cofactor-clearing properties.
//
// Disabled curve25519 not available yet, but maybe can use own curve25519
/*
fn fn_mon(scalar [32]u8) bool {
               mut s := new_scalar().set_bytes_with_clamping(scalar[..])
               p := (&Point{}).scalar_base_mult(s)
               got := p.bytes_montgomery()
               want, _ := curve25519.X25519(scalar[..], curve25519.Basepoint)
               return bytes.equal(got, want)
       }

fn test_bytes_montgomery() {
       /* f := fn(scalar [32]u8) bool {
               s := new_scalar().set_bytes_with_clamping(scalar[..])
               p := (&Point{}).scalar_base_mult(s)
               got := p.bytes_montgomery()
               want, _ := curve25519.X25519(scalar[..], curve25519.Basepoint)
               return bytes.equal(got, want)
       } */
       if err := quick.Check(f, nil); err != nil {
               t.Error(err)
       }
}*/

fn test_bytes_montgomery_sodium() ? {
	// Generated with libsodium.js 1.0.18
	// crypto_sign_keypair().pubkey
	pubkey := '3bf918ffc2c955dc895bf145f566fb96623c1cadbe040091175764b5fde322c0'
	mut p := Point{}
	p.set_bytes(hex.decode(pubkey)?)?

	// crypto_sign_ed25519_pk_to_curve25519(pubkey)
	want := 'efc6c9d0738e9ea18d738ad4a2653631558931b0f1fde4dd58c436d19686dc28'
	got := hex.encode(p.bytes_montgomery())
	assert got == want
}

fn test_bytes_montgomery_infinity() {
	mut p := new_identity_point()
	want := '0000000000000000000000000000000000000000000000000000000000000000'
	got := hex.encode(p.bytes_montgomery())

	assert got == want
}

const (
	loworder_string = '26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85'
	loworder_bytes  = hex.decode(loworder_string) or { panic(err) }
)

fn fn_cofactor(mut data []u8) bool {
	if data.len != 64 {
		panic('data.len should be 64')
	}
	mut loworder := Point{}
	loworder.set_bytes(edwards25519.loworder_bytes) or { panic(err) }

	mut s := new_scalar()
	mut p := Point{}
	mut p8 := Point{}
	s.set_uniform_bytes(data) or { panic(err) }
	p.scalar_base_mult(mut s)
	p8.mult_by_cofactor(p)

	assert check_on_curve(p8) == true

	// 8 * p == (8 * s) * B
	mut sc := Scalar{
		s: [32]u8{}
	}
	sc.s[0] = u8(0x08)
	s.multiply(s, sc)
	mut pp := Point{}
	pp.scalar_base_mult(mut s)
	if p8.equal(pp) != 1 {
		return false
	}

	// 8 * p == 8 * (loworder + p)
	pp.add(p, loworder)
	pp.mult_by_cofactor(pp)
	if p8.equal(pp) != 1 {
		return false
	}

	// 8 * p == p + p + p + p + p + p + p + p
	pp.set(new_identity_point())
	for i := 0; i < 8; i++ {
		pp.add(pp, p)
	}
	return p8.equal(pp) == 1
}

fn test_mult_by_cofactor() ? {
	mut loworder := Point{}
	mut data := rand.bytes(64)?

	assert fn_cofactor(mut data) == true
}

fn invert_works(mut xinv Scalar, x NotZeroScalar) bool {
	xinv.invert(x)
	mut check := Scalar{}
	check.multiply(x, xinv)
	return check == sc_one && is_reduced(xinv)
}

fn test_scalar_invert() {
	nsc := generate_notzero_scalar(5) or { panic(err) }
	mut xsc := generate_scalar(5) or { panic(err) }
	assert invert_works(mut xsc, nsc) == true

	mut zero := new_scalar()
	mut xx := new_scalar()
	xx.invert(zero)
	assert xx.equal(zero) == 1
}

fn test_multiscalarmultmatchesbasemult() {
	for i in 0 .. 6 {
		x := generate_scalar(100) or { panic(err) }
		y := generate_scalar(5) or { panic(err) }
		z := generate_scalar(2) or { panic(err) }
		assert multiscalarmultmatchesbasemult(x, y, z) == true
	}
}

fn multiscalarmultmatchesbasemult(xx Scalar, yy Scalar, zz Scalar) bool {
	mut x := xx
	mut y := yy
	mut z := zz

	mut p := Point{}
	mut q1 := Point{}
	mut q2 := Point{}
	mut q3 := Point{}
	mut check := Point{}
	mut b := new_generator_point()

	p.multi_scalar_mult([x, y, z], [b, b, b])

	q1.scalar_base_mult(mut x)
	q2.scalar_base_mult(mut y)
	q3.scalar_base_mult(mut z)
	check.add(q1, q2)
	check.add(check, q3)

	check_on_curve(p, check, q1, q2, q3)
	return p.equal(check) == 1
}

fn vartime_multiscala_rmultmatches_basemult(xx Scalar, yy Scalar, zz Scalar) bool {
	mut x := xx
	mut y := yy
	mut z := zz
	mut p := Point{}
	mut q1 := Point{}
	mut q2 := Point{}
	mut q3 := Point{}
	mut check := Point{}
	mut b := new_generator_point()

	p.vartime_multiscalar_mult([x, y, z], [b, b, b])

	q1.scalar_base_mult(mut x)
	q2.scalar_base_mult(mut y)
	q3.scalar_base_mult(mut z)
	check.add(q1, q2)
	check.add(check, q3)

	check_on_curve(p, check, q1, q2, q3)
	return p.equal(check) == 1
}

fn test_vartimemultiscalarmultmatchesbasemult() {
	for i in 0 .. 5 {
		x := generate_scalar(100) or { panic(err) }
		y := generate_scalar(5) or { panic(err) }
		z := generate_scalar(2) or { panic(err) }
		assert vartime_multiscala_rmultmatches_basemult(x, y, z) == true
	}
}
