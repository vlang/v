module edwards25519

import os
import rand
import encoding.hex
import math.big

const github_job = os.getenv('GITHUB_JOB')

fn testsuite_begin() {
	if edwards25519.github_job != '' {
		// ensure that the CI does not run flaky tests:
		rand.seed([u32(0xffff24), 0xabcd])
	}
}

fn test_scalar_equal() {
	assert sc_one.equal(sc_minus_one) != 1

	assert sc_minus_one.equal(sc_minus_one) != 0
}

fn test_scalar_non_adjacent_form() {
	mut s := Scalar{
		s: [byte(0x1a), 0x0e, 0x97, 0x8a, 0x90, 0xf6, 0x62, 0x2d, 0x37, 0x47, 0x02, 0x3f, 0x8a,
			0xd8, 0x26, 0x4d, 0xa7, 0x58, 0xaa, 0x1b, 0x88, 0xe0, 0x40, 0xd1, 0x58, 0x9e, 0x7b,
			0x7f, 0x23, 0x76, 0xef, 0x09]!
	}
	expected_naf := [i8(0), 13, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, -9, 0, 0, 0, 0, -11,
		0, 0, 0, 0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 9, 0, 0, 0, 0, -5, 0, 0, 0, 0, 0, 0, 3, 0, 0,
		0, 0, 11, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, -9, 0, 0, 0, 0, 0, -3, 0, 0, 0, 0, 9, 0, 0, 0,
		0, 0, 1, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 9, 0, 0, 0, 0, -15, 0, 0, 0, 0, -7, 0, 0,
		0, 0, -9, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, -3, 0, 0, 0, 0, -11, 0, 0, 0,
		0, -7, 0, 0, 0, 0, -13, 0, 0, 0, 0, 11, 0, 0, 0, 0, -9, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
		-15, 0, 0, 0, 0, 1, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 13, 0, 0,
		0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, -9, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0,
		0, 0, 0, 0, 7, 0, 0, 0, 0, 0, -15, 0, 0, 0, 0, 0, 15, 0, 0, 0, 0, 15, 0, 0, 0, 0, 15, 0,
		0, 0, 0, 0, 1, 0, 0, 0, 0]

	snaf := s.non_adjacent_form(5)
	for i := 0; i < 256; i++ {
		assert expected_naf[i] == snaf[i]
	}
}

fn addlike_subneg(x Scalar, y Scalar) bool {
	// Compute t1 = x - y
	mut t1 := Scalar{}
	t1.subtract(x, y)

	// Compute t2 = -y + x
	mut t2 := Scalar{}
	t2.negate(y)
	t2.add(t2, x)

	return t1 == t2 && is_reduced(t1)
}

fn test_scalar_add_like_subneg() {
	for i in 0 .. 15 {
		x := generate_scalar(1000) or { panic(err.msg) }
		y := generate_scalar(1000) or { panic(err.msg) }
		assert addlike_subneg(x, y) == true
	}
}

fn fg(sc Scalar) bool {
	return is_reduced(sc)
}

fn test_scalar_generate() ? {
	for i in 0 .. 15 {
		sc := generate_scalar(1000) or { panic(err.msg) }

		assert fg(sc) == true
	}
}

//
fn test_scalar_set_canonical_bytes() ? {
	for i in 0 .. 10 {
		mut buf := rand.bytes(32) or { panic(err.msg) }
		mut sc := generate_scalar(1000) or { panic(err.msg) }
		buf[buf.len - 1] &= (1 << 4) - 1
		sc = sc.set_canonical_bytes(buf) or { panic(err.msg) }

		assert buf[..] == sc.bytes()
		assert is_reduced(sc)
	}
}

fn test_scalar_set_canonical_bytes_round_trip() ? {
	for i in 0 .. 10 {
		mut sc1 := generate_scalar(2) ?
		mut sc2 := generate_scalar(6) ?
		sc2.set_canonical_bytes(sc1.bytes()) or { panic(err.msg) }

		assert sc1 == sc2
	}
}

const (
	sc_error = Scalar{
		s: [32]byte{init: (byte(-1))}
	}
)

fn test_scalar_set_canonical_bytes_on_noncanonical_value() ? {
	mut b := sc_minus_one.s
	b[31] += 1

	mut s := sc_one
	out := s.set_canonical_bytes(b[..]) or { edwards25519.sc_error } // set_canonical_bytes shouldn't worked on a non-canonical value"
	assert out == edwards25519.sc_error
	assert s == sc_one
}

fn test_scalar_set_uniform_bytes() ? {
	// mod, _ := new(big.Integer).SetString("27742317777372353535851937790883648493", 10)
	mut mod := big.integer_from_string('27742317777372353535851937790883648493') ?
	// mod.Add(mod, new(big.Integer).Lsh(big.NewInt(1), 252))
	mod = mod + big.integer_from_i64(1).lshift(252)

	mut sc := generate_scalar(100) ?
	inp := rand.bytes(64) ?

	sc.set_uniform_bytes(inp[..]) ?
	assert is_reduced(sc) == true

	scbig := bigint_from_le_bytes(sc.s[..])
	inbig := bigint_from_le_bytes(inp)
	// return inbig.Mod(inbig, mod).Cmp(scbig) == 0
	_, m := inbig.div_mod(mod)
	assert m.abs_cmp(scbig) == 0 // NEED FIX
}

fn bigint_from_le_bytes(b []byte) big.Integer {
	mut bc := b.clone()
	buf := swap_endianness(mut bc) // WITHOUT THIS, some test would fail
	bg := big.integer_from_bytes(buf)
	return bg
}

fn test_scalar_set_bytes_with_clamping() {
	// Generated with libsodium.js 1.0.18 crypto_scalarmult_ed25519_base.
	/*
	random := "633d368491364dc9cd4c1bf891b1d59460face1644813240a313e61f2c88216e"
	s, _ := new(Scalar).SetBytesWithClamping(decodeHex(random))
	p := new(Point).ScalarBaseMult(s)
	want := "1d87a9026fd0126a5736fe1628c95dd419172b5b618457e041c9c861b2494a94"
	if got := hex.EncodeToString(p.Bytes()); got != want {
		t.Errorf("random: got %q, want %q", got, want)
	}*/
	random := '633d368491364dc9cd4c1bf891b1d59460face1644813240a313e61f2c88216e'
	random_bytes := hex.decode(random) or { panic(err.msg) }

	mut s0 := Scalar{}
	s0.set_bytes_with_clamping(random_bytes) or { panic(err.msg) }

	mut p0 := Point{}
	p0.scalar_base_mult(mut s0)

	want0 := '1d87a9026fd0126a5736fe1628c95dd419172b5b618457e041c9c861b2494a94'
	got0 := hex.encode(p0.bytes())

	assert got0 == want0

	zero := '0000000000000000000000000000000000000000000000000000000000000000'
	mut s1 := Scalar{}
	zero_bytes := hex.decode(zero) or { panic(err.msg) }
	s1.set_bytes_with_clamping(zero_bytes) or { panic(err.msg) }
	mut p1 := Point{}
	p1.scalar_base_mult(mut s1)

	want1 := '693e47972caf527c7883ad1b39822f026f47db2ab0e1919955b8993aa04411d1'
	got1 := hex.encode(p1.bytes())
	assert want1 == got1

	one := 'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
	mut s2 := Scalar{}
	mut one_bytes := hex.decode(one) or { panic(err.msg) }
	s2.set_bytes_with_clamping(one_bytes) or { panic(err.msg) }
	mut p2 := Point{}
	p2.scalar_base_mult(mut s2)

	want2 := '12e9a68b73fd5aacdbcaf3e88c46fea6ebedb1aa84eed1842f07f8edab65e3a7'
	got2 := hex.encode(p2.bytes())

	assert want2 == got2
}

fn test_scalar_multiply_distributes_over_add() ? {
	x := generate_scalar(100) ?
	y := generate_scalar(100) ?
	z := generate_scalar(100) ?

	// Compute t1 = (x+y)*z
	mut t1 := Scalar{}
	t1.add(x, y)
	t1.multiply(t1, z)

	// Compute t2 = x*z + y*z
	mut t2 := Scalar{}
	mut t3 := Scalar{}
	t2.multiply(x, z)
	t3.multiply(y, z)
	t2.add(t2, t3)

	assert t1 == t2 && is_reduced(t1) && is_reduced(t3)
}
