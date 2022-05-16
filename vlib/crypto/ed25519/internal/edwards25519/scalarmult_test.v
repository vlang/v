module edwards25519

const (
	dalek_scalar  = Scalar{[u8(219), 106, 114, 9, 174, 249, 155, 89, 69, 203, 201, 93, 92, 116,
		234, 187, 78, 115, 103, 172, 182, 98, 62, 103, 187, 136, 13, 100, 248, 110, 12, 4]!}
	dsc_basepoint = [u8(0xf4), 0xef, 0x7c, 0xa, 0x34, 0x55, 0x7b, 0x9f, 0x72, 0x3b, 0xb6, 0x1e,
		0xf9, 0x46, 0x9, 0x91, 0x1c, 0xb9, 0xc0, 0x6c, 0x17, 0x28, 0x2d, 0x8b, 0x43, 0x2b, 0x5,
		0x18, 0x6a, 0x54, 0x3e, 0x48]
)

fn dalek_scalar_basepoint() Point {
	mut p := Point{}
	p.set_bytes(edwards25519.dsc_basepoint) or { panic(err) }
	return p
}

fn test_scalar_mult_small_scalars() {
	mut z := Scalar{}
	mut p := Point{}
	mut b := new_generator_point()
	mut i := new_identity_point()
	p.scalar_mult(mut z, b)

	assert i.equal(p) == 1
	assert check_on_curve(p) == true

	z = Scalar{[u8(1), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0]!}
	p.scalar_mult(mut z, b)

	assert b.equal(p) == 1
	assert check_on_curve(p) == true
}

fn test_scalar_mult_vs_dalek() {
	mut p := Point{}
	mut b := new_generator_point()
	mut dsc := edwards25519.dalek_scalar
	p.scalar_mult(mut dsc, b)
	mut ds := dalek_scalar_basepoint()
	assert ds.equal(p) == 1

	assert check_on_curve(p) == true
}

fn test_scalar_base_mult_vs_dalek() {
	mut p := Point{}
	mut dsc := edwards25519.dalek_scalar
	p.scalar_base_mult(mut dsc)
	mut ds := dalek_scalar_basepoint()
	assert ds.equal(p) == 1

	assert check_on_curve(p)
}

fn test_vartime_double_basemult_vs_dalek() {
	mut p := Point{}
	mut z := Scalar{}
	b := new_generator_point()
	p.vartime_double_scalar_base_mult(edwards25519.dalek_scalar, b, z)

	mut ds := dalek_scalar_basepoint()
	assert ds.equal(p) == 1
	assert check_on_curve(p)

	p.vartime_double_scalar_base_mult(z, b, edwards25519.dalek_scalar)

	assert ds.equal(p) == 1
	assert check_on_curve(p)
}

fn test_scalar_mult_distributes_over_add() {
	mut x := generate_scalar(100) or { panic(err) }
	mut y := generate_scalar(100) or { panic(err) }
	mut z := Scalar{}

	z.add(x, y)

	mut p := Point{}
	mut q := Point{}
	mut r := Point{}
	mut check := Point{}
	mut b := new_generator_point()

	p.scalar_mult(mut x, b)
	q.scalar_mult(mut y, b)
	r.scalar_mult(mut z, b)
	check.add(p, q)

	assert check_on_curve(p, q, r, check) == true
	assert check.equal(r) == 1
}

fn test_scalarmult_non_identity_point() ? {
	// Check whether p.ScalarMult and q.ScalaBaseMult give the same,
	// when p and q are originally set to the base point.

	mut x := generate_scalar(5000)?

	mut p := Point{}
	mut q := Point{}
	mut b := new_generator_point()
	p.set(b)
	q.set(b)

	p.scalar_mult(mut x, b)
	q.scalar_base_mult(mut x)

	assert check_on_curve(p, q) == true

	assert p.equal(q) == 1
}

fn test_basepoint_table_generation() {
	// The basepoint table is 32 affineLookupTables,
	// corresponding to (16^2i)*B for table i.
	bptable := basepoint_table()
	b := new_generator_point()
	mut tmp1 := ProjectiveP1{}
	mut tmp2 := ProjectiveP2{}
	mut tmp3 := Point{}
	tmp3.set(b)
	mut table := []AffineLookupTable{len: 32}
	for i := 0; i < 32; i++ {
		// Build the table
		table[i].from_p3(tmp3)

		// Assert equality with the hardcoded one
		assert table[i] == bptable[i]

		// Set p = (16^2)*p = 256*p = 2^8*p
		tmp2.from_p3(tmp3)
		for j := 0; j < 7; j++ {
			tmp1.double(tmp2)
			tmp2.from_p1(tmp1)
		}
		tmp1.double(tmp2)
		tmp3.from_p1(tmp1)

		assert check_on_curve(tmp3) == true
	}
}

fn test_scalar_mult_matches_base_mult() {
	mut x := generate_scalar(100) or { panic(err) }
	b := new_generator_point()
	mut p := Point{}
	mut q := Point{}

	p.scalar_mult(mut x, b)
	q.scalar_base_mult(mut x)

	assert check_on_curve(p, q) == true
	assert p.equal(q) == 1
}

fn test_basepoint_naf_table_generation() {
	mut table := NafLookupTable8{}
	b := new_generator_point()

	table.from_p3(b)

	bnt := basepoint_naf_table()
	assert table == bnt
}

fn test_vartime_double_scalar_base_mult() {
	mut x := generate_scalar(100) or { panic(err) }
	mut y := generate_scalar(100) or { panic(err) }
	b := new_generator_point()

	mut p := Point{}
	mut q1 := Point{}
	mut q2 := Point{}
	mut check := Point{}

	p.vartime_double_scalar_base_mult(x, b, y)

	q1.scalar_base_mult(mut x)
	q2.scalar_base_mult(mut y)
	check.add(q1, q2)

	assert check_on_curve(p, check, q1, q2) == true
	assert p.equal(check) == 1
}
