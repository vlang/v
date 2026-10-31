import gg.m4

// A vector that is not on any axis, so a sign or an ordering mistake shows up
// as a wrong value rather than as a value that happens to be right.
const sample_v = m4.Vec4{ e: [f32(2), -3, 4, 5]! }

// round-trips through two trig calls, so compare with a tolerance rather than exactly
const eps = f32(1e-2)

fn close(a f32, b f32) bool {
	return f32_abs(a - b) < eps
}

fn test_deg_and_rad_round_trip() {
	for d in [f32(0), 45, 90, 180, 270, 360, -90] {
		assert close(m4.deg(m4.rad(d)), d)
	}
	assert m4.rad(0) == 0.0
	assert m4.deg(0) == 0.0
	// a half turn is twice a quarter turn
	assert close(m4.rad(180), m4.rad(90) * 2)
	// deg is the inverse of rad, not of itself
	assert close(m4.deg(m4.rad(1)), 1)
	assert close(m4.rad(m4.deg(1)), 1)
}

fn test_vector_constructors() {
	assert m4.zero_v4().sum() == 0.0
	assert m4.one_v4().sum() == 4.0
	assert m4.set_v4(3).sum() == 12.0
	// blank_v4 is the homogeneous "position" seed: only w is set
	assert m4.blank_v4().sum() == 1.0
	assert m4.blank_v4().mod() == 1.0
	assert m4.blank_v4().mod3() == 0.0
	assert m4.set_v4(3.5) == m4.Vec4{ e: [f32(3.5), 3.5, 3.5, 3.5]! }
	assert m4.vec3(1, 2, 3) == m4.Vec4{ e: [f32(1), 2, 3, 1]! }
	assert m4.vec4(1, 2, 3, 4) == m4.Vec4{ e: [f32(1), 2, 3, 4]! }
}

fn test_matrix_constructors_fill_every_slot() {
	assert m4.zero_m4().sum_all() == 0.0
	assert m4.unit_m4().sum_all() == 4.0
	assert m4.set_m4(2).sum_all() == 32.0
	// the diagonal of the unit matrix is all ones, and nothing else is
	for i in 0 .. 16 {
		want := if i % 5 == 0 { f32(1) } else { f32(0) }
		assert m4.unit_m4().get_e(i) == want
	}
	// a matrix and its negation cancel out
	assert m4.add(m4.set_m4(1.5), m4.set_m4(-1.5)).is_equal(m4.zero_m4())
}

fn test_perspective_scales_x_by_the_aspect_ratio() {
	ar := f32(1.5)
	p := m4.perspective(60, ar, 0.01, 10.0)
	// x is divided by the aspect ratio while y is left alone, so scaling x by
	// the aspect ratio recovers y exactly
	assert close(p.get_e(0) * ar, p.get_e(5))
	// the w row supplies the -1 that makes the division a perspective divide
	assert p.get_e(11) == -1.0
	assert p.get_e(15) == 0.0
	// nothing off the diagonal contributes except the z row
	for i in [1, 2, 3, 4, 6, 7, 8, 9, 12, 13] {
		assert p.get_e(i) == 0.0
	}
	// doubling the field of view shrinks the vertical scale to a third of it,
	// because tan(60)/tan(30) is exactly 3
	wide := m4.perspective(120, ar, 0.01, 10.0)
	assert close(p.get_e(5) / wide.get_e(5), 3.0)
}

fn test_look_at_builds_an_orthonormal_frame() {
	eye := m4.vec4(0, 1.5, 6, 1)
	la := m4.look_at(eye, m4.vec3(0, 0, 0), m4.vec3(0, 1, 0))
	sx := m4.Vec4{ e: [la.get_e(0), la.get_e(4), la.get_e(8), 0]! }
	sy := m4.Vec4{ e: [la.get_e(1), la.get_e(5), la.get_e(9), 0]! }
	sz := m4.Vec4{ e: [la.get_e(2), la.get_e(6), la.get_e(10), 0]! }
	for axis in [sx, sy, sz] {
		assert close(axis.mod3(), 1.0)
	}
	assert close(sx * sy, 0.0)
	assert close(sx * sz, 0.0)
	assert close(sy * sz, 0.0)
	// the first three entries of the last row are a translation, so they are 0
	assert la.get_e(3) == 0.0
	assert la.get_e(7) == 0.0
	assert la.get_e(11) == 0.0
	assert la.get_e(15) == 1.0
	// the translation is -R*eye, one axis at a time. Column 2 holds -f, not f,
	// so its row-3 entry carries the opposite sign to the other two.
	assert close(la.get_e(12), -(sx * eye))
	assert close(la.get_e(13), -(sy * eye))
	assert close(la.get_e(14), -(sz * eye))
}

fn test_calc_tr_matrices_matches_its_composition() {
	w := f32(800)
	h := f32(600)
	rx := f32(30)
	ry := f32(45)
	in_scale := f32(1.0)

	proj := m4.perspective(60, w / h, 0.01, 10.0)
	view := m4.look_at(m4.Vec4{ e: [f32(0.0), 1.5, 6, 0]! }, m4.Vec4{ e: [f32(0), 0, 0, 0]! },
		m4.Vec4{ e: [f32(0), 1.0, 0, 0]! })
	rxm := m4.rotate(m4.rad(rx), m4.Vec4{ e: [f32(1), 0, 0, 0]! })
	rym := m4.rotate(m4.rad(ry), m4.Vec4{ e: [f32(0), 1, 0, 0]! })
	scale_m := m4.scale(m4.Vec4{ e: [in_scale, in_scale, in_scale, 1]! })
	recomposed := (scale_m * (rym * rxm)) * (view * proj)

	got := m4.calc_tr_matrices(w, h, rx, ry, in_scale)
	assert got.is_equal(recomposed)
	// the projection only depends on the aspect ratio, not on the pixel size
	assert m4.calc_tr_matrices(640, 480, rx, ry, in_scale).is_equal(got)
	// ... but it does depend on the rotation
	assert !m4.calc_tr_matrices(w, h, 90, ry, in_scale).is_equal(got)
}

fn test_matrix_getters_and_setters_round_trip() {
	mut m := m4.zero_m4()
	for i in 0 .. 16 {
		m.set_e(i, f32(i))
		assert m.get_e(i) == f32(i)
	}
	mut n := m4.unit_m4()
	unsafe {
		for row in 0 .. 4 {
			for col in 0 .. 4 {
				assert n.get_f(col, row) == n.get_row(row).e[col]
				assert n.get_f(col, row) == n.get_col(col).e[row]
			}
		}
	}
	// set_trace and get_trace see the same four diagonal slots
	mut t := m4.zero_m4()
	t.set_trace(m4.vec4(1, 2, 3, 4))
	assert t.get_trace() == m4.vec4(1, 2, 3, 4)
	// set_f32 fills every slot with the same value
	mut f := m4.unit_m4()
	f.set_f32(7)
	assert f.sum_all() == 112.0
}

fn test_vector_operators_obey_their_definitions() {
	// the dot product is commutative
	assert close(sample_v * sample_v, sample_v.mod() * sample_v.mod())
	// the cross product is anti-commutative and orthogonal to both inputs.
	// compare with is_equal: scaling by -1 turns the cross product's hardcoded
	// w=0 into -0, which does not compare equal under ==.
	a := m4.vec3(1, 2, 3)
	b := m4.vec3(4, 5, 6)
	cross := a % b
	assert close(cross * a, 0.0)
	assert close(cross * b, 0.0)
	assert (a % b).is_equal((b % a).mul_scalar(-1))
	// element-wise multiplication and the dot product agree once summed
	assert sample_v.mul_vec4(sample_v).sum() == (sample_v * sample_v)
	// + and - undo each other
	assert (sample_v + sample_v) - sample_v == sample_v
}

fn test_normalize_and_invert_are_mutual() {
	v := m4.vec4(1, 2, 3, 4)
	assert close(v.normalize().mod(), 1.0)
	// a zero vector has no direction to keep, so it stays zero
	assert m4.zero_v4().normalize().is_equal(m4.zero_v4())
	assert m4.zero_v4().normalize3().is_equal(m4.zero_v4())
	// normalize3 discards w entirely
	assert close(v.normalize3().mod3(), 1.0)
	assert v.normalize3().e[3] == 0.0
	// inv() maps 0 to 0 rather than dividing by zero
	inv := m4.Vec4{ e: [f32(0), 2, 0, 4]! }.inv()
	assert inv == m4.Vec4{ e: [f32(0), 0.5, 0, 0.25]! }
	// multiplying by the inverse gives 1 wherever the element was non-zero
	w := v.mul_vec4(v.inv())
	assert w.e[0] == 1.0
	assert w.e[3] == 1.0
}

fn test_vec4_str_is_left_aligned_to_width_six() {
	assert m4.vec3(1, 2, 3).str() == '|1     ,2     ,3     ,1     |'
	assert m4.vec3(1, 2, 3).str().len == 29
	// a longer value only shortens the pad, it never widens the row
	neg := m4.vec3(-1, -2, -3).str()
	assert neg.len == 29
	assert neg.starts_with('|-1    ')
	assert m4.unit_m4().str().count('\n') == 3
}
