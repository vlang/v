struct Vec3 {
mut:
	x f32
	y f32
	z f32
}

// C++ translated by c2v: `float &operator[](int i) { return (&x)[i]; }`.
fn (v &Vec3) at(i int) &f32 {
	return unsafe { &(&v.x)[i] }
}

fn test_address_of_an_element_indexed_through_a_pointer_expression() {
	mut v := Vec3{}
	p := v.at(1)
	unsafe {
		*p = 5
	}
	assert v.y == 5
	q := v.at(2)
	unsafe {
		*q += 1.5
	}
	assert v.z == 1.5
}
