pub struct Vector {
	vec []f64
}

pub fn (a Vector) + (b Vector) Vector {
	size := a.vec.len
	if size != b.vec.len {
		panic('unequal sizes')
	}
	mut c := []f64{len: size}
	for i in 0 .. size {
		c[i] = a.vec[i] + b.vec[i]
	}
	return Vector{
		vec: c
	}
}

type Vec = Vector

fn test_alias_operator_overloading() {
	a := Vec{
		vec: [0.1, 0.2]
	}
	b := Vec{
		vec: [0.3, 0.2]
	}
	c := a + b
	println(c)
	assert c.vec == [0.4, 0.4]
}
