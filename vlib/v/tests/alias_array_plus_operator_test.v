type Vector = []f64

[heap]
struct HeapArray {
mut:
	data []f64
}

fn new_vector(x f64, y f64, z f64, w f64) &Vector {
	a := HeapArray{
		data: [x, y, z, w]
	}
	return &Vector(&a.data)
}

pub fn (a &Vector) + (b &Vector) &Vector {
	mut res := HeapArray{
		data: []f64{len: a.len}
	}
	for i := 0; i < a.len; i++ {
		res.data[i] = (*a)[i] + (*b)[i]
	}
	return &Vector(&res.data)
}

fn test_alias_array_plus_overload() {
	mut a := new_vector(12, 4.5, 6.7, 6)
	b := new_vector(12, 4.5, 6.7, 6)
	dump(a)
	dump(b)
	c := a + b
	dump(c)
	assert *c == Vector([f64(24), 9, 13.4, 12])
	a = a + b
	assert a.str() == c.str()
	dump(a)
}
