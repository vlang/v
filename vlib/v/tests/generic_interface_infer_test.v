struct Struct<T> {
	value int
	x     T
}

fn (s Struct<T>) method() T {
	return s.x + s.x
}

interface Interface<T> {
	method() T
}

fn test_infer_generic_interface() {
	s := Struct<u32>{7, 5}
	println(s)
	i := Interface<u32>(s)
	println(i)
	assert i.method() == 10
}
