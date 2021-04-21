struct MyStruct<T> {
mut:
	pos    int
	buffer []&T
}

fn (mut s MyStruct<T>) add<T>(e &T) bool {
	s.buffer[0] = e
	return true
}

fn fill(mut s MyStruct<i64>) {
	s.add(&i64(123))
}

fn test_generics_call_with_reference_arg() {
	mut s := MyStruct<i64>{
		pos: 1
		buffer: []&i64{len: 2}
	}
	fill(mut s)
	println(s.pos)
	assert s.pos == 1
	println(s.buffer.len)
	assert s.buffer.len == 2
}
