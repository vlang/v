module main

struct Test {
}

fn (mut t Test) decode_array[X](x []X) []X {
	println('second: x.typename = ${typeof(x).name}')
	return x
}

fn (mut t Test) decode[T]() T {
	return t.decode_array(T{})
}

fn test_main() {
	mut x := Test{}
	x.decode[[]u8]()
	x.decode[[]int]()
}
