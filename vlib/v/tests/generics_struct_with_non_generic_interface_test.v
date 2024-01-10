interface Box {
	transform(input int) int
}

struct Test[T] {
	data T
	salt int
}

fn (t Test[T]) transform(input int) int {
	return input + t.salt
}

fn box_transform(b Box) int {
	return b.transform(100)
}

fn test_generic_struct_with_non_generic_interface() {
	ret := box_transform(Test[string]{
		data: 'hello'
		salt: 6
	})
	println(ret)
	assert ret == 106
}

fn run[T](data T) {
	t := Test[T]{
		data: data
		salt: 6
	}
	x := box_transform(t)
	println(x)
	assert x == 106
}

fn test_generic_struct_with_non_generic_interface_in_generic_fn() {
	run('foo')
}
