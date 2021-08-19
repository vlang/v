interface Box {
	transform(input int) int
}

struct Test<T> {
	data T
	salt int
}

fn (t Test<T>) transform(input int) int {
	return input + t.salt
}

fn box_transform(b Box) int {
	return b.transform(100)
}

fn test_generic_struct_with_non_generic_interface() {
	ret := box_transform(Test<string>{
		data: 'hello'
		salt: 6
	})
	println(ret)
	assert ret == 106
}
