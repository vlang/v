fn show_array_of_u8(data []u8) string {
	println(data)
	return '$data'
}

struct Foo {}

fn (f Foo) show_array_of_u8(data []u8) string {
	println(data)
	return '$data'
}

fn test_fn_with_array_of_aliases_argument() {
	a := [byte(1), 2, 3]

	s1 := show_array_of_u8(a)
	println(s1)
	assert s1 == '[1, 2, 3]'

	foo := Foo{}
	s2 := foo.show_array_of_u8(a)
	println(s2)
	assert s2 == '[1, 2, 3]'
}
