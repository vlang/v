type Arr = [4]u8

struct Foo {
	bar int
	baz ?Arr
}

fn test_struct_field_init_with_fixed_array_opt() {
	f := Foo{
		bar: 1
		baz: Arr([u8(5), 4, 3, 2]!)
	}
	println(f)
	assert f.baz as Arr == [u8(5), 4, 3, 2]!
}
