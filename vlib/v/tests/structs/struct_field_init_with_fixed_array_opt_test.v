type Arr = [4]u8

struct Foo {
	bar int
	baz ?Arr
}

fn test_struct_field_init_with_fixed_array_opt() {
	f1 := Foo{
		bar: 1
		baz: Arr([u8(5), 4, 3, 2]!)
	}
	println(f1)
	assert f1.baz as Arr == [u8(5), 4, 3, 2]!

	f2 := Foo{
		bar: 1
		baz: ?Arr(none)
	}
	println(f2)
	assert f2.bar == 1
	assert f2.baz == none

	arr := Arr([u8(5), 4, 3, 2]!)
	f3 := Foo{
		bar: 1
		baz: arr
	}
	println(f3)
	assert f3.bar == 1
	assert f3.baz as Arr == [u8(5), 4, 3, 2]!
}
