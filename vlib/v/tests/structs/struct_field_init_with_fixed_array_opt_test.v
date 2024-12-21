type Arr = [4]u8

struct Foo {
	bar int
	baz ?Arr
}

fn test_struct_field_init_with_fixed_array_opt() {
	foo1 := Foo{
		bar: 1
		baz: Arr([u8(5), 4, 3, 2]!)
	}
	println(foo1)
	assert foo1.baz as Arr == [u8(5), 4, 3, 2]!

	foo11 := Foo{
		bar: 1
		baz: ?Arr([u8(5), 4, 3, 2]!)
	}
	println(foo11)
	assert foo11.baz as Arr == [u8(5), 4, 3, 2]!

	foo12 := Foo{
		bar: 1
		baz: [u8(5), 4, 3, 2]!
	}
	println(foo12)
	assert foo12.baz as Arr == [u8(5), 4, 3, 2]!

	foo2 := Foo{
		bar: 1
		baz: ?Arr(none)
	}
	println(foo2)
	assert foo2.bar == 1
	assert foo2.baz == none

	foo21 := Foo{
		bar: 1
		baz: none
	}
	println(foo21)
	assert foo21.bar == 1
	assert foo21.baz == none

	arr1 := Arr([u8(5), 4, 3, 2]!)
	foo3 := Foo{
		bar: 1
		baz: arr1
	}
	println(foo3)
	assert foo3.bar == 1
	assert foo3.baz as Arr == [u8(5), 4, 3, 2]!

	arr2 := ?Arr([u8(5), 4, 3, 2]!)
	foo31 := Foo{
		bar: 1
		baz: arr2
	}
	println(foo31)
	assert foo31.bar == 1
	assert foo31.baz as Arr == [u8(5), 4, 3, 2]!

	arr3 := [u8(5), 4, 3, 2]!
	foo32 := Foo{
		bar: 1
		baz: arr3
	}
	println(foo32)
	assert foo32.bar == 1
	assert foo32.baz as Arr == [u8(5), 4, 3, 2]!
}
