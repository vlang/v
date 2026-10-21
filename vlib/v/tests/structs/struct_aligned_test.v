@[aligned: 512]
struct MyStruct {
	a int
}

fn test_struct_aligned() {
	x := u64(voidptr(&MyStruct{}))
	assert x % 512 == 0

	y := MyStruct{}
	ptr := u64(voidptr(&y))
	assert ptr % 512 == 0
}

fn aligned_struct_value() MyStruct {
	return MyStruct{ a: 42 }
}

fn aligned_struct_pointer() &MyStruct {
	value := aligned_struct_value()
	return &value
}

fn test_promoted_struct_preserves_alignment() {
	value := aligned_struct_pointer()
	assert value.a == 42
	assert usize(voidptr(value)) % 512 == 0
}
