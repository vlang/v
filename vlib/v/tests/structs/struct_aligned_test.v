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
