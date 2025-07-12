@[aligned: 64]
struct MyStruct {
	a int
}

fn test_struct_aligned() {
	x := u64(voidptr(&MyStruct{}))
	assert x % 64 == 0

	y := MyStruct{}
	ptr := u64(voidptr(&y))
	assert ptr % 64 == 0
}
