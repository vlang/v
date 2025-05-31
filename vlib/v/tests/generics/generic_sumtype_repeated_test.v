type Name[T] = T | int | string

fn test_main() {
	a := Name[int](123)
	assert dump(a) == Name[int](123)
	println(a)
}
