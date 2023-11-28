struct Foo[T] {
	field T
}

struct MainStruct[T] {
	Foo[T]
}

fn test_main() {
	m := MainStruct[int]{}
	assert m.field == 0
}
