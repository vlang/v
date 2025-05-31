module main

@[heap]
struct Wrapper {
	a &int = unsafe { nil }
}

fn test[T](a &T) Wrapper {
	$if T is int {
		return Wrapper{a}
	}
	return Wrapper{}
}

fn test_main() {
	a := 123
	assert test(a).a == &a
}
