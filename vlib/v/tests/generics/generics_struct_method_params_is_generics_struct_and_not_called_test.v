struct Param[T] {
}

struct Struct[T] {
}

pub fn (s Struct[T]) method[T](p Param[T]) {
}

fn test_main() {
	_ = Struct[int]{}
	assert true
	// NOTE:
	// Do not test call Struct.method() here,
	// to test unwrap of generic struct parameters when the method is not called.
	// test results only need to can compile.
}
