struct Foo[T] {
	a T
}

fn r[T]() Foo[T] {
	return Foo[T]{}
}

fn t[T](v T) !Foo[T] {
	return match typeof(v).name {
		'string' {
			r[T]()
		}
		else {
			r[T]()
		}
	}
}

fn test_main() {
	t(1)!
	t('')!
	assert true
}
