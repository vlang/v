struct Flag[T] {}

fn (f Flag[T]) verify() {
	if T.name == 'int' {
		println('It is an int!')
		assert true
	}
}

fn test_generic_type_name_in_if() {
	flag := Flag[int]{}
	flag.verify()
}
