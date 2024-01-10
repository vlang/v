interface Depends {
	depends() []Depends
}

struct Signal[T] {
}

fn (x Signal[T]) depends() []Depends {
	return []
}

struct Add[T] {
	a Signal[T]
	b Signal[T]
}

fn (a Add[T]) depends() []Depends {
	return [a.a, a.b]
}

fn test_generics_interface_decl() {
	assert true
}
