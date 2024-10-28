struct St[T] {
mut:
	a T
}

fn (mut s St[T]) f(e T) {
	if e != T{} {
		s.a = e
	}
}

fn test_main() {
	mut s := St[int]{}
	s.f(1)
	assert s.a == 1
}
