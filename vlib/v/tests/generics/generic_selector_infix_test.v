fn lerp[T](x T) T {
	return x
}

struct Foo[T] {
mut:
	value T
}

fn (mut t Foo[T]) r[T](dt f64) {
	mut value := t.value + dt
	lerp(value)
}

fn test_main() {
	mut t2 := Foo[f32]{}
	t2.r(2.1)
	assert true
}
