@[heap]
struct Matrix[T] {
	data []T
}

fn (m &Matrix[T]) get() T { return m.data[0] }

fn split[T](m &Matrix[T]) !(&Matrix[T], &Matrix[T]) { return m, m }

fn compute[T](n T) !(T, T) {
	m := &Matrix[T]{ data: [n] }
	q, r := split(m)!
	x := q.get()
	y := r.get()
	return x, y
}

fn test_generic_tuple_receiver() {
	a, b := compute(f64(42))!
	assert a == 42
	assert b == 42
}
