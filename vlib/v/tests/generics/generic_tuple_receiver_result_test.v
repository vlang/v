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

fn split_plain[T](m &Matrix[T]) (&Matrix[T], &Matrix[T]) {
	return m, m
}

fn select_second[A, B](first A, second B) B {
	_ = first
	m := &Matrix[B]{ data: [second] }
	q, _ := split_plain(m)
	return q.get()
}

fn test_generic_tuple_receiver_uses_second_generic_argument() {
	assert select_second(1, 'okay') == 'okay'
}

fn (m &Matrix[T]) pair() !(T, T) {
	return m.data[0], m.data[0]
}

fn pair_from_second[A, B](first A, second B) !(B, B) {
	_ = first
	m := &Matrix[B]{ data: [second] }
	q, _ := split_plain(m)
	return q.pair()!
}

fn test_generic_result_tuple_method_uses_second_generic_argument() {
	a, b := pair_from_second(1, 'okay')!
	assert a == 'okay'
	assert b == 'okay'
}

fn pair_local_from_first[A, B](first A, second B) !(B, B) {
	m := &Matrix[A]{ data: [first] }
	q, _ := split_plain(m)
	x, y := q.pair()!
	_ = x
	_ = y
	return second, second
}

fn test_generic_result_tuple_method_local_does_not_use_enclosing_return() {
	a, b := pair_local_from_first(1, 'okay')!
	assert a == 'okay'
	assert b == 'okay'
}
