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

interface Named {
	name() string
}

struct NamedItem {
	label string
}

fn (item NamedItem) name() string {
	return item.label
}

fn widened_tuple_receiver[T](item T) !Named {
	m := &Matrix[T]{ data: [item] }
	q, _ := split(m)!
	return q.get()
}

fn test_generic_result_tuple_receiver_keeps_type_for_interface_return() {
	result := widened_tuple_receiver(NamedItem{ label: 'kept' })!
	assert result.name() == 'kept'
}

fn split_heterogeneous[T](m &Matrix[T]) !(&Matrix[T], int) {
	return m, 7
}

fn split_heterogeneous_last[T](m &Matrix[T]) !(int, &Matrix[T]) {
	return 7, m
}

fn heterogeneous_first[A, B](first A, second B) !B {
	_ = first
	m := &Matrix[B]{ data: [second] }
	q, _ := split_heterogeneous(m)!
	return q.get()
}

fn heterogeneous_last[A, B](first A, second B) !B {
	_ = first
	m := &Matrix[B]{ data: [second] }
	_, q := split_heterogeneous_last(m)!
	return q.get()
}

fn test_heterogeneous_result_tuple_receiver_preserves_slot_type() {
	assert heterogeneous_first(1, 'first')! == 'first'
	assert heterogeneous_last(1, 'last')! == 'last'
}

struct TupleFactory[T] {}

fn (f TupleFactory[T]) make[U]() ?U {
	return U{}
}

fn option_from_third[A, B, C](first A, second B, third C) ?C {
	_ = first
	_ = second
	_ = third
	factory := TupleFactory[A]{}
	return factory.make()?
}

fn test_generic_option_method_uses_enclosing_return_context() {
	assert option_from_third(1, true, 'value')? == ''
}

fn contextual_zero[T]() T {
	return T{}
}

fn contextual_result_zero[T](fail bool) !T {
	if fail { return error('fallback') }
	return T{}
}

fn contextual_if[A, B](flag bool, first A, second B) B {
	_ = first
	return if flag { contextual_zero() } else { second }
}

fn contextual_match[A, B](choice int, first A, second B) B {
	_ = first
	return match choice {
		0 { contextual_zero() }
		1 {
			if true { contextual_zero() } else { second }
		}
		else { second }
	}
}

fn contextual_folded_if[A, B](first A, second B) B {
	_ = first
	return if B.name == 'string' { contextual_zero() } else { second }
}

fn contextual_or[A, B](fail bool, first A, second B) B {
	_ = first
	_ = second
	return contextual_result_zero(fail) or { contextual_zero() }
}

fn test_generic_contextual_returns_reach_branch_tails_and_or_values() {
	assert contextual_if(true, 1, 'value') == ''
	assert contextual_if(false, 1, 'value') == 'value'
	assert contextual_match(0, 1, 'value') == ''
	assert contextual_match(1, 1, 'value') == ''
	assert contextual_match(2, 1, 'value') == 'value'
	assert contextual_folded_if(1, 'value') == ''
	assert contextual_or(false, 1, 'value') == ''
	assert contextual_or(true, 1, 'value') == ''
}

struct ContextualGuard {
mut:
	value int
}

fn contextual_rlock[A, B](shared guard ContextualGuard, first A, second B) B {
	_ = first
	_ = second
	return rlock guard {
		contextual_zero()
	}
}

fn contextual_lock[A, B](shared guard ContextualGuard, first A, second B) B {
	_ = first
	_ = second
	return lock guard {
		guard.value++
		contextual_zero()
	}
}

fn test_generic_contextual_returns_reach_lock_expression_body() {
	shared guard := ContextualGuard{}
	assert contextual_rlock(shared guard, 1, 'value') == ''
	assert contextual_lock(shared guard, 1, 'value') == ''
	rlock guard {
		assert guard.value == 1
	}
}
