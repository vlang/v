import pointee_sum

// `pointee_sum.Number` has an `int` variant: an `is` or a `match` arm that
// tests `Maybe[&int]` for its `&int` variant must take that variant, not the
// `int` of another sum type.
struct None {}

type Maybe[T] = None | T

fn (m Maybe[T]) str[T]() string {
	return if m is T {
		x := m as T
		'Some(${x})'
	} else {
		'Noth'
	}
}

fn (m Maybe[T]) or_else[T](fallback T) T {
	return match m {
		T { m }
		None { fallback }
	}
}

fn test_pointer_variant_of_a_generic_sum_type() {
	value := 123
	ptr := &value
	some := Maybe[&int](ptr)
	assert some.str() == 'Some(${ptr_str(ptr)})'
	fallback := 0
	held := some.or_else(&fallback)
	assert *held == 123
	noth := Maybe[&int](None{})
	assert noth.str() == 'Noth'
	missing := noth.or_else(&fallback)
	assert *missing == 0
	five := Maybe[int](5)
	assert five.or_else(0) == 5
}

fn test_module_sum_type_with_the_pointee_variant() {
	number := pointee_sum.Number(7)
	assert number is int
	if number is int {
		assert number == 7
	}
}
