module main

struct Pair[A, B] {
	a A
	b B
}

struct Container[T] {
	value T
}

pub fn (c Container[T]) get() !T {
	return c.value
}

fn make_pair[A, B](a A, b B) Container[Pair[A, B]] {
	return Container[Pair[A, B]]{
		value: Pair[A, B]{
			a: a
			b: b
		}
	}
}

fn make_list[T](items []T) Container[[]T] {
	return Container[[]T]{
		value: items
	}
}

fn test_pair_container_get_returns_pair() {
	c := make_pair[int, int](7, 11)
	p := c.get() or {
		assert false, 'get() on Container[Pair[int, int]] errored: ${err}'
		return
	}
	assert p.a == 7, 'expected p.a == 7, got ${p.a}'
	assert p.b == 11, 'expected p.b == 11, got ${p.b}'
}

fn test_list_container_get_returns_slice() {
	c := make_list[int]([1, 2, 3])
	lst := c.get() or {
		assert false, 'get() on Container[[]int] errored: ${err}'
		return
	}
	assert lst.len == 3, 'expected length 3, got ${lst.len}'
	assert lst[0] == 1
	assert lst[1] == 2
	assert lst[2] == 3
}

fn test_sibling_factories_return_independent_shapes() {
	pc := make_pair[string, int]('hello', 42)
	lc := make_list[string](['a', 'b'])

	p := pc.get() or {
		assert false, 'pair get failed: ${err}'
		return
	}
	assert p.a == 'hello'
	assert p.b == 42

	lst := lc.get() or {
		assert false, 'list get failed: ${err}'
		return
	}
	assert lst.len == 2
	assert lst[0] == 'a'
	assert lst[1] == 'b'
}
