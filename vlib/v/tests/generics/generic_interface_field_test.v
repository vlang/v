pub struct Range[T] {
pub:
	from      T
	to        T
	inclusive bool
pub mut:
	step T = T(1)
	i    T
mut:
	started bool
}

pub fn (mut r Range[T]) next[T]() ?T {
	if !r.started {
		r.started = true
		assert r.step > 0
		if r.from > r.to {
			r.step = -r.step
		}
		r.i = r.from
	}
	i := r.i
	next_i := i + r.step
	if r.inclusive {
		if r.step < 0 && i < r.to {
			return none
		}
		if r.step > 0 && i > r.to {
			return none
		}
	} else {
		if r.step < 0 && i <= r.to {
			return none
		}
		if r.step > 0 && i >= r.to {
			return none
		}
	}
	r.i = next_i
	return i
}

pub interface Iterator[T] {
mut:
	next() ?T
}

pub struct Zip[A, B] {
mut:
	a Iterator[A]
	b Iterator[B]
}

pub struct Pair[A, B] {
	a A
	b B
}

pub fn (mut z Zip[A, B]) next[A, B]() ?Pair[A, B] {
	a := z.a.next()?
	b := z.b.next()?
	return Pair[A, B]{a, b}
}

fn test_main() {
	mut r1 := Range{
		from: 5
		to:   10
	}
	_ := Iterator[int](r1)

	mut r2 := Range{
		from: 10
		to:   5
	}
	_ := Iterator[int](r2)

	_ := Zip[int, int]{}
}
