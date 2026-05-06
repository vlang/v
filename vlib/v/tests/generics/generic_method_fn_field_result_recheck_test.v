// Reproducer for issue 27032: a generic method forwarding to a generic function
// field used to generate stale Result C types after monomorphization.

struct Pair[A, B] {
pub:
	first  A
	second B
}

struct Box[T] {
	run fn (int) !T = unsafe { nil }
}

fn (b Box[T]) call(x int) !T {
	return b.run(x)!
}

fn combine[A, B](a Box[A], b Box[B]) Box[Pair[A, B]] {
	return Box[Pair[A, B]]{
		run: fn [a, b] [A, B](x int) !Pair[A, B] {
			first := a.call(x)!
			second := b.call(x)!
			return Pair[A, B]{
				first:  first
				second: second
			}
		}
	}
}

fn int_box() Box[int] {
	return Box[int]{
		run: fn (x int) !int {
			return x + 1
		}
	}
}

fn use[T](b Box[T]) !T {
	return b.call(0)!
}

fn test_generic_method_fn_field_result_recheck() ! {
	b := combine[int, int](int_box(), int_box())
	v := use[Pair[int, int]](b)!
	assert v.first == 1
	assert v.second == 1
}
