module main

struct Pair[A, B] {
	first  A
	second B
}

fn (p Pair[A, B]) str() string {
	return 'Pair(' + p.first.str() + ', ' + p.second.str() + ')'
}

struct Gen[T] {
	run fn () !T = unsafe { nil }
}

type SmartValue = int | string

fn (g Gen[T]) draw() !T {
	v := g.run()!
	return v
}

fn int_gen() Gen[int] {
	return Gen[int]{
		run: fn () !int {
			return 1
		}
	}
}

fn combine[A, B](a Gen[A], b Gen[B]) Gen[Pair[A, B]] {
	return Gen[Pair[A, B]]{
		run: fn [a, b] [A, B]() !Pair[A, B] {
			first := a.draw()!
			second := b.draw()!
			return Pair[A, B]{
				first:  first
				second: second
			}
		}
	}
}

fn pair_gen() Gen[Pair[int, int]] {
	return combine[int, int](int_gen(), int_gen())
}

fn smart_value_gen() Gen[SmartValue] {
	return Gen[SmartValue]{
		run: fn () !SmartValue {
			return SmartValue(7)
		}
	}
}

fn delete_pass_simple[T](g Gen[T]) []string {
	mut trail := []string{}
	if v := g.draw() {
		trail << '${v}'
	}
	return trail
}

fn delete_pass_int_width[T](g Gen[T]) []string {
	mut trail := []string{}
	if v := g.draw() {
		trail << '${v:4d}'
	}
	return trail
}

fn delete_pass_string_width[T](g Gen[T]) []string {
	mut trail := []string{}
	if v := g.draw() {
		trail << '${v:12s}'
	}
	return trail
}

fn delete_pass_smartcast_width[T](g Gen[T]) []string {
	mut trail := []string{}
	if v := g.draw() {
		if v is int {
			trail << '${v:4d}'
		}
	}
	return trail
}

fn test_generic_if_guard_simple_interpolation_int_then_pair() {
	assert delete_pass_simple[int](int_gen()) == ['1']
	assert delete_pass_simple[Pair[int, int]](pair_gen()) == ['Pair(1, 1)']
}

fn test_generic_if_guard_simple_interpolation_pair_then_int() {
	assert delete_pass_simple[Pair[int, int]](pair_gen()) == ['Pair(1, 1)']
	assert delete_pass_simple[int](int_gen()) == ['1']
}

fn test_generic_if_guard_full_interpolation_int_then_pair() {
	assert delete_pass_int_width[int](int_gen()) == ['   1']
	assert delete_pass_string_width[Pair[int, int]](pair_gen()) == ['  Pair(1, 1)']
}

fn test_generic_if_guard_full_interpolation_pair_then_int() {
	assert delete_pass_string_width[Pair[int, int]](pair_gen()) == ['  Pair(1, 1)']
	assert delete_pass_int_width[int](int_gen()) == ['   1']
}

fn test_generic_if_guard_smartcast_interpolation_keeps_smartcast_type() {
	assert delete_pass_smartcast_width[SmartValue](smart_value_gen()) == ['   7']
}
