module gates

import math

struct Gate[T] {
	value T
}

fn make_gate[T](value T) &Gate[T] { return &Gate[T]{ value: value } }

fn (g &Gate[T]) backward() f64 { return math.cos(f64(g.value)) }

// apply evaluates a method reached through a generic factory result.
pub fn apply[T](value T) f64 {
	gate := make_gate[T](value)
	return gate.backward()
}
