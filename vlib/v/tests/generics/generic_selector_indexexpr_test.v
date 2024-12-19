module main

pub struct Vector[T] {
	n      int
	values []T
}

pub fn Vector.new[T](v []T) Vector[T] {
	return Vector[T]{
		n:      v.len
		values: v
	}
}

pub fn (v Vector[T]) str() string {
	return v.values.str()
}

pub fn (v Vector[T]) + (u Vector[T]) Vector[T] {
	assert v.n == u.n
	return Vector[T]{
		n:      v.n
		values: []T{len: v.n, init: T(v.values[index] + u.values[index])}
	}
}

pub fn (v Vector[T]) dot(u Vector[T]) T {
	assert v.n == u.n
	mut sum := T(0)
	for i in 0 .. v.n {
		sum += v.values[i] * u.values[i]
	}
	return sum
}

fn test_main() {
	v := Vector.new[f64]([1.0, 2, 3])
	vdotv := v.dot(v)
	assert vdotv == 14.0

	u := Vector.new[int]([1, 2, 3])
	udotu := u.dot(u)
	assert udotu == 14
}
