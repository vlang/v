module shift_const_values

pub struct Box[T] {
	value T
}

pub fn (b Box[T]) calculated() T {
	return T(1) << 8
}

pub fn forwarded[T]() T {
	return Box[T]{}.calculated()
}
