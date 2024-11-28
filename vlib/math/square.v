module math

// square returns the square of the argument x, i.e. x * x
@[inline]
pub fn square[T](x T) T {
	return x * x
}

// cube returns the cube of the argument x, i.e. x * x * x
@[inline]
pub fn cube[T](x T) T {
	return x * x * x
}
