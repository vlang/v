module genericmodule

pub struct Box[T] {
pub:
	value T
}

// box creates a generic box.
pub fn box[T](value T) &Box[T] {
	return &Box[T]{ value: value }
}

// get returns the boxed value.
pub fn (b &Box[T]) get[T]() T {
	return b.value
}

// same compares two boxed values.
pub fn (b &Box[T]) same[T](other &Box[T]) bool {
	return b.value == other.value
}

// copy returns a new box through a result.
pub fn (b &Box[T]) copy[T]() !&Box[T] {
	return box(b.value)
}

// read_box infers its type parameter from an imported box.
pub fn read_box[T](b &Box[T]) T {
	return b.value
}

// result_box returns a box through a result.
pub fn result_box[T](value T) !&Box[T] {
	return box(value)
}

// plus adds an integer to the boxed value.
pub fn (b &Box[T]) plus[T](amount int) T {
	return b.value + T(amount)
}

// optional_box returns a box through an option.
pub fn optional_box[T](value T) ?&Box[T] {
	return box(value)
}
