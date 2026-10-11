module dep

pub struct Box[T] {
pub:
	value T
}

// box wraps value in this module's generic Box.
pub fn box[T](value T) Box[T] {
	return Box[T]{ value: value }
}
