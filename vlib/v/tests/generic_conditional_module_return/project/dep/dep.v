module dep

pub struct Box[T] {
pub:
	value  T
	marker int
}

pub type Boxes[T] = []Box[T]

// make wraps a value in this module's generic box.
pub fn make[T](value T) Box[T] {
	return Box[T]{ value: value, marker: 41 }
}

// many returns this module's generic array alias.
pub fn many[T](value T) Boxes[T] {
	return [make(value)]
}
