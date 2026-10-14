module aliases

pub struct Item {
	label string
}

pub type Values[T] = []T

// values returns an array through a generic alias.
pub fn values[T](items []T) Values[T] {
	return Values[T](items)
}
