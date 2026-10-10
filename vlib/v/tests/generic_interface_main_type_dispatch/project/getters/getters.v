module getters

pub struct Item {
	label string
}

pub interface Getter[T] {
	get() T
}

// read returns the concrete value provided by a generic interface.
pub fn read[T](getter Getter[T]) T {
	return getter.get()
}

// read_array returns an array through its specialized interface.
pub fn read_array[T](getter Getter[[]T]) []T {
	return getter.get()
}

// read_scalar returns a non-colliding value through its specialized interface.
pub fn read_scalar[T](getter Getter[T]) T {
	return getter.get()
}
