module typeof_parameterized_runtime

// idx returns the reflected index of the concrete caller type.
pub fn idx[T]() int {
	return typeof[T]().idx
}

// name returns the reflected name of the concrete caller type.
pub fn name[T]() string {
	return typeof[T]().name
}
