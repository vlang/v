module typeof_parameterized_runtime

pub fn idx[T]() int {
	return typeof[T]().idx
}

pub fn name[T]() string {
	return typeof[T]().name
}
