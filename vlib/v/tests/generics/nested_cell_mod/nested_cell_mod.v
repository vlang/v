module nested_cell_mod

@[heap]
pub struct Cell[T] {
pub mut:
	value T
}

pub fn name[T]() string {
	return typeof[T]().name
}
