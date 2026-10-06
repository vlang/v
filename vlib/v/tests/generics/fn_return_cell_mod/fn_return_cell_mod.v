module fn_return_cell_mod

pub struct Cell[T] {
pub mut:
	value T
}

pub fn empty[T]() bool {
	cell := Cell[fn () T]{}
	return cell.value == unsafe { nil }
}

pub fn invoke[T](callback fn () T) T {
	cell := Cell[fn () T]{ value: callback }
	return cell.value()
}

pub fn apply[T](callback fn (T) int, value T) int {
	cell := Cell[fn (T) int]{ value: callback }
	return cell.value(value)
}
