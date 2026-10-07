module fn_return_cell_mod

pub struct Cell[T] {
pub mut:
	value T
}

// empty reports whether a new callback cell contains nil.
pub fn empty[T]() bool {
	cell := Cell[fn () T]{}
	return cell.value == unsafe { nil }
}

// invoke calls the callback stored in a generic cell.
pub fn invoke[T](callback fn () T) T {
	cell := Cell[fn () T]{ value: callback }
	return cell.value()
}

// apply calls the stored callback with the supplied value.
pub fn apply[T](callback fn (T) int, value T) int {
	cell := Cell[fn (T) int]{ value: callback }
	return cell.value(value)
}
