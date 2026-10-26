module genericmodule

// Row has the name of a caller's type, which generic inference must not
// confuse with it.
pub struct Row {
pub:
	name string
}

@[heap]
pub struct Cell[T] {
pub:
	value T
}

struct Each[T] {
	visit fn (cell &Cell[T]) int = unsafe { nil }
}

// sum_rows calls `visit` with a cell of every item `items` returns: `T` is
// bound only by the callback parameters.
pub fn sum_rows[T](items fn () []T, visit fn (cell &Cell[T]) int) int {
	each := Each[T]{
		visit: visit
	}
	mut total := 0
	for item in items() {
		total += each.visit(&Cell[T]{
			value: item
		})
	}
	return total
}

struct EachResult[T] {
	visit fn (cell &Cell[T]) ! = unsafe { nil }
}

// visit_each calls `visit` with a cell of every item `items` returns, and stops
// at its first error.
pub fn visit_each[T](items fn () []T, visit fn (cell &Cell[T]) !) ! {
	each := EachResult[T]{
		visit: visit
	}
	for item in items() {
		each.visit(&Cell[T]{
			value: item
		})!
	}
}

// visit_value calls `visit` with a cell of `value`: `T` is bound by `value`,
// and the type of the callback parameter follows it.
pub fn visit_value[T](value T, visit fn (cell &Cell[T]) int) int {
	each := Each[T]{
		visit: visit
	}
	return each.visit(&Cell[T]{
		value: value
	})
}

// cell_of returns a cell of `value`.
pub fn cell_of[T](value T) Cell[T] {
	return Cell[T]{
		value: value
	}
}

// value_of returns the value of `cell`.
pub fn value_of[T](cell Cell[T]) T {
	return cell.value
}

// map_value calls `f` with the value of `c`: `U` is bound by the callback, and
// `T` by the receiver.
pub fn (c Cell[T]) map_value[U](f fn (values []T) []U) []U {
	return f([c.value])
}

// first_of returns the first value a channel of `T` receives.
pub fn first_of[T](values chan T) T {
	return <-values
}

// fixed_first returns the first value of a fixed array of `T`.
pub fn fixed_first[T](values [2]T) T {
	return values[0]
}
