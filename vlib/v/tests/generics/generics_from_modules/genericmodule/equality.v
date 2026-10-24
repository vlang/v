module genericmodule

// Detail and Record have the names of a caller's types: the equality of an
// instance over the caller's types compares their fields, not these.
pub struct Detail {
pub:
	label string
	flag  bool
}

pub struct Record {
pub:
	name   string
	detail Detail
}

// equal_values reports whether `a` and `b` are equal.
pub fn equal_values[T](a T, b T) bool {
	return a == b
}

// differ_values reports whether `a` and `b` differ.
pub fn differ_values[T](a T, b T) bool {
	return a != b
}

// contains_value reports whether `items` holds `item`.
pub fn contains_value[T](items []T, item T) bool {
	return item in items
}

// equal_arrays reports whether `a` and `b` hold equal values.
pub fn equal_arrays[T](a []T, b []T) bool {
	return a == b
}

@[heap]
pub struct Slot[T] {
pub mut:
	value T
}

// peek returns the value of `s`.
pub fn (s &Slot[T]) peek() T {
	return s.value
}

// Tracker keeps a slot per position.
pub struct Tracker[T] {
mut:
	slots map[int]&Slot[T]
}

// update stores `items` and returns how many of them changed.
pub fn (mut tracker Tracker[T]) update(items []T) int {
	mut changed := 0
	for i, item in items {
		if mut slot := tracker.slots[i] {
			if slot.peek() != item {
				slot.value = item
				changed++
			}
			continue
		}
		tracker.slots[i] = &Slot[T]{
			value: item
		}
		changed++
	}
	return changed
}

// own_records returns records of this module.
pub fn own_records() []Record {
	return [Record{
		name: 'own'
	}]
}
