module arrays

// bisect_left returns the leftmost index where `val` could be inserted into
// the ascending-sorted `array` keeping it sorted: the index of the first
// element >= val. It is the V-idiomatic equivalent of Python's
// `bisect.bisect_left` / Go's `sort.Search`. Unsorted input yields an
// undefined (but memory-safe) result, matching both languages.
pub fn bisect_left[T](array []T, val T) int {
	mut lo := 0
	mut hi := array.len
	for lo < hi {
		mid := lo + (hi - lo) / 2
		if array[mid] < val {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	return lo
}

// bisect_right returns the rightmost insertion index: the index of the first
// element > val. It is the V-idiomatic equivalent of Python's
// `bisect.bisect_right` (a.k.a. bisect).
pub fn bisect_right[T](array []T, val T) int {
	mut lo := 0
	mut hi := array.len
	for lo < hi {
		mid := lo + (hi - lo) / 2
		if val < array[mid] {
			hi = mid
		} else {
			lo = mid + 1
		}
	}
	return lo
}

// insort_left inserts `val` into the ascending-sorted `array` at the
// bisect_left position, keeping it sorted.
pub fn insort_left[T](mut array []T, val T) {
	i := bisect_left(array, val)
	array.insert(i, val)
}

// insort_right inserts `val` into the ascending-sorted `array` at the
// bisect_right position, keeping it sorted (equal values go after existing
// ones).
pub fn insort_right[T](mut array []T, val T) {
	i := bisect_right(array, val)
	array.insert(i, val)
}
