module arrays

// is_sorted reports whether a is in non-descending order (a[i-1] <= a[i]).
// The empty and single-element arrays are sorted.
// It is the V-idiomatic equivalent of Go's slices.IsSorted.
pub fn is_sorted[T](a []T) bool {
	for i in 1 .. a.len {
		if a[i - 1] > a[i] {
			return false
		}
	}
	return true
}
