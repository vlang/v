module arrays

// uniq filters the adjacent matching elements from the given array.
// All adjacent matching elements, are merged to their first occurrence,
// so the output will have no repeating elements.
// Note: `uniq` does not detect repeats, unless they are adjacent.
// You may want to call a.sorted() on your array, before passing the result to arrays.uniq().
// See also arrays.distinct, which is essentially arrays.uniq(a.sorted()) .
// Example: assert arrays.uniq( []int{} ) == []
// Example: assert arrays.uniq( [1, 1] ) == [1]
// Example: assert arrays.uniq( [2, 1] ) == [2, 1]
// Example: assert arrays.uniq( [5, 5, 1, 5, 2, 1, 1, 9] ) == [5, 1, 5, 2, 1, 9]
pub fn uniq[T](a []T) []T {
	mut res := []T{cap: a.len / 10}
	mut j := -1
	if a.len > 0 {
		j = 0
		res << a[0]
	}
	for idx, e in a {
		if a[j] == e {
			continue
		}
		j = idx
		res << e
	}
	return res
}

// uniq_only filters the adjacent matching elements from the given array.
// All adjacent matching elements, are removed.
// The output will contain only the elements that *did not have* any adjacent matches.
// Note: `uniq_only` does not detect repeats, unless they are adjacent.
// You may want to call a.sorted() on your array, before passing the result to arrays.uniq_only().
// Example: assert arrays.uniq_only( []int{} ) == []
// Example: assert arrays.uniq_only( [1, 1] ) == []
// Example: assert arrays.uniq_only( [2, 1] ) == [2, 1]
// Example: assert arrays.uniq_only( [1, 5, 5, 1, 5, 2, 1, 1, 9] ) == [1, 1, 5, 2, 9]
pub fn uniq_only[T](a []T) []T {
	// simple cases:
	if a.len == 0 {
		return []
	}
	if a.len == 1 {
		return a.clone()
	}
	if a.len == 2 {
		if a[0] != a[1] {
			return a.clone()
		}
		return []
	}
	mut res := []T{cap: a.len / 20}
	// head element:
	if a[0] != a[1] {
		res << a[0]
	}
	// middle elements:
	for idx := 1; idx + 1 < a.len; idx++ {
		if a[idx - 1] != a[idx] && a[idx + 1] != a[idx] {
			res << a[idx]
		}
	}
	// tail element:
	if a[a.len - 2] != a[a.len - 1] {
		res << a[a.len - 1]
	}
	return res
}

// uniq_only_repeated produces the adjacent matching elements from the given array.
// Unique elements, with no duplicates are removed.
// Adjacent matching elements, are reduced to just 1 element per repeat group.
// Note: `uniq_only_repeated` does not detect repeats, unless they are adjacent.
// You may want to call a.sorted() on your array, before passing the result to arrays.uniq_only_repeated().
// Example: assert arrays.uniq_only_repeated( []int{} ) == []
// Example: assert arrays.uniq_only_repeated( [1, 5] ) == []
// Example: assert arrays.uniq_only_repeated( [5, 5] ) == [5]
// Example: assert arrays.uniq_only_repeated( [5, 5, 1, 5, 2, 1, 1, 9] ) == [5, 1]
pub fn uniq_only_repeated[T](a []T) []T {
	// simple cases:
	if a.len == 0 || a.len == 1 {
		return []
	}
	mut res := []T{cap: a.len / 20}
	loop: for i := 0; i + 1 < a.len; i++ {
		if a[i] == a[i + 1] {
			// at least 2 match; find the span length:
			for j := i + 2; j < a.len; j++ {
				if a[i] != a[j] {
					// found the right border of the repeated elements
					if j - i > 1 {
						res << a[i]
						i = j - 1
						continue loop
					}
				}
			}
			break
		}
	}
	// tail element:
	if a[a.len - 2] == a[a.len - 1] {
		res << a[a.len - 1]
	}
	return res
}

// uniq_all_repeated produces all adjacent matching elements from the given array.
// Unique elements, with no duplicates are removed.
// The output will contain all the duplicated elements, repeated just like they were in the original.
// Note: `uniq_all_repeated` does not detect repeats, unless they are adjacent.
// You may want to call a.sorted() on your array, before passing the result to arrays.uniq_all_repeated().
// Example: assert arrays.uniq_all_repeated( []int{} ) == []
// Example: assert arrays.uniq_all_repeated( [1, 5] ) == []
// Example: assert arrays.uniq_all_repeated( [5, 5] ) == [5,5]
// Example: assert arrays.uniq_all_repeated( [5, 5, 1, 5, 2, 1, 1, 9] ) == [5, 5, 1, 1]
pub fn uniq_all_repeated[T](a []T) []T {
	// simple cases:
	if a.len == 0 || a.len == 1 {
		return []
	}
	if a.len == 2 {
		if a[0] == a[1] {
			return a.clone()
		}
	}
	mut res := []T{cap: a.len / 20}
	loop: for i := 0; i + 1 < a.len; i++ {
		if a[i] == a[i + 1] {
			res << a[i]
			for j := i + 1; j < a.len; j++ {
				if a[i] != a[j] && j - i > 0 {
					// found the right border of the repeated elements
					i = j - 1
					continue loop
				}
				res << a[i]
			}
			break
		}
	}
	return res
}

// distinct returns all distinct elements from the given array a.
// The results are guaranteed to be unique, i.e. not have duplicates.
// See also arrays.uniq, which can be used to achieve the same goal,
// but needs you to first sort the array.
// Example: assert arrays.distinct( [5, 5, 1, 5, 2, 1, 1, 9] ) == [1, 2, 5, 9]
pub fn distinct[T](a []T) []T {
	return uniq(a.sorted(a < b))
}
