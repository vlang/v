module arrays

// Common arrays functions:
// - min / max - return the value of the minumum / maximum
// - idx_min / idx_max - return the index of the first minumum / maximum
// - merge - combine two sorted arrays and maintain sorted order
// - chunk - chunk array to arrays with n elements
// - window - get snapshots of the window of the given size sliding along array with the given step, where each snapshot is an array
// - zip - concat two arrays into one map

// min returns the minimum
pub fn min<T>(a []T) T {
	if a.len == 0 {
		panic('.min called on an empty array')
	}
	mut val := a[0]
	for e in a {
		if e < val {
			val = e
		}
	}
	return val
}

// max returns the maximum
pub fn max<T>(a []T) T {
	if a.len == 0 {
		panic('.max called on an empty array')
	}
	mut val := a[0]
	for e in a {
		if e > val {
			val = e
		}
	}
	return val
}

// idx_min returns the index of the first minimum
pub fn idx_min<T>(a []T) int {
	if a.len == 0 {
		panic('.idx_min called on an empty array')
	}
	mut idx := 0
	mut val := a[0]
	for i, e in a {
		if e < val {
			val = e
			idx = i
		}
	}
	return idx
}

// idx_max returns the index of the first maximum
pub fn idx_max<T>(a []T) int {
	if a.len == 0 {
		panic('.idx_max called on an empty array')
	}
	mut idx := 0
	mut val := a[0]
	for i, e in a {
		if e > val {
			val = e
			idx = i
		}
	}
	return idx
}

// merge two sorted arrays (ascending) and maintain sorted order
[direct_array_access]
pub fn merge<T>(a []T, b []T) []T {
	mut m := []T{len: a.len + b.len}
	mut ia := 0
	mut ib := 0
	mut j := 0
	// TODO efficient approach to merge_desc where: a[ia] >= b[ib]
	for ia < a.len && ib < b.len {
		if a[ia] <= b[ib] {
			m[j] = a[ia]
			ia++
		} else {
			m[j] = b[ib]
			ib++
		}
		j++
	}
	// a leftovers
	for ia < a.len {
		m[j] = a[ia]
		ia++
		j++
	}
	// b leftovers
	for ib < b.len {
		m[j] = b[ib]
		ib++
		j++
	}
	return m
}

// group n arrays into a single array of arrays with n elements
pub fn group<T>(lists ...[]T) [][]T {
	mut length := if lists.len > 0 { lists[0].len } else { 0 }
	// calculate length of output by finding shortest input array
	for ndx in 1 .. lists.len {
		if lists[ndx].len < length {
			length = lists[ndx].len
		}
	}

	if length > 0 {
		mut arr := [][]T{cap: length}
		// append all combined arrays into the resultant array
		for ndx in 0 .. length {
			mut zipped := []T{cap: lists.len}
			// combine each list item for the ndx position into one array
			for list_ndx in 0 .. lists.len {
				zipped << lists[list_ndx][ndx]
			}
			arr << zipped
		}
		return arr
	}

	return [][]T{}
}

// chunk array to arrays with n elements
// example: arrays.chunk([1, 2, 3], 2) => [[1, 2], [3]]
pub fn chunk<T>(list []T, size int) [][]T {
	// allocate chunk array
	mut chunks := [][]T{cap: list.len / size + if list.len % size == 0 { 0 } else { 1 }}

	for i := 0; true; {
		// check chunk size is greater than remaining element size
		if list.len < i + size {
			// check if there's no more element to chunk
			if list.len <= i {
				break
			}

			chunks << list[i..]

			break
		}

		chunks << list[i..i + size]
		i += size
	}

	return chunks
}

pub struct WindowAttribute {
	size int
	step int = 1
}

// get snapshots of the window of the given size sliding along array with the given step, where each snapshot is an array.
// - `size` - snapshot size
// - `step` - gap size between each snapshot, default is 1.
// example A: arrays.window([1, 2, 3, 4], size: 2) => [[1, 2], [2, 3], [3, 4]]
// example B: arrays.window([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], size: 3, step: 2) => [[1, 2, 3], [3, 4, 5], [5, 6, 7], [7, 8, 9]]
pub fn window<T>(list []T, attr WindowAttribute) [][]T {
	// allocate snapshot array
	mut windows := [][]T{cap: list.len - attr.size + 1}

	for i := 0; true; {
		// check remaining elements size is less than snapshot size
		if list.len < i + attr.size {
			break
		}

		windows << list[i..i + attr.size]
		i += attr.step
	}

	return windows
}
