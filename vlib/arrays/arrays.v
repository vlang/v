module arrays

// Common arrays functions:
// - min / max - return the value of the minumum / maximum
// - idx_min / idx_max - return the index of the first minumum / maximum
// - merge - combine two sorted arrays and maintain sorted order
// - chunk - chunk array to arrays with n elements
// - window - get snapshots of the window of the given size sliding along array with the given step, where each snapshot is an array
// - zip - concat two arrays into one map

// min returns the minimum value in the array
// Example: arrays.min([1,2,3,0,9]) => 0
pub fn min<T>(a []T) ?T {
	if a.len == 0 {
		return error('.min called on an empty array')
	}
	mut val := a[0]
	for e in a {
		if e < val {
			val = e
		}
	}
	return val
}

// max returns the maximum the maximum value in the array
// Example: arrays.max([1,2,3,0,9]) => 9
pub fn max<T>(a []T) ?T {
	if a.len == 0 {
		return error('.max called on an empty array')
	}
	mut val := a[0]
	for e in a {
		if e > val {
			val = e
		}
	}
	return val
}

// idx_min returns the index of the minimum value in the array
// Example: arrays.idx_min([1,2,3,0,9]) => 3
pub fn idx_min<T>(a []T) ?int {
	if a.len == 0 {
		return error('.idx_min called on an empty array')
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

// idx_max returns the index of the maximum value in the array
// Example: arrays.idx_max([1,2,3,0,9]) => 4
pub fn idx_max<T>(a []T) ?int {
	if a.len == 0 {
		return error('.idx_max called on an empty array')
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
// Example: arrays.merge([1,3,5,7], [2,4,6,8]) => [1,2,3,4,5,6,7,8]
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
// (an error will be generated if the type annotation is omitted)
// Example: arrays.group<int>([1,2,3],[4,5,6]) => [[1, 4], [2, 5], [3, 6]]
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

// chunk array into a single array of arrays where each element is the next `size` elements of the original
// Example: arrays.chunk([1, 2, 3, 4, 5, 6, 7, 8, 9], 2)) => [[1, 2], [3, 4], [5, 6], [7, 8], [9]]
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
//
// Example: arrays.window([1, 2, 3, 4], size: 2) => [[1, 2], [2, 3], [3, 4]]
// Example: arrays.window([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], size: 3, step: 2) => [[1, 2, 3], [3, 4, 5], [5, 6, 7], [7, 8, 9]]
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

// sum up array, return nothing when array has no elements
//
// NOTICE: currently V has bug that cannot make sum function takes custom struct with + operator overloaded
// which means you can only pass array of numbers for now.
// TODO: Fix generic operator overloading detection issue.
// Example: arrays.sum<int>([1, 2, 3, 4, 5])? => 15
pub fn sum<T>(list []T) ?T {
	if list.len == 0 {
		return error('Cannot sum up array of nothing.')
	} else {
		mut head := list[0]

		for i, e in list {
			if i == 0 {
				continue
			} else {
				head += e
			}
		}

		return head
	}
}

// accumulates values with the first element and applying providing operation to current accumulator value and each elements.
// If the array is empty, then returns error.
// Example: arrays.reduce([1, 2, 3, 4, 5], fn (t1 int, t2 int) int { return t1 * t2 })? => 120
pub fn reduce<T>(list []T, reduce_op fn (t1 T, t2 T) T) ?T {
	if list.len == 0 {
		return error('Cannot reduce array of nothing.')
	} else {
		mut value := list[0]

		for i, e in list {
			if i == 0 {
				continue
			} else {
				value = reduce_op(value, e)
			}
		}

		return value
	}
}

// accumulates values with providing initial value and applying providing operation to current accumulator value and each elements.
// Example: arrays.fold<string, byte>(['H', 'e', 'l', 'l', 'o'], 0, fn (r int, t string) int { return r + t[0] }) => 149
pub fn fold<T, R>(list []T, init R, fold_op fn (r R, t T) R) R {
	mut value := init

	for e in list {
		value = fold_op(value, e)
	}

	return value
}

// flattens n + 1 dimensional array into n dimensional array
// Example: arrays.flatten<int>([[1, 2, 3], [4, 5]]) => [1, 2, 3, 4, 5]
pub fn flatten<T>(list [][]T) []T {
	// calculate required capacity
	mut required_size := 0

	for e1 in list {
		for _ in e1 {
			required_size += 1
		}
	}

	// allocate flattened array
	mut result := []T{cap: required_size}

	for e1 in list {
		for e2 in e1 {
			result << e2
		}
	}

	return result
}

// grouping list of elements with given key selector.
// Example: arrays.assort<int, string>(['H', 'el', 'lo'], fn (v string) int { return v.len }) => {1: ['H'], 2: ['el', 'lo']}
pub fn group_by<K, V>(list []V, grouping_op fn (v V) K) map[K][]V {
	mut result := map[K][]V{}

	for v in list {
		key := grouping_op(v)

		// check if key exists, if not, then create a new array with matched value, otherwise append.
		if key in result {
			result[key] << v
		} else {
			result[key] = [v]
		}
	}

	return result
}

// concatenate an array with an arbitrary number of additional values
//
// PROPOSED CHANGE: make the type of the second argument []T instead of ...T
// Example: arrays.concat([1, 2, 3], 4, 5, 6) => [1, 2, 3, 4, 5, 6]
// Example: arrays.concat([1, 2, 3], ...[4, 5, 6]) => [1, 2, 3, 4, 5, 6]
pub fn concat<T>(a []T, b ...T) []T {
	mut m := []T{cap: a.len + b.len}

	m << a
	m << b

	return m
}

// returns the smallest element >= val, requires `arr` to be sorted
// Example: arrays.lower_bound([2, 4, 6, 8], 3)? => 4
pub fn lower_bound<T>(arr []T, val T) ?T {
	if arr.len == 0 {
		return error('.lower_bound called on an empty array')
	}
	mut left, mut right := 0, arr.len - 1
	for ; left <= right; {
		idx := (left + right) / 2
		elem := arr[idx]
		if elem < val {
			left = idx + 1
		} else {
			right = idx - 1
		}
	}
	if left >= arr.len {
		return error('')
	} else {
		return arr[left]
	}
}

// returns the largest element <= val, requires `arr` to be sorted
// Example: arrays.upper_bound([2, 4, 6, 8], 3)? => 2
pub fn upper_bound<T>(arr []T, val T) ?T {
	if arr.len == 0 {
		return error('.upper_bound called on an empty array')
	}
	mut left, mut right := 0, arr.len - 1
	for ; left <= right; {
		idx := (left + right) / 2
		elem := arr[idx]
		if elem > val {
			right = idx - 1
		} else {
			left = idx + 1
		}
	}
	if right < 0 {
		return error('')
	} else {
		return arr[right]
	}
}

// binary search, requires `arr` to be sorted, returns index of found item or error.
// Binary searches on sorted lists can be faster than other array searches because at maximum
// the algorithm only has to traverse log N elements
// Example: arrays.binary_search([1, 2, 3, 4], 4) ? => 3
pub fn binary_search<T>(arr []T, target T) ?int {
	mut left := 0
	mut right := arr.len - 1
	for ; left <= right; {
		idx := (left + right) / 2
		elem := arr[idx]
		if elem == target {
			return idx
		}
		if elem < target {
			left = idx + 1
		} else {
			right = idx - 1
		}
	}
	return error('')
}
