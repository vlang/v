module arrays

// Common arrays functions:
// - min / max - return the value of the minumum / maximum
// - idx_min / idx_max - return the index of the first minumum / maximum
// - merge - combine two sorted arrays and maintain sorted order
// - chunk - chunk array to arrays with n elements
// - window - get snapshots of the window of the given size sliding along array with the given step, where each snapshot is an array
// - group - merge two arrays by interleaving e.g. arrays.group([1,3,5], [2,4,6]) => [[1,2],[3,4],[5,6]]
// - flatten - reduce dimensionality of array by one. e.g. arrays.flatten([[1,2],[3,4],[5,6]]) => [1,2,3,4,5,6]

// min returns the minimum value in the array
// Example: arrays.min([1,2,3,0,9]) // => 0
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
// Example: arrays.max([1,2,3,0,9]) // => 9
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
// Example: arrays.idx_min([1,2,3,0,9]) // => 3
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
// Example: arrays.idx_max([1,2,3,0,9]) // => 4
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
// Example: arrays.merge([1,3,5,7], [2,4,6,8]) // => [1,2,3,4,5,6,7,8]
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
//
// This function is analogous to the "zip" function of other languages.
// To fully interleave two arrays, follow this function with a call to `flatten`.
//
// NOTE: An error will be generated if the type annotation is omitted.
// Example: arrays.group<int>([1,2,3],[4,5,6]) // => [[1, 4], [2, 5], [3, 6]]
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
			mut grouped := []T{cap: lists.len}
			// combine each list item for the ndx position into one array
			for list_ndx in 0 .. lists.len {
				grouped << lists[list_ndx][ndx]
			}
			arr << grouped
		}
		return arr
	}

	return [][]T{}
}

// chunk array into a single array of arrays where each element is the next `size` elements of the original
// Example: arrays.chunk([1, 2, 3, 4, 5, 6, 7, 8, 9], 2)) // => [[1, 2], [3, 4], [5, 6], [7, 8], [9]]
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
// Example: arrays.window([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], size: 3, step: 2) // => [[1, 2, 3], [3, 4, 5], [5, 6, 7], [7, 8, 9]]
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
// Example: arrays.sum<int>([1, 2, 3, 4, 5])? // => 15
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
// Example: arrays.reduce([1, 2, 3, 4, 5], fn (t1 int, t2 int) int { return t1 * t2 })? // => 120
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
// Example: arrays.fold<string, byte>(['H', 'e', 'l', 'l', 'o'], 0, fn (r int, t string) int { return r + t[0] }) // => 149
pub fn fold<T, R>(list []T, init R, fold_op fn (r R, t T) R) R {
	mut value := init

	for e in list {
		value = fold_op(value, e)
	}

	return value
}

// flattens n + 1 dimensional array into n dimensional array
// Example: arrays.flatten<int>([[1, 2, 3], [4, 5]]) // => [1, 2, 3, 4, 5]
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
// Example: arrays.group_by<int, string>(['H', 'el', 'lo'], fn (v string) int { return v.len }) // => {1: ['H'], 2: ['el', 'lo']}
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
// NOTE: if you have two arrays, you should simply use the `<<` operator directly
// Example: arrays.concat([1, 2, 3], 4, 5, 6) == [1, 2, 3, 4, 5, 6] // => true
// Example: arrays.concat([1, 2, 3], ...[4, 5, 6]) == [1, 2, 3, 4, 5, 6] // => true
// Example: arr << [4, 5, 6] // does what you need if arr is mutable
[deprecated]
pub fn concat<T>(a []T, b ...T) []T {
	mut m := []T{cap: a.len + b.len}

	m << a
	m << b

	return m
}

// returns the smallest element >= val, requires `arr` to be sorted
// Example: arrays.lower_bound([2, 4, 6, 8], 3)? // => 4
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
// Example: arrays.upper_bound([2, 4, 6, 8], 3)? // => 2
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
// Example: arrays.binary_search([1, 2, 3, 4], 4) ? // => 3
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

// rotate_left rotates the array in-place such that the first `mid` elements of the array move to the end
// while the last `arr.len - mid` elements move to the front. After calling `rotate_left`, the element
// previously at index `mid` will become the first element in the array.
// Example:
// ```v
// mut x := [1,2,3,4,5,6]
// arrays.rotate_left(mut x,2)
// println(x) // [3, 4, 5, 6, 1, 2]
// ```
pub fn rotate_left<T>(mut arr []T, mid int) {
	assert mid <= arr.len && mid >= 0
	k := arr.len - mid
	p := &T(arr.data)
	unsafe {
		ptr_rotate<T>(mid, &T(usize(voidptr(p)) + usize(sizeof(T)) * usize(mid)), k)
	}
}

// rotate_right rotates the array in-place such that the first `arr.len - k` elements of the array move to the end
// while the last `k` elements move to the front. After calling `rotate_right`, the element previously at index `arr.len - k`
// will become the first element in the array.
// Example:
// ```v
// mut x := [1,2,3,4,5,6]
// arrays.rotate_right(mut x, 2)
// println(x) // [5, 6, 1, 2, 3, 4]
// ```
pub fn rotate_right<T>(mut arr []T, k int) {
	assert k <= arr.len && k >= 0
	mid := arr.len - k
	p := &T(arr.data)
	unsafe {
		ptr_rotate<T>(mid, &T(usize(voidptr(p)) + usize(sizeof(T)) * usize(mid)), k)
	}
}

[unsafe]
fn ptr_rotate<T>(left_ int, mid &T, right_ int) {
	mut left := usize(left_)
	mut right := usize(right_)
	for {
		delta := if left < right { left } else { right }

		if delta <= raw_array_cap<T>() {
			break
		}
		unsafe {
			swap_nonoverlapping<T>(&T(usize(voidptr(mid)) - left * usize(sizeof(T))),
				&T(usize(voidptr(mid)) + usize(right - delta) * usize(sizeof(T))), int(delta))
		}
		if left <= right {
			right -= delta
		} else {
			left -= delta
		}
	}

	unsafe {
		sz := usize(sizeof(T))
		rawarray := C.malloc(raw_array_malloc_size<T>())
		dim := &T(usize(voidptr(mid)) - left * sz + right * sz)
		if left <= right {
			C.memcpy(rawarray, voidptr(usize(voidptr(mid)) - left * sz), left * sz)
			C.memmove(voidptr(usize(voidptr(mid)) - left * sz), voidptr(mid), right * sz)
			C.memcpy(voidptr(dim), rawarray, left * sz)
		} else {
			C.memcpy(rawarray, voidptr(mid), right * sz)
			C.memmove(voidptr(dim), voidptr(usize(voidptr(mid)) - left * sz), left * sz)
			C.memcpy(voidptr(usize(voidptr(mid)) - left * sz), rawarray, right * sz)
		}
		C.free(rawarray)
	}
}

struct Block {
mut:
	x u64
	y u64
	z u64
	w u64
}

struct UnalignedBlock {
mut:
	x u64
	y u64
	z u64
	w u64
}

const (
	extra_size = 32 * sizeof(usize)
)

fn raw_array_cap<T>() usize {
	if sizeof(T) > arrays.extra_size {
		return 1
	} else {
		return arrays.extra_size / sizeof(T)
	}
}

fn raw_array_malloc_size<T>() usize {
	if sizeof(T) > arrays.extra_size {
		return usize(sizeof(T)) * 2
	} else {
		return 32 * usize(sizeof(usize))
	}
}

[unsafe]
fn memswap(x voidptr, y voidptr, len usize) {
	block_size := sizeof(Block)

	mut i := usize(0)
	for i + block_size <= len {
		mut t_ := Block{}
		t := voidptr(&t_)

		xi := usize(x) + i
		yi := usize(y) + i
		unsafe {
			C.memcpy(t, voidptr(xi), block_size)
			C.memcpy(voidptr(xi), voidptr(yi), block_size)
			C.memcpy(t, voidptr(yi), block_size)
		}
		i += block_size
	}
	if i < len {
		mut t_ := UnalignedBlock{}
		t := voidptr(&t_)
		rem := len - i
		xi := usize(x) + i
		yi := usize(y) + i
		unsafe {
			C.memcpy(t, voidptr(xi), rem)
			C.memcpy(voidptr(xi), voidptr(yi), rem)
			C.memcpy(voidptr(yi), t, rem)
		}
	}
}

[unsafe]
fn swap_nonoverlapping<T>(x_ &T, y_ &T, count int) {
	x := voidptr(x_)
	y := voidptr(y_)

	len := usize(sizeof(T)) * usize(count)
	unsafe {
		memswap(x, y, len)
	}
}
