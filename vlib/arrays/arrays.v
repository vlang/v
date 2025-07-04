module arrays

import strings

// Common arrays functions:
// - min / max - return the value of the minimum / maximum
// - idx_min / idx_max - return the index of the first minimum / maximum
// - merge - combine two sorted arrays and maintain sorted order
// - append - combine two arrays, by appending the second array to the first
// - chunk - chunk array to arrays with n elements
// - window - get snapshots of the window of the given size sliding along array with the given step, where each snapshot is an array
// - group - merge two arrays by interleaving e.g. arrays.group([1,3,5], [2,4,6]) => [[1,2],[3,4],[5,6]]
// - flatten - reduce dimensionality of array by one. e.g. arrays.flatten([[1,2],[3,4],[5,6]]) => [1,2,3,4,5,6]
// - each - call a callback fn, for each element of the array, similar to a.map(), but unlike it, the callback should not return anything

// min returns the minimum value in the array.
// Example: arrays.min([1, 2, 3, 0, 9])! // => 0
pub fn min[T](array []T) !T {
	if array.len == 0 {
		return error('.min called on an empty array')
	}
	mut val := array[0]
	for e in array {
		if e < val {
			val = e
		}
	}
	return val
}

// max returns the maximum value in the array.
// Example: arrays.max([1, 2, 3, 0, 9])! // => 9
pub fn max[T](array []T) !T {
	if array.len == 0 {
		return error('.max called on an empty array')
	}
	mut val := array[0]
	for e in array {
		if e > val {
			val = e
		}
	}
	return val
}

// idx_min returns the index of the minimum value in the array.
// Example: arrays.idx_min([1, 2, 3, 0, 9])! // => 3
pub fn idx_min[T](array []T) !int {
	if array.len == 0 {
		return error('.idx_min called on an empty array')
	}
	mut idx := 0
	mut val := array[0]
	for i, e in array {
		if e < val {
			val = e
			idx = i
		}
	}
	return idx
}

// idx_max returns the index of the maximum value in the array.
// Example: arrays.idx_max([1, 2, 3, 0, 9])! // => 4
pub fn idx_max[T](array []T) !int {
	if array.len == 0 {
		return error('.idx_max called on an empty array')
	}
	mut idx := 0
	mut val := array[0]
	for i, e in array {
		if e > val {
			val = e
			idx = i
		}
	}
	return idx
}

// merge two sorted arrays (ascending) and maintain sorted order.
// Example: arrays.merge([1, 3, 5, 7], [2, 4, 6, 8]) // => [1, 2, 3, 4, 5, 6, 7, 8]
@[direct_array_access]
pub fn merge[T](a []T, b []T) []T {
	mut m := []T{len: a.len + b.len}
	mut ia := 0
	mut ib := 0
	mut j := 0
	// TODO: efficient approach to merge_desc where: a[ia] >= b[ib]
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

// append the second array `b` to the first array `a`, and return the result.
// Note, that unlike arrays.concat, arrays.append is less flexible, but more efficient,
// since it does not require you to use ...a for the second parameter.
// Example: arrays.append([1, 3, 5, 7], [2, 4, 6, 8]) // => [1, 3, 5, 7, 2, 4, 6, 8]
pub fn append[T](a []T, b []T) []T {
	mut m := []T{cap: a.len + b.len}
	m << a
	m << b
	return m
}

// group n arrays into a single array of arrays with n elements.
// This function is analogous to the "zip" function of other languages.
// To fully interleave two arrays, follow this function with a call to `flatten`.
// NOTE: An error will be generated if the type annotation is omitted.
// Example: arrays.group[int]([1, 2, 3], [4, 5, 6]) // => [[1, 4], [2, 5], [3, 6]]
pub fn group[T](arrs ...[]T) [][]T {
	mut length := if arrs.len > 0 { arrs[0].len } else { 0 }
	// calculate length of output by finding shortest input array
	for ndx in 1 .. arrs.len {
		if arrs[ndx].len < length {
			length = arrs[ndx].len
		}
	}

	if length > 0 {
		mut arr := [][]T{cap: length}
		// append all combined arrays into the resultant array
		for ndx in 0 .. length {
			mut grouped := []T{cap: arrs.len}
			// combine each list item for the ndx position into one array
			for arr_ndx in 0 .. arrs.len {
				grouped << arrs[arr_ndx][ndx]
			}
			arr << grouped
		}
		return arr
	}

	return [][]T{}
}

// chunk array into a single array of arrays where each element is the next `size` elements of the original.
// Example: arrays.chunk([1, 2, 3, 4, 5, 6, 7, 8, 9], 2)) // => [[1, 2], [3, 4], [5, 6], [7, 8], [9]]
pub fn chunk[T](array []T, size int) [][]T {
	// allocate chunk array
	mut chunks := [][]T{cap: array.len / size + if array.len % size == 0 { 0 } else { 1 }}

	for i := 0; true; {
		// check chunk size is greater than remaining element size
		if array.len < i + size {
			// check if there's no more element to chunk
			if array.len <= i {
				break
			}

			chunks << array[i..]

			break
		}

		chunks << array[i..i + size]
		i += size
	}

	return chunks
}

// chunk_while splits the input array `a` into chunks of varying length, using the `predicate`, passing to it pairs of adjacent elements `before` and `after`.
// Each chunk, will contain all ajdacent elements, for which the `predicate` returned true.
// The chunks are split *between* the `before` and `after` elements, for which the `predicate` returned false.
// Example: assert arrays.chunk_while([0,9,2,2,3,2,7,5,9,5],fn(x int,y int)bool{return x<=y})==[[0,9],[2,2,3],[2,7],[5,9],[5]]
// Example: assert arrays.chunk_while('aaaabbbcca'.runes(),fn(x rune,y rune)bool{return x==y})==[[`a`,`a`,`a`,`a`],[`b`,`b`,`b`],[`c`,`c`],[`a`]]
// Example: assert arrays.chunk_while('aaaabbbcca'.runes(),fn(x rune,y rune)bool{return x==y}).map({it[0]:it.len})==[{`a`:4},{`b`:3},{`c`:2},{`a`:1}]
pub fn chunk_while[T](a []T, predicate fn (before T, after T) bool) [][]T {
	if a.len == 0 {
		return []
	}
	mut chunks := [][]T{}
	mut chunk := [a[0]]
	mut i := 0
	for i = 1; i < a.len; i++ {
		// eprintln('> i: ${i} | a[i]: ${a[i]} | predicate: ${predicate(a[i-1], a[i]):10} | chunk: ${chunk}')
		if predicate(a[i - 1], a[i]) {
			chunk << a[i]
			continue
		}
		if chunk.len > 0 {
			chunks << chunk
		}
		chunk = [a[i]]
	}
	if chunk.len > 0 {
		chunks << chunk
	}
	return chunks
}

pub struct WindowAttribute {
pub:
	size int
	step int = 1
}

// get snapshots of the window of the given size sliding along array with the given step, where each snapshot is an array.
// - `size` - snapshot size
// - `step` - gap size between each snapshot, default is 1.
//
// Example: arrays.window([1, 2, 3, 4], size: 2) // => [[1, 2], [2, 3], [3, 4]]
// Example: arrays.window([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], size: 3, step: 2) // => [[1, 2, 3], [3, 4, 5], [5, 6, 7], [7, 8, 9]]
pub fn window[T](array []T, attr WindowAttribute) [][]T {
	if array.len == 0 {
		return [][]T{}
	}
	// allocate snapshot array
	mut windows := [][]T{cap: array.len - attr.size + 1}

	for i := 0; true; {
		// check remaining elements size is less than snapshot size
		if array.len < i + attr.size {
			break
		}

		windows << array[i..i + attr.size]
		i += attr.step
	}

	return windows
}

// sum up array, return an error, when the array has no elements.
// Example: arrays.sum([1, 2, 3, 4, 5])! // => 15
pub fn sum[T](array []T) !T {
	if array.len == 0 {
		return error('Cannot sum up array of nothing.')
	} else {
		mut head := array[0]

		for e in array[1..] {
			head += e
		}

		return head
	}
}

// reduce sets `acc = array[0]`, then successively calls `acc = reduce_op(acc, elem)` for each remaining element in `array`.
// returns the accumulated value in `acc`.
// returns an error if the array is empty.
// See also: [fold](#fold).
// Example: arrays.reduce([1, 2, 3, 4, 5], fn (t1 int, t2 int) int { return t1 * t2 })! // => 120
pub fn reduce[T](array []T, reduce_op fn (acc T, elem T) T) !T {
	if array.len == 0 {
		return error('Cannot reduce array of nothing.')
	} else {
		mut value := array[0]

		for e in array[1..] {
			value = reduce_op(value, e)
		}

		return value
	}
}

// reduce_indexed sets `acc = array[0]`, then successively calls `acc = reduce_op(idx, acc, elem)` for each remaining element in `array`.
// returns the accumulated value in `acc`.
// returns an error if the array is empty.
// See also: [fold_indexed](#fold_indexed).
pub fn reduce_indexed[T](array []T, reduce_op fn (idx int, acc T, elem T) T) !T {
	if array.len == 0 {
		return error('Cannot reduce array of nothing.')
	} else {
		mut value := array[0]

		for i, e in array {
			if i == 0 {
				continue
			} else {
				value = reduce_op(i, value, e)
			}
		}

		return value
	}
}

// filter_indexed filters elements based on `predicate` function being invoked on each element with its index in the original array.
pub fn filter_indexed[T](array []T, predicate fn (idx int, elem T) bool) []T {
	mut result := []T{cap: array.len}

	for i, e in array {
		if predicate(i, e) {
			result << e
		}
	}

	return result
}

// fold sets `acc = init`, then successively calls `acc = fold_op(acc, elem)` for each element in `array`.
// returns `acc`.
// Example:
// ```v
// // Sum the length of each string in an array
// a := ['Hi', 'all']
// r := arrays.fold[string, int](a, 0,
// 	fn (r int, t string) int { return r + t.len })
// assert r == 5
// ```
pub fn fold[T, R](array []T, init R, fold_op fn (acc R, elem T) R) R {
	mut value := R{}
	$if R is $array {
		value = init.clone()
	} $else {
		value = init
	}

	for e in array {
		value = fold_op(value, e)
	}

	return value
}

// fold_indexed sets `acc = init`, then successively calls `acc = fold_op(idx, acc, elem)` for each element in `array`.
// returns `acc`.
pub fn fold_indexed[T, R](array []T, init R, fold_op fn (idx int, acc R, elem T) R) R {
	mut value := init

	for i, e in array {
		value = fold_op(i, value, e)
	}

	return value
}

// flatten flattens n + 1 dimensional array into n dimensional array.
// Example: arrays.flatten[int]([[1, 2, 3], [4, 5]]) // => [1, 2, 3, 4, 5]
pub fn flatten[T](array [][]T) []T {
	// calculate required capacity
	mut required_size := 0

	for e1 in array {
		for _ in e1 {
			required_size += 1
		}
	}

	// allocate flattened array
	mut result := []T{cap: required_size}

	for e1 in array {
		for e2 in e1 {
			result << e2
		}
	}

	return result
}

// flat_map creates a new array populated with the flattened result of calling transform function being invoked on each element of `list`.
pub fn flat_map[T, R](array []T, transform fn (elem T) []R) []R {
	mut result := [][]R{cap: array.len}

	for v in array {
		result << transform(v)
	}

	return flatten(result)
}

// flat_map_indexed creates a new array with the flattened result of calling the `transform` fn, invoked on each idx,elem pair from the original.
pub fn flat_map_indexed[T, R](array []T, transform fn (idx int, elem T) []R) []R {
	mut result := [][]R{cap: array.len}

	for i, v in array {
		result << transform(i, v)
	}

	return flatten(result)
}

// map_indexed creates a new array with the result of calling the `transform` fn, invoked on each idx,elem pair from the original.
pub fn map_indexed[T, R](array []T, transform fn (idx int, elem T) R) []R {
	mut result := []R{cap: array.len}

	for i, v in array {
		result << transform(i, v)
	}

	return result
}

// group_by groups together elements, for which the `grouping_op` callback produced the same result.
// Example: arrays.group_by[int, string](['H', 'el', 'lo'], fn (v string) int { return v.len }) // => {1: ['H'], 2: ['el', 'lo']}
pub fn group_by[K, V](array []V, grouping_op fn (val V) K) map[K][]V {
	mut result := map[K][]V{}

	for v in array {
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

// concatenate an array with an arbitrary number of additional values.
//
// NOTE: if you have two arrays, you should simply use the `<<` operator directly
// Example: arrays.concat([1, 2, 3], 4, 5, 6) == [1, 2, 3, 4, 5, 6] // => true
// Example: arrays.concat([1, 2, 3], ...[4, 5, 6]) == [1, 2, 3, 4, 5, 6] // => true
// Example: arr << [4, 5, 6] // does what you need if arr is mutable
pub fn concat[T](a []T, b ...T) []T {
	mut m := []T{cap: a.len + b.len}

	m << a
	m << b

	return m
}

// returns the smallest element >= val, requires `array` to be sorted.
// Example: arrays.lower_bound([2, 4, 6, 8], 3)! // => 4
pub fn lower_bound[T](array []T, val T) !T {
	if array.len == 0 {
		return error('.lower_bound called on an empty array')
	}
	mut left, mut right := 0, array.len - 1
	for ; left <= right; {
		idx := (left + right) / 2
		elem := array[idx]
		if elem < val {
			left = idx + 1
		} else {
			right = idx - 1
		}
	}
	if left >= array.len {
		return error('')
	} else {
		return array[left]
	}
}

// returns the largest element <= val, requires `array` to be sorted.
// Example: arrays.upper_bound([2, 4, 6, 8], 3)! // => 2
pub fn upper_bound[T](array []T, val T) !T {
	if array.len == 0 {
		return error('.upper_bound called on an empty array')
	}
	mut left, mut right := 0, array.len - 1
	for ; left <= right; {
		idx := (left + right) / 2
		elem := array[idx]
		if elem > val {
			right = idx - 1
		} else {
			left = idx + 1
		}
	}
	if right < 0 {
		return error('')
	} else {
		return array[right]
	}
}

// binary_search, requires `array` to be sorted, returns index of found item or error.
// Binary searches on sorted lists can be faster than other array searches because at maximum
// the algorithm only has to traverse log N elements
// Example: arrays.binary_search([1, 2, 3, 4], 4)! // => 3
pub fn binary_search[T](array []T, target T) !int {
	mut left := 0
	mut right := array.len - 1
	for ; left <= right; {
		idx := (left + right) / 2
		elem := array[idx]
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

// rotate_left rotates the array in-place.
// It does it in such a way, that the first `mid` elements of the array, move to the end,
// while the last `array.len - mid` elements move to the front.
// After calling `rotate_left`, the element previously at index `mid` will become the first element in the array.
// Example:
// ```v
// mut x := [1,2,3,4,5,6]
// arrays.rotate_left(mut x, 2)
// println(x) // [3, 4, 5, 6, 1, 2]
// ```
pub fn rotate_left[T](mut array []T, mid int) {
	assert mid <= array.len && mid >= 0
	k := array.len - mid
	p := &T(array.data)
	unsafe {
		ptr_rotate[T](mid, &T(usize(voidptr(p)) + usize(sizeof(T)) * usize(mid)), k)
	}
}

// rotate_right rotates the array in-place.
// It does it in such a way, that the first `array.len - k` elements of the array, move to the end,
// while the last `k` elements move to the front.
// After calling `rotate_right`, the element previously at index `array.len - k` will become the first element in the array.
// Example:
// ```v
// mut x := [1,2,3,4,5,6]
// arrays.rotate_right(mut x, 2)
// println(x) // [5, 6, 1, 2, 3, 4]
// ```
pub fn rotate_right[T](mut array []T, k int) {
	assert k <= array.len && k >= 0
	mid := array.len - k
	p := &T(array.data)
	unsafe {
		ptr_rotate[T](mid, &T(usize(voidptr(p)) + usize(sizeof(T)) * usize(mid)), k)
	}
}

@[unsafe]
fn ptr_rotate[T](left_ int, mid &T, right_ int) {
	sz := usize(sizeof(T))
	mut left := usize(left_)
	mut right := usize(right_)
	limit := raw_array_cap[T]()
	for {
		delta := if left < right { left } else { right }
		if delta <= usize(limit) {
			break
		}
		unsafe {
			swap_nonoverlapping[T](&T(usize(voidptr(mid)) - left * sz), &T(usize(voidptr(mid)) +
				usize(right - delta) * sz), int(delta))
		}
		if left <= right {
			right -= delta
		} else {
			left -= delta
		}
	}
	unsafe {
		rawarray := malloc(raw_array_malloc_size[T]())
		defer {
			free(rawarray)
		}
		dim := &T(usize(voidptr(mid)) - left * sz + right * sz)
		if left <= right {
			vmemcpy(rawarray, voidptr(usize(voidptr(mid)) - left * sz), isize(left * sz))
			vmemmove(voidptr(usize(voidptr(mid)) - left * sz), voidptr(mid), isize(right * sz))
			vmemcpy(voidptr(dim), rawarray, isize(left * sz))
		} else {
			vmemcpy(rawarray, voidptr(mid), isize(right * sz))
			vmemmove(voidptr(dim), voidptr(usize(voidptr(mid)) - left * sz), isize(left * sz))
			vmemcpy(voidptr(usize(voidptr(mid)) - left * sz), rawarray, isize(right * sz))
		}
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

const extra_size = 32 * isize(sizeof(usize))

fn raw_array_cap[T]() isize {
	size := isize(sizeof(T))
	if size > extra_size {
		return 1
	} else {
		return extra_size / size
	}
}

fn raw_array_malloc_size[T]() isize {
	size := isize(sizeof(T))
	if size > extra_size {
		return size * 2
	} else {
		return extra_size
	}
}

@[unsafe]
fn memswap(x voidptr, y voidptr, len usize) {
	block_size := isize(sizeof(Block))

	mut i := usize(0)
	for i + usize(block_size) <= len {
		mut t_ := Block{}
		t := voidptr(&t_)

		xi := usize(x) + i
		yi := usize(y) + i
		unsafe {
			vmemcpy(t, voidptr(xi), block_size)
			vmemcpy(voidptr(xi), voidptr(yi), block_size)
			vmemcpy(t, voidptr(yi), block_size)
		}
		i += usize(block_size)
	}
	if i < len {
		mut t_ := UnalignedBlock{}
		t := voidptr(&t_)
		rem := isize(len - i)
		xi := usize(x) + i
		yi := usize(y) + i
		unsafe {
			vmemcpy(t, voidptr(xi), rem)
			vmemcpy(voidptr(xi), voidptr(yi), rem)
			vmemcpy(voidptr(yi), t, rem)
		}
	}
}

@[unsafe]
fn swap_nonoverlapping[T](x_ &T, y_ &T, count int) {
	x := voidptr(x_)
	y := voidptr(y_)

	len := usize(sizeof(T)) * usize(count)
	unsafe {
		memswap(x, y, len)
	}
}

// copy copies the `src` array elements to the `dst` array.
// The number of the elements copied is the minimum of the length of both arrays.
// Returns the number of elements copied.
pub fn copy[T](mut dst []T, src []T) int {
	min := if dst.len < src.len { dst.len } else { src.len }
	if min <= 0 {
		return 0
	}
	if can_copy_bits[T]() {
		blen := min * isize(sizeof(T))
		unsafe { vmemmove(&T(dst.data), src.data, blen) }
	} else {
		for i in 0 .. min {
			dst[i] = src[i]
		}
	}
	return min
}

// can_copy_bits determines if T can be copied using `memcpy`.
// false if autofree needs to intervene
// false if type is not copyable e.g. map
fn can_copy_bits[T]() bool {
	// references, C pointers, integers, floats, runes
	if T.name[0] in [`&`, `b`, `c`, `f`, `i`, `r`, `u`, `v`] {
		return true
	}
	return false
}

// carray_to_varray copies a C byte array into a V array of type `T`.
// See also: `cstring_to_vstring`
@[unsafe]
pub fn carray_to_varray[T](c_array_data voidptr, items int) []T {
	mut v_array := []T{len: items}
	total_size := items * isize(sizeof(T))
	unsafe { vmemcpy(v_array.data, c_array_data, total_size) }
	return v_array
}

// find_first returns the first element that matches the given predicate.
// Returns `none` if no match is found.
// Example: arrays.find_first([1, 2, 3, 4, 5], fn (i int) bool { return i == 3 })? // => 3
pub fn find_first[T](array []T, predicate fn (elem T) bool) ?T {
	if array.len == 0 {
		return none
	}
	for item in array {
		if predicate(item) {
			return item
		}
	}
	return none
}

// find_last returns the last element that matches the given predicate.
// Returns `none` if no match is found.
// Example: arrays.find_last([1, 2, 3, 4, 5], fn (i int) bool { return i == 3})? // => 3
pub fn find_last[T](array []T, predicate fn (elem T) bool) ?T {
	if array.len == 0 {
		return none
	}
	for idx := array.len - 1; idx >= 0; idx-- {
		item := array[idx]
		if predicate(item) {
			return item
		}
	}
	return none
}

// join_to_string takes in a custom transform function and joins all elements into a string with
// the specified separator
@[manualfree]
pub fn join_to_string[T](array []T, separator string, transform fn (elem T) string) string {
	mut sb := strings.new_builder(array.len * 2)
	defer {
		unsafe { sb.free() }
	}
	for i, item in array {
		x := transform(item)
		sb.write_string(x)
		unsafe { x.free() }
		if i < array.len - 1 {
			sb.write_string(separator)
		}
	}
	return sb.str()
}

// partition splits the original array into pair of lists.
// The first list contains elements for which the predicate fn returned true,
// while the second list contains elements for which the predicate fn returned false.
pub fn partition[T](array []T, predicate fn (elem T) bool) ([]T, []T) {
	mut matching, mut non_matching := []T{}, []T{}
	for item in array {
		if predicate(item) {
			matching << item
		} else {
			non_matching << item
		}
	}
	return matching, non_matching
}

// each calls the callback fn `cb`, for each element of the given array `a`.
pub fn each[T](a []T, cb fn (elem T)) {
	for item in a {
		cb(item)
	}
}

// each_indexed calls the callback fn `cb`, for each element of the given array `a`.
// It passes the callback both the index of the current element, and the element itself.
pub fn each_indexed[T](a []T, cb fn (i int, e T)) {
	for idx, item in a {
		cb(idx, item)
	}
}
