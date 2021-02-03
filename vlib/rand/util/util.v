// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import rand

[direct_array_access]
fn element_location(element int, array []int) int {
	if array.len == 0 {
		return 0
	}
	if array.len == 1 {
		return if element == array[0] {
			0
		} else {
			1
		}
	}
	mut lo := 0
	mut hi := array.len - 1
	for lo <= hi {
		mut mid := lo + ((hi - lo) >> 1)
		current_element := array[mid]
		if element == current_element {
			return mid
		} else if element < current_element {
			hi = mid - 1
		} else {
			lo = mid + 1
		}
	}
	return lo
}

[direct_array_access]
fn insert_element(element int, location int, mut array []int) {
	needs_shift := location < array.len
	array << element
	if needs_shift {
		count := array.len - location
		unsafe {
			C.memcpy(&array[location + 1], &array[location], count * 4)
		}
		array[location] = element
	}
}

// sample_nr returns a sample of the array without replacement. This means the indices cannot repeat and it restricts the sample size to be less than or equal to the size of the given array. Note that if the array has repeating elements, then the sample may have repeats as well.
pub fn sample_nr<T>(array []T, k int) []T {
	n := array.len
	if k > n {
		panic('Cannot sample $k elements without replacement from a $n-element array.')
	}
	mut results := []T{len: k}
	mut indices := []int{cap: k}
	for indices.len < k {
		index := rand.intn(n)
		location := element_location(index, indices)
		if location >= indices.len || indices[location] != index {
			insert_element(index, location, mut indices)
		}
	}
	for i in 0 .. k {
		results[i] = array[indices[i]]
	}
	return results
}

// sample_r returns a sample of the array with replacement. This means the elements can repeat and the size of the sample may exceed the size of the array
pub fn sample_r<T>(array []T, k int) []T {
	n := array.len
	mut results := []T{len: k}
	for i in 0 .. k {
		results[i] = array[rand.intn(n)]
	}
	return results
}
