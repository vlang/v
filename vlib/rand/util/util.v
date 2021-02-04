// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import rand
import arrays

// sample_nr returns a sample of the array without replacement. This means the indices cannot repeat and it restricts the sample size to be less than or equal to the size of the given array. Note that if the array has repeating elements, then the sample may have repeats as well.
pub fn sample_nr<T>(array []T, k int) []T {
	n := array.len
	if k > n {
		panic('Cannot sample $k elements without replacement from a $n-element array.')
	}
	mut results := []T{len: k}
	mut indices := []int{len: n}
	// Initialize with all indices
	for i, mut v in indices {
		v = i
	}
	arrays.shuffle<int>(mut indices, k)
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
