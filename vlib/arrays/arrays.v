module arrays

// Common arrays functions:
// - min / max - return the value of the minumum / maximum
// - idx_min / idx_max - return the index of the first minumum / maximum
// - merge - combine two sorted arrays and maintain sorted order

// min returns the minimum
[direct_array_access]
pub fn min<T>(a []T) T {
	if a.len == 0 {
		panic('.min called on an empty array')
	}
	mut val := a[0]
	for i in 0 .. a.len {
		if a[i] < val {
			val = a[i]
		}
	}
	return val
}

// max returns the maximum
[direct_array_access]
pub fn max<T>(a []T) T {
	if a.len == 0 {
		panic('.max called on an empty array')
	}
	mut val := a[0]
	for i in 0 .. a.len {
		if a[i] > val {
			val = a[i]
		}
	}
	return val
}

// idx_min returns the index of the first minimum
[direct_array_access]
pub fn idx_min<T>(a []T) int {
	if a.len == 0 {
		panic('.idxmin called on an empty array')
	}
	mut idx := 0
	mut val := a[0]
	for i in 0 .. a.len {
		if a[i] < val {
			val = a[i]
			idx = i
		}
	}
	return idx
}

// idx_max returns the index of the first maximum
[direct_array_access]
pub fn idx_max<T>(a []T) int {
	if a.len == 0 {
		panic('.idxmax called on an empty array')
	}
	mut idx := 0
	mut val := a[0]
	for i in 0 .. a.len {
		if a[i] > val {
			val = a[i]
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
