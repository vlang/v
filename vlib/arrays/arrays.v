module arrays
import rand

// min returns the minimum
[direct_array_access]
pub fn min<T>(a []T) T {
	if a.len==0 { panic('.min called on an empty array') } // TODO
	mut val := a[0]
	for i in 0..a.len {
		if a[i] < val {
			val = a[i]
		}
	}
	return val
}

// max returns the maximum
[direct_array_access]
pub fn max<T>(a []T) T {
	if a.len==0 { panic('.max called on an empty array') } // TODO
	mut val := a[0]
	for i in 0..a.len {
		if a[i] > val {
			val = a[i]
		}
	}
	return val
}

// argmax returns the index of the first minimum
[direct_array_access]
pub fn argmin<T>(a []T) int {
	if a.len==0 { panic('.argmin called on an empty array') } // TODO
	mut idx := 0
	mut val := a[0]
	for i in 0..a.len {
		if a[i] < val {
			val = a[i]
			idx = i
		}
	}
	return idx
}

// argmax returns the index of the first maximum
[direct_array_access]
pub fn argmax<T>(a []T) int {
	if a.len==0 { panic('.argmax called on an empty array') } // TODO
	mut idx := 0
	mut val := a[0]
	for i in 0..a.len {
		if a[i] > val {
			val = a[i]
			idx = i
		}
	}
	return idx
}

// shuffle randomizes the items of an array in place
[direct_array_access]
pub fn shuffle<T>(mut a []T) {
	for i in 0..a.len-1 {
		x := rand.int_in_range(i,a.len)
		if i != x {
			a[i],a[x] = a[x],a[i] // swap
		}
	}
}

// all checks if all array items are equal to given value
[direct_array_access]
pub fn all<T>(a []T, value T) bool {
	if a.len == 0 { return false }
	for i in 0..a.len {
		if a[i] != value { return false }
	}
	return true
}


/*
// replace values given in old_value with new_value
[direct_array_access]
pub fn replace<T>(mut a[]T, old_value T, new_value T) {
	for i in 0..a.len {
		if a[i] == old_value {
			a[i] = new_value
		}
	}
}
*/
