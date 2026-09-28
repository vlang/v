@[has_globals; translated]
module decay

__global retained_pointer = unsafe { &int(nil) }

// pick preserves the storage of an array hidden by a generic parameter.
pub fn pick[T](values T) &int {
	return values + 1
}

// retain keeps the array storage without returning a pointer.
pub fn retain[T](values T) {
	retained_pointer = values + 1
}

// read_retained reads the array after its caller has left the source scope.
pub fn read_retained() int {
	return unsafe { *retained_pointer }
}
