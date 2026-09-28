@[has_globals; translated]
module decay

// pick returns a pointer into the array parameter's original storage.
pub fn pick(values [3]int) &int {
	return values + 1
}

// pick_offset also exercises argument lowering around a value branch.
pub fn pick_offset(values [3]int, offset int) &int {
	return values + offset
}

pub struct Picker {}

// pick checks the same parameter storage rule for receiver methods.
pub fn (_ Picker) pick(values [3]int) &int {
	return values + 1
}

// keep exercises retention through a void interface method.
pub fn (_ Picker) keep(values [3]int) {
	retained_pointer = values + 1
}

__global retained_pointer = unsafe { &int(nil) }

// pick_after checks source ordering around a fixed-array argument.
pub fn pick_after(prefix int, values [3]int, offset int) &int {
	assert prefix == 17
	return values + offset
}

// retain keeps the parameter storage without returning a pointer.
pub fn retain(values [3]int) {
	retained_pointer = values + 1
}

// read_retained reads the value after its caller has left the source scope.
pub fn read_retained() int {
	return unsafe { *retained_pointer }
}
