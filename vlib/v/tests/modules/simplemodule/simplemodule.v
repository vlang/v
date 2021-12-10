module simplemodule

pub fn iadd(x int, y int) int {
	return x + y
}

pub fn imul(x int, y int) int {
	return x * y
}

pub struct ThisIsGeneric<T> {
	msg T
}

pub struct Data {
pub:
	value int
}

pub const zero = 0
