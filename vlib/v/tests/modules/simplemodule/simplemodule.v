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
