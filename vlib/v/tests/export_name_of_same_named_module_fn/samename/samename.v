module samename

// sum has the name of a function that the main module exports, but it is not
// exported itself.
pub fn sum(a int, b int) int {
	return a + b + 100
}
