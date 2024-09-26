module builtin

// str returns the value of the `int` as a `string`.
// Example: assert int(-2020).str() == '-2020'
pub fn int_str(n int) string {
	return i64(n).str()
}
