module strings

// compare returns -1, 0, or 1 depending on lexicographic order.
pub fn compare(a string, b string) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}
