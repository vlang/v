module enumfromsecond

enum Choice {
	second = 2
}

// parse returns the integer value of a valid Choice, or -1 for invalid input.
pub fn parse(value int) int {
	return int(Choice.from(value) or { return -1 })
}
