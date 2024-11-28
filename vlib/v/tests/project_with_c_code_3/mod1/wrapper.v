module mod1

// Note: the function here, should be overridden by the one in the wrapper.c.v file with the same name
pub fn vadd(a int, b int) int {
	return 123456
}

// this should NOT be overridden by the different wrapper.X.v files:
pub fn a_common_pure_v_fn() int {
	return 987654
}
