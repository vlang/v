module genericmodule

pub fn take<T>(a bool, b T, c T) T {
	if a {
		return b
	}
	return c
}
