module strings

pub fn repeat(c byte, n int) string {
	if n <= 0 {
		return ''
	}
	arr := [c].repeat(n)
	return arr.bytestr()
}

pub fn repeat_string(s string, n int) string {
	/*
	// TODO: uncomment this. It is commented for now, so that `v doc strings` works
	res := # s.repeat(n)
	return res
	*/
}
