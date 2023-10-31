module strings

pub fn repeat(c u8, n int) string {
	if n <= 0 {
		return ''
	}
	arr := [c].repeat(n)
	return arr.bytestr()
}

pub fn repeat_string(s string, n int) string {
	res := ''
	#res.str = s.str.repeat(n.valueOf())

	return res
}
