module strings

pub fn repeat(c byte, n int) string {
	if n <= 0 {
		return ''
	}
	mut arr := [c].repeat(n + 1)
	arr[n] = `\0`
	return string(arr,n)
}

pub fn repeat_string(s string, n int) string {
	res := # s.repeat(n)
	return res
}
