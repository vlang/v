module strings

pub fn repeat(c byte, n int) string {
	if n <= 0 {
		return ''
	}
	mut bytes := &byte(0)
	unsafe { bytes = malloc(n + 1) }
	C.memset( bytes, c, n )
	bytes[n] = `0`
	return string( bytes, n )
}
