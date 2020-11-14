module strings

// strings.repeat - fill a string with `n` repetitions of the character `c`
pub fn repeat(c byte, n int) string {
	if n <= 0 {
		return ''
	}
	mut bytes := unsafe {malloc(n + 1)}
	unsafe {
		C.memset( bytes, c, n )
		bytes[n] = `0`
	}
	return unsafe { bytes.vstring_with_len(n) }
}

// strings.repeat_string - gives you `n` repetitions of the substring `s`
// NB: strings.repeat, that repeats a single byte, is between 2x
// and 24x faster than strings.repeat_string called for a 1 char string.
[deprecated]
pub fn repeat_string(s string, n int) string {
	eprintln('strings.repeat_string(s, n) is deprecated, use s.repeat(n) instead')
	return s.repeat(n)
}
