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


pub fn repeat_string(s string, n int) string {
	if n <= 0 {
		return ''
	}
	slen := s.len
	blen := s.len*n
	mut bytes := &byte(0)
	unsafe { bytes = malloc(blen + 1) }
	for bi in 0..n {
		for si in 0..slen {
			bytes[bi*slen+si] = s[si]
		}
	}
	bytes[blen] = `0`
	return string( bytes, blen )
}
