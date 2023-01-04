module strings

// strings.repeat - fill a string with `n` repetitions of the character `c`
[direct_array_access]
pub fn repeat(c u8, n int) string {
	if n <= 0 {
		return ''
	}
	mut bytes := unsafe { malloc_noscan(n + 1) }
	unsafe {
		C.memset(bytes, c, n)
		bytes[n] = `0`
	}
	return unsafe { bytes.vstring_with_len(n) }
}

// strings.repeat_string - gives you `n` repetitions of the substring `s`
// Note: strings.repeat, that repeats a single byte, is between 2x
// and 24x faster than strings.repeat_string called for a 1 char string.
[direct_array_access]
pub fn repeat_string(s string, n int) string {
	if n <= 0 || s.len == 0 {
		return ''
	}
	slen := s.len
	blen := slen * n
	mut bytes := unsafe { malloc_noscan(blen + 1) }
	for bi in 0 .. n {
		bislen := bi * slen
		for si in 0 .. slen {
			unsafe {
				bytes[bislen + si] = s[si]
			}
		}
	}
	unsafe {
		bytes[blen] = `0`
	}
	return unsafe { bytes.vstring_with_len(blen) }
}
