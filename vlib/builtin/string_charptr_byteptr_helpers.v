module builtin

// Note: this file will be removed soon

// byteptr.vbytes() - makes a V []byte structure from a C style memory buffer. Note: the data is reused, NOT copied!
[unsafe]
pub fn (data byteptr) vbytes(len int) []byte {
	return unsafe { voidptr(data).vbytes(len) }
}

// vstring converts a C style string to a V string. Note: the string data is reused, NOT copied.
// strings returned from this function will be normal V strings beside that (i.e. they would be
// freed by V's -autofree mechanism, when they are no longer used).
[unsafe]
pub fn (bp byteptr) vstring() string {
	return string{
		str: bp
		len: unsafe { vstrlen(bp) }
	}
}

// vstring_with_len converts a C style string to a V string.
// Note: the string data is reused, NOT copied.
[unsafe]
pub fn (bp byteptr) vstring_with_len(len int) string {
	return string{
		str: bp
		len: len
		is_lit: 0
	}
}

// vstring converts C char* to V string.
// Note: the string data is reused, NOT copied.
[unsafe]
pub fn (cp charptr) vstring() string {
	return string{
		str: byteptr(cp)
		len: unsafe { vstrlen_char(cp) }
		is_lit: 0
	}
}

// vstring_with_len converts C char* to V string.
// Note: the string data is reused, NOT copied.
[unsafe]
pub fn (cp charptr) vstring_with_len(len int) string {
	return string{
		str: byteptr(cp)
		len: len
		is_lit: 0
	}
}

// vstring_literal converts a C style string to a V string.
// Note: the string data is reused, NOT copied.
// NB2: unlike vstring, vstring_literal will mark the string
// as a literal, so it will not be freed by autofree.
// This is suitable for readonly strings, C string literals etc,
// that can be read by the V program, but that should not be
// managed by it, for example `os.args` is implemented using it.
[unsafe]
pub fn (bp byteptr) vstring_literal() string {
	return string{
		str: bp
		len: unsafe { vstrlen(bp) }
		is_lit: 1
	}
}

// vstring_with_len converts a C style string to a V string.
// Note: the string data is reused, NOT copied.
[unsafe]
pub fn (bp byteptr) vstring_literal_with_len(len int) string {
	return string{
		str: bp
		len: len
		is_lit: 1
	}
}

// vstring_literal converts C char* to V string.
// See also vstring_literal defined on byteptr for more details.
// Note: the string data is reused, NOT copied.
[unsafe]
pub fn (cp charptr) vstring_literal() string {
	return string{
		str: byteptr(cp)
		len: unsafe { vstrlen_char(cp) }
		is_lit: 1
	}
}

// vstring_literal_with_len converts C char* to V string.
// See also vstring_literal_with_len defined on byteptr.
// Note: the string data is reused, NOT copied.
[unsafe]
pub fn (cp charptr) vstring_literal_with_len(len int) string {
	return string{
		str: byteptr(cp)
		len: len
		is_lit: 1
	}
}
