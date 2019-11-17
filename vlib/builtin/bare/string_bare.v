module builtin

pub struct string {
pub:
	str byteptr
	len int
}

pub fn strlen(s byteptr) int {
	mut i := 0
	for ; s[i] != 0; i++ {}	
	return i
}	

pub fn tos(s byteptr, len int) string {
	if s == 0 {
		panic('tos(): nil string')
	}
	return string {
		str: s
		len: len
	}
}

/*
pub fn tos_clone(s byteptr) string {
	if s == 0 {
		panic('tos: nil string')
	}
	return tos2(s).clone()
}
*/

// Same as `tos`, but calculates the length. Called by `string(bytes)` casts.
// Used only internally.
pub fn tos2(s byteptr) string {
	if s == 0 {
		panic('tos2: nil string')
	}
	return string {
		str: s
		len: strlen(s)
	}
}

pub fn tos3(s *C.char) string {
	if s == 0 {
		panic('tos3: nil string')
	}
	return string {
		str: byteptr(s)
		len: strlen(byteptr(s))
	}
}

/*
pub fn (a string) clone() string {
	mut b := string {
		len: a.len
		str: malloc(a.len + 1)
	}
	for i := 0; i < a.len; i++ {
		b[i] = a[i]
	}
	b[a.len] = `\0`
	return b
}
*/
