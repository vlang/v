module builtin

pub struct string {
pub:
	str &byte
	len int
}

pub fn strlen(s &byte) int {
	mut i := 0
	for ; s[i] != 0; i++ {}
	return i
}

pub fn tos(s &byte, len int) string {
	if s == 0 {
		panic('tos(): nil string')
	}
	return string{
		str: s
		len: len
	}
}

fn (s string) add(a string) string {
	new_len := a.len + s.len
	mut res := string{
		len: new_len
		str: malloc(new_len + 1)
	}
	for j in 0 .. s.len {
		res[j] = s[j]
	}
	for j in 0 .. a.len {
		res[s.len + j] = a[j]
	}
	res[new_len] = 0 // V strings are not null terminated, but just in case
	return res
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
pub fn tos2(s &byte) string {
	if s == 0 {
		panic('tos2: nil string')
	}
	return string{
		str: s
		len: strlen(s)
	}
}

pub fn tos3(s &char) string {
	if s == 0 {
		panic('tos3: nil string')
	}
	return string{
		str: &u8(s)
		len: strlen(&u8(s))
	}
}

pub fn string_eq(s1 string, s2 string) bool {
	if s1.len != s2.len {
		return false
	}
	for i in 0 .. s1.len {
		if s1[i] != s2[i] {
			return false
		}
	}
	return true
}

pub fn string_ne(s1 string, s2 string) bool {
	return !string_eq(s1, s2)
}

pub fn i64_tos(buf &byte, len int, n0 i64, base int) string {
	if base < 2 {
		panic('base must be >= 2')
	}
	if base > 36 {
		panic('base must be <= 36')
	}

	mut b := tos(buf, len)
	mut i := len - 1

	mut n := n0
	neg := n < 0
	if neg {
		n = -n
	}

	b[i--] = 0

	for {
		c := (n % base) + 48
		b[i--] = if c > 57 { c + 7 } else { c }
		if i < 0 {
			panic('buffer to small')
		}
		n /= base
		if n < 1 {
			break
		}
	}
	if neg {
		if i < 0 {
			panic('buffer to small')
		}
		b[i--] = 45
	}
	offset := i + 1
	b.str = b.str + offset
	b.len -= (offset + 1)
	return b
}

pub fn i64_str(n0 i64, base int) string {
	buf := malloc(80)
	return i64_tos(buf, 79, n0, base)
}

pub fn ptr_str(ptr voidptr) string {
	buf := [16]u8{}
	hex := i64_tos(buf, 15, i64(ptr), 16)
	res := '0x' + hex
	return res
}

pub fn (a string) clone() string {
	mut b := string{
		len: a.len
		str: malloc(a.len + 1)
	}
	mem_copy(b.str, a.str, a.len)
	b[a.len] = 0
	return b
}
