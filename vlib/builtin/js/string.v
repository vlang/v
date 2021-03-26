module builtin

pub struct string {
pub:
	str JS.String
	len u32
}

pub fn (s string) slice(a int, b int) string {
	return string(s.str.slice(a, b))
}

pub fn (s string) after(dot string) string {
	return string(s.str.slice(s.str.lastIndexOf(dot.str) + 1, int(s.str.length)))
}

pub fn (s string) after_char(dot byte) string {
	// TODO: Implement after byte
	return s
}

pub fn (s string) all_after(dot string) string {
	return string(s.str.slice(s.str.indexOf(dot.str) + 1, int(s.str.length)))
}

// why does this exist?
pub fn (s string) all_after_last(dot string) string {
	return s.after(dot)
}

pub fn (s string) all_before(dot string) string {
	return string(s.str.slice(0, s.str.indexOf(dot.str)))
}

pub fn (s string) all_before_last(dot string) string {
	return string(s.str.slice(0, s.str.lastIndexOf(dot.str)))
}

pub fn (s string) bool() bool {
	return s == 'true'
}

pub fn (s string) split(dot string) []string {
	return s.str.split(dot.str).map(string(it))
}

pub fn (s string) bytes() []byte {
	sep := ''
	return s.str.split(sep.str).map(it.charCodeAt(0))
}

pub fn (s string) capitalize() string {
	part := string(s.str.slice(1, int(s.str.length)))
	return string(s.str.charAt(0).toUpperCase().concat(part.str))
}

pub fn (s string) clone() string {
	return string(s.str)
}

pub fn (s string) contains(substr string) bool {
	return s.str.includes(substr.str)
}

pub fn (s string) contains_any(chars string) bool {
	sep := ''
	for x in chars.str.split(sep.str) {
		if s.str.includes(x) {
			return true
		}
	}
	return false
}

pub fn (s string) contains_any_substr(chars []string) bool {
	for x in chars {
		if s.str.includes(x.str) {
			return true
		}
	}
	return false
}

pub fn (s string) count(substr string) int {
	// TODO: "error: `[]JS.String` is not a struct" when returning arr.length or arr.len
	arr := s.str.split(substr.str)
	return native_str_arr_len(arr)
}

pub fn (s string) ends_with(p string) bool {
	return s.str.ends_with(p.str)
}

pub fn (s string) starts_with(p string) bool {
	return s.str.starts_with(p.str)
}

pub fn (s string) fields() []string {
	return [] // s.str.split()
}

pub fn (s string) find_between(start string, end string) string {
	return string(s.str.slice(s.str.indexOf(start.str), s.str.indexOf(end.str) + 1))
}

// unnecessary in the JS backend, implemented for api parity.
pub fn (s string) free() {}

pub fn (s string) hash() int {
	mut h := u32(0)
	if h == 0 && s.len > 0 {
		for c in s {
			h = h * 31 + u32(c)
		}
	}
	return int(h)
}

// int returns the value of the string as an integer `'1'.int() == 1`.
pub fn (s string) int() int {
	return int(JS.parseInt(s))
}

// i64 returns the value of the string as i64 `'1'.i64() == i64(1)`.
pub fn (s string) i64() i64 {
	return i64(JS.parseInt(s))
}

// i8 returns the value of the string as i8 `'1'.i8() == i8(1)`.
pub fn (s string) i8() i8 {
	return i8(JS.parseInt(s))
}

// i16 returns the value of the string as i16 `'1'.i16() == i16(1)`.
pub fn (s string) i16() i16 {
	return i16(JS.parseInt(s))
}

// f32 returns the value of the string as f32 `'1.0'.f32() == f32(1)`.
pub fn (s string) f32() f32 {
	// return C.atof(charptr(s.str))
	return f32(JS.parseFloat(s))
}

// f64 returns the value of the string as f64 `'1.0'.f64() == f64(1)`.
pub fn (s string) f64() f64 {
	return f64(JS.parseFloat(s))
}

// u16 returns the value of the string as u16 `'1'.u16() == u16(1)`.
pub fn (s string) u16() u16 {
	return u16(JS.parseInt(s))
}

// u32 returns the value of the string as u32 `'1'.u32() == u32(1)`.
pub fn (s string) u32() u32 {
	return u32(JS.parseInt(s))
}

// u64 returns the value of the string as u64 `'1'.u64() == u64(1)`.
pub fn (s string) u64() u64 {
	return u64(JS.parseInt(s))
}
