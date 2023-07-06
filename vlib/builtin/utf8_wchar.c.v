module builtin

import strings

// string_from_wchar creates a V string, encoded in UTF-8, given a wchar_t*
// wide C style L"" string. It relies that the string has a 0 terminator
// at its end, to know its length.
// Note, that the size of wchar_t is platform-dependent,
// and is 2 bytes on windows, while 4 on most everything else.
// Unless you are interfacing with a C library, that does specifically use `wchar_t`,
// consider using `string_from_wide` instead.
[manualfree; unsafe]
pub fn string_from_wchar(_wstr voidptr) string {
	mut len := 0
	$if windows {
		p2 := &u16(_wstr)
		for unsafe { p2[len] } != 0 {
			len++
		}
	} $else {
		p4 := &u32(_wstr)
		for unsafe { p4[len] } != 0 {
			len++
		}
	}
	return unsafe { string_from_wchar2(_wstr, len) }
}

// string_from_wide2 creates a V string, encoded in UTF-8, given a wchar_t*
// wide C style L"" string. Note, that the size of wchar_t is platform-dependent,
// and is 2 bytes on windows, while 4 on most everything else.
// Unless you are interfacing with a C library, that does specifically use wchar_t,
// consider using string_from_wide2 instead.
[manualfree; unsafe]
pub fn string_from_wchar2(_wstr voidptr, len int) string {
	p2 := &u16(_wstr)
	p4 := &u32(_wstr)
	mut sb := strings.new_builder(len)
	defer {
		unsafe { sb.free() }
	}
	mut u := rune(0)
	for i := 0; i < len; i++ {
		$if windows {
			u = unsafe { rune(p2[i]) }
		} $else {
			u = unsafe { rune(p4[i]) }
		}
		sb.write_rune(u)
	}
	res := sb.str()
	return res
}
