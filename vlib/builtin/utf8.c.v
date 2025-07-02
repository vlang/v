module builtin

import strings

const cp_acp = 0
const cp_utf8 = 65001

// to_wide returns a pointer to an UTF-16 version of the string receiver.
// In V, strings are encoded using UTF-8 internally, but on windows most APIs,
// that accept strings, need them to be in UTF-16 encoding.
// The returned pointer of .to_wide(), has a type of &u16, and is suitable
// for passing to Windows APIs that expect LPWSTR or wchar_t* parameters.
// See also MultiByteToWideChar ( https://learn.microsoft.com/en-us/windows/win32/api/stringapiset/nf-stringapiset-multibytetowidechar )
// See also builtin.wchar.from_string/1, for a version, that produces a
// platform dependant L"" C style wchar_t* wide string.
pub fn (_str string) to_wide() &u16 {
	$if windows {
		unsafe {
			num_chars := (C.MultiByteToWideChar(cp_utf8, 0, &char(_str.str), _str.len,
				0, 0))
			mut wstr := &u16(malloc_noscan((num_chars + 1) * 2)) // sizeof(wchar_t)
			if wstr != 0 {
				C.MultiByteToWideChar(cp_utf8, 0, &char(_str.str), _str.len, wstr, num_chars)
				C.memset(&u8(wstr) + num_chars * 2, 0, 2)
			}
			return wstr
		}
	} $else {
		srunes := _str.runes()
		unsafe {
			mut result := &u16(vcalloc_noscan((srunes.len + 1) * 2))
			for i, r in srunes {
				result[i] = u16(r)
			}
			result[srunes.len] = 0
			return result
		}
	}
}

// string_from_wide creates a V string, encoded in UTF-8, given a windows
// style string encoded in UTF-16. Note that this function first searches
// for the string terminator 0 character, and is thus slower, while more
// convenient compared to string_from_wide2/2 (you have to know the length
// in advance to use string_from_wide2/2).
// See also builtin.wchar.to_string/1, for a version that eases working with
// the platform dependent &wchar_t L"" strings.
@[manualfree; unsafe]
pub fn string_from_wide(_wstr &u16) string {
	$if windows {
		unsafe {
			wstr_len := C.wcslen(_wstr)
			return string_from_wide2(_wstr, int(wstr_len))
		}
	} $else {
		mut i := 0
		for unsafe { _wstr[i] } != 0 {
			i++
		}
		return unsafe { string_from_wide2(_wstr, i) }
	}
}

// string_from_wide2 creates a V string, encoded in UTF-8, given a windows
// style string, encoded in UTF-16. It is more efficient, compared to
// string_from_wide, but it requires you to know the input string length,
// and to pass it as the second argument.
// See also builtin.wchar.to_string2/2, for a version that eases working
// with the platform dependent &wchar_t L"" strings.
@[manualfree; unsafe]
pub fn string_from_wide2(_wstr &u16, len int) string {
	$if windows {
		unsafe {
			num_chars := C.WideCharToMultiByte(cp_utf8, 0, _wstr, len, 0, 0, 0, 0)
			mut str_to := malloc_noscan(num_chars + 1)
			if str_to != 0 {
				C.WideCharToMultiByte(cp_utf8, 0, _wstr, len, &char(str_to), num_chars,
					0, 0)
				C.memset(str_to + num_chars, 0, 1)
			}
			return tos2(str_to)
		}
	} $else {
		mut sb := strings.new_builder(len)
		for i := 0; i < len; i++ {
			u := unsafe { rune(_wstr[i]) }
			sb.write_rune(u)
		}
		res := sb.str()
		unsafe { sb.free() }
		return res
	}
}

// wide_to_ansi create an ANSI string, given a windows style string, encoded in UTF-16.
// It use CP_ACP, which is ANSI code page identifier, as dest encoding.
// NOTE: It return a vstring(encoded in UTF-8) []u8 under Linux.
pub fn wide_to_ansi(_wstr &u16) []u8 {
	$if windows {
		num_bytes := C.WideCharToMultiByte(cp_acp, 0, _wstr, -1, 0, 0, 0, 0)
		if num_bytes != 0 {
			mut str_to := []u8{len: num_bytes}
			C.WideCharToMultiByte(cp_acp, 0, _wstr, -1, &char(str_to.data), str_to.len,
				0, 0)
			return str_to
		} else {
			return []u8{}
		}
	} $else {
		s := unsafe { string_from_wide(_wstr) }
		mut str_to := []u8{len: s.len + 1}
		unsafe { vmemcpy(str_to.data, s.str, s.len) }
		return str_to
	}
	return []u8{} // TODO: remove this, bug?
}
