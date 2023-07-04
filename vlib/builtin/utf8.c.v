module builtin

import strings

const cp_utf8 = 65001

// to_wide returns a pointer to an UTF-16 version of the string receiver.
// In V, strings are encoded using UTF-8 internally, but on windows most APIs,
// that accept strings, need them to be in UTF-16 encoding.
// The returned pointer of .to_wide(), has a type of &u16, and is suitable
// for passing to Windows APIs that expect LPWSTR or wchar_t* parameters.
// See also MultiByteToWideChar ( https://learn.microsoft.com/en-us/windows/win32/api/stringapiset/nf-stringapiset-multibytetowidechar )
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
			return result
		}
	}
}

// string_from_wide creates a V string, encoded in UTF-8, given a windows
// style string encoded in UTF-16.
[manualfree; unsafe]
pub fn string_from_wide(_wstr &u16) string {
	$if windows {
		unsafe {
			wstr_len := C.wcslen(_wstr)
			return string_from_wide2(_wstr, wstr_len)
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
[manualfree; unsafe]
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
		defer {
			unsafe { sb.free() }
		}
		for i := 0; i < len; i++ {
			u := unsafe { rune(_wstr[i]) }
			sb.write_rune(u)
		}
		res := sb.str()
		return res
	}
}
