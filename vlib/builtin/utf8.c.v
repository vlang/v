module builtin

const (
	cp_utf8 = 65001
)

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

[unsafe]
pub fn string_from_wide(_wstr &u16) string {
	$if windows {
		unsafe {
			wstr_len := C.wcslen(_wstr)
			return string_from_wide2(_wstr, wstr_len)
		}
	} $else {
		return ''
	}
}

[unsafe]
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
		return ''
	}
}
