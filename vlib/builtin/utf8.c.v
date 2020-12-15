module builtin

const (
	cp_utf8 = 65001
)

pub fn (_str string) to_wide() &u16 {
	$if windows {
		num_chars := (C.MultiByteToWideChar(cp_utf8, 0, charptr(_str.str), _str.len, 0,
			0))
		mut wstr := &u16(malloc((num_chars + 1) * 2)) // sizeof(wchar_t)
		if wstr != 0 {
			C.MultiByteToWideChar(cp_utf8, 0, charptr(_str.str), _str.len, wstr, num_chars)
			unsafe {C.memset(&byte(wstr) + num_chars * 2, 0, 2)}
		}
		return wstr
	} $else {
		return 0
	}
}

pub fn string_from_wide(_wstr &u16) string {
	$if windows {
		wstr_len := C.wcslen(_wstr)
		return string_from_wide2(_wstr, wstr_len)
	} $else {
		return ''
	}
}

pub fn string_from_wide2(_wstr &u16, len int) string {
	$if windows {
		num_chars := C.WideCharToMultiByte(cp_utf8, 0, _wstr, len, 0, 0, 0, 0)
		mut str_to := malloc(num_chars + 1)
		if str_to != 0 {
			C.WideCharToMultiByte(cp_utf8, 0, _wstr, len, charptr(str_to), num_chars,
				0, 0)
			unsafe {C.memset(str_to + num_chars, 0, 1)}
		}
		return tos2(str_to)
	} $else {
		return ''
	}
}

// Reads an utf8 character from standard input
pub fn utf8_getchar() int {
	c := C.getchar()
	len := utf8_len(byte(~c))
	if c < 0 {
		return 0
	} else if len == 0 {
		return c
	} else if len == 1 {
		return -1
	} else {
		mut uc := c & ((1 << (7 - len)) - 1)
		for i := 0; i + 1 < len; i++ {
			c2 := C.getchar()
			if c2 != -1 && (c2 >> 6) == 2 {
				uc <<= 6
				uc |= (c2 & 63)
			} else if c2 == -1 {
				return 0
			} else {
				return -1
			}
		}
		return uc
	}
}
