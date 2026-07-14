module builtin

import strings

const cp_acp = 0
const cp_utf8 = 65001

@[params]
pub struct ToWideConfig {
pub:
	from_ansi bool
}

// to_wide returns a pointer to an UTF-16 version of the string receiver.
// In V, strings are encoded using UTF-8 internally, but on windows most APIs,
// that accept strings, need them to be in UTF-16 encoding.
// The returned pointer of .to_wide(), has a type of &u16, and is suitable
// for passing to Windows APIs that expect LPWSTR or wchar_t* parameters.
// See also MultiByteToWideChar ( https://learn.microsoft.com/en-us/windows/win32/api/stringapiset/nf-stringapiset-multibytetowidechar )
// See also builtin.wchar.from_string/1, for a version, that produces a
// platform dependant L"" C style wchar_t* wide string.
pub fn (_str string) to_wide(param ToWideConfig) &u16 {
	$if windows {
		if !param.from_ansi {
			return wtf8_to_wide(_str)
		}
		unsafe {
			num_chars := (C.MultiByteToWideChar(cp_acp, 0, &char(_str.str), _str.len, 0, 0))
			mut wstr := &u16(malloc_noscan((num_chars + 1) * 2)) // sizeof(wchar_t)
			if wstr != 0 {
				C.MultiByteToWideChar(cp_acp, 0, &char(_str.str), _str.len, wstr, num_chars)
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
		return wtf8_from_wide(_wstr, len)
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

// wtf8_to_wide decodes V's Windows path representation. Unlike strict UTF-8,
// WTF-8 preserves unpaired UTF-16 surrogates used by Windows file names.
fn wtf8_to_wide(value string) &u16 {
	unsafe {
		mut out := &u16(malloc_noscan((value.len + 1) * 2))
		mut src := 0
		mut dst := 0
		for src < value.len {
			b0 := value.str[src]
			mut codepoint := u32(0xfffd)
			mut width := 1
			if b0 < 0x80 {
				codepoint = b0
			} else if b0 >= 0xc2 && b0 <= 0xdf && src + 1 < value.len
				&& (value.str[src + 1] & 0xc0) == 0x80 {
				codepoint = (u32(b0 & 0x1f) << 6) | u32(value.str[src + 1] & 0x3f)
				width = 2
			} else if b0 >= 0xe0 && b0 <= 0xef && src + 2 < value.len
				&& (value.str[src + 1] & 0xc0) == 0x80 && (value.str[src + 2] & 0xc0) == 0x80
				&& (b0 != 0xe0 || value.str[src + 1] >= 0xa0) {
				codepoint = (u32(b0 & 0x0f) << 12) | (u32(value.str[src + 1] & 0x3f) << 6) | u32(value.str[
					src + 2] & 0x3f)
				width = 3
			} else if b0 >= 0xf0 && b0 <= 0xf4 && src + 3 < value.len
				&& (value.str[src + 1] & 0xc0) == 0x80 && (value.str[src + 2] & 0xc0) == 0x80
				&& (value.str[src + 3] & 0xc0) == 0x80 && (b0 != 0xf0 || value.str[src + 1] >= 0x90)
				&& (b0 != 0xf4 || value.str[src + 1] <= 0x8f) {
				codepoint = (u32(b0 & 0x07) << 18) | (u32(value.str[src + 1] & 0x3f) << 12) | (u32(value.str[
					src + 2] & 0x3f) << 6) | u32(value.str[src + 3] & 0x3f)
				width = 4
			}
			src += width
			if codepoint <= 0xffff {
				out[dst] = u16(codepoint)
				dst++
			} else {
				adjusted := codepoint - 0x10000
				out[dst] = u16(0xd800 + (adjusted >> 10))
				out[dst + 1] = u16(0xdc00 + (adjusted & 0x3ff))
				dst += 2
			}
		}
		out[dst] = 0
		return out
	}
}

// wtf8_from_wide encodes Windows UTF-16 without losing unpaired surrogates.
@[unsafe]
fn wtf8_from_wide(value &u16, len int) string {
	mut out := &u8(malloc_noscan(len * 3 + 1))
	mut src := 0
	mut dst := 0
	for src < len {
		first := unsafe { value[src] }
		mut codepoint := u32(first)
		if first >= 0xd800 && first <= 0xdbff && src + 1 < len {
			second := unsafe { value[src + 1] }
			if second >= 0xdc00 && second <= 0xdfff {
				codepoint = 0x10000 + ((u32(first) - 0xd800) << 10) + (u32(second) - 0xdc00)
				src++
			}
		}
		src++
		unsafe {
			if codepoint < 0x80 {
				out[dst] = u8(codepoint)
				dst++
			} else if codepoint < 0x800 {
				out[dst] = u8(0xc0 | (codepoint >> 6))
				out[dst + 1] = u8(0x80 | (codepoint & 0x3f))
				dst += 2
			} else if codepoint < 0x10000 {
				out[dst] = u8(0xe0 | (codepoint >> 12))
				out[dst + 1] = u8(0x80 | ((codepoint >> 6) & 0x3f))
				out[dst + 2] = u8(0x80 | (codepoint & 0x3f))
				dst += 3
			} else {
				out[dst] = u8(0xf0 | (codepoint >> 18))
				out[dst + 1] = u8(0x80 | ((codepoint >> 12) & 0x3f))
				out[dst + 2] = u8(0x80 | ((codepoint >> 6) & 0x3f))
				out[dst + 3] = u8(0x80 | (codepoint & 0x3f))
				dst += 4
			}
		}
	}
	unsafe {
		out[dst] = 0
		return tos(out, dst)
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
			C.WideCharToMultiByte(cp_acp, 0, _wstr, -1, &char(str_to.data), str_to.len, 0, 0)
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
