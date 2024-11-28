module wchar

import strings

#include <wchar.h>

@[typedef]
pub struct C.wchar_t {}

// Character is a type, that eases working with the platform dependent C.wchar_t type.
// Note: the size of C.wchar_t varies between platforms, it is 2 bytes on windows,
// and usually 4 bytes elsewhere.
pub type Character = C.wchar_t

// zero is a Character, that in C L"" strings represents the string end character (terminator).
pub const zero = from_rune(0)

// return a string representation of the given Character
pub fn (a Character) str() string {
	return a.to_rune().str()
}

// == is an equality operator, to ease comparing Characters
// TODO: the default == operator, that V generates, does not work for C.wchar_t .
@[inline]
pub fn (a Character) == (b Character) bool {
	return u64(a) == u64(b)
}

// to_rune creates a V rune, given a Character
@[inline]
pub fn (c Character) to_rune() rune {
	$if windows {
		return unsafe { *(&rune(&c)) } & 0xFFFF
	} $else {
		return unsafe { *(&rune(&c)) }
	}
}

// from_rune creates a Character, given a V rune
@[inline]
pub fn from_rune(r rune) Character {
	return unsafe { *(&Character(&r)) }
}

// length_in_characters returns the length of the given wchar_t* wide C style L"" string.
// Example: assert unsafe { wchar.length_in_characters(wchar.from_string('abc')) } == 3
// See also `length_in_bytes` .
@[unsafe]
pub fn length_in_characters(p voidptr) int {
	mut len := 0
	pc := &Character(p)
	for unsafe { pc[len] != zero } {
		len++
	}
	return len
}

// length_in_bytes returns the length of the given wchar_t* wide C style L"" string in bytes.
// Note that the size of wchar_t is different on the different platforms, thus the length in
// bytes for the same data converted from UTF-8 to a &Character buffer, will be different as well.
// i.e. unsafe { wchar.length_in_bytes(wchar.from_string('abc')) } will be 12 on unix, but
// 6 on windows.
@[unsafe]
pub fn length_in_bytes(p voidptr) int {
	return unsafe { length_in_characters(p) } * int(sizeof(Character))
}

// to_string creates a V string, encoded in UTF-8, given a wchar_t*
// wide C style L"" string. It relies that the string has a 0 terminator at its end,
// to determine the string's length.
// Note, that the size of wchar_t is platform-dependent, and is *2 bytes* on windows,
// while it is *4 bytes* on most everything else.
// Unless you are interfacing with a C library, that does specifically use `wchar_t`,
// consider using `string_from_wide` instead, which will always assume that the input
// data is in an UTF-16 encoding, no matter what the platform is.
@[unsafe]
pub fn to_string(p voidptr) string {
	unsafe {
		len := length_in_characters(p)
		return to_string2(p, len)
	}
}

// to_string2 creates a V string, encoded in UTF-8, given a `C.wchar_t*`
// wide C style L"" string. Note, that the size of `C.wchar_t` is platform-dependent,
// and is *2 bytes* on windows, while *4* on most everything else.
// Unless you are interfacing with a C library, that does specifically use wchar_t,
// consider using string_from_wide2 instead, which will always assume that the input
// data is in an UTF-16 encoding, no matter what the platform is.
@[manualfree; unsafe]
pub fn to_string2(p voidptr, len int) string {
	pc := &Character(p)
	mut sb := strings.new_builder(len)
	defer {
		unsafe { sb.free() }
	}
	for i := 0; i < len; i++ {
		u := unsafe { rune(pc[i]) }
		sb.write_rune(u)
	}
	res := sb.str()
	return res
}

// from_string converts the V string (in UTF-8 encoding), into a newly allocated
// platform specific buffer of C.wchar_t .
// The conversion is done by processing each rune of the input string 1 by 1.
@[manualfree]
pub fn from_string(s string) &Character {
	srunes := s.runes()
	unsafe {
		mut result := &Character(vcalloc_noscan((srunes.len + 1) * int(sizeof(Character))))
		for i, r in srunes {
			result[i] = from_rune(r)
		}
		result[srunes.len] = zero
		return result
	}
}
