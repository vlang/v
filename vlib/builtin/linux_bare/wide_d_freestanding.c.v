module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_wide_abi.h"
	fn C.v_wide_codeset(uint) uint
	fn C.v_wide_state(i32) uint
	fn C.v_wide_decode(uint, uint, uint, uint) uint
	fn C.v_wide_encode(uint, u32, uint) uint

	fn bare_wide_utf8(locale uint) bool {
		codeset := unsafe { tos2(&u8(C.v_wide_codeset(locale))) }
		return codeset == 'UTF-8'
	}
	// Predicate and printable ranges follow musl's C.UTF-8 policy (MIT).
	fn bare_wide_space(value u32) bool {
		return value == 32 || (value >= 9 && value <= 13) || value == 0x85
			|| (value >= 0x2000 && value <= 0x2006) || (value >= 0x2008 && value <= 0x200a)
			|| value == 0x2028 || value == 0x2029 || value == 0x205f || value == 0x3000
	}
	fn bare_wide_print(value u32) bool {
		return value >= 32 && !(value >= 127 && value <= 159) && value <= 0x10ffff
			&& !(value >= 0xd800 && value <= 0xdfff) && value != 0x2028 && value != 0x2029
			&& !(value >= 0xfff9 && value <= 0xfffb) && value & 0xfffe != 0xfffe
	}
	@[export: 'v_wide_class'; markused]
	fn bare_wide_class(value u32, kind uint, locale uint) i32 {
		if value > 0x10ffff { return 0 }
		unicode := value < 128 || bare_wide_utf8(locale)
		if !unicode { return 0 }
		letter := bare_wide_letter(value)
		digit := value >= `0` && value <= `9`
		space := bare_wide_space(value)
		printable := bare_wide_print(value)
		graph := printable && !space
		return i32(match kind {
			1 { letter || digit }
			2 { letter }
			3 {
				space && value != 10 && value != 13 && value != 11 && value != 12 && value != 0x85 && value != 0x2028 && value != 0x2029
			}
			4 { value < 32 || (value >= 127 && value <= 159) || value == 0x2028 || value == 0x2029 }
			5 { digit }
			6 { graph }
			7 { bare_wide_lower(value) }
			8 { printable }
			9 { graph && !letter && !digit }
			10 { space }
			11 { bare_wide_upper(value) }
			12 { digit || (value >= `a` && value <= `f`) || (value >= `A` && value <= `F`) }
			else { false }
		})
	}
	@[export: 'v_wide_class_name'; markused]
	fn bare_wide_class_name(address uint) uint {
		name := unsafe { tos2(&u8(address)) }
		return bare_wide_class_id(name)
	}
	fn bare_wide_class_id(name string) uint {
		return match name {
			'alnum' { uint(1) }
			'alpha' { uint(2) }
			'blank' { uint(3) }
			'cntrl' { uint(4) }
			'digit' { uint(5) }
			'graph' { uint(6) }
			'lower' { uint(7) }
			'print' { uint(8) }
			'punct' { uint(9) }
			'space' { uint(10) }
			'upper' { uint(11) }
			'xdigit' { uint(12) }
			else { uint(0) }
		}
	}
	@[export: 'v_wide_case'; markused]
	fn bare_wide_case(value u32, upper i32, locale uint) u32 {
		if value > 0x10ffff || (value >= 128 && !bare_wide_utf8(locale)) { return value }
		return u32(if upper != 0 { rune(value).to_upper() } else { rune(value).to_lower() })
	}
	@[export: 'v_wide_width'; markused]
	fn bare_wide_width(value u32) i32 {
		if value == 0 { return 0 }
		if bare_wide_class(value, 8, 0) == 0 { return -1 }
		return i32(utf8_rune_visible_width(rune(value), grapheme_break_property(rune(value))))
	}
	@[export: 'v_wide_length'; markused]
	fn bare_wide_length(address uint, limit uint) uint {
		mut length := uint(0)
		for length < limit && unsafe { (&u32(address))[length] } != 0 { length++ }
		return length
	}
	@[export: 'v_wide_compare'; markused]
	fn bare_wide_compare(left uint, right uint, limit uint, nul i32) i32 {
		for i := uint(0); i < limit; i++ {
			a := unsafe { (&u32(left))[i] }
			b := unsafe { (&u32(right))[i] }
			if a != b {
				less := if C.WCHAR_MIN < 0 { i32(a) < i32(b) } else { a < b }
				return if less { i32(-1) } else { i32(1) }
			}
			if nul != 0 && a == 0 { break }
		}
		return 0
	}
	@[export: 'v_wide_find'; markused]
	fn bare_wide_find(address uint, value u32, limit uint, reverse i32, nul i32) uint {
		mut found := uint(0)
		for i := uint(0); i < limit; i++ {
			character := unsafe { (&u32(address))[i] }
			if character == value {
				found = address + i * 4
				if reverse == 0 { break }
			}
			if nul != 0 && character == 0 { break }
		}
		return found
	}
	@[export: 'v_wide_fill'; markused]
	fn bare_wide_fill(output uint, value u32, count uint) uint {
		for i := uint(0); i < count; i++ {
			unsafe { (&u32(output))[i] = value }
		}
		return output
	}
	@[export: 'v_wide_transform'; markused]
	fn bare_wide_transform(output uint, source uint, count uint) uint {
		length := bare_wide_length(source, ~uint(0))
		copied := if count > length { length + 1 } else { count }
		if copied != 0 { unsafe { vmemcpy(voidptr(output), voidptr(source), int(copied * 4)) } }
		return length
	}
	@[export: 'v_wide_single_byte'; markused]
	fn bare_wide_single_byte(value u32) i32 {
		return if value < 128 { i32(value) } else { i32(-1) }
	}
	@[export: 'v_wide_mbsnrtowcs'; markused]
	fn bare_wide_mbsnrtowcs(output uint, source uint, bytes uint, limit uint, state_value uint) uint {
		mut state := if state_value == 0 { C.v_wide_state(0) } else { state_value }
		mut query := [8]u8{}
		if output == 0 {
			unsafe { vmemcpy(&query[0], voidptr(state), 8) }
			state = uint(&query[0])
		}
		mut input := unsafe { *(&uint(source)) }
		mut remaining := bytes
		mut count := uint(0)
		for remaining > 0 && (output == 0 || count < limit) {
			mut value := u32(0)
			used := C.v_wide_decode(uint(&value), input, remaining, state)
			if used == ~uint(0) {
				if output != 0 { unsafe { *(&uint(source)) = input } }
				return used
			}
			if used == ~uint(0) - 1 {
				if output != 0 { unsafe { *(&uint(source)) = input + remaining } }
				return count
			}
			if output != 0 { unsafe { (&u32(output))[count] = value } }
			if used == 0 {
				if output != 0 { unsafe { *(&uint(source)) = 0 } }
				return count
			}
			count++
			input += used
			remaining -= used
		}
		if output != 0 { unsafe { *(&uint(source)) = input } }
		return count
	}
	@[export: 'v_wide_wcsnrtombs'; markused]
	fn bare_wide_wcsnrtombs(output uint, source uint, characters uint, limit uint, state_value uint) uint {
		mut state := if state_value == 0 { C.v_wide_state(1) } else { state_value }
		mut query := [8]u8{}
		if output == 0 {
			unsafe { vmemcpy(&query[0], voidptr(state), 8) }
			state = uint(&query[0])
		}
		mut input := unsafe { *(&uint(source)) }
		mut remaining := characters
		mut count := uint(0)
		for remaining > 0 && (output == 0 || count < limit) {
			value := unsafe { *(&u32(input)) }
			mut buffer := [8]u8{}
			mut next := [8]u8{}
			unsafe { vmemcpy(&next[0], voidptr(state), 8) }
			size := C.v_wide_encode(uint(&buffer[0]), value, uint(&next[0]))
			if size == ~uint(0) {
				if output != 0 { unsafe { *(&uint(source)) = input } }
				return size
			}
			if output != 0 && size > limit - count {
				unsafe { *(&uint(source)) = input }
				return count
			}
			if count > ~uint(0) - size {
				bf_native_errno_cell = i32(C.EOVERFLOW)
				return ~uint(0)
			}
			if output != 0 { unsafe { vmemcpy(voidptr(output + count), &buffer[0], int(size)) } }
			unsafe { vmemcpy(voidptr(state), &next[0], 8) }
			if value == 0 {
				if output != 0 { unsafe { *(&uint(source)) = 0 } }
				return count + size - 1
			}
			count += size
			input += 4
			remaining--
		}
		if output != 0 { unsafe { *(&uint(source)) = input } }
		return count
	}
}
