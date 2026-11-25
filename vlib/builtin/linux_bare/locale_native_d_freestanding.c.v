module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_locale_abi.h"
	// Native buffers below are caller-owned wchar_t/char storage. Bounds are
	// checked before raw accesses; struct tm remains opaque to V here.
	fn C.v_locale_select(uint) uint
	fn C.v_locale_time_format(uint, uint, uint, uint, uint) uint

	// Format one ASCII conversion through the existing locale/calendar engine,
	// retaining wide literals without a lossy multibyte round trip.
	fn bare_wide_time_piece(output uint, capacity uint, format uint, length uint, calendar uint, locale uint) (uint, bool) {
		if length > ~uint(0) - 2 || capacity == 0 || capacity - 1 > (~uint(0) - 2) / 4 {
			return 0, false
		}
		mut local_format := [128]u8{}
		mut narrow := uint(&local_format[0])
		if length + 2 > 128 {
			narrow = uint(__malloc(length + 2))
			if narrow == 0 { return 0, false }
		}
		defer { if narrow != uint(&local_format[0]) { __free(voidptr(narrow)) } }
		for i := uint(0); i < length; i++ {
			character := unsafe { (&u32(format))[i] }
			if character > 127 { return 0, false }
			unsafe { (&u8(narrow))[i] = u8(character) }
		}
		// A sentinel distinguishes an empty expansion (for example %Z) from
		// strftime's insufficient-capacity result.
		unsafe {
			(&u8(narrow))[length] = 1
			(&u8(narrow))[length + 1] = 0
		}
		maximum := (capacity - 1) * 4 + 2
		mut local_result := [256]u8{}
		mut result := uint(&local_result[0])
		mut used := C.v_locale_time_format(result, if maximum < 256 { maximum } else { uint(256) }, narrow, calendar, locale)
		if used == 0 && maximum > 256 {
			result = uint(__malloc(maximum))
			if result == 0 { return 0, false }
			used = C.v_locale_time_format(result, maximum, narrow, calendar, locale)
		}
		defer { if result != uint(&local_result[0]) { __free(voidptr(result)) } }
		if used == 0 { return 0, false }
		mut position := uint(0)
		mut count := uint(0)
		mut state := [8]u8{}
		for position < used - 1 {
			mut character := u32(0)
			bytes := C.v_wide_decode(uint(&character), result + position, used - 1 - position, uint(&state[0]))
			if bytes == ~uint(0) || bytes == ~uint(0) - 1 || bytes == 0 || count == capacity - 1 {
				return 0, false
			}
			unsafe { (&u32(output))[count] = character }
			position += bytes
			count++
		}
		return count, true
	}

	@[export: 'v_wide_strftime'; markused]
	fn bare_wide_strftime(output uint, capacity uint, format uint, calendar uint, locale uint) uint {
		if capacity == 0 { return 0 }
		previous := C.v_locale_select(locale)
		defer { if locale != 0 { C.v_locale_select(previous) } }
		mut input := uint(0)
		mut count := uint(0)
		defer { unsafe { (&u32(output))[count] = 0 } }
		for {
			character := unsafe { (&u32(format))[input] }
			if character == 0 { return count }
			if character != `%` {
				if count == capacity - 1 { return 0 }
				unsafe { (&u32(output))[count] = character }
				count++
				input++
				continue
			}
			start := input
			input++
			for {
				flag := unsafe { (&u32(format))[input] }
				if flag != `-` && flag != `_` && flag != `+` && !(flag >= `0` && flag <= `9`) {
					break
				}
				input++
			}
			modifier := unsafe { (&u32(format))[input] }
			if modifier == `E` || modifier == `O` { input++ }
			if unsafe { (&u32(format))[input] } == 0 { return 0 }
			input++
			written, ok := bare_wide_time_piece(output + count * 4, capacity - count, format + start * 4, input - start, calendar, locale)
			if !ok { return 0 }
			count += written
		}
		return 0
	}
}
