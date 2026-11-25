@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_locale_abi.h"
	fn C.v_locale_gnu_data() uint
	fn C.v_locale_gnu_count() u32
	fn C.v_locale_langinfo_base(i32, uint) uint
	fn C.v_locale_bits(uint) u32
	__global bare_locale_wide_cache [512]uint
	__global bare_locale_info_mutex u32

	@[export: 'v_locale_mask'; markused]
	fn bare_locale_mask(requested i32, all_category i32, all_mask i32) i32 {
		return if requested == all_category { all_mask } else { requested }
	}

	fn bare_locale_wide_info(slot uint, source uint) uint {
		bare_compat_lock(&bare_locale_info_mutex)
		defer { bare_compat_unlock(&bare_locale_info_mutex) }
		if bare_locale_wide_cache[slot] != 0 { return bare_locale_wide_cache[slot] }
		length := bare_string_chr_nul(source, 0) - source
		if length > uint(max_int) / 4 - 1 {
			bf_native_errno_cell = 12
			return 0
		}
		output := uint(__malloc((length + 1) * 4))
		if output == 0 { return 0 }
		// All supported locale data is C/POSIX English. Immutable wide data is
		// process-owned, shared by C/C.UTF-8 locales and safe after freelocale.
		for i := uint(0); i < length; i++ {
			unsafe { (&u32(output))[i] = u32((&u8(source))[i]) }
		}
		unsafe { (&u32(output))[length] = 0 }
		bare_locale_wide_cache[slot] = output
		return output
	}

	@[export: 'v_locale_langinfo'; markused]
	fn bare_locale_langinfo(item i32, locale uint) uint {
		if item == i32(C.CODESET) { return C.v_locale_langinfo_base(item, locale) }
		data := C.v_locale_gnu_data()
		for row in 0 .. C.v_locale_gnu_count() {
			// Four target-native i32 words per compiler-produced constant row.
			entry := data + uint(row) * 16
			first := unsafe { (&i32(entry))[0] }
			last := unsafe { (&i32(entry))[1] }
			if item < first || item > last { continue }
			source := unsafe { (&i32(entry))[2] }
			kind := unsafe { (&i32(entry))[3] }
			if kind == 1 {
				slot := uint(row) * 16 + uint(item - first)
				if slot >= 512 {
					bf_native_errno_cell = 75
					return 0
				}
				return bare_locale_wide_info(slot, C.v_locale_langinfo_base(source + item - first, locale))
			}
			if kind == 2 { return uint(source) }
			if kind == 3 { return if source == 255 { uint(c'\xff') } else { uint(c'\x7f') } }
			bits := C.v_locale_bits(locale)
			utf8 := bits & (u32(1) << u32(source)) != 0
			if kind == 4 { return if utf8 { uint(c'UTF-8') } else { uint(c'ANSI_X3.4-1968') } }
			if kind == 5 { return if utf8 { uint(c'C.UTF-8') } else { uint(c'C') } }
		}
		return C.v_locale_langinfo_base(item, locale)
	}
}
