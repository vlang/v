module multiwindow

$if darwin {
	import macos
	import math

	@[unsafe]
	fn C.strlen(s &char) uint
	fn C.calloc(n uint, size uint) voidptr
	fn C.memcpy(dest voidptr, src voidptr, n uint) voidptr

	@[export: 'v_multiwindow_appkit_string']
	fn v_multiwindow_appkit_string(value &char) voidptr {
		if value == unsafe { nil } {
			return macos.nsstring('')
		}
		return macos.nsstring(unsafe { value.vstring() })
	}

	@[export: 'v_multiwindow_appkit_max_int']
	fn v_multiwindow_appkit_max_int(value int, minimum int) int {
		if minimum > 0 && value < minimum {
			return minimum
		}
		return value
	}

	@[export: 'v_multiwindow_appkit_clamp_unit']
	fn v_multiwindow_appkit_clamp_unit(value f64) f32 {
		if value < 0.0 {
			return 0.0
		}
		if value > 1.0 {
			return 1.0
		}
		return f32(value)
	}

	@[export: 'v_multiwindow_appkit_clamped_int']
	fn v_multiwindow_appkit_clamped_int(value f64) int {
		if value >= 2147483647.0 {
			return 2147483647
		}
		if value <= -2147483648.0 {
			return -2147483648
		}
		return int(math.round(value))
	}

	@[export: 'v_multiwindow_appkit_copy_cstring'; unsafe]
	fn v_multiwindow_appkit_copy_cstring(value &char) &char {
		if value == unsafe { nil } {
			return unsafe { nil }
		}
		len := uint(C.strlen(value))
		copy := &char(C.calloc(len + 1, 1))
		if copy == unsafe { nil } {
			return unsafe { nil }
		}
		C.memcpy(copy, value, len)
		return copy
	}
}
