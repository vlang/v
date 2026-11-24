module builtin

$if freestanding && !native && amd64 {
	#include <strings.h>
	#include <string.h>

	fn C.bzero(destination voidptr, count uint)
	fn C.__bzero(destination voidptr, count uint)
	fn C.memchr(const_buffer voidptr, value i32, count uint) voidptr

	// Export through the compiler's C ABI adapters. The byte operations stay in
	// the existing V implementation and are usable before allocator/TLS setup.
	@[export: 'memcpy']
	@[no_profile; no_trace]
	fn bare_macos_memcpy(destination voidptr, const_source voidptr, count uint) voidptr {
		return unsafe { memcpy(destination, const_source, count) }
	}

	@[export: 'memmove']
	@[no_profile; no_trace]
	fn bare_macos_memmove(destination voidptr, const_source voidptr, count uint) voidptr {
		return unsafe { memmove(destination, const_source, count) }
	}

	@[export: 'memset']
	@[no_profile; no_trace]
	fn bare_macos_memset(destination voidptr, value i32, count uint) voidptr {
		return unsafe { memset(destination, int(value), count) }
	}

	@[export: 'memcmp']
	@[no_profile; no_trace]
	fn bare_macos_memcmp(const_a voidptr, const_b voidptr, count uint) i32 {
		return i32(memcmp(const_a, const_b, count))
	}

	@[export: 'strlen']
	@[no_profile; no_trace]
	fn bare_macos_strlen(const_text &char) uint {
		return unsafe { strlen(voidptr(const_text)) }
	}

	@[export: 'bzero']
	@[no_profile; no_trace]
	fn bare_macos_bzero(destination voidptr, count uint) {
		unsafe { memset(destination, 0, count) }
	}

	@[export: '__bzero']
	@[no_profile; no_trace]
	fn bare_macos_internal_bzero(destination voidptr, count uint) {
		unsafe { memset(destination, 0, count) }
	}

	@[export: 'memchr']
	@[no_profile; no_trace]
	fn bare_macos_memchr(const_buffer voidptr, value i32, count uint) voidptr {
		needle := u8(value)
		mut index := uint(0)
		unsafe {
			bytes := &u8(const_buffer)
			for index < count {
				if bytes[index] == needle { return voidptr(&bytes[index]) }
				index++
			}
		}
		return unsafe { nil }
	}
}
