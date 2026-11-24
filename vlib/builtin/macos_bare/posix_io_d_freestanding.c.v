module builtin

$if amd64 && !native && !v_bare_no_startup ? {
	#include <unistd.h>
	#include <errno.h>
	#include <sys/ioctl.h>
	#include <sys/conf.h>
	#include <termios.h>

	// These file-local declarations own the export ABI. The general builtin
	// read declaration uses i32, whereas Darwin's ssize_t is pointer-sized.
	fn C.read(fd i32, buffer voidptr, count uint) isize
	fn C.write(fd i32, const_buffer voidptr, count uint) isize
	fn C.isatty(fd i32) i32

	// The SDK owns the complete layout and stack size; no fields are accessed.
	struct C.termios {}

	@[inline; no_profile; no_trace]
	fn bare_macos_posix_io_result(value u64, code i32) isize {
		if code != 0 {
			bf_native_errno_cell = code
			return isize(-1)
		}
		return isize(value)
	}

	@[export: 'read'; no_profile; no_trace]
	fn bare_macos_posix_read(fd i32, buffer voidptr, count uint) isize {
		mut value := u64(0)
		code := mac_kernel_bsd_call6(u32(mac_nr_read & 0xffffff), u64(i64(fd)), u64(buffer),
			u64(count), 0, 0, 0, &value)
		// POSIX callers observe the kernel's short result or EINTR unchanged.
		return bare_macos_posix_io_result(value, code)
	}

	@[export: 'write'; no_profile; no_trace]
	fn bare_macos_posix_write(fd i32, const_buffer voidptr, count uint) isize {
		mut value := u64(0)
		code := mac_kernel_bsd_call6(u32(mac_nr_write & 0xffffff), u64(i64(fd)),
			u64(const_buffer), u64(count), 0, 0, 0, &value)
		return bare_macos_posix_io_result(value, code)
	}

	@[export: 'isatty'; no_profile; no_trace]
	fn bare_macos_posix_isatty(fd i32) i32 {
		mut kind := i32(0)
		code := mac_kernel_bsd_call6(u32(mac_nr_ioctl & 0xffffff), u64(i64(fd)),
			u64(C.FIODTYPE), u64(&kind), 0, 0, 0, unsafe { nil })
		if code == 0 {
			if kind == i32(C.D_TTY) { return 1 }
			bf_native_errno_cell = i32(C.ENOTTY)
			return 0
		}
		// Some descriptors do not implement FIODTYPE. Darwin's libc falls back
		// to the terminal-attributes query rather than rejecting them outright.
		attributes := C.termios{}
		fallback_code := mac_kernel_bsd_call6(u32(mac_nr_ioctl & 0xffffff), u64(i64(fd)),
			u64(C.TIOCGETA), u64(&attributes), 0, 0, 0, unsafe { nil })
		if fallback_code != 0 {
			bf_native_errno_cell = fallback_code
			return 0
		}
		return 1
	}
}
