module builtin

$if !i386 {
	#flag @DIR/libc_posix.c
	#include "@DIR/libc_posix.h"

	fn C.v_freestanding_read(fd i32, address uint, count uint) i64
	fn C.v_freestanding_clock_gettime(clock i32, stamp uint) i64
	fn C.v_freestanding_nanosleep(request uint, remaining uint) i64
	fn C.v_freestanding_tty_probe(fd i32, request uint, termios uint) i64

	// These C ABI bridges preserve raw kernel results, including EINTR. POSIX
	// callers decide whether to retry; the C shims update native errno only on
	// failure. Scalar addresses keep C qualifiers and native structs in C.
	@[export: 'v_freestanding_read']
	fn libc_read(fd i32, address uint, count uint) i64 {
		$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
			return bare_cancel_syscall(i64(bf_nr_read), uint(fd), address, count, 0, 0, 0)
		}
		return i64(sys_call3(bf_nr_read, u64(fd), u64(address), u64(count)))
	}

	@[export: 'v_freestanding_clock_gettime']
	fn libc_clock_gettime(clock i32, stamp uint) i64 {
		return i64(sys_call2(bf_nr_clock_gettime, u64(clock), u64(stamp)))
	}

	@[export: 'v_freestanding_nanosleep']
	fn libc_nanosleep(request uint, remaining uint) i64 {
		$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
			return bare_cancel_syscall(i64(bf_nr_nanosleep), request, remaining, 0, 0, 0, 0)
		}
		return i64(sys_call2(bf_nr_nanosleep, u64(request), u64(remaining)))
	}

	@[export: 'v_freestanding_tty_probe']
	fn libc_tty_probe(fd i32, request uint, termios uint) i64 {
		return i64(sys_call3(bf_nr_ioctl, u64(fd), u64(request), u64(termios)))
	}
}
