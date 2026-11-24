module builtin

$if amd64 && !native && !v_bare_no_startup ? {
	#include <assert.h>
	#include <stdlib.h>
	#include <signal.h>

	@[noreturn]
	fn C.abort()

	@[noreturn]
	fn C.__assert_rtn(const_function &char, const_file &char, line i32, const_expression &char)

	@[no_profile; no_trace]
	fn bare_macos_abort_mask(mask u32) {
		// XNU's per-thread mask service works without pthread registration and
		// does not access the native or BF errno cells.
		mac_kernel_bsd_call6(329, u64(C.SIG_SETMASK), u64(&mask), 0, 0, 0, 0,
			unsafe { nil })
	}

	@[no_profile; no_trace]
	fn bare_macos_abort_initial_mask() {
		// Give SIGABRT its handler opportunity. Leave synchronous memory faults
		// unblocked too, as Darwin's abort does; other signals cannot interrupt
		// the diagnostic or redirect this thread's termination sequence.
		mut mask := ~u32(0)
		mask &= ~(u32(1) << (u32(C.SIGABRT) - 1))
		mask &= ~(u32(1) << (u32(C.SIGSEGV) - 1))
		mask &= ~(u32(1) << (u32(C.SIGBUS) - 1))
		bare_macos_abort_mask(mask)
	}

	@[no_profile; no_trace]
	fn bare_macos_abort_delivery_wait() {
		// Match Darwin's brief signal-delivery opportunity without a libc sleep,
		// allocator, TLS lookup, or pthread workqueue. This clock core preserves
		// both errno cells and is safe before the runtime initializer.
		mut remaining := FSysClockStamp{}
		mac_clock_sleep_native(FSysClockStamp{ nanoseconds: 10_000_000 }, mut remaining)
	}

	@[no_profile; no_trace; noreturn]
	fn bare_macos_abort_trap() {
		// An undelivered SIGABRT must still terminate abnormally. Darwin's final
		// fallback is an illegal-instruction trap, with ILL/TRAP unblocked.
		mut mask := ~u32(0)
		mask &= ~(u32(1) << (u32(C.SIGILL) - 1))
		mask &= ~(u32(1) << (u32(C.SIGTRAP) - 1))
		bare_macos_abort_mask(mask)
		for {
			asm volatile amd64 {
				ud2
				; ; ; memory
			}
		}
	}

	@[export: 'abort'; no_profile; no_trace; noreturn]
	fn bare_macos_abort() {
		thread := mac_kernel_thread_self()
		bare_macos_abort_initial_mask()
		mac_kernel_bsd_call6(328, u64(thread), u64(C.SIGABRT), 0, 0, 0, 0, unsafe { nil })
		bare_macos_abort_delivery_wait()
		// Ignoring SIGABRT or returning from its handler cannot make abort
		// return. Reset the disposition, unblock it again, and target this same
		// calling thread. Its send right dies with the terminating process.
		action := BareMacSignalAction{ mask: ~u32(0) }
		mac_kernel_bsd_call6(46, u64(C.SIGABRT), u64(&action), 0, 0, 0, 0, unsafe { nil })
		bare_macos_abort_mask(~u32(0) & ~(u32(1) << (u32(C.SIGABRT) - 1)))
		mac_kernel_bsd_call6(328, u64(thread), u64(C.SIGABRT), 0, 0, 0, 0, unsafe { nil })
		bare_macos_abort_delivery_wait()
		bare_macos_abort_trap()
	}

	@[no_profile; no_trace]
	fn bare_macos_assert_write(bytes &u8, count uint) {
		mut offset := uint(0)
		for offset < count {
			mut written := u64(0)
			code := mac_kernel_bsd_call6(u32(mac_nr_write & 0xffffff), 2,
				u64(bytes) + u64(offset), u64(count - offset), 0, 0, 0, &written)
			if code == 4 { continue }
			if code != 0 || written == 0 || written > u64(count - offset) { return }
			offset += uint(written)
		}
	}

	@[no_profile; no_trace]
	fn bare_macos_assert_text(const_text &char) {
		if const_text == unsafe { nil } {
			bare_macos_assert_text(c'(null)')
			return
		}
		mut length := uint(0)
		unsafe {
			for const_text[length] != 0 { length++ }
			bare_macos_assert_write(&u8(const_text), length)
		}
	}

	@[no_profile; no_trace]
	fn bare_macos_assert_line(line i32) {
		mut value := i64(line)
		if value < 0 {
			bare_macos_assert_text(c'-')
			value = -value
		}
		mut digits := [10]u8{}
		mut position := 10
		for {
			position--
			previous := value
			value /= 10
			// An i32 magnitude has at most ten digits. Avoid bounds/panic and
			// checked-modulo runtime helpers on this pre-initialization path.
			unsafe { digits[position] = u8(48 + previous - value * 10) }
			if value == 0 { break }
		}
		bare_macos_assert_write(unsafe { &digits[position] }, uint(10 - position))
	}

	@[export: '__assert_rtn'; no_profile; no_trace; noreturn]
	fn bare_macos_assert_rtn(const_function &char, const_file &char, line i32,
		const_expression &char) {
		bare_macos_abort_initial_mask()
		if u64(const_function) == ~u64(0) {
			// Darwin uses the -1 function sentinel for the older __eprintf ABI.
			bare_macos_assert_text(const_file)
			bare_macos_assert_text(c':')
			bare_macos_assert_line(line)
			bare_macos_assert_text(c': failed assertion `')
			bare_macos_assert_text(const_expression)
			bare_macos_assert_text(c"'\n")
		} else {
			bare_macos_assert_text(c'Assertion failed: (')
			bare_macos_assert_text(const_expression)
			bare_macos_assert_text(c')')
			if const_function != unsafe { nil } {
				bare_macos_assert_text(c', function ')
				bare_macos_assert_text(const_function)
			}
			bare_macos_assert_text(c', file ')
			bare_macos_assert_text(const_file)
			bare_macos_assert_text(c', line ')
			bare_macos_assert_line(line)
			bare_macos_assert_text(c'.\n')
		}
		bare_macos_abort()
	}
}
