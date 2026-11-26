module builtin

#flag @DIR/libc_write.c
#include "@DIR/libc_write.h"

fn C.v_freestanding_write(fd i32, address uint, count uint) i64

// libc_write is the scalar bridge used by the POSIX C write shim. It borrows
// the caller's readable bytes and returns a byte count or a negative native
// errno. The C shim owns errno updates and the const-correct POSIX prototype.
@[export: 'v_freestanding_write']
fn libc_write(fd i32, address uint, count uint) i64 {
	$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
		return bare_cancel_syscall(i64(bf_nr_write), uint(fd), address, count, 0, 0, 0)
	}
	written, code := sys_write(i64(fd), unsafe { &u8(address) }, u64(count))
	if code != .enoerror {
		return -i64(code)
	}
	return written
}
