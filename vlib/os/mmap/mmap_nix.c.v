module mmap

import os

#include <sys/mman.h>

pub const prot_none = C.PROT_NONE
pub const prot_read = C.PROT_READ
pub const prot_write = C.PROT_WRITE
pub const prot_exec = C.PROT_EXEC

pub const map_file = C.MAP_FILE
pub const map_shared = C.MAP_SHARED
pub const map_private = C.MAP_PRIVATE
pub const map_type = C.MAP_TYPE
pub const map_fixed = C.MAP_FIXED
pub const map_anonymous = C.MAP_ANONYMOUS
pub const map_anon = map_anonymous

// Flags for msync.
pub const ms_async = C.MS_ASYNC
pub const ms_sync = C.MS_SYNC
pub const ms_invalidate = C.MS_INVALIDATE

fn C.mmap(addr voidptr, len usize, prot int, flags int, fd int, offset i64) voidptr
fn C.munmap(addr voidptr, len usize) int
fn C.mprotect(addr voidptr, len usize, prot int) int
fn C.msync(addr voidptr, len usize, flags int) int
fn C.mlock(addr voidptr, len usize) int
fn C.munlock(addr voidptr, len usize) int

// mmap map files or devices into memory
pub fn mmap(args MmapOptions) !voidptr {
	addr := C.mmap(args.addr, args.len, args.prot, args.flags, args.fd, args.offset)
	if addr != C.MAP_FAILED {
		return addr
	}
	return os.error_posix()
}

// munmap unmap memory region
pub fn munmap(addr voidptr, len usize) ! {
	if C.munmap(addr, len) != 0 {
		return os.error_posix()
	}
}

// mprotect change memory protection
pub fn mprotect(addr voidptr, len usize, prot int) ! {
	if C.mprotect(addr, len, prot) != 0 {
		return os.error_posix()
	}
}

// msync sync memory changes to file
pub fn msync(addr voidptr, len usize, flags int) ! {
	if C.msync(addr, len, flags) != 0 {
		return os.error_posix()
	}
}

// mlock lock memory to prevent paging
pub fn mlock(addr voidptr, len usize) ! {
	if C.mlock(addr, len) != 0 {
		return os.error_posix()
	}
}

// munlock unlock memory to allow paging
pub fn munlock(addr voidptr, len usize) ! {
	if C.munlock(addr, len) != 0 {
		return os.error_posix()
	}
}
