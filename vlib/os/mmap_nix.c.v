module os

#include <sys/mman.h>

fn C.msync(addr voidptr, len uint, flags i32) i32

// mmap_map_nix opens path, maps len bytes and closes the fd again (the
// mapping survives a closed fd on POSIX). With create it creates/truncates
// the file to len first.
fn mmap_map_nix(path string, len u64, write bool, create bool) !voidptr {
	$if vlibc ? {
		// Hosted -d vlibc: open/size/map via raw vlibc syscalls (no libc).
		// Raw Linux flag constants (match vlibc_open style, not C.*):
		// O_RDONLY=0, O_RDWR=2, O_CREAT=0x40, O_TRUNC=0x200.
		open_flags := if write { u64(2) } else { u64(0) }
		mut oflags := open_flags
		if create {
			oflags |= u64(0x40) | u64(0x200)
		}
		fd, e := vlibc_open(path, oflags, 0o644)
		if e != .enoerror {
			return error('os: cannot open `${path}` for mapping')
		}
		if create {
			e2 := vlibc_ftruncate(fd, len)
			if e2 != .enoerror {
				vlibc_close(fd)
				return error('os: cannot size `${path}` for mapping')
			}
		}
		// PROT_READ=1, PROT_WRITE=2, MAP_SHARED=1.
		prot := if write { 1 | 2 } else { 1 }
		addr, e3 := vlibc_sys_mmap(unsafe { nil }, len, prot, 1, fd, 0)
		vlibc_close(fd)
		if e3 != .enoerror {
			return error('os: mmap of `${path}` failed')
		}
		return addr
	} $else {
		flags := if write { C.O_RDWR } else { C.O_RDONLY }
		mut open_flags := flags
		if create {
			open_flags |= C.O_CREAT | C.O_TRUNC
		}
		fd := C.open(&char(path.str), open_flags, 0o644)
		if fd == -1 {
			return error('os: cannot open `${path}` for mapping')
		}
		if create {
			if C.ftruncate(fd, len) != 0 {
				C.close(fd)
				return error('os: cannot size `${path}` for mapping')
			}
		}
		prot := if write { C.PROT_READ | C.PROT_WRITE } else { C.PROT_READ }
		addr := C.mmap(unsafe { nil }, uint(len), prot, C.MAP_SHARED, fd, 0)
		C.close(fd)
		if addr == voidptr(C.MAP_FAILED) {
			return error('os: mmap of `${path}` failed')
		}
		return addr
	}
}

// mmap_unmap_nix releases a mapping.
fn mmap_unmap_nix(addr voidptr, len u64) {
	$if vlibc ? {
		vlibc_sys_munmap(addr, len)
	} $else {
		C.munmap(addr, uint(len))
	}
}

// mmap_sync_nix flushes a mapping to the file.
fn mmap_sync_nix(addr voidptr, len u64) ! {
	$if vlibc ? {
		// No vlibc_sys_msync helper exists yet; hosted -d vlibc still links
		// libc, so keep msync via libc (same as page_size via sysconf).
		if C.msync(addr, uint(len), C.MS_SYNC) != 0 {
			return error('os: msync failed')
		}
	} $else {
		if C.msync(addr, uint(len), C.MS_SYNC) != 0 {
			return error('os: msync failed')
		}
	}
}

// mmap_sync_platform flushes a mapping (nix side).
fn mmap_sync_platform(addr voidptr, len u64, file_handle voidptr) ! {
	_ = file_handle
	return mmap_sync_nix(addr, len)
}

// mmap_unmap_platform releases a mapping (nix side; mapping handle unused).
fn mmap_unmap_platform(addr voidptr, len u64, mapping voidptr, file_handle voidptr) {
	_ = file_handle
	_ = mapping
	mmap_unmap_nix(addr, len)
}
